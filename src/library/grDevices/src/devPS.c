/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

#include <stdio.h>
#include <ctype.h>
#include <limits.h> /* required for MB_LEN_MAX */
#include <string.h>

#include <wchar.h>
#include <wctype.h>
static void
mbcsToSbcs(const char *in, char *out, const char *encoding, int enc, int silent);


#include <R_ext/Riconv.h>

#include <Rmath.h>		/* for fround */
#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include <R_ext/Error.h>
#include <R_ext/RS.h>
#include "Fileio.h"
#include "grDevices.h"

#ifdef HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif

#include "zlib.h"

#ifndef max
#define max(a,b) ((a > b) ? a : b)
#endif
#ifndef min
#define min(a,b) ((a > b) ? b : a)
#endif

/* from connections.o */
extern gzFile R_gzopen (const char *path, const char *mode);
extern char *R_gzgets(gzFile file, char *buf, int len);
extern int R_gzclose (gzFile file);

#define INVALID_COL 0xff0a0b0c

/* Define this to use hyphen except in -[0-9] */
#undef USE_HYPHEN
/* In ISOLatin1, minus is 45 and hyphen is 173 */
#ifdef USE_HYPHEN
static unsigned char PS_hyphen = 173;
#endif

#define USERAFM 999

/* Part 0.  AFM File Names */

static const char *CIDBoldFontStr1 =
"16 dict begin\n"
"  /basecidfont exch def\n"
"  /basefont-H /.basefont-H /Identity-H [ basecidfont ] composefont def\n"
"  /basefont-V /.basefont-V /Identity-V [ basecidfont ] composefont def\n"
"  /CIDFontName dup basecidfont exch get def\n"
"  /CIDFontType 1 def\n"
"  /CIDSystemInfo dup basecidfont exch get def\n"
"  /FontInfo dup basecidfont exch get def\n"
"  /FontMatrix [ 1 0 0 1 0 0 ] def\n"
"  /FontBBox [\n"
"    basecidfont /FontBBox get cvx exec\n"
"    4 2 roll basecidfont /FontMatrix get transform\n"
"    4 2 roll basecidfont /FontMatrix get transform\n"
"  ] def\n"
"  /cid 2 string def\n";
static const char *CIDBoldFontStr2 =
"  /BuildGlyph {\n"
"    gsave\n"
"    exch begin\n"
"      dup 256 idiv cid exch 0 exch put\n"
"      256 mod cid exch 1 exch put\n"
"      rootfont\n"
"        /WMode known { rootfont /WMode get 1 eq } { false } ifelse\n"
"      { basefont-V } { basefont-H } ifelse setfont\n"
"      .03 setlinewidth 1 setlinejoin\n"
"      newpath\n"
"      0 0 moveto cid false charpath stroke\n"
"      0 0 moveto cid show\n"
"      currentpoint setcharwidth\n"
"    end\n"
"    grestore\n"
"  } bind def\n"
"  currentdict\n"
"end\n"
"/CIDFont defineresource pop\n";


/* Part 1.  AFM File Parsing.  */

/* These are the basic entities in the AFM file */

// The structure is defined in
// https://adobe-type-tools.github.io/font-tech-notes/pdfs/5004.AFM_Spec.pdf

#define BUFSIZE 512
#define NA_SHORT -30000

typedef struct {
    unsigned char c1;
    unsigned char c2;
    short kern;
} KP;

typedef struct {
    short FontBBox[4];
    short CapHeight;
    short XHeight;
    short Descender;
    short Ascender;
    short StemH;
    short StemV;
    short ItalicAngle;
    struct {
	short WX;
	short BBox[4];
    } CharInfo[256];
    KP *KernPairs;
    short KPstart[256];
    short KPend[256];
    short nKP;
    short IsFixedPitch;
} FontMetricInfo;

enum {
    Empty,
    StartFontMetrics,
    Comment,
    FontName,
    EncodingScheme,
    FullName,
    FamilyName,
    Weight,
    ItalicAngle,
    IsFixedPitch,
    UnderlinePosition,
    UnderlineThickness,
    Version,
    Notice,
    FontBBox,
    CapHeight,
    XHeight,
    Descender,
    Ascender,
    StartCharMetrics,
    C,
    CH,
    EndCharMetrics,
    StartKernData,
    StartKernPairs,
    KPX,
    EndKernPairs,
    EndKernData,
    StartComposites,
    CC,
    EndComposites,
    EndFontMetrics,
    StdHW,
    StdVW,
    CharacterSet,
    Unknown
};

static const struct {
    const char *keyword;
    const int code;
}
KeyWordDictionary[] = {
    { "StartFontMetrics",    StartFontMetrics },
    { "Comment",	     Comment },
    { "FontName",	     FontName },
    { "EncodingScheme",	     EncodingScheme },
    { "FullName",	     FullName },
    { "FamilyName",	     FamilyName },
    { "Weight",		     Weight },
    { "ItalicAngle",	     ItalicAngle },
    { "IsFixedPitch",	     IsFixedPitch },
    { "UnderlinePosition",   UnderlinePosition },
    { "UnderlineThickness",  UnderlineThickness },
    { "Version",	     Version },
    { "Notice",		     Notice },
    { "FontBBox",	     FontBBox },
    { "CapHeight",	     CapHeight },
    { "XHeight",	     XHeight },
    { "Descender",	     Descender },
    { "Ascender",	     Ascender },
    { "StartCharMetrics",    StartCharMetrics },
    { "C ",		     C },
    { "CH ",		     CH },
    { "EndCharMetrics",	     EndCharMetrics },
    { "StartKernData",	     StartKernData },
    { "StartKernPairs",	     StartKernPairs },
    { "KPX ",		     KPX },
    { "EndKernPairs",	     EndKernPairs },
    { "EndKernData",	     EndKernData },
    { "StartComposites",     StartComposites },
    { "CC ",		     CC },
    { "EndComposites",	     EndComposites },
    { "EndFontMetrics",	     EndFontMetrics },
    { "StdHW",		     StdHW },
    { "StdVW",		     StdVW },
    { "CharacterSet",	     CharacterSet},
    { NULL,		     Unknown },
};

static int MatchKey(char const * l, char const * k)
{
    while (*k)
	if (*k++ != *l++) return 0;
    return 1;
}

static int KeyType(const char * const s)
{
    int i;
    if (*s == '\n')
	return Empty;
    for (i = 0; KeyWordDictionary[i].keyword; i++)
	if (MatchKey(s, KeyWordDictionary[i].keyword))
	    return KeyWordDictionary[i].code;
//    printf("Unknown %s\n", s); // not needed, PR#15057 found it annoying
    return Unknown;
}

static char *SkipToNextItem(char *p)
{
    while (!isspace((int)*p)) p++;
    while (isspace((int)*p)) p++;
    return p;
}

static char *SkipToNextKey(char *p)
{
    while (*p != ';') p++;
    p++;
    while (isspace((int)*p)) p++;
    return p;
}

static int GetFontBBox(const char *buf, FontMetricInfo *metrics)
{
    if (sscanf(buf, "FontBBox %hd %hd %hd %hd",
	      &(metrics->FontBBox[0]),
	      &(metrics->FontBBox[1]),
	      &(metrics->FontBBox[2]),
	      &(metrics->FontBBox[3])) != 4) return 0;
#ifdef DEBUG_PS2
    Rprintf("FontBBox %d %d %d %d\n",
	    (metrics->FontBBox[0]),
	    (metrics->FontBBox[1]),
	    (metrics->FontBBox[2]),
	    (metrics->FontBBox[3]));
#endif
    return 1;
}

/* The longest named Adobe glyph is 39 chars:
   whitediamondcontainingblacksmalldiamond
 */
typedef struct {
    char cname[40];
} CNAME;


/* If reencode > 0, remap to new encoding */
// File format in
// https://adobe-type-tools.github.io/font-tech-notes/pdfs/5004.AFM_Spec.pdf
static int GetCharInfo(char *buf, FontMetricInfo *metrics,
		       CNAME *charnames, CNAME *encnames,
		       int reencode)
{
    char *p = buf, charname[40];
    int nchar, nchar2 = -1, i;
    short WX;

    if (!MatchKey(buf, "C ")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%d", &nchar);
    if ((nchar < 0 || nchar > 255) && !reencode) return 1;
    p = SkipToNextKey(p);

    if (!MatchKey(p, "WX")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%hd", &WX);
    p = SkipToNextKey(p);

    if (!MatchKey(p, "N ")) return 0;
    p = SkipToNextItem(p);
    if(reencode) {
	sscanf(p, "%39s", charname);
#ifdef DEBUG_PS2
	Rprintf("char name %s\n", charname);
#endif
	/* a few chars appear twice in ISOLatin1 */
	nchar = nchar2 = -1;
	for (i = 0; i < 256; i++)
	    if(!strcmp(charname, encnames[i].cname)) {
		strcpy(charnames[i].cname, charname);
		if(nchar == -1) nchar = i; else nchar2 = i;
	    }
	if (nchar == -1) return 1;
    } else {
	sscanf(p, "%39s", charnames[nchar].cname);
    }
    metrics->CharInfo[nchar].WX = WX;
    p = SkipToNextKey(p);

    if (!MatchKey(p, "B ")) return 0;
    p = SkipToNextItem(p);
    // Bounding box is llx lly urx ury, y measured from baseline.
    sscanf(p, "%hd %hd %hd %hd",
	   &(metrics->CharInfo[nchar].BBox[0]),
	   &(metrics->CharInfo[nchar].BBox[1]),
	   &(metrics->CharInfo[nchar].BBox[2]),
	   &(metrics->CharInfo[nchar].BBox[3]));

#ifdef DEBUG_PS2
    Rprintf("nchar = %d %d %d %d %d %d\n", nchar,
	    metrics->CharInfo[nchar].WX,
	    metrics->CharInfo[nchar].BBox[0],
	    metrics->CharInfo[nchar].BBox[1],
	    metrics->CharInfo[nchar].BBox[2],
	    metrics->CharInfo[nchar].BBox[3]);
#endif
    if (nchar2 > 0) {
	metrics->CharInfo[nchar2].WX = WX;
	sscanf(p, "%hd %hd %hd %hd",
	       &(metrics->CharInfo[nchar2].BBox[0]),
	       &(metrics->CharInfo[nchar2].BBox[1]),
	       &(metrics->CharInfo[nchar2].BBox[2]),
	       &(metrics->CharInfo[nchar2].BBox[3]));

#ifdef DEBUG_PS2
	Rprintf("nchar = %d %d %d %d %d %d\n", nchar2,
		metrics->CharInfo[nchar2].WX,
		metrics->CharInfo[nchar2].BBox[0],
		metrics->CharInfo[nchar2].BBox[1],
		metrics->CharInfo[nchar2].BBox[2],
		metrics->CharInfo[nchar2].BBox[3]);
#endif
    }
    return 1;
}

static int GetKPX(char *buf, int nkp, FontMetricInfo *metrics,
		  CNAME *charnames)
{
    char *p = buf, c1[50], c2[50];
    int i, done = 0;

    p = SkipToNextItem(p);
    sscanf(p, "%49s %49s %hd", c1, c2, &(metrics->KernPairs[nkp].kern));
    if (streql(c1, "space") || streql(c2, "space")) return 0;
    for(i = 0; i < 256; i++) {
	if (!strcmp(c1, charnames[i].cname)) {
	    metrics->KernPairs[nkp].c1 = (unsigned char) i;
	    done++;
	    break;
	}
    }
    for(i = 0; i < 256; i++)
	if (!strcmp(c2, charnames[i].cname)) {
	    metrics->KernPairs[nkp].c2 = (unsigned char) i;
	    done++;
	    break;
	}
    return (done==2);
}

/* Encode File Parsing.  */
/* Statics here are OK, as all the calls are in one initialization
   so no concurrency (until threads?) */

typedef struct {
  /* Probably can make buf and p0 local variables. Only p needs to be
     stored across calls. Need to investigate this more closely. */
  char buf[1000];
  char *p;
  char *p0;
} EncodingInputState;

/* read in the next encoding item, separated by white space. */
static int GetNextItem(FILE *fp, char *dest, int c, EncodingInputState *state)
{
    if (c < 0) state->p = NULL;
    while (1) {
	if (feof(fp)) { state->p = NULL; return 1; }
	if (!state->p || *state->p == '\n' || *state->p == '\0') {
	    state->p = fgets(state->buf, 1000, fp);
	}
	/* check for incomplete encoding file */
	if(!state->p) return 1;
	while (isspace((int)* state->p)) state->p++;
	if (*state->p == '\0' || *state->p == '%'|| *state->p == '\n') { state->p = NULL; continue; }
	state->p0 = state->p;
	while (!isspace((int)*state->p)) state->p++;
	if (*state->p != '\0') *state->p++ = '\0';
	if(c == 45) strcpy(dest, "/minus"); else strcpy(dest, state->p0);
	break;
    }
    return 0;
}

/*
 * Convert the encoding file name into a name to be used with iconv()
 * in mbcsToSbcs()
 *
 * FIXME:  Doesn't trim path/to/encfile (i.e., doesn't handle
 *         custom encoding file selected by user).
 *         Also assumes that encpath has ".enc" suffix supplied
 *         (not required by R interface)
 */

static int pathcmp(const char *encpath, const char *comparison) {
    char pathcopy[R_PATH_MAX];
    char *p1, *p2;
    strcpy(pathcopy, encpath);
    /*
     * Strip path/to/encfile/
     */
    p1 = &(pathcopy[0]);
    while ((p2 = strchr(p1, FILESEP[0]))) {
	p1 = p2 + sizeof(char);
    }
    /*
     * Strip suffix
     */
    p2 = (strchr(p1, '.'));
    if (p2)
	*p2 = '\0';
    return strcmp(p1, comparison);
}

static void seticonvName(const char *encpath, char *convname)
{
    /*
     * Default to "latin1"
     */
    char *p;
    strcpy(convname, "latin1");
    if(pathcmp(encpath, "ISOLatin1") == 0)
	strcpy(convname, "latin1");
    else if (pathcmp(encpath, "WinAnsi") == 0)
	strcpy(convname, "cp1252");
#ifdef OS_MUSL
    else if(pathcmp(encpath, "ISOLatin2") == 0)
	strcpy(convname, "iso88592");
    else if(pathcmp(encpath, "ISOLatin7") == 0)
	strcpy(convname, "iso885913");
    else if(pathcmp(encpath, "ISOLatin9") == 0)
	strcpy(convname, "iso885915");
    else if(pathcmp(encpath, "Greek") == 0)
	strcpy(convname, "iso88597");
    else if(pathcmp(encpath, "Cyrillic") == 0)
	strcpy(convname, "iso88595");
#else
    else if(pathcmp(encpath, "ISOLatin2") == 0)
	strcpy(convname, "latin2");
    else if(pathcmp(encpath, "ISOLatin7") == 0)
	strcpy(convname, "latin7");
    else if(pathcmp(encpath, "ISOLatin9") == 0)
	strcpy(convname, "latin-9");
    else if(pathcmp(encpath, "Greek") == 0)
	strcpy(convname, "iso-8859-7");
    else if(pathcmp(encpath, "Cyrillic") == 0)
	strcpy(convname, "iso-8859-5");
#endif
    else {
	/*
	 * Last resort = trim .enc off encpath to produce convname
	 * Used for CP12xx, KOI8-?
	 */
	strcpy(convname, encpath);
	p = strrchr(convname, '.');
	if(p) *p = '\0';
    }
    //fprintf(stderr, "mapping %s to %s\n", encpath, convname);
}

/* Load encoding array from a file: defaults to the R_HOME/library/grDevices/afm directory */

/*
 * encpath gives the file to read from
 * encname is filled with the encoding name from the file
 * encconvname is filled with a "translation" of the encoding name into
 *             one that can be used with iconv()
 * encnames is filled with the character names from the file
 * enccode is filled with the raw source of the file
 */
static int
LoadEncoding(const char *encpath, char *encname,
	     char *encconvname, CNAME *encnames,
	     char *enccode, bool isPDF)
{
    char buf[BUFSIZE]; // BUFSIZE is 512
    int i;
    FILE *fp;
    EncodingInputState state;
    state.p = state.p0 = NULL;

    seticonvName(encpath, encconvname);

    if(strchr(encpath, FILESEP[0])) strcpy(buf, encpath);
    else snprintf(buf, BUFSIZE,"%s%slibrary%sgrDevices%senc%s%s",
		  R_Home, FILESEP, FILESEP, FILESEP, FILESEP, encpath);
#ifdef DEBUG_PS
    Rprintf("encoding path is %s\n", buf);
#endif
    if (!(fp = R_fopen(R_ExpandFileName(buf), "r"))) {
	strcat(buf, ".enc");
	if (!(fp = R_fopen(R_ExpandFileName(buf), "r"))) return 0;
    }
    if (GetNextItem(fp, buf, -1, &state)) { fclose(fp); return 0;} /* encoding name */
    memcpy(encname, buf+1, 99); // was strncpy, deliberate truncation
    encname[99] = '\0';
    if (!isPDF) snprintf(enccode, 5000, "/%s [\n", encname);
    else enccode[0] = '\0';
    if (GetNextItem(fp, buf, 0, &state)) { fclose(fp); return 0;} /* [ */
    for(i = 0; i < 256; i++) {
	if (GetNextItem(fp, buf, i, &state)) { fclose(fp); return 0; }
	memcpy(encnames[i].cname, buf+1, 39); // was strncpy, gcc10 warned
	encnames[i].cname[39] = '\0';
	strcat(enccode, " /"); strcat(enccode, encnames[i].cname);
	if(i%8 == 7) strcat(enccode, "\n");
    }
    if (GetNextItem(fp, buf, 0, &state)) { fclose(fp); return 0;} /* ] */
    fclose(fp);
    if (!isPDF) strcat(enccode,"]\n");
    return 1;
}

/* Load font metrics from a file: defaults to the
   R_HOME/library/grDevices/afm directory */
static int
PostScriptLoadFontMetrics(const char * const fontpath,
			  FontMetricInfo *metrics,
			  char *fontname,
			  CNAME *charnames,
			  CNAME *encnames,
			  int reencode)
{
    char buf[BUFSIZE], *p, truth[10];
    int mode, i = 0, j, ii, nKPX=0;
    gzFile fp;

    if(strchr(fontpath, FILESEP[0])) strcpy(buf, fontpath);
    else
	snprintf(buf, BUFSIZE,"%s%slibrary%sgrDevices%safm%s%s.gz",
		 R_Home, FILESEP, FILESEP, FILESEP, FILESEP, fontpath);
#ifdef DEBUG_PS
    Rprintf("afmpath is %s\n", buf);
    Rprintf("reencode is %d\n", reencode);
#endif

    if (!(fp = R_gzopen(R_ExpandFileName(buf), "rb"))) {
	/* try uncompressed version */
	snprintf(buf, BUFSIZE,"%s%slibrary%sgrDevices%safm%s%s",
		 R_Home, FILESEP, FILESEP, FILESEP, FILESEP, fontpath);
	if (!(fp = R_gzopen(R_ExpandFileName(buf), "rb"))) {
	    warning(_("afm file '%s' could not be opened"), 
		    R_ExpandFileName(buf));
	    return 0;
	}
    }

    metrics->KernPairs = NULL;
    metrics->CapHeight = metrics->XHeight = metrics->Descender =
	metrics->Ascender = metrics->StemH = metrics->StemV = NA_SHORT;
    metrics->IsFixedPitch = -1;
    metrics->ItalicAngle = 0;
    mode = 0;
    for (ii = 0; ii < 256; ii++) {
	charnames[ii].cname[0] = '\0';
	metrics->CharInfo[ii].WX = NA_SHORT;
	for(j = 0; j < 4; j++) metrics->CharInfo[ii].BBox[j] = 0;
    }
    while (R_gzgets(fp, buf, BUFSIZE)) {
	switch(KeyType(buf)) {

	case StartFontMetrics:
	    mode = StartFontMetrics;
	    break;

	case EndFontMetrics:
	    mode = 0;
	    break;

	case FontBBox:
	    if (!GetFontBBox(buf, metrics)) {
		warning("'FontBBox' could not be parsed");
		goto pserror;
	    }
	    break;

	case C:
	    if (mode != StartFontMetrics) goto pserror;
	    if (!GetCharInfo(buf, metrics, charnames, encnames, reencode)) {
		warning("'CharInfo' could not be parsed");
		goto pserror;
	    }
	    break;

	case StartKernData:
	    mode = StartKernData;
	    break;

	case StartKernPairs:
	    if(mode != StartKernData) goto pserror;
	    p = SkipToNextItem(buf);
	    sscanf(p, "%d", &nKPX);
	    if(nKPX > 0) {
		/* nPKX == 0 should not happen, but has */
		metrics->KernPairs = (KP *) malloc(nKPX * sizeof(KP));
		if (!metrics->KernPairs) goto pserror;
	    }
	    break;

	case KPX:
	    if(mode != StartKernData || i >= nKPX) goto pserror;
	    if (GetKPX(buf, i, metrics, charnames)) i++;
	    break;

	case EndKernData:
	    mode = 0;
	    break;

	case Unknown:
	    warning(_("unknown AFM entity encountered"));
	    break;

	case FontName:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%[^\n\f\r]", fontname);
	    break;

	case CapHeight:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->CapHeight);
	    break;

	case XHeight:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->XHeight);
	    break;

	case Ascender:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->Ascender);
	    break;

	case Descender:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->Descender);
	    break;

	case StdHW:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->StemH);
	    break;

	case StdVW:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->StemV);
	    break;

	case ItalicAngle:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%hd", &metrics->ItalicAngle);
	    break;

	case IsFixedPitch:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%[^\n\f\r]", truth);
	    metrics->IsFixedPitch = strcmp(truth, "true") == 0;
	    break;

	case Empty:
	default:
	    break;
	}
    }
    metrics->nKP = (short) i;
    R_gzclose(fp);
    /* Make an index for kern-pair searches: relies on having contiguous
       blocks by first char for efficiency, but works in all cases. */
    {
	short ind, tmp;
	for (j = 0; j < 256; j++) {
	    metrics->KPstart[j] = (short) i;
	    metrics->KPend[j] = 0;
	}
	for (j = 0; j < i; j++) {
	    ind = metrics->KernPairs[j].c1;
	    tmp = metrics->KPstart[ind];
	    if(j < tmp) metrics->KPstart[ind] = (short) j;
	    tmp = metrics->KPend[ind];
	    if(j > tmp) metrics->KPend[ind] = (short) j;
	}
    }
    return 1;
pserror:
    R_gzclose(fp);
    return 0;
}


#include <rlocale.h> /* for Ri18n_wcwidth */


static double
    PostScriptStringWidth(const unsigned char *str, int enc,
			  FontMetricInfo *metrics,
			  bool useKerning,
			  int face, const char *encoding)
{
    int sum = 0;
    const unsigned char *p = NULL, *str1 = str;

    int status;
    if(!metrics && (face % 5) != 0) {
	/* This is the CID font case, and should only happen for
	   non-symbol fonts.  So we assume monospaced with multipliers.
	   We need to remap even if we are in a SBCS, should we get to here */
	size_t ucslen;
	ucslen = mbcsToUcs2((char *)str, NULL, 0, enc);
	if (ucslen != (size_t)-1) {
	    /* We convert the characters but not the terminator here */
	    R_CheckStack2(ucslen * sizeof(R_ucs2_t));
	    R_ucs2_t ucs2s[ucslen];
	    status = (int) mbcsToUcs2((char *)str, ucs2s, (int) ucslen, enc);
	    if (status >= 0)
		for(int i = 0 ; i < ucslen ; i++) {
		    short wx;
#ifdef USE_RI18N_WIDTH
		    wx = (short)(500 * Ri18n_wcwidth(ucs2s[i]));
#else
		    wx = (short)(500 * wcwidth(ucs2s[i]));
#endif
		    /* printf("width for U+%04x is %d\n", ucs2s[i], wx); */
		    sum += wx;
		}
	    else
		warning(_("invalid string in '%s'"), "PostScriptStringWidth");
	    return 0.001 * sum;
	} else {
	    warning(_("invalid string in '%s'"), "PostScriptStringWidth");
	    return 0.0;
	}
    } else
	if(!strIsASCII((char *) str) &&
	   /*
	    * Every fifth font is a symbol font:
	    * see postscriptFonts()
	    */
	   (face % 5) != 0) {
	    R_CheckStack2(2*strlen((char *)str)+1);
	    /* Output string cannot be longer
	       -- but it can be in bytes if transliteration is used */
	    char *buff = alloca(2*strlen((char *)str)+1);
	    // Do not show warnings (but throw any errors)
	    mbcsToSbcs((char *)str, buff, encoding, enc, 1);
	    str1 = (unsigned char *)buff;
	}

    /* safety */
    if(!metrics) return 0.0;


    /* Now we know we have an 8-bit encoded string in the encoding to
       be used for output. */
    for (p = str1; *p; p++) {
	short wx;
#ifdef USE_HYPHEN
	if (*p == '-' && !isdigit(p[1]))
	    wx = metrics->CharInfo[(int)PS_hyphen].WX;
	else
#endif
	    wx = metrics->CharInfo[*p].WX;
	if(wx == NA_SHORT)
	    warning(_("font width unknown for character 0x%02x in encoding %s" ),
		    *p, encoding);
	else sum += wx;

	if(useKerning) {
	    /* check for kerning adjustment */
	    unsigned char p1 = p[0], p2 = p[1];
	    for (int i =  metrics->KPstart[p1]; i < metrics->KPend[p1]; i++)
		/* second test is a safety check: should all start with p1 */
		if(metrics->KernPairs[i].c2 == p2 &&
		   metrics->KernPairs[i].c1 == p1) {
		    sum += metrics->KernPairs[i].kern;
		    break;
		}
	}
    }
    return 0.001 * sum;
}


/* Be careful about the assumptions here.  In an 8-bit locale 0 <= c < 256
   and it is in the encoding in use.  As it is not going to be
   re-encoded when text is output, it is correct not to re-encode here.

   When called in an MBCS locale and font != 5, chars < 128 are sent
   as is (we assume that is ASCII) and others are re-encoded to
   Unicode in GEText (and interpreted as Unicode in GESymbol).

   FIXME: this assumes the mbcs is UTF-8.
*/
# ifdef WORDS_BIGENDIAN
static const char UCS2ENC[] = "UCS-2BE";
# else
static const char UCS2ENC[] = "UCS-2LE";
# endif

static void
PostScriptMetricInfo(int c, double *ascent, double *descent, double *width,
		     FontMetricInfo *metrics,
		     bool useKerning,
		     bool isSymbol,
		     const char *encoding)
{
    if (c == 0) {
	*ascent = 0.001 * metrics->FontBBox[3];
	*descent = -0.001 * metrics->FontBBox[1];
	*width = 0.001 * (metrics->FontBBox[2] - metrics->FontBBox[0]);
	return;
    }

    bool Unicode = mbcslocale;
    if (c < 0) { Unicode = true; c = -c;}
    
    if(Unicode && !isSymbol && c >= 128) { // don't really need to except ASCII
	if (c >= 65536) {
	    // No afm nor enc has entries for chars beyond the basic plane */
	    *ascent = 0;
	    *descent = 0;
	    *width = 0;
	    // wchar_t might be 16-bit so do not attempt to print
	    warning(_("font metrics unknown for Unicode character U+%04X"), c);
	    return;
	}
        char str[10]; // one Unicode point in an mbcs, almost always UTF-8.
	{
	    void *cd = NULL;
	    const char *i_buf; char *o_buf;
	    size_t i_len, o_len, status;
	    unsigned short w[2];
	    // need to convert Unicode point c (in BMP) to the mbcs.
	    if ((void*)-1 == (cd = Riconv_open("UTF-8", UCS2ENC)))
		error(_("unknown encoding '%s' in 'PostScriptMetricInfo'"),
		      encoding);
	    /* Here we use terminated strings, but could use one char */
	    memset(str, 0, 10);
	    w[0] = (unsigned short) c; w[1] = 0;
	    //printf("converting %lc\n", w[0]);
	    i_buf = (char *)w;
	    i_len = 4;
	    o_buf = str;
	    o_len = 10;
	    status = Riconv(cd, &i_buf, (size_t *)&i_len,
			    (char **)&o_buf, (size_t *)&o_len);
	    Riconv_close(cd);
	    if (status == (size_t)-1) {
		*ascent = 0;
		*descent = 0;
		*width = 0;
		warning(_("Unicode character %lc (U+%04X) cannot be converted"),
			(wint_t)c, c);
		return;
	    }
	}
	char out[10]; // one Unicode point in an sbcs, with possible transliteration.
	// Do not show warnings (but do throw errors)
	mbcsToSbcs(str, out, encoding, CE_UTF8, 1);

	int a = -9999, d = 9999;
	short wx = 0;
	for (char *p = out; *p; p++) {
	    unsigned char c = *p;
#ifdef USE_HYPHEN
	    if (c == '-') c = PS_hyphen;
#endif
	    short w = metrics->CharInfo[c].WX;
	    // WX = NA_SHORT means that all the metrics are missing:
	    if(w == NA_SHORT)
		warning(_("font metrics unknown for character 0x%02x in encoding %s"), c, encoding);
	    /* FIXME;  The AFM spec says
	       B llx lly urx ury
	       (Optional.) Character bounding box where llx, lly, urx, and ury
	       are all numbers. If a character makes no marks on the page
	       (for example, the space character), this field reads B 0 0 0 0,
	       and these values are not considered when computing the FontBBox.

 	       Also NBSP and Euro in some fonts.  Should that be
 	       skipped for acent and descent?  However:
	       1) There are currently no transliterations including 
	       space/NBSP (although macOS did at one point).
	       2) The graphivs engine does not do this.
	    else if
	       (metrics->CharInfo[c].BBox[0] == 0 &&
		metrics->CharInfo[c].BBox[1] == 0 &&
		metrics->CharInfo[c].BBox[2] == 0 &&
		metrics->CharInfo[c].BBox[3] == 0) {
		warning(_("character 0x%02x in encoding %s makes no mark"), c, encoding);
                wx += w;
            }
	    */
	    else {
		wx += w;
		a = max(a, metrics->CharInfo[c].BBox[3]);
		d = min(d, metrics->CharInfo[c].BBox[1]);
		if (useKerning) {
		    unsigned char p1 = p[0], p2 = p[1];
		    for (int i =  metrics->KPstart[p1]; i < metrics->KPend[p1]; i++)
			if(metrics->KernPairs[i].c2 == p2 &&
			   metrics->KernPairs[i].c1 == p1) {
			    wx += metrics->KernPairs[i].kern;
			    break;
			}
		}
	    }
	}
	// printf("MetricInfo for %s: %d %d %d\n", out, wx, a, d);
	*width = 0.001 * wx;
	// the original 8-bit version treated missing as 0.
	if (a == -9999) a = 0;
	if (d == 9999) d = 0;
	*ascent =  0.001 * a;
	*descent = 0.001 * -d;
	return;
    } else {
	// 8-bit case
#ifdef USE_HYPHEN
	    if (c == '-') c = PS_hyphen;
#endif
	*ascent = 0.001 * metrics->CharInfo[c].BBox[3];
	*descent = -0.001 * metrics->CharInfo[c].BBox[1];
	short wx = metrics->CharInfo[c].WX;
	if(wx == NA_SHORT) {
	    warning(_("font metrics unknown for character 0x%02x in encoding %s"), c, encoding);
	    wx = 0;
	}
	*width = 0.001 * wx;
	return;
    }
}

static void
PostScriptCIDMetricInfo(int c, double *ascent, double *descent, double *width)
{
    /* calling in a SBCS is probably not intentional, but we should try to
       cope sensibly. */
    if(!mbcslocale && c > 0) {
	if (c > 255)
	    error(_("invalid character (%04X) sent to 'PostScriptCIDMetricInfo' in a single-byte locale"),
		  c);
	else {
	    /* convert to UCS-2 to use wcwidth. */
	    char str[2]={0,0};
	    R_ucs2_t out;
	    str[0] = (char) c;
	    if(mbcsToUcs2(str, &out, 1, CE_NATIVE) == (size_t)-1)
		error(_("invalid character sent to 'PostScriptCIDMetricInfo' in a single-byte locale"));
	    c = out;
	}
    }

    /* Design values for all CJK fonts */
    *ascent = 0.880;
    *descent = -0.120;
    if (c == 0 || c > 65535) *width = 1.;
    else {
#ifdef USE_RI18N_WIDTH
	*width = 0.5*Ri18n_wcwidth(c);
#else
	*width = 0.5*wcwidth(c);
#endif
    }
}


/*******************************************************
 * Data structures and functions for loading Type 1 fonts into an R session.
 *
 * Used by PostScript and PDF drivers.
 *
 * The idea is that font information is only loaded once for each font
 * within an R session.  Also, each encoding is only loaded once per
 * session.  A global list of loaded fonts and a global list of
 * loaded encodings are maintained.  Devices maintain their own list
 * of fonts and encodings used on the device;  the elements of these
 * lists are just pointers to the elements of the global lists.
 *
 * Cleaning up device lists just involves free'ing the lists themselves.
 * When the R session closes, the actual font and encoding information
 * is unloaded using the global lists.
 */

/*
 * Information about one Type 1 font
 */
typedef struct CIDFontInfo {
    char name[50];
} CIDFontInfo, *cidfontinfo;

typedef struct T1FontInfo {
    char name[50];
    FontMetricInfo metrics;
    CNAME charnames[256];
} Type1FontInfo, *type1fontinfo;

/*
 * Information about a font encoding
 */
typedef struct EncInfo {
    char encpath[R_PATH_MAX];
    char name[100]; /* Name written to PostScript/PDF file */
    char convname[50]; /* Name used in mbcsToSbcs() with iconv() */
    CNAME encnames[256];
    char enccode[5000];
} EncodingInfo, *encodinginfo;

/*
 * Information about a font family
 * (5 fonts representing plain, bold, italic, bolditalic, and symbol)
 *
 * The name is a graphics engine font family name
 * (distinct from the Type 1 font name)
 */
typedef struct CIDFontFamily {
    char fxname[50];
    cidfontinfo cidfonts[4];
    type1fontinfo symfont;
    char cmap[50];
    char encoding[50];
} CIDFontFamily, *cidfontfamily;

typedef struct T1FontFamily {
    char fxname[50];
    type1fontinfo fonts[5];
    encodinginfo encoding;
} Type1FontFamily, *type1fontfamily;

/*
 * A list of Type 1 font families
 *
 * Used to keep track of fonts currently loaded in the session
 * AND by each device to keep track of fonts currently used on the device.
 */
typedef struct CIDFontList {
    cidfontfamily cidfamily;
    struct CIDFontList *next;
} CIDFontList, *cidfontlist;

typedef struct T1FontList {
    type1fontfamily family;
    struct T1FontList *next;
} Type1FontList, *type1fontlist;

/*
 * Same as type 1 font list, but for encodings.
 */
typedef struct EncList {
    encodinginfo encoding;
    struct EncList *next;
} EncodingList, *encodinglist;

/*
 * Various constructors and destructors
 */
static cidfontinfo makeCIDFont(void)
{
    cidfontinfo font = (CIDFontInfo *) malloc(sizeof(CIDFontInfo));
    if (!font)
	warning(_("failed to allocate CID font info"));
    return font;
}

static type1fontinfo makeType1Font(void)
{
    type1fontinfo font = (Type1FontInfo *) malloc(sizeof(Type1FontInfo));
    if (font) {
	/*
	 * Initialise font->metrics.KernPairs to NULL
	 * so that we know NOT to free it if we fail to
	 * load this font and have to
	 * bail out and free this type1fontinfo
	 */
	font->metrics.KernPairs = NULL;    
    } else
	warning(_("failed to allocate Type 1 font info"));
    return font;
}

static void freeCIDFont(cidfontinfo font)
{
    free(font);
}

static void freeType1Font(type1fontinfo font)
{
    if (font->metrics.KernPairs)
	free(font->metrics.KernPairs);
    free(font);
}

static encodinginfo makeEncoding(void)
{
    encodinginfo encoding = (EncodingInfo *) malloc(sizeof(EncodingInfo));
    if (!encoding)
	warning(_("failed to allocate encoding info"));
    return encoding;
}

static void freeEncoding(encodinginfo encoding)
{
    free(encoding);
}

static cidfontfamily makeCIDFontFamily(void)
{
    cidfontfamily family = (CIDFontFamily *) malloc(sizeof(CIDFontFamily));
    if (family) {
	int i;
	for (i = 0; i < 4; i++)
	    family->cidfonts[i] = NULL;
	family->symfont = NULL;
    } else
	warning(_("failed to allocate CID font family"));
    return family;
}

static type1fontfamily makeFontFamily(void)
{
    type1fontfamily family = (Type1FontFamily *) malloc(sizeof(Type1FontFamily));
    if (family) {
	int i;
	for (i = 0; i < 5; i++)
	    family->fonts[i] = NULL;
	family->encoding = NULL;
    } else
	warning(_("failed to allocate Type 1 font family"));
    return family;
}
/*
 * Frees a font family, including fonts, but NOT encoding
 *
 * Used by global font list to free all fonts loaded in session
 * (should not be used by devices; else may free fonts more than once)
 *
 * Encodings are freed using the global encoding list
 * (to ensure that each encoding is only freed once)
 */
static void freeCIDFontFamily(cidfontfamily family)
{
    int i;
    for (i = 0; i < 4; i++)
	if (family->cidfonts[i])
	    freeCIDFont(family->cidfonts[i]);
    if (family->symfont)
	freeType1Font(family->symfont);
    free(family);
}

static void freeFontFamily(type1fontfamily family)
{
    int i;
    for (i=0; i<5; i++)
	if (family->fonts[i])
	    freeType1Font(family->fonts[i]);
    free(family);
}

static cidfontlist makeCIDFontList(void)
{
    cidfontlist fontlist = (CIDFontList *) malloc(sizeof(CIDFontList));
    if (fontlist) {
	fontlist->cidfamily = NULL;
	fontlist->next = NULL;
    } else
	warning(_("failed to allocate font list"));
    return fontlist;
}

static type1fontlist makeFontList(void)
{
    type1fontlist fontlist = (Type1FontList *) malloc(sizeof(Type1FontList));
    if (fontlist) {
	fontlist->family = NULL;
	fontlist->next = NULL;
    } else
	warning(_("failed to allocate font list"));
    return fontlist;
}

/*
 * Just free the Type1FontList structure, do NOT free elements it points to
 *
 * Used by both global font list and devices to free the font lists
 * (global font list separately takes care of the fonts pointed to)
 */
static void freeCIDFontList(cidfontlist fontlist) {
    /*
     * These will help to find any errors if attempt to
     * use freed font list.
     */
    fontlist->cidfamily = NULL;
    fontlist->next = NULL;
    free(fontlist);
}
static void freeFontList(type1fontlist fontlist) {
    /*
     * These will help to find any errors if attempt to
     * use freed font list.
     */
    fontlist->family = NULL;
    fontlist->next = NULL;
    free(fontlist);
}

static void freeDeviceCIDFontList(cidfontlist fontlist) {
    if (fontlist) {
	if (fontlist->next)
	    freeDeviceCIDFontList(fontlist->next);
	freeCIDFontList(fontlist);
    }
}
static void freeDeviceFontList(type1fontlist fontlist) {
    if (fontlist) {
	if (fontlist->next)
	    freeDeviceFontList(fontlist->next);
	freeFontList(fontlist);
    }
}

static encodinglist makeEncList(void)
{
    encodinglist enclist = (EncodingList *) malloc(sizeof(EncodingList));
    if (enclist) {
	enclist->encoding = NULL;
	enclist->next = NULL;
    } else
	warning(_("failed to allocated encoding list"));
    return enclist;
}

static void freeEncList(encodinglist enclist)
{
    enclist->encoding = NULL;
    enclist->next = NULL;
    free(enclist);
}

static void freeDeviceEncList(encodinglist enclist) {
    if (enclist) {
	if (enclist->next)
	    freeDeviceEncList(enclist->next);
	freeEncList(enclist);
    }
}

/*
 * Global list of fonts and encodings that have been loaded this session
 */
static cidfontlist loadedCIDFonts = NULL;
static type1fontlist loadedFonts = NULL;
static encodinglist loadedEncodings = NULL;
/*
 * There are separate PostScript and PDF font databases at R level
 * so MUST have separate C level records too
 * (because SAME device-independent font family name could map
 *  to DIFFERENT font for PostScript and PDF)
 */
static cidfontlist PDFloadedCIDFonts = NULL;
static type1fontlist PDFloadedFonts = NULL;
static encodinglist PDFloadedEncodings = NULL;

/*
 * Names of R level font databases
 */
static char PostScriptFonts[] = ".PostScript.Fonts";
static char PDFFonts[] = ".PDF.Fonts";

/*
 * Free the above globals
 *
 * NOTE that freeing the font families does NOT free the encodings
 * Hence we free all encodings first.
 */

/* NB this is exported, and was at some point used by KillAllDevices
   in src/main/graphics.c.  That would be a problem now it is in a
   separate DLL.
*/
#if 0
void freeType1Fonts()
{
    encodinglist enclist = loadedEncodings;
    type1fontlist fl = loadedFonts;
    cidfontlist   cidfl = loadedCIDFonts;
    type1fontlist pdffl = PDFloadedFonts;
    cidfontlist   pdfcidfl = PDFloadedCIDFonts;
    while (enclist) {
	enclist = enclist->next;
	freeEncoding(loadedEncodings->encoding);
	freeEncList(loadedEncodings);
	loadedEncodings = enclist;
    }
    while (fl) {
	fl = fl->next;
	freeFontFamily(loadedFonts->family);
	freeFontList(loadedFonts);
	loadedFonts = fl;
    }
    while (cidfl) {
	cidfl = cidfl->next;
	freeCIDFontFamily(loadedCIDFonts->cidfamily);
	freeCIDFontList(loadedCIDFonts);
	loadedCIDFonts = cidfl;
    }
    while (pdffl) {
	pdffl = pdffl->next;
	freeFontFamily(PDFloadedFonts->family);
	freeFontList(PDFloadedFonts);
	PDFloadedFonts = pdffl;
    }
    while (pdfcidfl) {
	pdfcidfl = pdfcidfl->next;
	freeCIDFontFamily(PDFloadedCIDFonts->cidfamily);
	freeCIDFontList(PDFloadedCIDFonts);
	PDFloadedCIDFonts = pdfcidfl;
    }
}
#endif

/*
 * Given a path to an encoding file,
 * find an EncodingInfo that corresponds
 */
static encodinginfo
findEncoding(const char *encpath, encodinglist deviceEncodings, bool isPDF)
{
    encodinglist enclist = isPDF ? PDFloadedEncodings : loadedEncodings;
    encodinginfo encoding = NULL;
    int found = 0;
    /*
     * "default" is a special encoding which means use the
     * default (FIRST) encoding set up ON THIS DEVICE.
     */
    if (!strcmp(encpath, "default")) {
	found = 1;
	// called from PDFDeviceDriver with null deviceEncodings as last resort
	if (deviceEncodings) encoding = deviceEncodings->encoding;
    } else {
	while (enclist && !found) {
	    found = !strcmp(encpath, enclist->encoding->encpath);
	    if (found)
		encoding = enclist->encoding;
	    enclist = enclist->next;
	}
    }
    return encoding;
}

/*
 * Find an encoding in device encoding list
 */
static encodinginfo
findDeviceEncoding(const char *encpath, encodinglist enclist, int *index)
{
    encodinginfo encoding = NULL;
    int found = 0;
    *index = 0;
    while (enclist && !found) {
	found = !strcmp(encpath, enclist->encoding->encpath);
	if (found)
	    encoding = enclist->encoding;
	enclist = enclist->next;
	*index = *index + 1;
    }
    return encoding;
}

/*
 * Utility to avoid string overrun
 */
static void safestrcpy(char *dest, const char *src, int maxlen)
{
    if (strlen(src) < maxlen)
	strcpy(dest, src);
    else {
	warning(_("truncated string which was too long for copy"));
	strncpy(dest, src, maxlen-1);
	dest[maxlen-1] = '\0';
    }
}

/*
 * Add an encoding to the list of loaded encodings ...
 *
 * ... and return the new encoding
 */
static encodinginfo addEncoding(const char *encpath, bool isPDF)
{
    encodinginfo encoding = makeEncoding();
    if (encoding) {
	if (LoadEncoding(encpath,
			 encoding->name,
			 encoding->convname,
			 encoding->encnames,
			 encoding->enccode,
			 isPDF)) {
	    encodinglist newenc = makeEncList();
	    if (!newenc) {
		freeEncoding(encoding);
		encoding = NULL;
	    } else {
		encodinglist enclist =
		    isPDF ? PDFloadedEncodings : loadedEncodings;
		safestrcpy(encoding->encpath, encpath, R_PATH_MAX);
		newenc->encoding = encoding;
		if (!enclist) {
		    if(isPDF) PDFloadedEncodings = newenc;
		    else loadedEncodings = newenc;
		} else {
		    while (enclist->next)
			enclist = enclist->next;
		    enclist->next = newenc;
		}
	    }
	} else {
	    warning(_("failed to load encoding file '%s'"), encpath);
	    freeEncoding(encoding);
	    encoding = NULL;
	}
    } else
	encoding = NULL;
    return encoding;
}

/*
 * Add an encoding to a list of device encodings ...
 *
 * ... and return the new list
 */
static encodinglist addDeviceEncoding(encodinginfo encoding,
				      encodinglist devEncs)
{
    encodinglist newenc = makeEncList();
    if (!newenc) {
	devEncs = NULL;
    } else {
	encodinglist enclist = devEncs;
	newenc->encoding = encoding;
	if (!devEncs)
	    devEncs = newenc;
	else {
	    while (enclist->next)
		enclist = enclist->next;
	    enclist->next = newenc;
	}
    }
    return devEncs;
}

/*
 * Given a graphics engine font family name,
 * find a Type1FontFamily that corresponds
 *
 * If get fxname match, check whether the encoding in the
 * R database is "default"
 * (i.e., the graphics engine font family encoding is unspecified)
 * If it is "default" then check that the loaded encoding is the
 * same as the encoding we want.  A matching encoding is defined
 * as one which leads to the same iconvname (see seticonvName()).
 * This could perhaps be made more rigorous by actually looking inside
 * the relevant encoding file for the encoding name.
 *
 * If the encoding we want is NULL, then we just don't care.
 *
 * Returns NULL if can't find font in loadedFonts
 */

static const char *getFontEncoding(const char *family, const char *fontdbname);

static type1fontfamily
findLoadedFont(const char *name, const char *encoding, bool isPDF)
{
    type1fontlist fontlist;
    type1fontfamily font = NULL;
    char *fontdbname;
    int found = 0;

    if (isPDF) {
	fontlist = PDFloadedFonts;
	fontdbname = PDFFonts;
    } else {
	fontlist = loadedFonts;
	fontdbname = PostScriptFonts;
    }
    while (fontlist && !found) {
	found = !strcmp(name, fontlist->family->fxname);
	if (found) {
	    font = fontlist->family;
	    if (encoding) {
		char encconvname[50];
		const char *encname = getFontEncoding(name, fontdbname);
		// encname could be NULL
		if(encname) {
		    seticonvName(encoding, encconvname);
		    if (!strcmp(encname, "default") &&
			strcmp(fontlist->family->encoding->convname,
			       encconvname)) {
			font = NULL;
			found = 0;
		    }
		} else {
		    font = NULL;
		    found = 0;
		}
	    }
	}
	fontlist = fontlist->next;
    }
    return font;
}

SEXP Type1FontInUse(SEXP name, SEXP isPDF)
{
    if (!isString(name) || LENGTH(name) > 1)
	error(_("invalid font name or more than one font name"));
    return ScalarLogical(
	findLoadedFont(CHAR(STRING_ELT(name, 0)), NULL, asBool(isPDF))
	!= NULL);
}

static cidfontfamily findLoadedCIDFont(const char *family, bool isPDF)
{
    cidfontlist fontlist;
    cidfontfamily font = NULL;
    int found = 0;

    if (isPDF) {
	fontlist = PDFloadedCIDFonts;
    } else {
	fontlist = loadedCIDFonts;
    }
    while (fontlist && !found) {
	found = !strcmp(family, fontlist->cidfamily->cidfonts[0]->name);
	if (found)
	    font = fontlist->cidfamily;
	fontlist = fontlist->next;
    }
#ifdef PS_DEBUG
    if(found)
	Rprintf("findLoadedCIDFont found = %s\n",family);
#endif
    return font;
}

// called from grDevices:::checkFontInUse
SEXP CIDFontInUse(SEXP name, SEXP isPDF)
{
    if (!isString(name) || LENGTH(name) > 1)
	error(_("invalid font name or more than one font name"));
    return ScalarLogical(
	findLoadedCIDFont(CHAR(STRING_ELT(name, 0)), asBool(isPDF))
	!= NULL);
}

/*
 * Find a font in device font list
 */
static cidfontfamily
findDeviceCIDFont(const char *name, cidfontlist fontlist, int *index)
{
    cidfontfamily font = NULL;
    int found = 0;
    *index = 0;
    /*
     * If the graphics engine font family is ""
     * just use the default font that was loaded when the device
     * was created.
     * This will (MUST) be the first font in the device
     */
#ifdef DEBUG_PS
    Rprintf("findDeviceCIDFont=%s\n", name);
    Rprintf("? cidfontlist %s\n", (fontlist) ? "found" : "not found");
#endif

    if (strlen(name) > 0) {
	while (fontlist && !found) {
#ifdef DEBUG_PS
	    Rprintf("findDeviceCIDFont=%s\n", name);
	    Rprintf("findDeviceCIDFont fontlist->cidfamily->name=%s\n",
		    fontlist->cidfamily->fxname);
#endif

	    found = !strcmp(name, fontlist->cidfamily->fxname);
	    if (found)
		font = fontlist->cidfamily;
	    fontlist = fontlist->next;
	    *index = *index + 1;
	}
    } else {
	font = fontlist->cidfamily;
	*index = 1;
    }
#ifdef DEBUG_PS
    Rprintf("findDeviceCIDFont find index=%d\n", *index);
    Rprintf("findDeviceCIDFont find font=%s\n", (font) ? "Found" : "NULL");
#endif
    return font;
}

/*
 * Must only be called once a device has at least one font added
 * (i.e., after the default font has been added)
 */
static type1fontfamily
findDeviceFont(const char *name, type1fontlist fontlist, int *index)
{
    type1fontfamily font = NULL;
    int found = 0;
    *index = 0;
    /*
     * If the graphics engine font family is ""
     * just use the default font that was loaded when the device
     * was created.
     * This will (MUST) be the first font in the device
     */
    if (strlen(name) > 0) {
	while (fontlist && !found) {
	    found = !strcmp(name, fontlist->family->fxname);
	    if (found)
		font = fontlist->family;
	    fontlist = fontlist->next;
	    *index = *index + 1;
	}
    } else {
	font = fontlist->family;
	*index = 1;
    }
    return font;
}

/*
 * Get an R-level font database
 */
static SEXP getFontDB(const char *fontdbname) {
    SEXP graphicsNS, PSenv;
    SEXP fontdb;
    PROTECT(graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT(PSenv = findVar(install(".PSenv"), graphicsNS));
    /* under lazy loading this will be a promise on first use */
    if(TYPEOF(PSenv) == PROMSXP) {
	PROTECT(PSenv);
	PSenv = eval(PSenv, graphicsNS);
	UNPROTECT(2);
	PROTECT(PSenv);
    }
    PROTECT(fontdb = findVar(install(fontdbname), PSenv));
    UNPROTECT(3);
    return fontdb;
}

/*
 * Get an R-level font object
 */
static SEXP getFont(const char *family, const char *fontdbname) {
    int i, nfonts;
    SEXP result = R_NilValue;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    SEXP fontnames;
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    result = VECTOR_ELT(fontdb, i);
	}
    }
    if (!found)
	warning(_("font family '%s' not found in PostScript font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Get the path to the afm file for a user-specifed font
 * given a graphics engine font family and the face
 * index (0..4)
 *
 * Do this by looking up the font name in the PostScript
 * font database
 */
static const char*
fontMetricsFileName(const char *family, int faceIndex,
		    const char *fontdbname)
{
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    SEXP fontnames;
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i = 0; i < nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 1 means vector of font afm file paths */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 1),
				     faceIndex));
	}
    }
    if (!found)
	warning(_("font family '%s' not found in PostScript font database"),
		family);
    UNPROTECT(2);
    return result;
}

static const char *getFontType(const char *family, const char *fontdbname)
{
    const char *result = NULL;
    SEXP font = getFont(family, fontdbname);
    if (!isNull(font)) {
        result = CHAR(STRING_ELT(getAttrib(font, R_ClassSymbol), 0));
    }
    return result;
}

static bool isType1Font(const char *family, const char *fontdbname,
			    type1fontfamily defaultFont)
{
    /*
     * If family is "" then we're referring to the default device
     * font, so the test is just whether the default font is
     * type1
     *
     * If loading font, send NULL for defaultFont
     */
    if (strlen(family) == 0) {
	if (defaultFont)
	    return true;
	else
	    return false;
    } else {
        const char *fontType = getFontType(family, fontdbname);
        if (fontType) 
            return !strcmp(fontType, "Type1Font");
        else
            return false;
    }
}

static bool isCIDFont(const char *family, const char *fontdbname,
			  cidfontfamily defaultCIDFont) {
    /*
     * If family is "" then we're referring to the default device
     * font, so the test is just whether the default font is
     * type1
     *
     * If loading font, send NULL for defaultCIDFont
     */
    if (strlen(family) == 0) {
	if (defaultCIDFont)
	    return true;
	else
	    return false;
    } else {
        const char *fontType = getFontType(family, fontdbname);
        if (fontType) 
            return !strcmp(fontType, "CIDFont");
        else
            return false;
    }
}

/*
 * Get encoding name from font database
 */
static const char *getFontEncoding(const char *family, const char *fontdbname)
{
    SEXP fontnames;
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 2 means 'encoding' element */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 2), 0));
	}
    }
    if (!found)
	warning(_("font encoding for family '%s' not found in font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Get Font name from font database
 */
static const char *getFontName(const char *family, const char *fontdbname)
{
    SEXP fontnames;
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 0 means 'family' element */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 0), 0));
	}
    }
    if (!found)
	warning(_("font CMap for family '%s' not found in font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Get CMap name from font database
 */
static const char *getFontCMap(const char *family, const char *fontdbname)
{
    SEXP fontnames;
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 2 means 'cmap' element */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 2), 0));
	}
    }
    if (!found)
	warning(_("font CMap for family '%s' not found in font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Get Encoding name from CID font in font database
 */
static const char *
getCIDFontEncoding(const char *family, const char *fontdbname)
{
    SEXP fontnames;
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(fontdbname));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 3 means 'encoding' element */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 3), 0));
	}
    }
    if (!found)
	warning(_("font encoding for family '%s' not found in font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Get Encoding name from CID font in font database
 */
static const char *getCIDFontPDFResource(const char *family)
{
    SEXP fontnames;
    int i, nfonts;
    const char *result = NULL;
    int found = 0;
    SEXP fontdb = PROTECT(getFontDB(PDFFonts));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    for (i=0; i<nfonts && !found; i++) {
	const char *fontFamily = CHAR(STRING_ELT(fontnames, i));
	if (strcmp(family, fontFamily) == 0) {
	    found = 1;
	    /* 4 means 'pdfresource' element */
	    result = CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(fontdb, i), 4), 0));
	}
    }
    if (!found)
	warning(_("font encoding for family '%s' not found in font database"),
		family);
    UNPROTECT(2);
    return result;
}

/*
 * Add a graphics engine font family/encoding to the list of loaded fonts ...
 *
 * ... and return the new font
 */
static cidfontfamily addLoadedCIDFont(cidfontfamily font, bool isPDF)
{
    cidfontlist newfont = makeCIDFontList();
    if (!newfont) {
	freeCIDFontFamily(font);
	font = NULL;
    } else {
	cidfontlist fontlist;
	if (isPDF)
	    fontlist = PDFloadedCIDFonts;
	else
	    fontlist = loadedCIDFonts;
	newfont->cidfamily = font;
	if (!fontlist) {
	    if (isPDF)
		PDFloadedCIDFonts = newfont;
	    else
		loadedCIDFonts = newfont;
	} else {
	    while (fontlist->next)
		fontlist = fontlist->next;
	    fontlist->next = newfont;
	}
    }
    return font;
}
static type1fontfamily addLoadedFont(type1fontfamily font, bool isPDF)
{
    type1fontlist newfont = makeFontList();
    if (!newfont) {
	freeFontFamily(font);
	font = NULL;
    } else {
	type1fontlist fontlist;
	if (isPDF)
	    fontlist = PDFloadedFonts;
	else
	    fontlist = loadedFonts;
	newfont->family = font;
	if (!fontlist) {
	    if (isPDF)
		PDFloadedFonts = newfont;
	    else
		loadedFonts = newfont;
	} else {
	    while (fontlist->next)
		fontlist = fontlist->next;
	    fontlist->next = newfont;
	}
    }
    return font;
}

/*
 * Add a font from a graphics engine font family name
 */
static cidfontfamily addCIDFont(const char *name, bool isPDF)
{
    cidfontfamily fontfamily = makeCIDFontFamily();
    char *fontdbname;
    if (isPDF)
	fontdbname = PDFFonts;
    else
	fontdbname = PostScriptFonts;
    if (fontfamily) {
	int i;
	const char *cmap = getFontCMap(name, fontdbname);
	if (!cmap) {
	    freeCIDFontFamily(fontfamily);
	    fontfamily = NULL;
	} else {
	    /*
	     * Set the name of the font
	     */
	    safestrcpy(fontfamily->fxname, name, 50);
	    /*
	     * Get the font CMap
	     */
	    safestrcpy(fontfamily->cmap, cmap, 50);
	    /*
	     * Get the font Encoding (name)
	     *
	     * If we have got here then we know there is a
	     * match in the font database because we already
	     * have the CMap => don't need to check for failure
	     */
	    safestrcpy(fontfamily->encoding,
		       getCIDFontEncoding(name, fontdbname), 50);
	    /*
	     * Load font info
	     */
	    for(i = 0; i < 4; i++) {
		fontfamily->cidfonts[i] = makeCIDFont();
		/*
		 * Use name from R object font database.
		 */
		safestrcpy(fontfamily->cidfonts[i]->name,
			   getFontName(name, fontdbname), 50);
	    }
	    /*
	     * Load the (Type 1!) symbol font
	     *
	     * Gratuitous loop of length 1 so "break" jumps to end of loop
	     */
	    for (i = 0; i < 1; i++) {
		type1fontinfo font = makeType1Font();
		const char *afmpath = fontMetricsFileName(name, 4, fontdbname);
		if (!font) {
		    freeCIDFontFamily(fontfamily);
		    fontfamily = NULL;
		    break;
		}
		if (!afmpath) {
		    freeCIDFontFamily(fontfamily);
		    fontfamily = NULL;
		    freeType1Font(font);
		    break;
		}
		fontfamily->symfont = font;
		if (!PostScriptLoadFontMetrics(afmpath,
					       &(fontfamily->symfont->metrics),
					       fontfamily->symfont->name,
					       fontfamily->symfont->charnames,
					       /*
						* Reencode all but
						* symbol face
						*/
					       NULL, 0)) {
		    warning(_("cannot load afm file '%s'"), afmpath);
		    freeCIDFontFamily(fontfamily);
		    fontfamily = NULL;
		    break;
		}
	    }
	    /*
	     * Add font
	     */
	    if (fontfamily)
		fontfamily = addLoadedCIDFont(fontfamily, isPDF);
	}
    } else
	fontfamily = NULL;
#ifdef DEBUG_PS
    Rprintf("%d fontfamily =  %s\n", __LINE__, (fontfamily) ? "set" : "null");
    Rprintf("%d addCIDFont = %s\n", __LINE__, fontfamily->fxname);
#endif
    return fontfamily;
}

static type1fontfamily addFont(const char *name, bool isPDF,
			       encodinglist deviceEncodings)
{
    type1fontfamily fontfamily = makeFontFamily();
    char *fontdbname;
    if (isPDF)
	fontdbname = PDFFonts;
    else
	fontdbname = PostScriptFonts;
    if (fontfamily) {
	int i;
	encodinginfo encoding;
	const char *encpath = getFontEncoding(name, fontdbname);
	if (!encpath) {
	    freeFontFamily(fontfamily);
	    fontfamily = NULL;
	} else {
	    /*
	     * Set the name of the font
	     */
	    safestrcpy(fontfamily->fxname, name, 50);
	    /*
	     * Find or add encoding
	     */
	    if (!(encoding = findEncoding(encpath, deviceEncodings, isPDF)))
		encoding = addEncoding(encpath, isPDF);
	    if (!encoding) {
		freeFontFamily(fontfamily);
		fontfamily = NULL;
	    } else {
		/*
		 * Load font info
		 */
		fontfamily->encoding = encoding;
		for(i = 0; i < 5 ; i++) {
		    type1fontinfo font = makeType1Font();
		    const char *afmpath = fontMetricsFileName(name, i, fontdbname);
		    if (!font) {
			freeFontFamily(fontfamily);
			fontfamily = NULL;
			break;
		    }
		    if (!afmpath) {
			freeFontFamily(fontfamily);
			fontfamily = NULL;
			freeType1Font(font);
			break;
		    }
		    fontfamily->fonts[i] = font;
		    if (!PostScriptLoadFontMetrics(afmpath,
						   &(fontfamily->fonts[i]->metrics),
						   fontfamily->fonts[i]->name,
						   fontfamily->fonts[i]->charnames,
						   /*
						    * Reencode all but
						    * symbol face
						    */
						   encoding->encnames,
						   (i < 4)?1:0)) {
			warning(_("cannot load afm file '%s'"), afmpath);
			freeFontFamily(fontfamily);
			fontfamily = NULL;
			break;
		    }
		}
		/*
		 * Add font
		 */
		if (fontfamily)
		    fontfamily = addLoadedFont(fontfamily, isPDF);
	    }
	}
    } else
	fontfamily = NULL;
    return fontfamily;
}

/*
 * Add a default font family/encoding to the list of loaded fonts ...
 *
 * ... using a set of AFM paths ...
 *
 * ... and return the new font
 */

static type1fontfamily
addDefaultFontFromAFMs(const char *encpath, const char **afmpaths,
		       bool isPDF,
		       encodinglist deviceEncodings)
{
    encodinginfo encoding;
    type1fontfamily fontfamily = makeFontFamily();
    if (fontfamily) {
	int i;
	if (!(encoding = findEncoding(encpath, deviceEncodings, isPDF)))
	    encoding = addEncoding(encpath, isPDF);
	if (!encoding) {
	    freeFontFamily(fontfamily);
	    fontfamily = NULL;
	} else {
	    /*
	     * This is the device default font, so set the
	     * graphics engine font family name to ""
	     */
	    fontfamily->fxname[0] ='\0';
	    /*
	     * Load font info
	     */
	    fontfamily->encoding = encoding;
	    for(i = 0; i < 5 ; i++) {
		type1fontinfo font = makeType1Font();
		if (!font) {
		    freeFontFamily(fontfamily);
		    fontfamily = NULL;
		    break;
		}
		fontfamily->fonts[i] = font;
		if (!PostScriptLoadFontMetrics(afmpaths[i],
					       &(fontfamily->fonts[i]->metrics),
					       fontfamily->fonts[i]->name,
					       fontfamily->fonts[i]->charnames,
					       /*
						* Reencode all but
						* symbol face
						*/
					       encoding->encnames,
					       (i < 4)?1:0)) {
		    warning(_("cannot load afm file '%s'"), afmpaths[i]);
		    freeFontFamily(fontfamily);
		    fontfamily = NULL;
		    break;
		}
	    }
	    /*
	     * Add font
	     */
	    if (fontfamily)
		fontfamily = addLoadedFont(fontfamily, isPDF);
	}
    } else
	fontfamily = NULL;
    return fontfamily;
}

/*
 * Add a graphics engine font family/encoding to a list of device fonts ...
 *
 * ... and return the new font list
 */
static cidfontlist addDeviceCIDFont(cidfontfamily font,
				    cidfontlist devFonts,
				    int *index)
{
    cidfontlist newfont = makeCIDFontList();
    *index = 0;
    if (!newfont) {
	devFonts = NULL;
    } else {
	cidfontlist fontlist = devFonts;
	newfont->cidfamily = font;
	*index = 1;
	if (!devFonts) {
	    devFonts = newfont;
	} else {
	    while (fontlist->next) {
		fontlist = fontlist->next;
		*index = *index + 1;
	    }
	    fontlist->next = newfont;
	}
    }
    return devFonts;
}
static type1fontlist addDeviceFont(type1fontfamily font,
				   type1fontlist devFonts,
				   int *index)
{
    type1fontlist newfont = makeFontList();
    *index = 0;
    if (!newfont) {
	devFonts = NULL;
    } else {
	type1fontlist fontlist = devFonts;
	newfont->family = font;
	*index = 1;
	if (!devFonts) {
	    devFonts = newfont;
	} else {
	    while (fontlist->next) {
		fontlist = fontlist->next;
		*index = *index + 1;
	    }
	    fontlist->next = newfont;
	}
    }
    return devFonts;
}

/*
***********************************************************
*/

/* Part 2.  Device Driver State. */

typedef struct {
    char filename[R_PATH_MAX];
    int open_type;

    char papername[64];	/* paper name */
    int paperwidth;	/* paper width in big points (1/72 in) */
    int paperheight;	/* paper height in big points */
    bool landscape;	/* landscape mode */
    int pageno;		/* page number */
    int fileno;		/* file number */

    int maxpointsize;

    double width;	/* plot width in inches */
    double height;	/* plot height in inches */
    double pagewidth;	/* page width in inches */
    double pageheight;	/* page height in inches */
    bool pagecentre;/* centre image on page? */
    bool printit;	/* print page at close? */
    char command[2*R_PATH_MAX];
    char title[1024];
    char colormodel[30];

    FILE *psfp;		/* output file */

    bool onefile;	/* EPSF header etc*/
    bool paperspecial;	/* suppress %%Orientation */
    bool warn_trans; /* have we warned about translucent cols? */
    bool useKern;
    bool fillOddEven; /* polygon fill mode */

    /* This group of variables track the current device status.
     * They should only be set by routines that emit PostScript code. */
    struct {
	double lwd;		 /* line width */
	int lty;		 /* line type */
	R_GE_lineend lend;
	R_GE_linejoin ljoin;
	double lmitre;
	int font;
	int cidfont;
	int fontsize;	         /* font size in points */
	rcolor col;		 /* color */
	rcolor fill;	         /* fill color */
    } current;

    /*
     * Fonts and encodings used on the device
     */
    type1fontlist fonts;
    cidfontlist   cidfonts;
    encodinglist  encodings;
    /*
     * These next two just record the default device font
     */
    type1fontfamily defaultFont;
    cidfontfamily   defaultCIDFont;
}
PostScriptDesc;

/*  Part 3.  Graphics Support Code.  */

static void specialCaseCM(FILE *fp, type1fontfamily family, int familynum)
{
	fprintf(fp, "%% begin encoding\n");
	fprintf(fp, "/SymbolEncoding [\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /space /exclam /universal /numbersign /existential /percent /ampersand /suchthat\n");
	fprintf(fp, " /parenleft /parenright /asteriskmath /plus /comma /minus /period /slash\n");
	fprintf(fp, " /zero /one /two /three /four /five /six /seven\n");
	fprintf(fp, " /eight /nine /colon /semicolon /less /equal /greater /question\n");
	fprintf(fp, " /congruent /Alpha /Beta /Chi /Delta /Epsilon /Phi /Gamma\n");
	fprintf(fp, " /Eta /Iota /theta1 /Kappa /Lambda /Mu /Nu /Omicron\n");
	fprintf(fp, " /Pi /Theta /Rho /Sigma /Tau /Upsilon /sigma1 /Omega\n");
	fprintf(fp, " /Xi /Psi /Zeta /bracketleft /therefore /bracketright /perpendicular /underscore\n");
	fprintf(fp, " /radicalex /alpha /beta /chi /delta /epsilon /phi /gamma\n");
	fprintf(fp, " /eta /iota /phi1 /kappa /lambda /mu /nu /omicron\n");
	fprintf(fp, " /pi /theta /rho /sigma /tau /upsilon /omega1 /omega\n");
	fprintf(fp, " /xi /psi /zeta /braceleft /bar /braceright /similar /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef /.notdef\n");
	fprintf(fp, " /Euro /Upsilon1 /minute /lessequal /fraction /infinity /florin /club\n");
	fprintf(fp, " /diamond /heart /spade /arrowboth /arrowleft /arrowup /arrowright /arrowdown\n");
	fprintf(fp, " /degree /plusminus /second /greaterequal /multiply /proportional /partialdiff /bullet\n");
	fprintf(fp, " /divide /notequal /equivalence /approxequal /ellipsis /arrowvertex /arrowhorizex /carriagereturn\n");
	fprintf(fp, " /aleph /Ifraktur /Rfraktur /weierstrass /circlemultiply /circleplus /emptyset /intersection\n");
	fprintf(fp, " /union /propersuperset /reflexsuperset /notsubset /propersubset /reflexsubset /element /notelement\n");
	fprintf(fp, " /angle /gradient /registerserif /copyrightserif /trademarkserif /product /radical /dotmath\n");
	fprintf(fp, " /logicalnot /logicaland /logicalor /arrowdblboth /arrowdblleft /arrowdblup /arrowdblright /arrowdbldown\n");
	fprintf(fp, " /lozenge /angleleft /registersans /copyrightsans /trademarksans /summation /parenlefttp /parenleftex\n");
	fprintf(fp, " /parenleftbt /bracketlefttp /bracketleftex /bracketleftbt /bracelefttp /braceleftmid /braceleftbt /braceex\n");
	fprintf(fp, " /.notdef /angleright /integral /integraltp /integralex /integralbt /parenrighttp /parenrightex\n");
	fprintf(fp, " /parenrightbt /bracketrighttp /bracketrightex /bracketrightbt /bracerighttp /bracerightmid /bracerightbt /.notdef\n");
	fprintf(fp, "] def\n");
	fprintf(fp, "%% end encoding\n");
	fprintf(fp, "/mergefonts\n");
	fprintf(fp, "{ /targetencoding exch def\n");
	fprintf(fp, "  /fontarray exch def\n");
	fprintf(fp, "  fontarray 0 get dup maxlength dict begin\n");
	fprintf(fp, "  { 1 index /FID ne { def } { pop pop } ifelse } forall\n");
	fprintf(fp, "  %% Create a new dictionary\n");
	fprintf(fp, "  /CharStrings 256 dict def\n");
	fprintf(fp, "  %% Add a definition of .notdef\n");
	fprintf(fp, "  fontarray\n");
	fprintf(fp, "  { /CharStrings get dup /.notdef known\n");
	fprintf(fp, "    { /.notdef get /result exch def exit }\n");
	fprintf(fp, "    { pop } ifelse\n");
	fprintf(fp, "  } forall\n");
	fprintf(fp, "  CharStrings /.notdef result put\n");
	fprintf(fp, "  %% Add in the other definitions\n");
	fprintf(fp, "  targetencoding\n");
	fprintf(fp, "  { /code exch def\n");
	fprintf(fp, "    %% Check that it is not a .notdef\n");
	fprintf(fp, "    code /.notdef eq\n");
	fprintf(fp, "    { /.notdef }\n");
	fprintf(fp, "    { fontarray\n");
	fprintf(fp, "      { /CharStrings get dup code known\n");
	fprintf(fp, "        { code get /result exch def /found true def exit }\n");
	fprintf(fp, "        { pop /found false def } ifelse\n");
	fprintf(fp, "      } forall\n");
	fprintf(fp, "      %% define character if it was found and accumulate encoding\n");
	fprintf(fp, "      found { CharStrings code result put code } { /.notdef } ifelse\n");
	fprintf(fp, "    } ifelse\n");
	fprintf(fp, "  } forall\n");
	fprintf(fp, "  %% grab new encoding off of stack\n");
	fprintf(fp, "  256 array astore /Encoding exch def\n");
	fprintf(fp, "  %% Undefine some local variables\n");
	fprintf(fp, "  currentdict /fontarray undef\n");
	fprintf(fp, "  currentdict /targetencoding undef\n");
	fprintf(fp, "  currentdict /code undef\n");
	fprintf(fp, "  currentdict /result undef\n");
	fprintf(fp, "  currentdict /found undef\n");
	fprintf(fp, "  %% Leave new font on the stack\n");
	fprintf(fp, "  currentdict\n");
	fprintf(fp, "  end\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "%%%%IncludeResource: font %s\n",
		family->fonts[0]->name);
	fprintf(fp, "%%%%IncludeResource: font CMSY10\n");
	fprintf(fp, "[ /%s findfont /CMSY10 findfont ] %s mergefonts\n",
		family->fonts[0]->name, family->encoding->name);
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + 1);
	fprintf(fp, "%%%%IncludeResource: font %s\n",
		family->fonts[1]->name);
	fprintf(fp, "%%%%IncludeResource: font CMBSY10\n");
	fprintf(fp, "[ /%s findfont /CMBSY10 findfont ] %s mergefonts\n",
		family->fonts[1]->name, family->encoding->name);
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + 2);
	fprintf(fp, "%%%%IncludeResource: font %s\n",
		family->fonts[2]->name);
	fprintf(fp, "[ /%s findfont /CMSY10 findfont ] %s mergefonts\n",
		family->fonts[2]->name, family->encoding->name);
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + 3);
	fprintf(fp, "%%%%IncludeResource: font %s\n",
		family->fonts[3]->name);
	fprintf(fp, "[ /%s findfont /CMBSY10 findfont ] %s mergefonts\n",
		family->fonts[3]->name, family->encoding->name);
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + 4);
	fprintf(fp, "%%%%IncludeResource: font CMMI10\n");
	fprintf(fp, "[ /CMR10 findfont /CMSY10 findfont /CMMI10 findfont ] SymbolEncoding mergefonts\n");
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + 5);
}

static void PSEncodeFonts(FILE *fp, PostScriptDesc *pd)
{
    type1fontlist fonts = pd->fonts;
    int familynum = 1;
    int haveWrittenDefaultEnc = 0;
    cidfontlist cidfonts = pd->cidfonts;
    int cidfamilynum = 1;

    while (fonts) {
	int dontcare;
	/*
	 * Has the encoding already been used on the device?
	 */
	encodinginfo encoding =
	    findDeviceEncoding(fonts->family->encoding->encpath,
			       pd->encodings, &dontcare);
	/*
	 * If we've added the encoding to the device then it has been
	 * written to file ...
	 *
	 * ... UNLESS this is the default encoding for the device, in
	 * which case it has been added, but not written to file.
	 *
	 * Use haveWrittenDefaultEnc to make sure we only do it once.
	 */
	if (!encoding ||
	    (encoding == pd->encodings->encoding && !haveWrittenDefaultEnc)) {
	    /*
	     * Don't need to add default encoding again.
	     */
	    if (encoding != pd->encodings->encoding) {
		/*
		 * The encoding should have been loaded when the
		 * font was loaded
		 */
		encoding = findEncoding(fonts->family->encoding->encpath,
					pd->encodings, FALSE);
		if (!encoding)
		    warning(_("corrupt loaded encodings;  encoding not recorded"));
		else {
		    /*
		     * Record encoding on device's list of encodings so
		     * don't write same encoding more than once
		     */
		    encodinglist enclist = addDeviceEncoding(encoding,
							     pd->encodings);
		    if (enclist)
			pd->encodings = enclist;
		    else
			warning(_("failed to record device encoding"));
		}
	    } else {
		/*
		 * Make sure we only write default encoding once.
		 */
		haveWrittenDefaultEnc = 1;
	    }
	    /*
	     * Include encoding unless it is ISOLatin1Encoding,
	     * which is predefined
	     */
	    if (strcmp(fonts->family->encoding->name, "ISOLatin1Encoding"))
		fprintf(fp, "%% begin encoding\n%s def\n%% end encoding\n",
			fonts->family->encoding->enccode);
	}
	if(strcmp(fonts->family->fonts[4]->name,
		  "CMSY10 CMBSY10 CMMI10") == 0) {
	    /* use different ps fragment for CM fonts */
	    specialCaseCM(fp, fonts->family, familynum);
	} else {
	    int i;
	    for (i = 0; i < 4 ; i++) {
		fprintf(fp, "%%%%IncludeResource: font %s\n",
			fonts->family->fonts[i]->name);
		fprintf(fp, "/%s findfont\n",
			fonts->family->fonts[i]->name);
		fprintf(fp, "dup length dict begin\n");
		fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
		fprintf(fp, "  /Encoding %s def\n",
			fonts->family->encoding->name);
		fprintf(fp, "  currentdict\n");
		fprintf(fp, "  end\n");
		fprintf(fp, "/Font%d exch definefont pop\n",
			(familynum - 1)*5 + i + 1);
	    }
	    fprintf(fp, "%%%%IncludeResource: font %s\n",
		    fonts->family->fonts[4]->name);
	    fprintf(fp, "/%s findfont\n",
		    fonts->family->fonts[4]->name);
	    fprintf(fp, "dup length dict begin\n");
	    fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
	    fprintf(fp, "  currentdict\n");
	    fprintf(fp, "  end\n");
	    fprintf(fp, "/Font%d exch definefont pop\n",
		    (familynum - 1)*5 + 5);
	}

	familynum++;
	fonts = fonts->next;
    }
    while(cidfonts) {
	int i;
	char *name = cidfonts->cidfamily->cidfonts[0]->name;
	fprintf(fp, "%%%%IncludeResource: CID fake Bold font %s\n", name);
	fprintf(fp, "/%s-Bold\n/%s /CIDFont findresource\n", name, name);
	fprintf(fp, "%s", CIDBoldFontStr1);
	fprintf(fp, "%s", CIDBoldFontStr2);
	for (i = 0; i < 4 ; i++) {
	    char *fmt = NULL /* -Wall */;
	    fprintf(fp, "%%%%IncludeResource: CID font %s-%s\n", name,
		    cidfonts->cidfamily->cmap);
	    switch(i) {
	    case 0: fmt = "/%s-%s findfont\n";
		break;
	    case 1: fmt = "/%s-Bold-%s findfont\n";
		break;
	    case 2: fmt = "/%s-%s findfont [1 0 .3 1 0 0] makefont\n";
		break;
	    case 3: fmt = "/%s-Bold-%s findfont [1 0 .3 1 0 0] makefont\n";
		break;
	    default:
		break;
	    }
	    fprintf(fp, fmt, name, cidfonts->cidfamily->cmap);
	    fprintf(fp, "dup length dict begin\n");
	    fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
	    fprintf(fp, "  currentdict\n");
	    fprintf(fp, "  end\n");
	    fprintf(fp, "/Font%d exch definefont pop\n",
		    (familynum - 1)*5 + (cidfamilynum - 1)*5 + i + 1);
	}
	/*
	 * Symbol font
	 */
	fprintf(fp, "%%%%IncludeResource: font %s\n",
		cidfonts->cidfamily->symfont->name);
	fprintf(fp, "/%s findfont\n",
		cidfonts->cidfamily->symfont->name);
	fprintf(fp, "dup length dict begin\n");
	fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
	fprintf(fp, "  currentdict\n");
	fprintf(fp, "  end\n");
	fprintf(fp, "/Font%d exch definefont pop\n",
		(familynum - 1)*5 + (cidfamilynum - 1)*5 + 5);
	cidfamilynum++;
	cidfonts = cidfonts->next;
    }
}

/* The variables "paperwidth" and "paperheight" give the dimensions */
/* of the (unrotated) printer page in points whereas the graphics */
/* region box is for the rotated page. */

static void PSFileHeader(FILE *fp,
			 const char *papername, double paperwidth,
			 double paperheight, bool landscape,
			 int EPSFheader, bool paperspecial,
			 double left, double bottom, double right, double top,
			 const char *title,
			 PostScriptDesc *pd)
{
    int i;
    SEXP prolog;
    type1fontlist fonts = pd->fonts;
    int firstfont = 1;

    if(EPSFheader)
	fprintf(fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
    else
	fprintf(fp, "%%!PS-Adobe-3.0\n");
    /*
     * DocumentNeededResources names all fonts
     */
    while (fonts) {
	for (i=0; i<5; i++)
	    if (firstfont) {
		fprintf(fp, "%%%%DocumentNeededResources: font %s\n",
			fonts->family->fonts[0]->name);
		firstfont = 0;
	    } else
	fprintf(fp, "%%%%+ font %s\n", fonts->family->fonts[i]->name);
	fonts = fonts->next;
    }

    if(!EPSFheader)
	fprintf(fp, "%%%%DocumentMedia: %s %.0f %.0f 0 () ()\n",
		papername, paperwidth, paperheight);
    fprintf(fp, "%%%%Title: %s\n", title);
    fprintf(fp, "%%%%Creator: R Software\n");
    fprintf(fp, "%%%%Pages: (atend)\n");
    if (!EPSFheader && !paperspecial) { /* gs gets confused by this */
	if (landscape)
	    fprintf(fp, "%%%%Orientation: Landscape\n");
	else
	    fprintf(fp, "%%%%Orientation: Portrait\n");
    }
    fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
	    left, bottom, right, top);
    fprintf(fp, "%%%%EndComments\n");
    fprintf(fp, "%%%%BeginProlog\n");
    fprintf(fp,  "/bp  { gs");
    if (streql(pd->colormodel, "srgb")) fprintf(fp,  " sRGB");
    if (landscape)
	fprintf(fp, " %.2f 0 translate 90 rotate", paperwidth);
    fprintf(fp, " gs } def\n");
    prolog = findVar(install(".ps.prolog"), R_GlobalEnv);
    if(prolog == R_UnboundValue) {
	/* if no object is visible, look in the graphics namespace */
	SEXP graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices")));
	PROTECT(graphicsNS);
	prolog = findVar(install(".ps.prolog"), graphicsNS);
	/* under lazy loading this will be a promise on first use */
	if(TYPEOF(prolog) == PROMSXP) {
	    PROTECT(prolog);
	    prolog = eval(prolog, graphicsNS);
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
    if(!isString(prolog))
	error(_("object '.ps.prolog' is not a character vector"));
    fprintf(fp, "%% begin .ps.prolog\n");
    for (i = 0; i < length(prolog); i++)
	fprintf(fp, "%s\n", CHAR(STRING_ELT(prolog, i)));
    fprintf(fp, "%% end   .ps.prolog\n");
    if (streql(pd->colormodel, "srgb+gray") || streql(pd->colormodel, "srgb")) {
	SEXP graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices")));
	PROTECT(graphicsNS);
	prolog = findVar(install(".ps.prolog.srgb"), graphicsNS);
	/* under lazy loading this will be a promise on first use */
	if(TYPEOF(prolog) == PROMSXP) {
	    PROTECT(prolog);
	    prolog = eval(prolog, graphicsNS);
	    UNPROTECT(1);
	}
	UNPROTECT(1);
	for (i = 0; i < length(prolog); i++)
	    fprintf(fp, "%s\n", CHAR(STRING_ELT(prolog, i)));
    }
    if (streql(pd->colormodel, "srgb+gray"))
	fprintf(fp, "/srgb { sRGB setcolor } bind def\n");
    else if (streql(pd->colormodel, "srgb"))
	fprintf(fp, "/srgb { setcolor } bind def\n");
    PSEncodeFonts(fp, pd);

    fprintf(fp, "%%%%EndProlog\n");
}

static void PostScriptFileTrailer(FILE *fp, int pageno)
{
    fprintf(fp, "ep\n");
    fprintf(fp, "%%%%Trailer\n");
    fprintf(fp, "%%%%Pages: %d\n", pageno);
    fprintf(fp, "%%%%EOF\n");
}

static void PostScriptStartPage(FILE *fp, int pageno)
{
    fprintf(fp, "%%%%Page: %d %d\n", pageno, pageno);
    fprintf(fp, "bp\n");
}

static void PostScriptEndPage(FILE *fp)
{
    fprintf(fp, "ep\n");
}

static void PostScriptSetClipRect(FILE *fp, double x0, double x1,
				  double y0, double y1)
{
    fprintf(fp, "%.2f %.2f %.2f %.2f cl\n", x0, y0, x1, y1);
}

static void PostScriptSetLineWidth(FILE *fp, double linewidth)
{
    /* Must not allow line width to be zero */
    if (linewidth < .01)
        linewidth = .01;
    fprintf(fp, "%.2f setlinewidth\n", linewidth);
}

static void PostScriptSetLineEnd(FILE *fp, R_GE_lineend lend)
{
    int lineend = 1; /* -Wall */
    switch (lend) {
    case GE_ROUND_CAP:
	lineend = 1;
	break;
    case GE_BUTT_CAP:
	lineend = 0;
	break;
    case GE_SQUARE_CAP:
	lineend = 2;
	break;
    default:
	error(_("invalid line end"));
    }
    fprintf(fp, "%1d setlinecap\n", lineend);
}

static void PostScriptSetLineJoin(FILE *fp, R_GE_linejoin ljoin)
{
    int linejoin = 1; /* -Wall */
    switch (ljoin) {
    case GE_ROUND_JOIN:
	linejoin = 1;
	break;
    case GE_MITRE_JOIN:
	linejoin = 0;
	break;
    case GE_BEVEL_JOIN:
	linejoin = 2;
	break;
    default:
	error(_("invalid line join"));
    }
    fprintf(fp, "%1d setlinejoin\n", linejoin);
}

static void PostScriptSetLineMitre(FILE *fp, double linemitre)
{
    if (linemitre < 1)
	error(_("invalid line mitre"));
    fprintf(fp, "%.2f setmiterlimit\n", linemitre);
}

static void PostScriptSetFont(FILE *fp, int fontnum, double size)
{
    fprintf(fp, "/Font%d findfont %.0f s\n", fontnum, size);
}

static void
PostScriptSetLineTexture(FILE *fp, const char *dashlist, int nlty,
			 double lwd, int lend)
{
/* Historically the adjustment was 1 to allow for round end caps.
   As from 2.11.0, no adjustment is done for butt endcaps.
   The + 1 adjustment on the 'off' segments seems wrong, but it
   has been left in for back-compatibility
*/
    double dash[8], a = (lend == GE_BUTT_CAP) ? 0. : 1.;
    int i;
    bool allzero = true;
    for (i = 0; i < nlty; i++) {
	dash[i] = lwd *				
	    ((i % 2) ? (dashlist[i] + a)
	     : ((nlty == 1 && dashlist[i] == 1.) ? 1. : dashlist[i] - a) );
	if (dash[i] < 0) dash[i] = 0;
        if (dash[i] > .01) allzero = false;
    }
    fprintf(fp,"[");
    if (!allzero) {
        for (i = 0; i < nlty; i++) {
            fprintf(fp," %.2f", dash[i]);
        }
    }
    fprintf(fp,"] 0 setdash\n");
}


static void PostScriptMoveTo(FILE *fp, double x, double y)
{
    fprintf(fp, "%.2f %.2f m\n", x, y);
}

static void PostScriptRLineTo(FILE *fp, double x0, double y0,
			      double x1, double y1)
{
    double x = fround(x1, 2) - fround(x0, 2),
	y = fround(y1, 2) - fround(y0, 2);
    /* Warning: some machines seem to compute these differently from
       others, and we do want to diff the output.  x and y should be
       above around 0.01 or negligible (1e-14), and it is the latter case
       we are watching out for here.
    */

    if(fabs(x) < 0.005) fprintf(fp, "0"); else fprintf(fp, "%.2f", x);
    if(fabs(y) < 0.005) fprintf(fp, " 0"); else fprintf(fp, " %.2f", y);
    fprintf(fp, " l\n");
}

static void PostScriptStartPath(FILE *fp)
{
    fprintf(fp, "np\n");
}

static void PostScriptEndPath(FILE *fp)
{
    fprintf(fp, "o\n");
}

static void PostScriptRectangle(FILE *fp, double x0, double y0,
				double x1, double y1)
{
    fprintf(fp, "%.2f %.2f %.2f %.2f r ", x0, y0, x1-x0, y1-y0);
}

static void PostScriptCircle(FILE *fp, double x, double y, double r)
{
    fprintf(fp, "%.2f %.2f %.2f c ", x, y, r);
}

static void PostScriptWriteString(FILE *fp, const char *str, size_t nb)
{
    size_t i;

    fputc('(', fp);
    for (i = 0 ; i < nb && *str; i++, str++)
	switch(*str) {
	case '\n':
	    fprintf(fp, "\\n");
	    break;
	case '\\':
	    fprintf(fp, "\\\\");
	    break;
	case '-':
#ifdef USE_HYPHEN
	    if (!isdigit((int)str[1]))
		fputc(PS_hyphen, fp);
	    else
#endif
		fputc(*str, fp);
	    break;
	case '(':
	case ')':
	    fprintf(fp, "\\%c", *str);
	    break;
	default:
	    fputc(*str, fp);
	    break;
	}
    fputc(')', fp);
}


static FontMetricInfo *metricInfo(const char *, int, PostScriptDesc *);

static void PostScriptText(FILE *fp, double x, double y,
			   const char *str, size_t nb, double xc, double rot,
			   const pGEcontext gc,
			   pDevDesc dd)
{
    int face = gc->fontface;

    if(face < 1 || face > 5) face = 1;

    fprintf(fp, "%.2f %.2f ", x, y);

    PostScriptWriteString(fp, str, nb);

    if(xc == 0) fprintf(fp, " 0");
    else if(xc == 0.5) fprintf(fp, " .5");
    else if(xc == 1) fprintf(fp, " 1");
    else fprintf(fp, " %.2f", xc);

    if(rot == 0) fprintf(fp, " 0");
    else if(rot == 90) fprintf(fp, " 90");
    else fprintf(fp, " %.2f", rot);

    fprintf(fp, " t\n");
}

static void PostScriptText2(FILE *fp, double x, double y,
			    const char *str, size_t nb,
			    bool relative, double rot,
			    const pGEcontext gc,
			    pDevDesc dd)
{
    int face = gc->fontface;

    if(face < 1 || face > 5) face = 1;

    if(relative) {
	fprintf(fp, "\n%.3f ", x);
	PostScriptWriteString(fp, str, nb);
	fprintf(fp, " tb");
    } else {
	fprintf(fp, "%.2f %.2f ", x, y);
	PostScriptWriteString(fp, str, nb);
	if(rot == 0) fprintf(fp, " 0");
	else if(rot == 90) fprintf(fp, " 90");
	else fprintf(fp, " %.2f", rot);
	fprintf(fp, " ta");
    }
}

static void PostScriptHexText(FILE *fp, double x, double y,
			      const char *str, size_t strlen,
			      double xc, double rot)
{
    unsigned char *p = (unsigned char *)str;
    size_t i;

    fprintf(fp, "%.2f %.2f ", x, y);
    fprintf(fp, "<");
    for(i = 0; i < strlen; i++) fprintf(fp, "%02x", *p++);
    fprintf(fp, ">");

    if(xc == 0) fprintf(fp, " 0");
    else if(xc == 0.5) fprintf(fp, " .5");
    else if(xc == 1) fprintf(fp, " 1");
    else fprintf(fp, " %.2f", xc);

    if(rot == 0) fprintf(fp, " 0");
    else if(rot == 90) fprintf(fp, " 90");
    else fprintf(fp, " %.2f", rot);

    fprintf(fp, " t\n");
}

static void
PostScriptTextKern(FILE *fp, double x, double y,
		   const char *str, double xc, double rot,
		   const pGEcontext gc,
		   pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int face = gc->fontface;
    FontMetricInfo *metrics;
    size_t i, n, nout = 0;
    int j, w;
    unsigned char p1, p2;
    double fac = 0.001 * floor(gc->cex * gc->ps + 0.5);
    bool relative = false;
    bool haveKerning = false;

    if(face < 1 || face > 5) {
	warning(_("attempt to use invalid font %d replaced by font 1"), face);
	face = 1;
    }
    /* check if this is T1 -- should be, but be safe*/
    if(!isType1Font(gc->fontfamily, PostScriptFonts, pd->defaultFont)) {
	PostScriptText(fp, x, y, str, strlen(str), xc, rot, gc, dd);
	return;
    }
    metrics = metricInfo(gc->fontfamily, face, pd);

    n = strlen(str);
    if (n < 1) return;
    /* First check for any kerning */
    for(i = 0; i < n-1; i++) {
	p1 = str[i];
	p2 = str[i+1];
#ifdef USE_HYPHEN
	if (p1 == '-' && !isdigit((int)p2))
	    p1 = (unsigned char)PS_hyphen;
#endif
	for (j = metrics->KPstart[p1]; j < metrics->KPend[p1]; j++)
	    if(metrics->KernPairs[j].c2 == p2 &&
	       metrics->KernPairs[j].c1 == p1) {
		haveKerning = true;
		break;
	    }
    }

    if(haveKerning) {
	/* We have to start at the left edge, as we are going
	   to do this in pieces */
	if (xc != 0) {
	    double rot1 = rot * M_PI/180.;
	    int w = 0; short wx;
	    for(i = 0; i < n; i++) {
		unsigned char p1 = str[i];
		wx = metrics->CharInfo[(int)p1].WX;
		w += (wx == NA_SHORT) ? 0 : wx;
	    }
	    x -= xc*fac*cos(rot1)*w;
	    y -= xc*fac*sin(rot1)*w;
	}
	for(i = 0; i < n-1; i++) {
	    p1 = str[i];
	    p2 = str[i+1];
#ifdef USE_HYPHEN
	    if (p1 == '-' && !isdigit((int)p2))
		p1 = (unsigned char)PS_hyphen;
#endif
	    for (j = metrics->KPstart[p1]; j < metrics->KPend[p1]; j++)
		if(metrics->KernPairs[j].c2 == p2 &&
		   metrics->KernPairs[j].c1 == p1) {
		    PostScriptText2(fp, x, y, str+nout, i+1-nout,
				    relative, rot, gc, dd);
		    nout = i+1;
		    w = metrics->KernPairs[j].kern;
		    x = fac*w; y = 0;
		    relative = true;
		    break;
		}
	}
	PostScriptText2(fp, x, y, str+nout, n-nout, relative, rot, gc, dd);
	fprintf(fp, " gr\n");
    } else
	PostScriptText(fp, x, y, str, strlen(str), xc, rot, gc, dd);
}

/* Device Driver Actions */

static void PS_Circle(double x, double y, double r,
		      const pGEcontext gc,
		      pDevDesc dd);
static void PS_Clip(double x0, double x1, double y0, double y1,
		     pDevDesc dd);
static void PS_Close(pDevDesc dd);
static void PS_Line(double x1, double y1, double x2, double y2,
		    const pGEcontext gc,
		    pDevDesc dd);
static void PS_MetricInfo(int c,
			  const pGEcontext gc,
			  double* ascent, double* descent,
			  double* width, pDevDesc dd);
static void PS_NewPage(const pGEcontext gc,
		       pDevDesc dd);
static void PS_Open(pDevDesc, PostScriptDesc*);
static void PS_Polygon(int n, double *x, double *y,
		       const pGEcontext gc,
		       pDevDesc dd);
static void PS_Polyline(int n, double *x, double *y,
			const pGEcontext gc,
			pDevDesc dd);
static void PS_Rect(double x0, double y0, double x1, double y1,
		    const pGEcontext gc,
		    pDevDesc dd);
static void PS_Path(double *x, double *y,
                    int npoly, int *nper,
                    Rboolean winding, // Rboolean in GraphicsDevice.h
                    const pGEcontext gc,
                    pDevDesc dd);
static void PS_Raster(unsigned int *raster, int w, int h,
		      double x, double y, double width, double height,
		      double rot,
		      Rboolean interpolate, // Rboolean in GraphicsDevice.h
		      const pGEcontext gc, pDevDesc dd);
static void PS_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd);
static double PS_StrWidth(const char *str,
			  const pGEcontext gc,
			  pDevDesc dd);
static void PS_Text(double x, double y, const char *str,
		    double rot, double hadj,
		    const pGEcontext gc,
		    pDevDesc dd);
static double PS_StrWidthUTF8(const char *str,
			      const pGEcontext gc,
			      pDevDesc dd);
static void PS_TextUTF8(double x, double y, const char *str,
			double rot, double hadj,
			const pGEcontext gc,
			pDevDesc dd);
static SEXP     PS_setPattern(SEXP pattern, pDevDesc dd);
static void     PS_releasePattern(SEXP ref, pDevDesc dd);
static SEXP     PS_setClipPath(SEXP path, SEXP ref, pDevDesc dd);
static void     PS_releaseClipPath(SEXP ref, pDevDesc dd);
static SEXP     PS_setMask(SEXP path, SEXP ref, pDevDesc dd);
static void     PS_releaseMask(SEXP ref, pDevDesc dd);
static SEXP     PS_defineGroup(SEXP source, int op, SEXP destination, 
                               pDevDesc dd);
static void     PS_useGroup(SEXP ref, SEXP trans, pDevDesc dd);
static void     PS_releaseGroup(SEXP ref, pDevDesc dd);
static void     PS_stroke(SEXP path, const pGEcontext gc, pDevDesc dd);
static void     PS_fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd);
static void     PS_fillStroke(SEXP path, int rule, const pGEcontext gc, 
                              pDevDesc dd);
static SEXP     PS_capabilities(SEXP capabilities);

/* PostScript Support (formerly in PostScript.c) */

static void PostScriptSetCol(FILE *fp, double r, double g, double b,
			     PostScriptDesc *pd)
{
    const char *mm = pd->colormodel;
    if(r == g && g == b && 
       !(streql(mm, "cmyk") || streql(mm, "srgb") 
	 || streql(mm, "rgb-nogray")) ) { /* grey */
	if(r == 0) fprintf(fp, "0");
	else if (r == 1) fprintf(fp, "1");
	else fprintf(fp, "%.4f", r);
	fprintf(fp," setgray");
    } else {
	if(strcmp(mm, "gray") == 0) {
	    fprintf(fp, "%.4f setgray", 0.213*r + 0.715*g + 0.072*b);
	    // error(_("only gray colors are allowed in this color model"));
	} else if(strcmp(mm, "cmyk") == 0) {
	    double c = 1.0-r, m=1.0-g, y=1.0-b, k=c;
	    k = fmin2(k, m);
	    k = fmin2(k, y);
	    if(k == 1.0) c = m = y = 0.0;
	    else { c = (c-k)/(1-k); m = (m-k)/(1-k); y = (y-k)/(1-k); }
	    /* else {c /= (1.-k); m /= (1.-k); y /= (1.-k);} */
	    if(c == 0) fprintf(fp, "0");
	    else if (c == 1) fprintf(fp, "1");
	    else fprintf(fp, "%.4f", c);
	    if(m == 0) fprintf(fp, " 0");
	    else if (m == 1) fprintf(fp, " 1");
	    else fprintf(fp, " %.4f", m);
	    if(y == 0) fprintf(fp, " 0");
	    else if (y == 1) fprintf(fp, " 1");
	    else fprintf(fp, " %.4f", y);
	    if(k == 0) fprintf(fp, " 0");
	    else if (k == 1) fprintf(fp, " 1");
	    else fprintf(fp, " %.4f", k);
	    fprintf(fp," setcmykcolor\n");
	} else {
	    if(r == 0) fprintf(fp, "0");
	    else if (r == 1) fprintf(fp, "1");
	    else fprintf(fp, "%.4f", r);
	    if(g == 0) fprintf(fp, " 0");
	    else if (g == 1) fprintf(fp, " 1");
	    else fprintf(fp, " %.4f", g);
	    if(b == 0) fprintf(fp, " 0");
	    else if (b == 1) fprintf(fp, " 1");
	    else fprintf(fp, " %.4f", b);
	    if (streql(mm, "srgb+gray") || streql(mm, "srgb")) 
		fprintf(fp," srgb");
	    else fprintf(fp," rgb");
	}
    }
}

static void PostScriptSetFill(FILE *fp, double r, double g, double b,
			      PostScriptDesc *pd)
{
    fprintf(fp,"/bg { ");
    PostScriptSetCol(fp, r, g, b, pd);
    fprintf(fp, " } def\n");
}



/* Driver Support Routines */

static void SetColor(int, pDevDesc);
static void SetFill(int, pDevDesc);
static void SetFont(int, int, pDevDesc);
static void SetLineStyle(const pGEcontext, pDevDesc dd);
static void Invalidate(pDevDesc);

static void PS_cleanup(int stage, pDevDesc dd, PostScriptDesc *pd);


bool
PSDeviceDriver(pDevDesc dd, const char *file, const char *paper,
	       const char *family, const char **afmpaths, const char *encoding,
	       const char *bg, const char *fg, double width, double height,
	       bool horizontal, double ps,
	       bool onefile, bool pagecentre, bool printit,
	       const char *cmd, const char *title, SEXP fonts,
	       const char *colormodel, int useKern, bool fillOddEven)
{
    /* If we need to bail out with some sort of "error"
       then we must free(dd) */

    double xoff, yoff, pointsize;
    rcolor setbg, setfg;
    encodinginfo enc;
    encodinglist enclist;
    type1fontfamily font;
    cidfontfamily cidfont = NULL;
    int gotFont;

    PostScriptDesc *pd;

    /* Check and extract the device parameters */

    if(strlen(file) > R_PATH_MAX - 1) {
	free(dd);
	error(_("filename too long in %s()"), "postscript");
    }

    /* allocate new postscript device description */
    if (!(pd = (PostScriptDesc *) malloc(sizeof(PostScriptDesc)))) {
	free(dd);
	error(_("memory allocation problem in %s()"), "postscript");
    }

    /* from here on, if need to bail out with "error", must also */
    /* free(pd) */

    /* initialise postscript device description */
    strcpy(pd->filename, file);
    safestrcpy(pd->papername, paper, 64);
    strncpy(pd->title, title, 1023);
    pd->title[1023] = '\0';
    if (streql(colormodel, "grey")) strcpy(pd->colormodel, "grey");
    else { strncpy(pd->colormodel, colormodel, 29); pd->colormodel[29] = '\0';}
    pd->useKern = (useKern != 0);
    pd->fillOddEven = fillOddEven;

    if(strlen(encoding) > R_PATH_MAX - 1) {
	PS_cleanup(1, dd, pd);
	error(_("encoding path is too long in %s()"), "postscript");
    }
    /*
     * Load the default encoding AS THE FIRST ENCODING FOR THIS DEVICE.
     *
     * encpath MUST NOT BE "default"
     */
    pd->encodings = NULL;
    if (!(enc = findEncoding(encoding, pd->encodings, FALSE)))
	enc = addEncoding(encoding, 0);
    if (enc && (enclist = addDeviceEncoding(enc, pd->encodings))) {
	pd->encodings = enclist;
    } else {
	PS_cleanup(1, dd, pd);
	error(_("failed to load encoding file in %s()"), "postscript");
    }

    /*****************************
     * Load fonts
     *****************************/
    pd->fonts = NULL;
    pd->cidfonts = NULL;

    gotFont = 0;
    /*
     * If user specified afms then assume the font hasn't been loaded
     * Could lead to redundant extra loading of a font, but not often(?)
     */
    if (!strcmp(family, "User")) {
	font = addDefaultFontFromAFMs(encoding, afmpaths, 0, pd->encodings);
    } else {
	/*
	 * Otherwise, family is a device-independent font family.
	 * One of the elements of postscriptFonts().
	 * NOTE this is the first font loaded on this device!
	 */
	/*
	 * Check first whether this font has been loaded
	 * in this R session
	 */
	font = findLoadedFont(family, encoding, FALSE);
	cidfont = findLoadedCIDFont(family, FALSE);
	if (!(font || cidfont)) {
	    /*
	     * If the font has not been loaded yet, load it.
	     *
	     * The family SHOULD be in the font database to get this far.
	     * (checked at R level in postscript() in postscript.R)
	     */
	    if (isType1Font(family, PostScriptFonts, NULL)) {
		font = addFont(family, FALSE, pd->encodings);
	    } else if (isCIDFont(family, PostScriptFonts, NULL)) {
		cidfont = addCIDFont(family, FALSE);
	    } else {
		/*
		 * Should NOT get here.
		 * AND if we do, we should free
		 */
		PS_cleanup(3, dd, pd);
		error(_("invalid font type"));
	    }
	}
    }
    if (font || cidfont) {
	/*
	 * At this point the font is loaded, so add it to the
	 * device's list of fonts.
	 *
	 * If the user specified a vector of AFMs, it is a Type 1 font
	 */
	if (!strcmp(family, "User") ||
	    isType1Font(family, PostScriptFonts, NULL)) {
	    pd->fonts = addDeviceFont(font, pd->fonts, &gotFont);
	    pd->defaultFont = pd->fonts->family;
	    pd->defaultCIDFont = NULL;
	} else /* (isCIDFont(family, PostScriptFonts)) */ {
	    pd->cidfonts = addDeviceCIDFont(cidfont, pd->cidfonts, &gotFont);
	    pd->defaultFont = NULL;
	    pd->defaultCIDFont = pd->cidfonts->cidfamily;
	}
    }
    if (!gotFont) {
	PS_cleanup(3, dd, pd);
	error(_("failed to initialise default PostScript font"));
    }

    /*
     * Load the font names sent in via the fonts arg
     * NOTE that these are the font names specified at the
     * R-level, NOT the translated font names.
     */
    if (!isNull(fonts)) {
	int i, dontcare, gotFonts = 0, nfonts = LENGTH(fonts);
	type1fontlist fontlist;
	cidfontlist cidfontlist;
	for (i = 0; i < nfonts; i++) {
	    int index, cidindex;
	    const char *name = CHAR(STRING_ELT(fonts, i));
	    /*
	     * Check first whether this device is already
	     * using this font.
	     */
	    if (findDeviceFont(name, pd->fonts, &index) ||
		findDeviceCIDFont(name, pd->cidfonts, &cidindex))
		gotFonts++;
	    else {
		/*
		 * Check whether the font is loaded and, if not,
		 * load it.
		 */
		font = findLoadedFont(name, encoding, FALSE);
		cidfont = findLoadedCIDFont(name, FALSE);
		if (!(font || cidfont)) {
		    if (isType1Font(name, PostScriptFonts, NULL)) {
			font = addFont(name, FALSE, pd->encodings);
		    } else if (isCIDFont(name, PostScriptFonts, NULL)) {
			cidfont = addCIDFont(name, FALSE);
		    } else {
			/*
			 * Should NOT get here.
			 */
			PS_cleanup(4, dd, pd);
			error(_("invalid font type"));
		    }
		}
		/*
		 * Once the font is loaded, add it to the device's
		 * list of fonts.
		 */
		if (font || cidfont) {
		    if (isType1Font(name, PostScriptFonts, NULL)) {
			if ((fontlist = addDeviceFont(font, pd->fonts,
						      &dontcare))) {
			    pd->fonts = fontlist;
			    gotFonts++;
			}
		    } else /* (isCIDFont(family, PostScriptFonts)) */ {
			if ((cidfontlist = addDeviceCIDFont(cidfont,
							    pd->cidfonts,
							    &dontcare))) {
			    pd->cidfonts = cidfontlist;
			    gotFonts++;
			}
		    }
		}
	    }
	}
	if (gotFonts < nfonts) {
	    PS_cleanup(4, dd, pd);
	    error(_("failed to initialise additional PostScript fonts"));
	}
    }
    /*****************************
     * END Load fonts
     *****************************/

    setbg = R_GE_str2col(bg);
    setfg = R_GE_str2col(fg);

    pd->width = width;
    pd->height = height;
    pd->landscape = horizontal;
    pointsize = floor(ps);
    if(R_TRANSPARENT(setbg) && R_TRANSPARENT(setfg)) {
	PS_cleanup(4, dd, pd);
	error(_("invalid foreground/background color (postscript)"));
    }
    pd->printit = printit;
    if(strlen(cmd) > 2*R_PATH_MAX - 1) {
	PS_cleanup(4, dd, pd);
	error(_("'command' is too long"));
    }
    strcpy(pd->command, cmd);
    if (printit && strlen(cmd) == 0) {
	PS_cleanup(4, dd, pd);
	error(_("'postscript(print.it=TRUE)' used with an empty 'print' command"));
    }
    strcpy(pd->command, cmd);


    /* Deal with paper and plot size and orientation */

    pd->paperspecial = FALSE;
    if(!strcmp(pd->papername, "Default") ||
       !strcmp(pd->papername, "default")) {
	SEXP s = STRING_ELT(GetOption1(install("papersize")), 0);
	if(s != NA_STRING && strlen(CHAR(s)) > 0)
	    strcpy(pd->papername, CHAR(s));
	else strcpy(pd->papername, "a4");
    }
    if(!strcmp(pd->papername, "A4") ||
       !strcmp(pd->papername, "a4")) {
	pd->pagewidth  = 21.0 / 2.54;
	pd->pageheight = 29.7  /2.54;
    }
    else if(!strcmp(pd->papername, "Letter") ||
	    !strcmp(pd->papername, "letter") ||
	    !strcmp(pd->papername, "US") ||
	    !strcmp(pd->papername, "us")) {
	pd->pagewidth  =  8.5;
	pd->pageheight = 11.0;
    }
    else if(!strcmp(pd->papername, "Legal") ||
	    !strcmp(pd->papername, "legal")) {
	pd->pagewidth  =  8.5;
	pd->pageheight = 14.0;
    }
    else if(!strcmp(pd->papername, "Executive") ||
	    !strcmp(pd->papername, "executive")) {
	pd->pagewidth  =  7.25;
	pd->pageheight = 10.5;
    }
    else if(!strcmp(pd->papername, "special")) {
	if(pd->landscape) {
	    pd->pagewidth  = height;
	    pd->pageheight =  width;
	} else {
	    pd->pagewidth  =  width;
	    pd->pageheight = height;
	}
	pd->paperspecial = TRUE;
    }
    else {
	char errbuf[strlen(pd->papername) + 1];
	strcpy(errbuf, pd->papername);
	PS_cleanup(4, dd, pd);
	error(_("invalid page type '%s' (postscript)"), errbuf);
    }
    pd->pagecentre = pagecentre;
    pd->paperwidth = (int)(72 * pd->pagewidth);
    pd->paperheight = (int)(72 * pd->pageheight);
    pd->onefile = onefile;
    if(pd->landscape) {
	double tmp;
	tmp = pd->pagewidth;
	pd->pagewidth = pd->pageheight;
	pd->pageheight = tmp;
    }
    if(strcmp(pd->papername, "special"))
    {
	if(pd->width < 0.1 || pd->width > pd->pagewidth-0.5)
	    pd->width = pd->pagewidth-0.5;
	if(pd->height < 0.1 || pd->height > pd->pageheight-0.5)
	    pd->height = pd->pageheight-0.5;
    }
    if(pagecentre)
    {
	xoff = (pd->pagewidth - pd->width)/2.0;
	yoff = (pd->pageheight - pd->height)/2.0;
    } else {
	xoff = yoff = 0.0;
    }
    pd->maxpointsize = (int)(72.0 * ((pd->pageheight > pd->pagewidth) ?
				     pd->pageheight : pd->pagewidth));
    pd->pageno = pd->fileno = 0;
    pd->warn_trans = FALSE;

    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */
    /* Only right for 12 point font. */
    /* Max pointsize suggested by Peter Dalgaard */

    if(pointsize < 6.0) pointsize = 6.0;
    if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
    dd->startps = pointsize;
    dd->startfont = 1;
    dd->startlty = 0;
    dd->startfill = setbg;
    dd->startcol = setfg;
    dd->startgamma = 1;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->left = 72 * xoff;			/* left */
    dd->right = 72 * (xoff + pd->width);	/* right */
    dd->bottom = 72 * yoff;			/* bottom */
    dd->top = 72 * (yoff + pd->height);	        /* top */
    dd->clipLeft = dd->left; dd->clipRight = dd->right;
    dd->clipBottom = dd->bottom; dd->clipTop = dd->top;

    dd->cra[0] = 0.9 * pointsize;
    dd->cra[1] = 1.2 * pointsize;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->xCharOffset =  0.4900;
    dd->yCharOffset =  0.3333;
    dd->yLineBias = 0.2;

    /* Inches per Raster Unit */
    /* We use points (72 dots per inch) */

    dd->ipr[0] = 1.0/72.0;
    dd->ipr[1] = 1.0/72.0;
    /* GREset(.)  dd->gp.mkh = dd->gp.cra[0] * dd->gp.ipr[0]; */

    dd->canClip = TRUE;
    dd->canHAdj = 2;
    dd->canChangeGamma = FALSE;

    /*	Start the driver */
    PS_Open(dd, pd);

    dd->close      = PS_Close;
    dd->size     = PS_Size;
    dd->newPage    = PS_NewPage;
    dd->clip	      = PS_Clip;
    dd->text	      = PS_Text;
    dd->strWidth   = PS_StrWidth;
    dd->metricInfo = PS_MetricInfo;
    dd->rect	      = PS_Rect;
    dd->path     = PS_Path;
    dd->raster     = PS_Raster;
    dd->circle     = PS_Circle;
    dd->line	      = PS_Line;
    dd->polygon    = PS_Polygon;
    dd->polyline   = PS_Polyline;
    /* dd->locator    = PS_Locator;
       dd->mode	      = PS_Mode; */
    dd->hasTextUTF8   = TRUE;
    dd->textUTF8      = PS_TextUTF8;
    dd->strWidthUTF8  = PS_StrWidthUTF8;
    dd->useRotatedTextInContour = TRUE;
    dd->haveTransparency = 1;
    dd->haveTransparentBg = 2;
    dd->haveRaster = 3; /* non-missing colours */
    dd->setPattern      = PS_setPattern;
    dd->releasePattern  = PS_releasePattern;
    dd->setClipPath     = PS_setClipPath;
    dd->releaseClipPath = PS_releaseClipPath;
    dd->setMask         = PS_setMask;
    dd->releaseMask     = PS_releaseMask;
    dd->defineGroup     = PS_defineGroup;
    dd->useGroup        = PS_useGroup;
    dd->releaseGroup    = PS_releaseGroup;
    dd->stroke          = PS_stroke;
    dd->fill            = PS_fill;
    dd->fillStroke      = PS_fillStroke;
    dd->capabilities    = PS_capabilities;

    dd->deviceSpecific = (void *) pd;
    dd->displayListOn = FALSE;

    dd->deviceVersion = R_GE_group;
    return TRUE;
}

static void CheckAlpha(int color, PostScriptDesc *pd)
{
    unsigned int alpha = R_ALPHA(color);
    if (alpha > 0 && alpha < 255 && !pd->warn_trans) {
	warning(_("semi-transparency is not supported on this device: reported only once per page"));
	pd->warn_trans = TRUE;
    }
}

static void SetColor(int color, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(color != pd->current.col) {
	PostScriptSetCol(pd->psfp,
			 R_RED(color)/255.0,
			 R_GREEN(color)/255.0,
			 R_BLUE(color)/255.0, pd);
	fprintf(pd->psfp, "\n");
	pd->current.col = color;
    }
}

static void SetFill(int color, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(color != pd->current.fill) {
	PostScriptSetFill(pd->psfp,
			  R_RED(color)/255.0,
			  R_GREEN(color)/255.0,
			  R_BLUE(color)/255.0, pd);
	pd->current.fill = color;
    }
}

/* Note that the line texture is scaled by the line width. */

static void SetLineStyle(const pGEcontext gc, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    char dashlist[8];
    int i;
    int newlty = gc->lty;
    double newlwd = gc->lwd;
    R_GE_lineend newlend = gc->lend;
    R_GE_linejoin newljoin = gc->ljoin;
    double newlmitre = gc->lmitre;

    if (pd->current.lty != newlty || pd->current.lwd != newlwd) {
	pd->current.lwd = newlwd;
	pd->current.lty = newlty;
	PostScriptSetLineWidth(pd->psfp, newlwd * 0.75);
	/* process lty : */
	for(i = 0; i < 8 && newlty & 15 ; i++) {
	    dashlist[i] = newlty & 15;
	    newlty = newlty >> 4;
	}
	PostScriptSetLineTexture(pd->psfp, dashlist, i, newlwd * 0.75, newlend);
    }
    if (pd->current.lend != newlend) {
	pd->current.lend = newlend;
	PostScriptSetLineEnd(pd->psfp, newlend);
    }
    if (pd->current.ljoin != newljoin) {
	pd->current.ljoin = newljoin;
	PostScriptSetLineJoin(pd->psfp, newljoin);
    }
    if (pd->current.lmitre != newlmitre) {
	pd->current.lmitre = newlmitre;
	PostScriptSetLineMitre(pd->psfp, newlmitre);
    }
}

static void SetFont(int font, int size, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(size < 1 || size > pd->maxpointsize)
	size = 10;
    if (size != pd->current.fontsize || font != pd->current.font) {
	PostScriptSetFont(pd->psfp, font, size);
	pd->current.fontsize = size;
	pd->current.font = font;
    }
}

static void PS_cleanup(int stage, pDevDesc dd, PostScriptDesc *pd)
{
    switch (stage) {
    case 4: /* Allocated fonts */
    freeDeviceFontList(pd->fonts);
    freeDeviceCIDFontList(pd->cidfonts);
    case 3: /* Allocated encodings */
    freeDeviceEncList(pd->encodings);
    case 1: /* Allocated PDFDesc */
    free(pd);
    free(dd);
    }
}

// value was not used
static void PS_Open(pDevDesc dd, PostScriptDesc *pd)
{
    char buf[512];

    if (strlen(pd->filename) == 0) {
	if(strlen(pd->command) == 0)
	    pd->psfp = NULL;
	else {
	    errno = 0;
	    pd->psfp = R_popen(pd->command, "w");
	    pd->open_type = 1;
	}
	if (!pd->psfp || errno != 0) {
	    char errbuf[strlen(pd->command) + 1];
	    strcpy(errbuf, pd->command);
	    PS_cleanup(4, dd, pd);
	    error(_("cannot open 'postscript' pipe to '%s'"), errbuf);
	    return;
	}
    } else if (pd->filename[0] == '|') {
	errno = 0;
	pd->psfp = R_popen(pd->filename + 1, "w");
	pd->open_type = 1;
	if (!pd->psfp || errno != 0) {
	    char errbuf[strlen(pd->filename + 1) + 1];
	    strcpy(errbuf, pd->filename + 1);
	    PS_cleanup(4, dd, pd);
	    error(_("cannot open 'postscript' pipe to '%s'"),
		     errbuf);
	    return;
	}
    } else {
	snprintf(buf, 512, pd->filename, pd->fileno + 1); /* file 1 to start */
	pd->psfp = R_fopen(R_ExpandFileName(buf), "w");
	pd->open_type = 0;
    }
    if (!pd->psfp) {
	PS_cleanup(4, dd, pd);
	error(_("cannot open file '%s'"), buf);
	return;
    }

    if(pd->landscape)
	PSFileHeader(pd->psfp,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     !(pd->onefile),
		     pd->paperspecial,
		     dd->bottom,
		     dd->left,
		     dd->top,
		     dd->right,
		     pd->title,
		     pd);
    else
	PSFileHeader(pd->psfp,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     !(pd->onefile),
		     pd->paperspecial,
		     dd->left,
		     dd->bottom,
		     dd->right,
		     dd->top,
		     pd->title,
		     pd);

    return;
}

/* The driver keeps track of the current values of colors, fonts and
   line parameters, to save emitting some PostScript. In some cases,
   the state becomes unknown, notably after changing the clipping and
   at the start of a new page, so we have the following routine to
   invalidate the saved values, which in turn causes the parameters to
   be set before usage.

   Called at the start of each page and by PS_Clip (since that 
   does a grestore).
*/

static void Invalidate(pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    pd->current.font = -1;
    pd->current.fontsize = -1;
    pd->current.lwd = -1;
    pd->current.lty = -1;
    pd->current.lend = 0;
    pd->current.ljoin = 0;
    pd->current.lmitre = 0;
    pd->current.col = INVALID_COL;
    pd->current.fill = INVALID_COL;
}

static void PS_Clip(double x0, double x1, double y0, double y1, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptSetClipRect(pd->psfp, x0, x1, y0, y1);
    /* clipping does grestore so invalidate monitor variables */
    Invalidate(dd);
}

static void PS_Size(double *left, double *right,
		    double *bottom, double *top,
		    pDevDesc dd)
{
    *left = dd->left;
    *right = dd->right;
    *bottom = dd->bottom;
    *top = dd->top;
}

static void PostScriptClose(pDevDesc dd);

static void PS_NewPage(const pGEcontext gc,
		       pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;


    if(pd->onefile) {
	if(++pd->pageno > 1) PostScriptEndPage(pd->psfp);
    } else if(pd->pageno > 0) {
	PostScriptClose(dd);
	pd->fileno++;
	PS_Open(dd, pd);
	pd->pageno = 1;
    } else pd->pageno++;
    PostScriptStartPage(pd->psfp, pd->pageno);
    Invalidate(dd);
    CheckAlpha(gc->fill, pd);
    if(R_OPAQUE(gc->fill)) {
	/*
	 * Override some gc settings
	 */
	gc->col = R_TRANWHITE;
	PS_Rect(0, 0, 72.0 * pd->pagewidth, 72.0 * pd->pageheight, gc, dd);
    }
    pd->warn_trans = FALSE;
}

#ifdef Win32
#include "run.h" /* for runcmd */
#endif
static void PostScriptClose(pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptFileTrailer(pd->psfp, pd->pageno);
    if(pd->open_type == 1)
	pclose(pd->psfp);
    else {
	fclose(pd->psfp);
	if (pd->printit) {
	    char buff[3*R_PATH_MAX+ 10];
	    int err = 0;
	    /* This should not be possible: the command is limited
	       to 2*R_PATH_MAX */
	    if(strlen(pd->command) + strlen(pd->filename) > 3*R_PATH_MAX) {
		warning(_("error from postscript() in running:\n    %s"),
			pd->command);
		return;
	    }
	    strcpy(buff, pd->command);
	    strcat(buff, " ");
	    strcat(buff, pd->filename);
/*	    Rprintf("buff is %s\n", buff); */
#ifdef Unix
	    err = R_system(buff);
#endif
#ifdef Win32
	    err = Rf_runcmd(buff, CE_NATIVE, 0, 0, NULL, NULL, NULL);
#endif
	    if (err)
		warning(_("error from postscript() in running:\n    %s"),
			buff);
	}
    }
}

static void PS_Close(pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptClose(dd);
    freeDeviceCIDFontList(pd->cidfonts);
    freeDeviceFontList(pd->fonts);
    freeDeviceEncList(pd->encodings);
    pd->cidfonts = NULL;
    pd->fonts = NULL;
    pd->encodings = NULL;
    free(pd);
}

static FontMetricInfo
*CIDsymbolmetricInfo(const char *family, PostScriptDesc *pd)
{
    FontMetricInfo *result = NULL;
    int fontIndex;
    cidfontfamily fontfamily;

    fontfamily = findDeviceCIDFont(family, pd->cidfonts, &fontIndex);
    if (fontfamily) {
	/* (Type 1!) symbol font */
	result = &(fontfamily->symfont->metrics);
    } else
	error(_("CID family '%s' not included in postscript() device"),
	      family);
    return result;
}

static FontMetricInfo *metricInfo(const char *family, int face,
				  PostScriptDesc *pd) {
    FontMetricInfo *result = NULL;
    int fontIndex;
    type1fontfamily fontfamily = findDeviceFont(family, pd->fonts, &fontIndex);
    if (fontfamily) {
	if(face < 1 || face > 5) {
	    warning(_("attempt to use invalid font %d replaced by font 1"),
		    face);
	    face = 1;
	}
	result = &(fontfamily->fonts[face-1]->metrics);
    } else
	error(_("family '%s' not included in postscript() device"), family);
    return result;
}

static char *convname(const char *family, PostScriptDesc *pd) {
    char *result = NULL;
    int fontIndex;
    type1fontfamily fontfamily = findDeviceFont(family, pd->fonts, &fontIndex);
    if (fontfamily)
	result = fontfamily->encoding->convname;
    else
	error(_("family '%s' not included in postscript() device"), family);
    return result;
}

static double PS_StrWidth(const char *str,
			  const pGEcontext gc,
			  pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int face = gc->fontface;

    if(face < 1 || face > 5) face = 1;
    if (isType1Font(gc->fontfamily, PostScriptFonts, pd->defaultFont)) {
	return floor(gc->cex * gc->ps + 0.5) *
	    PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				  metricInfo(gc->fontfamily, face, pd),
				  pd->useKern, face,
				  convname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily, PostScriptFonts) */
	if (face < 5) {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				      NULL, FALSE, face, NULL);
	} else {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				      /* Send symbol face metric info */
				      CIDsymbolmetricInfo(gc->fontfamily, pd),
				      FALSE, face, NULL);
	}
    }
}

static double PS_StrWidthUTF8(const char *str,
			      const pGEcontext gc,
			      pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int face = gc->fontface;

    if(face < 1 || face > 5) face = 1;
    if (isType1Font(gc->fontfamily, PostScriptFonts, pd->defaultFont)) {
	return floor(gc->cex * gc->ps + 0.5) *
	    PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				  metricInfo(gc->fontfamily, face, pd),
				  pd->useKern, face,
				  convname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily, PostScriptFonts) */
	if (face < 5) {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				      NULL, FALSE, face, NULL);
	} else {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				      /* Send symbol face metric info */
				      CIDsymbolmetricInfo(gc->fontfamily, pd),
				      FALSE, face, NULL);
	}
    }
}

static void PS_MetricInfo(int c,
			  const pGEcontext gc,
			  double* ascent, double* descent,
			  double* width, pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int face = gc->fontface;

    if(face < 1 || face > 5) face = 1;

    if (isType1Font(gc->fontfamily, PostScriptFonts, pd->defaultFont)) {
	PostScriptMetricInfo(c, ascent, descent, width,
			     metricInfo(gc->fontfamily, face, pd),
			     true,
			     face == 5, convname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily, PostScriptFonts) */
	if (face < 5) {
	    PostScriptCIDMetricInfo(c, ascent, descent, width);
	} else {
	    PostScriptMetricInfo(c, ascent, descent, width,
				 CIDsymbolmetricInfo(gc->fontfamily, pd),
				 false, true, "");
	}
    }
    *ascent = floor(gc->cex * gc->ps + 0.5) * *ascent;
    *descent = floor(gc->cex * gc->ps + 0.5) * *descent;
    *width = floor(gc->cex * gc->ps + 0.5) * *width;
}

static void PS_Rect(double x0, double y0, double x1, double y1,
		    const pGEcontext gc,
		    pDevDesc dd)
{
    int code;
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */

    CheckAlpha(gc->fill, pd);
    CheckAlpha(gc->col, pd);
    code = 2 * (R_OPAQUE(gc->fill)) + (R_OPAQUE(gc->col));

    if (code) {
	if(code & 2)
	    SetFill(gc->fill, dd);
	if(code & 1) {
	    SetColor(gc->col, dd);
	    SetLineStyle(gc, dd);
	}
	PostScriptRectangle(pd->psfp, x0, y0, x1, y1);
	fprintf(pd->psfp, "p%d\n", code);
    }
}

typedef rcolor * rcolorPtr;

static void PS_imagedata(rcolorPtr raster,
			 int w, int h,
			 PostScriptDesc *pd)
{
    /* Each original byte is translated to two hex digits
       (representing a number between 0 and 255) */
    for (int i = 0; i < w*h; i++)
	fprintf(pd->psfp, "%02x%02x%02x",
		R_RED(raster[i]), R_GREEN(raster[i]), R_BLUE(raster[i]));
}

static void PS_grayimagedata(rcolorPtr raster,
			     int w, int h,
			     PostScriptDesc *pd)
{
    /* Weights as in PDF gray conversion */
    for (int i = 0; i < w*h; i++) {
	double r = 0.213 * R_RED(raster[i]) + 0.715 * R_GREEN(raster[i])
	    + 0.072 * R_BLUE(raster[i]);
	fprintf(pd->psfp, "%02x", (int)(r+0.49));
    }
}

/* Could support 'colormodel = "cmyk"' */
static void PS_writeRaster(unsigned int *raster, int w, int h,
			   double x, double y,
			   double width, double height,
			   double rot,
			   Rboolean interpolate,
			   pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    /* This takes the simple approach of creating an inline
     * image.
     * There is no support for semitransparent images, not even
     * for transparent pixels (missing values in image(useRaster = TRUE) ).
     *
     * The version in R < 2.13.2 used colorimage, hence the DeviceRGB
     * colour space.
     */

    /* Now we are using level-2 features, there are other things we could do
       (a) encode the data more compactly, e.g. using 
       /DataSource currentfile /ASCII85Decode filter /FlateDecode filter def

       (b) add a mask with ImageType 3: see PLRM 3rd ed section 4.10.6.

       (c) interpolation (done but disabled, as at least ghostscript
       seems to ignore the request, and Mac preview always
       interpolates.)

       (d) sRGB colorspace (done)
    */

    /* Save graphics state */
    fprintf(pd->psfp, "gsave\n");
    /* set the colour space: this form of the image operator uses the 
       current colour space. */
    if (streql(pd->colormodel, "srgb+gray")) 
	fprintf(pd->psfp, "sRGB\n");
    else if (streql(pd->colormodel, "srgb")) /* set for page */ ; 
    else if (streql(pd->colormodel, "gray"))
	fprintf(pd->psfp, "/DeviceGray setcolorspace\n");
    else
	fprintf(pd->psfp, "/DeviceRGB setcolorspace\n");
    /* translate */
    fprintf(pd->psfp, "%.2f %.2f translate\n", x, y);
    /* rotate */
    if (rot != 0.0) fprintf(pd->psfp, "%.2f rotate\n", rot);
    /* scale */
    fprintf(pd->psfp, "%.2f %.2f scale\n", width, height);
    /* write dictionary */
    fprintf(pd->psfp, "8 dict dup begin\n");
    fprintf(pd->psfp, "  /ImageType 1 def\n");
    fprintf(pd->psfp, "  /Width %d def\n", w);
    fprintf(pd->psfp, "  /Height %d def\n", h);
    fprintf(pd->psfp, "  /BitsPerComponent 8 def\n");
    if (interpolate)
	fprintf(pd->psfp, "  /Interpolate true def\n");
    if (streql(pd->colormodel, "gray"))
	fprintf(pd->psfp, "  /Decode [0 1] def\n");
    else
	fprintf(pd->psfp, "  /Decode [0 1 0 1 0 1] def\n");
    fprintf(pd->psfp, "  /DataSource currentfile /ASCIIHexDecode filter def\n");
    fprintf(pd->psfp, "  /ImageMatrix [%d 0 0 %d 0 %d] def\n", w, -h, h);
    fprintf(pd->psfp, "end\n");
    fprintf(pd->psfp, "image\n");
    /* now the data */
    if (streql(pd->colormodel, "gray"))
	PS_grayimagedata(raster, w, h, pd);
    else
	PS_imagedata(raster, w, h, pd);
    fprintf(pd->psfp, ">\n");
    /* Restore graphics state */
    fprintf(pd->psfp, "grestore\n");
}

/* see comments above */
#define OLD 1
static void PS_Raster(unsigned int *raster, int w, int h,
		      double x, double y,
		      double width, double height,
		      double rot,
		      Rboolean interpolate,
		      const pGEcontext gc, pDevDesc dd)
{
#ifdef OLD
    if (interpolate) {
	/* Generate a new raster
	 * which is interpolated from the original
	 * Assume a resolution for the new raster of 72 dpi
	 * Ideally would allow user to set this.
	 */
	const void *vmax;
	vmax = vmaxget();
	int newW = (int) width;
	int newH = (int) height;
	unsigned int *newRaster =
	    (unsigned int *) R_alloc(newW * newH, sizeof(unsigned int));

	R_GE_rasterInterpolate(raster, w, h,
			       newRaster, newW, newH);
	PS_writeRaster(newRaster, newW, newH,
		       x, y, width, height, rot, FALSE, dd);
	vmaxset(vmax);
    } else {
	PS_writeRaster(raster, w, h,
		       x, y, width, height, rot, FALSE, dd);
    }
#else
	PS_writeRaster(raster, w, h,
		       x, y, width, height, rot, interpolate, dd);
#endif
}

static void PS_Circle(double x, double y, double r,
		      const pGEcontext gc,
		      pDevDesc dd)
{
    int code;
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */

    CheckAlpha(gc->fill, pd);
    CheckAlpha(gc->col, pd);
    code = 2 * (R_OPAQUE(gc->fill)) + (R_OPAQUE(gc->col));

    if (code) {
	if(code & 2)
	    SetFill(gc->fill, dd);
	if(code & 1) {
	    SetColor(gc->col, dd);
	    SetLineStyle(gc, dd);
	}
	PostScriptCircle(pd->psfp, x, y, r);
	fprintf(pd->psfp, "p%d\n", code);
    }
}

static void PS_Line(double x1, double y1, double x2, double y2,
		    const pGEcontext gc,
		    pDevDesc dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    CheckAlpha(gc->col, pd);
    /* FIXME : clip to the device extents here */
    if(R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLineStyle(gc, dd);
	PostScriptStartPath(pd->psfp);
	PostScriptMoveTo(pd->psfp, x1, y1);
	PostScriptRLineTo(pd->psfp, x1, y1, x2, y2);
	/* fprintf(pd->psfp, "%.2f %.2f rl\n", x2 - x1, y2 - y1);*/
	PostScriptEndPath(pd->psfp);
    }
}

static void PS_Polygon(int n, double *x, double *y,
		       const pGEcontext gc,
		       pDevDesc dd)
{
    PostScriptDesc *pd;
    int i, code;

    pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */
    /* code == 6, eofill only */
    /* code == 7, outline and eofill */

    CheckAlpha(gc->fill, pd);
    CheckAlpha(gc->col, pd);
    code = 2 * (R_OPAQUE(gc->fill)) + (R_OPAQUE(gc->col));

    if (code) {
	if(code & 2) {
	    SetFill(gc->fill, dd);
	    if (pd->fillOddEven) code |= 4;
	}
	if(code & 1) {
	    SetColor(gc->col, dd);
	    SetLineStyle(gc, dd);
	}
	fprintf(pd->psfp, "np\n");
	fprintf(pd->psfp, " %.2f %.2f m\n", x[0], y[0]);
	for(i = 1 ; i < n ; i++)
	    if (i % 100 == 0)
		fprintf(pd->psfp, "%.2f %.2f lineto\n", x[i], y[i]);
	    else
		PostScriptRLineTo(pd->psfp, x[i-1], y[i-1], x[i], y[i]);
	fprintf(pd->psfp, "cp p%d\n", code);
    }
}

static void PS_Path(double *x, double *y,
                    int npoly, int *nper,
                    Rboolean winding,
                    const pGEcontext gc,
                    pDevDesc dd)
{
    PostScriptDesc *pd;
    int i, j, index, code;

    pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */
    /* code == 6, eofill only */
    /* code == 7, outline and eofill */

    CheckAlpha(gc->fill, pd);
    CheckAlpha(gc->col, pd);
    code = 2 * (R_OPAQUE(gc->fill)) + (R_OPAQUE(gc->col));

    if (code) {
	if(code & 2) {
	    SetFill(gc->fill, dd);
	    if (!winding) code |= 4;
	}
	if(code & 1) {
	    SetColor(gc->col, dd);
	    SetLineStyle(gc, dd);
	}
	fprintf(pd->psfp, "np\n");
        index = 0;
        for (i = 0; i < npoly; i++) {
            fprintf(pd->psfp, " %.2f %.2f m\n", x[index], y[index]);
            index++;
            for(j = 1; j < nper[i]; j++) {
                if (j % 100 == 0)
                    fprintf(pd->psfp, "%.2f %.2f lineto\n", 
                            x[index], y[index]);
                else
                    PostScriptRLineTo(pd->psfp, x[index-1], y[index-1], 
                                      x[index], y[index]);
                index++;
            }
            fprintf(pd->psfp, "cp\n");        
        }
	fprintf(pd->psfp, "p%d\n", code);
    }
}

static void PS_Polyline(int n, double *x, double *y,
			const pGEcontext gc,
			pDevDesc dd)
{
    PostScriptDesc *pd;
    int i;

    pd = (PostScriptDesc*) dd->deviceSpecific;
    CheckAlpha(gc->col, pd);
    if(R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLineStyle(gc, dd);
	fprintf(pd->psfp, "np\n");
	fprintf(pd->psfp, "%.2f %.2f m\n", x[0], y[0]);
	for(i = 1 ; i < n ; i++) {
	    /* split up solid lines (only) into chunks of size 1000 */
	    if(gc->lty == 0 && i%1000 == 0)
		fprintf(pd->psfp, "currentpoint o m\n");
	    if (i % 100 == 0)
		fprintf(pd->psfp, "%.2f %.2f lineto\n", x[i], y[i]);
	    else
		PostScriptRLineTo(pd->psfp, x[i-1], y[i-1], x[i], y[i]);
	}
	fprintf(pd->psfp, "o\n");
    }
}

static int translateFont(char *family, int style, PostScriptDesc *pd)
{
    int result = style;
    type1fontfamily fontfamily;
    int fontIndex;
    if(style < 1 || style > 5) {
	warning(_("attempt to use invalid font %d replaced by font 1"), style);
	style = 1;
    }
    fontfamily = findDeviceFont(family, pd->fonts, &fontIndex);
    if (fontfamily) {
	result = (fontIndex - 1)*5 + style;
    } else {
	warning(_("family '%s' not included in postscript() device"), family);
    }
    return result;
}

static int numFonts(type1fontlist fonts) {
    int i = 0;
    while (fonts) {
	i++;
	fonts = fonts->next;
    }
    return i;
}

static int translateCIDFont(char *family, int style, PostScriptDesc *pd)
{
    int result = style;
    cidfontfamily fontfamily;
    int fontIndex;
    if(style < 1 || style > 5) {
	warning(_("attempt to use invalid font %d replaced by font 1"), style);
	style = 1;
    }
    fontfamily = findDeviceCIDFont(family, pd->cidfonts, &fontIndex);
    if (fontfamily) {
	/*
	 * CID fonts all listed after all Type 1 fonts.
	 */
	result = (numFonts(pd->fonts)*5) + (fontIndex - 1)*5 + style;
    } else {
	warning(_("family '%s' not included in postscript() device"), family);
    }
    return result;
}

static void drawSimpleText(double x, double y, const char *str,
			   double rot, double hadj,
			   int font,
			   const pGEcontext gc,
			   pDevDesc dd) {
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    SetFont(font,
	    (int)floor(gc->cex * gc->ps + 0.5),dd);
    CheckAlpha(gc->col, pd);
    if(R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	if(pd->useKern)
	    PostScriptTextKern(pd->psfp, x, y, str, hadj, rot, gc, dd);
	else
	    PostScriptText(pd->psfp, x, y, str, strlen(str), hadj, rot, gc, dd);
    }
}

#ifdef Win32
static R_wchar_t utf8toutf32(const char *utf8char)
{
    void *cd = NULL;
    cd = Riconv_open("UTF-32LE", "UTF-8");
    if (cd == (void *)-1)
	return 0;

    R_wchar_t out;
    char *outbuf = (char *)&out;
    size_t status;
    size_t inbytesleft = utf8clen(*utf8char);
    size_t outbytesleft = 4;
    
    status = Riconv(cd, &utf8char, &inbytesleft, &outbuf, &outbytesleft);
    Riconv_close(cd);

    if (status == (size_t)-1)
	return 0;
    else
	return out;
}
#endif

/* <FIXME> it would make sense to cache 'cd' here, but we would also
   need to know if the current locale's charset changes.  However,
   currently this is only called in a UTF-8 locale.
 */
static void mbcsToSbcs(const char *in, char *out, const char *encoding,
		       int enc, int silent)
{
    void *cd = NULL;
    const char *i_buf; char *o_buf;
    size_t i_len, o_len, status;
    const char *fromenc = (enc == CE_UTF8) ? "UTF-8" : "";

/* Win32 - disable "OS"-level transliteration. */
#if 0
    if (utf8locale) {
	/* //nobestfit is not portable, only supported by R's customized
	   copy of win_iconv */
	size_t needed = strlen(encoding) + strlen("//nobestfit") + 1;
	R_CheckStack2(needed);
	char toenc[needed];
	snprintf(toenc, needed, "%s//nobestfit", encoding);
	cd = Riconv_open(toenc, fromenc);
    } else
	cd = Riconv_open(encoding, fromenc);
#else
    cd = Riconv_open(encoding, fromenc);
#endif
    if (cd == (void*)-1)
	error(_("unknown encoding '%s' in 'mbcsToSbcs'"), encoding);

    if (!silent) {
	const char *p = getenv("_R_SILENT_PDF_SUBSTITUTION_");
	if(p) silent = 1;
    }
    i_buf = (char *) in;
    i_len = strlen(in)+1; /* include terminator */
    o_buf = (char *) out;
    /* libiconv in macOS 14 can expand 1 UTF-8 char to at least 4 (o/oo) */
    o_len = 2*i_len;
next_char:
    status = Riconv(cd, &i_buf, &i_len, &o_buf, &o_len);
    /* GNU libiconv 1.13 gave EINVAL on \xe0 in UTF-8 (as used in fBasics) */
    if(status == (size_t) -1 && (errno == EILSEQ || errno == EINVAL)) {
	Riconv(cd, NULL, NULL, &o_buf, &o_len);
	const char *m = getenv("_R_CHECK_MBCS_CONVERSION_FAILURE_");
	int fail = (m != NULL);
	if (m && streql(m, "silent")) silent = 0;
	if (utf8locale) {
	    /* We attempt to do better here in a UTF-8 locale if the
	       input is valid and give transliteration or one dot per
	       UTF-8 character.  However, packages PBSmodelling and
	       fBasics used invalid 8-bit inputs.
	    */
	    int clen = utf8clen(*i_buf);
	    wchar_t wc;
	    int res =
		(int) utf8toucs(&wc, i_buf); // gives -1 for a conversion error
	    if (res != -1) {
		R_wchar_t ucs = wc;
		R_CheckStack2(clen + 1);
		char badchar[clen + 1];
		memcpy(badchar, i_buf, clen);
		badchar[clen] = '\0';
#ifdef Win32
		if (IS_HIGH_SURROGATE(wc)) {
		    ucs = utf8toutf32(i_buf);
		    if (!ucs)
			/* conversion failed (should not happen) */
			ucs = wc;
		}
#endif
		char *fix = NULL;
		// four-char fixups
		if (ucs == 0x2030) fix = "o/oo"; // permille, done by macOS
		else if (ucs == 0x33C2) fix = "a.m.";
		else if (ucs == 0x33D8) fix = "p.m.";
		
		// three-char fixups, done by macOS
		else if (ucs == 0xFB03) fix = "ffi";
		else if (ucs == 0xFB04) fix = "ffl";
		else if (ucs == 0x2194) fix = "<->";
		else if (ucs == 0x21D4) fix = "<=>";
		else if (ucs == 0x22D8) fix = "<<<";
		else if (ucs == 0x22D9) fix = ">>>";
		else if (ucs == 0x2026) fix = "...";
		else if (ucs == 0x22EF) { // done by macOS in latin1
		    /* In most 8-bit encodings B7 is the 'middle dot',
		       U+00D7,, but not all, e.g. KOI8-?.
		       Very rare, so don't worry about speed.
		    */
		    if (streql(encoding, "latin1") ||
			streql(encoding, "latin2") ||
			streql(encoding, "latin7") ||
			streql(encoding, "latin-9") ||
			streql(encoding, "iso-8859-7") ||
			streql(encoding, "cp1252") ||
			streql(encoding, "cp1253") ||
			streql(encoding, "cp1257") ||
			streql(encoding, "iso88592") ||
			streql(encoding, "iso88597") ||
			streql(encoding, "iso885913") ||
			streql(encoding, "iso885915"))
			fix = "\267\267\267";
		    else fix = "...";
		}
		// Possible future re-mapping
		// else if (ucs == 0x20AC) fix = "EUR";

		// two-char fixups
		// left and right arrows, 0x2190 0x2192 0xFFE9 0xFFEB done by macOS
		else if(ucs == 0x2190 || ucs == 0xFFE9) fix = "<-";
		else if(ucs == 0x2192 || ucs == 0x2794 || ucs == 0x279C ||
			ucs == 0x279D || ucs == 0x279E || ucs == 0x279F ||
			ucs == 0x27a1 || ucs == 0x27a2 || ucs == 0xFFEB)
		    fix = "->";
		// more done by macOS
		else if(ucs == 0x2260) fix = "/=";
		else if(ucs == 0x226A) fix = "<<";
		else if(ucs == 0x226B) fix = ">>";
		else if(ucs == 0x2025) fix = "..";
		else if(ucs == 0x203C) fix = "!!";
		// some common ligatures: AE and ae are latin1.
		// all done by macOS
		else if(ucs == 0xFB00) fix = "ff";
		else if(ucs == 0xFB01) fix = "fi";
		else if(ucs == 0xFB02) fix = "fl";
		else if(ucs == 0x0152) fix = "OE";
		else if(ucs == 0x0153) fix = "oe";
		else if(ucs == 0x2122) fix = "TM";
		// The next two could and probably should be done by plotmath.
		else if(ucs == 0x2264) fix = "<=";
		else if(ucs == 0x2265) fix = ">=";

		// one-char fixups
		else if (ucs == 0x2013 || ucs == 0x2014 || ucs == 0x2212)
		    fix = "-"; // dashes, minus, done by macOS
		// done by macOS, latter to acute accent U+00B4 in Latin-1 (but not Latin-7)
		else if (ucs == 0x2018 || ucs == 0x2019) fix = "'";
		else if (ucs == 0x201C || ucs == 0x201D) fix = "\""; // done by macOS
		else if (ucs == 0x2022) fix = "."; // 'bullet', changed to "o" by macOS
		else if (ucs == 0x2605 || ucs == 0x2737) fix = "*";
		if (!fix) {
		    if (fail)
			error("conversion failure on '%s' in 'mbcsToSbcs': for %s (U+%04X)", in, badchar, (unsigned int)ucs);
		    else if(!silent)
			warning("conversion failure on '%s' in 'mbcsToSbcs': for %s (U+%04X)", in, badchar, (unsigned int)ucs);
		    fix = ".";  // default fix is one dot per char
		} else if(!silent)
		    warning("for '%s' in 'mbcsToSbcs': %s substituted for %s (U+%04X)", in, fix, badchar, (unsigned int)ucs);
		size_t nfix = strlen(fix);
		for (int j = 0; j < nfix; j++) *o_buf++ = *fix++;
		o_len -= nfix; i_buf += clen; i_len -= clen;
	    } else { // invalid UTF-8 so one dot per byte
		if (fail)
		    error(_("conversion failure on '%s' in 'mbcsToSbcs': for <%02x>"), in, (unsigned char) *i_buf);
		else if(!silent)
		    warning(_("conversion failure on '%s' in 'mbcsToSbcs': dot substituted for <%02x>"), in, (unsigned char) *i_buf);
		*o_buf++ = '.'; o_len--; i_buf++; i_len--;
	    }
	} else { // non-UTF-8, one dot per byte
	    if (fail)
		error(_("conversion failure on '%s' in 'mbcsToSbcs': for <%02x>"), in, (unsigned char) *i_buf);
	    else if(!silent)
		warning(_("conversion failure on '%s' in 'mbcsToSbcs': dot substituted for <%02x>"), in, (unsigned char) *i_buf);
	    *o_buf++ = '.'; o_len--; i_buf++; i_len--;
	}
	if(i_len > 0) goto next_char;
    }

    Riconv_close(cd);
    if (status == (size_t)-1) {  /* internal error? */
	// 'in' might not be valid in the session encoding.
	bool valid = mbcsValid(in);
	error("conversion failure from %s to %s on '%s' in 'mbcsToSbcs'",
	      (enc == CE_UTF8) ? "UTF-8" : "native", encoding,
	      valid ? in : "invalid input");
    }
}

static void PS_Text0(double x, double y, const char *str, int enc,
		     double rot, double hadj,
		     const pGEcontext gc,
		     pDevDesc dd)
{
    const char *str1 = str;
    char *buff;

    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    if (gc->fontface == 5) {
	if (isCIDFont(gc->fontfamily, PostScriptFonts, pd->defaultCIDFont)) {
	    drawSimpleText(x, y, str, rot, hadj,
			   translateCIDFont(gc->fontfamily, gc->fontface, pd),
			   gc, dd);
	    return;
	} else {
	    drawSimpleText(x, y, str, rot, hadj,
			   translateFont(gc->fontfamily, gc->fontface, pd),
			   gc, dd);
	    return;
	}
    }

    /* No symbol fonts from now on */

    if (isCIDFont(gc->fontfamily, PostScriptFonts, pd->defaultCIDFont)) {
	/* NB, we could be in a SBCS here */
	size_t ucslen;
	int fontIndex;

	/*
	 * CID convert optimize PS encoding == locale encode case
	 */
	cidfontfamily cidfont = findDeviceCIDFont(gc->fontfamily,
						  pd->cidfonts,
						  &fontIndex);
	if(!cidfont)
	    error(_("family '%s' not included in postscript() device"),
		  gc->fontfamily);

	if (!dd->hasTextUTF8 &&
	    !strcmp(locale2charset(NULL), cidfont->encoding)) {
	    SetFont(translateCIDFont(gc->fontfamily, gc->fontface, pd),
		    (int)floor(gc->cex * gc->ps + 0.5),dd);
	    CheckAlpha(gc->col, pd);
	    if(R_OPAQUE(gc->col)) {
		SetColor(gc->col, dd);
		PostScriptHexText(pd->psfp, x, y, str, strlen(str), hadj, rot);
	    }
	    return;
	}

	/*
	 * CID convert PS encoding != locale encode case
	 */
	ucslen = (dd->hasTextUTF8) ? Rf_utf8towcs(NULL, str, 0) : mbstowcs(NULL, str, 0);
	if (ucslen != (size_t)-1) {
	    void *cd;
	    const char  *i_buf; char *o_buf;
	    size_t nb, i_len,  o_len, buflen = ucslen * sizeof(R_ucs2_t);
	    size_t status;

	    cd = (void*) Riconv_open(cidfont->encoding,
				     (enc == CE_UTF8) ? "UTF-8" : "");
	    if(cd == (void*)-1) {
		warning(_("failed open converter to encoding '%s'"),
			cidfont->encoding);
		return;
	    }

	    R_CheckStack2(buflen);
	    unsigned char buf[buflen];

	    i_buf = (char *)str;
	    o_buf = (char *)buf;
	    i_len = strlen(str); /* do not include terminator */
	    nb = o_len = buflen;

	    status = Riconv(cd, &i_buf, (size_t *)&i_len,
			    (char **)&o_buf, (size_t *)&o_len);

	    Riconv_close(cd);
	    if(status == (size_t)-1)
		warning(_("failed in text conversion to encoding '%s'"),
			cidfont->encoding);
	    else {
		SetFont(translateCIDFont(gc->fontfamily, gc->fontface, pd),
			(int)floor(gc->cex * gc->ps + 0.5), dd);
		CheckAlpha(gc->col, pd);
		if(R_OPAQUE(gc->col)) {
		    SetColor(gc->col, dd);
		    PostScriptHexText(pd->psfp, x, y, (char *)buf,
				      nb - o_len, hadj, rot);
		}
	    }
	    return;
	} else {
	    warning(_("invalid string in '%s'"), "PS_Text");
	    return;
	}
    }

    /* Now using single-byte non-symbol font.

       Was utf8locale, but it is not entirely obvious that only UTF-8
       needs re-encoding, although we don't have any other MBCSs that
       can sensibly be mapped to a SBCS.
       It would be perverse (but possible) to write English in a
       CJK MBCS.
    */
    if((enc == CE_UTF8 || mbcslocale) && !strIsASCII(str)) {
	R_CheckStack2(2*strlen(str)+1);
	/* Output string cannot be longer
	   -- but it can be in bytes if transliteration is used
	 */
	buff = alloca(2*strlen(str)+1);
	mbcsToSbcs(str, buff, convname(gc->fontfamily, pd), enc, 0);
	str1 = buff;
    }
    drawSimpleText(x, y, str1, rot, hadj,
		   translateFont(gc->fontfamily, gc->fontface, pd),
		   gc, dd);
}

static void PS_Text(double x, double y, const char *str,
		    double rot, double hadj,
		    const pGEcontext gc,
		    pDevDesc dd)
{
    PS_Text0(x, y, str, CE_NATIVE, rot, hadj, gc, dd);
}

static void PS_TextUTF8(double x, double y, const char *str,
			double rot, double hadj,
			const pGEcontext gc,
			pDevDesc dd)
{
    PS_Text0(x, y, str, CE_UTF8, rot, hadj, gc, dd);
}

static SEXP PS_setPattern(SEXP pattern, pDevDesc dd) {
    return R_NilValue;
}

static void PS_releasePattern(SEXP ref, pDevDesc dd) {} 

static SEXP PS_setClipPath(SEXP path, SEXP ref, pDevDesc dd) {
    return R_NilValue;
}

static void PS_releaseClipPath(SEXP ref, pDevDesc dd) {}

static SEXP PS_setMask(SEXP path, SEXP ref, pDevDesc dd) {
    return R_NilValue;
}

static void PS_releaseMask(SEXP ref, pDevDesc dd) {}

static SEXP PS_defineGroup(SEXP source, int op, SEXP destination, pDevDesc dd) {
    return R_NilValue;
}

static void PS_useGroup(SEXP ref, SEXP trans, pDevDesc dd) {}

static void PS_releaseGroup(SEXP ref, pDevDesc dd) {}

static void PS_stroke(SEXP path, const pGEcontext gc, pDevDesc dd) {}

static void PS_fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd) {}

static void PS_fillStroke(SEXP path, int rule, const pGEcontext gc, 
                          pDevDesc dd) {}

static SEXP PS_capabilities(SEXP capabilities) { return capabilities; }


/***********************************************************************

		 PDF driver also shares font handling

************************************************************************/

typedef struct {
    rcolorPtr raster;
    int w;
    int h;
    Rboolean interpolate;
    int nobj;     /* The object number when written out */
    int nmaskobj; /* The mask object number */
} rasterImage;

#define DEFBUFSIZE 8192

#define PDFlinearGradient 0
#define PDFstitchedFunction 1
#define PDFexpFunction 2
#define PDFshadingPattern 3
#define PDFsoftMask 4
#define PDFclipPath 5
#define PDFcontent 6
#define PDFtilingPattern 7
#define PDFgroup 8
#define PDFstrokePath 9
#define PDFfillPath 10
#define PDFfillStrokePath 11
#define PDFtemp 12
#define PDFshadingSoftMask 13
#define PDFglyphFont 14

/* PDF Blend Modes */
#define PDFnormal 0
/* NOTE that the *order* from here on matches the order of R_GE_composite<*>
 * in GraphicsEngine.h (so we can get one from the other via simple offset)
 */
#define PDFmultiply 1
#define PDFscreen 2
#define PDFoverlay 3
#define PDFdarken 4
#define PDFlighten 5
#define PDFcolorDodge 6
#define PDFcolorBurn 7
#define PDFhardLight 8
#define PDFsoftLight 9
#define PDFdifference 10
#define PDFexclusion 11

#define PDFnumBlendModes 12

const char* PDFblendModes[] = { "Normal",
                                "Multiply", "Screen", "Overlay", "Darken",
                                "Lighten", "ColorDodge", "ColorBurn",
                                "HardLight", "SoftLight", "Difference",
                                "Exclusion", "Hue", "Saturation", "Color",
                                "Luminosity" };

typedef struct {
    int type;
    int nchar;
    char* str;
    int contentDefn;
} PDFdefn;

typedef struct {
    char filename[R_PATH_MAX];
    int open_type;
    char cmd[R_PATH_MAX];

    char papername[64];	/* paper name */
    int paperwidth;	/* paper width in big points (1/72 in) */
    int paperheight;	/* paper height in big points */
    int pageno;		/* page number */
    int fileno;		/* file number */

    int maxpointsize;

    double width;	/* plot width in inches */
    double height;	/* plot height in inches */
    double pagewidth;	/* page width in inches */
    double pageheight;	/* page height in inches */
    bool pagecentre;  /* centre image on page? */
    bool onefile;	/* one file or one file per page? */

    FILE *pdffp;        /* output file */
    FILE *mainfp;
    FILE *pipefp;

    /* This group of variables track the current device status.
     * They should only be set by routines that emit PDF. */
    struct {
	double lwd;		 /* line width */
	int lty;		 /* line type */
	R_GE_lineend lend;
	R_GE_linejoin ljoin;
	double lmitre;
	int fontsize;	         /* font size in points */
	rcolor col;		 /* color */
	rcolor fill;	         /* fill color */
	rcolor bg;		 /* color */
	int srgb_fg, srgb_bg;    /* Are stroke and fill colorspaces set? */
        int patternfill;
        int mask;
    } current;

    /*
     * This is a record of the alpha transparency levels used during
     * drawing to the device.
     * Only allow 256 different alpha levels
     * (because R uses 8-bit alpha channel).
     * "alphas" is a record of alphas used so far (unused set to -1)
     * There are separate alpha levels for stroking and filling
     * (i.e., col and fill)
     */
    short colAlpha[256];
    short fillAlpha[256];
    bool usedAlpha;

    /*
     * What version of PDF are we trying to work with?
     * This is used (so far) for implementing transparency and CID fonts
     * Alphas are only used if version is at least 1.4
     */
    int versionMajor;
    int versionMinor;

    int nobjs;  /* number of objects */
    int *pos; /* object positions */
    int max_nobjs; /* current allocation size */
    int *pageobj; /* page object numbers */
    int pagemax;
    int startstream; /* position of start of current stream */
    bool inText;
    char title[1024];
    char colormodel[30];
    bool dingbats, useKern;
    bool fillOddEven; /* polygon fill mode */
    bool useCompression;
    bool timestamp;
    bool producer;
    char author[1024];
    char tmpname[R_PATH_MAX]; /* used before compression */

    /*
     * Fonts and encodings used on the device
     */
    type1fontlist fonts;
    cidfontlist   cidfonts;
    encodinglist  encodings;
    /*
     * These next two just record the default device font
     */
    type1fontfamily defaultFont;
    cidfontfamily   defaultCIDFont;
    /* Record if fonts are used */
    bool fontUsed[100];

    /* Raster images used on the device */
    rasterImage *rasters;
    int numRasters; /* number in use */
    int writtenRasters; /* number written out on this page */
    int fileRasters; /* number written out in this file */
    int maxRasters; /* size of array allocated */
    /* Soft masks for raster images */
    int *masks;
    int numMasks;

    PDFdefn *definitions;
    int numDefns;
    int maxDefns;
    int appendingPath; /* Are we defining a (clipping) path ? */
    bool pathContainsText; /* Does the path contain text ? */
    bool pathContainsDrawing; /* Does the path contain any drawing ? */
    int appendingMask; /* Are we defining a mask ? */
    int currentMask;
    int appendingPattern; /* Are we defining a (tiling) pattern ? */
    int blendModes[PDFnumBlendModes];
    int appendingGroup; /* Are we defining a transparency group ? */
    int numGlyphFonts;

    /* Is the device "offline" (does not write out to a file) */
    bool offline;
}
PDFDesc;

/* Called at the start of a page and when clipping is reset */
static void PDF_Invalidate(PDFDesc *pd)
{
    pd->current.fontsize = -1;
    pd->current.lwd = -1;
    pd->current.lty = -1;
    pd->current.lend = 0;
    pd->current.ljoin = 0;
    pd->current.lmitre = 0;
    /* page starts with black as the default fill and stroke colours */
    pd->current.col = INVALID_COL;
    pd->current.fill = INVALID_COL;
    pd->current.bg = INVALID_COL;
    pd->current.srgb_fg = pd->current.srgb_bg = 0;
    pd->current.patternfill = -1;
    pd->current.mask = -1;
}

/* Macro for driver actions to check for "offline" device and bail out */

#define PDF_checkOffline() if (pd->offline) return

/* Device Driver Actions */

static void PDF_Open(pDevDesc, PDFDesc*);
static void PDF_Circle(double x, double y, double r,
		       const pGEcontext gc,
		       pDevDesc dd);
static void PDF_Clip(double x0, double x1, double y0, double y1,
		     pDevDesc dd);
static void PDF_Close(pDevDesc dd);
static void PDF_Line(double x1, double y1, double x2, double y2,
		     const pGEcontext gc,
		     pDevDesc dd);
void PDF_MetricInfo(int c,
                    const pGEcontext gc,
                    double* ascent, double* descent,
                    double* width, pDevDesc dd);
static void PDF_NewPage(const pGEcontext gc, pDevDesc dd);
static void PDF_Polygon(int n, double *x, double *y,
			const pGEcontext gc,
			pDevDesc dd);
static void PDF_Polyline(int n, double *x, double *y,
			 const pGEcontext gc,
			 pDevDesc dd);
static void PDF_Rect(double x0, double y0, double x1, double y1,
		     const pGEcontext gc,
		     pDevDesc dd);
static void PDF_Path(double *x, double *y,
                     int npoly, int *nper,
                     Rboolean winding, // Rboolean in GraphicsDevice.h
                     const pGEcontext gc,
                     pDevDesc dd);
static void PDF_Raster(unsigned int *raster, int w, int h,
		       double x, double y, double width, double height,
		       double rot,
		       Rboolean interpolate, // Rboolean in GraphicsDevice.h
		       const pGEcontext gc, pDevDesc dd);
static void PDF_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd);
double PDF_StrWidth(const char *str,
                    const pGEcontext gc,
                    pDevDesc dd);
static void PDF_Text(double x, double y, const char *str,
		     double rot, double hadj,
		     const pGEcontext gc,
		     pDevDesc dd);
static double PDF_StrWidthUTF8(const char *str,
			       const pGEcontext gc,
			       pDevDesc dd);
static void PDF_TextUTF8(double x, double y, const char *str,
			 double rot, double hadj,
			 const pGEcontext gc,
			 pDevDesc dd);
static SEXP     PDF_setPattern(SEXP pattern, pDevDesc dd);
static void     PDF_releasePattern(SEXP ref, pDevDesc dd);
static SEXP     PDF_setClipPath(SEXP path, SEXP ref, pDevDesc dd);
static void     PDF_releaseClipPath(SEXP ref, pDevDesc dd);
static SEXP     PDF_setMask(SEXP path, SEXP ref, pDevDesc dd);
static void     PDF_releaseMask(SEXP ref, pDevDesc dd);
static SEXP     PDF_defineGroup(SEXP source, int op, SEXP destination, 
                                pDevDesc dd);
static void     PDF_useGroup(SEXP ref, SEXP trans, pDevDesc dd);
static void     PDF_releaseGroup(SEXP ref, pDevDesc dd);
static void     PDF_stroke(SEXP path, const pGEcontext gc, pDevDesc dd);
static void     PDF_fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd);
static void     PDF_fillStroke(SEXP path, int rule, 
                               const pGEcontext gc, pDevDesc dd);
static SEXP     PDF_capabilities(SEXP capabilities);
static void     PDF_glyph(int n, int *glyphs, double *x, double *y, 
                          SEXP font, double size, 
                          int colour, double rot, pDevDesc dd);

/***********************************************************************
 * Stuff for recording definitions
 */

static void initDefn(int i, int type, PDFDesc *pd) 
{
    pd->definitions[i].type = type;
    pd->definitions[i].str = malloc(DEFBUFSIZE*sizeof(char));
    if (pd->definitions[i].str) {
        pd->definitions[i].nchar = DEFBUFSIZE;
        pd->definitions[i].str[0] = '\0';
    } else {
        warning(_("Failed to allocate PDF definition string"));
        pd->definitions[i].nchar = 0;
        pd->definitions[i].str = NULL;
    }
    pd->definitions[i].contentDefn = -1;
}

static void addDefnContent(int i, int content, PDFDesc *pd)
{
    pd->definitions[i].contentDefn = content;
}

static void catDefn(char* buf, int i, PDFDesc *pd) 
{
    size_t len = strlen(pd->definitions[i].str);
    size_t buflen = strlen(buf); 
    /* Grow definition string if necessary) */
    if (len + buflen + 1 >= pd->definitions[i].nchar) {
	void *tmp;
        pd->definitions[i].nchar = pd->definitions[i].nchar + DEFBUFSIZE;
	tmp = realloc(pd->definitions[i].str, 
                      (pd->definitions[i].nchar)*sizeof(char));
	if (!tmp) error(_("failed to increase definition string (shut down PDF device)"));
	pd->definitions[i].str = tmp;
    }
    strncat(pd->definitions[i].str, buf, 
            /* Instead of 'buflen', ensure that cannot write more
             * characters from 'buf' than 'pd->definitions[i].str' 
             * can hold, including leaving room for terminating \0
             */
            pd->definitions[i].nchar - strlen(pd->definitions[i].str) - 1);
}

static void copyDefn(int fromDefn, int toDefn, PDFDesc *pd)
{
    catDefn(pd->definitions[fromDefn].str, toDefn, pd);
}

static void trimDefn(int i, PDFDesc *pd) 
{
    size_t len = strlen(pd->definitions[i].str);
    pd->definitions[i].str = realloc(pd->definitions[i].str, 
                                     (len + 1)*sizeof(char));
    pd->definitions[i].str[len] = '\0';
}

static void killDefn(int i, PDFDesc *pd) 
{
    if (pd->definitions[i].str != NULL) {
        free(pd->definitions[i].str);
    }
}

static void initDefinitions(PDFDesc *pd)
{
    int i;
    pd->definitions = malloc(pd->maxDefns*sizeof(PDFdefn));
    
    if (pd->definitions) {
        for (i = 0; i < pd->maxDefns; i++) {
            pd->definitions[i].str = NULL;
        }
    } /* else error thrown in PDFDeviceDriver */
}

static int growDefinitions(PDFDesc *pd)
{
    if (pd->numDefns == pd->maxDefns) {
	int newMax = 2*pd->maxDefns;
	void *tmp;
	/* Do it this way so previous pointer is retained if it fails */
	tmp = realloc(pd->definitions, newMax*sizeof(PDFdefn));
	if(!tmp) error(_("failed to increase 'maxDefns'"));
	pd->definitions = tmp;
	for (int i = pd->maxDefns; i < newMax; i++) {
	    pd->definitions[i].str = NULL;
	}
	pd->maxDefns = newMax;
    }
    return pd->numDefns++;
}

static void shrinkDefinitions(PDFDesc *pd)
{
    pd->numDefns--;
}

static void killDefinitions(PDFDesc *pd)
{
    int i;
    for (i = 0; i < pd->numDefns; i++)
        killDefn(i, pd);
    free(pd->definitions);
}

/* For when end file and start a new one on new page */
static void resetDefinitions(PDFDesc *pd)
{
    int i;
    for (i = 0; i < pd->numDefns; i++)
        killDefn(i, pd);
    pd->numDefns = 0;
    /* Leave pd->maxDefns as is */
}

/***********************************************************************
 * Stuff for gradients
 */

static void addRGBExpGradientFunction(SEXP gradient, int i, 
                                      double start, double end,
                                      int toDefn,
                                      PDFDesc *pd)
{
    char buf[300]; 
    rcolor col1 = 0, col2 = 0; // -Wall
    switch(R_GE_patternType(gradient)) {
    case R_GE_linearGradientPattern:
        col1 = R_GE_linearGradientColour(gradient, i);
        col2 = R_GE_linearGradientColour(gradient, i + 1);
        break;
    case R_GE_radialGradientPattern:
        col1 = R_GE_radialGradientColour(gradient, i);
        col2 = R_GE_radialGradientColour(gradient, i + 1);
        break;
    }
    snprintf(buf,
             300,
             "<<\n/FunctionType 2\n/Domain [%.4f %.4f]\n/C0 [%0.4f %0.4f %0.4f]\n/C1 [%0.4f %0.4f %0.4f]\n/N 1\n>>\n",
             start,
             end,
             R_RED(col1)/255.0,
             R_GREEN(col1)/255.0,
             R_BLUE(col1)/255.0,
             R_RED(col2)/255.0,
             R_GREEN(col2)/255.0,
             R_BLUE(col2)/255.0);
    catDefn(buf, toDefn, pd);
}

static void addAlphaExpGradientFunction(SEXP gradient, int i, 
                                        double start, double end,
                                        int toDefn,
                                        PDFDesc *pd)
{
    char buf[300]; 
    rcolor col1 = 0, col2 = 0; // -Wall
    switch(R_GE_patternType(gradient)) {
    case R_GE_linearGradientPattern:
        col1 = R_GE_linearGradientColour(gradient, i);
        col2 = R_GE_linearGradientColour(gradient, i + 1);
        break;
    case R_GE_radialGradientPattern:
        col1 = R_GE_radialGradientColour(gradient, i);
        col2 = R_GE_radialGradientColour(gradient, i + 1);
        break;
    }
    snprintf(buf,
             300,
             "<<\n/FunctionType 2\n/Domain [%.4f %.4f]\n/C0 [%0.4f]\n/C1 [%0.4f]\n/N 1\n>>\n",
             start,
             end,
             R_ALPHA(col1)/255.0,
             R_ALPHA(col2)/255.0);
    catDefn(buf, toDefn, pd);
}

static void addStitchedGradientFunction(SEXP gradient, int nStops, int toDefn, 
                                        bool alpha, PDFDesc *pd)
{
    int defNum = growDefinitions(pd);
    double firstStop = 0.0, lastStop = 0.0, stop = 0.0; // -Wall for gcc 9
    char buf[100];  
    initDefn(defNum, PDFstitchedFunction, pd);
    switch(R_GE_patternType(gradient)) {
    case R_GE_linearGradientPattern:
        firstStop = R_GE_linearGradientStop(gradient, 0);
        lastStop = R_GE_linearGradientStop(gradient, nStops - 1);
        break;
    case R_GE_radialGradientPattern:
        firstStop = R_GE_radialGradientStop(gradient, 0);
        lastStop = R_GE_radialGradientStop(gradient, nStops - 1);
        break;
    }
    snprintf(buf,
             100,
             "<<\n/FunctionType 3\n/Domain [%0.4f %0.4f]\n/Functions [\n",
             firstStop,
             lastStop);
    catDefn(buf, defNum, pd);
    for (int i = 0; i < (nStops - 1); i++) {
        if (alpha) {
            addAlphaExpGradientFunction(gradient, i, 0.0, 1.0, defNum, pd);
        } else {
            addRGBExpGradientFunction(gradient, i, 0.0, 1.0, defNum, pd);
        }
    }
    catDefn("]\n/Bounds [", defNum, pd);
    for (int i = 1; i < (nStops - 1); i++) {
        switch(R_GE_patternType(gradient)) {
        case R_GE_linearGradientPattern:
            stop = R_GE_linearGradientStop(gradient, i);
            break;
        case R_GE_radialGradientPattern:
            stop = R_GE_radialGradientStop(gradient, i);
            break;
        }
        snprintf(buf,
		 100,
                "%0.4f ",
                stop);
        catDefn(buf, defNum, pd);
    }
    catDefn("]\n/Encode [", defNum, pd);
    for (int i = 0; i < (nStops - 1); i++)
        catDefn("0 1 ", defNum, pd);
    catDefn("]\n>>\n", defNum, pd);
    /* Copy toDefn */
    copyDefn(defNum, toDefn, pd);
    /* Discard temporary definition */
    killDefn(defNum, pd);
    shrinkDefinitions(pd);
}

static void addGradientFunction(SEXP gradient, int toDefn, 
                                bool alpha, PDFDesc *pd)
{
    int nStops = 0; // -Wall
    switch(R_GE_patternType(gradient)) {
    case R_GE_linearGradientPattern:
        nStops = R_GE_linearGradientNumStops(gradient);
        break;
    case R_GE_radialGradientPattern:
        nStops = R_GE_radialGradientNumStops(gradient);
        break;
    }
    if (nStops > 2) {
        addStitchedGradientFunction(gradient, nStops, toDefn, alpha, pd);
    } else {
        double start = 0.0, end = 0.0; // -Wall
        switch(R_GE_patternType(gradient)) {
        case R_GE_linearGradientPattern:
            start = R_GE_linearGradientStop(gradient, 0);
            end = R_GE_linearGradientStop(gradient, 1);
            break;
        case R_GE_radialGradientPattern:
            start = R_GE_radialGradientStop(gradient, 0);
            end = R_GE_radialGradientStop(gradient, 1);
            break;
        }
        if (alpha) {
            addAlphaExpGradientFunction(gradient, 0, start, end, toDefn, pd);
        } else {
            addRGBExpGradientFunction(gradient, 0, start, end, toDefn, pd);
        }
    }
}

static void addLinearGradient(SEXP gradient, char* colormodel,
                              int toDefn, PDFDesc *pd)
{
    int defNum = growDefinitions(pd);
    char buf[200];
    char colorspace[12];
    if (streql(colormodel, "gray"))
        strcpy(colorspace, "/DeviceGray");
    else if (streql(colormodel, "srgb"))
        strcpy(colorspace, "5 0 R");
    else
        strcpy(colorspace, "/DeviceRGB");
    initDefn(defNum, PDFlinearGradient, pd);
    snprintf(buf,
             200,
             "<<\n/ShadingType 2\n/ColorSpace %s\n/Coords [%.4f %.4f %.4f %.4f]\n/Function\n",
             colorspace,
             R_GE_linearGradientX1(gradient),
             R_GE_linearGradientY1(gradient),
             R_GE_linearGradientX2(gradient),
             R_GE_linearGradientY2(gradient));
    catDefn(buf, defNum, pd);
    if (streql(colormodel, "gray")) {
        addGradientFunction(gradient, defNum, TRUE, pd);
    } else {
        addGradientFunction(gradient, defNum, FALSE, pd);
    }    
    char extend[6];
    switch(R_GE_linearGradientExtend(gradient)) {
    case R_GE_patternExtendPad:
        strcpy(extend, "true");
        break;
    case R_GE_patternExtendRepeat:
    case R_GE_patternExtendReflect:
        warning("Repeat or reflect pattern not supported on PDF device");
    case R_GE_patternExtendNone:
        strcpy(extend, "false");
    }
    snprintf(buf,
             200,
             "/Extend [%s %s]\n>>\n",
             extend,
             extend);
    catDefn(buf, defNum, pd);
    /* Copy toDefn */
    copyDefn(defNum, toDefn, pd);
    /* Discard temporary definition */
    killDefn(defNum, pd);
    shrinkDefinitions(pd);
}

static void addRadialGradient(SEXP gradient, char* colormodel,
                              int toDefn, PDFDesc *pd)
{
    int defNum = growDefinitions(pd);
    char buf[200];
    char colorspace[12];
    if (streql(colormodel, "gray"))
        strcpy(colorspace, "/DeviceGray");
    else if (streql(colormodel, "srgb"))
        strcpy(colorspace, "5 0 R");
    else
        strcpy(colorspace, "/DeviceRGB");
    initDefn(defNum, PDFlinearGradient, pd);
    snprintf(buf,
             200,
             "<<\n/ShadingType 3\n/ColorSpace %s\n/Coords [%.4f %.4f %.4f %.4f %.4f %.4f]\n/Function\n",
             colorspace,
             R_GE_radialGradientCX1(gradient),
             R_GE_radialGradientCY1(gradient),
             R_GE_radialGradientR1(gradient),
             R_GE_radialGradientCX2(gradient),
             R_GE_radialGradientCY2(gradient),
             R_GE_radialGradientR2(gradient));
    catDefn(buf, defNum, pd);
    if (streql(colormodel, "gray")) {
        addGradientFunction(gradient, defNum, TRUE, pd);
    } else {
        addGradientFunction(gradient, defNum, FALSE, pd);
    }    
    char extend[6];
    switch(R_GE_radialGradientExtend(gradient)) {
    case R_GE_patternExtendPad:
        strcpy(extend, "true");
        break;
    case R_GE_patternExtendRepeat:
    case R_GE_patternExtendReflect:
        warning("Repeat or reflect pattern not supported on PDF device");
    case R_GE_patternExtendNone:
        strcpy(extend, "false");
    }
    snprintf(buf,
             200,
             "/Extend [%s %s]\n>>\n",
             extend,
             extend);
    catDefn(buf, defNum, pd);
    /* Copy toDefn */
    copyDefn(defNum, toDefn, pd);
    /* Discard temporary definition */
    killDefn(defNum, pd);
    shrinkDefinitions(pd);
}

static int addShadingSoftMask(SEXP pattern, PDFDesc *pd)
{
    int defNum = growDefinitions(pd);
    initDefn(defNum, PDFshadingSoftMask, pd);
    int xobjDefn = growDefinitions(pd);
    initDefn(xobjDefn, PDFcontent, pd);
    addDefnContent(defNum, xobjDefn, pd);
    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n<<\n/Type /ExtGState\n/AIS false\n/SMask\n<<\n",
            defNum, pd);
    catDefn("/Type /Mask\n/S /Luminosity\n/G ",
            defNum, pd);
    /* Mask definition completed when definition written
     * to file (PDF_endfile)
     */

    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n",
            xobjDefn, pd);
    catDefn("<<\n/Type /XObject\n/Subtype /Form\n/FormType 1\n/Group\n<<\n",
            xobjDefn, pd);
    catDefn("/Type /Group\n/CS /DeviceGray\n/I true\n/S /Transparency\n",
            xobjDefn, pd);
    catDefn(">>\n/Resources\n<<\n",
            xobjDefn, pd);
    catDefn("/Shading\n<<\n/S0\n",
            xobjDefn, pd);
    switch(R_GE_patternType(pattern)) {
    case R_GE_linearGradientPattern: 
        addLinearGradient(pattern, "gray", xobjDefn, pd);
        break;
    case R_GE_radialGradientPattern: 
        addRadialGradient(pattern, "gray", xobjDefn, pd);
        break;
    default:
        warning("Shading type not yet supported");
        return -1;
    }
    catDefn(">>\n/ExtGState << /G0 << /CA 1 /ca 1 >> >>\n",
            xobjDefn, pd);
    char buf[30];
    snprintf(buf, 
             30,
             ">>\n/BBox [0 0 %d %d]\n",
             (int) (0.5 + pd->paperwidth), (int) (0.5 + pd->paperheight));
    catDefn(buf, xobjDefn, pd);
    /* Note the spaces before the >> just after the endstream;
     * ghostscript seems to need those to avoid error (!?) */
    catDefn("/Length 14\n>>\nstream\n/G0 gs /S0 sh\nendstream\nendobj\n",
            xobjDefn, pd);
    trimDefn(xobjDefn, pd);

    return defNum;
}

static void completeShadingSoftMask(int defNum, int defnOffset, PDFDesc *pd)
{
    /* Write out mask content object */
    int contentObj = pd->definitions[defNum].contentDefn + defnOffset + 1;
    char buf[100];
    snprintf(buf, 100,"%d 0 R\n>>\n>>\nendobj\n", contentObj);
    catDefn(buf, defNum, pd);
}

/*
 * Do we need to bother with semi-transparency?
 */
static int semiTransparent(int col)
{
    return !(R_OPAQUE(col) || R_TRANSPARENT(col));
}

static bool semiTransparentShading(SEXP pattern)
{
    int i, nStops = 0; // -Wall
    switch(R_GE_patternType(pattern)) {
    case R_GE_linearGradientPattern:
        nStops = R_GE_linearGradientNumStops(pattern);
        break;
    case R_GE_radialGradientPattern:
        nStops = R_GE_radialGradientNumStops(pattern);
        break;
    }
    rcolor col = 0; // -Wall
    bool anyOpaque = false;
    bool anyTransparent = false;
    for (i = 0; i < nStops; i++) {
        switch(R_GE_patternType(pattern)) {
        case R_GE_linearGradientPattern: 
            col = R_GE_linearGradientColour(pattern, i);
            break;
        case R_GE_radialGradientPattern: 
            col = R_GE_radialGradientColour(pattern, i);
            break;
        }
        if (semiTransparent(col)) 
            return TRUE;
        if (R_OPAQUE(col)) anyOpaque = true;
        if (R_TRANSPARENT(col)) anyTransparent = true;
        if (anyOpaque && anyTransparent)
            return true;
    }
    return false;
}

static SEXP addShading(SEXP pattern, PDFDesc *pd)
{
    SEXP ref = R_NilValue;
    int defNum = growDefinitions(pd);
    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    initDefn(defNum, PDFshadingPattern, pd);
    catDefn(" 0 obj\n<<\n/Type /Pattern\n/PatternType 2\n/Shading\n", 
            defNum, pd);
    switch(R_GE_patternType(pattern)) {
    case R_GE_linearGradientPattern: 
        addLinearGradient(pattern, pd->colormodel, defNum, pd);
        break;
    case R_GE_radialGradientPattern:
        addRadialGradient(pattern, pd->colormodel, defNum, pd);
        break;
    default:
        warning("Shading type not yet supported");
        return R_NilValue;
    }
    catDefn(">>\nendobj\n", defNum, pd);
    trimDefn(defNum, pd);
    if (defNum >= 0) {
        if (semiTransparentShading(pattern)) {
            int maskNum = addShadingSoftMask(pattern, pd);
            if (maskNum >= 0) {
                addDefnContent(defNum, maskNum, pd);
                PROTECT(ref = allocVector(INTSXP, 2));
                INTEGER(ref)[0] = defNum;
                INTEGER(ref)[1] = maskNum;
                UNPROTECT(1);
            }
        } else {
            PROTECT(ref = allocVector(INTSXP, 1));
            INTEGER(ref)[0] = defNum;
            UNPROTECT(1);
        }
    }
    return ref;
}

static void completeShading(int defNum, int defnOffset, PDFDesc *pd)
{
    /* If we started a soft mask (for semitransparent shading)
     * we need to finish it here */
    int maskNum = pd->definitions[defNum].contentDefn;
    if (maskNum >= 0) {    
        completeShadingSoftMask(maskNum, defnOffset, pd);
    }
}

/***********************************************************************
 * Stuff for tiling patterns
 */

static int newTiling(SEXP pattern, PDFDesc *pd)
{
    SEXP R_fcall;
    int mainPattern;
    char buf[100];
    int defNum = growDefinitions(pd);
    initDefn(defNum, PDFtilingPattern, pd);

    /* Use separate definition to store the pattern content
     * so we can determine length of the content
     */
    int contentDefn = growDefinitions(pd);
    /* Use PDFtemp instead of PDFcontent because this content is 
     * NOT written out as separate object */
    initDefn(contentDefn, PDFtemp, pd);
    addDefnContent(defNum, contentDefn, pd);
    /* Some initialisation that newpage does
     * (expected by other captured output)
     */
    catDefn("1 J 1 j q\n", contentDefn, pd);

    mainPattern = pd->appendingPattern;
    pd->appendingPattern = contentDefn;

    /* Invalidate current settings so pattern enforces its settings */
    PDF_Invalidate(pd);

    /* Evaluate the pattern function to generate the pattern */
    R_fcall = PROTECT(lang1(R_GE_tilingPatternFunction(pattern)));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);

    /* Invalidate current settings so normal drawing enforces its settings */
    PDF_Invalidate(pd);

    /* Some finalisation that endpage does
     * (to match the newpage initilisation)
     */
    catDefn("Q\n", contentDefn, pd);
    /* Cannot discard temporary definition because there may have been
     * other definitions created during its creation (so it may no
     * longer be the topmost definition)
     */
    trimDefn(contentDefn, pd);

    pd->appendingPattern = mainPattern;

    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n<<\n/Type /Pattern\n/PatternType 1\n/PaintType 1\n",
            defNum, pd);
    catDefn("/TilingType 1\n",
            defNum, pd);
    snprintf(buf, 
             100,
             "/BBox [%d %d %d %d]\n",
             (int) (0.5 + R_GE_tilingPatternX(pattern)), 
             (int) (0.5 + R_GE_tilingPatternY(pattern)), 
             (int) (0.5 + R_GE_tilingPatternX(pattern) + 
                    R_GE_tilingPatternWidth(pattern)), 
             (int) (0.5 + R_GE_tilingPatternY(pattern) + 
                    R_GE_tilingPatternHeight(pattern)));
    catDefn(buf, defNum, pd);
    snprintf(buf,
             100,
             "/XStep %d /YStep %d\n",
             (int) (0.5 + R_GE_tilingPatternWidth(pattern)),
             (int) (0.5 + R_GE_tilingPatternHeight(pattern)));
    catDefn(buf, defNum, pd);

    /* Tiling pattern will be completed at end of file with
     * call to completeTiling()
     */

    return defNum;
}

static int PDFwriteResourceDictionary(int objOffset, bool endpage, 
                                      int excludeDef, PDFDesc *pd);

static void completeTiling(int defNum, int resourceDictOffset, PDFDesc *pd)
{
    char buf[100];
    /* (strong) assumption here that tiling pattern content is 
     * very next definition
     */
    int contentDefn = pd->definitions[defNum].contentDefn;

    catDefn("/Resources\n",
            defNum, pd);

    /* Write the resource dictionary for the tiling pattern.
     * This is just a copy of the Resource Dictionary for the page,
     * WITHOUT the tiling pattern itself.
     * This will be AT LEAST as much as the tiling pattern needs,
     * but it MUST exclude itself to avoid infinite loop in PDF viewer.
     */
    /* Redirect PDFwriteResourceDictionary() output to pattern */
    pd->appendingPattern = defNum;
    PDFwriteResourceDictionary(resourceDictOffset, false, defNum, pd);

    /* Note the spaces before the >> just after the endstream;
     * ghostscript seems to need those to avoid error (!?) */
    snprintf(buf,
             100,
             "/Length %d\n",
             (int) strlen(pd->definitions[contentDefn].str));
    catDefn(buf, defNum, pd);
    catDefn(">>\nstream\n", defNum, pd);
    /* Copy pattern content */
    copyDefn(contentDefn, defNum, pd);
    catDefn("endstream\nendobj\n", defNum, pd);

    trimDefn(defNum, pd);
}

static SEXP addTiling(SEXP pattern, PDFDesc *pd)
{
    SEXP ref = R_NilValue;
    int defNum = newTiling(pattern, pd);
    
    if (defNum >= 0) {
        PROTECT(ref = allocVector(INTSXP, 1));
        INTEGER(ref)[0] = defNum;
        UNPROTECT(1);
    }

    return ref;
}

static void addToPattern(char* str, PDFDesc *pd)
{
    /* append to a tiling pattern content definition */
    catDefn(str, pd->appendingPattern, pd);
}

/***********************************************************************
 * Stuff for patterns
 */

static SEXP addPattern(SEXP pattern, PDFDesc *pd)
{
    SEXP ref = R_NilValue;
    switch(R_GE_patternType(pattern)) {
    case R_GE_linearGradientPattern: 
    case R_GE_radialGradientPattern: 
        ref = addShading(pattern, pd);
        break;
    case R_GE_tilingPattern:
        ref = addTiling(pattern, pd);
        break;
    }
    return ref;
}

static int countPatterns(PDFDesc *pd) 
{
    int i, count = 0;
    for (i = 0; i < pd->numDefns; i++) {
        if (pd->definitions[i].type == PDFshadingPattern ||
            pd->definitions[i].type == PDFtilingPattern) {
            count++;
        }
    }
    return count;
}

/***********************************************************************
 * Stuff for (clipping) paths
 */

static bool appendingPathWithText(PDFDesc *pd) {
    /* Are we are capturing a path AND 
     * there is already text in the path ? */
    if (pd->appendingPath >= 0 &&
        pd->pathContainsText) {
        warning(_("Drawing not appended to path (contains text)"));
        return true;
    } else {
        return false;
    }
}

static void addToPath(char* str, PDFDesc *pd)
{
    /* Just append to the "current" definition */
    catDefn(str, pd->appendingPath, pd);
}

static int newPath(SEXP path, int type, PDFDesc *pd)
{
    SEXP R_fcall;
    int defNum = growDefinitions(pd);
    initDefn(defNum, type, pd);
    if (type == PDFclipPath) {
        catDefn("Q q\n", defNum, pd);
    }

    /* Put device in "append mode" */
    pd->appendingPath = defNum;
    pd->pathContainsText = FALSE;
    pd->pathContainsDrawing = FALSE;

    /* Evaluate the path function to generate the clipping path */
    R_fcall = PROTECT(lang1(path));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);

    if (type == PDFclipPath) {
        switch (R_GE_clipPathFillRule(path)) {
        case R_GE_nonZeroWindingRule: 
            catDefn(" W n\n", defNum, pd); break;
        case R_GE_evenOddRule:
            catDefn(" W* n\n", defNum, pd); break;
        }
    }

    trimDefn(defNum, pd);
    /* Exit "append mode" */
    pd->appendingPath = -1;

    return defNum;
}

/***********************************************************************
 * Stuff for masks
 */

static void addToMask(char* str, PDFDesc *pd)
{
    /* append to the a mask content definition */
    catDefn(str, pd->appendingMask, pd);
}

static int newMask(SEXP mask, PDFDesc *pd)
{
    SEXP R_fcall;
    int mainMask;
    char buf[100];
    int defNum = growDefinitions(pd);
    initDefn(defNum, PDFsoftMask, pd);
    int xobjDefn = growDefinitions(pd);
    initDefn(xobjDefn, PDFcontent, pd);
    addDefnContent(defNum, xobjDefn, pd);

    /* Use temporary definition to store the mask content
     * so we can determine length of the content
     */
    int tempDefn = growDefinitions(pd);
    initDefn(tempDefn, PDFtemp, pd);
    /* Some initialisation that newpage does
     * (expected by other captured output)
     */
    catDefn("1 J 1 j q\n", tempDefn, pd);

    mainMask = pd->appendingMask;
    pd->appendingMask = tempDefn;

    /* Invalidate current settings so mask enforces its settings */
    PDF_Invalidate(pd);

    /* Evaluate the mask function to generate the mask */
    R_fcall = PROTECT(lang1(mask));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);

    /* Invalidate current settings so normal drawing enforces its settings */
    PDF_Invalidate(pd);

    /* Some finalisation that endpage does
     * (to match the newpage initilisation)
     */
    catDefn("Q\n", tempDefn, pd);
    /* Cannot discard temporary definition because there may have been
     * other definitions created during its creation (so it may no
     * longer be the topmost definition)
     */
    trimDefn(tempDefn, pd);

    pd->appendingMask = mainMask;

    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n<<\n/Type /ExtGState\n/AIS false\n/SMask\n<<\n",
            defNum, pd);
    switch(R_GE_maskType(mask)) {
    case R_GE_alphaMask:
        catDefn("/Type /Mask\n/S /Alpha\n/G", defNum, pd);
        break;
    case R_GE_luminanceMask:
        catDefn("/Type /Mask\n/S /Luminosity\n/G", defNum, pd);
        break;
    }
    /* Mask definition completed when definition written
     * to file (PDF_endfile)
     */
    
    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n<</Type /XObject\n/Subtype /Form\n/FormType 1\n/Group\n<<\n",
            xobjDefn, pd);
    char colorspace[12];
    if (streql(pd->colormodel, "gray"))
        strcpy(colorspace, "/DeviceGray");
    else if (streql(pd->colormodel, "srgb"))
        strcpy(colorspace, "5 0 R");
    else
        strcpy(colorspace, "/DeviceRGB");
    snprintf(buf,
             100,
             "/Type /Group\n/CS %s\n/I true\n/S /Transparency\n",
             colorspace);
    catDefn(buf, xobjDefn, pd);
    snprintf(buf, 
             100,
             ">>\n/BBox [0 0 %d %d]\n",
             (int) (0.5 + pd->paperwidth), (int) (0.5 + pd->paperheight));
    catDefn(buf, xobjDefn, pd);

    /* Note the spaces before the >> just after the endstream;
     * ghostscript seems to need those to avoid error (!?) */
    snprintf(buf,
             100,
             "/Length %d\n",
             (int) strlen(pd->definitions[tempDefn].str));
    catDefn(buf, xobjDefn, pd);
    catDefn(">>\nstream\n", xobjDefn, pd);
    /* Copy mask content */
    copyDefn(tempDefn, xobjDefn, pd);
    catDefn("endstream\n", xobjDefn, pd);
    catDefn("endobj\n", xobjDefn, pd);

    trimDefn(xobjDefn, pd);
    return defNum;
}

static void completeMask(int defNum, int defnOffset, PDFDesc *pd)
{
    /* Write out mask content object */
    int contentObj = pd->definitions[defNum].contentDefn + defnOffset + 1;
    char buf[100];
    snprintf(buf, 100," %d 0 R\n>>\n>>\nendobj\n", contentObj);
    catDefn(buf, defNum, pd);
}

static SEXP addMask(SEXP mask, SEXP ref, PDFDesc *pd) 
{
    SEXP newref = R_NilValue;
    int index;

    if (isNull(mask)) {
        /* Set NO mask */
        index = -1;
    } else {
        if (isNull(ref)) {
            /* Generate new mask */
            index = newMask(mask, pd);
            if (index >= 0) {
                PROTECT(newref = allocVector(INTSXP, 1));
                INTEGER(newref)[0] = index;
                UNPROTECT(1);
            }
        } else {
            /* Reuse existing mask */
            index = INTEGER(ref)[0];
            newref = ref;
        }
    }
    pd->currentMask = index;

    return newref;
}

/***********************************************************************
 * Stuff for compositing groups
 */

static void initBlendModes(PDFDesc *pd)
{
    int i;
    for (i=0; i<PDFnumBlendModes; i++) {
        pd->blendModes[i] = 0;
    }
}

static void blendModeFromCompositingOperator(int op, char* mode, int size,
                                             PDFDesc *pd)
{
    int blendMode;
    switch(op) {
    case R_GE_compositeClear: 
    case R_GE_compositeSource: 
    case R_GE_compositeIn: 
    case R_GE_compositeOut: 
    case R_GE_compositeAtop: 
    case R_GE_compositeDest: 
    case R_GE_compositeDestOver: 
    case R_GE_compositeDestIn: 
    case R_GE_compositeDestOut: 
    case R_GE_compositeDestAtop: 
    case R_GE_compositeXor: 
    case R_GE_compositeAdd:
    case R_GE_compositeSaturate:
        warning(_("Compositing operator has no corresponding blend mode; defaulting to Normal"));
        blendMode = PDFnormal;
        break;
    case R_GE_compositeOver:
        blendMode = PDFnormal;
        break;
    default:
        blendMode = op - 14;
    }
    /* Record that blend mode has been used */
    pd->blendModes[blendMode] = 1;
    /* Enforce graphics state defined elsewhere via ExtGState */
    snprintf(mode, size, "/bm%d gs\n", blendMode);
}

static void addToGroup(char* str, PDFDesc *pd)
{
    /* append to a composite content definition */
    catDefn(str, pd->appendingGroup, pd);
}

static int newGroup(SEXP source, int op, SEXP destination, PDFDesc *pd)
{
    SEXP R_fcall;
    int mainGroup;
    char buf[100];
    int defNum = growDefinitions(pd);
    initDefn(defNum, PDFgroup, pd);
    
    /* Use temporary definition to store the mask content
     * so we can determine length of the content
     */
    int tempDefn = growDefinitions(pd);
    initDefn(tempDefn, PDFtemp, pd);
    /* Some initialisation that newpage does
     * (expected by other captured output)
     */
    catDefn("1 J 1 j q\n", tempDefn, pd);
    /* Ensure that current graphical parameter settings are recorded
     * with the group definition.
     */
    PDF_Invalidate(pd);

    mainGroup = pd->appendingGroup;
    pd->appendingGroup = tempDefn;

    if (destination != R_NilValue) {
        /* Evaluate the destination function to generate the destination */
        R_fcall = PROTECT(lang1(destination));
        eval(R_fcall, R_GlobalEnv);
        UNPROTECT(1);
    }

    /* Set the blend mode */
    blendModeFromCompositingOperator(op, buf, 100, pd);
    catDefn(buf, tempDefn, pd);

    /* Evaluate the source function to generate the source */
    R_fcall = PROTECT(lang1(source));
    eval(R_fcall, R_GlobalEnv);
    UNPROTECT(1);

    /* Some finalisation that endpage does
     * (to match the newpage initilisation)
     */
    catDefn("Q\n", tempDefn, pd);
    /* Cannot discard temporary definition because there may have been
     * other definitions created during its creation (so it may no
     * longer be the topmost definition)
     */
    trimDefn(tempDefn, pd);

    pd->appendingGroup = mainGroup;

    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n<<\n",
            defNum, pd);
    catDefn("/Type /XObject\n/Subtype /Form\n/FormType 1\n/Group\n<<\n",
            defNum, pd);
    char colorspace[12];
    if (streql(pd->colormodel, "gray"))
        strcpy(colorspace, "/DeviceGray");
    else if (streql(pd->colormodel, "srgb"))
        strcpy(colorspace, "5 0 R");
    else
        strcpy(colorspace, "/DeviceRGB");
    snprintf(buf,
             100,
             "/Type /Group\n/CS %s\n/I true\n/S /Transparency\n",
             colorspace);
    catDefn(buf, defNum, pd);
    snprintf(buf, 
             100,
             ">>\n/BBox [0 0 %d %d]\n",
             (int) (0.5 + pd->paperwidth), (int) (0.5 + pd->paperheight));
    catDefn(buf, defNum, pd);

    /* Note the spaces before the >> just after the endstream;
     * ghostscript seems to need those to avoid error (!?) */
    snprintf(buf,
             100,
             "/Length %d\n",
             (int) strlen(pd->definitions[tempDefn].str));
    catDefn(buf, defNum, pd);
    catDefn(">>\nstream\n", defNum, pd);
    /* Copy composite content */
    copyDefn(tempDefn, defNum, pd);
    catDefn("endstream\nendobj\n", defNum, pd);

    trimDefn(defNum, pd);
    return defNum;
}

/***********************************************************************
 * Stuff for glyphs
 */

static int newGlyphFont(const char *fontname, PDFDesc *pd)
{
    /* Must be able to handle a glyph info font file name */
    char buf[600];
    int defNum = growDefinitions(pd);
    initDefn(defNum, PDFglyphFont, pd);

    pd->numGlyphFonts += 1;

    /* Object number will be determined when definition written
     * to file (PDF_endfile)
     */
    catDefn(" 0 obj\n", defNum, pd);
    catDefn("<<\n/Type /Font\n/Subtype /Type0\n", defNum, pd);
    snprintf(buf,
             100,
             "/Name /glyph-font-%i\n",
             pd->numGlyphFonts);
    catDefn(buf, defNum, pd);
    snprintf(buf,
             100,
             "/BaseFont /%s\n",
             fontname);
    catDefn(buf, defNum, pd);
    catDefn("/Encoding /Identity-H\n/DescendantFonts [\n<<\n/Type /Font\n/Subtype /CIDFontType2\n", 
            defNum, pd);
    /* BaseFont again */
    catDefn(buf, defNum, pd);
    catDefn("/CIDSystemInfo\n<<\n/Registry (Adobe)\n/Ordering (Identity)\n/Supplement 0\n>>\n/FontDescriptor\n<<\n/Type /FontDescriptor\n", 
            defNum, pd);
    snprintf(buf,
             100,
             "/FontName /%s\n",
             fontname);
    catDefn(buf, defNum, pd);
    catDefn("/Flags 6\n/FontBBox [-1000 -1000 1000 1000]\n/ItalicAngle 0\n/Ascent 1000\n/Descent -1000\n/CapHeight 1000\n/StemV 100\n>>\n/CIDToGIDMap /Identity\n>>\n]\n>>\nendobj\n",
            defNum, pd);    
    trimDefn(defNum, pd);

    return defNum;
}

/***********************************************************************
 * Stuff for writing out PDF code
 */

/* Write output to a variety of destinations 
 * (buf must be preallocated)
 *
 * Check for (clip) path first 
 * (because paths cannot be nested and 
 *  because patterns and masks cannot be used in paths)
 *
 * Check for mask or pattern or group next 
 * (and capture all output to highest of those in that case)
 *
 * Otherwise, write directly to the PDF file
 */
static int PDFwrite(char *buf, size_t size, const char *fmt, PDFDesc *pd, ...)
{
    int val;
    va_list ap;
    
    va_start(ap, pd);
    val = vsnprintf(buf, size, fmt, ap);
    va_end(ap);

    if (pd->appendingPath >= 0) {
        addToPath(buf, pd);
    } else if (pd->appendingPattern >= 0 && 
               (pd->appendingPattern > pd->appendingMask) &&
               (pd->appendingPattern > pd->appendingGroup)) {
        addToPattern(buf, pd);
    } else if (pd->appendingMask >= 0 &&
               (pd->appendingMask > pd->appendingPattern) &&
               (pd->appendingMask > pd->appendingGroup)) {
        addToMask(buf, pd);
    } else if (pd->appendingGroup >= 0) {
        addToGroup(buf, pd);
    } else {
        fputs(buf, pd->pdffp);
    }
    
    return val;
}

static void PDFwritePatternDefs(int objoffset, int excludeDef, PDFDesc *pd)
{
    int i;
    char buf[100];
    PDFwrite(buf, 100, "/Pattern\n<<\n", pd);
    for (i = 0; i < pd->numDefns; i++) {
        if ((pd->definitions[i].type == PDFshadingPattern ||
             pd->definitions[i].type == PDFtilingPattern) &&
            /* Only write patterns with higher def number 
             * (defined AFTER this pattern)
             * to avoid infinite loop from later pattern referring to
             * earlier pattern (when earlier pattern being filled with
             * later pattern) */
            i > excludeDef) {
            PDFwrite(buf, 100, "/Def%d %d 0 R\n", pd,
                     i, i + objoffset);
        }
    }
    PDFwrite(buf, 100, ">>\n", pd);
}

static void PDFwriteGlyphFontDefs(int objOffset, PDFDesc *pd) {
    int i;
    char buf[100];
    int glyphFontCount = 0;
    for (i = 0; i < pd->numDefns; i++) {
        if (pd->definitions[i].type == PDFglyphFont) {
            PDFwrite(buf, 100, "/glyph-font-%d %d 0 R ", pd,
                     ++glyphFontCount, i + objOffset);
        }
    }
}

static void PDFwriteSoftMaskDefs(int objoffset, PDFDesc *pd)
{
    int i;
    char buf[100];
    for (i = 0; i < pd->numDefns; i++) {
        if (pd->definitions[i].type == PDFsoftMask ||
            pd->definitions[i].type == PDFshadingSoftMask) {
            PDFwrite(buf, 100, "/Def%d %d 0 R\n", pd,
                     i, i + objoffset);
        }
    }
}

static void PDFwriteGroupDefs(int objoffset, PDFDesc *pd)
{
    int i;
    char buf[100];
    for (i = 0; i < pd->numDefns; i++) {
        if (pd->definitions[i].type == PDFgroup) {
            PDFwrite(buf, 100, "/Def%d %d 0 R\n", pd,
                     i, i + objoffset);
        }
    }
}

static void PDFwriteClipPath(int i, PDFDesc *pd)
{
    char* buf;
    size_t len = strlen(pd->definitions[i].str);
    buf = malloc((len + 1)*sizeof(char));

    if (buf) {
        PDFwrite(buf, len + 1, "%s", pd, pd->definitions[i].str);
        free(buf);
    } else {
        warning(_("Failed to write PDF clipping path"));
    }        
}

static void PDFwriteMask(int i, PDFDesc *pd)
{
    char buf[20];
    if (pd->current.mask != i) {
        PDFwrite(buf, 20, "/Def%d gs\n", pd, i);
        pd->current.mask = i;
    }
}

static void PDFStrokePath(int i, PDFDesc *pd)
{
    char* buf1;
    char buf2[10];
    size_t len = strlen(pd->definitions[i].str);
    buf1 = malloc((len + 1)*sizeof(char));

    if (buf1) {
        PDFwrite(buf1, len + 1, "%s", pd, pd->definitions[i].str);
        PDFwrite(buf2, 10, " S n\n", pd);
        free(buf1);
    } else {
        warning(_("Failed to write PDF stroke"));
    }        
}

static void PDFFillPath(int i, int rule, PDFDesc *pd)
{
    char* buf1;
    char buf2[10];
    size_t len = strlen(pd->definitions[i].str);
    buf1 = malloc((len + 1)*sizeof(char));

    if (buf1) {
        PDFwrite(buf1, len + 1, "%s", pd, pd->definitions[i].str);
        switch (rule) {
        case R_GE_nonZeroWindingRule:
            PDFwrite(buf2, 10, " f n\n", pd); break;
        case R_GE_evenOddRule:
            PDFwrite(buf2, 10, " f* n\n", pd); break;
        }
        free(buf1);
    } else {
        warning(_("Failed to write PDF fill"));        
    }
}

static void PDFFillStrokePath(int i, int rule, PDFDesc *pd)
{
    char* buf1;
    char buf2[10];
    size_t len = strlen(pd->definitions[i].str);
    buf1 = malloc((len + 1)*sizeof(char));

    if (buf1) {
        PDFwrite(buf1, len + 1, "%s", pd, pd->definitions[i].str);
        switch (rule) {
        case R_GE_nonZeroWindingRule:
            PDFwrite(buf2, 10, " B n\n", pd); break;
        case R_GE_evenOddRule:
            PDFwrite(buf2, 10, " B* n\n", pd); break;
        }        
        free(buf1);
    } else {
        warning(_("Failed to write PDF fillStroke"));
    }
}

/* This was an optimization that has effectively been disabled in
   2.8.0, to avoid repeatedly going in and out of text mode.  Howver,
   Acrobat puts all text rendering calls in BT...ET into a single
   transparency group, and other viewers do not.  So for consistent
   rendering we put each text() call into a separate group.
*/
static void texton(PDFDesc *pd)
{
    char buf[10];
    PDFwrite(buf, 10, "BT\n", pd);
    pd->inText = TRUE;
}

static void textoff(PDFDesc *pd)
{
    char buf[10];
    PDFwrite(buf, 10, "ET\n", pd);
    pd->inText = FALSE;
}

static void PDFGlyphs(int n, int *glyphs, double *x, double *y, 
                      double size, double rot, PDFDesc *pd) {
    int i;
    char buf[200];
    double a, b, bm, rot1; 

    rot1 = rot * DEG2RAD;
    a = size * cos(rot1);
    b = size * sin(rot1);
    bm = -b;
    /* avoid printing -0.00 on rotated text */
    if(fabs(a) < 0.01) a = 0.0;
    if(fabs(b) < 0.01) {b = 0.0; bm = 0.0;}

    if(!pd->inText) texton(pd);

    PDFwrite(buf, 200, "/glyph-font-%d 1 Tf\n", pd, pd->numGlyphFonts);

    for (i=0; i<n; i++) {
        PDFwrite(buf, 200, "%.2f %.2f %.2f %.2f %.2f %.2f Tm ", pd,
                 a, b, bm, a, x[i], y[i]);
        if (glyphs[i] > 0xFFFF) 
            warning(_("Glyph ID larger than 0xFFFF; output will be incorrect"));
        PDFwrite(buf, 200, "<%04x> Tj\n", pd, glyphs[i]);        
    }

    textoff(pd); 
}
                      
/*
 * Search through the alphas used so far and return
 * existing index if there is one.
 * Otherwise, add alpha to the list and return new index
 */
static int alphaIndex(int alpha, short *alphas) {
    int i, found = 0;
    for (i = 0; i < 256 && !found; i++) {
	if (alphas[i] < 0) {
	    alphas[i] = (short) alpha;
	    found = 1;
	}
	else if (alpha == alphas[i])
	    found = 1;
    }
    if (!found)
	error(_("invalid 'alpha' value in PDF"));
    return i;
}

/*
 * colAlpha graphics state parameter dictionaries are named
 * /GS1 to /GS256
 * fillAlpha graphics state parameter dictionaries are named
 * /GS257 to /GS512
 */
static int colAlphaIndex(int alpha, PDFDesc *pd) {
    return alphaIndex(alpha, pd->colAlpha);
}

static int fillAlphaIndex(int alpha, PDFDesc *pd) {
    return alphaIndex(alpha, pd->fillAlpha) + 256;
}

static void PDFwriteGroup(int i, PDFDesc *pd)
{
    char buf[20];

    /* Ensure stroke and fill alpha are 1 when group is used
     * (because group definition will have recorded any pre-existing alpha
     *  at definition time, and the current stroke and fill alpha
     *  may have been set to non-opaque due to previous drawing,
     *  and we do not want to double-up the alpha level).
     *
     * https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdfs/pdf_reference_archives/PDFReference.pdf
     * 
     * "Before execution of the transparency group XObject's content
     *  stream, the current blend mode in the graphics state is
     *  initialized to Normal, the current stroking and nonstroking alpha
     *  constants to 1.0, and the current soft mask to None."
     */
    PDFwrite(buf, 20, "/GS%i gs\n", pd, colAlphaIndex(255, pd));
    PDFwrite(buf, 20, "/GS%i gs\n", pd, fillAlphaIndex(255, pd));    
    /* Draw the transparency group */
    PDFwrite(buf, 20, "/Def%d Do\n", pd, i);
}

static void PDFwriteDefinitions(int resourceDictOffset, PDFDesc *pd)
{
    int defnOffset = pd->nobjs;
    for (int i = 0; i < pd->numDefns; i++) {
        /* All definitions written out, to keep the math somewhere near sane,
         * but some definitions are just empty here
         * (e.g., clipping paths are written inline every time 
         *  they are used rather than here AND temporary mask content
         *  is still hanging around)
         */
        pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
        /* Definition object number */
        fprintf(pd->pdffp, "%d", pd->nobjs);
        if (pd->definitions[i].type == PDFclipPath ||
            pd->definitions[i].type == PDFstrokePath ||
            pd->definitions[i].type == PDFfillPath ||
            pd->definitions[i].type == PDFfillStrokePath ||
            pd->definitions[i].type == PDFtemp) {
            fprintf(pd->pdffp, " 0 obj << >> endobj\n");
        } else if (pd->definitions[i].type == PDFshadingPattern) {
            /* IF semitransparent shading, 
             * need to complete mask at end of file to get its
             * content object number right */
            completeShading(i, defnOffset, pd);
            fputs(pd->definitions[i].str, pd->pdffp);
        } else if (pd->definitions[i].type == PDFtilingPattern) {
            /* Need to complete tiling pattern at end of file
             * to get its Resource Dictionary right */         
            completeTiling(i, resourceDictOffset, pd);
            fputs(pd->definitions[i].str, pd->pdffp);
        } else if (pd->definitions[i].type == PDFsoftMask) {
            /* Need to complete mask at end of file to get its
             * content object number right */
            completeMask(i, defnOffset, pd);
            fputs(pd->definitions[i].str, pd->pdffp);
        } else {
            fputs(pd->definitions[i].str, pd->pdffp);
        }    
    }
}


/***********************************************************************
 * Some stuff for recording raster images
 */
/* Detect an image by non-NULL rasters[] */
static rasterImage* initRasterArray(int numRasters) 
{
    int i;
    /* why not use calloc? */
    rasterImage* rasters = malloc(numRasters*sizeof(rasterImage));
    if (rasters) {
	for (i = 0; i < numRasters; i++) {
	    rasters[i].raster = NULL;
	}
    } /* else error thrown in PDFDeviceDriver */
    return rasters;
}

/* Add a raster (by making a copy)
 * Return value indicates whether the image is semi-transparent
 */
static int addRaster(rcolorPtr raster, int w, int h,
		     Rboolean interpolate, PDFDesc *pd) 
{
    int i, alpha = 0;
    rcolorPtr newRaster;

    if (pd->numRasters == pd->maxRasters) {
	int new = 2*pd->maxRasters;
	void *tmp;
	/* Do it this way so previous pointer is retained if it fails */
	tmp = realloc(pd->masks, new*sizeof(int));
	if(!tmp) error(_("failed to increase 'maxRaster'"));
	pd->masks = tmp;
	tmp = realloc(pd->rasters, new*sizeof(rasterImage));
	if(!tmp) error(_("failed to increase 'maxRaster'"));
	pd->rasters = tmp;
	for (i = pd->maxRasters; i < new; i++) {
	    pd->rasters[i].raster = NULL;
	    pd->masks[i] = -1;
	}
	pd->maxRasters = new;
    }

    newRaster = malloc(w*h*sizeof(rcolor));

    if (!newRaster)
	error(_("unable to allocate raster image"));

    for (i = 0; i < w*h; i++) {
	newRaster[i] = raster[i];
	if (!alpha && R_ALPHA(raster[i]) < 255) alpha = 1;
    }
    pd->rasters[pd->numRasters].raster = newRaster;
    pd->rasters[pd->numRasters].w = w;
    pd->rasters[pd->numRasters].h = h;
    pd->rasters[pd->numRasters].interpolate = interpolate;
    pd->rasters[pd->numRasters].nobj = -1; /* not yet written out */
    pd->rasters[pd->numRasters].nmaskobj = -1; /* not yet written out */

    /* If any of the pixels are not opaque, we need to add
     * a mask as well */
    if (alpha)
	pd->masks[pd->numRasters] = pd->numMasks++;

    pd->numRasters++;

    return alpha;
}

static void killRasterArray(rasterImage *rasters, int numRasters) {
    int i;
    for (i = 0; i < numRasters; i++)
	if (rasters[i].raster != NULL) free(rasters[i].raster);
}

/* Detect a mask by masks[] >= 0 */
static int* initMaskArray(int numRasters) {
    int i;
    int* masks = malloc(numRasters*sizeof(int));
    if (masks) {
	for (i = 0; i < numRasters; i++) masks[i] = -1;
    } /* else error thrown in PDFDeviceDriver */
    return masks;
}

static void writeRasterXObject(rasterImage raster, int n,
			       int mask, int maskObj, PDFDesc *pd)
{
    Bytef *buf, *buf2, *p;
    uLong inlen;
    
    if (streql(pd->colormodel, "gray")) {
	inlen = raster.w * raster.h;
	p = buf = R_Calloc(inlen, Bytef);
	for(int i = 0; i < raster.w * raster.h; i++) {
	    double r =  0.213 * R_RED(raster.raster[i]) 
		+ 0.715 * R_GREEN(raster.raster[i])
		+ 0.072 * R_BLUE(raster.raster[i]);
	    *p++ = (Bytef)(r + 0.49);
	}
    } else {
	inlen = 3 * raster.w * raster.h;
	p = buf = R_Calloc(inlen, Bytef);
	for(int i = 0; i < raster.w * raster.h; i++) {
	    *p++ = R_RED(raster.raster[i]);
	    *p++ = R_GREEN(raster.raster[i]);
	    *p++ = R_BLUE(raster.raster[i]);
	}
    }
    uLong outlen = inlen;
    if (pd->useCompression) {
        outlen += (inlen >> 10) + 20; // (1.001*inlen + 20) warns [-Wconversion]; 2^(-10) ~= 0.001
	buf2 = R_Calloc(outlen, Bytef);
	int res = compress(buf2, &outlen, buf, inlen);
	if(res != Z_OK) error("internal error %d in writeRasterXObject", res);
	R_Free(buf);
	buf = buf2;
    }
    fprintf(pd->pdffp, "%d 0 obj <<\n", n);
    fprintf(pd->pdffp, "  /Type /XObject\n");
    fprintf(pd->pdffp, "  /Subtype /Image\n");
    fprintf(pd->pdffp, "  /Width %d\n", raster.w);
    fprintf(pd->pdffp, "  /Height %d\n", raster.h);
    if (streql(pd->colormodel, "gray"))
	fprintf(pd->pdffp, "  /ColorSpace /DeviceGray\n");
    else if (streql(pd->colormodel, "srgb"))
	fprintf(pd->pdffp, "  /ColorSpace 5 0 R\n"); /* sRGB */
    else
	fprintf(pd->pdffp, "  /ColorSpace /DeviceRGB\n");
    fprintf(pd->pdffp, "  /BitsPerComponent 8\n");
    fprintf(pd->pdffp, "  /Length %u\n", (unsigned) 
	    (pd->useCompression ? outlen : 2 * outlen + 1));
    if (raster.interpolate)
	fprintf(pd->pdffp, "  /Interpolate true\n");
    if (pd->useCompression)
	fprintf(pd->pdffp, "  /Filter /FlateDecode\n");
    else
	fprintf(pd->pdffp, "  /Filter /ASCIIHexDecode\n");
    if (mask >= 0)
	fprintf(pd->pdffp, "  /SMask %d 0 R\n", maskObj);
    fprintf(pd->pdffp, "  >>\nstream\n");
    if (pd->useCompression) {
	size_t res = fwrite(buf, 1, outlen, pd->pdffp);
	if(res != outlen) error(_("write failed"));
    } else {
	for(int i = 0; i < outlen; i++)
	    fprintf(pd->pdffp, "%02x", buf[i]);
	fprintf(pd->pdffp, ">\n");
    }
    R_Free(buf);
    fprintf(pd->pdffp, "endstream\nendobj\n");
}

static void writeMaskXObject(rasterImage raster, int n, PDFDesc *pd) 
{
    Bytef *buf, *buf2, *p;
    uLong inlen = raster.w * raster.h, outlen = inlen;
    p = buf = R_Calloc(outlen, Bytef);
    for(int i = 0; i < raster.w * raster.h; i++) 
        *p++ = (Bytef)R_ALPHA(raster.raster[i]);
    if (pd->useCompression) {
        outlen += (inlen >> 10) + 20;
	buf2 = R_Calloc(outlen, Bytef);
	int res = compress(buf2, &outlen, buf, inlen);
	if(res != Z_OK) error("internal error %d in writeRasterXObject", res);
	R_Free(buf);
	buf = buf2;
    }
    fprintf(pd->pdffp, "%d 0 obj <<\n", n);
    fprintf(pd->pdffp, "  /Type /XObject\n");
    fprintf(pd->pdffp, "  /Subtype /Image\n");
    fprintf(pd->pdffp, "  /Width %d\n", raster.w);
    fprintf(pd->pdffp, "  /Height %d\n", raster.h);
    /* This is not a mask but a 'soft mask' */
    fprintf(pd->pdffp, "  /ColorSpace /DeviceGray\n");
    fprintf(pd->pdffp, "  /BitsPerComponent 8\n");
    fprintf(pd->pdffp, "  /Length %u\n", (unsigned) 
	    (pd->useCompression ? outlen : 2 * outlen + 1));
    if (raster.interpolate)
	fprintf(pd->pdffp, "  /Interpolate true\n");
    if (pd->useCompression)
	fprintf(pd->pdffp, "  /Filter /FlateDecode\n");
    else
	fprintf(pd->pdffp, "  /Filter /ASCIIHexDecode\n");
    fprintf(pd->pdffp, "  >>\nstream\n");
    if (pd->useCompression) {
	size_t res = fwrite(buf, 1, outlen, pd->pdffp);
	if(res != outlen) error(_("write failed"));
    } else {
	for(int i = 0; i < outlen; i++)
	    fprintf(pd->pdffp, "%02x", buf[i]);
	fprintf(pd->pdffp, ">\n");
    }
    R_Free(buf);
    fprintf(pd->pdffp, "endstream\nendobj\n");
}

/***********************************************************************
 * Some stuff for fonts
 */
/*
 * Add a graphics engine font family to the list of fonts used on a
 * PDF device ...
 *
 * ... AND add the font encoding to the list of encodings used on the
 * device (if necessary)
 */
/*
 * Differs from addDeviceFont (used in PostScript device)
 * because we do not need to immediately write font
 * information to file.  In PDF, the font information is
 * all written at the end as part of the file footer.
 */
static bool addPDFDeviceCIDfont(cidfontfamily family,
				    PDFDesc *pd,
				    int *fontIndex)
{
    Rboolean result = FALSE;
    cidfontlist fontlist = addDeviceCIDFont(family, pd->cidfonts, fontIndex);
    if (fontlist) {
	pd->cidfonts = fontlist;
	result = true;
    }
    return result;
}

static bool addPDFDevicefont(type1fontfamily family,
				 PDFDesc *pd,
				 int *fontIndex)
{
    Rboolean result = false;
    type1fontlist fontlist = addDeviceFont(family, pd->fonts, fontIndex);
    if (fontlist) {
	int dontcare;
	encodinginfo encoding =
	    findDeviceEncoding(family->encoding->encpath,
			       pd->encodings, &dontcare);
	if (encoding) {
	    pd->fonts = fontlist;
	    result = true;
	} else {
	    /*
	     * The encoding should have been loaded when the font was loaded
	     */
	    encoding = findEncoding(family->encoding->encpath,
				    pd->encodings, TRUE);
	    if (!encoding) {
		warning(_("corrupt loaded encodings;  font not added"));
		/* NOTE: in fact the font was added */
	    } else {
		encodinglist enclist = addDeviceEncoding(encoding,
							 pd->encodings);
		if (enclist) {
		    pd->fonts = fontlist;
		    pd->encodings = enclist;
		    result = true;
		} else
		    warning(_("failed to record device encoding; font not added"));
		    /* NOTE: in fact the font was added */
	    }
	}
    }
    return result;
}

static void PDFcleanup(int stage, PDFDesc *pd) {
    switch (stage) {
    case 7: /* Allocated defns */
        killDefinitions(pd);
    case 6: /* Allocated masks */
	free(pd->masks);
    case 5: /* Allocated rasters */
	free(pd->rasters);
    case 4: /* Allocated fonts */
	freeDeviceFontList(pd->fonts);
	freeDeviceCIDFontList(pd->cidfonts);
	freeDeviceEncList(pd->encodings);
	pd->fonts = NULL;
	pd->cidfonts = NULL;
	pd->encodings = NULL;
    case 3: /* Allocated pageobj */
	free(pd->pageobj);
    case 2: /* Allocated pos */
	free(pd->pos);
    case 1: /* Allocated PDFDesc */
	free(pd);
    }
}

bool
PDFDeviceDriver(pDevDesc dd, const char *file, const char *paper,
		const char *family, const char **afmpaths,
		const char *encoding,
		const char *bg, const char *fg, double width, double height,
		double ps, bool onefile, bool pagecentre,
		const char *title, SEXP fonts,
		int versionMajor, int versionMinor,
		const char *colormodel, int dingbats, int useKern,
		bool fillOddEven, bool useCompression, 
		bool timestamp, bool producer, const char *author)
{
    /* If we need to bail out with some sort of "error" */
    /* then we must free(dd) */

    int i, gotFont;
    double xoff = 0.0, yoff = 0.0, pointsize;
    rcolor setbg, setfg;
    encodinginfo enc;
    encodinglist enclist;
    type1fontfamily font;
    cidfontfamily cidfont = NULL;

    PDFDesc *pd;

    /* Check and extract the device parameters */

    /* 'file' could be NULL */
    if(file && strlen(file) > R_PATH_MAX - 1) {
	/* not yet created PDFcleanup(0, pd); */
	free(dd);
	error(_("filename too long in %s()"), "pdf");
    }

    /* allocate new PDF device description */
    if (!(pd = (PDFDesc *) malloc(sizeof(PDFDesc)))) {
	free(dd);
	error(_("memory allocation problem in %s()"), "pdf");
    }
    /* from here on, if need to bail out with "error", must also
       free(pd) */

    pd->versionMajor = versionMajor;
    pd->versionMinor = versionMinor;
    /* Precaution: should be initialized in PDF_newpage, but package
       PerformanceAnalytics manages to call PDF_Clip without.  */
    pd->inText = FALSE;

    /* This is checked at the start of every page.  We typically have
       three objects per page plus one or two for each raster image, 
       so this is an ample initial allocation.
     */
    pd->max_nobjs = 2000;
    pd->pos = (int *) calloc(pd->max_nobjs, sizeof(int));
    if(!pd->pos) {
	PDFcleanup(1, pd);
	free(dd);
	error("cannot allocate pd->pos");
    }
    /* This one is dynamic: initial allocation */
    pd->pagemax = 100;
    pd->pageobj = (int *) calloc(pd->pagemax, sizeof(int));
    if(!pd->pageobj) {
	PDFcleanup(2, pd);
	free(dd);
	error("cannot allocate pd->pageobj");
    }


    /* initialize PDF device description */
    /* 'file' could be NULL */
    if (file) 
        strcpy(pd->filename, file);
    else 
        strcpy(pd->filename, "nullPDF");
    safestrcpy(pd->papername, paper, 64);
    strncpy(pd->title, title, 1023);
    pd->title[1023] = '\0';
    memset(pd->fontUsed, 0, 100*sizeof(bool));
    if (streql(colormodel, "grey")) strcpy(pd->colormodel, "gray");
    else {strncpy(pd->colormodel, colormodel, 29); pd->colormodel[29] = '\0';}
    pd->dingbats = (dingbats != 0);
    pd->useKern = (useKern != 0);
    pd->fillOddEven = fillOddEven;
    pd->useCompression = useCompression;
    pd->timestamp = timestamp;
    pd->producer = producer;
    safestrcpy(pd->author, author, 1024);
    if(useCompression && pd->versionMajor == 1 && pd->versionMinor < 2) {
	pd->versionMinor = 2;
	warning(_("increasing the PDF version to 1.2"));
    }

    pd->width = width;
    pd->height = height;

    if (file)
        pd->offline = FALSE;
    else 
        pd->offline = TRUE;

    if(strlen(encoding) > R_PATH_MAX - 1) {
	PDFcleanup(3, pd);
	free(dd);
	error(_("encoding path is too long in %s()"), "pdf");
    }
    /*
     * Load the default encoding AS THE FIRST ENCODING FOR THIS DEVICE.
     *
     * encpath MUST NOT BE "default"
     */
    pd->encodings = NULL;
    if (!(enc = findEncoding(encoding, pd->encodings, TRUE)))
	enc = addEncoding(encoding, 1);
    if (enc && (enclist = addDeviceEncoding(enc,
					    pd->encodings))) {
	pd->encodings = enclist;
    } else {
	PDFcleanup(3, pd);
	free(dd);
	error(_("failed to load default encoding"));
    }

    /*****************************
     * Load fonts
     *****************************/
    pd->fonts = NULL;
    pd->cidfonts = NULL;

    gotFont = 0;
    /*
     * If user specified afms then assume the font hasn't been loaded
     * Could lead to redundant extra loading of a font, but not often(?)
     */
    if (!strcmp(family, "User")) {
	font = addDefaultFontFromAFMs(encoding, afmpaths, 0, pd->encodings);
    } else {
	/*
	 * Otherwise, family is a device-independent font family.
	 * One of the elements of pdfFonts().
	 * NOTE this is the first font loaded on this device!
	 */
	/*
	 * Check first whether this font has been loaded
	 * in this R session
	 */
	font = findLoadedFont(family, encoding, TRUE);
	cidfont = findLoadedCIDFont(family, TRUE);
	if (!(font || cidfont)) {
	    /*
	     * If the font has not been loaded yet, load it.
	     *
	     * The family SHOULD be in the font database to get this far.
	     * (checked at R level in postscript() in postscript.R)
	     */
	    if (isType1Font(family, PDFFonts, NULL)) {
		font = addFont(family, TRUE, pd->encodings);
	    } else if (isCIDFont(family, PDFFonts, NULL)) {
		cidfont = addCIDFont(family, TRUE);
	    } else {
		/*
		 * Should NOT get here.
		 */
		error(_("invalid font type"));
	    }
	}
    }
    if (font || cidfont) {
	/*
	 * At this point the font is loaded, so add it to the
	 * device's list of fonts.
	 */
	if (!strcmp(family, "User") ||
	    isType1Font(family, PDFFonts, NULL)) {
	    addPDFDevicefont(font, pd, &gotFont);
	    /* NOTE: should check result, encoding may not have been found */
	    pd->defaultFont = pd->fonts->family;
	    pd->defaultCIDFont = NULL;
	} else /* (isCIDFont(family, PDFFonts)) */ {
	    addPDFDeviceCIDfont(cidfont, pd, &gotFont);
	    pd->defaultFont = NULL;
	    pd->defaultCIDFont = pd->cidfonts->cidfamily;
	}
    }
    if (!gotFont) {
	PDFcleanup(4, pd);
	free(dd);
	error(_("failed to initialise default PDF font"));
    }

    /*
     * Load the font names sent in via the fonts arg
     * NOTE that these are the font names specified at the
     * R-level, NOT the translated font names.
     */
    if (!isNull(fonts)) {
	int i, dontcare, gotFonts = 0, nfonts = LENGTH(fonts);
	for (i = 0; i < nfonts; i++) {
	    int index, cidindex;
	    const char *name = CHAR(STRING_ELT(fonts, i));
	    if (findDeviceFont(name, pd->fonts, &index) ||
		findDeviceCIDFont(name, pd->cidfonts, &cidindex))
		gotFonts++;
	    else {
		/*
		 * Check whether the font is loaded and, if not,
		 * load it.
		 */
		font = findLoadedFont(name, encoding, TRUE);
		cidfont = findLoadedCIDFont(name, TRUE);
		if (!(font || cidfont)) {
		    if (isType1Font(name, PDFFonts, NULL)) {
			font = addFont(name, TRUE, pd->encodings);
		    } else if (isCIDFont(name, PDFFonts, NULL)) {
			cidfont = addCIDFont(name, TRUE);
		    } else {
			/*
			 * Should NOT get here.
			 */
			error(_("invalid font type"));
		    }
		}
		/*
		 * Once the font is loaded, add it to the device's
		 * list of fonts.
		 */
		if (font || cidfont) {
		    if (isType1Font(name, PDFFonts, NULL)) {
			if (addPDFDevicefont(font, pd, &dontcare)) {
			    gotFonts++;
			}
		    } else /* (isCIDFont(family, PDFFonts)) */ {
			if (addPDFDeviceCIDfont(cidfont, pd, &dontcare)) {
			    gotFonts++;
			}
		    }
		}
	    }
	}
	if (gotFonts < nfonts) {
	    PDFcleanup(4, pd);
	    free(dd);
	    error(_("failed to initialise additional PDF fonts"));
	}
    }
    /*****************************
     * END Load fonts
     *****************************/

    pd->numRasters = pd->writtenRasters = pd->fileRasters = 0;
    pd->maxRasters = 64; /* dynamic */
    pd->rasters = initRasterArray(pd->maxRasters);
    if (!pd->rasters) {
	PDFcleanup(4, pd);
	free(dd);
	error(_("failed to allocate rasters"));
    }
    pd->numMasks = 0;
    pd->masks = initMaskArray(pd->maxRasters);
    if (!pd->masks) {
	PDFcleanup(5, pd);
	free(dd);
	error(_("failed to allocate masks"));
    }

    pd->numDefns = 0;
    pd->maxDefns = 64;
    initBlendModes(pd);
    initDefinitions(pd);
    if (!pd->definitions) {
        PDFcleanup(6, pd);
        free(dd);
	error(_("failed to allocate definitions"));
    }
    pd->appendingPath = -1;
    pd->pathContainsText = FALSE;
    pd->pathContainsDrawing = FALSE;
    pd->appendingMask = -1;
    pd->currentMask = -1;
    pd->appendingPattern = -1;
    pd->appendingGroup = -1;
    pd->numGlyphFonts = 0;

    setbg = R_GE_str2col(bg);
    setfg = R_GE_str2col(fg);

    /*
     * Initialise all alphas to -1
     */
    pd->usedAlpha = FALSE;
    for (i = 0; i < 256; i++) {
	pd->colAlpha[i] = -1;
	pd->fillAlpha[i] = -1;
    }

    /* Deal with paper and plot size and orientation */

    if(!strcmp(pd->papername, "Default") ||
       !strcmp(pd->papername, "default")) {
	SEXP s = STRING_ELT(GetOption1(install("papersize")), 0);
	if(s != NA_STRING && strlen(CHAR(s)) > 0)
	    strcpy(pd->papername, CHAR(s));
	else strcpy(pd->papername, "a4");
    }
    if(!strcmp(pd->papername, "A4") ||
       !strcmp(pd->papername, "a4")) {
	pd->pagewidth  = 21.0 / 2.54;
	pd->pageheight = 29.7  /2.54;
    }
    else if(!strcmp(pd->papername, "A4r") ||
       !strcmp(pd->papername, "a4r")) {
	pd->pageheight = 21.0 / 2.54;
	pd->pagewidth  = 29.7  /2.54;
    }
    else if(!strcmp(pd->papername, "Letter") ||
	    !strcmp(pd->papername, "letter") ||
	    !strcmp(pd->papername, "US") ||
	    !strcmp(pd->papername, "us")) {
	pd->pagewidth  =  8.5;
	pd->pageheight = 11.0;
    }
    else if(!strcmp(pd->papername, "USr") ||
	    !strcmp(pd->papername, "usr")) {
	pd->pageheight =  8.5;
	pd->pagewidth  = 11.0;
    }
    else if(!strcmp(pd->papername, "Legal") ||
	    !strcmp(pd->papername, "legal")) {
	pd->pagewidth  =  8.5;
	pd->pageheight = 14.0;
    }
    else if(!strcmp(pd->papername, "Executive") ||
	    !strcmp(pd->papername, "executive")) {
	pd->pagewidth  =  7.25;
	pd->pageheight = 10.5;
    }
    else if(!strcmp(pd->papername, "special")) {
      pd->pagewidth  =  width;
      pd->pageheight = height;
    }
    else {
	char errbuf[strlen(pd->papername) + 1];
	strcpy(errbuf, pd->papername);
	PDFcleanup(7, pd);
	free(dd);
	error(_("invalid paper type '%s' (pdf)"), errbuf);
    }
    pd->pagecentre = pagecentre;
    pd->paperwidth = (int)(72 * pd->pagewidth);
    pd->paperheight = (int)(72 * pd->pageheight);
    if(strcmp(pd->papername, "special"))
    {
	if(pd->width < 0.1 || pd->width > pd->pagewidth-0.5)
	    pd->width = pd->pagewidth-0.5;
	if(pd->height < 0.1 || pd->height > pd->pageheight-0.5)
	    pd->height = pd->pageheight-0.5;
    }
    if(pagecentre)
    {
	xoff = (pd->pagewidth - pd->width)/2.0;
	yoff = (pd->pageheight - pd->height)/2.0;
    } else {
	xoff = yoff = 0.0;
    }

    pointsize = floor(ps);
    if(R_TRANSPARENT(setbg) && R_TRANSPARENT(setfg)) {
	PDFcleanup(7, pd);
	free(dd);
	error(_("invalid foreground/background color (pdf)"));
    }

    pd->onefile = onefile;
    pd->maxpointsize = (int)(72.0 * ((pd->pageheight > pd->pagewidth) ?
				     pd->pageheight : pd->pagewidth));
    pd->pageno = pd->fileno = 0;
    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */
    /* Only right for 12 point font. */
    /* Max pointsize suggested by Peter Dalgaard */

    if(pointsize < 6.0) pointsize = 6.0;
    if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
    dd->startps = pointsize;
    dd->startlty = 0;
    dd->startfont = 1;
    dd->startfill = setbg;
    dd->startcol = setfg;
    dd->startgamma = 1;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->left = 72 * xoff;			/* left */
    dd->right = 72 * (xoff + pd->width);	/* right */
    dd->bottom = 72 * yoff;			/* bottom */
    dd->top = 72 * (yoff + pd->height);	/* top */
    dd->clipLeft = dd->left; dd->clipRight = dd->right;
    dd->clipBottom = dd->bottom; dd->clipTop = dd->top;

    dd->cra[0] = 0.9 * pointsize;
    dd->cra[1] = 1.2 * pointsize;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->xCharOffset =  0.4900;
    dd->yCharOffset =  0.3333;
    dd->yLineBias = 0.2;

    /* Inches per Raster Unit */
    /* 1200 dpi */
    dd->ipr[0] = 1.0/72.0;
    dd->ipr[1] = 1.0/72.0;

    dd->canClip = TRUE;
    dd->canHAdj = 0;
    dd->canChangeGamma = FALSE;

    /*	Start the driver */
    PDF_Open(dd, pd); /* errors on failure */

    dd->close      = PDF_Close;
    dd->size     = PDF_Size;
    dd->newPage    = PDF_NewPage;
    dd->clip	      = PDF_Clip;
    dd->text	      = PDF_Text;
    dd->strWidth   = PDF_StrWidth;
    dd->metricInfo = PDF_MetricInfo;
    dd->rect	      = PDF_Rect;
    dd->path	      = PDF_Path;
    dd->raster	      = PDF_Raster;
    dd->circle     = PDF_Circle;
    dd->line	      = PDF_Line;
    dd->polygon    = PDF_Polygon;
    dd->polyline   = PDF_Polyline;
    /* dd->locator    = PDF_Locator;
       dd->mode	      = PDF_Mode; */
    dd->hasTextUTF8   = TRUE;
    dd->textUTF8       = PDF_TextUTF8;
    dd->strWidthUTF8   = PDF_StrWidthUTF8;
    dd->useRotatedTextInContour = TRUE;
    dd->haveTransparency = 2;
    dd->haveTransparentBg = 3;
    dd->haveRaster = 2;
    dd->setPattern      = PDF_setPattern;
    dd->releasePattern  = PDF_releasePattern;
    dd->setClipPath     = PDF_setClipPath;
    dd->releaseClipPath = PDF_releaseClipPath;
    dd->setMask         = PDF_setMask;
    dd->releaseMask     = PDF_releaseMask;
    dd->defineGroup     = PDF_defineGroup;
    dd->useGroup        = PDF_useGroup;
    dd->releaseGroup    = PDF_releaseGroup;
    dd->stroke          = PDF_stroke;
    dd->fill            = PDF_fill;
    dd->fillStroke      = PDF_fillStroke;
    dd->capabilities    = PDF_capabilities;
    dd->glyph           = PDF_glyph;

    dd->deviceSpecific = (void *) pd;
    dd->displayListOn = FALSE;
    dd->deviceVersion = R_GE_glyphs;
    return TRUE;
}


/*
 * Does the version support alpha transparency?
 * As from R 2.4.0 bump the version number so it does.
 */
static void alphaVersion(PDFDesc *pd) {
    if(pd->versionMajor == 1 && pd->versionMinor < 4) {
	pd->versionMinor  = 4;
	warning(_("increasing the PDF version to 1.4"));
    }
    pd->usedAlpha = TRUE;
}

static void PDF_SetLineColor(int color, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char buf[100];

    if(color != pd->current.col) {
	unsigned int alpha = R_ALPHA(color);
	if (0 < alpha && alpha < 255) alphaVersion(pd);
	if (pd->usedAlpha) {
	    /*
	     * Apply graphics state parameter dictionary
	     * to set alpha
	     */
	    PDFwrite(buf, 100, "/GS%i gs\n", pd, colAlphaIndex(alpha, pd));
	}
	if(streql(pd->colormodel, "gray")) {
	    double r = R_RED(color)/255.0, g = R_GREEN(color)/255.0,
		b = R_BLUE(color)/255.0;
	    /* weights from C-9 of 
	       http://www.faqs.org/faqs/graphics/colorspace-faq/ 
	       Those from C-11 might be more appropriate.
	    */
	    PDFwrite(buf, 100, "%.3f G\n", pd, (0.213*r+0.715*g+0.072*b));
	} else if(streql(pd->colormodel, "cmyk")) {
	    double r = R_RED(color)/255.0, g = R_GREEN(color)/255.0,
		b = R_BLUE(color)/255.0;
	    double c = 1.0-r, m = 1.0-g, y = 1.0-b, k = c;
	    k = fmin2(k, m);
	    k = fmin2(k, y);
	    if(k == 1.0) c = m = y = 0.0;
	    else { c = (c-k)/(1-k); m = (m-k)/(1-k); y = (y-k)/(1-k); }
	    PDFwrite(buf, 100, "%.3f %.3f %.3f %.3f K\n", pd, c, m, y, k);
	} else if(streql(pd->colormodel, "rgb")) {
	    PDFwrite(buf, 100, "%.3f %.3f %.3f RG\n", pd,
		    R_RED(color)/255.0,
		    R_GREEN(color)/255.0,
		    R_BLUE(color)/255.0);
	} else {
	    if (!streql(pd->colormodel, "srgb"))
		warning(_("unknown 'colormodel', using 'srgb'"));
	    if (!pd->current.srgb_bg) {
		PDFwrite(buf, 100, "/sRGB CS\n", pd);
		pd->current.srgb_bg = 1;
	    }
	    PDFwrite(buf, 100, "%.3f %.3f %.3f SCN\n", pd,
		    R_RED(color)/255.0,
		    R_GREEN(color)/255.0,
		    R_BLUE(color)/255.0);
	}
	pd->current.col = color;
    }
}

static void PDF_SetFill(int color, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char buf[100];
    if(color != pd->current.fill) {
	unsigned int alpha = R_ALPHA(color);
	if (0 < alpha && alpha < 255) alphaVersion(pd);
	if (pd->usedAlpha) {
	    /*
	     * Apply graphics state parameter dictionary
	     * to set alpha
	     */
	    PDFwrite(buf, 100, "/GS%i gs\n", pd, fillAlphaIndex(alpha, pd));
	}
	if(streql(pd->colormodel, "gray")) {
	    double r = R_RED(color)/255.0, g = R_GREEN(color)/255.0,
		b = R_BLUE(color)/255.0;
	    PDFwrite(buf, 100, "%.3f g\n", pd, (0.213*r+0.715*g+0.072*b));
	} else if(streql(pd->colormodel, "cmyk")) {
	    double r = R_RED(color)/255.0, g = R_GREEN(color)/255.0,
		b = R_BLUE(color)/255.0;
	    double c = 1.0-r, m = 1.0-g, y = 1.0-b, k = c;
	    k = fmin2(k, m);
	    k = fmin2(k, y);
	    if(k == 1.0) c = m = y = 0.0;
	    else { c = (c-k)/(1-k); m = (m-k)/(1-k); y = (y-k)/(1-k); }
	    PDFwrite(buf, 100, "%.3f %.3f %.3f %.3f k\n", pd, c, m, y, k);
	} else if(streql(pd->colormodel, "rgb")) {
	    PDFwrite(buf, 100, "%.3f %.3f %.3f rg\n", pd,
		    R_RED(color)/255.0,
		    R_GREEN(color)/255.0,
		    R_BLUE(color)/255.0);
	} else {
	    if (!streql(pd->colormodel, "srgb"))
		warning(_("unknown 'colormodel', using 'srgb'"));
	    if (!pd->current.srgb_fg) {
		PDFwrite(buf, 100, "/sRGB cs\n", pd);
		pd->current.srgb_fg = 1;
	    }
	    PDFwrite(buf, 100, "%.3f %.3f %.3f scn\n", pd,
		    R_RED(color)/255.0,
		    R_GREEN(color)/255.0,
		    R_BLUE(color)/255.0);
	}

	pd->current.fill = color;
    }
    /* Fill set means pattern fill not set */
    pd->current.patternfill = -1;
}

static void PDF_SetPatternFill(SEXP ref, pDevDesc dd) 
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int patternIndex = INTEGER(ref)[0];

    if (pd->current.patternfill != patternIndex) {
        char buf[100];
        if (length(ref) > 1) {
            /* Define soft mask as well as pattern */
            int maskIndex = INTEGER(ref)[1];
            PDFwrite(buf, 100, 
                     "/Def%d gs /Pattern cs /Def%d scn\n", 
                     pd,
                     maskIndex,
                     patternIndex);
        } else {
            PDFwrite(buf, 100, 
                     "/Pattern cs /Def%d scn\n", 
                     pd,
                     patternIndex);
        }
        pd->current.patternfill = patternIndex;
    }
    /* Pattern fill set means fill not set */
    pd->current.fill = INVALID_COL;    
}

static void PDFSetLineEnd(PDFDesc *pd, R_GE_lineend lend)
{
    char buf[10];
    int lineend = 1; /* -Wall */
    switch (lend) {
    case GE_ROUND_CAP:
	lineend = 1;
	break;
    case GE_BUTT_CAP:
	lineend = 0;
	break;
    case GE_SQUARE_CAP:
	lineend = 2;
	break;
    default:
	error(_("invalid line end"));
    }
    PDFwrite(buf, 10, "%1d J\n", pd, lineend);
}

static void PDFSetLineJoin(PDFDesc *pd, R_GE_linejoin ljoin)
{
    char buf[10];
    int linejoin = 1; /* -Wall */
    switch (ljoin) {
    case GE_ROUND_JOIN:
	linejoin = 1;
	break;
    case GE_MITRE_JOIN:
	linejoin = 0;
	break;
    case GE_BEVEL_JOIN:
	linejoin = 2;
	break;
    default:
	error(_("invalid line join"));
    }
    PDFwrite(buf, 10, "%1d j\n", pd, linejoin);
}

/* Note that the line texture is scaled by the line width.*/
static void PDFSetLineTexture(PDFDesc *pd, const char *dashlist, int nlty,
			      double lwd, int lend)
{
    double dash[8], a = (lend == GE_BUTT_CAP) ? 0. : 1.;
    int i;
    bool allzero = true;
    char buf[10];
    for (i = 0; i < nlty; i++) {
	dash[i] = lwd *				
	    ((i % 2) ? (dashlist[i] + a)
	     : ((nlty == 1 && dashlist[i] == 1.) ? 1. : dashlist[i] - a) );
	if (dash[i] < 0) dash[i] = 0;
        if (dash[i] > .01) allzero = false;
    }
    PDFwrite(buf, 10, "[", pd);
    if (!allzero) {
        for (i = 0; i < nlty; i++) {
            PDFwrite(buf, 10," %.2f", pd, dash[i]);
        }
    }
    PDFwrite(buf, 10, "] 0 d\n", pd);
}

static void PDF_SetLineStyle(const pGEcontext gc, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char dashlist[8];
    int i;
    int newlty = gc->lty;
    double linewidth;
    double newlwd = gc->lwd; 
    R_GE_lineend newlend = gc->lend;
    R_GE_linejoin newljoin = gc->ljoin;
    double newlmitre = gc->lmitre;
    char buf[100];

    if (pd->current.lty != newlty || pd->current.lwd != newlwd ||
	pd->current.lend != newlend) {
	pd->current.lwd = newlwd;
	pd->current.lty = newlty;
        linewidth = newlwd * 0.75;
        /* Must not allow line width to be zero */
        if (linewidth < .01)
            linewidth = .01;
	PDFwrite(buf, 100, "%.2f w\n", pd, linewidth);
	/* process lty : */
	for(i = 0; i < 8 && newlty & 15 ; i++) {
	    dashlist[i] = newlty & 15;
	    newlty = newlty >> 4;
	}
	PDFSetLineTexture(pd, dashlist, i, newlwd * 0.75, newlend);
    }
    if (pd->current.lend != newlend) {
	pd->current.lend = newlend;
	PDFSetLineEnd(pd, newlend);
    }
    if (pd->current.ljoin != newljoin) {
	pd->current.ljoin = newljoin;
	PDFSetLineJoin(pd, newljoin);
    }
    if (pd->current.lmitre != newlmitre) {
	pd->current.lmitre = newlmitre;
	PDFwrite(buf, 100, "%.2f M\n", pd, newlmitre);
    }
}

static void PDF_Encodings(PDFDesc *pd)
{
    encodinglist enclist = pd->encodings;

    while (enclist) {
	encodinginfo encoding = enclist->encoding;
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);

	fprintf(pd->pdffp, "%d 0 obj\n<<\n/Type /Encoding ", pd->nobjs);
	if (strcmp(encoding->name, "WinAnsiEncoding") == 0 ||
	    strcmp(encoding->name, "PDFDocEncoding") == 0) {
	    fprintf(pd->pdffp, "/BaseEncoding /%s\n", encoding->name);
	    fprintf(pd->pdffp, "/Differences [ 45/minus ]\n");
	} else if (strcmp(encoding->name, "ISOLatin1Encoding") == 0) {
	    fprintf(pd->pdffp, "/BaseEncoding /WinAnsiEncoding\n");
	    fprintf(pd->pdffp, "/Differences [ 45/minus 96/quoteleft\n144/dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent\n/dieresis /.notdef /ring /cedilla /.notdef /hungarumlaut /ogonek /caron /space]\n");
	} else {
	    int enc_first;
	    int c = 0;
	    int len;
	    char buf[128];
	    for(enc_first=0;encoding->enccode[enc_first]!='['   &&
			    encoding->enccode[enc_first]!='\0' ;enc_first++);
	    if (enc_first >= strlen(encoding->enccode))
		enc_first=0;
	    fprintf(pd->pdffp, "/BaseEncoding /PDFDocEncoding\n");
	    fprintf(pd->pdffp, "/Differences [\n");
	    while(encoding->enccode[enc_first]) {
		switch (encoding->enccode[enc_first]) {
		  case ' ':
		  case '\t':
		  case '\n':
		  case '[':
		  case ']':
		    enc_first++;
		    continue;
		}
		for(len=0;
		    (encoding->enccode[enc_first+len]!=' ')   &&
		    (encoding->enccode[enc_first+len]!=']')   &&
		    (encoding->enccode[enc_first+len]!='\t')   &&
		    (encoding->enccode[enc_first+len]!='\0')   &&
		    (encoding->enccode[enc_first+len]!='\n') ;
		    len++);
		memcpy(buf,encoding->enccode + enc_first , len);
		buf[len]='\0';
		fprintf(pd->pdffp, " %d%s", c, buf);
		if ( (c+1) % 8 == 0 ) fprintf(pd->pdffp, "\n");
		c++;
		enc_first+=len;
	    }
	    fprintf(pd->pdffp, "\n]\n");
	}
	fprintf(pd->pdffp, ">>\nendobj\n");

	enclist = enclist->next;
    }
}

/* Read sRGB profile from icc/srgb.flate
 * HexCode original from
 * http://code.google.com/p/ghostscript/source/browse/trunk/gs/iccprofiles/srgb.icc
 */
#define BUFSIZE2 10000
static void PDFwritesRGBcolorspace(PDFDesc *pd) 
{
    char buf[BUFSIZE2];
    FILE *fp;

    snprintf(buf, BUFSIZE2, "%s%slibrary%sgrDevices%sicc%s%s",
             R_Home, FILESEP, FILESEP, FILESEP, FILESEP,
	     pd->useCompression ? "srgb.flate" : "srgb");
    if (!(fp = R_fopen(R_ExpandFileName(buf), "rb")))
        error(_("failed to load sRGB colorspace file"));
    size_t res = fread(buf, 1, BUFSIZE2, fp);
    res = fwrite(buf, 1, res, pd->pdffp);
    fclose(fp);
}

#include <time.h>  // for time_t, time, localtime
#include <Rversion.h>

static void PDF_startfile(PDFDesc *pd)
{
    struct tm *ltm;
    time_t ct;

    pd->nobjs = 0;
    pd->pageno = 0;
    /*
     * I destroy it when I open in Japanese environment carelessly
     */
    fprintf(pd->pdffp, "%%PDF-%i.%i\n%%\x81\xe2\x81\xe3\x81\xcf\x81\xd3\x5c\x72\n",
	    pd->versionMajor, pd->versionMinor);
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);

    /* Object 1 is Info node. Date format is from the PDF manual */

    ct = time(NULL);
    ltm = localtime(&ct);
    fprintf(pd->pdffp, "1 0 obj\n<<\n");
    if (pd->timestamp) {
	fprintf(pd->pdffp,
	    "/CreationDate (D:%04d%02d%02d%02d%02d%02d)\n",
	    1900 + ltm->tm_year, ltm->tm_mon+1, ltm->tm_mday,
	    ltm->tm_hour, ltm->tm_min, ltm->tm_sec);
	fprintf(pd->pdffp,
	    "/ModDate (D:%04d%02d%02d%02d%02d%02d)\n",
	    1900 + ltm->tm_year, ltm->tm_mon+1, ltm->tm_mday,
	    ltm->tm_hour, ltm->tm_min, ltm->tm_sec);
    }
    if (strlen(pd->title) > 0)
	fprintf(pd->pdffp, "/Title (%s)\n", pd->title);
    if (strlen(pd->author) > 0)
	fprintf(pd->pdffp, "/Author (%s)\n", pd->author);
    if (pd->producer)
	fprintf(pd->pdffp, "/Producer (R %s.%s)\n", R_MAJOR, R_MINOR);
    fprintf(pd->pdffp, "/Creator (R)\n>>\nendobj\n");

    /* Object 2 is the Catalog, pointing to pages list in object 3 (at end) */

    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "2 0 obj\n<< /Type /Catalog /Pages 3 0 R >>\nendobj\n");

    /* Objects at the end */
    pd->nobjs += 2;
    if (streql(pd->colormodel, "srgb")) pd->nobjs += 2;
}

static const char *Base14[] =
{
    "Courier", "Courier-Oblique", "Courier-Bold", "Courier-BoldOblique",
    "Helvetica", "Helvetica-Oblique", "Helvetica-Bold",
    "Helvetica-BoldOblique", "Symbol", "Times-Roman", "Times-Italic",
    "Times-Bold", "Times-BoldItalic", "ZapfDingbats"
};

static int isBase14(const char *name)
{
    int i;
    for(i = 0; i < 14; i++)
	if(strcmp(name, Base14[i]) == 0) return 1;
    return 0;
}

static const char *KnownSanSerif[] =
{
    "AvantGarde", "Helvetica-Narrow", "URWGothic", "NimbusSan"
};


static int isSans(const char *name)
{
    int i;
    for(i = 0; i < 4; i++)
	if(strncmp(name, KnownSanSerif[i], strlen(KnownSanSerif[i])) == 0)
	    return 1;
    return 0;
}

#define boldslant(x) ((x==3)?",BoldItalic":((x==2)?",Italic":((x==1)?",Bold":"")))

#if defined(BUFSIZ) && (BUFSIZ > 512)
/* OS's buffer size in stdio.h, probably.
   Windows has 512, Solaris 1024, glibc 8192
 */
# define APPENDBUFSIZE BUFSIZ
#else
# define APPENDBUFSIZE 512
#endif

/* Write out the resources for a page OR for a tiling pattern.
 * Return the number of objects in the dictionary
 */
static int PDFwriteResourceDictionary(int objOffset, bool endpage, 
                                      int excludeDef, PDFDesc *pd)
{
    char buf[100];
    int i, objCount, nenc, nfonts, cidnfonts, nraster, nmask;

    nraster = pd->numRasters;
    nmask = pd->numMasks;

    /* ProcSet is regarded as obsolete as from PDF 1.4 */
    if (nraster > 0) {
	if (nmask > 0) {
	    PDFwrite(buf, 
                     100,
                     "<<\n/ProcSet [/PDF /Text /ImageC /ImageB]\n/Font <<",
                     pd);

	} else {
	    PDFwrite(buf, 
                     100,
		    "<<\n/ProcSet [/PDF /Text /ImageC]\n/Font <<",
                     pd);
	}
    } else {
	/* fonts */
        PDFwrite(buf, 
                 100,
                 "<<\n/ProcSet [/PDF /Text]\n/Font <<",
                 pd);
    }

    /* Count how many encodings will be included:
     * fonts come after encodings */
    nenc = 0;
    if (pd->encodings) {
	encodinglist enclist = pd->encodings;
	while (enclist) {
	    nenc++;
	    enclist = enclist->next;
	}
    }
    /* Should be a default text font at least, plus possibly others */
    objCount = objOffset + nenc;

    /* Dingbats always F1 */
    if (pd->fontUsed[1]) 
        PDFwrite(buf, 100, " /F1 %d 0 R ", pd, ++objCount);

    nfonts = 2;
    if (pd->fonts) {
	type1fontlist fontlist = pd->fonts;
	while (fontlist) {
	    for (i = 0; i < 5; i++) {
		if(nfonts >= 100 || pd->fontUsed[nfonts]) {
                    PDFwrite(buf, 100, "/F%d %d 0 R ", pd, nfonts, ++objCount);
		    /* Allow for the font descriptor object, if present */
		    if(!isBase14(fontlist->family->fonts[i]->name)) objCount++;
		}
		nfonts++;
	    }
	    fontlist = fontlist->next;
	}
    }
    cidnfonts = 0;
    if (pd->cidfonts) {
	cidfontlist fontlist = pd->cidfonts;
	while (fontlist) {
	    for (i = 0; i < 5; i++) {
		PDFwrite(buf, 100, "/F%d %d 0 R ", pd,
			1000 + cidnfonts + 1, ++objCount);
		cidnfonts++;
	    }
	    fontlist = fontlist->next;
	}
    }

    /* Definitions start after ExtGState */
    int defnOffset = objCount + 1;
    for (i = 0; i < 256 && pd->colAlpha[i] >= 0; i++)
        ++defnOffset;
    for (i = 0; i < 256 && pd->fillAlpha[i] >= 0; i++)
        ++defnOffset;
    for (i = 0; i < PDFnumBlendModes; i++)
        if (pd->blendModes[i])
            ++defnOffset;
    if (nmask > 0)
        ++defnOffset;
    
    PDFwriteGlyphFontDefs(defnOffset, pd);

    PDFwrite(buf, 100, ">>\n", pd);

    /* graphics state parameter dictionaries */
    PDFwrite(buf, 100, "/ExtGState << ", pd);
    for (i = 0; i < 256 && pd->colAlpha[i] >= 0; i++)
	PDFwrite(buf, 100, "/GS%i %d 0 R ", pd, i + 1, ++objCount);
    for (i = 0; i < 256 && pd->fillAlpha[i] >= 0; i++)
	PDFwrite(buf, 100, "/GS%i %d 0 R ", pd, i + 257, ++objCount);
    for (i = 0; i < PDFnumBlendModes; i++)
        if (pd->blendModes[i])
            PDFwrite(buf, 100, "/bm%i %d 0 R ", pd, i, ++objCount);
    /* Special state to set AIS if we have soft masks */
    if (nmask > 0)
	PDFwrite(buf, 100, "/GSais %d 0 R ", pd, ++objCount);
    /* Soft mask definitions */
    if (pd->numDefns > 0) {
        PDFwriteSoftMaskDefs(defnOffset, pd);
    }    
    PDFwrite(buf, 100, ">>\n", pd);

    /* Map resource names to XObjects */
    if (nraster > 0 || pd->numDefns > 0) {
	PDFwrite(buf, 100, "/XObject <<\n", pd);

	/* image XObjects */
        int start = 0;
        if (endpage) 
            start = pd->fileRasters;
	for (i = start; i < nraster; i++) {
	    PDFwrite(buf, 100, "  /Im%d %d 0 R\n", pd,
                     i, pd->rasters[i].nobj);
		if (pd->masks[i] >= 0)
		    PDFwrite(buf, 100, "  /Mask%d %d 0 R\n", pd,
                             pd->masks[i], pd->rasters[i].nmaskobj);
	}

        /* Group XObjects */
        PDFwriteGroupDefs(defnOffset, pd);

	PDFwrite(buf, 100, ">>\n", pd);
        if (endpage) {
            pd->fileRasters = nraster;
        }
    }

    /* patterns */
    if (pd->numDefns > 0) {
        PDFwritePatternDefs(defnOffset, excludeDef, pd);
    }

    if (streql(pd->colormodel, "srgb")) {
	/* Ojects 5 and 6 are the sRGB color space, if required */
	PDFwrite(buf, 100, "/ColorSpace << /sRGB 5 0 R >>\n", pd);
    }
    PDFwrite(buf, 100, ">>\n", pd);

    return objCount;
}

static void PDF_endfile(PDFDesc *pd)
{
    int i, startxref, tempnobj, nfonts, cidnfonts, firstencobj;
    int nraster, nmask, npattern;

    /* object 3 lists all the pages */

    pd->pos[3] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "3 0 obj\n<< /Type /Pages /Kids [ ");
    for(i = 0; i < pd->pageno; i++)
	fprintf(pd->pdffp, "%d 0 R ", pd->pageobj[i]);

    fprintf(pd->pdffp,
	    "] /Count %d /MediaBox [0 0 %d %d] >>\nendobj\n",
	    pd->pageno,
	    (int) (0.5 + pd->paperwidth), (int) (0.5 + pd->paperheight));

    /* Object 4 is the standard resources dict for each page */

    /* Count how many images and masks and patterns */
    nraster = pd->numRasters;
    nmask = pd->numMasks;
    npattern = countPatterns(pd);

    if(pd->nobjs + nraster + nmask + npattern + 500 >= pd->max_nobjs) {
	int new =  pd->nobjs + nraster + nmask + npattern + 500;
	void *tmp = realloc(pd->pos, new * sizeof(int));
	if(!tmp)
	    error("unable to increase object limit: please shutdown the pdf device");
	pd->pos = (int *) tmp;
	pd->max_nobjs = new;
    }

    int resourceDictOffset = pd->nobjs;
    pd->pos[4] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "4 0 obj\n");
    /* The resource dictionary for the page */
    tempnobj = PDFwriteResourceDictionary(resourceDictOffset, true, -1, pd);
    fprintf(pd->pdffp, "endobj\n");

    if (streql(pd->colormodel, "srgb")) {
	pd->pos[5] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "5 0 obj\n[/ICCBased 6 0 R]\nendobj\n");
	pd->pos[6] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "6 0 obj\n");
	PDFwritesRGBcolorspace(pd);    
	fprintf(pd->pdffp, "endobj\n");
    }

    if(tempnobj >= pd->max_nobjs) {
	int new = tempnobj + 500;
	void *tmp = realloc(pd->pos, new * sizeof(int));
	if(!tmp)
	    error("unable to increase object limit: please shutdown the pdf device");
	pd->pos = (int *) tmp;
	pd->max_nobjs = new;
    }

   /*
     * Write out objects representing the encodings
     */

    firstencobj = pd->nobjs;
    PDF_Encodings(pd);

    /*
     * Write out objects representing the fonts
     */

    if (pd->fontUsed[1]) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "%d 0 obj\n<< /Type /Font /Subtype /Type1 /Name /F1 /BaseFont /ZapfDingbats >>\nendobj\n", pd->nobjs);
    }


    nfonts = 2;
    if (pd->fonts) {
	type1fontlist fontlist = pd->fonts;
	while (fontlist) {
	    FontMetricInfo *metrics;
	    /*
	     * Find the index of the device encoding
	     * This really should be there
	     */
	    int encIndex;
	    encodinginfo encoding =
		findDeviceEncoding(fontlist->family->encoding->encpath,
				   pd->encodings, &encIndex);
	    if (!encoding)
		error(_("corrupt encodings in PDF device"));
	    for (i = 0; i < 5; i++) {
		if (nfonts >= 100 || pd->fontUsed[nfonts]) {
		    type1fontinfo fn = fontlist->family->fonts[i];
		    int base = isBase14(fn->name);
		    metrics = &fn->metrics;
		    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
		    fprintf(pd->pdffp, "%d 0 obj\n<< /Type /Font /Subtype /Type1 /Name /F%d /BaseFont /%s\n",
			    pd->nobjs,
			    nfonts,
			    fn->name);
		    if (!base) {
			int ii, first, last, tmp;
			for(first = 1, ii = 0; ii < 255; ii++)
			    if(metrics->CharInfo[ii].WX != NA_SHORT) {
				first = ii;
				break;
			    }
			for(last = 255, ii = 254; ii >= 0; ii--)
			    if(metrics->CharInfo[ii].WX != NA_SHORT) {
				last = ii + 1;
				break;
			    }
			fprintf(pd->pdffp,
				"/FirstChar %d /LastChar %d /Widths [\n",
				first, last);
			for (ii = first; ii <= last; ii++) {
			    tmp = metrics->CharInfo[ii].WX;
			    fprintf(pd->pdffp, " %d", tmp==NA_SHORT ? 0 : tmp);
			    if ((ii + 1) % 15 == 0) fprintf(pd->pdffp, "\n");
			}
			fprintf(pd->pdffp, "]\n");
			fprintf(pd->pdffp, "/FontDescriptor %d 0 R\n",
				pd->nobjs + 1);
		    }
		    if(i < 4)
			fprintf(pd->pdffp, "/Encoding %d 0 R ",
				/* Encodings come after dingbats font which is
				 * object 5 */
				encIndex + firstencobj);
		    fprintf(pd->pdffp, ">>\nendobj\n");
		    if(!base) {
			/* write font descriptor */
			int flags = 32 /*bit 6, non-symbolic*/ +
			    ((i==2 || i==3) ? 64/* italic */: 0) +
			    (metrics->IsFixedPitch > 0 ? 1 : 0) +
			    (isSans(fn->name) ? 0 : 2);
			/* <FIXME> we have no real way to know
			   if this is serif or not */
			pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
			fprintf(pd->pdffp,
				"%d 0 obj <<\n"
				" /Type /FontDescriptor\n"
				" /FontName /%s\n"
				" /Flags %d\n"
				" /FontBBox [%d %d %d %d]\n"
				" /CapHeight %d\n /Ascent %d\n /Descent %d\n"
				" /ItalicAngle %d\n /XHeight %d\n /StemV %d\n"
				">>\nendobj\n",
				pd->nobjs,
				fn->name,
				(i == 4) ? 4 : flags,
				metrics->FontBBox[0], metrics->FontBBox[1],
				metrics->FontBBox[2], metrics->FontBBox[3],
				metrics->CapHeight, metrics->Ascender,
				metrics->Descender,
				metrics->ItalicAngle, metrics->XHeight,
				(metrics->StemV != NA_SHORT) ? metrics->StemV :
				(i==2 || i==3) ? 140 : 83);
		    }
		}
		nfonts++;
	    }
	    fontlist = fontlist->next;
	}
    }
    cidnfonts = 0;
    if (pd->cidfonts) {
	cidfontlist fontlist = pd->cidfonts;
	if(pd->versionMajor == 1 && pd->versionMinor < 3) {
	    pd->versionMinor  = 3;
	    warning(_("increasing the PDF version to 1.3"));
	}
	while (fontlist) {
	    for (i = 0; i < 4; i++) {
		pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
		fprintf(pd->pdffp,
			/** format **/
			"%d 0 obj\n"
			"<<\n"
			"  /Type /Font\n"
			"  /Subtype /Type0\n"
			"  /Name /F%d\n"
			"  /BaseFont /%s%s\n"
			"  /DescendantFonts [\n"
			"    <<\n"
			"      /Type /Font\n"
			"      /Subtype /CIDFontType0\n"
			"      /BaseFont /%s%s\n"
			"      %s"
			"    >>\n"
			"  ]\n"
			"  /Encoding /%s\n"
			">>\n"
			"endobj\n",
			/** vararg **/
			pd->nobjs,                          /* pdf objnum  */
			1000 + cidnfonts + 1,               /* - face      */
			fontlist->cidfamily->cidfonts[i]->name,/* /BaseFont*/
			boldslant(i),                       /* - boldslant */
			fontlist->cidfamily->cidfonts[i]->name,/* /BaseFont*/
			boldslant(i),                       /* - boldslant */
							    /* Resource    */
			/*
			 * Pull the resource out of R object
			 * Hopefully one day this will be unnecessary
			 */
			getCIDFontPDFResource(fontlist->cidfamily->fxname),
			fontlist->cidfamily->cmap           /* /Encoding   */
			);
		cidnfonts++;
	    }
	    /* Symbol face does not use encoding */
	    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	    fprintf(pd->pdffp, "%d 0 obj\n<<\n/Type /Font\n/Subtype /Type1\n/Name /F%d\n/BaseFont /%s\n>>\nendobj\n",
		    pd->nobjs,
		    1000 + cidnfonts + 1,
		    fontlist->cidfamily->symfont->name);
	    cidnfonts++;
	    fontlist = fontlist->next;
	}
    }

    /*
     * Write out objects representing the graphics state parameter
     * dictionaries for alpha transparency
     */
    for (i = 0; i < 256 && pd->colAlpha[i] >= 0; i++) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp,
		"%d 0 obj\n<<\n/Type /ExtGState\n/CA %1.3f >>\nendobj\n",
		pd->nobjs, pd->colAlpha[i]/255.0);
    }
    for (i = 0; i < 256 && pd->fillAlpha[i] >= 0; i++) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp,
		"%d 0 obj\n<<\n/Type /ExtGState\n/ca %1.3f\n>>\nendobj\n",
		pd->nobjs, pd->fillAlpha[i]/255.0);
    }
    /* graphics state parameter dictionaries for (used) blend modes */
    for (i = 0; i < PDFnumBlendModes; i++) {
        if (pd->blendModes[i]) {
            pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
            fprintf(pd->pdffp,
                    "%d 0 obj\n<<\n/Type /ExtGState\n/BM /%s\n>>\nendobj\n",
                    pd->nobjs, PDFblendModes[i]);
        }
    }

    if (nmask > 0) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp,
		"%d 0 obj\n<<\n/Type /ExtGState\n/AIS false\n>>\nendobj\n",
		pd->nobjs);
    }

    /* Write out definitions */
    PDFwriteDefinitions(resourceDictOffset, pd);

    /* write out xref table */

    startxref = (int) ftell(pd->pdffp);
    /* items here must be exactly 20 bytes including terminator */
    fprintf(pd->pdffp, "xref\n0 %d\n", pd->nobjs+1);
    fprintf(pd->pdffp, "0000000000 65535 f \n");
    for(i = 1; i <= pd->nobjs; i++) {
	fprintf(pd->pdffp, "%010d 00000 n \n", pd->pos[i]);
    }
    fprintf(pd->pdffp,
	    "trailer\n<< /Size %d /Info 1 0 R /Root 2 0 R >>\nstartxref\n%d\n",
	    pd->nobjs+1, startxref);
    fprintf(pd->pdffp, "%%%%EOF\n");

    /* now seek back and update the header */
    rewind(pd->pdffp);
    fprintf(pd->pdffp, "%%PDF-%i.%i\n", pd->versionMajor, pd->versionMinor);
    fclose(pd->pdffp);
    if (pd->open_type == 1) {
	char buf[APPENDBUFSIZE];
	size_t nc;
	pd->pdffp = R_fopen(pd->filename, "rb"); 
	while((nc = fread(buf, 1, APPENDBUFSIZE, pd->pdffp))) {
	    if(nc != fwrite(buf, 1, nc, pd->pipefp))
		error("write error");
	    if (nc < APPENDBUFSIZE) break;
	}
	fclose(pd->pdffp);
	pclose(pd->pipefp);
	unlink(pd->filename);
    }
}


static void PDF_Open(pDevDesc dd, PDFDesc *pd)
{
    char buf[512];

    if (pd->offline)
        return;
    
    if (pd->filename[0] == '|') {
	strncpy(pd->cmd, pd->filename + 1, R_PATH_MAX - 1);
	pd->cmd[R_PATH_MAX - 1] = '\0';
	char *tmp = R_tmpnam("Rpdf", R_TempDir);
	strncpy(pd->filename, tmp, R_PATH_MAX - 1);
	pd->filename[R_PATH_MAX - 1] = '\0';
	free(tmp);
	errno = 0;
	pd->pipefp = R_popen(pd->cmd, "w");
	if (!pd->pipefp || errno != 0) {
	    char errbuf[strlen(pd->cmd) + 1];
	    strcpy(errbuf, pd->cmd);
	    PDFcleanup(7, pd);
	    error(_("cannot open 'pdf' pipe to '%s'"), errbuf);
	    return;
	}
	pd->open_type = 1;
	if (!pd->onefile) {
	    pd->onefile = TRUE;
	    warning(_("file = \"|cmd\" implies 'onefile = TRUE'"));
	}
    } else pd->open_type = 0;
    snprintf(buf, 512, pd->filename, pd->fileno + 1); /* file 1 to start */
    /* NB: this must be binary to get tell positions and line endings right,
       as well as allowing binary streams */
    pd->mainfp = R_fopen(R_ExpandFileName(buf), "wb");
    if (!pd->mainfp) {
	PDFcleanup(7, pd);
	free(dd);	
	error(_("cannot open file '%s'"), buf);
    }
    pd->pdffp = pd->mainfp;

    PDF_startfile(pd);
    return;
}

static void pdfClip(double x0, double x1, double y0, double y1, PDFDesc *pd)
{
    char buf[100];
    if(x0 != 0.0 || y0 != 0.0 || x1 != 72*pd->width || y1 != 72*pd->height)
	PDFwrite(buf, 100, "Q q %.2f %.2f %.2f %.2f re W n\n", pd,
                 x0, y0, x1 - x0, y1 - y0);
    else PDFwrite(buf, 100, "Q q\n", pd);
}

static void PDF_Clip(double x0, double x1, double y0, double y1, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_checkOffline();

    if(pd->inText) textoff(pd);
    pdfClip(x0, x1, y0, y1, pd);
    PDF_Invalidate(pd);
}

static void PDF_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd)
{
    *left = dd->left;
    *right = dd->right;
    *bottom = dd->bottom;
    *top = dd->top;
}

static void PDF_endpage(PDFDesc *pd)
{
    if(pd->inText) textoff(pd);
    fprintf(pd->pdffp, "Q\n");
    if (pd->useCompression) {
	fflush(pd->pdffp);
	fseek(pd->pdffp, 0, SEEK_END);
	unsigned int len = (unsigned int) ftell(pd->pdffp);
	fseek(pd->pdffp, 0, SEEK_SET);
	Bytef *buf = R_Calloc(len, Bytef);
	uLong outlen = (uLong)(1.001*len + 20);
	Bytef *buf2 = R_Calloc(outlen, Bytef);
	size_t res = fread(buf, 1, len, pd->pdffp);
	if (res < len) error("internal read error in PDF_endpage");
	fclose(pd->pdffp);
	unlink(pd->tmpname);
	pd->pdffp = pd->mainfp;
	int res2 = compress(buf2, &outlen, buf, len);
	if(res2 != Z_OK) 
	    error("internal compression error %d in PDF_endpage", res2);
	fprintf(pd->pdffp, "%d 0 obj\n<<\n/Length %d /Filter /FlateDecode\n>>\nstream\n", 
		pd->nobjs, (int) outlen);
	size_t nwrite = fwrite(buf2, 1, outlen, pd->pdffp);
	if(nwrite != outlen) error(_("write failed"));
	R_Free(buf); R_Free(buf2);
	fprintf(pd->pdffp, "endstream\nendobj\n");
    } else {
	int here = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "endstream\nendobj\n");
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "%d 0 obj\n%d\nendobj\n", pd->nobjs,
		here - pd->startstream);
    }

    if(pd->nobjs + 2*(pd->numRasters - pd->writtenRasters) + 500 
       >= pd->max_nobjs) {
	int new =  pd->nobjs + 2*(pd->numRasters - pd->writtenRasters) + 2000;
	void *tmp = realloc(pd->pos, new * sizeof(int));
	if(!tmp)
	    error("unable to increase object limit: please shutdown the pdf device");
	pd->pos = (int *) tmp;
	pd->max_nobjs = new;
    }

    /* Write out any new rasters */
    for (int i = pd->writtenRasters; i < pd->numRasters; i++) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	pd->rasters[i].nobj = pd->nobjs;
	writeRasterXObject(pd->rasters[i], pd->nobjs,
			   pd->masks[i], pd->nobjs+1, pd);
 	if (pd->masks[i] >= 0) {
	    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	    pd->rasters[i].nmaskobj = pd->nobjs;
	    writeMaskXObject(pd->rasters[i], pd->nobjs, pd);
	}
	free(pd->rasters[i].raster);
	pd->rasters[i].raster = NULL;
	pd->writtenRasters = pd->numRasters;
    }
}

#define R_VIS(col) (R_ALPHA(col) > 0)

static void PDF_NewPage(const pGEcontext gc,
			pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char buf[512];

    PDF_checkOffline();

    if(pd->pageno >= pd->pagemax) {
	void * tmp = realloc(pd->pageobj, 2*pd->pagemax * sizeof(int));
	if(!tmp)
	    error("unable to increase page limit: please shutdown the pdf device");
	pd->pageobj = (int *) tmp;
	pd->pagemax *= 2;
    }
    if(pd->nobjs + 500 >= pd->max_nobjs) {
	int new = pd->max_nobjs + 2000;
	void *tmp = realloc(pd->pos, new * sizeof(int));
	if(!tmp)
	    error("unable to increase object limit: please shutdown the pdf device");
	pd->pos = (int *) tmp;
	pd->max_nobjs = new;
    }


    if(pd->pageno > 0) {
	PDF_endpage(pd);
	if(!pd->onefile) {
	    PDF_endfile(pd);
	    pd->fileno++;
	    snprintf(buf, 512, pd->filename, pd->fileno + 1); /* file 1 to start */
	    pd->mainfp = R_fopen(R_ExpandFileName(buf), "wb");
	    if (!pd->mainfp)
		error(_("cannot open 'pdf' file argument '%s'\n  please shut down the PDF device"), buf);
	    pd->pdffp = pd->mainfp;
            resetDefinitions(pd);
            pd->numGlyphFonts = 0;
	    PDF_startfile(pd);
	}
    }

    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    pd->pageobj[pd->pageno++] = pd->nobjs;
    fprintf(pd->pdffp, "%d 0 obj\n<< /Type /Page /Parent 3 0 R /Contents %d 0 R /Resources 4 0 R >>\nendobj\n",
	    pd->nobjs, pd->nobjs+1);
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    if (pd->useCompression) {
	char *tmp = R_tmpnam("pdf", R_TempDir);
	/* assume tmpname is less than R_PATH_MAX */
	strcpy(pd->tmpname, tmp);
	pd->pdffp = fopen(tmp, "w+b");
	if (! pd->pdffp) {
            pd->pdffp = pd->mainfp;
            pd->useCompression = 0;
            warning(_("Cannot open temporary file '%s' for compression (reason: %s); compression has been turned off for this device"), 
                    tmp, strerror(errno));
        }
	free(tmp);
    } 
    /* May have turned compression off in previous block */
    if (!pd->useCompression) {
	fprintf(pd->pdffp, "%d 0 obj\n<<\n/Length %d 0 R\n>>\nstream\n",
		pd->nobjs, pd->nobjs + 1);
	pd->startstream = (int) ftell(pd->pdffp);
    }

    /*
     * Line end/join/mitre now controlled by user
     * Same old defaults
     * .. but they are still needed because SetXXX produces the corresponding
     * command only if the value changes - so we have to define base defaults
     * according to the values reset by Invalidate. I'm pretty sure about j/J
     * but not so about M because Invalidate uses 0 yet the default used to be
     * 10.
     *
     * fprintf(pd->pdffp, "1 J 1 j 10 M q\n");
     */
    fprintf(pd->pdffp, "1 J 1 j q\n");
    PDF_Invalidate(pd);
    pd->appendingPath = -1;
    pd->pathContainsText = FALSE;
    pd->pathContainsDrawing = FALSE;
    pd->appendingMask = -1;
    pd->currentMask = -1;
    pd->appendingPattern = -1;
    if(R_VIS(gc->fill)) {
	PDF_SetFill(gc->fill, dd);
	fprintf(pd->pdffp, "0 0 %.2f %.2f re f\n",
		72.0 * pd->width, 72.0 * pd->height);
    }
    pd->inText = FALSE;
}

static void PDF_Close(pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    if (!pd->offline) {
        if(pd->pageno > 0) PDF_endpage(pd);
        PDF_endfile(pd);
        /* may no longer be needed */
        killRasterArray(pd->rasters, pd->maxRasters);
    }
    PDFcleanup(7, pd); /* which frees masks and rasters */
}

static void PDF_Rect(double x0, double y0, double x1, double y1,
		     const pGEcontext gc,
		     pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int code;
    char buf[100];

    PDF_checkOffline();

    if (appendingPathWithText(pd)) return;

    if (gc->patternFill != R_NilValue) { 
        if (R_VIS(gc->col)) {
            code = 3;
        } else {
            code = 2;
        }
    } else {            
        code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
    }
    if (code) {
        if(pd->inText) textoff(pd);
        /*
         * IF appending a clip path:
         *    Do NOT set graphical parameters
         *    Do NOT stroke or fill
         *
         * IF there is a pattern fill, use that instead of fill
         * 
         * IF there is a mask apply that
         *
         * PDFwrite writes to ...
         *    clip path (if appending a clip path)
         *    mask (if appending a mask)
         *    file (otherwise)
         */
        if (pd->appendingPath < 0) {
            if (gc->patternFill != R_NilValue) { 
                PDF_SetPatternFill(gc->patternFill, dd);
            } else if(code & 2) {
                PDF_SetFill(gc->fill, dd);
            }
            if(code & 1) {
                PDF_SetLineColor(gc->col, dd);
                PDF_SetLineStyle(gc, dd);
            }
        }
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        PDFwrite(buf, 100, "%.2f %.2f %.2f %.2f re\n", pd, x0, y0, x1-x0, y1-y0);
        if (pd->appendingPath < 0) {
            switch(code) {
            case 1: PDFwrite(buf, 100, " S\n", pd); break;
            case 2: PDFwrite(buf, 100, " f\n", pd); break;
            case 3: PDFwrite(buf, 100, " B\n", pd); break;
            }
        } else {
            pd->pathContainsDrawing = TRUE;
        }
    }
}

#ifdef SIMPLE_RASTER
/* Maybe reincoporate this simpler approach as an alternative
 * (for opaque raster images) because it has the advantage of
 * NOT keeping the raster in memory until the PDF file is complete
 */
static void PDF_Raster(unsigned int *raster,
		       int w, int h,
		       double x, double y,
		       double width, double height,
		       double rot, Rboolean interpolate,
		       const pGEcontext gc, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    double angle, cosa, sina;

    PDF_checkOffline();

    /* This takes the simple approach of creating an inline
     * image.  This is not recommended for larger images
     * because it makes more work for the PDF viewer.
     * It also does not allow for semitransparent images.
     */
    if(pd->inText) textoff(pd);
    /* Save graphics state */
    fprintf(pd->pdffp, "q\n");
    /* translate */
    fprintf(pd->pdffp,
	    "1 0 0 1 %.2f %.2f cm\n",
	    x, y);
    /* rotate */
    angle = rot*M_PI/180;
    cosa = cos(angle);
    sina = sin(angle);
    fprintf(pd->pdffp,
	    "%.2f %.2f %.2f %.2f 0 0 cm\n",
	    cosa, sina, -sina, cosa);
    /* scale */
    fprintf(pd->pdffp,
	    "%.2f 0 0 %.2f 0 0 cm\n",
	    width, height);
    /* Begin image */
    fprintf(pd->pdffp, "BI\n");
    /* Image characteristics */
    /* Use ASCIIHexDecode filter for now, just because
     * it's easier to implement */
    fprintf(pd->pdffp,
	    "  /W %d\n  /H %d\n  /CS /RGB\n  /BPC 8\n  /F [/AHx]\n",
	    w, h);
    if (interpolate) {
	fprintf(pd->pdffp, "  /I true\n");
    }
    /* Begin image data */
    fprintf(pd->pdffp, "ID\n");
    /* The image stream */
    PDF_imagedata(raster, w, h, pd);
    /* End image */
    fprintf(pd->pdffp, "EI\n");
    /* Restore graphics state */
    fprintf(pd->pdffp, "Q\n");
}
#else

static void PDF_Raster(unsigned int *raster,
		       int w, int h,
		       double x, double y,
		       double width, double height,
		       double rot, Rboolean interpolate,
		       const pGEcontext gc, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    double angle, cosa, sina;
    int alpha;
    char buf[100];

    PDF_checkOffline();

    /* A raster image adds nothing to a (clipping) path */
    if (pd->appendingPath >= 0) 
        return;

    /* Record the raster so can write it out when page is finished */
    alpha = addRaster(raster, w, h, interpolate, pd);

    if(pd->inText) textoff(pd);
    /* Save graphics state */
    PDFwrite(buf, 100, "q\n", pd);
    if (pd->currentMask >= 0) {
        PDFwriteMask(pd->currentMask, pd);
    }
    /* Need to set AIS graphics state parameter ? */
    if (alpha) 
        PDFwrite(buf, 100, "/GSais gs\n", pd);
    /* translate */
    PDFwrite(buf, 100,  "1 0 0 1 %.2f %.2f cm\n", pd, x, y);
    /* rotate */
    angle = rot*M_PI/180;
    cosa = cos(angle);
    sina = sin(angle);
    PDFwrite(buf, 100, "%.2f %.2f %.2f %.2f 0 0 cm\n", pd, 
             cosa, sina, -sina, cosa);
    /* scale */
    PDFwrite(buf, 100, "%.2f 0 0 %.2f 0 0 cm\n", pd, width, height);
    /* Refer to XObject which will be written to file when page is finished */
    PDFwrite(buf, 100, "/Im%d Do\n", pd, pd->numRasters - 1);
    /* Restore graphics state */
    PDFwrite(buf, 100, "Q\n", pd);
}

#endif

/* r is in device coords */
static void PDF_Circle(double x, double y, double r,
		       const pGEcontext gc,
		       pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int code, tr;
    double xx, yy, a;
    char buf[100];

    PDF_checkOffline();

    if (r <= 0.0) return;  /* since PR#14797 use 0-sized pch=1, but now
			      GECircle omits such circles */

    if (appendingPathWithText(pd)) return;

    if (gc->patternFill != R_NilValue) { 
        if (R_VIS(gc->col)) {
            code = 3;
        } else {
            code = 2;
        }
    } else {            
        code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
    }
    if (pd->appendingPath < 0) {
        if (gc->patternFill != R_NilValue) { 
            PDF_SetPatternFill(gc->patternFill, dd);
        } else if(code & 2) {
            PDF_SetFill(gc->fill, dd);
        }
        if(code & 1) {
            PDF_SetLineColor(gc->col, dd);
            PDF_SetLineStyle(gc, dd);
        }
    }
    if (pd->currentMask >= 0) {
        PDFwriteMask(pd->currentMask, pd);
    }
    if (code) {
        if (semiTransparent(gc->col) || semiTransparent(gc->fill)
            || r > 10  || !pd->dingbats) {
            /*
             * Due to possible bug in Acrobat Reader for rendering
             * semi-transparent text, only ever draw Bezier curves
             * regardless of circle size.  Otherwise use font up to 20pt
             */
            {
                /* Use four Bezier curves, hand-fitted to quadrants */
                double s = 0.55 * r;
                if(pd->inText) textoff(pd);
                PDFwrite(buf, 100, "  %.2f %.2f m\n", pd, x - r, y);
                PDFwrite(buf, 100, 
                         "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", pd,
                         x - r, y + s, x - s, y + r, x, y + r);
                PDFwrite(buf, 100, 
                         "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", pd,
                         x + s, y + r, x + r, y + s, x + r, y);
                PDFwrite(buf, 100, 
                         "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", pd,
                         x + r, y - s, x + s, y - r, x, y - r);
                PDFwrite(buf, 100, 
                         "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", pd,
                         x - s, y - r, x - r, y - s, x - r, y);
                if (pd->appendingPath < 0) {
                    switch(code) {
                    case 1: PDFwrite(buf, 100, "S\n", pd); break;
                    case 2: PDFwrite(buf, 100, "f\n", pd); break;
                    case 3: PDFwrite(buf, 100, "B\n", pd); break;
                    }
                } else {
                    pd->pathContainsDrawing = TRUE;
                }
            }
        } else {
            pd->fontUsed[1] = TRUE;
            /* Use char 108 in Dingbats, which is a solid disc
               afm is C 108 ; WX 791 ; N a71 ; B 35 -14 757 708 ;
               so diameter = 0.722 * size
               centre = (0.396, 0.347) * size
            */
            a = 2./0.722 * r;
            if (a < 0.01) return; // avoid 0 dims below.
            xx = x - 0.396*a;
            yy = y - 0.347*a;
            if (pd->appendingPath >= 0) {
                tr = 7;
            } else {
                tr = (R_OPAQUE(gc->fill)) +
                    2 * (R_OPAQUE(gc->col)) - 1;
            }
            if(!pd->inText) texton(pd);
            PDFwrite(buf, 100,
                     "/F1 1 Tf %d Tr %.2f 0 0 %.2f %.2f %.2f Tm", pd,
                     tr, a, a, xx, yy);
            PDFwrite(buf, 100, " (l) Tj 0 Tr\n", pd);
            textoff(pd); /* added in 2.8.0 */
            if (pd->appendingPath >= 0) {
                pd->pathContainsText = TRUE;
                pd->pathContainsDrawing = TRUE;
            }
        }
    }
}

static void PDF_Line(double x1, double y1, double x2, double y2,
		     const pGEcontext gc,
		     pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char buf[100];

    PDF_checkOffline();

    if(!R_VIS(gc->col)) return;

    if (appendingPathWithText(pd)) return;

    if (pd->appendingPath < 0) {
        PDF_SetLineColor(gc->col, dd);
        PDF_SetLineStyle(gc, dd);
    }
    if (pd->currentMask >= 0) {
        PDFwriteMask(pd->currentMask, pd);
    }

    if(pd->inText) textoff(pd);
    PDFwrite(buf, 100, "%.2f %.2f m %.2f %.2f l ", pd, x1, y1, x2, y2);

    if (pd->appendingPath < 0) {
        PDFwrite(buf, 100, " S\n", pd);
    } else {
        pd->pathContainsDrawing = TRUE;
    }
}

static void PDF_Polygon(int n, double *x, double *y,
			const pGEcontext gc,
			pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    double xx, yy;
    int i, code;
    char buf[100];

    PDF_checkOffline();

    if (appendingPathWithText(pd)) return;

    if (gc->patternFill != R_NilValue) { 
        if (R_VIS(gc->col)) {
            code = 3;
        } else {
            code = 2;
        }
    } else {            
        code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
    }
    if (code) {
        if(pd->inText) textoff(pd);
        if (pd->appendingPath < 0) {
            if (gc->patternFill != R_NilValue) { 
                PDF_SetPatternFill(gc->patternFill, dd);
            } else if(code & 2) {
                PDF_SetFill(gc->fill, dd);
            }
            if(code & 1) {
                PDF_SetLineColor(gc->col, dd);
                PDF_SetLineStyle(gc, dd);
            }
        }
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        xx = x[0];
        yy = y[0];
        PDFwrite(buf, 100, "%.2f %.2f m\n", pd, xx, yy);
        for(i = 1 ; i < n ; i++) {
            xx = x[i];
            yy = y[i];
            PDFwrite(buf, 100, "%.2f %.2f l\n", pd, xx, yy);
        }
        PDFwrite(buf, 100, "h ", pd, xx, yy);
        if (pd->appendingPath < 0) {
            if (pd->fillOddEven) {
                switch(code) {
                case 1: PDFwrite(buf, 100, "S\n", pd); break;
                case 2: PDFwrite(buf, 100, "f*\n", pd); break;
                case 3: PDFwrite(buf, 100, "B*\n", pd); break;
                }
            } else {
                switch(code) {
                case 1: PDFwrite(buf, 100, "S\n", pd); break;
                case 2: PDFwrite(buf, 100, "f\n", pd); break;
                case 3: PDFwrite(buf, 100, "B\n", pd); break;
                }
            }
        } else {
            pd->pathContainsDrawing = TRUE;
        }
    }
}

static void PDF_Path(double *x, double *y,
                     int npoly, int *nper,
                     Rboolean winding,
                     const pGEcontext gc,
                     pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    double xx, yy;
    int i, j, index, code;
    char buf[100];

    PDF_checkOffline();

    if (appendingPathWithText(pd)) return;

    if (gc->patternFill != R_NilValue) { 
        if (R_VIS(gc->col)) {
            code = 3;
        } else {
            code = 2;
        }
    } else {            
        code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
    }
    if (code) {
        if(pd->inText) textoff(pd);
        if (pd->appendingPath < 0) {
            if (gc->patternFill != R_NilValue) { 
                PDF_SetPatternFill(gc->patternFill, dd);
            } else if(code & 2) {
                PDF_SetFill(gc->fill, dd);
            }
            if(code & 1) {
                PDF_SetLineColor(gc->col, dd);
                PDF_SetLineStyle(gc, dd);
            }
        }
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        index = 0;
        for (i=0; i < npoly; i++) {
            xx = x[index];
            yy = y[index];
            index++;
            PDFwrite(buf, 100, "%.2f %.2f m\n", pd, xx, yy);
            for(j=1; j < nper[i]; j++) {
                xx = x[index];
                yy = y[index];
                index++;
                PDFwrite(buf, 100, "%.2f %.2f l\n", pd, xx, yy);
            }
            if (i < npoly - 1)
                PDFwrite(buf, 100, "h\n", pd);
        }
        PDFwrite(buf, 100, "h\n", pd);
        if (pd->appendingPath < 0) {
            if (winding) {
            switch(code) {
                case 1: PDFwrite(buf, 100, "S\n", pd); break;
                case 2: PDFwrite(buf, 100, "f\n", pd); break;
                case 3: PDFwrite(buf, 100, "B\n", pd); break;
                }
            } else {
                switch(code) {
                case 1: PDFwrite(buf, 100, "S\n", pd); break;
                case 2: PDFwrite(buf, 100, "f*\n", pd); break;
                case 3: PDFwrite(buf, 100, "B*\n", pd); break;
                }
            }
        } else {
            pd->pathContainsDrawing = TRUE;
        }
    }
}

static void PDF_Polyline(int n, double *x, double *y,
			 const pGEcontext gc,
			 pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc*) dd->deviceSpecific;
    double xx, yy;
    int i;
    char buf[100];

    PDF_checkOffline();

    if (appendingPathWithText(pd)) return;

    if(pd->inText) textoff(pd);
    if(R_VIS(gc->col)) {
        if (pd->appendingPath < 0) {        
            PDF_SetLineColor(gc->col, dd);
            PDF_SetLineStyle(gc, dd);
        }
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
	xx = x[0];
	yy = y[0];
	PDFwrite(buf, 100, "%.2f %.2f m\n", pd, xx, yy);
	for(i = 1 ; i < n ; i++) {
	    xx = x[i];
	    yy = y[i];
	    PDFwrite(buf, 100, "%.2f %.2f l\n", pd, xx, yy);
	}
        if (pd->appendingPath < 0) {        
            PDFwrite(buf, 100, "S\n", pd);
        } else {
            pd->pathContainsDrawing = TRUE;
        }
    }
}

static int PDFfontNumber(const char *family, int face, PDFDesc *pd)
{
    /* DingBats is font 1 */
    int num = 1;

    if (strlen(family) > 0) {
	int fontIndex, cidfontIndex;
	/*
	 * Try to find font in already loaded fonts
	 */
	type1fontfamily fontfamily = findDeviceFont(family, pd->fonts,
						    &fontIndex);
	cidfontfamily cidfontfamily = findDeviceCIDFont(family, pd->cidfonts,
							&cidfontIndex);
	if (fontfamily)
	    num = (fontIndex - 1)*5 + 1 + face;
	else if (cidfontfamily)
	    /*
	     * Use very high font number for CID fonts to avoid
	     * Type 1 fonts
	     */
	    num = 1000 + (cidfontIndex - 1)*5 + face;
	else {
	    /*
	     * Check whether the font is loaded and, if not,
	     * load it.
	     */
	    fontfamily = findLoadedFont(family,
					pd->encodings->encoding->encpath,
					TRUE);
	    cidfontfamily = findLoadedCIDFont(family, TRUE);
	    if (!(fontfamily || cidfontfamily)) {
		if (isType1Font(family, PDFFonts, NULL)) {
		    fontfamily = addFont(family, TRUE, pd->encodings);
		} else if (isCIDFont(family, PDFFonts, NULL)) {
		    cidfontfamily = addCIDFont(family, TRUE);
		} else {
		    /*
		     * Should NOT get here.
		     */
		    error(_("invalid font type"));
		}
	    }
	    /*
	     * Once the font is loaded, add it to the device's
	     * list of fonts.
	     */
	    if (fontfamily || cidfontfamily) {
		if (isType1Font(family, PDFFonts, NULL)) {
		    if (addPDFDevicefont(fontfamily, pd, &fontIndex)) {
			num = (fontIndex - 1)*5 + 1 + face;
		    } else {
			fontfamily = NULL;
		    }
		} else /* (isCIDFont(family, PDFFonts)) */ {
		    if (addPDFDeviceCIDfont(cidfontfamily, pd,
					    &cidfontIndex)) {
			num = 1000 + (cidfontIndex - 1)*5 + face;
		    } else {
			cidfontfamily = NULL;
		    }
		}
	    }
	}
	if (!(fontfamily || cidfontfamily))
	    error(_("failed to find or load PDF font"));
    } else {
	if (isType1Font(family, PDFFonts, pd->defaultFont))
	    num = 1 + face;
	else
	    num = 1000 + face;
    }
    if(num < 100) pd->fontUsed[num] = TRUE;
    return num;
}

static void PDFWriteString(const char *str, size_t nb, PDFDesc *pd)
{
    size_t i;
    char buf[10];

    PDFwrite(buf, 2, "(", pd);
    for (i = 0 ; i < nb && *str; i++, str++)
	switch(*str) {
	case '\n':
            PDFwrite(buf, 10, "\\n", pd);
	    break;
	case '\\':
            PDFwrite(buf, 10, "\\\\", pd);
	    break;
	case '-':
#ifdef USE_HYPHEN
	    if (!isdigit((int)str[1]))
                PDFwrite(buf, 2, "%c", pd, PS_hyphen);
	    else
#endif
                PDFwrite(buf, 2, "%c", pd, *str);
	    break;
	case '(':
	case ')':
            PDFwrite(buf, 10, "\\%c", pd, *str);
	    break;
	default:
            PDFwrite(buf, 2, "%c", pd, *str);
	    break;
	}
    PDFwrite(buf, 2, ")", pd);
}

/* added for 2.9.0 (donated by Ei-ji Nakama) : */
static void PDFWriteT1KerningString(const char *str,
				    FontMetricInfo *metrics,
				    const pGEcontext gc,
                                    PDFDesc *pd)
{
    unsigned char p1, p2;
    size_t i, n;
    int j, ary_buf[128], *ary;
    bool haveKerning = false;
    char buf[10];

    n = strlen(str);
    if (n < 1) return;
    if(n > sizeof(ary_buf)/sizeof(int))
	ary = R_Calloc(n, int);
    else ary = ary_buf;

    for(i = 0; i < n-1; i++) {
	ary[i] = 0.;
	p1 = str[i];
	p2 = str[i+1];
#ifdef USE_HYPHEN
	if (p1 == '-' && !isdigit((int)p2))
	    p1 = PS_hyphen;
#endif
	for (j = metrics->KPstart[p1]; j < metrics->KPend[p1]; j++)
	    if(metrics->KernPairs[j].c2 == p2 &&
	       metrics->KernPairs[j].c1 == p1) {
		ary[i] += metrics->KernPairs[j].kern;
		haveKerning = true;
		break;
	    }
    }
    ary[i] = 0;
    if(haveKerning) {
        PDFwrite(buf, 10, "[", pd); 
        PDFwrite(buf, 10, "(", pd);
	for(i =  0; str[i]; i++) {
	    switch(str[i]) {
	    case '\n':
                PDFwrite(buf, 10, "\\n", pd);
		break;
	    case '\\':
                PDFwrite(buf, 10, "\\\\", pd);
		break;
	    case '-':
#ifdef USE_HYPHEN
		if (!isdigit((int)str[i+1]))
		    PDFwrite(buf, 2, "%c", pd, PS_hyphen);
		else
#endif
                PDFwrite(buf, 2, "%c", pd, str[i]);
		break;
	    case '(':
	    case ')':
                PDFwrite(buf, 10, "\\%c", pd, str[i]);
		break;
	    default:
                PDFwrite(buf, 2, "%c", pd, str[i]);
		break;
	    }
	    if ( ary[i] != 0 && str[i+1] ) 
                PDFwrite(buf, 10, ") %d (", pd, -ary[i]);
	}
        PDFwrite(buf, 10, ")] TJ\n", pd);
    } else {
	PDFWriteString(str, strlen(str), pd);
        PDFwrite(buf, 10, " Tj\n", pd);
    }

    if(ary != ary_buf) R_Free(ary);
}

static void PDFSetTextGraphicsState(const pGEcontext gc, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int code;
    if (pd->appendingPath < 0) {
        PDF_SetFill(gc->col, dd);
    } else {
        /* Obey different parameter settings if adding to a path */
        if (gc->patternFill != R_NilValue) { 
            if (R_VIS(gc->col)) {
                code = 3;
            } else {
                code = 2;
            }
        } else {            
            code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
        }
        if (gc->patternFill != R_NilValue) { 
            PDF_SetPatternFill(gc->patternFill, dd);
        } else if (code & 2) {
            PDF_SetFill(gc->fill, dd);
        }
        if (code & 1) {
            PDF_SetLineColor(gc->col, dd);
            PDF_SetLineStyle(gc, dd);
        }
    }
}

static void PDFSetTextRenderMode(PDFDesc *pd) {
    char buf[10];
    int mode = 0;
    if (pd->appendingPath >= 0) {
        /* Set text rendering mode */
        switch (pd->definitions[pd->appendingPath].type) {
        case PDFclipPath: mode = 7; break;
        case PDFstrokePath: mode = 1; break;
        case PDFfillPath: mode = 0; break;
        case PDFfillStrokePath: mode = 2; break;
        }
        PDFwrite(buf, 10, "%d Tr\n", pd, mode);
    }
}

static FontMetricInfo *PDFmetricInfo(const char *, int, PDFDesc *);
static void PDFSimpleText(double x, double y, const char *str,
			  double rot, double hadj,
			  int font,
			  const pGEcontext gc,
			  pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int size = (int)floor(gc->cex * gc->ps + 0.5);
    int face = gc->fontface;
    double a, b, bm, rot1;
    char buf[200];

    if(!R_VIS(gc->col) || size <= 0) return;

    if(face < 1 || face > 5) {
	warning(_("attempt to use invalid font %d replaced by font 1"), face);
	face = 1;
    }

    /* Do NOT write text to a path that already contains drawing */
    if (pd->appendingPath >= 0 &&
        pd->pathContainsDrawing) {
        warning(_("Text not added to path containing other drawing"));
    } else {

        rot1 = rot * DEG2RAD;
        a = size * cos(rot1);
        b = size * sin(rot1);
        bm = -b;
        /* avoid printing -0.00 on rotated text */
        if(fabs(a) < 0.01) a = 0.0;
        if(fabs(b) < 0.01) {b = 0.0; bm = 0.0;}

        if(!pd->inText) texton(pd);
        PDFSetTextGraphicsState(gc, dd);
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        PDFSetTextRenderMode(pd);
        PDFwrite(buf, 200, "/F%d 1 Tf %.2f %.2f %.2f %.2f %.2f %.2f Tm ", pd,
                 font, a, b, bm, a, x, y);
        if (pd->useKern &&
            isType1Font(gc->fontfamily, PDFFonts, pd->defaultFont)) {
            PDFWriteT1KerningString(str,
                                    PDFmetricInfo(gc->fontfamily, face, pd), 
                                    gc, pd);
        } else {
            PDFWriteString(str, strlen(str), pd);
            PDFwrite(buf, 200, " Tj\n", pd);
        }
        textoff(pd); /* added in 2.8.0 */

        if (pd->appendingPath >= 0) {
            pd->pathContainsText = TRUE;
            pd->pathContainsDrawing = TRUE;
        }
    }
}

static char *PDFconvname(const char *family, PDFDesc *pd);

static void PDF_Text0(double x, double y, const char *str, int enc,
		      double rot, double hadj,
		      const pGEcontext gc,
		      pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int size = (int) floor(gc->cex * gc->ps + 0.5);
    int face = gc->fontface;
    double a, b, bm, rot1;
    char *buff;
    const char *str1;
    char buf10[10], buf200[200];

    PDF_checkOffline();

    if(!R_VIS(gc->col) || size <= 0) return;

    /* Do NOT write text to a path that already contains drawing */
    if (pd->appendingPath >= 0 &&
        pd->pathContainsDrawing) {
        warning(_("Text not added to path containing other drawing"));

    } else {

        if(face < 1 || face > 5) {
            warning(_("attempt to use invalid font %d replaced by font 1"), 
                    face);
            face = 1;
        }
        if (face == 5) {
            PDFSimpleText(x, y, str, rot, hadj,
                          PDFfontNumber(gc->fontfamily, face, pd),
                          gc, dd);
            return;
        }

        rot1 = rot * DEG2RAD;
        a = size * cos(rot1);
        b = size * sin(rot1);
        bm = -b;
        /* avoid printing -0.00 on rotated text */
        if(fabs(a) < 0.01) a = 0.0;
        if(fabs(b) < 0.01) {b = 0.0; bm = 0.0;}
        if(!pd->inText) texton(pd);

        if(isCIDFont(gc->fontfamily, PDFFonts, pd->defaultCIDFont) && face != 5) {
            /* NB we could be in a SBCS here */
            size_t ucslen;
            unsigned char *p;
            int fontIndex;

            /*
             * CID convert optimize PDF encoding == locale encode case
             */
            cidfontfamily cidfont = findDeviceCIDFont(gc->fontfamily,
                                                      pd->cidfonts,
                                                      &fontIndex);
            if (!cidfont) {
                int dontcare;
                /*
                 * Try to load the font
                 */
                cidfont = addCIDFont(gc->fontfamily, 1);
                if (cidfont) {
                    if (!addPDFDeviceCIDfont(cidfont, pd, &dontcare)) {
                        cidfont = NULL;
                    }
                }
            }
            if (!cidfont)
                error(_("failed to find or load PDF CID font"));
            if(!dd->hasTextUTF8 &&
               !strcmp(locale2charset(NULL), cidfont->encoding)) {
                PDFSetTextGraphicsState(gc, dd);
                if (pd->currentMask >= 0) {
                    PDFwriteMask(pd->currentMask, pd);
                }
                PDFSetTextRenderMode(pd);
                PDFwrite(buf200, 200,
                         "/F%d 1 Tf %.2f %.2f %.2f %.2f %.2f %.2f Tm ", pd,
                         PDFfontNumber(gc->fontfamily, face, pd),
                         a, b, bm, a, x, y);
                PDFwrite(buf10, 10, "<", pd);
                p = (unsigned char *) str;
                while(*p)
                    PDFwrite(buf10, 10, "%02x", pd, *p++);
                PDFwrite(buf10, 10, ">", pd);
                PDFwrite(buf10, 10, " Tj\n", pd);

                if (pd->appendingPath >= 0) {
                    pd->pathContainsText = TRUE;
                    pd->pathContainsDrawing = TRUE;
                }

                return;
            }

            /*
             * CID convert  PDF encoding != locale encode case
             */
            ucslen = (dd->hasTextUTF8) ? Rf_utf8towcs(NULL, str, 0): mbstowcs(NULL, str, 0);
            if (ucslen != (size_t)-1) {
                void *cd;
                const char *i_buf; char *o_buf;
                size_t i, nb, i_len,  o_len, buflen = ucslen*sizeof(R_ucs2_t);
                size_t status;

                cd = (void*)Riconv_open(cidfont->encoding,
                                        (enc == CE_UTF8) ? "UTF-8": "");
                if(cd  == (void*)-1) return;

                R_CheckStack2(buflen);
                unsigned char buf[buflen];

                i_buf = (char *)str;
                o_buf = (char *)buf;
                i_len = strlen(str); /* no terminator,
                                        as output a byte at a time */
                nb = o_len = buflen;

                status = Riconv(cd, &i_buf, (size_t *)&i_len,
                                (char **)&o_buf, (size_t *)&o_len);

                Riconv_close(cd);
                if(status == (size_t)-1)
                    warning(_("failed in text conversion to encoding '%s'"),
                            cidfont->encoding);
                else {
                    unsigned char *p;
                    PDFSetTextGraphicsState(gc, dd);
                    if (pd->currentMask >= 0) {
                        PDFwriteMask(pd->currentMask, pd);
                    }
                    PDFSetTextRenderMode(pd);
                    PDFwrite(buf200, 200,
                            "/F%d 1 Tf %.2f %.2f %.2f %.2f %.2f %.2f Tm <", pd,
                            PDFfontNumber(gc->fontfamily, face, pd),
                            a, b, bm, a, x, y);
                    for(i = 0, p = buf; i < nb - o_len; i++)
                        PDFwrite(buf10, 10, "%02x", pd, *p++);
                    PDFwrite(buf10, 10, "> Tj\n", pd);

                    if (pd->appendingPath >= 0) {
                        pd->pathContainsText = TRUE;
                        pd->pathContainsDrawing = TRUE;
                    }

                }
                return;
            } else {
                warning(_("invalid string in '%s'"), "PDF_Text");
                return;
            }
        }

        PDFSetTextGraphicsState(gc, dd);
        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        PDFSetTextRenderMode(pd);
        PDFwrite(buf200, 200, "/F%d 1 Tf %.2f %.2f %.2f %.2f %.2f %.2f Tm ", pd,
                PDFfontNumber(gc->fontfamily, face, pd),
                a, b, bm, a, x, y);
        if((enc == CE_UTF8 || mbcslocale) && !strIsASCII(str) && face < 5) {
            /* face 5 handled above */
            R_CheckStack2(2*strlen(str)+1);
	    /* Output string cannot be longer
	       -- but it can be in bytes if transliteration is used
	     */
            buff = alloca(2*strlen(str)+1);
            mbcsToSbcs(str, buff, PDFconvname(gc->fontfamily, pd), enc, 0);
            str1 = buff;
        } else str1 = str;

        if (pd->useKern &&
            isType1Font(gc->fontfamily, PDFFonts, pd->defaultFont)) {
            PDFWriteT1KerningString(str1,
                                    PDFmetricInfo(gc->fontfamily, face, pd), 
                                    gc, pd);
        } else{
            PDFWriteString(str1, strlen(str1), pd);
            PDFwrite(buf10, 10, " Tj\n", pd);
        }
        textoff(pd); /* added in 2.8.0 */

        if (pd->appendingPath >= 0) {
            pd->pathContainsText = TRUE;
            pd->pathContainsDrawing = TRUE;
        }

    }
}

static void PDF_Text(double x, double y, const char *str,
		      double rot, double hadj,
		      const pGEcontext gc,
		      pDevDesc dd)
{
    PDF_Text0(x, y, str, CE_NATIVE, rot, hadj, gc, dd);
}

static void PDF_TextUTF8(double x, double y, const char *str,
			 double rot, double hadj,
			 const pGEcontext gc,
			 pDevDesc dd)
{
    PDF_Text0(x, y, str, CE_UTF8, rot, hadj, gc, dd);
}

static FontMetricInfo
*PDFCIDsymbolmetricInfo(const char *family, PDFDesc *pd)
{
    FontMetricInfo *result = NULL;
    if (strlen(family) > 0) {
	int dontcare;
	/*
	 * Find the family in pd->cidfonts
	 */
	cidfontfamily fontfamily = findDeviceCIDFont(family,
						     pd->cidfonts,
						     &dontcare);
	if (fontfamily)
	    result = &(fontfamily->symfont->metrics);
	else {
	    /*
	     * Try to load the font
	     */
	    fontfamily = addCIDFont(family, 1);
	    if (fontfamily) {
		if (addPDFDeviceCIDfont(fontfamily, pd, &dontcare)) {
		    result = &(fontfamily->symfont->metrics);
		} else {
		    fontfamily = NULL;
		}
	    }
	}
	if (!fontfamily)
	    error(_("failed to find or load PDF CID font"));
    } else {
	result = &(pd->cidfonts->cidfamily->symfont->metrics);
    }
    return result;
}

static FontMetricInfo
*PDFmetricInfo(const char *family, int face, PDFDesc *pd)
{
    FontMetricInfo *result = NULL;
    if (strlen(family) > 0) {
	int dontcare;
	/*
	 * Find the family in pd->fonts
	 */
	type1fontfamily fontfamily = findDeviceFont(family, pd->fonts,
						    &dontcare);
	if (fontfamily)
	    result = &(fontfamily->fonts[face-1]->metrics);
	else {
	    /*
	     * Check whether the font is loaded and, if not,
	     * load it.
	     */
	    fontfamily = findLoadedFont(family,
					pd->encodings->encoding->encpath,
					TRUE);
	    if (!fontfamily) {
		fontfamily = addFont(family, TRUE, pd->encodings);
	    }
	    /*
	     * Once the font is loaded, add it to the device's
	     * list of fonts.
	     */
	    if (fontfamily) {
		int dontcare;
		if (addPDFDevicefont(fontfamily, pd, &dontcare)) {
		    result = &(fontfamily->fonts[face-1]->metrics);
		} else {
		    fontfamily = NULL;
		}
	    }
	}
	if (!fontfamily)
	    error(_("failed to find or load PDF font"));
    } else {
	result = &(pd->fonts->family->fonts[face-1]->metrics);
    }
    return result;
}

static char
*PDFconvname(const char *family, PDFDesc *pd)
{
    char *result = (pd->fonts) ? pd->fonts->family->encoding->convname : "latin1";
    /* pd->fonts is NULL when CIDfonts are used */

    if (strlen(family) > 0) {
	int dontcare;
	/*
	 * Find the family in pd->fonts
	 */
	type1fontfamily fontfamily = findDeviceFont(family, pd->fonts,
						    &dontcare);
	if (fontfamily)
	    result = fontfamily->encoding->convname;
	else {
	    /*
	     * Check whether the font is loaded and, if not,
	     * load it.
	     */
	    fontfamily = findLoadedFont(family,
					pd->encodings->encoding->encpath,
					TRUE);
	    if (!fontfamily) {
		fontfamily = addFont(family, TRUE, pd->encodings);
	    }
	    /*
	     * Once the font is loaded, add it to the device's
	     * list of fonts.
	     */
	    if (fontfamily) {
		int dontcare;
		if (addPDFDevicefont(fontfamily, pd, &dontcare)) {
		    result = fontfamily->encoding->convname;
		} else {
		    fontfamily = NULL;
		}
	    }
	}
	if (!fontfamily)
	    error(_("failed to find or load PDF font"));
    }
    return result;
}

double PDF_StrWidth(const char *str,
                    const pGEcontext gc,
                    pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    if(gc->fontface < 1 || gc->fontface > 5) gc->fontface = 1;
    if (isType1Font(gc->fontfamily, PDFFonts, pd->defaultFont)) {
	return floor(gc->cex * gc->ps + 0.5) *
	    PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				  PDFmetricInfo(gc->fontfamily,
						gc->fontface, pd),
				  pd->useKern, gc->fontface,
				  PDFconvname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily) */
	if (gc->fontface < 5) {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				      NULL, FALSE, gc->fontface, NULL);
	} else {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_NATIVE,
				      PDFCIDsymbolmetricInfo(gc->fontfamily,
							     pd),
				      FALSE, gc->fontface, NULL);
	}
    }
}

static double PDF_StrWidthUTF8(const char *str,
			       const pGEcontext gc,
			       pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int face = gc->fontface;

    if(gc->fontface < 1 || gc->fontface > 5) gc->fontface = 1;
    if (isType1Font(gc->fontfamily, PDFFonts, pd->defaultFont)) {
	return floor(gc->cex * gc->ps + 0.5) *
	    PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				  PDFmetricInfo(gc->fontfamily,
						gc->fontface, pd),
				  pd->useKern, gc->fontface,
				  PDFconvname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily) */
	if (face < 5) {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				      NULL, FALSE, gc->fontface, NULL);
	} else {
	    return floor(gc->cex * gc->ps + 0.5) *
		PostScriptStringWidth((const unsigned char *)str, CE_UTF8,
				      PDFCIDsymbolmetricInfo(gc->fontfamily,
							     pd),
				      FALSE, gc->fontface, NULL);
	}
    }
}

void PDF_MetricInfo(int c,
                    const pGEcontext gc,
                    double* ascent, double* descent,
                    double* width, pDevDesc dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int face = gc->fontface;

    if(gc->fontface < 1 || gc->fontface > 5) gc->fontface = 1;
    if (isType1Font(gc->fontfamily, PDFFonts, pd->defaultFont)) {
	PostScriptMetricInfo(c, ascent, descent, width,
			     PDFmetricInfo(gc->fontfamily,
					   gc->fontface, pd),
			     true, face == 5, PDFconvname(gc->fontfamily, pd));
    } else { /* cidfont(gc->fontfamily) */
	if (face < 5) {
	    PostScriptCIDMetricInfo(c, ascent, descent, width);
	} else {
	    PostScriptMetricInfo(c, ascent, descent, width,
				 PDFCIDsymbolmetricInfo(gc->fontfamily, pd),
				 false, true, "");
	}
    }
    *ascent = floor(gc->cex * gc->ps + 0.5) * *ascent;
    *descent = floor(gc->cex * gc->ps + 0.5) * *descent;
    *width = floor(gc->cex * gc->ps + 0.5) * *width;
}

static SEXP PDF_setPattern(SEXP pattern, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    SEXP ref = R_NilValue;

    if (!pd->offline) {
        ref = addPattern(pattern, pd);
    }

    return ref;
}

static void PDF_releasePattern(SEXP ref, pDevDesc dd) {} 

static SEXP PDF_setClipPath(SEXP path, SEXP ref, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    SEXP newref = R_NilValue;

    if (!pd->offline) {
        if (isNull(ref)) {
            /* Generate new clipping path */
            int index = newPath(path, PDFclipPath, pd);
            if (index >= 0) {
                PDFwriteClipPath(index, pd);
                PROTECT(newref = allocVector(INTSXP, 1));
                INTEGER(newref)[0] = index;
                UNPROTECT(1);
            }
        } else {
            /* Reuse existing clipping path */
            int index = INTEGER(ref)[0];
            PDFwriteClipPath(index, pd);
            newref = ref;
        }

        PDF_Invalidate(pd);
    }

    return newref;

}

static void PDF_releaseClipPath(SEXP ref, pDevDesc dd) {}

static SEXP PDF_setMask(SEXP path, SEXP ref, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    if (!pd->offline) {
        ref = addMask(path, ref, pd);
    }

    return ref;
}

static void PDF_releaseMask(SEXP ref, pDevDesc dd) {}

static SEXP PDF_defineGroup(SEXP source, int op, SEXP destination, 
                            pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    SEXP ref = R_NilValue;

    if (!pd->offline) {
        int index = newGroup(source, op, destination, pd);
        if (index >= 0) {
            PROTECT(ref = allocVector(INTSXP, 1));
            INTEGER(ref)[0] = index;
            UNPROTECT(1);
        }
    }

    return ref;
}

static void PDF_useGroup(SEXP ref, SEXP trans, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int index;

    PDF_checkOffline();

    if(pd->inText) textoff(pd);

    /* Compositing groups do not contribute to a clipping path */
    if (pd->appendingPath < 0) {

        if (pd->currentMask >= 0) {
            PDFwriteMask(pd->currentMask, pd);
        }
        index = INTEGER(ref)[0];
        /* Draw the transparency group */
        if (index >= 0) {
            if (trans != R_NilValue) {
                char buf[100];
                PDFwrite(buf, 4, "q\n", pd);
                /* Apply the transform */
                PDFwrite(buf, 100, "%f %f %f %f %f %f cm\n", pd,
                         REAL(trans)[0],
                         REAL(trans)[3],
                         REAL(trans)[1],
                         REAL(trans)[4],
                         REAL(trans)[2],
                         REAL(trans)[5]);
            }

            PDFwriteGroup(index, pd);

            if (trans != R_NilValue) {
                char buf[4];
                PDFwrite(buf, 4, "Q\n", pd);
            }
        }

    }
}

static void PDF_releaseGroup(SEXP ref, pDevDesc dd) {}

static void PDF_stroke(SEXP path, const pGEcontext gc, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_checkOffline();

    int index = newPath(path, PDFstrokePath, pd);
    if (index >= 0) {
        if(pd->inText) textoff(pd);
        if(R_VIS(gc->col)) {
            PDF_SetLineColor(gc->col, dd);
            PDF_SetLineStyle(gc, dd);
            if (pd->currentMask >= 0) {
                PDFwriteMask(pd->currentMask, pd);
            }
            PDFStrokePath(index, pd);
        }
    }
}

static void PDF_fill(SEXP path, int rule, const pGEcontext gc, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_checkOffline();

    int index = newPath(path, PDFfillPath, pd);
    if (index >= 0) {
        if (gc->patternFill != R_NilValue || R_VIS(gc->fill)) {
            if(pd->inText) textoff(pd);
            if (gc->patternFill != R_NilValue) { 
                PDF_SetPatternFill(gc->patternFill, dd);
            } else if (R_VIS(gc->fill)) {
                PDF_SetFill(gc->fill, dd);
            }
            if (pd->currentMask >= 0) {
                PDFwriteMask(pd->currentMask, pd);
            }
            PDFFillPath(index, rule, pd);
        }
    }
}

static void PDF_fillStroke(SEXP path, int rule, 
                           const pGEcontext gc, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_checkOffline();

    int code;
    int index = newPath(path, PDFfillStrokePath, pd);
    if (index >= 0) {
        if (gc->patternFill != R_NilValue) { 
            if (R_VIS(gc->col)) {
                code = 3;
            } else {
                code = 2;
            }
        } else {            
            code = 2 * (R_VIS(gc->fill)) + (R_VIS(gc->col));
        }
        if (code) {
            if(pd->inText) textoff(pd);
            if (gc->patternFill != R_NilValue) { 
                PDF_SetPatternFill(gc->patternFill, dd);
            } else if(code & 2) {
                PDF_SetFill(gc->fill, dd);
            }
            if(code & 1) {
                PDF_SetLineColor(gc->col, dd);
                PDF_SetLineStyle(gc, dd);
            }
            if (pd->currentMask >= 0) {
                PDFwriteMask(pd->currentMask, pd);
            }
            switch(code) {
            case 1: PDFStrokePath(index, pd); break;
            case 2: PDFFillPath(index, rule, pd); break;
            case 3: PDFFillStrokePath(index, rule, pd); break;
            }

        }
    }
}

static SEXP PDF_capabilities(SEXP capabilities) {
    SEXP patterns, clippingPaths, masks, compositing, transforms, paths, glyphs;

    PROTECT(patterns = allocVector(INTSXP, 3));
    INTEGER(patterns)[0] = R_GE_linearGradientPattern;
    INTEGER(patterns)[1] = R_GE_radialGradientPattern;
    INTEGER(patterns)[2] = R_GE_tilingPattern;
    SET_VECTOR_ELT(capabilities, R_GE_capability_patterns, patterns);
    UNPROTECT(1);

    PROTECT(clippingPaths = allocVector(INTSXP, 1));
    INTEGER(clippingPaths)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_clippingPaths, clippingPaths);
    UNPROTECT(1);

    PROTECT(masks = allocVector(INTSXP, 2));
    INTEGER(masks)[0] = R_GE_alphaMask;
    INTEGER(masks)[1] = R_GE_luminanceMask;
    SET_VECTOR_ELT(capabilities, R_GE_capability_masks, masks);
    UNPROTECT(1);

    PROTECT(compositing = allocVector(INTSXP, 11));
    INTEGER(compositing)[0] = R_GE_compositeMultiply;
    INTEGER(compositing)[1] = R_GE_compositeScreen;
    INTEGER(compositing)[2] = R_GE_compositeOverlay;
    INTEGER(compositing)[3] = R_GE_compositeDarken;
    INTEGER(compositing)[4] = R_GE_compositeLighten;
    INTEGER(compositing)[5] = R_GE_compositeColorDodge;
    INTEGER(compositing)[6] = R_GE_compositeColorBurn;
    INTEGER(compositing)[7] = R_GE_compositeHardLight;
    INTEGER(compositing)[8] = R_GE_compositeSoftLight;
    INTEGER(compositing)[9] = R_GE_compositeDifference;
    INTEGER(compositing)[10] = R_GE_compositeExclusion;
    SET_VECTOR_ELT(capabilities, R_GE_capability_compositing, compositing);
    UNPROTECT(1);

    PROTECT(transforms = allocVector(INTSXP, 1));
    INTEGER(transforms)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_transformations, transforms);
    UNPROTECT(1);

    PROTECT(paths = allocVector(INTSXP, 1));
    INTEGER(paths)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_paths, paths);
    UNPROTECT(1);

    PROTECT(glyphs = allocVector(INTSXP, 1));
    INTEGER(glyphs)[0] = 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_glyphs, glyphs);
    UNPROTECT(1);

    return capabilities;
}

static void PDF_glyph(int n, int *glyphs, double *x, double *y, 
                      SEXP font, double size, 
                      int colour, double rot, pDevDesc dd) {
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_checkOffline();

    int index = newGlyphFont(R_GE_glyphFontPSname(font), pd);
    if (index >= 0) {
        if (R_VIS(colour)) {
            if(pd->inText) textoff(pd);
            PDF_SetFill(colour, dd);
            if (pd->currentMask >= 0) {
                PDFwriteMask(pd->currentMask, pd);
            }
            PDFSetTextRenderMode(pd);
            PDFGlyphs(n, glyphs, x, y, size, rot, pd);
        }
    }
}


/*  PostScript Device Driver Parameters:
 *  ------------------------
 *  file	= output filename
 *  paper	= paper type
 *  family	= typeface = "family"
 *  encoding	= char encoding file name
 *  cidfamily	= char encoding file name for CID fonts
 *  bg		= background color
 *  fg		= foreground color
 *  width	= width in inches
 *  height	= height in inches
 *  horizontal	= {TRUE: landscape; FALSE: portrait}
 *  ps		= pointsize
 *  onefile     = {TRUE: normal; FALSE: single EPSF page}
 *  pagecentre  = centre plot region on paper?
 *  printit     = 'print' after closing device?
 *  command     = 'print' command
 *  title       = character string
 *  fonts
 *  colorModel
 *  useKerning
 *  fillOddEven
 */

SEXP PostScript(SEXP args)
{
    pGEDevDesc gdd;
    const void *vmax;
    const char *file, *paper, *family=NULL, *bg, *fg, *cmd;
    const char *afms[5];
    const char *encoding, *title, call[] = "postscript", *colormodel;
    int i, horizontal,  useKern;
    bool onefile, pagecentre, printit;
    double height, width, ps;
    SEXP fam, fonts;
    bool fillOddEven;

    vmax = vmaxget();
    args = CDR(args); /* skip entry point name */
    SEXP tmp = asChar(CAR(args));
    if (tmp == NA_STRING)
	error(_("invalid 'file' parameter in %s"), call);
    file = translateCharFP(tmp);  args = CDR(args);
    paper = CHAR(asChar(CAR(args))); args = CDR(args);

    /* 'family' can be either one string or a 5-vector of afmpaths. */
    fam = CAR(args); args = CDR(args);
    if(length(fam) == 1)
	family = CHAR(asChar(fam));
    else if(length(fam) == 5) {
	if(!isString(fam)) error(_("invalid 'family' parameter in %s"), call);
	family = "User";
	for(i = 0; i < 5; i++) afms[i] = CHAR(STRING_ELT(fam, i));
    } else
	error(_("invalid 'family' parameter in %s"), call);

    encoding = CHAR(asChar(CAR(args)));    args = CDR(args);
    bg = CHAR(asChar(CAR(args)));    args = CDR(args);
    fg = CHAR(asChar(CAR(args)));    args = CDR(args);
    width = asReal(CAR(args));	      args = CDR(args);
    height = asReal(CAR(args));	      args = CDR(args);
    horizontal = asLogical(CAR(args));args = CDR(args);
    if(horizontal == NA_LOGICAL)
	horizontal = 1;
    ps = asReal(CAR(args));	      args = CDR(args);
    onefile = asBool(CAR(args));   args = CDR(args);
    pagecentre = asBool(CAR(args));args = CDR(args);
    printit = asBool(CAR(args));   args = CDR(args);
    cmd = CHAR(asChar(CAR(args)));    args = CDR(args);
    title = translateChar(asChar(CAR(args)));  args = CDR(args);
    fonts = CAR(args);		      args = CDR(args);
    if (!isNull(fonts) && !isString(fonts))
	error(_("invalid 'fonts' parameter in %s"), call);
    colormodel = CHAR(asChar(CAR(args)));  args = CDR(args);
    useKern = asLogical(CAR(args));   args = CDR(args);
    if (useKern == NA_LOGICAL) useKern = 1;
    fillOddEven = asBool(CAR(args));
    if (fillOddEven == NA_LOGICAL)
	error(_("invalid value of '%s'"), "fillOddEven");

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev;
	if (!(dev = (pDevDesc) calloc(1, sizeof(DevDesc))))
	    return 0;
	if(!PSDeviceDriver(dev, file, paper, family, afms, encoding, bg, fg,
			   width, height, (bool)horizontal, ps, onefile,
			   pagecentre, printit, cmd, title, fonts,
			   colormodel, useKern, fillOddEven)) {
	    /* we no longer get here: error is thrown in PSDeviceDriver */
	    error(_("unable to start %s() device"), "postscript");
	}
	gdd = GEcreateDevDesc(dev);
	GEaddDevice2f(gdd, "postscript", file);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}



/*  PDF Device Driver Parameters:
 *  ------------------------
 *  file	= output filename
 *  paper       = paper type
 *  family	= typeface = "family"
 *  encoding	= char encoding file name
 *  cidfamily	= char encoding file name for CID fonts
 *  bg		= background color
 *  fg		= foreground color
 *  width	= width in inches
 *  height	= height in inches
 *  ps		= pointsize
 *  onefile     = {TRUE: normal; FALSE: single page per file}
 *  pagecentre
 *  title
 *  fonts
 *  versionMajor
 *  versionMinor
 *  colormodel
 *  useDingbats
 *  forceLetterSpacing
 *  fillOddEven
 *  ...
 */

SEXP PDF(SEXP args)
{
    pGEDevDesc gdd;
    const void *vmax;
    const char *file, *paper, *encoding, *family = NULL /* -Wall */,
	*bg, *fg, *title, call[] = "PDF", *colormodel, *author;
    const char *afms[5];
    double height, width, ps;
    int i, major, minor, dingbats, useKern, useCompression, 
	timestamp, producer, fillOddEven;
    bool onefile, pagecentre;
    SEXP fam, fonts;

    vmax = vmaxget();
    args = CDR(args); /* skip entry point name */
    if (isNull(CAR(args)))
        file = NULL;
    else
        file = translateCharFP(asChar(CAR(args)));  
    args = CDR(args);
    paper = CHAR(asChar(CAR(args))); args = CDR(args);
    fam = CAR(args); args = CDR(args);
    if(length(fam) == 1)
	family = CHAR(asChar(fam));
    else if(length(fam) == 5) {
	if(!isString(fam)) error(_("invalid 'family' parameter in %s"), call);
	family = "User";
	for(i = 0; i < 5; i++) afms[i] = CHAR(STRING_ELT(fam, i));
    } else
	error(_("invalid 'family' parameter in %s"), call);
    encoding = CHAR(asChar(CAR(args)));  args = CDR(args);
    bg = CHAR(asChar(CAR(args)));    args = CDR(args);
    fg = CHAR(asChar(CAR(args)));    args = CDR(args);
    width = asReal(CAR(args));	      args = CDR(args);
    height = asReal(CAR(args));	      args = CDR(args);
    ps = asReal(CAR(args));           args = CDR(args);
    onefile = asBool(CAR(args)); args = CDR(args);
    pagecentre = asBool(CAR(args));args = CDR(args);
    title = translateChar(asChar(CAR(args))); args = CDR(args);
    fonts = CAR(args); args = CDR(args);
    if (!isNull(fonts) && !isString(fonts))
	error(_("invalid 'fonts' parameter in %s"), call);
    major = asInteger(CAR(args)); args = CDR(args);
    minor = asInteger(CAR(args)); args = CDR(args);
    colormodel = CHAR(asChar(CAR(args))); args = CDR(args);
    dingbats = asLogical(CAR(args)); args = CDR(args);
    if (dingbats == NA_LOGICAL) dingbats = 1;
    useKern = asLogical(CAR(args)); args = CDR(args);
    if (useKern == NA_LOGICAL) useKern = 1;
    fillOddEven = asLogical(CAR(args)); args = CDR(args);
    if (fillOddEven == NA_LOGICAL)
	error(_("invalid value of '%s'"), "fillOddEven");
    useCompression = asLogical(CAR(args)); args = CDR(args);
    if (useCompression == NA_LOGICAL)
	error(_("invalid value of '%s'"), "useCompression");
    timestamp = asLogical(CAR(args)); args = CDR(args);
    if (timestamp == NA_LOGICAL)
	error(_("invalid value of '%s'"), "timestamp");
    producer = asLogical(CAR(args)); args = CDR(args);
    if (producer == NA_LOGICAL)
	error(_("invalid value of '%s'"), "producer");
    author = translateChar(asChar(CAR(args))); args = CDR(args);

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	pDevDesc dev;
	if (!(dev = (pDevDesc) calloc(1, sizeof(DevDesc))))
	    return 0;
	if(!PDFDeviceDriver(dev, file, paper, family, afms, encoding, bg, fg,
			    width, height, ps, onefile, pagecentre,
			    title, fonts, major, minor, colormodel,
			    dingbats, useKern, fillOddEven,
			    useCompression, timestamp, producer, author)) {
	    /* we no longer get here: error is thrown in PDFDeviceDriver */
	    error(_("unable to start %s() device"), "pdf");
	}
	gdd = GEcreateDevDesc(dev);
	GEaddDevice2f(gdd, "pdf", file);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}

