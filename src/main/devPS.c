/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2001  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <ctype.h>

#include "Defn.h"
#include "Graphics.h"
#include "R_ext/Error.h"
#include "Fileio.h"
#include <Rdevices.h>

/* Define this to use hyphen except in -[0-9] */
#undef USE_HYPHEN
/* In ISOLatin1, minus is 45 and hyphen is 173 */
#ifdef USE_HYPHEN
static char PS_hyphen = 173;
#endif

#define USERAFM 999

/* Part 0.  AFM File Names */

/* This structure gives the set of font names for each type face. */
/* They also give the afm file names. */

static struct {
    char *family;
    char *afmfile[4];
}
Family[] = {

    { "AvantGarde",
      {"agw_____.afm", "agd_____.afm", "agwo____.afm", "agdo____.afm" }
    },

    { "Bookman",
      {"bkl_____.afm", "bkd_____.afm", "bkli____.afm", "bkdi____.afm"}
    },

    { "Courier",
      {"com_____.afm", "cob_____.afm", "coo_____.afm", "cobo____.afm"}
    },

    { "Helvetica",
      {"hv______.afm", "hvb_____.afm", "hvo_____.afm", "hvbo____.afm"}
    },

    { "Helvetica-Narrow",
      {"hvn_____.afm", "hvnb____.afm", "hvno____.afm", "hvnbo___.afm"}
    },

    { "NewCenturySchoolbook",
      {"ncr_____.afm", "ncb_____.afm", "nci_____.afm", "ncbi____.afm"}
    },

    { "Palatino",
      {"por_____.afm", "pob_____.afm", "poi_____.afm", "pobi____.afm"}
    },

    { "Times",
      {"tir_____.afm", "tib_____.afm", "tii_____.afm", "tibi____.afm"}
    },

    { NULL }
};


static char familyname[5][50];

/* Part 1.  AFM File Parsing.  */

/* These are the basic entities in the AFM file */

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
    struct {
	short WX;
	short BBox[4];
    } CharInfo[256];
    KP *KernPairs;
    short KPstart[256];
    short KPend[256];
    short nKP;
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
    Unknown
};

static struct {
    char *keyword;
    int code;
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
    { NULL,		     Unknown },
};

static int MatchKey(char *l, char *k)
{
    while (*k)
	if (*k++ != *l++) return 0;
    return 1;
}

static int KeyType(char *s)
{
    int i;
    if (*s == '\n')
	return Empty;
    for (i = 0; KeyWordDictionary[i].keyword; i++)
	if (MatchKey(s, KeyWordDictionary[i].keyword))
	    return KeyWordDictionary[i].code;
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

static int GetFontBBox(char *buf, FontMetricInfo *metrics)
{
    if (sscanf(buf, "FontBBox %hd %hd %hd %hd",
	      &(metrics->FontBBox[0]),
	      &(metrics->FontBBox[1]),
	      &(metrics->FontBBox[2]),
	      &(metrics->FontBBox[3])) != 4) return 0;
#ifdef DEBUG_PS
    Rprintf("FontBBox %d %d %d %d\n",
	    (metrics->FontBBox[0]),
	    (metrics->FontBBox[1]),
	    (metrics->FontBBox[2]),
	    (metrics->FontBBox[3]));
#endif
    return 1;
}

static char charnames[256][25];
static char encnames[256][25];

/* If reencode > 0, remap to new encoding */
static int GetCharInfo(char *buf, FontMetricInfo *metrics, int reencode)
{
    char *p = buf, charname[25];
    int nchar, nchar2=-1, i;
    short WX;

    if (!MatchKey(buf, "C ")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%d", &nchar);
    if (nchar < 0 && !reencode) return 1;
    p = SkipToNextKey(p);

    if (!MatchKey(p, "WX")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%hd", &WX);
    p = SkipToNextKey(p);

    if (!MatchKey(p, "N ")) return 0;
    p = SkipToNextItem(p);
    if(reencode) {
	sscanf(p, "%s", charname);
#ifdef DEBUG_PS
	Rprintf("char name %s\n", charname);
#endif
	/* a few chars appear twice in ISOLatin1 */
	nchar = nchar2 = -1;
	for (i = 0; i < 256; i++)
	    if(!strcmp(charname, encnames[i])) {
		strcpy(charnames[i], charname); 
		if(nchar == -1) nchar = i; else nchar2 = i;
	    }
	if (nchar == -1) return 1;
    } else {
	sscanf(p, "%s", charnames[nchar]);
    }
    metrics->CharInfo[nchar].WX = WX;
    p = SkipToNextKey(p);

    if (!MatchKey(p, "B ")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%hd %hd %hd %hd",
	   &(metrics->CharInfo[nchar].BBox[0]),
	   &(metrics->CharInfo[nchar].BBox[1]),
	   &(metrics->CharInfo[nchar].BBox[2]),
	   &(metrics->CharInfo[nchar].BBox[3]));

#ifdef DEBUG_PS
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

#ifdef DEBUG_PS
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

static int GetKPX(char *buf, int nkp, FontMetricInfo *metrics)
{
    char *p = buf, c1[50], c2[50];
    int i, done = 0;

    p = SkipToNextItem(p);
    sscanf(p, "%s %s %hd", c1, c2, &(metrics->KernPairs[nkp].kern));
    for(i = 0; i < 256; i++) {
	if (!strcmp(c1, charnames[i])) {
	    metrics->KernPairs[nkp].c1 = i;
	    done++;
	    break;
	}
    }
    for(i = 0; i < 256; i++)
	if (!strcmp(c2, charnames[i])) {
	    metrics->KernPairs[nkp].c2 = i;
	    done++;
	    break;
	}
    return (done==2);
}

/* Encode File Parsing.  */
/* Statics here are OK, as all the calls are in one initialization
   so no concurrency (until threads?) */

/* read in the next encoding item, separated by white space. */
static int GetNextItem(FILE *fp, char *dest, int c)
{
    static char buf[1000], *p = NULL, *p0;

    if (c < 0) p = NULL;
    while (1) {
	if (feof(fp)) { p = NULL; return 1; }
	if (!p || *p == '\n' || *p == '\0') {
	    p = fgets(buf, 1000, fp);
	}
	while (isspace((int)*p)) p++;
	if (p == '\0' || *p == '%'|| *p == '\n') { p = NULL; continue; }
	p0 = p;
	while (!isspace((int)*p)) p++;
	if (p != '\0') *p++ = '\0';
	if(c == 45) strcpy(dest, "/minus"); else strcpy(dest, p0);
	break;
    }
    return 0;
}

static char enccode[5000];

/* Load encoding array from a file: defaults to the R_HOME/afm directory */
static int
LoadEncoding(char *encpath, char *encname, Rboolean isPDF)
{
    char buf[BUFSIZE];
    int i;
    FILE *fp;

    if(strchr(encpath, FILESEP[0])) strcpy(buf, encpath);
    else sprintf(buf, "%s%safm%s%s", R_Home, FILESEP, FILESEP, encpath);
#ifdef DEBUG_PS
    Rprintf("encoding path is %s\n", buf);
#endif
    if (!(fp = R_fopen(R_ExpandFileName(buf), "r"))) {
	strcat(buf, ".enc");
	if (!(fp = R_fopen(R_ExpandFileName(buf), "r"))) return 0;
    }
    if (GetNextItem(fp, buf, -1)) return 0; /* encoding name */
    strcpy(encname, buf+1); 
    if (!isPDF) sprintf(enccode, "/%s [\n", encname);
    else enccode[0] = '\0';
    if (GetNextItem(fp, buf, 0)) { fclose(fp); return 0;} /* [ */
    for(i = 0; i < 256; i++) {
	if (GetNextItem(fp, buf, i)) { fclose(fp); return 0; }
	strcpy(encnames[i], buf+1);
	strcat(enccode, " /"); strcat(enccode, encnames[i]);
	if(i%8 == 7) strcat(enccode, "\n");
    }
    if (GetNextItem(fp, buf, 0)) { fclose(fp); return 0;} /* ] */
    fclose(fp);
    if (!isPDF) strcat(enccode,"]\n");
    return 1;
}



/* Load font metrics from a file: defaults to the R_HOME/afm directory */

static int
PostScriptLoadFontMetrics(char *fontpath, FontMetricInfo *metrics,
			  char *fontname, int reencode)
{
    char buf[BUFSIZE], *p;
    int mode, i = 0, j, ii, nKPX=0;
    FILE *fp;

    if(strchr(fontpath, FILESEP[0])) strcpy(buf, fontpath);
    else sprintf(buf, "%s%safm%s%s", R_Home, FILESEP, FILESEP, fontpath);
#ifdef DEBUG_PS
    Rprintf("afmpath is %s\n", buf);
#endif

    if (!(fp = R_fopen(R_ExpandFileName(buf), "r"))) return 0;

    mode = 0;
    for (ii = 0; ii < 256; ii++) {
	charnames[ii][0] = '\0';
	metrics->CharInfo[ii].WX = NA_SHORT;
	for(j = 0; j < 4; j++) metrics->CharInfo[ii].BBox[j] = 0;
    }
    while (fgets(buf, BUFSIZE, fp)) {
	switch(KeyType(buf)) {

	case StartFontMetrics:
	    mode = StartFontMetrics;
	    break;

	case EndFontMetrics:
	    mode = 0;
	    break;

	case FontBBox:
	    if (!GetFontBBox(buf, metrics)) goto error;
	    break;

	case C:
	    if (mode != StartFontMetrics) goto error;
	    if (!GetCharInfo(buf, metrics, reencode)) goto error;
	    break;

	case StartKernData:
	    mode = StartKernData;
	    break;

	case StartKernPairs:
	    if(mode != StartKernData) goto error;
	    p = SkipToNextItem(buf);
	    sscanf(p, "%d", &nKPX);
	    metrics->KernPairs = (KP *) malloc(nKPX * sizeof(KP));
	    if (!metrics->KernPairs) goto error;
	    break;

	case KPX:
	    if(mode != StartKernData || i >= nKPX) goto error;
	    if (GetKPX(buf, i, metrics)) i++;
	    break;

	case EndKernData:
	    mode = 0;
	    break;

	case Unknown:
	    warning("unknown AFM entity encountered");
	    break;

	case FontName:
	    p = SkipToNextItem(buf);
	    sscanf(p, "%s", fontname);
	    break;

	case Empty:
	default:
	    break;
	}
    }
    metrics->nKP = i;
    fclose(fp);
    /* Make an index for kern-pair searches: relies on having contiguous
       blocks by first char for efficiency, but works in all cases. */
    {
	short ind, tmp;
	for (j = 0; j < 256; j++) {
	    metrics->KPstart[j] = i;
	    metrics->KPend[j] = 0;
	}
	for (j = 0; j < i; j++) {
	    ind = metrics->KernPairs[j].c1;
	    tmp = metrics->KPstart[ind];
	    if(j < tmp) metrics->KPstart[ind] = j;
	    tmp = metrics->KPend[ind];
	    if(j > tmp) metrics->KPend[ind] = j;
	}
    }
    return 1;
 error:
    fclose(fp);
    return 0;
}

static double
PostScriptStringWidth(unsigned char *p, FontMetricInfo *metrics)
{
    int sum = 0, i;
    short wx;
    unsigned char p1, p2;
    for ( ; *p; p++) {
#ifdef USE_HYPHEN
	if (*p == '-' && !isdigit(p[1]))
	    wx = metrics->CharInfo[(int)PS_hyphen].WX;
	else
#endif
	    wx = metrics->CharInfo[*p].WX;
	if(wx == NA_SHORT)
	    warning("font width unknown for character %d", *p);
	else sum += wx;

	/* check for kerning adjustment */
	p1 = p[0]; p2 = p[1];
	for (i =  metrics->KPstart[p1]; i < metrics->KPend[p1]; i++)
	/* second test is a safety check: should all start with p1  */
	    if(metrics->KernPairs[i].c2 == p2 &&
	       metrics->KernPairs[i].c1 == p1) {
		sum += metrics->KernPairs[i].kern;
		break;
	    }
    }
    return 0.001 * sum;
}

static void
PostScriptMetricInfo(int c, double *ascent, double *descent,
		     double *width, FontMetricInfo *metrics)
{
    short wx;
    
    if (c == 0) {
	*ascent = 0.001 * metrics->FontBBox[3];
	*descent = -0.001 * metrics->FontBBox[1];
	*width = 0.001 * (metrics->FontBBox[2] - metrics->FontBBox[0]);
    }
    else {
	*ascent = 0.001 * metrics->CharInfo[c].BBox[3];
	*descent = -0.001 * metrics->CharInfo[c].BBox[1];
	wx = metrics->CharInfo[c].WX;
	if(wx == NA_SHORT) {
	    warning("font metrics unknown for character %d", c);
	    wx = 0;
	}
	*width = 0.001 * wx;
    }
}


/*  Part 2.  Graphics Support Code.  */

static char *TypeFaceDef[] = { "R", "B", "I", "BI", "S" };

static void PSEncodeFont(FILE *fp, char *encname)
{
    int i;

    /* include encoding unless it is ISOLatin1Encoding, which is predefined */
    if (strcmp(encname, "ISOLatin1Encoding")) 
	fprintf(fp, "%% begin encoding\n%s def\n%% end encoding\n", enccode);
    for (i = 0; i < 4 ; i++) {
	fprintf(fp, "/%s findfont\n", familyname[i]);
	fprintf(fp, "dup length dict begin\n");
	fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
	fprintf(fp, "  /Encoding %s def\n", encname);
	fprintf(fp, "  currentdict\n");
	fprintf(fp, "  end\n");
	fprintf(fp, "/Font%d exch definefont pop\n", i + 1);
    }
    fprintf(fp, "/Symbol findfont\n");
    fprintf(fp, "dup length dict begin\n");
    fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
    fprintf(fp, "  currentdict\n");
    fprintf(fp, "  end\n");
    fprintf(fp, "/Font5 exch definefont pop\n");
}

/* The variables "paperwidth" and "paperheight" give the dimensions */
/* of the (unrotated) printer page in points whereas the graphics */
/* region box is for the rotated page. */

static void PSFileHeader(FILE *fp, char* encname, 
			 char *papername, double paperwidth, 
			 double paperheight, Rboolean landscape,
			 int EPSFheader,
			 double left, double bottom, double right, double top)
{
    int i;
    SEXP prolog;

    if(EPSFheader)
	fprintf(fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
    else
	fprintf(fp, "%%!PS-Adobe-3.0\n");
    fprintf(fp, "%%%%DocumentNeededResources: font Symbol\n");
    for (i = 0; i < 4; i++)
	fprintf(fp, "%%%%+ font %s\n", familyname[i]);
    if(!EPSFheader)
	fprintf(fp, "%%%%DocumentMedia: %s %.0f %.0f 0 () ()\n",
		papername, paperwidth, paperheight);
    fprintf(fp, "%%%%Title: R Graphics Output\n");
    fprintf(fp, "%%%%Creator: R Software\n");
    fprintf(fp, "%%%%Pages: (atend)\n");
    if (landscape) {
	fprintf(fp, "%%%%Orientation: Landscape\n");
    }
    else {
	fprintf(fp, "%%%%Orientation: Portrait\n");
    }
    fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
	    left, bottom, right, top);
    fprintf(fp, "%%%%EndComments\n");
    fprintf(fp, "%%%%BeginProlog\n");
    if (landscape)
	fprintf(fp, "/bp  { gs %.2f 0 translate 90 rotate gs } def\n", paperwidth);
    else
	fprintf(fp, "/bp  { gs gs } def\n");
    prolog = findVar(install(".ps.prolog"), R_GlobalEnv);
    if(!isString(prolog))
	error("Object .ps.prolog is not a character vector");
    fprintf(fp, "%% begin .ps.prolog\n");
    for (i = 0; i < length(prolog); i++)
	fprintf(fp, "%s\n", CHAR(STRING_ELT(prolog, i)));
    fprintf(fp, "%% end   .ps.prolog\n");
    PSEncodeFont(fp, encname);
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
    fprintf(fp, "%.2f setlinewidth\n", linewidth);
}

static void PostScriptSetFont(FILE *fp, int typeface, double size)
{
    fprintf(fp, "/ps %.0f def %s %.0f s\n", size, TypeFaceDef[typeface], size);
}

static void PostScriptSetLineTexture(FILE *fp, int *lty, int nlty, double lwd)
{
    double dash;
    int i;
    fprintf(fp,"[");
    for (i = 0; i < nlty; i++) {
        dash = (lwd >= 1 ? lwd:1) * ((i % 2) ? lty[i] + 1 : lty[i] - 1);
	if (dash < 0) dash = 0;
	fprintf(fp," %.2f", dash);
    }
    fprintf(fp,"] 0 setdash\n");
}


static void PostScriptMoveTo(FILE *fp, double x, double y)
{
    fprintf(fp, "%.2f %.2f m\n", x, y);
}

static void PostScriptLineTo(FILE *fp, double x, double y)
{
    fprintf(fp, "%.2f %.2f l\n", x, y);
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
    fprintf(fp, "%.2f %.2f %.2f %.2f r ", x0, y0, x1, y1);
}

static void PostScriptCircle(FILE *fp, double x, double y, double r)
{
    fprintf(fp, "%.2f %.2f %.2f c ", x, y, r);
}

static void PostScriptWriteString(FILE *fp, char *str)
{
    fputc('(', fp);
    for ( ; *str; str++)
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

static void PostScriptText(FILE *fp, double x, double y,
			   char *str, double xc, double yc, double rot)
{
    fprintf(fp, "%.2f %.2f ", x, y);
    PostScriptWriteString(fp, str);
    fprintf(fp, " %.2f %.2f %.2f t\n", xc, yc, rot);
}


/* Part 3.  Device Driver State. */

typedef struct {
    char filename[PATH_MAX];
    int open_type;

    char papername[64];	/* paper name */
    int paperwidth;	/* paper width in big points (1/72 in) */
    int paperheight;	/* paper height in big points */
    Rboolean landscape;	/* landscape mode */
    int pageno;		/* page number */

    int fontfamily;	/* font family */
    char encpath[PATH_MAX]; /* font encoding file */
    char encname[100]; /* font encoding */
    char **afmpaths;	/* for user-specified family */
    int maxpointsize;

    double width;	/* plot width in inches */
    double height;	/* plot height in inches */
    double pagewidth;	/* page width in inches */
    double pageheight;	/* page height in inches */
    Rboolean pagecentre;/* centre image on page? */
    Rboolean printit;	/* print page at close? */
    char command[PATH_MAX];

    FILE *psfp;		/* output file */

    Rboolean onefile;	/* EPSF header etc*/

    /* This group of variables track the current device status.
     * They should only be set by routines that emit PostScript code. */
    struct {
	double lwd;		 /* line width */
	int lty;		 /* line type */
	int fontstyle;	         /* font style, R, B, I, BI, S */
	int fontsize;	         /* font size in points */
	rcolor col;		 /* color */
	rcolor fill;	         /* fill color */
	rcolor bg;		 /* color */
    } current;

    FontMetricInfo metrics[5];	/* font metrics */
}
PostScriptDesc;


/* Device Driver Actions */

static void   PS_Activate(DevDesc*);
static void   PS_Circle(double, double, int, double, int, int, DevDesc*);
static void   PS_Clip(double, double, double, double, DevDesc*);
static void   PS_Close(DevDesc*);
static void   PS_Deactivate(DevDesc*);
static void   PS_Hold(DevDesc*);
static void   PS_Line(double, double, double, double, int, DevDesc*);
static Rboolean PS_Locator(double*, double*, DevDesc*);
static void   PS_Mode(int, DevDesc*);
static void   PS_NewPage(DevDesc*);
static Rboolean PS_Open(DevDesc*, PostScriptDesc*);
static void   PS_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   PS_Polyline(int, double*, double*, int, DevDesc*);
static void   PS_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   PS_Resize(DevDesc*);
static double PS_StrWidth(char*, DevDesc*);
static void   PS_MetricInfo(int, double*, double*, double*, DevDesc*);
static void   PS_Text(double, double, int, char*, double, double, DevDesc*);



/* PostScript Support (formerly in PostScript.c) */

static void PostScriptSetCol(FILE *fp, double r, double g, double b)
{
	fprintf(fp,"%.4f %.4f %.4f rgb\n", r, g, b);
}

static void PostScriptSetFill(FILE *fp, double r, double g, double b)
{
	fprintf(fp,"/bg { %.4f %.4f %.4f } def\n", r, g, b);
}



/* Driver Support Routines */

static void SetColor(int, DevDesc*);
static void SetFill(int, DevDesc*);
static void SetFont(int, int, DevDesc*);
static void SetLineStyle(int newlty, double newlwd, DevDesc *dd);
static void Invalidate(DevDesc*);
static int  MatchFamily(char *name);


Rboolean
PSDeviceDriver(DevDesc *dd, char *file, char *paper, char *family,
	       char **afmpaths, char *encoding,
	       char *bg, char *fg,
	       double width, double height,
	       Rboolean horizontal, double ps,
	       Rboolean onefile, Rboolean pagecentre, 
	       Rboolean printit, char*cmd)
{
    /* If we need to bail out with some sort of "error"
       then we must free(dd) */

    double xoff, yoff, pointsize;
    rcolor setbg, setfg;

    PostScriptDesc *pd;

    /* Check and extract the device parameters */

    if(strlen(file) > PATH_MAX - 1) {
	free(dd);
	error("filename too long in postscript");
    }

    /* allocate new postscript device description */
    if (!(pd = (PostScriptDesc *) malloc(sizeof(PostScriptDesc))))
	return FALSE;

    /* from here on, if need to bail out with "error", must also */
    /* free(pd) */

    /* initialise postscript device description */
    strcpy(pd->filename, file);
    strcpy(pd->papername, paper);
    pd->fontfamily = strcmp(family, "User") ? MatchFamily(family) : USERAFM;
    strcpy(pd->encpath, encoding);
    pd->afmpaths = afmpaths;

    setbg = str2col(bg);
    setfg = str2col(fg);

    pd->width = width;
    pd->height = height;
    pd->landscape = horizontal;
    pointsize = floor(ps);
    if(setbg == NA_INTEGER && setfg == NA_INTEGER) {
	free(dd);
	free(pd);
	error("invalid foreground/background color (postscript)");
    }
    pd->printit = printit;
    strcpy(pd->command, cmd);
    if (printit && strlen(cmd) == 0)
	error("postscript(print.it=T) used with an empty print command");
    strcpy(pd->command, cmd);


    /* Deal with paper and plot size and orientation */

    if(!strcmp(pd->papername, "Default") ||
       !strcmp(pd->papername, "default")) {
	SEXP s = STRING_ELT(GetOption(install("papersize"), R_NilValue), 0);
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
	    !strcmp(pd->papername, "letter")) {
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
    }
    else {
	free(dd);
	free(pd);
	error("invalid page type `%s' (postscript)", pd->papername);
    }
    pd->pagecentre = pagecentre;
    pd->paperwidth = 72 * pd->pagewidth;
    pd->paperheight = 72 * pd->pageheight;
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
    pd->maxpointsize = 72.0 * ((pd->pageheight > pd->pagewidth) ?
			       pd->pageheight : pd->pagewidth);
    pd->pageno = 0;
    dd->dp.lty = 0;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->dp.bg = setbg;
    dd->dp.fg = dd->dp.col = setfg;
    dd->dp.left = 72 * xoff;			/* left */
    dd->dp.right = 72 * (xoff + pd->width);	/* right */
    dd->dp.bottom = 72 * yoff;			/* bottom */
    dd->dp.top = 72 * (yoff + pd->height);	/* top */

    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */
    /* Only right for 12 point font. */
    /* Max pointsize suggested by Peter Dalgaard */

    if(pointsize < 6.0) pointsize = 6.0;
    if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
    dd->dp.ps = pointsize;
    dd->dp.font = 1;
    dd->dp.cra[0] = 0.9 * pointsize;
    dd->dp.cra[1] = 1.2 * pointsize;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->dp.xCharOffset =  0.4900;
    dd->dp.yCharOffset =  0.3333;
    dd->dp.yLineBias = 0.2;

    /* Inches per Raster Unit */
    /* We use points (72 dots per inch) */

    dd->dp.ipr[0] = 1.0/72.0;
    dd->dp.ipr[1] = 1.0/72.0;
    /* GREset(.)  dd->gp.mkh = dd->gp.cra[0] * dd->gp.ipr[0]; */

    dd->dp.canResizePlot = 0;
    dd->dp.canChangeFont = 1;
    dd->dp.canRotateText = 1;
    dd->dp.canResizeText = 1;
    dd->dp.canClip = 1;
    dd->dp.canHAdj = 2;

    /*	Start the driver */

    pd->pageno = 0;
    if(!PS_Open(dd, pd)) {
	free(pd);
	return FALSE;
    }

    dd->dp.open	      = PS_Open;
    dd->dp.close      = PS_Close;
    dd->dp.activate   = PS_Activate;
    dd->dp.deactivate = PS_Deactivate;
    dd->dp.resize     = PS_Resize;
    dd->dp.newPage    = PS_NewPage;
    dd->dp.clip	      = PS_Clip;
    dd->dp.text	      = PS_Text;
    dd->dp.strWidth   = PS_StrWidth;
    dd->dp.metricInfo = PS_MetricInfo;
    dd->dp.rect	      = PS_Rect;
    dd->dp.circle     = PS_Circle;
    dd->dp.line	      = PS_Line;
    dd->dp.polygon    = PS_Polygon;
    dd->dp.polyline   = PS_Polyline;
    dd->dp.locator    = PS_Locator;
    dd->dp.mode	      = PS_Mode;
    dd->dp.hold	      = PS_Hold;

    dd->deviceSpecific = (void *) pd;
    dd->displayListOn = FALSE;
    return TRUE;
}

static int MatchFamily(char *name)
{
    int i;
    for(i = 0; Family[i].family != NULL; i++)
	if(!strcmp(name, Family[i].family)) return i;
    warning("unknown postscript font family, using %s",
	    Family[3].family);
    return 3;
}

static void SetColor(int color, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(color != pd->current.col) {
	PostScriptSetCol(pd->psfp,
			 R_RED(color)/255.0,
			 R_GREEN(color)/255.0,
			 R_BLUE(color)/255.0);
	pd->current.col = color;
    }
}

static void SetFill(int color, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(color != pd->current.fill) {
	PostScriptSetFill(pd->psfp,
			  R_RED(color)/255.0,
			  R_GREEN(color)/255.0,
			  R_BLUE(color)/255.0);
	pd->current.fill = color;
    }
}

/* Note that the line texture is scaled by the line width. */

static void SetLineStyle(int newlty, double newlwd, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int i, ltyarray[8];

    if (pd->current.lty != newlty || pd->current.lwd != newlwd) {
	pd->current.lwd = newlwd;
	pd->current.lty = newlty;
	PostScriptSetLineWidth(pd->psfp, newlwd * 0.75);
	/* process lty : */
	for(i = 0; i < 8 && newlty & 15 ; i++) {
	    ltyarray[i] = newlty & 15;
	    newlty = newlty >> 4;
	}
	PostScriptSetLineTexture(pd->psfp, ltyarray, i, newlwd * 0.75);
    }
}

static void SetFont(int style, int size, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(style < 1 || style > 5)
	style = 1;
    if(size < 1 || size > pd->maxpointsize)
	size = 10;
    if(size != pd->current.fontsize || style != pd->current.fontstyle) {
	PostScriptSetFont(pd->psfp, style-1, size);
	pd->current.fontsize = size;
	pd->current.fontstyle = style;
    }
}

static Rboolean PS_Open(DevDesc *dd, PostScriptDesc *pd)
{
    char buf[512], *p;
    int i;

    if (!LoadEncoding(pd->encpath, pd->encname, FALSE)) {
	warning("problem loading encoding file");
	return FALSE;
    }
    for(i = 0; i < 4 ; i++) {
	if(pd->fontfamily == USERAFM) p = pd->afmpaths[i];
	else p = Family[pd->fontfamily].afmfile[i];
	if(!PostScriptLoadFontMetrics(p, &(pd->metrics[i]),
				      familyname[i], 1)) {
	    warning("cannot read afm file %s", p);
	    return FALSE;
	}
    }
    if(!PostScriptLoadFontMetrics("sy______.afm", &(pd->metrics[4]),
				  familyname[4], 0)) {
	warning("cannot read afm file sy______.afm");
	return FALSE;
    }

    if (strlen(pd->filename) == 0) {
#ifndef HAVE_POPEN
	warning("printing via file = \"\" is not implemented in this version");
	return FALSE;
#else
	if(strlen(pd->command) == 0) return FALSE;
	pd->psfp = popen(pd->command, "w");
	pd->open_type = 1;
#endif
    } else if (pd->filename[0] == '|') {
#ifndef HAVE_POPEN
	warning("file = \"|cmd\" is not implemented in this version");
	return FALSE;
#else
	pd->psfp = popen(pd->filename + 1, "w");
	pd->open_type = 1;
	if (!pd->psfp) {
	    warning("cannot open `postscript' pipe to `%s'", pd->filename + 1);
	    return FALSE;
	}
#endif
    } else {
	sprintf(buf, pd->filename, pd->pageno + 1); /* page 1 to start */
	pd->psfp = R_fopen(R_ExpandFileName(buf), "w");
	pd->open_type = 0;
    }
    if (!pd->psfp) {
	warning("cannot open `postscript' file argument `%s'", buf);
	return FALSE;
    }

    if(pd->landscape)
	PSFileHeader(pd->psfp,
		     pd->encname,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     !(pd->onefile),
		     dd->dp.bottom,
		     dd->dp.left,
		     dd->dp.top,
		     dd->dp.right);
    else
	PSFileHeader(pd->psfp,
		     pd->encname,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     !(pd->onefile),
		     dd->dp.left,
		     dd->dp.bottom,
		     dd->dp.right,
		     dd->dp.top);

    return TRUE;
}

/* The driver keeps track of the current values of colors, fonts and
   line parameters, to save emitting some PostScript. In some cases,
   the state becomes unknown, notably after changing the clipping and
   at the start of a new page, so we have the following routine to
   invalidate the saved values, which in turn causes the parameters to
   be set before usage. */

static void Invalidate(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    pd->current.fontsize = -1;
    pd->current.fontstyle = -1;
    pd->current.lwd = -1;
    pd->current.lty = -1;
    pd->current.col = 0xffffffff;
    pd->current.fill = 0xffffffff;
    pd->current.bg = 0xffffffff;
}

static void PS_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptSetClipRect(pd->psfp, x0, x1, y0, y1);
    /* clipping does grestore so invalidate monitor variables */
    Invalidate(dd);
}

static void PS_Resize(DevDesc *dd)
{
}

static void PostScriptClose(DevDesc *dd);

static void PS_NewPage(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;


    if(pd->onefile) {
	if(++pd->pageno > 1) PostScriptEndPage(pd->psfp);
    } else if(pd->pageno > 0) {
	PostScriptClose(dd);
	PS_Open(dd, pd);
	pd->pageno++;
    } else pd->pageno++;
    PostScriptStartPage(pd->psfp, pd->pageno);
    Invalidate(dd);

    if(dd->gp.bg != R_RGB(255,255,255)) {
	PS_Rect(0, 0, 72.0 * pd->pagewidth, 72.0 * pd->pageheight,
		DEVICE, dd->gp.bg, NA_INTEGER, dd);
    }
}

#ifdef Win32
int   runcmd(char *cmd, int wait, int visible, char *finput);
#endif
static void PostScriptClose(DevDesc *dd)
{
    char buff[PATH_MAX];
    int err = 0;

    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptFileTrailer(pd->psfp, pd->pageno);
    if(pd->open_type == 1)
	pclose(pd->psfp);
    else {
	fclose(pd->psfp);
	if (pd->printit) {
	    strcpy(buff, pd->command);
	    strcat(buff, " ");
	    strcat(buff, pd->filename);
/*	    Rprintf("buff is %s\n", buff); */
#ifdef Unix
	    err = system(buff);
#endif
#ifdef Win32
	    err = runcmd(buff, 0, 0, NULL);
#endif
	    if (err)
		warning("error from postscript() in running:\n    %s", buff);
	}
    }
}

static void PS_Close(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptClose(dd);
    free(pd);
}

static void PS_Activate(DevDesc *dd) {}
static void PS_Deactivate(DevDesc *dd) {}

static double PS_StrWidth(char *str, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int face = dd->gp.font;
    
    if(face < 1 || face > 5) face = 1;
    return floor(dd->gp.cex * dd->gp.ps + 0.5) *
	PostScriptStringWidth((unsigned char *)str,
			      &(pd->metrics[face-1]));
}

static void PS_MetricInfo(int c, double *ascent, double *descent,
			  double *width, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptMetricInfo(c, ascent, descent, width,
			 &(pd->metrics[dd->gp.font-1]));
    *ascent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *ascent;
    *descent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *descent;
    *width = floor(dd->gp.cex * dd->gp.ps + 0.5) * *width;
}

#ifdef NOT_used_currently/*-- out 'def'	 (-Wall) --*/
static void PS_MoveTo(double x, double y, int coords, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    GConvert(&x, &y, coords, DEVICE, dd);
    PostScriptMoveTo(pd->psfp, x, y);
}

static void PS_StartPath(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    SetColor(dd->gp.col, dd);
    SetLineStyle(dd->gp.lty, dd->lwd, dd);
    PostScriptStartPath(pd->psfp);
}

static void PS_EndPath(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    PostScriptEndPath(pd->psfp);
}
#endif

static void PS_Rect(double x0, double y0, double x1, double y1, int coords,
		    int bg, int fg, DevDesc *dd)
{
    int code;
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);

    if (code) {
	if(code & 2)
	    SetFill(bg, dd);
	if(code & 1) {
	    SetColor(fg, dd);
	    SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	GConvert(&x0, &y0, coords, DEVICE, dd);
	GConvert(&x1, &y1, coords, DEVICE, dd);
	PostScriptRectangle(pd->psfp, x0, y0, x1, y1);
	fprintf(pd->psfp, "p%d\n", code);
    }
}

static void PS_Circle(double x, double y, int coords, double r,
		      int bg, int fg, DevDesc *dd)
{
    int code;
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);

    if (code) {
	if(code & 2)
	    SetFill(bg, dd);
	if(code & 1) {
	    SetColor(fg, dd);
	    SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	GConvert(&x, &y, coords, DEVICE, dd);
	PostScriptCircle(pd->psfp, x, y, r);
	fprintf(pd->psfp, "p%d\n", code);
    }
}

static void PS_Line(double x1, double y1, double x2, double y2,
		    int coords, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);
    /* FIXME : clip to the device extents here */
    SetColor(dd->gp.col, dd);
    SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
    PostScriptStartPath(pd->psfp);
    PostScriptMoveTo(pd->psfp, x1, y1);
    PostScriptLineTo(pd->psfp, x2, y2);
    PostScriptEndPath(pd->psfp);
}

static void PS_Polygon(int n, double *x, double *y, int coords,
		       int bg, int fg, DevDesc *dd)
{
    PostScriptDesc *pd;
    double xx, yy;
    int i, code;

    pd = (PostScriptDesc *) dd->deviceSpecific;

    /* code is set as follows */
    /* code == 0, nothing to draw */
    /* code == 1, outline only */
    /* code == 2, fill only */
    /* code == 3, outline and fill */

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);

    if (code) {
	if(code & 2)
	    SetFill(bg, dd);
	if(code & 1) {
	    SetColor(fg, dd);
	    SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	fprintf(pd->psfp, "np\n");
	xx = x[0];
	yy = y[0];
	GConvert(&xx, &yy, coords, DEVICE, dd);
	fprintf(pd->psfp, "  %.2f %.2f m\n", xx, yy);
	for(i = 1 ; i < n ; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, coords, DEVICE, dd);
	    fprintf(pd->psfp, "	 %.2f %.2f l\n", xx, yy);
	}
	fprintf(pd->psfp, "cp p%d\n", code);
    }
}

static void PS_Polyline(int n, double *x, double *y, int coords,
			DevDesc *dd)
{
    PostScriptDesc *pd;
    double xx, yy;
    int i;

    pd = (PostScriptDesc*) dd->deviceSpecific;
    SetColor(dd->gp.col, dd);
    SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
    fprintf(pd->psfp, "np\n");
    xx = x[0];
    yy = y[0];
    GConvert(&xx, &yy, coords, DEVICE, dd);
    fprintf(pd->psfp, "%.2f %.2f m\n", xx, yy);
    for(i = 1 ; i < n ; i++) {
	xx = x[i];
	yy = y[i];
	GConvert(&xx, &yy, coords, DEVICE, dd);
	fprintf(pd->psfp, "%.2f %.2f l\n", xx, yy);
    }
    fprintf(pd->psfp, "o\n");
}

static void PS_Text(double x, double y, int coords,
		    char *str, double rot, double hadj, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    GConvert(&x, &y, coords, DEVICE, dd);
    SetFont(dd->gp.font, (int)floor(dd->gp.cex * dd->gp.ps + 0.5), dd);
    SetColor(dd->gp.col, dd);
    PostScriptText(pd->psfp, x, y, str, hadj, 0.0, rot);
}

static Rboolean PS_Locator(double *x, double *y, DevDesc *dd)
{
    return FALSE;
}

static void PS_Mode(int mode, DevDesc* dd)
{
}

static void PS_Hold(DevDesc *dd)
{
}



/***********************************************************************

                 XFig driver shares font handling

************************************************************************/



typedef struct {
    char filename[PATH_MAX];

    char papername[64];	 /* paper name */
    int paperwidth;	 /* paper width in big points (1/72 in) */
    int paperheight;	 /* paper height in big points */
    Rboolean landscape;	 /* landscape mode */
    int pageno;		 /* page number */

    int fontfamily;	 /* font family */
    int fontnum;	 /* font number in XFig */
    int fontstyle;	 /* font style, R, B, I, BI, S */
    int fontsize;	 /* font size in points */
    int maxpointsize;

    double width;	 /* plot width in inches */
    double height;	 /* plot height in inches */
    double pagewidth;	 /* page width in inches */
    double pageheight;	 /* page height in inches */
    Rboolean pagecentre;      /* centre image on page? */

    double lwd;		 /* current line width */
    int lty;		 /* current line type */
    rcolor col;		 /* current color */
    rcolor fill;	 /* current fill color */
    rcolor bg;		 /* background color */
    int XFigColors[534]; int nXFigColors;

    FILE *psfp;		 /* output file */
    FILE *tmpfp;         /* temp file */
    char tmpname[PATH_MAX];

    Rboolean onefile;
    int ymax;            /* used to invert coord system */

    FontMetricInfo metrics[5];	/* font metrics */

} XFigDesc;

static void
XF_FileHeader(FILE *fp, char *papername, Rboolean landscape, Rboolean onefile)
{
    fprintf(fp, "#FIG 3.2\n");
    fprintf(fp, landscape ? "Landscape\n" : "Portrait\n");
    fprintf(fp, "Flush Left\nInches\n");
    /* Fix */fprintf(fp, "%s\n", papername);
    fprintf(fp, "100.0\n");
    fprintf(fp, onefile ? "Multiple\n" : "Single\n");
    fprintf(fp, "-2\n"); /* no background */
    fprintf(fp, "1200 2\n"); /* coordinate system */
    fprintf(fp, "# End of XFig header\n");
}

static void XF_FileTrailer(FILE *fp)
{
    fprintf(fp, "# end of XFig file\n");
}


static void XF_EndPage(FILE *fp)
{
    fprintf(fp, "# end of XFig page\n");
}

static void XF_WriteString(FILE *fp, char *str)
{
    unsigned int c;
    for ( ; *str; str++) {
	c = (unsigned char)*str;
	if (c > 127) {
	    fprintf(fp, "\\%o", c);
	} else {
	    switch(*str) {
	    case '\n':
		fprintf(fp, "\\n");
		break;
	    case '\\':
		fprintf(fp, "\\\\");
		break;
	    default:
		fputc(*str, fp);
		break;
	    }
	}
    }
}

static int XF_SetColor(int color, XFigDesc *pd)
{
    int i;
    if (color <0 || color > 0xffffff) return -1;
    for (i = 0; i < pd->nXFigColors; i++)
    {
	if(color == pd->XFigColors[i]) return i;
    }
    if(pd->nXFigColors == 534) {
	error("run out of colors in xfig()");
    }
    /* new colour */
    fprintf(pd->psfp, "0 %d #%02x%02x%02x\n", pd->nXFigColors,
	    R_RED(color), R_GREEN(color), R_BLUE(color));
    pd->XFigColors[pd->nXFigColors] = color;
    return pd->nXFigColors++;
}

static void XFconvert(double *x, double *y, XFigDesc *pd)
{
    (*x) *= 16.667;
    (*y) = pd->ymax - 16.667*(*y);
}


static int XF_SetLty(int lty)
{
    switch(lty) {
    case LTY_SOLID:
    case 1: /* "solid" */
	return 0;
    case LTY_DASHED:
	return 1;
    case LTY_DOTTED:
	return 2;
    case LTY_DOTDASH:
	return 3;
    default:
	warning("unimplemented line texture %u: using Dash-double-dotted",
		lty);
	return 4;
    }
}

/* Device Driver Actions */

static void   XFig_Activate(DevDesc*);
static void   XFig_Circle(double, double, int, double, int, int, DevDesc*);
static void   XFig_Clip(double, double, double, double, DevDesc*);
static void   XFig_Close(DevDesc*);
static void   XFig_Deactivate(DevDesc*);
static void   XFig_Hold(DevDesc*);
static void   XFig_Line(double, double, double, double, int, DevDesc*);
static Rboolean XFig_Locator(double*, double*, DevDesc*);
static void   XFig_Mode(int, DevDesc*);
static void   XFig_NewPage(DevDesc*);
static Rboolean XFig_Open(DevDesc*, XFigDesc*);
static void   XFig_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   XFig_Polyline(int, double*, double*, int, DevDesc*);
static void   XFig_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   XFig_Resize(DevDesc*);
static double XFig_StrWidth(char*, DevDesc*);
static void   XFig_MetricInfo(int, double*, double*, double*, DevDesc*);
static void   XFig_Text(double, double, int, char*, double, double, DevDesc*);

static int XFig_basenums[] = {4, 8, 12, 16, 20, 24, 28, 0};


/* Driver Support Routines */

Rboolean
XFigDeviceDriver(DevDesc *dd, char *file, char *paper, char *family,
		 char *bg, char *fg,
		 double width, double height,
		 Rboolean horizontal, double ps,
		 Rboolean onefile, Rboolean pagecentre)
{
    /* If we need to bail out with some sort of "error" */
    /* then we must free(dd) */

    double xoff, yoff, pointsize;
    XFigDesc *pd;

    /* Check and extract the device parameters */

    if(strlen(file) > PATH_MAX - 1) {
	free(dd);
	error("filename too long in xfig");
    }

    /* allocate new xfig device description */
    if (!(pd = (XFigDesc *) malloc(sizeof(XFigDesc))))
	return 0;

    /* from here on, if need to bail out with "error", must also */
    /* free(pd) */

    /* initialize xfig device description */
    strcpy(pd->filename, file);
    strcpy(pd->papername, paper);
    pd->fontfamily = MatchFamily(family);
    pd->fontnum = XFig_basenums[pd->fontfamily];
    pd->bg = str2col(bg);
    pd->col = str2col(fg);
    pd->fill = NA_INTEGER;
    pd->width = width;
    pd->height = height;
    pd->landscape = horizontal;
    pointsize = floor(ps);
    if(pd->bg == NA_INTEGER && pd->col == NA_INTEGER) {
	free(dd);
	free(pd);
	error("invalid foreground/background color (xfig)");
    }


    /* Deal with paper and plot size and orientation */

    if(!strcmp(pd->papername, "Default") ||
       !strcmp(pd->papername, "default")) {
	SEXP s = STRING_ELT(GetOption(install("papersize"), R_NilValue), 0);
	if(s != NA_STRING && strlen(CHAR(s)) > 0)
	    strcpy(pd->papername, CHAR(s));
	else strcpy(pd->papername, "A4");
    }
    if(!strcmp(pd->papername, "A4") ||
       !strcmp(pd->papername, "a4")) {
	strcpy(pd->papername, "A4");
	pd->pagewidth  = 21.0 / 2.54;
	pd->pageheight = 29.7 / 2.54;
    }
    else if(!strcmp(pd->papername, "Letter") ||
	    !strcmp(pd->papername, "letter")) {
	strcpy(pd->papername, "Letter");
	pd->pagewidth  =  8.5;
	pd->pageheight = 11.0;
    }
    else if(!strcmp(pd->papername, "Legal") ||
	    !strcmp(pd->papername, "legal")) {
	strcpy(pd->papername, "Legal");
	pd->pagewidth  =  8.5;
	pd->pageheight = 14.0;
    }
    else {
	free(dd);
	free(pd);
	error("invalid page type `%s' (xfig)", pd->papername);
    }
    pd->pagecentre = pagecentre;
    pd->paperwidth = 72 * pd->pagewidth;
    pd->paperheight = 72 * pd->pageheight;
    if(!onefile) {
	char *p = strrchr(pd->filename, '%');
	if(!p)
	    warning("xfig(%s, onefile=FALSE) will only return the last plot", pd->filename);
    }
    if(pd->landscape) {
	double tmp;
	tmp = pd->pagewidth;
	pd->pagewidth = pd->pageheight;
	pd->pageheight = tmp;
    }
    if(pd->width < 0.1 || pd->width > pd->pagewidth-0.5)
	pd->width = pd->pagewidth-0.5;
    if(pd->height < 0.1 || pd->height > pd->pageheight-0.5)
	pd->height = pd->pageheight-0.5;
    if(pagecentre) {
	xoff = (pd->pagewidth - pd->width)/2.0;
	yoff = (pd->pageheight - pd->height)/2.0;
    } else {
	xoff = yoff = 0.0;
    }
    if(pagecentre)
	pd->ymax = (int)(1200.0 * pd->pageheight);
    else
	pd->ymax = (int)(1200.0 * pd->height);
    pd->onefile = onefile;
    pd->maxpointsize = 72.0 * ((pd->pageheight > pd->pagewidth) ?
			       pd->pageheight : pd->pagewidth);
    pd->pageno = 0;
    pd->lty = 1;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->dp.bg = pd->bg;
    dd->dp.fg = dd->dp.col = pd->col;
    dd->dp.left = 72 * xoff;			/* left */
    dd->dp.right = 72 * (xoff + pd->width);	/* right */
    dd->dp.bottom = 72 * yoff;		/* bottom */
    dd->dp.top = 72 * (yoff + pd->height);	/* top */

    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */
    /* Only right for 12 point font. */
    /* Max pointsize suggested by Peter Dalgaard */

    if(pointsize < 6.0) pointsize = 6.0;
    if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
    dd->dp.ps = pointsize;
    dd->dp.cra[0] = 0.9 * pointsize;
    dd->dp.cra[1] = 1.2 * pointsize;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->dp.xCharOffset =  0.4900;
    dd->dp.yCharOffset =  0.3333;
    dd->dp.yLineBias = 0.2;

    /* Inches per Raster Unit */
    /* 1200 dpi */
    dd->dp.ipr[0] = 1.0/72.0;
    dd->dp.ipr[1] = 1.0/72.0;

    dd->dp.canResizePlot = 0;
    dd->dp.canChangeFont = 1;
    dd->dp.canRotateText = 1;
    dd->dp.canResizeText = 1;
    dd->dp.canClip = 0;
    dd->dp.canHAdj = 1; /* 0, 0.5, 1 */

    pd->XFigColors[7] = 0xffffff;
    pd->nXFigColors = 32;

    /*	Start the driver */

    if(!XFig_Open(dd, pd)) {
	free(pd);
	return 0;
    }

    dd->dp.open	      = XFig_Open;
    dd->dp.close      = XFig_Close;
    dd->dp.activate   = XFig_Activate;
    dd->dp.deactivate = XFig_Deactivate;
    dd->dp.resize     = XFig_Resize;
    dd->dp.newPage    = XFig_NewPage;
    dd->dp.clip	      = XFig_Clip;
    dd->dp.text	      = XFig_Text;
    dd->dp.strWidth   = XFig_StrWidth;
    dd->dp.metricInfo = XFig_MetricInfo;
    dd->dp.rect	      = XFig_Rect;
    dd->dp.circle     = XFig_Circle;
    dd->dp.line	      = XFig_Line;
    dd->dp.polygon    = XFig_Polygon;
    dd->dp.polyline   = XFig_Polyline;
    dd->dp.locator    = XFig_Locator;
    dd->dp.mode	      = XFig_Mode;
    dd->dp.hold	      = XFig_Hold;

    dd->deviceSpecific = (void *) pd;
    dd->displayListOn = FALSE;
    return 1;
}

#ifdef Unix
char * Runix_tmpnam(char * prefix);
#endif
#ifdef Win32
char * Rwin32_tmpnam(char * prefix);
#endif
#ifdef Macintosh
char * Rmac_tmpnam(char * prefix);
#endif

static Rboolean XFig_Open(DevDesc *dd, XFigDesc *pd)
{
    char buf[512], name[50];
    int i;

    if (!LoadEncoding("ISOLatin1.enc", buf, FALSE))
	error("problem loading encoding file");
    for(i = 0; i < 4 ; i++) {
	if(!PostScriptLoadFontMetrics(Family[pd->fontfamily].afmfile[i],
				      &(pd->metrics[i]), name, 1)) {
	    warning("cannot read afm file %s",
		    Family[pd->fontfamily].afmfile[i]);
	    return FALSE;
	}
    }
    if(!PostScriptLoadFontMetrics("sy______.afm",
				  &(pd->metrics[4]), name, 0)) {
	warning("cannot read afm file sy______.afm");
	return FALSE;
    }

    if (strlen(pd->filename) == 0) {
	error("empty file name");
	return FALSE;
    } else {
	sprintf(buf, pd->filename, pd->pageno + 1); /* page 1 to start */
	pd->psfp = R_fopen(R_ExpandFileName(buf), "w");
    }
    if (!pd->psfp) return FALSE;
#ifdef Unix
    strcpy(pd->tmpname, Runix_tmpnam("Rxfig"));
#endif
#ifdef Win32
    strcpy(pd->tmpname, Rwin32_tmpnam("Rxfig"));
#endif
#ifdef Macintosh
    strcpy(pd->tmpname, Rmac_tmpnam("Rxfig"));
#endif
    pd->tmpfp = R_fopen(pd->tmpname, "w");
    if (!pd->tmpfp) {
	fclose(pd->psfp);
	return FALSE;
    }
    XF_FileHeader(pd->psfp, pd->papername, pd->landscape, pd->onefile);
    pd->fontstyle = 1;
    pd->fontsize = 10;
    pd->pageno = 0;
    return TRUE;
}


static void XFig_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
}

static void XFig_Resize(DevDesc *dd)
{
}

#define CHUNK 10000
static void XFig_NewPage(DevDesc *dd)
{
    char buf[PATH_MAX];
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;

    pd->pageno++;
    if(pd->onefile) {
	fprintf(pd->tmpfp, "#Start of page %d\n", pd->pageno);
	if(pd->pageno > 1) XF_EndPage(pd->tmpfp);
    } else {
	char buffer[CHUNK];
	size_t nread;
	if(pd->pageno == 1) return;
	XF_FileTrailer(pd->tmpfp);
	fclose(pd->tmpfp);
	pd->tmpfp = R_fopen(pd->tmpname, "r");
	while(1) {
	    nread = fread(buffer, 1, CHUNK, pd->tmpfp);
	    if(nread > 0) fwrite(buffer, 1, nread, pd->psfp);
	    if(nread < CHUNK) break;
	}
	fclose(pd->tmpfp);
	fclose(pd->psfp);
	sprintf(buf, pd->filename, pd->pageno);
	pd->psfp = R_fopen(R_ExpandFileName(buf), "w");
	pd->tmpfp = R_fopen(pd->tmpname, "w");
	XF_FileHeader(pd->psfp, pd->papername, pd->landscape, pd->onefile);
	pd->XFigColors[7] = 0xffffff;
	pd->nXFigColors = 32;
    }
     if(dd->dp.bg != R_RGB(255,255,255)) {
	 int cbg = XF_SetColor(dd->dp.bg, pd);
	 int ix0, iy0, ix1, iy1;
	 double x0 = 0.0, y0 = 0.0, x1 = 72.0 * pd->pagewidth,
	     y1 = 72.0 * pd->pageheight;
	 XFconvert(&x0, &y0, pd); XFconvert(&x1, &y1, pd);
	 ix0 = (int)x0; iy0 = (int)y0; ix1 = (int)x1; iy1 = (int)y1;
	 fprintf(fp, "2 2 "); /* Polyline */
	 fprintf(fp, "%d %d ", 0, 0); /* style, thickness */
	 fprintf(fp, "%d %d ", cbg, cbg); /* pen colour fill colour */
	 fprintf(fp, "200 0 20 4.0 0 0 -1 0 0 ");
	 fprintf(fp, "%d\n", 5); /* number of points */
	 fprintf(fp, "%d %d ", ix0, iy0);
	 fprintf(fp, "%d %d ", ix0, iy1);
	 fprintf(fp, "%d %d ", ix1, iy1);
	 fprintf(fp, "%d %d ", ix1, iy0);
	 fprintf(fp, "%d %d\n", ix0, iy0);
   }
}

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

static void XFig_Close(DevDesc *dd)
{
    char buf[CHUNK];
    size_t nread;
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;

    XF_FileTrailer(pd->tmpfp);
    fclose(pd->tmpfp);
    pd->tmpfp = R_fopen(pd->tmpname, "r");
    while(1) {
	nread = fread(buf, 1, CHUNK, pd->tmpfp);
	if(nread > 0) fwrite(buf, 1, nread, pd->psfp);
	if(nread < CHUNK) break;
    }
    fclose(pd->tmpfp);
    unlink(pd->tmpname);
    fclose(pd->psfp);
    free(pd);
}

static void XFig_Activate(DevDesc *dd) {}
static void XFig_Deactivate(DevDesc *dd) {}

static void XFig_Rect(double x0, double y0, double x1, double y1, int coords,
		      int bg, int fg, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    int ix0, iy0, ix1, iy1;
    int cbg = XF_SetColor(bg, pd), cfg = XF_SetColor(fg, pd), cpen,
	dofill, lty = XF_SetLty(dd->gp.lty), lwd = dd->gp.lwd*0.833 + 0.5;

    cpen = (fg != NA_INTEGER)? cfg: -1;
    dofill = (bg != NA_INTEGER)? 20: -1;

    GConvert(&x0, &y0, coords, DEVICE, dd); XFconvert(&x0, &y0, pd);
    GConvert(&x1, &y1, coords, DEVICE, dd); XFconvert(&x1, &y1, pd);
    ix0 = (int)x0; iy0 = (int)y0; ix1 = (int)x1; iy1 = (int)y1;
    fprintf(fp, "2 2 "); /* Polyline */
    fprintf(fp, "%d %d ", lty, lwd>0?lwd:1); /* style, thickness */
    fprintf(fp, "%d %d ", cpen, cbg); /* pen colour fill colour */
    fprintf(fp, "100 0 %d ", dofill); /* depth, pen style, area fill */
    fprintf(fp, "%.2f 0 0 -1 0 0 ", 4.0*lwd); /* style value, join .... */
    fprintf(fp, "%d\n", 5); /* number of points */
    fprintf(fp, "  %d %d ", ix0, iy0);
    fprintf(fp, "  %d %d ", ix0, iy1);
    fprintf(fp, "  %d %d ", ix1, iy1);
    fprintf(fp, "  %d %d ", ix1, iy0);
    fprintf(fp, "  %d %d\n", ix0, iy0);
}

static void XFig_Circle(double x, double y, int coords, double r,
		      int bg, int fg, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    int ix, iy, ir;
    int cbg = XF_SetColor(bg, pd), cfg = XF_SetColor(fg, pd), cpen,
	dofill, lty = XF_SetLty(dd->gp.lty), lwd = dd->gp.lwd*0.833 + 0.5;

    cpen = (fg != NA_INTEGER)? cfg: -1;
    dofill = (bg != NA_INTEGER)? 20: -1;

    GConvert(&x, &y, coords, DEVICE, dd); XFconvert(&x, &y, pd);
    ix = (int)x; iy = (int)y; ir = (int)(16.667*r);

    fprintf(fp, "1 3 "); /* Circle + radius */
    fprintf(fp, "%d %d ", lty, lwd>0?lwd:1); /* style, thickness */
    fprintf(fp, "%d %d ", cpen, cbg); /* pen colour fill colour */
    fprintf(fp, "100 0 %d ", dofill); /* depth, pen style, area fill */
    fprintf(fp, "%.2f 1 0 ", 4.0*lwd); /* style value, direction, x, angle */
    fprintf(fp, "  %d %d %d %d %d %d %d %d \n",
	    ix, iy, ir, ir, ix, iy, ix+ir, iy);
}

static void XFig_Line(double x1, double y1, double x2, double y2,
		    int coords, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    int lty = XF_SetLty(dd->gp.lty), lwd = dd->gp.lwd*0.833 + 0.5;

    GConvert(&x1, &y1, coords, DEVICE, dd); XFconvert(&x1, &y1, pd);
    GConvert(&x2, &y2, coords, DEVICE, dd); XFconvert(&x2, &y2, pd);
    fprintf(fp, "2 1 "); /* Polyline */
    fprintf(fp, "%d %d ", lty, lwd>0?lwd:1); /* style, thickness */
    fprintf(fp, "%d %d ", XF_SetColor(dd->gp.col, pd), 7);
      /* pen colour fill colour */
    fprintf(fp, "100 0 -1 "); /* depth, pen style, area fill */
    fprintf(fp, "%.2f 0 0 -1 0 0 ", 4.0*lwd); /* style value, join .... */
    fprintf(fp, "%d\n", 2); /* number of points */
    fprintf(fp, "%d %d %d %d\n", (int)x1, (int)y1, (int)x2, (int)y2);
}

static void XFig_Polygon(int n, double *x, double *y, int coords,
			 int bg, int fg, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    double xx, yy;
    int i;
    int cbg = XF_SetColor(bg, pd), cfg = XF_SetColor(fg, pd), cpen,
	dofill, lty = XF_SetLty(dd->gp.lty), lwd = dd->gp.lwd*0.833 + 0.5;


    cpen = (fg != NA_INTEGER)? cfg: -1;
    dofill = (bg != NA_INTEGER)? 20: -1;

    fprintf(fp, "2 3 "); /* Polyline */
    fprintf(fp, "%d %d ", lty, lwd>0?lwd:1); /* style, thickness */
    fprintf(fp, "%d %d ", cpen, cbg); /* pen colour fill colour */
    fprintf(fp, "100 0 %d ", dofill); /* depth, pen style, area fill */
    fprintf(fp, "%.2f 0 0 -1 0 0 ", 4.0*lwd); /* style value, join .... */
    fprintf(fp, "%d\n", n+1); /* number of points */
    /* close the path */
    for(i = 0 ; i <= n ; i++) {
	xx = x[i%n];
	yy = y[i%n];
	GConvert(&xx, &yy, coords, DEVICE, dd); XFconvert(&xx, &yy, pd);
	fprintf(fp, "  %d %d\n", (int)xx, (int)yy);
    }
}

static void XFig_Polyline(int n, double *x, double *y, int coords,
			DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc*) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    double xx, yy;
    int i, lty = XF_SetLty(dd->gp.lty), lwd = dd->gp.lwd*0.833 + 0.5;

    fprintf(fp, "2 1 "); /* Polyline */
    fprintf(fp, "%d %d ", lty, lwd>0?lwd:1); /* style, thickness */
    fprintf(fp, "%d %d ", XF_SetColor(dd->gp.col, pd), 7); /* pen colour fill colour */
    fprintf(fp, "100 0 -1 "); /* depth, pen style, area fill */
    fprintf(fp, "%.2f 0 0 -1 0 0 ", 4.0*lwd); /* style value, join .... */
    fprintf(fp, "%d\n", n); /* number of points */
    for(i = 0 ; i < n ; i++) {
	xx = x[i];
	yy = y[i];
	GConvert(&xx, &yy, coords, DEVICE, dd); XFconvert(&xx, &yy, pd);
	fprintf(fp, "  %d %d\n", (int)xx, (int)yy);
    }
}

static int styles[4] = {0,2,1,3};

static void XFig_Text(double x, double y, int coords,
		      char *str, double rot, double hadj, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;
    FILE *fp = pd->tmpfp;
    int fontnum, style = dd->gp.font;
    double size = floor(dd->gp.cex * dd->gp.ps + 0.5);

    if(style < 1 || style > 5) style = 1;
    pd->fontsize = size;
    pd->fontstyle = style;
    if(style == 5) fontnum = 32;
    else fontnum = pd->fontnum + styles[style-1];

    GConvert(&x, &y, coords, DEVICE, dd); XFconvert(&x, &y, pd);
    fprintf(fp, "4 %d ", (int)floor(2*hadj)); /* Text, how justified */
    fprintf(fp, "%d 100 0 ", XF_SetColor(dd->gp.col, pd));
      /* color, depth, pen_style */
    fprintf(fp, "%d %d %.4f 4 ", fontnum, (int)size, rot * DEG2RAD);
      /* font pointsize angle flags (Postscript font) */
    fprintf(fp, "%d %d ", (int)(16.667*GStrHeight(str, DEVICE, dd)+0.5),
	    (int)(16.667*GStrWidth(str, DEVICE, dd)+0.5));
    fprintf(fp, "%d %d ", (int)x, (int)y);
    XF_WriteString(fp, str);
    fprintf(fp, "\\001\n");
}

static Rboolean XFig_Locator(double *x, double *y, DevDesc *dd)
{
    return FALSE;
}

static void XFig_Mode(int mode, DevDesc* dd)
{
}

static void XFig_Hold(DevDesc *dd)
{
}

static double XFig_StrWidth(char *str, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;

    return floor(dd->gp.cex * dd->gp.ps + 0.5) *
	PostScriptStringWidth((unsigned char *)str,
			      &(pd->metrics[dd->gp.font-1]));
}

static void XFig_MetricInfo(int c, double *ascent, double *descent,
			  double *width, DevDesc *dd)
{
    XFigDesc *pd = (XFigDesc *) dd->deviceSpecific;

    PostScriptMetricInfo(c, ascent, descent, width,
			 &(pd->metrics[dd->gp.font-1]));
    *ascent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *ascent;
    *descent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *descent;
    *width = floor(dd->gp.cex * dd->gp.ps + 0.5) * *width;
}


/***********************************************************************

                 PDF driver also shares font handling

************************************************************************/

/* TODO
   Flate encoding?
   clipping?
*/


typedef struct {
    char filename[PATH_MAX];

    int pageno;		/* page number */

    int fontfamily;	/* font family */
    char encpath[PATH_MAX]; /* font encoding */
    char encname[100]; 
    char **afmpaths;	/* for user-specified family */
    int maxpointsize;

    double width;	/* plot width in inches */
    double height;	/* plot height in inches */
    Rboolean onefile;	/* one file or one file per page? */

    FILE *pdffp;		/* output file */

    FontMetricInfo metrics[5];	/* font metrics */

    /* This group of variables track the current device status.
     * They should only be set by routines that emit PDF. */
    struct {
	double lwd;		 /* line width */
	int lty;		 /* line type */
	int fontstyle;	         /* font style, R, B, I, BI, S */
	int fontsize;	         /* font size in points */
	rcolor col;		 /* color */
	rcolor fill;	         /* fill color */
	rcolor bg;		 /* color */
    } current;

    int nobjs;  /* number of objects */
    int pos[1100]; /* object positions */
    int pageobj[500]; /* page object numbers */
    int startstream; /* position of start of current stream */
    Rboolean inText;
}
PDFDesc;

/* Device Driver Actions */

static void   PDF_Activate(DevDesc*);
static void   PDF_Circle(double, double, int, double, int, int, DevDesc*);
static void   PDF_Clip(double, double, double, double, DevDesc*);
static void   PDF_Close(DevDesc*);
static void   PDF_Deactivate(DevDesc*);
static void   PDF_Hold(DevDesc*);
static void   PDF_Line(double, double, double, double, int, DevDesc*);
static Rboolean PDF_Locator(double*, double*, DevDesc*);
static void   PDF_Mode(int, DevDesc*);
static void   PDF_NewPage(DevDesc*);
static Rboolean PDF_Open(DevDesc*, PDFDesc*);
static void   PDF_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   PDF_Polyline(int, double*, double*, int, DevDesc*);
static void   PDF_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   PDF_Resize(DevDesc*);
static double PDF_StrWidth(char*, DevDesc*);
static void   PDF_MetricInfo(int, double*, double*, double*, DevDesc*);
static void   PDF_Text(double, double, int, char*, double, double, DevDesc*);

Rboolean
PDFDeviceDriver(DevDesc* dd, char *file, char *family, char *encoding, 
		char *bg, char *fg, double width, double height, double ps,
		int onefile)
{
    /* If we need to bail out with some sort of "error" */
    /* then we must free(dd) */

    double xoff = 0.0, yoff = 0.0, pointsize;
    rcolor setbg, setfg;

    PDFDesc *pd;

    /* Check and extract the device parameters */

    if(strlen(file) > PATH_MAX - 1) {
	free(dd);
	error("filename too long in pdf");
    }

    /* allocate new PDF device description */
    if (!(pd = (PDFDesc *) malloc(sizeof(PDFDesc))))
	return 0;

    /* from here on, if need to bail out with "error", must also */
    /* free(pd) */

    /* initialize PDF device description */
    strcpy(pd->filename, file);
    pd->fontfamily = MatchFamily(family);
    strcpy(pd->encpath, encoding);
    setbg = str2col(bg);
    setfg = str2col(fg);

    pd->width = width;
    pd->height = height;
    pointsize = floor(ps);
    if(setbg == NA_INTEGER && setfg  == NA_INTEGER) {
	free(dd);
	free(pd);
	error("invalid foreground/background color (pdf)");
    }

    pd->onefile = onefile;
    pd->maxpointsize = 72.0 * ((height > width) ? height : width);
    pd->pageno = 0;
    dd->dp.lty = 0;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->dp.bg = setbg;
    dd->dp.fg = dd->dp.col = setfg;
    dd->dp.left = 72 * xoff;			/* left */
    dd->dp.right = 72 * (xoff + pd->width);	/* right */
    dd->dp.bottom = 72 * yoff;			/* bottom */
    dd->dp.top = 72 * (yoff + pd->height);	/* top */

    /* Base Pointsize */
    /* Nominal Character Sizes in Pixels */
    /* Only right for 12 point font. */
    /* Max pointsize suggested by Peter Dalgaard */

    if(pointsize < 6.0) pointsize = 6.0;
    if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
    dd->dp.ps = pointsize;
    dd->dp.cra[0] = 0.9 * pointsize;
    dd->dp.cra[1] = 1.2 * pointsize;

    /* Character Addressing Offsets */
    /* These offsets should center a single */
    /* plotting character over the plotting point. */
    /* Pure guesswork and eyeballing ... */

    dd->dp.xCharOffset =  0.4900;
    dd->dp.yCharOffset =  0.3333;
    dd->dp.yLineBias = 0.2;

    /* Inches per Raster Unit */
    /* 1200 dpi */
    dd->dp.ipr[0] = 1.0/72.0;
    dd->dp.ipr[1] = 1.0/72.0;

    dd->dp.canResizePlot = 0;
    dd->dp.canChangeFont = 1;
    dd->dp.canRotateText = 1;
    dd->dp.canResizeText = 1;
    dd->dp.canClip = 0;
    dd->dp.canHAdj = 0; 

    /*	Start the driver */

    if(!PDF_Open(dd, pd)) {
	free(pd);
	return 0;
    }

    dd->dp.open	      = PDF_Open;
    dd->dp.close      = PDF_Close;
    dd->dp.activate   = PDF_Activate;
    dd->dp.deactivate = PDF_Deactivate;
    dd->dp.resize     = PDF_Resize;
    dd->dp.newPage    = PDF_NewPage;
    dd->dp.clip	      = PDF_Clip;
    dd->dp.text	      = PDF_Text;
    dd->dp.strWidth   = PDF_StrWidth;
    dd->dp.metricInfo = PDF_MetricInfo;
    dd->dp.rect	      = PDF_Rect;
    dd->dp.circle     = PDF_Circle;
    dd->dp.line	      = PDF_Line;
    dd->dp.polygon    = PDF_Polygon;
    dd->dp.polyline   = PDF_Polyline;
    dd->dp.locator    = PDF_Locator;
    dd->dp.mode	      = PDF_Mode;
    dd->dp.hold	      = PDF_Hold;

    dd->deviceSpecific = (void *) pd;
    dd->displayListOn = FALSE;
    return 1;
}

static void PDF_Invalidate(DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    pd->current.fontsize = -1;
    pd->current.fontstyle = -1;
    pd->current.lwd = 1;
    pd->current.lty = -1;
    /* page starts with black as the default fill and stroke colours */
    pd->current.col = 0;
    pd->current.fill = 0;
    pd->current.bg = 0xffffffff;
}

static void PDF_SetLineColor(int color, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    if(color != pd->current.col) {
	fprintf(pd->pdffp, "%.3f %.3f %.3f RG\n",
		R_RED(color)/255.0,
		R_GREEN(color)/255.0,
		R_BLUE(color)/255.0);
	pd->current.col = color;
    }
}

static void PDF_SetFill(int color, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    if(color != pd->current.fill) {
	fprintf(pd->pdffp, "%.3f %.3f %.3f rg\n",
		   R_RED(color)/255.0,
		   R_GREEN(color)/255.0,
		   R_BLUE(color)/255.0);
	pd->current.fill = color;
    }
}

/* Note that the line texture is scaled by the line width.
   Almost the same as PS, but the operator has a short name */

static void PDFSetLineTexture(FILE *fp, int *lty, int nlty, double lwd)
{
    double dash;
    int i;
    fprintf(fp,"[");
    for (i = 0; i < nlty; i++) {
        dash = (lwd >= 1 ? lwd:1) * ((i % 2) ? lty[i] + 1 : lty[i] - 1);
	if (dash < 0) dash = 0;
	fprintf(fp," %.2f", dash);
    }
    fprintf(fp,"] 0 d\n");
}

static void PDF_SetLineStyle(int newlty, double newlwd, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int i, ltyarray[8];

    if (pd->current.lty != newlty || pd->current.lwd != newlwd) {
	pd->current.lwd = newlwd;
	pd->current.lty = newlty;
	fprintf(pd->pdffp, "%.2f w\n", newlwd * 0.75);
	/* process lty : */
	for(i = 0; i < 8 && newlty & 15 ; i++) {
	    ltyarray[i] = newlty & 15;
	    newlty = newlty >> 4;
	}
	PDFSetLineTexture(pd->pdffp, ltyarray, i, newlwd * 0.75);
    }
}

static void texton(PDFDesc *pd)
{
    fprintf(pd->pdffp, "BT\n");
    pd->inText = TRUE;
}

static void textoff(PDFDesc *pd)
{
    fprintf(pd->pdffp, "ET\n");
    pd->inText = FALSE;
}

static void PDF_EncodeFont(PDFDesc *pd, int nobj)
{
    char *encname = pd->encname;

    fprintf(pd->pdffp, "%d 0 obj\n<<\n/Type /Encoding\n", nobj);
    if (strcmp(encname, "WinAnsiEncoding") == 0 || 
	strcmp(encname, "MacRomanEncoding") == 0 ||
	strcmp(encname, "PDFDocEncoding") == 0) {
	fprintf(pd->pdffp, "/BaseEncoding /%s\n", encname);
	fprintf(pd->pdffp, "/Differences [ 45/minus ]\n");
    } else if (strcmp(encname, "ISOLatin1Encoding") == 0) {
	fprintf(pd->pdffp, "/BaseEncoding /PDFDocEncoding\n");
	fprintf(pd->pdffp, "/Differences [ 45/minus 96/quoteleft\n144/dotlessi /grave /acute /circumflex /tilde /macron /breve /dotaccent\n/dieresis /.notdef /ring /cedilla /.notdef /hungarumlaut /ogonek /caron /space]\n");
    } else {
	fprintf(pd->pdffp, "/BaseEncoding /PDFDocEncoding\n");
	fprintf(pd->pdffp, "/Differences [ 0 %s ]\n", enccode);
    }
    fprintf(pd->pdffp, ">>\nendobj\n");
}

#include <time.h>

static void PDF_startfile(PDFDesc *pd)
{
    int i;
    struct tm *ltm;
    time_t ct;

    pd->nobjs = 0;
    pd->pageno = 0;
    fprintf(pd->pdffp, "%%PDF-1.1\n%%âãÏÓ\r\n");
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);

    /* Object 1 is Info node. Date format is from the PDF manual */

    ct = time(NULL);
    ltm = localtime(&ct);
    fprintf(pd->pdffp, 
	    "1 0 obj\n<<\n/CreationDate (D:%04d%02d%02d%02d%02d%02d)\n",
	    1900 + ltm->tm_year, ltm->tm_mon+1, ltm->tm_mday, 
	    ltm->tm_hour, ltm->tm_min, ltm->tm_sec);
    fprintf(pd->pdffp, "/Producer (R Graphics)\n>>\nendobj\n");

    /* Object 2 is the Catalog, pointing to pages list in object 3 (at end) */

    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "2 0 obj\n<<\n/Type /Catalog\n/Pages 3 0 R\n>>\nendobj\n");

    /* Object 3 will be at the end */

    /* Object 4 is the standard resources dict for each page */

    ++pd->nobjs;
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, 
	    "4 0 obj\n<<\n/ProcSet [/PDF /Text]\n/Font << %s %s %s %s %s %s >>\n>>\nendobj\n",
	    "/F1 6 0 R","/F2 7 0 R","/F3 8 0 R","/F4 9 0 R","/F5 10 0 R",
	    "/F6 11 0 R");

    /* Object 5 is the encoding for text fonts */

    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    PDF_EncodeFont(pd, pd->nobjs);

    /* Objects 6, 7, 8, 9 are the fonts for the text family */
    /* Object 10 is the Symbol font */
    /* Object 11 is Dingbats, used for (small) circles */

    for (i = 0; i < 4; i++) {
	pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
	fprintf(pd->pdffp, "%d 0 obj\n<<\n/Type /Font\n/Subtype /Type1\n/Name /F%d\n/BaseFont /%s\n/Encoding 5 0 R\n>>\nendobj\n", 
		i+6, i+1, familyname[i]);
    }
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "10 0 obj\n<<\n/Type /Font\n/Subtype /Type1\n/Name /F5\n/BaseFont /Symbol\n>>\nendobj\n");
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "11 0 obj\n<<\n/Type /Font\n/Subtype /Type1\n/Name /F6\n/BaseFont /ZapfDingbats\n>>\nendobj\n");
}

static void PDF_endfile(PDFDesc *pd)
{
    int i, startxref;
    /* object 3 lists all the pages */

    pd->pos[3] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "3 0 obj\n<<\n/Type /Pages\n/Kids [\n");
    for(i = 0; i < pd->pageno; i++)
	fprintf(pd->pdffp, "%d 0 R\n", pd->pageobj[i]);
    
    fprintf(pd->pdffp, 
	    "]\n/Count %d\n/MediaBox [0 0 %d %d]\n>>\nendobj\n", 
	    pd->pageno, 
	    (int) (0.5 + 72*pd->width), (int) (0.5 + 72*pd->height));

    /* write out xref table */

    startxref = (int) ftell(pd->pdffp);
    /* items here must be exactly 20 bytes including terminator */
    fprintf(pd->pdffp, "xref\n0 %d\n", pd->nobjs+1);
    fprintf(pd->pdffp, "0000000000 65535 f \n");
    for(i = 1; i <= pd->nobjs; i++) {
	fprintf(pd->pdffp, "%010d 00000 n \n", pd->pos[i]);
    }
    fprintf(pd->pdffp, 
	    "trailer\n<<\n/Size %d\n/Info 1 0 R\n/Root 2 0 R\n>>\nstartxref\n%d\n", 
	    pd->nobjs+1, startxref);
    fprintf(pd->pdffp, "%%%%EOF\n");

    fclose(pd->pdffp);
}


static Rboolean PDF_Open(DevDesc *dd, PDFDesc *pd)
{
    char buf[512], *p;
    int i;

    if (!LoadEncoding(pd->encpath, pd->encname, TRUE)) {
	warning("problem loading encoding file");
	return FALSE;
    }
    for(i = 0; i < 4 ; i++) {
	p = Family[pd->fontfamily].afmfile[i];
	if(!PostScriptLoadFontMetrics(p, &(pd->metrics[i]),
				      familyname[i], 1)) {
	    warning("cannot read afm file %s", p);
	    return FALSE;
	}
    }
    if(!PostScriptLoadFontMetrics("sy______.afm", &(pd->metrics[4]),
				  familyname[4], 0)) {
	warning("cannot read afm file sy______.afm");
	return FALSE;
    }

    /* NB: this must be binary to get tell positions and line endings right */

    sprintf(buf, pd->filename, pd->pageno + 1); /* page 1 to start */
    pd->pdffp = R_fopen(R_ExpandFileName(buf), "wb");
    if (!pd->pdffp) {
	warning("cannot open `pdf' file argument `%s'", buf);
	return FALSE;
    }

    PDF_startfile(pd);
    return TRUE;
}


static void PDF_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
/*    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    fprintf(pd->pdffp, "Q q %.2f %.2f %.2f %.2f re W\n", 
	    x0, y0, x1 - x0, y1 - y0);    
    PDF_Invalidate(dd); */
}

static void PDF_Resize(DevDesc *dd)
{
}

static void PDF_endpage(PDFDesc *pd)
{
    int here;
    if(pd->inText) textoff(pd);
    fprintf(pd->pdffp, "Q\n");
    here = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "endstream\nendobj\n");
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "%d 0 obj\n%d\nendobj\n", pd->nobjs,
	    here - pd->startstream);   
}

static void PDF_NewPage(DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    char buf[512];

    if(pd->pageno > 499 || pd->nobjs > 1099)
	error("limit on pages or objects exceeded:please shut down the PDFdevice");

    if(pd->pageno > 0) {
	PDF_endpage(pd);
	if(!pd->onefile) {
	    PDF_endfile(pd);
	    sprintf(buf, pd->filename, pd->pageno + 1); /* page 1 to start */
	    pd->pdffp = R_fopen(R_ExpandFileName(buf), "wb");
	    if (!pd->pdffp)
		error("cannot open `pdf' file argument `%s'\n  please shut down the PDFdevice", buf);
	    PDF_startfile(pd);
	}
    }
    
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    pd->pageobj[pd->pageno++] = pd->nobjs;
    fprintf(pd->pdffp, "%d 0 obj\n<<\n/Type /Page\n/Parent 3 0 R\n/Contents %d 0 R\n/Resources 4 0 R\n>>\nendobj\n", 
	    pd->nobjs, pd->nobjs+1);
    pd->pos[++pd->nobjs] = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "%d 0 obj\n<<\n/Length %d 0 R\n>>\nstream\r\n", 
	    pd->nobjs, pd->nobjs + 1);
    pd->startstream = (int) ftell(pd->pdffp);
    fprintf(pd->pdffp, "1 J 1 j 10 M q\n");
    PDF_Invalidate(dd);

    if(dd->gp.bg != R_RGB(255,255,255)) {
	PDF_SetFill(dd->gp.bg, dd);
	fprintf(pd->pdffp, "0 0 %.2f %.2f re f\n",
		72.0 * pd->width, 72.0 * pd->height);
    }
    pd->inText = FALSE;
}

static void PDF_Close(DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    
    if(pd->pageno > 0) PDF_endpage(pd);
    PDF_endfile(pd);
    free(pd); 
}

static void PDF_Activate(DevDesc *dd) {}
static void PDF_Deactivate(DevDesc *dd) {}

static void PDF_Rect(double x0, double y0, double x1, double y1, int coords,
		      int bg, int fg, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int code;

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);
    if (code) {
	if(pd->inText) textoff(pd);
	if(code & 2)
	    PDF_SetFill(bg, dd);
	if(code & 1) {
	    PDF_SetLineColor(fg, dd);
	    PDF_SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	GConvert(&x0, &y0, coords, DEVICE, dd);
	GConvert(&x1, &y1, coords, DEVICE, dd);
	fprintf(pd->pdffp, "%.2f %.2f %.2f %.2f re", x0, y0, x1-x0, y1-y0);
	switch(code){
	case 1: fprintf(pd->pdffp, " S\n"); break;
	case 2: fprintf(pd->pdffp, " f\n"); break;
	case 3: fprintf(pd->pdffp, " B\n"); break;
	}
    }
}

/* r is in device coords */
static void PDF_Circle(double x, double y, int coords, double r,
		      int bg, int fg, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int code, tr;
    double xx, yy, a;

    GConvert(&x, &y, coords, DEVICE, dd);

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);

    if (code) {
	if(code & 2)
	    PDF_SetFill(bg, dd);
	if(code & 1) {
	    PDF_SetLineColor(fg, dd);
	    PDF_SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	if(r > 10) { /* somewhat arbitrary, use font up to 20pt */
            /* Use four Bezier curves, hand-fitted to quadrants */
	    double s = 0.55 * r;
	    if(pd->inText) textoff(pd);
	    fprintf(pd->pdffp, "  %.2f %.2f m\n", x - r, y);
	    fprintf(pd->pdffp, "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", 
		   x - r, y + s, x - s, y + r, x, y + r);
	    fprintf(pd->pdffp, "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", 
		   x + s, y + r, x + r, y + s, x + r, y);
	    fprintf(pd->pdffp, "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", 
		   x + r, y - s, x + s, y - r, x, y - r);
	    fprintf(pd->pdffp, "  %.2f %.2f %.2f %.2f %.2f %.2f c\n", 
		   x - s, y - r, x - r, y - s, x - r, y);
	    switch(code){
	    case 1: fprintf(pd->pdffp, "S\n"); break;
	    case 2: fprintf(pd->pdffp, "f\n"); break;
	    case 3: fprintf(pd->pdffp, "B\n"); break;
	    }
	} else {
	    /* Use char 108 in Dingbats, which is a solid disc
	       afm is C 108 ; WX 791 ; N a71 ; B 35 -14 757 708 ;
	       so diameter = 0.722 * size
	       centre = (0.396, 0.347) * size
	    */
	    a = 2./0.722 * r;
	    xx = x - 0.396*a;
	    yy = y - 0.347*a;
	    tr = (bg != NA_INTEGER) + 2 * (fg != NA_INTEGER) - 1;
	    if(!pd->inText) texton(pd);
	    fprintf(pd->pdffp, 
		    "/F6 1 Tf %d Tr %.2f 0 0 %.2f %.2f %.2f Tm", 
		    tr, a, a, xx, yy);
	    fprintf(pd->pdffp, " (l) Tj 0 Tr\n");	    
	}
    }
}

static void PDF_Line(double x1, double y1, double x2, double y2,
		    int coords, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PDF_SetLineColor(dd->gp.col, dd);
    PDF_SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);
    if(pd->inText) textoff(pd);
    fprintf(pd->pdffp, "%.2f %.2f m %.2f %.2f l S\n", x1, y1, x2, y2);
}

static void PDF_Polygon(int n, double *x, double *y, int coords,
			 int bg, int fg, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    double xx, yy;
    int i, code;

    code = 2 * (bg != NA_INTEGER) + (fg != NA_INTEGER);

    if (code) {
	if(pd->inText) textoff(pd);
	if(code & 2)
	    PDF_SetFill(bg, dd);
	if(code & 1) {
	    PDF_SetLineColor(fg, dd);
	    PDF_SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
	}
	xx = x[0];
	yy = y[0];
	GConvert(&xx, &yy, coords, DEVICE, dd);
	fprintf(pd->pdffp, "  %.2f %.2f m\n", xx, yy);
	for(i = 1 ; i < n ; i++) {
	    xx = x[i];
	    yy = y[i];
	    GConvert(&xx, &yy, coords, DEVICE, dd);
	    fprintf(pd->pdffp, "  %.2f %.2f l\n", xx, yy);
	}
	switch(code){
	case 1: fprintf(pd->pdffp, "s\n"); break;
	case 2: fprintf(pd->pdffp, "h f\n"); break;
	case 3: fprintf(pd->pdffp, "b\n"); break;
	}
    }

}

static void PDF_Polyline(int n, double *x, double *y, int coords,
			DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc*) dd->deviceSpecific;
    double xx, yy;
    int i;

    if(pd->inText) textoff(pd);
    PDF_SetLineColor(dd->gp.col, dd);
    PDF_SetLineStyle(dd->gp.lty, dd->gp.lwd, dd);
    xx = x[0];
    yy = y[0];
    GConvert(&xx, &yy, coords, DEVICE, dd);
    fprintf(pd->pdffp, "%.2f %.2f m\n", xx, yy);
    for(i = 1 ; i < n ; i++) {
	xx = x[i];
	yy = y[i];
	GConvert(&xx, &yy, coords, DEVICE, dd);
	fprintf(pd->pdffp, "%.2f %.2f l\n", xx, yy);
    }
    fprintf(pd->pdffp, "S\n");
}


static void PDF_Text(double x, double y, int coords,
		     char *str, double rot, double hadj, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;
    int size = (int)floor(dd->gp.cex * dd->gp.ps + 0.5);
    int face = dd->gp.font;
    double a, b, rot1;

    rot1 = rot * DEG2RAD;
    a = size * cos(rot1);
    b = size * sin(rot1);
    GConvert(&x, &y, coords, DEVICE, dd);
    if(!pd->inText) texton(pd);
    PDF_SetFill(dd->gp.col, dd);
    fprintf(pd->pdffp, "/F%d 1 Tf %.2f %.2f %.2f %.2f %.2f %.2f Tm ", 
	    face, a, b, -b, a, x, y);
    PostScriptWriteString(pd->pdffp, str);
    fprintf(pd->pdffp, " Tj\n");
}

static Rboolean PDF_Locator(double *x, double *y, DevDesc *dd)
{
    return FALSE;
}

static void PDF_Mode(int mode, DevDesc* dd)
{
}

static void PDF_Hold(DevDesc *dd)
{
}

static double PDF_StrWidth(char *str, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    return floor(dd->gp.cex * dd->gp.ps + 0.5) *
	PostScriptStringWidth((unsigned char *)str,
			      &(pd->metrics[dd->gp.font-1]));
}

static void PDF_MetricInfo(int c, double *ascent, double *descent,
			  double *width, DevDesc *dd)
{
    PDFDesc *pd = (PDFDesc *) dd->deviceSpecific;

    PostScriptMetricInfo(c, ascent, descent, width,
			 &(pd->metrics[dd->gp.font-1]));
    *ascent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *ascent;
    *descent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *descent;
    *width = floor(dd->gp.cex * dd->gp.ps + 0.5) * *width;
}
