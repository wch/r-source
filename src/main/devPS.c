/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 1999  Robert Gentleman, Ross Ihaka and the
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
#include <Rconfig.h>
#endif

#include <stdio.h>
#include <ctype.h>

#include "Defn.h"
#include "Graphics.h"
#include "Error.h"
#include "Fileio.h"

#define PS_minus_default 45
/* wrongly was 177 (plusminus);
   hyphen = 45 or 173;	(n-dash not available as code!)
   175 = "¯" (= "overline" (= high 'negative' sign))
*/
char PS_minus = PS_minus_default;/*-> TODO: make this a ps.option() !*/

/* Part 0.  AFM File Names */

/* This structure gives the set of font names for each type face. */
/* They also give the official Adobe abbreviated name which is used */
/* for files such as those which contain the font metrics. */

static struct {
    char *family;
    struct {
	char *name;
	char *abbr;
    } font[5];
}
Family[] = {

    { "AvantGarde",
      {{ "AvantGarde-Book",			"agw_____" },
       { "AvantGarde-Demi",			"agd_____" },
       { "AvantGarde-BookOblique",		"agwo____" },
       { "AvantGarde-DemiOblique",		"agdo____" },
       { "Symbol",				"sy______" }}
    },
    
    { "Bookman",
      {{ "Bookman-Light",			"bkl_____" },
       { "Bookman-Demi",			"bkd_____" },
       { "Bookman-LightItalic",			"bkli____" },
       { "Bookman-DemiItalic",			"bkdi____" },
       { "Symbol",				"sy______" }}
    },

    { "Courier",
      {{ "Courier",				"com_____" },
       { "Courier-Bold",			"cob_____" },
       { "Courier-Oblique",			"coo_____" },
       { "Courier-BoldOblique",			"cobo____" },
       { "Symbol",				"sy______" }}
    },
	  
    { "Helvetica",
      {{ "Helvetica",				"hv______" },
       { "Helvetica-Bold",			"hvb_____" },
       { "Helvetica-Oblique",			"hvo_____" },
       { "Helvetica-BoldOblique",		"hvbo____" },
       { "Symbol",				"sy______" }}
    },

#ifdef NOTYET
    { "Helvetica-Condensed",
      {{ "Helvetica-Condensed",			"hvc_____" },
       { "Helvetica-Condensed-Bold",		"hvcb____" },
       { "Helvetica-Condensed-Oblique",		"hvcdo___" },
       { "Helvetica-Condensed-BoldObl",		"hvnbo___" },
       { "Symbol",				"sy______" }}
    },
#endif

    { "Helvetica-Narrow",
      {{ "Helvetica-Narrow",			"hvn_____" },
       { "Helvetica-Narrow-Bold",		"hvnb____" },
       { "Helvetica-Narrow-Oblique",		"hvno____" },
       { "Helvetica-Narrow-BoldOblique",	"hvnbo___" },
       { "Symbol",				"sy______" }}
    },

    { "NewCenturySchoolbook",
      {{ "NewCenturySchlbk-Roman",		"ncr_____" },
       { "NewCenturySchlbk-Bold",		"ncb_____" },
       { "NewCenturySchlbk-Italic",		"nci_____" },
       { "NewCenturySchlbk-BoldItalic",		"ncbi____" },
      { "Symbol",				"sy______" }}
    },

    { "Palatino",
      {{ "Palatino-Roman",			"por_____" },
       { "Palatino-Bold",			"pob_____" },
       { "Palatino-Italic",			"poi_____" },
       { "Palatino-BoldItalic",			"pobi____" },
       { "Symbol",				"sy______" }}
    },

    { "Times",
      {{ "Times-Roman",				"tir_____" },
       { "Times-Bold",				"tib_____" },
       { "Times-Italic",			"tii_____" },
       { "Times-BoldItalic",			"tibi____" },
       { "Symbol",				"sy______" }}
    },

    { NULL }
};

/* These are the file extensions used on metrics files. */

static char *Extension[] = {
    "afm",			/* AdobeStandardEncoding (Unused) */
    "lt1",			/* ISOLatin1Encoding */
    "lt2",			/* ISOLatin2Encoding */
    0
};



/* Part 1.  AFM File Parsing.  */

/* These are the basic entities in the AFM file */

#define BUFSIZE 512

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

static int GetCharInfo(char *buf, FontMetricInfo *metrics)
{
    char *p = buf;
    int nchar;

    if (!MatchKey(buf, "C ")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%d", &nchar);
    if (nchar < 0) return 1;
    p = SkipToNextKey(p);

    if (!MatchKey(p, "WX")) return 0;
    p = SkipToNextItem(p);
    sscanf(p, "%hd", &(metrics->CharInfo[nchar].WX));
    p = SkipToNextKey(p);

    if (!MatchKey(p, "N ")) return 0;
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
    return 1;
}


/* Load Fontmetrics from a File */

int PostScriptLoadFontMetrics(char *fontname, FontMetricInfo *metrics)
{
    char buf[BUFSIZE];
    int mode;
    FILE *fp;

    if (!(fp = R_fopen(fontname, "r"))) return 0;

    mode = 0;
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
	    if (!GetCharInfo(buf, metrics)) goto error;
	    break;

	case Unknown:
	    printf("Warning: unknown AFM entity encountered");
	    break;

	case Empty:
	default:
	    break;
	}
    }
    fclose(fp);
    return 1;
 error:
    fclose(fp);
    return 0;
}

double PostScriptStringWidth(unsigned char *p, FontMetricInfo *metrics)
{
    int sum = 0;
    for ( ; *p; p++) {
	if (*p == '-' && isdigit(p[1]))
	    sum += metrics->CharInfo[(int)PS_minus].WX;
	else
	    sum += metrics->CharInfo[*p].WX;
    }
    return 0.001 * sum;
}

void PostScriptMetricInfo(int c, double *ascent, double *descent,
			  double *width, FontMetricInfo *metrics)
{
    if (c == 0) {
	*ascent = 0.001 * metrics->FontBBox[3];
	*descent = -0.001 * metrics->FontBBox[1];
	*width = 0.001 * (metrics->FontBBox[2] - metrics->FontBBox[0]);

    }
    else {
	*ascent = 0.001 * metrics->CharInfo[c].BBox[3];
	*descent = -0.001 * metrics->CharInfo[c].BBox[1];
	*width = 0.001 * metrics->CharInfo[c].WX;
    }
}


/*  Part 2.  Graphics Support Code.  */

static char *TypeFaceDef[] = { "R", "B", "I", "BI", "S" };

static void PSEncodeFont(FILE *fp, int index, int encoding)
{
    int i;
    for (i = 0; i < 4 ; i++) {
	fprintf(fp, "/%s findfont\n", Family[index].font[i].name);
	fprintf(fp, "dup length dict begin\n");
	fprintf(fp, "  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
	if (encoding) fprintf(fp, "  /Encoding ISOLatin1Encoding def\n");
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

static void PSFileHeader(FILE *fp, int font, int encoding, char *papername,
			 double paperwidth, double paperheight, int landscape,
			 int EPSFheader,
			 double left, double bottom, double right, double top)
{
    if(EPSFheader) 
	fprintf(fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
    else
	fprintf(fp, "%%!PS-Adobe-3.0\n");
    fprintf(fp, "%%%%DocumentFonts: %s %s %s\n%%%%+ %s %s\n",
	    Family[font].font[0].name, Family[font].font[1].name,
	    Family[font].font[2].name, Family[font].font[3].name,
	    Family[font].font[4].name);
    if(!EPSFheader)
	fprintf(fp, "%%%%DocumentMedia: %s %.0f %.0f 0 ()\n",
		papername, paperwidth, paperheight);
    fprintf(fp, "%%%%Title: R Graphics Output\n");
    fprintf(fp, "%%%%Creator: R Software\n");
    fprintf(fp, "%%%%Pages: (atend)\n");
    if (landscape) {
	fprintf(fp, "%%%%Orientation: Landscape\n");
	fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
		left, bottom, right, top);
/* This appears to be wrong, use *same* BBox as Portrait
		bottom, left, top, right);
*/
    }
    else {
	fprintf(fp, "%%%%Orientation: Portrait\n");
	fprintf(fp, "%%%%BoundingBox: %.0f %.0f %.0f %.0f\n",
		left, bottom, right, top);
    }
    fprintf(fp, "%%%%EndComments\n");
    fprintf(fp, "%%%%BeginProlog\n");
    fprintf(fp, "/gs  { gsave } def\n");
    fprintf(fp, "/gr  { grestore } def\n");
    if (landscape)
	fprintf(fp, "/bp  { gs %.2f 0 translate 90 rotate} def\n", paperwidth);
    else
	fprintf(fp, "/bp  { gs } def\n");
    fprintf(fp, "/ep  { showpage gr } def\n");
    fprintf(fp, "/m   { moveto } def\n");
    fprintf(fp, "/l   { lineto } def\n");
    fprintf(fp, "/np  { newpath } def\n");
    fprintf(fp, "/cp  { closepath } def\n");
    fprintf(fp, "/f   { fill } def\n");
    fprintf(fp, "/o   { stroke } def\n");
    fprintf(fp, "/c   { newpath 0 360 arc } def\n");
    fprintf(fp, "/r   { 3 index 3 index moveto 1 index 4 -1 roll\n");
    fprintf(fp, "	lineto exch 1 index lineto lineto closepath } def\n");
    fprintf(fp, "/p1  { stroke } def\n");
    fprintf(fp, "/p2  { bg setrgbcolor fill fg setrgbcolor } def\n");
    fprintf(fp, "/p3  { gsave bg setrgbcolor fill grestore stroke } def\n");
    fprintf(fp, "/t   { 6 -2 roll moveto gsave rotate\n");
    fprintf(fp, "       ps mul neg 0 2 1 roll rmoveto\n");
    fprintf(fp, "       1 index stringwidth pop\n");
    fprintf(fp, "       mul neg 0 rmoveto show grestore } def\n");
    fprintf(fp, "/rgb { setrgbcolor } def\n");
    fprintf(fp, "/s   { scalefont setfont } def\n");
    fprintf(fp, "/R   { /Font1 findfont } def\n");
    fprintf(fp, "/B   { /Font2 findfont } def\n");
    fprintf(fp, "/I   { /Font3 findfont } def\n");
    fprintf(fp, "/BI  { /Font4 findfont } def\n");
    fprintf(fp, "/S   { /Font5 findfont } def\n");
    PSEncodeFont(fp, font, encoding);
    fprintf(fp, "1 setlinecap 1 setlinejoin\n");
    fprintf(fp, "%%%%EndProlog\n");
}

void PostScriptFileTrailer(FILE *fp, int pageno)
{
    fprintf(fp, "ep\n");
    fprintf(fp, "%%%%Trailer\n");
    fprintf(fp, "%%%%Pages: %d\n", pageno);
    fprintf(fp, "%%%%EOF\n");
}

void PostScriptStartPage(FILE *fp, int pageno)
{
    fprintf(fp, "%%%%Page: %d %d\n", pageno, pageno);
    fprintf(fp, "bp\n");
}

void PostScriptEndPage(FILE *fp)
{
    fprintf(fp, "ep\n");
}

void PostScriptSetLineWidth(FILE *fp, double linewidth)
{
    fprintf(fp, "%.2f setlinewidth\n", linewidth);
}

void PostScriptSetFont(FILE *fp, int typeface, double size)
{
    fprintf(fp, "/ps %.0f def %s %.0f s\n", size, TypeFaceDef[typeface], size);
}

void PostScriptSetColor(FILE *fp, double r, double g, double b)
{
    fprintf(fp,"%.4f %.4f %.4f rgb\n", r, g, b);
}

void PostScriptSetLineTexture(FILE *fp, int *lty, int nlty, double lwd)
{
    double dash;
    int i;
    fprintf(fp,"[");
    for (i = 0; i < nlty; i++) {
        dash = lwd * ((i % 2) ? lty[i] + 1 : lty[i] - 1);
	if (dash < 0) dash = 0;
	fprintf(fp," %.2f", dash);
    }
    fprintf(fp,"] 0 setdash\n");
}


void PostScriptMoveTo(FILE *fp, double x, double y)
{
    fprintf(fp, "%.2f %.2f m\n", x, y);
}

void PostScriptLineTo(FILE *fp, double x, double y)
{
    fprintf(fp, "%.2f %.2f l\n", x, y);
}

void PostScriptStartPath(FILE *fp)
{
    fprintf(fp, "np\n");
}

void PostScriptEndPath(FILE *fp)
{
    fprintf(fp, "o\n");
}

void PostScriptRectangle(FILE *fp, double x0, double y0, double x1, double y1)
{
    fprintf(fp, "%.2f %.2f %.2f %.2f r ", x0, y0, x1, y1);
}

void PostScriptCircle(FILE *fp, double x, double y, double r)
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
	case '-':
	    if (isdigit(str[1]))
		fputc(PS_minus, fp);
	    else
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

void PostScriptText(FILE *fp, double x, double y,
		    char *str, double xc, double yc, double rot)
{
    fprintf(fp, "%.2f %.2f ", x, y);
    PostScriptWriteString(fp, str);
    fprintf(fp, " %.2f %.2f %.2f t\n", xc, yc, rot);
}


/* Part 3.  Device Driver State. */

typedef struct {
    char filename[128];

    char papername[64];	 /* paper name */
    int paperwidth;	 /* paper width in inches */
    int paperheight;	 /* paper height in inches */
    int landscape;	 /* landscape mode */
    int pageno;		 /* page number */

    int fontfamily;	 /* font family */
    int encoding;	 /* font encoding */
    int fontstyle;	 /* font style, R, B, I, BI, S */
    int fontsize;	 /* font size in points */
    int maxpointsize;

    double width;	 /* plot width in points */
    double height;	 /* plot height in points */
    double pagewidth;	 /* page width in points */
    double pageheight;	 /* page height in points */
    int pagecentre;      /* centre image on page? */

    double lwd;		 /* current line width */
    int lty;		 /* current line type */
    rcolor col;		 /* current color */
    rcolor fill;	 /* current fill color */
    rcolor bg;		 /* background color */

    FILE *psfp;		 /* output file */

    int EPSFheader;      /* EPSF header */
}
PostScriptDesc;

static FontMetricInfo metrics[5];	/* font metrics */


/* Device Driver Actions */

static void   PS_Activate(DevDesc*);
static void   PS_Circle(double, double, int, double, int, int, DevDesc*);
static void   PS_Clip(double, double, double, double, DevDesc*);
static void   PS_Close(DevDesc*);
static void   PS_Deactivate(DevDesc*);
static void   PS_Hold(DevDesc*);
static void   PS_Line(double, double, double, double, int, DevDesc*);
static int    PS_Locator(double*, double*, DevDesc*);
static void   PS_Mode(int, DevDesc*);
static void   PS_NewPage(DevDesc*);
static int    PS_Open(DevDesc*, PostScriptDesc*);
static void   PS_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   PS_Polyline(int, double*, double*, int, DevDesc*);
static void   PS_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   PS_Resize(DevDesc*);
static double PS_StrWidth(char*, DevDesc*);
static void   PS_MetricInfo(int, double*, double*, double*, DevDesc*);
static void   PS_Text(double, double, int, char*, double, double, double,
		      DevDesc*);



/* PostScript Support (formally in PostScript.c) */

static void PostScriptSetCol(FILE *fp, double r, double g, double b)
{
	fprintf(fp,"/fg { %.4f %.4f %.4f } def fg rgb\n", r, g, b);
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
static int  MatchFamily(char *name);


int PSDeviceDriver(DevDesc *dd, char *file, char *paper, char *family,
		   char *bg, char *fg,
		   double width, double height,
		   double horizontal, double ps,
		   int onefile, int pagecentre)
{
    /* If we need to bail out with some sort of "error" */
    /* then we must free(dd) */

    double xoff, yoff, pointsize;
    PostScriptDesc *pd;

    /* Check and extract the device parameters */

    if(strlen(file) > 127) {
	free(dd);
	error("filename to long in postscript");
    }

    /* allocate new postscript device description */
    if (!(pd = (PostScriptDesc *) malloc(sizeof(PostScriptDesc))))
	return 0;

    /* from here on, if need to bail out with "error", must also */
    /* free(pd) */

    /* initialise postscript device description */
    strcpy(pd->filename, file);
    strcpy(pd->papername, paper);
    pd->fontfamily = MatchFamily(family);
    pd->encoding = 1;
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
	error("invalid foreground/background color (postscript)");
    }

    /* Deal with paper and plot size and orientation */

    if(!strcmp(pd->papername, "Default") ||
       !strcmp(pd->papername, "default")) {
	char *ps = getenv("R_PAPERSIZE");
	if(ps) strcpy(pd->papername, ps);
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
	error("invalid page type (postscript)");
    }
    pd->pagecentre = pagecentre;
    pd->paperwidth = 72 * pd->pagewidth;
    pd->paperheight = 72 * pd->pageheight;
    pd->EPSFheader = !onefile;
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
    pd->lty = 1;

    /* Set graphics parameters that must be set by device driver. */
    /* Page dimensions in points. */

    dd->dp.bg = pd->bg;
    dd->dp.fg = dd->dp.col = pd->col;
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
    dd->dp.cra[0] = (6.0 / 12.0) * pointsize;
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
    dd->dp.canClip = 0;

    /*	Start the driver */

    if(!PS_Open(dd, pd)) {
	free(pd);
	return 0;
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
    dd->displayListOn = 0;
    return 1;
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
    if(color != pd->col) {
	PostScriptSetCol(pd->psfp,
			 R_RED(color)/255.0,
			 R_GREEN(color)/255.0,
			 R_BLUE(color)/255.0);
	pd->col = color;
    }
}

static void SetFill(int color, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(color != pd->fill) {
	PostScriptSetFill(pd->psfp,
			  R_RED(color)/255.0,
			  R_GREEN(color)/255.0,
			  R_BLUE(color)/255.0);
	pd->fill = color;
    }
}

/* Note that the line texture is scaled by the line width. */

static void SetLineStyle(int newlty, double newlwd, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    int i, ltyarray[8];
    if (pd->lty != newlty || pd->lwd != newlwd) {
	pd->lwd = newlwd;
	pd->lty = newlty;
	PostScriptSetLineWidth(pd->psfp, dd->gp.lwd*0.75);
	for(i = 0; i < 8 && newlty & 15 ; i++) {
	    ltyarray[i] = newlty & 15;
	    newlty = newlty >> 4;
	}
	PostScriptSetLineTexture(pd->psfp, ltyarray, i, dd->gp.lwd * 0.75);
    }
}

static void SetFont(int style, int size, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;
    if(style < 1 || style > 5)
	style = 1;
    if(size < 1 || size > pd->maxpointsize)
	size = 10;
    if(size != pd->fontsize || style != pd->fontstyle) {
	PostScriptSetFont(pd->psfp, style-1, size);
	pd->fontsize = size;
	pd->fontstyle = style;
    }
}

static int PS_Open(DevDesc *dd, PostScriptDesc *pd)
{
    char buf[512];
    int i;

    for(i = 0; i < 5 ; i++) {
	sprintf(buf, "%s/afm/%s.%s", R_Home,
		Family[pd->fontfamily].font[i].abbr,
		(i == 4) ? "afm" : Extension[pd->encoding]);
	if(!PostScriptLoadFontMetrics(buf, &(metrics[i])))
	    return 0;
    }

    if (strlen(pd->filename) == 0)
	pd->psfp = popen(R_PRINTCMD, "w");
    else
	pd->psfp = R_fopen(R_ExpandFileName(pd->filename), "w");
    if (!pd->psfp) return 0;

    if(pd->landscape)
	PSFileHeader(pd->psfp,
		     pd->fontfamily,
		     pd->encoding,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     pd->EPSFheader,
		     dd->dp.bottom,
		     dd->dp.left,
		     dd->dp.top,
		     dd->dp.right);
    else
	PSFileHeader(pd->psfp,
		     pd->fontfamily,
		     pd->encoding,
		     pd->papername,
		     pd->paperwidth,
		     pd->paperheight,
		     pd->landscape,
		     pd->EPSFheader,
		     dd->dp.left,
		     dd->dp.bottom,
		     dd->dp.right,
		     dd->dp.top);

    pd->fontstyle = 1;
    pd->fontsize = 10;
    pd->pageno = 0;
    return 1;
}


static void PS_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
}


static void PS_Resize(DevDesc *dd)
{
}

static void PS_NewPage(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    pd->pageno++;
    if(pd->pageno > 1) PostScriptEndPage(pd->psfp);
    if(pd->pageno > 1 && pd->EPSFheader) 
	warning("multiple pages used in postscript() with onefile=FALSE");
    PostScriptStartPage(pd->psfp, pd->pageno);
    PostScriptSetFont(pd->psfp, pd->fontstyle-1, pd->fontsize);
    PostScriptSetLineWidth(pd->psfp, 0.75);
    PostScriptSetCol(pd->psfp,
		     R_RED(pd->col)/255.0,
		     R_GREEN(pd->col)/255.0,
		     R_BLUE(pd->col)/255.0);
    if(dd->dp.bg != R_RGB(255,255,255)) {
	SetFill(dd->dp.bg, dd);
#ifdef OLD
	PostScriptRectangle(pd->psfp,
			    dd->gp.left,
			    dd->gp.bottom,
			    dd->gp.right,
			    dd->gp.top);
	fprintf(pd->psfp, "p2\n");
#else
	PostScriptRectangle(pd->psfp,
			    0, 0, 72.0 * pd->pagewidth, 72.0 * pd->pageheight);
	fprintf(pd->psfp, "p2\n");
#endif
    }
}

static void PS_Close(DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    PostScriptFileTrailer(pd->psfp, pd->pageno);
    fclose(pd->psfp);
    free(pd);
}

static void PS_Activate(DevDesc *dd) {}
static void PS_Deactivate(DevDesc *dd) {}

static double PS_StrWidth(char *str, DevDesc *dd)
{
    return floor(dd->gp.cex * dd->gp.ps + 0.5) *
	PostScriptStringWidth((unsigned char *)str, &(metrics[dd->gp.font-1]));
}

static void PS_MetricInfo(int c, double *ascent, double *descent,
			  double *width, DevDesc *dd)
{
    PostScriptMetricInfo(c, ascent, descent, width, &(metrics[dd->gp.font-1]));
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
    /* code == 3, outine and fill */

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
    /* code == 3, outine and fill */

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
    /* code == 3, outine and fill */

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
		    char *str, double xc, double yc, double rot, DevDesc *dd)
{
    PostScriptDesc *pd = (PostScriptDesc *) dd->deviceSpecific;

    GConvert(&x, &y, coords, DEVICE, dd);
    SetFont(dd->gp.font, floor(dd->gp.cex * dd->gp.ps + 0.5), dd);
    SetColor(dd->gp.col, dd);
    PostScriptText(pd->psfp, x, y, str, xc, yc, rot);
}

static int PS_Locator(double *x, double *y, DevDesc *dd)
{
    return 0;
}

static void PS_Mode(int mode, DevDesc* dd)
{
}

static void PS_Hold(DevDesc *dd)
{
}
