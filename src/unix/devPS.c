/*
 *  R : A Computer Langage for Statistical Data Analysis
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Graphics.h"
#include "PS.h"
#include "Errormsg.h"
#include "Fileio.h"
#include <stdio.h>

unsigned int str2col(char*);

static char filename[128];

static char papername[64];			/* paper name */
static int paperwidth;				/* paper width in inches */
static int paperheight;				/* paper height in inches */
static int landscape;				/* landscape mode */
static int pageno;				/* page number */

static int fontfamily;				/* font family */
static int fontstyle;				/* font style, R, B, I, BI, S */
static int fontsize;				/* font size in points */
static int maxpointsize;

static double width;				/* plot width in points */
static double height;				/* plot height in points */
static double pagewidth;			/* page width in points */
static double pageheight;			/* page height in points */

static int lty;					/* current line type */
static rcolor col;				/* current color */
static rcolor bg;				/* background color */

static FontMetricInfo metrics[5]; 		/* font metrics */

static FILE *psfp;				/* output file */

	/* Device Driver Entry Point */

int           PSDeviceDriver(char**, int, double*, int);

	/* Device Driver Actions */

static void   PS_Circle(double, double, double, int, int);
static void   PS_Clip(double, double, double, double);
static void   PS_Close(void);
static void   PS_EndPath(void);
static void   PS_Hold(void);
static void   PS_LineTo(double, double);
static int    PS_Locator(double*, double*);
static void   PS_Mode(int);
static void   PS_MoveTo(double, double);
static void   PS_NewPlot(void);
static int    PS_Open(void);
static void   PS_Polygon(int, double*, double*, int, int);
static void   PS_Rect(double, double, double, double, int, int);
static void   PS_Resize(void);
static void   PS_StartPath(void);
static double PS_StrWidth(char*);
static void   PS_MetricInfo(int, double*, double*, double*);
static void   PS_Text(double, double, char*, double, double, double);

	/* Support Routines */

static void   SetColor(int);
static void   SetFont(int, int);
static void   SetLinetype(int);
static int    matchfamily(char *name);

	/*  PostScript Device Driver Parameters  */
	/*  cpars[0] = output filename           */
	/*  cpars[1] = paper type                */
	/*  cpars[2] = typeface                  */
	/*  cpars[3] = background color          */
	/*  cpars[4] = foreground color          */
	/*  npars[0] = width in inches           */
	/*  npars[1] = height in inches          */
	/*  npars[2] = landscape                 */
	/*  npars[3] = pointsize                 */


int PSDeviceDriver(char **cpars, int ncpars, double *npars, int nnpars)
{
	double xoff, yoff, pointsize;
	DevInit = 0;

		/* Check and extract the device parameters */

	if(ncpars != 5 || nnpars != 4)
		error("invalid device parameters (postscript)\n");
	if(strlen(cpars[0]) > 127)
		error("filename to long in postscript\n");
	strcpy(filename, cpars[0]);
	strcpy(papername, cpars[1]);
	fontfamily = matchfamily(cpars[2]);
	bg = str2col(cpars[3]);
	col = str2col(cpars[4]);
	width = npars[0];
	height = npars[1];
	landscape = npars[2];
	pointsize = floor(npars[3]);
	if(bg == NA_INTEGER && col == NA_INTEGER)
		error("invalid foreground/background color (postscript)\n");
	DP->bg = GP->bg = bg;
	DP->col = GP->col = col;

		/* Deal with paper and plot size and orientation */

	if(!strcmp(papername, "Default") || !strcmp(papername, "default")) {
		char *ps = getenv("R_PAPERSIZE");
		if(ps) strcpy(papername, ps);
		else strcpy(papername, "a4");
	}
	if(!strcmp(papername, "A4") || !strcmp(papername, "a4")) {
		pagewidth  = 21.0/2.54;
		pageheight = 29.7/2.54;
	}
	else if(!strcmp(papername, "Letter") || !strcmp(papername, "letter")) {
		pagewidth  =  8.5;
		pageheight = 11.0;
	}
	else if(!strcmp(papername, "Legal") || !strcmp(papername, "legal")) {
		pagewidth  =  8.5;
		pageheight = 14.0;
	}
	else if(!strcmp(papername, "Executive") || !strcmp(papername, "executive")) {
		pagewidth  =  7.25;
		pageheight = 10.5;
	}
	else error("invalid page type (postscript)\n");
	paperwidth = 72 * pagewidth;
	paperheight = 72 * pageheight;
	if(landscape) {
		double tmp;
		tmp = pagewidth;
		pagewidth = pageheight;
		pageheight = tmp;;
	}
	if(width < 0.1 || width > pagewidth-0.5)
		width = pagewidth-0.5;
	if(height < 0.1 || height > pageheight-0.5)
		height = pageheight-0.5;
	xoff = (pagewidth - width)/2.0;
	yoff = (pageheight - height)/2.0;
	maxpointsize = 72.0 * ((pageheight>pagewidth) ? pageheight : pagewidth);

		/*  Page dimensions in points  */

	GP->left = 72 * xoff;			/* left */
	GP->right = 72 * (xoff + width); 	/* right */
	GP->bottom = 72 * yoff;			/* bottom */
	GP->top = 72 * (yoff + height);		/* top */

		/*  Start the driver */

	if(!PS_Open()) return 0;

	DevOpen = PS_Open;
	DevClose = PS_Close;
	DevResize = PS_Resize;
	DevNewPlot = PS_NewPlot;
	DevClip = PS_Clip;
	DevStartPath = PS_StartPath;
	DevEndPath = PS_EndPath;
	DevMoveTo = PS_MoveTo;
	DevLineTo = PS_LineTo;
	DevText = PS_Text;
	DevStrWidth = PS_StrWidth;
	DevMetricInfo = PS_MetricInfo;
	DevRect = PS_Rect;
	DevCircle = PS_Circle;
	DevPolygon = PS_Polygon;
	DevLocator = PS_Locator;
	DevMode = PS_Mode;
	DevHold = PS_Hold;

		/* Base Pointsize */
		/* Nominal Character Sizes in Pixels */
		/* Only right for 12 point font. */
		/* Max pointsize suggested by Peter Dalgaard */

	if(pointsize < 6.0) pointsize = 6.0;
	if(pointsize > maxpointsize) pointsize = maxpointsize;
	GP->ps = pointsize;
	GP->cra[0] = (6.0/12.0) * pointsize;
	GP->cra[1] = (10.0/12.0) * pointsize;

		/* Character Addressing Offsets */
		/* These offsets should center a single */
		/* plotting character over the plotting point. */
		/* Pure guesswork and eyeballing ... */

	GP->xCharOffset =  0.4900;
	GP->yCharOffset =  0.3333;
	GP->yLineBias = 0.1;

		/* Inches per Raster Unit */
		/* We use points (72 dots per inch) */

	GP->ipr[0] = 1.0/72.0;
	GP->ipr[1] = 1.0/72.0;

	GP->canResizePlot = 0;
	GP->canChangeFont = 1;
	GP->canRotateText = 1;
	GP->canResizeText = 1;
	GP->canClip = 1;

	lty = 1;
	pageno = 0;

	DevInit = 1;
	return 1;
}


static char *FamilyName[][6][2] = {

	{ { "AvantGarde",				"AvantGarde",},
	  { "AvantGarde-Book",				"AvantB",},
	  { "AvantGarde-Demi",				"AvantD",},
	  { "AvantGarde-BookOblique",			"AvantBO",},
	  { "AvantGarde-DemiOblique",			"AvantDO",},
	  { "Symbol", 					"Symbol",}, },

	{ { "Bookman",					"Bookman",},
	  { "Bookman-Light",				"BookL",},
	  { "Bookman-Demi",				"BookD",},
	  { "Bookman-LightItalic",			"BookLI",},
	  { "Bookman-DemiItalic",			"BookDI",},
	  { "Symbol", 					"Symbol",}, },

	{ { "Courier",					"Courier",},
	  { "Courier",					"Cour",},
	  { "Courier-Bold",				"CourB",},
	  { "Courier-BoldOblique",			"CourBO",},
	  { "Courier-Oblique",				"CourO",},
	  { "Symbol", 					"Symbol",}, },

	{ { "Helvetica",				"Helvetica",},
	  { "Helvetica",				"Helv",},
	  { "Helvetica-Bold",				"HelvB",},
	  { "Helvetica-Oblique",			"HelvO",},
	  { "Helvetica-BoldOblique",			"HelvBO",},
	  { "Symbol", 					"Symbol",}, },

	{ { "Helvetica-Narrow",				"Helvetica-Narrow",},
	  { "Helvetica-Narrow",				"HelvN",},
	  { "Helvetica-Narrow-Bold",			"HelvNB",},
	  { "Helvetica-Narrow-Oblique",			"HelvNO",},
	  { "Helvetica-Narrow-BoldOblique",		"HelvNBO",},
	  { "Symbol", 					"Symbol",}, },

	{ { "NewCenturySchoolbook",			"NewCenturySchoolbook",},
	  { "NewCenturySchlbk-Roman",			"NCSchlR",},
	  { "NewCenturySchlbk-Bold",			"NCSchlB",},
	  { "NewCenturySchlbk-Italic",			"NCSchlI",},
	  { "NewCenturySchlbk-BoldItalic",		"NCSchlBI"},
	  { "Symbol", 					"Symbol",}, },

	{ { "Palatino",					"Palatino",},
	  { "Palatino-Roman",				"PalatR",},
	  { "Palatino-Bold",				"PalatB",},
	  { "Palatino-Italic",				"PalatI",},
	  { "Palatino-BoldItalic",			"PalatBI",},
	  { "Symbol", 					"Symbol",}, },

	{ { "Times",					"Times",},
	  { "Times-Roman",				"TimesR",},
	  { "Times-Bold",				"TimesB",},
          { "Times-Italic",				"TimesI",},
	  { "Times-BoldItalic",				"TimesBI",},
	  { "Symbol", 					"Symbol",}, },

	NULL
};

static int matchfamily(char *name)
{
	int i;
	for(i=0 ; FamilyName[i] != NULL ; i++)
		if(!strcmp(name, FamilyName[i][0][0])) return i;
	warning("unknown postscript font family, using %s\n",
		FamilyName[3][0][0]);
	return 3;
}

static void SetColor(int color)
{
	if(color != col) {
		PostScriptSetColor(psfp, 
			R_RED(color)/255.0,
			R_GREEN(color)/255.0,
			R_BLUE(color)/255.0);
		col = color;
	}
}

static void SetLinetype(int newlty)
{
	int i, ltyarray[8];
	lty = newlty;
	for(i=0 ; i<8 && newlty&15 ; i++) {
		ltyarray[i] = newlty&15;
		newlty = newlty>>4;
	}
	PostScriptSetLineTexture(psfp, ltyarray, i);
}

static void SetFont(int style, int size)
{
	if(style < 1 || style > 5)
		style = 1;
	if(size < 1 || size > maxpointsize)
		size = 10;
	if(size != fontsize || style != fontstyle) {
		PostScriptSetFont(psfp, style-1, size);
		fontsize = size;
		fontstyle = style;
	}
}

static int PS_Open(void)
{
	char buf[512];
	char *rhome;
	int i;

	if((rhome = getenv("RHOME")) == NULL)
		return 0;

	for(i=1 ; i<=5 ; i++) {
		sprintf(buf, "%s/afm/%s", rhome,
			FamilyName[fontfamily][i][1]);
		if(!PostScriptLoadFontMetrics(buf, &(metrics[i-1])))
			return 0;
	}

	if(!(psfp = R_fopen(filename, "w"))) return 0;

	if(landscape)
		PostScriptFileHeader(psfp,
			&(FamilyName[fontfamily][0][0]),
			papername,
			paperwidth,
			paperheight,
			landscape,
			GP->bottom,
			GP->left,
			GP->top,
			GP->right);
	else
		PostScriptFileHeader(psfp,
			&(FamilyName[fontfamily][0][0]),
			papername,
			paperwidth,
			paperheight,
			landscape,
			GP->left,
			GP->bottom,
			GP->right,
			GP->top);

	fontstyle = 1;
	fontsize = 10;
	pageno = 0;
	GP->new = 1;
	return 1;
}


static void PS_Clip(double x0, double x1, double y0, double y1)
{
	PostScriptSetClipRect(psfp, x0, x1, y0, y1);
}


static void PS_Resize(void)
{
}

static void PS_NewPlot(void)
{
	pageno++;
	if(pageno > 1) PostScriptEndPage(psfp);
	PostScriptStartPage(psfp, pageno);
	PostScriptSetFont(psfp, fontstyle-1, fontsize);
	PostScriptSetLineWidth(psfp, 0.75);
	PostScriptSetColor(psfp,
			R_RED(col)/255.0,
                        R_GREEN(col)/255.0,
                        R_BLUE(col)/255.0);
	if(DP->bg != R_RGB(255,255,255)) {
		SetColor(DP->bg);
#ifdef OLD
		PostScriptFilledRectangle(psfp,
			0, 0, 72.0 * pagewidth, 72.0 * pageheight);
#else
		PostScriptFilledRectangle(psfp,
			GP->left,
			GP->bottom,
			GP->right,
			GP->top);
#endif
	}
}

static void PS_Close(void)
{
	PostScriptFileTrailer(psfp, pageno);
	fclose(psfp);
}

static void PS_MoveTo(double x, double y)
{
	PostScriptMoveTo(psfp, x, y);
}

static double PS_StrWidth(char *str)
{
	return floor(GP->cex * GP->ps + 0.5) *
	 PostScriptStringWidth((unsigned char *)str, &(metrics[GP->font-1]));
}

static void PS_MetricInfo(int c, double *ascent, double *descent, double *width)
{
	PostScriptMetricInfo(c, ascent, descent, width, &(metrics[GP->font-1]));
	*ascent = floor(GP->cex * GP->ps + 0.5) * *ascent;
	*descent = floor(GP->cex * GP->ps + 0.5) * *descent;
	*width = floor(GP->cex * GP->ps + 0.5) * *width;
}

static void PS_LineTo(double x, double y)
{
	PostScriptLineTo(psfp, x, y);
}

static void PS_StartPath(void)
{
	SetColor(GP->col);
	SetLinetype(GP->lty);
	PostScriptStartPath(psfp);
}

static void PS_EndPath(void)
{
	PostScriptEndPath(psfp);
}

static void PS_Rect(double x0, double y0, double x1, double y1, int bg, int fg)
{
	if(bg != NA_INTEGER) {
		SetColor(bg);
		PostScriptFilledRectangle(psfp, x0, y0, x1, y1);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg);
		SetLinetype(GP->lty);
		PostScriptOpenRectangle(psfp, x0, y0, x1, y1);
	}
}

static void PS_Circle(double x, double y, double r, int bg, int fg)
{
	if(bg != NA_INTEGER) {
		SetColor(bg);
		PostScriptFilledCircle(psfp, x, y, r);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg);
		SetLinetype(GP->lty);
		PostScriptOpenCircle(psfp, x, y, r);
	}
}

static void PS_Polygon(int n, double *x, double *y, int bg, int fg)
{
	if(bg != NA_INTEGER) {
		SetColor(bg);
		PostScriptFilledPolygon(psfp, x, y, n);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg);
		SetLinetype(GP->lty);
		PostScriptOpenPolygon(psfp, x, y, n);
	}
}

static void PS_Text(double x, double y, char *str, double xc, double yc, double rot)
{
	SetFont(GP->font, floor(GP->cex * GP->ps + 0.5));
	SetColor(GP->col);
	PostScriptText(psfp, x, y, str, xc, yc, rot);
}

static int PS_Locator(double *x, double *y)
{
	return 0;
}

static void PS_Mode(int mode)
{
}

static void PS_Hold(void)
{
}
