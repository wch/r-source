/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998	      Robert Gentleman, Ross Ihaka and the R core team.
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

#include "Defn.h"
#include "Graphics.h"
#include "PS.h"
#include "Errormsg.h"
#include "Fileio.h"
#include "Platform.h"
#include <stdio.h>

unsigned int str2col(char*);

typedef struct {
	char filename[128];

	char papername[64];			/* paper name */
	int paperwidth;				/* paper width in inches */
	int paperheight;			/* paper height in inches */
	int landscape;				/* landscape mode */
	int pageno;				/* page number */

	int fontfamily;				/* font family */
	int fontstyle;				/* font style, R, B, I, BI, S */
	int fontsize;				/* font size in points */
	int maxpointsize;

	double width;				/* plot width in points */
	double height;				/* plot height in points */
	double pagewidth;			/* page width in points */
	double pageheight;			/* page height in points */

	int lty;				/* current line type */
	rcolor col;				/* current color */
	rcolor bg;				/* background color */

	FILE *psfp;				/* output file */
} postscriptDesc;

static FontMetricInfo metrics[5];		/* font metrics */

	/* Device Driver Entry Point */

int PSDeviceDriver(DevDesc*, char*, char*, char*,
		char*, char*, double, double, double, double);

	/* Device Driver Actions */

static void   PS_Activate(DevDesc*);
static void   PS_Circle(double, double, int, double, int, int, DevDesc*);
static void   PS_Clip(double, double, double, double, DevDesc*);
static void   PS_Close(DevDesc*);
static void   PS_Deactivate(DevDesc*);
#ifdef NOT_used_currently/*-- (-Wall) --*/
static void   PS_EndPath(DevDesc*);
static void   PS_StartPath(DevDesc*);
#endif
static void   PS_Hold(DevDesc*);
static void   PS_Line(double, double, double, double, int, DevDesc*);
static int    PS_Locator(double*, double*, DevDesc*);
static void   PS_Mode(int);
static void   PS_NewPage(DevDesc*);
static int    PS_Open(DevDesc*, postscriptDesc*);
static void   PS_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   PS_Polyline(int, double*, double*, int, DevDesc*);
static void   PS_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   PS_Resize(DevDesc*);
static double PS_StrWidth(char*, DevDesc*);
static void   PS_MetricInfo(int, double*, double*, double*, DevDesc*);
static void   PS_Text(double, double, int, char*, double, double, double,
		      DevDesc*);

	/* Support Routines */

static void   SetColor(int, DevDesc*);
static void   SetFont(int, int, DevDesc*);
static void   SetLinetype(int, DevDesc*);
static void   SetLinewidth(DevDesc*);
static int    matchfamily(char *name);

	/*  PostScript Device Driver Parameters	 */
	/*  cpars[0] = output filename		 */
	/*  cpars[1] = paper type		 */
	/*  cpars[2] = typeface			 */
	/*  cpars[3] = background color		 */
	/*  cpars[4] = foreground color		 */
	/*  npars[0] = width in inches		 */
	/*  npars[1] = height in inches		 */
	/*  npars[2] = landscape		 */
	/*  npars[3] = pointsize		 */


int PSDeviceDriver(DevDesc *dd, char *file, char *paper, char *family,
		char *bg, char *fg, double width, double height,
		double horizontal, double ps)
{
	/* If we need to bail out with some sort of "error" */
	/* then we must free(dd) */

	double xoff, yoff, pointsize;
	postscriptDesc *pd;

		/* Check and extract the device parameters */

	if(strlen(file) > 127) {
		free(dd);
		error("filename to long in postscript\n");
	}

	/* allocate new postscript device description */
	if (!(pd = (postscriptDesc *) malloc(sizeof(postscriptDesc))))
		return 0;

	/* from here on, if need to bail out with "error", must also */
	/* free(pd) */

	/* initialise postscript device description */
	strcpy(pd->filename, file);
	strcpy(pd->papername, paper);
	pd->fontfamily = matchfamily(family);
	pd->bg = str2col(bg);
	pd->col = str2col(fg);
	pd->width = width;
	pd->height = height;
	pd->landscape = horizontal;
	pointsize = floor(ps);
	if(pd->bg == NA_INTEGER && pd->col == NA_INTEGER) {
		free(dd);
		free(pd);
		error("invalid foreground/background color (postscript)\n");
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
		pd->pagewidth  = 21.0/2.54;
		pd->pageheight = 29.7/2.54;
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
	else {
		free(dd);
		free(pd);
		error("invalid page type (postscript)\n");
	}
	pd->paperwidth = 72 * pd->pagewidth;
	pd->paperheight = 72 * pd->pageheight;
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
	xoff = (pd->pagewidth - pd->width)/2.0;
	yoff = (pd->pageheight - pd->height)/2.0;
	pd->maxpointsize = 72.0 * ((pd->pageheight > pd->pagewidth) ?
				    pd->pageheight : pd->pagewidth);

	pd->pageno = 0;
	pd->lty = 1;

	/* set graphics parameters that must be set by device driver */
		/*  Page dimensions in points  */

	dd->dp.bg = pd->bg;
	dd->dp.fg = dd->dp.col = pd->col;
	dd->dp.left = 72 * xoff;			/* left */
	dd->dp.right = 72 * (xoff + pd->width);		/* right */
	dd->dp.bottom = 72 * yoff;			/* bottom */
	dd->dp.top = 72 * (yoff + pd->height);		/* top */

		/* Base Pointsize */
		/* Nominal Character Sizes in Pixels */
		/* Only right for 12 point font. */
		/* Max pointsize suggested by Peter Dalgaard */

	if(pointsize < 6.0) pointsize = 6.0;
	if(pointsize > pd->maxpointsize) pointsize = pd->maxpointsize;
	dd->dp.ps = pointsize;
	dd->dp.cra[0] = (6.0/12.0) * pointsize;
	dd->dp.cra[1] = (10.0/12.0) * pointsize;

		/* Character Addressing Offsets */
		/* These offsets should center a single */
		/* plotting character over the plotting point. */
		/* Pure guesswork and eyeballing ... */

	dd->dp.xCharOffset =  0.4900;
	dd->dp.yCharOffset =  0.3333;
	dd->dp.yLineBias = 0.1;

		/* Inches per Raster Unit */
		/* We use points (72 dots per inch) */

	dd->dp.ipr[0] = 1.0/72.0;
	dd->dp.ipr[1] = 1.0/72.0;
	/* GREset(.)  dd->gp.mkh = dd->gp.cra[0] * dd->gp.ipr[0]; */

	dd->dp.canResizePlot = 0;
	dd->dp.canChangeFont = 1;
	dd->dp.canRotateText = 1;
	dd->dp.canResizeText = 1;
#ifdef OLD
	dd->dp.canClip = 1;
#else
	dd->dp.canClip = 0;
#endif

		/*  Start the driver */

	if(!PS_Open(dd, pd)) {
		free(pd);
		return 0;
	}

	dd->dp.open = PS_Open;
	dd->dp.close = PS_Close;
	dd->dp.activate = PS_Activate;
	dd->dp.deactivate = PS_Deactivate;
	dd->dp.resize = PS_Resize;
	dd->dp.newPage = PS_NewPage;
	dd->dp.clip = PS_Clip;
	dd->dp.text = PS_Text;
	dd->dp.strWidth = PS_StrWidth;
	dd->dp.metricInfo = PS_MetricInfo;
	dd->dp.rect = PS_Rect;
	dd->dp.circle = PS_Circle;
	dd->dp.line = PS_Line;
	dd->dp.polygon = PS_Polygon;
	dd->dp.polyline = PS_Polyline;
	dd->dp.locator = PS_Locator;
	dd->dp.mode = PS_Mode;
	dd->dp.hold = PS_Hold;

	dd->deviceSpecific = (void *) pd;

	dd->displayListOn = 0;

	return 1;
}


static char *FamilyName[][6][2] = {

	{ { "AvantGarde",				"AvantGarde",},
	  { "AvantGarde-Book",				"AvantB",},
	  { "AvantGarde-Demi",				"AvantD",},
	  { "AvantGarde-BookOblique",			"AvantBO",},
	  { "AvantGarde-DemiOblique",			"AvantDO",},
	  { "Symbol",					"Symbol",}, },

	{ { "Bookman",					"Bookman",},
	  { "Bookman-Light",				"BookL",},
	  { "Bookman-Demi",				"BookD",},
	  { "Bookman-LightItalic",			"BookLI",},
	  { "Bookman-DemiItalic",			"BookDI",},
	  { "Symbol",					"Symbol",}, },

	{ { "Courier",					"Courier",},
	  { "Courier",					"Cour",},
	  { "Courier-Bold",				"CourB",},
	  { "Courier-BoldOblique",			"CourBO",},
	  { "Courier-Oblique",				"CourO",},
	  { "Symbol",					"Symbol",}, },

	{ { "Helvetica",				"Helvetica",},
	  { "Helvetica",				"Helv",},
	  { "Helvetica-Bold",				"HelvB",},
	  { "Helvetica-Oblique",			"HelvO",},
	  { "Helvetica-BoldOblique",			"HelvBO",},
	  { "Symbol",					"Symbol",}, },

	{ { "Helvetica-Narrow",				"Helvetica-Narrow",},
	  { "Helvetica-Narrow",				"HelvN",},
	  { "Helvetica-Narrow-Bold",			"HelvNB",},
	  { "Helvetica-Narrow-Oblique",			"HelvNO",},
	  { "Helvetica-Narrow-BoldOblique",		"HelvNBO",},
	  { "Symbol",					"Symbol",}, },

	{ { "NewCenturySchoolbook",			"NewCenturySchoolbook",},
	  { "NewCenturySchlbk-Roman",			"NCSchlR",},
	  { "NewCenturySchlbk-Bold",			"NCSchlB",},
	  { "NewCenturySchlbk-Italic",			"NCSchlI",},
	  { "NewCenturySchlbk-BoldItalic",		"NCSchlBI"},
	  { "Symbol",					"Symbol",}, },

	{ { "Palatino",					"Palatino",},
	  { "Palatino-Roman",				"PalatR",},
	  { "Palatino-Bold",				"PalatB",},
	  { "Palatino-Italic",				"PalatI",},
	  { "Palatino-BoldItalic",			"PalatBI",},
	  { "Symbol",					"Symbol",}, },

	{ { "Times",					"Times",},
	  { "Times-Roman",				"TimesR",},
	  { "Times-Bold",				"TimesB",},
	  { "Times-Italic",				"TimesI",},
	  { "Times-BoldItalic",				"TimesBI",},
	  { "Symbol",					"Symbol",}, },

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

static void SetColor(int color, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	if(color != pd->col) {
		PostScriptSetColor(pd->psfp,
			R_RED(color)/255.0,
			R_GREEN(color)/255.0,
			R_BLUE(color)/255.0);
		pd->col = color;
	}
}

static void SetLinetype(int newlty, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;
	int i, ltyarray[8];

	pd->lty = newlty;
	for(i=0 ; i<8 && newlty&15 ; i++) {
		ltyarray[i] = newlty&15;
		newlty = newlty>>4;
	}
	/* the line texture is scaled by the line width */
	PostScriptSetLineTexture(pd->psfp, ltyarray, i, dd->gp.lwd*0.75);
}

static void SetLinewidth(DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	PostScriptSetLineWidth(pd->psfp, dd->gp.lwd*0.75);
}

static void SetFont(int style, int size, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

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

static int PS_Open(DevDesc *dd, postscriptDesc *pd)
{
	char buf[512];
	char *rhome;
	int i;

	if((rhome = getenv("RHOME")) == NULL)
		return 0;

	for(i=1 ; i<=5 ; i++) {
		sprintf(buf, "%s/afm/%s", rhome,
			FamilyName[pd->fontfamily][i][1]);
		if(!PostScriptLoadFontMetrics(buf, &(metrics[i-1])))
			return 0;
	}

	if (strlen(pd->filename) == 0)
		pd->psfp = popen(R_PRINTCMD, "w");
	else
		pd->psfp = R_fopen(pd->filename, "w");
	if (!pd->psfp) return 0;

	if(pd->landscape)
		PostScriptFileHeader(pd->psfp,
			&(FamilyName[pd->fontfamily][0][0]),
			pd->papername,
			pd->paperwidth,
			pd->paperheight,
			pd->landscape,
			dd->dp.bottom,
			dd->dp.left,
			dd->dp.top,
			dd->dp.right);
	else
		PostScriptFileHeader(pd->psfp,
			&(FamilyName[pd->fontfamily][0][0]),
			pd->papername,
			pd->paperwidth,
			pd->paperheight,
			pd->landscape,
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
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	PostScriptSetClipRect(pd->psfp, x0, x1, y0, y1);
}


static void PS_Resize(DevDesc *dd)
{
}

static void PS_NewPage(DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	pd->pageno++;
	if(pd->pageno > 1) PostScriptEndPage(pd->psfp);
	PostScriptStartPage(pd->psfp, pd->pageno);
	PostScriptSetFont(pd->psfp, pd->fontstyle-1, pd->fontsize);
	PostScriptSetLineWidth(pd->psfp, 0.75);
	PostScriptSetColor(pd->psfp,
			R_RED(pd->col)/255.0,
			R_GREEN(pd->col)/255.0,
			R_BLUE(pd->col)/255.0);
	if(dd->dp.bg != R_RGB(255,255,255)) {
		SetColor(dd->dp.bg, dd);
#ifdef OLD
		PostScriptFilledRectangle(psfp,
			0, 0, 72.0 * pagewidth, 72.0 * pageheight);
#else
		PostScriptFilledRectangle(pd->psfp,
			dd->gp.left,
			dd->gp.bottom,
			dd->gp.right,
			dd->gp.top);
#endif
	}
}

static void PS_Close(DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

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

static void PS_MetricInfo(int c, double *ascent, double *descent, double *width,
			  DevDesc *dd)
{
	PostScriptMetricInfo(c, ascent, descent, width,
			     &(metrics[dd->gp.font-1]));
	*ascent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *ascent;
	*descent = floor(dd->gp.cex * dd->gp.ps + 0.5) * *descent;
	*width = floor(dd->gp.cex * dd->gp.ps + 0.5) * *width;
}

#ifdef NOT_used_currently/*-- out 'def'  (-Wall) --*/
static void PS_MoveTo(double x, double y, int coords, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	GConvert(&x, &y, coords, DEVICE, dd);
	PostScriptMoveTo(pd->psfp, x, y);
}

static void PS_StartPath(DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	SetColor(dd->gp.col, dd);
	SetLinetype(dd->gp.lty, dd);
	SetLineWidth(dd);
	PostScriptStartPath(pd->psfp);
}

static void PS_EndPath(DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	PostScriptEndPath(pd->psfp);
}
#endif

static void PS_Rect(double x0, double y0, double x1, double y1, int coords,
		    int bg, int fg, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	GConvert(&x0, &y0, coords, DEVICE, dd);
	GConvert(&x1, &y1, coords, DEVICE, dd);
	if(bg != NA_INTEGER) {
		SetColor(bg, dd);
		PostScriptFilledRectangle(pd->psfp, x0, y0, x1, y1);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg, dd);
		SetLinetype(dd->gp.lty, dd);
		SetLinewidth(dd);
		PostScriptOpenRectangle(pd->psfp, x0, y0, x1, y1);
	}
}

static void PS_Circle(double x, double y, int coords, double r,
		      int bg, int fg, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	GConvert(&x, &y, coords, DEVICE, dd);
	if(bg != NA_INTEGER) {
		SetColor(bg, dd);
		PostScriptFilledCircle(pd->psfp, x, y, r);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg, dd);
		SetLinetype(dd->gp.lty, dd);
		SetLinewidth(dd);
		PostScriptOpenCircle(pd->psfp, x, y, r);
	}
}

static void PS_Line(double x1, double y1, double x2, double y2,
		    int coords, DevDesc *dd)
{
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	GConvert(&x1, &y1, coords, DEVICE, dd);
	GConvert(&x2, &y2, coords, DEVICE, dd);
	/* FIXME : clip to the device extents here */
	SetColor(dd->gp.col, dd);
	SetLinetype(dd->gp.lty, dd);
	SetLinewidth(dd);
	PostScriptStartPath(pd->psfp);
	PostScriptMoveTo(pd->psfp, x1, y1);
	PostScriptLineTo(pd->psfp, x2, y2);
	PostScriptEndPath(pd->psfp);
}

static void PS_Polygon(int n, double *x, double *y, int coords,
		       int bg, int fg, DevDesc *dd)
{
	postscriptDesc *pd;
	double xx, yy;
	int i;

	pd = (postscriptDesc *) dd->deviceSpecific;
	if(bg != NA_INTEGER) {
		SetColor(bg, dd);
		fprintf(pd->psfp, "np\n");
		xx = x[0];
		yy = y[0];
		GConvert(&xx, &yy, coords, DEVICE, dd);
		fprintf(pd->psfp, "  %.2f %.2f m\n", xx, yy);
		for(i=1 ; i<n ; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, coords, DEVICE, dd);
			fprintf(pd->psfp, "  %.2f %.2f l\n", xx, yy);
		}
		fprintf(pd->psfp, "cp f\n");
	}

	if(fg != NA_INTEGER) {
		SetColor(fg, dd);
		SetLinetype(dd->gp.lty, dd);
		SetLinewidth(dd);
		fprintf(pd->psfp, "np\n");
		xx = x[0];
		yy = y[0];
		GConvert(&xx, &yy, coords, DEVICE, dd);
		fprintf(pd->psfp, "%.2f %.2f m\n", xx, yy);
		for(i=1 ; i<n ; i++) {
			xx = x[i];
			yy = y[i];
			GConvert(&xx, &yy, coords, DEVICE, dd);
			fprintf(pd->psfp, "%.2f %.2f l\n", xx, yy);
		}
		fprintf(pd->psfp, "cp o\n");
	}
}

static void PS_Polyline(int n, double *x, double *y, int coords,
			DevDesc *dd)
{
	postscriptDesc *pd;
	double xx, yy;
	int i;

	pd = (postscriptDesc*) dd->deviceSpecific;
	SetColor(dd->gp.col, dd);
	SetLinetype(dd->gp.lty, dd);
	SetLinewidth(dd);
	fprintf(pd->psfp, "np\n");
	xx = x[0];
	yy = y[0];
	GConvert(&xx, &yy, coords, DEVICE, dd);
	fprintf(pd->psfp, "%.2f %.2f m\n", xx, yy);
	for(i=1 ; i<n ; i++) {
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
	postscriptDesc *pd = (postscriptDesc *) dd->deviceSpecific;

	GConvert(&x, &y, coords, DEVICE, dd);
	SetFont(dd->gp.font, floor(dd->gp.cex * dd->gp.ps + 0.5), dd);
	SetColor(dd->gp.col, dd);
	PostScriptText(pd->psfp, x, y, str, xc, yc, rot);
}

static int PS_Locator(double *x, double *y, DevDesc *dd)
{
	return 0;
}

static void PS_Mode(int mode)
{
}

static void PS_Hold(DevDesc *dd)
{
}
