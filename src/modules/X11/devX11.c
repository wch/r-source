/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
# include <config.h>
#endif

/*
#define OLD
*/

#ifdef HAVE_RINT
#define R_rint(x) rint(x)
#else
#define R_rint(x) ((int) x + 0.5)
#endif

#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>	/*->	Xlib.h	Xutil.h Xresource.h .. */


#include "Defn.h"
#include "Graphics.h"
#include "Fileio.h"		/* R_fopen */
#include "rotated.h"		/* 'Public' routines from here */
/* For the input handlers of the event loop mechanism: */
#include <R_ext/eventloop.h>
#include <R_ext/Memory.h>	/* vmaxget */
#include "Rdevices.h"

#define R_X11_DEVICE 1
#include "devX11.h"

#include <R_ext/RX11.h>

#define CURSOR		XC_crosshair		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

#define IS_100DPI ((int) (1./pixelHeight() + 0.5) == 100)


#define X_BELL_VOLUME 0 /* integer between -100 and 100 for the volume
                            of the bell in locator. */
			/* Note: This is in relation to
			the general bell level. Was 50, but if > 0
			then "xset b off" will not disable the
			locator bell - pd 2002-3-11 */
/* a colour used to represent the background on png if transparent
   NB: must be grey as used as RGB and BGR
*/
#define PNG_TRANS 0xfefefe

	/********************************************************/
	/* If there are resources that are shared by all devices*/
	/* of this type, you may wish to make them globals	*/
	/* rather than including them in the device-specific	*/
	/* parameters structure (especially if they are large !)*/
	/********************************************************/

	/* X11 Driver Specific parameters
	 * with only one copy for all x11 devices */

static Display *display;			/* Display */
static int screen;				/* Screen */
static Window rootwin;				/* Root Window */
static Visual *visual;				/* Visual */
static int depth;				/* Pixmap depth */
static int Vclass;				/* Visual class */
static X_COLORTYPE model;			/* User color model */
static int maxcubesize;				/* Max colorcube size */
static XSetWindowAttributes attributes;		/* Window attributes */
static Colormap colormap;			/* Default color map */
static int blackpixel;				/* Black */
static int whitepixel;				/* White */
static XContext devPtrContext;
static Atom _XA_WM_PROTOCOLS, protocol;

static Rboolean displayOpen = FALSE;
static Rboolean inclose = FALSE;
static int numX11Devices = 0;

	/********************************************************/
	/* There must be an entry point for the device driver	*/
	/* which will create device-specific resources,		*/
	/* initialise the device-specific parameters structure	*/
	/* and return whether the setup succeeded		*/
	/* This is called by the graphics engine when the user	*/
	/* creates a new device of this type			*/
	/********************************************************/


	/********************************************************/
	/* There are a number of actions that every device	*/
	/* driver is expected to perform (even if, in some	*/
	/* cases it does nothing - just so long as it doesn't	*/
	/* crash !).  this is how the graphics engine interacts */
	/* with each device. ecah action will be documented	*/
	/* individually.					*/
	/* hooks for these actions must be set up when the	*/
	/* device is first created				*/
	/********************************************************/

	/* Device Driver Actions */

static void newX11_Activate(NewDevDesc *dd);
static void newX11_Circle(double x, double y, double r,
			  int col, int fill, double gamma,
			  int lty, double lwd,
			  NewDevDesc *dd);
static void newX11_Clip(double x0, double x1, double y0, double y1,
		     NewDevDesc *dd);
static void newX11_Close(NewDevDesc *dd);
static void newX11_Deactivate(NewDevDesc *dd);
static void newX11_Hold(NewDevDesc *dd);
static Rboolean newX11_Locator(double *x, double *y, NewDevDesc *dd);
static void newX11_Line(double x1, double y1, double x2, double y2,
			int col, double gamma, int lty, double lwd,
			NewDevDesc *dd);
static void newX11_MetricInfo(int c, int font, double cex, double ps,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd);
static void newX11_Mode(int mode, NewDevDesc *dd);
static void newX11_NewPage(int fill, double gamma, NewDevDesc *dd);
Rboolean newX11_Open(NewDevDesc *dd, newX11Desc *xd,
		     char *dsp, double w, double h,
		     double gamma_fac, X_COLORTYPE colormodel,
		     int maxcube, int canvascolor);
static void newX11_Polygon(int n, double *x, double *y,
			   int col, int fill, double gamma,
			   int lty, double lwd,
			   NewDevDesc *dd);
static void newX11_Polyline(int n, double *x, double *y,
			    int col, double gamma, int lty, double lwd,
			    NewDevDesc *dd);
static void newX11_Rect(double x0, double y0, double x1, double y1,
			int col, int fill, double gamma,
			int lty, double lwd,
			NewDevDesc *dd);
static void newX11_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd);
static double newX11_StrWidth(char *str, int font,
			      double cex, double ps, NewDevDesc *dd);
static void newX11_Text(double x, double y, char *str,
			double rot, double hadj,
			int col, double gamma,
			int font, double cex, double ps,
			NewDevDesc *dd);

	/*************************************************/
	/* End of list of required device driver actions */
	/*************************************************/

	/* Support Routines */

static XFontStruct *RLoadFont(int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static int SetBaseFont(newX11Desc*);
static void SetColor(int, NewDevDesc*);
static void SetFont(int, int, NewDevDesc*);
static void SetLinetype(int, double, NewDevDesc*);
static void X11_Close_bitmap(newX11Desc *xd);



	/************************/
	/* X11 Color Management */
	/************************/

static double RedGamma	 = 0.6;
static double GreenGamma = 0.6;
static double BlueGamma	 = 0.6;


/* Variables Used To Store Colormap Information */
static struct { int red; int green; int blue; } RPalette[512];
static XColor XPalette[512];
static int PaletteSize;


/* Monochome Displays : Compute pixel values by converting */
/* RGB values to luminance and then thresholding. */
/* See: Foley & van Damm. */

static void SetupMonochrome()
{
    depth = 1;
}

static unsigned GetMonochromePixel(int r, int g, int b)
{
    if ((int)(0.299 * r + 0.587 * g + 0.114 * b) > 127)
	return WhitePixel(display, screen);
    else
	return BlackPixel(display, screen);
}


/* Grayscale Displays : Compute pixel values by converting */
/* RGB values to luminance.  See: Foley & van Damm. */

static unsigned GetGrayScalePixel(int r, int g, int b)
{
    unsigned int d, dmin = 0xFFFFFFFF;
    unsigned int dr;
    int i;
    unsigned int pixel = 0;  /* -Wall */
    int gray = (0.299 * r + 0.587 * g + 0.114 * b) + 0.0001;
    for (i = 0; i < PaletteSize; i++) {
	dr = (RPalette[i].red - gray);
	d = dr * dr;
	if (d < dmin) {
	    pixel = XPalette[i].pixel;
	    dmin = d;
	}
    }
    return pixel;
}

static Rboolean GetGrayPalette(Display *displ, Colormap cmap, int n)
{
    int status, i, m;
    m = 0;
    i = 0;
    for (i = 0; i < n; i++) {
	RPalette[i].red	  = (i * 0xff) / (n - 1);
	RPalette[i].green = RPalette[i].red;
	RPalette[i].blue  = RPalette[i].red;
	/* Gamma correct here */
	XPalette[i].red	  = (i * 0xffff) / (n - 1);
	XPalette[i].green = XPalette[i].red;
	XPalette[i].blue  = XPalette[i].red;
	status = XAllocColor(displ, cmap, &XPalette[i]);
	if (status == 0) {
	    XPalette[i].flags = 0;
	    m++;
	}
	else
	    XPalette[i].flags = DoRed|DoGreen|DoBlue;
    }
    PaletteSize = n;
    if (m > 0) {
	for (i = 0; i < PaletteSize; i++) {
	    if (XPalette[i].flags != 0)
		XFreeColors(displ, cmap, &(XPalette[i].pixel), 1, 0);
	}
	PaletteSize = 0;
	return FALSE;
    }
    else return TRUE;
}

static void SetupGrayScale()
{
    int res = 0, d;
    PaletteSize = 0;
    /* try for 128 grays on an 8-bit display */
    if (depth > 8) d = depth = 8; else d = depth - 1;
    /* try (256), 128, 64, 32, 16 grays */
    while (d >= 4 && !(res = GetGrayPalette(display, colormap, 1 << d)))
	d--;
    if (!res) {
	/* Can't find a sensible grayscale, so revert to monochrome */
	warning("can't set grayscale: reverting to monochrome");
	model = MONOCHROME;
	SetupMonochrome();
    }
}

/* PseudoColor Displays : There are two strategies here. */
/* 1) allocate a standard color cube and match colors */
/* within that based on (weighted) distances in RGB space. */
/* 2) allocate colors exactly as they are requested until */
/* all color cells are used.  Fail with an error message */
/* when this happens. */

static int RGBlevels[][3] = {  /* PseudoColor Palettes */
    { 8, 8, 4 },
    { 6, 7, 6 },
    { 6, 6, 6 },
    { 6, 6, 5 },
    { 6, 6, 4 },
    { 5, 5, 5 },
    { 5, 5, 4 },
    { 4, 4, 4 },
    { 4, 4, 3 },
    { 3, 3, 3 },
    { 2, 2, 2 }
};
static int NRGBlevels = sizeof(RGBlevels) / (3 * sizeof(int));


static int GetColorPalette(Display *dpy, Colormap cmap, int nr, int ng, int nb)
{
    int status, i, m, r, g, b;
    m = 0;
    i = 0;
    for (r = 0; r < nr; r++) {
	for (g = 0; g < ng; g++) {
	    for (b = 0; b < nb; b++) {
		RPalette[i].red	  = (r * 0xff) / (nr - 1);
		RPalette[i].green = (g * 0xff) / (ng - 1);
		RPalette[i].blue  = (b * 0xff) / (nb - 1);
		/* Perform Gamma Correction Here */
#ifdef OLD
		XPalette[i].red	  = (r * 0xffff) / (nr - 1);
		XPalette[i].green = (g * 0xffff) / (ng - 1);
		XPalette[i].blue  = (b * 0xffff) / (nb - 1);
#else
		XPalette[i].red	  = pow(r / (nr - 1.0), RedGamma) * 0xffff;
		XPalette[i].green = pow(g / (ng - 1.0), GreenGamma) * 0xffff;
		XPalette[i].blue  = pow(b / (nb - 1.0), BlueGamma) * 0xffff;
#endif
		/* End Gamma Correction */
		status = XAllocColor(dpy, cmap, &XPalette[i]);
		if (status == 0) {
		    XPalette[i].flags = 0;
		    m++;
		}
		else
		    XPalette[i].flags = DoRed|DoGreen|DoBlue;
		i++;
	    }
	}
    }
    PaletteSize = nr * ng * nb;
    if (m > 0) {
	for (i = 0; i < PaletteSize; i++) {
	    if (XPalette[i].flags != 0)
		XFreeColors(dpy, cmap, &(XPalette[i].pixel), 1, 0);
	}
	PaletteSize = 0;
	return 0;
    }
    else
	return 1;
}

static void SetupPseudoColor()
{
    int i, size;
    PaletteSize = 0;
    if (model == PSEUDOCOLOR1) {
	for (i = 0; i < NRGBlevels; i++) {
	    size = RGBlevels[i][0] * RGBlevels[i][1] * RGBlevels[i][2];
	    if (size < maxcubesize && GetColorPalette(display, colormap,
				RGBlevels[i][0],
				RGBlevels[i][1],
				RGBlevels[i][2]))
		break;
	}
	if (PaletteSize == 0) {
	    warning("X11 driver unable to obtain color cube\n  reverting to monochrome");
	    model = MONOCHROME;
	    SetupMonochrome();
	}
    }
    else {
	PaletteSize = 0;
    }
}

static unsigned int GetPseudoColor1Pixel(int r, int g, int b)
{
    unsigned int d, dmin = 0xFFFFFFFF;
    unsigned int dr, dg, db;
    unsigned int pixel;
    int i;
    pixel = 0;			/* -Wall */
    for (i = 0; i < PaletteSize; i++) {
	dr = (RPalette[i].red - r);
	dg = (RPalette[i].green - g);
	db = (RPalette[i].blue - b);
	d = dr * dr + dg * dg + db * db;
	if (d < dmin) {
	    pixel = XPalette[i].pixel;
	    dmin = d;
	}
    }
    return pixel;
}

static unsigned int GetPseudoColor2Pixel(int r, int g, int b)
{
    int i;
    /* Search for previously allocated color */
    for (i = 0; i < PaletteSize ; i++) {
	if (r == RPalette[i].red &&
	    g == RPalette[i].green &&
	    b == RPalette[i].blue) return XPalette[i].pixel;
    }
    /* Attempt to allocate a new color */
    XPalette[PaletteSize].red	= pow(r / 255.0, RedGamma) * 0xffff;
    XPalette[PaletteSize].green = pow(g / 255.0, GreenGamma) * 0xffff;
    XPalette[PaletteSize].blue	= pow(b / 255.0, BlueGamma) * 0xffff;
    if (PaletteSize == 256 ||
	XAllocColor(display, colormap, &XPalette[PaletteSize]) == 0) {
	error("Error: X11 cannot allocate additional graphics colors.\n"
	      "Consider using X11 with colortype=\"pseudo.cube\" or \"gray\".");
    }
    RPalette[PaletteSize].red = r;
    RPalette[PaletteSize].green = g;
    RPalette[PaletteSize].blue = b;
    PaletteSize++;
    return XPalette[PaletteSize - 1].pixel;
}

static unsigned int GetPseudoColorPixel(int r, int g, int b)
{
    if (model == PSEUDOCOLOR1)
	return GetPseudoColor1Pixel(r, g, b);
    else
	return GetPseudoColor2Pixel(r, g, b);
}

/* Truecolor Displays : Allocate the colors as they are requested */

static unsigned int RMask, RShift;
static unsigned int GMask, GShift;
static unsigned int BMask, BShift;

static void SetupTrueColor()
{
    RMask = visual->red_mask;
    GMask = visual->green_mask;
    BMask = visual->blue_mask;
    RShift = 0; while ((RMask & 1) == 0) { RShift++; RMask >>= 1; }
    GShift = 0; while ((GMask & 1) == 0) { GShift++; GMask >>= 1; }
    BShift = 0; while ((BMask & 1) == 0) { BShift++; BMask >>= 1; }
}

static unsigned GetTrueColorPixel(int r, int g, int b)
{
    r = pow((r / 255.0), RedGamma) * 255;
    g = pow((g / 255.0), GreenGamma) * 255;
    b = pow((b / 255.0), BlueGamma) * 255;
    return
	(((r * RMask) / 255) << RShift) |
	(((g * GMask) / 255) << GShift) |
	(((b * BMask) / 255) << BShift);
}

/* Interface for General Visual */

static unsigned int GetX11Pixel(int r, int g, int b)
{
    switch(model) {
    case MONOCHROME:
	return GetMonochromePixel(r, g, b);
    case GRAYSCALE:
	return GetGrayScalePixel(r, g, b);
    case PSEUDOCOLOR1:
    case PSEUDOCOLOR2:
	return GetPseudoColorPixel(r, g, b);
    case TRUECOLOR:
	return GetTrueColorPixel(r, g, b);
    default:
	printf("Unknown Visual\n");
    }
    return 0;
}

static void FreeX11Colors()
{
    int i;
    if (model == PSEUDOCOLOR2) {
	for (i = 0; i < PaletteSize; i++)
	    XFreeColors(display, colormap, &(XPalette[i].pixel), 1, 0);
	PaletteSize = 0;
    }
}

static Rboolean SetupX11Color()
{
    if (depth <= 1) {
	/* On monchome displays we must use black/white */
	model = MONOCHROME;
	SetupMonochrome();
    }
    else if (Vclass ==	StaticGray || Vclass == GrayScale) {
	if (model == MONOCHROME)
	    SetupMonochrome();
	else {
	    model = GRAYSCALE;
	    SetupGrayScale();
	}
    }
    else if (Vclass == StaticColor) {
	/* FIXME : Currently revert to mono. */
	/* Should do the real thing. */
	model = MONOCHROME;
	SetupMonochrome();
    }
    else if (Vclass ==	PseudoColor) {
	if (model == MONOCHROME)
	    SetupMonochrome();
	else if (model == GRAYSCALE)
	    SetupGrayScale();
	else {
	    if (model == TRUECOLOR)
		model = PSEUDOCOLOR2;
	    SetupPseudoColor();
	}
    }
    else if (Vclass == TrueColor) {
	if (model == MONOCHROME)
	    SetupMonochrome();
	else if (model == GRAYSCALE)
	    SetupGrayScale();
	else if (model == PSEUDOCOLOR1 || model == PSEUDOCOLOR2)
	    SetupPseudoColor();
	else
	    SetupTrueColor();
    }
    else if (Vclass == DirectColor) {
	/* FIXME : Currently revert to mono. */
	/* Should do the real thing. */
	model = MONOCHROME;
	SetupMonochrome();
    }
    else {
	printf("Unknown Visual\n");
	return FALSE;
    }
    return TRUE;
}

	/* Pixel Dimensions (Inches) */


static double pixelWidth(void)
{
    double width, widthMM;
    width = DisplayWidth(display, screen);
    widthMM = DisplayWidthMM(display, screen);
    return ((double)widthMM / (double)width) / MM_PER_INCH;
}

static double pixelHeight(void)
{
    double height, heightMM;
    height = DisplayHeight(display, screen);
    heightMM = DisplayHeightMM(display, screen);
    return ((double)heightMM / (double)height) / MM_PER_INCH;
}

static void handleEvent(XEvent event)
{
    caddr_t temp;
    NewDevDesc *dd;
    newX11Desc *xd;
    int devNum = 0;

    if (event.xany.type == Expose) {
	while(XCheckTypedEvent(display, Expose, &event))
	    ;
	XFindContext(display, event.xexpose.window,
		     devPtrContext, &temp);
	dd = (NewDevDesc *) temp;
	xd = (newX11Desc *) dd->deviceSpecific;
	if (xd->resize) {
	    dd->size(&(dd->left), &(dd->right), &(dd->bottom), &(dd->top),
		     dd);
	    xd->resize = 0;
	}
	/* It appears possible that a device may receive an expose
	 * event in the middle of the device being "kill"ed by R
	 * This means that R knows nothing about the device
	 * so devNumber becomes 0 (the null device) and it is not
	 * a good idea to pass the null device to GEplayDisplayList
	 */
	devNum = devNumber((DevDesc*) dd);
	if (devNum > 0)
	    GEplayDisplayList((GEDevDesc*) GetDevice(devNum));
    }
    else if (event.type == ConfigureNotify) {
	XFindContext(display, event.xconfigure.window,
		     devPtrContext, &temp);
	dd = (NewDevDesc *) temp;
	xd = (newX11Desc *) dd->deviceSpecific;
	xd->windowWidth = event.xconfigure.width;
	xd->windowHeight = event.xconfigure.height;
	xd->resize = 1;
    }
    else if ((event.type == ClientMessage) &&
	     (event.xclient.message_type == _XA_WM_PROTOCOLS))
	if (!inclose && event.xclient.data.l[0] == protocol) {
	    XFindContext(display, event.xclient.window,
			 devPtrContext, &temp);
	    dd = (NewDevDesc *) temp;
	    KillDevice((DevDesc*) GetDevice(devNumber((DevDesc*) dd)));
	}
}

static void R_ProcessEvents(void *data)
{
    XEvent event;

    while (displayOpen && XPending(display)) {
	XNextEvent(display, &event);
	/* printf("%i\n",event.type); */
	handleEvent(event);
    }
}

#ifdef OLD
	/* Font information array. */
	/* Point sizes: 6-24 */
	/* Faces: plain, bold, oblique, bold-oblique */
	/* Symbol may be added later */

#define NFONT 19

static XFontStruct *fontarray[NFONT][5] = {
    {NULL, NULL, NULL, NULL, NULL},	/*  6 */
    {NULL, NULL, NULL, NULL, NULL},	/*  7 */
    {NULL, NULL, NULL, NULL, NULL},	/*  8 */
    {NULL, NULL, NULL, NULL, NULL},	/*  9 */
    {NULL, NULL, NULL, NULL, NULL},	/* 10 */
    {NULL, NULL, NULL, NULL, NULL},	/* 11 */
    {NULL, NULL, NULL, NULL, NULL},	/* 12 */
    {NULL, NULL, NULL, NULL, NULL},	/* 13 */
    {NULL, NULL, NULL, NULL, NULL},	/* 14 */
    {NULL, NULL, NULL, NULL, NULL},	/* 15 */
    {NULL, NULL, NULL, NULL, NULL},	/* 16 */
    {NULL, NULL, NULL, NULL, NULL},	/* 17 */
    {NULL, NULL, NULL, NULL, NULL},	/* 18 */
    {NULL, NULL, NULL, NULL, NULL},	/* 19 */
    {NULL, NULL, NULL, NULL, NULL},	/* 20 */
    {NULL, NULL, NULL, NULL, NULL},	/* 21 */
    {NULL, NULL, NULL, NULL, NULL},	/* 22 */
    {NULL, NULL, NULL, NULL, NULL},	/* 23 */
    {NULL, NULL, NULL, NULL, NULL},	/* 24 */
};

static int missingfont[NFONT][5] = {
    {0, 0, 0, 0, 0},	/*  6 */
    {0, 0, 0, 0, 0},	/*  7 */
    {0, 0, 0, 0, 0},	/*  8 */
    {0, 0, 0, 0, 0},	/*  9 */
    {0, 0, 0, 0, 0},	/* 10 */
    {0, 0, 0, 0, 0},	/* 11 */
    {0, 0, 0, 0, 0},	/* 12 */
    {0, 0, 0, 0, 0},	/* 13 */
    {0, 0, 0, 0, 0},	/* 14 */
    {0, 0, 0, 0, 0},	/* 15 */
    {0, 0, 0, 0, 0},	/* 16 */
    {0, 0, 0, 0, 0},	/* 17 */
    {0, 0, 0, 0, 0},	/* 18 */
    {0, 0, 0, 0, 0},	/* 19 */
    {0, 0, 0, 0, 0},	/* 20 */
    {0, 0, 0, 0, 0},	/* 21 */
    {0, 0, 0, 0, 0},	/* 22 */
    {0, 0, 0, 0, 0},	/* 23 */
    {0, 0, 0, 0, 0},	/* 24 */
};

static char *fontname_R6 = "-adobe-helvetica-%s-%s-*-*-*-%d-*-*-*-*-*-*";
static char *fontname_R5 = "-adobe-helvetica-%s-%s-*-*-*-%d-*-*-*-*-*-*";
static char *symbolname	 = "-adobe-symbol-*-*-*-*-*-%d-*-*-*-*-*-*";
static char *fontname;

static char *slant[]  = {"r", "o"};
static char *weight[] = {"medium", "bold"};

	/* attempt to load a font into the fontarray and return the font */
	/* if the font is already there don't load it again */
	/* if can't load the font, return NULL */

static XFontStruct *RLoadFont(int face, int size)
{
    if (fontarray[size-6][face-1])
	return fontarray[size-6][face-1];
    else {
	char buf[128];
	XFontStruct *tmp;

	if (face == 5)
	    sprintf(buf, symbolname, 10 * size);
	else
	    sprintf(buf, fontname,
		    weight[(face-1)%2],
		    slant[((face-1)/2)%2], 10 * size);
#ifdef DEBUG_X11
	Rprintf("loading:\n%s\n",buf);
#endif
	tmp = XLoadQueryFont(display, buf);
#ifdef DEBUG_X11
	if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
	if (tmp)
	    fontarray[size-6][face-1] = tmp;
	return tmp;
    }
}

			/* Quiz the server about fonts. */
			/* 1) Try for 100dpi (X11R6) font */
			/* 2) Try for *dpi (X11R6) font */
			/* 3) Try "fixed" and if that fails, bail out */

static int SetBaseFont(newX11Desc *xd)
{
    xd->fontface = 1;
    xd->fontsize = 12;
    xd->usefixed = 0;
    fontname = fontname_R6;
    xd->font = RLoadFont(xd->fontface, xd->fontsize);
    if (!xd->font) {
	fontname = fontname_R5;
	xd->font = RLoadFont(xd->fontface, xd->fontsize);
    }
    if (!xd->font) {
	xd->usefixed = 1;
	xd->font = xd->fixedfont = XLoadQueryFont(display, "fixed");
	if (!xd->fixedfont)
	    return 0;
    }
    return 1;
}

			/* Set the font size and face */
			/* If the font of this size and at that the specified */
			/* rotation is not present it is loaded. */
			/* 0 = plain text, 1 = bold */
			/* 2 = oblique, 3 = bold-oblique */

#define SMALLEST 8
#define LARGEST 24

static void SetFont(int face, int size, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (face < 1 || face > 5) face = 1;
    size = 2 * size / 2;
    if (size < SMALLEST) size = SMALLEST;
    if (size > LARGEST) size = LARGEST;

    if (!xd->usefixed && (size != xd->fontsize	|| face != xd->fontface)) {
	while(size < dd->gp.ps) {
	    if (fontarray[size-6][face-1]) goto found;
	    if (!missingfont[size-6][face-1]) {
		fontarray[size-6][face-1] = RLoadFont(face, size);
		if (fontarray[size-6][face-1]) goto found;
		missingfont[size-6][face-1] = 1;
	    }
	    size += 2;
	}
	while(size >= dd->gp.ps) {
	    if (fontarray[size-6][face-1]) goto found;
	    if (!missingfont[size-6][face-1]) {
		fontarray[size-6][face-1] = RLoadFont(face, size);
		if (fontarray[size-6][face-1]) goto found;
		missingfont[size-6][face-1] = 1;
	    }
	    size -= 2;
	}
	size = dd->gp.ps;
	face = 1;
    found:
	xd->font = fontarray[size-6][face-1];
	xd->fontface = face;
	xd->fontsize = size;
	XSetFont(display, xd->wgc, xd->font->fid);
    }
}
#else /* not OLD */
static char *fontname = "-*-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*";
static char *symbolname	 = "-*-symbol-*-*-*-*-%d-*-*-*-*-*-*-*";

static char *slant[]  = {"r", "o"};
static char *weight[] = {"medium", "bold"};

/* Bitmap of the Adobe design sizes */

static unsigned int adobe_sizes = 0x0403165D;

#define MAXFONTS 64
#define CLRFONTS 16 /* Number to free when cache runs full */

typedef struct {int face, size;	 XFontStruct *font;} cacheentry;

static cacheentry fontcache[MAXFONTS];
static int nfonts = 0;
static int force_nonscalable = 0; /* for testing */

#define ADOBE_SIZE(I) ((I) > 7 && (I) < 35 && (adobe_sizes & (1<<((I)-8))))
#define SMALLEST 2

static XFontStruct *RLoadFont(int face, int size)
{
    int pixelsize, i;
    cacheentry *f;
    char buf[128];
    XFontStruct *tmp;

    if (size < SMALLEST) size = SMALLEST;
    face--;

    /* Here's a 1st class fudge: make sure that the Adobe design sizes
       8, 10, 11, 12, 14, 17, 18, 20, 24, 25, 34 can be obtained via
       an integer "size" at 100 dpi, namely 6, 7, 8, 9, 10, 12, 13,
       14, 17, 18, 24 points. It's almost y = x * 100/72, but not
       quite. The constants were found using lm(). --pd */
    if (IS_100DPI) size = R_rint(size * 1.43 - 0.4);

    /* search fontcache */
    for ( i = nfonts ; i-- ; ) {
	f = &fontcache[i];
	if ( f->face == face && f->size == size ) return f->font;
    }

    /* 'size' is the requested size, 'pixelsize'  the size of the
       actually allocated font*/
    pixelsize = size;

    if (face == 4)
	sprintf(buf, symbolname,  pixelsize);
    else
	sprintf(buf, fontname,
		weight[face & 1],
		slant[(face & 2)>>1 ],	pixelsize);
#ifdef DEBUG_X11
    Rprintf("loading:\n%s\n",buf);
#endif
    tmp = XLoadQueryFont(display, buf);
#ifdef DEBUG_X11
    if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
    if (!tmp || (force_nonscalable && !ADOBE_SIZE(size)) ){
	static int near[]=
	  {14,14,14,17,17,18,20,20,20,20,24,24,24,25,25,25,25};
	/* 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29  */

	/* If ADOBE_SIZE(pixelsize) is true at this point then
	   the user's system does not have the standard ADOBE font set
	   so we just have to use a "fixed" font.
	   If we can't find a "fixed" font then something is seriously
	   wrong */
	if ( ADOBE_SIZE(pixelsize) ) {
	    tmp = XLoadQueryFont(display, "fixed");
	    if (tmp)
		return tmp;
	    else
		error("Could not find any X11 fonts\nCheck that the Font Path is correct.");
	}

	if ( pixelsize < 8 )
	    pixelsize = 8;
	else if (pixelsize == 9)
	    pixelsize = 8;
	else if (pixelsize < 30) /* must be at least 13 */
	    pixelsize = near[size-13];
	else
	    pixelsize = 34;


	if (face == 4)
	    sprintf(buf, symbolname, pixelsize);
	else
	    sprintf(buf, fontname,
		    weight[face & 1],
		    slant[(face & 2)>>1 ],  pixelsize);
#ifdef DEBUG_X11
	Rprintf("loading:\n%s\n",buf);
#endif
	tmp = XLoadQueryFont(display, buf);
#ifdef DEBUG_X11
	if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
    }
    if(!tmp && size > 24) {
	/* final try, size 24 */
	pixelsize = 24;
	if (face == 4)
	    sprintf(buf, symbolname, 24);
	else
	    sprintf(buf, fontname,
		    weight[face & 1],
		    slant[(face & 2)>>1 ],  24);
#ifdef DEBUG_X11
	Rprintf("loading:\n%s\n",buf);
#endif
	tmp = XLoadQueryFont(display, buf);
#ifdef DEBUG_X11
	if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
    }


    if (tmp){
	f = &fontcache[nfonts++];
	f->face = face;
	f->size = size;
	f->font = tmp;
	if (fabs( (pixelsize - size)/(double)size ) > 0.1)
	    warning("X11 used font size %d when %d was requested",
		    pixelsize, size);
    }
    if (nfonts == MAXFONTS) /* make room in the font cache */
    {
	for (i = 0 ; i < CLRFONTS ; i++)
	    XFreeFont(display, fontcache[i].font);
	for (i = CLRFONTS ; i < MAXFONTS ; i++)
	    fontcache[i - CLRFONTS] = fontcache[i];
	nfonts -= CLRFONTS;
    }
    return tmp;
}

static int SetBaseFont(newX11Desc *xd)
{
    xd->fontface = xd->basefontface;
    xd->fontsize = xd->basefontsize;
    xd->usefixed = 0;
    xd->font = RLoadFont(xd->fontface, xd->fontsize);
    if (!xd->font) {
	xd->usefixed = 1;
	xd->font = xd->fixedfont = XLoadQueryFont(display, "fixed");
	if (!xd->fixedfont)
	    return 0;
    }
    return 1;
}

static void SetFont(int face, int size, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    XFontStruct *tmp;

    if (face < 1 || face > 5) face = 1;

    if (!xd->usefixed && (size != xd->fontsize	|| face != xd->fontface)) {
	tmp = RLoadFont(face, size);
	if(tmp) {
	    xd->font = tmp;
	    xd->fontface = face;
	    xd->fontsize = size;
	    XSetFont(display, xd->wgc, xd->font->fid);
	} else
	    error("X11 font at size %d could not be loaded", size);
    }
}
#endif


static void SetColor(int color, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    if (color != xd->col) {
	blackpixel = GetX11Pixel(R_RED(color), R_GREEN(color), R_BLUE(color));
	xd->col = color;
	XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
    }
}

/* --> See "Notes on Line Textures" in ../../include/Rgraphics.h
 *
 *	27/5/98 Paul - change to allow lty and lwd to interact:
 *	the line texture is now scaled by the line width so that,
 *	for example, a wide (lwd=2) dotted line (lty=2) has bigger
 *	dots which are more widely spaced.  Previously, such a line
 *	would have "dots" which were wide, but not long, nor widely
 *	spaced.
 */
static void SetLinetype(int newlty, double nlwd, NewDevDesc *dd)
{
    static char dashlist[8];
    int i, newlwd;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    newlwd = nlwd;/*cast*/
    if (newlwd < 1)/* not less than 1 pixel */
	newlwd = 1;
    if (newlty != xd->lty || newlwd != xd->lwd) {
	xd->lty = newlty;
	xd->lwd = newlwd;
	if (newlty == 0) {/* special hack for lty = 0 -- only for X11 */
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineSolid,
			       CapRound,
			       JoinRound);
	}
	else {
	    for(i = 0 ; i < 8 && (newlty != 0); i++) {
		int j = newlty & 15;
		if (j == 0) j = 1; /* Or we die with an X Error */
		/* scale line texture for line width */
		j = j*newlwd;
		/* make sure that scaled line texture */
		/* does not exceed X11 storage limits */
		if (j > 255) j=255;
		dashlist[i] = j;
		newlty = newlty >> 4;
	    }
	    XSetDashes(display, xd->wgc, 0, dashlist, i);
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineOnOffDash,
			       CapButt,
			       JoinRound);
	}
    }
}

static int R_X11Err(Display *dsp, XErrorEvent *event)
{
    char buff[1000];
    XGetErrorText(dsp, event->error_code, buff, 1000);
    warning("X11 protocol error: %s", buff);
    return 0;
}

static int R_X11IOErr(Display *dsp)
{
    int fd = ConnectionNumber(display);
    /*
    while (nfonts--)  XFreeFont(display, fontcache[nfonts].font);
    nfonts = 0;
    */
    removeInputHandler(&R_InputHandlers,
		       getInputHandler(R_InputHandlers,fd));
    /*
    XCloseDisplay(display);
    displayOpen = FALSE;
    */
    error("X11 fatal IO error: please save work and shut down R");
    return 0; /* but should never get here */
}


Rboolean
newX11_Open(NewDevDesc *dd, newX11Desc *xd, char *dsp, double w, double h,
	 double gamma_fac, X_COLORTYPE colormodel, int maxcube,
	 int canvascolor)
{
    /* if we have to bail out with "error", then must free(dd) and free(xd) */
    /* That means the *caller*: the X11DeviceDriver code frees xd, for example */

    XEvent event;
    int iw, ih;
    X_GTYPE type;
    char *p = dsp;
    XGCValues gcv;
    /* Indicates whether the display is created within this particular call: */
    Rboolean DisplayOpened = FALSE;

    if (!strncmp(dsp, "png::", 5)) {
	char buf[512];
	FILE *fp;
#ifndef HAVE_PNG
	warning("No png support in this version of R");
	return FALSE;
#endif
	strcpy(xd->filename, dsp+5);
	sprintf(buf, dsp+5, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning("could not open PNG file `%s'", buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = PNG;
	p = "";
    }
    else if (!strncmp(dsp, "jpeg::", 6)) {
	char buf[512];
	FILE *fp;
#ifndef HAVE_JPEG
	warning("No jpeg support in this version of R");
	return FALSE;
#endif
	p = strchr(dsp+6, ':'); *p='\0';
	xd->quality = atoi(dsp+6);
	strcpy(xd->filename, p+1);
	sprintf(buf, p+1, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning("could not open JPEG file `%s'", buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = JPEG;
	p = "";
    } else if (!strcmp(dsp, "XImage")) {
	type = XIMAGE;
	xd->fp = NULL;
	p = "";
    }
    else type = WINDOW;
    xd->type = type;

    /* If there is no server connection, establish one and */
    /* initialize the X11 device driver data structures. */

    if (!displayOpen) {
	if ((display = XOpenDisplay(p)) == NULL) {
	    warning("unable to open connection to X11 display`%s'", p);
	    return FALSE;
	}
	DisplayOpened = TRUE;
	Rf_setX11Display(display, gamma_fac, colormodel, maxcube, TRUE);
	displayOpen = TRUE;
	if(xd->handleOwnEvents == FALSE)
	    addInputHandler(R_InputHandlers, ConnectionNumber(display),
			    R_ProcessEvents, XActivity);
    }
    /* whitepixel = GetX11Pixel(255, 255, 255); */
    whitepixel = GetX11Pixel(R_RED(canvascolor), R_GREEN(canvascolor),
			     R_BLUE(canvascolor));
    blackpixel = GetX11Pixel(0, 0, 0);

    if (!SetBaseFont(xd)) {
	Rprintf("can't find X11 font\n");
	return FALSE;
    }

    /* Foreground and Background Colors */

    xd->fill = 0xffffffff; /* transparent, was R_RGB(255, 255, 255); */
    xd->col = R_RGB(0, 0, 0);
    xd->canvas = canvascolor;
    if(type == JPEG && !R_OPAQUE(xd->canvas)) {
	warning("jpeg() does not support transparency: using white bg");
	xd->canvas = 0xffffff;
    }
    if(type > WINDOW) xd->fill = xd->canvas;


    /* Try to create a simple window. */
    /* We want to know about exposures */
    /* and window-resizes and locations. */

    attributes.background_pixel = whitepixel;
    attributes.border_pixel = blackpixel;
    attributes.backing_store = Always;
    attributes.event_mask = ButtonPressMask
	| ExposureMask
	| StructureNotifyMask;


    if (type == WINDOW) {
	int alreadyCreated = (xd->window != (Window)NULL);
	if(alreadyCreated == 0) {
	    xd->windowWidth = iw = w/pixelWidth();
	    xd->windowHeight = ih = h/pixelHeight();
	    if ((xd->window = XCreateWindow(
		display, rootwin,
		DisplayWidth(display, screen) - iw - 10, 10, iw, ih, 1,
		DefaultDepth(display, screen),
		InputOutput,
		DefaultVisual(display, screen),
		CWEventMask | CWBackPixel | CWBorderPixel | CWBackingStore,
		&attributes)) == 0) {
		warning("unable to create X11 window");
		return FALSE;
	    }

	    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
			    8, PropModeReplace,
			    (unsigned char*)"R Graphics", 13);

	    xd->gcursor = XCreateFontCursor(display, CURSOR);
	    XDefineCursor(display, xd->window, xd->gcursor);

	    /* set up protocols so that window manager sends */
	    /* me an event when user "destroys" window */
	    _XA_WM_PROTOCOLS = XInternAtom(display, "WM_PROTOCOLS", 0);
	    protocol = XInternAtom(display, "WM_DELETE_WINDOW", 0);
	    XSetWMProtocols(display, xd->window, &protocol, 1);

	}
	/* Save the NewDevDesc* with the window for event dispatching */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);

	/* Map the window */
	if(alreadyCreated == 0) {
	    XSelectInput(display, xd->window,
			 ExposureMask | ButtonPressMask | StructureNotifyMask);
	    XMapWindow(display, xd->window);
	    XSync(display, 0);

	    /* Gobble expose events */

	    while ( XPeekEvent(display, &event),
		    !XCheckTypedEvent(display, Expose, &event))
                ;
	    /* XNextEvent(display, &event);
	       if (event.xany.type == Expose) {
	       while (event.xexpose.count)
	       XNextEvent(display, &event);
	       }
	    */
	}
    } else { /* PIXMAP */
	xd->windowWidth = iw = w;
	xd->windowHeight = ih = h;
	if ((xd->window = XCreatePixmap(
	    display, rootwin,
	    iw, ih, DefaultDepth(display, screen))) == 0) {
	    warning("unable to create pixmap");
	    return FALSE;
	}
	/* Save the NewDevDesc* with the window for event dispatching */
	/* Is this needed? */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);
	xd->npages = 0;
    }

    /* Set the graphics context */

    gcv.arc_mode = ArcChord;
    xd->wgc = XCreateGC(display, xd->window, GCArcMode, &gcv);
    XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
    XSetFont(display, xd->wgc, xd->font->fid);

    /* ensure that line drawing is set up at the first */
    /* graphics call */
    xd->lty = -1;
    xd->lwd = -1;


    numX11Devices++;
    return TRUE;
}

static double newX11_StrWidth(char *str, int font,
			      double cex, double ps, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    int size = cex * ps + 0.5;
    SetFont(font, size, dd);
    return (double)XTextWidth(xd->font, str, strlen(str));
}


	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void newX11_MetricInfo(int c, int font, double cex, double ps,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd)
{
    int first, last;
    int size = cex * ps + 0.5;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    SetFont(font, size, dd);
    first = xd->font->min_char_or_byte2;
    last = xd->font->max_char_or_byte2;

    if (c == 0) {
	*ascent = xd->font->ascent;
	*descent = xd->font->descent;
	*width = xd->font->max_bounds.width;
    }
    else if (first <= c && c <= last) {
	*ascent = xd->font->per_char[c-first].ascent;
	*descent = xd->font->per_char[c-first].descent;
	*width = xd->font->per_char[c-first].width;
    }
    else {
	*ascent = 0;
	*descent = 0;
	*width = 0;
    }
}

static void newX11_Clip(double x0, double x1, double y0, double y1,
			NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (x0 < x1) {
	xd->clip.x = (int) x0 ;
	xd->clip.width = (int) x1 - (int) x0 + 1;
    }
    else {
	xd->clip.x = (int) x1;
	xd->clip.width = (int) x0 - (int) x1 + 1;
    }

    if (y0 < y1) {
	xd->clip.y = (int) y0;
	xd->clip.height = (int) y1 -  (int) y0 + 1;
    }
    else {
	xd->clip.y = (int) y1;
	xd->clip.height = (int) y0 - (int) y1 + 1;
    }

    XSetClipRectangles(display, xd->wgc, 0, 0, &(xd->clip), 1, Unsorted);
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

static void newX11_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    *left = 0.0;
    *right = xd->windowWidth;
    *bottom = xd->windowHeight;
    *top = 0.0;
}

static void newX11_NewPage(int fill, double gamma, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) {
	if (xd->npages++) {
	    /* try to preserve the page we do have */
	    if (xd->type != XIMAGE) X11_Close_bitmap(xd);
	    if (xd->type != XIMAGE && xd->fp != NULL) fclose(xd->fp);
	    if (xd->type == PNG) {
		char buf[512];
		sprintf(buf, xd->filename, xd->npages);
		xd->fp = R_fopen(R_ExpandFileName(buf), "w");
		if (!xd->fp)
		    error("could not open PNG file `%s'", buf);
	    }
	    if (xd->type == JPEG) {
		char buf[512];
		sprintf(buf, xd->filename, xd->npages);
		xd->fp = R_fopen(R_ExpandFileName(buf), "w");
		if (!xd->fp)
		    error("could not open JPEG file `%s'", buf);
	    }
	    /* error("attempt to draw second page on pixmap device");*/
	}
/* we want to override the default bg="transparent" */
/*	xd->fill = R_OPAQUE(dd->bg) ? dd->bg : xd->canvas; */
	xd->fill = R_OPAQUE(fill) ? fill: PNG_TRANS;
	SetColor(xd->fill, dd);
	XFillRectangle(display, xd->window, xd->wgc, 0, 0,
		       xd->windowWidth, xd->windowHeight);
	return;
    }

    FreeX11Colors();
    if ( (model == PSEUDOCOLOR2) || (xd->fill != fill)) {
	xd->fill = R_OPAQUE(fill) ? fill : xd->canvas;
	whitepixel = GetX11Pixel(R_RED(xd->fill),R_GREEN(xd->fill),R_BLUE(xd->fill));
	XSetWindowBackground(display, xd->window, whitepixel);
    }
    XClearWindow(display, xd->window);
#ifdef XSYNC
    XSync(display, 0);
#endif
}

extern int R_SaveAsPng(void  *d, int width, int height,
		       unsigned long (*gp)(XImage *, int, int),
		       int bgr, FILE *fp, unsigned int transparent);

extern int R_SaveAsJpeg(void  *d, int width, int height,
			unsigned long (*gp)(XImage *, int, int),
			int bgr, int quality, FILE *outfile);


static long knowncols[512];


static unsigned long bitgp(XImage *xi, int x, int y)
{
    int i, r, g, b;
    XColor xcol;
    /*	returns the colour of the (x,y) pixel stored as RGB */
    i = XGetPixel(xi, y, x);
    switch(model) {
    case MONOCHROME:
	return (i==0)?0xFFFFFF:0;
    case GRAYSCALE:
    case PSEUDOCOLOR1:
    case PSEUDOCOLOR2:
	if (i < 512) {
	    if (knowncols[i] < 0) {
		xcol.pixel = i;
		XQueryColor(display, colormap, &xcol);
		knowncols[i] = ((xcol.red>>8)<<16) | ((xcol.green>>8)<<8)
		    | (xcol.blue>>8);
	    }
	    return knowncols[i];
	}
	else {
	    xcol.pixel = i;
	    XQueryColor(display, colormap, &xcol);
	    return ((xcol.red>>8)<<16) | ((xcol.green>>8)<<8) | (xcol.blue>>8);
	}
    case TRUECOLOR:
	r = ((i>>RShift)&RMask) * 255 /(RMask);
	g = ((i>>GShift)&GMask) * 255 /(GMask);
	b = ((i>>BShift)&BMask) * 255 /(BMask);
	return (r<<16) | (g<<8) | b;
    default:
	return 0;
    }
    return 0; /* not reached, needed for some compilers */
}

static void X11_Close_bitmap(newX11Desc *xd)
{
    int i;
    XImage *xi;
    for (i = 0; i < 512; i++) knowncols[i] = -1;
    xi = XGetImage(display, xd->window, 0, 0,
		   xd->windowWidth, xd->windowHeight,
		   AllPlanes, ZPixmap);
    if (xd->type == PNG) {
	unsigned int pngtrans = PNG_TRANS;
	if(model == TRUECOLOR) {
	    int i, r, g, b;
	    /* some `truecolor' displays distort colours */
	    i = GetX11Pixel(R_RED(PNG_TRANS),
			    R_GREEN(PNG_TRANS),
			    R_BLUE(PNG_TRANS));
	    r = ((i>>RShift)&RMask) * 255 /(RMask);
	    g = ((i>>GShift)&GMask) * 255 /(GMask);
	    b = ((i>>BShift)&BMask) * 255 /(BMask);
	    pngtrans = (r<<16) | (g<<8) | b;
	}
	R_SaveAsPng(xi, xd->windowWidth, xd->windowHeight,
		    bitgp, 0, xd->fp,
		    (xd->fill != PNG_TRANS) ? 0 : pngtrans);
    } else if (xd->type == JPEG)
	R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight,
		     bitgp, 0, xd->quality, xd->fp);
    XDestroyImage(xi);
}

static void newX11_Close(NewDevDesc *dd)
{
#ifdef OLD
    int i, j;
#endif
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type == WINDOW) {
	/* process pending events */
	/* set block on destroy events */
	inclose = TRUE;
	R_ProcessEvents((void*) NULL);

	XFreeCursor(display, xd->gcursor);
	XDestroyWindow(display, xd->window);
	XSync(display, 0);
    } else {
	if (xd->npages && xd->type != XIMAGE) X11_Close_bitmap(xd);
	XFreeGC(display, xd->wgc);
	XFreePixmap(display, xd->window);
	if (xd->type != XIMAGE && xd->fp != NULL) fclose(xd->fp);
    }

    numX11Devices--;
    if (numX11Devices == 0)  {
      int fd = ConnectionNumber(display);
	/* Free Resources Here */
#ifdef OLD
	for(i=0 ; i<NFONT ; i++)
	    for(j=0 ; j<5 ; j++) {
		if (fontarray[i][j] != NULL) {
		    XUnloadFont(display, fontarray[i][j]->fid);
		    fontarray[i][j] = NULL;
		}
		missingfont[i][j] = 0;
	    }
#else
	while (nfonts--)  XFreeFont(display, fontcache[nfonts].font);
	nfonts = 0;
#endif
        if(xd->handleOwnEvents == FALSE)
	    removeInputHandler(&R_InputHandlers,
			       getInputHandler(R_InputHandlers,fd));
	XCloseDisplay(display);
	displayOpen = FALSE;
    }

    free(xd);
    inclose = FALSE;
}

static char title[11] = "R Graphics";

static void newX11_Activate(NewDevDesc *dd)
{
    char t[50];
    char num[3];
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    strcpy(t, title);
    strcat(t, ": Device ");
    sprintf(num, "%i", devNumber((DevDesc*)(dd))+1);
    strcat(t, num);
    strcat(t, " (ACTIVE)");
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, strlen(t));
    XSync(display, 0);
}

static void newX11_Deactivate(NewDevDesc *dd)
{
    char t[50];
    char num[3];
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    strcpy(t, title);
    strcat(t, ": Device ");
    sprintf(num, "%i", devNumber((DevDesc*)(dd))+1);
    strcat(t, num);
    strcat(t, " (inactive)");
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, strlen(t));
    XSync(display, 0);
}

static void newX11_Rect(double x0, double y0, double x1, double y1,
			int col, int fill, double gamma,
			int lty, double lwd,
			NewDevDesc *dd)
{
    int tmp;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (x0 > x1) {
	tmp = x0;
	x0 = x1;
	x1 = tmp;
    }
    if (y0 > y1) {
	tmp = y0;
	y0 = y1;
	y1 = tmp;
    }
    if (R_OPAQUE(fill)) {
	SetColor(fill, dd);
	XFillRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
    if (R_OPAQUE(col)) {
	SetColor(col, dd);
	SetLinetype(lty, lwd, dd);
	XDrawRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

static void newX11_Circle(double x, double y, double r,
			  int col, int fill, double gamma,
			  int lty, double lwd,
			  NewDevDesc *dd)
{
    int ir, ix, iy;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

#ifdef OLD
    ir = ceil(r);
#else
    ir = floor(r + 0.5);
#endif

    ix = (int)x;
    iy = (int)y;
    if (R_OPAQUE(fill)) {
	SetColor(fill, dd);
	XFillArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
    if (R_OPAQUE(col)) {
	SetLinetype(lty, lwd, dd);
	SetColor(col, dd);
	XDrawArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
}

static void newX11_Line(double x1, double y1, double x2, double y2,
			int col, double gamma, int lty, double lwd,
			NewDevDesc *dd)
{
    int xx1, yy1, xx2, yy2;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    /* In-place conversion ok */

    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    if (R_OPAQUE(col)) {
	SetColor(col, dd);
	SetLinetype(lty, lwd, dd);
	XDrawLine(display, xd->window, xd->wgc, xx1, yy1, xx2, yy2);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
}

static void newX11_Polyline(int n, double *x, double *y,
			    int col, double gamma, int lty, double lwd,
			    NewDevDesc *dd)
{
    char *vmax = vmaxget();
    XPoint *points;
    int i, j;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    points = (XPoint *) R_alloc(n, sizeof(XPoint));

    for(i=0 ; i<n ; i++) {
	points[i].x = (int)(x[i]);
	points[i].y = (int)(y[i]);
    }

    if (R_OPAQUE(col)) {
	SetColor(col, dd);
	SetLinetype(lty, lwd, dd);
/* Some X servers need npoints < 64K */
	for(i = 0; i < n; i+= 10000-1) {
	    j = n - i;
	    j = (j <= 10000) ? j: 10000; /* allow for overlap */
	    XDrawLines(display, xd->window, xd->wgc, points+i, j,
		       CoordModeOrigin);
	}
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }

    vmaxset(vmax);
}

static void newX11_Polygon(int n, double *x, double *y,
			   int col, int fill, double gamma,
			   int lty, double lwd,
			   NewDevDesc *dd)
{
    char *vmax = vmaxget();
    XPoint *points;
    int i;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    points = (XPoint *) R_alloc(n+1, sizeof(XPoint));

    for (i=0 ; i<n ; i++) {
	points[i].x = (int)(x[i]);
	points[i].y = (int)(y[i]);
    }
    points[n].x = (int)(x[0]);
    points[n].y = (int)(y[0]);
    if (R_OPAQUE(fill)) {
	SetColor(fill, dd);
	XFillPolygon(display, xd->window, xd->wgc, points, n, Complex, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
    if (R_OPAQUE(col)) {
	SetColor(col, dd);
	SetLinetype(lty, lwd, dd);
	XDrawLines(display, xd->window, xd->wgc, points, n+1, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }

    vmaxset(vmax);
}


static void newX11_Text(double x, double y,
			char *str, double rot, double hadj,
			int col, double gamma,
			int font, double cex, double ps,
			NewDevDesc *dd)
{
    int len, size;
/*    double xl, yl, rot1;*/
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    size = cex * ps + 0.5;
    SetFont(font, size, dd);
    if (R_OPAQUE(col)) {
	SetColor(col, dd);
	len = strlen(str);
	XRotDrawString(display, xd->font, rot, xd->window, xd->wgc,
		       (int)x, (int)y, str);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
}

static Rboolean newX11_Locator(double *x, double *y, NewDevDesc *dd)
{
    XEvent event;
    NewDevDesc *ddEvent;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    caddr_t temp;
    int done = 0;

    if (xd->type > WINDOW) return 0;
    R_ProcessEvents((void*)NULL);	/* discard pending events */
    XSync(display, 1);
    /* handle X events as normal until get a button */
    /* click in the desired device */
    while (!done && displayOpen) {
	XNextEvent(display, &event);
	if (event.type == ButtonPress) {
	    XFindContext(display, event.xbutton.window,
			 devPtrContext, &temp);
	    ddEvent = (NewDevDesc *) temp;
	    if (ddEvent == dd) {
		if (event.xbutton.button == Button1) {
		    *x = event.xbutton.x;
		    *y = event.xbutton.y;
  		       /* Make a beep! Was print "\07", but that
                          messes up some terminals. */
                    XBell(display, X_BELL_VOLUME);
		    XSync(display, 0);
		    done = 1;
		}
		else
		    done = 2;
	    }
	}
	else
	    handleEvent(event);
    }
    /* if it was a Button1 succeed, otherwise fail */
    return (done == 1);
}

/* Set Graphics mode - not needed for X11 */
static void newX11_Mode(int mode, NewDevDesc *dd)
{

#ifdef XSYNC
    if (mode == 0) XSync(display, 0);
#else
    XSync(display, 0);
#endif
}

/* Hold the Picture Onscreen - not needed for X11 */
static void newX11_Hold(NewDevDesc *dd)
{
}


	/*  X11 Device Driver Arguments	:	*/
	/*	1) display name			*/
	/*	2) width (inches)		*/
	/*	3) height (inches)		*/
	/*	4) base pointsize		*/
	/*	5) gamma correction factor	*/
	/*	6) colormodel,			*/
	/*	 see X_COLORTYPE at top of file */
	/*	7) maxcube			*/

Rboolean newX11DeviceDriver(DevDesc *dd,
			    char *disp_name,
			    double width,
			    double height,
			    double pointsize,
			    double gamma_fac,
			    X_COLORTYPE colormodel,
			    int maxcube, int canvascolor)
{
    newX11Desc *xd;

    xd = Rf_allocNewX11DeviceDesc(pointsize);
    /* Used to set dd->dp.font=1 and dd->dp.ps=pointsize,
     * but Paul removed that.
     * This sort of initialisation occurs in R base graphics now.
     */

    /*	Start the Device Driver and Hardcopy.  */

    if (!newX11_Open((NewDevDesc*)(dd), xd, disp_name, width, height,
		     gamma_fac, colormodel, maxcube, canvascolor)) {
	free(xd);
	return FALSE;
    }

    Rf_setNewX11DeviceData((NewDevDesc*)(dd), gamma_fac, xd);

#if BUG
    R_ProcessEvents((void*) NULL);
#endif

    return TRUE;
}

/**
  This fills the general device structure (dd) with the X-specific
  methods/functions. It also specifies the current values of the
  dimensions of the device, and establishes the fonts, line styles, etc.
 */
int
Rf_setNewX11DeviceData(NewDevDesc *dd, double gamma_fac, newX11Desc *xd)
{
    dd->newDevStruct = 1;

    /*	Set up Data Structures. */

    dd->open = newX11_Open;
    dd->close = newX11_Close;
    dd->activate = newX11_Activate;
    dd->deactivate = newX11_Deactivate;
    dd->size = newX11_Size;
    dd->newPage = newX11_NewPage;
    dd->clip = newX11_Clip;
    dd->strWidth = newX11_StrWidth;
    dd->text = newX11_Text;
    dd->rect = newX11_Rect;
    dd->circle = newX11_Circle;
    dd->line = newX11_Line;
    dd->polyline = newX11_Polyline;
    dd->polygon = newX11_Polygon;
    dd->locator = newX11_Locator;
    dd->mode = newX11_Mode;
    dd->hold = newX11_Hold;
    dd->metricInfo = newX11_MetricInfo;

    /* Set required graphics parameters. */

    /* Window Dimensions in Pixels */
    /* Initialise the clipping rect too */

    dd->left = dd->clipLeft = 0;			/* left */
    dd->right = dd->clipRight = xd->windowWidth;	/* right */
    dd->bottom = dd->clipBottom = xd->windowHeight;	/* bottom */
    dd->top = dd->clipTop = 0;			/* top */

    /* Nominal Character Sizes in Pixels */

    SetBaseFont(xd);
    dd->cra[0] = xd->font->max_bounds.rbearing -
	xd->font->min_bounds.lbearing;
    dd->cra[1] = xd->font->max_bounds.ascent +
	xd->font->max_bounds.descent;

    /* Character Addressing Offsets */
    /* These are used to plot a single plotting character */
    /* so that it is exactly over the plotting point */

    dd->xCharOffset = 0.4900;
    dd->yCharOffset = 0.3333;
    dd->yLineBias = 0.1;

    /* Inches per raster unit */

    dd->ipr[0] = pixelWidth();
    dd->ipr[1] = pixelHeight();

    /* Device capabilities */

    dd->canResizePlot = TRUE;
    dd->canChangeFont = FALSE;
    dd->canRotateText = TRUE;
    dd->canResizeText = TRUE;
    dd->canClip = TRUE;
    dd->canHAdj = 0;
    dd->canChangeGamma = FALSE;

    dd->startps = xd->basefontsize;
    dd->startcol = xd->col;
    dd->startfill = xd->fill;
    dd->startlty = LTY_SOLID;
    dd->startfont = 1;
    dd->startgamma = gamma_fac;

    /* initialise x11 device description */
    /* (most of the work has been done in X11_Open) */

    xd->cex = 1.0;
    xd->lty = 0;
    xd->resize = 0;

    dd->deviceSpecific = (void *) xd;

    dd->displayListOn = TRUE;

  return(TRUE);
}


/**
 This allocates an newX11Desc instance  and sets its default values.
 */
newX11Desc * Rf_allocNewX11DeviceDesc(double ps)
{
    newX11Desc *xd;
    /* allocate new device description */
    if (!(xd = (newX11Desc*)calloc(1, sizeof(newX11Desc))))
	return FALSE;

    /* From here on, if we need to bail out with "error", */
    /* then we must also free(xd). */

    /*	Font will load at first use.  */

    if (ps < 6 || ps > 24) ps = 12;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->basefontface = 1;
    xd->basefontsize = ps;
    xd->handleOwnEvents = FALSE;
    xd->window = (Window) NULL;

    return(xd);
}


Rboolean R_GetX11Image(int d, XImage **pximage, int *pwidth, int *pheight)
{
    SEXP dev = elt(findVar(install(".Devices"), R_NilValue), d);

    if (TYPEOF(dev) != STRSXP ||
	!(strcmp(CHAR(STRING_ELT(dev, 0)), "XImage") == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "PNG", 3) == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "X11", 3) == 0))
	return FALSE;
    else {
	NewDevDesc *dd = ((GEDevDesc *)GetDevice(d))->dev;
	newX11Desc *xd = dd->deviceSpecific;

	*pximage = XGetImage(display, xd->window, 0, 0,
			     xd->windowWidth, xd->windowHeight,
			     AllPlanes, ZPixmap);
	*pwidth = xd->windowWidth;
	*pheight = xd->windowHeight;
	return TRUE;
    }
}

/**
   Allows callers to retrieve the current Display setting for the process.
 */
Display*
Rf_getX11Display()
{
  return(display);
}


/**
 Allows the caller to register the X11 Display object for the process.
 Typically this will be done when the first X device is created, but this allows
 other code to generate the Display object and then register it with the R graphics
 engine.
 In addition to providing the Display, the caller should also give the default value for the
 gamma factor and also the colormodel and color cube size. See the documentation for the x11()
 function.
 Finally, setHandlers controls whether the code establishes handlers for the X errors.
 */
int
Rf_setX11Display(Display *dpy, double gamma_fac, X_COLORTYPE colormodel,
		 int maxcube, Rboolean setHandlers)
{
/*    static int alreadyDone = 0;
    if(alreadyDone) return(TRUE);
    alreadyDone = 1; */
    display = dpy;

#define SETGAMMA
#ifdef SETGAMMA
    RedGamma   = gamma_fac;
    GreenGamma = gamma_fac;
    BlueGamma  = gamma_fac;
#endif
    screen = DefaultScreen(display);
    rootwin = DefaultRootWindow(display);
    depth = DefaultDepth(display, screen);
    visual = DefaultVisual(display, screen);
    colormap = DefaultColormap(display, screen);
    Vclass = visual->class;
    model = colormodel;
    maxcubesize = maxcube;
    SetupX11Color();
    devPtrContext = XUniqueContext();
    displayOpen = TRUE;
    /* set error handlers */
    if(setHandlers == TRUE) {
	XSetErrorHandler(R_X11Err);
	XSetIOErrorHandler(R_X11IOErr);
    }

    return(TRUE);
}

/**
 This allocates an x11Desc instance  and sets its default values.
 */
newX11Desc * Rf_allocX11DeviceDesc(double ps)
{
  newX11Desc *xd;
    /* allocate new device description */
    if (!(xd = (newX11Desc*)malloc(sizeof(newX11Desc))))
	return FALSE;

    /* From here on, if we need to bail out with "error", */
    /* then we must also free(xd). */

    /*	Font will load at first use.  */

    if (ps < 6 || ps > 24) ps = 12;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->basefontface = 1;
    xd->basefontsize = ps;
    xd->handleOwnEvents = FALSE;
    xd->window = (Window) NULL;

  return(xd);
}

extern SEXP RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

void R_init_X11(DllInfo *info)
{
      /* Ideally, we would not cast X11DeviceDriver here.
         However, the declaration in R_ext/RX11.h doesn't have access
         to the definition of X_COLORTYPE, at present. Thus we need
         to explicitly cast to avoid compiler warnings.
       */
    R_setX11Routines((R_X11DeviceDriverRoutine) newX11DeviceDriver,
                      RX11_dataentry,
                      R_GetX11Image);
}
