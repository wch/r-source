/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  Robert Gentleman, Ross Ihaka and the
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
#include <config.h>
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
#include <X11/Intrinsic.h>/*->	Xlib.h	Xutil.h Xresource.h .. */

#include "Defn.h"
#include "Graphics.h"
#include "Fileio.h" /* R_fopen */
#include "rotated.h"/* 'Public' routines from here */
/* For the input handlers of the event loop mechanism: */
#include "R_ext/eventloop.h" 
#include "R_ext/Memory.h" /* vmaxget */
#include "Devices.h"

#include "devX11.h"

	/********************************************************/
	/* This device driver has been documented so that it be	*/
	/* used as a template for new drivers			*/
	/********************************************************/

#define CURSOR		XC_crosshair		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

#define IS_100DPI ((int) (1./pixelHeight() + 0.5) == 100)

	/********************************************************/
	/* Each driver can have its own device-specic graphical */
	/* parameters and resources.  these should be wrapped	*/
	/* in a structure (like the x11Desc structure below)	*/
	/* and attached to the overall device description via	*/
	/* the dd->deviceSpecific pointer			*/
	/* NOTE that there are generic graphical parameters	*/
	/* which must be set by the device driver, but are	*/
	/* common to all device types (see Graphics.h)		*/
	/* so go in the GPar structure rather than this device- */
	/* specific structure					*/
	/********************************************************/

typedef struct {
    /* R Graphics Parameters */
    /* Local device copy so that we can detect */
    /* when parameter changes. */

    double cex;				/* Character expansion */
    double srt;				/* String rotation */
    int lty;				/* Line type */
    double lwd;
    int col;				/* Color */
    int fg;				/* Foreground */
    int bg;				/* Background */
    int fontface;			/* Typeface */
    int fontsize;			/* Size in points */
    int basefontface;			/* Typeface */
    int basefontsize;			/* Size in points */

    /* X11 Driver Specific */
    /* Parameters with copy per X11 device. */

    int windowWidth;			/* Window width (pixels) */
    int windowHeight;			/* Window height (pixels) */
    int resize;				/* Window resized */
    Window window;			/* Graphics Window */
    GC wgc;				/* GC for window */
    Cursor gcursor;			/* Graphics Cursor */
    XSetWindowAttributes attributes;	/* Window attributes */
#if 0
    XColor fgcolor;			/* Foreground color */
    XColor bgcolor;			/* Background color */
#endif
    XRectangle clip;			/* The clipping rectangle */

    int usefixed;
    XFontStruct *fixedfont;
    XFontStruct *font;
    X_GTYPE type;			/* Window or pixmap? */
    int npages;				/* counter for a pixmap */
    FILE *fp;				/* file for a bitmap device */
    int quality;			/* JPEG quality */

} x11Desc;


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

static void   X11_Activate(DevDesc *);
static void   X11_Circle(double, double, int, double, int, int, DevDesc*);
static void   X11_Clip(double, double, double, double, DevDesc*);
static void   X11_Close(DevDesc*);
static void   X11_Deactivate(DevDesc *);
static void   X11_Hold(DevDesc*);
static void   X11_Line(double, double, double, double, int, DevDesc*);
static Rboolean X11_Locator(double*, double*, DevDesc*);
static void   X11_Mode(int, DevDesc*);
static void   X11_NewPage(DevDesc*);
static Rboolean 
X11_Open(DevDesc*, x11Desc*, char*, double, double, double, X_COLORTYPE, int);
static void   X11_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   X11_Polyline(int, double*, double*, int, DevDesc*);
static void   X11_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   X11_Resize(DevDesc*);
static double X11_StrWidth(char*, DevDesc*);
static void   X11_Text(double, double, int, char*, double, double, DevDesc*);
static void   X11_MetricInfo(int, double*, double*, double*, DevDesc*);

	/*************************************************/
	/* End of list of required device driver actions */
	/*************************************************/

	/* Support Routines */

static XFontStruct *RLoadFont(int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static int SetBaseFont(x11Desc*);
static void SetColor(int, DevDesc*);
static void SetFont(int, int, DevDesc*);
static void SetLinetype(int, double, DevDesc*);


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
static int RedLevels, GreenLevels, BlueLevels;


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
    int i, imin;
    unsigned int pixel = 0;  /* -Wall */
    int gray = (0.299 * r + 0.587 * g + 0.114 * b) + 0.0001;
    for (i = 0; i < PaletteSize; i++) {
	dr = (RPalette[i].red - gray);
	d = dr * dr;
	if (d < dmin) {
	    pixel = XPalette[i].pixel;
	    dmin = d;
	    imin = i;
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
    else {
	RedLevels = nr;
	GreenLevels = ng;
	BlueLevels = nb;
	return 1;
    }
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
    int i, imin;
    pixel = 0;			/* -Wall */
    for (i = 0; i < PaletteSize; i++) {
	dr = (RPalette[i].red - r);
	dg = (RPalette[i].green - g);
	db = (RPalette[i].blue - b);
	d = dr * dr + dg * dg + db * db;
	if (d < dmin) {
	    pixel = XPalette[i].pixel;
	    dmin = d;
	    imin = i;
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
	    SetupPseudoColor(model);
	}
    }
    else if (Vclass == TrueColor) {
	if (model == MONOCHROME)
	    SetupMonochrome();
	else if (model == GRAYSCALE)
	    SetupGrayScale();
	else if (model == PSEUDOCOLOR1 || model == PSEUDOCOLOR2)
	    SetupPseudoColor(model);
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
    DevDesc *dd;
    x11Desc *xd;
    if (event.xany.type == Expose) {
	while(XCheckTypedEvent(display, Expose, &event))
	    ;
	XFindContext(display, event.xexpose.window,
		     devPtrContext, &temp);
	dd = (DevDesc *) temp;
	xd = (x11Desc *) dd->deviceSpecific;
	if (xd->resize)
	    dd->dp.resize(dd);
	playDisplayList(dd);
    }
    else if (event.type == ConfigureNotify) {
	XFindContext(display, event.xconfigure.window,
		     devPtrContext, &temp);
	dd = (DevDesc *) temp;
	xd = (x11Desc *) dd->deviceSpecific;
	xd->windowWidth = event.xconfigure.width;
	xd->windowHeight = event.xconfigure.height;
	xd->resize = 1;
    }
    else if ((event.type == ClientMessage) &&
	     (event.xclient.message_type == _XA_WM_PROTOCOLS))
	if (!inclose && event.xclient.data.l[0] == protocol) {
	    XFindContext(display, event.xclient.window,
			 devPtrContext, &temp);
	    dd = (DevDesc *) temp;
	    KillDevice(dd);
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

static int SetBaseFont(x11Desc *xd)
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

static void SetFont(int face, int size, DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

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

    /* search fontcache */
    for ( i = nfonts ; i-- ; ) {
	f = &fontcache[i];
	if ( f->face == face && f->size == size ) return f->font;
    }



    /* Here's a 1st class fudge: make sure that the Adobe design sizes
       8, 10, 11, 12, 14, 17, 18, 20, 24, 25, 34 can be obtained via
       an integer "size" at 100 dpi, namely 6, 7, 8, 9, 10, 12, 13,
       14, 17, 18, 24 points. It's almost y = x * 100/72, but not
       quite. The constants were found using lm(). --pd */

    pixelsize = IS_100DPI ? R_rint(size * 1.43 - 0.4) : size;

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

static int SetBaseFont(x11Desc *xd)
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

static void SetFont(int face, int size, DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;
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


static void SetColor(int color, DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;
    if (color != xd->col) {
	blackpixel = GetX11Pixel(R_RED(color), R_GREEN(color), R_BLUE(color));
	xd->col = color;
	XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
    }
}

/* --> See "Notes on Line Textures" in ../include/Graphics.h
 *
 *	27/5/98 Paul - change to allow lty and lwd to interact:
 *	the line texture is now scaled by the line width so that,
 *	for example, a wide (lwd=2) dotted line (lty=2) has bigger
 *	dots which are more widely spaced.  Previously, such a line
 *	would have "dots" which were wide, but not long, nor widely
 *	spaced.
 */
static void SetLinetype(int newlty, double nlwd, DevDesc *dd)
{
    static char dashlist[8];
    int i, ndash, newlwd;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    newlwd = nlwd;
    if (newlwd < 1)/* not less than 1 pixel */
	newlwd = 1;
    if (newlty != xd->lty || newlwd != xd->lwd) {
	xd->lty = newlty;
	xd->lwd = newlwd;
	if (newlty == 0) {/* special hack for  lty = 0 -- only for X11 */
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineSolid,
			       CapRound,
			       JoinRound);
	}
	else {
	    ndash = 0;
	    for(i = 0 ; i < 8 && (newlty != 0); i++) {
		int j = newlty & 15;
		if (j == 0) j = 1; /* Or we die with an X Error */
		/* scale line texture for line width */
		j = j*newlwd;
		/* make sure that scaled line texture */
		/* does not exceed X11 storage limits */
		if (j > 255) j=255;
		dashlist[ndash++] = j;
		newlty = newlty >> 4;
	    }
	    XSetDashes(display, xd->wgc, 0, dashlist, ndash);
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineOnOffDash,
			       CapButt,
			       JoinRound);
	}
    }
}


	/********************************************************/
	/* device_Open is not usually called directly by the	*/
	/* graphics engine;  it is usually only called from	*/
	/* the device-driver entry point.			*/
	/* this function should set up all of the device-	*/
	/* specific resources for a new device			*/
	/* this function is given a new	structure for device-	*/
	/* specific information AND it must FREE the structure	*/
	/* if anything goes seriously wrong			*/
	/* NOTE that it is perfectly acceptable for this	*/
	/* function to set generic graphics parameters too	*/
	/* (i.e., override the generic parameter settings	*/
	/* which GInit sets up) all at the author's own risk	*/
	/* of course :)						*/
	/********************************************************/

static int R_X11Err(Display *dsp, XErrorEvent *event)
{
    char buff[1000];
    XGetErrorText(dsp, event->error_code, buff, 1000);
    warning("X11 protocol error: %s", buff);
    return 0;
}

static int R_X11IOErr(Display *dsp)
{
    error("X11 fatal IO error: please save work and shut down R");
    return 0; /* but should never get here */
}


static Rboolean 
X11_Open(DevDesc *dd, x11Desc *xd, char *dsp, double w, double h, 
	 double gamma_fac, X_COLORTYPE colormodel, int maxcube)
{
    /* if we have to bail out with "error", then must free(dd) and free(xd) */

    XEvent event;
    int iw, ih;
    X_GTYPE type;
    char *p = dsp;
    XGCValues gcv;
    /* Indicates whether the display is created within this particular call: */
    Rboolean DisplayOpened = FALSE;

    if (!strncmp(dsp, "png::", 5)) {
	FILE *fp;
#ifndef HAVE_PNG
	error("No png support in this version of R");
#endif
	if (!(fp = R_fopen(R_ExpandFileName(dsp+5), "w")))
	    error("could not open PNG file `%s'", dsp+6);
	xd->fp = fp;
	type = PNG;
	p = "";
    } 
    else if (!strncmp(dsp, "jpeg::", 6)) {
	FILE *fp;
#ifndef HAVE_JPEG
	error("No jpeg support in this version of R");
#endif
	p = strchr(dsp+6, ':'); *p='\0';
	xd->quality = atoi(dsp+6);
	if (!(fp = R_fopen(R_ExpandFileName(p+1), "w")))
	    error("could not open JPEG file `%s'", p+1);
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
	if ((display = XOpenDisplay(p)) == NULL)
	    return FALSE;
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
	displayOpen = DisplayOpened = TRUE;
	/* set error handlers */
	XSetErrorHandler(R_X11Err);
	XSetIOErrorHandler(R_X11IOErr);
    }
    whitepixel = GetX11Pixel(255, 255, 255);
    blackpixel = GetX11Pixel(0, 0, 0);

    if (!SetBaseFont(xd)) {
	Rprintf("can't find X11 font\n");
	return FALSE;
    }

    /* Foreground and Background Colors */

    xd->bg =  dd->dp.bg	 = R_RGB(255, 255, 255);
    xd->fg =  dd->dp.fg	 = R_RGB(0, 0, 0);
    xd->col = dd->dp.col = xd->fg;

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
	xd->windowWidth = iw = w/pixelWidth();
	xd->windowHeight = ih = h/pixelHeight();
	if ((xd->window = XCreateWindow(
	    display, rootwin,
	    DisplayWidth(display, screen) - iw - 10, 10, iw, ih, 1,
	    DefaultDepth(display, screen),
	    InputOutput,
	    DefaultVisual(display, screen),
	    CWEventMask | CWBackPixel | CWBorderPixel | CWBackingStore,
	    &attributes)) == 0)
	    return FALSE;

	XChangeProperty( display, xd->window, XA_WM_NAME, XA_STRING,
			 8, PropModeReplace, (unsigned char*)"R Graphics", 13);

	xd->gcursor = XCreateFontCursor(display, CURSOR);
	XDefineCursor(display, xd->window, xd->gcursor);

	/* set up protocols so that window manager sends */
	/* me an event when user "destroys" window */
	_XA_WM_PROTOCOLS = XInternAtom(display, "WM_PROTOCOLS", 0);
	protocol = XInternAtom(display, "WM_DELETE_WINDOW", 0);
	XSetWMProtocols(display, xd->window, &protocol, 1);

	/* Save the devDesc* with the window for event dispatching */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);

	/* Map the window */

	XSelectInput(display, xd->window,
		     ExposureMask | ButtonPressMask | StructureNotifyMask);
	XMapWindow(display, xd->window);
	XSync(display, 0);

	/* Gobble expose events */

	XNextEvent(display, &event);
	if (event.xany.type == Expose) {
	    while (event.xexpose.count)
		XNextEvent(display, &event);
	}
    } else { /* PIXMAP */
	xd->windowWidth = iw = w;
	xd->windowHeight = ih = h;
	if ((xd->window = XCreatePixmap(
	    display, rootwin,
	    iw, ih, DefaultDepth(display, screen))) == 0)
	    return FALSE;
	/* Save the devDesc* with the window for event dispatching */
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

    if (DisplayOpened) {
	addInputHandler(R_InputHandlers, ConnectionNumber(display),
			R_ProcessEvents, XActivity);
    }

    numX11Devices++;
    return TRUE;
}

	/********************************************************/
	/* device_StrWidth should return the width of the given */
	/* string in DEVICE units (GStrWidth is responsible for */
	/* converting from DEVICE to whatever units the user	*/
	/* asked for						*/
	/********************************************************/

static double X11_StrWidth(char *str, DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    int size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd->gp.font, size, dd);
    return (double)XTextWidth(xd->font, str, strlen(str));
}


	/********************************************************/
	/* device_MetricInfo should return height, depth, and	*/
	/* width information for the given character in DEVICE	*/
	/* units (GMetricInfo does the necessary conversions)	*/
	/* This is used for formatting mathematical expressions	*/
	/* and for exact centering of text (see GText)		*/
	/* If the device cannot provide metric information then */
	/* it MUST return 0.0 for ascent, descent, and width	*/
	/********************************************************/

	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void X11_MetricInfo(int c, double* ascent, double* descent,
			   double* width, DevDesc *dd)
{
    int first, last;
    int size = dd->gp.cex * dd->gp.ps + 0.5;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    SetFont(dd->gp.font, size, dd);
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

	/********************************************************/
	/* device_Clip is given the left, right, bottom, and	*/
	/* top of a rectangle (in DEVICE coordinates).	it	*/
	/* should have the side-effect that subsequent output	*/
	/* is clipped to the given rectangle			*/
	/********************************************************/

static void X11_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

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

	/********************************************************/
	/* device_Resize is called whenever the device is	*/
	/* resized.  the function must update the GPar		*/
	/* parameters (left, right, bottom, and top) for the	*/
	/* new device size					*/
	/* this is not usually called directly by the graphics	*/
	/* engine because the detection of device resizes	*/
	/* (e.g., a window resize) are usually detected by	*/
	/* device-specific code	(see R_ProcessEvents in this file)*/
	/********************************************************/

static void X11_Resize(DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    if (xd->resize) {
	dd->dp.left = dd->gp.left = 0.0;
	dd->dp.right = dd->gp.right =  xd->windowWidth;
	dd->dp.bottom = dd->gp.bottom = xd->windowHeight;
	dd->dp.top = dd->gp.top = 0.0;
	xd->resize = 0;
    }
}

	/********************************************************/
	/* device_NewPage is called whenever a new plot requires*/
	/* a new page.	a new page might mean just clearing the	*/
	/* device (as in this case) or moving to a new page	*/
	/* (e.g., postscript)					*/
	/********************************************************/

static void X11_NewPage(DevDesc *dd)
{
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) {
	if (xd->npages++)
	    error("attempt to draw second page on pixmap device");
	xd->bg = dd->dp.bg;
	SetColor(xd->bg, dd);
	XFillRectangle(display, xd->window, xd->wgc, 0, 0,
		       xd->windowWidth, xd->windowHeight);
	return;
    }

    FreeX11Colors();
    if ( (model == PSEUDOCOLOR2) || (xd->bg != dd->dp.bg)) {
	xd->bg = dd->dp.bg;
	whitepixel = GetX11Pixel(R_RED(xd->bg),R_GREEN(xd->bg),R_BLUE(xd->bg));
	XSetWindowBackground(display, xd->window, whitepixel);
    }
    XClearWindow(display, xd->window);
#ifdef XSYNC
    XSync(display, 0);
#endif
}

extern int R_SaveAsPng(void  *d, int width, int height, 
		       unsigned long (*gp)(XImage *, int, int),
		       int bgr, FILE *fp);

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

	/********************************************************/
	/* device_Close is called when the device is killed	*/
	/* this function is responsible for destroying any	*/
	/* device-specific resources that were created in	*/
	/* device_Open and for FREEing the device-specific	*/
	/* parameters structure					*/
	/********************************************************/

static void X11_Close(DevDesc *dd)
{
#ifdef OLD
    int i, j;
#endif
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    if (xd->type == WINDOW) {
	/* process pending events */
	/* set block on destroy events */
	inclose = TRUE;
	R_ProcessEvents((void*) NULL);

	XFreeCursor(display, xd->gcursor);
	XDestroyWindow(display, xd->window);
	XSync(display, 0);
    } else {
	if (xd->npages && xd->type != XIMAGE) {
	    int i;
	    XImage *xi;
	    for (i = 0; i < 512; i++) knowncols[i] = -1;
	    xi = XGetImage(display, xd->window, 0, 0, 
			   xd->windowWidth, xd->windowHeight, 
			   AllPlanes, ZPixmap);
	    if (xd->type == PNG) 
		R_SaveAsPng(xi, xd->windowWidth, xd->windowHeight, 
			    bitgp, 0, xd->fp);
	    else if (xd->type == JPEG)
		R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight, 
			     bitgp, 0, xd->quality, xd->fp);
	    XDestroyImage(xi);
	}
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
	removeInputHandler(&R_InputHandlers,
			   getInputHandler(R_InputHandlers,fd));
	XCloseDisplay(display);
	displayOpen = FALSE;
    }

    free(xd);
    inclose = FALSE;
}

	/********************************************************/
	/* device_Activate is called when a device becomes the	*/
	/* active device.  in this case it is used to change the*/
	/* title of a window to indicate the active status of	*/
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/
static char title[11] = "R Graphics";

static void X11_Activate(DevDesc *dd)
{
    char t[50];
    char num[3];
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    strcpy(t, title);
    strcat(t, ": Device ");
    sprintf(num, "%i", deviceNumber(dd)+1);
    strcat(t, num);
    strcat(t, " (ACTIVE)");
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, 50);
    XSync(display, 0);
}

	/********************************************************/
	/* device_Deactivate is called when a device becomes	*/
	/* inactive.  in this case it is used to change the	*/
	/* title of a window to indicate the inactive status of */
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/

static void X11_Deactivate(DevDesc *dd)
{
    char t[50];
    char num[3];
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    strcpy(t, title);
    strcat(t, ": Device ");
    sprintf(num, "%i", deviceNumber(dd)+1);
    strcat(t, num);
    strcat(t, " (inactive)");
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, 50);
    XSync(display, 0);
}

	/********************************************************/
	/* device_Rect should have the side-effect that a	*/
	/* rectangle is drawn with the given locations for its	*/
	/* opposite corners.  the border of the rectangle	*/
	/* should be in the given "fg" colour and the rectangle	*/
	/* should be filled with the given "bg" colour		*/
	/* if "fg" is NA_INTEGER then no border should be drawn */
	/* if "bg" is NA_INTEGER then the rectangle should not	*/
	/* be filled						*/
	/* the locations are in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* locations to DEVICE coordinates using GConvert	*/
	/********************************************************/

static void X11_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
    int tmp;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    /* These in-place conversions are ok */

    GConvert(&x0, &y0, coords, DEVICE, dd);
    GConvert(&x1, &y1, coords, DEVICE, dd);

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
    if (bg != NA_INTEGER) {
	SetColor(bg, dd);
	XFillRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
	SetColor(bg, dd);
	XFillRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
    if (fg != NA_INTEGER) {
	SetColor(fg, dd);
	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	XDrawRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

	/********************************************************/
	/* device_Circle should have the side-effect that a	*/
	/* circle is drawn, centred at the given location, with */
	/* the given radius.  the border of the circle should be*/
	/* drawn in the given "col", and the circle should be	*/
	/* filled with the given "border" colour.		*/
	/* if "col" is NA_INTEGER then no border should be drawn*/
	/* if "border" is NA_INTEGER then the circle should not */
	/* be filled						*/
	/* the location is in arbitrary coordinates and the	*/
	/* function is responsible for converting this to	*/
	/* DEVICE coordinates.	the radius is given in DEVICE	*/
	/* coordinates						*/
	/********************************************************/

static void X11_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
    int ir, ix, iy;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

#ifdef OLD
    ir = ceil(r);
#else
    ir = floor(r + 0.5);
#endif

    /* In-place conversion ok */

    GConvert(&x, &y, coords, DEVICE, dd);
    ix = (int)x;
    iy = (int)y;
    if (col != NA_INTEGER) {
	SetColor(col, dd);
	XFillArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
    if (border != NA_INTEGER) {
	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	SetColor(border, dd);
	XDrawArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
}

	/********************************************************/
	/* device_Line should have the side-effect that a single*/
	/* line is drawn (from x1,y1 to x2,y2)			*/
	/* x1, y1, x2, and y2 are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void X11_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
    int xx1, yy1, xx2, yy2;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    /* In-place conversion ok */

    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);
    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    SetColor(dd->gp.col, dd);
    SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
    XDrawLine(display, xd->window, xd->wgc, xx1, yy1, xx2, yy2);
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

	/********************************************************/
	/* device_Polyline should have the side-effect that a	*/
	/* series of line segments are drawn using the given x	*/
	/* and y values						*/
	/* the x and y values are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void X11_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
    XPoint *points;
    double devx, devy;
    int i, j;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;
    points = (XPoint *) C_alloc(n, sizeof(XPoint));

    for(i=0 ; i<n ; i++) {
	devx = x[i];  devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points[i].x = (int)(devx);
	points[i].y = (int)(devy);
    }

    SetColor(dd->gp.col, dd);
    SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
/* Some X servers need npoints < 64K */
    for(i = 0; i < n; i+= 10000-1) {
	j = n - i;
	j = (j <= 10000) ? j: 10000; /* allow for overlap */
	XDrawLines(display, xd->window, xd->wgc, points+i, j, CoordModeOrigin);
    }
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif

    C_free((char *) points);
}

	/********************************************************/
	/* device_Polygon should have the side-effect that a	*/
	/* polygon is drawn using the given x and y values	*/
	/* the polygon border should be drawn in the "fg"	*/
	/* colour and filled with the "bg" colour		*/
	/* if "fg" is NA_INTEGER don't draw the border		*/
	/* if "bg" is NA_INTEGER don't fill the polygon		*/
	/* the x and y values are in arbitrary coordinates and	*/
	/* the function is responsible for converting them to	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void X11_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
    XPoint *points;
    double devx, devy;
    int i;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    points = (XPoint *) C_alloc(n+1, sizeof(XPoint));

    for (i=0 ; i<n ; i++) {
	devx = x[i];  devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points[i].x = (int)(devx);
	points[i].y = (int)(devy);
    }
    devx = x[0]; devy = y[0];
    GConvert(&devx, &devy, coords, DEVICE, dd);
    points[n].x = (int)(devx);
    points[n].y = (int)(devy);
    if (bg != NA_INTEGER) {
	SetColor(bg, dd);
	XFillPolygon(display, xd->window, xd->wgc, points, n, Complex, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
    if (fg != NA_INTEGER) {
	SetColor(fg, dd);
	SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
	XDrawLines(display, xd->window, xd->wgc, points, n+1, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }

    C_free((char *) points);
}


	/********************************************************/
	/* device_Text should have the side-effect that the	*/
	/* given text is drawn at the given location		*/
	/* the text should be rotated according to rot (degrees)*/
	/* the location is in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* location to DEVICE coordinates using GConvert	*/
	/********************************************************/

static void X11_Text(double x, double y, int coords,
		     char *str, double rot, double hadj, DevDesc *dd)
{
    int len, size;
/*    double xl, yl, rot1;*/
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;

    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd->gp.font, size, dd);
    SetColor(dd->gp.col, dd);
    len = strlen(str);
    GConvert(&x, &y, coords, DEVICE, dd);
    XRotDrawString(display, xd->font, rot, xd->window, xd->wgc,
		   (int)x, (int)y, str);
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

	/********************************************************/
	/* device_Locator should return the location of the next*/
	/* mouse click (in DEVICE coordinates;	GLocator is	*/
	/* responsible for any conversions)			*/
	/* not all devices will do anythin (e.g., postscript)	*/
	/********************************************************/

static Rboolean X11_Locator(double *x, double *y, DevDesc *dd)
{
    XEvent event;
    DevDesc *ddEvent;
    x11Desc *xd = (x11Desc *) dd->deviceSpecific;
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
	    ddEvent = (DevDesc *) temp;
	    if (ddEvent == dd) {
		if (event.xbutton.button == Button1) {
		    *x = event.xbutton.x;
		    *y = event.xbutton.y;
		    fprintf(stderr, "\07");
		    fflush(stderr);
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

	/********************************************************/
	/* device_Mode is called whenever the graphics engine	*/
	/* starts drawing (mode=1) or stops drawing (mode=1)	*/
	/* the device is not required to do anything		*/
	/********************************************************/

/* Set Graphics mode - not needed for X11 */
static void X11_Mode(int mode, DevDesc *dd)
{
#ifdef XSYNC
    if (mode == 0) XSync(display, 0);
#else
    XSync(display, 0);
#endif
}

	/********************************************************/
	/* i don't know what this is for and i can't find it	*/
	/* being used anywhere, but i'm loath to kill it in	*/
	/* case i'm missing something important			*/
	/********************************************************/

/* Hold the Picture Onscreen - not needed for X11 */
static void X11_Hold(DevDesc *dd)
{
}

	/********************************************************/
	/* the device-driver entry point is given a device	*/
	/* description structure that it must set up.  this	*/
	/* involves several important jobs ...			*/
	/* (1) it must ALLOCATE a new device-specific parameters*/
	/* structure and FREE that structure if anything goes	*/
	/* wrong (i.e., it won't report a successful setup to	*/
	/* the graphics engine (the graphics engine is NOT	*/
	/* responsible for allocating or freeing device-specific*/
	/* resources or parameters)				*/
	/* (2) it must initialise the device-specific resources */
	/* and parameters (mostly done by calling device_Open)	*/
	/* (3) it must initialise the generic graphical		*/
	/* parameters that are not initialised by GInit (because*/
	/* only the device knows what values they should have)	*/
	/* see Graphics.h for the official list of these	*/
	/* (4) it may reset generic graphics parameters that	*/
	/* have already been initialised by GInit (although you	*/
	/* should know what you are doing if you do this)	*/
	/* (5) it must attach the device-specific parameters	*/
	/* structure to the device description structure	*/
	/* e.g., dd->deviceSpecfic = (void *) xd;		*/
	/* (6) it must FREE the overall device description if	*/
	/* it wants to bail out to the top-level		*/
	/* the graphics engine is responsible for allocating	*/
	/* the device description and freeing it in most cases	*/
	/* but if the device driver freaks out it needs to do	*/
	/* the clean-up itself					*/
	/********************************************************/


	/*  X11 Device Driver Arguments	:	*/
	/*	1) display name			*/
	/*	2) width (inches)		*/
	/*	3) height (inches)		*/
	/*	4) base pointsize		*/
	/*	5) gamma correction factor	*/
	/*	6) colormodel,			*/
	/*	 see X_COLORTYPE at top of file */
	/*	7) maxcube			*/

Rboolean
X11DeviceDriver(DevDesc *dd,
		char *disp_name,
		double width,
		double height,
		double pointsize,
		double gamma_fac,
		X_COLORTYPE colormodel,
		int maxcube)
{
    int ps;
    x11Desc *xd;

    /* allocate new device description */
    if (!(xd = (x11Desc*)malloc(sizeof(x11Desc))))
	return FALSE;

    /* From here on, if we need to bail out with "error", */
    /* then we must also free(xd). */

    /*	Font will load at first use.  */

    ps = pointsize;
    if (ps < 6 || ps > 24) ps = 12;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->basefontface = 1;
    xd->basefontsize = ps;
    dd->dp.font = 1;
    dd->dp.ps = ps;

    /*	Start the Device Driver and Hardcopy.  */

    if (!X11_Open(dd, xd, disp_name, width, height, 
		  gamma_fac, colormodel, maxcube)) {
	free(xd);
	return FALSE;
    }

    /*	Set up Data Structures. */

    dd->dp.open = X11_Open;
    dd->dp.close = X11_Close;
    dd->dp.activate = X11_Activate;
    dd->dp.deactivate = X11_Deactivate;
    dd->dp.resize = X11_Resize;
    dd->dp.newPage = X11_NewPage;
    dd->dp.clip = X11_Clip;
    dd->dp.strWidth = X11_StrWidth;
    dd->dp.text = X11_Text;
    dd->dp.rect = X11_Rect;
    dd->dp.circle = X11_Circle;
    dd->dp.line = X11_Line;
    dd->dp.polyline = X11_Polyline;
    dd->dp.polygon = X11_Polygon;
    dd->dp.locator = X11_Locator;
    dd->dp.mode = X11_Mode;
    dd->dp.hold = X11_Hold;
    dd->dp.metricInfo = X11_MetricInfo;

    /* Set required graphics parameters. */

    /* Window Dimensions in Pixels */

    dd->dp.left = 0;			/* left */
    dd->dp.right = xd->windowWidth;	/* right */
    dd->dp.bottom = xd->windowHeight;	/* bottom */
    dd->dp.top = 0;			/* top */

    /* Nominal Character Sizes in Pixels */

    SetBaseFont(xd);
    dd->dp.cra[0] = xd->font->max_bounds.rbearing -
	xd->font->min_bounds.lbearing;
    dd->dp.cra[1] = xd->font->max_bounds.ascent +
	xd->font->max_bounds.descent;

    /* Character Addressing Offsets */
    /* These are used to plot a single plotting character */
    /* so that it is exactly over the plotting point */

    dd->dp.xCharOffset = 0.4900;
    dd->dp.yCharOffset = 0.3333;
    dd->dp.yLineBias = 0.1;

    /* Inches per raster unit */

    dd->dp.ipr[0] = pixelWidth();
    dd->dp.ipr[1] = pixelHeight();

    /* Device capabilities */

    dd->dp.canResizePlot = TRUE;
    dd->dp.canChangeFont = FALSE;
    dd->dp.canRotateText = TRUE;
    dd->dp.canResizeText = TRUE;
    dd->dp.canClip = TRUE;
    dd->dp.canHAdj = FALSE;

    dd->dp.ask = FALSE;

    /* initialise x11 device description */
    /* (most of the work has been done in X11_Open) */

    xd->cex = 1.0;
    xd->srt = 0.0;
    xd->lty = 0;
    xd->resize = 0;

    dd->deviceSpecific = (void *) xd;

    dd->displayListOn = TRUE;

    R_ProcessEvents((void*) NULL);

    return TRUE;
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
	DevDesc *dd = GetDevice(d);
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	*pximage = XGetImage(display, xd->window, 0, 0, 
			     xd->windowWidth, xd->windowHeight, 
			     AllPlanes, ZPixmap);
	*pwidth = xd->windowWidth;
	*pheight = xd->windowHeight;
	return TRUE;
    }
}
