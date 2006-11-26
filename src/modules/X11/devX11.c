/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2005  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* The version for R 2.1.0 is partly based on patches by
   Ei-ji Nakama <nakama@ki.rim.or.jp> for use in Japanese.

   <MBCS> all the strings manipulated here like display and fonts specs
   are probably ASCII, or at least start with ASCII in the part searched.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

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
#ifdef HAVE_X11_Xmu
# include <X11/Xmu/Atoms.h>
#endif


#include "Graphics.h"
#include "Fileio.h"		/* R_fopen */
#include "rotated.h"		/* 'Public' routines from here */
/* For the input handlers of the event loop mechanism: */
#include <R_ext/eventloop.h>
#include <R_ext/Memory.h>	/* vmaxget */
#include <Rdevices.h>

#ifdef SUPPORT_MBCS
/* This uses fontsets only in mbcslocales */
# define USE_FONTSET 1
/* In theory we should do this, but it works less well
# ifdef X_HAVE_UTF8_STRING
#  define HAVE_XUTF8TEXTESCAPEMENT 1
#  define HAVE_XUTF8TEXTEXTENTS 1
# endif */
#endif

#define R_X11_DEVICE 1
#include "devX11.h"

#include <Rmodules/RX11.h>

#define CURSOR		XC_crosshair		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

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
			  R_GE_gcontext *gc,
			  NewDevDesc *dd);
static void newX11_Clip(double x0, double x1, double y0, double y1,
			NewDevDesc *dd);
static void newX11_Close(NewDevDesc *dd);
static void newX11_Deactivate(NewDevDesc *dd);
static void newX11_Hold(NewDevDesc *dd);
static Rboolean newX11_Locator(double *x, double *y, NewDevDesc *dd);
static void newX11_Line(double x1, double y1, double x2, double y2,
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static void newX11_MetricInfo(int c,
			      R_GE_gcontext *gc,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd);
static void newX11_Mode(int mode, NewDevDesc *dd);
static void newX11_NewPage(R_GE_gcontext *gc, NewDevDesc *dd);
/* declared in devX11.h
Rboolean newX11_Open(NewDevDesc *dd, newX11Desc *xd,
		     char *dsp, double w, double h,
		     double gamma_fac, X_COLORTYPE colormodel,
		     int maxcube, int bgcolor, int canvascolor, int res); */
static void newX11_Polygon(int n, double *x, double *y,
			   R_GE_gcontext *gc,
			   NewDevDesc *dd);
static void newX11_Polyline(int n, double *x, double *y,
			    R_GE_gcontext *gc,
			    NewDevDesc *dd);
static void newX11_Rect(double x0, double y0, double x1, double y1,
			R_GE_gcontext *gc,
			NewDevDesc *dd);
static void newX11_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd);
static double newX11_StrWidth(char *str,
			      R_GE_gcontext *gc,
			      NewDevDesc *dd);
static void newX11_Text(double x, double y, char *str,
			double rot, double hadj,
			R_GE_gcontext *gc,
			NewDevDesc *dd);

	/*************************************************/
	/* End of list of required device driver actions */
	/*************************************************/

	/* Support Routines */

static void *RLoadFont(newX11Desc*, char*, int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static int SetBaseFont(newX11Desc*);
static void SetColor(int, NewDevDesc*);
static void SetFont(char*, int, int, NewDevDesc*);
static void SetLinetype(R_GE_gcontext*, NewDevDesc*);
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
	warning(_("cannot set grayscale: reverting to monochrome"));
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
		XPalette[i].red	  = pow(r / (nr - 1.0), RedGamma) * 0xffff;
		XPalette[i].green = pow(g / (ng - 1.0), GreenGamma) * 0xffff;
		XPalette[i].blue  = pow(b / (nb - 1.0), BlueGamma) * 0xffff;
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
	    warning(_("X11 driver unable to obtain color cube\n  reverting to monochrome"));
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
	error(_("Error: X11 cannot allocate additional graphics colors.\n\
Consider using X11 with colortype=\"pseudo.cube\" or \"gray\"."));
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
    NewDevDesc *dd = NULL;	/* -Wall */
    newX11Desc *xd;
    int devNum = 0;
    int do_update = 0;

    if (event.xany.type == Expose) {
	while(XCheckTypedEvent(display, Expose, &event))
	    ;
	XFindContext(display, event.xexpose.window,
		     devPtrContext, &temp);
	dd = (NewDevDesc *) temp;
	if (event.xexpose.count == 0)
	    do_update = 1;
    }
    else if (event.type == ConfigureNotify) {
	while(XCheckTypedEvent(display, ConfigureNotify, &event))
	    ;
	XFindContext(display, event.xconfigure.window,
		     devPtrContext, &temp);
	dd = (NewDevDesc *) temp;
	xd = (newX11Desc *) dd->deviceSpecific;
	if (xd->windowWidth != event.xconfigure.width ||
	    xd->windowHeight != event.xconfigure.height)
	    do_update = 1;
	xd->windowWidth = event.xconfigure.width;
	xd->windowHeight = event.xconfigure.height;
        dd->size(&(dd->left), &(dd->right), &(dd->bottom), &(dd->top),
		     dd);

	if (do_update) /* Gobble Expose events; we'll redraw anyway */
	    while(XCheckTypedEvent(display, Expose, &event))
		;
    }
    else if ((event.type == ClientMessage) &&
	     (event.xclient.message_type == _XA_WM_PROTOCOLS))
	if (!inclose && event.xclient.data.l[0] == protocol) {
	    XFindContext(display, event.xclient.window,
			 devPtrContext, &temp);
	    dd = (NewDevDesc *) temp;
	    KillDevice((DevDesc*) GetDevice(devNumber((DevDesc*) dd)));
	}

    if (do_update) {
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
}

static void R_ProcessX11Events(void *data)
{
    XEvent event;

    while (displayOpen && XPending(display)) {
	XNextEvent(display, &event);
	/* printf("%i\n",event.type); */
	handleEvent(event);
    }
}

static char *fontname = "-adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*";
static char *symbolname	 = "-adobe-symbol-medium-r-*-*-%d-*-*-*-*-*-*-*";

static char *slant[]  = {"r", "o"};
static char *weight[] = {"medium", "bold"};

/* Bitmap of the Adobe design sizes */

static unsigned int adobe_sizes = 0x0403165D;

#define MAXFONTS 64
#define CLRFONTS 16 /* Number to free when cache runs full */

typedef struct {
    char family[500];
    int face, size;
    R_XFont *font;
} cacheentry;

static cacheentry fontcache[MAXFONTS];
static int nfonts = 0;
static int force_nonscalable = 0; /* for testing */

#define ADOBE_SIZE(I) ((I) > 7 && (I) < 35 && (adobe_sizes & (1<<((I)-8))))
#define SMALLEST 2


static R_XFont *R_XLoadQueryFont(Display *display, char *name)
{
    R_XFont *tmp;
    tmp = (R_XFont *) malloc(sizeof(R_XFont));
    tmp->type = One_Font;
    tmp->font = XLoadQueryFont(display, name);
    if(tmp->font)
	return tmp;
    else {
	free(tmp);
	return NULL;
    }
}

static void R_XFreeFont(Display *display, R_XFont *font)
{
    if(font->type == Font_Set) XFreeFontSet(display, font->fontset);
    else XFreeFont(display, font->font);
    free(font);
}


/*
 * Can't load Symbolfont to XFontSet!!
 */
#ifdef USE_FONTSET
static R_XFont *R_XLoadQueryFontSet(Display *display,
				    const char *fontset_name)
{
    R_XFont *tmp = (R_XFont *) malloc(sizeof(R_XFont));
    XFontSet fontset;
    int  /*i,*/ missing_charset_count;
    char **missing_charset_list, *def_string;

#ifdef DEBUG_X11
    printf("loading fontset %s\n", fontset_name);
#endif
    fontset = XCreateFontSet(display, fontset_name, &missing_charset_list,
			     &missing_charset_count, &def_string);
    if(!fontset) {
	free(tmp);
	return NULL;
    }
    if (missing_charset_count) {
#ifdef DEBUG_X11
	int i;
	for(i = 0; i < missing_charset_count; i++)
	   warning("font for charset %s is lacking.", missing_charset_list[i]);
	XFreeStringList(missing_charset_list);
#endif
    }
    tmp->type = Font_Set;
    tmp->fontset = fontset;
    return tmp;
}
#endif

static void *RLoadFont(newX11Desc *xd, char* family, int face, int size)
{
    int pixelsize, i, dpi;
    cacheentry *f;
    char buf[BUFSIZ];
#ifdef USE_FONTSET
    char buf1[BUFSIZ];
#endif
    R_XFont *tmp = NULL;

#ifdef DEBUG_X11
    printf("trying face %d size %d\n", face, size);
#endif

    if (size < SMALLEST) size = SMALLEST;
    face--;

    dpi = (1./pixelHeight() + 0.5);
    if(dpi < 80) {
	/* use pointsize as pixel size */
    } else if(abs(dpi - 100) < 5) {
    /* Here's a 1st class fudge: make sure that the Adobe design sizes
       8, 10, 11, 12, 14, 17, 18, 20, 24, 25, 34 can be obtained via
       an integer "size" at 100 dpi, namely 6, 7, 8, 9, 10, 12, 13,
       14, 17, 18, 24 points. It's almost y = x * 100/72, but not
       quite. The constants were found using lm(). --pd */
 	size = R_rint(size * 1.43 - 0.4);
    } else size = R_rint(size * dpi/72);

    /* search fontcache */
    for ( i = nfonts ; i-- ; ) {
	f = &fontcache[i];
	if ( strcmp(f->family, family) == 0 &&
	     f->face == face &&
	     f->size == size )
	    return f->font;
    }

    /* 'size' is the requested size, 'pixelsize'  the size of the
       actually allocated font*/
    pixelsize = size;

    /*
     * The symbol font face is a historical oddity
     * Always use a standard font for font face 5
     */
    if (face == SYMBOL_FONTFACE - 1) /* NB: face-- above */
        sprintf(buf, xd->symbolfamily,  pixelsize);
    else
#ifdef USE_FONTSET
      if (mbcslocale && *slant[(face & 2) >> 1] == 'o') {
        sprintf(buf, family, weight[face & 1], slant[(face & 2) >> 1],
		pixelsize);
        sprintf(buf1, family, weight[face & 1], "i",  pixelsize);
	strcat(buf,",");
	strcat(buf,buf1);
      } else
#endif
	sprintf(buf, family, weight[face & 1], slant[(face & 2) >> 1],
		pixelsize);
#ifdef DEBUG_X11
    Rprintf("loading:\n%s\n",buf);
#endif
    if (!mbcslocale || face == SYMBOL_FONTFACE - 1)
      tmp = R_XLoadQueryFont(display, buf);
#ifdef USE_FONTSET
    else
      tmp = R_XLoadQueryFontSet(display, buf);
#endif

#ifdef DEBUG_X11
    if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
    /*
     * IF can't find the font specified then
     * go to great lengths to find something else to use.
     */
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
#ifdef USE_FONTSET
	    if(mbcslocale)
		tmp = (void*) R_XLoadQueryFontSet(display,
                   "-*-fixed-medium-r-*--13-*-*-*-*-*-*-*");
	    else
#endif
		tmp = (void*) R_XLoadQueryFont(display, "fixed");

	    if (tmp)
		return tmp;
	    else
		error(_("could not find any X11 fonts\nCheck that the Font Path is correct."));
	}

	if ( pixelsize < 8 )
	    pixelsize = 8;
	else if (pixelsize == 9)
	    pixelsize = 8;
	else if (pixelsize < 30) /* must be at least 13 */
	    pixelsize = near[size-13];
	else
	    pixelsize = 34;


	if (face == SYMBOL_FONTFACE - 1)
	    sprintf(buf, symbolname, pixelsize);
	else
	    sprintf(buf, fontname,
		    weight[face & 1],
		    slant[(face & 2) >> 1 ],  pixelsize);
#ifdef DEBUG_X11
	Rprintf("loading:\n%s\n",buf);
#endif
	if (!mbcslocale || face == SYMBOL_FONTFACE - 1)
	    tmp = R_XLoadQueryFont(display, buf);
#ifdef USE_FONTSET
	else
	    tmp = R_XLoadQueryFontSet(display, buf);
#endif
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
		    slant[(face & 2) >> 1 ],  24);
#ifdef DEBUG_X11
	Rprintf("loading:\n%s\n",buf);
#endif

	if (!mbcslocale || face == SYMBOL_FONTFACE - 1)
	    tmp = R_XLoadQueryFont(display, buf);
#ifdef USE_FONTSET
	else
	    tmp = R_XLoadQueryFontSet(display, buf);
#endif

#ifdef DEBUG_X11
	if (tmp) Rprintf("success\n"); else Rprintf("failure\n");
#endif
    }

    if (tmp){
	f = &fontcache[nfonts++];
	strcpy(f->family, family);
	f->face = face;
	f->size = size;
	f->font = tmp;
	if (fabs( (pixelsize - size)/(double)size ) > 0.1)
	    warning(_("X11 used font size %d when %d was requested"),
		    pixelsize, size);
    }
    if (nfonts == MAXFONTS) /* make room in the font cache */
    {
	for (i = 0 ; i < CLRFONTS ; i++)
	      R_XFreeFont(display, fontcache[i].font);
	for (i = CLRFONTS ; i < MAXFONTS ; i++)
	    fontcache[i - CLRFONTS] = fontcache[i];
	nfonts -= CLRFONTS;
    }
    return tmp;
}

/* This should never be used for a Symbol face */
static int SetBaseFont(newX11Desc *xd)
{
    xd->fontface = xd->basefontface;
    if (xd->fontface < 1 || xd->fontface > 5) xd->fontface = 1;
    xd->fontsize = xd->basefontsize;
    xd->usefixed = 0;
    xd->font = RLoadFont(xd, xd->fontfamily, xd->fontface, xd->fontsize);
    if (!xd->font) {
	xd->usefixed = 1;
#ifdef USE_FONTSET
	if(mbcslocale)
	    xd->font = xd->fixedfont =
		R_XLoadQueryFontSet(display,
				    "-*-fixed-medium-r-*--13-*-*-*-*-*-*-*");
	else
#endif
	    xd->font = xd->fixedfont = R_XLoadQueryFont(display, "fixed");
	if (!xd->fixedfont)
	    return 0;
    }
    return 1;
}

static void SetFont(char* family, int face, int size, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    R_XFont *tmp;

    if (face < 1 || face > 5) face = 1;

    if (!xd->usefixed &&
	(size != xd->fontsize	||
	 face != xd->fontface ||
	 strcmp(family, xd->fontfamily) != 0)) {

	tmp = RLoadFont(xd, family, face, size);
	if(tmp) {
	    xd->font = tmp;
	    strcpy(xd->fontfamily, family);
	    xd->fontface = face;
	    xd->fontsize = size;
            /* if (xd->font == One_Font)
	       XSetFont(display, xd->wgc, (xd->font->font)->fid);*/
	} else
	    error(_("X11 font at size %d could not be loaded"), size);
    }
}


static void SetColor(int color, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    if (color != xd->col) {
	blackpixel = GetX11Pixel(R_RED(color), R_GREEN(color), R_BLUE(color));
	xd->col = color;
	XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
    }
}

static int gcToX11lend(R_GE_lineend lend) {
    int newend = CapRound; /* -Wall */
    switch (lend) {
    case GE_ROUND_CAP:
        newend = CapRound;
	break;
    case GE_BUTT_CAP:
        newend = CapButt;
	break;
    case GE_SQUARE_CAP:
        newend = CapProjecting;
	break;
    default:
        error(_("invalid line end"));
    }
    return newend;
}

static int gcToX11ljoin(R_GE_linejoin ljoin) {
    int newjoin = JoinRound; /* -Wall */
    switch (ljoin) {
    case GE_ROUND_JOIN:
        newjoin = JoinRound;
	break;
    case GE_MITRE_JOIN:
        newjoin = JoinMiter;
	break;
    case GE_BEVEL_JOIN:
        newjoin = JoinBevel;
	break;
    default:
        error(_("invalid line join"));
    }
    return newjoin;
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
static void SetLinetype(R_GE_gcontext *gc, NewDevDesc *dd)
{
    static char dashlist[8];
    int i, newlty, newlwd, newlend, newljoin;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    newlty = gc->lty;
    newlwd = gc->lwd;/*cast*/
    newlend = gcToX11lend(gc->lend);
    newljoin = gcToX11ljoin(gc->ljoin);
    if (newlwd < 1)/* not less than 1 pixel */
	newlwd = 1;
    if (newlty != xd->lty || newlwd != xd->lwd ||
	newlend!= xd->lend || newljoin!= xd->ljoin) {
	xd->lty = newlty;
	xd->lwd = newlwd;
	xd->lend = newlend;
	xd->ljoin = newljoin;
	if (newlty == 0) {/* special hack for lty = 0 -- only for X11 */
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineSolid,
			       xd->lend, /* CapRound, */
			       xd->ljoin); /* JoinRound); */
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
	    /* NB if i is odd the pattern will be interpreted as
	       the original pattern concatenated with itself */
	    XSetDashes(display, xd->wgc, 0, dashlist, i);
	    XSetLineAttributes(display,
			       xd->wgc,
			       newlwd,
			       LineOnOffDash,
			       xd->lend, /* CapButt */
			       xd->ljoin); /* JoinRound); */
	}
    }
}

static int R_X11Err(Display *dsp, XErrorEvent *event)
{
    char buff[1000];
    XGetErrorText(dsp, event->error_code, buff, 1000);
    /* for R commander */
    if(strncmp(buff, "BadWindow (invalid Window parameter)", 36) != 0)
        warning(_("X11 protocol error: %s"), buff);
    return 0;
}

static int R_X11IOErr(Display *dsp)
{
    int fd = ConnectionNumber(display);
    /*
    while (nfonts--)  R_XFreeFont(display, fontcache[nfonts].font);
    nfonts = 0;
    */
    removeInputHandler(&R_InputHandlers,
		       getInputHandler(R_InputHandlers,fd));
    /*
    XCloseDisplay(display);
    displayOpen = FALSE;
    */
    error(_("X11 fatal IO error: please save work and shut down R"));
    return 0; /* but should never get here */
}

#define USE_Xt 1

#ifdef USE_Xt
#include <X11/StringDefs.h>
#include <X11/Shell.h>
typedef struct gx_device_X_s {
    Pixel background, foreground, borderColor;
    Dimension borderWidth;
    String geometry;
} gx_device_X;

/* (String) casts are here to suppress warnings about discarding `const' */
#define RINIT(a,b,t,s,o,it,n)\
  {(String)(a), (String)(b), (String)t, sizeof(s),\
   XtOffsetOf(gx_device_X, o), (String)it, (n)}
#define rpix(a,b,o,n)\
  RINIT(a,b,XtRPixel,Pixel,o,XtRString,(XtPointer)(n))
#define rdim(a,b,o,n)\
  RINIT(a,b,XtRDimension,Dimension,o,XtRImmediate,(XtPointer)(n))
#define rstr(a,b,o,n)\
  RINIT(a,b,XtRString,String,o,XtRString,(char*)(n))

static XtResource x_resources[] = {
    rpix(XtNbackground, XtCBackground, background, "XtDefaultBackground"),
    rstr(XtNgeometry, XtCGeometry, geometry, NULL),
};

static const int x_resource_count = XtNumber(x_resources);

static String x_fallback_resources[] = {
    (String) "R_x11*Background: white",
    NULL
};
#endif

Rboolean
newX11_Open(NewDevDesc *dd, newX11Desc *xd, char *dsp, double w, double h,
	 double gamma_fac, X_COLORTYPE colormodel, int maxcube,
	 int bgcolor, int canvascolor, int res, int xpos, int ypos)
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
    static const char *title = "R Graphics";
    XSizeHints *hint;

#ifdef USE_FONTSET
    if (!XSupportsLocale ())
	warning(_("locale not supported by Xlib: some X ops will operate in C locale"));
    if (!XSetLocaleModifiers ("")) warning(_("X cannot set locale modifiers"));
#endif

    if (!strncmp(dsp, "png::", 5)) {
#ifndef HAVE_PNG
	warning(_("no png support in this version of R"));
	return FALSE;
#else
	char buf[PATH_MAX]; /* allow for pageno formats */
	FILE *fp;
	if(strlen(dsp+5) >= PATH_MAX)
	    error(_("filename too long in png() call"));
	strcpy(xd->filename, dsp+5);
	snprintf(buf, PATH_MAX, dsp+5, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning(_("could not open PNG file '%s'"), buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = PNG;
	p = "";
	xd->res_dpi = res; /* place holder */
#endif
    }
    else if (!strncmp(dsp, "jpeg::", 6)) {
#ifndef HAVE_JPEG
	warning(_("no jpeg support in this version of R"));
	return FALSE;
#else
	char buf[PATH_MAX]; /* allow for pageno formats */
	FILE *fp;
	p = strchr(dsp+6, ':'); *p='\0';
	xd->quality = atoi(dsp+6);
	if(strlen(p+1) >= PATH_MAX)
	    error(_("filename too long in jpeg() call"));
	strcpy(xd->filename, p+1);
	snprintf(buf, PATH_MAX, p+1, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning(_("could not open JPEG file '%s'"), buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = JPEG;
	p = "";
	xd->res_dpi = res; /* place holder */
#endif
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
	    warning(_("unable to open connection to X11 display '%s'"), p);
	    return FALSE;
	}
	DisplayOpened = TRUE;
	Rf_setX11Display(display, gamma_fac, colormodel, maxcube, TRUE);
	displayOpen = TRUE;
	if(xd->handleOwnEvents == FALSE)
	    addInputHandler(R_InputHandlers, ConnectionNumber(display),
			    R_ProcessX11Events, XActivity);
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

    xd->fill = bgcolor; /* was transparent */
    xd->col = R_RGB(0, 0, 0);
    xd->canvas = canvascolor;
    if(type == JPEG && !R_OPAQUE(xd->canvas)) {
	warning(_("jpeg() does not support transparency: using white bg"));
	xd->canvas = 0xffffff;
    }
    if(type > WINDOW) xd->fill = xd->canvas;


    /* Try to create a simple window. */
    /* We want to know about exposures */
    /* and window-resizes and locations. */

    /*
     * <MBCS-FIXED>: R on gnome window manager task-bar button see?
     * I try it.
     * A button such as, maximization disappears
     * unless I give Hint for clear statement in
     * gnome window manager.
     */

    memset(&attributes, 0, sizeof(attributes));
    attributes.background_pixel = whitepixel;
    attributes.border_pixel = blackpixel;
    attributes.backing_store = Always;
    attributes.event_mask = ButtonPressMask
      | ExposureMask
      | StructureNotifyMask;

    if (type == WINDOW) {
	int alreadyCreated = (xd->window != (Window)NULL);
	if(alreadyCreated == 0) {
	    xd->windowWidth = iw = (ISNA(w)?7:w)/pixelWidth();
	    xd->windowHeight = ih = (ISNA(h)?7:h)/pixelHeight();

	    hint = XAllocSizeHints();
	    if(xpos == NA_INTEGER)
		hint->x = numX11Devices*20 %
		    ( DisplayWidth(display, screen) - iw - 10 );
	    else hint->x = (xpos >= 0) ? xpos :
		DisplayWidth(display, screen) - iw + xpos;

	    if(ypos == NA_INTEGER)
		hint->y = numX11Devices*20 %
		    ( DisplayHeight(display, screen) + ih - 10 );
	    else hint->y = (ypos >= 0)? ypos :
		DisplayHeight(display, screen) - iw - ypos;
	    hint->width  = iw;
	    hint->height = ih;
	    hint->flags  = PPosition | PSize;
#ifdef USE_Xt
	    {
		XtAppContext app_con;
		Widget toplevel;
		Display *xtdpy;
                int zero = 0;
                gx_device_X xdev;

		XtToolkitInitialize();

		app_con = XtCreateApplicationContext();
		XtAppSetFallbackResources(app_con, x_fallback_resources);
		xtdpy = XtOpenDisplay(app_con, NULL, "r_x11", "R_x11",
				      NULL, 0, &zero, NULL);
		toplevel = XtAppCreateShell(NULL, "R_x11",
					    applicationShellWidgetClass,
					    xtdpy, NULL, 0);
		XtGetApplicationResources(toplevel, (XtPointer) &xdev,
					  x_resources,
					  x_resource_count,
					  NULL, 0);
		XtDestroyWidget(toplevel);
		XtCloseDisplay(xtdpy);
		XtDestroyApplicationContext(app_con);
		if (xdev.geometry != NULL) {
		    char gstr[40];
		    int bitmask;

		    sprintf(gstr, "%dx%d+%d+%d", hint->width,
			    hint->height, hint->x, hint->y);
		    bitmask = XWMGeometry(display, DefaultScreen(display),
					  xdev.geometry, gstr,
					  1,
					  hint,
					  &hint->x, &hint->y,
					  &hint->width, &hint->height,
					  &hint->win_gravity);

		    if (bitmask & (XValue | YValue))
			hint->flags |= USPosition;
		    if (bitmask & (WidthValue | HeightValue))
			hint->flags |= USSize;
		    /* Restore user-specified settings */
		    if(xpos != NA_INTEGER)
			hint->x = (xpos >= 0) ? xpos :
			    DisplayWidth(display, screen) - iw + xpos;
		    if(ypos != NA_INTEGER)
			hint->y = (ypos >= 0)? ypos :
			    DisplayHeight(display, screen) - iw - ypos;
		    if(!ISNA(w)) hint->width = iw;
		    if(!ISNA(h)) hint->height = ih;
		}
	    }
#endif
	    xd->windowWidth = hint->width;
	    xd->windowHeight = hint->height;
	    /*printf("x = %d, y = %d\n", hint->x, hint->y);*/
	    xd->window = XCreateSimpleWindow(display,
					     rootwin,
					     hint->x,hint->y,
					     hint->width, hint->height,
					     1,
					     blackpixel,
					     whitepixel);
	    if (xd->window == 0 ){
	      XFree(hint);
	      warning(_("unable to create X11 window"));
	      return FALSE;
	    }
	    XSetWMNormalHints(display, xd->window, hint);
	    XFree(hint);
      	    XChangeWindowAttributes(display, xd->window,
				    CWEventMask | CWBackPixel |
				    CWBorderPixel | CWBackingStore,
				    &attributes);

	    XStoreName(display, xd->window, title);

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
	if (iw < 20 && ih < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    iw, ih);
	if ((xd->window = XCreatePixmap(
	    display, rootwin,
	    iw, ih, DefaultDepth(display, screen))) == 0) {
	    warning(_("unable to create pixmap"));
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
    /* if ( xd->font->type == One_Font )
       XSetFont(display, xd->wgc, (xd->font->font)->fid);*/

    /* ensure that line drawing is set up at the first */
    /* graphics call */
    xd->lty = -1;
    xd->lwd = -1;
    xd->lend = 0;
    xd->ljoin = 0;

    numX11Devices++;
    return TRUE;
}

/* Return a non-relocatable copy of a string */

static char *SaveFontSpec(SEXP sxp, int offset)
{
    char *s;
    if(!isString(sxp) || length(sxp) <= offset)
	error(_("invalid font specification"));
    s = R_alloc(strlen(CHAR(STRING_ELT(sxp, offset)))+1, sizeof(char));
    strcpy(s, CHAR(STRING_ELT(sxp, offset)));
    return s;
}

/*
 * Take the fontfamily from a gcontext (which is device-independent)
 * and convert it into an X11-specific font description using
 * the X11 font database (see src/library/graphics/R/unix/x11.R)
 *
 * IF gcontext fontfamily is empty ("")
 * OR IF can't find gcontext fontfamily in font database
 * THEN return xd->basefontfamily (the family set up when the
 *   device was created)
 */
static char* translateFontFamily(char* family, newX11Desc* xd) {
    SEXP graphicsNS, x11env, fontdb, fontnames;
    int i, nfonts;
    char* result = xd->basefontfamily;
    PROTECT_INDEX xpi;

    PROTECT(graphicsNS = R_FindNamespace(ScalarString(mkChar("grDevices"))));
    PROTECT_WITH_INDEX(x11env = findVar(install(".X11env"), graphicsNS), &xpi);
    if(TYPEOF(x11env) == PROMSXP)
	REPROTECT(x11env = eval(x11env, graphicsNS), xpi);
    PROTECT(fontdb = findVar(install(".X11.Fonts"), x11env));
    PROTECT(fontnames = getAttrib(fontdb, R_NamesSymbol));
    nfonts = LENGTH(fontdb);
    if (strlen(family) > 0) {
	int found = 0;
	for (i=0; i<nfonts && !found; i++) {
	    char* fontFamily = CHAR(STRING_ELT(fontnames, i));
	    if (strcmp(family, fontFamily) == 0) {
		found = 1;
		result = SaveFontSpec(VECTOR_ELT(fontdb, i), 0);
	    }
	}
	if (!found)
	    warning(_("font family not found in X11 font database"));
    }
    UNPROTECT(4);
    return result;
}

static double newX11_StrWidth(char *str,
			      R_GE_gcontext *gc,
			      NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    int size = gc->cex * gc->ps + 0.5;
    SetFont(translateFontFamily(gc->fontfamily, xd), gc->fontface, size, dd);

#ifdef USE_FONTSET
    if (xd->font->type == One_Font)
	return (double) XTextWidth(xd->font->font, str, strlen(str));
    else  {
#ifdef HAVE_XUTF8TEXTESCAPEMENT
	if(utf8locale)
	    return (double) Xutf8TextEscapement(xd->font->fontset,
						str, strlen(str));
	else
#endif
	    return (double) XmbTextEscapement(xd->font->fontset,
					      str, strlen(str));
    }
#else
    return (double) XTextWidth(xd->font->font, str, strlen(str));
#endif
}


	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void newX11_MetricInfo(int c,
			      R_GE_gcontext *gc,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;
    int first = 0, last = 0;
    int size = gc->cex * gc->ps + 0.5;
    XFontStruct *f = NULL;


    SetFont(translateFontFamily(gc->fontfamily, xd), gc->fontface, size, dd);

#ifdef USE_FONTSET
    *ascent = 0; *descent = 0; *width = 0; /* fallback position */
    if (xd->font) {
	if (xd->font->type != One_Font) {
	    char **ml; XFontStruct **fs_list;
#ifdef DEBUG_X11
	    int i, cnt = XFontsOfFontSet(xd->font->fontset, &fs_list, &ml);

	    for (i = 0; i < cnt; i++) printf("%s\n", ml[i]);
	    printf("--- end of fontlist ---\n\n");
#else
	    XFontsOfFontSet(xd->font->fontset, &fs_list, &ml);
#endif

	    f = fs_list[0];
	} else f = xd->font->font;
	first = f->min_char_or_byte2;
	last = f->max_char_or_byte2;
    } else return;

    if (c == 0) {
        *ascent = f->ascent;
        *descent = f->descent;
        *width = f->max_bounds.width;
	return;
    }

    if (xd->font->type != One_Font) {
	char buf[10];
	wchar_t wc[2] = L" ";XRectangle ink, log;
	wchar_t *wcs=wc;

	wc[0] = (unsigned int) c;

	wcsrtombs(buf, (const wchar_t **)&wcs, sizeof(wc), NULL);
#ifdef HAVE_XUTF8TEXTEXTENTS
	if(utf8locale)
	    Xutf8TextExtents(xd->font->fontset, buf, strlen(buf), &ink, &log);
	else
#endif
	    XmbTextExtents(xd->font->fontset, buf, strlen(buf), &ink, &log);
	/* Rprintf("%d %d %d %d\n", ink.x, ink.y, ink.width, ink.height);
	   Rprintf("%d %d %d %d\n", log.x, log.y, log.width, log.height); */
	*ascent = -ink.y;
	*descent = ink.y + ink.height;
	*width = log.width;
	/* Rprintf("%d %lc w=%f a=%f d=%f\n", c, wc[0],
	            *width, *ascent, *descent);*/
    } else { /* symbol font */
	if(first <= c && c <= last) {
	  /*
	   * <MBCS-FIXED>: try demo(lm.glm,package="stats")
	   * per_char is NULL case.
	   */
	  if(f->per_char) {
	    *ascent = f->per_char[c-first].ascent;
	    *descent = f->per_char[c-first].descent;
	    *width = f->per_char[c-first].width;
	  } else {
	    *ascent = f->max_bounds.ascent;
	    *descent = f->max_bounds.descent;
	    *width = f->max_bounds.width;
	  }
	}
    }
#else
    f = xd->font->font;
    first = f->min_char_or_byte2;
    last = f->max_char_or_byte2;
    if (c == 0) {
	*ascent = f->ascent;
	*descent = f->descent;
	*width = f->max_bounds.width;
    } else if (first <= c && c <= last) {
    /* It seems that per_char could be NULL
       http://www.ac3.edu.au/SGI_Developer/books/XLib_PG/sgi_html/ch06.html
    */
	if(f->per_char) {
	    *ascent = f->per_char[c-first].ascent;
	    *descent = f->per_char[c-first].descent;
	    *width = f->per_char[c-first].width;
	} else {
	    *ascent = f->max_bounds.ascent;
	    *descent = f->max_bounds.descent;
	    *width = f->max_bounds.width;
	}
    } else {
	*ascent = 0;
	*descent = 0;
	*width = 0;
    }
#endif
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

static void newX11_NewPage(R_GE_gcontext *gc,
			   NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type > WINDOW) {
	if (xd->npages++) {
	    /* try to preserve the page we do have */
	    if (xd->type != XIMAGE) X11_Close_bitmap(xd);
	    if (xd->type != XIMAGE && xd->fp != NULL) fclose(xd->fp);
	    if (xd->type == PNG) {
		char buf[PATH_MAX];
		snprintf(buf, PATH_MAX, xd->filename, xd->npages);
		xd->fp = R_fopen(R_ExpandFileName(buf), "w");
		if (!xd->fp)
		    error(_("could not open PNG file '%s'"), buf);
	    }
	    if (xd->type == JPEG) {
		char buf[PATH_MAX];
		snprintf(buf, PATH_MAX, xd->filename, xd->npages);
		xd->fp = R_fopen(R_ExpandFileName(buf), "w");
		if (!xd->fp)
		    error(_("could not open JPEG file '%s'"), buf);
	    }
	    /* error("attempt to draw second page on pixmap device");*/
	}
/* we want to override the default bg="transparent" */
/*	xd->fill = R_OPAQUE(dd->bg) ? dd->bg : xd->canvas; */
	xd->fill = R_OPAQUE(gc->fill) ? gc->fill: PNG_TRANS;
	SetColor(xd->fill, dd);
	xd->clip.x = 0; xd->clip.width = xd->windowWidth;
	xd->clip.y = 0; xd->clip.height = xd->windowHeight;
	XSetClipRectangles(display, xd->wgc, 0, 0, &(xd->clip), 1, Unsorted);
	XFillRectangle(display, xd->window, xd->wgc, 0, 0,
		       xd->windowWidth, xd->windowHeight);
	return;
    }

    FreeX11Colors();
    if ( (model == PSEUDOCOLOR2) || (xd->fill != gc->fill)) {
	xd->fill = R_OPAQUE(gc->fill) ? gc->fill : xd->canvas;
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
		       int bgr, FILE *fp, unsigned int transparent, int res);

extern int R_SaveAsJpeg(void  *d, int width, int height,
			unsigned long (*gp)(XImage *, int, int),
			int bgr, int quality, FILE *outfile, int res);


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
	    /* some 'truecolor' displays distort colours */
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
		    (xd->fill != PNG_TRANS) ? 0 : pngtrans, xd->res_dpi);
    } else if (xd->type == JPEG)
	R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight,
		     bitgp, 0, xd->quality, xd->fp, xd->res_dpi);
    XDestroyImage(xi);
}

static void newX11_Close(NewDevDesc *dd)
{
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    if (xd->type == WINDOW) {
	/* process pending events */
	/* set block on destroy events */
	inclose = TRUE;
	R_ProcessX11Events((void*) NULL);

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
	while (nfonts--)
	      R_XFreeFont(display, fontcache[nfonts].font);
	nfonts = 0;
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
    /**
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, strlen(t));
    **/
    XStoreName(display, xd->window, t);
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
    /**
    XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
		    8, PropModeReplace, (unsigned char*)t, strlen(t));
    **/
    XStoreName(display, xd->window, t);
    XSync(display, 0);
}

static void newX11_Rect(double x0, double y0, double x1, double y1,
			R_GE_gcontext *gc,
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
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, dd);
	XFillRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLinetype(gc, dd);
	XDrawRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

static void newX11_Circle(double x, double y, double r,
			  R_GE_gcontext *gc,
			  NewDevDesc *dd)
{
    int ir, ix, iy;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    ir = floor(r + 0.5);

    ix = (int)x;
    iy = (int)y;
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, dd);
	XFillArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
    if (R_OPAQUE(gc->col)) {
	SetLinetype(gc, dd);
	SetColor(gc->col, dd);
	XDrawArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
}

static void newX11_Line(double x1, double y1, double x2, double y2,
			R_GE_gcontext *gc,
			NewDevDesc *dd)
{
    int xx1, yy1, xx2, yy2;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    /* In-place conversion ok */

    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLinetype(gc, dd);
	XDrawLine(display, xd->window, xd->wgc, xx1, yy1, xx2, yy2);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
}

static void newX11_Polyline(int n, double *x, double *y,
			    R_GE_gcontext *gc,
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

    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLinetype(gc, dd);
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
			   R_GE_gcontext *gc,
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
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, dd);
	XFillPolygon(display, xd->window, xd->wgc, points, n, Complex, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	SetLinetype(gc, dd);
	XDrawLines(display, xd->window, xd->wgc, points, n+1, CoordModeOrigin);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }

    vmaxset(vmax);
}


static void newX11_Text(double x, double y,
			char *str, double rot, double hadj,
			R_GE_gcontext *gc,
			NewDevDesc *dd)
{
    int size;
    newX11Desc *xd = (newX11Desc *) dd->deviceSpecific;

    size = gc->cex * gc->ps + 0.5;
    SetFont(translateFontFamily(gc->fontfamily, xd), gc->fontface, size, dd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, dd);
	XRfRotDrawString(display, xd->font, rot, xd->window,
			 xd->wgc, (int)x, (int)y, str);
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
    R_ProcessX11Events((void*)NULL);	/* discard pending events */
    XSync(display, 1);
    /* handle X events as normal until get a button */
    /* click in the desired device */
    while (!done && displayOpen) {
	XNextEvent(display, &event);
	/* possibly later R_CheckUserInterrupt(); */
	if (event.type == ButtonPress) {
	    XFindContext(display, event.xbutton.window,
			 devPtrContext, &temp);
	    ddEvent = (NewDevDesc *) temp;
	    if (ddEvent == dd) {
		if (event.xbutton.button == Button1) {
		    int useBeep = asLogical(GetOption(install("locatorBell"),
						      R_BaseEnv));
		    *x = event.xbutton.x;
		    *y = event.xbutton.y;
  		       /* Make a beep! Was print "\07", but that
                          messes up some terminals. */
                    if(useBeep) XBell(display, X_BELL_VOLUME);
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
			    int maxcube,
			    int bgcolor,
			    int canvascolor,
			    SEXP sfonts,
			    int res,
			    int xpos, int ypos)
{
    newX11Desc *xd;
    char *fn;

    xd = Rf_allocNewX11DeviceDesc(pointsize);
    if(!xd) return FALSE;

    /* Used to set dd->dp.font=1 and dd->dp.ps=pointsize,
     * but Paul removed that.
     * This sort of initialisation occurs in R base graphics now.
     */

    if(strlen(fn = CHAR(STRING_ELT(sfonts, 0))) > 499) {
	strcpy(xd->basefontfamily, fontname);
	strcpy(xd->fontfamily, fontname);
    } else {
	strcpy(xd->basefontfamily,fn);
	strcpy(xd->fontfamily,fn);
    }
    if(strlen(fn = CHAR(STRING_ELT(sfonts, 1))) > 499)
	strcpy(xd->symbolfamily, symbolname);
    else strcpy(xd->symbolfamily,fn);

    /*	Start the Device Driver and Hardcopy.  */

    if (!newX11_Open((NewDevDesc*)(dd), xd, disp_name, width, height,
		     gamma_fac, colormodel, maxcube, bgcolor,
		     canvascolor, res, xpos, ypos)) {
	free(xd);
	return FALSE;
    }

    Rf_setNewX11DeviceData((NewDevDesc*)(dd), gamma_fac, xd);
    xd->fill = 0xffffffff; /* this is needed to ensure that the
			      first newpage does set whitecolor
			      if par("bg") is not transparent */

#if BUG
    R_ProcessX11Events((void*) NULL);
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
    {
	XFontStruct *f;
#ifdef USE_FONTSET
	/* Use fudge size of M, not the max of the first font or the
	   whole fontset which may have very wide characters if it contains
	   a comprehensive ISO 10646 font.
	 */
	if(xd->font->type == Font_Set) {
	    char buf[10];
	    XRectangle ink, log;

	    wcstombs(buf, L"M", 10);
#ifdef HAVE_XUTF8TEXTEXTENTS
	    if(utf8locale)
		Xutf8TextExtents(xd->font->fontset, buf, strlen(buf), &ink, &log);
	    else
#endif
		XmbTextExtents(xd->font->fontset, buf, strlen(buf), &ink, &log);
	    dd->cra[0] = 2+log.width; /* fudge to allow some space */
	    dd->cra[1] = 2+log.height;/* M has no descenders */
	} else
#endif
	{
	    f = xd->font->font;
	    dd->cra[0] = f->max_bounds.rbearing - f->min_bounds.lbearing;
	    dd->cra[1] = f->max_bounds.ascent + f->max_bounds.descent;
	}
#ifdef DEBUG_X11
	printf("cra = %f %f\n", dd->cra[0], dd->cra[1]);
#endif
    }

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
    dd->startfont = xd->basefontface;
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
	return NULL;

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


static
Rboolean in_R_GetX11Image(int d, void *pximage, int *pwidth, int *pheight)
{
    SEXP dev = elt(findVar(install(".Devices"), R_BaseEnv), d);

    if (TYPEOF(dev) != STRSXP ||
	!(strcmp(CHAR(STRING_ELT(dev, 0)), "XImage") == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "PNG", 3) == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "X11", 3) == 0))
	return FALSE;
    else {
	NewDevDesc *dd = ((GEDevDesc *)GetDevice(d))->dev;
	newX11Desc *xd = dd->deviceSpecific;

	*((XImage**) pximage) =
            XGetImage(display, xd->window, 0, 0,
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

typedef Rboolean (*X11DeviceDriverRoutine)(DevDesc*, char*,
					   double, double, double, double,
					   X_COLORTYPE, int, int);

static SEXP gcall;

/* Return a non-relocatable copy of a string */

static char *SaveString(SEXP sxp, int offset)
{
    char *s;
    if(!isString(sxp) || length(sxp) <= offset)
	errorcall(gcall, _("invalid string argument"));
    s = R_alloc(strlen(CHAR(STRING_ELT(sxp, offset)))+1, sizeof(char));
    strcpy(s, CHAR(STRING_ELT(sxp, offset)));
    return s;
}

static DevDesc*
Rf_addX11Device(char *display, double width, double height, double ps,
		double gamma, int colormodel, int maxcubesize,
		int bgcolor, int canvascolor, char *devname, SEXP sfonts,
		int res, int xpos, int ypos)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (NewDevDesc*)calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->newDevStruct = 1;
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	/* Took out the GInit because MOST of it is setting up
	 * R base graphics parameters.
	 * This is supposed to happen via addDevice now.
	 */
	if (!newX11DeviceDriver((DevDesc*)(dev), display, width, height,
				ps, gamma, colormodel, maxcubesize,
				bgcolor, canvascolor, sfonts, res,
				xpos, ypos)) {
	    free(dev);
	    errorcall(gcall, _("unable to start device %s"), devname);
       	}
	gsetVar(install(".Device"), mkString(devname), R_BaseEnv);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;

    return((DevDesc*) dd);
}

SEXP in_do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char *display, *vmax, *cname, *devname;
    double height, width, ps, gamma;
    int colormodel, maxcubesize, bgcolor, canvascolor, res, xpos, ypos;
    SEXP sc, sfonts;

    checkArity(op, args);
    gcall = call;
    vmax = vmaxget();

    /* Decode the arguments */
    display = SaveString(CAR(args), 0); args = CDR(args);
    width = asReal(CAR(args));	args = CDR(args);
    height = asReal(CAR(args)); args = CDR(args);
    if (width <= 0 || height <= 0)
	errorcall(call, _("invalid 'width' or 'height'"));
    ps = asReal(CAR(args)); args = CDR(args);
    gamma = asReal(CAR(args)); args = CDR(args);
    if (gamma < 0 || gamma > 100)
	errorcall(call, _("invalid '%s' value"), "gamma");

    if (!isValidString(CAR(args)))
	error(_("invalid colortype passed to X11 driver"));
    cname = CHAR(STRING_ELT(CAR(args), 0));
    if (strcmp(cname, "mono") == 0)
	colormodel = 0;
    else if (strcmp(cname, "gray") == 0 || strcmp(cname, "grey") == 0)
	colormodel = 1;
    else if (strcmp(cname, "pseudo.cube") == 0)
	colormodel = 2;
    else if (strcmp(cname, "pseudo") == 0)
	colormodel = 3;
    else if (strcmp(cname, "true") == 0)
	colormodel = 4;
    else {
	warningcall(call,
		    _("unknown X11 color/colour model -- using monochrome"));
	colormodel = 0;
    }
    args = CDR(args);
    maxcubesize = asInteger(CAR(args));
    if (maxcubesize < 1 || maxcubesize > 256)
        maxcubesize = 256;
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	errorcall(call, _("invalid '%s' value"), "bg");
    bgcolor = RGBpar(sc, 0);
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) && !isInteger(sc) && !isLogical(sc) && !isReal(sc))
	errorcall(call, _("invalid '%s' value"), "canvas");
    canvascolor = RGBpar(sc, 0);
    args = CDR(args);
    sfonts = CAR(args);
    if (!isString(sfonts) || LENGTH(sfonts) != 2)
	errorcall(call, _("invalid '%s' value"), "fonts");
    args = CDR(args);
    res = asInteger(CAR(args));
    args = CDR(args);
    xpos = asInteger(CAR(args));
    args = CDR(args);
    ypos = asInteger(CAR(args));

    devname = "X11";
    if (!strncmp(display, "png::", 5)) devname = "PNG";
    else if (!strncmp(display, "jpeg::", 6)) devname = "JPEG";
    else if (!strcmp(display, "XImage")) devname = "XImage";

    Rf_addX11Device(display, width, height, ps, gamma, colormodel,
		    maxcubesize, bgcolor, canvascolor, devname, sfonts,
		    res, xpos, ypos);
    vmaxset(vmax);
    return R_NilValue;
}

extern SEXP RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

static int in_R_X11_access(void)
{
    char *p;

    if (displayOpen) return TRUE;
    if(!(p = getenv("DISPLAY"))) return FALSE;
    if ((display = XOpenDisplay(NULL)) == NULL) {
	return FALSE;
    } else {
	XCloseDisplay(display);
	return TRUE;
    }
}

static Rboolean in_R_X11readclp(Rclpconn this, char *type)
{
    Window clpwin;
    Atom sel = XA_PRIMARY, pty, pty_type;
    XEvent evt;
    unsigned char *buffer;
    unsigned long pty_size, pty_items;
    int pty_format, ret;
    Rboolean res = TRUE;

    if (!displayOpen) {
	if ((display = XOpenDisplay(NULL)) == NULL) {
	    warning(_("unable to contact X11 display"));
	    return FALSE;
	}
    }
    if(strcmp(type, "X11_secondary") == 0) sel = XA_SECONDARY;
    if(strcmp(type, "X11_clipboard") == 0)
#ifdef HAVE_X11_Xmu
      sel = XA_CLIPBOARD(display);
#else
      error("X11 clipboard selection is not supported on this system");
#endif

    pty = XInternAtom(display, "RCLIP_READ", False);

    clpwin = XCreateSimpleWindow(display, DefaultRootWindow(display),
				 0, 0, 1, 1, 0, 0, 0);
    /* <FIXME> this is not optimal in a UTF-8 locale. 
       What we should do is see if UTF-8 extensions are available
       (via X_HAVE_UTF8_STRING) then ask with target TARGETS and see if
       UTF8_STRING is available.  See
       http://www.pps.jussieu.fr/~jch/software/UTF8_STRING/UTF8_STRING.text
    */

    /* send a selection request */
    ret = XConvertSelection(display, sel, XA_STRING, pty, clpwin, CurrentTime);

    /* wait for the response */
    while(1) {
	XNextEvent(display, &evt);
	if (evt.type == SelectionNotify) break;
    }

    /* find the size and format of the data in the selection */
    XGetWindowProperty(display, clpwin, pty, 0, 0, False, AnyPropertyType,
		       &pty_type, &pty_format, &pty_items, &pty_size, &buffer);
    XFree(buffer);
    if (pty_format != 8) { /* bytes */
	warning(_("clipboard cannot be opened or contains no text"));
	res = FALSE;
    } else { /* read the property */
	XGetWindowProperty(display, clpwin, pty, 0, (long)pty_size, False,
			   AnyPropertyType, &pty_type, &pty_format,
			   &pty_items, &pty_size, &buffer);
	this->buff = (char *)malloc(pty_items + 1);
	this->last = this->len = pty_items;
	if(this->buff) {
	    /* property always ends in 'extra' zero byte */
	    memcpy(this->buff, buffer, pty_items + 1);
	} else {
	    warning(_("memory allocation to copy clipboard failed"));
	    res = FALSE;
	}
    }
    XDeleteProperty(display, clpwin, pty);
    XFree(buffer);
    if (!displayOpen) XCloseDisplay(display);
    return res;
}

void R_init_R_X11(DllInfo *info)
{
    R_X11Routines *tmp;
    tmp = (R_X11Routines*) malloc(sizeof(R_X11Routines));
    if(!tmp) {
	error(_("cannot allocate memory for X11Routines structure"));
	return;
    }
    tmp->X11 = in_do_X11;
    tmp->de = RX11_dataentry;
    tmp->image = in_R_GetX11Image;
    tmp->access = in_R_X11_access;
    tmp->readclp = in_R_X11readclp;
     R_setX11Routines(tmp);
}
