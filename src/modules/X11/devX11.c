/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2015  The R Core Team
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

/* The version for R 2.1.0 is partly based on patches by
   Ei-ji Nakama for use in Japanese.

   <MBCS> all the strings manipulated here like display and fonts specs
   are probably ASCII, or at least start with ASCII in the part searched.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

/* rint is C99 */
#ifdef HAVE_RINT
#define R_rint(x) rint(x)
#else
#define R_rint(x) ((int) x + 0.5)
#endif

/* needed on Solaris */
#define XK_MISCELLANY
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
#include <X11/keysymdef.h>


#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include "Fileio.h"		/* R_fopen */
#include "rotated.h"		/* 'Public' routines from here */
/* For the input handlers of the event loop mechanism: */
#include <R_ext/eventloop.h>
#include <R_ext/Memory.h>	/* vmaxget */

/* In theory we should do this, but it works less well
# ifdef X_HAVE_UTF8_STRING
#  define HAVE_XUTF8TEXTESCAPEMENT 1
#  define HAVE_XUTF8TEXTEXTENTS 1
# endif */

typedef int (*X11IOhandler)(Display *);

#include "devX11.h"
#include "rlogo_icon.h" /* hard-coded ARGB icon */

#include <Rmodules/RX11.h>

static Cursor watch_cursor = (Cursor) 0 ;
static Cursor arrow_cursor = (Cursor) 0 ;
static Cursor cross_cursor = (Cursor) 0 ;


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
static char dspname[101]="";
static int screen;				/* Screen */
static Window rootwin;				/* Root Window */
static Visual *visual;				/* Visual */
static int depth;				/* Pixmap depth */
static int Vclass;				/* Visual class */
static X_COLORTYPE model;			/* User color model */
static int maxcubesize;				/* Max colorcube size */
static XSetWindowAttributes attributes;		/* Window attributes */
static Colormap colormap;			/* Default color map */
static int whitepixel;				/* bg overlaying canvas */
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

static void X11_Activate(pDevDesc dd);
static void X11_Circle(double x, double y, double r,
		       const pGEcontext gc, pDevDesc dd);
static void X11_Clip(double x0, double x1, double y0, double y1,
		     pDevDesc dd);
static void X11_Close(pDevDesc dd);
static void X11_Deactivate(pDevDesc dd);
static Rboolean X11_Locator(double *x, double *y, pDevDesc dd);
static void X11_Line(double x1, double y1, double x2, double y2,
		     const pGEcontext gc, pDevDesc dd);
static void X11_MetricInfo(int c, const pGEcontext gc,
			   double* ascent, double* descent,
			   double* width, pDevDesc dd);
static void X11_Mode(int mode, pDevDesc dd);
static void X11_NewPage(const pGEcontext gc, pDevDesc dd);
static void X11_Polygon(int n, double *x, double *y,
			const pGEcontext gc, pDevDesc dd);
static void X11_Polyline(int n, double *x, double *y,
			 const pGEcontext gc, pDevDesc dd);
static void X11_Rect(double x0, double y0, double x1, double y1,
		     const pGEcontext gc, pDevDesc dd);
static void X11_Raster(unsigned int *raster, int w, int h,
                       double x, double y, double width, double height,
                       double rot, Rboolean interpolate,
                       const pGEcontext gc, pDevDesc dd);
static SEXP X11_Cap(pDevDesc dd);
static void X11_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd);
static double X11_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd);
static void X11_Text(double x, double y, const char *str,
		     double rot, double hadj,
		     const pGEcontext gc, pDevDesc dd);
static void X11_eventHelper(pDevDesc dd, int code);

	/*************************************************/
	/* End of list of required device driver actions */
	/*************************************************/

	/* Support Routines */

static void *RLoadFont(pX11Desc, char*, int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static void SetColor(unsigned int, pX11Desc);
static void SetFont(const pGEcontext, pX11Desc);
static void SetLinetype(const pGEcontext, pX11Desc);
static void X11_Close_bitmap(pX11Desc xd);
static char* translateFontFamily(char* family, pX11Desc xd);

static double RedGamma	 = 1.0;
static double GreenGamma = 1.0;
static double BlueGamma	 = 1.0;

#ifdef HAVE_WORKING_CAIRO
# include "cairoFns.c"

	/************************/
	/*        Buffering     */
	/************************/

/*
  Buffering is only implemented for the cairo-based devices.
   The original (Feb 2008) version had two types:
   - "nbcairo".  This wrote directly to a cairo_xlib_surface, xd->cs.
   - "cairo".  This wrote to a cairo_image_surface xd->cs, and copied that to 
     the cairo_xlib_surface (xd->xcs) at mode(0) calls.

   Further types were introduced (experimentally) in May 2011.  We kept:
   - "dbcairo".  Similar to cairo, but the copying is only done when needed
      based on a timer.
   Timing requires a medium-res timer. The current method is to update
   ca 100ms after the last activity (using the event loop) or at a
   mode(0) call if it is 500ms after the last update.
 */

#if (defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)) || defined(HAVE_GETTIMEOFDAY)
/* We need to avoid this in the rare case that it is only in seconds */
extern double currentTime(void); /* from datetime.c */
#else
/* Alternatively, use times() which R >= 2.14.0 requires.  This could
  conceivably wrap around, but on the sort of system where this might
  be used, clock_t is 32-bit (it is typically long or unsigned long)
  and CLK_TCK is 60-100, so it happens after many months of uptime.
*/
# include <sys/times.h>
# ifndef CLK_TCK
#   define CLK_TCK 60
# endif
static double currentTime(void)
{
    struct tms ti;
    return ((double) times(&ti))/CLK_TCK;
}
#endif

static void Cairo_update(pX11Desc xd)
{
    if(inclose || !xd || !xd->buffered || xd->holdlevel > 0) return;
    cairo_paint(xd->xcc);
    /* workaround for bug in cairo 1.12.x (PR#15168) */
    cairo_surface_flush(xd->xcs);
    if (xd->type == WINDOW) XDefineCursor(display, xd->window, arrow_cursor);
    XSync(display, 0);
    xd->last = currentTime();
}


/* 
   We record a linked list of devices which are open and double-buffered.
   The head of the list is a dummy entry to make removals the same for 
   any element.  
*/
struct xd_list {
    pX11Desc this;
   struct xd_list *next;
};

typedef struct xd_list *Xdl;
static struct xd_list xdl0;
static Xdl xdl = &xdl0;

static void CairoHandler(void)
{
    static int  buffer_lock = 0; /* reentrancy guard */
    if (!buffer_lock && xdl->next) {
	double current = currentTime();
	buffer_lock = 1;
	for(Xdl z = xdl->next; z; z = z->next) {
	    pX11Desc xd = z->this;
	    if(xd->last > xd->last_activity) continue;
	    if((current - xd->last) < xd->update_interval) continue;
	    Cairo_update(xd);
	}
	buffer_lock = 0;
    }
}

/* private hooks in sys-std.c */
extern void (* Rg_PolledEvents)(void);
extern int Rg_wait_usec;

/*
  check for updates every 50ms:
  by default the updater is only run >= 100ms after last update.
*/
#define WAIT 50000
static int timingInstalled = 0;
static void addBuffering(pX11Desc xd)
{
    Xdl xdln = (Xdl) malloc(sizeof(struct xd_list));
    xdln->this = xd;
    xdln->next = xdl->next;
    xdl->next = xdln;
    if(timingInstalled) return;
    timingInstalled = 1;
    Rg_PolledEvents = CairoHandler;
    Rg_wait_usec = WAIT;
}

static void removeBuffering(pX11Desc xd)
{
    for(Xdl z = xdl; z->next; z = z->next)
	if (z->next->this == xd) {
	    Xdl old = z->next;
	    z->next = z->next->next;
	    free(old);
	    break; 
	}
    if(xdl->next == NULL) {
	Rg_wait_usec = 0;
	timingInstalled = 0;
    }
}

static void Cairo_NewPage(const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    cairo_reset_clip(xd->cc);
    xd->fill = R_OPAQUE(gc->fill) ? gc->fill: xd->canvas;
    CairoColor(xd->fill, xd);
    cairo_new_path(xd->cc);
    cairo_paint(xd->cc);
    if(xd->buffered) Cairo_update(xd); 
    else XSync(display, 0);
}

static int Cairo_holdflush(pDevDesc dd, int level)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int old = xd->holdlevel;

    if(!xd->buffered) return old;
    xd->holdlevel += level;
    if(xd->holdlevel <= 0) xd->holdlevel = 0;
//    printf("holdlevel = %d\n",  xd->holdlevel);
    /* flush if at level zero - also changes cursor */
    if(xd->holdlevel == 0) {
	if(xd->buffered) Cairo_update(xd);
	else {
	  if (xd->type == WINDOW) XDefineCursor(display, xd->window, arrow_cursor);
	    XSync(display, 0);
	}
    } else if (old == 0) {
	/* May need to flush before holding */
	if(xd->buffered > 1 && xd->last_activity > xd->last) {
	    xd->holdlevel = old;
	    Cairo_update(xd);
	    xd->holdlevel = level;
	}
	if (xd->type == WINDOW) XDefineCursor(display, xd->window, watch_cursor);
	XSync(display, 0);
    }
    return xd->holdlevel;
}
#endif /* HAVE_WORKING_CAIRO */



	/************************/
	/* X11 Color Management */
	/************************/

/* Variables Used To Store Colormap Information */
static struct { int red; int green; int blue; } RPalette[512];
static XColor XPalette[512];
static int PaletteSize;


/* Monochome Displays : Compute pixel values by converting */
/* RGB values to luminance and then thresholding. */
/* See: Foley & van Damm. */

static void SetupMonochrome(void)
{
    depth = 1;
}

static unsigned GetMonochromePixel(int r, int g, int b)
{
    if ((int)(0.299 * r + 0.587 * g + 0.114 * b) > 127)
	return (unsigned) WhitePixel(display, screen);
    else
	return (unsigned) BlackPixel(display, screen);
}


/* Grayscale Displays : Compute pixel values by converting */
/* RGB values to luminance.  See: Foley & van Damm. */

static unsigned GetGrayScalePixel(int r, int g, int b)
{
    unsigned int d, dmin = 0xFFFFFFFF;
    unsigned int dr;
    int i;
    unsigned int pixel = 0;  /* -Wall */
    int gray = (int)((0.299 * r + 0.587 * g + 0.114 * b) + 0.0001);
    for (i = 0; i < PaletteSize; i++) {
	dr = (RPalette[i].red - gray);
	d = dr * dr;
	if (d < dmin) {
	    pixel = (unsigned) XPalette[i].pixel;
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
	RPalette[i].red	  = (unsigned short) ((i * 0xff) / (n - 1));
	RPalette[i].green = RPalette[i].red;
	RPalette[i].blue  = RPalette[i].red;
	/* Gamma correct here */
	XPalette[i].red	  = (unsigned short)((i * 0xffff) / (n - 1));
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

static void SetupGrayScale(void)
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
		XPalette[i].red	  =
		    (unsigned short)(pow(r / (nr - 1.0), RedGamma) * 0xffff);
		XPalette[i].green = 
		    (unsigned short)(pow(g / (ng - 1.0), GreenGamma) * 0xffff);
		XPalette[i].blue  = 
		    (unsigned short)(pow(b / (nb - 1.0), BlueGamma) * 0xffff);
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

static void SetupPseudoColor(void)
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
	    pixel = (unsigned int) XPalette[i].pixel;
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
	    b == RPalette[i].blue) return (unsigned int) XPalette[i].pixel;
    }
    /* Attempt to allocate a new color */
    XPalette[PaletteSize].red	= 
	(unsigned short)(pow(r / 255.0, RedGamma) * 0xffff);
    XPalette[PaletteSize].green = 
	(unsigned short)(pow(g / 255.0, GreenGamma) * 0xffff);
    XPalette[PaletteSize].blue	= 
	(unsigned short)(pow(b / 255.0, BlueGamma) * 0xffff);
    if (PaletteSize == 256 ||
	XAllocColor(display, colormap, &XPalette[PaletteSize]) == 0) {
	error(_("Error: X11 cannot allocate additional graphics colors.\n\
Consider using X11 with colortype=\"pseudo.cube\" or \"gray\"."));
    }
    RPalette[PaletteSize].red = r;
    RPalette[PaletteSize].green = g;
    RPalette[PaletteSize].blue = b;
    PaletteSize++;
    return (unsigned int)XPalette[PaletteSize - 1].pixel;
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

static void SetupTrueColor(void)
{
    RMask = (unsigned int)visual->red_mask;
    GMask = (unsigned int)visual->green_mask;
    BMask = (unsigned int)visual->blue_mask;
    RShift = 0; while ((RMask & 1) == 0) { RShift++; RMask >>= 1; }
    GShift = 0; while ((GMask & 1) == 0) { GShift++; GMask >>= 1; }
    BShift = 0; while ((BMask & 1) == 0) { BShift++; BMask >>= 1; }
}

static unsigned GetTrueColorPixel(int r, int g, int b)
{
    r = (int)(pow((r / 255.0), RedGamma) * 255);
    g = (int)(pow((g / 255.0), GreenGamma) * 255);
    b = (int)(pow((b / 255.0), BlueGamma) * 255);
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

static void FreeX11Colors(void)
{
    int i;
    if (model == PSEUDOCOLOR2) {
	for (i = 0; i < PaletteSize; i++)
	    XFreeColors(display, colormap, &(XPalette[i].pixel), 1, 0);
	PaletteSize = 0;
    }
}

static Rboolean SetupX11Color(void)
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
    if (event.xany.type == Expose) {
	/* ----- window repaint ------ */
	while (XCheckTypedWindowEvent(display, event.xexpose.window, Expose, &event));
	if (inclose) return;
	if (event.xexpose.count != 0) return;
	caddr_t temp;
	XFindContext(display, event.xexpose.window, devPtrContext, &temp);
	pDevDesc dd = (pDevDesc) temp;
	pGEDevDesc gdd = desc2GEDesc(dd);
	if(gdd->dirty) {
#ifdef HAVE_WORKING_CAIRO
	    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
	    /* We can use the buffered copy where we have it */ 
	    if(xd->buffered == 1) {
		cairo_paint(xd->xcc);
		/* workaround for bug in cairo 1.12.x (PR#15168) */
		cairo_surface_flush(xd->xcs);
	    } else if (xd->buffered > 1)
		/* rely on timer to repaint eventually */
		xd->last_activity = currentTime();
	    else
#endif
		GEplayDisplayList(gdd);
	    XSync(display, 0);
	}
    } else if (event.type == ConfigureNotify) {
	while (XCheckTypedEvent(display, ConfigureNotify, &event)) ;
	if (inclose) return;
	caddr_t temp;
	XFindContext(display, event.xconfigure.window, devPtrContext, &temp);
	pDevDesc dd = (pDevDesc) temp;
	pX11Desc xd = (pX11Desc) dd->deviceSpecific;
	if (xd->windowWidth != event.xconfigure.width ||
	    xd->windowHeight != event.xconfigure.height) {

	    /* ----- window resize ------ */

	    xd->windowWidth = event.xconfigure.width;
	    xd->windowHeight = event.xconfigure.height;
#if defined HAVE_WORKING_CAIRO
	    if(xd->useCairo) {
		if(xd->buffered) {
		    cairo_surface_destroy(xd->cs); xd->cs = NULL;
		    cairo_destroy(xd->cc); xd->cc = NULL;
		    cairo_xlib_surface_set_size(xd->xcs, xd->windowWidth,
						    xd->windowHeight);
		    xd->cs = 
			cairo_image_surface_create(CAIRO_FORMAT_RGB24,
						   xd->windowWidth,
						   xd->windowHeight);
		    cairo_status_t res = cairo_surface_status(xd->cs);
		    if (res != CAIRO_STATUS_SUCCESS) {
			warning("cairo error '%s'", 
				cairo_status_to_string(res));
			error("fatal error on resize: please shut down the device");
		    }
		    xd->cc = cairo_create(xd->cs);
		    cairo_set_antialias(xd->cc, xd->antialias);
		    cairo_set_source_surface (xd->xcc, xd->cs, 0, 0);
		} else { /* not buffered */
		    cairo_xlib_surface_set_size(xd->cs, xd->windowWidth,
						xd->windowHeight);
		    cairo_reset_clip(xd->cc);
		}
	    }
#endif
	    dd->size(&(dd->left), &(dd->right), &(dd->bottom), &(dd->top), dd);	
	    /* gobble Expose events; we'll redraw anyway */
	    while (XCheckTypedWindowEvent(display, event.xexpose.window, Expose, &event));
	    pGEDevDesc gdd = desc2GEDesc(dd);
	    if(gdd->dirty) {
		GEplayDisplayList(gdd);
		XSync(display, 0);
	    }
	}
    } else if ((event.type == ClientMessage) &&
	     (event.xclient.message_type == _XA_WM_PROTOCOLS)) {
	if (!inclose && event.xclient.data.l[0] == protocol) {
	    caddr_t temp;
	    XFindContext(display, event.xclient.window, devPtrContext, &temp);
	    killDevice(ndevNumber((pDevDesc) temp));
	}
    }
}

static void R_ProcessX11Events(void *data)
{
    XEvent event;

    while (!R_isForkedChild && displayOpen && XPending(display)) {
	XNextEvent(display, &event);
	/* printf("%i\n",event.type); */
	handleEvent(event);
    }
}


	/************************/
	/* X11 Font Management  */
	/************************/

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


static void *RLoadFont(pX11Desc xd, char* family, int face, int size)
{
    /* size is in points here */
    int pixelsize, i, dpi;
    cacheentry *f;
    char buf[BUFSIZ];
    char buf1[BUFSIZ];
    R_XFont *tmp = NULL;

#ifdef DEBUG_X11
    printf("trying face %d size %d\n", face, size);
#endif

    if (size < SMALLEST) size = SMALLEST;
    face--;

    if(xd->type == PNG || xd->type == JPEG ||
       xd->type == TIFF || xd->type == BMP) {
	dpi = (xd->res_dpi > 0) ? (int)(xd->res_dpi + 0.5) : 72;
    } else {
	dpi = (int)(1./pixelHeight() + 0.5);
    }

    if(abs(dpi - 75) < 5) {
	/* use pointsize as pixel size */
    } else if(abs(dpi - 100) < 5) {
    /* Here's a 1st class fudge: make sure that the Adobe design sizes
       8, 10, 11, 12, 14, 17, 18, 20, 24, 25, 34 can be obtained via
       an integer "size" at 100 dpi, namely 6, 7, 8, 9, 10, 12, 13,
       14, 17, 18, 24 points. It's almost y = x * 100/72, but not
       quite. The constants were found using lm(). --pd */
	size = (int) R_rint(size * 1.43 - 0.4);
    } else size = (int) R_rint(size * dpi/72);

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
      if (mbcslocale && *slant[(face & 2) >> 1] == 'o') {
	sprintf(buf, family, weight[face & 1], slant[(face & 2) >> 1],
		pixelsize);
	sprintf(buf1, family, weight[face & 1], "i",  pixelsize);
	strcat(buf,",");
	strcat(buf,buf1);
      } else
	  sprintf(buf, family, weight[face & 1], slant[(face & 2) >> 1],
		  pixelsize);
#ifdef DEBUG_X11
    Rprintf("loading:\n%s\n",buf);
#endif
    if (!mbcslocale || face == SYMBOL_FONTFACE - 1)
      tmp = R_XLoadQueryFont(display, buf);
    else
      tmp = R_XLoadQueryFontSet(display, buf);

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
	    if(tmp)
		R_XFreeFont(display, tmp);
	    if(mbcslocale)
		tmp = (void*) R_XLoadQueryFontSet(display,
		   "-*-fixed-medium-r-*--13-*-*-*-*-*-*-*");
	    else
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
	else
	    tmp = R_XLoadQueryFontSet(display, buf);
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
	else
	    tmp = R_XLoadQueryFontSet(display, buf);

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


static void SetFont(const pGEcontext gc, pX11Desc xd)
{
    R_XFont *tmp;
    char *family = translateFontFamily(gc->fontfamily, xd);
    /* size is in points here */
    int size = (int)(gc->cex * gc->ps + 0.5), face = gc->fontface;

    if (face < 1 || face > 5) face = 1;

    if (size != xd->fontsize	|| face != xd->fontface ||
	strcmp(family, xd->fontfamily) != 0) {

	tmp = RLoadFont(xd, family, face, size);
	if(tmp) {
	    xd->font = tmp;
	    strcpy(xd->fontfamily, family);
	    xd->fontface = face;
	    xd->fontsize = size;
	} else
	    error(_("X11 font %s, face %d at size %d could not be loaded"),
		  family, face, size);
    }
}

static void CheckAlpha(int color, pX11Desc xd)
{
    unsigned int alpha = R_ALPHA(color);
    if (alpha > 0 && alpha < 255 && !xd->warn_trans) {
	warning(_("semi-transparency is not supported on this device: reported only once per page"));
	xd->warn_trans = TRUE;
    }
}

static void SetColor(unsigned int color, pX11Desc xd)
{
    if (color != xd->col) {
	int col = GetX11Pixel(R_RED(color), R_GREEN(color), R_BLUE(color));
	xd->col = color;
	XSetState(display, xd->wgc, col, whitepixel, GXcopy, AllPlanes);
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

/* --> See "Notes on Line Textures" in GraphicsEngine.h
 *
 *	27/5/98 Paul - change to allow lty and lwd to interact:
 *	the line texture is now scaled by the line width so that,
 *	for example, a wide (lwd=2) dotted line (lty=2) has bigger
 *	dots which are more widely spaced.  Previously, such a line
 *	would have "dots" which were wide, but not long, nor widely
 *	spaced.
 */

/* Not at all clear the optimization here is worth it */
static void SetLinetype(const pGEcontext gc, pX11Desc xd)
{
    int i, newlty, newlend, newljoin;
    double newlwd;

    newlty = gc->lty;
    newlwd = gc->lwd;
    if (newlwd < 1)/* not less than 1 pixel */
	newlwd = 1;
    if (newlty != xd->lty || newlwd != xd->lwd ||
	gc->lend != xd->lend || gc->ljoin != xd->ljoin) {
	xd->lty = newlty;
	xd->lwd = newlwd;
	xd->lend = gc->lend;
	xd->ljoin = gc->ljoin;
	newlend = gcToX11lend(gc->lend);
	newljoin = gcToX11ljoin(gc->ljoin);
	if (newlty == 0 || newlty == NA_INTEGER) {/* special hack for lty = 0 -- only for X11 */
	    XSetLineAttributes(display, xd->wgc,
			       (int)(newlwd*xd->lwdscale+0.5),
			       LineSolid, newlend, newljoin);
	} else {
	    static char dashlist[8];
	    for(i = 0 ; i < 8 && (newlty != 0); i++) {
		int j = newlty & 15;
		if (j == 0) j = 1; /* Or we die with an X Error */
		/* scale line texture for line width */
		j = (int)(j*newlwd*xd->lwdscale+0.5);
		/* make sure that scaled line texture */
		/* does not exceed X11 storage limits */
		if (j > 255) j = 255;
		dashlist[i] = (char) j;
		newlty >>= 4;
	    }
	    /* NB if i is odd the pattern will be interpreted as
	       the original pattern concatenated with itself */
	    XSetDashes(display, xd->wgc, 0, dashlist, i);
	    XSetLineAttributes(display, xd->wgc,
			       (int)(newlwd*xd->lwdscale+0.5),
			       LineOnOffDash, newlend, newljoin);
	}
    }
}

/* Error handling. FIXME: This is rather sloppy; we ought to respect
   any 3rd party handlers by checking whether dsp is "our" display and
   calling the previous handler otherwise. */

static int R_X11Err(Display *dsp, XErrorEvent *event)
{
    char buff[1000];
    /* for tcl/tk */
    if (event->error_code == BadWindow) return 0;

    XGetErrorText(dsp, event->error_code, buff, 1000);
    warning(_("X11 protocol error: %s"), buff);
    return 0;
}

static int NORET R_X11IOErrSimple(Display *dsp)
{
    char *dn = XDisplayName(dspname);
    strcpy(dspname, "");
    error(_("X11 I/O error while opening X11 connection to '%s'"), dn);
}

static int NORET R_X11IOErr(Display *dsp)
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
    strcpy(dspname, "");
    */
    error(_("X11 fatal IO error: please save work and shut down R"));
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
X11_Open(pDevDesc dd, pX11Desc xd, const char *dsp,
	 double w, double h, double gamma_fac, X_COLORTYPE colormodel,
	 int maxcube, int bgcolor, int canvascolor, int res,
	 int xpos, int ypos)
{
    /* if we have to bail out with "error", then must free(dd) and free(xd) */
    /* That means the *caller*: the X11DeviceDriver code frees xd, for example */

    XEvent event;
    int iw, ih, blackpixel;
    X_GTYPE type;
    const char *p = dsp;
    XGCValues gcv;
    XSizeHints *hint;

    if (!XSupportsLocale ())
	warning(_("locale not supported by Xlib: some X ops will operate in C locale"));
    if (!XSetLocaleModifiers ("")) warning(_("X cannot set locale modifiers"));

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
	dd->displayListOn = FALSE;
#endif
    }
    else if (!strncmp(dsp, "jpeg::", 6)) {
#ifndef HAVE_JPEG
	warning(_("no jpeg support in this version of R"));
	return FALSE;
#else
	char buf[PATH_MAX]; /* allow for pageno formats */
	char tmp[PATH_MAX], *pp;
	FILE *fp;
	strcpy(tmp, dsp+6);
	pp = strchr(tmp, ':'); *pp='\0';
	xd->quality = atoi(dsp+6);
	if(strlen(pp+1) >= PATH_MAX)
	    error(_("filename too long in jpeg() call"));
	strcpy(xd->filename, pp+1);
	snprintf(buf, PATH_MAX, pp+1, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning(_("could not open JPEG file '%s'"), buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = JPEG;
	p = "";
	xd->res_dpi = res; /* place holder */
	dd->displayListOn = FALSE;
#endif
    }
    else if (!strncmp(dsp, "tiff::", 5)) {
#ifndef HAVE_TIFF
	warning(_("no tiff support in this version of R"));
	return FALSE;
#else
	char tmp[PATH_MAX], *pp;
	strcpy(tmp, dsp+6);
	pp = strchr(tmp, ':'); *pp='\0';
	xd->quality = atoi(dsp+6);
	if(strlen(pp+1) >= PATH_MAX)
	    error(_("filename too long in tiff() call"));
	strcpy(xd->filename, pp+1);
	xd->fp = NULL;
	type = TIFF;
	p = "";
	xd->res_dpi = res; /* place holder */
	dd->displayListOn = FALSE;
#endif
    } else if (!strncmp(dsp, "bmp::", 5)) {
	char buf[PATH_MAX]; /* allow for pageno formats */
	FILE *fp;
	if(strlen(dsp+5) >= PATH_MAX)
	    error(_("filename too long in bmp() call"));
	strcpy(xd->filename, dsp+5);
	snprintf(buf, PATH_MAX, dsp+5, 1); /* page 1 to start */
	if (!(fp = R_fopen(R_ExpandFileName(buf), "w"))) {
	    warning(_("could not open BMP file '%s'"), buf);
	    return FALSE;
	}
	xd->fp = fp;
	type = BMP;
	p = "";
	xd->res_dpi = res; /* place holder */
	dd->displayListOn = FALSE;
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
	/* Bill Dunlap sees an error when tunneling to a non-existent
	   X11 connection that BDR cannot reproduce.  We leave a handler set
	   if we get an error, but that is rare.
	*/
	X11IOhandler old;
	strncpy(dspname, p, 101);
	dspname[100] = '\0';
	old = XSetIOErrorHandler(R_X11IOErrSimple);
	if ((display = XOpenDisplay(p)) == NULL) {
	    XSetIOErrorHandler(old);
	    warning(_("unable to open connection to X11 display '%s'"), p);
	    return FALSE;
	}
	XSetIOErrorHandler(old);
	Rf_setX11Display(display, gamma_fac, colormodel, maxcube, TRUE);
	displayOpen = TRUE;
	if(xd->handleOwnEvents == FALSE)
	    addInputHandler(R_InputHandlers, ConnectionNumber(display),
			    R_ProcessX11Events, XActivity);
    } else if(strcmp(p, dspname))
	warning(_("ignoring 'display' argument as an X11 device is already open"));
    whitepixel = GetX11Pixel(R_RED(canvascolor), R_GREEN(canvascolor),
			     R_BLUE(canvascolor));
    blackpixel = GetX11Pixel(0, 0, 0);
#ifdef HAVE_WORKING_CAIRO
    if(xd->useCairo && Vclass != TrueColor) {
	warning(_("cairo-based types may only work correctly on TrueColor visuals"));
    }
#endif

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
      | ButtonMotionMask 
      | PointerMotionHintMask
      | ButtonReleaseMask
      | ExposureMask
      | StructureNotifyMask
      | KeyPressMask;


    if (type == WINDOW) {
	int alreadyCreated = (xd->window != (Window)NULL);
	if(alreadyCreated == 0) {
	    xd->windowWidth = iw = (int)((ISNA(w)?7:w)/pixelWidth());
	    xd->windowHeight = ih = (int)((ISNA(h)?7:h)/pixelHeight());

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
		xtdpy = XtOpenDisplay(app_con, dspname, "r_x11", "R_x11",
				      NULL, 0, &zero, NULL);
		if(xtdpy) {
		    toplevel = XtAppCreateShell(NULL, "R_x11",
						applicationShellWidgetClass,
						xtdpy, NULL, 0);
		    XtGetApplicationResources(toplevel, (XtPointer) &xdev,
					      x_resources,
					      x_resource_count,
					      NULL, 0);
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
		    XtDestroyWidget(toplevel);
		    XtCloseDisplay(xtdpy);
		} else {
		    warning(_("unable to obtain information on display '%s'"),
			    dsp);
		}
		XtDestroyApplicationContext(app_con);
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
	    if (xd->window == 0 ) {
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

	    XStoreName(display, xd->window, xd->title);

	    /* See (PR#14588) */
	    XClassHint *chint;
	    chint = XAllocClassHint();
	    if (chint) {
		chint->res_name = "r_x11";
		chint->res_class = "R_x11";
		XSetClassHint(display, xd->window, chint);
	    	XFree(chint);
	    }

            /* set window icon */
            XChangeProperty(display, xd->window,
                            XInternAtom(display, "_NET_WM_ICON", False),
                            XInternAtom(display, "CARDINAL", False), 32,
                            PropModeReplace,
                            (const unsigned char*) rlogo_icon, 2 + 48*48);

	    /* set up protocols so that window manager sends */
	    /* me an event when user "destroys" window */
	    _XA_WM_PROTOCOLS = XInternAtom(display, "WM_PROTOCOLS", 0);
	    protocol = XInternAtom(display, "WM_DELETE_WINDOW", 0);
	    XSetWMProtocols(display, xd->window, &protocol, 1);

	    if(!arrow_cursor)
		arrow_cursor = XCreateFontCursor(display, XC_left_ptr) ;
	    if(!cross_cursor)
		cross_cursor = XCreateFontCursor(display, XC_crosshair);
	    if(!watch_cursor)
		watch_cursor = XCreateFontCursor(display, XC_watch) ;
	    if(xd->type==WINDOW) XDefineCursor(display, xd->window, arrow_cursor);

#ifdef HAVE_WORKING_CAIRO
	    if(xd->useCairo) {
		cairo_status_t res;
		if(xd->buffered) {
		    xd->xcs = 
			cairo_xlib_surface_create(display, xd->window,
						  visual,
						  xd->windowWidth,
						  xd->windowHeight);
		    res = cairo_surface_status(xd->xcs);
		    if (res != CAIRO_STATUS_SUCCESS) {
			warning("cairo error '%s'",
				cairo_status_to_string(res));
			/* bail out */
			return FALSE;
		    }
		    xd->xcc = cairo_create(xd->xcs);
		    res = cairo_status(xd->xcc);
		    if (res != CAIRO_STATUS_SUCCESS) {
			warning("cairo error '%s'", 
				cairo_status_to_string(res));
			cairo_surface_destroy(xd->xcs);
			/* bail out */
			return FALSE;
		    }
		    xd->cs = 
			cairo_image_surface_create(CAIRO_FORMAT_RGB24,
						   xd->windowWidth,
						   xd->windowHeight);
		    cairo_set_source_surface (xd->xcc, xd->cs, 0, 0);
		    if(xd->buffered > 1) addBuffering(xd);
		} else /* non-buffered */
		    xd->cs = 
			cairo_xlib_surface_create(display, xd->window,
						  visual,
						  xd->windowWidth,
						  xd->windowHeight);

		res = cairo_surface_status(xd->cs);
		if (res != CAIRO_STATUS_SUCCESS) {
		    warning("cairo error '%s'", cairo_status_to_string(res));
		    /* bail out */
		    if(xd->xcs) cairo_surface_destroy(xd->xcs);
		    if(xd->xcc) cairo_destroy(xd->xcc);
		    return FALSE;
		}
		xd->cc = cairo_create(xd->cs);
		res = cairo_status(xd->cc);
		if (res != CAIRO_STATUS_SUCCESS) {
		    warning("cairo error '%s'", cairo_status_to_string(res));
		    cairo_surface_destroy(xd->cs);
		    /* bail out */
		    if(xd->xcs) cairo_surface_destroy(xd->xcs);
		    if(xd->xcc) cairo_destroy(xd->xcc);
		    return FALSE;
		}
		cairo_set_operator(xd->cc, CAIRO_OPERATOR_OVER);
		cairo_set_antialias(xd->cc, xd->antialias);
		CairoColor(xd->canvas, xd);
		cairo_new_path(xd->cc);
		cairo_paint(xd->cc);
	    }
#endif
	}
	/* Save the pDevDesc with the window for event dispatching */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);

	/* Map the window */
	if(alreadyCreated == 0) {
	    XSelectInput(display, xd->window,
			 ExposureMask | ButtonPressMask | StructureNotifyMask 
			 | ButtonReleaseMask | ButtonMotionMask  
                         | PointerMotionHintMask | KeyPressMask);
	    XMapWindow(display, xd->window);
	    XSync(display, 0);

	    /* Gobble MapNotify events */

	    while ( XPeekEvent(display, &event),
		    !XCheckTypedEvent(display, MapNotify, &event))
		;
	    /* XNextEvent(display, &event);
	       if (event.xany.type == Expose) {
	       while (event.xexpose.count)
	       XNextEvent(display, &event);
	       }
	    */
	}
    } else { /* PIXMAP */
	xd->windowWidth = iw = (int) w;
	xd->windowHeight = ih = (int) h;
	if (iw < 20 && ih < 20)
	    warning(_("'width=%d, height=%d' are unlikely values in pixels"),
		    iw, ih);
	if ((xd->window = XCreatePixmap(
	    display, rootwin,
	    iw, ih, DefaultDepth(display, screen))) == 0) {
	    warning(_("unable to create pixmap"));
	    return FALSE;
	}
	/* Save the pDevDesc with the window for event dispatching */
	/* Is this needed? */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);
	xd->npages = 0;
    }

    /* Set the graphics context */

    gcv.arc_mode = ArcChord;
    xd->wgc = XCreateGC(display, xd->window, GCArcMode, &gcv);
    XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);

    /* ensure that line drawing is set up at the first graphics call */
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
static char* translateFontFamily(char* family, pX11Desc xd)
{
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
    if (family[0]) {
	Rboolean found = FALSE;
	for (i = 0; i < nfonts && !found; i++) {
	    const char* fontFamily = CHAR(STRING_ELT(fontnames, i));
	    if (strcmp(family, fontFamily) == 0) {
		found = TRUE;
		result = SaveFontSpec(VECTOR_ELT(fontdb, i), 0);
	    }
	}
	if (!found)
	    warning(_("font family not found in X11 font database"));
    }
    UNPROTECT(4);
    return result;
}

static double X11_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    SetFont(gc, xd);

    if (xd->font->type == One_Font)
	return (double) XTextWidth(xd->font->font, str, (int)strlen(str));
    else  {
#ifdef HAVE_XUTF8TEXTESCAPEMENT
	if(utf8locale)
	    return (double) Xutf8TextEscapement(xd->font->fontset,
						str, (int)strlen(str));
	else
#endif
	    return (double) XmbTextEscapement(xd->font->fontset,
					      str, (int)strlen(str));
    }
}


	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void X11_MetricInfo(int c, const pGEcontext gc,
			   double* ascent, double* descent,
			   double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int first = 0, last = 0;
    XFontStruct *f = NULL;

    if (c < 0)
	error(_("invalid use of %d < 0 in '%s'"), c, "X11_MetricInfo");

    SetFont(gc, xd);

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

    if (xd->font->type != One_Font) {  /* so an MBCS */
	XRectangle ink, log;
	char buf[16];

	ucstomb(buf, (unsigned int) c);
#ifdef HAVE_XUTF8TEXTEXTENTS
	if(utf8locale)
	    Xutf8TextExtents(xd->font->fontset, buf, (int)strlen(buf), &ink, &log);
	else
#endif
	    XmbTextExtents(xd->font->fontset, buf, (int)strlen(buf), &ink, &log);
	/* Rprintf("%d %d %d %d\n", ink.x, ink.y, ink.width, ink.height);
	   Rprintf("%d %d %d %d\n", log.x, log.y, log.width, log.height); */
	*ascent = -ink.y;
	*descent = ink.y + ink.height;
	/* <FIXME> why logical and not ink width? */
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
}

static void X11_Clip(double x0, double x1, double y0, double y1,
			pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (x0 < x1) {
	xd->clip.x = (unsigned short) x0 ;
	xd->clip.width = (unsigned short) x1 - (unsigned short) x0 + 1;
    }
    else {
	xd->clip.x = (unsigned short) x1;
	xd->clip.width = (unsigned short) x0 - (unsigned short) x1 + 1;
    }

    if (y0 < y1) {
	xd->clip.y = (unsigned short) y0;
	xd->clip.height = (unsigned short) y1 -  (unsigned short) y0 + 1;
    }
    else {
	xd->clip.y = (unsigned short) y1;
	xd->clip.height = (unsigned short) y0 - (unsigned short) y1 + 1;
    }

    XSetClipRectangles(display, xd->wgc, 0, 0, &(xd->clip), 1, Unsorted);
}

static void X11_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    *left = 0.0;
    *right = xd->windowWidth;
    *bottom = xd->windowHeight;
    *top = 0.0;
}

static void X11_NewPage(const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    xd->warn_trans = FALSE;
    if (xd->type > WINDOW) {
	if (xd->npages++) {
	    /* try to preserve the page we do have */
	    if (xd->type != XIMAGE) X11_Close_bitmap(xd);
	    if (xd->type != XIMAGE && xd->fp != NULL) fclose(xd->fp);
	    if (xd->type == PNG || xd->type == JPEG || xd->type == BMP) {
		char buf[PATH_MAX];
		snprintf(buf, PATH_MAX, xd->filename, xd->npages);
		xd->fp = R_fopen(R_ExpandFileName(buf), "w");
		if (!xd->fp)
		    error(_("could not open file '%s'"), buf);
	    }
	}
	CheckAlpha(gc->fill, xd);
	xd->fill = R_OPAQUE(gc->fill) ? gc->fill: PNG_TRANS;
	SetColor(xd->fill, xd);
	xd->clip.x = 0; xd->clip.width = (unsigned short)xd->windowWidth;
	xd->clip.y = 0; xd->clip.height = (unsigned short)xd->windowHeight;
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
    XSync(display, 0);
}

#include "bitmap.h"

static int knowncols[512];

static unsigned int bitgp(void *xi, int x, int y)
{
    int i, r, g, b;
    XColor xcol;

    /*	returns the colour of the (x,y) pixel stored as RGB */
    i = (int) XGetPixel((XImage *) xi, y, x);
    switch(model) {
    case MONOCHROME:
	return i == 0 ? 0xFFFFFFFF : 0;
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
	    return knowncols[i] | 0xFF000000;
	} else {
	    xcol.pixel = i;
	    XQueryColor(display, colormap, &xcol);
	    return ((xcol.red>>8)<<16) | ((xcol.green>>8)<<8) | (xcol.blue>>8);
	}
    case TRUECOLOR:
	r = ((i>>RShift)&RMask) * 255 /(RMask);
	g = ((i>>GShift)&GMask) * 255 /(GMask);
	b = ((i>>BShift)&BMask) * 255 /(BMask);
	return (r<<16) | (g<<8) | b | 0xFF000000;
    default:
	return 0;
    }
    /* return 0;  not reached, needed for some compilers */
}

static void X11_Close_bitmap(pX11Desc xd)
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
	    pngtrans = (r<<16) | (g<<8) | b | 0xFF000000;
	}
	R_SaveAsPng(xi, xd->windowWidth, xd->windowHeight,
		    bitgp, 0, xd->fp,
		    (xd->fill != PNG_TRANS) ? 0 : pngtrans, xd->res_dpi);
    } else if (xd->type == JPEG)
	R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight,
		     bitgp, 0, xd->quality, xd->fp, xd->res_dpi);
    else if (xd->type == BMP)
	R_SaveAsBmp(xi, xd->windowWidth, xd->windowHeight,
		    bitgp, 0, xd->fp, xd->res_dpi);
    else if (xd->type == TIFF) {
	char buf[PATH_MAX];
	snprintf(buf, PATH_MAX, xd->filename, xd->npages);
	R_SaveAsTIFF(xi, xd->windowWidth, xd->windowHeight,
		     bitgp, 0, R_ExpandFileName(buf), xd->res_dpi,
		     xd->quality);
    }

    XDestroyImage(xi);
}

static void X11_Close(pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (xd->type == WINDOW) {
#ifdef HAVE_WORKING_CAIRO
	if(xd->buffered > 1) removeBuffering(xd);
#endif
	/* process pending events */
	/* set block on destroy events */
	inclose = TRUE;
	R_ProcessX11Events((void*) NULL);

#ifdef HAVE_WORKING_CAIRO
	if(xd->useCairo) {
	    if(xd->cs) cairo_surface_destroy(xd->cs);
	    if(xd->cc) cairo_destroy(xd->cc);
	    if(xd->xcs) cairo_surface_destroy(xd->xcs);
	    if(xd->xcc) cairo_destroy(xd->xcc);
	}
#endif

	XFreeGC(display, xd->wgc);
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
	if(arrow_cursor) XFreeCursor(display, arrow_cursor);
	if(cross_cursor) XFreeCursor(display, cross_cursor);
	if(watch_cursor) XFreeCursor(display, watch_cursor);
	arrow_cursor = cross_cursor = watch_cursor = (Cursor) 0;
	XCloseDisplay(display);
	displayOpen = FALSE;
    }

    free(xd);
    inclose = FALSE;
}

static void X11_Activate(pDevDesc dd)
{
    char t[150];
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    if(xd->title[0]) {
	snprintf(t, 140, xd->title, ndevNumber(dd) + 1);
	t[139] = '\0';
    } else {
	sprintf(t, "R Graphics: Device %d", ndevNumber(dd) + 1);
    }
    strcat(t, " (ACTIVE)");
    XStoreName(display, xd->window, t);
    XSync(display, 0);
}

static void X11_Deactivate(pDevDesc dd)
{
    char t[150];
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (xd->type > WINDOW) return;
    if(xd->title[0]) {
	snprintf(t, 140, xd->title, ndevNumber(dd) + 1);
	t[139] = '\0';
    } else {
	sprintf(t, "R Graphics: Device %d", ndevNumber(dd) + 1);
    }
    strcat(t, " (inactive)");
    XStoreName(display, xd->window, t);
    XSync(display, 0);
}

static void X11_Rect(double x0, double y0, double x1, double y1,
		     const pGEcontext gc, pDevDesc dd)
{
    double tmp;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

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
    CheckAlpha(gc->fill, xd);
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, xd);
	XFillRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, xd);
	SetLinetype(gc, xd);
	XDrawRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
		       (int)x1 - (int)x0, (int)y1 - (int)y0);
    }
}

static void X11_Path(double *x, double *y,
                     int npoly, int *nper,
                     Rboolean winding,
                     const pGEcontext gc, pDevDesc dd)
{
    warning(_("%s not available for this device"), "Path drawing");
}

static unsigned int makeX11Pixel(unsigned int * rasterImage, int pixel) {
    return GetX11Pixel(R_RED(rasterImage[pixel]), 
                       R_GREEN(rasterImage[pixel]), 
                       R_BLUE(rasterImage[pixel]));
}

static void flipRaster(unsigned int *rasterImage,
                       int imageWidth, int imageHeight,
                       int invertX, int invertY,
                       unsigned int *flippedRaster) {
    int i, j;
    int rowInc, rowOff, colInc, colOff;

    if (invertX) {
        colInc = -1;
        colOff = imageWidth - 1;
    } else {
        colInc = 1;
        colOff = 0;
    }
    if (invertY) {
        rowInc = -1;
        rowOff = imageHeight - 1;
    } else {
        rowInc = 1;
        rowOff = 0;
    }

    for (i = 0; i < imageHeight ;i++) {
        for (j = 0; j < imageWidth; j++) {
            int row = (rowInc*i + rowOff);
            int col = (colInc*j + colOff);
            flippedRaster[i*imageWidth + j] = 
                rasterImage[row*imageWidth + col];
        }
    }
}

static void X11_Raster(unsigned int *raster, int w, int h,
                      double x, double y, 
                      double width, double height,
                      double rot, 
                      Rboolean interpolate,
                      const pGEcontext gc, pDevDesc dd)
{
    int i, j, pixel;
    int imageWidth;
    int imageHeight;
    int invertX = 0;
    int invertY = 0;
    double angle = rot*M_PI/180;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    XImage *image;
    unsigned int *rasterImage;
    const void *vmax = vmaxget();
   
    if (height < 0) {
        imageHeight = (int) -(height - .5);
        /* convert (x, y) from bottom-left to top-left */
        y = y - imageHeight*cos(angle);
        if (angle != 0) {
            x = x - imageHeight*sin(angle);
        }
    } else {
        imageHeight = (int) (height + .5);
        invertY = 1;
    }

    if (width < 0) {
        imageWidth = (int) -(width - .5);
        x = x - imageWidth*cos(angle);
        if (angle != 0)
            y = y - imageWidth*sin(angle);
        invertX = 1;
    } else {
        imageWidth = (int) (width + .5);
    }

    rasterImage = (unsigned int *) R_alloc(imageWidth * imageHeight,
                                           sizeof(unsigned int));
    if (interpolate) {
        R_GE_rasterInterpolate(raster, w, h, 
                               rasterImage, imageWidth, imageHeight);
    } else {
        R_GE_rasterScale(raster, w, h, 
                         rasterImage, imageWidth, imageHeight);
    }
    
    if (rot != 0) {
        
        int newW, newH;
        double xoff, yoff;
        unsigned int *resizedRaster, *rotatedRaster;

        R_GE_rasterRotatedSize(imageWidth, imageHeight, angle, &newW, &newH);
        R_GE_rasterRotatedOffset(imageWidth, imageHeight, angle, 0,
                                 &xoff, &yoff);

        resizedRaster = (unsigned int *) R_alloc(newW * newH, 
                                             sizeof(unsigned int));
        R_GE_rasterResizeForRotation(rasterImage, imageWidth, imageHeight, 
                                     resizedRaster, newW, newH, gc);

        rotatedRaster = (unsigned int *) R_alloc(newW * newH, 
                                                 sizeof(unsigned int));
        R_GE_rasterRotate(resizedRaster, newW, newH, angle, rotatedRaster, gc,
                          FALSE);                          
            
        /* 
         * Adjust (x, y) for resized and rotated image
         */
        x = x - (newW - imageWidth)/2 - xoff;
        y = y - (newH - imageHeight)/2 + yoff;        

        rasterImage = rotatedRaster;
        imageWidth = newW;
        imageHeight = newH;
    }

    if (invertX || invertY) {
        unsigned int *flippedRaster;

        flippedRaster = (unsigned int *) R_alloc(imageWidth * imageHeight,
                                                 sizeof(unsigned int));
        flipRaster(rasterImage, imageWidth, imageHeight, 
                   invertX, invertY, flippedRaster);
        rasterImage = flippedRaster;
    }

    image = XCreateImage(display, visual, depth,
                         ZPixmap,
                         0, /* offset */
                         /* This is just provides (at least enough)
                          * allocated memory for the image data;  
                          * each pixel is set separately below
                          */
                         (char *) rasterImage,
                         imageWidth, imageHeight,
                         depth >= 24 ? 32 : 16, /* bitmap_pad */
                         0); /* bytes_per_line: 0 means auto-calculate*/

    if (image == NULL || XInitImage(image) == 0)
        error(_("Unable to create XImage"));

    for (i = 0; i < imageHeight ;i++) {
        for (j = 0; j < imageWidth; j++) {
            pixel = i*imageWidth + j;
            XPutPixel(image, j, i, makeX11Pixel(rasterImage, pixel));
        }
    }

    XPutImage(display, xd->window, xd->wgc, 
              image, 0, 0,
              (int) x, (int) y, imageWidth, imageHeight);

    /* XFree() rather than XDestroyImage() because the latter
     * tries to free the image 'data' as well
     */
    XFree(image);

    vmaxset(vmax);
}

static SEXP X11_Cap(pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    XImage *image = XGetImage(display, xd->window, 0, 0,
                              xd->windowWidth, xd->windowHeight, 
                              AllPlanes, ZPixmap);
    SEXP raster = R_NilValue;

    if (image) {
        int i, j;
        SEXP dim;
        int size = xd->windowWidth * xd->windowHeight;
        const void *vmax = vmaxget();
        unsigned int *rint;

        PROTECT(raster = allocVector(INTSXP, size));
        
        /* Copy each byte of screen to an R matrix. 
         * The ARGB32 needs to be converted to an R ABGR32 */
        rint = (unsigned int *) INTEGER(raster);
        for (i = 0; i < xd->windowHeight; i++) {
            for (j = 0; j < xd->windowWidth; j++) {
                /* 
                 * Convert each pixel in image to an R colour
                 */
                rint[i*xd->windowWidth + j] = bitgp((void *) image, i, j);
            }
        }
        PROTECT(dim = allocVector(INTSXP, 2));
        INTEGER(dim)[0] = xd->windowHeight;
        INTEGER(dim)[1] = xd->windowWidth;
        setAttrib(raster, R_DimSymbol, dim);
    
        UNPROTECT(2);

        XDestroyImage(image);
        vmaxset(vmax);
    }

    return raster;
}

static void X11_Circle(double x, double y, double r,
		       const pGEcontext gc, pDevDesc dd)
{
    int ir, ix, iy;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    ir = (int)floor(r + 0.5);

    ix = (int)x;
    iy = (int)y;
    CheckAlpha(gc->fill, xd);
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, xd);
	XFillArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetLinetype(gc, xd);
	SetColor(gc->col, xd);
	XDrawArc(display, xd->window, xd->wgc,
		 ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
    }
}

static void X11_Line(double x1, double y1, double x2, double y2,
		     const pGEcontext gc, pDevDesc dd)
{
    int xx1, yy1, xx2, yy2;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    /* In-place conversion ok */

    xx1 = (int) x1;
    yy1 = (int) y1;
    xx2 = (int) x2;
    yy2 = (int) y2;

    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, xd);
	SetLinetype(gc, xd);
	XDrawLine(display, xd->window, xd->wgc, xx1, yy1, xx2, yy2);
    }
}

static void X11_Polyline(int n, double *x, double *y,
			 const pGEcontext gc, pDevDesc dd)
{
    const void *vmax = vmaxget();
    XPoint *points;
    int i, j;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    points = (XPoint *) R_alloc(n, sizeof(XPoint));

    for(i = 0 ; i < n ; i++) {
	points[i].x = (short)(x[i]);
	points[i].y = (short)(y[i]);
    }

    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, xd);
	SetLinetype(gc, xd);
/* Some X servers need npoints < 64K */
	for(i = 0; i < n; i+= 10000-1) {
	    j = n - i;
	    j = (j <= 10000) ? j: 10000; /* allow for overlap */
	    XDrawLines(display, xd->window, xd->wgc, points+i, j,
		       CoordModeOrigin);
	}
    }

    vmaxset(vmax);
}

static void X11_Polygon(int n, double *x, double *y,
			const pGEcontext gc, pDevDesc dd)
{
    const void *vmax = vmaxget();
    XPoint *points;
    int i;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    points = (XPoint *) R_alloc(n+1, sizeof(XPoint));

    for (i = 0 ; i < n ; i++) {
	points[i].x = (short)(x[i]);
	points[i].y = (short)(y[i]);
    }
    points[n].x = (short)(x[0]);
    points[n].y = (short)(y[0]);
    CheckAlpha(gc->fill, xd);
    if (R_OPAQUE(gc->fill)) {
	SetColor(gc->fill, xd);
	XFillPolygon(display, xd->window, xd->wgc, points, n,
		     Complex, CoordModeOrigin);
    }
    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, xd);
	SetLinetype(gc, xd);
	XDrawLines(display, xd->window, xd->wgc, points, n+1, CoordModeOrigin);
    }

    vmaxset(vmax);
}


static void X11_Text(double x, double y,
		     const char *str, double rot, double hadj,
		     const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    SetFont(gc, xd);
    CheckAlpha(gc->col, xd);
    if (R_OPAQUE(gc->col)) {
	SetColor(gc->col, xd);
	XRfRotDrawString(display, xd->font, rot, xd->window,
			 xd->wgc, (int)x, (int)y, str);
    }
}

static Rboolean X11_Locator(double *x, double *y, pDevDesc dd)
{
    XEvent event;
    pDevDesc ddEvent;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    caddr_t temp;
    int done = 0;

    if (xd->type > WINDOW) return 0;
#ifdef HAVE_WORKING_CAIRO
    if (xd->holdlevel > 0)
	error(_("attempt to use the locator after dev.hold()"));
    if (xd->buffered) Cairo_update(xd);
#endif
    R_ProcessX11Events((void*)NULL);	/* discard pending events */
    if(xd->type==WINDOW) XDefineCursor(display, xd->window, cross_cursor);
    XSync(display, 1);
    /* handle X events as normal until get a button */
    /* click in the desired device */
    while (!done && displayOpen) {
	XNextEvent(display, &event);
	/* possibly later R_CheckUserInterrupt(); */
	if (event.type == ButtonPress) {
	    XFindContext(display, event.xbutton.window,
			 devPtrContext, &temp);
	    ddEvent = (pDevDesc) temp;
	    if (ddEvent == dd) {
		if (event.xbutton.button == Button1) {
		    int useBeep = asLogical(GetOption1(install("locatorBell")));
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
    /* In case it got closed asynchronously, PR#14872 */
    if (!displayOpen) return 0;
    /* if it was a Button1 succeed, otherwise fail */
    if(xd->type==WINDOW) XDefineCursor(display, xd->window, arrow_cursor);
    XSync(display, 0);
    return (done == 1);
}

static int translate_key(KeySym keysym)
{
    if ((keysym >= XK_F1) && (keysym <= XK_F12))
    	return knF1 + (int)keysym - XK_F1;
    else {
    	switch(keysym) {
	case XK_Left: return knLEFT;
	case XK_Up:   return knUP;
	case XK_Right:return knRIGHT;
	case XK_Down: return knDOWN;
	case XK_Page_Up: 	return knPGUP;
	case XK_Page_Down: 	return knPGDN;
	case XK_End:  return knEND;
	case XK_Begin:return knHOME;
	case XK_Insert:  	return knINS;
	}
    }
    return knUNKNOWN;
}

static void X11_eventHelper(pDevDesc dd, int code)
{
    XEvent event;
    pDevDesc ddEvent;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    caddr_t temp;
    int done = 0;

    if (xd->type > WINDOW) return;
    if (code == 1) {
    	R_ProcessX11Events((void*)NULL);	/* discard pending events */
    	if (isEnvironment(dd->eventEnv)) {
    	    SEXP prompt = findVar(install("prompt"), dd->eventEnv);
    	    if (isString(prompt) && length(prompt) == 1) {
    		 PROTECT(prompt);
    		 XStoreName(display, xd->window, CHAR(asChar(prompt)));
    		 UNPROTECT(1);
    	    } else 
    	    	XStoreName(display, xd->window, "");
    	}
    	XSync(display, 1);
    } else if (code == 2) {
	XNextEvent(display, &event);
	if (event.type == ButtonRelease || event.type == ButtonPress || event.type == MotionNotify) {
	    XFindContext(display, event.xbutton.window,
			 devPtrContext, &temp);
	    ddEvent = (pDevDesc) temp;
	    if (ddEvent == dd && dd->gettingEvent) {
		if (event.type == MotionNotify) { /* Because of PointerMotionHintMask, need to update */
		    Window root, child;
		    int rootX, rootY, winX, winY;
		    unsigned int mask;
		    if (!XQueryPointer(display, event.xbutton.window,
                                      &root, &child, &rootX, &rootY,
				      &winX, &winY, &mask)) {
			done = 1;
		    } else {
			event.xbutton.x = winX;
			event.xbutton.y = winY;
		    }
		}
		if (!done) {
        	    doMouseEvent(dd, event.type == ButtonRelease ? meMouseUp :
        	                 event.type == ButtonPress ? meMouseDown : meMouseMove, 
        	                 event.xbutton.button, event.xbutton.x, event.xbutton.y);
                    XSync(display, 0);
                    done = 1;
		}
    	    }
	} else if (event.type == KeyPress) {
	    char keybuffer[13] = "";
	    char *keystart=keybuffer;
	    XComposeStatus compose;
  	    KeySym keysym;
	    int keycode;
	    if (event.xkey.state & ControlMask) {
	    	keystart += 5; 
	    	sprintf(keybuffer, "ctrl-"); /* report control keys using labels like "ctrl-A" */
	    	event.xkey.state &= !ControlMask;
	    	event.xkey.state |= ShiftMask;
	    }
      	    XLookupString(&event.xkey, keystart, 
			  sizeof(keybuffer)-(int)(keystart-keybuffer), 
			  &keysym, &compose);
      	    /* Rprintf("keysym=%x\n", keysym); */
      	    if ((keycode = translate_key(keysym)) > knUNKNOWN)
      	    	doKeybd(dd, keycode, NULL);
      	    else if (*keystart)
	    	doKeybd(dd, knUNKNOWN, keybuffer);
	    done = 1;
	}
	if (!done) 
	    handleEvent(event);
    } else if (code == 0) {
	/* Restore the default title */
	if (ndevNumber(dd) == curDevice())
	    X11_Activate(dd);
	else
	    X11_Deactivate(dd);  
    }

    return;
}

	/********************************************************/
	/* device_Mode is called whenever the graphics engine	*/
	/* starts drawing (mode=1) or stops drawing (mode=0)	*/
	/* the device is not required to do anything		*/
	/********************************************************/

static void X11_Mode(int mode, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    if(xd->holdlevel > 0) {
#ifdef HAVE_WORKING_CAIRO
	if(mode == 0 && xd->buffered > 1)
	    xd->last_activity = currentTime();
#endif
	return;
    }
    if(mode == 1) {
	if(xd->type==WINDOW) XDefineCursor(display, xd->window, watch_cursor);
	XSync(display, 0);
    }
    if(mode == 0) {
#ifdef HAVE_WORKING_CAIRO
	if(xd->buffered > 1) {
	    xd->last_activity = currentTime();
	    if((currentTime() - xd->last) > 0.5 /* 5*xd->update_interval */)
		Cairo_update(xd);
	    return;
	}
	if(xd->buffered) {
	    cairo_paint(xd->xcc);
	    cairo_surface_flush(xd->xcs);
	}
	
#endif
	if(xd->type==WINDOW) XDefineCursor(display, xd->window, arrow_cursor);
	XSync(display, 0);
    }
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

Rboolean X11DeviceDriver(pDevDesc dd,
			 const char *disp_name,
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
			 int xpos, int ypos,
			 const char *title,
			 int useCairo,
			 int antialias,
			 const char *family)
{
    pX11Desc xd;
    const char *fn;

    xd = Rf_allocX11DeviceDesc(pointsize);
    if(!xd) return FALSE;
    xd->bg = bgcolor;
#ifdef HAVE_WORKING_CAIRO
    xd->useCairo = useCairo != 0;
    xd->buffered = 0;
    switch(useCairo) {
    case 0: break; /* Xlib */
    case 1: xd->buffered = 1; break; /* cairo */
    case 2: xd->buffered = 0; break; /* nbcairo */
    case 3: xd->buffered = 2; break; /* dbcairo */
    default:
	warning("that type is not supported on this platform - using \"nbcairo\"");
	xd->buffered = 0;
    }
    if(useCairo) {
	switch(antialias){
	case 1: xd->antialias = CAIRO_ANTIALIAS_DEFAULT; break;
	case 2: xd->antialias = CAIRO_ANTIALIAS_NONE; break;
	case 3: xd->antialias = CAIRO_ANTIALIAS_GRAY; break;
	case 4: xd->antialias = CAIRO_ANTIALIAS_SUBPIXEL; break;
	}
    }
#else
    /* Currently this gets caught at R level */
    if(useCairo) {
	warning("cairo-based types are not supported on this build - using \"Xlib\"");
	useCairo = FALSE;
    }
#endif

    if(!useCairo) {
	if(strlen(fn = CHAR(STRING_ELT(sfonts, 0))) > 499) {
	    strcpy(xd->basefontfamily, fontname);
	    strcpy(xd->fontfamily, fontname);
	} else {
	    strcpy(xd->basefontfamily, fn);
	    strcpy(xd->fontfamily, fn);
	}
	if(strlen(fn = CHAR(STRING_ELT(sfonts, 1))) > 499)
	    strcpy(xd->symbolfamily, symbolname);
	else strcpy(xd->symbolfamily, fn);
    } else strcpy(xd->basefontfamily, family);

    /*	Start the Device Driver and Hardcopy.  */

    strncpy(xd->title, title, 100);
    xd->title[100] = '\0';

#ifdef HAVE_WORKING_CAIRO
    {
	SEXP timeouts = GetOption1(install("X11updates"));
	double tm = asReal(timeouts);
	xd->update_interval = (ISNAN(tm) || tm < 0) ? 0.10 : tm;
    }
#endif

    if (!X11_Open(dd, xd, disp_name, width, height,
		  gamma_fac, colormodel, maxcube, bgcolor,
		  canvascolor, res, xpos, ypos)) {
	free(xd);
	return FALSE;
    }

    Rf_setX11DeviceData(dd, gamma_fac, xd);
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
Rf_setX11DeviceData(pDevDesc dd, double gamma_fac, pX11Desc xd)
{
    double ps = xd->pointsize;
    int res0 = (xd->res_dpi > 0) ? xd->res_dpi : 72;
    /*	Set up Data Structures. */

#ifdef HAVE_WORKING_CAIRO
    if(xd->useCairo) {
	dd->newPage = Cairo_NewPage;
	dd->clip = Cairo_Clip;
	dd->rect = Cairo_Rect;
	dd->circle = Cairo_Circle;
	dd->line = Cairo_Line;
	dd->polyline = Cairo_Polyline;
	dd->polygon = Cairo_Polygon;
        dd->path = Cairo_Path;
        dd->raster = Cairo_Raster;
        dd->cap = Cairo_Cap;
	dd->hasTextUTF8 = TRUE;
	dd->wantSymbolUTF8 = TRUE;
#ifdef HAVE_PANGOCAIRO
	dd->metricInfo = PangoCairo_MetricInfo;
	dd->strWidth = dd->strWidthUTF8 = PangoCairo_StrWidth;
	dd->text = dd->textUTF8 = PangoCairo_Text;
#else
	dd->metricInfo = Cairo_MetricInfo;
	dd->strWidth = dd->strWidthUTF8 = Cairo_StrWidth;
	dd->text = dd->textUTF8 = Cairo_Text;
#endif
	dd->holdflush = Cairo_holdflush;
	dd->haveTransparency = 2;
	dd->haveTransparentBg = 3;
	dd->haveRaster = 2;
	dd->haveCapture = (xd->type > WINDOW) ? 1 : 2;
	dd->haveLocator = (xd->type > WINDOW) ? 1 : 2;
    } else
#endif
    {
	dd->newPage = X11_NewPage;
	dd->clip = X11_Clip;
	dd->strWidth = X11_StrWidth;
	dd->text = X11_Text;
	dd->rect = X11_Rect;
        dd->path = X11_Path;
        dd->raster     = X11_Raster;
        dd->cap        = X11_Cap;
	dd->circle = X11_Circle;
	dd->line = X11_Line;
	dd->polyline = X11_Polyline;
	dd->polygon = X11_Polygon;
	dd->metricInfo = X11_MetricInfo;
	dd->hasTextUTF8 = FALSE;
    	dd->eventHelper = X11_eventHelper;
    	dd->canGenMouseDown = TRUE;
	dd->canGenMouseUp = TRUE;
	dd->canGenMouseMove = TRUE;
	dd->canGenKeybd = TRUE;

	dd->haveTransparency = 1;
	dd->haveTransparentBg = 2;
	dd->haveRaster = 3;
	dd->haveCapture = (xd->type > WINDOW) ? 1 : 2;
	dd->haveLocator = (xd->type > WINDOW) ? 1 : 2;
    }

    dd->activate = X11_Activate;
    dd->close = X11_Close;
    dd->deactivate = X11_Deactivate;
    dd->size = X11_Size;
    dd->locator = X11_Locator;
    dd->mode = X11_Mode;
    dd->useRotatedTextInContour = FALSE;

    /* Set required graphics parameters. */

    /* Window Dimensions in Pixels */
    /* Initialise the clipping rect too */

    dd->left = dd->clipLeft = 0;			/* left */
    dd->right = dd->clipRight = xd->windowWidth;	/* right */
    dd->bottom = dd->clipBottom = xd->windowHeight;	/* bottom */
    dd->top = dd->clipTop = 0;			/* top */

    /* Nominal Character Sizes in Pixels */
    /* Recommendation from 'R internals': changed for 2.7.0 */
    /* Inches per raster unit */

    /* ps is in points, we want this in device units */
    if(xd->type == PNG || xd->type == JPEG ||
       xd->type == BMP || xd->type == TIFF) {
	dd->cra[0] = 0.9*ps * res0/72.0;
	dd->cra[1] = 1.2*ps * res0/72.0;
	dd->ipr[0] =  dd->ipr[1] = 1.0/res0;
	xd->lwdscale = res0/96.0;
    } else if(xd->type >= SVG) { /* SVG, PDF, PS -- unused */
	/* Device units are bp */
	dd->cra[0] = 0.9*ps;
	dd->cra[1] = 1.2*ps;
	dd->ipr[0] =  dd->ipr[1] = 1.0/72.0;
	xd->lwdscale = 1.0/96.0;
    } else {
	dd->cra[0] = 0.9*ps * 1.0/(72.0*pixelWidth());
	dd->cra[1] = 1.2*ps * 1.0/(72.0*pixelHeight());
	dd->ipr[0] = pixelWidth();
	dd->ipr[1] = pixelHeight();
	xd->lwdscale = 1.0/(96.0*pixelWidth());
#ifdef HAVE_WORKING_CAIRO
	if(xd->useCairo) {
# ifdef HAVE_PANGOCAIRO
	    /* Pango's default resolution is 96 dpi */
	    ps *= 1.0/(96.0*pixelWidth());
# else
	    /* Cairo's default resolution is 72 dpi */
	    ps *= 1.0/(72.0*pixelWidth());
# endif
	}
#endif
    }

    /* Character Addressing Offsets */
    /* These are used to plot a single plotting character */
    /* so that it is exactly over the plotting point */

    dd->xCharOffset = 0.4900;
    dd->yCharOffset = 0.3333;
    dd->yLineBias = 0.2;


    /* Device capabilities */

    dd->canClip = TRUE;
#ifdef HAVE_WORKING_CAIRO
    dd->canHAdj = xd->useCairo ? 2 : 0;
#else
    dd->canHAdj = 0;
#endif
    dd->canChangeGamma = FALSE;

    dd->startps = ps;
    xd->fontscale = 1.0;
    dd->startcol = xd->col;
    dd->startfill = xd->fill;
    dd->startlty = LTY_SOLID;
    dd->startfont = 1;
    dd->startgamma = gamma_fac;

    /* initialise x11 device description */
    /* (most of the work has been done in X11_Open) */
    xd->resize = 0;

    dd->deviceSpecific = (void *) xd;

    dd->displayListOn = TRUE;

    return TRUE;
}


/**
 This allocates an X11Desc instance  and sets its default values.
 */
pX11Desc Rf_allocX11DeviceDesc(double ps)
{
    pX11Desc xd;
    /* allocate new device description */
    if (!(xd = (pX11Desc)calloc(1, sizeof(X11Desc))))
	return NULL;

    /* From here on, if we need to bail out with "error", */
    /* then we must also free(xd). */

    /*	Font will load at first use.  */

    if (ps < 6 || ps > 24) ps = 12;
    xd->fontface = -1;
    xd->fontsize = -1;
    xd->pointsize = ps;
    xd->handleOwnEvents = FALSE;
    xd->window = (Window) NULL;

    return xd;
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
	pX11Desc xd = GEgetDevice(d)->dev->deviceSpecific;

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
Rf_getX11Display(void)
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

/* Note: this sets a global gamma, not just for the current device */
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

typedef Rboolean (*X11DeviceDriverRoutine)(pDevDesc, char*,
					   double, double, double, double,
					   X_COLORTYPE, int, int);

static void
Rf_addX11Device(const char *display, double width, double height, double ps,
		double gamma, int colormodel, int maxcubesize,
		int bgcolor, int canvascolor, const char *devname, SEXP sfonts,
		int res, int xpos, int ypos, const char *title,
		int useCairo, int antialias, const char * family, SEXP call)
{
    pDevDesc dev = NULL;
    pGEDevDesc dd;

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (pDevDesc) calloc(1, sizeof(DevDesc)))) return;
	if (!X11DeviceDriver(dev, display, width, height,
			     ps, gamma, colormodel, maxcubesize,
			     bgcolor, canvascolor, sfonts, res,
			     xpos, ypos, title, useCairo, antialias, family)) {
	    free(dev);
	    errorcall(call, _("unable to start device %s"), devname);
	}
	dd = GEcreateDevDesc(dev);
	GEaddDevice2(dd, devname);
    } END_SUSPEND_INTERRUPTS;
}

static SEXP in_do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *display, *cname, *devname, *title, *family;
    const void *vmax;
    double height, width, ps, gamma;
    int colormodel, maxcubesize, bgcolor, canvascolor, res, xpos, ypos,
	useCairo, antialias;
    SEXP sc, sfonts;

    checkArity(op, args);
    vmax = vmaxget();

    if(R_isForkedChild)
	error("a forked child should not open a graphics device");

    /* Decode the arguments */
    display = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
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
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) || LENGTH(sc) != 1)
	errorcall(call, _("invalid '%s' value"), "title");
    title = CHAR(STRING_ELT(sc, 0));
    args = CDR(args);
    useCairo = asInteger(CAR(args));
    if (useCairo == NA_INTEGER)
	errorcall(call, _("invalid '%s' value"), "type");
    args = CDR(args);
    antialias = asInteger(CAR(args));
    if (antialias == NA_INTEGER)
	errorcall(call, _("invalid '%s' value"), "antialias");
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) || LENGTH(sc) != 1)
	errorcall(call, _("invalid '%s' value"), "family");
    family = CHAR(STRING_ELT(sc, 0));


    if (!strncmp(display, "png::", 5)) devname = "PNG";
    else if (!strncmp(display, "jpeg::", 6)) devname = "JPEG";
    else if (!strncmp(display, "tiff::", 6)) devname = "TIFF";
    else if (!strncmp(display, "bmp::", 5)) devname = "BMP";
    else if (!strcmp(display, "XImage")) devname = "XImage";
    else if (useCairo) devname = "X11cairo";
    else devname = "X11";

    Rf_addX11Device(display, width, height, ps, gamma, colormodel,
		    maxcubesize, bgcolor, canvascolor, devname, sfonts,
		    res, xpos, ypos, title, useCairo, antialias, family, call);
    vmaxset(vmax);
    return R_NilValue;
}


#ifdef HAVE_WORKING_CAIRO
static int stride;
static unsigned int Sbitgp(void *xi, int x, int y)
{
    unsigned int *data = xi;
    return data[x*stride+y] | 0xFF000000; /* force opaque */
}


/* savePlot(filename, type, device) */
static SEXP in_do_saveplot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int devNr;
    const char *fn, *type;
    pGEDevDesc gdd;
    pX11Desc xd;

    checkArity(op, args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) < 1)
	error(_("invalid '%s' argument"), "filename");
    fn = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (!isString(CADR(args)) || LENGTH(CADR(args)) < 1)
	error(_("invalid '%s' argument"), "type");
    type = CHAR(STRING_ELT(CADR(args), 0));
    devNr = asInteger(CADDR(args));
    if (devNr == NA_INTEGER) error(_("invalid '%s' argument"), "device");
    gdd = GEgetDevice(devNr - 1); /* 0-based */
    if (!gdd->dirty) error(_("no plot on device to save"));
    xd = gdd->dev->deviceSpecific;
    if (!xd->cs || !xd->useCairo) error(_("not an open X11cairo device"));
    if (streql(type, "png")) {
	cairo_status_t res = cairo_surface_write_to_png(xd->cs, fn);
	if (res != CAIRO_STATUS_SUCCESS)
	    error("cairo error '%s'", cairo_status_to_string(res));
    }
    else if (streql(type, "jpeg")) {
	void *xi = cairo_image_surface_get_data(xd->cs);
	FILE *fp = R_fopen(fn, "w");
	if (!fp) error(_("cannot open file '%s'"), fn);
	stride = xd->windowWidth;
	R_SaveAsJpeg(xi, xd->windowWidth, xd->windowHeight,
		     Sbitgp, 0, 75, fp, 0);
	fclose(fp);
    } else if (streql(type, "tiff")) {
	void *xi = cairo_image_surface_get_data(xd->cs);
	stride = xd->windowWidth;
	R_SaveAsTIFF(xi, xd->windowWidth, xd->windowHeight,
		     Sbitgp, 0, fn, 0, 1L);
    } else
	error(_("invalid '%s' argument"), "type");
    return R_NilValue;
}
#else
static SEXP in_do_saveplot(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error(_("savePlot() is not supported on this build"));
    return R_NilValue;
}
#endif


static int in_R_X11_access(void)
{
    char *p;
    X11IOhandler old;

    if (displayOpen) return TRUE;
    if(!(p = getenv("DISPLAY"))) return FALSE;
    /* Bill Dunlap sees an error when tunneling to a non-existent
       X11 connection that BDR cannot reproduce.  We leave a handler set
       if we get an error, but that is rare.
    */
    old = XSetIOErrorHandler(R_X11IOErrSimple);
    if ((display = XOpenDisplay(NULL)) == NULL) {
	XSetIOErrorHandler(old);
	return FALSE;
    } else {
	XCloseDisplay(display);
	XSetIOErrorHandler(old);
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
    ret = XGetWindowProperty(display, clpwin, pty, 0, 0, False, AnyPropertyType,
			     &pty_type, &pty_format, &pty_items, &pty_size,
			     &buffer);
    if (ret) {
	warning(_("clipboard cannot be opened or contains no text"));
	res = FALSE;
    } else {
	XFree(buffer);
	if (pty_format != 8) { /* bytes */
	    warning(_("clipboard cannot be opened or contains no text"));
	    res = FALSE;
	} else { /* read the property */
	    ret = XGetWindowProperty(display, clpwin, pty, 0, (long)pty_size, False,
				     AnyPropertyType, &pty_type, &pty_format,
				     &pty_items, &pty_size, &buffer);
	    if (ret) {
		warning(_("clipboard cannot be read (error code %d)"), ret);
		res = FALSE;
	    } else {
		this->buff = (char *)malloc(pty_items + 1);
		this->last = this->len = (int) pty_items;
		if(this->buff) {
		    /* property always ends in 'extra' zero byte */
		    memcpy(this->buff, buffer, pty_items + 1);
		} else {
		    warning(_("memory allocation to copy clipboard failed"));
		    res = FALSE;
		}
		XFree(buffer);
	    }
	}
    }
    
    XDeleteProperty(display, clpwin, pty);
    if (!displayOpen) {
	XCloseDisplay(display);
	strcpy(dspname, "");
    }
    return res;
}

#include <R_ext/Rdynload.h>

extern const char * in_R_pngVersion(void);
extern const char * in_R_jpegVersion(void);
extern const char * in_R_tiffVersion(void);

void R_init_R_X11(DllInfo *info)
{
    R_X11Routines *tmp;
    tmp = (R_X11Routines*) malloc(sizeof(R_X11Routines));
    if(!tmp) {
	error(_("cannot allocate memory for X11Routines structure"));
	return;
    }
    tmp->X11 = in_do_X11;
    tmp->saveplot = in_do_saveplot;
    tmp->image = in_R_GetX11Image;
    tmp->access = in_R_X11_access;
    tmp->readclp = in_R_X11readclp;
    tmp->R_pngVersion = in_R_pngVersion;
    tmp->R_jpegVersion = in_R_jpegVersion;
    tmp->R_tiffVersion = in_R_tiffVersion;
    R_setX11Routines(tmp);
}
