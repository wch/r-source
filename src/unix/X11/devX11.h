#ifndef _DEV_X11_H
#define _DEV_X11_H

/* These are the currently supported device "models" */
typedef enum {
    MONOCHROME = 0,
    GRAYSCALE,
    PSEUDOCOLOR1,
    PSEUDOCOLOR2,
    TRUECOLOR
} X_COLORTYPE;

typedef enum {
    WINDOW, /* NB: have "type > WINDOW" below ... */
    PNG,
    JPEG,
    XIMAGE
} X_GTYPE;


/*
  For the moment, we just conditionally activate the remainder of this
  section iff we are in devX11.c which defines R_X11_DEVICE.
  This allows devUI.h to include this file to get X_COLORTYPE.
  However, that should probably not be happening if HAVE_X11 is not defined
  due to the configuration being done --without-x. Why is unix/devices.c 
  not getting compiled if no X11 support is available? DTL.
 */
#if R_X11_DEVICE

#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>  /*->	Xlib.h	Xutil.h Xresource.h .. */



Rboolean X11DeviceDriver(DevDesc*, char*, double, double, double, double, 
			 X_COLORTYPE, int);


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

    Rboolean handleOwnEvents;           /* Flag indicating whether events will be handled externally from R (TRUE),
                                           or whether R is to handle the events (FALSE) */
} x11Desc;



x11Desc *Rf_allocX11DeviceDesc(double ps);
int      Rf_setX11Display(Display *dpy, double gamma_fac, X_COLORTYPE colormodel, int maxcube, Rboolean setHandlers);
int      Rf_setX11DeviceData(DevDesc *dd, x11Desc *xd);
Rboolean X11_Open(DevDesc *dd, x11Desc *xd, char *dsp, double w, double h, double gamma_fac, X_COLORTYPE colormodel, int maxcube);
Display* Rf_getX11Display();

#endif /* R_X11_DEVICE */

#endif

