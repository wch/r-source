/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
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


#define R_USE_PROTOTYPES 1
#include <R_ext/GraphicsEngine.h>
#include "Fileio.h"		/* R_fopen */
#include "rotated.h"		/* 'Public' routines from here */
/* For the input handlers of the event loop mechanism: */
#include <R_ext/eventloop.h>
#include <R_ext/Memory.h>	/* vmaxget */


#ifdef HAVE_WORKING_CAIRO
#include <pango/pango.h>
#include <pango/pangocairo.h>
#include <cairo-xlib.h>
#endif


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
static XSetWindowAttributes attributes;		/* Window attributes */
static Colormap colormap;			/* Default color map */
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
		       pGEcontext gc, pDevDesc dd);
static void X11_Clip(double x0, double x1, double y0, double y1,
		     pDevDesc dd);
static void X11_Close(pDevDesc dd);
static void X11_Deactivate(pDevDesc dd);
static Rboolean X11_Locator(double *x, double *y, pDevDesc dd);
static void X11_Line(double x1, double y1, double x2, double y2,
		     pGEcontext gc, pDevDesc dd);
static void X11_MetricInfo(int c, pGEcontext gc,
			   double* ascent, double* descent,
			   double* width, pDevDesc dd);
static void X11_Mode(int mode, pDevDesc dd);
static void X11_NewPage(pGEcontext gc, pDevDesc dd);
static void X11_Polygon(int n, double *x, double *y,
			pGEcontext gc, pDevDesc dd);
static void X11_Polyline(int n, double *x, double *y,
			 pGEcontext gc, pDevDesc dd);
static void X11_Rect(double x0, double y0, double x1, double y1,
		     pGEcontext gc, pDevDesc dd);
static void X11_Size(double *left, double *right,
		     double *bottom, double *top,
		     pDevDesc dd);
static double X11_StrWidth(const char *str, pGEcontext gc, pDevDesc dd);
static void X11_Text(double x, double y, const char *str,
		     double rot, double hadj,
		     pGEcontext gc, pDevDesc dd);

	/*************************************************/
	/* End of list of required device driver actions */
	/*************************************************/

	/* Support Routines */

static double pixelHeight(void);
static double pixelWidth(void);
static void SetColor(unsigned int, pDevDesc);
static void SetLinetype(R_GE_gcontext*, pX11Desc);



	/************************/
	/* X11 Color Management */
	/************************/

static double RedGamma	 = 0.6;
static double GreenGamma = 0.6;
static double BlueGamma	 = 0.6;

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
    pDevDesc dd = NULL;	/* -Wall */
    pX11Desc xd;
    int devNum = 0;
    int do_update = 0;

    if (event.xany.type == Expose) {
	while(XCheckTypedEvent(display, Expose, &event))
	    ;
	XFindContext(display, event.xexpose.window,
		     devPtrContext, &temp);
	dd = (pDevDesc) temp;
	if (event.xexpose.count == 0)
	    do_update = 1;
    }
    else if (event.type == ConfigureNotify) {
	while(XCheckTypedEvent(display, ConfigureNotify, &event))
	    ;
	XFindContext(display, event.xconfigure.window,
		     devPtrContext, &temp);
	dd = (pDevDesc) temp;
	xd = (pX11Desc) dd->deviceSpecific;
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
	    dd = (pDevDesc) temp;
	    killDevice(ndevNumber(dd));
	}

    if (do_update) {
	/* It appears possible that a device may receive an expose
	 * event in the middle of the device being "kill"ed by R
	 * This means that R knows nothing about the device
	 * so devNumber becomes 0 (the null device) and it was not
	 * a good idea to pass the null device to GEplayDisplayList
	 * -- although GEplayDisplayList now checks this.
	 */
	devNum = ndevNumber(dd);
	if (devNum > 0) {
	    pGEDevDesc gdd = GEgetDevice(devNum);
	    dd = (pDevDesc) temp;
	    xd = (pX11Desc) dd->deviceSpecific;
	    if (xd->cs)
		cairo_xlib_surface_set_size(xd->cs, xd->windowWidth, 
					    xd->windowHeight);
	    /* avoid replying a device list until something has been drawn */
	    if(gdd->dirty) GEplayDisplayList(gdd);
	}
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

static void SetColor(unsigned int col, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    unsigned int alpha = R_ALPHA(col);
    double red, blue, green;

    red = R_RED(col)/255.0;
    green = R_GREEN(col)/255.0;
    blue = R_BLUE(col)/255.0;
    /* NB: this uses display gamma 0.6 for historical reasons,
       to match the previous X11 device */
    red = pow(red, RedGamma);
    green = pow(green, GreenGamma);
    blue = pow(blue, BlueGamma);
    
    if (alpha == 255) 
	cairo_set_source_rgb(xd->cc, red, green, blue); 
    else
	cairo_set_source_rgba(xd->cc, red, green, blue, alpha/255.0); 
}


static void SetLinetype(pGEcontext gc, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    R_GE_lineend lend = CAIRO_LINE_CAP_SQUARE;
    R_GE_linejoin ljoin = CAIRO_LINE_JOIN_ROUND;
    cairo_set_line_width(cc, gc->lwd);
    switch(gc->lend){
    case GE_ROUND_CAP: lend = CAIRO_LINE_CAP_ROUND; break;
    case GE_BUTT_CAP: lend = CAIRO_LINE_CAP_BUTT; break;
    case GE_SQUARE_CAP: lend = CAIRO_LINE_CAP_SQUARE; break;
    }
    cairo_set_line_cap(cc, lend);
    switch(gc->ljoin){
    case GE_ROUND_JOIN: ljoin = CAIRO_LINE_JOIN_ROUND; break;
    case GE_MITRE_JOIN: ljoin = CAIRO_LINE_JOIN_MITER; break;
    case GE_BEVEL_JOIN: ljoin = CAIRO_LINE_JOIN_BEVEL; break;
    } 
    cairo_set_line_join(cc, ljoin);

    if (gc->lty == 0 || gc->lty == -1)
	cairo_set_dash(cc, 0, 0, 0);
    else {
	double ls[16]; /* max 16x4=64 bit */
	int l = 0, dt = gc->lty;
	while (dt > 0) {
	    ls[l] = (double)(dt&15);
	    dt >>= 4;
	    l++;
	}
	cairo_set_dash(cc, ls, l, 0);
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
X11_Open(pDevDesc dd, pX11Desc xd, const char *dsp,
	    double w, double h, double gamma_fac, X_COLORTYPE colormodel,
	    int maxcube, int bgcolor, int canvascolor, int res,
	    int xpos, int ypos)
{
    /* if we have to bail out with "error", then must free(dd) and free(xd) */
    /* That means the *caller*: the X11DeviceDriver code frees xd, for example */

    XEvent event;
    int iw, ih, blackpixel, whitepixel;
    X_GTYPE type;
    const char *p = dsp;
    XGCValues gcv;
    /* Indicates whether the display is created within this particular call: */
    Rboolean DisplayOpened = FALSE;
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

    blackpixel = 0;
    {
	static unsigned int RMask, RShift;
	static unsigned int GMask, GShift;
	static unsigned int BMask, BShift;
	unsigned int r = R_RED(canvascolor), g = R_GREEN(canvascolor),
	    b = R_BLUE(canvascolor);
	r = pow((r / 255.0), RedGamma) * 255;
	g = pow((g / 255.0), GreenGamma) * 255;
	b = pow((b / 255.0), BlueGamma) * 255;
	RMask = visual->red_mask;
	GMask = visual->green_mask;
	BMask = visual->blue_mask;
	RShift = 0; while ((RMask & 1) == 0) { RShift++; RMask >>= 1; }
	GShift = 0; while ((GMask & 1) == 0) { GShift++; GMask >>= 1; }
	BShift = 0; while ((BMask & 1) == 0) { BShift++; BMask >>= 1; }
	whitepixel = (((r * RMask) / 255) << RShift) |
	    (((g * GMask) / 255) << GShift) |
	    (((b * BMask) / 255) << BShift);
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
		xtdpy = XtOpenDisplay(app_con, dsp, "r_x11", "R_x11",
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

	    XStoreName(display, xd->window, xd->title);

	    xd->gcursor = XCreateFontCursor(display, CURSOR);
	    XDefineCursor(display, xd->window, xd->gcursor);

	    /* set up protocols so that window manager sends */
	    /* me an event when user "destroys" window */
	    _XA_WM_PROTOCOLS = XInternAtom(display, "WM_PROTOCOLS", 0);
	    protocol = XInternAtom(display, "WM_DELETE_WINDOW", 0);
	    XSetWMProtocols(display, xd->window, &protocol, 1);
	    xd->cs = cairo_xlib_surface_create(display, xd->window, 
					       visual,
					       (double)xd->windowWidth,
					       (double)xd->windowHeight);
	    if (cairo_surface_status(xd->cs) != CAIRO_STATUS_SUCCESS) {
		/* bail out */
	    }
	    xd->cc = cairo_create(xd->cs);
	    if (cairo_status(xd->cc) != CAIRO_STATUS_SUCCESS) {
		/* bail out */
	    }
	    cairo_set_operator(xd->cc, CAIRO_OPERATOR_ATOP);
	    cairo_reset_clip(xd->cc);

	}
	/* Save the pDevDesc with the window for event dispatching */
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
	/* Save the pDevDesc with the window for event dispatching */
	/* Is this needed? */
	XSaveContext(display, xd->window, devPtrContext, (caddr_t) dd);
	xd->npages = 0;
    }

    /* Set the graphics context */

    gcv.arc_mode = ArcChord;
    xd->wgc = XCreateGC(display, xd->window, GCArcMode, &gcv);
    XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);

    xd->lty = -1;
    xd->lwd = -1;
    xd->lend = 0;
    xd->ljoin = 0;

    numX11Devices++;
    return TRUE;
}

static void X11_Clip(double x0, double x1, double y0, double y1,
			pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (x1 < x0) { double h = x1; x1 = x0; x0 = h; };
    if (y1 < y0) { double h = y1; y1 = y0; y0 = h; };

    cairo_reset_clip(xd->cc);
    cairo_new_path(xd->cc);
    cairo_rectangle(xd->cc, x0, y0, x1 - x0 + 1, y1 - y0 + 1);
    /* Add 1 per X11_Clip */
    cairo_clip(xd->cc);
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

static void X11_NewPage(pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    cairo_reset_clip(xd->cc);
    xd->fill = R_OPAQUE(gc->fill) ? gc->fill: PNG_TRANS;
SetColor(xd->fill, dd);
    cairo_new_path(xd->cc);
    cairo_paint(xd->cc);

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


static void X11_Close(pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (xd->type == WINDOW) {
	cairo_surface_destroy(xd->cs);
	cairo_destroy(xd->cc);

	/* process pending events */
	/* set block on destroy events */
	inclose = TRUE;
	R_ProcessX11Events((void*) NULL);

	XFreeCursor(display, xd->gcursor);
	XDestroyWindow(display, xd->window);
	XSync(display, 0);
#if 0
    } else {
	if (xd->npages && xd->type != XIMAGE) X11_Close_bitmap(xd);
	XFreeGC(display, xd->wgc);
	XFreePixmap(display, xd->window);
	if (xd->type != XIMAGE && xd->fp != NULL) fclose(xd->fp);
#endif
    }

    numX11Devices--;
    if (numX11Devices == 0)  {
      int fd = ConnectionNumber(display);
	/* Free Resources Here */
        if(xd->handleOwnEvents == FALSE)
	    removeInputHandler(&R_InputHandlers,
			       getInputHandler(R_InputHandlers,fd));
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
    if(strlen(xd->title)) {
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
    if(strlen(xd->title)) {
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
		     pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    cairo_new_path(xd->cc);
    cairo_rectangle(xd->cc, x0, y0, x1 - x0, y1 - y0);

    if (R_ALPHA(gc->fill) > 0) {
	SetColor(gc->fill, dd);
	cairo_fill_preserve(xd->cc);
    }

    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	SetColor(gc->col, dd);
	SetLinetype(gc, xd);
	cairo_stroke(xd->cc);
    } else cairo_new_path(xd->cc);

#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

static void X11_Circle(double x, double y, double r,
		       pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    cairo_new_path(xd->cc);
    cairo_arc(xd->cc, x, y, r + 0.5 , 0.0, 2 * M_PI);

    if (R_ALPHA(gc->fill) > 0) {
	SetColor(gc->fill, dd);
	cairo_fill_preserve(xd->cc);
    }
    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	SetLinetype(gc, xd);
	SetColor(gc->col, dd);
	cairo_stroke(xd->cc);
    } else cairo_new_path(xd->cc);
}

static void X11_Line(double x1, double y1, double x2, double y2,
		     pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (R_ALPHA(gc->col) > 0) { /* FIXME */
	cairo_new_path(xd->cc);
	cairo_move_to(xd->cc, x1, y1);
	cairo_line_to(xd->cc, x2, y2);
	SetColor(gc->col, dd);
	SetLinetype(gc, xd);
	cairo_stroke(xd->cc);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
}

static void X11_Polyline(int n, double *x, double *y,
			 pGEcontext gc, pDevDesc dd)
{
    int i;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (R_ALPHA(gc->col) > 0) {
	SetColor(gc->col, dd);
	SetLinetype(gc, xd);
	cairo_new_path(xd->cc);
	cairo_move_to(xd->cc, x[0], y[0]);
	for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
	cairo_stroke(xd->cc);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
    }
}

static void X11_Polygon(int n, double *x, double *y,
			pGEcontext gc, pDevDesc dd)
{
    int i;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    cairo_new_path(xd->cc);
    cairo_move_to(xd->cc, x[0], y[0]);
    for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
    cairo_close_path(xd->cc);

    if (R_ALPHA(gc->fill) > 0) {
	SetColor(gc->fill, dd);
	cairo_fill_preserve(xd->cc);
    }
    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	SetColor(gc->col, dd);
	SetLinetype(gc, xd);
	cairo_stroke(xd->cc);
    } else cairo_new_path(xd->cc);

#ifdef XSYNC
    if (xd->type == WINDOW) XSync(display, 0);
#endif
}

static PangoFontDescription *getFont(pDevDesc dd, pGEcontext gc)
{
    PangoFontDescription *fontdesc;
    gint size, face = gc->fontface;
    
    if (face < 1 || face > 5) face = 1;

    size = gc->cex * gc->ps + 0.5;
	
    fontdesc = pango_font_description_new();
    if (face == 5)
	pango_font_description_set_family(fontdesc, "symbol");
    else {
	char *fm = gc->fontfamily;
	pango_font_description_set_family(fontdesc, fm[0] ? fm : "helvetica");
	if(face == 2 || face == 4)
	    pango_font_description_set_weight(fontdesc, PANGO_WEIGHT_BOLD);
	if(face == 3 || face == 4)
	    pango_font_description_set_style(fontdesc, PANGO_STYLE_OBLIQUE);
    }
    pango_font_description_set_size(fontdesc, PANGO_SCALE * size);
	
    return fontdesc;
}

static PangoLayout 
*layoutText(PangoFontDescription *desc, cairo_t *cc, const char *str)
{
    PangoLayout *layout;
	
    layout = pango_cairo_create_layout(cc);
    pango_layout_set_font_description(layout, desc);
    pango_layout_set_text(layout, str, -1);
    return layout;
}

static void
text_extents(PangoFontDescription *desc, cairo_t *cc,
	     pGEcontext gc, const gchar *str,
	     gint *lbearing, gint *rbearing, 
	     gint *width, gint *ascent, gint *descent, int ink)
{
    PangoLayout *layout;
    PangoRectangle rect;
	
    layout = layoutText(desc, cc, str);
    if(ink) 
	pango_layout_line_get_pixel_extents(pango_layout_get_line(layout, 0),
					    &rect, NULL);
    else
	pango_layout_line_get_pixel_extents(pango_layout_get_line(layout, 0),
					    NULL, &rect);

    if(ascent) *ascent = PANGO_ASCENT(rect);
    if(descent) *descent = PANGO_DESCENT(rect);
    if(width) *width = rect.width;
    if(lbearing) *lbearing = PANGO_LBEARING(rect);
    if(rbearing) *rbearing = PANGO_RBEARING(rect);
    g_object_unref(layout);
}

static void X11_MetricInfo(int c, pGEcontext gc,
			   double* ascent, double* descent,
			   double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    char str[16];
    int Unicode = mbcslocale;
    PangoFontDescription *desc = getFont(dd, gc);
    gint iascent, idescent, iwidth;
	
    if(c == 0) c = 77;
    if(c < 0) {c = -c; Unicode = 1;}

    if(Unicode) {
	Rf_ucstoutf8(str, (unsigned int) c);
    } else {
	str[0] = c; str[1] = 0;
	/* Here, we assume that c < 256 */
    }
    text_extents(desc, xd->cc, gc, str, NULL, NULL, 
		 &iwidth, &iascent, &idescent, 1);
    *ascent = iascent;
    *descent = idescent;
    *width = iwidth;
#if 0
    printf("c = %d, '%s', face %d %f %f %f\n", 
	   c, str, gc->fontface, *width, *ascent, *descent);
#endif
}


static double X11_StrWidth(const char *str, pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint width;
    PangoFontDescription *desc = getFont(dd, gc);

    text_extents(desc, xd-> cc, gc, str, NULL, NULL, &width, NULL, NULL, 0);
    pango_font_description_free(desc);
    return (double) width;
}

static void X11_Text(double x, double y,
		     const char *str, double rot, double hadj,
		     pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint ascent, lbearing, width;
    PangoLayout *layout;
    PangoFontDescription *desc = getFont(dd, gc);
    
    if (R_ALPHA(gc->col) > 0) {
	cairo_save(xd->cc);
	text_extents(desc, xd->cc, gc, str, &lbearing, NULL, &width, 
		     &ascent, NULL, 0);
	cairo_move_to(xd->cc, x, y);
	if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	/* pango has a coord system at top left */
	cairo_rel_move_to(xd->cc, -lbearing - width*hadj, -ascent);
	SetColor(gc->col, dd);
	layout = layoutText(desc, xd->cc, str);
	pango_cairo_show_layout(xd->cc, layout);
	g_object_unref(layout);
	pango_font_description_free(desc);
	cairo_restore(xd->cc);
#ifdef XSYNC
	if (xd->type == WINDOW) XSync(display, 0);
#endif
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
	    ddEvent = (pDevDesc) temp;
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

static void X11_Mode(int mode, pDevDesc dd)
{
#ifdef XSYNC
    if (mode == 0) XSync(display, 0);
#else
    XSync(display, 0);
#endif
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
			 const char *title)
{
    pX11Desc xd;
    const char *fn;

    xd = Rf_allocX11DeviceDesc(pointsize);
    if(!xd) return FALSE;
    xd->bg = bgcolor;

    if(strlen(fn = CHAR(STRING_ELT(sfonts, 0))) > 499) {
	strcpy(xd->basefontfamily, "Helvetica");
	strcpy(xd->fontfamily, "Helvetica");
    } else {
	strcpy(xd->basefontfamily,fn);
	strcpy(xd->fontfamily,fn);
    }
    if(strlen(fn = CHAR(STRING_ELT(sfonts, 1))) > 499)
	strcpy(xd->symbolfamily, "Symbol");
    else strcpy(xd->symbolfamily,fn);

    /*	Start the Device Driver and Hardcopy.  */

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
    strncpy(xd->title, title, 100);
    xd->title[100] = '\0';

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
    /*	Set up Data Structures. */

    dd->close = X11_Close;
    dd->activate = X11_Activate;
    dd->deactivate = X11_Deactivate;
    dd->size = X11_Size;
    dd->newPage = X11_NewPage;
    dd->clip = X11_Clip;
    dd->strWidth = X11_StrWidth;
    dd->text = X11_Text;
    dd->rect = X11_Rect;
    dd->circle = X11_Circle;
    dd->line = X11_Line;
    dd->polyline = X11_Polyline;
    dd->polygon = X11_Polygon;
    dd->locator = X11_Locator;
    dd->mode = X11_Mode;
    dd->metricInfo = X11_MetricInfo;
    dd->hasTextUTF8 = TRUE;
    dd->strWidthUTF8 = X11_StrWidth;
    dd->textUTF8 = X11_Text;
    dd->wantSymbolUTF8 = TRUE;
    dd->useRotatedTextInContour = FALSE;

    /* Set required graphics parameters. */

    /* Window Dimensions in Pixels */
    /* Initialise the clipping rect too */

    dd->left = dd->clipLeft = 0;			/* left */
    dd->right = dd->clipRight = xd->windowWidth;	/* right */
    dd->bottom = dd->clipBottom = xd->windowHeight;	/* bottom */
    dd->top = dd->clipTop = 0;			/* top */

    /* Nominal Character Sizes in Pixels */

    dd->cra[0] = xd->basefontsize*1.25;
    dd->cra[1] = xd->basefontsize*1.5;
#ifdef DEBUG_X11
    printf("cra = %f %f\n", dd->cra[0], dd->cra[1]);
#endif

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

    dd->canClip = TRUE;
    dd->canHAdj = 2;
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
 This allocates an X11Desc instance  and sets its default values.
 */
pX11Desc  Rf_allocX11DeviceDesc(double ps)
{
    pX11Desc xd;
    /* allocate new device description */
    if (!(xd = (X11Desc*)calloc(1, sizeof(X11Desc))))
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

static void
Rf_addX11Device(const char *display, double width, double height, double ps,
		double gamma, int colormodel, int maxcubesize,
		int bgcolor, int canvascolor, const char *devname, SEXP sfonts,
		int res, int xpos, int ypos, const char *title)
{
    pDevDesc dev = NULL;
    GEDevDesc *dd;

    R_GE_checkVersionOrDie(R_GE_version);
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	/* Allocate and initialize the device driver data */
	if (!(dev = (pDevDesc) calloc(1, sizeof(NewDevDesc)))) return;
	if (!X11DeviceDriver(dev, display, width, height,
				ps, gamma, colormodel, maxcubesize,
				bgcolor, canvascolor, sfonts, res,
				xpos, ypos, title)) {
	    free(dev);
	    errorcall(gcall, _("unable to start device %s"), devname);
       	}
	dd = GEcreateDevDesc(dev);
	GEaddDevice2(dd, devname);
    } END_SUSPEND_INTERRUPTS;
}

static SEXP in_do_X11(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *display, *cname, *devname, *title;
    char *vmax;
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
    args = CDR(args);
    sc = CAR(args);
    if (!isString(sc) || LENGTH(sc) != 1)
	error(_("invalid value of 'title' in devWindows"));
    title = CHAR(STRING_ELT(sc, 0));

    devname = "X11";
    if (!strncmp(display, "png::", 5)) devname = "PNG";
    else if (!strncmp(display, "jpeg::", 6)) devname = "JPEG";
    else if (!strcmp(display, "XImage")) devname = "XImage";

    Rf_addX11Device(display, width, height, ps, gamma, colormodel,
		    maxcubesize, bgcolor, canvascolor, devname, sfonts,
		    res, xpos, ypos, title);
    vmaxset(vmax);
    return R_NilValue;
}

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

extern SEXP in_R_X11_dataviewer(SEXP call, SEXP op, SEXP args, SEXP rho);
extern SEXP in_RX11_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho);

#include <R_ext/Rdynload.h>
void R_init_R_X11(DllInfo *info)
{
    R_X11Routines *tmp;
    tmp = (R_X11Routines*) malloc(sizeof(R_X11Routines));
    if(!tmp) {
	error(_("cannot allocate memory for X11Routines structure"));
	return;
    }
    tmp->X11 = in_do_X11;
    tmp->de = in_RX11_dataentry;
    tmp->image = in_R_GetX11Image;
    tmp->access = in_R_X11_access;
    tmp->readclp = in_R_X11readclp;
    tmp->dv = in_R_X11_dataviewer;
    R_setX11Routines(tmp);
}
