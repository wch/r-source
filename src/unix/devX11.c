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

#include "Defn.h"
#include "Graphics.h"
#include "psx11.h"
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>/*->  Xlib.h  Xutil.h Xresource.h .. */
#include "rotated.h"
#include "devX11.h"/* 'Public' routines from here */

	/********************************************************/
	/* This device driver has been documented so that it be	*/
	/* used as a template for new drivers 			*/
	/********************************************************/

#define CURSOR		XC_crosshair		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

	/********************************************************/
	/* Each driver can have its own device-specic graphical */
	/* parameters and resources.  these should be wrapped	*/
	/* in a structure (like the x11Desc structure below)    */
	/* and attached to the overall device description via 	*/
	/* the dd->deviceSpecific pointer			*/
	/* NOTE that there are generic graphical parameters	*/
	/* which must be set by the device driver, but are	*/
	/* common to all device types (see Graphics.h)		*/
	/* so go in the GPar structure rather than this device- */
	/* specific structure					*/
	/********************************************************/

typedef struct {
			/* R Graphics Parameters */
			/* local device copy so that we can detect */
			/* when parameter changes */

	double cex;				/* Character expansion */
	double srt;				/* String rotation */
	int lty;				/* Line type */
	double lwd;
	int col;				/* Color */
	int fg;					/* Foreground */
	int bg;					/* Background */
	int fontface;				/* Typeface */
	int fontsize;				/* Size in points */

			/* X11 Driver Specific */
			/* parameters with copy per x11 device */

	int windowWidth;			/* Window width (pixels) */
	int windowHeight;			/* Window height (pixels) */
	int resize;				/* Window resized */
	Window window;				/* Graphics Window */
	GC wgc;					/* GC for window */
	Cursor gcursor;				/* Graphics Cursor */
	XSetWindowAttributes attributes;	/* Window attributes */
	XColor fgcolor;				/* Foreground color */
	XColor bgcolor;				/* Background color */
	XRectangle clip;			/* The clipping rectangle */

	int usefixed;
	XFontStruct *fixedfont;
	XFontStruct *font;

} x11Desc;

	/********************************************************/
	/* If there are resources that are shared by all devices*/
	/* of this type, you may wish to make them globals	*/
	/* rather than including them in the device-specific	*/
	/* parameters structure (especially if they are large !)*/
	/********************************************************/

			/* X11 Driver Specific */
			/* parameters with only one copy for all x11 devices */

static Display *display;			/* Display */
static int screen;				/* Screen */
static Window rootWindow;			/* Root Window */
static int depth;				/* Pixmap depth */
static XSetWindowAttributes attributes;		/* Window attributes */
static Colormap cmap;				/* Default color map */
static int blackpixel;				/* Black */
static int whitepixel;				/* White */
static XContext devPtrContext;
static Atom _XA_WM_PROTOCOLS, protocol;

static int displayOpen = 0;
static int numX11Devices = 0;


	/********************************************************/
	/* There must be an entry point for the device driver	*/
	/* which will create device-specific resources, 	*/
	/* initialise the device-specific parameters structure 	*/
	/* and return whether the setup succeeded		*/
	/* This is called by the graphics engine when the user	*/
	/* creates a new device of this type			*/
	/********************************************************/

	/* Device Driver Entry Point */

int X11DeviceDriver(DevDesc*, char*, double, double, double);

	/********************************************************/
	/* There are a number of actions that every device 	*/
	/* driver is expected to perform (even if, in some	*/
	/* cases it does nothing - just so long as it doesn't 	*/
	/* crash !).  this is how the graphics engine interacts */
	/* with each device. ecah action will be documented 	*/
	/* individually. 					*/
	/* hooks for these actions must be set up when the 	*/
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
static int    X11_Locator(double*, double*, DevDesc*);
static void   X11_Mode(int);
static void   X11_NewPage(DevDesc*);
static int    X11_Open(DevDesc*, x11Desc*, char*, double, double);
static void   X11_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   X11_Polyline(int, double*, double*, int, DevDesc*);
static void   X11_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   X11_Resize(DevDesc*);
static double X11_StrWidth(char*, DevDesc*);
static void   X11_Text(double, double, int, char*, double, double, double,
		       DevDesc*);
static void   X11_MetricInfo(int, double*, double*, double*, DevDesc*);

	/********************************************************/
	/* end of list of required device driver actions 	*/
	/********************************************************/

	/* Support Routines */

static void FreeColors(void);
static XFontStruct *RLoadFont(int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static int SetBaseFont(x11Desc*);
static void SetColor(int, DevDesc*);
static void SetFont(int, int, DevDesc*);
static void SetLinetype(int, double, DevDesc*);


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
		if (event.xclient.data.l[0] == protocol) {
			XFindContext(display, event.xclient.window,
				     devPtrContext, &temp);
			dd = (DevDesc *) temp;
			KillDevice(dd);
		}
}

void ProcessEvents(void)
{
	XEvent event;
	while (displayOpen && XPending(display)) {
		XNextEvent(display, &event);
		/* printf("%i\n",event.type); */
		handleEvent(event);
	}
}

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
static char *symbolname  = "-adobe-symbol-*-*-*-*-*-%d-*-*-*-*-*-*";
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

		if(face == 5)
			sprintf(buf, symbolname, 10 * size);
		else
			sprintf(buf, fontname,
				weight[(face-1)%2],
				slant[((face-1)/2)%2], 10 * size);
#ifdef DEBUGGING
		Rprintf("loading:\n%s\n",buf);
#endif
		tmp = XLoadQueryFont(display, buf);
#ifdef DEBUGGING
		if(tmp) Rprintf("success\n"); else Rprintf("failure\n");
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
	if(!xd->font) {
		fontname = fontname_R5;
		xd->font = RLoadFont(xd->fontface, xd->fontsize);
	}
	if(!xd->font) {
		xd->usefixed = 1;
		xd->font = xd->fixedfont = XLoadQueryFont(display, "fixed");
		if(!xd->fixedfont)
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

	if(face < 1 || face > 5) face = 1;
	size = 2 * size / 2;
	if(size < SMALLEST) size = SMALLEST;
	if(size > LARGEST) size = LARGEST;

	if(!xd->usefixed && (size != xd->fontsize  || face != xd->fontface)) {
		while(size < dd->gp.ps) {
			if(fontarray[size-6][face-1]) goto found;
			if(!missingfont[size-6][face-1]) {
				fontarray[size-6][face-1] = RLoadFont(face, size);
				if(fontarray[size-6][face-1]) goto found;
				missingfont[size-6][face-1] = 1;
			}
			size += 2;
		}
		while(size >= dd->gp.ps) {
			if(fontarray[size-6][face-1]) goto found;
			if(!missingfont[size-6][face-1]) {
				fontarray[size-6][face-1] = RLoadFont(face, size);
				if(fontarray[size-6][face-1]) goto found;
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

static struct {
	int rcolor;
	unsigned long pixel;
} Colors[256];

static int NColors;

static void SetColor(int color, DevDesc *dd)
{
	int i, r, g, b;
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	if(color != xd->col) {
		for(i=0 ; i<NColors ; i++) {
			if(color == Colors[i].rcolor) {
				xd->fgcolor.pixel = Colors[i].pixel;
				goto found;
			}
		}

		/* Gamma Correction */
		/* This is very experimental! */

		if(dd->gp.gamma != 1) {
			r = (int)(255*pow(R_RED(color)/255.0, dd->gp.gamma));
			g = (int)(255*pow(R_GREEN(color)/255.0, dd->gp.gamma));
			b = (int)(255*pow(R_BLUE(color)/255.0, dd->gp.gamma));
		}
		else {
			r = R_RED(color);
			g = R_GREEN(color);
			b = R_BLUE(color);
		}

		/* Note that X11 uses 16 bits to describe the   */
		/* level of the RGB primaries and this explains */
		/* the additional * 257 below.                  */
		/* 257 = (2^16-1)/(2^8-1)			*/

		xd->fgcolor.red   = r * 257;
		xd->fgcolor.green = g * 257;
		xd->fgcolor.blue  = b * 257;

		if(NColors == 255 ||
		   XAllocColor(display, cmap, &(xd->fgcolor)) == 0)
			error("color allocation error\n");
		Colors[NColors].rcolor = color;
		Colors[NColors].pixel = xd->fgcolor.pixel;
		NColors++;
	found:
		blackpixel = xd->fgcolor.pixel;
		xd->col = color;
		XSetState(display, xd->wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
	}
}

static void FreeColors()
{
	int i;
	for(i=0 ; i<NColors ; i++)
		XFreeColors(display, cmap, &(Colors[i].pixel), 1, 0);
	NColors = 0;
}

/*
 *	Some Notes on Line Textures
 *
 *	Line textures are stored as an array of 4-bit integers within
 *	a single 32-bit word.  These integers contain the lengths of
 *	lines to be drawn with the pen alternately down and then up.
 *	The device should try to arrange that these values are measured
 *	in points if possible, although pixels is ok on most displays.
 *
 *	If newlty contains a line texture description it is decoded
 *	as follows:
 *
 *		ndash = 0;
 *		for(i=0 ; i<8 && newlty&15 ; i++) {
 *			dashlist[ndash++] = newlty&15;
 *			newlty = newlty>>4;
 *		}
 *		dashlist[0] = length of pen-down segment
 *		dashlist[1] = length of pen-up segment
 *		etc
 *
 *	An integer containing a zero terminates the pattern.  Hence
 *	ndash in this code fragment gives the length of the texture
 *	description.  If a description contains an odd number of
 *	elements it is replicated to create a pattern with an
 *	even number of elements.  (If this is a pain, do something
 *	different its not crucial).
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
	if(newlwd < 1)
		newlwd = 1;
	if(newlty != xd->lty || newlwd != xd->lwd) {
		xd->lty = newlty;
		xd->lwd = newlwd;
		if(newlty == 0) {
			XSetLineAttributes(display,
				xd->wgc,
				newlwd,
				LineSolid,
				CapRound,
				JoinRound);
		}
		else {
			ndash = 0;
			for(i=0 ; i<8 && newlty != 0 ; i++) {
				int j = newlty & 15;
				if (j == 0) j = 1; /* Or we die with an X Error */
				/* make sure that scaled line texture */
				/* does not exceed X11 storage limits */
				j = j*newlwd;
				if (j > 255) j=255;
				/* scale line texture for line width */
				dashlist[ndash++] = j;
				newlty = newlty>>4;
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
	/* device_Open is not usually called directly by the 	*/
	/* graphics engine;  it is usually only called from 	*/
	/* the device-driver entry point.			*/
	/* this function should set up all of the device-	*/
	/* specific resources for a new device			*/
	/* this function is given a new	structure for device-	*/
	/* specific information AND it must FREE the structure 	*/
	/* if anything goes seriously wrong			*/
	/* NOTE that it is perfectly acceptable for this 	*/
	/* function to set generic graphics parameters too	*/
	/* (i.e., override the generic parameter settings	*/
	/* which GInit sets up) all at the author's own risk	*/
	/* of course :)						*/
	/********************************************************/

static int X11_Open(DevDesc *dd, x11Desc *xd, char *dsp, double w, double h)
{
		/* if have to bail out with "error" then must */
		/* free(dd) and free(xd) */

	XEvent event;
	int iw, ih, result;
	XGCValues gcv;
	XColor exact;

		/* only open display if we haven't already */
	if (!displayOpen) {

		/* Open the default display */
		/* A return value of 0 indicates failure */

		if ((display = XOpenDisplay(dsp)) == NULL)
			return 0;

			/* Default Screen and Root Window */

		screen = XDefaultScreen(display);
		rootWindow = XDefaultRootWindow(display);
		depth = XDefaultDepth(display, screen);

		cmap = DefaultColormap(display, screen);
		NColors = 0;

		devPtrContext = XUniqueContext();

		displayOpen = 1;
	}

	if (!SetBaseFont(xd)) {
		Rprintf("can't find X11 font\n");
		return 0;
	}

		/* Foreground and Background Colors */

	xd->bg =  dd->dp.bg  = R_RGB(255,255,255);
	xd->fg =  dd->dp.fg  = R_RGB(0,0,0);
	xd->col = dd->dp.col = xd->fg;

	result = XAllocNamedColor(display, cmap, "white", &exact, &(xd->bgcolor));
	if (result == 0) {
		free(xd);
		free(dd);
		error("color allocation error\n");
	}

	result = XAllocNamedColor(display, cmap, "black", &exact, &(xd->fgcolor));
	if (result == 0) {
		free(xd);
		free(dd);
		error("color allocation error\n");
	}

	whitepixel = xd->bgcolor.pixel;
	blackpixel = xd->fgcolor.pixel;

		/* Try to create a simple window */
		/* Want to know about exposures */
		/* and window-resizes and locations */

	attributes.background_pixel = whitepixel;
	attributes.border_pixel = blackpixel;
	attributes.backing_store = Always;
	attributes.event_mask = ButtonPressMask
	    | ExposureMask
	    | StructureNotifyMask;

	xd->windowWidth = iw = w/pixelWidth();
	xd->windowHeight = ih = h/pixelHeight();

	if ((xd->window = XCreateWindow(
		display,
		rootWindow,
		DisplayWidth(display, screen) - iw - 10, 10, iw, ih, 1,
		DefaultDepth(display, screen),
		InputOutput,
		DefaultVisual(display, screen),
		CWEventMask | CWBackPixel | CWBorderPixel | CWBackingStore,
		&attributes)) == 0)
		return 0;

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
	return 1;
}

	/********************************************************/
	/* device_StrWidth should return the width of the given */
	/* string in DEVICE units (GStrWidth is responsible for */
	/* converting from DEVICE to whatever units the user 	*/
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
	/* device_MetricInfo should return height, depth, and 	*/
	/* width information for the given character in DEVICE	*/
	/* units (GMetricInfo does the necessary conversions)	*/
	/* This is used for formatting mathematical expressions	*/
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

	if(c == 0) {
		*ascent = xd->font->ascent;
		*descent = xd->font->descent;
		*width = xd->font->max_bounds.width;
	}
	else if(first <= c && c <= last) {
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
	/* device_Clip is given the left, right, bottom, and 	*/
	/* top of a rectangle (in DEVICE coordinates).  it 	*/
	/* should have the side-effect that subsequent output	*/
	/* is clipped to the given rectangle			*/
	/********************************************************/

static void X11_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	if (x0 < x1) {
		xd->clip.x = (int)x0;
		xd->clip.width = (int)(x1 - x0);
	}
	else {
		xd->clip.x = (int)x1;
		xd->clip.width = (int)(x0 - x1);
	}
	if (y0 < y1) {
		xd->clip.y = (int)y0;
		xd->clip.height = (int)(y1 - y0);
	}
	else {
		xd->clip.y = (int)y1;
		xd->clip.height = (int)(y0 - y1);
	}
	XSetClipRectangles(display, xd->wgc, 0, 0, &(xd->clip), 1, Unsorted);
	XSync(display, 0);
}

	/********************************************************/
	/* device_Resize is called whenever the device is 	*/
	/* resized.  the function must update the GPar 		*/
	/* parameters (left, right, bottom, and top) for the 	*/
	/* new device size					*/
	/* this is not usually called directly by the graphics	*/
	/* engine because the detection of device resizes	*/
	/* (e.g., a window resize) are usually detected by	*/
	/* device-specific code	(see ProcessEvents in this file)*/
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
	/* a new page.  a new page might mean just clearing the	*/
	/* device (as in this case) or moving to a new page	*/
	/* (e.g., postscript)					*/
	/********************************************************/

static void X11_NewPage(DevDesc *dd)
{
	int result;
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	FreeColors();

	if(xd->bg != dd->dp.bg) {
		xd->bg = dd->dp.bg;
		xd->bgcolor.red   = R_RED(xd->bg)   * 257;
		xd->bgcolor.green = R_GREEN(xd->bg) * 257;
		xd->bgcolor.blue  = R_BLUE(xd->bg)  * 257;
		result = XAllocColor(display, cmap, &(xd->bgcolor));
		if (result == 0) error("color allocation error\n");
		whitepixel = xd->bgcolor.pixel;
		XSetWindowBackground(display, xd->window, whitepixel);
	}
	XClearWindow(display, xd->window);
	XSync(display, 0);
}

	/********************************************************/
	/* device_Close is called when the device is killed	*/
	/* this function is responsible for destroying any 	*/
	/* device-specific resources that were created in	*/
	/* device_Open and for FREEing the device-specific	*/
	/* parameters structure					*/
	/********************************************************/

static void X11_Close(DevDesc *dd)
{
	int i, j;
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	/* process pending events */
	ProcessEvents();

	XFreeCursor(display, xd->gcursor);
	XFreeGC(display, xd->wgc);
	XDestroyWindow(display, xd->window);
	XSync(display, 0);

	numX11Devices--;
	if (numX11Devices == 0)  {
		/* Free Resources Here */
		for(i=0 ; i<NFONT ; i++)
			for(j=0 ; j<5 ; j++) {
				if(fontarray[i][j] != NULL) {
					XUnloadFont(display, fontarray[i][j]->fid);
					fontarray[i][j] = NULL;
				}
				missingfont[i][j] = 0;
			}
		XCloseDisplay(display);
		displayOpen = 0;
	}

	free(xd);
}

	/********************************************************/
	/* device_Activate is called when a device becomes the 	*/
	/* active device.  in this case it is used to change the*/
	/* title of a window to indicate the active status of 	*/
	/* the device to the user.  not all device types will 	*/
	/* do anything						*/
	/********************************************************/

static unsigned char title[11] = "R Graphics";

static void X11_Activate(DevDesc *dd)
{
	char t[50];
	char num[3];
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	strcpy(t, title);
	strcat(t, ": Device ");
	sprintf(num, "%i", deviceNumber(dd)+1);
	strcat(t, num);
	strcat(t, " (ACTIVE)");
	XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
			8, PropModeReplace, t, 50);
	XSync(display, 0);
}

	/********************************************************/
	/* device_Deactivate is called when a device becomes	*/
	/* inactive.  in this case it is used to change the 	*/
	/* title of a window to indicate the inactive status of */
	/* the device to the user.  not all device types will	*/
	/* do anything						*/
	/********************************************************/

static void X11_Deactivate(DevDesc *dd)
{
	char t[50];
	char num[3];
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	strcpy(t, title);
	strcat(t, ": Device ");
	sprintf(num, "%i", deviceNumber(dd)+1);
	strcat(t, num);
	strcat(t, " (inactive)");
	XChangeProperty(display, xd->window, XA_WM_NAME, XA_STRING,
			8, PropModeReplace, t, 50);
	XSync(display, 0);
}

	/********************************************************/
	/* device_Rect should have the side-effect that a 	*/
	/* rectangle is drawn with the given locations for its 	*/
	/* opposite corners.  the border of the rectangle	*/
	/* should be in the given "fg" colour and the rectangle	*/
	/* should be filled with the given "bg" colour		*/
	/* if "fg" is NA_INTEGER then no border should be drawn */
	/* if "bg" is NA_INTEGER then the rectangle should not 	*/
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
	}
	if (fg != NA_INTEGER) {
		SetColor(fg, dd);
		SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
		XDrawRectangle(display, xd->window, xd->wgc, (int)x0, (int)y0,
			(int)x1 - (int)x0, (int)y1 - (int)y0);
	}
	XSync(display, 0);
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
	/* the location is in arbitrary coordinates and the 	*/
	/* function is responsible for converting this to	*/
	/* DEVICE coordinates.  the radius is given in DEVICE	*/
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
	if(col != NA_INTEGER) {
		SetColor(col, dd);
		XFillArc(display, xd->window, xd->wgc, ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
	}
	if(border != NA_INTEGER) {
		SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
		SetColor(border, dd);
		XDrawArc(display, xd->window, xd->wgc, ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
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
	double xx1, yy1, xx2, yy2;
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
	XSync(display, 0);
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
	int i;
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
	XDrawLines(display, xd->window, xd->wgc, points, n, CoordModeOrigin);
	XSync(display, 0);

	C_free((char *) points);
}

	/********************************************************/
	/* device_Polygon should have the side-effect that a 	*/
	/* polygon is drawn using the given x and y values	*/
	/* the polygon border should be drawn in the "fg" 	*/
	/* colour and filled with the "bg" colour		*/
	/* if "fg" is NA_INTEGER don't draw the border		*/
	/* if "bg" is NA_INTEGER don't fill the polygon		*/
	/* the x and y values are in arbitrary coordinates and 	*/
	/* the function is responsible for converting them to 	*/
	/* DEVICE coordinates using GConvert			*/
	/********************************************************/

static void X11_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
	XPoint *points;
	char *vmaxget();
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
	if(bg != NA_INTEGER) {
		SetColor(bg, dd);
		XFillPolygon(display, xd->window, xd->wgc, points, n, Complex, CoordModeOrigin);
		XSync(display, 0);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg, dd);
		SetLinetype(dd->gp.lty, dd->gp.lwd, dd);
		XDrawLines(display, xd->window, xd->wgc, points, n+1, CoordModeOrigin);
		XSync(display, 0);
	}

	C_free((char *) points);
}


	/********************************************************/
	/* device_Text should have the side-effect that the 	*/
	/* given text is drawn at the given location		*/
	/* the text should be justified according to "xc" and	*/
	/* "yc" (0 = left, 0.5 = centre, 1 = right)		*/
	/* and rotated according to rot (degrees)		*/
	/* the location is in an arbitrary coordinate system	*/
	/* and this function is responsible for converting the	*/
	/* location to DEVICE coordinates using GConvert	*/
	/********************************************************/

static double deg2rad = 0.01745329251994329576;

static void X11_Text(double x, double y, int coords,
		     char *str, double xc, double yc, double rot, DevDesc *dd)
{
	int len, size;
	double xl, yl;
	x11Desc *xd = (x11Desc *) dd->deviceSpecific;

	size = dd->gp.cex * dd->gp.ps + 0.5;
	SetFont(dd->gp.font, size, dd);
	SetColor(dd->gp.col, dd);
	len = strlen(str);
	GConvert(&x, &y, coords, DEVICE, dd);
	if(xc != 0.0 || yc != 0) {
		xl = X11_StrWidth(str, dd);
		yl = GConvertYUnits(1, CHARS, DEVICE, dd);
		x += -xc * xl * cos(deg2rad * rot) +
		      yc * yl * sin(deg2rad * rot);
		y -= -xc * xl * sin(deg2rad * rot) -
		      yc * yl * cos(deg2rad * rot);
	}
	XRotDrawString(display, xd->font, rot, xd->window, xd->wgc,
		       (int)x, (int)y, str);
	XSync(display, 0);
}

	/********************************************************/
	/* device_Locator should return the location of the next*/
	/* mouse click (in DEVICE coordinates;  GLocator is	*/
	/* responsible for any conversions)			*/
	/* not all devices will do anythin (e.g., postscript)	*/
	/********************************************************/

static int X11_Locator(double *x, double *y, DevDesc *dd)
{
	XEvent event;
	DevDesc *ddEvent;
	caddr_t temp;
	int done = 0;
	ProcessEvents();	/* discard pending events */
	XSync(display, 1);
		/* handle X events as normal until get a button */
		/* click in the desired device */
	while (!done) {
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
	if (done == 1)
		return 1;
	else
		return 0;
}

	/********************************************************/
	/* device_Mode is called whenever the graphics engine 	*/
	/* starts drawing (mode=1) or stops drawing (mode=1)	*/
	/* the device is not required to do anything		*/
	/********************************************************/

/* Set Graphics mode - not needed for X11 */
static void X11_Mode(int mode)
{
	if(mode == 0) XSync(display, 0);
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
	/* the device-driver entry point is given a device 	*/
	/* description structure that it must set up.  this 	*/
	/* involves several important jobs ...			*/
	/* (1) it must ALLOCATE a new device-specific parameters*/
	/* structure and FREE that structure if anything goes	*/
	/* wrong (i.e., it won't report a successful setup to	*/
	/* the graphics engine (the graphics engine is NOT	*/
	/* responsible for allocating or freeing device-specific*/
	/* resources or parameters)				*/
	/* (2) it must initialise the device-specific resources */
	/* and parameters (mostly done by calling device_Open)	*/
	/* (3) it must initialise the generic graphical 	*/
	/* parameters that are not initialised by GInit (because*/
	/* only the device knows what values they should have)	*/
	/* see Graphics.h for the official list of these	*/
	/* (4) it may reset generic graphics parameters that	*/
	/* have already been initialised by GInit (although you	*/
	/* should know what you are doing if you do this)	*/
	/* (5) it must attach the device-specific parameters	*/
        /* structure to the device description structure	*/
	/* e.g., dd->deviceSpecfic = (void *) xd;		*/
	/* (6) it must FREE the overall device description if 	*/
	/* it wants to bail out to the top-level		*/
	/* the graphics engine is responsible for allocating 	*/
	/* the device description and freeing it in most cases	*/
	/* but if the device driver freaks out it needs to do 	*/
	/* the clean-up itself					*/
	/********************************************************/


	/*  X11 Device Driver Arguments               */

	/*  cpars[0] = display name                   */
	/*  cpars[1] = paper type (a4, letter, none)  */

	/*  npars[0] = width (inches)                 */
	/*  npars[1] = height (inches)                */
	/*  npars[2] = base pointsize                 */
	/*  npars[3] = paper orientation              */
	/*	       1 - portrait                   */
	/*	       2 - landscape                  */
	/*	       3 - flexible                   */

int X11DeviceDriver(DevDesc *dd, char *display, double width, double height, double pointsize)
{
	/* if need to bail out with some sort of "error" then */
	/* must free(dd) */

	int ps;
	x11Desc *xd;

	/* allocate new device description */
	if (!(xd = (x11Desc *) malloc(sizeof(x11Desc))))
		return 0;

	/* from here on, if need to bail out with "error", must also */
	/* free(xd) */

	/*  Font will load at first use  */

	ps = pointsize;
	if(ps < 6 || ps > 24) ps = 12;
	ps = 2*(ps/2);
	xd->fontface = -1;
	xd->fontsize = -1;
	dd->dp.font = 1;
	dd->dp.ps = ps;

	/*  Start the Device Driver and Hardcopy.  */

	if (!X11_Open(dd, xd, display, width, height)) {
		free(xd);
		return 0;
	}

	/*  Set up Data Structures  */

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

	/* set graphics parameters that must be set by device driver */
	/* Window Dimensions in Pixels */

	dd->dp.left = 0;			/* left */
	dd->dp.right = xd->windowWidth;		/* right */
	dd->dp.bottom = xd->windowHeight;	/* bottom */
	dd->dp.top = 0;				/* top */

	/* Nominal Character Sizes in Pixels */

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
	/* Clipping is problematic for X11 */
	/* Graphics is clipped, text is not */

	dd->dp.canResizePlot = 1;
	dd->dp.canChangeFont = 0;
	dd->dp.canRotateText = 1;
	dd->dp.canResizeText = 1;
	dd->dp.canClip = 1;

	/* initialise x11 device description (most of the work */
	/* has been done in X11_Open) */

	xd->cex = 1.0;
	xd->srt = 0.0;
	xd->lty = 0;
	xd->resize = 0;

	dd->deviceSpecific = (void *) xd;

	dd->displayListOn = 1;

	ProcessEvents();

	return 1;
}

int X11ConnectionNumber()
{
	if (displayOpen)
		return ConnectionNumber(display);
	else
		return 0;
}

