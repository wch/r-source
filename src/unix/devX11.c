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
#include "rotated.h"

#define CURSOR		XC_crosshair		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */


			/* R Graphics Parameters */

static double cex = 1.0;			/* Character expansion */
static double srt = 0.0;			/* String rotation */
static int lty = -1;				/* Line type */
static double lwd = -1.0;
static int col = -1;				/* Color */
static int fg;					/* Foreground */
static int bg;					/* Background */
static int fontface = -1;			/* Typeface */
static int fontsize = -1;			/* Size in points */


			/* X11 Driver Specific */

static int windowWidth;				/* Window width (pixels) */
static int windowHeight;			/* Window height (pixels) */
static int resize = 0;				/* Window resized */
static Display *display;			/* Display */
static int screen;				/* Screen */
static int depth;				/* Pixmap depth */
static Window rootWindow;			/* Root Window */
static Window window;				/* Graphics Window */
static Cursor gcursor;				/* Graphics Cursor */
static GC wgc;					/* GC for window */
static XSetWindowAttributes attributes;		/* Window attributes */
static XEvent event;				/* Event */
static Colormap cmap;				/* Default color map */
static XColor fgcolor;				/* Foreground color */
static XColor bgcolor;				/* Background color */
static int blackpixel;				/* Black */
static int whitepixel;				/* White */
static XRectangle clip;				/* The clipping rectangle */
static int hardcopy;				/* Generate Hardcopy? */

	/* Device Driver Entry Point */

int X11DeviceDriver(char**, int, double*, int);

	/* Device Driver Actions */

static void   X11_Circle(double, double, double, int, int);
static void   X11_Clip(double, double, double, double);
static void   X11_Close(void);
static void   X11_EndPath(void);
static void   X11_Hold(void);
static void   X11_LineTo(double, double);
static int    X11_Locator(double*, double*);
static void   X11_Mode(int);
static void   X11_MoveTo(double, double);
static void   X11_NewPlot(void);
static int    X11_Open(char*, double, double);
static void   X11_Polygon(int, double*, double*, int, int);
static void   X11_PrintPlot(char*);
static void   X11_Rect(double, double, double, double, int, int);
static void   X11_Resize(void);
static void   X11_SavePlot(char*);
static void   X11_StartPath(void);
static double X11_StrWidth(char*);
static void   X11_Text(double, double, char*, double, double, double);
static void   X11_MetricInfo(int, double*, double*, double*);

	/* Support Routines */

static void FreeColors(void);
static void ProcessEvents(void);
static XFontStruct *RLoadFont(int, int);
static double pixelHeight(void);
static double pixelWidth(void);
static int SetBaseFont(void);
static void SetColor(int);
static void SetFont(int, int);
static void SetLinetype(int, double);


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

static void ProcessEvents(void)
{
	while (XPending(display)) {
		XNextEvent(display, &event);
		if (event.xany.type == Expose) {
			while (event.xexpose.count) {
				XNextEvent(display, &event);
			}
		}
		else if (event.type == ConfigureNotify) {
			windowWidth = event.xconfigure.width;
			windowHeight = event.xconfigure.height;
			resize = 1;
		}
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

static int usefixed = 0;
static XFontStruct *fixedfont;
static XFontStruct *font;

static XFontStruct *RLoadFont(int face, int size)
{
#ifdef DEBUGGING
	XFontStruct *tmp;
#endif
	char buf[128];
	if(face == 5)
		sprintf(buf, symbolname, 10 * size);
	else
		sprintf(buf, fontname, weight[(face-1)%2], slant[((face-1)/2)%2], 10 * size);
#ifdef DEBUGGING
	Rprintf("loading:\n%s\n",buf);
	tmp = XLoadQueryFont(display, buf);
	if(tmp) Rprintf("success\n"); else Rprintf("failure\n");
	return tmp;
#else
	return XLoadQueryFont(display, buf);
#endif
}

			/* Quiz the server about fonts. */
			/* 1) Try for 100dpi (X11R6) font */
			/* 2) Try for *dpi (X11R6) font */
			/* 3) Try "fixed" and if that fails, bail out */

static int SetBaseFont()
{
	fontface = 1;
	fontsize = GP->ps;
	fontname = fontname_R6;
	font = fontarray[fontsize-6][fontface-1] = RLoadFont(fontface, fontsize);
	if(!font) {
		fontname = fontname_R5;
		font = fontarray[fontsize-6][fontface-1] = RLoadFont(fontface, fontsize);
	}
	if(!font) {
		usefixed = 1;
		font = fixedfont = XLoadQueryFont(display, "fixed");
		if(!fixedfont)
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

static void SetFont(int face, int size)
{
	if(face < 1 || face > 5) face = 1;
	size = 2 * size / 2;
	if(size < SMALLEST) size = SMALLEST;
	if(size > LARGEST) size = LARGEST;

	if(!usefixed && (size != fontsize  || face != fontface)) {
		while(size < GP->ps) {
			if(fontarray[size-6][face-1]) goto found;
			if(!missingfont[size-6][face-1]) {
				fontarray[size-6][face-1] = RLoadFont(face, size);
				if(fontarray[size-6][face-1]) goto found;
				missingfont[size-6][face-1] = 1;
			}
			size += 2;
		}
		while(size >= GP->ps) {
			if(fontarray[size-6][face-1]) goto found;
			if(!missingfont[size-6][face-1]) {
				fontarray[size-6][face-1] = RLoadFont(face, size);
				if(fontarray[size-6][face-1]) goto found;
				missingfont[size-6][face-1] = 1;
			}
			size -= 2;
		}
		size = GP->ps;
		face = 1;
	found:
		font = fontarray[size-6][face-1];
		fontface = face;
		fontsize = size;
		XSetFont(display, wgc, font->fid);
	}
}

static struct {
	int rcolor;
	unsigned long pixel;
} Colors[256];

static int NColors;

static void SetColor(int color)
{
	int i, r, g, b;
	if(color != col) {
		for(i=0 ; i<NColors ; i++) {
			if(color == Colors[i].rcolor) {
				fgcolor.pixel = Colors[i].pixel;
				goto found;
			}
		}

		/* Gamma Correction */
		/* This is very experimental! */
		
		if(GP->gamma != 1) {
			r = (int)(255*pow(R_RED(color)/255.0, GP->gamma));
			g = (int)(255*pow(R_GREEN(color)/255.0, GP->gamma));
			b = (int)(255*pow(R_BLUE(color)/255.0, GP->gamma));
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

		fgcolor.red   = r * 257;
		fgcolor.green = g * 257;
		fgcolor.blue  = b * 257;

		if(NColors == 255 || XAllocColor(display, cmap, &fgcolor) == 0)
			error("color allocation error\n");
		Colors[NColors].rcolor = color;
		Colors[NColors].pixel = fgcolor.pixel;
		NColors++;
	found:
		blackpixel = fgcolor.pixel;
		col = color;
		XSetState(display, wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
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
 */

static void SetLinetype(int newlty, double nlwd)
{
	unsigned char dashlist[8];
	int i, ndash, newlwd;

	newlwd = nlwd;
	if(newlwd < 1)
		newlwd = 1;
	if(newlty != lty || newlwd != lwd) {
		lty = newlty;
		lwd = newlwd;
		if(newlty == 0) {
			XSetLineAttributes(display,
				wgc,
				newlwd,
				LineSolid,
				CapRound,
				JoinRound);
		}
		else {
			ndash = 0;
			for(i=0 ; i<8 && newlty&15 ; i++) {
				dashlist[ndash++] = newlty & 15;
				newlty = newlty>>4;
			}
			XSetDashes(display, wgc, 0, dashlist, ndash);
			XSetLineAttributes(display,
				wgc,
				newlwd,
				LineOnOffDash,
				CapButt,
				JoinRound);
		}
	}
}

static int X11_Open(char *dsp, double w, double h)
{
	int iw, ih, result;
	XGCValues gcv;
	XColor exact;

		/* Open the default display */
		/* A return value of 0 indicates failure */

	if ((display = XOpenDisplay(dsp)) == NULL)
		return 0;
	if (!SetBaseFont()) {
		Rprintf("can't find X11 font\n");
		XCloseDisplay(display);
		return 0;
	}

		/* Default Screen and Root Window */

	screen = XDefaultScreen(display);
	rootWindow = XDefaultRootWindow(display);
	depth = XDefaultDepth(display, screen);

		/* Foreground and Background Colors */

	bg =  DP->bg  = GP->bg  = R_RGB(255,255,255);
	fg =  DP->fg  = GP->fg  = R_RGB(0,0,0);
	col = DP->col = GP->col = fg;

	cmap = DefaultColormap(display, screen);
	NColors = 0;

	result = XAllocNamedColor(display, cmap, "white", &exact, &bgcolor);
	if (result == 0) error("color allocation error\n");

	result = XAllocNamedColor(display, cmap, "black", &exact, &fgcolor);
	if (result == 0) error("color allocation error\n");

	whitepixel = bgcolor.pixel;
	blackpixel = fgcolor.pixel;

		/* Try to create a simple window */
		/* Want to know about exposures */
		/* and window-resizes and locations */

	attributes.background_pixel = whitepixel;
	attributes.border_pixel = blackpixel;
	attributes.backing_store = Always;
	attributes.event_mask = ButtonPressMask
	    | ExposureMask
	    | StructureNotifyMask;

	windowWidth = iw = w/pixelWidth();
	windowHeight = ih = h/pixelHeight();

	if ((window = XCreateWindow(
		display,
		rootWindow,
		DisplayWidth(display, screen) - iw - 10, 10, iw, ih, 1,
		DefaultDepth(display, screen),
		InputOutput,
		DefaultVisual(display, screen),
		CWEventMask | CWBackPixel | CWBorderPixel | CWBackingStore,
		&attributes)) == 0)
		return 0;

	XChangeProperty( display, window, XA_WM_NAME, XA_STRING,
		8, PropModeReplace, "R Graphics", 13);

	gcursor = XCreateFontCursor(display, CURSOR);
	XDefineCursor(display, window, gcursor);

		/* Map the window */

	XSelectInput(display, window,
		   ExposureMask | ButtonPressMask | StructureNotifyMask);
	XMapWindow(display, window);
	XSync(display, 0);

		/* Gobble expose events */

	XNextEvent(display, &event);
	if (event.xany.type == Expose) {
		while (event.xexpose.count)
			XNextEvent(display, &event);
	}

		/* Set the graphics context */

	gcv.arc_mode = ArcChord;
	wgc = XCreateGC(display, window, GCArcMode, &gcv);
	XSetState(display, wgc, blackpixel, whitepixel, GXcopy, AllPlanes);
	XSetFont(display, wgc, font->fid);
	SetLinetype(0, 1);
	return 1;
}

static double X11_StrWidth(char *str)
{
	int size = GP->cex * GP->ps + 0.5;
	SetFont(GP->font, size);
	return (double)XTextWidth(font, str, strlen(str));
}


	/* Character Metric Information */
	/* Passing c == 0 gets font information */

static void X11_MetricInfo(int c, double* ascent, double* descent, double* width)
{
	int first, last;
	int size = GP->cex * GP->ps + 0.5;

	SetFont(GP->font, size);
	first = font->min_char_or_byte2;
	last = font->max_char_or_byte2;

	if(c == 0) {
		*ascent = font->ascent;
		*descent = font->descent;
		*width = font->max_bounds.width;
	}
	else if(first <= c && c <= last) {
		*ascent = font->per_char[c-first].ascent;
		*descent = font->per_char[c-first].descent;
		*width = font->per_char[c-first].width;
	}
	else {
		*ascent = 0;
		*descent = 0;
		*width = 0;
	}
}

static void X11_Clip(double x0, double x1, double y0, double y1)
{
	if (x0 < x1) {
		clip.x = (int)x0;
		clip.width = (int)(x1 - x0);
	}
	else {
		clip.x = (int)x1;
		clip.width = (int)(x0 - x1);
	}
	if (y0 < y1) {
		clip.y = (int)y0;
		clip.height = (int)(y1 - y0);
	}
	else {
		clip.y = (int)y1;
		clip.height = (int)(y0 - y1);
	}
	XSetClipRectangles(display, wgc, 0, 0, &clip, 1, Unsorted);
	if(hardcopy) psx11_Clip(x0, x1, y0, y1);
}

static void X11_Resize()
{
	ProcessEvents();
	if (resize) {
		DP->left = 0.0;
		DP->right = windowWidth;
		DP->bottom = windowHeight;
		DP->top = 0.0;
		resize = 0;
	}
}

static void X11_NewPlot()
{
	int result;

	FreeColors();

	if(bg != DP->bg) {
		bg = DP->bg;
		bgcolor.red   = R_RED(bg)   * 257;
		bgcolor.green = R_GREEN(bg) * 257;
		bgcolor.blue  = R_BLUE(bg)  * 257;
		result = XAllocColor(display, cmap, &bgcolor);
		if (result == 0) error("color allocation error\n");
		whitepixel = bgcolor.pixel;
		XSetWindowBackground(display, window, whitepixel);
	}
	XClearWindow(display, window);
	XSync(display, 0);
	if(hardcopy)
		psx11_NewPlot(windowWidth, windowHeight,
			pixelWidth(), pixelHeight(), fontface, fontsize,
			col, lty, bg);
}

static void X11_Close(void)
{
	int i, j;

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
	if(hardcopy) psx11_Close();
}

static void X11_StartPath()
{
	int lwd;
	SetColor(GP->col);
	SetLinetype(GP->lty, GP->lwd);
	if(hardcopy) {
		psx11_SetColor(GP->col);
		psx11_SetLinetype(GP->lty);
		psx11_StartPath();
	}
}

static void X11_EndPath()
{
	if(hardcopy) psx11_EndPath();
}

static double xlast;
static double ylast;

static void X11_MoveTo(double x, double y)
{
	if(hardcopy) psx11_MoveTo(x, y);
	xlast = x;
	ylast = y;
}

static void X11_LineTo(double x, double y)
{
	XDrawLine(display, window, wgc, (int)xlast, (int)ylast, (int)x, (int)y);
	if(hardcopy) psx11_LineTo(x, y);
	xlast = x;
	ylast = y;
	XSync(display, 0);
}

static void X11_Rect(double x0, double y0, double x1, double y1, int bg, int fg)
{
	int tmp;
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
		SetColor(bg);
		XFillRectangle(display, window, wgc, (int)x0, (int)y0,
			(int)x1 - (int)x0, (int)y1 - (int)y0);
	}
	if (fg != NA_INTEGER) {
		SetColor(fg);
		SetLinetype(GP->lty, GP->lwd);
		XDrawRectangle(display, window, wgc, (int)x0, (int)y0,
			(int)x1 - (int)x0, (int)y1 - (int)y0);
	}
	XSync(display, 0);
	if(hardcopy) {
		/* psx11_SetColor(GP->col); */
		psx11_SetLinetype(GP->lty);
		psx11_Rect(x0, y0, x1, y1, bg, fg);
	}
}

static void X11_Circle(double x, double y, double r, int col, int border)
{
	int ir, ix, iy;
#ifdef OLD
	ir = ceil(r);
#else
	ir = floor(r + 0.5);
#endif
	ix = (int)x;
	iy = (int)y;
	if(col != NA_INTEGER) {
		SetColor(col);
		XFillArc(display, window, wgc, ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
	}
	if(border != NA_INTEGER) {
		SetLinetype(GP->lty, GP->lwd);
		SetColor(border);
		XDrawArc(display, window, wgc, ix-ir, iy-ir, 2*ir, 2*ir, 0, 23040);
	}
	if(hardcopy) {
		psx11_SetLinetype(GP->lty);
		psx11_Circle(x, y, r, col, border);
	}
}

static void X11_Polygon(int n, double *x, double *y, int bg, int fg)
{
	XPoint *points;
	char *vmax, *vmaxget();
	int i;
	
	vmax = vmaxget();
	if((points=(XPoint*)R_alloc(n+1, sizeof(XPoint))) == NULL)
		error("out of memory while drawing polygon\n");
	for(i=0 ; i<n ; i++) {
		points[i].x = (int)(x[i]);
		points[i].y = (int)(y[i]);
	}
	points[n].x = (int)(x[0]);
	points[n].y = (int)(y[0]);
	if(bg != NA_INTEGER) {
		SetColor(bg);
		XFillPolygon(display, window, wgc, points, n, Complex, CoordModeOrigin);
		XSync(display, 0);
	}
	if(fg != NA_INTEGER) {
		SetColor(fg);
		SetLinetype(GP->lty, GP->lwd);
		XDrawLines(display, window, wgc, points, n+1, CoordModeOrigin);
		XSync(display, 0);
	}
	if(hardcopy) {
		/* psx11_SetColor(GP->col); */
		psx11_SetLinetype(GP->lty);
		psx11_Polygon(n, x, y, bg, fg);
	}
	vmaxset(vmax);
}


static double deg2rad = 0.01745329251994329576;

static void X11_Text(double x, double y, char *str, double xc, double yc, double rot)
{
	int len, size;
	double xl, yl;

	size = GP->cex * GP->ps + 0.5;
	SetFont(GP->font, size);
	SetColor(GP->col);
	len = strlen(str);
	xlast = x;
	ylast = y;
	if(xc != 0.0 || yc != 0) {
		xl = X11_StrWidth(str);
		yl = GP->cex * GP->cra[1];
		x += -xc * xl * cos(deg2rad * rot) + yc * yl * sin(deg2rad * rot);
		y -= -xc * xl * sin(deg2rad * rot) - yc * yl * cos(deg2rad * rot);
	}
	XRotDrawString(display, font, rot, window, wgc, (int)x, (int)y, str);
	XSync(display, 0);
	if(hardcopy) {
		psx11_SetFont(GP->font, size);
		psx11_SetColor(GP->col);
		psx11_Text(xlast, ylast, str, xc, yc, rot);
	}
}

static int X11_Locator(double *x, double *y)
{
	ProcessEvents();	/* discard pending events */
	XSync(display, 1);
	XNextEvent(display, &event);
	if (event.xbutton.button == Button1) {
		*x = event.xbutton.x;
		*y = event.xbutton.y;
		fprintf(stderr, "\07");
		fflush(stderr);
		XSync(display, 0);
		return 1;
	}
	else {
		XSync(display, 0);
		return 0;
	}
}

/* Set Graphics mode - not needed for X11 */
static void X11_Mode(int mode)
{
	if(mode == 0) XSync(display, 0);
}


/* Hold the Picture Onscreen - not needed for X11 */
static void X11_Hold()
{
}


static void X11_SavePlot(char *name)
{
	if(hardcopy) {
		psx11_SavePlot(name);
	}
	else error("no hardcopy available\n");
}


static void X11_PrintPlot(char *name)
{
	if(hardcopy) {
		psx11_PrintPlot();
	}
	else error("no hardcopy available\n");
}


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

int X11DeviceDriver(char **cpars, int ncpars, double *npars, int nnpars)
{
	int ps;
	DevInit = 0;

	if(ncpars != 2 || nnpars != 4)
		error("invalid device parameters (x11)\n");

	/*  Font will load at first use  */

	ps = npars[2];
	if(ps < 6 || ps > 24) ps = 12;
	ps = 2*(ps/2);
	fontface = -1;
	fontsize = -1;
	GP->font = 1;
	GP->ps = ps;

	/*  Start the Device Driver and Hardcopy.  */

	if (!X11_Open(cpars[0], npars[0], npars[1])) return 0;
	hardcopy = psx11_Open(cpars[1], (int)(npars[3]+0.5));
	ProcessEvents();

	/*  Set up Data Structures  */

	DevOpen = X11_Open;
	DevClose = X11_Close;
	DevResize = X11_Resize;
	DevNewPlot = X11_NewPlot;
	DevClip = X11_Clip;
	DevStartPath = X11_StartPath;
	DevEndPath = X11_EndPath;
	DevMoveTo = X11_MoveTo;
	DevLineTo = X11_LineTo;
	DevStrWidth = X11_StrWidth;
	DevText = X11_Text;
	DevRect = X11_Rect;
	DevCircle = X11_Circle;
	DevPolygon = X11_Polygon;
	DevLocator = X11_Locator;
	DevMode = X11_Mode;
	DevHold = X11_Hold;
	DevSavePlot = X11_SavePlot;
	DevPrintPlot = X11_PrintPlot;
	DevMetricInfo = X11_MetricInfo;

	/* Window Dimensions in Pixels */

	GP->left = 0;			/* left */
	GP->right = windowWidth;	/* right */
	GP->bottom = windowHeight;	/* bottom */
	GP->top = 0;			/* top */

	/* Nominal Character Sizes in Pixels */

	GP->cra[0] = font->max_bounds.rbearing - font->min_bounds.lbearing;
	GP->cra[1] = font->max_bounds.ascent + font->max_bounds.descent;

	/* Character Addressing Offsets */
	/* These are used to plot a single plotting character */
	/* so that it is exactly over the plotting point */

	GP->xCharOffset = 0.4900;
	GP->yCharOffset = 0.3333;
	GP->yLineBias = 0.1;

	/* Inches per raster unit */

	GP->ipr[0] = pixelWidth();
	GP->ipr[1] = pixelHeight();

	/* Device capabilities */
	/* Clipping is problematic for X11 */
	/* Graphics is clipped, text is not */

	GP->canResizePlot = 1;
	GP->canChangeFont = 0;
	GP->canRotateText = 1;
	GP->canResizeText = 1;
	GP->canClip = 1;

	DevInit = 1;
	cex = 1.0;
	lty = 0;
	xlast = 250;
	ylast = 250;
	return 1;
}
