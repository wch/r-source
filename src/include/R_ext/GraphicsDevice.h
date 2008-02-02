/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8 The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* Used by third-party graphics devices.
 *
 * This defines NewDevDesc, whereas GraphicsEngine.h defines GEDevDesc.
 * Also contains entry points from gevents.c
 */

#ifndef R_GRAPHICSDEVICE_H_
#define R_GRAPHICSDEVICE_H_

/* until 2.7.0 alpha */
#define OLD 1

/* ideally we would use prototypes in NewDevDesc.  
   But the pre-1.4.0 system used DevDesc*, and some devices
   have taken to passing pointers to their own structure
   instead of NewDevDesc* , defining R_USE_PROTOTYPES 0 allows them to
   opt out.
*/

#ifndef  R_USE_PROTOTYPES
# ifdef OLD
#  define R_USE_PROTOTYPES 0
# else
#  define R_USE_PROTOTYPES 1
/* This looks recursive, but the guards will force
 * GraphicsEngine.h and then this file (and no more)
 */
#  include <R_ext/GraphicsEngine.h> /* needed for R_GE_gcontext */
# endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* --------- New (in 1.4.0) device driver structure ---------
 * NOTES:
 * 1. All locations and dimensions are in device coordinates.
 * 2. I found this comment in the doc for dev_Open -- looks nasty
 *    Any known instances of such a thing happening?  Should be
 *    replaced by a function to query the device for preferred gpars
 *    settings? (to be called when the device is initialised)
         *
         * NOTE that it is perfectly acceptable for this
         * function to set generic graphics parameters too
         * (i.e., override the generic parameter settings
         * which GInit sets up) all at the author's own risk
         * of course :)
	 *
 * 3. Do we really need dev_StrWidth as well as dev_MetricInfo?
 *    I can see the difference between the two -- its just a
 *    question of whether dev_MetricInfo should just return
 *    what dev_StrWidth would give if font metric information is
 *    not available.  I guess having both allows the developer
 *    to decide when to ask for which sort of value, and to decide
 *    what to do when font metric information is not available.
 *    And why not a dev_StrHeight?
 * 4. Should "ipr", "asp", and "cra" be in the device description?
 *    If not, then where?
 *    I guess they don't need to be if no device makes use of them.
 *    On the other hand, they would need to be replaced by a device
 *    call that R base graphics could use to get enough information
 *    to figure them out.  (e.g., some sort of dpi() function to
 *    complement the size() function.)
 */

typedef struct _NewDevDesc NewDevDesc;
typedef NewDevDesc* pDevDesc;

struct _NewDevDesc {
#ifdef OLD
    /* The first element is a boolean indicating whether this is
     * a new device driver (always 1) -- the old device driver structure
     * has had a similar element added (which will always be 0)
     *
     * This needs to be removed once the old DevDesc structure has been
     * removed from the system.
     */
    int newDevStruct;
#endif
    /********************************************************
     * Device physical characteristics
     ********************************************************/
    double left;	        /* left raster coordinate */
    double right;	        /* right raster coordinate */
    double bottom;	        /* bottom raster coordinate */
    double top;		        /* top raster coordinate */
    /* R only has the notion of a rectangular clipping region
     */
    double clipLeft;
    double clipRight;
    double clipBottom;
    double clipTop;
    /* I hate these next three -- they seem like a real fudge
     * BUT I'm not sure what to replace them with so they stay for now.
     */
    double xCharOffset;	        /* x character addressing offset */
    double yCharOffset;	        /* y character addressing offset */
    double yLineBias;	        /* 1/2 interline space as frac of line hght */
    double ipr[2];	        /* Inches per raster; [0]=x, [1]=y */
    /* It seems this is unused, and unset by all(?) devices */
    double asp;		        /* Pixel aspect ratio = ipr[1]/ipr[0] */
    /* I hate this guy too -- seems to assume that a device can only
     * have one font size during its lifetime
     * BUT removing/replacing it would take quite a lot of work
     * to design and insert a good replacement so it stays for now.
     */
    double cra[2];	        /* Character size in rasters; [0]=x, [1]=y */
    double gamma;	        /* Device Gamma Correction */
    /********************************************************
     * Device capabilities
     ********************************************************/
    Rboolean canResizePlot;	/* can the graphics surface be resized */
    Rboolean canChangeFont;	/* device has multiple fonts */
    Rboolean canRotateText;	/* text can be rotated */
    Rboolean canResizeText;	/* text can be resized */
    Rboolean canClip;		/* Hardware clipping */
    Rboolean canChangeGamma;    /* can the gamma factor be modified */
    int canHAdj;	        /* Can do at least some horiz adjust of text
			           0 = none, 1 = {0,0.5, 1}, 2 = [0,1] */
    /********************************************************
     * Device initial settings
     ********************************************************/
    /* These are things that the device must set up when it is created.
     * The graphics system can modify them and track current values,
     * but some devices want to know what the original setting was.
     */
    double startps;
    int startcol;
    int startfill;
    int startlty;
    int startfont;
    double startgamma;
    /********************************************************
     * Device specific information
     ********************************************************/
    void *deviceSpecific;	/* pointer to device specific parameters */
    /********************************************************
     * Device display list
     ********************************************************/
    /* I think it would feel nicer if this stuff was part of the
     * graphics engine (GEDevDesc), but this is another thing that
     * needs more time to implement a change properly.
     */
    Rboolean displayListOn;     /* toggle for display list status */
    SEXP displayList;           /* display list */
    SEXP DLlastElt;
    SEXP savedSnapshot;         /* The last value of the display list
				 * just prior to when the display list
				 * was last initialised
				 */

    /********************************************************
     * Event handling entries
     ********************************************************/

    /* These determine whether getGraphicsEvent will try to set an event handler */

    Rboolean canGenMouseDown; /* can the device generate mousedown events */
    Rboolean canGenMouseMove; /* can the device generate mousemove events */
    Rboolean canGenMouseUp;   /* can the device generate mouseup events */
    Rboolean canGenKeybd;     /* can the device generate keyboard events */

    Rboolean gettingEvent;    /* This is set while getGraphicsEvent
				 is actively looking for events */

    /********************************************************
     * Device procedures.
     ********************************************************/

    /*
     * ---------------------------------------
     * GENERAL COMMENT ON GRAPHICS PARAMETERS:
     * ---------------------------------------
     * Graphical parameters are now passed in a graphics context
     * structure (R_GE_gcontext*) rather than individually.
     * Each device action should extract the parameters it needs
     * and ignore the others.  Thought should be given to which
     * parameters are relevant in each case -- the graphics engine
     * does not REQUIRE that each parameter is honoured, but if
     * a parameter is NOT honoured, it might be a good idea to
     * issue a warning when a parameter is not honoured (or at
     * the very least document which parameters are not honoured
     * in the user-level documentation for the device).  [An example
     * of a parameter that may not be honoured by many devices is
     * transparency.]
     */

    /*
     * device_Activate is called when a device becomes the
     * active device.  For example, it can be used to change the
     * title of a window to indicate the active status of
     * the device to the user.  Not all device types will
     * do anything.
     * The only parameter is a device driver structure.
     * An example is ...
     *
     * static void   X11_Activate(pDevDesc dd);
     *
     */
#if R_USE_PROTOTYPES
    void (*activate)(pDevDesc );
#else
    void (*activate)();
#endif
    /*
     * device_Circle should have the side-effect that a
     * circle is drawn, centred at the given location, with
     * the given radius.  The border of the circle should be
     * drawn in the given "col", and the circle should be
     * filled with the given "fill" colour.
     * If "col" is NA_INTEGER then no border should be drawn
     * If "fill" is NA_INTEGER then the circle should not
     * be filled.
     * An example is ...
     *
     * static void X11_Circle(double x, double y, double r,
     *                        R_GE_gcontext *gc,
     *                        pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   col, fill, gamma, lty, lwd
     */
#if R_USE_PROTOTYPES
    void (*circle)(double x, double y, double r,
		   R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*circle)();
#endif
    /*
     * device_Clip is given the left, right, bottom, and
     * top of a rectangle (in DEVICE coordinates).
     * It should have the side-effect that subsequent output
     * is clipped to the given rectangle.
     * NOTE that R's graphics engine already clips to the
     * extent of the device.
     * NOTE also that this will probably only be called if
     * the flag canClip is true.
     * An example is ...
     *
     * static void X11_Clip(double x0, double x1, double y0, double y1,
     *                      pDevDesc dd)
     */
#if R_USE_PROTOTYPES
    void (*clip)(double x0, double x1, double y0, double y1, pDevDesc dd);
#else
    void (*clip)();
#endif
    /*
     * device_Close is called when the device is killed.
     * This function is responsible for destroying any
     * device-specific resources that were created in
     * device_Open and for FREEing the device-specific
     * parameters structure.
     * An example is ...
     *
     * static void X11_Close(pDevDesc dd)
     *
     */
#if R_USE_PROTOTYPES
    void (*close)();
#else
    void (*close)();
#endif
    /*
     * device_Deactivate is called when a device becomes
     * inactive.
     * This allows the device to undo anything it did in
     * dev_Activate.
     * Not all device types will do anything.
     * An example is ...
     *
     * static void X11_Deactivate(pDevDesc dd)
     *
     */
#if R_USE_PROTOTYPES
    void (*deactivate)(pDevDesc );
#else
    void (*deactivate)();
#endif

#ifdef OLD
    void (*dot)();
    /*
     * I don't know what this is for and i can't find it
     * being used anywhere, but i'm loath to kill it in
     * case i'm missing something important
     * An example is ...
     *
     * static void X11_Hold(pDevDesc dd)
     *
     */

#if R_USE_PROTOTYPES
    void (*hold)(pDevDesc );
#else
    void (*hold)();
#endif
#endif

    /*
     * device_Locator should return the location of the next
     * mouse click (in DEVICE coordinates)
     * Not all devices will do anything (e.g., postscript)
     * An example is ...
     *
     * static Rboolean X11_Locator(double *x, double *y, pDevDesc dd)
     *
     */
#if R_USE_PROTOTYPES
    Rboolean (*locator)(double *x, double *y, pDevDesc dd);
#else
    Rboolean (*locator)();
#endif
    /*
     * device_Line should have the side-effect that a single
     * line is drawn (from x1,y1 to x2,y2)
     * An example is ...
     *
     * static void X11_Line(double x1, double y1, double x2, double y2,
     *                      R_GE_gcontext *gc,
     *                      pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   col, gamma, lty, lwd
     */
#if R_USE_PROTOTYPES
    void (*line)(double x1, double y1, double x2, double y2,
		 R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*line)();
#endif
    /*
     * device_MetricInfo should return height, depth, and
     * width information for the given character in DEVICE
     * units.
     * Note: in an 8-bit locale, c is 'char'.
     * In an mbcslocale, it is wchar_t, and at least some
     * of code assumes that is UCS-2 (Windows, true) or UCS-4.
     * This is used for formatting mathematical expressions
     * and for exact centering of text (see GText)
     * If the device cannot provide metric information then
     * it MUST return 0.0 for ascent, descent, and width.
     * An example is ...
     *
     * static void X11_MetricInfo(int c,
     *                            R_GE_gcontext *gc,
     *                            double* ascent, double* descent,
     *                            double* width, pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   font, cex, ps
     */
#if R_USE_PROTOTYPES
    void (*metricInfo)(int c, R_GE_gcontext *gc,
		       double* ascent, double* descent, double* width,
		       pDevDesc dd);
#else
    void (*metricInfo)();
#endif
    /*
     * device_Mode is called whenever the graphics engine
     * starts drawing (mode=1) or stops drawing (mode=0)
     * The device is not required to do anything
     * An example is ...
     *
     * static void X11_Mode(int mode, pDevDesc dd);
     *
     */
#if R_USE_PROTOTYPES
    void (*mode)(int mode, pDevDesc dd);
#else
    void (*mode)();
#endif
    /*
     * device_NewPage is called whenever a new plot requires
     * a new page.
     * A new page might mean just clearing the
     * device (e.g., X11) or moving to a new page
     * (e.g., postscript)
     * An example is ...
     *
     *
     * static void X11_NewPage(R_GE_gcontext *gc,
     *                         pDevDesc dd);
     *
     */
#if R_USE_PROTOTYPES
    void (*newPage)(R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*newPage)();
#endif
    /*
     * device_Open is not called directly by the
     * graphics engine;  it is only called from
     * the device-driver entry point.
     *
     * Its presence is a historical anomaly, and it should
     * no longer be set.
     *
     * This function should set up all of the device-
     * specific resources for a new device
     * This function is given a newly-allocated NewDevDesc structure
     * and it must FREE the structure if anything goes seriously wrong.
     * NOTE that different devices will accept different parameter
     * lists, corresponding to different device-specific preferences.
     * An example is ...
     *
     * Rboolean X11_Open(pDevDesc dd, const char *dsp, double w, double h,
     *                   double gamma_fac, X_COLORTYPE colormodel,
     *                   int maxcube, int canvascolor);
     *
     */
    Rboolean (*open)(/* args are device-specific */);
    /*
     * device_Polygon should have the side-effect that a
     * polygon is drawn using the given x and y values
     * the polygon border should be drawn in the "col"
     * colour and filled with the "fill" colour.
     * If "col" is NA_INTEGER don't draw the border
     * If "fill" is NA_INTEGER don't fill the polygon
     * An example is ...
     *
     * static void X11_Polygon(int n, double *x, double *y,
     *                         R_GE_gcontext *gc,
     *                         pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   col, fill, gamma, lty, lwd
     */
#if R_USE_PROTOTYPES
    void (*polygon)(int n, double *x, double *y, R_GE_gcontext *gc,
		    pDevDesc dd);
#else
    void (*polygon)();
#endif
    /*
     * device_Polyline should have the side-effect that a
     * series of line segments are drawn using the given x
     * and y values.
     * An example is ...
     *
     * static void X11_Polyline(int n, double *x, double *y,
     *                          R_GE_gcontext *gc,
     *                          pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   col, gamma, lty, lwd
     */
#if R_USE_PROTOTYPES
    void (*polyline)(int n, double *x, double *y, R_GE_gcontext *gc,
		     pDevDesc dd);
#else
    void (*polyline)();
#endif
    /*
     * device_Rect should have the side-effect that a
     * rectangle is drawn with the given locations for its
     * opposite corners.  The border of the rectangle
     * should be in the given "col" colour and the rectangle
     * should be filled with the given "fill" colour.
     * If "col" is NA_INTEGER then no border should be drawn
     * If "fill" is NA_INTEGER then the rectangle should not
     * be filled.
     * An example is ...
     *
     * static void X11_Rect(double x0, double y0, double x1, double y1,
     *                      R_GE_gcontext *gc,
     *                      pDevDesc dd);
     *
     */
#if R_USE_PROTOTYPES
    void (*rect)(double x0, double y0, double x1, double y1,
		 R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*rect)();
#endif
    /*
     * device_Size is called whenever the device is
     * resized.
     * The function returns (left, right, bottom, and top) for the
     * new device size.
     * This is not usually called directly by the graphics
     * engine because the detection of device resizes
     * (e.g., a window resize) are usually detected by
     * device-specific code.
     * An example is ...
     *
     * static void X11_Size(double *left, double *right,
     *                      double *bottom, double *top,
     *                      pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   col, fill, gamma, lty, lwd
     */
#if R_USE_PROTOTYPES
    void (*size)(double *left, double *right, double *bottom, double *top,
		 pDevDesc dd);
#else
    void (*size)();
#endif
    /*
     * device_StrWidth should return the width of the given
     * string in DEVICE units.
     * An example is ...
     *
     * static double X11_StrWidth(const char *str,
     *                            R_GE_gcontext *gc,
     *                            pDevDesc dd)
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   font, cex, ps
     */
#if R_USE_PROTOTYPES
    double (*strWidth)(const char *str, R_GE_gcontext *gc, pDevDesc dd);
#else
    double (*strWidth)();
#endif
    /*
     * device_Text should have the side-effect that the
     * given text is drawn at the given location.
     * The text should be rotated according to rot (degrees)
     * An example is ...
     *
     * static void X11_Text(double x, double y, const char *str,
     *                      double rot, double hadj,
     *                      R_GE_gcontext *gc,
     * 	                    pDevDesc dd);
     *
     * R_GE_gcontext parameters that should be honoured (if possible):
     *   font, cex, ps, col, gamma
     */
#if R_USE_PROTOTYPES
    void (*text)(double x, double y, const char *str, double rot,
		 double hadj, R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*text)();
#endif
    /*
     * device_onExit is called by GEonExit when the user has aborted
     * some operation, and so an R_ProcessEvents call may not return normally.
     * It need not be set to any value; if null, it will not be called.
     *
     * An example is ...
     *
     * static void X11_onExit(pDevDesc dd);
    */
#if R_USE_PROTOTYPES
    void (*onExit)(pDevDesc dd);
#else
    void (*onExit)();
#endif
    /*
     * device_getEvent is called by do_getGraphicsEvent to get a modal
     * graphics event.  It should call R_ProcessEvents() until one
     * of the event handlers sets eventResult to a non-null value,
     * and then return it
     * An example is ...
     *
     * static SEXP GA_getEvent(SEXP eventRho, const char *prompt);
     */
    SEXP (*getEvent)(SEXP, const char *);

    /* Optional features introduced in 2.7.0 */
    /* Some devices can plot UTF-8 text directly without converting
       to the native encoding.  E.g. windows().
       If the flag is TRUE, the metricInfo entry point should
       accept negative values for 'c' and treat them as indicating
       Unicode points (as well as postive values in a MBCS locale).
    */
    Rboolean hasTextUTF8; /* and strWidthUTF8 */
#if R_USE_PROTOTYPES
    void (*textUTF8)(double x, double y, const char *str, double rot,
		     double hadj, R_GE_gcontext *gc, pDevDesc dd);
    double (*strWidthUTF8)(const char *str, R_GE_gcontext *gc, pDevDesc dd);
#else
    void (*textUTF8)();
    double (*strWidthUTF8)();
#endif

    /* Is rotated text good enough to be preferable to Hershey in
       contour labels?
    */
    Rboolean useRotatedTextInContour;
};

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

/* moved from Rgraphics.h */

/*
 *	Some Notes on Color
 *
 *	R uses a 24-bit color model.  Colors are specified in 32-bit
 *	integers which are partitioned into 4 bytes as follows.
 *
 *		<-- most sig	    least sig -->
 *		+-------------------------------+
 *		|   0	| blue	| green |  red	|
 *		+-------------------------------+
 *
 *	The red, green and blue bytes can be extracted as follows.
 *
 *		red   = ((color	     ) & 255)
 *		green = ((color >>  8) & 255)
 *		blue  = ((color >> 16) & 255)
 */
/*
 *	Changes as from 1.4.0: use top 8 bits as an alpha channel.
 * 	0 = opaque, 255 = transparent.
 *	At present only 0 and >0 are used, with no semi-transparent.
 */
/*
 * Changes as from 2.0.0:  use top 8 bits as full alpha channel
 *      1 = opaque, 0 = transparent
 *      [to conform with SVG, PDF and others]
 *      and everything in between is used
 *      [which means that NA is not stored as an internal colour;
 *       it is converted to R_RGBA(255, 255, 255, 0)]
 */

#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16)|0xFF000000)
#define R_RGBA(r,g,b,a)	((r)|((g)<<8)|((b)<<16)|((a)<<24))
#define R_RED(col)	(((col)	   )&255)
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)
#define R_ALPHA(col)	(((col)>>24)&255)
#define R_OPAQUE(col)	(R_ALPHA(col) == 255)
#define R_TRANSPARENT(col) (R_ALPHA(col) == 0)
    /*
     * A transparent white
     */
#define R_TRANWHITE     (R_RGBA(255, 255, 255, 0))


/* used in various devices */

typedef unsigned int rcolor;

#define curDevice		Rf_curDevice
#define killDevice		Rf_killDevice
#define ndevNumber		Rf_ndevNumber
#define NewFrameConfirm		Rf_NewFrameConfirm
#define nextDevice		Rf_nextDevice
#define NoDevices		Rf_NoDevices
#define NumDevices		Rf_NumDevices
#define prevDevice		Rf_prevDevice
#define selectDevice		Rf_selectDevice

/* Properly declared version of devNumber */
int ndevNumber(pDevDesc );

/* Formerly in Rdevices.h */

/* How many devices exist ? (>= 1) */
int NumDevices(void);

/* Check for an available device slot */
void R_CheckDeviceAvailable(void);
Rboolean R_CheckDeviceAvailableBool(void);

/* Return the number of the current device. */
int curDevice(void);

/* Return the number of the next device. */
int nextDevice(int);

/* Return the number of the previous device. */
int prevDevice(int);

/* Make the specified device (specified by number) the current device */
int selectDevice(int);

/* Kill device which is identified by number. */
void killDevice(int);

int NoDevices(void); /* used in engine, graphics, plot, grid */

void NewFrameConfirm(void); /* used in graphics.c, grid */


/* Graphics events: defined in gevents.c */

/* These give the indices of some known keys */

typedef enum {knUNKNOWN = -1,
              knLEFT = 0, knUP, knRIGHT, knDOWN,
              knF1, knF2, knF3, knF4, knF5, knF6, knF7, knF8, knF9, knF10,
              knF11, knF12,
              knPGUP, knPGDN, knEND, knHOME, knINS, knDEL} R_KeyName;

/* These are the three possible mouse events */

typedef enum {meMouseDown = 0,
	      meMouseUp,
	      meMouseMove} R_MouseEvent;

#define leftButton   1
#define middleButton 2
#define rightButton  4

#define doKeybd			Rf_doKeybd
#define doMouseEvent		Rf_doMouseEvent

SEXP doMouseEvent(SEXP eventRho, pDevDesc dd, R_MouseEvent event,
                  int buttons, double x, double y);
SEXP doKeybd(SEXP eventRho, pDevDesc dd, R_KeyName rkey,
	     const char *keyname);


#ifdef __cplusplus
}
#endif

#endif /* R_GRAPHICSDEVICE_ */
