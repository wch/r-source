/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-4 The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* Used by third-party graphics devices */

/* The graphics engine will only accept locations and dimensions 
 * in native device coordinates, but it provides the following functions
 * for converting between a couple of simple alternative coordinate 
 * systems and device coordinates:
 *    DEVICE = native units of the device
 *    NDC = Normalised device coordinates 
 *    INCHES = inches (!)
 *    CM = centimetres (!!)
 */

typedef enum {
 GE_DEVICE	= 0,	/* native device coordinates (rasters) */
 GE_NDC	= 1,	/* normalised device coordinates x=(0,1), y=(0,1) */
 GE_INCHES = 2,
 GE_CM     = 3
} GEUnit;

#define MAX_GRAPHICS_SYSTEMS 24

typedef enum {
    /* In response to this event, the registered graphics system
     * should allocate and initialise the systemSpecific structure
     */
    GE_InitState = 0,
    /* This event gives the registered system a chance to undo
     * anything done in the initialisation.
     */
    GE_FinaliseState = 1,
    /* This is sent by the graphics engine prior to initialising
     * the display list.  It give the graphics system the chance
     * to squirrel away information it will need for redrawing the
     * the display list
     */
    GE_SaveState = 2,
    /* This is sent by the graphics engine prior to replaying the
     * display list.  It gives the graphics system the chance to
     * restore any information it saved on the GE_SaveState event
     */
    GE_RestoreState = 6,
    /* Copy system state information to the current device.
     * This is used when copying graphics from one device to another
     * so all the graphics system needs to do is to copy across
     * the bits required for the display list to draw faithfully
     * on the new device.
     */
    GE_CopyState = 3,
    /* Create a snapshot of the system state that is sufficient
     * for the current "image" to be reproduced
     */
    GE_SaveSnapshotState = 4,
    /* Restore the system state that is saved by GE_SaveSnapshotState
     */
    GE_RestoreSnapshotState = 5,
    /* When replaying the display list, the graphics engine 
     * checks, after each replayed action, that the action 
     * produced valid output.  This is the graphics system's
     * chance to say that the output is crap (in which case the
     * graphics engine will abort the display list replay).
     */
    GE_CheckPlot = 7,
    /* The device wants to scale the current pointsize
     * (for scaling an image)
     * This is not a nice general solution, but a quick fix for 
     * the Windows device.
     */
    GE_ScalePS = 8
} GEevent;

typedef struct _GEDevDesc GEDevDesc;

typedef SEXP (* GEcallback)(GEevent, GEDevDesc *, SEXP);

typedef struct { 
    /* An array of information about each graphics system that
     * has registered with the graphics engine.
     * This is used to store graphics state for each graphics
     * system on each device.
     */
    void *systemSpecific;
    /* 
     * An array of function pointers, one per graphics system that
     * has registered with the graphics engine.
     *
     * system_Callback is called when the graphics engine wants
     * to give a graphics system the chance to play with its
     * device-specific information (stored in systemSpecific)
     * There are two parameters:  an "event" to tell the graphics
     * system why the graphics engine has called this function,
     * and the systemSpecific pointer.  The graphics engine
     * has to pass the systemSpecific pointer because only
     * the graphics engine will know what array index to use.
     */
    GEcallback callback;
} GESystemDesc;

struct _GEDevDesc {
    int newDevStruct;
    /* 
     * Stuff that the devices can see (and modify).
     * All detailed in GraphicsDevice.h
     */
    NewDevDesc *dev;
    /*
     * Stuff about the device that only the graphics engine sees
     * (the devices don't see it).
     * Display list stuff should come here from NewDevDesc struct.
     */
    Rboolean dirty;  /* Has the device received any output? */
    /* 
     * Stuff about the device that only graphics systems see.
     * The graphics engine has no idea what is in here.
     * Used by graphics systems to store system state per device.
     */
    GESystemDesc *gesd[MAX_GRAPHICS_SYSTEMS];
};

/*
 *  Some line end/join constants
 */
typedef enum {
  GE_ROUND_CAP  = 1,
  GE_BUTT_CAP   = 2,
  GE_SQUARE_CAP = 3
} R_GE_lineend;

typedef enum {
  GE_ROUND_JOIN = 1,
  GE_MITRE_JOIN = 2,
  GE_BEVEL_JOIN = 3
} R_GE_linejoin;

/* 
 * A structure containing graphical parameters 
 *
 * This is how graphical parameters are passed from graphics systems
 * to the graphics engine AND from the graphics engine to graphics
 * devices.
 *
 * Devices are not *required* to honour graphical parameters
 * (e.g., alpha transparency is going to be tough for some)
 */
typedef struct {
    /*
     * Colours
     *
     * NOTE:  Alpha transparency included in col & fill
     */
    int col;             /* pen colour (lines, text, borders, ...) */
    int fill;            /* fill colour (for polygons, circles, rects, ...) */
    double gamma;        /* Gamma correction */
    /* 
     * Line characteristics
     */
    double lwd;          /* Line width (roughly number of pixels) */
    int lty;             /* Line type (solid, dashed, dotted, ...) */
    R_GE_lineend lend;   /* Line end */
    R_GE_linejoin ljoin; /* line join */
    double lmitre;       /* line mitre */
    /*
     * Text characteristics
     */
    double cex;          /* Character expansion (font size = fontsize*cex) */
    double ps;           /* Font size in points */
    double lineheight;   /* Line height (multiply by font size) */
    int fontface;        /* Font face (plain, italic, bold, ...) */
    char fontfamily[50]; /* Font family */
} R_GE_gcontext;

GEDevDesc* GEcreateDevDesc(NewDevDesc* dev);
void GEdestroyDevDesc(GEDevDesc* dd);
void* GEsystemState(GEDevDesc *dd, int index);
void GEregisterWithDevice(GEDevDesc *dd);
void GEregisterSystem(GEcallback callback, int *systemRegisterIndex);
void GEunregisterSystem(int registerIndex);

SEXP GEHandleEvent(GEevent event, NewDevDesc *dev, SEXP data);

double fromDeviceX(double value, GEUnit to, GEDevDesc *dd); 
double toDeviceX(double value, GEUnit from, GEDevDesc *dd);
double fromDeviceY(double value, GEUnit to, GEDevDesc *dd); 
double toDeviceY(double value, GEUnit from, GEDevDesc *dd);
double fromDeviceWidth(double value, GEUnit to, GEDevDesc *dd); 
double toDeviceWidth(double value, GEUnit from, GEDevDesc *dd);
double fromDeviceHeight(double value, GEUnit to, GEDevDesc *dd); 
double toDeviceHeight(double value, GEUnit from, GEDevDesc *dd);

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
 *		for(i=0 ; i<8 && newlty & 15 ; i++) {
 *			dashlist[ndash++] = newlty & 15;
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
 */

/*--- The basic numbered & names line types; Here device-independent:
  e.g. "dashed" == "44",  "dotdash" == "1343"
*/

#define LTY_BLANK	-1
#define LTY_SOLID	0
#define LTY_DASHED	4 + (4<<4)
#define LTY_DOTTED	1 + (3<<4)
#define LTY_DOTDASH	1 + (3<<4) + (4<<8) + (3<<12)
#define LTY_LONGDASH	7 + (3<<4)
#define LTY_TWODASH	2 + (2<<4) + (6<<8) + (2<<12)

R_GE_lineend LENDpar(SEXP value, int ind);
SEXP LENDget(R_GE_lineend lend);
R_GE_linejoin LJOINpar(SEXP value, int ind);
SEXP LJOINget(R_GE_linejoin ljoin);

void GESetClip(double x1, double y1, double x2, double y2, GEDevDesc *dd);
void GENewPage(R_GE_gcontext *gc, GEDevDesc *dd);
void GELine(double x1, double y1, double x2, double y2, 
	    R_GE_gcontext *gc, GEDevDesc *dd);
void GEPolyline(int n, double *x, double *y, 
		R_GE_gcontext *gc, GEDevDesc *dd);
void GEPolygon(int n, double *x, double *y, 
	       R_GE_gcontext *gc, GEDevDesc *dd);
void GECircle(double x, double y, double radius,
	      R_GE_gcontext *gc, GEDevDesc *dd);
void GERect(double x0, double y0, double x1, double y1,
	    R_GE_gcontext *gc, GEDevDesc *dd);
void GEText(double x, double y, char *str,
	    double xc, double yc, double rot,
	    R_GE_gcontext *gc, GEDevDesc *dd);
void GEMode(int mode, GEDevDesc* dd);
void GESymbol(double x, double y, int pch, double size,
	      R_GE_gcontext *gc, GEDevDesc *dd);
void GEPretty(double *lo, double *up, int *ndiv);
void GEMetricInfo(int c, R_GE_gcontext *gc, 
		  double *ascent, double *descent, double *width,
		  GEDevDesc *dd);
double GEStrWidth(char *str, 
		  R_GE_gcontext *gc, GEDevDesc *dd);
double GEStrHeight(char *str, 
		  R_GE_gcontext *gc, GEDevDesc *dd);

/* 
 * From plotmath.c 
 */
double GEExpressionWidth(SEXP expr, 
			 R_GE_gcontext *gc, GEDevDesc *dd);
double GEExpressionHeight(SEXP expr, 
			  R_GE_gcontext *gc, GEDevDesc *dd);
void GEMathText(double x, double y, SEXP expr,
		double xc, double yc, double rot, 
		R_GE_gcontext *gc, GEDevDesc *dd);
/* 
 * (End from plotmath.c)
 */

/* 
 * From plot3d.c 
 */
SEXP GEcontourLines(double *x, int nx, double *y, int ny,
		    double *z, double *levels, int nl,
		    GEDevDesc *dd);
/* 
 * (End from plot3d.c)
 */

/* 
 * From vfonts.c
 */
typedef void (*R_GE_VTextRoutine)(double x, double y, char *s, 
				  double x_justify, double y_justify, 
				  double rotation,
				  R_GE_gcontext *gc, GEDevDesc *dd);

typedef double (*R_GE_VStrWidthRoutine)(const unsigned char *s, 
					R_GE_gcontext *gc, GEDevDesc *dd);

typedef double (*R_GE_VStrHeightRoutine)(const unsigned char *s, 
					 R_GE_gcontext *gc, GEDevDesc *dd);

void R_GE_setVFontRoutines(R_GE_VStrWidthRoutine vwidth, 
			   R_GE_VStrHeightRoutine vheight, 
			   R_GE_VTextRoutine vtext);

double R_GE_VStrWidth(const unsigned char *s, 
		      R_GE_gcontext *gc, GEDevDesc *dd);

double R_GE_VStrHeight(const unsigned char *s, 
		       R_GE_gcontext *gc, GEDevDesc *dd);

void R_GE_VText(double x, double y, char *s, 
		double x_justify, double y_justify, double rotation,
		R_GE_gcontext *gc, GEDevDesc *dd);
/* 
 * (End from vfonts.c)
 */

#define	DEG2RAD 0.01745329251994329576

GEDevDesc* GEcurrentDevice();
Rboolean GEdeviceDirty(GEDevDesc *dd);
void GEdirtyDevice(GEDevDesc *dd);
Rboolean GEcheckState(GEDevDesc *dd);
void GErecordGraphicOperation(SEXP op, SEXP args, GEDevDesc *dd);
void GEinitDisplayList(GEDevDesc *dd);
void GEplayDisplayList(GEDevDesc *dd);
void GEcopyDisplayList(int fromDevice);
SEXP GEcreateSnapshot(GEDevDesc *dd);
void GEplaySnapshot(SEXP snapshot, GEDevDesc* dd);
