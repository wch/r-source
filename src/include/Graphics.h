/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--1999  R Development Core Team
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

#ifndef GRAPHICS_H_
#define GRAPHICS_H_

#include "Defn.h"

#ifndef NA_REAL
#define NA_REAL R_NaReal
#endif

#ifndef NA_INTEGER
#define NA_INTEGER R_NaInt
#endif

#include <math.h>

#ifdef Windows
#include <windows.h>
#endif

#include <float.h>
#include <stdlib.h>
#include <stdio.h>

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
#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16))
#define R_RED(col)	(((col)	   )&255)
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)
#define COLOR_TABLE_SIZE 1024

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

#define MAX_LAYOUT_ROWS 15
#define MAX_LAYOUT_COLS 15

	/* possible coordinate systems (for specifying locations) */

#define DEVICE	0	/* native device coordinates (rasters) */
#define NDC	1	/* normalised device coordinates x=(0,1), y=(0,1) */
#define INCHES 13	/* inches x=(0,width), y=(0,height) */
#define NIC	6	/* normalised inner region coordinates (0,1) */
#define OMA1	2	/* outer margin 1 (bottom) x=NIC, y=LINES */
#define OMA2	3	/* outer margin 2 (left) */
#define OMA3	4	/* outer margin 3 (top) */
#define OMA4	5	/* outer margin 4 (right) */
#define NFC	7	/* normalised figure region coordinates (0,1) */
#define NPC	16	/* normalised plot region coordinates (0,1) */
#define USER	12	/* user/data/world corrdinates */
			/* x=(xmin,xmax), y=(ymin,ymax) */
#define MAR1	8	/* figure margin 1 (bottom) x=USER(x), y=LINES */
#define MAR2	9	/* figure margin 2 (left) x=USER(y) y=LINES */
#define MAR3	10	/* figure margin 3 (top) x=USER(x), y=LINES */
#define MAR4	11	/* figure margin 4 (right) x=USER(y) y=LINES */

	/* possible units (for specifying dimensions) */
	/* all of the above, plus ... */

#define LINES 14	/* multiples of a line in the margin (mex) */
#define CHARS 15	/* multiples of text height (cex) */

#define R_MaxDevices 64

#define	DEG2RAD 0.01745329251994329576

typedef unsigned int rcolor;

typedef struct {
	double ax;
	double bx;
	double ay;
	double by;
} GTrans;

struct colorDataBaseEntry {
	char *name;	/* X11 Color Name */
	char *rgb;	/* #RRGGBB String */
	unsigned int code;  /* Internal R Color Code */
};

typedef struct colorDataBaseEntry ColorDataBaseEntry;

extern int R_ColorTableSize;
extern unsigned int R_ColorTable[];
extern ColorDataBaseEntry ColorDataBase[];
extern char *DefaultPalette[];

/* Graphics State:
 *
 * The following structure defines state for a graphics device driver.
 * Two copies are kept; the ``default'' set of values, and a set which
 * can be modified during calls to an application program.  When a
 * new graphics frame is started, the values revert to the defaults
 *
 */

typedef struct {
    /* Basic Device Driver Properties */
    /* These MUST be set by device drivers on open */

    /* These parameters cannot be set by the user */
    /* although left, right, bottom, and top can be */
    /* interrogated indirectly (i.e., par("din")) */
    /* and cra can be interrogated directly (i.e., par("cra")) */

    double left;	/* left raster coordinate */
    double right;	/* right raster coordinate */
    double bottom;	/* bottom raster coordinate */
    double top;		/* top raster coordinate */
    double xCharOffset;	/* x character addressing offset */
    double yCharOffset;	/* y character addressing offset */
    double yLineBias;	/* 1/2 interline space as fraction of line height */
    int canResizePlot;	/* can the graphics surface be resized */
    int canChangeFont;	/* device has multiple fonts */
    int canRotateText;	/* text can be rotated */
    int canResizeText;	/* text can be resized */
    int canClip;	/* Hardware clipping */
    int canHAdj;	/* Can do at least some horizontal adjustment of text
			   0 = none, 1 = {0,0.5, 1}, 2 = [0,1] */

    /* a couple of the GRZ-like parameters that have to be */
    /* set by the device */

    double ipr[2];	/* Inches per raster; [0]=x, [1]=y */
    double asp;		/* Pixel aspect ratio = ipr[1]/ipr[0] */
    double cra[2];	/* Character size in rasters; [0]=x, [1]=y */

    /* Plot State */
    /* When the device driver is started this is 0 */
    /* After the first call to plot.new it is 1 */
    /* Every graphics operation except plot.new */
    /* should fail if state = 0 */
    /* This is checked at the highest internal function */
    /* level (e.g., do_lines, do_axis, do_plot_xy, ...) */

    int	state;		/* Plot State */
    int	valid;		/* valid layout ? */

    /* GRZ-like Graphics Parameters */
    /* ``The horror, the horror ... '' */
    /* Marlon Brando - Appocalypse Now */

    /* General Parameters -- set and interrogated directly */

    double adj;		/* String adjustment */
    int	ann;		/* Should annotation take place */
    int	ask;		/* User confirmation of ``page eject'' */
    rcolor bg;		/* **R ONLY** Background color */
    int	bty;		/* Box type */
    double cex;		/* Character expansion */
    rcolor col;		/* Plotting Color */
    double crt;		/* Character/string rotation */
    double din[2];	/* device size in inches */
    int	err;		/* Error repporting level */
    rcolor fg;		/* **R ONLY** Foreground Color */
    int	font;		/* Text font */
    double gamma;	/* Device Gamma Correction */
    int	lab[3];		/* Axis labelling */
			/* [0] = # ticks on x-axis */
			/* [1] = # ticks on y-axis */
			/* [2] = length of axis labels */
    int	las;		/* Label style (rotation) */
    int	lty;		/* Line texture */
    double lwd;		/* Line width */
    double mgp[3];	/* Annotation location */
			/* [0] = location of axis title */
			/* [1] = location of axis label */
			/* [2] = location of axis line */
    double mkh;		/* Mark size in inches */
    int	pch;		/* Plotting character */
    int	ps;		/* Text & symbol pointsize */
    int	smo;		/* Curve smoothness */
    double srt;		/* String Rotation */
    double tck;		/* Tick size as in S */
    double tcl;		/* Tick size in "lines" */
    double tmag;	/* **R ONLY** Title Magnification */
    int	type;		/* type of plot desired */
    double xaxp[3];	/* X Axis annotation */
			/* [0] = coordinate of lower tick */
			/* [1] = coordinate of upper tick */
			/* [2] = num tick intervals */
			/* almost always used internally */
    int	xaxs;		/* X Axis style */
    int	xaxt;		/* X Axis type */
    int	xpd;		/* Clip to plot region indicator */
    int	oldxpd;
    double yaxp[3];	/* Y Axis annotation */
    int	yaxs;		/* Y Axis style */
    int	yaxt;		/* Y Axis type */
    int	xlog;		/* Log Axis for X */
    int	ylog;		/* Log Axis for Y */

    /* Annotation Parameters */

    float cexbase;	/* Base character size */
    float cexmain;	/* Main title size */
    float cexlab;	/* xlab and ylab size */
    float cexsub;	/* Sub title size */
    float cexaxis;	/* Axis label size */

    int	fontmain;	/* Main title font */
    int	fontlab;	/* Xlab and ylab font */
    int	fontsub;	/* Subtitle font */
    int	fontaxis;	/* Axis label fonts */

    int	colmain;	/* Main title color */
    int	collab;		/* Xlab and ylab color */
    int	colsub;		/* Subtitle color */
    int	colaxis;	/* Axis label color */

    /* Layout Parameters */

    int	layout;		/* has a layout been specified */

    int	numrows;
    int	numcols;
    int	currentFigure;
    int	lastFigure;
    double heights[MAX_LAYOUT_ROWS];
    double widths[MAX_LAYOUT_COLS];
    int	cmHeights[MAX_LAYOUT_ROWS];
    int	cmWidths[MAX_LAYOUT_COLS];
    int	order[MAX_LAYOUT_ROWS][MAX_LAYOUT_COLS];
    int	rspct;	        /* 0 = none, 1 = full, 2 = see respect */
    int	respect[MAX_LAYOUT_ROWS][MAX_LAYOUT_COLS];

    int	mfind;		/* By row/col indicator */

    /* Layout parameters which can be set directly by the */
    /* user (e.g., par(fig=c(.5,1,0,1))) or otherwise are */
    /* calculated automatically */
    /* NOTE that *Units parameters are for internal use only */

    double fig[4];	/* (current) Figure size (proportion) */
			/* [0] = left, [1] = right */
			/* [2] = bottom, [3] = top */
    double fin[2];	/* (current) Figure size (inches) */
			/* [0] = width, [1] = height */
    int	fUnits;		/* (current) figure size units */
    int	defaultFigure;	/* calculate figure from layout ? */
    double plt[4];	/* (current) Plot size (proportions) */
			/* [0] = left, [1] = right */
			/* [2] = bottom, [3] = top */
    double pin[2];	/* (current) plot size (inches) */
			/* [0] = width, [1] = height */
    int	pUnits;		/* (current) plot size units */
    int	defaultPlot;	/* calculate plot from figure - margins ? */

    /* Layout parameters which are set directly by the user */

    double mar[4];	/* Plot margins in lines */
    double mai[4];	/* Plot margins in inches */
			/* [0] = bottom, [1] = left */
			/* [2] = top, [3] = right */
    int	mUnits;		/* plot margin units */
    double mex;		/* Margin expansion factor */
    double oma[4];	/* Outer margins in lines */
    double omi[4];	/* outer margins in inches */
    double omd[4];	/* outer margins in NDC */
			/* [0] = bottom, [1] = left */
			/* [2] = top, [3] = right */
    int	oUnits;		/* outer margin units */
    int	pty;		/* Plot type */

    /* Layout parameters which can be set by the user, but */
    /* almost always get automatically calculated anyway */

    double usr[4];	/* Graphics window */
			/* [0] = xmin, [1] = xmax */
			/* [2] = ymin, [3] = ymax */

    /* The logged usr parameter;  if xlog, use logusr[0:1] */
    /* if ylog, use logusr[2:3] */

    double logusr[4];

    /* Layout parameter: Internal flags */

    int	new;		/* Clean plot ? */
    int	devmode;	/* creating new image or adding to existing one */

    /* Coordinate System Mappings */
    /* These are only used internally (i.e., cannot be */
    /* set directly by the user) */

    /* The reliability of these parameters relies on */
    /* the fact that plot.new is the */
    /* first graphics operation called in the creation */
    /* of a graph */

    /* udpated per plot.new */

    double xNDCPerChar;	/* Nominal character width (NDC) */
    double yNDCPerChar;	/* Nominal character height (NDC) */
    double xNDCPerLine;	/* Nominal line width (NDC) */
    double yNDCPerLine;	/* Nominal line height (NDC) */
    double xNDCPerInch;	/* xNDC -> Inches */
    double yNDCPerInch;	/* yNDC -> Inches */

    /* updated per plot.new and if inner2dev changes */

    GTrans fig2dev;	/* Figure to device */

    /* udpated per DevNewPlot and if ndc2dev changes */

    GTrans inner2dev;	/* Inner region to device */

    /* udpated per device resize */

    GTrans ndc2dev;	/* NDC to raw device */

    /* updated per plot.new and per plot.window */

    GTrans win2fig;	/* Window to figure mapping */

    /* NOTE: if user has not set fig and/or plt then */
    /* they need to be updated per plot.new too */

    /* device operations */
    int (*open)();
    void (*close)();
    void (*activate)();
    void (*deactivate)();
    void (*resize)();
    void (*newPage)();
    void (*clip)();
    double (*strWidth)();
    void (*line)();
    void (*polyline)();
    void (*text)();
    void (*dot)();
    void (*rect)();
    void (*circle)();
    void (*polygon)();
    int (*locator)();
    void (*mode)();
    void (*hold)();
    void (*metricInfo)();
} GPar;

typedef struct {
	GPar dp;		/* current device default parameters */
	GPar gp;		/* current device current parameters */
	GPar dpSaved;		/* saved device default parameters */
	void *deviceSpecific;	/* pointer to device specific parameters */
	int displayListOn;	/* toggle for display list status */
	SEXP displayList;	/* display list */
} DevDesc;

		/* Drivers from ../main/dev....c , description there: */

int PSDeviceDriver(DevDesc*, char*, char*, char*,
		   char*, char*, double, double, double, double, int, 
		   int, int, char*);

int PicTeXDeviceDriver(DevDesc*, char*, char*, char*, double, double, int);

int XFigDeviceDriver(DevDesc*, char*, char*, char*,
		     char*, char*, double, double, double, double, int, 
		     int);

/*ifdef Unix : ../unix/devX11.h	 only in few places*/

#ifdef Win32
int WinDeviceDriver(char**, int, double*, int);
#endif

#ifdef OLD_Macintosh
int MacDeviceDriver(char**, int, double*, int);
#endif


#ifndef R_NO_REMAP
#define addDevice		Rf_addDevice
#define char2col		Rf_char2col
#define col2name		Rf_col2name
#define copyDisplayList		Rf_copyDisplayList
#define copyGPar		Rf_copyGPar
#define CreateAtVector		Rf_CreateAtVector
#define curDevice		Rf_curDevice
#define CurrentDevice		Rf_CurrentDevice
#define currentFigureLocation	Rf_currentFigureLocation
#define deviceNumber		Rf_deviceNumber
#define DevNull			Rf_DevNull
#define FixupCex		Rf_FixupCex
#define FixupCol		Rf_FixupCol
#define FixupFont		Rf_FixupFont
#define FixupLty		Rf_FixupLty
#define FixupLwd		Rf_FixupLwd
#define FixupPch		Rf_FixupPch
#define FixupVFont		Rf_FixupVFont
#define GArrow			Rf_GArrow
#define GBox			Rf_GBox
#define GCheckState		Rf_GCheckState
#define GCircle			Rf_GCircle
#define GClip			Rf_GClip
#define GClipPolygon		Rf_GClipPolygon
#define GConvert		Rf_GConvert
#define GConvertX		Rf_GConvertX
#define GConvertXUnits		Rf_GConvertXUnits
#define GConvertY		Rf_GConvertY
#define GConvertYUnits		Rf_GConvertYUnits
#define GEndPath		Rf_GEndPath
#define GetAxisLimits		Rf_GetAxisLimits
#define GetDevice		Rf_GetDevice
#define GExpressionHeight	Rf_GExpressionHeight
#define GExpressionWidth	Rf_GExpressionWidth
#define GForceClip		Rf_GForceClip
#define GInit			Rf_GInit
#define GLine			Rf_GLine
#define GLocator		Rf_GLocator
#define GLPretty		Rf_GLPretty
#define GMapUnits		Rf_GMapUnits
#define GMapWin2Fig		Rf_GMapWin2Fig
#define GMathText		Rf_GMathText
#define GMetricInfo		Rf_GMetricInfo
#define GMMathText		Rf_GMMathText
#define GMode			Rf_GMode
#define GMtext			Rf_GMtext
#define GNewPlot		Rf_GNewPlot
#define GPolygon		Rf_GPolygon
#define GPolyline		Rf_GPolyline
#define GPretty			Rf_GPretty
#define GRect			Rf_GRect
#define GReset			Rf_GReset
#define GRestore		Rf_GRestore
#define GRestorePars		Rf_GRestorePars
#define GSavePars		Rf_GSavePars
#define GScale			Rf_GScale
#define GSetState		Rf_GSetState
#define GSetupAxis		Rf_GSetupAxis
#define GStartPath		Rf_GStartPath
#define GStrHeight		Rf_GStrHeight
#define GStrWidth		Rf_GStrWidth
#define GSymbol			Rf_GSymbol
#define GText			Rf_GText
#define GVStrHeight		Rf_GVStrHeight
#define GVStrWidth		Rf_GVStrWidth
#define GVText			Rf_GVText
#define inhibitDisplayList	Rf_inhibitDisplayList
#define initDisplayList		Rf_initDisplayList
#define InitGraphics		Rf_InitGraphics
#define KillAllDevices		Rf_KillAllDevices
#define KillDevice		Rf_KillDevice
#define killDevice		Rf_killDevice
#define labelformat		Rf_labelformat
#define LTYget			Rf_LTYget
#define LTYpar			Rf_LTYpar
#define name2col		Rf_name2col
#define NewFrameConfirm		Rf_NewFrameConfirm
#define nextDevice		Rf_nextDevice
#define NoDevices		Rf_NoDevices
#define NumDevices		Rf_NumDevices
#define playDisplayList		Rf_playDisplayList
#define prevDevice		Rf_prevDevice
#define ProcessInlinePars	Rf_ProcessInlinePars
#define recordGraphicOperation	Rf_recordGraphicOperation
#define rgb2col			Rf_rgb2col
#define RGB2rgb			Rf_RGB2rgb
#define RGBpar			Rf_RGBpar
#define ScaleColor		Rf_ScaleColor
#define selectDevice		Rf_selectDevice
#define Specify2		Rf_Specify2
#define StartDevice		Rf_StartDevice
#define str2col			Rf_str2col
#define StrMatch		Rf_StrMatch
#define xDevtoNDC		Rf_xDevtoNDC
#define xDevtoNFC		Rf_xDevtoNFC
#define xNPCtoUsr		Rf_xNPCtoUsr
#define yDevtoNDC		Rf_yDevtoNDC
#define yDevtoNFC		Rf_yDevtoNFC
#define yNPCtoUsr		Rf_yNPCtoUsr
#endif

		/* User Callable Functions */

/*-------------------------------------------------------------------
 *
 *  DEVICE FUNCTIONS are concerned with the creation and destruction
 *  of devices.
 *
 */

/* Return a pointer to the current device. */
DevDesc* CurrentDevice();
/* Return a pointer to a device which is identified by number */
DevDesc* GetDevice(int);
/* Initialise internal device structures. */
void InitGraphics(void);
/* Kill device which is identified by number. */
void KillDevice(DevDesc*);
/* Kill all active devices (used at shutdown). */
void KillAllDevices();
/* Is the null device the current device? */
int NoDevices();
/* How many devices exist ? (>= 1) */
int NumDevices();
/* Get the index of the specified device. */
int deviceNumber(DevDesc*);
/* Create a new device. */
int StartDevice(SEXP, SEXP, int, SEXP, int);

void DevNull(void);

/* Miscellaneous */
void NewFrameConfirm();
void recordGraphicOperation(SEXP, SEXP, DevDesc*);
void initDisplayList();
void copyDisplayList(int);
void playDisplayList(DevDesc*);
void inhibitDisplayList(DevDesc*);

/*-------------------------------------------------------------------
 *
 *  DEVICE UTILITIES are concerned with providing information
 *  for R interpreted functions.
 *
 */

/* Return the number of the current device. */
int curDevice();
/* Return the number of the next device. */
int nextDevice(int);
/* Return the number of the previous device. */
int prevDevice(int);
/* Make the specified device (specified by number) the current device */
int selectDevice(int);
/* Kill device which is identified by number. */
void killDevice(int);
/* ...NO DOC... */
void addDevice(DevDesc *);


/*-------------------------------------------------------------------
 *
 *  GPAR FUNCTIONS are concerned with operations on the
 *  entire set of graphics parameters for a device
 *  (e.g., initialisation, saving, and restoring)
 */

/* Default the settings for general graphical parameters
 * (i.e., defaults that do not depend on the device type: */
void GInit(GPar*);
/* Reset the current graphical parameters from the default ones: */
void GRestore(DevDesc*);
/* Make a temporary copy of the current parameters */
void GSavePars(DevDesc*);
/* Restore the temporary copy saved by GSavePars */
void GRestorePars(DevDesc*);

		/* More Programmer GPar functions */

void ProcessInlinePars(SEXP, DevDesc*);
void Specify2(char*, SEXP, DevDesc*);
void RecordGraphicsCall(SEXP);

SEXP FixupPch(SEXP, int);
SEXP FixupLty(SEXP, int);
SEXP FixupFont(SEXP, int);
SEXP FixupCol(SEXP, unsigned int);
SEXP FixupCex(SEXP, double);
SEXP FixupLwd(SEXP, double);
SEXP FixupVFont(SEXP);



/*-------------------------------------------------------------------
 *
 *  DEVICE STATE FUNCTIONS are concerned with getting and setting
 *  the current state of the device;  is it ready to be drawn into?
 */

/* has plot.new been called yet? */
void GCheckState(DevDesc*);
/* Set to 1 when plot.new succeeds
 * Set to 0 when don't want drawing to go ahead */
void GSetState(int, DevDesc*);



/*-------------------------------------------------------------------
 *
 *  GRAPHICAL PRIMITIVES are the generic front-end for the functions
 *  that every device driver must provide.
 *
 *  NOTE that locations supplied to these functions may be in any
 *  of the valid coordinate systems (each function takes a "coords"
 *  parameter to indicate the coordinate system);  the device-specific
 *  version of the function is responsible for calling GConvert to get
 *  the location into device coordinates.
 *
 */


/* Draw a circle, centred on (x,y) with radius r (in inches). */
void GCircle(double, double, int, double, int, int, DevDesc*);
/* Set clipping region (based on current setting of dd->gp.xpd).
 * Only clip if new clipping region is different from the current one */
void GClip(DevDesc*);
/* Polygon clipping: */
int GClipPolygon(double *, double *, int, int, int,
		 double *, double *, DevDesc *);
/* Always clips */
void GForceClip(DevDesc*);
/* Draw a line from (x1,y1) to (x2,y2): */
void GLine(double, double, double, double, int, DevDesc*);
/* Return the location of the next mouse click: */
int  GLocator(double*, double*, int, DevDesc*);
/* Return the height, depth, and width of the specified
 * character in the specified units: */
void GMetricInfo(int, double*, double*, double*, int, DevDesc*);
/* Set device "mode" (drawing or not drawing) here for windows and mac drivers.
 */
void GMode(int, DevDesc*);
/* Draw a polygon using the specified lists of x and y values: */
void GPolygon(int, double*, double*, int, int, int, DevDesc*);
/* Draw series of straight lines using the specified lists of x and y values: */
void GPolyline(int, double*, double*, int, DevDesc*);
/* Draw a rectangle given two opposite corners: */
void GRect(double, double, double, double, int, int, int, DevDesc*);
/* Return the height of the specified string in the specified units: */
double GStrHeight(char*, int, DevDesc*);
/* Return the width of the specified string in the specified units */
double GStrWidth(char*, int, DevDesc*);
/* Draw the specified text at location (x,y) with the specified
 * rotation and justification: */
void GText(double, double, int, char*, double, double, double, DevDesc*);


void GStartPath(DevDesc*);
void GEndPath(DevDesc*);

void GMathText(double, double, int, SEXP, double, double, double, DevDesc*);
void GMMathText(SEXP, int, double, int, double, int, DevDesc*);

void GVText(double x, double y, int unit, char* s, int typeface, int fontindex,
	    double xadj, double yadj, double rot, DevDesc *dd);
double GVStrWidth(const unsigned char *s, int typeface, int fontindex,
		  int unit, DevDesc *dd);
double GVStrHeight(const unsigned char *s, int typeface, int fontindex,
		   int unit, DevDesc *dd);

/*-------------------------------------------------------------------
 *
 *  GRAPHICAL UTILITIES are functions that produce graphical output
 *  using the graphical primitives (i.e., they are generic - NOT
 *  device-specific).
 *
 */

/* Draw a line from (x1,y1) to (x2,y2) with an arrow head
 * at either or both ends. */
void GArrow(double, double, double, double, int, double, double, int, DevDesc*);
/* Draw a box around specified region:
 *  1=plot region, 2=figure region, 3=inner region, 4=device. */
void GBox(int, DevDesc*);
/* Return a "nice" min, max and number of intervals for a given
 * range on a linear or _log_ scale, respectively: */
void GPretty(double*, double*, int*);
void GLPretty(double*, double*, int*);
/* Draw text in margins. */
void GMtext(char*, int, double, int, double, int, DevDesc*);
/* Draw one of the predefined symbols (circle, square, diamond, ...) */
void GSymbol(double, double, int, int, DevDesc*);

double GExpressionHeight(SEXP, int, DevDesc*);
double GExpressionWidth(SEXP, int, DevDesc*);



/*-------------------------------------------------------------------
 *
 *  COLOUR CODE is concerned with the internals of R colour representation
 *
 */

/* Convert an R colour specification (which might be a number or */
/* a string) into an internal colour specification. */
unsigned int RGBpar(SEXP, int);


/*-------------------------------------------------------------------
 *
 *  LINE TEXTURE CODE is concerned with the internals of R
 *  line texture representation.
 */
unsigned int LTYpar(SEXP, int);
SEXP LTYget(unsigned int);


/*----------------------------------------------------------------------
 *
 *  TRANSFORMATIONS are concerned with converting locations between
 *  coordinate systems and dimensions between different units.
 */

/* Convert an R unit (e.g., "user") into an internal unit (e.g., USER)> */
int GMapUnits(int);
/* Convert a LOCATION from one coordinate system to another: */
void GConvert(double*, double*, int, int, DevDesc*);
double GConvertX(double, int, int, DevDesc*);
double GConvertY(double, int, int, DevDesc*);
/* Convert an x/y-dimension from one set of units to another: */
double GConvertXUnits(double, int, int, DevDesc*);
double GConvertYUnits(double, int, int, DevDesc*);

/* Set up the different regions on a device (i.e., inner region,
 * figure region, plot region) and transformations for associated
 * coordinate systems (called whenever anything that affects the
 * coordinate transformations changes):
 */
void GReset(DevDesc*);

/* Set up the user coordinate transformations: */
void GMapWin2Fig(DevDesc*);
/* Set up the device for a new plot by Resetting graphics parameters
 * and Resetting the regions and coordinate Systems */
DevDesc *GNewPlot(int, int);
/* Set up the user coordinates based on the axis limits */
void GScale(double, double, int, DevDesc*);
/* Set up the axis limits based on the user coordinates */
void GSetupAxis(int, DevDesc*);
/* Return row and column of current figure in the layout matrix */
void currentFigureLocation(int*, int*, DevDesc*);

double R_Log10(double);

double xDevtoNDC(double x, DevDesc *dd);
double yDevtoNDC(double y, DevDesc *dd);
double xDevtoNFC(double x, DevDesc *dd);
double yDevtoNFC(double y, DevDesc *dd);
double xNPCtoUsr(double, DevDesc*);
double yNPCtoUsr(double, DevDesc*);


		/* Miscellaneous (from graphics.c & colors.c) */

unsigned int rgb2col(char *);
unsigned int name2col(char *);
unsigned int char2col(char *s);/* rgb2col() or name2col() */
char* col2name(unsigned int);
unsigned int str2col(char *s);

unsigned int ScaleColor(double x);

char* RGB2rgb(unsigned int, unsigned int, unsigned int);

int StrMatch(char *s, char *t);
void copyGPar(GPar *, GPar *);

/* some functions that plot.c needs to share with plot3d.c */
SEXP CreateAtVector(double*, double*, int, int);
void GetAxisLimits(double, double, double*, double*);
SEXP labelformat(SEXP);

/* Vector fonts */

double GVStrWidth (const unsigned char *, int, int, int, DevDesc *);
double GVStrHeight (const unsigned char *, int, int, int, DevDesc *);
void GVText (double, double, int, char *, int, int, 
	     double, double, double, DevDesc *);

#endif
