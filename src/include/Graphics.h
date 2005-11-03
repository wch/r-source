/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  R Development Core Team
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

#define R_GRAPHICS_INTERNAL 1

#include <R_ext/Boolean.h>

#include <R_ext/GraphicsDevice.h>
#include <R_ext/GraphicsEngine.h>

#define R_MaxDevices 64

#define	DEG2RAD 0.01745329251994329576

#define COLOR_TABLE_SIZE 1024

#define MAX_LAYOUT_ROWS 50
#define MAX_LAYOUT_COLS 50
#define MAX_LAYOUT_CELLS 500 /* must be less than 65535, 
				3 copies, 3bytes each */

typedef unsigned int rcolor;

typedef struct {
	double ax;
	double bx;
	double ay;
	double by;
} GTrans;

	/* possible coordinate systems (for specifying locations) */
typedef enum {
 DEVICE	= 0,	/* native device coordinates (rasters) */
 NDC	= 1,	/* normalised device coordinates x=(0,1), y=(0,1) */
 INCHES = 13,	/* inches x=(0,width), y=(0,height) */
 NIC	= 6,	/* normalised inner region coordinates (0,1) */
 OMA1	= 2,	/* outer margin 1 (bottom) x=NIC, y=LINES */
 OMA2	= 3,	/* outer margin 2 (left) */
 OMA3	= 4,	/* outer margin 3 (top) */
 OMA4	= 5,	/* outer margin 4 (right) */
 NFC	= 7,	/* normalised figure region coordinates (0,1) */
 NPC	= 16,	/* normalised plot region coordinates (0,1) */
 USER	= 12,	/* user/data/world coordinates;
		 * x,=(xmin,xmax), y=(ymin,ymax) */
 MAR1	= 8,	/* figure margin 1 (bottom) x=USER(x), y=LINES */
 MAR2	= 9,	/* figure margin 2 (left)   x=USER(y), y=LINES */
 MAR3	= 10,	/* figure margin 3 (top)    x=USER(x), y=LINES */
 MAR4	= 11,	/* figure margin 4 (right)  x=USER(y), y=LINES */

	/* possible, units (for specifying dimensions) */
	/* all of the above, plus ... */

 LINES = 14,	/* multiples of a line in the margin (mex) */
 CHARS = 15	/* multiples of text height (cex) */
} GUnit;


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
    Rboolean canResizePlot;	/* can the graphics surface be resized */
    Rboolean canChangeFont;	/* device has multiple fonts */
    Rboolean canRotateText;	/* text can be rotated */
    Rboolean canResizeText;	/* text can be resized */
    Rboolean canClip;		/* Hardware clipping */
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
    Rboolean valid;	/* valid layout ? */

    /* GRZ-like Graphics Parameters */
    /* ``The horror, the horror ... '' */
    /* Marlon Brando - Appocalypse Now */

    /* General Parameters -- set and interrogated directly */

    double adj;		/* String adjustment */
    Rboolean ann;	/* Should annotation take place */
    Rboolean ask;	/* User confirmation of ``page eject'' */
    rcolor bg;		/* **R ONLY** Background color */
    int	bty;		/* Box type */
    double cex;		/* Character expansion */
    double lheight;     /* Line height
			   The height of a line of text is:
			   ps * cex * lheight */
    rcolor col;		/* Plotting Color */
    double crt;		/* Character/string rotation */
    double din[2];	/* device size in inches */
    int	err;		/* Error repporting level */
    rcolor fg;		/* **R ONLY** Foreground Color */
    char family[50];    /* **R ONLY** Font family 
			   Simple name which is mapped by device-specific
			   font database to device-specific name.
			   Only used if not "".
			   Default is "".
			   Ignored by some devices. */
    int	font;		/* Text font */
    double gamma;	/* Device Gamma Correction */
    int	lab[3];		/* Axis labelling */
			/* [0] = # ticks on x-axis */
			/* [1] = # ticks on y-axis */
			/* [2] = length of axis labels */
    int	las;		/* Label style (rotation) */
    int	lty;		/* Line texture */
    double lwd;		/* Line width */
    R_GE_lineend lend;  /* **R ONLY** Line end style */
    R_GE_linejoin ljoin;/* **R ONLY** Line join style */
    double lmitre;      /* **R ONLY** Line mitre limit */
    double mgp[3];	/* Annotation location */
			/* [0] = location of axis title */
			/* [1] = location of axis label */
			/* [2] = location of axis line */
    double mkh;		/* Mark size in inches */
    int	pch;		/* Plotting character */
    int ps;		/* Text & symbol pointsize */
    int	smo;		/* Curve smoothness */
    double srt;		/* String Rotation */
    double tck;		/* Tick size as in S */
    double tcl;		/* Tick size in "lines" */
    double tmag;	/* **R ONLY** Title Magnification */
    /* int	type;	    type of plot desired -- removed in 2.3.0 */
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
    Rboolean xlog;	/* Log Axis for X */
    Rboolean ylog;	/* Log Axis for Y */

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

    Rboolean layout;	/* has a layout been specified */

    int	numrows;
    int	numcols;
    int	currentFigure;
    int	lastFigure;
    double heights[MAX_LAYOUT_ROWS];
    double widths[MAX_LAYOUT_COLS];
    int	cmHeights[MAX_LAYOUT_ROWS];
    int	cmWidths[MAX_LAYOUT_COLS];
    unsigned short order[MAX_LAYOUT_CELLS];
    int	rspct;		/* 0 = none, 1 = full, 2 = see respect */
    unsigned char respect[MAX_LAYOUT_CELLS];

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
    GUnit fUnits;	/* (current) figure size units */
    double plt[4];	/* (current) Plot size (proportions) */
			/* [0] = left, [1] = right */
			/* [2] = bottom, [3] = top */
    double pin[2];	/* (current) plot size (inches) */
			/* [0] = width, [1] = height */
    GUnit pUnits;	/* (current) plot size units */
    Rboolean defaultFigure;	/* calculate figure from layout ? */
    Rboolean defaultPlot;	/* calculate plot from figure - margins ? */

    /* Layout parameters which are set directly by the user */

    double mar[4];	/* Plot margins in lines */
    double mai[4];	/* Plot margins in inches */
			/* [0] = bottom, [1] = left */
			/* [2] = top, [3] = right */
    GUnit mUnits;	/* plot margin units */
    double mex;		/* Margin expansion factor */
    double oma[4];	/* Outer margins in lines */
    double omi[4];	/* outer margins in inches */
    double omd[4];	/* outer margins in NDC */
			/* [0] = bottom, [1] = left */
			/* [2] = top, [3] = right */
    GUnit oUnits;	/* outer margin units */
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

    Rboolean new;	/* Clean plot ? */
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

    double scale;       /* An internal "zoom" factor to apply to ps and lwd */
                        /* (for fit-to-window resizing in Windows) */

    /* device operations */
    Rboolean (*open)();
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
    Rboolean (*locator)();
    void (*mode)();
    void (*hold)();
    void (*metricInfo)();
} GPar;

typedef struct {
    /* New flag to indicate that this is an "old" device
     * structure.
     */
    int newDevStruct;
    GPar dp;		/* current device default parameters */
    GPar gp;		/* current device current parameters */
    GPar dpSaved;		/* saved device default parameters */
    void *deviceSpecific;	/* pointer to device specific parameters */
    Rboolean displayListOn;	/* toggle for display list status */
    SEXP displayList;	/* display list */
} DevDesc;

/* For easy reference: Here are the source files of
 * currently existing device drivers:
 * FILE				driver name prefix
 * ----------------------	------------------
 * ../main/devPS.c		PS , PDF _and_  XFig
 * ../main/devPicTeX.c		PicTeX
 * ../modules/X11/devX11.c	X11
 * ../gnuwin32/devga.c		GA
 * ../modules/gnome/devGTK.c	GTK
 * ../modules/gnome/devGNOME.c	Gnome
 */

/* always remap private functions */
#include <Rgraphics.h>
#define char2col		Rf_char2col
#define col2name		Rf_col2name
#define copyGPar		Rf_copyGPar
#define curDevice               Rf_curDevice
#define GetDevice               Rf_GetDevice
#define GInit			Rf_GInit
#define name2col		Rf_name2col
#define nextDevice              Rf_nextDevice
#define number2col		Rf_number2col
#define NumDevices              Rf_NumDevices
#define rgb2col			Rf_rgb2col
#define RGB2rgb			Rf_RGB2rgb
#define RGBA2rgb		Rf_RGBA2rgb
#define ScaleColor		Rf_ScaleColor
#define str2col			Rf_str2col
#define StrMatch		Rf_StrMatch
#define isNAcol                 Rf_isNAcol

/* NOTE: during replays, call == R_NilValue;
   ----  the following adds readability: */
Rboolean GRecording(SEXP, DevDesc*);

/* Default the settings for general graphical parameters
 * (i.e., defaults that do not depend on the device type: */
void GInit(GPar*);

void copyGPar(GPar *, GPar *);

int curDevice(void);

DevDesc* GetDevice(int i);

int nextDevice(int from);

int NumDevices(void);

int deviceNumber(DevDesc *dd);

int devNumber(DevDesc *dd);

		/* Miscellaneous (from graphics.c & colors.c) */

unsigned int rgb2col(char *);
unsigned int name2col(char *);
unsigned int number2col(char *);
unsigned int char2col(char *);/* rgb2col() or name2col() */
unsigned int str2col(char *);

char* col2name(unsigned int);

unsigned int ScaleColor(double x);
unsigned int CheckColor(int x);
Rboolean isNAcol(SEXP col, int index, int ncol);

char* RGB2rgb(unsigned int, unsigned int, unsigned int);
char* RGBA2rgb(unsigned int, unsigned int, unsigned int, unsigned int);

int StrMatch(char *s, char *t);

double R_Log10(double);

#include <R_ext/GraphicsBase.h>

/* 
 * Function to generate an R_GE_gcontext from Rf_gpptr info
 */
void gcontextFromGP(R_GE_gcontext *gc, DevDesc *dd);

/* FIXME: Make this a macro to avoid function call overhead?
 */
GPar* Rf_gpptr(DevDesc *dd);
GPar* Rf_dpptr(DevDesc *dd);
GPar* Rf_dpSavedptr(DevDesc *dd);
SEXP Rf_displayList(DevDesc *dd);

/* Graphics events */

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

SEXP doMouseEvent(SEXP eventRho, NewDevDesc *dd, R_MouseEvent event, 
                  int buttons, double x, double y);
SEXP doKeybd	(SEXP eventRho, NewDevDesc *dd, R_KeyName rkey, char *keyname);

#endif /* GRAPHICS_H_ */
