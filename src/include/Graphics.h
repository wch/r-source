/*
 *  R : A Computer Language for Statistical Data Analysis
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

#ifndef GRAPHICS_H_
#define GRAPHICS_H_

#include "Defn.h"
#include "Arith.h"
#include "Errormsg.h"

#ifndef NA_REAL
#define NA_REAL R_NaReal
#endif

#ifndef NA_INTEGER
#define NA_INTEGER R_NaInt
#endif

#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#endif

#ifdef Windows
#include "windows.h"
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
 *		<-- most sig        least sig -->
 *		+-------------------------------+
 *		|   0   | blue  | green |  red  |
 *		+-------------------------------+
 *
 *	The red, green and blue bytes can be extracted as follows.
 *
 *		red   = ((color      ) & 255)
 *		green = ((color >>  8) & 255)
 *		blue  = ((color >> 16) & 255)
 */

#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16))
#define R_RED(col)	(((col)    )&255)
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)
#define COLOR_TABLE_SIZE 256


#ifdef Unix
#define LTY_SOLID	0
#define LTY_DASHED	4 + (4<<4)
#define LTY_DOTTED	1 + (3<<4)
#define LTY_DOTDASH	3 + (1<<4) + (3<<8) + (4<<12)
#endif

#ifdef Windows
#define LTY_SOLID	PS_SOLID
#define LTY_DASHED	PS_DASH
#define LTY_DOTTED	PS_DOT
#define LTY_DOTDASH	PS_DASHDOT
#endif

#define MAX_LAYOUT_ROWS 15
#define MAX_LAYOUT_COLS 15

	/* possible coordinate systems (for specifying locations) */

#define DEVICE	0	/* native device coordinates (rasters) */
#define NDC	1	/* normalised device coordinates x=(0,1), y=(0,1) */
#define INCHES 13 	/* inches x=(0,width), y=(0,height) */
#define NIC	6	/* normalised inner region coordinates (0,1) */
#define OMA1	2	/* outer margin 1 (bottom) x=NIC, y=LINES */
#define OMA2	3	/* outer margin 2 (left) */
#define OMA3	4	/* outer margin 3 (top) */
#define OMA4	5	/* outer margin 4 (right) */
#define NFC	7	/* normalised figure region coordinates (0,1) */
#define NPC     16	/* normalised plot region coordinates (0,1) */
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

typedef unsigned int rcolor;

typedef struct {
	double ax;
	double bx;
	double ay;
	double by;
} GTrans;

struct colorDataBaseEntry {
	char *name;     /* X11 Color Name */
	char *rgb;      /* #RRGGBB String */
	unsigned code;  /* Internal R Color Code */
};

typedef struct colorDataBaseEntry ColorDataBaseEntry;

extern int ColorTableSize;
extern unsigned int ColorTable[];
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

	double	left;		/* left raster coordinate */
	double	right;		/* right raster coordinate */
	double	bottom;		/* bottom raster coordinate */
	double	top;		/* top raster coordinate */
	double	xCharOffset;	/* x character addressing offset */
	double	yCharOffset;	/* y character addressing offset */
	double	yLineBias;	/* 1/2 interline space as fraction of line height */
	int	canResizePlot;	/* can the graphics surface be resized */
	int	canChangeFont;	/* device has multiple fonts */
	int	canRotateText;	/* text can be rotated */
	int	canResizeText;	/* text can be resized */
	int	canClip;	/* Hardware clipping */

		/* a couple of the GRZ-like parameters that have to be */
		/* set by the device */

	double	ipr[2];		/* Inches per raster; [0]=x, [1]=y */
	double	asp;		/* Pixel aspect ratio = ipr[1]/ipr[0] */
	double	cra[2];		/* Character size in rasters; [0]=x, [1]=y */

	/* Plot State */
		/* When the device driver is started this is 0 */
		/* After the first call to plot.new it is 1 */
		/* Every graphics operation except plot.new */
		/* should fail if state = 0 */
		/* This is checked at the highest internal function */
		/* level (e.g., do_lines, do_axis, do_plot_xy, ...) */

	int	state;		/* Plot State */
	int 	valid;		/* valid layout ? */

	/* GRZ-like Graphics Parameters */
		/* ``The horror, the horror ... '' */
		/* Marlon Brando - Appocalypse Now */


		/* General Parameters -- set and interrogated directly */

	double	adj;		/* String adjustment */
	int	ann;		/* Should annotation take place */
	int	ask;		/* User confirmation of ``page eject'' */
	rcolor	bg;		/* **R ONLY** Background color */
	int	bty;		/* Box type */
	double	cex;		/* Character expansion */
	rcolor	col;		/* Plotting Color */
	double	crt;		/* Character/string rotation */
	double	din[2];		/* device size in inches */
	int	err;		/* Error repporting level */
	rcolor	fg;		/* **R ONLY** Foreground Color */
	int	font;		/* Text font */
	double	gamma;		/* Device Gamma Correction */
	int	lab[3];		/* Axis labelling */
				/* [0] = # ticks on x-axis */
				/* [1] = # ticks on y-axis */
				/* [2] = length of axis labels */
	int	las;		/* Label style (rotation) */
	int	lty;		/* Line texture */
	double	lwd;		/* Line width */
	double	mgp[3];		/* Annotation location */
				/* [0] = location of axis title */
				/* [1] = location of axis label */
				/* [2] = location of axis line */
	double	mkh;		/* Mark size in inches */
	int	pch;		/* Plotting character */
	int	ps;		/* Text & symbol pointsize */
	int	smo;		/* Curve smoothness */
	double	srt;		/* String Rotation */
	double	tck;		/* Tick size as in S */
	double  tcl;            /* Tick size in "lines" */
	double	tmag;		/* **R ONLY** Title Magnification */
	int	type;		/* type of plot desired */
	double	xaxp[3];	/* X Axis annotation */
				/* [0] = coordinate of lower tick */
				/* [1] = coordinate of upper tick */
				/* [2] = num tick intervals */
				/* almost always used internally */
	int	xaxs;		/* X Axis style */
	int	xaxt;		/* X Axis type */
	int	xpd;		/* Clip to plot region indicator */
	int 	oldxpd;
	double	yaxp[3];	/* Y Axis annotation */
	int	yaxs;		/* Y Axis style */
	int	yaxt;		/* Y Axis type */
	int	xlog;		/* Log Axis for X */
	int	ylog;		/* Log Axis for Y */

		/* Annotation Parameters */

	float	cexbase;	/* Base character size */
	float	cexmain;	/* Main title size */
	float	cexlab;		/* xlab and ylab size */
	float	cexsub;		/* Sub title size */
	float	cexaxis;	/* Axis label size */

	int	fontmain;	/* Main title font */
	int	fontlab;	/* Xlab and ylab font */
	int	fontsub;	/* Subtitle font */
	int	fontaxis;	/* Axis label fonts */

	int	colmain;	/* Main title color */
	int	collab;		/* Xlab and ylab color */
	int	colsub;		/* Subtitle color */
	int	colaxis;	/* Axis label color */

		/* Layout Parameters */

	int     layout;		/* has a layout been specified */

	int 	numrows;
	int 	numcols;
	int 	currentFigure;
	int 	lastFigure;
	double	heights[MAX_LAYOUT_ROWS];
	double 	widths[MAX_LAYOUT_COLS];
	int 	cmHeights[MAX_LAYOUT_ROWS];
	int 	cmWidths[MAX_LAYOUT_COLS];
	int 	order[MAX_LAYOUT_ROWS][MAX_LAYOUT_COLS];
	int 	rspct;	/* 0 = none, 1 = full, 2 = see respect */
	int 	respect[MAX_LAYOUT_ROWS][MAX_LAYOUT_COLS];

	int     mfind;          /* By row/col indicator */

		/* Layout parameters which can be set directly by the */
		/* user (e.g., par(fig=c(.5,1,0,1))) or otherwise are */
		/* calculated automatically */
		/* NOTE that *Units parameters are for internal use only */

	double	fig[4];		/* (current) Figure size (proportion) */
				/* [0] = left, [1] = right */
				/* [2] = bottom, [3] = top */
	double  fin[2];		/* (current) Figure size (inches) */
				/* [0] = width, [1] = height */
	int 	fUnits;		/* (current) figure size units */
	int 	defaultFigure;	/* calculate figure from layout ? */
	double	plt[4];		/* (current) Plot size (proportions) */
				/* [0] = left, [1] = right */
				/* [2] = bottom, [3] = top */
	double  pin[2];		/* (current) plot size (inches) */
				/* [0] = width, [1] = height */
	int 	pUnits;		/* (current) plot size units */
	int 	defaultPlot;	/* calculate plot from figure - margins ? */

		/* Layout parameters which are set directly by the user */

	double	mar[4];		/* Plot margins in lines */
	double  mai[4];		/* Plot margins in inches */
				/* [0] = bottom, [1] = left */
				/* [2] = top, [3] = right */
	int 	mUnits;		/* plot margin units */
	double	mex;		/* Margin expansion factor */
	double	oma[4];		/* Outer margins in lines */
	double  omi[4];		/* outer margins in inches */
	double	omd[4];		/* outer margins in NDC */
				/* [0] = bottom, [1] = left */
				/* [2] = top, [3] = right */
	int 	oUnits;		/* outer margin units */
	int	pty;		/* Plot type */

		/* Layout parameters which can be set by the user, but */
		/* almost always get automatically calculated anyway */

	double	usr[4];		/* Graphics window */
				/* [0] = xmin, [1] = xmax */
				/* [2] = ymin, [3] = ymax */

		/* The logged usr parameter;  if xlog, use logusr[0:1] */
		/*                            if ylog, use logusr[2:3] */

	double logusr[4];

		/* Layout parameter: Internal flags */

	int	new;		/* Clean plot ? */
	int 	devmode;	/* creating new image or adding to existing one */

		/* Coordinate System Mappings */
		/* These are only used internally (i.e., cannot be */
		/* set directly by the user) */

		/* The reliability of these parameters relies on */
		/* the fact that plot.new is the */
		/* first graphics operation called in the creation */
		/* of a graph */

		/* udpated per plot.new */

	double	xNDCPerChar;	/* Nominal character width (NDC) */
	double	yNDCPerChar;	/* Nominal character height (NDC) */
	double	xNDCPerLine;	/* Nominal line width (NDC) */
	double	yNDCPerLine;	/* Nominal line height (NDC) */
	double	xNDCPerInch;	/* xNDC -> Inches */
	double	yNDCPerInch;	/* yNDC -> Inches */

		/* updated per plot.new and if inner2dev changes */

	GTrans	fig2dev;	/* Figure to device */

		/* udpated per DevNewPlot and if ndc2dev changes */

	GTrans 	inner2dev;	/* Inner region to device */

		/* udpated per device resize */

	GTrans	ndc2dev;	/* NDC to raw device */

		/* updated per plot.new and per plot.window */

	GTrans	win2fig;	/* Window to figure mapping */

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
	GPar dp;          	/* current device default parameters */
	GPar gp;		/* current device current parameters */
	GPar dpSaved; 		/* saved device default parameters */
	void *deviceSpecific;   /* pointer to device specific parameters */
	int displayListOn;    	/* toggle for display list status */
	SEXP displayList;     	/* display list */
} DevDesc;

char *R_alloc(long, int);
char *vmaxget(void);
void vmaxset(char*);

		/* User Callable Functions */

		/* Programmer Device functions */

DevDesc* CurrentDevice();
void DevNull(void);
DevDesc* GetDevice(int);
void InitGraphics(void);
void KillDevice(DevDesc*);
void KillAllDevices();
int NoDevices();
int NumDevices();
int deviceNumber(DevDesc*);
int StartDevice(SEXP, SEXP, int, SEXP, int);
/*- these added by MM, eliminating -Wall "implicit declaration"s: */
void recordGraphicOperation(SEXP, SEXP, DevDesc*);
void initDisplayList();
void copyDisplayList(int);
void playDisplayList(DevDesc*);
void inhibitDisplayList(DevDesc*);

		/* Utility Device functions */

void addDevice(DevDesc *);
int curDevice();
int nextDevice(int);
int prevDevice(int);
int selectDevice(int);
void killDevice(int);

		/* Programmer GPar functions */

void GInit(GPar*);
void GRestore(DevDesc*);
void GSavePars(DevDesc*);
void GRestorePars(DevDesc*);

SEXP FixupPch(SEXP, DevDesc*);
SEXP FixupLty(SEXP, DevDesc*);
SEXP FixupFont(SEXP);		/* FIXME: need 2nd arg? */
SEXP FixupCol(SEXP, DevDesc*);
SEXP FixupCex(SEXP);


		/* Device State functions */

void GCheckState(DevDesc*);
void GSetState(int, DevDesc*);

		/* Graphical Primitives */
		/* Device Drivers must do all of these */

void GCircle(double, double, int, double, int, int, DevDesc*);
void GClip(DevDesc*);
void GEndPath(DevDesc*);
void GForceClip(DevDesc*);
void GLine(double, double, double, double, int, DevDesc*);
int  GLocator(double*, double*, int, DevDesc*);
void GMetricInfo(int, double*, double*, double*, int, DevDesc*);
void GMode(DevDesc*, int);
void GPolygon(int, double*, double*, int, int, int, DevDesc*);
void GPolyline(int, double*, double*, int, DevDesc*);
void GRect(double, double, double, double, int, int, int, DevDesc*);
void GStartPath(DevDesc*);
double GStrHeight(char*, int, DevDesc*);
double GStrWidth(char*, int, DevDesc*);
void GText(double, double, int, char*, double, double, double, DevDesc*);

void GMathText(double, double, int, SEXP, double, double, double, DevDesc*);
void GMMathText(SEXP, int, double, int, double, int, DevDesc*);

		/* Graphical Utilities */

void GArrow(double, double, double, double, int, double, double, int, DevDesc*);
void GBox(int, DevDesc*);
double GExpressionHeight(SEXP, int, DevDesc*);
double GExpressionWidth(SEXP, int, DevDesc*);
void GLPretty(double*, double*, int*);
void GMtext(char*, int, double, int, double, int, DevDesc*);
void GPretty(double*, double*, int*);
void GSymbol(double, double, int, int, DevDesc*);
unsigned RGBpar(SEXP, int, DevDesc*);

		/* Coordinate Transformation functions */

double Log10(double);
void currentFigureLocation(int*, int*, DevDesc*);
DevDesc *GNewPlot(int, int);
void GReset(DevDesc*);
void GMapWin2Fig(DevDesc*);
void GConvert(double*, double*, int, int, DevDesc*);
double GConvertXUnits(double, int, int, DevDesc*);
double GConvertYUnits(double, int, int, DevDesc*);
void GScale(double, double, int, DevDesc*);
void GSetupAxis(int, DevDesc*);
int GMapUnits(int);

double xNPCtoUsr(double, DevDesc*);
double yNPCtoUsr(double, DevDesc*);

		/* Miscellaneous (from graphics.c) */

unsigned int rgb2col(char *);
unsigned int name2col(char *);

#endif
