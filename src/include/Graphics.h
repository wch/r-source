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

typedef unsigned int rcolor;

typedef struct {
	double ax;
	double bx;
	double ay;
	double by;
} GTrans;

/* Graphics State:
 *
 * The following structure defines state for a graphics device driver.
 * Two copies are kept; the ``default'' set of values, and a set which
 * can be modified during calls to an application program.  When a
 * new graphics frame is started, the values revert to the defaults
 */

typedef struct {
		/* Basic Device Driver Properties */
		/* These MUST be set by device drivers on open */

	double	left;		/* left raster coordinate */
	double	right;		/* right raster coordinate */
	double	bottom;		/* bottom raster coordinate */
	double	top;		/* top raster coordinate */
	double	xCharOffset;	/* x character addressing offset */
	double	yCharOffset;	/* y character addressing offset */
	double	yLineBias;	/* 1/2 interline space as a fraction of line height */
	int	canResizePlot;	/* can the graphics surface be resized */
	int	canChangeFont;	/* device has multiple fonts */
	int	canRotateText;	/* text can be rotated */
	int	canResizeText;	/* text can be resized */
	int	canClip;	/* Hardware clipping */

		/* Plot State */
		/* When the device driver is started this is 0 */
		/* After the first call to plot.new it is 1 */

	int	state;		/* Plot State */

		/* Parameters computed from those */
		/* above at the start of a new plot */

	double	xNDCPerChar;	/* Nominal character width (NDC) */
	double	yNDCPerChar;	/* Nominal character height (NDC) */
	double	xNDCPerInch;	/* xNDC -> Inches */
	double	yNDCPerInch;	/* yNDC -> Inches */

		/* GRZ-like Graphics Parameters */
		/* ``The horror, the horror ... '' */
		/* Marlon Brando - Appocalypse Now */

		/* General Parameters */

	double	ipr[2];		/* Inches per raster */
	double	asp;		/* Pixel aspect ratio = ipr[1]/ipr[0] */
	double	adj;		/* String adjustment */
	int	ann;		/* Should annotation take place */
	int	ask;		/* User confirmation of ``page eject'' */
	rcolor	bg;		/* **R ONLY** Background color */
	int	bty;		/* Box type */
	double	cex;		/* Character expansion */
	rcolor	col;		/* Plotting Color */
	double	cra[2];		/* Character size in rasters */
	double	crt;		/* Character/string rotation */
	int	err;		/* Error repporting level */
	rcolor	fg;		/* **R ONLY** Foreground Color */
	int	font;		/* Text font */
	double	gamma;		/* Device Gamma Correction */
	int	lab[3];		/* Axis labelling */
	int	las;		/* Label style (rotation) */
	int	lty;		/* Line texture */
	double	lwd;		/* Line width */
	double	mgp[3];		/* Annotation location */
	double	mkh;		/* Mark size in inches */
	int	pch;		/* Plotting character */
	int	ps;		/* Text & symbol pointsize */
	int	smo;		/* Curve smoothness */
	double	srt;		/* String Rotation */
	double	tck;		/* Tick size */
	double	tmag;		/* **R ONLY** Title Magnification */
	int	type;		/* type of plot desired */
	double	xaxp[3];	/* X Axis annotation */
	int	xaxs;		/* X Axis style */
	int	xaxt;		/* X Axis type */
	int	xpd;		/* Clip to plot region indicator */
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

	double	fig[4];		/* Figure size (proportion) */
	double	mar[4];		/* Plot margins in lines */
	double	mex;		/* Margin expansion factor */
	int	mfind;		/* By row/col indicator */
	int	mfg[4];		/* Multiple figure parameters */
	int	new;		/* Clean plot ? */
	double	oma[4];		/* Outer margins in lines */
	double	plt[4];		/* Plot size (proportions) */
	int	pty;		/* Plot type */
	double	usr[4];		/* Graphics window */

		/* State Parameters */

	double xlast;		/* x last point */
	double ylast;		/* y last point */

		/* Window Mappings */

	GTrans	win2fig;	/* Window to figure mapping */
	GTrans	fig2dev;	/* Figure to device */
	GTrans 	inner2dev;	/* Inner region to device */
	GTrans	ndc2dev;	/* NDC to raw device */

} GPar;

char *R_alloc(long, int);
char *vmaxget(void);
void vmaxset(char*);

extern int  DevInit;
extern int  (*DevOpen)();
extern void (*DevClose)(void);
extern void (*DevResize)(void);
extern void (*DevNewPlot)(void);
extern void (*DevClip)();
extern void (*DevStartPath)();
extern double (*DevStrWidth)();
extern void (*DevEndPath)();
extern void (*DevMoveTo)();
extern void (*DevLineTo)();
extern void (*DevText)();
extern void (*DevDot)();
extern void (*DevRect)();
extern void (*DevCircle)();
extern void (*DevPolygon)();
extern int  (*DevLocator)();
extern void (*DevMode)();
extern void (*DevHold)();
extern void (*DevSavePlot)();
extern void (*DevPrintPlot)();
extern void (*DevMetricInfo)();
#ifdef OLD
extern void (*DevColor)();
extern void (*DevFont)();
extern void (*DevLinetype)();
#endif

extern GPar GParams;	/* current parameters */
extern GPar DParams;	/* default parameters */
extern GPar *GP;
extern GPar *DP;

	/* World -> Figure Mapping */
#define XMAP(x) (GP->win2fig.ax+GP->win2fig.bx*(x))
#define YMAP(y) (GP->win2fig.ay+GP->win2fig.by*(y))

	/* Figure -> Device Mapping */
#define XFMAP(x) (GP->fig2dev.ax+GP->fig2dev.bx*(x))
#define YFMAP(y) (GP->fig2dev.ay+GP->fig2dev.by*(y))

	/* Inner -> Device Mapping */
#define XIMAP(x) (GP->inner2dev.ax+GP->inner2dev.bx*(x))
#define YIMAP(y) (GP->inner2dev.ay+GP->inner2dev.by*(y))

	/* NDC -> Device Mapping */
#define XNMAP(x) (GP->ndc2dev.ax+GP->ndc2dev.bx*(x))
#define YNMAP(y) (GP->ndc2dev.ay+GP->ndc2dev.by*(y))


/* User Callable Functions */
void DevNull(void);
void GArrow(double, double, double, double, double, double, int);
void GBox(int);
void GCheckState();
void GClip(void);
void GEndPath(void);
double GExpressionHeight(SEXP, int);
double GExpressionWidth(SEXP, int);
void GForceClip(void);
int  GGetIndex(unsigned int);
void GGetRGB(unsigned int, int*, int*, int*);
void GInit(void);
void GLPretty(double*, double*, int*);
void GLineTo(double, double);
int  GLocator(double*, double*, int);
#ifdef NEW
void GMapFig2Dev(void);
void GMapInner2Dev(void);
void GMapNDC2Dev(void);
void GMapWin2Fig(void);
#endif
void GMapping(int);
void GMetricInfo(int, double*, double*, double*, int);
void GMode(int);
void GMoveTo(double, double);
void GMtext(char*, int, double, int, double, int);
void GNewPlot(void);
void GPolygon(int, double*, double*, int, int, int, double *work);
void GPretty(double*, double*, int*);
void GRect(double, double, double, double, int, int);
void GCircle(double x, double y, double radius, int col, int border);
void GReset(void);
void GRestore(void);
void GSavePars(void);
void GSavePlot(char*);
void GPrintPlot(void);
void GRestorePars(void);
void GScale(double, double, int);
void GSetupAxis(int);
int  GSetRGB(unsigned int, unsigned int, unsigned int);
#ifdef NEW
void GSetViewPort(void);
#endif
void GStartPath(void);
void GSetState(int);
double GStrHeight(char*, int);
double GStrWidth(char*, int);
void GSymbol(double, double, int);
void GText(double, double, char*, double, double, double);
void GTitles(char*, char*, char*, char*);
void KillDevice(void);
int SetDevice(char*, char**, int, double*, int);
double xChartoInch(double);
double xChartoNDC(double);
double xFigtoInch(double);
double xInchtoChar(double);
double xInchtoFig(double);
double xInchtoNDC(double);
double xNDCtoChar(double);
double xNDCtoInch(double);
double xUsrtoInch(double);
double yChartoInch(double);
double yChartoNDC(double);
double yFigtoInch(double);
double yInchtoChar(double);
double yInchtoFig(double);
double yInchtoNDC(double);
double yNDCtoChar(double);
double yNDCtoInch(double);
double yUsrtoInch(double);

#endif
