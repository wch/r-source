/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012  R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

/* Internal header, not installed */

#ifndef GRAPHICS_H_
#define GRAPHICS_H_

#include <R_ext/Boolean.h>

#include <R_ext/GraphicsEngine.h>
/* needed for R_GE_lineend/join, R_GE_gcontext */

#define R_GRAPHICS 1
#include <Rgraphics.h> /* RUnit */

/* base.c, graphics.c, par.c */
#define MAX_LAYOUT_ROWS 200
#define MAX_LAYOUT_COLS 200
#define MAX_LAYOUT_CELLS 10007 /* must be less than 65535,
				3 copies, 3bytes each */

typedef struct {
	double ax;
	double bx;
	double ay;
	double by;
} GTrans;

typedef struct {
    /* Plot State */
    /*
       When the device driver is started this is 0
       After the first call to plot.new/perps it is 1
       Every graphics operation except plot.new/persp
       should fail if state = 0
       This is checked at the highest internal function
       level (e.g., do_lines, do_axis, do_plot_xy, ...)
    */

    int	state;		/* plot state: 1 if GNewPlot has been called
			   (by plot.new or persp) */
    Rboolean valid;	/* valid layout ?  Used in GCheckState & do_playDL */

    /* GRZ-like Graphics Parameters */
    /* ``The horror, the horror ... '' */
    /* Marlon Brando - Appocalypse Now */

    /* General Parameters -- set and interrogated directly */

    double adj;		/* String adjustment */
    Rboolean ann;	/* Should annotation take place */
    rcolor bg;		/* **R ONLY** Background color */
    char bty;		/* Box type */
    double cex;		/* Character expansion */
    double lheight;     /* Line height
			   The height of a line of text is:
			   ps * cex * lheight */
    rcolor col;		/* Plotting Color */
    double crt;		/* Character/string rotation */
    double din[2];	/* device size in inches */
    int	err;		/* Error repporting level */
    rcolor fg;		/* **R ONLY** Foreground Color */
    char family[201];  /* **R ONLY** Font family
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
    /* Note that ps is never changed, so always the same as dev->startps.
       However, the ps in the graphics context is changed */
    double ps;		/* Text & symbol pointsize */
    int	smo;		/* Curve smoothness */
    double srt;		/* String Rotation */
    double tck;		/* Tick size as in S */
    double tcl;		/* Tick size in "lines" */
    double xaxp[3];	/* X Axis annotation */
			/* [0] = coordinate of lower tick */
			/* [1] = coordinate of upper tick */
			/* [2] = num tick intervals */
			/* almost always used internally */
    char xaxs;		/* X Axis style */
    char xaxt;		/* X Axis type */
    Rboolean xlog;	/* Log Axis for X */
    int	xpd;		/* Clip to plot region indicator */
    int	oldxpd;
    double yaxp[3];	/* Y Axis annotation */
    char yaxs;		/* Y Axis style */
    char yaxt;		/* Y Axis type */
    Rboolean ylog;	/* Log Axis for Y */

    /* Annotation Parameters */

    double cexbase;	/* Base character size */
    double cexmain;	/* Main title size */
    double cexlab;	/* xlab and ylab size */
    double cexsub;	/* Sub title size */
    double cexaxis;	/* Axis label size */

    int	fontmain;	/* Main title font */
    int	fontlab;	/* Xlab and ylab font */
    int	fontsub;	/* Subtitle font */
    int	fontaxis;	/* Axis label fonts */

    rcolor colmain;	/* Main title color */
    rcolor collab;	/* Xlab and ylab color */
    rcolor colsub;	/* Subtitle color */
    rcolor colaxis;	/* Axis label color */

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
    char pty;		/* Plot type */

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
    /* of a graph  (unless it is a call to persp) */

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
} GPar;

/* always remap private functions */
#define copyGPar		Rf_copyGPar
#define FixupCol		Rf_FixupCol
#define FixupLty		Rf_FixupLty
#define FixupLwd		Rf_FixupLwd
#define FixupVFont		Rf_FixupVFont
#define GInit			Rf_GInit
#define labelformat		Rf_labelformat
#define ProcessInlinePars	Rf_ProcessInlinePars
#define recordGraphicOperation	Rf_recordGraphicOperation

/* NOTE: during replays, call == R_NilValue;
   ----  the following adds readability: */
Rboolean GRecording(SEXP, pGEDevDesc);

/* Default the settings for general graphical parameters
 * (i.e., defaults that do not depend on the device type: */
void GInit(GPar*);

void copyGPar(GPar *, GPar *);

 /* from graphics.c, used in par.c */
double R_Log10(double);

/* from par.c, called in plot.c, plot3d.c */
void ProcessInlinePars(SEXP, pGEDevDesc);

/* from device.c */
void recordGraphicOperation(SEXP, SEXP, pGEDevDesc);

/* some functions that plot.c needs to share with plot3d.c */
SEXP FixupCol(SEXP, unsigned int);
SEXP FixupLty(SEXP, int);
SEXP FixupLwd(SEXP, double);
SEXP FixupVFont(SEXP);
SEXP labelformat(SEXP);

/*
 * Function to generate an R_GE_gcontext from Rf_gpptr info
 *
 * from graphics.c, used in plot.c, plotmath.c
 */
void gcontextFromGP(pGEcontext gc, pGEDevDesc dd);

/* From base.c */
#define gpptr Rf_gpptr
#define dpptr Rf_dpptr
GPar* Rf_gpptr(pGEDevDesc dd);
GPar* Rf_dpptr(pGEDevDesc dd);

#endif /* GRAPHICS_H_ */
