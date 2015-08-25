/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2008  R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#ifndef RGRAPHICS_H_
#define RGRAPHICS_H_

/* This was a public header in R < 2.8.0, but no longer */

#ifdef  __cplusplus
extern "C" {
#endif

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


#define currentFigureLocation	Rf_currentFigureLocation
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
#define GExpressionHeight	Rf_GExpressionHeight
#define GExpressionWidth	Rf_GExpressionWidth
#define GForceClip		Rf_GForceClip
#define GLine			Rf_GLine
#define GLocator		Rf_GLocator
#define GMapUnits		Rf_GMapUnits
#define GMapWin2Fig		Rf_GMapWin2Fig
#define GMathText		Rf_GMathText
#define GMetricInfo		Rf_GMetricInfo
#define GMMathText		Rf_GMMathText
#define GMode			Rf_GMode
#define GMtext			Rf_GMtext
#define GNewPlot		Rf_GNewPlot
#define GPath   		Rf_GPath
#define GPolygon		Rf_GPolygon
#define GPolyline		Rf_GPolyline
#define GPretty			Rf_GPretty
#define GRect			Rf_GRect
#define GRaster			Rf_GRaster
#define GReset			Rf_GReset
#define GRestore		Rf_GRestore
#define GRestorePars		Rf_GRestorePars
#define GSavePars		Rf_GSavePars
#define GScale			Rf_GScale
#define GSetState		Rf_GSetState
#define GSetupAxis		Rf_GSetupAxis
#define GStrHeight		Rf_GStrHeight
#define GStrWidth		Rf_GStrWidth
#define GSymbol			Rf_GSymbol
#define GText			Rf_GText
#define GVStrHeight		Rf_GVStrHeight
#define GVStrWidth		Rf_GVStrWidth
#define GVText			Rf_GVText

#define xDevtoNDC		Rf_xDevtoNDC
#define xDevtoNFC		Rf_xDevtoNFC
#define xDevtoNPC		Rf_xDevtoNPC
#define xDevtoUsr		Rf_xDevtoUsr
#define xNPCtoUsr		Rf_xNPCtoUsr
#define yDevtoNDC		Rf_yDevtoNDC
#define yDevtoNFC		Rf_yDevtoNFC
#define yDevtoNPC		Rf_yDevtoNPC
#define yDevtoUsr		Rf_yDevtoUsr
#define yNPCtoUsr		Rf_yNPCtoUsr


/*-------------------------------------------------------------------
 *
 *  GPAR FUNCTIONS are concerned with operations on the
 *  entire set of graphics parameters for a device
 *  (e.g., initialisation, saving, and restoring)
 *
 *  From graphics.c, used in plot.c.
 */

/* Reset the current graphical parameters from the default ones: */
void GRestore(pGEDevDesc);
/* Make a temporary copy of the current parameters */
void GSavePars(pGEDevDesc);
/* Restore the temporary copy saved by GSavePars */
void GRestorePars(pGEDevDesc);


/*-------------------------------------------------------------------
 *
 *  DEVICE STATE FUNCTIONS are concerned with getting and setting
 *  the current state of the device;  is it ready to be drawn into?
 *
 *  From graphics.c, used in plot.c.
 */

/* has plot.new been called yet? */
void GCheckState(pGEDevDesc);
/* Set to 1 when plot.new succeeds
 * Set to 0 when don't want drawing to go ahead */
void GSetState(int, pGEDevDesc);

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
 *  From graphics.c, used in plot.c.
 */


/* Draw a circle, centred on (x,y) with radius r (in inches). */
void GCircle(double, double, int, double, int, int, pGEDevDesc);
/* Set clipping region (based on current setting of dd->gp.xpd).
 * Only clip if new clipping region is different from the current one */
void GClip(pGEDevDesc);
/* Polygon clipping: */
int GClipPolygon(double *, double *, int, int, int,
		 double *, double *, pGEDevDesc);
/* Always clips */
void GForceClip(pGEDevDesc);
/* Draw a line from (x1,y1) to (x2,y2): */
void GLine(double, double, double, double, int, pGEDevDesc);
/* Return the location of the next mouse click: */
Rboolean GLocator(double*, double*, int, pGEDevDesc);
/* Return the height, depth, and width of the specified
 * character in the specified units: */
void GMetricInfo(int, double*, double*, double*, GUnit, pGEDevDesc);
/* Set device "mode" (drawing or not drawing) here for windows and mac drivers.
 */
void GMode(int, pGEDevDesc);
/* Draw a path using the specified lists of x and y values: */
void GPath(double*, double*, int, int*, Rboolean, int, int, pGEDevDesc);
/* Draw a polygon using the specified lists of x and y values: */
void GPolygon(int, double*, double*, int, int, int, pGEDevDesc);
/* Draw series of straight lines using the specified lists of x and y values: */
void GPolyline(int, double*, double*, int, pGEDevDesc);
/* Draw a rectangle given two opposite corners: */
void GRect(double, double, double, double, int, int, int, pGEDevDesc);
/* Draw a raster image given two opposite corners: */
void GRaster(unsigned int*, int, int,
             double, double, double, double,
             double, Rboolean, pGEDevDesc);
/* Return the height of the specified string in the specified units: */
double GStrHeight(const char *, cetype_t, GUnit, pGEDevDesc);
/* Return the width of the specified string in the specified units */
double GStrWidth(const char *, cetype_t, GUnit, pGEDevDesc);
/* Draw the specified text at location (x,y) with the specified
 * rotation and justification: */
void GText(double, double, int, const char *, cetype_t, double, double, double,
	   pGEDevDesc);

/* From plotmath.c, used in plot.c */
void GMathText(double, double, int, SEXP, double, double, double, pGEDevDesc);
void GMMathText(SEXP, int, double, int, double, int, double, pGEDevDesc);


/*-------------------------------------------------------------------
 *
 *  GRAPHICAL UTILITIES are functions that produce graphical output
 *  using the graphical primitives (i.e., they are generic - NOT
 *  device-specific).
 *
 *  From graphics.c, used in plot.c.
 */

/* Draw a line from (x1,y1) to (x2,y2) with an arrow head
 * at either or both ends. */
void GArrow(double, double, double, double, int, double, double, int, pGEDevDesc);
/* Draw a box around specified region:
 *  1=plot region, 2=figure region, 3=inner region, 4=device. */
void GBox(int, pGEDevDesc);
/* Return a "nice" min, max and number of intervals for a given
 * range on a linear or _log_ scale, respectively: */
void GPretty(double*, double*, int*); /* used in plot3d.c */
/* Draw text in margins. */
void GMtext(const char *, cetype_t, int, double, int, double, int, double, pGEDevDesc);
/* Draw one of the predefined symbols (circle, square, diamond, ...) */
void GSymbol(double, double, int, int, pGEDevDesc);

/* From plotmath.c, used in plot.c */
double GExpressionHeight(SEXP, GUnit, pGEDevDesc);
double GExpressionWidth(SEXP, GUnit, pGEDevDesc);



/*----------------------------------------------------------------------
 *
 *  TRANSFORMATIONS are concerned with converting locations between
 *  coordinate systems and dimensions between different units.
 *
 *  From graphics.c, used in par.c, plot.c, plot3d.c
 */

/* Convert an R unit (e.g., "user") into an internal unit (e.g., USER)> */
GUnit GMapUnits(int);
/* Convert a LOCATION from one coordinate system to another: */
void GConvert(double*, double*, GUnit, GUnit, pGEDevDesc);
double GConvertX(double, GUnit, GUnit, pGEDevDesc);
double GConvertY(double, GUnit, GUnit, pGEDevDesc);
/* Convert an x/y-dimension from one set of units to another: */
double GConvertXUnits(double, GUnit, GUnit, pGEDevDesc);
double GConvertYUnits(double, GUnit, GUnit, pGEDevDesc);

/* Set up the different regions on a device (i.e., inner region,
 * figure region, plot region) and transformations for associated
 * coordinate systems (called whenever anything that affects the
 * coordinate transformations changes):
 */
void GReset(pGEDevDesc);

/* Set up the user coordinate transformations: */
void GMapWin2Fig(pGEDevDesc);
/* Set up the device for a new plot by Resetting graphics parameters
 * and Resetting the regions and coordinate Systems */
pGEDevDesc GNewPlot(Rboolean);
/* Set up the user coordinates based on the axis limits */
void GScale(double, double, int, pGEDevDesc);
/* Set up the axis limits based on the user coordinates */
void GSetupAxis(int, pGEDevDesc);
/* Return row and column of current figure in the layout matrix */
void currentFigureLocation(int*, int*, pGEDevDesc);

/* which of these conversions should be public? maybe all? [NO_REMAP] */
double xDevtoNDC(double, pGEDevDesc);
double yDevtoNDC(double, pGEDevDesc);
double xDevtoNFC(double, pGEDevDesc);
double yDevtoNFC(double, pGEDevDesc);
double xDevtoNPC(double, pGEDevDesc);
double yDevtoNPC(double, pGEDevDesc);
double xDevtoUsr(double, pGEDevDesc);
double yDevtoUsr(double, pGEDevDesc);
double xNPCtoUsr(double, pGEDevDesc);
double yNPCtoUsr(double, pGEDevDesc);

#ifdef  __cplusplus
}
#endif

#endif /* RGRAPHICS_H_ */
