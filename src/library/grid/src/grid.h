/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003 The R Development Core Team
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 59 Temple Place,
 *  Suite 330, Boston, MA  02111-1307  USA.
 */

#include <Rconfig.h>
#include <Rinternals.h>
#include <Rgraphics.h>  
#include <Rmath.h>

#include <R_ext/Constants.h>
#include <R_ext/GraphicsDevice.h>
#include <R_ext/GraphicsEngine.h>

/* All lattice type names are prefixed with an "L" 
 * All lattice global variable names are prefixe with an "L_" 
 */

/* This information is stored with R's graphics engine so that 
 * grid can have state information per device and grid output can
 * be maintained on multiple devices.
 */

#define GSS_DEVSIZE 0
#define GSS_CURRLOC 1
#define GSS_DL 2
#define GSS_DLINDEX 3
#define GSS_DLON 4
#define GSS_GPAR 5
#define GSS_GPSAVED 6
#define GSS_VP 7
#define GSS_GLOBALINDEX 8
#define GSS_GRIDDEVICE 9
#define GSS_PREVLOC 10
#define GSS_ENGINEDLON 11
#define GSS_CURRGROB 12
#define GSS_ENGINERECORDING 13
#define GSS_ASK 14

/*
 * Structure of a viewport
 */
#define VP_X 0
#define VP_Y 1
#define VP_WIDTH 2
#define VP_HEIGHT 3
#define VP_JUST 4
#define VP_GP 5
#define VP_CLIP 6
#define VP_XSCALE 7
#define VP_YSCALE 8
#define VP_ANGLE 9
#define VP_LAYOUT 10
#define VP_LPOSROW 11
#define VP_LPOSCOL 12
#define VP_VALIDJUST 13
#define VP_VALIDLPOSROW 14
#define VP_VALIDLPOSCOL 15
#define VP_NAME 16
/* 
 * Additional structure of a pushedvp
 */
#define PVP_GPAR 17
#define PVP_TRANS 18
#define PVP_WIDTHS 19
#define PVP_HEIGHTS 20
#define PVP_WIDTHCM 21
#define PVP_HEIGHTCM 22
#define PVP_ROTATION 23
#define PVP_CLIPRECT 24
#define PVP_PARENT 25
#define PVP_CHILDREN 26
#define PVP_DEVWIDTHCM 27
#define PVP_DEVHEIGHTCM 28
#define PVP_PARENTGPAR 29

/*
 * Structure of a layout
 */
#define LAYOUT_NROW 0
#define LAYOUT_NCOL 1
#define LAYOUT_WIDTHS 2
#define LAYOUT_HEIGHTS 3
#define LAYOUT_RESPECT 4
#define LAYOUT_VRESPECT 5
#define LAYOUT_MRESPECT 6
#define LAYOUT_JUST 7
#define LAYOUT_VJUST 8

#define GP_FILL 0
#define GP_COL 1
#define GP_GAMMA 2
#define GP_LTY 3
#define GP_LWD 4
#define GP_CEX 5
#define GP_FONTSIZE 6
#define GP_LINEHEIGHT 7
#define GP_FONT 8
#define GP_FONTFAMILY 9
#define GP_ALPHA 10
#define GP_LINEEND 11
#define GP_LINEJOIN 12
#define GP_LINEMITRE 13
#define GP_LEX 14
/* 
 * Keep fontface at the end because it is never used in C code
 */
#define GP_FONTFACE 15

typedef double LTransform[3][3];

typedef double LLocation[3];

typedef enum {
    L_adding = 1,
    L_subtracting = 2,
    L_summing = 3,
    L_plain = 4,
    L_maximising = 5,
    L_minimising = 6,
    L_multiplying = 7
} LNullArithmeticMode;

/* NOTE: The order of the enums here must match the order of the
 * strings in viewport.R
 */
typedef enum {
    L_NPC = 0,
    L_CM = 1,
    L_INCHES = 2,
    L_LINES = 3,
    L_NATIVE = 4,
    L_NULL = 5, /* only used in layout specifications (?) */
    L_SNPC = 6,
    L_MM = 7,
    /* Some units based on TeX's definition thereof
     */
    L_POINTS = 8,           /* 72.27 pt = 1 in */
    L_PICAS = 9,            /* 1 pc     = 12 pt */
    L_BIGPOINTS = 10,       /* 72 bp    = 1 in */
    L_DIDA = 11,            /* 1157 dd  = 1238 pt */
    L_CICERO = 12,          /* 1 cc     = 12 dd */
    L_SCALEDPOINTS = 13,    /* 65536 sp = 1pt */
    /* Some units which require an object to query for a value.
     */
    L_STRINGWIDTH = 14,
    L_STRINGHEIGHT = 15,
    /* L_LINES now means multiples of the line height.
     * This is multiples of the font size.
     */
    L_CHAR = 18,
    L_GROBWIDTH = 19,
    L_GROBHEIGHT = 20,
    L_MYLINES = 21,
    L_MYCHAR = 22,
    L_MYSTRINGWIDTH = 23,
    L_MYSTRINGHEIGHT = 24
} LUnit;

typedef enum {
    L_LEFT = 0,
    L_RIGHT = 1,
    L_BOTTOM = 2,
    L_TOP = 3,
    L_CENTRE = 4,
    L_CENTER = 5
} LJustification;

/* An arbitrarily-oriented rectangle.
 * The vertices are assumed to be in order going anitclockwise 
 * around the rectangle.
 */
typedef struct {
    double x1;
    double x2;
    double x3;
    double x4;
    double y1;
    double y2;
    double y3;
    double y4;
} LRect;

/* A description of the location of a viewport */
typedef struct {
    SEXP x;
    SEXP y;
    SEXP width;
    SEXP height;
    double hjust;
    double vjust;
} LViewportLocation;

/* Components of a viewport which provide coordinate information
 * for children of the viewport
 */
typedef struct {
    double xscalemin;
    double xscalemax;
    double yscalemin;
    double yscalemax;
} LViewportContext;

/* Evaluation environment */
#ifndef GRID_MAIN
extern SEXP R_gridEvalEnv;
#else
SEXP R_gridEvalEnv;
#endif


/* Functions called by R code
 * (from all over the place)
 */
SEXP L_initGrid(SEXP GridEvalEnv); 
SEXP L_killGrid(); 
SEXP L_gridDirty();
SEXP L_currentViewport(); 
SEXP L_setviewport(SEXP vp, SEXP hasParent);
SEXP L_downviewport(SEXP vp, SEXP strict);
SEXP L_downvppath(SEXP path, SEXP name, SEXP strict);
SEXP L_unsetviewport(SEXP last);
SEXP L_upviewport(SEXP last);
SEXP L_getDisplayList(); 
SEXP L_setDisplayList(SEXP dl); 
SEXP L_getDLelt(SEXP index);
SEXP L_setDLelt(SEXP value);
SEXP L_getDLindex();
SEXP L_setDLindex(SEXP index);
SEXP L_getDLon();
SEXP L_setDLon(SEXP value);
SEXP L_getEngineDLon();
SEXP L_setEngineDLon(SEXP value);
SEXP L_getCurrentGrob();
SEXP L_setCurrentGrob(SEXP value);
SEXP L_getEngineRecording();
SEXP L_setEngineRecording(SEXP value);
SEXP L_getAsk();
SEXP L_setAsk(SEXP value);
SEXP L_currentGPar();
SEXP L_newpagerecording();
SEXP L_newpage();
SEXP L_initGPar();
SEXP L_initViewportStack();
SEXP L_initDisplayList();
SEXP L_convertToNative(SEXP x, SEXP what); 
SEXP L_moveTo(SEXP x, SEXP y);
SEXP L_lineTo(SEXP x, SEXP y);
SEXP L_lines(SEXP x, SEXP y); 
SEXP L_segments(SEXP x0, SEXP y0, SEXP x1, SEXP y1); 
SEXP L_arrows(SEXP x1, SEXP x2, SEXP xnm1, SEXP xn, 
	      SEXP y1, SEXP y2, SEXP ynm1, SEXP yn, 
	      SEXP angle, SEXP length, SEXP ends, SEXP type);
SEXP L_polygon(SEXP x, SEXP y, SEXP index);
SEXP L_circle(SEXP x, SEXP y, SEXP r);
SEXP L_rect(SEXP x, SEXP y, SEXP w, SEXP h, SEXP hjust, SEXP vjust); 
SEXP L_text(SEXP label, SEXP x, SEXP y, SEXP hjust, SEXP vjust, 
	    SEXP rot, SEXP checkOverlap);
SEXP L_points(SEXP x, SEXP y, SEXP pch, SEXP size);
SEXP L_pretty(SEXP scale);
SEXP L_locator();
SEXP L_convert(SEXP x, SEXP whatfrom,
	       SEXP whatto, SEXP unitto);
SEXP L_layoutRegion(SEXP layoutPosRow, SEXP layoutPosCol);

/* From matrix.c */
double locationX(LLocation l);

double locationY(LLocation l);

void copyTransform(LTransform t1, LTransform t2);

void identity(LTransform m);

void translation(double tx, double ty, LTransform m);

void scaling(double sx, double sy, LTransform m);

void rotation(double theta, LTransform m);

void multiply(LTransform m1, LTransform m2, LTransform m);

void location(double x, double y, LLocation v);

void trans(LLocation vin, LTransform m, LLocation vout);

/* From unit.c */
SEXP unit(double value, int unit);

double unitValue(SEXP unit, int index);

int unitUnit(SEXP unit, int index);

SEXP unitData(SEXP unit, int index);

int unitLength(SEXP u);

extern int L_nullLayoutMode;

double pureNullUnitValue(SEXP unit, int index);

int pureNullUnit(SEXP unit, int index, GEDevDesc *dd);

double transformX(SEXP x, int index, LViewportContext vpc, 
		  R_GE_gcontext *gc,
		  double widthCM, double heightCM,
		  int nullLMode, int nullAMode,
		  GEDevDesc *dd);

double transformY(SEXP y, int index, LViewportContext vpc,
		  R_GE_gcontext *gc,
		  double widthCM, double heightCM,
		  int nullLMode, int nullAMode,
		  GEDevDesc *dd);

double transformWidth(SEXP width, int index, LViewportContext vpc,
		      R_GE_gcontext *gc,
		      double widthCM, double heightCM,
		      int nullLMode, int nullAMode,
		      GEDevDesc *dd);

double transformHeight(SEXP height, int index, LViewportContext vpc,
		       R_GE_gcontext *gc,
		       double widthCM, double heightCM,
		       int nullLMode, int nullAMode,
		       GEDevDesc *dd);

double transformXtoINCHES(SEXP x, int index, LViewportContext vpc,
			  R_GE_gcontext *gc,
			  double widthCM, double heightCM,
			  GEDevDesc *dd);

double transformYtoINCHES(SEXP y, int index, LViewportContext vpc,
			  R_GE_gcontext *gc,
			  double widthCM, double heightCM,
			  GEDevDesc *dd);

void transformLocn(SEXP x, SEXP y, int index, LViewportContext vpc,
		   R_GE_gcontext *gc,
		   double widthCM, double heightCM,
		   GEDevDesc *dd,
		   LTransform t,
		   double *xx, double *yy);

double transformWidthtoINCHES(SEXP w, int index, LViewportContext vpc,
			      R_GE_gcontext *gc,
			      double widthCM, double heightCM,
			      GEDevDesc *dd);

double transformHeighttoINCHES(SEXP h, int index, LViewportContext vpc,
			       R_GE_gcontext *gc,
			       double widthCM, double heightCM,
			       GEDevDesc *dd);

void transformDimn(SEXP w, SEXP h, int index, LViewportContext vpc,
		   R_GE_gcontext *gc,
		   double widthCM, double heightCM,
		   GEDevDesc *dd,
		   double rotationAngle,
		   double *ww, double *hh);

double transformXYFromINCHES(double location, int unit, 
			     double scalemin, double scalemax,
			     R_GE_gcontext *gc,
			     double thisCM, double otherCM,
			     GEDevDesc *dd);

double transformWidthHeightFromINCHES(double value, int unit, 
				      double scalemin, double scalemax,
				      R_GE_gcontext *gc,
				      double thisCM, double otherCM,
				      GEDevDesc *dd);

/* From just.c */
double justifyX(double x, double width, double hjust);

double justifyY(double y, double height, double vjust);

double convertJust(int vjust);

void justification(double width, double height, double hjust, double vjust,
		   double *hadj, double *vadj);

/* From util.c */
SEXP getListElement(SEXP list, char *str);

void setListElement(SEXP list, char *str, SEXP value);

SEXP getSymbolValue(char *symbolName);

void setSymbolValue(char *symbolName, SEXP value);

double numeric(SEXP x, int index);

void rect(double x1, double x2, double x3, double x4, 
	  double y1, double y2, double y3, double y4, 
	  LRect *r);

void copyRect(LRect r1, LRect *r);

int intersect(LRect r1, LRect r2);

void textRect(double x, double y, SEXP text, int i,
	      R_GE_gcontext *gc,
	      double xadj, double yadj,
	      double rot, GEDevDesc *dd, LRect *r);

/* From gpar.c */
double gpFontSize(SEXP gp, int i);

double gpLineHeight(SEXP gp, int i);

int gpCol(SEXP gp, int i);

SEXP gpFillSXP(SEXP gp);

int gpFill(SEXP gp, int i);

double gpGamma(SEXP gp, int i);

int gpLineType(SEXP gp, int i);

double gpLineWidth(SEXP gp, int i);

double gpCex(SEXP gp, int i);

int gpFont(SEXP gp, int i);

char* gpFontFamily(SEXP gp, int i);

SEXP gpFontSXP(SEXP gp);

SEXP gpFontFamilySXP(SEXP gp);

SEXP gpFontSizeSXP(SEXP gp);

SEXP gpLineHeightSXP(SEXP gp);

void gcontextFromgpar(SEXP gp, int i, R_GE_gcontext *gc);

void initGPar(GEDevDesc *dd);

/* From viewport.c */
SEXP viewportX(SEXP vp);

SEXP viewportY(SEXP vp);

SEXP viewportWidth(SEXP vp);

SEXP viewportHeight(SEXP vp);

SEXP viewportgpar(SEXP vp);

char* viewportFontFamily(SEXP vp);

int viewportFont(SEXP vp);

double viewportFontSize(SEXP vp);

double viewportLineHeight(SEXP vp);

Rboolean viewportClip(SEXP vp);

SEXP viewportClipRect(SEXP vp);

double viewportXScaleMin(SEXP vp);

double viewportXScaleMax(SEXP vp);

double viewportYScaleMin(SEXP vp);

double viewportYScaleMax(SEXP vp);

double viewportHJust(SEXP v);

double viewportVJust(SEXP vp);

SEXP viewportLayout(SEXP vp);

SEXP viewportParent(SEXP vp);

SEXP viewportTransform(SEXP vp);

SEXP viewportLayoutWidths(SEXP vp);

SEXP viewportLayoutHeights(SEXP vp);

SEXP viewportWidthCM(SEXP vp);

SEXP viewportHeightCM(SEXP vp);

SEXP viewportRotation(SEXP vp);

SEXP viewportParent(SEXP vp);

SEXP viewportChildren(SEXP vp);

SEXP viewportDevWidthCM(SEXP vp);

SEXP viewportDevHeightCM(SEXP vp);

void fillViewportContextFromViewport(SEXP vp, LViewportContext *vpc);

void copyViewportContext(LViewportContext vpc1, LViewportContext *vpc2);

void gcontextFromViewport(SEXP vp, R_GE_gcontext *gc);

void calcViewportTransform(SEXP vp, SEXP parent, Rboolean incremental,
			   GEDevDesc *dd);

void initVP(GEDevDesc *dd);

/* From layout.c */
void calcViewportLayout(SEXP viewport,
			double parentWidthCM,
			double parentHeightCM,
			LViewportContext parentContext,
			R_GE_gcontext *parentgc,
			GEDevDesc *dd);

void calcViewportLocationFromLayout(SEXP layoutPosRow,
				    SEXP layoutPosCol,
				    SEXP parent,
				    LViewportLocation *vpl);

/* From state.c */
void initDL(GEDevDesc *dd);

SEXP gridStateElement(GEDevDesc *dd, int elementIndex);

void setGridStateElement(GEDevDesc *dd, int elementIndex, SEXP value);

SEXP gridCallback(GEevent task, GEDevDesc *dd, SEXP data);

/* From grid.c */
SEXP doSetViewport(SEXP vp, 
		   Rboolean topLevelVP,
		   Rboolean pushing,
		   GEDevDesc *dd);

void getDeviceSize(GEDevDesc *dd, double *devWidthCM, double *devHeightCM); 

GEDevDesc* getDevice();

void getViewportTransform(SEXP currentvp, 
			  GEDevDesc *dd, 
			  double *vpWidthCM, double *vpHeightCM,
			  LTransform transform, double *rotationAngle);


