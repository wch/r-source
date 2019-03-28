/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2016 The R Core Team
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

#include "grid.h"
#include <math.h>
#include <float.h>
#include <string.h>

/* Function to build a single-value unit SEXP internally.
 * Cannot build units requiring data as yet.
 */
SEXP unit(double value, int unit) 
{
  SEXP units = PROTECT(allocVector(VECSXP, 1));
  SEXP u = SET_VECTOR_ELT(units, 0, allocVector(VECSXP, 3));
  SET_VECTOR_ELT(u, 0, ScalarReal(value));
  SET_VECTOR_ELT(u, 1, R_NilValue);
  SET_VECTOR_ELT(u, 2, ScalarInteger(unit));
  SEXP cl = PROTECT(mkString("unit"));
  classgets(units, cl);
  UNPROTECT(2);
  return units;
}

int isSimpleUnit(SEXP unit) {
	return inherits(unit, "simpleUnit");
}

/* Accessor functions for unit objects
 */
/*
 * This extracts the underlying scalar unit list structure from the unit vector
 */
SEXP unitScalar(SEXP unit, int index) {
	int i = index % LENGTH(unit);
	if (isSimpleUnit(unit)) {
		SEXP newUnit = PROTECT(allocVector(VECSXP, 3));
		SET_VECTOR_ELT(newUnit, 0, Rf_ScalarReal(REAL(unit)[i]));
		SET_VECTOR_ELT(newUnit, 1, R_NilValue);
		SET_VECTOR_ELT(newUnit, 2, Rf_ScalarInteger(INTEGER(getAttrib(unit, install("unit")))[0]));
		UNPROTECT(1);
		return newUnit;
	}
	return VECTOR_ELT(unit, i);
}

double unitValue(SEXP unit, int index) {
	if (isSimpleUnit(unit)) return REAL(unit)[index % LENGTH(unit)];
	return uValue(unitScalar(unit, index));
}

int unitUnit(SEXP unit, int index) {
	if (isSimpleUnit(unit)) return INTEGER(getAttrib(unit, install("unit")))[0];
	return uUnit(unitScalar(unit, index));
}

SEXP unitData(SEXP unit, int index) {
	if (isSimpleUnit(unit)) return R_NilValue;
	return uData(unitScalar(unit, index));
}

/* Old alternative to LENGTH when using that didn't work on all unit struct
 */
int unitLength(SEXP u) {
  return LENGTH(u);
}


/**************************
 * Code for handling "null" units
 **************************
 */

/* Global mode indicators:
 * The value returned for a "null" unit depends on ...
 * (i) whether layout is calling for evaluation of a "pure null" unit
 *     (in which case, the value of the "null" unit is returned)
 * (ii) the sort of arithmetic that is being performed
 *      (in which case, an "identity" value is returned)
 */

/* 
 * Evaluate a "null" _value_ dependent on the evaluation context
 */
static double evaluateNullUnit(double value, double thisCM,
			       int nullLayoutMode, int nullArithmeticMode) {
    double result = value;
    if (!nullLayoutMode)
	switch (nullArithmeticMode) {
	case L_plain:
	case L_adding:
	case L_subtracting:
	case L_summing:
	    result = 0;
	    break;
	case L_multiplying:
	    result = 0;
	    break;
	case L_maximising:
	    result = 0;
	    break;
	case L_minimising:
	    result = thisCM;
	    break;
	}
    return result;
}

/*
 * Evaluate a "null" _unit_ 
 * This is used by layout code to get a single "null" _value_
 * from a pureNullUnit
 *
 * This must ONLY be called on a unit which has passed the 
 * pureNullUnit test below.
 */
double pureNullUnitValue(SEXP unit, int index)
{
  double result = 0;
  int i, n, u = unitUnit(unit, index);
  double temp, value = unitValue(unit, index);
  SEXP data;
  switch(u) {
  case L_SUM:
  	data = unitData(unit, index);
  	n = unitLength(data);
  	for (i = 0; i < n; i++) {
  		result += pureNullUnitValue(data, i);
  	}
  	result *= value;
  	break;
  case L_MIN:
  	data = unitData(unit, index);
  	n = unitLength(data);
  	result = DBL_MAX;
  	for (i = 0; i < n; i++) {
  		temp = pureNullUnitValue(data, i);
  		if (temp < result) result = temp;
  	}
  	result *= value;
  	break;
  case L_MAX:
  	data = unitData(unit, index);
  	n = unitLength(data);
  	result = DBL_MIN;
  	for (i = 0; i < n; i++) {
  		temp = pureNullUnitValue(data, i);
  		if (temp > result) result = temp;
  	}
  	result *= value;
  	break;
  default:
  	result = value;
  	break;
  }
  return result;
}

int pureNullUnit(SEXP unit, int index, pGEDevDesc dd) {
  int i, n, result, u = unitUnit(unit, index);
  if (isArith(u)) {
    SEXP data = unitData(unit, index);
    n = unitLength(data);
    i = 0;
    result = 1;
    while (result && i < n) {
      result = result && pureNullUnit(data, i, dd);
      i += 1;
    }
  } else {  /* Just a plain unit */
	/* Special case:  if "grobwidth" or "grobheight" unit
	 * and width/height(grob) is pure null
	 */
	if (unitUnit(unit, index) == L_GROBWIDTH) {
	    SEXP grob, updatedgrob, width;
	    SEXP widthPreFn, widthFn, widthPostFn, findGrobFn;
	    SEXP R_fcall0, R_fcall1, R_fcall2, R_fcall3;
	    SEXP savedgpar, savedgrob;
	    /*
	     * The data could be a gPath to a grob
	     * In this case, need to find the grob first, and in order
	     * to do that correctly, need to call pre/postDraw code 
	     */
	    PROTECT(grob = unitData(unit, index));
	    PROTECT(savedgpar = gridStateElement(dd, GSS_GPAR));
	    PROTECT(savedgrob = gridStateElement(dd, GSS_CURRGROB));
	    PROTECT(widthPreFn = findFun(install("preDraw"), 
					 R_gridEvalEnv));
	    PROTECT(widthFn = findFun(install("width"), R_gridEvalEnv));
	    PROTECT(widthPostFn = findFun(install("postDraw"), 
					  R_gridEvalEnv));
	    if (inherits(grob, "gPath")) {
		if (isNull(savedgrob)) {
		    PROTECT(findGrobFn = findFun(install("findGrobinDL"), 
						 R_gridEvalEnv));
		    PROTECT(R_fcall0 = lang2(findGrobFn, 
					     getListElement(grob, "name")));
		    grob = eval(R_fcall0, R_gridEvalEnv);
		} else {
		    PROTECT(findGrobFn =findFun(install("findGrobinChildren"), 
						R_gridEvalEnv));
		    PROTECT(R_fcall0 = lang3(findGrobFn, 
					     getListElement(grob, "name"),
					     getListElement(savedgrob, 
							    "children")));
		    grob = eval(R_fcall0, R_gridEvalEnv);
		}
		UNPROTECT(2);
	    }
	    PROTECT(R_fcall1 = lang2(widthPreFn, grob));
            PROTECT(updatedgrob = eval(R_fcall1, R_gridEvalEnv));
	    PROTECT(R_fcall2 = lang2(widthFn, updatedgrob));
	    PROTECT(width = eval(R_fcall2, R_gridEvalEnv));
	    result = pureNullUnit(width, 0, dd);
	    PROTECT(R_fcall3 = lang2(widthPostFn, updatedgrob));
	    eval(R_fcall3, R_gridEvalEnv);
	    setGridStateElement(dd, GSS_GPAR, savedgpar);
	    setGridStateElement(dd, GSS_CURRGROB, savedgrob);
	    UNPROTECT(11);
	} else if (unitUnit(unit, index) == L_GROBHEIGHT) {
	    SEXP grob, updatedgrob, height;
	    SEXP heightPreFn, heightFn, heightPostFn, findGrobFn;
	    SEXP R_fcall0, R_fcall1, R_fcall2, R_fcall3;
	    SEXP savedgpar, savedgrob;
	    /*
	     * The data could be a gPath to a grob
	     * In this case, need to find the grob first, and in order
	     * to do that correctly, need to call pre/postDraw code 
	     */
	    PROTECT(grob = unitData(unit, index));
	    PROTECT(savedgpar = gridStateElement(dd, GSS_GPAR));
	    PROTECT(savedgrob = gridStateElement(dd, GSS_CURRGROB));
	    PROTECT(heightPreFn = findFun(install("preDraw"), 
					 R_gridEvalEnv));
	    PROTECT(heightFn = findFun(install("height"), R_gridEvalEnv));
	    PROTECT(heightPostFn = findFun(install("postDraw"), 
					  R_gridEvalEnv));
	    if (inherits(grob, "gPath")) {
		if (isNull(savedgrob)) {
		    PROTECT(findGrobFn = findFun(install("findGrobinDL"), 
						 R_gridEvalEnv));
		    PROTECT(R_fcall0 = lang2(findGrobFn, 
					     getListElement(grob, "name")));
		    grob = eval(R_fcall0, R_gridEvalEnv);
		} else {
		    PROTECT(findGrobFn =findFun(install("findGrobinChildren"), 
						R_gridEvalEnv));
		    PROTECT(R_fcall0 = lang3(findGrobFn, 
					     getListElement(grob, "name"),
					     getListElement(savedgrob, 
							    "children")));
		    grob = eval(R_fcall0, R_gridEvalEnv);
		}
		UNPROTECT(2);
	    }
	    PROTECT(R_fcall1 = lang2(heightPreFn, grob));
	    PROTECT(updatedgrob = eval(R_fcall1, R_gridEvalEnv));
	    PROTECT(R_fcall2 = lang2(heightFn, updatedgrob));
	    PROTECT(height = eval(R_fcall2, R_gridEvalEnv));
	    result = pureNullUnit(height, 0, dd);
	    PROTECT(R_fcall3 = lang2(heightPostFn, updatedgrob));
	    eval(R_fcall3, R_gridEvalEnv);
	    setGridStateElement(dd, GSS_GPAR, savedgpar);
	    setGridStateElement(dd, GSS_CURRGROB, savedgrob);
	    UNPROTECT(11);
	} else
	    result = unitUnit(unit, index) == L_NULL;
    }
    return result;    
}

/**************************
 * Code for handling "grobwidth" units
 **************************
 */

/* NOTE:  this code calls back to R code to perform 
 * set.gpar operations, which will impact on grid state variables
 * BUT that's ok(ish) because we save and restore the relevant state
 * variables in here so that the overall effect is NULL.
 *
 * FIXME:  OTOH, the calls back to R Code may also perform
 * viewport operations.  Again, we restore state as much as possible,
 * but this can "pollute" the viewport tree in some cases.
 */

double evaluateGrobUnit(double value, SEXP grob,
			double vpwidthCM, double vpheightCM,
			int nullLMode, int nullAMode,
			/*
			 * Evaluation type
			 * 0 = x, 1 = y, 2 = width, 3 = height
			 */
			int evalType,
			pGEDevDesc dd) 
{
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform, savedTransform;
    SEXP currentvp, currentgp;
    SEXP preFn,  postFn, findGrobFn;
    SEXP evalFnx = R_NilValue, evalFny = R_NilValue;
    SEXP R_fcall0, R_fcall1, R_fcall2x, R_fcall2y, R_fcall3;
    SEXP savedgpar, savedgrob, updatedgrob;
    SEXP unitx = R_NilValue, unity = R_NilValue;
    double result = 0.0;
    Rboolean protectedGrob = FALSE;
    /*
     * We are just doing calculations, not drawing, so
     * we don't want anything recorded on the graphics engine DL
     *
     * FIXME:  This should probably be done via a GraphicsEngine.h
     * function call rather than directly playing with dd->recordGraphics
     */
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    /*
     * Save the current viewport transform 
     * (use to convert location relative to current viewport)
     */
    currentvp = gridStateElement(dd, GSS_VP);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 savedTransform, &rotationAngle);
    /* 
     * Save the current gpar state and restore it at the end
     */
    PROTECT(savedgpar = gridStateElement(dd, GSS_GPAR));
    /*
     * Save the current grob and restore it at the end
     */
    PROTECT(savedgrob = gridStateElement(dd, GSS_CURRGROB));
    /*
     * Set up for calling R functions 
     */
    PROTECT(preFn = findFun(install("preDraw"), R_gridEvalEnv));
    switch(evalType) {
    case 0:
    case 1:
	PROTECT(evalFnx = findFun(install("xDetails"), R_gridEvalEnv));
	PROTECT(evalFny = findFun(install("yDetails"), R_gridEvalEnv));
	break;
    case 2:
	PROTECT(evalFnx = findFun(install("width"), R_gridEvalEnv));
	break;
    case 3:
	PROTECT(evalFny = findFun(install("height"), R_gridEvalEnv));
	break;
    case 4:
	PROTECT(evalFny = findFun(install("ascentDetails"), R_gridEvalEnv));
        break;
    case 5:
	PROTECT(evalFny = findFun(install("descentDetails"), R_gridEvalEnv));
        break;
    }
    PROTECT(postFn = findFun(install("postDraw"), R_gridEvalEnv));
    /*
     * If grob is actually a gPath, use it to find an actual grob
     */
    if (inherits(grob, "gPath")) {
	/* 
	 * If the current grob is NULL then we are at the top level
	 * and we search the display list, otherwise we search the 
	 * children of the current grob
	 *
	 * NOTE: assume here that only gPath of depth == 1 are valid
	 */
	if (isNull(savedgrob)) {
	    PROTECT(findGrobFn = findFun(install("findGrobinDL"), 
					 R_gridEvalEnv));
	    PROTECT(R_fcall0 = lang2(findGrobFn, 
				     getListElement(grob, "name")));
	    PROTECT(grob = eval(R_fcall0, R_gridEvalEnv));
	} else {
	    PROTECT(findGrobFn = findFun(install("findGrobinChildren"), 
					 R_gridEvalEnv));
	    PROTECT(R_fcall0 = lang3(findGrobFn, 
				     getListElement(grob, "name"),
				     getListElement(savedgrob, "children")));
	    PROTECT(grob = eval(R_fcall0, R_gridEvalEnv));
	}
	/*
	 * Flag to make sure we UNPROTECT these at the end
	 */
	protectedGrob = TRUE;
    }
    /* Call preDraw(grob) 
     */
    PROTECT(R_fcall1 = lang2(preFn, grob));
    PROTECT(updatedgrob = eval(R_fcall1, R_gridEvalEnv));
    /* 
     * The call to preDraw may have pushed viewports and/or
     * enforced gpar settings, SO we need to re-establish the
     * current viewport and gpar settings before evaluating the
     * width unit.
     * 
     * NOTE:  we are really relying on the grid state to be coherent
     * when we do stuff like this (i.e., not to have changed since
     * we started evaluating the unit [other than the changes we may
     * have deliberately made above by calling preDraw]).  In other
     * words we are relying on no other drawing occurring at the 
     * same time as we are doing this evaluation.  In other other
     * words, we are relying on there being only ONE process
     * (i.e., NOT multi-threaded).
     */
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    fillViewportContextFromViewport(currentvp, &vpc);
    /* Call whatever(grob)
     * to get the unit representing the x/y/width/height
     */
    switch (evalType) {
    case 0:
    case 1:
	/*
	 * When evaluating grobX/grobY, the value of the unit
	 * is an angle that gets passed to xDetails/yDetails
	 */
	{
	    SEXP val;
	    PROTECT(val = ScalarReal(value));
	    PROTECT(R_fcall2x = lang3(evalFnx, updatedgrob, val));
	    PROTECT(unitx = eval(R_fcall2x, R_gridEvalEnv));
	    PROTECT(R_fcall2y = lang3(evalFny, updatedgrob, val));
	    PROTECT(unity = eval(R_fcall2y, R_gridEvalEnv));
	}
	break;
    case 2:
	PROTECT(R_fcall2x = lang2(evalFnx, updatedgrob));
	PROTECT(unitx = eval(R_fcall2x, R_gridEvalEnv));
	break;
    case 3:
    case 4:
    case 5:
	PROTECT(R_fcall2y = lang2(evalFny, updatedgrob));
	PROTECT(unity = eval(R_fcall2y, R_gridEvalEnv));
	break;
    }
    /* 
     * Transform the unit
     * NOTE:  We transform into INCHES so can produce final answer in terms
     * of NPC for original context
     */
    /* Special case for "null" units
     */
    gcontextFromgpar(currentgp, 0, &gc, dd);
    switch(evalType) {
    case 0:
    case 1:
	if (evalType && pureNullUnit(unity, 0, dd)) {
	    result = evaluateNullUnit(pureNullUnitValue(unity, 0), 
				      vpWidthCM,
				      nullLMode, nullAMode);
	} else if (pureNullUnit(unitx, 0, dd)) {
	    result = evaluateNullUnit(pureNullUnitValue(unitx, 0), 
				      vpWidthCM,
				      nullLMode, nullAMode);
	} else {
	    /*
	     * Transform to device (to allow for viewports in grob)
	     * then adjust relative to current viewport.
	     */
	    double xx, yy;
	    LLocation lin, lout;
	    LTransform invt;
	    invTransform(savedTransform, invt);
	    transformLocn(unitx, unity, 0,
			  vpc, &gc,
			  vpWidthCM, vpHeightCM, dd,
			  transform, &xx, &yy);
	    location(xx, yy, lin);
	    trans(lin, invt, lout);
	    xx = locationX(lout);
	    yy = locationY(lout);
	    if (evalType)
		result = yy;
	    else
		result = xx;
	}
	break;
    case 2:
	if (pureNullUnit(unitx, 0, dd)) {
	    result = evaluateNullUnit(pureNullUnitValue(unitx, 0), 
				      vpWidthCM,
				      nullLMode, nullAMode);
	} else {
	    result = transformWidthtoINCHES(unitx, 0, vpc, &gc,
					    vpWidthCM, vpHeightCM,
					    dd);
	}
	break;
    case 3:
    case 4:
    case 5:
	if (pureNullUnit(unity, 0, dd)) {
	    result = evaluateNullUnit(pureNullUnitValue(unity, 0), 
				      vpWidthCM,
				      nullLMode, nullAMode);
	} else {
	    result = transformHeighttoINCHES(unity, 0, vpc, &gc,
					     vpWidthCM, vpHeightCM,
					     dd);
	}
	break;
    }
    /* Call postDraw(grob)
     */
    PROTECT(R_fcall3 = lang2(postFn, updatedgrob));
    eval(R_fcall3, R_gridEvalEnv);
    /* 
     * Restore the saved gpar state and grob
     */
    setGridStateElement(dd, GSS_GPAR, savedgpar);
    setGridStateElement(dd, GSS_CURRGROB, savedgrob);
    if (protectedGrob) 
	UNPROTECT(3);
    switch(evalType) {
    case 0:
    case 1:
	UNPROTECT(14);
	break;
    case 2:
    case 3:
    case 4:
    case 5:
	UNPROTECT(10);
    }
    /* Return the transformed width
     */
    /*
     * If there is an error or user-interrupt in the above
     * evaluation, dd->recordGraphics is set to TRUE
     * on all graphics devices (see GEonExit(); called in errors.c)
     */
    dd->recordGraphics = record;
    return result;
}

double evaluateGrobXUnit(double value, SEXP grob, 
			 double vpheightCM, double vpwidthCM,
			 int nullLMode, int nullAMode,
			 pGEDevDesc dd) 
{
    return evaluateGrobUnit(value, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 0, dd);
}

double evaluateGrobYUnit(double value, SEXP grob, 
			 double vpheightCM, double vpwidthCM,
			 int nullLMode, int nullAMode,
			 pGEDevDesc dd) 
{
    return evaluateGrobUnit(value, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 1, dd);
}

double evaluateGrobWidthUnit(SEXP grob, 
			     double vpheightCM, double vpwidthCM,
			     int nullLMode, int nullAMode,
			     pGEDevDesc dd) 
{
    return evaluateGrobUnit(1, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 2, dd);
}

double evaluateGrobHeightUnit(SEXP grob, 
			     double vpheightCM, double vpwidthCM,
			     int nullLMode, int nullAMode,
			     pGEDevDesc dd) 
{
    return evaluateGrobUnit(1, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 3, dd);
}

double evaluateGrobAscentUnit(SEXP grob, 
                              double vpheightCM, double vpwidthCM,
                              int nullLMode, int nullAMode,
                              pGEDevDesc dd) 
{
    return evaluateGrobUnit(1, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 4, dd);
}

double evaluateGrobDescentUnit(SEXP grob, 
                               double vpheightCM, double vpwidthCM,
                               int nullLMode, int nullAMode,
                               pGEDevDesc dd) 
{
    return evaluateGrobUnit(1, grob, vpheightCM, vpwidthCM, 
			    nullLMode, nullAMode, 5, dd);
}

/**************************
 * TRANSFORMATIONS
 **************************
 */
    
/* Map a value from arbitrary units to INCHES */

/*
 * NULL units are a special case
 * If L_nullLayoutMode = 1 then the value returned is a NULL unit value
 * Otherwise it is an INCHES value
 */
double transform(double value, int unit, SEXP data,
		 double scalemin, double scalemax,
		 const pGEcontext gc,
		 double thisCM, double otherCM,
		 int nullLMode, int nullAMode, pGEDevDesc dd)
{
    double asc, dsc, wid;
    double result = value;
    switch (unit) {
    case L_NPC:      
	result = (result * thisCM)/2.54; /* 2.54 cm per inch */
	break;
    case L_CM: 
	result = result/2.54;
	break;
    case L_INCHES: 
	break;
    /* FIXME:  The following two assume that the pointsize specified
     * by the user is actually the pointsize provided by the
     * device.  This is NOT a safe assumption
     * One possibility would be to do a call to GReset(), just so
     * that mapping() gets called, just so that things like
     * xNDCPerLine are up-to-date, THEN call GStrHeight("M")
     * or somesuch.
     */
    case L_CHAR:
    case L_MYCHAR:  /* FIXME: Remove this when I can */
	result = (result * gc->ps * gc->cex)/72; /* 72 points per inch */
	break;	
    case L_LINES:
    case L_MYLINES: /* FIXME: Remove this when I can */
	result = (result * gc->ps * gc->cex * gc->lineheight)/72;
	break;
    case L_SNPC:        
	if (thisCM <= otherCM)
	    result = (result * thisCM)/2.54;
	else                     
	    result = (result * otherCM)/2.54;
	break;
    case L_MM:
	result = (result/10)/2.54;
	break;
	/* Maybe an opportunity for some constants below here (!) 
	 */
    case L_POINTS:
	result = result/72.27;
	break;
    case L_PICAS:
	result = (result*12)/72.27;
	break;
    case L_BIGPOINTS:
	result = result/72; 
	break;
    case L_DIDA:
	result = result/1157*1238/72.27;
	break;
    case L_CICERO:
	result = result*12/1157*1238/72.27;
	break;
    case L_SCALEDPOINTS:
	result = result/65536/72.27;
	break;
    case L_STRINGWIDTH:
    case L_MYSTRINGWIDTH: /* FIXME: Remove this when I can */
	if (isExpression(data))
	    result = result*
		fromDeviceWidth(GEExpressionWidth(VECTOR_ELT(data, 0), gc, dd),
				GE_INCHES, dd);
	else
	    result = result*
		fromDeviceWidth(GEStrWidth(CHAR(STRING_ELT(data, 0)), 
					   getCharCE(STRING_ELT(data, 0)), 
					   gc, dd),
				GE_INCHES, dd);
	break;
    case L_STRINGHEIGHT:
    case L_MYSTRINGHEIGHT: /* FIXME: Remove this when I can */
	if (isExpression(data))
	    result = result*
		fromDeviceHeight(GEExpressionHeight(VECTOR_ELT(data, 0), 
						    gc, dd),
				 GE_INCHES, dd);
	else
	    /* FIXME: what encoding is this? */
	    result = result*
		fromDeviceHeight(GEStrHeight(CHAR(STRING_ELT(data, 0)), -1,
					     gc, dd),
				 GE_INCHES, dd);
	break;
    case L_STRINGASCENT:        
	if (isExpression(data))
            GEExpressionMetric(VECTOR_ELT(data, 0), gc, 
                               &asc, &dsc, &wid,
                               dd);
	else
            GEStrMetric(CHAR(STRING_ELT(data, 0)), 
                        getCharCE(STRING_ELT(data, 0)), gc,
                        &asc, &dsc, &wid,
                        dd);
        result = result*fromDeviceHeight(asc, GE_INCHES, dd);
	break;
    case L_STRINGDESCENT:        
	if (isExpression(data))
            GEExpressionMetric(VECTOR_ELT(data, 0), gc, 
                               &asc, &dsc, &wid,
                               dd);
	else
            GEStrMetric(CHAR(STRING_ELT(data, 0)), 
                        getCharCE(STRING_ELT(data, 0)), gc,
                        &asc, &dsc, &wid,
                        dd);
        result = result*fromDeviceHeight(dsc, GE_INCHES, dd);
	break;
    case L_GROBX:
	result = evaluateGrobXUnit(value, data, thisCM, otherCM,
				   nullLMode, nullAMode, dd);
	break;	
    case L_GROBY:
	result = evaluateGrobYUnit(value, data, otherCM, thisCM,
				   nullLMode, nullAMode, dd);
	break;	
    case L_GROBWIDTH:
	result = value*evaluateGrobWidthUnit(data, thisCM, otherCM,
					     nullLMode, nullAMode, dd);
	break;
    case L_GROBHEIGHT:
	result = value*evaluateGrobHeightUnit(data, otherCM, thisCM,
					      nullLMode, nullAMode, dd);
	break;
    case L_GROBASCENT:
	result = value*evaluateGrobAscentUnit(data, otherCM, thisCM,
					      nullLMode, nullAMode, dd);
	break;
    case L_GROBDESCENT:
	result = value*evaluateGrobDescentUnit(data, otherCM, thisCM,
                                               nullLMode, nullAMode, dd);
	break;
    case L_NULL:
	result = evaluateNullUnit(result, thisCM, nullLMode, nullAMode);
	break;
    default:
	error(_("invalid unit or unit not yet implemented"));
    }
    /*
     * For physical units, scale the result by GSS_SCALE (a "zoom" factor)
     */
    switch (unit) {
    case L_INCHES:
    case L_CM:
    case L_MM:
    case L_POINTS:
    case L_PICAS:
    case L_BIGPOINTS:
    case L_DIDA:
    case L_CICERO:
    case L_SCALEDPOINTS:
      result = result * REAL(gridStateElement(dd, GSS_SCALE))[0];
      break;
    default:
      /*
       * No need to scale relative coordinates (NPC, NATIVE, NULL)
       * CHAR and LINES already scaled because of scaling in gcontextFromGPar()
       * Ditto STRINGWIDTH/HEIGHT
       * GROBWIDTH/HEIGHT recurse into here so scaling already done
       */
      break;
    }
    return result;
}

/* FIXME:  scales are only linear at the moment */
double transformLocation(double location, int unit, SEXP data,
			 double scalemin, double scalemax,
			 const pGEcontext gc,
			 double thisCM, double otherCM,
			 int nullLMode, int nullAMode, pGEDevDesc dd)
{
    double result = location;
    switch (unit) {
    case L_NATIVE:
	/* It is invalid to create a viewport with identical limits on scale
         * so we are protected from divide-by-zero
         */
	result = ((result - scalemin)/(scalemax - scalemin))*thisCM/2.54;
	break;
    default:
	result = transform(location, unit, data, scalemin, scalemax,
			   gc, thisCM, otherCM, nullLMode, nullAMode, dd);
    }
    return result;
}

double transformX(SEXP x, int index,
		  LViewportContext vpc,
		  const pGEcontext gc,
		  double widthCM, double heightCM,
		  int nullLMode, int nullAMode, pGEDevDesc dd)
{
  double result;
  int i, n, nullamode, unit = unitUnit(x, index);
  double temp, value = unitValue(x, index);
  SEXP data = unitData(x, index);
  switch (unit) {
  case L_SUM:
  	n = unitLength(data);
  	result = 0.0;
  	for (i = 0; i < n; i++) {
  		result += transformX(data, i, vpc, gc,
                         widthCM, heightCM,
                         nullLMode, L_summing, dd);
  	}
  	result *= value;
  	break;
  case L_MIN:
  	n = unitLength(data);
  	result = DBL_MAX;
  	for (i = 0; i < n; i++) {
  		temp = transformX(data, i, vpc, gc,
                      widthCM, heightCM,
                      nullLMode, L_minimising,
                      dd);
  		if (temp < result) result = temp;
  	}
  	result *= value;
  	break;
  case L_MAX:
  	n = unitLength(data);
  	result = DBL_MIN;
  	for (i = 0; i < n; i++) {
  		temp = transformX(data, i, vpc, gc,
                      widthCM, heightCM,
                      nullLMode, L_maximising,
                      dd);
  		if (temp > result)
  			result = temp;
  	}
  	result *= value;
  	break;
  default:
  	nullamode = nullAMode ? nullAMode : L_plain;
  	result = transformLocation(value, unit, data,
                              vpc.xscalemin, vpc.xscalemax, gc,
                              widthCM, heightCM,
                              nullLMode,
                              nullamode,
                              dd);
  }
  
  return result;
}

double transformY(SEXP y, int index, 
		  LViewportContext vpc,
		  const pGEcontext gc,
		  double widthCM, double heightCM,
		  int nullLMode, int nullAMode, pGEDevDesc dd)
{
	double result;
	int i, n, nullamode, unit = unitUnit(y, index);
	double temp, value = unitValue(y, index);
	SEXP data = unitData(y, index);
	switch (unit) {
	case L_SUM:
		n = unitLength(data);
		result = 0.0;
		for (i = 0; i < n; i++) {
			result += transformY(data, i, vpc, gc,
                        widthCM, heightCM,
                        nullLMode, L_summing, dd);
		}
		result *= value;
		break;
	case L_MIN:
		n = unitLength(data);
		result = DBL_MAX;
		for (i = 0; i < n; i++) {
			temp = transformY(data, i, vpc, gc,
                     widthCM, heightCM,
                     nullLMode, L_minimising,
                     dd);
			if (temp < result) result = temp;
		}
		result *= value;
		break;
	case L_MAX:
		n = unitLength(data);
		result = DBL_MIN;
		for (i = 0; i < n; i++) {
			temp = transformY(data, i, vpc, gc,
                     widthCM, heightCM,
                     nullLMode, L_maximising,
                     dd);
			if (temp > result)
				result = temp;
		}
		result *= value;
		break;
	default:
		nullamode = nullAMode ? nullAMode : L_plain;
		result = transformLocation(value, unit, data,
                             vpc.yscalemin, vpc.yscalemax, gc,
                             heightCM, widthCM,
                             nullLMode,
                             nullamode,
                             dd);
	}
	
	return result;
}

double transformDimension(double dim, int unit, SEXP data,
			  double scalemin, double scalemax,
			  const pGEcontext gc,
			  double thisCM, double otherCM,
			  int nullLMode, int nullAMode,
			  pGEDevDesc dd)
{
    double result = dim;
    switch (unit) {
    case L_NATIVE:
	/* It is invalid to create a viewport with identical limits on scale
         * so we are protected from divide-by-zero
         */
	result = ((dim)/(scalemax - scalemin))*thisCM/2.54;
	break;
    default:
	result = transform(dim, unit, data, scalemin, scalemax, gc,
			   thisCM, otherCM, nullLMode, nullAMode, dd);
    }
    return result;
}
double transformWidth(SEXP width, int index, 
		      LViewportContext vpc,
		      const pGEcontext gc,
		      double widthCM, double heightCM,
		      int nullLMode, int nullAMode, pGEDevDesc dd)
{
	double result;
	int i, n, nullamode, unit = unitUnit(width, index);
	double temp, value = unitValue(width, index);
	SEXP data = unitData(width, index);
	switch (unit) {
	case L_SUM:
		n = unitLength(data);
		result = 0.0;
		for (i = 0; i < n; i++) {
			result += transformWidth(data, i, vpc, gc,
                        widthCM, heightCM,
                        nullLMode, L_summing, dd);
		}
		result *= value;
		break;
	case L_MIN:
		n = unitLength(data);
		result = DBL_MAX;
		for (i = 0; i < n; i++) {
			temp = transformWidth(data, i, vpc, gc,
                     widthCM, heightCM,
                     nullLMode, L_minimising,
                     dd);
			if (temp < result) result = temp;
		}
		result *= value;
		break;
	case L_MAX:
		n = unitLength(data);
		result = DBL_MIN;
		for (i = 0; i < n; i++) {
			temp = transformWidth(data, i, vpc, gc,
                     widthCM, heightCM,
                     nullLMode, L_maximising,
                     dd);
			if (temp > result)
				result = temp;
		}
		result *= value;
		break;
	default:
		nullamode = nullAMode ? nullAMode : L_plain;
		result = transformDimension(value, unit, data,
                              vpc.xscalemin, vpc.xscalemax, gc,
                              widthCM, heightCM,
                              nullLMode,
                              nullamode,
                              dd);
	}
	
	return result;
}

double transformHeight(SEXP height, int index,
		       LViewportContext vpc,
		       const pGEcontext gc,
		       double widthCM, double heightCM,
		       int nullLMode, int nullAMode, pGEDevDesc dd)
{
	double result;
	int i, n, nullamode, unit = unitUnit(height, index);
	double temp, value = unitValue(height, index);
	SEXP data = unitData(height, index);
	switch (unit) {
	case L_SUM:
		n = unitLength(data);
		result = 0.0;
		for (i = 0; i < n; i++) {
			result += transformHeight(data, i, vpc, gc,
                            widthCM, heightCM,
                            nullLMode, L_summing, dd);
		}
		result *= value;
		break;
	case L_MIN:
		n = unitLength(data);
		result = DBL_MAX;
		for (i = 0; i < n; i++) {
			temp = transformHeight(data, i, vpc, gc,
                         widthCM, heightCM,
                         nullLMode, L_minimising,
                         dd);
			if (temp < result) result = temp;
		}
		result *= value;
		break;
	case L_MAX:
		n = unitLength(data);
		result = DBL_MIN;
		for (i = 0; i < n; i++) {
			temp = transformHeight(data, i, vpc, gc,
                         widthCM, heightCM,
                         nullLMode, L_maximising,
                         dd);
			if (temp > result)
				result = temp;
		}
		result *= value;
		break;
	default:
		nullamode = nullAMode ? nullAMode : L_plain;
		result = transformDimension(value, unit, data,
                              vpc.yscalemin, vpc.yscalemax, gc,
                              heightCM, widthCM,
                              nullLMode,
                              nullamode,
                              dd);
	}
	
	return result;
}

/* Code for transforming a location in INCHES using a transformation matrix.
 * We work in INCHES so that rotations can be incorporated within the
 * transformation matrix (i.e., the units are the same in both x- and
 * y-directions).
 * INCHES rather than CM because the R graphics engine only has INCHES.
 */

/* The original transform[X | Y | Width | Height] functions
 * were written to transform to NPC.  Rather than muck with them,
 * I am just wrappering them to get the new transformation to INCHES
 * In other words, the reason for the apparent inefficiency here
 * is historical.
 */

/* It is even more inefficient-looking now because I ended up mucking
 * with transform() to return INCHES (to fix bug if width/heightCM == 0)
 * and by then there was too much code that called transformXtoINCHES
 * to be bothered changing calls to it
 */

/* The difference between transform*toINCHES and transformLocn/Dimn 
 * is that the former are just converting from one coordinate system
 * to INCHES;  the latter are converting from INCHES relative to
 * the parent to INCHES relative to the device.
 */
double transformXtoINCHES(SEXP x, int index, 
			  LViewportContext vpc, 
			  const pGEcontext gc,
			  double widthCM, double heightCM,
			  pGEDevDesc dd)
{
    return transformX(x, index, vpc, gc,
		      widthCM, heightCM, 0, 0, dd);
}

double transformYtoINCHES(SEXP y, int index, 
			  LViewportContext vpc,
			  const pGEcontext gc,
			  double widthCM, double heightCM,
			  pGEDevDesc dd)
{
    return transformY(y, index, vpc, gc,
		      widthCM, heightCM, 0, 0, dd);
}

void transformLocn(SEXP x, SEXP y, int index, 
		   LViewportContext vpc,
		   const pGEcontext gc,
		   double widthCM, double heightCM,
		   pGEDevDesc dd,
		   LTransform t,
		   double *xx, double *yy)
{
    LLocation lin, lout;
    /* x and y are unit objects (i.e., values in any old coordinate
     * system) so the first step is to convert them both to CM
     */
    *xx = transformXtoINCHES(x, index, vpc, gc,
			     widthCM, heightCM, dd);
    *yy = transformYtoINCHES(y, index, vpc, gc,
			     widthCM, heightCM, dd);
    location(*xx, *yy, lin);
    trans(lin, t, lout);
    *xx = locationX(lout);
    *yy = locationY(lout);
}

double transformWidthtoINCHES(SEXP w, int index,
			      LViewportContext vpc,
			      const pGEcontext gc,
			      double widthCM, double heightCM,
			      pGEDevDesc dd)
{
    return transformWidth(w, index, vpc, gc,
			  widthCM, heightCM, 0, 0, dd);
}

double transformHeighttoINCHES(SEXP h, int index,
			       LViewportContext vpc,
			       const pGEcontext gc,
			       double widthCM, double heightCM,
			       pGEDevDesc dd)
{
    return transformHeight(h, index, vpc, gc,
			   widthCM, heightCM, 0, 0, dd);
}

void transformDimn(SEXP w, SEXP h, int index, 
		   LViewportContext vpc,
		   const pGEcontext gc,
		   double widthCM, double heightCM,
		   pGEDevDesc dd,
		   double rotationAngle,
		   double *ww, double *hh)
{
    LLocation din, dout;
    LTransform r;
    *ww = transformWidthtoINCHES(w, index, vpc, gc,
				 widthCM, heightCM, dd);
    *hh = transformHeighttoINCHES(h, index, vpc, gc,
				  widthCM, heightCM, dd);
    location(*ww, *hh, din);
    rotation(rotationAngle, r);
    trans(din, r, dout);
    *ww = locationX(dout);
    *hh = locationY(dout);
}

/*
 * ****************************
 * Inverse Transformations
 * ****************************
 */

/*
 * Take a value in inches within the viewport and convert to some
 * other coordinate system
 */

double transformFromINCHES(double value, int unit, 
			   const pGEcontext gc,
			   double thisCM, double otherCM,
			   pGEDevDesc dd)
{
    /*      
     * Convert to NPC
     */
    double result = value;
    switch (unit) {
    case L_NPC:      
	result = result/(thisCM/2.54);
	break;
    case L_CM: 
	result = result*2.54;
	break;
    case L_INCHES: 
	break;
    /* FIXME:  The following two assume that the pointsize specified
     * by the user is actually the pointsize provided by the
     * device.  This is NOT a safe assumption
     * One possibility would be to do a call to GReset(), just so
     * that mapping() gets called, just so that things like
     * xNDCPerLine are up-to-date, THEN call GStrHeight("M")
     * or somesuch.
     */
    case L_CHAR:
	result = (result*72)/(gc->ps*gc->cex);
	break;	
    case L_LINES:
	result = (result*72)/(gc->ps*gc->cex*gc->lineheight);
	break;
    case L_MM:
	result = result*2.54*10;
	break;
	/* Maybe an opportunity for some constants below here (!) 
	 */
    case L_POINTS:
	result = result*72.27;
	break;
    case L_PICAS:
	result = result/12*72.27;
	break;
    case L_BIGPOINTS:
	result = result*72; 
	break;
    case L_DIDA:
	result = result/1238*1157*72.27;
	break;
    case L_CICERO:
	result = result/1238*1157*72.27/12;
	break;
    case L_SCALEDPOINTS:
	result = result*65536*72.27;
	break;
	/*
	 * I'm not sure the remaining ones makes any sense.
	 * For simplicity, these are just forbidden for now.
	 */
    case L_SNPC:        
    case L_MYCHAR:
    case L_MYLINES:
    case L_STRINGWIDTH:
    case L_MYSTRINGWIDTH:
    case L_STRINGHEIGHT:
    case L_MYSTRINGHEIGHT:
    case L_GROBX:
    case L_GROBY:
    case L_GROBWIDTH:
    case L_GROBHEIGHT:
    case L_NULL:
    default:
	error(_("invalid unit or unit not yet implemented"));
    }
    /*
     * For physical units, reverse the scale by GSS_SCALE (a "zoom" factor)
     */
    switch (unit) {
    case L_INCHES:
    case L_CM:
    case L_MM:
    case L_POINTS:
    case L_PICAS:
    case L_BIGPOINTS:
    case L_DIDA:
    case L_CICERO:
    case L_SCALEDPOINTS:
      result = result / REAL(gridStateElement(dd, GSS_SCALE))[0];
      break;
    default:
      /*
       * No need to scale relative coordinates (NPC, NATIVE, NULL)
       * All other units forbidden anyway
       */
      break;
    }
    return result;
}

/* 
 * This corresponds to transform[X|Y]toINCHES() because
 * it works only within the current viewport, BUT
 * it is much simpler because it is supplied with a 
 * double value in INCHES (rather than a unit object in
 * an arbitrary unit).
 *
 * For conceptual symmetry, it should probably return a
 * unit object, but it only returns a double value.
 * The construction of a unit object with the appropriate
 * unit must be performed by the calling function (or higher).
 * This is probably easiest done right up in R code.
 */
double transformXYFromINCHES(double location, int unit, 
			     double scalemin, double scalemax,
			     const pGEcontext gc,
			     double thisCM, double otherCM,
			     pGEDevDesc dd)
{
    double result = location;
    /* Special case if "thisCM == 0":
     * If converting FROM relative unit, result will already be zero 
     * so leave it there.
     * If converting FROM absolute unit that is zero, ditto.
     * Otherwise (converting FROM non-zero absolute unit), 
     * converting to relative unit is an error.
     */
    if ((unit == L_NATIVE || unit == L_NPC) &&
        thisCM < 1e-6) {
        if (result != 0)
            error(_("Viewport has zero dimension(s)"));
    } else {
        switch (unit) {
        case L_NATIVE:
            result = scalemin + (result/(thisCM/2.54))*(scalemax - scalemin);
            break;        
        default:
            result = transformFromINCHES(location, unit, gc,
                                         thisCM, otherCM, dd);
        }
    }
    return result;
}

double transformWidthHeightFromINCHES(double dimension, int unit, 
				      double scalemin, double scalemax,
				      const pGEcontext gc,
				      double thisCM, double otherCM,
				      pGEDevDesc dd)
{
    double result = dimension;
    /* Special case if "thisCM == 0":
     * If converting FROM relative unit, result will already be zero 
     * so leave it there.
     * If converting FROM absolute unit that is zero, ditto.
     * Otherwise (converting FROM non-zero absolute unit), 
     * converting to relative unit is an error.
     */
    if ((unit == L_NATIVE || unit == L_NPC) &&
        thisCM < 1e-6) {
        if (result != 0)
            error(_("Viewport has zero dimension(s)"));
    } else {
        switch (unit) {
        case L_NATIVE:       
            result = (result/(thisCM/2.54))*(scalemax - scalemin);
            break;
        default:
            result = transformFromINCHES(dimension, unit, gc,
                                         thisCM, otherCM, dd);
        }
    }
    return result;
}

/*
 * Special case conversion from relative unit to relative unit,
 * only used when relevant widthCM or heightCM is zero, so
 * we cannot transform thru INCHES (or we get divide-by-zero)
 *
 * Protected from divide-by-zero here because viewport with
 * identical scale limits is disallowed.
 */
double transformXYtoNPC(double x, int from, double min, double max)
{
    double result = x;
    switch (from) {
    case L_NPC:
        break;
    case L_NATIVE:
        result = (x - min)/(max - min);
        break;
    default:
        error(_("Unsupported unit conversion"));
    }
    return(result);
}

double transformWHtoNPC(double x, int from, double min, double max)
{
    double result = x;
    switch (from) {
    case L_NPC:
        break;
    case L_NATIVE:
        result = x/(max - min);
        break;
    default:
        error(_("Unsupported unit conversion"));
    }
    return(result);
}

double transformXYfromNPC(double x, int to, double min, double max)
{
    double result = x;
    switch (to) {
    case L_NPC:
        break;
    case L_NATIVE:
        result = min + x*(max - min);
        break;
    default:
        error(_("Unsupported unit conversion"));
    }
    return(result);
}

double transformWHfromNPC(double x, int to, double min, double max)
{
    double result = x;
    switch (to) {
    case L_NPC:
        break;
    case L_NATIVE:
        result = x*(max - min);
        break;
    default:
        error(_("Unsupported unit conversion"));
    }
    return(result);
}

/* Attempt to make validating units faster
 */
typedef struct {
    char *name;
    int code;
} UnitTab;

/* NOTE this table must match the order in grid.h
 */
static UnitTab UnitTable[] = {
    { "npc",            0 },
    { "cm",             1 },
    { "inches",         2 },
    { "lines",          3 },
    { "native",         4 },
    { "null",           5 },
    { "snpc",           6 },
    { "mm",             7 },
    { "points",         8 },
    { "picas",          9 },
    { "bigpts",        10 },
    { "dida",          11 },
    { "cicero",        12 },
    { "scaledpts",     13 },
    { "strwidth",      14 },
    { "strheight",     15 },
    { "strascent",     16 },
    { "strdescent",    17 },

    { "char",          18 },
    { "grobx",         19 },
    { "groby",         20 },
    { "grobwidth",     21 },
    { "grobheight",    22 },
    { "grobascent",    23 },
    { "grobdescent",   24 },

    { "mylines",       103 },
    { "mychar",        104 },
    { "mystrwidth",    105 },
    { "mystrheight",   106 },

    { "sum",           201 },
    { "min",           202 },
    { "max",           203 },

    /*
     * Some pseudonyms 
     */
    { "centimetre",  1001 },
    { "centimetres", 1001 },
    { "centimeter",  1001 },
    { "centimeters", 1001 },
    { "in",          1002 },
    { "inch",        1002 },
    { "line",        1003 },
    { "millimetre",  1007 },
    { "millimetres", 1007 },
    { "millimeter",  1007 },
    { "millimeters", 1007 },
    { "point",       1008 },
    { "pt",          1008 },

    { NULL,            -1 }
};

int convertUnit(SEXP unit, int index) 
{
    int i = 0;
    int result = 0;
    int found = 0;
    while (result >= 0 && !found) {
	if (UnitTable[i].name == NULL) 
	    result = -1;
	else {
	    found = !strcmp(CHAR(STRING_ELT(unit, index)), UnitTable[i].name);
	    if (found) {
		result = UnitTable[i].code;
                /* resolve pseudonyms */
                if (result > 1000) {
                    result = result - 1000;
                }
            }
	}
	i += 1;
    }
    if (result < 0)
	error(_("Invalid unit"));
    return result;
}
	    
SEXP validUnits(SEXP units) 
{
    int n = LENGTH(units);
    SEXP answer = R_NilValue;
    if (n > 0) {
	if (isString(units)) {
	    PROTECT(answer = allocVector(INTSXP, n));
	    for (int i = 0; i<n; i++) 
		INTEGER(answer)[i] = convertUnit(units, i);
	    UNPROTECT(1);
	} else {
	    error(_("'units' must be character"));
	}
    } else {
	error(_("'units' must be of length > 0"));
    }
    return answer;
}
SEXP validData(SEXP data, SEXP validUnits, int n) {
	int nData = LENGTH(data);
	int nUnit = LENGTH(validUnits);
	int *pValidUnits = INTEGER(validUnits);
	int dataCopied = 0;
	
	if (nData != 1 && nData != n) {
		error(_("data must be either NULL, have length 1, or match the length of the final unit vector"));
	}
	
	for (int i = 0; i < nUnit; i++) {
		SEXP singleData = VECTOR_ELT(data, i % nData);
		int singleUnit = pValidUnits[i % nUnit];
		int unitIsString = isStringUnit(singleUnit);
		int unitIsGrob = isGrobUnit(singleUnit);
		
		if (unitIsString && !Rf_isString(singleData) && !Rf_isLanguage(singleData)) {
			error(_("no string supplied for 'strwidth/height' unit"));
		}
		if (unitIsGrob) {
			if (!Rf_inherits(singleData, "grob") && !Rf_inherits(singleData, "gPath") && !Rf_isString(singleData)) {
				error(_("no 'grob' supplied for 'grobwidth/height' unit"));
			}
			if (Rf_isString(singleData)) {
				if (!dataCopied) {
					data = PROTECT(shallow_duplicate(data));
					dataCopied = 1;
				}
				SEXP fcall = PROTECT(lang2(install("gPath"), singleData));
				singleData = eval(fcall, R_gridEvalEnv);
				SET_VECTOR_ELT(data, i % nData, singleData);
				UNPROTECT(1);
			}
			if (Rf_inherits(singleData, "gPath")) {
				SEXP fcall = PROTECT(lang2(install("depth"), singleData));
				SEXP depth = PROTECT(eval(fcall, R_gridEvalEnv));
				int tooDeep = INTEGER(depth)[0] > 1;
				UNPROTECT(2);
				if (tooDeep) {
					error(_("'gPath' must have depth 1 in 'grobwidth/height' units"));
				}
			}
		}
		if (!unitIsString && !unitIsGrob && singleData != R_NilValue) {
			error(_("non-NULL value supplied for plain unit"));
		}
	}
	UNPROTECT(dataCopied);
	return data;
}
void makeSimpleUnit(SEXP values, SEXP unit) {
	setAttrib(values, install("unit"), unit);
	SEXP classes = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(classes, 0, mkChar("simpleUnit"));
	SET_STRING_ELT(classes, 1, mkChar("unit"));
	classgets(values, classes);
	UNPROTECT(1);
}
SEXP constructUnits(SEXP amount, SEXP data, SEXP unit) {
	int nAmount = LENGTH(amount);
	int nData = LENGTH(data);
	int nUnit = LENGTH(unit);
	SEXP valUnits = PROTECT(validUnits(unit));
	if (nUnit == 1) {
		int u = INTEGER(valUnits)[0];
		if (!(isStringUnit(u) || isGrobUnit(u))) {
			int referenced = MAYBE_REFERENCED(amount);
			if (referenced) amount = PROTECT(duplicate(amount));
			makeSimpleUnit(amount, valUnits);
			UNPROTECT(1 + referenced);
			return amount;
		}
	}
	int n = nAmount < nUnit ? nUnit : nAmount;
	SEXP units = PROTECT(allocVector(VECSXP, n));
	
	data = validData(data, valUnits, n);
	
	double* pAmount = REAL(amount);
	int *pValUnits = INTEGER(valUnits);
	for (int i = 0; i < n; i++) {
		SEXP unit = SET_VECTOR_ELT(units, i, allocVector(VECSXP, 3));
		SET_VECTOR_ELT(unit, 0, Rf_ScalarReal(pAmount[i % nAmount]));
		SET_VECTOR_ELT(unit, 1, VECTOR_ELT(data, i % nData));
		SET_VECTOR_ELT(unit, 2, Rf_ScalarInteger(pValUnits[i % nUnit]));
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(units, cl);
	UNPROTECT(3);
	return units;
}
SEXP asUnit(SEXP simpleUnit) {
	if (inherits(simpleUnit, "unit")) {
		if (!inherits(simpleUnit, "simpleUnit")) {
			return simpleUnit;
		}
	} else {
		error(_("object is not coercible to a unit"));
	}
	int n = LENGTH(simpleUnit);
	SEXP units = PROTECT(allocVector(VECSXP, n));
	double* pAmount = REAL(simpleUnit);
	SEXP valUnit = getAttrib(simpleUnit, install("unit"));
	for (int i = 0; i < n; i++) {
		SEXP unit = SET_VECTOR_ELT(units, i, allocVector(VECSXP, 3));
		SET_VECTOR_ELT(unit, 0, Rf_ScalarReal(pAmount[i]));
		SET_VECTOR_ELT(unit, 1, R_NilValue);
		SET_VECTOR_ELT(unit, 2, valUnit);
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(units, cl);
	UNPROTECT(2);
	return units;
}
SEXP conformingUnits(SEXP unitList) {
	int n = LENGTH(unitList);
	int unitType;
	SEXP uAttrib = install("unit");
	for (int i = 0; i < n; i++) {
		SEXP unit = VECTOR_ELT(unitList, i);
		if (!inherits(unit, "unit")) error(_("object is not a unit"));
		if (!inherits(unit, "simpleUnit")) return R_NilValue;
		int tempUnit = INTEGER(getAttrib(unit, uAttrib))[0];
		if (i == 0) unitType = tempUnit;
		else if (unitType != tempUnit) return R_NilValue;
	}
	return Rf_ScalarInteger(unitType);
}

SEXP matchUnit(SEXP units, SEXP unit) {
	int n = unitLength(units);
	int unitInt = INTEGER(unit)[0];
	int count = 0;
	SEXP matches = PROTECT(allocVector(INTSXP, n));
	for (int i = 0; i < n; i++) {
		if (unitUnit(units, i) == unitInt) {
			INTEGER(matches)[count] = i + 1;
			count++;
		}
	}
	SETLENGTH(matches, count);
	UNPROTECT(1);
	return matches;
}

int allAbsolute(SEXP units) {
	int all = 1;
	int n = unitLength(units);
	
	for (int i = 0; i < n; i++) {
		int u = unitUnit(units, i);
		if (isArith(u)) {
			all = allAbsolute(unitData(units, i));
		} else {
			all = isAbsolute(u);
		}
		if (!all) break;
	}
	
	return all;
}

SEXP absoluteUnits(SEXP units) {
	int n = unitLength(units);
	if (isSimpleUnit(units)) {
		if (isAbsolute(INTEGER(getAttrib(units, install("unit")))[0])) {
			return units;
		}
		units = PROTECT(allocVector(REALSXP, n));
		double *pVal = REAL(units);
		for (int i = 0; i < n; i++) pVal[i] = 1.0;
		makeSimpleUnit(units, Rf_ScalarInteger(5));
		UNPROTECT(1);
		return units;
	}
	int unitIsAbsolute[n];
	int unitsAllAbsolute = 1;
	for (int i = 0; i < n; i++) {
		int u = unitUnit(units, i);
		if (isArith(u)) {
			unitIsAbsolute[i] = allAbsolute(unitData(units, i));
		} else {
			unitIsAbsolute[i] = isAbsolute(u);
		}
		if (!unitIsAbsolute[i]) unitsAllAbsolute = 0;
	}
	// Early exit avoiding a copy
	if (unitsAllAbsolute) return units;
	
	SEXP absolutes = PROTECT(allocVector(VECSXP, n));
	SEXP nullUnit = PROTECT(allocVector(VECSXP, 3));
	SET_VECTOR_ELT(nullUnit, 0, Rf_ScalarReal(1.0));
	SET_VECTOR_ELT(nullUnit, 1, R_NilValue);
	SET_VECTOR_ELT(nullUnit, 2, Rf_ScalarInteger(5));
	for (int i = 0; i < n; i++) {
		SEXP unit;
		if (unitIsAbsolute[i]) {
			unit = PROTECT(shallow_duplicate(unitScalar(units, i)));
		} else if (isArith(unitUnit(units, i))) {
			unit = PROTECT(allocVector(VECSXP, 3));
			SET_VECTOR_ELT(unit, 0, VECTOR_ELT(VECTOR_ELT(units, i), 0));
			SET_VECTOR_ELT(unit, 1, absoluteUnits(unitData(units, i)));
			SET_VECTOR_ELT(unit, 2, VECTOR_ELT(VECTOR_ELT(units, i), 2));
		} else {
			unit = PROTECT(shallow_duplicate(nullUnit));
		}
		SET_VECTOR_ELT(absolutes, i, unit);
		UNPROTECT(1);
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(absolutes, cl);
	UNPROTECT(3);
	return absolutes;
}
SEXP multUnit(SEXP unit, double value) {
	SEXP mult = PROTECT(shallow_duplicate(unit));
	SET_VECTOR_ELT(mult, 0, Rf_ScalarReal(value * uValue(mult)));
	UNPROTECT(1);
	return mult;
}
SEXP multUnits(SEXP units, SEXP values) {
	int nValues = LENGTH(values);
	int n = LENGTH(units) < nValues ? nValues : LENGTH(units);
	SEXP multiplied = PROTECT(allocVector(VECSXP, n));
	double *pValues = REAL(values);
	
	for (int i = 0; i < n; i++) {
		SEXP unit = PROTECT(unitScalar(units, i));
		SET_VECTOR_ELT(multiplied, i, multUnit(unit, pValues[i % nValues]));
		UNPROTECT(1);
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(multiplied, cl);
	UNPROTECT(2);
	return multiplied;
}
SEXP addUnit(SEXP u1, SEXP u2) {
	SEXP result = PROTECT(allocVector(VECSXP, 3));
	
	double amount1 = uValue(u1);
	double amount2 = uValue(u2);
	int type1 = uUnit(u1);
	int type2 = uUnit(u2);
	SEXP data1 = uData(u1);
	SEXP data2 = uData(u2);
	
	if (type1 == type2 && R_compute_identical(data1, data2, 15)) {
		// Two units are of same type and amount can just be added
		SET_VECTOR_ELT(result, 0, Rf_ScalarReal(amount1 + amount2));
		SET_VECTOR_ELT(result, 1, data1);
		SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(type1));
		UNPROTECT(1);
		return result;
	}
	// Otherwise we construct a summation
	SET_VECTOR_ELT(result, 0, Rf_ScalarReal(1.0));
	SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(L_SUM));
	int isSum1 = type1 == L_SUM;
	int isSum2 = type2 == L_SUM;
	int lengthData1 = isSum1 ? LENGTH(data1) : 1;
	int lengthData2 = isSum2 ? LENGTH(data2) : 1;
	SEXP data = SET_VECTOR_ELT(result, 1, allocVector(VECSXP, lengthData1 + lengthData2));
	// If u1 is a sum unit, add all internal units to final data, otherwise add the unit itself
	if (isSum1) {
		// No need to modify data as value is 1
		if (amount1 == 1.0) {
			for (int j = 0; j < lengthData1; j++) {
				SET_VECTOR_ELT(data, j, unitScalar(data1, j));
			}
		} else { // Multiply the data with the value of the summation unit
			for (int j = 0; j < lengthData1; j++) {
				SEXP dataUnit = PROTECT(unitScalar(data1, j));
				SET_VECTOR_ELT(data, j, multUnit(dataUnit, amount1));
				UNPROTECT(1);
			}
		}
	} else {
		SET_VECTOR_ELT(data, 0, u1);
	}
	// Same as above but for u2
	if (isSum2) {
		if (amount2 == 1.0) {
			for (int j = 0; j < lengthData2; j++) {
				SET_VECTOR_ELT(data, j + lengthData1, unitScalar(data2, j));
			}
		} else {
			for (int j = 0; j < lengthData2; j++) {
				SEXP dataUnit = PROTECT(unitScalar(data2, j));
				SET_VECTOR_ELT(data, j + lengthData1, multUnit(dataUnit, amount2));
				UNPROTECT(1);
			}
		}
	} else {
		SET_VECTOR_ELT(data, lengthData1, u2);
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(data, cl);
	
	UNPROTECT(2);
	return result;
}
SEXP addUnits(SEXP u1, SEXP u2) {
	int n = LENGTH(u1) < LENGTH(u2) ? LENGTH(u2) : LENGTH(u1);
	SEXP added = PROTECT(allocVector(VECSXP, n));
	for (int i = 0; i < n; i++) {
		SEXP unit1 = PROTECT(unitScalar(u1, i));
		SEXP unit2 = PROTECT(unitScalar(u2, i));
		SET_VECTOR_ELT(added, i, addUnit(unit1, unit2));
		UNPROTECT(2);
	}
	SEXP cl = PROTECT(mkString("unit"));
	classgets(added, cl);
	UNPROTECT(2);
	return added;
}
SEXP flipUnits(SEXP units) {
	return multUnits(units, Rf_ScalarReal(-1.0));
}
SEXP summaryUnits(SEXP units, SEXP op_type) {
	int n = 0;
	int m = LENGTH(units);
	for (int i = 0; i < m; i++) {
		int nTemp = LENGTH(VECTOR_ELT(units, i));
		n = n < nTemp ? nTemp : n;
	}
	int type = INTEGER(op_type)[0];
	SEXP out = PROTECT(allocVector(VECSXP, n));
	SEXP cl = PROTECT(mkString("unit"));

	int is_type[m];
	int all_type = 1;
	
	for (int i = 0; i < n; i++) {
		int k = 0;
		int first_type, current_type;
		SEXP unit = SET_VECTOR_ELT(out, i, allocVector(VECSXP, 3));
		SEXP first_data;
		for (int j = 0; j < m; j++) {
			SEXP unit_temp = PROTECT(unitScalar(VECTOR_ELT(units, j), i));
			current_type = uUnit(unit_temp);
			if (j == 0) {
				first_type = current_type;
				first_data = uData(unit_temp);
			}
			is_type[j] = current_type == type;
			all_type = j == 0 || (current_type == first_type && R_compute_identical(uData(unit_temp), first_data, 15));
			k += is_type[j] ? LENGTH(uData(unit_temp)) : 1;
			UNPROTECT(1);
		}
		if (all_type) {
			// The units are of same type and amount can just collapsed
			double amount = unitValue(VECTOR_ELT(units, 0), i);
			for (int j = 0; j < m; j++) {
				double amount_temp = unitValue(VECTOR_ELT(units, j), i);
				switch(type) {
				case L_SUM:
					amount += amount_temp;
					break;
				case L_MIN:
					amount = amount < amount_temp ? amount : amount_temp;
					break;
				case L_MAX:
					amount = amount > amount_temp ? amount : amount_temp;
					break;
				}
			}
			SET_VECTOR_ELT(unit, 0, Rf_ScalarReal(amount));
			SET_VECTOR_ELT(unit, 1, unitData(VECTOR_ELT(units, 0), i));
			SET_VECTOR_ELT(unit, 2, Rf_ScalarInteger(current_type));
			continue;
		}
		SET_VECTOR_ELT(unit, 0, Rf_ScalarReal(1.0));
		SET_VECTOR_ELT(unit, 2, Rf_ScalarInteger(type));
		SEXP data = SET_VECTOR_ELT(unit, 1, allocVector(VECSXP, k));
		k = 0;
		for (int j = 0; j < m; j++) {
			SEXP unit_temp = PROTECT(unitScalar(VECTOR_ELT(units, j), i));
			if (is_type[j]) {
				SEXP current_data = uData(unit_temp);
				double amount = uValue(unit_temp);
				for (int jj = 0; jj < LENGTH(current_data); jj++) {
					SEXP inner_data = SET_VECTOR_ELT(data, jj + k, shallow_duplicate(VECTOR_ELT(current_data, jj)));
					SET_VECTOR_ELT(inner_data, 0, Rf_ScalarReal(amount * uValue(inner_data)));
				}
				k += LENGTH(current_data);
			} else {
				SET_VECTOR_ELT(data, k, unit_temp);
				k++;
			}
			UNPROTECT(1);
		}
		classgets(data, cl);
	}
	classgets(out, cl);
	UNPROTECT(2);
	return out;
}
