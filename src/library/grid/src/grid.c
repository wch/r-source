/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2013 The R Core Team
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


#define GRID_MAIN
#include "grid.h"
#include <math.h>
#include <float.h>
#include <string.h>

/* NOTE:
 * The extensive use of L or L_ prefixes dates back to when this 
 * package used to be called "lattice"
 */

void getDeviceSize(pGEDevDesc dd, double *devWidthCM, double *devHeightCM) 
{
    double left, right, bottom, top;
    dd->dev->size(&left, &right, &bottom, &top, dd->dev);
    *devWidthCM = fabs(right - left) * dd->dev->ipr[0] * 2.54;
    *devHeightCM = fabs(top - bottom) * dd->dev->ipr[1] * 2.54;
}

static Rboolean deviceChanged(double devWidthCM, double devHeightCM, 
			      SEXP currentvp)
{
    Rboolean result = FALSE;
    SEXP pvpDevWidthCM, pvpDevHeightCM;
    PROTECT(pvpDevWidthCM = VECTOR_ELT(currentvp, PVP_DEVWIDTHCM));
    PROTECT(pvpDevHeightCM = VECTOR_ELT(currentvp, PVP_DEVHEIGHTCM));
    if (fabs(REAL(pvpDevWidthCM)[0] - devWidthCM) > 1e-6) {
	result = TRUE;
	REAL(pvpDevWidthCM)[0] = devWidthCM;
	SET_VECTOR_ELT(currentvp, PVP_DEVWIDTHCM, pvpDevWidthCM);
    }
    if (fabs(REAL(pvpDevHeightCM)[0] - devHeightCM) > 1e-6) {
	result = TRUE;
	REAL(pvpDevHeightCM)[0] = devHeightCM;
	SET_VECTOR_ELT(currentvp, PVP_DEVHEIGHTCM, pvpDevHeightCM);
    }
    UNPROTECT(2);
    return result;
}

/* Register grid with R's graphics engine
 */
SEXP L_initGrid(SEXP GridEvalEnv) 
{
    R_gridEvalEnv = GridEvalEnv;
    GEregisterSystem(gridCallback, &gridRegisterIndex);
    return R_NilValue;
}

SEXP L_killGrid() 
{
    GEunregisterSystem(gridRegisterIndex);
    return R_NilValue;
}

/* Get the current device (the graphics engine creates one if nec.)
 */
pGEDevDesc getDevice() 
{
    return GEcurrentDevice();
}

/* If this is the first time that a grid operation has occurred for 
 * this device, do some initialisation.
 */
void dirtyGridDevice(pGEDevDesc dd) {
    if (!LOGICAL(gridStateElement(dd, GSS_GRIDDEVICE))[0]) {
	SEXP gsd, griddev;
	/* Record the fact that this device has now received grid output
	 */
	gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
	PROTECT(griddev = allocVector(LGLSXP, 1));
	LOGICAL(griddev)[0] = TRUE;
	SET_VECTOR_ELT(gsd, GSS_GRIDDEVICE, griddev);
	UNPROTECT(1);
	/*
	 * Start the first page on the device
	 * (But only if no other graphics system has not already done so)
	 */
	if (!GEdeviceDirty(dd)) {
	    R_GE_gcontext gc;
	    SEXP currentgp = gridStateElement(dd, GSS_GPAR);
	    gcontextFromgpar(currentgp, 0, &gc, dd);
	    GENewPage(&gc, dd);
	    GEdirtyDevice(dd);
	}
	/*
	 * Only initialise viewport once new page has started
	 * (required for postscript output [at least])
	 */
	initVP(dd);
	initDL(dd);
    }
}

SEXP L_gridDirty()
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    dirtyGridDevice(dd);
    return R_NilValue;
}

void getViewportContext(SEXP vp, LViewportContext *vpc)
{
    fillViewportContextFromViewport(vp, vpc);
}

SEXP L_currentViewport() 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_VP);
}

SEXP doSetViewport(SEXP vp, 
		   /* 
		    * Are we setting the top-level viewport?
		    */
		   Rboolean topLevelVP,
		   /* 
		    * Are we pushing a new viewport?
		    * (or just revisiting an already-pushed viewport?)
		    */
		   Rboolean pushing,
		   pGEDevDesc dd)
{
    int i, j;
    double devWidthCM, devHeightCM;
    double xx1, yy1, xx2, yy2;
    SEXP currentClip, widthCM, heightCM;
    /* Get the current device size 
     */
    getDeviceSize((dd), &devWidthCM, &devHeightCM);
    if (!topLevelVP && pushing) {
	SEXP parent = gridStateElement(dd, GSS_VP);
	/* Set the viewport's parent
	 * Need to do this in here so that redrawing via R BASE display
	 * list works 
	 */
	SET_VECTOR_ELT(vp, PVP_PARENT, parent);
	/*
	 * Make this viewport a child of its parent
	 * This involves assigning a value in the parent's
	 * children slot (which is an environment), using
	 * the viewport's name as the symbol name.
	 * NOTE that we are deliberately using defineVar to
	 * assign the vp SEXP itself, NOT a copy.
	 */
	defineVar(installChar(STRING_ELT(VECTOR_ELT(vp, VP_NAME), 0)),
		  vp, 
		  VECTOR_ELT(parent, PVP_CHILDREN));
    }
    /* Calculate the transformation for the viewport.
     * This will hopefully only involve updating the transformation
     * from the previous viewport.
     * However, if the device has changed size, we will need to
     * recalculate the transformation from the top-level viewport
     * all the way down.
     * NEVER incremental for top-level viewport
     */
    calcViewportTransform(vp, viewportParent(vp), 
			  !topLevelVP &&
			  !deviceChanged(devWidthCM, devHeightCM, 
					 viewportParent(vp)), dd);
    /* 
     * We must "turn off" clipping
     * We set the clip region to be the entire device
     * (actually, as for the top-level viewport, we set it
     *  to be slightly larger than the device to avoid 
     *  "edge effects")
     */
    if (viewportClip(vp) == NA_LOGICAL) {
	xx1 = toDeviceX(-0.5*devWidthCM/2.54, GE_INCHES, dd);
	yy1 = toDeviceY(-0.5*devHeightCM/2.54, GE_INCHES, dd);
	xx2 = toDeviceX(1.5*devWidthCM/2.54, GE_INCHES, dd);
	yy2 = toDeviceY(1.5*devHeightCM/2.54, GE_INCHES, dd);
	GESetClip(xx1, yy1, xx2, yy2, dd);
    }
    /* If we are supposed to clip to this viewport ...
     * NOTE that we will only clip if there is no rotation
     */
    else if (viewportClip(vp)) {
	double rotationAngle = REAL(viewportRotation(vp))[0];
	if (rotationAngle != 0 &&
            rotationAngle != 90 &&
            rotationAngle != 270 &&
            rotationAngle != 360) {
	    warning(_("cannot clip to rotated viewport"));
            /* Still need to set clip region for this viewport.
               So "inherit" parent clip region.
               In other words, 'clip=TRUE' + 'rot=15' = 'clip=FALSE'
            */
            SEXP parentClip;
            PROTECT(parentClip = viewportClipRect(viewportParent(vp)));
            xx1 = REAL(parentClip)[0];
            yy1 = REAL(parentClip)[1];
            xx2 = REAL(parentClip)[2];
            yy2 = REAL(parentClip)[3];
            UNPROTECT(1);
        } else {
	    /* Calculate a clipping region and set it
	     */
	    SEXP x1, y1, x2, y2;
	    LViewportContext vpc;
	    double vpWidthCM = REAL(viewportWidthCM(vp))[0];
	    double vpHeightCM = REAL(viewportHeightCM(vp))[0];
	    R_GE_gcontext gc;
	    LTransform transform;
	    for (i=0; i<3; i++)
		for (j=0; j<3; j++)
		    transform[i][j] = 
			REAL(viewportTransform(vp))[i + 3*j];
	    if (!topLevelVP) {
		PROTECT(x1 = unit(0, L_NPC));
		PROTECT(y1 = unit(0, L_NPC));
		PROTECT(x2 = unit(1, L_NPC));
		PROTECT(y2 = unit(1, L_NPC));
	    } else {
		/* Special case for top-level viewport.
		 * Set clipping region outside device boundaries.
		 * This means that we have set the clipping region to
		 * something, but avoids problems if the nominal device
		 * limits are actually within its physical limits
		 * (e.g., PostScript)
		 */
	        PROTECT(x1 = unit(-.5, L_NPC));
		PROTECT(y1 = unit(-.5, L_NPC));
		PROTECT(x2 = unit(1.5, L_NPC));
		PROTECT(y2 = unit(1.5, L_NPC));
	    }
	    getViewportContext(vp, &vpc);
	    gcontextFromViewport(vp, &gc, dd);
	    transformLocn(x1, y1, 0, vpc, &gc, 
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx1, &yy1);
	    transformLocn(x2, y2, 0, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx2, &yy2);
	    UNPROTECT(4);  /* unprotect x1, y1, x2, y2 */
	    /* The graphics engine only takes device coordinates
	     */
	    xx1 = toDeviceX(xx1, GE_INCHES, dd);
	    yy1 = toDeviceY(yy1, GE_INCHES, dd);
	    xx2 = toDeviceX(xx2, GE_INCHES, dd);
	    yy2 = toDeviceY(yy2, GE_INCHES, dd);
	    GESetClip(xx1, yy1, xx2, yy2, dd);
	}
    } else {
	/* If we haven't set the clipping region for this viewport
	 * we need to save the clipping region from its parent
	 * so that when we pop this viewport we can restore that.
	 */
	/* NOTE that we are relying on grid.R setting clip=TRUE
	 * for the top-level viewport, else *BOOM*!
	 */
	SEXP parentClip;
	PROTECT(parentClip = viewportClipRect(viewportParent(vp)));
	xx1 = REAL(parentClip)[0];
	yy1 = REAL(parentClip)[1];
	xx2 = REAL(parentClip)[2];
	yy2 = REAL(parentClip)[3];
	UNPROTECT(1);
        /* If we are revisiting a viewport that inherits a clip
         * region from a parent viewport, we may need to reset 
         * the clip region (at worst, we generate a redundant clip)
         */
        if (!pushing) {
	    GESetClip(xx1, yy1, xx2, yy2, dd);
        }
    }
    PROTECT(currentClip = allocVector(REALSXP, 4));
    REAL(currentClip)[0] = xx1;
    REAL(currentClip)[1] = yy1;
    REAL(currentClip)[2] = xx2;
    REAL(currentClip)[3] = yy2;
    SET_VECTOR_ELT(vp, PVP_CLIPRECT, currentClip);
    /*
     * Save the current device size
     */
    PROTECT(widthCM = allocVector(REALSXP, 1));
    REAL(widthCM)[0] = devWidthCM;
    SET_VECTOR_ELT(vp, PVP_DEVWIDTHCM, widthCM);
    PROTECT(heightCM = allocVector(REALSXP, 1));
    REAL(heightCM)[0] = devHeightCM;
    SET_VECTOR_ELT(vp, PVP_DEVHEIGHTCM, heightCM);
    UNPROTECT(3);
    return vp;
}

SEXP L_setviewport(SEXP invp, SEXP hasParent)
{
    SEXP vp;
    SEXP pushedvp, fcall;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    /*
     * Duplicate the viewport passed in because we are going
     * to modify it to hell and gone.
     */
    PROTECT(vp = duplicate(invp));
    /* 
     * Call R function pushedvp() 
     */
    PROTECT(fcall = lang2(install("pushedvp"),
			  vp));
    PROTECT(pushedvp = eval(fcall, R_gridEvalEnv)); 
    pushedvp = doSetViewport(pushedvp, !LOGICAL(hasParent)[0], TRUE, dd);
    /* Set the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    setGridStateElement(dd, GSS_VP, pushedvp);
    UNPROTECT(3);
    return R_NilValue;
}

/* 
 * Find a viewport in the current viewport tree by name
 *
 * Have to do this in C code so that we get THE SEXP in
 * the tree, NOT a copy of it.
 */

/*
 * Some helper functions to call R code because I have no idea
 * how to do this in C code
 */
static Rboolean noChildren(SEXP children) 
{
    SEXP result, fcall;
    PROTECT(fcall = lang2(install("no.children"),
			  children));
    PROTECT(result = eval(fcall, R_gridEvalEnv)); 
    UNPROTECT(2);
    return LOGICAL(result)[0];
}

static Rboolean childExists(SEXP name, SEXP children) 
{
    SEXP result, fcall;
    PROTECT(fcall = lang3(install("child.exists"),
			  name, children));
    PROTECT(result = eval(fcall, R_gridEvalEnv)); 
    UNPROTECT(2);
    return LOGICAL(result)[0];
}

static SEXP childList(SEXP children) 
{
    SEXP result, fcall;
    PROTECT(fcall = lang2(install("child.list"),
			  children));
    PROTECT(result = eval(fcall, R_gridEvalEnv)); 
    UNPROTECT(2);
    return result;    
}

/*
find.in.children <- function(name, children) {
  cpvps <- ls(env=children)
  ncpvp <- length(cpvps)
  count <- 0
  found <- FALSE
  while (count < ncpvp && !found) {
    result <- find.viewport(name, get(cpvps[count+1], env=children))
    found <- result$found
    count <- count + 1
  }
  if (!found)
    result <- list(found=FALSE, pvp=NULL)
  return(result)
}
*/
static SEXP findViewport(SEXP name, SEXP strict, SEXP vp, int depth);
static SEXP findInChildren(SEXP name, SEXP strict, SEXP children, int depth) 
{
    SEXP childnames = childList(children);
    int n = LENGTH(childnames);
    int count = 0;
    Rboolean found = FALSE;
    SEXP result = R_NilValue;
    PROTECT(childnames);
    PROTECT(result);
    while (count < n && !found) {
	result = findViewport(name, strict,
			      PROTECT(findVar(installChar(STRING_ELT(childnames, count)),
				      children)),
			      depth);
	found = INTEGER(VECTOR_ELT(result, 0))[0] > 0;
	UNPROTECT(1);
	count = count + 1;
    }
    if (!found) {
	SEXP temp, zeroDepth;
	PROTECT(temp = allocVector(VECSXP, 2));
	PROTECT(zeroDepth = allocVector(INTSXP, 1));
	INTEGER(zeroDepth)[0] = 0;
	SET_VECTOR_ELT(temp, 0, zeroDepth);
	SET_VECTOR_ELT(temp, 1, R_NilValue);
	UNPROTECT(2);
	result = temp;
    }
    UNPROTECT(2);
    return result;
}
			   
/*
find.viewport <- function(name, pvp) {
  found <- FALSE
  if (length(ls(env=pvp$children)) == 0)
    return(list(found=FALSE, pvp=NULL))
  else 
    if (exists(name, env=pvp$children, inherits=FALSE)) 
      return(list(found=TRUE,
                  pvp=get(name, env=pvp$children, inherits=FALSE)))
    else 
      find.in.children(name, pvp$children)
}
*/
static SEXP findViewport(SEXP name, SEXP strict, SEXP vp, int depth) 
{
    SEXP result, zeroDepth, curDepth;
    PROTECT(result = allocVector(VECSXP, 2));
    PROTECT(zeroDepth = allocVector(INTSXP, 1));
    INTEGER(zeroDepth)[0] = 0;
    PROTECT(curDepth = allocVector(INTSXP, 1));
    INTEGER(curDepth)[0] = depth;
    /* 
     * If there are no children, we fail
     */
    if (noChildren(viewportChildren(vp))) {
	SET_VECTOR_ELT(result, 0, zeroDepth);
	SET_VECTOR_ELT(result, 1, R_NilValue);
    } else if (childExists(name, viewportChildren(vp))) {
	SET_VECTOR_ELT(result, 0, curDepth);
	SET_VECTOR_ELT(result, 1, 
		       /*
			* Does this do inherits=FALSE?
			*/
		       findVar(installChar(STRING_ELT(name, 0)),
			       viewportChildren(vp)));
    } else {
	/*
	 * If this is a strict match, fail
	 * Otherwise recurse into children
	 */
	if (LOGICAL(strict)[0]) {
	    SET_VECTOR_ELT(result, 0, zeroDepth);
	    SET_VECTOR_ELT(result, 1, R_NilValue);
	} else {
	    result = findInChildren(name, strict, viewportChildren(vp),
				    depth + 1);
	}
    }
    UNPROTECT(3);
    return result;
}

SEXP L_downviewport(SEXP name, SEXP strict) 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    /* Get the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */    
    SEXP gvp = gridStateElement(dd, GSS_VP);
    /* 
     * Try to find the named viewport
     */
    SEXP found, vp;
    int depth = 1;
    PROTECT(found = findViewport(name, strict, gvp, depth));
    if (INTEGER(VECTOR_ELT(found, 0))[0]) {
	vp = doSetViewport(VECTOR_ELT(found, 1), FALSE, FALSE, dd);
	/* Set the value of the current viewport for the current device
	 * Need to do this in here so that redrawing via R BASE display
	 * list works 
	 */
	setGridStateElement(dd, GSS_VP, vp);
        UNPROTECT(1);    
    } else {
        /* Important to have an error here, rather than back in
         * R code AFTER this point.  Otherwise, an unsuccessful
         * downViewport() will be recorded on the engine DL!
         */
        char msg[1024];
        snprintf(msg, 1024, "Viewport '%s' was not found", 
		 CHAR(STRING_ELT(name, 0)));
        UNPROTECT(1);    
        error(_(msg));
    }
    return VECTOR_ELT(found, 0);    
}

/* 
 * Find a viewport PATH in the current viewport tree by name
 *
 * Similar to L_downviewport
 */

static Rboolean pathMatch(SEXP path, SEXP pathsofar, SEXP strict) 
{
    SEXP result, fcall;
    PROTECT(fcall = lang4(install("pathMatch"),
			  path, pathsofar, strict));
    PROTECT(result = eval(fcall, R_gridEvalEnv)); 
    UNPROTECT(2);
    return LOGICAL(result)[0];    
}

static SEXP growPath(SEXP pathsofar, SEXP name) 
{
    SEXP result, fcall;
    if (isNull(pathsofar))
	result = name;
    else {
	PROTECT(fcall = lang3(install("growPath"),
			      pathsofar, name));
	PROTECT(result = eval(fcall, R_gridEvalEnv)); 
	UNPROTECT(2);
    }
    return result;    
}

static SEXP findvppath(SEXP path, SEXP name, SEXP strict, 
		       SEXP pathsofar, SEXP vp, int depth);
static SEXP findvppathInChildren(SEXP path, SEXP name, 
				 SEXP strict, SEXP pathsofar,
				 SEXP children, int depth) 
{
    SEXP childnames = childList(children);
    int n = LENGTH(childnames);
    int count = 0;
    Rboolean found = FALSE;
    SEXP result = R_NilValue;
    PROTECT(childnames);
    PROTECT(result);
    while (count < n && !found) {
	SEXP vp, newpathsofar;
	PROTECT(vp = findVar(installChar(STRING_ELT(childnames, count)),
			     children));
	PROTECT(newpathsofar = growPath(pathsofar,
					VECTOR_ELT(vp, VP_NAME)));
	result = findvppath(path, name, strict, newpathsofar, vp, depth);
	found = INTEGER(VECTOR_ELT(result, 0))[0] > 0;
	count = count + 1;
	UNPROTECT(2);
    }
    if (!found) {
	SEXP temp, zeroDepth;
	PROTECT(temp = allocVector(VECSXP, 2));
	PROTECT(zeroDepth = allocVector(INTSXP, 1));
	INTEGER(zeroDepth)[0] = 0;
	SET_VECTOR_ELT(temp, 0, zeroDepth);
	SET_VECTOR_ELT(temp, 1, R_NilValue);
	UNPROTECT(2);
	result = temp;
    }
    UNPROTECT(2);
    return result;
}
			   
static SEXP findvppath(SEXP path, SEXP name, SEXP strict, 
		       SEXP pathsofar, SEXP vp, int depth) 
{
    SEXP result, zeroDepth, curDepth;
    PROTECT(result = allocVector(VECSXP, 2));
    PROTECT(zeroDepth = allocVector(INTSXP, 1));
    INTEGER(zeroDepth)[0] = 0;
    PROTECT(curDepth = allocVector(INTSXP, 1));
    INTEGER(curDepth)[0] = depth;
    /* 
     * If there are no children, we fail
     */
    if (noChildren(viewportChildren(vp))) {
	SET_VECTOR_ELT(result, 0, zeroDepth);
	SET_VECTOR_ELT(result, 1, R_NilValue);
	
    } 
    /* 
     * Check for the viewport name AND whether the rest
     * of the path matches (possibly strictly)
     */
    else if (childExists(name, viewportChildren(vp)) &&
	     pathMatch(path, pathsofar, strict)) {
	SET_VECTOR_ELT(result, 0, curDepth);
	SET_VECTOR_ELT(result, 1, 
		       /*
			* Does this do inherits=FALSE?
			*/
		       findVar(installChar(STRING_ELT(name, 0)),
			       viewportChildren(vp)));
    } else {
	result = findvppathInChildren(path, name, strict, pathsofar,
				      viewportChildren(vp), depth + 1);
    }
    UNPROTECT(3);
    return result;
}

SEXP L_downvppath(SEXP path, SEXP name, SEXP strict) 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    /* Get the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */    
    SEXP gvp = gridStateElement(dd, GSS_VP);
    /* 
     * Try to find the named viewport
     */
    SEXP found, vp;
    int depth = 1;
    PROTECT(found = findvppath(path, name, strict, R_NilValue, gvp, depth));
    if (INTEGER(VECTOR_ELT(found, 0))[0]) {
	vp = doSetViewport(VECTOR_ELT(found, 1), FALSE, FALSE, dd);
	/* Set the value of the current viewport for the current device
	 * Need to do this in here so that redrawing via R BASE display
	 * list works 
	 */
	setGridStateElement(dd, GSS_VP, vp);
        UNPROTECT(1);    
    } else {
        /* Important to have an error here, rather than back in
         * R code AFTER this point.  Otherwise, an unsuccessful
         * downViewport() will be recorded on the engine DL!
         */
        char msg[1024];
        snprintf(msg, 1024, "Viewport '%s' was not found", 
		 CHAR(STRING_ELT(name, 0)));
        UNPROTECT(1);    
        error(_(msg));
    }
    return VECTOR_ELT(found, 0);    
}

/* This is similar to L_setviewport, except that it will NOT 
 * recalculate the viewport transform if the device has not changed size
 */
SEXP L_unsetviewport(SEXP n)
{
    int i;
    double xx1, yy1, xx2, yy2;
    double devWidthCM, devHeightCM;
    SEXP parentClip;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    /* Get the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */    
    SEXP gvp = gridStateElement(dd, GSS_VP);
    /* NOTE that the R code has already checked that .grid.viewport$parent
     * is non-NULL
     * 
     * BUT this may not be called from R code !!
     * (e.g., when the graphics engine display list is replayed;
     *  problems can occur when grid output is mixed with base output;
     *  for example, plot.new() is called between a viewport push and pop)
     */
    SEXP newvp = VECTOR_ELT(gvp, PVP_PARENT);
    if (isNull(newvp))
	error(_("cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)"));
    for (i = 1; i < INTEGER(n)[0]; i++) {
	gvp = newvp;
	newvp = VECTOR_ELT(gvp, PVP_PARENT);
	if (isNull(newvp))
	    error(_("cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)"));
    }
    /* 
     * Remove the child (gvp) from the parent's (newvp) "list" of
     * children
     */
    /* 
     * This has to be done via a call to R-level ...
     *   remove(gvp$name, envir=newvp$children, inherits=FALSE)
     * ... because RemoveVariable in envir.c is not exported (why not?)
     *
     * I tried to model this on the example in the section 
     * "System and foreign language interfaces ... Evaluating R expressions"
     * in the "Writing R Extensions" manual, but the compiler didn't
     * like CAR(t) as an lvalue.
     */
    PROTECT(gvp); PROTECT(newvp);
    {
	SEXP fcall, false, t;
	PROTECT(false = allocVector(LGLSXP, 1));
	LOGICAL(false)[0] = FALSE;
	PROTECT(fcall = lang4(install("remove"), 
			      VECTOR_ELT(gvp, VP_NAME),
			      VECTOR_ELT(newvp, PVP_CHILDREN),
			      false));
	t = fcall;
	t = CDR(CDR(t));
	SET_TAG(t, install("envir")); 
	t = CDR(t);
	SET_TAG(t, install("inherits")); 
	eval(fcall, R_gridEvalEnv); 
	UNPROTECT(2); /* false, fcall */
    }
    /* Get the current device size 
     */
    getDeviceSize(dd, &devWidthCM, &devHeightCM);
    if (deviceChanged(devWidthCM, devHeightCM, newvp))
	calcViewportTransform(newvp, viewportParent(newvp), 1, dd);
    /* 
     * Enforce the current viewport settings
     */
    setGridStateElement(dd, GSS_GPAR, viewportgpar(newvp));
    /* Set the clipping region to the parent's cur.clip
     */
    parentClip = viewportClipRect(newvp);
    xx1 = REAL(parentClip)[0];
    yy1 = REAL(parentClip)[1];
    xx2 = REAL(parentClip)[2];
    yy2 = REAL(parentClip)[3];
    GESetClip(xx1, yy1, xx2, yy2, dd);
    /* Set the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    setGridStateElement(dd, GSS_VP, newvp);
    /* 
     * Remove the parent from the child
     * This is not strictly necessary, but it is conceptually
     * more complete and makes it more likely that we will
     * detect incorrect code elsewhere (because it is likely to
     * trigger a segfault if other code is incorrect)
     *
     * NOTE: Do NOT do this any earlier or you will
     * remove the PROTECTive benefit of newvp pointing
     * to part of the (global) grid state
     */
    SET_VECTOR_ELT(gvp, PVP_PARENT, R_NilValue);
    UNPROTECT(2); /* gvp, newvp */
    return R_NilValue;
}

/* This is similar to L_unsetviewport, except that it will NOT 
 * modify parent-child relations 
 */
SEXP L_upviewport(SEXP n)
{
    int i;
    double xx1, yy1, xx2, yy2;
    double devWidthCM, devHeightCM;
    SEXP parentClip;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    /* Get the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */    
    SEXP gvp = gridStateElement(dd, GSS_VP);
    SEXP newvp = VECTOR_ELT(gvp, PVP_PARENT);
    if (isNull(newvp))
	error(_("cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)"));
    for (i = 1; i < INTEGER(n)[0]; i++) {
	gvp = newvp;
	newvp = VECTOR_ELT(gvp, PVP_PARENT);
	if (isNull(newvp))
	    error(_("cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)"));
    }
    /* Get the current device size 
     */
    getDeviceSize(dd, &devWidthCM, &devHeightCM);
    if (deviceChanged(devWidthCM, devHeightCM, newvp))
	calcViewportTransform(newvp, viewportParent(newvp), 1, dd);
    /* 
     * Enforce the current viewport settings
     */
    setGridStateElement(dd, GSS_GPAR, viewportgpar(newvp));
    /* Set the clipping region to the parent's cur.clip
     */
    parentClip = viewportClipRect(newvp);
    xx1 = REAL(parentClip)[0];
    yy1 = REAL(parentClip)[1];
    xx2 = REAL(parentClip)[2];
    yy2 = REAL(parentClip)[3];
    GESetClip(xx1, yy1, xx2, yy2, dd);
#if 0
	    /* This is a VERY short term fix to avoid mucking
	     * with the core graphics during feature freeze
	     * It should be removed post R 1.4 release
	     */
	    dd->dev->clipLeft = fmin2(xx1, xx2);
	    dd->dev->clipRight = fmax2(xx1, xx2);
	    dd->dev->clipTop = fmax2(yy1, yy2);
	    dd->dev->clipBottom = fmin2(yy1, yy2); 
#endif
    /* Set the value of the current viewport for the current device
     * Need to do this in here so that redrawing via R BASE display
     * list works 
     */
    setGridStateElement(dd, GSS_VP, newvp);
    return R_NilValue;
}

SEXP L_getDisplayList() 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_DL);
}

SEXP L_setDisplayList(SEXP dl) 
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_DL, dl);
    return R_NilValue;
}

/*
 * Get the element at index on the DL
 */
SEXP L_getDLelt(SEXP index)
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    SEXP dl, result;
    PROTECT(dl = gridStateElement(dd, GSS_DL));
    result = VECTOR_ELT(dl, INTEGER(index)[0]);
    UNPROTECT(1);
    return result;
}

/* Add an element to the display list at the current location
 * Location is maintained in R code
 */
SEXP L_setDLelt(SEXP value)
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    SEXP dl;
    PROTECT(dl = gridStateElement(dd, GSS_DL));
    SET_VECTOR_ELT(dl, INTEGER(gridStateElement(dd, GSS_DLINDEX))[0], value);
    UNPROTECT(1);
    return R_NilValue;
}

SEXP L_getDLindex()
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_DLINDEX);
}

SEXP L_setDLindex(SEXP index)
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_DLINDEX, index);
    return R_NilValue;
}

SEXP L_getDLon()
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_DLON);
}

SEXP L_setDLon(SEXP value)
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    SEXP prev;
    prev = gridStateElement(dd, GSS_DLON);
    setGridStateElement(dd, GSS_DLON, value);
    return prev;
}

SEXP L_getEngineDLon()
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_ENGINEDLON);
}

SEXP L_setEngineDLon(SEXP value)
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_ENGINEDLON, value);
    return R_NilValue;
}

SEXP L_getCurrentGrob() 
{
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_CURRGROB);
}

SEXP L_setCurrentGrob(SEXP value)
{
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_CURRGROB, value);
    return R_NilValue;
}

SEXP L_getEngineRecording() 
{
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_ENGINERECORDING);
}

SEXP L_setEngineRecording(SEXP value)
{
    pGEDevDesc dd = getDevice();
    setGridStateElement(dd, GSS_ENGINERECORDING, value);
    return R_NilValue;
}

SEXP L_currentGPar()
{
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    return gridStateElement(dd, GSS_GPAR);
}

SEXP L_newpagerecording()
{
    pGEDevDesc dd = getDevice();
    if (dd->ask) {
	NewFrameConfirm(dd->dev);
	/*
	 * User may have killed device during pause for prompt
	 */
	if (NoDevices())
	    error(_("attempt to plot on null device"));
	else
	    /* 
	     * Should throw an error if dd != GECurrentDevice ?
	     */
	    dd = GEcurrentDevice();
    }
    GEinitDisplayList(dd);
    return R_NilValue;
}

SEXP L_newpage()
{
    pGEDevDesc dd = getDevice();
    R_GE_gcontext gc;
    /* 
     * Has the device been drawn on yet?
     */
    Rboolean deviceDirty = GEdeviceDirty(dd);
    /*
     * Has the device been drawn on BY GRID yet?
     */
    Rboolean deviceGridDirty = LOGICAL(gridStateElement(dd, 
							GSS_GRIDDEVICE))[0];
    /*
     * Initialise grid on device
     * If no drawing on device yet, does a new page
     */
    if (!deviceGridDirty) {
        dirtyGridDevice(dd);
    } 
    /*
     * If device has previously been drawn on (by grid or other system)
     * do a new page
     */
    if (deviceGridDirty || deviceDirty) {
	SEXP currentgp = gridStateElement(dd, GSS_GPAR);
	gcontextFromgpar(currentgp, 0, &gc, dd);
	GENewPage(&gc, dd);
    }
    return R_NilValue;
}

SEXP L_initGPar()
{
    pGEDevDesc dd = getDevice();
    initGPar(dd);
    return R_NilValue;
}

SEXP L_initViewportStack()
{
    pGEDevDesc dd = getDevice();
    initVP(dd);
    return R_NilValue;
}

SEXP L_initDisplayList()
{
    pGEDevDesc dd = getDevice();
    initDL(dd);
    return R_NilValue;
}

void getViewportTransform(SEXP currentvp, 
			  pGEDevDesc dd, 
			  double *vpWidthCM, double *vpHeightCM,
			  LTransform transform, double *rotationAngle) 
{
    int i, j;
    double devWidthCM, devHeightCM;
    getDeviceSize((dd), &devWidthCM, &devHeightCM) ;
    if (deviceChanged(devWidthCM, devHeightCM, currentvp)) {
	/* IF the device has changed, recalculate the viewport transform
	 */
	calcViewportTransform(currentvp, viewportParent(currentvp), 1, dd); 
    }
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    transform[i][j] = 
		REAL(viewportTransform(currentvp))[i + 3*j];
    *rotationAngle = REAL(viewportRotation(currentvp))[0];
    *vpWidthCM = REAL(viewportWidthCM(currentvp))[0];
    *vpHeightCM = REAL(viewportHeightCM(currentvp))[0];
}


/***************************
 * CONVERSION FUNCTIONS
 ***************************
 */

/*
 * WITHIN THE CURRENT VIEWPORT ... 
 *
 * Given a unit object and whether it is a location/dimension,
 * convert to location/dimension in unit B
 *
 * NOTE: When this is used to convert a mouse click on a device to
 * a location/dimension, the conversion of the mouse click to 
 * a unit within the current viewport has to be done elsewhere.
 * e.g., in interactive.R, this is done by applying the inverse
 * of the current viewport transformation to get a location in
 * inches within the current viewport.
 * 
 * This should ideally create a unit object to ensure that the
 * values it returns are treated as values in the correct 
 * coordinate system.  For now, this is MUCH easier to do in
 * R code, so it is the responsibility of the R code calling this
 * to create the unit object correctly/honestly.
 *
 * NOTE also that the unitto supplied should be a "valid" integer 
 * (the best way to get that is to use the valid.units()
 *  function in unit.R)
 *
 * what = 0 means x, 1 means y, 2 means width, 3 means height
 */
SEXP L_convert(SEXP x, SEXP whatfrom,
	       SEXP whatto, SEXP unitto) {
    int i, nx;
    SEXP answer;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    int TOunit, FROMaxis, TOaxis;
    Rboolean relConvert;
    /* 
     * Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    /* 
     * We do not need the current transformation, but
     * we need the side effects of calculating it in
     * case the device has been resized (or only just created)
     */
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x);
    PROTECT(answer = allocVector(REALSXP, nx));
    for (i=0; i<nx; i++) {
        gcontextFromgpar(currentgp, i, &gc, dd);
        TOunit = INTEGER(unitto)[i % LENGTH(unitto)];
        FROMaxis = INTEGER(whatfrom)[0];
        TOaxis = INTEGER(whatto)[0];
        /* 
         * Special case: FROM unit is just a plain, relative unit AND
         *               TO unit is relative AND
         *               NOT converting from 'x' to 'y' (or vice versa) ...
         * 
         *               ... AND relevant widthCM or heightCM is zero
         *
         * In these cases do NOT transform thru INCHES 
         * (to avoid divide-by-zero, but still do something useful)
         */
        relConvert = (!isUnitArithmetic(x) && !isUnitList(x) &&
                      (unitUnit(x, i) == L_NATIVE || unitUnit(x, i) == L_NPC) &&
                      (TOunit == L_NATIVE || TOunit == L_NPC) &&
                      ((FROMaxis == TOaxis) ||
                       (FROMaxis == 0 && TOaxis == 2) ||
                       (FROMaxis == 2 && TOaxis == 0) ||
                       (FROMaxis == 1 && TOaxis == 3) ||
                       (FROMaxis == 3 && TOaxis == 1)));
        /* 
         * First, convert the unit object passed in to a value in INCHES
         * (within the current viewport)
         */
        switch (FROMaxis) {
        case 0:
            if (relConvert && vpWidthCM < 1e-6) {
                REAL(answer)[i] = 
                    transformXYtoNPC(unitValue(x, i), unitUnit(x, i),
                                     vpc.xscalemin, vpc.xscalemax);
            } else {
                relConvert = FALSE;
                REAL(answer)[i] = 
                    transformXtoINCHES(x, i, vpc, &gc,
                                       vpWidthCM, vpHeightCM, 
                                       dd);
            }
            break;
        case 1:
            if (relConvert && vpHeightCM < 1e-6) {
                REAL(answer)[i] = 
                    transformXYtoNPC(unitValue(x, i), unitUnit(x, i),
                                     vpc.yscalemin, vpc.yscalemax);
            } else {
                relConvert = FALSE;
                REAL(answer)[i] = 
                    transformYtoINCHES(x, i, vpc, &gc,
                                       vpWidthCM, vpHeightCM, 
                                       dd);
            }
            break;
        case 2:
            if (relConvert && vpWidthCM < 1e-6) {
                REAL(answer)[i] = 
                    transformWHtoNPC(unitValue(x, i), unitUnit(x, i),
                                     vpc.xscalemin, vpc.xscalemax);
            } else {
                relConvert = FALSE;
                REAL(answer)[i] = 
                    transformWidthtoINCHES(x, i, vpc, &gc,
                                           vpWidthCM, vpHeightCM, 
                                           dd);
            }
            break;
        case 3:
            if (relConvert && vpHeightCM < 1e-6) {
                REAL(answer)[i] = 
                    transformWHtoNPC(unitValue(x, i), unitUnit(x, i),
                                     vpc.yscalemin, vpc.yscalemax);
            } else {
                relConvert = FALSE;
                REAL(answer)[i] = 
                    transformHeighttoINCHES(x, i, vpc, &gc,
                                            vpWidthCM, vpHeightCM, 
                                            dd);
            }
            break;
        }
        /* 
         * Now, convert the values in INCHES to a value in the specified
         * coordinate system 
         * (within the current viewport)
         *
         * BUT do NOT do this step for the special "relConvert" case
         */
        switch (TOaxis) {
        case 0:
            if (relConvert) {
                REAL(answer)[i] = transformXYfromNPC(REAL(answer)[i], TOunit,
                                                     vpc.xscalemin,
                                                     vpc.xscalemax);
            } else {
                REAL(answer)[i] = 
                    transformXYFromINCHES(REAL(answer)[i], TOunit,
                                          vpc.xscalemin,
                                          vpc.xscalemax,
                                          &gc,
                                          vpWidthCM, vpHeightCM, 
                                          dd);
            }
            break;
        case 1:
            if (relConvert) {
                REAL(answer)[i] = transformXYfromNPC(REAL(answer)[i], TOunit,
                                                     vpc.yscalemin,
                                                     vpc.yscalemax);
            } else {
                REAL(answer)[i] = 
                    transformXYFromINCHES(REAL(answer)[i], TOunit,
                                          vpc.yscalemin,
                                          vpc.yscalemax,
                                          &gc,
                                          vpHeightCM, vpWidthCM, 
                                          dd);
            }
            break;
        case 2:
            if (relConvert) {
                REAL(answer)[i] = transformWHfromNPC(REAL(answer)[i], TOunit,
                                                     vpc.xscalemin,
                                                     vpc.xscalemax);
            } else {
                REAL(answer)[i] = 
                    transformWidthHeightFromINCHES(REAL(answer)[i], TOunit,
                                                   vpc.xscalemin,
                                                   vpc.xscalemax,
                                                   &gc,
                                                   vpWidthCM, vpHeightCM, 
                                                   dd);
            }
            break;
        case 3:
            if (relConvert) {
                REAL(answer)[i] = transformWHfromNPC(REAL(answer)[i], TOunit,
                                                     vpc.yscalemin,
                                                     vpc.yscalemax);
            } else {
                REAL(answer)[i] = 
                    transformWidthHeightFromINCHES(REAL(answer)[i], TOunit,
                                                   vpc.yscalemin,
                                                   vpc.yscalemax,
                                                   &gc,
                                                   vpHeightCM, vpWidthCM, 
                                                   dd);
                break;
            }
        }
    }
    UNPROTECT(1);
    return answer;
}

/*
 * Convert locations or dimensions to device inches
 */
SEXP L_devLoc(SEXP x, SEXP y) {
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP devx, devy, result;
    SEXP currentvp, currentgp;
    int i, maxn, ny;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    gcontextFromgpar(currentgp, 0, &gc, dd);
    /* Convert the x and y values to device inches locations */
    maxn = unitLength(x); 
    ny = unitLength(y); 
    if (ny > maxn)
	maxn = ny;
    PROTECT(devx = allocVector(REALSXP, maxn));
    PROTECT(devy = allocVector(REALSXP, maxn));
    PROTECT(result = allocVector(VECSXP, 2));
    for (i=0; i<maxn; i++) {
        transformLocn(x, y, i, vpc, &gc,
                      vpWidthCM, vpHeightCM,
                      dd,
                      transform,
                      &xx, &yy);
        REAL(devx)[i] = xx;
        REAL(devy)[i] = yy;
    }
    SET_VECTOR_ELT(result, 0, devx);
    SET_VECTOR_ELT(result, 1, devy);
    UNPROTECT(3);
    return result;
}

SEXP L_devDim(SEXP x, SEXP y) {
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP devx, devy, result;
    SEXP currentvp, currentgp;
    int i, maxn, ny;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    gcontextFromgpar(currentgp, 0, &gc, dd);
    /* Convert the x and y values to device inches locations */
    maxn = unitLength(x); 
    ny = unitLength(y); 
    if (ny > maxn)
	maxn = ny;
    PROTECT(devx = allocVector(REALSXP, maxn));
    PROTECT(devy = allocVector(REALSXP, maxn));
    PROTECT(result = allocVector(VECSXP, 2));
    for (i=0; i<maxn; i++) {
        transformDimn(x, y, i, vpc, &gc,
                      vpWidthCM, vpHeightCM,
                      dd, rotationAngle,
                      &xx, &yy);
        REAL(devx)[i] = xx;
        REAL(devy)[i] = yy;
    }
    SET_VECTOR_ELT(result, 0, devx);
    SET_VECTOR_ELT(result, 1, devy);
    UNPROTECT(3);
    return result;
}

/*
 * Given a layout.pos.row and a layout.pos.col, calculate
 * the region allocated by the layout of the current viewport
 *
 * Not a conversion as such, but similarly vulnerable to device resizing
 */
SEXP L_layoutRegion(SEXP layoutPosRow, SEXP layoutPosCol) {
    LViewportLocation vpl;
    SEXP answer;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LTransform transform;
    SEXP currentvp;
    /* 
     * Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    //currentgp = gridStateElement(dd, GSS_GPAR);
    /* 
     * We do not need the current transformation, but
     * we need the side effects of calculating it in
     * case the device has been resized (or only just created)
     */
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    /* 
     * Only proceed if there is a layout currently defined
     */
    if (isNull(viewportLayout(currentvp)))
	error(_("there is no layout defined"));
    /* 
     * The result is a numeric containing left, bottom, width, and height
     */
    PROTECT(answer = allocVector(REALSXP, 4));
    /* 
     * NOTE:  We are assuming here that calcViewportLocationFromLayout
     * returns the allocated region with a ("left", "bottom") 
     * justification.  This is CURRENTLY true, but ...
     */
    calcViewportLocationFromLayout(layoutPosRow,
				   layoutPosCol,
				   currentvp,
				   &vpl);
    /* 
     * I am not returning the units created in C code
     * because they do not have the units attribute set
     * so they do not behave nicely back in R code.
     * Instead, I take the values and my knowledge that they
     * are NPC units and construct real unit objects back in 
     * R code.
     */
    REAL(answer)[0] = unitValue(vpl.x, 0);
    REAL(answer)[1] = unitValue(vpl.y, 0);
    REAL(answer)[2] = unitValue(vpl.width, 0);
    REAL(answer)[3] = unitValue(vpl.height, 0);
    UNPROTECT(1);
    return answer;
}

/***************************
 * EDGE DETECTION 
 ***************************
 */

/*
 * Calculate the point on the edge of a rectangle at angle theta
 * 0 = East, 180 = West, etc ...
 * Assumes that x- and y-values are in INCHES
 * Assumes that theta is within [0, 360)
 */
static void rectEdge(double xmin, double ymin, double xmax, double ymax,
		     double theta,
		     double *edgex, double *edgey) 
{
    double xm = (xmin + xmax)/2;
    double ym = (ymin + ymax)/2;
    double dx = (xmax - xmin)/2;
    double dy = (ymax - ymin)/2;
    /*
     * FIXME: Special case 0 width or 0 height
     */
    /*
     * Special case angles 
     */
    if (theta == 0) {
	*edgex = xmax;
	*edgey = ym;
    } else if (theta == 270) {
	*edgex = xm;
	*edgey = ymin;
    } else if (theta == 180) {
	*edgex = xmin;
	*edgey = ym;
    } else if (theta == 90) {
	*edgex = xm;
	*edgey = ymax;
    } else {
	double cutoff = dy/dx;
	double angle = theta/180*M_PI;
	double tanTheta = tan(angle);
	double cosTheta = cos(angle);
	double sinTheta = sin(angle);
	if (fabs(tanTheta) < cutoff) { /* Intersect with side */
	    if (cosTheta > 0) { /* Right side */
		*edgex = xmax;
		*edgey = ym + tanTheta*dx;
	    } else { /* Left side */
		*edgex = xmin;
		*edgey = ym - tanTheta*dx;
	    }
	} else { /* Intersect with top/bottom */
	    if (sinTheta > 0) { /* Top */
		*edgey = ymax;
		*edgex = xm + dy/tanTheta;
	    } else { /* Bottom */
		*edgey = ymin;
		*edgex = xm - dy/tanTheta;
	    }
	}
    }
}

/*
 * Calculate the point on the edge of a rectangle at angle theta
 * 0 = East, 180 = West, etc ...
 * Assumes that x- and y-values are in INCHES
 * Assumes that theta is within [0, 360)
 */
static void circleEdge(double x, double y, double r,
		       double theta,
		       double *edgex, double *edgey)
{
    double angle = theta/180*M_PI;
    *edgex = x + r*cos(angle);
    *edgey = y + r*sin(angle);
}

/*
 * Calculate the point on the edge of a *convex* polygon at angle theta
 * 0 = East, 180 = West, etc ...
 * Assumes that x- and y-values are in INCHES
 * Assumes that vertices are in clock-wise order
 * Assumes that theta is within [0, 360)
 */
static void polygonEdge(double *x, double *y, int n,
			double theta,
			double *edgex, double *edgey) {
    int i, v1, v2;
    double xm, ym;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    int found = 0;
    double angle = theta/180*M_PI;
    double vangle1, vangle2;
    /*
     * Find "centre" of polygon
     */
    for (i=0; i<n; i++) {
	if (x[i] < xmin)
	    xmin = x[i];
	if (x[i] > xmax)
	    xmax = x[i];
	if (y[i] < ymin)
	    ymin = y[i];
	if (y[i] > ymax)
	    ymax = y[i];
    }
    xm = (xmin + xmax)/2;
    ym = (ymin + ymax)/2;
    /*
     * Special case zero-width or zero-height
     */ 
    if (fabs(xmin - xmax) < 1e-6) {
        *edgex = xmin;
        if (theta == 90) 
	    *edgey = ymax;
	else if (theta == 270)
	    *edgey = ymin;
	else
	    *edgey = ym;
	return;
    }
    if (fabs(ymin - ymax) < 1e-6) {
        *edgey = ymin;
        if (theta == 0) 
	    *edgex = xmax;
	else if (theta == 180)
	    *edgex = xmin;
	else
	    *edgex = xm;
	return;
    }	    
    /*
     * Find edge that intersects line from centre at angle theta
     */
    for (i=0; i<n; i++) {
	v1 = i;
	v2 = v1 + 1;
	if (v2 == n)
	    v2 = 0;
	/*
	 * Result of atan2 is in range -PI, PI so convert to
	 * 0, 360 to correspond to angle
	 */
	vangle1 = atan2(y[v1] - ym, x[v1] - xm);
	if (vangle1 < 0)
	    vangle1 = vangle1 + 2*M_PI;
	vangle2 = atan2(y[v2] - ym, x[v2] - xm);
	if (vangle2 < 0)
	    vangle2 = vangle2 + 2*M_PI;
	/*
	 * If vangle1 < vangle2 then angles are either side of 0
	 * so check is more complicated
	 */
	if ((vangle1 >= vangle2 && 
	     vangle1 >= angle && vangle2 <= angle) || 
	    (vangle1 < vangle2 && 
	     ((vangle1 >= angle && 0 <= angle) ||
	      (vangle2 <= angle && 2*M_PI >= angle)))) {
	    found = 1;
	    break;
	}
    }
    /*
     * Find intersection point of "line from centre to bounding rect"
     * and edge
     */
    if (found) {
	double x1 = xm;
	double y1 = ym;
	double x2, y2;
	double x3 = x[v1];
	double y3 = y[v1];
	double x4 = x[v2];
	double y4 = y[v2];
	double numa, denom, ua;
	rectEdge(xmin, ymin, xmax, ymax, theta,
		 &x2, &y2);
	numa = ((x4 - x3)*(y1 - y3) - (y4 - y3)*(x1 - x3));
	denom = ((y4 - y3)*(x2 - x1) - (x4 - x3)*(y2 - y1));
	ua = numa/denom;
	if (!R_FINITE(ua)) {
	    /*
	     * Should only happen if lines are parallel, which 
	     * shouldn't happen!  Unless, perhaps the polygon has
	     * zero extent vertically or horizontally ... ?
	     */
	    error(_("polygon edge not found (zero-width or zero-height?)"));
	}
	/*
	 * numb = ((x2 - x1)*(y1 - y3) - (y2 - y1)*(x1 - x3));
	 * ub = numb/denom;
	 */
	*edgex = x1 + ua*(x2 - x1);
	*edgey = y1 + ua*(y2 - y1);
    } else {
	error(_("polygon edge not found"));    
    }
}

/*
 * Given a set of points, calculate the convex hull then
 * find the edge of that hull
 *
 * NOTE:  assumes that 'grDevices' package has been loaded 
 * so that chull() is available (grid depends on grDevices)
 */
static void hullEdge(double *x, double *y, int n,
		     double theta,
		     double *edgex, double *edgey) 
{
    const void *vmax;
    int i, nh;
    double *hx, *hy;
    SEXP xin, yin, chullFn, R_fcall, hull;    
    int adjust = 0;
    double *xkeep, *ykeep;
    vmax = vmaxget();
    /* Remove any NA's because chull() can't cope with them */
    xkeep = (double *) R_alloc(n, sizeof(double));
    ykeep = (double *) R_alloc(n, sizeof(double));
    for (i=0; i<n; i++) {
        if (!R_FINITE(x[i]) || !R_FINITE(y[i])) {
            adjust--;
        } else {
            xkeep[i + adjust] = x[i];
            ykeep[i + adjust] = y[i];
        }
    }
    n = n + adjust;
    PROTECT(xin = allocVector(REALSXP, n));
    PROTECT(yin = allocVector(REALSXP, n));
    for (i=0; i<n; i++) {
        REAL(xin)[i] = xkeep[i];
        REAL(yin)[i] = ykeep[i];
    }
    /*
     * Determine convex hull
     */
    PROTECT(chullFn = findFun(install("chull"), R_gridEvalEnv));
    PROTECT(R_fcall = lang3(chullFn, xin, yin));
    PROTECT(hull = eval(R_fcall, R_gridEvalEnv));
    nh = LENGTH(hull);
    hx = (double *) R_alloc(nh, sizeof(double));
    hy = (double *) R_alloc(nh, sizeof(double));
    for (i=0; i<nh; i++) {
	hx[i] = x[INTEGER(hull)[i] - 1];
	hy[i] = y[INTEGER(hull)[i] - 1];
    }
    /*
     * Find edge of that hull
     */
    polygonEdge(hx, hy, nh, theta, 
		edgex, edgey);
    vmaxset(vmax);
    UNPROTECT(5);
}

/***************************
 * DRAWING PRIMITIVES
 ***************************
 */

/*
 * Draw an arrow head, given the vertices of the arrow head.
 * Assume vertices are in DEVICE coordinates.
 */
static void drawArrow(double *x, double *y, SEXP type, int i,
		      const pGEcontext gc, pGEDevDesc dd) 
{
    int nt = LENGTH(type);
    switch (INTEGER(type)[i % nt]) {
    case 1:
	GEPolyline(3, x, y, gc, dd);
	break;
    case 2:
	GEPolygon(3, x, y, gc, dd);
	break;
    }
}

/*
 * Calculate vertices for drawing an arrow head.
 * Assumes that x and y locations are in INCHES.
 * Returns vertices in DEVICE coordinates.
 */
static void calcArrow(double x1, double y1,
		      double x2, double y2,
		      SEXP angle, SEXP length, int i,
		      LViewportContext vpc,
		      double vpWidthCM, double vpHeightCM,
		      double *vertx, double *verty,
		      const pGEcontext gc, pGEDevDesc dd) 
{
    int na = LENGTH(angle);
    int nl = LENGTH(length);
    double xc, yc, rot;
    double l1, l2, l, a;
    l1 = transformWidthtoINCHES(length, i % nl, vpc, gc,
				vpWidthCM, vpHeightCM,
				dd);
    l2 = transformHeighttoINCHES(length, i % nl, vpc, gc,
				 vpWidthCM, vpHeightCM,
				 dd);
    l = fmin2(l1, l2);
    a = DEG2RAD * REAL(angle)[i % na];
    xc = x2 - x1;
    yc = y2 - y1;
    rot= atan2(yc, xc);
    vertx[0] = toDeviceX(x1 + l * cos(rot+a),
			 GE_INCHES, dd);
    verty[0] = toDeviceY(y1 + l * sin(rot+a),
			 GE_INCHES, dd);
    vertx[1] = toDeviceX(x1, 
			 GE_INCHES, dd);
    verty[1] = toDeviceY(y1,
			 GE_INCHES, dd);
    vertx[2] = toDeviceX(x1 + l * cos(rot-a),
			 GE_INCHES, dd);
    verty[2] = toDeviceY(y1 + l * sin(rot-a),
			 GE_INCHES, dd);
}

/*
 * Assumes x and y are at least length 2
 * Also assumes x and y are in DEVICE coordinates
 */
static void arrows(double *x, double *y, int n,
		   SEXP arrow, int i,
		   /*
		    * Which ends we are allowed to draw arrow heads on
		    * (we may be drawing a line segment that has been
		    *  broken by NAs)
		    */
		   Rboolean start, Rboolean end,
		   LViewportContext vpc,
		   double vpWidthCM, double vpHeightCM,
		   const pGEcontext gc, pGEDevDesc dd) 
{
    /*
     * Write a checkArrow() function to make
     * sure 'a' is a valid arrow description ?
     * If someone manages to sneak in a
     * corrupt arrow description ... BOOM!
     */			
    SEXP ends = VECTOR_ELT(arrow, GRID_ARROWENDS);
    int ne = LENGTH(ends);
    double vertx[3], verty[3];
    Rboolean first, last;
    if (n < 2)
	error(_("require at least two points to draw arrow"));
    first = TRUE;
    last = TRUE;
    switch (INTEGER(ends)[i % ne]) {
    case 2: 
	first = FALSE;
	break;
    case 1:
	last = FALSE;
	break;
    }
    if (first && start) {
	calcArrow(fromDeviceX(x[0], GE_INCHES, dd),
		  fromDeviceY(y[0], GE_INCHES, dd),
		  fromDeviceX(x[1], GE_INCHES, dd),
		  fromDeviceY(y[1], GE_INCHES, dd),
		  VECTOR_ELT(arrow, GRID_ARROWANGLE), 
		  VECTOR_ELT(arrow, GRID_ARROWLENGTH),
		  i, vpc, vpWidthCM, vpHeightCM, vertx, verty, gc, dd);
	drawArrow(vertx, verty, 
		  VECTOR_ELT(arrow, GRID_ARROWTYPE), i, 
		  gc, dd);
    } 
    if (last && end) {
	calcArrow(fromDeviceX(x[n - 1], GE_INCHES, dd),
		  fromDeviceY(y[n - 1], GE_INCHES, dd),
		  fromDeviceX(x[n - 2], GE_INCHES, dd),
		  fromDeviceY(y[n - 2], GE_INCHES, dd),
		  VECTOR_ELT(arrow, GRID_ARROWANGLE), 
		  VECTOR_ELT(arrow, GRID_ARROWLENGTH),
		  i, vpc, vpWidthCM, vpHeightCM, vertx, verty, gc, dd);
	drawArrow(vertx, verty, 
		  VECTOR_ELT(arrow, GRID_ARROWTYPE), i, 
		  gc, dd);
    }
}

SEXP L_moveTo(SEXP x, SEXP y)
{    
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP devloc, prevloc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    PROTECT(prevloc = gridStateElement(dd, GSS_PREVLOC));
    PROTECT(devloc = gridStateElement(dd, GSS_CURRLOC));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    gcontextFromgpar(currentgp, 0, &gc, dd);
    /* Convert the x and y values to CM locations */
    transformLocn(x, y, 0, vpc, &gc,
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    /*
     * Non-finite values are ok here
     * L_lineTo figures out what to draw 
     * when values are non-finite
     */
    REAL(prevloc)[0] = REAL(devloc)[0];
    REAL(prevloc)[1] = REAL(devloc)[1];
    REAL(devloc)[0] = xx;
    REAL(devloc)[1] = yy;
    UNPROTECT(2);
    return R_NilValue;
}

SEXP L_lineTo(SEXP x, SEXP y, SEXP arrow)
{
    double xx0, yy0, xx1, yy1;
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP devloc, prevloc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    PROTECT(prevloc = gridStateElement(dd, GSS_PREVLOC));
    PROTECT(devloc = gridStateElement(dd, GSS_CURRLOC));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    gcontextFromgpar(currentgp, 0, &gc, dd);
    /* Convert the x and y values to CM locations */
    transformLocn(x, y, 0, vpc, &gc,
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    REAL(prevloc)[0] = REAL(devloc)[0];
    REAL(prevloc)[1] = REAL(devloc)[1];
    REAL(devloc)[0] = xx;
    REAL(devloc)[1] = yy;
    /* The graphics engine only takes device coordinates
     */
    xx0 = toDeviceX(REAL(prevloc)[0], GE_INCHES, dd);
    yy0 = toDeviceY(REAL(prevloc)[1], GE_INCHES, dd), 
    xx1 = toDeviceX(xx, GE_INCHES, dd);
    yy1 = toDeviceY(yy, GE_INCHES, dd);
    if (R_FINITE(xx0) && R_FINITE(yy0) &&
	R_FINITE(xx1) && R_FINITE(yy1)) {
	GEMode(1, dd);
	GELine(xx0, yy0, xx1, yy1, &gc, dd);
	if (!isNull(arrow)) {
	    double ax[2], ay[2];
	    ax[0] = xx0;
	    ax[1] = xx1;
	    ay[0] = yy0;
	    ay[1] = yy1;
	    arrows(ax, ay, 2,
		   arrow, 0, TRUE, TRUE, 
		   vpc, vpWidthCM, vpHeightCM, &gc, dd);
	}
	GEMode(0, dd);
    }
    UNPROTECT(2);
    return R_NilValue;
}

/* We are assuming here that the R code has checked that x and y 
 * are unit objects and that vp is a viewport
 */
SEXP L_lines(SEXP x, SEXP y, SEXP index, SEXP arrow) 
{
    int i, j, nx, nl, start=0;
    double *xx, *yy;
    double xold, yold;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    const void *vmax;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    GEMode(1, dd);
    /* 
     * Number of lines 
     */
    nl = LENGTH(index);
    for (j=0; j<nl; j++) {
	SEXP indices = VECTOR_ELT(index, j);
	gcontextFromgpar(currentgp, j, &gc, dd);
	/* 
	 * Number of vertices
	 *
	 * x and y same length forced in R code 
	 */
	nx = LENGTH(indices); 
	/* Convert the x and y values to CM locations */
	vmax = vmaxget();
	xx = (double *) R_alloc(nx, sizeof(double));
	yy = (double *) R_alloc(nx, sizeof(double));
	xold = NA_REAL;
	yold = NA_REAL;
	for (i=0; i<nx; i++) {
	    transformLocn(x, y, INTEGER(indices)[i] - 1, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &(xx[i]), &(yy[i]));
	    /* The graphics engine only takes device coordinates
	     */
	    xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	    yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
	    if ((R_FINITE(xx[i]) && R_FINITE(yy[i])) &&
		!(R_FINITE(xold) && R_FINITE(yold)))
	        start = i;
	    else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		     !(R_FINITE(xx[i]) && R_FINITE(yy[i]))) {
	        if (i-start > 1) {
		    GEPolyline(i-start, xx+start, yy+start, &gc, dd);
		    if (!isNull(arrow)) {
		        /*
			 * Can draw an arrow at the start if the points
			 * include the first point.
			 * CANNOT draw an arrow at the end point 
			 * because we have just broken the line for an NA.
			 */
		        arrows(xx+start, yy+start, i-start,
			       arrow, j, start == 0, FALSE,
			       vpc, vpWidthCM, vpHeightCM, &gc, dd);
		    }
		}
	    }
	    else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		     (i == nx-1)) {
	        GEPolyline(nx-start, xx+start, yy+start, &gc, dd);
		if (!isNull(arrow)) {
		    /*
		     * Can draw an arrow at the start if the points
		     * include the first point.
		     * Can draw an arrow at the end point.
		     */
 		    arrows(xx+start, yy+start, nx-start, 
			   arrow, j, start == 0, TRUE,
			   vpc, vpWidthCM, vpHeightCM, &gc, dd);
		}
	    } 
	    xold = xx[i];
	    yold = yy[i];
	}
	vmaxset(vmax);
    }
    GEMode(0, dd);
    return R_NilValue;
}

/* We are assuming here that the R code has checked that x and y 
 * are unit objects 
 */
SEXP gridXspline(SEXP x, SEXP y, SEXP s, SEXP o, SEXP a, SEXP rep, SEXP index,
		 double theta, Rboolean draw, Rboolean trace) 
{
    int i, j, nx, np, nloc;
    double *xx, *yy, *ss;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP tracePts = R_NilValue;
    SEXP result = R_NilValue;
    double edgex, edgey;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    gcontextFromgpar(currentgp, 0, &gc, dd);
    /* 
     * Number of xsplines
     */
    np = LENGTH(index);
    PROTECT(tracePts = allocVector(VECSXP, np));
    nloc = 0;
    for (i=0; i<np; i++) {
	const void *vmax;
	SEXP indices = VECTOR_ELT(index, i);
	SEXP points;
	gcontextFromgpar(currentgp, i, &gc, dd);
	/* 
	 * Number of vertices
	 *
	 * Check in R code that x and y same length
	 */
	nx = LENGTH(indices); 
	/* Convert the x and y values to CM locations */
	vmax = vmaxget();
	if (draw)
	    GEMode(1, dd);
	xx = (double *) R_alloc(nx, sizeof(double));
	yy = (double *) R_alloc(nx, sizeof(double));
	ss = (double *) R_alloc(nx, sizeof(double));
	for (j=0; j<nx; j++) {
	    ss[j] = REAL(s)[(INTEGER(indices)[j] - 1) % LENGTH(s)];
	    /*
	     * If drawing, convert to INCHES on device
	     * If just calculating bounds, convert to INCHES within current vp
	     */
	    if (draw) {
		transformLocn(x, y, INTEGER(indices)[j] - 1, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd,
			      transform,
			      &(xx[j]), &(yy[j]));
	    } else {
		xx[j] = transformXtoINCHES(x, INTEGER(indices)[j] - 1,
					   vpc, &gc,
					   vpWidthCM, vpHeightCM, 
					   dd);
		yy[j] = transformYtoINCHES(y, INTEGER(indices)[j] - 1,
					   vpc, &gc,
					   vpWidthCM, vpHeightCM, 
					   dd);	    
	    }
	    /* The graphics engine only takes device coordinates
	     */
	    xx[j] = toDeviceX(xx[j], GE_INCHES, dd);
	    yy[j] = toDeviceY(yy[j], GE_INCHES, dd);
	    if (!(R_FINITE(xx[j]) && R_FINITE(yy[j]))) {
		    error(_("non-finite control point in Xspline"));
	    }
	}
	PROTECT(points = GEXspline(nx, xx, yy, ss,
				   LOGICAL(o)[0], LOGICAL(rep)[0],
				   draw, &gc, dd));
        {
            /*
             * In some cases, GEXspline seems to produce identical points 
             * (at least observed at end of spline)
             * so trim identical points from the ends 
             * (so arrow heads are drawn at correct angle)
             */
            int np = LENGTH(VECTOR_ELT(points, 0));
            double *px = REAL(VECTOR_ELT(points, 0));
            double *py = REAL(VECTOR_ELT(points, 1));
            int start = 0;
            int end = np - 1;
            /*
             * DEBUGGING ...
             int k;
             for (k=0; k<np; k++) {
             GESymbol(px[k], py[k], 16, 3, &gc, dd); 
             }
	     * ... DEBUGGING
	     */
	    while (np > 1 && 
		   (px[start] == px[start + 1]) &&
		   (py[start] == py[start + 1])) {
		start++;
		np--;
	    }
	    while (np > 1 && 
		   (px[end] == px[end - 1]) &&
		   (py[end] == py[end - 1])) {
		end--;
		np--;
	    }
            if (trace) {
                int k;
                int count = end - start + 1;
                double *keepXptr, *keepYptr;
                SEXP keepPoints, keepX, keepY;
                PROTECT(keepPoints = allocVector(VECSXP, 2));
                PROTECT(keepX = allocVector(REALSXP, count));
                PROTECT(keepY = allocVector(REALSXP, count));
                keepXptr = REAL(keepX);
                keepYptr = REAL(keepY);
                for (k=start; k<(end + 1); k++) {
                    keepXptr[k - start] = fromDeviceX(px[k], GE_INCHES, dd);
                    keepYptr[k - start] = fromDeviceY(py[k], GE_INCHES, dd);
                }
                SET_VECTOR_ELT(keepPoints, 0, keepX);
                SET_VECTOR_ELT(keepPoints, 1, keepY);
                SET_VECTOR_ELT(tracePts, i, keepPoints);
                UNPROTECT(3); /* keepPoints & keepX & keepY */
            }
            if (draw && !isNull(a) && !isNull(points)) {
                /*
                 * Can draw an arrow at the either end.
                 */
                arrows(&(px[start]), &(py[start]), np,
                       a, i, TRUE, TRUE,
                       vpc, vpWidthCM, vpHeightCM, &gc, dd);
            }
            if (!draw && !trace && !isNull(points)) {
                /*
                 * Update bounds
                 */
                int j, n = LENGTH(VECTOR_ELT(points, 0));
                double *pxx = (double *) R_alloc(n, sizeof(double));
                double *pyy = (double *) R_alloc(n, sizeof(double));
                for (j=0; j<n; j++) {
                    pxx[j] = fromDeviceX(px[j], GE_INCHES, dd);
                    pyy[j] = fromDeviceY(py[j], GE_INCHES, dd);
                    if (R_FINITE(pxx[j]) && R_FINITE(pyy[j])) {
                        if (pxx[j] < xmin)
                            xmin = pxx[j];
                        if (pxx[j] > xmax)
                            xmax = pxx[j];
                        if (pyy[j] < ymin)
                            ymin = pyy[j];
                        if (pyy[j] > ymax)
                            ymax = pyy[j];
                        nloc++;
                    }
                }
                /*
                 * Calculate edgex and edgey for case where this is 
                 * the only xspline
                 */
                hullEdge(pxx, pyy, n, theta, &edgex, &edgey);
            }
        } /* End of trimming-redundant-points code */
	UNPROTECT(1); /* points */
	if (draw)
	    GEMode(0, dd);
	vmaxset(vmax);
    }
    if (!draw && !trace && nloc > 0) {
	PROTECT(result = allocVector(REALSXP, 4));
	/*
	 * If there is more than one xspline, just produce edge
	 * based on bounding rect of all xsplines
	 */
	if (np > 1) {
	    rectEdge(xmin, ymin, xmax, ymax, theta,
		     &edgex, &edgey);
	}
	/*
	 * Reverse the scale adjustment (zoom factor)
	 * when calculating physical value to return to user-level
	 */
	REAL(result)[0] = edgex / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[1] = edgey / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[2] = (xmax - xmin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[3] = (ymax - ymin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
        UNPROTECT(1); /* result */
    } else if (trace) {
        result = tracePts;
    }
    UNPROTECT(1); /* tracePts */
    return result;
}

SEXP L_xspline(SEXP x, SEXP y, SEXP s, SEXP o, SEXP a, SEXP rep, SEXP index) 
{
    gridXspline(x, y, s, o, a, rep, index, 0, TRUE, FALSE);
    return R_NilValue;
}

SEXP L_xsplineBounds(SEXP x, SEXP y, SEXP s, SEXP o, SEXP a, SEXP rep, 
		     SEXP index, SEXP theta) 
{
    return gridXspline(x, y, s, o, a, rep, index, REAL(theta)[0], 
                       FALSE, FALSE);
}

SEXP L_xsplinePoints(SEXP x, SEXP y, SEXP s, SEXP o, SEXP a, SEXP rep, 
		     SEXP index, SEXP theta) 
{
    return gridXspline(x, y, s, o, a, rep, index, REAL(theta)[0], 
                       FALSE, TRUE);
}

SEXP L_segments(SEXP x0, SEXP y0, SEXP x1, SEXP y1, SEXP arrow) 
{
    int i, nx0, ny0, nx1, ny1, maxn;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    int gpIsScalar[15] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
    LViewportContext vpc;
    R_GE_gcontext gc, gcCache;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    initGContext(currentgp, &gc, dd, gpIsScalar, &gcCache);
    maxn = nx0 = unitLength(x0); 
    ny0 = unitLength(y0);
    nx1 = unitLength(x1);
    ny1 = unitLength(y1);
    if (ny0 > maxn)
	maxn = ny0;
    if (nx1 > maxn)
	maxn = nx1;
    if (ny1 > maxn)
	maxn = ny1;
    /* Convert the x and y values to INCHES locations */
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    for (i=0; i<maxn; i++) {
	double xx0, yy0, xx1, yy1;
    updateGContext(currentgp, i, &gc, dd, gpIsScalar, &gcCache);
	transformLocn(x0, y0, i, vpc, &gc, 
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx0, &yy0);
	transformLocn(x1, y1, i, vpc, &gc,
		      vpWidthCM, vpHeightCM,
		      dd, transform, &xx1, &yy1);
	/* The graphics engine only takes device coordinates
	 */
	xx0 = toDeviceX(xx0, GE_INCHES, dd);
	yy0 = toDeviceY(yy0, GE_INCHES, dd);
	xx1 = toDeviceX(xx1, GE_INCHES, dd);
	yy1 = toDeviceY(yy1, GE_INCHES, dd);
	if (R_FINITE(xx0) && R_FINITE(yy0) &&
	    R_FINITE(xx1) && R_FINITE(yy1)) {
	    GELine(xx0, yy0, xx1, yy1, &gc, dd);
	    if (!isNull(arrow)) {
		double ax[2], ay[2];
		ax[0] = xx0;
		ax[1] = xx1;
		ay[0] = yy0;
		ay[1] = yy1;
		arrows(ax, ay, 2,
		       arrow, i, TRUE, TRUE,
		       vpc, vpWidthCM, vpHeightCM, &gc, dd);
	    }
	}
    }
    GEMode(0, dd);
    return R_NilValue;
}

static int getArrowN(SEXP x1, SEXP x2, SEXP xnm1, SEXP xn, 
		     SEXP y1, SEXP y2, SEXP ynm1, SEXP yn)
{      
    int nx2, nxnm1, nxn, ny1, ny2, nynm1, nyn, maxn;
    maxn = 0;
    /* 
     * x1, y1, xnm1, and ynm1 could be NULL if this is adding
     * arrows to a line.to
     */
    if (isNull(y1))
	ny1 = 0;
    else
	ny1 = unitLength(y1);
    nx2 = unitLength(x2);
    ny2 = unitLength(y2);
    if (isNull(xnm1))
	nxnm1 = 0;
    else
	nxnm1 = unitLength(xnm1);
    if (isNull(ynm1))
	nynm1 = 0;
    else
	nynm1 = unitLength(ynm1);
    nxn = unitLength(xn);
    nyn = unitLength(yn);
    if (ny1 > maxn)
	maxn = ny1;
    if (nx2 > maxn)
	maxn = nx2;
    if (ny2 > maxn)
	maxn = ny2;
    if (nxnm1 > maxn)
	maxn = nxnm1;
    if (nynm1 > maxn)
	maxn = nynm1;
    if (nxn > maxn)
	maxn = nxn;
    if (nyn > maxn)
	maxn = nyn;
    return maxn;
}

SEXP L_arrows(SEXP x1, SEXP x2, SEXP xnm1, SEXP xn, 
	      SEXP y1, SEXP y2, SEXP ynm1, SEXP yn, 
	      SEXP angle, SEXP length, SEXP ends, SEXP type) 
{
    int i, maxn;
    int ne;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    Rboolean first, last;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP devloc = R_NilValue; /* -Wall */
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    maxn = getArrowN(x1, x2, xnm1, xn,
		     y1, y2, ynm1, yn);
    ne = LENGTH(ends);
    /* Convert the x and y values to INCHES locations */
    /* FIXME:  Need to check for NaN's and NA's
     */
    GEMode(1, dd);
    for (i=0; i<maxn; i++) {
	double xx1, xx2, xxnm1, xxn, yy1, yy2, yynm1, yyn;
	double vertx[3];
	double verty[3];
	first = TRUE;
	last = TRUE;
	switch (INTEGER(ends)[i % ne]) {
	case 2: 
	    first = FALSE;
	    break;
	case 1:
	    last = FALSE;
	    break;
	}
	gcontextFromgpar(currentgp, i, &gc, dd);
	/*
	 * If we're adding arrows to a line.to
	 * x1 will be NULL
	 */
	if (isNull(x1)) 
	    PROTECT(devloc = gridStateElement(dd, GSS_CURRLOC));
	if (first) {
	    if (isNull(x1)) {
		xx1 = REAL(devloc)[0];
		yy1 = REAL(devloc)[1];
	    } else 
		transformLocn(x1, y1, i, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, transform, &xx1, &yy1);
	    transformLocn(x2, y2, i, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd, transform, &xx2, &yy2);
	    calcArrow(xx1, yy1, xx2, yy2, angle, length, i,
		      vpc, vpWidthCM, vpHeightCM,
		      vertx, verty, &gc, dd);
	    /* 
	     * Only draw arrow if both ends of first segment 
	     * are not non-finite
	     */
	    if (R_FINITE(toDeviceX(xx2, GE_INCHES, dd)) &&
		R_FINITE(toDeviceY(yy2, GE_INCHES, dd)) &&
		R_FINITE(vertx[1]) && R_FINITE(verty[1]))
		drawArrow(vertx, verty, type, i, &gc, dd);
	}
	if (last) {
	    if (isNull(xnm1)) {
		xxnm1 = REAL(devloc)[0];
		yynm1 = REAL(devloc)[1];
	    } else 
		transformLocn(xnm1, ynm1, i, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, transform, &xxnm1, &yynm1);
	    transformLocn(xn, yn, i, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd, transform, &xxn, &yyn);
	    calcArrow(xxn, yyn, xxnm1, yynm1, angle, length, i,
		      vpc, vpWidthCM, vpHeightCM,
		      vertx, verty, &gc, dd);
	    /* 
	     * Only draw arrow if both ends of laste segment are
	     * not non-finite
	     */
	    if (R_FINITE(toDeviceX(xxnm1, GE_INCHES, dd)) &&
		R_FINITE(toDeviceY(yynm1, GE_INCHES, dd)) &&
		R_FINITE(vertx[1]) && R_FINITE(verty[1]))
		drawArrow(vertx, verty, type, i, &gc, dd);
	}
	if (isNull(x1))
	    UNPROTECT(1);
    }
    GEMode(0, dd);
    return R_NilValue;
}

SEXP L_polygon(SEXP x, SEXP y, SEXP index)
{
    int i, j, nx, np, start=0;
    double *xx, *yy;
    double xold, yold;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    GEMode(1, dd);
    /* 
     * Number of polygons 
     */
    np = LENGTH(index);
    for (i=0; i<np; i++) {
	const void *vmax;
	SEXP indices = VECTOR_ELT(index, i);
	gcontextFromgpar(currentgp, i, &gc, dd);
	/* 
	 * Number of vertices
	 *
	 * Check in R code that x and y same length
	 */
	nx = LENGTH(indices); 
	/* Convert the x and y values to CM locations */
	vmax = vmaxget();
	xx = (double *) R_alloc(nx + 1, sizeof(double));
	yy = (double *) R_alloc(nx + 1, sizeof(double));
	xold = NA_REAL;
	yold = NA_REAL;
	for (j=0; j<nx; j++) {
	    transformLocn(x, y, INTEGER(indices)[j] - 1, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &(xx[j]), &(yy[j]));
	    /* The graphics engine only takes device coordinates
	     */
	    xx[j] = toDeviceX(xx[j], GE_INCHES, dd);
	    yy[j] = toDeviceY(yy[j], GE_INCHES, dd);
	    if ((R_FINITE(xx[j]) && R_FINITE(yy[j])) &&
		!(R_FINITE(xold) && R_FINITE(yold)))
		start = j; /* first point of current segment */
	    else if ((R_FINITE(xold) && R_FINITE(yold)) &&
		     !(R_FINITE(xx[j]) && R_FINITE(yy[j]))) {
		if (j-start > 1) {
		    GEPolygon(j-start, xx+start, yy+start, &gc, dd);
		}
	    }
	    else if ((R_FINITE(xold) && R_FINITE(yold)) && (j == nx-1)) { 
		/* last */
		GEPolygon(nx-start, xx+start, yy+start, &gc, dd);
	    }
	    xold = xx[j];
	    yold = yy[j];
	}
	vmaxset(vmax);
    }
    GEMode(0, dd);
    return R_NilValue;
}

static SEXP gridCircle(SEXP x, SEXP y, SEXP r, 
		       double theta, Rboolean draw)
{
    int i, nx, ny, nr, ncirc;
    double xx, yy, rr1, rr2, rr = 0.0 /* -Wall */;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP result = R_NilValue;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    double edgex, edgey;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    ny = unitLength(y);
    nr = unitLength(r);
    if (ny > nx) 
	nx = ny;
    if (nr > nx)
	nx = nr;
    if (draw) {
	GEMode(1, dd);
    }
    ncirc = 0;
    for (i=0; i<nx; i++) {
	gcontextFromgpar(currentgp, i, &gc, dd);
	/*
	 * If drawing, convert to INCHES on device
	 * If just calculating bounds, convert to INCHES within current vp
	 */
	if (draw) {
	    transformLocn(x, y, i, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx, &yy);
	} else {
	    xx = transformXtoINCHES(x, i, vpc, &gc,
				    vpWidthCM, vpHeightCM, 
				    dd);
	    yy = transformYtoINCHES(y, i, vpc, &gc,
				    vpWidthCM, vpHeightCM, 
				    dd);	    
	}
	/* These two will give the same answer unless r is in "native",
	 * "npc", or some other relative units;  in those cases, just
	 * take the smaller of the two values.
	 */
	rr1 = transformWidthtoINCHES(r, i % nr, vpc, &gc,
				     vpWidthCM, vpHeightCM,
				     dd);
	rr2 = transformHeighttoINCHES(r, i % nr, vpc, &gc,
				      vpWidthCM, vpHeightCM,
				      dd);
	/*
	 * A negative radius is silently converted to absolute value
	 */
	rr = fmin2(fabs(rr1), fabs(rr2));
	if (R_FINITE(xx) && R_FINITE(yy) && R_FINITE(rr)) {
	    if (draw) {
                /* The graphics engine only takes device coordinates
		 */
                xx = toDeviceX(xx, GE_INCHES, dd);
		yy = toDeviceY(yy, GE_INCHES, dd);
		rr = toDeviceWidth(rr, GE_INCHES, dd);
		GECircle(xx, yy, rr, &gc, dd);
	    } else {
		if (xx + rr < xmin)
		    xmin = xx + rr;
		if (xx + rr > xmax)
		    xmax = xx + rr;
		if (xx - rr < xmin)
		    xmin = xx - rr;
		if (xx - rr > xmax)
		    xmax = xx - rr;
		if (yy + rr < ymin)
		    ymin = yy + rr;
		if (yy + rr > ymax)
		    ymax = yy + rr;
		if (yy - rr < ymin)
		    ymin = yy - rr;
		if (yy - rr > ymax)
		    ymax = yy - rr;
		ncirc++;		
	    }
	}
    }
    if (draw) {
	GEMode(0, dd);
    } else if (ncirc > 0) {
	result = allocVector(REALSXP, 4);
	if (ncirc == 1) {
	    /*
	     * Produce edge of actual circle
	     */
	    circleEdge(xx, yy, rr, theta, &edgex, &edgey);
	} else {
	    /*
	     * Produce edge of rect bounding all circles
	     */
	    rectEdge(xmin, ymin, xmax, ymax, theta,
		     &edgex, &edgey);
	}
	/*
	 * Reverse the scale adjustment (zoom factor)
	 * when calculating physical value to return to user-level
	 */
	REAL(result)[0] = edgex / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[1] = edgey /
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[2] = (xmax - xmin) /
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[3] = (ymax - ymin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
    } 
    return result;
}

SEXP L_circle(SEXP x, SEXP y, SEXP r)
{
    gridCircle(x, y, r, 0, TRUE);
    return R_NilValue;
}

SEXP L_circleBounds(SEXP x, SEXP y, SEXP r, SEXP theta)
{
    return gridCircle(x, y, r, REAL(theta)[0], FALSE);
}

/* We are assuming here that the R code has checked that 
 * x, y, w, and h are all unit objects and that vp is a viewport
 */
static SEXP gridRect(SEXP x, SEXP y, SEXP w, SEXP h, 
		     SEXP hjust, SEXP vjust, double theta, Rboolean draw) 
{
    double xx, yy, ww, hh;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    int i, ny, nw, nh, maxn, nrect;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP result = R_NilValue;
    double edgex, edgey;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    maxn = unitLength(x); 
    ny = unitLength(y); 
    nw = unitLength(w); 
    nh = unitLength(h); 
    if (ny > maxn)
	maxn = ny;
    if (nw > maxn)
	maxn = nw;
    if (nh > maxn)
	maxn = nh;
    if (draw) {
	GEMode(1, dd);
    }
    nrect = 0;
    for (i=0; i<maxn; i++) {
	gcontextFromgpar(currentgp, i, &gc, dd);
	/*
	 * If drawing, convert to INCHES on device
	 * If just calculating bounds, convert to INCHES within current vp
	 */
	if (draw) {	
	    transformLocn(x, y, i, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &xx, &yy);
	} else {
	    xx = transformXtoINCHES(x, i, vpc, &gc,
				    vpWidthCM, vpHeightCM, 
				    dd);
	    yy = transformYtoINCHES(y, i, vpc, &gc,
				    vpWidthCM, vpHeightCM, 
				    dd);	    
	}
	ww = transformWidthtoINCHES(w, i, vpc, &gc,
				    vpWidthCM, vpHeightCM,
				    dd);
	hh = transformHeighttoINCHES(h, i, vpc, &gc,
				     vpWidthCM, vpHeightCM,
				     dd);
	/* If the total rotation angle is zero then we can draw a 
	 * rectangle as the devices understand rectangles
	 * Otherwise we have to draw a polygon equivalent.
	 */
	if (draw) {
	    if (rotationAngle == 0) {
		xx = justifyX(xx, ww, REAL(hjust)[i % LENGTH(hjust)]);
		yy = justifyY(yy, hh, REAL(vjust)[i % LENGTH(vjust)]);
		/* The graphics engine only takes device coordinates
		 */
		xx = toDeviceX(xx, GE_INCHES, dd);
		yy = toDeviceY(yy, GE_INCHES, dd);
		ww = toDeviceWidth(ww, GE_INCHES, dd);
		hh = toDeviceHeight(hh, GE_INCHES, dd);
		if (R_FINITE(xx) && R_FINITE(yy) && 
		    R_FINITE(ww) && R_FINITE(hh))
		    GERect(xx, yy, xx + ww, yy + hh, &gc, dd);
	    } else {
		/* We have to do a little bit of work to figure out where the 
		 * corners of the rectangle are.
		 */
		double xxx[5], yyy[5], xadj, yadj;
		double dw, dh;
		SEXP zeroInches, xadjInches, yadjInches, wwInches, hhInches;
		int tmpcol;
                PROTECT(zeroInches = unit(0, L_INCHES));
		/* Find bottom-left location */
		justification(ww, hh, 
			      REAL(hjust)[i % LENGTH(hjust)], 
			      REAL(vjust)[i % LENGTH(vjust)], 
			      &xadj, &yadj);
		PROTECT(xadjInches = unit(xadj, L_INCHES));
		PROTECT(yadjInches = unit(yadj, L_INCHES));
		transformDimn(xadjInches, yadjInches, 0, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, rotationAngle,
			      &dw, &dh);
		xxx[0] = xx + dw;
		yyy[0] = yy + dh;
		/* Find top-left location */
		PROTECT(hhInches = unit(hh, L_INCHES));
		transformDimn(zeroInches, hhInches, 0, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, rotationAngle,
			      &dw, &dh);
		xxx[1] = xxx[0] + dw;
		yyy[1] = yyy[0] + dh;
		/* Find top-right location */
		PROTECT(wwInches = unit(ww, L_INCHES));
		transformDimn(wwInches, hhInches, 0, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, rotationAngle,
			      &dw, &dh);
		xxx[2] = xxx[0] + dw;
		yyy[2] = yyy[0] + dh;
		/* Find bottom-right location */
		transformDimn(wwInches, zeroInches, 0, vpc, &gc,
			      vpWidthCM, vpHeightCM,
			      dd, rotationAngle,
			      &dw, &dh);
		xxx[3] = xxx[0] + dw;
		yyy[3] = yyy[0] + dh;
		if (R_FINITE(xxx[0]) && R_FINITE(yyy[0]) &&
		    R_FINITE(xxx[1]) && R_FINITE(yyy[1]) &&
		    R_FINITE(xxx[2]) && R_FINITE(yyy[2]) &&
		    R_FINITE(xxx[3]) && R_FINITE(yyy[3])) {
		    /* The graphics engine only takes device coordinates
		     */
		    xxx[0] = toDeviceX(xxx[0], GE_INCHES, dd);
		    yyy[0] = toDeviceY(yyy[0], GE_INCHES, dd);
		    xxx[1] = toDeviceX(xxx[1], GE_INCHES, dd);
		    yyy[1] = toDeviceY(yyy[1], GE_INCHES, dd);
		    xxx[2] = toDeviceX(xxx[2], GE_INCHES, dd);
		    yyy[2] = toDeviceY(yyy[2], GE_INCHES, dd);
		    xxx[3] = toDeviceX(xxx[3], GE_INCHES, dd);
		    yyy[3] = toDeviceY(yyy[3], GE_INCHES, dd);
		    /* Close the polygon */
		    xxx[4] = xxx[0];
		    yyy[4] = yyy[0];
		    /* Do separate fill and border to avoid border being 
		     * drawn on clipping boundary when there is a fill
		     */
		    tmpcol = gc.col;
		    gc.col = R_TRANWHITE;
		    GEPolygon(5, xxx, yyy, &gc, dd);
		    gc.col = tmpcol;
		    gc.fill = R_TRANWHITE;
		    GEPolygon(5, xxx, yyy, &gc, dd);
		}
                UNPROTECT(5);
	    }
	} else { /* Just calculating boundary */
	    xx = justifyX(xx, ww, REAL(hjust)[i % LENGTH(hjust)]);
	    yy = justifyY(yy, hh, REAL(vjust)[i % LENGTH(vjust)]);
	    if (R_FINITE(xx) && R_FINITE(yy) && 
		R_FINITE(ww) && R_FINITE(hh)) {
		if (xx < xmin)
		    xmin = xx;
		if (xx > xmax)
		    xmax = xx;
		if (xx + ww < xmin)
		    xmin = xx + ww;
		if (xx + ww > xmax)
		    xmax = xx + ww;
		if (yy < ymin)
		    ymin = yy;
		if (yy > ymax)
		    ymax = yy;
		if (yy + hh < ymin)
		    ymin = yy + hh;
		if (yy + hh > ymax)
		    ymax = yy + hh;
		/*
		 * Calculate edgex and edgey for case where this is 
		 * the only rect
		 */
		rectEdge(xx, yy, xx + ww, yy + hh, theta,
			 &edgex, &edgey);
		nrect++;
	    }
	}
    }
    if (draw) {
	GEMode(0, dd);
    }
    if (nrect > 0) {
	result = allocVector(REALSXP, 4);
	/*
	 * If there is more than one rect, just produce edge
	 * based on bounding rect of all rects
	 */
	if (nrect > 1) {
	    rectEdge(xmin, ymin, xmax, ymax, theta,
		     &edgex, &edgey);
	}
	/*
	 * Reverse the scale adjustment (zoom factor)
	 * when calculating physical value to return to user-level
	 */
	REAL(result)[0] = edgex / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[1] = edgey /
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[2] = (xmax - xmin) /
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[3] = (ymax - ymin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
    } 
    return result;
}

SEXP L_rect(SEXP x, SEXP y, SEXP w, SEXP h, SEXP hjust, SEXP vjust) 
{
    gridRect(x, y, w, h, hjust, vjust, 0, TRUE);
    return R_NilValue;    
}

SEXP L_rectBounds(SEXP x, SEXP y, SEXP w, SEXP h, SEXP hjust, SEXP vjust,
		  SEXP theta) 
{
    return gridRect(x, y, w, h, hjust, vjust, REAL(theta)[0], FALSE);
}

/* FIXME: need to add L_pathBounds ? */

SEXP L_path(SEXP x, SEXP y, SEXP index, SEXP rule)
{
    int i, j, k, h, npoly, *nper, ntot;
    double *xx, *yy;
    const void *vmax;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    GEMode(1, dd);
    /*
     * Iterate over all paths
     */
    for (h = 0; h < LENGTH(index); h++) {
    	SEXP polyInd = VECTOR_ELT(index, h);
    	/* 
    	 * Number of polygons 
    	 */
    	npoly = LENGTH(polyInd);
    	/* 
    	 * Total number of points and 
    	 * Number of points per polygon
    	 */ 
    	ntot = 0;
    	nper = (int *) R_alloc(npoly, sizeof(int));
    	for (i=0; i < npoly; i++) {
    		nper[i] = LENGTH(VECTOR_ELT(polyInd, i));
    		ntot = ntot + nper[i];
    	}
    	vmax = vmaxget();
    	xx = (double *) R_alloc(ntot, sizeof(double));
    	yy = (double *) R_alloc(ntot, sizeof(double));
    	k = 0;
    	for (i=0; i < npoly; i++) {
            int *indices = INTEGER(VECTOR_ELT(polyInd, i));
            for (j=0; j < nper[i]; j++) {            
                transformLocn(x, y, indices[j] - 1, vpc, &gc,
                              vpWidthCM, vpHeightCM,
                              dd,
                              transform,
                              &(xx[k]), &(yy[k]));
                /* The graphics engine only takes device coordinates
                 */
                xx[k] = toDeviceX(xx[k], GE_INCHES, dd);
                yy[k] = toDeviceY(yy[k], GE_INCHES, dd);
                /* NO NA values allowed in 'x' or 'y'
                 */
                if (!R_FINITE(xx[k]) || !R_FINITE(yy[k]))
                    error(_("non-finite x or y in graphics path"));
                k++;
            }
    	}
    	gcontextFromgpar(currentgp, h, &gc, dd);
    	GEPath(xx, yy, npoly, nper, INTEGER(rule)[0], &gc, dd);
    	vmaxset(vmax);
    }
    GEMode(0, dd);
    return R_NilValue;
}

/* FIXME: need to add L_rasterBounds */

/* FIXME:  Add more checks on correct inputs,
   e.g., Raster should be a matrix of R colors */
SEXP L_raster(SEXP raster, SEXP x, SEXP y, SEXP w, SEXP h, 
              SEXP hjust, SEXP vjust, SEXP interpolate)
{
    const void *vmax;
    int i, n, ny, nw, nh, maxn;
    double xx, yy, ww, hh;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP dim;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    unsigned int *image;
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    /* Convert the raster matrix to R internal colours */
    n = LENGTH(raster);
    if (n <= 0) {
        error(_("Empty raster"));  
    }
    vmax = vmaxget();
    /* raster is rather inefficient so allow a native representation as
       an integer array which requires no conversion */
    if (inherits(raster, "nativeRaster") && isInteger(raster)) {
	image = (unsigned int*) INTEGER(raster);
    } else {
        image = (unsigned int*) R_alloc(n, sizeof(unsigned int));
        for (i=0; i<n; i++) {
            image[i] = RGBpar3(raster, i, R_TRANWHITE);
        }
    }
    dim = getAttrib(raster, R_DimSymbol);
    maxn = unitLength(x); 
    ny = unitLength(y); 
    nw = unitLength(w); 
    nh = unitLength(h); 
    if (ny > maxn)
	maxn = ny;
    if (nw > maxn)
	maxn = nw;
    if (nh > maxn)
	maxn = nh;
    GEMode(1, dd);
    for (i=0; i<maxn; i++) {
        gcontextFromgpar(currentgp, i, &gc, dd);
        transformLocn(x, y, i, vpc, &gc,
                      vpWidthCM, vpHeightCM,
                      dd,
                      transform,
                      &xx, &yy);
        ww = transformWidthtoINCHES(w, i, vpc, &gc,
                                    vpWidthCM, vpHeightCM,
                                    dd);
        hh = transformHeighttoINCHES(h, i, vpc, &gc,
                                     vpWidthCM, vpHeightCM,
                                     dd);
        if (rotationAngle == 0) {
            xx = justifyX(xx, ww, REAL(hjust)[i % LENGTH(hjust)]);
            yy = justifyY(yy, hh, REAL(vjust)[i % LENGTH(vjust)]);
            /* The graphics engine only takes device coordinates
             */
            xx = toDeviceX(xx, GE_INCHES, dd);
            yy = toDeviceY(yy, GE_INCHES, dd);
            ww = toDeviceWidth(ww, GE_INCHES, dd);
            hh = toDeviceHeight(hh, GE_INCHES, dd);
            if (R_FINITE(xx) && R_FINITE(yy) && 
                R_FINITE(ww) && R_FINITE(hh))
                GERaster(image, INTEGER(dim)[1], INTEGER(dim)[0],
                         xx, yy, ww, hh, rotationAngle, 
                         LOGICAL(interpolate)[i % LENGTH(interpolate)], 
                         &gc, dd);
        } else {
            /* We have to do a little bit of work to figure out where the 
             * bottom-left corner of the image is.
             */
            double xbl, ybl, xadj, yadj;
            double dw, dh;
            SEXP xadjInches, yadjInches;
            /* Find bottom-left location */
            justification(ww, hh, 
                          REAL(hjust)[i % LENGTH(hjust)], 
                          REAL(vjust)[i % LENGTH(vjust)], 
                          &xadj, &yadj);
            PROTECT(xadjInches = unit(xadj, L_INCHES));
            PROTECT(yadjInches = unit(yadj, L_INCHES));
            transformDimn(xadjInches, yadjInches, 0, vpc, &gc,
                          vpWidthCM, vpHeightCM,
                          dd, rotationAngle,
                          &dw, &dh);
            xbl = xx + dw;
            ybl = yy + dh;
            xbl = toDeviceX(xbl, GE_INCHES, dd);
            ybl = toDeviceY(ybl, GE_INCHES, dd);
            ww = toDeviceWidth(ww, GE_INCHES, dd);
            hh = toDeviceHeight(hh, GE_INCHES, dd);
            if (R_FINITE(xbl) && R_FINITE(ybl) &&
                R_FINITE(ww) && R_FINITE(hh)) {
                /* The graphics engine only takes device coordinates
                 */
                GERaster(image, INTEGER(dim)[1], INTEGER(dim)[0],
                         xbl, ybl, ww, hh, rotationAngle, 
                         LOGICAL(interpolate)[i % LENGTH(interpolate)], 
                         &gc, dd);
            }
            UNPROTECT(2);
        }
    }
    GEMode(0, dd);
    vmaxset(vmax);
    return R_NilValue;
}

SEXP L_cap()
{
    int i, col, row, nrow, ncol, size;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    int *rint;
    SEXP raster; 
    /* The raster is R internal colours, so convert to 
     * R external colours (strings) 
     * AND the raster is BY ROW so need to rearrange it
     * to be BY COLUMN (though the dimensions are correct) */
    SEXP image, idim;
    
    PROTECT(raster = GECap(dd));
    /* Non-complying devices will return NULL */
    if (isNull(raster)) {
        image = raster;
    } else {
        size = LENGTH(raster);
        nrow = INTEGER(getAttrib(raster, R_DimSymbol))[0];
        ncol = INTEGER(getAttrib(raster, R_DimSymbol))[1];
        
        PROTECT(image = allocVector(STRSXP, size));
        rint = INTEGER(raster);
        for (i=0; i<size; i++) {
            col = i % ncol + 1;
            row = i / ncol + 1;
            SET_STRING_ELT(image, (col - 1)*nrow + row - 1, 
                           mkChar(col2name(rint[i])));
        }
        
        PROTECT(idim = allocVector(INTSXP, 2));
        INTEGER(idim)[0] = nrow;
        INTEGER(idim)[1] = ncol;
        setAttrib(image, R_DimSymbol, idim);
        
        UNPROTECT(2);
    }
    UNPROTECT(1);
    return image;
}

/*
 * Code to draw OR size text
 * Combined to avoid code replication
 */
static SEXP gridText(SEXP label, SEXP x, SEXP y, SEXP hjust, SEXP vjust, 
		     SEXP rot, SEXP checkOverlap, double theta, Rboolean draw)
{
    int i, nx, ny;
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP txt, result = R_NilValue;
    double edgex, edgey;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    /* 
     * Bounding rectangles for checking overlapping
     * Initialised to shut up compiler
     */
    LRect *bounds = NULL;
    LRect trect;
    int numBounds = 0;
    int overlapChecking = LOGICAL(checkOverlap)[0];
    const void *vmax;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    ny = unitLength(y);
    if (ny > nx) 
	nx = ny;
    vmax = vmaxget();
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    for (i=0; i<nx; i++) {
	gcontextFromgpar(currentgp, i, &gc, dd);
	/*
	 * If drawing, convert to INCHES on device
	 * If just calculating bounds, convert to INCHES within current vp
	 */
	if (draw) {	
	    transformLocn(x, y, i, vpc, &gc,
			  vpWidthCM, vpHeightCM,
			  dd,
			  transform,
			  &(xx[i]), &(yy[i]));
	} else {
	    xx[i] = transformXtoINCHES(x, i, vpc, &gc,
				       vpWidthCM, vpHeightCM, 
				       dd);
	    yy[i] = transformYtoINCHES(y, i, vpc, &gc,
				       vpWidthCM, vpHeightCM, 
				       dd);	    
	}
    }
    /* The label can be a string or an expression
     */
    PROTECT(txt = label);
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    UNPROTECT(1);
    PROTECT(txt);
    if (overlapChecking || !draw) {
	bounds = (LRect *) R_alloc(nx, sizeof(LRect));
    }
    /* 
     * Check we have any text to draw
     */
    if (LENGTH(txt) > 0) {
	int ntxt = 0;
	if (draw) {
	    /*
	     * Drawing text
	     */
	    GEMode(1, dd);
	}
	for (i=0; i<nx; i++) {
	    int doDrawing = 1;
	    gcontextFromgpar(currentgp, i, &gc, dd);
	    /* 
	     * Generate bounding boxes when checking for overlap
	     * or sizing text
	     */
	    if (overlapChecking || !draw) {
		int j = 0;
		textRect(xx[i], yy[i], txt, i, &gc,
			 REAL(hjust)[i % LENGTH(hjust)], 
			 REAL(vjust)[i % LENGTH(vjust)], 
			 /*
			  * When calculating bounding rect for text
			  * only consider rotation of text within 
			  * local context, not relative to device
			  * (so don't add rotationAngle)
			  */
			 numeric(rot, i % LENGTH(rot)), 
			 dd, &trect);
		while (doDrawing && (j < numBounds)) 
		    if (intersect(trect, bounds[j++]))
			doDrawing = 0;
		if (doDrawing) {
		    copyRect(trect, &(bounds[numBounds]));
		    numBounds++;
		}
	    }
	    if (draw && doDrawing) {
		/* The graphics engine only takes device coordinates
		 */
		xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
		yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
		if (R_FINITE(xx[i]) && R_FINITE(yy[i])) {
		    gcontextFromgpar(currentgp, i, &gc, dd);
		    if (isExpression(txt))
			GEMathText(xx[i], yy[i],
				   VECTOR_ELT(txt, i % LENGTH(txt)),
				   REAL(hjust)[i % LENGTH(hjust)], 
				   REAL(vjust)[i % LENGTH(vjust)], 
				   numeric(rot, i % LENGTH(rot)) + 
				   rotationAngle, 
				   &gc, dd);
		    else
			GEText(xx[i], yy[i], 
			       CHAR(STRING_ELT(txt, i % LENGTH(txt))),
			       (gc.fontface == 5) ? CE_SYMBOL :
			       getCharCE(STRING_ELT(txt, i % LENGTH(txt))),
			       REAL(hjust)[i % LENGTH(hjust)], 
			       REAL(vjust)[i % LENGTH(vjust)], 
			       numeric(rot, i % LENGTH(rot)) + 
			       rotationAngle, 
			       &gc, dd);
		}
	    }
	    if (!draw) {
		double minx, maxx, miny, maxy;
		/*
		 * Sizing text
		 */
		if (R_FINITE(xx[i]) && R_FINITE(yy[i])) {
		    minx = fmin2(trect.x1, 
				 fmin2(trect.x2, 
				       fmin2(trect.x3, trect.x4)));
		    if (minx < xmin)
			xmin = minx;
		    maxx = fmax2(trect.x1, 
				 fmax2(trect.x2, 
				       fmax2(trect.x3, trect.x4)));
		    if (maxx > xmax)
			xmax = maxx;
		    miny = fmin2(trect.y1, 
				 fmin2(trect.y2, 
				       fmin2(trect.y3, trect.y4)));
		    if (miny < ymin)
			ymin = miny;
		    maxy = fmax2(trect.y1, 
				 fmax2(trect.y2, 
				       fmax2(trect.y3, trect.y4)));
		    if (maxy > ymax)
			ymax = maxy;
		    /*
		     * Calculate edgex and edgey for case where this is 
		     * the only rect
		     */
		    {
			double xxx[4], yyy[4];
			/*
			 * Must be in clock-wise order for polygonEdge
			 */
			xxx[0] = trect.x4; yyy[0] = trect.y4;
			xxx[1] = trect.x3; yyy[1] = trect.y3;
			xxx[2] = trect.x2; yyy[2] = trect.y2;
			xxx[3] = trect.x1; yyy[3] = trect.y1;
			polygonEdge(xxx, yyy, 4, theta,
				    &edgex, &edgey);
		    }
		    ntxt++;
		}
	    }
	}
	if (draw) {
	    GEMode(0, dd);
	}
	if (ntxt > 0) {
	    result = allocVector(REALSXP, 4);
	    /*
	     * If there is more than one text, just produce edge
	     * based on bounding rect of all text
	     */
	    if (ntxt > 1) {
		/*
		 * Produce edge of rect bounding all text
		 */
		rectEdge(xmin, ymin, xmax, ymax, theta,
			 &edgex, &edgey);
	    }
	    /*
	     * Reverse the scale adjustment (zoom factor)
	     * when calculating physical value to return to user-level
	     */
	    REAL(result)[0] = edgex / 
		REAL(gridStateElement(dd, GSS_SCALE))[0];
	    REAL(result)[1] = edgey / 
		REAL(gridStateElement(dd, GSS_SCALE))[0];
	    REAL(result)[2] = (xmax - xmin) / 
		REAL(gridStateElement(dd, GSS_SCALE))[0];
	    REAL(result)[3] = (ymax - ymin) / 
		REAL(gridStateElement(dd, GSS_SCALE))[0];
	}
    }
    vmaxset(vmax);
    UNPROTECT(1);
    return result;
}

SEXP L_text(SEXP label, SEXP x, SEXP y, SEXP hjust, SEXP vjust, 
	    SEXP rot, SEXP checkOverlap)
{
    gridText(label, x, y, hjust, vjust, rot, checkOverlap, 0, TRUE);
    return R_NilValue;    
}

/*
 * Return four values representing boundary of text (which may consist
 * of multiple pieces of text, unaligned, and/or rotated) 
 * in INCHES.
 *
 * Result is (xmin, xmax, ymin, ymax)
 *
 * Return NULL if no text to draw;  R code will generate unit from that
 */
SEXP L_textBounds(SEXP label, SEXP x, SEXP y, 
		  SEXP hjust, SEXP vjust, SEXP rot, SEXP theta)
{
    SEXP checkOverlap = allocVector(LGLSXP, 1);
    LOGICAL(checkOverlap)[0] = FALSE;
    return gridText(label, x, y, hjust, vjust, rot, checkOverlap, 
		    REAL(theta)[0], FALSE);
}

SEXP L_points(SEXP x, SEXP y, SEXP pch, SEXP size)
{
    int i, nx, npch, nss;
    /*    double *xx, *yy;*/
    double *xx, *yy, *ss;
    int *ps;
    int pType;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    double symbolSize;
    const void *vmax;
    int gpIsScalar[15] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
    LViewportContext vpc;
    R_GE_gcontext gc, gcCache;
    LTransform transform;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    initGContext(currentgp, &gc, dd, gpIsScalar, &gcCache);
    nx = unitLength(x); 
    npch = LENGTH(pch);
    nss = unitLength(size);
    /* Convert the x and y values to CM locations */
    vmax = vmaxget();
    xx = (double *) R_alloc(nx, sizeof(double));
    yy = (double *) R_alloc(nx, sizeof(double));
    for (i=0; i<nx; i++) {
	updateGContext(currentgp, i, &gc, dd, gpIsScalar, &gcCache);
	transformLocn(x, y, i, vpc, &gc,
		      vpWidthCM, vpHeightCM,
		      dd,
		      transform,
		      &(xx[i]), &(yy[i]));
	/* The graphics engine only takes device coordinates
	 */
	xx[i] = toDeviceX(xx[i], GE_INCHES, dd);
	yy[i] = toDeviceY(yy[i], GE_INCHES, dd);
    }
    ss = (double *) R_alloc(nss, sizeof(double));
    for (i=0; i < nss; i++) {
        ss[i] = transformWidthtoINCHES(size, i, vpc, &gc,
                                       vpWidthCM, vpHeightCM, dd);
        ss[i] = toDeviceWidth(ss[i], GE_INCHES, dd);
    }
    ps = (int *) R_alloc(npch, sizeof(int));
    if (isString(pch)) pType = 0;
    else if (isInteger(pch)) pType = 1;
    else if (isReal(pch)) pType = 2;
    else pType = 3;
    for (i=0; i < npch; i++) {
        switch (pType) {
        case 0:
            /* 
             * FIXME:
             * Resolve any differences between this and FixupPch()
             * in plot.c ? 
             */
            ps[i] = GEstring_to_pch(STRING_ELT(pch, i));
            break;
        case 1:
            ps[i] = INTEGER(pch)[i];
            break;
        case 2:
            ps[i] = R_FINITE(REAL(pch)[i]) ? (int) REAL(pch)[i] : NA_INTEGER;
            break;
        }
    }
    GEMode(1, dd);
    for (i=0; i<nx; i++)
	if (R_FINITE(xx[i]) && R_FINITE(yy[i])) {
	    /* FIXME:  The symbols will not respond to viewport
	     * rotations !!!
	     */
	    int ipch = NA_INTEGER /* -Wall */;
	    updateGContext(currentgp, i, &gc, dd, gpIsScalar, &gcCache);
	    symbolSize = ss[i % nss];
	    if (R_FINITE(symbolSize)) {
	        if (pType == 3) {
	            error(_("invalid plotting symbol"));
	        }
	        ipch = ps[i % npch];
	        /*
	         * special case for pch = "."
	         */
	        if (ipch == 46) symbolSize = gpCex(currentgp, i);
            /*
             * FIXME: 
             * For character-based symbols, we need to modify
             * gc->cex so that the FONT size corresponds to
             * the specified symbolSize.
             */
	        GESymbol(xx[i], yy[i], ipch, symbolSize, &gc, dd);
	    }
	}
    GEMode(0, dd);
    vmaxset(vmax);
    return R_NilValue;
}

SEXP L_clip(SEXP x, SEXP y, SEXP w, SEXP h, SEXP hjust, SEXP vjust) 
{
    double xx, yy, ww, hh;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp, currentClip;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    GEMode(1, dd);
    /*
     * Only set ONE clip rectangle (i.e., NOT vectorised)
     */
    gcontextFromgpar(currentgp, 0, &gc, dd);
    transformLocn(x, y, 0, vpc, &gc,
		  vpWidthCM, vpHeightCM,
		  dd,
		  transform,
		  &xx, &yy);
    ww = transformWidthtoINCHES(w, 0, vpc, &gc,
				vpWidthCM, vpHeightCM,
				dd);
    hh = transformHeighttoINCHES(h, 0, vpc, &gc,
				 vpWidthCM, vpHeightCM,
				 dd);
    /* 
     * We can ONLY clip if the total rotation angle is zero.
     */
    if (rotationAngle == 0) {
        xx = justifyX(xx, ww, REAL(hjust)[0]);
	yy = justifyY(yy, hh, REAL(vjust)[0]);
	/* The graphics engine only takes device coordinates
	 */
	xx = toDeviceX(xx, GE_INCHES, dd);
	yy = toDeviceY(yy, GE_INCHES, dd);
	ww = toDeviceWidth(ww, GE_INCHES, dd);
	hh = toDeviceHeight(hh, GE_INCHES, dd);
	if (R_FINITE(xx) && R_FINITE(yy) && 
	    R_FINITE(ww) && R_FINITE(hh)) {
	    GESetClip(xx, yy, xx + ww, yy + hh, dd);
	    /*
	     * ALSO set the current clip region for the 
	     * current viewport so that, if a viewport
	     * is pushed within the current viewport,
	     * when that viewport gets popped again,
	     * the clip region returns to what was set
	     * by THIS clipGrob (NOT to the current
	     * viewport's previous setting)
	     */
	    PROTECT(currentClip = allocVector(REALSXP, 4));
	    REAL(currentClip)[0] = xx;
	    REAL(currentClip)[1] = yy;
	    REAL(currentClip)[2] = xx + ww;
	    REAL(currentClip)[3] = yy + hh;
	    SET_VECTOR_ELT(currentvp, PVP_CLIPRECT, currentClip);
	    UNPROTECT(1);
	}
    } else {
        warning(_("unable to clip to rotated rectangle"));
    }
    GEMode(0, dd);
    return R_NilValue;    
}

SEXP L_pretty(SEXP scale) {
    double min = numeric(scale, 0);
    double max = numeric(scale, 1);
    double temp;
    /* FIXME:  This is just a dummy pointer because we do not have
     * log scales.  This will cause death and destruction if it is 
     * not addressed when log scales are added !
     */
    double *usr = NULL;
    double axp[3];
    /* FIXME:  Default preferred number of ticks hard coded ! */
    int n = 5;
    Rboolean swap = min > max;
    /* 
     * Feature: 
     * like R, something like  xscale = c(100,0)  just works 
     */
    if(swap) { 
	temp = min; min = max; max = temp;
    }

    GEPretty(&min, &max, &n);

    if(swap) {
	temp = min; min = max; max = temp;
    }

    axp[0] = min;
    axp[1] = max;
    axp[2] = n;
    /* FIXME:  "log" flag hard-coded to FALSE because we do not
     * have log scales yet
     */
    return Rf_CreateAtVector(axp, usr, n, FALSE);
}

/* 
 * NOTE: This does not go through the graphics engine, but
 * skips straight to the device to obtain a mouse click.
 * This is because I do not want to put a GELocator in the 
 * graphics engine;  that would be a crappy long term solution.
 * I will wait for a better event-loop/call-back solution before
 * doing something with the graphics engine.
 * This is a stop gap in the meantime.
 * 
 * The answer is in INCHES
 */

SEXP L_locator() {
    double x = 0;
    double y = 0;
    SEXP answer;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    GEMode(2, dd);
    PROTECT(answer = allocVector(REALSXP, 2));
    /*
     * Get a mouse click
     * Fails if user did not click mouse button 1
     */
    if(dd->dev->locator && dd->dev->locator(&x, &y, dd->dev)) {
	REAL(answer)[0] = fromDeviceX(x, GE_INCHES, dd);
	REAL(answer)[1] = fromDeviceY(y, GE_INCHES, dd);
    } else {
	REAL(answer)[0] = NA_REAL;
	REAL(answer)[1] = NA_REAL;	
    }
    GEMode(0, dd);
    UNPROTECT(1);
    return answer;
}

/*
 * ****************************************
 * Calculating boundaries of primitives
 *
 * ****************************************
 */

/*
 * Return four values representing boundary of set of locations
 * in INCHES.
 *
 * Result is (xmin, xmax, ymin, ymax)
 *
 * Used for lines, segments, polygons
 */
SEXP L_locnBounds(SEXP x, SEXP y, SEXP theta)
{
    int i, nx, ny, nloc;
    double *xx, *yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP result = R_NilValue;
    const void *vmax;
    double xmin = DOUBLE_XMAX;
    double xmax = -DOUBLE_XMAX;
    double ymin = DOUBLE_XMAX;
    double ymax = -DOUBLE_XMAX;
    double edgex, edgey;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    nx = unitLength(x); 
    ny = unitLength(y);
    if (ny > nx) 
	nx = ny;
    nloc = 0;
    vmax = vmaxget();
    if (nx > 0) {
	xx = (double *) R_alloc(nx, sizeof(double));
	yy = (double *) R_alloc(nx, sizeof(double));
	for (i=0; i<nx; i++) {
	    gcontextFromgpar(currentgp, i, &gc, dd);
	    xx[i] = transformXtoINCHES(x, i, vpc, &gc,
				       vpWidthCM, vpHeightCM, 
				       dd);
	    yy[i] = transformYtoINCHES(y, i, vpc, &gc,
				       vpWidthCM, vpHeightCM, 
				       dd);
	    /*
	     * Determine min/max x/y values
	     */
	    if (R_FINITE(xx[i]) && R_FINITE(yy[i])) {
		if (xx[i] < xmin)
		    xmin = xx[i];
		if (xx[i] > xmax)
		    xmax = xx[i];
		if (yy[i] < ymin)
		    ymin = yy[i];
		if (yy[i] > ymax)
		    ymax = yy[i];
		nloc++;
	    }
	}
    }
    if (nloc > 0) {
	hullEdge(xx, yy, nx, REAL(theta)[0], &edgex, &edgey);
	result = allocVector(REALSXP, 4);
	/*
	 * Reverse the scale adjustment (zoom factor)
	 * when calculating physical value to return to user-level
	 */
	REAL(result)[0] = edgex / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[1] = edgey / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[2] = (xmax - xmin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
	REAL(result)[3] = (ymax - ymin) / 
	    REAL(gridStateElement(dd, GSS_SCALE))[0];
    } 
    vmaxset(vmax);
    return result;
}

/*
 * ****************************************
 * Calculating text metrics
 *
 * ****************************************
 */
SEXP L_stringMetric(SEXP label)
{
    int i, n;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    R_GE_gcontext gc;
    LTransform transform;
    SEXP currentvp, currentgp;
    SEXP txt;
    SEXP result = R_NilValue;
    SEXP ascent = R_NilValue;
    SEXP descent = R_NilValue;
    SEXP width = R_NilValue;
    const void *vmax;
    double asc, dsc, wid;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    getViewportContext(currentvp, &vpc);
    /* The label can be a string or an expression: is protected.
     */
    txt = label;
    if (isSymbol(txt) || isLanguage(txt))
	txt = coerceVector(txt, EXPRSXP);
    else if (!isExpression(txt))
	txt = coerceVector(txt, STRSXP);
    PROTECT(txt);
    n = LENGTH(txt);
    vmax = vmaxget();
    PROTECT(ascent = allocVector(REALSXP, n));
    PROTECT(descent = allocVector(REALSXP, n));
    PROTECT(width = allocVector(REALSXP, n));
    if (n > 0) {
	for (i=0; i<n; i++) {
	    gcontextFromgpar(currentgp, i, &gc, dd);
            if (isExpression(txt))
                GEExpressionMetric(VECTOR_ELT(txt, i % LENGTH(txt)), &gc, 
                                   &asc, &dsc, &wid,
                                   dd);
            else 
                GEStrMetric(CHAR(STRING_ELT(txt, i)), 
                            getCharCE(STRING_ELT(txt, i)), &gc,
                            &asc, &dsc, &wid,
                            dd);
            /*
             * Reverse the scale adjustment (zoom factor)
             * when calculating physical value to return to user-level
             */
            REAL(ascent)[i] = fromDeviceHeight(asc, GE_INCHES, dd) / 
                REAL(gridStateElement(dd, GSS_SCALE))[0];
            REAL(descent)[i] = fromDeviceHeight(dsc, GE_INCHES, dd) /
                REAL(gridStateElement(dd, GSS_SCALE))[0];
            REAL(width)[i] = fromDeviceWidth(wid, GE_INCHES, dd) /
                REAL(gridStateElement(dd, GSS_SCALE))[0];
	}
    }
    PROTECT(result = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(result, 0, ascent);
    SET_VECTOR_ELT(result, 1, descent);
    SET_VECTOR_ELT(result, 2, width);    
    vmaxset(vmax);
    UNPROTECT(5);
    return result;
}

