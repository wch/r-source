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

#include "grid.h"

extern int gridRegisterIndex;

/* Some access methods for viewports */
SEXP viewportX(SEXP vp) {
    return getListElement(vp, "x");
}

SEXP viewportY(SEXP vp) {
    return getListElement(vp, "y");
}

SEXP viewportWidth(SEXP vp) {
    return getListElement(vp, "width");
}

SEXP viewportHeight(SEXP vp) {
    return getListElement(vp, "height");
}

char* viewportFontFamily(SEXP vp) {
    return CHAR(STRING_ELT(getListElement(vp, "cur.fontfamily"), 0));
}

int viewportFont(SEXP vp) {
    return INTEGER(getListElement(vp, "cur.font"))[0];
}

double viewportFontSize(SEXP vp) {
    return numeric(getListElement(vp, "cur.fontsize"), 0);
}

double viewportLineHeight(SEXP vp) {
    return numeric(getListElement(vp, "cur.lineheight"), 0);
}

double viewportCex(SEXP vp) {
    return numeric(getListElement(vp, "cur.cex"), 0);
}

Rboolean viewportClip(SEXP vp) {
    return LOGICAL(getListElement(vp, "clip"))[0];
}

SEXP viewportCurClip(SEXP vp) {
    return getListElement(vp, "cur.clip");
}

double viewportXScaleMin(SEXP vp) {
    return numeric(getListElement(vp, "xscale"), 0);
}

double viewportXScaleMax(SEXP vp) {
    return numeric(getListElement(vp, "xscale"), 1);
}

double viewportYScaleMin(SEXP vp) {
    return numeric(getListElement(vp, "yscale"), 0);
}

double viewportYScaleMax(SEXP vp) {
    return numeric(getListElement(vp, "yscale"), 1);
}

int viewportHJust(SEXP vp) {
    return INTEGER(getListElement(vp, "valid.just"))[0];
}

int viewportVJust(SEXP vp) {
    return INTEGER(getListElement(vp, "valid.just"))[1];
}

double viewportAngle(SEXP vp) {
    return numeric(getListElement(vp, "angle"), 0);
}

SEXP viewportLayout(SEXP vp) {
    return getListElement(vp, "layout");
}

SEXP viewportLayoutPosRow(SEXP vp) {
    return getListElement(vp, "valid.pos.row");
}

SEXP viewportLayoutPosCol(SEXP vp) {
    return getListElement(vp, "valid.pos.col");
}

SEXP viewportParent(SEXP vp) {
    return getListElement(vp, "parent");
}

SEXP viewportCurrentTransform(SEXP vp) {
    return getListElement(vp, "cur.trans");
}

SEXP viewportCurrentLayoutWidths(SEXP vp) {
    return getListElement(vp, "cur.widths");
}

SEXP viewportCurrentLayoutHeights(SEXP vp) {
    return getListElement(vp, "cur.heights");
}

SEXP viewportCurrentWidthCM(SEXP vp) {
    return getListElement(vp, "cur.width.cm");
}

SEXP viewportCurrentHeightCM(SEXP vp) {
    return getListElement(vp, "cur.height.cm");
}

SEXP viewportCurrentRotation(SEXP vp) {
    return getListElement(vp, "cur.rotation");
}

void fillViewportLocationFromViewport(SEXP vp, LViewportLocation *vpl) 
{
    vpl->x = viewportX(vp);
    vpl->y = viewportY(vp);
    vpl->width = viewportWidth(vp);
    vpl->height = viewportHeight(vp);
    vpl->hjust = viewportHJust(vp);
    vpl->vjust = viewportVJust(vp);
}

void fillViewportContextFromViewport(SEXP vp, 
				     LViewportContext *vpc)
{
    vpc->xscalemin = viewportXScaleMin(vp);
    vpc->xscalemax = viewportXScaleMax(vp);
    vpc->yscalemin = viewportYScaleMin(vp);
    vpc->yscalemax = viewportYScaleMax(vp);
}

void copyViewportContext(LViewportContext vpc1, LViewportContext *vpc2)
{
    vpc2->xscalemin = vpc1.xscalemin;
    vpc2->xscalemax = vpc1.xscalemax;
    vpc2->yscalemin = vpc1.yscalemin;
    vpc2->yscalemax = vpc1.yscalemax;
}

void gcontextFromViewport(SEXP vp, LGContext *gc) {
    gc->font = viewportFont(vp);
    gc->fontsize = viewportFontSize(vp);
    gc->cex = viewportCex(vp);
    gc->lineheight = viewportLineHeight(vp);
    strcpy(gc->fontfamily, viewportFontFamily(vp));
}

/* The idea is to produce a transformation for this viewport which
 * will take any location in INCHES and turn it into a location on the 
 * Device in INCHES.
 * The reason for working in INCHES is because we want to be able to
 * do rotations as part of the transformation.
 * If "incremental" is true, then we just work from the "current"
 * values of the parent.  Otherwise, we have to recurse and recalculate
 * everything from scratch.
 */
void calcViewportTransform(SEXP vp, SEXP parent, Rboolean incremental,
			   GEDevDesc *dd)
{
    int i, j;
    double vpWidthCM, vpHeightCM, rotationAngle;
    double parentWidthCM, parentHeightCM;
    double xINCHES, yINCHES;
    double xadj, yadj;
    double parentAngle;
    LViewportLocation vpl;
    LViewportContext vpc, parentContext;
    LGContext gc, parentgc;
    LTransform thisLocation, thisRotation, thisJustification, thisTransform;
    LTransform tempTransform, parentTransform, transform;
    SEXP currentWidthCM, currentHeightCM, currentRotation;
    SEXP currentTransform;
    /* This should never be true when we are doing an incremental
     * calculation
     */
    if (isNull(parent)) {
	/* We have a top-level viewport; the parent is the device
	 */
	getDeviceSize(dd, &parentWidthCM, &parentHeightCM);
	/* For a device the transform is the identity transform
	 */
	identity(parentTransform);
	/* For a device, xmin=0, ymin=0, xmax=1, ymax=1, and
	 */
	parentContext.xscalemin = 0;
	parentContext.yscalemin = 0;
	parentContext.xscalemax = 1;
	parentContext.yscalemax = 1;
	/* FIXME:  How do I figure out the device fontsize ?
	 * From ps.options etc, ... ?
	 * FIXME:  How do I figure out the device lineheight ??
	 * FIXME:  How do I figure out the device cex ??
	 * FIXME:  How do I figure out the device font ??
	 * FIXME:  How do I figure out the device fontfamily ??
	 */
	parentgc.fontsize = 10;
	parentgc.lineheight = 1.2;
	parentgc.cex = 1;
	parentgc.font = 1;
	parentgc.fontfamily[0] = '\0';
	/* The device is not rotated
	 */
	parentAngle = 0;
	fillViewportLocationFromViewport(vp, &vpl);
    } else {
	/* Get parent transform (etc ...)
	 * If necessary, recalculate the parent transform (etc ...)
	 */
	if (!incremental)
	    calcViewportTransform(parent, viewportParent(parent), 0, dd);
	/* Get information required to transform viewport location
	 */
	parentWidthCM = REAL(viewportCurrentWidthCM(parent))[0];
	parentHeightCM = REAL(viewportCurrentHeightCM(parent))[0];
	parentAngle = REAL(viewportCurrentRotation(parent))[0];
	for (i=0; i<3; i++)
	    for (j=0; j<3; j++)
		parentTransform[i][j] = 
		    REAL(viewportCurrentTransform(parent))[i +3*j];
	fillViewportContextFromViewport(parent, &parentContext);
	gcontextFromViewport(parent, &parentgc);
	/* In order for the vp to get its vpl from a layout
	 * it must have specified a layout.pos and the parent
	 * must have a layout
	 * FIXME:  Actually, in addition, layout.pos.row and
	 * layout.pos.col must be valid for the layout
	 */
	if ((isNull(viewportLayoutPosRow(vp)) && 
	     isNull(viewportLayoutPosCol(vp))) ||
	    isNull(viewportLayout(parent)))
	    fillViewportLocationFromViewport(vp, &vpl);
	else
	    calcViewportLocationFromLayout(viewportLayoutPosRow(vp),
					   viewportLayoutPosCol(vp),
					   parent,
					   &vpl);
    }
    /* NOTE that we are not doing a transformLocn here because
     * we just want locations and dimensions (in INCHES) relative to 
     * the parent, NOT relative to the device.
     */
    /* First, convert the location of the viewport into CM
     */
    xINCHES = transformXtoINCHES(vpl.x, 0, parentContext, &parentgc,
				 parentWidthCM, parentHeightCM, 
				 dd);
    yINCHES = transformYtoINCHES(vpl.y, 0, parentContext, &parentgc,
				 parentWidthCM, parentHeightCM, 
				 dd);
    /* Calculate the width and height of the viewport in CM too
     * so that any viewports within this one can do transformations
     */
    vpWidthCM = transformWidthtoINCHES(vpl.width, 0, parentContext, &parentgc,
				       parentWidthCM, parentHeightCM,
				       dd)*2.54;
    vpHeightCM = transformHeighttoINCHES(vpl.height, 0, parentContext, 
					 &parentgc,
					 parentWidthCM, 
					 parentHeightCM, 
					 dd)*2.54;
    /* Determine justification required
     */
    justification(vpWidthCM, vpHeightCM, vpl.hjust, vpl.vjust,
		  &xadj, &yadj);
    /* Next, produce the transformation to add the location of
     * the viewport to the location.
     */
    /* Produce transform for this viewport
     */
    translation(xINCHES, yINCHES, thisLocation);
    if (viewportAngle(vp) != 0)
	rotation(viewportAngle(vp), thisRotation);
    else
	identity(thisRotation);
    translation(xadj/2.54, yadj/2.54, thisJustification);
    /* Position relative to origin of rotation THEN rotate.
     */
    multiply(thisJustification, thisRotation, tempTransform);
    /* Translate to bottom-left corner.
     */
    multiply(tempTransform, thisLocation, thisTransform);
    /* Combine with parent's transform
     */
    multiply(thisTransform, parentTransform, transform);
    /* Sum up the rotation angles
     */
    rotationAngle = parentAngle + viewportAngle(vp);
    /* Finally, allocate the rows and columns for this viewport's
     * layout if it has one
     */
    if (!isNull(viewportLayout(vp))) {
	fillViewportContextFromViewport(vp, &vpc);
	gcontextFromViewport(vp, &gc);
	calcViewportLayout(vp, vpWidthCM, vpHeightCM, vpc, &gc, dd);
    }
    /* Record all of the answers in the viewport
     * (the layout calculations are done within calcViewportLayout)
     */
    PROTECT(currentWidthCM = allocVector(REALSXP, 1));
    REAL(currentWidthCM)[0] = vpWidthCM;
    PROTECT(currentHeightCM = allocVector(REALSXP, 1));
    REAL(currentHeightCM)[0] = vpHeightCM;
    PROTECT(currentRotation = allocVector(REALSXP, 1));
    REAL(currentRotation)[0] = rotationAngle;
    PROTECT(currentTransform = allocMatrix(REALSXP, 3, 3));
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    REAL(currentTransform)[i + 3*j] = transform[i][j];
    setListElement(vp, "cur.width.cm", currentWidthCM);
    setListElement(vp, "cur.height.cm", currentHeightCM);
    setListElement(vp, "cur.rotation", currentRotation);
    setListElement(vp, "cur.trans", currentTransform);
    UNPROTECT(4);
}

void initVP(GEDevDesc *dd)
{
    SEXP vpfnname, vpfn, vp;
    SEXP xscale, yscale;
    SEXP ff, font, lh, fs, cex;
    SEXP currentgp = gridStateElement(dd, GSS_GPAR);
    SEXP gsd = (SEXP) dd->gesd[gridRegisterIndex]->systemSpecific;
    PROTECT(vpfnname = findFun(install("grid.top.level.vp"), R_gridEvalEnv));
    PROTECT(vpfn = lang1(vpfnname));
    PROTECT(vp = eval(vpfn, R_GlobalEnv));
    /* 
     * Set the "native" scale of the top viewport to be the
     * natural device coordinate system (e.g., points in 
     * postscript, pixels in X11, ...)
     */
    PROTECT(xscale = allocVector(REALSXP, 2));
    REAL(xscale)[0] = dd->dev->left;
    REAL(xscale)[1] = dd->dev->right;
    setListElement(vp, "xscale", xscale);
    PROTECT(yscale = allocVector(REALSXP, 2));
    REAL(yscale)[0] = dd->dev->bottom;
    REAL(yscale)[1] = dd->dev->top;
    setListElement(vp, "yscale", yscale);
    PROTECT(ff = allocVector(STRSXP, 1));
    SET_STRING_ELT(ff, 0, mkChar(gpFontFamily(currentgp, 0)));
    setListElement(vp, "cur.fontfamily", ff);
    PROTECT(font = allocVector(INTSXP, 1));
    INTEGER(font)[0] = gpFont(currentgp, 0);
    setListElement(vp, "cur.font", font);
    PROTECT(lh = allocVector(REALSXP, 1));
    REAL(lh)[0] = gpLineHeight(currentgp, 0);
    setListElement(vp, "cur.lineheight", lh);
    PROTECT(fs = allocVector(REALSXP, 1));
    REAL(fs)[0] = gpFontSize(currentgp, 0);
    setListElement(vp, "cur.fontsize", fs);
    PROTECT(cex = allocVector(REALSXP, 1));
    REAL(cex)[0] = gpCex(currentgp, 0);
    setListElement(vp, "cur.cex", cex);
    vp = doSetViewport(vp, R_NilValue, dd);
    SET_VECTOR_ELT(gsd, GSS_VP, vp);
    UNPROTECT(10);
}

