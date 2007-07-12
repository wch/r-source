/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-5 The R Development Core Team
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
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

#include "grid.h"

extern int gridRegisterIndex;

/* Some access methods for viewports */
SEXP viewportX(SEXP vp) {
    return VECTOR_ELT(vp, VP_X);
}

SEXP viewportY(SEXP vp) {
    return VECTOR_ELT(vp, VP_Y);
}

SEXP viewportWidth(SEXP vp) {
    return VECTOR_ELT(vp, VP_WIDTH);
}

SEXP viewportHeight(SEXP vp) {
    return VECTOR_ELT(vp, VP_HEIGHT);
}

Rboolean viewportClip(SEXP vp) {
    return LOGICAL(VECTOR_ELT(vp, VP_CLIP))[0];
}

double viewportXScaleMin(SEXP vp) {
    return numeric(VECTOR_ELT(vp, VP_XSCALE), 0);
}

double viewportXScaleMax(SEXP vp) {
    return numeric(VECTOR_ELT(vp, VP_XSCALE), 1);
}

double viewportYScaleMin(SEXP vp) {
    return numeric(VECTOR_ELT(vp, VP_YSCALE), 0);
}

double viewportYScaleMax(SEXP vp) {
    return numeric(VECTOR_ELT(vp, VP_YSCALE), 1);
}

double viewportAngle(SEXP vp) {
    return numeric(VECTOR_ELT(vp, VP_ANGLE), 0);
}

SEXP viewportLayout(SEXP vp) {
    return VECTOR_ELT(vp, VP_LAYOUT);
}

double viewportHJust(SEXP vp) {
    return REAL(VECTOR_ELT(vp, VP_VALIDJUST))[0];
}

double viewportVJust(SEXP vp) {
    return REAL(VECTOR_ELT(vp, VP_VALIDJUST))[1];
}

SEXP viewportLayoutPosRow(SEXP vp) {
    return VECTOR_ELT(vp, VP_VALIDLPOSROW);
}

SEXP viewportLayoutPosCol(SEXP vp) {
    return VECTOR_ELT(vp, VP_VALIDLPOSCOL);
}

SEXP viewportgpar(SEXP vp) {
    return VECTOR_ELT(vp, PVP_GPAR);
}

const char* viewportFontFamily(SEXP vp) {
    return CHAR(STRING_ELT(VECTOR_ELT(VECTOR_ELT(vp, PVP_GPAR), GP_FONTFAMILY),
			   0));
}

int viewportFont(SEXP vp) {
    return INTEGER(VECTOR_ELT(VECTOR_ELT(vp, PVP_GPAR), GP_FONT))[0];
}

double viewportFontSize(SEXP vp) {
    return REAL(VECTOR_ELT(VECTOR_ELT(vp, PVP_GPAR), GP_FONTSIZE))[0];
}

double viewportLineHeight(SEXP vp) {
    return REAL(VECTOR_ELT(VECTOR_ELT(vp, PVP_GPAR), GP_LINEHEIGHT))[0];
}

double viewportCex(SEXP vp) {
    return numeric(VECTOR_ELT(VECTOR_ELT(vp, PVP_GPAR), GP_CEX), 0);
}

SEXP viewportTransform(SEXP vp) {
    return VECTOR_ELT(vp, PVP_TRANS);
}

SEXP viewportLayoutWidths(SEXP vp) {
    return VECTOR_ELT(vp, PVP_WIDTHS);
}

SEXP viewportLayoutHeights(SEXP vp) {
    return VECTOR_ELT(vp, PVP_HEIGHTS);
}

SEXP viewportWidthCM(SEXP vp) {
    return VECTOR_ELT(vp, PVP_WIDTHCM);
}

SEXP viewportHeightCM(SEXP vp) {
    return VECTOR_ELT(vp, PVP_HEIGHTCM);
}

SEXP viewportRotation(SEXP vp) {
    return VECTOR_ELT(vp, PVP_ROTATION);
}

SEXP viewportClipRect(SEXP vp) {
    return VECTOR_ELT(vp, PVP_CLIPRECT);
}

SEXP viewportParent(SEXP vp) {
    return VECTOR_ELT(vp, PVP_PARENT);
}

SEXP viewportChildren(SEXP vp) {
    return VECTOR_ELT(vp, PVP_CHILDREN);
}

SEXP viewportDevWidthCM(SEXP vp) {
    return VECTOR_ELT(vp, PVP_DEVWIDTHCM);
}

SEXP viewportDevHeightCM(SEXP vp) {
    return VECTOR_ELT(vp, PVP_DEVHEIGHTCM);
}

SEXP viewportParentGPar(SEXP vp) {
    return VECTOR_ELT(vp, PVP_PARENTGPAR);
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

void gcontextFromViewport(SEXP vp, R_GE_gcontext *gc, GEDevDesc *dd) {
    gcontextFromgpar(viewportgpar(vp), 0, gc, dd);
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
    R_GE_gcontext gc, parentgc;
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
	parentgc.ps = 10;
	parentgc.lineheight = 1.2;
	parentgc.cex = 1;
	parentgc.fontface = 1;
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
	parentWidthCM = REAL(viewportWidthCM(parent))[0];
	parentHeightCM = REAL(viewportHeightCM(parent))[0];
	parentAngle = REAL(viewportRotation(parent))[0];
	for (i=0; i<3; i++)
	    for (j=0; j<3; j++)
		parentTransform[i][j] = 
		    REAL(viewportTransform(parent))[i +3*j];
	fillViewportContextFromViewport(parent, &parentContext);
	/* 
	 * Don't get gcontext from parent because the most recent
	 * previous gpar setting may have come from a gTree
	 * So we look at this viewport's parentgpar slot instead
	 * 
	 * WAS gcontextFromViewport(parent, &parentgc);
	 */
	gcontextFromgpar(viewportParentGPar(vp), 0, &parentgc, dd);
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
	else if (checkPosRowPosCol(vp, parent))
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
    /* Fall out if location or size are non-finite
     */
    if (!R_FINITE(xINCHES) || 
	!R_FINITE(yINCHES) || 
	!R_FINITE(vpWidthCM) || 
	!R_FINITE(vpHeightCM))
	error(_("Non-finite location and/or size for viewport"));
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
	gcontextFromViewport(vp, &gc, dd);
	calcViewportLayout(vp, vpWidthCM, vpHeightCM, vpc, &gc, dd);
    }
    /* Record all of the answers in the viewport
     * (the layout calculations are done within calcViewportLayout)
     */
    PROTECT(currentWidthCM = ScalarReal(vpWidthCM));
    PROTECT(currentHeightCM = ScalarReal(vpHeightCM));
    PROTECT(currentRotation = ScalarReal(rotationAngle));
    PROTECT(currentTransform = allocMatrix(REALSXP, 3, 3));
    for (i=0; i<3; i++)
	for (j=0; j<3; j++)
	    REAL(currentTransform)[i + 3*j] = transform[i][j];
    SET_VECTOR_ELT(vp, PVP_WIDTHCM, currentWidthCM);
    SET_VECTOR_ELT(vp, PVP_HEIGHTCM, currentHeightCM);
    SET_VECTOR_ELT(vp, PVP_ROTATION, currentRotation);
    SET_VECTOR_ELT(vp, PVP_TRANS, currentTransform);
    UNPROTECT(4);
}

void initVP(GEDevDesc *dd)
{
    SEXP vpfnname, vpfn, vp;
    SEXP xscale, yscale;
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
    SET_VECTOR_ELT(vp, VP_XSCALE, xscale);
    PROTECT(yscale = allocVector(REALSXP, 2));
    REAL(yscale)[0] = dd->dev->bottom;
    REAL(yscale)[1] = dd->dev->top;
    SET_VECTOR_ELT(vp, VP_YSCALE, yscale);
    SET_VECTOR_ELT(vp, PVP_GPAR, currentgp);
    vp = doSetViewport(vp, TRUE, TRUE, dd);
    SET_VECTOR_ELT(gsd, GSS_VP, vp);
    UNPROTECT(5);
}

