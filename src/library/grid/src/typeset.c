/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2022 The R Core Team
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

/* We are assuming here that the R code has checked that 
 * span is a "RTextSpan"
 */
static void typesetSpan(SEXP span, SEXP x, SEXP y, SEXP w, Rboolean draw) 
{
    double xx, yy, ww;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    R_GE_gcontext gc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    /* This copy is used to store/cache resolved gp$fill to avoid
     * stupid amounts of pattern resolving (resolving a resolved
     * pattern is basically a no-op), WITHOUT touching current gp
     * in 'grid' state. */
    currentgp = PROTECT(duplicate(currentgp));
    /* Do not need fill, so set gp$fill to "black" to avoid any
     * pattern resolution. */
    SET_VECTOR_ELT(currentgp, GP_FILL, mkString("black"));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    /* getViewportContext(currentvp, &vpc); */
    fillViewportContextFromViewport(currentvp, &vpc);
    if (draw) {
	GEMode(1, dd);
    }
    xx = transformXtoINCHES(x, 0, vpc, &gc, vpWidthCM, vpHeightCM, dd);
    yy = transformYtoINCHES(y, 0, vpc, &gc, vpWidthCM, vpHeightCM, dd);
    xx = toDeviceX(xx, GE_INCHES, dd);
    yy = toDeviceY(yy, GE_INCHES, dd);
    if (ISNA(REAL(w)[0])) {
        ww = NA_REAL;
    } else {
        /* Width is taken to be in inches by textshaping::shape_text() */
	ww = transformWidthtoINCHES(w, 0, vpc, &gc,
				    vpWidthCM, vpHeightCM,
				    dd);
    }
    if (R_FINITE(xx) && R_FINITE(yy))
        GETypeset(span, xx, yy, ww, dd);
    if (draw) {
	GEMode(0, dd);
    }
    UNPROTECT(1);
}

SEXP L_typeset(SEXP span, SEXP x, SEXP y, SEXP w) {
    typesetSpan(span, x, y, w, TRUE);
    return R_NilValue;    
}

/* We are assuming here that the R code has checked that 
 * info is a "RGlyphInfo"
 */
static void renderGlyphs(SEXP runs, SEXP glyphInfo, 
                         SEXP x, SEXP y, Rboolean draw) 
{
    int i, nruns = LENGTH(runs);
    double xx, yy;
    double vpWidthCM, vpHeightCM;
    double rotationAngle;
    LViewportContext vpc;
    LTransform transform;
    R_GE_gcontext gc;
    SEXP currentvp, currentgp;
    /* Get the current device 
     */
    pGEDevDesc dd = getDevice();
    currentvp = gridStateElement(dd, GSS_VP);
    currentgp = gridStateElement(dd, GSS_GPAR);
    /* This copy is used to store/cache resolved gp$fill to avoid
     * stupid amounts of pattern resolving (resolving a resolved
     * pattern is basically a no-op), WITHOUT touching current gp
     * in 'grid' state. */
    currentgp = PROTECT(duplicate(currentgp));
    /* Do not need fill, so set gp$fill to "black" to avoid any
     * pattern resolution. */
    SET_VECTOR_ELT(currentgp, GP_FILL, mkString("black"));
    getViewportTransform(currentvp, dd, 
			 &vpWidthCM, &vpHeightCM, 
			 transform, &rotationAngle);
    /* getViewportContext(currentvp, &vpc); */
    fillViewportContextFromViewport(currentvp, &vpc);
    if (draw) {
	GEMode(1, dd);
    }
    xx = transformXtoINCHES(x, 0, vpc, &gc, vpWidthCM, vpHeightCM, dd);
    yy = transformYtoINCHES(y, 0, vpc, &gc, vpWidthCM, vpHeightCM, dd);
    xx = toDeviceX(xx, GE_INCHES, dd);
    yy = toDeviceY(yy, GE_INCHES, dd);
    int *glyphs = INTEGER(R_GE_glyphIndex(glyphInfo));
    double *glyphX = REAL(R_GE_glyphXOffset(glyphInfo));
    double *glyphY = REAL(R_GE_glyphYOffset(glyphInfo));
    if (R_FINITE(xx) && R_FINITE(yy)) {
        int offset = 0;
        for (i=0; i<nruns; i++) {
            int runLength = INTEGER(runs)[i];
            SEXP font = VECTOR_ELT(R_GE_glyphFont(glyphInfo), offset);
            GEGlyph(runLength, 
                    glyphs + offset, 
                    glyphX + offset, 
                    glyphY + offset, 
                    font,
                    xx, yy, dd);
            offset = offset + runLength;
        }
    }
    if (draw) {
        GEMode(0, dd);
    }
    UNPROTECT(1);
}

SEXP L_glyph(SEXP runs, SEXP glyphInfo, SEXP x, SEXP y) {
    renderGlyphs(runs, glyphInfo, x, y, TRUE);
    return R_NilValue;    
}
