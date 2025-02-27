/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2025  The R Core Team
 *  Copyright (C) 2002--2005  The R Foundation
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


 *  This is (a small part of) an extensive reworking by Paul Murrell
 *  of an original quick hack by Ross Ihaka designed to give a
 *  superset of the functionality in the AT&T Bell Laboratories GRZ
 *  library.
 *
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <GraphicsBase.h> 
#include <R_ext/GraphicsEngine.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#undef _
#define _(String) dgettext ("grDevices", String)
#else
#define _(String) (String)
#endif


#define checkArity_length 			\
    args = CDR(args);		       	       	\
    if(!LENGTH(CAR(args)))	       	       	\
	error(_("argument must have positive length"))

SEXP devcontrol(SEXP args)
{
    int listFlag;
    pGEDevDesc gdd = GEcurrentDevice();

    args = CDR(args);
    listFlag = asLogical(CAR(args));
    if(listFlag == NA_LOGICAL) error(_("invalid argument"));
    GEinitDisplayList(gdd);
    gdd->displayListOn = listFlag ? TRUE: FALSE;
    return ScalarLogical(listFlag);
}

SEXP devdisplaylist(SEXP args)
{
    pGEDevDesc gdd = GEcurrentDevice();
    return ScalarLogical(gdd->displayListOn);
}

SEXP devcopy(SEXP args)
{
    checkArity_length;
    GEcopyDisplayList(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

SEXP devcur(SEXP args)
{
    args = CDR(args);
    return ScalarInteger(curDevice() + 1);
}

SEXP devnext(SEXP args)
{
    checkArity_length;
    int nxt = INTEGER(CAR(args))[0];
    if (nxt == NA_INTEGER) error(_("NA argument is invalid"));
    return ScalarInteger( nextDevice(nxt - 1) + 1 );
}

SEXP devprev(SEXP args)
{
    checkArity_length;
    int prev = INTEGER(CAR(args))[0];
    if (prev == NA_INTEGER) error(_("NA argument is invalid"));
    return ScalarInteger( prevDevice(prev - 1) + 1 );
}

SEXP devset(SEXP args)
{
    checkArity_length;
    int devNum = INTEGER(CAR(args))[0];
    if (devNum == NA_INTEGER) error(_("NA argument is invalid"));
    return ScalarInteger( selectDevice(devNum - 1) + 1 );
}

SEXP devoff(SEXP args)
{
    checkArity_length;
    killDevice(INTEGER(CAR(args))[0] - 1);
    return R_NilValue;
}

SEXP devsize(SEXP args)
{
    SEXP ans;
    pDevDesc dd = GEcurrentDevice()->dev;
    double left, right, bottom, top;

    dd->size(&left, &right, &bottom, &top, dd);
    ans = allocVector(REALSXP, 2);
    REAL(ans)[0] = fabs(right - left);
    REAL(ans)[1] = fabs(bottom - top);
    return ans;
}

SEXP devholdflush(SEXP args)
{
    pDevDesc dd = GEcurrentDevice()->dev;

    args = CDR(args);
    int level = asInteger(CAR(args));
    if(dd->holdflush && level != NA_INTEGER) level = (dd->holdflush(dd, level));
    else level = 0;
    return ScalarInteger(level);
}

SEXP devcap(SEXP args)
{
    SEXP capabilities, devcap;
    SEXP trans, transbg, raster, capture, locator, events, 
        patterns, clippaths, masks, compositing, transforms, paths, glyphs;
    pDevDesc dd = GEcurrentDevice()->dev;

    args = CDR(args);
    capabilities = CAR(args);

    PROTECT(trans = allocVector(INTSXP, 1));
    INTEGER(trans)[0] = dd->haveTransparency;
    SET_VECTOR_ELT(capabilities, R_GE_capability_semiTransparency, trans);
    UNPROTECT(1);

    PROTECT(transbg = allocVector(INTSXP, 1));
    INTEGER(transbg)[0] = dd->haveTransparentBg;
    SET_VECTOR_ELT(capabilities, 
                   R_GE_capability_transparentBackground, transbg);
    UNPROTECT(1);
    
    /* These will be NULL if the device does not define them */
    PROTECT(raster = allocVector(INTSXP, 1));
    INTEGER(raster)[0] = (dd->raster != NULL) ? dd->haveRaster : 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_rasterImage, raster);
    UNPROTECT(1);
    PROTECT(capture = allocVector(INTSXP, 1));
    INTEGER(capture)[0] = (dd->cap != NULL) ? dd->haveCapture : 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_capture, capture);
    UNPROTECT(1);
    PROTECT(locator = allocVector(INTSXP, 1));
    INTEGER(locator)[0] = (dd->locator != NULL) ? dd->haveLocator : 1;
    SET_VECTOR_ELT(capabilities, R_GE_capability_locator, locator);
    UNPROTECT(1);

    /* FIXME:  there should be a way for a device to declare its own
               events, and return information on how to set them */
    PROTECT(events = allocVector(INTSXP, 5));
    INTEGER(events)[0] = (int)(dd->canGenMouseDown);
    INTEGER(events)[1] = (int)(dd->canGenMouseMove);
    INTEGER(events)[2] = (int)(dd->canGenMouseUp);
    INTEGER(events)[3] = (int)(dd->canGenKeybd);
    INTEGER(events)[4] = (int)(dd->canGenIdle);
    SET_VECTOR_ELT(capabilities, R_GE_capability_events, events);
    UNPROTECT(1);

    /* Make conservative default guesses */
    PROTECT(patterns = allocVector(INTSXP, 1));
    INTEGER(patterns)[0] = NA_INTEGER;
    SET_VECTOR_ELT(capabilities, R_GE_capability_patterns, patterns);
    UNPROTECT(1);

    PROTECT(clippaths = allocVector(INTSXP, 1));
    INTEGER(clippaths)[0] = NA_INTEGER;
    SET_VECTOR_ELT(capabilities, 
                   R_GE_capability_clippingPaths, clippaths);
    UNPROTECT(1);

    PROTECT(masks = allocVector(INTSXP, 1));
    INTEGER(masks)[0] = NA_INTEGER;
    SET_VECTOR_ELT(capabilities, R_GE_capability_masks, masks);
    UNPROTECT(1);

    PROTECT(compositing = allocVector(INTSXP, 1));
    PROTECT(transforms = allocVector(INTSXP, 1));
    PROTECT(paths = allocVector(INTSXP, 1));
    if (dd->deviceVersion < R_GE_group) {
        INTEGER(compositing)[0] = 0;
        INTEGER(transforms)[0] = 0;
        INTEGER(paths)[0] = 0;
    } else {            
        INTEGER(compositing)[0] = NA_INTEGER;
        INTEGER(transforms)[0] = NA_INTEGER;
        INTEGER(paths)[0] = NA_INTEGER;
    }
    SET_VECTOR_ELT(capabilities, R_GE_capability_compositing, compositing);
    SET_VECTOR_ELT(capabilities, R_GE_capability_transformations, transforms);
    SET_VECTOR_ELT(capabilities, R_GE_capability_paths, paths);
    UNPROTECT(3);

    PROTECT(glyphs = allocVector(INTSXP, 1));
    if (dd->deviceVersion < R_GE_glyphs) {
        INTEGER(glyphs)[0] = 0;
    } else {
        INTEGER(glyphs)[0] = NA_INTEGER;
    }
    SET_VECTOR_ELT(capabilities, R_GE_capability_glyphs, glyphs);
    UNPROTECT(1);

    /* Further capabilities can be filled in by device */
    if (dd->deviceVersion >= R_GE_group && dd->capabilities) {
        devcap = dd->capabilities(capabilities);
    } else {
        devcap = capabilities;
    }

    return devcap;
}

SEXP devcapture(SEXP args)
{
    int i, col, row, nrow, ncol, size;
    pGEDevDesc gdd = GEcurrentDevice();
    int *rint;
    SEXP raster, image, idim;
    
    args = CDR(args);

    int native = asLogical(CAR(args));
    if (native != TRUE) native = FALSE;

    raster = GECap(gdd);
    if (isNull(raster)) /* NULL = unsupported */
	return raster;

    PROTECT(raster);
    if (native) {
	setAttrib(raster, R_ClassSymbol, mkString("nativeRaster"));
	UNPROTECT(1);
	return raster;
    }

    /* non-native, covert to color strings (this is based on grid.cap) */
    size = LENGTH(raster);
    nrow = INTEGER(getAttrib(raster, R_DimSymbol))[0];
    ncol = INTEGER(getAttrib(raster, R_DimSymbol))[1];
        
    PROTECT(image = allocVector(STRSXP, size));
    rint = INTEGER(raster);
    for (i = 0; i < size; i++) {
	col = i % ncol + 1;
	row = i / ncol + 1;
	SET_STRING_ELT(image, (col - 1) * nrow + row - 1, 
		       mkChar(col2name(rint[i])));
    }
        
    PROTECT(idim = allocVector(INTSXP, 2));
    INTEGER(idim)[0] = nrow;
    INTEGER(idim)[1] = ncol;
    setAttrib(image, R_DimSymbol, idim);
    UNPROTECT(3);

    return image;    
}
