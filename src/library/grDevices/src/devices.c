/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
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
    SEXP ans;
    int i = 0;
    pDevDesc dd = GEcurrentDevice()->dev;

    args = CDR(args);

    PROTECT(ans = allocVector(INTSXP, 9));
    INTEGER(ans)[i] = dd->haveTransparency;
    INTEGER(ans)[++i] = dd->haveTransparentBg;
    /* These will be NULL if the device does not define them */
    INTEGER(ans)[++i] = (dd->raster != NULL) ? dd->haveRaster : 1;
    INTEGER(ans)[++i] = (dd->cap != NULL) ? dd->haveCapture : 1;
    INTEGER(ans)[++i] = (dd->locator != NULL) ? dd->haveLocator : 1;
    INTEGER(ans)[++i] = (int)(dd->canGenMouseDown);
    INTEGER(ans)[++i] = (int)(dd->canGenMouseMove);
    INTEGER(ans)[++i] = (int)(dd->canGenMouseUp);
    INTEGER(ans)[++i] = (int)(dd->canGenKeybd);
    /* FIXME:  there should be a way for a device to declare its own
               events, and return information on how to set them */

    UNPROTECT(1);
    return ans;
}

SEXP devcapture(SEXP args)
{
    int i, col, row, nrow, ncol, size;
    Rboolean native;
    pGEDevDesc gdd = GEcurrentDevice();
    int *rint;
    SEXP raster, image, idim;
    
    args = CDR(args);

    native = asLogical(CAR(args));
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
