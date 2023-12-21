/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2023  The R Core Team
 *  Copyright (C) 2003	     The R Foundation
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"

SEXP defineGroup(SEXP args) 
{
    SEXP ref = R_NilValue;
    pGEDevDesc dd = GEcurrentDevice();
    if (dd->dev->deviceVersion >= R_GE_group) {
        if (dd->appending) {
            warning(_("Group definition ignored (device is appending path)"));
        } else {
            SEXP source = CADR(args);
            SEXP op = CADDR(args);
            SEXP destination = CADDDR(args);
            ref = dd->dev->defineGroup(source, INTEGER(op)[0], destination, 
                                       dd->dev);
        }
    }
    return ref;
}

SEXP useGroup(SEXP args) 
{
    pGEDevDesc dd = GEcurrentDevice();
    /* This device operation is actually performing rendering */
    GEMode(1, dd);
    if (dd->dev->deviceVersion >= R_GE_group) {
        if (dd->appending) {
            warning(_("Group use ignored (device is appending path)"));
        } else {
            SEXP ref = CADR(args);
            SEXP trans = CADDR(args);
            dd->dev->useGroup(ref, trans, dd->dev);
        }
    }
    GEMode(0, dd);
    return R_NilValue;
}

SEXP devUp(SEXP args) {
    pDevDesc dd = GEcurrentDevice()->dev;
    double left, right, bottom, top;
    SEXP ans;

    dd->size(&left, &right, &bottom, &top, dd);
    ans = allocVector(LGLSXP, 1);
    LOGICAL(ans)[0] = top > bottom;
    return ans;
}
