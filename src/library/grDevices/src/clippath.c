/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2014  The R Core Team
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

SEXP setClipPath(SEXP args) 
{
    pGEDevDesc dd = GEcurrentDevice();
    SEXP path = CADR(args);
    int index = INTEGER(CADDR(args))[0];
    index = dd->dev->setClipPath(path, index, dd->dev);
    SEXP result;
    result = PROTECT(allocVector(INTSXP, 1));
    INTEGER(result)[0] = index;
    UNPROTECT(1);
    return result;
}
