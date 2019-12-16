/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003-2019 The R Core Team
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

Rboolean isClipPath(SEXP clip) {
    return Rf_inherits(clip, "GridClipPath");
}

SEXP setClipPath(SEXP path, pGEDevDesc dd) 
{
    pDevDesc dev = dd->dev;
    SEXP resolveFn, R_fcall, result;
    SEXP pathFun = PROTECT(VECTOR_ELT(path, 0));
    SEXP index = PROTECT(allocVector(INTSXP, 1));
    INTEGER(index)[0] = dev->setClipPath(pathFun, dev);
    PROTECT(resolveFn = findFun(install("resolvedClipPath"), R_gridEvalEnv));
    PROTECT(R_fcall = lang3(resolveFn, path, index));
    result = eval(R_fcall, R_gridEvalEnv);
    UNPROTECT(4);
    return result;    
}

