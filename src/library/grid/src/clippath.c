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

SEXP resolveClipPath(SEXP path, pGEDevDesc dd) 
{
    SEXP resolveFn, R_fcall, result;
    setGridStateElement(dd, GSS_RESOLVINGCLIP, ScalarLogical(TRUE));
    PROTECT(resolveFn = findFun(install("resolveClipPath"), R_gridEvalEnv));
    PROTECT(R_fcall = lang2(resolveFn, path));
    result = eval(R_fcall, R_gridEvalEnv);
    setGridStateElement(dd, GSS_RESOLVINGCLIP, ScalarLogical(FALSE));
    UNPROTECT(2);
    return result;    
}

