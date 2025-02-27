/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-2025   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"

SEXP R_CreateAtVector(SEXP axp, SEXP usr, SEXP nint, SEXP is_log)
{
    int nint_v = asInteger(nint);
    Rboolean logflag = asRboolean(is_log);

    PROTECT(axp = coerceVector(axp, REALSXP));
    PROTECT(usr = coerceVector(usr, REALSXP));
    if(LENGTH(axp) != 3) error(_("'%s' must be numeric of length %d"), "axp", 3);
    if(LENGTH(usr) != 2) error(_("'%s' must be numeric of length %d"), "usr", 2);

    SEXP res = CreateAtVector(REAL(axp), REAL(usr), nint_v, logflag);
    // -> ../../../main/plot.c
    UNPROTECT(2);
    return res;
}

// R's  .axisPars(usr, log, nintLog)
SEXP R_GAxisPars(SEXP usr, SEXP is_log, SEXP nintLog)
{
    usr = coerceVector(usr, REALSXP);
    if(LENGTH(usr) != 2) error(_("'%s' must be numeric of length %d"), "usr", 2);
    double
	min = REAL(usr)[0],
	max = REAL(usr)[1];
    Rboolean logflag = asRboolean(is_log);
    int n = asInteger(nintLog);// will be changed on output ..

    GAxisPars(&min, &max, &n, logflag, 0);// axis = 0 :<==> do not warn
    // -> ../../../main/graphics.c

    const char *nms[] = {"axp", "n", ""};
    SEXP axp, ans = PROTECT(mkNamed(VECSXP, nms));
    SET_VECTOR_ELT(ans, 0, (axp = allocVector(REALSXP, 2)));// protected
    SET_VECTOR_ELT(ans, 1, ScalarInteger(n));
    REAL(axp)[0] = min;
    REAL(axp)[1] = max;

    UNPROTECT(1);
    return ans;
}
