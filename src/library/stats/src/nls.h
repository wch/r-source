/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2007   The R Core Team.
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

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

SEXP nls_iter(SEXP m, SEXP control, SEXP doTraceArg);
SEXP numeric_deriv(SEXP expr, SEXP theta, SEXP rho, SEXP dir);


