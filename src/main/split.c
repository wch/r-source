/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2006-2016 The R Core Team
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

#include "Defn.h"
#include <Internal.h>
#include <R_ext/Itermacros.h>

SEXP attribute_hidden do_split(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, f, counts, vec, nm, nmj;
    Rboolean have_names;

    checkArity(op, args);

    x = CAR(args);
    f = CADR(args);
    if (!isVector(x))
	error(_("first argument must be a vector"));
    if (!isFactor(f))
	error(_("second argument must be a factor"));
    int nlevs = nlevels(f);
    R_xlen_t nfac = XLENGTH(CADR(args));
    R_xlen_t nobs = XLENGTH(CAR(args));
    if (nfac <= 0 && nobs > 0)
	error(_("group length is 0 but data length > 0"));
    if (nfac > 0 && (nobs % nfac) != 0)
	warning(_("data length is not a multiple of split variable"));
    nm = getAttrib(x, R_NamesSymbol);
    have_names = nm != R_NilValue;

#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(x))
# define _L_INTSXP_ REALSXP
# define _L_INTEG_  REAL
# define _L_int_    R_xlen_t
# include "split-incl.c"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_
    else
#endif

# define _L_INTSXP_ INTSXP
# define _L_INTEG_  INTEGER
# define _L_int_    int
# include "split-incl.c"

# undef _L_INTSXP_
# undef _L_INTEG_
# undef _L_int_

    setAttrib(vec, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
    UNPROTECT(2);
    return vec;
}
