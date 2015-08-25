/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2007   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
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


#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h> /* for DL_FUNC */

/* from Rdynpriv.h, but that pulls in too much */
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);

#include "methods.h"

SEXP NORET R_dummy_extern_place()
{
    error(_("calling the C routine used as an initializer for 'externalptr' objects"));
}

SEXP R_externalptr_prototype_object()
{
    return R_MakeExternalPtrFn((DL_FUNC) R_dummy_extern_place, R_NilValue,
			       R_NilValue);
}

