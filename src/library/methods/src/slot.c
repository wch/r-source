/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2013   The R Core Team.
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
#include "methods.h"

SEXP R_get_slot(SEXP obj, SEXP name)
{
    return R_do_slot(obj, name);
}

SEXP R_set_slot(SEXP obj, SEXP name, SEXP value)
{
    return R_do_slot_assign(obj, name, value);
}

SEXP R_hasSlot(SEXP obj, SEXP name)
{
    return ScalarLogical(R_has_slot(obj, name));
}



