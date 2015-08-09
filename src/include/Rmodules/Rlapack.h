/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2010 The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Definitions for the Lapack module.  Not intended for end-user use */

#ifndef R_LAPACK_MODULE_H
#define R_LAPACK_MODULE_H

#include <Rinternals.h>

typedef SEXP (*Rf_do_lapack)(SEXP, SEXP, SEXP, SEXP);

typedef struct {
    Rf_do_lapack do_lapack;
} R_LapackRoutines;

R_LapackRoutines *R_setLapackRoutines(R_LapackRoutines *routines);


#endif /* R_LAPACK_MODULE_H */
