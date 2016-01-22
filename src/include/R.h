/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2016 The R Core Team.
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

#ifndef R_R_H
#define R_R_H

#ifndef USING_R
# define USING_R
#endif

#ifndef NO_C_HEADERS
#include <stdlib.h> /* Not used by R itself, but widely assumed in packages */
#include <stdio.h>  /* Used by ca 200 packages, but not in R itself */
#include <limits.h> /* for INT_MAX */
#include <math.h>
/* 
   math.h   is also included by R_ext/Arith.h, except in C++ code
   stddef.h is included by R_ext/Memory.h
   string.h is included by R_ext/RS.h
   All guarded by NO_C_HEADERS.
*/
# if defined(__sun)
/* Solaris' stdlib.h includes a header which defines these (and more) */
# undef DO
# undef DS
# undef SO
# undef SS
# endif
#endif

#include <Rconfig.h>
#include <R_ext/Arith.h>      /* R_FINITE, ISNAN, ... */
#include <R_ext/Boolean.h>    /* Rboolean type */
#include <R_ext/Complex.h>    /* Rcomplex type */
#include <R_ext/Constants.h>  /* PI, DOUBLE_EPS, etc */
#include <R_ext/Error.h>      /* error and warning */
#include <R_ext/Memory.h>     /* R_alloc and S_alloc */
#include <R_ext/Print.h>      /* Rprintf etc */
#include <R_ext/Random.h>     /* RNG interface */
#include <R_ext/Utils.h>      /* sort routines et al */
#include <R_ext/RS.h>
/* for PROBLEM ... Calloc, Realloc, Free, Memcpy, F77_xxxx */


typedef double Sfloat;
typedef int Sint;
#define SINT_MAX INT_MAX
#define SINT_MIN INT_MIN

#ifdef __cplusplus
extern "C" {
#endif

void R_FlushConsole(void);
/* always declared, but only usable under Win32 and Aqua */
void R_ProcessEvents(void);
#ifdef Win32
void R_WaitEvent(void);
#endif

#ifdef __cplusplus
}
#endif

#endif /* !R_R_H */
