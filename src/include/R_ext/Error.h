/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2024   The R Core Team
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
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

/* Included by R.h: Part of the API. */

#ifndef R_ERROR_H_
#define R_ERROR_H_

#if defined(__cplusplus) && !defined(DO_NOT_USE_CXX_HEADERS)
# include <cstddef>
#else
# include <stddef.h> /* for size_t */
#endif

#include <R_ext/Print.h>
#include <Rconfig.h>            /* for HAVE_F77_UNDERSCORE */

#ifdef  __cplusplus
extern "C" {
#endif

/* C23 has a [[noreturn]] attribute supported in GCC 13 and LLVM clang
 * 15 with -std=c2x but not Apple clang 14.  All have version 202000L.
 * In C11 there is _Noreturn * (or noreturn in header <stdnoreturn.h>).
 */
#if defined NORET
#elif (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202301L)
# define NORET [[noreturn]]
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201102L
# define NORET _Noreturn
#elif defined(__GNUC__) && __GNUC__ >= 3
// LLVM and Apple clang identify themselves as 4.
// But Mandriva (or OpenMandriva) is said to patch clang to 11.
// Boost also uses this for __SUNPRO_CC >= 0x590
# define NORET __attribute__((noreturn))
#else
# define NORET
#endif

NORET void Rf_error(const char *, ...) R_PRINTF_FORMAT(1, 2);

NORET void UNIMPLEMENTED(const char *);
NORET void WrongArgCount(const char *);

void Rf_warning(const char *, ...) R_PRINTF_FORMAT(1,2);

void R_ShowMessage(const char *s);

#if 0
/* xerbla is a a C function intended to be called from Fortran.
 * It wraps Rf_error, so use that directtly from C/C++
*/
#ifdef HAVE_F77_UNDERSCORE
/* F77_NAME is in RS.h, but better not include it here (e.g. due to
 * name conflicts involving symbols defined with !STRICT_R_HEADERS) .
 * However, using a trailing underline is not universal, and print.c
 * uses F77_SUB.
 */
# ifdef FC_LEN_T
NORET void xerbla_(const char *srname, int *info, const FC_LEN_T srname_len);
# else
NORET void xerbla_(const char *srname, int *info);
# endif
#else
# ifdef FC_LEN_T
NORET void xerbla(const char *srname, int *info, const FC_LEN_T srname_len);
# else
NORET void xerbla(const char *srname, int *info);
# endif
#endif

#endif

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
