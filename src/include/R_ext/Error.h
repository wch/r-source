/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2025   The R Core Team
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

#include <R_ext/Print.h> // for R_PRINTF_FORMAT

#ifdef  __cplusplus
extern "C" {
#endif

/*
 * As this is sometimes an attribute, it should precede 'static' in a
 * function declaration.
 * gcc 15 requires it to precede 'attribute_hidden'.
 * OTOH, '_Noreturn' is an obsolescent (in C23) function specifier.
 */
#if defined NORET
#elif (defined(__STDC_VERSION__) && __STDC_VERSION__ >= 202301L)
// gcc 15 LLVM clang 19- and Apple clang 17
# define NORET [[noreturn]]
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201102L
# define NORET _Noreturn
#elif defined(__GNUC__) && __GNUC__ >= 3
// All platforms these days should be using C >= 11 but perhaps used for C++
# define NORET __attribute__((noreturn))
#else
// C++ and legacy
# define NORET
#endif

#ifdef  __cplusplus
// Only supported in C++ >= 11, but that is all current R supports
// Defining NORET caused conflict in many C++-using packages
[[noreturn]] void Rf_error(const char *, ...) R_PRINTF_FORMAT(1, 2);

[[noreturn]] void UNIMPLEMENTED(const char *);
[[noreturn]] void WrongArgCount(const char *);
#else
NORET void Rf_error(const char *, ...) R_PRINTF_FORMAT(1, 2);

NORET void UNIMPLEMENTED(const char *);
NORET void WrongArgCount(const char *);
#endif

void Rf_warning(const char *, ...) R_PRINTF_FORMAT(1,2);

void R_ShowMessage(const char *s);

/* xerbla is a C function intended to be called from Fortran.
 * which forerly had a C declaration here.
 *
 * It wraps Rf_error, so use that directtly from C/C++
*/

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
