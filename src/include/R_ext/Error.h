/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2023   The R Core Team
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

/* Included by R.h: API */

#ifndef R_ERROR_H_
#define R_ERROR_H_

#include <R_ext/Print.h>

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

void	Rf_warning(const char *, ...) R_PRINTF_FORMAT(1,2);

void 	R_ShowMessage(const char *s);
    

#ifdef  __cplusplus
}
#endif

#ifndef R_NO_REMAP
#define error Rf_error
#define warning Rf_warning
#endif


#endif /* R_ERROR_H_ */
