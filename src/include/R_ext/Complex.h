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

#ifndef R_COMPLEX_H
#define R_COMPLEX_H

#ifdef  __cplusplus
extern "C" {
#endif

# ifdef R_LEGACY_RCOMPLEX

/* This definition does not work with optimizing compilers which take
advantage of strict aliasing rules.  It is not safe to use with Fortran
COMPLEX*16 (PR#18430) or in arguments to library calls expecting C99
_Complex double.  This definition should not be used, but if it were still
necessary, one should at least disable LTO.
*/

typedef struct {
 	double r;
 	double i;
 } Rcomplex;

# else

/* This definition uses an anonymous structure, which is defined in C11 (but
not C99).  It is, however, supported at least by GCC, clang and icc.  The
private_data_c member should never be used in code, but tells the compiler
about type punning when accessing the .r and .i elements, so is safer to use
when interfacing with Fortran COMPLEX*16 or directly C99 _Complex double
(PR#18430).

This form of static initialization works with both definitions:
Rcomplex z = { .r = 1, .i = 2 };

Anonymous structures and C99 _Complex have not been incorporated into C++
standard.  While they are usually supported as compiler extensions, warnings
are typically issued (-pedantic) by a C++ compiler.
*/

#ifdef __cplusplus
// Look for clang first as it defines __GNUC__ and reacts to #pragma GCC
# if defined(__clang__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wgnu-anonymous-struct"
#  pragma clang diagnostic ignored "-Wc99-extensions"
# elif defined(__GNUC__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wpedantic"
# endif
#endif

typedef union {
    struct {
	double r;
	double i;
    };
    double _Complex private_data_c;
} Rcomplex;

#ifdef __cplusplus
# if defined(__clang__)
#  pragma clang diagnostic pop
# elif defined(__GNUC__)
#  pragma GCC diagnostic pop
# endif
#endif

# endif 

#ifdef  __cplusplus
}
#endif

#endif /* R_COMPLEX_H */
