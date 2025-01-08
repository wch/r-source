/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2025 The R Core Team.
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

/* Included by R.h: nowadays almost all API */

#ifndef R_RS_H
#define R_RS_H

#if defined(__cplusplus) && !defined(DO_NOT_USE_CXX_HEADERS)
# include <cstring>
# include <cstddef>
# define R_SIZE_T std::size_t
#else
# include <string.h>		/* for memcpy, memset */
# include <stddef.h> /* for size_t */
# define R_SIZE_T size_t
#endif

#include <Rconfig.h>		/* for HAVE_F77_UNDERSCORE */

#ifdef  __cplusplus
extern "C" {
#endif

/* S Like Memory Management */

/* not of themselves API */
extern void *R_chk_calloc(R_SIZE_T, R_SIZE_T);
extern void *R_chk_realloc(void *, R_SIZE_T);
extern void R_chk_free(void *);
extern void *R_chk_memcpy(void *, const void *, R_SIZE_T);
extern void *R_chk_memset(void *, int, R_SIZE_T);

/* API */
#define R_Calloc(n, t)   (t *) R_chk_calloc( (R_SIZE_T) (n), sizeof(t) )
#define R_Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (R_SIZE_T)((n) * sizeof(t)) )
#define R_Free(p)      (R_chk_free( (void *)(p) ), (p) = NULL)

/* Nowadays API: undocumented until 4.1.2: widely used. */
#define Memcpy(p,q,n)  R_chk_memcpy( p, q, (R_SIZE_T)(n) * sizeof(*p) )

/* Nowadays API: added for 3.0.0 but undocumented until 4.1.2. */
#define Memzero(p,n)  R_chk_memset(p, 0, (R_SIZE_T)(n) * sizeof(*p))

/* API: Added in R 2.6.0 */
#define CallocCharBuf(n) (char *) R_chk_calloc(((R_SIZE_T)(n))+1, sizeof(char))

/* S Like Fortran Interface */
/* These may not be adequate everywhere. Convex had _ prepending common
   blocks, and some compilers may need to specify Fortran linkage.

   HP-UX did not add a trailing underscore.  (It still existed in
   2024, but R ports had not been seen for many years.)

   Note that this is an F77 interface, intended only for valid F77
   names of <= 6 ASCII characters (and no underscores) and there is an
   implicit assumption that the Fortran compiler maps names to
   lower-case (and 'x' is lower-case when called).

   The configure code has

   HAVE_F77_EXTRA_UNDERSCORE
   Define if your Fortran compiler appends an extra_underscore to
   external names containing an underscore.

   but that is not used here (and none of gfortran, flang-new nor
   x86_64 ifx do so: earlier Intel x86 compilere might have).  It is
   used in Rdynload.c to support .Fortran.

   These macros have always been the same in R.  Their documented uses are

   F77_SUB to define a function in C to be called from Fortran 
   F77_NAME to declare a Fortran routine in C before use 
   F77_CALL to call a Fortran routine from C
 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_CALL(x)	x ## _
#else
# define F77_CALL(x)	x
#endif
#define F77_NAME(x)    F77_CALL(x)
#define F77_SUB(x)     F77_CALL(x)
/* Last two were historical from S, not used in R, deprecated in 4.4.2, removed in 4.5.0
#define F77_COM(x)     F77_CALL(x)
#define F77_COMDECL(x) F77_CALL(x)
*/
 
/* call_R was deprecated in R 2.15.0, removed in R 4.2.0 */

#ifdef  __cplusplus
}
#endif

#endif /* R_RS_H */
