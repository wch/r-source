/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2023 The R Core Team.
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

/* This file is only used on macOS.

   It provdes 'shims' from Fortran-style BLAS calls to the interface
   provided by  Apple's Accelerate in macOS >= 13.3.

   Not supplying arguments is crude but works.
*/

#include <complex.h>
#include <R_ext/RS.h> // for F77_NAME
// clang -Wall -pedantic would warn on this
#pragma clang diagnostic ignored "-Wdollar-in-identifier-extension"
// For ILP64 this would be x ## $NEWLAPACK ## $ILP64
#define ACC_NAME(x) x ## $NEWLAPACK

// Fortran functions
// will need changes for ILP64: __LAPACK_int is int or long.
#define Mint(fun) \
int ACC_NAME(fun)(void); \
int F77_NAME(fun)(void) { \
    return ACC_NAME(fun)(); \
}
#define Mdouble(fun) \
double ACC_NAME(fun)(void); \
double F77_NAME(fun)(void) { \
    return ACC_NAME(fun)(); \
}
/*
#define Mcomplex(fun)		    \
double complex ACC_NAME(fun)(void); \
double complex F77_NAME(fun)(void) { \
    return ACC_NAME(fun)(); \
}
*/

// Fortran subroutines
#define M1(fun)\
void ACC_NAME(fun)(void); \
void F77_NAME(fun)(void) { \
    ACC_NAME(fun)(); \
}
M1(dasum)
M1(daxpy)
Mdouble(dcabs1)
M1(dcopy)
M1(ddot)
M1(dgbmv)
M1(dgemm)
M1(dgemv)
M1(dger)
M1(dnrm2)
M1(drot)
M1(drotg)
M1(drotm)
M1(drotmg)
M1(dsbmv)
M1(dscal)
M1(dsdot)
M1(dspmv)
M1(dspr2)
M1(dspr)
M1(dswap)
M1(dsymm)
M1(dsymv)
M1(dsyr2)
M1(dsyr2k)
M1(dsyr)
M1(dsyrk)
M1(dtbmv)
M1(dtbsv)
M1(dtpmv)
M1(dtpsv)
M1(dtrmm)
M1(dtrmv)
M1(dtrsm)
M1(dtrsv)
Mdouble(dzasum)
M1(dznrm2)
Mint(idamax)
Mint(izamax)
M1(zaxpy)
M1(zcopy)
// gfortran and C have different ABIs for double complex return values
//Mcomplex(zdotc)
//Mcomplex(zdotu)
M1(zdrot)
M1(zdscal)
M1(zgbmv)
M1(zgemm)
M1(zgemv)
M1(zgerc)
M1(zgeru)
M1(zhbmv)
M1(zhemm)
M1(zhemv)
M1(zher2)
M1(zher2k)
M1(zher)
M1(zherk)
M1(zhpmv)
M1(zhpr2)
M1(zhpr)
M1(zrotg)
M1(zscal)
M1(zswap)
M1(zsymm)
M1(zsyr2k)
M1(zsyrk)
M1(ztbmv)
M1(ztbsv)
M1(ztpmv)
M1(ztpsv)
M1(ztrmm)
M1(ztrmv)
M1(ztrsm)
M1(ztrsv)
