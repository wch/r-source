/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2023 The R Core Team.
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

/*
   C declarations of BLAS Fortran subroutines always available in R.

   Part of the API.

   R packages that use these should have PKG_LIBS in src/Makevars include
   $(BLAS_LIBS) $(FLIBS)
 */

/* Part of the API */

#ifndef R_BLAS_H
#define R_BLAS_H

#include <R_ext/RS.h>		/* for F77_... */
#include <R_ext/Complex.h>	/* for Rcomplex */

#ifdef FC_LEN_T
# include <stddef.h> // for size_t if needed
# define FCLEN ,FC_LEN_T
# define FCONE ,(FC_LEN_T)1
#else
# define FCLEN
# define FCONE
#endif

// allow future 64-bit nrow, ncol and increments. by using int64_t
// configure checked that Fortran INTEGER is 32-bit and the same as C int.
// Only LSAME (not included here) has LOGICAL in its declaration.
// It is unclear what i[dz]amaz should return: the reference code has INTEGER, but CBLAS has size_t.
#ifndef ILP64
# define BLAS_INT int
#endif

#ifdef  __cplusplus
extern "C" {
#endif

// never defined in R itself.
#ifndef BLAS_extern
#define BLAS_extern extern
#endif

/* Double Precision Level 1 BLAS */

BLAS_extern double /* DASUM - sum of absolute values of a one-dimensional array */
F77_NAME(dasum)(const BLAS_INT *n, const double *dx, const BLAS_INT *incx);
BLAS_extern void   /* DAXPY - replace y by da*x + y */
F77_NAME(daxpy)(const BLAS_INT *n, const double *da,
		const double *dx, const BLAS_INT *incx,
		double *dy, const BLAS_INT *incy);
BLAS_extern void   /* DCOPY - copy x to y */
F77_NAME(dcopy)(const BLAS_INT *n, const double *dx, const BLAS_INT *incx,
		double *dy, const BLAS_INT *incy);
BLAS_extern double /* DDOT - inner product of x and y */
F77_NAME(ddot)(const BLAS_INT *n, const double *dx, const BLAS_INT *incx,
	       const double *dy, const BLAS_INT *incy);
BLAS_extern double /* DNRM2 - 2-norm of a vector */
F77_NAME(dnrm2)(const BLAS_INT *n, const double *dx, const BLAS_INT *incx);
BLAS_extern void   /* DROT - apply a Givens rotation */
F77_NAME(drot)(const BLAS_INT *n, double *dx, const BLAS_INT *incx,
	       double *dy, const BLAS_INT *incy, const double *c, const double *s);
BLAS_extern void   /* DROTG - generate a Givens rotation */
F77_NAME(drotg)(const double *a, const double *b, double *c, double *s);
BLAS_extern void   /* DROTM - apply a modified Givens rotation */
F77_NAME(drotm)(const BLAS_INT *n, double *dx, const BLAS_INT *incx,
		double *dy, const BLAS_INT *incy, const double *dparam);
BLAS_extern void   /* DROTMG - generate a modified Givens rotation */
F77_NAME(drotmg)(const double *dd1, const double *dd2, const double *dx1,
		 const double *dy1, double *param);
BLAS_extern void   /* DSCAL - scale a one-dimensional array */
F77_NAME(dscal)(const BLAS_INT *n, const double *alpha, double *dx, const BLAS_INT *incx);
BLAS_extern void   /* DSWAP - interchange one-dimensional arrays */
F77_NAME(dswap)(const BLAS_INT *n, double *dx, const BLAS_INT *incx,
		double *dy, const BLAS_INT *incy);
BLAS_extern BLAS_INT    /* IDAMAX - return the index of the element with max abs value */
F77_NAME(idamax)(const BLAS_INT *n, const double *dx, const BLAS_INT *incx);

/* Double Precision Level 2 BLAS */

/* DGBMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y, */
BLAS_extern void
F77_NAME(dgbmv)(const char *trans, const BLAS_INT *m, const BLAS_INT *n,
		const BLAS_INT *kl,const BLAS_INT *ku,
		const double *alpha, const double *a, const BLAS_INT *lda,
		const double *x, const BLAS_INT *incx,
		const double *beta, double *y, const BLAS_INT *incy FCLEN);
/* DGEMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y,  */
BLAS_extern void
F77_NAME(dgemv)(const char *trans, const BLAS_INT *m, const BLAS_INT *n,
		const double *alpha, const double *a, const BLAS_INT *lda,
		const double *x, const BLAS_INT *incx, const double *beta,
		double *y, const BLAS_INT *incy FCLEN);
/* DSBMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dsbmv)(const char *uplo, const BLAS_INT *n, const BLAS_INT *k,
		const double *alpha, const double *a, const BLAS_INT *lda,
		const double *x, const BLAS_INT *incx,
		const double *beta, double *y, const BLAS_INT *incy FCLEN);
/* DSPMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dspmv)(const char *uplo, const BLAS_INT *n,
		const double *alpha, const double *ap,
		const double *x, const BLAS_INT *incx,
		const double *beta, double *y, const BLAS_INT *incy FCLEN);

/* DSYMV - perform the matrix-vector operation */
/*  y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dsymv)(const char *uplo, const BLAS_INT *n, const double *alpha,
		const double *a, const BLAS_INT *lda,
		const double *x, const BLAS_INT *incx,
		const double *beta, double *y, const BLAS_INT *incy FCLEN);
/* DTBMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtbmv)(const char *uplo, const char *trans,
		const char *diag, const BLAS_INT *n, const BLAS_INT *k,
		const double *a, const BLAS_INT *lda,
		double *x, const BLAS_INT *incx FCLEN FCLEN FCLEN);
/* DTPMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtpmv)(const char *uplo, const char *trans, const char *diag,
		const BLAS_INT *n, const double *ap,
		double *x, const BLAS_INT *incx FCLEN FCLEN FCLEN);
/* DTRMV - perform one of the matrix-vector operations  */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtrmv)(const char *uplo, const char *trans, const char *diag,
		const BLAS_INT *n, const double *a, const BLAS_INT *lda,
		double *x, const BLAS_INT *incx FCLEN FCLEN FCLEN);
/* DTBSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtbsv)(const char *uplo, const char *trans,
		const char *diag, const BLAS_INT *n, const BLAS_INT *k,
		const double *a, const BLAS_INT *lda,
		double *x, const BLAS_INT *incx FCLEN FCLEN FCLEN);
/* DTPSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtpsv)(const char *uplo, const char *trans,
		const char *diag, const BLAS_INT *n,
		const double *ap, double *x, const BLAS_INT *incx
		FCLEN FCLEN FCLEN);
/* DTRSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtrsv)(const char *uplo, const char *trans,
		const char *diag, const BLAS_INT *n,
		const double *a, const BLAS_INT *lda,
		double *x, const BLAS_INT *incx FCLEN FCLEN FCLEN);
/* DGER - perform the rank 1 operation   A := alpha*x*y' + A */
BLAS_extern void
F77_NAME(dger)(const BLAS_INT *m, const BLAS_INT *n, const double *alpha,
	       const double *x, const BLAS_INT *incx,
	       const double *y, const BLAS_INT *incy,
	       double *a, const BLAS_INT *lda);
/* DSYR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
BLAS_extern void
F77_NAME(dsyr)(const char *uplo, const BLAS_INT *n, const double *alpha,
	       const double *x, const BLAS_INT *incx,
	       double *a, const BLAS_INT *lda FCLEN);
/* DSPR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
BLAS_extern void
F77_NAME(dspr)(const char *uplo, const BLAS_INT *n, const double *alpha,
	       const double *x, const BLAS_INT *incx, double *ap FCLEN);
/* DSYR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A, */
BLAS_extern void
F77_NAME(dsyr2)(const char *uplo, const BLAS_INT *n, const double *alpha,
		const double *x, const BLAS_INT *incx,
		const double *y, const BLAS_INT *incy,
		double *a, const BLAS_INT *lda FCLEN);
/* DSPR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A,  */
BLAS_extern void
F77_NAME(dspr2)(const char *uplo, const BLAS_INT *n, const double *alpha,
		const double *x, const BLAS_INT *incx,
		const double *y, const BLAS_INT *incy, double *ap FCLEN);

/* Double Precision Level 3 BLAS */

/* DGEMM - perform one of the matrix-matrix operations    */
/* C := alpha*op( A )*op( B ) + beta*C */
BLAS_extern void
F77_NAME(dgemm)(const char *transa, const char *transb, const BLAS_INT *m,
		const BLAS_INT *n, const BLAS_INT *k, const double *alpha,
		const double *a, const BLAS_INT *lda,
		const double *b, const BLAS_INT *ldb,
		const double *beta, double *c, const BLAS_INT *ldc 
		FCLEN FCLEN);
/* DTRSM - solve one of the matrix equations  */
/* op(A)*X = alpha*B, or  X*op(A) = alpha*B  */
BLAS_extern void
F77_NAME(dtrsm)(const char *side, const char *uplo,
		const char *transa, const char *diag,
		const BLAS_INT *m, const BLAS_INT *n, const double *alpha,
		const double *a, const BLAS_INT *lda,
		double *b, const BLAS_INT *ldb
		FCLEN FCLEN FCLEN FCLEN);
/* DTRMM - perform one of the matrix-matrix operations */
/* B := alpha*op( A )*B, or B := alpha*B*op( A ) */
BLAS_extern void
F77_NAME(dtrmm)(const char *side, const char *uplo, const char *transa,
		const char *diag, const BLAS_INT *m, const BLAS_INT *n,
		const double *alpha, const double *a, const BLAS_INT *lda,
		double *b, const BLAS_INT *ldb
		FCLEN FCLEN FCLEN FCLEN);
/* DSYMM - perform one of the matrix-matrix operations   */
/*  C := alpha*A*B + beta*C, */
BLAS_extern void
F77_NAME(dsymm)(const char *side, const char *uplo, const BLAS_INT *m,
		const BLAS_INT *n, const double *alpha,
		const double *a, const BLAS_INT *lda,
		const double *b, const BLAS_INT *ldb,
		const double *beta, double *c, const BLAS_INT *ldc
		FCLEN FCLEN);
/* DSYRK - perform one of the symmetric rank k operations */
/* C := alpha*A*A' + beta*C or C := alpha*A'*A + beta*C */
BLAS_extern void
F77_NAME(dsyrk)(const char *uplo, const char *trans,
		const BLAS_INT *n, const BLAS_INT *k,
		const double *alpha, const double *a, const BLAS_INT *lda,
		const double *beta, double *c, const BLAS_INT *ldc
		FCLEN FCLEN);
/* DSYR2K - perform one of the symmetric rank 2k operations */
/* C := alpha*A*B' + alpha*B*A' + beta*C or */
/* C := alpha*A'*B + alpha*B'*A + beta*C */
BLAS_extern void
F77_NAME(dsyr2k)(const char *uplo, const char *trans,
		 const BLAS_INT *n, const BLAS_INT *k,
		 const double *alpha, const double *a, const BLAS_INT *lda,
		 const double *b, const BLAS_INT *ldb,
		 const double *beta, double *c, const BLAS_INT *ldc
		 FCLEN FCLEN);
/*
  LSAME is a LAPACK support routine, not part of BLAS
*/

/* Double complex BLAS routines added for 2.3.0 */
/* #ifdef HAVE_FORTRAN_DOUBLE_COMPLEX */
    BLAS_extern double
    F77_NAME(dcabs1)(const Rcomplex *z);
    BLAS_extern double
    F77_NAME(dzasum)(const BLAS_INT *n, const Rcomplex *zx, const BLAS_INT *incx);
    BLAS_extern double
    F77_NAME(dznrm2)(const BLAS_INT *n, const Rcomplex *x, const BLAS_INT *incx);
    BLAS_extern BLAS_INT
    F77_NAME(izamax)(const BLAS_INT *n, const Rcomplex *zx, const BLAS_INT *incx);
    BLAS_extern void
    F77_NAME(zaxpy)(const BLAS_INT *n, const Rcomplex *za, const Rcomplex *zx,
		    const BLAS_INT *incx, const Rcomplex *zy, const BLAS_INT *incy);
    BLAS_extern void
    F77_NAME(zcopy)(const BLAS_INT *n, const Rcomplex *zx, const BLAS_INT *incx,
		    const Rcomplex *zy, const BLAS_INT *incy);

    /* WARNING!  The next two return a value that may not be
       compatible between C and Fortran, and even if it is, this might
       not be the right translation to C.  Only use after
       configure-testing with your compilers.
     */
    BLAS_extern Rcomplex
    F77_NAME(zdotc)(const BLAS_INT *n,
		    const Rcomplex *zx, const BLAS_INT *incx, 
		    const Rcomplex *zy, const BLAS_INT *incy);
    BLAS_extern Rcomplex
    F77_NAME(zdotu)(const BLAS_INT *n,
		    const Rcomplex *zx, const BLAS_INT *incx,
		    const Rcomplex *zy, const BLAS_INT *incy);

    BLAS_extern void
    F77_NAME(zdrot)(const BLAS_INT *n, 
		    const Rcomplex *zx, const BLAS_INT *incx, 
		    Rcomplex *zy, const BLAS_INT *incy, 
		    const double *c, const double *s);
    BLAS_extern void
    F77_NAME(zdscal)(const BLAS_INT *n, const double *da, 
		     Rcomplex *zx, const BLAS_INT *incx);
    BLAS_extern void
    F77_NAME(zgbmv)(const char *trans, BLAS_INT *m, BLAS_INT *n, BLAS_INT *kl,
		    BLAS_INT *ku, Rcomplex *alpha, Rcomplex *a, BLAS_INT *lda,
		    Rcomplex *x, BLAS_INT *incx, Rcomplex *beta, Rcomplex *y,
		    BLAS_INT *incy FCLEN);
    BLAS_extern void
    F77_NAME(zgemm)(const char *transa, const char *transb, const BLAS_INT *m,
		    const BLAS_INT *n, const BLAS_INT *k, const Rcomplex *alpha,
		    const Rcomplex *a, const BLAS_INT *lda,
		    const Rcomplex *b, const BLAS_INT *ldb,
		    const Rcomplex *beta, Rcomplex *c, const BLAS_INT *ldc
		    FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zgemv)(const char *trans, const BLAS_INT *m, const BLAS_INT *n,
		    const Rcomplex *alpha, const Rcomplex *a, const BLAS_INT *lda,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *beta,
		    Rcomplex *y, const BLAS_INT *incy FCLEN);
    BLAS_extern void
    F77_NAME(zgerc)(const BLAS_INT *m, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *y,
		    const BLAS_INT *incy, Rcomplex *a, const BLAS_INT *lda);
    BLAS_extern void
    F77_NAME(zgeru)(const BLAS_INT *m, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *y,
		    const BLAS_INT *incy, Rcomplex *a, const BLAS_INT *lda);
    BLAS_extern void
    F77_NAME(zhbmv)(const char *uplo, const BLAS_INT *n, const BLAS_INT *k,
		    const Rcomplex *alpha, const Rcomplex *a, const BLAS_INT *lda,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *beta,
		    Rcomplex *y, const BLAS_INT *incy FCLEN);
    BLAS_extern void
    F77_NAME(zhemm)(const char *side, const char *uplo, const BLAS_INT *m,
		    const BLAS_INT *n, const Rcomplex *alpha, const Rcomplex *a,
		    const BLAS_INT *lda, const Rcomplex *b, const BLAS_INT *ldb,
		    const Rcomplex *beta, Rcomplex *c, const BLAS_INT *ldc
		    FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zhemv)(const char *uplo, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *a, const BLAS_INT *lda, const Rcomplex *x,
		    const BLAS_INT *incx, const Rcomplex *beta,
		    Rcomplex *y, const BLAS_INT *incy FCLEN);
    BLAS_extern void
    F77_NAME(zher)(const char *uplo, const BLAS_INT *n, const double *alpha,
		   const Rcomplex *x, const BLAS_INT *incx, Rcomplex *a,
		   const BLAS_INT *lda FCLEN);
    BLAS_extern void
    F77_NAME(zher2)(const char *uplo, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *y,
		    const BLAS_INT *incy, Rcomplex *a, const BLAS_INT *lda FCLEN);
    BLAS_extern void
    F77_NAME(zher2k)(const char *uplo, const char *trans, const BLAS_INT *n,
		     const BLAS_INT *k, const Rcomplex *alpha, const Rcomplex *a,
		     const BLAS_INT *lda, const Rcomplex *b, const  BLAS_INT *ldb,
		     const double *beta, Rcomplex *c, const BLAS_INT *ldc
		     FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zherk)(const char *uplo, const char *trans, const BLAS_INT *n,
		    const BLAS_INT *k, const double *alpha, const Rcomplex *a,
		    const BLAS_INT *lda, const double *beta, Rcomplex *c,
		    const BLAS_INT *ldc FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zhpmv)(const char *uplo, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *ap, const Rcomplex *x, const BLAS_INT *incx,
		    const Rcomplex * beta, Rcomplex *y, const BLAS_INT *incy
		    FCLEN);
    BLAS_extern void
    F77_NAME(zhpr)(const char *uplo, const BLAS_INT *n, const double *alpha,
		   const Rcomplex *x, const BLAS_INT *incx, Rcomplex *ap
		   FCLEN);
    BLAS_extern void
    F77_NAME(zhpr2)(const char *uplo, const BLAS_INT *n, const Rcomplex *alpha,
		    const Rcomplex *x, const BLAS_INT *incx, const Rcomplex *y,
		    const BLAS_INT *incy, Rcomplex *ap FCLEN);
    BLAS_extern void
    F77_NAME(zrotg)(const Rcomplex *ca, const Rcomplex *cb, 
		    double *c, Rcomplex *s);
    BLAS_extern void
    F77_NAME(zscal)(const BLAS_INT *n, const Rcomplex *za, Rcomplex *zx,
		    const BLAS_INT *incx);
    BLAS_extern void
    F77_NAME(zswap)(const BLAS_INT *n, Rcomplex *zx, const BLAS_INT *incx,
		    Rcomplex *zy, const BLAS_INT *incy);
    BLAS_extern void
    F77_NAME(zsymm)(const char *side, const char *uplo, const BLAS_INT *m,
		    const BLAS_INT *n, const Rcomplex *alpha, const Rcomplex *a,
		    const BLAS_INT *lda, const Rcomplex *b, const BLAS_INT *ldb,
		    const Rcomplex *beta, Rcomplex *c, const BLAS_INT *ldc
		    FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zsyr2k)(const char *uplo, const char *trans, BLAS_INT *n, BLAS_INT *k,
		     Rcomplex *alpha, Rcomplex *a, BLAS_INT *lda, Rcomplex *b,
		     BLAS_INT *ldb, Rcomplex *beta, Rcomplex *c, BLAS_INT *ldc
		     FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(zsyrk)(const char *uplo, const char *trans, const  BLAS_INT *n,
		    const BLAS_INT *k, const Rcomplex *alpha, const Rcomplex *a,
		    const BLAS_INT *lda, const Rcomplex *beta, Rcomplex *c,
		    const BLAS_INT *ldc FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztbmv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const BLAS_INT *k, const Rcomplex *a,
		    const BLAS_INT *lda, Rcomplex *x, const BLAS_INT *incx
		    FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztbsv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const BLAS_INT *k, const Rcomplex *a,
		    const BLAS_INT *lda, Rcomplex *x, const BLAS_INT *incx
		    FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztpmv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const Rcomplex *ap, Rcomplex *x,
		    const BLAS_INT *incx FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztpsv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const Rcomplex *ap, Rcomplex *x,
		    const BLAS_INT *incx FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztrmm)(const char *side, const char *uplo, const char *transa,
		    const char *diag, const BLAS_INT *m, const BLAS_INT *n,
		    const Rcomplex *alpha, const Rcomplex *a,
		    const BLAS_INT *lda, Rcomplex *b, const BLAS_INT *ldb
		    FCLEN FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztrmv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const Rcomplex *a, const BLAS_INT *lda,
		    Rcomplex *x, const BLAS_INT *incx
		    FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztrsm)(const char *side, const char *uplo, const char *transa,
		    const char *diag, BLAS_INT *m, BLAS_INT *n, Rcomplex *alpha,
		    Rcomplex *a, BLAS_INT *lda, Rcomplex *b, BLAS_INT *ldb
		    FCLEN FCLEN FCLEN FCLEN);
    BLAS_extern void
    F77_NAME(ztrsv)(const char *uplo, const char *trans, const char *diag,
		    const BLAS_INT *n, const Rcomplex *a, const BLAS_INT *lda,
		    Rcomplex *x, const BLAS_INT *incx
		    FCLEN FCLEN FCLEN);
/* #endif */

#ifdef  __cplusplus
}
#endif

#endif /* R_BLAS_H */
