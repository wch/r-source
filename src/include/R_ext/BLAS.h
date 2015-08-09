/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-12 The R Core Team.
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

#ifdef  __cplusplus
extern "C" {
#endif

// never defined in R itself.
#ifndef BLAS_extern
#define BLAS_extern extern
#endif

/* Double Precision Level 1 BLAS */

BLAS_extern double /* DASUM - sum of absolute values of a one-dimensional array */
F77_NAME(dasum)(const int *n, const double *dx, const int *incx);
BLAS_extern void   /* DAXPY - replace y by alpha*x + y */
F77_NAME(daxpy)(const int *n, const double *alpha,
		const double *dx, const int *incx,
		double *dy, const int *incy);
BLAS_extern void   /* DCOPY - copy x to y */
F77_NAME(dcopy)(const int *n, const double *dx, const int *incx,
		double *dy, const int *incy);
BLAS_extern double /* DDOT - inner product of x and y */
F77_NAME(ddot)(const int *n, const double *dx, const int *incx,
	       const double *dy, const int *incy);
BLAS_extern double /* DNRM2 - 2-norm of a vector */
F77_NAME(dnrm2)(const int *n, const double *dx, const int *incx);
BLAS_extern void   /* DROT - apply a Given's rotation */
F77_NAME(drot)(const int *n, double *dx, const int *incx,
	       double *dy, const int *incy, const double *c, const double *s);
BLAS_extern void   /* DROTG - generate a Given's rotation */
F77_NAME(drotg)(const double *a, const double *b, double *c, double *s);
BLAS_extern void   /* DROTM - apply a modified Given's rotation */
F77_NAME(drotm)(const int *n, double *dx, const int *incx,
		double *dy, const int *incy, const double *dparam);
BLAS_extern void   /* DROTMG - generate a modified Given's rotation */
F77_NAME(drotmg)(const double *dd1, const double *dd2, const double *dx1,
		 const double *dy1, double *param);
BLAS_extern void   /* DSCAL - scale a one-dimensional array */
F77_NAME(dscal)(const int *n, const double *alpha, double *dx, const int *incx);
BLAS_extern void   /* DSWAP - interchange one-dimensional arrays */
F77_NAME(dswap)(const int *n, double *dx, const int *incx,
		double *dy, const int *incy);
BLAS_extern int    /* IDAMAX - return the index of the element with max abs value */
F77_NAME(idamax)(const int *n, const double *dx, const int *incx);

/* Double Precision Level 2 BLAS */

/* DGBMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y, */
BLAS_extern void
F77_NAME(dgbmv)(const char *trans, const int *m, const int *n,
		const int *kl,const int *ku,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DGEMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y,  */
BLAS_extern void
F77_NAME(dgemv)(const char *trans, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx, const double *beta,
		double *y, const int *incy);
/* DSBMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dsbmv)(const char *uplo, const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DSPMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dspmv)(const char *uplo, const int *n,
		const double *alpha, const double *ap,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);

/* DSYMV - perform the matrix-vector operation */
/*  y := alpha*A*x + beta*y, */
BLAS_extern void
F77_NAME(dsymv)(const char *uplo, const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DTBMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtbmv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx);
/* DTPMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtpmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *ap,
		double *x, const int *incx);
/* DTRMV - perform one of the matrix-vector operations  */
/* x := A*x, or x := A'*x, */
BLAS_extern void
F77_NAME(dtrmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *a, const int *lda,
		double *x, const int *incx);
/* DTBSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtbsv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx);
/* DTPSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtpsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *ap, double *x, const int *incx);
/* DTRSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
BLAS_extern void
F77_NAME(dtrsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *a, const int *lda,
		double *x, const int *incx);
/* DGER - perform the rank 1 operation   A := alpha*x*y' + A */
BLAS_extern void
F77_NAME(dger)(const int *m, const int *n, const double *alpha,
	       const double *x, const int *incx,
	       const double *y, const int *incy,
	       double *a, const int *lda);
/* DSYR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
BLAS_extern void
F77_NAME(dsyr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx,
	       double *a, const int *lda);
/* DSPR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
BLAS_extern void
F77_NAME(dspr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx, double *ap);
/* DSYR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A, */
BLAS_extern void
F77_NAME(dsyr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy,
		double *a, const int *lda);
/* DSPR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A,  */
BLAS_extern void
F77_NAME(dspr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy, double *ap);

/* Double Precision Level 3 BLAS */

/* DGEMM - perform one of the matrix-matrix operations    */
/* C := alpha*op( A )*op( B ) + beta*C */
BLAS_extern void
F77_NAME(dgemm)(const char *transa, const char *transb, const int *m,
		const int *n, const int *k, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc);
/* DTRSM - solve one of the matrix equations  */
/* op(A)*X = alpha*B, or  X*op(A) = alpha*B  */
BLAS_extern void
F77_NAME(dtrsm)(const char *side, const char *uplo,
		const char *transa, const char *diag,
		const int *m, const int *n, const double *alpha,
		const double *a, const int *lda,
		double *b, const int *ldb);
/* DTRMM - perform one of the matrix-matrix operations */
/* B := alpha*op( A )*B, or B := alpha*B*op( A ) */
BLAS_extern void
F77_NAME(dtrmm)(const char *side, const char *uplo, const char *transa,
		const char *diag, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		double *b, const int *ldb);
/* DSYMM - perform one of the matrix-matrix operations   */
/*  C := alpha*A*B + beta*C, */
BLAS_extern void
F77_NAME(dsymm)(const char *side, const char *uplo, const int *m,
		const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc);
/* DSYRK - perform one of the symmetric rank k operations */
/* C := alpha*A*A' + beta*C or C := alpha*A'*A + beta*C */
BLAS_extern void
F77_NAME(dsyrk)(const char *uplo, const char *trans,
		const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *beta, double *c, const int *ldc);
/* DSYR2K - perform one of the symmetric rank 2k operations */
/* C := alpha*A*B' + alpha*B*A' + beta*C or */
/* C := alpha*A'*B + alpha*B'*A + beta*C */
BLAS_extern void
F77_NAME(dsyr2k)(const char *uplo, const char *trans,
		 const int *n, const int *k,
		 const double *alpha, const double *a, const int *lda,
		 const double *b, const int *ldb,
		 const double *beta, double *c, const int *ldc);
/*
  LSAME is a LAPACK support routine, not part of BLAS
*/

/* Double complex BLAS routines added for 2.3.0 */
/* #ifdef HAVE_FORTRAN_DOUBLE_COMPLEX */
    BLAS_extern double
    F77_NAME(dcabs1)(double *z);
    BLAS_extern double
    F77_NAME(dzasum)(int *n, Rcomplex *zx, int *incx);
    BLAS_extern double
    F77_NAME(dznrm2)(int *n, Rcomplex *x, int *incx);
    BLAS_extern int
    F77_NAME(izamax)(int *n, Rcomplex *zx, int *incx);
    BLAS_extern void
    F77_NAME(zaxpy)(int *n, Rcomplex *za, Rcomplex *zx,
		    int *incx, Rcomplex *zy, int *incy);
    BLAS_extern void
    F77_NAME(zcopy)(int *n, Rcomplex *zx, int *incx,
		    Rcomplex *zy, int *incy);

    /* WARNING!  The next two return a value that may not be
       compatible between C and Fortran, and even if it is, this might
       not be the right translation to C.  Only use after
       configure-testing with your compilers.
     */
    BLAS_extern Rcomplex
    F77_NAME(zdotc)(int *n,
		    Rcomplex *zx, int *incx, Rcomplex *zy, int *incy);
    BLAS_extern Rcomplex
    F77_NAME(zdotu)(int *n,
		    Rcomplex *zx, int *incx, Rcomplex *zy, int *incy);

    BLAS_extern void
    F77_NAME(zdrot)(int *n, Rcomplex *zx, int *incx, Rcomplex *zy,
		int *incy, double *c, double *s);
    BLAS_extern void
    F77_NAME(zdscal)(int *n, double *da, Rcomplex *zx, int *incx);
    BLAS_extern void
    F77_NAME(zgbmv)(char *trans, int *m, int *n, int *kl,
		    int *ku, Rcomplex *alpha, Rcomplex *a, int *lda,
		    Rcomplex *x, int *incx, Rcomplex *beta, Rcomplex *y,
		    int *incy);
    BLAS_extern void
    F77_NAME(zgemm)(const char *transa, const char *transb, const int *m,
		    const int *n, const int *k, const Rcomplex *alpha,
		    const Rcomplex *a, const int *lda,
		    const Rcomplex *b, const int *ldb,
		    const Rcomplex *beta, Rcomplex *c, const int *ldc);
    BLAS_extern void
    F77_NAME(zgemv)(char *trans, int *m, int *n, Rcomplex *alpha,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx,
		    Rcomplex *beta, Rcomplex *y, int * incy);
    BLAS_extern void
    F77_NAME(zgerc)(int *m, int *n, Rcomplex *alpha, Rcomplex *x,
		    int *incx, Rcomplex *y, int *incy, Rcomplex *a, int *lda);
    BLAS_extern void
    F77_NAME(zgeru)(int *m, int *n, Rcomplex *alpha, Rcomplex *x,
		    int *incx, Rcomplex *y, int *incy, Rcomplex *a, int *lda);
    BLAS_extern void
    F77_NAME(zhbmv)(char *uplo, int *n, int *k, Rcomplex *alpha,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx,
		    Rcomplex *beta, Rcomplex *y, int *incy);
    BLAS_extern void
    F77_NAME(zhemm)(char *side, char *uplo, int *m, int *n,
		    Rcomplex *alpha, Rcomplex *a, int *lda, Rcomplex *b,
		    int *ldb, Rcomplex *beta, Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(zhemv)(char *uplo, int *n, Rcomplex *alpha, Rcomplex *a,
		    int *lda, Rcomplex *x, int *incx, Rcomplex *beta,
		    Rcomplex *y, int *incy);
    BLAS_extern void
    F77_NAME(zher)(char *uplo, int *n, double *alpha, Rcomplex *x,
		   int *incx, Rcomplex *a, int *lda);
    BLAS_extern void
    F77_NAME(zher2)(char *uplo, int *n, Rcomplex *alpha, Rcomplex *x,
		    int *incx, Rcomplex *y, int *incy, Rcomplex *a, int *lda);
    BLAS_extern void
    F77_NAME(zher2k)(char *uplo, char *trans, int *n, int *k,
		     Rcomplex *alpha, Rcomplex *a, int *lda, Rcomplex *b,
		     int *ldb, double *beta, Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(zherk)(char *uplo, char *trans, int *n, int *k,
		    double *alpha, Rcomplex *a, int *lda, double *beta,
		    Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(zhpmv)(char *uplo, int *n, Rcomplex *alpha, Rcomplex *ap,
		    Rcomplex *x, int *incx, Rcomplex * beta, Rcomplex *y,
		    int *incy);
    BLAS_extern void
    F77_NAME(zhpr)(char *uplo, int *n, double *alpha,
		   Rcomplex *x, int *incx, Rcomplex *ap);
    BLAS_extern void
    F77_NAME(zhpr2)(char *uplo, int *n, Rcomplex *alpha, Rcomplex *x,
		    int *incx, Rcomplex *y, int *incy, Rcomplex *ap);
    BLAS_extern void
    F77_NAME(zrotg)(Rcomplex *ca, Rcomplex *cb, double *c, Rcomplex *s);
    BLAS_extern void
    F77_NAME(zscal)(int *n, Rcomplex *za, Rcomplex *zx, int *incx);
    BLAS_extern void
    F77_NAME(zswap)(int *n, Rcomplex *zx, int *incx, Rcomplex *zy, int *incy);
    BLAS_extern void
    F77_NAME(zsymm)(char *side, char *uplo, int *m, int *n,
		    Rcomplex *alpha, Rcomplex *a, int *lda, Rcomplex *b,
		    int *ldb, Rcomplex *beta, Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(zsyr2k)(char *uplo, char *trans, int *n, int *k,
		     Rcomplex *alpha, Rcomplex *a, int *lda, Rcomplex *b,
		     int *ldb, Rcomplex *beta, Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(zsyrk)(char *uplo, char *trans, int *n, int *k,
		    Rcomplex *alpha, Rcomplex *a, int *lda,
		    Rcomplex *beta, Rcomplex *c, int *ldc);
    BLAS_extern void
    F77_NAME(ztbmv)(char *uplo, char *trans, char *diag, int *n, int *k,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx);
    BLAS_extern void
    F77_NAME(ztbsv)(char *uplo, char *trans, char *diag, int *n, int *k,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx);
    BLAS_extern void
    F77_NAME(ztpmv)(char *uplo, char *trans, char *diag, int *n,
		    Rcomplex *ap, Rcomplex *x, int *incx);
    BLAS_extern void
    F77_NAME(ztpsv)(char *uplo, char *trans, char *diag, int *n,
		    Rcomplex *ap, Rcomplex *x, int *incx);
    BLAS_extern void
    F77_NAME(ztrmm)(char *side, char *uplo, char *transa, char *diag,
		    int *m, int *n, Rcomplex *alpha, Rcomplex *a,
		    int *lda, Rcomplex *b, int *ldb);
    BLAS_extern void
    F77_NAME(ztrmv)(char *uplo, char *trans, char *diag, int *n,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx);
    BLAS_extern void
    F77_NAME(ztrsm)(char *side, char *uplo, char *transa, char *diag,
		    int *m, int *n, Rcomplex *alpha, Rcomplex *a,
		    int *lda, Rcomplex *b, int *ldb);
    BLAS_extern void
    F77_NAME(ztrsv)(char *uplo, char *trans, char *diag, int *n,
		    Rcomplex *a, int *lda, Rcomplex *x, int *incx);
/* #endif */

#ifdef  __cplusplus
}
#endif

#endif /* R_BLAS_H */
