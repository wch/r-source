/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003 The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef R_BLAS_H
#define R_BLAS_H
/* C declarations of BLAS routines.  R packages that use these should have */
/* src/Makevars declare PKG_LIBS = $(BLAS_LIBS) $(FLIBS) */

#include <R_ext/RS.h>		/* for F77_... */
#include <R_ext/Complex.h>	/* for Rcomplex */

#ifdef  __cplusplus
extern "C" {
#endif

/* Double Precision Level 1 BLAS */

double /* DASUM - sum of absolute values of a one-dimensional array */
F77_NAME(dasum)(const int *n, const double *dx, const int *incx);
void   /* DAXPY - replace y by alpha*x + y */
F77_NAME(daxpy)(const int *n, const double *alpha,
		const double *dx, const int *incx,
		double *dy, const int *incy);
void   /* DCOPY - copy x to y */
F77_NAME(dcopy)(const int *n, const double *dx, const int *incx,
		double *dy, const int *incy);
double /* DDOT - inner product of x and y */
F77_NAME(ddot)(const int *n, const double *dx, const int *incx, 
	       const double *dy, const int *incy);
double /* DNRM2 - 2-norm of a vector */
F77_NAME(dnrm2)(const int *n, const double *dx, const int *incx); 
void   /* DROT - apply a Given's rotation */
F77_NAME(drot)(const int *n, double *dx, const int *incx,
	       double *dy, const int *incy, const double *c, const double *s);
void   /* DROTG - generate a Given's rotation */
F77_NAME(drotg)(const double *a, const double *b, double *c, double *s);
void   /* DSCAL - scale a one-dimensional array */
F77_NAME(dscal)(const int *n, const double *alpha, double *dx, const int *incx);
void   /* DSWAP - interchange one-dimensional arrays */
F77_NAME(dswap)(const int *n, double *dx, const int *incx,
		double *dy, const int *incy);
int    /* IDAMAX - return the index of the element with max abs value */
F77_NAME(idamax)(const int *n, const double *dx, const int *incx);

/* Double Precision Level 2 BLAS */

/* DGBMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y, */
void
F77_NAME(dgbmv)(const char *trans, const int *m, const int *n,
		const int *kl,const int *ku,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DGEMV - perform one of the matrix-vector operations */
/* y := alpha*A*x + beta*y, or y := alpha*A'*x + beta*y,  */
void
F77_NAME(dgemv)(const char *trans, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx, const double *beta,
		double *y, const int *incy); 
/* DSBMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
void
F77_NAME(dsbmv)(const char *uplo, const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DSPMV - perform the matrix-vector operation */
/* y := alpha*A*x + beta*y, */
void
F77_NAME(dspmv)(const char *uplo, const int *n,
		const double *alpha, const double *ap,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);

/* DSYMV - perform the matrix-vector operation */
/*  y := alpha*A*x + beta*y, */
void
F77_NAME(dsymv)(const char *uplo, const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy);
/* DTBMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
void
F77_NAME(dtbmv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx); 
/* DTPMV - perform one of the matrix-vector operations */
/* x := A*x, or x := A'*x, */
void 
F77_NAME(dtpmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *ap, 
		double *x, const int *incx);
/* DTRMV - perform one of the matrix-vector operations  */
/* x := A*x, or x := A'*x, */
void
F77_NAME(dtrmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *a, const int *lda,
		double *x, const int *incx);
/* DTBSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
void
F77_NAME(dtbsv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx); 
/* DTPSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
void
F77_NAME(dtpsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *ap, double *x, const int *incx);
/* DTRSV - solve one of the systems of equations */
/* A*x = b, or A'*x = b, */
void
F77_NAME(dtrsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *a, const int *lda,
		double *x, const int *incx); 
/* DGER - perform the rank 1 operation   A := alpha*x*y' + A */
void
F77_NAME(dger)(const int *m, const int *n, const double *alpha,
	       double *x, const int *incx,
	       double *y, const int *incy,
	       double *a, const int *lda);
/* DSYR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
void 
F77_NAME(dsyr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx,
	       double *a, const int *lda);
/* DSPR - perform the symmetric rank 1 operation A := alpha*x*x' + A */
void
F77_NAME(dspr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx, double *ap);
/* DSYR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A, */
void
F77_NAME(dsyr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy,
		double *a, const int *lda); 
/* DSPR2 - perform the symmetric rank 2 operation */
/* A := alpha*x*y' + alpha*y*x' + A,  */
void
F77_NAME(dspr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy, double *ap);

/* Double Precision Level 3 BLAS */

/* DGEMM - perform one of the matrix-matrix operations    */
/* C := alpha*op( A )*op( B ) + beta*C */
void 
F77_NAME(dgemm)(const char *transa, const char *transb, const int *m,
		const int *n, const int *k, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc);
/* DTRSM - solve one of the matrix equations  */
/* op(A)*X = alpha*B, or  X*op(A) = alpha*B  */
void
F77_NAME(dtrsm)(const char *side, const char *uplo,
		const char *transa, const char *diag,
		const int *m, const int *n, const double *alpha,
		const double *a, const int *lda,
		double *b, const int *ldb);
/* DTRMM - perform one of the matrix-matrix operations */
/* B := alpha*op( A )*B, or B := alpha*B*op( A ) */
void
F77_NAME(dtrmm)(const char *side, const char *uplo, const char *transa,
		const char *diag, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		double *b, const int *ldb); 
/* DSYMM - perform one of the matrix-matrix operations   */
/*  C := alpha*A*B + beta*C, */
void
F77_NAME(dsymm)(const char *side, const char *uplo, const int *m,
		const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc);
/* DSYRK - perform one of the symmetric rank k operations */
/* C := alpha*A*A' + beta*C or C := alpha*A'*A + beta*C */
void
F77_NAME(dsyrk)(const char *uplo, const char *trans,
		const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *beta, double *c, const int *ldc);
/* DSYR2K - perform one of the symmetric rank 2k operations */
/* C := alpha*A*B' + alpha*B*A' + beta*C or */
/* C := alpha*A'*B + alpha*B'*A + beta*C */
void
F77_NAME(dsyr2k)(const char *uplo, const char *trans,
		 const int *n, const int *k,
		 const double *alpha, const double *a, const int *lda,
		 const double *b, const int *ldb,
		 const double *beta, double *c, const int *ldc);

/*  LSAME returns .TRUE. if CA is the same letter as CB regardless of
    case. */
extern int
F77_NAME(lsame)(char *ca, char *cb);

#ifdef  __cplusplus
}
#endif

#endif /* R_BLAS_H */
