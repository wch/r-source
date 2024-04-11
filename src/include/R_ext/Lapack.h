/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2024 The R Core Team.
 *  Copyright (C) 2008-2019 The R Foundation
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
   C declarations of LAPACK Fortran subroutines included in R.
   Those used (currently or previously) by C routines in R itself,
   plus a few used in packages.

   Part of the API.

   R packages that use these should have PKG_LIBS in src/Makevars include 
   $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)
 */


#ifndef R_LAPACK_H
#define R_LAPACK_H

#include <R_ext/RS.h>		/* for F77_... */
#include <R_ext/BLAS.h>         /* defines FCLEN */

// allow future 64-bit nrow, ncol and increments. by using int64_t
// configure checked that Fortran INTEGER is 32-bit and the same as C int.

/* logicals need checking (already they are compiler-dependent)
   LAPACKE has 
     #define lapack_logical lapack_int
   hence int32_t/int64_t
   But that is not what Fortran comoilers do (int_least32_t for gfortran) 
   OTOH the Accelerate headers use int/long.
*/

// Keep these in step with BLAS.h
#ifndef ILP64
# define La_INT int
# define La_LGL int
#endif

#ifndef La_complex
# include <R_ext/Complex.h>
# define La_complex Rcomplex
#endif

/*
  LAPACK function names are [dz]<name>(), where d denotes the real
  version of the function, z the complex version.  (Only
  double-precision/double-complex versions are used in R.)
*/

#ifdef	__cplusplus
extern "C" {
#endif

/* The LAPACK version: might change after installation with
   external LAPACK.

   In the LAPACK sources in the INSTALL directory.
*/
extern void F77_NAME(ilaver)(La_INT *major, La_INT *minor, La_INT *patch);

// Never defined by R itself.
#ifndef La_extern
#define La_extern extern
#endif

// Utilities for Lapack-using packages :
// ------------------------------------

/* matrix norms: converting typstr[]  to one of {'M', 'O', 'I', 'F'}
 * or signal error(): */
// La_extern char La_norm_type(const char *typstr);

/* matrix (reciprocal) condition numbers: convert typstr[]  to 'O'(ne) or 'I'(nf)
 * or signal error(): */
// La_extern char La_rcond_type(const char *typstr);


/* Selected Double Precision Lapack Routines
   ========
 */

//* Double precision BiDiagonal and DIagonal matrices  -> DBD & DDI

/* DBDSQR - compute the singular value decomposition (SVD) of a real */
/* N-by-N (upper or lower) bidiagonal matrix B */
La_extern void
F77_NAME(dbdsqr)(const char* uplo, const La_INT* n, const La_INT* ncvt,
		 const La_INT* nru, const La_INT* ncc, double* d, double* e,
		 double* vt, const La_INT* ldvt, double* u, const La_INT* ldu,
		 double* c, const La_INT* ldc, double* work, La_INT* info FCLEN);
/* DDISNA - compute the reciprocal condition numbers for the */
/* eigenvectors of a real symmetric or complex Hermitian matrix or */
/* for the left or right singular vectors of a general m-by-n */
/* matrix */
La_extern void
F77_NAME(ddisna)(const char* job, const La_INT* m, const La_INT* n,
		 double* d, double* sep, La_INT* info FCLEN);


//* Double precision General Banded matrices -> DGB

/* DGBBRD - reduce a real general m-by-n band matrix A to upper */
/* bidiagonal form B by an orthogonal transformation  */
La_extern void
F77_NAME(dgbbrd)(const char* vect, const La_INT* m, const La_INT* n,
		 const La_INT* ncc, const La_INT* kl, const La_INT* ku,
		 double* ab, const La_INT* ldab,
		 double* d, double* e, double* q,
		 const La_INT* ldq, double* pt, const La_INT* ldpt,
		 double* c, const La_INT* ldc,
		 double* work, La_INT* info FCLEN);
/* DGBCON - estimate the reciprocal of the condition number of a */
/* real general band matrix A, in either the 1-norm or the */
/* infinity-norm */
La_extern void
F77_NAME(dgbcon)(const char* norm, const La_INT* n, const La_INT* kl,
		 const La_INT* ku, double* ab, const La_INT* ldab,
		 La_INT* ipiv, const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGBEQU - compute row and column scalings intended to equilibrate */
/* an M-by-N band matrix A and reduce its condition number */
La_extern void
F77_NAME(dgbequ)(const La_INT* m, const La_INT* n, const La_INT* kl, const La_INT* ku,
		 double* ab, const La_INT* ldab, double* r, double* c,
		 double* rowcnd, double* colcnd, double* amax, La_INT* info);
/* DGBRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is banded, and provides */
/* error bounds and backward error estimates for the solution */
La_extern void
F77_NAME(dgbrfs)(const char* trans, const La_INT* n, const La_INT* kl,
		 const La_INT* ku, const La_INT* nrhs, double* ab,
		 const La_INT* ldab, double* afb, const La_INT* ldafb,
		 La_INT* ipiv, double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGBSV - compute the solution to a real system of linear */
/* equations A * X = B, where A is a band matrix of order N with */
/* KL subdiagonals and KU superdiagonals, and X and B are */
/* N-by-NRHS matrices */
La_extern void
F77_NAME(dgbsv)(const La_INT* n, const La_INT* kl,const La_INT* ku,
		const La_INT* nrhs, double* ab, const La_INT* ldab,
		La_INT* ipiv, double* b, const La_INT* ldb, La_INT* info);
/* DGBSVX - use the LU factorization to compute the solution to a */
/* real system of linear equations A * X = B or A**T * X = B */
La_extern void
F77_NAME(dgbsvx)(const La_INT* fact, const char* trans,
		 const La_INT* n, const La_INT* kl,const La_INT* ku,
		 const La_INT* nrhs, double* ab, const La_INT* ldab,
		 double* afb, const La_INT* ldafb, La_INT* ipiv,
		 const char* equed, double* r, double* c,
		 double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* rcond, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN FCLEN);
/* DGBTF2 - compute an LU factorization of a real m-by-n band */
/* matrix A using partial pivoting with row La_INT *erchanges */
La_extern void
F77_NAME(dgbtf2)(const La_INT* m, const La_INT* n, const La_INT* kl,const La_INT* ku,
		 double* ab, const La_INT* ldab, La_INT* ipiv, La_INT* info);
/* DGBTRF - compute an LU factorization of a real m-by-n band */
/* matrix A using partial pivoting with row La_INT *erchanges */
La_extern void
F77_NAME(dgbtrf)(const La_INT* m, const La_INT* n, const La_INT* kl,const La_INT* ku,
		  double* ab, const La_INT* ldab, La_INT* ipiv, La_INT* info);
/* DGBTRS - solve a system of linear equations	A * X = B or  */
/* A' * X = B with a general band matrix A using the LU */
/* factorization computed by DGBTRF */
La_extern void
F77_NAME(dgbtrs)(const char* trans, const La_INT* n,
		 const La_INT* kl, const La_INT* ku, const La_INT* nrhs,
		 const double* ab, const La_INT* ldab, const La_INT* ipiv,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);


//* Double precision GEneral matrices -> DGE

/* DGEBAK - form the right or left eigenvectors of a real general */
/* matrix by backward transformation on the computed eigenvectors */
/* of the balanced matrix output by DGEBAL  */
La_extern void
F77_NAME(dgebak)(const char* job, const char* side, const La_INT* n,
		 const La_INT* ilo, const La_INT* ihi, double* scale,
		 const La_INT* m, double* v, const La_INT* ldv, La_INT* info
		 FCLEN FCLEN);
/* DGEBAL - balance a general real matrix A */
La_extern void
F77_NAME(dgebal)(const char* job, const La_INT* n, double* a, const La_INT* lda,
		 La_INT* ilo, La_INT* ihi, double* scale, La_INT* info FCLEN);
/* DGEBD2 - reduce a real general m by n matrix A to upper or */
/* lower bidiagonal form B by an orthogonal transformation */
La_extern void
F77_NAME(dgebd2)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* d, double* e, double* tauq, double* taup,
		 double* work, La_INT* info);
/* DGEBRD - reduce a general real M-by-N matrix A to upper or */
/* lower bidiagonal form B by an orthogonal transformation */
La_extern void
F77_NAME(dgebrd)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* d, double* e, double* tauq, double* taup,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGECON - estimate the reciprocal of the condition number of a */
/* general real matrix A, in either the 1-norm or the */
/* infinity-norm, using the LU factorization computed by DGETRF */
La_extern void
F77_NAME(dgecon)(const char* norm, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGEEQU - compute row and column scalings La_INT *ended to equilibrate */
/* an M-by-N matrix A and reduce its condition number */
La_extern void
F77_NAME(dgeequ)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* r, double* c, double* rowcnd, double* colcnd,
		 double* amax, La_INT* info);
/* DGEES - compute for an N-by-N real nonsymmetric matrix A, the */
/* eigenvalues, the real Schur form T, and, optionally, the matrix */
/* of Schur vectors Z */
La_extern void
F77_NAME(dgees)(const char* jobvs, const char* sort,
		La_LGL (*select)(const double*, const double*),
		const La_INT* n, double* a, const La_INT* lda,
		La_INT* sdim, double* wr, double* wi,
		double* vs, const La_INT* ldvs,
		double* work, const La_INT* lwork, La_LGL* bwork, La_INT* info 
		FCLEN FCLEN);
/* DGEESX - compute for an N-by-N real nonsymmetric matrix A, the */
/* eigenvalues, the real Schur form T, and, optionally, the matrix */
/* of Schur vectors Z */
La_extern void
F77_NAME(dgeesx)(const char* jobvs, const char* sort,
		 La_LGL (*select)(const double*, const double*),
		 const char* sense, const La_INT* n, double* a,
		 const La_INT* lda, La_INT* sdim, double* wr, double* wi,
		 double* vs, const La_INT* ldvs, double* rconde,
		 double* rcondv, double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_LGL* bwork, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DGEEV - compute for an N-by-N real nonsymmetric matrix A, the */
/* eigenvalues and, optionally, the left and/or right eigenvectors */
La_extern void
F77_NAME(dgeev)(const char* jobvl, const char* jobvr,
		const La_INT* n, double* a, const La_INT* lda,
		double* wr, double* wi, double* vl, const La_INT* ldvl,
		double* vr, const La_INT* ldvr,
		double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DGEEVX - compute for an N-by-N real nonsymmetric matrix A, the */
/* eigenvalues and, optionally, the left and/or right eigenvectors */
La_extern void
F77_NAME(dgeevx)(const char* balanc, const char* jobvl, const char* jobvr,
		 const char* sense, const La_INT* n, double* a, const La_INT* lda,
		 double* wr, double* wi, double* vl, const La_INT* ldvl,
		 double* vr, const La_INT* ldvr, La_INT* ilo, La_INT* ihi,
		 double* scale, double* abnrm, double* rconde, double* rcondv,
		 double* work, const La_INT* lwork, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN FCLEN);
/* DGEHD2 - reduce a real general matrix A to upper Hessenberg */
/* form H by an orthogonal similarity transformation */
La_extern void
F77_NAME(dgehd2)(const La_INT* n, const La_INT* ilo, const La_INT* ihi,
		 double* a, const La_INT* lda, double* tau,
		 double* work, La_INT* info);
/* DGEHRD - reduce a real general matrix A to upper Hessenberg */
/* form H by an orthogonal similarity transformation */
La_extern void
F77_NAME(dgehrd)(const La_INT* n, const La_INT* ilo, const La_INT* ihi,
		 double* a, const La_INT* lda, double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGELQ2 - compute an LQ factorization of a real m by n matrix A */
La_extern void
F77_NAME(dgelq2)(const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda, double* tau,
		 double* work, La_INT* info);
/* DGELQF - compute an LQ factorization of a real M-by-N matrix A */
La_extern void
F77_NAME(dgelqf)(const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda, double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGELS - solve overdetermined or underdetermined real linear */
/* systems involving an M-by-N matrix A, or its transpose, using a */
/* QR or LQ factorization of A */
La_extern void
F77_NAME(dgels)(const char* trans, const La_INT* m, const La_INT* n,
		const La_INT* nrhs, double* a, const La_INT* lda,
		double* b, const La_INT* ldb,
		double* work, const La_INT* lwork, La_INT* info FCLEN);
/* DGELSS - compute the minimum norm solution to a real linear */
/* least squares problem */
La_extern void
F77_NAME(dgelss)(const La_INT* m, const La_INT* n, const La_INT* nrhs,
		 double* a, const La_INT* lda, double* b, const La_INT* ldb,
		 double* s, double* rcond, La_INT* rank,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGELSY - compute the minimum-norm solution to a real linear */
/* least squares problem */
La_extern void
F77_NAME(dgelsy)(const La_INT* m, const La_INT* n, const La_INT* nrhs,
		 double* a, const La_INT* lda, double* b, const La_INT* ldb,
		 La_INT* jpvt, const double* rcond, La_INT* rank,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGEQL2 - compute a QL factorization of a real m by n matrix A */
La_extern void
F77_NAME(dgeql2)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* tau, double* work, La_INT* info);
/* DGEQLF - compute a QL factorization of a real M-by-N matrix A */
La_extern void
F77_NAME(dgeqlf)(const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda, double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGEQP3 - compute a QR factorization with column pivoting of a */
/* real M-by-N matrix A using level 3 BLAS */
La_extern void
F77_NAME(dgeqp3)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 La_INT* jpvt, double* tau, double* work, const La_INT* lwork,
		 La_INT* info);
/* DGEQR2 - compute a QR factorization of a real m by n matrix A */
La_extern void
F77_NAME(dgeqr2)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* tau, double* work, La_INT* info);
/* DGEQRF - compute a QR factorization of a real M-by-N matrix A */
La_extern void
F77_NAME(dgeqrf)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* tau, double* work, const La_INT* lwork, La_INT* info);
/* DGERFS - improve the computed solution to a system of linear */
/* equations and provides error bounds and backward error */
/* estimates for the solution */
La_extern void
F77_NAME(dgerfs)(const char* trans, const La_INT* n, const La_INT* nrhs,
		 double* a, const La_INT* lda, double* af, const La_INT* ldaf,
		 La_INT* ipiv, double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGERQ2 - compute an RQ factorization of a real m by n matrix A */
La_extern void
F77_NAME(dgerq2)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* tau, double* work, La_INT* info);
/* DGERQF - compute an RQ factorization of a real M-by-N matrix A */
La_extern void
F77_NAME(dgerqf)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 double* tau, double* work, const La_INT* lwork, La_INT* info);
/* DGESV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dgesv)(const La_INT* n, const La_INT* nrhs, double* a, const La_INT* lda,
		La_INT* ipiv, double* b, const La_INT* ldb, La_INT* info);
/* DGESVD - compute the singular value decomposition (SVD); of a */
/* real M-by-N matrix A, optionally computing the left and/or */
/* right singular vectors */
La_extern void
F77_NAME(dgesvd)(const char* jobu, const char* jobvt, const La_INT* m,
		 const La_INT* n, double* a, const La_INT* lda, double* s,
		 double* u, const La_INT* ldu, double* vt, const La_INT* ldvt,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DGESVX - use the LU factorization to compute the solution to a */
/* real system of linear equations  A * X = B, */
La_extern void
F77_NAME(dgesvx)(const char* fact, const char* trans, const La_INT* n,
		 const La_INT* nrhs, double* a, const La_INT* lda,
		 double* af, const La_INT* ldaf, La_INT* ipiv,
		 char* equed, double* r, double* c,
		 double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* rcond, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN FCLEN FCLEN);
/* DGETF2 - compute an LU factorization of a general m-by-n */
/* matrix A using partial pivoting with row La_INT *erchanges */
La_extern void
F77_NAME(dgetf2)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 La_INT* ipiv, La_INT* info);
/* DGETRF - compute an LU factorization of a general M-by-N */
/* matrix A using partial pivoting with row La_INT *erchanges */
La_extern void
F77_NAME(dgetrf)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 La_INT* ipiv, La_INT* info);
/* DGETRI - compute the inverse of a matrix using the LU */
/* factorization computed by DGETRF */
La_extern void
F77_NAME(dgetri)(const La_INT* n, double* a, const La_INT* lda,
		 La_INT* ipiv, double* work, const La_INT* lwork, La_INT* info);
/* DGETRS - solve a system of linear equations	A * X = B or A' * */
/* X = B with a general N-by-N matrix A using the LU factorization */
/* computed by DGETRF */
La_extern void
F77_NAME(dgetrs)(const char* trans, const La_INT* n, const La_INT* nrhs,
		 const double* a, const La_INT* lda, const La_INT* ipiv,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);


//* Double precision General matrices Generalized problems -> DGG

/* DGGBAK - form the right or left eigenvectors of a real */
/* generalized eigenvalue problem A*x = lambda*B*x, by backward */
/* transformation on the computed eigenvectors of the balanced */
/* pair of matrices output by DGGBAL */
La_extern void
F77_NAME(dggbak)(const char* job, const char* side,
		 const La_INT* n, const La_INT* ilo, const La_INT* ihi,
		 double* lscale, double* rscale, const La_INT* m,
		 double* v, const La_INT* ldv, La_INT* info FCLEN FCLEN);
/* DGGBAL - balance a pair of general real matrices (A,B); */
La_extern void
F77_NAME(dggbal)(const char* job, const La_INT* n, double* a, const La_INT* lda,
		 double* b, const La_INT* ldb, La_INT* ilo, La_INT* ihi,
		 double* lscale, double* rscale, double* work, La_INT* info
		 FCLEN);
/* DGGES - compute for a pair of N-by-N real nonsymmetric */
/* matrices A, B the generalized eigenvalues, the generalized */
/* real Schur form (S,T), optionally, the left and/or right matrices */
/* of Schur vectors (VSL and VSR)*/
La_extern void
F77_NAME(dgges)(const char* jobvsl, const char* jobvsr, const char* sort,
		La_LGL (*delztg)(double*, double*, double*),
		const La_INT* n, double* a, const La_INT* lda,
		double* b, const La_INT* ldb, double* alphar,
		double* alphai, const double* beta,
		double* vsl, const La_INT* ldvsl,
		double* vsr, const La_INT* ldvsr,
		double* work, const La_INT* lwork, La_LGL* bwork, La_INT* info 
		FCLEN FCLEN FCLEN);

/* DGGGLM - solve a general Gauss-Markov linear model (GLM) problem */
La_extern void
F77_NAME(dggglm)(const La_INT* n, const La_INT* m, const La_INT* p,
		 double* a, const La_INT* lda, double* b, const La_INT* ldb,
		 double* d, double* x, double* y,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGGHRD - reduce a pair of real matrices (A,B); to generalized */
/* upper Hessenberg form using orthogonal transformations, where A */
/* is a general matrix and B is upper triangular */
La_extern void
F77_NAME(dgghrd)(const char* compq, const char* compz, const La_INT* n,
		 const La_INT* ilo, const La_INT* ihi, double* a, const La_INT* lda,
		 double* b, const La_INT* ldb, double* q, const La_INT* ldq,
		 double* z, const La_INT* ldz, La_INT* info FCLEN FCLEN);
/* DGGLSE - solve the linear equality-constrained least squares */
/* (LSE) problem */
La_extern void
F77_NAME(dgglse)(const La_INT* m, const La_INT* n, const La_INT* p,
		 double* a, const La_INT* lda,
		 double* b, const La_INT* ldb,
		 double* c, double* d, double* x,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGGQRF - compute a generalized QR factorization of an N-by-M */
/* matrix A and an N-by-P matrix B */
La_extern void
F77_NAME(dggqrf)(const La_INT* n, const La_INT* m, const La_INT* p,
		 double* a, const La_INT* lda, double* taua,
		 double* b, const La_INT* ldb, double* taub,
		 double* work, const La_INT* lwork, La_INT* info);
/* DGGRQF - compute a generalized RQ factorization of an M-by-N */
/* matrix A and a P-by-N matrix B */
La_extern void
F77_NAME(dggrqf)(const La_INT* m, const La_INT* p, const La_INT* n,
		 double* a, const La_INT* lda, double* taua,
		 double* b, const La_INT* ldb, double* taub,
		 double* work, const La_INT* lwork, La_INT* info);

//* Double precision General Tridiagonal matrices  -> DGT

/* DGTCON - estimate the reciprocal of the condition number of a real */
/* tridiagonal matrix A using the LU factorization as computed by DGTTRF */
La_extern void
F77_NAME(dgtcon)(const char* norm, const La_INT* n, double* dl, double* d,
		 double* du, double* du2, La_INT* ipiv, const double* anorm,
		 double* rcond, double* work, La_INT* iwork, La_INT* info
		 FCLEN);
/* DGTRFS - improve the computed solution to a system of linear equations */
/* when the coefficient matrix is tridiagonal, and provides error bounds */
/* and backward error estimates for the solution */
La_extern void
F77_NAME(dgtrfs)(const char* trans, const La_INT* n, const La_INT* nrhs,
		 double* dl, double* d, double* du, double* dlf,
		 double* df, double* duf, double* du2,
		 La_INT* ipiv, double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGTSV - solve the equation	A*X = B, */
La_extern void
F77_NAME(dgtsv)(const La_INT* n, const La_INT* nrhs,
		double* dl, double* d, double* du,
		double* b, const La_INT* ldb, La_INT* info);
/* DGTSVX - use the LU factorization to compute the solution to a */
/* real system of linear equations A * X = B or A**T * X = B, */
La_extern void
F77_NAME(dgtsvx)(const La_INT* fact, const char* trans,
		 const La_INT* n, const La_INT* nrhs,
		 double* dl, double* d, double* du,
		 double* dlf, double* df, double* duf,
		 double* du2, La_INT* ipiv,
		 double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* rcond, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DGTTRF - compute an LU factorization of a real tridiagonal matrix */
/* A using elimination with partial pivoting and row La_INT *erchanges */
La_extern void
F77_NAME(dgttrf)(const La_INT* n, double* dl, double* d,
		 double* du, double* du2, La_INT* ipiv, La_INT* info);
/* DGTTRS - solve one of the systems of equations  A*X = B or */
/* A'*X = B, */
La_extern void
F77_NAME(dgttrs)(const char* trans, const La_INT* n, const La_INT* nrhs,
		 double* dl, double* d, double* du, double* du2,
		 La_INT* ipiv, double* b, const La_INT* ldb, La_INT* info FCLEN);


//* Double precision Orthogonal matrices  -> DOP & DOR

/* DOPGTR - generate a real orthogonal matrix Q which is defined */
/* as the product of n-1 elementary reflectors H(i); of order n, */
/* as returned by DSPTRD using packed storage */
La_extern void
F77_NAME(dopgtr)(const char* uplo, const La_INT* n,
		 const double* ap, const double* tau,
		 double* q, const La_INT* ldq,
		 double* work, La_INT* info FCLEN);
/* DOPMTR - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dopmtr)(const char* side, const char* uplo,
		 const char* trans, const La_INT* m, const La_INT* n,
		 const double* ap, const double* tau,
		 double* c, const La_INT* ldc,
		 double* work, La_INT* info FCLEN FCLEN FCLEN);
/* DORG2L - generate an m by n real matrix Q with orthonormal */
/* columns, */
La_extern void
F77_NAME(dorg2l)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda,
		 const double* tau, double* work, La_INT* info);
/* DORG2R - generate an m by n real matrix Q with orthonormal */
/* columns, */
La_extern void
F77_NAME(dorg2r)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda,
		 const double* tau, double* work, La_INT* info);
/* DORGBR - generate one of the real orthogonal matrices Q or */
/* P**T determined by DGEBRD when reducing a real matrix A to */
/* bidiagonal form */
La_extern void
F77_NAME(dorgbr)(const char* vect, const La_INT* m,
		 const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda,
		 const double* tau, double* work,
		 const La_INT* lwork, La_INT* info FCLEN);
/* DORGHR - generate a real orthogonal matrix Q which is defined */
/* as the product of IHI-ILO elementary reflectors of order N, as */
/* returned by DGEHRD */
La_extern void
F77_NAME(dorghr)(const La_INT* n, const La_INT* ilo, const La_INT* ihi,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DORGL2 - generate an m by n real matrix Q with orthonormal */
/* rows, */
La_extern void
F77_NAME(dorgl2)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, La_INT* info);
/* DORGLQ - generate an M-by-N real matrix Q with orthonormal */
/* rows, */
La_extern void
F77_NAME(dorglq)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda,
		 const double* tau, double* work,
		 const La_INT* lwork, La_INT* info);
/* DORGQL - generate an M-by-N real matrix Q with orthonormal */
/* columns, */
La_extern void
F77_NAME(dorgql)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda,
		 const double* tau, double* work,
		 const La_INT* lwork, La_INT* info);
/* DORGQR - generate an M-by-N real matrix Q with orthonormal */
/* columns, */
La_extern void
F77_NAME(dorgqr)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DORGR2 - generate an m by n real matrix Q with orthonormal */
/* rows, */
La_extern void
F77_NAME(dorgr2)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, La_INT* info);
/* DORGRQ - generate an M-by-N real matrix Q with orthonormal rows */
La_extern void
F77_NAME(dorgrq)(const La_INT* m, const La_INT* n, const La_INT* k,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, const La_INT* lwork, La_INT* info);
/* DORGTR - generate a real orthogonal matrix Q which is defined */
/* as the product of n-1 elementary reflectors of order const La_INT* n, as */
/* returned by DSYTRD */
La_extern void
F77_NAME(dorgtr)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, const double* tau,
		 double* work, const La_INT* lwork, La_INT* info FCLEN);
/* DORM2L - overwrite the general real m by n matrix C with   Q * */
/* C if SIDE = 'L' and TRANS = 'N', or	 Q'* C if SIDE = 'L' and */
/* TRANS = 'T', or   C * Q if SIDE = 'R' and TRANS = 'N', or   C * */
/* Q' if SIDE = 'R' and TRANS = 'T', */
La_extern void
F77_NAME(dorm2l)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, La_INT* info FCLEN FCLEN);
/* DORM2R - overwrite the general real m by n matrix C with   Q * C */
/* if SIDE = 'L' and TRANS = 'N', or   Q'* C if SIDE = 'L' and */
/* TRANS = 'T', or   C * Q if SIDE = 'R' and TRANS = 'N', or   C * */
/* Q' if SIDE = 'R' and TRANS = 'T', */
La_extern void
F77_NAME(dorm2r)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda, const double* tau,
		 double* c, const La_INT* ldc, double* work, La_INT* info
		 FCLEN FCLEN);
/* DORMBR - VECT = 'Q', DORMBR overwrites the general real M-by-N */
/* matrix C with  SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormbr)(const char* vect, const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda, const double* tau,
		 double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN FCLEN);
/* DORMHR - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormhr)(const char* side, const char* trans, const La_INT* m,
		 const La_INT* n, const La_INT* ilo, const La_INT* ihi,
		 const double* a, const La_INT* lda, const double* tau,
		 double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DORML2 - overwrite the general real m by n matrix C with   Q * */
/* C if SIDE = 'L' and TRANS = 'N', or	 Q'* C if SIDE = 'L' and */
/* TRANS = 'T', or   C * Q if SIDE = 'R' and TRANS = 'N', or   C * */
/* Q' if SIDE = 'R' and TRANS = 'T', */
La_extern void
F77_NAME(dorml2)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda, const double* tau,
		 double* c, const La_INT* ldc, double* work, La_INT* info
		 FCLEN FCLEN);
/* DORMLQ - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N'  */
La_extern void
F77_NAME(dormlq)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DORMQL - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormql)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DORMQR - overwrite the general real M-by-N matrix C with   SIDE = */
/* 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormqr)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DORMR2 - overwrite the general real m by n matrix C with   Q * */
/* C if SIDE = 'L' and TRANS = 'N', or	 Q'* C if SIDE = 'L' and */
/* TRANS = 'T', or   C * Q if SIDE = 'R' and TRANS = 'N', or   C * */
/* Q' if SIDE = 'R' and TRANS = 'T', */
La_extern void
F77_NAME(dormr2)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, La_INT* info FCLEN FCLEN);
/* DORMRQ - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormrq)(const char* side, const char* trans,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);
/* DORMTR - overwrite the general real M-by-N matrix C with */
/* SIDE = 'L' SIDE = 'R' TRANS = 'N' */
La_extern void
F77_NAME(dormtr)(const char* side, const char* uplo,
		 const char* trans, const La_INT* m, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const double* tau, double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork, La_INT* info
		 FCLEN FCLEN FCLEN);


//* Double precision Positive definite Band matrices  -> DPB

/* DPBCON - estimate the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric positive definite band matrix */
/* using the Cholesky factorization A = U**T*U or A = L*L**T */
/* computed by DPBTRF */
La_extern void
F77_NAME(dpbcon)(const char* uplo, const La_INT* n, const La_INT* kd,
		 const double* ab, const La_INT* ldab,
		 const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPBEQU - compute row and column scalings La_INT *ended to */
/* equilibrate a symmetric positive definite band matrix A and */
/* reduce its condition number (with respect to the two-norm); */
La_extern void
F77_NAME(dpbequ)(const char* uplo, const La_INT* n, const La_INT* kd,
		 const double* ab, const La_INT* ldab,
		 double* s, double* scond, double* amax, La_INT* info FCLEN);
/* DPBRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric positive */
/* definite and banded, and provides error bounds and backward */
/* error estimates for the solution */
La_extern void
F77_NAME(dpbrfs)(const char* uplo, const La_INT* n,
		 const La_INT* kd, const La_INT* nrhs,
		 const double* ab, const La_INT* ldab,
		 const double* afb, const La_INT* ldafb,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPBSTF - compute a split Cholesky factorization of a real */
/* symmetric positive definite band matrix A */
La_extern void
F77_NAME(dpbstf)(const char* uplo, const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab, La_INT* info FCLEN);
/* DPBSV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dpbsv)(const char* uplo, const La_INT* n,
		const La_INT* kd, const La_INT* nrhs,
		double* ab, const La_INT* ldab,
		double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DPBSVX - use the Cholesky factorization A = U**T*U or A = */
/* L*L**T to compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dpbsvx)(const La_INT* fact, const char* uplo, const La_INT* n,
		 const La_INT* kd, const La_INT* nrhs,
		 double* ab, const La_INT* ldab,
		 double* afb, const La_INT* ldafb,
		 char* equed, double* s,
		 double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* rcond,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN FCLEN);
/* DPBTF2 - compute the Cholesky factorization of a real */
/* symmetric positive definite band matrix A */
La_extern void
F77_NAME(dpbtf2)(const char* uplo, const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab, La_INT* info FCLEN);
/* DPBTRF - compute the Cholesky factorization of a real */
/* symmetric positive definite band matrix A */
La_extern void
F77_NAME(dpbtrf)(const char* uplo, const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab, La_INT* info FCLEN);
/* DPBTRS - solve a system of linear equations A*X = B with a */
/* symmetric positive definite band matrix A using the Cholesky */
/* factorization A = U**T*U or A = L*L**T computed by DPBTRF */
La_extern void
F77_NAME(dpbtrs)(const char* uplo, const La_INT* n,
		 const La_INT* kd, const La_INT* nrhs,
		 const double* ab, const La_INT* ldab,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);


//* Double precision Positive definite matrices  -> DPO

/* DPOCON - estimate the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric positive definite matrix using */
/* the Cholesky factorization A = U**T*U or A = L*L**T computed by */
/* DPOTRF */
La_extern void
F77_NAME(dpocon)(const char* uplo, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPOEQU - compute row and column scalings La_INT *ended to */
/* equilibrate a symmetric positive definite matrix A and reduce */
/* its condition number (with respect to the two-norm); */
La_extern void
F77_NAME(dpoequ)(const La_INT* n, const double* a, const La_INT* lda,
		 double* s, double* scond, double* amax, La_INT* info);
/* DPORFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric positive */
/* definite, */
La_extern void
F77_NAME(dporfs)(const char* uplo, const La_INT* n, const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 const double* af, const La_INT* ldaf,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPOSV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dposv)(const char* uplo, const La_INT* n, const La_INT* nrhs,
		double* a, const La_INT* lda,
		double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DPOSVX - use the Cholesky factorization A = U**T*U or A = */
/* L*L**T to compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dposvx)(const La_INT* fact, const char* uplo,
		 const La_INT* n, const La_INT* nrhs,
		 double* a, const La_INT* lda,
		 double* af, const La_INT* ldaf, const char* equed,
		 double* s, double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* rcond,
		 double* ferr, double* berr, double* work,
		 La_INT* iwork, La_INT* info FCLEN FCLEN);
/* DPOTF2 - compute the Cholesky factorization of a real */
/* symmetric positive definite matrix A */
La_extern void
F77_NAME(dpotf2)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);
/* DPOTRF - compute the Cholesky factorization of a real */
/* symmetric positive definite matrix A */
La_extern void
F77_NAME(dpotrf)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);
/* DPOTRI - compute the inverse of a real symmetric positive */
/* definite matrix A using the Cholesky factorization A = U**T*U */
/* or A = L*L**T computed by DPOTRF */
La_extern void
F77_NAME(dpotri)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);
/* DPOTRS - solve a system of linear equations A*X = B with a */
/* symmetric positive definite matrix A using the Cholesky */
/* factorization A = U**T*U or A = L*L**T computed by DPOTRF */
La_extern void
F77_NAME(dpotrs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs, const double* a, const La_INT* lda,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DPPCON - estimate the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric positive definite packed */
/* matrix using the Cholesky factorization A = U**T*U or A = */
/* L*L**T computed by DPPTRF */
La_extern void
F77_NAME(dppcon)(const char* uplo, const La_INT* n,
		 const double* ap, const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPPEQU - compute row and column scalings La_INT *ended to */
/* equilibrate a symmetric positive definite matrix A in packed */
/* storage and reduce its condition number (with respect to the */
/* two-norm); */
La_extern void
F77_NAME(dppequ)(const char* uplo, const La_INT* n,
		 const double* ap, double* s, double* scond,
		 double* amax, La_INT* info FCLEN);


//* Double precision Positive definite matrices in Packed storage  -> DPP

/* DPPRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric positive */
/* definite and packed, and provides error bounds and backward */
/* error estimates for the solution */
La_extern void
F77_NAME(dpprfs)(const char* uplo, const La_INT* n, const La_INT* nrhs,
		 const double* ap, const double* afp,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DPPSV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dppsv)(const char* uplo, const La_INT* n,
		const La_INT* nrhs, const double* ap,
		double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DPPSVX - use the Cholesky factorization A = U**T*U or A = */
/* L*L**T to compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dppsvx)(const char* fact, const char* uplo,
		 const La_INT* n, const La_INT* nrhs, double* ap,
		 double* afp, const char* equed, double* s,
		 double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* rcond, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DPPTRF - compute the Cholesky factorization of a real */
/* symmetric positive definite matrix A stored in packed format */
La_extern void
F77_NAME(dpptrf)(const char* uplo, const La_INT* n, double* ap, La_INT* info FCLEN);
/* DPPTRI - compute the inverse of a real symmetric positive */
/* definite matrix A using the Cholesky factorization A = U**T*U */
/* or A = L*L**T computed by DPPTRF  */
La_extern void
F77_NAME(dpptri)(const char* uplo, const La_INT* n, double* ap, La_INT* info FCLEN);
/* DPPTRS - solve a system of linear equations A*X = B with a */
/* symmetric positive definite matrix A in packed storage using */
/* the Cholesky factorization A = U**T*U or A = L*L**T computed by */
/* DPPTRF */
La_extern void
F77_NAME(dpptrs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs, const double* ap,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);

//* Double precision symmetric Positive definite Tridiagonal matrices  -> DPT

/* DPTCON - compute the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric positive definite tridiagonal */
/* matrix using the factorization A = L*D*L**T or A = U**T*D*U */
/* computed by DPTTRF */
La_extern void
F77_NAME(dptcon)(const La_INT* n,
		 const double* d, const double* e,
		 const double* anorm, double* rcond,
		 double* work, La_INT* info);
/* DPTEQR - compute all eigenvalues and, optionally, eigenvectors */
/* of a symmetric positive definite tridiagonal matrix by first */
/* factoring the matrix using DPTTRF, and then calling DBDSQR to */
/* compute the singular values of the bidiagonal factor */
La_extern void
F77_NAME(dpteqr)(const char* compz, const La_INT* n, double* d,
		 double* e, double* z, const La_INT* ldz,
		 double* work, La_INT* info FCLEN);
/* DPTRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric positive */
/* definite and tridiagonal, and provides error bounds and */
/* backward error estimates for the solution */
La_extern void
F77_NAME(dptrfs)(const La_INT* n, const La_INT* nrhs,
		 const double* d, const double* e,
		 const double* df, const double* ef,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* info);
/* DPTSV - compute the solution to a real system of linear */
/* equations A*X = B, where A is an N-by-N symmetric positive */
/* definite tridiagonal matrix, and X and B are N-by-NRHS matrices */
La_extern void
F77_NAME(dptsv)(const La_INT* n, const La_INT* nrhs, double* d,
		double* e, double* b, const La_INT* ldb, La_INT* info);
/* DPTSVX - use the factorization A = L*D*L**T to compute the */
/* solution to a real system of linear equations A*X = B, where A */
/* is an N-by-N symmetric positive definite tridiagonal matrix and */
/* X and B are N-by-NRHS matrices */
La_extern void
F77_NAME(dptsvx)(const La_INT* fact, const La_INT* n,
		 const La_INT* nrhs,
		 const double* d, const double* e,
		 double* df, double* ef,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* rcond,
		 double* ferr, double* berr,
		 double* work, La_INT* info);
/* DPTTRF - compute the factorization of a real symmetric */
/* positive definite tridiagonal matrix A */
La_extern void
F77_NAME(dpttrf)(const La_INT* n, double* d, double* e, La_INT* info);
/* DPTTRS - solve a system of linear equations A * X = B with a */
/* symmetric positive definite tridiagonal matrix A using the */
/* factorization A = L*D*L**T or A = U**T*D*U computed by DPTTRF */
La_extern void
F77_NAME(dpttrs)(const La_INT* n, const La_INT* nrhs,
		 const double* d, const double* e,
		 double* b, const La_INT* ldb, La_INT* info);
/* DRSCL - multiply an n-element real vector x by the real scalar */
/* 1/a */
La_extern void
F77_NAME(drscl)(const La_INT* n, const double* da,
		double* x, const La_INT* incx);

//* Double precision Symmetric Band matrices  -> DSB

/* DSBEV - compute all the eigenvalues and, optionally, */
/* eigenvectors of a real symmetric band matrix A */
La_extern void
F77_NAME(dsbev)(const char* jobz, const char* uplo,
		const La_INT* n, const La_INT* kd,
		double* ab, const La_INT* ldab,
		double* w, double* z, const La_INT* ldz,
		double* work, La_INT* info FCLEN FCLEN);
/* DSBEVD - compute all the eigenvalues and, optionally, */
/* eigenvectors of a real symmetric band matrix A */
La_extern void
F77_NAME(dsbevd)(const char* jobz, const char* uplo,
		 const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab,
		 double* w, double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info FCLEN FCLEN);
/* DSBEVX - compute selected eigenvalues and, optionally, */
/* eigenvectors of a real symmetric band matrix A */
La_extern void
F77_NAME(dsbevx)(const char* jobz, const char* range,
		 const char* uplo, const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab,
		 double* q, const La_INT* ldq,
		 const double* vl, const double* vu,
		 const La_INT* il, const La_INT* iu,
		 const double* abstol,
		 La_INT* m, double* w,
		 double* z, const La_INT* ldz,
		 double* work, La_INT* iwork,
		 La_INT* ifail, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DSBGST - reduce a real symmetric-definite banded generalized */
/* eigenproblem A*x = lambda*B*x to standard form C*y = lambda*y, */
La_extern void
F77_NAME(dsbgst)(const char* vect, const char* uplo,
		 const La_INT* n, const La_INT* ka, const La_INT* kb,
		 double* ab, const La_INT* ldab,
		 double* bb, const La_INT* ldbb,
		 double* x, const La_INT* ldx,
		 double* work, La_INT* info FCLEN FCLEN);
/* DSBGV - compute all the eigenvalues, and optionally, the */
/* eigenvectors of a real generalized symmetric-definite banded */
/* eigenproblem, of the form A*x=(lambda);*B*x */
La_extern void
F77_NAME(dsbgv)(const char* jobz, const char* uplo,
		const La_INT* n, const La_INT* ka, const La_INT* kb,
		double* ab, const La_INT* ldab,
		double* bb, const La_INT* ldbb,
		double* w, double* z, const La_INT* ldz,
		double* work, La_INT* info FCLEN FCLEN);
/* DSBTRD - reduce a real symmetric band matrix A to symmetric */
/* tridiagonal form T by an orthogonal similarity transformation */
La_extern void
F77_NAME(dsbtrd)(const char* vect, const char* uplo,
		 const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab,
		 double* d, double* e,
		 double* q, const La_INT* ldq,
		 double* work, La_INT* info FCLEN FCLEN);

//* Double precision Symmetric Packed matrices  -> DSP

/* DSPCON - estimate the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric packed matrix A using the */
/* factorization A = U*D*U**T or A = L*D*L**T computed by DSPTRF */
La_extern void
F77_NAME(dspcon)(const char* uplo, const La_INT* n,
		 const double* ap, const La_INT* ipiv,
		 const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DSPEV - compute all the eigenvalues and, optionally, */
/* eigenvectors of a real symmetric matrix A in packed storage */
La_extern void
F77_NAME(dspev)(const char* jobz, const char* uplo, const La_INT* n,
		double* ap, double* w, double* z, const La_INT* ldz,
		double* work, La_INT* info FCLEN FCLEN);
/* DSPEVD - compute all the eigenvalues and, optionally, */
/* eigenvectors of a real symmetric matrix A in packed storage */
La_extern void
F77_NAME(dspevd)(const char* jobz, const char* uplo,
		 const La_INT* n, double* ap, double* w,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info FCLEN FCLEN);
/* DSPEVX - compute selected eigenvalues and, optionally, */
/* eigenvectors of a real symmetric matrix A in packed storage */
La_extern void
F77_NAME(dspevx)(const char* jobz, const char* range,
		 const char* uplo, const La_INT* n, double* ap,
		 const double* vl, const double* vu,
		 const La_INT* il, const La_INT* iu,
		 const double* abstol,
		 La_INT* m, double* w,
		 double* z, const La_INT* ldz,
		 double* work, La_INT* iwork,
		 La_INT* ifail, La_INT* info FCLEN FCLEN FCLEN);
/* DSPGST - reduce a real symmetric-definite generalized */
/* eigenproblem to standard form, using packed storage */
La_extern void
F77_NAME(dspgst)(const La_INT* itype, const char* uplo,
		 const La_INT* n, double* ap, double* bp, La_INT* info FCLEN);
/* DSPGV - compute all the eigenvalues and, optionally, the */
/* eigenvectors of a real generalized symmetric-definite */
/* eigenproblem, of the form A*x=(lambda)*B*x, A*Bx=(lambda)*x, */
/* or B*A*x=(lambda)*x */
La_extern void
F77_NAME(dspgv)(const La_INT* itype, const char* jobz,
		const char* uplo, const La_INT* n,
		double* ap, double* bp, double* w,
		double* z, const La_INT* ldz,
		double* work, La_INT* info
#ifndef usePR18534fix
		FCLEN
#endif
		FCLEN FCLEN);

/* DSPRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric indefinite */
/* and packed, and provides error bounds and backward error */
/* estimates for the solution */
La_extern void
F77_NAME(dsprfs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs, const double* ap,
		 const double* afp, const La_INT* ipiv,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);

/* DSPSV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dspsv)(const char* uplo, const La_INT* n,
		const La_INT* nrhs, double* ap, La_INT* ipiv,
		double* b, const La_INT* ldb, La_INT* info FCLEN);

/* DSPSVX - use the diagonal pivoting factorization A = U*D*U**T */
/* or A = L*D*L**T to compute the solution to a real system of */
/* linear equations A * X = B, where A is an N-by-N symmetric */
/* matrix stored in packed format and X and B are N-by-NRHS */
/* matrices */
La_extern void
F77_NAME(dspsvx)(const La_INT* fact, const char* uplo,
		 const La_INT* n, const La_INT* nrhs,
		 const double* ap, double* afp, La_INT* ipiv,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* rcond, double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);

/* DSPTRD - reduce a real symmetric matrix A stored in packed */
/* form to symmetric tridiagonal form T by an orthogonal */
/* similarity transformation */
La_extern void
F77_NAME(dsptrd)(const char* uplo, const La_INT* n,
		 double* ap, double* d, double* e,
		 double* tau, La_INT* info FCLEN);

/* DSPTRF - compute the factorization of a real symmetric matrix */
/* A stored in packed format using the Bunch-Kaufman diagonal */
/* pivoting method */
La_extern void
F77_NAME(dsptrf)(const char* uplo, const La_INT* n,
		 double* ap, La_INT* ipiv, La_INT* info FCLEN);

/* DSPTRI - compute the inverse of a real symmetric indefinite */
/* matrix A in packed storage using the factorization A = U*D*U**T */
/* or A = L*D*L**T computed by DSPTRF */
La_extern void
F77_NAME(dsptri)(const char* uplo, const La_INT* n,
		 double* ap, const La_INT* ipiv,
		 double* work, La_INT* info FCLEN);

/* DSPTRS - solve a system of linear equations A*X = B with a */
/* real symmetric matrix A stored in packed format using the */
/* factorization A = U*D*U**T or A = L*D*L**T computed by DSPTRF */
La_extern void
F77_NAME(dsptrs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs, const double* ap,
		 const La_INT* ipiv, double* b, const La_INT* ldb, 
		 La_INT* info FCLEN);


//* Double precision Symmetric Tridiagonal matrices  -> DST

/* DSTEBZ - compute the eigenvalues of a symmetric tridiagonal */
/* matrix T */
La_extern void
F77_NAME(dstebz)(const char* range, const char* order, const La_INT* n,
		 const double* vl, const double* vu,
		 const La_INT* il, const La_INT* iu,
		 const double *abstol,
		 const double* d, const double* e,
		 La_INT* m, La_INT* nsplit, double* w,
		 La_INT* iblock, La_INT* isplit,
		 double* work, La_INT* iwork,
		 La_INT* info FCLEN FCLEN);
/* DSTEDC - compute all eigenvalues and, optionally, eigenvectors */
/* of a symmetric tridiagonal matrix using the divide and conquer */
/* method */
La_extern void
F77_NAME(dstedc)(const char* compz, const La_INT* n,
		 double* d, double* e,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info FCLEN);
/* DSTEIN - compute the eigenvectors of a real symmetric */
/* tridiagonal matrix T corresponding to specified eigenvalues, */
/* using inverse iteration */
La_extern void
F77_NAME(dstein)(const La_INT* n, const double* d, const double* e,
		 const La_INT* m, const double* w,
		 const La_INT* iblock, const La_INT* isplit,
		 double* z, const La_INT* ldz,
		 double* work, La_INT* iwork,
		 La_INT* ifail, La_INT* info);
/* DSTEQR - compute all eigenvalues and, optionally, eigenvectors */
/* of a symmetric tridiagonal matrix using the implicit QL or QR */
/* method */
La_extern void
F77_NAME(dsteqr)(const char* compz, const La_INT* n, double* d, double* e,
		 double* z, const La_INT* ldz, double* work, La_INT* info FCLEN);
/* DSTERF - compute all eigenvalues of a symmetric tridiagonal */
/* matrix using the Pal-Walker-Kahan variant of the QL or QR */
/* algorithm */
La_extern void
F77_NAME(dsterf)(const La_INT* n, double* d, double* e, La_INT* info);
/* DSTEV - compute all eigenvalues and, optionally, eigenvectors */
/* of a real symmetric tridiagonal matrix A */
La_extern void
F77_NAME(dstev)(const char* jobz, const La_INT* n,
		double* d, double* e,
		double* z, const La_INT* ldz,
		double* work, La_INT* info FCLEN);
/* DSTEVD - compute all eigenvalues and, optionally, eigenvectors */
/* of a real symmetric tridiagonal matrix */
La_extern void
F77_NAME(dstevd)(const char* jobz, const La_INT* n,
		 double* d, double* e,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info FCLEN);
/* DSTEVX - compute selected eigenvalues and, optionally, */
/* eigenvectors of a real symmetric tridiagonal matrix A */
La_extern void
F77_NAME(dstevx)(const char* jobz, const char* range,
		 const La_INT* n, double* d, double* e,
		 const double* vl, const double* vu,
		 const La_INT* il, const La_INT* iu,
		 const double* abstol,
		 La_INT* m, double* w,
		 double* z, const La_INT* ldz,
		 double* work, La_INT* iwork,
		 La_INT* ifail, La_INT* info FCLEN FCLEN);

//* Double precision SYmmetric matrices  -> DSY

/* DSYCON - estimate the reciprocal of the condition number (in */
/* the 1-norm); of a real symmetric matrix A using the */
/* factorization A = U*D*U**T or A = L*D*L**T computed by DSYTRF */
La_extern void
F77_NAME(dsycon)(const char* uplo, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const La_INT* ipiv,
		 const double* anorm, double* rcond,
		 double* work, La_INT* iwork, La_INT* info FCLEN);
/* DSYEV - compute all eigenvalues and, optionally, eigenvectors */
/* of a real symmetric matrix A */
La_extern void
F77_NAME(dsyev)(const char* jobz, const char* uplo,
		const La_INT* n, double* a, const La_INT* lda,
		double* w, double* work, const La_INT* lwork, La_INT* info
		FCLEN FCLEN);
/* DSYEVD - compute all eigenvalues and, optionally, eigenvectors */
/* of a real symmetric matrix A */
La_extern void
F77_NAME(dsyevd)(const char* jobz, const char* uplo,
		 const La_INT* n, double* a, const La_INT* lda,
		 double* w, double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info FCLEN FCLEN);
/* DSYEVX - compute selected eigenvalues and, optionally, */
/* eigenvectors of a real symmetric matrix A */
La_extern void
F77_NAME(dsyevx)(const char* jobz, const char* range,
		 const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda,
		 const double* vl, const double* vu,
		 const La_INT* il, const La_INT* iu,
		 const double* abstol,
		 La_INT* m, double* w,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork, La_INT* iwork,
		 La_INT* ifail, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DSYEVR - compute all eigenvalues and, optionally, eigenvectors   */
/* of a real symmetric matrix A					   */
La_extern void
F77_NAME(dsyevr)(const char* jobz, const char* range, const char* uplo,
		 const La_INT *n, double *a, const La_INT *lda,
		 const double *vl, const double *vu,
		 const La_INT *il, const La_INT *iu,
		 const double *abstol, La_INT *m, double *w,
		 double *z, const La_INT *ldz, La_INT *isuppz,
		 double *work, const La_INT *lwork,
		 La_INT *iwork, const La_INT *liwork,
		 La_INT *info FCLEN FCLEN FCLEN);
/* DSYGS2 - reduce a real symmetric-definite generalized */
/* eigenproblem to standard form */
La_extern void
F77_NAME(dsygs2)(const La_INT* itype, const char* uplo,
		 const La_INT* n, double* a, const La_INT* lda,
		 const double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DSYGST - reduce a real symmetric-definite generalized */
/* eigenproblem to standard form */
La_extern void
F77_NAME(dsygst)(const La_INT* itype, const char* uplo,
		 const La_INT* n, double* a, const La_INT* lda,
		 const double* b, const La_INT* ldb, La_INT* info FCLEN);
/* DSYGV - compute all the eigenvalues, and optionally, the */
/* eigenvectors of a real generalized symmetric-definite */
/* eigenproblem, of the form A*x=(lambda);*B*x, A*Bx=(lambda);*x, */
/* or B*A*x=(lambda);*x */
La_extern void
F77_NAME(dsygv)(const La_INT* itype, const char* jobz,
		const char* uplo, const La_INT* n,
		double* a, const La_INT* lda,
		double* b, const La_INT* ldb,
		double* w, double* work, const La_INT* lwork,
		La_INT ** info FCLEN FCLEN);
/* DSYRFS - improve the computed solution to a system of linear */
/* equations when the coefficient matrix is symmetric indefinite, */
/* and provides error bounds and backward error estimates for the */
/* solution */
La_extern void
F77_NAME(dsyrfs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 const double* af, const La_INT* ldaf,
		 const La_INT* ipiv,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN);

/* DSYSV - compute the solution to a real system of linear */
/* equations  A * X = B, */
La_extern void
F77_NAME(dsysv)(const char* uplo, const La_INT* n,
		const La_INT* nrhs,
		double* a, const La_INT* lda, La_INT* ipiv,
		double* b, const La_INT* ldb,
		double* work, const La_INT* lwork, La_INT* info FCLEN);

/* DSYSVX - use the diagonal pivoting factorization to compute */
/* the solution to a real system of linear equations A * X = B, */
La_extern void
F77_NAME(dsysvx)(const La_INT* fact, const char* uplo,
		 const La_INT* n, const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 double* af, const La_INT* ldaf, La_INT* ipiv,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx, double* rcond,
		 double* ferr, double* berr,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, La_INT* info FCLEN);

/* DSYTD2 - reduce a real symmetric matrix A to symmetric */
/* tridiagonal form T by an orthogonal similarity transformation */
La_extern void
F77_NAME(dsytd2)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda,
		 double* d, double* e, double* tau,
		 La_INT* info FCLEN);

/* DSYTF2 - compute the factorization of a real symmetric matrix */
/* A using the Bunch-Kaufman diagonal pivoting method */
La_extern void
F77_NAME(dsytf2)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda,
		 La_INT* ipiv, La_INT* info FCLEN);

/* DSYTRD - reduce a real symmetric matrix A to real symmetric */
/* tridiagonal form T by an orthogonal similarity transformation */
La_extern void
F77_NAME(dsytrd)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda,
		 double* d, double* e, double* tau,
		 double* work, const La_INT* lwork, La_INT* info FCLEN);

/* DSYTRF - compute the factorization of a real symmetric matrix */
/* A using the Bunch-Kaufman diagonal pivoting method */
La_extern void
F77_NAME(dsytrf)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* ipiv,
		 double* work, const La_INT* lwork, La_INT* info FCLEN);

/* DSYTRI - compute the inverse of a real symmetric indefinite */
/* matrix A using the factorization A = U*D*U**T or A = L*D*L**T */
/* computed by DSYTRF */
La_extern void
F77_NAME(dsytri)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, const La_INT* ipiv,
		 double* work, La_INT* info FCLEN);

/* DSYTRS - solve a system of linear equations A*X = B with a */
/* real symmetric matrix A using the factorization A = U*D*U**T or */
/* A = L*D*L**T computed by DSYTRF */
La_extern void
F77_NAME(dsytrs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 const La_INT* ipiv,
		 double* b, const La_INT* ldb, La_INT* info FCLEN);

//* Double precision Triangular Band matrices  -> DTB

/* DTBCON - estimate the reciprocal of the condition number of a */
/* triangular band matrix A, in either the 1-norm or the */
/* infinity-norm */
La_extern void
F77_NAME(dtbcon)(const char* norm, const char* uplo,
		 const char* diag, const La_INT* n, const La_INT* kd,
		 const double* ab, const La_INT* ldab,
		 double* rcond, double* work,
		 La_INT* iwork, La_INT* info FCLEN FCLEN FCLEN);
/* DTBRFS - provide error bounds and backward error estimates for */
/* the solution to a system of linear equations with a triangular */
/* band coefficient matrix */
La_extern void
F77_NAME(dtbrfs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n, const La_INT* kd,
		 const La_INT* nrhs,
		 const double* ab, const La_INT* ldab,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info FCLEN FCLEN FCLEN);
/* DTBTRS - solve a triangular system of the form   A * X = B or */
/* A**T * X = B,  */
La_extern void
F77_NAME(dtbtrs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n,
		 const La_INT* kd, const La_INT* nrhs,
		 const double* ab, const La_INT* ldab,
		 double* b, const La_INT* ldb, La_INT* info FCLEN FCLEN FCLEN);

//* Double precision Triangular matrices Generalized problems  -> DTG

/* DTGEVC - compute some or all of the right and/or left */
/* generalized eigenvectors of a pair of real upper triangular */
/* matrices (A,B); */
La_extern void
F77_NAME(dtgevc)(const char* side, const char* howmny,
		 const La_LGL* select, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const double* b, const La_INT* ldb,
		 double* vl, const La_INT* ldvl,
		 double* vr, const La_INT* ldvr,
		 const La_INT* mm, La_INT* m, double* work, La_INT* info FCLEN FCLEN);

/* DTGSJA - compute the generalized singular value decomposition */
/* (GSVD); of two real upper triangular (or trapezoidal); matrices */
/* A and B */
La_extern void
F77_NAME(dtgsja)(const char* jobu, const char* jobv, const char* jobq,
		 const La_INT* m, const La_INT* p, const La_INT* n,
		 const La_INT* k, const La_INT* l,
		 double* a, const La_INT* lda,
		 double* b, const La_INT* ldb,
		 const double* tola, const double* tolb,
		 double* alpha, double* beta,
		 double* u, const La_INT* ldu,
		 double* v, const La_INT* ldv,
		 double* q, const La_INT* ldq,
		 double* work, La_INT* ncycle, La_INT* info
		 FCLEN FCLEN FCLEN);

//* Double precision Triangular matrices Packed storage  -> DTP

/* DTPCON - estimate the reciprocal of the condition number of a */
/* packed triangular matrix A, in either the 1-norm or the */
/* infinity-norm */
La_extern void
F77_NAME(dtpcon)(const char* norm, const char* uplo,
		 const char* diag, const La_INT* n,
		 const double* ap, double* rcond,
		 double* work, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);

/* DTPRFS - provide error bounds and backward error estimates for */
/* the solution to a system of linear equations with a triangular */
/* packed coefficient matrix */
La_extern void
F77_NAME(dtprfs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n,
		 const La_INT* nrhs, const double* ap,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DTPTRI - compute the inverse of a real upper or lower */
/* triangular matrix A stored in packed format */
La_extern void
F77_NAME(dtptri)(const char* uplo, const char* diag,
		 const La_INT* n, double* ap, La_INT* info FCLEN FCLEN);

/* DTPTRS - solve a triangular system of the form   A * X = B or */
/* A**T * X = B, */
La_extern void
F77_NAME(dtptrs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n,
		 const La_INT* nrhs, const double* ap,
		 double* b, const La_INT* ldb, La_INT* info
#ifdef usePR18534fix
		 FCLEN		 
#endif
		 FCLEN FCLEN);

//* Double precision TRiangular matrices -> DTR

/* DTRCON - estimate the reciprocal of the condition number of a */
/* triangular matrix A, in either the 1-norm or the infinity-norm */
La_extern void
F77_NAME(dtrcon)(const char* norm, const char* uplo,
		 const char* diag, const La_INT* n,
		 const double* a, const La_INT* lda,
		 double* rcond, double* work,
		 La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);

/* DTREVC - compute some or all of the right and/or left */
/* eigenvectors of a real upper quasi-triangular matrix T */
La_extern void
F77_NAME(dtrevc)(const char* side, const char* howmny,
		 const La_LGL* select, const La_INT* n,
		 const double* t, const La_INT* ldt,
		 double* vl, const La_INT* ldvl,
		 double* vr, const La_INT* ldvr,
		 const La_INT* mm, La_INT* m, double* work, La_INT* info
		 FCLEN FCLEN);

/* DTREXC - reorder the real Schur factorization of a real matrix */
/* A = Q*T*Q**T, so that the diagonal block of T with row index */
/* IFST is moved to row ILST */
La_extern void
F77_NAME(dtrexc)(const char* compq, const La_INT* n,
		 double* t, const La_INT* ldt,
		 double* q, const La_INT* ldq,
		 La_INT* ifst, La_INT* ILST,
		 double* work, La_INT* info FCLEN);

/* DTRRFS - provide error bounds and backward error estimates for */
/* the solution to a system of linear equations with a triangular */
/* coefficient matrix */
La_extern void
F77_NAME(dtrrfs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n, const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 const double* b, const La_INT* ldb,
		 double* x, const La_INT* ldx,
		 double* ferr, double* berr,
		 double* work, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);

/* DTRSEN - reorder the real Schur factorization of a real matrix */
/* A = Q*T*Q**T, so that a selected cluster of eigenvalues appears */
/* in the leading diagonal blocks of the upper quasi-triangular */
/* matrix T, */
La_extern void
F77_NAME(dtrsen)(const char* job, const char* compq,
		 const La_LGL* select, const La_INT* n,
		 double* t, const La_INT* ldt,
		 double* q, const La_INT* ldq,
		 double* wr, double* wi,
		 La_INT* m, double* s, double* sep,
		 double* work, const La_INT* lwork,
		 La_INT* iwork, const La_INT* liwork, La_INT* info 
		 FCLEN FCLEN);

/* DTRSNA - estimate reciprocal condition numbers for specified */
/* eigenvalues and/or right eigenvectors of a real upper */
/* quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q */
/* orthogonal); */
La_extern void
F77_NAME(dtrsna)(const char* job, const char* howmny,
		 const La_LGL* select, const La_INT* n,
		 const double* t, const La_INT* ldt,
		 const double* vl, const La_INT* ldvl,
		 const double* vr, const La_INT* ldvr,
		 double* s, double* sep, const La_INT* mm,
		 La_INT* m, double* work, const La_INT* lwork,
		 La_INT* iwork, La_INT* info FCLEN FCLEN);

/* DTRSYL - solve the real Sylvester matrix equation */
La_extern void
F77_NAME(dtrsyl)(const char* trana, const char* tranb,
		 const La_INT* isgn, const La_INT* m, const La_INT* n,
		 const double* a, const La_INT* lda,
		 const double* b, const La_INT* ldb,
		 double* c, const La_INT* ldc,
		 double* scale, La_INT* info FCLEN FCLEN);

/* DTRTI2 - compute the inverse of a real upper or lower */
/* triangular matrix */
La_extern void
F77_NAME(dtrti2)(const char* uplo, const char* diag,
		 const La_INT* n, double* a, const La_INT* lda,
		 La_INT* info FCLEN FCLEN);

/* DTRTRI - compute the inverse of a real upper or lower */
/* triangular matrix A */
La_extern void
F77_NAME(dtrtri)(const char* uplo, const char* diag,
		 const La_INT* n, double* a, const La_INT* lda,
		 La_INT* info FCLEN FCLEN);

/* DTRTRS - solve a triangular system of the form   A * X = B or */
/* A**T * X = B	 */
La_extern void
F77_NAME(dtrtrs)(const char* uplo, const char* trans,
		 const char* diag, const La_INT* n, const La_INT* nrhs,
		 const double* a, const La_INT* lda,
		 double* b, const La_INT* ldb, La_INT* info
		 FCLEN FCLEN FCLEN);



//* Double precision utilities in Lapack 

/* DHGEQZ - implement a single-/double-shift version of the QZ */
/* method for finding the generalized eigenvalues */
/* w(j);=(ALPHAR(j); + i*ALPHAI(j););/BETAR(j); of the equation */
/* det( A - w(i); B ); = 0  In addition, the pair A,B may be */
/* reduced to generalized Schur form */
La_extern void
F77_NAME(dhgeqz)(const char* job, const char* compq, const char* compz,
		 const La_INT* n, const La_INT *ILO, const La_INT* IHI,
		 double* a, const La_INT* lda,
		 double* b, const La_INT* ldb,
		 double* alphar, double* alphai, const double* beta,
		 double* q, const La_INT* ldq,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DHSEIN - use inverse iteration to find specified right and/or */
/* left eigenvectors of a real upper Hessenberg matrix H */
La_extern void
F77_NAME(dhsein)(const char* side, const char* eigsrc,
		 const char* initv, La_LGL* select,
		 const La_INT* n, double* h, const La_INT* ldh,
		 double* wr, double* wi,
		 double* vl, const La_INT* ldvl,
		 double* vr, const La_INT* ldvr,
		 const La_INT* mm, La_INT* m, double* work,
		 La_INT* ifaill, La_INT* ifailr, La_INT* info
		 FCLEN FCLEN FCLEN);
/* DHSEQR - compute the eigenvalues of a real upper Hessenberg */
/* matrix H and, optionally, the matrices T and Z from the Schur */
/* decomposition H = Z T Z**T, where T is an upper */
/* quasi-triangular matrix (the Schur form);, and Z is the */
/* orthogonal matrix of Schur vectors */
La_extern void
F77_NAME(dhseqr)(const char* job, const char* compz, const La_INT* n,
		 const La_INT* ilo, const La_INT* ihi,
		 double* h, const La_INT* ldh,
		 double* wr, double* wi,
		 double* z, const La_INT* ldz,
		 double* work, const La_INT* lwork, La_INT* info
		 FCLEN FCLEN);
/* DLABAD - take as input the values computed by SLAMCH for */
/* underflow and overflow, and returns the square root of each of */
/* these values if the log of LARGE is sufficiently large */
La_extern void
F77_NAME(dlabad)(double* small, double* large);
/* DLABRD - reduce the first NB rows and columns of a real */
/* general m by n matrix A to upper or lower bidiagonal form by an */
/* orthogonal transformation Q' * A * P, and returns the matrices */
/* X and Y which are needed to apply the transformation to the */
/* unreduced part of A */
La_extern void
F77_NAME(dlabrd)(const La_INT* m, const La_INT* n, const La_INT* nb,
		 double* a, const La_INT* lda, double* d, double* e,
		 double* tauq, double* taup,
		 double* x, const La_INT* ldx, double* y, const La_INT* ldy);
/* DLACON - estimate the 1-norm of a square, real matrix A */
La_extern void
F77_NAME(dlacon)(const La_INT* n, double* v, double* x,
		 La_INT* isgn, double* est, La_INT* kase);
/* DLACPY - copy all or part of a two-dimensional matrix A to */
/* another matrix B */
La_extern void
F77_NAME(dlacpy)(const char* uplo, const La_INT* m, const La_INT* n,
		 const double* a, const La_INT* lda,
		 double* b, const La_INT* ldb FCLEN);
/* DLADIV - perform complex division in real arithmetic	 */
La_extern void
F77_NAME(dladiv)(const double* a, const double* b,
		 const double* c, const double* d,
		 double* p, double* q);
/* DLAE2 - compute the eigenvalues of a 2-by-2 symmetric matrix [ A B ] */
/*								[ B C ] */
La_extern void
F77_NAME(dlae2)(const double* a, const double* b, const double* c,
		double* rt1, double* rt2);
/* DLAEBZ - contain the iteration loops which compute and use the */
/* function N(w);, which is the count of eigenvalues of a */
/* symmetric tridiagonal matrix T less than or equal to its */
/* argument w  */
La_extern void
F77_NAME(dlaebz)(const La_INT* ijob, const La_INT* nitmax, const La_INT* n,
		 const La_INT* mmax, const La_INT* minp, const La_INT* nbmin,
		 const double* abstol, const double* reltol,
		 const double* pivmin, double* d, double* e,
		 double* e2, La_INT* nval, double* ab, double* c,
		 La_INT* mout, La_INT* nab, double* work, La_INT* iwork,
		 La_INT* info);
/* DLAED0 - compute all eigenvalues and corresponding */
/* eigenvectors of a symmetric tridiagonal matrix using the divide */
/* and conquer method */
La_extern void
F77_NAME(dlaed0)(const La_INT* icompq, const La_INT* qsiz, const La_INT* n,
		 double* d, double* e, double* q, const La_INT* ldq,
		 double* qstore, const La_INT* ldqs,
		 double* work, La_INT* iwork, La_INT* info);
/* DLAED1 - compute the updated eigensystem of a diagonal matrix */
/* after modification by a rank-one symmetric matrix */
La_extern void
F77_NAME(dlaed1)(const La_INT* n, double* d, double* q, const La_INT* ldq,
		 La_INT* indxq, const double* rho, const La_INT* cutpnt,
		 double* work, La_INT* iwork, La_INT* info);
/* DLAED2 - merge the two sets of eigenvalues together La_INT *o a */
/* single sorted set */
La_extern void
F77_NAME(dlaed2)(const La_INT* k, const La_INT* n, double* d,
		 double* q, const La_INT* ldq, La_INT* indxq,
		 double* rho, double* z,
		 double* dlamda, double* w, double* q2,
		 La_INT* indx, La_INT* indxc, La_INT* indxp,
		 La_INT* coltyp, La_INT* info);
/* DLAED3 - find the roots of the secular equation, as defined by */
/* the values in double* d, W, and RHO, between KSTART and KSTOP */
La_extern void
F77_NAME(dlaed3)(const La_INT* k, const La_INT* n, const La_INT* n1,
		 double* d, double* q, const La_INT* ldq,
		 const double* rho, double* dlamda, double* q2, 
		 La_INT* indx, La_INT* ctot, double* w,
		 double* s, La_INT* info);
/* DLAED4 - subroutine computes the I-th updated eigenvalue of a */
/* symmetric rank-one modification to a diagonal matrix whose */
/* elements are given in the array d, and that	 D(i); < D(j); for */
/* i < j  and that RHO > 0 */
La_extern void
F77_NAME(dlaed4)(const La_INT* n, const La_INT* i, const double* d,
		 const double* z, const double* delta,
		 const double* rho, double* dlam, La_INT* info);
/* DLAED5 - subroutine computes the I-th eigenvalue of a */
/* symmetric rank-one modification of a 2-by-2 diagonal matrix */
/* diag( D ); + RHO  The diagonal elements in the array D are */
/* assumed to satisfy	D(i); < D(j); for i < j	 */
La_extern void
F77_NAME(dlaed5)(const La_INT* i, const double* d, const double* z,
		 double* delta, const double* rho, double* dlam);
/* DLAED6 - compute the positive or negative root (closest to the */
/* origin); of	z(1); z(2); z(3); f(x); = rho + --------- + */
/* ---------- + ---------  d(1);-x d(2);-x d(3);-x  It is assumed */
/* that	  if ORGATI = .true  */
La_extern void
F77_NAME(dlaed6)(const La_INT* kniter, const La_INT* orgati,
		 const double* rho, const double* d,
		 const double* z, const double* finit,
		 double* tau, La_INT* info);
/* DLAED7 - compute the updated eigensystem of a diagonal matrix */
/* after modification by a rank-one symmetric matrix */
La_extern void
F77_NAME(dlaed7)(const La_INT* icompq, const La_INT* n,
		 const La_INT* qsiz, const La_INT* tlvls,
		 const La_INT* curlvl, const La_INT* curpbm,
		 double* d, double* q, const La_INT* ldq,
		 La_INT* indxq, const double* rho, const La_INT* cutpnt,
		 double* qstore, double* qptr, const La_INT* prmptr,
		 const La_INT* perm, const La_INT* givptr,
		 const La_INT* givcol, const double* givnum,
		 double* work, La_INT* iwork, La_INT* info);
/* DLAED8 - merge the two sets of eigenvalues together La_INT *o a */
/* single sorted set */
La_extern void
F77_NAME(dlaed8)(const La_INT* icompq, const La_INT* k,
		 const La_INT* n, const La_INT* qsiz,
		 double* d, double* q, const La_INT* ldq,
		 const La_INT* indxq, double* rho,
		 const La_INT* cutpnt, const double* z,
		 double* dlamda, double* q2, const La_INT* ldq2,
		 double* w, La_INT* perm, La_INT* givptr,
		 La_INT* givcol, double* givnum, La_INT* indxp,
		 La_INT* indx, La_INT* info);
/* DLAED9 - find the roots of the secular equation, as defined by */
/* the values in double* d, Z, and RHO, between KSTART and KSTOP */
La_extern void
F77_NAME(dlaed9)(const La_INT* k, const La_INT* kstart, const La_INT* kstop,
		 const La_INT* n, double* d, double* q, const La_INT* ldq,
		 const double* rho, const double* dlamda,
		 const double* w, double* s, const La_INT* lds, La_INT* info);
/* DLAEDA - compute the Z vector corresponding to the merge step */
/* in the CURLVLth step of the merge process with TLVLS steps for */
/* the CURPBMth problem */
La_extern void
F77_NAME(dlaeda)(const La_INT* n, const La_INT* tlvls, const La_INT* curlvl,
		 const La_INT* curpbm, const La_INT* prmptr, const La_INT* perm,
		 const La_INT* givptr, const La_INT* givcol,
		 const double* givnum, const double* q,
		 const La_INT* qptr, double* z, double* ztemp, La_INT* info);
/* DLAEIN - use inverse iteration to find a right or left */
/* eigenvector corresponding to the eigenvalue (WR,WI); of a real */
/* upper Hessenberg matrix H */
La_extern void
F77_NAME(dlaein)(const La_INT* rightv, const La_INT* noinit, const La_INT* n,
		 const double* h, const La_INT* ldh,
		 const double* wr, const double* wi,
		 double* vr, double* vi,
		 double* b, const La_INT* ldb, double* work,
		 const double* eps3, const double* smlnum,
		 const double* bignum, La_INT* info);
/* DLAEV2 - compute the eigendecomposition of a 2-by-2 symmetric */
/* matrix  [ A B ]  [ B C ] */
La_extern void
F77_NAME(dlaev2)(const double* a, const double* b, const double* c,
		 double* rt1, double* rt2, double* cs1, double *sn1);
/* DLAEXC - swap adjacent diagonal blocks T11 and T22 of order 1 */
/* or 2 in an upper quasi-triangular matrix T by an orthogonal */
/* similarity transformation */
La_extern void
F77_NAME(dlaexc)(const La_INT* wantq, const La_INT* n, double* t, const La_INT* ldt,
		  double* q, const La_INT* ldq, const La_INT* j1,
		 const La_INT* n1, const La_INT* n2, double* work, La_INT* info);
/* DLAG2 - compute the eigenvalues of a 2 x 2 generalized */
/* eigenvalue problem A - w B, with scaling as necessary to aextern void */
/* over-/underflow */
La_extern void
F77_NAME(dlag2)(const double* a, const La_INT* lda, const double* b,
		const La_INT* ldb, const double* safmin,
		double* scale1, double* scale2,
		double* wr1, double* wr2, double* wi);
/* DLAGS2 - compute 2-by-2 orthogonal matrices U, V and Q, such */
/* that if ( UPPER ); then   U'*A*Q = U'*( A1 A2 );*Q = ( x 0 ); */
/* ( 0 A3 ); ( x x ); and  V'*B*Q = V'*( B1 B2 );*Q = ( x 0 );	( */
/* 0 B3 ); ( x x );  or if ( .NOT.UPPER ); then	  U'*A*Q = U'*( A1 */
/* 0 );*Q = ( x x );  ( A2 A3 ); ( 0 x ); and  V'*B*Q = V'*( B1 0 */
/* );*Q = ( x x );  ( B2 B3 ); ( 0 x );	 The rows of the */
/* transformed A and B are parallel, where   U = ( CSU SNU );, V = */
/* ( CSV SNV );, Q = ( CSQ SNQ );  ( -SNU CSU ); ( -SNV CSV ); ( */
/* -SNQ CSQ );	Z' denotes the transpose of Z */
La_extern void
F77_NAME(dlags2)(const La_INT* upper,
		 const double* a1, const double* a2, const double* a3,
		 const double* b1, const double* b2, const double* b3,
		 double* csu, double* snu,
		 double* csv, double* snv, double *csq, double *snq);
/* DLAGTF - factorize the matrix (T - lambda*I);, where T is an n */
/* by n tridiagonal matrix and lambda is a scalar, as	T - */
/* lambda*I = PLU, */
La_extern void
F77_NAME(dlagtf)(const La_INT* n, double* a, const double* lambda,
		 double* b, double* c, const double *tol,
		 double* d, La_INT* in, La_INT* info);
/* DLAGTM - perform a matrix-vector product of the form	  B := */
/* alpha * A * X + beta * B  where A is a tridiagonal matrix of */
/* order N, B and X are N by NRHS matrices, and alpha and beta are */
/* real scalars, each of which may be 0., 1., or -1 */
La_extern void
F77_NAME(dlagtm)(const char* trans, const La_INT* n, const La_INT* nrhs,
		 const double* alpha, const double* dl,
		 const double* d, const double* du,
		 const double* x, const La_INT* ldx, const double* beta,
		 double* b, const La_INT* ldb FCLEN);
/* DLAGTS - may be used to solve one of the systems of equations */
/* (T - lambda*I);*x = y or (T - lambda*I);'*x = y, */
La_extern void
F77_NAME(dlagts)(const La_INT* job, const La_INT* n,
		 const double* a, const double* b,
		 const double* c, const double* d,
		 const La_INT* in, double* y, double* tol, La_INT* info);
/* DLAHQR - an auxiliary routine called by DHSEQR to update the */
/* eigenvalues and Schur decomposition already computed by DHSEQR, */
/* by dealing with the Hessenberg submatrix in rows and columns */
/* ILO to IHI */
La_extern void
F77_NAME(dlahqr)(const La_INT* wantt, const La_INT* wantz, const La_INT* n,
		 const La_INT* ilo, const La_INT* ihi,
		 double* H, const La_INT* ldh, double* wr, double* wi,
		 const La_INT* iloz, const La_INT* ihiz,
		 double* z, const La_INT* ldz, La_INT* info);
/* DLAIC1 - apply one step of incremental condition estimation in */
/* its simplest version */
La_extern void
F77_NAME(dlaic1)(const La_INT* job, const La_INT* j, const double* x,
		 const double* sest, const double* w,
		 const double* gamma, double* sestpr,
		 double* s, double* c);
/* DLALN2 - solve a system of the form (ca A - w D ); X = s B or */
/* (ca A' - w D); X = s B with possible scaling ("s"); and */
/* perturbation of A */
La_extern void
F77_NAME(dlaln2)(const La_INT* ltrans, const La_INT* na, const La_INT* nw,
		 const double* smin, const double* ca,
		 const double* a, const La_INT* lda,
		 const double* d1, const double* d2,
		 const double* b, const La_INT* ldb,
		 const double* wr, const double* wi,
		 double* x, const La_INT* ldx, double* scale,
		 double* xnorm, La_INT* info);
/* DLAMCH - determine double precision machine parameters */
La_extern double
F77_NAME(dlamch)(const char* cmach FCLEN);
/* DLAMRG - will create a permutation list which will merge the */
/* elements of A (which is composed of two independently sorted */
/* sets); La_INT *o a single set which is sorted in ascending order */
La_extern void
F77_NAME(dlamrg)(const La_INT* n1, const La_INT* n2, const double* a,
		 const La_INT* dtrd1, const La_INT* dtrd2, La_INT* index);
/* DLANGB - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of an n by n band matrix A, with kl sub-diagonals and ku */
/* super-diagonals */
La_extern double
F77_NAME(dlangb)(const char* norm, const La_INT* n,
		 const La_INT* kl, const La_INT* ku, const double* ab,
		 const La_INT* ldab, double* work FCLEN);
/* DLANGE - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real matrix A */
La_extern double
F77_NAME(dlange)(const char* norm, const La_INT* m, const La_INT* n,
		 const double* a, const La_INT* lda, double* work FCLEN);
/* DLANGT - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real tridiagonal matrix A */
La_extern double
F77_NAME(dlangt)(const char* norm, const La_INT* n,
		 const double* dl, const double* d,
		 const double* du FCLEN);
/* DLANHS - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a Hessenberg matrix A */
La_extern double
F77_NAME(dlanhs)(const char* norm, const La_INT* n,
		 const double* a, const La_INT* lda, double* work FCLEN);
/* DLANSB - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of an n by n symmetric band matrix A, with k */
/* super-diagonals */
La_extern double
F77_NAME(dlansb)(const char* norm, const char* uplo,
		 const La_INT* n, const La_INT* k,
		 const double* ab, const La_INT* ldab, double* work
		 FCLEN FCLEN);
/* DLANSP - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real symmetric matrix A, supplied in packed form */
La_extern double
F77_NAME(dlansp)(const char* norm, const char* uplo,
		 const La_INT* n, const double* ap, double* work
		 FCLEN FCLEN);
/* DLANST - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real symmetric tridiagonal matrix A */
La_extern double
F77_NAME(dlanst)(const char* norm, const La_INT* n,
		 const double* d, const double* e FCLEN);
/* DLANSY - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real symmetric matrix A */
La_extern double
F77_NAME(dlansy)(const char* norm, const char* uplo, const La_INT* n,
		 const double* a, const La_INT* lda, double* work
		 FCLEN FCLEN);
/* DLANTB - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of an n by n triangular band matrix A, with ( k + 1 ) diagonals */
La_extern double
F77_NAME(dlantb)(const char* norm, const char* uplo,
		 const char* diag, const La_INT* n, const La_INT* k,
		 const double* ab, const La_INT* ldab, double* work
		 FCLEN FCLEN FCLEN);
/* DLANTP - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a triangular matrix A, supplied in packed form */
La_extern double
F77_NAME(dlantp)(const char* norm, const char* uplo, const char* diag,
		 const La_INT* n, const double* ap, double* work
		 FCLEN FCLEN FCLEN);
/* DLANTR - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a trapezoidal or triangular matrix A */
La_extern double
F77_NAME(dlantr)(const char* norm, const char* uplo,
		 const char* diag, const La_INT* m, const La_INT* n,
		 const double* a, const La_INT* lda, double* work
		 FCLEN FCLEN FCLEN);
/* DLANV2 - compute the Schur factorization of a real 2-by-2 */
/* nonsymmetric matrix in standard form */
La_extern void
F77_NAME(dlanv2)(double* a, double* b, double* c, double* d,
		 double* rt1r, double* rt1i, double* rt2r, double* rt2i,
		 double* cs, double *sn);
/* DLAPLL - two column vectors X and Y, let A = ( X Y ); */
La_extern void
F77_NAME(dlapll)(const La_INT* n, double* x, const La_INT* incx,
		 double* y, const La_INT* incy, double* ssmin);
/* DLAPMT - rearrange the columns of the M by N matrix X as */
/* specified by the permutation K(1);,K(2);,...,K(N); of the */
/* La_INT *egers 1,...,N */
La_extern void
F77_NAME(dlapmt)(const La_LGL* forwrd, const La_INT* m, const La_INT* n,
		 double* x, const La_INT* ldx, const La_INT* k);
/* DLAPY2 - return sqrt(x**2+y**2);, taking care not to cause */
/* unnecessary overflow */
La_extern double
F77_NAME(dlapy2)(const double* x, const double* y);
/* DLAPY3 - return sqrt(x**2+y**2+z**2);, taking care not to */
/* cause unnecessary overflow */
La_extern double
F77_NAME(dlapy3)(const double* x, const double* y, const double* z);
/* DLAQGB - equilibrate a general M by N band matrix A with KL */
/* subdiagonals and KU superdiagonals using the row and scaling */
/* factors in the vectors R and C */
La_extern void
F77_NAME(dlaqgb)(const La_INT* m, const La_INT* n,
		 const La_INT* kl, const La_INT* ku,
		 double* ab, const La_INT* ldab,
		 double* r, double* c,
		 double* rowcnd, double* colcnd,
		 const double* amax, const char* equed FCLEN);
/* DLAQGE - equilibrate a general M by N matrix A using the row */
/* and scaling factors in the vectors R and C */
La_extern void
F77_NAME(dlaqge)(const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda,
		 double* r, double* c,
		 double* rowcnd, double* colcnd,
		 const double* amax, const char* equed FCLEN);
/* DLAQSB - equilibrate a symmetric band matrix A using the */
/* scaling factors in the vector S */
La_extern void
F77_NAME(dlaqsb)(const char* uplo, const La_INT* n, const La_INT* kd,
		 double* ab, const La_INT* ldab, const double* s,
		 const double* scond, const double* amax, 
		 const char* equed FCLEN FCLEN);
/* DLAQSP - equilibrate a symmetric matrix A using the scaling */
/* factors in the vector S */
La_extern void
F77_NAME(dlaqsp)(const char* uplo, const La_INT* n,
		 double* ap, const double* s, const double* scond,
		 const double* amax, La_INT* equed FCLEN);
/* DLAQSY - equilibrate a symmetric matrix A using the scaling */
/* factors in the vector S */
La_extern void
F77_NAME(dlaqsy)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda,
		 const double* s, const double* scond,
		 const double* amax, La_INT* equed FCLEN);
/* DLAQTR - solve the real quasi-triangular system   */
/* op(T) * p = scale*c */
La_extern void
F77_NAME(dlaqtr)(const La_INT* ltran, const La_INT* lreal, const La_INT* n,
		 const double* t, const La_INT* ldt,
		 const double* b, const double* w,
		 double* scale, double* x, double* work, La_INT* info);
/* DLAR2V - apply a vector of real plane rotations from both */
/* sides to a sequence of 2-by-2 real symmetric matrices, defined */
/* by the elements of the vectors x, y and z  */
La_extern void
F77_NAME(dlar2v)(const La_INT* n, double* x, double* y,
		 double* z, const La_INT* incx,
		 const double* c, const double* s,
		 const La_INT* incc);
/* DLARF - apply a real elementary reflector H to a real m by n */
/* matrix C, from either the left or the right */
La_extern void
F77_NAME(dlarf)(const char* side, const La_INT* m, const La_INT* n,
		const double* v, const La_INT* incv, const double* tau,
		double* c, const La_INT* ldc, double* work FCLEN);
/* DLARFB - apply a real block reflector H or its transpose H' */
/* to a real m by n matrix C, from either the left or the right */
La_extern void
F77_NAME(dlarfb)(const char* side, const char* trans,
		 const char* direct, const char* storev,
		 const La_INT* m, const La_INT* n, const La_INT* k,
		 const double* v, const La_INT* ldv,
		 const double* t, const La_INT* ldt,
		 double* c, const La_INT* ldc,
		 double* work, const La_INT* lwork
		 FCLEN FCLEN FCLEN FCLEN);
/* DLARFG - generate a real elementary reflector H of order n, */
/* such that   H * ( alpha ) = ( beta ), H' * H = I */
La_extern void
F77_NAME(dlarfg)(const La_INT* n, const double* alpha,
		 double* x, const La_INT* incx, double* tau);
/* DLARFT - form the triangular factor T of a real block */
/* reflector H of order n, which is defined as a product of k */
/* elementary reflectors */
La_extern void
F77_NAME(dlarft)(const char* direct, const char* storev,
		 const La_INT* n, const La_INT* k, double* v, const La_INT* ldv,
		 const double* tau, double* t, const La_INT* ldt FCLEN FCLEN);
/* DLARFX - apply a real elementary reflector H to a real m by n */
/* matrix C, from either the left or the right */
La_extern void
F77_NAME(dlarfx)(const char* side, const La_INT* m, const La_INT* n,
		 const double* v, const double* tau,
		 double* c, const La_INT* ldc, double* work FCLEN);
/* DLARGV - generate a vector of real plane rotations, determined */
/* by elements of the real vectors x and y */
La_extern void
F77_NAME(dlargv)(const La_INT* n, double* x, const La_INT* incx,
		 double* y, const La_INT* incy, double* c, const La_INT* incc);
/* DLARNV - return a vector of n random real numbers from a */
/* uniform or normal distribution */
La_extern void
F77_NAME(dlarnv)(const La_INT* idist, La_INT* iseed, const La_INT* n, double* x);
/* DLARTG - generate a plane rotation so that	[ CS SN ]  */
La_extern void
F77_NAME(dlartg)(const double* f, const double* g, double* cs,
		 double* sn, double *r);
/* DLARTV - apply a vector of real plane rotations to elements of */
/* the real vectors x and y */
La_extern void
F77_NAME(dlartv)(const La_INT* n, double* x, const La_INT* incx,
		 double* y, const La_INT* incy,
		 const double* c, const double* s,
		 const La_INT* incc);
/* DLARUV - return a vector of n random real numbers from a */
/* uniform (0,1); */
La_extern void
F77_NAME(dlaruv)(La_INT* iseed, const La_INT* n, double* x);

/* DLAS2 - compute the singular values of the 2-by-2 matrix */
/* [ F G ]  [ 0 H ] */
La_extern void
F77_NAME(dlas2)(const double* f, const double* g, const double* h,
		 double* ssmin, double* ssmax);

/* DLASCL - multiply the M by N real matrix A by the real scalar */
/* CTO/CFROM */
La_extern void
F77_NAME(dlascl)(const char* type,
		 const La_INT* kl,const La_INT* ku,
		 double* cfrom, double* cto,
		 const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);

/* DLASET - initialize an m-by-n matrix A to BETA on the diagonal */
/* and ALPHA on the offdiagonals */
La_extern void
F77_NAME(dlaset)(const char* uplo, const La_INT* m, const La_INT* n,
		 const double* alpha, const double* beta,
		 double* a, const La_INT* lda FCLEN);
/* DLASQ1 - DLASQ1 computes the singular values of a real N-by-N */
/* bidiagonal  matrix with diagonal D and off-diagonal E */
La_extern void
F77_NAME(dlasq1)(const La_INT* n, double* d, double* e,
		 double* work, La_INT* info);
/* DLASQ2 - DLASQ2 computes the singular values of a real N-by-N */
/* unreduced  bidiagonal matrix with squared diagonal elements in */
/* Q and  squared off-diagonal elements in E */
La_extern void
F77_NAME(dlasq2)(const La_INT* m, double* q, double* e,
		 double* qq, double* ee, const double* eps,
		 const double* tol2, const double* small2,
		 double* sup, La_INT* kend, La_INT* info);
/* DLASQ3 - DLASQ3 is the workhorse of the whole bidiagonal SVD */
/* algorithm */
La_extern void
F77_NAME(dlasq3)(La_INT* n, double* q, double* e, double* qq,
		 double* ee, double* sup, double *sigma,
		 La_INT* kend, La_INT* off, La_INT* iphase,
		 const La_INT* iconv, const double* eps,
		 const double* tol2, const double* small2);
/* DLASQ4 - DLASQ4 estimates TAU, the smallest eigenvalue of a */
/* matrix */
La_extern void
F77_NAME(dlasq4)(const La_INT* n, const double* q, const double* e,
		 double* tau, double* sup);
/* DLASR - perform the transformation	A := P*A, when SIDE = 'L' */
/* or 'l' ( Left-hand side );	A := A*P', when SIDE = 'R' or 'r' */
/* ( Right-hand side );	 where A is an m by n real matrix and P is */
/* an orthogonal matrix, */
La_extern void
F77_NAME(dlasr)(const char* side, const char* pivot,
		const char* direct, const La_INT* m, const La_INT* n,
		const double* c, const double* s,
		double* a, const La_INT* lda FCLEN FCLEN FCLEN);
/* DLASRT - the numbers in D in increasing order (if ID = 'I'); */
/* or in decreasing order (if ID = 'D' ); */
La_extern void
F77_NAME(dlasrt)(const char* id, const La_INT* n, double* d, La_INT* info FCLEN);
/* DLASSQ - return the values scl and smsq such that   ( scl**2 */
/* );*smsq = x( 1 );**2 +...+ x( n );**2 + ( scale**2 );*sumsq, */
La_extern void
F77_NAME(dlassq)(const La_INT* n, const double* x, const La_INT* incx,
		 double* scale, double* sumsq);
/* DLASV2 - compute the singular value decomposition of a 2-by-2 */
/* triangular matrix  [ F G ]  [ 0 H ] */
La_extern void
F77_NAME(dlasv2)(const double* f, const double* g, const double* h,
		 double* ssmin, double* ssmax, double* snr, double* csr,
		 double* snl, double* csl);
/* DLASWP - perform a series of row La_INT *erchanges on the matrix A */
La_extern void
F77_NAME(dlaswp)(const La_INT* n, double* a, const La_INT* lda,
		 const La_INT* k1, const La_INT* k2,
		 const La_INT* ipiv, const La_INT* incx);
/* DLASY2 - solve for the N1 by N2 matrix double* x, 1 <= N1,N2 <= 2, in */
/* op(TL);*X + ISGN*X*op(TR); = SCALE*B, */
La_extern void
F77_NAME(dlasy2)(const La_INT* ltranl, const La_INT* ltranr,
		 const La_INT* isgn, const La_INT* n1, const La_INT* n2,
		 const double* tl, const La_INT* ldtl,
		 const double* tr, const La_INT* ldtr,
		 const double* b, const La_INT* ldb,
		 double* scale, double* x, const La_INT* ldx,
		 double* xnorm, La_INT* info);
/* DLASYF - compute a partial factorization of a real symmetric */
/* matrix A using the Bunch-Kaufman diagonal pivoting method */
La_extern void
F77_NAME(dlasyf)(const char* uplo, const La_INT* n,
		 const La_INT* nb, const La_INT* kb,
		 double* a, const La_INT* lda, La_INT* ipiv,
		 double* w, const La_INT* ldw, La_INT* info FCLEN);
/* DLATBS - solve one of the triangular systems	  A *x = s*b or */
/* A'*x = s*b  with scaling to prevent overflow, where A is an */
/* upper or lower triangular band matrix */
La_extern void
F77_NAME(dlatbs)(const char* uplo, const char* trans,
		 const char* diag, const char* normin,
		 const La_INT* n, const La_INT* kd,
		 const double* ab, const La_INT* ldab,
		 double* x, double* scale, double* cnorm, La_INT* info
		 FCLEN FCLEN FCLEN FCLEN);
/* DLATPS - solve one of the triangular systems	  A *x = s*b or */
/* A'*x = s*b  with scaling to prevent overflow, where A is an */
/* upper or lower triangular matrix stored in packed form */
La_extern void
F77_NAME(dlatps)(const char* uplo, const char* trans,
		 const char* diag, const char* normin,
		 const La_INT* n, const double* ap,
		 double* x, double* scale, double* cnorm, La_INT* info
		 FCLEN FCLEN FCLEN FCLEN);
/* DLATRD - reduce NB rows and columns of a real symmetric matrix */
/* A to symmetric tridiagonal form by an orthogonal similarity */
/* transformation Q' * A * Q, and returns the matrices V and W */
/* which are needed to apply the transformation to the unreduced */
/* part of A */
La_extern void
F77_NAME(dlatrd)(const char* uplo, const La_INT* n, const La_INT* nb,
		 double* a, const La_INT* lda, double* e, double* tau,
		 double* w, const La_INT* ldw FCLEN);
/* DLATRS - solve one of the triangular systems	  A *x = s*b or */
/* A'*x = s*b  with scaling to prevent overflow */
La_extern void
F77_NAME(dlatrs)(const char* uplo, const char* trans,
		 const char* diag, const char* normin,
		 const La_INT* n, const double* a, const La_INT* lda,
		 double* x, double* scale, double* cnorm, La_INT* info
		 FCLEN FCLEN FCLEN FCLEN );
/* DLAUU2 - compute the product U * U' or L' * const La_INT* l, where the */
/* triangular factor U or L is stored in the upper or lower */
/* triangular part of the array A */
La_extern void
F77_NAME(dlauu2)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);
/* DLAUUM - compute the product U * U' or L' * L, where the */
/* triangular factor U or L is stored in the upper or lower */
/* triangular part of the array A */
La_extern void
F77_NAME(dlauum)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* info FCLEN);

/* ======================================================================== */


//* Selected Double Complex Lapack Routines
/*  ========
 */

/* IZMAX1 finds the index of the element whose real part has maximum
 * absolute value. Returns Fortran INTEGER. */
La_extern La_INT *
F77_NAME(izmax1)(const La_INT *n, La_complex *cx, const La_INT *incx);


/*  ZGECON estimates the reciprocal of the condition number of a general
 *  complex matrix A, in either the 1-norm or the infinity-norm, using
 *  the LU factorization computed by ZGETRF.
 */
La_extern void
F77_NAME(zgecon)(const char* norm, const La_INT *n,
		 const La_complex *a, const La_INT *lda,
		 const double *anorm, double *rcond,
		 La_complex *work, double *rwork, La_INT *info FCLEN);

/* ZGESV computes the solution to a complex system of linear equations */
La_extern void
F77_NAME(zgesv)(const La_INT *n, const La_INT *nrhs, La_complex *a,
		const La_INT *lda, La_INT *ipiv, La_complex *b,
		const La_INT *ldb, La_INT *info);

/*  ZGEQP3 computes a QR factorization with column pivoting */
La_extern void
F77_NAME(zgeqp3)(const La_INT *m, const La_INT *n,
		 La_complex *a, const La_INT *lda,
		 La_INT *jpvt, La_complex *tau,
		 La_complex *work, const La_INT *lwork,
		 double *rwork, La_INT *info);

/* ZUNMQR applies Q or Q**H from the Left or Right */
La_extern void
F77_NAME(zunmqr)(const char* side, const char* trans,
		 const La_INT *m, const La_INT *n, const La_INT *k,
		 La_complex *a, const La_INT *lda,
		 La_complex *tau,
		 La_complex *c, const La_INT *ldc,
		 La_complex *work, const La_INT *lwork, La_INT *info FCLEN FCLEN);

/*  ZTRTRS solves triangular systems */
La_extern void
F77_NAME(ztrtrs)(const char* uplo, const char* trans, const char* diag,
		 const La_INT *n, const La_INT *nrhs,
		 La_complex *a, const La_INT *lda,
		 La_complex *b, const La_INT *ldb, La_INT *info
		 FCLEN FCLEN FCLEN);
/* ZGESVD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or	   */
/* right singular vectors					   */
La_extern void
F77_NAME(zgesvd)(const char* jobu, const char* jobvt,
		 const La_INT *m, const La_INT *n,
		 La_complex *a, const La_INT *lda, double *s,
		 La_complex *u, const La_INT *ldu,
		 La_complex *vt, const La_INT *ldvt,
		 La_complex *work, const La_INT *lwork, double *rwork,
		 La_INT *info FCLEN FCLEN);

/* ZGHEEV - compute all eigenvalues and, optionally, eigenvectors */
/* of a Hermitian matrix A */
La_extern void
F77_NAME(zheev)(const char* jobz, const char* uplo,
		const La_INT *n, La_complex *a, const La_INT *lda,
		double *w, La_complex *work, const La_INT *lwork,
		double *rwork, La_INT *info FCLEN FCLEN);

/* ZGGEEV - compute all eigenvalues and, optionally, eigenvectors */
/* of a complex non-symmetric matrix A */
La_extern void
F77_NAME(zgeev)(const char* jobvl, const char* jobvr,
		const La_INT *n, La_complex *a, const La_INT *lda,
		La_complex *wr, La_complex *vl, const La_INT *ldvl,
		La_complex *vr, const La_INT *ldvr,
		La_complex *work, const La_INT *lwork,
		double *rwork, La_INT *info FCLEN FCLEN);

/*  ZLACN2 estimates the 1-norm of a square, complex matrix A.
 *  Reverse communication is used for evaluating matrix-vector products.
 * Added in R 2.7.0
*/
La_extern void
F77_NAME(zlacn2)(const La_INT *n, La_complex *v, La_complex *x,
                 double *est, La_INT *kase, La_INT *isave);

/*  ZLANSP estimates the 1-norm of a square, complex matrix A.
 *  Reverse communication is used for evaluating matrix-vector products.
 * Added in R 4.4.0
*/
La_extern double
F77_NAME(zlansp)(const char* norm, const char* uplo,
		 const La_INT *n, La_complex *ap,
		 double *work FCLEN FCLEN);
La_extern double
F77_NAME(zlansy)(const char* norm, const char* uplo,
		 const La_INT *n, La_complex *a, const La_INT *lda,
		 double *work FCLEN FCLEN);

/* ZLANTR  -  return the value of the one norm, or the Frobenius norm,
 * or the infinity norm, or the element of largest absolute value of
 * a trapezoidal or triangular matrix A.
 * Added in R 2.7.0
*/
La_extern double
F77_NAME(zlantr)(const char* norm, const char* uplo, const char* diag,
		 const La_INT *m, const La_INT *n, La_complex *a,
		 const La_INT *lda, double *work FCLEN FCLEN FCLEN);

/* DZSUM1 - take the sum of the absolute values of a complex
 * vector and returns a double precision result
 * Added in R 2.15.2
*/
La_extern double
F77_NAME(dzsum1)(const La_INT *n, La_complex *CX, const La_INT *incx);

/* Added in R 3.6.2, 4.4.0 -- now alphabetically */
La_extern void
F77_NAME(zpotrf)(const char* uplo, const La_INT* n,
		 La_complex* a, const La_INT* lda, La_INT* info FCLEN);
La_extern void
F77_NAME(zpotrf2)(const char* uplo, const La_INT* n,
		 La_complex* a, const La_INT* lda, La_INT* info FCLEN);

La_extern void
F77_NAME(zpotri)(const char* uplo, const La_INT* n,
		 La_complex* a, const La_INT* lda, La_INT* info FCLEN);

La_extern void
F77_NAME(zpotrs)(const char* uplo, const La_INT* n,
		 const La_INT* nrhs, const La_complex* a, const La_INT* lda,
		 La_complex* b, const La_INT* ldb, La_INT* info FCLEN);

La_extern void
F77_NAME(zppcon)(const char* uplo, const La_INT* n,
		 const La_complex* ap,
		 const double* anorm, double* rcond,
		 La_complex* work, double* rwork, La_INT* info FCLEN);

La_extern void
F77_NAME(zpptrf)(const char* uplo, const La_INT* n,
		 La_complex *ap, La_INT *info FCLEN);

La_extern void
F77_NAME(zpptri)(const char* uplo, const La_INT* n,
		 La_complex *ap, La_INT *info FCLEN);

La_extern void
F77_NAME(zpptrs)(const char* uplo, const La_INT* n,
		 const La_INT *nrhs, const La_complex *a,
		 La_complex* b, const La_INT* ldb, La_INT* info FCLEN);

La_extern void
F77_NAME(zpstrf)(const char* uplo, const La_INT* n, La_complex* a, const La_INT* lda,
		 La_INT* ipiv, La_INT *rank, const double* tol, double* work,
		 La_INT* info FCLEN);


/* ======================================================================== */

//* Other double precision and double complex Lapack routines provided by libRlapack.
/*
   These are extracted from the CLAPACK headers.
*/

La_extern void
F77_NAME(dbdsdc)(const char* uplo, const char* compq, La_INT *n,
	double * d, double *e, double *u, La_INT *ldu, double *vt,
	La_INT *ldvt, double *q, La_INT *iq, double *work, La_INT * iwork, La_INT *info
		 FCLEN FCLEN);

La_extern void
F77_NAME(dgelsd)(La_INT *m, La_INT *n, La_INT *nrhs,
	double *a, La_INT *lda, double *b, La_INT *ldb, double *s,
	double *rcond, La_INT *rank, double *work, La_INT *lwork,
	 La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dgesc2)(La_INT *n, double *a, La_INT *lda,
	double *rhs, La_INT *ipiv, La_INT *jpiv, double *scale);

/* DGESDD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or	   */
/* right singular vectors.  If singular vectors are desired, it uses a */
/* divide-and-conquer algorithm.				   */
La_extern void
F77_NAME(dgesdd)(const char* jobz,
		 const La_INT *m, const La_INT *n,
		 double *a, const La_INT *lda, double *s,
		 double *u, const La_INT *ldu,
		 double *vt, const La_INT *ldvt,
		 double *work, const La_INT *lwork, La_INT *iwork, La_INT *info FCLEN );

La_extern void
F77_NAME(dgetc2)(La_INT *n, double *a, La_INT *lda, La_INT *
	*ipiv, La_INT *jpiv, La_INT *info);

typedef La_INT *(*L_fp)(double *, double *, double *);
La_extern void
F77_NAME(dggesx)(const char* jobvsl, const char* jobvsr, const char* sort, L_fp
	delctg, const char* sense, La_INT *n, double *a, La_INT *lda,
	double *b, La_INT *ldb, La_INT *sdim, double *alphar,
	double *alphai, double *beta, double *vsl, La_INT *ldvsl,
	 double *vsr, La_INT *ldvsr, double *rconde, double *
	rcondv, double *work, La_INT *lwork, La_INT *iwork,
	La_INT * liwork, La_LGL *bwork, La_INT *info
		FCLEN FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dggev)(const char* jobvl, const char* jobvr, La_INT *n, double *
	a, La_INT *lda, double *b, La_INT *ldb, double *alphar,
	double *alphai, double *beta, double *vl, La_INT *ldvl,
	double *vr, La_INT *ldvr, double *work, La_INT *lwork,
		La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dggevx)(const char* balanc, const char* jobvl, const char* jobvr, const char* 
	sense, La_INT *n, double *a, La_INT *lda, double *b,
	La_INT *ldb, double *alphar, double *alphai, double *
	beta, double *vl, La_INT *ldvl, double *vr, La_INT *ldvr,
	La_INT *ilo, La_INT *ihi, double *lscale, double *rscale,
	double *abnrm, double *bbnrm, double *rconde, double *
	rcondv, double *work, La_INT *lwork, La_INT *iwork,
	La_LGL * bwork, La_INT *info FCLEN FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dgtts2)(La_INT *itrans, La_INT *n, La_INT *nrhs,
	double *dl, double *d, double *du, double *du2,
	La_INT *ipiv, double *b, La_INT *ldb);
La_extern void
F77_NAME(dlagv2)(double *a, La_INT *lda, double *b, La_INT *ldb, double *alphar,
		 double *alphai, double * beta, double *csl, double *snl,
		 double *csr, double * snr);

La_extern void
F77_NAME(dlals0)(La_INT *icompq, La_INT *nl, La_INT *nr,
	La_INT *sqre, La_INT *nrhs, double *b, La_INT *ldb, double
	*bx, La_INT *ldbx, La_INT *perm, La_INT *givptr, La_INT *givcol,
	La_INT *ldgcol, double *givnum, La_INT *ldgnum, double *
	poles, double *difl, double *difr, double *z, La_INT *
	k, double *c, double *s, double *work, La_INT *info);

La_extern void
F77_NAME(dlalsa)(La_INT *icompq, La_INT *smlsiz, La_INT *n,
	La_INT *nrhs, double *b, La_INT *ldb, double *bx, La_INT *
	ldbx, double *u, La_INT *ldu, double *vt, La_INT *k,
	double *difl, double *difr, double *z, double *
	poles, La_INT *givptr, La_INT *givcol, La_INT *ldgcol, La_INT *
	perm, double *givnum, double *c, double *s, double *
	work, La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dlalsd)(const char* uplo, La_INT *smlsiz, La_INT *n,
		 La_INT *nrhs, double *d, double *e, double *b, La_INT *ldb,
		 double *rcond, La_INT *rank, double *work, La_INT *iwork,
		 La_INT *info FCLEN);

La_extern void
F77_NAME(dlamc1)(La_INT *beta, La_INT *t, La_INT *rnd, La_INT *
	*ieee1);

La_extern void
F77_NAME(dlamc2)(La_INT *beta, La_INT *t, La_INT *rnd,
	double *eps, La_INT *emin, double *rmin, La_INT *emax,
	double *rmax);

La_extern double
F77_NAME(dlamc3)(double *a, double *b);

La_extern void
F77_NAME(dlamc4)(La_INT *emin, double *start, La_INT *base);

La_extern void
F77_NAME(dlamc5)(La_INT *beta, La_INT *p, La_INT *emin,
	La_INT *ieee, La_INT *emax, double *rmax);

La_extern void
F77_NAME(dlaqp2)(La_INT *m, La_INT *n, La_INT *offset,
	double *a, La_INT *lda, La_INT *jpvt, double *tau,
	double *vn1, double *vn2, double *work);

La_extern void
F77_NAME(dlaqps)(La_INT *m, La_INT *n, La_INT *offset,

		 La_INT *nb, La_INT *kb, double *a, La_INT *lda, La_INT *jpvt,
		 double *tau, double *vn1, double *vn2, double *auxv,
		 double *f, La_INT *ldf);

La_extern void
F77_NAME(dlar1v)(La_INT *n, La_INT *b1, La_INT *bn, double
	*sigma, double *d, double *l, double *ld, double *
	lld, double *gersch, double *z, double *ztz, double
	*mingma, La_INT *r, La_INT *isuppz, double *work);

La_extern void
F77_NAME(dlarrb)(La_INT *n, double *d, double *l,
	double *ld, double *lld, La_INT *ifirst, La_INT *ilast,
	double *sigma, double *reltol, double *w, double *
	wgap, double *werr, double *work, La_INT *iwork, La_INT *
	info);

La_extern void
F77_NAME(dlarre)(La_INT *n, double *d, double *e,
	double *tol, La_INT *nsplit, La_INT *isplit, La_INT *m,
	double *w, double *woff, double *gersch, double *work,
	 La_INT *info);

La_extern void
F77_NAME(dlarrf)(La_INT *n, double *d, double *l,
	double *ld, double *lld, La_INT *ifirst, La_INT *ilast,
	double *w, double *dplus, double *lplus, double *work,
	 La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dlarrv)(La_INT *n, double *d, double *l,
	La_INT *isplit, La_INT *m, double *w, La_INT *iblock,
	double *gersch, double *tol, double *z, La_INT *ldz,
	La_INT *isuppz, double *work, La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dlarz)(const char* side, La_INT *m, La_INT *n, La_INT *l,
	double *v, La_INT *incv, double *tau, double *c,
		La_INT *ldc, double *work FCLEN);

La_extern void
F77_NAME(dlarzb)(const char* side, const char* trans, const char* direct, const char* 
	storev, La_INT *m, La_INT *n, La_INT *k, La_INT *l, double *v,
	 La_INT *ldv, double *t, La_INT *ldt, double *c, La_INT *
		 ldc, double *work, La_INT *ldwork
		 FCLEN FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dlarzt)(const char* direct, const char* storev, La_INT *n, La_INT *
	k, double *v, La_INT *ldv, double *tau, double *t,
		 La_INT *ldt FCLEN FCLEN);

La_extern void
F77_NAME(dlasd0)(La_INT *n, La_INT *sqre, double *d,
	double *e, double *u, La_INT *ldu, double *vt, La_INT *
	ldvt, La_INT *smlsiz, La_INT *iwork, double *work, La_INT *
	info);

La_extern void
F77_NAME(dlasd1)(La_INT *nl, La_INT *nr, La_INT *sqre,
	double *d, double *alpha, double *beta, double *u,
	La_INT *ldu, double *vt, La_INT *ldvt, La_INT *idxq, La_INT *
	iwork, double *work, La_INT *info);

La_extern void
F77_NAME(dlasd2)(La_INT *nl, La_INT *nr, La_INT *sqre, La_INT *
	*k, double *d, double *z, double *alpha, double *
	beta, double *u, La_INT *ldu, double *vt, La_INT *ldvt,
	double *dsigma, double *u2, La_INT *ldu2, double *vt2,
	La_INT *ldvt2, La_INT *idxp, La_INT *idx, La_INT *idxc, La_INT *
	idxq, La_INT *coltyp, La_INT *info);

La_extern void
F77_NAME(dlasd3)(La_INT *nl, La_INT *nr, La_INT *sqre, La_INT *
	*k, double *d, double *q, La_INT *ldq, double *dsigma,
	double *u, La_INT *ldu, double *u2, La_INT *ldu2,
	double *vt, La_INT *ldvt, double *vt2, La_INT *ldvt2,
	La_INT *idxc, La_INT *ctot, double *z, La_INT *info);

La_extern void
F77_NAME(dlasd4)(La_INT *n, La_INT *i, double *d,
	double *z, double *delta, double *rho, double *
	sigma, double *work, La_INT *info);

La_extern void
F77_NAME(dlasd5)(La_INT *i, double *d, double *z,
	double *delta, double *rho, double *dsigma, double *
	work);

La_extern void
F77_NAME(dlasd6)(La_INT *icompq, La_INT *nl, La_INT *nr,
	La_INT *sqre, double *d, double *vf, double *vl,
	double *alpha, double *beta, La_INT *idxq, La_INT *perm,
	La_INT *givptr, La_INT *givcol, La_INT *ldgcol, double *givnum,
	 La_INT *ldgnum, double *poles, double *difl, double *
	difr, double *z, La_INT *k, double *c, double *s,
	double *work, La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dlasd7)(La_INT *icompq, La_INT *nl, La_INT *nr,
	La_INT *sqre, La_INT *k, double *d, double *z,
	double *zw, double *vf, double *vfw, double *vl,
	double *vlw, double *alpha, double *beta, double *
	dsigma, La_INT *idx, La_INT *idxp, La_INT *idxq, La_INT *perm,
	La_INT *givptr, La_INT *givcol, La_INT *ldgcol, double *givnum,
	 La_INT *ldgnum, double *c, double *s, La_INT *info);

La_extern void
F77_NAME(dlasd8)(La_INT *icompq, La_INT *k, double *d,
	double *z, double *vf, double *vl, double *difl,
	double *difr, La_INT *lddifr, double *dsigma, double *
	work, La_INT *info);

La_extern void
F77_NAME(dlasd9)(La_INT *icompq, La_INT *ldu, La_INT *k,
	double *d, double *z, double *vf, double *vl,
	double *difl, double *difr, double *dsigma, double *
	work, La_INT *info);

La_extern void
F77_NAME(dlasda)(La_INT *icompq, La_INT *smlsiz, La_INT *n,
	La_INT *sqre, double *d, double *e, double *u, La_INT *
	*ldu, double *vt, La_INT *k, double *difl, double *difr,
	double *z, double *poles, La_INT *givptr, La_INT *givcol,
	La_INT *ldgcol, La_INT *perm, double *givnum, double *c,
	double *s, double *work, La_INT *iwork, La_INT *info);

La_extern void
F77_NAME(dlasdq)(const char* uplo, La_INT *sqre, La_INT *n, La_INT *
	ncvt, La_INT *nru, La_INT *ncc, double *d, double *e,
	double *vt, La_INT *ldvt, double *u, La_INT *ldu,
		 double *c, La_INT *ldc, double *work, La_INT *info FCLEN);

La_extern void
F77_NAME(dlasdt)(La_INT *n, La_INT *lvl, La_INT *nd, La_INT *
	inode, La_INT *ndiml, La_INT *ndimr, La_INT *msub);

La_extern void
F77_NAME(dlasq5)(La_INT *i0, La_INT *n0, double *z,
	La_INT *pp, double *tau, double *dmin, double *dmin1,
	double *dmin2, double *dn, double *dnm1, double *dnm2,
	 La_INT *ieee);

La_extern void
F77_NAME(dlasq6)(La_INT *i0, La_INT *n0, double *z,
	La_INT *pp, double *dmin, double *dmin1, double *dmin2,
	 double *dn, double *dnm1, double *dnm2);

La_extern void
F77_NAME(dlatdf)(La_INT *ijob, La_INT *n, double *z,
	La_INT *ldz, double *rhs, double *rdsum, double *rdscal,
	La_INT *ipiv, La_INT *jpiv);

La_extern void
F77_NAME(dlatrz)(La_INT *m, La_INT *n, La_INT *l, double *
	a, La_INT *lda, double *tau, double *work);

La_extern void
F77_NAME(dormr3)(const char* side, const char* trans, La_INT *m, La_INT *n,
		 La_INT *k, La_INT *l, double *a, La_INT *lda, double *tau,
		 double *c, La_INT *ldc, double *work, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dormrz)(const char* side, const char* trans, La_INT *m, La_INT *n,
	La_INT *k, La_INT *l, double *a, La_INT *lda, double *tau,
	double *c, La_INT *ldc, double *work, La_INT *lwork,
		 La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dptts2)(La_INT *n, La_INT *nrhs, double *d,
	double *e, double *b, La_INT *ldb);

La_extern void
F77_NAME(dsbgvd)(const char* jobz, const char* uplo, La_INT *n, La_INT *ka,
	La_INT *kb, double *ab, La_INT *ldab, double *bb, La_INT *
	ldbb, double *w, double *z, La_INT *ldz, double *work,
		 La_INT *lwork, La_INT *iwork, La_INT *liwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dsbgvx)(const char* jobz, const char* range, const char* uplo, La_INT *n,
	La_INT *ka, La_INT *kb, double *ab, La_INT *ldab, double *
	bb, La_INT *ldbb, double *q, La_INT *ldq, double *vl,
	double *vu, La_INT *il, La_INT *iu, double *abstol, La_INT *
	*m, double *w, double *z, La_INT *ldz, double *work,
		 La_INT *iwork, La_INT *ifail, La_INT *info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dspgvd)(La_INT *itype, const char* jobz, const char* uplo, La_INT *
		 n, double *ap, double *bp, double *w, double *z,
		 La_INT *ldz, double *work, La_INT *lwork, La_INT *iwork,
		 La_INT *liwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dspgvx)(La_INT *itype, const char* jobz, const char* range, const char* 
		 uplo, La_INT *n, double *ap, double *bp, double *vl,
		 double *vu, La_INT *il, La_INT *iu, double *abstol, La_INT *
		 *m, double *w, double *z, La_INT *ldz, double *work,
		 La_INT *iwork, La_INT *ifail, La_INT *info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dstegr)(const char* jobz, const char* range, La_INT *n, double *
		 d, double *e, double *vl, double *vu, La_INT *il,
		 La_INT *iu, double *abstol, La_INT *m, double *w,
		 double *z, La_INT *ldz, La_INT *isuppz, double *work,
		 La_INT *lwork, La_INT *iwork, La_INT *liwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dstevr)(const char* jobz, const char* range, La_INT *n,
		 double *d, double *e, double *vl, double *vu, La_INT *il,
		 La_INT *iu, double *abstol, La_INT *m, double *w,
		 double *z, La_INT *ldz, La_INT *isuppz, double *work,
		 La_INT *lwork, La_INT *iwork, La_INT *liwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dsygvd)(La_INT *itype, const char* jobz, const char* uplo, La_INT *
		 n, double *a, La_INT *lda, double *b, La_INT *ldb,
		 double *w, double *work, La_INT *lwork, La_INT *iwork,
		 La_INT *liwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dsygvx)(La_INT *itype, const char* jobz, const char* range,
		 const char* uplo, La_INT *n, double *a, La_INT *lda, double *b,
		 La_INT *ldb, double *vl, double *vu, La_INT *il, La_INT *iu,
		 double *abstol, La_INT *m, double *w, double *z,
		 La_INT *ldz, double *work, La_INT *lwork, La_INT *iwork,
		 La_INT *ifail, La_INT *info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(dtgex2)(La_LGL const *wantq, La_LGL const *wantz, La_INT *n,
	double *a, La_INT *lda, double *b, La_INT *ldb, double *
	q, La_INT *ldq, double *z, La_INT *ldz, La_INT *j1, La_INT *
	n1, La_INT *n2, double *work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(dtgexc)(La_LGL *wantq, La_LGL *wantz, La_INT *n,
	double *a, La_INT *lda, double *b, La_INT *ldb, double *
	q, La_INT *ldq, double *z, La_INT *ldz, La_INT *ifst,
	La_INT *ilst, double *work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(dtgsen)(La_INT *ijob, La_LGL const *wantq, La_LGL const *wantz,
	const La_LGL *select,
	La_INT *n, double *a, La_INT *lda, double *
	b, La_INT *ldb, double *alphar, double *alphai, double *
	beta, double *q, La_INT *ldq, double *z, La_INT *ldz,
	La_INT *m, double *pl, double *pr, double *dif,
	double *work, La_INT *lwork, La_INT *iwork, La_INT *liwork,
	La_INT *info);

La_extern void
F77_NAME(dtgsna)(const char* job, const char* howmny, La_LGL *select,
	La_INT *n, double *a, La_INT *lda, double *b, La_INT *ldb,
	double *vl, La_INT *ldvl, double *vr, La_INT *ldvr,
	double *s, double *dif, La_INT *mm, La_INT *m, double *
		 work, La_INT *lwork, La_INT *iwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dtgsy2)(const char* trans, La_INT *ijob, La_INT *m, La_INT *
	n, double *a, La_INT *lda, double *b, La_INT *ldb,
	double *c, La_INT *ldc, double *d, La_INT *ldd,
	double *e, La_INT *lde, double *f, La_INT *ldf, double *
	scale, double *rdsum, double *rdscal, La_INT *iwork, La_INT *
		 *pq, La_INT *info FCLEN);

La_extern void
F77_NAME(dtgsyl)(const char* trans, La_INT *ijob, La_INT *m, La_INT *
	n, double *a, La_INT *lda, double *b, La_INT *ldb,
	double *c, La_INT *ldc, double *d, La_INT *ldd,
	double *e, La_INT *lde, double *f, La_INT *ldf, double *
	scale, double *dif, double *work, La_INT *lwork, La_INT *
		 iwork, La_INT *info FCLEN);

La_extern void
F77_NAME(dtzrzf)(La_INT *m, La_INT *n, double *a, La_INT *
	lda, double *tau, double *work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(dpstrf)(const char* uplo, const La_INT* n,
		 double* a, const La_INT* lda, La_INT* piv, La_INT* rank,
		 double* tol, double *work, La_INT* info FCLEN);

/* This returns Fortran LOGICAL.  See comments at the top of the file.
   clapack and Accelerate headers declare lsamen but not lsame.

   Some people regard this as part of BLAS, and it is used on some
   BLAS routines.  However, it is not included by
   Apple's Accelerate (nor cblas: it is in lapacke).
 */
La_extern La_LGL *
F77_NAME(lsame)(const char* ca, const char* cb FCLEN FCLEN);

La_extern void
F77_NAME(zbdsqr)(const char* uplo, La_INT *n, La_INT *ncvt, La_INT *
	nru, La_INT *ncc, double *d, double *e, La_complex *vt,
	La_INT *ldvt, La_complex *u, La_INT *ldu, La_complex *c,
		 La_INT *ldc, double *rwork, La_INT *info FCLEN);

La_extern void
F77_NAME(zdrot)(const La_INT *n, const La_complex *cx, const La_INT *incx,
	La_complex *cy, const La_INT *incy, const double *c, const double *s);

La_extern void
F77_NAME(zgebak)(const char* job, const char* side, La_INT *n, La_INT *ilo,
		 La_INT *ihi, double *scale, La_INT *m, La_complex *v,
		 La_INT *ldv, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(zgebal)(const char *job, const La_INT *n, La_complex *a,
		 const La_INT *lda, La_INT *ilo, La_INT *ihi, double *scale,
		 La_INT *info FCLEN);

La_extern void
F77_NAME(zgebd2)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, double *d, double *e, La_complex *tauq,
	La_complex *taup, La_complex *work, La_INT *info);

La_extern void
F77_NAME(zgebrd)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, double *d, double *e, La_complex *tauq,
	La_complex *taup, La_complex *work, La_INT *lwork, La_INT *
	info);
La_extern void
F77_NAME(zgehd2)(La_INT *n, La_INT *ilo, La_INT *ihi,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *info);

La_extern void
F77_NAME(zgehrd)(La_INT *n, La_INT *ilo, La_INT *ihi,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zgelq2)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, La_complex *tau, La_complex *work, La_INT *info);

La_extern void
F77_NAME(zgelqf)(La_INT *m, La_INT *n, La_complex *a,
		 La_INT *lda, La_complex *tau, La_complex *work, La_INT *lwork,
		 La_INT *info);

La_extern void
F77_NAME(zgeqr2)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, La_complex *tau, La_complex *work, La_INT *info);

La_extern void
F77_NAME(zgeqrf)(La_INT *m, La_INT *n, La_complex *a,
		 La_INT *lda, La_complex *tau, La_complex *work, La_INT *lwork,
		 La_INT *info);

La_extern void
F77_NAME(zgetf2)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, La_INT *ipiv, La_INT *info);

La_extern void
F77_NAME(zgetrf)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, La_INT *ipiv, La_INT *info);
La_extern void
F77_NAME(zgetrf2)(La_INT *m, La_INT *n, La_complex *a,
	La_INT *lda, La_INT *ipiv, La_INT *info);

/* ZGETRI computes the inverse of a matrix using the LU factorization
 * computed by ZGETRF.
 * Added in R 3.3.0
 */
La_extern void
F77_NAME(zgetri)(La_INT *n, La_complex *a, La_INT *lda,
		 La_INT *ipiv, La_complex *work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zgetrs)(const char* trans, La_INT *n, La_INT *nrhs,
	La_complex *a, La_INT *lda, La_INT *ipiv, La_complex *b,
		 La_INT *ldb, La_INT *info FCLEN);


La_extern void
F77_NAME(zhetd2)(const char* uplo, La_INT *n, La_complex *a, La_INT *lda, double *d,
		 double *e, La_complex *tau, La_INT *info FCLEN);

La_extern void
F77_NAME(zhetrd)(const char* uplo, La_INT *n, La_complex *a,
		 La_INT *lda, double *d, double *e, La_complex *tau,
		 La_complex *work, La_INT *lwork, La_INT *info FCLEN);

La_extern void
F77_NAME(zhseqr)(const char* job, const char* compz, La_INT *n, La_INT *ilo,
	 La_INT *ihi, La_complex *h, La_INT *ldh, La_complex *w,
	La_complex *z, La_INT *ldz, La_complex *work, La_INT *lwork,
		 La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(zlabrd)(La_INT *m, La_INT *n, La_INT *nb,
	La_complex *a, La_INT *lda, double *d, double *e,
	La_complex *tauq, La_complex *taup, La_complex *x, La_INT *
	ldx, La_complex *y, La_INT *ldy);

La_extern void
F77_NAME(zlacgv)(La_INT *n, La_complex *x, La_INT *incx);

La_extern void
F77_NAME(zlacpy)(const char* uplo, La_INT *m, La_INT *n,
		 La_complex *a, La_INT *lda, La_complex *b, La_INT *ldb FCLEN);

La_extern void
F77_NAME(zlahqr)(La_INT *wantt, La_INT *wantz, La_INT *n,
	La_INT *ilo, La_INT *ihi, La_complex *h, La_INT *ldh,
	La_complex *w, La_INT *iloz, La_INT *ihiz, La_complex *z,
	La_INT *ldz, La_INT *info);

La_extern double
F77_NAME(zlange)(const char* norm, La_INT *m, La_INT *n, La_complex *a, La_INT *lda,
		 double *work FCLEN);

La_extern double
F77_NAME(zlanhe)(const char* norm,  const char* uplo, La_INT *n, La_complex *a,
		 La_INT *lda, double *work FCLEN FCLEN);

La_extern double
F77_NAME(zlanhs)(const char* norm, La_INT *n, La_complex *a, La_INT *lda, 
		 double *work FCLEN);

La_extern double
F77_NAME(zlantp)(const char* norm, const char* uplo, const char* diag,
		 La_INT *n, La_complex *ap, double *work FCLEN FCLEN FCLEN);


La_extern void
F77_NAME(zlaqp2)(La_INT *m, La_INT *n, La_INT *offset,
	La_complex *a, La_INT *lda, La_INT *jpvt, La_complex *tau,
	double *vn1, double *vn2, La_complex *work);

La_extern void
F77_NAME(zlaqps)(La_INT *m, La_INT *n, La_INT *offset, La_INT *
	*nb, La_INT *kb, La_complex *a, La_INT *lda, La_INT *jpvt,
	La_complex *tau, double *vn1, double *vn2, La_complex *
	auxv, La_complex *f, La_INT *ldf);

La_extern void
F77_NAME(zlarf)(const char* side, La_INT *m, La_INT *n, La_complex
	*v, La_INT *incv, La_complex *tau, La_complex *c, La_INT *
		ldc, La_complex *work FCLEN);

La_extern void
F77_NAME(zlarfb)(const char* side, const char* trans, 
		 const char* direct, const char*  storev,
		 La_INT *m, La_INT *n, La_INT *k, La_complex *v, La_INT *ldv,
		 La_complex *t, La_INT *ldt, La_complex *c, La_INT *
		 ldc, La_complex *work, La_INT *ldwork
		 FCLEN FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(zlarfg)(La_INT *n, La_complex *alpha, La_complex *
	x, La_INT *incx, La_complex *tau);

La_extern void
F77_NAME(zlarft)(const char* direct, const char* storev, La_INT *n, La_INT *
		 k, La_complex *v, La_INT *ldv, La_complex *tau, La_complex *
		 t, La_INT *ldt FCLEN FCLEN);

La_extern void
F77_NAME(zlarfx)(const char* side, La_INT *m, La_INT *n,
	La_complex *v, La_complex *tau, La_complex *c, La_INT *
		 ldc, La_complex *work FCLEN);

La_extern void
F77_NAME(zlascl)(const char* type, La_INT *kl, La_INT *ku,
	double *cfrom, double *cto, La_INT *m, La_INT *n,
		 La_complex *a, La_INT *lda, La_INT *info FCLEN);

La_extern void
F77_NAME(zlaset)(const char* uplo, La_INT *m, La_INT *n,
	La_complex *alpha, La_complex *beta, La_complex *a, La_INT *
		 lda FCLEN);

La_extern void
F77_NAME(zlasr)(const char* side, const char* pivot, const char* direct,
		La_INT *m, La_INT *n, double *c, double *s, La_complex *a, La_INT *lda
		FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(zlassq)(La_INT *n, La_complex *x, La_INT *incx,
	double *scale, double *sumsq);

La_extern void
F77_NAME(zlaswp)(La_INT *n, La_complex *a, La_INT *lda,
	La_INT *k1, La_INT *k2, La_INT *ipiv, La_INT *incx);

La_extern void
F77_NAME(zlatrd)(const char* uplo, La_INT *n, La_INT *nb,
	La_complex *a, La_INT *lda, double *e, La_complex *tau,
		 La_complex *w, La_INT *ldw FCLEN);

La_extern void
F77_NAME(zlatrs)(const char* uplo, const char* trans, 
		 const char* diag, const char*  normin,
		 La_INT *n, La_complex *a, La_INT *lda, La_complex *x,
		 double *scale, double *cnorm, La_INT *info
		 FCLEN FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(zsteqr)(const char* compz, La_INT *n, double *d,
	double *e, La_complex *z, La_INT *ldz, double *work,
		 La_INT *info FCLEN);

La_extern void
F77_NAME(zpocon)(const char* uplo, const La_INT *n, const La_complex *a, const La_INT *lda,
		 const double *anorm, double *rcond,
		 La_complex *work, double *rwork, La_INT *info FCLEN);

La_extern void
F77_NAME(zspcon)(const char* uplo, const La_INT *n, const La_complex *ap,
		 const La_INT *ipiv, const double *anorm,
		 double *rcond,
		 La_complex *work, La_INT *info FCLEN);

La_extern void
F77_NAME(zsptrf)(const char* uplo, const La_INT *n, La_complex *ap,
		 La_INT *ipiv, La_INT *info FCLEN);

La_extern void
F77_NAME(zsptri)(const char* uplo, const La_INT* n, La_complex* ap,
		 La_INT *ipiv, La_complex* work, La_INT *info FCLEN);

La_extern void
F77_NAME(zsptrs)(const char* uplo, const La_INT* n, const La_INT* nrhs,
		 const La_complex* ap, const La_INT* ipiv,
		 La_complex* b, const La_INT* ldb, La_INT* info FCLEN);

La_extern void
F77_NAME(zsycon)(const char* uplo, const La_INT *n, const La_complex *a, const La_INT *lda,
		 La_INT *ipiv, const double *anorm,
		 double *rcond,
		 La_complex *work, La_INT *info FCLEN);

/* ZSYMV  performs the matrix-vector  operation   y := alpha*A*x + beta*y  */
La_extern void
F77_NAME(zsymv)(const char* uplo, const La_INT* n, const La_complex* alpha,
		const La_complex* a, const La_INT* lda,
		const La_complex* x, const La_INT* incx,
		const La_complex* beta,
		La_complex* y, const La_INT* incy FCLEN);

La_extern void
F77_NAME(zsytrf)(const char* uplo, const La_INT *n, La_complex *a, const La_INT* lda,
		 La_INT *ipiv, La_complex *work, const La_INT *lwork, La_INT *info FCLEN);

La_extern void
F77_NAME(zsytri)(const char* uplo, const La_INT *n, La_complex *a, const La_INT* lda,
		 const La_INT *ipiv, La_complex *work, La_INT *info FCLEN);

La_extern void
F77_NAME(zsytrs)(const char* uplo, const La_INT* n, const La_INT* nrhs,
	const La_complex* a, const La_INT* lda, const La_INT* ipiv,
	      La_complex* b, const La_INT* ldb, La_INT* info FCLEN);


La_extern void
F77_NAME(ztpcon)(const char* norm, const char* uplo, const char* diag,
                 const La_INT *n, const La_complex *ap,
		 double *rcond, La_complex *work, double *rwork, La_INT *info
		 FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(ztptri)(const char* uplo, const char* diag, const La_INT* n, La_complex* ap,
		 La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(ztptrs)(const char* uplo, const char* trans, const char* diag, 
		 const La_INT* n, const La_INT* nrhs, const La_complex* ap,
		 La_complex* b, const La_INT* ldb, La_INT* info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(ztrtri)(const char* uplo, const char* diag,
		 const La_INT* n, La_complex* a, const La_INT* lda,
		 La_INT *info FCLEN FCLEN);

/* ZTRCON estimates the reciprocal of the condition number of a
 * triangular matrix A, in either the 1-norm or the infinity-norm.
 */
La_extern void
F77_NAME(ztrcon)(const char* norm, const char* uplo, const char* diag,
                 const La_INT *n, const La_complex *a, const La_INT *lda,
		 double *rcond, La_complex *work, double *rwork, La_INT *info
		 FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(ztrevc)(const char* side, const char* howmny, La_LGL *select,
	La_INT *n, La_complex *t, La_INT *ldt, La_complex *vl,
	La_INT *ldvl, La_complex *vr, La_INT *ldvr, La_INT *mm, La_INT *
		 *m, La_complex *work, double *rwork, La_INT *info
		 FCLEN FCLEN);

La_extern void
F77_NAME(zung2l)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *info);

La_extern void
F77_NAME(zung2r)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *info);

La_extern void
F77_NAME(zungbr)(const char* vect, La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
		 work, La_INT *lwork, La_INT *info FCLEN);

La_extern void
F77_NAME(zunghr)(La_INT *n, La_INT *ilo, La_INT *ihi,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zungl2)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *info);

La_extern void
F77_NAME(zunglq)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zungql)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zungqr)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zungr2)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *info);

La_extern void
F77_NAME(zungrq)(La_INT *m, La_INT *n, La_INT *k,
	La_complex *a, La_INT *lda, La_complex *tau, La_complex *
	work, La_INT *lwork, La_INT *info);

La_extern void
F77_NAME(zungtr)(const char* uplo, La_INT *n, La_complex *a,
	La_INT *lda, La_complex *tau, La_complex *work, La_INT *lwork,
		 La_INT *info FCLEN);

La_extern void
F77_NAME(zunm2r)(const char* side, const char* trans, La_INT *m, La_INT *n,
		 La_INT *k, La_complex *a, La_INT *lda, La_complex *tau,
		 La_complex *c, La_INT *ldc, La_complex *work, La_INT *info
		 FCLEN FCLEN);

La_extern void
F77_NAME(zunmbr)(const char* vect, const char* side, const char* trans, La_INT *m,
	La_INT *n, La_INT *k, La_complex *a, La_INT *lda, La_complex
	*tau, La_complex *c, La_INT *ldc, La_complex *work, La_INT *
		 lwork, La_INT *info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(zunml2)(const char* side, const char* trans, La_INT *m, La_INT *n,
	La_INT *k, La_complex *a, La_INT *lda, La_complex *tau,
		 La_complex *c, La_INT *ldc, La_complex *work, La_INT *info
		 FCLEN FCLEN);

La_extern void
F77_NAME(zunmlq)(const char* side, const char* trans, La_INT *m, La_INT *n,
		 La_INT *k, La_complex *a, La_INT *lda, La_complex *tau,
	La_complex *c, La_INT *ldc, La_complex *work, La_INT *lwork,
		 La_INT *info FCLEN FCLEN);

/* Added in R 3.1.0 */
/* ZGESVD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or	   */
/* right singular vectors					   */
La_extern void
F77_NAME(zgesdd)(const char* jobz,
		 const La_INT *m, const La_INT *n,
		 La_complex *a, const La_INT *lda, double *s,
		 La_complex *u, const La_INT *ldu,
		 La_complex *vt, const La_INT *ldvt,
		 La_complex *work, const La_INT *lwork, double *rwork,
		 La_INT *iwork, La_INT *info FCLEN);
La_extern void
F77_NAME(zgelsd)(La_INT *m, La_INT *n, La_INT *nrhs,
	La_complex *a, La_INT *lda, La_complex *b, La_INT *ldb, double *s,
        double *rcond, La_INT *rank, 
        La_complex *work, La_INT *lwork, double *rwork, La_INT *iwork, La_INT *info);

/* More Complex for R 4.4.0  --- Complex Hermitian (incl "crossprod") */

La_extern double
F77_NAME(zlanhp)(const char* norm, const char* uplo,
		 La_INT const* n,
		 La_complex const* AP,
		 double* work FCLEN FCLEN);

La_extern void
F77_NAME(zhpcon)(const char* uplo, La_INT const* n, La_complex const* AP,
		 La_INT const* ipiv, double const* anorm,
		 double* rcond,
		 La_complex* work, La_INT* info FCLEN);

La_extern void
F77_NAME(zhptrf)(const char* uplo, La_INT const* n, La_complex* AP, La_INT* ipiv,
		 La_INT* info FCLEN);
    
La_extern void
F77_NAME(zhptri)(const char* uplo, La_INT const* n, La_complex* AP, La_INT const* ipiv,
		 La_complex* work, La_INT* info FCLEN);

La_extern void
F77_NAME(zhptrs)(const char* uplo, La_INT const* n, La_INT const* nrhs,
		 La_complex const* AP, La_INT const* ipiv,
		 La_complex* B, La_INT const* ldb,
		 La_INT* info FCLEN);

La_extern void
F77_NAME(zhecon)(const char* uplo, La_INT const* n,
		 La_complex const* A, La_INT const* lda,
		 La_INT const* ipiv, double const* anorm,
		 double* rcond,
		 La_complex* work, La_INT* info FCLEN);

La_extern void
F77_NAME(zhetrf)(const char* uplo, La_INT const* n,
		 La_complex* A, La_INT const* lda, La_INT* ipiv,
		 La_complex* work, La_INT const* lwork, La_INT* info FCLEN);

La_extern void
F77_NAME(zhetri)(const char* uplo, La_INT const* n,
		 La_complex* A, La_INT const* lda, La_INT const* ipiv,
		 La_complex* work, La_INT* info FCLEN);

La_extern void
F77_NAME(zhetrs)(const char* uplo, La_INT const* n, La_INT const* nrhs,
		 La_complex const* A, La_INT const* lda, La_INT const* ipiv,
		 La_complex* B, La_INT const* ldb,
		 La_INT* info FCLEN);

La_extern void
F77_NAME(zgees)(const char *jobvs, const char *sort,
		 La_LGL (*select)(const La_complex *),
		 const La_INT *n, La_complex *a, const La_INT *lda,
		 La_INT *sdim, La_complex *w, La_complex *vs, const La_INT *ldvs,
		 La_complex *work, const La_INT *lwork, double *rwork, La_LGL *bwork,
		 La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(zhpev)(const char *jobz, const char *uplo,
		 const La_INT *n, La_complex *ap,
		 double *w, La_complex *z, const La_INT *ldz,
		 La_complex *work, double *rwork,
		 La_INT *info FCLEN FCLEN);

/* =========================== DEPRECATED ==============================

   Routines below were deprecated in LAPACK 3.6.0, and are not
   included in a default build of LAPACK.

   Currently dgegv, dgeqpf, dggsvd and dggsvp are included in R, but
   that may change in future.
 */

/* DGEGV - compute for a pair of n-by-n real nonsymmetric */
/* matrices A and B, the generalized eigenvalues (alphar +/- */
/* alphai*i, beta);, and optionally, the left and/or right */
/* generalized eigenvectors (VL and VR); */
La_extern void
F77_NAME(dgegv)(const char* jobvl, const char* jobvr,
		const La_INT* n, double* a, const La_INT* lda,
		double* b, const La_INT* ldb,
		double* alphar, double* alphai,
		const double* beta, double* vl, const La_INT* ldvl,
		double* vr, const La_INT* ldvr,
		double* work, const La_INT* lwork, La_INT* info FCLEN FCLEN);

/* DGEQPF - compute a QR factorization with column pivoting of a */
/* real M-by-N matrix A */
La_extern void
F77_NAME(dgeqpf)(const La_INT* m, const La_INT* n, double* a, const La_INT* lda,
		 La_INT* jpvt, double* tau, double* work, La_INT* info);

/* DGGSVD - compute the generalized singular value decomposition */
/* (GSVD) of an M-by-N real matrix A and P-by-N real matrix B */
La_extern void
F77_NAME(dggsvd)(const char* jobu, const char* jobv, const char* jobq,
		 const La_INT* m, const La_INT* n, const La_INT* p,
		 const La_INT* k, const La_INT* l,
		 double* a, const La_INT* lda,
		 double* b, const La_INT* ldb,
		 const double* alpha, const double* beta,
		 double* u, const La_INT* ldu,
		 double* v, const La_INT* ldv,
		 double* q, const La_INT* ldq,
		 double* work, La_INT* iwork, La_INT* info
		 FCLEN FCLEN FCLEN);

/* DTZRQF - reduce the M-by-N ( M<=N ); real upper trapezoidal */
/* matrix A to upper triangular form by means of orthogonal */
/* transformations  */
La_extern void
F77_NAME(dtzrqf)(const La_INT* m, const La_INT* n,
		 double* a, const La_INT* lda,
		 double* tau, La_INT* info);

/* DLAHRD - reduce the first NB columns of a real general */
/* n-by-(n-k+1); matrix A so that elements below the k-th */
/* subdiagonal are zero */
La_extern void
F77_NAME(dlahrd)(const La_INT* n, const La_INT* k, const La_INT* nb,
		 double* a, const La_INT* lda,
		 double* tau, double* t, const La_INT* ldt,
		 double* y, const La_INT* ldy);

/* DLATZM - apply a Householder matrix generated by DTZRQF to a */
/* matrix */
La_extern void
F77_NAME(dlatzm)(const char* side, const La_INT* m, const La_INT* n,
		 const double* v, const La_INT* incv,
		 const double* tau, double* c1, double* c2,
		 const La_INT* ldc, double* work FCLEN);

La_extern void
F77_NAME(dgegs)(const char* jobvsl, const char* jobvsr, La_INT *n,
	double *a, La_INT *lda, double *b, La_INT *ldb, double *
	alphar, double *alphai, double *beta, double *vsl,
	La_INT *ldvsl, double *vsr, La_INT *ldvsr, double *work,
		La_INT *lwork, La_INT *info FCLEN FCLEN);

La_extern void
F77_NAME(dgelsx)(La_INT *m, La_INT *n, La_INT *nrhs,
	double *a, La_INT *lda, double *b, La_INT *ldb, La_INT *
	jpvt, double *rcond, La_INT *rank, double *work, La_INT *
	info);

La_extern void
F77_NAME(dggsvp)(const char* jobu, const char* jobv, const char* jobq, La_INT *m,
	La_INT *p, La_INT *n, double *a, La_INT *lda, double *b,
	La_INT *ldb, double *tola, double *tolb, La_INT *k, La_INT *
	*l, double *u, La_INT *ldu, double *v, La_INT *ldv,
	double *q, La_INT *ldq, La_INT *iwork, double *tau,
		 double *work, La_INT *info FCLEN FCLEN FCLEN);

La_extern void
F77_NAME(zlahrd)(La_INT *n, La_INT *k, La_INT *nb,
		 La_complex *a, La_INT *lda, La_complex *tau, La_complex *t,
		 La_INT *ldt, La_complex *y, La_INT *ldy);


#ifdef	__cplusplus
}
#endif

#endif /* R_LAPACK_H */

// Local variables: ***
// mode: outline-minor ***
// outline-regexp: "^\^L\\|^//[*]+" ***
// End: ***
