/* C declarations of principal Lapack routines */

#include "Rinternals.h"
#include "R_ext/RS.h"

/* Level 3 BLAS */

/* DGEMM - perform one of the matrix-matrix operations    */
/* C := alpha*op( A )*op( B ) + beta*C,                   */
void F77_NAME(dgemm)(const char *transa, const char *transb,
		     const int *m, const int *n,
		     const int *k, const double *alpha,
		     const double *a, const int *lda,
		     const double *b, const int *ldb,
		     const double *beta, double *c, const int *ldc);

/* DGEEV - compute for an N-by-N real nonsymmetric matrix A, the   */
/* eigenvalues and, optionally, the left and/or right eigenvectors */
void F77_NAME(dgeev)(const char *jobvl, const char *jobvr,
		     const int *n, double *a, const int *lda,
		     double *wr, double *wi,
		     double *vl, const int *ldvl,
		     double *vr, const int *ldvr,
		     double *work, const int *lwork, int *info);

/* DGESVD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or      */
/* right singular vectors                                          */
void F77_NAME(dgesvd)(const char *jobu, const char *jobvt,
		      const int *m, const int *n,
		      double *a, const int *lda, double *s,
		      double *u, const int *ldu,
		      double *vt, const int *ldvt,
		      double *work, const int *lwork, int *info);

/* DSYEV - compute all eigenvalues and, optionally, eigenvectors   */
/* of a real symmetric matrix A                                    */
void F77_NAME(dsyev)(const char *jobz, const char *uplo,
		     const int *n, double *a, const int *lda,
		     double *w, double *work, const int *lwork,
		     int *info);

/* ZGESV computes the solution to a complex system of linear equations */
void F77_NAME(zgesv)(const int *n, const int *nrhs, Rcomplex *a,
		     const int *lda, int *ipiv, Rcomplex *b,
		     const int *ldb, int *info);

/*  ZGEQP3 computes a QR factorization with column pivoting */
void F77_NAME(zgeqp3)(const int *m, const int *n,
		      Rcomplex *a, const int *lda,
		      int *jpvt, Rcomplex *tau,
		      Rcomplex *work, const int *lwork,
		      double *rwork, int *info);

/* ZUNMQR applies Q or Q**H from the Left or Right */
void F77_NAME(zunmqr)(const char *side, const char *trans,
		      const int *m, const int *n, const int *k,
		      Rcomplex *a, const int *lda,
		      Rcomplex *tau,
		      Rcomplex *c, const int *ldc,
		      Rcomplex *work, const int *lwork, int *info);

/*  ZTRTRS solves triangular systems */
void F77_NAME(ztrtrs)(const char *uplo, const char *trans, const char *diag,
		      const int *n, const int *nrhs,
		      Rcomplex *a, const int *lda,
		      Rcomplex *b, const int *ldb,
		      int *info);
/* ZGESVD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or      */
/* right singular vectors                                          */
void F77_NAME(zgesvd)(const char *jobu, const char *jobvt,
		      const int *m, const int *n,
		      Rcomplex *a, const int *lda, double *s,
		      Rcomplex *u, const int *ldu,
		      Rcomplex *vt, const int *ldvt,
		      Rcomplex *work, const int *lwork, double *rwork,
		      int *info);

/* ZGHEEV - compute all eigenvalues and, optionally, eigenvectors   */
/* of a Hermitian matrix A                                    */
void F77_NAME(zheev)(const char *jobz, const char *uplo,
		     const int *n, Rcomplex *a, const int *lda,
		     double *w, Rcomplex *work, const int *lwork,
		     double *rwork, int *info);

/* ZGGEEV - compute all eigenvalues and, optionally, eigenvectors   */
/* of a complex non-symmetric matrix A                                    */
void F77_NAME(zgeev)(const char *jobvl, const char *jobvr,
		     const int *n, Rcomplex *a, const int *lda,
		     Rcomplex *wr,
		     Rcomplex *vl, const int *ldvl,
		     Rcomplex *vr, const int *ldvr,
		     Rcomplex *work, const int *lwork, 
		     double *rwork, int *info);
