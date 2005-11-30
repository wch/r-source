/* C declarations of principal Lapack routines */

#include <R_ext/Complex.h>
#include <R_ext/RS.h>


/* Level 3 BLAS */

/* DGEMM - perform one of the matrix-matrix operations    */
/* C := alpha*op( A )*op( B ) + beta*C,                   */
void F77_NAME(dgemm)(const char *transa, const char *transb,
		     const int *m, const int *n,
		     const int *k, const double *alpha,
		     const double *a, const int *lda,
		     const double *b, const int *ldb,
		     const double *beta, double *c, const int *ldc);

/* DGESVD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or      */
/* right singular vectors                                          */
void F77_NAME(dgesvd)(const char *jobu, const char *jobvt,
		      const int *m, const int *n,
		      double *a, const int *lda, double *s,
		      double *u, const int *ldu,
		      double *vt, const int *ldvt,
		      double *work, const int *lwork, int *info);

/* DGESDD - compute the singular value decomposition (SVD); of a   */
/* real M-by-N matrix A, optionally computing the left and/or      */
/* right singular vectors.  If singular vectors are desired, it uses a */
/* divide-and-conquer algorithm.                                   */
void F77_NAME(dgesdd)(const char *jobz,
		      const int *m, const int *n,
		      double *a, const int *lda, double *s,
		      double *u, const int *ldu,
		      double *vt, const int *ldvt,
		      double *work, const int *lwork, int *iwork, int *info);

/* DGETRF - compute an LU factorization of a general M-by-N */
/* matrix A using partial pivoting with row interchanges */
void F77_NAME(dgetrf)(const int* m, const int* n, double* a, const int* lda,
		      int* ipiv, int* info);

#ifdef HAVE_LAPACK

/* DGEEV - compute for an N-by-N real nonsymmetric matrix A, the   */
/* eigenvalues and, optionally, the left and/or right eigenvectors */
void F77_NAME(dgeev)(const char *jobvl, const char *jobvr,
		     const int *n, double *a, const int *lda,
		     double *wr, double *wi,
		     double *vl, const int *ldvl,
		     double *vr, const int *ldvr,
		     double *work, const int *lwork, int *info);

/* DSYEV - compute all eigenvalues and, optionally, eigenvectors   */
/* of a real symmetric matrix A                                    */
void F77_NAME(dsyev)(const char *jobz, const char *uplo,
		     const int *n, double *a, const int *lda,
		     double *w, double *work, const int *lwork,
		     int *info);

/* DSYEVR - compute all eigenvalues and, optionally, eigenvectors   */
/* of a real symmetric matrix A                                    */
void F77_NAME(dsyevr)(const char *jobz, const char *range, const char *uplo,
		      const int *n, double *a, const int *lda,
		      const double *vl, const double *vu,
		      const int *il, const int *iu,
		      const double *abstol, int *m, double *w, 
		      double *z, const int *ldz, int *isuppz, 
		      double *work, const int *lwork,
		      int *iwork, const int *liwork,
		      int *info);
#else /* remapped to avoid conflicts */
void F77_NAME(rgeev)(const char *jobvl, const char *jobvr,
		     const int *n, double *a, const int *lda,
		     double *wr, double *wi,
		     double *vl, const int *ldvl,
		     double *vr, const int *ldvr,
		     double *work, const int *lwork, int *info);
void F77_NAME(rsyev)(const char *jobz, const char *uplo,
                     const int *n, double *a, const int *lda,
                     double *w, double *work, const int *lwork,
                     int *info);
void F77_NAME(rsyevr)(const char *jobz, const char *range, const char *uplo,
                      const int *n, double *a, const int *lda,
                      const double *vl, const double *vu,
                      const int *il, const int *iu,
                      const double *abstol, int *m, double *w,
                      double *z, const int *ldz, int *isuppz,
                      double *work, const int *lwork,
                      int *iwork, const int *liwork,
                      int *info);
#endif

/* DPOTRF - compute the Cholesky factorization of a real */
/* symmetric positive definite matrix A */
void F77_NAME(dpotrf)(const char *uplo, const int *n,
		      double* a, const int *lda, int *info);

/* DPOTRI - compute the inverse of a real symmetric positive */
/* definite matrix A using the Cholesky factorization A = U**T*U */
/* or A = L*L**T computed by DPOTRF */
void F77_NAME(dpotri)(const char *uplo, const int *n,
		      double* a, const int *lda, int *info);

/* DPOTRS - solve a system of linear equations A*X = B with a */
/* symmetric positive definite matrix A using the Cholesky */
/* factorization A = U**T*U or A = L*L**T computed by DPOTRF */
void F77_NAME(dpotrs)(const char *uplo, const int *n,
		      const int *nrhs,
		      const double* a, const int *lda,
		      double* b, const int *ldb, int *info);

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

/* DGECON - estimate the reciprocal of the condition number of a */
/* general real matrix A, in either the 1-norm or the */
/* infinity-norm, using the LU factorization computed by DGETRF */
void F77_NAME(dgecon)(const char* norm, const int* n,
    		  const double* a, const int* lda,
    		  const double* anorm, double* rcond,
    		  double* work, int* iwork, int* info);

/* DGESV computes the solution to a complex system of linear equations */
void F77_NAME(dgesv)(const int *n, const int *nrhs, double *a,
		     const int *lda, int *ipiv, double *b,
		     const int *ldb, int *info);

/*  DGEQP3 computes a QR factorization with column pivoting */
void F77_NAME(dgeqp3)(const int *m, const int *n,
		      double *a, const int *lda,
		      int *jpvt, double *tau,
		      double *work, const int *lwork, int *info);

/* DORMQR applies Q or Q**T from the Left or Right */
void F77_NAME(dormqr)(const char *side, const char *trans,
		      const int *m, const int *n, const int *k,
		      double *a, const int *lda,
		      double *tau,
		      double *c, const int *ldc,
		      double *work, const int *lwork, int *info);

/*  DTRTRS solves triangular systems */
void F77_NAME(dtrtrs)(const char *uplo, const char *trans, const char *diag,
		      const int *n, const int *nrhs,
		      double *a, const int *lda,
		      double *b, const int *ldb,
		      int *info);

/* DLANGE - return the value of the one norm, or the Frobenius */
/* norm, or the infinity norm, or the element of largest absolute */
/* value of a real matrix A */
double
F77_NAME(dlange)(const char* norm, const int* m, const int* n,
		 const double* a, const int* lda, double* work);
