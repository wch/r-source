#include <R_ext/BLAS.h>

double 
DASUM(const int *n, const double *dx, const int *incx);

void 
DAXPY(const int *n, const double *alpha,
      const double *dx, const int *incx,
      double *dy, const int *incy);

void 
DCOPY(const int *n, const double *dx, const int *incx,
      double *dy, const int *incy);

double 
DDOT(const int *n, const double *dx, const int *incx, 
     const double *dy, const int *incy);

double 
DNRM2(const int *n, const double *dx, const int *incx); 

void 
DROT(const int *n, double *dx, const int *incx,
     double *dy, const int *incy, const double *c, const double *s);

void 
DROTG(const double *a, const double *b, double *c, double *s);


void 
DROTM(const int *n, double *dx, const int *incx,
      double *dy, const int *incy, const double *dparam);

void 
DROTMG(const double *dd1, const double *dd2, const double *dx1, 
       const double *dy1, double *dparam);

void 
DSCAL(const int *n, const double *alpha, double *dx, const int *incx);

void 
DSWAP(const int *n, double *dx, const int *incx,
      double *dy, const int *incy);

int 
IDAMAX(const int *n, const double *dx, const int *incx);

/* Double Precision Level 2 BLAS */

void 
DGBMV(const char *trans, const int *m, const int *n,
      const int *kl,const int *ku,
      const double *alpha, const double *a, const int *lda,
      const double *x, const int *incx,
      const double *beta, double *y, const int *incy);

void 
DGEMV(const char *trans, const int *m, const int *n,
      const double *alpha, const double *a, const int *lda,
      const double *x, const int *incx, const double *beta,
      double *y, const int *incy); 

void 
DSBMV(const char *uplo, const int *n, const int *k,
      const double *alpha, const double *a, const int *lda,
      const double *x, const int *incx,
      const double *beta, double *y, const int *incy);

void 
DSPMV(const char *uplo, const int *n,
      const double *alpha, const double *ap,
      const double *x, const int *incx,
      const double *beta, double *y, const int *incy);

void 
DSYMV(const char *uplo, const int *n, const double *alpha,
      const double *a, const int *lda,
      const double *x, const int *incx,
      const double *beta, double *y, const int *incy);

void 
DTBMV(const char *uplo, const char *trans,
      const char *diag, const int *n, const int *k,
      const double *a, const int *lda,
      double *x, const int *incx); 

void  
DTPMV(const char *uplo, const char *trans, const char *diag,
      const int *n, const double *ap, 
      double *x, const int *incx);

void 
DTRMV(const char *uplo, const char *trans, const char *diag,
      const int *n, const double *a, const int *lda,
      double *x, const int *incx);

void 
DTBSV(const char *uplo, const char *trans,
      const char *diag, const int *n, const int *k,
      const double *a, const int *lda,
      double *x, const int *incx); 

void 
DTPSV(const char *uplo, const char *trans,
      const char *diag, const int *n,
      const double *ap, double *x, const int *incx);

void 
DTRSV(const char *uplo, const char *trans,
      const char *diag, const int *n,
      const double *a, const int *lda,
      double *x, const int *incx); 

void 
DGER(const int *m, const int *n, const double *alpha,
     double *x, const int *incx,
     double *y, const int *incy,
     double *a, const int *lda);

void 
DSYR(const char *uplo, const int *n, const double *alpha,
     const double *x, const int *incx,
     double *a, const int *lda);

void 
DSPR(const char *uplo, const int *n, const double *alpha,
     const double *x, const int *incx, double *ap);

void 
DSYR2(const char *uplo, const int *n, const double *alpha,
      const double *x, const int *incx,
      const double *y, const int *incy,
      double *a, const int *lda); 

void 
DSPR2(const char *uplo, const int *n, const double *alpha,
      const double *x, const int *incx,
      const double *y, const int *incy, double *ap);

/* Double Precision Level 3 BLAS */

void 
DGEMM(const char *transa, const char *transb, const int *m,
      const int *n, const int *k, const double *alpha,
      const double *a, const int *lda,
      const double *b, const int *ldb,
      const double *beta, double *c, const int *ldc);

void 
DTRSM(const char *side, const char *uplo,
      const char *transa, const char *diag,
      const int *m, const int *n, const double *alpha,
      const double *a, const int *lda,
      double *b, const int *ldb);

void 
DTRMM(const char *side, const char *uplo, const char *transa,
      const char *diag, const int *m, const int *n,
      const double *alpha, const double *a, const int *lda,
      double *b, const int *ldb); 

void 
DSYMM(const char *side, const char *uplo, const int *m,
      const int *n, const double *alpha,
      const double *a, const int *lda,
      const double *b, const int *ldb,
      const double *beta, double *c, const int *ldc);

void 
DSYRK(const char *uplo, const char *trans,
      const int *n, const int *k,
      const double *alpha, const double *a, const int *lda,
      const double *beta, double *c, const int *ldc);

void 
DSYR2K(const char *uplo, const char *trans,
       const int *n, const int *k,
       const double *alpha, const double *a, const int *lda,
       const double *b, const int *ldb,
       const double *beta, double *c, const int *ldc);

/* ================================================================== */

double 
DZASUM(const int *n, const Rcomplex *zx, const int *incx);

double 
DZNRM2(const int *n, const Rcomplex *zx, const int *incx);

int 
IZAMAX(const int *n, const Rcomplex *zx, const int *incx);

void 
ZAXPY(const int *n, const Rcomplex *alpha,
      const Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy);

void 
ZCOPY(const int *n, const Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy);

Rcomplex 
ZDOTC(const int *n, const Rcomplex *dx, const int *incx, 
     const Rcomplex *dy, const int *incy);

Rcomplex 
ZDOTU(const int *n, const Rcomplex *dx, const int *incx, 
     const Rcomplex *dy, const int *incy);

void 
ZDSCAL(const int *n, const double *alpha, Rcomplex *dx, const int *incx);

void 
ZGEMM(const char *transa, const char *transb, const int *m,
      const int *n, const int *k, const Rcomplex *alpha,
      const Rcomplex *a, const int *lda,
      const Rcomplex *b, const int *ldb,
      const Rcomplex *beta, Rcomplex *c, const int *ldc);

void 
ZGEMV(const char *trans, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      const Rcomplex *x, const int *incx, const Rcomplex *beta,
      Rcomplex *y, const int *incy); 

void 
ZGERC(const int *m, const int *n, const Rcomplex *alpha,
      Rcomplex *x, const int *incx,
      Rcomplex *y, const int *incy,
      Rcomplex *a, const int *lda);

void 
ZHEMV(const char *uplo, const int *n, const Rcomplex *alpha,
      const int *lda, Rcomplex *x, const int *incx, const Rcomplex *beta,
      Rcomplex *y, const int *incy);

void 
ZHER2(const char *uplo, const int *n, const Rcomplex *alpha,
      Rcomplex *x, const int *incx,
      Rcomplex *y, const int *incy, 
      Rcomplex *a, const int *lda);

void 
ZHER2K(const char *uplo, const char *trans, const int *n, const int *k,
       const Rcomplex *alpha, Rcomplex *a, const int *lda,
       Rcomplex *b, const int *ldb, const Rcomplex *beta,
       Rcomplex *c, const int *ldc);

void 
ZTRMM(const char *side, const char *uplo, const char *transa,
      const char *diag, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      Rcomplex *b, const int *ldb); 

void 
ZTRMV(const char *uplo, const char *trans, const char *diag, const int *n,
      Rcomplex *a, const int *lda,  Rcomplex *x, const int *incx);

void 
ZTRSM(const char *side, const char *uplo, const char *transa,
      const char *diag, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      Rcomplex *b, const int *ldb); 

void 
ZTRSV(const char *uplo, const char *trans, const char *diag, const int *n,
      Rcomplex *a, const int *lda,  Rcomplex *x, const int *incx);


double 
DCABS1(const Rcomplex *z);

void 
ZROT(const int *n, Rcomplex *dx, const int *incx,
     Rcomplex *dy, const int *incy, const Rcomplex *c, const Rcomplex *s);

void 
ZROTG(const Rcomplex *a, const Rcomplex *b, Rcomplex *c, Rcomplex *s);

void 
ZSCAL(const int *n, const Rcomplex *alpha, Rcomplex *dx, const int *incx);

void 
ZSWAP(const int *n, Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy);

int LSAME(const char *ca, const char *cb);


/* ================================================================== */

/* Double Precision Level 1 BLAS */
double 
F77_NAME(dasum)(const int *n, const double *dx, const int *incx)
{
    return DASUM(n, dx, incx);
}


void 
F77_NAME(daxpy)(const int *n, const double *alpha,
		const double *dx, const int *incx,
		double *dy, const int *incy)
{
    DAXPY(n, alpha, dx, incx, dy, incy);
}


void 
F77_NAME(dcopy)(const int *n, const double *dx, const int *incx,
		double *dy, const int *incy)
{
    DCOPY(n, dx, incx, dy, incy);
}

double 
F77_NAME(ddot)(const int *n, const double *dx, const int *incx, 
	       const double *dy, const int *incy)
{
    return DDOT(n, dx, incx, dy, incy);
}


double 
F77_NAME(dnrm2)(const int *n, const double *dx, const int *incx)
{
    return DNRM2(n, dx, incx);
}


void 
F77_NAME(drot)(const int *n, double *dx, const int *incx,
	       double *dy, const int *incy, const double *c, const double *s)
{
    DROT(n, dx, incx, dy, incy, c, s);
}


void 
F77_NAME(drotg)(const double *a, const double *b, double *c, double *s)
{
    DROTG(a, b, c, s);
}


void 
F77_NAME(dscal)(const int *n, const double *alpha, double *dx, const int *incx)
{
    DSCAL(n, alpha, dx, incx);
}

void 
F77_NAME(dswap)(const int *n, double *dx, const int *incx,
		double *dy, const int *incy)
{
    DSWAP(n, dx, incx, dy, incy);
}


int F77_NAME(idamax)(const int *n, const double *dx, const int *incx)
{
    return IDAMAX(n, dx, incx);
}



/* Double Precision Level 2 BLAS */

void
F77_NAME(dgbmv)(const char *trans, const int *m, const int *n,
		const int *kl,const int *ku,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy)
{
    DGBMV(trans, m, n, kl,ku, alpha, a, lda, x, incx, beta, y, incy);
    
}


void
F77_NAME(dgemv)(const char *trans, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx, const double *beta,
		double *y, const int *incy)
{
    DGEMV(trans, m, n, alpha, a, lda, x, incx, beta, y, incy);
}


void
F77_NAME(dsbmv)(const char *uplo, const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy)
{
    DSBMV(uplo, n, k, alpha, a, lda, x, incx, beta, y, incy);
}


void
F77_NAME(dspmv)(const char *uplo, const int *n,
		const double *alpha, const double *ap,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy)
{
    DSPMV(uplo, n, alpha, ap, x, incx, beta, y, incy);
}


void
F77_NAME(dsymv)(const char *uplo, const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *x, const int *incx,
		const double *beta, double *y, const int *incy)
{
    DSYMV(uplo, n, alpha, a, lda, x, incx, beta, y, incy);
}


void
F77_NAME(dtbmv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx)
{
    DTBMV(uplo, trans, diag, n, k, a, lda, x, incx);
}


void 
F77_NAME(dtpmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *ap, 
		double *x, const int *incx)
{
    DTPMV(uplo, trans, diag, n, ap, x, incx);
}


void
F77_NAME(dtrmv)(const char *uplo, const char *trans, const char *diag,
		const int *n, const double *a, const int *lda,
		double *x, const int *incx)
{
    DTRMV(uplo, trans, diag, n, a, lda, x, incx);
}


void
F77_NAME(dtbsv)(const char *uplo, const char *trans,
		const char *diag, const int *n, const int *k,
		const double *a, const int *lda,
		double *x, const int *incx)
{
    DTBSV(uplo, trans, diag, n, k, a, lda, x, incx);
}


void
F77_NAME(dtpsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *ap, double *x, const int *incx)
{
    DTPSV(uplo, trans, diag, n, ap, x, incx);
}


void
F77_NAME(dtrsv)(const char *uplo, const char *trans,
		const char *diag, const int *n,
		const double *a, const int *lda,
		double *x, const int *incx)
{
    DTRSV(uplo, trans, diag, n, a, lda, x, incx);
}


void
F77_NAME(dger)(const int *m, const int *n, const double *alpha,
	       double *x, const int *incx,
	       double *y, const int *incy,
	       double *a, const int *lda)
{
    DGER(m, n, alpha, x, incx, y, incy, a, lda);
}


void 
F77_NAME(dsyr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx,
	       double *a, const int *lda)
{
    DSYR(uplo, n, alpha, x, incx, a, lda);
}


void
F77_NAME(dspr)(const char *uplo, const int *n, const double *alpha,
	       const double *x, const int *incx, double *ap)
{
    DSPR(uplo, n, alpha, x, incx, ap);
}


void
F77_NAME(dsyr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy,
		double *a, const int *lda)
{
    DSYR2(uplo, n, alpha, x, incx, y, incy, a, lda);
}


void
F77_NAME(dspr2)(const char *uplo, const int *n, const double *alpha,
		const double *x, const int *incx,
		const double *y, const int *incy, double *ap)
{
    DSPR2(uplo, n, alpha, x, incx, y, incy, ap);
}


/* Double Precision Level 3 BLAS */

void 
F77_NAME(dgemm)(const char *transa, const char *transb, const int *m,
		const int *n, const int *k, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc)
{
    DGEMM(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}

void
F77_NAME(dtrsm)(const char *side, const char *uplo,
		const char *transa, const char *diag,
		const int *m, const int *n, const double *alpha,
		const double *a, const int *lda,
		double *b, const int *ldb)
{
    DTRSM(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb);
}

void
F77_NAME(dtrmm)(const char *side, const char *uplo, const char *transa,
		const char *diag, const int *m, const int *n,
		const double *alpha, const double *a, const int *lda,
		double *b, const int *ldb)
{
    DTRMM(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb);
}

void
F77_NAME(dsymm)(const char *side, const char *uplo, const int *m,
		const int *n, const double *alpha,
		const double *a, const int *lda,
		const double *b, const int *ldb,
		const double *beta, double *c, const int *ldc)
{
    DSYMM(side, uplo, m, n, alpha, a, lda, b, ldb, beta, c, ldc);
}

void
F77_NAME(dsyrk)(const char *uplo, const char *trans,
		const int *n, const int *k,
		const double *alpha, const double *a, const int *lda,
		const double *beta, double *c, const int *ldc)
{
    DSYRK(uplo, trans, n, k, alpha, a, lda, beta, c, ldc);
}

void
F77_NAME(dsyr2k)(const char *uplo, const char *trans,
		 const int *n, const int *k,
		 const double *alpha, const double *a, const int *lda,
		 const double *b, const int *ldb,
		 const double *beta, double *c, const int *ldc)
{
    DSYR2K(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}


/* Double Complex BLAS

double 
F77_NAME(dzasum)(const int *n, const Rcomplex *zx, const int *incx)
{
    return DZASUM(n, zx, incx);
}

double 
F77_NAME(dznrm2)(const int *n, const Rcomplex *zx, const int *incx)
{
    return DZNRM2(n, zx, incx);
}


int 
F77_NAME(izamax)(const int *n, const Rcomplex *zx, const int *incx)
{
    return IZAMAX(n, zx, incx);
}


void 
F77_NAME(zaxpy)(const int *n, const Rcomplex *alpha,
      const Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy)
{
    ZAXPY(n, alpha, dx, incx, dy, incy);
}

void 
F77_NAME(zcopy)(const int *n, const Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy)
{
    ZCOPY(n, dx, incx, dy, incy);
}

Rcomplex 
F77_NAME(zdotc)(const int *n, const Rcomplex *dx, const int *incx, 
		const Rcomplex *dy, const int *incy)
{
    return ZDOTC(n, dx, incx, dy, incy);
}

Rcomplex 
F77_NAME(zdotu)(const int *n, const Rcomplex *dx, const int *incx, 
		const Rcomplex *dy, const int *incy)
{
    return ZDOTU(n, dx, incx, dy, incy);
}

void 
F77_NAME(zdscal)(const int *n, const double *alpha, Rcomplex *dx, const int *incx)
{
    ZDSCAL(n, alpha, dx, incx);
}

void 
F77_NAME(zgemm)(const char *transa, const char *transb, const int *m,
      const int *n, const int *k, const Rcomplex *alpha,
      const Rcomplex *a, const int *lda,
      const Rcomplex *b, const int *ldb,
      const Rcomplex *beta, Rcomplex *c, const int *ldc)
{
    ZGEMM(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}

void 
F77_NAME(zgemv)(const char *trans, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      const Rcomplex *x, const int *incx, const Rcomplex *beta,
      Rcomplex *y, const int *incy)
{
    ZGEMV(trans, m, n, alpha, a, lda, x, incx, beta, y, incy);
}

void 
F77_NAME(zgerc)(const int *m, const int *n, const Rcomplex *alpha,
      Rcomplex *x, const int *incx,
      Rcomplex *y, const int *incy,
      Rcomplex *a, const int *lda)
{
    ZGERC(m, n, alpha, x, incx, y, incy, a, lda);
}

void 
F77_NAME(zhemv)(const char *uplo, const int *n, const Rcomplex *alpha,
      const int *lda, Rcomplex *x, const int *incx, const Rcomplex *beta,
      Rcomplex *y, const int *incy)
{
    ZHEMV(uplo, n, alpha, lda, x, incx, beta, y, incy);
}

void 
F77_NAME(zher2)(const char *uplo, const int *n, const Rcomplex *alpha,
      Rcomplex *x, const int *incx,
      Rcomplex *y, const int *incy, 
      Rcomplex *a, const int *lda)
{
    ZHER2(uplo, n, alpha, x, incx, y, incy, a, lda);
}

void 
F77_NAME(zher2k)(const char *uplo, const char *trans, const int *n, const int *k,
       const Rcomplex *alpha, Rcomplex *a, const int *lda,
       Rcomplex *b, const int *ldb, const Rcomplex *beta,
       Rcomplex *c, const int *ldc)
{
    ZHER2K(uplo, trans, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}

void 
F77_NAME(ztrmm)(const char *side, const char *uplo, const char *transa,
      const char *diag, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      Rcomplex *b, const int *ldb)
{
    ZTRMM(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb);
}

void 
F77_NAME(ztrmv)(const char *uplo, const char *trans, const char *diag, const int *n,
      Rcomplex *a, const int *lda,  Rcomplex *x, const int *incx)
{
    ZTRMV(uplo, trans, diag, n, a, lda,  x, incx);
}

void 
F77_NAME(ztrsm)(const char *side, const char *uplo, const char *transa,
      const char *diag, const int *m, const int *n,
      const Rcomplex *alpha, const Rcomplex *a, const int *lda,
      Rcomplex *b, const int *ldb)
{
    ZTRSM(side, uplo, transa, diag, m, n, alpha, a,lda, b, ldb);
}

void 
F77_NAME(ztrsv)(const char *uplo, const char *trans, const char *diag, const int *n,
      Rcomplex *a, const int *lda,  Rcomplex *x, const int *incx)
{
    ZTRSV(uplo, trans, diag, n, a, lda,  x, incx);
}

double 
F77_NAME(dcabs1)(const Rcomplex *z)
{
    return DCABS1(z);
}

void 
F77_NAME(zscal)(const int *n, const Rcomplex *alpha, Rcomplex *dx, const int *incx)
{
    ZSCAL(n, alpha, dx, incx);
}

void 
F77_NAME(zswap)(const int *n, Rcomplex *dx, const int *incx,
      Rcomplex *dy, const int *incy)
{
    ZSWAP(n, dx, incx, dy, incy);
}

*/

int
F77_NAME(lsame)(const char *ca, const char *cb)
{
    return LSAME(ca, cb);
}

void 
F77_NAME(drotm)(const int *n, double *dx, const int *incx,
		double *dy, const int *incy, const double *dparam)
{
    DROTM(n, dx, incx, dy, incy, dparam);
}


void 
F77_NAME(drotmg)(const double *dd1, const double *dd2, const double *dx1, 
		 const double *dy1, double *dparam)
{
    DROTMG(dd1, dd2, dx1, dy1, dparam);
}


