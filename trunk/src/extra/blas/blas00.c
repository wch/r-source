#include <config.h>
#include <R.h>

void blas_dummy()
{
    F77_CALL(dasum)();
    F77_CALL(daxpy)();
    F77_CALL(dcopy)();
    F77_CALL(ddot)();
    F77_CALL(dgbmv)();
    F77_CALL(dgemm)();
    F77_CALL(dgemv)();
    F77_CALL(dger)();
    F77_CALL(dnrm2)();
    F77_CALL(drot)();
    F77_CALL(drotg)();
    F77_CALL(drotm)();
    F77_CALL(drotmg)();
    F77_CALL(dsbmv)();
    F77_CALL(dscal)();
    F77_CALL(dsdot)();
    F77_CALL(dspmv)();
    F77_CALL(dspr)();
    F77_CALL(dspr2)();
    F77_CALL(dswap)();
    F77_CALL(dsymm)();
    F77_CALL(dsymv)();
    F77_CALL(dsyr)();
    F77_CALL(dsyr2)();
    F77_CALL(dsyr2k)();
    F77_CALL(dsyrk)();
    F77_CALL(dtbmv)();
    F77_CALL(dtbsv)();
    F77_CALL(dtpmv)();
    F77_CALL(dtpsv)();
    F77_CALL(dtrmm)();
    F77_CALL(dtrmv)();
    F77_CALL(dtrsm)();
    F77_CALL(dtrsv)();
    F77_CALL(idamax)();
    F77_CALL(lsame)();
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
/* cmplxblas.o */
    F77_CALL(dcabs1)();
    F77_CALL(dzasum)();
    F77_CALL(dznrm2)();
    F77_CALL(izamax)();
    F77_CALL(zaxpy)();
    F77_CALL(zcopy)();
    F77_CALL(zdotc)();
    F77_CALL(zdotu)();
    F77_CALL(zdrot)();
    F77_CALL(zdscal)();
    F77_CALL(zgbmv)();
    F77_CALL(zgemm)();
    F77_CALL(zgemv)();
    F77_CALL(zgerc)();
    F77_CALL(zgeru)();
    F77_CALL(zhbmv)();
    F77_CALL(zhemm)();
    F77_CALL(zhemv)();
    F77_CALL(zher)();
    F77_CALL(zherk)();
    F77_CALL(zher2)();
    F77_CALL(zher2k)();
    F77_CALL(zhpmv)();
    F77_CALL(zhpr)();
    F77_CALL(zhpr2)();
    F77_CALL(zrotg)();
    F77_CALL(zscal)();
    F77_CALL(zswap)();
    F77_CALL(zsymm)();
    F77_CALL(zsyr2k)();
    F77_CALL(zsyrk)();
    F77_CALL(ztbmv)();
    F77_CALL(ztbsv)();
    F77_CALL(ztpmv)();
    F77_CALL(ztpsv)();
    F77_CALL(ztrmm)();
    F77_CALL(ztrmv)();
    F77_CALL(ztrsm)();
    F77_CALL(ztrsv)();
#endif
/* other
    F77_CALL(xerbla)(); */
}
