/* It seems Cygwin does not allow mutually dependent dlls, so fake xerbla */

#include <R_ext/RS.h>

/* Fortran-callable error routine for lapack */

void F77_NAME(xerbla)(const char *srname, int *info)
{
   /* srname is not null-terminated.  It should be 6 characters. */
    char buf[7];
    strncpy(buf, srname, 6);
    buf[6] = '\0';
    printf("BLAS/LAPACK routine '%6s' gave error code %d", buf, -(*info));
}
