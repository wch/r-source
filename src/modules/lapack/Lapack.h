/* C declarations of principal Lapack routines,
 * i.e.,
 * those with a .Call() interface defined via ./Lapack.c
 * (and half a dozen more declaration/interface/registration files
 */

#include <R_ext/Complex.h>
#include <R_ext/RS.h>

/* use declarations in R_ext/Lapack.h (instead of having them there *and* here)
  but ``delete'' the 'extern' there : */
#define La_extern
#define BLAS_extern

#include <R_ext/Lapack.h>

#undef La_extern
#undef BLAS_extern
