#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vecLib/vecLib.h>

void F77_FUNC_(rcblas_cdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu) 
{ cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu); };

void F77_FUNC_(rcblas_cdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc); };

void F77_FUNC_(rcblas_zdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu)
{ cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu); };

void F77_FUNC_(rcblas_zdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc); };
