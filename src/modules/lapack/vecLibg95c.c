#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <AvailabilityMacros.h> /* for MAC_OS_X_VERSION_10_* -- present on 10.2+ (according to Apple) */
/* Since macOS 10.8 vecLib requires Accelerate to be included first (which in turn includes vecLib) */
#if defined MAC_OS_X_VERSION_10_8 && MAC_OS_X_VERSION_MIN_REQUIRED >= 1040
#include <Accelerate/Accelerate.h>
#else
#include <vecLib/vecLib.h>
#endif

/* These calls were deprcated in 'new' Accelerate and are warned about, so
   suppress the warnings 

   FIXME: define ACCELERATE_NEW_LAPACK where appropriate
   *and* use new entry points.
*/

#ifdef __clang__
# pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

void FC_FUNC_(rcblas_cdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu) 
{ cblas_cdotu_sub(*N, X, *incX, Y, *incY, dotu); }

void FC_FUNC_(rcblas_cdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_cdotc_sub(*N, X, *incX, Y, *incY, dotc); }

void FC_FUNC_(rcblas_zdotu_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotu)
{ cblas_zdotu_sub(*N, X, *incX, Y, *incY, dotu); }

void FC_FUNC_(rcblas_zdotc_sub,)(const int *N, const void *X, const int *incX,
                       const void *Y, const int *incY, void *dotc)
{ cblas_zdotc_sub(*N, X, *incX, Y, *incY, dotc); }
