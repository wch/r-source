/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2022 The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* Formerly part of platform.c */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <float.h> // -> FLT_RADIX

/* Machine Constants */

#define DTYPE double
#define MACH_NAME machar
#define ABS fabs
#include "machar.c"
#undef DTYPE
#undef MACH_NAME
#undef ABS

#ifdef HAVE_LONG_DOUBLE
# define DTYPE long double
# define MACH_NAME machar_LD
# define ABS fabsl
# include "machar.c"
# undef DTYPE
# undef MACH_NAME
# undef ABS
#endif

#ifdef  USE_INTERNAL_MKTIME
// for R_time_t
# include "datetime.h"
#else
// for time_t
# include <time.h>
#endif

attribute_hidden void Init_R_Machine(SEXP rho)
{
    machar(&R_AccuracyInfo.ibeta,
	   &R_AccuracyInfo.it,
	   &R_AccuracyInfo.irnd,
	   &R_AccuracyInfo.ngrd,
	   &R_AccuracyInfo.machep,
	   &R_AccuracyInfo.negep,
	   &R_AccuracyInfo.iexp,
	   &R_AccuracyInfo.minexp,
	   &R_AccuracyInfo.maxexp,
	   &R_AccuracyInfo.eps,
	   &R_AccuracyInfo.epsneg,
	   &R_AccuracyInfo.xmin,
	   &R_AccuracyInfo.xmax);

    R_dec_min_exponent = (int) floor(log10(R_AccuracyInfo.xmin)); /* smallest decimal exponent */

    /*
#ifdef HAVE_LONG_DOUBLE
# define MACH_SIZE 18+10
#else
# define MACH_SIZE 18
#endif
    */
    int MACH_SIZE = 19;
    if (sizeof(LDOUBLE) > sizeof(double)) MACH_SIZE += 10;
    
    SEXP ans = PROTECT(allocVector(VECSXP, MACH_SIZE)),
	 nms = PROTECT(allocVector(STRSXP, MACH_SIZE));

    SET_STRING_ELT(nms, 0, mkChar("double.eps"));
    SET_VECTOR_ELT(ans, 0, ScalarReal(R_AccuracyInfo.eps));

    SET_STRING_ELT(nms, 1, mkChar("double.neg.eps"));
    SET_VECTOR_ELT(ans, 1, ScalarReal(R_AccuracyInfo.epsneg));

    SET_STRING_ELT(nms, 2, mkChar("double.xmin"));
    SET_VECTOR_ELT(ans, 2, ScalarReal(R_AccuracyInfo.xmin));

    SET_STRING_ELT(nms, 3, mkChar("double.xmax"));
    SET_VECTOR_ELT(ans, 3, ScalarReal(R_AccuracyInfo.xmax));

    SET_STRING_ELT(nms, 4, mkChar("double.base"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(R_AccuracyInfo.ibeta));

    SET_STRING_ELT(nms, 5, mkChar("double.digits"));
    SET_VECTOR_ELT(ans, 5, ScalarInteger(R_AccuracyInfo.it));

    SET_STRING_ELT(nms, 6, mkChar("double.rounding"));
    SET_VECTOR_ELT(ans, 6, ScalarInteger(R_AccuracyInfo.irnd));

    SET_STRING_ELT(nms, 7, mkChar("double.guard"));
    SET_VECTOR_ELT(ans, 7, ScalarInteger(R_AccuracyInfo.ngrd));

    SET_STRING_ELT(nms, 8, mkChar("double.ulp.digits"));
    SET_VECTOR_ELT(ans, 8, ScalarInteger(R_AccuracyInfo.machep));

    SET_STRING_ELT(nms, 9, mkChar("double.neg.ulp.digits"));
    SET_VECTOR_ELT(ans, 9, ScalarInteger(R_AccuracyInfo.negep));

    SET_STRING_ELT(nms, 10, mkChar("double.exponent"));
    SET_VECTOR_ELT(ans, 10, ScalarInteger(R_AccuracyInfo.iexp));

    SET_STRING_ELT(nms, 11, mkChar("double.min.exp"));
    SET_VECTOR_ELT(ans, 11, ScalarInteger(R_AccuracyInfo.minexp));

    SET_STRING_ELT(nms, 12, mkChar("double.max.exp"));
    SET_VECTOR_ELT(ans, 12, ScalarInteger(R_AccuracyInfo.maxexp));

    SET_STRING_ELT(nms, 13, mkChar("integer.max"));
    SET_VECTOR_ELT(ans, 13, ScalarInteger(INT_MAX));

    SET_STRING_ELT(nms, 14, mkChar("sizeof.long"));
    SET_VECTOR_ELT(ans, 14, ScalarInteger(SIZEOF_LONG));

    SET_STRING_ELT(nms, 15, mkChar("sizeof.longlong"));
    SET_VECTOR_ELT(ans, 15, ScalarInteger(SIZEOF_LONG_LONG));

    SET_STRING_ELT(nms, 16, mkChar("sizeof.longdouble"));
#ifdef HAVE_LONG_DOUBLE
    SET_VECTOR_ELT(ans, 16, ScalarInteger(SIZEOF_LONG_DOUBLE));
#else
    SET_VECTOR_ELT(ans, 16, ScalarInteger(0));
#endif

    SET_STRING_ELT(nms, 17, mkChar("sizeof.pointer"));
    SET_VECTOR_ELT(ans, 17, ScalarInteger(sizeof(SEXP)));

    SET_STRING_ELT(nms, 18, mkChar("sizeof.time_t"));
#ifdef  USE_INTERNAL_MKTIME
    SET_VECTOR_ELT(ans, 18, ScalarInteger(sizeof(R_time_t)));
#else
    SET_VECTOR_ELT(ans, 18, ScalarInteger(sizeof(time_t)));
#endif
/* This used to be just
#ifdef HAVE_LONG_DOUBLE
   but platforms can have the type and it be identical to double
   (as on ARM).  So do the same as capabilities("long.double")
*/
#ifdef HAVE_LONG_DOUBLE
    if (sizeof(LDOUBLE) > sizeof(double)) {
	static struct {
	    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
	    long double eps, epsneg, xmin, xmax;
	} R_LD_AccuracyInfo;
	
	machar_LD(&R_LD_AccuracyInfo.ibeta,
		  &R_LD_AccuracyInfo.it,
		  &R_LD_AccuracyInfo.irnd,
		  &R_LD_AccuracyInfo.ngrd,
		  &R_LD_AccuracyInfo.machep,
		  &R_LD_AccuracyInfo.negep,
		  &R_LD_AccuracyInfo.iexp,
		  &R_LD_AccuracyInfo.minexp,
		  &R_LD_AccuracyInfo.maxexp,
		  &R_LD_AccuracyInfo.eps,
		  &R_LD_AccuracyInfo.epsneg,
		  &R_LD_AccuracyInfo.xmin,
		  &R_LD_AccuracyInfo.xmax);
#define PT1 19
	SET_STRING_ELT(nms, PT1+0, mkChar("longdouble.eps"));
	SET_VECTOR_ELT(ans, PT1+0, ScalarReal((double) R_LD_AccuracyInfo.eps));

	SET_STRING_ELT(nms, PT1+1, mkChar("longdouble.neg.eps"));
	SET_VECTOR_ELT(ans, PT1+1, ScalarReal((double) R_LD_AccuracyInfo.epsneg));
	SET_STRING_ELT(nms, PT1+2, mkChar("longdouble.digits"));
	SET_VECTOR_ELT(ans, PT1+2, ScalarInteger(R_LD_AccuracyInfo.it));

	SET_STRING_ELT(nms, PT1+3, mkChar("longdouble.rounding"));
	SET_VECTOR_ELT(ans, PT1+3, ScalarInteger(R_LD_AccuracyInfo.irnd));

	SET_STRING_ELT(nms, PT1+4, mkChar("longdouble.guard"));
	SET_VECTOR_ELT(ans, PT1+4, ScalarInteger(R_LD_AccuracyInfo.ngrd));

	SET_STRING_ELT(nms, PT1+5, mkChar("longdouble.ulp.digits"));
	SET_VECTOR_ELT(ans, PT1+5, ScalarInteger(R_LD_AccuracyInfo.machep));

	SET_STRING_ELT(nms, PT1+6, mkChar("longdouble.neg.ulp.digits"));
	SET_VECTOR_ELT(ans, PT1+6, ScalarInteger(R_LD_AccuracyInfo.negep));

	SET_STRING_ELT(nms, PT1+7, mkChar("longdouble.exponent"));
	SET_VECTOR_ELT(ans, PT1+7, ScalarInteger(R_LD_AccuracyInfo.iexp));

	SET_STRING_ELT(nms, PT1+8, mkChar("longdouble.min.exp"));
	SET_VECTOR_ELT(ans, PT1+8, ScalarInteger(R_LD_AccuracyInfo.minexp));

	SET_STRING_ELT(nms, PT1+9, mkChar("longdouble.max.exp"));
	SET_VECTOR_ELT(ans, PT1+9, ScalarInteger(R_LD_AccuracyInfo.maxexp));

    }
#endif

    setAttrib(ans, R_NamesSymbol, nms);
    defineVar(install(".Machine"), ans, rho);
    UNPROTECT(2);
}

