/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2000  The R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

/* Private header file for use during compilation of Mathlib */
#ifndef MATHLIB_PRIVATE_H
#define MATHLIB_PRIVATE_H

#include <Rconfig.h>
#define MATHLIB_PRIVATE
#include "R_ext/Mathlib.h"
#undef  MATHLIB_PRIVATE
#include "R_ext/RS.h"

#ifndef MATHLIB_STANDALONE
/* Mathlib in R */

#include "R_ext/Error.h"
# define MATHLIB_ERROR(fmt,x)		error(fmt,x);
# define MATHLIB_WARNING(fmt,x)		warning(fmt,x)
# define MATHLIB_WARNING2(fmt,x,x2)	warning(fmt,x,x2)
# define MATHLIB_WARNING3(fmt,x,x2,x3)	warning(fmt,x,x2,x3)
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) warning(fmt,x,x2,x3,x4)

#include "R_ext/Arith.h"
#define ML_POSINF	R_PosInf
#define ML_NEGINF	R_NegInf
#define ML_NAN		R_NaN

#ifdef IEEE_754
#define ML_ERROR(x)	/* nothing */
#define ML_UNDERFLOW	(DBL_MIN * DBL_MIN)
#define ML_VALID(x)	(!ISNAN(x))
#else
void ml_error(int n);
#define ML_ERROR(x)	ml_error(x)
#define ML_UNDERFLOW	0
#define ML_VALID(x)	(errno == 0)
#endif

#else
/* Mathlib standalone */

#include <stdio.h>
# define MATHLIB_ERROR(fmt,x)	{ printf(fmt,x); exit(1); }
# define MATHLIB_WARNING(fmt,x)		printf(fmt,x)
# define MATHLIB_WARNING2(fmt,x,x2)	printf(fmt,x,x2)
# define MATHLIB_WARNING3(fmt,x,x2,x3)	printf(fmt,x,x2,x3)
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) printf(fmt,x,x2,x3,x4)

#define ISNAN(x)       R_IsNaNorNA(x)
#define R_FINITE(x)    R_finite(x)
int R_IsNaNorNA(double);
int R_finite(double);

#ifdef IEEE_754
#define ML_POSINF	(1.0 / 0.0)
#define ML_NEGINF	((-1.0) / 0.0)
#define ML_NAN		(0.0 / 0.0)
#else
#define ML_POSINF	DBL_MAX
#define ML_NEGINF	(-DBL_MAX)
#define ML_NAN		(-DBL_MAX*(1-1e-15))
#endif

#endif /* standalone */

#ifdef IEEE_754
#define ML_ERROR(x)	/* nothing */
#define ML_UNDERFLOW	(DBL_MIN * DBL_MIN)
#define ML_VALID(x)	(!ISNAN(x))
#else/*--- NO IEEE: No +/-Inf, NAN,... ---*/
void ml_error(int n);
#define ML_ERROR(x)	ml_error(x)
#define ML_UNDERFLOW	0
#define ML_VALID(x)	(errno == 0)
#endif

#define ME_NONE		0
/*	no error */
#define ME_DOMAIN	1
/*	argument out of domain */
#define ME_RANGE	2
/*	value out of range */
#define ME_NOCONV	4
/*	process did not converge */
#define ME_PRECISION	8
/*	does not have "full" precision */
#define ME_UNDERFLOW	16
/*	and underflow occured (important for IEEE)*/

#define ML_ERR_return_NAN { ML_ERROR(ME_DOMAIN); return ML_NAN; }

/* Wilcoxon Rank Sum Distribution */

#define WILCOX_MAX 50

/* Wilcoxon Signed Rank Distribution */

#define SIGNRANK_MAX 50

/* Formerly private part of Mathlib.h */

/* always remap internal functions */
#define bd0       	Rf_bd0
#define chebyshev_eval	Rf_chebyshev_eval
#define chebyshev_init	Rf_chebyshev_init
#define fastchoose	Rf_fastchoose
#define i1mach		Rf_i1mach
#define gammalims	Rf_gammalims
#define lfastchoose	Rf_lfastchoose
#define lgammacor	Rf_lgammacor
#define stirlerr       	Rf_stirlerr

	/* Chebyshev Series */

int	chebyshev_init(double*, int, double);
double	chebyshev_eval(double, const double *, const int);

	/* Gamma and Related Functions */

void	gammalims(double*, double*);
double	lgammacor(double); /* log(gamma) correction */
double  stirlerr(double);  /* Stirling expansion "error" */

double	fastchoose(double, double);
double	lfastchoose(double, double);

double  bd0(double, double);

/* Consider adding these two to the API (Rmath.h): */
double	dbinom_raw(double, double, double, double, int);
double	dpois_raw (double, double, int);
double  pnchisq_raw(double, double, double, double, int);

int	i1mach(int);


#endif /* MATHLIB_PRIVATE_H */
