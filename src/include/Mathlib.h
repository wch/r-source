/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-1999 R Development Core Team
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

 * Mathlib.h  should contain ALL headers from R's C code in `src/nmath'
   ---------  such that ``the Math library'' can be used by simply

   ``#include "Mathlib.h" ''

   and nothing else.
*/
#ifndef MATHLIB_H
#define MATHLIB_H

/*-- Mathlib as part of R --  undefine this for standalone : */
#define MATHLIB_IN_R

#include "Arith.h"
#include "Random.h"

#ifdef FORTRAN_H
#error __MUST__include "Mathlib.h"  _before_  "Fortran.h"
#endif

#include <errno.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>

/* TRUE and FALSE conflict with the Mac --- Fortran.h still defines them... */
#define LTRUE	(1)
#define LFALSE	(0)

/* 30 Decimal-place constants */
/* Computed with bc -l (scale=32; proper round) */

/* SVID & X/Open Constants */
/* Names from Solaris math.h */

#ifndef M_E
#define M_E		2.718281828459045235360287471353	/* e */
#endif

#ifndef M_LOG2E
#define M_LOG2E		1.442695040888963407359924681002	/* log2(e) */
#endif

#ifndef M_LOG10E
#define M_LOG10E	0.434294481903251827651128918917	/* log10(e) */
#endif

#ifndef M_LN2
#define M_LN2		0.693147180559945309417232121458	/* ln(2) */
#endif

#ifndef M_LN10
#define M_LN10		2.302585092994045684017991454684	/* ln(10) */
#endif

#ifndef M_PI
#define M_PI		3.141592653589793238462643383280	/* pi */
#endif

#ifndef M_PI_2
#define M_PI_2		1.570796326794896619231321691640	/* pi/2 */
#endif

#ifndef M_PI_4
#define M_PI_4		0.785398163397448309615660845820	/* pi/4 */
#endif

#ifndef M_1_PI
#define M_1_PI		0.318309886183790671537767526745	/* 1/pi */
#endif

#ifndef M_2_PI
#define M_2_PI		0.636619772367581343075535053490	/* 1/pi */
#endif

#ifndef M_2_SQRTPI
#define M_2_SQRTPI	1.128379167095512573896158903122	/* 1/sqrt(pi) */
#endif

#ifndef M_SQRT2
#define M_SQRT2		1.414213562373095048801688724210	/* sqrt(2) */
#endif

#ifndef M_SQRT1_2
#define M_SQRT1_2	0.707106781186547524400844362105	/* 1/sqrt(2) */
#endif

/* Other, R-Specific Constants */
/* Note there are some repeats of values above */
/* Needs a cleanup */

#ifndef M_1_SQRT_2
#define M_1_SQRT_2	0.707106781186547524400844362105	/* 1/sqrt(2) */
#endif

#ifndef M_SQRT_32
#define M_SQRT_32	5.656854249492380195206754896838	/* sqrt(32) */
#endif

#ifndef M_LOG10_2
#define M_LOG10_2	0.301029995663981195213738894724	/* log10(2) */
#endif

#ifndef M_PI_half
#define M_PI_half	1.570796326794896619231321691640	/* pi/2 */
#endif

#ifndef M_SQRT_PI
#define M_SQRT_PI	1.772453850905516027298167483341	/* sqrt(pi) */
#endif

#ifndef M_1_SQRT_2PI
#define M_1_SQRT_2PI	0.398942280401432677939946059934	/* 1/sqrt(2pi) */
#endif

#ifndef M_SQRT_2dPI
#define M_SQRT_2dPI	0.797884560802865355879892119869	/* sqrt(2/pi) */
#endif


#ifndef M_LN_SQRT_PI
#define M_LN_SQRT_PI	0.572364942924700087071713675677	/* log(sqrt(pi)) */
#endif

#ifndef M_LN_SQRT_2PI
#define M_LN_SQRT_2PI	0.918938533204672741780329736406	/* log(sqrt(2*pi)) */
#endif

#ifndef M_LN_SQRT_PId2
#define M_LN_SQRT_PId2	0.225791352644727432363097614947	/* log(sqrt(pi/2)) */
#endif


#ifdef MATHLIB_IN_R/* Mathlib in R */

#include "Error.h"
# define MATHLIB_ERROR(fmt,x)		error(fmt,x);
# define MATHLIB_WARNING(fmt,x)		warning(fmt,x)
# define MATHLIB_WARNING2(fmt,x,x2)	warning(fmt,x,x2)
# define MATHLIB_WARNING3(fmt,x,x2,x3)	warning(fmt,x,x2,x3)
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) warning(fmt,x,x2,x3,x4)

#else/* Mathlib standalone */

#include <stdio.h>
# define MATHLIB_ERROR(fmt,x)	{ printf(fmt,x); exit(1) }
# define MATHLIB_WARNING(fmt,x)		printf(fmt,x)
# define MATHLIB_WARNING2(fmt,x,x2)	printf(fmt,x,x2)
# define MATHLIB_WARNING3(fmt,x,x2,x3)	printf(fmt,x,x2,x3)
# define MATHLIB_WARNING4(fmt,x,x2,x3,x4) printf(fmt,x,x2,x3,x4)
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


#ifdef IEEE_754

# ifdef HAVE_IEEE754_H
#  include <ieee754.h> /* newer Linuxen */
# else
#  ifdef HAVE_IEEEFP_H
#   include <ieeefp.h> /* others [Solaris 2.5.x], .. */
#  endif
# endif

extern double m_zero;
extern double m_one;
/* extern double m_tiny; */
#define ML_ERROR(x)	/* nothing */
#define ML_POSINF	(m_one / m_zero)
#define ML_NEGINF	((-m_one) / m_zero)
#define ML_NAN		(m_zero / m_zero)
#define ML_UNDERFLOW	(DBL_MIN * DBL_MIN)
#define ML_VALID(x)	(!isnan(x))

#else/*--- NO IEEE: No +/-Inf, NAN,... ---*/
#define ML_ERROR(x)	ml_error(x)
#define ML_POSINF	DBL_MAX
#define ML_NEGINF	(-DBL_MAX)
#define ML_NAN		(-DBL_MAX)
#define ML_UNDERFLOW	0
#define ML_VALID(x)	(errno == 0)
#endif

	/* Splus Compatibility */

#define snorm	norm_rand
#define sunif	unif_rand
#define sexp	exp_rand

	/* Undo SGI Madness */

#ifdef ftrunc
# undef ftrunc
#endif
#ifdef qexp
# undef qexp
#endif
#ifdef qgamma
# undef qgamma
#endif

	/* Name Hiding to Avoid Clashes with Fortran */

#ifdef HIDE_NAMES
# define d1mach	c_d1mach
# define i1mach	c_i1mach
#endif

#define	rround	fround
#define	prec	fprec
#undef trunc
#define	trunc	ftrunc


	/* Utilities for `dpq' handling (density/probability/quantile) */

#define R_D__0 (give_log ? ML_NEGINF : 0.)
#define R_D__1 (give_log ? 0. : 1.)
#define R_DT_0 (lower_tail ? R_D__0 : R_D__1)
#define R_DT_1 (lower_tail ? R_D__1 : R_D__0)

#define R_D_val(x)   (give_log	 ? log(x) : x)	      /*  x  */
#define R_D_log(x)   (give_log	 ?  x	  : exp(x))   /* log(x) */

#define R_DT_val(x)  R_D_val(lower_tail ? x	 : 1. - x) /*  x  */
#define R_DT_Cval(x) R_D_val(lower_tail ? 1. - x : x)	   /*  1 - x */
#define R_DT_log(x)  R_D_log(lower_tail ? x	 : 1. - x) /* log(x) */
#define R_DT_Clog(x) R_D_log(lower_tail ? 1. - x : x)	   /* log(1 - x) */

#define R_D_give_log(dd)    (((int)dd) >> 1) /* Extract ``give_log'' flag */
#define R_D_lower_tail(dd)  (((int)dd) % 2)  /* Extract ``lower_tail'' flag */

	/* R's version of C functions: */

double R_log(double x);
double R_pow(double x, double y);

	/* Machine Characteristics */

double	d1mach(int);
double	d1mach_(int*);
int	i1mach(int);
int	i1mach_(int*);

	/* General Support Functions */

int	imax2(int, int);
int	imin2(int, int);
double	fmax2(double, double);
double	fmin2(double, double);
double	fmod(double, double);
double	fprec(double, double);
double	fround(double, double);
double	ftrunc(double);
double	sign(double);
double	fsign(double, double);
double	fsquare(double);
double	fcube(double);

	/* Random Number Generators */

double	snorm(void);
double	sunif(void);
double	sexp(void);

	/* Chebyshev Series */

int	chebyshev_init(double*, int, double);
double	chebyshev_eval(double, double *, int);

	/* Gamma and Related Functions */

double	logrelerr(double);
void	gammalims(double*, double*);
double	lgammacor(double);
double	gammafn(double);
double	gamma_cody(double);
double	lgammafn(double);
void	dpsifn(double, int, int, int, double*, int*, int*);
double	digamma(double);
double	trigamma(double);
double	tetragamma(double);
double	pentagamma(double);

double	choose(double, double);
double	lchoose(double, double);
double	fastchoose(double, double);
double	lfastchoose(double, double);

	/* Bessel Functions of All Kinds */

double	bessel_i(double, double, double);
double	bessel_j(double, double);
double	bessel_k(double, double, double);
double	bessel_y(double, double);
void	I_bessel(double*, double*, long*, long*, double*, long*);
void	J_bessel(double*, double*, long*,	 double*, long*);
void	K_bessel(double*, double*, long*, long*, double*, long*);
void	Y_bessel(double*, double*, long*,	 double*, long*);

	/* Beta and Related Functions */

double	beta(double, double);
double	lbeta(double, double);

	/* Normal Distribution */

double	dnorm(double, double, double);
double	pnorm(double, double, double);
double	qnorm(double, double, double);
double	rnorm(double, double);

	/* Uniform Distribution */

double	dunif(double, double, double);
double	punif(double, double, double);
double	qunif(double, double, double);
double	runif(double, double);

	/* Gamma Distribution */

double	dgamma(double, double, double);
double	pgamma(double, double, double);
double	qgamma(double, double, double);
double	rgamma(double, double);

	/* Beta Distribution */

double	dbeta(double, double, double);
double	pbeta(double, double, double);
double	pbeta_raw(double, double, double);
double	qbeta(double, double, double);
double	rbeta(double, double);

	/* Lognormal Distribution */

double	dlnorm(double, double, double);
double	plnorm(double, double, double);
double	qlnorm(double, double, double);
double	rlnorm(double, double);

	/* Chi-squared Distribution */

double	dchisq(double, double);
double	pchisq(double, double);
double	qchisq(double, double);
double	rchisq(double);

	/* Non-central Chi-squared Distribution */

double	dnchisq(double, double, double);
double	pnchisq(double, double, double);
double	qnchisq(double, double, double);
double	rnchisq(double, double);

	/* F Distibution */

double	df(double, double, double);
double	pf(double, double, double);
double	qf(double, double, double);
double	rf(double, double);

	/* Student t Distibution */

double	dt(double, double);
double	pt(double, double);
double	qt(double, double);
double	rt(double);

	/* Binomial Distribution */

double	dbinom(double, double, double);
double	pbinom(double, double, double);
double	qbinom(double, double, double);
double	rbinom(double, double);

	/* Cauchy Distribution */

double	dcauchy(double, double, double);
double	pcauchy(double, double, double);
double	qcauchy(double, double, double);
double	rcauchy(double, double);

	/* Exponential Distribution */

double	dexp(double, double);
double	pexp(double, double);
double	qexp(double, double);
double	rexp(double);

	/* Geometric Distribution */

double	dgeom(double, double);
double	pgeom(double, double);
double	qgeom(double, double);
double	rgeom(double);

	/* Hypergeometric Distibution */

double	dhyper(double, double, double, double);
double	phyper(double, double, double, double);
double	qhyper(double, double, double, double);
double	rhyper(double, double, double);

	/* Negative Binomial Distribution */

double	dnbinom(double, double, double);
double	pnbinom(double, double, double);
double	qnbinom(double, double, double);
double	rnbinom(double, double);

	/* Poisson Distribution */

double	dpois(double, double);
double	ppois(double, double);
double	qpois(double, double);
double	rpois(double);

	/* Weibull Distribution */

double	dweibull(double, double, double);
double	pweibull(double, double, double);
double	qweibull(double, double, double);
double	rweibull(double, double);

	/* Logistic Distribution */

double	dlogis(double, double, double);
double	plogis(double, double, double);
double	qlogis(double, double, double);
double	rlogis(double, double);

	/* Non-central Beta Distribution */

double	dnbeta(double, double, double, double);
double	pnbeta(double, double, double, double);
double	qnbeta(double, double, double, double);
double	rnbeta(double, double, double);

	/* Non-central F Distribution */

double	dnf(double, double, double, double);
double	pnf(double, double, double, double);
double	qnf(double, double, double, double);
double	rnf(double, double, double);

	/* Non-central Student t Distribution */

double	dnt(double, double, double);
double	pnt(double, double, double);
double	qnt(double, double, double);
double	rnt(double, double);

	/* Studentized Range Distribution */

double	dtukey(double, double, double, double);
double	ptukey(double, double, double, double);
double	qtukey(double, double, double, double);
double	rtukey(double, double, double);

/* Wilcoxon Rank Sum Distribution */

#define WILCOX_MAX 50
double dwilcox(double, double, double);
double pwilcox(double, double, double);
double qwilcox(double, double, double);
double rwilcox(double, double);

/* Wilcoxon Signed Rank Distribution */

#define SIGNRANK_MAX 50
double dsignrank(double, double);
double psignrank(double, double);
double qsignrank(double, double);
double rsignrank(double);

#endif
