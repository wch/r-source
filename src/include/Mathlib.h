/*
 *  R : A Computer Langage for Statistical Data Analysis
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef MATHLIB_H_
#define MATHLIB_H_

#include "Arith.h"

#ifdef Macintosh
#include <fp.h>
double hypot(double x, double y);
#else
#include <math.h>
#endif

/* SGI math.h conflict */
#undef qexp

#include <stdlib.h>
#include <limits.h>
#include <float.h>
#include <errno.h>

/* 30 Decimal-place constants computed with bc  (scale=32; proper round) */

#ifndef M_1_SQRT_2
#define M_1_SQRT_2	0.707106781186547524400844362105
#endif

#ifndef M_PI
#define M_PI		3.141592653589793238462643383279
#endif

#ifndef M_PI_half
#define M_PI_half	1.570796326794896619231321691640
#endif

#ifndef M_SQRT_PI
#define M_SQRT_PI	1.772453850905516027298167483341
#endif

#ifndef M_1_SQRT_2PI
#define M_1_SQRT_2PI	0.398942280401432677939946059934
#endif

/* log(sqrt(2*pi)) = log(2*pi)/2 : */
#ifndef M_LN_SQRT_2PI
#define M_LN_SQRT_2PI	0.918938533204672741780329736406
#endif


#ifndef	HAVE_RINT
double rint(double);
#endif

	/* Random Number Generation */

extern double snorm(void);
extern double sunif(void);
extern double sexp(void);

	/* Port Constants */

extern int F77_SYMBOL(i1mach)(int*);
extern double F77_SYMBOL(d1mach)(int*);

	/* Fortran Compatibility */

extern double fint(double);
extern double fmax2(double, double);
extern double fmin2(double, double);
extern double fmod(double, double);
extern double fsign(double, double);
extern double fsquare(double);
extern double fcube(double);
extern int imax2(int, int);
extern int imin2(int, int);

	/* Utilities */

extern double rround(double, double);
extern double prec(double, double);
extern double sign(double);
extern double trunc(double);


	/* Mathematical Special Functions */

extern double beta(double, double);
extern double lbeta(double, double);
extern double gamma(double);
extern double lgamma(double);
extern double lfastchoose(double, double);
extern double fastchoose(double, double);
extern double choose(double, double);
extern double lchoose(double, double);
extern void dpsifn(double, int, int, int, double*, int*, int*);
extern double digamma(double);
extern double trigamma(double);
extern double tetragamma(double);
extern double pentagamma(double);

	/* Distributions */

extern double dbeta(double, double, double);
extern double pbeta(double, double, double);
extern double pbeta_b(double, double, double, double);
extern double qbeta(double, double, double);
extern double rbeta(double, double);

extern double dbinom(double, double, double);
extern double pbinom(double, double, double);
extern double qbinom(double, double, double);
extern double rbinom(double, double);

extern double dcauchy(double, double, double);
extern double pcauchy(double, double, double);
extern double qcauchy(double, double, double);
extern double rcauchy(double, double);

extern double dchisq(double, double);
extern double pchisq(double, double);
extern double qchisq(double, double);
extern double rchisq(double);

extern double dexp(double, double);
extern double pexp(double, double);
extern double qexp(double, double);
extern double rexp(double);

extern double df(double, double, double);
extern double pf(double, double, double);
extern double qf(double, double, double);
extern double rf(double, double);

extern double dgamma(double, double, double);
extern double pgamma(double, double, double);
extern double qgamma(double, double, double);
extern double rgamma(double, double);

extern double dgeom(double, double);
extern double pgeom(double, double);
extern double qgeom(double, double);
extern double rgeom(double);

extern double dhyper(double, double, double, double);
extern double phyper(double, double, double, double);
extern double qhyper(double, double, double, double);
extern double rhyper(double, double, double);

extern double dlnorm(double, double, double);
extern double plnorm(double, double, double);
extern double qlnorm(double, double, double);
extern double rlnorm(double, double);

extern double dlogis(double, double, double);
extern double plogis(double, double, double);
extern double qlogis(double, double, double);
extern double rlogis(double, double);

extern double dnbinom(double, double, double);
extern double pnbinom(double, double, double);
extern double qnbinom(double, double, double);
extern double rnbinom(double, double);

extern double dnchisq(double, double, double);
extern double pnchisq(double, double, double);
extern double qnchisq(double, double, double);
extern double rnchisq(double, double);

extern double dnorm(double, double, double);
extern double pnorm(double, double, double);
extern double qnorm(double, double, double);
extern double rnorm(double, double);

extern double dpois(double, double);
extern double ppois(double, double);
extern double qpois(double, double);
extern double rpois(double);

extern double dt(double, double);
extern double pt(double, double);
extern double qt(double, double);
extern double rt(double);

extern double dunif(double, double, double);
extern double punif(double, double, double);
extern double qunif(double, double, double);
extern double runif(double, double);

extern double dweibull(double, double, double);
extern double pweibull(double, double, double);
extern double qweibull(double, double, double);
extern double rweibull(double, double);

extern int errno;

#endif
