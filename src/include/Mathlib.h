#ifndef MATHLIB_H
#define MATHLIB_H

#include "Arith.h"

#include <errno.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>

/* 30 Decimal-place constants computed with bc -l (scale=32; proper round) */

#ifndef M_SQRT_2
#define M_SQRT_2	1.4142135623730950488016887242097
#define M_1_SQRT_2	0.707106781186547524400844362105	/* 1/sqrt(2) */
#define M_SQRT_32	5.656854249492380195206754896838	/* sqrt(32) */
#endif

#ifndef M_LN_2
#define M_LN_2		0.693147180559945309417232121458176568
#define M_LOG10_2	0.301029995663981195213738894724493027
#endif

#ifndef M_PI
#define M_PI		3.141592653589793238462643383279502884197169399375
#endif
#ifndef M_PI_half
#define M_PI_half	1.570796326794896619231321691640
#endif

#ifndef M_SQRT_PI
/* sqrt(pi),  1/sqrt(2pi),  sqrt(2/pi) : */
#define M_SQRT_PI	1.772453850905516027298167483341
#define M_1_SQRT_2PI	0.398942280401432677939946059934
#define M_SQRT_2dPI	0.79788456080286535587989211986876
#endif


#ifndef M_LN_SQRT_PI
/* log(sqrt(pi)) = log(pi)/2 : */
#define M_LN_SQRT_PI	0.5723649429247000870717136756765293558
/* log(sqrt(2*pi)) = log(2*pi)/2 : */
#define M_LN_SQRT_2PI	0.91893853320467274178032973640562
/* log(sqrt(pi/2)) = log(pi/2)/2 : */
#define M_LN_SQRT_PId2	0.225791352644727432363097614947441
#endif



#define MATHLIB_ERROR(x)   { printf("%s\n",x); exit(1); }
#define MATHLIB_WARNING(x) { printf("%s\n",x); }

#define ME_NONE		0
#define ME_DOMAIN	1
#define ME_RANGE	2
#define ME_NOCONV	3
#define ME_PRECISION	4
#define ME_UNDERFLOW	5

#undef ML_PRECISION_WARNINGS

#ifdef IEEE_754
#ifdef HAVE_IEEE754_H
#include <ieee754.h> /* newer Linuxen */
#else
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h> /* others [Solaris 2.5.x], .. */
#endif
#endif

extern double m_zero;
extern double m_one;
extern double m_tiny;
#define ML_ERROR(x)	/* nothing */
#define ML_POSINF	(m_one / m_zero)
#define ML_NEGINF	((-m_one) / m_zero)
#define ML_NAN		(m_zero / m_zero)
#define ML_UNDERFLOW	(m_tiny * m_tiny)
#define ML_VALID(x)	(!isnan(x))
#else
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
#undef ftrunc
#endif
#ifdef qexp
#undef qexp
#endif

	/* Name Hiding to Avoid Clashes with Fortran */

#ifdef HIDE_NAMES
#define d1mach	c_d1mach
#define i1mach	c_i1mach
#endif

#define	rround	fround
#define	prec	fprec
#define	trunc	ftrunc
/* NO!  fsign(.) has 2 arguments;  sign(.) has 1..
 #define	sign	fsign
*/

	/* Machine Characteristics */

double	d1mach(int);
double	d1mach_(int*);
int	i1mach(int);
int	i1mach_(int*);

	/* General Support Functions */

int	imax2(int, int);
int	imin2(int, int);
double	sign(double);
double	fmax2(double, double);
double	fmin2(double, double);
double	fmod(double, double);
double	fprec(double, double);
double	fround(double, double);
double	ftrunc(double);
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
double	gamma(double);
double	lgamma(double);
void	dpsifn(double, int, int, int, double*, int*, int*);
double	digamma(double);
double	trigamma(double);
double	tetragamma(double);
double	pentagamma(double);

double	choose(double, double);
double	lchoose(double, double);
double	fastchoose(double, double);
double	lfastchoose(double, double);

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

	/* Wilcoxon Distribution */

#define WILCOX_MMAX 50
#define WILCOX_NMAX 50
double dwilcox(double, double, double);
double pwilcox(double, double, double);
double qwilcox(double, double, double);
double rwilcox(double, double);

#endif
