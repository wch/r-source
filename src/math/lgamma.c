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

#include "Mathlib.h"

/*----  lgamma(x)  :=  log  | gamma(x) |   ------------*/

int signgam = 0; /* the sign of gamma(x), set in lgamma(.), used by gamma(.) */

static double Gam_pos(double);
static double lGam_neg(double);
static double asform(double); /* ASymptotic FORM */

double lgamma(double x)
{
	signgam = 1.0;
	if (x <= 0.0) return (lGam_neg(x));
	if (x >  8.0) return (asform(x));
	return (log(Gam_pos(x)));
}

/* Coefficients  from Cheney and Hart (??--REF---??) */
/* Asymptotic form: (not quite, why are the coeff. slightly changed ? */
#define M 6
static double p1[] =
{
	0.83333333333333101837e-1,
	-.277777777735865004e-2,
	0.793650576493454e-3,
	-.5951896861197e-3,
	0.83645878922e-3,
	-.1633436431e-2,
};
static double asform(double x)
{
  /* Equation 6.1.41 Abramowitz and Stegun -- extended Stirling */
  /* See also ACM algorithm 291 */

	double log();
	double nfac, xsq;
	int i;

	xsq = 1. / (x * x);
	for (nfac = 0, i = M - 1; i >= 0; i--) {
		nfac = nfac * xsq + p1[i];
	}
	return ((x - .5) * log(x) - x + M_LN_SQRT_2PI + nfac / x);
}

static double lGam_neg(double x)
{ /* log |gamma(x)|   for  x <= 0   ### G(-x) = - pi / [ G(x) x  sin(pi x) ] */
	double sinpx;
	double log(), sin(), Gam_pos();
 
	x = -x;
	sinpx = sin(M_PI * x);
	if (sinpx == 0.0)
		DOMAIN_ERROR;
	if (sinpx < 0.0)
		sinpx = -sinpx;
	else
		signgam = -1;
	return (-log(x * Gam_pos(x) * sinpx / M_PI));
}

/* Coefficients for rational approximation  gamma(x),  2 <= x <= 3 : */
#define N 8
static double p2[] =
{
	-.42353689509744089647e5,
	-.20886861789269887364e5,
	-.87627102978521489560e4,
	-.20085274013072791214e4,
	-.43933044406002567613e3,
	-.50108693752970953015e2,
	-.67449507245925289918e1,
	0.0,
};
static double q2[] =
{
	-.42353689509744090010e5,
	-.29803853309256649932e4,
	0.99403074150827709015e4,
	-.15286072737795220248e4,
	-.49902852662143904834e3,
	0.18949823415702801641e3,
	-.23081551524580124562e2,
	0.10000000000000000000e1,
};

/* gamma(x) for x >= 0 :*/
static double Gam_pos(double x)
{
	double n, d, s;
	register int i;

	if (x < 2.0) return (Gam_pos(x + 1.0) / x);
	if (x > 3.0) return ((x - 1.0) * Gam_pos(x - 1.0));
	/*-- rational approximation for  2 <= x <= 3 */
	s = x - 2.;
	for (n = 0, d = 0, i = N - 1; i >= 0; i--) {
		n = n * s + p2[i];
		d = d * s + q2[i];
	}
	return (n / d);
}
