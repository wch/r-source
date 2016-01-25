/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000		R Core Team
 *  Copyright (C) 2003		The R Foundation
 *  based on AS 89 (C) 1975 Royal Statistical Society
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
 *
 */

#include <math.h>
#include <Rmath.h>

/* Was
	double precision function prho(n, is, ifault)

 Changed to subroutine by KH to allow for .Fortran interfacing.
 Also, change `prho' argument in the code to `pv'.
 And, fix a bug.

 From R ver. 1.1.x [March, 2000] by MM:
 - Translate Fortran to C
 - use pnorm() instead of less precise alnorm().
 - new argument lower_tail --> potentially increased precision in extreme cases.
*/
void
prho(int n, double is, double *pv, int ifault, int lower_tail)
{
/*	Algorithm AS 89	  Appl. Statist. (1975) Vol.24, No. 3, P377.

	To evaluate the probability  Pr[ S >= is ]
	{or Pr [ S < is]  if(lower_tail) }, where

	S   = (n^3 - n) * (1-R)/6,
	is  = (n^3 - n) * (1-r)/6,
	R,r = Spearman's rho (r.v. and observed),  and	n >= 2
*/

    /* Edgeworth coefficients : */
    const double
	c1 = .2274,
	c2 = .2531,
	c3 = .1745,
	c4 = .0758,
	c5 = .1033,
	c6 = .3932,
	c7 = .0879,
	c8 = .0151,
	c9 = .0072,
	c10= .0831,
	c11= .0131,
	c12= 4.6e-4;

    /* Local variables */
    double b, u, x, y, n3;/*, js */

#define n_small 9
/* originally: n_small = 6 (speed!);
 * even n_small = 10 (and n = 10) needs quite a bit longer than the approx!
 * larger than 12 ==> integer overflow in nfac and (probably) ifr
*/
    int l[n_small];
    int nfac, i, m, mt, ifr, ise, n1;

    /* Test admissibility of arguments and initialize */
    *pv = lower_tail ? 0. : 1.;
    if (n <= 1) { ifault = 1; return; }

    ifault = 0;
    if (is <= 0.) return;/* with p = 1 */

    n3 = (double)n;
    n3 *= (n3 * n3 - 1.) / 3.;/* = (n^3 - n)/3 */
    if (is > n3) { /* larger than maximal value */
	*pv = 1 - *pv; return;
    }
    /* NOT rounding to even anymore:  with ties, S, may even be non-integer!
     * js = is;
     * if(fmod(js, 2.) != 0.) ++js;
     */
    if (n <= n_small) { /* 2 <= n <= n_small :
			  * Exact evaluation of probability */
	nfac = 1.;
	for (i = 1; i <= n; ++i) {
	    nfac *= i;
	    l[i - 1] = i;
	}
	/* KH mod next line: was `!=' in the code but `.eq.' in the paper */
	if (is == n3) {
	    ifr = 1;
	}
	else {
	    ifr = 0;
	    for (m = 0; m < nfac; ++m) {
		ise = 0;
		for (i = 0; i < n; ++i) {
		    n1 = i + 1 - l[i];
		    ise += n1 * n1;
		}
		if (is <= ise)
		    ++ifr;

		n1 = n;
		do {
		    mt = l[0];
		    for (i = 1; i < n1; ++i)
			l[i - 1] = l[i];
		    --n1;
		    l[n1] = mt;
		} while (mt == n1+1 && n1 > 1);
	    }
	}
	*pv = (lower_tail ? nfac-ifr : ifr) / (double) nfac;
    } /* exact for n <= n_small */

    else { /* n >= 7 :	Evaluation by Edgeworth series expansion */

	y = (double) n;
	b = 1 / y;
	x = (6. * (is - 1) * b / (y * y - 1) - 1) * sqrt(y - 1);
	/* = rho * sqrt(n-1)  ==  rho / sqrt(var(rho))  ~  (0,1) */
	y = x * x;
	u = x * b * (c1 + b * (c2 + c3 * b) +
		     y * (-c4 + b * (c5 + c6 * b) -
			  y * b * (c7 + c8 * b -
				   y * (c9 - c10 * b + y * b * (c11 - c12 * y))
			      )));
	y = u / exp(y / 2.);
	*pv = (lower_tail ? -y : y) +
	    pnorm(x, 0., 1., lower_tail, /*log_p = */FALSE);
	/* above was call to alnorm() [algorithm AS 66] */
	if (*pv < 0) *pv = 0.;
	if (*pv > 1) *pv = 1.;
    }
    return;
} /* prho */

#include <Rinternals.h>
SEXP pRho(SEXP q, SEXP sn, SEXP lower)
{
    double s = asReal(q), p;
    int n = asInteger(sn), ltail = asInteger(lower), ifault = 0;
    prho(n, s, &p, ifault, ltail);
    return ScalarReal(p);
}
