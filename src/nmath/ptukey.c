/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double ptukey(q, rr, cc, df);
 *
 *  DESCRIPTION
 *
 *    Computes the probability that the maximum of rr studentized
 *    ranges, each based on cc means and with df degrees of freedom
 *    for the standard error, is less than q.
 *
 *    The algorithm is based on that of the reference.
 *
 *  REFERENCE
 *
 *    Copenhaver, Margaret Diponzio & Holland, Burt S.
 *    Multiple comparisons of simple effects in
 *    the two-way analysis of variance with fixed effects.
 *    Journal of Statistical Computation and Simulation,
 *    Vol.30, pp.1-15, 1988.
 */

/*
 *  This function calculates probability integral of Hartley's
 *  form of the range.
 *
 *  w     = value of range
 *  rr    = no. of rows or groups
 *  cc    = no. of columns or treatments
 *  ir    = error flag = 1 if wprob probability > 1
 *  wprob = returned probability integral from (0, w)
 *
 *  program will not terminate if ir is raised.
 *
 *  bb = upper limit of legendre integration
 *  eps = maximum acceptable value of integral
 *  nleg = order of legendre quadrature
 *  ihalf = int ((nleg + 1) / 2)
 *  wlar = value of range above which wincr1 intervals are used to
 *         calculate second part of integral,
 *         else wincr2 intervals are used.
 *  eps1, eps2, eps3 = values which are used as cutoffs for terminating
 *  or modifying a calculation.
 *
 *  M_1_SQRT_2PI = 1 / sqrt(2 * pi);  from abramowitz & stegun, p. 3.
 *  M_SQRT2 = sqrt(2)
 *  xleg = legendre 12-point nodes
 *  aleg = legendre 12-point coefficients
 */

#include "Mathlib.h"

#define nleg	12
#define ihalf	6

static double wprob(double w, double rr, double cc)
{
    static double eps  =   1.0;
    static double eps1 = -30.0;
    static double eps2 = -50.0;
    static double eps3 =  60.0;
    static double bb = 8.0;
    static double wlar = 3.0;
    static double wincr1 = 2.0;
    static double wincr2 = 3.0;
    static double xleg[ihalf] = {
	0.981560634246719250690549090149e0,
	0.904117256370474856678465866119e0,
	0.769902674194304687036893833213e0,
	0.587317954286617447296702418941e0,
	0.367831498998180193752691536644e0,
	0.125233408511468915472441369464e0
    };
    static double aleg[ihalf] = {
	0.047175336386511827194615961485,
	0.106939325995318430960254718194,
	0.160078328543346226334652529543,
	0.203167426723065921749064455810,
	0.233492536538354808760849898925,
	0.249147045813402785000562436043
    };
    double a, ac, ans, b, binc, blb, bub, c, cc1, einsum, elsum,
	pminus, pplus, qexpo, qsqz, rinsum, wi, wincr, xx;
    int j, jj;

    qsqz = w * 0.5;

    /* if w >= 16 then the integral lower bound (occurs for c=20) */
    /* is 0.99999999999995 so return a value of 1. */

    ans = 1.0;
    if (qsqz >= bb)
	return 1.0;

    /* find (f(w/2) - 1) ** cc */
    /* (first term in integral of hartley's form). */

    /* if ans ** cc < 2e-22 then set ans = 0 */

    ans = erf(qsqz / M_SQRT2);
    if (ans >= exp(eps2 / cc))
	ans = pow(ans, cc);
    else
	ans = 0.0;

    /* if w is large then the second component of the */
    /* integral is small, so fewer intervals are needed. */

    if (w > wlar)
	wincr = wincr1;
    else
	wincr = wincr2;

    /* find the integral of second term of hartley's form */
    /* for the integral of the range for equal-length */
    /* intervals using legendre quadrature.  limits of */
    /* integration are from (w/2, 8).  two or three */
    /* equal-length intervals are used. */

    /* blb and bub are lower and upper limits of integration. */

    blb = qsqz;
    binc = (bb - qsqz) / wincr;
    bub = blb + binc;
    einsum = 0.0;

    /* integrate over each interval */

    cc1 = cc - 1.0;
    for (wi = 1; wi <= wincr; wi++) {
	elsum = 0.0;
	a = 0.5 * (bub + blb);

	/* legendre quadrature with order = nleg */

	b = 0.5 * (bub - blb);

	for (jj = 1; jj <= nleg; jj++) {
	    if (ihalf < jj) {
		j = (nleg - jj) + 1;
		xx = xleg[j-1];
	    } else {
		j = jj;
		xx = -xleg[j-1];
	    }
	    c = b * xx;
	    ac = a + c;

	    /* if exp(-qexpo/2) < 9e-14, */
	    /* then doesn't contribute to integral */

	    qexpo = ac * ac;

	    if (qexpo > eps3)
		break;
	    if (ac > 0.0)
		pplus = 1.0 + erf(ac / M_SQRT2);
	    else
		pplus = erfc(-(ac / M_SQRT2));

	    if (ac > w)
		pminus = 1.0 + erf((ac / M_SQRT2) - (w / M_SQRT2));
	    else
		pminus = erfc((w / M_SQRT2) - (ac / M_SQRT2));

	    /* if rinsum ** (cc-1) < 9e-14, */
	    /* then doesn't contribute to integral */

	    rinsum = (pplus * 0.5) - (pminus * 0.5);
	    if (rinsum >= exp(eps1 / cc1)) {
		rinsum = (aleg[j-1] * exp(-(0.5 * qexpo)))
		    * pow(rinsum, cc1);
		elsum = elsum + rinsum;
	    }
	}
	elsum = (((2.0 * b) * cc) * M_1_SQRT_2PI) * elsum;
	einsum = einsum + elsum;
	blb = bub;
	bub = bub + binc;
    }

    /* if ans ** rr < 9e-14, then return 0.0 */

    ans = einsum + ans;
    if (ans <= exp(eps1 / rr)) {
	return 0.0;
    }
    ans = pow(ans, rr);
    if (ans >= eps)
	ans = 1.0;
    return ans;
}

/*
 *  function qprob
 *
 *  q = value of studentized range
 *  rr = no. of rows or groups
 *  cc = no. of columns or treatments
 *  df = degrees of freedom of error term
 *  ir[0] = error flag = 1 if wprob probability > 1
 *  ir[1] = error flag = 1 if qprob probability > 1
 *
 *  qprob = returned probability integral over [0, q]
 *
 *  The program will not terminate if ir[0] or ir[1] are raised.
 *
 *  All references in wprob to Abramowitz and Stegun
 *  are from the following reference:
 *
 *  Abramowitz, Milton and Stegun, Irene A.
 *  Handbook of Mathematical Functions.
 *  New York:  Dover publications, Inc. (1970).
 *
 *  All constants taken from this text are
 *  given to 25 significant digits.
 *
 *  nlegq = order of legendre quadrature
 *  ihalfq = int ((nlegq + 1) / 2)
 *  eps = max. allowable value of integral
 *  eps1 & eps2 = values below which there is
 *                no contribution to integral.
 *
 *  d.f. <= dhaf:   integral is divided into ulen1 length intervals.  else
 *  d.f. <= dquar:  integral is divided into ulen2 length intervals.  else
 *  d.f. <= deigh:  integral is divided into ulen3 length intervals.  else
 *  d.f. <= dlarg:  integral is divided into ulen4 length intervals.
 *
 *  d.f. > dlarg:   the range is used to calculate integral.
 *
 *  M_LN2 = log(2)
 *
 *  xlegq = legendre 16-point nodes
 *
 *  alegq = legendre 16-point coefficients
 *
 *  The coefficients and nodes for the legendre quadrature used in
 *  qprob and wprob were calculated using the algorithms found in:
 *
 *  Stroud, A. H. and Secrest, D.
 *  Gaussian Quadrature Formulas.
 *  Englewood Cliffs,
 *  New Jersey:  Prentice-Hall, Inc, 1966.
 *
 *  All values matched the tables (provided in same reference)
 *  to 30 significant digits.
 *
 *  f(x) = .5 + erf(x / sqrt(2)) / 2      for x > 0
 *
 *  f(x) = erfc( -x / sqrt(2)) / 2        for x < 0
 *
 *  where f(x) is standard normal c. d. f.
 *
 *  if degrees of freedom large, approximate integral
 *  with range distribution.
 */

#define nlegq	16
#define ihalfq	8

double ptukey(double q, double rr, double cc, double df)
{
    static double eps = 1.0e0;
    static double eps1 = -30.0e0;
    static double eps2 = 1.0e-14;
    static double dhaf = 100.0e0;
    static double dquar = 800.0e0;
    static double deigh = 5000.0e0;
    static double dlarg = 25000.0e0;
    static double ulen1 = 1.0e0;
    static double ulen2 = 0.5e0;
    static double ulen3 = 0.25e0;
    static double ulen4 = 0.125e0;
    static double xlegq[ihalfq] = {
	0.989400934991649932596154173450e+00,
	0.944575023073232576077988415535e+00,
	0.865631202387831743880467897712e+00,
	0.755404408355003033895101194847e+00,
	0.617876244402643748446671764049e+00,
	0.458016777657227386342419442984e+00,
	0.281603550779258913230460501460e+00,
	0.950125098376374401853193354250e-01
    };
    static double alegq[ihalfq] = {
	0.271524594117540948517805724560e-01,
	0.622535239386478928628438369944e-01,
	0.951585116824927848099251076022e-01,
	0.124628971255533872052476282192e+00,
	0.149595988816576732081501730547e+00,
	0.169156519395002538189312079030e+00,
	0.182603415044923588866763667969e+00,
	0.189450610455068496285396723208e+00
    };
    double ans, f2, f21, f2lf, ff4, otsum, qsqz, rotsum,
	t1, twa1, ulen, wprb;
    int i, j, jj;

#ifdef IEEE_754
    if (ISNAN(q) || ISNAN(rr) || ISNAN(cc) || ISNAN(df)) {
        ML_ERROR(ME_DOMAIN);
        return q + rr + cc + df;
    }
#endif
    if (q <= 0)
	return 0;

    /* df must be > 1 */
    /* there must be at least two values */

    if (df < 2 || rr < 1 || cc < 2) {
        ML_ERROR(ME_DOMAIN);
        return ML_NAN;
    }

#ifdef IEEE_754
    if(!FINITE(q))
	return 1;
#endif

    if (df > dlarg) {
	ans = wprob(q, rr, cc);
	return ans;
    }

    /* calculate leading constant */
    /* lgamma is the log gamma function. */

    f2 = df * 0.5;
    f2lf = ((f2 * log(df)) - (df * M_LN2)) - lgammafn(f2);
    f21 = f2 - 1.0;

    /* integral is divided into unit, half-unit, quarter-unit, or */
    /* eighth-unit length intervals depending on the value of the */
    /* degrees of freedom. */

    ff4 = df * 0.25;
    if (df <= dhaf) {
	ulen = ulen1;
    } else if (df <= dquar) {
	ulen = ulen2;
    } else if (df <= deigh) {
	ulen = ulen3;
    } else {
	ulen = ulen4;
    }

    f2lf = f2lf + log(ulen);

    /* integrate over each subinterval */

    ans = 0.0;

    for (i = 1; i <= 50; i++) {
	otsum = 0.0;

	/* legendre quadrature with order = nlegq */
	/* nodes (stored in xlegq) are symmetric around zero. */

	twa1 = ((2.0 * i) - 1.0) * ulen;

	for (jj = 1; jj <= nlegq; jj++) {
	    if (ihalfq < jj) {
		j = jj - ihalfq - 1;
		t1 = (f2lf + (f21 * log(twa1 + (xlegq[j] * ulen))))
		    - (((xlegq[j] * ulen) + twa1) * ff4);
	    } else {
		j = jj - 1;
		t1 = (f2lf + (f21 * log(twa1 - (xlegq[j] * ulen))))
		    + (((xlegq[j] * ulen) - twa1) * ff4);

	    }

	    /* if exp(t1) < 9e-14, then doesn't */
	    /* contribute to integral */

	    if (t1 >= eps1) {
		if (ihalfq < jj) {
		    qsqz = q * sqrt(((xlegq[j] * ulen) + twa1) * 0.5);
		} else {
		    qsqz = q * sqrt(((-(xlegq[j] * ulen)) + twa1) * 0.5);


		}

		/* call wprob to find integral */
		/* of range portion */

		wprb = wprob(qsqz, rr, cc);
		rotsum = (wprb * alegq[j]) * exp(t1);
		otsum = rotsum + otsum;
	    }
	    /* end legendre integral for interval i */
	    /* L200: */
	}

	/* if integral for interval i < 1e-14, */
	/* then stop. however, in order to avoid */
	/* small area under left tail, at least */
	/* 1 / ulen intervals are calculated. */

	if (i * ulen >= 1.0 && otsum <= eps2)
	    goto L400;

	/* end of interval i */
	/* L330: */

	ans = ans + otsum;
    }
  L400:
    if (ans > eps)
	ans = 1.0;
    return ans;
}
