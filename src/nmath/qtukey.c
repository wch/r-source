/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *  based in part on AS70 (C) 1974 Royal Statistical Society
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *	#include "Mathlib.h"
 *	double qtukey(p, rr, cc, df, lower_tail, log_p);
 *
 *  DESCRIPTION
 *
 *	Computes the quantiles of the maximum of rr studentized
 *	ranges, each based on cc means and with df degrees of freedom
 *	for the standard error, is less than q.
 *
 *	The algorithm is based on that of the reference.
 *
 *  REFERENCE
 *
 *	Copenhaver, Margaret Diponzio & Holland, Burt S.
 *	Multiple comparisons of simple effects in
 *	the two-way analysis of variance with fixed effects.
 *	Journal of Statistical Computation and Simulation,
 *	Vol.30, pp.1-15, 1988.
 */

#include "nmath.h"
#include "dpq.h"

/* qinv() :
 *	this function finds percentage point of the studentized range
 *	which is used as initial estimate for the secant method.
 *	function is adapted from portion of algorithm as 70
 *	from applied statistics (1974) ,vol. 23, no. 1
 *	by odeh, r. e. and evans, j. o.
 *
 *	  p = percentage point
 *	  c = no. of columns or treatments
 *	  v = degrees of freedom
 *	  qinv = returned initial estimate
 *
 *	vmax is cutoff above which degrees of freedom
 *	is treated as infinity.
 */

static double qinv(double p, double c, double v)
{
    static const double p0 = 0.322232421088;
    static const double q0 = 0.993484626060e-01;
    static const double p1 = -1.0;
    static const double q1 = 0.588581570495;
    static const double p2 = -0.342242088547;
    static const double q2 = 0.531103462366;
    static const double p3 = -0.204231210125;
    static const double q3 = 0.103537752850;
    static const double p4 = -0.453642210148e-04;
    static const double q4 = 0.38560700634e-02;
    static const double c1 = 0.8832;
    static const double c2 = 0.2368;
    static const double c3 = 1.214;
    static const double c4 = 1.208;
    static const double c5 = 1.4142;
    static const double vmax = 120.0;

    double ps, q, t, yi;

    ps = 0.5 - 0.5 * p;
    yi = sqrt (log (1.0 / (ps * ps)));
    t = yi + (((( yi * p4 + p3) * yi + p2) * yi + p1) * yi + p0)
	   / (((( yi * q4 + q3) * yi + q2) * yi + q1) * yi + q0);
    if (v < vmax) t += (t * t * t + t) / v / 4.0;
    q = c1 - c2 * t;
    if (v < vmax) q += -c3 / v + c4 * t / v;
    return t * (q * log (c - 1.0) + c5);
}

/*
 *  Copenhaver, Margaret Diponzio & Holland, Burt S.
 *  Multiple comparisons of simple effects in
 *  the two-way analysis of variance with fixed effects.
 *  Journal of Statistical Computation and Simulation,
 *  Vol.30, pp.1-15, 1988.
 *
 *  Uses the secant method to find critical values.
 *
 *  p = confidence level (1 - alpha)
 *  rr = no. of rows or groups
 *  cc = no. of columns or treatments
 *  df = degrees of freedom of error term
 *
 *  ir(1) = error flag = 1 if wprob probability > 1
 *  ir(2) = error flag = 1 if ptukey probability > 1
 *  ir(3) = error flag = 1 if convergence not reached in 50 iterations
 *		       = 2 if df < 2
 *
 *  qtukey = returned critical value
 *
 *  If the difference between successive iterates is less than eps,
 *  the search is terminated
 */


double qtukey(double p, double rr, double cc, double df,
	      int lower_tail, int log_p)
{
    static const double eps = 0.0001;
    static const int maxiter = 50;

    double ans, valx0, valx1, x0, x1, xabs;
    int iter;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(rr) || ISNAN(cc) || ISNAN(df)) {
	ML_ERROR(ME_DOMAIN);
	return p + rr + cc + df;
    }
#endif

    R_Q_P01_check(p);
    if (p == 1) ML_ERR_return_NAN;

    /* df must be > 1 */
    /* there must be at least two values */

    if (df < 2 || rr < 1 || cc < 2) ML_ERR_return_NAN;

    if (p == R_DT_0) return 0;

    p = R_DT_qIv(p); /* lower_tail,non-log "p" */

    /* Initial value */

    x0 = qinv(p, cc, df);

    /* Find prob(value < x0) */

    valx0 = ptukey(x0, rr, cc, df, /*LOWER*/LTRUE, /*LOG_P*/LFALSE) - p;

    /* Find the second iterate and prob(value < x1). */
    /* If the first iterate has probability value */
    /* exceeding p then second iterate is 1 less than */
    /* first iterate; otherwise it is 1 greater. */

    if (valx0 > 0.0)
	x1 = fmax2(0.0, x0 - 1.0);
    else
	x1 = x0 + 1.0;
    valx1 = ptukey(x1, rr, cc, df, /*LOWER*/LTRUE, /*LOG_P*/LFALSE) - p;

    /* Find new iterate */

    for(iter=1 ; iter < maxiter ; iter++) {
	ans = x1 - ((valx1 * (x1 - x0)) / (valx1 - valx0));
	valx0 = valx1;

	/* New iterate must be >= 0 */

	x0 = x1;
	if (ans < 0.0) {
	    ans = 0.0;
	    valx1 = -p;
	}
	/* Find prob(value < new iterate) */

	valx1 = ptukey(ans, rr, cc, df, /*LOWER*/LTRUE, /*LOG_P*/LFALSE) - p;
	x1 = ans;

	/* If the difference between two successive */
	/* iterates is less than eps, stop */

	xabs = fabs(x1 - x0);
	if (xabs < eps)
	    return ans;
    }

    /* The process did not converge in 'maxiter' iterations */
    ML_ERROR(ME_NOCONV);
    return ans;
}
