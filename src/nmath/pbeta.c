/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2005 The R Development Core Team
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
 * #include <Rmath.h>
 *
 * double pbeta_raw(double x, double pin, double qin, int lower_tail)
 * double pbeta	   (double x, double pin, double qin, int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	Returns distribution function of the beta distribution.
 *	( = The incomplete beta ratio I_x(p,q) ).
 *
 *  NOTES
 *
 *	This routine is a translation into C of a Fortran subroutine
 *	by W. Fullerton of Los Alamos Scientific Laboratory.
 *
 *  REFERENCE
 *
 *	Bosten and Battiste (1974).
 *	Remark on Algorithm 179, CACM 17, p153, (1974).
 */

#include "nmath.h"
#include "dpq.h"

/* This is called from	qbeta(.) in a root-finding loop --- be FAST! */

double pbeta_raw(double x, double pin, double qin, int lower_tail)
{
    double ans, c, finsum, p, ps, p1, q, term, xb, xi, y;
    int n, i, ib, swap_tail;

    const double eps = .5*DBL_EPSILON;
    const double sml = DBL_MIN;
    const double lneps = log(eps);
    const double lnsml = log(sml);

    /* Switch to TOMS 708 if p or q is large */
    if (pin > 15 || qin > 15) {
	double x1 = 1 - x, w, wc;
	int ierr;
	bratio(pin, qin, x, x1, &w, &wc, &ierr);
	return lower_tail ? w : wc;
    }

    /* swap tails if x is greater than the mean */
    if (pin / (pin + qin) < x) {
	swap_tail = 1;
	y = 1 - x;
	p = qin;
	q = pin;
    }
    else {
	swap_tail = 0;
	y = x;
	p = pin;
	q = qin;
    }

    if ((p + q) * y / (p + 1) < eps) {

	/* tail approximation */

	xb = p * log(fmax2(y, sml)) - log(p) - lbeta(p, q);
	if (xb > lnsml && y != 0) {
	    ans = (swap_tail == lower_tail) ? -expm1(xb) : exp(xb);
	} else {
	    ans = (swap_tail == lower_tail) ? 1. : 0;
	}
    } else {
	/* evaluate the infinite sum first.  term will equal */
	/* y^p / beta(ps, p) * (1 - ps)-sub-i * y^i / fac(i) */

	/* Ly := log(y) */
	double Ly = swap_tail ? log1p(-x) : log(y);

	ps = q - floor(q);
	xb = p * Ly;
	if (ps == 0)
	    ps = 1; /*==> lbeta(ps,p)= log Beta(1,p) = log(1/p) = -log(p) */
	else
	    xb -= (lbeta(ps, p) + log(p));
	ans = 0;
	if (xb >= lnsml) {
	    ans = exp(xb);
	    term = ans * p;
	    if (ps != 1) {
		n = fmax2(lneps/Ly, 4.0);
		for(i=1 ; i <= n ; i++) {
		    xi = i;
		    term *= (xi - ps) * y / xi;
		    ans += term / (p + xi);
		}
	    }
	}

	/* now evaluate the finite sum, maybe. */

	if (q > 1) {

	    double liy;/* == log(1-y) */
	    if(swap_tail) {
		c = 1./x;/* == 1/(1 - y) */
		liy = log(x);
	    }
	    else {
		c = 1./(1. - y);
		liy = log1p(-y);
	    }
	    xb = p * Ly + q * liy - lbeta(p, q) - log(q);
	    ib = fmax2(xb / lnsml, 0.0);
	    term = exp(xb - ib * lnsml);
	    p1 = q * c / (p + q - 1);

	    finsum = 0;
	    n = q;
	    if (q == n)
		n--;
	    for(i= 1; i <= n; i++) {
		if (p1 <= 1 && term / eps <= finsum)
		    break;
		xi = i;
		term = (q - xi + 1) * c * term / (p + q - xi);
		if (term > 1) {
		    ib--;
		    term *= sml;
		}
		if (ib == 0)
		    finsum += term;
	    }
	    ans += finsum;
	}
	if (swap_tail == lower_tail)
	    ans = 1 - ans;
	ans = fmax2(fmin2(ans, 1.), 0.);
    }
    return ans;
} /* pbeta_raw() */

double pbeta(double x, double pin, double qin, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(pin) || ISNAN(qin))
	return x + pin + qin;
#endif

    if (pin <= 0 || qin <= 0) ML_ERR_return_NAN;

    if (x <= 0)
	return R_DT_0;
    if (x >= 1)
	return R_DT_1;
    return R_D_val(pbeta_raw(x, pin, qin, lower_tail));
}
