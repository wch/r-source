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
 *    double pbeta(double x, double pin, double qin);
 *
 *  DESCRIPTION
 *
 *    Returns distribution function of the beta distribution.
 *    (The incomplete beta ratio).
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 *
 *  REFERENCE
 *
 *    Bosten and Battiste (1974).
 *    Remark on Algorithm 179,
 *    CACM 17, p153, (1974).
 */

#include "Mathlib.h"

double pbeta_raw(double x, double pin, double qin)
{
    double ans, c, finsum, p, ps, p1, q, term, xb, xi, y;
    int n, i, ib;

    static double eps = 0;
    static double alneps = 0;
    static double sml = 0;
    static double alnsml = 0;

    if (eps == 0) {
        eps = d1mach(3);
        alneps = log(eps);
        sml = d1mach(1);
        alnsml = log(sml);
    }

    y = x;
    p = pin;
    q = qin;

    /* swap tails if x is greater than the mean */

    if (p / (p + q) < x) {
        y = 1 - y;
        p = qin;
        q = pin;
    }

    if ((p + q) * y / (p + 1) < eps) {

	/* tail approximation */

        ans = 0;
        xb = p * log(fmax2(y, sml)) - log(p) - lbeta(p, q);
        if (xb > alnsml && y != 0)
            ans = exp(xb);
        if (y != x || p != pin)
            ans = 1 - ans;
    }
    else {

        /* evaluate the infinite sum first.  term will equal */
        /* y^p / beta(ps, p) * (1 - ps)-sub-i * y^i / fac(i) */

        ps = q - floor(q);
        if (ps == 0)
            ps = 1;
        xb = p * log(y) - lbeta(ps, p) - log(p);
        ans = 0;
        if (xb >= alnsml) {
            ans = exp(xb);
            term = ans * p;
            if (ps != 1) {
                n = fmax2(alneps/log(y), 4.0);
		for(i=1 ; i<= n ; i++) {
                    xi = i;
                    term = term * (xi - ps) * y / xi;
                    ans = ans + term / (p + xi);
                }
            }
        }

        /* now evaluate the finite sum, maybe. */

        if (q > 1) {
            xb = p * log(y) + q * log(1 - y) - lbeta(p, q) - log(q);
            ib = fmax2(xb / alnsml, 0.0);
            term = exp(xb - ib * alnsml);
            c = 1 / (1 - y);
            p1 = q * c / (p + q - 1);

            finsum = 0;
            n = q;
            if (q == n)
                n = n - 1;
	    for(i=1 ; i<=n ; i++) {
                if (p1 <= 1 && term / eps <= finsum)
                    break;
                xi = i;
                term = (q - xi + 1) * c * term / (p + q - xi);
                if (term > 1) {
                    ib = ib - 1;
                    term = term * sml;
		}
                if (ib == 0)
                    finsum = finsum + term;
            }
            ans = ans + finsum;
        }
        if (y != x || p != pin)
            ans = 1 - ans;
        ans = fmax2(fmin2(ans, 1.0), 0.0);
    }
    return ans;
}

double pbeta(double x, double pin, double qin)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(pin) || ISNAN(qin))
	return x + pin + qin;
#endif

    if (pin <= 0 || qin <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x <= 0)
	return 0;
    if (x >= 1)
	return 1;
    return pbeta_raw(x, pin, qin);
}
