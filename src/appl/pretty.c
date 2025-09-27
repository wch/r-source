/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2025  The R Core Team
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
 */

/* Pretty Intervals
 * ----------------
 * Constructs m "pretty" values which cover the given interval	*lo <= *up
 *	m ~= *ndiv + 1	(i.e., ndiv := approximate number of INTERVALS)
 *
 * It is not quite clear what should happen for	 *lo = *up;
 * S itself behaves quite funilly, then.
 *
 * In my opinion, a proper 'pretty' should always ensure
 * *lo < *up, and hence *ndiv >=1 in the result.
 * However, in S and here, we allow  *lo == *up, and *ndiv = 0.
 * Note however, that we are NOT COMPATIBLE to S. [Martin M.]
 *
 * NEW (0.63.2): ns, nu are double (==> no danger of integer overflow)
 *
 * We determine
 * if the interval (up - lo) is ``small'' [<==>	 i_small == TRUE, below].
 * For the ``i_small'' situation, there is a parameter  shrink_sml,
 * the factor by which the "scale" is shrunk.		~~~~~~~~~~
 * It is advisable to set it to some (smaller) integer power of 2,
 * since this enables exact floating point division.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

#include <math.h>
#include <float.h> /* DBL_MAX */
#include <R_ext/Arith.h>	/* NA handling */
#include <Rmath.h>
#include <R_ext/Error.h>

#include <R_ext/Applic.h>

#ifdef DEBUGpr
# include <R_ext/Print.h>
#endif

#include <R_ext/Visibility.h>
/*
#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif
*/

attribute_hidden
double R_pretty(double *lo, double *up, int *ndiv, int min_n,
		double shrink_sml,
		const double high_u_fact[], // = (h, h5, f_min) below
		int eps_correction, int return_bounds)
{
/* From version 0.65 on, we had rounding_eps := 1e-5, before, r..eps = 0
 * then, 1e-7 was consistent with seq.default() and seq.int() till 2010-02-03,
 * where it was changed to 1e-10 for seq*(), and in 2017-08-14 for pretty(): */
#define rounding_eps 1e-10

// (h, h5, f_min) = c(high.u.bias, u5.bias, f.min) in base::pretty.default():
#define h     high_u_fact[0]
#define h5    high_u_fact[1]
#define f_min high_u_fact[2]

    double // save input boundaries
	lo_ = *lo,
	up_ = *up,
	dx = up_ - lo_,
	cell, U;
    bool i_small;
    /* cell := "scale"	here */
    if(dx == 0 && up_ == 0) { /*  up == lo == 0	 */
	cell = 1;
	i_small = true;
    } else {
	cell = fmax2(fabs(lo_),fabs(up_));
	/* U = upper bound on cell/unit */
	U = 1 + ((h5 >= 1.5*h+.5) ? 1/(1+h) : 1.5/(1+h5));
	U *= imax2(1,*ndiv) * DBL_EPSILON; // avoid overflow for large ndiv
	/* added times 3, as several calculations here */
	i_small = dx < cell * U * 3;
    }

#ifdef DEBUGpr
    REprintf("R_pretty(lo=%g,up=%g,ndiv=%d,min_n=%d,shrink=%g,high_u=(%g,%g,%g),eps=%d,bnds=%d)"
	     "\n\t => dx=%g; i_small:%s. ==> first cell=%g\n",
	     lo_, up_, *ndiv, min_n, shrink_sml, h, h5, f_min,
	     eps_correction, return_bounds,
	     dx, i_small ? "TRUE" : "F", cell);
#endif

    /*OLD: cell = FLT_EPSILON+ dx / *ndiv; FLT_EPSILON = 1.192e-07 */
    if(i_small) {
	if(cell > 10)
	    cell = 9 + cell/10;
	cell *= shrink_sml;
	if(min_n > 1) cell /= min_n;
    } else {
	cell = dx;
	if(R_FINITE(dx)) {
	    if(*ndiv > 1) cell /= *ndiv;
	} else { // up - lo = +Inf (overflow; both are finite)
	    if(*ndiv < 2) {
		warning(_("R_pretty(): infinite range; *ndiv=%d, should have ndiv >= 2"),
			*ndiv);
	    } else {
		cell = up_/(*ndiv) - lo_/(*ndiv);
	    }
	}
    }

// f_min: arg, default = 2^-20, was 20.  till R 4.1.0 (2021-05)
#define MAX_F 1.25 //		was 10.   "   "   "

    double subsmall = f_min*DBL_MIN;
    if(subsmall == 0.) // subnormals underflowing to zero (not yet seen!)
	subsmall = DBL_MIN;
    if(cell < subsmall) { // possibly subnormal
	if(cell > 0)
	  warning(_("R_pretty(): very small range 'cell'=%.3g, increased to %g"),
		cell, subsmall);
	cell = subsmall;
    } else if(cell > DBL_MAX/MAX_F) {
	warning(_("R_pretty(): very large range 'cell'=%.3g, decreased to %g"),
		cell, DBL_MAX/MAX_F);
	cell = DBL_MAX/MAX_F;
    }

#undef MAX_F

    /* NB: the power can be negative and this relies on exact
       calculation, which glibc's exp10 does not achieve */
    double base = pow(10.0, floor(log10(cell))); /* base <= cell < 10*base */

    /* unit : from { 1,2,5,10 } * base
     *	 such that |u - cell| is small,
     * favoring larger (if h > 1, else smaller)  u  values;
     * favor '5' more than '2'  if h5 > h  (default h5 = .5 + 1.5 h) */
    double unit = base;
    if((U = 2*base)-cell <  h*(cell-unit)) { unit = U;
    if((U = 5*base)-cell < h5*(cell-unit)) { unit = U;
    if((U =10*base)-cell <  h*(cell-unit))   unit = U; }}
    /* Result (c := cell,  b := base,  u := unit):
     *	c in [	1,	       (2+ h)/ (1+h) ] b ==> u=  b
     *	c in ( (2+ h) /(1+h),  (5+2h5)/(1+h5)] b ==> u= 2b
     *	c in ( (5+2h5)/(1+h5), (10+5h)/(1+h) ] b ==> u= 5b
     *	c in ((10+5h) /(1+h),	     10      ) b ==> u=10b
     *
     *	===>	2/5 *(2+h)/(1+h)  <=  c/u  <=  (2+h)/(1+h)	*/

    double
	ns = floor(lo_/unit+rounding_eps),
	nu = ceil (up_/unit-rounding_eps);
#ifdef DEBUGpr
    REprintf("\t => final cell=%g; base=%g unit=%g; (ns,nu) = (%g,%g)\n",
	     cell, base, unit, ns, nu);
#endif
    if(eps_correction && (eps_correction > 1 || !i_small)) {
#define E_ DBL_EPSILON
	const double D_max = DBL_MAX*(1. - ldexp(E_, -1));
	/* move *lo  to the left, assuming <subnorm>*(1-E_) does not underflow to 0 : */
	if(lo_ < 0.) *lo *= (1+E_); else if(lo_ > 0) *lo *= (1-E_);  else *lo = -fmin2(unit, DBL_MIN);
	/* and  *up  to the right : */
	if(up_ < 0.) *up *= (1-E_); else if(up_ > 0.) {
				     if(up_ < D_max) *up *= (1+E_);} else *up = +fmin2(unit, DBL_MIN);
#undef E_
#ifdef DEBUGpr
	REprintf("  eps_correction (assuming lo=%g <= %g=up): new *lo=%g, *up=%g\n",
		 lo_, up_, *lo, *up);
#endif
    }

#ifdef DEBUGpr
    if(ns*unit > *lo + rounding_eps*unit)
	REprintf("\t ns= %.0f -- while(ns*unit > lo + r_eps * u) ns--;\n", ns);
#endif
    while(ns*unit > *lo + rounding_eps*unit) ns--;

#ifdef DEBUGpr
    if(!R_FINITE(ns*unit))
	REprintf("\t while(!finite((ns=%.0f)*(unit=%g))) ns++\n", ns, unit);
#endif
    while(!R_FINITE(ns*unit)) ns++;


#ifdef DEBUGpr
    if(nu*unit < *up - rounding_eps*unit)
	REprintf("\t nu= %.0f -- while(nu*unit < up - r_eps * u) nu++;\n", nu);
#endif
    while(nu*unit < *up - rounding_eps*unit) nu++;

#ifdef DEBUGpr
    if(!R_FINITE(nu*unit))
	REprintf("\t while(!finite((nu=%.0f)*(unit=%g)) nu--\n", nu, unit);
#endif
    while(!R_FINITE(nu*unit)) nu--;

    int k = (int)(0.5 + nu - ns);
#ifdef DEBUGpr
    REprintf(" possibly adjusted  (ns,nu) = (%g,%g) ==> k=%d\n", ns, nu, k);
#endif
    if(k < min_n) {
	/* ensure that	nu - ns	 == min_n */
#ifdef DEBUGpr
	REprintf("\tnu-ns=%.0f-%.0f=%d SMALL -> ensure nu-ns= min_n=%d\n",
		 nu,ns, k, min_n);
#endif
	k = min_n - k;
	if(lo_ == 0. && ns == 0. && up_ != 0.) {
	    nu += k;
	} else if(up_ == 0. && nu == 0. && lo_ != 0.) {
	    ns -= k;
	} else if(ns >= 0.) {
	    nu += k/2;
	    ns -= k/2 + k%2;/* ==> nu-ns = old(nu-ns) + min_n -k = min_n */
	} else {
	    ns -= k/2;
	    nu += k/2 + k%2;
	}
	*ndiv = min_n;
    }
    else {
	*ndiv = k;
    }
    if(return_bounds) {// used in pretty.default(), ensure result covers original range
#ifdef DEBUGpr
	if(ns * unit < *lo) {REprintf("lo=%g too large, set to ns*unit\n", *lo); *lo = ns * unit;}
	if(nu * unit > *up) {REprintf("up=%g too small, set to nu*unit\n", *up); *up = nu * unit;}
#else
	if(ns * unit < *lo) *lo = ns * unit;
	if(nu * unit > *up) *up = nu * unit;
#endif
    } else { // used in graphics GEPretty(), hence grid::grid.pretty()
	*lo = ns;
	*up = nu;
    }
#ifdef DEBUGpr
    REprintf("\t ns=%5.0g ==> lo=%g\n", ns, *lo);
    REprintf("\t nu=%5.0g ==> up=%g  ==> ndiv = %d\n", nu, *up, *ndiv);
#endif
    return unit;
#undef h
#undef h5
}
