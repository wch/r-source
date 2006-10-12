/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2001   Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h>
#include <R_ext/Error.h>
#include <R_ext/Applic.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

/* Linear and Step Function Interpolation */

/* Assumes that ordinates are in ascending order
 * The right interval is found by bisection
 * Linear/constant interpolation then takes place on that interval
*/

/* NB:  R_interv(.) in ./interv.c  is conceptually a special case of
 *	this, where y = 1:n */

typedef struct {
    double ylow;
    double yhigh;
    double f1;
    double f2;
    int kind;
} appr_meth;

static double approx1(double v, double *x, double *y, int n,
		      appr_meth *Meth)
{
    /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
    int i, j, ij;

    if(!n) return R_NaN;

    i = 0;
    j = n - 1;

    /* handle out-of-domain points */

    if(v < x[i]) return Meth->ylow;
    if(v > x[j]) return Meth->yhigh;

    /* find the correct interval by bisection */

    while(i < j - 1) { /* x[i] <= v <= x[j] */
	ij = (i + j)/2; /* i+1 <= ij <= j-1 */
	if(v < x[ij]) j = ij;
	else i = ij;
	/* still i < j */
    }
    /* provably have i == j-1 */

    /* interpolation */

    if(v == x[j]) return y[j];
    if(v == x[i]) return y[i];
    /* impossible: if(x[j] == x[i]) return y[i]; */

    if(Meth->kind == 1) { /* linear */
	return y[i] + (y[j] - y[i]) * ((v - x[i])/(x[j] - x[i]));
    }
    else { /* 2 : constant */
	return y[i] * Meth->f1 + y[j] * Meth->f2;
    }
}/* approx1() */


	/* R Frontend for Linear and Constant Interpolation */

void R_approx(double *x, double *y, int *nxy, double *xout, int *nout,
	      int *method, double *yleft, double *yright, double *f)
{
    int i;
    appr_meth M = {0.0, 0.0, 0.0, 0.0, 0}; /* -Wall */

    /* check interpolation method */

    switch(*method) {
    case 1: /* linear */
	break;
    case 2: /* constant */
	if(!R_FINITE(*f) || *f < 0 || *f > 1)
	    error(_("approx(): invalid f value"));
	M.f2 = *f;
	M.f1 = 1 - *f;
	break;
    default:
	error(_("approx(): invalid interpolation method"));
	break;
    }

    for(i=0 ; i<*nxy ; i++)
	if(ISNA(x[i]) || ISNA(y[i]))
	    error(_("approx(): attempted to interpolate NA values"));

    M.kind = *method;
    M.ylow = *yleft;
    M.yhigh = *yright;

    for(i=0 ; i < *nout; i++)
	if(!ISNA(xout[i]))
	    xout[i] = approx1(xout[i], x, y, *nxy, &M);
}

