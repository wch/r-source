/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"

static SEXP cumsum(SEXP x, SEXP s)
{
    int i;
    double sum;
    sum = 0.0;
    for (i = 0 ; i < length(x) ; i++) {
#ifndef IEEE_754
	if (ISNAN(REAL(x)[i]))
	    break;
#endif
	sum += REAL(x)[i];
	REAL(s)[i] = sum;
    }
    return s;
}

static SEXP ccumsum(SEXP x, SEXP s)
{
    int i;
    Rcomplex sum;
    sum.r = 0;
    sum.i = 0;
    for (i = 0 ; i < length(x) ; i++) {
#ifndef IEEE_754
	if (ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i))
	    break;
#endif
	sum.r += COMPLEX(x)[i].r;
	sum.i += COMPLEX(x)[i].i;
	COMPLEX(s)[i].r = sum.r;
	COMPLEX(s)[i].i = sum.i;
    }
    return s;
}

static SEXP cumprod(SEXP x, SEXP s)
{
    int i;
    double prod;
    prod = 1.0;
    for (i = 0 ; i < length(x) ; i++) {
#ifndef IEEE_754
	if (ISNAN(REAL(x)[i]))
	    break;
#endif
	prod *= REAL(x)[i];
	REAL(s)[i] = prod;
    }
    return s;
}

static SEXP ccumprod(SEXP x, SEXP s)
{
    Rcomplex prod, tmp;
    int i;
    prod.r = 1;
    prod.i = 0;
    for (i = 0 ; i < length(x) ; i++) {
#ifndef IEEE_754
	if (ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i))
	    break;
#endif
	tmp.r = prod.r;
	tmp.i = prod.i;
	prod.r = COMPLEX(x)[i].r * tmp.r - COMPLEX(x)[i].i * tmp.i;
	prod.i = COMPLEX(x)[i].r * tmp.i + COMPLEX(x)[i].i * tmp.r;
	COMPLEX(s)[i].r = prod.r;
	COMPLEX(s)[i].i = prod.i;
    }
    return s;
}

static SEXP cummax(SEXP x, SEXP s)
{
    int i;
    double max;
#ifdef IEEE_754
    max = R_NegInf;
#else
    max = NA_REAL;
#endif
    for (i = 0 ; i < length(x) ; i++) {
	if(ISNAN(REAL(x)[i]) || ISNAN(max))
#ifdef IEEE_754
	    max = max + REAL(x)[i];  /* propagate NA and NaN */
#else
	break;
#endif
	else
	    max = (max > REAL(x)[i]) ? max : REAL(x)[i];
	REAL(s)[i] = max;
    }
    return s;
}

static SEXP cummin(SEXP x, SEXP s)
{
    int i;
    double min;
#ifdef IEEE_754
    min = R_PosInf;
#else
    min = NA_REAL;
#endif
    for (i = 0 ; i < length(x) ; i++ ) {
	if (ISNAN(REAL(x)[i]) || ISNAN(min))
#ifdef IEEE_754
	    min = min + REAL(x)[i];  /* propagate NA and NaN */
#else
	break;
#endif
	else
	    min = (min < REAL(x)[i]) ? min : REAL(x)[i];
	REAL(s)[i] = min;
    }
    return s;
}

SEXP do_cum(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, ans;
    int i;
    checkArity(op, args);
    if (DispatchGroup("Math", call, op, args, env, &ans))
	return ans;
    if (isComplex(CAR(args))) {
	t = CAR(args);
	s = allocVector(CPLXSXP, LENGTH(t));
	for (i = 0 ; i < length(t) ; i++) {
	    COMPLEX(s)[i].r = NA_REAL;
	    COMPLEX(s)[i].i = NA_REAL;
	}
	switch (PRIMVAL(op) ) {
	case 1:	/* cumsum */
	    return ccumsum(t, s);
	    break;
	case 2: /* cumprod */
	    return ccumprod(t, s);
	    break;
	case 3: /* cummax */
	case 4: /* cummin */
	    errorcall(call, "min/max not defined for complex numbers");
	    break;
	default:
	    errorcall(call,"unknown cumxxx function");
	}
    }
    else { /* Non-Complex:  here, (sh|c)ould differentiate  real / int */
	PROTECT(t = coerceVector(CAR(args), REALSXP));
	s = allocVector(REALSXP, LENGTH(t));
	for(i = 0 ; i < length(t) ; i++)
	    REAL(s)[i] = NA_REAL;
	UNPROTECT(1);
	switch (PRIMVAL(op) ) {
	case 1:	/* cumsum */
	    return cumsum(t,s);
	    break;
	case 2: /* cumprod */
	    return cumprod(t,s);
	    break;
	case 3: /* cummax */
	    return cummax(t,s);
	    break;
	case 4: /* cummin */
	    return cummin(t,s);
	    break;
	default:
	    errorcall(call,"Unknown cum function");
	}
    }
    return R_NilValue; /* for -Wall */
}
