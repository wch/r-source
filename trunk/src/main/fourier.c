/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* These are the R interface routines to the plain FFT code 
   fft_factor() & fft_work() in ../appl/fft.c. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <R_ext/Applic.h>

/* Fourier Transform for Univariate Spatial and Time Series */

SEXP attribute_hidden do_fft(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP z, d;
    int i, inv, maxf, maxmaxf, maxmaxp, maxp, n, ndims, nseg, nspn;
    double *work;
    int *iwork;
    char *vmax;

    checkArity(op, args);

    z = CAR(args);

    switch (TYPEOF(z)) {
    case INTSXP:
    case LGLSXP:
    case REALSXP:
	z = coerceVector(z, CPLXSXP);
	break;
    case CPLXSXP:
	if (NAMED(z)) z = duplicate(z);
	break;
    default:
	errorcall(call, _("non-numeric argument"));
    }
    PROTECT(z);

    /* -2 for forward transform, complex values */
    /* +2 for backward transform, complex values */

    inv = asLogical(CADR(args));
    if (inv == NA_INTEGER || inv == 0)
	inv = -2;
    else
	inv = 2;

    if (LENGTH(z) > 1) {
	vmax = vmaxget();
	if (isNull(d = getAttrib(z, R_DimSymbol))) {  /* temporal transform */
	    n = length(z);
	    fft_factor(n, &maxf, &maxp);
	    if (maxf == 0)
		errorcall(call, _("fft factorization error"));
	    work = (double*)R_alloc(4 * maxf, sizeof(double));
	    iwork = (int*)R_alloc(maxp, sizeof(int));
	    fft_work(&(COMPLEX(z)[0].r), &(COMPLEX(z)[0].i),
		     1, n, 1, inv, work, iwork);
	}
	else {					     /* spatial transform */
	    maxmaxf = 1;
	    maxmaxp = 1;
	    ndims = LENGTH(d);
	    /* do whole loop just for error checking and maxmax[fp] .. */
	    for (i = 0; i < ndims; i++) {
		if (INTEGER(d)[i] > 1) {
		    fft_factor(INTEGER(d)[i], &maxf, &maxp);
		    if (maxf == 0)
			errorcall(call, _("fft factorization error"));
		    if (maxf > maxmaxf)
			maxmaxf = maxf;
		    if (maxp > maxmaxp)
			maxmaxp = maxp;
		}
	    }
	    work = (double*)R_alloc(4 * maxmaxf, sizeof(double));
	    iwork = (int*)R_alloc(maxmaxp, sizeof(int));
	    nseg = LENGTH(z);
	    n = 1;
	    nspn = 1;
	    for (i = 0; i < ndims; i++) {
		if (INTEGER(d)[i] > 1) {
		    nspn *= n;
		    n = INTEGER(d)[i];
		    nseg /= n;
		    fft_factor(n, &maxf, &maxp);
		    fft_work(&(COMPLEX(z)[0].r), &(COMPLEX(z)[0].i),
			     nseg, n, nspn, inv, work, iwork);
		}
	    }
	}
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return z;
}

/* Fourier Transform for Vector-Valued ("multivariate") Series */
/* Not to be confused with the spatial case (in do_fft). */

SEXP attribute_hidden do_mvfft(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP z, d;
    int i, inv, maxf, maxp, n, p;
    double *work;
    int *iwork;
    char *vmax;

    checkArity(op, args);

    z = CAR(args);

    d = getAttrib(z, R_DimSymbol);
    if (d == R_NilValue || length(d) > 2)
	errorcall(call, _("vector-valued (multivariate) series required"));
    n = INTEGER(d)[0];
    p = INTEGER(d)[1];

    switch(TYPEOF(z)) {
    case INTSXP:
    case LGLSXP:
    case REALSXP:
	z = coerceVector(z, CPLXSXP);
	break;
    case CPLXSXP:
	if (NAMED(z)) z = duplicate(z);
	break;
    default:
	errorcall(call, _("non-numeric argument"));
    }
    PROTECT(z);

    /* -2 for forward  transform, complex values */
    /* +2 for backward transform, complex values */

    inv = asLogical(CADR(args));
    if (inv == NA_INTEGER || inv == 0) inv = -2;
    else inv = 2;

    if (n > 1) {
	vmax = vmaxget();
	fft_factor(n, &maxf, &maxp);
	if (maxf == 0)
	    errorcall(call, _("fft factorization error"));
	work = (double*)R_alloc(4 * maxf, sizeof(double));
	iwork = (int*)R_alloc(maxp, sizeof(int));
	for (i = 0; i < p; i++) {
	    fft_factor(n, &maxf, &maxp);
	    fft_work(&(COMPLEX(z)[i*n].r), &(COMPLEX(z)[i*n].i),
		     1, n, 1, inv, work, iwork);
	}
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return z;
}

static Rboolean ok_n(int n, int *f, int nf)
{
    int i;
    for (i = 0; i < nf; i++) {
	while(n % f[i] == 0) {
	    if ((n = n / f[i]) == 1)
		return TRUE;
	}
    }
    return n == 1;
}

static int nextn(int n, int *f, int nf)
{
    while(!ok_n(n, f, nf))
	n++;
    return n;
}

SEXP attribute_hidden do_nextn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP n, f, ans;
    int i, nn, nf;
    checkArity(op, args);
    PROTECT(n = coerceVector(CAR(args), INTSXP));
    PROTECT(f = coerceVector(CADR(args), INTSXP));
    nn = LENGTH(n);
    nf = LENGTH(f);

    /* check the factors */

    if (nf == 0) errorcall(call, _("no factors"));
    for (i = 0; i < nf; i++)
	if (INTEGER(f)[i] == NA_INTEGER || INTEGER(f)[i] <= 1)
	    errorcall(call, _("invalid factors"));

    ans = allocVector(INTSXP, nn);
    for (i = 0; i < nn; i++) {
	if (INTEGER(n)[i] == NA_INTEGER)
	    INTEGER(ans)[i] = NA_INTEGER;
	else if (INTEGER(n)[i] <= 1)
	    INTEGER(ans)[i] = 1;
	else
	    INTEGER(ans)[i] = nextn(INTEGER(n)[i], INTEGER(f), nf);
    }
    UNPROTECT(2);
    return ans;
}
