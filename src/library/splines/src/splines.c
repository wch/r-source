/*  Routines for manipulating B-splines.  These are intended for use with
 *  S or S-PLUS or R.
 *
 *     Copyright (C) 1999-2017 The R Core Team.
 *     Copyright (C) 1998 Douglas M. Bates and William N. Venables.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * These functions are distributed in the hope that they will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 * The routines are loosely based on the pseudo-code in Schumacher (Wiley,
 * 1981) and the CMLIB library DBSPLINES.
 */

#include <R.h>
#include <Rinternals.h>
#include <string.h> // for memcpy

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("splines", String)
#else
#define _(String) (String)
#endif

typedef struct spl_struct {
    int order,			/* order of the spline */
	ordm1,			/* order - 1 (3 for cubic splines) */
	nknots,			/* number of knots */
	curs,			/* current position in knots vector */
	boundary;		/* must have knots[curs] <= x < knots[curs+1] */
				/* except for the boundary case */

    double *ldel,		/* differences from knots on the left */
	*rdel,			/* differences from knots on the right */
	*knots,			/* knot vector */
	*coeff,			/* coefficients */
	*a;			/* scratch array */
} *splPTR;

/* Exports */
SEXP spline_basis(SEXP knots, SEXP order, SEXP xvals, SEXP derivs);
SEXP spline_value(SEXP knots, SEXP coeff, SEXP order, SEXP x, SEXP deriv);


/* set sp->curs to the index of the first knot position > x.
   Special handling for x == sp->knots[sp->nknots - sp-order + 1] */
static int
set_cursor(splPTR sp, double x)
{
    int i;
    /* don't assume x's are sorted */

    sp->curs = -1; /* Wall */
    sp->boundary = 0;
    for (i = 0; i < sp->nknots; i++) {
	if (sp->knots[i] >= x) sp->curs = i;
	if (sp->knots[i] > x) break;
    }
    if (sp->curs > sp->nknots - sp->order) {
	int lastLegit = sp->nknots - sp->order;
	if (x == sp->knots[lastLegit]) {
	    sp->boundary = 1; sp->curs = lastLegit;
	}
    }
    return sp->curs;
}

static void
diff_table(splPTR sp, double x, int ndiff)
{
  int i;
  for (i = 0; i < ndiff; i++) {
      sp->rdel[i] = sp->knots[sp->curs + i] - x;
      sp->ldel[i] = x - sp->knots[sp->curs - (i + 1)];
  }
}

/* fast evaluation of basis functions */
static void
basis_funcs(splPTR sp, double x, double *b)
{
    diff_table(sp, x, sp->ordm1);
    b[0] = 1.;
    for (int j = 1; j <= sp->ordm1; j++) {
	double saved = 0.;
	for (int r = 0; r < j; r++) { // do not divide by zero
	    double den = sp->rdel[r] + sp->ldel[j - 1 - r];
	    if(den != 0) {
		double term = b[r]/den;
		b[r] = saved + sp->rdel[r] * term;
		saved = sp->ldel[j - 1 - r] * term;
	    } else {
		if(r != 0 || sp->rdel[r] != 0.)
		    b[r] = saved;
		saved = 0.;
	    }
	}
	b[j] = saved;
    }
}

/* "slow" evaluation of (derivative of) basis functions */
static double
evaluate(splPTR sp, double x, int nder)
{
    register double *lpt, *rpt, *apt, *ti = sp->knots + sp->curs;
    int inner, outer = sp->ordm1;

    if (sp->boundary && nder == sp->ordm1) { /* value is arbitrary */
	return 0.0;
    }
    while(nder--) {  // FIXME: divides by zero
	for(inner = outer, apt = sp->a, lpt = ti - outer; inner--; apt++, lpt++)
	    *apt = outer * (*(apt + 1) - *apt)/(*(lpt + outer) - *lpt);
	outer--;
    }
    diff_table(sp, x, outer);
    while(outer--)
	for(apt = sp->a, lpt = sp->ldel + outer, rpt = sp->rdel, inner = outer + 1;
	    inner--; lpt--, rpt++, apt++)
	    // FIXME: divides by zero
	    *apt = (*(apt + 1) * *lpt + *apt * *rpt)/(*rpt + *lpt);
    return sp->a[0];
}

/* called from	predict.bSpline() and predict.pbSpline() : */
SEXP
spline_value(SEXP knots, SEXP coeff, SEXP order, SEXP x, SEXP deriv)
{
    SEXP val;
    splPTR sp;
    double *xx, *kk;
    int n, nk;

    PROTECT(knots = coerceVector(knots, REALSXP));
    kk = REAL(knots); nk = length(knots);
    PROTECT(coeff = coerceVector(coeff, REALSXP));
    PROTECT(x = coerceVector(x, REALSXP));
    xx = REAL(x); n = length(x);
    int ord = asInteger(order);
    int der = asInteger(deriv);
    if (ord == NA_INTEGER || ord <= 0)
	error(_("'ord' must be a positive integer"));

    /* populate the spl_struct */
    sp = (struct spl_struct *) R_alloc(1, sizeof(struct spl_struct));
    sp->order = ord;
    sp->ordm1 = ord - 1;
    sp->ldel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->rdel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->knots = kk; sp->nknots = nk;
    sp->coeff = REAL(coeff);
    sp->a = (double *) R_alloc(sp->order, sizeof(double));

    PROTECT(val = allocVector(REALSXP, n));
    double *rval = REAL(val);

    for (int i = 0; i < n; i++) {
	set_cursor(sp, xx[i]);
	if (sp->curs < sp->order || sp->curs > (nk - sp->order)) {
	    rval[i] = R_NaN;
	} else {
	    Memcpy(sp->a, sp->coeff + sp->curs - sp->order, sp->order);
	    rval[i] = evaluate(sp, xx[i], der);
	}
    }
    UNPROTECT(4);
    return val;
}

/* called from	splineDesign() : */
SEXP
spline_basis(SEXP knots, SEXP order, SEXP xvals, SEXP derivs)
{
/* evaluate the non-zero B-spline basis functions (or their derivatives)
 * at xvals.  */

    PROTECT(knots = coerceVector(knots, REALSXP));
    double *kk = REAL(knots); int nk = length(knots);
    int ord = asInteger(order);
    PROTECT(xvals = coerceVector(xvals, REALSXP));
    double *xx = REAL(xvals); int nx = length(xvals);
    PROTECT(derivs = coerceVector(derivs, INTSXP));
    int *ders = INTEGER(derivs), nd = length(derivs);

    splPTR sp = (struct spl_struct *) R_alloc(1, sizeof(struct spl_struct));
    /* fill sp : */
    sp->order = ord;
    sp->ordm1 = ord - 1;
    sp->rdel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->ldel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->knots = kk; sp->nknots = nk;
    sp->a = (double *) R_alloc(ord, sizeof(double));
    SEXP val = PROTECT(allocMatrix(REALSXP, ord, nx)),
	offsets = PROTECT(allocVector(INTSXP, nx));
    double *valM = REAL(val);
    int *ioff = INTEGER(offsets);

    for(int i = 0; i < nx; i++) {
	set_cursor(sp, xx[i]);
	// ==> io  \in {0,..,nk} is the knot-interval "number"
	int io = ioff[i] = sp->curs - ord,
	    der_i = ders[i % nd];
	if (io < 0 || io > nk) {
	    for (int j = 0; j < ord; j++) {
		valM[i * ord + j] = R_NaN;
	    }
	} else if (der_i > 0) { /* slow method for derivatives */
	    if (der_i >= ord) {
		if(nd == 1) {
		    error(_("derivs = %d >= ord = %d, but should be in {0,..,ord-1}"),
			  der_i, ord);
		} else {
		    error(_("derivs[%d] = %d >= ord = %d, but should be in {0,..,ord-1}"),
			  i+1, der_i, ord);
		}
	    }
	    for(int ii = 0; ii < ord; ii++) {
		for(int j = 0; j < ord; j++) sp->a[j] = 0;
		sp->a[ii] = 1;
		valM[i * ord + ii] =
		    evaluate(sp, xx[i], der_i);
	    }
	} else {		/* fast method for value */
	    basis_funcs(sp, xx[i], valM + i * ord);
	}
    }
    setAttrib(val, install("Offsets"), offsets);
    UNPROTECT(5);
    return val;
}

#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
    CALLDEF(spline_basis, 4),
    CALLDEF(spline_value, 5),
    {NULL, NULL, 0}
};


void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_splines(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
