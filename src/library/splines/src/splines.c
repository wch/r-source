/*  Routines for manipulating B-splines.  These are intended for use with
 *  S or S-PLUS or R.
 *
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
 *  http://www.r-project.org/Licenses/
 *
 * The routines are loosely based on the pseudo-code in Schumacher (Wiley,
 * 1981) and the CMLIB library DBSPLINES.
 */

#include <R.h>
#include <Rinternals.h>

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
    int j, r;
    double saved, term;

    diff_table(sp, x, sp->ordm1);
    b[0] = 1.;
    for (j = 1; j <= sp->ordm1; j++) {
	saved = 0.;
	for (r = 0; r < j; r++) {
	    term = b[r]/(sp->rdel[r] + sp->ldel[j - 1 - r]);
	    b[r] = saved + sp->rdel[r] * term;
	    saved = sp->ldel[j - 1 - r] * term;
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
    while(nder--) {
	for(inner = outer, apt = sp->a, lpt = ti - outer; inner--; apt++, lpt++)
	    *apt = outer * (*(apt + 1) - *apt)/(*(lpt + outer) - *lpt);
	outer--;
    }
    diff_table(sp, x, outer);
    while(outer--)
	for(apt = sp->a, lpt = sp->ldel + outer, rpt = sp->rdel, inner = outer + 1;
	    inner--; lpt--, rpt++, apt++)
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
    int der, i, n, nk;

    PROTECT(knots = coerceVector(knots, REALSXP));
    kk = REAL(knots); nk = length(knots);
    PROTECT(coeff = coerceVector(coeff, REALSXP));
    PROTECT(x = coerceVector(x, REALSXP));
    n = length(x);
    xx = REAL(x);
    PROTECT(order = coerceVector(order, INTSXP));
    PROTECT(deriv = coerceVector(deriv, INTSXP));
    der = INTEGER(deriv)[0];
    PROTECT(val = allocVector(REALSXP, n));

    /* populate the spl_struct */

    sp = (struct spl_struct *) R_alloc(1, sizeof(struct spl_struct));
    sp->order = INTEGER(order)[0];
    if (sp->order <= 0) { error(_("'ord' must be a positive integer")); }
    sp->ordm1 = sp->order - 1;
    sp->ldel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->rdel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->knots = kk; sp->nknots = nk;
    sp->coeff = REAL(coeff);
    sp->a = (double *) R_alloc(sp->order, sizeof(double));

    for (i = 0; i < n; i++) {
	set_cursor(sp, xx[i]);
	if (sp->curs < sp->order || sp->curs > (nk - sp->order)) {
	    REAL(val)[i] = R_NaN;
	} else {
	    Memcpy(sp->a, REAL(coeff) + sp->curs - sp->order, sp->order);
	    REAL(val)[i] = evaluate(sp, xx[i], der);
	}
    }
    UNPROTECT(6);
    return val;
}

/* called from	splineDesign() : */
SEXP
spline_basis(SEXP knots, SEXP order, SEXP xvals, SEXP derivs)
{
/* evaluate the non-zero B-spline basis functions (or their derivatives)
 * at xvals.  */
    int nd, nk, nx, i, j, *ders;
    double *kk, *xx;
    SEXP val, offsets;
    splPTR sp = (struct spl_struct *) R_alloc(1, sizeof(struct spl_struct));

    PROTECT(knots = coerceVector(knots, REALSXP));
    kk = REAL(knots); nk = length(knots);
    PROTECT(order = coerceVector(order, INTSXP));
    PROTECT(xvals = coerceVector(xvals, REALSXP));
    xx = REAL(xvals); nx = length(xvals);
    PROTECT(derivs = coerceVector(derivs, INTSXP));
    ders = INTEGER(derivs); nd = length(derivs);

    /* fill sp : */
    sp->order = INTEGER(order)[0];
    sp->ordm1 = sp->order - 1;
    sp->rdel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->ldel = (double *) R_alloc(sp->ordm1, sizeof(double));
    sp->knots = kk; sp->nknots = nk;
    sp->a = (double *) R_alloc(sp->order, sizeof(double));
    PROTECT(val = allocMatrix(REALSXP, sp->order, nx));
    PROTECT(offsets = allocVector(INTSXP, nx));

    for(i = 0; i < nx; i++) {
	set_cursor(sp, xx[i]);
	INTEGER(offsets)[i] = j = sp->curs - sp->order;
	if (j < 0 || j > nk) {
	    for (j = 0; j < sp->order; j++) {
		REAL(val)[i * sp->order + j] = R_NaN;
	    }
	} else {
	    if (ders[i % nd] > 0) { /* slow method for derivatives */
		int ii;
		for(ii = 0; ii < sp->order; ii++) {
		    for(j = 0; j < sp->order; j++) sp->a[j] = 0;
		    sp->a[ii] = 1;
		    REAL(val)[i * sp->order + ii] =
			evaluate(sp, xx[i], ders[i % nd]);
		}
	    } else {		/* fast method for value */
		basis_funcs(sp, xx[i], REAL(val) + i * sp->order);
	    }
	}
    }
    setAttrib(val, install("Offsets"), offsets);
    UNPROTECT(6);
    return val;
}

#include <R_ext/Rdynload.h>

const static R_CallMethodDef R_CallDef[] = {
   {"spline_basis", (DL_FUNC)&spline_basis, 4},
   {"spline_value", (DL_FUNC)&spline_value, 5},
   {NULL, NULL, 0}
};


void
R_init_splines(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
//    R_forceSymbols(dll, TRUE); // too few to worry about, used in cobs*
}
