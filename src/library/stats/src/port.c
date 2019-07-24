/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-2015   The R Core Team.
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

#include "port.h"

#include <R_ext/Constants.h>
#include <R_ext/BLAS.h>
#include <R_ext/Print.h>

				/* names of 1-based indices into iv and v */
#define AFCTOL  31
#define ALGSAV  51
#define COVPRT  14
#define COVREQ  15
#define DRADPR 101
#define DTYPE   16
#define F       10
#define F0      13
#define FDIF    11
#define G       28
#define HC      71
#define IERR    75
#define INITH   25
#define INITS   25
#define IPIVOT  76
#define IVNEED   3
#define LASTIV  44
#define LASTV   45
#define LMAT    42
#define MXFCAL  17
#define MXITER  18
#define NEXTV   47
#define NFCALL   6
#define NFCOV   52
#define NFGCAL   7
#define NGCOV   53
#define NITER   31
#define NVDFLT  50
#define NVSAVE   9
#define OUTLEV  19
#define PARPRT  20
#define PARSAV  49
#define PERM    58
#define PRUNIT  21
#define QRTYP   80
#define RDREQ   57
#define RMAT    78
#define SOLPRT  22
#define STATPR  23
#define TOOBIG   2
#define VNEED    4
#define VSAVE   60
#define X0PRT   24


/* C-language replacements for Fortran utilities in PORT sources */

/* dd7tpr... returns inner product of two vectors. */
double F77_NAME(dd7tpr)(int *p, const double x[], const double y[])
{
    int ione = 1;
    return F77_CALL(ddot)(p, x, &ione, y, &ione);
}

/* ditsum... prints iteration summary, initial and final alf. */
void F77_NAME(ditsum)(const double d[], const double g[],
		      int iv[], const int *liv, const int *lv,
		      const int *n, double v[], const double x[])
{
    int i, nn = *n;
    int *ivm = iv - 1; double *vm = v - 1; /* offsets for 1-based indices */
    if (!ivm[OUTLEV]) return;	/* no iteration output */
    if (!(ivm[NITER] % ivm[OUTLEV])) { /* output every ivm[OUTLEV] iterations */
	Rprintf("%3d:%#14.8g:", ivm[NITER], vm[F]);
	for (i = 0; i < nn; i++) Rprintf(" %#8g", x[i]);
	Rprintf("\n");
    }
}

				/* port sources */
/* dv7dfl.... provides default values to v. */
extern void F77_NAME(dv7dfl)(const int *Alg, const int *Lv, double v[]);

/**
 * Supply default values for elements of the iv and v arrays
 *
 * @param alg algorithm specification (1 <= alg <= 2)  (was alg <= 4, but reduced to work around gcc bug; see PR#15914)
 * @param iv integer working vector
 * @param liv length of iv
 * @param lv length of v
 * @param v double precision working vector
 */
void Rf_divset(int alg, int iv[], int liv, int lv, double v[])
{
/*  ***  ALG = 1 MEANS REGRESSION CONSTANTS. */
/*  ***  ALG = 2 MEANS GENERAL UNCONSTRAINED OPTIMIZATION CONSTANTS. */


    /* Initialized data */

    // alg[orithm] :          1   2   3    4
    static int miniv[] = {0, 82, 59, 103, 103};
    static int minv [] = {0, 98, 71, 101, 85};

    int mv, miv, alg1;

    /* Parameter adjustments - code will use 1-based indices*/
    --iv;
    --v;

    /* Function Body */


    if (PRUNIT <= liv) iv[PRUNIT] = 0;	/* suppress all Fortran output */
    if (ALGSAV <= liv) iv[ALGSAV] = alg;
    if (alg < 1 || alg > 4)
	error(_("Rf_divset: alg = %d must be 1, 2, 3, or 4"), alg);

    miv = miniv[alg];
    if (liv < miv) {
	iv[1] = 15;
	return;
    }
    mv = minv[alg];
    if (lv < mv) {
	iv[1] = 16;
	return;
    }
    alg1 = (alg - 1) % 2 + 1;
    F77_CALL(dv7dfl)(&alg1, &lv, &v[1]);
    //       ------
    iv[1] = 12;
    if (alg > 2) error(_("port algorithms 3 or higher are not supported"));
    iv[IVNEED] = 0;
    iv[LASTIV] = miv;
    iv[LASTV] = mv;
    iv[LMAT] = mv + 1;
    iv[MXFCAL] = 200;
    iv[MXITER] = 150;
    iv[OUTLEV] = 0;		/* default is no iteration output */
    iv[PARPRT] = 1;
    iv[PERM] = miv + 1;
    iv[SOLPRT] = 0;		/* was 1 but we suppress Fortran output */
    iv[STATPR] = 0;		/* was 1 but we suppress Fortran output */
    iv[VNEED] = 0;
    iv[X0PRT] = 1;

    if (alg1 >= 2) {		/*  GENERAL OPTIMIZATION values: nlminb() */
	iv[DTYPE] = 0;
	iv[INITS] = 1;
	iv[NFCOV] = 0;
	iv[NGCOV] = 0;
	iv[NVDFLT] = 25;
	iv[PARSAV] = (alg > 2) ? 61 : 47;

	v[AFCTOL] = 0.0; /* since R 2.12.0:  Skip |f(x)| test */
    }
    else { 			/* REGRESSION  values: nls() */
	iv[COVPRT] = 3;
	iv[COVREQ] = 1;
	iv[DTYPE] = 1;
	iv[HC] = 0;
	iv[IERR] = 0;
	iv[INITH] = 0;
	iv[IPIVOT] = 0;
	iv[NVDFLT] = 32;
	iv[VSAVE] = (alg > 2) ? 61 : 58;
	iv[PARSAV] = iv[60] + 9;
	iv[QRTYP] = 1;
	iv[RDREQ] = 3;
	iv[RMAT] = 0;
    }
    return;
}


/* divset.... supply default values for elements of the iv and v arrays */
void F77_NAME(divset)(const int *Alg, int iv[], const int *Liv,
		      const int *Lv, double v[])
{
    Rf_divset(*Alg, iv, *Liv, *Lv, v);
}

/* dn2cvp... prints covariance matrix. */
void F77_NAME(dn2cvp)(const int iv[], int *liv, int *lv, int *p,
		      const double v[])
{
				/* Done elsewhere */
}

/* dn2rdp... prints regression diagnostics for mlpsl and nl2s1. */
void F77_NAME(dn2rdp)(const int iv[], int *liv, int *lv, int *n,
		      const double rd[], const double v[])
{
				/* Done elsewhere */
}

/* ds7cpr... prints linear parameters at solution. */
void F77_NAME(ds7cpr)(const double c[], const int iv[], int *l, int *liv)
{
				/* Done elsewhere */
}

/* dv2axy... computes scalar times one vector plus another */
void F77_NAME(dv2axy)(int *n, double w[], const double *a,
		      const double x[], const double y[])
{
    int i, nn = *n; double aa = *a;
    for (i = 0; i < nn; i++) w[i] = aa * x[i] + y[i];
}

/* dv2nrm... returns the 2-norm of a vector. */
double F77_NAME(dv2nrm)(int *n, const double x[])
{
    int ione = 1;
    return F77_CALL(dnrm2)(n, x, &ione);
}

/* dv7cpy.... copy src to dest */
void F77_NAME(dv7cpy)(int *n, double dest[], const double src[])
{
    /* Was memcpy, but overlaps seen */
    memmove(dest, src, *n * sizeof(double));
}

/* dv7ipr... applies forward permutation to vector.  */
void F77_NAME(dv7ipr)(int *n, const int ip[], double x[])
{
    /* permute x so that x[i] := x[ip[i]]. */
    int i, nn = *n;
    double *xcp = Calloc(nn, double);

    for (i = 0; i < nn; i++) xcp[i] = x[ip[i] - 1]; /* ip contains 1-based indices */
    Memcpy(x, xcp, nn);
    Free(xcp);
}

/* dv7prm... applies reverse permutation to vector.  */
void F77_NAME(dv7prm)(int *n, const int ip[], double x[])
{
    /* permute x so that x[ip[i]] := x[i]. */
    int i, nn = *n;
    double *xcp = Calloc(nn, double);

    for (i = 0; i < nn; i++) xcp[ip[i] - 1] = x[i]; /* ip contains 1-based indices */
    Memcpy(x, xcp, nn);
    Free(xcp);
}

/* dv7scl... scale src by *scal to dest */
void F77_NAME(dv7scl)(int *n, double dest[],
		      const double *scal, const double src[])
{
    int nn = *n; double sc = *scal;
    while (nn-- > 0) *dest++ = sc * *src++;
}

/* dv7scp... set values of an array to a constant */
void F77_NAME(dv7scp)(int *n, double dest[], double *c)
{
    int nn = *n; double cc = *c;
    while (nn-- > 0) *dest++ = cc;
}

/* dv7swp... interchange n-vectors x and y. */
void F77_NAME(dv7swp)(int *n, double x[], double y[])
{
    int ione = 1;
    F77_CALL(dswap)(n, x, &ione, y, &ione);
}

/* i7copy... copies one integer vector to another. */
void F77_NAME(i7copy)(int *n, int dest[], const int src[])
{
    int nn = *n;
    while (nn-- > 0) *dest++ = *src++;
}

/* i7pnvr... inverts permutation array. (Indices in array are 1-based) */
void F77_NAME(i7pnvr)(int *n, int x[], const int y[])
{
    int i, nn = *n;
    for (i = 0; i < nn; i++) x[y[i] - 1] = i + 1;
}

/* stopx.... returns .true. if the break key has been pressed. */
/* should match Fortran LOGICAL, in gfortran int_least32_t but
   compiler-specific */
int F77_NAME(stopx)(void)
{
    return 0;			/* interrupts are caught elsewhere */
}

static
double* check_gv(SEXP gr, SEXP hs, SEXP rho, int n, double *gv, double *hv)
{
    SEXP gval = PROTECT(coerceVector(PROTECT(eval(gr, rho)), REALSXP));
    if (LENGTH(gval) != n)
	error(_("gradient function must return a numeric vector of length %d"), n);
    Memcpy(gv, REAL(gval), n);
    for (int i = 0; i < n; i++)
	if(ISNAN(gv[i])) error("NA/NaN gradient evaluation");
    if (hv) {
	SEXP hval = PROTECT(eval(hs, rho));
	SEXP dim = getAttrib(hval, R_DimSymbol);
	int i, j, pos;
	double *rhval = REAL(hval);

	if (!isReal(hval) || LENGTH(dim) != 2 ||
	    INTEGER(dim)[0] != n || INTEGER(dim)[1] != n)
	    error(_("Hessian function must return a square numeric matrix of order %d"),
		  n);
	for (i = 0, pos = 0; i < n; i++) /* copy lower triangle row-wise */
	    for (j = 0; j <= i; j++) {
		hv[pos] = rhval[i + j * n];
		if(ISNAN(hv[pos])) error("NA/NaN Hessian evaluation");
		pos++;
	    }
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return gv;
}

void
nlminb_iterate(double b[], double d[], double fx, double g[], double h[],
	       int iv[], int liv, int lv, int n, double v[], double x[])
{
    int lh = (n * (n + 1))/2;
    if (b) {
	if (g) {
	    if (h)
		F77_CALL(drmnhb)(b, d, &fx, g, h, iv, &lh, &liv, &lv, &n, v, x);
	    else
		F77_CALL(drmngb)(b, d, &fx, g, iv, &liv, &lv, &n, v, x);
	} else F77_CALL(drmnfb)(b, d, &fx, iv, &liv, &lv, &n, v, x);
    } else {
	if (g) {
	    if (h)
		F77_CALL(drmnh)(d, &fx, g, h, iv, &lh, &liv, &lv, &n, v, x);
	    else
		F77_CALL(drmng)(d, &fx, g, iv, &liv, &lv, &n, v, x);
	} else F77_CALL(drmnf)(d, &fx, iv, &liv, &lv, &n, v, x);
    }
}

// setup working vectors 'iv' and 'v' - called from R's nlminb() :
SEXP port_ivset(SEXP kind, SEXP iv, SEXP v)
{
    Rf_divset(asInteger(kind), INTEGER(iv), LENGTH(iv), LENGTH(v), REAL(v));
    return R_NilValue;
}

// Main routines - called from R's nlminb()
SEXP port_nlminb(SEXP fn, SEXP gr, SEXP hs, SEXP rho,
		 SEXP lowerb, SEXP upperb, SEXP d, SEXP iv, SEXP v)
{
    int i, n = LENGTH(d);
    SEXP xpt;
    SEXP dot_par_symbol = install(".par");
    double *b = (double *) NULL, *g = (double *) NULL,
	*h = (double *) NULL, fx = R_PosInf;
    if (isNull(rho)) {
	error(_("use of NULL environment is defunct"));
	rho = R_BaseEnv;
    } else
    if (!isEnvironment(rho))
	error(_("'rho' must be an environment"));
    if (!isReal(d) || n < 1)
	error(_("'d' must be a nonempty numeric vector"));
    if (hs != R_NilValue && gr == R_NilValue)
	error(_("When Hessian defined must also have gradient defined"));
    if (R_NilValue == (xpt = findVarInFrame(rho, dot_par_symbol)) ||
	!isReal(xpt) || LENGTH(xpt) != n)
	error(_("environment 'rho' must contain a numeric vector '.par' of length %d"),
	      n);
    /* We are going to alter .par, so must duplicate it */
    defineVar(dot_par_symbol, duplicate(xpt), rho);
    PROTECT(xpt = findVarInFrame(rho, dot_par_symbol));

    if ((LENGTH(lowerb) == n) && (LENGTH(upperb) == n)) {
	if (isReal(lowerb) && isReal(upperb)) {
	    double *rl=REAL(lowerb), *ru=REAL(upperb);
	    b = (double *)R_alloc(2*n, sizeof(double));
	    for (i = 0; i < n; i++) {
		b[2*i] = rl[i];
		b[2*i + 1] = ru[i];
	    }
	} else error(_("'lower' and 'upper' must be numeric vectors"));
    }
    if (gr != R_NilValue) {
	g = (double *)R_alloc(n, sizeof(double));
	if (hs != R_NilValue)
	    h = (double *)R_alloc((n * (n + 1))/2, sizeof(double));
    }

    do {
	nlminb_iterate(b, REAL(d), fx, g, h, INTEGER(iv), LENGTH(iv),
		       LENGTH(v), n, REAL(v), REAL(xpt));
	if (INTEGER(iv)[0] == 2 && g) check_gv(gr, hs, rho, n, g, h);
	else {
	    fx = asReal(eval(fn, rho));
	    if (ISNAN(fx)) {
		warning("NA/NaN function evaluation");
		fx = R_PosInf;
	    }
	}

	/* duplicate .par value again in case a callback has stored
	   value (package varComp does this) */
	defineVar(dot_par_symbol, duplicate(xpt), rho);
	xpt = findVarInFrame(rho, dot_par_symbol);
	UNPROTECT(1);
	PROTECT(xpt);
    } while(INTEGER(iv)[0] < 3);

    UNPROTECT(1); /* xpt */
    return R_NilValue;
}

void
nlsb_iterate(double b[], double d[], double dr[], int iv[], int liv,
	     int lv, int n, int nd, int p, double r[], double rd[],
	     double v[], double x[])
{
    int ione = 1;
    if (b)
	F77_CALL(drn2gb)(b, d, dr, iv, &liv, &lv, &n, &nd,
			 &ione, &nd, &p, r, rd, v, x);
    else
	F77_CALL(drn2g)(d, dr, iv, &liv, &lv, &n, &nd, &ione,
			&nd, &p, r, rd, v, x);
}

/**
 * Return the element of a given name from a named list
 *
 * @param list
 * @param nm name of desired element
 *
 * @return element of list with name nm
 */
static R_INLINE SEXP getElement(SEXP list, char *nm)
{
    int i; SEXP names = getAttrib(list, R_NamesSymbol);

    if (!isNewList(list) || LENGTH(names) != LENGTH(list))
	error(_("'getElement' applies only to named lists"));
    for (i = 0; i < LENGTH(list); i++)
	if (!strcmp(CHAR(STRING_ELT(names, i)), nm)) /* ASCII only */
	    return(VECTOR_ELT(list, i));
    return R_NilValue;
}

/**
 * Return the element of a given name from a named list after ensuring
 * that it is a function
 *
 * @param list
 * @param enm name of desired element
 * @param lnm string version of the name of the list
 *
 * @return a SEXP that points to a function
 */
static R_INLINE SEXP getFunc(SEXP list, char *enm, char *lnm)
{
    SEXP ans;
    if (!isFunction(ans = getElement(list, enm)))
	error(_("%s$%s() not found"), lnm, enm);
    return ans;
}

static void neggrad(SEXP gf, SEXP rho, SEXP gg)
{
    SEXP val = PROTECT(eval(gf, rho));
    int *dims = INTEGER(getAttrib(val, R_DimSymbol)),
	*gdims = INTEGER(getAttrib(gg, R_DimSymbol));
    int i, ntot = gdims[0] * gdims[1];

    if (TYPEOF(val) != TYPEOF(gg) || !isMatrix(val) || dims[0] != gdims[0] ||
	dims[1] != gdims[1])
	error(_("'gradient' must be a numeric matrix of dimension (%d,%d)"),
	      gdims[0], gdims[1]);
    for (i = 0; i < ntot; i++) REAL(gg)[i] = - REAL(val)[i];
    UNPROTECT(1);
}

/**
 * Evaluate an expression in an environment, check that the length and
 * mode are as expected and store the result.
 *
 * @param fcn expression to evaluate
 * @param rho environment in which to evaluate it
 * @param vv position to store the result
 *
 * @return vv with new contents
 */
static
SEXP eval_check_store(SEXP fcn, SEXP rho, SEXP vv)
{
    SEXP v = PROTECT(eval(fcn, rho));
    if (TYPEOF(v) != TYPEOF(vv) || LENGTH(v) != LENGTH(vv))
	error(_("fcn produced mode %d, length %d - wanted mode %d, length %d"),
	      TYPEOF(v), LENGTH(v), TYPEOF(vv), LENGTH(vv));
    switch (TYPEOF(v)) {
    case LGLSXP:
	Memcpy(LOGICAL(vv), LOGICAL(v), LENGTH(vv));
	break;
    case INTSXP:
	Memcpy(INTEGER(vv), INTEGER(v), LENGTH(vv));
	break;
    case REALSXP:
	Memcpy(REAL(vv), REAL(v), LENGTH(vv));
	break;
    default:
	error(_("invalid type for eval_check_store"));
    }
    UNPROTECT(1);
    return vv;
}

SEXP port_nlsb(SEXP m, SEXP d, SEXP gg, SEXP iv, SEXP v,
	       SEXP lowerb, SEXP upperb)
{
    int *dims = INTEGER(getAttrib(gg, R_DimSymbol));
    int i, n = LENGTH(d), p = LENGTH(d), nd = dims[0];
    SEXP getPars, setPars, resid, gradient,
	rr = PROTECT(allocVector(REALSXP, nd)),
	x = PROTECT(allocVector(REALSXP, n));
    // This used to use Calloc, but that will leak if 
    // there is a premature return (and did in package drfit)
    double *b = (double *) NULL,
	*rd = (double *)R_alloc(nd, sizeof(double));

    if (!isReal(d) || n < 1)
	error(_("'d' must be a nonempty numeric vector"));
    if(!isNewList(m)) error(_("m must be a list"));
				/* Initialize parameter vector */
    getPars = PROTECT(lang1(getFunc(m, "getPars", "m")));
    eval_check_store(getPars, R_GlobalEnv, x);
				/* Create the setPars call */
    setPars = PROTECT(lang2(getFunc(m, "setPars", "m"), x));
				/* Evaluate residual and gradient */
    resid = PROTECT(lang1(getFunc(m, "resid", "m")));
    eval_check_store(resid, R_GlobalEnv, rr);
    gradient = PROTECT(lang1(getFunc(m, "gradient", "m")));
    neggrad(gradient, R_GlobalEnv, gg);

    if ((LENGTH(lowerb) == n) && (LENGTH(upperb) == n)) {
	if (isReal(lowerb) && isReal(upperb)) {
	    double *rl = REAL(lowerb), *ru = REAL(upperb);
	    b = (double *)R_alloc(2*n, sizeof(double));
	    for (i = 0; i < n; i++) {
		b[2*i] = rl[i];
		b[2*i + 1] = ru[i];
	    }
	} else error(_("'lowerb' and 'upperb' must be numeric vectors"));
    }

    do {
	nlsb_iterate(b, REAL(d), REAL(gg), INTEGER(iv), LENGTH(iv),
		     LENGTH(v), n, nd, p, REAL(rr), rd,
		     REAL(v), REAL(x));
	switch(INTEGER(iv)[0]) {
	case -3:
	    eval(setPars, R_GlobalEnv);
	    eval_check_store(resid, R_GlobalEnv, rr);
	    neggrad(gradient, R_GlobalEnv, gg);
	    break;
	case -2:
	    eval_check_store(resid, R_GlobalEnv, rr);
	    neggrad(gradient, R_GlobalEnv, gg);
	    break;
	case -1:
	    eval(setPars, R_GlobalEnv);
	    eval_check_store(resid, R_GlobalEnv, rr);
	    neggrad(gradient, R_GlobalEnv, gg);
	    break;
	case 0:
	    Rprintf("nlsb_iterate returned %d", INTEGER(iv)[0]);
	    break;
	case 1:
	    eval(setPars, R_GlobalEnv);
	    eval_check_store(resid, R_GlobalEnv, rr);
	    break;
	case 2:
	    eval(setPars, R_GlobalEnv);
	    neggrad(gradient, R_GlobalEnv, gg);
	    break;
	}
    } while(INTEGER(iv)[0] < 3);

    UNPROTECT(6);
    return R_NilValue;
}
