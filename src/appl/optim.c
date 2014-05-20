/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2013  The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/Random.h>	/* for the random number generation in
				   samin() */
#include <R_ext/Applic.h>
#include <R_ext/Print.h>	/* for Rprintf */

static double * vect(int n)
{
    return (double *)R_alloc(n, sizeof(double));
}

typedef struct opt_struct
{
    SEXP R_fcall;    /* function */
    SEXP R_gcall;    /* gradient */
    SEXP R_env;      /* where to evaluate the calls */
    double* ndeps;   /* tolerances for numerical derivatives */
    double fnscale;  /* scaling for objective */
    double* parscale;/* scaling for parameters */
    int usebounds;
    double* lower, *upper;
    SEXP names;	     /* names for par */
} opt_struct, *OptStruct;

static void genptry(int n, double *p, double *ptry, double scale, void *ex)
{
    SEXP s, x;
    int i;
    OptStruct OS = (OptStruct) ex;
    PROTECT_INDEX ipx;

    if (!isNull(OS->R_gcall)) {
	/* user defined generation of candidate point */
	PROTECT(x = allocVector(REALSXP, n));
	for (i = 0; i < n; i++) {
	    if (!R_FINITE(p[i]))
		error(_("non-finite value supplied by 'optim'"));
	    REAL(x)[i] = p[i] * (OS->parscale[i]);
	}
	SETCADR(OS->R_gcall, x);
	PROTECT_WITH_INDEX(s = eval(OS->R_gcall, OS->R_env), &ipx);
	REPROTECT(s = coerceVector(s, REALSXP), ipx);
	if(LENGTH(s) != n)
	    error(_("candidate point in 'optim' evaluated to length %d not %d"),
		  LENGTH(s), n);
	for (i = 0; i < n; i++)
	    ptry[i] = REAL(s)[i] / (OS->parscale[i]);
	UNPROTECT(2);
    }
    else {  /* default Gaussian Markov kernel */
	for (i = 0; i < n; i++)
	    ptry[i] = p[i] + scale * norm_rand();  /* new candidate point */
    }
}


static double ** matrix(int nrh, int nch)
{
    int   i;
    double **m;

    m = (double **) R_alloc((nrh + 1), sizeof(double *));
    for (i = 0; i <= nrh; i++)
	m[i] = (double*) R_alloc((nch + 1), sizeof(double));
    return m;
}

static double ** Lmatrix(int n)
{
    int   i;
    double **m;

    m = (double **) R_alloc(n, sizeof(double *));
    for (i = 0; i < n; i++)
	m[i] = (double *) R_alloc((i + 1), sizeof(double));
    return m;
}



#define stepredn	0.2
#define acctol		0.0001
#define reltest		10.0


/*  BFGS variable-metric method, based on Pascal code
in J.C. Nash, `Compact Numerical Methods for Computers', 2nd edition,
converted by p2c then re-crafted by B.D. Ripley */

void
vmmin(int n0, double *b, double *Fmin, optimfn fminfn, optimgr fmingr,
      int maxit, int trace, int *mask,
      double abstol, double reltol, int nREPORT, void *ex,
      int *fncount, int *grcount, int *fail)
{
    Rboolean accpoint, enough;
    double *g, *t, *X, *c, **B;
    int   count, funcount, gradcount;
    double f, gradproj;
    int   i, j, ilast, iter = 0;
    double s, steplength;
    double D1, D2;
    int   n, *l;

    if (maxit <= 0) {
	*fail = 0;
	*Fmin = fminfn(n0, b, ex);
	*fncount = *grcount = 0;
	return;
    }

    if (nREPORT <= 0)
	error(_("REPORT must be > 0 (method = \"BFGS\")"));
    l = (int *) R_alloc(n0, sizeof(int));
    n = 0;
    for (i = 0; i < n0; i++) if (mask[i]) l[n++] = i;
    g = vect(n0);
    t = vect(n);
    X = vect(n);
    c = vect(n);
    B = Lmatrix(n);
    f = fminfn(n0, b, ex);
    if (!R_FINITE(f))
	error(_("initial value in 'vmmin' is not finite"));
    if (trace) Rprintf("initial  value %f \n", f);
    *Fmin = f;
    funcount = gradcount = 1;
    fmingr(n0, b, g, ex);
    iter++;
    ilast = gradcount;

    do {
	if (ilast == gradcount) {
	    for (i = 0; i < n; i++) {
		for (j = 0; j < i; j++) B[i][j] = 0.0;
		B[i][i] = 1.0;
	    }
	}
	for (i = 0; i < n; i++) {
	    X[i] = b[l[i]];
	    c[i] = g[l[i]];
	}
	gradproj = 0.0;
	for (i = 0; i < n; i++) {
	    s = 0.0;
	    for (j = 0; j <= i; j++) s -= B[i][j] * g[l[j]];
	    for (j = i + 1; j < n; j++) s -= B[j][i] * g[l[j]];
	    t[i] = s;
	    gradproj += s * g[l[i]];
	}

	if (gradproj < 0.0) {	/* search direction is downhill */
	    steplength = 1.0;
	    accpoint = FALSE;
	    do {
		count = 0;
		for (i = 0; i < n; i++) {
		    b[l[i]] = X[i] + steplength * t[i];
		    if (reltest + X[i] == reltest + b[l[i]]) /* no change */
			count++;
		}
		if (count < n) {
		    f = fminfn(n0, b, ex);
		    funcount++;
		    accpoint = R_FINITE(f) &&
			(f <= *Fmin + gradproj * steplength * acctol);
		    if (!accpoint) {
			steplength *= stepredn;
		    }
		}
	    } while (!(count == n || accpoint));
	    enough = (f > abstol) &&
		fabs(f - *Fmin) > reltol * (fabs(*Fmin) + reltol);
	    /* stop if value if small or if relative change is low */
	    if (!enough) {
		count = n;
		*Fmin = f;
	    }
	    if (count < n) {/* making progress */
		*Fmin = f;
		fmingr(n0, b, g, ex);
		gradcount++;
		iter++;
		D1 = 0.0;
		for (i = 0; i < n; i++) {
		    t[i] = steplength * t[i];
		    c[i] = g[l[i]] - c[i];
		    D1 += t[i] * c[i];
		}
		if (D1 > 0) {
		    D2 = 0.0;
		    for (i = 0; i < n; i++) {
			s = 0.0;
			for (j = 0; j <= i; j++)
			    s += B[i][j] * c[j];
			for (j = i + 1; j < n; j++)
			    s += B[j][i] * c[j];
			X[i] = s;
			D2 += s * c[i];
		    }
		    D2 = 1.0 + D2 / D1;
		    for (i = 0; i < n; i++) {
			for (j = 0; j <= i; j++)
			    B[i][j] += (D2 * t[i] * t[j]
					- X[i] * t[j] - t[i] * X[j]) / D1;
		    }
		} else {	/* D1 < 0 */
		    ilast = gradcount;
		}
	    } else {	/* no progress */
		if (ilast < gradcount) {
		    count = 0;
		    ilast = gradcount;
		}
	    }
	} else {		/* uphill search */
	    count = 0;
	    if (ilast == gradcount) count = n;
	    else ilast = gradcount;
	    /* Resets unless has just been reset */
	}
	if (trace && (iter % nREPORT == 0))
	    Rprintf("iter%4d value %f\n", iter, f);
	if (iter >= maxit) break;
	if (gradcount - ilast > 2 * n)
	    ilast = gradcount;	/* periodic restart */
    } while (count != n || ilast != gradcount);
    if (trace) {
	Rprintf("final  value %f \n", *Fmin);
	if (iter < maxit) Rprintf("converged\n");
	else Rprintf("stopped after %i iterations\n", iter);
    }
    *fail = (iter < maxit) ? 0 : 1;
    *fncount = funcount;
    *grcount = gradcount;
}


#define big             1.0e+35   /*a very large number*/


/* Nelder-Mead, based on Pascal code
   in J.C. Nash, `Compact Numerical Methods for Computers', 2nd edition,
   converted by p2c then re-crafted by B.D. Ripley */
void nmmin(int n, double *Bvec, double *X, double *Fmin, optimfn fminfn,
	   int *fail, double abstol, double intol, void *ex,
	   double alpha, double bet, double gamm, int trace,
	   int *fncount, int maxit)
{
    char action[50];
    int C;
    Rboolean calcvert;
    double convtol, f;
    int funcount=0, H, i, j, L=0;
    int n1=0;
    double oldsize;
    double **P;
    double size, step, temp, trystep;
    char tstr[9]; // allow for 10^8 iters ...
    double VH, VL, VR;

    if (maxit <= 0) {
	*Fmin = fminfn(n, Bvec, ex);
	*fncount = 0;
	*fail = 0;
	return;
    }
    if (trace)
	Rprintf("  Nelder-Mead direct search function minimizer\n");
    P = matrix(n, n+1);
    *fail = FALSE;
    f = fminfn(n, Bvec, ex);
    if (!R_FINITE(f)) {
	error(_("function cannot be evaluated at initial parameters"));
	*fail = TRUE;
    } else {
	if (trace) Rprintf("function value for initial parameters = %f\n", f);
	funcount = 1;
	convtol = intol * (fabs(f) + intol);
	if (trace) Rprintf("  Scaled convergence tolerance is %g\n", convtol);
	n1 = n + 1;
	C = n + 2;
	P[n1 - 1][0] = f;
	for (i = 0; i < n; i++)
	    P[i][0] = Bvec[i];

	L = 1;
	size = 0.0;

	step = 0.0;
	for (i = 0; i < n; i++) {
	    if (0.1 * fabs(Bvec[i]) > step)
		step = 0.1 * fabs(Bvec[i]);
	}
	if (step == 0.0) step = 0.1;
	if (trace) Rprintf("Stepsize computed as %f\n", step);
	for (j = 2; j <= n1; j++) {
	    strcpy(action, "BUILD          ");
	    for (i = 0; i < n; i++)
		P[i][j - 1] = Bvec[i];

	    trystep = step;
	    while (P[j - 2][j - 1] == Bvec[j - 2]) {
		P[j - 2][j - 1] = Bvec[j - 2] + trystep;
		trystep *= 10;
	    }
	    size += trystep;
	}
	oldsize = size;
	calcvert = TRUE;
	do {
	    if (calcvert) {
		for (j = 0; j < n1; j++) {
		    if (j + 1 != L) {
			for (i = 0; i < n; i++)
			    Bvec[i] = P[i][j];
			f = fminfn(n, Bvec, ex);
			if (!R_FINITE(f)) f = big;
			funcount++;
			P[n1 - 1][j] = f;
		    }
		}
		calcvert = FALSE;
	    }

	    VL = P[n1 - 1][L - 1];
	    VH = VL;
	    H = L;

	    for (j = 1; j <= n1; j++) {
		if (j != L) {
		    f = P[n1 - 1][j - 1];
		    if (f < VL) {
			L = j;
			VL = f;
		    }
		    if (f > VH) {
			H = j;
			VH = f;
		    }
		}
	    }

	    if (VH <= VL + convtol || VL <= abstol) break;

	    // avoid buffer overflow at 100001 iters. (PR#15240)
	    if (trace) {
		snprintf(tstr, 9, "%5d", funcount);
		Rprintf("%s%s %f %f\n", action, tstr, VH, VL);
	    }

	    for (i = 0; i < n; i++) {
		temp = -P[i][H - 1];
		for (j = 0; j < n1; j++)
		    temp += P[i][j];
		P[i][C - 1] = temp / n;
	    }
	    for (i = 0; i < n; i++)
		Bvec[i] = (1.0 + alpha) * P[i][C - 1] - alpha * P[i][H - 1];
	    f = fminfn(n, Bvec, ex);
	    if (!R_FINITE(f)) f = big;
	    funcount++;
	    strcpy(action, "REFLECTION     ");
	    VR = f;
	    if (VR < VL) {
		P[n1 - 1][C - 1] = f;
		for (i = 0; i < n; i++) {
		    f = gamm * Bvec[i] + (1 - gamm) * P[i][C - 1];
		    P[i][C - 1] = Bvec[i];
		    Bvec[i] = f;
		}
		f = fminfn(n, Bvec, ex);
		if (!R_FINITE(f)) f = big;
		funcount++;
		if (f < VR) {
		    for (i = 0; i < n; i++)
			P[i][H - 1] = Bvec[i];
		    P[n1 - 1][H - 1] = f;
		    strcpy(action, "EXTENSION      ");
		} else {
		    for (i = 0; i < n; i++)
			P[i][H - 1] = P[i][C - 1];
		    P[n1 - 1][H - 1] = VR;
		}
	    } else {
		strcpy(action, "HI-REDUCTION   ");
		if (VR < VH) {
		    for (i = 0; i < n; i++)
			P[i][H - 1] = Bvec[i];
		    P[n1 - 1][H - 1] = VR;
		    strcpy(action, "LO-REDUCTION   ");
		}

		for (i = 0; i < n; i++)
		    Bvec[i] = (1 - bet) * P[i][H - 1] + bet * P[i][C - 1];
		f = fminfn(n, Bvec, ex);
		if (!R_FINITE(f)) f = big;
		funcount++;

		if (f < P[n1 - 1][H - 1]) {
		    for (i = 0; i < n; i++)
			P[i][H - 1] = Bvec[i];
		    P[n1 - 1][H - 1] = f;
		} else {
		    if (VR >= VH) {
			strcpy(action, "SHRINK         ");
			calcvert = TRUE;
			size = 0.0;
			for (j = 0; j < n1; j++) {
			    if (j + 1 != L) {
				for (i = 0; i < n; i++) {
				    P[i][j] = bet * (P[i][j] - P[i][L - 1])
					+ P[i][L - 1];
				    size += fabs(P[i][j] - P[i][L - 1]);
				}
			    }
			}
			if (size < oldsize) {
			    oldsize = size;
			} else {
			    if (trace)
				Rprintf("Polytope size measure not decreased in shrink\n");
			    *fail = 10;
			    break;
			}
		    }
		}
	    }

	} while (funcount <= maxit);

    }

    if (trace) {
	Rprintf("Exiting from Nelder Mead minimizer\n");
	Rprintf("    %d function evaluations used\n", funcount);
    }
    *Fmin = P[n1 - 1][L - 1];
    for (i = 0; i < n; i++) X[i] = P[i][L - 1];
    if (funcount > maxit) *fail = 1;
    *fncount = funcount;
}

/* Conjugate gradients, based on Pascal code
   in J.C. Nash, `Compact Numerical Methods for Computers', 2nd edition,
   converted by p2c then re-crafted by B.D. Ripley */
void cgmin(int n, double *Bvec, double *X, double *Fmin,
	   optimfn fminfn, optimgr fmingr, int *fail,
	   double abstol, double intol, void *ex, int type, int trace,
	   int *fncount, int *grcount, int maxit)
{
    Rboolean accpoint;
    double *c, *g, *t;
    int count, cycle, cyclimit;
    double f;
    double G1, G2, G3, gradproj;
    int funcount=0, gradcount=0, i;
    double newstep, oldstep, setstep, steplength=1.0;
    double tol;

    if (maxit <= 0) {
	*Fmin = fminfn(n, Bvec, ex);
	*fncount = *grcount = 0;
	*fail = FALSE;
	return;
    }
    if (trace) {
	Rprintf("  Conjugate gradients function minimizer\n");
	switch (type) {
	case 1:	    Rprintf("Method: Fletcher Reeves\n");	break;
	case 2:	    Rprintf("Method: Polak Ribiere\n");		break;
	case 3:	    Rprintf("Method: Beale Sorenson\n");	break;
	default:
	    error(_("unknown 'type' in \"CG\" method of 'optim'"));
	}
    }
    c = vect(n); g = vect(n); t = vect(n);

    setstep = 1.7;
    *fail = 0;
    cyclimit = n;
    tol = intol * n * sqrt(intol);

    if (trace) Rprintf("tolerance used in gradient test=%g\n", tol);
    f = fminfn(n, Bvec, ex);
    if (!R_FINITE(f)) {
	error(_("Function cannot be evaluated at initial parameters"));
    } else {
	*Fmin = f;
	funcount = 1;
	gradcount = 0;
	do {
	    for (i = 0; i < n; i++) {
		t[i] = 0.0;
		c[i] = 0.0;
	    }
	    cycle = 0;
	    oldstep = 1.0;
	    count = 0;
	    do {
		cycle++;
		if (trace) {
		    Rprintf("%d %d %f\n", gradcount, funcount, *Fmin);
		    Rprintf("parameters ");
		    for (i = 1; i <= n; i++) {
			Rprintf("%10.5f ", Bvec[i - 1]);
			if (i / 7 * 7 == i && i < n)
			    Rprintf("\n");
		    }
		    Rprintf("\n");
		}
		gradcount++;
		if (gradcount > maxit) {
		    *fncount = funcount;
		    *grcount = gradcount;
		    *fail = 1;
		    return;
		}
		fmingr(n, Bvec, g, ex);
		G1 = 0.0;
		G2 = 0.0;
		for (i = 0; i < n; i++) {
		    X[i] = Bvec[i];
		    switch (type) {

		    case 1: /* Fletcher-Reeves */
			G1 += g[i] * g[i];
			G2 += c[i] * c[i];
			break;

		    case 2: /* Polak-Ribiere */
			G1 += g[i] * (g[i] - c[i]);
			G2 += c[i] * c[i];
			break;

		    case 3: /* Beale-Sorenson */
			G1 += g[i] * (g[i] - c[i]);
			G2 += t[i] * (g[i] - c[i]);
			break;

		    default:
			error(_("unknown type in \"CG\" method of 'optim'"));
		    }
		    c[i] = g[i];
		}
		if (G1 > tol) {
		    if (G2 > 0.0)
			G3 = G1 / G2;
		    else
			G3 = 1.0;
		    gradproj = 0.0;
		    for (i = 0; i < n; i++) {
			t[i] = t[i] * G3 - g[i];
			gradproj += t[i] * g[i];
		    }
		    steplength = oldstep;

		    accpoint = FALSE;
		    do {
			count = 0;
			for (i = 0; i < n; i++) {
			    Bvec[i] = X[i] + steplength * t[i];
			    if (reltest + X[i] == reltest + Bvec[i])
				count++;
			}
			if (count < n) { /* point is different */
			    f = fminfn(n, Bvec, ex);
			    funcount++;
			    accpoint = (R_FINITE(f) &&
					f <= *Fmin + gradproj * steplength * acctol);

			    if (!accpoint) {
				steplength *= stepredn;
				if (trace) Rprintf("*");
			    } else *Fmin = f; /* we improved, so update value */
			}
		    } while (!(count == n || accpoint));
		    if (count < n) {
			newstep = 2 * (f - *Fmin - gradproj * steplength);
			if (newstep > 0) {
			    newstep = -(gradproj * steplength * steplength / newstep);
			    for (i = 0; i < n; i++)
				Bvec[i] = X[i] + newstep * t[i];
			    *Fmin = f;
			    f = fminfn(n, Bvec, ex);
			    funcount++;
			    if (f < *Fmin) {
				*Fmin = f;
				if (trace) Rprintf(" i< ");
			    } else { /* reset Bvec to match lowest point */
				if (trace) Rprintf(" i> ");
				for (i = 0; i < n; i++)
				    Bvec[i] = X[i] + steplength * t[i];
			    }
			}
		    }
		}
		oldstep = setstep * steplength;
		if (oldstep > 1.0)
		    oldstep = 1.0;
	    } while ((count != n) && (G1 > tol) && (cycle != cyclimit));

	} while ((cycle != 1) ||
		 ((count != n) && (G1 > tol) && *Fmin > abstol));

    }
    if (trace) {
	Rprintf("Exiting from conjugate gradients minimizer\n");
	Rprintf("    %d function evaluations used\n", funcount);
	Rprintf("    %d gradient evaluations used\n", gradcount);
    }
    *fncount = funcount;
    *grcount = gradcount;
}

/* include setulb() */
#include "lbfgsb.c"

void lbfgsb(int n, int m, double *x, double *l, double *u, int *nbd,
	    double *Fmin, optimfn fminfn, optimgr fmingr, int *fail,
	    void *ex, double factr, double pgtol,
	    int *fncount, int *grcount, int maxit, char *msg,
	    int trace, int nREPORT)
{
    char task[60];
    double f, *g, dsave[29], *wa;
    int tr = -1, iter = 0, *iwa, isave[22];

    if(n == 0) { /* not handled in setulb */
	*fncount = 1;
	*grcount = 0;
	*Fmin = fminfn(n, u, ex);
	strcpy(msg, "NOTHING TO DO");
	*fail = 0;
	return;
    }
    if (nREPORT <= 0)
	error(_("REPORT must be > 0 (method = \"L-BFGS-B\")"));
    switch(trace) {
    case 2: tr = 0; break;
    case 3: tr = nREPORT; break;
    case 4: tr = 99; break;
    case 5: tr = 100; break;
    case 6: tr = 101; break;
    default: tr = -1; break;
    }

    *fail = 0;
    g = vect(n);
    /* this needs to be zeroed for snd in mainlb to be zeroed */
    wa = (double *) S_alloc(2*m*n+4*n+11*m*m+8*m, sizeof(double));
    iwa = (int *) R_alloc(3*n, sizeof(int));
    strcpy(task, "START");
    while(1) {
	setulb(n, m, x, l, u, nbd, &f, g, factr, &pgtol, wa, iwa, task,
	       tr, isave, dsave);
/*	Rprintf("in lbfgsb - %s\n", task);*/
	if (strncmp(task, "FG", 2) == 0) {
	    f = fminfn(n, x, ex);
	    if (!R_FINITE(f))
		error(_("L-BFGS-B needs finite values of 'fn'"));
	    fmingr(n, x, g, ex);
	} else if (strncmp(task, "NEW_X", 5) == 0) {
	    iter++;
	    if(trace == 1 && (iter % nREPORT == 0)) {
		Rprintf("iter %4d value %f\n", iter, f);
	    }
	    if (iter > maxit) {
		*fail = 1;
		break;
	    }
	} else if (strncmp(task, "WARN", 4) == 0) {
	    *fail = 51;
	    break;
	} else if (strncmp(task, "CONV", 4) == 0) {
	    break;
	} else if (strncmp(task, "ERROR", 5) == 0) {
	    *fail = 52;
	    break;
	} else { /* some other condition that is not supposed to happen */
	    *fail = 52;
	    break;
	}
    }
    *Fmin = f;
    *fncount = *grcount = isave[12];
    if (trace) {
	Rprintf("final  value %f \n", *Fmin);
	if (iter < maxit && *fail == 0) Rprintf("converged\n");
	else Rprintf("stopped after %i iterations\n", iter);
    }
    strcpy(msg, task);
}


#define E1 1.7182818  /* exp(1.0)-1.0 */

void samin(int n, double *pb, double *yb, optimfn fminfn, int maxit,
	   int tmax, double ti, int trace, void *ex)

/* Given a starting point pb[0..n-1], simulated annealing minimization
   is performed on the function fminfn. The starting temperature
   is input as ti. To make sann work silently set trace to zero.
   sann makes in total maxit function evaluations, tmax
   evaluations at each temperature. Returned quantities are pb
   (the location of the minimum), and yb (the minimum value of
   the function func).  Author: Adrian Trapletti
*/
{
    long j;
    int k, its, itdoc;
    double t, y, dy, ytry, scale;
    double *p, *ptry;

    /* Above have: if(trace != 0) trace := REPORT control argument = STEPS */
    if (trace < 0)
	error(_("trace, REPORT must be >= 0 (method = \"SANN\")"));

    if(n == 0) { /* don't even attempt to optimize */
	*yb = fminfn(n, pb, ex);
	return;
    }
    p = vect (n); ptry = vect (n);
    GetRNGstate();
    *yb = fminfn (n, pb, ex);  /* init best system state pb, *yb */
    if (!R_FINITE(*yb)) *yb = big;
    for (j = 0; j < n; j++) p[j] = pb[j];
    y = *yb;  /* init system state p, y */
    if (trace) {
	Rprintf ("sann objective function values\n");
	Rprintf ("initial       value %f\n", *yb);
    }
    scale = 1.0/ti;
    its = itdoc = 1;
    while (its < maxit) {  /* cool down system */
	t = ti/log((double)its + E1);  /* temperature annealing schedule */
	k = 1;
	while ((k <= tmax) && (its < maxit))  /* iterate at constant temperature */
	{
	    genptry(n, p, ptry, scale * t, ex);  /* generate new candidate point */
	    ytry = fminfn (n, ptry, ex);
	    if (!R_FINITE(ytry)) ytry = big;
	    dy = ytry - y;
	    if ((dy <= 0.0) || (unif_rand() < exp(-dy/t))) {  /* accept new point? */
		for (j = 0; j < n; j++) p[j] = ptry[j];
		y = ytry;  /* update system state p, y */
		if (y <= *yb)  /* if system state is best, then update best system state pb, *yb */
		{
		    for (j = 0; j < n; j++) pb[j] = p[j];
		    *yb = y;
		}
	    }
	    its++; k++;
	}
	if (trace && ((itdoc % trace) == 0))
	    Rprintf("iter %8d value %f\n", its - 1, *yb);
	itdoc++;
    }
    if (trace) {
	Rprintf ("final         value %f\n", *yb);
	Rprintf ("sann stopped after %d iterations\n", its - 1);
    }
    PutRNGstate();
}

#undef E1
