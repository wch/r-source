/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2015   The R Core Team.
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include <stdlib.h> // for abs
#include <string.h>
#include <math.h>

#include <R.h>
#include "ts.h"
#include "statsR.h" // for getListElement

#ifndef max
#define max(a,b) ((a < b)?(b):(a))
#endif
#ifndef min
#define min(a,b) ((a < b)?(a):(b))
#endif


/* 
  KalmanLike, internal to StructTS:
  .Call(C_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h, mod$Pn,
        nit, FALSE, update)
  KalmanRun:
  .Call(C_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V, mod$h, mod$Pn,
        nit, TRUE, update)
*/

/* y vector length n of observations
   Z vector length p for observation equation y_t = Za_t +  eps_t
   a vector length p of initial state
   P p x p matrix for initial state uncertainty (contemparaneous)
   T  p x p transition matrix
   V  p x p = RQR'
   h = var(eps_t)
   anew used for a[t|t-1]
   Pnew used for P[t|t -1]
   M used for M = P[t|t -1]Z

   op is FALSE for KalmanLike, TRUE for KalmanRun.  
   The latter computes residuals and states and has 
   a more elaborate return value.

   Almost no checking here!
 */

SEXP
KalmanLike(SEXP sy, SEXP mod, SEXP sUP, SEXP op, SEXP update)
{
    int lop = asLogical(op);
    mod = PROTECT(duplicate(mod));

    SEXP sZ = getListElement(mod, "Z"), sa = getListElement(mod, "a"), 
	sP = getListElement(mod, "P"), sT = getListElement(mod, "T"), 
	sV = getListElement(mod, "V"), sh = getListElement(mod, "h"),
	sPn = getListElement(mod, "Pn");

    if (TYPEOF(sy) != REALSXP || TYPEOF(sZ) != REALSXP ||
	TYPEOF(sa) != REALSXP || TYPEOF(sP) != REALSXP ||
	TYPEOF(sPn) != REALSXP ||
	TYPEOF(sT) != REALSXP || TYPEOF(sV) != REALSXP)
	error(_("invalid argument type"));

    int n = LENGTH(sy), p = LENGTH(sa);
    double *y = REAL(sy), *Z = REAL(sZ), *T = REAL(sT), *V = REAL(sV),
	*P = REAL(sP), *a = REAL(sa), *Pnew = REAL(sPn), h = asReal(sh);

    double *anew = (double *) R_alloc(p, sizeof(double));
    double *M = (double *) R_alloc(p, sizeof(double));
    double *mm = (double *) R_alloc(p * p, sizeof(double));
    // These are only used if(lop), but avoid -Wall trouble
    SEXP ans = R_NilValue, resid = R_NilValue, states = R_NilValue;
    if(lop) {
	PROTECT(ans = allocVector(VECSXP, 3));
	SET_VECTOR_ELT(ans, 1, resid = allocVector(REALSXP, n));
	SET_VECTOR_ELT(ans, 2, states = allocMatrix(REALSXP, n, p));
	SEXP nm = PROTECT(allocVector(STRSXP, 3));
	SET_STRING_ELT(nm, 0, mkChar("values"));
	SET_STRING_ELT(nm, 1, mkChar("resid"));
	SET_STRING_ELT(nm, 2, mkChar("states"));
	setAttrib(ans, R_NamesSymbol, nm);
	UNPROTECT(1);
    }

    double sumlog = 0.0, ssq = 0.0;
    int nu = 0;
    for (int l = 0; l < n; l++) {
	for (int i = 0; i < p; i++) {
	    double tmp = 0.0;
	    for (int k = 0; k < p; k++)
		tmp += T[i + p * k] * a[k];
	    anew[i] = tmp;
	}
	if (l > asInteger(sUP)) {
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = 0.0;
		    for (int k = 0; k < p; k++)
			tmp += T[i + p * k] * P[k + p * j];
		    mm[i + p * j] = tmp;
		}
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = V[i + p * j];
		    for (int k = 0; k < p; k++)
			tmp += mm[i + p * k] * T[j + p * k];
		    Pnew[i + p * j] = tmp;
		}
	}
	if (!ISNAN(y[l])) {
	    nu++;
	    double *rr = NULL /* -Wall */;
	    if(lop) rr = REAL(resid);
	    double resid0 = y[l];
	    for (int i = 0; i < p; i++)
		resid0 -= Z[i] * anew[i];
	    double gain = h;
	    for (int i = 0; i < p; i++) {
		double tmp = 0.0;
		for (int j = 0; j < p; j++)
		    tmp += Pnew[i + j * p] * Z[j];
		M[i] = tmp;
		gain += Z[i] * M[i];
	    }
	    ssq += resid0 * resid0 / gain;
	    if(lop) rr[l] = resid0 / sqrt(gain);
	    sumlog += log(gain);
	    for (int i = 0; i < p; i++)
		a[i] = anew[i] + M[i] * resid0 / gain;
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++)
		    P[i + j * p] = Pnew[i + j * p] - M[i] * M[j] / gain;
	} else {
	    double *rr = NULL /* -Wall */;
	    if(lop) rr = REAL(resid);
	    for (int i = 0; i < p; i++)
		a[i] = anew[i];
	    for (int i = 0; i < p * p; i++)
		P[i] = Pnew[i];
	    if(lop) rr[l] = NA_REAL;
	}
	if(lop) {
	    double *rs = REAL(states);
	    for (int j = 0; j < p; j++) rs[l + n*j] = a[j];
	}
    }

    SEXP res = PROTECT(allocVector(REALSXP, 2));
    REAL(res)[0] = ssq/nu; REAL(res)[1] = sumlog/nu;
    if(lop) {
	SET_VECTOR_ELT(ans, 0, res);
	if(asLogical(update)) setAttrib(ans, install("mod"), mod);
	UNPROTECT(3);
	return ans;
    } else {
	if(asLogical(update)) setAttrib(res, install("mod"), mod);
	UNPROTECT(2);
	return res;
    }
}

SEXP
KalmanSmooth(SEXP sy, SEXP mod, SEXP sUP)
{
    SEXP sZ = getListElement(mod, "Z"), sa = getListElement(mod, "a"), 
	sP = getListElement(mod, "P"), sT = getListElement(mod, "T"), 
	sV = getListElement(mod, "V"), sh = getListElement(mod, "h"),
	sPn = getListElement(mod, "Pn");

    if (TYPEOF(sy) != REALSXP || TYPEOF(sZ) != REALSXP ||
	TYPEOF(sa) != REALSXP || TYPEOF(sP) != REALSXP ||
	TYPEOF(sT) != REALSXP || TYPEOF(sV) != REALSXP)
	error(_("invalid argument type"));

    SEXP ssa, ssP, ssPn, res, states = R_NilValue, sN;
    int n = LENGTH(sy), p = LENGTH(sa);
    double *y = REAL(sy), *Z = REAL(sZ), *a, *P,
	*T = REAL(sT), *V = REAL(sV), h = asReal(sh), *Pnew;
    double *at, *rt, *Pt, *gains, *resids, *Mt, *L, gn, *Nt;
    Rboolean var = TRUE;

    PROTECT(ssa = duplicate(sa)); a = REAL(ssa);
    PROTECT(ssP = duplicate(sP)); P = REAL(ssP);
    PROTECT(ssPn = duplicate(sPn)); Pnew = REAL(ssPn);

    PROTECT(res = allocVector(VECSXP, 2));
    SEXP nm = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("smooth"));
    SET_STRING_ELT(nm, 1, mkChar("var"));
    setAttrib(res, R_NamesSymbol, nm);
    UNPROTECT(1);
    SET_VECTOR_ELT(res, 0, states = allocMatrix(REALSXP, n, p));
    at = REAL(states);
    SET_VECTOR_ELT(res, 1, sN = allocVector(REALSXP, n*p*p));
    Nt = REAL(sN);

    double *anew, *mm, *M;
    anew = (double *) R_alloc(p, sizeof(double));
    M = (double *) R_alloc(p, sizeof(double));
    mm = (double *) R_alloc(p * p, sizeof(double));

    Pt = (double *) R_alloc(n * p * p, sizeof(double));
    gains = (double *) R_alloc(n, sizeof(double));
    resids = (double *) R_alloc(n, sizeof(double));
    Mt = (double *) R_alloc(n * p, sizeof(double));
    L = (double *) R_alloc(p * p, sizeof(double));

    for (int l = 0; l < n; l++) {
	for (int i = 0; i < p; i++) {
	    double tmp = 0.0;
	    for (int k = 0; k < p; k++)
		tmp += T[i + p * k] * a[k];
	    anew[i] = tmp;
	}
	if (l > asInteger(sUP)) {
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = 0.0;
		    for (int k = 0; k < p; k++)
			tmp += T[i + p * k] * P[k + p * j];
		    mm[i + p * j] = tmp;
		}
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = V[i + p * j];
		    for (int k = 0; k < p; k++)
			tmp += mm[i + p * k] * T[j + p * k];
		    Pnew[i + p * j] = tmp;
		}
	}
	for (int i = 0; i < p; i++) at[l + n*i] = anew[i];
	for (int i = 0; i < p*p; i++) Pt[l + n*i] = Pnew[i];
	if (!ISNAN(y[l])) {
	    double resid0 = y[l];
	    for (int i = 0; i < p; i++)
		resid0 -= Z[i] * anew[i];
	    double gain = h;
	    for (int i = 0; i < p; i++) {
		double tmp = 0.0;
		for (int j = 0; j < p; j++)
		    tmp += Pnew[i + j * p] * Z[j];
		Mt[l + n*i] = M[i] = tmp;
		gain += Z[i] * M[i];
	    }
	    gains[l] = gain;
	    resids[l] = resid0;
	    for (int i = 0; i < p; i++)
		a[i] = anew[i] + M[i] * resid0 / gain;
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++)
		    P[i + j * p] = Pnew[i + j * p] - M[i] * M[j] / gain;
	} else {
	    for (int i = 0; i < p; i++) {
		a[i] = anew[i];
		Mt[l + n * i] = 0.0;
	    }
	    for (int i = 0; i < p * p; i++)
		P[i] = Pnew[i];
	    gains[l] = NA_REAL;
	    resids[l] = NA_REAL;
	}
    }

    /* rt stores r_{t-1} */
    rt = (double *) R_alloc(n * p, sizeof(double));
    for (int l = n - 1; l >= 0; l--) {
	if (!ISNAN(gains[l])) {
	    gn = 1/gains[l];
	    for (int i = 0; i < p; i++)
		rt[l + n * i] = Z[i] * resids[l] * gn;
	} else {
	    for (int i = 0; i < p; i++) rt[l + n * i] = 0.0;
	    gn = 0.0;
	}

	if (var) {
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++)
		    Nt[l + n*i + n*p*j] = Z[i] * Z[j] * gn;
	}

	if (l < n - 1) {
	    /* compute r_{t-1} */
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++)
		    mm[i + p * j] = ((i==j) ? 1:0) - Mt[l + n * i] * Z[j] * gn;
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = 0.0;
		    for (int k = 0; k < p; k++)
			tmp += T[i + p * k] * mm[k + p * j];
		    L[i + p * j] = tmp;
		}
	    for (int i = 0; i < p; i++) {
		double tmp = 0.0;
		for (int j = 0; j < p; j++)
		    tmp += L[j + p * i] * rt[l + 1 + n * j];
		rt[l + n * i] += tmp;
	    }
	    if(var) { /* compute N_{t-1} */
		for (int i = 0; i < p; i++)
		    for (int j = 0; j < p; j++) {
			double tmp = 0.0;
			for (int k = 0; k < p; k++)
			    tmp += L[k + p * i] * Nt[l + 1 + n*k + n*p*j];
			mm[i + p * j] = tmp;
		    }
		for (int i = 0; i < p; i++)
		    for (int j = 0; j < p; j++) {
			double tmp = 0.0;
			for (int k = 0; k < p; k++)
			    tmp += mm[i + p * k] * L[k + p * j];
			Nt[l + n*i + n*p*j] += tmp;
		    }
	    }
	}

	for (int i = 0; i < p; i++) {
	    double tmp = 0.0;
	    for (int j = 0; j < p; j++)
		tmp += Pt[l + n*i + n*p*j] * rt[l + n * j];
	    at[l + n*i] += tmp;
	}
    }
    if (var)
	for (int l = 0; l < n; l++) {
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = 0.0;
		    for (int k = 0; k < p; k++)
			tmp += Pt[l + n*i + n*p*k] * Nt[l + n*k + n*p*j];
		    mm[i + p * j] = tmp;
		}
	    for (int i = 0; i < p; i++)
		for (int j = 0; j < p; j++) {
		    double tmp = Pt[l + n*i + n*p*j];
		    for (int k = 0; k < p; k++)
			tmp -= mm[i + p * k] * Pt[l + n*k + n*p*j];
		    Nt[l + n*i + n*p*j] = tmp;
		}
	}
    UNPROTECT(4);
    return res;
}


SEXP
KalmanFore(SEXP nahead, SEXP mod, SEXP update)
{
    mod = PROTECT(duplicate(mod));
    SEXP sZ = getListElement(mod, "Z"), sa = getListElement(mod, "a"), 
	sP = getListElement(mod, "P"), sT = getListElement(mod, "T"), 
	sV = getListElement(mod, "V"), sh = getListElement(mod, "h");

    if (TYPEOF(sZ) != REALSXP ||
	TYPEOF(sa) != REALSXP || TYPEOF(sP) != REALSXP ||
	TYPEOF(sT) != REALSXP || TYPEOF(sV) != REALSXP)
	error(_("invalid argument type"));

    int  n = asInteger(nahead), p = LENGTH(sa);
    double *Z = REAL(sZ), *a = REAL(sa), *P = REAL(sP), *T = REAL(sT),
	*V = REAL(sV), h = asReal(sh);
    double *mm, *anew, *Pnew;

    anew = (double *) R_alloc(p, sizeof(double));
    Pnew = (double *) R_alloc(p * p, sizeof(double));
    mm = (double *) R_alloc(p * p, sizeof(double));
    SEXP res, forecasts, se;
    PROTECT(res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, forecasts = allocVector(REALSXP, n));
    SET_VECTOR_ELT(res, 1, se = allocVector(REALSXP, n));
    {
	SEXP nm = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 0, mkChar("pred"));
	SET_STRING_ELT(nm, 1, mkChar("var"));
	setAttrib(res, R_NamesSymbol, nm);
	UNPROTECT(1);
    }
    for (int l = 0; l < n; l++) {
	double fc = 0.0;
	for (int i = 0; i < p; i++) {
	    double tmp = 0.0;
	    for (int k = 0; k < p; k++)
		tmp += T[i + p * k] * a[k];
	    anew[i] = tmp;
	    fc += tmp * Z[i];
	}
	for (int i = 0; i < p; i++)
	    a[i] = anew[i];
	REAL(forecasts)[l] = fc;

	for (int i = 0; i < p; i++)
	    for (int j = 0; j < p; j++) {
		double tmp = 0.0;
		for (int k = 0; k < p; k++)
		    tmp += T[i + p * k] * P[k + p * j];
		mm[i + p * j] = tmp;
	    }
	for (int i = 0; i < p; i++)
	    for (int j = 0; j < p; j++) {
		double tmp = V[i + p * j];
		for (int k = 0; k < p; k++)
		    tmp += mm[i + p * k] * T[j + p * k];
		Pnew[i + p * j] = tmp;
	    }
	double tmp = h;
	for (int i = 0; i < p; i++)
	    for (int j = 0; j < p; j++) {
		P[i + j * p] = Pnew[i + j * p];
		tmp += Z[i] * Z[j] * P[i + j * p];
	    }
	REAL(se)[l] = tmp;
    }
    if(asLogical(update)) setAttrib(res, install("mod"), mod);
    UNPROTECT(2);
    return res;
}


static void partrans(int p, double *raw, double *new)
{
    int j, k;
    double a, work[100];

    if(p > 100) error(_("can only transform 100 pars in arima0"));

    /* Step one: map (-Inf, Inf) to (-1, 1) via tanh
       The parameters are now the pacf phi_{kk} */
    for(j = 0; j < p; j++) work[j] = new[j] = tanh(raw[j]);
    /* Step two: run the Durbin-Levinson recursions to find phi_{j.},
       j = 2, ..., p and phi_{p.} are the autoregression coefficients */
    for(j = 1; j < p; j++) {
	a = new[j];
	for(k = 0; k < j; k++)
	    work[k] -= a * new[j - k - 1];
	for(k = 0; k < j; k++) new[k] = work[k];
    }
}

SEXP ARIMA_undoPars(SEXP sin, SEXP sarma)
{
    int *arma = INTEGER(sarma), mp = arma[0], mq = arma[1], msp = arma[2],
	v, n = LENGTH(sin);
    double *params, *in = REAL(sin);
    SEXP res = allocVector(REALSXP, n);

    params = REAL(res);
    for (int i = 0; i < n; i++) params[i] = in[i];
    if (mp > 0) partrans(mp, in, params);
    v = mp + mq;
    if (msp > 0) partrans(msp, in + v, params + v);
    return res;
}


SEXP ARIMA_transPars(SEXP sin, SEXP sarma, SEXP strans)
{
    int *arma = INTEGER(sarma), trans = asLogical(strans);
    int mp = arma[0], mq = arma[1], msp = arma[2], msq = arma[3],
	ns = arma[4], i, j, p = mp + ns * msp, q = mq + ns * msq, v;
    double *in = REAL(sin), *params = REAL(sin), *phi, *theta;
    SEXP res, sPhi, sTheta;

    PROTECT(res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, sPhi = allocVector(REALSXP, p));
    SET_VECTOR_ELT(res, 1, sTheta = allocVector(REALSXP, q));
    phi = REAL(sPhi);
    theta = REAL(sTheta);
    if (trans) {
	int n = mp + mq + msp + msq;

	params = (double *) R_alloc(n, sizeof(double));
	for (i = 0; i < n; i++) params[i] = in[i];
	if (mp > 0) partrans(mp, in, params);
	v = mp + mq;
	if (msp > 0) partrans(msp, in + v, params + v);
    }
    if (ns > 0) {
	/* expand out seasonal ARMA models */
	for (i = 0; i < mp; i++) phi[i] = params[i];
	for (i = 0; i < mq; i++) theta[i] = params[i + mp];
	for (i = mp; i < p; i++) phi[i] = 0.0;
	for (i = mq; i < q; i++) theta[i] = 0.0;
	for (j = 0; j < msp; j++) {
	    phi[(j + 1) * ns - 1] += params[j + mp + mq];
	    for (i = 0; i < mp; i++)
		phi[(j + 1) * ns + i] -= params[i] * params[j + mp + mq];
	}
	for (j = 0; j < msq; j++) {
	    theta[(j + 1) * ns - 1] += params[j + mp + mq + msp];
	    for (i = 0; i < mq; i++)
		theta[(j + 1) * ns + i] += params[i + mp] *
		    params[j + mp + mq + msp];
	}
    } else {
	for (i = 0; i < mp; i++) phi[i] = params[i];
	for (i = 0; i < mq; i++) theta[i] = params[i + mp];
    }
    UNPROTECT(1);
    return res;
}

#if !defined(atanh) && defined(HAVE_DECL_ATANH) && !HAVE_DECL_ATANH
extern double atanh(double x);
#endif
static void invpartrans(int p, double *phi, double *new)
{
    int j, k;
    double a, work[100];

    if(p > 100) error(_("can only transform 100 pars in arima0"));

    for(j = 0; j < p; j++) work[j] = new[j] = phi[j];
    /* Run the Durbin-Levinson recursions backwards
       to find the PACF phi_{j.} from the autoregression coefficients */
    for(j = p - 1; j > 0; j--) {
	a = new[j];
	for(k = 0; k < j; k++)
	    work[k]  = (new[k] + a * new[j - k - 1]) / (1 - a * a);
	for(k = 0; k < j; k++) new[k] = work[k];
    }
    for(j = 0; j < p; j++) new[j] = atanh(new[j]);
}

SEXP ARIMA_Invtrans(SEXP in, SEXP sarma)
{
    int *arma = INTEGER(sarma), mp = arma[0], mq = arma[1], msp = arma[2],
	i, v, n = LENGTH(in);
    SEXP y = allocVector(REALSXP, n);
    double *raw = REAL(in), *new = REAL(y);

    for(i = 0; i < n; i++) new[i] = raw[i];
    if (mp > 0) invpartrans(mp, raw, new);
    v = mp + mq;
    if (msp > 0) invpartrans(msp, raw + v, new + v);
    return y;
}

#define eps 1e-3
SEXP ARIMA_Gradtrans(SEXP in, SEXP sarma)
{
    int *arma = INTEGER(sarma), mp = arma[0], mq = arma[1], msp = arma[2],
	n = LENGTH(in);
    SEXP y = allocMatrix(REALSXP, n, n);
    double *raw = REAL(in), *A = REAL(y), w1[100], w2[100], w3[100];

    for (int i = 0; i < n; i++)
	for (int j = 0; j < n; j++)
	    A[i + j*n] = (i == j);
    if(mp > 0) {
	for (int i = 0; i < mp; i++) w1[i] = raw[i];
	partrans(mp, w1, w2);
	for (int i = 0; i < mp; i++) {
	    w1[i] += eps;
	    partrans(mp, w1, w3);
	    for (int j = 0; j < mp; j++) A[i + j*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    if(msp > 0) {
	int v = mp + mq;
	for (int i = 0; i < msp; i++) w1[i] = raw[i + v];
	partrans(msp, w1, w2);
	for(int i = 0; i < msp; i++) {
	    w1[i] += eps;
	    partrans(msp, w1, w3);
	    for(int j = 0; j < msp; j++)
		A[i + v + (j+v)*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    return y;
}


SEXP
ARIMA_Like(SEXP sy, SEXP mod, SEXP sUP, SEXP giveResid)
{
    SEXP sPhi = getListElement(mod, "phi"), 
	sTheta = getListElement(mod, "theta"), 
	sDelta = getListElement(mod, "Delta"),
	sa = getListElement(mod, "a"),
	sP = getListElement(mod, "P"),
	sPn = getListElement(mod, "Pn");

    if (TYPEOF(sPhi) != REALSXP || TYPEOF(sTheta) != REALSXP ||
	TYPEOF(sDelta) != REALSXP || TYPEOF(sa) != REALSXP ||
	TYPEOF(sP) != REALSXP || TYPEOF(sPn) != REALSXP)
	error(_("invalid argument type"));

    SEXP res, nres, sResid = R_NilValue;
    int n = LENGTH(sy), rd = LENGTH(sa), p = LENGTH(sPhi),
	q = LENGTH(sTheta), d = LENGTH(sDelta), r = rd - d;
    double *y = REAL(sy), *a = REAL(sa), *P = REAL(sP), *Pnew = REAL(sPn);
    double *phi = REAL(sPhi), *theta = REAL(sTheta), *delta = REAL(sDelta);
    double sumlog = 0.0, ssq = 0, *anew, *mm = NULL, *M;
    int nu = 0;
    Rboolean useResid = asLogical(giveResid);
    double *rsResid = NULL /* -Wall */;

    anew = (double *) R_alloc(rd, sizeof(double));
    M = (double *) R_alloc(rd, sizeof(double));
    if (d > 0) mm = (double *) R_alloc(rd * rd, sizeof(double));

    if (useResid) {
	PROTECT(sResid = allocVector(REALSXP, n));
	rsResid = REAL(sResid);
    }

    for (int l = 0; l < n; l++) {
	for (int i = 0; i < r; i++) {
	    double tmp = (i < r - 1) ? a[i + 1] : 0.0;
	    if (i < p) tmp += phi[i] * a[0];
	    anew[i] = tmp;
	}
	if (d > 0) {
	    for (int i = r + 1; i < rd; i++) anew[i] = a[i - 1];
	    double tmp = a[0];
	    for (int i = 0; i < d; i++) tmp += delta[i] * a[r + i];
	    anew[r] = tmp;
	}
	if (l > asInteger(sUP)) {
	    if (d == 0) {
		for (int i = 0; i < r; i++) {
		    double vi = 0.0;
		    if (i == 0) vi = 1.0; else if (i - 1 < q) vi = theta[i - 1];
		    for (int j = 0; j < r; j++) {
			double tmp = 0.0;
			if (j == 0) tmp = vi; else if (j - 1 < q) tmp = vi * theta[j - 1];
			if (i < p && j < p) tmp += phi[i] * phi[j] * P[0];
			if (i < r - 1 && j < r - 1) tmp += P[i + 1 + r * (j + 1)];
			if (i < p && j < r - 1) tmp += phi[i] * P[j + 1];
			if (j < p && i < r - 1) tmp += phi[j] * P[i + 1];
			Pnew[i + r * j] = tmp;
		    }
		}
	    } else {
		/* mm = TP */
		for (int i = 0; i < r; i++)
		    for (int j = 0; j < rd; j++) {
			double tmp = 0.0;
			if (i < p) tmp += phi[i] * P[rd * j];
			if (i < r - 1) tmp += P[i + 1 + rd * j];
			mm[i + rd * j] = tmp;
		    }
		for (int j = 0; j < rd; j++) {
		    double tmp = P[rd * j];
		    for (int k = 0; k < d; k++)
			tmp += delta[k] * P[r + k + rd * j];
		    mm[r + rd * j] = tmp;
		}
		for (int i = 1; i < d; i++)
		    for (int j = 0; j < rd; j++)
			mm[r + i + rd * j] = P[r + i - 1 + rd * j];

		/* Pnew = mmT' */
		for (int i = 0; i < r; i++)
		    for (int j = 0; j < rd; j++) {
			double tmp = 0.0;
			if (i < p) tmp += phi[i] * mm[j];
			if (i < r - 1) tmp += mm[rd * (i + 1) + j];
			Pnew[j + rd * i] = tmp;
		    }
		for (int j = 0; j < rd; j++) {
		    double tmp = mm[j];
		    for (int k = 0; k < d; k++)
			tmp += delta[k] * mm[rd * (r + k) + j];
		    Pnew[rd * r + j] = tmp;
		}
		for (int i = 1; i < d; i++)
		    for (int j = 0; j < rd; j++)
			Pnew[rd * (r + i) + j] = mm[rd * (r + i - 1) + j];
		/* Pnew <- Pnew + (1 theta) %o% (1 theta) */
		for (int i = 0; i <= q; i++) {
		    double vi = (i == 0) ? 1. : theta[i - 1];
		    for (int j = 0; j <= q; j++)
			Pnew[i + rd * j] += vi * ((j == 0) ? 1. : theta[j - 1]);
		}
	    }
	}
	if (!ISNAN(y[l])) {
	    double resid = y[l] - anew[0];
	    for (int i = 0; i < d; i++)
		resid -= delta[i] * anew[r + i];

	    for (int i = 0; i < rd; i++) {
		double tmp = Pnew[i];
		for (int j = 0; j < d; j++)
		    tmp += Pnew[i + (r + j) * rd] * delta[j];
		M[i] = tmp;
	    }

	    double gain = M[0];
	    for (int j = 0; j < d; j++) gain += delta[j] * M[r + j];
	    if(gain < 1e4) {
		nu++;
		ssq += resid * resid / gain;
		sumlog += log(gain);
	    }
	    if (useResid) rsResid[l] = resid / sqrt(gain);
	    for (int i = 0; i < rd; i++)
		a[i] = anew[i] + M[i] * resid / gain;
	    for (int i = 0; i < rd; i++)
		for (int j = 0; j < rd; j++)
		    P[i + j * rd] = Pnew[i + j * rd] - M[i] * M[j] / gain;
	} else {
	    for (int i = 0; i < rd; i++) a[i] = anew[i];
	    for (int i = 0; i < rd * rd; i++) P[i] = Pnew[i];
	    if (useResid) rsResid[l] = NA_REAL;
	}
    }

    if (useResid) {
	PROTECT(res = allocVector(VECSXP, 3));
	SET_VECTOR_ELT(res, 0, nres = allocVector(REALSXP, 3));
	REAL(nres)[0] = ssq;
	REAL(nres)[1] = sumlog;
	REAL(nres)[2] = (double) nu;
	SET_VECTOR_ELT(res, 1, sResid);
	UNPROTECT(2);
	return res;
    } else {
	nres = allocVector(REALSXP, 3);
	REAL(nres)[0] = ssq;
	REAL(nres)[1] = sumlog;
	REAL(nres)[2] = (double) nu;
	return nres;
    }
}

/* do differencing here */
/* arma is p, q, sp, sq, ns, d, sd */
SEXP
ARIMA_CSS(SEXP sy, SEXP sarma, SEXP sPhi, SEXP sTheta,
	  SEXP sncond, SEXP giveResid)
{
    SEXP res, sResid = R_NilValue;
    double ssq = 0.0, *y = REAL(sy), tmp;
    double *phi = REAL(sPhi), *theta = REAL(sTheta), *w, *resid;
    int n = LENGTH(sy), *arma = INTEGER(sarma), p = LENGTH(sPhi),
	q = LENGTH(sTheta), ncond = asInteger(sncond);
    int ns, nu = 0;
    Rboolean useResid = asLogical(giveResid);

    w = (double *) R_alloc(n, sizeof(double));
    for (int l = 0; l < n; l++) w[l] = y[l];
    for (int i = 0; i < arma[5]; i++)
	for (int l = n - 1; l > 0; l--) w[l] -= w[l - 1];
    ns = arma[4];
    for (int i = 0; i < arma[6]; i++)
	for (int l = n - 1; l >= ns; l--) w[l] -= w[l - ns];

    PROTECT(sResid = allocVector(REALSXP, n));
    resid = REAL(sResid);
    if (useResid) for (int l = 0; l < ncond; l++) resid[l] = 0;

    for (int l = ncond; l < n; l++) {
	tmp = w[l];
	for (int j = 0; j < p; j++) tmp -= phi[j] * w[l - j - 1];
	for (int j = 0; j < min(l - ncond, q); j++)
	    tmp -= theta[j] * resid[l - j - 1];
	resid[l] = tmp;
	if (!ISNAN(tmp)) {
	    nu++;
	    ssq += tmp * tmp;
	}
    }
    if (useResid) {
	PROTECT(res = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(res, 0, ScalarReal(ssq / (double) (nu)));
	SET_VECTOR_ELT(res, 1, sResid);
	UNPROTECT(2);
	return res;
    } else {
	UNPROTECT(1);
	return ScalarReal(ssq / (double) (nu));
    }
}

SEXP TSconv(SEXP a, SEXP b)
{
    int na, nb, nab;
    SEXP ab;
    double *ra, *rb, *rab;

    PROTECT(a = coerceVector(a, REALSXP));
    PROTECT(b = coerceVector(b, REALSXP));
    na = LENGTH(a);
    nb = LENGTH(b);
    nab = na + nb - 1;
    PROTECT(ab = allocVector(REALSXP, nab));
    ra = REAL(a); rb = REAL(b); rab = REAL(ab);
    for (int i = 0; i < nab; i++) rab[i] = 0.0;
    for (int i = 0; i < na; i++)
	for (int j = 0; j < nb; j++)
	    rab[i + j] += ra[i] * rb[j];
    UNPROTECT(3);
    return (ab);
}

/* based on code from AS154 */

static void
inclu2(size_t np, double *xnext, double *xrow, double ynext,
       double *d, double *rbar, double *thetab)
{
    double cbar, sbar, di, xi, xk, rbthis, dpi;
    size_t i, k, ithisr;

/*   This subroutine updates d, rbar, thetab by the inclusion
     of xnext and ynext. */

    for (i = 0; i < np; i++) xrow[i] = xnext[i];

    for (ithisr = 0, i = 0; i < np; i++) {
	if (xrow[i] != 0.0) {
	    xi = xrow[i];
	    di = d[i];
	    dpi = di + xi * xi;
	    d[i] = dpi;
	    cbar = di / dpi;
	    sbar = xi / dpi;
	    for (k = i + 1; k < np; k++) {
		xk = xrow[k];
		rbthis = rbar[ithisr];
		xrow[k] = xk - xi * rbthis;
		rbar[ithisr++] = cbar * rbthis + sbar * xk;
	    }
	    xk = ynext;
	    ynext = xk - xi * thetab[i];
	    thetab[i] = cbar * thetab[i] + sbar * xk;
	    if (di == 0.0) return;
	} else
	    ithisr = ithisr + np - i - 1;
    }
}

#ifdef DEBUG_Q0bis
# include <R_ext/Print.h>
  double chk_V(double v[], char* nm, int jj, int len) { 
    // len = length(<vector>)  <==> index must be in  {0, len-1}
    if(jj < 0 || jj >= len)
	REprintf(" %s[%2d]\n", nm, jj); 
    return(v[jj]); 
  }
#endif

/*
  Matwey V. Kornilov's implementation of algorithm by
  Dr. Raphael Rossignol
  See https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14682 for details.
*/
SEXP getQ0bis(SEXP sPhi, SEXP sTheta, SEXP sTol)
{
    SEXP res;
    int p = LENGTH(sPhi), q = LENGTH(sTheta);
    double *phi = REAL(sPhi), *theta = REAL(sTheta); // tol = REAL(sTol)[0];

    int i,j, r = max(p, q + 1);

    /* Final result is block product 
     *   Q0 = A1 SX A1^T + A1 SXZ A2^T + (A1 SXZ A2^T)^T + A2 A2^T ,
     * where A1 [i,j] = phi[i+j],
     *       A2 [i,j] = ttheta[i+j],  and SX, SXZ are defined below */
    PROTECT(res = allocMatrix(REALSXP, r, r));
    double *P = REAL(res);

    /* Clean P */
    Memzero(P, r*r);

#ifdef DEBUG_Q0bis
#define _ttheta(j) chk_V(ttheta, "ttheta", j, q+1)// was  r
#define _tphi(j)   chk_V(tphi,   "tphi",   j, p+1)
#define _rrz(j)    chk_V(rrz,    "rrz",    j, q)
#else
#define _ttheta(j) ttheta[j]
#define _tphi(j) tphi[j]
#define _rrz(j)  rrz [j]
#endif

    double *ttheta = (double *) R_alloc(q + 1, sizeof(double));
    /* Init ttheta = c(1, theta) */
    ttheta[0] = 1.;
    for (i = 1; i < q + 1; ++i) ttheta[i] = theta[i - 1];

    if( p > 0 ) {
	int r2 = max(p + q, p + 1);
	SEXP sgam = PROTECT(allocMatrix(REALSXP, r2, r2)),
	    sg = PROTECT(allocVector(REALSXP, r2));
	double *gam = REAL(sgam);
	double *g = REAL(sg);
	double *tphi = (double *) R_alloc(p + 1, sizeof(double));
	/* Init tphi = c(1, -phi) */
	tphi[0] = 1.;
	for (i = 1; i < p + 1; ++i) tphi[i] = -phi[i - 1];

    /* Compute the autocovariance function of U, the AR part of X */

    /* Gam := C1 + C2 ; initialize */
	Memzero(gam, r2*r2);

    /* C1[E] */
	for (j = 0; j < r2; ++j)
	    for (i = j; i < r2 && i - j < p + 1; ++i)
		gam[j*r2 + i] += _tphi(i-j);

    /* C2[E] */
	for (i = 0; i < r2; ++i)
	    for (j = 1; j < r2 && i + j < p + 1; ++j)
		gam[j*r2 + i] += _tphi(i+j);

    /* Initialize g = (1 0 0 .... 0) */
	g[0] = 1.;
	for (i = 1; i < r2; ++i)
	    g[i] = 0.;

    /* rU = solve(Gam, g)  -> solve.default() -> .Internal(La_solve, .,)
     * --> fiddling with R-objects -> C and then F77_CALL(.) of dgesv, dlange, dgecon
     * FIXME: call these directly here, possibly even use 'info' instead of error(.)
     * e.g., in case of exact singularity.
     */
	SEXP callS = PROTECT(lang4(install("solve.default"), sgam, sg, sTol)),
	    su = PROTECT(eval(callS, R_BaseEnv));
	double *u = REAL(su);
    /* SX = A SU A^T */
    /* A[i,j]  = ttheta[j-i] */
    /* SU[i,j] = u[abs(i-j)] */
    /* Q0 += ( A1 SX A1^T == A1 A SU A^T A1^T) */
	// (relying on good compiler optimization here:)
	for (i = 0; i < r; ++i)
	    for (j = i; j < r; ++j)
		for (int k = 0; i + k < p; ++k)
		    for (int L = k; L - k < q + 1; ++L)
			for (int m = 0; j + m < p; ++m)
			    for (int n = m; n - m < q + 1; ++n)
				P[r*i + j] += phi[i + k] * phi[j + m] *
				    _ttheta(L - k) * _ttheta(n - m) * u[abs(L - n)];
	UNPROTECT(4);
    /* Compute correlation matrix between X and Z */
    /* forwardsolve(C1, g) */
    /* C[i,j] = tphi[i-j] */
    /* g[i] = _ttheta(i) */
	double *rrz = (double *) R_alloc(q, sizeof(double));
	if(q > 0) {
	    for (i = 0; i < q; ++i) {
		rrz[i] = _ttheta(i);
		for (j = max(0, i - p); j < i; ++j)
		    rrz[i] -= _rrz(j) * _tphi(i-j);
	    }
	}

    /* Q0 += A1 SXZ A2^T + (A1 SXZ A2^T)^T */
    /* SXZ[i,j] = rrz[j-i-1], j > 0 */
	for (i = 0; i < r; ++i)
	    for (j = i; j < r; ++j) {
		int k, L;
		for (k = 0; i + k < p; ++k)
		    for (L = k+1; j + L < q + 1; ++L)
			P[r*i + j] += phi[i + k] * _ttheta(j + L) * _rrz(L - k - 1);
		for (k = 0; j + k < p; ++k)
		    for (L = k+1; i + L < q + 1; ++L)
			P[r*i + j] += phi[j + k] * _ttheta(i + L) * _rrz(L - k - 1);
	    }
    } // end if(p > 0)

    /* Q0 += A2 A2^T */
    for (i = 0; i < r; ++i)
	for (j = i; j < r; ++j)
	    for (int k = 0; j + k < q + 1; ++k)
		 P[r*i + j] += _ttheta(i + k) * _ttheta(j + k);

    /* Symmetrize result */
    for (i = 0; i < r; ++i)
	for (j = i+1; j < r; ++j)
	    P[r*j + i] = P[r*i + j];

    UNPROTECT(1);
    return res;
}

SEXP getQ0(SEXP sPhi, SEXP sTheta)
{
    SEXP res;
    int  p = LENGTH(sPhi), q = LENGTH(sTheta);
    double *phi = REAL(sPhi), *theta = REAL(sTheta);

    /* thetab[np], xnext[np], xrow[np].  rbar[rbar] */
    /* NB: nrbar could overflow */
    int r = max(p, q + 1);
    size_t np = r * (r + 1) / 2, nrbar = np * (np - 1) / 2, npr, npr1;
    size_t indi, indj, indn, i, j, ithisr, ind, ind1, ind2, im, jm;


    /* This is the limit using an int index.  We could use
       size_t and get more on a 64-bit system,
       but there seems no practical need. */
    if(r > 350) error(_("maximum supported lag is 350"));
    double *xnext, *xrow, *rbar, *thetab, *V;
    xnext = (double *) R_alloc(np, sizeof(double));
    xrow = (double *) R_alloc(np, sizeof(double));
    rbar = (double *) R_alloc(nrbar, sizeof(double));
    thetab = (double *) R_alloc(np, sizeof(double));
    V = (double *) R_alloc(np, sizeof(double));
    for (ind = 0, j = 0; j < r; j++) {
	double vj = 0.0;
	if (j == 0) vj = 1.0; else if (j - 1 < q) vj = theta[j - 1];
	for (i = j; i < r; i++) {
	    double vi = 0.0;
	    if (i == 0) vi = 1.0; else if (i - 1 < q) vi = theta[i - 1];
	    V[ind++] = vi * vj;
	}
    }

    PROTECT(res = allocMatrix(REALSXP, r, r));
    double *P = REAL(res);

    if (r == 1) {
	if (p == 0) P[0] = 1.0; // PR#16419
	else P[0] = 1.0 / (1.0 - phi[0] * phi[0]);
	UNPROTECT(1);
	return res;
    }
    if (p > 0) {
/*      The set of equations s * vec(P0) = vec(v) is solved for
	vec(P0).  s is generated row by row in the array xnext.  The
	order of elements in P is changed, so as to bring more leading
	zeros into the rows of s. */

	for (i = 0; i < nrbar; i++) rbar[i] = 0.0;
	for (i = 0; i < np; i++) {
	    P[i] = 0.0;
	    thetab[i] = 0.0;
	    xnext[i] = 0.0;
	}
	ind = 0;
	ind1 = -1;
	npr = np - r;
	npr1 = npr + 1;
	indj = npr;
	ind2 = npr - 1;
	for (j = 0; j < r; j++) {
	    double phij = (j < p) ? phi[j] : 0.0;
	    xnext[indj++] = 0.0;
	    indi = npr1 + j;
	    for (i = j; i < r; i++) {
		double ynext = V[ind++];
		double phii = (i < p) ? phi[i] : 0.0;
		if (j != r - 1) {
		    xnext[indj] = -phii;
		    if (i != r - 1) {
			xnext[indi] -= phij;
			xnext[++ind1] = -1.0;
		    }
		}
		xnext[npr] = -phii * phij;
		if (++ind2 >= np) ind2 = 0;
		xnext[ind2] += 1.0;
		inclu2(np, xnext, xrow, ynext, P, rbar, thetab);
		xnext[ind2] = 0.0;
		if (i != r - 1) {
		    xnext[indi++] = 0.0;
		    xnext[ind1] = 0.0;
		}
	    }
	}

	ithisr = nrbar - 1;
	im = np - 1;
	for (i = 0; i < np; i++) {
	    double bi = thetab[im];
	    for (jm = np - 1, j = 0; j < i; j++)
		bi -= rbar[ithisr--] * P[jm--];
	    P[im--] = bi;
	}

/*        now re-order p. */

	ind = npr;
	for (i = 0; i < r; i++) xnext[i] = P[ind++];
	ind = np - 1;
	ind1 = npr - 1;
	for (i = 0; i < npr; i++) P[ind--] = P[ind1--];
	for (i = 0; i < r; i++) P[i] = xnext[i];
    } else {

/* P0 is obtained by backsubstitution for a moving average process. */

	indn = np;
	ind = np;
	for (i = 0; i < r; i++)
	    for (j = 0; j <= i; j++) {
		--ind;
		P[ind] = V[ind];
		if (j != 0) P[ind] += P[--indn];
	    }
    }
    /* now unpack to a full matrix */
    for (i = r - 1, ind = np; i > 0; i--)
	for (j = r - 1; j >= i; j--)
	    P[r * i + j] = P[--ind];
    for (i = 0; i < r - 1; i++)
	for (j = i + 1; j < r; j++)
	    P[i + r * j] = P[j + r * i];
    UNPROTECT(1);
    return res;
}
