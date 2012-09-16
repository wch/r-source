/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 1999-2012	The R Core Team
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
 *  http://www.r-project.org/Licenses/.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if !defined(atanh) && defined(HAVE_DECL_ATANH) && !HAVE_DECL_ATANH
extern double atanh(double x);
#endif

/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include <R.h>
#include "ts.h"

#ifndef max
#define max(a,b) ((a < b)?(b):(a))
#endif
#ifndef min
#define min(a,b) ((a > b)?(b):(a))
#endif


/* Internal */
static void partrans(int np, double *raw, double *new);
static void dotrans(Starma G, double *raw, double *new, int trans);


/* cor is the autocorrelations starting from 0 lag*/
static void uni_pacf(double *cor, double *p, int nlag)
{
    double a, b, c, *v, *w;

    v = (double*) R_alloc(nlag, sizeof(double));
    w = (double*) R_alloc(nlag, sizeof(double));
    w[0] = p[0] = cor[1];
    for(int ll = 1; ll < nlag; ll++) {
	a = cor[ll+1];
	b = 1.0;
	for(int i = 0; i < ll; i++) {
	    a -= w[i] * cor[ll - i];
	    b -= w[i] * cor[i + 1];
	}
	p[ll] = c = a/b;
	if(ll+1 == nlag) break;
	w[ll] = c;
	for(int i = 0; i < ll; i++)
	    v[ll-i-1] = w[i];
	for(int i = 0; i < ll; i++)
	    w[i] -= c*v[i];
    }
}

SEXP pacf1(SEXP acf, SEXP lmax)
{
    int lagmax = asInteger(lmax);
    acf = PROTECT(coerceVector(acf, REALSXP));
    SEXP ans = PROTECT(allocVector(REALSXP, lagmax));
    uni_pacf(REAL(acf), REAL(ans), lagmax);
    SEXP d = PROTECT(allocVector(INTSXP, 3));
    INTEGER(d)[0] = lagmax;
    INTEGER(d)[1] = INTEGER(d)[2] = 1;
    setAttrib(ans, R_DimSymbol, d);
    UNPROTECT(3);
    return ans;
}


/* Use an external reference to store the structure we keep allocated
   memory in */
static SEXP Starma_tag;

#define GET_STARMA \
    Starma G; \
    if (TYPEOF(pG) != EXTPTRSXP || R_ExternalPtrTag(pG) != Starma_tag) \
	error(_("bad Starma struct"));\
    G = (Starma) R_ExternalPtrAddr(pG)

SEXP setup_starma(SEXP na, SEXP x, SEXP pn, SEXP xreg, SEXP pm,
		  SEXP dt, SEXP ptrans, SEXP sncond)
{
    Starma G;
    int i, n, m, ip, iq, ir, np;
    SEXP res;
    double *rx = REAL(x), *rxreg = REAL(xreg);

    G = Calloc(1, starma_struct);
    G->mp = INTEGER(na)[0];
    G->mq = INTEGER(na)[1];
    G->msp = INTEGER(na)[2];
    G->msq = INTEGER(na)[3];
    G->ns = INTEGER(na)[4];
    G->n = n = asInteger(pn);
    G->ncond = asInteger(sncond);
    G->m = m = asInteger(pm);
    G->params = Calloc(G->mp + G->mq + G->msp + G->msq + G->m, double);
    G->p = ip = G->ns*G->msp + G->mp;
    G->q = iq = G->ns*G->msq + G->mq;
    G->r = ir = max(ip, iq + 1);
    G->np = np = (ir*(ir + 1))/2;
    G->nrbar = max(1, np*(np - 1)/2);
    G->trans = asInteger(ptrans);
    G->a = Calloc(ir, double);
    G->P = Calloc(np, double);
    G->V = Calloc(np, double);
    G->thetab = Calloc(np, double);
    G->xnext = Calloc(np, double);
    G->xrow = Calloc(np, double);
    G->rbar = Calloc(G->nrbar, double);
    G->w = Calloc(n, double);
    G->wkeep = Calloc(n, double);
    G->resid = Calloc(n, double);
    G->phi = Calloc(ir, double);
    G->theta = Calloc(ir, double);
    G->reg = Calloc(1 + n*m, double); /* AIX can't calloc 0 items */
    G->delta = asReal(dt);
    for(i = 0; i < n; i++) G->w[i] = G->wkeep[i] = rx[i];
    for(i = 0; i < n*m; i++) G->reg[i] = rxreg[i];
    Starma_tag = install("STARMA_TAG");
    res = R_MakeExternalPtr(G, Starma_tag, R_NilValue);
    return res;
}

SEXP free_starma(SEXP pG)
{
    GET_STARMA;

    Free(G->params); Free(G->a); Free(G->P); Free(G->V); Free(G->thetab);
    Free(G->xnext); Free(G->xrow); Free(G->rbar);
    Free(G->w); Free(G->wkeep); Free(G->resid); Free(G->phi); Free(G->theta);
    Free(G->reg); Free(G);
    return R_NilValue;
}

SEXP Starma_method(SEXP pG, SEXP method)
{
    GET_STARMA;

    G->method = asInteger(method);
    return R_NilValue;
}

SEXP Dotrans(SEXP pG, SEXP x)
{
    SEXP y = allocVector(REALSXP, LENGTH(x));
    GET_STARMA;

    dotrans(G, REAL(x), REAL(y), 1);
    return y;
}

SEXP set_trans(SEXP pG, SEXP ptrans)
{
    GET_STARMA;

    G->trans = asInteger(ptrans);
    return R_NilValue;
}

SEXP arma0fa(SEXP pG, SEXP inparams)
{
    int i, j, ifault = 0, it, streg;
    double sumlog, ssq, tmp, ans;

    GET_STARMA;
    dotrans(G, REAL(inparams), G->params, G->trans);

    if(G->ns > 0) {
	/* expand out seasonal ARMA models */
	for(i = 0; i < G->mp; i++) G->phi[i] = G->params[i];
	for(i = 0; i < G->mq; i++) G->theta[i] = G->params[i + G->mp];
	for(i = G->mp; i < G->p; i++) G->phi[i] = 0.0;
	for(i = G->mq; i < G->q; i++) G->theta[i] = 0.0;
	for(j = 0; j < G->msp; j++) {
	    G->phi[(j + 1)*G->ns - 1] += G->params[j + G->mp + G->mq];
	    for(i = 0; i < G->mp; i++)
		G->phi[(j + 1)*G->ns + i] -= G->params[i]*
		    G->params[j + G->mp + G->mq];
	}
	for(j = 0; j < G->msq; j++) {
	    G->theta[(j + 1)*G->ns - 1] +=
		G->params[j + G->mp + G->mq + G->msp];
	    for(i = 0; i < G->mq; i++)
		G->theta[(j + 1)*G->ns + i] += G->params[i + G->mp]*
		    G->params[j + G->mp + G->mq + G->msp];
	}
    } else {
	for(i = 0; i < G->mp; i++) G->phi[i] = G->params[i];
	for(i = 0; i < G->mq; i++) G->theta[i] = G->params[i + G->mp];
    }

    streg = G->mp + G->mq + G->msp + G->msq;
    if(G->m > 0) {
	for(i = 0; i < G->n; i++) {
	    tmp = G->wkeep[i];
	    for(j = 0; j < G->m; j++)
		tmp -= G->reg[i + G->n*j] * G->params[streg + j];
	    G->w[i] = tmp;
	}
    }

    if(G->method == 1) {
	int p = G->mp + G->ns * G->msp, q = G->mq + G->ns * G->msq, nu = 0;
	ssq = 0.0;
	for(i = 0; i < G->ncond; i++) G->resid[i] = 0.0;
	for(i = G->ncond; i < G->n; i++) {
	    tmp = G->w[i];
	    for(j = 0; j < min(i - G->ncond, p); j++)
		tmp -= G->phi[j] * G->w[i - j - 1];
	    for(j = 0; j < min(i - G->ncond, q); j++)
		tmp -= G->theta[j] * G->resid[i - j - 1];
	    G->resid[i] = tmp;
	    if(!ISNAN(tmp)) {
		nu++;
		ssq += tmp * tmp;
	    }
	}
	G->s2 = ssq/(double)(nu);
	ans = 0.5 * log(G->s2);
    } else {
	starma(G, &ifault);
	if(ifault) error(_("starma error code %d"), ifault);
	sumlog = 0.0;
	ssq = 0.0;
	it = 0;
	karma(G, &sumlog, &ssq, 1, &it);
	G->s2 = ssq/(double)G->nused;
	ans = 0.5*(log(ssq/(double)G->nused) + sumlog/(double)G->nused);
    }
    return ScalarReal(ans);
}

SEXP get_s2(SEXP pG)
{
    GET_STARMA;
    return ScalarReal(G->s2);
}

SEXP get_resid(SEXP pG)
{
    SEXP res;
    int i;
    double *rres;
    GET_STARMA;

    res = allocVector(REALSXP, G->n);
    rres = REAL(res);
    for(i = 0; i < G->n; i++) rres[i] = G->resid[i];
    return res;
}

SEXP arma0_kfore(SEXP pG, SEXP pd, SEXP psd, SEXP nahead)
{
    int dd = asInteger(pd);
    int d, il = asInteger(nahead), ifault = 0, i, j;
    double *del, *del2;
    SEXP res, x, var;
    GET_STARMA;

    PROTECT(res = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(res, 0, x = allocVector(REALSXP, il));
    SET_VECTOR_ELT(res, 1, var = allocVector(REALSXP, il));

    d = dd + G->ns * asInteger(psd);

    del = (double *) R_alloc(d + 1, sizeof(double));
    del2 = (double *) R_alloc(d + 1, sizeof(double));
    del[0] = 1;
    for(i = 1; i <= d; i++) del[i] = 0;
    for (j = 0; j < dd; j++) {
	for(i = 0; i <= d; i++) del2[i] = del[i];
	for(i = 0; i <= d - 1; i++) del[i+1] -= del2[i];
    }
    for (j = 0; j < asInteger(psd); j++) {
	for(i = 0; i <= d; i++) del2[i] = del[i];
	for(i = 0; i <= d - G->ns; i++) del[i + G->ns] -= del2[i];
    }
    for(i = 1; i <= d; i++) del[i] *= -1;


    forkal(G, d, il, del + 1, REAL(x), REAL(var), &ifault);
    if(ifault) error(_("forkal error code %d"), ifault);
    UNPROTECT(1);
    return res;
}

static void artoma(int p, double *phi, double *psi, int npsi)
{
    int i, j;

    for(i = 0; i < p; i++) psi[i] = phi[i];
    for(i = p; i < npsi; i++) psi[i] = 0.0;
    for(i = 0; i < npsi - p - 1; i++)
	for(j = 0; j < p; j++) psi[i + j + 1] += phi[j]*psi[i];
}

SEXP ar2ma(SEXP ar, SEXP npsi)
{
    ar = PROTECT(coerceVector(ar, REALSXP));
    int p = LENGTH(ar), ns = asInteger(npsi), ns1 = ns + p + 1;
    SEXP psi = PROTECT(allocVector(REALSXP, ns1));
    artoma(p, REAL(ar), REAL(psi), ns1);
    SEXP ans = lengthgets(psi, ns);
    UNPROTECT(2);
    return ans;
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

static void dotrans(Starma G, double *raw, double *new, int trans)
{
    int i, v, n = G->mp + G->mq + G->msp + G->msq + G->m;

    for(i = 0; i < n; i++) new[i] = raw[i];
    if(trans) {
	partrans(G->mp, raw, new);
	v = G->mp;
	partrans(G->mq, raw + v, new + v);
	v += G->mq;
	partrans(G->msp, raw + v, new + v);
	v += G->msp;
	partrans(G->msq, raw + v, new + v);
    }
}

#ifdef WIN32
extern double atanh(double);
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

SEXP Invtrans(SEXP pG, SEXP x)
{
    SEXP y = allocVector(REALSXP, LENGTH(x));
    int i, v, n;
    double *raw = REAL(x), *new = REAL(y);
    GET_STARMA;

    n = G->mp + G->mq + G->msp + G->msq;

    v = 0;
    invpartrans(G->mp, raw + v, new + v);
    v += G->mp;
    invpartrans(G->mq, raw + v, new + v);
    v += G->mq;
    invpartrans(G->msp, raw + v, new + v);
    v += G->msp;
    invpartrans(G->msq, raw + v, new + v);
    for(i = n; i < n + G->m; i++) new[i] = raw[i];
    return y;
}

#define eps 1e-3
SEXP Gradtrans(SEXP pG, SEXP x)
{
    SEXP y = allocMatrix(REALSXP, LENGTH(x), LENGTH(x));
    int i, j, v, n;
    double *raw = REAL(x), *A = REAL(y), w1[100], w2[100], w3[100];
    GET_STARMA;

    n = G->mp + G->mq + G->msp + G->msq + G->m;
    for(i = 0; i < n; i++)
	for(j = 0; j < n; j++)
	    A[i + j*n] = (i == j);
    if(G->mp > 0) {
	for(i = 0; i < G->mp; i++) w1[i] = raw[i];
	partrans(G->mp, w1, w2);
	for(i = 0; i < G->mp; i++) {
	    w1[i] += eps;
	    partrans(G->mp, w1, w3);
	    for(j = 0; j < G->mp; j++) A[i + j*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    if(G->mq > 0) {
	v = G->mp;
	for(i = 0; i < G->mq; i++) w1[i] = raw[i + v];
	partrans(G->mq, w1, w2);
	for(i = 0; i < G->mq; i++) {
	    w1[i] += eps;
	    partrans(G->mq, w1, w3);
	    for(j = 0; j < G->mq; j++) A[i + v + j*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    if(G->msp > 0) {
	v = G->mp + G->mq;
	for(i = 0; i < G->msp; i++) w1[i] = raw[i + v];
	partrans(G->msp, w1, w2);
	for(i = 0; i < G->msp; i++) {
	    w1[i] += eps;
	    partrans(G->msp, w1, w3);
	    for(j = 0; j < G->msp; j++)
		A[i + v + (j+v)*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    if(G->msq > 0) {
	v = G->mp + G->mq + G->msp;
	for(i = 0; i < G->msq; i++) w1[i] = raw[i + v];
	partrans(G->msq, w1, w2);
	for(i = 0; i < G->msq; i++) {
	    w1[i] += eps;
	    partrans(G->msq, w1, w3);
	    for(j = 0; j < G->msq; j++)
		A[i + v + (j+v)*n] = (w3[j] - w2[j])/eps;
	    w1[i] -= eps;
	}
    }
    return y;
}

SEXP
ARMAtoMA(SEXP ar, SEXP ma, SEXP lag_max)
{
    int i, j, p = LENGTH(ar), q = LENGTH(ma), m = asInteger(lag_max);
    double *phi = REAL(ar), *theta = REAL(ma), *psi, tmp;
    SEXP res;

    if(m <= 0 || m == NA_INTEGER)
	error(_("invalid value of lag.max"));
    PROTECT(res = allocVector(REALSXP, m));
    psi = REAL(res);
    for(i = 0; i < m; i++) {
	tmp = (i < q) ? theta[i] : 0.0;
	for(j = 0; j < min(i+1, p); j++)
	    tmp += phi[j] * ((i-j-1 >= 0) ? psi[i-j-1] : 1.0);
	psi[i] = tmp;
    }
    UNPROTECT(1);
    return res;
}
