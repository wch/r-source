/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1999        The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
 */

#include <R.h>

/* cor is the autocorrelations starting from 0 lag*/
void uni_pacf(double *cor, double *p, int *pnlag)
{
    int nlag = *pnlag;
    int i, ll;
    double a, b, c, *v, *w;

    v = (double*) R_alloc(nlag, sizeof(double));
    w = (double*) R_alloc(nlag, sizeof(double));
    w[0] = p[0] = cor[1];
    for(ll = 1; ll < nlag; ll++) {
	a = cor[ll+1];
	b = 1.0;
	for(i = 0; i < ll; i++) {
	    a -= w[i] * cor[ll - i];
	    b -= w[i] * cor[i + 1];
	}
	p[ll] = c = a/b;
	if(ll+1 == nlag) break;
	w[ll] = c;
	for(i = 0; i < ll; i++)
	    v[ll-i-1] = w[i];
	for(i = 0; i < ll; i++)
	    w[i] -= c*v[i];
    }
}
#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#endif

#ifndef max
#define max(a,b) ((a < b)?(b):(a))
#endif

static int ip, iq, mp, mq, msp, msq, ns, ir, np, nrbar, n, m, trans;
static double *a, *p, *v, *thetab, *xnext, *xrow, *rbar, *e,
    *w, *wkeep, delta, *resid, *phi, *theta, s2, *reg, *params;

void F77_NAME(starma)(int* ip, int* iq, int* ir, int* np, double* phi,
		      double* theta, double* a,
		      double* p, double* v, double* thetab, double* xnext,
		      double* xrow, double* rbar, int* nrbar, int* ifault);

void F77_NAME(karma)(int* ip, int* iq, int* ir, int* np, double* phi,
		     double* theta, double* a, double* p,
		     double* v, int *n, double*  w, double* resid,
		     double* sumlog, double* ssq, int* iupd,
		     double* delta, double* e, int* nit);

void F77_NAME(kalfor)(int *m, int* ip, int* ir, int* np, double* phi,
		      double *a, double *p, double *v, double *work,
		      double *x, double *var);

void F77_NAME(forkal)(int *ip, int *iq, int *ir, int *np, int *ird, 
		      int *irz, int *id, int *il, int *n, int *nrbar, 
		      double *phi, double *theta, double *delta, 
		      double *w, double *y, double *amse, double *a, 
		      double *p, double *v, double *resid, double *e, 
		      double *xnext, double *xrow, double *rbar, 
		      double *thetab, double *store, int *ifault);


void setup_starma(int *na, double *x, int *pn, double *xreg, int *pm,
		  double *dt, int *ptrans)
{
    int i;

    mp = na[0];
    mq = na[1];
    msp = na[2];
    msq = na[3];
    ns = na[4];
    n = *pn;
    m = *pm;
    params = Calloc(mp+mq+msp+msq+m, double);
    ip = ns*msp + mp;
    iq = ns*msq + mq;
    ir = max(ip, iq+1);
    np = (ir*(ir+1))/2;
    nrbar = max(1, np*(np-1)/2);
    trans = *ptrans;
    a = Calloc(ir, double);
    p = Calloc(np, double);
    v = Calloc(np, double);
    thetab = Calloc(np, double);
    xnext = Calloc(np, double);
    xrow = Calloc(np, double);
    rbar = Calloc(nrbar, double);
    e = Calloc(ir, double);
    w = Calloc(n, double);
    wkeep = Calloc(n, double);
    resid = Calloc(n, double);
    phi = Calloc(ir, double);
    theta = Calloc(ir, double);
    reg = Calloc(1+n*m, double); /* AIX can't calloc 0 items */
    delta = *dt;
    for(i = 0; i < n; i++) w[i] = wkeep[i] = x[i];
    for(i = 0; i < n*m; i++) reg[i] = xreg[i];
}

void free_starma()
{
    Free(params); Free(a); Free(p); Free(v); Free(thetab);
    Free(xnext); Free(xrow); Free(rbar); Free(e);
    Free(w); Free(wkeep); Free(resid); Free(phi); Free(theta); Free(reg);
}

void dotrans(double *, double *, int);

void Dotrans(double *x, double *y)
{
    dotrans(x, y, 1);
}

void arma0fa(double *inparams, double *res)
{
    int i, j, ifault, it, iupd, streg;
    double sumlog, ssq, tmp;

    /*for(i=0; i < mp+mq+msp+msq+m; i++)
      printf(" %f", inparams[i]); printf("\n");  */
    dotrans(inparams, params, trans);
    /*for(i=0; i < mp+mq+msp+msq+m; i++)
      printf(" %f", params[i]); printf("\n");  */
    
    if(ns > 0) {
	/* expand out seasonal ARMA models */
	for(i = 0; i < mp; i++) phi[i] = params[i];
	for(i = 0; i < mq; i++) theta[i] = params[i+mp];
	for(i = mp; i < ip; i++) phi[i] = 0.0;
	for(i = mq; i < iq; i++) theta[i] = 0.0;
	for(j = 0; j < msp; j++) {
	    phi[(j+1)*ns-1] += params[j+mp+mq];
	    for(i = 0; i < mp; i++)
		phi[(j+1)*ns+i] -= params[i]*params[j+mp+mq];
	}
	for(j = 0; j < msq; j++) {
	    theta[(j+1)*ns-1] += params[j+mp+mq+msp];
	    for(i = 0; i < mq; i++)
		theta[(j+1)*ns+i] += params[i+mp]*params[j+mp+mq+msp];
	}
    } else {
	for(i = 0; i < mp; i++) phi[i] = params[i];
	for(i = 0; i < mq; i++) theta[i] = params[i+mp];
    }
/*    for(i=0; i < ip; i++) printf(" %f", phi[i]); printf("\n");
      for(i=0; i < iq; i++) printf(" %f", theta[i]); printf("\n");*/

    streg = mp + mq + msp + msq;
    if(m > 0) {
	for(i = 0; i < n; i++) {
	    tmp = wkeep[i];
	    for(j = 0; j < m; j++) tmp -= reg[i + n*j] * params[streg + j];
	    w[i] = tmp;
	}
    }

    F77_CALL(starma)(&ip, &iq, &ir, &np, phi, theta, a, p, v, thetab,
		     xnext, xrow, rbar, &nrbar, &ifault);
    sumlog = 0.0;
    ssq = 0.0;
    it = 0;
    iupd = 1;
    F77_CALL(karma)(&ip, &iq, &ir, &np, phi, theta, a, p, v, &n, w, resid,
		    &sumlog, &ssq, &iupd, &delta, e, &it);
    *res = 0.5*(log(ssq/(double)n) + sumlog/(double)n);
    s2 = ssq/(double)n;
}

void get_s2(double *res)
{
    *res=s2;
}

void get_resid(double *res)
{
    int i;

    for(i = 0; i < n; i++) res[i] = resid[i];
}

void arma0_fore(int *n_ahead, double *x, double *var)
{
    double *work;

    work = Calloc(ir, double);
    F77_CALL(kalfor)(n_ahead, &ip, &ir, &np, phi, a, p, v, work, x, var);
    Free(work);
}

void arma0_kfore(int *pd, int *psd, int *n_ahead, double *x, double *var)
{
    int d, ird, irz, il=*n_ahead, ifault=0, i, j;
    double *del, *del2, *a1, *p1, *e1, *store;
    
    d = *pd + ns**psd;
    ird = ir + d;
    irz = ird*(ird+1)/2;
    
    del = Calloc(d+1, double);
    del2 = Calloc(d+1, double);
    del[0] = 1;
    for(i = 1; i <= d; i++) del[i] = 0;
    for (j = 0; j < *pd; j ++) {
	for(i = 0; i <= d; i++) del2[i] = del[i];
	for(i = 0; i <= d-1; i++) del[i+1] -= del2[i];
    }
    for (j = 0; j < *psd; j ++) {
	for(i = 0; i <= d; i++) del2[i] = del[i];
	for(i = 0; i <= d-ns; i++) del[i+ns] -= del2[i];
    }
    for(i = 1; i <= d; i++) del[i] *= -1;
    /*for(i = 1; i <= d; i++) printf(" %f", del[i]); printf("\n");*/

    
    a1 = Calloc(ird, double);
    p1 = Calloc(irz, double);
    resid = Calloc(n, double);
    e1 = Calloc(ir, double);
    store = Calloc(ird, double);

    F77_CALL(forkal)(&ip, &iq, &ir, &np, &ird, 
		     &irz, &d, &il, &n, &nrbar, 
		     phi, theta, del+1, 
		     w, x, var, a1, 
		     p1, v, resid, e1, 
		     xnext, xrow, rbar, 
		     thetab, store, &ifault);
    Free(del); Free(del2); 
    Free(a1); Free(p1); Free(e1);
    Free(store);
}

void arima0_fore(int *n_ahead, int *pn, double *x, int *seas, int *nsea)
{
    int i, k, sd, n = *pn, na = *n_ahead, N = n+na, ns = *nsea, nc = 0;

    /* initialize all the differenced series */
    for(k = 0; k < ns; k++) {
	sd = seas[k];
	nc += sd;
	for(i = nc; i < n; i++) 
	    x[i + (k+1)*N] = x[i + k*N] - x[i - sd + k*N];
    }
    /* predict them all one step at a time: the top level is already there */
    for(i = 0; i < na; i++) {
	for(k = ns - 1; k >= 0; k--)
	    x[n + i + k*N] = x[n + i - seas[k] + k*N] + x[n+i + (k+1)*N];
    }	    
}

void artoma(int *pp, double *phi, double *psi, int *npsi)
{
    int i, j, p =*pp;

    for(i = 0; i < p; i++) psi[i] = phi[i];
    for(i = p+1; i < *npsi; i++) psi[i] = 0.0;
    for(i = 0; i < *npsi - p - 1; i++) {
	for(j = 0; j < p; j++) psi[i+j+1] += phi[j]*psi[i];
    }
}

void arimatoma(int *arma, double *params, double *psi, int *npsi)
{
    int i, j;
    double tmp;

    ns = arma[4];
    ip = arma[0] + arma[5] + ns*(arma[3] + arma[6]);
    iq = arma[1] + ns*arma[3];
    phi = (double*) R_alloc(ip, sizeof(double));
    theta = (double*) R_alloc(iq, sizeof(double));
    
    mp = arma[0];
    mq = arma[1];
    msp = arma[2];
    msq = arma[3];
    if(ns > 0) {
	/* expand out seasonal ARMA models */
	for(i = 0; i < mp; i++) phi[i] = params[i];
	for(i = 0; i < mq; i++) theta[i] = params[i+mp];
	for(i = mp; i < ip; i++) phi[i] = 0.0;
	for(i = mq; i < iq; i++) theta[i] = 0.0;
	for(j = 0; j < msp; j++) {
	    phi[(j+1)*ns-1] += params[j+mp+mq];
	    for(i = 0; i < mp; i++)
		phi[(j+1)*ns+i] -= params[i]*params[j+mp+mq];
	}
	for(j = 0; j < msq; j++) {
	    theta[(j+1)*ns-1] += params[j+mp+mq+msp];
	    for(i = 0; i < mq; i++)
		theta[(j+1)*ns+i] += params[i+mp]*params[j+mp+mq+msp];
	}
    } else {
	for(i = 0; i < mp; i++) phi[i] = params[i];
	for(i = 0; i < mq; i++) theta[i] = params[i+mp];
    }

    /* expand out differencing */
    for(i = 0; i < arma[5]; i++) {
	for(j = ip - 1; j >= 1; j--) phi[j] -= phi[j-1];
	phi[0] += 1.0;
    }
    for(i = 0; i < arma[6]; i++) {
	for(j = ip - 1; j >= ns; j--) phi[j] -= phi[j-ns];
	phi[ns-1] += 1.0;
    }

    /* Invert: Harvey 1993, p. 117) */
    for(i = 0; i < *npsi; i++) {
	tmp = 0.0;
	for(j = 0; j < ip; j ++) {
	    if(j == i){
		tmp += phi[j]; /* implied 1 on psi */
		break;
	    }
	    tmp += phi[j] * psi[i - j - 1];
	}
	if(i < iq) tmp += theta[i];
	psi[i] = tmp;
    }
}

static void partrans(int np, double *raw, double *new)
{
    int i, j;

    for(i = 0; i < np; i++) raw[i] = new[i] = tanh(raw[i]);
    for(j = 1; j < np; j++)
	for(i = 0; i < j; i++)
	    raw[i] -= new[j] * new[j - i - 1];
	    for(i = 0; i < j; i++) new[i] = raw[i];
}

/* raw is overwritten */
void dotrans(double *raw, double *new, int trans)
{
    int i, v;

    if(trans) {
	v = 0;
	for(i = 0; i < mp; i++)
	    partrans(mp, raw+v, new+v);
	v += mp;
	for(i = 0; i < mq; i++)
	    partrans(mq, raw+v, new+v);
	v += mq;
	for(i = 0; i < msp; i++)
	    partrans(msp, raw+v, new+v);
	v += msp;
	for(i = 0; i < msq; i++)
	    partrans(msq, raw+v, new+v);
	for(i = mp+mq+msp+msq; i < mp+mq+msp+msq + m; i++) new[i] = raw[i];
    } else
	for(i = 0; i < mp+mq+msp+msq+m; i++) new[i] = raw[i];
}
