/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 1999-2001	The R Development Core Team
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
#include "ts.h"

#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#endif

#ifndef max
#define max(a,b) ((a < b)?(b):(a))
#endif


/* Internal */
static void partrans(int np, double *raw, double *new);
static void dotrans(double *raw, double *new, int trans);

/* Globals for `starma' : */
#ifdef ONE_global
typedef struct {
#endif
    int ip, iq, mp, mq, msp, msq, ns, ir, np, nrbar, n, m, trans;
    double *a, *p, *v, *thetab, *xnext, *xrow, *rbar,
	*w, *wkeep, delta, *resid, *phi, *theta, s2, *reg, *params;
#ifdef ONE_global
} starma_Gtype
static starma_Gtype *starma_G;
#endif

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

void free_starma(void)
{
    Free(params); Free(a); Free(p); Free(v); Free(thetab);
    Free(xnext); Free(xrow); Free(rbar);
    Free(w); Free(wkeep); Free(resid); Free(phi); Free(theta); Free(reg);
}

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

    starma(&ip, &iq, &ir, &np, phi, theta, a, p, v, thetab,
	   xnext, xrow, rbar, &nrbar, &ifault);
    sumlog = 0.0;
    ssq = 0.0;
    it = 0;
    iupd = 1;
    karma(&ip, &iq, &ir, &np, phi, theta, a, p, v, &n, w, resid,
	  &sumlog, &ssq, &iupd, &delta, &it);
    *res = 0.5*(log(ssq/(double)n) + sumlog/(double)n);
    s2 = ssq/(double)n;
}

void get_s2(double *res)
{
    *res = s2;
}

void get_resid(double *res)
{
    int i;

    for(i = 0; i < n; i++) res[i] = resid[i];
}

void arma0_kfore(int *pd, int *psd, int *n_ahead, double *x, double *var)
{
    int d, ird, irz, il = *n_ahead, ifault=0, i, j;
    double *del, *del2, *a1, *p1, *store;
    
    d = *pd + ns**psd;
    ird = ir + d;
    irz = ird*(ird+1)/2;
    
    del = (double *) R_alloc(d+1, sizeof(double));
    del2 = (double *) R_alloc(d+1, sizeof(double));
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

    
    a1 = (double *) R_alloc(ird, sizeof(double));
    p1 = (double *) R_alloc(irz, sizeof(double));
    store = (double *) R_alloc(ird, sizeof(double));

    forkal(&ip, &iq, &ir, &np, &ird, 
		     &irz, &d, &il, &n, &nrbar, 
		     phi, theta, del+1, 
		     w, x, var, a1, 
		     p1, v, resid, 
		     xnext, xrow, rbar, 
		     thetab, store, &ifault);
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
static void dotrans(double *raw, double *new, int trans)
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
