/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 1999-2002	The R Development Core Team
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
#ifndef min
#define min(a,b) ((a > b)?(b):(a))
#endif

/* Code in this file based on Applied Statistics algorithms AS154/182
   (C) Royal Statistical Society 1980, 1982 */

static void
inclu2(int np, double *xnext, double *xrow, double ynext,
       double *d, double *rbar, double *thetab)
{
    double cbar, sbar, di, xi, xk, rbthis, dpi;
    int i, k, ithisr;

/*   This subroutine updates d, rbar, thetab by the inclusion
     of xnext and ynext with a specified weight. The values of xnext,
     ynext and weight will be conserved. the corresponding value of
     recres is calculated. */

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

void
starma(int p, int q, int r, int np, double *phi,
       double *theta, double *a, double *P, double *V,
       double *thetab, double *xnext, double *xrow,
       double *rbar, int nrbar, int *ifault)
{
    int indi, indj, indn;
    double phii, phij, ynext, vj, bi;
    int i, j, k, ithisr, ind, npr, ind1, ind2, npr1, im, jm;

/*      Invoking this subroutine sets the values of v and phi, and
        obtains the initial values of a and p. */

/*     Check if ar(1) */

    if (!(q > 0 || p > 1)) {
	V[0] = 1.0;
	a[0] = 0.0;
	P[0] = 1.0 / (1.0 - phi[0] * phi[0]);
	return;
    }

/*        Check for failure indication. */
    *ifault = 0;
    if (p < 0) *ifault = 1;
    if (q < 0) *ifault += 2;
    if (p == 0 && q == 0) *ifault = 4;
    k = q + 1;
    if (k < p) k = p;
    if (r != k) *ifault = 5;
    if (np != r * (r + 1) / 2) *ifault = 6;
    if (nrbar != np * (np - 1) / 2) *ifault = 7;
    if (r == 1) *ifault = 8;
    if (*ifault != 0) return;

/*        Now set a(0), v and phi. */

    for (i = 1; i < r; i++) {
	a[i] = 0.0;
	if (i >= p) phi[i] = 0.0;
	V[i] = 0.0;
	if (i < q + 1) V[i] = theta[i - 1];
    }
    a[0] = 0.0;
    if (p == 0) phi[0] = 0.0;
    V[0] = 1.0;
    ind = r;
    for (j = 1; j < r; j++) {
	vj = V[j];
	for (i = j; i < r; i++) V[ind++] = V[i] * vj;
    }

/*        Now find p(0). */

    if (p > 0) {
/*      The set of equations s * vec(p(0)) = vec(v) is solved for
        vec(p(0)).  s is generated row by row in the array xnext.  The
        order of elements in p is changed, so as to bring more leading
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
	    phij = phi[j];
	    xnext[indj++] = 0.0;
	    indi = npr1 + j;
	    for (i = j; i < r; i++) {
		ynext = V[ind++];
		phii = phi[i];
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
	    bi = thetab[im];
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

/* p(0) is obtained by backsubstitution for a moving average process. */

	indn = np;
	ind = np;
	for (i = 0; i < r; i++)
	    for (j = 0; j <= i; j++) {
		--ind;
		P[ind] = V[ind];
		if (j != 0) P[ind] += P[--indn];
	    }
    }
}


void
karma(int p, int q, int r, int np, double *phi, double *theta,
      double *a, double *P, double *V, int n, double *w, double *resid,
      double *sumlog, double *ssq, int iupd, double delta, int *nit)
{
    int i, j, l, ii, ind, indn, indw;
    double a1, dt, et, ft, g, ut;

/*  Invoking this subroutine updates a, P, sumlog and ssq by inclusion
    of data values w(1) to w(n). the corresponding values of resid are
    also obtained.  When ft is less than (1 + delta), quick recursions
    are used. */

/*        for non-zero values of nit, perform quick recursions. */

    if (*nit == 0) {
	for (i = 0; i < n; i++) {

/*        prediction. */

	    if (iupd != 1 || i > 0) {

/*        here dt = ft - 1.0 */

		dt = (r > 1) ? P[r] : 0.0;
		if (dt < delta) goto L610;
		a1 = a[0];
		for (j = 0; j < r - 1; j++) a[j] = a[j + 1];
		a[r - 1] = 0.0;
		for (j = 0; j < p; j++) a[j] += phi[j] * a1;
		ind = -1;
		indn = r;
		for (l = 0; l < r; l++)
		    for (j = l; j < r; j++) {
			++ind;
			P[ind] = V[ind];
			if (j < r - 1) P[ind] += P[indn++];
		    }
	    }

/*        updating. */

	    ft = P[0];
	    if(!ISNAN(w[i])) {
		ut = w[i] - a[0];
		if (r > 1)
		    for (j = 1, ind = r; j < r; j++) {
			g = P[j] / ft;
			a[j] += g * ut;
			for (l = j; l < r; l++) P[ind++] -= g * P[l];
		    }
		a[0] = w[i];
		resid[i] = ut / sqrt(ft);
		*ssq += ut * ut / ft;
		*sumlog += log(ft);
	    } else resid[i] = NA_REAL;

	    for (l = 0; l < r; l++) P[l] = 0.0;
	}
	*nit = n;

    } else {

/*        quick recursions */

	i = 0;
 L610:
	*nit = i;
	for (ii = i; ii < n; ii++) {
	    et = w[ii];
	    indw = ii;
	    for (j = 0; j < p; j++) {
		if (--indw < 0) break;
		et -= phi[j] * w[indw];
	    }
	    for (j = 0; j < q; j++)
		et -= theta[j] * resid[ii - j - 1];
	    resid[ii] = et;
	    *ssq += et * et;
	}
    }
}


/*  start of as 182 */
void
forkal(int ip, int iq, int ir, int np, int id, int il, 
       int n, int nrbar, double *phi, double *theta, double *delta,
       double *w, double *y, double *amse, double *v, double *resid, 
       double *xnext, double *xrow, double *rbar, double *thetab,
       int *ifault)
{
    double *a, *p, *store;
    int ird = ir + id, irz = ird*(ird + 1)/2;
    
    /* Local variables */
    int id2r1, iddr, id2r2;
    double phii, phij;
    int idrr1, i, j, k, l;
    double sigma, a1;
    int j1, k1;
    double aa;
    int i45, jj, kk, lk, ll;
    double dt;
    int nt;
    double phijdt;
    int id1;
    double sumlog;
    int kk1, lk1, ir1, ir2, ibc;
    int iid, idk, ind, jkl, kkk, jrj, jrk;
    double ams;
    int nit;
    double ssq, tmp;
    int idd1, idd2, ind1, ind2, id2r, jkl1, iri1;

/*  Finite sample prediction from ARIMA processes. */

/*  This routine will calculate the finite sample predictions
    and their conditional mean square errors for any ARIMA process. */

    /* Parameter adjustments */
    --phi;
    --v;
    --delta;
    --amse;
    --y;
    --w;

    /* Function Body */

/*     invoking this routine will calculate the finite sample predictions */
/*     and their conditional mean square errors for any arima process. */

/*     check for input faults. */

    a = (double *) R_alloc(ird, sizeof(double));
    p = (double *) R_alloc(irz, sizeof(double));
    store = (double *) R_alloc(ird, sizeof(double));
    --store;
    --a;
    --p;
    
    *ifault = 0;
    if (ip < 0) *ifault = 1;
    if (iq < 0) *ifault += 2;
    if (ip * ip + iq * iq == 0) *ifault = 4;
    k = iq + 1;
    if (k < ip) k = ip;
    if (ir != k) *ifault = 5;
    if (np != ir * (ir + 1) / 2) *ifault = 6;
    if (nrbar < np * (np - 1) / 2) *ifault = 7;
    if (id < 0) *ifault = 8;
    if (ird != ir + id) *ifault = 9;
    if (irz != ird * (ird + 1) / 2) *ifault = 10;
    if (il < 1) *ifault = 11;
    if (*ifault != 0) return;

/*     Calculate initial conditions for Kalman filter */

    a[1] = 0.0;
    v[1] = 1.0;

/*     Find initial likelihood conditions. */

    if (ir == 1) p[1] = 1.0 / (1.0 - phi[1] * phi[1]);
    else
	starma(ip, iq, ir, np, &phi[1], theta, &a[1], &p[1], &v[1],
	       thetab, xnext, xrow, rbar, nrbar, ifault);

/*     Calculate data transformations */

    nt = n - id;
    if (id > 0) {
	for (j = 1; j <= id; j++)
	    store[j] = w[n - j];
	for (i = 1; i <= nt; i++) {
	    aa = 0.0;
	    for (k = 1; k <= id; ++k) {
		idk = id + i - k;
		aa -= delta[k] * w[idk];
	    }
	    iid = i + id;
	    w[i] = w[iid] + aa;
	}
    }

/*     Evaluate likelihood to obtain final Kalman filter conditions */

    sumlog = 0.0;
    ssq = 0.0;
    nit = 0;
    karma(ip, iq, ir, np, &phi[1], theta, &a[1], &p[1], &v[1], nt, 
	  &w[1], resid, &sumlog, &ssq, 1, -1.0, &nit);

/*     Calculate m.l.e. of sigma squared */

    sigma = 0.0;
    for (j = 1; j <= nt; j++) {
	tmp = resid[j-1];
	sigma += tmp * tmp;
    }
    sigma /= nt;

/*     reset the initial a and p when differencing occurs */

    if (id > 0) {
	for (i = 1; i <= np; i++) xrow[i-1] = p[i];
	for (i = 1; i <= irz; i++) p[i] = 0.0;
	ind = 0;
	for (j = 1; j <= ir; j++) {
	    k = (j - 1) * (id + ir + 1) - (j - 1) * j / 2;
	    for (i = j; i <= ir; i++) p[++k] = xrow[ind++];
	}
	for (j = 1; j <= id; j++) a[ir + j] = store[j];
    }

/*     Set up constants */

    ir2 = ir + 1;
    ir1 = ir - 1;
    id1 = id - 1;
    id2r = ird << 1;
    id2r1 = id2r - 1;
    idd1 = (id << 1) + 1;
    idd2 = idd1 + 1;
    i45 = id2r + 1;
    idrr1 = ird + 1;
    iddr = (id << 1) + ir;
    jkl = ir * (iddr + 1) / 2;
    jkl1 = jkl + 1;
    id2r2 = id2r + 2;
    ibc = ir * (i45 - ir) / 2;
    for (l = 1; l <= il; ++l) {

/*     predict a */

	a1 = a[1];
	for (i = 1; i <= ir1; i++) a[i] = a[i + 1];
	a[ir] = 0.0;
	for (j = 1; j <= ip; j++) a[j] += phi[j] * a1;
	if (id > 0) {
	    for (j = 1; j <= id; j++) a1 += delta[j] * a[ir + j];
	    for (i = 1; i <= id1; i++) {
		iri1 = ird - i;
		a[iri1 + 1] = a[iri1];
	    }
	    a[ir2] = a1;
	}

/*     predict p */

	if (id > 0) {
	    for (i = 1; i <= id; i++) {
		store[i] = 0.0;
		for (j = 1; j <= id; j++) {
		    ll = max(i,j);
		    k = min(i,j);
		    jj = jkl + (ll - k) + 1 + (k - 1) * (idd2 - k) / 2;
		    store[i] += delta[j] * p[jj];
		}
	    }
	    if (id > 1) {
		for (j = 1; j <= id1; j++) {
		    jj = id - j;
		    lk = (jj - 1) * (idd2 - jj) / 2 + jkl;
		    lk1 = jj * (idd1 - jj) / 2 + jkl;
		    for (i = 1; i <= j; i++) p[++lk1] = p[++lk];
		}
		for (j = 1; j <= id1; j++)
		    p[jkl1 + j] = store[j] + p[ir + j];
	    }
	    p[jkl1] = p[1];
	    for (i = 1; i <= id; i++)
		p[jkl1] += delta[i] * (store[i] + 2.0 * p[ir + i]);
	    for (i = 1; i <= id; i++) store[i] = p[ir + i];
	    for (j = 1; j <= ir; j++) {
		kk1 = j * (id2r1 - j) / 2 + ir;
		k1 = (j - 1) * (id2r - j) / 2 + ir;
		for (i = 1; i <= id; i++) {
		    kk = kk1 + i;
		    k = k1 + i;
		    p[k] = phi[j] * store[i];
		    if (j != ir) p[k] += p[kk];
		}
	    }

	    for (j = 1; j <= ir; j++) {
		store[j] = 0.0;
		kkk = j * (i45 - j) / 2 - id;
		for (i = 1; i <= id; i++)
		    store[j] += delta[i] * p[++kkk];
	    }
	    for (j = 1; j <= ir; j++) {
		k = j * idrr1 - j * (j + 1) / 2 + 1;
		for (i = 1; i <= id1; i++) {
		    --k;
		    p[k] = p[k - 1];
		}
	    }
	    for (j = 1; j <= ir; j++) {
		k = (j - 1) * (id2r - j) / 2 + ir + 1;
		p[k] = store[j] + phi[j] * p[1];
		if (j < ir) p[k] += p[j + 1];
	    }
	}
	for (i = 1; i <= ir; i++) store[i] = p[i];

	ind = 0;
	dt = p[1];
	for (j = 1; j <= ir; j++) {
	    phij = phi[j];
	    phijdt = phij * dt;
	    ind2 = (j - 1) * (id2r2 - j) / 2;
	    ind1 = j * (i45 - j) / 2;
	    for (i = j; i <= ir; i++) {
		++ind;
		++ind2;
		phii = phi[i];
		p[ind2] = v[ind] + phii * phijdt;
		if (j < ir)
		    p[ind2] += store[j + 1] * phii;
		if (i < ir)
		    p[ind2] = p[ind2] + store[i + 1] * phij + p[++ind1];
	    }
	}

/*     predict y */

	y[l] = a[1];
	for (j = 1; j <= id; j++) y[l] += a[ir + j] * delta[j];

/*     calculate m.s.e. of y */

	ams = p[1];
	if (id > 0) {
	    for (j = 1; j <= id; j++) {
		jrj = ibc + (j - 1) * (idd2 - j) / 2;
		tmp = delta[j];
		ams = ams + 2.0 * delta[j] * p[ir + j] + p[jrj + 1] * tmp * tmp;
	    }
	    for (j = 1; j <= id1; j++) {
		j1 = j + 1;
		jrk = ibc + 1 + (j - 1) * (idd2 - j) / 2;
		for (i = j1; i <= id; i++)
		    ams += 2.0 * delta[i] * delta[j] * p[++jrk];
	    }
	}
	amse[l] = ams * sigma;
    }
    return;
}
