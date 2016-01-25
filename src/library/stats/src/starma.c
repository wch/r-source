/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 1999-2002 The R Core Team
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
 *  https://www.R-project.org/Licenses/.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>
#include <math.h>

#include <R.h>
#include "ts.h"


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
	} else ithisr = ithisr + np - i - 1;
    }
}

void starma(Starma G, int *ifault)
{
    int p = G->p, q = G->q, r = G->r, np = G->np, nrbar = G->nrbar;
    double *phi = G->phi, *theta = G->theta, *a = G->a,
	*P = G->P, *V = G->V, *thetab = G->thetab, *xnext = G->xnext,
	*xrow = G->xrow, *rbar = G->rbar;
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

/*        Now set a(0), V and phi. */

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

/* P(0) is obtained by backsubstitution for a moving average process. */

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

void karma(Starma G, double *sumlog, double *ssq, int iupd, int *nit)
{
    int p = G->p, q = G->q, r = G->r, n = G->n, nu = 0;
    double *phi = G->phi, *theta = G->theta, *a = G->a, *P = G->P,
	*V = G->V, *w = G->w, *resid = G->resid, *work = G->xnext;

    int i, j, l, ii, ind, indn, indw;
    double a1, dt, et, ft, g, ut, phij, phijdt;

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
		if (dt < G->delta) goto L610;
		a1 = a[0];
		for (j = 0; j < r - 1; j++) a[j] = a[j + 1];
		a[r - 1] = 0.0;
		for (j = 0; j < p; j++) a[j] += phi[j] * a1;
		if(P[0] == 0.0) { /* last obs was available */
		    ind = -1;
		    indn = r;
		    for (j = 0; j < r; j++)
			for (l = j; l < r; l++) {
			    ++ind;
			    P[ind] = V[ind];
			    if (l < r - 1) P[ind] += P[indn++];
			}
		} else {
		    for (j = 0; j < r; j++) work[j] = P[j];
		    ind = -1;
		    indn = r;
		    dt = P[0];
		    for (j = 0; j < r; j++) {
			phij = phi[j];
			phijdt = phij * dt;
			for(l = j; l < r; l++) {
			    ++ind;
			    P[ind] = V[ind] + phi[l] * phijdt;
			    if (j < r - 1) P[ind] += work[j+1] * phi[l];
			    if (l < r - 1)
				P[ind] += work[l+1] * phij + P[indn++];
			}
		    }
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
		nu++;
		for (l = 0; l < r; l++) P[l] = 0.0;
	    } else resid[i] = NA_REAL;

	}
	*nit = n;

    } else {

/*        quick recursions: never used with missing values */

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
	    for (j = 0; j < min(ii, q); j++)
		et -= theta[j] * resid[ii - j - 1];
	    resid[ii] = et;
	    *ssq += et * et;
	    nu++;
	}
    }
    G->nused = nu;
}


/*  start of AS 182 */
void
forkal(Starma G, int d, int il, double *delta, double *y, double *amse,
       int *ifault)
{
    int p = G->p, q = G->q, r = G->r, n = G->n, np = G->np;
    double *phi = G->phi, *V = G->V, *w = G->w, *xrow = G->xrow;
    double *a, *P, *store;
    int rd = r + d, rz = rd*(rd + 1)/2;
    double phii, phij, sigma2, a1, aa, dt, phijdt, ams, tmp;
    int i, j, k, l, nu = 0;
    int k1;
    int i45, jj, kk, lk, ll;
    int nt;
    int kk1, lk1;
    int ind, jkl, kkk;
    int ind1, ind2;

/*  Finite sample prediction from ARIMA processes. */

/*  This routine will calculate the finite sample predictions
    and their conditional mean square errors for any ARIMA process. */

/*     invoking this routine will calculate the finite sample predictions */
/*     and their conditional mean square errors for any arima process. */

    store = (double *) R_alloc(rd, sizeof(double));
    Free(G->a); G->a = a = Calloc(rd, double);
    Free(G->P); G->P = P = Calloc(rz, double);

/*     check for input faults. */
    *ifault = 0;
    if (p < 0) *ifault = 1;
    if (q < 0) *ifault += 2;
    if (p * p + q * q == 0) *ifault = 4;
    if (r != max(p, q + 1)) *ifault = 5;
    if (np != r * (r + 1) / 2) *ifault = 6;
    if (d < 0) *ifault = 8;
    if (il < 1) *ifault = 11;
    if (*ifault != 0) return;

/*     Find initial likelihood conditions. */

    if (r == 1) {
	a[0] = 0.0;
	V[0] = 1.0;
	P[0] = 1.0 / (1.0 - phi[0] * phi[0]);
    } else starma(G, ifault);

/*     Calculate data transformations */

    nt = n - d;
    if (d > 0) {
	for (j = 0; j < d; j++) {
	    store[j] = w[n - j - 2];
	    if(ISNAN(store[j]))
		error(_("missing value in last %d observations"), d);
	}
	for (i = 0; i < nt; i++) {
	    aa = 0.0;
	    for (k = 0; k < d; ++k) aa -= delta[k] * w[d + i - k - 1];
	    w[i] = w[i + d] + aa;
	}
    }

/*     Evaluate likelihood to obtain final Kalman filter conditions */

    {
	double sumlog = 0.0, ssq = 0.0;
	int nit = 0;
	G->n = nt;
	karma(G, &sumlog, &ssq, 1, &nit);
    }


/*     Calculate m.l.e. of sigma squared */

    sigma2 = 0.0;
    for (j = 0; j < nt; j++) {
	/* MacOS X/gcc 3.5 does/didn't have isnan defined properly */
	tmp = G->resid[j];
	if(!ISNAN(tmp)) { nu++; sigma2 += tmp * tmp; }
    }

    sigma2 /= nu;

/*     reset the initial a and P when differencing occurs */

    if (d > 0) {
	for (i = 0; i < np; i++) xrow[i] = P[i];
	for (i = 0; i < rz; i++) P[i] = 0.0;
	ind = 0;
	for (j = 0; j < r; j++) {
	    k = j * (rd + 1) - j * (j + 1) / 2;
	    for (i = j; i < r; i++) P[k++] = xrow[ind++];
	}
	for (j = 0; j < d; j++) a[r + j] = store[j];
    }

    i45 = 2*rd + 1;
    jkl = r * (2*d + r + 1) / 2;

    for (l = 0; l < il; ++l) {

/*     predict a */

	a1 = a[0];
	for (i = 0; i < r - 1; i++) a[i] = a[i + 1];
	a[r - 1] = 0.0;
	for (j = 0; j < p; j++) a[j] += phi[j] * a1;
	if (d > 0) {
	    for (j = 0; j < d; j++) a1 += delta[j] * a[r + j];
	    for (i = rd - 1; i > r; i--) a[i] = a[i - 1];
	    a[r] = a1;
	}

/*     predict P */

	if (d > 0) {
	    for (i = 0; i < d; i++) {
		store[i] = 0.0;
		for (j = 0; j < d; j++) {
		    ll = max(i, j);
		    k = min(i, j);
		    jj = jkl + (ll - k) + k * (2*d + 2 - k - 1) / 2;
		    store[i] += delta[j] * P[jj];
		}
	    }
	    if (d > 1) {
		for (j = 0; j < d - 1; j++) {
		    jj = d - j - 1;
		    lk = (jj - 1) * (2*d + 2 - jj) / 2 + jkl;
		    lk1 = jj * (2*d + 1 - jj) / 2 + jkl;
		    for (i = 0; i <= j; i++) P[lk1++] = P[lk++];
		}
		for (j = 0; j < d - 1; j++)
		    P[jkl + j + 1] = store[j] + P[r + j];
	    }
	    P[jkl] = P[0];
	    for (i = 0; i < d; i++)
		P[jkl] += delta[i] * (store[i] + 2.0 * P[r + i]);
	    for (i = 0; i < d; i++) store[i] = P[r + i];
	    for (j = 0; j < r; j++) {
		kk1 = (j+1) * (2*rd - j - 2) / 2 + r;
		k1 = j * (2*rd - j - 1) / 2 + r;
		for (i = 0; i < d; i++) {
		    kk = kk1 + i;
		    k = k1 + i;
		    P[k] = phi[j] * store[i];
		    if (j < r - 1) P[k] += P[kk];
		}
	    }

	    for (j = 0; j < r; j++) {
		store[j] = 0.0;
		kkk = (j + 1) * (i45 - j - 1) / 2 - d;
		for (i = 0; i < d; i++) store[j] += delta[i] * P[kkk++];
	    }
	    for (j = 0; j < r; j++) {
		k = (j + 1) * (rd + 1) - (j + 1) * (j + 2) / 2;
		for (i = 0; i < d - 1; i++) {
		    --k;
		    P[k] = P[k - 1];
		}
	    }
	    for (j = 0; j < r; j++) {
		k = j * (2*rd - j - 1) / 2 + r;
		P[k] = store[j] + phi[j] * P[0];
		if (j < r - 1) P[k] += P[j + 1];
	    }
	}
	for (i = 0; i < r; i++) store[i] = P[i];

	ind = 0;
	dt = P[0];
	for (j = 0; j < r; j++) {
	    phij = phi[j];
	    phijdt = phij * dt;
	    ind2 = j * (2*rd - j + 1) / 2 - 1;
	    ind1 = (j + 1) * (i45 - j - 1) / 2 - 1;
	    for (i = j; i < r; i++) {
		++ind2;
		phii = phi[i];
		P[ind2] = V[ind++] + phii * phijdt;
		if (j < r - 1) P[ind2] += store[j + 1] * phii;
		if (i < r - 1)
		    P[ind2] += store[i + 1] * phij + P[++ind1];
	    }
	}

/*     predict y */

	y[l] = a[0];
	for (j = 0; j < d; j++) y[l] += a[r + j] * delta[j];

/*     calculate m.s.e. of y */

	ams = P[0];
	if (d > 0) {
	    for (j = 0; j < d; j++) {
		k = r * (i45 - r) / 2 + j * (2*d + 1 - j) / 2;
		tmp = delta[j];
		ams += 2.0 * tmp * P[r + j] + P[k] * tmp * tmp;
	    }
	    for (j = 0; j < d - 1; j++) {
		k = r * (i45 - r) / 2 + 1 + j * (2*d + 1 - j) / 2;
		for (i = j + 1; i < d; i++)
		    ams += 2.0 * delta[i] * delta[j] * P[k++];
	    }
	}
	amse[l] = ams * sigma2;
    }
    return;
}
