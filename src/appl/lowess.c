/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Mathlib.h"

static void sort(double *x, int n)
{
	int i, ir, j, l;
	double xt;

	x--;

	l = (n/2)+1;
	ir = n;
	for(;;) {
		if (l > 1) {
			l = l-1;
			xt = x[l];
		}
		else {
			xt = x[ir];
			x[ir] = x[1];
			ir = ir-1;
			if (ir == 1)
				break;
		}
		i = l;
		j = 2*l;
		while (j <= ir) {
			if (j < ir && x[j] < x[j+1])
				j = j+1;
			if (xt >= x[j])
				j = ir+1;
			else {
				x[i] = x[j];
				i = j;
				j = j+i;
			}
		}
		x[i] = xt;
	}
	x[1] = xt;
}


static void lowest(double *x, double *y, int n, double *xs, double *ys,
	int nleft, int nright, double *w,
	int userw, double *rw, int *ok)
{
	int nrt, j;
	double a, b, c, h, h1, h9, r, range;

	x--;
	y--;
	w--;
	rw--;

	range = x[n]-x[1];
	h = fmax2(*xs-x[nleft], x[nright]-*xs);
	h9 = 0.999*h;
	h1 = 0.001*h;

	/* sum of weights */

	a = 0.0;
	j = nleft;
	while (j<=n) {

		/* compute weights */
		/* (pick up all ties on right) */

		w[j] = 0.0;
		r = fabs(x[j] - *xs);
		if (r <= h9) {
			if (r <= h1)
				w[j] = 1.0;
			else
				w[j] = fcube(1.0-fcube(r/h));
			if (userw)
				w[j] = rw[j]*w[j];
			a = a+w[j];
		}
		else if (x[j] > *xs)
			break;
		j = j+1;
	}

	/* rightmost pt (may be greater */
	/* than nright because of ties) */

	nrt = j-1;
	if (a <= 0.0)
		*ok = 0;
	else {
		*ok = 1;

		/* weighted least squares */
		/* make sum of w[j] == 1 */

		for(j=nleft ; j<=nrt ; j++)
			w[j] = w[j]/a;
		if (h > 0.0) {
			a = 0.0;

			/*  use linear fit */
			/* weighted center of x values */

			for(j=nleft ; j<=nrt ; j++)
				a = a + w[j] * x[j];
			b = *xs - a;
			c = 0.0;
			for(j=nleft ; j<=nrt ; j++)
				c = c+w[j]*fsquare(x[j]-a);
			if (sqrt(c)>0.001*range) {
				b = b/c;

				/* points are spread out */
				/* enough to compute slope */

				for(j=nleft ; j<=nrt ; j++)
					w[j] = w[j]*(b*(x[j]-a)+1.0);
			}
		}
		*ys = 0.0;
		for(j=nleft ; j<=nrt ; j++)
			*ys = *ys + w[j] * y[j];
	}
}

static void clowess(double *x, double *y, int n,
	double f, int nsteps, double delta,
	double *ys, double *rw, double *res)
{
	int i, iter, j, last, m1, m2, nleft, nright, ns, ok;
	double alpha, c1, c9, cmad, cut, d1, d2, denom, r;

	x--;
	y--;
	ys--;
	rw--;
	res--;

	if (n < 2) {
		ys[1] = y[1];
		return;
	}

		/* at least two,  at most n points */

	ns = imax2(imin2((int)(f*n), n), 2);

		/* robustness iterations */

	iter = 1;
	while (iter <= nsteps+1) {
		nleft = 1;
		nright = ns;
		last = 0;	/* index of prev estimated point */
		i = 1;		/* index of current point */

		for(;;) {
			if (nright < n) {

				/* move nleft,  nright to right */
				/* if radius decreases */

				d1 = x[i]-x[nleft];
				d2 = x[nright+1]-x[i];

				/* if d1 <= d2 with */
				/* x[nright+1] == x[nright], */
				/* lowest fixes */

				if (d1 > d2) {

					/* radius will not */
					/* decrease by */
					/* move right */

					nleft = nleft+1;
					nright = nright+1;
					continue;
				}
			}

				/* fitted value at x[i] */

			lowest(&x[1], &y[1], n, &x[i], &ys[i],
				nleft, nright, res, iter>1, &rw[1], &ok);
			if (!ok) ys[i] = y[i];

			/* all weights zero */
			/* copy over value (all rw==0) */

			if (last < i-1) {
				denom = x[i]-x[last];

				/* skipped points -- interpolate */
				/* non-zero - proof? */

				j = last+1;
				while (j < i) {
					alpha = (x[j]-x[last])/denom;
					ys[j] = alpha*ys[i]+(1.0-alpha)*ys[last];
					j = j+1;
				}
			}

			/* last point actually estimated */
			last = i;

			/* x coord of close points */
			cut = x[last]+delta;
			i = last+1;
			while (i <= n) {
				if (x[i] > cut)
					break;
				if (x[i] == x[last]) {
					ys[i] = ys[last];
					last = i;
				}
				i = i+1;
			}
			i = imax2(last+1, i-1);
			if (last >= n)
				break;
		}
		/* residuals */
		for(i=1 ; i<=n ; i++)
			res[i] = y[i] - ys[i];

		/* compute robustness weights */
		/* except last time */

		if (iter > nsteps)
			break;
		for(i=1 ; i<=n ; i++)
			rw[i] = fabs(res[i]);
		sort(&rw[1], n);
		m1 = n/2+1;
		m2 = n-m1+1;

		/* 6 median abs resid */

		cmad = 3.0*(rw[m1]+rw[m2]);
		c9 = 0.999*cmad;
		c1 = 0.001*cmad;
		for(i=1 ; i<=n ; i++) {
			r = fabs(res[i]);
			if (r <= c1)
				rw[i] = 1.0;
			else if (r <= c9)
				rw[i] = fsquare(1.0-fsquare(r/cmad));
			else
				rw[i] = 0.0;
		}
		iter = iter+1;
	}
}

int lowess(double *x, double *y, int *n,
	double *f, int *nsteps, double *delta,
	double *ys, double *rw, double *res)
{
	clowess(x, y, *n, *f, *nsteps, *delta, ys, rw, res);
	return 0;
}
