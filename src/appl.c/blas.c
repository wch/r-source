/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1997  Robert Gentleman and Ross Ihaka
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
 *
 *  BLAS in C
 *
 *  This is essentially machine translated from Netlib code.
 *  Some minor fixups and formatting has been done.
 */

#include "Fortran.h"
#include "Blas.h"

/* takes the sum of the absolute values. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

double 
F77_SYMBOL(dasum) (int *n, double *dx, int *incx)
{
	double dtemp;
	double ret_val, d__1, d__2, d__3, d__4, d__5, d__6;
	int i, m;
	int i__1, i__2;
	int nincx, mp1;

	--dx;

	ret_val = 0.;
	dtemp = 0.;
	if (*n <= 0 || *incx <= 0) {
		return ret_val;
	}
	if (*incx == 1) {
		goto L20;
	}

	/* code for increment not equal to 1 */

	nincx = *n * *incx;
	i__1 = nincx;
	i__2 = *incx;
	for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
		dtemp += (d__1 = dx[i], abs(d__1));
	}
	ret_val = dtemp;
	return ret_val;

	/* code for increment equal to 1 */

	/* clean-up loop */

L20:
	m = *n % 6;
	if (m == 0) {
		goto L40;
	}
	i__2 = m;
	for (i = 1; i <= i__2; ++i) {
		dtemp += (d__1 = dx[i], abs(d__1));
	}
	if (*n < 6) {
		goto L60;
	}
L40:
	mp1 = m + 1;
	i__2 = *n;
	for (i = mp1; i <= i__2; i += 6) {
		dtemp = dtemp + (d__1 = dx[i], abs(d__1))
			+ (d__2 = dx[i + 1], abs(d__2))
			+ (d__3 = dx[i + 2], abs(d__3))
			+ (d__4 = dx[i + 3], abs(d__4))
			+ (d__5 = dx[i + 4], abs(d__5))
			+ (d__6 = dx[i + 5] ,abs(d__6));
	}
L60:
	ret_val = dtemp;
	return ret_val;
}


 /* constant times a vector plus a vector. */
 /* uses unrolled loops for increments equal to one. */
 /* jack dongarra, linpack, 3/11/78. */
 /* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(daxpy) (int *n, double *da, double *dx,
		   int *incx, double *dy, int *incy)
{
	int i__1;
	int i, m, ix, iy, mp1;


	--dy;
	--dx;

	if (*n <= 0) {
		return 0;
	}
	if (*da == 0.) {
		return 0;
	}
	if (*incx == 1 && *incy == 1) {
		goto L20;
	}

	/* code for unequal increments or equal increments */
	/* not equal to 1 */

	ix = 1;
	iy = 1;
	if (*incx < 0) {
		ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
		iy = (-(*n) + 1) * *incy + 1;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dy[iy] += *da * dx[ix];
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

	/* clean-up loop */
L20:
	m = *n % 4;
	if (m == 0) {
		goto L40;
	}
	i__1 = m;
	for (i = 1; i <= i__1; ++i) {
		dy[i] += *da * dx[i];
	}
	if (*n < 4) {
		return 0;
	}
L40:
	mp1 = m + 1;
	i__1 = *n;
	for (i = mp1; i <= i__1; i += 4) {
		dy[i] += *da * dx[i];
		dy[i + 1] += *da * dx[i + 1];
		dy[i + 2] += *da * dx[i + 2];
		dy[i + 3] += *da * dx[i + 3];
	}
	return 0;
}


/* copies a vector, x, to a vector, y. */
/* uses unrolled loops for increments equal to one. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(dcopy) (int *n, double *dx, int *incx,
		   double *dy, int *incy)
{
	int i__1;
	int i, m, ix, iy, mp1;

	--dy;
	--dx;

	if (*n <= 0) {
		return 0;
	}
	if (*incx == 1 && *incy == 1) {
		goto L20;
	}

	/* code for unequal increments or equal increments */
	/* not equal to 1 */

	ix = 1;
	iy = 1;
	if (*incx < 0) {
		ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
		iy = (-(*n) + 1) * *incy + 1;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dy[iy] = dx[ix];
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

	/* clean-up loop */
L20:
	m = *n % 7;
	if (m == 0) {
		goto L40;
	}
	i__1 = m;
	for (i = 1; i <= i__1; ++i) {
		dy[i] = dx[i];
	}
	if (*n < 7) {
		return 0;
	}
L40:
	mp1 = m + 1;
	i__1 = *n;
	for (i = mp1; i <= i__1; i += 7) {
		dy[i] = dx[i];
		dy[i + 1] = dx[i + 1];
		dy[i + 2] = dx[i + 2];
		dy[i + 3] = dx[i + 3];
		dy[i + 4] = dx[i + 4];
		dy[i + 5] = dx[i + 5];
		dy[i + 6] = dx[i + 6];
	}
	return 0;
}


/* forms the dot product of two vectors. */
/* uses unrolled loops for increments equal to one. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

double 
F77_SYMBOL(ddot) (int *n, double *dx, int *incx, double *dy,
		  int *incy)
{
	int i__1;
	double ret_val;
	int i, m;
	double dtemp;
	int ix, iy, mp1;

	--dy;
	--dx;

	ret_val = 0.;
	dtemp = 0.;
	if (*n <= 0) {
		return ret_val;
	}
	if (*incx == 1 && *incy == 1) {
		goto L20;
	}
	/* code for unequal increments or equal increments */
	/* not equal to 1 */

	ix = 1;
	iy = 1;
	if (*incx < 0) {
		ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
		iy = (-(*n) + 1) * *incy + 1;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dtemp += dx[ix] * dy[iy];
		ix += *incx;
		iy += *incy;
	}
	ret_val = dtemp;
	return ret_val;

	/* code for both increments equal to 1 */

	/* clean-up loop */
L20:
	m = *n % 5;
	if (m == 0) {
		goto L40;
	}
	i__1 = m;
	for (i = 1; i <= i__1; ++i) {
		dtemp += dx[i] * dy[i];
	}
	if (*n < 5) {
		goto L60;
	}
L40:
	mp1 = m + 1;
	i__1 = *n;
	for (i = mp1; i <= i__1; i += 5) {
		dtemp = dtemp + dx[i] * dy[i]
			+ dx[i + 1] * dy[i + 1]
			+ dx[i + 2] * dy[i + 2]
			+ dx[i + 3] * dy[i + 3]
			+ dx[i + 4] * dy[i + 4];
	}
L60:
	ret_val = dtemp;
	return ret_val;
}


/* smach computes machine parameters of floating point */
/* arithmetic for use in testing only.  not required by */
/* linpack proper. */

/* if trouble with automatic computation of these quantities, */
/* they can be set by direct assignment statements. */
/* assume the computer has */

/* b = base of arithmetic */
/* t = number of base  b  digits */
/* l = smallest possible exponent */
/* u = largest possible exponent */

/* then */

/* eps = b**(1-t) */
/* tiny = 100.0*b**(-l+t) */
/* huge = 0.01*b**(u-t) */

/* dmach same as smach except t, l, u apply to */
/* double precision. */

/* cmach same as smach except if complex division */
/* is done by */

/* 1/(x+i*y) = (x-i*y)/(x**2+y**2) */

/* then */

/* tiny = sqrt(tiny) */
/* huge = sqrt(huge) */

/* job is 1, 2 or 3 for epsilon, tiny and huge, respectively. */

double 
F77_SYMBOL(dmach) (int *job)
{
	double ret_val;
	double huge, tiny, s, eps;

	eps = 1.;
L10:
	eps /= 2.;
	s = eps + 1.;
	if (s > 1.) {
		goto L10;
	}
	eps *= 2.;

	s = 1.;
L20:
	tiny = s;
	s /= 16.;
	if (s * 1.0f != 0.) {
		goto L20;
	}
	tiny = tiny / eps * 100.0f;
	huge = 1. / tiny;

	if (*job == 1) {
		ret_val = eps;
	}
	if (*job == 2) {
		ret_val = tiny;
	}
	if (*job == 3) {
		ret_val = huge;
	}
	return ret_val;
}


 /* DNRM2 returns the euclidean norm of a vector via the function */
 /* name, so that */
 /* DNRM2 := sqrt( x'*x ) */
 /* -- This version written on 25-October-1982. */
 /* Modified on 14-October-1993 to inline the call to DLASSQ. */
 /* Sven Hammarling, Nag Ltd. */

double 
F77_SYMBOL(dnrm2) (int *n, double *x, int *incx)
{
	int i__1, i__2;
	double ret_val, d__1;
	double sqrt();
	double norm, scale, absxi;
	int ix;
	double ssq;

	--x;

	if (*n < 1 || *incx < 1) {
		norm = 0.;
	} else if (*n == 1) {
		norm = abs(x[1]);
	} else {
		scale = 0.;
		ssq = 1.;

		i__1 = (*n - 1) * *incx + 1;
		i__2 = *incx;
		for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
			if (x[ix] != 0.) {
				absxi = (d__1 = x[ix], abs(d__1));
				if (scale < absxi) {
					d__1 = scale / absxi;
					ssq = ssq * (d__1 * d__1) + 1.;
					scale = absxi;
				} else {
					d__1 = absxi / scale;
					ssq += d__1 * d__1;
				}
			}
		}
		norm = scale * sqrt(ssq);
	}

	ret_val = norm;
	return ret_val;
}


/* applies a plane rotation. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(drot) (int *n, double *dx, int *incx,
		  double *dy, int *incy, double *c, double *s)
{
	int i__1;
	int i;
	double dtemp;
	int ix, iy;


	--dy;
	--dx;

	if (*n <= 0) {
		return 0;
	}
	if (*incx == 1 && *incy == 1) {
		goto L20;
	}

	/* code for unequal increments or equal increments not equal */
	/* to 1 */

	ix = 1;
	iy = 1;
	if (*incx < 0) {
		ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
		iy = (-(*n) + 1) * *incy + 1;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dtemp = *c * dx[ix] + *s * dy[iy];
		dy[iy] = *c * dy[iy] - *s * dx[ix];
		dx[ix] = dtemp;
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dtemp = *c * dx[i] + *s * dy[i];
		dy[i] = *c * dy[i] - *s * dx[i];
		dx[i] = dtemp;
	}
	return 0;
}


/* construct givens plane rotation. */
/* jack dongarra, linpack, 3/11/78. */

static double c_b39 = 1.;

int 
F77_SYMBOL(drotg) (double *da, double *db, double *c,
		   double *s)
{
	double d__1, d__2;
	double sqrt(), DSIGN();
	double r, scale, z, roe;


	roe = *db;
	if (abs(*da) > abs(*db)) {
		roe = *da;
	}
	scale = abs(*da) + abs(*db);
	if (scale != 0.) {
		goto L10;
	}
	*c = 1.;
	*s = 0.;
	r = 0.;
	z = 0.;
	goto L20;
L10:
	d__1 = *da / scale;
	d__2 = *db / scale;
	r = scale * sqrt(d__1 * d__1 + d__2 * d__2);
	r = DSIGN(&c_b39, &roe) * r;
	*c = *da / r;
	*s = *db / r;
	z = 1.;
	if (abs(*da) > abs(*db)) {
		z = *s;
	}
	if (abs(*db) >= abs(*da) && *c != 0.) {
		z = 1. / *c;
	}
L20:
	*da = r;
	*db = z;
	return 0;
}


/* scales a vector by a constant. */
/* uses unrolled loops for increment equal to one. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(dscal) (int *n, double *da, double *dx,
		   int *incx)
{
	int i__1, i__2;
	int i, m, nincx, mp1;

	--dx;

	if (*n <= 0 || *incx <= 0) {
		return 0;
	}
	if (*incx == 1) {
		goto L20;
	}

	/* code for increment not equal to 1 */

	nincx = *n * *incx;
	i__1 = nincx;
	i__2 = *incx;
	for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
		dx[i] = *da * dx[i];
	}
	return 0;

	/* code for increment equal to 1 */

	/* clean-up loop */
L20:
	m = *n % 5;
	if (m == 0) {
		goto L40;
	}
	i__2 = m;
	for (i = 1; i <= i__2; ++i) {
		dx[i] = *da * dx[i];
	}
	if (*n < 5) {
		return 0;
	}
L40:
	mp1 = m + 1;
	i__2 = *n;
	for (i = mp1; i <= i__2; i += 5) {
		dx[i] = *da * dx[i];
		dx[i + 1] = *da * dx[i + 1];
		dx[i + 2] = *da * dx[i + 2];
		dx[i + 3] = *da * dx[i + 3];
		dx[i + 4] = *da * dx[i + 4];
	}
	return 0;
}


/* interchanges two vectors. */
/* uses unrolled loops for increments equal one. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(dswap) (int *n, double *dx, int *incx,
		   double *dy, int *incy)
{
	int i__1;
	int i, m;
	double dtemp;
	int ix, iy, mp1;

	--dy;
	--dx;

	if (*n <= 0) {
		return 0;
	}
	if (*incx == 1 && *incy == 1) {
		goto L20;
	}

	/* code for unequal increments or equal increments not equal */
	/* to 1 */

	ix = 1;
	iy = 1;
	if (*incx < 0) {
		ix = (-(*n) + 1) * *incx + 1;
	}
	if (*incy < 0) {
		iy = (-(*n) + 1) * *incy + 1;
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		dtemp = dx[ix];
		dx[ix] = dy[iy];
		dy[iy] = dtemp;
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

	/* clean-up loop */
L20:
	m = *n % 3;
	if (m == 0) {
		goto L40;
	}
	i__1 = m;
	for (i = 1; i <= i__1; ++i) {
		dtemp = dx[i];
		dx[i] = dy[i];
		dy[i] = dtemp;
	}
	if (*n < 3) {
		return 0;
	}
L40:
	mp1 = m + 1;
	i__1 = *n;
	for (i = mp1; i <= i__1; i += 3) {
		dtemp = dx[i];
		dx[i] = dy[i];
		dy[i] = dtemp;
		dtemp = dx[i + 1];
		dx[i + 1] = dy[i + 1];
		dy[i + 1] = dtemp;
		dtemp = dx[i + 2];
		dx[i + 2] = dy[i + 2];
		dy[i + 2] = dtemp;
	}
	return 0;
}


/* finds the index of element having max. absolute value. */
/* jack dongarra, linpack, 3/11/78. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(idamax) (int *n, double *dx, int *incx)
{
	int ret_val, i__1;
	double d__1;
	double dmax_;
	int i, ix;

	--dx;

	ret_val = 0;
	if (*n < 1 || *incx <= 0) {
		return ret_val;
	}
	ret_val = 1;
	if (*n == 1) {
		return ret_val;
	}
	if (*incx == 1) {
		goto L20;
	}

	/* code for increment not equal to 1 */

	ix = 1;
	dmax_ = abs(dx[1]);
	ix += *incx;
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		if ((d__1 = dx[ix], abs(d__1)) <= dmax_) {
			goto L5;
		}
		ret_val = i;
		dmax_ = (d__1 = dx[ix], abs(d__1));
L5:
		ix += *incx;
	}
	return ret_val;

	/* code for increment equal to 1 */

L20:
	dmax_ = abs(dx[1]);
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		if ((d__1 = dx[i], abs(d__1)) <= dmax_) {
			goto L30;
		}
		ret_val = i;
		dmax_ = (d__1 = dx[i], abs(d__1));
L30:
		;
	}
	return ret_val;
}
