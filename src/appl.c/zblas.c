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

/* dznrm2 returns the euclidean norm of a vector via the function */
/* name, so that */
/* dznrm2 := sqrt( conjg( x' )*x ) */
/* -- this version written on 25-october-1982. */
/* modified on 14-october-1993 to inline the call to zlassq. */
/* sven hammarling, nag ltd. */

double 
F77_SYMBOL(dznrm2)(int *n, complex * x, int *incx)
{
	int i__1, i__2, i__3;
	double ret_val, d__1;
	double ZIMAG(), sqrt();
	double temp, norm, scale;
	int ix;
	double ssq;

	--x;

	if (*n < 1 || *incx < 1) {
		norm = 0.;
	} else {
		scale = 0.;
		ssq = 1.;

		i__1 = (*n - 1) * *incx + 1;
		i__2 = *incx;
		for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
			i__3 = ix;
			if (x[i__3].r != 0.) {
				i__3 = ix;
				temp = (d__1 = x[i__3].r, abs(d__1));
				if (scale < temp) {
					d__1 = scale / temp;
					ssq = ssq * (d__1 * d__1) + 1.;
					scale = temp;
				} else {
					d__1 = temp / scale;
					ssq += d__1 * d__1;
				}
			}
			if (ZIMAG(&x[ix]) != 0.) {
				temp = (d__1 = ZIMAG(&x[ix]), abs(d__1));
				if (scale < temp) {
					d__1 = scale / temp;
					ssq = ssq * (d__1 * d__1) + 1.;
					scale = temp;
				} else {
					d__1 = temp / scale;
					ssq += d__1 * d__1;
				}
			}
		}
		norm = scale * sqrt(ssq);
	}

	ret_val = norm;
	return ret_val;
}


/* finds the index of element having max. absolute value. */
/* jack dongarra, 1/15/85. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(izamax) (int *n, complex * zx, int *incx)
{
	int ret_val, i__1;
	double ZABS();
	double smax;
	int i, ix;

	--zx;

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
	smax = ZABS(&zx[1]);
	ix += *incx;
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		if (ZABS(&zx[ix]) <= smax) {
			goto L5;
		}
		ret_val = i;
		smax = ZABS(&zx[ix]);
L5:
		ix += *incx;
	}
	return ret_val;

	/* code for increment equal to 1 */

L20:
	smax = ZABS(&zx[1]);
	i__1 = *n;
	for (i = 2; i <= i__1; ++i) {
		if (ZABS(&zx[i]) <= smax) {
			goto L30;
		}
		ret_val = i;
		smax = ZABS(&zx[i]);
L30:
		;
	}
	return ret_val;
}


/* constant times a vector plus a vector. */
/* jack dongarra, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(zaxpy) (int *n, complex * za, complex * zx,
		   int *incx, complex * zy, int *incy)
{
	int i__1, i__2, i__3, i__4;
	complex z__1, z__2;
	double ZABS();
	int i, ix, iy;

	--zy;
	--zx;

	if (*n <= 0) {
		return 0;
	}
	if (ZABS(za) == 0.) {
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
		i__2 = iy;
		i__3 = iy;
		i__4 = ix;
		z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i;
		z__2.i = za->r * zx[i__4].i + za->i * zx[i__4].r;
		z__1.r = zy[i__3].r + z__2.r;
		z__1.i = zy[i__3].i + z__2.i;
		zy[i__2].r = z__1.r;
		zy[i__2].i = z__1.i;
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		i__3 = i;
		i__4 = i;
		z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i;
		z__2.i = za->r * zx[i__4].i + za->i * zx[i__4].r;
		z__1.r = zy[i__3].r + z__2.r;
		z__1.i = zy[i__3].i + z__2.i;
		zy[i__2].r = z__1.r;
		zy[i__2].i = z__1.i;
	}
	return 0;
}


/* copies a vector, x, to a vector, y. */
/* jack dongarra, linpack, 4/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(zcopy) (int *n, complex * zx, int *incx,
		   complex * zy, int *incy)
{
	int i__1, i__2, i__3;
	int i, ix, iy;

	--zy;
	--zx;

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
		i__2 = iy;
		i__3 = ix;
		zy[i__2].r = zx[i__3].r;
		zy[i__2].i = zx[i__3].i;
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		i__3 = i;
		zy[i__2].r = zx[i__3].r;
		zy[i__2].i = zx[i__3].i;
	}
	return 0;
}


/* forms the dot product of a vector. */
/* jack dongarra, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

/* Double Complex */ void 
F77_SYMBOL(zdotc) (complex * ret_val, int *n,
		   complex * zx, int *incx, complex * zy, int *incy)
{
	int i__1, i__2;
	complex z__1, z__2, z__3;
	void ZCNJG();
	int i;
	complex ztemp;
	int ix, iy;

	--zy;
	--zx;

	ztemp.r = 0., ztemp.i = 0.;
	ret_val->r = 0., ret_val->i = 0.;
	if (*n <= 0) {
		return;
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
		ZCNJG(&z__3, &zx[ix]);
		i__2 = iy;
		z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i;
		z__2.i = z__3.r * zy[i__2].i + z__3.i * zy[i__2].r;
		z__1.r = ztemp.r + z__2.r;
		z__1.i = ztemp.i + z__2.i;
		ztemp.r = z__1.r;
		ztemp.i = z__1.i;
		ix += *incx;
		iy += *incy;
	}
	ret_val->r = ztemp.r;
	ret_val->i = ztemp.i;
	return;

	/* code for both increments equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		ZCNJG(&z__3, &zx[i]);
		i__2 = i;
		z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i;
		z__2.i = z__3.r * zy[i__2].i + z__3.i * zy[i__2].r;
		z__1.r = ztemp.r + z__2.r;
		z__1.i = ztemp.i + z__2.i;
		ztemp.r = z__1.r;
		ztemp.i = z__1.i;
	}
	ret_val->r = ztemp.r;
	ret_val->i = ztemp.i;
	return;
}


/* forms the dot product of two vectors. */
/* jack dongarra, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

/* Double Complex */ void 
F77_SYMBOL(zdotu) (complex * ret_val, int *n,
		   complex * zx, int *incx, complex * zy, int *incy)
{
	int i__1, i__2, i__3;
	complex z__1, z__2;
	int i;
	complex ztemp;
	int ix, iy;

	--zy;
	--zx;

	ztemp.r = 0.;
	ztemp.i = 0.;
	ret_val->r = 0.;
	ret_val->i = 0.;
	if (*n <= 0) {
		return;
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
		i__2 = ix;
		i__3 = iy;
		z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i;
		z__2.i = zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
		z__1.r = ztemp.r + z__2.r;
		z__1.i = ztemp.i + z__2.i;
		ztemp.r = z__1.r;
		ztemp.i = z__1.i;
		ix += *incx;
		iy += *incy;
	}
	ret_val->r = ztemp.r, ret_val->i = ztemp.i;
	return;

	/* code for both increments equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		i__3 = i;
		z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i;
		z__2.i = zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
		z__1.r = ztemp.r + z__2.r;
		z__1.i = ztemp.i + z__2.i;
		ztemp.r = z__1.r;
		ztemp.i = z__1.i;
	}
	ret_val->r = ztemp.r;
	ret_val->i = ztemp.i;
	return;
}


/* scales a vector by a constant. */
/* jack dongarra, 3/11/78. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(zdscal) (int *n, double *da, complex * zx,
		    int *incx)
{
	int i__1, i__2, i__3;
	double d__1;
	complex q__1;
	complex z__1;
	int i, ix;

	--zx;

	if (*n <= 0 || *incx <= 0) {
		return 0;
	}
	if (*incx == 1) {
		goto L20;
	}

	/* code for increment not equal to 1 */

	ix = 1;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = ix;
		d__1 = *da;
		q__1.r = d__1;
		q__1.i = 0.0f;
		i__3 = ix;
		z__1.r = q__1.r * zx[i__3].r - q__1.i * zx[i__3].i;
		z__1.i = q__1.r * zx[i__3].i + q__1.i * zx[i__3].r;
		zx[i__2].r = z__1.r;
		zx[i__2].i = z__1.i;
		ix += *incx;
	}
	return 0;

	/* code for increment equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		d__1 = *da;
		q__1.r = d__1;
		q__1.i = 0.0f;
		i__3 = i;
		z__1.r = q__1.r * zx[i__3].r - q__1.i * zx[i__3].i;
		z__1.i = q__1.r * zx[i__3].i + q__1.i * zx[i__3].r;
		zx[i__2].r = z__1.r;
		zx[i__2].i = z__1.i;
	}
	return 0;
}

int 
F77_SYMBOL(zrotg) (complex * ca, complex * cb, double *
		   c, complex * s)
{
	double d__1, d__2, d__3, d__4;
	complex q__1, q__2;
	complex z__1, z__2, z__3, z__4;
	double ZABS();
	void ZDIV();
	double sqrt();
	void ZCNJG();
	double norm;
	complex alpha;
	double scale;

	if (ZABS(ca) != 0.) {
		goto L10;
	}
	*c = 0.;
	s->r = 1.;
	s->i = 0.;
	ca->r = cb->r;
	ca->i = cb->i;
	goto L20;
L10:
	scale = ZABS(ca) + ZABS(cb);
	d__2 = scale;
	q__1.r = d__2;
	q__1.i = 0.0f;
	z__2.r = q__1.r;
	z__2.i = q__1.i;
	ZDIV(&z__1, ca, &z__2);
	d__1 = ZABS(&z__1);
	d__4 = scale;
	q__2.r = d__4;
	q__2.i = 0.0f;
	z__4.r = q__2.r;
	z__4.i = q__2.i;
	ZDIV(&z__3, cb, &z__4);
	d__3 = ZABS(&z__3);
	norm = scale * sqrt(d__1 * d__1 + d__3 * d__3);
	d__1 = ZABS(ca);
	z__1.r = ca->r / d__1;
	z__1.i = ca->i / d__1;
	alpha.r = z__1.r;
	alpha.i = z__1.i;
	*c = ZABS(ca) / norm;
	ZCNJG(&z__3, cb);
	z__2.r = alpha.r * z__3.r - alpha.i * z__3.i;
	z__2.i = alpha.r * z__3.i + alpha.i * z__3.r;
	z__1.r = z__2.r / norm;
	z__1.i = z__2.i / norm;
	s->r = z__1.r;
	s->i = z__1.i;
	z__1.r = norm * alpha.r;
	z__1.i = norm * alpha.i;
	ca->r = z__1.r;
	ca->i = z__1.i;
L20:
	return 0;
}


/* scales a vector by a constant. */
/* jack dongarra, 3/11/78. */
/* modified 3/93 to return if incx .le. 0. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(zscal) (int *n, complex * za, complex * zx,
		   int *incx)
{
	int i__1, i__2, i__3;
	complex z__1;
	int i, ix;

	--zx;

	if (*n <= 0 || *incx <= 0) {
		return 0;
	}
	if (*incx == 1) {
		goto L20;
	}
	/* code for increment not equal to 1 */

	ix = 1;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = ix;
		i__3 = ix;
		z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i;
		z__1.i = za->r * zx[i__3].i + za->i * zx[i__3].r;
		zx[i__2].r = z__1.r;
		zx[i__2].i = z__1.i;
		ix += *incx;
	}
	return 0;

	/* code for increment equal to 1 */

L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		i__3 = i;
		z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i;
		z__1.i = za->r * zx[i__3].i + za->i * zx[i__3].r;
		zx[i__2].r = z__1.r;
		zx[i__2].i = z__1.i;
	}
	return 0;
}


/* interchanges two vectors. */
/* jack dongarra, 3/11/78. */
/* modified 12/3/93, array(1) declarations changed to array(*) */

int 
F77_SYMBOL(zswap) (int *n, complex * zx, int *incx,
		   complex * zy, int *incy)
{
	int i__1, i__2, i__3;

	int i;
	complex ztemp;
	int ix, iy;


	--zy;
	--zx;

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
		i__2 = ix;
		ztemp.r = zx[i__2].r;
		ztemp.i = zx[i__2].i;
		i__2 = ix;
		i__3 = iy;
		zx[i__2].r = zy[i__3].r;
		zx[i__2].i = zy[i__3].i;
		i__2 = iy;
		zy[i__2].r = ztemp.r;
		zy[i__2].i = ztemp.i;
		ix += *incx;
		iy += *incy;
	}
	return 0;

	/* code for both increments equal to 1 */
L20:
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = i;
		ztemp.r = zx[i__2].r;
		ztemp.i = zx[i__2].i;
		i__2 = i;
		i__3 = i;
		zx[i__2].r = zy[i__3].r;
		zx[i__2].i = zy[i__3].i;
		i__2 = i;
		zy[i__2].r = ztemp.r;
		zy[i__2].i = ztemp.i;
	}
	return 0;
}
