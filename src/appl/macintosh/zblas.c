/* zblas.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"


/*     takes the sum of the absolute values. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

doublereal dzasum(integer *n, doublecomplex *zx, integer *incx)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Builtin functions */
    double z_abs(doublecomplex *);

    /* Local variables */
    static integer i__;
    static doublereal stemp;
    static integer ix;


    /* Parameter adjustments */
    --zx;

    /* Function Body */
    ret_val = 0.;
    stemp = 0.;
    if (*n <= 0 || *incx <= 0) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    ix = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	stemp += z_abs(&zx[ix]);
	ix += *incx;
/* L10: */
    }
    ret_val = stemp;
    return ret_val;

/*        code for increment equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	stemp += z_abs(&zx[i__]);
/* L30: */
    }
    ret_val = stemp;
    return ret_val;
} /* dzasum_ */


/*  DZNRM2 returns the euclidean norm of a vector via the function */
/*  name, so that */

/*     DZNRM2 := sqrt( conjg( x' )*x ) */



/*  -- This version written on 25-October-1982. */
/*     Modified on 14-October-1993 to inline the call to ZLASSQ. */
/*     Sven Hammarling, Nag Ltd. */


doublereal dznrm2(integer *n, doublecomplex *x, integer *incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_imag(doublecomplex *), sqrt(doublereal);

    /* Local variables */
    static doublereal temp, norm, scale;
    static integer ix;
    static doublereal ssq;

/*     .. Scalar Arguments .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Parameters .. */
/*     .. Local Scalars .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Executable Statements .. */
    /* Parameter adjustments */
    --x;

    /* Function Body */
    if (*n < 1 || *incx < 1) {
	norm = 0.;
    } else {
	scale = 0.;
	ssq = 1.;
/*        The following loop is equivalent to this call to the LAPACK */
/*        auxiliary routine: */
/*        CALL ZLASSQ( N, X, INCX, SCALE, SSQ ) */

	i__1 = (*n - 1) * *incx + 1;
	i__2 = *incx;
	for (ix = 1; i__2 < 0 ? ix >= i__1 : ix <= i__1; ix += i__2) {
	    i__3 = ix;
	    if (x[i__3].r != 0.) {
		i__3 = ix;
		temp = (d__1 = x[i__3].r, abs(d__1));
		if (scale < temp) {
/* Computing 2nd power */
		    d__1 = scale / temp;
		    ssq = ssq * (d__1 * d__1) + 1.;
		    scale = temp;
		} else {
/* Computing 2nd power */
		    d__1 = temp / scale;
		    ssq += d__1 * d__1;
		}
	    }
	    if (d_imag(&x[ix]) != 0.) {
		temp = (d__1 = d_imag(&x[ix]), abs(d__1));
		if (scale < temp) {
/* Computing 2nd power */
		    d__1 = scale / temp;
		    ssq = ssq * (d__1 * d__1) + 1.;
		    scale = temp;
		} else {
/* Computing 2nd power */
		    d__1 = temp / scale;
		    ssq += d__1 * d__1;
		}
	    }
/* L10: */
	}
	norm = scale * sqrt(ssq);
    }

    ret_val = norm;
    return ret_val;
} /* dznrm2_ */


/*     finds the index of element having max. absolute value. */
/*     jack dongarra, 1/15/85. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

integer izamax(integer *n, doublecomplex *zx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    double z_abs(doublecomplex *);

    /* Local variables */
    static doublereal smax;
    static integer i__, ix;


    /* Parameter adjustments */
    --zx;

    /* Function Body */
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

/*        code for increment not equal to 1 */

    ix = 1;
    smax = z_abs(&zx[1]);
    ix += *incx;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (z_abs(&zx[ix]) <= smax) {
	    goto L5;
	}
	ret_val = i__;
	smax = z_abs(&zx[ix]);
L5:
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*        code for increment equal to 1 */

L20:
    smax = z_abs(&zx[1]);
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (z_abs(&zx[i__]) <= smax) {
	    goto L30;
	}
	ret_val = i__;
	smax = z_abs(&zx[i__]);
L30:
	;
    }
    return ret_val;
} /* izamax_ */


/*     constant times a vector plus a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Subroutine */ int zaxpy(integer *n, doublecomplex *za, doublecomplex *zx, 
	integer *incx, doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double z_abs(doublecomplex *);

    /* Local variables */
    static integer i__, ix, iy;

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (z_abs(za) == 0.) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = iy;
	i__3 = iy;
	i__4 = ix;
	z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
		i__4].i + za->i * zx[i__4].r;
	z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	i__3 = i__;
	i__4 = i__;
	z__2.r = za->r * zx[i__4].r - za->i * zx[i__4].i, z__2.i = za->r * zx[
		i__4].i + za->i * zx[i__4].r;
	z__1.r = zy[i__3].r + z__2.r, z__1.i = zy[i__3].i + z__2.i;
	zy[i__2].r = z__1.r, zy[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* zaxpy_ */


/*     copies a vector, x, to a vector, y. */
/*     jack dongarra, linpack, 4/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Subroutine */ int zcopy(integer *n, doublecomplex *zx, integer *incx, 
	doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i__, ix, iy;


    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = iy;
	i__3 = ix;
	zy[i__2].r = zx[i__3].r, zy[i__2].i = zx[i__3].i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	i__3 = i__;
	zy[i__2].r = zx[i__3].r, zy[i__2].i = zx[i__3].i;
/* L30: */
    }
    return 0;
} /* zcopy_ */


/*     forms the dot product of a vector. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Double Complex */ VOID zdotc(doublecomplex * ret_val, integer *n, 
	doublecomplex *zx, integer *incx, doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer i__;
    static doublecomplex ztemp;
    static integer ix, iy;

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    ztemp.r = 0., ztemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d_cnjg(&z__3, &zx[ix]);
	i__2 = iy;
	z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i, z__2.i = z__3.r * 
		zy[i__2].i + z__3.i * zy[i__2].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	d_cnjg(&z__3, &zx[i__]);
	i__2 = i__;
	z__2.r = z__3.r * zy[i__2].r - z__3.i * zy[i__2].i, z__2.i = z__3.r * 
		zy[i__2].i + z__3.i * zy[i__2].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;
} /* zdotc_ */


/*     forms the dot product of two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Double Complex */ VOID zdotu(doublecomplex * ret_val, integer *n, 
	doublecomplex *zx, integer *incx, doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i__;
    static doublecomplex ztemp;
    static integer ix, iy;

    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    ztemp.r = 0., ztemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        code for unequal increments or equal increments */
/*          not equal to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = ix;
	i__3 = iy;
	z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i, z__2.i = 
		zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;

/*        code for both increments equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	i__3 = i__;
	z__2.r = zx[i__2].r * zy[i__3].r - zx[i__2].i * zy[i__3].i, z__2.i = 
		zx[i__2].r * zy[i__3].i + zx[i__2].i * zy[i__3].r;
	z__1.r = ztemp.r + z__2.r, z__1.i = ztemp.i + z__2.i;
	ztemp.r = z__1.r, ztemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ztemp.r,  ret_val->i = ztemp.i;
    return ;
} /* zdotu_ */


/*     scales a vector by a constant. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Subroutine */ int zdscal(integer *n, doublereal *da, doublecomplex *zx, 
	integer *incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i__, ix;


    /* Parameter adjustments */
    --zx;

    /* Function Body */
    if (*n <= 0 || *incx <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    ix = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = ix;
	z__2.r = *da, z__2.i = 0.;
	i__3 = ix;
	z__1.r = z__2.r * zx[i__3].r - z__2.i * zx[i__3].i, z__1.i = z__2.r * 
		zx[i__3].i + z__2.i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
	ix += *incx;
/* L10: */
    }
    return 0;

/*        code for increment equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	z__2.r = *da, z__2.i = 0.;
	i__3 = i__;
	z__1.r = z__2.r * zx[i__3].r - z__2.i * zx[i__3].i, z__1.i = z__2.r * 
		zx[i__3].i + z__2.i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* zdscal_ */

/* Subroutine */ int zrotg(doublecomplex *ca, doublecomplex *cb, doublereal *
	c__, doublecomplex *s)
{
    /* System generated locals */
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double z_abs(doublecomplex *);
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *);
    double sqrt(doublereal);
    void d_cnjg(doublecomplex *, doublecomplex *);

    /* Local variables */
    static doublereal norm;
    static doublecomplex alpha;
    static doublereal scale;

    if (z_abs(ca) != 0.) {
	goto L10;
    }
    *c__ = 0.;
    s->r = 1., s->i = 0.;
    ca->r = cb->r, ca->i = cb->i;
    goto L20;
L10:
    scale = z_abs(ca) + z_abs(cb);
    z__2.r = scale, z__2.i = 0.;
    z_div(&z__1, ca, &z__2);
/* Computing 2nd power */
    d__1 = z_abs(&z__1);
    z__4.r = scale, z__4.i = 0.;
    z_div(&z__3, cb, &z__4);
/* Computing 2nd power */
    d__2 = z_abs(&z__3);
    norm = scale * sqrt(d__1 * d__1 + d__2 * d__2);
    d__1 = z_abs(ca);
    z__1.r = ca->r / d__1, z__1.i = ca->i / d__1;
    alpha.r = z__1.r, alpha.i = z__1.i;
    *c__ = z_abs(ca) / norm;
    d_cnjg(&z__3, cb);
    z__2.r = alpha.r * z__3.r - alpha.i * z__3.i, z__2.i = alpha.r * z__3.i + 
	    alpha.i * z__3.r;
    z__1.r = z__2.r / norm, z__1.i = z__2.i / norm;
    s->r = z__1.r, s->i = z__1.i;
    z__1.r = norm * alpha.r, z__1.i = norm * alpha.i;
    ca->r = z__1.r, ca->i = z__1.i;
L20:
    return 0;
} /* zrotg_ */


/*     scales a vector by a constant. */
/*     jack dongarra, 3/11/78. */
/*     modified 3/93 to return if incx .le. 0. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Subroutine */ int zscal(integer *n, doublecomplex *za, doublecomplex *zx, 
	integer *incx)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1;

    /* Local variables */
    static integer i__, ix;


    /* Parameter adjustments */
    --zx;

    /* Function Body */
    if (*n <= 0 || *incx <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        code for increment not equal to 1 */

    ix = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = ix;
	i__3 = ix;
	z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i, z__1.i = za->r * zx[
		i__3].i + za->i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
	ix += *incx;
/* L10: */
    }
    return 0;

/*        code for increment equal to 1 */

L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	i__3 = i__;
	z__1.r = za->r * zx[i__3].r - za->i * zx[i__3].i, z__1.i = za->r * zx[
		i__3].i + za->i * zx[i__3].r;
	zx[i__2].r = z__1.r, zx[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* zscal_ */


/*     interchanges two vectors. */
/*     jack dongarra, 3/11/78. */
/*     modified 12/3/93, array(1) declarations changed to array(*) */

/* Subroutine */ int zswap(integer *n, doublecomplex *zx, integer *incx, 
	doublecomplex *zy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i__;
    static doublecomplex ztemp;
    static integer ix, iy;


    /* Parameter adjustments */
    --zy;
    --zx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       code for unequal increments or equal increments not equal */
/*         to 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = ix;
	ztemp.r = zx[i__2].r, ztemp.i = zx[i__2].i;
	i__2 = ix;
	i__3 = iy;
	zx[i__2].r = zy[i__3].r, zx[i__2].i = zy[i__3].i;
	i__2 = iy;
	zy[i__2].r = ztemp.r, zy[i__2].i = ztemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       code for both increments equal to 1 */
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	ztemp.r = zx[i__2].r, ztemp.i = zx[i__2].i;
	i__2 = i__;
	i__3 = i__;
	zx[i__2].r = zy[i__3].r, zx[i__2].i = zy[i__3].i;
	i__2 = i__;
	zy[i__2].r = ztemp.r, zy[i__2].i = ztemp.i;
/* L30: */
    }
    return 0;
} /* zswap_ */
