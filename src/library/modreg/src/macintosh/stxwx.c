/* stxwx.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__1 = 1;

/* Output from Public domain Ratfor, version 1.0 */
/* Subroutine */ int stxwx(doublereal *x, doublereal *z__, doublereal *w, 
	integer *k, doublereal *xknot, integer *n, doublereal *y, doublereal *
	hs0, doublereal *hs1, doublereal *hs2, doublereal *hs3)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal work[16];
    static integer i__, j, mflag, ileft, lenxk;
    static doublereal vnikx[4]	/* was [4][1] */;
    extern /* Subroutine */ int bsplvd(doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *), 
	    interv(doublereal *, integer *, doublereal *, integer *, integer 
	    *);
    static integer ilo;
    static doublereal eps;

/* local */
    /* Parameter adjustments */
    --w;
    --z__;
    --x;
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --y;
    --xknot;

    /* Function Body */
    lenxk = *n + 4;
/*     Initialise the output vectors */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	y[i__] = 0.;
	hs0[i__] = 0.;
	hs1[i__] = 0.;
	hs2[i__] = 0.;
	hs3[i__] = 0.;
/* L1: */
    }
/*     Compute X'WX -> hs0,hs1,hs2,hs3  and X'WZ -> y */
    ilo = 1;
    eps = 1e-10;
    i__1 = *k;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* 	 call intrv (xknot(1),(n+1),x(i),ilo,ileft,mflag) */
	i__2 = *n + 1;
	interv(&xknot[1], &i__2, &x[i__], &ileft, &mflag);
/*        if(mflag==-1) {write(6,'("Error in hess ",i2)')mflag;stop} */
/*        if(mflag==-1) {return} */
	if (mflag == 1) {
	    if (x[i__] <= xknot[ileft] + eps) {
		--ileft;
	    } else {
		return 0;
	    }
/*        else{write(6,'("Error in hess ",i2)')mflag;stop}} */
	}
/* 	 call bspvd (xknot,4,1,x(i),ileft,4,vnikx,work) */
	bsplvd(&xknot[1], &lenxk, &c__4, &x[i__], &ileft, work, vnikx, &c__1)
		;
/*     check 2 */
/*     print 2999;2999 format(" got through check2 stxwx ") */
	j = ileft - 3;
/* Computing 2nd power */
	d__1 = w[i__];
	y[j] += d__1 * d__1 * z__[i__] * vnikx[0];
/* Computing 2nd power */
	d__1 = w[i__];
/* Computing 2nd power */
	d__2 = vnikx[0];
	hs0[j] += d__1 * d__1 * (d__2 * d__2);
/* Computing 2nd power */
	d__1 = w[i__];
	hs1[j] += d__1 * d__1 * vnikx[0] * vnikx[1];
/* Computing 2nd power */
	d__1 = w[i__];
	hs2[j] += d__1 * d__1 * vnikx[0] * vnikx[2];
/* Computing 2nd power */
	d__1 = w[i__];
	hs3[j] += d__1 * d__1 * vnikx[0] * vnikx[3];
	j = ileft - 2;
/* Computing 2nd power */
	d__1 = w[i__];
	y[j] += d__1 * d__1 * z__[i__] * vnikx[1];
/* Computing 2nd power */
	d__1 = w[i__];
/* Computing 2nd power */
	d__2 = vnikx[1];
	hs0[j] += d__1 * d__1 * (d__2 * d__2);
/* Computing 2nd power */
	d__1 = w[i__];
	hs1[j] += d__1 * d__1 * vnikx[1] * vnikx[2];
/* Computing 2nd power */
	d__1 = w[i__];
	hs2[j] += d__1 * d__1 * vnikx[1] * vnikx[3];
	j = ileft - 1;
/* Computing 2nd power */
	d__1 = w[i__];
	y[j] += d__1 * d__1 * z__[i__] * vnikx[2];
/* Computing 2nd power */
	d__1 = w[i__];
/* Computing 2nd power */
	d__2 = vnikx[2];
	hs0[j] += d__1 * d__1 * (d__2 * d__2);
/* Computing 2nd power */
	d__1 = w[i__];
	hs1[j] += d__1 * d__1 * vnikx[2] * vnikx[3];
	j = ileft;
/* Computing 2nd power */
	d__1 = w[i__];
	y[j] += d__1 * d__1 * z__[i__] * vnikx[3];
/* Computing 2nd power */
	d__1 = w[i__];
/* Computing 2nd power */
	d__2 = vnikx[3];
	hs0[j] += d__1 * d__1 * (d__2 * d__2);
/* L100: */
    }
    return 0;
} /* stxwx_ */
