/* sgram.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__3 = 3;

/* Output from Public domain Ratfor, version 1.0 */
/* PURPOSE */
/* 	Calculation of the cubic B-spline smoothness prior */
/* 	for "usual" interior knot setup. */
/* 	Uses BSPVD and INTRV in the CMLIB */
/* 	sgm[0-3](nb)    Symmetric matrix */
/*                       whose (i,j)'th element contains the integral of */
/*                       B''(i,.) B''(j,.) , i=1,2 ... nb and j=i,...nb. */
/*                       Only the upper four diagonals are computed. */
/* Subroutine */ int sgram(doublereal *sg0, doublereal *sg1, doublereal *sg2,
	 doublereal *sg3, doublereal *tb, integer *nb)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static doublereal work[16];
    static integer i__, mflag, ileft, lentb;
    static doublereal vnikx[12]	/* was [4][3] */;
    static integer ii, jj;
    extern /* Subroutine */ int bsplvd(doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *), 
	    interv(doublereal *, integer *, doublereal *, integer *, integer 
	    *);
    static doublereal yw1[4], yw2[4];
    static integer ilo;
    static doublereal wpt;

/* indices */
/*     ------------- */
    /* Parameter adjustments */
    --tb;
    --sg3;
    --sg2;
    --sg1;
    --sg0;

    /* Function Body */
    lentb = *nb + 4;
/* Initialise the sigma vectors */
    i__1 = *nb;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sg0[i__] = 0.f;
	sg1[i__] = 0.f;
	sg2[i__] = 0.f;
	sg3[i__] = 0.f;
/* L1: */
    }
    ilo = 1;
    i__1 = *nb;
    for (i__ = 1; i__ <= i__1; ++i__) {
/*     Calculate a linear approximation to the */
/*     second derivative of the non-zero B-splines */
/*     over the interval [tb(i),tb(i+1)]. */
/*     call intrv(tb(1),(nb+1),tb(i),ilo,ileft,mflag) */
	i__2 = *nb + 1;
	interv(&tb[1], &i__2, &tb[i__], &ileft, &mflag);
/*     Left end second derivatives */
/*     call bspvd (tb,4,3,tb(i),ileft,4,vnikx,work) */
	bsplvd(&tb[1], &lentb, &c__4, &tb[i__], &ileft, work, vnikx, &c__3);
/*     Put values into yw1 */
	for (ii = 1; ii <= 4; ++ii) {
	    yw1[ii - 1] = vnikx[ii + 7];
/* L4: */
	}
/*     Right end second derivatives */
/*     call bspvd (tb,4,3,tb(i+1),ileft,4,vnikx,work) */
	bsplvd(&tb[1], &lentb, &c__4, &tb[i__ + 1], &ileft, work, vnikx, &
		c__3);
/*     Slope*(length of interval) in Linear Approximation to B'' */
	for (ii = 1; ii <= 4; ++ii) {
	    yw2[ii - 1] = vnikx[ii + 7] - yw1[ii - 1];
/* L6: */
	}
	wpt = tb[i__ + 1] - tb[i__];
/*     Calculate Contributions to the simga vectors */
	if (ileft >= 4) {
	    for (ii = 1; ii <= 4; ++ii) {
		jj = ii;
		sg0[ileft - 4 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (
			yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1])
			 * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		jj = ii + 1;
		if (jj <= 4) {
		    sg1[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
		jj = ii + 2;
		if (jj <= 4) {
		    sg2[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
		jj = ii + 3;
		if (jj <= 4) {
		    sg3[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
/* L10: */
	    }
	} else if (ileft == 3) {
	    for (ii = 1; ii <= 3; ++ii) {
		jj = ii;
		sg0[ileft - 3 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (
			yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1])
			 * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		jj = ii + 1;
		if (jj <= 3) {
		    sg1[ileft + ii - 3] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
		jj = ii + 2;
		if (jj <= 3) {
		    sg2[ileft + ii - 3] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
/* L20: */
	    }
	} else if (ileft == 2) {
	    for (ii = 1; ii <= 2; ++ii) {
		jj = ii;
		sg0[ileft - 2 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (
			yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1])
			 * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		jj = ii + 1;
		if (jj <= 2) {
		    sg1[ileft + ii - 2] += wpt * (yw1[ii - 1] * yw1[jj - 1] + 
			    (yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii 
			    - 1]) * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
		}
/* L28: */
	    }
	} else if (ileft == 1) {
	    for (ii = 1; ii <= 1; ++ii) {
		jj = ii;
		sg0[ileft - 1 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (
			yw2[ii - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1])
			 * .5f + yw2[ii - 1] * yw2[jj - 1] * .333f);
/* L34: */
	    }
	}
/* L2: */
    }
    return 0;
} /* sgram_ */
