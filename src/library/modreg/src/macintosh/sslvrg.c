/* sslvrg.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 16.;
static integer c__3 = 3;
static integer c__4 = 4;
static integer c__0 = 0;
static integer c__1 = 1;

/* Output from Public domain Ratfor, version 1.0 */
/* Subroutine */ int sslvrg(doublereal *penalt, doublereal *dofoff, 
	doublereal *x, doublereal *y, doublereal *w, doublereal *ssw, integer 
	*n, doublereal *knot, integer *nk, doublereal *coef, doublereal *sz, 
	doublereal *lev, doublereal *crit, integer *icrit, doublereal *spar, 
	doublereal *ratio, doublereal *xwy, doublereal *hs0, doublereal *hs1, 
	doublereal *hs2, doublereal *hs3, doublereal *sg0, doublereal *sg1, 
	doublereal *sg2, doublereal *sg3, doublereal *abd, doublereal *p1ip, 
	doublereal *p2ip, integer *ld4, integer *ldnk, integer *info)
{
    /* System generated locals */
    integer abd_dim1, abd_offset, p1ip_dim1, p1ip_offset, p2ip_dim1, 
	    p2ip_offset, i__1, i__2;
    doublereal d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static doublereal work[16], sumw;
    static integer i__, j;
    extern /* Subroutine */ int dpbfa(doublereal *, integer *, integer *, 
	    integer *, integer *);
    static integer icoef, mflag, ileft;
    extern /* Subroutine */ int dpbsl(doublereal *, integer *, integer *, 
	    integer *, doublereal *);
    static doublereal b0, b1, b2, b3, vnikx[4]	/* was [4][1] */, df, lambda;
    extern doublereal bvalue(doublereal *, integer *, doublereal *, integer *
	    , integer *, doublereal *, integer *);
    static doublereal xv;
    static integer lenkno;
    extern /* Subroutine */ int bsplvd(doublereal *, integer *, integer *, 
	    doublereal *, integer *, doublereal *, doublereal *, integer *), 
	    sinerp(doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *), interv(doublereal *, 
	    integer *, doublereal *, integer *, integer *);
    static integer ilo;
    static doublereal eps, rss;

/*     ------------- */
/* local variables */
    /* Parameter adjustments */
    --lev;
    --sz;
    --w;
    --y;
    --x;
    --sg3;
    --sg2;
    --sg1;
    --sg0;
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --xwy;
    --coef;
    --knot;
    p1ip_dim1 = *ld4;
    p1ip_offset = p1ip_dim1 + 1;
    p1ip -= p1ip_offset;
    abd_dim1 = *ld4;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;
    p2ip_dim1 = *ldnk;
    p2ip_offset = p2ip_dim1 + 1;
    p2ip -= p2ip_offset;

    /* Function Body */
    lenkno = *nk + 4;
    ilo = 1;
    eps = 1e-11;
/* Purpose : Solves the smoothing problem and computes the */
/*           criterion function (OCV or GCV). */
/* The coeficients of estimated smooth */
    d__1 = *spar * 6.f - 2.f;
    lambda = *ratio * pow_dd(&c_b2, &d__1);
    i__1 = *nk;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L1: */
	coef[i__] = xwy[i__];
    }
    i__1 = *nk;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L2: */
	abd[i__ * abd_dim1 + 4] = hs0[i__] + lambda * sg0[i__];
    }
    i__1 = *nk - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	abd[(i__ + 1) * abd_dim1 + 3] = hs1[i__] + lambda * sg1[i__];
/* L4: */
    }
    i__1 = *nk - 2;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L6: */
	abd[(i__ + 2) * abd_dim1 + 2] = hs2[i__] + lambda * sg2[i__];
    }
    i__1 = *nk - 3;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L8: */
	abd[(i__ + 3) * abd_dim1 + 1] = hs3[i__] + lambda * sg3[i__];
    }
    dpbfa(&abd[abd_offset], ld4, nk, &c__3, info);
    if (*info != 0) {
	return 0;
    }
    dpbsl(&abd[abd_offset], ld4, nk, &c__3, &coef[1]);
/*     Value of smooth at the data points */
    icoef = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xv = x[i__];
/* L12: */
	sz[i__] = bvalue(&knot[1], &lenkno, &coef[1], nk, &c__4, &xv, &c__0);
    }
/*     Compute the criterion function if requested */
    if (*icrit == 0) {
	return 0;
    } else {
/*     Ordinary or Generalized CV */
/*     Get Leverages First */
	sinerp(&abd[abd_offset], ld4, nk, &p1ip[p1ip_offset], &p2ip[
		p2ip_offset], ldnk, &c__0);
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    xv = x[i__];
	    i__2 = *nk + 1;
	    interv(&knot[1], &i__2, &xv, &ileft, &mflag);
	    if (mflag == -1) {
		ileft = 4;
		xv = knot[4] + eps;
	    }
	    if (mflag == 1) {
		ileft = *nk;
		xv = knot[*nk + 1] - eps;
	    }
	    j = ileft - 3;
/*     call bspvd(knot,4,1,xv,ileft,4,vnikx,work) */
	    bsplvd(&knot[1], &lenkno, &c__4, &xv, &ileft, work, vnikx, &c__1)
		    ;
	    b0 = vnikx[0];
	    b1 = vnikx[1];
	    b2 = vnikx[2];
	    b3 = vnikx[3];
/* Computing 2nd power */
	    d__1 = b0;
/* Computing 2nd power */
	    d__2 = b1;
/* Computing 2nd power */
	    d__3 = b2;
/* Computing 2nd power */
	    d__4 = b3;
/* Computing 2nd power */
	    d__5 = w[i__];
	    lev[i__] = (p1ip[j * p1ip_dim1 + 4] * (d__1 * d__1) + p1ip[j * 
		    p1ip_dim1 + 3] * 2.f * b0 * b1 + p1ip[j * p1ip_dim1 + 2] *
		     2.f * b0 * b2 + p1ip[j * p1ip_dim1 + 1] * 2.f * b0 * b3 
		    + p1ip[(j + 1) * p1ip_dim1 + 4] * (d__2 * d__2) + p1ip[(j 
		    + 1) * p1ip_dim1 + 3] * 2.f * b1 * b2 + p1ip[(j + 1) * 
		    p1ip_dim1 + 2] * 2.f * b1 * b3 + p1ip[(j + 2) * p1ip_dim1 
		    + 4] * (d__3 * d__3) + p1ip[(j + 2) * p1ip_dim1 + 3] * 
		    2.f * b2 * b3 + p1ip[(j + 3) * p1ip_dim1 + 4] * (d__4 * 
		    d__4)) * (d__5 * d__5);
/* L16: */
	}
/*     Evaluate Criterion */
	if (*icrit == 1) {
/*     Generalized CV */
	    rss = *ssw;
	    df = 0.;
	    sumw = 0.;
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing 2nd power */
		d__1 = (y[i__] - sz[i__]) * w[i__];
		rss += d__1 * d__1;
/* L24: */
	    }
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L26: */
		df += lev[i__];
	    }
/* Here w(i) is the square root of the weights */
	    i__1 = *n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
/* L28: */
/* Computing 2nd power */
		d__1 = w[i__];
		sumw += d__1 * d__1;
	    }
/* scaling made sumw the total number of observations */
/* Computing 2nd power */
	    d__1 = 1. - (*dofoff + *penalt * df) / sumw;
	    *crit = rss / sumw / (d__1 * d__1);
/*            call dblepr("spar", 4, spar, 1) */
/*            call dblepr("crit", 4, crit, 1) */
	} else {
	    if (*icrit == 2) {
/*     Ordinary CV */
		*crit = 0.;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* L30: */
/* Computing 2nd power */
		    d__1 = (y[i__] - sz[i__]) * w[i__] / (1 - lev[i__]);
		    *crit += d__1 * d__1;
		}
		*crit /= *n;
/*            call dblepr("spar", 4, spar, 1) */
/*            call dblepr("crit", 4, crit, 1) */
	    } else {
/*     df matching */
		*crit = 0.;
		i__1 = *n;
		for (i__ = 1; i__ <= i__1; ++i__) {
/* L32: */
		    *crit += lev[i__];
		}
/* Computing 2nd power */
		d__1 = *dofoff - *crit;
		*crit = d__1 * d__1 + 3;
	    }
	}
	return 0;
    }
    return 0;
} /* sslvrg_ */
