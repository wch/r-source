/* starma.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Code in this file based on Applied Statistics algorithms */
/* (C) Royal Statistical Society 1980, 1982 */

/*  applied statistics algorithm as154 */

/*  start of as 154 */
/* Subroutine */ int starma(integer *ip, integer *iq, integer *ir, integer *
	np, doublereal *phi, doublereal *theta, doublereal *a, doublereal *p, 
	doublereal *v, doublereal *thetab, doublereal *xnext, doublereal *
	xrow, doublereal *rbar, integer *nrbar, integer *ifault)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer indi, indj, indn;
    static doublereal phii, phij;
    static integer i__, j, k, ifail, irank;
    static doublereal ynext;
    extern /* Subroutine */ int inclu2(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *);
    static doublereal vj, recres;
    extern /* Subroutine */ int regres(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal ssqerr;
    static integer ind, npr, ind1, ind2, npr1;


    /* Parameter adjustments */
    --a;
    --theta;
    --phi;
    --xrow;
    --xnext;
    --thetab;
    --v;
    --p;
    --rbar;

    /* Function Body */

/*        algorithm as 154  appl. statist. (1980) vol.29, p.311 */

/*        invoking this subroutine sets the values of v and phi, and */
/*        obtains the initial values of a and p. */
/*        this routine is not suitable for use with an ar(1) process. */
/*        in this case the following instructions should be used for */
/*        initialisation. */

/*     check if ar(1) */

    if (*iq > 0 || *ip > 1) {
	goto L5;
    }
    v[1] = 1.f;
    a[1] = 0.f;
    p[1] = 1.f / (1.f - phi[1] * phi[1]);
    return 0;

/*        check for failure indication. */

L5:
    *ifault = 0;
    if (*ip < 0) {
	*ifault = 1;
    }
    if (*iq < 0) {
	*ifault += 2;
    }
    if (*ip == 0 && *iq == 0) {
	*ifault = 4;
    }
    k = *iq + 1;
    if (k < *ip) {
	k = *ip;
    }
    if (*ir != k) {
	*ifault = 5;
    }
    if (*np != *ir * (*ir + 1) / 2) {
	*ifault = 6;
    }
    if (*nrbar != *np * (*np - 1) / 2) {
	*ifault = 7;
    }
    if (*ir == 1) {
	*ifault = 8;
    }
    if (*ifault != 0) {
	return 0;
    }

/*        now set a(0), v and phi. */

    i__1 = *ir;
    for (i__ = 2; i__ <= i__1; ++i__) {
	a[i__] = zero;
	if (i__ > *ip) {
	    phi[i__] = zero;
	}
	v[i__] = zero;
	if (i__ <= *iq + 1) {
	    v[i__] = theta[i__ - 1];
	}
/* L10: */
    }
    a[1] = zero;
    if (*ip == 0) {
	phi[1] = zero;
    }
    v[1] = one;
    ind = *ir;
    i__1 = *ir;
    for (j = 2; j <= i__1; ++j) {
	vj = v[j];
	i__2 = *ir;
	for (i__ = j; i__ <= i__2; ++i__) {
	    ++ind;
	    v[ind] = v[i__] * vj;
/* L20: */
	}
    }

/*        now find p(0). */

    if (*ip == 0) {
	goto L300;
    }

/*        the set of equations s * vec(p(0)) = vec(v) */
/*        is solved for vec(p(0)). */
/*        s is generated row by row in the array xnext. */
/*        the order of elements in p is changed, so as to */
/*        bring more leading zeros into the rows of s, */
/*        hence achieving a reduction of computing time. */

    irank = 0;
    ssqerr = zero;
    i__2 = *nrbar;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L40: */
	rbar[i__] = zero;
    }
    i__2 = *np;
    for (i__ = 1; i__ <= i__2; ++i__) {
	p[i__] = zero;
	thetab[i__] = zero;
	xnext[i__] = zero;
/* L50: */
    }
    ind = 0;
    ind1 = 0;
    npr = *np - *ir;
    npr1 = npr + 1;
    indj = npr1;
    ind2 = npr;
    i__2 = *ir;
    for (j = 1; j <= i__2; ++j) {
	phij = phi[j];
	xnext[indj] = zero;
	++indj;
	indi = npr1 + j;
	i__1 = *ir;
	for (i__ = j; i__ <= i__1; ++i__) {
	    ++ind;
	    ynext = v[ind];
	    phii = phi[i__];
	    if (j == *ir) {
		goto L100;
	    }
	    xnext[indj] = -phii;
	    if (i__ == *ir) {
		goto L100;
	    }
	    xnext[indi] -= phij;
	    ++ind1;
	    xnext[ind1] = -one;
L100:
	    xnext[npr1] = -phii * phij;
	    ++ind2;
	    if (ind2 > *np) {
		ind2 = 1;
	    }
	    xnext[ind2] += one;
	    inclu2(np, nrbar, &one, &xnext[1], &xrow[1], &ynext, &p[1], &
		    rbar[1], &thetab[1], &ssqerr, &recres, &irank, &ifail);

/*        no need to check ifail as weight = 1.0 */

	    xnext[ind2] = zero;
	    if (i__ == *ir) {
		goto L110;
	    }
	    xnext[indi] = zero;
	    ++indi;
	    xnext[ind1] = zero;
L110:
	    ;
	}
    }
    regres(np, nrbar, &rbar[1], &thetab[1], &p[1]);

/*        now re-order p. */

    ind = npr;
    i__1 = *ir;
    for (i__ = 1; i__ <= i__1; ++i__) {
	++ind;
	xnext[i__] = p[ind];
/* L200: */
    }
    ind = *np;
    ind1 = npr;
    i__1 = npr;
    for (i__ = 1; i__ <= i__1; ++i__) {
	p[ind] = p[ind1];
	--ind;
	--ind1;
/* L210: */
    }
    i__1 = *ir;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L220: */
	p[i__] = xnext[i__];
    }
    return 0;

/*        p(0) is obtained by backsubstitution for */
/*        a moving average process. */

L300:
    indn = *np + 1;
    ind = *np + 1;
    i__1 = *ir;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = i__;
	for (j = 1; j <= i__2; ++j) {
	    --ind;
	    p[ind] = v[ind];
	    if (j == 1) {
		goto L310;
	    }
	    --indn;
	    p[ind] += p[indn];
L310:
	    ;
	}
    }
    return 0;
} /* starma_ */


/* Subroutine */ int karma(integer *ip, integer *iq, integer *ir, integer *
	np, doublereal *phi, doublereal *theta, doublereal *a, doublereal *p, 
	doublereal *v, integer *n, doublereal *w, doublereal *resid, 
	doublereal *sumlog, doublereal *ssq, integer *iupd, doublereal *delta,
	 doublereal *e, integer *nit)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal), log(doublereal);

    /* Local variables */
    static integer inde, indn, indw;
    static doublereal g;
    static integer i__, j, l;
    static doublereal a1, wnext;
    static integer ii;
    static doublereal dt, et, ft, ut;
    static integer ir1, ind;


/*        algorithm as 154.1  appl. statist. (1980) vol.29, p.311 */

/*        invoking this subroutine updates a, p, sumlog and ssq by */
/*        inclusion of data values w(1) to w(n). the corresponding */
/*        values of resid are also obtained. */
/*        when ft is less than (1 + delta), quick recursions are used. */


    /* Parameter adjustments */
    --e;
    --a;
    --theta;
    --phi;
    --v;
    --p;
    --resid;
    --w;

    /* Function Body */


    ir1 = *ir - 1;
    i__1 = *ir;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	e[i__] = zero;
    }
    inde = 1;

/*        for non-zero values of nit, perform quick recursions. */

    if (*nit != 0) {
	goto L600;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wnext = w[i__];

/*        prediction. */

	if (*iupd == 1 && i__ == 1) {
	    goto L300;
	}

/*        here dt = ft - 1.0 */

	dt = zero;
	if (*ir != 1) {
	    dt = p[*ir + 1];
	}
	if (dt < *delta) {
	    goto L610;
	}
	a1 = a[1];
	if (*ir == 1) {
	    goto L110;
	}
	i__2 = ir1;
	for (j = 1; j <= i__2; ++j) {
/* L100: */
	    a[j] = a[j + 1];
	}
L110:
	a[*ir] = zero;
	if (*ip == 0) {
	    goto L200;
	}
	i__2 = *ip;
	for (j = 1; j <= i__2; ++j) {
/* L120: */
	    a[j] += phi[j] * a1;
	}
L200:
	ind = 0;
	indn = *ir;
	i__2 = *ir;
	for (l = 1; l <= i__2; ++l) {
	    i__3 = *ir;
	    for (j = l; j <= i__3; ++j) {
		++ind;
		p[ind] = v[ind];
		if (j == *ir) {
		    goto L210;
		}
		++indn;
		p[ind] += p[indn];
L210:
		;
	    }
	}

/*        updating. */

L300:
	ft = p[1];
	ut = wnext - a[1];
	if (*ir == 1) {
	    goto L410;
	}
	ind = *ir;
	i__3 = *ir;
	for (j = 2; j <= i__3; ++j) {
	    g = p[j] / ft;
	    a[j] += g * ut;
	    i__2 = *ir;
	    for (l = j; l <= i__2; ++l) {
		++ind;
		p[ind] -= g * p[l];
/* L400: */
	    }
	}
L410:
	a[1] = wnext;
	i__2 = *ir;
	for (l = 1; l <= i__2; ++l) {
/* L420: */
	    p[l] = zero;
	}
	resid[i__] = ut / sqrt(ft);
	e[inde] = resid[i__];
	++inde;
	if (inde > *iq) {
	    inde = 1;
	}
	*ssq += ut * ut / ft;
	*sumlog += log(ft);
/* L500: */
    }
    *nit = *n;
    return 0;

/*        quick recursions */

L600:
    i__ = 1;
L610:
    *nit = i__ - 1;
    i__1 = *n;
    for (ii = i__; ii <= i__1; ++ii) {
	et = w[ii];
	indw = ii;
	if (*ip == 0) {
	    goto L630;
	}
	i__2 = *ip;
	for (j = 1; j <= i__2; ++j) {
	    --indw;
	    if (indw < 1) {
		goto L630;
	    }
	    et -= phi[j] * w[indw];
/* L620: */
	}
L630:
	if (*iq == 0) {
	    goto L645;
	}
	i__2 = *iq;
	for (j = 1; j <= i__2; ++j) {
	    --inde;
	    if (inde == 0) {
		inde = *iq;
	    }
	    et -= theta[j] * e[inde];
/* L640: */
	}
L645:
	e[inde] = et;
	resid[ii] = et;
	*ssq += et * et;
	++inde;
	if (inde > *iq) {
	    inde = 1;
	}
/* L650: */
    }
    return 0;
} /* karma_ */


/* Subroutine */ int kalfor(integer *m, integer *ip, integer *ir, integer *
	np, doublereal *phi, doublereal *a, doublereal *p, doublereal *v, 
	doublereal *work, doublereal *x, doublereal *var)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static doublereal phii, phij;
    static integer i__, j, l;
    static doublereal a1, dt, phijdt;
    static integer ir1, ind, ind1;


/*        algorithm as 154.2  appl. statist. (1980) vol.29, p.311 */

/*        invoking this subroutine obtains predictions */
/*        of a and p, m steps ahead. */


    /* Parameter adjustments */
    --var;
    --x;
    --work;
    --a;
    --phi;
    --v;
    --p;

    /* Function Body */

    ir1 = *ir - 1;
    i__1 = *m;
    for (l = 1; l <= i__1; ++l) {

/*        predict a. */

	a1 = a[1];
	if (*ir == 1) {
	    goto L110;
	}
	i__2 = ir1;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L100: */
	    a[i__] = a[i__ + 1];
	}
L110:
	a[*ir] = zero;
	if (*ip == 0) {
	    goto L200;
	}
	i__2 = *ip;
	for (j = 1; j <= i__2; ++j) {
/* L120: */
	    a[j] += phi[j] * a1;
	}

/*        predict p. */

L200:
	i__2 = *ir;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L210: */
	    work[i__] = p[i__];
	}
	ind = 0;
	ind1 = *ir;
	dt = p[1];
	i__2 = *ir;
	for (j = 1; j <= i__2; ++j) {
	    phij = phi[j];
	    phijdt = phij * dt;
	    i__3 = *ir;
	    for (i__ = j; i__ <= i__3; ++i__) {
		++ind;
		phii = phi[i__];
		p[ind] = v[ind] + phii * phijdt;
		if (j < *ir) {
		    p[ind] += work[j + 1] * phii;
		}
		if (i__ == *ir) {
		    goto L220;
		}
		++ind1;
		p[ind] = p[ind] + work[i__ + 1] * phij + p[ind1];
L220:
		;
	    }
	}
/* modifications here */
	x[l] = a[1];
	var[l] = p[1];
/* L300: */
    }
    return 0;
} /* kalfor_ */


/* Subroutine */ int inclu2(integer *np, integer *nrbar, doublereal *weight, 
	doublereal *xnext, doublereal *xrow, doublereal *ynext, doublereal *
	d__, doublereal *rbar, doublereal *thetab, doublereal *ssqerr, 
	doublereal *recres, integer *irank, integer *ifault)
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal cbar, sbar;
    static integer i__, k;
    static doublereal y;
    static integer i1;
    static doublereal di, xi, xk, wt, rbthis;
    static integer ithisr;
    static doublereal dpi;


/*        algorithm as 154.3  appl. statist. (1980) vol.29, p.311 */

/*        fortran version of revised version of algorithm as 75.1 */
/*        appl. statist. (1974) vol.23, p.448 */
/*        see remark as r17 appl. statist. (1976) vol.25, p.323 */


    /* Parameter adjustments */
    --thetab;
    --d__;
    --xrow;
    --xnext;
    --rbar;

    /* Function Body */

/*        invoking this subroutine updates d, rbar, thetab, ssqerr */
/*        and irank by the inclusion of xnext and ynext with a */
/*        specified weight. the values of xnext, ynext and weight will */
/*        be conserved. the corresponding value of recres is calculated. */

    y = *ynext;
    wt = *weight;
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	xrow[i__] = xnext[i__];
    }
    *recres = zero;
    *ifault = 1;
    if (wt <= zero) {
	return 0;
    }
    *ifault = 0;

    ithisr = 0;
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (xrow[i__] != zero) {
	    goto L20;
	}
	ithisr = ithisr + *np - i__;
	goto L50;
L20:
	xi = xrow[i__];
	di = d__[i__];
	dpi = di + wt * xi * xi;
	d__[i__] = dpi;
	cbar = di / dpi;
	sbar = wt * xi / dpi;
	wt = cbar * wt;
	if (i__ == *np) {
	    goto L40;
	}
	i1 = i__ + 1;
	i__2 = *np;
	for (k = i1; k <= i__2; ++k) {
	    ++ithisr;
	    xk = xrow[k];
	    rbthis = rbar[ithisr];
	    xrow[k] = xk - xi * rbthis;
	    rbar[ithisr] = cbar * rbthis + sbar * xk;
/* L30: */
	}
L40:
	xk = y;
	y = xk - xi * thetab[i__];
	thetab[i__] = cbar * thetab[i__] + sbar * xk;
	if (di == zero) {
	    goto L100;
	}
L50:
	;
    }
    *ssqerr += wt * y * y;
    *recres = y * sqrt(wt);
    return 0;
L100:
    ++(*irank);
    return 0;
} /* inclu2_ */


/* Subroutine */ int regres(integer *np, integer *nrbar, doublereal *rbar, 
	doublereal *thetab, doublereal *beta)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, i1;
    static doublereal bi;
    static integer im, jm, ithisr;


/*        algorithm as 154.4  appl. statist. (1980) vol.29, p.311 */

/*        revised version of algorithm as 75.4 */
/*        appl. statist. (1974) vol.23, p.448 */
/*        invoking this subroutine obtains beta by backsubstitution */
/*        in the triangular system rbar and thetab. */

    /* Parameter adjustments */
    --beta;
    --thetab;
    --rbar;

    /* Function Body */
    ithisr = *nrbar;
    im = *np;
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
	bi = thetab[im];
	if (im == *np) {
	    goto L30;
	}
	i1 = i__ - 1;
	jm = *np;
	i__2 = i1;
	for (j = 1; j <= i__2; ++j) {
	    bi -= rbar[ithisr] * beta[jm];
	    --ithisr;
	    --jm;
/* L10: */
	}
L30:
	beta[im] = bi;
	--im;
/* L50: */
    }
    return 0;
} /* regres_ */

/*  end of as 154 */
/*  applied statistics algorithm as182 */

/*  start of as 182 */
/* Subroutine */ int forkal(integer *ip, integer *iq, integer *ir, integer *
	np, integer *ird, integer *irz, integer *id, integer *il, integer *n, 
	integer *nrbar, doublereal *phi, doublereal *theta, doublereal *delta,
	 doublereal *w, doublereal *y, doublereal *amse, doublereal *a, 
	doublereal *p, doublereal *v, doublereal *resid, doublereal *e, 
	doublereal *xnext, doublereal *xrow, doublereal *rbar, doublereal *
	thetab, doublereal *store, integer *ifault)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal two = 2.;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer id2r1, iddr, id2r2;
    static doublereal phii;
    static integer jklj;
    static doublereal phij;
    static integer iupd, idrr1, i__, j, k, l;
    extern /* Subroutine */ int karma(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *, doublereal *, doublereal *, doublereal *
	    , doublereal *, integer *, doublereal *, doublereal *, integer *);
    static doublereal sigma, a1;
    static integer j1, k1;
    static doublereal aa;
    static integer i45, jj, kk, lk, ll, nj;
    static doublereal dt;
    static integer nt;
    static doublereal phijdt;
    extern /* Subroutine */ int starma(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, integer *);
    static integer id1;
    static doublereal sumlog;
    static integer kk1, lk1, ir1, ir2, ibc;
    static doublereal del;
    static integer iid, idk, ind, jkl, kkk, iri, irj, jrj, jrk;
    static doublereal ams;
    static integer nit;
    static doublereal ssq;
    static integer idd1, idd2, ind1, ind2, id2r, jkl1, iri1;


/*     algorithm as 182  appl. statist. (1982) vol.31, no.2 */

/*     finite sample prediction from arima processes. */

/*     auxiliary routines required: karma & starma from as 154 and */
/*     routines called by them: inclu2 from asr 17 (a slight variant on */
/*     as 75, and regres from as 75. */

    /* Parameter adjustments */
    --e;
    --theta;
    --phi;
    --thetab;
    --xrow;
    --xnext;
    --v;
    --store;
    --a;
    --p;
    --delta;
    --amse;
    --y;
    --resid;
    --w;
    --rbar;

    /* Function Body */

/*     invoking this routine will calculate the finite sample predictions */
/*     and their conditional mean square errors for any arima process. */

/*     check for input faults. */

    *ifault = 0;
    if (*ip < 0) {
	*ifault = 1;
    }
    if (*iq < 0) {
	*ifault += 2;
    }
    if (*ip * *ip + *iq * *iq == 0) {
	*ifault = 4;
    }
    k = *iq + 1;
    if (k < *ip) {
	k = *ip;
    }
    if (*ir != k) {
	*ifault = 5;
    }
    if (*np != *ir * (*ir + 1) / 2) {
	*ifault = 6;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ifault = 7;
    }
    if (*id < 0) {
	*ifault = 8;
    }
    if (*ird != *ir + *id) {
	*ifault = 9;
    }
    if (*irz != *ird * (*ird + 1) / 2) {
	*ifault = 10;
    }
    if (*il < 1) {
	*ifault = 11;
    }
    if (*ifault != 0) {
	return 0;
    }

/*     calculate initial conditions for kalman filter */

    a[1] = zero;
    v[1] = one;
/* This is all pointless: starma will revise v */
/* It over-runs v, too. */
/*      if (np .eq. 1) go to 130 */
/*      do 100 i = 2, np */
/*  100 v(i) = zero */
/*      if (iq .eq. 0) go to 130 */
/*      iq1 = iq + 1 */
/*      do 110 i = 2, iq1 */
/*  110 v(i) = theta(i-1) */
/*      do 120 j = 1, iq */
/* 	ll = j * (2*ir + 1 - j) / 2 */
/* 	do 120 i = j, iq */
/* 	  lli = ll + i */
/* 	  v(lli) = theta(i) * theta(j) */
/*  120 continue */

/*     find initial likelihood conditions. */
/*     ifault not tested on exit from starma as all possible errors */
/*     have been checked above. */

/* L130: */
    if (*ir == 1) {
	p[1] = one / (one - phi[1] * phi[1]);
    }
    if (*ir != 1) {
	starma(ip, iq, ir, np, &phi[1], &theta[1], &a[1], &p[1], &v[1], &
		thetab[1], &xnext[1], &xrow[1], &rbar[1], nrbar, ifault);
    }

/*     calculate data transformations */

    nt = *n - *id;
    if (*id == 0) {
	goto L170;
    }
    i__1 = *id;
    for (j = 1; j <= i__1; ++j) {
	nj = *n - j;
	store[j] = w[nj];
/* L140: */
    }
    i__1 = nt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	aa = zero;
	i__2 = *id;
	for (k = 1; k <= i__2; ++k) {
	    idk = *id + i__ - k;
	    aa -= delta[k] * w[idk];
/* L150: */
	}
	iid = i__ + *id;
	w[i__] = w[iid] + aa;
/* L160: */
    }

/*     evaluate likelihood to obtain final kf conditions */

L170:
    sumlog = zero;
    ssq = zero;
    iupd = 1;
    del = -one;
    nit = 0;
    karma(ip, iq, ir, np, &phi[1], &theta[1], &a[1], &p[1], &v[1], &nt, &w[1]
	    , &resid[1], &sumlog, &ssq, &iupd, &del, &e[1], &nit);

/*     calculate m.l.e. of sigma squared */

    sigma = zero;
    i__1 = nt;
    for (j = 1; j <= i__1; ++j) {
/* L200: */
/* Computing 2nd power */
	d__1 = resid[j];
	sigma += d__1 * d__1;
    }
    sigma /= nt;

/*     reset the initial a and p when differencing occurs */

    if (*id == 0) {
	goto L250;
    }
    i__1 = *np;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L210: */
	xrow[i__] = p[i__];
    }
    i__1 = *irz;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L220: */
	p[i__] = zero;
    }
    ind = 0;
    i__1 = *ir;
    for (j = 1; j <= i__1; ++j) {
	k = (j - 1) * (*id + *ir + 1) - (j - 1) * j / 2;
	i__2 = *ir;
	for (i__ = j; i__ <= i__2; ++i__) {
	    ++ind;
	    ++k;
	    p[k] = xrow[ind];
/* L230: */
	}
    }
    i__2 = *id;
    for (j = 1; j <= i__2; ++j) {
	irj = *ir + j;
	a[irj] = store[j];
/* L240: */
    }

/*     set up constants */

L250:
    ir2 = *ir + 1;
    ir1 = *ir - 1;
    id1 = *id - 1;
    id2r = *ird << 1;
    id2r1 = id2r - 1;
    idd1 = (*id << 1) + 1;
    idd2 = idd1 + 1;
    i45 = id2r + 1;
    idrr1 = *ird + 1;
    iddr = (*id << 1) + *ir;
    jkl = *ir * (iddr + 1) / 2;
    jkl1 = jkl + 1;
    id2r2 = id2r + 2;
    ibc = *ir * (i45 - *ir) / 2;
    i__2 = *il;
    for (l = 1; l <= i__2; ++l) {

/*     predict a */

	a1 = a[1];
	if (*ir == 1) {
	    goto L310;
	}
	i__1 = ir1;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L300: */
	    a[i__] = a[i__ + 1];
	}
L310:
	a[*ir] = zero;
	if (*ip == 0) {
	    goto L330;
	}
	i__1 = *ip;
	for (j = 1; j <= i__1; ++j) {
/* L320: */
	    a[j] += phi[j] * a1;
	}
/* original has label 360 and overruns a */
L330:
	if (*id == 0) {
	    goto L361;
	}
	i__1 = *id;
	for (j = 1; j <= i__1; ++j) {
	    irj = *ir + j;
	    a1 += delta[j] * a[irj];
/* L340: */
	}
	if (*id < 2) {
	    goto L360;
	}
	i__1 = id1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    iri1 = *ird - i__;
	    a[iri1 + 1] = a[iri1];
/* L350: */
	}
L360:
	a[ir2] = a1;
L361:

/*     predict p */

	if (*id == 0) {
	    goto L480;
	}
	i__1 = *id;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    store[i__] = zero;
	    i__3 = *id;
	    for (j = 1; j <= i__3; ++j) {
		ll = max(i__,j);
		k = min(i__,j);
		jj = jkl + (ll - k) + 1 + (k - 1) * (idd2 - k) / 2;
		store[i__] += delta[j] * p[jj];
/* L370: */
	    }
	}
	if (*id == 1) {
	    goto L400;
	}
	i__3 = id1;
	for (j = 1; j <= i__3; ++j) {
	    jj = *id - j;
	    lk = (jj - 1) * (idd2 - jj) / 2 + jkl;
	    lk1 = jj * (idd1 - jj) / 2 + jkl;
	    i__1 = j;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		++lk;
		++lk1;
		p[lk1] = p[lk];
/* L380: */
	    }
	}
	i__1 = id1;
	for (j = 1; j <= i__1; ++j) {
	    jklj = jkl1 + j;
	    irj = *ir + j;
	    p[jklj] = store[j] + p[irj];
/* L390: */
	}
L400:
	p[jkl1] = p[1];
	i__1 = *id;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    iri = *ir + i__;
	    p[jkl1] += delta[i__] * (store[i__] + two * p[iri]);
/* L410: */
	}
	i__1 = *id;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    iri = *ir + i__;
	    store[i__] = p[iri];
/* L420: */
	}
	i__1 = *ir;
	for (j = 1; j <= i__1; ++j) {
	    kk1 = j * (id2r1 - j) / 2 + *ir;
	    k1 = (j - 1) * (id2r - j) / 2 + *ir;
	    i__3 = *id;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		kk = kk1 + i__;
		k = k1 + i__;
		p[k] = phi[j] * store[i__];
		if (j != *ir) {
		    p[k] += p[kk];
		}
/* L430: */
	    }
	}

	i__3 = *ir;
	for (j = 1; j <= i__3; ++j) {
	    store[j] = zero;
	    kkk = j * (i45 - j) / 2 - *id;
	    i__1 = *id;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		++kkk;
		store[j] += delta[i__] * p[kkk];
/* L440: */
	    }
	}
	if (*id == 1) {
	    goto L460;
	}
	i__1 = *ir;
	for (j = 1; j <= i__1; ++j) {
	    k = j * idrr1 - j * (j + 1) / 2 + 1;
	    i__3 = id1;
	    for (i__ = 1; i__ <= i__3; ++i__) {
		--k;
		p[k] = p[k - 1];
/* L450: */
	    }
	}
L460:
	i__3 = *ir;
	for (j = 1; j <= i__3; ++j) {
	    k = (j - 1) * (id2r - j) / 2 + *ir + 1;
	    p[k] = store[j] + phi[j] * p[1];
	    if (j < *ir) {
		p[k] += p[j + 1];
	    }
/* L470: */
	}
L480:
	i__3 = *ir;
	for (i__ = 1; i__ <= i__3; ++i__) {
/* L490: */
	    store[i__] = p[i__];
	}

	ind = 0;
	dt = p[1];
	i__3 = *ir;
	for (j = 1; j <= i__3; ++j) {
	    phij = phi[j];
	    phijdt = phij * dt;
	    ind2 = (j - 1) * (id2r2 - j) / 2;
	    ind1 = j * (i45 - j) / 2;
	    i__1 = *ir;
	    for (i__ = j; i__ <= i__1; ++i__) {
		++ind;
		++ind2;
		phii = phi[i__];
		p[ind2] = v[ind] + phii * phijdt;
		if (j < *ir) {
		    p[ind2] += store[j + 1] * phii;
		}
		if (i__ == *ir) {
		    goto L500;
		}
		++ind1;
		p[ind2] = p[ind2] + store[i__ + 1] * phij + p[ind1];
L500:
		;
	    }
	}

/*     predict y */

	y[l] = a[1];
	if (*id == 0) {
	    goto L520;
	}
	i__1 = *id;
	for (j = 1; j <= i__1; ++j) {
	    irj = *ir + j;
	    y[l] += a[irj] * delta[j];
/* L510: */
	}

/*     calculate m.s.e. of y */

L520:
	ams = p[1];
	if (*id == 0) {
	    goto L550;
	}
	i__1 = *id;
	for (j = 1; j <= i__1; ++j) {
	    jrj = ibc + (j - 1) * (idd2 - j) / 2;
	    irj = *ir + j;
/* Computing 2nd power */
	    d__1 = delta[j];
	    ams = ams + two * delta[j] * p[irj] + p[jrj + 1] * (d__1 * d__1);
/* L530: */
	}
	if (*id == 1) {
	    goto L550;
	}
	i__1 = id1;
	for (j = 1; j <= i__1; ++j) {
	    j1 = j + 1;
	    jrk = ibc + 1 + (j - 1) * (idd2 - j) / 2;
	    i__3 = *id;
	    for (i__ = j1; i__ <= i__3; ++i__) {
		++jrk;
		ams += two * delta[i__] * delta[j] * p[jrk];
/* L540: */
	    }
	}
L550:
	amse[l] = ams * sigma;
/* L560: */
    }

    return 0;
} /* forkal_ */
