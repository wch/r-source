/* stl.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static logical c_false = FALSE_;
static integer c__2 = 2;
static integer c__1 = 1;
static logical c_true = TRUE_;


/*     from netlib/a/stl: no authorship nor copyright claim in the source; */
/*     presumably by the authors of */

/*     R.B. Cleveland, W.S.Cleveland, J.E. McRae, and I. Terpenning, */
/*     STL: A Seasonal-Trend Decomposition Procedure Based on Loess, */
/*     Statistics Research Report, AT&T Bell Laboratories. */

/*     converted to double precision by B.D. Ripley 1999 */

/* Subroutine */ int stl(doublereal *y, integer *n, integer *np, integer *ns,
	 integer *nt, integer *nl, integer *isdeg, integer *itdeg, integer *
	ildeg, integer *nsjump, integer *ntjump, integer *nljump, integer *ni,
	 integer *no, doublereal *rw, doublereal *season, doublereal *trend, 
	doublereal *work)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1;

    /* Local variables */
    extern /* Subroutine */ int stlrwts(doublereal *, integer *, doublereal *
	    , doublereal *);
    static integer i__, k, newnl, newnp, newns, newnt;
    static logical userw;
    extern /* Subroutine */ int stl1stp(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, logical *, doublereal 
	    *, doublereal *, doublereal *, doublereal *);

    /* Parameter adjustments */
    --trend;
    --season;
    --rw;
    --y;
    work_dim1 = *n + (*np << 1);
    work_offset = work_dim1 + 1;
    work -= work_offset;

    /* Function Body */
    userw = FALSE_;
    k = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	trend[i__] = 0.f;
/* L23000: */
    }
    newns = max(3,*ns);
    newnt = max(3,*nt);
    newnl = max(3,*nl);
    newnp = max(2,*np);
    if (! (newns % 2 == 0)) {
	goto L23002;
    }
    ++newns;
L23002:
    if (! (newnt % 2 == 0)) {
	goto L23004;
    }
    ++newnt;
L23004:
    if (! (newnl % 2 == 0)) {
	goto L23006;
    }
    ++newnl;
L23006:
L23008:
    stl1stp(&y[1], n, &newnp, &newns, &newnt, &newnl, isdeg, itdeg, ildeg, 
	    nsjump, ntjump, nljump, ni, &userw, &rw[1], &season[1], &trend[1],
	     &work[work_offset]);
    ++k;
    if (! (k > *no)) {
	goto L23011;
    }
    goto L23010;
L23011:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[i__ + work_dim1] = trend[i__] + season[i__];
/* L23013: */
    }
    stlrwts(&y[1], n, &work[work_dim1 + 1], &rw[1]);
    userw = TRUE_;
/* L23009: */
    goto L23008;
L23010:
    if (! (*no <= 0)) {
	goto L23015;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rw[i__] = 1.f;
/* L23017: */
    }
L23015:
    return 0;
} /* stl_ */

/* Subroutine */ int stless(doublereal *y, integer *n, integer *len, integer 
	*ideg, integer *njump, logical *userw, doublereal *rw, doublereal *ys,
	 doublereal *res)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Local variables */
    static integer i__, j, k;
    static doublereal delta;
    static integer nleft, newnj;
    static logical ok;
    static integer nright;
    extern /* Subroutine */ int stlest(doublereal *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, logical *, doublereal *, logical *);
    static integer nsh;

    /* Parameter adjustments */
    --res;
    --ys;
    --rw;
    --y;

    /* Function Body */
    if (! (*n < 2)) {
	goto L23019;
    }
    ys[1] = y[1];
    return 0;
L23019:
/* Computing MIN */
    i__1 = *njump, i__2 = *n - 1;
    newnj = min(i__1,i__2);
    if (! (*len >= *n)) {
	goto L23021;
    }
    nleft = 1;
    nright = *n;
    i__1 = *n;
    i__2 = newnj;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	d__1 = (doublereal) i__;
	stlest(&y[1], n, len, ideg, &d__1, &ys[i__], &nleft, &nright, &res[1]
		, userw, &rw[1], &ok);
	if (ok) {
	    goto L23025;
	}
	ys[i__] = y[i__];
L23025:
/* L23023: */
	;
    }
    goto L23022;
L23021:
    if (! (newnj == 1)) {
	goto L23027;
    }
    nsh = (*len + 1) / 2;
    nleft = 1;
    nright = *len;
    i__2 = *n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	if (! (i__ > nsh && nright != *n)) {
	    goto L23031;
	}
	++nleft;
	++nright;
L23031:
	d__1 = (doublereal) i__;
	stlest(&y[1], n, len, ideg, &d__1, &ys[i__], &nleft, &nright, &res[1]
		, userw, &rw[1], &ok);
	if (ok) {
	    goto L23033;
	}
	ys[i__] = y[i__];
L23033:
/* L23029: */
	;
    }
    goto L23028;
L23027:
    nsh = (*len + 1) / 2;
    i__2 = *n;
    i__1 = newnj;
    for (i__ = 1; i__1 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__1) {
	if (! (i__ < nsh)) {
	    goto L23037;
	}
	nleft = 1;
	nright = *len;
	goto L23038;
L23037:
	if (! (i__ >= *n - nsh + 1)) {
	    goto L23039;
	}
	nleft = *n - *len + 1;
	nright = *n;
	goto L23040;
L23039:
	nleft = i__ - nsh + 1;
	nright = *len + i__ - nsh;
L23040:
L23038:
	d__1 = (doublereal) i__;
	stlest(&y[1], n, len, ideg, &d__1, &ys[i__], &nleft, &nright, &res[1]
		, userw, &rw[1], &ok);
	if (ok) {
	    goto L23041;
	}
	ys[i__] = y[i__];
L23041:
/* L23035: */
	;
    }
L23028:
L23022:
    if (! (newnj != 1)) {
	goto L23043;
    }
    i__1 = *n - newnj;
    i__2 = newnj;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	delta = (ys[i__ + newnj] - ys[i__]) / (doublereal) newnj;
	i__3 = i__ + newnj - 1;
	for (j = i__ + 1; j <= i__3; ++j) {
	    ys[j] = ys[i__] + delta * (doublereal) (j - i__);
/* L23047: */
	}
/* L23045: */
    }
    k = (*n - 1) / newnj * newnj + 1;
    if (! (k != *n)) {
	goto L23049;
    }
    d__1 = (doublereal) (*n);
    stlest(&y[1], n, len, ideg, &d__1, &ys[*n], &nleft, &nright, &res[1], 
	    userw, &rw[1], &ok);
    if (ok) {
	goto L23051;
    }
    ys[*n] = y[*n];
L23051:
    if (! (k != *n - 1)) {
	goto L23053;
    }
    delta = (ys[*n] - ys[k]) / (doublereal) (*n - k);
    i__2 = *n - 1;
    for (j = k + 1; j <= i__2; ++j) {
	ys[j] = ys[k] + delta * (doublereal) (j - k);
/* L23055: */
    }
L23053:
L23049:
L23043:
    return 0;
} /* stless_ */

/* Subroutine */ int stlest(doublereal *y, integer *n, integer *len, integer 
	*ideg, doublereal *xs, doublereal *ys, integer *nleft, integer *
	nright, doublereal *w, logical *userw, doublereal *rw, logical *ok)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static doublereal a, b, c__, h__;
    static integer j;
    static doublereal r__, range, h1, h9;

    /* Parameter adjustments */
    --rw;
    --w;
    --y;

    /* Function Body */
    range = (doublereal) (*n) - 1.;
/* Computing MAX */
    d__1 = *xs - (doublereal) (*nleft), d__2 = (doublereal) (*nright) - *xs;
    h__ = max(d__1,d__2);
    if (! (*len > *n)) {
	goto L23057;
    }
    h__ += (doublereal) ((*len - *n) / 2);
L23057:
    h9 = h__ * .999f;
    h1 = h__ * .001f;
    a = 0.f;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
	w[j] = 0.f;
	r__ = (d__1 = (doublereal) j - *xs, abs(d__1));
	if (! (r__ <= h9)) {
	    goto L23061;
	}
	if (! (r__ <= h1)) {
	    goto L23063;
	}
	w[j] = 1.f;
	goto L23064;
L23063:
/* Computing 3rd power */
	d__2 = r__ / h__, d__3 = d__2;
/* Computing 3rd power */
	d__1 = 1.f - d__3 * (d__2 * d__2), d__4 = d__1;
	w[j] = d__4 * (d__1 * d__1);
L23064:
	if (! (*userw)) {
	    goto L23065;
	}
	w[j] = rw[j] * w[j];
L23065:
	a += w[j];
L23061:
/* L23059: */
	;
    }
    if (! (a <= 0.f)) {
	goto L23067;
    }
    *ok = FALSE_;
    goto L23068;
L23067:
    *ok = TRUE_;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
	w[j] /= a;
/* L23069: */
    }
    if (! (h__ > 0.f && *ideg > 0)) {
	goto L23071;
    }
    a = 0.f;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
	a += w[j] * (doublereal) j;
/* L23073: */
    }
    b = *xs - a;
    c__ = 0.f;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
/* Computing 2nd power */
	d__1 = (doublereal) j - a;
	c__ += w[j] * (d__1 * d__1);
/* L23075: */
    }
    if (! (sqrt(c__) > range * .001f)) {
	goto L23077;
    }
    b /= c__;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
	w[j] *= b * ((doublereal) j - a) + 1.f;
/* L23079: */
    }
L23077:
L23071:
    *ys = 0.f;
    i__1 = *nright;
    for (j = *nleft; j <= i__1; ++j) {
	*ys += w[j] * y[j];
/* L23081: */
    }
L23068:
    return 0;
} /* stlest_ */

/* Subroutine */ int stlfts(doublereal *x, integer *n, integer *np, 
	doublereal *trend, doublereal *work)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int stlma(doublereal *, integer *, integer *, 
	    doublereal *);

    /* Parameter adjustments */
    --work;
    --trend;
    --x;

    /* Function Body */
    stlma(&x[1], n, np, &trend[1]);
    i__1 = *n - *np + 1;
    stlma(&trend[1], &i__1, np, &work[1]);
    i__1 = *n - (*np << 1) + 2;
    stlma(&work[1], &i__1, &c__3, &trend[1]);
    return 0;
} /* stlfts_ */

/* Subroutine */ int stlma(doublereal *x, integer *n, integer *len, 
	doublereal *ave)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static doublereal flen;
    static integer newn, i__, j, k, m;
    static doublereal v;

    /* Parameter adjustments */
    --ave;
    --x;

    /* Function Body */
    newn = *n - *len + 1;
    flen = (doublereal) (*len);
    v = 0.f;
    i__1 = *len;
    for (i__ = 1; i__ <= i__1; ++i__) {
	v += x[i__];
/* L23083: */
    }
    ave[1] = v / flen;
    if (! (newn > 1)) {
	goto L23085;
    }
    k = *len;
    m = 0;
    i__1 = newn;
    for (j = 2; j <= i__1; ++j) {
	++k;
	++m;
	v = v - x[m] + x[k];
	ave[j] = v / flen;
/* L23087: */
    }
L23085:
    return 0;
} /* stlma_ */

/* Subroutine */ int stl1stp(doublereal *y, integer *n, integer *np, integer 
	*ns, integer *nt, integer *nl, integer *isdeg, integer *itdeg, 
	integer *ildeg, integer *nsjump, integer *ntjump, integer *nljump, 
	integer *ni, logical *userw, doublereal *rw, doublereal *season, 
	doublereal *trend, doublereal *work)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2;

    /* Local variables */
    static integer i__, j;
    extern /* Subroutine */ int stlss(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, logical *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), stless(doublereal *, integer *, integer *, 
	    integer *, integer *, logical *, doublereal *, doublereal *, 
	    doublereal *), stlfts(doublereal *, integer *, integer *, 
	    doublereal *, doublereal *);

    /* Parameter adjustments */
    --trend;
    --season;
    --rw;
    --y;
    work_dim1 = *n + (*np << 1);
    work_offset = work_dim1 + 1;
    work -= work_offset;

    /* Function Body */
    i__1 = *ni;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[i__ + work_dim1] = y[i__] - trend[i__];
/* L23091: */
	}
	stlss(&work[work_dim1 + 1], n, np, ns, isdeg, nsjump, userw, &rw[1], 
		&work[(work_dim1 << 1) + 1], &work[work_dim1 * 3 + 1], &work[(
		work_dim1 << 2) + 1], &work[work_dim1 * 5 + 1], &season[1]);
	i__2 = *n + (*np << 1);
	stlfts(&work[(work_dim1 << 1) + 1], &i__2, np, &work[work_dim1 * 3 + 
		1], &work[work_dim1 + 1]);
	stless(&work[work_dim1 * 3 + 1], n, nl, ildeg, nljump, &c_false, &
		work[(work_dim1 << 2) + 1], &work[work_dim1 + 1], &work[
		work_dim1 * 5 + 1]);
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    season[i__] = work[*np + i__ + (work_dim1 << 1)] - work[i__ + 
		    work_dim1];
/* L23093: */
	}
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    work[i__ + work_dim1] = y[i__] - season[i__];
/* L23095: */
	}
	stless(&work[work_dim1 + 1], n, nt, itdeg, ntjump, userw, &rw[1], &
		trend[1], &work[work_dim1 * 3 + 1]);
/* L23089: */
    }
    return 0;
} /* stl1stp_ */

/* Subroutine */ int stlrwts(doublereal *y, integer *n, doublereal *fit, 
	doublereal *rw)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static doublereal cmad;
    static integer i__;
    static doublereal r__, c1, c9;
    extern /* Subroutine */ int psort(doublereal *, integer *, integer *, 
	    integer *);
    static integer mid[2];

    /* Parameter adjustments */
    --rw;
    --fit;
    --y;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rw[i__] = (d__1 = y[i__] - fit[i__], abs(d__1));
/* L23097: */
    }
    mid[0] = *n / 2 + 1;
    mid[1] = *n - mid[0] + 1;
    psort(&rw[1], n, mid, &c__2);
    cmad = (rw[mid[0]] + rw[mid[1]]) * 3.f;
    c9 = cmad * .999f;
    c1 = cmad * .001f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r__ = (d__1 = y[i__] - fit[i__], abs(d__1));
	if (! (r__ <= c1)) {
	    goto L23101;
	}
	rw[i__] = 1.f;
	goto L23102;
L23101:
	if (! (r__ <= c9)) {
	    goto L23103;
	}
/* Computing 2nd power */
	d__2 = r__ / cmad;
/* Computing 2nd power */
	d__1 = 1.f - d__2 * d__2;
	rw[i__] = d__1 * d__1;
	goto L23104;
L23103:
	rw[i__] = 0.f;
L23104:
L23102:
/* L23099: */
	;
    }
    return 0;
} /* stlrwts_ */

/* Subroutine */ int stlss(doublereal *y, integer *n, integer *np, integer *
	ns, integer *isdeg, integer *nsjump, logical *userw, doublereal *rw, 
	doublereal *season, doublereal *work1, doublereal *work2, doublereal *
	work3, doublereal *work4)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, k, m, nleft;
    static logical ok;
    static doublereal xs;
    static integer nright;
    extern /* Subroutine */ int stless(doublereal *, integer *, integer *, 
	    integer *, integer *, logical *, doublereal *, doublereal *, 
	    doublereal *), stlest(doublereal *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, integer *, integer *, 
	    doublereal *, logical *, doublereal *, logical *);

    /* Parameter adjustments */
    --work4;
    --work3;
    --work2;
    --work1;
    --rw;
    --y;
    --season;

    /* Function Body */
    j = 1;
L23105:
    if (! (j <= *np)) {
	goto L23107;
    }
    k = (*n - j) / *np + 1;
    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work1[i__] = y[(i__ - 1) * *np + j];
/* L23108: */
    }
    if (! (*userw)) {
	goto L23110;
    }
    i__1 = k;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work3[i__] = rw[(i__ - 1) * *np + j];
/* L23112: */
    }
L23110:
    stless(&work1[1], &k, ns, isdeg, nsjump, userw, &work3[1], &work2[2], &
	    work4[1]);
    xs = 0.;
    nright = min(*ns,k);
    stlest(&work1[1], &k, ns, isdeg, &xs, &work2[1], &c__1, &nright, &work4[
	    1], userw, &work3[1], &ok);
    if (ok) {
	goto L23114;
    }
    work2[1] = work2[2];
L23114:
    xs = (doublereal) (k + 1);
/* Computing MAX */
    i__1 = 1, i__2 = k - *ns + 1;
    nleft = max(i__1,i__2);
    stlest(&work1[1], &k, ns, isdeg, &xs, &work2[k + 2], &nleft, &k, &work4[
	    1], userw, &work3[1], &ok);
    if (ok) {
	goto L23116;
    }
    work2[k + 2] = work2[k + 1];
L23116:
    i__1 = k + 2;
    for (m = 1; m <= i__1; ++m) {
	season[(m - 1) * *np + j] = work2[m];
/* L23118: */
    }
    ++j;
    goto L23105;
L23107:
    return 0;
} /* stlss_ */

/* Subroutine */ int stlez(doublereal *y, integer *n, integer *np, integer *
	ns, integer *isdeg, integer *itdeg, logical *robust, integer *no, 
	doublereal *rw, doublereal *season, doublereal *trend, doublereal *
	work)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1, i__2;
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int stlrwts(doublereal *, integer *, doublereal *
	    , doublereal *);
    static doublereal difs, dift, mins, mint, maxs, maxt;
    static integer i__, j, ildeg;
    static doublereal maxds, maxdt;
    static integer newnp, newns, ni, nl, nt, nljump, nsjump, ntjump;
    extern /* Subroutine */ int stl1stp(doublereal *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, logical *, doublereal 
	    *, doublereal *, doublereal *, doublereal *);

    /* Parameter adjustments */
    --trend;
    --season;
    --rw;
    --y;
    work_dim1 = *n + (*np << 1);
    work_offset = work_dim1 + 1;
    work -= work_offset;

    /* Function Body */
    ildeg = *itdeg;
    newns = max(3,*ns);
    if (! (newns % 2 == 0)) {
	goto L23120;
    }
    ++newns;
L23120:
    newnp = max(2,*np);
    nt = newnp * 1.5f / (1 - 1.5f / newns) + .5f;
    nt = max(3,nt);
    if (! (nt % 2 == 0)) {
	goto L23122;
    }
    ++nt;
L23122:
    nl = newnp;
    if (! (nl % 2 == 0)) {
	goto L23124;
    }
    ++nl;
L23124:
    if (! (*robust)) {
	goto L23126;
    }
    ni = 1;
    goto L23127;
L23126:
    ni = 2;
L23127:
/* Computing MAX */
    i__1 = 1, i__2 = (integer) ((real) newns / 10 + .9f);
    nsjump = max(i__1,i__2);
/* Computing MAX */
    i__1 = 1, i__2 = (integer) ((real) nt / 10 + .9f);
    ntjump = max(i__1,i__2);
/* Computing MAX */
    i__1 = 1, i__2 = (integer) ((real) nl / 10 + .9f);
    nljump = max(i__1,i__2);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	trend[i__] = 0.f;
/* L23128: */
    }
    stl1stp(&y[1], n, &newnp, &newns, &nt, &nl, isdeg, itdeg, &ildeg, &
	    nsjump, &ntjump, &nljump, &ni, &c_false, &rw[1], &season[1], &
	    trend[1], &work[work_offset]);
    *no = 0;
    if (! (*robust)) {
	goto L23130;
    }
    j = 1;
L23132:
    if (! (j <= 15)) {
	goto L23134;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	work[i__ + work_dim1 * 6] = season[i__];
	work[i__ + work_dim1 * 7] = trend[i__];
	work[i__ + work_dim1] = trend[i__] + season[i__];
/* L23135: */
    }
    stlrwts(&y[1], n, &work[work_dim1 + 1], &rw[1]);
    stl1stp(&y[1], n, &newnp, &newns, &nt, &nl, isdeg, itdeg, &ildeg, &
	    nsjump, &ntjump, &nljump, &ni, &c_true, &rw[1], &season[1], &
	    trend[1], &work[work_offset]);
    ++(*no);
    maxs = work[work_dim1 * 6 + 1];
    mins = work[work_dim1 * 6 + 1];
    maxt = work[work_dim1 * 7 + 1];
    mint = work[work_dim1 * 7 + 1];
    maxds = (d__1 = work[work_dim1 * 6 + 1] - season[1], abs(d__1));
    maxdt = (d__1 = work[work_dim1 * 7 + 1] - trend[1], abs(d__1));
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (! (maxs < work[i__ + work_dim1 * 6])) {
	    goto L23139;
	}
	maxs = work[i__ + work_dim1 * 6];
L23139:
	if (! (maxt < work[i__ + work_dim1 * 7])) {
	    goto L23141;
	}
	maxt = work[i__ + work_dim1 * 7];
L23141:
	if (! (mins > work[i__ + work_dim1 * 6])) {
	    goto L23143;
	}
	mins = work[i__ + work_dim1 * 6];
L23143:
	if (! (mint > work[i__ + work_dim1 * 7])) {
	    goto L23145;
	}
	mint = work[i__ + work_dim1 * 7];
L23145:
	difs = (d__1 = work[i__ + work_dim1 * 6] - season[i__], abs(d__1));
	dift = (d__1 = work[i__ + work_dim1 * 7] - trend[i__], abs(d__1));
	if (! (maxds < difs)) {
	    goto L23147;
	}
	maxds = difs;
L23147:
	if (! (maxdt < dift)) {
	    goto L23149;
	}
	maxdt = dift;
L23149:
/* L23137: */
	;
    }
    if (! (maxds / (maxs - mins) < .01f && maxdt / (maxt - mint) < .01f)) {
	goto L23151;
    }
    goto L23134;
L23151:
    ++j;
    goto L23132;
L23134:
L23130:
    if (*robust) {
	goto L23153;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	rw[i__] = 1.f;
/* L23155: */
    }
L23153:
    return 0;
} /* stlez_ */

/* Subroutine */ int psort(doublereal *a, integer *n, integer *ind, integer *
	ni)
{
    static integer indl[16], indu[16], i__, j, k, l, m, p;
    static doublereal t;
    static integer ij, il[16], jl, iu[16], ju;
    static doublereal tt;

    /* Parameter adjustments */
    --a;
    --ind;

    /* Function Body */
    if (! (*n < 0 || *ni < 0)) {
	goto L23157;
    }
    return 0;
L23157:
    if (! (*n < 2 || *ni == 0)) {
	goto L23159;
    }
    return 0;
L23159:
    jl = 1;
    ju = *ni;
    indl[0] = 1;
    indu[0] = *ni;
    i__ = 1;
    j = *n;
    m = 1;
L23161:
    if (! (i__ < j)) {
	goto L23164;
    }
    goto L10;
L23164:
L23166:
    --m;
    if (! (m == 0)) {
	goto L23169;
    }
    goto L23163;
L23169:
    i__ = il[m - 1];
    j = iu[m - 1];
    jl = indl[m - 1];
    ju = indu[m - 1];
    if (! (jl <= ju)) {
	goto L23171;
    }
L23173:
    if (! (j - i__ > 10)) {
	goto L23174;
    }
L10:
    k = i__;
    ij = (i__ + j) / 2;
    t = a[ij];
    if (! (a[i__] > t)) {
	goto L23175;
    }
    a[ij] = a[i__];
    a[i__] = t;
    t = a[ij];
L23175:
    l = j;
    if (! (a[j] < t)) {
	goto L23177;
    }
    a[ij] = a[j];
    a[j] = t;
    t = a[ij];
    if (! (a[i__] > t)) {
	goto L23179;
    }
    a[ij] = a[i__];
    a[i__] = t;
    t = a[ij];
L23179:
L23177:
L23181:
    --l;
    if (! (a[l] <= t)) {
	goto L23184;
    }
    tt = a[l];
L23186:
    ++k;
/* L23187: */
    if (! (a[k] >= t)) {
	goto L23186;
    }
    if (! (k > l)) {
	goto L23189;
    }
    goto L23183;
L23189:
    a[l] = a[k];
    a[k] = tt;
L23184:
/* L23182: */
    goto L23181;
L23183:
    indl[m - 1] = jl;
    indu[m - 1] = ju;
    p = m;
    ++m;
    if (! (l - i__ <= j - k)) {
	goto L23191;
    }
    il[p - 1] = k;
    iu[p - 1] = j;
    j = l;
L23193:
    if (! (jl > ju)) {
	goto L23196;
    }
    goto L23167;
L23196:
    if (! (ind[ju] <= j)) {
	goto L23198;
    }
    goto L23195;
L23198:
    --ju;
/* L23194: */
    goto L23193;
L23195:
    indl[p - 1] = ju + 1;
    goto L23192;
L23191:
    il[p - 1] = i__;
    iu[p - 1] = l;
    i__ = k;
L23200:
    if (! (jl > ju)) {
	goto L23203;
    }
    goto L23167;
L23203:
    if (! (ind[jl] >= i__)) {
	goto L23205;
    }
    goto L23202;
L23205:
    ++jl;
/* L23201: */
    goto L23200;
L23202:
    indu[p - 1] = jl - 1;
L23192:
    goto L23173;
L23174:
    if (! (i__ == 1)) {
	goto L23207;
    }
    goto L23168;
L23207:
    --i__;
L23209:
    ++i__;
    if (! (i__ == j)) {
	goto L23212;
    }
    goto L23211;
L23212:
    t = a[i__ + 1];
    if (! (a[i__] > t)) {
	goto L23214;
    }
    k = i__;
L23216:
    a[k + 1] = a[k];
    --k;
/* L23217: */
    if (! (t >= a[k])) {
	goto L23216;
    }
    a[k + 1] = t;
L23214:
/* L23210: */
    goto L23209;
L23211:
L23171:
L23167:
    goto L23166;
L23168:
/* L23162: */
    goto L23161;
L23163:
    return 0;
} /* psort_ */
