/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2013	    The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include "statsR.h"

#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif
/* interval at which to check interrupts */
//#define NINTERRUPT 1000000


#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("Non-numeric argument to mathematical function")


/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

#define SETUP_Math2					\
    na = XLENGTH(sa);					\
    nb = XLENGTH(sb);					\
    if ((na == 0) || (nb == 0))	{			\
	PROTECT(sy = allocVector(REALSXP, 0));		\
	if (na == 0) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
	UNPROTECT(1);					\
	return(sy);					\
    }							\
    n = (na < nb) ? nb : na;				\
    PROTECT(sa = coerceVector(sa, REALSXP));		\
    PROTECT(sb = coerceVector(sb, REALSXP));		\
    PROTECT(sy = allocVector(REALSXP, n));		\
    a = REAL(sa);					\
    b = REAL(sb);					\
    y = REAL(sy);					\
    naflag = 0

#define FINISH_Math2					\
    if(naflag) warning(R_MSG_NA);			\
    if (n == na)  SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    UNPROTECT(3)

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;


static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI, double (*f)(double, double, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int m_opt;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	error(R_MSG_NONNUM_MATH);

    SETUP_Math2;
    m_opt = asInteger(sI);

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, m_opt);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int i_1, i_2;
    int naflag;
    if (!isNumeric(sa) || !isNumeric(sb))
	error(R_MSG_NONNUM_MATH);

    SETUP_Math2;
    i_1 = asInteger(sI1);
    i_2 = asInteger(sI2);

    mod_iterate(na, nb, ia, ib) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_2() */

#define DEFMATH2_1(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sI) { \
        return math2_1(sa, sb, sI, name); \
    }

DEFMATH2_1(dchisq)
DEFMATH2_1(dexp)
DEFMATH2_1(dgeom)
DEFMATH2_1(dpois)
DEFMATH2_1(dt)
DEFMATH2_1(dsignrank)

#define DEFMATH2_2(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sI, SEXP sJ) { \
        return math2_2(sa, sb, sI, sJ, name); \
    }

DEFMATH2_2(pchisq)
DEFMATH2_2(qchisq)
DEFMATH2_2(pexp)
DEFMATH2_2(qexp)
DEFMATH2_2(pgeom)
DEFMATH2_2(qgeom)
DEFMATH2_2(ppois)
DEFMATH2_2(qpois)
DEFMATH2_2(pt)
DEFMATH2_2(qt)
DEFMATH2_2(psignrank)
DEFMATH2_2(qsignrank)

/* Mathematical Functions of Three (Real) Arguments */

#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define mod_iterate3(n1,n2,n3,i1,i2,i3) for (i=i1=i2=i3=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,				\
	i2 = (++i2==n2) ? 0 : i2,				\
	i3 = (++i3==n3) ? 0 : i3,				\
	++i)

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	error(R_MSG_NONNUM_MATH);			\
								\
    na = XLENGTH(sa);						\
    nb = XLENGTH(sb);						\
    nc = XLENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0))			\
	return(allocVector(REALSXP, 0));			\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    a = REAL(sa);						\
    b = REAL(sb);						\
    c = REAL(sc);						\
    y = REAL(sy);						\
    naflag = 0

#define FINISH_Math3					\
    if(naflag)  warning(R_MSG_NA);			\
    							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    UNPROTECT(4)

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1,i_2;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_2 */

#define DEFMATH3_1(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sc, SEXP sI) { \
        return math3_1(sa, sb, sc, sI, name); \
    }

DEFMATH3_1(dbeta)
DEFMATH3_1(dbinom)
DEFMATH3_1(dcauchy)
DEFMATH3_1(df)
DEFMATH3_1(dgamma)
DEFMATH3_1(dlnorm)
DEFMATH3_1(dlogis)
DEFMATH3_1(dnbinom)
DEFMATH3_1(dnbinom_mu)
DEFMATH3_1(dnorm)
DEFMATH3_1(dweibull)
DEFMATH3_1(dunif)
DEFMATH3_1(dnt)
DEFMATH3_1(dnchisq)
DEFMATH3_1(dwilcox)

#define DEFMATH3_2(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ) { \
        return math3_2(sa, sb, sc, sI, sJ, name); \
    }

DEFMATH3_2(pbeta)
DEFMATH3_2(qbeta)
DEFMATH3_2(pbinom)
DEFMATH3_2(qbinom)
DEFMATH3_2(pcauchy)
DEFMATH3_2(qcauchy)
DEFMATH3_2(pf)
DEFMATH3_2(qf)
DEFMATH3_2(pgamma)
DEFMATH3_2(qgamma)
DEFMATH3_2(plnorm)
DEFMATH3_2(qlnorm)
DEFMATH3_2(plogis)
DEFMATH3_2(qlogis)
DEFMATH3_2(pnbinom)
DEFMATH3_2(qnbinom)
DEFMATH3_2(pnbinom_mu)
DEFMATH3_2(qnbinom_mu)
DEFMATH3_2(pnorm)
DEFMATH3_2(qnorm)
DEFMATH3_2(pweibull)
DEFMATH3_2(qweibull)
DEFMATH3_2(punif)
DEFMATH3_2(qunif)
DEFMATH3_2(pnt)
DEFMATH3_2(qnt)
DEFMATH3_2(pnchisq)
DEFMATH3_2(qnchisq)
DEFMATH3_2(pwilcox)
DEFMATH3_2(qwilcox)

/* Mathematical Functions of Four (Real) Arguments */

#define if_NA_Math4_set(y,a,b,c,d)				\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;

#define mod_iterate4(n1,n2,n3,n4,i1,i2,i3,i4) for (i=i1=i2=i3=i4=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,					\
	i2 = (++i2==n2) ? 0 : i2,					\
	i3 = (++i3==n3) ? 0 : i3,					\
	i4 = (++i4==n4) ? 0 : i4,					\
	++i)

#define SETUP_Math4							\
    if(!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	error(R_MSG_NONNUM_MATH);				\
									\
    na = XLENGTH(sa);							\
    nb = XLENGTH(sb);							\
    nc = XLENGTH(sc);							\
    nd = XLENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))		\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    y = REAL(sy);							\
    naflag = 0

#define FINISH_Math4					\
    if(naflag) warning(R_MSG_NA);			\
    							\
    if (n == na) SHALLOW_DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) SHALLOW_DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) SHALLOW_DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) SHALLOW_DUPLICATE_ATTRIB(sy, sd);	\
    UNPROTECT(5)

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)(double, double, double, double, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, double, int, int))
{
    SEXP sy;
    R_xlen_t i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1, i_2;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_2() */

#define DEFMATH4_1(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI) { \
        return math4_1(sa, sb, sc, sd, sI, name); \
    }

DEFMATH4_1(dhyper)
DEFMATH4_1(dnbeta)
DEFMATH4_1(dnf)

#define DEFMATH4_2(name) \
    SEXP do_##name(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ) { \
        return math4_2(sa, sb, sc, sd, sI, sJ, name); \
    }

DEFMATH4_2(phyper)
DEFMATH4_2(qhyper)
DEFMATH4_2(pnbeta)
DEFMATH4_2(qnbeta)
DEFMATH4_2(pnf)
DEFMATH4_2(qnf)
DEFMATH4_2(ptukey)
DEFMATH4_2(qtukey)

/* These are here to get them in the correct package */


/* from src/nmath/wilcox.c */
extern void signrank_free(void);
extern void wilcox_free(void);

SEXP stats_signrank_free(void)
{
    signrank_free();
    return R_NilValue;
}

SEXP stats_wilcox_free(void)
{
    wilcox_free();
    return R_NilValue;
}
