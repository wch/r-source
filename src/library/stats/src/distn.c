/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2012	    The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include "statsR.h"

/* interval at which to check interrupts */
//#define NINTERRUPT 1000000


#define R_MSG_NA	_("NaNs produced")
#define R_MSG_NONNUM_MATH _("Non-numeric argument to mathematical function")


/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

#define SETUP_Math2				\
    na = XLENGTH(sa);				\
    nb = XLENGTH(sb);				\
    if ((na == 0) || (nb == 0))	{		\
	PROTECT(sy = allocVector(REALSXP, 0));	\
	if (na == 0) DUPLICATE_ATTRIB(sy, sa);	\
	UNPROTECT(1);				\
	return(sy);				\
    }						\
    n = (na < nb) ? nb : na;			\
    PROTECT(sa = coerceVector(sa, REALSXP));	\
    PROTECT(sb = coerceVector(sb, REALSXP));	\
    PROTECT(sy = allocVector(REALSXP, n));	\
    a = REAL(sa);				\
    b = REAL(sb);				\
    y = REAL(sy);				\
    naflag = 0

#define FINISH_Math2				\
    if(naflag) warning(R_MSG_NA);		\
    if (n == na)  DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
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

#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN)
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN)

SEXP distn2(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    const char *dn = CHAR(STRING_ELT(getListElement(CAR(args), "name"), 0));
    args = CDR(args);

    if (streql(dn, "dchisq")) return Math2_1(args, dchisq);
    else if (streql(dn, "pchisq")) return Math2_2(args, pchisq);
    else if (streql(dn, "qchisq")) return Math2_2(args, qchisq);
    else if (streql(dn, "dexp")) return Math2_1(args, dexp);
    else if (streql(dn, "pexp")) return Math2_2(args, pexp);
    else if (streql(dn, "qexp")) return Math2_2(args, qexp);
    else if (streql(dn, "dgeom")) return Math2_1(args, dgeom);
    else if (streql(dn, "pgeom")) return Math2_2(args, pgeom);
    else if (streql(dn, "qgeom")) return Math2_2(args, qgeom);
    else if (streql(dn, "dpois")) return Math2_1(args, dpois);
    else if (streql(dn, "ppois")) return Math2_2(args, ppois);
    else if (streql(dn, "qpois")) return Math2_2(args, qpois);
    else if (streql(dn, "dt")) return Math2_1(args, dt);
    else if (streql(dn, "pt")) return Math2_2(args, pt);
    else if (streql(dn, "qt")) return Math2_2(args, qt);
    else if (streql(dn, "dsignrank")) return Math2_1(args, dsignrank);
    else if (streql(dn, "psignrank")) return Math2_2(args, psignrank);
    else if (streql(dn, "qsignrank")) return Math2_2(args, qsignrank);
    else error("unknown distribution %s", dn);
    return R_NilValue;
}


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

#define FINISH_Math3				\
    if(naflag)  warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
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

#define Math3_1(A, FUN)	math3_1(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN)
#define Math3_2(A, FUN) math3_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN)

SEXP distn3(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    const char *dn = CHAR(STRING_ELT(getListElement(CAR(args), "name"), 0));
    args = CDR(args);

    if (streql(dn, "dbeta")) return Math3_1(args, dbeta);
    else if (streql(dn, "pbeta")) return Math3_2(args, pbeta);
    else if (streql(dn, "qbeta")) return Math3_2(args, qbeta);
    else if (streql(dn, "dbinom")) return Math3_1(args, dbinom);
    else if (streql(dn, "pbinom")) return Math3_2(args, pbinom);
    else if (streql(dn, "qbinom")) return Math3_2(args, qbinom);
    else if (streql(dn, "dcauchy")) return Math3_1(args, dcauchy);
    else if (streql(dn, "pcauchy")) return Math3_2(args, pcauchy);
    else if (streql(dn, "qcauchy")) return Math3_2(args, qcauchy);
    else if (streql(dn, "df")) return Math3_1(args, df);
    else if (streql(dn, "pf")) return Math3_2(args, pf);
    else if (streql(dn, "qf")) return Math3_2(args, qf);
    else if (streql(dn, "dgamma")) return Math3_1(args, dgamma);
    else if (streql(dn, "pgamma")) return Math3_2(args, pgamma);
    else if (streql(dn, "qgamma")) return Math3_2(args, qgamma);
    else if (streql(dn, "dlnorm")) return Math3_1(args, dlnorm);
    else if (streql(dn, "plnorm")) return Math3_2(args, plnorm);
    else if (streql(dn, "qlnorm")) return Math3_2(args, qlnorm);
    else if (streql(dn, "dlogis")) return Math3_1(args, dlogis);
    else if (streql(dn, "plogis")) return Math3_2(args, plogis);
    else if (streql(dn, "qlogis")) return Math3_2(args, qlogis);
    else if (streql(dn, "dnbinom")) return Math3_1(args, dnbinom);
    else if (streql(dn, "pnbinom")) return Math3_2(args, pnbinom);
    else if (streql(dn, "qnbinom")) return Math3_2(args, qnbinom);
    else if (streql(dn, "dnbinom_mu")) return Math3_1(args, dnbinom_mu);
    else if (streql(dn, "pnbinom_mu")) return Math3_2(args, pnbinom_mu);
    else if (streql(dn, "qnbinom_mu")) return Math3_2(args, qnbinom_mu);
    else if (streql(dn, "dnorm")) return Math3_1(args, dnorm);
    else if (streql(dn, "pnorm")) return Math3_2(args, pnorm);
    else if (streql(dn, "qnorm")) return Math3_2(args, qnorm);
    else if (streql(dn, "dweibull")) return Math3_1(args, dweibull);
    else if (streql(dn, "pweibull")) return Math3_2(args, pweibull);
    else if (streql(dn, "qweibull")) return Math3_2(args, qweibull);
    else if (streql(dn, "dunif")) return Math3_1(args, dunif);
    else if (streql(dn, "punif")) return Math3_2(args, punif);
    else if (streql(dn, "qunif")) return Math3_2(args, qunif);
    else if (streql(dn, "dnt")) return Math3_1(args, dnt);
    else if (streql(dn, "pnt")) return Math3_2(args, pnt);
    else if (streql(dn, "qnt")) return Math3_2(args, qnt);
    else if (streql(dn, "dnchisq")) return Math3_1(args, dnchisq);
    else if (streql(dn, "pnchisq")) return Math3_2(args, pnchisq);
    else if (streql(dn, "qnchisq")) return Math3_2(args, qnchisq);
    else if (streql(dn, "dwilcox")) return Math3_1(args, dwilcox);
    else if (streql(dn, "pwilcox")) return Math3_2(args, pwilcox);
    else if (streql(dn, "qwilcox")) return Math3_2(args, qwilcox);
    else error("unknown distribution %s", dn);
    return R_NilValue;
}

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

#define FINISH_Math4				\
    if(naflag) warning(R_MSG_NA);		\
						\
    if (n == na) DUPLICATE_ATTRIB(sy, sa);	\
    else if (n == nb) DUPLICATE_ATTRIB(sy, sb);	\
    else if (n == nc) DUPLICATE_ATTRIB(sy, sc);	\
    else if (n == nd) DUPLICATE_ATTRIB(sy, sd);	\
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


#define CAD3R	CADDDR
/* This is not (yet) in Rinternals.h : */
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))

#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN)

SEXP distn4(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    const char *dn = CHAR(STRING_ELT(getListElement(CAR(args), "name"), 0));
    args = CDR(args);

    if (streql(dn, "dhyper")) return Math4_1(args, dhyper);
    else if (streql(dn, "phyper")) return Math4_2(args, phyper);
    else if (streql(dn, "qhyper")) return Math4_2(args, qhyper);
    else if (streql(dn, "dnbeta")) return Math4_1(args, dnbeta);
    else if (streql(dn, "pnbeta")) return Math4_2(args, pnbeta);
    else if (streql(dn, "qnbeta")) return Math4_2(args, qnbeta);
    else if (streql(dn, "dnf")) return Math4_1(args, dnf);
    else if (streql(dn, "pnf")) return Math4_2(args, pnf);
    else if (streql(dn, "qnf")) return Math4_2(args, qnf);
    else if (streql(dn, "ptukey")) return Math4_2(args, ptukey);
    else if (streql(dn, "qtukey")) return Math4_2(args, qtukey);
    else error("unknown distribution %s", dn);
    return R_NilValue;
}

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
