/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Mathlib.h"

#define R_INT_MIN 1+INT_MIN
#define Int2Real(i) (i == NA_INTEGER) ? NA_REAL : (double)i;


#ifdef DEBUG_sum
#define DbgP1(s) REprintf(s)
#define DbgP2(s,a) REprintf(s,a)
#define DbgP3(s,a,b) REprintf(s,a,b)
#else
#define DbgP1(s)
#define DbgP2(s,a)
#define DbgP3(s,a,b)
#endif

/* These GLOBALS are set/initialized in do_summary: */
static int narm;
static int updated;
	/* updated := 1 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned */

static void isum(int *x, int n, int *value)
{
    double s;
    int i;
    for (i = 0, s = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if(!updated) updated = 1;
	    s += x[i];
	} else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_INTEGER;
	    return;
	}
    }
    if(s > INT_MAX || s < R_INT_MIN){
	warning("Integer overflow in sum(.); use sum(as.numeric(.))");
	*value = NA_INTEGER;
    }
    else *value = s;
}

static void rsum(double *x, int n, double *value)
{
    double s;
    int i;
    for (i = 0, s = 0; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if(!updated) updated = 1;
	    s += x[i];
	}
	else if (!narm) {
	    if(!updated) updated = 1;
#ifdef IEEE_754
	    s += x[i];
#else
	    *value = NA_REAL;
	    return;
#endif
	}
    }
    *value = s;
}

static void csum(complex *x, int n, complex *value)
{
    complex s;
    int i;
    s.r = s.i = 0;
    for (i = 0; i < n; i++) {
	if ((!ISNAN(x[i].r) && !ISNAN(x[i].i))
#ifdef IEEE_754
	    || !narm
#endif
	    ) {
	    if(!updated) updated=1;
	    s.r += x[i].r;
	    s.i += x[i].i;
#ifndef IEEE_754
	}
	else if (!narm) {
	    if(!updated) updated=1;
	    value->r = value->i = NA_REAL;
	    return;
#endif
	}
    }
    value->r = s.r;
    value->i = s.i;
}

static void imin(int *x, int n, int *value)
{
    int i, s;
    s = INT_MAX;
    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (s > x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_INTEGER;
	    return;
	}
    }
    *value = s;
}

static void rmin(double *x, int n, double *value)
{
    double s;
    int i;
#ifdef IEEE_754
    s = R_PosInf;
    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {
	    if (!narm) {
		s += x[i];/* Na(N) */
		if(!updated) updated = 1;
	    }
	} else if (x[i] < s) {
	    s = x[i];
	    if(!updated) updated = 1;
	}
    }
    *value = /* (!updated) ? NA_REAL : */ s;
#else
    s = NA_REAL;
    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if (ISNAN(s) || s > x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_REAL;
	    return;
	}
    }
    *value = s;
#endif
}

static void imax(int *x, int n, int *value)
{
    int i, s;
    s = R_INT_MIN;
    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (s < x[i]) {
		s = x[i];
		if(!updated) updated = 1;
	    }
	} else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_INTEGER;
	    return;
	}
    }
    *value = s;
}

static void rmax(double *x, int n, double *value)
{
    double s;
    int i;
#ifdef IEEE_754
    s = R_NegInf;
    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {
	    if (!narm) {
		s += x[i];/* Na(N) */
		if(!updated) updated = 1;
	    }
	}
	else if (x[i] > s) {
	    s = x[i];
	    if(!updated) updated = 1;
	}
    }
    *value = /* (!updated) ? NA_REAL : */ s;
#else
    s = NA_REAL;
    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if (ISNAN(s) || s < x[i])
		s = x[i];
	    if(!updated) updated = 1;
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_REAL;
	    return;
	}
    }
    *value = s;
#endif
}

static void iprod(int *x, int n, double *value)
{
    double s;
    int i;
    s = 1;
    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    s = MATH_CHECK(s * x[i]);
	    if(!updated) updated = 1;
	}
	else if (!narm) {
	    if(!updated) updated = 1;
	    *value = NA_REAL;
	    return;
	}

	if(ISNAN(s)) {
	    *value = NA_REAL;
	    return;
	}
    }
    *value = s;
}

static void rprod(double *x, int n, double *value)
{
    double s;
    int i;
    for (i = 0, s = 1; i < n; i++) {
	if (!ISNAN(x[i])) {
	    if(!updated) updated = 1;
	    s = MATH_CHECK(s * x[i]);
	}
	else if (!narm) {
	    if(!updated) updated = 1;
#ifdef IEEE_754
	    s *= x[i];/* Na(N) */
#else
	    *value = NA_REAL;
	    return;
#endif
	}
    }
    *value = s;
}

static void cprod(complex *x, int n, complex *value)
{
    complex s, t;
    int i;
    s.r = 1;
    s.i = 0;
    for (i = 0; i < n; i++) {
	if ((!ISNAN(x[i].r) && !ISNAN(x[i].i))
#ifdef IEEE_754
	    || !narm
#endif
	    ) {
	    if(!updated) updated = 1;
	    t.r = s.r;
	    t.i = s.i;
	    s.r = MATH_CHECK(t.r * x[i].r - t.i * x[i].i);
	    s.i = MATH_CHECK(t.r * x[i].i + t.i * x[i].r);
	}
#ifndef IEEE_754
	else if (!narm) {
	    if(!updated) updated = 1;
	    value->r = value->i = NA_REAL;
	    return;
	}
	if(ISNAN(s.r) || ISNAN(s.i)) {
	    value->r = value->i = NA_REAL;
	    return;
	}
#endif
    }
    value->r = s.r;
    value->i = s.i;
}


/* do_summary provides a variety of data summaries
	op : 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod */
/* NOTE: mean() [op = 1]  is no longer processed by this code.
		(NEVER was correct for multiple arguments!) */

SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a;
    double tmp;
    complex z, ztmp, zcum;
    int itmp, icum=0, int_a, empty;
    short iop;
    SEXPTYPE ans_type;/* only INTEGER, REAL, or COMPLEX here */

    if(DispatchGroup("Summary",call, op, args, env, &ans))
	return ans;

    ans = matchArg(R_NaRmSymbol, &args);
    narm = asLogical(ans);
    updated = 0;
    empty = 1;/*- =1: only zero-length arguments, or NA with na.rm=T */

    iop = PRIMVAL(op);
    switch(iop) {
    case 0:/* sum */
	ans_type = INTSXP;/* try to keep if possible.. */
	zcum.r = zcum.i = 0.; icum = 0; break;

    case 2:/* min */
	DbgP2("do_summary: min(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
#ifdef IEEE_754
	zcum.r = R_PosInf;
#else
	zcum.r = NA_REAL;
#endif
	icum = INT_MAX; break;

    case 3:/* max */
	DbgP2("do_summary: max(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
#ifdef IEEE_754
	zcum.r = R_NegInf;;
#else
	zcum.r = NA_REAL;
#endif
	icum = R_INT_MIN;/* since INT_MIN is the NA_INTEGER value !! */
	break;

    case 4:/* prod */
	ans_type = REALSXP;
	zcum.r = 1.;
	zcum.i = 0.;
	break;

    default:
	errorcall(call,"internal error ('op' in do_summary).\t Call a Guru\n");
	return R_NilValue;/*-Wall */
    }

    /*-- now loop over all arguments.  Do the 'op' switch INSIDE : */
    while (args != R_NilValue) {
	a = CAR(args);
	int_a = 0;/* int_a = 1	<-->	a is INTEGER */

	if(length(a) > 0) {
	    updated = 0;/*- GLOBAL -*/

	    switch(iop) {
	    case 2:/* min */
	    case 3:/* max */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP: int_a = 1;
		    if (iop == 2) imin(INTEGER(a), length(a), &itmp);
		    else	  imax(INTEGER(a), length(a), &itmp);
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    if (iop == 2) rmin(REAL(a), length(a), &tmp);
		    else	  rmax(REAL(a), length(a), &tmp);
		    break;
		default:
		    goto badmode;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (itmp == NA_INTEGER) goto na_answer;
			if ((iop==2 && itmp < icum)    /* min */
			    ||(iop==3 && itmp > icum)) /* max */
			    icum = itmp;
		    } else { /* real */
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNAN(tmp)) {
#ifdef IEEE_754
			    zcum.r += tmp;/* NA or NaN */
#else
			    goto na_answer;
#endif
			} else if(
#ifndef IEEE_754
			    ISNAN(zcum.r) ||
#endif
			    (iop==2 && tmp < zcum.r) ||
			    (iop==3 && tmp > zcum.r))	zcum.r = tmp;
		    }
		}/*updated*/ else {
		    /*-- in what cases does this happen here at all? */
		    DbgP2(" NOT updated [!! RARE !!]: int_a=%d\n", int_a);
		}

		break;/*--- end of  min() / max() ---*/

	    case 0:/* sum */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    isum(INTEGER(a), length(a), &itmp);
		    if(updated) {
			if(itmp == NA_INTEGER) goto na_answer;
			if(ans_type == INTSXP)
			    icum += itmp;
			else
			    zcum.r += Int2Real(itmp);
		    }
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    rsum(REAL(a), length(a), &tmp);
		    if(updated) {
#ifndef IEEE_754
			if(ISNAN(tmp)) goto na_answer;
#endif
			zcum.r += tmp;
		    }
		    break;
		case CPLXSXP:
		    if(ans_type == INTSXP) {
			ans_type = CPLXSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    } else if (ans_type == REALSXP)
			ans_type = CPLXSXP;
		    csum(COMPLEX(a), length(a), &ztmp);
		    if(updated) {
#ifndef IEEE_754
			if(ISNAN(ztmp.r)) goto na_answer;
#endif
			zcum.r += ztmp.r;
			zcum.i += ztmp.i;
		    }
		    break;
		default:
		    goto badmode;
		}

		break;/* sum() part */

	    case 4:/* prod */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		    if(TYPEOF(a) == REALSXP)
			rprod(REAL(a), length(a), &tmp);
		    else
			iprod(INTEGER(a), length(a), &tmp);
		    if(updated) {
#ifndef IEEE_754
			if(ISNAN(tmp)) goto na_answer;
#endif
			zcum.r *= tmp;
			zcum.i *= tmp;
		    }
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    cprod(COMPLEX(a), length(a), &ztmp);
		    if(updated) {
#ifndef IEEE_754
			if(ISNAN(ztmp.r)) goto na_answer;
#endif
			z.r = zcum.r;
			z.i = zcum.i;
			zcum.r = MATH_CHECK(z.r * ztmp.r - z.i * ztmp.i);
			zcum.i = MATH_CHECK(z.r * ztmp.i + z.i * ztmp.r);
		    }
		    break;
		default:
		    goto badmode;
		}

		break;/* prod() part */

	    }/* switch(iop) */

	} else/*len(a)=0*/ if(ans_type < TYPEOF(a) && ans_type != CPLXSXP) {
	    if(!empty && ans_type == INTSXP)
		zcum.r = Int2Real(icum);
	    ans_type = TYPEOF(a);
	}
	DbgP3(" .. upd.=%d, empty: old=%d", updated, empty);
	if(empty && updated) empty=0;
	DbgP2(", new=%d\n", empty);
	args = CDR(args);
    } /*-- while(..) loop over args */

    /*-------------------------------------------------------*/
    if(empty && (iop == 2 || iop == 3))
	warningcall(call,"no finite arguments to min/max; returning extreme.");

    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = icum;break;
    case REALSXP:	REAL(ans)[0] = zcum.r; break;
    case CPLXSXP:
	COMPLEX(ans)[0].r = zcum.r;
	COMPLEX(ans)[0].i = zcum.i;
    }
    return ans;

na_answer: /* even for IEEE, for INT : */
    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = NA_INTEGER; break;
    case REALSXP:	REAL(ans)[0] = NA_REAL; break;
    case CPLXSXP:	COMPLEX(ans)[0].r = COMPLEX(ans)[0].i = NA_REAL;
    }
    return ans;


badmode:
    errorcall(call, "invalid \"mode\" of argument\n");
    return R_NilValue;/*-Wall */
}/* do_summary */

SEXP do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Summary", call, op, args, env, &ans))
	return(ans);
    PROTECT(op = findFun(install("range.default"), env));
    ans = applyClosure(call, op, args, env, R_NilValue);
    UNPROTECT(1);
    return(ans);
}


SEXP do_compcases(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, t, u, rval;
    int i, len;

    /* checkArity(op, args); */
    len = -1;

    for (s = args; s != R_NilValue; s = CDR(s)) {
	if (isList(CAR(s))/* || isFrame(CAR(s)) */) {
	    for (t = CAR(s); t != R_NilValue; t = CDR(t))
		if (isMatrix(CAR(t))) {
		    u = getAttrib(CAR(t), R_DimSymbol);
		    if (len < 0)
			len = INTEGER(u)[0];
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
		else if (isVector(CAR(t))) {
		    if (len < 0)
			len = LENGTH(CAR(t));
		    else if (len != LENGTH(CAR(t)))
			goto bad;
		}
		else
		    goto bad_mode;
	}
	/* FIXME : Need to be careful with the use of isVector() */
	/* since this includes the new list structure and expressions. */
	else if (isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = length(t);
	    for (it = 0 ; it < nt ; it++) {
		if (isMatrix(VECTOR(t)[it])) {
		    u = getAttrib(VECTOR(t)[it], R_DimSymbol);
		    if (len < 0)
			len = INTEGER(u)[0];
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
		else if (isVector(VECTOR(t)[it])) {
		    if (len < 0)
			len = LENGTH(VECTOR(t)[it]);
		    else if (len != LENGTH(VECTOR(t)[it]))
			goto bad;
		}
		else
		    goto bad_mode;
	    }

	}
	else if (isMatrix(CAR(s))) {
	    u = getAttrib(CAR(s), R_DimSymbol);
	    if (len < 0)
		len = INTEGER(u)[0];
	    else if (len != INTEGER(u)[0])
		goto bad;
	}
	else if (isVector(CAR(s))) {
	    if (len < 0)
		len = LENGTH(CAR(s));
	    else if (len != LENGTH(CAR(s)))
		goto bad;
	}
	else
	    goto bad_mode;
    }
    PROTECT(rval = allocVector(LGLSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(rval)[i] = 1;
    /* FIXME : there is a lot of shared code here for vectors. */
    /* It should be abstracted out and optimized. */
    for (s = args; s != R_NilValue; s = CDR(s)) {
	if (isList(CAR(s)) /* || isFrame(CAR(s))*/) {
	    /* Now we only need to worry about vectors */
	    /* since we use mod to handle arrays. */
	    /* FIXME : using mod like this causes */
	    /* a potential performance hit. */
	    for (t = CAR(s); t != R_NilValue; t = CDR(t)) {
		u = CAR(t);
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING(u)[i] == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			goto bad_mode;
		    }
		}
	    }
	}
	if (isNewList(CAR(s))) {
	    int it, nt;
	    t = CAR(s);
	    nt = length(t);
	    for (it = 0 ; it < nt ; it++) {
		u = VECTOR(t)[it];
		for (i = 0; i < LENGTH(u); i++) {
		    switch (TYPEOF(u)) {
		    case INTSXP:
		    case LGLSXP:
			if (INTEGER(u)[i] == NA_INTEGER)
			    INTEGER(rval)[i % len] = 0;
			break;
		    case REALSXP:
			if (ISNAN(REAL(u)[i]))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case CPLXSXP:
			if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			    INTEGER(rval)[i % len] = 0;
			break;
		    case STRSXP:
			if (STRING(u)[i] == NA_STRING)
			    INTEGER(rval)[i % len] = 0;
			break;
		    default:
			UNPROTECT(1);
			goto bad_mode;
		    }
		}
	    }
	}
	else {
	    for (i = 0; i < LENGTH(CAR(s)); i++) {
		u = CAR(s);
		switch (TYPEOF(u)) {
		case INTSXP:
		case LGLSXP:
		    if (INTEGER(u)[i] == NA_INTEGER)
			INTEGER(rval)[i % len] = 0;
		    break;
		case REALSXP:
		    if (ISNAN(REAL(u)[i]))
			INTEGER(rval)[i % len] = 0;
		    break;
		case CPLXSXP:
		    if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
			INTEGER(rval)[i % len] = 0;
		    break;
		case STRSXP:
		    if (STRING(u)[i] == NA_STRING)
			INTEGER(rval)[i % len] = 0;
		    break;
		default:
		    UNPROTECT(1);
		    goto bad_mode;
		}
	    }
	}
    }
    UNPROTECT(1);
    return rval;

 bad:
    error("complete.cases: not all arguments have the same length\n");

 bad_mode:
    error("complete.cases: invalid mode of argument\n");

    return R_NilValue;		/* NOTREACHED; for -Wall */
}
