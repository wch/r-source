/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2014   The R Core Team
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
#include <Internal.h>
#include <R_ext/Itermacros.h>

#include <float.h> // for DBL_MAX

#include "duplicate.h"

#define R_MSG_type	_("invalid 'type' (%s) of argument")
#define imax2(x, y) ((x < y) ? y : x)

#define R_INT_MIN	(1+INT_MIN)
	/* since INT_MIN is the NA_INTEGER value ! */
#define Int2Real(i)	((i == NA_INTEGER) ? NA_REAL : (double)i)

#ifdef DEBUG_sum
#define DbgP1(s) REprintf(s)
#define DbgP2(s,a) REprintf(s,a)
#define DbgP3(s,a,b) REprintf(s,a,b)
#else
#define DbgP1(s)
#define DbgP2(s,a)
#define DbgP3(s,a,b)
#endif

#ifdef LONG_INT
static Rboolean isum(int *x, R_xlen_t n, int *value, Rboolean narm, SEXP call)
{
    LONG_INT s = 0;  // at least 64-bit
    Rboolean updated = FALSE;
#ifdef LONG_VECTOR_SUPPORT
    int ii = R_INT_MIN; // need > 2^32 entries to overflow.
#endif

    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if(!updated) updated = TRUE;
	    s += x[i];
#ifdef LONG_VECTOR_SUPPORT
	    if (ii++ > 1000) {
		ii = 0;
		if (s > 9000000000000000L || s < -9000000000000000L) {
		    if(!updated) updated = TRUE;
		    *value = NA_INTEGER;
		    warningcall(call, _("integer overflow - use sum(as.numeric(.))"));
		    return updated;
		}
	    }
#endif
	} else if (!narm) {
	    if(!updated) updated = TRUE;
	    *value = NA_INTEGER;
	    return updated;
	}
    }
    if(s > INT_MAX || s < R_INT_MIN){
	warningcall(call, _("integer overflow - use sum(as.numeric(.))"));
	*value = NA_INTEGER;
    }
    else *value = (int) s;

    return updated;
}
#else
/* Version from R 3.0.0: should never be used with a C99/C11 compiler */
static Rboolean isum(int *x, R_xlen_t n, int *value, Rboolean narm, SEXP call)
{
    double s = 0.0;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if(!updated) updated = TRUE;
	    s += x[i];
	} else if (!narm) {
	    if(!updated) updated = TRUE;
	    *value = NA_INTEGER;
	    return updated;
	}
    }
    if(s > INT_MAX || s < R_INT_MIN){
	warningcall(call, _("integer overflow - use sum(as.numeric(.))"));
	*value = NA_INTEGER;
    }
    else *value = (int) s;

    return updated;
}
#endif

static Rboolean rsum(double *x, R_xlen_t n, double *value, Rboolean narm)
{
    LDOUBLE s = 0.0;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (!narm || !ISNAN(x[i])) {
	    if(!updated) updated = TRUE;
	    s += x[i];
	}
    }
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean csum(Rcomplex *x, R_xlen_t n, Rcomplex *value, Rboolean narm)
{
    LDOUBLE sr = 0.0, si = 0.0;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (!narm || (!ISNAN(x[i].r) && !ISNAN(x[i].i))) {
	    if(!updated) updated = TRUE;
	    sr += x[i].r;
	    si += x[i].i;
	}
    }
    value->r = (double) sr;
    value->i = (double) si;

    return updated;
}

static Rboolean imin(int *x, R_xlen_t n, int *value, Rboolean narm)
{
    int s = 0 /* -Wall */;
    Rboolean updated = FALSE;

    /* Used to set s = INT_MAX, but this ignored INT_MAX in the input */
    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (!updated || s > x[i]) {
		s = x[i];
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = NA_INTEGER;
	    return(TRUE);
	}
    }
    *value = s;

    return updated;
}

static Rboolean rmin(double *x, R_xlen_t n, double *value, Rboolean narm)
{
    double s = 0.0; /* -Wall */
    Rboolean updated = FALSE;

    /* s = R_PosInf; */
    for (R_xlen_t i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(!ISNA(s)) s = x[i]; /* so any NA trumps all NaNs */
		if(!updated) updated = TRUE;
	    }
	}
	else if (!updated || x[i] < s) {  /* Never true if s is NA/NaN */
	    s = x[i];
	    if(!updated) updated = TRUE;
	}
    }
    *value = s;

    return updated;
}

static Rboolean smin(SEXP x, SEXP *value, Rboolean narm)
{
    SEXP s = NA_STRING; /* -Wall */
    Rboolean updated = FALSE;
    const void *vmax = vmaxget(); // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) > 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return(TRUE);
	}
    }
    *value = s;

    vmaxset(vmax);
    return updated;
}

static Rboolean imax(int *x, R_xlen_t n, int *value, Rboolean narm)
{
    int s = 0 /* -Wall */;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if (!updated || s < x[i]) {
		s = x[i];
		if(!updated) updated = TRUE;
	    }
	} else if (!narm) {
	    *value = NA_INTEGER;
	    return(TRUE);
	}
    }
    *value = s;

    return updated;
}

static Rboolean rmax(double *x, R_xlen_t n, double *value, Rboolean narm)
{
    double s = 0.0 /* -Wall */;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(!ISNA(s)) s = x[i]; /* so any NA trumps all NaNs */
		if(!updated) updated = TRUE;
	    }
	}
	else if (!updated || x[i] > s) {  /* Never true if s is NA/NaN */
	    s = x[i];
	    if(!updated) updated = TRUE;
	}
    }
    *value = s;

    return updated;
}

static Rboolean smax(SEXP x, SEXP *value, Rboolean narm)
{
    SEXP s = NA_STRING; /* -Wall */
    Rboolean updated = FALSE;
    const void *vmax = vmaxget(); // precautionary for Scollate

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
	if (STRING_ELT(x, i) != NA_STRING) {
	    if (!updated ||
		(s != STRING_ELT(x, i) && Scollate(s, STRING_ELT(x, i)) < 0)) {
		s = STRING_ELT(x, i);
		if(!updated) updated = TRUE;
	    }
	}
	else if (!narm) {
	    *value = NA_STRING;
	    return(TRUE);
	}
    }
    *value = s;

    vmaxset(vmax);
    return updated;
}

static Rboolean iprod(int *x, R_xlen_t n, double *value, Rboolean narm)
{
    LDOUBLE s = 1.0;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    s *= x[i];
	    if(!updated) updated = TRUE;
	}
	else if (!narm) {
	    if(!updated) updated = TRUE;
	    *value = NA_REAL;
	    return updated;
	}

	if(ISNAN(s)) {  /* how can this happen? */
	    *value = NA_REAL;
	    return updated;
	}
    }
    // This could over/underflow (does in package POT)
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean rprod(double *x, R_xlen_t n, double *value, Rboolean narm)
{
    LDOUBLE s = 1.0;
    Rboolean updated = FALSE;

    for (R_xlen_t i = 0; i < n; i++) {
	if (!narm || !ISNAN(x[i])) {
	    if(!updated) updated = TRUE;
	    s *= x[i];
	}
    }
    if(s > DBL_MAX) *value = R_PosInf;
    else if (s < -DBL_MAX) *value = R_NegInf;
    else *value = (double) s;

    return updated;
}

static Rboolean cprod(Rcomplex *x, R_xlen_t n, Rcomplex *value, Rboolean narm)
{
    LDOUBLE sr = 1.0, si = 0.0;
    Rboolean updated = FALSE;
    for (R_xlen_t i = 0; i < n; i++) {
	if (!narm || (!ISNAN(x[i].r) && !ISNAN(x[i].i))) {
	    if(!updated) updated = TRUE;
	    LDOUBLE tr = sr, ti = si;
	    sr = tr * x[i].r - ti * x[i].i;
	    si = tr * x[i].i + ti * x[i].r;
	}
    }
    value->r = (double) sr;
    value->i = (double) si;

    return updated;
}


attribute_hidden
SEXP fixup_NaRm(SEXP args)
{
    SEXP t, na_value;

    /* Need to make sure na.rm is last and exists */
    na_value = ScalarLogical(FALSE);
    for(SEXP a = args, prev = R_NilValue; a != R_NilValue; a = CDR(a)) {
	if(TAG(a) == R_NaRmSymbol) {
	    if(CDR(a) == R_NilValue) return args;
	    na_value = CAR(a);
	    if(prev == R_NilValue) args = CDR(a);
	    else SETCDR(prev, CDR(a));
	}
	prev = a;
    }

    PROTECT(na_value);
    t = CONS(na_value, R_NilValue);
    UNPROTECT(1);
    PROTECT(t);
    SET_TAG(t, R_NaRmSymbol);
    if (args == R_NilValue)
	args = t;
    else {
	SEXP r = args;
	while (CDR(r) != R_NilValue) r = CDR(r);
	SETCDR(r, t);
    }
    UNPROTECT(1);
    return args;
}

/* do_summary provides a variety of data summaries
	op : 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod
 */
/* NOTE: mean() is rather different as only one arg and no na.rm, and
 * dispatch is from an R-level generic, this being a special case of
 * mean.default.
 */

SEXP attribute_hidden do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a, stmp = NA_STRING /* -Wall */, scum = NA_STRING, call2;
    double tmp = 0.0, s;
    Rcomplex z, ztmp, zcum={0.0, 0.0} /* -Wall */;
    int itmp = 0, icum = 0, int_a, real_a, empty, warn = 0 /* dummy */;
    SEXPTYPE ans_type;/* only INTEGER, REAL, COMPLEX or STRSXP here */

    Rboolean narm;
    int updated;
	/* updated := 1 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned */

    if(PRIMVAL(op) == 1) { /* mean */
	LDOUBLE s = 0., si = 0., t = 0., ti = 0.;
	R_xlen_t i, n = XLENGTH(CAR(args));
	SEXP x = CAR(args);
	switch(TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    PROTECT(ans = allocVector(REALSXP, 1));
	    for (i = 0; i < n; i++) {
		if(INTEGER(x)[i] == NA_INTEGER) {
		    REAL(ans)[0] = R_NaReal;
		    UNPROTECT(1); /* ans */
		    return ans;
		}
		s += INTEGER(x)[i];
	    }
	    REAL(ans)[0] = (double) (s/n);
	    break;
	case REALSXP:
	    PROTECT(ans = allocVector(REALSXP, 1));
	    for (i = 0; i < n; i++) s += REAL(x)[i];
	    s /= n;
	    if(R_FINITE((double)s)) {
		for (i = 0; i < n; i++) t += (REAL(x)[i] - s);
		s += t/n;
	    }
	    REAL(ans)[0] = (double) s;
	    break;
	case CPLXSXP:
	    PROTECT(ans = allocVector(CPLXSXP, 1));
	    for (i = 0; i < n; i++) {
		s += COMPLEX(x)[i].r;
		si += COMPLEX(x)[i].i;
	    }
	    s /= n; si /= n;
	    if( R_FINITE((double)s) && R_FINITE((double)si) ) {
		for (i = 0; i < n; i++) {
		    t += COMPLEX(x)[i].r - s;
		    ti += COMPLEX(x)[i].i - si;
		}
		s += t/n; si += ti/n;
	    }
	    COMPLEX(ans)[0].r = (double) s;
	    COMPLEX(ans)[0].i = (double) si;
	    break;
	default:
	    error(R_MSG_type, type2char(TYPEOF(x)));
	    ans = R_NilValue; // -Wall on clang 4.2
	}
	UNPROTECT(1); /* ans */
	return ans;
    }

    /* match to foo(..., na.rm=FALSE) */
    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2); /* call2, args */
	return(ans);
    }
    UNPROTECT(1); /* call2 */

#ifdef DEBUG_Summary
    REprintf("C do_summary(op%s, *): did NOT dispatch\n", PRIMNAME(op));
#endif

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);
    updated = 0;
    empty = 1;/*- =1: only zero-length arguments, or NA with na.rm=T */

    int iop = PRIMVAL(op);
    switch(iop) {
    case 0:/* sum */
    /* we need to find out if _all_ the arguments are integer or logical
       in advance, as we might overflow before we find out.  NULL is
       documented to be the same as integer(0).
    */
	a = args;
	int_a = 1;
	while (a != R_NilValue) {
	    if(!isInteger(CAR(a)) &&  !isLogical(CAR(a)) && !isNull(CAR(a))) {
		int_a = 0;
		break;
	    }
	    a = CDR(a);
	}
	ans_type = int_a ? INTSXP: REALSXP; /* try to keep if possible.. */
	zcum.r = zcum.i = 0.; icum = 0;
	break;

    case 2:/* min */
	DbgP2("do_summary: min(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_PosInf;
	icum = INT_MAX;
	break;

    case 3:/* max */
	DbgP2("do_summary: max(.. na.rm=%d) ", narm);
	ans_type = INTSXP;
	zcum.r = R_NegInf;;
	icum = R_INT_MIN;
	break;

    case 4:/* prod */
	ans_type = REALSXP;
	zcum.r = 1.;
	zcum.i = 0.;
	break;

    default:
	errorcall(call,
		  _("internal error ('op = %d' in do_summary).\t Call a Guru"),
		  iop);
	return R_NilValue;/*-Wall */
    }

    /*-- now loop over all arguments.  Do the 'op' switch INSIDE : */
    PROTECT(scum);
    while (args != R_NilValue) {
	a = CAR(args);
	int_a = 0;/* int_a = 1	<-->	a is INTEGER */
	real_a = 0;

	if(xlength(a) > 0) {
	    updated = 0;/*- GLOBAL -*/

	    switch(iop) {
	    case 2:/* min */
	    case 3:/* max */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    int_a = 1;
		    if (iop == 2) updated = imin(INTEGER(a), XLENGTH(a), &itmp, narm);
		    else	  updated = imax(INTEGER(a), XLENGTH(a), &itmp, narm);
		    break;
		case REALSXP:
		    real_a = 1;
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    if (iop == 2) updated = rmin(REAL(a), XLENGTH(a), &tmp, narm);
		    else	  updated = rmax(REAL(a), XLENGTH(a), &tmp, narm);
		    break;
		case STRSXP:
		    if(!empty && ans_type == INTSXP) {
			scum = StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if(!empty && ans_type == REALSXP) {
			scum = StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    if (iop == 2) updated = smin(a, &stmp, narm);
		    else updated = smax(a, &stmp, narm);
		    break;
		default:
		    goto invalid_type;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (icum == NA_INTEGER); /* NA trumps anything */
			else if (itmp == NA_INTEGER ||
			    (iop == 2 && itmp < icum) || /* min */
			    (iop == 3 && itmp > icum))   /* max */
			    icum = itmp;
		    } else if(ans_type == REALSXP) {
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNA(zcum.r)); /* NA trumps anything */
			else if (ISNAN(tmp)) {
			    if (ISNA(tmp)) zcum.r = tmp;
			    else zcum.r += tmp;/* NA or NaN */
			} else if(
			    (iop == 2 && tmp < zcum.r) ||
			    (iop == 3 && tmp > zcum.r))	zcum.r = tmp;
		    } else if(ans_type == STRSXP) {
			if(empty) scum = stmp;
			else if (scum != NA_STRING) {
			    if(int_a)
				stmp = StringFromInteger(itmp, &warn);
			    if(real_a)
				stmp = StringFromReal(tmp, &warn);
			    PROTECT(stmp);
			    if(stmp == NA_STRING ||
			       (iop == 2 && stmp != scum && Scollate(stmp, scum) < 0) ||
			       (iop == 3 && stmp != scum && Scollate(stmp, scum) > 0) )
				scum = stmp;
			    UNPROTECT(1); /* stmp */
			}
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		}/*updated*/ else {
		    /*-- in what cases does this happen here at all?
		      -- if there are no non-missing elements.
		     */
		    DbgP2(" NOT updated [!! RARE !!]: int_a=%d\n", int_a);
		}

		break;/*--- end of  min() / max() ---*/

	    case 0:/* sum */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		    updated = isum(TYPEOF(a) == LGLSXP ?
				   LOGICAL(a) :INTEGER(a), XLENGTH(a),
				   &itmp, narm, call);
		    if(updated) {
			if(itmp == NA_INTEGER) goto na_answer;
			if(ans_type == INTSXP) {
			    s = (double) icum + (double) itmp;
			    if(s > INT_MAX || s < R_INT_MIN){
				warningcall(call,_("Integer overflow - use sum(as.numeric(.))"));
				goto na_answer;
			    }
			    else icum += itmp;
			} else
			    zcum.r += Int2Real(itmp);
		    }
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    updated = rsum(REAL(a), XLENGTH(a), &tmp, narm);
		    if(updated) {
			zcum.r += tmp;
		    }
		    break;
		case CPLXSXP:
		    if(ans_type == INTSXP) {
			ans_type = CPLXSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    } else if (ans_type == REALSXP)
			ans_type = CPLXSXP;
		    updated = csum(COMPLEX(a), XLENGTH(a), &ztmp, narm);
		    if(updated) {
			zcum.r += ztmp.r;
			zcum.i += ztmp.i;
		    }
		    break;
		default:
		    goto invalid_type;
		}

		break;/* sum() part */

	    case 4:/* prod */

		switch(TYPEOF(a)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		    if(TYPEOF(a) == REALSXP)
			updated = rprod(REAL(a), XLENGTH(a), &tmp, narm);
		    else
			updated = iprod(INTEGER(a), XLENGTH(a), &tmp, narm);
		    if(updated) {
			zcum.r *= tmp;
			zcum.i *= tmp;
		    }
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    updated = cprod(COMPLEX(a), XLENGTH(a), &ztmp, narm);
		    if(updated) {
			z.r = zcum.r;
			z.i = zcum.i;
			zcum.r = z.r * ztmp.r - z.i * ztmp.i;
			zcum.i = z.r * ztmp.i + z.i * ztmp.r;
		    }
		    break;
		default:
		    goto invalid_type;
		}

		break;/* prod() part */

	    } /* switch(iop) */

	} else { /* len(a)=0 */
	    /* Even though this has length zero it can still be invalid,
	       e.g. list() or raw() */
	    switch(TYPEOF(a)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case NILSXP:  /* OK historically, e.g. PR#1283 */
		break;
	    case CPLXSXP:
		if (iop == 2 || iop == 3) goto invalid_type;
		break;
	    case STRSXP:
		if (iop == 2 || iop == 3) {
		    if(!empty && ans_type == INTSXP) {
			scum = StringFromInteger(icum, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    } else if(!empty && ans_type == REALSXP) {
			scum = StringFromReal(zcum.r, &warn);
			UNPROTECT(1); /* scum */
			PROTECT(scum);
		    }
		    ans_type = STRSXP;
		    break;
		}
	    default:
		goto invalid_type;
	    }
	    if(ans_type < TYPEOF(a) && ans_type != CPLXSXP) {
		if(!empty && ans_type == INTSXP)
		    zcum.r = Int2Real(icum);
		ans_type = TYPEOF(a);
	    }
	}
	DbgP3(" .. upd.=%d, empty: old=%d", updated, empty);
	if(empty && updated) empty=0;
	DbgP2(", new=%d\n", empty);
	args = CDR(args);
    } /*-- while(..) loop over args */

    /*-------------------------------------------------------*/
    if(empty && (iop == 2 || iop == 3)) {
	if(ans_type == STRSXP) {
	    warningcall(call, _("no non-missing arguments, returning NA"));
	} else {
	    if(iop == 2)
		warningcall(call, _("no non-missing arguments to min; returning Inf"));
	    else
		warningcall(call, _("no non-missing arguments to max; returning -Inf"));
	    ans_type = REALSXP;
	}
    }

    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:   INTEGER(ans)[0] = icum;break;
    case REALSXP:  REAL(ans)[0] = zcum.r; break;
    case CPLXSXP:  COMPLEX(ans)[0].r = zcum.r; COMPLEX(ans)[0].i = zcum.i;break;
    case STRSXP:   SET_STRING_ELT(ans, 0, scum); break;
    }
    UNPROTECT(2); /* scum, args */
    return ans;

na_answer: /* only sum(INTSXP, ...) case currently used */
    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = NA_INTEGER; break;
    case REALSXP:	REAL(ans)[0] = NA_REAL; break;
    case CPLXSXP:	COMPLEX(ans)[0].r = COMPLEX(ans)[0].i = NA_REAL; break;
    case STRSXP:        SET_STRING_ELT(ans, 0, NA_STRING); break;
    }
    UNPROTECT(2); /* scum, args */
    return ans;

invalid_type:
    errorcall(call, R_MSG_type, type2char(TYPEOF(a)));
    return R_NilValue;
}/* do_summary */


SEXP attribute_hidden do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a, b, prargs, call2;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = shallow_duplicate(call));
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2);
	return(ans);
    }
    UNPROTECT(1);

    PROTECT(op = findFun(install("range.default"), env));
    PROTECT(prargs = promiseArgs(args, R_GlobalEnv));
    for (a = args, b = prargs; a != R_NilValue; a = CDR(a), b = CDR(b))
	SET_PRVALUE(CAR(b), CAR(a));
    ans = applyClosure(call, op, prargs, env, R_NilValue);
    UNPROTECT(3);
    return(ans);
}

/* which.min(x) : The index (starting at 1), of the first min(x) in x */
SEXP attribute_hidden do_first_min(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP sx, ans;
    double s, *r;
    int i, n, indx;

    checkArity(op, args);
    PROTECT(sx = coerceVector(CAR(args), REALSXP));
    if (!isNumeric(sx))
	error(_("non-numeric argument"));
    r = REAL(sx);
    n = LENGTH(sx);
    indx = NA_INTEGER;

    if(PRIMVAL(op) == 0) { /* which.min */
	s = R_PosInf;
	for (i = 0; i < n; i++)
	    if ( !ISNAN(r[i]) && (r[i] < s || indx == NA_INTEGER) ) {
		s = r[i]; indx = i;
	    }
    } else { /* which.max */
	s = R_NegInf;
	for (i = 0; i < n; i++)
	    if ( !ISNAN(r[i]) && (r[i] > s || indx == NA_INTEGER) ) {
		s = r[i]; indx = i;
	    }
    }

    i = (indx != NA_INTEGER);
    PROTECT(ans = allocVector(INTSXP, i ? 1 : 0));
    if (i) {
	INTEGER(ans)[0] = indx + 1;
	if (getAttrib(sx, R_NamesSymbol) != R_NilValue) { /* preserve names */
	    SEXP ansnam;
	    PROTECT(ansnam =
		    ScalarString(STRING_ELT(getAttrib(sx, R_NamesSymbol), indx)));
	    setAttrib(ans, R_NamesSymbol, ansnam);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(2);
    return ans;
}

/* which(x) : indices of non-NA TRUE values in x */
SEXP attribute_hidden do_which(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP v, v_nms, ans, ans_nms = R_NilValue;
    int i, j = 0, len, *buf;

    checkArity(op, args);
    v = CAR(args);
    if (!isLogical(v))
        error(_("argument to 'which' is not logical"));
    len = length(v);
    buf = (int *) R_alloc(len, sizeof(int));

    for (i = 0; i < len; i++) {
        if (LOGICAL(v)[i] == TRUE) {
            buf[j] = i + 1;
            j++;
        }
    }

    len = j;
    PROTECT(ans = allocVector(INTSXP, len));
    memcpy(INTEGER(ans), buf, sizeof(int) * len);

    if ((v_nms = getAttrib(v, R_NamesSymbol)) != R_NilValue) {
        PROTECT(ans_nms = allocVector(STRSXP, len));
        for (i = 0; i < len; i++) {
            SET_STRING_ELT(ans_nms, i,
                           STRING_ELT(v_nms, INTEGER(ans)[i] - 1));
        }
        setAttrib(ans, R_NamesSymbol, ans_nms);
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}


/* op = 0 is pmin, op = 1 is pmax
   NULL and logicals are handled as if they had been coerced to integer.
 */
SEXP attribute_hidden do_pmin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, x, ans;
    int narm;
    R_xlen_t i, n, len, i1;
    SEXPTYPE type, anstype;

    narm = asLogical(CAR(args));
    if(narm == NA_LOGICAL)
	error(_("invalid '%s' value"), "na.rm");
    args = CDR(args);
    x = CAR(args);
    if(args == R_NilValue) error(_("no arguments"));

    anstype = TYPEOF(x);
    switch(anstype) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
	break;
    default:
	error(_("invalid input type"));
    }
    a = CDR(args);
    if(a == R_NilValue) return x; /* one input */

    len = xlength(x); /* not LENGTH, as NULL is allowed */
    for(; a != R_NilValue; a = CDR(a)) {
	x = CAR(a);
	type = TYPEOF(x);
	switch(type) {
	case NILSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case STRSXP:
	    break;
	default:
	    error(_("invalid input type"));
	}
	if(type > anstype) anstype = type;
	n = xlength(x);
	if ((len > 0) ^ (n > 0)) {
	    // till 2.15.0:  error(_("cannot mix 0-length vectors with others"));
	    len = 0;
	    break;
	}
	len = imax2(len, n);
    }
    if(anstype < INTSXP) anstype = INTSXP;
    if(len == 0) return allocVector(anstype, 0);
    /* Check for fractional recycling (added in 2.14.0) */
    for(a = args; a != R_NilValue; a = CDR(a)) {
	n = length(CAR(a));
	if (len % n) {
	    warning(_("an argument will be fractionally recycled"));
	    break;
	}
    }

    PROTECT(ans = allocVector(anstype, len));
    switch(anstype) {
    case INTSXP:
    {
	int *r,  *ra = INTEGER(ans), tmp;
	PROTECT(x = coerceVector(CAR(args), anstype));
	r = INTEGER(x);
	n = XLENGTH(x);
	xcopyIntegerWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    x = CAR(a);
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    r = INTEGER(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(PRIMVAL(op) == 1) {
		    if( (narm && ra[i] == NA_INTEGER) ||
			(ra[i] != NA_INTEGER && tmp != NA_INTEGER
			 && tmp > ra[i]) ||
			(!narm && tmp == NA_INTEGER) )
			ra[i] = tmp;
		} else {
		    if( (narm && ra[i] == NA_INTEGER) ||
			(ra[i] != NA_INTEGER && tmp != NA_INTEGER
			 && tmp < ra[i]) ||
			(!narm && tmp == NA_INTEGER) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case REALSXP:
    {
	double *r, *ra = REAL(ans), tmp;
	PROTECT(x = coerceVector(CAR(args), anstype));
	r = REAL(x);
	n = XLENGTH(x);
	xcopyRealWithRecycle(ra, r, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    r = REAL(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = r[i1];
		if(PRIMVAL(op) == 1) {
		    if( (narm && ISNAN(ra[i])) ||
			(!ISNAN(ra[i]) && !ISNAN(tmp) && tmp > ra[i]) ||
			(!narm && ISNAN(tmp)) )
			ra[i] = tmp;
		} else {
		    if( (narm && ISNAN(ra[i])) ||
			(!ISNAN(ra[i]) && !ISNAN(tmp) && tmp < ra[i]) ||
			(!narm && ISNAN(tmp)) )
			ra[i] = tmp;
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    case STRSXP:
    {
	PROTECT(x = coerceVector(CAR(args), anstype));
	n = XLENGTH(x);
	xcopyStringWithRecycle(ans, x, 0, len, n);
	UNPROTECT(1);
	for(a = CDR(args); a != R_NilValue; a = CDR(a)) {
	    SEXP tmp, t2;
	    PROTECT(x = coerceVector(CAR(a), anstype));
	    n = XLENGTH(x);
	    MOD_ITERATE1(len, n, i, i1, {
		tmp = STRING_ELT(x, i1);
		t2 = STRING_ELT(ans, i);
		if(PRIMVAL(op) == 1) {
		    if( (narm && t2 == NA_STRING) ||
			(t2 != NA_STRING && tmp != NA_STRING && tmp != t2 && Scollate(tmp, t2) > 0) ||
			(!narm && tmp == NA_STRING) )
			SET_STRING_ELT(ans, i, tmp);
		} else {
		    if( (narm && t2 == NA_STRING) ||
			(t2 != NA_STRING && tmp != NA_STRING && tmp != t2 && Scollate(tmp, t2) < 0) ||
			(!narm && tmp == NA_STRING) )
			SET_STRING_ELT(ans, i, tmp);
		}
	    });
	    UNPROTECT(1);
	}
    }
	break;
    default:
	break;
    }
    UNPROTECT(1);
    return ans;
}
