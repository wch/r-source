/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2006   Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>

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


static Rboolean isum(int *x, int n, int *value, Rboolean narm)
{
    double s = 0.0;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    if(!updated) updated = TRUE;
	    s += x[i];
	} else if (!narm) {
	    if(!updated) updated = TRUE;
	    *value = NA_INTEGER;
	    return(updated);
	}
    }
    if(s > INT_MAX || s < R_INT_MIN){
	warning(_("Integer overflow in sum(.); use sum(as.numeric(.))"));
	*value = NA_INTEGER;
    }
    else *value = s;

    return(updated);
}

static Rboolean rsum(double *x, int n, double *value, Rboolean narm)
{
    LDOUBLE s = 0.0;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i]) || !narm) {
	    if(!updated) updated = TRUE;
	    s += x[i];
	}
    }
    *value = s;

    return(updated);
}

static Rboolean csum(Rcomplex *x, int n, Rcomplex *value, Rboolean narm)
{
    LDOUBLE sr = 0.0, si = 0.0;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if ((!ISNAN(x[i].r) && !ISNAN(x[i].i)) || !narm) {
	    if(!updated) updated = TRUE;
	    sr += x[i].r;
	    si += x[i].i;
	}
    }
    value->r = sr;
    value->i = si;

    return(updated);
}

static Rboolean imin(int *x, int n, int *value, Rboolean narm)
{
    int i, s = 0 /* -Wall */;
    Rboolean updated = FALSE;

    /* Used to set s = INT_MAX, but this ignored INT_MAX in the input */
    for (i = 0; i < n; i++) {
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

    return(updated);
}

static Rboolean rmin(double *x, int n, double *value, Rboolean narm)
{
    double s = 0.0 /* -Wall */;
    int i;
    Rboolean updated = FALSE;

    /* s = R_PosInf; */
    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(s != NA_REAL) s = x[i]; /* so any NA trumps all NaNs */
		if(!updated) updated = TRUE;
	    }
	}
	else if (!updated || x[i] < s) {  /* Never true if s is NA/NaN */
	    s = x[i];
	    if(!updated) updated = TRUE;
	}
    }
    *value = s;

    return(updated);
}

static Rboolean imax(int *x, int n, int *value, Rboolean narm)
{
    int i, s = 0 /* -Wall */;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
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

    return(updated);
}

static Rboolean rmax(double *x, int n, double *value, Rboolean narm)
{
    double s = 0.0 /* -Wall */;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if (ISNAN(x[i])) {/* Na(N) */
	    if (!narm) {
		if(s != NA_REAL) s = x[i]; /* so any NA trumps all NaNs */
		if(!updated) updated = TRUE;
	    }
	}
	else if (!updated || x[i] > s) {  /* Never true if s is NA/NaN */
	    s = x[i];
	    if(!updated) updated = TRUE;
	}
    }
    *value = s;

    return(updated);
}

static Rboolean iprod(int *x, int n, double *value, Rboolean narm)
{
    double s = 1.0;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if (x[i] != NA_INTEGER) {
	    s *= x[i];
	    if(!updated) updated = TRUE;
	}
	else if (!narm) {
	    if(!updated) updated = TRUE;
	    *value = NA_REAL;
	    return(updated);
	}

	if(ISNAN(s)) {  /* how can this happen? */
	    *value = NA_REAL;
	    return(updated);
	}
    }
    *value = s;

    return(updated);
}

static Rboolean rprod(double *x, int n, double *value, Rboolean narm)
{
    LDOUBLE s = 1.0;
    int i;
    Rboolean updated = FALSE;

    for (i = 0; i < n; i++) {
	if (!ISNAN(x[i]) || !narm) {
	    if(!updated) updated = TRUE;
	    s *= x[i];
	}
    }
    *value = s;

    return(updated);
}

static Rboolean cprod(Rcomplex *x, int n, Rcomplex *value, Rboolean narm)
{
    LDOUBLE sr, si, tr, ti;
    int i;
    Rboolean updated = FALSE;
    sr = 1;
    si = 0;
    for (i = 0; i < n; i++) {
	if ((!ISNAN(x[i].r) && !ISNAN(x[i].i)) || !narm) {
	    if(!updated) updated = TRUE;
	    tr = sr;
	    ti = si;
	    sr = tr * x[i].r - ti * x[i].i;
	    si = tr * x[i].i + ti * x[i].r;
	}
    }
    value->r = sr;
    value->i = si;

    return(updated);
}


/* do_summary provides a variety of data summaries
	op : 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod */
/* NOTE: mean() is rather different as only one arg and no na.rm. */

SEXP attribute_hidden do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a;
    double tmp = 0.0, s;
    Rcomplex z, ztmp, zcum={0.0, 0.0} /* -Wall */;
    int itmp = 0, icum=0, int_a, empty;
    short iop;
    SEXPTYPE ans_type;/* only INTEGER, REAL, or COMPLEX here */

    Rboolean narm;
    int updated;
	/* updated := 1 , as soon as (i)tmp (do_summary),
	   or *value ([ir]min / max) is assigned */

    if(PRIMVAL(op) == 1) { /* mean */
	LDOUBLE s = 0., si = 0., t = 0., ti = 0.;
	int i, n = LENGTH(CAR(args));
	SEXP x = CAR(args);
	switch(TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    PROTECT(ans = allocVector(REALSXP, 1));
	    for (i = 0; i < n; i++) s += INTEGER(x)[i];
	    REAL(ans)[0] = s/n;
	    break;
	case REALSXP:
	    PROTECT(ans = allocVector(REALSXP, 1));
	    for (i = 0; i < n; i++) s += REAL(x)[i];
	    s /= n;
	    if(R_FINITE((double)s)) {
		for (i = 0; i < n; i++) t += (REAL(x)[i] - s);
		s += t/n;
	    }
	    REAL(ans)[0] = s;
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
		    s += COMPLEX(x)[i].r;
		    si += COMPLEX(x)[i].i;
		}
		s += t/n; si += ti/n;
	    }
	    COMPLEX(ans)[0].r = s;
	    COMPLEX(ans)[0].i = si;
	    break;
	default:
	    errorcall_return(call, R_MSG_mode);
	}
	UNPROTECT(1);
	return ans;
    }


    if(DispatchGroup("Summary", call, op, args, env, &ans))
	return ans;

#ifdef DEBUG_Summary
    REprintf("C do_summary(op%s, *): did NOT dispatch\n", PRIMNAME(op));
#endif

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);
    updated = 0;
    empty = 1;/*- =1: only zero-length arguments, or NA with na.rm=T */

    iop = PRIMVAL(op);
    switch(iop) {
    case 0:/* sum */
    /* we need to find out if _all_ the arguments are integer or logical
       in advance, as we might overflow before we find out */
	a = args;
	int_a = 1;
	while (a != R_NilValue) {
	    if(!isInteger(CAR(a)) &&  !isLogical(CAR(a))) {
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
		    if (iop == 2) updated = imin(INTEGER(a), length(a), &itmp, narm);
		    else	  updated = imax(INTEGER(a), length(a), &itmp, narm);
		    break;
		case REALSXP:
		    if(ans_type == INTSXP) {/* change to REAL */
			ans_type = REALSXP;
			if(!empty) zcum.r = Int2Real(icum);
		    }
		    if (iop == 2) updated = rmin(REAL(a), length(a), &tmp, narm);
		    else	  updated = rmax(REAL(a), length(a), &tmp, narm);
		    break;
		default:
		    goto badmode;
		}

		if(updated) {/* 'a' had non-NA elements; --> "add" tmp or itmp*/
		    DbgP1(" updated:");
		    if(ans_type == INTSXP) {
			DbgP3(" INT: (old)icum= %ld, itmp=%ld\n", icum,itmp);
			if (itmp == NA_INTEGER) goto na_answer;
			if ((iop == 2 && itmp < icum) || /* min */
			    (iop == 3 && itmp > icum))   /* max */
			    icum = itmp;
		    } else { /* real */
			if (int_a) tmp = Int2Real(itmp);
			DbgP3(" REAL: (old)cum= %g, tmp=%g\n", zcum.r,tmp);
			if (ISNAN(tmp)) {
			    zcum.r += tmp;/* NA or NaN */
			} else if(
			    (iop == 2 && tmp < zcum.r) ||
			    (iop == 3 && tmp > zcum.r))	zcum.r = tmp;
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
		    updated = isum(INTEGER(a), length(a), &itmp, narm);
		    if(updated) {
			if(itmp == NA_INTEGER) goto na_answer;
			if(ans_type == INTSXP) {
			    s = (double) icum + (double) itmp;
			    if(s > INT_MAX || s < R_INT_MIN){
				warning(_("Integer overflow in sum(.); use sum(as.numeric(.))"));
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
		    updated = rsum(REAL(a), length(a), &tmp, narm);
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
		    updated = csum(COMPLEX(a), length(a), &ztmp, narm);
		    if(updated) {
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
			updated = rprod(REAL(a), length(a), &tmp, narm);
		    else
			updated = iprod(INTEGER(a), length(a), &tmp, narm);
		    if(updated) {
			zcum.r *= tmp;
			zcum.i *= tmp;
		    }
		    break;
		case CPLXSXP:
		    ans_type = CPLXSXP;
		    updated = cprod(COMPLEX(a), length(a), &ztmp, narm);
		    if(updated) {
			z.r = zcum.r;
			z.i = zcum.i;
			zcum.r = z.r * ztmp.r - z.i * ztmp.i;
			zcum.i = z.r * ztmp.i + z.i * ztmp.r;
		    }
		    break;
		default:
		    goto badmode;
		}

		break;/* prod() part */

	    }/* switch(iop) */

	} else { /*len(a)=0*/
	    if(TYPEOF(a) == CPLXSXP && (iop == 2 || iop == 3)) goto badmode;
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
	if(iop == 2)
	    warning(_("no non-missing arguments to min; returning Inf"));
	else
	    warning(_("no non-missing arguments to max; returning -Inf"));
	ans_type = REALSXP;
    }

    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = icum;break;
    case REALSXP:	REAL(ans)[0] = zcum.r; break;
    case CPLXSXP:
	COMPLEX(ans)[0].r = zcum.r;
	COMPLEX(ans)[0].i = zcum.i;
    }
    return ans;

na_answer: /* only INTSXP case curently used */
    ans = allocVector(ans_type, 1);
    switch(ans_type) {
    case INTSXP:	INTEGER(ans)[0] = NA_INTEGER; break;
    case REALSXP:	REAL(ans)[0] = NA_REAL; break;
    case CPLXSXP:	COMPLEX(ans)[0].r = COMPLEX(ans)[0].i = NA_REAL;
    }
    return ans;

badmode:
    errorcall_return(call, R_MSG_mode);
}/* do_summary */

SEXP attribute_hidden do_range(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, a, b, prargs;

    if (DispatchGroup("Summary", call, op, args, env, &ans))
	return(ans);
    PROTECT(op = findFun(install("range.default"), env));
    PROTECT(prargs = promiseArgs(args, R_GlobalEnv));
    for (a = args, b = prargs; a != R_NilValue; a = CDR(a), b = CDR(b))
	SET_PRVALUE(CAR(b), CAR(a));
    ans = applyClosure(call, op, prargs, env, R_BaseEnv);
    UNPROTECT(2);
    return(ans);
}

/* which.min(x) : The index (starting at 1), of the first min(x) in x */
SEXP attribute_hidden do_first_min(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#define Beg_do_first					\
    SEXP sx, ans;					\
    double s;						\
    int i, n, indx;					\
							\
    checkArity(op, args);				\
							\
    PROTECT(sx = coerceVector(CAR(args), REALSXP));	\
    if (!isNumeric(sx))					\
      errorcall(call, _("non-numeric argument"));	\
    n = LENGTH(sx);					\
    indx = NA_INTEGER;

    Beg_do_first

    s = R_PosInf;
    for (i = 0; i < n; i++)
	if (!ISNAN(REAL(sx)[i]) && REAL(sx)[i] < s) {
	    s = REAL(sx)[i]; indx = i;
	}

#define End_do_first							\
    i = (indx != NA_INTEGER);						\
    PROTECT(ans = allocVector(INTSXP, i ? 1 : 0));			\
    if (i) {								\
	INTEGER(ans)[0] = indx + 1;					\
	if (getAttrib(sx, R_NamesSymbol) != R_NilValue) { /* keep name */\
	    SEXP ansnam;						\
	    PROTECT(ansnam = allocVector(STRSXP, 1));			\
	    SET_STRING_ELT(ansnam, 0,					\
			   STRING_ELT(getAttrib(sx, R_NamesSymbol), indx));\
	    setAttrib(ans, R_NamesSymbol, ansnam);			\
	    UNPROTECT(1);						\
	}								\
    }									\
    UNPROTECT(2);							\
    return ans;

    End_do_first
}

/* which.max(x) : The index (starting at 1), of the first max(x) in x */
SEXP attribute_hidden do_first_max(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Beg_do_first

    s = R_NegInf;
    for (i = 0; i < n; i++)
	if (!ISNAN(REAL(sx)[i]) && REAL(sx)[i] > s) {
	    s = REAL(sx)[i]; indx = i;
	}

    End_do_first
}
#undef Beg_do_first
#undef End_do_first


/* complete.cases(.) */
SEXP attribute_hidden do_compcases(SEXP call, SEXP op, SEXP args, SEXP rho)
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
		if (isMatrix(VECTOR_ELT(t, it))) {
		    u = getAttrib(VECTOR_ELT(t, it), R_DimSymbol);
		    if (len < 0)
			len = INTEGER(u)[0];
		    else if (len != INTEGER(u)[0])
			goto bad;
		}
		else if (isVector(VECTOR_ELT(t, it))) {
		    if (len < 0)
			len = LENGTH(VECTOR_ELT(t, it));
		    else if (len != LENGTH(VECTOR_ELT(t, it)))
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
			if (STRING_ELT(u, i) == NA_STRING)
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
		u = VECTOR_ELT(t, it);
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
			if (STRING_ELT(u, i) == NA_STRING)
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
		    if (STRING_ELT(u, i) == NA_STRING)
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
    errorcall(call, _("not all arguments have the same length"));

 bad_mode:
    errorcall_return(call, R_MSG_mode);
}
