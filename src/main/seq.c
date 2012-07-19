/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2012  The R Core Team.
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

/* The x:y  primitive calls do_colon(); do_colon() calls cross_colon() if
   both arguments are factors and seq_colon() otherwise.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <float.h>  /* for DBL_EPSILON */
#include <Rmath.h>

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#define _S4_rep_keepClass
/* ==>  rep(<S4>, .) keeps class e.g., for list-like */

static SEXP cross_colon(SEXP call, SEXP s, SEXP t)
{
    SEXP a, la, ls, lt, rs, rt;
    int i, j, k, n, nls, nlt;
    char *cbuf;

    if (length(s) != length(t))
	errorcall(call, _("unequal factor lengths"));
    n = length(s);
    ls = getAttrib(s, R_LevelsSymbol);
    lt = getAttrib(t, R_LevelsSymbol);
    nls = LENGTH(ls);
    nlt = LENGTH(lt);
    PROTECT(a = allocVector(INTSXP, n));
    PROTECT(rs = coerceVector(s, INTSXP));
    PROTECT(rt = coerceVector(t, INTSXP));
    for (i = 0; i < n; i++) {
	int vs = INTEGER(rs)[i];
	int vt = INTEGER(rt)[i];
	if ((vs == NA_INTEGER) || (vt == NA_INTEGER))
	    INTEGER(a)[i] = NA_INTEGER;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    UNPROTECT(2);
    if (!isNull(ls) && !isNull(lt)) {
	PROTECT(la = allocVector(STRSXP, nls * nlt));
	k = 0;
	/* FIXME: possibly UTF-8 version */
	for (i = 0; i < nls; i++) {
	    const char *vi = translateChar(STRING_ELT(ls, i));
	    size_t vs = strlen(vi);
	    for (j = 0; j < nlt; j++) {
		const char *vj = translateChar(STRING_ELT(lt, j));
		size_t vt = strlen(vj);
		cbuf = R_AllocStringBuffer(vs + vt + 1, &cbuff);
		sprintf(cbuf, "%s:%s", vi, vj);
		SET_STRING_ELT(la, k, mkChar(cbuf));
		k++;
	    }
	}
	setAttrib(a, R_LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = mkString("factor"));
    setAttrib(a, R_ClassSymbol, la);
    UNPROTECT(2);
    R_FreeStringBufferL(&cbuff);
    return(a);
}

static SEXP seq_colon(double n1, double n2, SEXP call)
{
    int in1;
    R_xlen_t n;
    double r;
    SEXP ans;
    Rboolean useInt;

    r = fabs(n2 - n1);
    if(r >= R_XLEN_T_MAX) 
	errorcall(call, _("result would be too long a vector"));

    n = (R_xlen_t)(r + 1 + FLT_EPSILON);

    in1 = (int)(n1);
    useInt = (n1 == in1);
    if(useInt) {
	if(n1 <= INT_MIN || n1 > INT_MAX)
	    useInt = FALSE;
	else {
	    /* r := " the effective 'to' "  of  from:to */
	    double dn = (double) n;
	    r = n1 + ((n1 <= n2) ? dn-1 : -(dn-1));
	    if(r <= INT_MIN || r > INT_MAX) useInt = FALSE;
	}
    }
    if (useInt) {
	ans = allocVector(INTSXP, n);
	if (n1 <= n2)
	    for (int i = 0; i < n; i++) INTEGER(ans)[i] = in1 + i;
	else
	    for (int i = 0; i < n; i++) INTEGER(ans)[i] = in1 - i;
    } else {
	ans = allocVector(REALSXP, n);
	if (n1 <= n2)
	    for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = n1 + (double)i;
	else
	    for (R_xlen_t i = 0; i < n; i++) REAL(ans)[i] = n1 - (double)i;
    }
    return ans;
}

SEXP attribute_hidden do_colon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s1, s2;
    double n1, n2;

    checkArity(op, args);
    if (inherits(CAR(args), "factor") && inherits(CADR(args), "factor"))
	return(cross_colon(call, CAR(args), CADR(args)));

    s1 = CAR(args);
    s2 = CADR(args);
    n1 = length(s1);
    n2 = length(s2);
    if (n1 > 1)
	warningcall(call, _("numerical expression has %d elements: only the first used"), (int) n1);
    else if (n1 == 0 || n2 == 0)
	errorcall(call, _("argument of length 0"));
    if (n2 > 1)
	warningcall(call, _("numerical expression has %d elements: only the first used"), (int) n2);
    n1 = asReal(s1);
    n2 = asReal(s2);
    if (ISNAN(n1) || ISNAN(n2))
	errorcall(call, _("NA/NaN argument"));
    return seq_colon(n1, n2, call);
}

static SEXP rep2(SEXP s, SEXP ncopy)
{
    R_xlen_t i, na, nc, n;
    int j;
    SEXP a, t, u;

    PROTECT(t = coerceVector(ncopy, INTSXP));

    nc = xlength(ncopy);
    na = 0;
    for (i = 0; i < nc; i++) {
	if (INTEGER(t)[i] == NA_INTEGER || INTEGER(t)[i] < 0)
	    error(_("invalid '%s' value"), "times");
	na += INTEGER(t)[i];
    }

    if (isVector(s))
	a = allocVector(TYPEOF(s), na);
    else {
	if (na > INT_MAX) error("too long for a pairlist");
	a = allocList((int) na);
    }
    PROTECT(a);
    n = 0;
    switch (TYPEOF(s)) {
    case LGLSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		LOGICAL(a)[n++] = LOGICAL(s)[i];
	break;
    case INTSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		INTEGER(a)[n++] = INTEGER(s)[i];
	break;
    case REALSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		REAL(a)[n++] = REAL(s)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		COMPLEX(a)[n++] = COMPLEX(s)[i];
	break;
    case STRSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		SET_STRING_ELT(a, n++, STRING_ELT(s, i));
	break;
    case VECSXP:
    case EXPRSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		SET_VECTOR_ELT(a, n++, VECTOR_ELT(s, i));
	break;
    case LISTSXP:
	u = a;
	for (int ii = 0; ii < (int)nc; ii++)
	    for (j = 0; j < (INTEGER(t)[ii]); j++) {
		SETCAR(u, duplicate(CAR(nthcdr(s, ii))));
		u = CDR(u);
	    }
	break;
    case RAWSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		RAW(a)[n++] = RAW(s)[i];
	break;
    default:
	UNIMPLEMENTED_TYPE("rep2", s);
    }
    UNPROTECT(2);
    return a;
}

static SEXP rep3(SEXP s, R_xlen_t na)
{
    R_xlen_t i, ns = xlength(s);
    SEXP a;

    if (isVector(s))
	a = allocVector(TYPEOF(s), na);
    else {
	if (na > INT_MAX) error("too long for a pairlist");
	a = allocList((int) na);
    }

    /* We need to handle ns = 0 and do that separately for speed. */
    if (ns > 0) {
	switch (TYPEOF(s)) {
	case LGLSXP:
	    for (i = 0; i < na; i++) LOGICAL(a)[i] = LOGICAL(s)[i % ns];
	    break;
	case INTSXP:
	    for (i = 0; i < na; i++) INTEGER(a)[i] = INTEGER(s)[i % ns];
	    break;
	case REALSXP:
	    for (i = 0; i < na; i++) REAL(a)[i] = REAL(s)[i % ns];
	    break;
	case CPLXSXP:
	    for (i = 0; i < na; i++) COMPLEX(a)[i] = COMPLEX(s)[i % ns];
	    break;
	case RAWSXP:
	    for (i = 0; i < na; i++) RAW(a)[i] = RAW(s)[i % ns];
	    break;
	case STRSXP:
	    for (i = 0; i < na; i++) SET_STRING_ELT(a, i, STRING_ELT(s, i% ns));
	    break;
	case LISTSXP:
	{
	    int i = 0, nss = (int) ns;
	    for (SEXP t = a; t != R_NilValue; t = CDR(t), i++)
		SETCAR(t, duplicate(CAR(nthcdr(s, (i % nss)))));
	}
	break;
	case VECSXP:
	    for (i = 0; i < na; i++)
		SET_VECTOR_ELT(a, i, duplicate(VECTOR_ELT(s, i % ns)));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("rep3", s);
	}
    } else {
	switch (TYPEOF(s)) {
	case LGLSXP:
	    for (i = 0; i < na; i++) LOGICAL(a)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    for (i = 0; i < na; i++) INTEGER(a)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    for (i = 0; i < na; i++) REAL(a)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    for (i = 0; i < na; i++) {
		COMPLEX(a)[i].r = NA_REAL;
		COMPLEX(a)[i].i = NA_REAL;
	    }
	    break;
	case RAWSXP:
	    for (i = 0; i < na; i++) RAW(a)[i] = (Rbyte) 0;
	    break;
	case STRSXP:
	    for (i = 0; i < na; i++) SET_STRING_ELT(a, i, NA_STRING);
	    break;
	case LISTSXP:
	{
	    int i = 0;
	    for (SEXP t = a; t != R_NilValue; t = CDR(t), i++)
		SETCAR(t, R_NilValue);
	}
	break;
	case VECSXP:
	    for (i = 0; i < na; i++) SET_VECTOR_ELT(a, i, R_NilValue);
	    break;
	default:
	    UNIMPLEMENTED_TYPE("rep3", s);
	}
    }
    return a;
}

SEXP attribute_hidden do_rep_int(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP s = CAR(args), ncopy = CADR(args);
    R_xlen_t nc;
    SEXP a;

    if (!isVector(ncopy))
	error(_("incorrect type for second argument"));

    if (!isVector(s) && (!isList(s)))
	error(_("attempt to replicate non-vector"));

    nc = xlength(ncopy); // might be 0
    if (nc == xlength(s)) PROTECT(a = rep2(s, ncopy));
    else {
	if (nc != 1) error(_("invalid '%s' value"), "times");
	
#ifdef LONG_VECTOR_SUPPORT
	double snc = asReal(ncopy);
	if (!R_FINITE(snc) || snc < 0)
	    error(_("invalid '%s' value"), "times");
	nc = (R_xlen_t) snc;
#else
	if ((nc = asInteger(ncopy)) == NA_INTEGER || nc < 0)/* nc = 0 ok */
	    error(_("invalid '%s' value"), "times");
#endif
	PROTECT(a = rep3(s, nc * xlength(s)));
    }

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

SEXP attribute_hidden do_rep_len(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_xlen_t na;
    SEXP a, s, len;

    checkArity(op, args);
    s = CAR(args);

    if (!isVector(s) && (!isList(s)))
	error(_("attempt to replicate non-vector"));

    len = CADR(args);
    if(length(len) != 1)
	error(_("invalid '%s' value"), "length.out");
#ifdef LONG_VECTOR_SUPPORT
    double sna = asReal(len);
    if (!R_FINITE(sna) || sna < 0)
	error(_("invalid '%s' value"), "length.out");
    na = (R_xlen_t) sna;
#else
    if ((na = asInteger(len)) == NA_INTEGER || na < 0) /* na = 0 ok */
	error(_("invalid '%s' value"), "length.out");
#endif

    PROTECT(a = rep3(s, na));

#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(s)) { /* e.g. contains = "list" */
	setAttrib(a, R_ClassSymbol, getAttrib(s, R_ClassSymbol));
	SET_S4_OBJECT(a);
    }
#endif

    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    SET_STRING_ELT(tmp, 0, mkChar("ordered"));
	    SET_STRING_ELT(tmp, 1, mkChar("factor"));
	} else PROTECT(tmp = mkString("factor"));
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}


/* We are careful to use evalListKeepMissing here (inside
   DispatchOrEval) to avoid dropping missing arguments so e.g.
   rep(1:3,,8) matches length.out */

/* This is a primitive SPECIALSXP with internal argument matching */
SEXP attribute_hidden do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, ap, times = R_NilValue /* -Wall */, ind;
    int each = 1, nprotect = 4;
    R_xlen_t i, lx, len = NA_INTEGER, nt;

    if (DispatchOrEval(call, op, "rep", args, rho, &ans, 0, 0))
	return(ans);

    /* This has evaluated all the non-missing arguments into ans */
    PROTECT(args = ans);

    /* This is a primitive, and we have not dispatched to a method
       so we manage the argument matching ourselves.  We pretend this is
       rep(x, times, length.out, each, ...)
    */
    PROTECT(ap = CONS(R_NilValue,
		      list4(R_NilValue, R_NilValue, R_NilValue, R_NilValue)));
    SET_TAG(ap, install("x"));
    SET_TAG(CDR(ap), install("times"));
    SET_TAG(CDDR(ap), install("length.out"));
    SET_TAG(CDR(CDDR(ap)), install("each"));
    SET_TAG(CDDR(CDDR(ap)), R_DotsSymbol);
    PROTECT(args = matchArgs(ap, args, call));

    x = CAR(args);
    lx = xlength(x);

    double slen = asReal(CADDR(args));
    if (R_FINITE(slen)) {
	if(slen < 0)
	    errorcall(call, _("invalid '%s' argument"), "length.out");
	len = (R_xlen_t) slen;
    } else {
	len = asInteger(CADDR(args));
	if(len != NA_INTEGER && len < 0)
	    errorcall(call, _("invalid '%s' argument"), "length.out");
    }
    if(length(CADDR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"), 
		    "length.out");

    each = asInteger(CADDDR(args));
    if(each != NA_INTEGER && each < 0)
	errorcall(call, _("invalid '%s' argument"), "each");
    if(length(CADDDR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"), "each");
    if(each == NA_INTEGER) each = 1;

    if(lx == 0) {
	if(len > 0 && x == R_NilValue) 
	    warningcall(call, "'x' is NULL so the result will be NULL");
	SEXP a;
	PROTECT(a = duplicate(x));
	if(len != NA_INTEGER && len > 0) a = xlengthgets(a, len);
	UNPROTECT(4);
	return a;
    }

    if(len != NA_INTEGER) { /* takes precedence over times */
	nt = 1;
    } else {
	R_xlen_t sum = 0;
	if(CADR(args) == R_MissingArg) PROTECT(times = ScalarInteger(1));
	else PROTECT(times = coerceVector(CADR(args), INTSXP));
	nprotect++;
	nt = XLENGTH(times);
	if(nt != 1 && nt != lx * each)
	    errorcall(call, _("invalid '%s' argument"), "times");
	if(nt == 1) {
	    int it = INTEGER(times)[0];
	    if (it == NA_INTEGER || it < 0)
		errorcall(call, _("invalid '%s' argument"), "times");
	    len = lx * it * each;
	} else {
	    for(i = 0; i < nt; i++) {
		int it = INTEGER(times)[i];
		if (it == NA_INTEGER || it < 0)
		    errorcall(call, _("invalid '%s' argument"), "times");
		sum += it;
	    }
            len = sum;
	}
    }
    if(len > 0 && each == 0)
	errorcall(call, _("invalid '%s' argument"), "each");
 #ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	PROTECT(ind = allocVector(REALSXP, len));
	if(nt == 1)
	    for(i = 0; i < len; i++)
		REAL(ind)[i] = (double)( 1 + ((i/each) % lx));
	else {
	    R_xlen_t j, k, k2, k3;
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) {
		R_xlen_t sum = 0;
		for(j = 0; j < each; j++) sum += INTEGER(times)[k++];
		for(k3 = 0; k3 < sum; k3++) {
		    REAL(ind)[k2++] = (double)(i+1);
		    if(k2 == len) goto done;
		}
	    }
	}
    } else
#endif
    {
	PROTECT(ind = allocVector(INTSXP, len));
	if(nt == 1)
	    for(i = 0; i < len; i++)
		INTEGER(ind)[i] = (int)( 1 + ((i/each) % lx));
	else {
	    R_xlen_t j, k, k2, k3;
	    for(i = 0, k = 0, k2 = 0; i < lx; i++) {
		R_xlen_t sum = 0;
		for(j = 0; j < each; j++) sum += INTEGER(times)[k++];
		for(k3 = 0; k3 < sum; k3++) {
		    INTEGER(ind)[k2++] = (int)(i+1);
		    if(k2 == len) goto done;
		}
	    }
	}
    }

done:
    ans = do_subset_dflt(R_NilValue, R_NilValue, list2(x, ind), rho);
#ifdef _S4_rep_keepClass
    if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	setAttrib(ans, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	SET_S4_OBJECT(ans);
    }
#endif
    /* 1D arrays get dimensions preserved by subsetting */
    setAttrib(ans, R_DimSymbol, R_NilValue);
    UNPROTECT(nprotect);
    return ans;
}


/*
  This is a primitive SPECIALSXP with internal argument matching,
  implementing seq.int.

   'along' has to be used on an unevaluated argument, and evalList
   tries to evaluate language objects.
 */

#define FEPS 1e-10
#define myabs(x) (x < 0 ? x : -x)
/* to match seq.default */
SEXP attribute_hidden do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue /* -Wall */, ap, tmp, from, to, by, len, along;
    int nargs = length(args), lf;
    Rboolean One = nargs == 1;
    R_xlen_t i, lout = NA_INTEGER;

    if (DispatchOrEval(call, op, "seq", args, rho, &ans, 0, 1))
	return(ans);

    /* This is a primitive and we manage argument matching ourselves.
       We pretend this is
       seq(from, to, by, length.out, along.with, ...)
    */
    PROTECT(ap = CONS(R_NilValue,
		      CONS(R_NilValue,
			   list4(R_NilValue, R_NilValue, R_NilValue,
				 R_NilValue))));
    tmp = ap;
    SET_TAG(tmp, install("from")); tmp = CDR(tmp);
    SET_TAG(tmp, install("to")); tmp = CDR(tmp);
    SET_TAG(tmp, install("by")); tmp = CDR(tmp);
    SET_TAG(tmp, install("length.out")); tmp = CDR(tmp);
    SET_TAG(tmp, install("along.with")); tmp = CDR(tmp);
    SET_TAG(tmp, R_DotsSymbol);
    PROTECT(args = matchArgs(ap, args, call));

    from = CAR(args); args = CDR(args);
    to = CAR(args); args = CDR(args);
    by = CAR(args); args = CDR(args);
    len = CAR(args); args = CDR(args);
    along = CAR(args);

    if(One && from != R_MissingArg) {
	lf = length(from);
	if(lf == 1 && (TYPEOF(from) == INTSXP || TYPEOF(from) == REALSXP))
	    ans = seq_colon(1.0, asReal(from), call);
	else if (lf)
	    ans = seq_colon(1.0, (double)lf, call);
	else
	    ans = allocVector(INTSXP, 0);
	goto done;
    }
    if(along != R_MissingArg) {
	lout = XLENGTH(along);
	if(One) {
	    ans = lout ? seq_colon(1.0, (double)lout, call) : allocVector(INTSXP, 0);
	    goto done;
	}
    } else if(len != R_MissingArg && len != R_NilValue) {
	double rout = asReal(len);
	if(ISNAN(rout) || rout <= -0.5)
	    errorcall(call, _("'length.out' must be a non-negative number"));
	if(length(len) != 1)
	    warningcall(call, _("first element used of '%s' argument"), 
			"length.out");
	lout = (R_xlen_t) ceil(rout);
    }

    if(lout == NA_INTEGER) {
	double rfrom = asReal(from), rto = asReal(to), rby = asReal(by), *ra;
	if(from == R_MissingArg) rfrom = 1.0;
	else if(length(from) != 1) error("'from' must be of length 1");
	if(to == R_MissingArg) rto = 1.0;
	else if(length(to) != 1) error("'to' must be of length 1");
	if(by == R_MissingArg)
	    ans = seq_colon(rfrom, rto, call);
	else {
	    if(length(by) != 1) error("'by' must be of length 1");
	    double del = rto - rfrom, n, dd;
	    R_xlen_t nn;
	    if(!R_FINITE(rfrom))
		errorcall(call, _("'from' must be finite"));
	    if(!R_FINITE(rto))
		errorcall(call, _("'to' must be finite"));
	    if(del == 0.0 && rto == 0.0) {
		ans = to;
		goto done;
	    }
	    /* printf("from = %f, to = %f, by = %f\n", rfrom, rto, rby); */
	    n = del/rby;
	    if(!R_FINITE(n)) {
		if(del == 0.0 && rby == 0.0) {
		    ans = from;
		    goto done;
		} else
		    errorcall(call, _("invalid '(to - from)/by' in 'seq'"));
	    }
	    dd = fabs(del)/fmax2(fabs(rto), fabs(rfrom));
	    if(dd < 100 * DBL_EPSILON) {
		ans = from;
		goto done;
	    }
#ifdef LONG_VECTOR_SUPPORT
	    if(n > 100 * (double) INT_MAX)
#else
	    if(n > (double) INT_MAX)
#endif
		errorcall(call, _("'by' argument is much too small"));
	    if(n < - FEPS)
		errorcall(call, _("wrong sign in 'by' argument"));
	    if(TYPEOF(from) == INTSXP &&
	       TYPEOF(to) == INTSXP &&
	       TYPEOF(by) == INTSXP) {
		int *ia, ifrom = asInteger(from), iby = asInteger(by);
		/* With the current limits on integers and FEPS
		   reduced below 1/INT_MAX this is the same as the
		   next, so this is future-proofing against longer integers.
		*/
		/* seq.default gives integer result from
		   from + (0:n)*by
		*/
		nn = (R_xlen_t) n;
		ans = allocVector(INTSXP, nn+1);
		ia = INTEGER(ans);
		for(i = 0; i <= nn; i++)
		    ia[i] = (int)(ifrom + i * iby);
	    } else {
		nn = (int)(n + FEPS);
		ans = allocVector(REALSXP, nn+1);
		ra = REAL(ans);
		for(i = 0; i <= nn; i++)
		    ra[i] = rfrom + (double)i * rby;
		/* Added in 2.9.0 */
		if (nn > 0)
		    if((rby > 0 && ra[nn] > rto) || (rby < 0 && ra[nn] < rto))
			ra[nn] = rto;
	    }
	}
    } else if (lout == 0) {
	ans = allocVector(INTSXP, 0);
    } else if (One) {
	ans = seq_colon(1.0, (double)lout, call);
    } else if (by == R_MissingArg) {
	double rfrom = asReal(from), rto = asReal(to), rby;
	if(to == R_MissingArg) rto = rfrom + (double)lout - 1;
	if(from == R_MissingArg) rfrom = rto - (double)lout + 1;
	if(!R_FINITE(rfrom))
	    errorcall(call, _("'from' must be finite"));
	if(!R_FINITE(rto))
	    errorcall(call, _("'to' must be finite"));
	ans = allocVector(REALSXP, lout);
	if(lout > 0) REAL(ans)[0] = rfrom;
	if(lout > 1) REAL(ans)[lout - 1] = rto;
	if(lout > 2) {
	    rby = (rto - rfrom)/(double)(lout - 1);
	    for(i = 1; i < lout-1; i++) REAL(ans)[i] = rfrom + (double)i*rby;
	}
    } else if (to == R_MissingArg) {
	double rfrom = asReal(from), rby = asReal(by), rto;
	if(from == R_MissingArg) rfrom = 1.0;
	if(!R_FINITE(rfrom))
	    errorcall(call, _("'from' must be finite"));
	if(!R_FINITE(rby))
	    errorcall(call, _("'by' must be finite"));
	rto = rfrom + (double)(lout-1)*rby;
	if(rby == (int)rby && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++)
		INTEGER(ans)[i] = (int)(rfrom + (double)i*rby);
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++)
		REAL(ans)[i] = rfrom + (double)i*rby;
	}
    } else if (from == R_MissingArg) {
	double rto = asReal(to), rby = asReal(by),
	    rfrom = rto - (double)(lout-1)*rby;
	if(!R_FINITE(rto))
	    errorcall(call, _("'to' must be finite"));
	if(!R_FINITE(rby))
	    errorcall(call, _("'by' must be finite"));
	if(rby == (int)rby && rfrom <= INT_MAX && rfrom >= INT_MIN
	   && rto <= INT_MAX && rto >= INT_MIN) {
	    ans = allocVector(INTSXP, lout);
	    for(i = 0; i < lout; i++)
		INTEGER(ans)[i] = (int)(rto - (double)(lout - 1 - i)*rby);
	} else {
	    ans = allocVector(REALSXP, lout);
	    for(i = 0; i < lout; i++)
		REAL(ans)[i] = rto - (double)(lout - 1 - i)*rby;
	}
    } else
	errorcall(call, _("too many arguments"));

done:
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_seq_along(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    R_xlen_t len;
    static SEXP length_op = NULL;

    /* Store the .Primitive for 'length' for DispatchOrEval to use. */
    if (length_op == NULL) {
	SEXP R_lengthSymbol = install("length");
	length_op = eval(R_lengthSymbol, R_BaseEnv);
	if (TYPEOF(length_op) != BUILTINSXP) {
	    length_op = NULL;
	    error("'length' is not a BUILTIN");
	}
	R_PreserveObject(length_op);
    }

    checkArity(op, args);
    check1arg(args, call, "along.with");

    /* Try to dispatch to S3 or S4 methods for 'length'.  For cases
       where no methods are defined this is more efficient than an
       unconditional callback to R */
    if (isObject(CAR(args)) &&
	DispatchOrEval(call, length_op, "length", args, rho, &ans, 0, 1)) {
	len = asInteger(ans);
    }
    else
	len = xlength(CAR(args));

#ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	ans = allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) p[i] = (double) (i+1);
    } else
#endif
    {
	ans = allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) p[i] = i+1;
    }
    return ans;
}

SEXP attribute_hidden do_seq_len(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity(op, args);
    check1arg(args, call, "length.out");
    double dlen = asReal(CAR(args));
    if(!R_FINITE(dlen) || dlen < 0)
	errorcall(call, _("argument must be coercible to non-negative integer"));
    if(length(CAR(args)) != 1)
	warningcall(call, _("first element used of '%s' argument"),
		    "length.out");
    R_xlen_t len = (R_xlen_t) dlen;

 #ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) {
	ans = allocVector(REALSXP, len);
	double *p = REAL(ans);
	for(R_xlen_t i = 0; i < len; i++) p[i] = (double) (i+1);
    } else
#endif
    {
	ans = allocVector(INTSXP, len);
	int *p = INTEGER(ans);
	for(int i = 0; i < len; i++) p[i] = i+1;
    }
    return ans;
}
