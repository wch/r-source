/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2003  The R Development Core Team
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
 *
 *
 *  See ./printutils.c	 for general remarks on Printing
 *                       and the Encode.. utils.
 *
 *  See ./format.c	 for the  format_Foo_  functions.
 */

/* <UTF8> char here is handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"

/*  .Internal(paste(args, sep, collapse))
 *
 * do_paste uses two passes to paste the arguments (in CAR(args)) together.
 * The first pass calculates the width of the paste buffer,
 * then it is alloc-ed and the second pass stuffs the information in.
 */
SEXP do_paste(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, collapse, sep, x, tmpchar;
    int i, j, k, maxlen, nx, pwidth, sepw;
    char *s, *buf;

    checkArity(op, args);

    /* We use formatting and so we */
    /* must initialize printing. */

    PrintDefaults(env);

    /* Check the arguments */

    x = CAR(args);
    if (!isVectorList(x))
	errorcall(call, _("invalid first argument"));

    sep = CADR(args);
    if (!isString(sep) || LENGTH(sep) <= 0)
	errorcall(call, _("invalid separator"));
    sep = STRING_ELT(sep, 0);
    sepw = strlen(CHAR(sep)); /* not LENGTH as might contain \0 */

    collapse = CADDR(args);
    if (!isNull(collapse))
	if(!isString(collapse) || LENGTH(collapse) <= 0)
	    errorcall(call, _("invalid 'collapse' argument"));

    /* Maximum argument length and */
    /* check for arguments of list type */

    nx = length(x);
    maxlen = 0;
    for (j = 0; j < nx; j++) {
	if (!isString(VECTOR_ELT(x, j)))
	    error(_("non-string argument to Internal paste"));
	if(length(VECTOR_ELT(x, j)) > maxlen)
	    maxlen = length(VECTOR_ELT(x, j));
    }
    if(maxlen == 0)
	return (!isNull(collapse)) ? mkString("") : allocVector(STRSXP, 0);

    PROTECT(ans = allocVector(STRSXP, maxlen));

    for (i = 0; i < maxlen; i++) {
	pwidth = 0;
	for (j = 0; j < nx; j++) {
	    k = length(VECTOR_ELT(x, j));
	    if (k > 0)
		pwidth += strlen(CHAR(STRING_ELT(VECTOR_ELT(x, j), i % k)));
	}
	pwidth += (nx - 1) * sepw;
	tmpchar = allocString(pwidth);
	buf = CHAR(tmpchar);
	for (j = 0; j < nx; j++) {
	    k = length(VECTOR_ELT(x, j));
	    if (k > 0) {
		s = CHAR(STRING_ELT(VECTOR_ELT(x, j), i % k));
                strcpy(buf, s);
		buf += strlen(s);
	    }
	    if (j != nx - 1 && sepw != 0) {
	        strcpy(buf, CHAR(sep));
		buf += sepw;
	    }
	}
	SET_STRING_ELT(ans, i, tmpchar);
    }

    /* Now collapse, if required. */

    if(collapse != R_NilValue && (nx=LENGTH(ans)) != 0) {
	sep = STRING_ELT(collapse, 0);
	sepw = strlen(CHAR(sep));
	pwidth = 0;
	for (i = 0; i < nx; i++)
	    pwidth += strlen(CHAR(STRING_ELT(ans, i)));
	pwidth += (nx - 1) * sepw;
	tmpchar = allocString(pwidth);
	buf = CHAR(tmpchar);
	for (i = 0; i < nx; i++) {
	    if(i > 0) {
	        strcpy(buf, CHAR(sep));
		buf += sepw;
	    }
	    s = CHAR(STRING_ELT(ans, i));
            strcpy(buf, s);
	    while (*buf)
		buf++;
	}
	PROTECT(tmpchar);
	ans = allocVector(STRSXP, 1);
	UNPROTECT(1);
	SET_STRING_ELT(ans, 0, tmpchar);
    }
    UNPROTECT(1);
    return ans;
}

/* format.default(x, trim, nsmall) : ../library/base/R/format.R
 * --------------   See "FIXME" in that file !
 */
SEXP do_format(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP l, x, y;
    int i, n, trim=0, nsmall=0;
    int w, d, e;
    int wi, di, ei;
    char *strp;

    PrintDefaults(env);

    switch (length(args)) {
    case 1:
	break;
    case 3:
	nsmall = asInteger(CADDR(args));
	if (nsmall == NA_INTEGER || nsmall < 0 || nsmall > 20)
	    errorcall(call, _("invalid 'nsmall' argument"));
	/* drop through */
    case 2:
	trim = asLogical(CADR(args));
	if (trim == NA_INTEGER)
	    errorcall(call, _("invalid 'trim' argument"));
	break;
    default:
	errorcall(call, _("incorrect number of arguments"));
    }

    if (!isVector(x = CAR(args)))
	errorcall(call, _("first argument must be atomic"));

    if ((n = LENGTH(x)) <= 0)
	return allocVector(STRSXP, 0);

    switch (TYPEOF(x)) {

    case LGLSXP:
	PROTECT(y = allocVector(STRSXP, n));
	if (trim)
	    w = 0;
	else
	    formatLogical(LOGICAL(x), n, &w);
	for (i = 0; i < n; i++) {
	    strp = EncodeLogical(LOGICAL(x)[i], w);
	    SET_STRING_ELT(y, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;

    case INTSXP:
	PROTECT(y = allocVector(STRSXP, n));
	if (trim) w = 0;
	else formatInteger(INTEGER(x), n, &w);
	for (i = 0; i < n; i++) {
	    strp = EncodeInteger(INTEGER(x)[i], w);
	    SET_STRING_ELT(y, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;

    case REALSXP:
	formatReal(REAL(x), n, &w, &d, &e, nsmall);
	if (trim) w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeReal(REAL(x)[i], w, d, e);
	    SET_STRING_ELT(y, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;

    case CPLXSXP:
	formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei, nsmall);
	if (trim) wi = w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeComplex(COMPLEX(x)[i], w, d, e, wi, di, ei);
	    SET_STRING_ELT(y, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;

    case STRSXP: /* this is *UN*used now (1.2) */

	formatString(STRING_PTR(x), n, &w, 0);
	if (trim) w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeString(STRING_ELT(x, i), w, 0, Rprt_adj_left);
	    SET_STRING_ELT(y, i, mkChar(strp));
	}
	UNPROTECT(1);
	break;
    default:
	errorcall(call, _("Impossible mode ( x )")); y = R_NilValue;/* -Wall */
    }
    PROTECT(y);
    if((l = getAttrib(x, R_DimSymbol)) != R_NilValue)
	setAttrib(y, R_DimSymbol, l);
    if((l = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	setAttrib(y, R_DimNamesSymbol, l);
    UNPROTECT(1);
    return y;
}

/* format.info(obj)  --> 3 integers  (w,d,e) with the formatting information
 *			w = total width (#{chars}) per item
 *			d = #{digits} to RIGHT of "."
 *			e = {0:2}.   0: Fixpoint;
 *				   1,2: exponential with 2/3 digit expon.
 *
 * for complex : 2 x 3 integers for (Re, Im)
 */

SEXP do_formatinfo(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x;
    int n, nsmall, w, d, e;
    int wi, di, ei;
    checkArity(op, args);
    x = CAR(args);
    n = LENGTH(x);
    nsmall = asInteger(CADR(args));
    w = 0;
    d = 0;
    e = 0;
    switch (TYPEOF(x)) {

    case LGLSXP:
	formatLogical(LOGICAL(x), n, &w);
	break;

    case INTSXP:
	formatInteger(INTEGER(x), n, &w);
	break;

    case REALSXP:
	formatReal(REAL(x), n, &w, &d, &e, nsmall);
	break;

    case CPLXSXP:
	wi = di = ei = 0;
	formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei, nsmall);
	n = -1;/* complex 'code' */
	break;

    case STRSXP:
	formatString(STRING_PTR(x), n, &w, 0);
	break;

    default:
	errorcall(call, _("vector arguments only"));
    }
    x = allocVector(INTSXP, (n >= 0) ? 3 : 6);
    INTEGER(x)[0] = w;
    INTEGER(x)[1] = d;
    INTEGER(x)[2] = e;
    if(n < 0) { /*- complex -*/
	INTEGER(x)[3] = wi;
	INTEGER(x)[4] = di;
	INTEGER(x)[5] = ei;
    }
    return x;
}
