/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
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
	errorcall(call, "invalid first argument\n");

    sep = CADR(args);
    if (!isString(sep) || LENGTH(sep) <= 0)
	errorcall(call, "invalid separator\n");
    sep = STRING(sep)[0];
    sepw = LENGTH(sep);

    collapse = CADDR(args);
    if (!isNull(collapse))
	if(!isString(collapse) || LENGTH(collapse) <= 0)
	    errorcall(call, "invalid collapse argument\n");

    /* Maximum argument length and */
    /* check for arguments of list type */

    nx = length(x);
    maxlen = 0;
    for (j = 0; j < nx; j++) {
	if (!isString(VECTOR(x)[j]))
	    error("non-string argument to Internal paste\n");
	if(length(VECTOR(x)[j]) > maxlen)
	    maxlen = length(VECTOR(x)[j]);
    }
    if(maxlen == 0)
	return mkString("");

    PROTECT(ans = allocVector(STRSXP, maxlen));

    for (i = 0; i < maxlen; i++) {
	pwidth = 0;
	for (j = 0; j < nx; j++) {
	    k = length(VECTOR(x)[j]);
	    if (k > 0)
		pwidth += LENGTH(STRING(VECTOR(x)[j])[i % k]);
	}
	pwidth += (nx - 1) * sepw;
	tmpchar = allocString(pwidth);
	buf = CHAR(tmpchar);
	for (j = 0; j < nx; j++) {
	    k = length(VECTOR(x)[j]);
	    if (k > 0) {
		s = CHAR(STRING(VECTOR(x)[j])[i % k]);
		sprintf(buf, "%s", s);
		buf += LENGTH(STRING(VECTOR(x)[j])[i % k]);
	    }
	    if (j != nx - 1 && sepw != 0) {
		sprintf(buf, "%s", CHAR(sep));
		buf += sepw;
	    }
	}
	STRING(ans)[i] = tmpchar;
    }

    /* Now collapse, if required. */

    if(collapse != R_NilValue && (nx=LENGTH(ans)) != 0) {
	sep = STRING(collapse)[0];
	sepw = LENGTH(sep);
	pwidth = 0;
	for (i = 0; i < nx; i++)
	    pwidth += LENGTH(STRING(ans)[i]);
	pwidth += (nx - 1) * sepw;
	tmpchar = allocString(pwidth);
	buf = CHAR(tmpchar);
	for (i = 0; i < nx; i++) {
	    if(i > 0) {
		sprintf(buf, "%s", CHAR(sep));
		buf += sepw;
	    }
	    s = CHAR(STRING(ans)[i]);
	    sprintf(buf, "%s", s);
	    while (*buf)
		buf++;
	}
	PROTECT(tmpchar);
	ans = allocVector(STRSXP, 1);
	UNPROTECT(1);
	STRING(ans)[0] = tmpchar;
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_format(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP l, x, y;
    int i, n, trim=0;
    int w, d, e;
    int wi, di, ei;
    char *strp;

    PrintDefaults(env);

    switch (length(args)) {
    case 1:
	trim = 0;
	break;
    case 2:
	trim = asLogical(CADR(args));
	if (trim == NA_INTEGER)
	    errorcall(call, "invalid \"trim\" argument\n");
	break;
    default:
	errorcall(call, "incorrect number of arguments\n");
    }

    if (!isVector(x = CAR(args)))
	errorcall(call, "first argument must be atomic\n");

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
	    STRING(y)[i] = mkChar(strp);
	}
	UNPROTECT(1);
	break;

    case INTSXP:
	PROTECT(y = allocVector(STRSXP, n));
	if (trim) w = 0;
	else formatInteger(INTEGER(x), n, &w);
	for (i = 0; i < n; i++) {
	    strp = EncodeInteger(INTEGER(x)[i], w);
	    STRING(y)[i] = mkChar(strp);
	}
	UNPROTECT(1);
	break;

    case REALSXP:
	formatReal(REAL(x), n, &w, &d, &e);
	if (trim) w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeReal(REAL(x)[i], w, d, e);
	    STRING(y)[i] = mkChar(strp);
	}
	UNPROTECT(1);
	break;

    case CPLXSXP:
	formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei);
	if (trim) wi = w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeComplex(COMPLEX(x)[i], w, d, e, wi, di, ei);
	    STRING(y)[i] = mkChar(strp);
	}
	UNPROTECT(1);
	break;

    case STRSXP:
	formatString(STRING(x), n, &w, 0);
	if (trim) w = 0;
	PROTECT(y = allocVector(STRSXP, n));
	for (i = 0; i < n; i++) {
	    strp = EncodeString(CHAR(STRING(x)[i]), w, 0, adj_left);
	    STRING(y)[i] = mkChar(strp);
	}
	UNPROTECT(1);
	break;
    default:
	errorcall(call, "Impossible mode ( x )\n"); y = R_NilValue;/* -Wall */
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
    int n, w, d, e;
    int wi, di, ei;
    checkArity(op, args);
    x = CAR(args);
    n = LENGTH(x);
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
	formatReal(REAL(x), n, &w, &d, &e);
	break;

    case CPLXSXP:
	wi = di = ei = 0;
	formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei);
	n = -1;/* complex 'code' */
	break;

    case STRSXP:
	formatString(STRING(x), n, &w, 0);
	break;

    default:
	errorcall(call, "vector arguments only\n");
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
