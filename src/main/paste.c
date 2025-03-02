/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2025  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *
 *  See ./printutils.c	 for general remarks on Printing
 *                       and the Encode.. utils.
 *
 *  See ./format.c	 for the  format_Foo_  functions.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

#define imax2(x, y) ((x < y) ? y : x)

#include "Print.h"
#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#ifndef HAVE_STPCPY
static char *R_stpcpy(char *dest, const char *src)
{
    while ((*dest++ = *src++) != '\0');
    return dest - 1;
}
#else
# define R_stpcpy stpcpy
#endif

/*
  .Internal(paste (args, sep, collapse, recycle0))
  .Internal(paste0(args,      collapse, recycle0))
*/
attribute_hidden SEXP do_paste(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* do_paste uses two passes to paste the arguments (in CAR(args)) together.
 * The first pass calculates the width of the paste buffer,
 * then it is alloc-ed and the second pass stuffs the information in.

 * Note that NA_STRING is not handled separately here.  This is
 *  deliberate -- see ?paste -- and implicitly coerces it to "NA"
 */

    SEXP collapse, sep;
    bool recycle_0;

/* We need to be careful here.  For example currently Windows 4.1
 * packages are links to 4.0, 4.0.0 uses only 3 args for paste and
 * 4.0.x (x >= 1) use 3 unless recycle0 is true.  So 4.1 needs to
 * accept 3-arg form, silently.
 */

#ifdef future_R_4_1_or_newer
    checkArity(op, args);
#else
    int nargs = length(args);
    bool correct_nargs = (PRIMARITY(op) == nargs);
    if(!correct_nargs) { // we allow one less for capture from earlier versions
	if(PRIMARITY(op) == nargs + 1) {
	    recycle_0 = false;
#if 0
	    REprintf("%d arguments passed to .Internal(%s) which requires %d;\n an S4 method"
		     " may need to be redefined, typically by re-installing a package\n",
		     nargs, PRIMNAME(op), PRIMARITY(op));
#endif
	}
	else // not even "ok":
	    error(ngettext("%d argument passed to .Internal(%s) which requires %d",
			   "%d arguments passed to .Internal(%s) which requires %d",
			   (unsigned long) nargs),
		  nargs, PRIMNAME(op), PRIMARITY(op));
    }
#endif

    /* We use formatting and so we must initialize printing. */

    PrintDefaults();

    /* Check the arguments */

    SEXP x = CAR(args);
    if (!isVectorList(x))
	error(_("invalid first argument"));
    R_xlen_t nx = xlength(x);

    const char *csep = NULL;
    int sepw, u_sepw;
    bool sepASCII = true, sepUTF8 = false, sepBytes = false,
	sepKnown = false, use_sep = (PRIMVAL(op) == 0);
    if(use_sep) { /* paste(..., sep, .) */
	sep = CADR(args);
	if (!isString(sep) || LENGTH(sep) <= 0 || STRING_ELT(sep, 0) == NA_STRING)
	    error(_("invalid separator"));
	sep = STRING_ELT(sep, 0);
	csep = translateChar(sep);
	u_sepw = sepw = (int) strlen(csep); // will be short
	sepASCII = IS_ASCII(sep);
	sepKnown = ENC_KNOWN(sep) > 0;
	sepUTF8 = IS_UTF8(sep);
	sepBytes = IS_BYTES(sep);
	collapse = CADDR(args);
	if(correct_nargs)
	    recycle_0 = asBool2(CADDDR(args), call);
    } else { /* paste0(..., .) */
	u_sepw = sepw = 0; sep = R_NilValue;/* -Wall */
	collapse = CADR(args);
	if(correct_nargs)
	    recycle_0 = asBool2(CADDR(args), call);
    }
    bool do_collapse = (collapse != R_NilValue); // == !isNull(collapse)
    if (do_collapse)
	if(!isString(collapse) || LENGTH(collapse) <= 0 ||
	   STRING_ELT(collapse, 0) == NA_STRING)
	    error(_("invalid '%s' argument"), "collapse");

#define zero_return  \
    return (do_collapse ? mkString("") : allocVector(STRSXP, 0))

    if(nx == 0)
	zero_return;

    /* Maximum argument length, coerce if needed */

    R_xlen_t maxlen = 0;
    bool has_0_len = false;
    for (R_xlen_t j = 0; j < nx; j++) {
	if (!isString(VECTOR_ELT(x, j))) {
	    /* formerly in R code: moved to C for speed */
	    SEXP call, xj = VECTOR_ELT(x, j);
	    if(OBJECT(xj)) { /* method dispatch */
		PROTECT(call = lang2(R_AsCharacterSymbol, xj));
		SET_VECTOR_ELT(x, j, eval(call, env));
		UNPROTECT(1);
	    } else if (isSymbol(xj))
		SET_VECTOR_ELT(x, j, ScalarString(PRINTNAME(xj)));
	    else
		SET_VECTOR_ELT(x, j, coerceVector(xj, STRSXP));

	    if (!isString(VECTOR_ELT(x, j)))
		error(_("non-string argument to .Internal(%s)"), PRIMNAME(op));
	}
	if(recycle_0 && !has_0_len && XLENGTH(VECTOR_ELT(x, j)) == 0) {
	    has_0_len = true;
	    break;
	}
	else if(maxlen < XLENGTH(VECTOR_ELT(x, j)))
	        maxlen = XLENGTH(VECTOR_ELT(x, j));
    }
    if(recycle_0 && has_0_len) // one of the args was character(0)
	zero_return;
    if(maxlen == 0) // all of the arguments where (equivalent to)  character(0)
	zero_return;

    SEXP ans = PROTECT( allocVector(STRSXP, maxlen));

    bool allKnown, anyKnown, use_UTF8, use_Bytes;

    for (R_xlen_t i = 0; i < maxlen; i++) {
	/* Strategy for marking the encoding: if all inputs (including
	 * the separator) are ASCII, so is the output and we don't
	 * need to mark.  Otherwise if all non-ASCII inputs are of
	 * declared encoding, we should mark.
	 * Need to be careful only to include separator if it is used.
	 */
	anyKnown = false; allKnown = true; use_UTF8 = false; use_Bytes = false;
	if(nx > 1) {
	    allKnown = sepKnown || sepASCII;
	    anyKnown = sepKnown;
	    use_UTF8 = sepUTF8;
	    use_Bytes = sepBytes;
	}

	for (R_xlen_t j = 0; j < nx; j++) {
	    R_xlen_t k = XLENGTH(VECTOR_ELT(x, j));
	    if (k > 0) {
		SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i % k);
		if(IS_UTF8(cs)) use_UTF8 = true;
		if(IS_BYTES(cs)) use_Bytes = true;
	    }
	}
	if (use_Bytes) use_UTF8 = false;
	R_xlen_t pwidth = 0;
	const void *vmax = vmaxget();
	for (R_xlen_t j = 0; j < nx; j++) {
	    R_xlen_t k = XLENGTH(VECTOR_ELT(x, j));
	    if (k > 0) {
		if(use_Bytes)
		    pwidth += strlen(CHAR             (STRING_ELT(VECTOR_ELT(x, j), i % k)));
		else if(use_UTF8)
		    pwidth += strlen(translateCharUTF8(STRING_ELT(VECTOR_ELT(x, j), i % k)));
		else
		    pwidth += strlen(translateChar    (STRING_ELT(VECTOR_ELT(x, j), i % k)));
		vmaxset(vmax);
	    }
	}
	const char *u_csep = NULL;
	if(use_sep) {
	    if (use_UTF8 && !u_csep) {
		u_csep = translateCharUTF8(sep);
		u_sepw = (int) strlen(u_csep); // will be short
	    }
	    pwidth += (nx - 1) * (use_UTF8 ? u_sepw : sepw);
	}
	if (pwidth > INT_MAX)
	    error(_("result would exceed 2^31-1 bytes"));
	char *buf = R_AllocStringBuffer(pwidth, &cbuff);
	const char *cbuf = buf;
	vmax = vmaxget();
	for (R_xlen_t j = 0; j < nx; j++) {
	    R_xlen_t k = XLENGTH(VECTOR_ELT(x, j));
	    if (k > 0) {
		SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i % k);
		if (use_UTF8) {
		    const char *s = translateCharUTF8(cs);
		    buf = R_stpcpy(buf, s);
		} else {
		    const char *s = use_Bytes ? CHAR(cs) : translateChar(cs);
		    buf = R_stpcpy(buf, s);
		    allKnown = allKnown && (IS_ASCII(cs) || (ENC_KNOWN(cs)> 0));
		    anyKnown = anyKnown || (ENC_KNOWN(cs)> 0);
		}
	    }
	    if (sepw != 0 && j != nx - 1) {
		if (use_UTF8) {
		    strcpy(buf, u_csep);
		    buf += u_sepw;
		} else {
		    strcpy(buf, csep);
		    buf += sepw;
		}
	    }
	    vmax = vmaxget();
	}
	int ienc = 0;
	if(use_UTF8) ienc = CE_UTF8;
	else if(use_Bytes) ienc = CE_BYTES;
	else if(anyKnown && allKnown) {
	    if(known_to_be_latin1) ienc = CE_LATIN1;
	    if(known_to_be_utf8) ienc = CE_UTF8;
	}
	SET_STRING_ELT(ans, i, mkCharCE(cbuf, ienc));
    }

    /* Now collapse, if required. */

    if(do_collapse && (nx = XLENGTH(ans)) > 0) {
	sep = STRING_ELT(collapse, 0);
	use_UTF8 = IS_UTF8(sep);
	use_Bytes = IS_BYTES(sep);
	for (R_xlen_t i = 0; i < nx; i++) {
	    if(!use_UTF8  && IS_UTF8 (STRING_ELT(ans, i))) use_UTF8  = true;
	    if(!use_Bytes && IS_BYTES(STRING_ELT(ans, i))) use_Bytes = true;
	}
	if(use_Bytes) {
	    csep = CHAR(sep);
	    use_UTF8 = false;
	} else if(use_UTF8)
	    csep = translateCharUTF8(sep);
	else
	    csep = translateChar(sep);
	sepw = (int) strlen(csep);
	anyKnown = ENC_KNOWN(sep) > 0;
	allKnown = anyKnown || IS_ASCII(sep);
	R_xlen_t pwidth = 0;
	const void *vmax = vmaxget();
	for (R_xlen_t i = 0; i < nx; i++)
	    if(use_UTF8) {
		pwidth += strlen(translateCharUTF8(STRING_ELT(ans, i)));
		vmaxset(vmax);
	    } else /* already translated */
		pwidth += strlen(CHAR(STRING_ELT(ans, i)));
	pwidth += (nx - 1) * sepw;
	if (pwidth > INT_MAX)
	    error(_("result would exceed 2^31-1 bytes"));
	char *buf = R_AllocStringBuffer(pwidth, &cbuff);
	const char *cbuf = buf;
	vmax = vmaxget();
	for (R_xlen_t i = 0; i < nx; i++) {
	    if(i > 0) {
		strcpy(buf, csep);
		buf += sepw;
	    }
	    const char *s;
	    SEXP el = STRING_ELT(ans, i);
	    if(use_UTF8)
		s = translateCharUTF8(el);
	    else /* already translated */
		s = CHAR(el);
	    buf = R_stpcpy(buf, s);
	    allKnown = allKnown && (IS_ASCII(el) || (ENC_KNOWN(el) > 0));
	    anyKnown = anyKnown || (ENC_KNOWN(el) > 0);
	    if(use_UTF8) vmaxset(vmax);
	}
	UNPROTECT(1);
	int ienc = CE_NATIVE;
	if(use_UTF8) ienc = CE_UTF8;
	else if(use_Bytes) ienc = CE_BYTES;
	else if(anyKnown && allKnown) {
	    if(known_to_be_latin1) ienc = CE_LATIN1;
	    if(known_to_be_utf8)   ienc = CE_UTF8;
	}
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkCharCE(cbuf, ienc));
    }
    R_FreeStringBufferL(&cbuff);
    UNPROTECT(1);
    return ans;
}

/*
  Encoding support added for R 4.0.0.  One would normally expect file
  paths (and their components) to be in the session encoding, but on
  Windows there is some support for Unicode paths encoded (inside R) in UTF-8.

  This should not do translations with escapes.
 */
attribute_hidden SEXP do_filepath(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    /* Check the arguments */

    SEXP x = CAR(args);
    if (!isVectorList(x))
	error(_("invalid first argument"));
    int nx = length(x);
    if (nx == 0) return allocVector(STRSXP, 0);

    SEXP sep = CADR(args);
    if (!isString(sep) || LENGTH(sep) <= 0 || STRING_ELT(sep, 0) == NA_STRING)
	error(_("invalid separator"));
    sep = STRING_ELT(sep, 0);
    const char *csep = CHAR(sep);
    int sepw = (int) strlen(csep); /* hopefully 1 */

    /* Any zero-length argument gives zero-length result */
    int maxlen = 0, nzero = 0;
    for (int j = 0; j < nx; j++) {
	if (!isString(VECTOR_ELT(x, j))) {
	    SEXP call, xj = VECTOR_ELT(x, j);
	    if(OBJECT(xj)) { /* method dispatch */
		PROTECT(call = lang2(R_AsCharacterSymbol, xj));
		SET_VECTOR_ELT(x, j, eval(call, env));
		UNPROTECT(1);
	    } else if (isSymbol(xj))
		SET_VECTOR_ELT(x, j, ScalarString(PRINTNAME(xj)));
	    else
		SET_VECTOR_ELT(x, j, coerceVector(xj, STRSXP));

	    if (!isString(VECTOR_ELT(x, j)))
		error(_("non-string argument to .Internal(%s)"), PRIMNAME(op));
	}
	int ln = LENGTH(VECTOR_ELT(x, j));
	if (ln == 0) {nzero++; break;}
	if (ln > maxlen) maxlen = ln;
    }
    if (nzero || maxlen == 0) return allocVector(STRSXP, 0);

    for (int j = 0; j < nx; j++) {
	int k = LENGTH(VECTOR_ELT(x, j));
	for (int i = 0; i < k; i++) {
	    SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i);
	    if (IS_BYTES(cs))
		error(_("strings with \"bytes\" encoding are not allowed"));
	}
    }
    SEXP ans = PROTECT(allocVector(STRSXP, maxlen));

    for (int i = 0; i < maxlen; i++) {
	bool use_UTF8;
	if (utf8locale)
	    use_UTF8 = true;
	else {
	    use_UTF8 = false;
	    for (int j = 0; j < nx; j++) {
		int k = LENGTH(VECTOR_ELT(x, j));
		SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i % k);
		if(IS_UTF8(cs)) {use_UTF8 = true; break;}
		if(!latin1locale && IS_LATIN1(cs)) {use_UTF8 = true; break;}
	    }
	}
	int pwidth = 0;
	for (int j = 0; j < nx; j++) {
	    int k = LENGTH(VECTOR_ELT(x, j));
	    SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i % k);
	    if(use_UTF8)
		pwidth += (int) strlen(trCharUTF8(cs));
	    else
		pwidth += (int) strlen(translateCharFP(cs));
	}
	pwidth += (nx - 1) * sepw;
	char *buf = R_AllocStringBuffer(pwidth, &cbuff);
	const char *cbuf = buf;
	for (int j = 0; j < nx; j++) {
	    int k = LENGTH(VECTOR_ELT(x, j));
	    // k == 0 already handled above
	    SEXP cs = STRING_ELT(VECTOR_ELT(x, j), i % k);
	    const char *s;
	    if (use_UTF8)
		s = trCharUTF8(cs);
	    else
		s = translateCharFP(cs);
	    buf = R_stpcpy(buf, s);
	    if (j != nx - 1 && sepw != 0) {
		strcpy(buf, csep);
		buf += sepw;
	    }
	}
#ifdef Win32
	// Trailing seps are invalid for file paths except for / and d:/
	if(streql(csep, "/") || streql(csep, "\\")) {
	    if(buf > cbuf) {
		buf--;
		if(*buf == csep[0] && buf > cbuf &&
		   (buf != cbuf+2 || cbuf[1] != ':')) *buf = '\0';
	    }
	}
#endif
	SET_STRING_ELT(ans, i, mkCharCE(cbuf, use_UTF8 ? CE_UTF8 : 0));
    }
    R_FreeStringBufferL(&cbuff);
    UNPROTECT(1);
    return ans;
}

/* format.default(x, trim, digits, nsmall, width, justify, na.encode,
		  scientific, decimal.mark) */
attribute_hidden SEXP do_format(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    PrintDefaults();
    int scikeep = R_print.scipen;

    SEXP x = CAR(args), y, l;
    if (isEnvironment(x)) {
	return mkString(EncodeEnvironment(x));
    }
    else if (TYPEOF(x) == EXTPTRSXP)
	return mkString(EncodeExtptr(x));
    else if (!isVectorAtomic(x))
	error(_("first argument must be atomic or environment"));
    args = CDR(args);

    int trim = asLogical(CAR(args));
    if (trim == NA_INTEGER)
	error(_("invalid '%s' argument"), "trim");
    args = CDR(args);

    if (!isNull(CAR(args))) {
	int digits = asInteger(CAR(args));
	if (digits == NA_INTEGER || digits < R_MIN_DIGITS_OPT
	    || digits > R_MAX_DIGITS_OPT)
	    error(_("invalid value %d for '%s' argument"), digits, "digits");
	R_print.digits = digits;
    }
    args = CDR(args);

    int nsmall = asInteger(CAR(args));
    if (nsmall == NA_INTEGER || nsmall < 0 || nsmall > 20)
	error(_("invalid '%s' argument"), "nsmall");
    args = CDR(args);

    int wd = isNull(l = CAR(args)) ? 0 : asInteger(l);
    if(wd == NA_INTEGER)
	error(_("invalid '%s' argument"), "width");
    args = CDR(args);

    int adj = asInteger(CAR(args));
    if(adj == NA_INTEGER || adj < 0 || adj > 3)
	error(_("invalid '%s' argument"), "justify");
    args = CDR(args);

    int na = asLogical(CAR(args));
    if(na == NA_LOGICAL)
	error(_("invalid '%s' argument"), "na.encode");
    args = CDR(args);
    if(LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "scientific");
    int sci = 0;
    if(isLogical(CAR(args))) {
	int tmp = LOGICAL(CAR(args))[0];
	if(tmp == NA_LOGICAL) sci = NA_INTEGER;
	else sci = tmp > 0 ? -99 : 310;
    } else if (isNumeric(CAR(args))) {
	sci = asInteger(CAR(args));
    } else
	error(_("invalid '%s' argument"), "scientific");
    if(sci != NA_INTEGER) R_print.scipen = sci;
    args = CDR(args);
    // copy/paste from "OutDec" part of ./options.c
    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "decimal.mark");
    char *my_OutDec;
    if(STRING_ELT(CAR(args), 0) == NA_STRING)
	my_OutDec = OutDec; // default
    else {
	static char sdec[11];
#undef  _WARN_decimal_mark_non_1 /* as we now warn in EncodeReal0() */
#ifdef  _WARN_decimal_mark_non_1
 	if(R_nchar(STRING_ELT(CAR(args), 0), Chars,
		   /* allowNA = */ FALSE, /* keepNA = */ FALSE,
		   "decimal.mark") != 1) // will become an error
	    warning(_("'decimal.mark' must be a string of one character"));
#endif
	strncpy(sdec, CHAR(STRING_ELT(CAR(args), 0)), 10);
	sdec[10] = '\0';
	my_OutDec = sdec;
    }

    R_xlen_t i, n = XLENGTH(x);
    if (n <= 0) {
	PROTECT(y = allocVector(STRSXP, 0));
    } else {
	int w, d, e;
	const char *strp;
	switch (TYPEOF(x)) {

	case LGLSXP:
	    PROTECT(y = allocVector(STRSXP, n));
	    if (trim) w = 0; else formatLogical(LOGICAL(x), n, &w);
	    w = imax2(w, wd);
	    for (i = 0; i < n; i++) {
		strp = EncodeLogical(LOGICAL(x)[i], w);
		SET_STRING_ELT(y, i, mkChar(strp));
	    }
	    break;

	case INTSXP:
	    PROTECT(y = allocVector(STRSXP, n));
	    if (trim) w = 0;
	    else formatInteger(INTEGER(x), n, &w);
	    w = imax2(w, wd);
	    for (i = 0; i < n; i++) {
		strp = EncodeInteger(INTEGER(x)[i], w);
		SET_STRING_ELT(y, i, mkChar(strp));
	    }
	    break;

	case REALSXP:
	    formatReal(REAL(x), n, &w, &d, &e, nsmall);
	    if (trim) w = 0;
	    w = imax2(w, wd);
	    PROTECT(y = allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		strp = EncodeReal0(REAL(x)[i], w, d, e, my_OutDec);
		SET_STRING_ELT(y, i, mkChar(strp));
	    }
	    break;

	case CPLXSXP:
	{
	    int wi, di, ei;
	    formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei, nsmall);
	    if (trim) wi = w = 0;
	    w = imax2(w, wd); wi = imax2(wi, wd);
	    PROTECT(y = allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		strp = EncodeComplex(COMPLEX(x)[i], w, d, e, wi, di, ei, my_OutDec);
		SET_STRING_ELT(y, i, mkChar(strp));
	    }
	    break;
	}
	case STRSXP:
	{
	    /* this has to be different from formatString/EncodeString as
	       we don't actually want to encode here */
	    const char *s;
	    char *q;
	    int b, cnt = 0, j;
	    /* This is clumsy, but it saves rewriting and re-testing
	       this complex code */
	    SEXP xx = PROTECT(duplicate(x));
	    for (i = 0; i < n; i++) {
		SEXP x_i =  STRING_ELT(xx, i);
		if(IS_BYTES(x_i)) {
		    const char *p = CHAR(x_i), *q;
		    char *pp = R_alloc(4*strlen(p)+1, 1), *qq = pp, buf[5];
		    for (q = p; *q; q++) {
			unsigned char k = (unsigned char) *q;
			if (k >= 0x20 && k < 0x80) {
			    *qq++ = *q;
			} else {
			    snprintf(buf, 5, "\\x%02x", k);
			    for(int j = 0; j < 4; j++) *qq++ = buf[j];
			}
		    }
		    *qq = '\0';
		    s = pp;
		} else s = translateChar(x_i);
		if(s != CHAR(x_i)) SET_STRING_ELT(xx, i, mkChar(s));
	    }

	    w = wd;
	    if (adj != Rprt_adj_none) {
		for (i = 0; i < n; i++)
		    if (STRING_ELT(xx, i) != NA_STRING)
			w = imax2(w, Rstrlen(STRING_ELT(xx, i), 0));
		    else if (na) w = imax2(w, R_print.na_width);
	    } else w = 0;
	    /* now calculate the buffer size needed, in bytes */

	    for (i = 0; i < n; i++)
		if (STRING_ELT(xx, i) != NA_STRING) {
		    int il = Rstrlen(STRING_ELT(xx, i), 0);
		    cnt = imax2(cnt, LENGTH(STRING_ELT(xx, i)) + imax2(0, w-il));
		} else if (na)
		    cnt = imax2(cnt, R_print.na_width + imax2(0, w-R_print.na_width));
	    R_CheckStack2(cnt+1);
	    char buff[cnt+1];
	    PROTECT(y = allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		if(!na && STRING_ELT(xx, i) == NA_STRING) {
		    SET_STRING_ELT(y, i, NA_STRING);
		} else {
		    q = buff;
		    SEXP s0 = (STRING_ELT(xx, i) == NA_STRING) ? R_print.na_string : STRING_ELT(xx, i);
		    s = CHAR(s0);
		    int il = Rstrlen(s0, 0);
		    b = w - il;
		    if(b > 0 && adj != Rprt_adj_left) {
			int b0 = (adj == Rprt_adj_centre) ? b/2 : b;
			for(j = 0 ; j < b0 ; j++) *q++ = ' ';
			b -= b0;
		    }
		    for(j = 0; j < LENGTH(s0); j++) *q++ = *s++;
		    if(b > 0 && adj != Rprt_adj_right)
			for(j = 0 ; j < b ; j++) *q++ = ' ';
		    *q = '\0';
		    SET_STRING_ELT(y, i, mkChar(buff));
		}
	    }
	}
	UNPROTECT(2); /* xx , y */
	PROTECT(y);
	break;
	default:
	    error(_("Impossible mode ( x )")); y = R_NilValue;/* -Wall */
	}
    }
    if((l = getAttrib(x, R_DimSymbol)) != R_NilValue) {
	setAttrib(y, R_DimSymbol, l);
	if((l = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	    setAttrib(y, R_DimNamesSymbol, l);
    } else if((l = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	setAttrib(y, R_NamesSymbol, l);

    /* In case something else forgets to set PrintDefaults(), PR#14477 */
    R_print.scipen = scikeep;

    UNPROTECT(1); /* y */
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

attribute_hidden SEXP do_formatinfo(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x;
    int digits, nsmall, no = 1, w, d, e, wi, di, ei;

    checkArity(op, args);
    x = CAR(args);
    R_xlen_t n = XLENGTH(x);
    PrintDefaults();

    if (!isNull(CADR(args))) {
	digits = asInteger(CADR(args));
	if (digits == NA_INTEGER || digits < R_MIN_DIGITS_OPT
	    || digits > R_MAX_DIGITS_OPT)
	    error(_("invalid '%s' argument"), "digits");
	R_print.digits = digits;
    }
    nsmall = asInteger(CADDR(args));
    if (nsmall == NA_INTEGER || nsmall < 0 || nsmall > 20)
	error(_("invalid '%s' argument"), "nsmall");

    w = 0;
    d = 0;
    e = 0;
    switch (TYPEOF(x)) {

    case RAWSXP:
	formatRaw(RAW(x), n, &w);
	break;

    case LGLSXP:
	formatLogical(LOGICAL(x), n, &w);
	break;

    case INTSXP:
	formatInteger(INTEGER(x), n, &w);
	break;

    case REALSXP:
	no = 3;
	formatReal(REAL(x), n, &w, &d, &e, nsmall);
	break;

    case CPLXSXP:
	no = 6;
	wi = di = ei = 0;
	formatComplex(COMPLEX(x), n, &w, &d, &e, &wi, &di, &ei, nsmall);
	break;

    case STRSXP:
	for (R_xlen_t i = 0; i < n; i++)
	    if (STRING_ELT(x, i) != NA_STRING) {
		int il = Rstrlen(STRING_ELT(x, i), 0);
		if (il > w) w = il;
	    }
	break;

    default:
	error(_("atomic vector arguments only"));
    }
    x = allocVector(INTSXP, no);
    INTEGER(x)[0] = w;
    if(no > 1) {
	INTEGER(x)[1] = d;
	INTEGER(x)[2] = e;
	if(no > 3) {
	    INTEGER(x)[3] = wi;
	    INTEGER(x)[4] = di;
	    INTEGER(x)[5] = ei;
	}
    }
    return x;
}
