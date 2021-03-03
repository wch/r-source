/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2021   The R Core Team.
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
# include <config.h>
#endif

#include <string.h>
#include <R.h>
#include <Rinternals.h>

#include "utils.h"

/* from src/main/eval.c */
SEXP do_Rprof(SEXP args);

SEXP Rprof(SEXP args)
{
    return do_Rprof(CDR(args));
}

/* from src/main/memory.c */
SEXP do_Rprofmem(SEXP args);
SEXP Rprofmem(SEXP args)
{
    return do_Rprofmem(CDR(args));
}

/* from src/main/dounzip.c */
SEXP Runzip(SEXP args);

SEXP unzip(SEXP args)
{
    return Runzip(CDR(args));
}

#include <wctype.h>
#include "rlocale.h" // may remap iswctype, wctype

/* Declarations from Defn.h */
int IS_ASCII(SEXP x);
int IS_UTF8(SEXP x);
const wchar_t *Rf_wtransChar(SEXP x);

#if defined(USE_RI18N_FNS) || (defined(HAVE_ISWCTYPE) && defined(HAVE_WCTYPE))
SEXP charClass(SEXP x, SEXP scl)
{
    R_xlen_t n;
    const int *px;
    if (isString(x)) {
	if (XLENGTH(x) != 1)
	    error(_("argument 'x' must be a length-1 character vector"));
	if (IS_ASCII(x) || IS_UTF8(x))
	    error(_("argument 'x' must be UTF-8 encoded (including ASCII)"));
	PROTECT(x); // PROTECT for balance only
	SEXP sx = STRING_ELT(x, 0);
	const wchar_t *wx = Rf_wtransChar(sx);
	n = wcslen(wx);;
	px = (const int*) wx;
    } else {
	PROTECT(x = coerceVector(x, INTSXP));
	n = XLENGTH(x);
	px = INTEGER(x);
    }

    if (!isString(scl))
	error(_("argument 'class' must be a character string"));
    const char *cl = CHAR(STRING_ELT(scl, 0));
    wctype_t wcl = wctype(cl);
    if(wcl == 0) error("character class \"%s\" is invalid", cl);

    SEXP ans = allocVector(LGLSXP, n);
    int *pans = LOGICAL(ans);
    for (R_xlen_t i = 0; i < n; i++) {
	int this = px[i];
	if (this < 0) pans[i] = NA_LOGICAL;
	else pans[i] = iswctype(this, wcl);
    }
    UNPROTECT(1);
    return ans;
}
#else
SEXP charClass(SEXP x, SEXP scl)
{
    error("'charClass' is not available on this platform");
    return R_NilValue;
}
#endif


#include <lzma.h>

SEXP crc64(SEXP in)
{
    uint64_t crc = 0;
    char ans[17];
    if (!isString(in)) error("input must be a character string");
    const char *str = CHAR(STRING_ELT(in, 0));

    /* Seems this is really 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return mkString(ans);
}

// As from 3.3.0 this means on Unix.
#if defined(HAVE_ARPA_INET_H)
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

SEXP nsl(SEXP hostname)
{
    SEXP ans = R_NilValue;
    const char *name; char ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    if (!isString(hostname) || length(hostname) != 1)
	error(_("'hostname' must be a character vector of length 1"));
    name = translateChar(STRING_ELT(hostname, 0));

    hp = gethostbyname(name);

    if (hp == NULL) {		/* cannot resolve the address */
	warning(_("nsl() was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warning(_("unknown format returned by 'gethostbyname'"));
	}
	ans = mkString(ip);
    }
    return ans;
}
#else
SEXP nsl(SEXP hostname)
{
    warning(_("nsl() is not supported on this platform"));
    return R_NilValue;
}
#endif
