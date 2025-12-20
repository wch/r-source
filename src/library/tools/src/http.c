/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2092--2025     The R Core Team
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
 */

#include <Rinternals.h>
#include "tools.h"

extern int extR_HTTPDCreate(const char *ip, int port);
extern void extR_HTTPDStop(void);

SEXP startHTTPD(SEXP sIP, SEXP sPort)
{
    const char *ip = 0;
    if (sIP != R_NilValue && (TYPEOF(sIP) != STRSXP || LENGTH(sIP) != 1))
	error(_("invalid bind address specification"));
    if (sIP != R_NilValue) ip = CHAR(STRING_ELT(sIP, 0));
    int port = asInteger(sPort);
    if (port < 0 || port > 65535)
	error(
	    _("Invalid port number %d: should be in 0:65535, typically above 1024"),
	    port);
    return ScalarInteger(extR_HTTPDCreate(ip, port));
}

SEXP stopHTTPD(void)
{
    extR_HTTPDStop();
    return R_NilValue;
}

/* Copied from src/modules/internet/Rhttp.c, modulo changing
   char *p -> const char *p
   Rstrdup -> strdup
   error("...") -> error(_("..."))
*/
/* Remove . and (most) .. from "p" following RFC 3986, 5.2.4.*/
static char *remove_dot_segments(const char *p) {

    char *inbuf = strdup(p);
    char *in = inbuf;   /* first byte of input buffer */

    char *outbuf = malloc(strlen(inbuf) + 1);
    if (!outbuf)
	error(_("allocation error in remove_dot_segments"));
    char *out = outbuf; /* last byte (terminator) of output buffer */
    *out = '\0';
    
    while(*in) {
/*
       A.  If the input buffer begins with a prefix of "../" or "./",
           then remove that prefix from the input buffer; otherwise,
*/
	if (in[0] == '.' && in[1] == '.' && in[2] == '/') {
	    /* remove "../" */
	    in += 3;
	    continue;
	}
	if (in[0] == '.' && in[1] == '/') {
	    /* remove "./" */
	    in += 2;
	    continue;
	}
/*
       B.  if the input buffer begins with a prefix of "/./" or "/.",
           where "." is a complete path segment, then replace that
           prefix with "/" in the input buffer; otherwise,
*/
	if (in[0] == '/' && in[1] == '.' && in[2] == '/') {
	    /* replace "/./" by "/"  */
	    in += 2;
	    continue;
	}
	if (in[0] == '/' && in[1] == '.' && in[2] == '\0') {
	    /* replace trailing "/." by "/"  */
	    in[1] = '\0';
	    continue;
	}
/*
       C.  if the input buffer begins with a prefix of "/../" or "/..",
           where ".." is a complete path segment, then replace that
           prefix with "/" in the input buffer and remove the last
           segment and its preceding "/" (if any) from the output
           buffer; otherwise,
*/
	if (in[0] == '/' && in[1] == '.' && in[2] == '.' && in[3] == '/') {
	    /* replace "/../" by "/" */
	    in += 3;
	    /* remove trailing "/segment" from output */
	    while(out > outbuf && *out != '/') out--;
	    *out = '\0';
	    continue;
	}
	if (in[0] == '/' && in[1] == '.' && in[2] == '.' && in[3] == '\0') {
	    /* replace trailing "/.." by "/" */
	    in[1] = '\0';
	    /* remove trailing "/segment" from output */
	    while(out > outbuf && *out != '/') out--;
	    *out = '\0';
	    continue;
	}
/*
       D.  if the input buffer consists only of "." or "..", then remove
           that from the input buffer; otherwise,
*/
	if ( (in[0] == '.' && in[1] == '\0') ||
	     (in[0] == '.' && in[1] == '.' && in[2] == '\0') ) {
	    /* remove input */
	    in[0] = '\0';
	    continue;
	}
/*
       E.  move the first path segment in the input buffer to the end of
           the output buffer, including the initial "/" character (if
           any) and any subsequent characters up to, but not including,
           the next "/" character or the end of the input buffer.
*/
	if (in[0] == '/') {
	    *out++ = '/';
	    in++;
	}
	for(; *in && *in != '/'; in++) *out++ = *in;
	*out = '\0';
    }

    free(inbuf);
    return outbuf;
}

SEXP remove_dot_segments_wrapper(SEXP x) {
    R_xlen_t i, n = XLENGTH(x);
    SEXP s, y;

    if(TYPEOF(x) != STRSXP)
	error(_("non-character argument"));
    PROTECT(y = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	s = STRING_ELT(x, i);
	if(s == NA_STRING) {
	    SET_STRING_ELT(y, i, NA_STRING);
	    continue;
	}
	SET_STRING_ELT(y, i, mkChar(remove_dot_segments(CHAR(s))));
    }
    UNPROTECT(1);
    return y;
}

