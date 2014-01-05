/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002--2013     The R Core Team
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
 *
 * Formerly part of src/main/sprintf.c
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include "RBufferUtils.h"

#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif


#define MAXLINE MAXELTSIZE
#define MAXNARGS 100
/*               ^^^ not entirely arbitrary, but strongly linked to allowing %$1 to %$99 !*/


#define TRANSLATE_CHAR(_STR_, _i_)  \
   ((use_UTF8) ? translateCharUTF8(STRING_ELT(_STR_, _i_))  \
    : translateChar(STRING_ELT(_STR_, _i_)))

SEXP getfmts(SEXP format)
{
    int cnt, v, nfmt;
    char fmt[MAXLINE+1], bit[MAXLINE+1];
    const char *formatString;
    size_t n, cur, chunk, maxlen = 0;

    int nthis, nstar;
    Rboolean use_UTF8;
    const void *vmax = vmaxget();
    
    SEXP res = PROTECT(allocVector(STRSXP, MAXNARGS));
    
#define SET_RESULT(n, s) {						\
     if (n >= MAXNARGS) error(_("only %d arguments are allowed"), MAXNARGS); \
	maxlen = (n) < maxlen ? maxlen : (n) + 1;			\
	SET_STRING_ELT(res, (n), mkChar(s));				\
    }
    
    if (!isString(format)) error(_("'fmt' is not a character vector"));
    nfmt = LENGTH(format);
    if (nfmt != 1) 
        error(_("'fmt' must be length 1"));

    use_UTF8 = getCharCE(STRING_ELT(format, 0)) == CE_UTF8;
    formatString = TRANSLATE_CHAR(format, 0);
    n = strlen(formatString);
    if (n > MAXLINE)
	error(_("'fmt' length exceeds maximal format length %d"), MAXLINE);
    /* process the format string */
    for (cur = 0, cnt = 0; cur < n; cur += chunk) {
	const char *curFormat = formatString + cur;
	char *starc;
	if (formatString[cur] == '%') { /* handle special format command */

	    if (cur < n - 1 && formatString[cur + 1] == '%') {
		/* take care of %% in the format */
		chunk = 2;
		strcpy(bit, "%");
	    }
	    else {
		/* recognise selected types from Table B-1 of K&R */
		/* NB: we deal with "%%" in branch above. */
		/* This is MBCS-OK, as we are in a format spec */
		    
		/*  Include formats c, u, p and n as well as the R formats; this needs to match */
		/*  C code as well */
		chunk = strcspn(curFormat + 1, "diosfeEgGxXaAcupn") + 2;
		if (cur + chunk > n)
		    error(_("unrecognised format specification '%s'"), curFormat);

		strncpy(fmt, curFormat, chunk);
		fmt[chunk] = '\0';

		nthis = -1;
		/* now look for %n$ or %nn$ form */
		if (strlen(fmt) > 3 && fmt[1] >= '1' && fmt[1] <= '9') {
		    v = fmt[1] - '0';
		    if(fmt[2] == '$') {
			nthis = v-1;
			memmove(fmt+1, fmt+3, strlen(fmt)-2);
		    } else if(fmt[2] >= '0' && fmt[2] <= '9' && fmt[3] == '$') {
			v = 10*v + fmt[2] - '0';
			nthis = v-1;
			memmove(fmt+1, fmt+4, strlen(fmt)-3);
		    }
		}

		starc = Rf_strchr(fmt, '*');
		if (starc) { /* handle  *  format if present */
		    nstar = -1;
		    if (strlen(starc) > 3 && starc[1] >= '1' && starc[1] <= '9') {
			v = starc[1] - '0';
			if(starc[2] == '$') {
			    nstar = v-1;
			    memmove(starc+1, starc+3, strlen(starc)-2);
			} else if(starc[2] >= '0' && starc[2] <= '9'
				  && starc[3] == '$') {
			    v = 10*v + starc[2] - '0';
			    nstar = v-1;
			    memmove(starc+1, starc+4, strlen(starc)-3);
			}
		    }

		    if(nstar < 0) {
			nstar = cnt++;
		    }

		    if (Rf_strchr(starc+1, '*'))
			error(_("at most one asterisk '*' is supported in each conversion specification"));

		    SET_RESULT(nstar, "*");

		}

		if (fmt[strlen(fmt) - 1] == '%') {
		} else {
		    if(nthis < 0) {
			nthis = cnt++;
		    }
		    SET_RESULT(nthis, fmt);
		}
	    }
	}
	else { /* not '%' : handle string part */
	    char *ch = Rf_strchr(curFormat, '%'); /* MBCS-aware version used */
	    chunk = (ch) ? (size_t) (ch - curFormat) : strlen(curFormat);
	    strncpy(bit, curFormat, chunk);
	    bit[chunk] = '\0';
	}
    }  /* end for ( each chunk ) */

    res = xlengthgets(res, maxlen);
    vmaxset(vmax);
    UNPROTECT(1);
    return res;
}
