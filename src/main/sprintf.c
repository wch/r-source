/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-5     the R Development Core Team
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
 * Originally written by Jonathan Rougier, email J.C.Rougier@durham.ac.uk
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

#define MAXLINE MAXELTSIZE

SEXP do_sprintf(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, nargs, cnt, v, thislen;
    char *formatString;
    char fmt[MAXLINE+1], bit[MAXLINE+1], outputString[MAXLINE+1];
    size_t n, cur, chunk;

    SEXP format, ans, this, a[100], tmp;
    int ns, maxlen, lens[100], nthis;

    /* grab the format string */

    nargs = length(args);
    format = CAR(args);
    if (!isString(format) || length(format) == 0)
	errorcall(call, _("'fmt' is not a non-empty character vector"));
    args = CDR(args); nargs--;
    if(nargs >= 100)
	errorcall(call, _("only 100 arguments are allowed"));

    /* record the args for later re-ordering */
    for(i = 0; i < nargs; i++, args = CDR(args)) a[i] = CAR(args);

    maxlen = length(format);

    for(i = 0; i < nargs; i++) {
	lens[i] = length(a[i]);
	if(lens[i] == 0)
	    errorcall(call, _("zero-length argument"));
	if(maxlen < lens[i]) maxlen = lens[i];
    }
    if(maxlen % length(format))
	errorcall(call, _("arguments cannot be recycled to the same length"));
    for(i = 0; i < nargs; i++) {
	if(maxlen % lens[i])
	    errorcall(call, _("arguments cannot be recycled to the same length"));
    }

    PROTECT(ans = allocVector(STRSXP, maxlen));
    for(ns = 0; ns < maxlen; ns++) {
	cnt = 0;
	outputString[0] = '\0';
	formatString = CHAR(STRING_ELT(format, ns % length(format)));
	n = strlen(formatString);
	if (n > MAXLINE)
	    errorcall(call, _("'fmt' length exceeds maximal buffer length %d"),
		      MAXLINE);
 	/* process the format string */
	for (cur = 0; cur < n; cur += chunk) {

	    if (formatString[cur] == '%') { /* handle special format command */

		if (cur < n - 1 && formatString[cur + 1] == '%') {
		    /* take care of %% in the format */
		    chunk = 2;
		    strcpy(bit, "%");
		}
		else {
		    /* recognise selected types from Table B-1 of K&R */
		    /* This is MBCS-OK, as we are in a format spec */
		    chunk = strcspn(formatString + cur, "disfeEgGxX") + 1;
		    if (cur + chunk > n)
			errorcall(call, _("unrecognised format at end of string"));

		    strncpy(fmt, formatString + cur, chunk);
		    fmt[chunk] = '\0';

		    nthis = -1;
		    /* now look for %n$ or %nn$ form */
		    if (strlen(fmt) > 3 && fmt[1] >= '1' && fmt[1] <= '9') {
			v = fmt[1] - '0';
			if(fmt[2] == '$') {
			    if(v > nargs)
				errorcall(call, _("reference to non-existent argument %d"), v);
			    nthis = v-1;
			    memmove(fmt+1, fmt+3, strlen(fmt)-2);
			} else if(fmt[2] >= '1' && fmt[2] <= '9' 
				  && fmt[3] == '$') {
			    v = 10*v + fmt[2] - '0';
			    if(v > nargs)
				errorcall(call, _("reference to non-existent argument %d"), v);
			    nthis = v-1;
			    memmove(fmt+1, fmt+4, strlen(fmt)-3);
			}
		    }
		
		    if(nthis < 0) {
			if (cnt >= nargs) errorcall(call, _("too few arguments"));
			nthis = cnt++;
		    }
		    this = a[nthis];

		    /* Now let us see if some minimal coercion would be sensible.
		    */
		    switch(tolower(fmt[strlen(fmt) - 1])) {
		    case 'd':
		    case 'i':
		    case 'x':
		    case 'X':
			if(TYPEOF(this) == REALSXP) {
			    double r = REAL(this)[0];
			    if((double)((int) r) == r)
				this = coerceVector(this, INTSXP);
			}
			break;
		    case 'e':
		    case 'f':
		    case 'g':
			if(TYPEOF(this) != REALSXP) {
			    PROTECT(tmp = lang2(install("as.double"), this));
			    this = eval(tmp, env);
			    UNPROTECT(1);
			}
			break;
		    case 's':
			if(TYPEOF(this) != STRSXP) {
			    PROTECT(tmp = 
				    lang2(install("as.character"), this));
			    this = eval(tmp, env);
			    UNPROTECT(1);
			}
			break;
		    default:
			break;
		    }
		    PROTECT(this);
		    thislen = length(this);
		    if(thislen == 0)
			error(_("coercion has changed vector length to 0"));

		    switch(TYPEOF(this)) {
		    case LGLSXP:
		    {
			int x = LOGICAL(this)[ns % thislen];
			if (strcspn(fmt, "di") >= strlen(fmt))
			    error("%s", 
				  _("use format %d or %i for logical objects"));
			if (x == NA_LOGICAL) {
			    fmt[strlen(fmt)-1] = 's';
			    sprintf(bit, fmt, "NA");
			} else
			    sprintf(bit, fmt, x);
			break;
		    }
		    case INTSXP:
		    {
			int x = INTEGER(this)[ns % thislen];
			if (strcspn(fmt, "dixX") >= strlen(fmt))
			    error("%s",
				  _("use format %d, %i, %x or %X for integer objects"));
			if (x == NA_INTEGER) {
			    fmt[strlen(fmt)-1] = 's';
			    sprintf(bit, fmt, "NA");
			} else
			    sprintf(bit, fmt, x);
			break;
		    }
		    case REALSXP:
		    {
			double x = REAL(this)[ns % thislen];
			if (strcspn(fmt, "feEgG") >= strlen(fmt))
			    error("%s", 
				  _("use format %f, %e or %g for numeric objects"));
			if (R_FINITE(x)) {
			    sprintf(bit, fmt, x);
			} else {
			    char *p = strchr(fmt, '.');
			    if (p) {
				*p++ = 's'; *p ='\0';
			    } else
				fmt[strlen(fmt)-1] = 's';
			    if (ISNA(x)) {
				if (strcspn(fmt, " ") < strlen(fmt))
				    sprintf(bit, fmt, " NA");
				else
				    sprintf(bit, fmt, "NA");
			    } else if (ISNAN(x)) {
				if (strcspn(fmt, " ") < strlen(fmt))
				    sprintf(bit, fmt, " NaN");
				else
				    sprintf(bit, fmt, "NaN");
			    } else if (x == R_PosInf) {
				if (strcspn(fmt, "+") < strlen(fmt))
				    sprintf(bit, fmt, "+Inf");
				else if (strcspn(fmt, " ") < strlen(fmt))
				    sprintf(bit, fmt, " Inf");
				else
				    sprintf(bit, fmt, "Inf");
			    } else if (x == R_NegInf)
				sprintf(bit, fmt, "-Inf");
			}
			break;
		    }
		    case STRSXP:
			/* NA_STRING will be printed as `NA' */
			if (strcspn(fmt, "s") >= strlen(fmt))
			    error("%s", _("use format %s for character objects"));
			if(strlen(CHAR(STRING_ELT(this, ns % thislen)))
			   > MAXLINE)
			    warning(_("Likely truncation of character string"));
			snprintf(bit, MAXLINE, fmt, 
				 CHAR(STRING_ELT(this, ns % thislen)));
			bit[MAXLINE] = '\0';
			break;
		    default:
			errorcall(call, _("unsupported type"));
			break;
		    }
		    UNPROTECT(1);
		}
	    }
	    else { /* not '%' : handle string part */
		char *ch = strchr(formatString + cur, '%'); /* MBCS-aware
							       version used */
		if(ch) chunk = ch - formatString - cur;
		else chunk = strlen(formatString + cur);
		strncpy(bit, formatString + cur, chunk);
		bit[chunk] = '\0';
	    }

	    if (strlen(outputString) + strlen(bit) > MAXLINE)
		errorcall(call, _("String length exceeds buffer size of %d"), 
			  MAXLINE);
	    strcat(outputString, bit);
	}
	SET_STRING_ELT(ans, ns, mkChar(outputString));
    }

    UNPROTECT(1);
    return ans;
}
