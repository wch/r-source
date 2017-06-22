/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002--2016     The R Core Team
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
 * Originally written by Jonathan Rougier
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include "RBufferUtils.h"
#include <R_ext/RS.h> /* for Calloc/Free */
#ifdef Win32
#include <trioremap.h>
#endif

#define MAXLINE MAXELTSIZE
#define MAXNARGS 100
/*               ^^^ not entirely arbitrary, but strongly linked to allowing %$1 to %$99 !*/

/*
   This is passed a format that started with % and may include other
   chars, e.g. '.2f abc'.  It's aim is to show that this is a valid
   format from one of the types given in pattern.
*/

static const char *findspec(const char *str)
{
    /* This is not strict about checking where '.' is allowed.
       It should allow  - + ' ' # 0 as flags
       m m. .n n.m as width/precision
    */
    const char *p = str;

    if(*p != '%') return p;
    for(p++; ; p++) {
	if(*p == '-' || *p == '+' || *p == ' ' || *p == '#' || *p == '.' ) continue;
	/* '*' will currently have got substituted before this */
	if(*p == '*' || (*p >= '0' && *p <= '9')) continue;
	break;
    }
    return p;
}


/*   FALSE is success, TRUE is an error: pattern *not* found . */
static Rboolean checkfmt(const char *fmt, const char *pattern)
{
    const char *p =fmt;

    if(*p != '%') return TRUE;
    p = findspec(fmt);
    return strcspn(p, pattern) ? TRUE : FALSE;
}

#define TRANSLATE_CHAR(_STR_, _i_)  \
   ((use_UTF8) ? translateCharUTF8(STRING_ELT(_STR_, _i_))  \
    : translateChar(STRING_ELT(_STR_, _i_)))


SEXP attribute_hidden do_sprintf(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, nargs, cnt, v, thislen, nfmt, nprotect = 0;
    /* fmt2 is a copy of fmt with '*' expanded.
       bit will hold numeric formats and %<w>s, so be quite small. */
    char fmt[MAXLINE+1], fmt2[MAXLINE+10], *fmtp, bit[MAXLINE+1],
	*outputString;
    const char *formatString;
    size_t n, cur, chunk;

    SEXP format, _this, a[MAXNARGS], ans /* -Wall */ = R_NilValue;
    int ns, maxlen, lens[MAXNARGS], nthis, nstar, star_arg = 0;
    static R_StringBuffer outbuff = {NULL, 0, MAXELTSIZE};
    Rboolean has_star, use_UTF8;

#define _my_sprintf(_X_)						\
    {									\
	int nc = snprintf(bit, MAXLINE+1, fmtp, _X_);			\
	if (nc > MAXLINE)						\
	    error(_("required resulting string length %d is greater than maximal %d"), \
		  nc, MAXLINE);						\
    }

    nargs = length(args);
    /* grab the format string */
    format = CAR(args);
    if (!isString(format))
	error(_("'fmt' is not a character vector"));
    nfmt = length(format);
    if (nfmt == 0) return allocVector(STRSXP, 0);
    args = CDR(args); nargs--;
    if(nargs >= MAXNARGS)
	error(_("only %d arguments are allowed"), MAXNARGS);

    /* record the args for possible coercion and later re-ordering */
    for(i = 0; i < nargs; i++, args = CDR(args)) {
	SEXPTYPE t_ai;
	a[i] = CAR(args);
	if((t_ai = TYPEOF(a[i])) == LANGSXP || t_ai == SYMSXP) /* << maybe add more .. */
	    error(_("invalid type of argument[%d]: '%s'"),
		  i+1, CHAR(type2str(t_ai)));
	lens[i] = length(a[i]);
	if(lens[i] == 0) return allocVector(STRSXP, 0);
    }

#define CHECK_maxlen							\
    maxlen = nfmt;							\
    for(i = 0; i < nargs; i++)						\
	if(maxlen < lens[i]) maxlen = lens[i];				\
    if(maxlen % nfmt)							\
	error(_("arguments cannot be recycled to the same length"));	\
    for(i = 0; i < nargs; i++)						\
	if(maxlen % lens[i])						\
	    error(_("arguments cannot be recycled to the same length"))

    CHECK_maxlen;

    outputString = R_AllocStringBuffer(0, &outbuff);

    /* We do the format analysis a row at a time */
    for(ns = 0; ns < maxlen; ns++) {
	outputString[0] = '\0';
	use_UTF8 = getCharCE(STRING_ELT(format, ns % nfmt)) == CE_UTF8;
	if (!use_UTF8) {
	    for(i = 0; i < nargs; i++) {
		if (!isString(a[i])) continue;
		if (getCharCE(STRING_ELT(a[i], ns % lens[i])) == CE_UTF8) {
		    use_UTF8 = TRUE; break;
		}
	    }
	}

	formatString = TRANSLATE_CHAR(format, ns % nfmt);
	n = strlen(formatString);
	if (n > MAXLINE)
	    error(_("'fmt' length exceeds maximal format length %d"), MAXLINE);
	/* process the format string */
	for (cur = 0, cnt = 0; cur < n; cur += chunk) {
	    const char *curFormat = formatString + cur, *ss;
	    char *starc;
	    ss = NULL;
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
		    chunk = strcspn(curFormat + 1, "diosfeEgGxXaA") + 2;
		    if (cur + chunk > n)
			error(_("unrecognised format specification '%s'"), curFormat);

		    strncpy(fmt, curFormat, chunk);
		    fmt[chunk] = '\0';

		    nthis = -1;
		    /* now look for %n$ or %nn$ form */
		    if (strlen(fmt) > 3 && fmt[1] >= '1' && fmt[1] <= '9') {
			v = fmt[1] - '0';
			if(fmt[2] == '$') {
			    if(v > nargs)
				error(_("reference to non-existent argument %d"), v);
			    nthis = v-1;
			    memmove(fmt+1, fmt+3, strlen(fmt)-2);
			} else if(fmt[2] >= '0' && fmt[2] <= '9' && fmt[3] == '$') {
			    v = 10*v + fmt[2] - '0';
			    if(v > nargs)
				error(_("reference to non-existent argument %d"), v);
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
				if(v > nargs)
				    error(_("reference to non-existent argument %d"), v);
				nstar = v-1;
				memmove(starc+1, starc+3, strlen(starc)-2);
			    } else if(starc[2] >= '0' && starc[2] <= '9'
				      && starc[3] == '$') {
				v = 10*v + starc[2] - '0';
				if(v > nargs)
				    error(_("reference to non-existent argument %d"), v);
				nstar = v-1;
				memmove(starc+1, starc+4, strlen(starc)-3);
			    }
			}

			if(nstar < 0) {
			    if (cnt >= nargs) error(_("too few arguments"));
			    nstar = cnt++;
			}

			if (Rf_strchr(starc+1, '*'))
			    error(_("at most one asterisk '*' is supported in each conversion specification"));

			_this = a[nstar];
			if(ns == 0 && TYPEOF(_this) == REALSXP) {
			    _this = coerceVector(_this, INTSXP);
			    PROTECT(a[nstar] = _this);
			    nprotect++;
			}
			if(TYPEOF(_this) != INTSXP || LENGTH(_this)<1 ||
			   INTEGER(_this)[ns % LENGTH(_this)] == NA_INTEGER)
			    error(_("argument for '*' conversion specification must be a number"));
			star_arg = INTEGER(_this)[ns % LENGTH(_this)];
			has_star = TRUE;
		    }
		    else
			has_star = FALSE;

		    if (fmt[strlen(fmt) - 1] == '%') {
			/* handle % with formatting options */
			if (has_star)
			    snprintf(bit, MAXLINE+1, fmt, star_arg);
			else
			    strcpy(bit, fmt);
			/* was sprintf(..)  for which some compiler warn */
		    } else {
			Rboolean did_this = FALSE;
			if(nthis < 0) {
			    if (cnt >= nargs) error(_("too few arguments"));
			    nthis = cnt++;
			}
			_this = a[nthis];
			if (has_star) {
			    size_t nf; char *p, *q = fmt2;
			    for (p = fmt; *p; p++)
				if (*p == '*') q += sprintf(q, "%d", star_arg);
				else *q++ = *p;
			    *q = '\0';
			    nf = strlen(fmt2);
			    if (nf > MAXLINE)
				error(_("'fmt' length exceeds maximal format length %d"),
				      MAXLINE);
			    fmtp = fmt2;
			} else fmtp = fmt;

#define CHECK_this_length						\
			do {						\
			    PROTECT(_this);				\
			    thislen = length(_this);			\
			    if(thislen == 0)				\
				error(_("coercion has changed vector length to 0")); \
			} while (0)

			/* Now let us see if some minimal coercion
			   would be sensible, but only do so once, for ns = 0: */
			if(ns == 0) {
			    SEXP tmp; Rboolean do_check;
			    switch(*findspec(fmtp)) {
			    case 'd':
			    case 'i':
			    case 'o':
			    case 'x':
			    case 'X':
				if(TYPEOF(_this) == REALSXP) {
				    // qdapTools manages to call this with NaN
				    Rboolean exactlyInteger = TRUE;
				    R_xlen_t i = 0;
				    R_xlen_t n = XLENGTH(_this);
				    for(i = 0; i < n; i++) {
					double r = REAL(_this)[i];
					if (R_IsNA(r)) continue; // NA_REAL is ok
					if (!R_FINITE(r) || (double)((int) r) != r) {
					    exactlyInteger = FALSE;
					    break;
					}
				    } 
				    if(exactlyInteger)
					_this = coerceVector(_this, INTSXP);
				    PROTECT(a[nthis] = _this);
				    nprotect++;
				}
				break;
			    case 'a':
			    case 'A':
			    case 'e':
			    case 'f':
			    case 'g':
			    case 'E':
			    case 'G':
				if(TYPEOF(_this) != REALSXP &&
				   /* no automatic as.double(<string>) : */
				   TYPEOF(_this) != STRSXP) {
				    PROTECT(tmp = lang2(install("as.double"), _this));
#define COERCE_THIS_TO_A						\
				    _this = eval(tmp, env);		\
				    UNPROTECT(1);			\
				    PROTECT(a[nthis] = _this);		\
				    nprotect++;				\
				    did_this = TRUE;			\
				    CHECK_this_length;			\
				    do_check = (lens[nthis] == maxlen);	\
				    lens[nthis] = thislen; /* may have changed! */ \
				    if(do_check && thislen < maxlen) {	\
					CHECK_maxlen;			\
				    }

				    COERCE_THIS_TO_A
				}
				break;
			    case 's':
				if(TYPEOF(_this) != STRSXP) {
				    /* as.character method might call sprintf() */
				    size_t nc = strlen(outputString);
				    char *z = Calloc(nc+1, char);
				    strcpy(z, outputString);
				    PROTECT(tmp = lang2(R_AsCharacterSymbol, _this));

				    COERCE_THIS_TO_A
				    strcpy(outputString, z);
				    Free(z);
				}
				break;
			    default:
				break;
			    }
			} /* ns == 0 (first-time only) */

			if(!did_this)
			    CHECK_this_length;

			switch(TYPEOF(_this)) {
			case LGLSXP:
			    {
				int x = LOGICAL(_this)[ns % thislen];
				if (checkfmt(fmtp, "di"))
				    error(_("invalid format '%s'; %s"), fmtp,
					  _("use format %d or %i for logical objects"));
				if (x == NA_LOGICAL) {
				    fmtp[strlen(fmtp)-1] = 's';
				    _my_sprintf("NA")
				} else {
				    _my_sprintf(x)
				}
				break;
			    }
			case INTSXP:
			    {
				int x = INTEGER(_this)[ns % thislen];
				if (checkfmt(fmtp, "dioxX"))
				    error(_("invalid format '%s'; %s"), fmtp,
					  _("use format %d, %i, %o, %x or %X for integer objects"));
				if (x == NA_INTEGER) {
				    fmtp[strlen(fmtp)-1] = 's';
				    _my_sprintf("NA")
				} else {
				    _my_sprintf(x)
				}
				break;
			    }
			case REALSXP:
			    {
				double x = REAL(_this)[ns % thislen];
				if (checkfmt(fmtp, "aAfeEgG"))
				    error(_("invalid format '%s'; %s"), fmtp,
					  _("use format %f, %e, %g or %a for numeric objects"));
				if (R_FINITE(x)) {
				    _my_sprintf(x)
				} else {
				    char *p = Rf_strchr(fmtp, '.');
				    if (p) {
					*p++ = 's'; *p ='\0';
				    } else
					fmtp[strlen(fmtp)-1] = 's';
				    if (ISNA(x)) {
					if (strcspn(fmtp, " ") < strlen(fmtp))
					    _my_sprintf(" NA")
					else
					    _my_sprintf("NA")
				    } else if (ISNAN(x)) {
					if (strcspn(fmtp, " ") < strlen(fmtp))
					    _my_sprintf(" NaN")
					else
					    _my_sprintf("NaN")
				    } else if (x == R_PosInf) {
					if (strcspn(fmtp, "+") < strlen(fmtp))
					    _my_sprintf("+Inf")
					else if (strcspn(fmtp, " ") < strlen(fmtp))
					    _my_sprintf(" Inf")
					else
					    _my_sprintf("Inf")
				    } else if (x == R_NegInf)
					_my_sprintf("-Inf")
				}
				break;
			    }
			case STRSXP:
			    /* NA_STRING will be printed as 'NA' */
			    if (checkfmt(fmtp, "s"))
				error(_("invalid format '%s'; %s"), fmtp,
				      _("use format %s for character objects"));

			    ss = TRANSLATE_CHAR(_this, ns % thislen);
			    if(fmtp[1] != 's') {
				if(strlen(ss) > MAXLINE)
				    warning(_("likely truncation of character string to %d characters"),
					    MAXLINE-1);
				_my_sprintf(ss)
				bit[MAXLINE] = '\0';
				ss = NULL;
			    }
			    break;

			default:
			    error(_("unsupported type"));
			    break;
			}

			UNPROTECT(1);
		    }
		}
	    }
	    else { /* not '%' : handle string part */
		char *ch = Rf_strchr(curFormat, '%'); /* MBCS-aware version used */
		chunk = (ch) ? (size_t) (ch - curFormat) : strlen(curFormat);
		strncpy(bit, curFormat, chunk);
		bit[chunk] = '\0';
	    }
	    if(ss) {
		outputString = R_AllocStringBuffer(strlen(outputString) +
						   strlen(ss) + 1, &outbuff);
		strcat(outputString, ss);
	    } else {
		outputString = R_AllocStringBuffer(strlen(outputString) +
						   strlen(bit) + 1, &outbuff);
		strcat(outputString, bit);
	    }
	}  /* end for ( each chunk ) */

	if(ns == 0) { /* may have adjusted maxlen now ... */
	    PROTECT(ans = allocVector(STRSXP, maxlen));
	    nprotect++;
	}
	SET_STRING_ELT(ans, ns, mkCharCE(outputString,
					 use_UTF8 ? CE_UTF8 : CE_NATIVE));
    } /* end for(ns ...) */

    UNPROTECT(nprotect);
    R_FreeStringBufferL(&outbuff);
    return ans;
}
