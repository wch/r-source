/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2015   The R Core Team.
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

/* <UTF8> OK, provided the delimiters are ASCII
   match length is in now in chars.
*/

#include <R.h>
#include "tools.h"

#include <stdlib.h> /* for MB_CUR_MAX */
#include <wchar.h>
LibExtern Rboolean mbcslocale;
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);

/* .Call, so manages R_alloc stack */
SEXP
delim_match(SEXP x, SEXP delims)
{
    /*
      Match delimited substrings in a character vector x.

      Returns an integer vector with the same length of x giving the
      starting position of the match (including the start delimiter), or
      -1 if there is none, with attribute "match.length" giving the
      length of the matched text (including the end delimiter), or -1
      for no match.

      This is still very experimental.

      Currently, the start and end delimiters must be single characters;
      it would be nice to allow for arbitrary regexps.

      Currently, the only syntax supported is Rd ('\' is the escape
      character, '%' starts a comment extending to the next newline, no
      quote characters.  It would be nice to generalize this, too.

      Nevertheless, this is already useful for parsing Rd.
    */

    char c;
    const char *s, *delim_start, *delim_end;
    Sint n, i, pos, start, end, delim_depth;
    int lstart, lend;
    Rboolean is_escaped, equal_start_and_end_delims;
    SEXP ans, matchlen;
    mbstate_t mb_st; int used;

    if(!isString(x) || !isString(delims) || (length(delims) != 2))
	error(_("invalid argument type"));

    delim_start = translateChar(STRING_ELT(delims, 0));
    delim_end = translateChar(STRING_ELT(delims, 1));
    lstart = (int) strlen(delim_start); lend = (int) strlen(delim_end);
    equal_start_and_end_delims = strcmp(delim_start, delim_end) == 0;

    n = length(x);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for(i = 0; i < n; i++) {
	memset(&mb_st, 0, sizeof(mbstate_t));
	start = end = -1;
	s = translateChar(STRING_ELT(x, i));
	pos = is_escaped = delim_depth = 0;
	while((c = *s) != '\0') {
	    if(c == '\n') {
		is_escaped = FALSE;
	    }
	    else if(c == '\\') {
		is_escaped = is_escaped ? FALSE : TRUE;
	    }
	    else if(is_escaped) {
		is_escaped = FALSE;
	    }
	    else if(c == '%') {
		while((c != '\0') && (c != '\n')) {
		    if(mbcslocale) {
			used = (int) Rf_mbrtowc(NULL, s, MB_CUR_MAX, &mb_st);
			if(used == 0) break;
			s += used; c = *s;
		    } else
			c = *++s;
		    pos++;
		}
	    }
	    else if(strncmp(s, delim_end, lend) == 0) {
		if(delim_depth > 1) delim_depth--;
		else if(delim_depth == 1) {
		    end = pos;
		    break;
		}
		else if(equal_start_and_end_delims) {
		    start = pos;
		    delim_depth++;
		}
	    }
	    else if(strncmp(s, delim_start, lstart) == 0) {
		if(delim_depth == 0) start = pos;
		delim_depth++;
	    }
	    if(mbcslocale) {
		used = (int) Rf_mbrtowc(NULL, s, MB_CUR_MAX, &mb_st);
		if(used == 0) break;
		s += used;
	    } else
		s++; pos++;
	}
	if(end > -1) {
	    INTEGER(ans)[i] = start + 1; /* index from one */
	    INTEGER(matchlen)[i] = end - start + 1;
	}
	else {
	    INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	}
    }
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return(ans);
}

SEXP
check_nonASCII(SEXP text, SEXP ignore_quotes)
{
    /* Check if all the lines in 'text' are ASCII, after removing
       comments and ignoring the contents of quotes (unless ignore_quotes)
       (which might span more than one line and might be escaped).

       This cannot be entirely correct, as quotes and \ might occur as
       part of another character in a MBCS: but this does not happen
       in UTF-8.
    */
    int i, nbslash = 0; /* number of preceding backslashes */
    const char *p;
    char quote= '\0';
    Rboolean ign, inquote = FALSE;

    if(TYPEOF(text) != STRSXP) error("invalid input");
    ign = asLogical(ignore_quotes);
    if(ign == NA_LOGICAL) error("'ignore_quotes' must be TRUE or FALSE");

    for (i = 0; i < LENGTH(text); i++) {
	p = CHAR(STRING_ELT(text, i)); // ASCII or not not affected by charset
	inquote = FALSE; /* avoid runaway quotes */
	for(; *p; p++) {
	    if(!inquote && *p == '#') break;
	    if(!inquote || ign) {
		if((unsigned int) *p > 127) {
		    /* Rprintf("%s\n", CHAR(STRING_ELT(text, i)));
		       Rprintf("found %x\n", (unsigned int) *p); */
		    return ScalarLogical(TRUE);
		}
	    }
	    if((nbslash % 2 == 0) && (*p == '"' || *p == '\'')) {
		if(inquote && *p == quote) {
		    inquote = FALSE;
		} else if(!inquote) {
		    quote = *p;
		    inquote = TRUE;
		}
	    }
	    if(*p == '\\') nbslash++; else nbslash = 0;
	}
    }
    return ScalarLogical(FALSE);
}

SEXP check_nonASCII2(SEXP text)
{
    SEXP ans = R_NilValue;
    int i, m = 0, m_all = 100, *ind, *ians, yes;
    const char *p;

    if(TYPEOF(text) != STRSXP) error("invalid input");
    ind = Calloc(m_all, int);
    for (i = 0; i < LENGTH(text); i++) {
	p = CHAR(STRING_ELT(text, i));
	yes = 0;
	for(; *p; p++)
	    if((unsigned int) *p > 127) {
		yes = 1;
		break;
	    }
	if(yes) {
	    if(m >= m_all) {
		m_all *= 2;
		ind = Realloc(ind, m_all, int);
	    }
	    ind[m++] = i + 1; /* R is 1-based */
	}
    }
    if(m) {
	ans = allocVector(INTSXP, m);
	ians = INTEGER(ans);
	for(i = 0; i < m; i++) ians[i] = ind[i];
    }
    Free(ind);
    return ans;
}

SEXP doTabExpand(SEXP strings, SEXP starts)  /* does tab expansion for UTF-8 strings only */
{
    int i,start, bufsize = 1024;
    char *buffer = malloc(bufsize*sizeof(char)), *b;
    const char *input;
    SEXP result;
    if (!buffer) error(_("out of memory"));
    PROTECT(result = allocVector(STRSXP, length(strings)));
    for (i = 0; i < length(strings); i++) {
    	input = CHAR(STRING_ELT(strings, i));
    	start = INTEGER(starts)[i];
    	for (b = buffer; *input; ) {   
    	    /* only the first byte of multi-byte chars counts */
    	    if (0x80 <= (unsigned char)*input && (unsigned char)*input <= 0xBF)
    		start--;
    	    else if (*input == '\n')
    	    	start = (int)(buffer-b-1);
    	    if (*input == '\t') do {
    	    	*b++ = ' ';
    	    } while (((b-buffer+start) & 7) != 0);
    	    else *b++ = *input;
    	    if (b - buffer >= bufsize - 8) {
    	    	int pos = (int)(b - buffer);
		char *tmp;
    	        bufsize *= 2;
    	    	tmp = realloc(buffer, bufsize*sizeof(char));
    	    	if (!tmp) {
		    free(buffer); /* free original allocation */
		    error(_("out of memory"));
		} else buffer = tmp;
    	    	b = buffer + pos;
    	    }
    	    input++;
    	}
    	*b = '\0';
    	SET_STRING_ELT(result, i, mkCharCE(buffer, Rf_getCharCE(STRING_ELT(strings, i))));
    }
    UNPROTECT(1);
    free(buffer);
    return result;
}

/* This could be done in wchar_t, but it is only used for
   ASCII delimiters which are not lead bytes in UTF-8 or
   DBCS encodings. */
SEXP splitString(SEXP string, SEXP delims)
{
    if(!isString(string) || length(string) != 1)
	error("first arg must be a single character string");
    if(!isString(delims) || length(delims) != 1)
	error("first arg must be a single character string");
    const char *in = CHAR(STRING_ELT(string, 0)),
	*del = CHAR(STRING_ELT(delims, 0));
    cetype_t ienc = getCharCE(STRING_ELT(string, 0));
    int nc = (int) strlen(in), used = 0;

    // Used for short strings, so OK to over-allocate wildly
    SEXP out = PROTECT(allocVector(STRSXP, nc));
    const char *p;
    char tmp[nc], *this = tmp;
    int nthis = 0;
    for(p = in; *p ; p++) {
	if(strchr(del, *p)) {
	    // put out current string (if any)
	    if(nthis) 
		SET_STRING_ELT(out, used++, mkCharLenCE(tmp, nthis, ienc));
	    // put out delimiter
	    SET_STRING_ELT(out, used++, mkCharLen(p, 1));
	    // restart
	    this = tmp; nthis = 0;
	} else {
	    *this++ = *p;
	    nthis++;
	}
    }
    if(nthis) SET_STRING_ELT(out, used++, mkCharLenCE(tmp, nthis, ienc));

    SEXP ans = lengthgets(out, used);
    UNPROTECT(1);
    return ans;
}
