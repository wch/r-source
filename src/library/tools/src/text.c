/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-7   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> OK, provided the delimiters are ASCII
   match length is in now in chars.
*/

#include <R.h>
#include "tools.h"

#ifdef SUPPORT_MBCS
#include <wchar.h>
LibExtern Rboolean mbcslocale;
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
#endif

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
    const char *s, *s0, *delim_start, *delim_end;
    Sint n, i, pos, start, end, delim_depth;
    int lstart, lend;
    Rboolean is_escaped, equal_start_and_end_delims;
    SEXP ans, matchlen;
#ifdef SUPPORT_MBCS
    mbstate_t mb_st; int used;
#endif
    if(!isString(x) || !isString(delims) || (length(delims) != 2))
	error(_("invalid argument type"));

    delim_start = translateChar(STRING_ELT(delims, 0));
    delim_end = translateChar(STRING_ELT(delims, 1));
    lstart = strlen(delim_start); lend = strlen(delim_end);
    equal_start_and_end_delims = strcmp(delim_start, delim_end) == 0;

    n = length(x);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for(i = 0; i < n; i++) {
#ifdef SUPPORT_MBCS
	memset(&mb_st, 0, sizeof(mbstate_t));
#endif
	start = end = -1;
	s0 = s = translateChar(STRING_ELT(x, i));
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
#ifdef SUPPORT_MBCS
		    if(mbcslocale) {
			used = Rf_mbrtowc(NULL, s, MB_CUR_MAX, &mb_st);
			if(used == 0) break;
			s += used; c = *s;
		    } else
#endif
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
#ifdef SUPPORT_MBCS
	    if(mbcslocale) {
		used = Rf_mbrtowc(NULL, s, MB_CUR_MAX, &mb_st);
		if(used == 0) break;
		s += used;
	    } else
#endif
		s++;
	    pos++;
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
	p = CHAR(STRING_ELT(text, i)); /* ASCII or not not affected by charset */
	inquote =FALSE; /* avoid runaway quotes */
	for(; *p; p++) {
	    if(!inquote && *p == '#') break;
	    if(!inquote || ign) {
		if((unsigned int) *p > 127) {
		    /* Rprintf("%s\n", CHAR(STRING_ELT(text, i)));
		       Rprintf("found %x\n", (unsigned int) *p); */
		    return ScalarLogical(TRUE);
		}
	    }
	    if(nbslash % 2 && (*p == '"' || *p == '\'')) {
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
