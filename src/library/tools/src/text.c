#include <R.h>
#include "tools.h"

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

    char c, *s, delim_start, delim_end;
    Sint n, i, pos, start, end, delim_depth;
    Rboolean is_escaped, equal_start_and_end_delims;
    SEXP ans, matchlen;

    if(!isString(x) || !isString(delims) || (length(delims) != 2))
	error("invalid argument type");

    delim_start = CHAR(STRING_ELT(delims, 0))[0];
    delim_end = CHAR(STRING_ELT(delims, 1))[0];
    equal_start_and_end_delims = (delim_start == delim_end);

    n = length(x);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for(i = 0; i < n; i++) {
	start = end = -1;
	s = CHAR(STRING_ELT(x, i));
	if(*s == '\0') continue;
	pos = is_escaped = delim_depth = 0;
	while((c = *s++) != '\0') {
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
		    c = *s++; pos++;
		}
	    }
	    else if(c == delim_end) {
		if(delim_depth > 1) delim_depth--;
		else if(delim_depth == 1) {
		    end = pos;
		    continue;
		}
		else if(equal_start_and_end_delims) {
		    start = pos;
		    delim_depth++;
		}
	    }
	    else if(c == delim_start) {
		if(delim_depth == 0) start = pos;
		delim_depth++;
	    }
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
