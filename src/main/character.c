/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
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
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

# include <sys/types.h>

#include "Defn.h"

/* The next must come after other header files to redefine RE_DUP_MAX */
#ifdef USE_SYSTEM_REGEX
# include <regex.h>
#else
# include "Rregex.h"
#endif

#include <Print.h> /* for R_print */

#include "apse.h"

#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

#ifndef MAX
# define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

SEXP do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x, ch;
    int i, len;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	errorcall(call, "nchar() requires a character vector");
    len = LENGTH(x);
    PROTECT(s = allocVector(INTSXP, len));
    for (i = 0; i < len; i++) {
	ch = STRING_ELT(x, i);
	/* give print width for an NA_STRING */
	INTEGER(s)[i] =
	    (ch == NA_STRING) ? R_print.na_width : strlen(CHAR(ch));
    }
    if ((d = getAttrib(x, R_DimSymbol)) != R_NilValue)
	setAttrib(s, R_DimSymbol, d);
    if ((d = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	setAttrib(s, R_DimNamesSymbol, d);
    UNPROTECT(2);
    return s;
}

static char *buff=NULL;		/* Buffer for character strings */

static void AllocBuffer(int len)
{
    static int bufsize = 0;

    if(len >= 0 ) {
	if(len*sizeof(char) < bufsize) return;
	len = (len+1)*sizeof(char);
	if(len < MAXELTSIZE) len = MAXELTSIZE;
	/* Protect against broken realloc */
	if(buff) buff = (char *) realloc(buff, len);
	else buff = (char *) malloc(len);
	bufsize = len;
	if(!buff) {
	    bufsize = 0;
	    error("Could not allocate memory for substr / strsplit");
	}
    } else {
	if(bufsize == MAXELTSIZE) return;
	free(buff);
	buff = (char *) malloc(MAXELTSIZE);
	bufsize = MAXELTSIZE;
    }
}


static void substr(char *buf, char *str, int sa, int so)
{
/* Store the substring	str [sa:so]  into buf[] */
    int i;
    str += (sa - 1);
    for (i = 0; i <= (so - sa); i++)
	*buf++ = *str++;
    *buf = '\0';
}

SEXP do_substr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so;
    int i, len, start, stop, slen, k, l;
/*    char buff[MAXELTSIZE];*/

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CAR(CDDR(args));
    k = LENGTH(sa);
    l = LENGTH(so);

    if(!isString(x))
      errorcall(call, "extracting substrings from a non-character object");
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if(len > 0) {
      if (!isInteger(sa) || !isInteger(so) || k==0 || l==0)
	errorcall(call, "invalid substring argument(s) in substr()");

      for (i = 0; i < len; i++) {
	if (STRING_ELT(x,i)==NA_STRING){
	    SET_STRING_ELT(s,i,NA_STRING);
	    continue;
	}
	start = INTEGER(sa)[i % k];
	stop = INTEGER(so)[i % l];
	slen = strlen(CHAR(STRING_ELT(x, i)));
	if (start < 1)
	    start = 1;
	if (start > stop || start > slen) {
	    AllocBuffer(1);
	    buff[0]='\0';
	}
	else {
	    AllocBuffer(slen);
	    if (stop > slen)
		stop = slen;
	    substr(buff, CHAR(STRING_ELT(x, i)), start, stop);
	}
	SET_STRING_ELT(s, i, mkChar(buff));
      }
      AllocBuffer(-1);
    }
    UNPROTECT(1);
    return s;
}

static void substrset(char *buf, char *str, int sa, int so)
{
/* Replace the substring str [sa:so] in buf[] */
    int i;
    buf += (sa - 1);
    for (i = 0; i <= (so - sa); i++)
	*buf++ = *str++;
}

SEXP do_substrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so, value;
    int i, len, start, stop, slen, vlen, k, l, v;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    value = CADDDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if(!isString(x))
      errorcall(call, "replacing substrings in a non-character object");
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if(len > 0) {
      if (!isInteger(sa) || !isInteger(so) || k==0 || l==0)
	errorcall(call,"invalid substring argument(s) in substr<-()");

      v = LENGTH(value);
      if (!isString(value) || v == 0)
	errorcall(call, "invalid rhs in substr<-()");

      for (i = 0; i < len; i++) {
	if (STRING_ELT(x,i)==NA_STRING || STRING_ELT(value, i % v)==NA_STRING){
	    SET_STRING_ELT(s, i, NA_STRING);
	    continue;
	}
	start = INTEGER(sa)[i % k];
	stop = INTEGER(so)[i % l];
	slen = strlen(CHAR(STRING_ELT(x, i)));
	if (start < 1) start = 1;
	if (stop > slen) stop = slen;
	if (start > stop) {
	    AllocBuffer(0); /* since we reset later */
	    /* just copy element across */
	    SET_STRING_ELT(s, i, STRING_ELT(x, i));
	}
	else {
	    AllocBuffer(slen);
	    strcpy(buff, CHAR(STRING_ELT(x, i)));
	    vlen = strlen(CHAR(STRING_ELT(value, i % v)));
	    if(stop > start + vlen - 1) stop = start + vlen - 1;
	    substrset(buff, CHAR(STRING_ELT(value, i % v)), start, stop);
	    SET_STRING_ELT(s, i, mkChar(buff));
	}
      }
      AllocBuffer(-1);
    }
    UNPROTECT(1);
    return s;
}

/* strsplit is going to split the strings in the first argument into
 * tokens depending on the second argument. The characters of the second
 * argument are used to split the first argument.  A list of vectors is
 * returned of length equal to the input vector x, each element of the
 * list is the collection of splits for the corresponding element of x.
*/
SEXP do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tok, x;
    int i, j, len, tlen, ntok, slen;
    int extended_opt, eflags, fixed, perl;
    char *buf, *pt = NULL, *split = "", *bufp, *laststart;
    regex_t reg;
    regmatch_t regmatch[1];
    pcre *re_pcre = NULL;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL;
    int options = 0, erroffset, ovector[30];
    const char *errorptr;
    Rboolean usedRegex = FALSE, usedPCRE = FALSE;

    checkArity(op, args);
    x = CAR(args);
    tok = CADR(args);
    extended_opt = asLogical(CADDR(args));
    fixed = asLogical(CADDDR(args));
    perl = asLogical(CAD4R(args));

    if(!isString(x) || !isString(tok))
	errorcall_return(call, "non-character argument in strsplit()");
    if(extended_opt == NA_INTEGER) extended_opt = 1;
    if(perl == NA_INTEGER) perl = 0;

    eflags = 0;
    if(extended_opt) eflags = eflags | REG_EXTENDED;

    len = LENGTH(x);
    tlen = LENGTH(tok);
    /* special case split="" for efficiency */
    if(tlen == 1 && strlen(CHAR(STRING_ELT(tok, 0))) == 0) tlen = 0;

    PROTECT(s = allocVector(VECSXP, len));
    for(i = 0; i < len; i++) {
	if (STRING_ELT(x, i) == NA_STRING){
	    PROTECT(t = allocVector(STRSXP, 1));
	    SET_STRING_ELT(t, 0, NA_STRING);
	    SET_VECTOR_ELT(s, i, t);
	    UNPROTECT(1);
	    continue;
	}
	/* buffer was only used for reading
	   AllocBuffer(strlen(CHAR(STRING_ELT(x, i))));
	   strcpy(buff, CHAR(STRING_ELT(x, i))); */
	buf = CHAR(STRING_ELT(x, i));
	if(tlen > 0) {
	    /* NA token doesn't split */
	    if (STRING_ELT(tok,i % tlen) == NA_STRING){
		    PROTECT(t = allocVector(STRSXP, 1));
		    bufp = buf;
		    SET_STRING_ELT(t, 0, mkChar(bufp));
		    SET_VECTOR_ELT(s, i, t);
		    UNPROTECT(1);
		    continue;
	    }
	    /* find out how many splits there will be */
	    split = CHAR(STRING_ELT(tok, i % tlen));
	    slen = strlen(split);
	    ntok = 0;
	    if(fixed) {
		laststart = buf;
		for(bufp = buf; bufp-buf < strlen(buf); bufp++) {
		    if((slen == 1 && *bufp != *split) || 
		       (slen > 1 && strncmp(bufp, split, slen))) continue;
		    ntok++;
		    bufp += MAX(slen - 1, 0);
		    laststart = bufp+1;
		}
		bufp = laststart;
	    } else if(perl) {
		usedPCRE = TRUE;
		tables = pcre_maketables();
		re_pcre = pcre_compile(split, options, 
				       &errorptr, &erroffset, tables);
		if (!re_pcre) errorcall(call, "invalid regular expression");
		re_pe = pcre_study(re_pcre, 0, &errorptr);
		bufp = buf;
		if(*bufp != '\0') {
		    while(pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0, 
				    ovector, 30) >= 0) {
			/* Empty matches get the next char, so move by
			   one. */
			bufp += MAX(ovector[1], 1);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		}
	    } else {
		/* Careful: need to distinguish empty (rm_eo == 0) from
		   non-empty (rm_eo > 0) matches.  In the former case, the
		   token extracted is the next character.  Otherwise, it is
		   everything before the start of the match, which may be
		   the empty string (not a ``token'' in the strict sense).
		*/
		usedRegex = TRUE;
		if(regcomp(&reg, split, eflags))
		    errorcall(call, "invalid split pattern");
		bufp = buf;
		if(*bufp != '\0') {
		    while(regexec(&reg, bufp, 1, regmatch, eflags) == 0) {
			/* Empty matches get the next char, so move by
			   one. */
			bufp += MAX(regmatch[0].rm_eo, 1);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		}
	    }
	    if(*bufp == '\0')
		PROTECT(t = allocVector(STRSXP, ntok));
	    else
		PROTECT(t = allocVector(STRSXP, ntok + 1));
	    /* and fill with the splits */
	    laststart = bufp = buf;
	    pt = (char *) realloc(pt, (strlen(buf)+1) * sizeof(char));
	    for(j = 0; j < ntok; j++) {
		if(fixed) {
		    for(; bufp - buf < strlen(buf); bufp++) {
			if((slen == 1 && *bufp != *split) || 
			   (slen > 1 && strncmp(bufp, split, slen))) continue;
			if(slen) {
			    strncpy(pt, laststart, bufp - laststart);
			    pt[bufp - laststart] = '\0';
			} else {
			    pt[0] = *bufp; pt[1] ='\0';
			}
			bufp += MAX(slen-1, 0);
			laststart = bufp+1;
			SET_STRING_ELT(t, j, mkChar(pt));
			break;
		    }
		    bufp = laststart;
		} else if(perl) {
		    pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0, 
			      ovector, 30);
		    if(ovector[1] > 0) {
			/* Match was non-empty. */
			if(ovector[0] > 0)
			    strncpy(pt, bufp, ovector[0]);
			pt[ovector[0]] = '\0';
			bufp += ovector[1];
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;			
		    }
		    SET_STRING_ELT(t, j, mkChar(pt));
		} else {
		    regexec(&reg, bufp, 1, regmatch, eflags);
		    if(regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if(regmatch[0].rm_so > 0)
			    strncpy(pt, bufp, regmatch[0].rm_so);
			pt[regmatch[0].rm_so] = '\0';
			bufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;
		    }
		    SET_STRING_ELT(t, j, mkChar(pt));
		}
	    }
	    if(*bufp != '\0')
		SET_STRING_ELT(t, ntok, mkChar(bufp));
	} else {
	    /* split into individual characters */
	    char bf[2];
	    ntok = strlen(buf);
	    PROTECT(t = allocVector(STRSXP, ntok));
	    bf[1] = '\0';
	    for (j = 0; j < ntok; j++) {
		bf[0] = buf[j];
		SET_STRING_ELT(t, j, mkChar(bf));
	    }
	}
	UNPROTECT(1);
	SET_VECTOR_ELT(s, i, t);
	if(usedRegex) {
	    regfree(&reg);
	    usedRegex = FALSE;
	}
	if(usedPCRE) {
	    pcre_free(re_pe);
	    pcre_free(re_pcre);
	    pcre_free((void *)tables);
	    usedPCRE = FALSE;
	}
    }

    if (getAttrib(x, R_NamesSymbol) != R_NilValue)
	namesgets(s, getAttrib(x, R_NamesSymbol));
    UNPROTECT(1);
    /* AllocBuffer(-1); */
    free(pt);
    return s;
}


/* Abbreviate
   long names in the S-designated fashion:
   1) spaces
   2) lower case vowels
   3) lower case consonants
   4) upper case letters
   5) special characters.

   Letters are dropped from the end of words
   and at least one letter is retained from each word.

   If unique abbreviations are not produced letters are added until the
   results are unique (duplicated names are removed prior to entry).
   names, minlength, use.classes, dot
*/


#define FIRSTCHAR(i) (isspace((int)buff1[i-1]))
#define LASTCHAR(i) (!isspace((int)buff1[i-1]) && (!buff1[i+1] || isspace((int)buff1[i+1])))
#define LOWVOW(i) (buff1[i] == 'a' || buff1[i] == 'e' || buff1[i] == 'i' || \
		   buff1[i] == 'o' || buff1[i] == 'u')


/* memmove does allow overlapping src and dest */
static void mystrcpy(char *dest, const char *src)
{
    memmove(dest, src, strlen(src)+1);
}


static SEXP stripchars(SEXP inchar, int minlen)
{
/* abbreviate(inchar, minlen) */

/* This routine used strcpy with overlapping dest and src. 
   That is not allowed by ISO C. 
 */
    int i, j, nspace = 0, upper;
    char buff1[MAXELTSIZE];

    mystrcpy(buff1, CHAR(inchar));
    upper = strlen(buff1)-1;

    /* remove leading blanks */
    j = 0;
    for (i = 0 ; i < upper ; i++)
	if (isspace((int)buff1[i]))
	    j++;
	else
	    break;

    mystrcpy(buff1, &buff1[j]);
    upper = strlen(buff1) - 1;

    if (strlen(buff1) < minlen)
	goto donesc;

    for (i = upper, j = 1; i > 0; i--) {
	if (isspace((int)buff1[i])) {
	    if (j)
		buff1[i] = '\0' ;
	    else
		nspace++;
	}
	else
	    j = 0;
	/*strcpy(buff1[i],buff1[i+1]);*/
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if(LOWVOW(i) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (LOWVOW(i) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    /* all else has failed so we use brute force */

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (!FIRSTCHAR(i) && !isspace((int)buff1[i]))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

donesc:

    upper = strlen(buff1);
    if (upper > minlen)
	for (i = upper - 1; i > 0; i--)
	    if (isspace((int)buff1[i]))
		mystrcpy(&buff1[i], &buff1[i + 1]);

    return(mkChar(buff1));
}


SEXP do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    int i, len, minlen, uclass;

    checkArity(op,args);

    if (!isString(CAR(args)))
	errorcall_return(call, "the first argument must be a string");
    len = length(CAR(args));

    PROTECT(ans = allocVector(STRSXP, len));
    minlen = asInteger(CADR(args));
    uclass = asLogical(CAR(CDDR(args)));
    for (i = 0 ; i < len ; i++) {
	if (STRING_ELT(CAR(args),i)==NA_STRING)
	    SET_STRING_ELT(ans,i,NA_STRING);
	else
	    SET_STRING_ELT(ans, i, stripchars(STRING_ELT(CAR(args), i), minlen));
    }

    UNPROTECT(1);
    return(ans);
}


SEXP do_makenames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, ans;
    int i, l, n, allow_;
    char *p, *this;
    Rboolean need_prefix;

    checkArity(op ,args);
    arg = CAR(args);
    if (!isString(arg))
	errorcall(call, "non-character names");
    n = length(arg);
    allow_ = asLogical(CADR(args));
    if(allow_ == NA_LOGICAL)
	errorcall(call, "invalid allow_");
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0 ; i < n ; i++) {
	this = CHAR(STRING_ELT(arg, i));
	l = strlen(this);
	/* need to prefix names not beginning with alpha or ., as
	   well as . followed by a number */
	need_prefix = FALSE;
	if (this[0] == '.') {
	    if (l >= 1 && isdigit((int) this[1])) need_prefix = TRUE;
	} else if (!isalpha((int) this[0])) need_prefix = TRUE;
	if (need_prefix) {
	    SET_STRING_ELT(ans, i, allocString(l + 1));
	    strcpy(CHAR(STRING_ELT(ans, i)), "X");
	    strcat(CHAR(STRING_ELT(ans, i)), CHAR(STRING_ELT(arg, i)));
	    
	} else {
	    SET_STRING_ELT(ans, i, allocString(l));
	    strcpy(CHAR(STRING_ELT(ans, i)), CHAR(STRING_ELT(arg, i)));
	}
	this = p = CHAR(STRING_ELT(ans, i));
	while (*p) {
	    if (!isalnum((int)*p) && *p != '.' && (allow_ && *p != '_')) 
		*p = '.';
	    p++;
	}
	/* do we have a reserved word?  If so the name is invalid */
	if (!isValidName(this)) {
	    SET_STRING_ELT(ans, i, allocString(strlen(this) + 1));
	    strcpy(CHAR(STRING_ELT(ans, i)), this);
	    strcat(CHAR(STRING_ELT(ans, i)), ".");
	}
    }
    UNPROTECT(1);
    return ans;
}

/* This could be faster for plen > 1, but uses in R are for small strings */
static int fgrep_one(char *pat, char *target)
{
    int i = -1, plen=strlen(pat), len=strlen(target);
    char *p;

    if(plen == 0) return 0;
    if(plen == 1) {
    /* a single char is a common case */
	for(i = 0, p = target; *p; p++, i++)
	    if(*p == pat[0]) return i;
	return -1;
    }
    for(i = 0; i <= len-plen; i++)
	if(strncmp(pat, target+i, plen) == 0) return i;
    return -1;
}

SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    regex_t reg;
    int i, j, n, nmatches;
    int igcase_opt, extended_opt, value_opt, fixed_opt, eflags;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args));
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;

    if (!isString(pat) || length(pat) < 1 || !isString(vec))
	errorcall(call, R_MSG_IA);

    n = length(vec);
    nmatches = 0;
    PROTECT(ind = allocVector(LGLSXP, n));
    /* NAs are removed in R code so this isn't used */
    /* it's left in case we change our minds again */
    /* special case: NA pattern matches only NAs in vector */
    if (STRING_ELT(pat, 0) == NA_STRING){
	for(i = 0; i < n; i++){
	    if(STRING_ELT(vec, i) == NA_STRING){
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	}
	/* end NA pattern handling */
    } else {
	eflags = 0;
	if (extended_opt) eflags = eflags | REG_EXTENDED;
	if (igcase_opt) eflags = eflags | REG_ICASE;

	if (!fixed_opt && regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	    errorcall(call, "invalid regular expression");

	for (i = 0 ; i < n ; i++) {
	    LOGICAL(ind)[i] = 0;
	    if (STRING_ELT(vec, i) != NA_STRING) {
		if (fixed_opt) LOGICAL(ind)[i] = 
				   fgrep_one(CHAR(STRING_ELT(pat, 0)),
					     CHAR(STRING_ELT(vec, i))) >= 0;
		else if(regexec(&reg, CHAR(STRING_ELT(vec, i)), 0, NULL, 0) == 0)
		    LOGICAL(ind)[i] = 1;
	    }
	    if(LOGICAL(ind)[i]) nmatches++;
	}
	if (!fixed_opt) regfree(&reg);
    }

    if (value_opt) {
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
	ans = allocVector(STRSXP, nmatches);
	for (i = 0, j = 0; i < n ; i++)
	    if (LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
	/* copy across names and subset */
	if (!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if (LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
    } else {
	ans = allocVector(INTSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (LOGICAL(ind)[i]) INTEGER(ans)[j++] = i + 1;
    }
    UNPROTECT(1);
    return ans;
}

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static int length_adj(char *repl, regmatch_t *regmatch, int nsubexpr)
{
    int k, n;
    char *p = repl;
    n = strlen(repl) - (regmatch[0].rm_eo - regmatch[0].rm_so);
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		if (k > nsubexpr)
		    error("invalid backreference in regular expression");
		n += (regmatch[k].rm_eo - regmatch[k].rm_so) - 2;
		p++;
	    }
	    else if (p[1] == 0) {
				/* can't escape the final '\0' */
		n -= 1;
	    }
	    else {
		n -= 1;
		p++;
	    }
	}
	p++;
    }
    return n;
}

static char *string_adj(char *target, char *orig, char *repl,
			regmatch_t *regmatch, int nsubexpr)
{
    int i, k;
    char *p = repl, *t = target;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) {
		p += 1;
	    }
	    else {
		p += 1;
		*t++ = *p++;
	    }
	}
	else *t++ = *p++;
    }
    return t;
}


SEXP do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, vec, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, j, n, ns, nmatch, offset;
    int global, igcase_opt, extended_opt, eflags;
    char *s, *t, *u;

    checkArity(op, args);

    global = PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;

    if (length(pat) < 1 || length(rep) < 1)
	errorcall(call, R_MSG_IA);

    if (!isString(pat)) PROTECT(pat = coerceVector(pat, STRSXP));
    else PROTECT(pat);
    if (!isString(rep)) PROTECT(rep = coerceVector(rep, STRSXP));
    else PROTECT(rep);
    if (!isString(vec)) PROTECT(vec = coerceVector(vec, STRSXP));
    else PROTECT(vec);

    eflags = 0;
    if (extended_opt) eflags = eflags | REG_EXTENDED;
    if (igcase_opt) eflags = eflags | REG_ICASE;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
      /* NA `pat' are removed in R code */
      /* the C code is left in case we change our minds again */
      /* NA matches only itself */
        if (STRING_ELT(vec,i)==NA_STRING){
	    if (STRING_ELT(pat,0)==NA_STRING) 
		SET_STRING_ELT(ans, i, STRING_ELT(rep,0));
	    else
		SET_STRING_ELT(ans, i, NA_STRING);
	    continue;
	}
	if (STRING_ELT(pat, 0)==NA_STRING){
	    SET_STRING_ELT(ans, i, STRING_ELT(vec,i));
	    continue;
	}
	/* end NA handling */
	offset = 0;
	nmatch = 0;
	s = CHAR(STRING_ELT(vec, i));
	t = CHAR(STRING_ELT(rep, 0));
	ns = strlen(s);
	while (regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
	    nmatch += 1;
	    if (regmatch[0].rm_eo == 0)
		offset++;
	    else {
		ns += length_adj(t, regmatch, reg.re_nsub);
		offset += regmatch[0].rm_eo;
	    }
	    if (s[offset] == '\0' || !global)
		break;
	}
	if (nmatch == 0)
	    SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	else if (STRING_ELT(rep, 0)==NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    SET_STRING_ELT(ans, i, allocString(ns));
	    offset = 0;
	    nmatch = 0;
	    s = CHAR(STRING_ELT(vec, i));
	    t = CHAR(STRING_ELT(rep, 0));
	    u = CHAR(STRING_ELT(ans, i));
	    ns = strlen(s);
	    while (regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = s[offset+j];
		if (regmatch[0].rm_eo == 0) {
		    *u++ = s[offset];
		    offset++;
		}
		else {
		    u = string_adj(u, &s[offset], t, regmatch,
				   reg.re_nsub);
		    offset += regmatch[0].rm_eo;
		}
		if (s[offset] == '\0' || !global)
		    break;
	    }
	    if (offset < ns)
		for (j = offset ; s[j] ; j++)
		    *u++ = s[j];
	    *u = '\0';
	}
    }
    regfree(&reg);
    UNPROTECT(4);
    return ans;
}

SEXP do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchlen;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, n, st, extended_opt, fixed_opt, eflags;
    char *spat = NULL; /* -Wall */

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    fixed_opt = asLogical(CAR(args));
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;

    if (length(pat) < 1 || length(text) < 1 ||
	STRING_ELT(pat,0) == NA_STRING)
	errorcall(call, R_MSG_IA);

    if (!isString(pat)) PROTECT(pat = coerceVector(pat, STRSXP));
    else PROTECT(pat);
    if (!isString(text)) PROTECT(text = coerceVector(text, STRSXP));
    else PROTECT(text);

    eflags = extended_opt ? REG_EXTENDED : 0;

    if (!fixed_opt && regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");
    if (fixed_opt) spat = CHAR(STRING_ELT(pat, 0));
    n = length(text);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(text, i) == NA_STRING) {
	    INTEGER(matchlen)[i] = INTEGER(ans)[i] = R_NaInt;
	} else {
	    if (fixed_opt) {
		st = fgrep_one(spat, CHAR(STRING_ELT(text, i)));
		INTEGER(ans)[i] = (st > -1)?(st +1):-1;
		INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ? strlen(spat):-1;
	    } else {
		if(regexec(&reg, CHAR(STRING_ELT(text, i)), 1, 
			   regmatch, 0) == 0) {
		    st = regmatch[0].rm_so;
		    INTEGER(ans)[i] = st + 1; /* index from one */
		    INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
		} else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	    }
	}
    }
    if (!fixed_opt) regfree(&reg);
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(4);
    return ans;
}

SEXP
do_tolower(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    int i, n;
    char *p;

    checkArity(op, args);
    x = CAR(args);
    if(!isString(x))
	errorcall(call, "non-character argument to tolower()");
    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(y, i, allocString(strlen(CHAR(STRING_ELT(x, i)))));
	strcpy(CHAR(STRING_ELT(y, i)), CHAR(STRING_ELT(x, i)));
    }

    for(i = 0; i < n; i++) {
	for(p = CHAR(STRING_ELT(y, i)); *p != '\0'; p++) {
	    if (STRING_ELT(x,i)==NA_STRING) 
		SET_STRING_ELT(y,i,NA_STRING);
	    else
		*p = tolower(*p);
	}
    }
    UNPROTECT(1);
    return(y);
}

SEXP
do_toupper(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    int i, n;
    char *p;

    checkArity(op, args);
    x = CAR(args);
    if(!isString(x))
	errorcall(call, "non-character argument to toupper()");
    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(y, i, allocString(strlen(CHAR(STRING_ELT(x, i)))));
	strcpy(CHAR(STRING_ELT(y, i)), CHAR(STRING_ELT(x, i)));
    }

    for(i = 0; i < n; i++) {
	for(p = CHAR(STRING_ELT(y, i)); *p != '\0'; p++) {
	    if (STRING_ELT(x,i)==NA_STRING) 
		SET_STRING_ELT(y,i,NA_STRING);
	    else
	        *p = toupper(*p);
	}
    }
    UNPROTECT(1);
    return(y);
}

struct tr_spec {
    enum { TR_INIT, TR_CHAR, TR_RANGE } type;
    struct tr_spec *next;
    union {
	unsigned char c;
	struct {
	    unsigned char first;
	    unsigned char last;
	} r;
    } u;
};

/*
  FIXME:
  We should really check all subsequent malloc()'s for their return
  value.
  */

static void
tr_build_spec(const char *s, struct tr_spec *trs) {
    int i, len = strlen(s);
    struct tr_spec *this, *new;

    this = trs;
    for(i = 0; i < len - 2; ) {
	new = (struct tr_spec *) malloc(sizeof(struct tr_spec));
	new->next = NULL;
	if(s[i + 1] == '-') {
	    new->type = TR_RANGE;
	    if(s[i] > s[i + 2])
		error("decreasing range specification (`%c-%c')",
		      s[i], s[i + 2]);
	    new->u.r.first = s[i];
	    new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    new->type = TR_CHAR;
	    new->u.c = s[i];
	    i++;
	}
	this = this->next = new;
    }
    for( ; i < len; i++) {
	new = (struct tr_spec *) malloc(sizeof(struct tr_spec));
	new->next = NULL;
	new->type = TR_CHAR;
	new->u.c = s[i];
	this = this->next = new;
    }
}

static void
tr_free_spec(struct tr_spec *trs) {
    struct tr_spec *this, *next;
    this = trs;
    while(this) {
	next = this->next;
	free(this);
	this = next;
    }
}

static unsigned char
tr_get_next_char_from_spec(struct tr_spec **p) {
    unsigned char c;
    struct tr_spec *this;

    this = *p;
    if(!this)
	return('\0');
    switch(this->type) {
	/* Note: this code does not deal with the TR_INIT case. */
    case TR_CHAR:
	c = this->u.c;
	*p = this->next;
	break;
    case TR_RANGE:
	c = this->u.r.first;
	if(c == this->u.r.last) {
	    *p = this->next;
	} else {
	    (this->u.r.first)++;
	}
	break;
    default:
	c = '\0';
	break;
    }
    return(c);
}

SEXP
do_chartr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP old, new, x, y;
    unsigned char xtable[UCHAR_MAX + 1], *p, c_old, c_new;
    struct tr_spec *trs_old, **trs_old_ptr;
    struct tr_spec *trs_new, **trs_new_ptr;
    int i, n;

    checkArity(op, args);
    old = CAR(args); args = CDR(args);
    new = CAR(args); args = CDR(args);
    x = CAR(args);
    if(!isString(old) || (length(old) < 1) ||
       !isString(new) || (length(new) < 1) ||
       !isString(x) )
	errorcall(call, R_MSG_IA);

    if (STRING_ELT(old,0)==NA_STRING ||
	STRING_ELT(new,0)==NA_STRING){
	errorcall(call,"invalid (NA) arguments.");
    }

    for(i = 0; i <= UCHAR_MAX; i++)
	xtable[i] = i;

    /* Initialize the old and new tr_spec lists. */
    trs_old = (struct tr_spec *) malloc(sizeof(struct tr_spec));
    trs_old->type = TR_INIT;
    trs_old->next = NULL;
    trs_new = (struct tr_spec *) malloc(sizeof(struct tr_spec));
    trs_new->type = TR_INIT;
    trs_new->next = NULL;
    /* Build the old and new tr_spec lists. */
    tr_build_spec(CHAR(STRING_ELT(old, 0)), trs_old);
    tr_build_spec(CHAR(STRING_ELT(new, 0)), trs_new);
    /* Initialize the pointers for walking through the old and new
       tr_spec lists and retrieving the next chars from the lists.
       */
    trs_old_ptr = (struct tr_spec **) malloc(sizeof(struct tr_spec *));
    *trs_old_ptr = trs_old->next;
    trs_new_ptr = (struct tr_spec **) malloc(sizeof(struct tr_spec *));
    *trs_new_ptr = trs_new->next;
    for(;;) {
	c_old = tr_get_next_char_from_spec(trs_old_ptr);
	c_new = tr_get_next_char_from_spec(trs_new_ptr);
	if(c_old == '\0')
	    break;
	else if(c_new == '\0')
	    errorcall(call, "old is longer than new");
	else
	    xtable[c_old] = c_new;
    }
    /* Free the memory occupied by the tr_spec lists. */
    tr_free_spec(trs_old);
    tr_free_spec(trs_new);
    free(trs_old_ptr); free(trs_new_ptr);

    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(y, i, allocString(strlen(CHAR(STRING_ELT(x, i)))));
	strcpy(CHAR(STRING_ELT(y, i)), CHAR(STRING_ELT(x, i)));
    }

    for(i = 0; i < length(y); i++) {
	if (STRING_ELT(x,i)==NA_STRING)
	    SET_STRING_ELT(y,i,NA_STRING);
	else
	    for(p = (unsigned char *)CHAR(STRING_ELT(y, i)); *p != '\0'; p++) {
	        *p = xtable[*p];
	    }
    }

    UNPROTECT(1);
    return(y);
}

SEXP
do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches;
    int igcase_opt, value_opt, max_distance_opt;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    apse_t *aps;
    char *str;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    max_distance_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_deletions_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_insertions_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_substitutions_opt = (apse_size_t)asInteger(CAR(args));

    if(igcase_opt == NA_INTEGER) igcase_opt = 0;
    if(value_opt == NA_INTEGER) value_opt = 0;

    if(!isString(pat) || length(pat) < 1 || !isString(vec))
	errorcall(call, R_MSG_IA);

    /* NAs are removed in R code so this isn't used */
    /* it's left in case we change our minds again */
    /* special case: NA pattern matches only NAs in vector */
    if (STRING_ELT(pat,0)==NA_STRING){
	n = length(vec);
	nmatches=0;
	PROTECT(ind = allocVector(LGLSXP, n));
	for(i=0; i<n; i++){
	    if(STRING_ELT(vec,i)==NA_STRING){
		INTEGER(ind)[i]=1;
		nmatches++;
	    } 
	    else
		INTEGER(ind)[i]=0;
	}
	if (value_opt) {
	    ans = allocVector(STRSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (INTEGER(ind)[i]) {
		    SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
		}
	}
	else {
	    ans = allocVector(INTSXP, nmatches);
	    j = 0;
	    for (i = 0 ; i < n ; i++)
		if (INTEGER(ind)[i]) INTEGER(ans)[j++] = i + 1;
	}
	UNPROTECT(1);
    return ans;
    }
    /* end NA pattern handling */

    /* Create search pattern object. */
    str = CHAR(STRING_ELT(pat, 0));
    aps = apse_create((unsigned char *)str, (apse_size_t)strlen(str),
		      max_distance_opt);
    if(!aps)
	error("could not allocate memory for approximate matching");

    /* Set further restrictions on search distances. */
    apse_set_deletions(aps, max_deletions_opt);
    apse_set_insertions(aps, max_insertions_opt);
    apse_set_substitutions(aps, max_substitutions_opt);

    /* Matching. */
    n = length(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    for(i = 0 ; i < n ; i++) {
	if (STRING_ELT(vec,i)==NA_STRING){
		INTEGER(ind)[i]=0;
		continue;
	}
	str = CHAR(STRING_ELT(vec, i));
	/* Set case ignore flag for the whole string to be matched. */
	if(!apse_set_caseignore_slice(aps, 0,
				      (apse_ssize_t)strlen(str),
				      (apse_bool_t)igcase_opt)) {
	    /* Most likely, an error in apse_set_caseignore_slice()
	     * means that allocating memory failed (as we ensure that
	     * the slice is contained in the string) ... */
	    errorcall(call, "could not perform case insensitive matching");
	}
	/* Perform match. */
	if(apse_match(aps,
		      (unsigned char *)str,
		      (apse_size_t)strlen(str))) {
	    INTEGER(ind)[i] = 1;
	    nmatches++;
	}
	else INTEGER(ind)[i] = 0;
    }
    apse_destroy(aps);

    PROTECT(ans = value_opt
                ? allocVector(STRSXP, nmatches)
                : allocVector(INTSXP, nmatches));
    if(value_opt) {
	for(j = i = 0 ; i < n ; i++) {
	    if(INTEGER(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
	}
    }
    else {
	for(j = i = 0 ; i < n ; i++) {
	    if(INTEGER(ind)[i]==1)
		INTEGER(ans)[j++] = i + 1;
	}
    }

    UNPROTECT(2);
    return ans;
}

#define isRaw(x) (TYPEOF(x) == RAWSXP)

SEXP do_charToRaw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int nc;

    if(!isString(x) || LENGTH(x) == 0)
	errorcall(call, "argument must be a character vector of length 1");
    if(LENGTH(x) > 1)
	warningcall(call, "argument should be a character vector of length 1\nall but the first element will be ignored");
    nc = LENGTH(STRING_ELT(x, 0));
    ans = allocVector(RAWSXP, nc);
    memcpy(RAW(ans), CHAR(STRING_ELT(x, 0)), nc);
    return ans;
}


SEXP do_rawToChar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, c, x = CAR(args);
    int i, nc = LENGTH(x), multiple, len;
    char buf[2];

    if(!isRaw(x))
	errorcall(call, "argument 'x' must be a raw vector");
    multiple = asLogical(CADR(args));
    if(multiple == NA_LOGICAL)
	errorcall(call, "argument 'multiple' must be TRUE or FALSE");
    if(multiple) {
	buf[1] = '\0';
	PROTECT(ans = allocVector(STRSXP, nc));
	for(i = 0; i < nc; i++) {
	    buf[0] = (char) RAW(x)[i];
	    SET_STRING_ELT(ans, i, mkChar(buf));
	}
	/* do we want to copy e.g. names here? */
    } else {
	len = LENGTH(x);
	PROTECT(ans = allocVector(STRSXP, 1));
	/* String is not necessarily 0-terminated and may contain nuls 
	   so don't use mkChar */
	c = allocString(len); /* adds zero terminator */
	memcpy(CHAR(c), RAW(x), len);
	SET_STRING_ELT(ans, 0, c);
    }
    UNPROTECT(1);
    return ans;    
}


SEXP do_rawShift(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, shift = asInteger(CADR(args));
    
    if(!isRaw(x))
	errorcall(call, "argument 'x' must be a raw vector");
    if(shift == NA_INTEGER || shift < -8 || shift > 8)
	errorcall(call, "argument 'shift' must be a small integer");
    PROTECT(ans = duplicate(x));
    if (shift > 0)
	for(i = 0; i < LENGTH(x); i++)
	    RAW(ans)[i] <<= shift;
    else
	for(i = 0; i < LENGTH(x); i++)
	    RAW(ans)[i] >>= (-shift);
    UNPROTECT(1);
    return ans;    
}

SEXP do_rawToBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, j = 0, k;
    unsigned int tmp;
    
    if(!isRaw(x))
	errorcall(call, "argument 'x' must be a raw vector");
    PROTECT(ans = allocVector(RAWSXP, 8*LENGTH(x)));
    for(i = 0; i < LENGTH(x); i++) {
	tmp = (unsigned int) RAW(x)[i];
	for(k = 0; k < 8; k++, tmp >>= 1)
	    RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(1);
    return ans;    
}

SEXP do_intToBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, j = 0, k;
    unsigned int tmp;
    
    if(!isInteger(x))
	errorcall(call, "argument 'x' must be a integer vector");
    PROTECT(ans = allocVector(RAWSXP, 32*LENGTH(x)));
    for(i = 0; i < LENGTH(x); i++) {
	tmp = (unsigned int) INTEGER(x)[i];
	for(k = 0; k < 32; k++, tmp >>= 1)
	    RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(1);
    return ans;    
}

SEXP do_packBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args), stype = CADR(args);
    Rboolean useRaw;
    int i, j, k, fac, len = LENGTH(x), slen;
    unsigned int itmp;
    Rbyte btmp;
    
    if (TYPEOF(x) != RAWSXP && TYPEOF(x) != RAWSXP && TYPEOF(x) != INTSXP)
	errorcall(call, "argument 'x' must be raw, integer or logical");
    if (!isString(stype)  || LENGTH(stype) != 1)
	errorcall(call, "argument 'type' must be a character string");
    useRaw = strcmp(CHAR(STRING_ELT(stype, 0)), "integer");
    fac = useRaw ? 8 : 32;
    if (len% fac)
	errorcall(call, "argument 'x' must be a multiple of %d long", fac);
    slen = len/fac;
    PROTECT(ans = allocVector(useRaw ? RAWSXP : INTSXP, slen));
    for(i = 0; i < slen; i++)
	if(useRaw) {
	    btmp = 0;
	    for(k = 7; k >= 0; k--) {
		btmp <<= 1;
		if(isRaw(x)) 
		    btmp |= RAW(x)[8*i + k] & 0x1;
		else if(isLogical(x) || isInteger(x)) {
		    j = INTEGER(x)[8*i+k];
		    if(j == NA_INTEGER)
			errorcall(call, "argument 'x' must not contain NAs");
		    btmp |= j & 0x1;
		}
	    }
	    RAW(ans)[i] = btmp;
	} else {
	    itmp = 0;
	    for(k = 31; k >= 0; k--) {
		itmp <<= 1;
		if(isRaw(x)) 
		    itmp |= RAW(x)[32*i + k] & 0x1;
		else if(isLogical(x) || isInteger(x)) {
		    j = INTEGER(x)[32*i+k];
		    if(j == NA_INTEGER)
			errorcall(call, "argument 'x' must not contain NAs");
		    itmp |= j & 0x1;
		}
	    }
	    INTEGER(ans)[i] = (int) itmp;
	}
    UNPROTECT(1);
    return ans;    
}
