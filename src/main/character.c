/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2001  Robert Gentleman, Ross Ihaka and the
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
#include <config.h>
#endif

#ifndef Macintosh
#include <sys/types.h>
#else 
#include <types.h>
#endif

#include "Defn.h"
/* The next must come after other header files to redefine RE_DUP_MAX */
#ifdef USE_SYSTEM_REGEX
#include <regex.h>
#else
#include "Rregex.h"
#endif

#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

SEXP do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x;
    int i, len;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	errorcall(call, "nchar() requires a character vector");
    len = LENGTH(x);
    PROTECT(s = allocVector(INTSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(s)[i] = strlen(CHAR(STRING_ELT(x, i)));
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
	buff = (char *) realloc(buff, len);
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

    if (!isString(x) || !isInteger(sa) || !isInteger(so) || k==0 || l==0)
	errorcall(call,"invalid argument in substr()");

    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    for (i = 0; i < len; i++) {
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
	    /* if (stop > MAXELTSIZE) {
		stop = MAXELTSIZE;
		warningcall(call, "a string was truncated in substr()");
		}*/
	    substr(buff, CHAR(STRING_ELT(x, i)), start, stop);
	}
	SET_STRING_ELT(s, i, mkChar(buff));
    }
    UNPROTECT(1);
    AllocBuffer(-1);
    return s;
}


/* strsplit is going to split the strings in the first argument into */
/* tokens depending on the second argument. The characters of the second */
/* argument are used to split the first argument.  A list of vectors is */
/* returned of length equal to the input vector x, each element of the */
/* list is the collection of splits for the corresponding element of x. */

SEXP do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tok, x;
    int i, j, len, tlen, ntok;
    int extended_opt, eflags;
    char *pt = NULL, *split = "", *bufp;
    regex_t reg;
    regmatch_t regmatch[1];

    checkArity(op, args);
    x = CAR(args);
    tok = CADR(args);
    extended_opt = asLogical(CADDR(args));

    if(!isString(x) || !isString(tok))
	errorcall_return(call,"non-character argument in strsplit()");
    if(extended_opt == NA_INTEGER) extended_opt = 1;    
    
    eflags = 0;
    if(extended_opt) eflags = eflags | REG_EXTENDED;    

    len = LENGTH(x);
    tlen = LENGTH(tok);
    PROTECT(s = allocVector(VECSXP, len));
    for(i = 0; i < len; i++) {
	AllocBuffer(strlen(CHAR(STRING_ELT(x, i))));
	strcpy(buff, CHAR(STRING_ELT(x, i)));
	if(tlen > 0) {
	    /* find out how many splits there will be */
	    split = CHAR(STRING_ELT(tok, i % tlen));
	    ntok = 0;
	    /* Careful: need to distinguish empty (rm_eo == 0) from
	       non-empty (rm_eo > 0) matches.  In the former case, the
	       token extracted is the next character.  Otherwise, it is
	       everything before the start of the match, which may be
	       the empty string (not a ``token'' in the strict sense).
	       */
	    if(regcomp(&reg, split, eflags))
		errorcall(call, "invalid split pattern");
	    bufp = buff;
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
	    if(*bufp == '\0')
		PROTECT(t = allocVector(STRSXP, ntok));
	    else
		PROTECT(t = allocVector(STRSXP, ntok + 1));
	    /* and fill with the splits */
	    bufp = buff;
	    pt = (char *) realloc(pt, (strlen(buff)+1) * sizeof(char));
	    for(j = 0; j < ntok; j++) {
		regexec(&reg, bufp, 1, regmatch, eflags);
		if(regmatch[0].rm_eo > 0) {
		    /* Match was non-empty. */
		    if(regmatch[0].rm_so > 0)
			strncpy(pt, bufp, regmatch[0].rm_so);
		    pt[regmatch[0].rm_so] = '\0';
		    bufp += regmatch[0].rm_eo;
		}
		else {
		    /* Match was empty. */
		    pt[0] = *bufp;
		    pt[1] = '\0';
		    bufp++;
		}
		SET_STRING_ELT(t, j, mkChar(pt));
	    }
	    if(*bufp != '\0')
		SET_STRING_ELT(t, ntok, mkChar(bufp));
	    regfree(&reg);
	}
	else {
	    char bf[2];
	    ntok = strlen(buff);
	    PROTECT(t = allocVector(STRSXP, ntok));
	    bf[1]='\0';
	    for (j = 0; j < ntok; j++) {
		bf[0]=buff[j];
		SET_STRING_ELT(t, j, mkChar(bf));
	    }
	}
	UNPROTECT(1);
	SET_VECTOR_ELT(s, i, t);
    }
    UNPROTECT(1);
    AllocBuffer(-1);
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

static SEXP stripchars(SEXP inchar, int minlen)
{
/* abbreviate(inchar, minlen) */

    int i, j, nspace = 0, upper;
    char buff1[MAXELTSIZE];

    strcpy(buff1, CHAR(inchar));
    upper = strlen(buff1)-1;

    /* remove leading blanks */
    j = 0;
    for (i = 0 ; i < upper ; i++)
	if (isspace((int)buff1[i]))
	    j++;
	else
	    break;

    strcpy(buff1, &buff1[j]);
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
	    strcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (LOWVOW(i) && !FIRSTCHAR(i))
	    strcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && LASTCHAR(i))
	    strcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && !FIRSTCHAR(i))
	    strcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    /* all else has failed so we use brute force */

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (!FIRSTCHAR(i) && !isspace((int)buff1[i]))
	    strcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

donesc:

    upper = strlen(buff1);
    if (upper > minlen)
	for (i = upper - 1; i > 0; i--)
	    if (isspace((int)buff1[i]))
		strcpy(&buff1[i], &buff1[i + 1]);

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
    for (i = 0 ; i < len ; i++)
	SET_STRING_ELT(ans, i, stripchars(STRING_ELT(CAR(args), i), minlen));

    UNPROTECT(1);
    return(ans);
}


SEXP do_makenames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, ans;
    int i, l, n;
    char *p;

    checkArity(op ,args);
    arg = CAR(args);
    if (!isString(arg))
	errorcall(call, "non-character names");
    n = length(arg);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0 ; i < n ; i++) {
	l = strlen(CHAR(STRING_ELT(arg, i)));
	if (isalpha((int)CHAR(STRING_ELT(arg, i))[0])) {
	    SET_STRING_ELT(ans, i, allocString(l));
	    strcpy(CHAR(STRING_ELT(ans, i)), CHAR(STRING_ELT(arg, i)));
	}
	else {
	    SET_STRING_ELT(ans, i, allocString(l + 1));
	    strcpy(CHAR(STRING_ELT(ans, i)), "X");
	    strcat(CHAR(STRING_ELT(ans, i)), CHAR(STRING_ELT(arg, i)));
	}
	p = CHAR(STRING_ELT(ans, i));
	while (*p) {
	    if (!isalnum((int)*p) && *p != '.')
		*p = '.';
	    p++;
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    regex_t reg;
    int i, j, n, nmatches;
    int igcase_opt, extended_opt, value_opt, eflags;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (value_opt == NA_INTEGER) value_opt = 0;

    if (!isString(pat) || length(pat) < 1 || !isString(vec))
	errorcall(call, R_MSG_IA);

    eflags = 0;

    if (extended_opt) eflags = eflags | REG_EXTENDED;
    if (igcase_opt) eflags = eflags | REG_ICASE;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");

    n = length(vec);
    ind = allocVector(LGLSXP, n);
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
	if (regexec(&reg, CHAR(STRING_ELT(vec, i)), 0, NULL, 0) == 0) {
	    INTEGER(ind)[i] = 1;
	    nmatches++;
	}
	else INTEGER(ind)[i] = 0;
    }
    regfree(&reg);
    PROTECT(ind);
    if (value_opt) {
	ans = allocVector(STRSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (INTEGER(ind)[i]) {
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
		/* FIXME: Want to inherit 'names(vec)': [the following is wrong]
		   TAG	 (ans)[j]   = TAG(vec)[i]; */
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

    if (!isString(pat) || length(pat) < 1 ||
	!isString(rep) || length(rep) < 1 ||
	!isString(vec))
	errorcall(call, R_MSG_IA);

    eflags = 0;
    if (extended_opt) eflags = eflags | REG_EXTENDED;
    if (igcase_opt) eflags = eflags | REG_ICASE;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
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
	    for (j = offset ; s[j] ; j++)
		*u++ = s[j];
	    *u = '\0';
	}
    }
    regfree(&reg);
    UNPROTECT(1);
    return ans;
}

SEXP do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchlen;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, n, st, extended_opt, eflags;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    extended_opt = asLogical(CAR(args));
    if (extended_opt == NA_INTEGER) extended_opt = 1;

    if (!isString(pat) || length(pat) < 1 ||
	!isString(text) || length(text) < 1 )
	errorcall(call, R_MSG_IA);

    eflags = extended_opt ? REG_EXTENDED : 0;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");
    n = length(text);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	if(regexec(&reg, CHAR(STRING_ELT(text, i)), 1, regmatch, 0) == 0) {
	    st = regmatch[0].rm_so;
	    INTEGER(ans)[i] = st + 1; /* index from one */
	    INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
	} else {
	    INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	}
    }
    regfree(&reg);
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
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
       !isString(x))
	errorcall(call, R_MSG_IA);

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
    /* Initilize the pointers for walking through the old and new
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

    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	SET_STRING_ELT(y, i, allocString(strlen(CHAR(STRING_ELT(x, i)))));
	strcpy(CHAR(STRING_ELT(y, i)), CHAR(STRING_ELT(x, i)));
    }

    for(i = 0; i < length(y); i++) {
	for(p = (unsigned char *)CHAR(STRING_ELT(y, i)); *p != '\0'; p++) {
	    *p = xtable[*p];
	}
    }

    UNPROTECT(1);
    return(y);
}
