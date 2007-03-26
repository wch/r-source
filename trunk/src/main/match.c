/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA


 *  Matching and Partial Matching for Strings
 *
 *  In theory all string matching code should be placed in this file
 *  At present there are still a couple of rogue matchers about.
 *
 *
 *  psmatch(char *, char *, int);
 *
 *  This code will perform partial matching for list tags.  When
 *  exact is 1, and exact match is required (typically after ...)
 *  otherwise partial matching is performed.
 *
 *  Examples:
 *
 *	psmatch("aaa", "aaa", 0) -> 1
 *	psmatch("aaa", "aa", 0) -> 1
 *	psmatch("aa", "aaa", 0) -> 0
 *
 */

/* <UTF8> char here is either ASCII or handled as a whole
   or as an initial segment (which is OK if entities are valid)
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"


/* used in subscript.c and subassign.c */
Rboolean NonNullStringMatch(SEXP s, SEXP t)
{
    /* "" or NA string matches nothing */
    if (s == NA_STRING || t == NA_STRING) return FALSE;
    if (CHAR(s)[0] && CHAR(t)[0] && 
	strcmp(translateChar(s), translateChar(t)) == 0)
	return TRUE;
    else
	return FALSE;
}

/* currently unused outside this file */
Rboolean psmatch(char *f, char *t, Rboolean exact)
{
    if (exact)
	return (Rboolean)!strcmp(f, t);
    /* else */
    while (*f || *t) {
	if (*t == '\0') return TRUE;
	if (*f == '\0') return FALSE;
	if (*t != *f)   return FALSE;
	t++;
	f++;
    }
    return TRUE;
}


/* Matching formals and arguments */

Rboolean pmatch(SEXP formal, SEXP tag, Rboolean exact)
{
    char *f, *t;
    switch (TYPEOF(formal)) {
    case SYMSXP:
	f = CHAR(PRINTNAME(formal));
	break;
    case CHARSXP:
	f = CHAR(formal);
	break;
    case STRSXP:
	f = translateChar(STRING_ELT(formal, 0));
	break;
    default:
	goto fail;
    }
    switch(TYPEOF(tag)) {
    case SYMSXP:
	t = CHAR(PRINTNAME(tag));
	break;
    case CHARSXP:
	t = CHAR(tag);
	break;
    case STRSXP:
	t = translateChar(STRING_ELT(tag, 0));
	break;
    default:
	goto fail;
    }
    return psmatch(f, t, exact);
 fail:
    error(_("invalid partial string match"));
    return FALSE;/* for -Wall */
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a C string. */

static SEXP matchPar_int(char *tag, SEXP *list, Rboolean exact)
{
    if (*list == R_NilValue)
	return R_MissingArg;
    else if (TAG(*list) != R_NilValue &&
	     psmatch(tag, CHAR(PRINTNAME(TAG(*list))), exact)) {
	SEXP s = *list;
	*list = CDR(*list);
	return CAR(s);
    }
    else {
	SEXP last = *list;
	SEXP next = CDR(*list);
	while (next != R_NilValue) {
	    if (TAG(next) != R_NilValue &&
		psmatch(tag, CHAR(PRINTNAME(TAG(next))), exact)) {
		SETCDR(last, CDR(next));
		return CAR(next);
	    }
	    else {
		last = next;
		next = CDR(next);
	    }
	}
	return R_MissingArg;
    }
}

/* unused outside this file */
SEXP attribute_hidden matchPar(char *tag, SEXP * list)
{
    return matchPar_int(tag, list, FALSE);
}



/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArg(SEXP tag, SEXP * list)
{
    return matchPar(CHAR(PRINTNAME(tag)), list);
}


/* Destructively Extract A Named List Element. */
/* Returns the first exactly matching tag found. */
/* Pattern is a symbol. */

SEXP attribute_hidden matchArgExact(SEXP tag, SEXP * list)
{
      return matchPar_int(CHAR(PRINTNAME(tag)), list, TRUE);  
}


/* Match the supplied arguments with the formals and */
/* return the matched arguments in actuals. */

#define ARGUSED(x) LEVELS(x)
#define SET_ARGUSED(x,v) SETLEVELS(x,v)


/* We need to leave 'supplied' unchanged in case we call UseMethod */
/* MULTIPLE_MATCHES was added by RI in Jan 2005 but never activated */

SEXP attribute_hidden matchArgs(SEXP formals, SEXP supplied)
{
    int i, seendots;
    SEXP f, a, b, dots, actuals;
#ifdef MULTIPLE_MATCHES
    int havedots = 0;
#endif

    actuals = R_NilValue;
    for (f = formals ; f != R_NilValue ; f = CDR(f)) {
#ifdef MULTIPLE_MATCHES
	if (TAG(f) ==  R_DotsSymbol) havedots = 1;
#endif
	actuals = CONS(R_MissingArg, actuals);
	SET_MISSING(actuals, 1);
	SET_ARGUSED(f, 0);
    }

    for(b = supplied; b != R_NilValue; b=CDR(b))
	SET_ARGUSED(b, 0);

    PROTECT(actuals);

    /* First pass: exact matches by tag */
    /* Grab matched arguments and check */
    /* for multiple exact matches. */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (TAG(f) != R_DotsSymbol) {
	    i = 1;
	    for (b = supplied; b != R_NilValue; b = CDR(b)) {
		if (TAG(b) != R_NilValue && pmatch(TAG(f), TAG(b), 1)) {
		    if (ARGUSED(f) == 2)
#ifdef MULTIPLE_MATCHES
{
			if (havedots) goto nextarg1;
#endif
			error(_("formal argument \"%s\" matched by multiple actual arguments"),
			      CHAR(PRINTNAME(TAG(f))));
#ifdef MULTIPLE_MATCHES
		    }
#endif
		    if (ARGUSED(b) == 2)
			error(_("argument %d matches multiple formal arguments"), i);
		    SETCAR(a, CAR(b));
		    if(CAR(b) != R_MissingArg)
			SET_MISSING(a, 0);	/* not missing this arg */
		    SET_ARGUSED(b, 2);
		    SET_ARGUSED(f, 2);
		}
		i++;
#ifdef MULTIPLE_MATCHES
nextarg1:
		;
#endif
	    }
	}
	f = CDR(f);
	a = CDR(a);
    }

    /* Second pass: partial matches based on tags */
    /* An exact match is required after first ... */
    /* The location of the first ... is saved in "dots" */

    dots = R_NilValue;
    seendots = 0;
    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (ARGUSED(f) == 0) {
	    if (TAG(f) == R_DotsSymbol && !seendots) {
		/* Record where ... value goes */
		dots = a;
		seendots = 1;
	    }
	    else {
		i = 1;
		for (b = supplied; b != R_NilValue; b = CDR(b)) {
		    if (ARGUSED(b) != 2 && TAG(b) != R_NilValue &&
			pmatch(TAG(f), TAG(b), seendots)) {
			if (ARGUSED(b))
			    error(_("argument %d matches multiple formal arguments"), i);
			if (ARGUSED(f) == 1)
#ifdef MULTIPLE_MATCHES
			{
			    if (havedots) goto nextarg2;
#endif
			    error(_("formal argument \"%s\" matched by multiple actual arguments"),
				  CHAR(PRINTNAME(TAG(f))));
#ifdef MULTIPLE_MATCHES
			}
#endif
			SETCAR(a, CAR(b));
			if (CAR(b) != R_MissingArg)
			    SET_MISSING(a, 0);       /* not missing this arg */
			SET_ARGUSED(b, 1);
			SET_ARGUSED(f, 1);
		    }
		    i++;
#ifdef MULTIPLE_MATCHES
nextarg2:
		    ;
#endif
		}
	    }
	}
	f = CDR(f);
	a = CDR(a);
    }

    /* Third pass: matches based on order */
    /* All args specified in tag=value form */
    /* have now been matched.  If we find ... */
    /* we gobble up all the remaining args. */
    /* Otherwise we bind untagged values in */
    /* order to any unmatched formals. */

    f = formals;
    a = actuals;
    b = supplied;
    seendots = 0;

    while (f != R_NilValue && b != R_NilValue && !seendots) {
	if (TAG(f) == R_DotsSymbol) {
	    /* Skip ... matching until all tags done */
	    seendots = 1;
	    f = CDR(f);
	    a = CDR(a);
	}
	else if (CAR(a) != R_MissingArg) {
	    /* Already matched by tag */
	    /* skip to next formal */
	    f = CDR(f);
	    a = CDR(a);
	}
	else if (ARGUSED(b) || TAG(b) != R_NilValue) {
	    /* This value used or tagged , skip to next value */
	    /* The second test above is needed because we */
	    /* shouldn't consider tagged values for positional */
	    /* matches. */
	    /* The formal being considered remains the same */
	    b = CDR(b);
	}
	else {
	    /* We have a positional match */
	    SETCAR(a, CAR(b));
	    if(CAR(b) != R_MissingArg)
		SET_MISSING(a, 0);
	    SET_ARGUSED(b, 1);
	    b = CDR(b);
	    f = CDR(f);
	    a = CDR(a);
	}
    }

    if (dots != R_NilValue) {
	/* Gobble up all unused actuals */
	SET_MISSING(dots, 0);
	i=0;
	for(a=supplied; a!=R_NilValue ; a=CDR(a) )
	    if(!ARGUSED(a)) i++;

	if (i) {
	    a = allocList(i);
	    SET_TYPEOF(a, DOTSXP);
	    f=a;
	    for(b=supplied;b!=R_NilValue;b=CDR(b))
		if(!ARGUSED(b)) {
		    SETCAR(f, CAR(b));
		    SET_TAG(f, TAG(b));
		    f=CDR(f);
		}
	    SETCAR(dots, a);
	}
    }
    else {
	/* Check that all arguments are used */
	SEXP unused = R_NilValue, last = R_NilValue;
	for (b = supplied; b != R_NilValue; b = CDR(b))
	    /* Uncomment to allow unmatched empty args, as done < 2.4.0 */
	    if (!ARGUSED(b)/* && CAR(b) != R_MissingArg) */) {
		if(last == R_NilValue) {
		    PROTECT(unused = CONS(CAR(b), R_NilValue));
		    SET_TAG(unused, TAG(b));
		    last = unused;
		} else {
		    SETCDR(last, CONS(CAR(b), R_NilValue));
		    last = CDR(last);
		    SET_TAG(last, TAG(b));
		}
	    }

	if(last != R_NilValue) {
	    errorcall(R_GlobalContext->call,
		      _("unused argument(s) %s"), 
		      CHAR(STRING_ELT(deparse1line(unused, 0), 0)) + 4);
	}
    }
    UNPROTECT(1);
    return(actuals);
}
