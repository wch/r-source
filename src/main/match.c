/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
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

#include "Defn.h"


int NonNullStringMatch(SEXP s, SEXP t)
{
    if (CHAR(s)[0] && CHAR(t)[0] && strcmp(CHAR(s), CHAR(t)) == 0)
	return 1;
    else
	return 0;
}

int psmatch(char *f, char *t, int exact)
{
    if (exact) {
	return !strcmp(f, t);
    }
    else {
	while (*f || *t) {
	    if (*t == '\0') return 1;
	    if (*f == '\0') return 0;
	    if (*t != *f)   return 0;
	    t++;
	    f++;
	}
	return 1;
    }
}

/* Matching formals and arguments */

int pmatch(SEXP formal, SEXP tag, int exact)
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
	f = CHAR(STRING(formal)[0]);
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
	t = CHAR(STRING(tag)[0]);
	break;
    default:
	goto fail;
    }
    return psmatch(f, t, exact);
 fail:
    error("invalid partial string match\n");
    return 0;/* for -Wall */
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a C string. */

SEXP matchPar(char *tag, SEXP * list)
{
    SEXP *l, s;

    for (l = list; *l != R_NilValue; l = &CDR(*l))
	if (TAG(*l) != R_NilValue && 
	    psmatch(tag, CHAR(PRINTNAME(TAG(*l))), 0)) {
	    s = *l;
	    *l = CDR(*l);
	    return CAR(s);
	}
    return R_MissingArg;
}


/* Destructively Extract A Named List Element. */
/* Returns the first partially matching tag found. */
/* Pattern is a symbol. */

SEXP matchArg(SEXP tag, SEXP * list)
{
    return matchPar(CHAR(PRINTNAME(tag)), list);
}


/* Match the supplied arguments with the formals and */
/* return the matched arguments in actuals. */

#define ARGUSED(x) LEVELS(x)


/* We need to leave supplied unchanged in case we call UseMethod */

SEXP matchArgs(SEXP formals, SEXP supplied)
{
    int i, seendots;
    SEXP f, a, b, dots, actuals;

    actuals = R_NilValue;
    for (f = formals ; f != R_NilValue ; f = CDR(f)) {
	actuals = CONS(R_MissingArg, actuals);
	MISSING(actuals) = 1;
	ARGUSED(f) = 0;
    }

    for(b = supplied; b != R_NilValue; b=CDR(b))
	ARGUSED(b) = 0;

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
			error("formal argument \"%s\" matched by multiple actual arguments\n", CHAR(PRINTNAME(TAG(f))));
		    if (ARGUSED(b) == 2)
			error("argument %d matches multiple formal arguments\n", i);
		    CAR(a) = CAR(b);
		    if(CAR(b) != R_MissingArg)
			MISSING(a) = 0;		/* not missing this arg */
		    ARGUSED(b) = 2;
		    ARGUSED(f) = 2;
		}
		i++;
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
			    error("argument %d matches multiple formal arguments\n", i);
			if (ARGUSED(f) == 1)
			    error("formal argument \"%s\" matched by multiple actual arguments\n", CHAR(PRINTNAME(TAG(f))));
			CAR(a) = CAR(b);
			if (CAR(b) != R_MissingArg)
			    MISSING(a) = 0;         /* not missing this arg */
			ARGUSED(b) = 1;
			ARGUSED(f) = 1;
		    }
		    i++;
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
	    CAR(a) = CAR(b);
	    if(CAR(b) != R_MissingArg)
		MISSING(a) = 0;
	    ARGUSED(b) = 1;
	    b = CDR(b);
	    f = CDR(f);
	    a = CDR(a);
	}
    }

    if (dots != R_NilValue) {
	/* Gobble up all unused actuals */
	MISSING(dots) = 0;
	i=0;
	for(a=supplied; a!=R_NilValue ; a=CDR(a) )
	    if(!ARGUSED(a)) i++;

	if (i) {
	    a = allocList(i);
	    TYPEOF(a) = DOTSXP;
	    f=a;
	    for(b=supplied;b!=R_NilValue;b=CDR(b))
		if(!ARGUSED(b)) {
		    CAR(f)=CAR(b);
		    TAG(f)=TAG(b);
		    f=CDR(f);
		}
	    CAR(dots)=a;
	}
    }
    else {
	/* Check that all arguments are used */
	for (b = supplied; b != R_NilValue; b = CDR(b))
	    if (!ARGUSED(b) && CAR(b) != R_MissingArg)
		errorcall(R_GlobalContext->call,
			  "unused argument to function\n");
    }
    UNPROTECT(1);
    return(actuals);
}
