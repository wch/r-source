/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001        The R Development Core Team
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
 *
 *
 *  Basic List Handling Features
 *
 *  These remain here to show that R is truly descended from Lisp :-).
 *  There is one real function "allnames" shich should probably be
 *  elsewhere.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"


/* Return a dotted pair with the given CAR and CDR. */
/* The (R) TAG slot on the cell is set to NULL. */


/* Get the i-th element of a list */
SEXP elt(SEXP list, int i)
{
    int j;
    SEXP result = list;

    if ((i < 0) || (i > length(list)))
	return R_NilValue;
    else
	for (j = 0; j < i; j++)
	    result = CDR(result);

    return CAR(result);
}


/* Return the last element of a list */
SEXP lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue) {
	result = list;
	list = CDR(list);
    }
    return result;
}


/* Shorthands for creating small lists */

SEXP list1(SEXP s)
{
    return CONS(s, R_NilValue);
}


SEXP list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, list1(t));
    UNPROTECT(1);
    return s;
}


SEXP list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = CONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}


SEXP list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = CONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}


/* Destructive list append : See also ``append'' */

SEXP listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
	return t;
    r = s;
    while (CDR(r) != R_NilValue)
	r = CDR(r);
    SETCDR(r, t);
    return s;
}


/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/* Return a (language) dotted pair with the given car and cdr */

SEXP lcons(SEXP car, SEXP cdr)
{
    SEXP e = cons(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

SEXP lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

SEXP lang2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = LCONS(s, list1(t));
    UNPROTECT(1);
    return s;
}

SEXP lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = LCONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}

SEXP lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = LCONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}


/* The following code is used to recursive traverse a block */
/* of code and extract all the symbols present in that code. */

typedef struct {
 SEXP	ans;
 int	UniqueNames;
 int	IncludeFunctions;
 int	StoreValues;
 int	ItemCounts;
 int	MaxCount;
} NameWalkData;

static void namewalk(SEXP s, NameWalkData *d)
{
    int i, j, n;
    SEXP name;

    switch(TYPEOF(s)) {
    case SYMSXP:
	name = PRINTNAME(s);
	/* skip blank symbols */
	if(strlen(CHAR(name)) == 0) goto ignore;
	if(d->ItemCounts < d->MaxCount) {
	    if(d->StoreValues) {
		if(d->UniqueNames) {
		    for(j = 0 ; j < d->ItemCounts ; j++) {
			if(STRING_ELT(d->ans, j) == name)
			    goto ignore;
		    }
		}
		SET_STRING_ELT(d->ans, d->ItemCounts, name);
	    }
	    d->ItemCounts += 1;
	}
    ignore:
	break;
    case LANGSXP:
	if(!d->IncludeFunctions) s = CDR(s);
	while(s != R_NilValue) {
	    namewalk(CAR(s), d);
	    s = CDR(s);
	}
	break;
    case EXPRSXP:
	n = length(s);
	for(i=0 ; i<n ; i++)
	    namewalk(VECTOR_ELT(s, i), d);
	break;
    }
}

/* Also does all.vars wiht functions=FALSE
   .Internal(all.names(expr, functions, max.names, unique)) */
SEXP do_allnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP expr;
    int i, savecount;
    NameWalkData data = {NULL, 0, 0, 0, 0, 0};

    checkArity(op, args);

    expr = CAR(args);
    args = CDR(args);

    data.IncludeFunctions = asLogical(CAR(args));
    if(data.IncludeFunctions == NA_LOGICAL)
	data.IncludeFunctions = 0;
    args = CDR(args);

    data.MaxCount = asInteger(CAR(args));
    if(data.MaxCount < 0 || data.MaxCount == NA_INTEGER)
	data.MaxCount = 0;
    args = CDR(args);

    data.UniqueNames = asLogical(CAR(args));
    if(data.UniqueNames == NA_LOGICAL)
	data.UniqueNames = 1;

    namewalk(expr, &data);
    savecount = data.ItemCounts;

    data.ans = allocVector(STRSXP, data.ItemCounts);

    data.StoreValues = 1;
    data.ItemCounts = 0;
    namewalk(expr, &data);

    if(data.ItemCounts != savecount) {
	PROTECT(expr = data.ans);
	data.ans = allocVector(STRSXP, data.ItemCounts);
	for(i = 0 ; i < data.ItemCounts ; i++)
	    SET_STRING_ELT(data.ans, i, STRING_ELT(expr, i));
	UNPROTECT(1);
    }

    return data.ans;
}
