/*
 *  R : A Computer Language for Statistical Data Analysis
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

SEXP cons(SEXP car, SEXP cdr)
{
    SEXP e;
    PROTECT(car);
    PROTECT(cdr);
    e = allocSExp(LISTSXP);
    UNPROTECT(2);
    CAR(e) = car;
    SETCDR(e, cdr);
    return e;
}

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
    SEXP e;
    PROTECT(car);
    PROTECT(cdr);
    e = allocSExp(LANGSXP);
    UNPROTECT(2);
    CAR(e) = car;
    SETCDR(e, cdr);
    TAG(e) = R_NilValue;
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

static SEXP	ans;
static int	UniqueNames;
static int	IncludeFunctions;
static int	StoreValues;
static int	ItemCounts;
static int	MaxCount;

static void namewalk(SEXP s)
{
    int i, j, n;
    switch(TYPEOF(s)) {
    case SYMSXP:
	if(ItemCounts < MaxCount) {
	    if(StoreValues) {
		if(UniqueNames) {
		    for(j=0 ; j<ItemCounts ; j++) {
			if(STRING(ans)[j] == PRINTNAME(s))
			    goto ignore;
		    }
		}
		STRING(ans)[ItemCounts] = PRINTNAME(s);
	    }
	    ItemCounts += 1;
	}
    ignore:
	break;
    case LANGSXP:
	if(!IncludeFunctions) s = CDR(s);
	while(s != R_NilValue) {
	    namewalk(CAR(s));
	    s = CDR(s);
	}
	break;
    case EXPRSXP:
	n = length(s);
	for(i=0 ; i<n ; i++)
	    namewalk(VECTOR(s)[i]);
	break;
    }
}

SEXP do_allnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP expr;
    int i, savecount;

    checkArity(op, args);

    expr = CAR(args);
    args = CDR(args);

    IncludeFunctions = asLogical(CAR(args));
    if(IncludeFunctions == NA_LOGICAL)
	IncludeFunctions = 0;
    args = CDR(args);

    MaxCount = asInteger(CAR(args));
    if(MaxCount < 0 || MaxCount == NA_INTEGER)
	MaxCount = 0;
    args = CDR(args);

    UniqueNames = asLogical(CAR(args));
    if(UniqueNames == NA_LOGICAL)
	UniqueNames = 1;

    StoreValues = 0;
    ItemCounts = 0;
    namewalk(expr);
    savecount = ItemCounts;

    ans = allocVector(STRSXP, ItemCounts);

    StoreValues = 1;
    ItemCounts = 0;
    namewalk(expr);

    if(ItemCounts != savecount) {
	PROTECT(expr = ans);
	ans = allocVector(STRSXP, ItemCounts);
	for(i=0 ; i<ItemCounts ; i++)
	    STRING(ans)[i] = STRING(expr)[i];
	UNPROTECT(1);
    }

    return ans;
}
