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
 */

#include "Defn.h"

/*-----------------------*/
/* Basic List Constructs */
/*-----------------------*/

/* cons - return a dotted pair with the given car and cdr */
SEXP cons(SEXP car, SEXP cdr)
{
	SEXP e;
	PROTECT(car);
	PROTECT(cdr);
	e = allocSExp(LISTSXP);
	UNPROTECT(2);
	CAR(e) = car;
	SETCDR(e, cdr);
#ifdef oldmem
	TAG(e) = R_NilValue;
#endif
	return e;
}

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

/* listAppend - Destructive list append */
/* See also ``append'' */
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


/*--------------------------------*/
/* Language based list constructs */
/*--------------------------------*/

/* lcons - return a (language) dotted pair with the given car and cdr */
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

static SEXP	ans;
static int	UniqueNames;
static int	IncludeFunctions;
static int	StoreValues;
static int	ItemCount;
static int	MaxCount;

static void namewalk(SEXP s)
{
	int i, j, n;
	switch(TYPEOF(s)) {
	case SYMSXP:
		if(ItemCount < MaxCount) {
			if(StoreValues) {
				if(UniqueNames) {
					for(j=0 ; j<ItemCount ; j++) {
						if(STRING(ans)[j] == PRINTNAME(s))
							goto ignore;
					}
				}
				STRING(ans)[ItemCount] = PRINTNAME(s);
			}
			ItemCount += 1;
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
	ItemCount = 0;
	namewalk(expr);
	savecount = ItemCount;

	ans = allocVector(STRSXP, ItemCount);

	StoreValues = 1;
	ItemCount = 0;
	namewalk(expr);

	if(ItemCount != savecount) {
		PROTECT(expr = ans);
		ans = allocVector(STRSXP, ItemCount);
		for(i=0 ; i<ItemCount ; i++)
			STRING(ans)[i] = STRING(expr)[i];
		UNPROTECT(1);
	}

	return ans;
}
