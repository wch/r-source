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

/*  The `` x:y ''  primitive calls do_seq(.);
 *
 *  do_seq(.) calls  cross(.) if both arguments are factors
 *	      and    seq(.)   otherwise.
 */

#include "Defn.h"
#include "Mathlib.h"

static SEXP seq(SEXP call, SEXP s1, SEXP s2)
{
	int i, n, in1;
	double n1, n2;
	SEXP ans;

	n1 = length(s1);
	n2 = length(s1);
	if(n1 > 1 || n2 > 1) {
		n = (n1 > n2) ? n1 : n2;
		warningcall(call, "Numerical expression has %d elements: only the first used\n", n);
	}
	n1 = asReal(s1);
	n2 = asReal(s2);
	if (!FINITE(n1) || !FINITE(n2))
		errorcall(call, "NA argument\n");

	if (n1 <= INT_MIN || n2 <= INT_MIN || n1 > INT_MAX || n2 > INT_MAX
	    || abs(n2 - n1) >= INT_MAX)
		errorcall(call, "argument too large in magnitude\n");

	n = abs(n2 - n1) + 1 + FLT_EPSILON;
	if (n1 == (in1 = (int)(n1))) {
		ans = allocVector(INTSXP, n);
		if (n1 <= n2)
			for (i = 0; i < n; i++) INTEGER(ans)[i] = in1 + i;
		else
			for (i = 0; i < n; i++) INTEGER(ans)[i] = in1 - i;
	} else {
		ans = allocVector(REALSXP, n);
		if (n1 <= n2)
			for (i = 0; i < n; i++) REAL(ans)[i] = n1 + i;
		else
			for (i = 0; i < n; i++) REAL(ans)[i] = n1 - i;
	}
	return ans;
}

	/*  cross  -  the "cross" of two factors  */

	/*  Note that this always produces an unordered
	 *  result.  There is no way to order.	*/

static SEXP cross(SEXP s1, SEXP s2)
{
	SEXP ans, levs, levs1, levs2;
	int i, j, k, l1, l2, n, v1, v2;
	n = length(s1);
	l1 = LEVELS(s1);
	l2 = LEVELS(s2);
	PROTECT(ans = allocVector(FACTSXP, n));
	LEVELS(ans) = l1 * l2;
	for(i=0 ; i<n ; i++) {
		v1 = INTEGER(s1)[i];
		v2 = INTEGER(s2)[i];
		if(v1 == NA_INTEGER || v2 == NA_INTEGER)
			INTEGER(ans)[i] = NA_INTEGER;
		else
			INTEGER(ans)[i] = v2 + (v1 - 1) * l2;
	}
	levs1 = getAttrib(s1, R_LevelsSymbol);
	levs2 = getAttrib(s2, R_LevelsSymbol);
	if(!isNull(levs1) && !isNull(levs2)) {
		PROTECT(levs = allocVector(STRSXP, l1 * l2));
		k = 0;
		for(i=0 ; i<l1 ; i++) {
			v1 = strlen(CHAR(STRING(levs1)[i]));
			for(j=0 ; j<l2 ; j++) {
				v2 = strlen(CHAR(STRING(levs2)[j]));
				STRING(levs)[k] = allocString(v1+v2+1);
				sprintf(CHAR(STRING(levs)[k]), "%s:%s",
					CHAR(STRING(levs1)[i]),
					CHAR(STRING(levs2)[j]));
				k++;
			}
		}
		setAttrib(ans, R_LevelsSymbol, levs);
		UNPROTECT(1);
	}
	PROTECT(levs = allocVector(STRSXP, 1));
	STRING(levs)[0] = mkChar("factor");
	setAttrib(ans, R_ClassSymbol, levs);
	UNPROTECT(2);
	return ans;
}

SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	if(isFactor(CAR(args)) && isFactor(CADR(args))) {
		if(length(CAR(args)) != length(CADR(args)))
			errorcall(call, "unequal factor lengths\n");
		return cross(CAR(args), CADR(args));
	}
	return seq(call, CAR(args), CADR(args));
}


/* It is assumed that type-checking has been done in rep */
static SEXP rep2(SEXP s, SEXP ncopy)
{
	int i, na, nc, n, j;
	SEXP a, t, u;

	t = coerceVector(ncopy, INTSXP);
	PROTECT(t);

	nc = length(ncopy);
	na = 0;
	for (i = 0; i < nc; i++) {
		if (INTEGER(t)[i] == NA_INTEGER)
			error("invalid number of copies in \"rep\"\n");
		na += INTEGER(t)[i];
	}

	if (isVector(s))
		a = allocVector(TYPEOF(s), na);
	else
		a = allocList(na);
	PROTECT(a);
	n = 0;
	switch (TYPEOF(s)) {
	case LGLSXP:
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				LOGICAL(a)[n++] = LOGICAL(s)[i];
		break;
	case FACTSXP:
	case ORDSXP:
		LEVELS(a) = LEVELS(s);
		setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				INTEGER(a)[n++] = INTEGER(s)[i];
		break;
	case INTSXP:
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				INTEGER(a)[n++] = INTEGER(s)[i];
		break;
	case REALSXP:
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				REAL(a)[n++] = REAL(s)[i];
		break;
#ifdef COMPLEX_DATA
	case CPLXSXP:
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				COMPLEX(a)[n++] = COMPLEX(s)[i];
		break;
#endif
	case STRSXP:
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++)
				STRING(a)[n++] = STRING(s)[i];
		break;
	case LISTSXP:
		u = a;
		for (i = 0; i < nc; i++)
			for (j = 0; j < (INTEGER(t)[i]); j++) {
				CAR(u) = duplicate(CAR(nthcdr(s, i)));
				u = CDR(u);
			}
		break;
	default:
		UNIMPLEMENTED("rep2");
	}
	UNPROTECT(2);
	return a;
}

SEXP rep(SEXP s, SEXP ncopy)
{
	int i, ns, na, nc;
	SEXP a, t;

	if (!isVector(ncopy))
		error("\"rep\" incorrect type for second argument\n");

	if (!isVector(s) && (!isList(s)))
		error("attempt to replicate non-vector\n");

	if ((length(ncopy) == length(s)))
		return rep2(s, ncopy);

	if ((length(ncopy) != 1))
		error("invalid number of copies in \"rep\"\n");

	if ((nc = asInteger(ncopy)) == NA_INTEGER || nc <= 0)
		error("invalid number of copies in \"rep\"\n");

	ns = length(s);
	na = nc * ns;
	if (isVector(s))
		a = allocVector(TYPEOF(s), na);
	else
		a = allocList(na);
	PROTECT(a);

	switch (TYPEOF(s)) {
	case LGLSXP:
		for (i = 0; i < na; i++)
			LOGICAL(a)[i] = LOGICAL(s)[i % ns];
		break;
	case FACTSXP:
	case ORDSXP:
		LEVELS(a) = LEVELS(s);
		setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
		for (i = 0; i < na; i++)
			INTEGER(a)[i] = INTEGER(s)[i % ns];
		break;
	case INTSXP:
		for (i = 0; i < na; i++)
			INTEGER(a)[i] = INTEGER(s)[i % ns];
		break;
	case REALSXP:
		for (i = 0; i < na; i++)
			REAL(a)[i] = REAL(s)[i % ns];
		break;
#ifdef COMPLEX_DATA
	case CPLXSXP:
		for (i = 0; i < na; i++)
			COMPLEX(a)[i] = COMPLEX(s)[i % ns];
		break;
#endif
	case STRSXP:
		for (i = 0; i < na; i++)
			STRING(a)[i] = STRING(s)[i % ns];
		break;
	case LISTSXP:
		i = 0;
		for (t = a; t != R_NilValue; t = CDR(t), i++)
			CAR(t) = duplicate(CAR(nthcdr(s, (i % ns))));
		break;
	default:
		UNIMPLEMENTED("rep");
	}
	UNPROTECT(1);
	return a;
}

SEXP do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	return rep(CAR(args), CADR(args));
}
