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
#include "Mathlib.h"
#include "Print.h"

static SEXP coerceToLogical(SEXP v);
static SEXP coerceToFactor(SEXP v);
static SEXP coerceToOrdered(SEXP v);
static SEXP coerceToInteger(SEXP v);
static SEXP coerceToReal(SEXP v);
static SEXP coerceToComplex(SEXP v);
static SEXP coerceToString(SEXP v);
static SEXP coerceToExpression(SEXP v);
static SEXP coerceToList(SEXP v);

SEXP do_codes(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, y, s;
	int i, n;

	checkArity(op, args);
	x = CAR(args);
	if(!isFactor(x))
		errorcall(call, "not a factor\n");
	n = LENGTH(x);
	PROTECT(y = allocVector(INTSXP, n));
	for(i=0 ; i<n ; i++)
		INTEGER(y)[i] = INTEGER(x)[i];
	if((s=getAttrib(x, R_DimSymbol)) != R_NilValue) {
		setAttrib(y, R_DimSymbol, s);
		setAttrib(y, R_DimNamesSymbol, getAttrib(x,R_DimNamesSymbol));
	}
	else {
		setAttrib(y, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
	UNPROTECT(1);
	return y;
}

SEXP do_codesgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, y;
	int i, iy, lx, nx, ny;

	checkArity(op, args);
	x = CAR(args);
	y = CADR(args);
	if(!isFactor(x)) errorcall(call, "lhs not a factor\n");
	if(!isNumeric(y)) errorcall(call, "rhs not numeric\n");
	nx = LENGTH(x);
	ny = LENGTH(x);
	lx = LEVELS(x);
	if(nx != ny) errorcall(call, "unequal lengths\n");
	for(i=0 ; i<nx ; i++) {
		switch(TYPEOF(y)) {
			case INTSXP:
				iy = INTEGER(y)[i];
				break;
			case REALSXP:
				if(FINITE(REAL(y)[i])) iy = REAL(y)[i] + 0.5;
				else iy = NA_INTEGER;
				break;
		}
		if(iy == NA_INTEGER || (1 <= iy && iy <= lx))
			INTEGER(x)[i] = iy;
		else
			errorcall(call, "invalid factor level\n");
	}
	NAMED(x) = 0;
	return x;
}

SEXP CreateTag(SEXP x)
{
	if(isNull(x) || isSymbol(x))
		return x;

	if(isString(x)
	  && length(x) >= 1
	  && length(STRING(x)[0]) >= 1)
		x = install(CHAR(STRING(x)[0]));
	else
		x = install(CHAR(STRING(deparse1(x, 1))[0]));
	return x;
}

static SEXP asFunction(SEXP x)
{
	SEXP f, pf;
	int n;

	if(isFunction(x)) return x;

	PROTECT(f = allocSExp(CLOSXP));
	CLOENV(f) = R_GlobalEnv;

	if(NAMED(x)) PROTECT(x = duplicate(x));
	else PROTECT(x);

	if(isNull(x) || !isList(x)) {
		FORMALS(f) = R_NilValue;
		BODY(f) = x;
	}
	else {
		n = length(x);
		pf = FORMALS(f) = allocList(n - 1);
		while(--n) {
			if(TAG(x) == R_NilValue) {
				TAG(pf) = CreateTag(CAR(x));
				CAR(pf) = R_MissingArg;
			}
			else {
				CAR(pf) = CAR(x);
				TAG(pf) = TAG(x);
			}
			pf = CDR(pf);
			x = CDR(x);
		}
		BODY(f) = CAR(x);
	}
	UNPROTECT(2);
	return f;
}

SEXP do_as(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP n, u, v;
	int type;

	checkArity(op, args);

	switch (PRIMVAL(op)) {

	/* as.factor */
	/* as.ordered */

	case FACTSXP:
	case ORDSXP:
		return coerceVector(CAR(args), PRIMVAL(op));

	/* as.vector */

	case 101:
		if (!isString(CADR(args)) || LENGTH(CADR(args)) < 1)
			error("as.vector: invalid type argument\n");

		if(!strcmp("function", (CHAR(STRING(CADR(args))[0]))))
			return asFunction(CAR(args));

		type = str2type(CHAR(STRING(CADR(args))[0]));

		switch(type) {
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
		case EXPRSXP:
		case LISTSXP:
		case ANYSXP:
			break;
		default:
			errorcall(call, "mode \"%s\" is invalid\n", CHAR(STRING(CADR(args))[0]));
		}
		
		u = CAR(args);
		if (isVector(u) || isList(u) || isLanguage(u)) {
			if(NAMED(u)) v = duplicate(u);
			else v = u;
			if (type != ANYSXP) {
				PROTECT(v);
				v = coerceVector(v, type);
				UNPROTECT(1);
			}
			PROTECT(v);
			PROTECT(n = getAttrib(u, R_NamesSymbol));
			ATTRIB(v) = R_NilValue;
			if(n != R_NilValue)
				setAttrib(v, R_NamesSymbol, n);
			OBJECT(v) = 0;
			UNPROTECT(2);
			return v;
		}
		else if(isSymbol(u) && type == STRSXP) {
			u = mkString(CHAR(PRINTNAME(u)));
			/* UNPROTECT(1); */
			return u;
		}
		else errorcall(call, "cannot coerce to vector\n");

	/* as.name */
	case 102:
                u = CAR(args);
                if (TYPEOF(u) == SYMSXP)
                        return u;
                if (!isString(u) || LENGTH(u) < 0 ||
                        streql(CHAR(STRING(u)[0]), ""))
                                errorcall(call, "character argument required\n");
                return install(CHAR(STRING(u)[0]));
	default:
		errorcall(call, "unimplemented coersion\n");
	}
}

SEXP do_ascall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP s;

	checkArity(op, args);

	if( isLanguage(CAR(args)) )
		return CAR(args);

	if(!isList(CAR(args)) || length(CAR(args)) < 1 )
		errorcall(call, "invalid argument list\n");

	s = duplicate(args);
	TYPEOF(CAR(s)) = LANGSXP;
	TAG(CAR(s)) = R_NilValue;

	return CAR(s);
}

/* return the type of the SEXP */
SEXP do_typeof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans;
	checkArity(op, args);
	PROTECT(ans = allocVector(STRSXP, 1));
	STRING(ans)[0] = type2str(TYPEOF(CAR(args)));
	UNPROTECT(1);
	return ans;
}

SEXP do_is(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans;

	checkArity(op, args);

	PROTECT(ans = allocVector(LGLSXP, 1));

	switch (PRIMVAL(op)) {
	case NILSXP:		/* is.null */
		LOGICAL(ans)[0] = (CAR(args) == R_NilValue);
		break;
	case LGLSXP:		/* is.logical */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == LGLSXP);
		break;
	case FACTSXP:		/* is.factor */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == FACTSXP);
		break;
	case ORDSXP:		/* is.ordered */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == ORDSXP);
		break;
	case INTSXP:		/* is.integer */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == INTSXP);
		break;
	case REALSXP:		/* is.double */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == REALSXP);
		break;
	case CPLXSXP:		/* is.complex */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == CPLXSXP);
		break;
	case STRSXP:		/* is.character */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == STRSXP);
		break;
	case SYMSXP:		/* is.name */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP);
		break;
	case ENVSXP:		/* is.environment */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == ENVSXP);
		break;
	case LISTSXP:		/* is.list */
		LOGICAL(ans)[0] = ((TYPEOF(CAR(args)) == LISTSXP) || TYPEOF(CAR(args)) == NILSXP);
		break;
	case EXPRSXP:		/* is.expression */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == EXPRSXP);
		break;
	case 50:		/* is.object */
		LOGICAL(ans)[0] = OBJECT(CAR(args));
		break;
	case 75:		/* is.factor */
		LOGICAL(ans)[0] = isFactor(CAR(args));
		break;
	case 80:
		LOGICAL(ans)[0] = isFrame(CAR(args));
		break;

	case 100:		/* is.numeric */
		LOGICAL(ans)[0] = (isNumeric(CAR(args))
					&& !isLogical(CAR(args)));
		break;
	case 101:		/* is.matrix */
		LOGICAL(ans)[0] = isMatrix(CAR(args));
		break;
	case 102:		/* is.array */
		LOGICAL(ans)[0] = isArray(CAR(args));
		break;
	case 103:		/* is.ts */
		LOGICAL(ans)[0] = isTs(CAR(args));
		break;

	case 200:		/* is.atomic */
		switch(TYPEOF(CAR(args))) {
		case NILSXP:
		case CHARSXP:
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
			LOGICAL(ans)[0] = 1;
			break;
		default:
			LOGICAL(ans)[0] = 0;
			break;
		}
		break;
	case 201:		/* is.recursive */
		switch(TYPEOF(CAR(args))) {
		case LISTSXP:
		case CLOSXP:
		case ENVSXP:
		case PROMSXP:
		case LANGSXP:
		case SPECIALSXP:
		case BUILTINSXP:
		case DOTSXP:
		case ANYSXP:
			LOGICAL(ans)[0] = 1;
			break;
		default:
			LOGICAL(ans)[0] = 0;
			break;
		}
		break;

	case 300:		/* is.call */
		LOGICAL(ans)[0] = TYPEOF(CAR(args)) == LANGSXP;
		break;
	case 301:		/* is.language */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP) ||
				  (TYPEOF(CAR(args)) == LANGSXP);
		break;
	case 302:		/* is.function */
		LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == CLOSXP) ||
				  (TYPEOF(CAR(args)) == SPECIALSXP) ||
				  (TYPEOF(CAR(args)) == BUILTINSXP);
		break;

	case 999:	/* is.single */
		errorcall(call, "type unimplemented in R\n");
	default:
		errorcall(call, "unimplemented predicate\n");
	}
	UNPROTECT(1);
	return (ans);
}

/* What should is.vector do ? */
/* In S, if an object has no attributes it is a vector */
/* otherwise it is.  It seems to make more sense to check */
/* for a dim attribute. */

SEXP do_isvector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans, a;

	checkArity(op, args);

	if (!isString(CADR(args)) || LENGTH(CADR(args)) <= 0)
		errorcall(call, "is.vector invalid \"mode\" argument\n");

	PROTECT(ans = allocVector(LGLSXP, 1));

	if (streql(CHAR(STRING(CADR(args))[0]), "any")) {
		LOGICAL(ans)[0] = isVector(CAR(args));
	}
	else if (streql(CHAR(STRING(CADR(args))[0]), CHAR(type2str(TYPEOF(CAR(args)))))) {
		LOGICAL(ans)[0] = 1;
	}
	else
		LOGICAL(ans)[0] = 0;
	UNPROTECT(1);

	/* We allow a "names" attribute on any vector. */
	/* "class" and "levels" attributes on factors are also ok. */

	if(ATTRIB(CAR(args)) != R_NilValue) {
		a = ATTRIB(CAR(args));
		while(a != R_NilValue) {
			if(TAG(a) != R_NamesSymbol) {
				LOGICAL(ans)[0] = 0;
				return ans;
			}
			a = CDR(a);
		}
	}
	return (ans);
}

SEXP do_isna(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans, dims, names, x;
	int i, lenx;

        if(DispatchOrEval(call, op, args, rho, &ans, 1))
                return(ans);

	PROTECT(args = ans);
	checkArity(op, args);

	if (!isList(CAR(args)) && !isVector(CAR(args)))
		error("is.na applies only to lists and vectors\n");
	ans = allocVector(LGLSXP, length(CAR(args)));
	x = CAR(args);
	if (isVector(x)) {
		PROTECT(dims = getAttrib(x, R_DimSymbol));
		if (isArray(x))
			PROTECT(names = getAttrib(x, R_DimNamesSymbol));
		else
			PROTECT(names = getAttrib(x, R_NamesSymbol));
	}
	switch (TYPEOF(x)) {
	case LGLSXP:
	case FACTSXP:
	case ORDSXP:
	case INTSXP:
		for (i = 0; i < length(x); i++)
			LOGICAL(ans)[i] = (INTEGER(x)[i] == NA_INTEGER);
		break;
	case REALSXP:
		for (i = 0; i < length(x); i++)
			LOGICAL(ans)[i] = !FINITE(REAL(x)[i]);
		break;
	case CPLXSXP:
		for (i = 0; i < length(x); i++)
			LOGICAL(ans)[i] = !(FINITE(COMPLEX(x)[i].r)
					&& FINITE(COMPLEX(x)[i].i));
		break;
	case STRSXP:
		for (i = 0; i < length(x); i++)
			LOGICAL(ans)[i] = (STRING(x)[i] == NA_STRING);
		break;
	case LISTSXP:
		lenx = length(x);
		for (i = 0; i < lenx; i++) {
			if (!isVector(CAR(x)) || length(CAR(x)) > 1)
				LOGICAL(ans)[i] = 0;
			else {
				switch (TYPEOF(CAR(x))) {
				case LGLSXP:
				case FACTSXP:
				case ORDSXP:
				case INTSXP:
					LOGICAL(ans)[i] = (INTEGER(CAR(x))[0] == NA_INTEGER);
					break;
				case REALSXP:
					LOGICAL(ans)[i] = !FINITE(REAL(CAR(x))[0]);
					break;
				case STRSXP:
					LOGICAL(ans)[i] = (STRING(CAR(x))[0] == NA_STRING);
					break;
				case CPLXSXP:
					LOGICAL(ans)[i] = !(FINITE(COMPLEX(CAR(x))[0].r)
						&& FINITE(COMPLEX(CAR(x))[0].i));
					break;
				}
			}
			x = CDR(x);
		}
		break;
	}
	if (isVector(x)) {
		setAttrib(ans, R_DimSymbol, dims);
		if (isArray(x))
			setAttrib(ans, R_DimNamesSymbol, names);
		else
			setAttrib(ans, R_NamesSymbol, names);
		UNPROTECT(2);
	}
	UNPROTECT(1);
	return ans;
}

SEXP coerceVector(SEXP v, SEXPTYPE type)
{
	SEXP ans;

	if (TYPEOF(v) == type)
		return v;

	/* is this dangerous ??? */
	/* should we duplicate here */

	if (TYPEOF(v) == LANGSXP && type==LISTSXP) {
		TYPEOF(v) = LISTSXP;
		return(v);
	}
	if(type == EXPRSXP) {
		return coerceToExpression(v);
	}

	if (isList(v) || isLanguage(v))
		return coerceList(v, type);

	if ((type < LGLSXP || STRSXP < type) && (type != EXPRSXP) && (type != LISTSXP))
		error("attempt to coerce a vector to non-vector type\n");

	if (!isVector(v))
		error("attempt to type-coerce non-vector\n");

	PROTECT(v);
	switch (type) {
	case LGLSXP:
		ans = coerceToLogical(v);
		break;
	case FACTSXP:
		ans = coerceToFactor(v);
		break;
	case ORDSXP:
		ans = coerceToOrdered(v);
		break;
	case INTSXP:
		ans = coerceToInteger(v);
		break;
	case REALSXP:
		ans = coerceToReal(v);
		break;
	case CPLXSXP:
		ans = coerceToComplex(v);
		break;
	case STRSXP:
		ans = coerceToString(v);
		break;
	case EXPRSXP:
		ans = coerceToExpression(v);
		break;
	case LISTSXP:
		ans = coerceToList(v);
		break;
	}
	UNPROTECT(1);
	return ans;
}

/* coerce a list to a vector if the list is conformable */
SEXP coerceList(SEXP v, SEXPTYPE type)
{
	int i, n;
	SEXP rval, t, names;

	n = length(v);
	names = v;
	if (type == STRSXP) {
		PROTECT(rval = allocVector(type, n));
		i = 0;
		t = v;
		for ( ; v != R_NilValue; v = CDR(v), i++) {
			if( isString(CAR(v)) && length(CAR(v)) == 1 )
				STRING(rval)[i] = STRING(CAR(v))[0];
			else
				STRING(rval)[i] = STRING(deparse1(CAR(v), 0))[0];
		}
	}
	else if(type == EXPRSXP) {
		PROTECT(rval = allocVector(type, 1));
		VECTOR(rval)[0] = v;
		UNPROTECT(1);
		return rval;
	}
	else {
		if (!isVectorizable(v))
			error("object cannot be coerced to vector type\n");
		rval = allocVector(type, n);
		PROTECT(rval);
		for (i = 0; i < n; i++) {
			t = coerceVector(CAR(v), type);
			switch (type) {
			case LGLSXP:
			case FACTSXP:
			case ORDSXP:
			case INTSXP:
				INTEGER(rval)[i] = INTEGER(t)[0];
				break;
			case REALSXP:
				REAL(rval)[i] = REAL(t)[0];
				break;
			case CPLXSXP:
				COMPLEX(rval)[i] = COMPLEX(t)[0];
				break;
			default:
				UNIMPLEMENTED("coerceList");
			}
			v = CDR(v);
		}

		/* If any tags are non-null then we */
		/* need to add a names attribute. */
	}
	v = names;
	i = 0;
	for (t = v; t != R_NilValue; t = CDR(t))
		if (TAG(t) != R_NilValue)
			i = 1;

	if (i) {
		i = 0;
		names = allocVector(STRSXP, n);
		for (t = v; t != R_NilValue; t = CDR(t), i++)
			if (TAG(t) != R_NilValue)
				STRING(names)[i] = PRINTNAME(TAG(t));
		setAttrib(rval, R_NamesSymbol, names);
	}
	UNPROTECT(1);
	return rval;
}

static SEXP coerceToLogical(SEXP v)
{
	SEXP ans, levs;
	int i, n;

	ans = allocVector(LGLSXP, n = length(v));
	PROTECT(ans);
	ATTRIB(ans) = duplicate(ATTRIB(v));
	switch (TYPEOF(v)) {
	case FACTSXP:
	case ORDSXP:
		if((levs = getAttrib(v, R_LevelsSymbol)) == R_NilValue) {
			for (i = 0; i < n; i++) {
				if(INTEGER(v)[i] == NA_INTEGER)
					LOGICAL(ans)[i] = NA_LOGICAL;
				else 
					LOGICAL(ans)[i] = (INTEGER(v)[i] != 0);
			}
		}
		else {
			int vi;
			int nlevs = LEVELS(v);
			SEXP llevs;
			PROTECT(levs);
			PROTECT(llevs = allocVector(LGLSXP, nlevs));
			for(i=0 ; i<length(levs) ; i++) {
				if (StringTrue(CHAR(STRING(levs)[i])))
					LOGICAL(llevs)[i] = 1;
				else if (StringFalse(CHAR(STRING(levs)[i])))
					LOGICAL(llevs)[i] = 0;
				else
					LOGICAL(llevs)[i] = NA_LOGICAL;
			}
			for (i = 0; i < n; i++) {
				vi = INTEGER(v)[i];
				if(vi < 1 || vi > nlevs)
					LOGICAL(ans)[i] = NA_LOGICAL;
				else 
					LOGICAL(ans)[i] = LOGICAL(llevs)[vi-1];
			}
			UNPROTECT(2);
		}
		break;
	case INTSXP:
		for (i = 0; i < n; i++) {
			if(INTEGER(v)[i] == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else 
				LOGICAL(ans)[i] = (INTEGER(v)[i] != 0);
		}
		break;
	case REALSXP:
		for (i = 0; i < n; i++) {
			if(FINITE(REAL(v)[i])) LOGICAL(ans)[i] = (REAL(v)[i] != 0);
			else LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case CPLXSXP:
		for (i = 0; i < n; i++) {
			if(FINITE(COMPLEX(v)[i].r) && FINITE(COMPLEX(v)[i].i))
				LOGICAL(ans)[i] = (COMPLEX(v)[i].r != 0 || COMPLEX(v)[i].i != 0);
			else LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case STRSXP:
		error("character vectors cannot be coerced to logical\n");
		for (i = 0; i < n; i++) {
			if (StringTrue(CHAR(STRING(v)[i])))
				LOGICAL(ans)[i] = 1;
			else if (StringFalse(CHAR(STRING(v)[i])))
				LOGICAL(ans)[i] = 0;
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	}
	UNPROTECT(1);
	return ans;
}

static SEXP coerceToFactor(SEXP v)
{
	SEXP ans, levs;
	int i, n;

	PROTECT(ans = allocVector(FACTSXP, n = LENGTH(v)));
	ATTRIB(ans) = duplicate(ATTRIB(v));
	switch (TYPEOF(v)) {
	case LGLSXP:
		LEVELS(ans) = 2;
		for (i = 0; i < n; i++) {
			FACTOR(ans)[i] = (LOGICAL(v)[i] == NA_LOGICAL) ?
				NA_FACTOR : ((LOGICAL(v)[i] == 0) ? 1 : 2);
		}
		PROTECT(levs = allocVector(STRSXP, 2));
		STRING(levs)[0] = mkChar("FALSE");
		STRING(levs)[1] = mkChar("TRUE");
		setAttrib(ans, R_LevelsSymbol, levs);
		UNPROTECT(1);
		break;
	case ORDSXP:
		LEVELS(ans) = LEVELS(v);
		for (i = 0; i < n; i++) {
			FACTOR(ans)[i] = FACTOR(v)[i];
		}
		break;
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
		error("use \"factor\", \"ordered\" or \"cut\" to create factors\n");
		break;
	}
	PROTECT(levs = allocVector(STRSXP, 1));
	STRING(levs)[0] = mkChar("factor");
	setAttrib(ans, R_ClassSymbol, levs);
        UNPROTECT(2);
	return ans;
}

static SEXP coerceToOrdered(SEXP v)
{
	SEXP ans, levs;
	int i, n;

	ans = allocVector(ORDSXP, n = LENGTH(v));
	PROTECT(ans);
	ATTRIB(ans) = duplicate(ATTRIB(v));
	switch (TYPEOF(v)) {
	case LGLSXP:
		LEVELS(ans) = 2;
		for (i = 0; i < n; i++) {
			FACTOR(ans)[i] = (LOGICAL(v)[i] == NA_LOGICAL) ?
				NA_FACTOR : ((LOGICAL(v)[i] == 0) ? 1 : 2);
		}
		PROTECT(levs = allocVector(STRSXP, 2));
		STRING(levs)[0] = mkChar("FALSE");
		STRING(levs)[1] = mkChar("TRUE");
		setAttrib(ans, R_LevelsSymbol, levs);
		UNPROTECT(1);
		break;
	case FACTSXP:
		LEVELS(ans) = LEVELS(v);
		for (i = 0; i < n; i++) {
			FACTOR(ans)[i] = FACTOR(v)[i];
		}
		break;
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
		error("use \"factor\", \"ordered\" or \"cut\" to create factors\n");
		break;
	}
	PROTECT(levs = allocVector(STRSXP, 2));
	STRING(levs)[0] = mkChar("ordered");
	STRING(levs)[1] = mkChar("factor");
	setAttrib(ans, R_ClassSymbol, levs);
	UNPROTECT(2);
	return ans;
}

static SEXP coerceToInteger(SEXP v)
{
	SEXP ans, lv;
	int i, li, n, warn;
	double out;
	char *endp;

	warn = 0;
	PROTECT(ans = allocVector(INTSXP, n = LENGTH(v)));
	PROTECT(ATTRIB(ans) = duplicate(ATTRIB(v)));
	switch (TYPEOF(v)) {
	case LGLSXP:
		for (i = 0; i < n; i++) {
			INTEGER(ans)[i] = (LOGICAL(v)[i] == NA_LOGICAL) ?
				NA_INTEGER : LOGICAL(v)[i];
		}
		break;
	case FACTSXP:
	case ORDSXP:
		lv = coerceVector(getAttrib(v, R_LevelsSymbol), INTSXP);
		for (i = 0; i < n ; i++) {
			li = FACTOR(v)[i];
			INTEGER(ans)[i] = (li == NA_FACTOR) ?
				NA_INTEGER : INTEGER(lv)[li-1];
		}
		break;
	case REALSXP:
		for (i = 0; i < n; i++) {
			if( !FINITE(REAL(v)[i]) )
				INTEGER(ans)[i] = NA_INTEGER;
			else if ( REAL(v)[i] >= LONG_MAX+1.0 || REAL(v)[i]<= LONG_MIN-1.0 ) {
				warn = 1;
				INTEGER(ans)[i] = NA_INTEGER;
			}
			else
				INTEGER(ans)[i] = REAL(v)[i];
		}
		break;
	case CPLXSXP:
		for (i = 0; i < n; i++) {
			if( (FINITE(COMPLEX(v)[i].r) && FINITE(COMPLEX(v)[i].i)) )
				INTEGER(ans)[i] = NA_INTEGER;
			else if ( COMPLEX(v)[i].r >= LONG_MAX+1.0 || COMPLEX(v)[i].r <= LONG_MIN-1.0 ) {     
				warn = 1;
				INTEGER(ans)[i] = NA_INTEGER;
			}
			else

				INTEGER(ans)[i] = COMPLEX(v)[i].r;
		}
		break;
	case STRSXP:
		/*  Jeez!  Why was this again?  I've forgotten!
		 *  for reasons best known to ourselves we implement this by
		 *  first converting to real and then from real to integer  */
		for (i = 0; i < n; i++) {
			if (!strcmp(CHAR(STRING(v)[i]), "NA"))
				INTEGER(ans)[i] = NA_INTEGER;
			else {
				out = strtod(CHAR(STRING(v)[i]), &endp);
				if (*endp == '\0')	/* we have a real */ {
					if( out >= LONG_MAX+1.0 || out <= LONG_MIN-1.0 ) {
						warn = 1;
						INTEGER(ans)[i] = NA_INTEGER;
					}
					else 
						INTEGER(ans)[i] = out;
				}
				else
					INTEGER(ans)[i] = NA_INTEGER;
			}
		}
		break;
	}
	UNPROTECT(2);
	if( warn ) warning("integer conversion: some values were too large and were converted to NA\n");
	return ans;
}

static SEXP coerceToReal(SEXP v)
{
	SEXP ans, lv;
	int i, li, n;
	double out;
	char *endp;

	n = LENGTH(v);
	PROTECT(ans = allocVector(REALSXP, n));
	ATTRIB(ans) = duplicate(ATTRIB(v));
	switch (TYPEOF(v)) {
	case LGLSXP:
		for (i = 0; i < n; i++) {
			REAL(ans)[i] = (LOGICAL(v)[i] == NA_LOGICAL) ?
				NA_REAL : LOGICAL(v)[i];
		}
		break;
	case FACTSXP:
	case ORDSXP:
		lv = coerceVector(getAttrib(v, R_LevelsSymbol), REALSXP);
		for (i = 0; i < n ; i++) {
			li = FACTOR(v)[i];
			REAL(ans)[i] = (li == NA_FACTOR) ?
				NA_REAL : REAL(lv)[li-1];
		}
		break;
	case INTSXP:
		for (i = 0; i < n; i++) {
			REAL(ans)[i] = (INTEGER(v)[i] == NA_INTEGER) ?
				NA_REAL : INTEGER(v)[i];
		}
		break;
	case CPLXSXP:
		for (i = 0; i < n; i++) {
			REAL(ans)[i] = (COMPLEX(v)[i].r == NA_REAL
					|| COMPLEX(v)[i].i == NA_REAL) ?
				NA_REAL : COMPLEX(v)[i].r;
		}
		warning("complex values coerced to real by dropping imaginary parts\n");
		break;
	case STRSXP:
		for (i = 0; i < n; i++) {
			if (!strcmp(CHAR(STRING(v)[i]), "NA"))
				REAL(ans)[i] = NA_REAL;
			else {
				out = strtod(CHAR(STRING(v)[i]), &endp);
				if (*endp == '\0')	/* we have a real */
					REAL(ans)[i] = out;
				else
					REAL(ans)[i] = NA_REAL;
			}
		}
		break;
	}
	UNPROTECT(1);
	return ans;
}

static SEXP coerceToComplex(SEXP v)
{
	SEXP ans, lv;
	double outr, outi;
	char *endp;
	int i, li, n;

	n = LENGTH(v);
	PROTECT(ans = allocVector(CPLXSXP, n));
	PROTECT(ATTRIB(ans) = duplicate(ATTRIB(v)));
	switch (TYPEOF(v)) {
	case LGLSXP:
		for (i = 0; i < n; i++) {
			if (LOGICAL(v)[i] == NA_LOGICAL) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				COMPLEX(ans)[i].r = LOGICAL(v)[i];
				COMPLEX(ans)[i].i = 0.0;
			}
		}
		break;
	case FACTSXP:
	case ORDSXP:
		lv = coerceVector(getAttrib(v, R_LevelsSymbol), CPLXSXP);
		for (i = 0; i < n ; i++) {
			li = FACTOR(v)[i];
			if(li == NA_FACTOR) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				COMPLEX(ans)[i].r = COMPLEX(lv)[li-1].r;
				COMPLEX(ans)[i].i = COMPLEX(lv)[li-1].i;
			}
		}
		break;
		for (i = 0; i < n; i++) {
			if (FACTOR(v)[i] == NA_FACTOR) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				COMPLEX(ans)[i].r = FACTOR(v)[i];
				COMPLEX(ans)[i].i = 0.0;
			}
		}
		break;
	case INTSXP:
		for (i = 0; i < n; i++) {
			if (INTEGER(v)[i] == NA_INTEGER) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				COMPLEX(ans)[i].r = INTEGER(v)[i];
				COMPLEX(ans)[i].i = 0.0;
			}
		}
		break;
	case REALSXP:
		for (i = 0; i < n; i++) {
			if (!FINITE(REAL(v)[i])) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				COMPLEX(ans)[i].r = REAL(v)[i];
				COMPLEX(ans)[i].i = 0.0;
			}
		}
		break;
	case STRSXP:
		for (i = 0 ; i < n ; i++) {
			endp = CHAR(STRING(v)[i]);
			if (!strcmp(endp, "NA")) {
				COMPLEX(ans)[i].r = NA_REAL;
				COMPLEX(ans)[i].i = NA_REAL;
			}
			else {
				outr = strtod(endp, &endp);
				if (*endp == '\0') {
					COMPLEX(ans)[i].r = outr;
					COMPLEX(ans)[i].i = 0.0;
				}
				else if(*endp == '+' || *endp == '-') {
					outi = strtod(endp, &endp);
					if (endp[0] == 'i' && endp[1] == '\0') {
						COMPLEX(ans)[i].r = outr;
						COMPLEX(ans)[i].i = outi;
					}
					else {
						COMPLEX(ans)[i].r = NA_REAL;
						COMPLEX(ans)[i].i = NA_REAL;
					}
				}
				else {
					COMPLEX(ans)[i].r = NA_REAL;
					COMPLEX(ans)[i].i = NA_REAL;
				}
			}
		}
		break;
	}
	UNPROTECT(2);
	return ans;
}

static SEXP coerceToString(SEXP v)
{
	SEXP ans, tmpchar;
	int i, j, n;
	char *strp;

	PrintDefaults(R_NilValue);

	n = length(v);
	ans = allocVector(STRSXP, n);
	PROTECT(ans);
	ATTRIB(ans) = duplicate(ATTRIB(v));
	if (isFactor(v)
		&& (tmpchar = getAttrib(v, install("levels"))) != R_NilValue
		&& TYPEOF(tmpchar) == STRSXP
		&& LENGTH(tmpchar) == (int)LEVELS(v)) {
		for (i = 0; i < n; i++) {
			j = FACTOR(v)[i];
			if (j < 1 || j > (int)LEVELS(v))
				STRING(ans)[i] = NA_STRING;
			else
				STRING(ans)[i] = STRING(tmpchar)[j - 1];
		}
		setAttrib(ans, install("levels"), R_NilValue);
	}
	else {
		for (i = 0; i < n; i++) {
			strp = EncodeElement( v, i, 0);
			if (streql(strp, "NA"))
				STRING(ans)[i] = NA_STRING;
			else {
				tmpchar = allocString(strlen(strp));
				strcpy(CHAR(tmpchar), strp);
				STRING(ans)[i] = tmpchar;
			}
		}
	}
	UNPROTECT(1);
	return (ans);
}

static SEXP coerceToExpression(SEXP v)
{
	SEXP ans, tmp;
	int i, n, newtype;

		/* ATTRIB(ans) = duplicate(ATTRIB(v)); */
	if(isVector(v)) {
		n = LENGTH(v);
		PROTECT(ans = allocVector(EXPRSXP, n));
		switch (TYPEOF(v)) {
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
			if(isFactor(v)) newtype = INTSXP;
			else newtype = TYPEOF(v);
			for(i=0 ; i<n ; i++) {
				tmp = allocVector(newtype, 1);
				INTEGER(tmp)[0] = INTEGER(v)[i];
				VECTOR(ans)[i] = tmp;
			}
			break;
		case REALSXP:
			for(i=0 ; i<n ; i++) {
				tmp = allocVector(REALSXP, 1);
				REAL(tmp)[0] = REAL(v)[i];
				VECTOR(ans)[i] = tmp;
			}
			break;
		case CPLXSXP:
			for(i=0 ; i<n ; i++) {
				tmp = allocVector(CPLXSXP, 1);
				COMPLEX(tmp)[0].r = COMPLEX(v)[i].r;
				COMPLEX(tmp)[0].i = COMPLEX(v)[i].i;
				VECTOR(ans)[i] = tmp;
			}
			break;
		case STRSXP:
			for(i=0 ; i<n ; i++) {
				tmp = allocVector(STRSXP, 1);
				STRING(tmp)[0] = STRING(v)[i];
				VECTOR(ans)[i] = tmp;
			}
			break;
		}
	}
	else {
		PROTECT(ans = allocVector(EXPRSXP, 1));
		VECTOR(ans)[0] = duplicate(v);
	}
	UNPROTECT(1);
	return ans;
}

static SEXP coerceToList(SEXP v)
{
	SEXP ans, tmp;
	int i, n;

	n = LENGTH(v);
	ans = allocList(n);
	tmp = ans;

	PROTECT(ans);
	for (i = 0; i < n; i++) {
		switch (TYPEOF(v)) {
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
			CAR(tmp) = allocVector(INTSXP, 1);
			INTEGER(CAR(tmp))[0] = INTEGER(v)[i];
			break;
		case REALSXP:
			CAR(tmp) = allocVector(REALSXP, 1);
			REAL(CAR(tmp))[0] = REAL(v)[i];
			break;
		case CPLXSXP:
			CAR(tmp) = allocVector(CPLXSXP, 1);
			COMPLEX(CAR(tmp))[0] = COMPLEX(v)[i];
			break;
		case STRSXP:
			CAR(tmp) = allocVector(STRSXP, 1);
			STRING(CAR(tmp))[0] = STRING(v)[i];
			break;
		default:
			UNIMPLEMENTED("coerceToList");
		}
		tmp = CDR(tmp);
	}
	tmp = getAttrib(v, R_NamesSymbol);
	if (tmp != R_NilValue)	/* this will only handle vectors */
		setAttrib(ans, R_NamesSymbol, tmp);
	UNPROTECT(1);
	return (ans);
}

SEXP do_call(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP rest, evargs, rfun;

	PROTECT(rfun = eval(CAR(args), rho));
	if (!isString(rfun) || length(rfun) <= 0 ||
			streql(CHAR(STRING(rfun)[0]), ""))
		errorcall(call, "first argument must be a character string\n");
	PROTECT(rfun = install(CHAR(STRING(rfun)[0])));
	PROTECT(evargs = duplicate(CDR(args)));
	for (rest = evargs; rest != R_NilValue; rest = CDR(rest))
		CAR(rest) = eval(CAR(rest), rho);
	rfun = LCONS(rfun, evargs);
	UNPROTECT(3);
	return (rfun);
}

SEXP do_docall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	if (!isString(CAR(args)) || length(CAR(args)) < 0 ||
			streql(CHAR(STRING(CAR(args))[0]), ""))
		errorcall(call, "first argument must be a string\n");
	if (!isNull(CADR(args)) && !isList(CADR(args)))
		errorcall(call, "second argument must be a list\n");
	call = install(CHAR(STRING(CAR(args))[0]));
	PROTECT(call = LCONS(call, CADR(args)));
	call = eval(call, rho);
	UNPROTECT(1);
	return call;
}

/*
 * do_substitute has two arguments, an expression and an environment
 * (optional). Symbols found in the expression are substituted with their
 * values as found in the environment. There is no inheritance so only the
 * supplied environment is searched. If no environment is specified the
 * environment in which substitute was called is used. If the specified
 * environment is R_NilValue then R_GlobalEnv is used. Arguments to
 * do_substitute should not be evaluated.
 * 
 */

SEXP substituteList(SEXP, SEXP);

SEXP substitute(SEXP lang, SEXP rho)
{
	SEXP t;

	switch (TYPEOF(lang)) {
	case PROMSXP:
		return substitute(PREXPR(lang), rho);
	case SYMSXP:
		t = findVarInFrame(FRAME(rho), lang);
		if (t != R_UnboundValue) {
			if (TYPEOF(t) == PROMSXP) {
				do {
					t = PREXPR(t);
				}
				while(TYPEOF(t) == PROMSXP);
				return t;
/*
				return substitute(PREXPR(t), rho);
				return PREXPR(t);
*/
			}
			else if (TYPEOF(t) == DOTSXP) {
				error("... used in an incorrect context\n");
			}
			if (rho != R_GlobalEnv)
				return t;
		}
		return (lang);
	case LANGSXP:
		return substituteList(lang, rho);
	default:
		return (lang);
	}
}


	/* Work through a list doing substitute on the */
	/* elements taking particular care to handle ... */

SEXP substituteList(SEXP el, SEXP rho)
{
	SEXP h, t;
	if(isNull(el))
		return el;

	if (CAR(el) == R_DotsSymbol) {
		h = findVar(CAR(el), rho);
		if (h == R_NilValue)
			return substituteList(CDR(el), rho);
		if (TYPEOF(h) != DOTSXP) {
			if (h == R_MissingArg)
				return substituteList(CDR(el), rho);
			error("... used in an incorrect context\n");
		}
		PROTECT(h = substituteList(h, rho));
		PROTECT(t = substituteList(CDR(el), rho));
		t = listAppend(h, t);
		UNPROTECT(2);
		return t;
	}
/*
	else if (CAR(el) == R_MissingArg) {
		return substituteList(CDR(el), rho);
	}
*/
	else {
		PROTECT(h = substitute(CAR(el), rho));
		PROTECT(t = substituteList(CDR(el), rho));
		if(isLanguage(el))
			t = LCONS(h, t);
		else
			t = CONS(h, t);
		TAG(t) = TAG(el);
		UNPROTECT(2);
		return t;
	}
}

SEXP do_substitute(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP env, s;

		/* set up the environment for substitution */

	if (length(args) == 1)
		env = rho;
	else
		env = eval(CADR(args), rho);
	if (env == R_NilValue)
		env = R_GlobalEnv;
	if (TYPEOF(env) != ENVSXP)
		errorcall(call, "invalid environment specified\n");

	PROTECT(env);
	CDR(args) = R_NilValue;
	s = substituteList(args, env);
	UNPROTECT(1);
	return CAR(s);
}
