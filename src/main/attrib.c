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

static void checkNames(SEXP, SEXP);
static SEXP installAttrib(SEXP, SEXP, SEXP);
static SEXP removeAttrib(SEXP, SEXP);

SEXP comment(SEXP);
SEXP commentgets(SEXP, SEXP);

static SEXP stripAttrib(SEXP tag, SEXP lst)
{
	if(lst == R_NilValue) return lst;
	if(tag == TAG(lst)) return stripAttrib(tag, CDR(lst));
	CDR(lst) = stripAttrib(tag, CDR(lst));
	return lst;
}

SEXP getAttrib(SEXP vec, SEXP name)
{
	SEXP s, blank;
	int len, i, any;


	if (isString(name))
		name = install(CHAR(STRING(name)[0]));
	if ((name == R_NamesSymbol) && (isList(vec) || isLanguage(vec))) {
		len = length(vec);
		PROTECT(s = allocVector(STRSXP, len));
		blank = mkChar("");
		i = 0;
		any = 0;
		for (; vec != R_NilValue; vec=CDR(vec),i++) {
			if (TAG(vec) == R_NilValue)
				STRING(s)[i] = blank;
			else if (isSymbol(TAG(vec))) {
				any = 1;
				STRING(s)[i] = PRINTNAME(TAG(vec));
			}
			else
				error("getAttrib: invalid type for TAG\n");
		}
		UNPROTECT(1);
		if (any) return (s);
		return R_NilValue;
	}
	for (s = ATTRIB(vec); s != R_NilValue; s = CDR(s))
		if (TAG(s) == name) {
			NAMED(CAR(s)) = NAMED(vec);
			return CAR(s);
		}
	return R_NilValue;
}

SEXP setAttrib(SEXP vec, SEXP name, SEXP val)
{
	if (isString(name))
		name = install(CHAR(STRING(name)[0]));
	if (val == R_NilValue)
		return removeAttrib(vec, name);

	if (vec == R_NilValue)
		error("attempt to set an attribute on NULL\n");

	PROTECT(vec);
	PROTECT(name);
	val = duplicate(val);
	UNPROTECT(2);

	if (name == R_NamesSymbol)
		return namesgets(vec, val);
	else if (name == R_DimSymbol)
		return dimgets(vec, val);
	else if (name == R_DimNamesSymbol) 
		return dimnamesgets(vec, val);
	else if (name == R_ClassSymbol)
		return classgets(vec, val);
	else if (name == R_LevelsSymbol)
		return levelsgets(vec, val);
	else if (name == R_TspSymbol)
		return tspgets(vec, val);
	else if (name == R_RowNamesSymbol)
		return rownamesgets(vec, val);
	else if (name == R_CommentSymbol)
		return commentgets(vec, val);
	else
		return installAttrib(vec, name, val);
}

SEXP installAttrib(SEXP vec, SEXP name, SEXP val)
{
	SEXP s, t;

	PROTECT(vec);
	PROTECT(name);
	PROTECT(val);

	for (s = ATTRIB(vec); s != R_NilValue; s = CDR(s)) {
		if (TAG(s) == name) {
			CAR(s) = val;
			UNPROTECT(3);
			return val;
		}
	}
	s = allocList(1);
	CAR(s) = val;
	TAG(s) = name;
	if (ATTRIB(vec) == R_NilValue)
		ATTRIB(vec) = s;
	else {
		t = nthcdr(ATTRIB(vec), length(ATTRIB(vec)) - 1);
		SETCDR(t, s);
	}
	UNPROTECT(3);
	return val;
}

SEXP removeAttrib(SEXP vec, SEXP name)
{
	SEXP t;

	if (name == R_NamesSymbol && isList(vec)) {
		for (t = vec; t != R_NilValue; t = CDR(t))
			TAG(t) = R_NilValue;
		return R_NilValue;
	}
	else {
		if (name == R_DimSymbol)
			ATTRIB(vec) = stripAttrib(R_DimNamesSymbol, ATTRIB(vec));
		ATTRIB(vec) = stripAttrib(name, ATTRIB(vec));
		if (name == R_ClassSymbol)
			OBJECT(vec) = 0;
	}
	return R_NilValue;
}

void checkNames(SEXP x, SEXP s)
{
	if (isVector(x) || isList(x) || isLanguage(x)) {
		if (!isVector(s) && !isList(s))
			error("invalid type for names: must be vector\n");
		if (length(x) != length(s))
			error("names attribute must be the same length as the vector\n");
	}
	else error("names applied to non-vector\n");
}


	/* Time Series Parameters */

static void badtsp()
{
	error("invalid time series parameters specified\n");
}

SEXP tspgets(SEXP vec, SEXP val)
{
	double start, end, frequency;
	int n;

	if (!isNumeric(val) || length(val) != 3)
		error("tsp attribute must be numeric of length three\n");

	if (isReal(val)) {
		start = REAL(val)[0];
		end = REAL(val)[1];
		frequency = REAL(val)[2];
	}
	else {
		start = (INTEGER(val)[0] == NA_INTEGER) ? NA_REAL : INTEGER(val)[0];
		end = (INTEGER(val)[1] == NA_INTEGER) ? NA_REAL : INTEGER(val)[1];
		frequency = (INTEGER(val)[2] == NA_INTEGER) ? NA_REAL : INTEGER(val)[2];
	}
	if(frequency <= 0) badtsp();
	n = nrows(vec);
	if(fabs(end - start - (n - 1)/frequency) > 1.e-5)
		badtsp();

	PROTECT(vec);
	val = allocVector(REALSXP, 3);
	PROTECT(val);
	REAL(val)[0] = start;
	REAL(val)[1] = end;
	REAL(val)[2] = frequency;
	installAttrib(vec, R_TspSymbol, val);
	UNPROTECT(2);
	return vec;
}

SEXP levelsgets(SEXP vec, SEXP levels)
{
	if(isFactor(vec) && LENGTH(levels) != LEVELS(vec))
		error("length of \"levels\" vector and number of levels differ\n");
	PROTECT(vec);
	PROTECT(levels = coerceVector(levels, STRSXP));
	installAttrib(vec, R_LevelsSymbol, levels);
	UNPROTECT(2);
	return vec;
}

SEXP commentgets(SEXP vec, SEXP comment)
{
	if(isNull(comment) || isString(comment)) {
		if(length(comment) <= 0) {
			ATTRIB(vec) = stripAttrib(R_CommentSymbol, vec);
		}
		else {
			installAttrib(vec, R_CommentSymbol, comment);
		}
		return R_NilValue;
	}
	error("attempt to set invalid comment attribute\n");
}

SEXP do_commentgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) == 2) CAR(args) = duplicate(CAR(args));
	if(length(CADR(args)) == 0) CADR(args) = R_NilValue;
	setAttrib(CAR(args), R_CommentSymbol, CADR(args));
	return CAR(args);
}

SEXP do_comment(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	return getAttrib(CAR(args), R_CommentSymbol);
}

SEXP classgets(SEXP vec, SEXP class)
{
	if(isNull(class) || isString(class)) {
		if(length(class) <= 0) {
			ATTRIB(vec) = stripAttrib(R_ClassSymbol, vec);
			OBJECT(vec) = 0;
		}
		else {
			if(streql(CHAR(STRING(class)[0]), "data.frame") &&
				!isList(vec) )
				error("attempt to make non-list a data frame\n");
			installAttrib(vec, R_ClassSymbol, class);
			OBJECT(vec) = 1;
		}
		return R_NilValue;
	}
	error("attempt to set invalid class attribute\n");
}

SEXP do_classgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) == 2) CAR(args) = duplicate(CAR(args));
	if(length(CADR(args)) == 0) CADR(args) = R_NilValue;
	setAttrib(CAR(args), R_ClassSymbol, CADR(args));
	return CAR(args);
}

SEXP do_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	return getAttrib(CAR(args), R_ClassSymbol);
}


SEXP do_levelsgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) == 2) CAR(args) = duplicate(CAR(args));
	setAttrib(CAR(args), R_LevelsSymbol, CADR(args));
	return CAR(args);
}

SEXP do_levels(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;
	int i, n;
	char *s;

	checkArity(op, args);
	ans = getAttrib(CAR(args), R_LevelsSymbol);
	if(isFactor(CAR(args)) && ans == R_NilValue) {
		n = LEVELS(CAR(args));
		PROTECT(ans = allocVector(STRSXP, n));
		for(i=0 ; i<n ; i++) {
			s = Rsprintf("%d",i+1);
			STRING(ans)[i] = mkChar(s);
		}
		UNPROTECT(1);
	}
	return ans;
}

SEXP do_namesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) == 2) CAR(args) = duplicate(CAR(args));
	setAttrib(CAR(args), R_NamesSymbol, CADR(args));
	return CAR(args);
}

SEXP namesgets(SEXP vec, SEXP val)
{
	int i;
	SEXP s,rval;

	PROTECT(vec);
	PROTECT(val);

	if( isList(val) )
		if( !isVectorizable(val) )
			error("incompatible names argument\n");
		else {
			rval = allocVector(STRSXP, length(vec));
			PROTECT(rval);
			for (i = 0; i < length(vec); i++) {
				s = coerceVector(CAR(val), STRSXP);
				STRING(rval)[i] = STRING(s)[0];
			}
			UNPROTECT(1);
			val = rval;
		}
	else
		val = coerceVector(val, STRSXP);
	UNPROTECT(1);
	PROTECT(val);

	checkNames(vec, val);

	if (isList(vec) || isLanguage(vec)) {
		i=0;
		for (s = vec; s != R_NilValue; s = CDR(s), i++)
			if (STRING(val)[i] != R_NilValue
			 && STRING(val)[i] != R_NaString
			 && *CHAR(STRING(val)[i]) != 0)
				TAG(s) = install(CHAR(STRING(val)[i]));
			else
				TAG(s) = R_NilValue;
	}
	else if (isVector(vec))
		installAttrib(vec, R_NamesSymbol, val);
	else
		error("invalid type to set names attribute\n");
	UNPROTECT(2);
	return vec;
}

SEXP do_names(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, t;
	checkArity(op, args);
	s = CAR(args);
	if(isVector(s) || isList(s) || isLanguage(s)) {
		t = getAttrib(s, R_DimSymbol);
		if(TYPEOF(t) == INTSXP && length(t) == 1) {
			t = getAttrib(s, R_DimNamesSymbol);
			if(!isNull(t)) return CAR(t);
		}
		else return getAttrib(s, R_NamesSymbol);
	}
	return R_NilValue;
}

SEXP duplicated(SEXP);

SEXP rownamesgets(SEXP vec, SEXP val)
{
	int i;
	SEXP dups;

	PROTECT(vec);
	PROTECT(val);

	dups=duplicated(val);
	for(i=0; i < length(dups) ; i++ )
		if( LOGICAL(dups)[i] ) {
		    warning("some row names are duplicated; argument ignored\n");
		    UNPROTECT(2);
		    return vec;
		}

	if(isFrame(vec)) {
		val = coerceVector(val, STRSXP);
		UNPROTECT(1);
		PROTECT(val);

		if (nrows(CAR(vec)) != length(val))
			error("names attribute must be the same length as the vector\n");

	}
	installAttrib(vec, R_RowNamesSymbol, val);
	UNPROTECT(2);
	return vec;
}

SEXP do_rownames(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	return getAttrib(CAR(args), R_RowNamesSymbol);
}

SEXP do_dimnamesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) > 2) CAR(args) = duplicate(CAR(args));
	if( isFrame(CAR(args)) ) {
		if( !isList(CADR(args)) )
			errorcall(call,"wrong argument type for new dimnames\n");
		switch( length(CADR(args)) ) {
		case 0:
			setAttrib(CAR(args), R_RowNamesSymbol, R_NilValue);
			setAttrib(CAR(args), R_NamesSymbol, R_NilValue);
			break;
		case 2:
			setAttrib(CAR(args), R_RowNamesSymbol, CAR(CADR(args)));
			setAttrib(CAR(args), R_NamesSymbol, CADR(CADR(args)));
			break;
		default:
			errorcall(call,"wrong length for new dimnames\n");
		}
	}
	else
		setAttrib(CAR(args), R_DimNamesSymbol, CADR(args));
	return CAR(args);
}

SEXP dimnamesgets(SEXP vec, SEXP val)
{
	SEXP dims, top;
	int k, i;

	PROTECT(vec);
	PROTECT(val);

	if (!isArray(vec) && !isList(vec) && !isFrame(vec))
		error("dimnames applied to non-array\n");
	if (!isList(val)) error("invalid type for dimnames: must be a list\n");
	dims = getAttrib(vec, R_DimSymbol);
	if (isFrame(vec)) {
		if(length(val) != 2)
			error("dimnames: number of dimensions must equal number of names\n");
		vec = rownamesgets(vec, CAR(val));
		UNPROTECT(2);
		PROTECT(vec);
		PROTECT(val);
		vec = namesgets(vec, CADR(val));
		UNPROTECT(2);
		return vec;
	}
	if ((k = LENGTH(dims)) != length(val))
		error("dimnames: number of dimensions must equal number of names\n");
	top = val;
	for (i = 0; i < k; i++) {
		if (CAR(val) != R_NilValue) {
			if (!isVector(CAR(val)))
				error("invalid type for dim name must be a vector\n");
			if (INTEGER(dims)[i] != LENGTH(CAR(val)) && LENGTH(CAR(val)) != 0)
				error("length of namelist must equal dims\n");
			if(LENGTH(CAR(val)) == 0) {
				CAR(val) = R_NilValue;
			}
			else if (!isString(CAR(val))) {
					CAR(val) = coerceVector(CAR(val), STRSXP);
			}
		}
		val = CDR(val);
	}
	installAttrib(vec, R_DimNamesSymbol, top);
	UNPROTECT(2);
	return (vec);
}

SEXP do_dimnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(isFrame(CAR(args))) {
		PROTECT(op = allocList(2));
		CAR(op) = getAttrib(CAR(args),R_RowNamesSymbol);
		CADR(op) = getAttrib(CAR(args),R_NamesSymbol);
		UNPROTECT(1);
		return op;
	}
	return (getAttrib(CAR(args), R_DimNamesSymbol));
}

SEXP do_dim(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(isFrame(CAR(args))) {
		op = allocVector(INTSXP, 2);
		INTEGER(op)[0] = nrows(CAAR(args));
		INTEGER(op)[1] = length(CAR(args));
		return op;
	}
	return (getAttrib(CAR(args), R_DimSymbol));
}

SEXP do_dimgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(NAMED(CAR(args)) > 1) CAR(args) = duplicate(CAR(args));
	setAttrib(CAR(args), R_DimSymbol, CADR(args));
	setAttrib(CAR(args), R_NamesSymbol, R_NilValue);
	return CAR(args);
}

SEXP dimgets(SEXP vec, SEXP val)
{
	int len, ndim, i, total;

	PROTECT(vec);
	PROTECT(val);

	if (!isVector(vec) && !isList(vec))
		error("dim<- : invalid first argument\n");

	if (!isVector(val) && !isList(val))
		error("dim<- : invalid second argument\n");
	val = coerceVector(val, INTSXP);
	UNPROTECT(1);
	PROTECT(val);

	len = length(vec);
	ndim = length(val);
	if( ndim == 0 )
		error("dim: Invalid dimension vector\n");
	total = 1;
	for (i=0; i<ndim; i++)
		total *= INTEGER(val)[i];
	if (total != len)
		error("dim<- length of dims do not match the length of object\n");
	removeAttrib(vec, R_DimNamesSymbol);
	installAttrib(vec, R_DimSymbol, val);
	UNPROTECT(2);
	return vec;
}

SEXP do_attributes(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s;

	s = R_NilValue;
	if (isList(CAR(args)) || isFrame(CAR(args)))
		s = getAttrib(CAR(args), R_NamesSymbol);
	PROTECT(s);
	if (s != R_NilValue) {
		s = CONS(s, ATTRIB(CAR(args)));
		TAG(s) = R_NamesSymbol;
	}
	else
		s = ATTRIB(CAR(args));
	UNPROTECT(1);
	NAMED(s) = NAMED(CAR(args));
	return s;
}

static SEXP dimptr;

static SEXP TrimDim(SEXP l)
{
	if(l != R_NilValue) {
		if(TAG(l) == R_DimSymbol) {
			dimptr = l;
			return CDR(l);
		}
		else {
			CDR(l) = TrimDim(CDR(l));
			return l;
		}
	}
	return R_NilValue;
}

/* NOTE: The following code ensures that when an attribute list */
/* is attached to an object, that the "dim" attibute is always */
/* brought to the front of the list.  This ensures that when both */
/* "dim" and "dimnames" are set that the "dim" is attached first. */

SEXP do_attributesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, t;

	if(CAR(args) == R_NilValue ) {
		warning("attempt to set attributes on NULL\n");
		return(R_NilValue);
	}
	if(NAMED(CAR(args)) == 2) CAR(args) = duplicate(CAR(args));
	s = CAR(args);
	t = CADR(args);
	if(isList(s) || isFrame(s))
		setAttrib(s, R_NamesSymbol, R_NilValue);
	ATTRIB(s) = R_NilValue;

	OBJECT(s) = 0;

	if (!isList(t))
		errorcall(call, "attributes must be in a list\n");

		/* Ghastly hack to ensure that "dim" */
		/* is always the first attribute */

	dimptr = R_NilValue;
	t = TrimDim(t);
	if(dimptr != R_NilValue) {
		CDR(dimptr) = t;
		t = dimptr;
	}

	for (; t != R_NilValue; t = CDR(t)) {
		if (TAG(t) == R_NilValue)
			error("all attributes must have names\n");
		setAttrib(s, TAG(t), CAR(t));
	}
	return s;
}

SEXP do_attr(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, t;

	s = CAR(args);
	t = CADR(args);

	if (!isString(t))
		error("attribute name must be of mode character\n");

	return getAttrib(s, install(CHAR(STRING(t)[0])));
}

SEXP do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP obj, name, value;

	obj = eval(CAR(args), env);
	if(NAMED(obj) == 2)
		PROTECT(duplicate(obj));
	else
		PROTECT(obj);

	PROTECT(name = eval(CADR(args), env));
	if (!isString(name))
		error("attr<- : name must be of mode character\n");

	/* rhs is already evaluated */
	PROTECT(value = CAR(CDDR(args)));
	setAttrib(obj, name, value);
	UNPROTECT(3);
	return obj;
}
