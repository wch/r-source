/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Rdefines.h>

static void checkNames(SEXP, SEXP);
static SEXP installAttrib(SEXP, SEXP, SEXP);
static SEXP removeAttrib(SEXP, SEXP);

SEXP comment(SEXP);
static SEXP commentgets(SEXP, SEXP);

static SEXP stripAttrib(SEXP tag, SEXP lst)
{
    if(lst == R_NilValue) return lst;
    if(tag == TAG(lst)) return stripAttrib(tag, CDR(lst));
    SETCDR(lst, stripAttrib(tag, CDR(lst)));
    return lst;
}

SEXP getAttrib(SEXP vec, SEXP name)
{
    SEXP s;
    int len, i, any;

    /* pre-test to avoid expensive operations if clearly not needed -- LT */
    if (ATTRIB(vec) == R_NilValue &&
	! (TYPEOF(vec) == LISTSXP || TYPEOF(vec) == LANGSXP))
	return R_NilValue;

    if (isString(name)) name = install(CHAR(STRING_ELT(name, 0)));

    if (name == R_NamesSymbol) {
	if(isVector(vec) || isList(vec) || isLanguage(vec)) {
	    s = getAttrib(vec, R_DimSymbol);
	    if(TYPEOF(s) == INTSXP && length(s) == 1) {
		s = getAttrib(vec, R_DimNamesSymbol);
                if(!isNull(s)) {
                    SET_NAMED(VECTOR_ELT(s, 0), 2);
                    return VECTOR_ELT(s, 0);
                }
	    }
	}
	if (isList(vec) || isLanguage(vec)) {
	    len = length(vec);
	    PROTECT(s = allocVector(STRSXP, len));
	    i = 0;
	    any = 0;
	    for ( ; vec != R_NilValue; vec = CDR(vec), i++) {
		if (TAG(vec) == R_NilValue)
		    SET_STRING_ELT(s, i, R_BlankString);
		else if (isSymbol(TAG(vec))) {
		    any = 1;
		    SET_STRING_ELT(s, i, PRINTNAME(TAG(vec)));
		}
		else
		    error("getAttrib: invalid type for TAG");
	    }
	    UNPROTECT(1);
	    if (any) {
		if (!isNull(s)) SET_NAMED(s, 2);
		return (s);
	    }
	    return R_NilValue;
	}
    }
    /* This is where the old/new list ajustment happens. */
    for (s = ATTRIB(vec); s != R_NilValue; s = CDR(s))
	if (TAG(s) == name) {
	    if (name == R_DimNamesSymbol && TYPEOF(CAR(s)) == LISTSXP) {
		SEXP new, old;
		int i;
		new = allocVector(VECSXP, length(CAR(s)));
		old = CAR(s);
		i = 0;
		while (old != R_NilValue) {
		    SET_VECTOR_ELT(new, i++, CAR(old));
		    old = CDR(old);
		}
		SET_NAMED(new, 2);
		return new;
	    }
	    SET_NAMED(CAR(s), 2);
	    return CAR(s);
	}
    return R_NilValue;
}

SEXP setAttrib(SEXP vec, SEXP name, SEXP val)
{
    if (isString(name))
	name = install(CHAR(STRING_ELT(name, 0)));
    if (val == R_NilValue)
	return removeAttrib(vec, name);

    if (vec == R_NilValue)
	error("attempt to set an attribute on NULL");

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
    else if (name == R_TspSymbol)
	return tspgets(vec, val);
    else if (name == R_CommentSymbol)
	return commentgets(vec, val);
    else
	return installAttrib(vec, name, val);
}

/* This is called in the case of binary operations to copy */
/* most attributes from (one of) the input arguments to */
/* the output.	Note that the Dim and Names attributes */
/* should have been assigned elsewhere. */

void copyMostAttrib(SEXP inp, SEXP ans)
{
    SEXP s;
    PROTECT(ans);
    PROTECT(inp);
    for (s = ATTRIB(inp); s != R_NilValue; s = CDR(s)) {
	if ((TAG(s) != R_NamesSymbol) &&
	    (TAG(s) != R_DimSymbol) &&
	    (TAG(s) != R_DimNamesSymbol)) {
	    installAttrib(ans, TAG(s), CAR(s));
	}
    }
    SET_OBJECT(ans, OBJECT(inp));
    UNPROTECT(2);
}

static SEXP installAttrib(SEXP vec, SEXP name, SEXP val)
{
    SEXP s, t;
    if (vec == R_NilValue)
	error("attempt to set an attribute on NULL");
    PROTECT(vec);
    PROTECT(name);
    PROTECT(val);
    for (s = ATTRIB(vec); s != R_NilValue; s = CDR(s)) {
	if (TAG(s) == name) {
	    SETCAR(s, val);
	    UNPROTECT(3);
	    return val;
	}
    }
    s = allocList(1);
    SETCAR(s, val);
    SET_TAG(s, name);
    if (ATTRIB(vec) == R_NilValue)
	SET_ATTRIB(vec, s);
    else {
	t = nthcdr(ATTRIB(vec), length(ATTRIB(vec)) - 1);
	SETCDR(t, s);
    }
    UNPROTECT(3);
    return val;
}

static SEXP removeAttrib(SEXP vec, SEXP name)
{
    SEXP t;
    if (name == R_NamesSymbol && isList(vec)) {
	for (t = vec; t != R_NilValue; t = CDR(t))
	    SET_TAG(t, R_NilValue);
	return R_NilValue;
    }
    else {
	if (name == R_DimSymbol)
	    SET_ATTRIB(vec, stripAttrib(R_DimNamesSymbol, ATTRIB(vec)));
	SET_ATTRIB(vec, stripAttrib(name, ATTRIB(vec)));
	if (name == R_ClassSymbol)
	    SET_OBJECT(vec, 0);
    }
    return R_NilValue;
}

static void checkNames(SEXP x, SEXP s)
{
    if (isVector(x) || isList(x) || isLanguage(x)) {
	if (!isVector(s) && !isList(s))
	    error("invalid type for names: must be vector");
	if (length(x) != length(s))
	    error("names attribute must be the same length as the vector");
    }
    else error("names applied to non-vector");
}


/* Time Series Parameters */

static void badtsp()
{
    error("invalid time series parameters specified");
}

SEXP tspgets(SEXP vec, SEXP val)
{
    double start, end, frequency;
    int n;

    if (!isNumeric(val) || length(val) != 3)
	error("tsp attribute must be numeric of length three");

    if (isReal(val)) {
	start = REAL(val)[0];
	end = REAL(val)[1];
	frequency = REAL(val)[2];
    }
    else {
	start = (INTEGER(val)[0] == NA_INTEGER) ?
	    NA_REAL : INTEGER(val)[0];
	end = (INTEGER(val)[1] == NA_INTEGER) ?
	    NA_REAL : INTEGER(val)[1];
	frequency = (INTEGER(val)[2] == NA_INTEGER) ?
	    NA_REAL : INTEGER(val)[2];
    }
    if (frequency <= 0) badtsp();
    n = nrows(vec);
    if (n == 0) error("cannot assign `tsp' to zero-length vector");

    /* FIXME:  1.e-5 should rather be == option('ts.eps') !! */
    if (fabs(end - start - (n - 1)/frequency) > 1.e-5)
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

static SEXP commentgets(SEXP vec, SEXP comment)
{
    if (isNull(comment) || isString(comment)) {
	if (length(comment) <= 0) {
	    SET_ATTRIB(vec, stripAttrib(R_CommentSymbol, ATTRIB(vec)));
	}
	else {
	    installAttrib(vec, R_CommentSymbol, comment);
	}
	return R_NilValue;
    }
    error("attempt to set invalid comment attribute");
    return R_NilValue;/*- just for -Wall */
}

SEXP do_commentgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (NAMED(CAR(args)) == 2) SETCAR(args, duplicate(CAR(args)));
    if (length(CADR(args)) == 0) SETCADR(args, R_NilValue);
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
    if (isNull(class) || isString(class)) {
	if (length(class) <= 0) {
	    SET_ATTRIB(vec, stripAttrib(R_ClassSymbol, ATTRIB(vec)));
	    SET_OBJECT(vec, 0);
	}
	else {
	    /* When data frames where a special data type */
	    /* we had more exhaustive checks here.  Now that */
	    /* use JMCs interpreted code, we don't need this */
	    /* FIXME : The whole "classgets" may as well die. */
	    installAttrib(vec, R_ClassSymbol, class);
	    SET_OBJECT(vec, 1);
	}
	return R_NilValue;
    }
    error("attempt to set invalid class attribute");
    return R_NilValue;/*- just for -Wall */
}

SEXP do_classgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (NAMED(CAR(args)) == 2) SETCAR(args, duplicate(CAR(args)));
    if (length(CADR(args)) == 0) SETCADR(args, R_NilValue);
    setAttrib(CAR(args), R_ClassSymbol, CADR(args));
    return CAR(args);
}

SEXP do_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return getAttrib(CAR(args), R_ClassSymbol);
}

/* character elements corresponding to the syntactic types in the
   grammar */
static SEXP lang2str(SEXP obj, SEXPTYPE t)
{
  SEXP symb = CAR(obj);
  static SEXP if_sym = 0, while_sym, for_sym, eq_sym, gets_sym,
    lpar_sym, lbrace_sym, call_sym;
  if(!if_sym) {
    /* initialize:  another place for a hash table */
    if_sym = install("if");
    while_sym = install("while");
    for_sym = install("for");
    eq_sym = install("=");
    gets_sym = install("<-");
    lpar_sym = install("(");
    lbrace_sym = install("{");
    call_sym = install("call");
  }
  if(isSymbol(symb)) {
    if(symb == if_sym || symb == for_sym || symb == while_sym ||
       symb == lpar_sym || symb == lbrace_sym ||
       symb == eq_sym || symb == gets_sym)
      return PRINTNAME(symb);
  }
  return PRINTNAME(call_sym);
}

/* the S4-style class: for dispatch  required to be a single string;
   for the newClass function, keeps S3-style multiple classes. */

SEXP R_data_class(SEXP obj, int singleString)
{
    SEXP class, value; int n;
    class = getAttrib(obj, R_ClassSymbol);
    n = length(class);
    if(n == 1 || (n > 0 && !singleString))
	return(class);
    if(n == 0) {
	SEXP dim; int n;
	dim = getAttrib(obj, R_DimSymbol);
	n = length(dim);
	if(n > 0) {
	    if(n == 2)
		class = mkChar("matrix");
	    else
		class = mkChar("array");
	}
	else {
	  SEXPTYPE t = TYPEOF(obj);
	  switch(t) {
	  case CLOSXP: case SPECIALSXP: case BUILTINSXP:
	    class = mkChar("function");
	    break;
	  case REALSXP:
	    class = mkChar("numeric");
	    break;
	  case SYMSXP:
	    class = mkChar("name");
	    break;
	  case LANGSXP:
	    class = lang2str(obj, t);
	    break;
	  default:
	    class = type2str(t);
	  }
	}
    }
    else
	class = asChar(class);
    PROTECT(class);
    PROTECT(value = allocVector(STRSXP, 1));
    SET_STRING_ELT(value, 0, class);
    UNPROTECT(2);
    return value;
}

SEXP R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
  checkArity(op, args);
  return R_data_class(CAR(args), FALSE);
}

SEXP R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
  checkArity(op, args);
  return R_set_class(CAR(args), CADR(args), call);
}

/* names(object) <- name */
SEXP do_namesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (NAMED(CAR(args)) == 2)
        SETCAR(args, duplicate(CAR(args)));
    if (CADR(args) != R_NilValue) {
        PROTECT(call = allocList(2));
        SET_TYPEOF(call, LANGSXP);
        SETCAR(call, install("as.character"));
        SETCADR(call, CADR(args));
        SETCADR(args, eval(call, env));
        UNPROTECT(1);
    }
    setAttrib(CAR(args), R_NamesSymbol, CADR(args));
    return CAR(args);
}

SEXP namesgets(SEXP vec, SEXP val)
{
    int i;
    SEXP s, rval;

    PROTECT(vec);
    PROTECT(val);

    /* Ensure that the labels are indeed */
    /* a vector of character strings */

    if (isList(val)) {
	if (!isVectorizable(val))
	    error("incompatible names argument");
	else {
	    rval = allocVector(STRSXP, length(vec));
	    PROTECT(rval);
	    for (i = 0; i < length(vec); i++) {
		s = coerceVector(CAR(val), STRSXP);
		SET_STRING_ELT(rval, i, STRING_ELT(s, 0));
	    }
	    UNPROTECT(1);
	    val = rval;
	}
    } else val = coerceVector(val, STRSXP);
    UNPROTECT(1);
    PROTECT(val);

    /* Check that the lengths and types are compatible */

    checkNames(vec, val);

    /* Special treatment for one dimensional arrays */

    if (isVector(vec) || isList(vec) || isLanguage(vec)) {
	s = getAttrib(vec, R_DimSymbol);
	if (TYPEOF(s) == INTSXP && length(s) == 1) {
	    PROTECT(val = CONS(val, R_NilValue));
	    setAttrib(vec, R_DimNamesSymbol, val);
	    UNPROTECT(3);
	    return vec;
	}
    }

    /* Cons-cell based objects */

    if (isList(vec) || isLanguage(vec)) {
	i=0;
	for (s = vec; s != R_NilValue; s = CDR(s), i++)
	    if (STRING_ELT(val, i) != R_NilValue
		&& STRING_ELT(val, i) != R_NaString
		&& *CHAR(STRING_ELT(val, i)) != 0)
		SET_TAG(s, install(CHAR(STRING_ELT(val, i))));
	    else
		SET_TAG(s, R_NilValue);
    }
    else if (isVector(vec))
	installAttrib(vec, R_NamesSymbol, val);
    else
	error("invalid type to set names attribute");
    UNPROTECT(2);
    return vec;
}

SEXP do_names(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    checkArity(op, args);
    s = CAR(args);
    if (isVector(s) || isList(s) || isLanguage(s))
	return getAttrib(s, R_NamesSymbol);
    return R_NilValue;
}

SEXP do_dimnamesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    if (DispatchOrEval(call, op, "dimnames<-", args, env, &ans, 0, 0))
	return(ans);
    PROTECT(args = ans);
    checkArity(op, args);
    if (NAMED(CAR(args)) > 2) SETCAR(args, duplicate(CAR(args)));
    setAttrib(CAR(args), R_DimNamesSymbol, CADR(args));
    UNPROTECT(1);
    return CAR(args);
}

SEXP dimnamesgets(SEXP vec, SEXP val)
{
    SEXP dims, top;
    int i, k;

    PROTECT(vec);
    PROTECT(val);

    if (!isArray(vec) && !isList(vec))
	error("dimnames applied to non-array");
    /* This is probably overkill, but you never know; */
    /* there may be old pair-lists out there */
    if (!isPairList(val) && !isNewList(val))
	error("dimnames must be a list");
    dims = getAttrib(vec, R_DimSymbol);
    if ((k = LENGTH(dims)) != length(val))
	error("length of dimnames must match that of dims");
    /* Old list to new list */
    if (isList(val)) {
	SEXP newval;
	newval = allocVector(VECSXP, k);
	for (i = 0; i < k; i++) {
	    SET_VECTOR_ELT(newval, i, CAR(val));
	    val = CDR(val);
	}
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    for (i = 0; i < k; i++) {
	if (VECTOR_ELT(val, i) != R_NilValue) {
	    if (!isVector(VECTOR_ELT(val, i)))
		error("invalid type for dimname (must be a vector)");
	    if (INTEGER(dims)[i] != LENGTH(VECTOR_ELT(val, i))
		&& LENGTH(VECTOR_ELT(val, i)) != 0)
		error("length of dimnames[%d] not equal to array extent",i+1);
	    if (LENGTH(VECTOR_ELT(val, i)) == 0) {
		SET_VECTOR_ELT(val, i, R_NilValue);
	    }
	    else if (!isString(VECTOR_ELT(val, i))) {
		SET_VECTOR_ELT(val, i,
			       coerceVector(VECTOR_ELT(val, i), STRSXP));
	    }
	}
    }
    installAttrib(vec, R_DimNamesSymbol, val);
    if (isList(vec) && k == 1) {
	top = VECTOR_ELT(val, 0);
	i = 0;
	for (val = vec; !isNull(val); val = CDR(val))
	    SET_TAG(val, install(CHAR(STRING_ELT(top, i++))));
    }
    UNPROTECT(2);
    return (vec);
}

SEXP do_dimnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    if (DispatchOrEval(call, op, "dimnames", args, env, &ans, 0, 0))
	return(ans);
    PROTECT(args = ans);
    checkArity(op, args);
    ans = getAttrib(CAR(args), R_DimNamesSymbol);
    UNPROTECT(1);
    return ans;
}

SEXP do_dim(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    if (DispatchOrEval(call, op, "dim", args, env, &ans, 0, 0))
	return(ans);
    PROTECT(args = ans);
    checkArity(op, args);
    ans = getAttrib(CAR(args), R_DimSymbol);
    UNPROTECT(1);
    return ans;
}

SEXP do_dimgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    if (DispatchOrEval(call, op, "dim<-", args, env, &ans, 0, 0))
	return(ans);
    PROTECT(args = ans);
    checkArity(op, args);
    if (NAMED(CAR(args)) > 1) SETCAR(args, duplicate(CAR(args)));
    setAttrib(CAR(args), R_DimSymbol, CADR(args));
    setAttrib(CAR(args), R_NamesSymbol, R_NilValue);
    UNPROTECT(1);
    return CAR(args);
}

SEXP dimgets(SEXP vec, SEXP val)
{
    int len, ndim, i, total;
    PROTECT(vec);
    PROTECT(val);
    if (!isVector(vec) && !isList(vec))
	error("dim<- : invalid first argument");

    if (!isVector(val) && !isList(val))
	error("dim<- : invalid second argument");
    val = coerceVector(val, INTSXP);
    UNPROTECT(1);
    PROTECT(val);

    len = length(vec);
    ndim = length(val);
    if (ndim == 0)
	error("dim: Invalid dimension vector");
    total = 1;
    for (i = 0; i < ndim; i++)
	total *= INTEGER(val)[i];
    if (total != len)
	error("dim<- length of dims do not match the length of object");
    removeAttrib(vec, R_DimNamesSymbol);
    installAttrib(vec, R_DimSymbol, val);
    UNPROTECT(2);
    return vec;
}

SEXP do_attributes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP attrs, names, namesattr, value;
    int nvalues;
    namesattr = R_NilValue;
    attrs = ATTRIB(CAR(args));
    nvalues = length(attrs);
    if (isList(CAR(args))) {
	namesattr = getAttrib(CAR(args), R_NamesSymbol);
	if (namesattr != R_NilValue)
	    nvalues++;
    }
    /* FIXME */
    if (nvalues <= 0)
	return R_NilValue;
    /* FIXME */
    PROTECT(value = allocVector(VECSXP, nvalues));
    PROTECT(names = allocVector(STRSXP, nvalues));
    nvalues = 0;
    if (namesattr != R_NilValue) {
	SET_VECTOR_ELT(value, nvalues, namesattr);
	SET_STRING_ELT(names, nvalues, PRINTNAME(R_NamesSymbol));
	nvalues++;
    }
    while (attrs != R_NilValue) {
	SET_VECTOR_ELT(value, nvalues, CAR(attrs));
	if (TAG(attrs) == R_NilValue)
	    SET_STRING_ELT(names, nvalues, R_BlankString);
	else
	    SET_STRING_ELT(names, nvalues, PRINTNAME(TAG(attrs)));
	attrs = CDR(attrs);
	nvalues++;
    }
    setAttrib(value, R_NamesSymbol, names);
    SET_NAMED(value, NAMED(CAR(args)));
    UNPROTECT(2);
    return value;
}

/* attributes(object) <- attrs */
SEXP do_attributesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* NOTE: The following code ensures that when an attribute list */
/* is attached to an object, that the "dim" attibute is always */
/* brought to the front of the list.  This ensures that when both */
/* "dim" and "dimnames" are set that the "dim" is attached first. */

    SEXP object, attrs, names;
    int i, nattrs;

    /* If there are multiple references to the object being mutated, */
    /* we must duplicate so that the other references are unchanged. */

    if (NAMED(CAR(args)) == 2)
	SETCAR(args, duplicate(CAR(args)));

    /* Extract the arguments from the argument list */

    object = CAR(args);
    attrs = CADR(args);
    if (object == R_NilValue) {
	if (attrs == R_NilValue)
	    return R_NilValue;
	else
	    PROTECT(object = allocVector(VECSXP, 0));
    }
    else PROTECT(object);

    if (!isNewList(attrs))
	errorcall(call, "attributes must be in a list");

    /* Empty the existing attribute list */

    /* FIXME: the code below treats pair-based structures */
    /* in a special way.  This can probably be dropped down */
    /* the road (users should never encounter pair-based lists). */
    /* Of course, if we want backward compatibility we can't */
    /* make the change. :-( */

    if (isList(object))
	setAttrib(object, R_NamesSymbol, R_NilValue);
    SET_ATTRIB(object, R_NilValue);
    SET_OBJECT(object, 0);

    /* We do two passes through the attributes; the first */
    /* finding and transferring "dim"s and the second */
    /* transferring the rest.  This is to ensure that */
    /* "dim" occurs in the attribute list before "dimnames". */

    nattrs = length(attrs);
    if (nattrs > 0) {
	names = getAttrib(attrs, R_NamesSymbol);
	if (names == R_NilValue)
	    errorcall(call, "attributes must be named");
	for (i = 0; i < nattrs; i++) {
	    if (STRING_ELT(names, i) == R_NilValue ||
		CHAR(STRING_ELT(names, i))[0] == '\0') {
		errorcall(call, "all attributes must have names [%d]",i);
	    }
	    if (!strcmp(CHAR(STRING_ELT(names, i)), "dim"))
		setAttrib(object, R_DimSymbol, VECTOR_ELT(attrs, i));
	}
	for (i = 0; i < nattrs; i++) {
	    if (strcmp(CHAR(STRING_ELT(names, i)), "dim"))
		setAttrib(object, install(CHAR(STRING_ELT(names, i))),
			  VECTOR_ELT(attrs, i));
	}
    }
    UNPROTECT(1);
    return object;
}

/*  This code replaces an R function defined as

    attr <- function (x, which)
    {
        if (!is.character(which))
            stop("attribute name must be of mode character")
        if (length(which) != 1)
            stop("exactly one attribute name must be given")
        attributes(x)[[which]]
   }

The R functions was being called very often and replacing it by
something more efficient made a noticeable difference on several
benchmarks.  There is still some inefficiency since using getAttrib
means the attributes list will be searched twice, but this seems
fairly minor.  LT */

SEXP do_attr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tag = R_NilValue, alist;
    char *str;
    int n;
    enum { NONE, PARTIAL, FULL } match = NONE;

    s = CAR(args);
    t = CADR(args);

    if (!isString(t))
	error("attribute name must be of mode character");
    if (length(t) != 1)
	error("exactly one attribute name must be given");

    str = CHAR(STRING_ELT(t, 0));
    n = strlen(str);

    /* try to find a match among the attributes list */
    for (alist = ATTRIB(s); alist != R_NilValue; alist = CDR(alist)) {
	SEXP tmp = TAG(alist);
	if (! strncmp(CHAR(PRINTNAME(tmp)), str, n)) {
	    if (strlen(CHAR(PRINTNAME(tmp))) == n) {
		tag = tmp;
		match = FULL;
		break;
	    }
	    else if (match == PARTIAL)
		/* this match is partial and we already have a partial match,
		   so the query is ambiguous and we return R_NilValue */
		return R_NilValue;
	    else {
		tag = tmp;
		match = PARTIAL;
	    }
	}
    }

    /* unless a full match has been found, check for a "names" attribute */
    if (match != FULL && ! strncmp(CHAR(PRINTNAME(R_NamesSymbol)), str, n)) {
	if (strlen(CHAR(PRINTNAME(R_NamesSymbol))) == n) {
	    /* we have a full match on "names" */
	    tag = R_NamesSymbol;
	    match = FULL;
	}
	else if (match == NONE) {
	    /* no match on other attributes and a partial match on "names" */
	    tag = R_NamesSymbol;
	    match = PARTIAL;
	}
	else if (match == PARTIAL) {
	    /* There is a partial match on "names" and on another
	       attribute. If there really is a "names" attribute, then the
	       query is ambiguous and we return R_NilValue.  If there is no
	       "names" attribute, then the partially matched one, which is
	       the current value of tag, can be used. */
	    if (getAttrib(s, R_NamesSymbol) != R_NilValue)
		return R_NilValue;
	}
    }

    if (match == NONE)
	return R_NilValue;
    else
	return getAttrib(s, tag);
}

SEXP do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*  attr(obj, "<name>")  <-  value  */
    SEXP obj, name, value;

    obj = eval(CAR(args), env);
    if (NAMED(obj) == 2)
	PROTECT(obj = duplicate(obj));
    else
	PROTECT(obj);

    PROTECT(name = eval(CADR(args), env));
    if (!isValidString(name))
	errorcall(call, "name must be non-null character");

    /* no eval(.), RHS is already evaluated: */
    /* now it's a promise so we should eval it -RG- */
    PROTECT(value = eval(CADDR(args), env));
    setAttrib(obj, name, value);
    UNPROTECT(3);
    return obj;
}


/* These provide useful shortcuts which give access to */
/* the dimnames for matrices and arrays in a standard form. */

void GetMatrixDimnames(SEXP x, SEXP *rl, SEXP *cl, char **rn, char **cn)
{
    SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
    SEXP nn;

    if (isNull(dimnames)) {
	*rl = R_NilValue;
	*cl = R_NilValue;
	*rn = NULL;
	*cn = NULL;
    }
    else {
	*rl = VECTOR_ELT(dimnames, 0);
	*cl = VECTOR_ELT(dimnames, 1);
	nn = getAttrib(dimnames, R_NamesSymbol);
        if (isNull(nn)) {
	    *rn = NULL;
	    *cn = NULL;
        }
	else {
	    *rn = CHAR(STRING_ELT(nn, 0));
	    *cn = CHAR(STRING_ELT(nn, 1));
        }
    }
}


SEXP GetArrayDimnames(SEXP x)
{
    return getAttrib(x, R_DimNamesSymbol);
}


/* the code to manage slots in formal classes. These are attributes,
   but without partial matching and enforcing legal slot names (it's
   an error to get a slot that doesn't exist. */


static SEXP pseudo_NULL = 0;

static void init_pseudo_NULL() {
    /* create and preserve an object that is NOT R_NilValue, and is used
       to represent slots that are NULL (which an attribute can not
       be).  The point is not just to store NULL as a slot, but also to
       provide a check on invalid slot names (see get_slot below).

       The object has to be a symbol if we're going to check identity by
       just looking at referential equality. */
    pseudo_NULL = install("\001NULL\001");
}

static SEXP s_dot_Data;
static SEXP s_getDataPart;
static SEXP s_setDataPart;

static void init_slot_handling() {
    s_dot_Data = install(".Data");
    s_getDataPart = install("getDataPart");
    s_setDataPart = install("setDataPart");
}

static SEXP data_part(SEXP obj) {
    SEXP e, val;
    if(!s_getDataPart)
	init_slot_handling();
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, s_getDataPart);
    val = CDR(e);
    SETCAR(val, obj);
    val = eval(e, R_GlobalEnv);
    UNPROTECT(1);
    return(val);
}

static SEXP set_data_part(SEXP obj,  SEXP rhs) {
    SEXP e, val;
    if(!s_setDataPart)
	init_slot_handling();
    PROTECT(e = allocVector(LANGSXP, 3));
    SETCAR(e, s_setDataPart);
    val = CDR(e);
    SETCAR(val, obj);
    val = CDR(val);
    SETCAR(val, rhs);
    val = eval(e, R_GlobalEnv);
    UNPROTECT(1);
    return(val);
}

SEXP R_do_slot(SEXP obj, SEXP name) {
  /* Slots are stored as attributes to
     provide some back-compatibility
  */
    SEXP value = NULL; int nprotect = 0;
    if(!(isSymbol(name) || (isString(name) && LENGTH(name) == 1)))
	error("invalid type or length for slot name");
    if(!s_dot_Data)
	init_slot_handling();
    if(isString(name)) name = install(CHAR(STRING_ELT(name, 0)));
    if(name == s_dot_Data)
	return data_part(obj);
    value = getAttrib(obj, name);
    if(value == R_NilValue) {
	SEXP input = name;
	if(isSymbol(name) ) {
	    input = PROTECT(allocVector(STRSXP, 1));  nprotect++;
	SET_STRING_ELT(input, 0, PRINTNAME(name));
	}
 	/* not there.  But since even NULL really does get stored, this
	   implies that there is no slot of this name.  Or somebody
	   screwed up by using atttr(..) <- NULL */
	error("\"%s\" is not a valid slot for this object (or was mistakenly deleted)",
	      CHAR(asChar(input)));
    }
    else if(value == pseudo_NULL)
	value = R_NilValue;
    UNPROTECT(nprotect);
    return value;
}

SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value) {
    SEXP input = name; int nprotect = 0;
    if(isSymbol(name) ) {
	input = PROTECT(allocVector(STRSXP, 1)); nprotect++;
	SET_STRING_ELT(input, 0, PRINTNAME(name));
    }
    else if(!(isString(name) && LENGTH(name) == 1))
	error("invalid type or length for slot name");
    if(!s_dot_Data)
	init_slot_handling();
    if(isString(name)) name = install(CHAR(STRING_ELT(name, 0)));
    if(name == s_dot_Data)
	return set_data_part(obj, value);
    if(value == R_NilValue) {
	/* slots, but not attributes, can be NULL.  Store a special symbol
	   instead. */
	if(pseudo_NULL == 0)
	    init_pseudo_NULL();
	value = pseudo_NULL;
    }
    PROTECT(obj); nprotect++;
    setAttrib(obj, input, value);
    UNPROTECT(nprotect);
    return obj;
}

SEXP R_pseudo_null() {
    if(pseudo_NULL == 0)
	init_pseudo_NULL();
    return pseudo_NULL;
}


/* the @ operator, and its assignment form.  Processed much like $
   (see do_subset3) but without S3-style methods.
*/

SEXP do_AT(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  nlist, object, ans;

    nlist = CADR(args);
    PROTECT(object = eval(CAR(args), env));
    ans = R_do_slot(object, nlist);
    UNPROTECT(1);
    return ans;
}

SEXP do_AT_assign(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nlist, object, ans, value;
    PROTECT(object = eval(CAR(args), env));
    nlist = CADR(args);
    if(!(isSymbol(nlist) || isString(nlist)))
	errorcall_return(call, "invalid slot type");
    /* The code for "$<-" claims that the RHS is already evaluated, but
       this is not quite right.  It can, at the least, be a promise
       for the "@" case. */
    value = eval(CADDR(args), env);
    ans = R_do_slot_assign(object, nlist, value);
    UNPROTECT(1);
    return ans;
}

