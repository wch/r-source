/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> char here is handled as a whole string */

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

static SEXP row_names_gets(SEXP vec , SEXP val)
{
    SEXP ans;

    if(isReal(val) && length(val) == 2 && ISNAN(REAL(val)[0]) ) {
	/* This should not happen, but if a careless user dput()s a
	   data frame and sources the result, it will */
	PROTECT(val = coerceVector(val, INTSXP));
	ans =  installAttrib(vec, R_RowNamesSymbol, val);
	UNPROTECT(1);
	return ans;
    }
    if(isInteger(val)) {
	Rboolean OK_compact = TRUE;
	int i, n = LENGTH(val);
	if(n == 2 && INTEGER(val)[0] == NA_INTEGER) {
	    n = INTEGER(val)[1];
	} else if (n > 2) {
	    for(i = 0; i < n; i++)
		if(INTEGER(val)[i] != i+1) {
		    OK_compact = FALSE;
		    break;
		}
	} else OK_compact = FALSE;
	if(OK_compact) {
	    /* we hide the length in an impossible integer vector */
	    PROTECT(val = allocVector(INTSXP, 2));
	    INTEGER(val)[0] = NA_INTEGER;
	    INTEGER(val)[1] = n;
	    ans =  installAttrib(vec, R_RowNamesSymbol, val);
	    UNPROTECT(1);
	    return ans;
	}
    } else if(!isString(val))
	error(_("row names must be 'character' or 'integer', not '%s'"),
	      type2char(TYPEOF(val)));
    PROTECT(val);
    ans =  installAttrib(vec, R_RowNamesSymbol, val);
    UNPROTECT(1);
    return ans;
}

static SEXP stripAttrib(SEXP tag, SEXP lst)
{
    if(lst == R_NilValue) return lst;
    if(tag == TAG(lst)) return stripAttrib(tag, CDR(lst));
    SETCDR(lst, stripAttrib(tag, CDR(lst)));
    return lst;
}

/* NOTE: For environments serialize.c calls this function to find if
   there is a class attribute in order to reconstruct the object bit
   if needed.  This means the function cannot use OBJECT(vec) == 0 to
   conclude that the class attribute is R_NilValue.  If you want to
   rewrite this function to use such a pre-test, be sure to adjust
   serialize.c accordingly.  LT */
SEXP attribute_hidden getAttrib0(SEXP vec, SEXP name)
{
    SEXP s;
    int len, i, any;

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
		    error(_("getAttrib: invalid type (%s) for TAG"),
			  type2char(TYPEOF(TAG(vec))));
	    }
	    UNPROTECT(1);
	    if (any) {
		if (!isNull(s)) SET_NAMED(s, 2);
		return (s);
	    }
	    return R_NilValue;
	}
    }
    /* This is where the old/new list adjustment happens. */
    for (s = ATTRIB(vec); s != R_NilValue; s = CDR(s))
	if (TAG(s) == name) {
	    if (name == R_DimNamesSymbol && TYPEOF(CAR(s)) == LISTSXP) {
		SEXP _new, old;
		int i;
		_new = allocVector(VECSXP, length(CAR(s)));
		old = CAR(s);
		i = 0;
		while (old != R_NilValue) {
		    SET_VECTOR_ELT(_new, i++, CAR(old));
		    old = CDR(old);
		}
		SET_NAMED(_new, 2);
		return _new;
	    }
	    SET_NAMED(CAR(s), 2);
	    return CAR(s);
	}
    return R_NilValue;
}

SEXP getAttrib(SEXP vec, SEXP name)
{
    /* pre-test to avoid expensive operations if clearly not needed -- LT */
    if (ATTRIB(vec) == R_NilValue &&
	! (TYPEOF(vec) == LISTSXP || TYPEOF(vec) == LANGSXP))
	return R_NilValue;

    if (isString(name)) name = install(translateChar(STRING_ELT(name, 0)));

    /* special test for c(NA, n) rownames of data frames: */
    if (name == R_RowNamesSymbol) {
	SEXP s = getAttrib0(vec, R_RowNamesSymbol);
	if(isInteger(s) && LENGTH(s) == 2 && INTEGER(s)[0] == NA_INTEGER) {
	    int i, n = abs(INTEGER(s)[1]);
	    PROTECT(s = allocVector(INTSXP, n));
	    for(i = 0; i < n; i++)
		INTEGER(s)[i] = i+1;
	    UNPROTECT(1);
	}
	return s;
    } else
	return getAttrib0(vec, name);
}

SEXP R_shortRowNames(SEXP vec, SEXP stype)
{
    /* return  n if the data frame 'vec' has c(NA, n) rownames;
     *	       nrow(.) otherwise;  note that data frames with nrow(.) == 0
     *		have no row.names.
     ==> is also used in dim.data.frame() */
    SEXP s = getAttrib0(vec, R_RowNamesSymbol), ans = s;
    int type = asInteger(stype);

    if( type < 0 || type > 2)
	error(_("invalid '%s' argument"), "type");

    if(type >= 1) {
	int n;
	ans = allocVector(INTSXP, 1);
	n = (isInteger(s) && LENGTH(s) == 2 && INTEGER(s)[0] == NA_INTEGER)
	    ? INTEGER(s)[1] : (isNull(s) ? 0 : LENGTH(s));
	INTEGER(ans)[0] = (type == 1) ? n : abs(n);
    }
    return ans;
}

/* This is allowed to change 'out' */
SEXP R_copyDFattr(SEXP in, SEXP out)
{
    SET_ATTRIB(out, ATTRIB(in));
    return out;
}

/* 'name' should be 1-element STRSXP or SYMSXP */ 
SEXP setAttrib(SEXP vec, SEXP name, SEXP val)
{
    if (isString(name))
	name = install(translateChar(STRING_ELT(name, 0)));
    if (val == R_NilValue)
	return removeAttrib(vec, name);

    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

    PROTECT(vec);
    PROTECT(name);
    if (NAMED(val)) val = duplicate(val);
    SET_NAMED(val, NAMED(val) | NAMED(vec));
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
    else if (name == R_RowNamesSymbol)
	return row_names_gets(vec, val);
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

/* version that does not preserve ts information, for subsetting */
void attribute_hidden copyMostAttribNoTs(SEXP inp, SEXP ans)
{
    SEXP s;
    PROTECT(ans);
    PROTECT(inp);
    for (s = ATTRIB(inp); s != R_NilValue; s = CDR(s)) {
	if ((TAG(s) != R_NamesSymbol) &&
	    (TAG(s) != R_ClassSymbol) &&
	    (TAG(s) != R_TspSymbol) &&
	    (TAG(s) != R_DimSymbol) &&
	    (TAG(s) != R_DimNamesSymbol)) {
	    installAttrib(ans, TAG(s), CAR(s));
	} else if (TAG(s) == R_ClassSymbol) {
	    SEXP cl = CAR(s);
	    int i;
	    Rboolean ists = FALSE;
	    for (i = 0; i < LENGTH(cl); i++)
		if (strcmp(CHAR(STRING_ELT(cl, i)), "ts") == 0) { /* ASCII */
		    ists = TRUE;
		    break;
		}
	    if (!ists) installAttrib(ans, TAG(s), cl);
	    else if(LENGTH(cl) <= 1) {
	    } else {
		SEXP new_cl;
		int i, j, l = LENGTH(cl);
		PROTECT(new_cl = allocVector(STRSXP, l - 1));
		for (i = 0, j = 0; i < l; i++)
		    if (strcmp(CHAR(STRING_ELT(cl, i)), "ts")) /* ASCII */
			SET_STRING_ELT(new_cl, j++, STRING_ELT(cl, i));
		installAttrib(ans, TAG(s), new_cl);
		UNPROTECT(1);
	    }
	}
    }
    SET_OBJECT(ans, OBJECT(inp));
    UNPROTECT(2);
}

static SEXP installAttrib(SEXP vec, SEXP name, SEXP val)
{
    SEXP s, t;
    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));
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
	    error(_("invalid type (%s) for 'names': must be vector"),
		  type2char(TYPEOF(s)));
	if (length(x) != length(s))
	    error(_("'names' attribute [%d] must be the same length as the vector [%d]"), length(s), length(x));
    }
    else error(_("names() applied to a non-vector"));
}


/* Time Series Parameters */

static void badtsp()
{
    error(_("invalid time series parameters specified"));
}

SEXP tspgets(SEXP vec, SEXP val)
{
    double start, end, frequency;
    int n;

    if (!isNumeric(val) || length(val) != 3)
	error(_("'tsp' attribute must be numeric of length three"));

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
    if (n == 0) error(_("cannot assign 'tsp' to zero-length vector"));

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
    error(_("attempt to set invalid 'comment' attribute"));
    return R_NilValue;/*- just for -Wall */
}

SEXP attribute_hidden do_commentgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (NAMED(CAR(args)) == 2) SETCAR(args, duplicate(CAR(args)));
    if (length(CADR(args)) == 0) SETCADR(args, R_NilValue);
    setAttrib(CAR(args), R_CommentSymbol, CADR(args));
    return CAR(args);
}

SEXP attribute_hidden do_comment(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return getAttrib(CAR(args), R_CommentSymbol);
}

SEXP classgets(SEXP vec, SEXP klass)
{
    if (isNull(klass) || isString(klass)) {
	if (length(klass) <= 0) {
	    SET_ATTRIB(vec, stripAttrib(R_ClassSymbol, ATTRIB(vec)));
	    SET_OBJECT(vec, 0);
	}
	else {
	    /* When data frames were a special data type */
	    /* we had more exhaustive checks here.  Now that */
	    /* use JMCs interpreted code, we don't need this */
	    /* FIXME : The whole "classgets" may as well die. */

	    /* HOWEVER, it is the way that the object bit gets set/unset */

	    int i;
	    Rboolean isfactor = FALSE;
	    for(i = 0; i < length(klass); i++)
		if(streql(CHAR(STRING_ELT(klass, i)), "factor")) { /* ASCII */
		    isfactor = TRUE;
		    break;
		}
	    if(isfactor && TYPEOF(vec) != INTSXP) {
		/* we cannot coerce vec here, so just fail */
		error(_("adding class \"factor\" to an invalid object"));
	    }

	    installAttrib(vec, R_ClassSymbol, klass);
	    SET_OBJECT(vec, 1);
	}
	return R_NilValue;
    }
    error(_("attempt to set invalid 'class' attribute"));
    return R_NilValue;/*- just for -Wall */
}

/* oldClass() : */
SEXP attribute_hidden do_classgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (NAMED(CAR(args)) == 2) SETCAR(args, duplicate(CAR(args)));
    if (length(CADR(args)) == 0) SETCADR(args, R_NilValue);
    setAttrib(CAR(args), R_ClassSymbol, CADR(args));
    return CAR(args);
}

SEXP attribute_hidden do_class(SEXP call, SEXP op, SEXP args, SEXP env)
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

/* the S4-style class: for dispatch required to be a single string;
   for the new class() function;
   if(singleString) , keeps S3-style multiple classes.
   Called from the methods package, so exposed.
 */
SEXP R_data_class(SEXP obj, Rboolean singleString)
{
    SEXP value, klass = getAttrib(obj, R_ClassSymbol);
    int n = length(klass);
    if(n == 1 || (n > 0 && !singleString))
	return(klass);
    if(n == 0) {
	SEXP dim = getAttrib(obj, R_DimSymbol);
	int nd = length(dim);
	if(nd > 0) {
	    if(nd == 2)
		klass = mkChar("matrix");
	    else
		klass = mkChar("array");
	}
	else {
	  SEXPTYPE t = TYPEOF(obj);
	  switch(t) {
	  case CLOSXP: case SPECIALSXP: case BUILTINSXP:
	    klass = mkChar("function");
	    break;
	  case REALSXP:
	    klass = mkChar("numeric");
	    break;
	  case SYMSXP:
	    klass = mkChar("name");
	    break;
	  case LANGSXP:
	    klass = lang2str(obj, t);
	    break;
	  default:
	    klass = type2str(t);
	  }
	}
    }
    else
	klass = asChar(klass);
    PROTECT(klass);
    PROTECT(value = allocVector(STRSXP, 1));
    SET_STRING_ELT(value, 0, klass);
    UNPROTECT(2);
    return value;
}

/* Version for S3-dispatch */
SEXP attribute_hidden R_data_class2 (SEXP obj)
{
    SEXP klass = getAttrib(obj, R_ClassSymbol);
    if(length(klass) > 0)
	return(klass);
    else {
	SEXPTYPE t;
	SEXP value, class0 = R_NilValue, dim = getAttrib(obj, R_DimSymbol);
	int n = length(dim);
	if(n > 0) {
	    if(n == 2)
		class0 = mkChar("matrix");
	    else
		class0 = mkChar("array");
	}
	PROTECT(class0);
	switch(t = TYPEOF(obj)) {
	case CLOSXP: case SPECIALSXP: case BUILTINSXP:
	    klass = mkChar("function");
	    break;
	case INTSXP:
	case REALSXP:
	    if(isNull(class0)) {
		PROTECT(value = allocVector(STRSXP, 2));
		SET_STRING_ELT(value, 0, type2str(t));
		SET_STRING_ELT(value, 1, mkChar("numeric"));
		UNPROTECT(2);
	    }
	    else {
		PROTECT(value = allocVector(STRSXP, 3));
		SET_STRING_ELT(value, 0, class0);
		SET_STRING_ELT(value, 1, type2str(t));
		SET_STRING_ELT(value, 2, mkChar("numeric"));
		UNPROTECT(2);
	    }
	    return value;
	    break;
	case SYMSXP:
	    klass = mkChar("name");
	    break;
	case LANGSXP:
	    klass = lang2str(obj, t);
	    break;
	default:
	    klass = type2str(t);
	}
	PROTECT(klass);
	if(isNull(class0)) {
	    PROTECT(value = allocVector(STRSXP, 1));
	    SET_STRING_ELT(value, 0, klass);
	} else {
	    PROTECT(value = allocVector(STRSXP, 2));
	    SET_STRING_ELT(value, 0, class0);
	    SET_STRING_ELT(value, 1, klass);
	}
	UNPROTECT(3);
	return value;
    }
}

SEXP attribute_hidden R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
  checkArity(op, args);
  return R_data_class(CAR(args), FALSE);
}

/* names(object) <- name */
SEXP attribute_hidden do_namesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "names<-", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
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
    UNPROTECT(1);
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
	    error(_("incompatible 'names' argument"));
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

    if (length(val) < length(vec)) {
	val = lengthgets(val, length(vec));
	UNPROTECT(1);
	PROTECT(val);
    }

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
		&& *CHAR(STRING_ELT(val, i)) != 0) /* test of length */
		SET_TAG(s, install(translateChar(STRING_ELT(val, i))));
	    else
		SET_TAG(s, R_NilValue);
    }
    else if (isVector(vec))
	installAttrib(vec, R_NamesSymbol, val);
    else
	error(_("invalid type (%s) to set 'names' attribute"),
	      type2char(TYPEOF(vec)));
    UNPROTECT(2);
    return vec;
}

SEXP attribute_hidden do_names(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "names", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    ans = CAR(args);
    if (isVector(ans) || isList(ans) || isLanguage(ans))
	ans = getAttrib(ans, R_NamesSymbol);
    else ans =  R_NilValue;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_dimnamesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "dimnames<-", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    if (NAMED(CAR(args)) > 1) SETCAR(args, duplicate(CAR(args)));
    setAttrib(CAR(args), R_DimNamesSymbol, CADR(args));
    UNPROTECT(1);
    return CAR(args);
}

static SEXP dimnamesgets1(SEXP val1)
{
    SEXP this2;

    if (LENGTH(val1) == 0) return R_NilValue;
    /* if (isObject(val1)) dispatch on as.character.foo, but we don't
       have the context at this point to do so */
    if (inherits(val1, "factor")) { /* mimic as.character.factor */
	int i, n = LENGTH(val1);
	SEXP labels = getAttrib(val1, install("levels"));
	PROTECT(this2 = allocVector(STRSXP, n));
	for(i = 0; i < n; i++) {
	    SET_STRING_ELT(this2, i,
			   STRING_ELT(labels, INTEGER(val1)[i] - 1));
	}
	UNPROTECT(1);
	return this2;
    }
    if (!isString(val1)) { /* mimic as.character.default */
	PROTECT(this2 = coerceVector(val1, STRSXP));
	SET_ATTRIB(this2, R_NilValue);
	SET_OBJECT(this2, 0);
	UNPROTECT(1);
	return this2;
    }
    return val1;
}


SEXP dimnamesgets(SEXP vec, SEXP val)
{
    SEXP dims, top;
    int i, k;

    PROTECT(vec);
    PROTECT(val);

    if (!isArray(vec) && !isList(vec))
	error(_("'dimnames' applied to non-array"));
    /* This is probably overkill, but you never know; */
    /* there may be old pair-lists out there */
    if (!isPairList(val) && !isNewList(val))
	error(_("'dimnames' must be a list"));
    dims = getAttrib(vec, R_DimSymbol);
    if ((k = LENGTH(dims)) != length(val))
	error(_("length of 'dimnames' [%d] must match that of 'dims' [%d]"),
	      length(val), k);
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
	SEXP _this = VECTOR_ELT(val, i);
	if (_this != R_NilValue) {
	    if (!isVector(_this))
		error(_("invalid type (%s) for 'dimnames' (must be a vector)"),
		      type2char(TYPEOF(_this)));
	    if (INTEGER(dims)[i] != LENGTH(_this) && LENGTH(_this) != 0)
		error(_("length of 'dimnames' [%d] not equal to array extent"),
		      i+1);
	    SET_VECTOR_ELT(val, i, dimnamesgets1(_this));
	}
    }
    installAttrib(vec, R_DimNamesSymbol, val);
    if (isList(vec) && k == 1) {
	top = VECTOR_ELT(val, 0);
	i = 0;
	for (val = vec; !isNull(val); val = CDR(val))
	    SET_TAG(val, install(translateChar(STRING_ELT(top, i++))));
    }
    UNPROTECT(2);
    return (vec);
}

SEXP attribute_hidden do_dimnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "dimnames", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    ans = getAttrib(CAR(args), R_DimNamesSymbol);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_dim(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "dim", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    ans = getAttrib(CAR(args), R_DimSymbol);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_dimgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "dim<-", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
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
    if ((!isVector(vec) && !isList(vec)))
	error(_("dim<- : invalid first argument"));

    if (!isVector(val) && !isList(val))
	error(_("dim<- : invalid second argument"));
    val = coerceVector(val, INTSXP);
    UNPROTECT(1);
    PROTECT(val);

    len = length(vec);
    ndim = length(val);
    if (ndim == 0)
	error(_("dim: length-0 dimension vector is invalid"));
    total = 1;
    for (i = 0; i < ndim; i++)
	total *= INTEGER(val)[i];
    if (total != len)
	error(_("dim<- : dims [product %d] do not match the length of object [%d]"), total, len);
    removeAttrib(vec, R_DimNamesSymbol);
    installAttrib(vec, R_DimSymbol, val);
    UNPROTECT(2);
    return vec;
}

SEXP attribute_hidden do_attributes(SEXP call, SEXP op, SEXP args, SEXP env)
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
    PROTECT(namesattr);
    PROTECT(value = allocVector(VECSXP, nvalues));
    PROTECT(names = allocVector(STRSXP, nvalues));
    nvalues = 0;
    if (namesattr != R_NilValue) {
	SET_VECTOR_ELT(value, nvalues, namesattr);
	SET_STRING_ELT(names, nvalues, PRINTNAME(R_NamesSymbol));
	nvalues++;
    }
    while (attrs != R_NilValue) {
	/* treat R_RowNamesSymbol specially */
	if (TAG(attrs) == R_RowNamesSymbol)
	    SET_VECTOR_ELT(value, nvalues,
			   getAttrib(CAR(args), R_RowNamesSymbol));
	else
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
    UNPROTECT(3);
    return value;
}

SEXP attribute_hidden do_levelsgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "levels<-", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    if (NAMED(CAR(args)) > 1) SETCAR(args, duplicate(CAR(args)));
    setAttrib(CAR(args), R_LevelsSymbol, CADR(args));
    UNPROTECT(1);
    return CAR(args);
}

/* attributes(object) <- attrs */
SEXP attribute_hidden do_attributesgets(SEXP call, SEXP op, SEXP args, SEXP env)
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
	errorcall(call, _("attributes must be in a list"));

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
	    errorcall(call, _("attributes must be named"));
	for (i = 0; i < nattrs; i++) {
	    if (STRING_ELT(names, i) == R_NilValue ||
		CHAR(STRING_ELT(names, i))[0] == '\0') { /* all ASCII tests */
		errorcall(call, _("all attributes must have names [%d does not]"), i+1);
	    }
	    if (!strcmp(CHAR(STRING_ELT(names, i)), "dim"))
		setAttrib(object, R_DimSymbol, VECTOR_ELT(attrs, i));
	}
	for (i = 0; i < nattrs; i++) {
	    if (strcmp(CHAR(STRING_ELT(names, i)), "dim"))
		setAttrib(object, install(translateChar(STRING_ELT(names, i))),
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

SEXP attribute_hidden do_attr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tag = R_NilValue, alist;
    char *str;
    int n;
    enum { NONE, PARTIAL, PARTIAL2, FULL } match = NONE;

    s = CAR(args);
    t = CADR(args);

    if (!isString(t))
	error(_("attribute 'name' must be of mode character"));
    if (length(t) != 1)
	error(_("exactly one attribute 'name' must be given"));

    if(STRING_ELT(t, 0) == NA_STRING) return R_NilValue;
    str = translateChar(STRING_ELT(t, 0));
    n = strlen(str);

    /* try to find a match among the attributes list */
    for (alist = ATTRIB(s); alist != R_NilValue; alist = CDR(alist)) {
	SEXP tmp = TAG(alist);
	char *s = CHAR(PRINTNAME(tmp));
	if (! strncmp(s, str, n)) {
	    if (strlen(s) == n) {
		tag = tmp;
		match = FULL;
		break;
	    }
	    else if (match == PARTIAL || match == PARTIAL2) {
		/* this match is partial and we already have a partial match,
		   so the query is ambiguous and we will return R_NilValue
		   unless a full match comes up.
		*/
		match = PARTIAL2;
	    } else {
		tag = tmp;
		match = PARTIAL;
	    }
	}
    }
    if (match == PARTIAL2) return R_NilValue;

    /* Unless a full match has been found, check for a "names" attribute.
       This is stored via TAGs on pairlists, and via rownames on 1D arrays.
    */
    if (match != FULL && strncmp("names", str, n) == 0) {
	if (strlen("names") == n) {
	    /* we have a full match on "names" */
	    tag = R_NamesSymbol;
	    match = FULL;
	}
	else if (match == NONE) {
	    /* no match on other attributes and a partial match on "names" */
	    tag = R_NamesSymbol;
	    match = PARTIAL;
	}
	else if (match == PARTIAL && strcmp(CHAR(PRINTNAME(tag)), "names")) {
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

SEXP attribute_hidden do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*  attr(obj, "<name>")  <-  value  */
    SEXP obj, name;

    obj = CAR(args);
    if (NAMED(obj) == 2)
	PROTECT(obj = duplicate(obj));
    else
	PROTECT(obj);

    name = CADR(args);
    if (!isValidString(name) || STRING_ELT(name, 0) == NA_STRING)
	errorcall(call, _("'name' must be non-null character string"));
    setAttrib(obj, name, CADDR(args));
    UNPROTECT(1);
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
	    *rn = translateChar(STRING_ELT(nn, 0));
	    *cn = translateChar(STRING_ELT(nn, 1));
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

static SEXP s_dot_Data;
static SEXP s_getDataPart;
static SEXP s_setDataPart;

static void init_slot_handling() {
    s_dot_Data = install(".Data");
    s_getDataPart = install("getDataPart");
    s_setDataPart = install("setDataPart");
    /* create and preserve an object that is NOT R_NilValue, and is used
       to represent slots that are NULL (which an attribute can not
       be).  The point is not just to store NULL as a slot, but also to
       provide a check on invalid slot names (see get_slot below).

       The object has to be a symbol if we're going to check identity by
       just looking at referential equality. */
    pseudo_NULL = install("\001NULL\001");
}

static SEXP data_part(SEXP obj) {
    SEXP e, val;
    if(!s_getDataPart)
	init_slot_handling();
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, s_getDataPart);
    val = CDR(e);
    SETCAR(val, obj);
    val = eval(e, R_MethodsNamespace);
    UNSET_S4_OBJECT(val); /* data part must be base vector */
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
    val = eval(e, R_MethodsNamespace);
    SET_S4_OBJECT(val);
    UNPROTECT(1);
    return(val);
}

SEXP R_do_slot(SEXP obj, SEXP name) {
  /* Slots are stored as attributes to
     provide some back-compatibility
  */
    if(!(isSymbol(name) || (isString(name) && LENGTH(name) == 1)))
	error(_("invalid type or length for slot name"));
    if(!s_dot_Data)
	init_slot_handling();
    if(isString(name)) name = install(translateChar(STRING_ELT(name, 0)));
    if(name == s_dot_Data)
	return data_part(obj);
    else {
	SEXP value = getAttrib(obj, name);
	if(value == R_NilValue) {
	    SEXP input = name, classString;
	    if(isSymbol(name) ) {
		input = PROTECT(allocVector(STRSXP, 1));
		SET_STRING_ELT(input, 0, PRINTNAME(name));
		classString = GET_CLASS(obj);
		if(isNull(classString)) {
		    UNPROTECT(1);
		    error(_("cannot get a slot (\"%s\") from an object of type \"%s\""),
			  translateChar(asChar(input)),
			  CHAR(type2str(TYPEOF(obj))));
		}
	    }
	    else classString = R_NilValue; /* make sure it is initialized */
	    /* not there.  But since even NULL really does get stored, this
	       implies that there is no slot of this name.  Or somebody
	       screwed up by using attr(..) <- NULL */

	    UNPROTECT(1);
	    error(_("no slot of name \"%s\" for this object of class \"%s\""),
		  translateChar(asChar(input)),
		  translateChar(asChar(classString)));
	}
	else if(value == pseudo_NULL)
	    value = R_NilValue;
	return value;
    }
}

SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value) {
    PROTECT(obj); PROTECT(value);
				/* Ensure that name is a symbol */
    if(isString(name) && LENGTH(name) == 1)
	name = install(translateChar(STRING_ELT(name, 0)));
    if(TYPEOF(name) == CHARSXP)
	name = install(translateChar(name));
    if(!isSymbol(name) )
	error(_("invalid type or length for slot name"));

    if(!s_dot_Data)		/* initialize */
	init_slot_handling();

    if(name == s_dot_Data) {	/* special handling */
	obj = set_data_part(obj, value);
        UNPROTECT(2);
	return obj;
    }
    if(isNull(value))		/* Slots, but not attributes, can be NULL.*/
	value = pseudo_NULL;	/* Store a special symbol instead. */

    setAttrib(obj, name, value);
    UNPROTECT(2);
    return obj;
}

#ifdef UNUSED
SEXP R_pseudo_null() {
    if(pseudo_NULL == 0)
	init_slot_handling();
    return pseudo_NULL;
}
#endif


/* the @ operator, and its assignment form.  Processed much like $
   (see do_subset3) but without S3-style methods.
*/
#ifdef noSlotCheck
SEXP attribute_hidden do_AT(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  nlist, object, ans;

    nlist = CADR(args);
    PROTECT(object = eval(CAR(args), env));
    ans = R_do_slot(object, nlist);
    UNPROTECT(1);
    return ans;
}
#endif

#ifndef noSlotCheck

/* This does not get used anymore (commented out in the code below).
   Hence, comment out as well to make -Wall -pedantic happier.
   KH 2003-06-07.

static SEXP class_meta_data_env = NULL;

static int make_class_meta_data_env()
{
    class_meta_data_env = findVar(install("__ClassMetaData"), R_GlobalEnv);
    if(class_meta_data_env == R_UnboundValue) {
	class_meta_data_env = NULL;
	return 0;
    }
    else
	return 1;
}
*/

#if UNUSED
/* check for a class definition from the internal table -- will not get
 * classes whose definition has not been completed for this session,
 * so any code relying on this routine should call the S language
 * function comleteClassDefinition after a failed call. */
static Rboolean has_class_definition(SEXP class_name)
{
    /* In case we're called before initialization, try to find the
     * class metadata environment but don't insist on it. */
/*    if(class_meta_data_env || make_class_meta_data_env())
	return (findVarInFrame3(class_meta_data_env, class_name, FALSE) != R_UnboundValue);
	else */
	return FALSE;
}
#endif

static Rboolean can_test_S4Object = FALSE; /* turning this to TRUE will throw
   error or warning on all packages that have not been reinstalled for current R 2.3 */
SEXP attribute_hidden do_AT(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  nlist, object, ans, klass;

    if(!isMethodsDispatchOn())
	error(_("formal classes cannot be used without the methods package"));
    nlist = CADR(args);
    /* Do some checks here -- repeated in R_do_slot, but on repeat the
     * test expression should kick out on the first element. */
    if(!(isSymbol(nlist) || (isString(nlist) && LENGTH(nlist) == 1)))
	error(_("invalid type or length for slot name"));
    if(isString(nlist)) nlist = install(translateChar(STRING_ELT(nlist, 0)));
    PROTECT(object = eval(CAR(args), env));
    if(can_test_S4Object && !IS_S4_OBJECT(object)) {
	klass = getAttrib(object, R_ClassSymbol);
	if(length(klass) == 0)
	    error(_("trying to get slot \"%s\" from an object of a basic class (\"%s\") with no slots"),
		  CHAR(PRINTNAME(nlist)),
		  CHAR(STRING_ELT(R_data_class(object, FALSE), 0)));
	else {
	    if(isString(klass) &&
	       streql(CHAR(STRING_ELT(klass, 0)), "classRepresentation")) {
		warning("Class representations out of date--package(s) need to be reinstalled");
		can_test_S4Object = FALSE; /* turn tests off to avoid repeated warnings */
	    }
	    else
		error(_("trying to get slot \"%s\" from an object (class \"%s\") that is not an S4 object "),
		      CHAR(PRINTNAME(nlist)),
		      translateChar(STRING_ELT(klass, 0)));
	}
    }
    ans = R_do_slot(object, nlist);
    UNPROTECT(1);
    return ans;
}

#endif

#if 0
/* Was a .Primitive implementation for @<-; no longer needed? */
SEXP attribute_hidden do_AT_assign(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nlist, object, ans, value;
    PROTECT(object = eval(CAR(args), env));
    nlist = CADR(args);
    if(!(isSymbol(nlist) || isString(nlist)))
	errorcall_return(call, _("invalid slot type"));
    /* The code for "$<-" claims that the RHS is already evaluated, but
       this is not quite right.  It can, at the least, be a promise
       for the "@" case. */
    value = eval(CADDR(args), env);
    ans = R_do_slot_assign(object, nlist, value);
    UNPROTECT(1);
    return ans;
}
#endif
