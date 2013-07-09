/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>

static SEXP installAttrib(SEXP, SEXP, SEXP);
static SEXP removeAttrib(SEXP, SEXP);

SEXP comment(SEXP);
static SEXP commentgets(SEXP, SEXP);

static SEXP row_names_gets(SEXP vec , SEXP val)
{
    SEXP ans;

    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

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

/* used in removeAttrib, commentgets and classgets */
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
    if(TYPEOF(vec) == CHARSXP)
	error("cannot have attributes on a CHARSXP");
    /* pre-test to avoid expensive operations if clearly not needed -- LT */
    if (ATTRIB(vec) == R_NilValue &&
	! (TYPEOF(vec) == LISTSXP || TYPEOF(vec) == LANGSXP))
	return R_NilValue;

    if (isString(name)) name = installTrChar(STRING_ELT(name, 0));

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

attribute_hidden
SEXP do_shortRowNames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* return  n if the data frame 'vec' has c(NA, n) rownames;
     *	       nrow(.) otherwise;  note that data frames with nrow(.) == 0
     *		have no row.names.
     ==> is also used in dim.data.frame() */

    checkArity(op, args);
    SEXP s = getAttrib0(CAR(args), R_RowNamesSymbol), ans = s;
    int type = asInteger(CADR(args));

    if( type < 0 || type > 2)
	error(_("invalid '%s' argument"), "type");

    if(type >= 1) {
	int n = (isInteger(s) && LENGTH(s) == 2 && INTEGER(s)[0] == NA_INTEGER)
	    ? INTEGER(s)[1] : (isNull(s) ? 0 : LENGTH(s));
	ans = ScalarInteger((type == 1) ? n : abs(n));
    }
    return ans;
}

/* This is allowed to change 'out' */
attribute_hidden
SEXP do_copyDFattr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP in = CAR(args), out = CADR(args);
    SET_ATTRIB(out, ATTRIB(in));
    IS_S4_OBJECT(in) ?  SET_S4_OBJECT(out) : UNSET_S4_OBJECT(out);
    SET_OBJECT(out, OBJECT(in));
    return out;
}


/* 'name' should be 1-element STRSXP or SYMSXP */
SEXP setAttrib(SEXP vec, SEXP name, SEXP val)
{
    PROTECT(vec);
    PROTECT(name);

    if (isString(name)) {
	name = installTrChar(STRING_ELT(name, 0));
    }
    if (val == R_NilValue) {
	UNPROTECT(2);
	return removeAttrib(vec, name);
    }

    /* We allow attempting to remove names from NULL */
    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

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

    if (ans == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

    PROTECT(ans);
    PROTECT(inp);
    for (s = ATTRIB(inp); s != R_NilValue; s = CDR(s)) {
	if ((TAG(s) != R_NamesSymbol) &&
	    (TAG(s) != R_DimSymbol) &&
	    (TAG(s) != R_DimNamesSymbol)) {
	    installAttrib(ans, TAG(s), CAR(s));
	}
    }
    if (OBJECT(inp)) SET_OBJECT(ans, 1);
    IS_S4_OBJECT(inp) ?  SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
    UNPROTECT(2);
}

/* version that does not preserve ts information, for subsetting */
void copyMostAttribNoTs(SEXP inp, SEXP ans)
{
    SEXP s;

    if (ans == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

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
    IS_S4_OBJECT(inp) ?  SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
    UNPROTECT(2);
}

/* Tweaks here based in part on PR#14934 */
static SEXP installAttrib(SEXP vec, SEXP name, SEXP val)
{
    SEXP t = R_NilValue; /* -Wall */

    if(TYPEOF(vec) == CHARSXP)
	error("cannot set attribute on a CHARSXP");
    /* this does no allocation */
    for (SEXP s = ATTRIB(vec); s != R_NilValue; s = CDR(s)) {
	if (TAG(s) == name) {
	    SETCAR(s, val);
	    return val;
	}
	t = s; // record last attribute, if any
    }
    /* The usual convention is that the caller protects, 
       so this is historical over-cautiousness */
    PROTECT(vec); PROTECT(name); PROTECT(val);
    SEXP s = CONS(val, R_NilValue);
    SET_TAG(s, name);
    if (ATTRIB(vec) == R_NilValue) SET_ATTRIB(vec, s); else SETCDR(t, s);
    UNPROTECT(3);
    return val;
}

static SEXP removeAttrib(SEXP vec, SEXP name)
{
    SEXP t;
    if(TYPEOF(vec) == CHARSXP)
	error("cannot set attribute on a CHARSXP");
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
	if (xlength(x) != xlength(s))
	    error(_("'names' attribute [%d] must be the same length as the vector [%d]"), length(s), length(x));
    }
    else if(IS_S4_OBJECT(x)) {
      /* leave validity checks to S4 code */
    }
    else error(_("names() applied to a non-vector"));
}


/* Time Series Parameters */

static void badtsp(void)
{
    error(_("invalid time series parameters specified"));
}

attribute_hidden
SEXP tspgets(SEXP vec, SEXP val)
{
    double start, end, frequency;
    int n;

    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

    if(IS_S4_OBJECT(vec)) { /* leave validity checking to validObject */
        if (!isNumeric(val)) /* but should have been checked */
	    error(_("'tsp' attribute must be numeric"));
	installAttrib(vec, R_TspSymbol, val);
	return vec;
    }

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
    if (vec == R_NilValue)
	error(_("attempt to set an attribute on NULL"));

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
    SET_NAMED(CAR(args), 0);
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

	    if (vec == R_NilValue)
		error(_("attempt to set an attribute on NULL"));

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

/* oldClass<-(), primitive */
SEXP attribute_hidden do_classgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    if (NAMED(CAR(args)) == 2) SETCAR(args, duplicate(CAR(args)));
    if (length(CADR(args)) == 0) SETCADR(args, R_NilValue);
    if(IS_S4_OBJECT(CAR(args)))
      UNSET_S4_OBJECT(CAR(args));
    setAttrib(CAR(args), R_ClassSymbol, CADR(args));
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}

/* oldClass, primitive */
SEXP attribute_hidden do_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "x");
    SEXP x = CAR(args), s3class;
    if(IS_S4_OBJECT(x)) {
      if((s3class = S3Class(x)) != R_NilValue) {
	return s3class;
      }
    } /* else */
    return getAttrib(x, R_ClassSymbol);
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
   if(!singleString) , keeps S3-style multiple classes.
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
    value = ScalarString(klass);
    UNPROTECT(1);
    return value;
}

static SEXP s_dot_S3Class = 0;

static SEXP R_S4_extends_table = 0;

 
static SEXP cache_class(const char *class, SEXP klass) {
    if(!R_S4_extends_table) {
	R_S4_extends_table = R_NewHashedEnv(R_NilValue, ScalarInteger(0));
	R_PreserveObject(R_S4_extends_table);
    }
    if(isNull(klass)) { /* retrieve cached value */
	SEXP val;
	val = findVarInFrame(R_S4_extends_table, install(class));
	return (val == R_UnboundValue) ? klass : val;
    }
    defineVar(install(class), klass, R_S4_extends_table);
    return klass;
}

static SEXP S4_extends(SEXP klass) 
{
    static SEXP s_extends = 0, s_extendsForS3;
    SEXP e, val; const char *class;
    const void *vmax = vmaxget();
    if(!s_extends) {
	s_extends = install("extends");
	s_extendsForS3 = install(".extendsForS3");
	R_S4_extends_table = R_NewHashedEnv(R_NilValue, ScalarInteger(0));
	R_PreserveObject(R_S4_extends_table);
    }
    /* sanity check for methods package available */
    if(findVar(s_extends, R_GlobalEnv) == R_UnboundValue)
        return klass;
    class = translateChar(STRING_ELT(klass, 0)); /* TODO: include package attr. */
    val = findVarInFrame(R_S4_extends_table, install(class));
    vmaxset(vmax);
    if(val != R_UnboundValue)
       return val;
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, s_extendsForS3);
    val = CDR(e);
    SETCAR(val, klass);
    val = eval(e, R_MethodsNamespace);
    cache_class(class, val);
    UNPROTECT(1);
    return(val);
}

/* Version for S3-dispatch */
SEXP attribute_hidden R_data_class2 (SEXP obj)
{
    SEXP klass = getAttrib(obj, R_ClassSymbol);
      if(length(klass) > 0) {
	if(IS_S4_OBJECT(obj))
	    return S4_extends(klass);
	else
	    return klass;
      }
      else { /* length(klass) == 0 */
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
	    value = ScalarString(klass);
	} else {
	    value = allocVector(STRSXP, 2);
	    SET_STRING_ELT(value, 0, class0);
	    SET_STRING_ELT(value, 1, klass);
	}
	UNPROTECT(2);
	return value;
    }
}

/* class() : */
SEXP attribute_hidden R_do_data_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
  checkArity(op, args);
  if(PRIMVAL(op) == 1) {
      const char *class; SEXP klass;
      check1arg(args, call, "class");
      klass = CAR(args);
      if(TYPEOF(klass) != STRSXP || LENGTH(klass) < 1)
	  error("invalid class argument to internal .class_cache");
      class = translateChar(STRING_ELT(klass, 0));
      return cache_class(class, CADR(args));
  }
  check1arg(args, call, "x");
  return R_data_class(CAR(args), FALSE);
}

/* names(object) <- name */
SEXP attribute_hidden do_namesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "names<-", args, env, &ans, 0, 1))
	return(ans);
    /* Special case: removing non-existent names, to avoid a copy */
    if (CADR(args) == R_NilValue &&
	getAttrib(CAR(args), R_NamesSymbol) == R_NilValue)
	return CAR(args);
    PROTECT(args = ans);
    if (NAMED(CAR(args)) == 2)
	SETCAR(args, duplicate(CAR(args)));
    if(IS_S4_OBJECT(CAR(args))) {
	const char *klass = CHAR(STRING_ELT(R_data_class(CAR(args), FALSE), 0));
	if(getAttrib(CAR(args), R_NamesSymbol) == R_NilValue) {
	    /* S4 class w/o a names slot or attribute */
	    if(TYPEOF(CAR(args)) == S4SXP)
		error(_("class '%s' has no 'names' slot"), klass);
	    else
		warning(_("class '%s' has no 'names' slot; assigning a names attribute will create an invalid object"), klass);
	}
	else if(TYPEOF(CAR(args)) == S4SXP)
	    error(_("invalid to use names()<- to set the 'names' slot in a non-vector class ('%s')"), klass);
	/* else, go ahead, but can't check validity of replacement*/
    }
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
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}

SEXP namesgets(SEXP vec, SEXP val)
{
    int i;
    SEXP s, rval, tval;

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
	    /* See PR#10807 */
	    for (i = 0, tval = val;
		 i < length(vec) && tval != R_NilValue;
		 i++, tval = CDR(tval)) {
		s = coerceVector(CAR(tval), STRSXP);
		SET_STRING_ELT(rval, i, STRING_ELT(s, 0));
	    }
	    UNPROTECT(1);
	    val = rval;
	}
    } else val = coerceVector(val, STRSXP);
    UNPROTECT(1);
    PROTECT(val);

    /* Check that the lengths and types are compatible */

    if (xlength(val) < xlength(vec)) {
	val = xlengthgets(val, xlength(vec));
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

    if (isList(vec) || isLanguage(vec)) {
	/* Cons-cell based objects */
	i = 0;
	for (s = vec; s != R_NilValue; s = CDR(s), i++)
	    if (STRING_ELT(val, i) != R_NilValue
		&& STRING_ELT(val, i) != R_NaString
		&& *CHAR(STRING_ELT(val, i)) != 0) /* test of length */
		SET_TAG(s, installTrChar(STRING_ELT(val, i)));
	    else
		SET_TAG(s, R_NilValue);
    }
    else if (isVector(vec) || IS_S4_OBJECT(vec))
	/* Normal case */
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
    check1arg(args, call, "x");
    if (DispatchOrEval(call, op, "names", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    ans = CAR(args);
    if (isVector(ans) || isList(ans) || isLanguage(ans) ||
	IS_S4_OBJECT(ans))
	ans = getAttrib(ans, R_NamesSymbol);
    else ans =  R_NilValue;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_dimnamesgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "dimnames<-", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    if (NAMED(CAR(args)) > 1) SETCAR(args, duplicate(CAR(args)));
    setAttrib(CAR(args), R_DimNamesSymbol, CADR(args));
    UNPROTECT(1);
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}

static SEXP dimnamesgets1(SEXP val1)
{
    SEXP this2;

    if (LENGTH(val1) == 0) return R_NilValue;
    /* if (isObject(val1)) dispatch on as.character.foo, but we don't
       have the context at this point to do so */

    if (inherits(val1, "factor"))  /* mimic as.character.factor */
        return asCharacterFactor(val1);

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
    SEXP dims, top, newval;
    int i, k;

    PROTECT(vec);
    PROTECT(val);

    if (!isArray(vec) && !isList(vec))
	error(_("'dimnames' applied to non-array"));
    /* This is probably overkill, but you never know; */
    /* there may be old pair-lists out there */
    /* There are, when this gets used as names<- for 1-d arrays */
    if (!isPairList(val) && !isNewList(val))
	error(_("'dimnames' must be a list"));
    dims = getAttrib(vec, R_DimSymbol);
    if ((k = LENGTH(dims)) < length(val))
	error(_("length of 'dimnames' [%d] must match that of 'dims' [%d]"),
	      length(val), k);
    if (length(val) == 0) {
	removeAttrib(vec, R_DimNamesSymbol);
	UNPROTECT(2);
	return vec;
    }
    /* Old list to new list */
    if (isList(val)) {
	newval = allocVector(VECSXP, k);
	for (i = 0; i < k; i++) {
	    SET_VECTOR_ELT(newval, i, CAR(val));
	    val = CDR(val);
	}
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    if (length(val) > 0 && length(val) < k) {
	newval = lengthgets(val, k);
	UNPROTECT(1);
	PROTECT(val = newval);
    }
    if (k != length(val))
	error(_("length of 'dimnames' [%d] must match that of 'dims' [%d]"),
	      length(val), k);
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
	    SET_TAG(val, installTrChar(STRING_ELT(top, i++)));
    }
    UNPROTECT(2);
    return vec;
}

SEXP attribute_hidden do_dimnames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    check1arg(args, call, "x");
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
    check1arg(args, call, "x");
    if (DispatchOrEval(call, op, "dim", args, env, &ans, 0, 1))
	return(ans);
    PROTECT(args = ans);
    ans = getAttrib(CAR(args), R_DimSymbol);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_dimgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x;
    checkArity(op, args);
    if (DispatchOrEval(call, op, "dim<-", args, env, &ans, 0, 1))
	return(ans);
    x = CAR(args);
    /* Duplication might be expensive */
    if (CADR(args) == R_NilValue) {
	SEXP s;
	for (s = ATTRIB(x); s != R_NilValue; s = CDR(s))
	    if (TAG(s) == R_DimSymbol || TAG(s) == R_NamesSymbol) break;
	if (s == R_NilValue) return x;
    }
    PROTECT(args = ans);
    if (NAMED(x) > 1) SETCAR(args, x = duplicate(x));
    setAttrib(x, R_DimSymbol, CADR(args));
    setAttrib(x, R_NamesSymbol, R_NilValue);
    UNPROTECT(1);
    SET_NAMED(x, 0);
    return x;
}

SEXP dimgets(SEXP vec, SEXP val)
{
    int i, ndim;
    R_xlen_t len, total;
    PROTECT(vec);
    PROTECT(val);
    if ((!isVector(vec) && !isList(vec)))
	error(_("invalid first argument"));

    if (!isVector(val) && !isList(val))
	error(_("invalid second argument"));
    val = coerceVector(val, INTSXP);
    UNPROTECT(1);
    PROTECT(val);

    len = xlength(vec);
    ndim = length(val);
    if (ndim == 0)
	error(_("length-0 dimension vector is invalid"));
    total = 1;
    for (i = 0; i < ndim; i++) {
	/* need this test first as NA_INTEGER is < 0 */
	if (INTEGER(val)[i] == NA_INTEGER)
	    error(_("the dims contain missing values"));
	if (INTEGER(val)[i] < 0)
	    error(_("the dims contain negative values"));
	total *= INTEGER(val)[i];
    }
    if (total != len) {
	if (total > INT_MAX || len > INT_MAX)
	    error(_("dims do not match the length of object"), total, len);
	else
	    error(_("dims [product %d] do not match the length of object [%d]"), total, len);
    }
    removeAttrib(vec, R_DimNamesSymbol);
    installAttrib(vec, R_DimSymbol, val);
    UNPROTECT(2);
    return vec;
}

SEXP attribute_hidden do_attributes(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP attrs, names, namesattr, value;
    int nvalues;

    checkArity(op, args);
    check1arg(args, call, "x");
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
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "levels<-", args, env, &ans, 0, 1))
	/* calls, e.g., levels<-.factor() */
	return(ans);
    if(!isNull(CADR(args)) && any_duplicated(CADR(args), FALSE))
	warningcall(call, "duplicated levels in factors are deprecated");
/* TODO errorcall(call, _("duplicated levels are not allowed in factors anymore")); */
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

    SEXP object, attrs, names = R_NilValue /* -Wall */;
    int i, i0 = -1, nattrs;

    /* Extract the arguments from the argument list */

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
    attrs = CADR(args);

    /* Do checks before duplication */
    if (!isNewList(attrs))
	error(_("attributes must be a list or NULL"));
    nattrs = length(attrs);
    if (nattrs > 0) {
	names = getAttrib(attrs, R_NamesSymbol);
	if (names == R_NilValue)
	    error(_("attributes must be named"));
	for (i = 1; i < nattrs; i++) {
	    if (STRING_ELT(names, i) == R_NilValue ||
		CHAR(STRING_ELT(names, i))[0] == '\0') { /* all ASCII tests */
		error(_("all attributes must have names [%d does not]"), i+1);
	    }
	}
    }

    if (object == R_NilValue) {
	if (attrs == R_NilValue)
	    return R_NilValue;
	else
	    PROTECT(object = allocVector(VECSXP, 0));
    } else {
	/* Unlikely to have NAMED == 0 here.
	   As from R 2.7.0 we don't optimize NAMED == 1 _if_ we are
	   setting any attributes as an error later on would leave
	   'obj' changed */
	if (NAMED(object) > 1 || (NAMED(object) == 1 && nattrs))
	    object = duplicate(object);
	PROTECT(object);
    }


    /* Empty the existing attribute list */

    /* FIXME: the code below treats pair-based structures */
    /* in a special way.  This can probably be dropped down */
    /* the road (users should never encounter pair-based lists). */
    /* Of course, if we want backward compatibility we can't */
    /* make the change. :-( */

    if (isList(object))
	setAttrib(object, R_NamesSymbol, R_NilValue);
    SET_ATTRIB(object, R_NilValue);
    /* We have just removed the class, but might reset it later */
    SET_OBJECT(object, 0);
    /* Probably need to fix up S4 bit in other cases, but
       definitely in this one */
    if(nattrs == 0) UNSET_S4_OBJECT(object);

    /* We do two passes through the attributes; the first */
    /* finding and transferring "dim" and the second */
    /* transferring the rest.  This is to ensure that */
    /* "dim" occurs in the attribute list before "dimnames". */

    if (nattrs > 0) {
	for (i = 0; i < nattrs; i++) {
	    if (!strcmp(CHAR(STRING_ELT(names, i)), "dim")) {
		i0 = i;
		setAttrib(object, R_DimSymbol, VECTOR_ELT(attrs, i));
		break;
	    }
	}
	for (i = 0; i < nattrs; i++) {
	    if (i == i0) continue;
	    setAttrib(object, installTrChar(STRING_ELT(names, i)),
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
    SEXP ap, argList, s, t, tag = R_NilValue, alist, ans;
    const char *str;
    int nargs = length(args), exact = 0;
    enum { NONE, PARTIAL, PARTIAL2, FULL } match = NONE;

    if (nargs < 2 || nargs > 3)
	errorcall(call, "either 2 or 3 arguments are required");

    /* argument matching */
    PROTECT(ap = list3(R_NilValue, R_NilValue, R_NilValue));
    SET_TAG(ap,  install("x"));
    SET_TAG(CDR(ap), install("which"));
    SET_TAG(CDDR(ap), install("exact"));
    argList = matchArgs(ap, args, call);
    UNPROTECT(1); /* ap */
    PROTECT(argList);
    s = CAR(argList);
    t = CADR(argList);
    if (!isString(t))
	errorcall(call, _("'which' must be of mode character"));
    if (length(t) != 1)
	errorcall(call, _("exactly one attribute 'which' must be given"));

    if(nargs == 3) {
	exact = asLogical(CADDR(args));
	if(exact == NA_LOGICAL) exact = 0;
    }


    if(STRING_ELT(t, 0) == NA_STRING) {
	UNPROTECT(1);
	return R_NilValue;
    }
    str = translateChar(STRING_ELT(t, 0));
    size_t n = strlen(str);

    /* try to find a match among the attributes list */
    for (alist = ATTRIB(s); alist != R_NilValue; alist = CDR(alist)) {
	SEXP tmp = TAG(alist);
	const char *s = CHAR(PRINTNAME(tmp));
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
    if (match == PARTIAL2) {
	UNPROTECT(1);
	return R_NilValue;
    }

    /* Unless a full match has been found, check for a "names" attribute.
       This is stored via TAGs on pairlists, and via rownames on 1D arrays.
    */
    if (match != FULL && strncmp("names", str, n) == 0) {
	if (strlen("names") == n) {
	    /* we have a full match on "names", if there is such an
	       attribute */
	    tag = R_NamesSymbol;
	    match = FULL;
	}
	else if (match == NONE && !exact) {
	    /* no match on other attributes and a possible
	       partial match on "names" */
	    tag = R_NamesSymbol;
	    t = getAttrib(s, tag);
	    if(t != R_NilValue && R_warn_partial_match_attr)
		warningcall(call, _("partial match of '%s' to '%s'"), str,
			    CHAR(PRINTNAME(tag)));
	    UNPROTECT(1);
	    return t;
	}
	else if (match == PARTIAL && strcmp(CHAR(PRINTNAME(tag)), "names")) {
	    /* There is a possible partial match on "names" and on another
	       attribute. If there really is a "names" attribute, then the
	       query is ambiguous and we return R_NilValue.  If there is no
	       "names" attribute, then the partially matched one, which is
	       the current value of tag, can be used. */
	    if (getAttrib(s, R_NamesSymbol) != R_NilValue) {
		UNPROTECT(1);
		return R_NilValue;
	    }
	}
    }

    if (match == NONE  || (exact && match != FULL)) {
	UNPROTECT(1);
	return R_NilValue;
    }
    if (match == PARTIAL && R_warn_partial_match_attr)
	warningcall(call, _("partial match of '%s' to '%s'"), str,
		    CHAR(PRINTNAME(tag)));

    ans =  getAttrib(s, tag);
    UNPROTECT(1);
    return ans;
}

static void check_slot_assign(SEXP obj, SEXP input, SEXP value, SEXP env) 
{
    SEXP valueClass, objClass, e;

    valueClass = PROTECT(R_data_class(value, FALSE));
    objClass = PROTECT(R_data_class(obj, FALSE));
    e = PROTECT(lang4(install("checkAtAssignment"), 
		      objClass, input, valueClass));
    eval(e, env);
    UNPROTECT(3);
}


SEXP attribute_hidden do_attrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*  attr(x, which = "<name>")  <-  value  */
    SEXP obj, name, ap, argList;

    checkArity(op, args);

    if(PRIMVAL(op)) { /* @<- */
	SEXP input, nlist, ans, value;
	PROTECT(input = allocVector(STRSXP, 1));

	nlist = CADR(args);
	if (isSymbol(nlist))
	    SET_STRING_ELT(input, 0, PRINTNAME(nlist));
	else if(isString(nlist) )
	    SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
	else {
	    error(_("invalid type '%s' for slot name"), 
		  type2char(TYPEOF(nlist)));
	    return R_NilValue; /*-Wall*/
	}

	/* replace the second argument with a string */
	SETCADR(args, input);
	UNPROTECT(1); // 'input' is now protected

 	if(DispatchOrEval(call, op, "@<-", args, env, &ans, 0, 0))
	    return(ans);

	PROTECT(obj = CAR(ans));
	PROTECT(value = CADDR(ans));
	check_slot_assign(obj, input, value, env);
	value = R_do_slot_assign(obj, input, value);
	UNPROTECT(2);
	return value;
    }


    obj = CAR(args);
    if (NAMED(obj) == 2)
	PROTECT(obj = duplicate(obj));
    else
	PROTECT(obj);

    /* argument matching */
    PROTECT(ap = list3(R_NilValue, R_NilValue, R_NilValue));
    SET_TAG(ap,  install("x"));
    SET_TAG(CDR(ap), install("which"));
    SET_TAG(CDDR(ap), install("value"));
    argList = matchArgs(ap, args, call);
    UNPROTECT(1); /* ap */
    PROTECT(argList);

    name = CADR(argList);
    if (!isValidString(name) || STRING_ELT(name, 0) == NA_STRING)
	error(_("'name' must be non-null character string"));
    /* TODO?  if (isFactor(obj) && !strcmp(asChar(name), "levels"))
     * ---         if(any_duplicated(CADDR(args)))
     *                  error(.....)
     */
    setAttrib(obj, name, CADDR(args));
    UNPROTECT(2);
    SET_NAMED(obj, 0);
    return obj;
}


/* These provide useful shortcuts which give access to */
/* the dimnames for matrices and arrays in a standard form. */

/* NB: this may return R_alloc-ed rn and dn */
void GetMatrixDimnames(SEXP x, SEXP *rl, SEXP *cl,
		       const char **rn, const char **cn)
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

static void init_slot_handling(void) {
    s_dot_Data = install(".Data");
    s_dot_S3Class = install(".S3Class");
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

SEXP S3Class(SEXP obj)
{
    if(!s_dot_S3Class) init_slot_handling();
    return getAttrib(obj, s_dot_S3Class);
}

/* Slots are stored as attributes to
   provide some back-compatibility
*/

/**
 * R_has_slot() : a C-level test if a obj@<name> is available;
 *                as R_do_slot() gives an error when there's no such slot.
 */
int R_has_slot(SEXP obj, SEXP name) {

#define R_SLOT_INIT							\
    if(!(isSymbol(name) || (isString(name) && LENGTH(name) == 1)))	\
	error(_("invalid type or length for slot name"));		\
    if(!s_dot_Data)							\
	init_slot_handling();						\
    if(isString(name)) name = install(CHAR(STRING_ELT(name, 0)))

    R_SLOT_INIT;
    if(name == s_dot_Data && TYPEOF(obj) != S4SXP)
	return(1);
    /* else */
    return(getAttrib(obj, name) != R_NilValue);
}

/* the @ operator, and its assignment form.  Processed much like $
   (see do_subset3) but without S3-style methods.
*/
/* currently, R_get_slot() ["methods"] is a trivial wrapper for this: */
SEXP R_do_slot(SEXP obj, SEXP name) {
    R_SLOT_INIT;
    if(name == s_dot_Data)
	return data_part(obj);
    else {
	SEXP value = getAttrib(obj, name);
	if(value == R_NilValue) {
	    SEXP input = name, classString;
	    if(name == s_dot_S3Class) /* defaults to class(obj) */
	        return R_data_class(obj, FALSE);
	    else if(name == R_NamesSymbol &&
		    TYPEOF(obj) == VECSXP) /* needed for namedList class */
	        return value;
	    if(isSymbol(name) ) {
		input = PROTECT(ScalarString(PRINTNAME(name)));
		classString = getAttrib(obj, R_ClassSymbol);
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
#undef R_SLOT_INIT

/* currently, R_set_slot() ["methods"] is a trivial wrapper for this: */
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value) {
#ifndef _R_ver_le_2_11_x_
    if (isNull(obj))/* cannot use !IS_S4_OBJECT(obj), because
		     *  slot(obj, name, check=FALSE) <- value  must work on
		     * "pre-objects", currently only in makePrototypeFromClassDef() */
	error(_("attempt to set slot on NULL object"));
#endif
    PROTECT(obj); PROTECT(value);
    /* Ensure that name is a symbol */
    if(isString(name) && LENGTH(name) == 1)
	name = installTrChar(STRING_ELT(name, 0));
    if(TYPEOF(name) == CHARSXP)
	name = installTrChar(name);
    if(!isSymbol(name) )
	error(_("invalid type or length for slot name"));

    if(!s_dot_Data)		/* initialize */
	init_slot_handling();

    if(name == s_dot_Data) {	/* special handling */
	obj = set_data_part(obj, value);
    } else {
	if(isNull(value))		/* Slots, but not attributes, can be NULL.*/
	    value = pseudo_NULL;	/* Store a special symbol instead. */
	    
#ifdef _R_ver_le_2_11_x_
	setAttrib(obj, name, value);
#else
	/* simplified version of setAttrib(obj, name, value);
	   here we do *not* treat "names", "dimnames", "dim", .. specially : */
	PROTECT(name);
	if (NAMED(value)) value = duplicate(value);
	SET_NAMED(value, NAMED(value) | NAMED(obj));
	UNPROTECT(1);
	installAttrib(obj, name, value);
#endif
    }
    UNPROTECT(2);
    return obj;
}

SEXP attribute_hidden do_AT(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  nlist, object, ans, klass;

    if(!isMethodsDispatchOn())
	error(_("formal classes cannot be used without the 'methods' package"));
    nlist = CADR(args);
    /* Do some checks here -- repeated in R_do_slot, but on repeat the
     * test expression should kick out on the first element. */
    if(!(isSymbol(nlist) || (isString(nlist) && LENGTH(nlist) == 1)))
	error(_("invalid type or length for slot name"));
    if(isString(nlist)) nlist = installTrChar(STRING_ELT(nlist, 0));
    PROTECT(object = eval(CAR(args), env));
    if(!s_dot_Data) init_slot_handling();
    if(nlist != s_dot_Data && !IS_S4_OBJECT(object)) {
	klass = getAttrib(object, R_ClassSymbol);
	if(length(klass) == 0)
	    error(_("trying to get slot \"%s\" from an object of a basic class (\"%s\") with no slots"),
		  CHAR(PRINTNAME(nlist)),
		  CHAR(STRING_ELT(R_data_class(object, FALSE), 0)));
	else
	    error(_("trying to get slot \"%s\" from an object (class \"%s\") that is not an S4 object "),
		  CHAR(PRINTNAME(nlist)),
		  translateChar(STRING_ELT(klass, 0)));
    }

    ans = R_do_slot(object, nlist);
    UNPROTECT(1);
    return ans;
}

/* Return a suitable S3 object (OK, the name of the routine comes from
   an earlier version and isn't quite accurate.) If there is a .S3Class
   slot convert to that S3 class.
   Otherwise, unless type == S4SXP, look for a .Data or .xData slot.  The
   value of type controls what's wanted.  If it is S4SXP, then ONLY
   .S3class is used.  If it is ANYSXP, don't check except that automatic
   conversion from the current type only applies for classes that extend
   one of the basic types (i.e., not S4SXP).  For all other types, the
   recovered data must match the type.
   Because S3 objects can't have type S4SXP, .S3Class slot is not searched
   for in that type object, unless ONLY that class is wanted.
   (Obviously, this is another routine that has accumulated barnacles and
   should at some time be broken into separate parts.)
*/
SEXP attribute_hidden
R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
  static SEXP s_xData, s_dotData; SEXP value = R_NilValue;
  if(!s_xData) {
    s_xData = install(".xData");
    s_dotData = install(".Data");
  }
  if(TYPEOF(obj) != S4SXP || type == S4SXP) {
    SEXP s3class = S3Class(obj);
    if(s3class == R_NilValue && type == S4SXP)
      return R_NilValue;
    PROTECT(s3class);
    if(NAMED(obj)) obj = duplicate(obj);
    UNPROTECT(1);
    if(s3class != R_NilValue) {/* replace class with S3 class */
      setAttrib(obj, R_ClassSymbol, s3class);
      setAttrib(obj, s_dot_S3Class, R_NilValue); /* not in the S3 class */
    }
    else { /* to avoid inf. recursion, must unset class attribute */
      setAttrib(obj, R_ClassSymbol, R_NilValue);
    }
    UNSET_S4_OBJECT(obj);
    if(type == S4SXP)
      return obj;
    value = obj;
  }
  else
      value = getAttrib(obj, s_dotData);
  if(value == R_NilValue)
      value = getAttrib(obj, s_xData);
/* the mechanism for extending abnormal types.  In the future, would b
   good to consolidate under the ".Data" slot, but this has
   been used to mean S4 objects with non-S4 type, so for now
   a secondary slot name, ".xData" is used to avoid confusion
*/  if(value != R_NilValue &&
     (type == ANYSXP || type == TYPEOF(value)))
     return value;
  else
     return R_NilValue;
}
