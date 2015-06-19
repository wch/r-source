/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2013  The R Core Team
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

/* .Internal(lapply(X, FUN)) */

/* This is a special .Internal, so has unevaluated arguments.  It is
   called from a closure wrapper, so X and FUN are promises. 

   FUN must be unevaluated for use in e.g. bquote .
*/
SEXP attribute_hidden do_lapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX px;

    checkArity(op, args);
    SEXP X, XX, FUN;
    PROTECT_WITH_INDEX(X =CAR(args), &px);
    XX = PROTECT(eval(CAR(args), rho));
    R_xlen_t n = xlength(XX);  // a vector, so will be valid.
    FUN = CADR(args);
    Rboolean realIndx = n > INT_MAX;

    SEXP ans = PROTECT(allocVector(VECSXP, n));
    SEXP names = getAttrib(XX, R_NamesSymbol);
    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);

    /* Build call: FUN(XX[[<ind>]], ...) */

    /* Notice that it is OK to have one arg to LCONS do memory
       allocation and not PROTECT the result (LCONS does memory
       protection of its args internally), but not both of them,
       since the computation of one may destroy the other */
    
    SEXP ind = PROTECT(allocVector(realIndx ? REALSXP : INTSXP, 1));
    SEXP tmp;
    /* The R level code has ensured that XX is a vector.
       If it is atomic we can speed things up slightly by
       using the evaluated version.
    */
    if(isVectorAtomic(XX))
	tmp = PROTECT(tmp = LCONS(R_Bracket2Symbol,
				  LCONS(XX, LCONS(ind, R_NilValue))));
    else
	tmp = PROTECT(LCONS(R_Bracket2Symbol,
			    LCONS(X, LCONS(ind, R_NilValue))));
    SEXP R_fcall = PROTECT(LCONS(FUN,
				 LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

    for(R_xlen_t i = 0; i < n; i++) {
	if (realIndx) REAL(ind)[0] = (double)(i + 1);
	else INTEGER(ind)[0] = (int)(i + 1);
	tmp = eval(R_fcall, rho);
	if (NAMED(tmp)) tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
    }

    UNPROTECT(6);
    return ans;
}

/* .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES)) */

/* This is a special .Internal */
SEXP attribute_hidden do_vapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, ans, names = R_NilValue, rowNames = R_NilValue,
	X, XX, FUN, value, dim_v;
    R_xlen_t i, n;
    int commonLen;
    int useNames, rnk_v = -1; // = array_rank(value) := length(dim(value))
    Rboolean array_value;
    SEXPTYPE commonType;
    PROTECT_INDEX index = 0; /* initialize to avoid a warning */

    checkArity(op, args);
    PROTECT(X = CAR(args));
    PROTECT(XX = eval(CAR(args), rho));
    FUN = CADR(args);  /* must be unevaluated for use in e.g. bquote */
    PROTECT(value = eval(CADDR(args), rho));
    if (!isVector(value)) error(_("'FUN.VALUE' must be a vector"));
    useNames = asLogical(eval(CADDDR(args), rho));
    if (useNames == NA_LOGICAL) error(_("invalid '%s' value"), "USE.NAMES");

    n = xlength(XX);
    if (n == NA_INTEGER) error(_("invalid length"));
    Rboolean realIndx = n > INT_MAX;

    commonLen = length(value);
    if (commonLen > 1 && n > INT_MAX)
	error(_("long vectors are not supported for matrix/array results"));
    commonType = TYPEOF(value);
    dim_v = getAttrib(value, R_DimSymbol);
    array_value = (TYPEOF(dim_v) == INTSXP && LENGTH(dim_v) >= 1);
    PROTECT(ans = allocVector(commonType, n*commonLen));
    if (useNames) {
    	PROTECT(names = getAttrib(XX, R_NamesSymbol));
    	if (isNull(names) && TYPEOF(XX) == STRSXP) {
    	    UNPROTECT(1);
    	    PROTECT(names = XX);
    	}
    	PROTECT_WITH_INDEX(rowNames = getAttrib(value,
						array_value ? R_DimNamesSymbol
						: R_NamesSymbol),
			   &index);
    }
    /* The R level code has ensured that XX is a vector.
       If it is atomic we can speed things up slightly by
       using the evaluated version.
    */
    {
	SEXP ind, tmp;
	/* Build call: FUN(XX[[<ind>]], ...) */

	/* Notice that it is OK to have one arg to LCONS do memory
	   allocation and not PROTECT the result (LCONS does memory
	   protection of its args internally), but not both of them,
	   since the computation of one may destroy the other */

	PROTECT(ind = allocVector(INTSXP, 1));
	if(isVectorAtomic(XX))
	    PROTECT(tmp = LCONS(R_Bracket2Symbol,
				LCONS(XX, LCONS(ind, R_NilValue))));
	else
	    PROTECT(tmp = LCONS(R_Bracket2Symbol,
				LCONS(X, LCONS(ind, R_NilValue))));
	PROTECT(R_fcall = LCONS(FUN,
				LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

	for(i = 0; i < n; i++) {
	    SEXP val; SEXPTYPE valType;
	    PROTECT_INDEX indx;
	    if (realIndx) REAL(ind)[0] = (double)(i + 1);
	    else INTEGER(ind)[0] = (int)(i + 1);
	    val = eval(R_fcall, rho);
	    if (NAMED(val))
		val = lazy_duplicate(val);
	    PROTECT_WITH_INDEX(val, &indx);
	    if (length(val) != commonLen)
	    	error(_("values must be length %d,\n but FUN(X[[%d]]) result is length %d"),
	               commonLen, i+1, length(val));
	    valType = TYPEOF(val);
	    if (valType != commonType) {
	    	Rboolean okay = FALSE;
	    	switch (commonType) {
	    	case CPLXSXP: okay = (valType == REALSXP) || (valType == INTSXP)
	    	                    || (valType == LGLSXP); break;
	    	case REALSXP: okay = (valType == INTSXP) || (valType == LGLSXP); break;
	    	case INTSXP:  okay = (valType == LGLSXP); break;
	        }
	        if (!okay)
	            error(_("values must be type '%s',\n but FUN(X[[%d]]) result is type '%s'"),
	            	  type2char(commonType), i+1, type2char(valType));
	        REPROTECT(val = coerceVector(val, commonType), indx);
	    }
	    /* Take row names from the first result only */
	    if (i == 0 && useNames && isNull(rowNames))
	    	REPROTECT(rowNames = getAttrib(val,
					       array_value ? R_DimNamesSymbol : R_NamesSymbol),
			  index);
	    for (int j = 0; j < commonLen; j++) {
	    	switch (commonType) {
	    	case CPLXSXP: COMPLEX(ans)[i*commonLen + j] = COMPLEX(val)[j]; break;
	    	case REALSXP: REAL(ans)[i*commonLen + j] = REAL(val)[j]; break;
	    	case INTSXP:  INTEGER(ans)[i*commonLen + j] = INTEGER(val)[j]; break;
	    	case LGLSXP:  LOGICAL(ans)[i*commonLen + j] = LOGICAL(val)[j]; break;
	    	case RAWSXP:  RAW(ans)[i*commonLen + j] = RAW(val)[j]; break;
	    	case STRSXP:  SET_STRING_ELT(ans, i*commonLen + j, STRING_ELT(val, j)); break;
	    	case VECSXP:  SET_VECTOR_ELT(ans, i*commonLen + j, VECTOR_ELT(val, j)); break;
	    	default:
	    	    error(_("type '%s' is not supported"), type2char(commonType));
	    	}
	    }
	    UNPROTECT(1);
	}
	UNPROTECT(3);
    }

    if (commonLen != 1) {
	SEXP dim;
	rnk_v = array_value ? LENGTH(dim_v) : 1;
	PROTECT(dim = allocVector(INTSXP, rnk_v+1));
	if(array_value)
	    for(int j = 0; j < rnk_v; j++)
		INTEGER(dim)[j] = INTEGER(dim_v)[j];
	else
	    INTEGER(dim)[0] = commonLen;
	INTEGER(dim)[rnk_v] = (int) n;  // checked above
	setAttrib(ans, R_DimSymbol, dim);
	UNPROTECT(1);
    }

    if (useNames) {
	if (commonLen == 1) {
	    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
	} else {
	    if (!isNull(names) || !isNull(rowNames)) {
		SEXP dimnames;
		PROTECT(dimnames = allocVector(VECSXP, rnk_v+1));
		if(array_value && !isNull(rowNames)) {
		    if(TYPEOF(rowNames) != VECSXP || LENGTH(rowNames) != rnk_v)
			// should never happen ..
			error(_("dimnames(<value>) is neither NULL nor list of length %d"),
			      rnk_v);
		    for(int j = 0; j < rnk_v; j++)
			SET_VECTOR_ELT(dimnames, j, VECTOR_ELT(rowNames, j));
		} else
		    SET_VECTOR_ELT(dimnames, 0, rowNames);

		SET_VECTOR_ELT(dimnames, rnk_v, names);
		setAttrib(ans, R_DimNamesSymbol, dimnames);
		UNPROTECT(1);
	    }
	}
    }
    UNPROTECT(useNames ? 6 : 4); /* X, XX, value, ans, and maybe names and rowNames */
    return ans;
}

static SEXP do_one(SEXP X, SEXP FUN, SEXP classes, SEXP deflt,
		   Rboolean replace, SEXP rho)
{
    SEXP ans, names, klass, R_fcall;
    int i, j, n;
    Rboolean matched = FALSE;

    /* if X is a list, recurse.  Otherwise if it matches classes call f */
    if(isNewList(X)) {
	n = length(X);
	PROTECT(ans = allocVector(VECSXP, n));
	names = getAttrib(X, R_NamesSymbol);
	/* or copy attributes if replace = TRUE? */
	if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
	for(i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes,
					  deflt, replace, rho));
	UNPROTECT(1);
	return ans;
    }
    if(strcmp(CHAR(STRING_ELT(classes, 0)), "ANY") == 0) /* ASCII */
	matched = TRUE;
    else {
	PROTECT(klass = R_data_class(X, FALSE));
	for(i = 0; i < LENGTH(klass); i++)
	    for(j = 0; j < length(classes); j++)
		if(Seql(STRING_ELT(klass, i), STRING_ELT(classes, j)))
		    matched = TRUE;
	UNPROTECT(1);
    }
    if(matched) {
	/* PROTECT(R_fcall = lang2(FUN, X)); */
	PROTECT(R_fcall = lang3(FUN, X, R_DotsSymbol));
	ans = eval(R_fcall, rho);
	if (NAMED(ans))
	    ans = lazy_duplicate(ans);
	UNPROTECT(1);
	return(ans);
    } else if(replace) return lazy_duplicate(X);
    else return lazy_duplicate(deflt);
}

SEXP attribute_hidden do_rapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP X, FUN, classes, deflt, how, ans, names;
    int i, n;
    Rboolean replace;

    checkArity(op, args);
    X = CAR(args); args = CDR(args);
    FUN = CAR(args); args = CDR(args);
    if(!isFunction(FUN)) error(_("invalid '%s' argument"), "f");
    classes = CAR(args); args = CDR(args);
    if(!isString(classes)) error(_("invalid '%s' argument"), "classes");
    deflt = CAR(args); args = CDR(args);
    how = CAR(args);
    if(!isString(how)) error(_("invalid '%s' argument"), "how");
    replace = strcmp(CHAR(STRING_ELT(how, 0)), "replace") == 0; /* ASCII */
    n = length(X);
    PROTECT(ans = allocVector(VECSXP, n));
    names = getAttrib(X, R_NamesSymbol);
    /* or copy attributes if replace = TRUE? */
    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
    for(i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes, deflt,
				      replace, rho));
    UNPROTECT(1);
    return ans;
}

static Rboolean islistfactor(SEXP X)
{
    int i, n = length(X);

    if(n == 0) return FALSE;
    switch(TYPEOF(X)) {
    case VECSXP:
    case EXPRSXP:
	for(i = 0; i < LENGTH(X); i++)
	    if(!islistfactor(VECTOR_ELT(X, i))) return FALSE;
	return TRUE;
	break;
    }
    return isFactor(X);
}


/* is this a tree with only factor leaves? */

SEXP attribute_hidden do_islistfactor(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP X;
    Rboolean lans = TRUE, recursive;
    int i, n;

    checkArity(op, args);
    X = CAR(args);
    recursive = asLogical(CADR(args));
    n = length(X);
    if(n == 0 || !isVectorList(X)) {
	lans = FALSE;
	goto do_ans;
    }
    if(!recursive) {
    for(i = 0; i < LENGTH(X); i++)
	if(!isFactor(VECTOR_ELT(X, i))) {
	    lans = FALSE;
	    break;
	}
    } else {
	switch(TYPEOF(X)) {
	case VECSXP:
	case EXPRSXP:
	    break;
	default:
	    goto do_ans;
	}
	for(i = 0; i < LENGTH(X); i++)
	    if(!islistfactor(VECTOR_ELT(X, i))) {
		lans = FALSE;
		break;
	    }
    }
do_ans:
    return ScalarLogical(lans);
}
