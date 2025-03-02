/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2025  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

static SEXP checkArgIsSymbol(SEXP x) {
    if (TYPEOF(x) != SYMSXP)
	error("argument must be a symbol");
    return x;
}

/* .Internal(lapply(X, FUN)) */

/* This is a special .Internal, so has unevaluated arguments.  It is
   called from a closure wrapper, so X and FUN will be symbols that
   are bound to promises in rho.

   FUN must be unevaluated for use in e.g. bquote .
*/
attribute_hidden SEXP do_lapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX pidx, cidx;

    checkArity(op, args);
    SEXP X, XX, FUN;
    X = checkArgIsSymbol(CAR(args));
    XX = PROTECT(eval(CAR(args), rho));
    R_xlen_t n = xlength(XX);  // a vector, so will be valid.
    FUN = checkArgIsSymbol(CADR(args));
    bool realIndx = n > INT_MAX;

    SEXP ans = PROTECT(allocVector(VECSXP, n));
    SEXP names = getAttrib(XX, R_NamesSymbol);
    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);

    /* Build call: FUN(X[[<ind>]], ...) */
    SEXP isym = install("i");
    SEXP tmp = PROTECT(lang3(R_Bracket2Symbol, X, isym));
    SEXP R_fcall = PROTECT(lang3(FUN, tmp, R_DotsSymbol));
    MARK_NOT_MUTABLE(R_fcall);

    /* Create the loop index variable and value */
    SEXP ind = allocVector(realIndx ? REALSXP : INTSXP, 1);
    PROTECT_WITH_INDEX(ind, &pidx);
    defineVar(isym, ind, rho);
    INCREMENT_NAMED(ind);
    R_varloc_t loc = R_findVarLocInFrame(rho, isym);
    PROTECT_WITH_INDEX(loc.cell, &cidx);


    for(R_xlen_t i = 0; i < n; i++) {
	if (realIndx) REAL(ind)[0] = (double)(i + 1);
	else INTEGER(ind)[0] = (int)(i + 1);
	tmp = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(tmp)) tmp = lazy_duplicate(tmp);
	SET_VECTOR_ELT(ans, i, tmp);
	if (ind != R_GetVarLocValue(loc) || MAYBE_SHARED(ind)) {
	    /* ind has been captured or removed by FUN so fix it up */
	    REPROTECT(ind = duplicate(ind), pidx);
	    defineVar(isym, ind, rho);
	    INCREMENT_NAMED(ind);
	    loc = R_findVarLocInFrame(rho, isym);
	    REPROTECT(loc.cell, cidx);
	}
    }

    UNPROTECT(6);
    return ans;
}

/* .Internal(vapply(X, FUN, FUN.VALUE, USE.NAMES)) */

/* This is a special .Internal */
attribute_hidden SEXP do_vapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, ans, names = R_NilValue, rowNames = R_NilValue,
	X, XX, FUN, value, dim_v;
    R_xlen_t i, n;
    int commonLen;
    int useNames, rnk_v = -1; // = array_rank(value) := length(dim(value))
    bool array_value;
    SEXPTYPE commonType;
    PROTECT_INDEX index = 0; /* initialize to avoid a warning */

    checkArity(op, args);
    PROTECT(X = CAR(args));
    PROTECT(XX = eval(CAR(args), rho));
    FUN = CADR(args);  /* must be unevaluated for use in e.g. bquote */
    PROTECT(value = eval(CADDR(args), rho));
    if (!isVector(value)) error(_("'FUN.VALUE' must be a vector"));
    useNames = asLogical(PROTECT(eval(CADDDR(args), rho)));
    UNPROTECT(1);
    // FIXME: does not protect against length > 1
    if (useNames == NA_LOGICAL) error(_("invalid '%s' value"), "USE.NAMES");

    n = xlength(XX);
    if (n == NA_INTEGER) error(_("invalid length"));
    bool realIndx = n > INT_MAX;

    commonLen = length(value);
    if (commonLen > 1 && n > INT_MAX)
	error(_("long vectors are not supported for matrix/array results"));
    commonType = TYPEOF(value);
    // check once here
    if (commonType != CPLXSXP && commonType != REALSXP &&
	commonType != INTSXP  && commonType != LGLSXP &&
	commonType != RAWSXP  && commonType != STRSXP &&
	commonType != VECSXP)
	error(_("type '%s' is not supported"), R_typeToChar(value));
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

	SEXP isym = install("i");
	PROTECT(ind = allocVector(realIndx ? REALSXP : INTSXP, 1));
	defineVar(isym, ind, rho);
	INCREMENT_NAMED(ind);

	/* Notice that it is OK to have one arg to LCONS do memory
	   allocation and not PROTECT the result (LCONS does memory
	   protection of its args internally), but not both of them,
	   since the computation of one may destroy the other */
	PROTECT(tmp = LCONS(R_Bracket2Symbol,
			    LCONS(X, LCONS(isym, R_NilValue))));
	PROTECT(R_fcall = LCONS(FUN,
				LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

	int common_len_offset = 0;
	for(i = 0; i < n; i++) {
	    SEXP val; SEXPTYPE valType;
	    PROTECT_INDEX indx;
	    if (realIndx) REAL(ind)[0] = (double)(i + 1);
	    else INTEGER(ind)[0] = (int)(i + 1);
	    val = R_forceAndCall(R_fcall, 1, rho);
	    if (MAYBE_REFERENCED(val))
		val = lazy_duplicate(val); // Need to duplicate? Copying again anyway
	    PROTECT_WITH_INDEX(val, &indx);
	    if (length(val) != commonLen)
		error(_("values must be length %d,\n but FUN(X[[%lld]]) result is length %d"),
		       commonLen, (long long)i+1, length(val));
	    valType = TYPEOF(val);
	    if (valType != commonType) {
		bool okay = false;
		switch (commonType) {
		case CPLXSXP: okay = (valType == REALSXP) || (valType == INTSXP)
				    || (valType == LGLSXP); break;
		case REALSXP: okay = (valType == INTSXP) || (valType == LGLSXP); break;
		case INTSXP:  okay = (valType == LGLSXP); break;
		}
		if (!okay)
		    error(_("values must be type '%s',\n but FUN(X[[%lld]]) result is type '%s'"),
			  R_typeToChar(value), (long long)i+1, R_typeToChar(val));
		REPROTECT(val = coerceVector(val, commonType), indx);
	    }
	    /* Take row names from the first result only */
	    if (i == 0 && useNames && isNull(rowNames))
		REPROTECT(rowNames = getAttrib(val,
					       array_value ? R_DimNamesSymbol : R_NamesSymbol),
			  index);
	    // two cases - only for efficiency
	    if(commonLen == 1) { // common case
		switch (commonType) {
		case CPLXSXP: COMPLEX(ans)[i] = COMPLEX(val)[0]; break;
		case REALSXP: REAL(ans)   [i] = REAL   (val)[0]; break;
		case INTSXP:  INTEGER(ans)[i] = INTEGER(val)[0]; break;
		case LGLSXP:  LOGICAL(ans)[i] = LOGICAL(val)[0]; break;
		case RAWSXP:  RAW(ans)    [i] = RAW    (val)[0]; break;
		case STRSXP:  SET_STRING_ELT(ans, i, STRING_ELT(val, 0)); break;
		case VECSXP:  SET_VECTOR_ELT(ans, i, VECTOR_ELT(val, 0)); break;
		}
	    } else if (commonLen) { // commonLen > 1
		switch (commonType) {
		case REALSXP:
		    memcpy(REAL(ans) + common_len_offset,
			   REAL(val), commonLen * sizeof(double)); break;
		case INTSXP:
		    memcpy(INTEGER(ans) + common_len_offset,
			   INTEGER(val), commonLen * sizeof(int)); break;
		case LGLSXP:
		    memcpy(LOGICAL(ans) + common_len_offset,
			   LOGICAL(val), commonLen * sizeof(int)); break;
		case RAWSXP:
		    memcpy(RAW(ans) + common_len_offset,
			   RAW(val), commonLen * sizeof(Rbyte)); break;
		case CPLXSXP:
		    memcpy(COMPLEX(ans) + common_len_offset,
			   COMPLEX(val), commonLen * sizeof(Rcomplex)); break;
		case STRSXP:
		    for (int j = 0; j < commonLen; j++)
			SET_STRING_ELT(ans, common_len_offset + j, STRING_ELT(val, j));
		    break;
		case VECSXP:
		    for (int j = 0; j < commonLen; j++)
			SET_VECTOR_ELT(ans, common_len_offset + j, VECTOR_ELT(val, j));
		    break;
		}
		common_len_offset += commonLen;
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
	UNPROTECT(2); /* names and rowNames */
    }
    UNPROTECT(4); /* X, XX, value, ans */
    return ans;
}

//  Apply FUN() to X recursively;  workhorse of rapply()
static SEXP do_one(SEXP X, SEXP FUN, SEXP classes, SEXP deflt,
		   bool replace, SEXP rho)
{
    SEXP ans, names, klass;
    bool matched = false;

    /* if X is a list, recurse.  Otherwise if it matches classes call f */
    if(X == R_NilValue || isVectorList(X)) {
	R_xlen_t n = xlength(X);
	if (replace) {
	    PROTECT(ans = shallow_duplicate(X));
	} else {
	    PROTECT(ans = allocVector(VECSXP, n));
	    names = getAttrib(X, R_NamesSymbol);
	    if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
	}
	for(R_xlen_t i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes,
					  deflt, replace, rho));
	UNPROTECT(1);
	return ans;
    }
    if(strcmp(CHAR(STRING_ELT(classes, 0)), "ANY") == 0) /* ASCII */
	matched = true;
    else {
	PROTECT(klass = R_data_class(X, false));
	for(int i = 0; i < LENGTH(klass); i++)
	    for(int j = 0; j < length(classes); j++)
		if(Seql(STRING_ELT(klass, i), STRING_ELT(classes, j)))
		    matched = true;
	UNPROTECT(1);
    }
    if(matched) {
	/* This stores value to which the function is to be applied in
	   a variable X in the environment of the rapply closure call
	   that calls into the rapply .Internal. */
	SEXP R_fcall; /* could allocate once and preserve for re-use */
	SEXP Xsym = install("X");
	defineVar(Xsym, X, rho);
	INCREMENT_NAMED(X);
	/* PROTECT(R_fcall = lang2(FUN, Xsym)); */
	PROTECT(R_fcall = lang3(FUN, Xsym, R_DotsSymbol));
	ans = R_forceAndCall(R_fcall, 1, rho);
	if (MAYBE_REFERENCED(ans))
	    ans = lazy_duplicate(ans);
	UNPROTECT(1);
	return(ans);
    } else if(replace) return lazy_duplicate(X);
    else return lazy_duplicate(deflt);
}

attribute_hidden SEXP do_rapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP X, FUN, classes, deflt, how, ans;

    checkArity(op, args);
    X = CAR(args); args = CDR(args);
    if(!isVectorList(X))
	error(_("'%s' must be a list or expression"), "object");
    FUN = CAR(args); args = CDR(args);
    if(!isFunction(FUN)) error(_("invalid '%s' argument"), "f");
    classes = CAR(args); args = CDR(args);
    if(!isString(classes)) error(_("invalid '%s' argument"), "classes");
    deflt = CAR(args); args = CDR(args);
    how = CAR(args);
    if(!isString(how)) error(_("invalid '%s' argument"), "how");
    bool replace = strcmp(CHAR(STRING_ELT(how, 0)), "replace") == 0; /* ASCII */
    R_xlen_t n = xlength(X);
    if (replace) {
      PROTECT(ans = shallow_duplicate(X));
    } else {
      PROTECT(ans = allocVector(VECSXP, n));
      SEXP names = getAttrib(X, R_NamesSymbol);
      if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
    }
    for(R_xlen_t i = 0; i < n; i++)
	SET_VECTOR_ELT(ans, i, do_one(VECTOR_ELT(X, i), FUN, classes, deflt,
				      replace, rho));
    UNPROTECT(1);
    return ans;
}

/**
 * Recursively check if  X  is a tree with only factor leaves;
 *   the workhorse for do_islistfactor()
 * @param X  list or expression
 * @return TRUE(1), FALSE(0) or NA_LOGICAL
 */
static int islistfactor(SEXP X)
{
    switch(TYPEOF(X)) {
    case VECSXP:
    case EXPRSXP: {
	int n = LENGTH(X), ans = NA_LOGICAL;
	for(int i = 0; i < n; i++) {
	    int isLF = islistfactor(VECTOR_ELT(X, i));
	    if(!isLF)
		return false;
	    else if(isLF == true)
		ans = true;
	    // else isLF is NA
	}
	return ans;
    }
    default:
	return isFactor(X);
    }
}


/* is this a tree with only factor leaves? */
// currently only called from unlist()
attribute_hidden SEXP do_islistfactor(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP X = CAR(args);
    bool recursive = asBool2(CADR(args), call);
    int n = length(X);
    if(n == 0 || !isVectorList(X))
	return ScalarLogical(false);

    if(!recursive) {
	for(int i = 0; i < n; i++)
	    if(!isFactor(VECTOR_ELT(X, i)))
		return ScalarLogical(false);

	return ScalarLogical(true);
    }
    else { // recursive:  isVectorList(X) <==> X is VECSXP or EXPRSXP
	return ScalarLogical((islistfactor(X) == true) ? true : false);
    }
}
