/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2021   The R Core Team.
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

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Visibility.h>

#include "utils.h"

static R_INLINE SEXP checkArgCountPop(SEXP args, int n)
{
    args = CDR(args);
    if (length(args) != n)
	error("wrong argument count");
    return args;
}

static int HT_TypeFromString(SEXP x)
{
    if (TYPEOF(x) != STRSXP || XLENGTH(x) != 1)
	error("hash table type must be a scalar string");
    const char *s = CHAR(STRING_ELT(x, 0));
    if (strcmp(s, "identical") == 0)
	return HT_TYPE_IDENTICAL;
    else if (strcmp(s, "address") == 0)
	return HT_TYPE_ADDRESS;
    else
	error("hash table type '%s' is not supported", s);
}

SEXP hashtab_Ext(SEXP args)
{
    args = checkArgCountPop(args, 2);
    int type = HT_TypeFromString(CAR(args));
    int K = asInteger(CADR(args));
    SEXP val = PROTECT(allocVector(VECSXP, 1));
    SET_VECTOR_ELT(val, 0, R_HashtabSEXP(R_mkhashtab(type, K)));
    setAttrib(val, R_ClassSymbol, mkString("hashtab"));
    UNPROTECT(1); /* val */
    return val;
}

attribute_hidden
SEXP gethash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 3);
    R_hashtab_type h = R_asHashtable(CAR(args));
    SEXP key = CADR(args);
    SEXP nomatch = CADDR(args);
    return R_gethash(h, key, nomatch);
}

attribute_hidden
SEXP sethash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 3);
    R_hashtab_type h = R_asHashtable(CAR(args));
    SEXP key = CADR(args);
    SEXP value = CADDR(args);
    return R_sethash(h, key, value);
}

attribute_hidden
SEXP remhash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 2);
    R_hashtab_type h = R_asHashtable(CAR(args));
    SEXP key = CADR(args);
    return ScalarLogical(R_remhash(h, key));
}

attribute_hidden
SEXP numhash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 1);
    R_hashtab_type h = R_asHashtable(CAR(args));
    return ScalarInteger(R_numhash(h));
}

attribute_hidden
SEXP typhash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 1);
    R_hashtab_type h = R_asHashtable(CAR(args));
    switch(R_typhash(h)) {
    case HT_TYPE_IDENTICAL: return mkString("identical");
    case HT_TYPE_ADDRESS  : return mkString("address");
    default: error("bad hash table type");
    }
}

attribute_hidden
SEXP maphash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 2);
    R_hashtab_type h = R_asHashtable(CAR(args));
    SEXP FUN = CADR(args);
    return R_maphash(h, FUN);
}

attribute_hidden
SEXP clrhash_Ext(SEXP args)
{
    args = checkArgCountPop(args, 1);
    R_hashtab_type h = R_asHashtable(CAR(args));
    R_clrhash(h);
    return R_NilValue;
}

attribute_hidden
SEXP ishashtab_Ext(SEXP args)
{
    args = checkArgCountPop(args, 1);
    return ScalarLogical(R_isHashtable(CAR(args)));
}
