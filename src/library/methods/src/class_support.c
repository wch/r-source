
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

SEXP R_dummy_extern_place()
{
    error("calling the C routine used as an initializer for \"externalptr\" objects");
    return R_NilValue; /* -Wall */
}

SEXP R_externalptr_prototype_object()
{
    return R_MakeExternalPtr((void *)R_dummy_extern_place, R_NilValue, 
			     R_NilValue);
}

/* internal version of paste(".", prefix, name, sep="__"), 
   for speed so few checks */
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name)
{
    SEXP ans;
    char str[201];

    if(!isString(prefix) || length(prefix) != 1 ||
        !isString(name) || length(name) != 1)
        error("methodsPackageMetaName is being abused");
    snprintf(str, 200, ".__%s__%s", CHAR(STRING_ELT(prefix, 0)),
	     CHAR(STRING_ELT(name, 0)));
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(str));
    UNPROTECT(1);
    return ans;
}
