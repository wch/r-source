
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "methods.h"

SEXP R_dummy_extern_place()
{
    error(_("calling the C routine used as an initializer for 'externalptr' objects"));
    return R_NilValue; /* -Wall */
}

SEXP R_externalptr_prototype_object()
{
    return R_MakeExternalPtr((void *)R_dummy_extern_place, R_NilValue, 
			     R_NilValue);
}

