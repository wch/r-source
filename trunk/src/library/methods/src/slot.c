
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include "methods.h"

SEXP R_get_slot(SEXP obj, SEXP name)
{
    return R_do_slot(obj, name);
}

SEXP R_set_slot(SEXP obj, SEXP name, SEXP value)
{
    return R_do_slot_assign(obj, name, value);
}



