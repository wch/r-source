
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* slot management (in attrib.c) */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP check, SEXP value);

SEXP R_get_slot(SEXP obj, SEXP name)
{
    return R_do_slot(obj, name);
}

SEXP R_set_slot(SEXP obj, SEXP name, SEXP check, SEXP value)
{
    return R_do_slot_assign(obj, name, check, value);
}



