
#include <R.h>
#include <Rdefines.h>
#define NewEnvironment		Rf_NewEnvironment
#define substitute		Rf_substitute
#include "methods.h"

extern SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho);

/* substitute in an _evaluated_ object, with an explicit list as
   second arg (although old-style lists and environments are allowed).

*/

SEXP do_substitute_direct(SEXP f, SEXP env)
{
    SEXP s;
    if (TYPEOF(env) == VECSXP)
	env = NewEnvironment(R_NilValue, VectorToPairList(env), R_BaseEnv);
    else if (TYPEOF(env) == LISTSXP)
	env = NewEnvironment(R_NilValue, duplicate(env), R_BaseEnv);
    if(TYPEOF(env) != ENVSXP)
	error(_("invalid list for substitution"));
    PROTECT(env);
    PROTECT(f);
    s = substitute(f, env);
    UNPROTECT(2);
    return(s);
}

