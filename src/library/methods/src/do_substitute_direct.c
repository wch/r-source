
#include <R.h>
#include <Rdefines.h>
#define findVarInFrame		Rf_findVarInFrame
#define NewEnvironment		Rf_NewEnvironment
#define substitute		Rf_substitute

extern SEXP findVarInFrame(SEXP rho, SEXP symbol);
extern SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho);

static SEXP substituteDirectList(SEXP el, SEXP rho);
static SEXP substituteDirect(SEXP lang, SEXP rho);

/* substitute in an _evaluated_ object, with an explicit list as
   second arg (although old-style lists and environments are allowed).
   Simpler than the standard substitute in this situation.  More to
   the point, the standard code fails in several ways for substituting
   in a function: 1. it creates a CALL object, not a function.  2. it
   does not preserve the original environment 3. it can't handle
   functions with ... as a formal argument */

SEXP do_substitute_direct(SEXP f, SEXP env)
{
    SEXP s;
    if (TYPEOF(env) == VECSXP)
	env = NewEnvironment(R_NilValue, VectorToPairList(env), R_NilValue);
    else if (TYPEOF(env) == LISTSXP)
	env = NewEnvironment(R_NilValue, duplicate(env), R_NilValue);
    if(TYPEOF(env) != ENVSXP)
	error("invalid list for substitution");
    PROTECT(env);
    PROTECT(f);
    s = substitute(f, env);
    /* SET_TYPEOF(s, TYPEOF(f)); */
    UNPROTECT(2);
    return(s);
}

static SEXP substituteDirectList(SEXP el, SEXP rho)
{
    SEXP h, t;
    if (isNull(el))
	return el;
    PROTECT(h = substituteDirect(CAR(el), rho));
    PROTECT(t = substituteDirectList(CDR(el), rho));
    if (isLanguage(el))
	t = LCONS(h, t);
    else
	t = CONS(h, t);
    SET_TAG(t, TAG(el));
    UNPROTECT(2);
    return t;
}

static SEXP substituteDirect(SEXP lang, SEXP rho)
{
    SEXP t;
    switch (TYPEOF(lang)) {
    case SYMSXP:
	t = findVarInFrame( rho, lang);
	if (t != R_UnboundValue)
	    return t;
	else return lang;
    case LANGSXP:
	return substituteDirectList(lang, rho);
    default:
	return(lang);
    }
}
