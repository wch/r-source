/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *
 *  Environments:
 *
 *  All the action of associating values with symbols happens
 *  in this code.  An environment is (essentially) a list of
 *  environment "frames" of the form
 *
 *	CAR(envir) = FRAME(envir) = environment frame
 *	CDR(envir) = ENCLOS(envir) = parent environment
 *
 *  In addition, environments which are created by binding a
 *  function's (=closure's) formals to its actuals have a value
 *
 *	NARGs(envir)
 *
 *  which records the actual number of arguments passed in the
 *  function call.  This is the value returned by the function nargs().
 *
 *  Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 *  When the value of a symbol is required, the environment is
 *  traversed frame-by-frame until a value is found.
 *
 *  If a value is not found during the traversal, the symbol's
 *  "value" slot is inspected for a value.  This "top-level"
 *  environment is where system functions and variables reside.
 *
 */

#include "Defn.h"

/*----------------------------------------------------------------------

    NewEnvironment

    Create an environment by extending "rho" with a frame obtained by
    pairing the variable names given by the tags on "namelist" with
    the values given by the elements of "valuelist".


  ----------------------------------------------------------------------*/

SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho)
{
    SEXP v, n, newrho;
    PROTECT(namelist);
    PROTECT(valuelist);
    PROTECT(rho);
    newrho = allocSExp(ENVSXP);
    FRAME(newrho) = valuelist;
    v = valuelist;
    n = namelist;
    while (v != R_NilValue) {
	TAG(v) = TAG(n);
	v = CDR(v);
	n = CDR(n);
    }
    ENCLOS(newrho) = rho;
    UNPROTECT(3);
    return (newrho);
}


/*----------------------------------------------------------------------

    InitGlobalEnv :

    Create the initial global environment.  The global environment is
    no longer a linked list of environment frames.  Instead it is a
    vector of environments which is searched from beginning to end.

    Note that only the first frame of each of these environments is
    searched.  This is intended to make it possible to implement
    namespaces at some (indeterminate) point in the future.

  ----------------------------------------------------------------------*/

void InitGlobalEnv()
{
    R_GlobalEnv = NewEnvironment(R_NilValue, R_NilValue, R_NilValue);
}


/*----------------------------------------------------------------------

    unbindVar :
    
    Remove a value from an environment. This happens only in the frame
    of the specified frame.

    FIXME ? should this also unbind the symbol value slot when rho is
    R_NilValue.

  ----------------------------------------------------------------------*/

void unbindVar(SEXP symbol, SEXP rho)
{
    SEXP *v = &(FRAME(rho));
    while (*v != R_NilValue) {
	if (TAG(*v) == symbol) {
	    *v = CDR(*v);
	    R_DirtyImage = 1;
	    return;
	}
	v = &CDR(*v);
    }
}


/*----------------------------------------------------------------------

    findVarInFrame :

    Look up the value of a symbol in a single environment frame.  This
    is the basic building block of all variable lookups.

    It is important that this be as efficient as possible.

  ----------------------------------------------------------------------*/

SEXP findVarInFrame(SEXP frame, SEXP symbol)
{
    while (frame != R_NilValue) {
	if (TAG(frame) == symbol)
	    return CAR(frame);
	frame = CDR(frame);
    }
    return R_UnboundValue;
}


/*----------------------------------------------------------------------

    findVar :
    
    Look up a symbol in an environment.

    Changes :

    This needs to be changed so that the environment chain is searched
    and then the searchpath is traversed.  

  ----------------------------------------------------------------------*/

SEXP findVar(SEXP symbol, SEXP rho)
{
    SEXP vl;
    while (rho != R_NilValue) {
	vl = findVarInFrame(FRAME(rho), symbol);
	if (vl != R_UnboundValue)
	    return (vl);
	rho = ENCLOS(rho);
    }
    return (SYMVALUE(symbol));
}


/*----------------------------------------------------------------------

    ddfindVar : 

    This function fetches the variables ..1, ..2, etc from the first
    frame of the environment passed as the second argument to
    ddfindVar.  These variables are implicitly defined whenever a
    ... object is created.

    To determine values for the variables we first search for an
    explicit definition of the symbol, them we look for a ... object
    in the frame and then walk through it to find the appropriate
    values.

    If no value is obtained we return R_UnboundValue.

    It is an error to specify a .. index longer than the length of
    the ... object the value is sought in.

  ----------------------------------------------------------------------*/

SEXP ddfindVar(SEXP symbol, SEXP rho)
{
    int i;
    SEXP vl;

    /* first look for the .. symbol itself */
    vl = findVarInFrame(FRAME(rho), symbol);
    if (vl != R_UnboundValue)
	return(vl);

    i = DDVAL(symbol);
    vl = findVarInFrame(FRAME(rho), R_DotsSymbol);
    if (vl != R_UnboundValue) {
	if (length(vl) >= i) {
	    vl = nthcdr(vl, i - 1);
	    return(CAR(vl));
	}
	else
	    error("The ... list does not contain %d elements\n",i);
    }
    else
        error("..%d used in an incorrect context, no ... to look in\n",i);
    return R_NilValue;
}


/*----------------------------------------------------------------------

    dynamicFindVar :

    This function does a variable lookup, but uses dynamic scoping rules
    rather than the lexical scoping rules used in findVar.

    Return R_UnboundValue if the symbol isn't located and the calling
    function needs to handle the errors.

  ----------------------------------------------------------------------*/

SEXP dynamicfindVar(SEXP symbol, RCNTXT *cptr)
{
    SEXP vl;
    while (cptr != R_ToplevelContext) {
	if (cptr->callflag == CTXT_RETURN) {
	    vl = findVarInFrame(FRAME(cptr->cloenv), symbol);
	    if (vl != R_UnboundValue)
		return vl;
	}
	cptr = cptr->nextcontext;
    }
    return R_UnboundValue;
}


/*----------------------------------------------------------------------

    findFun :

    Search for a function in an environment This is a specially
    modified version of findVar which ignores values its finds if they
    are not functions.

    NEEDED: This needs to be modified so that an object of arbitrary mode
    is searmodify this so that a search for an
    arbitrary mode can be made.  Then findVar and findFun could become
    same function

  ----------------------------------------------------------------------*/

SEXP findFun(SEXP symbol, SEXP rho)
{
    SEXP vl;
    while (rho != R_NilValue) {
	vl = findVarInFrame(FRAME(rho), symbol);
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP ||
		TYPEOF(vl) == SPECIALSXP)
		return (vl);
	    if (vl == R_MissingArg)
		error("Argument \"%s\" is missing, with no default\n",
		      CHAR(PRINTNAME(symbol)));
#ifdef Warn_on_non_function
	    warning("ignored non function \"%s\"",
		    CHAR(PRINTNAME(symbol)));
#endif
	}
	rho = ENCLOS(rho);
    }
    if (SYMVALUE(symbol) == R_UnboundValue)
	error("couldn't find function \"%s\"\n", CHAR(PRINTNAME(symbol)));
    return SYMVALUE(symbol);
}


/*----------------------------------------------------------------------

    defineVar :

    Assign a value in a specific environment frame.
    This needs to be rethought when it comes time to add a search path.

  ----------------------------------------------------------------------*/

void defineVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP frame;
    R_DirtyImage = 1;
    if (rho != R_NilValue) {
	frame = FRAME(rho);
	while (frame != R_NilValue) {
	    if (TAG(frame) == symbol) {
		CAR(frame) = value;
		MISSING(frame) = 0;	/* Over-ride */
		return;
	    }
	    frame = CDR(frame);
	}
	FRAME(rho) = CONS(value, FRAME(rho));
	TAG(FRAME(rho)) = symbol;
	return;
    }
    SYMVALUE(symbol) = value;
}


/*----------------------------------------------------------------------

    setVarInFrame :

    Assign a new value to a symbol in a frame.  Return the symbol if
    successful and R_NilValue if not.

  ----------------------------------------------------------------------*/

SEXP setVarInFrame(SEXP frame, SEXP symbol, SEXP value)
{
    while (frame != R_NilValue) {
	if (TAG(frame) == symbol) {
	    CAR(frame) = value;
	    return symbol;
	}
	frame = CDR(frame);
    }
    return R_NilValue;
}


/*----------------------------------------------------------------------

    setVar :

    Assign a new value to bound symbol.  Note this does the "inherits"
    case.  I.e. it searches frame-by-frame for an symbol and binds the
    given value to the first symbol encountered.  If no symbol is found
    then a binding is created in the global environment.

  ----------------------------------------------------------------------*/

void setVar(SEXP symbol, SEXP value, SEXP rho)
{
    SEXP vl;
    while (rho != R_NilValue) {
	R_DirtyImage = 1;
	vl = setVarInFrame(FRAME(rho), symbol, value);
	if (vl != R_NilValue) {
	    return;
	}
	rho = ENCLOS(rho);
    }
    defineVar(symbol, value, R_GlobalEnv);
}


/*----------------------------------------------------------------------

    gsetVar :

    Assignment in the system environment.  Here we assign directly into
    the system environment.

  ----------------------------------------------------------------------*/

void gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
    R_DirtyImage = 1;
    SYMVALUE(symbol) = value;
}

/*----------------------------------------------------------------------

    do_globalenv

    Returns the current global environment.

  ----------------------------------------------------------------------*/


SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return R_GlobalEnv;
}


/*----------------------------------------------------------------------

    do_attach :

    To attach a list we make up an environment and insert components
    of the list in as the values of this env and intall the tags from
    the list as the names.

  ----------------------------------------------------------------------*/

SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name, s, t, x;
    int pos;
    checkArity(op, args);

    if (!isNewList(CAR(args)))
	error("attach only works for lists and data frames\n");
    CAR(args) = VectorToPairList(CAR(args));

    pos = asInteger(CADR(args));
    if (pos == NA_INTEGER)
	error("attach: pos must be an integer\n");

    name = CADDR(args);
    if(!isString(name) || length(name) != 1)
	error("attach: invalid object name\n");

    for (x = CAR(args); x != R_NilValue; x = CDR(x))
	if (TAG(x) == R_NilValue)
	    error("attach: all elements must be named\n");
    PROTECT(s = allocSExp(ENVSXP));
    setAttrib(s, install("name"), name);

    FRAME(s) = duplicate(CAR(args));
    for (t = R_GlobalEnv; ENCLOS(t) != R_NilValue && pos > 2; t = ENCLOS(t))
	pos--;
    if (ENCLOS(t) == R_NilValue) {
	ENCLOS(t) = s;
	ENCLOS(s) = R_NilValue;
    }
    else {
	x = ENCLOS(t);
	ENCLOS(t) = s;
	ENCLOS(s) = x;
    }
    UNPROTECT(1);
    return s;
}

SEXP do_detach(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, x;
    int pos;

    checkArity(op, args);
    pos = asInteger(CAR(args));

    for (t = R_GlobalEnv ; ENCLOS(t) != R_NilValue && pos > 2 ; t = ENCLOS(t))
	pos--;
    if (pos != 2) {
	error("detach: invalid pos= given\n");
	s = t;	/* for -Wall */
    }
    else {
	PROTECT(s = ENCLOS(t));
	x = ENCLOS(s);
	ENCLOS(t) = x;
    }
    R_Visible = 0;
    UNPROTECT(1);
    return FRAME(s);
}

SEXP do_search(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, name, t;
    int i, n;

    checkArity(op, args);

    n = 2;
    for (t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t))
	n++;
    PROTECT(ans = allocVector(STRSXP, n));

    /* TODO - what should the name of this be? */

    STRING(ans)[0] = mkChar(".GlobalEnv");
    STRING(ans)[n-1] = mkChar("package:base");

    i = 1;
    for (t = ENCLOS(R_GlobalEnv); t != R_NilValue ; t = ENCLOS(t)) {
	name = getAttrib(t, install("name"));
	if(!isString(name) || length(name) < 1)
	    STRING(ans)[i] = mkChar("(unknown)");
	else
	    STRING(ans)[i] = STRING(name)[0];
	i++;
    }
    UNPROTECT(1);
    return ans;
}

static SEXP FetchBuiltins(int intern, int all)
{
    SEXP s, ans;
    int i, count;
    count = 0;
    for (i = 0; i < HSIZE; i++) {
	for (s = R_SymbolTable[i] ; s != R_NilValue; s = CDR(s)) {
	    if (intern) {
		if (INTERNAL(CAR(s)) != R_NilValue)
		    count++;
	    }
	    else {
		if (SYMVALUE(CAR(s)) != R_UnboundValue)
		    count++;
	    }
	}
    }
    ans = allocVector(STRSXP, count);
    count = 0;
    for (i = 0; i < HSIZE; i++) {
	for (s = R_SymbolTable[i] ; s != R_NilValue ; s = CDR(s)) {
	    if (intern) {
		if (INTERNAL(CAR(s)) != R_NilValue)
		    STRING(ans)[count++] = PRINTNAME(CAR(s));
	    }
	    else {
		if (SYMVALUE(CAR(s)) != R_UnboundValue)
		    STRING(ans)[count++] = PRINTNAME(CAR(s));
	    }
	}
    }
    return ans;
}

SEXP do_builtins(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int intern;
    checkArity(op, args);
    intern = asInteger(CAR(args));
    if(intern == NA_INTEGER) intern = 0;
    ans = FetchBuiltins(intern, 1);
    sortVector(ans);
    return ans;
}


/* ls(envir, all.names) */

SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, env, envp, s;
    int all, i, j, k, n;
    checkArity(op, args);
    envp = CAR(args);
    if (isNull(envp) || !isNewList(envp)) {
	PROTECT(env = allocVector(VECSXP, 1));
	VECTOR(env)[0] = envp;
    }
    else
	PROTECT(env = envp);

    all = asLogical(CADR(args));
    if(all == NA_LOGICAL)
	all = 0;
    /* Step 1 : Compute the Vector Size */
    k = 0;
    n = length(env);
    for (i = 0; i < n; i++) {
	if (VECTOR(env)[i] == R_NilValue) {
	    for (j = 0; j < HSIZE; j++) {
		for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
		    if (SYMVALUE(CAR(s)) != R_UnboundValue)
			k++;
		}
	    }
	}
	else if (isEnvironment(VECTOR(env)[i])) {
	    s = FRAME(VECTOR(env)[i]);
	    while (s != R_NilValue) {
		if (all || CHAR(PRINTNAME(TAG(s)))[0] != '.')
		    k += 1;
		s = CDR(s);
	    }
	}
	else error("invalid envir= argument\n");
    }
    /* Step 2 : Allocate and Fill the Result */
    ans = allocVector(STRSXP, k);
    k = 0;
    for (i = 0; i < n; i++) {
	if (VECTOR(env)[i] == R_NilValue) {
	    for (j = 0; j < HSIZE; j++) {
		for (s = R_SymbolTable[j]; s != R_NilValue; s = CDR(s)) {
		    if (SYMVALUE(CAR(s)) != R_UnboundValue)
			STRING(ans)[k++] = PRINTNAME(CAR(s));
		}
	    }
	}
	else if (isEnvironment(VECTOR(env)[i])) {
	    s = FRAME(VECTOR(env)[i]);
	    while (s != R_NilValue) {
		if (all || CHAR(PRINTNAME(TAG(s)))[0] != '.') {
		    STRING(ans)[k++] = PRINTNAME(TAG(s));
		}
		s = CDR(s);
	    }
	}
    }
    UNPROTECT(1);
    sortVector(ans);
    return ans;
}

SEXP do_libfixup(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP lib, env, p;
    checkArity(op, args);
    lib = CAR(args);
    env = CADR(args);
    if (TYPEOF(lib) != ENVSXP || !isEnvironment(env))
	errorcall(call, "invalid arguments\n");
    p = FRAME(lib);
    while (p != R_NilValue) {
	if (TYPEOF(CAR(p)) == CLOSXP)
	    CLOENV(CAR(p)) = env;
	p = CDR(p);
    }
    return lib;
}

static SEXP pos2env(int pos, SEXP call)
{
    SEXP env;
    if (pos == NA_INTEGER || pos < -1 || pos == 0) {
	errorcall(call, "invalid argument\n");
	env = call;/* just for -Wall */
    }
    else if(pos == -1) {
	env = R_GlobalContext->sysparent;
	if(R_GlobalEnv != R_NilValue && env == R_NilValue)
	    errorcall(call, "invalid argument\n");
    }
    else {
	for (env = R_GlobalEnv; env != R_NilValue && pos > 1;
	     env = ENCLOS(env))
	    pos--;
	if(pos != 1)
	    error("invalid argument\n");
    }
    return env;
}

SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, pos;
    int i, npos;
    PROTECT(pos = coerceVector(CAR(args), INTSXP));
    npos = length(pos);
    if (npos <= 0)
	errorcall(call, "invalid \"pos\" argument\n");
    PROTECT(env = allocVector(VECSXP, npos));
    for (i = 0; i < npos; i++) {
	VECTOR(env)[i] = pos2env(INTEGER(pos)[i], call);
    }
    if (npos == 1) env = VECTOR(env)[0];
    UNPROTECT(2);
    return env;
}
