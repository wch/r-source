/*
 *  R : A Computer Langage for Statistical Data Analysis
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
 */

/*--------------------------------------------------------------
 *
 * Environments:
 *
 * All the action of associating values with symbols happens
 * in this code.  An environment is (essentially) a list of
 * environment "frames" of the form
 *
 *	CAR(envir) = FRAME(envir) = environment frame
 *	CDR(envir) = ENCLOS(envir) = parent environment
 *
 * In addition, environments which are created by binding a
 * function's (=closure's) formals to its actuals have a value
 *
 *	NARGs(envir)
 *
 * which records the actual number of arguments passed in the
 * function call.  This is the value returned by the function nargs().
 *
 * Each frame is a (tagged) list with
 *
 *	TAG(item) = symbol
 *	CAR(item) = value bound to symbol in this frame
 *	CDR(item) = next value on the list
 *
 * When the value of a symbol is required, the environment is
 * traversed frame-by-frame until a value is found.
 *
 * If a value is not found during the traversal, the symbol's
 * "value" slot is inspected for a value.  This "top-level"
 * environment is where system functions and variables reside.
 * Assignment in this environment is carried out with the :=
 * assignment operator.
 *
 * Note that: mkEnv can be found in dstruct.c
 *
 *--------------------------------------------------------------*/

#include "Defn.h"

extern int R_DirtyImage;

/* emptyEnv - return an environment with no bindings */
SEXP emptyEnv()
{
	return mkEnv(R_NilValue, R_NilValue, R_NilValue);
}


/* extendEnv - extend environment rho by binding vars to vals */
SEXP extendEnv(SEXP rho, SEXP vars, SEXP vals)
{
	return mkEnv(vars, vals, rho);
}


/* NEED: this should also unbind the symbol value slot */
/* when rho is R_NilValue */

/* unbindVar - remove a value from an environment */
/* this happens only in the current environment frame */
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


/* getVarInFrame - return an object whose car contains the */
/* value of "symbol" in the specified environment frame. */
/* This is called the symbol's "slot" below. */
/* If the symbol is unbound in the frame, returns R_NilValue */

SEXP getVarInFrame(SEXP frame, SEXP symbol)
{
	while (frame != R_NilValue) {
		if (TAG(frame) == symbol)
			return frame;
		frame = CDR(frame);
	}
	return R_NilValue;
}


/* getVar - return the slot for a symbol in an environment */
SEXP getVar(SEXP symbol, SEXP rho)
{
	SEXP vl;

	while (rho != R_NilValue) {
		vl = getVarInFrame(FRAME(rho), symbol);
		if (vl != R_NilValue)
			return (vl);
		rho = ENCLOS(rho);
	}
	return (symbol);
}


/* findVar - look up a symbol in an environment */
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

/* return R_UnboundValue if the symbol isn't located and the calling
   function needs to handle the errors
*/
SEXP dynamicfindVar(SEXP symbol, RCNTXT *cptr)
{
	SEXP vl;

	while(cptr != R_ToplevelContext) {
		if(cptr->callflag == CTXT_RETURN) {
			vl = findVarInFrame(FRAME(cptr->cloenv), symbol);
			if(vl != R_UnboundValue)
				return(vl);
		}
		cptr = cptr->nextcontext;
	}
	return(R_UnboundValue);
}


/* findFun - search for a function in an environment */
/* This is a specially modified version of findVar which */
/* ignores values its finds if they are not functions. */
/* NEEDED: modify this so that a search for an arbitrary */
/* mode can be made.  Then findVar and findFun could become */
/* same function */

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
			if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP || TYPEOF(vl) == SPECIALSXP)
				return (vl);
			if (vl == R_MissingArg)
				error("Argument \"%s\" is missing, with no default\n", CHAR(PRINTNAME(symbol)));
			warning("ignored non function \"%s\"\n", CHAR(PRINTNAME(symbol)));
		}
		rho = ENCLOS(rho);
	}
	if (SYMVALUE(symbol) == R_UnboundValue)
		error("couldn't find function \"%s\"\n", CHAR(PRINTNAME(symbol)));
	return (SYMVALUE(symbol));
}


/* findVarInFrame - look up name in a single environment frame */
SEXP findVarInFrame(SEXP frame, SEXP symbol)
{
	while (frame != R_NilValue) {
		if (TAG(frame) == symbol)
			return CAR(frame);
		frame = CDR(frame);
	}
	return R_UnboundValue;
}


/* defineVar - assign a value in a specific environment frame */
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

/* setVar - assign a new value to bound symbol */
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

/* assignment in the system environment */
/* Functions here all have the system environment as their environment. */
/* This means that redefinition c, t etc will not break a system function. */
void gsetVar(SEXP symbol, SEXP value, SEXP rho)
{
	R_DirtyImage = 1;
#ifdef EXPT
	if(TYPEOF(value) == CLOSXP)
		CLOENV(value) = R_NilValue;
#endif
	SYMVALUE(symbol) = value;
}

/* setVarInFrame - assign a new value to a symbol in a frame */
/* return the symbol if successful */
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

SEXP do_globalenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	return R_GlobalEnv;
}

	/* To attach a list we make up an environment and insert */
	/* components of the list in as the values of this env */
	/* and intall the tags from the list as the names. */

SEXP do_attach(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP name, s, t, x;
	int pos;

	checkArity(op, args);

	if (!isList(CAR(args)))
		error("attach only works for lists and data frames\n");

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

	for (t = R_GlobalEnv; ENCLOS(t) != R_NilValue && pos > 2; t = ENCLOS(t))
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
		for(s=R_SymbolTable[i]; s!=R_NilValue; s=CDR(s)) {
			if(intern) {
				if(INTERNAL(CAR(s)) != R_NilValue)
					count++;
			}
			else {
				if(SYMVALUE(CAR(s)) != R_UnboundValue)
					count++;
			}
		}
	}
	ans = allocVector(STRSXP, count);
	count = 0;
	for (i = 0; i < HSIZE; i++) {
		for(s=R_SymbolTable[i]; s!=R_NilValue; s=CDR(s)) {
			if(intern) {
				if(INTERNAL(CAR(s)) != R_NilValue)
					STRING(ans)[count++] = PRINTNAME(CAR(s));
			}
			else {
				if(SYMVALUE(CAR(s)) != R_UnboundValue)
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

SEXP do_ls(SEXP call, SEXP op, SEXP args, SEXP rho)
{
 /* ls(envir, all.names) */

	SEXP ans, env, envp, s;
	int all, i, k;

	checkArity(op, args);

	env = CAR(args);
	if(isNull(env) || !isList(env)) PROTECT(env = CONS(env, R_NilValue));
	else PROTECT(env);
	all = asLogical(CADR(args));
	if(all == NA_LOGICAL) all = 0;

		/* Step 1 : Compute the Vector Size */

	k = 0;
	for(envp=env ; envp!=R_NilValue ; envp=CDR(envp)) {
		if(CAR(envp) == R_NilValue) {
			for (i = 0; i < HSIZE; i++) {
				for(s=R_SymbolTable[i]; s!=R_NilValue; s=CDR(s)) {
					if(SYMVALUE(CAR(s)) != R_UnboundValue)
						k++;
				}
			}
		}
		else if(isEnvironment(CAR(envp))) {
			s = FRAME(CAR(envp));
			while (s != R_NilValue) {
				if(all || CHAR(PRINTNAME(TAG(s)))[0] != '.')
					k += 1;
				s = CDR(s);
			}
		}
		else error("invalid envir= argument\n");
	}

		/* Step 2 : Allocate and Fill the Result */

	ans = allocVector(STRSXP, k);
	k = 0;
	for(envp=env ; envp!=R_NilValue ; envp=CDR(envp)) {
		if(CAR(envp) == R_NilValue) {
			for (i = 0; i < HSIZE; i++) {
				for(s=R_SymbolTable[i]; s!=R_NilValue; s=CDR(s)) {
					if(SYMVALUE(CAR(s)) != R_UnboundValue)
						STRING(ans)[k++] = PRINTNAME(CAR(s));
				}
			}
		}
		else if(isEnvironment(CAR(envp))) {
			s = FRAME(CAR(envp));
			while (s != R_NilValue) {
				if(all || CHAR(PRINTNAME(TAG(s)))[0] != '.') {
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
	if(TYPEOF(lib) != ENVSXP || !isEnvironment(env))
		errorcall(call, "invalid arguments\n");
	p = FRAME(lib);
	while(p != R_NilValue) {
		if(TYPEOF(CAR(p)) == CLOSXP)
		CLOENV(CAR(p)) = env;
		p = CDR(p);
	}
	return lib;
}

static SEXP pos2env(int pos, SEXP call)
{
	SEXP env;

	if(pos == NA_INTEGER || pos < -1 || pos == 0) {
		errorcall(call, "invalid argument\n");
		env=call;/* just for -Wall */
	}
	else if(pos == -1) {
		env = R_GlobalContext->sysparent;
		if(R_GlobalEnv != R_NilValue && env == R_NilValue)
			errorcall(call, "invalid argument\n");
	}
	else {
		for (env = R_GlobalEnv; env != R_NilValue && pos > 1; env = ENCLOS(env))
			pos--;
		if(pos != 1)
			error("invalid argument\n");
	}
	return env;
}

SEXP do_pos2env(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP env, envp, pos;
	int i, npos;

	PROTECT(pos = coerceVector(CAR(args), INTSXP));
	npos = length(pos);
	if(npos <= 0)
		errorcall(call, "invalid \"pos\" argument\n");
	PROTECT(envp = env = allocList(npos));
	for(i=0 ; i<npos ; i++) {
		CAR(envp) = pos2env(INTEGER(pos)[i], call);
		envp = CDR(envp);
	}
	UNPROTECT(2);
	if(npos == 1) env = CAR(env);
	return env;
}
