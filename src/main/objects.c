/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2002-3	      The R Foundation
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

/*  This module contains support for S-style generic */
/*  functions and "class" support.  Gag, barf ...  */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <R_ext/RS.h> /* for Calloc, Realloc and for S4 object bit */

static SEXP GetObject(RCNTXT *cptr)
{
    SEXP s, sysp, b, formals, funcall, tag;
    sysp = R_GlobalContext->sysparent;

    PROTECT(funcall = R_syscall(0, cptr));

    if ( TYPEOF(CAR(funcall)) == SYMSXP )
	PROTECT(b = findFun(CAR(funcall), sysp));
    else
	PROTECT(b = eval(CAR(funcall), sysp));
    /**** use R_sysfunction here instead */
    if (TYPEOF(b) != CLOSXP) error(_("generic 'function' is not a function"));
    formals = FORMALS(b);

    tag = TAG(formals);
    if (tag != R_NilValue && tag != R_DotsSymbol) {
	s = R_NilValue;
	/** exact matches **/
	for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
	    if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 1)) {
		if (s != R_NilValue)
		    error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		else
		    s = CAR(b);
	    }

	if (s == R_NilValue)
	    /** partial matches **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 0)) {
		    if ( s != R_NilValue)
			error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		    else
			s = CAR(b);
		}
	if (s == R_NilValue)
	    /** first untagged argument **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) == R_NilValue )
		{
		    s = CAR(b);
		    break;
		}
	if (s == R_NilValue)
	    s = CAR(cptr->promargs);
/*
	    error("failed to match argument for dispatch");
*/
    }
    else
	s = CAR(cptr->promargs);

    UNPROTECT(2);
    if (TYPEOF(s) == PROMSXP) {
	if (PRVALUE(s) == R_UnboundValue)
	    s = eval(s, R_BaseEnv);
	else
	    s = PRVALUE(s);
    }
    return(s);
}

static SEXP applyMethod(SEXP call, SEXP op, SEXP args, SEXP rho, SEXP newrho)
{
    SEXP ans;
    if (TYPEOF(op) == SPECIALSXP) {
	int save = R_PPStackTop, flag = PRIMPRINT(op);
	R_Visible = flag != 1;
	ans = PRIMFUN(op) (call, op, args, rho);
	if (flag < 2) R_Visible = flag != 1;
	check_stack_balance(op, save);
    }
    /* In other places we add a context to builtins when profiling,
       but we have not bothered here (as there seem to be no primitives 
       used as methods, and this would have to be a primitive to be
       found).
     */
    else if (TYPEOF(op) == BUILTINSXP) {
	int save = R_PPStackTop, flag = PRIMPRINT(op);
	PROTECT(args = evalList(args, rho, op));
	R_Visible = flag != 1;
	ans = PRIMFUN(op) (call, op, args, rho);
	if (flag < 2) R_Visible = flag != 1;
	UNPROTECT(1);
	check_stack_balance(op, save);
    }
    else if (TYPEOF(op) == CLOSXP) {
	ans = applyClosure(call, op, args, rho, newrho);
    }
    else
	ans = R_NilValue;  /* for -Wall */
    return ans;
}


/* "newintoold" -  a destructive matching of arguments; */
/* newargs comes first; any element of oldargs with */
/* a name that matches a named newarg is deleted; the */
/* two resulting lists are appended and returned. */
/* S claims to do this (white book) but doesn't seem to. */

static SEXP newintoold(SEXP _new, SEXP old)
{
    if (_new == R_NilValue) return R_NilValue;
    SETCDR(_new, newintoold(CDR(_new),old));
    while (old != R_NilValue) {
	if (TAG(old) != R_NilValue && TAG(old) == TAG(_new)) {
	    SETCAR(old, CAR(_new));
	    return CDR(_new);
	}
	old = CDR(old);
    }
    return _new;
}

static SEXP matchmethargs(SEXP oldargs, SEXP newargs)
{
    newargs = newintoold(newargs, oldargs);
    return listAppend(oldargs, newargs);
}

/*  usemethod  -  calling functions need to evaluate the object
 *  (== 2nd argument).	They also need to ensure that the
 *  argument list is set up in the correct manner.
 *
 *    1. find the context for the calling function (i.e. the generic)
 *	 this gives us the unevaluated arguments for the original call
 *
 *    2. create an environment for evaluating the method and insert
 *	 a handful of variables (.Generic, .Class and .Method) into
 *	 that environment. Also copy any variables in the env of the
 *	 generic that are not formal (or actual) arguments.
 *
 *    3. fix up the argument list; it should be the arguments to the
 *	 generic matched to the formals of the method to be invoked */

SEXP R_LookupMethod(SEXP method, SEXP rho, SEXP callrho, SEXP defrho)
{
    SEXP val;

    if (TYPEOF(callrho) == NILSXP) {
	error(_("use of NULL environment is defunct"));
	callrho = R_BaseEnv;
    } else
	if (TYPEOF(callrho) != ENVSXP)
	    error(_("bad generic call environment"));
    if (TYPEOF(defrho) == NILSXP) {
	    error(_("use of NULL environment is defunct"));
	    defrho = R_BaseEnv;
    } else
	if (TYPEOF(defrho) != ENVSXP)
	    error(_("bad generic definition environment"));
    if (defrho == R_BaseEnv)
	defrho = R_BaseNamespace;
    
    /* This evaluates promises */
    val = findVar1(method, callrho, FUNSXP, TRUE);
    if (isFunction(val))
	return val;
    else {
	/* We assume here that no one registered a non-function */
	SEXP table = findVarInFrame3(defrho,
				     install(".__S3MethodsTable__."),
				     TRUE);
	if (TYPEOF(table) == PROMSXP) table = eval(table, R_BaseEnv);
	if (TYPEOF(table) == ENVSXP) {
	    val = findVarInFrame3(table, method, TRUE);
	    if (TYPEOF(val) == PROMSXP) val = eval(val, rho);
	    if (val != R_UnboundValue) return val;
	}
	return R_UnboundValue;
    }
}

int usemethod(char *generic, SEXP obj, SEXP call, SEXP args,
	      SEXP rho, SEXP callrho, SEXP defrho, SEXP *ans)
{
    SEXP klass, method, sxp, t, s, matchedarg;
    SEXP op, formals, newrho, newcall;
    char buf[512];
    int i, j, nclass, matched;
    RCNTXT *cptr;

    /* Get the context which UseMethod was called from. */

    cptr = R_GlobalContext;
    if ( !(cptr->callflag & CTXT_FUNCTION) || cptr->cloenv != rho)
	error(_("'UseMethod' used in an inappropriate fashion"));

    /* Create a new environment without any */
    /* of the formals to the generic in it. */

    PROTECT(newrho = allocSExp(ENVSXP));
    op = CAR(cptr->call);
    switch (TYPEOF(op)) {
    case SYMSXP:
	PROTECT(op = findFun(op, cptr->sysparent));
	break;
    case LANGSXP:
	PROTECT(op = eval(op, cptr->sysparent));
	break;
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
	PROTECT(op);
	break;
    default:
	error(_("Invalid generic function in 'usemethod'"));
    }

    if (TYPEOF(op) == CLOSXP) {
	formals = FORMALS(op);
	for (s = FRAME(cptr->cloenv); s != R_NilValue; s = CDR(s)) {
	    matched = 0;
	    for (t = formals; t != R_NilValue; t = CDR(t))
		if (TAG(t) == TAG(s)) matched = 1;

	    if (!matched) defineVar(TAG(s), CAR(s), newrho);
	}
    }

    PROTECT(matchedarg = cptr->promargs);
    PROTECT(newcall = duplicate(cptr->call));

    PROTECT(klass = R_data_class2(obj));
    nclass = length(klass);
    for (i = 0; i < nclass; i++) {
	char *ss = translateChar(STRING_ELT(klass, i));
	if(strlen(generic) + strlen(ss) + 2 > 512)
	    error(_("class name too long in '%s'"), generic);
	sprintf(buf, "%s.%s", generic, ss);
	method = install(buf);
	sxp = R_LookupMethod(method, rho, callrho, defrho);
	if (isFunction(sxp)) {
	    defineVar(install(".Generic"), mkString(generic), newrho);
	    if (i > 0) {
		PROTECT(t = allocVector(STRSXP, nclass - i));
		for (j = 0; j < length(t); j++, i++)
		    SET_STRING_ELT(t, j, STRING_ELT(klass, i));
		setAttrib(t, install("previous"), klass);
		defineVar(install(".Class"), t, newrho);
		UNPROTECT(1);
	    } else
		defineVar(install(".Class"), klass, newrho);
	    PROTECT(t = mkString(buf));
	    defineVar(install(".Method"), t, newrho);
	    UNPROTECT(1);
	    defineVar(install(".GenericCallEnv"), callrho, newrho);
	    defineVar(install(".GenericDefEnv"), defrho, newrho);
	    t = newcall;
	    SETCAR(t, method);
	    R_GlobalContext->callflag = CTXT_GENERIC;
	    *ans = applyMethod(t, sxp, matchedarg, rho, newrho);
	    R_GlobalContext->callflag = CTXT_RETURN;
	    UNPROTECT(5);
	    return 1;
	}
    }
    if(strlen(generic) + strlen("default") + 2 > 512)
	error(_("class name too long in '%s'"), generic);
    sprintf(buf, "%s.default", generic);
    method = install(buf);
    sxp = R_LookupMethod(method, rho, callrho, defrho);
    if (isFunction(sxp)) {
	defineVar(install(".Generic"), mkString(generic), newrho);
	defineVar(install(".Class"), R_NilValue, newrho);
	PROTECT(t = mkString(buf));
	defineVar(install(".Method"), t, newrho);
	UNPROTECT(1);
	defineVar(install(".GenericCallEnv"), callrho, newrho);
	defineVar(install(".GenericDefEnv"), defrho, newrho);
	t = newcall;
	SETCAR(t, method);
	R_GlobalContext->callflag = CTXT_GENERIC;
	*ans = applyMethod(t, sxp, matchedarg, rho, newrho);
	R_GlobalContext->callflag = CTXT_RETURN;
	UNPROTECT(5);
	return 1;
    }
    UNPROTECT(5);
    cptr->callflag = CTXT_RETURN;
    return 0;
}

/* Note: "do_usemethod" is not the only entry point to */
/* "usemethod". Things like [ and [[ call usemethod directly, */
/* hence do_usemethod should just be an interface to usemethod. */

SEXP attribute_hidden do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, generic = R_NilValue /* -Wall */, obj, val;
    SEXP callenv, defenv;
    int nargs;
    RCNTXT *cptr;

    nargs = length(args);

    if (nargs < 0)
	errorcall(call, _("corrupt internals!"));

    /* get environments needed for dispatching.
       callenv = environment from which the generic was called
       defenv = environment where the generic was defined */
    cptr = R_GlobalContext;
    if ( !(cptr->callflag & CTXT_FUNCTION) || cptr->cloenv != env)
	error(_("'UseMethod' used in an inappropriate fashion"));
    callenv = cptr->sysparent;
    if (nargs)
	PROTECT(generic = eval(CAR(args), env));
    else 
	errorcall(call, _("there must be a first argument"));
    /* We need to find the generic to find out where it is defined.
       This is set up to avoid getting caught by things like
 
        mycoef <- function(x)
       {
           mycoef <- function(x) stop("not this one")
           UseMethod("mycoef")
       }

	The generic need not be a closure (Henrik Bengtsson writes
	UseMethod("$"), although only functions are documented.)
    */
    val = findVar1(install(translateChar(STRING_ELT(generic, 0))),
		   ENCLOS(env), FUNSXP, TRUE); /* That has evaluated promises */
    if(TYPEOF(val) == CLOSXP) defenv = CLOENV(val);
    else defenv = R_BaseNamespace;
/*
    if(defenv !=  ENCLOS(env)) {
        printf("*** problem ***\n");
	PrintValue(generic);
	PrintValue(ENCLOS(env));
    }
*/  

    if (nargs > 2)  /* R-lang says there should be a warning */
	warningcall(call, _("arguments after the first two are ignored"));
    if (nargs >= 2)
	PROTECT(obj = eval(CADR(args), env));
    else {
	cptr = R_GlobalContext;
	while (cptr != NULL) {
	    if ( (cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == env)
		break;
	    cptr = cptr->nextcontext;
	}
	if (cptr == NULL)
	    error(_("'UseMethod' called from outside a closure"));
	/* if (generic == R_MissingArg)
	   PROTECT(generic = mkString(CHAR(PRINTNAME(CAR(cptr->call))))); */
	PROTECT(obj = GetObject(cptr));
    }

    if (TYPEOF(generic) != STRSXP ||
	LENGTH(generic) < 1 ||
	CHAR(STRING_ELT(generic, 0))[0] == '\0')
	errorcall(call, _("first argument must be a generic name"));

    if (usemethod(translateChar(STRING_ELT(generic, 0)), obj, call, CDR(args),
		  env, callenv, defenv, &ans) == 1) {
	UNPROTECT(1); /* obj */
	PROTECT(ans);
	findcontext(CTXT_RETURN, env, ans); /* does not return */
	UNPROTECT(1);
    }
    else
	error(_("no applicable method for \"%s\""), 
	      translateChar(STRING_ELT(generic, 0)));
    return R_NilValue; /* NOT Used */
}

/*
   fixcall: fixes up the call when arguments to the function may
   have changed; for now we only worry about tagged args, appending
   them if they are not already there
*/

static SEXP fixcall(SEXP call, SEXP args)
{
    SEXP s, t;
    int found;

    for(t = args; t != R_NilValue; t = CDR(t)) {
	if(TAG(t) != R_NilValue) {
		found = 0;
		for(s = call; CDR(s) != R_NilValue; s = CDR(s))
		    if(TAG(CDR(s)) == TAG(t)) found = 1;
		if( !found ) {
			SETCDR(s, allocList(1));
			SET_TAG(CDR(s), TAG(t));
			SETCAR(CDR(s), duplicate(CAR(t)));
		}
	}
    }
    return call;
}

/* If NextMethod has any arguments the first must be the generic */
/* the second the object and any remaining are matched with the */
/* formals of the chosen method. */

#define ARGUSED(x) LEVELS(x)

SEXP attribute_hidden do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[512], b[512], bb[512], tbuf[10], *sb, *sg, *sk;
    SEXP ans, s, t, klass, method, matchedarg, generic, nextfun;
    SEXP sysp, m, formals, actuals, tmp, newcall;
    SEXP a, group, basename;
    SEXP callenv, defenv;
    RCNTXT *cptr;
    int i, j, cftmp;

    cptr = R_GlobalContext;
    cftmp = cptr->callflag;
    cptr->callflag = CTXT_GENERIC;

    /* get the env NextMethod was called from */
    sysp = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == sysp) break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error(_("'NextMethod' called from outside a function"));

    PROTECT(newcall = duplicate(cptr->call));

    /* eg get("print.ts")(1) */
    if (TYPEOF(CAR(cptr->call)) == LANGSXP)
       error(_("'NextMethod' called from an anonymous function"));

    /* Find dispatching environments. Promises shouldn't occur, but
       check to be on the safe side.  If the variables are not in the
       environment (the method was called outside a method dispatch)
       then chose reasonable defaults. */
    callenv = findVarInFrame3(R_GlobalContext->sysparent,
			      install(".GenericCallEnv"), TRUE);
    if (TYPEOF(callenv) == PROMSXP)
	callenv = eval(callenv, R_BaseEnv);
    else if (callenv == R_UnboundValue)
	    callenv = env;
    defenv = findVarInFrame3(R_GlobalContext->sysparent,
			     install(".GenericDefEnv"), TRUE);
    if (TYPEOF(defenv) == PROMSXP) defenv = eval(defenv, R_BaseEnv);
    else if (defenv == R_UnboundValue) defenv = R_GlobalEnv;

    /* set up the arglist */
    s = R_LookupMethod(CAR(cptr->call), env, callenv, defenv);
    if (TYPEOF(s) == SYMSXP && s == R_UnboundValue) 
	error(_("no calling generic was found: was a method called directly?"));
    if (TYPEOF(s) != CLOSXP){ /* R_LookupMethod looked for a function */
	errorcall(R_NilValue, 
		  _("'function' is not a function, but of type %d"), 
		  TYPEOF(s));
    }
    /* get formals and actuals; attach the names of the formals to
       the actuals, expanding any ... that occurs */
    formals = FORMALS(s);
    PROTECT(actuals = matchArgs(formals, cptr->promargs));

    i = 0;
    for(s = formals, t = actuals; s != R_NilValue; s = CDR(s), t = CDR(t)) {
	SET_TAG(t, TAG(s));
	if(TAG(t) == R_DotsSymbol) i = length(CAR(t));
    }
    if(i) {   /* we need to expand out the dots */
	PROTECT(t = allocList(i+length(actuals)-1));
	for(s = actuals, m = t; s != R_NilValue; s = CDR(s)) {
	    if(TYPEOF(CAR(s)) == DOTSXP) {
		for(i = 1, a = CAR(s); a != R_NilValue; 
		    a = CDR(a), i++, m = CDR(m)) {
		    sprintf(tbuf, "..%d", i);
		    SET_TAG(m, mkSYMSXP(mkChar(tbuf), R_UnboundValue));
		    SETCAR(m, CAR(a));
		}
	    } else {
		SET_TAG(m, TAG(s));
		SETCAR(m, CAR(s));
		m = CDR(m);
	    }
	}
	UNPROTECT(1);
	actuals = t;
    }
    PROTECT(actuals);


    /* we can't duplicate because it would force the promises */
    /* so we do our own duplication of the promargs */

    PROTECT(matchedarg = allocList(length(cptr->promargs)));
    for (t = matchedarg, s = cptr->promargs; t != R_NilValue;
	 s = CDR(s), t = CDR(t)) {
	SETCAR(t, CAR(s));
	SET_TAG(t, TAG(s));
    }
    for (t = matchedarg; t != R_NilValue; t = CDR(t)) {
	for (m = actuals; m != R_NilValue; m = CDR(m))
	    if (CAR(m) == CAR(t))  {
		if (CAR(m) == R_MissingArg) {
		    tmp = findVarInFrame3(cptr->cloenv, TAG(m), TRUE);
		    if (tmp == R_MissingArg) break;
		}
		SETCAR(t, mkPROMISE(TAG(m), cptr->cloenv));
		break;
	   }
    }
    /*
      Now see if there were any other arguments passed in
      Currently we seem to only allow named args to change
      or to be added, this is at variance with p. 470 of the
      White Book
    */

    s = CADDR(args); /* this is ... and we need to see if it's bound */
    if (s == R_DotsSymbol) {
	t = findVarInFrame3(env, s, TRUE);
	if (t != R_NilValue && t != R_MissingArg) {
	    SET_TYPEOF(t, LISTSXP); /* a safe mutation */
	    s = matchmethargs(matchedarg, t);
	    UNPROTECT(1);
	    PROTECT(matchedarg = s);
	    newcall = fixcall(newcall, matchedarg);
	}
    }
    else
	errorcall(call, _("wrong argument ..."));

    /*
      .Class is used to determine the next method; if it doesn't
      exist the first argument to the current method is used
      the second argument to NextMethod is another option but
      isn't currently used).
    */
    klass = findVarInFrame3(R_GlobalContext->sysparent,
			    install(".Class"), TRUE);

    if (klass == R_UnboundValue) {
	s = GetObject(cptr);
	if (!isObject(s)) errorcall(call, _("object not specified"));
	klass = getAttrib(s, R_ClassSymbol);
    }

    /* the generic comes from either the sysparent or it's named */
    generic = findVarInFrame3(R_GlobalContext->sysparent,
			      install(".Generic"), TRUE);
    if (generic == R_UnboundValue)
	generic = eval(CAR(args), env);
    if( generic == R_NilValue )
	errorcall(call, _("generic function not specified"));
    PROTECT(generic);

    if (!isString(generic) || length(generic) > 1)
	errorcall(call, _("invalid generic argument to NextMethod"));

    if (CHAR(STRING_ELT(generic, 0))[0] == '\0')
	errorcall(call, _("generic function not specified"));

    /* determine whether we are in a Group dispatch */

    group = findVarInFrame3(R_GlobalContext->sysparent,
			    install(".Group"), TRUE);
    if (group == R_UnboundValue) PROTECT(group = mkString(""));
    else PROTECT(group);

    if (!isString(group) || length(group) > 1)
	errorcall(call, _("invalid 'group' argument found in NextMethod"));

    /* determine the root: either the group or the generic will be it */

    if (CHAR(STRING_ELT(group, 0))[0] == '\0') basename = generic;
    else basename = group;

    nextfun = R_NilValue;

    /* 
       Find the method currently being invoked and jump over the current call
       If t is R_UnboundValue then we called the current method directly
    */

    method = findVarInFrame3(R_GlobalContext->sysparent,
			     install(".Method"), TRUE);
    if( method != R_UnboundValue) {
	char *ss;
	if( !isString(method) )
	    error(_("wrong value for .Method"));
	for(i = 0; i < length(method); i++) {
	    ss = translateChar(STRING_ELT(method, i));
	    if(strlen(ss) >= 512)
		error(_("method name too long in '%s'"), ss);
	    sprintf(b, "%s", ss);
	    if(strlen(b)) break;
	}
	/* for binary operators check that the second argument's method
	   is the same or absent */
	for(j = i; j < length(method); j++){
	    char *ss = translateChar(STRING_ELT(method, j));
	    if(strlen(ss) >= 512)
		error(_("method name too long in '%s'"), ss);
	  sprintf(bb, "%s", ss);
	  if (strlen(bb) && strcmp(b,bb))
	      warning(_("Incompatible methods ignored"));
	}
    }
    else {
	if(strlen(CHAR(PRINTNAME(CAR(cptr->call)))) >= 512)
	   error(_("call name too long in '%s'"),
		 CHAR(PRINTNAME(CAR(cptr->call))));
	sprintf(b, "%s", CHAR(PRINTNAME(CAR(cptr->call))));
    }
    
    sb = translateChar(STRING_ELT(basename, 0));
    for (j = 0; j < length(klass); j++) {
	sk = translateChar(STRING_ELT(klass, j));
	if(strlen(sb) + strlen(sk) + 2 > 512)
	    error(_("class name too long in '%s'"), sb);
	sprintf(buf, "%s.%s", sb, sk);
	if (!strcmp(buf, b)) break;
    }

    if (!strcmp(buf, b)) /* we found a match and start from there */
      j++;
    else
      j = 0;  /*no match so start with the first element of .Class */

    /* we need the value of i on exit from the for loop to figure out
	   how many classes to drop. */
    
    sg = translateChar(STRING_ELT(generic, 0));
    for (i = j ; i < length(klass); i++) {
	sk = translateChar(STRING_ELT(klass, i));
	if(strlen(sg) + strlen(sk) + 2 > 512)
	    error(_("class name too long in '%s'"), sg);
	sprintf(buf, "%s.%s", sg, sk);
	nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	if (isFunction(nextfun)) break;
	if (group != R_UnboundValue) {
	    /* if not Generic.foo, look for Group.foo */
	    if(strlen(sb) + strlen(sk) + 2 > 512)
		error(_("class name too long in '%s'"), sb);
	    sprintf(buf, "%s.%s", sb, sk);
	    nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	    if(isFunction(nextfun))
		break;
	}
	if (isFunction(nextfun))
	    break;
    }
    if (!isFunction(nextfun)) {
	sprintf(buf, "%s.default", sg);
	nextfun = R_LookupMethod(install(buf), env, callenv, defenv);
	/* If there is no default method, try the generic itself,
	   provided it is primitive or a wrapper for a .Internal
	   function of the same name.
	 */
	if (!isFunction(nextfun)) {
	    t = install(sg);
	    nextfun = findVar(t, env);
	    if (TYPEOF(nextfun) == PROMSXP)
		nextfun = eval(nextfun, env);
	    if (!isFunction(nextfun))
		error(_("no method to invoke"));
	    if (TYPEOF(nextfun) == CLOSXP) {
		if (INTERNAL(t) != R_NilValue)
		    nextfun = INTERNAL(t);
		else
		    error(_("no method to invoke"));
	    }
	}
    }
    PROTECT(s = allocVector(STRSXP, length(klass) - i));
    PROTECT(klass = duplicate(klass));
    PROTECT(m = allocSExp(ENVSXP));
    for (j = 0; j < length(s); j++)
	SET_STRING_ELT(s, j, duplicate(STRING_ELT(klass, i++)));
    setAttrib(s, install("previous"), klass);
    defineVar(install(".Class"), s, m);
    /* It is possible that if a method was called directly that
	'method' is unset */
    if (method != R_UnboundValue) {
 	/* for Ops we need `method' to be a vector */
	PROTECT(method = duplicate(method));
	for(j = 0; j < length(method); j++) {
	    if (strlen(CHAR(STRING_ELT(method,j))))
		SET_STRING_ELT(method, j,  mkChar(buf));
        }
    } else
	PROTECT(method = mkString(buf));
    defineVar(install(".Method"), method, m);
    defineVar(install(".GenericCallEnv"), callenv, m);
    defineVar(install(".GenericDefEnv"), defenv, m);

    method = install(buf);

    defineVar(install(".Generic"), generic, m);

    defineVar(install(".Group"), group, m);

    SETCAR(newcall, method);
    ans = applyMethod(newcall, nextfun, matchedarg, env, m);
    UNPROTECT(10);
    return(ans);
}

SEXP attribute_hidden do_unclass(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    switch(TYPEOF(CAR(args))) {
    case ENVSXP:
	errorcall(call, _("cannot unclass an environment"));
	break;
    case EXTPTRSXP:
	errorcall(call, _("cannot unclass an external pointer"));
	break;
    default:
	break;
    }
    if (isObject(CAR(args))) {
	SETCAR(args, duplicate(CAR(args)));
	setAttrib(CAR(args), R_ClassSymbol, R_NilValue);
    }
    return CAR(args);
}

#if UNUSED
/* ___unused___	 InheritsClass() and RemoveClass() */
Rboolean InheritsClass(SEXP x, char *name)
{
/* does an object inherit from a class ? */
    SEXP class;
    int i, nclass;
    if (isObject(x)) {
	class = getAttrib(x, R_ClassSymbol);
	nclass = length(class);
	for (i = 0; i < nclass; i++)
	    if (!strcmp(translateChar(STRING_ELT(class, i)), name))
		return TRUE;
    }
    return FALSE;
}

void RemoveClass(SEXP x, char *name)
{
    SEXP class, newclass;
    int i, j, nclass, nmatch;

    if (isObject(x)) {
	PROTECT(x);
	class = getAttrib(x, R_ClassSymbol);
	nclass = length(class);
	nmatch = 0;
	for (i = 0; i < nclass; i++)
	    if (!strcmp(translateChar(STRING_ELT(class, i)), name))
		nmatch++;
	if (nmatch == nclass) {
	    setAttrib(x, R_ClassSymbol, R_NilValue);
	} else if (nmatch > 0) {
	    PROTECT(newclass = allocVector(STRSXP, nclass-nmatch));
	    for (i = 0, j = 0; i < nclass; i++)
		if (strcmp(translateChar(STRING_ELT(class, i)), name))
		    SET_STRING_ELT(newclass, j++, STRING_ELT(class, i));
	    setAttrib(x, R_ClassSymbol, newclass);
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
}
#endif

SEXP attribute_hidden do_inherits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, klass, what, which, rval = R_NilValue /* -Wall */;
    int i, j, nwhat, isvec, nclass;

    checkArity(op, args);

    x = CAR(args);
    klass = R_data_class(x, FALSE);
    nclass = length(klass);

    what = CADR(args);
    if(!isString(what))
	errorcall(call, _("'what' must be a character vector"));
    nwhat = length(what);

    which = CADDR(args);
    if( !isLogical(which) || (length(which) != 1) )
	errorcall(call, _("'which' must be a length 1 logical vector"));
    isvec = asLogical(which);

    if(isvec)
	rval = allocVector(INTSXP, nwhat);

    for(j = 0; j < nwhat; j++) {
	char *ss = translateChar(STRING_ELT(what, j));
	for(i = 0; i < nclass; i++) {
	    if(isvec)
		INTEGER(rval)[j] = 0;
	    if(!strcmp(translateChar(STRING_ELT(klass, i)), ss)) {
		if(isvec)
		   INTEGER(rval)[j] = i+1;
		else
		    return mkTrue();
		break;
	    }
	}
    }
    if(!isvec)
	return mkFalse();
    return rval;
}


/*
   ==============================================================

     code from here on down is support for the methods package 

   ==============================================================
*/

/* standardGeneric:  uses a pointer to R_standardGeneric, to be
   initialized when the methods package is attached.  When and if the
   methods code is automatically included, the pointer will not be
   needed

*/
static R_stdGen_ptr_t R_standardGeneric_ptr = 0;
static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef);
#define NOT_METHODS_DISPATCH_PTR(ptr) (ptr == 0 || ptr == dispatchNonGeneric)

R_stdGen_ptr_t R_get_standardGeneric_ptr()
{
    return R_standardGeneric_ptr;
}

R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t val, SEXP envir)
{
    R_stdGen_ptr_t old = R_standardGeneric_ptr;
    R_standardGeneric_ptr = val;
    if(envir && !isNull(envir))
	R_MethodsNamespace = envir;
    /* just in case ... */
    if(!R_MethodsNamespace)
	R_MethodsNamespace = R_GlobalEnv;
    return old;
}

SEXP R_isMethodsDispatchOn(SEXP onOff) {
    SEXP value = allocVector(LGLSXP, 1);
    Rboolean onOffValue;
    R_stdGen_ptr_t old = R_get_standardGeneric_ptr();
    LOGICAL(value)[0] = !NOT_METHODS_DISPATCH_PTR(old);
    if(length(onOff) > 0) {
	    onOffValue = asLogical(onOff);
	    if(onOffValue == FALSE)
		    R_set_standardGeneric_ptr(0, 0);
	    else if(NOT_METHODS_DISPATCH_PTR(old)) {
		    SEXP call;
		    PROTECT(call = allocList(2));
		    SETCAR(call, install("initMethodsDispatch"));
		    eval(call, R_GlobalEnv); /* only works with
						methods	 attached */
		    UNPROTECT(1);
	    }
    }
    return value;
}

/* simpler version for internal use */

attribute_hidden
Rboolean isMethodsDispatchOn(void)
{
    return !NOT_METHODS_DISPATCH_PTR(R_standardGeneric_ptr);
}


static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef)
{
    /* dispatch the non-generic definition of `name'.  Used to trap
       calls to standardGeneric during the loading of the methods package */
    SEXP e, value, rho, fun, symbol, dot_Generic;
    RCNTXT *cptr;
    /* find a non-generic function */
    symbol = install(translateChar(asChar(name)));
    dot_Generic = install(".Generic");
    for(rho = ENCLOS(env); rho != R_EmptyEnv;
	rho = ENCLOS(rho)) {
	fun = findVarInFrame3(rho, symbol, TRUE);
	if(fun == R_UnboundValue) continue;
	switch(TYPEOF(fun)) {
	case CLOSXP:
	    value = findVarInFrame3(CLOENV(fun), dot_Generic, TRUE);
	    if(value == R_UnboundValue) break;
	case BUILTINSXP:  case SPECIALSXP:
	default:
	    /* in all other cases, go on to the parent environment */
	    break;
	}
	fun = R_UnboundValue;
    }
    fun = SYMVALUE(symbol);
    if(fun == R_UnboundValue)
	error(_("unable to find a non-generic version of function \"%s\""),
	      translateChar(asChar(name)));
    cptr = R_GlobalContext;
    /* check this is the right context */
    while (cptr != R_ToplevelContext) {
	if (cptr->callflag & CTXT_FUNCTION )
	    if (cptr->cloenv == env)
		break;
	cptr = cptr->nextcontext;
    }

    PROTECT(e = duplicate(R_syscall(0, cptr)));
    SETCAR(e, fun);
    /* evaluate a call the non-generic with the same arguments and from
       the same environment as the call to the generic version */
    value = eval(e, cptr->sysparent);
    UNPROTECT(1);
    return value;
}


#ifdef UNUSED
static void load_methods_package()
{
    SEXP e;
    R_set_standardGeneric_ptr(dispatchNonGeneric, NULL);
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, install("library"));
    SETCAR(CDR(e), install("methods"));
    eval(e, R_GlobalEnv);
    UNPROTECT(1);
}
#endif

static SEXP get_this_generic(SEXP args);

SEXP attribute_hidden do_standardGeneric(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, value, fdef; R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr();
    if(!ptr) {
	warning(_("standardGeneric called without methods dispatch enabled (will be ignored)"));
	R_set_standardGeneric_ptr(dispatchNonGeneric, NULL);
	ptr = R_get_standardGeneric_ptr();
    }
    PROTECT(args);
    PROTECT(arg = CAR(args));
    if(!isValidStringF(arg))
      error(_("argument to standardGeneric must be a non-empty character string"));

    PROTECT(fdef = get_this_generic(args));

    if(isNull(fdef))
      error(_("call to standardGeneric(\"%s\") apparently not from the body of that generic function"), translateChar(STRING_ELT(arg, 0)));

    value = (*ptr)(arg, env, fdef);

    UNPROTECT(3);
    return value;
}

static int maxMethodsOffset = 0, curMaxOffset;
typedef enum {NO_METHODS, NEEDS_RESET, HAS_METHODS, SUPPRESSED} prim_methods_t;

static prim_methods_t *prim_methods;
static SEXP *prim_generics;
static SEXP *prim_mlist;
#define DEFAULT_N_PRIM_METHODS 100

SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist)
{
    char *code_string;
    if(!isValidString(code_vec))
	error(_("argument 'code' must be a character string"));
    code_string = translateChar(asChar(code_vec));
    do_set_prim_method(op, code_string, fundef, mlist);
    return(fname);
}

SEXP R_primitive_methods(SEXP op)
{
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	return R_NilValue;
    else {
	SEXP value = prim_mlist[offset];
	return value ? value : R_NilValue;
    }
}

SEXP R_primitive_generic(SEXP op)
{
    int offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	return R_NilValue;
    else {
	SEXP value = prim_generics[offset];
	return value ? value : R_NilValue;
    }
}

/* This is used in the methods package */
SEXP do_set_prim_method(SEXP op, char *code_string, SEXP fundef, SEXP mlist)
{
    int offset = 0;
    prim_methods_t code = NO_METHODS; /* -Wall */
    SEXP value;
    Rboolean errorcase = FALSE;
    switch(code_string[0]) {
    case 'c': /* clear */
	code = NO_METHODS; break;
    case 'r': /* reset */
	code = NEEDS_RESET; break;
    case 's': /* set or suppress */
	switch(code_string[1]) {
	case 'e': code = HAS_METHODS; break;
	case 'u': code = SUPPRESSED; break;
	default: errorcase = TRUE;
	}
	break;
    default:
	errorcase = TRUE;
    }
    if(errorcase) {
	error(_("invalid primitive methods code (\"%s\"): should be \"clear\", \"reset\", \"set\", or \"suppress\""), code_string);
	return R_NilValue;
    }
    switch(TYPEOF(op)) {
    case BUILTINSXP: case SPECIALSXP:
	offset = PRIMOFFSET(op);
	break;
    default:
	error(_("invalid object: must be a primitive function"));
    }
    if(offset >= maxMethodsOffset) {
	int n;
	n = offset + 1;
	if(n < DEFAULT_N_PRIM_METHODS)
	    n = DEFAULT_N_PRIM_METHODS;
	if(n < 2*maxMethodsOffset)
	    n = 2 * maxMethodsOffset;
	if(prim_methods) {
	    int i;

	    prim_methods  = Realloc(prim_methods,  n, prim_methods_t);
	    prim_generics = Realloc(prim_generics, n, SEXP);
	    prim_mlist	  = Realloc(prim_mlist,	   n, SEXP);

	    /* Realloc does not clear the added memory, hence: */
	    for (i = maxMethodsOffset ; i < n ; i++) {
		prim_methods[i]	 = NO_METHODS;
		prim_generics[i] = NULL;
		prim_mlist[i]	 = NULL;
	    }
	}
	else {
	    prim_methods  = Calloc(n, prim_methods_t);
	    prim_generics = Calloc(n, SEXP);
	    prim_mlist	  = Calloc(n, SEXP);
	}
	maxMethodsOffset = n;
    }
    if(offset > curMaxOffset)
	curMaxOffset = offset;
    prim_methods[offset] = code;
    /* store a preserved pointer to the generic function if there is not
       one there currently.  Unpreserve it if no more methods, but don't
       replace it otherwise:  the generic definition is not allowed to
       change while it's still defined! (the stored methods list can,
       however) */
    value = prim_generics[offset];
    if(code == SUPPRESSED) {} /* leave the structure alone */
    else if(code == NO_METHODS && prim_generics[offset]) {
	R_ReleaseObject(prim_generics[offset]);
	prim_generics[offset] = 0;
	prim_mlist[offset] = 0;
    }
    else if(fundef && !isNull(fundef) && !prim_generics[offset]) {
	if(TYPEOF(fundef) != CLOSXP)
	    error(_("the formal definition of a primitive generic must be a function object (got type '%s')"),
		  type2char(TYPEOF(fundef)));
	R_PreserveObject(fundef);
	prim_generics[offset] = fundef;
    }
    if(code==HAS_METHODS) {
	if(!mlist  || isNull(mlist)) {
		/* turning methods back on after a SUPPRESSED */
	}
	else {
	  if(prim_mlist[offset])
	    R_ReleaseObject(prim_mlist[offset]);
	  R_PreserveObject(mlist);
	  prim_mlist[offset] = mlist;
	}
    }
    return value;
}

static SEXP get_primitive_methods(SEXP op, SEXP rho)
{
    SEXP f, e;
    int nprotect = 0;
    f = PROTECT(allocVector(STRSXP, 1));  nprotect++;
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(op)));
    PROTECT(e = allocVector(LANGSXP, 2)); nprotect++;
    SETCAR(e, install("getMethods"));
    SETCAR(CDR(e), f);
    e = eval(e, rho);
    UNPROTECT(nprotect);
    return e;
}

/* get the generic function, defined to be the function definition for
the call to standardGeneric(), or for primitives, passed as the second
argument to standardGeneric.
*/
static SEXP get_this_generic(SEXP args)
{
    SEXP value = R_NilValue; static SEXP gen_name;
    int i, n;
    RCNTXT *cptr; char *fname;

    /* a second argument to the call, if any, is taken as the function */
    if(CDR(args) != R_NilValue)
	return CAR(CDR(args));
    /* else use sys.function (this is fairly expensive-- would be good
     * to force a second argument if possible) */
    PROTECT(args);
    if(!gen_name)
	gen_name = install("generic");
    cptr = R_GlobalContext;
    fname = translateChar(asChar(CAR(args)));
    n = framedepth(cptr);
    /* check for a matching "generic" slot */
    for(i=0;  i<n; i++) {
	SEXP rval = R_sysfunction(i, cptr);
	if(isObject(rval)) {
	    SEXP generic = getAttrib(rval, gen_name);
	    if(TYPEOF(generic) == STRSXP &&
	       !strcmp(translateChar(asChar(generic)), fname)) {
	      value = rval;
	      break;
	    }
	}
    }
    UNPROTECT(1);
    return(value);
}

/* Could there be methods for this op?	Checks
   only whether methods are currently being dispatched and, if so,
   whether methods are currently defined for this op. */
Rboolean R_has_methods(SEXP op)
{
    R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr(); int offset;
    if(NOT_METHODS_DISPATCH_PTR(ptr))
	return(FALSE);
    if(!op || TYPEOF(op) == CLOSXP) /* except for primitives, just test for the package */
	return(TRUE);
    offset = PRIMOFFSET(op);
    if(offset > curMaxOffset || prim_methods[offset] == NO_METHODS
       || prim_methods[offset] == SUPPRESSED)
	return(FALSE);
    return(TRUE);
}

static SEXP deferred_default_object;

SEXP R_deferred_default_method()
{
    if(!deferred_default_object)
	deferred_default_object = install("__Deferred_Default_Marker__");
    return(deferred_default_object);
}


static R_stdGen_ptr_t quick_method_check_ptr = NULL;
void R_set_quick_method_check(R_stdGen_ptr_t value)
{
    quick_method_check_ptr = value;
}

/* try to dispatch the formal method for this primitive op, by calling
   the stored generic function corresponding to the op.	 Requires that
   the methods be set up to return a special object rather than trying
   to evaluate the default (which would get us into a loop). */
SEXP attribute_hidden
R_possible_dispatch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fundef, value, mlist=R_NilValue; 
    int offset; 
    prim_methods_t current;
    offset = PRIMOFFSET(op);
    if(offset < 0 || offset > curMaxOffset)
	error(_("invalid primitive operation given for dispatch"));
    current = prim_methods[offset];
    if(current == NO_METHODS || current == SUPPRESSED)
	return(NULL);
    /* check that the methods for this function have been set */
    if(current == NEEDS_RESET) {
	/* get the methods and store them in the in-core primitive
	   method table.	The entries will be preserved via
	   R_preserveobject, so later we can just grab mlist from
	   prim_mlist */
	do_set_prim_method(op, "suppressed", R_NilValue, mlist);
	PROTECT(mlist = get_primitive_methods(op, rho));
	do_set_prim_method(op, "set", R_NilValue, mlist);
	current = prim_methods[offset]; /* as revised by do_set_prim_method */
	UNPROTECT(1);
    }
    mlist = prim_mlist[offset];
    if(mlist && !isNull(mlist)
       && quick_method_check_ptr) {
	value = (*quick_method_check_ptr)(args, mlist, op);
	if(isPrimitive(value))
	    return(NULL);
	if(isFunction(value))
	    /* found a method, call it */
	    return applyClosure(call, value, args, rho, R_BaseEnv);
	/* else, need to perform full method search */
    }
    fundef = prim_generics[offset];
    if(!fundef || TYPEOF(fundef) != CLOSXP)
	error(_("primitive function \"%s\" has been set for methods but no generic function supplied"),
	      PRIMNAME(op));
    /* To do:  arrange for the setting to be restored in case of an
       error in method search */
    value = applyClosure(call, fundef, args, rho, R_BaseEnv);
    prim_methods[offset] = current;
    if(value == deferred_default_object)
	return NULL;
    else
	return value;
}

SEXP R_do_MAKE_CLASS( char *what)
{
    static SEXP s_getClass = NULL;
    SEXP e, call;
    if(!what)
	error(_("C level MAKE_CLASS macro called with NULL string pointer"));
    if(!s_getClass)
	s_getClass = Rf_install("getClass");
    PROTECT(call = allocVector(LANGSXP, 2));
    SETCAR(call, s_getClass);
    SETCAR(CDR(call), mkString(what));
    e = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return(e);
}

SEXP R_do_new_object(SEXP class_def)
{
    static SEXP s_virtual = NULL, s_prototype, s_className;
    SEXP e, value;
    static SEXP R_packageSymbol = NULL;
    if(!s_virtual) {
	s_virtual = Rf_install("virtual");
	s_prototype = Rf_install("prototype");
	s_className = Rf_install("className");
        R_packageSymbol = install("package");
   }
    if(!class_def)
	error(_("C level NEW macro called with null class definition pointer"));
    e = R_do_slot(class_def, s_virtual);
    if(asLogical(e) != 0)  { /* includes NA, TRUE, or anything other than FALSE */
	e = R_do_slot(class_def, s_className);
	error(_("trying to generate an object from a virtual class (\"%s\")"),
	      translateChar(asChar(e)));
    }
    e = R_do_slot(class_def, s_className);
    value = duplicate(R_do_slot(class_def, s_prototype));
    if(TYPEOF(value) == S4SXP || getAttrib(e, R_packageSymbol) != R_NilValue)
      { /* Anything but an object from a base "class" (numeric, matrix,..) */
	  setAttrib(value, R_ClassSymbol, e);
	  SET_S4_OBJECT(value);
      }
    return value;
}

Rboolean R_seemsS4Object(SEXP object)  {
  static SEXP R_packageSymbol = NULL;
  SEXP klass;
  if(!isObject(object))
    return FALSE;
  if(TYPEOF(object) == S4SXP)
    return TRUE;
  if(!R_packageSymbol)
    R_packageSymbol = install("package");
  klass = getAttrib(object, R_ClassSymbol);
  return (klass != R_NilValue &&
	  getAttrib(klass, R_packageSymbol) != R_NilValue) ?
    TRUE: FALSE;
}


SEXP R_isS4Object(SEXP object) {
  /* wanted: return isS4(object) ? mkTrue() : mkFalse(); */
  return IS_S4_OBJECT(object) ? mkTrue() : mkFalse(); ;
}

SEXP R_setS4Object(SEXP object, SEXP onOff) {
    Rboolean flag = asLogical(onOff);
    /* wanted     return asS4(object, flag); */
   if(flag == IS_S4_OBJECT(object))
        return object;
    if(NAMED(object) == 2)
        object = duplicate(object);
    if(flag) SET_S4_OBJECT(object);
    else UNSET_S4_OBJECT(object);
    return object;
}

SEXP R_get_primname(SEXP object) 
{
    SEXP f;
    if(TYPEOF(object) != BUILTINSXP && TYPEOF(object) != SPECIALSXP)
	error(_("'R_get_primname' called on a non-primitive"));
    f = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(object)));
    UNPROTECT(1);
    return f;
}
