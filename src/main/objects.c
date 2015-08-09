/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2002-3     The R Foundation
 *  Copyright (C) 1999-2015  The R Core Team.
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
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/*  This module contains support for S-style generic */
/*  functions and "class" support.  Gag, barf ...  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <R_ext/RS.h> /* for Calloc, Realloc and for S4 object bit */

static SEXP GetObject(RCNTXT *cptr)
{
    SEXP s, b, formals, tag;

    b = cptr->callfun;
    if (TYPEOF(b) != CLOSXP) error(_("generic 'function' is not a function"));
    formals = FORMALS(b);

    tag = TAG(formals);
    if (tag != R_NilValue && tag != R_DotsSymbol) {
	s = NULL;
	/** exact matches **/
	for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
	    if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 1)) {
		if (s != NULL)
		    error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		else
		    s = CAR(b);
	    }

	if (s == NULL)
	    /** partial matches **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 0)) {
		    if ( s != NULL)
			error(_("formal argument \"%s\" matched by multiple actual arguments"), tag);
		    else
			s = CAR(b);
		}
	if (s == NULL)
	    /** first untagged argument **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b))
		if (TAG(b) == R_NilValue )
		{
		    s = CAR(b);
		    break;
		}
	if (s == NULL)
	    s = CAR(cptr->promargs);
/*
	    error("failed to match argument for dispatch");
*/
    }
    else
	s = CAR(cptr->promargs);

    if (TYPEOF(s) == PROMSXP) {
	if (PRVALUE(s) == R_UnboundValue)
	    s = eval(s, R_BaseEnv);
	else
	    s = PRVALUE(s);
    }
    return(s);
}

static SEXP applyMethod(SEXP call, SEXP op, SEXP args, SEXP rho, SEXP newvars)
{
    SEXP ans;
    if (TYPEOF(op) == SPECIALSXP) {
	int save = R_PPStackTop, flag = PRIMPRINT(op);
	const void *vmax = vmaxget();
	R_Visible = flag != 1;
	ans = PRIMFUN(op) (call, op, args, rho);
	if (flag < 2) R_Visible = flag != 1;
	check_stack_balance(op, save);
	vmaxset(vmax);
    }
    /* In other places we add a context to builtins when profiling,
       but we have not bothered here (as there seem to be no primitives
       used as methods, and this would have to be a primitive to be
       found).
     */
    else if (TYPEOF(op) == BUILTINSXP) {
	int save = R_PPStackTop, flag = PRIMPRINT(op);
	const void *vmax = vmaxget();
	PROTECT(args = evalList(args, rho, call, 0));
	R_Visible = flag != 1;
	ans = PRIMFUN(op) (call, op, args, rho);
	if (flag < 2) R_Visible = flag != 1;
	UNPROTECT(1);
	check_stack_balance(op, save);
	vmaxset(vmax);
    }
    else if (TYPEOF(op) == CLOSXP) {
	ans = applyClosure(call, op, args, rho, newvars);
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

/* R_MethodsNamespace is initialized to R_GlobalEnv when R is
   initialized.  If it set to the methods namespace when the latter is
   loaded, and back to R_GlobalEnv when it is unloaded. */

#ifdef S3_for_S4_warn /* not currently used */
static SEXP s_check_S3_for_S4 = NULL;
void R_warn_S3_for_S4(SEXP method) {
  SEXP call;
  if(!s_check_S3_for_S4)
    s_check_S3_for_S4 = install(".checkS3forS4");
  PROTECT(call = lang2(s_check_S3_for_S4, method));
  eval(call, R_MethodsNamespace);
  UNPROTECT(1);
}
#endif

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

attribute_hidden
SEXP R_LookupMethod(SEXP method, SEXP rho, SEXP callrho, SEXP defrho)
{
    SEXP val;
    static SEXP s_S3MethodsTable = NULL;

    if (TYPEOF(callrho) != ENVSXP) {
	if (TYPEOF(callrho) == NILSXP)
	    error(_("use of NULL environment is defunct"));
	else
	    error(_("bad generic call environment"));
    }
    if (defrho == R_BaseEnv)
	defrho = R_BaseNamespace;
    else if (TYPEOF(defrho) != ENVSXP) {
	if (TYPEOF(defrho) == NILSXP)
	    error(_("use of NULL environment is defunct"));
	else
	    error(_("bad generic definition environment"));
    }

    /* This evaluates promises */
    val = findVar1(method, callrho, FUNSXP, TRUE);
    if (isFunction(val))
	return val;
    else {
	/* We assume here that no one registered a non-function */
	if (!s_S3MethodsTable)
	    s_S3MethodsTable = install(".__S3MethodsTable__.");
	SEXP table = findVarInFrame3(defrho,
				     s_S3MethodsTable,
				     TRUE);
	if (TYPEOF(table) == PROMSXP) {
	    PROTECT(table);
	    table = eval(table, R_BaseEnv);
	    UNPROTECT(1);
	}
	if (TYPEOF(table) == ENVSXP) {
	    val = findVarInFrame3(table, method, TRUE);
	    if (TYPEOF(val) == PROMSXP) {
		PROTECT(val);
		val = eval(val, rho);
		UNPROTECT(1);
	    }
	    return val;
	}
	return R_UnboundValue;
    }
}

#ifdef UNUSED
static int match_to_obj(SEXP arg, SEXP obj) {
  return (arg == obj) ||
    (TYPEOF(arg) == PROMSXP && PRVALUE(arg) == obj);
}
#endif

/* look up the class name in the methods package table of S3 classes
   which should be explicitly converted when an S3 method is applied
   to an object from an S4 subclass.
*/
int isBasicClass(const char *ss) {
    static SEXP s_S3table = NULL;
    if(!s_S3table) {
      s_S3table = findVarInFrame3(R_MethodsNamespace, install(".S3MethodsClasses"), TRUE);
      if(s_S3table == R_UnboundValue)
	error(_("no '.S3MethodsClass' table, cannot use S4 objects with S3 methods ('methods' package not attached?)"));
      if (TYPEOF(s_S3table) == PROMSXP)  /* findVar... ignores lazy data */
	s_S3table = eval(s_S3table, R_MethodsNamespace);
    }
    if(s_S3table == R_UnboundValue)
      return FALSE; /* too screwed up to do conversions */
    return findVarInFrame3(s_S3table, install(ss), FALSE) != R_UnboundValue;
}

/* Note that ./attrib.c 's S4_extends() has an alternative
   'sanity check for methods package available' */
Rboolean R_has_methods_attached(void) {
    return(
	isMethodsDispatchOn() &&
	// based on unlockBinding() in ../library/methods/R/zzz.R  {since 2003}:
	!R_BindingIsLocked(install(".BasicFunsList"), R_MethodsNamespace));
}

static R_INLINE
SEXP addS3Var(SEXP vars, SEXP name, SEXP value) {

    SEXP res = CONS(value, vars);
    SET_TAG(res, name);
    return res;
}

attribute_hidden
SEXP createS3Vars(SEXP dotGeneric, SEXP dotGroup, SEXP dotClass, SEXP dotMethod,
		  SEXP dotGenericCallEnv, SEXP dotGenericDefEnv) {

    SEXP v = R_NilValue;
    v = addS3Var(v, R_dot_GenericDefEnv, dotGenericDefEnv);
    v = addS3Var(v, R_dot_GenericCallEnv, dotGenericCallEnv);
    v = addS3Var(v, R_dot_Group, dotGroup);
    v = addS3Var(v, R_dot_Method, dotMethod);
    v = addS3Var(v, R_dot_Class, dotClass);
    v = addS3Var(v, R_dot_Generic, dotGeneric);

    return v;
}


static
SEXP dispatchMethod(SEXP op, SEXP sxp, SEXP dotClass, RCNTXT *cptr, SEXP method,
		    const char *generic, SEXP rho, SEXP callrho, SEXP defrho) {

    SEXP newvars = PROTECT(createS3Vars(
	PROTECT(mkString(generic)),
	R_BlankScalarString,
	dotClass,
	PROTECT(ScalarString(PRINTNAME(method))),
	callrho,
	defrho
    ));

    /* Create a new environment without any */
    /* of the formals to the generic in it. */

    if (TYPEOF(op) == CLOSXP) {
	SEXP formals = FORMALS(op);
	SEXP s, t;
	int matched;

	for (s = FRAME(cptr->cloenv); s != R_NilValue; s = CDR(s)) {
	    matched = 0;
	    for (t = formals; t != R_NilValue; t = CDR(t))
		if (TAG(t) == TAG(s)) {
		    matched = 1;
		    break;
		}
	    if (!matched) {
		UNPROTECT(1); /* newvars */
		newvars = PROTECT(CONS(CAR(s), newvars));
		SET_TAG(newvars, TAG(s));
	    }
	}
    }

    if( (RDEBUG(op) && R_current_debug_state()) || RSTEP(op) ) {
	SET_RSTEP(sxp, 1);
    }

    SEXP newcall =  PROTECT(duplicate(cptr->call));
    SETCAR(newcall, method);
    R_GlobalContext->callflag = CTXT_GENERIC;
    SEXP matchedarg = PROTECT(cptr->promargs); /* ? is this PROTECT needed ? */
    SEXP ans = applyMethod(newcall, sxp, matchedarg, rho, newvars);
    R_GlobalContext->callflag = CTXT_RETURN;
    UNPROTECT(5); /* "generic,method", newvars, newcall, matchedarg */

    return ans;
}

attribute_hidden
int usemethod(const char *generic, SEXP obj, SEXP call, SEXP args,
	      SEXP rho, SEXP callrho, SEXP defrho, SEXP *ans)
{
    SEXP klass, method, sxp;
    SEXP op;
    int i, nclass;
    RCNTXT *cptr;

    /* Get the context which UseMethod was called from. */

    cptr = R_GlobalContext;
    op = cptr->callfun;
    PROTECT(klass = R_data_class2(obj));

    nclass = length(klass);
    for (i = 0; i < nclass; i++) {
	const void *vmax = vmaxget();
	const char *ss = translateChar(STRING_ELT(klass, i));
	method = installS3Signature(generic, ss);
	vmaxset(vmax);
	sxp = R_LookupMethod(method, rho, callrho, defrho);
	if (isFunction(sxp)) {
	    if(method == R_SortListSymbol && CLOENV(sxp) == R_BaseNamespace)
		continue; /* kludge because sort.list is not a method */
	    PROTECT(sxp);
	    if (i > 0) {
		SEXP dotClass = PROTECT(stringSuffix(klass, i));
		setAttrib(dotClass, R_PreviousSymbol, klass);
		*ans = dispatchMethod(op, sxp, dotClass, cptr, method, generic,
				      rho, callrho, defrho);
		UNPROTECT(1); /* dotClass */
	    } else {
		*ans = dispatchMethod(op, sxp, klass, cptr, method, generic,
				      rho, callrho, defrho);
	    }
	    UNPROTECT(2); /* klass, sxp */
	    return 1;
	}
    }
    method = installS3Signature(generic, "default");
    PROTECT(sxp = R_LookupMethod(method, rho, callrho, defrho));
    if (isFunction(sxp)) {
	*ans = dispatchMethod(op, sxp, R_NilValue, cptr, method, generic,
			      rho, callrho, defrho);
	UNPROTECT(2); /* klass, sxp */
	return 1;
    }
    UNPROTECT(2); /* klass, sxp */
    cptr->callflag = CTXT_RETURN;
    return 0;
}

/* Note: "do_usemethod" is not the only entry point to
   "usemethod". Things like [ and [[ call usemethod directly,
   hence do_usemethod should just be an interface to usemethod.
*/

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden NORET do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, generic = R_NilValue /* -Wall */, obj, val;
    SEXP callenv, defenv;
    SEXP argList;
    RCNTXT *cptr;
    static SEXP do_usemethod_formals = NULL;

    if (do_usemethod_formals == NULL)
	do_usemethod_formals = allocFormalsList2(install("generic"),
						 install("object"));

    PROTECT(argList = matchArgs(do_usemethod_formals, args, call));
    if (CAR(argList) == R_MissingArg)
	errorcall(call, _("there must be a 'generic' argument"));
    else
	PROTECT(generic = eval(CAR(argList), env));
    if(!isString(generic) || LENGTH(generic) != 1)
	errorcall(call, _("'generic' argument must be a character string"));


    /* get environments needed for dispatching.
       callenv = environment from which the generic was called
       defenv = environment where the generic was defined */
    cptr = R_GlobalContext;
    if ( !(cptr->callflag & CTXT_FUNCTION) || cptr->cloenv != env)
	errorcall(call, _("'UseMethod' used in an inappropriate fashion"));
    callenv = cptr->sysparent;
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
    val = findVar1(installTrChar(STRING_ELT(generic, 0)),
		   ENCLOS(env), FUNSXP, TRUE); /* That has evaluated promises */
    if(TYPEOF(val) == CLOSXP) defenv = CLOENV(val);
    else defenv = R_BaseNamespace;

    if (CADR(argList) != R_MissingArg)
	PROTECT(obj = eval(CADR(argList), env));
    else
	PROTECT(obj = GetObject(cptr));

    if (usemethod(translateChar(STRING_ELT(generic, 0)), obj, call, CDR(args),
		  env, callenv, defenv, &ans) == 1) {
	UNPROTECT(3); /* obj, generic, argList */
	findcontext(CTXT_RETURN, env, ans); /* does not return */
    }
    else {
	SEXP klass;
	int nclass;
	char cl[1000];
	PROTECT(klass = R_data_class2(obj));
	nclass = length(klass);
	if (nclass == 1)
	    strcpy(cl, translateChar(STRING_ELT(klass, 0)));
	else {
	    strcpy(cl, "c('");
	    for (int i = 0; i < nclass; i++) {
		if (i > 0) strcat(cl, "', '");
		strcat(cl, translateChar(STRING_ELT(klass, i)));
	    }
	    strcat(cl, "')");
	}
	errorcall(call, _("no applicable method for '%s' applied to an object of class \"%s\""),
		  translateChar(STRING_ELT(generic, 0)), cl);
    }
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
		    if(TAG(CDR(s)) == TAG(t)) {
			found = 1;
			break;
		    }
		if( !found ) {
			SETCDR(s, allocList(1));
			SET_TAG(CDR(s), TAG(t));
			SETCAR(CDR(s), duplicate(CAR(t)));
		}
	}
    }
    return call;
}

/*
   equalS3Signature: compares "signature" and "left.right"
   all arguments must be non-null
*/
static
Rboolean equalS3Signature(const char *signature, const char *left,
			 const char *right) {

    const char *s = signature;
    const char *a;

    for(a = left; *a; s++, a++) {
	if (*s != *a)
	    return FALSE;
    }
    if (*s++ != '.')
	return FALSE;
    for(a = right; *a; s++, a++) {
	if (*s != *a)
	    return FALSE;
    }
    return (*s == 0) ? TRUE : FALSE;
}

/* If NextMethod has any arguments the first must be the generic */
/* the second the object and any remaining are matched with the */
/* formals of the chosen method. */

#define ARGUSED(x) LEVELS(x)

/* This is a special .Internal */
SEXP attribute_hidden do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *sb, *sg, *sk;
    SEXP ans, s, t, klass, method, matchedarg, generic;
    SEXP nextfun, nextfunSignature;
    SEXP sysp, formals, newcall;
    SEXP group, basename;
    SEXP callenv, defenv;
    RCNTXT *cptr;
    int i, j;

    cptr = R_GlobalContext;
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

    readS3VarsFromFrame(sysp, &generic, &group, &klass, &method,
			&callenv, &defenv);

    /* Find dispatching environments. Promises shouldn't occur, but
       check to be on the safe side.  If the variables are not in the
       environment (the method was called outside a method dispatch)
       then chose reasonable defaults. */
    if (TYPEOF(callenv) == PROMSXP)
	callenv = eval(callenv, R_BaseEnv);
    else if (callenv == R_UnboundValue)
	callenv = env;
    if (TYPEOF(defenv) == PROMSXP) defenv = eval(defenv, R_BaseEnv);
    else if (defenv == R_UnboundValue) defenv = R_GlobalEnv;

    /* set up the arglist */
    s = cptr->callfun;

    if (TYPEOF(s) != CLOSXP){ /* R_LookupMethod looked for a function */
	if (s == R_UnboundValue)
	    error(_("no calling generic was found: was a method called directly?"));
	else
	    errorcall(R_NilValue,
		  _("'function' is not a function, but of type %d"),
		  TYPEOF(s));
    }
    /* get formals and actuals; attach the names of the formals to
       the actuals, expanding any ... that occurs */
    formals = FORMALS(s);
    PROTECT(matchedarg = patchArgsByActuals(formals, cptr->promargs, cptr->cloenv));

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
	error(_("wrong argument ..."));

    /*
      .Class is used to determine the next method; if it doesn't
      exist the first argument to the current method is used
      the second argument to NextMethod is another option but
      isn't currently used).
    */
    if (klass == R_UnboundValue) {
	/* we can get the object from actuals directly, but this
	   branch seems to be very cold if not dead */
	s = GetObject(cptr);
	if (!isObject(s)) error(_("object not specified"));
	klass = getAttrib(s, R_ClassSymbol);
    }

    /* the generic comes from either the sysparent or it's named */
    if (generic == R_UnboundValue)
	generic = eval(CAR(args), env);
    if (generic == R_NilValue)
	error(_("generic function not specified"));
    PROTECT(generic);

    if (!isString(generic) || LENGTH(generic) != 1)
	error(_("invalid generic argument to 'NextMethod'"));

    if (CHAR(STRING_ELT(generic, 0))[0] == '\0')
	error(_("generic function not specified"));

    /* determine whether we are in a Group dispatch */
    /* determine the root: either the group or the generic will be it */
    if (group == R_UnboundValue) {
	group = R_BlankScalarString;
	basename = generic;
    } else {
	if (!isString(group) || LENGTH(group) != 1)
	    error(_("invalid 'group' argument found in 'NextMethod'"));
	if (CHAR(STRING_ELT(group, 0))[0] == '\0') basename = generic;
	else basename = group;
    }
    PROTECT(group);

    nextfun = R_NilValue;
    nextfunSignature = R_NilValue;

    /*
       Find the method currently being invoked and jump over the current call
       If t is R_UnboundValue then we called the current method directly
    */
    const void *vmax = vmaxget(); /* needed for translateChar */
    const char *b = NULL;
    if (method != R_UnboundValue) {
	if (!isString(method))
	    error(_("wrong value for .Method"));
	for(i = 0; i < LENGTH(method); i++) {
	    b = translateChar(STRING_ELT(method, i));
	    if (strlen(b)) break;
	}
	/* for binary operators check that the second argument's method
	   is the same or absent */
	for(j = i; j < LENGTH(method); j++) {
	    const char *bb = translateChar(STRING_ELT(method, j));
	    if (strlen(bb) && strcmp(b,bb))
		warning(_("Incompatible methods ignored"));
	}
    }
    else {
	b = CHAR(PRINTNAME(CAR(cptr->call)));
    }

    sb = translateChar(STRING_ELT(basename, 0));
    Rboolean foundSignature = FALSE;
    for (j = 0; j < LENGTH(klass); j++) {
	sk = translateChar(STRING_ELT(klass, j));
	if (equalS3Signature(b, sb, sk)) { /* b == sb.sk */
	    foundSignature = TRUE;
	    break;
	}
    }

    if (foundSignature) /* we found a match and start from there */
      j++;
    else
      j = 0;  /* no match so start with the first element of .Class */

    /* we need the value of i on exit from the for loop to figure out
	   how many classes to drop. */

    sg = translateChar(STRING_ELT(generic, 0));
    for (i = j ; i < LENGTH(klass); i++) {
	sk = translateChar(STRING_ELT(klass, i));
	nextfunSignature = installS3Signature(sg, sk);
	nextfun = R_LookupMethod(nextfunSignature, env, callenv, defenv);
	if (isFunction(nextfun)) break;
	if (group != R_UnboundValue) {
	    /* if not Generic.foo, look for Group.foo */
	    nextfunSignature = installS3Signature(sb, sk);
	    nextfun = R_LookupMethod(nextfunSignature, env, callenv, defenv);
	    if(isFunction(nextfun))
		break;
	}
	if (isFunction(nextfun))
	    break;
    }
    if (!isFunction(nextfun)) {
	nextfunSignature = installS3Signature(sg, "default");
	nextfun = R_LookupMethod(nextfunSignature, env, callenv, defenv);
	/* If there is no default method, try the generic itself,
	   provided it is primitive or a wrapper for a .Internal
	   function of the same name.
	 */
	if (!isFunction(nextfun)) {
	    t = install(sg);
	    nextfun = findVar(t, env);
	    if (TYPEOF(nextfun) == PROMSXP) {
		PROTECT(nextfun);
		nextfun = eval(nextfun, env);
		UNPROTECT(1);
	    }
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
    PROTECT(nextfun);
    PROTECT(s = stringSuffix(klass, i));
    setAttrib(s, R_PreviousSymbol, klass);
    /* It is possible that if a method was called directly that
	'method' is unset */
    if (method != R_UnboundValue) {
	/* for Ops we need `method' to be a vector */
	PROTECT(method = duplicate(method));
	for(j = 0; j < LENGTH(method); j++) {
	    if (strlen(CHAR(STRING_ELT(method,j))))
		SET_STRING_ELT(method, j,  PRINTNAME(nextfunSignature));
	}
    } else
	PROTECT(method = PRINTNAME(nextfunSignature));

    SEXP newvars = PROTECT(createS3Vars(
	generic,
	group,
	s,
	method,
	callenv,
	defenv
    ));

    SETCAR(newcall, nextfunSignature);

    /* applyMethod expects that the parent of the caller is the caller
       of the generic, so fixup by brute force. This should fix
       PR#15267 --pd */
    R_GlobalContext->sysparent = callenv;

    ans = applyMethod(newcall, nextfun, matchedarg, env, newvars);
    vmaxset(vmax);
    UNPROTECT(8);
    return(ans);
}

/* primitive */
SEXP attribute_hidden do_unclass(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    if (isObject(CAR(args))) {
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
	if (MAYBE_REFERENCED(CAR(args)))
	    SETCAR(args, shallow_duplicate(CAR(args)));
	setAttrib(CAR(args), R_ClassSymbol, R_NilValue);
    }
    return CAR(args);
}



/* NOTE: Fast  inherits(x, what)    in ../include/Rinlinedfuns.h
 * ----        ----------------- */
/** C API for  R  inherits(x, what, which)
 *
 * @param x any R object
 * @param what character vector
 * @param which logical: "want vector result" ?
 *
 * @return if which is false, logical TRUE or FALSE
 *	   if which is true, integer vector of length(what) ..
 */
static SEXP inherits3(SEXP x, SEXP what, SEXP which)
{
    const void *vmax = vmaxget();
    SEXP klass, rval = R_NilValue /* -Wall */;

    if(IS_S4_OBJECT(x))
	PROTECT(klass = R_data_class2(x));
    else
	PROTECT(klass = R_data_class(x, FALSE));

    if(!isString(what))
	error(_("'what' must be a character vector"));
    int j, nwhat = LENGTH(what);

    if( !isLogical(which) || (LENGTH(which) != 1) )
	error(_("'which' must be a length 1 logical vector"));
    int isvec = asLogical(which);

#ifdef _be_too_picky_
    if(IS_S4_OBJECT(x) && nwhat == 1 && !isvec &&
       !isNull(R_getClassDef(translateChar(STRING_ELT(what, 0)))))
	warning(_("use 'is()' instead of 'inherits()' on S4 objects"));
#endif

    if(isvec)
	PROTECT(rval = allocVector(INTSXP, nwhat));

    for(j = 0; j < nwhat; j++) {
	const char *ss = translateChar(STRING_ELT(what, j));
	int i = stringPositionTr(klass, ss);
	if (isvec)
	    INTEGER(rval)[j] = i+1; /* 0 when ss is not in klass */
	else if (i >= 0) {
	    vmaxset(vmax);
	    UNPROTECT(1);
	    return mkTrue();
	}
    }
    vmaxset(vmax);
    if(!isvec) {
	UNPROTECT(1);
	return mkFalse();
    }
    UNPROTECT(2);
    return rval;
}

SEXP attribute_hidden do_inherits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    return inherits3(/* x = */ CAR(args),
		     /* what = */ CADR(args),
		     /* which = */ CADDR(args));
}


/*
   ==============================================================

     code from here on down is support for the methods package

   ==============================================================
*/

/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 * @param rho  the environment in which the class definitions exist.
 *
 * @return index of match or -1 for no match
 */
int R_check_class_and_super(SEXP x, const char **valid, SEXP rho)
{
    int ans;
    SEXP cl = PROTECT(asChar(getAttrib(x, R_ClassSymbol)));
    const char *class = CHAR(cl);
    for (ans = 0; ; ans++) {
	if (!strlen(valid[ans])) // empty string
	    break;
	if (!strcmp(class, valid[ans])) {
	    UNPROTECT(1); /* cl */
	    return ans;
	}
    }
    /* if not found directly, now search the non-virtual super classes :*/
    if(IS_S4_OBJECT(x)) {
	/* now try the superclasses, i.e.,  try   is(x, "....");  superCl :=
	   .selectSuperClasses(getClass("....")@contains, dropVirtual=TRUE)  */
	SEXP classExts, superCl, _call;
	static SEXP s_contains = NULL, s_selectSuperCl = NULL;
	int i;
	if(!s_contains) {
	    s_contains      = install("contains");
	    s_selectSuperCl = install(".selectSuperClasses");
	}
	SEXP classDef = PROTECT(R_getClassDef(class));
	PROTECT(classExts = R_do_slot(classDef, s_contains));
	PROTECT(_call = lang3(s_selectSuperCl, classExts,
			      /* dropVirtual = */ ScalarLogical(1)));
	superCl = eval(_call, rho);
	UNPROTECT(3); /* _call, classExts, classDef */
	PROTECT(superCl);
	for(i=0; i < LENGTH(superCl); i++) {
	    const char *s_class = CHAR(STRING_ELT(superCl, i));
	    for (ans = 0; ; ans++) {
		if (!strlen(valid[ans]))
		    break;
		if (!strcmp(s_class, valid[ans])) {
		    UNPROTECT(2); /* superCl, cl */
		    return ans;
		}
	    }
	}
	UNPROTECT(1); /* superCl */
    }
    UNPROTECT(1); /* cl */
    return -1;
}


/**
 * Return the 0-based index of an is() match in a vector of class-name
 * strings terminated by an empty string.  Returns -1 for no match.
 * Strives to find the correct environment() for is(), using .classEnv()
 * (from \pkg{methods}).
 *
 * @param x  an R object, about which we want is(x, .) information.
 * @param valid vector of possible matches terminated by an empty string.
 *
 * @return index of match or -1 for no match
 */
int R_check_class_etc(SEXP x, const char **valid)
{
    static SEXP meth_classEnv = NULL;
    SEXP cl = getAttrib(x, R_ClassSymbol), rho = R_GlobalEnv, pkg;
    if(!meth_classEnv)
	meth_classEnv = install(".classEnv");

    pkg = getAttrib(cl, R_PackageSymbol); /* ==R== packageSlot(class(x)) */
    if(!isNull(pkg)) { /* find  rho := correct class Environment */
	SEXP clEnvCall;
	// FIXME: fails if 'methods' is not loaded.
	PROTECT(clEnvCall = lang2(meth_classEnv, cl));
	rho = eval(clEnvCall, R_MethodsNamespace);
	UNPROTECT(1);
	if(!isEnvironment(rho))
	    error(_("could not find correct environment; please report!"));
    }
    PROTECT(rho);
    int res = R_check_class_and_super(x, valid, rho);
    UNPROTECT(1);
    return res;
}

/* standardGeneric:  uses a pointer to R_standardGeneric, to be
   initialized when the methods namespace is loaded,
   via R_initMethodDispatch.
*/
static R_stdGen_ptr_t R_standardGeneric_ptr = 0;
static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef);
#define NOT_METHODS_DISPATCH_PTR(ptr) (ptr == 0 || ptr == dispatchNonGeneric)

static
R_stdGen_ptr_t R_get_standardGeneric_ptr(void)
{
    return R_standardGeneric_ptr;
}

/* Also called from R_initMethodDispatch in methods C code, which is
   called when the methods namespace is loaded. */
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

// R's .isMethodsDispatchOn() -> do_S4on() ->
static SEXP R_isMethodsDispatchOn(SEXP onOff)
{
    R_stdGen_ptr_t old = R_get_standardGeneric_ptr();
    int ival =  !NOT_METHODS_DISPATCH_PTR(old);
    if(length(onOff) > 0) {
	Rboolean onOffValue = asLogical(onOff);
	if(onOffValue == NA_INTEGER)
	    error(_("'onOff' must be TRUE or FALSE"));
	else if(onOffValue == FALSE)
	    R_set_standardGeneric_ptr(NULL, R_GlobalEnv);
	// TRUE is not currently used
	else if(NOT_METHODS_DISPATCH_PTR(old)) {
	    // so not already on
	    // This may not work correctly: the default arg is incorrect.
	    warning("R_isMethodsDispatchOn(TRUE) called -- may not work correctly");
	    SEXP call = PROTECT(lang1(install("initMethodDispatch")));
	    eval(call, R_MethodsNamespace); // only works with methods loaded
	    UNPROTECT(1);
	}
    }
    return ScalarLogical(ival);
}

/* simpler version for internal use, in attrib.c and print.c */
attribute_hidden
Rboolean isMethodsDispatchOn(void)
{
    return !NOT_METHODS_DISPATCH_PTR(R_standardGeneric_ptr);
}


/* primitive for .isMethodsDispatchOn
   This is generally called without an arg, but is call with
   onOff=FALSE when package methods is detached/unloaded.

   It seems it is not currently called with onOff = TRUE (and would
   not have worked prior to 3.0.2).
*/
attribute_hidden
SEXP do_S4on(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(length(args) == 0) return ScalarLogical(isMethodsDispatchOn());
    return R_isMethodsDispatchOn(CAR(args));
}


static SEXP dispatchNonGeneric(SEXP name, SEXP env, SEXP fdef)
{
    /* dispatch the non-generic definition of `name'.  Used to trap
       calls to standardGeneric during the loading of the methods package */
    SEXP e, value, rho, fun, symbol;
    RCNTXT *cptr;

    /* find a non-generic function */
    symbol = installTrChar(asChar(name));
    for(rho = ENCLOS(env); rho != R_EmptyEnv;
	rho = ENCLOS(rho)) {
	fun = findVarInFrame3(rho, symbol, TRUE);
	if(fun == R_UnboundValue) continue;
	switch(TYPEOF(fun)) {
	case CLOSXP:
	    value = findVarInFrame3(CLOENV(fun), R_dot_Generic, TRUE);
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


static SEXP get_this_generic(SEXP args);

SEXP attribute_hidden do_standardGeneric(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, value, fdef; R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr();

    checkArity(op, args);
    check1arg(args, call, "f");

    if(!ptr) {
	warningcall(call,
		    _("'standardGeneric' called without 'methods' dispatch enabled (will be ignored)"));
	R_set_standardGeneric_ptr(dispatchNonGeneric, NULL);
	ptr = R_get_standardGeneric_ptr();
    }

    checkArity(op, args); /* set to -1 */
    arg = CAR(args);
    if(!isValidStringF(arg))
	errorcall(call,
		  _("argument to 'standardGeneric' must be a non-empty character string"));

    PROTECT(fdef = get_this_generic(args));

    if(isNull(fdef))
	error(_("call to standardGeneric(\"%s\") apparently not from the body of that generic function"), translateChar(STRING_ELT(arg, 0)));

    value = (*ptr)(arg, env, fdef);

    UNPROTECT(1);
    return value;
}

static int maxMethodsOffset = 0, curMaxOffset;
static Rboolean allowPrimitiveMethods = TRUE;
typedef enum {NO_METHODS, NEEDS_RESET, HAS_METHODS, SUPPRESSED} prim_methods_t;

static prim_methods_t *prim_methods;
static SEXP *prim_generics;
static SEXP *prim_mlist;
#define DEFAULT_N_PRIM_METHODS 100

// Called from methods package, ../library/methods/src/methods_list_dispatch.c
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist)
{
    const char *code_string;
    const void *vmax = vmaxget();
    if(!isValidString(code_vec))
	error(_("argument '%s' must be a character string"), "code");
    code_string = translateChar(asChar(code_vec));
    /* with a NULL op, turns all primitive matching off or on (used to avoid possible infinite
     recursion in methods computations*/
    if(op == R_NilValue) {
	SEXP value = allowPrimitiveMethods ? mkTrue() : mkFalse();
	switch(code_string[0]) {
	case 'c': case 'C':/* clear */
	    allowPrimitiveMethods = FALSE; break;
	case 's': case 'S': /* set */
	    allowPrimitiveMethods = TRUE; break;
	default: /* just report the current state */
	    break;
	}
	return value;
    }
    do_set_prim_method(op, code_string, fundef, mlist);
    vmaxset(vmax);
    return fname;
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

// used in the methods package, but also here
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef,
			SEXP mlist)
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
    if(code == HAS_METHODS) {
	if(!mlist  || isNull(mlist)) {
	    /* turning methods back on after a SUPPRESSED */
	} else {
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
    SEXP f, e, val;
    int nprotect = 0;
    f = PROTECT(allocVector(STRSXP, 1));  nprotect++;
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(op)));
    PROTECT(e = allocVector(LANGSXP, 2)); nprotect++;
    SETCAR(e, install("getGeneric"));
    val = CDR(e); SETCAR(val, f);
    val = eval(e, rho);
    /* a rough sanity check that this looks like a generic function */
    if(TYPEOF(val) != CLOSXP || !IS_S4_OBJECT(val))
	error(_("object returned as generic function \"%s\" does not appear to be one"), PRIMNAME(op));
    UNPROTECT(nprotect);
    return CLOENV(val);
}


/* get the generic function, defined to be the function definition for
the call to standardGeneric(), or for primitives, passed as the second
argument to standardGeneric.
*/
static SEXP get_this_generic(SEXP args)
{
    const void *vmax = vmaxget();
    SEXP value = R_NilValue; static SEXP gen_name;
    int i, n;
    RCNTXT *cptr;
    const char *fname;

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
	    PROTECT(rval);
	    SEXP generic = getAttrib(rval, gen_name);
	    if(TYPEOF(generic) == STRSXP &&
	       !strcmp(translateChar(asChar(generic)), fname)) {
	      value = rval;
	      UNPROTECT(1); /* rval */
	      break;
	    }
	    UNPROTECT(1); /* rval */
	}
    }
    UNPROTECT(1);
    vmaxset(vmax);

    return value;
}

/* Could there be methods for this op?	Checks
   only whether methods are currently being dispatched and, if so,
   whether methods are currently defined for this op. */
attribute_hidden
Rboolean R_has_methods(SEXP op)
{
    R_stdGen_ptr_t ptr = R_get_standardGeneric_ptr(); int offset;
    if(NOT_METHODS_DISPATCH_PTR(ptr))
	return(FALSE);
    if(!op || TYPEOF(op) == CLOSXP) /* except for primitives, just test for the package */
	return(TRUE);
    if(!allowPrimitiveMethods) /* all primitives turned off by a call to R_set_prim */
	return FALSE;
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

/* called from DispatchOrEval, DispatchGroup, do_matprod
   When called from the first the arguments have been enclosed in
   promises, but not from the other two: there all the arguments have
   already been evaluated.
 */
SEXP attribute_hidden
R_possible_dispatch(SEXP call, SEXP op, SEXP args, SEXP rho,
		    Rboolean promisedArgs)
{
    SEXP fundef, value, mlist=R_NilValue, s, a, b;
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
	if(isFunction(value)) {
	    /* found a method, call it with promised args */
	    if(!promisedArgs) {
		PROTECT(s = promiseArgs(CDR(call), rho));
		if (length(s) != length(args)) error(_("dispatch error"));
		for (a = args, b = s; a != R_NilValue; a = CDR(a), b = CDR(b))
		    SET_PRVALUE(CAR(b), CAR(a));
		value =  applyClosure(call, value, s, rho, R_NilValue);
		UNPROTECT(1);
		return value;
	    } else
		return applyClosure(call, value, args, rho, R_NilValue);
	}
	/* else, need to perform full method search */
    }
    fundef = prim_generics[offset];
    if(!fundef || TYPEOF(fundef) != CLOSXP)
	error(_("primitive function \"%s\" has been set for methods but no generic function supplied"),
	      PRIMNAME(op));
    /* To do:  arrange for the setting to be restored in case of an
       error in method search */
    if(!promisedArgs) {
	PROTECT(s = promiseArgs(CDR(call), rho));
	if (length(s) != length(args)) error(_("dispatch error"));
	for (a = args, b = s; a != R_NilValue; a = CDR(a), b = CDR(b))
	    SET_PRVALUE(CAR(b), CAR(a));
	value = applyClosure(call, fundef, s, rho, R_NilValue);
	UNPROTECT(1);
    } else
	value = applyClosure(call, fundef, args, rho, R_NilValue);
    prim_methods[offset] = current;
    if(value == deferred_default_object)
	return NULL;
    else
	return value;
}

SEXP R_do_MAKE_CLASS(const char *what)
{
    static SEXP s_getClass = NULL;
    SEXP e, call;
    if(!what)
	error(_("C level MAKE_CLASS macro called with NULL string pointer"));
    if(!s_getClass) s_getClass = install("getClass");
    PROTECT(call = allocVector(LANGSXP, 2));
    SETCAR(call, s_getClass);
    SETCAR(CDR(call), mkString(what));
    e = eval(call, R_MethodsNamespace);
    UNPROTECT(1);
    return(e);
}

// similar, but gives NULL instead of an error for a non-existing class
// and 'what' is never checked
SEXP R_getClassDef_R(SEXP what)
{
    static SEXP s_getClassDef = NULL;
    if(!s_getClassDef) s_getClassDef = install("getClassDef");
    if(!isMethodsDispatchOn()) error(_("'methods' package not yet loaded"));
    SEXP call = PROTECT(lang2(s_getClassDef, what));
    SEXP e = eval(call, R_MethodsNamespace);
    UNPROTECT(1);
    return(e);
}

SEXP R_getClassDef(const char *what)
{
    if(!what)
	error(_("R_getClassDef(.) called with NULL string pointer"));
    SEXP s = PROTECT(mkString(what));
    SEXP ans = R_getClassDef_R(s);
    UNPROTECT(1); /* s */
    return ans;
}

Rboolean R_isVirtualClass(SEXP class_def, SEXP env)
{
    if(!isMethodsDispatchOn()) return(FALSE);
    static SEXP isVCl_sym = NULL;
    if(!isVCl_sym) isVCl_sym = install("isVirtualClass");
    SEXP call = PROTECT(lang2(isVCl_sym, class_def));
    SEXP e = eval(call, env);
    UNPROTECT(1);
    // return(LOGICAL(e)[0]);
    // more cautious:
    return (asLogical(e) == TRUE);
}

Rboolean R_extends(SEXP class1, SEXP class2, SEXP env)
{
    if(!isMethodsDispatchOn()) return(FALSE);
    static SEXP extends_sym = NULL;
    if(!extends_sym) extends_sym = install("extends");
    SEXP call = PROTECT(lang3(extends_sym, class1, class2));
    SEXP e = eval(call, env);
    UNPROTECT(1);
    // return(LOGICAL(e)[0]);
    // more cautious:
    return (asLogical(e) == TRUE);
}

/* in Rinternals.h */
SEXP R_do_new_object(SEXP class_def)
{
    static SEXP s_virtual = NULL, s_prototype, s_className;
    SEXP e, value;
    const void *vmax = vmaxget();
    if(!s_virtual) {
	s_virtual = install("virtual");
	s_prototype = install("prototype");
	s_className = install("className");
    }
    if(!class_def)
	error(_("C level NEW macro called with null class definition pointer"));
    e = R_do_slot(class_def, s_virtual);
    if(asLogical(e) != 0)  { /* includes NA, TRUE, or anything other than FALSE */
	e = R_do_slot(class_def, s_className);
	error(_("trying to generate an object from a virtual class (\"%s\")"),
	      translateChar(asChar(e)));
    }
    PROTECT(e = R_do_slot(class_def, s_className));
    PROTECT(value = duplicate(R_do_slot(class_def, s_prototype)));
    if(TYPEOF(value) == S4SXP || getAttrib(e, R_PackageSymbol) != R_NilValue)
    { /* Anything but an object from a base "class" (numeric, matrix,..) */
	setAttrib(value, R_ClassSymbol, e);
	SET_S4_OBJECT(value);
    }
    UNPROTECT(2); /* value, e */
    vmaxset(vmax);
    return value;
}

Rboolean attribute_hidden R_seemsOldStyleS4Object(SEXP object)
{
    SEXP klass;
    if(!isObject(object) || IS_S4_OBJECT(object)) return FALSE;
    /* We want to know about S4SXPs with no S4 bit */
    /* if(TYPEOF(object) == S4SXP) return FALSE; */
    klass = getAttrib(object, R_ClassSymbol);
    return (klass != R_NilValue && LENGTH(klass) == 1 &&
	    getAttrib(klass, R_PackageSymbol) != R_NilValue) ? TRUE: FALSE;
}

SEXP attribute_hidden do_setS4Object(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP object = CAR(args);
    int flag = asLogical(CADR(args)), complete = asInteger(CADDR(args));
    if(length(CADR(args)) != 1 || flag == NA_INTEGER)
	error("invalid '%s' argument", "flag");
    if(complete == NA_INTEGER)
	error("invalid '%s' argument", "complete");
    if(flag == IS_S4_OBJECT(object))
	return object;
    else
      return asS4(object, flag, complete);
}

#ifdef UNUSED
SEXP R_get_primname(SEXP object)
{
    SEXP f;
    if(TYPEOF(object) != BUILTINSXP && TYPEOF(object) != SPECIALSXP)
	error("'R_get_primname' called on a non-primitive");
    PROTECT(f = allocVector(STRSXP, 1));
    SET_STRING_ELT(f, 0, mkChar(PRIMNAME(object)));
    UNPROTECT(1);
    return f;
}
#endif


Rboolean isS4(SEXP s)
{
    return IS_S4_OBJECT(s);
}

SEXP asS4(SEXP s, Rboolean flag, int complete)
{
    if(flag == IS_S4_OBJECT(s))
	return s;
    PROTECT(s);
    if(MAYBE_SHARED(s)) {
	s = shallow_duplicate(s);
	UNPROTECT(1);
	PROTECT(s);
    }
    if(flag) SET_S4_OBJECT(s);
    else {
	if(complete) {
	    SEXP value;
	    /* TENTATIVE:  how much does this change? */
	    if((value = R_getS4DataSlot(s, ANYSXP))
	       != R_NilValue && !IS_S4_OBJECT(value)) {
	      UNPROTECT(1);
	      return value;
	    }
	    /* else no plausible S3 object*/
	    else if(complete == 1) /* ordinary case (2, for conditional) */
	      error(_("object of class \"%s\" does not correspond to a valid S3 object"),
		      CHAR(STRING_ELT(R_data_class(s, FALSE), 0)));
	    else {
		UNPROTECT(1);
		return s; /*  unchanged */
	    }
	}
	UNSET_S4_OBJECT(s);
    }
    UNPROTECT(1);
    return s;
}
