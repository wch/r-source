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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*  This module contains support for S-style generic */
/*  functions and "class" support.  Gag, barf ...  */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

static SEXP GetObject(RCNTXT *cptr)
{
    SEXP s, sysp, b, formals, funcall, tag;
#ifdef OLD
    s = CAR(cptr->promargs);
#else
    sysp = R_GlobalContext->sysparent;

    PROTECT(funcall = R_syscall(0, cptr));

    if ( TYPEOF(CAR(funcall)) == SYMSXP )
	PROTECT(b = findFun(CAR(funcall), sysp));
    else
	PROTECT(b = eval(CAR(funcall), sysp));
    formals = FORMALS(b);

    tag = TAG(formals);
    if (tag != R_NilValue && tag != R_DotsSymbol) {
	s = R_NilValue;
	/** exact matches **/
	for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b)) 
	    if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 1)) { 
		if ( s != R_NilValue) 
		    error("formal argument \"%s\" matched by multiple actual arguments", tag);
		else 
		    s = CAR(b);
	    }

	if ( s == R_NilValue )
	    /** partial matches **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b)) 
		if (TAG(b) != R_NilValue && pmatch(tag, TAG(b), 0)) { 
		    if ( s != R_NilValue)
			error("formal argument \"%s\" matched by multiple actual arguments", tag);
		    else 
			s = CAR(b);
		}
	if ( s == R_NilValue )
	    /** first untagged argument **/
	    for (b = cptr->promargs ; b != R_NilValue ; b = CDR(b)) 
		if (TAG(b) == R_NilValue ) 
		{
		    s = CAR(b);
		    break;
		}
	if ( s == R_NilValue )
	    s = CAR(cptr->promargs);
/*
	    error("failed to match argument for dispatch");
*/
    }
    else
	s = CAR(cptr->promargs);

    UNPROTECT(2);
#endif
    if (TYPEOF(s) == PROMSXP) {
	if (PRVALUE(s) == R_UnboundValue)
	    SET_PRVALUE(s, eval(PREXPR(s), PRENV(s)));
	s = PRVALUE(s);
    }
    return(s);
}

static SEXP applyMethod(SEXP call, SEXP op, SEXP args, SEXP rho, SEXP newrho)
{
    SEXP ans;
    if (TYPEOF(op) == SPECIALSXP) {
	int save = R_PPStackTop;
	R_Visible = 1 - PRIMPRINT(op);
	ans = PRIMFUN(op) (call, op, args, rho);
	if (save != R_PPStackTop) {
	    Rprintf("stack imbalance in %s, %d then %d\n",
		    PRIMNAME(op), save, R_PPStackTop);
	}
    }
    else if (TYPEOF(op) == BUILTINSXP) {
	int save = R_PPStackTop;
	PROTECT(args = evalList(args, rho));
	R_Visible = 1 - PRIMPRINT(op);
	ans = PRIMFUN(op) (call, op, args, rho);
	UNPROTECT(1);
	if (save != R_PPStackTop) {
	    Rprintf("stack imbalance in %s, %d then %d\n",
		    PRIMNAME(op), save, R_PPStackTop);
	}
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

static SEXP newintoold(SEXP new, SEXP old)
{
    if (new == R_NilValue)
	return R_NilValue;
    SETCDR(new, newintoold(CDR(new),old));
    while (old != R_NilValue) {
	if (TAG(old) != R_NilValue && TAG(old) == TAG(new)) {
	    SETCAR(old, CAR(new));
	    return CDR(new);
	}
	old = CDR(old);
    }
    return new;
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

int usemethod(char *generic, SEXP obj, SEXP call, SEXP args,
	      SEXP rho, SEXP *ans)
{
    SEXP class, method, sxp, t, s, matchedarg;
    SEXP op, formals, newrho, newcall,tmp;
    char buf[512];
    int i, j, nclass, matched;
    RCNTXT *cptr;

    /* Get the context which UseMethod was called from. */

    cptr = R_GlobalContext;
    if ( !(cptr->callflag & CTXT_FUNCTION) || cptr->cloenv != rho)
	error("UseMethod used in an inappropriate fashion");

    /* Create a new environment without any */
    /* of the formals to the generic in it. */

    PROTECT(newrho = allocSExp(ENVSXP));
    /*
    PROTECT(op = findFun(CAR(cptr->call), cptr->sysparent));
    */
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
	error("Invalid generic function in usemethod");
    }

    if (TYPEOF(op) == CLOSXP) {
	formals = FORMALS(op);
	for (s = FRAME(cptr->cloenv); s != R_NilValue; s = CDR(s)) {
	    matched = 0;
	    for (t = formals; t != R_NilValue; t = CDR(t))
		if (TAG(t) == TAG(s))
		    matched = 1;
	    if (!matched)
		defineVar(TAG(s),CAR(s),newrho);
	}
    }

    PROTECT(matchedarg = cptr->promargs);
    PROTECT(newcall = duplicate(cptr->call));

    if (isObject(obj)) {
	class = getAttrib(obj, R_ClassSymbol);
	nclass = length(class);
	for (i = 0; i < nclass; i++) {
	    sprintf(buf, "%s.%s", generic, CHAR(STRING_ELT(class, i)));
	    method = install(buf);
	    sxp = findVar(method, rho);
	    /* autoloading requires that promises be evaluated <TSL>*/
	    if (TYPEOF(sxp)==PROMSXP){ 
		PROTECT(tmp=eval(sxp, rho)); 
		sxp=tmp;
		UNPROTECT(1);
	    }
	    if (isFunction(sxp)) {
		defineVar(install(".Generic"), mkString(generic), newrho);
		if (i > 0) {
		    PROTECT(t = allocVector(STRSXP, nclass - i));
		    for (j = 0; j < length(t); j++, i++)
			SET_STRING_ELT(t, j, STRING_ELT(class, i));
		    setAttrib(t, install("previous"), class);
		    defineVar(install(".Class"), t, newrho);
		    UNPROTECT(1);
		}
		else
		    defineVar(install(".Class"), class, newrho);
		PROTECT(t = mkString(buf));
		defineVar(install(".Method"), t, newrho);
		UNPROTECT(1);
		t = newcall;
		SETCAR(t, method);
		R_GlobalContext->callflag = CTXT_GENERIC;
		*ans = applyMethod(t, sxp, matchedarg, rho, newrho);
		R_GlobalContext->callflag = CTXT_RETURN;
		UNPROTECT(4);
		return 1;
	    }
	}
    }
    sprintf(buf, "%s.default", generic);
    method = install(buf);
    sxp = findVar(method, rho);
    if (isFunction(sxp)) {
	defineVar(install(".Generic"), mkString(generic), newrho);
	defineVar(install(".Class"), R_NilValue, newrho);
	PROTECT(t = mkString(buf));
	defineVar(install(".Method"), t, newrho);
	UNPROTECT(1);
	t = newcall;
	SETCAR(t, method);
	R_GlobalContext->callflag = CTXT_GENERIC;
	*ans = applyMethod(t, sxp, matchedarg, rho, newrho);
	R_GlobalContext->callflag = CTXT_RETURN;
	UNPROTECT(4);
	return 1;
    }
    UNPROTECT(4);
    cptr->callflag = CTXT_RETURN;
    return 0;
}

/* Note: "do_usemethod" is not the only entry point to */
/* "usemethod". Things like [ and [[ call usemethod directly, */
/* hence do_usemethod should just be an interface to usemethod. */

SEXP do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[128];
    SEXP ans, meth, obj;
    int nargs;
    RCNTXT *cptr;

    nargs = length(args);

if (nargs < 0)
    errorcall(call, "corrupt internals!");

    if (nargs)
	PROTECT(meth = eval(CAR(args), env));
    else
	meth = R_MissingArg;

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
	    error("UseMethod called from outside a closure");
	if (meth == R_MissingArg)
	    PROTECT(meth = mkString(CHAR(PRINTNAME(CAR(cptr->call)))));
	PROTECT(obj = GetObject(cptr));
    }

    if (TYPEOF(meth) != STRSXP ||
	LENGTH(meth) < 1 ||
	strlen(CHAR(STRING_ELT(meth, 0))) == 0)
	errorcall(call, "first argument must be a method name");

    strcpy(buf, CHAR(STRING_ELT(meth, 0)));

    if (usemethod(buf, obj, call, CDR(args), env, &ans) == 1) {
	UNPROTECT(1);
	PROTECT(ans);
	findcontext(CTXT_RETURN, env, ans);
	UNPROTECT(1);
    }
    else
	error("no applicable method for \"%s\"", buf);
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

    for( t = args; t != R_NilValue; t=CDR(t) ) {
	if( TAG(t) != R_NilValue ) {
		found = 0;
		for(s=call; CDR(s) != R_NilValue; s=CDR(s))
			if( TAG(CDR(s)) == TAG(t) )
				found = 1;
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

SEXP do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[128], b[512], tbuf[10];
    SEXP ans, s, t, class, method, matchedarg, generic, nextfun;
    SEXP sysp, m, formals, actuals, tmp, newcall;
    SEXP a, group, basename;
    RCNTXT *cptr;
    int i,j;

    cptr = R_GlobalContext;

    cptr->callflag = CTXT_GENERIC;

    /* get the env NextMethod was called from */
    sysp = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag & CTXT_FUNCTION && cptr->cloenv == sysp)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error("NextMethod called from outside a closure");

    PROTECT(newcall = duplicate(cptr->call));

    /* set up the arglist */
    s = findFun(CAR(cptr->call), cptr->sysparent);
    if (TYPEOF(s) != CLOSXP)
	errorcall(cptr->call, "function is not a closure");

    /* get formals and actuals; attach the names of the formals to
       the actuals, expanding any ... that occurs */
    formals = FORMALS(s);
    PROTECT(actuals = matchArgs(formals, cptr->promargs));

    i=0;
    for(s=formals, t=actuals; s!=R_NilValue; s=CDR(s), t=CDR(t) ) {
	SET_TAG(t, TAG(s));
	if(TAG(t)==R_DotsSymbol) i=length(CAR(t));
    }
    if(i) {   /* we need to expand out the dots */
	PROTECT(t = allocList(i+length(actuals)-1));
	for( s=actuals, m=t; s!=R_NilValue; s=CDR(s) ) {
	    if(TYPEOF(CAR(s)) == DOTSXP) {
		i=1;
		for(a=CAR(s); a!=R_NilValue; a=CDR(a), i++, m=CDR(m) ) {
		    sprintf(tbuf,"..%d",i);
		    SET_TAG(m, mkSYMSXP(mkChar(tbuf), R_UnboundValue));
		    SETCAR(m, CAR(a));
		}
	    }
	    else {
		SET_TAG(m, TAG(s));
		SETCAR(m, CAR(s));
		m=CDR(m);
	    }
        }
	UNPROTECT(1);
	actuals=t;
    }
    PROTECT(actuals);


    /* we can't duplicate because it would force the promises */
    /* so we do our own duplication of the promargs */

    PROTECT(matchedarg = allocList(length(cptr->promargs)));
    for (t = matchedarg, s = cptr->promargs; t != R_NilValue;
	  s = CDR(s), t=CDR(t)) {
		SETCAR(t, CAR(s));
		SET_TAG(t, TAG(s));
    }
    for (t = matchedarg; t != R_NilValue; t = CDR(t)) {
	for (m = actuals; m != R_NilValue; m = CDR(m))
	    if (CAR(m) == CAR(t))  {
		if (CAR(m) == R_MissingArg) {
		  
		  /*#ifdef USE_HASHTABLE */
		    tmp = findVarInFrame(cptr->cloenv, TAG(m));

		    /* Old */
		    /* tmp = findVarInFrame(FRAME(cptr->cloenv), TAG(m)); */
		    
		    /*#endif  USE_HASHTABLE */

		    if (tmp == R_MissingArg)
			break;
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
	t = findVarInFrame(env, s);
	if (t != R_NilValue && t != R_MissingArg) {
	    SET_TYPEOF(t, LISTSXP); /* a safe mutation */
	    s = matchmethargs(matchedarg,t);
	    UNPROTECT(1);
	    PROTECT(matchedarg = s);
	    newcall = fixcall(newcall, matchedarg);
	}
    }
    else
	errorcall(call,"wrong argument ...");

    /*
      .Class is used to determine the next method; if it doesn't
      exist the first argument to the current method is used
      the second argument to NextMethod is another option but
      isn't currently used).
    */
    class = findVarInFrame( R_GlobalContext->sysparent, install(".Class"));

    if (class == R_UnboundValue) {
	s = GetObject(cptr);
	if (!isObject(s))
	    errorcall(call, "object not specified");
	class = getAttrib(s, R_ClassSymbol);
    }

    /* the generic comes from either the sysparent or it's named */
    generic = findVarInFrame(R_GlobalContext->sysparent, install(".Generic"));
    if (generic == R_UnboundValue)
	generic = eval(CAR(args), env);
    if( generic == R_NilValue )
	errorcall(call,"generic function not specified");
    PROTECT(generic);

    if (!isString(generic) || length(generic) > 1)
	errorcall(call,"invalid generic argument to NextMethod");
    
    if (strlen(CHAR(STRING_ELT(generic, 0))) == 0)
	errorcall(call,"generic function not specified");

    /* determine whether we are in a Group dispatch */

    group = findVarInFrame(R_GlobalContext->sysparent,install(".Group") );
    if (group == R_UnboundValue){
	PROTECT(group = mkString(""));
    }
    else
	PROTECT(group);

    if (!isString(group) || length(group) > 1)
	errorcall(call, "invalid group argument found in NextMethod");

    /* determine the root: either the group or the generic will be it */

    if( strlen(CHAR(STRING_ELT(group, 0))) == 0 )
	basename = generic;
    else
	basename = group;

    nextfun = R_NilValue;

    /* find the method currently being invoked and jump over the current call */
    /* if t is R_UnboundValue then we called the current method directly */

    method = findVarInFrame(R_GlobalContext->sysparent,install(".Method") );
    if( method != R_UnboundValue) {
	if( !isString(method) ) 
	    error("Wrong value for .Method");
	for( i=0; i<length(method); i++ ) {
	  sprintf(b,"%s", CHAR(STRING_ELT(method, i)));
	  if( strlen(b) )
	    break;
	}
    }
    else {
      sprintf(b,"%s", CHAR(PRINTNAME(CAR(cptr->call))));	
    }

    /* we need the value of i on exit from the for loop to figure out
       how many classes to drop
    */

    for (j = 0; j < length(class); j++) {
      sprintf(buf,"%s.%s", CHAR(STRING_ELT(basename, 0)),
	CHAR(STRING_ELT(class, j)));
      if ( !strcmp(buf,b) ) 
        break;
    }

    if ( !strcmp(buf,b) ) /* we found a match and start from there */
      j++;
    else
      j = 0;  /*no match so start with the first element of .Class */

    for (i = j ; i < length(class); i++) {
	sprintf(buf, "%s.%s", CHAR(STRING_ELT(generic, 0)),
		CHAR(STRING_ELT(class, i)));
	nextfun = findVar(install(buf),env);
	if (isFunction(nextfun))
	    break;
    }
    if (!isFunction(nextfun)) {
	sprintf(buf, "%s.default", CHAR(STRING_ELT(generic, 0)));
	nextfun = findVar(install(buf), env);
	if (!isFunction(nextfun)) {
	    t = install(CHAR(STRING_ELT(generic, 0)));
	    nextfun = findVar(t,env);
	    if (!isFunction(nextfun))
		error("No method to invoke");
	    if (TYPEOF(nextfun) == CLOSXP) {
		if (INTERNAL(t) != R_NilValue)
		    nextfun = INTERNAL(t);
		else
		    error("No method to invoke");
	    }
	}
    }
    PROTECT(s = allocVector(STRSXP, length(class) - i));
    PROTECT(class = duplicate(class));
    PROTECT(m = allocSExp(ENVSXP));
    for (j = 0; j < length(s); j++)
	SET_STRING_ELT(s, j, duplicate(STRING_ELT(class, i++)));
    setAttrib(s, install("previous"), class);
    defineVar(install(".Class"), s, m);
    PROTECT(method = mkString(buf));
    defineVar(install(".Method"), method, m);
    method = install(buf);

    defineVar(install(".Generic"), generic, m);

    defineVar(install(".Group"), group, m);

    SETCAR(newcall, method);
    ans = applyMethod(newcall, nextfun, matchedarg, env, m);
    UNPROTECT(10);
    return(ans);
}

SEXP do_unclass(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (isObject(CAR(args))) {
	SETCAR(args, duplicate(CAR(args)));
	setAttrib(CAR(args), R_ClassSymbol, R_NilValue);
    }
    return CAR(args);
}

/* InheritsClass  -  does an object inherit from a class */

int InheritsClass(SEXP x, char *name)
{
    SEXP class;
    int i, nclass;
    if (isObject(x)) {
	class = getAttrib(x, R_ClassSymbol);
	nclass = length(class);
	for (i = 0; i < nclass; i++)
	    if (!strcmp(CHAR(STRING_ELT(class, i)), name))
		return 1;
    }
    return 0;
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
	    if (!strcmp(CHAR(STRING_ELT(class, i)), name))
		nmatch++;
	if (nmatch == nclass) {
	    setAttrib(x, R_ClassSymbol, R_NilValue);
	}
	else if (nmatch > 0) {
	    PROTECT(newclass = allocVector(STRSXP, nclass-nmatch));
	    for (i = 0, j = 0; i < nclass; i++)
		if (strcmp(CHAR(STRING_ELT(class, i)), name)) {
		    SET_STRING_ELT(newclass, j++, STRING_ELT(class, i));
		}
	    setAttrib(x, R_ClassSymbol, newclass);
	    UNPROTECT(1);
	}
	UNPROTECT(1);
    }
}

SEXP do_inherits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, class, what, which, rval = R_NilValue /* -Wall */;
    int i, j, nwhat, isvec, nclass;

    checkArity(op, args);

    x = CAR(args);
    /* if x isn't an object get out asap */
    if( !isObject(x) )
	return mkFalse();
    
    class = getAttrib(x, R_ClassSymbol);
    nclass = length(class);

    what = CADR(args);
    if( !isString(what) )
	errorcall(call, "what must be a character vector");
    nwhat = length(what);

    which = CADDR(args);
    if( !isLogical(which) || (length(which) != 1) )
	errorcall(call, "which must be a length 1 logical vector");
    isvec = asLogical(which);

    if( isvec )
	rval = allocVector(INTSXP, nwhat);

    for(j=0; j<nwhat; j++) {
	for(i=0; i<nclass; i++) {
	    if( isvec )
		INTEGER(rval)[j] = 0;
	    if(!strcmp(CHAR(STRING_ELT(class,i)), CHAR(STRING_ELT(what,j)))) {
		if(isvec) 
		   INTEGER(rval)[j] = i+1; 
		else 
		    return mkTrue();
		break;
	    }
	}
    }
    if( !isvec ) 
	return mkFalse();
    return rval;
}
	
