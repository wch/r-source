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

	/*  This module contains support for S-style generic */
	/*  functions and "class" support.  Gag, barf ...  */

#include "Defn.h"

static SEXP GetObject(RCNTXT *cptr)
{
	SEXP s;

	s = CAR(cptr->promargs);
	if( TYPEOF(s) == PROMSXP ) {
		if(PRVALUE(s) == R_UnboundValue ) {
			s = eval(PREXPR(s), PRENV(s));
			PRVALUE(CAR(cptr->promargs)) = s;
		}
		else
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
		if(save != R_PPStackTop) {
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
		if(save != R_PPStackTop) {
			Rprintf("stack imbalance in %s, %d then %d\n",
				PRIMNAME(op), save, R_PPStackTop);
		}
	}
	else if (TYPEOF(op) == CLOSXP) {
		ans = applyClosure(call, op, args, rho, newrho); 
	}
	return ans;
}


	/*  newintoold  -  a destructive matching of arguments;
	 *  newargs comes first; any element of oldargs with
	 *  a name that matches a named newarg is deleted; the
	 *  two resulting lists are appended and returned S
	 *  says they do this (white book) but doesn't seem to. */
	

static SEXP newintoold(SEXP new, SEXP old) 
{
	if( new==R_NilValue) 
		return R_NilValue;
	CDR(new) = newintoold(CDR(new),old);
	while( old != R_NilValue ) {
		if( TAG(old) != R_NilValue && TAG(old) == TAG(new) ) {
			CAR(old) = CAR(new);
			return CDR(new);
		}
		old = CDR(old);
	}
	return new;
}

static SEXP matchmethargs(SEXP oldargs, SEXP newargs)
{
	newargs = newintoold(newargs,oldargs);
	return listAppend(oldargs,newargs);
}

/*  usemethod  -  calling functions need to evaluate the object
 *  (== 2nd argument).  They also need to ensure that the
 *  argument list is set up in the correct manner.
 *	 
 *    1. find the context for the calling function (i.e. the generic)
 *	 this gives us the unevaluated arguments for the original call
 *
 *    2. create an environment for evaluating the method and insert
 *       a handful of variables (.Generic, .Class and .Method) into
 *       that environment. Also copy any variables in the env of the
 *       generic that are not formal (or actual) arguments.
 *
 *    3. fix up the argument list; it should be the arguments to the
 *       generic matched to the formals of the method to be invoked */

int usemethod(char *generic, SEXP obj, SEXP call, SEXP args, SEXP rho, SEXP *ans)
{
	SEXP class, method, sxp, t, s, cloenv, matchedarg;
	SEXP op, formals, newrho, newcall;
        char buf[512];
        int i, j, nclass, matched;
	RCNTXT *cptr;

		/* Get the context which UseMethod was called from. */

	cptr = R_GlobalContext;
	if(cptr->callflag != CTXT_RETURN || cptr->cloenv != rho)
		error("UseMethod used in an inappropriate fashion\n");

		/* Create a new environment without any */
		/* of the formals to the generic in it. */

	PROTECT(newrho=allocSExp(ENVSXP));
	PROTECT(op=findFun(CAR(cptr->call), cptr->sysparent));
	if(TYPEOF(op)==CLOSXP) {
		formals=FORMALS(op);
		for(s=FRAME(cptr->cloenv);s!=R_NilValue;s=CDR(s)) {
			matched=0;
			for(t=formals;t!=R_NilValue;t=CDR(t)) 
				if(TAG(t) == TAG(s))
					matched=1;
			if(!matched)
				defineVar(TAG(s),CAR(s),newrho);
		}
	}

	PROTECT(matchedarg= cptr->promargs);
	PROTECT(newcall = duplicate(cptr->call));

	if(isObject(obj)) {
		class = getAttrib(obj, R_ClassSymbol);
		nclass = length(class);
		for(i=0 ; i<nclass ; i++) {
			sprintf(buf, "%s.%s", generic, CHAR(STRING(class)[i]));
			method = install(buf);
			sxp = findVar(method, rho);
			if(isFunction(sxp)) {
				defineVar(install(".Generic"), mkString(generic), newrho);
				if(i>0) {
					PROTECT(t=allocVector(STRSXP,nclass-i));
					for(j=0 ; j< length(t) ; j++, i++)
						STRING(t)[j] = STRING(class)[i];
					setAttrib(t,install("previous"),class);
					defineVar(install(".Class"), t, newrho);
					UNPROTECT(1);
				}
				else
					defineVar(install(".Class"),class, newrho);
				PROTECT(t=mkString(buf));
				defineVar(install(".Method"), t, newrho);
				UNPROTECT(1);
				t=newcall;
				CAR(t) = method;
				R_GlobalContext->callflag=CTXT_GENERIC;
				*ans = applyMethod(t,sxp,matchedarg,rho,newrho);
				R_GlobalContext->callflag=CTXT_RETURN;
				UNPROTECT(4);
				return 1;
			}
		}
	}
	sprintf(buf, "%s.default", generic);
	method = install(buf);
	sxp = findVar(method, rho);
	if(isFunction(sxp)) {
		defineVar(install(".Generic"), mkString(generic), newrho);
		defineVar(install(".Class"), R_NilValue, newrho);
		PROTECT(t=mkString(buf));
		defineVar(install(".Method"), t, newrho);
		UNPROTECT(1);
		t=newcall;
		CAR(t) = method;
		R_GlobalContext->callflag=CTXT_GENERIC;
		*ans = applyMethod(t, sxp, matchedarg, rho, newrho);
		R_GlobalContext->callflag=CTXT_RETURN;
		UNPROTECT(4);
		return 1;
	}
	UNPROTECT(4);
	cptr->callflag = CTXT_RETURN;
	return 0;
}

	/*  do_usemethod is not the only entry point to usemethod,
	 *  things like [ and [[ call usemethod directly, hence
	 *  do_usemethod should just be an interface to usemethod. */

SEXP do_usemethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[128];
	SEXP ans, meth, obj;
	int nargs;
	RCNTXT *cptr;

	nargs = length(args);

	if( nargs )
		PROTECT(meth=eval(CAR(args), env));
	else
		meth=R_MissingArg;

	if( nargs >= 2 ) 
		PROTECT(obj = eval(CADR(args), env));
	else {
		cptr = R_GlobalContext;
		while(cptr != NULL) {
			if(cptr->callflag == CTXT_RETURN && cptr->cloenv == env)
				break;
			cptr = cptr->nextcontext;
		}
		if(cptr == NULL)
			error("UseMethod called from outside a closure\n");
		if( meth == R_MissingArg )
			PROTECT(meth = mkString(CHAR(PRINTNAME(CAR(cptr->call)))));
		PROTECT(obj = GetObject(cptr));
	}
			
	if(TYPEOF(meth) != STRSXP || LENGTH(meth) < 1 || strlen(CHAR(STRING(meth)[0])) == 0)
		errorcall(call, "first argument must be a method name\n");

	strcpy(buf, CHAR(STRING(meth)[0]));

	if(usemethod(buf, obj, call, CDR(args), env, &ans) == 1) {
		UNPROTECT(1);
		findcontext(CTXT_RETURN, ans);
	}
	else
		error("no applicable method for \"%s\"\n", buf);
}

/* 
   if NextMethod has any arguments the first must be the generic
   the second the object and any remaining are matched with the
   formals of the chosen method
  
*/
#define ARGUSED(x) LEVELS(x)

SEXP do_nextmethod(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[128],*pt, *pb;
	SEXP ans, s, t, class, method, matchedarg, generic, nextfun;
	SEXP sysp, m, formals, actuals, tmp, newcall;
	RCNTXT *cptr;
	int nargs,i,j;
	SEXP group,realgroup;

	cptr=R_GlobalContext;
	
	cptr->callflag = CTXT_GENERIC;

	/* get the env NextMethod was called from */
	sysp = R_GlobalContext->sysparent;
	while(cptr != NULL) {
		if(cptr->callflag == CTXT_RETURN && cptr->cloenv == sysp)
			break;
		cptr = cptr->nextcontext;
	}
	if(cptr == NULL)
		error("NextMethod called from outside a closure\n");

	PROTECT(newcall=duplicate(cptr->call));

	/* set up the arglist */
	s=findFun(CAR(cptr->call), cptr->sysparent);
	if( TYPEOF(s) != CLOSXP )
		errorcall(cptr->call,"function is not a closure\n");
	formals = FORMALS(s);
	actuals = matchArgs(formals, cptr->promargs);

	/* we can't duplicate because it would force the promises */
	/* so we do our own duplication of the promargs           */
	PROTECT(matchedarg = allocList(length(cptr->promargs)));
	for( t=matchedarg, s=cptr->promargs; t != R_NilValue; t=CDR(t), s=CDR(s)){
		CAR(t) = CAR(s);
		TAG(t) = TAG(s);
	}
	for(s=formals; s!=R_NilValue ; s=CDR(s) )
		ARGUSED(s) = 0;
	for( t=matchedarg ; t != R_NilValue ; t = CDR(t) ) {
		for (m = actuals, s= formals ; m != R_NilValue ; m = CDR(m),s=CDR(s) )
			if( (CAR(m) == CAR(t)) && !ARGUSED(s) ) {
				ARGUSED(s) = 1;
				if( CAR(m) == R_MissingArg ) {
					tmp = findVarInFrame(FRAME(cptr->cloenv), TAG(s));
					if (tmp == R_MissingArg) 
						break;
				}
				CAR(t) = mkPROMISE(TAG(s), cptr->cloenv);
				break;
			}
	}
	/*
		Now see if there were any other arguments passed in

		.Class is used to determine the next method; if it doesn't
		exist the first argument to the current method is used
		the second argument to NextMethod is another option but
		isn't currently used).
	*/
	s = CADDR(args); /* this is ... and we need to see if it's bound */
	if( s == R_DotsSymbol ) {
		t = findVarInFrame(FRAME(env),s);
		if( t != R_NilValue && t != R_MissingArg ) {
			TYPEOF(t)= LISTSXP; /* a safe mutation */
			s =matchmethargs(matchedarg,t); 
			UNPROTECT(1);
			PROTECT(matchedarg=s);
		}
	}
	else
		errorcall(call,"wrong argument ...\n"); 

	class = dynamicfindVar(install(".Class"),R_GlobalContext);

	if (class == R_UnboundValue) {
		s = GetObject(cptr);
		if( ! isObject(s) )
			errorcall(call, "object not specified\n");
		class = getAttrib(s, R_ClassSymbol);
	}

	generic = eval(CAR(args), env);
	if( generic == R_NilValue ) {
		generic = dynamicfindVar(install(".Generic"), R_GlobalContext);
		if( generic == R_UnboundValue) 
			generic = mkString(CHAR(PRINTNAME(CAR(cptr->call))));
	}

	PROTECT(generic);

	if( !isString(generic) || length(generic) > 1 )
		errorcall(call,"invalid generic argument to NextMethod\n");
	if( strlen(CHAR(STRING(generic)[0])) == 0 ) 
		errorcall(call,"generic function not specified\n");

	group=dynamicfindVar(install(".Group"),R_GlobalContext);
	PROTECT(realgroup=duplicate(group));
	if (group == R_UnboundValue){
	  group=generic;
	  realgroup=mkString("");
	}
	if (!isString(group) || length(group) > 1 )
	  errorcall(call,"invalid group argument found in NextMethod\n");
	if (strlen(CHAR(STRING(group)[0])) == 0 )
	  group=generic;

	/* we need the value of i on exit from the for loop to figure out 
	   how many classes to drop
	*/

	nextfun=R_NilValue;
	/* jump over the current call */
	t=CAR(cptr->call);
	for(j = 0; j<length(class); j++) {
		sprintf(buf,"%s.%s",CHAR(STRING(group)[0]),
				CHAR(STRING(class)[j]));
		if(install(buf) == t)
			break;
		sprintf(buf,"%s.%s",CHAR(STRING(generic)[0]),
				CHAR(STRING(class)[j]));
		if(install(buf) == t)
			break;
	}

	for(i = j+1; i<length(class); i++) {
		sprintf(buf,"%s.%s",CHAR(STRING(generic)[0]),
				CHAR(STRING(class)[i]));
		nextfun=findVar(install(buf),env);
		if(isFunction(nextfun))
			break;
		sprintf(buf,"%s.%s",CHAR(STRING(group)[0]),
				CHAR(STRING(class)[i]));
		nextfun=findVar(install(buf),env);
		if(isFunction(nextfun))
			break;
	}
	if(!isFunction(nextfun)) {
		sprintf(buf,"%s.default",CHAR(STRING(generic)[0]));
		nextfun=findVar(install(buf),env);
		if(!isFunction(nextfun)) {
			t = install(CHAR(STRING(generic)[0]));
			nextfun=findVar(t,env);
			if( !isFunction(nextfun) )
				error("No method to invoke\n");
			if (TYPEOF(nextfun) == CLOSXP )
				if(INTERNAL(t) != R_NilValue ) 
					nextfun = INTERNAL(t);
				else
					error("No method to invoke\n");
		}
	}
	PROTECT(s = allocVector(STRSXP,length(class)-i));
	PROTECT(class = duplicate(class));
	PROTECT(m = allocSExp(ENVSXP));
	for(j = 0 ; j< length(s) ; j++ )
		STRING(s)[j] = duplicate(STRING(class)[i++]);
	setAttrib(s,install("previous"),class);
	defineVar(install(".Class"),s, m); 
	PROTECT(method=mkString(buf));
	defineVar(install(".Method"), method, m);
	method=install(buf);

	defineVar(install(".Generic"), generic, m);
	
	defineVar(install(".Group"),realgroup,m);

	CAR(newcall) = method;
	ans = applyMethod(newcall, nextfun, matchedarg, env, m);
	UNPROTECT(7);
	UNPROTECT(1);
	return(ans);
}

SEXP do_unclass(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);
	if(isObject(CAR(args))) {
		CAR(args) = duplicate(CAR(args));
		setAttrib(CAR(args), R_ClassSymbol, R_NilValue);
	}
	return CAR(args);
}

	/*  InheritsClass  -  does an object inherit from a class */

int InheritsClass(SEXP x, char *name)
{
	SEXP class;
	int i, nclass;

	if(isObject(x)) {
		class = getAttrib(x, R_ClassSymbol);
		nclass = length(class);
		for(i=0 ; i<nclass ; i++)
			if(!strcmp(CHAR(STRING(class)[i]), name))
				return 1;
	}
	return 0;
}

void RemoveClass(SEXP x, char *name)
{
	SEXP class, newclass;
	int i, j, nclass, nmatch;

	if(isObject(x)) {
		PROTECT(x);
		class = getAttrib(x, R_ClassSymbol);  
		nclass = length(class);
		nmatch = 0;
		for(i=0 ; i<nclass ; i++)
			if(!strcmp(CHAR(STRING(class)[i]), name))
				nmatch++;
		if(nmatch == nclass) {
			setAttrib(x, R_ClassSymbol, R_NilValue);
		}
		else if(nmatch > 0) {
			PROTECT(newclass = allocVector(STRSXP, nclass-nmatch));
			for(i=0, j=0 ; i<nclass ; i++)
				if(strcmp(CHAR(STRING(class)[i]), name)) {
					STRING(newclass)[j++] = STRING(class)[i];
				}
			setAttrib(x, R_ClassSymbol, newclass);
			UNPROTECT(1);
		}
		UNPROTECT(1);
	}
}
