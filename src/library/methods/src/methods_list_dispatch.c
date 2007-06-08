/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2007   The R Development Core Team.
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

/* <UTF8>
   Does byte-level handling in primitive_case, but only of ASCII chars
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#undef _

#include "RSMethods.h"
#include "methods.h"
#include "Rdefines.h"

#if !defined(snprintf) && defined(HAVE_DECL_SNPRINTF) && !HAVE_DECL_SNPRINTF
extern int snprintf (char *s, size_t n, const char *format, ...);
#endif

/* the following utilities are included here for now, as statics.  But
   they will eventually be C implementations of slot, data.class,
   etc. */

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs);
static SEXP R_loadMethod(SEXP f, SEXP fname, SEXP ev);

/* objects, mostly symbols, that are initialized once to save a little time */
static int initialized = 0;
static SEXP s_dot_Methods, s_skeleton, s_expression, s_function,
    s_getAllMethods, s_objectsEnv, s_MethodsListSelect,
    s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_generic, s_package,
    s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets,
    s_argument, s_allMethods;
static SEXP R_FALSE, R_TRUE;
static Rboolean table_dispatch_on = 1;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


SEXP R_quick_method_check(SEXP object, SEXP fsym, SEXP fdef);

static SEXP R_target, R_defined, R_nextMethod, R_source;
static SEXP R_dot_target, R_dot_defined, R_dot_nextMethod;
static SEXP R_loadMethod_name, R_dot_Method;

static SEXP Methods_Namespace = NULL;

static const char *check_single_string(SEXP, Rboolean, const char *);
static const char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty,
                                          const char *what);
static const char *class_string(SEXP obj);

static void init_loadMethod()
{
    R_target = install("target");
    R_defined = install("defined");
    R_nextMethod = install("nextMethod");
    R_source = install("source");
    R_loadMethod_name = install("loadMethod");
    R_dot_target = install(".target");
    R_dot_defined = install(".defined");
    R_dot_nextMethod = install(".nextMethod");
    R_dot_Method = install(".Method");
}


SEXP R_initMethodDispatch(SEXP envir)
{
    if(envir && !isNull(envir))
	Methods_Namespace = envir;
    if(!Methods_Namespace)
	Methods_Namespace = R_GlobalEnv;
    if(initialized)
	return(envir);

    s_dot_Methods = Rf_install(".Methods");
    s_skeleton = Rf_install("skeleton");
    s_expression = Rf_install("expression");
    s_function = Rf_install("function");
    s_getAllMethods = Rf_install("getAllMethods");
    s_objectsEnv = Rf_install("objectsEnv");
    s_MethodsListSelect = Rf_install("MethodsListSelect");
    s_sys_dot_frame = Rf_install("sys.frame");
    s_sys_dot_call = Rf_install("sys.call");
    s_sys_dot_function = Rf_install("sys.function");
    s_generic = Rf_install("generic");
    s_package = Rf_install("package");
    s_generic_dot_skeleton = Rf_install("generic.skeleton");
    s_subset_gets = Rf_install("[<-");
    s_element_gets = Rf_install("[[<-");
    s_argument = Rf_install("argument");
    s_allMethods = Rf_install("allMethods");

    R_FALSE = ScalarLogical(FALSE);
    R_PreserveObject(R_FALSE);
    R_TRUE = ScalarLogical(TRUE);
    R_PreserveObject(R_TRUE);

    /* some strings (NOT symbols) */
    s_missing = mkString("missing");
    R_PreserveObject(s_missing);
    /*  Initialize method dispatch, using the static */
    R_set_standardGeneric_ptr(
	(table_dispatch_on ? R_dispatchGeneric : R_standardGeneric)
	, Methods_Namespace);
    R_set_quick_method_check(
	(table_dispatch_on ? R_quick_dispatch : R_quick_method_check));

    /* Some special lists of primitive skeleton calls.
       These will be promises under lazy-loading.
    */
    PROTECT(R_short_skeletons =
	    findVar(Rf_install(".ShortPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_short_skeletons) == PROMSXP)
	R_short_skeletons = eval(R_short_skeletons, Methods_Namespace);
    R_PreserveObject(R_short_skeletons);
    UNPROTECT(1);
    PROTECT(R_empty_skeletons =
	    findVar(Rf_install(".EmptyPrimitiveSkeletons"),
		    Methods_Namespace));
    if(TYPEOF(R_empty_skeletons) == PROMSXP)
	R_empty_skeletons = eval(R_empty_skeletons, Methods_Namespace);
    R_PreserveObject(R_empty_skeletons);
    UNPROTECT(1);
    if(R_short_skeletons == R_UnboundValue ||
       R_empty_skeletons == R_UnboundValue)
	error(_("could not find the skeleton calls for 'methods' (package detached?): expect very bad things to happen"));
    f_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 0);
    fgets_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 1);
    f_x_skeleton = VECTOR_ELT(R_empty_skeletons, 0);
    fgets_x_skeleton = VECTOR_ELT(R_empty_skeletons, 1);
    init_loadMethod();
    initialized = 1;
    return(envir);
}


/* simplified version of do_subset2_dflt, with no partial matching */
static SEXP R_element_named(SEXP obj, const char * what)
{
    int offset = -1, i, n;
    SEXP names = getAttrib(obj, R_NamesSymbol);
    n = length(names);
    if(n > 0) {
	for(i = 0; i < n; i++) {
	    if(streql(what, CHAR(STRING_ELT(names, i)))) {
		offset = i; break;
	    }
	}
    }
    if(offset < 0)
	return R_NilValue;
    else
	return VECTOR_ELT(obj, offset);
}

static SEXP R_insert_element(SEXP mlist, const char * what, SEXP object)
{
    SEXP sym = install(what);
    return R_subassign3_dflt(R_NilValue, mlist, sym, object);
}

SEXP R_el_named(SEXP object, SEXP what)
{
    const char * str;
    str = CHAR(asChar(what));
    return R_element_named(object, str);
}

SEXP R_set_el_named(SEXP object, SEXP what, SEXP value)
{
    const char * str;
    str = CHAR(asChar(what));
    return R_insert_element(object, str, value);
}

/*  */
static int n_ov = 0;

SEXP R_clear_method_selection()
{
    n_ov = 0;
    return R_NilValue;
}

static SEXP R_find_method(SEXP mlist, const char *class, SEXP fname)
{
    /* find the element of the methods list that matches this class,
       but not including inheritance. */
    SEXP value, methods;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue) {
	error(_("no \"allMethods\" slot found in object of class \"%s\" used as methods list for function '%s'"),
	      class_string(mlist), CHAR(asChar(fname)));
	return(R_NilValue); /* -Wall */
    }
    value = R_element_named(methods, class);
    return value;
}

SEXP R_quick_method_check(SEXP args, SEXP mlist, SEXP fdef)
{
    /* Match the list of (evaluated) args to the methods list. */
    SEXP object, methods, value, retValue = R_NilValue;
    const char *class; int nprotect = 0;
    if(!mlist)
	return R_NilValue;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue)
	return R_NilValue;
    while(!isNull(args) && !isNull(methods)) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP) {
	    if(PRVALUE(object) == R_UnboundValue) {
		SEXP tmp = eval(PRCODE(object), PRENV(object));
		PROTECT(tmp); nprotect++;
		SET_PRVALUE(object,  tmp);
		object = tmp;
	    }
	    else
		object = PRVALUE(object);
	}
	class = CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	value = R_element_named(methods, class);
	if(isNull(value) || isFunction(value)){
	    retValue = value;
	    break;
	}
	/* continue matching args down the tree */
	methods = R_do_slot(value, s_allMethods);
    }
    UNPROTECT(nprotect);
    return(retValue);
}

SEXP R_quick_dispatch(SEXP args, SEXP mtable, SEXP fdef)
{
    /* Match the list of (evaluated) args to the methods table. */
    SEXP object, value, retValue = R_NilValue;
    const char *class; int nprotect = 0;
#define NBUF 200
    char buf[NBUF]; char *ptr;
    if(!mtable || TYPEOF(mtable) != ENVSXP)
	return R_NilValue;
    buf[0] = '\0'; ptr = buf;
    /*  doesn't check for nargs() > .SigLength.
	Could do so but the search will fail
	in this case anyway, and the test is not clearly faster
	on average */
    while(!isNull(args)) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP) {
	    if(PRVALUE(object) == R_UnboundValue) {
		SEXP tmp = eval(PRCODE(object), PRENV(object));
		PROTECT(tmp); nprotect++;
		SET_PRVALUE(object,  tmp);
		object = tmp;
	    }
	    else
		object = PRVALUE(object);
	}
	class = CHAR(STRING_ELT(R_data_class(object, TRUE), 0));
	if(ptr - buf + strlen(class) + 2 > NBUF) {
	    UNPROTECT(nprotect);
	    return R_NilValue;
	}
	/* NB:  this code replicates .SigLabel().
	   If that changes, e.g. to include
	   the package, the code here must change too.
	   Or, better, the two should use the same C code. */
	if(ptr > buf) { ptr = strcpy(ptr, "#");  ptr += 2;}
	ptr = strcpy(ptr, class); ptr += strlen(class);
	value = findVarInFrame(mtable, install(buf));
	if(value != R_UnboundValue) {
	    retValue = value;
	    break;
	}
    }
    UNPROTECT(nprotect);
    return(retValue);
}

/* call some S language functions */

static SEXP R_S_MethodsListSelect(SEXP fname, SEXP ev, SEXP mlist, SEXP f_env)
{
    SEXP e, val; int n, check_err;
    n = isNull(f_env) ? 4 : 5;
    PROTECT(e = allocVector(LANGSXP, n));
    SETCAR(e, s_MethodsListSelect);
    val = CDR(e);
    SETCAR(val, fname);
    val = CDR(val);
    SETCAR(val, ev);
    val = CDR(val);
    SETCAR(val, mlist);
    if(n == 5) {
	    val = CDR(val);
	    SETCAR(val, f_env);
    }
    val = R_tryEval(e, Methods_Namespace, &check_err);
    if(check_err)
	error(_("S language method selection got an error when called from internal dispatch for function '%s'"),
	      check_symbol_or_string(fname, TRUE,
				  "Function name for method selection called internally"));
    UNPROTECT(1);
    return val;
}


/* quick tests for generic and non-generic functions.  May mistakenly
   identify non-generics as generics:  a class with data part of type
   CLOSXP and with a slot/attribute named "generic" will qualify.
*/
#define IS_NON_GENERIC(vl) (TYPEOF(vl) == BUILTINSXP ||TYPEOF(vl) == SPECIALSXP || \
            (TYPEOF(vl) == CLOSXP && getAttrib(vl, s_generic) == R_NilValue))
#define IS_GENERIC(vl) (TYPEOF(vl) == CLOSXP && getAttrib(vl, s_generic) != R_NilValue)
#define PACKAGE_SLOT(vl) getAttrib(vl, s_package)

static SEXP get_generic(SEXP symbol, SEXP rho, SEXP package)
{
    SEXP vl, generic = R_UnboundValue, gpackage; const char *pkg; Rboolean ok;
    if(!isSymbol(symbol))
	symbol = install(CHAR(asChar(symbol)));
    pkg = CHAR(STRING_ELT(package, 0)); /* package is guaranteed single string */

    while (rho != R_NilValue) {
	vl = findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    ok = FALSE;
	    if(IS_GENERIC(vl)) {
	      if(strlen(pkg)) {
		  gpackage = PACKAGE_SLOT(vl);
		  check_single_string(gpackage, FALSE, "The \"package\" slot in generic function object");
		  ok = !strcmp(pkg, CHAR(STRING_ELT(gpackage, 0)));
		}
		else
		  ok = TRUE;
	    }
	    if(ok) {
		generic = vl;
		break;
	    } else
		vl = R_UnboundValue;
	}
	rho = ENCLOS(rho);
    }
    /* look in base if either generic is missing */
    if(generic == R_UnboundValue) {
	vl = SYMVALUE(symbol);
	if(IS_GENERIC(vl)) {
	    generic = vl;
	    if(strlen(pkg)) {
		gpackage = PACKAGE_SLOT(vl);
		check_single_string(gpackage, FALSE, "The \"package\" slot in generic function object");
		if(strcmp(pkg, CHAR(STRING_ELT(gpackage, 0)))) generic = R_UnboundValue;
	    }
	}
    }
    return generic;
}

SEXP R_getGeneric(SEXP name, SEXP mustFind, SEXP env, SEXP package)
{
    SEXP value;
    if(isSymbol(name)) {}
    else check_single_string(name, TRUE, "The argument \"f\" to getGeneric");
    check_single_string(package, FALSE, "The argument \"package\" to getGeneric");
    value = get_generic(name, env, package);
    if(value == R_UnboundValue) {
	if(asLogical(mustFind)) {
	    if(env == R_GlobalEnv)
		error(_("no generic function definition found for '%s'"),
		  CHAR(asChar(name)));
	    else
		error(_("No generic function definition found for '%s' in the supplied environment"),
		  CHAR(asChar(name)));
	}
	value = R_NilValue;
    }
    return value;
}


/* C version of the standardGeneric R function. */
SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    SEXP f_env=R_BaseEnv, mlist=R_NilValue, f, val=R_NilValue, fsym; /* -Wall */
    int nprotect = 0; Rboolean prim_case = FALSE;

    if(!initialized)
	R_initMethodDispatch(NULL);
    fsym = fname;
    /* TODO:  the code for do_standardGeneric does a test of fsym,
     * with a less informative error message.  Should combine them.*/
    if(!isSymbol(fsym)) {
	const char *fname = check_single_string(fsym, TRUE, "The function name in the call to standardGeneric");
	fsym = install(fname);
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	PROTECT(mlist = findVar(s_dot_Methods, f_env)); nprotect++;
	if(mlist == R_UnboundValue)
            mlist = R_NilValue;
	prim_case = FALSE;
	break;
    case SPECIALSXP: case BUILTINSXP:
        f_env = R_BaseEnv;
	PROTECT(mlist = R_primitive_methods(fdef)); nprotect++;
	prim_case = TRUE;
	break;
    default: error(_("invalid generic function object for method selection for function '%s': expected a function or a primitive, got an object of class \"%s\""),
		   CHAR(asChar(fsym)), class_string(fdef));
    }
    switch(TYPEOF(mlist)) {
    case NILSXP:
    case CLOSXP:
    case SPECIALSXP: case BUILTINSXP:
	f = mlist; break;
    default:
	f = do_dispatch(fname, ev, mlist, TRUE, TRUE);
    }
    if(isNull(f)) {
	SEXP value;
	PROTECT(value = R_S_MethodsListSelect(fname, ev, mlist, f_env)); nprotect++;
	if(isNull(value))
	    error(_("no direct or inherited method for function '%s' for this call"),
		  CHAR(asChar(fname)));
	mlist = value;
	/* now look again.  This time the necessary method should
	   have been inserted in the MethodsList object */
	f = do_dispatch(fname, ev, mlist, FALSE, TRUE);
    }
    /* loadMethod methods */
    if(isObject(f))
	f = R_loadMethod(f, fname, ev);
    switch(TYPEOF(f)) {
    case CLOSXP:
	{
	    SEXP R_execMethod(SEXP, SEXP);
	    PROTECT(f); nprotect++; /* is this needed?? */
	    val = R_execMethod(f, ev);
	}
	break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
      val = R_deferred_default_method();
      break;
    default:
	error(_("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

/* Is the argument missing?  This _approximates_ the classic S sense of
   the question (is the argument missing in the call), rather than the
   R semantics (is the value of the argument R_MissingArg), but not if
   computations in the body of the function may have assigned to the
   argument name.
*/
static Rboolean is_missing_arg(SEXP symbol, SEXP ev)
{
    R_varloc_t loc = R_findVarLocInFrame(ev, symbol);
    if (loc == NULL)
	error(_("could not find symbol '%s' in frame of call"),
	      CHAR(asChar(symbol)));
    return R_GetVarLocMISSING(loc);
}

SEXP R_missingArg(SEXP symbol, SEXP ev)
{
    if(!isSymbol(symbol))
	error(_("invalid symbol in checking for missing argument in method dispatch: expected a name, got an object of class \"%s\""),
	     class_string(symbol));
    if (isNull(ev)) {
	error(_("use of NULL environment is defunct"));
	ev = R_BaseEnv;
    } else
    if(!isEnvironment(ev))
	error(_("invalid environment in checking for missing argument, '%s', in methods dispatch: got an object of class \"%s\""),
	     CHAR(PRINTNAME(symbol)), class_string(ev));
    if(is_missing_arg(symbol, ev))
	return R_TRUE;
    else
	return R_FALSE;
}



SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist, SEXP evalArgs)
{
    return do_dispatch(fname, ev, mlist, TRUE, asLogical(evalArgs));
}

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs)
{
    const char *class;
    SEXP arg_slot, arg_sym, method, value = R_NilValue;
    int nprotect = 0;
    /* check for dispatch turned off inside MethodsListSelect */
    if(isFunction(mlist))
	return mlist;
    PROTECT(arg_slot = R_do_slot(mlist, s_argument)); nprotect++;
    if(arg_slot == R_NilValue) {
	error(_("object of class \"%s\" used as methods list for function '%s' ( no 'argument' slot)"),
	      class_string(mlist), CHAR(asChar(fname)));
	return(R_NilValue); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = install(CHAR(asChar(arg_slot)));
    if(arg_sym == R_DotsSymbol || DDVAL(arg_sym) > 0)
	error(_("(in selecting a method for function '%s') '...' and related variables cannot be used for methods dispatch"),
	      CHAR(asChar(fname)));
    if(TYPEOF(ev) != ENVSXP) {
	error(_("(in selecting a method for function '%s') the 'environment' argument for dispatch must be an R environment; got an object of class \"%s\""),
	    CHAR(asChar(fname)), class_string(ev));
	return(R_NilValue); /* -Wall */
    }
    /* find the symbol in the frame, but don't use eval, yet, because
       missing arguments are ok & don't require defaults */
    if(evalArgs) {
	if(is_missing_arg(arg_sym, ev))
	    class = "missing";
	else {
	    /*  get its class */
	    SEXP arg, class_obj; int check_err;
	    PROTECT(arg = R_tryEval(arg_sym, ev, &check_err)); nprotect++;
	    if(check_err)
		error(_("error in evaluating the argument '%s' in selecting a method for function '%s'"),
		      CHAR(PRINTNAME(arg_sym)),CHAR(asChar(fname)));
	    PROTECT(class_obj = R_data_class(arg, TRUE)); nprotect++;
	    class = CHAR(STRING_ELT(class_obj, 0));
	}
    }
    else {
	/* the arg contains the class as a string */
	SEXP arg; int check_err;
	PROTECT(arg = R_tryEval(arg_sym, ev, &check_err)); nprotect++;
	if(check_err)
	    error(_("error in evaluating the argument '%s' in selecting a method for function '%s'"),
		  CHAR(PRINTNAME(arg_sym)),CHAR(asChar(fname)));
	class = CHAR(asChar(arg));
    }
    method = R_find_method(mlist, class, fname);
    if(isNull(method)) {
      if(!firstTry)
	error(_("no matching method for function '%s' (argument '%s', with class \"%s\")"),
	      CHAR(asChar(fname)), CHAR(PRINTNAME(arg_sym)), class);
      UNPROTECT(nprotect);
      return(R_NilValue);
    }
    if(value == R_MissingArg) {/* the check put in before calling
			  function  MethodListSelect in R */
      error(_("recursive use of function '%s' in method selection, with no default method"),
		  CHAR(asChar(fname)));
      return(R_NilValue);
    }
    if(!isFunction(method)) {
	/* assumes method is a methods list itself.  */
	/* call do_dispatch recursively.  Note the NULL for fname; this is
	   passed on to the S language search function for inherited
	   methods, to indicate a recursive call, not one to be stored in
	   the methods metadata */
	method = do_dispatch(R_NilValue, ev, method, firstTry, evalArgs);
    }
    UNPROTECT(nprotect); nprotect = 0;
    return method;
}

SEXP R_M_setPrimitiveMethods(SEXP fname, SEXP op, SEXP code_vec,
			     SEXP fundef, SEXP mlist)
{
    return R_set_prim_method(fname, op, code_vec, fundef, mlist);
}

SEXP R_nextMethodCall(SEXP matched_call, SEXP ev)
{
    SEXP e, val, args, this_sym, op;
    int nprotect = 0, i, nargs = length(matched_call)-1, error_flag;
    Rboolean prim_case, dotsDone;
    /* for primitive .nextMethod's, suppress further dispatch to avoid
     * going into an infinite loop of method calls
    */
    op = findVarInFrame3(ev, R_dot_nextMethod, TRUE);
    if(op == R_UnboundValue)
	error(_("internal error in 'callNextMethod': '.nextMethod' was not assigned in the frame of the method call"));
    /* If "..." is an argument, need to pass it down to next method;
     * match.call() doesn't seem (always?) to include this, so we
     * check below and add it if needed. */
    dotsDone = (findVarInFrame3(ev, R_DotsSymbol, TRUE) == R_UnboundValue);
    {PROTECT(e = duplicate(matched_call)); nprotect++;}
    if(!dotsDone) {
	SEXP ee = e, dots;
	PROTECT(dots = allocVector(LANGSXP, 1)); nprotect++;
	SETCAR(dots, R_DotsSymbol);
	for(ee = e; CDR(ee) != R_NilValue; ee = CDR(ee));
	SETCDR(ee, dots);
    }
    prim_case = isPrimitive(op);
    if(prim_case) {
	/* retain call to primitive function, suppress method
	   dispatch for it */
        do_set_prim_method(op, "suppress", R_NilValue, R_NilValue);
	PROTECT(op); nprotect++; /* needed? */
    }
    else
	SETCAR(e, R_dot_nextMethod); /* call .nextMethod instead */
    args = CDR(e);
    /* e is a copy of a match.call, with expand.dots=FALSE.  Turn each
    <TAG>=value into <TAG> = <TAG>, except  ... = goes into ... (if it
    appears) and there may be a "..." included */
    for(i=0; i<nargs; i++) {
	this_sym = TAG(args);
	if(this_sym == R_DotsSymbol) {
	    /* don't copy this; will have been appended */
	    if(dotsDone)
		error(_("in processing 'callNextMethod', found a '...' in the matched call, but no corresponding '...' argument"));
	}
	else if(CAR(args) != R_MissingArg) /* "missing" only possible in primitive */
	    SETCAR(args, this_sym);
	args = CDR(args);
    }
    if(prim_case) {
	val = R_tryEval(e, ev, &error_flag);
	/* reset the methods:  R_NilValue for the mlist argument
	   leaves the previous function, methods list unchanged */
	do_set_prim_method(op, "set", R_NilValue, R_NilValue);
	if(error_flag)
	    Rf_error(_("error in evaluating a 'primitive' next method"));
    }
    else
	val = eval(e, ev);
    UNPROTECT(nprotect);
    return val;
}


static SEXP R_loadMethod(SEXP def, SEXP fname, SEXP ev)
{
    /* since this is called every time a method is dispatched with a
       definition that has a class, it should be as efficient as
       possible => we build in knowledge of the standard
       MethodDefinition and MethodWithNext slots.  If these (+ the
       class slot) don't account for all the attributes, regular
       dispatch is done. */
    SEXP s, attrib;
    int found = 1; /* we "know" the class attribute is there */
    for(s = attrib = ATTRIB(def); s != R_NilValue; s = CDR(s)) {
	SEXP t = TAG(s);
	if(t == R_target) {
	    defineVar(R_dot_target, CAR(s), ev); found++;
	}
	else if(t == R_defined) {
	    defineVar(R_dot_defined, CAR(s), ev); found++;
	}
	else if(t == R_nextMethod)  {
	    defineVar(R_dot_nextMethod, CAR(s), ev); found++;
	}
	else if(t == R_source)  {
	    /* ignore */ found++;
	}
    }
    defineVar(R_dot_Method, def, ev);
    /* this shouldn't be needed but check the generic being
       "loadMethod", which would produce a recursive loop */
    if(strcmp(CHAR(asChar(fname)), "loadMethod") == 0)
	return def;
    if(found < length(attrib)) {
	SEXP e, val;
	PROTECT(e = allocVector(LANGSXP, 4));
	SETCAR(e, R_loadMethod_name); val = CDR(e);
	SETCAR(val, def); val = CDR(val);
	SETCAR(val, fname); val = CDR(val);
	SETCAR(val, ev);
	val = eval(e, ev);
	UNPROTECT(1);
	return val;
    }
    else return def;
}

static const char *
check_single_string(SEXP obj, Rboolean nonEmpty, const char *what)
{
    const char *string = "<unset>"; /* -Wall */
    if(isString(obj)) {
	if(length(obj) != 1)
	    error(_("'%s' must be a single string (got a character vector of length %d)"),
		  what, length(obj));
	string = CHAR(STRING_ELT(obj, 0));
	if(nonEmpty && (! string || !string[0]))
	    error(_("'%s' must be a non-empty string; got an empty string"),
		  what);
    }
    else {
	error(_("'%s' must be a single string (got an object of class \"%s\")"),
	      what, class_string(obj));
    }
    return string;
}

static const char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty,
                                          const char *what)
{
    if(isSymbol(obj))
	return CHAR(PRINTNAME(obj));
    else
	return check_single_string(obj, nonEmpty, what);
}

static const char *class_string(SEXP obj)
{
    return CHAR(STRING_ELT(R_data_class(obj, TRUE), 0));
}

/* internal version of paste(".", prefix, name, sep="__"),
   for speed so few checks */
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name)
{
    char str[201];
    const char *prefixString, *nameString;

    prefixString = check_single_string(prefix, TRUE,
				       "The internal prefix (e.g., \"C\") for a meta-data object");
    nameString = check_single_string(name, FALSE,
				     "The name of the object (e.g,. a class or generic function) to find in the meta-data");
    snprintf(str, 200, ".__%s__%s", prefixString, nameString);
    return mkString(str);
}

SEXP R_identC(SEXP e1, SEXP e2)
{
    if(TYPEOF(e1) == STRSXP && TYPEOF(e2) == STRSXP &&
       LENGTH(e1) == 1 && LENGTH(e2) == 1 &&
       streql(CHAR(STRING_ELT(e1, 0)), CHAR(STRING_ELT(e2, 0))))
	return R_TRUE;
    else
	return R_FALSE;
}

static SEXP do_inherited_table(SEXP class_objs, SEXP fdef, SEXP mtable, SEXP ev)
{
    static SEXP dotFind = NULL, f; SEXP  e, ee;
    if(dotFind == NULL) {
	dotFind = install(".InheritForDispatch");
	f = findFun(dotFind, R_MethodsNamespace);
    }
    PROTECT(e = allocVector(LANGSXP, 4));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, class_objs); ee = CDR(ee);
    SETCAR(ee, fdef); ee = CDR(ee);
    SETCAR(ee, mtable);
    ee = eval(e, ev);
    UNPROTECT(1);
    return ee;
}

static SEXP do_mtable(SEXP fdef, SEXP ev)
{
    static SEXP dotFind = NULL, f; SEXP  e, ee;
    if(dotFind == NULL) {
	dotFind = install(".getMethodsTable");
	f = findFun(dotFind, R_MethodsNamespace);
	R_PreserveObject(f);
    }
    PROTECT(e = allocVector(LANGSXP, 2));
    SETCAR(e, f); ee = CDR(e);
    SETCAR(ee, fdef);
    ee = eval(e, ev);
    UNPROTECT(1);
    return ee;
}

SEXP R_dispatchGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    static SEXP R_mtable = NULL, R_allmtable, R_sigargs, R_siglength;
    int nprotect = 0;
    SEXP mtable, classes, thisClass, sigargs, siglength, f_env = R_NilValue,
	method, f, val=R_NilValue;
    char *buf, *bufptr;
    int nargs, i, lwidth = 0;
    Rboolean prim_case = FALSE;

    if(!R_mtable) {
	R_mtable = install(".MTable");
	R_allmtable = install(".AllMTable");
	R_sigargs = install(".SigArgs");
	R_siglength = install(".SigLength");
    }
    switch(TYPEOF(fdef)) {
    case CLOSXP:
        f_env = CLOENV(fdef);
	break;
    case SPECIALSXP: case BUILTINSXP:
	PROTECT(fdef = R_primitive_generic(fdef)); nprotect++;
	if(TYPEOF(fdef) != CLOSXP) {
	    error(_("Failed to get the generic for the primitive \"%s\""), CHAR(asChar(fname)));
	    return R_NilValue;
	}
	f_env = CLOENV(fdef);
	prim_case = TRUE;
	break;
    default:
	error(_("Expected a generic function or a primitive for dispatch, got an object of class \"%s\""),
	      class_string(fdef));
    }
    PROTECT(mtable = findVarInFrame(f_env, R_allmtable)); nprotect++;
    if(mtable == R_UnboundValue) {
	do_mtable(fdef, ev); /* Should initialize the generic */
	PROTECT(mtable = findVarInFrame(f_env, R_allmtable)); nprotect++;
    }
    PROTECT(sigargs = findVarInFrame(f_env, R_sigargs)); nprotect++;
    PROTECT(siglength = findVarInFrame(f_env, R_siglength)); nprotect++;
    if(sigargs == R_UnboundValue || siglength == R_UnboundValue ||
       mtable == R_UnboundValue)
	error(_("Generic \"%s\" seems not to have been initialized for table dispatch---need to have .SigArgs and .AllMtable assigned in its environment"));
    nargs = NUMERIC_VALUE(siglength);
    PROTECT(classes = NEW_LIST(nargs)); nprotect++;
    for(i = 0; i < nargs; i++) {
	SEXP arg_sym = VECTOR_ELT(sigargs, i);
	if(is_missing_arg(arg_sym, ev))
	    thisClass = s_missing;
	else {
	    /*  get its class */
	    SEXP arg; int check_err;
	    PROTECT(arg = R_tryEval(arg_sym, ev, &check_err));
	    if(check_err)
		error(_("error in evaluating the argument '%s' in selecting a method for function '%s'"),
		      CHAR(PRINTNAME(arg_sym)),CHAR(asChar(fname)));
	    PROTECT(thisClass = R_data_class(arg, TRUE)); nprotect++;
	    UNPROTECT(1); /* for arg */
	}
	SET_VECTOR_ELT(classes, i, thisClass);
	lwidth += strlen(STRING_VALUE(thisClass)) + 1;
    }
    /* make the label */
    buf = (char *) R_alloc(lwidth + 1, sizeof(char));
    bufptr = buf;
    for(i = 0; i<nargs; i++) {
	if(i > 0)
	    *bufptr++ = '#';
	thisClass = VECTOR_ELT(classes, i);
	strcpy(bufptr, STRING_VALUE(thisClass));
	while(*bufptr)
	    bufptr++;
    }
    method = findVarInFrame(mtable, install(buf));
    if(method == R_UnboundValue) {
	method = do_inherited_table(classes, fdef, mtable, ev);
    }
    /* the rest of this is identical to R_standardGeneric; 
       hence the f=method to remind us  */
    f = method;
    if(isObject(f))
	f = R_loadMethod(f, fname, ev);
    switch(TYPEOF(f)) {
    case CLOSXP:
    {
	SEXP R_execMethod(SEXP, SEXP);
	PROTECT(f); nprotect++; /* is this needed?? */
	val = R_execMethod(f, ev);
    }
    break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
	val = R_deferred_default_method();
	break;
    default:
	error(_("invalid object (non-function) used as method"));
	break;
    }
    UNPROTECT(nprotect);
    return val;
}

SEXP R_set_method_dispatch(SEXP onOff)
{
    Rboolean value, prev; SEXP x;
    prev = table_dispatch_on;
    value = LOGICAL_VALUE(onOff);
    if(value == NA_LOGICAL) /*  just return previous*/
	value = prev;
    table_dispatch_on = value;
    if(value != prev) {
	R_set_standardGeneric_ptr(
	    (table_dispatch_on ? R_dispatchGeneric : R_standardGeneric)
	    , Methods_Namespace);
	R_set_quick_method_check(
	    (table_dispatch_on ? R_quick_dispatch : R_quick_method_check));
    }
    x = NEW_LOGICAL(1);
    LOGICAL_DATA(x)[0] = prev;
    return x;
}
