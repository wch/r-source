
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "RSMethods.h"

/* from Defn.h */
#define type2str		Rf_type2str
SEXP type2str(SEXPTYPE);
#define type2symbol		Rf_type2symbol
SEXP type2symbol(SEXPTYPE);
#define setVarInFrame		Rf_setVarInFrame
SEXP setVarInFrame(SEXP, SEXP, SEXP);
#define streql(s, t)	(!strcmp((s), (t)))
void R_PreserveObject(SEXP);

SEXP Rf_append(SEXP, SEXP);

/* environment cell access */
typedef struct R_varloc_st *R_varloc_t;
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);

/* from Defn.h */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP);
R_stdGen_ptr_t R_get_standardGeneric_ptr(); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t); /* set method */


/* attrib.c */
SEXP R_data_class(SEXP obj, int singleString);


/* from main/subassign.c */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val);

/* from main/objects.c */
SEXP R_deferred_default_method();
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef, 
		       SEXP mlist);
SEXP R_primitive_methods(SEXP op);
SEXP do_set_prim_method(SEXP op, char *code_string, SEXP fundef, SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_do_slot(SEXP obj, SEXP name); 

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
  s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_generic,
  s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets,
    s_argument, s_allMethods;
static SEXP R_FALSE, R_TRUE;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


SEXP R_quick_method_check(SEXP object, SEXP fsym, SEXP fdef);
	
static SEXP R_target, R_defined, R_nextMethod;
static SEXP R_dot_target, R_dot_defined, R_dot_nextMethod;
static SEXP R_loadMethod_name, R_dot_Method;

static char *check_single_string(SEXP, Rboolean, char *);
static char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty, char *what);
static char *class_string(SEXP obj);

static void init_loadMethod() {
    R_target = install("target");
    R_defined = install("defined");
    R_nextMethod = install("nextMethod");
    R_loadMethod_name = install("loadMethod");
    R_dot_target = install(".target");
    R_dot_defined = install(".defined");
    R_dot_nextMethod = install(".nextMethod");
    R_dot_Method = install(".Method");
}

void R_initMethodDispatch()
{
    R_set_standardGeneric_ptr(R_standardGeneric);
    if(initialized)
	return;
    R_set_quick_method_check(R_quick_method_check);
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
    s_generic_dot_skeleton = Rf_install("generic.skeleton");
    s_subset_gets = Rf_install("[<-");
    s_element_gets = Rf_install("[[<-");
    s_argument = Rf_install("argument");
    s_allMethods = Rf_install("allMethods");

    R_FALSE = PROTECT(NEW_LOGICAL(1));
    LOGICAL_POINTER(R_FALSE)[0] = FALSE;
    R_PreserveObject(R_FALSE);
    R_TRUE = PROTECT(NEW_LOGICAL(1));
    LOGICAL_POINTER(R_TRUE)[0] = TRUE;
    R_PreserveObject(R_TRUE);
    UNPROTECT(2);

    /* some strings (NOT symbols) */
    s_missing = PROTECT(NEW_CHARACTER(1));
    SET_STRING_ELT(s_missing, 0, mkChar("missing"));
    R_PreserveObject(s_missing);
    UNPROTECT(1);


    /* some special lists of primitive skeleton calls */
    R_PreserveObject(R_short_skeletons =
		     findVar(Rf_install(".ShortPrimitiveSkeletons"), 
			     R_GlobalEnv));
    R_PreserveObject(R_empty_skeletons =
		     findVar(Rf_install(".EmptyPrimitiveSkeletons"), 
			     R_GlobalEnv));
    if(R_short_skeletons == R_UnboundValue || 
       R_empty_skeletons == R_UnboundValue)
	error("Couldn't find the skeleton calls for methods (package  detached?): expect very bad things to happen");
    f_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 0);
    fgets_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 1);
    f_x_skeleton = VECTOR_ELT(R_empty_skeletons, 0);
    fgets_x_skeleton = VECTOR_ELT(R_empty_skeletons, 1);
    init_loadMethod();
    initialized = 1;
}


#ifdef UNUSED
/* return a symbol containing mode (well, actually, typeof) obj */
static SEXP R_mode(SEXP obj)
{
    return type2symbol(TYPEOF(obj));
}
#endif

/* the  SEXP for the data.class string (roughly, the green book class function) */


/* simplified version of do_subset2_dflt, with no partial matching */
static SEXP R_element_named(SEXP obj, char * what)
{
    int offset = -1, i, n;
    SEXP names = getAttrib(obj, R_NamesSymbol);
    n = length(names);
    if(n > 0) {
	for(i=0; i < n; i++) {
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

#ifdef UNUSED
static SEXP R_insert_element(SEXP mlist, char * what, SEXP object)
{
    SEXP sym = install(what);
    return R_subassign3_dflt(R_NilValue, mlist, sym, object);
}
#endif

/*  */
static SEXP ov_mlists[50], ov_methods[50];
static int n_ov = 0, max_ov = 50;
/* should make this flexible via malloc, realloc */
static SEXP getOverride(SEXP mlist)
{
    int i;
    for(i=0; i<n_ov; i++)
	if(ov_mlists[i] == mlist)
	    return ov_methods[i];
    return R_NilValue;
}

static SEXP setOverride(SEXP mlist, SEXP value)
{
    int i;
    for(i=0; i<n_ov; i++) {
	if(ov_mlists[i] == mlist) { /* set or clear */
	    if(value)
		ov_methods[i] = value;
	    else {
		ov_mlists[i] = NULL;
		if(i+1 == n_ov)
			n_ov--;
	    }
	    return value;
	}
	else if(!ov_mlists[i]) {
	    ov_mlists[i] = mlist;
	    ov_methods[i] = value;
	    return value;
	}
    }
    if(!value)
	    return value;
    if(n_ov >= max_ov) {
	/* should flex, but for now ... */
	n_ov = 0;
	error("More than %d nested methods list searches (system error?)", n_ov);
    }
    ov_mlists[n_ov] = mlist;
    ov_methods[n_ov] = value;
    n_ov++;
    return value;
}

SEXP R_clear_method_selection()
{
    n_ov = 0;
    return R_NilValue;
}

static SEXP R_find_method(SEXP mlist, char *class, SEXP fname)
{
    /* find the element of the methods list that matches this class,
       but not including inheritance. */
    SEXP value, methods;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue) {
	error("No \"allMethods\" slot found in object  of class \"%s\" used as methods list for function \"%s\"",
	      class_string(mlist), CHAR_STAR(fname));
	return(R_NilValue); /* -Wall */
    }
    value = R_element_named(methods, class);
    return value;
}

SEXP R_quick_method_check(SEXP args, SEXP mlist, SEXP fdef)
{
    /* Match the list of (evaluated) args to the methods list. */
    SEXP object, methods, value, retValue = R_NilValue;
    char *class; int nprotect = 0;
    if(!mlist)
	return R_NilValue;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue)
      {  return R_NilValue;}
    while(!isNull(args) && !isNull(methods)) {
	object = CAR(args); args = CDR(args);
	if(TYPEOF(object) == PROMSXP) {
	    if(PRVALUE(object) == R_UnboundValue) {
		SEXP tmp = eval(PREXPR(object), PRENV(object));
		PROTECT(tmp); nprotect++;
		SET_PRVALUE(object,  tmp);
		object = tmp;
	    }
	}
	class = CHAR(asChar(R_data_class(object, TRUE)));
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

/* call some S language functions */

static SEXP R_S_getAllMethods(SEXP fname, SEXP fdef)
{
    SEXP e, val, call;
    PROTECT(call = allocVector(LANGSXP, 3));
    SETCAR(call, s_getAllMethods);
    e = CDR(call);
    SETCAR(e, fname);
    e = CDR(e);
    SETCAR(e, fdef);
    /* We don't do checking here:  the calls from do_dispatch check
     * argument types. */
    val = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return(val);
}


static SEXP R_S_MethodsListSelect(SEXP fname, SEXP ev, SEXP mlist,
				  SEXP f_env)
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
    val = R_tryEval(e, R_GlobalEnv, &check_err);
    if(check_err)
	error("S language method selection got an error when called from internal dispatch for function \"%s\"",
	      check_symbol_or_string(fname, TRUE,
				  "Function name for method selection called internally"));
    UNPROTECT(1);
    return val;
}


static SEXP R_S_syscall(int n, SEXP ev)
{
    SEXP e, val, arg;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = n;
    SETCAR(e, s_sys_dot_call);
    SETCAR(CDR(e), arg);
    val = eval(e, ev);
    UNPROTECT(2);
    return val;
}

static SEXP R_S_sysfunction(int n, SEXP ev)
{
    SEXP e, val, arg;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_sys_dot_function, R_GlobalEnv));
    PROTECT(arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = n;
    SETCAR(e, val);
    SETCAR(CDR(e), arg);
    val = eval(e, ev);
    UNPROTECT(3);
    return val;
}

#if 0 /* -Wall warns and this confuses users */
static SEXP R_get_function_env(SEXP obj, SEXP fname)
{
    if(TYPEOF(obj) != CLOSXP)
	error("retrieved object for \"%s\" was not a function",
	      CHAR_STAR(fname));
    return CLOENV(obj);
}


static SEXP R_get_from_f_env(SEXP env, SEXP what, SEXP fname)
{
    SEXP obj;
    obj = findVarInFrame(env, what);
    if(obj == R_UnboundValue)
	error("No \"%s\" object in environment of function \"%s\"",
	      CHAR_STAR(what), CHAR_STAR(fname));
    return obj;
}
#endif

/* quick tests for generic and non-generic functions.  May mistakenly
   identify non-generics as generics:  a class with data part of type
   CLOSXP and with a slot/attribute named "generic" will qualify.
*/
#define IS_NON_GENERIC(vl) (TYPEOF(vl) == BUILTINSXP ||TYPEOF(vl) == SPECIALSXP || \
            (TYPEOF(vl) == CLOSXP && GET_ATTR(vl, s_generic) == R_NilValue))
#define IS_GENERIC(vl) (TYPEOF(vl) == CLOSXP && GET_ATTR(vl, s_generic) != R_NilValue)

static SEXP get_generic(SEXP symbol)
{
    SEXP rho = R_GlobalEnv;
    SEXP vl, generic = R_UnboundValue;
    if(!isSymbol(symbol))
	symbol = install(CHAR_STAR(symbol));
    while (rho != R_NilValue) {
	vl = findVarInFrame(rho, symbol);
	if (vl != R_UnboundValue) {
	    if (TYPEOF(vl) == PROMSXP) {
		PROTECT(vl);
		vl = eval(vl, rho);
		UNPROTECT(1);
	    }
	    if(IS_GENERIC(vl)) {
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
	if(IS_GENERIC(vl))
	    generic = vl;
    }
    return generic;
}

SEXP R_getGeneric(SEXP name, SEXP mustFind)
{
    SEXP value;
    if(isSymbol(name)) {}
    else check_single_string(name, TRUE, "The argument \"f\" to getGeneric");
    value = get_generic(name);
    if(value == R_UnboundValue) {
	if(LOGICAL_VALUE(mustFind))
	    error("No generic function definition for \"%s\"",
		  CHAR_STAR(name));
	value = R_NilValue;
    }
    return value;
}

static SEXP get_skeleton(SEXP symbol, SEXP generic)
{
    SEXP vl = R_UnboundValue;
    if(generic == R_NilValue)
	return R_NilValue; /* a primitive ? */
    /* get the skeleton call stored in the generic function, to use in case
       a call to this generic occurs in the S language code to merge
       methods and for the arguments to .Primitive methods */
    vl = GET_ATTR(generic, s_skeleton);
    if(vl == R_NilValue)
	error("Invalid generic function for \"%s\": no skeleton slot defined",
	      CHAR_STAR(symbol));
    return vl;
}

typedef enum {STANDARD, SUBSET, SUBSET_GETS, ELEMENT,
		     ELEMENT_GETS} primitive_type;

static primitive_type primitive_case(SEXP fname, SEXP op)
{
    /* assume fname is or has been made into a symbol */
    char *string;
    /* the only nonstandards are currently SPECIAL, not BUILTIN */
    if(TYPEOF(op) == BUILTINSXP)
	return STANDARD;
    string = CHAR(PRINTNAME(fname));
    switch(string[0]) {
    case '[':
	switch(string[1]) {
	case '\0': return SUBSET;
	case '[':
	    switch(string[2]) {
	    case '<':
		return (fname == s_element_gets ? ELEMENT_GETS : STANDARD);
	    case '\0': return ELEMENT;
	    default: return STANDARD;
	    }
	case '<': return (fname == s_subset_gets ? SUBSET_GETS : STANDARD);
	default: return STANDARD;
	}
    default: return STANDARD;
    }
}

static SEXP nonstandard_primitive(primitive_type which, SEXP skeleton,
				  SEXP prim, SEXP ev)
{
    SEXP call, frame, val, p1, p2; int nargs, i;
    frame = R_S_sysfunction(-1, ev);
    PROTECT(call = R_S_syscall(0, ev));
    nargs = length(call)-1;
    switch(which) {
    case SUBSET: case ELEMENT:
	switch(nargs) {
	case 2: val = f_x_i_skeleton; break;
	case 1: val = f_x_skeleton; break;
	default: val = skeleton; break;
	}
	break;
    case SUBSET_GETS: case ELEMENT_GETS:
	switch(nargs) {
	case 3: val = fgets_x_i_skeleton; break;
	case 2: val = fgets_x_skeleton; break;
	default: val = skeleton; break;
	}
	break;
    default:
	val = skeleton; break;
    }
    PROTECT(val = duplicate(val));
    /* the primitive call must have the correct pattern of missing
       actual arguments */
    p1 = CDR(call); p2 = CDR(val);
    for(i=0; i<nargs; i++) {
	if(CAR(p1) == R_MissingArg)
	    SETCAR(p2, R_MissingArg);
	p1 = CDR(p1); p2 = CDR(p2);
    }
    SETCAR(val, prim);
    UNPROTECT(2);
    return(val);
}


/* C version of the standardGeneric R function. */
SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef)
{
    SEXP f_env=R_NilValue, mlist=R_NilValue, f, val, call, fsym; /* -Wall */
    int nprotect = 0; Rboolean prim_case = FALSE;

    if(!initialized)
	R_initMethodDispatch();
    fsym = fname;
    /* TODO:  the code for do_standardGeneric does a test of fsym,
     * with a less informative error message.  Should combine them.*/
    if(!isSymbol(fsym)) {
	char *fname = check_single_string(fsym, TRUE, "The function name in the call to standardGeneric");
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
        f_env = R_NilValue;
	PROTECT(mlist = R_primitive_methods(fdef)); nprotect++;
	prim_case = TRUE;
	break;
    default: error("Invalid  generic function object for method selection for function \"%s\": expected a function or a primitive, got an object of class \"%s\"",
		   CHAR_STAR(fsym), class_string(fdef));
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
	    error("No direct or inherited method for function \"%s\" for this call",
		  CHAR_STAR(fname));
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
	error("invalid object (non-function) used as method");
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
    SEXP args = FRAME(ev);
    while(args != R_NilValue) {
	if(TAG(args) == symbol)
	    return MISSING(args);
	args = CDR(args);
    }
    error("Couldn't find symbol \"%s\" in frame of call",
	  CHAR_STAR(symbol));
    return FALSE;		/* -Wall */
}

SEXP R_missingArg(SEXP symbol, SEXP ev) {
    if(!isSymbol(symbol))
	error("invalid symbol in checking for missing argument in method dispatch: expected a name, got an object of class \"%s\"",
	     class_string(symbol));
    if(!isEnvironment(ev))
	error("invalid environment in checking for missing argument, \"%s\", in methods dispatch: got an object of class \"%s\"",
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
    char *class;
    SEXP arg_slot, arg_sym, method, value = R_NilValue;
    int nprotect = 0;
    PROTECT(arg_slot = R_do_slot(mlist, s_argument)); nprotect++;
    if(arg_slot == R_NilValue) {
	error("Object of class \"%s\" used as methods list for function \"%s\" ( no \"argument\" slot)",
	      class_string(mlist), CHAR_STAR(fname));
	return(R_NilValue); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = install(CHAR(asChar(arg_slot)));
    if(arg_sym == R_DotsSymbol || DDVAL(arg_sym) > 0)
	error("(in selecting a method for function \"%s\") \"...\" and related variables can't be used for methods dispatch",
	      CHAR_STAR(fname));
    if(TYPEOF(ev) != ENVSXP) {
	error("(in selecting a method for function \"%s\") The environment argument for dispatch must be an R environment; got an object of class \"%s\"",
	    CHAR_STAR(fname), class_string(ev));
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
		error("Unable to find the argument \"%s\" in selecting a method for function \"%s\"",
		      CHAR(PRINTNAME(arg_sym)),CHAR_STAR(fname)); 
	    PROTECT(class_obj = R_data_class(arg, TRUE)); nprotect++;
	    class = CHAR_STAR(class_obj);
	}
    }
    else {
	/* the arg contains the class as a string */
	SEXP arg; int check_err;
	PROTECT(arg = R_tryEval(arg_sym, ev, &check_err)); nprotect++;
	if(check_err)
	    error("Unable to find the argument \"%s\" in selecting a method for function \"%s\"",
		  CHAR(PRINTNAME(arg_sym)),CHAR_STAR(fname)); 
	class = CHAR_STAR(arg);
    }
    method = R_find_method(mlist, class, fname);
    if(isNull(method)) {
      if(!firstTry)
	error("No matching method for function \"%s\" (argument \"%s\", with class %s)",
	      CHAR_STAR(fname), CHAR(PRINTNAME(arg_sym)), class);
      UNPROTECT(nprotect);
      return(R_NilValue);
    }
    if(value == R_MissingArg) {/* the check put in before calling
			  function  MethodListSelect in R */
      error("Recursive use of function \"%s\" in method selection, with no default method",
		  CHAR_STAR(fname));
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

SEXP R_nextMethodCall(SEXP matched_call, SEXP ev) {
    SEXP e, val, args, this_sym, op;
    int nprotect = 0, i, nargs = length(matched_call)-1, error_flag;
    Rboolean prim_case, dotsDone;
    /* for primitive .nextMethod's, suppress further dispatch to avoid
     * going into an infinite loop of method calls
    */
    op = findVarInFrame3(ev, R_dot_nextMethod, TRUE);
    if(op == R_UnboundValue)
	error("Internal error in callNextMethod: \".nextMethod\" was not assigned in the frame of the method call");
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
		error("In processing callNextMethod, found a \"...\" in the matched call, but no corresponding ... argument ");
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
	    Rf_error("Error in evaluating a primitive next method");
    }
    else
	val = eval(e, ev);
    UNPROTECT(nprotect);
    return val;
}


static SEXP R_loadMethod(SEXP def, SEXP fname, SEXP ev) {
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
    }
    defineVar(R_dot_Method, def, ev);
    /* this shouldn't be needed but check the generic being
       "loadMethod", which would produce a recursive loop */
    if(strcmp(CHAR_STAR(fname), "loadMethod") == 0)
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

static char *check_single_string(SEXP obj, Rboolean nonEmpty, char *what) {
    char *string = "<unset>"; /* -Wall */
    if(isString(obj)) {
	if(length(obj) != 1)
	    error("%s must be a single string (got a character vector of length %d)",
		  what, length(obj));
	string = CHAR(asChar(obj));
	if(nonEmpty && (! string || !string[0]))
	    error("%s must be a non-empty string; got an empty string", what);
    }
    else {
	error("%s must be a single string (got an object of class \"%s\")",
	      what, class_string(obj));
    }
    return string;
}

static char *check_symbol_or_string(SEXP obj, Rboolean nonEmpty, char *what) {
    if(isSymbol(obj))
	return CHAR(PRINTNAME(obj));
    else
	return check_single_string(obj, nonEmpty, what);
}

static char *class_string(SEXP obj) {
    return CHAR(asChar(R_data_class(obj, 1)));
}

/* internal version of paste(".", prefix, name, sep="__"), 
   for speed so few checks */
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name)
{
    SEXP ans;
    char str[201], *prefixString, *nameString;

    prefixString = check_single_string(prefix, TRUE,
				       "The internal prefix (e.g., \"C\") for a meta-data object");
    nameString = check_single_string(name, FALSE,
				     "The name of the object (e.g,. a class or generic function) to find in the meta-data");
    snprintf(str, 200, ".__%s__%s", prefixString, nameString);
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(str));
    UNPROTECT(1);
    return ans;
}
