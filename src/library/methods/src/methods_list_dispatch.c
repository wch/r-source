
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

/* environment cell access */
typedef struct R_varloc_st *R_varloc_t;
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);

/* from Defn.h */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP);
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
static SEXP s_dot_Arguments, s_expression, s_function,
  s_getAllMethods, s_objectsEnv, s_MethodsListSelect,
  s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_dot_Methods,
  s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets,
    s_argument, s_allMethods;
static SEXP R_FALSE, R_TRUE;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


SEXP R_quick_method_check(SEXP object, SEXP fsym);
	
static SEXP R_target, R_defined, R_nextMethod;
static SEXP R_dot_target, R_dot_defined, R_dot_nextMethod;
static SEXP R_loadMethod_name, R_dot_Method;

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
    if(initialized)
	return;
    R_set_standardGeneric_ptr(R_standardGeneric);
    R_set_quick_method_check(R_quick_method_check);
    s_dot_Arguments = Rf_install(".Arguments");
    s_expression = Rf_install("expression");
    s_function = Rf_install("function");
    s_getAllMethods = Rf_install("getAllMethods");
    s_objectsEnv = Rf_install("objectsEnv");
    s_MethodsListSelect = Rf_install("MethodsListSelect");
    s_sys_dot_frame = Rf_install("sys.frame");
    s_sys_dot_call = Rf_install("sys.call");
    s_sys_dot_function = Rf_install("sys.function");
    s_dot_Methods = Rf_install(".Methods");
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
    for(i=0; i<n_ov; i++)
	if(ov_mlists[i] == mlist) {
	    ov_methods[i] = value;
	    return value;
	}
    if(n_ov >= max_ov) {
	/* should flex, but for now ... */
	error("Overflowed allowed number of recursive function calls in  methods list search");
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
	error("No \"allMethods\" slot found in \"mlist\" object for %s!",
	      CHAR_STAR(fname));
	return(R_NilValue); /* -Wall */
    }
    value = R_element_named(methods, class);
    return value;
}

SEXP R_quick_method_check(SEXP args, SEXP mlist)
{
    /* Match the list of (evaluated) args to the methods list. */
    SEXP object, methods, value;
    char *class;
    if(!mlist)
	return R_NilValue;
    methods = R_do_slot(mlist, s_allMethods);
    if(methods == R_NilValue)
      {  return R_NilValue;}
    while(!isNull(args) && !isNull(methods)) {
	object = CAR(args); args = CDR(args);
	class = CHAR(asChar(R_data_class(object, TRUE)));
	value = R_element_named(methods, class);
	if(isNull(value) || isFunction(value))
	    return value;
	/* continue matching args down the tree */
	methods = R_do_slot(value, s_allMethods);
    }
    return(R_NilValue);
}

/* call some S language functions */

static SEXP R_S_getAllMethods(SEXP fname)
{
    SEXP e, val;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_getAllMethods, R_GlobalEnv));
    SETCAR(e, val);
    SETCAR(CDR(e), fname);
    val = eval(e, R_NilValue);
    UNPROTECT(2);
    return(val);
}

#ifdef UNUSED
static SEXP R_S_generic_skeleton(SEXP fname)
{
    SEXP e, val;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_generic_dot_skeleton, R_GlobalEnv));
    SETCAR(e, val);
    SETCAR(CDR(e), fname);
    val = eval(e, R_NilValue);
    UNPROTECT(2);
    return(val);
}

static SEXP R_S_objectsEnv(SEXP ev)
{
    SEXP e, val;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_objectsEnv, R_GlobalEnv));
    SETCAR(e, val);
    SETCAR(CDR(e), ev);
    val = eval(e, R_NilValue);
    UNPROTECT(2);
    return(val);
}

static SEXP R_S_sysframe(int n, SEXP ev)
{
    SEXP e, val, arg;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_sys_dot_frame, R_GlobalEnv));
    PROTECT(arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = n;
    SETCAR(e, val);
    SETCAR(CDR(e), arg);
    val = eval(e, ev);
    UNPROTECT(3);
    return val;
}
#endif


static SEXP R_S_MethodsListSelect(SEXP fname, SEXP ev, SEXP mlist,
				  SEXP f_env)
{
    SEXP e, val;
    PROTECT(e = allocVector(LANGSXP, 5));
    SETCAR(e, s_MethodsListSelect);
    val = CDR(e);
    SETCAR(val, fname);
    val = CDR(val);
    SETCAR(val, ev);
    val = CDR(val);
    SETCAR(val, mlist);
    val = CDR(val);
    SETCAR(val, f_env);
    val = eval(e, R_GlobalEnv);
    UNPROTECT(1);
    return val;
}


static SEXP R_S_syscall(int n, SEXP ev)
{
    SEXP e, val, arg;
    PROTECT(e = allocVector(LANGSXP, 2));
    PROTECT(val = Rf_findFun(s_sys_dot_call, R_GlobalEnv));
    PROTECT(arg = NEW_INTEGER(1));
    INTEGER_DATA(arg)[0] = n;
    SETCAR(e, val);
    SETCAR(CDR(e), arg);
    val = eval(e, ev);
    UNPROTECT(3);
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

#define IS_NON_GENERIC(vl) (TYPEOF(vl) == BUILTINSXP ||TYPEOF(vl) == SPECIALSXP || \
            (TYPEOF(vl) == CLOSXP && findVarInFrame(CLOENV(vl),\
						    s_dot_Methods) == R_UnboundValue))
#define IS_GENERIC(vl) (TYPEOF(vl) == CLOSXP && findVarInFrame(CLOENV(vl),\
						    s_dot_Methods) != R_UnboundValue)

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
    if(generic == R_NilValue) {
      generic = get_generic(symbol);
      if(generic == R_UnboundValue) /* none found: usually a primitive
				     */
	return R_NilValue;
    }
    if(!IS_GENERIC(generic))
	error("No generic function found for \"%s\"",
	      CHAR_STAR(symbol));
    /* get the skeleton call stored in the environment, to use in case
       a call to this generic occurs in the S language code to merge
       methods and for the arguments to .Primitive methods */
    vl = findVarInFrame(CLOENV(generic), s_dot_Arguments);
    if(vl == R_UnboundValue)
	error("Invalid generic function for \"%s\": no .Arguments defined",
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
SEXP R_standardGeneric(SEXP fname, SEXP ev)
{
    SEXP mlist, f, val, f_env = R_NilValue, fdef, call, fsym;
    int nprotect = 0; primitive_type prim_case;

    fsym = fname;
    if(!isSymbol(fsym))
	fsym = install(CHAR(asChar(fsym)));
    PROTECT(fdef = R_get_from_method_metadata(fsym)); nprotect++;
    if(!initialized)
	R_initMethodDispatch();
    switch(TYPEOF(fdef)) {
    case LANGSXP:
	/* a recursive call; picked up the skeleton call (to be used as
	   the method) */
	f = fdef; mlist = R_NilValue;
	break;
    case NILSXP:  {
	/* call the S language function to merge generic
	   information. First assign a special version to trap recursive
	   calls to the same generic. */
      SEXP skeleton;
      skeleton = get_skeleton(fsym, R_NilValue);
      if(!isNull(skeleton)) /* else, none found; is this an error? */
	R_assign_to_method_metadata(fsym, skeleton);
      PROTECT(fdef = R_S_getAllMethods(fname)); nprotect++;
      R_assign_to_method_metadata(fsym, fdef);
      if(fdef == R_NilValue) {
	error("\"%s\" has no defined methods", CHAR_STAR(fsym));
	return R_NilValue; /* -Wall */
      }
	/* else, continue to the default case */
    }
    default:
	PROTECT(f_env = R_get_function_env(fdef, fsym)); nprotect++;
	mlist = R_get_from_f_env(f_env, s_dot_Methods, fsym);
	PROTECT(mlist); nprotect++;
	f = do_dispatch(fname, ev, mlist, TRUE, TRUE);
	if(isNull(f)) {
	    /* call the S language code to do a search with inheritance */
	    SEXP value;
	    SEXP deflt, prev_fun = R_NilValue, op = R_NilValue; 
	    Rboolean prim_case, firstCall;
	    value = getOverride(mlist);
	    firstCall = (value == R_NilValue); prim_case = FALSE;
	    /* Avoid recursive loop in searching for a method.
	       Two cases:  the original function is a primitive (and
	       then the default method is forced to be also); or
	       the original function is a closure */
	    if(firstCall) {
		PROTECT(deflt = R_find_method(mlist, "ANY", fname)); nprotect++;
		prim_case = isPrimitive(deflt);
		if(prim_case) {
		    op = deflt;
		    /* just shut off dispatching of methods for this op */
		    PROTECT(prev_fun = 
			    do_set_prim_method(deflt, "suppress", NULL, NULL)); nprotect++;
		    /* TO DO:  use context control to ensure the restores in
		       case of an error */
		}
		else {
		    if(deflt == R_NilValue)
			deflt = R_MissingArg; /* a fixed value indicating no default
					       */
		    setOverride(mlist, deflt);
		    /* call the S function, it returns a revised MethodsList
		       object, and also stores the revised MethodsList in the
		       methods metadata.
		    */
		    R_assign_to_method_metadata(fsym, get_skeleton(fsym,
								   R_NilValue));
		}
		PROTECT(value = R_S_MethodsListSelect(fname, ev, mlist, f_env)); nprotect++;
		if(firstCall) {
		    if(prim_case) {
			do_set_prim_method(op, "set", prev_fun, value);
		    }
		    else {
			R_assign_to_method_metadata(fsym, fdef);
			R_clear_method_selection();		    }
		}
		if(isNull(value))
		    error("No direct or inherited method for function \"%s\" for this call",
			  CHAR_STAR(fname));
		mlist = value;
		/* now look again.  This time the necessary method should
		   have been inserted in the MethodsList object */
		f = do_dispatch(fname, ev, mlist, FALSE, TRUE);
	    }
	}
    }
    val = R_NilValue;
    /* loadMethod methods */
    if(isObject(f))
	f = R_loadMethod(f, fname, ev);
    switch(TYPEOF(f)) {
    case CLOSXP:
      PROTECT(val = BODY(f)); nprotect++;
	val =  eval(val, ev);
	break;
    case SPECIALSXP: case BUILTINSXP:
	/* primitives  can't be methods; they arise only as the
	   default method when a primitive is made generic.  In this
	   case, return a special marker telling the C code to go on
	   with the internal computations. */
      val = R_deferred_default_method();
      break;
    case LANGSXP:
	if(mlist == R_NilValue) {
	    /* a recursive call: use the skeleton default call */
	    call = f;
	    f = CAR(f);
	    prim_case = primitive_case(fsym, f);
	    if(prim_case)
		call = nonstandard_primitive(prim_case, call, f, ev);
	    else
		/* the skeleton is almost surely a call to the same primitive, 
		   but we don't need to assume that. */
		SETCAR(call, f);
	    PROTECT(call); nprotect++;
	    val =  eval(call, ev);
	    break;
	}
	/* else, it's an errror */
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
	error("invalid `symbol' argument: expected a name, got a \"%s\"",
	     CHAR_STAR((isObject(symbol) ? R_data_class(symbol, 1) :
	      type2str(TYPEOF(symbol)))));
    if(!isEnvironment(ev))
	error("invalid `envir' argument: expected an environment, got a \"%s\"",
	     CHAR_STAR((isObject(ev) ? R_data_class(ev, 1) :
	      type2str(TYPEOF(ev)))));
    if(is_missing_arg(symbol, ev))
	return R_TRUE;
    else
	return R_FALSE;
}

    

SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist)
{
    return do_dispatch(fname, ev, mlist, TRUE, FALSE);
}

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs)
{
    char *class;
    SEXP arg_slot, arg_sym, method, value = R_NilValue;
    int nprotect = 0;
    PROTECT(arg_slot = R_do_slot(mlist, s_argument)); nprotect++;
    if(arg_slot == R_NilValue) {
	error("methods list had no \"argument\" slot");
	return(R_NilValue); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = install(CHAR(asChar(arg_slot)));
    if(arg_sym == R_DotsSymbol || DDVAL(arg_sym) > 0)
	error("... and related variables can't be used for methods dispatch");
    if(TYPEOF(ev) != ENVSXP) {
	error("The environment argument for dispatch must be an R environment");
	return(R_NilValue); /* -Wall */
    }
    /* find the symbol in the frame, but don't use eval, yet, because
       missing arguments are ok & don't require defaults */
    if(evalArgs) {
	if(is_missing_arg(arg_sym, ev))
	    class = "missing";
	else {
	    /*  get its class */
	    SEXP arg, class_obj;
	    PROTECT(arg = eval(arg_sym, ev)); nprotect++;
	    PROTECT(class_obj = R_data_class(arg, TRUE)); nprotect++;
	    class = CHAR_STAR(class_obj);
	}
    }
    else {
	/* the arg contains the class as a string */
	SEXP arg;
	PROTECT(arg = eval(arg_sym, ev)); nprotect++;
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

SEXP R_nextMethodCall(SEXP argNames, SEXP ev) {
    SEXP e, val, args = FRAME(ev);
    int nprotect = 0, n = length(args), i, extras, nargs = length(argNames);
    PROTECT(e = allocVector(LANGSXP, nargs+1)); nprotect++;
    SETCAR(e, R_dot_nextMethod); val = CDR(e);
    /* the arguments are the formal name, or missing, and we assume
       the first n elements of the frame are the arguments, since the
       method definition is required to have the same args (this can
       be checked by comparing to argNames).
    */
    extras = n - nargs;
    /* we assume (can we?) that the last nargs elements in the frame
       are the arguments, everything else gets pushed down before */
    for(i=0; i< extras; i++)
	args = CDR(args);
    for(i=0; i<nargs; i++) {
	if(MISSING(args))
	    SETCAR(val, R_MissingArg);
	else
	    SETCAR(val, TAG(args));
	val = CDR(val);
	args = CDR(args);
    }
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
