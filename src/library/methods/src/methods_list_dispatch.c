
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "RSMethods.h"

/* from Defn.h */
#define type2str		Rf_type2str
SEXP type2str(SEXPTYPE);
#define type2symbol		Rf_type2symbol
SEXP type2symbol(SEXPTYPE);
#define findVarInFrame		Rf_findVarInFrame
#define setVarInFrame		Rf_setVarInFrame
SEXP findVarInFrame(SEXP, SEXP);
SEXP setVarInFrame(SEXP, SEXP, SEXP);
#define streql(s, t)	(!strcmp((s), (t)))
void R_PreserveObject(SEXP);

/* attrib.c */
SEXP R_data_class(SEXP obj, int singleString);


/* from main/subassign.c */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val);

/* the following utilities are included here for now, as statics.  But
   they will eventually be C implementations of slot, data.class,
   etc. */

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs);

/* objects, mostly symbols, that are initialized once to save a little time */
static int initialized = 0;
static SEXP s_dot_Arguments, s_expression, s_function,
  s_getAllMethods, s_objectsEnv, s_MethodsListSelect,
  s_sys_dot_frame, s_sys_dot_call, s_sys_dot_function, s_dot_Methods,
  s_missing, s_generic_dot_skeleton, s_subset_gets, s_element_gets;
static SEXP R_FALSE, R_TRUE;

/* precomputed skeletons for special primitive calls */
static SEXP R_short_skeletons, R_empty_skeletons;
static SEXP f_x_i_skeleton, fgets_x_i_skeleton, f_x_skeleton, fgets_x_skeleton;


/* from Defn.h */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP);
R_stdGen_ptr_t R_get_standardGeneric_ptr(); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t new); /* set method */


void R_initMethodDispatch()
{
    if(initialized)
	return;
    R_set_standardGeneric_ptr(R_standardGeneric);
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
		     findVar(Rf_install(".ShortPrimitiveSkeletons"), R_GlobalEnv));
    R_PreserveObject(R_empty_skeletons =
		     findVar(Rf_install(".EmptyPrimitiveSkeletons"), R_GlobalEnv));
    if(R_short_skeletons == R_UnboundValue || R_empty_skeletons == R_UnboundValue)
	error("Couldn't find the skeleton calls for methods (package  detached?): expect very bad things to happen");
    f_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 0);
    fgets_x_i_skeleton = VECTOR_ELT(R_short_skeletons, 1);
    f_x_skeleton = VECTOR_ELT(R_empty_skeletons, 0);
    fgets_x_skeleton = VECTOR_ELT(R_empty_skeletons, 1);
    initialized = 1;
}

/* a quick version of attribute (== slot) lookup.  Note that,
   unlike the attr function in R, this does NOT do partial
   matching. Also, no special interpretations are made
   (e.g. "names", "dim", or other reserved attribute names). */
static SEXP R_get_attr(SEXP obj, char *what) {
  SEXP alist;
  for (alist = ATTRIB(obj); alist != R_NilValue; alist = CDR(alist)) {
    SEXP tmp = TAG(alist);
    if(!strcmp(CHAR(PRINTNAME(tmp)), what))
      return(CAR(alist));
  }
  return(R_NilValue);
}

/* return a symbol containing mode (well, actually, typeof) obj */
static SEXP R_mode(SEXP obj)
{
    return type2symbol(TYPEOF(obj));
}


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

static SEXP R_insert_element(SEXP mlist, char * what, SEXP object)
{
    SEXP sym = install(what);
    return R_subassign3_dflt(R_NilValue, mlist, sym, object);
}

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
    methods = R_get_attr(mlist, "allMethods");
    if(methods == R_NilValue) {
	error("No \"methods\" slot found in \"mlist\" object for %s!",
	      CHAR_STAR(fname));
	return(R_NilValue); /* -Wall */
    }
    value = R_element_named(methods, class);
    return value;
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
    if(generic == R_NilValue)
	generic = get_generic(symbol);
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
	R_assign_to_method_metadata(fsym, get_skeleton(fsym, R_NilValue));
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
	  SEXP value = getOverride(mlist);
	  /* Avoid recursive loop in searching for a method:  if the S
	     language search calls a generic for which no direct method is
	     defined, we MUST use the override default method.  If this is
	     not a recursive call, we set that override attribute (and the
	     R_S_methodsListSearch code must unset it). */
	  if(value == R_NilValue) {
	    SEXP deflt;
	    deflt = R_find_method(mlist, "ANY", fname);
	    if(deflt == R_NilValue)
	      deflt = R_MissingArg; /* a fixed value indicating no default
				       */
	    setOverride(mlist, deflt);
	    /* call the S function, it returns a revised MethodsList
	       object, and also stores the revised MethodsList in the
	       methods metadata.
	    */
	    R_assign_to_method_metadata(fsym, get_skeleton(fsym, R_NilValue));
	    PROTECT(value = R_S_MethodsListSelect(fname, ev, mlist, f_env)); nprotect++;
	    R_assign_to_method_metadata(fsym, fdef);
	    R_clear_method_selection(); /* to be safe.
				     The S language code is supposed
				     to clear also. */
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
    switch(TYPEOF(f)) {
    case CLOSXP:
      PROTECT(val = BODY(f)); nprotect++;
	val =  eval(val, ev);
	break;
    case SPECIALSXP: case BUILTINSXP: {
	/* most primitives can be handled just by calling the skeleton
	   function, but some need special attention */
	call = get_skeleton(fsym, fdef);   /* get the call with the formal arguments in it */
	prim_case = primitive_case(fsym, f);
	if(prim_case)
	    call = nonstandard_primitive(prim_case, call, f, ev);
	else
	    /* the skeleton is almost surely a call to the same primitive, but we
	       don't need to assume that. */
	    SETCAR(call, f);
	PROTECT(call); nprotect++;
	val = eval(call, ev);
	break;
    }
    case LANGSXP:
	if(mlist == R_NilValue) {
	    /* a recursive call: use the skeleton default call */
	    call = f;
	    f = CAR(f);
	    prim_case = primitive_case(fsym, f);
	    if(prim_case)
		call = nonstandard_primitive(prim_case, call, f, ev);
	    else
		/* the skeleton is almost surely a call to the same primitive, but we
		   don't need to assume that. */
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

SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist)
{
  return do_dispatch(fname, ev, mlist, TRUE, FALSE);
}

static SEXP do_dispatch(SEXP fname, SEXP ev, SEXP mlist, int firstTry,
			int evalArgs)
{
    char *arg_name, *class;
    SEXP arg_slot, arg_sym, arg, method, value, child;
    int inherited, nprotect = 0;
    PROTECT(arg_slot = R_get_attr(mlist, "argument")); nprotect++;
    if(arg_slot == R_NilValue) {
	error("methods list had no \"argument\" slot");
	return(R_NilValue); /* -Wall */
    }
    if(TYPEOF(arg_slot) == SYMSXP)
	arg_sym = arg_slot;
    else {
	arg_name = CHAR(asChar(arg_slot));
	/* shouldn't happen, since argument in class MethodsList has class
	   "name" */
	arg_sym = install(arg_name);
    }
    if(TYPEOF(ev) != ENVSXP) {
	error("The environment argument for dispatch must be an R environment");
	return(R_NilValue); /* -Wall */
    }
    /* find the symbol in the frame, but don't use eval, yet, because
       missing arguments are ok & don't require defaults */
    arg = findVarInFrame(ev, arg_sym);
    if(evalArgs) {
      SEXP class_obj;
      if(arg == R_UnboundValue) {
	error("The specified argument (\"%s\") not found in the environment",
	      CHAR(PRINTNAME(arg_sym)));
	return(R_NilValue); /* -Wall */
      }
      if(arg == R_MissingArg) {
	arg = R_NilValue;
	class_obj = s_missing;
      }
      else {
	/* should be a formal argument in the frame, get its class */
	PROTECT(class_obj = R_data_class(eval(arg_sym, ev), TRUE)); nprotect++;
      }
      class = CHAR(asChar(class_obj));
    }
    else {
      if(arg == R_UnboundValue)
	class = "ANY";
      else
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
