
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

static SEXP pseudo_NULL = 0;

static void init_pseudo_NULL()
{
    /* create and preserve an object that is NOT R_NilValue, and is used
       to represent slots that are NULL (which an attribute can not
       be).  The point is not just to store NULL as a slot, but also to
       provide a check on invalid slot names (see get_slot below).

       The object has to be a symbol if we're going to check identity by
       just looking at referential equality. */
    pseudo_NULL = install("\001NULL\001");
}

SEXP R_get_slot(SEXP obj, SEXP name)
{
    /* currently we just use attributes, with all the current semantics
       (except of course for no partial matching).  (So all the vagaries
       of name, dimnames, etc carray over.)  Probably reasonable for
       back compatibility. */
    SEXP value = getAttrib(obj, name);
    if(value == R_NilValue)
	/* not there.  But since even NULL really does get stored, this
	   implies that there is no slot of this name.  Or somebody
	   screwed up by using atttr(..) <- NULL */
	error("\"%s\" is not a valid slot for this object (or was mistakenly deleted)",
	      CHAR(asChar(name)));
    else if(value == pseudo_NULL)
	value = R_NilValue;
    return value;
}

SEXP R_set_slot(SEXP obj, SEXP name, SEXP value)
{
    if(value == R_NilValue) {
	/* slots, but not attributes, can be NULL.  Store a special symbol
	   instead. */
	if(pseudo_NULL == 0)
	    init_pseudo_NULL();
	value = pseudo_NULL;
    }
    PROTECT(obj);
    setAttrib(obj, name, value);
    UNPROTECT(1);
    return obj;
}



