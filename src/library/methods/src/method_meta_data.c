
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "RSMethods.h"

/* from Defn.h */
#define streql(s, t)	(!strcmp((s), (t)))
#define setVarInFrame		Rf_setVarInFrame
SEXP setVarInFrame(SEXP, SEXP, SEXP);

#define CHECK_METHODS_TABLE(x) check_methods_table(x)

/* These tables should not be "global".  They will need to be specific
   to thread and to namespace.  Remains to design a correct but
   efficient way to select the correct table */
static SEXP methods_table = 0;

static void check_methods_table(char *op)
{
  if(methods_table == 0)
    error("invalid %s operation on methods metadata:  internal table not set",
	  op);
  else if(TYPEOF(methods_table) != ENVSXP)
    error("invalid methods table (type %d) in %s operation",
	  TYPEOF(methods_table), op);
}

SEXP R_initialize_methods_metadata(SEXP table)
{
    /* the table argument is assumed assigned somewhere (usually by
       First.lib in the RSMethods package) to protect it from garbage
       collection */
    methods_table = table;
    check_methods_table("initialize");
    return table;
}

SEXP R_get_from_method_metadata(SEXP name)
{
    SEXP value;
    check_methods_table("get");
    if(!isSymbol(name))
	name = install(CHAR(asChar(name)));
    value = findVarInFrame(methods_table, name);
    if(value == R_UnboundValue)
	value = R_NilValue;
    return value;
}

SEXP R_assign_to_method_metadata(SEXP name, SEXP value)
{
    check_methods_table("assign");
    if(!isSymbol(name))
	name = install(CHAR(asChar(name)));
    defineVar(name, value, methods_table);
    return(name);
}

SEXP R_remove_from_method_metadata(SEXP name)
{
    /* rather than actually delete from the list in the hash table, we
       assign NULL (the value returned by get_from_method_metadata when
       a name is not found).  The use of this remove is most typically
       to uncache a definition, which is then likely to be re-assigned
       when needed. */
    check_methods_table("remove");
    if(R_get_from_method_metadata(name) != R_NilValue)
	R_assign_to_method_metadata(name, R_NilValue);
    return name;
}
