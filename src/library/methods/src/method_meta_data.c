
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "RSMethods.h"

/* taken or inferred from main/envir.c */
#define HASHTABLESIZE 100
#define HASHTABLEGROWTHRATE 1.2

/* from Defn.h */
#define streql(s, t)	(!strcmp((s), (t)))
#define setVarInFrame		Rf_setVarInFrame
SEXP setVarInFrame(SEXP, SEXP, SEXP);

SEXP R_NewHashTable(int size, int growth_rate);

/* These tables should not be "global".  They will need to be specific
   to thread and to namespace.  Remains to design a correct but
   efficient way to select the correct table */
static SEXP methods_table;

SEXP R_initialize_methods_metadata(SEXP table) {
  /* the table argument is assumed assigned somewhere (usually by
     First.lib in the RSMethods package) to protect it from garbage
     collection */
  methods_table = table;
  /* enforce hashing of entries by initializing the hash table */
  if(HASHTAB(table) == R_NilValue)
    SET_HASHTAB(table,  R_NewHashTable(HASHTABLESIZE, HASHTABLEGROWTHRATE));
  return table;
}

SEXP R_get_from_method_metadata(SEXP name) {
  SEXP value;
  if(!isSymbol(name))
    name = install(CHAR(asChar(name)));
  value = findVarInFrame(methods_table, name);
  if(value == R_UnboundValue)
    value = R_NilValue;
  return value;
}

SEXP R_assign_to_method_metadata(SEXP name, SEXP value) {
  if(!isSymbol(name))
    name = install(CHAR(asChar(name)));
   defineVar(name, value, methods_table);
   return(name);
}

SEXP R_remove_from_method_metadata(SEXP name) {
  /* rather than actually delete from the list in the hash table, we
     assign NULL (the value returned by get_from_method_metadata when
     a name is not found).  The use of this remove is most typically
     to uncache a definition, which is then likely to be re-assigned
     when needed. */
  if(R_get_from_method_metadata(name) != R_NilValue)
    R_assign_to_method_metadata(name, R_NilValue);
  return name;
}
