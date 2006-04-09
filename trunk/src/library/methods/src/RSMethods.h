#ifndef R_RSMETHODS_H
#define R_RSMETHODS_H

SEXP R_initialize_methods_metadata(SEXP table);
SEXP R_get_from_method_metadata(SEXP name);
SEXP R_assign_to_method_metadata(SEXP name, SEXP value);

SEXP R_methods_list_dispatch(SEXP fname, SEXP ev, SEXP must_find);

SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef);

/* the conditional on here should be fixed by adding to asChar's
   switch */
#define CHAR_STAR(obj) (CHAR(TYPEOF(obj) == SYMSXP ? PRINTNAME(obj) : asChar(obj)))

#endif   /* R_RSMETHODS_H */
