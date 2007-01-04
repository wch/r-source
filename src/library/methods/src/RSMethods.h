#ifndef R_RSMETHODS_H
#define R_RSMETHODS_H

SEXP R_initialize_methods_metadata(SEXP table);
SEXP R_get_from_method_metadata(SEXP name);
SEXP R_assign_to_method_metadata(SEXP name, SEXP value);

SEXP R_methods_list_dispatch(SEXP fname, SEXP ev, SEXP must_find);

SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef);
SEXP R_dispatchGeneric(SEXP fname, SEXP ev, SEXP fdef);
SEXP R_quick_dispatch(SEXP args, SEXP mtable, SEXP fdef);

#endif   /* R_RSMETHODS_H */
