/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("methods", String)
#else
#define _(String) (String)
#endif


SEXP R_M_setPrimitiveMethods(SEXP fname, SEXP op, SEXP code_vec,
			     SEXP fundef, SEXP mlist);
SEXP R_clear_method_selection();
SEXP R_dummy_extern_place();
SEXP R_el_named(SEXP object, SEXP what);
SEXP R_externalptr_prototype_object();
SEXP R_getGeneric(SEXP name, SEXP mustFind, SEXP env);
SEXP R_get_slot(SEXP obj, SEXP name);
SEXP R_identC(SEXP e1, SEXP e2);
SEXP R_initMethodDispatch(SEXP envir);
SEXP R_methodsPackageMetaName(SEXP prefix, SEXP name);
SEXP R_methods_test_MAKE_CLASS(SEXP className);
SEXP R_methods_test_NEW(SEXP className);
SEXP R_missingArg(SEXP symbol, SEXP ev);
SEXP R_nextMethodCall(SEXP matched_call, SEXP ev);
SEXP R_quick_method_check(SEXP args, SEXP mlist, SEXP fdef);
SEXP R_selectMethod(SEXP fname, SEXP ev, SEXP mlist, SEXP evalArgs);
SEXP R_set_el_named(SEXP object, SEXP what, SEXP value);
SEXP R_set_slot(SEXP obj, SEXP name, SEXP value);
SEXP R_standardGeneric(SEXP fname, SEXP ev, SEXP fdef);
SEXP do_substitute_direct(SEXP f, SEXP env);
