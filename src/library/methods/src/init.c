/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-12   The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <Rinternals.h>

#include "methods.h"
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(R_M_setPrimitiveMethods, 5),
    CALLDEF(R_clear_method_selection, 0),
    CALLDEF(R_dummy_extern_place, 0),
    CALLDEF(R_el_named, 2),
    CALLDEF(R_externalptr_prototype_object, 0),
    CALLDEF(R_getClassFromCache, 2),
    CALLDEF(R_getGeneric, 4),
    CALLDEF(R_get_slot, 2),
    CALLDEF(R_hasSlot, 2),
    CALLDEF(R_identC, 2),
    CALLDEF(R_initMethodDispatch, 1),
    CALLDEF(R_methodsPackageMetaName, 3),
    CALLDEF(R_methods_test_MAKE_CLASS, 1),
    CALLDEF(R_methods_test_NEW, 1),
    CALLDEF(R_missingArg, 2),
    CALLDEF(R_nextMethodCall, 2),
    CALLDEF(R_quick_method_check, 3),
    CALLDEF(R_selectMethod, 4),
    CALLDEF(R_set_el_named, 3),
    CALLDEF(R_set_slot, 3),
    CALLDEF(R_standardGeneric, 3),
    CALLDEF(do_substitute_direct, 2),
    CALLDEF(Rf_allocS4Object, 0),
    CALLDEF(R_set_method_dispatch, 1),
    CALLDEF(R_get_primname, 1),
    CALLDEF(new_object, 1),
    {NULL, NULL, 0}
};

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_methods(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
//    R_forceSymbols(dll, TRUE);
}
