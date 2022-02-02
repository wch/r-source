/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2007   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

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
