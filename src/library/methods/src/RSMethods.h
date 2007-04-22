/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2007   The R Development Core Team.
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
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
