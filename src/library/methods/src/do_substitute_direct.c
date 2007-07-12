/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2005   The R Development Core Team.
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

#include <R.h>
#include <Rdefines.h>
#define NewEnvironment		Rf_NewEnvironment
#define substitute		Rf_substitute
#include "methods.h"

extern SEXP NewEnvironment(SEXP namelist, SEXP valuelist, SEXP rho);

/* substitute in an _evaluated_ object, with an explicit list as
   second arg (although old-style lists and environments are allowed).

*/

SEXP do_substitute_direct(SEXP f, SEXP env)
{
    SEXP s;
    if (TYPEOF(env) == VECSXP)
	env = NewEnvironment(R_NilValue, VectorToPairList(env), R_BaseEnv);
    else if (TYPEOF(env) == LISTSXP)
	env = NewEnvironment(R_NilValue, duplicate(env), R_BaseEnv);
    if(TYPEOF(env) != ENVSXP)
	error(_("invalid list for substitution"));
    PROTECT(env);
    PROTECT(f);
    s = substitute(f, env);
    UNPROTECT(2);
    return(s);
}

