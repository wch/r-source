/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-4     the R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h> /* for checkArity */
#define R_INTERFACE_PTRS 1
#include <Rinterface.h>


SEXP attribute_hidden do_loadhistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    ptr_R_loadhistory(call, op, args, rho);
    return R_NilValue;
}

SEXP attribute_hidden do_savehistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    ptr_R_savehistory(call, op, args, rho);
    return R_NilValue;
}

SEXP attribute_hidden do_addhistory(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if(ptr_R_addhistory) ptr_R_addhistory(call, op, args, rho);
    return R_NilValue;
}
