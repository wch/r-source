/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2012 The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rdynpriv.h>
#include <Rmodules/Rlapack.h>

static R_LapackRoutines *ptr;


static int initialized = 0;

static void La_Init(void)
{
    int res = R_moduleCdynload("lapack", 1, 1); // -> ../modules/lapack/Lapack.c
    initialized = -1;
    if(!res) return;
    if(!ptr->do_lapack)
	error(_("LAPACK routines cannot be accessed in module"));
    initialized = 1;
    return;
}


SEXP attribute_hidden
do_lapack(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if(!initialized) La_Init();
    if(initialized > 0)
	return (*ptr->do_lapack)(call, op, args, env);
    else {
	error(_("LAPACK routines cannot be loaded"));
	return R_NilValue;
    }
}


R_LapackRoutines *
R_setLapackRoutines(R_LapackRoutines *routines)
{
    R_LapackRoutines *tmp;
    tmp = ptr;
    ptr = routines;
    return(tmp);
}
