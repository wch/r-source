/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-11   The R Core Team.
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
#include "tools.h"
#include <R_ext/Rdynload.h>

/* a test for re-encoding */
void Renctest(char **x)
{
    Rprintf("'%s', nbytes = %d\n", x[0], strlen(x[0]));
}

static const R_CallMethodDef callMethods[] = {
    {"delim_match", (DL_FUNC) &delim_match, 2},
    {"Rmd5", (DL_FUNC) &Rmd5, 1},
    {"check_nonASCII", (DL_FUNC) &check_nonASCII, 2},
    {"check_nonASCII2", (DL_FUNC) &check_nonASCII2, 1},
    {"doTabExpand", (DL_FUNC) &doTabExpand, 2},
    {"ps_kill", (DL_FUNC) &ps_kill, 2},
    {"ps_sigs", (DL_FUNC) &ps_sigs, 1},
    {"ps_priority", (DL_FUNC) &ps_priority, 2},
    {NULL, NULL, 0}
};

static const R_CMethodDef CEntries[]  = {
    {"Renctest", (DL_FUNC) &Renctest, 1},
    {NULL, NULL, 0}
};


void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_tools(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

