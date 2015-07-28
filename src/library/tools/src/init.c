/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-12   The R Core Team.
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

#ifdef UNUSED
/* a test for re-encoding */
void Renctest(char **x)
{
    Rprintf("'%s', nbytes = %d\n", x[0], strlen(x[0]));
}

static const R_CMethodDef CEntries[]  = {
    {"Renctest", (DL_FUNC) &Renctest, 1},
    {NULL, NULL, 0}
};
#endif

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(codeFilesAppend, 2),
    CALLDEF(delim_match, 2),
    CALLDEF(dirchmod, 2),
    {"C_getfmts", (DL_FUNC) &getfmts, 1},
    CALLDEF(Rmd5, 1),
    CALLDEF(check_nonASCII, 2),
    CALLDEF(check_nonASCII2, 1),
    CALLDEF(doTabExpand, 2),
    CALLDEF(ps_kill, 2),
    CALLDEF(ps_sigs, 1),
    CALLDEF(ps_priority, 2),
    CALLDEF(startHTTPD, 2),
    CALLDEF(stopHTTPD, 0),
    CALLDEF(C_deparseRd, 2),
    CALLDEF(splitString, 2),

    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}
static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(C_parseLatex, 4),
    EXTDEF(C_parseRd, 9),

    {NULL, NULL, 0}
};


void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_tools(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
}

