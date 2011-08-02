/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Development Core Team.
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

#include "parallel.h"
#include <R_ext/Rdynload.h>

static const R_CallMethodDef callMethods[] = {
    {"nextStream", (DL_FUNC) &nextStream, 1},
    {"nextSubStream", (DL_FUNC) &nextSubStream, 1},
#ifndef WIN32
    {"mc_fork", (DL_FUNC) &mc_fork, 0},
    {"mc_send_master", (DL_FUNC) &mc_send_master, 1},
    {"mc_select_children", (DL_FUNC) &mc_select_children, 2},
    {"mc_read_child", (DL_FUNC) &mc_read_child, 1},
    {"mc_exit", (DL_FUNC) &mc_exit, 1},
#endif
    {NULL, NULL, 0}
};

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_parallel(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
