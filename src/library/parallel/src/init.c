/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011-2017   The R Core Team.
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

#include <R.h>
#include "parallel.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef callMethods[] = {
    CALLDEF(nextStream, 1),
    CALLDEF(nextSubStream, 1),
#ifndef _WIN32
    CALLDEF(mc_children, 0),
    CALLDEF(mc_close_fds, 1),
    CALLDEF(mc_close_stderr, 1),
    CALLDEF(mc_close_stdout, 1),
    CALLDEF(mc_exit, 1),
    CALLDEF(mc_fds, 1),
    CALLDEF(mc_fork, 1),
    CALLDEF(mc_is_child, 0),
    CALLDEF(mc_kill, 2),
    CALLDEF(mc_master_fd, 0),
    CALLDEF(mc_read_child, 1),
    CALLDEF(mc_read_children, 1),
    CALLDEF(mc_rm_child, 1),
    CALLDEF(mc_send_master, 1),
    CALLDEF(mc_select_children, 2),
    CALLDEF(mc_send_child_stdin, 2),
    CALLDEF(mc_affinity, 1),
    CALLDEF(mc_interactive, 1),
    CALLDEF(mc_cleanup, 3),
    CALLDEF(mc_prepare_cleanup, 0),
#else
    CALLDEF(ncpus, 1),
#endif
    {NULL, NULL, 0}
};

void attribute_visible
R_init_parallel(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, FALSE);
}
