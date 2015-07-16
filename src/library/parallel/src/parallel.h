/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Core Team.
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

#ifndef R_PARALLEL_H
#define R_PARALLEL_H

#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("parallel", String)
#else
#define _(String) (String)
#endif

SEXP nextStream(SEXP);
SEXP nextSubStream(SEXP);

#ifndef WIN32
SEXP mc_children(void);
SEXP mc_close_fds(SEXP);
SEXP mc_close_stderr(SEXP);
SEXP mc_close_stdout(SEXP);
SEXP mc_create_list(SEXP);
SEXP mc_exit(SEXP);
SEXP mc_fds(SEXP);
SEXP mc_fork(SEXP);
SEXP mc_is_child(void);
SEXP mc_kill(SEXP, SEXP);
SEXP mc_master_fd(void);
SEXP mc_read_child(SEXP);
SEXP mc_read_children(SEXP);
SEXP mc_rm_child(SEXP);
SEXP mc_send_master(SEXP);
SEXP mc_select_children(SEXP, SEXP);
SEXP mc_send_child_stdin(SEXP, SEXP);
SEXP mc_affinity(SEXP);
SEXP mc_interactive(SEXP);
#else
SEXP ncpus(SEXP);
#endif

#endif
