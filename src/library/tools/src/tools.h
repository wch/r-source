/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2019   The R Core Team.
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

#ifndef R_TOOLS_H
#define R_TOOLS_H

#include <R.h>
#include <Rinternals.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

SEXP delim_match(SEXP x, SEXP delims);
SEXP dirchmod(SEXP dr, SEXP gwsxp);
SEXP Rmd5(SEXP files);
SEXP check_nonASCII(SEXP text, SEXP ignore_quotes);
SEXP check_nonASCII2(SEXP text);
SEXP doTabExpand(SEXP strings, SEXP starts);
SEXP ps_kill(SEXP pid, SEXP signal);
SEXP ps_sigs(SEXP);
SEXP ps_priority(SEXP pid, SEXP value);
SEXP codeFilesAppend(SEXP f1, SEXP f2);
SEXP getfmts(SEXP format);
SEXP startHTTPD(SEXP sIP, SEXP sPort);
SEXP stopHTTPD(void);
SEXP splitString(SEXP string, SEXP delims);

SEXP parseLatex(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP parseRd(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP deparseRd(SEXP e, SEXP state);

#endif
