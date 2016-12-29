/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2016 The R Core Team.
 *
 *  This header file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This file is part of R. R is distributed under the terms of the
 *  GNU General Public License, either Version 2, June 1991 or Version 3,
 *  June 2007. See doc/COPYRIGHTS for details of the copyright status of R.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_PRIVATE_INTERNALS_H_
#define R_PRIVATE_INTERNALS_H_

#include <Rinternals.h>

SEXP Rf_xlengthgets(SEXP, R_xlen_t);
SEXP Rf_matchE(SEXP, SEXP, int, SEXP);

#ifndef R_NO_REMAP
# define matchE			Rf_matchE
# define xlengthgets		Rf_xlengthgets
#endif

#define SET_RTRACE(x,v)	(((x)->sxpinfo.trace)=(v))
void (SET_RTRACE)(SEXP x, int v);

#define SYMVALUE(x)	((x)->u.symsxp.value)
SEXP (SYMVALUE)(SEXP x);

#define RTRACE(x)	((x)->sxpinfo.trace)
int  (RTRACE)(SEXP x);

#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
int  (DDVAL)(SEXP x);

SEXP R_tryEvalSilent(SEXP, SEXP, int *);

void SHALLOW_DUPLICATE_ATTRIB(SEXP to, SEXP from);

void SET_PRVALUE(SEXP x, SEXP v);

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);

#endif /* !R_RPRIVATE_H */