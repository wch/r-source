/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2012	The R Core Team.
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
 *
 *  This header contains the declarations of code to be used by
 *  .C, .Fortran, .Call or .External within the base package.
 *  These routines are `registered' in registration.c.
 */

void Rsockconnect(int *, char **);
void Rsockopen(int *);
void Rsocklisten(int *, char **, int *);
void Rsockclose(int *);
void Rsockread(int *, char **, int *);
void Rsockwrite(int *, char **, int *, int *, int *);

SEXP R_getTaskCallbackNames(void);
SEXP R_removeTaskCallback(SEXP);
SEXP R_addTaskCallback(SEXP, SEXP, SEXP, SEXP);

SEXP R_traceOnOff(SEXP);
SEXP R_setS4Object(SEXP, SEXP);

void F77_SYMBOL(dchdc)(double *, int *, int *, double *, int *, int *, int *);
void F77_SYMBOL(dpbfa)(double *, int *, int *, int *, int *);
void F77_SYMBOL(dpbsl)(double *, int *, int *, int *, double *);

SEXP R_getbcprofcounts(void);
SEXP R_startbcprof(void);
SEXP R_stopbcprof(void);
