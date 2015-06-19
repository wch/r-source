/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000--2013  The R Core Team
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

/* Ouch! - do we *really* need this? */
#define _GNU_SOURCE

#include <tcl.h>
#include <stdio.h>
#include <string.h>

#include <Rinternals.h>
#include <R_ext/Parse.h>

#ifndef Win32
/* From tcltk_unix.c */
void Tcl_unix_setup(void);
#endif

/* Globals exported from  ./tcltk.c : */

Tcl_Interp *RTcl_interp;      /* Interpreter for this application. */
void tcltk_init(int *);

SEXP dotTcl(SEXP args);
SEXP dotTclObjv(SEXP args);
SEXP dotTclcallback(SEXP args);

/* Used by .C */

#ifdef Win32
void tcltk_start(void);
void tcltk_end(void);
#else
void delTcl(void);
void RTcl_ActivateConsole(void);
#endif

/* Used by .Extern */

SEXP RTcl_ObjFromVar(SEXP args);
SEXP RTcl_AssignObjToVar(SEXP args);
SEXP RTcl_StringFromObj(SEXP args);
SEXP RTcl_ObjAsCharVector(SEXP args);
SEXP RTcl_ObjAsDoubleVector(SEXP args);
SEXP RTcl_ObjAsIntVector(SEXP args);
SEXP RTcl_ObjAsRawVector(SEXP args);
SEXP RTcl_ObjFromCharVector(SEXP args);
SEXP RTcl_ObjFromDoubleVector(SEXP args);
SEXP RTcl_ObjFromIntVector(SEXP args);
SEXP RTcl_ObjFromRawVector(SEXP args);
SEXP RTcl_GetArrayElem(SEXP args);
SEXP RTcl_SetArrayElem(SEXP args);
SEXP RTcl_RemoveArrayElem(SEXP args);
SEXP RTcl_ServiceMode(SEXP args);
