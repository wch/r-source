/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001  The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef  R_EXT_DYNLOAD_H_
#define  R_EXT_DYNLOAD_H_

#include "R_ext/Boolean.h"

typedef void * (*DL_FUNC)();

/* 
 These are very similar to those in  unix/dynload.c
 but we maintain them separately to give us more freedom to do
 some computations on the internal versions that are derived from
 these definitions.
*/
typedef struct {
    const char *name;
    DL_FUNC     fun;
    int         numArgs;
 
} R_CMethodDef;

typedef struct {
    const char *name;
    DL_FUNC     fun;
    int         numArgs;
 
} R_FortranMethodDef;

typedef struct {
    const char *name;
    DL_FUNC     fun;
    int         numArgs;
} R_CallMethodDef;

typedef struct {
    const char *name;
    DL_FUNC     fun;
    int         numArgs;
} R_ExternalMethodDef;


typedef struct _DllInfo DllInfo;

/* 
  Currently ignore the graphics routines, accessible via .External.graphics()
  and .Call.graphics().
 */
int R_registerRoutines(DllInfo *info, const R_CMethodDef * const croutines,
		       const R_CallMethodDef * const callRoutines, 
		       const R_FortranMethodDef * const fortranRoutines,
                       const R_ExternalMethodDef * const externalRoutines);

DllInfo *R_getDllInfo(const char *name);


typedef struct Rf_RegisteredNativeSymbol R_RegisteredNativeSymbol;
typedef enum {R_ANY_SYM=0, R_C_SYM, R_CALL_SYM, R_FORTRAN_SYM, R_EXTERNAL_SYM} NativeSymbolType;


DL_FUNC R_FindSymbol(char const *, char const *, 
                       R_RegisteredNativeSymbol *symbol);

int moduleCdynload(char *module, int local, int now);

Rboolean R_useDynamicSymbols(DllInfo *info, Rboolean value);

#endif /* R_EXT_DYNLOAD_H_ */
