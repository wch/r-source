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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifndef  R_EXT_DYNLOAD_H_
#define  R_EXT_DYNLOAD_H_

#include <R_ext/Boolean.h>

typedef void * (*DL_FUNC)();

typedef unsigned int R_NativePrimitiveArgType;

#define SINGLESXP 302 /* Don't have a single type for this. */

/* In the future, we will want to allow people register their own types
   and then refer to these in other contexts. Something like the Gtk type 
   system may be appropriate.
*/
typedef unsigned int R_NativeObjectArgType;


/* In the near future, we may support registering 
   information about the arguments of native routines 
   and whether they are used to return information.
   The hope is that we can minimize copying objects even 
   further. Not currently in use.
*/
typedef enum {R_ARG_IN, R_ARG_OUT, R_ARG_IN_OUT, R_IRRELEVANT} R_NativeArgStyle;



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
  
    R_NativePrimitiveArgType *types;
    R_NativeArgStyle         *styles; 
    
} R_CMethodDef;

typedef R_CMethodDef R_FortranMethodDef;



typedef struct {
    const char *name;
    DL_FUNC     fun;
    int         numArgs;
/* In the future, we will put types in here for the different arguments.
   We need a richer type system to do this effectively so that one
   can specify types for new classes.
*/
} R_CallMethodDef;
typedef R_CallMethodDef R_ExternalMethodDef;


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
