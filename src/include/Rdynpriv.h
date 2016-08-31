/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2016  The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_DYNPRIV_H
#define R_DYNPRIV_H

/*****************************************************
 These are internal routines and definitions subject
 to unannounced changes. Do not use for packages, etc.
 (The header is not installed.)
 
 There is a great deal of repetition in the definitions 
 of the user-level method definitions and in the internal
 definition structures. This is done to ensure that we
 don't get into troubles needing different types, etc.
 We could do it with typedef's and reduce the code, but it 
 is done now and isn't too complicated yet.
*****************************************************/


#ifdef Win32
#include <windows.h>
#define CACHE_DLL_SYM 1
#else
typedef void *HINSTANCE;
#endif


#include <Defn.h>
#include <R_ext/Rdynload.h>
int R_moduleCdynload(const char *module, int local, int now);

  /*
     A name-routine pair.
   */
typedef struct {
    char *name;
    DL_FUNC func;
} CFunTabEntry;

  /*
     These three structures are the processed, internal information about
     native routines that can be called by R. They are intended to be 
     instantiated by packages that explicitly register the routines in the
     library.

     More fields will be added to these "real soon now". These may contain
     information such as 
        a) whether the routine is thread-safe or not,
        b) with which other routines it must be sychronized,
        c) the parameter types,
        ...
   */

typedef struct {
    char       *name;
    DL_FUNC     fun;
    int         numArgs;

    R_NativePrimitiveArgType *types;
    R_NativeArgStyle *styles;
   
} Rf_DotCSymbol;

typedef Rf_DotCSymbol Rf_DotFortranSymbol;


typedef struct {
    char       *name;
    DL_FUNC     fun;
    int         numArgs;
    R_NativeObjectArgType *types;

    R_NativeArgStyle *styles;
} Rf_DotCallSymbol;

typedef Rf_DotCallSymbol Rf_DotExternalSymbol;



  /*
      This structure holds the information about a library that is 
      loaded into R and whose symbols are directly accessible to
      .C, .Call, .Fortran, .External, ...
      This stores the short name of the library (with the path and extension 
      removed), and its fully  qualified name including the path and extension.
      Additionally, it can potentially be populated with information about
      the native routines in that library that are callable by R.
   */
struct _DllInfo {
    char  *path;
    char  *name;
    HINSTANCE handle;
    Rboolean useDynamicLookup; /* Flag indicating whether we use both
				  registered and dynamic lookup (TRUE)
				  or just registered values if there
				  are any. */
    int numCSymbols;
    Rf_DotCSymbol *CSymbols;

    int numCallSymbols;
    Rf_DotCallSymbol *CallSymbols;

    int numFortranSymbols;
    Rf_DotFortranSymbol *FortranSymbols;

    int numExternalSymbols;
    Rf_DotExternalSymbol *ExternalSymbols;

    Rboolean forceSymbols;
};


struct Rf_RegisteredNativeSymbol {
    NativeSymbolType type;
    union {
	Rf_DotCSymbol        *c;
	Rf_DotCallSymbol     *call;
	Rf_DotFortranSymbol  *fortran;
	Rf_DotExternalSymbol *external;
    } symbol;
    DllInfo *dll;
};


  /* 
     An abstraction of the system-specific hooks that can be implemented
     to customize the dynamic loading for a particular operating system
     or application.
     The function pointers implement 
        the opening and closing of the libraries,
        the resolution of symbol, 
        returning error messages from system-level failures, 
        finding symbols in R itself,
        handling the cached symbols,
        processing the library path. 
   */
typedef struct {
    HINSTANCE (*loadLibrary)(const char *path, int asLocal, int now,
			     char const *search); 
    /* Load the dynamic library. */
    DL_FUNC (*dlsym)(DllInfo *info, char const *name); 
    /* Low-level symbol lookup in library */
    void (*closeLibrary)(HINSTANCE handle); 
    /* Unload the dynamic library from process. */
    void (*getError)(char *buf, int len); 
    /* Put the current system error in DLLerror. */


    void (*deleteCachedSymbols)(DllInfo *dll);  /* Discard cached symbols */
    DL_FUNC (*lookupCachedSymbol)(const char *name, const char *pkg, int all);

    void  (*fixPath)(char *path);
    void  (*getFullDLLPath)(SEXP call, char *buf, const char * const path);

} OSDynSymbol;

extern OSDynSymbol Rf_osDynSymbol, *R_osDynSymbol;


#ifdef CACHE_DLL_SYM
  /* 
     The collection of cached symbol holders which are used to make the lookup
     more efficient. The most recently resolved symbols are stored in this 
     pool if CACHE_DLL_SYM is defined and repeated lookups check here first,
     before using the dynamic loader's lookup mechanism.
   */
typedef struct {
    char pkg[21];
    char name[41];
    DL_FUNC func;
} R_CPFun;

extern R_CPFun CPFun[];
extern int nCPFun;

#endif /* CACHE_DLL_SYM */


DL_FUNC Rf_lookupCachedSymbol(const char *name, const char *pkg, int all);

DL_FUNC R_dlsym(DllInfo *info, char const *name, 
		R_RegisteredNativeSymbol *symbol);

/* Moved to API in R 3.4.0
  SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);
  DL_FUNC R_ExternalPtrAddrFn(SEXP s);
*/
DL_FUNC R_dotCallFn(SEXP, SEXP, int);
SEXP R_doDotCall(DL_FUNC, int, SEXP *, SEXP);

#endif /* ifdef R_DYNPRIV_H */
