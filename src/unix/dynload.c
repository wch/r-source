/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2016 The R Core Team
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

/* <UTF8> char here is handled as a whole string */


/* This provides a table of built-in C and Fortran functions.
   We include this table, even when we have dlopen and friends.
   This is so that the functions are actually loaded at link time. */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rdynpriv.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* HP-UX 11.0 has dlfcn.h, but according to libtool as of Dec 2001
   this support is broken. So we force use of shlib even when dlfcn.h
   is available */
# ifdef __hpux
#  ifdef HAVE_DL_H
#   include "hpdlfcn.c"
#   define HAVE_DYNAMIC_LOADING
#  endif
# else
#  ifdef HAVE_DLFCN_H
#   include <dlfcn.h>
#   define HAVE_DYNAMIC_LOADING
#  endif
# endif

#ifdef HAVE_DYNAMIC_LOADING

static void *loadLibrary(const char *path, int asLocal, int now,
			 const char *search);
static void closeLibrary(void *handle);
static void deleteCachedSymbols(DllInfo *);
static DL_FUNC R_local_dlsym(DllInfo *info, char const *name);
static void getFullDLLPath(SEXP call, char *buf, const char *path);
static void getSystemError(char *buf, int len);

static int computeDLOpenFlag(int asLocal, int now);

void attribute_hidden InitFunctionHashing()
{
    R_osDynSymbol->loadLibrary = loadLibrary;
    R_osDynSymbol->dlsym = R_local_dlsym;
    R_osDynSymbol->closeLibrary = closeLibrary;
    R_osDynSymbol->getError = getSystemError;

    R_osDynSymbol->deleteCachedSymbols = deleteCachedSymbols;
    R_osDynSymbol->lookupCachedSymbol = Rf_lookupCachedSymbol;

    R_osDynSymbol->getFullDLLPath = getFullDLLPath;
}

static void getSystemError(char *buf, int len)
{
    strcpy(buf, dlerror());
}

static void *loadLibrary(const char *path, int asLocal, int now,
			 const char *search)
{
    void *handle;
    int openFlag = 0;

    openFlag = computeDLOpenFlag(asLocal, now);
    handle = (void *) dlopen(path,openFlag);

    return(handle);
}

static void closeLibrary(HINSTANCE handle)
{
    dlclose(handle);
}

 /*
   If we are caching the native level symbols, this routine
   discards the ones from the DLL identified by loc.
   This is called as the initial action of DeleteDLL().
  */
static void deleteCachedSymbols(DllInfo *dll)
{
#ifdef CACHE_DLL_SYM
    int i;
    /* Wouldn't a linked list be easier here?
       Potentially ruin the contiguity of the memory.
    */
    for(i = nCPFun - 1; i >= 0; i--)
	if(!strcmp(CPFun[i].pkg, dll->name)) {
	    if(i < nCPFun - 1) {
		strcpy(CPFun[i].name, CPFun[--nCPFun].name);
		strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
		CPFun[i].func = CPFun[nCPFun].func;
	    } else nCPFun--;
	}
#endif /* CACHE_DLL_SYM */
}


 /*
    Computes the flag to be passed as the second argument to dlopen(),
    controlling whether the local or global symbol integration
    and lazy or eager resolution of the undefined symbols.
    The arguments determine which of each of these possibilities
    to use and the results are or'ed together. We need a separate
    routine to keep things clean(er) because some symbolic constants
    may not  be defined, such as RTLD_LOCAL on certain Solaris 2.5.1
    and Irix 6.4    boxes. In such cases, we emit a warning message and
    use the default by not modifying the value of the flag.

    Called only by AddDLL().
  */
static int computeDLOpenFlag(int asLocal, int now)
{
#if !defined(RTLD_LOCAL) || !defined(RTLD_GLOBAL) || !defined(RTLD_NOW) || !defined(RTLD_LAZY)
    static char *warningMessages[] = {
	N_("Explicit local dynamic loading not supported on this platform. Using default."),
	N_("Explicit global dynamic loading not supported on this platform. Using default."),
	N_("Explicit non-lazy dynamic loading not supported on this platform. Using default."),
	N_("Explicit lazy dynamic loading not supported on this platform. Using default.")
    };
    /* Define a local macro for issuing the warnings.
       This allows us to redefine it easily so that it only emits the
       warning once as in
       DL_WARN(i) if(warningMessages[i]) {\
       warning(warningMessages[i]); \
       warningMessages[i] = NULL; \
       }
       or to control the emission via the options currently in effect at
       call time.
    */
# define DL_WARN(i) \
    if(asInteger(GetOption1(install("warn"))) == 1 || \
       asInteger(GetOption1(install("verbose"))) > 0) \
	warning(_(warningMessages[i]))
#endif

    int openFlag = 0;		/* Default value so no-ops for undefined
				   flags should do nothing in the
				   resulting dlopen(). */

    if(asLocal != 0) {
#ifndef RTLD_LOCAL
	DL_WARN(0);
#else
	openFlag = RTLD_LOCAL;
#endif
    } else {
#ifndef RTLD_GLOBAL
	DL_WARN(1);
#else
	openFlag = RTLD_GLOBAL;
#endif
    }

    if(now != 0) {
#ifndef RTLD_NOW
	DL_WARN(2);
#else
	openFlag |= RTLD_NOW;
#endif
    } else {
#ifndef RTLD_LAZY
	DL_WARN(3);
#else
	openFlag |= RTLD_LAZY;
#endif
    }

    return(openFlag);
}


/*
  This is the system/OS-specific version for resolving a
  symbol in a shared object.  A cast would not be legal C.
 */
/*
  We are not interested in NULL symbols in the shared object.
  If we were, this would need to use dlerror() before and after
  dlsym, and check the second value is NULL.
 */
typedef union {void *p; DL_FUNC fn;} fn_ptr;
static DL_FUNC R_local_dlsym(DllInfo *info, char const *name)
{
    fn_ptr tmp;
    tmp.p = dlsym(info->handle, name);
    return tmp.fn;
}


/*
  In the future, this will receive an additional argument
  which will specify the nature of the symbol expected by the
  caller, specifically whether it is for a .C(), .Call(),
  .Fortran(), .External(), generic, etc. invocation. This will
  reduce the pool of possible symbols in the case of a library
  that registers its routines.
 */



static void getFullDLLPath(SEXP call, char *buf, const char *path)
{
    if(path[0] == '~')
	strcpy(buf, R_ExpandFileName(path));
    else if(path[0] != '/') {
#ifdef HAVE_GETCWD
	if(!getcwd(buf, PATH_MAX))
#endif
	    errorcall(call, _("cannot get working directory!"));
	strcat(buf, "/");
	strcat(buf, path);
    }
    else strcpy(buf, path);
}

#endif /* end of `ifdef HAVE_DYNAMIC_LOADING' */
