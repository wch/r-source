/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-1999 The R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*  Dynamic Loading Support
 *
 *  This module provides support for run-time loading of shared libraries
 *  access to symbols within such libraries via .C and .Fortran.  This is
 *  done under Unix with dlopen, dlclose and dlsym (the exception is
 *  hpux, where we use compatibility code provided by Luke Tierney.
 *  There are two cases:
 *
 *
 *  1. The dlopen interface is available.
 *
 *  In this case all symbol location is done using the dlopen routines.
 *  We maintain a list of currently loaded shared libraries in an array
 *  called "LoadedDLL" with the number of currenly loaded libraries
 *  being "CountDLL".  To locate a symbol, we probe the loaded libraries
 *  in order until the symbol is located.  If we do not find a symbol
 *  in the loaded libraries, we search the executable itself.  This
 *  search is not very efficient, but this probably pales into
 *  insignificance when compared with the inefficiencies in the R
 *  interpreter.
 *
 *  Loading and unloading of shared libraries is done via the routines
 *  AddDLL and DeleteDLL.  These routines maintain the list of currently
 *  loaded libraries.  When a library is added, any existing reference
 *  to that library are deleted and then the library is inserted at the
 *  start of the search list.  This way, symbols in more recently loaded
 *  libraries are found first.
 *
 *
 *  2. The dlopen interface is not available.
 *
 *  In this case we use the table "CFunTabEntry" to locate functions
 *  in the executable.	We do this by straight linear search through
 *  the table.	Note that the content of the table is created at
 *  system build time from the list in ../appl/ROUTINES.
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "Defn.h"
#include "Mathlib.h"

typedef int (*DL_FUNC)();
typedef struct {
    char *name;
    DL_FUNC func;
} CFunTabEntry;

#include "FFDecl.h"

/* This provides a table of built-in C and Fortran functions.
   We include this table, even when we have dlopen and friends.
   This is so that the functions are actually loaded at link time. */

static CFunTabEntry CFunTab[] =
{
#include "FFTab.h"
    {NULL, NULL}
};

/* The following code loads in a compatibility module written by Luke
   Tierney to support S version 4 on Hewlett-Packard machines.	The
   relevant defines are set up by autoconf. */

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#ifdef HAVE_DL_H
#include "hpdlfcn.c"
#define HAVE_DLFCN_H
#endif
#endif

#ifdef HAVE_DLFCN_H
#ifndef RTLD_LAZY
#define RTLD_LAZY 1
#endif
#ifndef RTLD_NOW
#define RTLD_NOW  2
#endif

#ifdef DL_SEARCH_PROG
static void *dlhandle;
#endif

void InitFunctionHashing()
{
#ifdef DL_SEARCH_PROG
    dlhandle = dlopen(0, RTLD_NOW);
#endif
}


#define MAX_NUM_DLLS	100

static int CountDLL = 0;

static struct {
    char	*path;
    char	*name;
    void	*handle;
}
LoadedDLL[MAX_NUM_DLLS];

	/* Remove the specified DLL from the current DLL list */
	/* Returns 1 if the DLL was found and removed from */
	/* the list and returns 0 otherwise. */

static int DeleteDLL(char *path)
{
    int i, loc;
    for(i=0 ; i<CountDLL ; i++) {
	if(!strcmp(path, LoadedDLL[i].path)) {
	    loc = i;
	    goto found;
	}
    }
    return 0;
found:
    free(LoadedDLL[i].name);
    free(LoadedDLL[i].path);
    dlclose(LoadedDLL[i].handle);
    for(i=loc+1 ; i<CountDLL ; i++) {
	LoadedDLL[i-1].path = LoadedDLL[i].path;
	LoadedDLL[i-1].name = LoadedDLL[i].name;
	LoadedDLL[i-1].handle = LoadedDLL[i].handle;
    }
    CountDLL--;
    return 1;
}

#define DLLerrBUFSIZE 1000
static char DLLerror[DLLerrBUFSIZE] = "";
/* the error message; length taken from ERRBUFSIZE in ./hpdlfcn.c  */

	/* Inserts the specified DLL at the start of the DLL list */
	/* All the other entries are "moved down" by one. */
	/* Returns 1 if the library was successfully added */
	/* and returns 0 if there library table is full or */
	/* or if dlopen fails for some reason. */

static int computeDLOpenFlag(int asLocal, int now); /* Defined below. */

static int AddDLL(char *path, int asLocal, int now)
{
    void *handle;
    char *dpath, *name, DLLname[PATH_MAX], *p;
    /*int i;*/
    int openFlag = 0;

    DeleteDLL(path);
    if(CountDLL == MAX_NUM_DLLS) {
	strcpy(DLLerror, "Maximal number of DLLs reached...");
	return 0;
    }

    openFlag = computeDLOpenFlag(asLocal, now);

    handle = dlopen(path,openFlag);
    if(handle == NULL) {
	strcpy(DLLerror, dlerror());
	return 0;
    }
    dpath = malloc(strlen(path)+1);
    if(dpath == NULL) {
	strcpy(DLLerror,"Couldn't allocate space for 'path'");
	dlclose(handle);
	return 0;
    }
    strcpy(dpath, path);

    p = strrchr(dpath, '/');  /* We are on Unix here */
    if(!p) p = dpath; else p++;
    strcpy(DLLname, p);
    p = strchr(DLLname, '.');
    if(p) *p = '\0';
    name = malloc(strlen(DLLname)+1);
    if(name == NULL) {
	strcpy(DLLerror,"Couldn't allocate space for 'name'");
	dlclose(handle);
	free(dpath);
	return 0;
    }
    strcpy(name, DLLname);

/*    for(i=CountDLL ; i>0 ; i--) {
	LoadedDLL[i].path = LoadedDLL[i-1].path;
	LoadedDLL[i].name = LoadedDLL[i-1].name;
	LoadedDLL[i].handle = LoadedDLL[i-1].handle;
    }
    LoadedDLL[0].path = dpath;
    LoadedDLL[0].name = name;
    LoadedDLL[0].handle = handle;*/

    LoadedDLL[CountDLL].path = dpath;
    LoadedDLL[CountDLL].name = name;
    LoadedDLL[CountDLL].handle = handle;
    CountDLL++;

    return 1;
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
static int
computeDLOpenFlag(int asLocal, int now)
{
 static char *warningMessages[] = {
  "Explicit local dynamic loading not supported on this platform. Using default.",
  "Explicit global dynamic loading not supported on this platform. Using default.",
  "Explicit non-lazy dynamic loading not supported on this platform. Using default.",
  "Explicit lazy dynamic loading not supported on this platform. Using default."
 };
  /* Define a local macro for issuing the warnings.
     This allows us to redefine it easily so that it only emits the warning
     once as in
         DL_WARN(i) if(warningMessages[i]) {\
                     warning(warningMessages[i]); \
                     warningMessages[i] = NULL; \
    	            }
     or to control the emission via the options currently in effect at call time.
   */
#define DL_WARN(i) \
   if(asInteger(GetOption(install("warn"),R_NilValue)) == 1 || \
         asInteger(GetOption(install("verbose"),R_NilValue)) > 0) \
                      warning(warningMessages[i]);

 int openFlag = 0; /* Default value so no-ops for undefined flags should do nothing
                      in the resulting dlopen(). */


#undef RTLD_LOCAL

if(asLocal != 0) {
#ifndef RTLD_LOCAL
  DL_WARN(0)
#else
  openFlag = RTLD_LOCAL;
#endif
} else {
#ifndef RTLD_GLOBAL
  DL_WARN(1)
#else
  openFlag = RTLD_GLOBAL;
#endif
}

if(now != 0) {
#ifndef RTLD_NOW
  DL_WARN(2)
#else
  openFlag |= RTLD_NOW;
#endif
} else {
#ifndef RTLD_LAZY
  DL_WARN(3)
#else
  openFlag |= RTLD_LAZY;
#endif
}

 return(openFlag);
}


	/* R_FindSymbol checks whether one of the libraries */
	/* that have been loaded contains the symbol name and */
	/* returns a pointer to that symbol upon success. */


DL_FUNC R_FindSymbol(char const *name, char const *pkg)
{
    char buf[MAXIDSIZE+1];
    DL_FUNC fcnptr;
    int i, all=(strlen(pkg) == 0), doit;
    
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    sprintf(buf, "%s", name);
#else
    sprintf(buf, "_%s", name);
#endif

	/* The following is not legal ANSI C. */
	/* It is only meant to be used in systems supporting */
	/* the dlopen() interface, in which systems data and  */
	/* function pointers _are_ the same size and _can_   */
	/* be cast without loss of information.		     */

    for (i=0 ; i<CountDLL ; i++) {
	doit = all;
	if(!doit && !strcmp(pkg, LoadedDLL[i].name)) doit = 2;
	if(doit) {
	   fcnptr = (DL_FUNC)dlsym(LoadedDLL[i].handle, buf);
	   if (fcnptr != (DL_FUNC)0) return fcnptr;
	}
	if(doit > 1) return (DL_FUNC)0;  /* Only look in the first-matching DLL */
    }
    if(all || !strcmp(pkg, "base")) {
#ifdef DL_SEARCH_PROG
	fcnptr = (DL_FUNC)dlsym(dlhandle, buf);
#else
	for(i=0 ; CFunTab[i].name ; i++)
	    if(!strcmp(name, CFunTab[i].name))
		return CFunTab[i].func;
#endif
    }
    return (DL_FUNC)0;
}


static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
    if(path[0] == '~')
	strcpy(buf, R_ExpandFileName(path));
    else if(path[0] != '/') {
#ifdef HAVE_UNISTD_H
	if(!getcwd(buf, PATH_MAX))
#endif
	    errorcall(call, "can't get working directory!\n");
	strcat(buf, "/");
	strcat(buf, path);
    }
    else strcpy(buf, path);
}

	/* do_dynload implements the R-Interface for the */
	/* loading of shared libraries */

/*
  Extended to support 2 additional arguments (3 in total).
  First argument is the name of the library.
  Second argument is a logical indicating whether we 
  want the symbols to be kept in their own local symbol table
  or added to the global symbol table of the application.
  Third argument is a logical indicating whether the 
  dynamic loading should relocate all routine symbols 
  now and signal any errors immediately or lazily relocate
  the symbols as they are invoked. This is useful for 
  developers so that they can ensure that all the symbols 
  are available before they release, and allows users to 
  call routines from "incomplete" libraries.
 */
SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[2 * PATH_MAX];
    checkArity(op,args);
    if (!isString(CAR(args)) || length(CAR(args)) < 1)
	errorcall(call, "character argument expected\n");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    DeleteDLL(buf);
    if(!AddDLL(buf,LOGICAL(CADR(args))[0],LOGICAL(CADDR(args))[0]))
	errorcall(call, "unable to load shared library \"%s\":\n  %s\n",
		  buf, DLLerror);
    return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[2 * PATH_MAX];
    checkArity(op,args);
    if (!isString(CAR(args)) || length(CAR(args)) < 1)
	errorcall(call, "character argument expected\n");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    if(!DeleteDLL(buf))
	errorcall(call, "shared library \"%s\" was not loaded\n", buf);
    return R_NilValue;
}

#else

void InitFunctionHashing()
{
#ifdef OLD
    NaokSymbol = install("NAOK");
    DupSymbol = install("DUP");
#endif
}

DL_FUNC R_FindSymbol(char const *name, char const *pkg)
{
    int i;
    for(i=0 ; CFunTab[i].name ; i++)
	if(!strcmp(name, CFunTab[i].name))
	    return CFunTab[i].func;
    return (DL_FUNC)0;
}

SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("no dyn.load support in this R version\n");
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("no dyn.load support in this R version\n");
}

#endif
