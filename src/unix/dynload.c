/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2000 The R Development Core Team
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
#include <config.h>
#endif

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "Defn.h"
#include "Mathlib.h"

/* DL_FUNC is in Defn.h */
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

#undef CACHE_DLL_SYM
#ifdef CACHE_DLL_SYM
/* keep a record of symbols that have been found */
static struct {
    char pkg[21];
    char name[21];
    DL_FUNC func;
}  CPFun[100];
static int nCPFun = 0;
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
    int   i, loc;

    for (i = 0; i < CountDLL; i++) {
	if (!strcmp(path, LoadedDLL[i].path)) {
	    loc = i;
	    goto found;
	}
    }
    return 0;
found:
#ifdef CACHE_DLL_SYM
    for(i = 0; i < nCPFun; i++)
	if(!strcmp(CPFun[i].pkg, LoadedDLL[loc].name)) {
	    strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
	    strcpy(CPFun[i].name, CPFun[nCPFun].name);
	    CPFun[i].func = CPFun[nCPFun--].func;
	}
#endif
    free(LoadedDLL[loc].name);
    free(LoadedDLL[loc].path);
    dlclose(LoadedDLL[loc].handle);
    for(i = loc + 1 ; i < CountDLL ; i++) {
	LoadedDLL[i - 1].path = LoadedDLL[i].path;
	LoadedDLL[i - 1].name = LoadedDLL[i].name;
	LoadedDLL[i - 1].handle = LoadedDLL[i].handle;
    }
    CountDLL--;
    return 1;
}

#define DLLerrBUFSIZE 1000
static char DLLerror[DLLerrBUFSIZE] = "";

/* the error message; length taken from ERRBUFSIZE in ./hpdlfcn.c  */

	/* Inserts the specified DLL at the head of the DLL list */
	/* Returns 1 if the library was successfully added */
	/* and returns 0 if the library table is full or */
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
#if !defined(RTLD_LOCAL) || !defined(RTLD_GLOBAL) || !defined(RTLD_NOW) || !defined(RTLD_LAZY)
    static char *warningMessages[] = {
	"Explicit local dynamic loading not supported on this platform. Using default.",
	"Explicit global dynamic loading not supported on this platform. Using default.",
	"Explicit non-lazy dynamic loading not supported on this platform. Using default.",
	"Explicit lazy dynamic loading not supported on this platform. Using default."
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
    if(asInteger(GetOption(install("warn"), R_NilValue)) == 1 || \
       asInteger(GetOption(install("verbose"), R_NilValue)) > 0) \
        warning(warningMessages[i]);
#endif

    int openFlag = 0;		/* Default value so no-ops for undefined
				   flags should do nothing in the
				   resulting dlopen(). */

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


static DL_FUNC R_dlsym(void *handle, char const *name)
{
    char buf[MAXIDSIZE+1];
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    sprintf(buf, "%s", name);
#else
    sprintf(buf, "_%s", name);
#endif
    return (DL_FUNC) dlsym(handle, buf);
}

	/* R_FindSymbol checks whether one of the libraries */
	/* that have been loaded contains the symbol name and */
	/* returns a pointer to that symbol upon success. */


DL_FUNC R_FindSymbol(char const *name, char const *pkg)
{
    DL_FUNC fcnptr;
    int i, all = (strlen(pkg) == 0), doit;

#ifdef CACHE_DLL_SYM
    for (i = 0; i < nCPFun; i++)
	if (!strcmp(pkg, CPFun[i].pkg) && 
	    !strcmp(name, CPFun[i].name))
	    return CPFun[i].func;
#endif


	/* The following is not legal ANSI C. */
	/* It is only meant to be used in systems supporting */
	/* the dlopen() interface, in which systems data and  */
	/* function pointers _are_ the same size and _can_   */
	/* be cast without loss of information.		     */

    for (i = CountDLL - 1; i >= 0; i--) {
	doit = all;
	if(!doit && !strcmp(pkg, LoadedDLL[i].name)) doit = 2;
	if(doit) {
	    fcnptr = R_dlsym(LoadedDLL[i].handle, name);
	    if (fcnptr != (DL_FUNC) NULL) {
#ifdef CACHE_DLL_SYM
		if(strlen(pkg) <= 20 && strlen(name) <= 20 && nCPFun < 100) {
		    strcpy(CPFun[nCPFun].pkg, pkg);
		    strcpy(CPFun[nCPFun].name, name);
		    CPFun[nCPFun++].func = fcnptr;
		}
#endif
	       return fcnptr;
	   }
	}
	if(doit > 1) return (DL_FUNC) NULL;  /* Only look in the first-matching DLL */
    }
    if(all || !strcmp(pkg, "base")) {
#ifdef DL_SEARCH_PROG
	fcnptr = R_dlsym(dlhandle, name);
#else
	for(i=0 ; CFunTab[i].name ; i++)
	    if(!strcmp(name, CFunTab[i].name))
		return CFunTab[i].func;
#endif
    }
    return (DL_FUNC) NULL;
}


static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
    if(path[0] == '~')
	strcpy(buf, R_ExpandFileName(path));
    else if(path[0] != '/') {
#ifdef HAVE_UNISTD_H
	if(!getcwd(buf, PATH_MAX))
#endif
	    errorcall(call, "can't get working directory!");
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
	errorcall(call, "character argument expected");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    DeleteDLL(buf);
    if(!AddDLL(buf,LOGICAL(CADR(args))[0],LOGICAL(CADDR(args))[0]))
	errorcall(call, "unable to load shared library \"%s\":\n  %s",
		  buf, DLLerror);
    return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[2 * PATH_MAX];
    checkArity(op,args);
    if (!isString(CAR(args)) || length(CAR(args)) < 1)
	errorcall(call, "character argument expected");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    if(!DeleteDLL(buf))
	errorcall(call, "shared library \"%s\" was not loaded", buf);
    return R_NilValue;
}


#include "Runix.h"
#include <sys/types.h>
#include <sys/stat.h>

extern DL_FUNC ptr_X11DeviceDriver, ptr_dataentry;

void R_load_X11_shlib()
{
    char X11_DLL[PATH_MAX], buf[1000], *p;
    void *handle;
    struct stat sb;

    p = getenv("R_HOME");
    if(!p) {
	sprintf(buf, "R_HOME was not set");
	R_Suicide(buf);
    }
    strcpy(X11_DLL, p);
    strcat(X11_DLL, "/bin/R_X11.");
    strcat(X11_DLL, SHLIBEXT); /* from config.h */
    if(stat(X11_DLL, &sb))
	R_Suicide("Probably no X11 support: the shared library was not found");
/* cannot use computeDLOpenFlag as warnings will crash R at this stage */
#ifdef RTLD_NOW
    handle = dlopen(X11_DLL, RTLD_NOW);
#else
    handle = dlopen(X11_DLL, 0);
#endif
    if(handle == NULL) {
	sprintf(buf, "The X11 shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }
    ptr_X11DeviceDriver = R_dlsym(handle, "X11DeviceDriver");
    if(!ptr_X11DeviceDriver) R_Suicide("Cannot load X11DeviceDriver");
    ptr_dataentry = R_dlsym(handle, "RX11_dataentry");
    if(!ptr_dataentry) R_Suicide("Cannot load do_dataentry");
}

extern DL_FUNC ptr_R_Suicide, ptr_R_ShowMessage, ptr_R_ReadConsole,
    ptr_R_WriteConsole, ptr_R_ResetConsole, ptr_R_FlushConsole,
    ptr_R_ClearerrConsole, ptr_R_Busy, ptr_R_CleanUp, ptr_R_ShowFiles,
    ptr_R_ChooseFile, ptr_gnome_start, 
    ptr_GnomeDeviceDriver, ptr_GTKDeviceDriver;


void R_load_gnome_shlib()
{
    char gnome_DLL[PATH_MAX], buf[1000], *p;
    void *handle;
    struct stat sb;

    p = getenv("R_HOME");
    if(!p) {
	sprintf(buf, "R_HOME was not set");
	R_Suicide(buf);
    }
    strcpy(gnome_DLL, p);
    strcat(gnome_DLL, "/gnome/R_gnome.");
    strcat(gnome_DLL, SHLIBEXT); /* from config.h */
    if(stat(gnome_DLL, &sb))
	R_Suicide("Probably no GNOME support: the shared library was not found");
/* cannot use computeDLOpenFlag as warnings will crash R at this stage */
#ifdef RTLD_NOW
    handle = dlopen(gnome_DLL, RTLD_NOW);
#else
    handle = dlopen(gnome_DLL, 0);
#endif
    if(handle == NULL) {
	sprintf(buf, "The GNOME shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }
    ptr_R_Suicide = R_dlsym(handle, "Rgnome_Suicide");
    if(!ptr_R_Suicide) Rstd_Suicide("Cannot load R_Suicide");
    ptr_R_ShowMessage = R_dlsym(handle, "Rgnome_ShowMessage");
    if(!ptr_R_ShowMessage) R_Suicide("Cannot load R_ShowMessage");
    ptr_R_ReadConsole = R_dlsym(handle, "Rgnome_ReadConsole");
    if(!ptr_R_ReadConsole) R_Suicide("Cannot load R_ReadConsole");
    ptr_R_WriteConsole = R_dlsym(handle, "Rgnome_WriteConsole");
    if(!ptr_R_WriteConsole) R_Suicide("Cannot load R_WriteConsole");
    ptr_R_ResetConsole = R_dlsym(handle, "Rgnome_ResetConsole");
    if(!ptr_R_ResetConsole) R_Suicide("Cannot load R_ResetConsole");
    ptr_R_FlushConsole = R_dlsym(handle, "Rgnome_FlushConsole");
    if(!ptr_R_FlushConsole) R_Suicide("Cannot load R_FlushConsole");
    ptr_R_ClearerrConsole = R_dlsym(handle, "Rgnome_ClearerrConsole");
    if(!ptr_R_ClearerrConsole) R_Suicide("Cannot load R_ClearerrConsole");
    ptr_R_Busy = R_dlsym(handle, "Rgnome_Busy");
    if(!ptr_R_Busy) R_Suicide("Cannot load R_Busy");
    ptr_R_CleanUp = R_dlsym(handle, "Rgnome_CleanUp");
    if(!ptr_R_CleanUp) R_Suicide("Cannot load R_CleanUp");
    ptr_R_ShowFiles = R_dlsym(handle, "Rgnome_ShowFiles");
    if(!ptr_R_ShowFiles) R_Suicide("Cannot load R_ShowFiles");
    ptr_R_ChooseFile = R_dlsym(handle, "Rgnome_ChooseFile");
    if(!ptr_R_ChooseFile) R_Suicide("Cannot load R_ChooseFile");
    ptr_gnome_start = R_dlsym(handle, "gnome_start");
    if(!ptr_gnome_start) R_Suicide("Cannot load gnome_start");
    ptr_GTKDeviceDriver = R_dlsym(handle, "GTKDeviceDriver");
    if(!ptr_GTKDeviceDriver) R_Suicide("Cannot load GTKDeviceDriver");
/* Uncomment the next two lines to experiment with the gnome() device */
/*    ptr_GnomeDeviceDriver = R_dlsym(handle, "GnomeDeviceDriver");
      if(!ptr_GnomeDeviceDriver) R_Suicide("Cannot load GnomeDeviceDriver");*/
}


#else /* no dyn.load support */

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
    error("no dyn.load support in this R version");
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("no dyn.load support in this R version");
}

void R_load_X11_shlib()
{
    R_Suicide("no support to load X11 shared library in this R version");
}


void R_load_gnome_shlib()
{
    R_Suicide("no support to load gnome shared library in this R version");
}
#endif
