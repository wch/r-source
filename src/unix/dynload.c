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
# include <config.h>
#endif

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#include <Defn.h>
#include <Rmath.h>

#ifndef HAVE_NO_SYMBOL_UNDERSCORE
# ifdef HAVE_ELF_H
#  define HAVE_NO_SYMBOL_UNDERSCORE
# endif
#endif

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
# define RTLD_LAZY 1
#endif
#ifndef RTLD_NOW
# define RTLD_NOW  2
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

#define MAX_NUM_DLLS	100

static int CountDLL = 0;

#include "R_ext/Rdynload.h"
typedef struct {
  char *name;
  void       *fun;
  int         numArgs;
  /* Add information about argument types or converters. */  
} Rf_DotCSymbol;

typedef struct {
  char *name;
  void       *fun;
  int         numArgs;

} Rf_DotCallSymbol;

typedef struct {
  char *name;
  void       *fun;
  int         numArgs;

} Rf_DotFortranSymbol;


struct _DllInfo {
    char	   *path;
    char	   *name;
    void	   *handle;

    int            numCSymbols;
    Rf_DotCSymbol     *CSymbols;

    int            numCallSymbols;
    Rf_DotCallSymbol  *CallSymbols;

    int              numFortranSymbols;
    Rf_DotFortranSymbol *FortranSymbols;
};

static DllInfo LoadedDLL[MAX_NUM_DLLS];


#ifdef DL_SEARCH_PROG
static DllInfo baseDll;
#endif

void InitFunctionHashing()
{
#ifdef DL_SEARCH_PROG
    baseDLL.handle = dlopen(0, RTLD_NOW);
#endif
}


void R_addCRoutine(DllInfo *info, R_CMethodDef *croutine, Rf_DotCSymbol *sym);
void R_addCallRoutine(DllInfo *info, R_CallMethodDef *croutine, Rf_DotCallSymbol *sym);
void R_addFortranRoutine(DllInfo *info, R_FortranMethodDef *croutine, Rf_DotFortranSymbol *sym);

/*
 Returns a reference to the DllInfo object associated with the dynamic library
 with the path name `path'. This ensures uniqueness rather than having the 
 undesirable situation of two libraries with the same name but in different
 directories.
 This is available so that itcan be called from arbitrary C routines
 that need to call R_registerRoutines(). The initialization routine
 R_init_<library name> is passed the DllInfo reference as an argument.
 Other routines must explicitly request it using this routine.
 */
DllInfo *
R_getDllInfo(const char *path)
{ 
  int i;
  for(i = 0; i < CountDLL; i++) {
    if(strcmp(LoadedDLL[i].path, path) == 0)
       return(&LoadedDLL[i]);
  }
  return((DllInfo*) NULL);
}

/*
  Explicitly register the native routines for use in .Call(), .C() and .Fortran()
  functions. These registered values are used to resolve symbols in a library
  that makes a call to this routine, rather than the usual dynamic resolution
  done by dlsym() or the equivalent on the different platforms.
 */
int
R_registerRoutines(DllInfo *info, R_CMethodDef *croutines,
                     R_CallMethodDef *callRoutines,
                     R_FortranMethodDef *fortranRoutines)
{
 int i, num;

 if(info == NULL)
   error("R_RegisterRoutines called with invalid DllInfo object.");


 if(croutines) {
   for(num=0; croutines[num].name != NULL; num++) {;}
   info->CSymbols = (Rf_DotCSymbol*)calloc(num, sizeof(Rf_DotCSymbol));
   info->numCSymbols = num;
   for(i = 0; i < num; i++) {
     R_addCRoutine(info, croutines+i, info->CSymbols + i);
   }
 }

 if(callRoutines) {
   for(num=0; callRoutines[num].name != NULL; num++) {;}
   info->CallSymbols = (Rf_DotCallSymbol*)calloc(num, sizeof(Rf_DotCallSymbol));
   info->numCallSymbols = num;
   for(i = 0; i < num; i++) {
     R_addCallRoutine(info, callRoutines+i, info->CallSymbols + i);
   }
 }

 if(fortranRoutines) {
   for(num=0; fortranRoutines[num].name != NULL; num++) {;}
   info->FortranSymbols = (Rf_DotFortranSymbol*)calloc(num, sizeof(Rf_DotFortranSymbol));
   info->numFortranSymbols = num;

   for(i = 0; i < num; i++) {
     R_addFortranRoutine(info, fortranRoutines+i, info->FortranSymbols + i);
   }
 }

 return(1);
}

void
R_addFortranRoutine(DllInfo *info, R_FortranMethodDef *croutine, Rf_DotFortranSymbol *sym)
{
 sym->name = strdup(croutine->name);
 sym->fun = croutine->fun;
 sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}


void
R_addCRoutine(DllInfo *info, R_CMethodDef *croutine, Rf_DotCSymbol *sym)
{
 sym->name = strdup(croutine->name);
 sym->fun = croutine->fun;
 sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}

void
R_addCallRoutine(DllInfo *info, R_CallMethodDef *croutine, Rf_DotCallSymbol *sym)
{
 sym->name = strdup(croutine->name);
 sym->fun = croutine->fun;
 sym->numArgs = croutine->numArgs > -1 ? croutine->numArgs : -1;
}



void
Rf_freeCSymbol(Rf_DotCSymbol *sym)
{
  free(sym->name);
}

void
Rf_freeCallSymbol(Rf_DotCallSymbol *sym)
{
  free(sym->name);
}

void
Rf_freeFortranSymbol(Rf_DotFortranSymbol *sym)
{
  free(sym->name);
}

void
Rf_freeDllInfo(DllInfo *info)
{
  int i;
    free(info->name);
    free(info->path);
    if(info->CSymbols) {
      for(i = 0; i < info->numCSymbols; i++)
        Rf_freeCSymbol(info->CSymbols+i);
      free(info->CSymbols);
    }
    if(info->CallSymbols) {
      for(i = 0; i < info->numCallSymbols; i++)
        Rf_freeCallSymbol(info->CallSymbols+i);
      free(info->CallSymbols);
    }
    if(info->FortranSymbols) {
      for(i = 0; i < info->numFortranSymbols; i++)
        Rf_freeFortranSymbol(info->FortranSymbols+i);
      free(info->FortranSymbols);
    }
}


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
      /* Wouldn't a linked list be easier here? 
         Potentially ruin the contiguity of the memory.
       */
    for(i = nCPFun - 1; i >= 0; i--)
	if(!strcmp(CPFun[i].pkg, LoadedDLL[loc].name)) {
	    if(i < nCPFun - 1) {
		strcpy(CPFun[i].name, CPFun[--nCPFun].name);
		strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
		CPFun[i].func = CPFun[nCPFun].func;
	    } else nCPFun--;
	}
#endif
    Rf_freeDllInfo(LoadedDLL+loc);
    dlclose(LoadedDLL[loc].handle);
    for(i = loc + 1 ; i < CountDLL ; i++) {
	LoadedDLL[i - 1].path = LoadedDLL[i].path;
	LoadedDLL[i - 1].name = LoadedDLL[i].name;
	LoadedDLL[i - 1].handle = LoadedDLL[i].handle;
	LoadedDLL[i - 1].numCSymbols = LoadedDLL[i].numCSymbols;
	LoadedDLL[i - 1].numCallSymbols = LoadedDLL[i].numCallSymbols;
	LoadedDLL[i - 1].numFortranSymbols = LoadedDLL[i].numFortranSymbols;
	LoadedDLL[i - 1].CSymbols = LoadedDLL[i].CSymbols;
	LoadedDLL[i - 1].CallSymbols = LoadedDLL[i].CallSymbols;
	LoadedDLL[i - 1].FortranSymbols = LoadedDLL[i].FortranSymbols;
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

static DL_FUNC R_dlsym(DllInfo *dll, char const *name);

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

    LoadedDLL[CountDLL].numCSymbols = 0;
    LoadedDLL[CountDLL].numCallSymbols = 0;
    LoadedDLL[CountDLL].numFortranSymbols = 0;
    LoadedDLL[CountDLL].CSymbols = NULL;
    LoadedDLL[CountDLL].CallSymbols = NULL;
    LoadedDLL[CountDLL].FortranSymbols = NULL;

    {
      char *tmp;
      DL_FUNC f;
      tmp = (char*) malloc(sizeof(char)*(strlen("R_init_") + strlen(name)+ 1));
      sprintf(tmp, "%s%s","R_init_", name);
      f = (DL_FUNC) dlsym(LoadedDLL[CountDLL].handle, tmp);
      if(f)
        f(LoadedDLL + CountDLL);
    }
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

Rf_DotCSymbol *
Rf_lookupRegisteredCSymbol(DllInfo *info, const char *name)
{
  int i;
      for(i = 0; i < info->numCSymbols; i++) {
        if(strcmp(name, info->CSymbols[i].name) == 0)
          return(&(info->CSymbols[i]));
      }

 return(NULL);
}

Rf_DotFortranSymbol *
Rf_lookupRegisteredFortranSymbol(DllInfo *info, const char *name)
{
  int i;
   for(i = 0; i < info->numFortranSymbols; i++) {
     if(strcmp(name, info->FortranSymbols[i].name) == 0)
      return(&(info->FortranSymbols[i]));
   }

 return((Rf_DotFortranSymbol*)NULL);
}

Rf_DotCallSymbol *
Rf_lookupRegisteredCallSymbol(DllInfo *info, const char *name)
{
  int i;
      for(i = 0; i < info->numCallSymbols; i++) {
        if(strcmp(name, info->CallSymbols[i].name) == 0)
          return(&(info->CallSymbols[i]));
      }
 return((Rf_DotCallSymbol*)NULL);
}


static DL_FUNC R_dlsym(DllInfo *info, char const *name)
{
    char buf[MAXIDSIZE+1];
    int fail = 0;
    if(info->numCSymbols > 0) {
      Rf_DotCSymbol *sym;
      sym = Rf_lookupRegisteredCSymbol(info, name);
      if(sym)
        return((DL_FUNC) sym->fun);
      fail = 1;
    }

    if(info->numFortranSymbols > 0) {
      Rf_DotFortranSymbol *sym;
      sym = Rf_lookupRegisteredFortranSymbol(info, name);
      if(sym)
        return((DL_FUNC) sym->fun);
      fail = 1;
    }

    if(info->numCallSymbols > 0) {
      Rf_DotCallSymbol *sym;
      sym = Rf_lookupRegisteredCallSymbol(info, name);
      if(sym)
        return((DL_FUNC) sym->fun);
      fail = 1;
    }
    
    if(fail)
      return((DL_FUNC) NULL);

#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    sprintf(buf, "%s", name);
#else
    sprintf(buf, "_%s", name);
#endif
    return (DL_FUNC) dlsym(info->handle, buf);
}

	/* R_FindSymbol checks whether one of the libraries */
	/* that have been loaded contains the symbol name and */
	/* returns a pointer to that symbol upon success. */

/*
  In the future, this will receive an additional argument
  which will specify the nature of the symbol expected by the 
  caller, specifically whether it is for a .C(), .Call(),
  .Fortran(), .External(), generic, etc. invocation. This will 
  reduce the pool of possible symbols in the case of a library
  that registers its routines.
 */

DL_FUNC R_FindSymbol(char const *name, char const *pkg)
{
    DL_FUNC fcnptr;
    int i, all = (strlen(pkg) == 0), doit;

#ifdef CACHE_DLL_SYM
    for (i = 0; i < nCPFun; i++)
	if (!strcmp(name, CPFun[i].name) && 
	    (all || !strcmp(pkg, CPFun[i].pkg)))
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
	    fcnptr = R_dlsym(&LoadedDLL[i], name);
	    if (fcnptr != (DL_FUNC) NULL) {
#ifdef CACHE_DLL_SYM
		if(strlen(pkg) <= 20 && strlen(name) <= 20 && nCPFun < 100) {
		    strcpy(CPFun[nCPFun].pkg, LoadedDLL[i].name);
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
	fcnptr = R_dlsym(&baseDll, name);
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
    GetFullDLLPath(call, buf, CHAR(STRING_ELT(CAR(args), 0)));
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
    GetFullDLLPath(call, buf, CHAR(STRING_ELT(CAR(args), 0)));
    if(!DeleteDLL(buf))
	errorcall(call, "shared library \"%s\" was not loaded", buf);
    return R_NilValue;
}


#include "Runix.h"
#include <sys/types.h>
#include <sys/stat.h>

extern DL_FUNC ptr_X11DeviceDriver, ptr_dataentry, ptr_R_GetX11Image,
    ptr_R_loadhistory, ptr_R_savehistory;

void R_load_X11_shlib(void)
{
    char X11_DLL[PATH_MAX], buf[1000], *p; 
    DllInfo dll = {(char *)NULL, (char*)NULL, (char*)NULL, 
                   0, (Rf_DotCSymbol*)NULL, 0, (Rf_DotCallSymbol*)NULL, 
                   0, (Rf_DotFortranSymbol*)NULL};
    struct stat sb;

    p = getenv("R_HOME");
    if(!p) {
	sprintf(buf, "R_HOME was not set");
	R_Suicide(buf);
    }
    strcpy(X11_DLL, p);
    strcat(X11_DLL, "/modules/R_X11.");
    strcat(X11_DLL, SHLIB_EXT); /* from config.h */
    if(stat(X11_DLL, &sb))
	R_Suicide("Probably no X11 support: the shared library was not found");
/* cannot use computeDLOpenFlag as warnings will crash R at this stage */
#ifdef RTLD_NOW
    dll.handle = dlopen(X11_DLL, RTLD_NOW);
#else
    dll.handle = dlopen(X11_DLL, 0);
#endif
    if(dll.handle == NULL) {
	sprintf(buf, "The X11 shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }
    ptr_X11DeviceDriver = R_dlsym(&dll, "X11DeviceDriver");
    if(!ptr_X11DeviceDriver) R_Suicide("Cannot load X11DeviceDriver");
    ptr_dataentry = R_dlsym(&dll, "RX11_dataentry");
    if(!ptr_dataentry) R_Suicide("Cannot load do_dataentry");
    ptr_R_GetX11Image = R_dlsym(&dll, "R_GetX11Image");
    if(!ptr_R_GetX11Image) R_Suicide("Cannot load R_GetX11Image");
}

extern DL_FUNC ptr_R_Suicide, ptr_R_ShowMessage, ptr_R_ReadConsole,
    ptr_R_WriteConsole, ptr_R_ResetConsole, ptr_R_FlushConsole,
    ptr_R_ClearerrConsole, ptr_R_Busy, ptr_R_CleanUp, ptr_R_ShowFiles,
    ptr_R_ChooseFile, ptr_gnome_start, 
    ptr_GnomeDeviceDriver, ptr_GTKDeviceDriver;


void R_load_gnome_shlib(void)
{
    char gnome_DLL[PATH_MAX], buf[1000], *p;
    DllInfo dll = {(char *)NULL, (char*)NULL, (char*)NULL, 
                   0, (Rf_DotCSymbol*)NULL, 0, (Rf_DotCallSymbol*)NULL, 
                   0, (Rf_DotFortranSymbol*)NULL};
    struct stat sb;

    p = getenv("R_HOME");
    if(!p) {
	sprintf(buf, "R_HOME was not set");
	R_Suicide(buf);
    }
    strcpy(gnome_DLL, p);
    strcat(gnome_DLL, "/modules/R_gnome.");
    strcat(gnome_DLL, SHLIB_EXT); /* from config.h */
    if(stat(gnome_DLL, &sb))
	R_Suicide("Probably no GNOME support: the shared library was not found");
/* cannot use computeDLOpenFlag as warnings will crash R at this stage */
#ifdef RTLD_NOW
    dll.handle = dlopen(gnome_DLL, RTLD_NOW);
#else
    dll.handle = dlopen(gnome_DLL, 0);
#endif
    if(dll.handle == NULL) {
	sprintf(buf, "The GNOME shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }
    ptr_R_Suicide = R_dlsym(&dll, "Rgnome_Suicide");
    if(!ptr_R_Suicide) Rstd_Suicide("Cannot load R_Suicide");
    ptr_R_ShowMessage = R_dlsym(&dll, "Rgnome_ShowMessage");
    if(!ptr_R_ShowMessage) R_Suicide("Cannot load R_ShowMessage");
    ptr_R_ReadConsole = R_dlsym(&dll, "Rgnome_ReadConsole");
    if(!ptr_R_ReadConsole) R_Suicide("Cannot load R_ReadConsole");
    ptr_R_WriteConsole = R_dlsym(&dll, "Rgnome_WriteConsole");
    if(!ptr_R_WriteConsole) R_Suicide("Cannot load R_WriteConsole");
    ptr_R_ResetConsole = R_dlsym(&dll, "Rgnome_ResetConsole");
    if(!ptr_R_ResetConsole) R_Suicide("Cannot load R_ResetConsole");
    ptr_R_FlushConsole = R_dlsym(&dll, "Rgnome_FlushConsole");
    if(!ptr_R_FlushConsole) R_Suicide("Cannot load R_FlushConsole");
    ptr_R_ClearerrConsole = R_dlsym(&dll, "Rgnome_ClearerrConsole");
    if(!ptr_R_ClearerrConsole) R_Suicide("Cannot load R_ClearerrConsole");
    ptr_R_Busy = R_dlsym(&dll, "Rgnome_Busy");
    if(!ptr_R_Busy) R_Suicide("Cannot load R_Busy");
    ptr_R_CleanUp = R_dlsym(&dll, "Rgnome_CleanUp");
    if(!ptr_R_CleanUp) R_Suicide("Cannot load R_CleanUp");
    ptr_R_ShowFiles = R_dlsym(&dll, "Rgnome_ShowFiles");
    if(!ptr_R_ShowFiles) R_Suicide("Cannot load R_ShowFiles");
    ptr_R_ChooseFile = R_dlsym(&dll, "Rgnome_ChooseFile");
    if(!ptr_R_ChooseFile) R_Suicide("Cannot load R_ChooseFile");
    ptr_gnome_start = R_dlsym(&dll, "gnome_start");
    if(!ptr_gnome_start) R_Suicide("Cannot load gnome_start");
    ptr_GTKDeviceDriver = R_dlsym(&dll, "GTKDeviceDriver");
    if(!ptr_GTKDeviceDriver) R_Suicide("Cannot load GTKDeviceDriver");
    ptr_R_loadhistory = R_dlsym(&dll, "Rgnome_loadhistory");
    if(!ptr_R_loadhistory) R_Suicide("Cannot load Rgnome_loadhsitoryr");
    ptr_R_savehistory = R_dlsym(&dll, "Rgnome_savehistory");
    if(!ptr_R_savehistory) R_Suicide("Cannot load Rgnome_savehsitoryr");
/* Uncomment the next two lines to experiment with the gnome() device */
/*    ptr_GnomeDeviceDriver = R_dlsym(&dll, "GnomeDeviceDriver");
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
