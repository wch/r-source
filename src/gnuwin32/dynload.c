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
#include <config.h>
#endif

#include "Defn.h"
#include "Mathlib.h"
#include <string.h>
#include <stdlib.h>
#include <direct.h>
#include <windows.h>

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

#define CACHE_DLL_SYM
#ifdef CACHE_DLL_SYM
/* keep a record of symbols that have been found */
static struct {
    char pkg[21];
    char name[21];
    DL_FUNC func;
}  CPFun[100];
static int nCPFun = 0;
#endif


void InitFunctionHashing()
{
}


#define MAX_NUM_DLLS    30

static int CountDLL = 0;

static struct {
    char  *path;
    char  *name;
    HINSTANCE dlh;
} LoadedDLL[MAX_NUM_DLLS];

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
    FreeLibrary(LoadedDLL[loc].dlh);
    for (i = loc + 1; i < CountDLL; i++) {
	LoadedDLL[i - 1].path = LoadedDLL[i].path;
	LoadedDLL[i - 1].name = LoadedDLL[i].name;
	LoadedDLL[i - 1].dlh = LoadedDLL[i].dlh;
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
        /* or if LoadLibrary fails for some reason. */

/*
  The arguments asLocal and now are unused in this version.
  They are here for consistency with the UNIX code.
 */
static int AddDLL(char *path, int asLocal, int now)
{
    HINSTANCE tdlh;
    char *dpath, *name, DLLname[MAX_PATH], *p, *st;
    /* int i; */

    DeleteDLL(path);
    if (CountDLL == MAX_NUM_DLLS) {
	strcpy(DLLerror, "Maximal number of DLLs reached...");
	return 0;
    }
    tdlh = LoadLibrary(path);
    if (tdlh == NULL) {
	strcpy(DLLerror, "LoadLibrary failure");
	return 0;
    }

    dpath = malloc(strlen(path)+1);
    if(dpath == NULL) {
	strcpy(DLLerror,"Couldn't allocate space for 'path'");
	FreeLibrary(tdlh);
	return 0;
    }
    strcpy(dpath, path);

    strcpy(DLLname, path);
    for(p = DLLname; *p != '\0'; p++) if(*p == '\\') *p = '/';
    p = strrchr(path, '/');
    if(!p) p = DLLname; else p++;
    st = strchr(p, '.');
    if(st) *st = '\0';
    name = malloc(strlen(p)+1);
    if(name == NULL) {
	strcpy(DLLerror,"Couldn't allocate space for 'name'");
	FreeLibrary(tdlh);
	free(dpath);
	return 0;
    }
    strcpy(name, p);

    LoadedDLL[CountDLL].path = dpath;
    LoadedDLL[CountDLL].name = name;
    LoadedDLL[CountDLL].dlh = tdlh;
    CountDLL++;
    return 1;
}


        /* R_FindSymbol checks whether one of the libraries */
        /* that have been loaded contains the symbol name and */
        /* returns a pointer to that symbol upon success. */

DL_FUNC R_FindSymbol(char const *name, char const *pkg)
{
    DL_FUNC fcnptr;
    int   i, j, all=(strlen(pkg) == 0), doit;
    static int NumStatic = 0;
    int   mid, high, low, cmp;

#ifdef CACHE_DLL_SYM
    for (i = 0; i < nCPFun; i++)
	if (!strcmp(pkg, CPFun[i].pkg) && 
	    !strcmp(name, CPFun[i].name))
	    return CPFun[i].func;
#endif

    for (i = CountDLL - 1; i >= 0; i--) {
	doit = all;
	if(!doit && !strcmp(pkg, LoadedDLL[i].name)) doit = 2;
	if(doit) {
	    fcnptr = (DL_FUNC) GetProcAddress(LoadedDLL[i].dlh, name);
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
	if(doit > 1) return (DL_FUNC) NULL;/* Only look in the first-matching DLL*/
    }
    if (!NumStatic && (all || !strcmp(pkg, "base"))) {
	char *tname;
	DL_FUNC tfunc;

	NumStatic = (sizeof(CFunTab) / sizeof(CFunTabEntry)) - 1;
	for (i = 0; i < NumStatic; i++)
	    for (j = i + 1; j < NumStatic; j++) {
		if (strcmp(CFunTab[j].name, CFunTab[i].name) < 0) {
		    tname = CFunTab[i].name;
		    tfunc = CFunTab[i].func;
		    CFunTab[i].name = CFunTab[j].name;
		    CFunTab[i].func = CFunTab[j].func;
		    CFunTab[j].name = tname;
		    CFunTab[j].func = tfunc;
		}
	    }
    }
    low = 0;
    mid = 0;
    high = NumStatic - 1;
    while (low <= high) {
	mid = (low + high) / 2;
	cmp = strcmp(name, CFunTab[mid].name);
	if (cmp < 0)
	    high = mid - 1;
	else if (cmp > 0)
	    low = mid + 1;
	else
	    break;
    }
    if (high < low)
	return (DL_FUNC) NULL;
    else
	return CFunTab[mid].func;
    return (DL_FUNC) NULL;
}


static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
    if ((path[0] != '/') && (path[0] != '\\') && (path[1] != ':')) {
	if (!getcwd(buf, MAX_PATH))
	    errorcall(call, "can't get working directory!");
	strcat(buf, "\\");
	strcat(buf, path);
    } else
	strcpy(buf, path);
}

        /* do_dynload implements the R-Interface for the */
        /* loading of shared libraries */
/* This looks very close the version in unix.
   Is the only reason it is not shared due to
    a) 2*PATH_MAX v's PATH_MAX for sizeof(buf)
    b) static routines in this file.
*/
SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char  buf[MAX_PATH];

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	errorcall(call, "character argument expected");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    DeleteDLL(buf);
    if (!AddDLL(buf,LOGICAL(CADR(args))[0],LOGICAL(CADDR(args))[0]))
	errorcall(call, "unable to load shared library \"%s\":\n  %s",
		  buf, DLLerror);
    return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char  buf[MAX_PATH];

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	errorcall(call, "character argument expected");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    if (!DeleteDLL(buf))
	errorcall(call, "dynamic library \"%s\" was not loaded", buf);
    return R_NilValue;
}
