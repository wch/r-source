/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997  The R Core Team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
 *  in the executable.  We do this by straight linear search through
 *  the table.  Note that the content of the table is created at
 *  system build time from the list in ../appl/ROUTINES.
 */

#include "Defn.h"
#include "Mathlib.h"
#include <string.h>
#include <stdlib.h>
#include <sys/param.h>

typedef int (*DL_FUNC)();
typedef struct {
        char *name;
        DL_FUNC func;
} CFunTabEntry;  
#include "FFDecl.h"
#include "FFDecl.h"

	/* This provides a table of built-in C and Fortran functions */
	/* We include this table, even when we have dlopen and friends */
	/* This is so that the functions are actually loaded at link time */

static CFunTabEntry CFunTab[] =
{
#include "FFTab.h"
        {NULL, NULL}
};      

	/* The following code loads in a compatibility module */
	/* written by Luke Tierney to support S version 4 on */
	/* Hewlett-Packard machines.  The relevant defines are */
	/* set up by autoconfigure */

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

#ifdef DL_SEARCH_PROG
static void *dlhandle;
#endif

void InitFunctionHashing()
{
#ifdef DL_SEARCH_PROG
	dlhandle = dlopen(0, RTLD_LAZY);
#endif
}


#define MAX_NUM_DLLS	100

static int CountDLL = 0;

static struct {
	char	*path;
	void	*handle;
}
LoadedDLL[MAX_NUM_DLLS];

	/* Remove the specified DLL from the current DLL list */
	/* Returns 1 if the DLL was found and removed from */
	/* the list and returns 0 otherwise. */

static DeleteDLL(char *path)
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
	free(LoadedDLL[i].path);
	dlclose(LoadedDLL[i].handle);
	for(i=loc+1 ; i<CountDLL ; i++) {
		LoadedDLL[i-1].path = LoadedDLL[i].path;
		LoadedDLL[i-1].handle = LoadedDLL[i].handle;
	}
	CountDLL--;
	return 1;
}

	/* Inserts the specified DLL at the start of the DLL list */
	/* All the other entries are "moved down" by one. */
	/* Returns 1 if the library was successfully added */
	/* and returns 0 if there library table is full or */
	/* or if dlopen fails for some reason. */

static AddDLL(char *path)
{
	void *handle;
	char *dpath;
	int i;
	if(CountDLL == MAX_NUM_DLLS)
		return 0;
	handle = dlopen(path, RTLD_LAZY);
	if(handle == NULL)
		return 0;
	dpath = malloc(strlen(path)+1);
	if(dpath == NULL) {
		dlclose(handle);
		return 0;
	}
	strcpy(dpath, path);
	for(i=CountDLL ; i>0 ; i--) {
		LoadedDLL[i].path = LoadedDLL[i-1].path;
		LoadedDLL[i].handle = LoadedDLL[i-1].handle;
	}
	LoadedDLL[0].path = dpath;
	LoadedDLL[0].handle = handle;
	CountDLL++;
	return 1;
}


	/* R_FindSymbol checks whether one of the libraries */
	/* that have been loaded contains the symbol name and */
	/* returns a pointer to that symbol upon success. */


DL_FUNC R_FindSymbol(char const *name)
{
	char buf[MAXIDSIZE+1];
	DL_FUNC fcnptr;
	int i;

#ifdef HAVE_NO_SYMBOL_UNDERSCORE
	sprintf(buf, "%s", name);
#else
	sprintf(buf, "_%s", name);
#endif

	/* The following is not legal ANSI C. */
	/* It is only meant to be used in systems supporting */
	/* the dlopen() interface, in which systems data and  */
	/* function pointers _are_ the same size and _can_   */
	/* be cast without loss of information.              */

	for (i=0 ; i<CountDLL ; i++) {
		fcnptr = (DL_FUNC)dlsym(LoadedDLL[i].handle, buf);
		if (fcnptr != (DL_FUNC)0) return fcnptr;
	}
#ifdef DL_SEARCH_PROG
	fcnptr = (DL_FUNC)dlsym(dlhandle, buf);
#else
	for(i=0 ; CFunTab[i].name ; i++)
		if(!strcmp(name, CFunTab[i].name))
			return CFunTab[i].func;
#endif
	return (DL_FUNC)0;
}


static GetFullDLLPath(SEXP call, char *buf, char *path)
{
	if(path[0] != '/') {
		if(!getcwd(buf, MAXPATHLEN))
			errorcall(call, "can't get working directory!\n");
		strcat(buf, "/");
		strcat(buf, path);
	}
	else strcpy(buf, path);
}

	/* do_dynload implements the R-Interface for the */
	/* loading of shared libraries */

SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[2*MAXPATHLEN];
	checkArity(op,args);
	if (!isString(CAR(args)) || length(CAR(args)) < 1)
		errorcall(call, "character argument expected\n");
	GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
	DeleteDLL(buf);
	if(!AddDLL(buf))
		errorcall(call, "unable to load shared library \"%s\"\n", buf);
	return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
	char buf[2*MAXPATHLEN];
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

DL_FUNC R_FindSymbol(char const *name)
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
