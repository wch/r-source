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
#include <direct.h>
#include <windows.h>

typedef int (*DL_FUNC) ();
typedef struct {
    char *name;
    DL_FUNC func;
}     CFunTabEntry;

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


void InitFunctionHashing()
{
}


#define MAX_NUM_DLLS    30

static int CountDLL = 0;

static struct {
    char  path[MAX_PATH + 1];
    HINSTANCE dlh;
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
    FreeLibrary(LoadedDLL[i].dlh);
    for (i = loc + 1; i < CountDLL; i++) {
	strcpy(LoadedDLL[i - 1].path, LoadedDLL[i].path);
	LoadedDLL[i - 1].dlh = LoadedDLL[i].dlh;
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
        /* or if LoadLibrary fails for some reason. */

static int AddDLL(char *path)
{
    HINSTANCE tdlh;

    if (CountDLL == MAX_NUM_DLLS) {
	strcpy(DLLerror, "Maximal number of DLLs reached...");
	return 0;
    }
    tdlh = LoadLibrary(path);
    if (tdlh == NULL) {
	strcpy(DLLerror, "LoadLibrary failure");
	return 0;
    }
    strcpy(LoadedDLL[CountDLL].path, path);
    LoadedDLL[CountDLL].dlh = tdlh;
    CountDLL++;
    return 1;
}


        /* R_FindSymbol checks whether one of the libraries */
        /* that have been loaded contains the symbol name and */
        /* returns a pointer to that symbol upon success. */

DL_FUNC R_FindSymbol(char const *name)
{
    char  buf[MAXIDSIZE + 1];
    DL_FUNC fcnptr;
    int   i, j;
    static int NumStatic = 0;
    int   mid, high, low, cmp;

#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    sprintf(buf, "%s", name);
#else
    sprintf(buf, "_%s", name);
#endif
    sprintf(buf, "%s", name);
    for (i = CountDLL - 1; i >= 0; i--) {
	fcnptr = (DL_FUNC) GetProcAddress(LoadedDLL[i].dlh, buf);
	if (fcnptr != NULL)
	    return fcnptr;
    }
    if (!NumStatic) {
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
	return (DL_FUNC) 0;
    else
	return CFunTab[mid].func;
    return (DL_FUNC) 0;
}


static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
    if ((path[0] != '/') && (path[0] != '\\') && (path[1] != ':')) {
	if (!getcwd(buf, MAX_PATH))
	    errorcall(call, "can't get working directory!\n");
	strcat(buf, "\\");
	strcat(buf, path);
    } else
	strcpy(buf, path);
}

        /* do_dynload implements the R-Interface for the */
        /* loading of shared libraries */

SEXP do_dynload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char  buf[MAX_PATH];

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	errorcall(call, "character argument expected\n");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    DeleteDLL(buf);
    if (!AddDLL(buf))
	errorcall(call, "unable to load shared library \"%s\":\n  %s\n",
		  buf, DLLerror);
    return R_NilValue;
}

SEXP do_dynunload(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char  buf[MAX_PATH];

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	errorcall(call, "character argument expected\n");
    GetFullDLLPath(call, buf, CHAR(STRING(CAR(args))[0]));
    if (!DeleteDLL(buf))
	errorcall(call, "dynamic library \"%s\" was not loaded\n", buf);
    return R_NilValue;
}
