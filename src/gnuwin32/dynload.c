/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2001 The R Development Core Team
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
#include <Defn.h>
#include <Rmath.h>
#include <direct.h>
#include <windows.h>

#include "R_ext/Rdynload.h"
#include "R_ext/Rdynpriv.h"

#include "FFDecl.h"

/* This provides a table of built-in C and Fortran functions.
   We include this table, even when we have dlopen and friends.
   This is so that the functions are actually loaded at link time. */

static CFunTabEntry CFunTab[] =
{
#include "FFTab.h"
    {NULL, NULL}
};



        /* Inserts the specified DLL at the head of the DLL list */
        /* Returns 1 if the library was successfully added */
        /* and returns 0 if the library table is full or */
        /* or if LoadLibrary fails for some reason. */

static void fixPath(char *path)
{
    char *p;
    for(p = path; *p != '\0'; p++) if(*p == '\\') *p = '/';
}

static HINSTANCE R_loadLibrary(const char *path, int asLocal, int now);
static DL_FUNC getRoutine(DllInfo *info, char const *name);
static void R_deleteCachedSymbols(DllInfo *dll);

static DL_FUNC getBaseSymbol(const char *name);

static void R_getDLLError(char *buf, int len);
static void GetFullDLLPath(SEXP call, char *buf, char *path);

static void closeLibrary(HINSTANCE handle)
{
    FreeLibrary(handle);
}

void InitFunctionHashing()
{
    R_osDynSymbol->loadLibrary = R_loadLibrary;
    R_osDynSymbol->dlsym = getRoutine;
    R_osDynSymbol->closeLibrary = closeLibrary;
    R_osDynSymbol->getError = R_getDLLError;
    R_osDynSymbol->getBaseSymbol = getBaseSymbol;

    R_osDynSymbol->deleteCachedSymbols = R_deleteCachedSymbols;
    R_osDynSymbol->lookupCachedSymbol = Rf_lookupCachedSymbol;

    R_osDynSymbol->CFunTab = CFunTab;

    R_osDynSymbol->fixPath = fixPath;
    R_osDynSymbol->getFullDLLPath = GetFullDLLPath;
}

static void R_deleteCachedSymbols(DllInfo *dll)
{
    int i;
    for(i = nCPFun - 1; i >= 0; i--)
	if(!strcmp(CPFun[i].pkg, dll->name)) {
	    if(i < nCPFun - 1) {
		strcpy(CPFun[i].name, CPFun[--nCPFun].name);
		strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
		CPFun[i].func = CPFun[nCPFun].func;
	    } else nCPFun--;
	}
}

HINSTANCE R_loadLibrary(const char *path, int asLocal, int now)
{
    HINSTANCE tdlh;

    tdlh = LoadLibrary(path);
    return(tdlh);
}

static DL_FUNC getRoutine(DllInfo *info, char const *name)
{
    DL_FUNC f;
    f = (DL_FUNC) GetProcAddress(info->handle, name);
    return(f);
}

static void R_getDLLError(char *buf, int len)
{
    LPVOID lpMsgBuf;
    FormatMessage(
	FORMAT_MESSAGE_ALLOCATE_BUFFER |
	FORMAT_MESSAGE_FROM_SYSTEM |
	FORMAT_MESSAGE_IGNORE_INSERTS,
	NULL,
	GetLastError(),
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	(LPTSTR) &lpMsgBuf,
	0,
	NULL
	);
    strcpy(buf, "LoadLibrary failure:  ");
    strcat(buf, lpMsgBuf);
    LocalFree(lpMsgBuf);
}


static DL_FUNC getBaseSymbol(const char *name)
{
    static int NumStatic = 0;
    int   mid, high, low, cmp;
    if (!NumStatic) {
        int i,j;
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
}

static void GetFullDLLPath(SEXP call, char *buf, char *path)
{
    char *p;

    if ((path[0] != '/') && (path[0] != '\\') && (path[1] != ':')) {
	if (!getcwd(buf, MAX_PATH))
	    errorcall(call, "can't get working directory!");
	strcat(buf, "\\");
	strcat(buf, path);
    } else
	strcpy(buf, path);
    /* fix slashes to allow inconsistent usage later */
    for (p = buf; *p; p++) if (*p == '\\') *p = '/';
}
