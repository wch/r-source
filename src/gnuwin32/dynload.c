/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2014 The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

/*  Dynamic Loading Support: See ../main/Rdynload.c and ../include/Rdynpriv.h
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <Defn.h>
#include <Rmath.h>
#include <direct.h>
#define WIN32_LEAN_AND_MEAN 1
/* Eventually #define _WIN32_WINNT 0x0502 for SetDllDirectoryA */
#include <windows.h>

/* SetDllDirectory is supported under XP SP1 and later.
   If called with a non-NULL argument it sets the argument to be
   the second item on the DLL search path (after the application
   launch directory).  This is removed if called with NULL.

   Prior to XP SP1 the second item was the current directory, but this
   has (by default, 'safe DLL search mode') been moved below the
   Windows dirs.  Using SetDllDirectory removes it altogether.

   We fudge this via the current directory in earlier systems.
 */
typedef BOOL (WINAPI *PSDD)(LPCTSTR);

int setDLLSearchPath(const char *path)
{
    int res = 0; /* failure */
    PSDD p = (PSDD) -1;
    static char wd[MAX_PATH] = "";  /* stored real current directory */

    // XP SP1 and later.
    if(p == (PSDD) -1)
	p = (PSDD) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
				  "SetDllDirectoryA");
    if(p) {
	res = p(path);
    } else { /* Windows 2000 */
	if(path) {
	    GetCurrentDirectory(MAX_PATH, wd);
	    SetCurrentDirectory(path);
	} else if (wd[0]) {
	    SetCurrentDirectory(wd);
	    wd[0] = '\0';
	}
    }
    return res;
}

#include <R_ext/Rdynload.h>
#include <Rdynpriv.h>


	/* Inserts the specified DLL at the head of the DLL list */
	/* Returns 1 if the library was successfully added */
	/* and returns 0 if the library table is full or */
	/* or if LoadLibrary fails for some reason. */

static void fixPath(char *path)
{
    char *p;
    for(p = path; *p != '\0'; p++) if(*p == '\\') *p = '/';
}

static HINSTANCE R_loadLibrary(const char *path, int asLocal, int now,
			       const char *search);
static DL_FUNC getRoutine(DllInfo *info, char const *name);
#ifdef CACHE_DLL_SYM
static void R_deleteCachedSymbols(DllInfo *dll);
#endif

static void R_getDLLError(char *buf, int len);
static void GetFullDLLPath(SEXP call, char *buf, const char *path);

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

#ifdef CACHE_DLL_SYM
    R_osDynSymbol->deleteCachedSymbols = R_deleteCachedSymbols;
    R_osDynSymbol->lookupCachedSymbol = Rf_lookupCachedSymbol;
#endif

    R_osDynSymbol->fixPath = fixPath;
    R_osDynSymbol->getFullDLLPath = GetFullDLLPath;
}

#ifdef CACHE_DLL_SYM
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
#endif

#ifndef _MCW_EM
_CRTIMP unsigned int __cdecl
_controlfp (unsigned int unNew, unsigned int unMask);
_CRTIMP unsigned int __cdecl _clearfp (void);
/* Control word masks for unMask */
#define	_MCW_EM		0x0008001F	/* Error masks */
#define	_MCW_IC		0x00040000	/* Infinity */
#define	_MCW_RC		0x00000300	/* Rounding */
#define	_MCW_PC		0x00030000	/* Precision */
#endif

HINSTANCE R_loadLibrary(const char *path, int asLocal, int now,
			const char *search)
{
    HINSTANCE tdlh;
    unsigned int dllcw, rcw;
    int useSearch = search && search[0];

    rcw = _controlfp(0,0) & ~_MCW_IC;  /* Infinity control is ignored */
    _clearfp();
    if(useSearch) setDLLSearchPath(search);
    tdlh = LoadLibrary(path);
    if(useSearch) setDLLSearchPath(NULL);
    dllcw = _controlfp(0,0) & ~_MCW_IC;
    if (dllcw != rcw) {
	_controlfp(rcw, _MCW_EM | _MCW_IC | _MCW_RC | _MCW_PC);
	if (LOGICAL(GetOption1(install("warn.FPU")))[0])
	    warning(_("DLL attempted to change FPU control word from %x to %x"),
		    rcw,dllcw);
    }
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
    LPSTR lpMsgBuf, p;
    char *q;
    FormatMessage(
	FORMAT_MESSAGE_ALLOCATE_BUFFER |
	FORMAT_MESSAGE_FROM_SYSTEM |
	FORMAT_MESSAGE_IGNORE_INSERTS,
	NULL,
	GetLastError(),
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	(LPSTR) &lpMsgBuf,
	0,
	NULL
	);
    strcpy(buf, "LoadLibrary failure:  ");
    q = buf + strlen(buf);
    /* It seems that Win 7 returns error messages with CRLF terminators */
    for (p = lpMsgBuf; *p; p++) if (*p != '\r') *q++ = *p;
    LocalFree(lpMsgBuf);
}

static void GetFullDLLPath(SEXP call, char *buf, const char *path)
{
    char *p;

    if ((path[0] != '/') && (path[0] != '\\') && (path[1] != ':')) {
	if (!getcwd(buf, MAX_PATH))
	    errorcall(call, _("cannot get working directory"));
	strcat(buf, "\\");
	strcat(buf, path);
    } else
	strcpy(buf, path);
    /* fix slashes to allow inconsistent usage later */
    for (p = buf; *p; p++) if (*p == '\\') *p = '/';
}
