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


/* This provides a table of built-in C and Fortran functions.
   We include this table, even when we have dlopen and friends.
   This is so that the functions are actually loaded at link time. */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "R_ext/Rdynpriv.h"

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#ifdef HAVE_DL_H
#include "hpdlfcn.c"
#define HAVE_DLFCN_H
#endif
#endif


#include "FFDecl.h"

static CFunTabEntry CFunTab[] =
{
#include "FFTab.h"
    {NULL, NULL}
};

#ifdef HAVE_DLFCN_H

static void *loadLibrary(const char *path, int asLocal, int now);
static void closeLibrary(void *handle);
static void deleteCachedSymbols(DllInfo *);
static DL_FUNC R_dlsym(DllInfo *info, char const *name);
static void getFullDLLPath(SEXP call, char *buf, char *path);
static DL_FUNC getBaseSymbol(const char *name);
static void getSystemError(char *buf, int len);

static int computeDLOpenFlag(int asLocal, int now);

void InitFunctionHashing()
{
#ifdef DL_SEARCH_PROG
    baseDLL.handle = dlopen(0, RTLD_NOW);
#endif

    R_osDynSymbol->loadLibrary = loadLibrary;
    R_osDynSymbol->dlsym = R_dlsym;
    R_osDynSymbol->closeLibrary = closeLibrary;
    R_osDynSymbol->getError = getSystemError;
    R_osDynSymbol->getBaseSymbol = getBaseSymbol;

    R_osDynSymbol->deleteCachedSymbols = deleteCachedSymbols;
    R_osDynSymbol->lookupCachedSymbol = Rf_lookupCachedSymbol;

    R_osDynSymbol->CFunTab = CFunTab;
    R_osDynSymbol->getFullDLLPath = getFullDLLPath;
}

static void getSystemError(char *buf, int len)
{
    strcpy(buf, dlerror());
}

static void *loadLibrary(const char *path, int asLocal, int now)
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
#ifdef Macintosh
    /* This goes in a different order than the Unix version. */
    for(i = 0; i < nCPFun; i++)
	if(!strcmp(CPFun[i].pkg, dll->name)) {
	    strcpy(CPFun[i].pkg, CPFun[nCPFun].pkg);
	    strcpy(CPFun[i].name, CPFun[nCPFun].name);
	    CPFun[i].func = CPFun[nCPFun--].func;
	}
#else /* Not Macintosh, so Unix */
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
#endif /* Macintosh */
#endif /* CACHE_DLL_SYM */
}


static DL_FUNC getBaseSymbol(const char *name)
{
#ifdef DL_SEARCH_PROG
    DL_FUNC fcnptr;

    fcnptr = R_osDynSymbol->dlsym(&baseDll, name);
    return(fcnptr);
#else
    int i;

    for(i = 0 ; R_osDynSymbol->CFunTab[i].name ; i++)
	if(!strcmp(name, R_osDynSymbol->CFunTab[i].name))
	    return R_osDynSymbol->CFunTab[i].func;

    return((DL_FUNC) NULL);
#endif
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


/*
  This is the system/OS-specific version for resolving a
  symbol in a shared library.
 */
static DL_FUNC R_dlsym(DllInfo *info, char const *name)
{
    return (DL_FUNC) dlsym(info->handle, name);
}


/*
  In the future, this will receive an additional argument
  which will specify the nature of the symbol expected by the
  caller, specifically whether it is for a .C(), .Call(),
  .Fortran(), .External(), generic, etc. invocation. This will
  reduce the pool of possible symbols in the case of a library
  that registers its routines.
 */



static void getFullDLLPath(SEXP call, char *buf, char *path)
{
#ifdef Macintosh
    if(path[0] != ':') {
	if(R_Home == NULL){
	    if (!getcwd(buf, PATH_MAX))
		errorcall(call, "can't get working directory!");
	    strcat(buf, path);
	}
	else
	    strcpy(buf,path);
    } else
	strcpy(buf, path);
    return;

#else /* Macintosh */

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
#endif
}


#ifndef Macintosh

#include "Runix.h"
#include <sys/types.h>
#include <sys/stat.h>

extern DL_FUNC ptr_X11DeviceDriver, ptr_dataentry, ptr_R_GetX11Image,
    ptr_R_loadhistory, ptr_R_savehistory;

void R_load_X11_shlib(void)
{
    char X11_DLL[PATH_MAX], buf[1000], *p;
    DllInfo dll = {(char *)NULL, (char*)NULL, (HINSTANCE) NULL,
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
    DllInfo dll = {(char *)NULL, (char*)NULL, (HINSTANCE)NULL,
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


#endif /* Macintosh */

#else /* no dyn.load support (end of `ifdef HAVE_DLFCN_H') */


#ifndef Macintosh

void R_load_X11_shlib()
{
    R_Suicide("no support to load X11 shared library in this R version");
}


void R_load_gnome_shlib()
{
    R_Suicide("no support to load gnome shared library in this R version");
}

#endif /* Macintosh */

#endif
