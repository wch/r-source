/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2001 The R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "R_ext/Rdynpriv.h"

#include "../unix/Runix.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifndef HAVE_NO_SYMBOL_UNDERSCORE
# ifdef HAVE_ELF_H
#  define HAVE_NO_SYMBOL_UNDERSCORE
# endif
#endif

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#else
#ifdef HAVE_DL_H
#include "hpdlfcn.c"
#define HAVE_DLFCN_H
#endif
#endif

#if defined(HAVE_GNOME) && defined(HAVE_DLFCN_H)

static DL_FUNC Rdlsym(void *handle, char const *name)
{
    char buf[MAXIDSIZE+1];
#ifdef HAVE_NO_SYMBOL_UNDERSCORE
    sprintf(buf, "%s", name);
#else
    sprintf(buf, "_%s", name);
#endif
    return (DL_FUNC) dlsym(handle, buf);
}


extern DL_FUNC ptr_R_Suicide, ptr_R_ShowMessage, ptr_R_ReadConsole,
    ptr_R_WriteConsole, ptr_R_ResetConsole, ptr_R_FlushConsole,
    ptr_R_ClearerrConsole, ptr_R_Busy, ptr_R_CleanUp, ptr_R_ShowFiles,
    ptr_R_ChooseFile, ptr_gnome_start,
    ptr_GnomeDeviceDriver, ptr_GTKDeviceDriver,
    ptr_R_loadhistory, ptr_R_savehistory;


/* This is called too early to use moduleCdynload */
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
    ptr_R_Suicide = Rdlsym(&dll, "Rgnome_Suicide");
    if(!ptr_R_Suicide) Rstd_Suicide("Cannot load R_Suicide");
    ptr_R_ShowMessage = Rdlsym(&dll, "Rgnome_ShowMessage");
    if(!ptr_R_ShowMessage) R_Suicide("Cannot load R_ShowMessage");
    ptr_R_ReadConsole = Rdlsym(&dll, "Rgnome_ReadConsole");
    if(!ptr_R_ReadConsole) R_Suicide("Cannot load R_ReadConsole");
    ptr_R_WriteConsole = Rdlsym(&dll, "Rgnome_WriteConsole");
    if(!ptr_R_WriteConsole) R_Suicide("Cannot load R_WriteConsole");
    ptr_R_ResetConsole = Rdlsym(&dll, "Rgnome_ResetConsole");
    if(!ptr_R_ResetConsole) R_Suicide("Cannot load R_ResetConsole");
    ptr_R_FlushConsole = Rdlsym(&dll, "Rgnome_FlushConsole");
    if(!ptr_R_FlushConsole) R_Suicide("Cannot load R_FlushConsole");
    ptr_R_ClearerrConsole = Rdlsym(&dll, "Rgnome_ClearerrConsole");
    if(!ptr_R_ClearerrConsole) R_Suicide("Cannot load R_ClearerrConsole");
    ptr_R_Busy = Rdlsym(&dll, "Rgnome_Busy");
    if(!ptr_R_Busy) R_Suicide("Cannot load R_Busy");
    ptr_R_CleanUp = Rdlsym(&dll, "Rgnome_CleanUp");
    if(!ptr_R_CleanUp) R_Suicide("Cannot load R_CleanUp");
    ptr_R_ShowFiles = Rdlsym(&dll, "Rgnome_ShowFiles");
    if(!ptr_R_ShowFiles) R_Suicide("Cannot load R_ShowFiles");
    ptr_R_ChooseFile = Rdlsym(&dll, "Rgnome_ChooseFile");
    if(!ptr_R_ChooseFile) R_Suicide("Cannot load R_ChooseFile");
    ptr_gnome_start = Rdlsym(&dll, "gnome_start");
    if(!ptr_gnome_start) R_Suicide("Cannot load gnome_start");
    ptr_GTKDeviceDriver = Rdlsym(&dll, "GTKDeviceDriver");
    if(!ptr_GTKDeviceDriver) R_Suicide("Cannot load GTKDeviceDriver");
    ptr_R_loadhistory = Rdlsym(&dll, "Rgnome_loadhistory");
    if(!ptr_R_loadhistory) R_Suicide("Cannot load Rgnome_loadhsitoryr");
    ptr_R_savehistory = Rdlsym(&dll, "Rgnome_savehistory");
    if(!ptr_R_savehistory) R_Suicide("Cannot load Rgnome_savehistory");
/* Uncomment the next two lines to experiment with the gnome() device */
/*    ptr_GnomeDeviceDriver = Rdlsym(&dll, "GnomeDeviceDriver");
      if(!ptr_GnomeDeviceDriver) R_Suicide("Cannot load GnomeDeviceDriver");*/
}

#else

void R_load_gnome_shlib()
{
    R_Suicide("no support to load gnome shared library in this R version");
}

#endif
