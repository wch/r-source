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

#if defined(HAVE_X11) && defined(HAVE_DLFCN_H)

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


extern DL_FUNC ptr_X11DeviceDriver, ptr_dataentry, ptr_R_GetX11Image;

/* This is called too early to use moduleCdynload */
void R_load_X11_shlib(void)
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
    strcat(X11_DLL, "/modules/R_X11.");
    strcat(X11_DLL, SHLIB_EXT); /* from config.h */
    if(stat(X11_DLL, &sb))
	R_Suicide("Probably no X11 support: the shared library was not found");
#ifdef RTLD_NOW
    handle = dlopen(X11_DLL, RTLD_NOW);
#else
    handle = dlopen(X11_DLL, 0);
#endif
    if(handle == NULL) {
	sprintf(buf, "The X11 shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }
    ptr_X11DeviceDriver = Rdlsym(handle, "X11DeviceDriver");
    if(!ptr_X11DeviceDriver) R_Suicide("Cannot load X11DeviceDriver");
    ptr_dataentry = Rdlsym(handle, "RX11_dataentry");
    if(!ptr_dataentry) R_Suicide("Cannot load do_dataentry");
    ptr_R_GetX11Image = Rdlsym(handle, "R_GetX11Image");
    if(!ptr_R_GetX11Image) R_Suicide("Cannot load R_GetX11Image");
}

#else

void R_load_X11_shlib()
{
    R_Suicide("no support to load X11 shared library in this R version");
}

#endif
