/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 2011     The R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>


#if 0
typedef int (*R_SaveAsBitmap)(/* variable set of args */);
static R_SaveAsBitmap R_devCairo;

#include <dlfcn.h>
static int Load_Rcairo_Dll(void)
{
    char dllpath[PATH_MAX], *p = getenv("R_HOME"), *module = "cairo";
    static int initialized = 0;
    void *handle;

    if (initialized) return initialized;
    initialized = -1;
    
#ifdef R_ARCH
    snprintf(dllpath, PATH_MAX, "%s/library/grDevices/libs/%s/%s%s", 
	     p, R_ARCH, module, SHLIB_EXT);
#else
    snprintf(dllpath, PATH_MAX, "%s/library/grDevices/libs/%s%s", 
	     p, module, SHLIB_EXT);
#endif
    if((handle = dlopen(dllpath, RTLD_LOCAL))) {
	R_devCairo = dlsym(handle, "in_Cairo");
	if (!R_devCairo) {
	    warning("unable to find R_devCairo");
	} else initialized = 1;
    }
    return initialized;
}
#endif

#include <R_ext/Rdynload.h>
int R_cairoCdynload(int local, int now);

typedef SEXP (*R_cairo)(SEXP args);

static R_cairo R_devCairo;

static int Load_Rcairo_Dll(void)
{
    static int initialized = 0;
 
    if (initialized) return initialized;
    initialized = -1;

    int res = R_cairoCdynload(1, 1);
    if(!res) return initialized;
    R_devCairo = (R_cairo) R_FindSymbol("in_Cairo", "cairo", NULL);
    if (!R_devCairo) error("failed to load cairo DLL");
    initialized = 1;
    return initialized;
}


SEXP devCairo(SEXP args)
{
    if (Load_Rcairo_Dll() < 0) warning("failed to load cairo DLL");
    else (R_devCairo)(args);
    return R_NilValue;
}
