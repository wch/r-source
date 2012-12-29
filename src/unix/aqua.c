/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2012 The R Core Team
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


/* This file provides support for R.app, the OS X front end */

/*
   There's another one in src/main/systutils.c, ptr_CocoaSystem .
*/

/* This sets ptr_QuartzBackend as a symbol in this file */
#define IN_AQUA_C 1
#include <R_ext/QuartzDevice.h>


#ifdef OLD
/* This is no longer used as from R.app r6413 */
#include <Defn.h>

#include <R_ext/GraphicsEngine.h>
#include <R_ext/Rdynload.h>

/* Defined in R_ext/QuartzDevice.h.
   Called from Mac-GUI/RController.m, before packages are loaded.
   If this fails, it hangs R.app */

/* FIXME: this should not be allowed: we were requiring symbols in
   grDevices.dll */
QuartzFunctions_t *getQuartzFunctions(void) 
{
    QuartzFunctions_t *(*fn)(void);
    fn = (QuartzFunctions_t *(*)(void)) R_FindSymbol("getQuartzAPI", "grDevices", NULL);
    if (!fn) {
	SEXP call = lang2(install("loadNamespace"), mkString("grDevices"));
	PROTECT(call);
	eval(call, R_GlobalEnv);
	UNPROTECT(1);
	fn = (QuartzFunctions_t *(*)(void)) R_FindSymbol("getQuartzAPI", "grDevices", NULL);
	if (!fn) error("unable to get QuartzAPI");
    }
    return fn();
}
#endif

