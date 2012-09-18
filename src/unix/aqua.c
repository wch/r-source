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

#include <Defn.h>

#include "Runix.h"
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

/* tell QuartzDevice.h to insert definitions for us (to maintain consistency) */
#define IN_AQUA_C 1

#include <R_ext/GraphicsEngine.h>
#include <R_ext/Rdynload.h>
#include <R_ext/QuartzDevice.h>

extern Rboolean useaqua; /* from src/unix/system.c */


DL_FUNC ptr_do_wsbrowser, ptr_GetQuartzParameters,
    ptr_do_dataentry, ptr_do_browsepkgs, ptr_do_datamanger,
    ptr_do_packagemanger, ptr_do_hsbrowser, ptr_do_selectlist;

// R.app used to set this, so keep it for now in case it does.
DL_FUNC ptr_do_flushconsole;

/* And from Rinterface.h: */
extern void (*ptr_R_FlushConsole)(void);


/* called from Mac-GUI/RController.m, before packages are loaded.
   If this fails, it hangs R.app */
QuartzFunctions_t *getQuartzFunctions(void) 
{
    /* presumably this was intended to cache the result.
       But,
       - it was never used
       - it would be unsafe as the namespace could be unloaded/reloaded
    static QuartzFunctions_t* qfn;
    if (qfn) return qfn;
    */

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


SEXP do_wsbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_wsbrowser(call, op, args, env));
}

#if defined(HAVE_X11)
extern SEXP X11_do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho); /* from src/unix/X11.c */
#endif

SEXP do_dataentry(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(useaqua)
	return(ptr_do_dataentry(call, op, args, env));
#if defined(HAVE_X11)
    else
	return(X11_do_dataentry(call, op, args, env));
#endif
}

SEXP do_browsepkgs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return ptr_do_browsepkgs(call, op, args, env);
}


SEXP do_datamanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return ptr_do_datamanger(call, op, args, env);
}


SEXP do_hsbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return ptr_do_hsbrowser(call, op, args, env);
}

SEXP do_packagemanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return ptr_do_packagemanger(call, op, args, env);
}

SEXP do_flushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if(ptr_R_FlushConsole) ptr_R_FlushConsole();
    else if(ptr_do_flushconsole) // but the args are unused in R.app.
	ptr_do_flushconsole(call, op, args, env);
    return R_NilValue;
}

SEXP do_selectlist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_selectlist(call, op, args, env));
}

// to be set by R.app
int (*ptr_Raqua_CustomPrint)(const char *, SEXP);

SEXP do_aqua_custom_print(SEXP call, SEXP op, SEXP args, SEXP env)
{
    if (!ptr_Raqua_CustomPrint) return R_NilValue;

    checkArity(op, args);
    SEXP objType = CAR(args), obj = CADR(args);

    if (!isString(objType) || LENGTH(objType) < 1)
	errorcall(call, "invalid arguments");
    const char *ct = CHAR(STRING_ELT(objType, 0));
    int cpr = ptr_Raqua_CustomPrint(ct, obj);

    return ScalarInteger(cpr);
}

