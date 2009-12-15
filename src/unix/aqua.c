/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2008 The R Development Core Team
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


#if defined(HAVE_AQUA)

/* tell QuartzDevice to insert definitions for us (to maintain consistency) */
#define IN_AQUA_C 1

#include <R_ext/GraphicsEngine.h>
#include <R_ext/Rdynload.h>
#include <R_ext/QuartzDevice.h>

extern Rboolean useaqua; /* from src/unix/system.c */


DL_FUNC ptr_do_wsbrowser, ptr_GetQuartzParameters,
    ptr_do_dataentry, ptr_do_browsepkgs, ptr_do_datamanger,
    ptr_do_packagemanger, ptr_do_flushconsole, ptr_do_hsbrowser,
    ptr_do_selectlist;

DL_FUNC ptr_R_ProcessEvents, ptr_CocoaSystem;

int (*ptr_Raqua_CustomPrint)(const char *, SEXP);

static QuartzFunctions_t* qfn;

QuartzFunctions_t *getQuartzFunctions(void) {
    if (qfn) return qfn;
    {
	QuartzFunctions_t *(*fn)(void);
	fn = (QuartzFunctions_t *(*)(void)) R_FindSymbol("getQuartzAPI", "grDevices", NULL);
	if (!fn) {
	    /* we need to load grDevices - not sure if this is the best way, though ... */
	    eval(LCONS(install("library"),CONS(install("grDevices"),R_NilValue)),R_GlobalEnv);
	    fn = (QuartzFunctions_t *(*)(void)) R_FindSymbol("getQuartzAPI", "grDevices", NULL);
	    if (!fn) error(_("unable to load Quartz"));
	}
	return fn();
    }
}

void R_ProcessEvents(void);

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
    return(ptr_do_browsepkgs(call, op, args, env));
}


SEXP do_datamanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_datamanger(call, op, args, env));
}


SEXP do_hsbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_hsbrowser(call, op, args, env));
}

SEXP do_packagemanger(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_packagemanger(call, op, args, env));
}

SEXP do_flushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
	if(ptr_do_flushconsole)
		ptr_do_flushconsole(call, op, args, env);
	return R_NilValue;
}

SEXP do_selectlist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return(ptr_do_selectlist(call, op, args, env));
}

SEXP do_aqua_custom_print(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const void *vm;
    const char *ct;
    int cpr;
    SEXP rv, objType, obj;

    if (!ptr_Raqua_CustomPrint) return R_NilValue;

    checkArity(op, args);

    vm = vmaxget();

    objType = CAR(args); args = CDR(args);
    obj = CAR(args);

    if (!isString(objType) || LENGTH(objType)<1)
	errorcall(call, "invalid arguments");
    ct=CHAR(STRING_ELT(objType,0));
    cpr=ptr_Raqua_CustomPrint(ct, obj);

    /* FIXME: trying to store a pointer in an integer is wrong */
    PROTECT(rv=allocVector(INTSXP, 1));
    INTEGER(rv)[0]=cpr;

    vmaxset(vm);
    UNPROTECT(1);

    return rv;
}

void R_ProcessEvents(void)
{
    if (ptr_R_ProcessEvents)
	ptr_R_ProcessEvents();
    if (cpuLimit > 0.0 || elapsedLimit > 0.0) {
	double cpu, data[5];
	R_getProcTime(data);
	cpu = data[0] + data[1] + data[3] + data[4];
	if (elapsedLimit > 0.0 && data[2] > elapsedLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (elapsedLimit2 > 0.0 && data[2] > elapsedLimit2) {
		elapsedLimit2 = -1.0;
		error(_("reached session elapsed time limit"));
	    } else
		error(_("reached elapsed time limit"));
	}
	if (cpuLimit > 0.0 && cpu > cpuLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (cpuLimit2 > 0.0 && cpu > cpuLimit2) {
		cpuLimit2 = -1.0;
		error(_("reached session CPU time limit"));
	    } else
		error(_("reached CPU time limit"));
	}
    }
    if (R_interrupts_pending)
	onintr();
}
#endif
