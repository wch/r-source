/* 
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2004 The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
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

#include <Rdevices.h>
#include <R_ext/GraphicsDevice.h> 
 
extern Rboolean useaqua; /* from src/unix/system.c */


DL_FUNC ptr_do_wsbrowser, ptr_GetQuartzParameters, 
    ptr_do_dataentry, ptr_do_browsepkgs, ptr_do_datamanger,
    ptr_do_packagemanger, ptr_do_flushconsole, ptr_do_hsbrowser,
    ptr_do_selectlist;

DL_FUNC ptr_R_ProcessEvents, ptr_CocoaInnerQuartzDevice, 
    ptr_CocoaGetQuartzParameters, ptr_CocoaSystem;

int (*ptr_Raqua_CustomPrint)(char *, SEXP);

Rboolean CocoaInnerQuartzDevice(NewDevDesc *dd,char *display,
				double width,double height,
				double pointsize,char *family,
				Rboolean antialias,
				Rboolean autorefresh,int quartzpos,
				int bg){
    return (Rboolean)ptr_CocoaInnerQuartzDevice(dd, display,
						width, height,
						pointsize, family,
						antialias,
						autorefresh, quartzpos,
						bg);
}

void CocoaGetQuartzParameters(double *width, double *height, double *ps, 
			      char *family, Rboolean *antialias, 
			      Rboolean *autorefresh, int *quartzpos){
    ptr_CocoaGetQuartzParameters(width, height, ps, family, antialias,
				 autorefresh, quartzpos);
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
    char *vm;
    char *ct;
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

    PROTECT(rv=allocVector(INTSXP, 1));
    INTEGER(rv)[0]=cpr;

    vmaxset(vm);
    UNPROTECT(1);

    return rv;
}

void R_ProcessEvents(void)
{
    if(!useaqua){
	if (R_interrupts_pending)
	    onintr();
	return;
    } else
	ptr_R_ProcessEvents();
}
#endif
