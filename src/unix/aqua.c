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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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


extern DL_FUNC 	ptr_R_ReadConsole, ptr_R_WriteConsole, ptr_R_ResetConsole, 
    ptr_R_FlushConsole, ptr_R_ClearerrConsole, ptr_R_StartConsole, 
    ptr_R_ShowFiles, ptr_R_loadhistory,  ptr_R_savehistory,
    ptr_R_ChooseFile, ptr_R_CleanUp, ptr_R_ShowMessage, ptr_R_Suicide,
    ptr_R_Busy;


DL_FUNC ptr_do_wsbrowser, ptr_GetQuartzParameters, 
    ptr_Raqua_Edit, ptr_do_dataentry, ptr_do_browsepkgs, ptr_do_datamanger,
    ptr_do_packagemanger, ptr_do_flushconsole, ptr_do_hsbrowser;

DL_FUNC ptr_R_ProcessEvents, ptr_CocoaInnerQuartzDevice, 
    ptr_CocoaGetQuartzParameters, ptr_CocoaSystem;

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
    return(ptr_do_flushconsole(call, op, args, env));
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
