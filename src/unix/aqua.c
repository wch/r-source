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

#include <Defn.h>
#include <Rdynpriv.h>

#include "Runix.h"
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifndef HAVE_NO_SYMBOL_UNDERSCORE
# ifdef HAVE_ELF_H
#  define HAVE_NO_SYMBOL_UNDERSCORE
# endif
#endif

#ifdef __APPLE_CC__
# include "dlfcn-darwin.h"
# define HAVE_DYNAMIC_LOADING
#else
/* HP-UX 11.0 has dlfcn.h, but according to libtool as of Dec 2001
   this support is broken. So we force use of shlib even when dlfcn.h
   is available */
#ifdef __hpux
# ifdef HAVE_DL_H
#  include "hpdlfcn.h"
#  define HAVE_DYNAMIC_LOADING
# endif
#else
# ifdef HAVE_DLFCN_H
#  include <dlfcn.h>
#  define HAVE_DYNAMIC_LOADING
# endif
#endif
#endif /* __APPLE_CC__ */

#if defined(HAVE_AQUA) && defined(HAVE_DYNAMIC_LOADING)
#define __DEBUGGING__

#include <R_ext/eventloop.h>

#include <Carbon/Carbon.h>
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

extern DL_FUNC 	ptr_R_ReadConsole, ptr_R_WriteConsole, ptr_R_ResetConsole, 
                ptr_R_FlushConsole, ptr_R_ClearerrConsole, ptr_R_StartConsole, 
                ptr_R_ShowFiles, ptr_R_loadhistory,  ptr_R_savehistory,
                ptr_R_ChooseFile, ptr_R_CleanUp, ptr_R_ShowMessage, ptr_R_Suicide,
                ptr_R_Busy;


DL_FUNC ptr_do_wsbrowser, ptr_GetQuartzParameters, ptr_FocusOnConsole, 
        ptr_Raqua_Edit, ptr_do_dataentry, ptr_do_browsepkgs, ptr_do_datamanger,
        ptr_do_packagemanger, ptr_do_flushconsole, ptr_do_hsbrowser, ptr_InitAquaIO,
		ptr_RSetConsoleWidth;


void R_ProcessEvents(void);

/* #define AQUA_POLLED_EVENTS 1 */
 
#ifdef AQUA_POLLED_EVENTS 
static void (* otherPolledEventHandler)(void);
static void	Raqua_ProcessEvents2(void);
#endif

/* This is called too early to use moduleCdynload */
void R_load_aqua_shlib(void)
{
    char aqua_DLL[PATH_MAX], buf[1000], *p;
    void *handle;
    struct stat sb;

    p = getenv("R_HOME");
    if(!p) {
	sprintf(buf, "R_HOME was not set");
	R_Suicide(buf);
    }
    strcpy(aqua_DLL, p);
    strcat(aqua_DLL, "/modules/R_aqua");
    strcat(aqua_DLL, SHLIB_EXT); /* from config.h */
    if(stat(aqua_DLL, &sb))
	R_Suicide("Probably no AQUA support: the shared library was not found");
/* cannot use computeDLOpenFlag as warnings will crash R at this stage */
#ifdef RTLD_NOW
    handle = dlopen(aqua_DLL, RTLD_NOW);
#else
    handle = dlopen(aqua_DLL, 0);
#endif
    if(handle == NULL) {
	sprintf(buf, "The AQUA shared library could not be loaded.\n  The error was %s\n", dlerror());
	R_Suicide(buf);
    }

    ptr_R_Suicide = Rdlsym(handle, "Raqua_Suicide");
    if(!ptr_R_Suicide) Rstd_Suicide("Cannot load Raqua_Suicide");
    ptr_R_StartConsole = Rdlsym(handle, "Raqua_StartConsole");
    if(!ptr_R_StartConsole) R_Suicide("Cannot load R_StartConsole");
    ptr_R_ReadConsole = Rdlsym(handle, "Raqua_ReadConsole");
    if(!ptr_R_ReadConsole) R_Suicide("Cannot load R_ReadConsole");
    ptr_R_WriteConsole = Rdlsym(handle, "Raqua_WriteConsole");
    if(!ptr_R_WriteConsole) R_Suicide("Cannot load R_WriteConsole");
    ptr_R_ResetConsole = Rdlsym(handle, "Raqua_ResetConsole");
    if(!ptr_R_ResetConsole) R_Suicide("Cannot load R_ResetConsole");
    ptr_R_FlushConsole = Rdlsym(handle, "Raqua_FlushConsole");
    if(!ptr_R_FlushConsole) R_Suicide("Cannot load R_FlushConsole");
    ptr_R_ClearerrConsole = Rdlsym(handle, "Raqua_ClearerrConsole");
    if(!ptr_R_ClearerrConsole) R_Suicide("Cannot load R_ClearerrConsole");
    ptr_do_wsbrowser = Rdlsym(handle, "Raqua_do_wsbrowser");
    if(!ptr_do_wsbrowser) R_Suicide("Cannot load do_wsbrowser");
    ptr_R_ShowFiles = Rdlsym(handle, "Raqua_ShowFiles");
    if(!ptr_R_ShowFiles) R_Suicide("Cannot load Raqua_R_ShowFiles");
    ptr_R_loadhistory = Rdlsym(handle, "Raqua_loadhistory");
    if(!ptr_R_loadhistory) R_Suicide("Cannot load Raqua_loadhistory");
    ptr_R_savehistory = Rdlsym(handle, "Raqua_savehistory");
    if(!ptr_R_savehistory) R_Suicide("Cannot load Raqua_savehistory");
    ptr_R_ChooseFile = Rdlsym(handle, "Raqua_ChooseFile");
    if(!ptr_R_ChooseFile) R_Suicide("Cannot load Raqua_R_ChooseFile");
    ptr_GetQuartzParameters = Rdlsym(handle, "Raqua_GetQuartzParameters");
    if(!ptr_GetQuartzParameters) R_Suicide("Cannot load Raqua_GetQuartzParameters");
    ptr_FocusOnConsole =  Rdlsym(handle, "Raqua_FocusOnConsole");
    if(!ptr_FocusOnConsole) R_Suicide("Cannot load Raqua_FocusOnConsole");
	ptr_Raqua_Edit = Rdlsym(handle, "Raqua_Edit");
    if(!ptr_Raqua_Edit) R_Suicide("Cannot load Raqua_Edit");
    ptr_do_dataentry = Rdlsym(handle, "Raqua_dataentry");
    if(!ptr_do_dataentry) R_Suicide("Cannot load Raqua_dataentry");
    ptr_do_browsepkgs = Rdlsym(handle, "Raqua_browsepkgs");
    if(!ptr_do_browsepkgs) R_Suicide("Cannot load Raqua_browsepkgs");
    ptr_R_ShowMessage = Rdlsym(handle, "Raqua_ShowMessage");
    if(!ptr_R_ShowMessage) R_Suicide("Cannot load Raqua_ShowMessage");
    ptr_R_CleanUp = Rdlsym(handle, "Raqua_CleanUp");
    if(!ptr_R_CleanUp) R_Suicide("Cannot load Raqua_CleanUp");
    ptr_do_datamanger = Rdlsym(handle, "Raqua_datamanger");
    if(!ptr_do_datamanger) R_Suicide("Cannot load Raqua_datamanger");
    ptr_do_packagemanger = Rdlsym(handle, "Raqua_packagemanger");
    if(!ptr_do_packagemanger) R_Suicide("Cannot load Raqua_packagemanger");
    ptr_do_flushconsole = Rdlsym(handle, "Raqua_doflushconsole");
    if(!ptr_do_flushconsole) R_Suicide("Cannot load Raqua_doflushconsole");
    ptr_do_hsbrowser = Rdlsym(handle, "Raqua_helpsearchbrowser");
    if(!ptr_do_hsbrowser) R_Suicide("Cannot load Raqua_helpsearchbrowser");
    ptr_R_Busy = Rdlsym(handle, "Raqua_Busy");
    if(!ptr_R_Busy) R_Suicide("Cannot load Raqua_Busy");
    ptr_InitAquaIO = Rdlsym(handle, "InitAquaIO");
    if(!ptr_InitAquaIO) R_Suicide("Cannot load InitAquaIO");
    ptr_RSetConsoleWidth = Rdlsym(handle, "RSetConsoleWidth");
    if(!ptr_RSetConsoleWidth) R_Suicide("Cannot load RSetConsoleWidth");

#ifdef AQUA_POLLED_EVENTS 
    otherPolledEventHandler = R_PolledEvents;
    R_PolledEvents = Raqua_ProcessEvents2;  
#endif
}


SEXP do_wsbrowser(SEXP call, SEXP op, SEXP args, SEXP env)
{
 return(ptr_do_wsbrowser(call, op, args, env));
}

#if defined(HAVE_X11)
extern SEXP X11_do_dataentry(SEXP call, SEXP op, SEXP args, SEXP rho); /* from src/unix/X11.c */
#endif
extern Rboolean useaqua; /* from src/unix/system.c */

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

void InitAquaIO(void);
void InitAquaIO(void){
 ptr_InitAquaIO();
}

void RSetConsoleWidth(void);
void RSetConsoleWidth(void){
 ptr_RSetConsoleWidth();
}

void R_ProcessEvents(void)
{
    EventRef theEvent;
    EventRecord	outEvent;
    EventTargetRef theTarget;
    bool	conv = false;

    if(!useaqua){
      if (R_interrupts_pending)
       onintr();
      return;
    }
 
    theTarget = GetEventDispatcherTarget();
    if(CheckEventQueueForUserCancel())
      onintr();

    if(ReceiveNextEvent(0, NULL,kEventDurationNoWait,true,&theEvent)== noErr){       
         conv = ConvertEventRefToEventRecord(theEvent, &outEvent);
    
        if(conv && (outEvent.what == kHighLevelEvent))
            AEProcessAppleEvent(&outEvent);
   
        SendEventToEventTarget (theEvent, theTarget);
        ReleaseEvent(theEvent);
            
    }

}



#ifdef AQUA_POLLED_EVENTS 
extern WindowRef ConsoleWindow;     
static void	Raqua_ProcessEvents2(void)
{
    EventRef theEvent;
    EventRecord	outEvent;
    EventTargetRef theTarget = GetEventDispatcherTarget();
    bool	conv = false;
     ProcessSerialNumber ourPSN;
   
   if(otherPolledEventHandler)
      otherPolledEventHandler();

/*    if (GetCurrentProcess(&ourPSN) == noErr)
        (void)SetFrontProcess(&ourPSN);
*/
    if(CheckEventQueueForUserCancel())
       onintr();
     
 /*
 
    if(ReceiveNextEvent(0, NULL,kEventDurationNoWait,true,&theEvent)== noErr){
        conv = ConvertEventRefToEventRecord(theEvent, &outEvent);
    
        if(conv && (outEvent.what == kHighLevelEvent))
            AEProcessAppleEvent(&outEvent);
         
        SendEventToEventTarget (theEvent, theTarget);
        ReleaseEvent(theEvent);
            
    }
 */   
}
#endif

#else

void R_load_aqua_shlib()
{
    R_Suicide("no support to load aqua shared library in this R version");
}

#endif
