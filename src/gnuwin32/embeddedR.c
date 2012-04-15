/*  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2006 R Core Team
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

#include <config.h>
#include <Defn.h>
#include <Rembedded.h>

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <Rversion.h>
#include <R_ext/RStartup.h>
/* for askok and askyesnocancel */
#include "graphapp/ga.h"

/* for signal-handling code */
#include <psignal.h>

/* one way to allow user interrupts: called in ProcessEvents */
extern int UserBreak;

/* calls into the R DLL */
extern char *getDLLVersion(), *getRUser(), *get_R_HOME();
extern void R_DefParams(Rstart), R_SetParams(Rstart), R_setStartTime();
extern void ProcessEvents(void);
extern int R_ReplDLLdo1();


/* simple input, simple output */

/* This version blocks all events: a real one needs to call ProcessEvents
   frequently. See rterm.c and ../system.c for one approach using
   a separate thread for input.
*/
static int myReadConsole(const char *prompt, char *buf, int len,
			 int addtohistory)
{
    fputs(prompt, stdout);
    fflush(stdout);
    if(fgets(buf, len, stdin)) return 1;
    else return 0;
}

static void myWriteConsole(const char *buf, int len)
{
    printf("%s", buf);
}

static void myCallBack()
{
    /* called during i/o, eval, graphics in ProcessEvents */
}

static void myBusy(int which)
{
    /* set a busy cursor ... if which = 1, unset if which = 0 */
}

static void my_onintr(int sig)
{
    UserBreak = 1;
}

extern Rboolean R_LoadRconsole;

int Rf_initialize_R(int argc, char **argv)
{
    structRstart rp;
    Rstart Rp = &rp;
    char Rversion[25], *RHome;

    snprintf(Rversion, 25, "%s.%s", R_MAJOR, R_MINOR);
    if(strncmp(getDLLVersion(), Rversion, 25) != 0) {
	fprintf(stderr, "Error: R.DLL version does not match\n");
	exit(1);
    }

    R_setStartTime();
    R_DefParams(Rp);
    if((RHome = get_R_HOME()) == NULL) {
	fprintf(stderr,
		"R_HOME must be set in the environment or Registry\n");
	exit(2);
    }
    Rp->rhome = RHome;
    Rp->home = getRUser();
    Rp->CharacterMode = LinkDLL;
    Rp->ReadConsole = myReadConsole;
    Rp->WriteConsole = myWriteConsole;
    Rp->CallBack = myCallBack;
    Rp->ShowMessage = askok;
    Rp->YesNoCancel = askyesnocancel;
    Rp->Busy = myBusy;

    Rp->R_Quiet = TRUE;
    Rp->R_Interactive = TRUE;
    Rp->RestoreAction = SA_RESTORE;
    Rp->SaveAction = SA_NOSAVE;
    R_SetParams(Rp);
    R_set_command_line_arguments(argc, argv);

    FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));

    signal(SIGBREAK, my_onintr);
    GA_initapp(0, 0);
    R_LoadRconsole = FALSE;
    readconsolecfg();

    return 0;
}

int Rf_initEmbeddedR(int argc, char **argv)
{
    Rf_initialize_R(argc, argv);
    setup_Rmainloop();
    return(1);
}

/* use fatal !=0 for emergency bail out */
void Rf_endEmbeddedR(int fatal)
{
    R_RunExitFinalizers();
    CleanEd();
    R_CleanTempDir();
    if(!fatal){
	Rf_KillAllDevices();
	AllDevicesKilled = TRUE;
    }
    if(!fatal && R_CollectWarnings)
	PrintWarnings();	/* from device close and .Last */
    app_cleanup();
}
