/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2002  R Development Core Team
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

/* 27/03/2000 win32-api needs this */
#define NONAMELESSUNION
#include <windows.h>
#include <stdio.h>
#include <config.h>
#include <Rversion.h>
#include <Startup.h>
/* for askok and askyesnocancel */
#include "graphapp/graphapp.h"

/* for signal-handling code */
#include <psignal.h>

/* one way to allow user interrupts: called in ProcessEvents */
#ifdef _MSC_VER
__declspec(dllimport) int UserBreak;
#else
#define UserBreak     (*_imp__UserBreak)
extern int UserBreak;
#endif

/* calls into the R DLL */
extern char *getDLLVersion();
extern void R_DefParams(Rstart);
extern void R_SetParams(Rstart);
extern void setup_term_ui(void);
extern void ProcessEvents(void);
extern void end_Rmainloop(void), R_ReplDLLinit(void);
extern int R_ReplDLLdo1();
extern void run_Rmainloop(void);


/* simple input, simple output */

/* This version blocks all events: a real one needs to call ProcessEvents
   frequently. See rterm.c and ../system.c for one approach using
   a separate thread for input */
int myReadConsole(char *prompt, char *buf, int len, int addtohistory)
{
    fputs(prompt, stdout);
    fflush(stdout);
    if(fgets(buf, len, stdin)) return 1;
    else return 0;
}

void myWriteConsole(char *buf, int len)
{
    printf("%s", buf);
}

void myCallBack()
{
    /* called during i/o, eval, graphics in ProcessEvents */
}

void myBusy(int which)
{
    /* set a busy cursor ... if which = 1, unset if which = 0 */
}

static void my_onintr(int sig)
{
    UserBreak = 1;
}

int main (int argc, char **argv)
{
    structRstart rp;
    Rstart Rp = &rp;
    char Rversion[25], RUser[MAX_PATH], RHome[MAX_PATH], *p;

    sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
    if(strcmp(getDLLVersion(), Rversion) != 0) {
	fprintf(stderr, "Error: R.DLL version does not match\n");
	exit(1);
    }

    R_DefParams(Rp);
    if(getenv("R_HOME")) {
	strcpy(RHome, getenv("R_HOME"));
    } else {
	fprintf(stderr, "R_HOME must be set\n");
	exit(1);
    }
    Rp->rhome = RHome;
/*
 * try R_USER then HOME then working directory
 */
    if (getenv("R_USER")) {
	strcpy(RUser, getenv("R_USER"));
    } else if (getenv("HOME")) {
	strcpy(RUser, getenv("HOME"));
    } else if (getenv("HOMEDIR")) {
	strcpy(RUser, getenv("HOMEDIR"));
	strcat(RUser, getenv("HOMEPATH"));
    } else
	GetCurrentDirectory(MAX_PATH, RUser);
    p = RUser + (strlen(RUser) - 1);
    if (*p == '/' || *p == '\\') *p = '\0';
    Rp->home = RUser;
    Rp->CharacterMode = LinkDLL;
    Rp->ReadConsole = myReadConsole;
    Rp->WriteConsole = myWriteConsole;
    Rp->CallBack = myCallBack;
    Rp->message = askok;
    Rp->yesnocancel = askyesnocancel;
    Rp->busy = myBusy;

    Rp->R_Quiet = TRUE;
    Rp->R_Interactive = FALSE;
    Rp->RestoreAction = SA_RESTORE;
    Rp->SaveAction = SA_NOSAVE;
    Rp->CommandLineArgs = NULL;
    Rp->NumCommandLineArgs = 0;
    /* Rp->nsize = 300000;
       Rp->vsize = 6e6; */
    R_SetParams(Rp); /* so R_ShowMessage is set */
    R_SizeFromEnv(Rp);
    R_SetParams(Rp);

    FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));

    signal(SIGBREAK, my_onintr);
    setup_term_ui();
    setup_Rmainloop();
#ifdef SIMPLE_CASE
    run_Rmainloop();
    end_Rmainloop();
#else
    R_ReplDLLinit();
    while(R_ReplDLLdo1() > 0) {
/* add user actions here if desired */
    }
/* only get here on EOF (not q()) */
    R_CleanUp(SA_DEFAULT, 0, 1);
#endif
    end_Rmainloop();
    return 0;
}
