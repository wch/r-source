/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2009  R Core Team
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
 *  along with this program; if not,  a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <io.h> /* for isatty */
#include <Rversion.h>
#include <Startup.h>
#include <psignal.h>
#include "../getline/getline.h"

extern void cmdlineoptions(int, char **);
extern void readconsolecfg(void);
extern int GA_initapp(int, char **);
extern void Rf_mainloop(void);
__declspec(dllimport) extern UImode CharacterMode;
__declspec(dllimport) extern int UserBreak;
__declspec(dllimport) extern int R_Interactive;
__declspec(dllimport) extern int R_HistorySize;
__declspec(dllimport) extern int R_RestoreHistory;
__declspec(dllimport) extern char *R_HistoryFile;

extern char *getDLLVersion(void);
extern void saveConsoleTitle(void);
extern void R_gl_tab_set(void);

static char Rversion[25];
char *getRVersion(void)
{
    snprintf(Rversion, 25, "%s.%s", R_MAJOR, R_MINOR);
    return(Rversion);
}

static DWORD mainThreadId;

static void my_onintr(int nSig)
{
  UserBreak = 1;
  PostThreadMessage(mainThreadId,0,0,0);
}

int AppMain(int argc, char **argv)
{
    CharacterMode = RTerm;
    if(strcmp(getDLLVersion(), getRVersion()) != 0) {
	fprintf(stderr, "Error: R.DLL version does not match\n");
	exit(1);
    }
    if (isatty(0)) 
	FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
    cmdlineoptions(argc, argv);
    mainThreadId = GetCurrentThreadId() ;
    /* The following restores Ctrl-C handling if we were started from R.exe */
    SetConsoleCtrlHandler(NULL, FALSE);
    signal(SIGBREAK, my_onintr);
    GA_initapp(0, NULL);
    readconsolecfg();
    if(R_Interactive) {
	R_gl_tab_set();
	gl_hist_init(R_HistorySize, 1);
	if (R_RestoreHistory) gl_loadhistory(R_HistoryFile);
	saveConsoleTitle();
#ifdef _WIN64
	SetConsoleTitle("Rterm (64-bit)");
#else
	SetConsoleTitle("Rterm (32-bit)");
#endif
    }
    Rf_mainloop();
    /* NOTREACHED */
    return 0;
}
