/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2019  R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <shlwapi.h> /* for PathFindOnPath */
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
extern int R_is_redirection_tty(int fd);

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

/* Used also by Rscript */
int AppMain(int argc, char **argv)
{
    if (R_is_redirection_tty(0) && !isatty(0) &&
        R_is_redirection_tty(1)) {
	/* RTerm is being run in a redirection tty (probably cygwin
	   or msys). Re-run RTerm with winpty, if available, to allow
	   line editing using Windows Console API. */
	int i, interactive;
	char winpty[MAX_PATH+1];

	interactive = 1;
	/* needs to be in sync with cmdlineoptions() */
	for(int i = 0; i< argc; i++) 
	    if (!strcmp(argv[i], "--ess"))
		interactive = 1;
	    else if (!strcmp(argv[i], "-f")) {
		interactive = 0;
		i++;
	    } else if (!strcmp(argv[i], "--file"))
		interactive = 0;
	    else if (!strcmp(argv[i], "-e")) {
		interactive = 0;
		i++;
	    }
	if (interactive && strcpy(winpty, "winpty.exe") &&
	    PathFindOnPath(winpty, NULL)) {

	    size_t len, pos;
	    int res;
	    char *cmd;

	    len = strlen(winpty) + 5; /* 4*quote, terminator */
	    for(i = 0; i < argc; i++)
		len += strlen(argv[i]) + 3; /* space, 2*quote */
	    cmd = (char *)malloc(len * sizeof(char));
	    if (!cmd) {
		fprintf(stderr, "Error: cannot allocate memory");
		exit(1);
	    }
	    pos = snprintf(cmd, len, "\"\"%s\"", winpty);
	    for(i = 0; i < argc; i++)
		pos += snprintf(cmd + pos, len - pos, " \"%s\"",
		                argv[i]);
	    strcat(cmd + pos, "\"");
	    /* Ignore Ctrl-C and let the child process handle it */
	    SetConsoleCtrlHandler(NULL, TRUE);
	    res = system(cmd);
	    free(cmd);
	    return res;
	}
	/* fall back to RTerm without support for line editing */
    }

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
