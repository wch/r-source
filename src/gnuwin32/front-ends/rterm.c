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

#define NONAMELESSUNION
#include <windows.h>
#include <stdio.h>
#include <io.h> /* for isatty */
#include "Rversion.h"
#include "Startup.h"
#include "psignal.h"
#include "../getline/getline.h"

extern void cmdlineoptions(int, char **);
extern void readconsolecfg();
extern int initapp(int, char **);
extern void Rf_mainloop(void);
__declspec(dllimport) extern UImode CharacterMode;
__declspec(dllimport) extern int UserBreak;
__declspec(dllimport) extern int R_Interactive;
__declspec(dllimport) extern int R_HistorySize;
__declspec(dllimport) extern int R_RestoreHistory;
__declspec(dllimport) extern char *R_HistoryFile;

extern char *getDLLVersion();
extern void saveConsoleTitle();

static char Rversion[25];
char *getRVersion()
{
    sprintf(Rversion, "%s.%s", R_MAJOR, R_MINOR);
    return(Rversion);
}

static DWORD mainThreadId;

static void my_onintr()
{
  UserBreak = 1;
  PostThreadMessage(mainThreadId,0,0,0);
}

int AppMain (int argc, char **argv)
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
    signal(SIGBREAK, my_onintr);
    initapp(0, NULL);
    readconsolecfg();
    if(R_Interactive) {
	gl_hist_init(R_HistorySize, 1);
	if (R_RestoreHistory) gl_loadhistory(R_HistoryFile);
	saveConsoleTitle();
	SetConsoleTitle("Rterm");
    }
    Rf_mainloop();
    /* NOTREACHED */
    return 0;
}
