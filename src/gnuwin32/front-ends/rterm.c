/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--1999  R Development Core Team
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

#include <windows.h>
#include <stdio.h>
#include <io.h> /* for isatty */
#include "Rversion.h"
#include "Startup.h"
#define PSIGNAL
#include "psignal.h"

#define CharacterMode (*__imp_CharacterMode)
#define UserBreak     (*__imp_UserBreak)

extern void cmdlineoptions(int, char **);
extern void setup_term_ui(void);
extern void mainloop(void);
extern UImode CharacterMode;
extern int UserBreak;

extern char *getDLLVersion();

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
    setup_term_ui();
    mainloop();
    /* NOTREACHED */
    return 0;
}
