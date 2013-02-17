/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2007  R Core Team
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

/* For AttachConsole: seems the MinGW headers are wrong and that
   requires XP or later, not 2000 or later.
   Mingw-w64 has it included unconditionally.
*/
/* Mingw-w64 defines this to be 0x0502 */
#define _WIN32_WINNT 0x0501
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <Rversion.h>
#include <Startup.h>
#include <stdlib.h>		/* for exit */

extern void cmdlineoptions(int, char **);
extern int setupui(void);
extern void Rf_mainloop(void);
__declspec(dllimport) extern UImode CharacterMode;
extern void GA_exitapp(void);

extern char *getDLLVersion(void);

static char Rversion[25];
char *getRVersion(void)
{
    snprintf(Rversion, 25, "%s.%s", R_MAJOR, R_MINOR);
    return(Rversion);
}

#include <wincon.h>
typedef BOOL (*AC)(DWORD);

int AppMain(int argc, char **argv)
{
    CharacterMode = RGui;
    if(strcmp(getDLLVersion(), getRVersion()) != 0) {
	MessageBox(0, "R.DLL version does not match", "Terminating",
		   MB_TASKMODAL | MB_ICONSTOP | MB_OK);
	exit(1);
    }
    cmdlineoptions(argc, argv);
    if (!setupui()) {
        MessageBox(0, "Error setting up console.  Try --vanilla option.",
                      "Terminating", MB_TASKMODAL | MB_ICONSTOP | MB_OK);
        GA_exitapp();
    }

/* C writes to stdout/stderr get set to the launching terminal (if
   there was one).  Needs XP, and works for C but not Fortran. */

    if (AttachConsole(ATTACH_PARENT_PROCESS))
    {
	freopen("CONIN$", "r", stdin);
	freopen("CONOUT$", "w", stdout);
	freopen("CONOUT$", "w", stderr);
    }

    Rf_mainloop();
    /* NOTREACHED */
    return 0;
}
