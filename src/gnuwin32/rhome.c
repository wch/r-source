/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999  Guido Masarotto and Brian Ripley
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
#include <Rconfig.h>
#endif

#include <windows.h>
#include <string.h>		/* for strrchr(...) */
#include <stdio.h>
#include <ctype.h>
#include "Rversion.h"

static char rhomebuf[MAX_PATH];

#define GOBACKONESLASH \
  p = strrchr(rhomebuf,'\\'); \
  if (!p) { \
    MessageBox(NULL, "Installation problem", "Terminating", \
		MB_TASKMODAL | MB_ICONSTOP | MB_OK);\
   exit(1); \
  } \
  *p = '\0'

char *getRHOME()
{
    DWORD nc;
    char *p;
    int hasspace = 0;

    nc = GetModuleFileName(NULL, rhomebuf, MAX_PATH);
    GOBACKONESLASH;
    GOBACKONESLASH;
    /* make sure no spaces in path */
    for (p = rhomebuf; *p; p++) 
	if (isspace(*p)) { hasspace = 1; break; }
    if (hasspace)
	GetShortPathName(rhomebuf, rhomebuf, MAX_PATH);
    return (rhomebuf);
}

static char DLLversion[25];

char *getDLLVersion()
{
    sprintf(DLLversion, "%s.%s", R_MAJOR, R_MINOR);
    return (DLLversion);
}
