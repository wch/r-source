/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2010  Guido Masarotto and Brian Ripley
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
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#undef sprintf

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>		/* for strrchr(...) */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>		/* for exit */

static char rhomebuf[MAX_PATH];

/* <MBCS-FIXME> We can't just use Rf_strchr as this is called
   from front-ends */
#define GOBACKONESLASH \
    p = strrchr(rhomebuf,'\\'); \
    if (!p) { \
	MessageBox(NULL, "Installation problem", "Terminating", \
		   MB_TASKMODAL | MB_ICONSTOP | MB_OK);\
	exit(1); \
    } \
    *p = '\0'

/* get R_HOME from the module path: used in RSetReg */
char *getRHOMElong(int m)
{
    char *p;

    GetModuleFileName(NULL, rhomebuf, MAX_PATH);
    for(int i=0; i < m; i++) {GOBACKONESLASH;}
    return (rhomebuf);
}

/* get no-spaces version of R_HOME from the module path: 
   used in Rgui, Rterm and Rcmd
*/
char *getRHOME(int m)
{
    char *p;
    int hasspace = 0;

    getRHOMElong(m);
    /* make sure no spaces in path */
    for (p = rhomebuf; *p; p++)
	if (isspace(*p)) { hasspace = 1; break; }
    if (hasspace)
	/* NOTE: this fails when short names are not enabled */
	GetShortPathName(rhomebuf, rhomebuf, MAX_PATH);
    return (rhomebuf);
}


/* get R_HOME from environment or registry: used in embedded apps */
char *get_R_HOME(void)
{
    LONG rc;
    HKEY hkey;
    DWORD keytype = REG_SZ, cbData = sizeof(rhomebuf);

    /* First try the C environment space */
    if(getenv("R_HOME")) {
	strncpy(rhomebuf, getenv("R_HOME"), MAX_PATH);
	return (rhomebuf);
    }

    /* Then the Windows API environment space */
    if (GetEnvironmentVariable ("R_HOME", rhomebuf, sizeof (rhomebuf)) > 0)
	return (rhomebuf);

    /* And then the registry */
    rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\R-core\\R", 0,
		      KEY_READ, &hkey);
    if (rc == ERROR_SUCCESS) {
	rc = RegQueryValueEx(hkey, "InstallPath", 0, &keytype,
			     (LPBYTE) rhomebuf, &cbData);
	RegCloseKey (hkey);
    } else return NULL;
    if (rc != ERROR_SUCCESS) return NULL;
    return rhomebuf;
}
