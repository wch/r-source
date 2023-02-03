/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file shext.c
 *  Copyright (C) 2001  Guido Masarotto and Brian Ripley
 *                2004-2023  R Core Team
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

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <shlobj.h>
#include <knownfolders.h>
#include <stdlib.h>

static char *ShellGetPersonalDirectory(void)
{
    wchar_t *wpath;
    char *result = NULL;

    /* CSIDL_PERSONAL */
    if (SHGetKnownFolderPath(&FOLDERID_Documents, KF_FLAG_CREATE, NULL,
                             &wpath) == S_OK) {
	size_t needed = wcstombs(NULL, wpath, 0);
	if (needed != (size_t)-1) {
	    result = (char *)malloc(needed + 1);
	    if (result)
		/* NOTE: some characters may not be representable */
		wcstombs(result, wpath, needed + 1);
	}
    }
    CoTaskMemFree(wpath);
    return result;
}

#include <winbase.h>
extern void R_Suicide(char *s);

/* Returns a result to be freed by freeRUser(). Previously, pointer to a static
   fixed-size buffer has been returned, so older embedding applications would
   not know to free the result, but they would probably call this function only
   once. */
char *getRUser(void)
{
    /*
     * try R_USER then HOME then Windows homes then working directory
     */
    char *p, *q;
    char *RUser = NULL;

    if ((p = getenv("R_USER")) || (p = getenv("HOME"))) {
	RUser = (char *)malloc(strlen(p) + 1);
	if (RUser)
	    strcpy(RUser, p);
    } else if ((RUser = ShellGetPersonalDirectory())) {
	/* nothing to do */;
    } else if ((p = getenv("HOMEDRIVE")) && (q = getenv("HOMEPATH"))) {
	RUser = (char *)malloc(strlen(p) + strlen(q) + 1);
	strcpy(RUser, p);
	strcat(RUser, q);
    } else {
	DWORD res = GetCurrentDirectory(0, NULL);
	if (res) {
	    RUser = (char *)malloc(res);
	    if (!GetCurrentDirectory(res, RUser)) {
		free(RUser);
		RUser = NULL;
	    }
	}
    }
    if (!RUser)
	R_Suicide("Cannot determine R user directory");

    p = RUser + (strlen(RUser) - 1);
    /* remove trailing file separator(s), unless root directory on a drive */
    while (p > RUser && (*p == '/' || *p == '\\')
           && (p > RUser+2 || *(p-1) != ':'))  *p-- = '\0';
    return RUser;
}

void freeRUser(char *s)
{
    free(s);
}

