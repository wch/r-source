/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file shext.c
 *  Copyright (C) 2001  Guido Masarotto and Brian Ripley
 *                2004  R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <shlobj.h>

int CALLBACK
InitBrowseCallbackProc( HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData )
{
    if (uMsg == BFFM_INITIALIZED)
	SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
    return(0);
}

/* browse for a folder under the Desktop, return the path in the argument */

void selectfolder(char *folder)
{
    char buf[MAX_PATH];
    LPMALLOC g_pMalloc;
    HWND hwnd=0;
    BROWSEINFO bi;
    LPITEMIDLIST pidlBrowse;

    /* Get the shell's allocator. */
    if (!SUCCEEDED(SHGetMalloc(&g_pMalloc))) return;

    bi.hwndOwner = hwnd;
    bi.pidlRoot = NULL;
    bi.pszDisplayName = buf;
    bi.lpszTitle = "Choose a directory";
    bi.ulFlags = BIF_RETURNONLYFSDIRS;
    bi.lpfn = (BFFCALLBACK) InitBrowseCallbackProc;
    bi.lParam = (int) folder;

    /* Browse for a folder and return its PIDL. */
    pidlBrowse = SHBrowseForFolder(&bi);
    if (pidlBrowse != NULL) {
	SHGetPathFromIDList(pidlBrowse, folder);
        g_pMalloc->lpVtbl->Free(g_pMalloc, pidlBrowse);
    }
}

int ShellGetPersonalDirectory(char *folder)  /* Folder is assumed to be at least MAX_PATH long */
{
    LPMALLOC g_pMalloc;
    LPITEMIDLIST pidlUser;
    int result;

    result = 0;

    /* Get the shell's allocator. */
    if (SUCCEEDED(SHGetMalloc(&g_pMalloc))) {

	/* Get the PIDL of the user's Directory. */
	if (SUCCEEDED(SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, &pidlUser))) {
	    if (SUCCEEDED(SHGetPathFromIDList(pidlUser, folder))) result = 1;
	    g_pMalloc->lpVtbl->Free(g_pMalloc, pidlUser);
	}
    }
    return(result);
}


static char RUser[MAX_PATH];
#include <winbase.h>
#include "shext.h"		/* for ShellGetPersonalDirectory */
extern void R_Suicide(char *s);

char * getRUser()
{
   /*
    * try R_USER then HOME then Windows homes then working directory
    */
    char *p, *q;

    if ((p = getenv("R_USER"))) {
	if(strlen(p) >= MAX_PATH) R_Suicide("Invalid R_USER");
	strcpy(RUser, p);
    } else if ((p = getenv("HOME"))) {
	if(strlen(p) >= MAX_PATH) R_Suicide("Invalid HOME");
	strcpy(RUser, p);
    } else if (ShellGetPersonalDirectory(RUser)) {
	/* nothing to do */;
    } else if ((p = getenv("HOMEDRIVE")) && (q = getenv("HOMEPATH"))) {
	if(strlen(p) >= MAX_PATH) R_Suicide("Invalid HOMEDRIVE");
	strcpy(RUser, p);
	if(strlen(RUser) + strlen(q) >= MAX_PATH)
	    R_Suicide("Invalid HOMEDRIVE+HOMEPATH");
	strcat(RUser, q);
    } else {
	GetCurrentDirectory(MAX_PATH, RUser);
    }
    p = RUser + (strlen(RUser) - 1);
    if (*p == '/' || *p == '\\') *p = '\0';
    return RUser;
}

