/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file shext.c
 *  Copyright (C) 2001  Guido Masarotto and Brian Ripley
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

/* 27/03/2000 win32-api needs this for ANSI compliance */
#define NONAMELESSUNION

#include <shlobj.h>

/* browse for a folder under the Desktop, return the path in the argument */
void selectfolder(char *folder)
{
    char buf[MAX_PATH];
    LPMALLOC g_pMalloc; 
    HWND hwnd=0;
    BROWSEINFO bi; 
    LPITEMIDLIST pidlDesktop;
    LPITEMIDLIST pidlBrowse;
 
    strcpy(folder, "");
    /* Get the shell's allocator. */
    if (!SUCCEEDED(SHGetMalloc(&g_pMalloc))) return; 
      
    /* Get the PIDL for the desktop. */
    if (!SUCCEEDED(SHGetSpecialFolderLocation(hwnd, CSIDL_DESKTOP, 
					      &pidlDesktop))) return; 
 
    bi.hwndOwner = hwnd; 
    bi.pidlRoot = pidlDesktop; 
    bi.pszDisplayName = buf; 
    bi.lpszTitle = "Choose a directory"; 
    bi.ulFlags = 0; 
    bi.lpfn = NULL; 
    bi.lParam = 0; 
 
    /* Browse for a folder and return its PIDL. */
    pidlBrowse = SHBrowseForFolder(&bi); 
    if (pidlBrowse != NULL) {
	SHGetPathFromIDList(pidlBrowse, folder); 
        g_pMalloc->lpVtbl->Free(g_pMalloc, pidlBrowse); 
    } 
    /* Clean up. */
    g_pMalloc->lpVtbl->Free(g_pMalloc, pidlDesktop); 
}
