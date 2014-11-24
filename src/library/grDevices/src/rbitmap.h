/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 2000-2014     The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
typedef int (*R_SaveAsBitmap)(/* variable set of args */);
static R_SaveAsBitmap R_SaveAsPng, R_SaveAsJpeg, R_SaveAsBmp, R_SaveAsTIFF;

static int RbitmapAlreadyLoaded = 0;
static HINSTANCE hRbitmapDll;

static int Load_Rbitmap_Dll()
{
    if (!RbitmapAlreadyLoaded) {
	char szFullPath[PATH_MAX];
	strcpy(szFullPath, R_HomeDir());
	strcat(szFullPath, "\\library\\grDevices\\libs\\");
	strcat(szFullPath, R_ARCH);
	strcat(szFullPath, "\\Rbitmap.dll");
	if (((hRbitmapDll = LoadLibrary(szFullPath)) != NULL) &&
	    ((R_SaveAsPng=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsPng"))
	     != NULL) &&
	    ((R_SaveAsBmp=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsBmp"))
	     != NULL) &&
	    ((R_SaveAsJpeg=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsJpeg"))
	     != NULL) &&
	    ((R_SaveAsTIFF=
	      (R_SaveAsBitmap)GetProcAddress(hRbitmapDll, "R_SaveAsTIFF"))
	     != NULL)
	    ) {
	    RbitmapAlreadyLoaded = 1;
	} else {
	    if (hRbitmapDll != NULL) FreeLibrary(hRbitmapDll);
	    RbitmapAlreadyLoaded= -1;
	    char buf[1000];
	    snprintf(buf, 1000, "Unable to load '%s'", szFullPath);
	    R_ShowMessage(buf);
	}
    }
    return (RbitmapAlreadyLoaded > 0);
}
