/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1996-2000  Luke Tierney, Bob Stine, and Steve Majewski
 *                2001  Stefano Iacus and the R core team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stddef.h>
#include <string.h>
#include <dlfcn.h>
#include <stdio.h>

#include <CodeFragments.h>
#include "macutils.h"

static char errbuf[512];

/* Minimal emulation of SysVR4-ELF dynamic loading routines for the Macintosh.
 * Based on code by Bob Stine as Modified by Steve Majewski. */
/* Carbonized by Stefano M.Iacus */

void *dlopen(const char *name, int mode)
{
    FSSpec fileSpec;
    Str255 errName, libName;
    OSErr err;
    Ptr mainAddr;
    CFragConnectionID connID;
    
    /* Build a file spec record for GetDiskFragment */
    if (strlen(name) < 254)
	strcpy((char *) libName, name);
    else {
	sprintf(errbuf, "library name too long");
	return NULL;
    }
#if ! TARGET_API_MAC_CARBON	
    CtoPstr((char *) libName);
#else
    CopyCStringToPascal((char *)libName,libName);
#endif
    err = FSMakeFSSpecFromPath((ConstStr255Param) libName, &fileSpec);
    if (err != noErr) {
	sprintf(errbuf, "error code %d creating file spec for library %s",
		err, name);
	return NULL;
    }

    /* Open the fragment (will not add another copy if loaded, though gives
       new ID) */
    err = GetDiskFragment(&fileSpec, 0, kCFragGoesToEOF, 0, kLoadCFrag,
			  &connID, &mainAddr, errName);
    if (err == noErr)
	return (void *) connID;
    else {
#if ! TARGET_API_MAC_CARBON	
	PtoCstr(errName);
#else
	CopyPascalStringToC(errName,(char *)errName);
#endif
	sprintf(errbuf, "error code %d getting disk fragment %s for library %s",
		err, errName, name);
	return NULL;
    }
}

/* This version does not handle NULL as the library for looking in the
   executable. It also does not check the symbol class. */
/* Carbonized by Stefano M.Iacus */
void *dlsym(void *lib, const char *name)
{
    CFragConnectionID connID = (CFragConnectionID) lib;
    OSErr err;
    Ptr symAddr;
    CFragSymbolClass symClass;
    Str255 symName;
     
    if (strlen(name) < 254)
	strcpy((char *) symName, name);
    else {
	sprintf(errbuf, "symbol name too long");
	return NULL;
    }
#if ! TARGET_API_MAC_CARBON	
    CtoPstr((char *) symName);
#else
    CopyCStringToPascal((char *)symName,symName);
#endif
    err = FindSymbol(connID, symName, &symAddr, &symClass);
    if (err == noErr)
	return (void *) symAddr;
    else {
	sprintf(errbuf, "error code %d looking up symbol %s", err, name);
	return NULL;
    }
}

int dlclose(void *lib)
{
    CFragConnectionID connID = (CFragConnectionID) lib;
    OSErr err;

    err = CloseConnection(&connID);
    if (err == noErr)
	return 0;
    else {
	sprintf(errbuf, "error code %d closing library", err);
	return -1;
    }
}

char *dlerror()
{
    return errbuf;
}
