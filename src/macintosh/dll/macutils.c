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

#include "macutils.h"

static void CopyNamePart(StringPtr Name, ConstStr255Param fileName, int start)
{
    int end = fileName[0] + 1, nlen, pos;
    for (nlen = 0, pos = start; pos < end && fileName[pos] == ':'; 
	 nlen++, pos++)
	Name[nlen + 1] = ':';
    for (; pos < end && fileName[pos] != ':'; nlen++, pos++)
	Name[nlen + 1] = fileName[pos];
    Name[0] = nlen;
}

/* This function is an adaptation of the function FSpLocationFromPath in
   tclMacUtils.c in the Tcl 8.0 distribution */
OSErr FSMakeFSSpecFromPath(ConstStr255Param fileName, FSSpecPtr spec)
{
    Boolean isDir, wasAlias;
    int pos, end;
    OSErr err;
    Str255 Name;
    short vRefNum;
    long dirID;
  
    /* get the initial directory information and set up first path component */
    CopyNamePart(Name, fileName, 1);
    if (Name[0] < fileName[0] && Name[1] != ':') { /* absolute path */
	Name[0]++;
	Name[Name[0]] = ':';
	if ((err = FSMakeFSSpec(0, 0, Name, spec)) != noErr)
	    return err;
	if ((err = FSpGetDirectoryID(spec, &dirID, &isDir)) != noErr)
	    return err;
	if (! isDir)
	    return dirNFErr;
	vRefNum = spec->vRefNum;
	pos = Name[0] + 1;
	CopyNamePart(Name, fileName, pos);
    }
    else {
	dirID = 0;
	vRefNum = 0;
	pos = 1;
	isDir = true;
    }
  
    /* process remaining path parts */
    end = fileName[0] + 1;
    while (true) {
	if ((err = FSMakeFSSpec(vRefNum, dirID, Name[0] == 0 ? NULL : Name,
				spec)) != noErr ||
	    (err = ResolveAliasFile(spec, true, &isDir, &wasAlias)) != noErr)
	    return err;
	pos += Name[0];
	if (pos < end) {
	    if ((err = FSpGetDirectoryID(spec, &dirID, &isDir)) != noErr)
		return err;
	    if (! isDir)
		return dirNFErr;
	    vRefNum = spec->vRefNum;
	    CopyNamePart(Name, fileName, pos);
	}
	else
	    return noErr;
    }
}


