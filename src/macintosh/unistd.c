/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file unistd.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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
 *
 *	File:		unistd.c
 *
 *	Content:	Interface file to standard UNIX-style entry points ...
 *
 *	NB:			This file implements some UNIX low level support.  These functions
 *				are not guaranteed to be 100% conformant.
 *
 */

#include <RCarbon.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <CarbonStdCLib.h>

#include <Errors.h>
#include <Files.h>
#include <LowMem.h>
#include <Processes.h>
#include <TextUtils.h> 
#include <MacTypes.h>	
#include <Devices.h>
 
#include <FullPath.h>

#define min(a,b) ((a) <= (b) ? (a) : (b))


char *getcwd(char * buf, int size);
int chdir(const char * path);

extern Boolean RunningOnCarbonX(void);

/* local typedefs */


static OSErr error;

#ifdef __MRC__
static char cwdir[256];
#endif

/*
 *	int chdir(const char *path)
 *
 *		Changes the current working directory (actually changes lowmem globals
 *		SFSaveDisk and CurDirStore which are used by open to open a file).
 *      Fixed to work on OS X when compiled with MRC
 */
int chdir(const char * path)
{
	WDPBRec			wdpb;
	Str255			ppath;
	OSErr			err = -1;

	if (path) {
		/* convert the c string into a pascal string */
		CopyCStringToPascal(path,ppath);
		
		
		if (ppath[ppath[0]] != ':')
			ppath[++ppath[0]] = ':';

		wdpb.ioNamePtr = ppath;
		wdpb.ioVRefNum = 0;
		wdpb.ioWDDirID = 0;
		err = PBHSetVolSync(&wdpb);
	}

	/* if we reach here we have an error */
	if (err != noErr)
		errno = err;
		
#ifdef __MRC__
    if( RunningOnCarbonX()) {
     ConvertHFSPathToUnixPath(path, (char *)&cwdir) ;
     bsd_chdir(cwdir);
    }
#endif

			
	return (err == noErr ? 0 : -1);
}

 
/*
 *	char *getcwd(char *buf, int size)
 *
 *		Returns the path to the current directory.
 */
char *getcwd(char * buf, int size)
{
    SInt16          vRefNum; 
    SInt32 			dirID;
    Handle			fullPath = NULL;
	short			fullPathLength;
	error = -1;

	if (size > 0 && buf) {

    error =  HGetVol(NULL,&vRefNum,&dirID);
	
	if(error != noErr) goto bad;
			
	error = GetFullPath(vRefNum, dirID, NULL, &fullPathLength, &fullPath);
	
	if(error != noErr) goto bad;
    
	HLock((Handle) fullPath);
    
    strncpy(buf,*fullPath,min(size,fullPathLength));
	
	buf[min(size,fullPathLength)] = '\0';
    HUnlock((Handle) fullPath);


     error = noErr;
	}

bad:
	if (error != noErr)
		errno = error;
	
	if(fullPath)
	 DisposeHandle(fullPath);

	return (error == noErr ? buf : NULL);
}
