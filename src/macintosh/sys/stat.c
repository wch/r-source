/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file devMacintosh.c
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
 *
 * Files stat.c, emulates stat.c with MPW compiler.
 * This file and stat.c have been originally written by
 * Dan Allen <danallen@nwlink.com>
 * Adapted for R by Stefano M. Iacus
 */

#include <RCarbon.h>

#include <mpw_stat.h>
#include "FullPath.h"

int stat(char *fileName,struct stat *statbuf)
{
	char *p,s[256];
	int i;
	OSErr	err;
	CInfoPBRec pb;

	p = fileName;
	i = 1;
	while(*p)
		s[i++] = *p++;
	s[0] = i-1;
	pb.hFileInfo.ioNamePtr = s;
	pb.hFileInfo.ioVRefNum = pb.hFileInfo.ioFDirIndex = pb.hFileInfo.ioDirID =
0;
	err = PBGetCatInfoSync(&pb);
	if (err) return -1;

	statbuf->st_dev = pb.hFileInfo.ioVRefNum;
	statbuf->st_ino = pb.hFileInfo.ioFRefNum;
	if (pb.dirInfo.ioFlAttrib & ioDirMask) {
		statbuf->st_mode = S_IFDIR;
		statbuf->st_nlink = pb.dirInfo.ioFDirIndex;
		statbuf->st_uid = pb.dirInfo.ioDrDirID;
		statbuf->st_gid = pb.dirInfo.ioDrParID;
		statbuf->st_rdev = pb.dirInfo.ioDrNmFls;
		statbuf->st_size = pb.dirInfo.ioDrNmFls;
		statbuf->st_blksize = pb.dirInfo.ioDrNmFls;
		statbuf->st_blocks = pb.dirInfo.ioDrNmFls;
		statbuf->st_atime = pb.dirInfo.ioDrBkDat - 2082844800; /* change date
offset to 1970.01.01 */
		statbuf->st_mtime = pb.dirInfo.ioDrMdDat - 2082844800; /* change date
offset to 1970.01.01 */
		statbuf->st_ctime = pb.dirInfo.ioDrCrDat - 2082844800; /* change date
offset to 1970.01.01 */
	}
	else {
		statbuf->st_mode = pb.hFileInfo.ioFlFndrInfo.fdType == 'APPL' ||
pb.hFileInfo.ioFlFndrInfo.fdType == 'MPST' ? S_IEXEC : S_IFREG;
		statbuf->st_nlink = 0;
		statbuf->st_uid = pb.hFileInfo.ioFlFndrInfo.fdType;
		statbuf->st_gid = pb.hFileInfo.ioFlFndrInfo.fdCreator;
		statbuf->st_rdev = pb.hFileInfo.ioFlRLgLen;
		statbuf->st_size = pb.hFileInfo.ioFlLgLen;
		statbuf->st_blksize = pb.hFileInfo.ioFlPyLen;
		statbuf->st_blocks = pb.hFileInfo.ioFlRPyLen;
		statbuf->st_atime = pb.hFileInfo.ioFlMdDat - 2082844800; /* change date
offset to 1970.01.01 */
		statbuf->st_mtime = pb.hFileInfo.ioFlMdDat - 2082844800; /* change date
offset to 1970.01.01 */
		statbuf->st_ctime = pb.hFileInfo.ioFlCrDat - 2082844800; /* change date
offset to 1970.01.01 */
	}
	return 0;
}

int fstat(int fd,struct stat *statbuf)
{
	char buf[512];

//	ioctl(fd,FIOFNAME,(long *) buf);
	return stat(buf,statbuf);
}

int mkdir_mac(const char *path){

 char path1[300];
 HFileParam		fpb;
 Str255			ppath;
 OSErr err;


  if (path) {
   if( strncmp("::",path,2) == 0  )
    strcpy(path1,&path[1]);
   else{  
    if(path[0] != ':')
     strcat(path1,":",path); 
    else
     strcpy(path1,path); 
   }
  
   CopyCStringToPascal(path1, ppath);
		
   fpb.ioNamePtr = ppath;
   fpb.ioVRefNum = 0;
   fpb.ioDirID = 0L;
   err = PBDirCreateSync((HParmBlkPtr)&fpb);
  }
  if(err = -48) /* dir exists */
   err = 0;
   
  return(err);
 
}



