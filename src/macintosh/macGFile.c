/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *
 *  This file was originally written by: Wing Kwong (Tiki), WAN 3/2/99
 *  This file is used some small routines which is used to link with the internal R.
 *
 *  Original file was:
 *
 *	WASTE Demo Project:
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */

#include <stdio.h>
#include <fp.h> /* Jago */
#include <Quickdraw.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"
#include "Graphics.h"
#include "RIntf.h"
#include <Rdevices.h>

OSErr  doWritePictData(WindowPtr windowPtr,SInt16 tempFileRefNum);
OSErr  doCopyAppNameResource(WindowPtr windowPtr);
OSErr  doCopyGraResource(ResType, SInt16, SInt16, SInt16);
OSErr  doWriteFile(WindowPtr windowPtr);
extern pascal	OSErr	FSpGetFullPath (const FSSpec*, short*, Handle*);

extern Graphic_Ref gGReference[MAX_NUM_G_WIN + 1];
extern SInt16 gAppResFileRefNum;
SInt16  ConsolefileRefNum;
FSSpec  ConsolefileFSSpec;
char    InitFile[256];

OSErr  doRSave(Boolean *haveCancel)
{
    FILE *fp ;
    OSErr err;

    if (strlen(InitFile) == 0) {
	err = doRSaveAs(haveCancel);
    } else {
	*haveCancel = false;
	err = noErr;

	fp = fopen(InitFile, "wb"); /* binary file */
	if (!fp)
	    error("can't save data -- unable to open ./%s\n", InitFile);
	if (HASHTAB(R_GlobalEnv) != R_NilValue)
	    R_SaveToFile(HASHTAB(R_GlobalEnv), fp, 0);
	else
	    R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
	fclose(fp);

    }
    return err;
}

/*
   Based on previous doRSaveAs of Ross Ihaka.
   Now handles correctly XDR. Jago Nov 2000 (Stefano M. Iacus)
*/

OSErr  doRSaveAs(Boolean *haveCancel) {
    StandardFileReply fileReply;
    OSType fileType;
    OSErr osError = 0;
    FILE *fp ;
    SInt16 pathLen;
    Handle pathName;
    char path[FILENAME_MAX], cur_path[FILENAME_MAX];

    StandardPutFile("\pSave as:","\p.RData", &fileReply);
    *haveCancel = !(fileReply.sfGood);
    if(fileReply.sfGood) {
	if(!(fileReply.sfReplacing)) {
	    fileType = 'ROBJ';
	    osError = FSpCreate(&fileReply.sfFile, R_ID, fileType,
				smSystemScript);
	    if(osError != noErr)
		return(osError);
	}


	FSpGetFullPath(&fileReply.sfFile, &pathLen, &pathName);
	HLock((Handle) pathName);
	strncpy(InitFile, *pathName, pathLen);
	InitFile[pathLen] = '\0';
	HUnlock((Handle) pathName);


	fp = fopen(InitFile, "wb"); /* binary file */
	if (!fp)
	    error("can't save data -- unable to open ./%s\n", InitFile);
	if (HASHTAB(R_GlobalEnv) != R_NilValue)
	    R_SaveToFile(HASHTAB(R_GlobalEnv), fp, 0);
	else
	    R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
	fclose(fp);
    }

    return(osError);
}

/* ××××××××××××××××××××× doSaveCommand
*/
OSErr  doSaveGraCommand(void)
{
    WindowPtr windowPtr;
    SInt16 WinIndex;
    OSErr osError = 0;

    windowPtr = FrontWindow();
    WinIndex = isGraphicWindow(windowPtr);

    if(gGReference[WinIndex].fileRefNum)
	osError = doWriteFile(windowPtr);
    else
	osError = doSaveAsGraCommand();

    return(osError);
}

/* ××××××××××××××××××× doSaveAsCommand
*/
OSErr  doSaveAsGraCommand(void)
{
    WindowPtr windowPtr;
    SInt16 WinIndex;
    StandardFileReply fileReply;
    OSType fileType;
    SInt16 fileRefNum;
    OSErr osError = 0;

    windowPtr = FrontWindow();
    WinIndex = isGraphicWindow(windowPtr);

    StandardPutFile("\pSave as:","\pR Picture",&fileReply);

    if(fileReply.sfGood) {
	if(!(fileReply.sfReplacing)) {
	    fileType = 'PICT';
	    osError = FSpCreate(&fileReply.sfFile, 'ABFF', fileType,
				smSystemScript);
	    if(osError != noErr)
		return(osError);
	}

	gGReference[WinIndex].fileFSSpec = fileReply.sfFile;

	if(gGReference[WinIndex].fileRefNum != 0) {
	    osError = FSClose(gGReference[WinIndex].fileRefNum);
	    gGReference[WinIndex].fileRefNum = 0;
	}

	if(osError == noErr)
	    osError = FSpOpenDF(&gGReference[WinIndex].fileFSSpec,fsRdWrPerm,&fileRefNum);

	if(osError == noErr) {
	    gGReference[WinIndex].fileRefNum = fileRefNum;
	    osError = doWriteFile(windowPtr);
	}
    }

    return(osError);
}

/* ××××××××××××××××××××××× doWritePictData
*/
OSErr  doWritePictData(WindowPtr windowPtr,SInt16 tempFileRefNum)
{
    PicHandle pictureHdl=NULL;
    SInt32 numberOfBytes, dummyData;
    SInt16 volRefNum, WinIndex;
    OSErr osError;
    DevDesc *dd;
    GrafPtr picPort;

    WinIndex = isGraphicWindow(windowPtr);
    SetPort(windowPtr);
    HLock((Handle) pictureHdl);
    dd = (DevDesc*)gGReference[WinIndex].devdesc;
    pictureHdl = OpenPicture(&(windowPtr->portRect));
    GetPort(&picPort);
    playDisplayList(dd);
    SetPort(picPort);
    ClosePicture();
    HUnlock((Handle) pictureHdl);

    numberOfBytes = 512;
    dummyData = 0;

    osError = SetFPos(tempFileRefNum,fsFromStart,0);

    if(osError == noErr)
	osError = FSWrite(tempFileRefNum,&numberOfBytes,&dummyData);

    numberOfBytes = GetHandleSize((Handle)pictureHdl);

    if(osError == noErr) {
	HLock((Handle)pictureHdl);
	osError = FSWrite(tempFileRefNum,&numberOfBytes,*pictureHdl);
	HUnlock((Handle)pictureHdl);
    }

    if(osError == noErr)
	osError = SetEOF(tempFileRefNum,512 + numberOfBytes);
    if(osError == noErr)
	osError = GetVRefNum(tempFileRefNum,&volRefNum);
    if(osError == noErr)
	osError = FlushVol(NULL,volRefNum);


    return(osError);
}

/* ×××××××××××××××× doCopyAppNameResource
*/
OSErr  doCopyAppNameResource(WindowPtr windowPtr)
{

    OSType fileType;
    OSErr osError;
    SInt16 fileRefNum, WinIndex;


    WinIndex = isGraphicWindow(windowPtr);
    fileType = 'PICT';

    FSpCreateResFile(&(gGReference[WinIndex].fileFSSpec),'ABFF',fileType,smSystemScript);

    osError = ResError();
    if(osError == noErr)
	fileRefNum = FSpOpenResFile(&gGReference[WinIndex].fileFSSpec,
				    fsRdWrPerm);

    if(fileRefNum > 0)
	osError = doCopyGraResource('STR ', -16396, gAppResFileRefNum,
				    fileRefNum);
    else
	osError = ResError();

    if(osError == noErr)
	CloseResFile(fileRefNum);

    osError = ResError();
    return(osError);
}

/* ××××××××××××××××××××× doCopyGraResource
*/
OSErr  doCopyGraResource(ResType resourceType, SInt16 resourceID,
 SInt16 sourceFileRefNum, SInt16 destFileRefNum)
{
    Handle	sourceResourceHdl;
    Str255	sourceResourceName;
    ResType	ignoredType;
    SInt16	ignoredID;

    UseResFile(sourceFileRefNum);

    sourceResourceHdl = GetResource(resourceType,resourceID);

    if(sourceResourceHdl != NULL) {
	GetResInfo(sourceResourceHdl, &ignoredID, &ignoredType,
		   sourceResourceName);
	DetachResource(sourceResourceHdl);
	UseResFile(destFileRefNum);
	AddResource(sourceResourceHdl, resourceType, resourceID,
		    sourceResourceName);
	if(ResError() == noErr)
	    UpdateResFile(destFileRefNum);
    }

    ReleaseResource(sourceResourceHdl);

    return(ResError());
}

/* ××××××××××××××××××××××××× doWriteFile
*/
OSErr  doWriteFile(WindowPtr windowPtr)
{

    FSSpec fileSpecActual, fileSpecTemp;
    UInt32 currentTime;
    Str255 tempFileName;
    SInt16 tempFileVolNum, tempFileRefNum, WinIndex;
    SInt32 tempFileDirID;
    OSErr osError;

    WinIndex = isGraphicWindow(windowPtr);
    fileSpecActual = gGReference[WinIndex].fileFSSpec;

    GetDateTime(&currentTime);
    NumToString((SInt32) currentTime,tempFileName);

    osError = FindFolder(fileSpecActual.vRefNum, kTemporaryFolderType,
			 kCreateFolder, &tempFileVolNum,&tempFileDirID);
    if(osError == noErr)
	osError = FSMakeFSSpec(tempFileVolNum, tempFileDirID, tempFileName,
			       &fileSpecTemp);
    if(osError == noErr || osError == fnfErr)
	osError = FSpCreate(&fileSpecTemp,'trsh', 'trsh', smSystemScript);
    if(osError == noErr)
	osError = FSpOpenDF(&fileSpecTemp, fsRdWrPerm, &tempFileRefNum);
    if(osError == noErr)
	osError = doWritePictData(windowPtr,tempFileRefNum);
    if(osError == noErr)
	osError = FSClose(tempFileRefNum);
    if(osError == noErr)
	osError = FSClose(gGReference[WinIndex].fileRefNum);
    if(osError == noErr)
	osError = FSpExchangeFiles(&fileSpecTemp, &fileSpecActual);
    if(osError == noErr)
	osError = FSpDelete(&fileSpecTemp);
    if(osError == noErr)
	osError = FSpOpenDF(&fileSpecActual, fsRdWrPerm,
			    &gGReference[WinIndex].fileRefNum);

/*	if(osError == noErr)
	{

	osError = doCopyAppNameResource(windowPtr);
	}
*/
    return(osError);
}



