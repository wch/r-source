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

//OSErr  doWritePictData(WindowPtr windowPtr,SInt16 tempFileRefNum);
OSErr  doCopyAppNameResource(WindowPtr windowPtr);
OSErr  doCopyGraResource(ResType, SInt16, SInt16, SInt16);
//OSErr  doWriteFile(WindowPtr windowPtr);
void PutPictToFile(PicHandle thePicture);

extern pascal	OSErr	FSpGetFullPath (const FSSpec*, short*, Handle*);

extern Graphic_Ref gGReference[MAX_NUM_G_WIN + 1];
extern SInt16 gAppResFileRefNum;
SInt16  ConsolefileRefNum;
FSSpec  ConsolefileFSSpec;
char    InitFile[256];
extern Boolean WeArePasting;



/*
#define	rAppStringsID			128

enum {
	sApplicationName 		= 1,
	sTranslationLockedErr,
	sTranslationErr,
	sOpeningErr,
	sReadErr,				
	sWriteToBusyFileErr,
	sBusyOpen,
	sChooseFile,
	sChooseFolder,
	sChooseVolume,		
	sCreateFolder,
	sChooseObject,
	sChooseApp
};

#define kSelectFolderPrefKey	4
*/

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
	    error("can't save data -- unable to write ./%s\n", InitFile);
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
   Completely rewritten to work with the new Navigation Services
   Jago, April 2001, Stefano M. Iacus
*/

OSErr  doRSaveAs(Boolean *haveCancel) {
    StandardFileReply 	fileReply;
    OSType 				fileType;
    OSErr 				osError = 0;
    FILE 				*fp ;
    SInt16 				pathLen;
    Handle 				pathName=NULL;
    char 				path[FILENAME_MAX], cur_path[FILENAME_MAX];
	OSErr               anErr = noErr;
    NavReplyRecord      reply;
    NavDialogOptions    dialogOptions;
    OSType              fileTypeToSave = 'BINA';
    OSType              creatorType = R_ID;



    anErr = NavGetDefaultDialogOptions(&dialogOptions); 
    if(anErr == noErr){
    	dialogOptions.dialogOptionFlags |= kNavSelectDefaultLocation;
    	dialogOptions.dialogOptionFlags |= kNavAllFilesInPopup;
    	dialogOptions.dialogOptionFlags |= kNavAllowInvisibleFiles;
    	
    	PStringCopy("\p.RData",dialogOptions.savedFileName);
        PStringCopy("\pSave Workspace Image",dialogOptions.windowTitle);
        PStringCopy("\pR",dialogOptions.clientName);
    
    	anErr = NavPutFile(nil, &reply, &dialogOptions, nil,
                       fileTypeToSave, creatorType, nil );


		*haveCancel = !reply.validRecord;
        
		if(anErr == noErr && reply.validRecord){
		 	AEKeyword	theKeyword;
		 	DescType	actualType;
		 	Size		actualSize;
		 	FSSpec		documentFSSpec;
		 	 	
		 	anErr = AEGetNthPtr(&(reply.selection), 1, typeFSS, &theKeyword, &actualType,
		 						&documentFSSpec, sizeof(documentFSSpec), &actualSize);
		 	if(anErr == noErr){
		 			FSpGetFullPath(&documentFSSpec, &pathLen, &pathName);
					HLock((Handle) pathName);
					strncpy(InitFile, *pathName, pathLen);
					InitFile[pathLen] = '\0';
					HUnlock((Handle) pathName);


					if( fp = fopen(InitFile, "wb") ){ /* binary file */
						if (HASHTAB(R_GlobalEnv) != R_NilValue)
	    					R_SaveToFile(HASHTAB(R_GlobalEnv), fp, 0);
						else
	    					R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
						fclose(fp);
						anErr = NavCompleteSave(&reply, kNavTranslateInPlace);
						}
					else
						error("can't save data -- unable to write ./%s\n", InitFile);


		 	}
		 	
        }
        
        (void) NavDisposeReply(&reply);   
    }                    

    return(anErr);

}

 
 



void PutPictToFile(PicHandle thePicture)

{
	
	OSErr               anErr = noErr;
    NavReplyRecord      reply;
    NavDialogOptions    dialogOptions;
    OSType              fileTypeToSave = 'PICT';
    OSType              creatorType = 'ogle';
	AEKeyword   		theKeyword;
    DescType    		actualType;
    Size        		actualSize;
    FSSpec      		documentFSSpec;
    long				inOutCount;
    short				refNum, count;
    unsigned char 		header[512];
    
    for (count = 0; count < 512; count++)
		header[count] = 0x00;

    anErr = NavGetDefaultDialogOptions(&dialogOptions); 
    dialogOptions.dialogOptionFlags |= kNavSelectDefaultLocation;
    
    anErr = NavPutFile( nil, 
    					&reply, 
    					&dialogOptions, 
    					nil,
                        fileTypeToSave, 
                        creatorType, 
                        nil );
    
    if (anErr == noErr && reply.validRecord) {
    
    					
    	anErr = AEGetNthPtr(&(reply.selection), 1, typeFSS,
                                &theKeyword, &actualType,
                                &documentFSSpec, sizeof(documentFSSpec),
                                &actualSize );
                                
  	  if (anErr == noErr) {
  	  
  	  		anErr = FSpCreate(&documentFSSpec, creatorType, fileTypeToSave, smSystemScript);
			if (anErr == dupFNErr) {
				anErr = FSpDelete(&documentFSSpec);
				anErr = FSpCreate(&documentFSSpec, creatorType, fileTypeToSave, smSystemScript);
			}		// this is quick 'n' dirty or there'd be more robust handling here
			
    		// write the file
    		FSpOpenDF(&documentFSSpec, fsRdWrPerm, &refNum );
    		inOutCount = 512;
   			anErr = FSWrite(refNum, &inOutCount, header);		// write the header
    		if (anErr == noErr) {
    			inOutCount = GetHandleSize((Handle)thePicture);
				anErr = FSWrite(refNum,&inOutCount,*thePicture);
    		}
    		FSClose( refNum );
  	  }
  	  reply.translationNeeded = false;
  	  anErr = NavCompleteSave(&reply, kNavTranslateInPlace);
    
 	  NavDisposeReply(&reply);
    }
}




OSErr  doSaveAsGraCommand(void)
{   
    WindowPtr 	window;
    SInt16 		WinIndex;
    DevDesc 	*dd;
    PicHandle	WPicHandle=nil;
    CGrafPtr 	savePort, tempPort;
    Rect 		portRect;

    window = FrontWindow();

    WinIndex = isGraphicWindow(window);

    dd = (DevDesc*)gGReference[WinIndex].devdesc;

    GetPort(&savePort);
    
    GetWindowPortBounds(window,&portRect);
	tempPort = CreateNewPort();
     
    WPicHandle = OpenPicture(&portRect);

    ClipRect(&portRect);
 	
 	gGReference[WinIndex].activePort = tempPort;

 	WeArePasting = true;

 	playDisplayList(dd);

 	WeArePasting = false;
	
	ClosePicture();

    DisposePort(tempPort);
    
    HLock((Handle)WPicHandle);
  
    PutPictToFile(WPicHandle);	/* put it to a file */
 
 	HUnlock((Handle)WPicHandle);
    
    KillPicture(WPicHandle);
    
    SetPort(savePort);  
    
    return noErr;

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
    Handle	sourceResourceHdl=NULL;
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
// *****************************************************************************
// *
// *	DoSelectDirectory( )
// *	
// *****************************************************************************
OSErr DoSelectDirectory( void )
{	
	NavReplyRecord		theReply;
	NavDialogOptions	dialogOptions;
	OSErr				theErr = noErr;
	NavEventUPP			eventUPP = nil; 
	SInt16 				pathLen;
    Handle 				pathName=NULL;
    char 				path[FILENAME_MAX];
	OSErr               anErr = noErr;
	theErr = NavGetDefaultDialogOptions( &dialogOptions );
	
	GetIndString( dialogOptions.message, rAppStringsID, sChooseFolder );
	
	dialogOptions.preferenceKey = kSelectFolderPrefKey;
	
	theErr = NavChooseFolder(	NULL,
								&theReply,
								&dialogOptions,
								eventUPP,
								NULL,
								nil);
	
	DisposeNavEventUPP( eventUPP );
	if ( theReply.validRecord && theErr == noErr)
	{
		// grab the target FSSpec from the AEDescList:	
		FSSpec		finalFSSpec;	
		AEKeyword 	keyWord;
		DescType 	typeCode;
		Size 		actualSize = 0;
		// there is only one selection here we get only the first AEDescList:
		if (( theErr = AEGetNthPtr( &(theReply.selection), 1, typeFSS, &keyWord, &typeCode, 
		         &finalFSSpec, sizeof( FSSpec ), &actualSize )) == noErr )		
		{
			// 'finalFSSpec' is the selected directoryÉ
			FSpGetFullPath(&finalFSSpec, &pathLen, &pathName);
			HLock((Handle) pathName);
			
			strncpy(path, *pathName, pathLen);
			path[pathLen] = '\0';
			chdir(path);		
			HUnlock((Handle) pathName);
			
		}
		
		theErr = NavDisposeReply( &theReply );
	}
		
	return theErr;
}
