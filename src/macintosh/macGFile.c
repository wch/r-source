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

#include <RCarbon.h>

#include <stdio.h>
#include <fp.h> /* Jago */
#include <Quickdraw.h>

#ifdef HAVE_CONFIG_H   
#include <config.h>
#endif
#include "Defn.h"
#include "Graphics.h"
#include "RIntf.h"
#include "Fileio.h"
#include <Rdevices.h>


extern pascal	OSErr	FSpGetFullPath (const FSSpec*, short*, Handle*);

extern Graphic_Ref gGReference[MAX_NUM_G_WIN + 1];
extern SInt16 gAppResFileRefNum;
SInt16  ConsolefileRefNum;
FSSpec  ConsolefileFSSpec;
char    InitFile[256];
extern Boolean WeArePasting;




OSErr  doRSave(Boolean *haveCancel)
{
    OSErr err;

    if (strlen(InitFile) == 0) {
	err = doRSaveAs(haveCancel);
    } else {
	*haveCancel = false;
	err = noErr;

	R_SaveGlobalEnvToFile(InitFile);
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
    SInt16 				pathLen;
    Handle 				pathName=NULL;
    char 				path[MAC_FILE_SIZE], cur_path[MAC_FILE_SIZE];
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
			    FSpGetFullPath(&documentFSSpec, &pathLen,
					   &pathName);
			    HLock((Handle) pathName);
			    strncpy(InitFile, *pathName, pathLen);
			    InitFile[pathLen] = '\0';
			    HUnlock((Handle) pathName);

			    R_SaveGlobalEnvToFile(InitFile);
			    anErr = NavCompleteSave(&reply,
						    kNavTranslateInPlace);
		 	}
		 	
        }
        
        (void) NavDisposeReply(&reply);
    }                    

    return(anErr);

}

 
 
/*
   This routines makes a copy of the current graphic window onto a 
   file specified by the user.  New and cleaner routine. Now copies
   also text characters in pictures.
   Jago November 2001, Stefano M. Iacus. 
*/

OSErr  doSaveAsGraCommand(void)
{   
    WindowPtr 			window;
    Rect 				portRect;
	PicHandle			picHandle = nil;
	OSErr				anErr = noErr;
	OSType              fileTypeToSave = 'PICT';
    OSType              creatorType = 'ogle';
    NavReplyRecord      reply;
    NavDialogOptions    dialogOptions;
    FSSpec      		documentFSSpec;
    long				inOutCount;
    short				refNum, count;
    AEKeyword   		theKeyword;
    DescType    		actualType;
    unsigned char 		header[512];
	Size        		actualSize;
	Rect				tempRect1;
 	Rect				resizeRect;
    CGrafPtr     		savePort, tempPort;
    RGBColor			oldColor, newColor;

    window = FrontWindow();
  	
	if(!window)
	 return(-1L);

    GetPort(&savePort);
 	 
	GetPortBounds(GetWindowPort(window), &tempRect1);
	 	
	SetPortWindowPort(window);
	
	GetForeColor(&oldColor);
	GetCPixel (tempRect1.right-16,tempRect1.bottom-16,&newColor);
	
	tempPort = CreateNewPort();
   
    SetPort(tempPort);
	
	picHandle = OpenPicture(&tempRect1);
	
	CopyBits(GetPortBitMapForCopyBits(GetWindowPort(window)), GetPortBitMapForCopyBits(tempPort),  &tempRect1, 
	   &tempRect1, srcCopy, 0L);
	
	SetRect(&resizeRect, tempRect1.right-15, tempRect1.bottom-15, tempRect1.right,tempRect1.bottom);
	
    RGBForeColor(&newColor);
	PaintRect(&resizeRect);
	RGBForeColor(&oldColor);
	 
	ClosePicture();
    
    DisposePort(tempPort);

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
    			inOutCount = GetHandleSize((Handle)picHandle);
				anErr = FSWrite(refNum,&inOutCount,*picHandle);
    		}
    		FSClose( refNum );
  	  }
  	  reply.translationNeeded = false;
  	  anErr = NavCompleteSave(&reply, kNavTranslateInPlace);
    
 	  NavDisposeReply(&reply);
    }
	
	KillPicture(picHandle);
    
    SetPort(savePort);  
    
	return anErr;

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
    char 				path[MAC_FILE_SIZE];
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


 

 