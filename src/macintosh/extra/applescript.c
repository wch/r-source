/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file applescript.c
 *  Copyright (C) 2001        Stefano M. Iacus and the R core team
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
 *
 *  This file implement the int.unzip function by calling an external unzip tool.
 *  For the time being, it is assumed that the user has MacZip, the Info-Zip
 *  porting for the Macintosh machines. The MacZip application is assumed to
 *  reside on the same volume as the R application itself. If R fails to
 *  find and launch MacZip it puts up an alert dialog to the user by asking
 *  to install MacZip application. The user is also informed on where to
 *  find MacZip (or Info-Zip)  on the WWW for free dowload.
 *
 *  Implemented in R 1.3.0.  Stefano M. Iacus, Jun 2001
 *  
 *   
 *	ExecuteScript, AddFileToDesc
 *  are:
 *      Copyright ¨1992-3 Apple Computer Inc.
 *      All rights reserved.
 * and written by Donald Olson
 *
 *
 */

#include <RCarbon.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

#include "RIntf.h"

/* Specific includes for AE and Process */


#include <Types.h>
#include <memory.h>
#include <Packages.h>
#include <Errors.h>
#include <fonts.h>
#include <dialogs.h>
#include <windows.h>
#include <Events.h>
#include <Menus.h>
#include <Devices.h>
#include <diskinit.h>
#include <OSUtils.h>
#include <resources.h>
#include <toolutils.h>
#include <AppleEvents.h>
#include <EPPC.h>
#include <Gestalt.h> 
#include <Processes.h>

#include <Errors.h>
#include <Dialogs.h>
#include <Memory.h>
#include <Processes.h>
#include <String.h>
#include <Resources.h>
#include <Packages.h>
#include <Quickdraw.h>


#include <StandardFile.h>
#include <Aliases.h>
#include <Finder.h>
#include <Files.h>
#include <QDOffscreen.h>
#include <ToolUtils.h>
#include "OSA.h"
						

#define	kMYEDITDIALOG			1112	// Dialog id
#define kSenceOfDecorum			15		// Just above center please
#define kMySPutRsrc				1111	// Custom put dialog

#define keyJustExecute			'doit'	// execute only parameter
#define kCommentResType 		'TEXT'
#define kStyleResType 			'styl'	// We don't use this since 
										// we don't do styled text

#define kToySurprise			'ToyS'	// Script file creator
#define kCompiledScript			'osas'	// Script file type for 
										// compiled scripts
#define kTextScript				'TEXT'	// Script file type for 
										// text scripts

#define	kOk	 			1				// Our dialog items
#define	kCancel	 		2
#define	kExecute		3
#define	kSaveAs	 		4
#define	kCommentField 	6
#define kScriptField	5
#define kResultField	7
#define kCycleAgain		10

#define kSaveCompiled  	15				// Items for our custom sfput
#define kSaveText		14

#define kScriptRezID	128				// script rez id's
#define kCommentRezID	1128
#define kStyleID		1128


#define	kCMDEventClass	'DCMD'  /* Event class command for MacZip */
#define	kCMDEvent    	'DCMD'  /* Event command                  */

#define kSearch 	200
#define kBadCombo 	129	/* ID for Alert: Switch to MacZip. Actually nto used but is here for future implementation */
#define kNoFind 	132	/* ID for Alert: MacZip not found */

extern AEIdleUPP gAEIdleUPP;  	/* defined in R_Event.c */



extern char *R_Home;

/* Function prototypes */
OSErr	ExecuteScript(FSSpec theFileToExecute, AEDesc *result);
pascal OSErr FSpLocationFromFullPath(short fullPathLength,
									 const void *fullPath,
									 FSSpec *spec);

OSErr	AddFileToDesc(FSSpec theFSSpec, AEDesc *theDesc);

/*
   do_applescript: This function imply runs an applescript.
   Two arguments: foldername and scriptname. If "foldername"
   is not given, than it assumed that the script resides in
   R_Home:script folder.
   Jago June 2001, Stefano M. Iacus   
*/

SEXP do_applescript(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    char scriptname[PATH_MAX],  foldername[PATH_MAX],fullpath[PATH_MAX];
    int i, ntopics, rc=0;
    FSSpec scriptfss;
    AEDesc aeresult;
    
    checkArity(op, args);

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid script folder name argument");
    strcpy(foldername, CHAR(STRING_ELT(CAR(args), 0)));

    if(strlen(foldername) == 0)
     sprintf(foldername,"%s:script",R_Home);
     
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid script name argument");
    strcpy(scriptname, CHAR(STRING_ELT(CAR(args), 0)));

    if(strlen(scriptname) == 0)
     {
     PROTECT(ans = allocVector(INTSXP, 1));
     INTEGER(ans)[0] = -1;
     UNPROTECT(1);
     errorcall(call,"Empty script name file");
     return ans;
     }

    
    sprintf(fullpath,"%s:%s",foldername,scriptname);
    
    if( (rc = FSpLocationFromFullPath(strlen(fullpath),fullpath,
									 &scriptfss)) != noErr){
	 errorcall(call,"Cannot find script file");
	 goto back;								
	 }
	 
    rc = ExecuteScript(scriptfss, &aeresult);
    if(rc != noErr)
     errorcall(call,"Script execution error");
     
back:
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = rc;
    UNPROTECT(1);
    return ans;
}



/*

  ExecuteScript
  Execute only, no dialog here!
  
  Copyright ¨1992-3 Apple Computer Inc.
  All rights reserved.
  
  written by Donald Olson
*/

OSErr	ExecuteScript(FSSpec theFileToExecute, AEDesc *result)
{
	OSAError 				theOSAErr = noErr;
	OSErr					theErr = noErr;
	OSAID					resultingID = 0;
	long					modeFlags = 0;
	Handle					tempHandle = nil;
	AEDesc					source;
	short					ourFileRef, curResFile;
	ComponentInstance		gASComponent = 0;
	
	// open generic component
	
	gASComponent = OpenDefaultComponent(kOSAComponentType, kOSAGenericScriptingComponentSubtype);
	
	// Check errors here!! 
/*	if((gASComponent == badComponentInstance) || (gASComponent == badComponentSelector))
		return invalidComponentID; 	// Is this the right error??
*/		
	curResFile = CurResFile();		// Save current res chain
	
	// Open our resource file  for reading
	ourFileRef = FSpOpenResFile(&theFileToExecute, fsRdWrPerm);
	if(ResError())
		return ResError();
	
	UseResFile(ourFileRef);
	
	// Get our resource handle
	tempHandle = Get1Resource(kOSAScriptResourceType, kScriptRezID);
	if(tempHandle == nil)
	{
		theErr = AddFileToDesc(theFileToExecute, &source);
		if(theErr == noErr) {
			theOSAErr = OSACompileExecute(	gASComponent,
											&source,
											kOSANullScript,
											modeFlags,
											&resultingID);
			
			if(theOSAErr == noErr){
				/*  
					Must use OSACoerceToDesc instead of OSADisplay
					here since we want to preserve the data type 
					returned from the execution.
				*/
				theOSAErr = OSACoerceToDesc(	gASComponent,
												resultingID,
												typeWildCard,
												modeFlags,
												result);
			}
									
			theErr = theOSAErr;
		}
	}
	else {
		DetachResource(tempHandle);
		
		// Put our text into an AEDesc
		theErr = AECreateDesc(	kOSAScriptResourceType, (Ptr)*tempHandle, 
								(Size)(GetHandleSize(tempHandle)), &source);
		if(theErr != noErr) {
			theErr = theOSAErr;
			goto BAILLABEL;
		}
		
		theOSAErr = OSALoadExecute(gASComponent,
									&source,
									kOSANullScript,
									modeFlags,
									&resultingID);
		if(theOSAErr != noErr){
			theErr = theOSAErr;
			goto BAILLABEL;
		}
		else  {
			theOSAErr = OSACoerceToDesc(	gASComponent,
									resultingID,
									typeWildCard,
									modeFlags,
									result);
		}
									
		theErr = theOSAErr;
	}	
	BAILLABEL:;
	// Clean up time
	if(resultingID != 0) OSADispose( gASComponent, resultingID);
	if(tempHandle != nil) DisposeHandle(tempHandle);
	if(source.dataHandle != nil) AEDisposeDesc(&source);
	if(gASComponent != 0) CloseComponent(gASComponent);	
	
	// Restore res file and close our script file
	UseResFile(curResFile);
	CloseResFile(ourFileRef);
	return theErr;
}


/*
	AddFileToDesc
	Takes the descriptor passed in and adds the data found in the FSSpec to it.
	This is for text scripts.

  Copyright ¨1992-3 Apple Computer Inc.
  All rights reserved.
  
  written by Donald Olson
*/

OSErr	AddFileToDesc(FSSpec theFSSpec, AEDesc *theDesc)
{
	CInfoPBPtr		pb = (CInfoPBPtr)NewPtrClear(sizeof(CInfoPBRec));
   	HFileInfo		*fpb = (HFileInfo*)pb;
   	long			fileSize = 0;
   	Handle			tempBuffer;
	short 			fRefNum = 0;
	OSErr			theErr = noErr;
	/* 
		Need to know the size of the file so we can create a buffer large enough to contain the
		file.
	*/
	
	/* Set up pblock */
	(*pb).hFileInfo.ioNamePtr = (StringPtr)&(theFSSpec.name);
	(*pb).hFileInfo.ioDirID = theFSSpec.parID;					
	(*pb).hFileInfo.ioVRefNum = theFSSpec.vRefNum;
	theErr = PBGetCatInfo(pb, false);
	
	if(theErr == noErr) {
		// Make sure we really got a file
		if(fpb->ioFlAttrib & 16) {
			theErr = 3030;
			goto DISPOSELABEL;
		}
	}
	else
		goto DISPOSELABEL;
	fileSize = fpb->ioFlLgLen;
	tempBuffer = NewHandleClear(fileSize);
	if(tempBuffer == nil) {
		theErr = memFullErr;
		goto DISPOSELABEL;
	}
	
	theErr = FSpOpenDF(&theFSSpec, 0, &fRefNum);		// Open data fork
	if(theErr != noErr) goto DISPOSELABEL;
	
	HLock(tempBuffer);	
	theErr = FSRead(fRefNum, &fileSize, *tempBuffer);	// Read in data to buffer
   	if(theErr != noErr)  goto DISPOSELABEL;
	
	theErr = FSClose(fRefNum);							// Close data fork
	if(theErr != noErr) goto DISPOSELABEL;
	
	// Add the data to our descriptor
	theErr = AECreateDesc(typeChar, (Ptr) *tempBuffer, fileSize, theDesc);
     
	DISPOSELABEL:;
	HUnlock(tempBuffer);                     		
	DisposeHandle(tempBuffer);
	DisposePtr((Ptr)pb);
	
	return theErr;
}


