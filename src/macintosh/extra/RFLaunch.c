/*	This file has been modified by Stefano M.Iacus to work on R from the original

        File:		FinderLaunch.c
	
	Description: 
 		A routine for sending an open documents Apple event to the
		finder.  This routine provides functionality equivalent to
		selecting a document/file/application and choosing the
		open command in the Finder's file menu.  

	Author:	John Montbriand

	Copyright: 
		Copyright © 1999 by Apple Computer, Inc.
		All rights reserved worldwide.
	
	Disclaimer:
		You may incorporate this sample code into your
		applications without restriction, though the sample
		code has been provided "AS IS" and the responsibility
		for its operation is 100% yours.  However, what you
		are not permitted to do is to redistribute the source
		as "DSC Sample Code" after having made changes. If
		you're going to re-distribute the source, we require
		that you make it clear in the source that the code was
		descended from Apple Sample Code, but that you've made
		changes.
	
	Change History (most recent first):
		9/13/99 created by John Montbriand */

#include <RCarbon.h>

#include "RFLaunch.h"
#include <QuickDraw.h>
#include <AppleEvents.h>
#include <Errors.h>
#include <Files.h>
#include <AERegistry.h>
#include <Aliases.h>




OSErr FinderLaunch(long nTargets, FSSpec *targetList) 
{
    OSErr err;
    AppleEvent theAEvent, theReply;
    AEAddressDesc fndrAddress;
    AEDescList targetListDesc;
    OSType fndrCreator;
    Boolean wasChanged;
    AliasHandle targetAlias;
    long index;
	
    /* verify parameters */
    if ((nTargets == 0) || (targetList == NULL)) return paramErr;
	
    /* set up locals  */
    AECreateDesc(typeNull, NULL, 0, &theAEvent);
    AECreateDesc(typeNull, NULL, 0, &fndrAddress);
    AECreateDesc(typeNull, NULL, 0, &theReply);
    AECreateDesc(typeNull, NULL, 0, &targetListDesc);
    targetAlias = NULL;
    fndrCreator = 'MACS';
	
    /* create an open documents event targeting the finder */
    err = AECreateDesc(typeApplSignature, (Ptr) &fndrCreator,
		       sizeof(fndrCreator), &fndrAddress);
    if (err != noErr) goto bail;
    err = AECreateAppleEvent(kCoreEventClass, kAEOpenDocuments,
			     &fndrAddress, kAutoGenerateReturnID,
			     kAnyTransactionID, &theAEvent);
    if (err != noErr) goto bail;
	
    /* create the list of files to open */
    err = AECreateList(NULL, 0, false, &targetListDesc);
    if (err != noErr) goto bail;
    for ( index=0; index < nTargets; index++) {
	if (targetAlias == NULL)
	    err = NewAlias(NULL, (targetList + index), &targetAlias);
	else err = UpdateAlias(NULL, (targetList + index), targetAlias, &wasChanged);
	if (err != noErr) goto bail;
	HLock((Handle) targetAlias);
	err = AEPutPtr(&targetListDesc, (index + 1), typeAlias, *targetAlias, GetHandleSize((Handle) targetAlias));
	HUnlock((Handle) targetAlias);
	if (err != noErr) goto bail;
    }
	
    /* add the file list to the apple event */
    err = AEPutParamDesc(&theAEvent, keyDirectObject, &targetListDesc);
    if (err != noErr) goto bail;

    /* send the event to the Finder */
    err = AESend(&theAEvent, &theReply, kAENoReply,
		 kAENormalPriority, kAEDefaultTimeout, NULL, NULL);

    /* clean up and leave */
 bail:
    if (targetAlias != NULL) DisposeHandle((Handle) targetAlias);
    AEDisposeDesc(&targetListDesc);
    AEDisposeDesc(&theAEvent);
    AEDisposeDesc(&fndrAddress);
    AEDisposeDesc(&theReply);
    return err;
}
