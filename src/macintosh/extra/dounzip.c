/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file dounzip.c
 *  Copyright (C) 1998--2000  Guido Masarotto and Brian Ripley
 *                2001        Stefano M. Iacus and the R core team
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
 *  Implemented in R 1.2.2.  Stefano M. Iacus, Feb 2001
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

/* Specific includes for AE and Process */

#include <Types.h>
#include <memory.h>
#include <Packages.h>
#include <Errors.h>
#include <quickdraw.h>
#include <fonts.h>
#include <dialogs.h>
#include <windows.h>
#include <menus.h>
#include <events.h>
#include <OSEvents.h>
#include <Desk.h>
#include <diskinit.h>
#include <OSUtils.h>
#include <resources.h>
#include <toolutils.h>
#include <AppleEvents.h>
#include <EPPC.h>
#include <GestaltEqu.h>
#include <PPCToolbox.h>
#include <Processes.h>


#define	kCMDEventClass	'DCMD'  /* Event class command for MacZip */
#define	kCMDEvent    	'DCMD'  /* Event command                  */

#define kSearch 	200
#define kBadCombo 	129	/* ID for Alert: Switch to MacZip. Actually nto used but is here for future implementation */
#define kNoFind 	132	/* ID for Alert: MacZip not found */

extern AEIdleUPP gAEIdleUPP;  	/* defined in R_Event.c */

short gInteract 	= 1; 		/* 1 => R and MacZip never interact       */
short replyValue 	= 1; 		/* 1 => R waits for a reply from MacZip   */
short gSwitchLayer 	= false; 	/* We leave MacZip work background        */


/* Apple Events switches */

short gSendInteractArray[4] =
{
    nil, kAENeverInteract, kAECanInteract, kAEAlwaysInteract,
};

short replyLevels[] =
{
    kAENoReply, kAEWaitReply,kAEQueueReply
};



/* Function prototypes */

static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
		    int nxfiles, char **xfiles, int over);

int mac_dounzip(int argc,char **argv);
int mac_unzip(char *commandstr);
OSErr LaunchUnZipTool(AEDesc *theAddress);

SEXP do_int_unzip(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn, ans;
    char zipname[PATH_MAX], *topics[500], dest[PATH_MAX];
    int i, ntopics, rc;


    checkArity(op, args);

    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid zip name argument");
    strcpy(zipname, CHAR(STRING_ELT(CAR(args), 0)));
    args = CDR(args);
    fn = CAR(args);
    ntopics = length(fn);
    if (ntopics > 0) {
	if (!isString(fn) || ntopics > 500)
	    errorcall(call, "invalid topics argument");
	for(i = 0; i < ntopics; i++)
	    topics[i] = CHAR(STRING_ELT(fn, i));
    }
    args = CDR(args);
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, "invalid destination argument");
    strcpy(dest, CHAR(STRING_ELT(CAR(args), 0)));

    rc = do_unzip(zipname, dest, ntopics, topics, 0 , NULL, 1);
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = rc;
    UNPROTECT(1);
    return ans;
}


/*************************************************************************
 do_unzip: sends an event ('DCMD') with a command line to MacZip

   If MacZip is not open, it is searched and
   launched. MacZip must be on the same volume as
   the R application is. This can be improved by a
   better search algorithm.
   This routine can be used to pass any command line to MacZip
   by simply setting 'commandstr' properly. The 'commandstr'
   must start with 'zip' or 'unzip' depending on which kind
   of action MacZip should take. For complete description
   refere to the original MacZip doc. A free copy of MacZip
   can be found at http://www.sitec.net/maczip.

   Implemented on 1/2/2001, Stefano M. Iacus
***************************************************************************/


static int do_unzip(char *zipname, char *dest, int nfiles, char **files,
	    int nxfiles, char **xfiles, int over)
{
    int retcode, i;
    int my_argc;
    char commandstr[2000];
    char buffer[2000];
    AppleEvent ourEvent,ourReply;
    short sendIt = 2;
    OSErr err;
    AEDesc gTheAddress;

    /* prepare the command line arg for MacZip */

    sprintf(commandstr,"unzip -oq \"%s\" ",zipname);

    for(i=0;i<nfiles;i++)
    {
	sprintf(buffer,"%s ",files[i]);
	strcat(commandstr,buffer);
    }

    if(dest && dest[0]!='\0')
	sprintf(commandstr,"%s -d \"%s\"",commandstr,dest);

    if( (err=LaunchUnZipTool(&gTheAddress)) != noErr){  /* we cannot find MacZip */
	StopAlert(kNoFind,nil);
	return;
    }

    /* prepares the AppleEvent */
    AECreateAppleEvent(kCMDEventClass, kCMDEvent, &gTheAddress, 
		       kAutoGenerateReturnID, kAnyTransactionID, &ourEvent);

    /* Add the command string to the AE */
    AEPutParamPtr(&ourEvent, keyDirectObject, typeChar, commandstr, 
		  sizeof(char)*strlen(commandstr));

    /* Send the Event */

    if (replyLevels[replyValue] == kAEWaitReply && !gSwitchLayer) {
       AESend(&ourEvent, &ourReply, (gSendInteractArray[gInteract] + gSwitchLayer) + replyLevels[replyValue], kAENormalPriority,
               kAEDefaultTimeout,gAEIdleUPP, nil);
    }

    if (sendIt == 2) {
        AESend(&ourEvent, &ourReply, (gSendInteractArray[gInteract] + gSwitchLayer) + replyLevels[replyValue], kAENormalPriority,
               kAEDefaultTimeout,gAEIdleUPP, nil);
    }
    AEDisposeDesc(&ourEvent);
    if(replyLevels[replyValue] == kAEWaitReply)AEDisposeDesc(&ourReply);

    return(err);
}

/*  The function LaunchZipTool() is adapted to invoke MacZip by Stefano M. Iacus
    and is based FindReciever from the file: 'sender.c'.  This function will be
    abandoned soon by a more efficient one (see description below).
    Copyrights terms follows:


    File:		sender.c

    Contains:	Sender and Reciever are simple AppleEvent programs that demonstrate
    all the permutations of interaction levels for sending
    and recieving APpleEvents.
    Have fun with them.
    P.S. This also uses PBCatSearch, so I've done the typing for you if you've wanted
    to use this call but have been confused by the param block

    Written by:

    Copyright:	Copyright © 1984-1999 by Apple Computer, Inc., All Rights Reserved.

    You may incorporate this Apple sample source code into your program(s) without
    restriction. This Apple sample source code has been provided "AS IS" and the
    responsibility for its operation is yours. You are not permitted to redistribute
    this Apple sample source code as "Apple sample source code" after having made
    changes. If you're going to re-distribute the source, we require that you make
    it clear in the source that the code was descended from Apple sample source
    code, but that you've made changes.

    Change History (most recent first):
    7/20/1999	Karl Groethe	Updated for Metrowerks Codewarror Pro 2.1


*/

OSErr LaunchUnZipTool(AEDesc *theAddress)
{
    OSErr myError;
    LaunchParamBlockRec launchUnZipTool;
    DialogPtr search = GetNewDialog(kSearch,nil,(WindowPtr)-1);
    CSParamPtr csBlockPtr = (CSParamPtr)NewPtrClear(sizeof(CSParam));
    long dirIDUnused;
    Str32 nulString = "\p";
    /* initialize the parameter block */
    DrawDialog(search);
    if (csBlockPtr) {
        csBlockPtr->ioSearchInfo1 = (CInfoPBPtr)NewPtrClear(sizeof(CInfoPBRec));
        csBlockPtr->ioSearchInfo2 = (CInfoPBPtr)NewPtrClear(sizeof(CInfoPBRec));
        if (csBlockPtr->ioSearchInfo1 && csBlockPtr->ioSearchInfo2) {
            csBlockPtr->ioMatchPtr = (FSSpecPtr)NewPtrClear(sizeof(FSSpec) * 1);        /* only looking for 1 */
            if (csBlockPtr->ioMatchPtr) {
                /* Now see if we can create an optimization buffer */
                csBlockPtr->ioOptBuffer = NewPtr(2048);
                if (csBlockPtr->ioOptBuffer)
                    csBlockPtr->ioOptBufSize = 2048;
                else
                    csBlockPtr->ioOptBufSize = 0;           /* no buffer, sorry */
                csBlockPtr->ioReqMatchCount = 1;
                csBlockPtr->ioSearchTime = 0;               /* no timeout */
            }
        }
    }
    HGetVol(nil, &csBlockPtr->ioVRefNum, &dirIDUnused);     /* get default volume for search */
    csBlockPtr->ioSearchInfo1->hFileInfo.ioNamePtr = nil;
    csBlockPtr->ioSearchInfo2->hFileInfo.ioNamePtr = nil;
    csBlockPtr->ioSearchInfo1->hFileInfo.ioFlFndrInfo.fdCreator = 'IZip';  /* This is the MacZip signature */
    csBlockPtr->ioSearchInfo1->hFileInfo.ioFlFndrInfo.fdType    = 'APPL';
    csBlockPtr->ioSearchBits = fsSBFlFndrInfo;
    csBlockPtr->ioSearchInfo2->hFileInfo.ioFlFndrInfo.fdCreator = 0xFFFFFFFF;
    csBlockPtr->ioSearchInfo2->hFileInfo.ioFlFndrInfo.fdType    = 0xFFFFFFFF;

    myError = PBCatSearch(csBlockPtr, false);               /* search sync */
    if (myError == noErr && csBlockPtr->ioActMatchCount != 0) {
        /* we found it, so launch it */

        launchUnZipTool.launchBlockID = extendedBlock;
        launchUnZipTool.launchEPBLength = extendedBlockLen;
        launchUnZipTool.launchFileFlags = nil;
        launchUnZipTool.launchControlFlags = launchContinue + launchNoFileFlags + launchDontSwitch;
        launchUnZipTool.launchAppSpec = &csBlockPtr->ioMatchPtr[0];
        myError = LaunchApplication(&launchUnZipTool);
        if (myError == noErr) {
            /* it launched fine.  we can use the PSN to make a target */
            AECreateDesc(typeProcessSerialNumber, (Ptr)&launchUnZipTool.launchProcessSN, sizeof(ProcessSerialNumber), theAddress);

        }
    }

    /* no matter what happened, kill the memory we had allocated */
    if (csBlockPtr) {
        if (csBlockPtr->ioSearchInfo1)
            DisposePtr((Ptr)csBlockPtr->ioSearchInfo1);
        if (csBlockPtr->ioSearchInfo2)
            DisposePtr((Ptr)csBlockPtr->ioSearchInfo2);
        if (csBlockPtr->ioMatchPtr)
            DisposePtr((Ptr)csBlockPtr->ioMatchPtr);
        if (csBlockPtr->ioOptBuffer)
            DisposePtr((Ptr)csBlockPtr->ioOptBuffer);
        DisposePtr((Ptr)csBlockPtr);
    }

    /* catsearch section end */
    DisposeDialog(search);
    return(myError);
}
