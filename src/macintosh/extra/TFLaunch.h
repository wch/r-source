/*	File:		TestFinderLaunch.h
	
Description: 
 	A test application for sending an open documents Apple event to the
	Finder.  This application calls the FinderLaunch routine defined
	in FinderLaunch.c.

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
		9/13/99 created by John Montbriand 
*/

#ifndef __TESTFINDERLAUNCH__
#define __TESTFINDERLAUNCH__

#include <MacTypes.h>
#include <Events.h>

	/* constants referring to the Apple menu */
enum {
    mApple = 128,
    iAbout = 1,
    iFirstAppleItem = 3
};

	/* constants referring to the File menu */
enum {
    mFile = 129,
    iSelectTargets = 1,
    iQuit = 3
};

	/* constants referring to the Edit menu */
enum {
    mEdit = 130,
    iUndo = 1,
    iCut = 3,
    iCopy = 4,
    iPaste = 5,
    iClear = 6
};


	/* constants referring to the main STR# resource */
enum {
    kMainStrings = 128,
    kNavTextMessage = 1
};

	/* constants referring to the main dialog window */
enum {
    kMainDialog = 128,
    kMainSelectButton = 1

};

	/* constants referring to the dialog resource used in the
	call to CustomGetFile.  */
enum {
    kMySFDialog = 130,
    kMySFSelectButton = 10

};

	/* constants referring to a number of error alerts. */
enum {
    kAboutBoxError = 130,
    kProgramAbortedError = 131,
    kSelectAbortedError = 132,
    kReqMgrsNotAvailError = 133
};


/* routine prototypes */


/* HandleNextEvent contains the application's event handling code.  When
   called, the routine performs any processing required or the
   event as it applies to the application. */
void HandleNextEvent(EventRecord *ev);

/* ProcessNextEvent calls WaitNextEvent to get the next event and then
   it passes the event along to the HandleNextEvent routine.
   sleepTime is passed to the WaitNextEvent routine in the sleep
   parameter.
*/
void ProcessNextEvent(long sleepTime);

/* ParamAlert is a general alert handling routine.  If Apple events
   exist, then it calls AEInteractWithUser to ensure the
   application is in the forground, and then it displays an alert
   after passing the s1 and s2 parameters to ParamText. 
*/
short ParamAlert(short alertID, StringPtr s1, StringPtr s2);

#endif
