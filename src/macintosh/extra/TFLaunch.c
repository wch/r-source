/*	File:		TestFinderLaunch.c
	
Description: 
	A test application for sending an open documents Apple event to the
	Finder.  This application calls the FinderLaunch routine defined
	in FinderLaunch.c.
		
	This file is organized into the following sections:

	HFS OBJECT SELECTION
		routines calling Navigation Services or Standard file allowing
		the user to select either a file or a directory.
	CALLING FinderLaunch
		a routine that calls the FinderLaunch routine defined
		in the file FinderLaunch.c.  Here, after gathering a
		list of files/folders from the user, an open documents
		apple event is sent to the Finder specifying the items
		selected.  The Finder, in turn, with
		launch/display/open the items as appropriate.
	MAIN WINDOW
		routines for drawing and handling clicks in the main window.
	MENU HANDLING
		menu handleing code
	APPLE EVENT HANDLERS
		apple event handlers for open and quit application events.
	EVENT HANDLING
		event dispatching code.  calls to WaitNextEvent are made here.
	MAIN PROGRAM
		the main program routine including initialization code, 
		the main loop, and teardown code.

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

#include "TestFinderLaunch.h"
#include "FinderLaunch.h"

#include <QuickDraw.h>
#include <Menus.h>
#include <Windows.h>
#include <Dialogs.h>
#include <Fonts.h>
#include <SegLoad.h>
#include <Resources.h>
#include <Balloons.h>
#include <Devices.h>
#include <AppleEvents.h>
#include <StdIO.h>
#include <TextUtils.h>
#include <string.h>
#include <Gestalt.h>
#include <Appearance.h>

#include <StandardFile.h>
#include <Navigation.h>


	/* true while the app is running */
Boolean gRunning = true;

#ifndef __MWERKS__
QDGlobals	qd;	/* QuickDraw globals*/
#endif

FileFilterYDUPP gFileFilter;
DlgHookYDUPP gSFHook;
NavEventUPP gNavEventProc;
AEIdleUPP gAEIdleProc = NULL; /* idle proc called from AEInteractWithUser */
DialogPtr gMainDialog = NULL;

Boolean gForground = true;
Boolean gAppleEvents = false;
Boolean gAppearance = false;



/* HFS OBJECT SELECTION ------------------------------------------------ */

/* in this section we define a set of routines for selecting a file or
   a folder.  Here, NavigationServices is used when it is
   present, otherwise we fall back to useing the standard file
   alerts for system 7.  Note that Navigation services is
   strictly PowerPC only, so any parts referring to that API are
   bracketed in conditional statements.
	
   These routines are here so there is a way to choose a file or
   folder from the main application.  After selecting a file or
   folder, an Apple event is sent to the finder asking it to
   open/launch/display the item.
*/
	

/* InvisoFilter is a file filter procedure passed to the CustomGetFile
   routine.  Its purpose is to prevent the display of invisible
   files and folders in the standard file window. 
*/
static pascal Boolean InvisoFilter(ParmBlkPtr PB, void *yourDataPtr) 
{
    CInfoPBRec *cat = (CInfoPBRec *) PB;
    /* filter invisible files and folders */
    if ((cat->hFileInfo.ioFlFndrInfo.fdFlags & kIsInvisible) == 0)
	return false;
    else return true;
}

/* MySFHook is a dialog hook routine passed to the CustomGetFile
   routine.  Its purpose is to maintain the 'Select' button in
   the bottom right corner of the window.  This button allows
   users to select a directory rather than navigate into it.  If
   the user clicks on the 'Select' button while a folder is
   hilited in the list view, then the folder is passed back to
   CustomGetFile's caller. 
*/
static pascal short MySFHook(short item, DialogPtr dialog, void *yourDataPtr) 
{
	
    static Boolean gFolderSelected = false;
    static ControlHandle gFolderControl = NULL;
    StandardFileReply *theReply;
	
    if (GetWRefCon(dialog) != sfMainDialogRefCon) return item;
    theReply = (StandardFileReply *) yourDataPtr;
    if (item == sfHookFirstCall) {
	short itemt;
	Rect itemb;
	GetDialogItem(dialog, kMySFSelectButton, &itemt, 
		      (Handle*) &gFolderControl, &itemb);
	HiliteControl(gFolderControl, 255);
	gFolderSelected = false;
    } else if ((item == kMySFSelectButton) && gFolderSelected) {
	return sfItemOpenButton;
    } else {
	if (theReply->sfIsFolder || theReply->sfIsVolume) {
	    if ( ! gFolderSelected) {
		HiliteControl(gFolderControl, 0);
		gFolderSelected = true;
	    }
	} else if (gFolderSelected) {
	    HiliteControl(gFolderControl, 255);
	    gFolderSelected = false;
	}
    }
    return item;
}


#if TARGET_CPU_PPC

/* NavEventCallBack is a callback routine provided to the
   NavChooseObject routine.  In this routine we process update
   and activate events for the main window while navigation
   services is displaying its window. 
*/
static pascal void 
NavEventCallBack( NavEventCallbackMessage callBackSelector,
		  NavCBRecPtr callBackParms, NavCallBackUserData callBackUD) 
{
    if (callBackSelector == kNavCBEvent) {
	short ewhat;
	ewhat = callBackParms->eventData.eventDataParms.event->what;
	if ((ewhat == updateEvt) || (ewhat == activateEvt)) {
		
	    HandleNextEvent(callBackParms->eventData.eventDataParms.event);

	}
    }
}

#endif


/* GetHFSObjectList opens a communication session with the user
   allowing them to choose one more file system objects.  After
   the user has made a selection, a list of the items chosen is
   passed back as a AEDescList containing a list of FSSpec
   records. if the user cancels the interaction, then no list is
   returned and a userCanceledErr is returned.
*/
static OSErr GetHFSObjectList(AEDescList *documents) 
{
#if TARGET_CPU_PPC
    NavReplyRecord theReply;
#endif
    Boolean hasNavReply;
    OSErr err;
    /* set up locals */
    AECreateDesc(typeNull, NULL, 0, documents);
    hasNavReply = false;
	
#if TARGET_CPU_PPC
    if (NavServicesAvailable()) {
	NavDialogOptions dialogOptions;
	/* set the message in the navigation window to indicated multiple
	   selections are allowed */
	memset(&theReply, 0, sizeof(theReply));
	err = NavGetDefaultDialogOptions(&dialogOptions);
	if (err != noErr) goto bail;
	dialogOptions.dialogOptionFlags = (kNavDontAutoTranslate | kNavAllowMultipleFiles);
	GetIndString(dialogOptions.message, kMainStrings, kNavTextMessage);
	/* run the navigation window */
	err = NavChooseObject( NULL, &theReply, &dialogOptions, 
			       gNavEventProc, NULL, NULL);
	if (err != noErr) goto bail;
	if (!theReply.validRecord) { err = userCanceledErr; goto bail; }
	hasNavReply = true;
	/* duplicate the returned document list */
	err = AEDuplicateDesc(&theReply.selection, documents);
	if (err != noErr) goto bail;
	/* clean up the navigation stuff */
	NavDisposeReply(&theReply);
    } else
#endif
    {	Point where = {100, 100};
    SFTypeList typeList;
    StandardFileReply reply;
    /* set up locals
       SetPt(&where, 100, 100);
       /* run the standard file window */
    CustomGetFile(gFileFilter, -1, typeList, &reply, 130, where, 
		  gSFHook, NULL, NULL, NULL, &reply);
    if (!reply.sfGood) { err = userCanceledErr; goto bail; }
    /* if successful, save the selection to a list descriptor */
    err = AECreateList(NULL, 0, false, documents);
    if (err != noErr) goto bail;
    err = AEPutPtr(documents, 0, typeFSS, &reply.sfFile, sizeof(FSSpec));
    if (err != noErr) goto bail;
    }
    return noErr;
	
    /* error recovery */
 bail:
#if TARGET_CPU_PPC
    if (hasNavReply) NavDisposeReply(&theReply);
#endif
    AEDisposeDesc(documents);
    return err;
}



/* CALLING FinderLaunch ------------------------------------------------ */

/* in this section, we call through to the FinderLaunch() routine
   defined in the file FinderLaunch.c.  After calling
   GetHFSObjectList to retrieve a list of one or more FSSpec
   records referring to a number of hfs objects, we coerce this
   list into an array of FSSpec records and pass it to the
   FinderLaunch routine. 
*/
	
	
/* SelectTargetsToLaunch calls GetHFSObjectList to retrieve a list of
   files or folders and then it passes the selected folders and
   files to the FinderLaunch routine. 
*/
static void SelectTargetsToLaunch(void) 
{
    AEDescList documents;
    OSErr err;
    long index, count;
    FSSpec *targets;
    AEKeyword keyword;
    DescType typecode;
    Size actualSize;
    /* set up locals */
    AECreateDesc(typeNull, NULL, 0, &documents);
    targets = NULL;
    /* get a list of files from the user */
    err = GetHFSObjectList(&documents);
    if (err != noErr) goto bail;
    /* count the items in the list */
    err = AECountItems(&documents, &count);
    if (err != noErr) goto bail;
    /* allocate an array to store the records */
    targets = (FSSpec *) NewPtr(count * sizeof(FSSpec));
    if (targets == NULL) { err = memFullErr; goto bail; }
    /* copy each record from the list to the array */
    for (index = 0; index < count; index++) {
	err = AEGetNthPtr(&documents, (index + 1), typeFSS, &keyword, 
			  &typecode,
			  (targets + index), sizeof(FSSpec), &actualSize);
	if (err != noErr) goto bail;
    }
    /* ask the Finder to launch the items */
    err = FinderLaunch(count, targets);
	
    /* clean up and leave, report any 'real' errors */
 bail:
    if ((err != noErr) && (err != userCanceledErr)) {
	Str255 errStr;
	NumToString(err, errStr);
	ParamAlert(kSelectAbortedError, errStr, NULL);
    }
    if (targets != NULL) DisposePtr((Ptr) targets);
    AEDisposeDesc(&documents);
}





/* MAIN WINDOW ------------------------------------------------ */


/* HitMainWindow is called when DialogSelect returns true for the main
   dialog window.  This usually indicates that there has been a
   mouse down inside of the main window. 
*/
static void HitMainWindow(DialogPtr theDialog, EventRecord *ev, short itemNo) 
{
    if (itemNo == kMainSelectButton)
	SelectTargetsToLaunch();
}


/* RedrawMainDialogWindow redraws the main window honoring the current
   activation state of the window. 
*/
static void RedrawMainDialogWindow(DialogPtr theDialog) 
{
    if (theDialog != NULL) {
	short itemt;
	ControlHandle theControl;
	Rect itemb;
		
	GetDialogItem(theDialog, kMainSelectButton, &itemt, 
		      (Handle*) &theControl, &itemb);
	if ((theDialog == FrontWindow()) && gForground)
	    HiliteControl(theControl, 0);
	else HiliteControl(theControl, 255);
	DrawDialog(theDialog);
    }
}



/* MENU HANDLING ------------------------------------------------ */


/* ResetMenus is called to reset the menus immediately before
   either MenuSelect or MenuKey is called.  Here, we disable the
   quit command during file copies. 
*/
static void ResetMenus(void) 
{
    /* nothing to do here */
}


/* DoMenuCommand is called after either MenuSelect of MenuKey.  The
	parameter rawMenuSelectResult is the result from one of these
	two routines.  DoMenuCommand parses this result into its two
	parts, and dispatches the menu command as appropriate. 
*/
static void DoMenuCommand(long rawMenuSelectResult) 
{
    short menu, item;
    /* decode the MenuSelect result */
    menu = (rawMenuSelectResult >> 16);
    if (menu == 0) return;
    item = (rawMenuSelectResult & 0x0000FFFF);
    /* dispatch on result */
    switch (menu) {
    case mApple:
	if (item == iAbout) {
	    /* show the about box. */
	    ParamAlert(kAboutBoxError, NULL, NULL);
	} else if (item >= iFirstAppleItem) {
	    Str255 deskAccName;
	    /* open an apple menu item. */
	    GetMenuItemText(GetMenuHandle(mApple), item, deskAccName);
	    OpenDeskAcc(deskAccName);
	}
	break;
    case mFile:
	if (item == iSelectTargets)
	    SelectTargetsToLaunch();
	else if (item == iQuit) 
	    gRunning = false;
	break;
    }
    /* unhilite the menu once we're done the command */
    HiliteMenu(0);
}



/* APPLE EVENT HANDLERS ------------------------------------------------ */

/* OpenApplication is an apple event handler called for 'open
   application' apple events. */
static pascal OSErr OpenApplication(const AppleEvent *appleEvt, 
				    AppleEvent* reply, long refcon) 
{
    gMainDialog = GetNewDialog(kMainDialog, NULL, (WindowPtr) (-1));
    return noErr;
}

/* CloseApplication is an apple event handler called for 'close
   application' apple events. */
static pascal OSErr CloseApplication(const AppleEvent *appleEvt, 
				     AppleEvent* reply, long refcon) 
{
    gRunning = false;
    return noErr;
}



/* EVENT HANDLING ------------------------------------------------ */


/* HandleNextEvent handles the event in the event record *ev
	dispatching the event to appropriate routines.  
*/
void HandleNextEvent(EventRecord *ev) 
{
    DialogPtr theDialog;
    WindowPtr theWindow;
    short itemNo;
	
    /* dialog pre-processing */
    if (((ev->what == keyDown) || (ev->what == autoKey)) && ((ev->modifiers & cmdKey) != 0)) {
	ResetMenus();
	DoMenuCommand(MenuKey((char) (ev->message & charCodeMask)));
    } else if (ev->what == osEvt) {
	/* process manager switches */
	if ( (((ev->message >> 24) & 0x0FF) == suspendResumeMessage) && ((ev->message & resumeFlag) != 0)) {
	    gForground = true;
	} else gForground = false;
	RedrawMainDialogWindow(gMainDialog);
    } else if (ev->what == activateEvt) {
	if ((gMainDialog == ((DialogPtr) ev->message)))
	    RedrawMainDialogWindow(gMainDialog);
    }

    /* handle clicks in the dialog window */
    if (IsDialogEvent(ev))
	if (DialogSelect(ev, &theDialog, &itemNo)) {
	    if (theDialog == gMainDialog)
		HitMainWindow(theDialog, ev, itemNo);
	}

    /* clicks and apple events... */
    if (ev->what == kHighLevelEvent) {
	AEProcessAppleEvent(ev);
    } else if (ev->what == mouseDown)
	switch (FindWindow(ev->where, &theWindow)) {
			
				/* menu bar clicks */
	case inMenuBar:
	    ResetMenus();
	    DoMenuCommand(MenuSelect(ev->where));
	    break;
				
				/* clicks in the close box, close the app */
	case inGoAway:
	    if (TrackGoAway(theWindow, ev->where)) {
		gRunning = false;
	    }
	    break;
				
				/* allow window drags */
	case inDrag:
	    if (theWindow == FrontWindow()) {
		Rect boundsRect = { -32000, -32000, 32000, 32000};
		DragWindow(theWindow, ev->where, &boundsRect);
	    }
	    break;
				
				/* desktop clicks, etc... */
	case inSysWindow:
	    SystemClick(ev, theWindow);
	    break;
	}
}


/* ProcessNextEvent calls WaitNextEvent to get the next event and then
   it passes the event along to the HandleNextEvent routine.
   sleepTime is passed to the WaitNextEvent routine in the sleep
   parameter. 
*/
void ProcessNextEvent(long sleepTime) 
{
    EventRecord ev;
    /* get the next event */
    if ( ! WaitNextEvent(everyEvent, &ev,  sleepTime, NULL) )
	ev.what = nullEvent;
    HandleNextEvent(&ev);
}

/* FDPIdleProcedure is the idle procedure called by AEInteractWithUser
   while we are waiting for the application to be pulled into the
   forground.  It simply passes the event along to
   HandleNextEvent 
*/
static pascal Boolean FDPIdleProcedure(EventRecord *theEvent, long *sleepTime, 
				       RgnHandle *mouseRgn) 
{
    HandleNextEvent(theEvent);
    return false;
}


/* ParamAlert is a general alert handling routine.  If Apple events
   exist, then it calls AEInteractWithUser to ensure the
   application is in the forground, and then it displays an alert
   after passing the s1 and s2 parameters to ParamText. 
*/
short ParamAlert(short alertID, StringPtr s1, StringPtr s2) 
{
    AEInteractWithUser(kNoTimeOut, NULL, gAEIdleProc);
    ParamText(s1, s2, NULL, NULL);
    return Alert(alertID, NULL);
}



/* MAIN PROGRAM ------------------------------------------------ */

int main(void) 
{
    OSErr err;
    AEEventHandlerUPP aehandler;
    long response;
	
    /* set up our app */
    SetApplLimit(GetApplLimit());
    MaxApplZone();
    InitGraf(&qd.thePort);
    InitFonts();
    InitWindows();
    TEInit();
    InitMenus();
    InitDialogs(0);
    FlushEvents(everyEvent, 0);
    InitCursor();


    /* apple events??? */
    if (Gestalt(gestaltAppleEventsAttr, &response) != noErr) response = 0;
    gAppleEvents = ((response & (1<<gestaltAppleEventsPresent)) != 0);
    if ( ! gAppleEvents) {
	ParamAlert(kReqMgrsNotAvailError, NULL, NULL);
	err = userCanceledErr;
	goto bail;
    }
    /* appearance */
    if (Gestalt(gestaltAppearanceAttr, &response) != noErr) response = 0;
    if ((response & (1<<gestaltAppearanceExists)) != 0) {
	err = RegisterAppearanceClient();
	if (err != noErr) goto bail;
	gAppearance = true;
    }

#if TARGET_CPU_PPC
    if (NavServicesAvailable())
	NavLoad();
    gNavEventProc = NewNavEventProc(NavEventCallBack);
    if (gNavEventProc == NULL) { err = memFullErr; goto bail; }
#endif
    gFileFilter = NewFileFilterYDProc(InvisoFilter);
    if (gFileFilter == NULL) { err = memFullErr; goto bail; }
    gSFHook = NewDlgHookYDProc(MySFHook);
    if (gSFHook == NULL) { err = memFullErr; goto bail; }
    gAEIdleProc = NewAEIdleProc(FDPIdleProcedure);
    if (gAEIdleProc == NULL) { err = memFullErr; goto bail; }
	
    /* standard apple events */
    aehandler = NewAEEventHandlerProc(OpenApplication);
    if (aehandler == NULL) { err = memFullErr; goto bail; }
    err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, aehandler, 0, false);
    if (err != noErr) goto bail;
    aehandler = NewAEEventHandlerProc(CloseApplication);
    if (aehandler == NULL) { err = memFullErr; goto bail; }
    err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, aehandler, 0, false);
    if (err != noErr) goto bail;

    /* set up the menu bar */
    SetMenuBar(GetNewMBar(128));
    DrawMenuBar();
    AppendResMenu(GetMenuHandle(mApple), 'DRVR');

    /* run the app */
    while (gRunning) {
		
	ProcessNextEvent(-1);
		
    }
	
 bail:
#if TARGET_CPU_PPC
    if (NavServicesAvailable())
	NavUnload();
#endif
    if (err != noErr && err != userCanceledErr) {
	Str255 errStr;
	NumToString(err, errStr);
	ParamAlert(kProgramAbortedError, errStr, NULL);
    }
    if (gAppearance)
	UnregisterAppearanceClient();
    ExitToShell();
    return 0;
}
