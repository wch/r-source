/*
	************************************************************************************************ 
	 
	REvent
	by wing kwong (Tiki), WAN 3/2/99
	
	************************************************************************************************ 
	Description

    This file is based on the WASTE and WASTE demo, I had do some modification to make it function
    as the way R want. The routine in here is used to handle event (high or low level event.)
    	
	************************************************************************************************ 
	Description of WASTE and WASTE Demo :
	
	??? Is it necessary in here  ??? I think we need

	WASTE Demo Project:
	Macintosh Controls with Long Values

	Copyright © 1993-1998 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub

	************************************************************************************************

*/

/* ************************************************************************************************ */
/*                                    INCLUDE HEADER FILE                                           */
/* ************************************************************************************************ */

#ifndef __APPLEEVENTS__
#include <AppleEvents.h>
#endif

#ifndef __AEREGISTRY__
#include <AERegistry.h>
#endif

#ifndef __DISKINIT__
#include <DiskInit.h>
#endif

#ifndef __TEXTSERVICES__
#include <TextServices.h>
#endif

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif
#include "Graphics.h"
#include "StandardGetFolder.h"

#include "Startup.h" // Jago
#include <Rdevices.h>

SEXP R_LoadFromFile(FILE*, int);

/* ************************************************************************************************ */
/*                                        Global variables                                          */
/* ************************************************************************************************ */ 

static UInt32		               sSleepTime = 0;			// sleep time for WaitNextEvent()
static RgnHandle	               sMouseRgn = nil;		// mouse region for WaitNextEvent()
PicHandle                          WinPic;
Boolean                            fstart = true;
SInt16                      Help_Window = 1;
WindowPtr                   Help_Windows[MAX_NUM_H_WIN + 1];

/* ************************************************************************************************ */
/*                                Extern  Global variables                                          */
/* ************************************************************************************************ */ 
extern Graphic_Ref                 gGReference[MAX_NUM_G_WIN + 1];
extern SInt16                      gExpose;
extern Boolean                     gfinishedInput;
extern WindowPtr                   Console_Window;
extern SInt16                      Edit_Window;
extern WindowPtr                   Edit_Windows[MAX_NUM_E_WIN + 1];
extern SInt32                      gChangeAble, gbuflen;
extern char*                       gbuf;
extern int                         isGraphicWindow (WindowPtr);
extern void                        doActivate(EventRecord*);
extern WindowPtr                   Working_Window;
extern char                        InitFile[256];
extern SInt16                      gTextSize;
extern RGBColor	                   gTypeColour;


/* ************************************************************************************************ */
/*                                        Protocol                                                  */
/* ************************************************************************************************ */ 
void DoGenKeyDown ( const EventRecord *event, Boolean Console);
extern pascal	OSErr	FSpGetFullPath (const FSSpec*, short*, Handle*);
extern int R_SetOptionWidth(int w);
extern int R_ChooseFile(int isNewFile, char *fileBuf, int buflen);
extern OSErr   OpenSelection                    (FSSpecPtr theDoc);
/*
************************************************************************************************
AdjustCursor routine :
************************************************************************************************
Desciption :
Used to adjust Cursor 
************************************************************************************************
*/
void AdjustCursor ( Point mouseLoc, RgnHandle mouseRgn )
{
	WindowPtr window ;

	// by default, set mouseRgn to the whole QuickDraw coordinate plane,
	// so that we never get mouse moved events

	SetRectRgn ( mouseRgn, -SHRT_MAX, -SHRT_MAX, SHRT_MAX, SHRT_MAX ) ;

	// give text services a chance to set the cursor shape

	if ( gHasTextServices )
	{
		if ( SetTSMCursor( mouseLoc ) )
		{
			return ;
		}
	}

	// if there is a window open, give WEAdjustCursor an opportunity to set the cursor
	// WEAdjustCursor intersects mouseRgn (if supplied) with a region within which
	// the cursor is to retain its shape
	// (if the cursor is outside the view region, this is subtracted from mouseRgn )


	if ( ( window = FrontWindow ( ) ) != nil )
	{
		if ( WEAdjustCursor ( mouseLoc, mouseRgn, GetWindowWE ( window ) ) )
		{
			return ;
		}
	}


	// set the cursor to the arrow cursor

	SetCursor( & qd . arrow ) ;
}

/*
************************************************************************************************
DoMouseDown routine :
************************************************************************************************
Desciption :
Used to dispatch the mouse down event
************************************************************************************************
*/

void DoMouseDown ( const EventRecord *event )
{
	WindowPtr window;
	SInt16 partCode;

	// find out where this click when down in

	partCode = FindWindow( event->where, &window );

     
	// dispatch on partCode

	switch ( partCode )
	{
		case inMenuBar :
		{
			PrepareMenus ( ) ;
			DoMenuChoice ( MenuSelect ( event -> where ), event -> modifiers ) ;
			break;
		}

		case inSysWindow:
		{
            #if defined ( UNIVERSAL_INTERFACES_VERSION ) && ( UNIVERSAL_INTERFACES_VERSION >= 0x211 )
			    SystemClick ( event, window ) ;
            #else
			//	Universal Headers prior to version 2.1.2 define SystemClick
			//	incorrectly if STRICT_WINDOWS == 1
			    SystemClick ( event, ( WindowPtr ) window ) ;
            #endif
			break ;
		}

		case inContent:
		{
			if ( DoContent ( event -> where, event, window ) )
			{
				SelectWindow ( window ) ;
			}
			break;
		}

		case inDrag :
		{
			DoDrag ( event -> where, window ) ;
			break;
		}

		case inGrow:
		{
			DoGrow ( event -> where, window ) ;
			break;
		}

		case inGoAway:
		{   
		    if (FrontWindow() == Console_Window)
		         DoQuit( savingAsk );
			if ( TrackGoAway ( window, event -> where ) )
			{
				DoClose( closingWindow, savingAsk, window ) ;
			}
			break;
		}

		case inZoomIn:
		case inZoomOut:
		{
			if ( TrackBox ( window, event -> where, partCode ) )
			{
				DoZoom ( partCode, window ) ;
			}
			break;
		}
	}	// switch
}

/*
************************************************************************************************
DoKeyDown routine :
************************************************************************************************
Desciption :
Handle normal Key down event of the front window
************************************************************************************************
*/


void DoKeyDown ( const EventRecord *event )
{    
    WEReference we ;
    SInt32   StartPos1, EndPos1, StartPos2, EndPos2;

    if (FrontWindow() == Console_Window){
       we = GetWindowWE ( Console_Window ) ;
       WEGetSelection ( &StartPos1, &EndPos1, we );
       DoGenKeyDown ( event, false);
       WEGetSelection(&StartPos2, &EndPos2, we);
       Change_Color_Range(StartPos1, StartPos2, gTypeColour.red, gTypeColour.green, gTypeColour.blue,we);

       WESetSelection ( StartPos2, EndPos2, we );
       
    }else{
       DoGenKeyDown ( event, false);
    }
}




/*
************************************************************************************************
DoGenKeyDown routine :
************************************************************************************************
Desciption :
It will be called by the DoKeyDown event, and sometime we need to directly insert a "\r" event 
of the console window.
************************************************************************************************
*/
void DoGenKeyDown ( const EventRecord *event, Boolean Console)
{
	SInt16 key;
	Boolean isCmdKey ;
    pascal Handle  TextHdl;
    SInt32 textLength;
    WEReference we ;
    Ptr   TextPtr;
    SInt32 selStart, selEnd ;
    Point				where = {-1, -1};
    char     cha;
    char     testbuf[256];
	// extract character code from event message
	if (Console){
	   we = GetWindowWE ( Console_Window);
	}else{
       we = GetWindowWE ( FrontWindow()); 
    }
	key = ( event -> message & charCodeMask );


	// is this a command+key combo?

	isCmdKey = ( ( event -> modifiers & cmdKey ) != 0 );

	// map function keys to the equivalent command+key combos
	// note that all fuction keys generate the same code, i.e. 0x10

	if ( key == 0x10 )
	{
		isCmdKey = true ;

		switch ( ( event -> message & keyCodeMask ) >> 8 )
		{
			case keyF1:
			{   
	          //  R_ShowFile("Macintosh HD:Desktop Folder:R copy 3:R:Projects:R help", "Happy");
			    key = 'c';
				break ;
			}

			case keyF2:
			{ 
	            RnWrite(testbuf, R_ChooseFile(true, testbuf, 256));
               // key = 0 ; 
				//key = 'v';
				break ;
			}

			case keyF3:
			{   
			    RnWrite(testbuf ,R_ChooseFile(false, testbuf, 256));
				key = 0;
				break ;
			}

			case keyF4:
			{
			    ErrorDia("No warning is the best");
				key = 0 ;
				break ;
			}

			default:
			{   
			    
				key = 0 ;
				break ;
			}
		}	// switch
	}	// if

	// command + printable character combos are routed to MenuKey( )
	// but be sure to pass command + arrow key combos to WEKey( )

	if ( isCmdKey && (key >= 0x20 ) )
	{
		PrepareMenus ( );
		DoMenuChoice ( MenuKey ( key ), event -> modifiers ) ;
	}
	else
	{  
	     
	    if ((FrontWindow () == Console_Window) || (Console)){
	       WEGetSelection ( & selStart, & selEnd, we );
	       if ((selEnd < gChangeAble) && (key >=0x20))
               SysBeep(10);
           else{
	          if(key == kDel){
                if (selEnd < gChangeAble)
                   SysBeep(10);
                else
                   DoKey ( key, event ) ;
              }else
                 if(key == kBs){
                    if (selEnd <= gChangeAble) 
                        SysBeep(10);
                    else
                        DoKey ( key, event ) ;    
                 }else{
		            if ((key == 0x1E) && ( gChangeAble <= selStart))      
                          do_Down_Array();
                       
                    else{
                       if ((key == 0x1F) && ( gChangeAble <= selStart))  
                          do_Up_Array();
                       else{   
                          if(key == kReturn) WESetSelection ( WEGetTextLength(we), WEGetTextLength(we), we );
		                  DoKey ( key, event ) ;
		                  }
		            }
		            if(key == kReturn){
                       TextHdl = WEGetText(we);
                       HLock(TextHdl);
                       TextPtr = *TextHdl;
                       TextPtr = TextPtr + gChangeAble;
                       strncpy(gbuf, TextPtr,gbuflen);
                       HUnlock(TextHdl);
                       textLength = WEGetTextLength(we) - gChangeAble;
                       if (gbuflen < textLength)
                          gbuf = gbuf + gbuflen;
                       else 
                          gbuf = gbuf + textLength;
                      *gbuf = '\0';
                      gfinishedInput = !gfinishedInput;
      		         }
      		      }
      		}
        }
        if (isEditWindow(FrontWindow()) != 0){
           DoKey ( key, event ) ;
        }
     
	}
}


/*
************************************************************************************************
DoDiskEvent routine :
************************************************************************************************
Desciption :
Handle Disk event
************************************************************************************************
*/

void DoDiskEvent ( const EventRecord * event )
{
	Point dialogCorner ;

	if ( ( event -> message >> 16 ) != noErr )
	{
		SetPt ( & dialogCorner, 112, 80 ) ;
		DIBadMount ( dialogCorner, event -> message ) ;
	}
}


/*
************************************************************************************************
DoOSEvent routine :
************************************************************************************************
Desciption :
Handle OS event
************************************************************************************************
*/

void DoOSEvent ( const EventRecord * event )
{
	SInt16		osMessage ;
	WindowPtr	window ;

	// extract the OS message field from the event record

	osMessage = ( event -> message & osEvtMessageMask ) >> 24 ;

	// dispatch on osMessage

	switch ( osMessage )
	{
		case suspendResumeMessage:
		{
			if ( ( window = FrontWindow( ) ) != nil )
			{
				DoActivate( (event->message & resumeFlag) != 0, window );
			}
			break;
		}

		case mouseMovedMessage:
		{
			// nothing
			break;
		}
	}
}


/*
************************************************************************************************
DoHighLevelEvent routine :
************************************************************************************************
Desciption :
Handle High Level event
************************************************************************************************
*/
void DoHighLevelEvent( const EventRecord *event )
{
	AEProcessAppleEvent( event );
}

void DoNullEvent( const EventRecord *event )
{
#pragma unused (event)

	WindowPtr window;

	if ( ( window = FrontWindow( ) ) != nil )
	{
		WEIdle( &sSleepTime, GetWindowWE(window) );
	}
	else
	{
		sSleepTime = LONG_MAX;
	}
}


/*
************************************************************************************************
DoWindowEvent routine :
************************************************************************************************
Desciption :
Handle High Level window event
************************************************************************************************
*/
void DoWindowEvent( const EventRecord *event )
{
	WindowPtr window;

	// the message field of the event record contains the window reference

	window = (WindowPtr) event->message;

	// make sure this window is an application window; check the windowKind field

	if ( GetWindowKind( window ) != userKind )
		return;

	switch ( event->what )
	{
		case updateEvt:
		{
			DoUpdate( window );
			break;
		}

		case activateEvt:
		{
			DoActivate( ( event->modifiers & activeFlag) != 0, window );
			break;
		}
	}
}


/*
************************************************************************************************
ProcessEvent routine :
************************************************************************************************
Desciption :
It is used in the Event loop, when you call Process Event, it will capture the next event that
available, and dispatch it to the corresponding routine.
************************************************************************************************
*/
void ProcessEvent( void )
{
	EventRecord event;
	Boolean gotEvent, SIOUXDidEvent, haveResize = false;
	WindowPtr windowPtr;
	DevDesc  *dd;
	SInt16 Console_Width, NumofChar;
	GrafPtr savePort;
    RgnHandle cursorRgn;
    
 /*   cursorRgn = NewRgn();
   */ 
	gotEvent = WaitNextEvent( everyEvent, &event, sSleepTime, sMouseRgn );
    
/*    if(gotEvent)
     SIOUXDidEvent = SIOUXHandleOneEvent(&event);
  */
    
  /*  if(SIOUXDidEvent)
     return;
    */
      
	// give text services a chance to intercept this event
	// if TSMEvent( ) handles the event, it will set event.what to nullEvent
    if (gExpose) {
		dd = (DevDesc*)gGReference[gExpose].devdesc;
		dd-> dp.resize(dd);
		playDisplayList(dd);
		haveResize = true;
		gExpose = false;
	}
    if (fstart) {
		Console_Width = (Console_Window ->portRect).right - (Console_Window ->portRect).left ;
		GetPort(&savePort);
		SetPort(Console_Window);
		TextFont(4);
		TextSize(gTextSize);
		NumofChar = (int)(((Console_Width - 15) / CharWidth('M')) -0.5) ; 
        R_SetOptionWidth(NumofChar);
		SetPort(savePort);
		fstart = false;
	}
	if (gHasTextServices) {
		TSMEvent(&event);
	}

	// adjust cursor shape and set mouse region
	// (we assume event.where is the current mouse position in global coordinates
	// if event.what <= osEvt; high-level events store the event ID there)

	if ( event.what <= osEvt ) {
		AdjustCursor( event.where, sMouseRgn );
	}

	// dispatch on event.what

	switch( event.what ) {
	
	case nullEvent:
		DoNullEvent( &event );
		break;

	case mouseDown:
		DoMouseDown( &event);
		break;

	case keyDown:
	case autoKey:
		DoKeyDown( &event );
		break;

	case updateEvt:
		windowPtr = (WindowPtr) (&event)->message;
		if (isGraphicWindow(windowPtr)) {
		}
		if (!haveResize)
			DoWindowEvent(&event);
		break;
			
	case activateEvt:
		windowPtr = (WindowPtr) (&event)->message;
		if (isGraphicWindow(windowPtr)) {
			doActivate(&event);
		}
		DoWindowEvent(&event);
		break;

	case diskEvt:
		DoDiskEvent( &event );
		break;

	case osEvt:
		DoOSEvent( &event );
		break;

	case kHighLevelEvent:
		DoHighLevelEvent( &event );
		break;
			
	} // switch

	if (gotEvent) {
		sSleepTime = 0;  // force early idle after non-idle event
	}
}

/*
************************************************************************************************
GotRequiredParams routine :
************************************************************************************************
*/
OSErr GotRequiredParams( const AppleEvent *ae )
{
	DescType actualType;
	Size actualSize;
	OSErr err;

	err = AEGetAttributePtr( ae, keyMissedKeywordAttr, typeWildCard, &actualType, nil, 0, &actualSize );

	return	( err == errAEDescNotFound ) ? noErr :
			( err == noErr ) ? errAEParamMissed : err;
}


/*
************************************************************************************************
HandleOpenDocument routine :
************************************************************************************************
Desciption :
This Event will be generate when you click on a R file icon. 
This event can only be depatched by the ProcessEvent routine. Thus, even you click on the file
icon of R to start R, this event will not be catch until the R_readConsole start.
************************************************************************************************
*/
static pascal OSErr	HandleOpenDocument( const AppleEvent *ae, AppleEvent *reply, SInt32 refCon )
{
#pragma unused ( reply, refCon )
	AEDescList		docList;
	AEKeyword		keyword;
	DescType		actualType;
	Size			actualSize;
	SInt32			numberOfDocuments, i;
	FSSpec			fileSpec;
	OSErr			err;
	FInfo           fileInfo;
    SInt16          pathLen;
    Handle          pathName;
    FILE            *fp;
    SEXP 			img, lst;
    
	docList.descriptorType = typeNull;
	docList.dataHandle = nil;
	// extract direct parameter from the Apple event
	if ( ( err = AEGetParamDesc( ae, keyDirectObject, typeAEList, &docList ) ) != noErr )
		goto cleanup;

	// perform the recommended check for additional required parameters

	if ( ( err = GotRequiredParams( ae ) ) != noErr )	 
		goto cleanup;

    i = 1;
    if ( ( err = AEGetNthPtr( &docList, i, typeFSS, &keyword, &actualType,
		&fileSpec, sizeof( fileSpec ), &actualSize ) ) != noErr )
			goto cleanup;

    err = FSpGetFInfo(&fileSpec, &fileInfo);
    if (err != noErr) goto cleanup;
	
    if (fileInfo.fdType == 'PICT'){
        error("No Picture Can be load in this version of R\n");
    }
    if (fileInfo.fdType == 'TEXT'){
        DoNew( );    
        ReadTextFile ( &fileSpec, GetWindowWE ( Edit_Windows[Edit_Window-1] ) );

    }else
	if (fileInfo.fdType == 'ROBJ'){
       FSpGetFullPath(&fileSpec, &pathLen, &pathName);
       HLock((Handle) pathName);
       strncpy(InitFile, *pathName, pathLen);
       InitFile[pathLen] = '\0';
       HUnlock((Handle) pathName);
  
  
  	if(!(fp = fopen(InitFile, "rb"))) { /* binary file */
  	    RWrite("File cannot be opened !");
  		    /* warning here perhaps */
	    return;
	}
#ifdef OLD
	FRAME(R_GlobalEnv) = R_LoadFromFile(fp, 1);
#else
	PROTECT(img = R_LoadFromFile(fp, 1));
	switch (TYPEOF(img)) {
	case LISTSXP:
	    while (img != R_NilValue) {
		defineVar(TAG(img), CAR(img), R_GlobalEnv);
		img = CDR(img);
	    }
	    break;
	case VECSXP:
	    for (i = 0; i < LENGTH(img); i++) {
//		lst = VECTOR(img)[i];
		lst = VECTOR_ELT(img,i);
		while (lst != R_NilValue) {
		    defineVar(TAG(lst), CAR(lst), R_GlobalEnv);
		    lst = CDR(lst);
		}
	    }
	    break;
	}
        UNPROTECT(1);
#endif
          
	 
//	   if(!(fp = fopen(InitFile,"r"))) {
//	         /* warning here perhaps */
//	         RWrite("File cannot be opened !");
//	        return;
//	   }
//	   FRAME(R_GlobalEnv) = R_LoadFromFile(fp,0);
//	   fclose(fp);



    }
          
  
   
cleanup:
    return err;

}



/*
************************************************************************************************
HandleQuitApplication routine :
************************************************************************************************
Desciption :
This event will be execute when the applcation exit normally
************************************************************************************************
*/
static pascal OSErr	HandleQuitApplication( const AppleEvent *ae, AppleEvent *reply, SInt32 refCon )
{
#pragma unused (reply, refCon)

	AEKeyword		optKey;
	DescType		actualType;
	Size			actualSize;
	SavingOption	saving;
	OSErr			err;

	// default saving option is savingAsk;

	saving = savingAsk;

	// extract optional save options

	if ( ( err = AEGetParamPtr( ae, keyAESaveOptions, typeEnumerated, &actualType, &optKey, sizeof( optKey ), &actualSize ) ) == noErr )
	{
		if ( optKey == kAEYes )
		{
			saving = savingYes;
		}
		else if (optKey == kAENo )
		{
			saving = savingNo;
		}
		else if ( optKey != kAEAsk )
		{
			err = paramErr;	// for want of a better code
			goto cleanup;
		}
	}

	// perform the recommended check for additional required parameters

	if ( ( err = GotRequiredParams( ae ) ) != noErr )
		goto cleanup;

	// actually do the quit stuff

	err = DoQuit( saving );

cleanup:
	return err;
}

OSErr InitializeEvents( void )
{
	OSErr	err;

	// allocate space for the mouse region

	sMouseRgn = NewRgn( );

	// install AppleEvent handlers for the Required Suite

	if ( ( err = AEInstallEventHandler( kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerProc( HandleOpenDocument ), kDoOpen, false ) ) != noErr )
		goto cleanup;

	if ( ( err = AEInstallEventHandler( kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerProc( HandleOpenDocument ), kDoPrint, false ) ) != noErr )
		goto cleanup;

	if ( ( err = AEInstallEventHandler( kCoreEventClass, kAEQuitApplication, NewAEEventHandlerProc( HandleQuitApplication ), 0, false ) ) != noErr )
		goto cleanup;

	// install Apple event handlers for a subset of the Core suite

	if ( ( err = InstallCoreHandlers( ) ) != noErr )
		goto cleanup;
    
	// install Apple event handlers for inline input
	if ( ( err = WEInstallTSMHandlers( ) ) != noErr )
		goto cleanup;

cleanup:
	return err;
}



void R_startBrowser(char *fileName){
   FSSpec HelpFile;
   Str255 name;
   name[0] = strlen(fileName);
   strncpy((char *)(&name[1]), fileName, name[0]);
   FSMakeFSSpec(0,0,name,&HelpFile);
   OpenSelection(&HelpFile);

}

