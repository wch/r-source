/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka
 *			      and the R Development Core Team
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
 */

/* See system.txt for a description of functions
 */
#ifndef __AQUA_CONSOLE__
#define __AQUA_CONSOLE__

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


/* necessary for some (older, i.e., ~ <= 1997) Linuxen, and apparently
   also some AIX systems.
   */
#ifndef FD_SET
# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* isatty() */
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Rdevices.h"		/* KillAllDevices() [nothing else?] */

#define __SYSTEM__
#include "../../unix/devUI.h" /* includes Startup.h */
#undef __SYSTEM__

#include "../../unix/Runix.h"


#ifdef HAVE_AQUA
#include <Carbon/Carbon.h>

OSStatus 	InitMLTE(void);
TXNObject	RConsoleObject = NULL;
bool 		WeHaveConsole = false;
bool 		InputFinished = false;

WindowRef	RAboutWindow;
pascal void RAboutHandler(WindowRef window);
#define kRAppSignature '????'

void 	Raqua_StartConsole(void);
void 	Raqua_WriteConsole(char *buf, int len);
int 	Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);
		     
#define kRAboutWinCmd 'abou'
#define kRVersionInfoID 132

static const EventTypeSpec	REvents[] =
{
	{ kEventClassTextInput, kEventTextInputUnicodeForKeyEvent }
};

static const EventTypeSpec	aboutSpec =
	{ kEventClassWindow, kEventWindowClose };


static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon); 
static OSStatus KeybHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
pascal OSStatus RAboutWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);

WindowRef			ConsoleWindow=NULL;

static const EventTypeSpec KeybEvents[] =  { kEventClassKeyboard, kEventRawKeyUp };


static const EventTypeSpec kCommandEvents[] =
{
	{ kEventClassCommand, kEventCommandProcess },
	{ kEventClassCommand, kEventCommandUpdateStatus }
};


void Raqua_StartConsole(void)
{
    IBNibRef 			nibRef = NULL;
	OSErr				err = noErr;
	CFURLRef    		bundleURL = NULL;
 	CFBundleRef 		RBundle = NULL;
        MenuRef  demoMenuRef = NULL;
/*    char                buf[300]; */
    
    /*    sprintf(buf,"%s/aqua.bundle",R_HomeDir());
     fprintf(stderr,"\n buf=%s",buf); 
    bundleURL = CFURLCreateWithFileSystemPath(kCFAllocatorDefault,
                      CFStringCreateWithCString(kCFAllocatorDefault,buf,
                       kCFStringEncodingMacRoman ),
                       kCFURLPOSIXPathStyle, TRUE);

    if(bundleURL == NULL)
     goto fine;

    RBundle = CFBundleCreate (kCFAllocatorDefault, bundleURL);
    if(RBundle==NULL)
     goto fine;

    err = CreateNibReferenceWithCFBundle (RBundle, 
                            CFSTR("main"), &nibRef);
*/   

    err = CreateNibReference(CFSTR("main"), &nibRef);
/*    fprintf(stderr,"\n CreateNibErr=%d",err); */
    if(err != noErr) 
     goto fine;
      
      err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"));
/*    fprintf(stderr,"\n CreateMenuBarNibErr=%d",err); */
    if(err != noErr)
     goto fine;
     
 	err = CreateWindowFromNib(nibRef,CFSTR("MainWindow"),&ConsoleWindow);
 /*   fprintf(stderr,"\n CreateWinFromNibErr=%d",err); */
    if(err != noErr)
     goto fine;
    
    if(nibRef)
     DisposeNibReference(nibRef);

    ShowWindow(ConsoleWindow);

    InitCursor();
    
	/* Check for availability of MLTE api */
	if (TXNVersionInformation == (void*)kUnresolvedCFragSymbolAddress){ 
	/*	  fprintf(stderr,"\nTXNVersionInformation error\n"); */							
          goto fine;
		}
		
	/* default settings for MLTE */	
    err = InitMLTE(); 							
    if(err!=noErr){ // Check for availability of MLTE api
		  fprintf(stderr,"\nNO MLTE error=%d\n",err);							
          goto fine;
	}


	if (ConsoleWindow != NULL) 
	{
		TXNFrameID		frameID	= 0;
		WindowPtr		paramWindow = NULL;
		TXNFrameOptions	frameOptions;
		
		frameOptions = kTXNShowWindowMask; 
		frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNDrawGrowIconMask;
		
		paramWindow = ConsoleWindow;

		err = TXNNewObject(	NULL, 
								paramWindow, 
								nil, 
								frameOptions,
								kTXNTextEditStyleFrameType,
								kTXNTextensionFile,
								kTXNSystemDefaultEncoding,
								&RConsoleObject,
								&frameID, 
								0);
		
		if (err == noErr)
		{		
			if (RConsoleObject != NULL) 
			{
				/* sets the state of the scrollbars so they are drawn correctly */
				err = TXNActivate(RConsoleObject, frameID, kScrollBarsSyncWithFocus);
				if (err != noErr){ /* Check for availability of MLTE api */		
                 goto fine;
		        }
			
				err = SetWindowProperty(ConsoleWindow,'GRIT','tFrm',sizeof(TXNFrameID),&frameID);
				err = SetWindowProperty(ConsoleWindow,'GRIT','tObj',sizeof(TXNObject),&RConsoleObject);
			}

		} 
	
	if(err == noErr){
	 WeHaveConsole =true;
	 InstallStandardEventHandler(GetApplicationEventTarget());
         err = InstallApplicationEventHandler( KeybHandler,
             GetEventTypeCount( KeybEvents ), KeybEvents, 0, NULL);
         err = AEInstallEventHandler( kCoreEventClass, kAEQuitApplication,
                 NewAEEventHandlerUPP(QuitAppleEventHandler), 0, false );
//         InstallEventHandler( GetWindowEventTarget( ConsoleWindow ), CommandHandler, GetEventTypeCount( kCommandEvents ),
//			kCommandEvents, ConsolWindow, NULL );

  //      err = CreateWindowFromNib(nibRef,CFSTR("AboutWindow"),&RAboutWindow);
  //      fprintf(stderr,"\n AboutWin err=%d",err);
        
     //   InstallWindowEventHandler(RAboutWindow, NewEventHandlerUPP(RAboutWinHandler), 1, &aboutSpec, (void *)RAboutWindow, NULL);

	}
	else
	 WeHaveConsole =false; 
	}

    SelectWindow(ConsoleWindow);

fine:
    if(bundleURL)
     CFRelease( bundleURL );
    if(RBundle)
     CFRelease( RBundle ); 
	return;
}

/* BEWARE: before quitting R via ExitToShell() call TXNTerminateTextension() */

void Aqua_RWrite(char *buf);
TXNOffset LastStartOffset = 0;

void Raqua_WriteConsole(char *buf, int len)
{
    OSStatus err;
    TXNOffset oStartOffset; 
    TXNOffset oEndOffset;

    if(WeHaveConsole){  
     TXNGetSelection (RConsoleObject, &oStartOffset, &oEndOffset);
     err =  TXNSetData (RConsoleObject, kTXNTextData, buf, strlen(buf), oStartOffset, oEndOffset);
     LastStartOffset=oEndOffset;
    }
    else{
     fprintf(stderr,"%s", buf);
    }
}


OSStatus InitMLTE(void)
{
	OSStatus							status=noErr;
	TXNMacOSPreferredFontDescription	defaults; 
	TXNInitOptions options;

	defaults.fontID = NULL; 
	defaults.pointSize = kTXNDefaultFontSize;
  	defaults.encoding = CreateTextEncoding(kTextEncodingMacRoman, kTextEncodingDefaultVariant,
  	 kTextEncodingDefaultFormat);
  	defaults.fontStyle 	= kTXNDefaultFontStyle;

	options = kTXNWantMoviesMask | kTXNWantSoundMask | kTXNWantGraphicsMask;

	status = TXNInitTextension(&defaults, 1, options);
	return(status);
}



#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#  define min(a,b) ((a) < (b) ? (a) : (b))
#endif

int Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory)
{
   OSStatus err = noErr;
   Handle DataHandle;
   TXNOffset oStartOffset; 
   TXNOffset oEndOffset;
   int i, lg=0;
         
   if(!InputFinished)
     Aqua_RWrite(prompt);

    while(!InputFinished)
     RunApplicationEventLoop();
    
   if(InputFinished){
    TXNGetSelection (RConsoleObject, &oStartOffset, &oEndOffset);
    //fprintf(stderr,"\n range = %d -%d=%d", oEndOffset, LastStartOffset, 
    // oEndOffset-LastStartOffset);
     err = TXNGetDataEncoded(RConsoleObject, LastStartOffset, oEndOffset, &DataHandle, kTXNTextData);
//    fprintf(stderr,"\n err GetData=%d", err );
     lg = min(len,oEndOffset - LastStartOffset);
     HLock( DataHandle );
     for(i=0; i<lg-1; i++)
       buf[i] = (*DataHandle)[i];
     HUnlock( DataHandle );
     if(DataHandle)
      DisposeHandle( DataHandle );
	
     buf[lg-1] = '\n';
     buf[lg] = '\0';

 //  fprintf(stderr,"\t data=[%s],len=%d,lg=%d", buf,strlen(buf),lg);
   
     InputFinished = false;

   }
 
  
   return(1);
}


void Aqua_RWrite(char *buf)
{
    OSStatus err;
    TXNOffset oStartOffset; 
    TXNOffset oEndOffset;

    if(WeHaveConsole){  
     TXNGetSelection (RConsoleObject, &oStartOffset, &oEndOffset);
     err =  TXNSetData (RConsoleObject, kTXNTextData, buf, strlen(buf), oStartOffset, oEndOffset);
     TXNGetSelection (RConsoleObject, &oStartOffset, &oEndOffset);
     LastStartOffset=oEndOffset;
    }
}




/* Indicate that input is coming from the console */
void Raqua_ResetConsole ()
{
}

/* Stdio support to ensure the console file buffer is flushed */
void Raqua_FlushConsole ()
{
}


/* Reset stdin if the user types EOF on the console. */
void Raqua_ClearerrConsole ()
{
}

static OSStatus KeybHandler(EventHandlerCallRef inCallRef, EventRef REvent, void *inUserData)
{
 OSStatus	err = eventNotHandledErr;
 UInt32		RKeyCode;

 /* make sure that we're processing a keyboard event */
 if ( GetEventClass( REvent ) == kEventClassKeyboard )
 {
  switch ( GetEventKind( REvent ) )
  {
   case kEventRawKeyUp:
    err = GetEventParameter (REvent, kEventParamKeyCode, typeUInt32, NULL, sizeof(RKeyCode), NULL, &RKeyCode);
    if( RKeyCode == 36 ){ /* we check wheter return key is released */
     InputFinished = true;
      QuitApplicationEventLoop();

    }
   break;
//   case kEventRawKeyDown:
//    err = GetEventParameter (REvent, kEventParamKeyCode, typeUInt32, NULL, sizeof(RKeyCode), NULL, &RKeyCode);
//    fprintf(stderr,"\n key=%d",RKeyCode);
//   break;
   
   default:
   break;
   
  }
  
 }
}
 

pascal void RAboutHandler(WindowRef window)
{
    CFStringRef	text;
    CFStringRef	appBundle;
    ControlID	versionInfoID = {kRAppSignature, kRVersionInfoID};
    ControlRef	versionControl;
    ControlFontStyleRec	controlStyle;
    
    appBundle = CFBundleGetMainBundle();
    text = (CFStringRef) CFBundleGetValueForInfoDictionaryKey(appBundle, CFSTR("CFBundleGetInfoString"));
    if((text== CFSTR(" ")) || (text==NULL))
        text = CFSTR("Nameless Application");
    GetControlByID(window, &versionInfoID, &versionControl);
    SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    controlStyle.flags = kControlUseJustMask;
    controlStyle.just = teCenter;
    SetControlFontStyle(versionControl, &controlStyle);
    ShowWindow(window);
    SelectWindow(window);    
}
 
pascal OSStatus RAboutWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData)
{
    OSStatus result = eventNotHandledErr;
    UInt32	eventKind;
    
    eventKind = GetEventKind(event);
    if( eventKind == kEventWindowClose)
    {
     HideWindow( (WindowRef)userData );
     result = noErr;
    }
    return result;
}
 
static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon) 
{
    fprintf(stderr,"\n quit app");
} 
  
 
#endif /* HAVE_AQUA */

#endif /* __AQUA_CONSOLE__ */
