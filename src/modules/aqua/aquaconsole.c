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

void 	Raqua_StartConsole(void);
void 	Raqua_WriteConsole(char *buf, int len);
int 	Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);
		     

static const EventTypeSpec	REvents[] =
{
	{ kEventClassTextInput, kEventTextInputUnicodeForKeyEvent }
};

void R_CarbonProcessEvents(void);

WindowRef			ConsoleWindow=NULL;

void Raqua_StartConsole(void)
{
    IBNibRef 			nibRef = NULL;
	OSErr				err = noErr;
	CFURLRef    		bundleURL = NULL;
 	CFBundleRef 		RBundle = NULL;

/*
    bundleURL = CFURLCreateWithFileSystemPath( 
                            kCFAllocatorDefault,
                            CFSTR("/Users/jago/NewR.bundle"), 
                            kCFURLPOSIXPathStyle, 
                            TRUE);

    if(bundleURL == NULL)
     goto fine;

    RBundle = CFBundleCreate (kCFAllocatorDefault, bundleURL);
    if(RBundle==NULL)
     goto fine;

    err = CreateNibReferenceWithCFBundle (RBundle, 
                            CFSTR("main"), &nibRef);
*/   
    err = CreateNibReference(CFSTR("main"), &nibRef);
  //  fprintf(stderr,"\n CreateNibErr=%d",err);
    if(err != noErr) 
     goto fine;
     
    err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"));
  //  fprintf(stderr,"\n CreateMenuBarNibErr=%d",err);
    if(err != noErr)
     goto fine;
    
    
 	err = CreateWindowFromNib(nibRef,CFSTR("MainWindow"),&ConsoleWindow);
 //   fprintf(stderr,"\n CreateWinFromNibErr=%d",err);
    if(err != noErr)
     goto fine;
    
    if(nibRef)
     DisposeNibReference(nibRef);

    ShowWindow(ConsoleWindow);

    InitCursor();
    
	/* Check for availability of MLTE api */
	if (TXNVersionInformation == (void*)kUnresolvedCFragSymbolAddress){ 
	//	  fprintf(stderr,"\nTXNVersionInformation error\n");							
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
				// sets the state of the scrollbars so they are drwan correctly
				err = TXNActivate(RConsoleObject, frameID, kScrollBarsSyncWithFocus);
				if (err != noErr){ // Check for availability of MLTE api
		       //  fprintf(stderr,"\nNO TXNACti error=%d\n",err);							
                 goto fine;
		        }
			
				err = SetWindowProperty(ConsoleWindow,'GRIT','tFrm',sizeof(TXNFrameID),&frameID);
				err = SetWindowProperty(ConsoleWindow,'GRIT','tObj',sizeof(TXNObject),&RConsoleObject);
			}

			
			//DoJustification(window, iDefaultJustify); 	// set justification default for new doc
	
		} 
	
	if(err == noErr){
	 WeHaveConsole =true;
	 InstallStandardEventHandler(GetApplicationEventTarget());
	}
	else
	 WeHaveConsole =false; 
	}


fine:
    if(bundleURL)
     CFRelease( bundleURL );
    if(RBundle)
     CFRelease( RBundle ); 
	return;
}


/* 
   alla fine si deve chiamare TXNTerminateTextension(); 
   prima di ExitToShell();
*/
int RITERS = 0;
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
/*    if(RITERS ==100){
         RunApplicationEventLoop(); 
         RITERS=0;
         }
     RITERS++;
*/
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
      R_CarbonProcessEvents();   
 
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

void R_CarbonProcessEvents(void)
{
 EventRef REvent;
 UInt32 EventKind, EventClass;
 OSStatus err = noErr;
 UInt32  RKeyCode;
 
 ReceiveNextEvent(0, NULL,kEventDurationForever,true,
            &REvent);

            
 EventKind = GetEventKind(REvent);
 EventClass = GetEventClass(REvent);
 
 //fprintf(stderr,"\n CarbonProcess class=%d kind=%d",EventClass,EventKind);
 switch(EventClass){
 
 case kEventClassTextInput:
 //fprintf(stderr,"\nkEventClassTextInput");
 break;
 
 case kEventClassMouse:
// fprintf(stderr,"\nkEventClassMouse");
 break; 
 
 case kEventClassKeyboard:
// fprintf(stderr,"\nkEventClassKeyboard");
 err= GetEventParameter (REvent, kEventParamKeyCode,typeUInt32, NULL, sizeof(RKeyCode), NULL, &RKeyCode);
// fprintf(stderr,"\n err=%d, chr=%d",err,RKeyCode);
 if( (RKeyCode == 36) && (EventKind == kEventRawKeyUp) ){
 // fprintf(stderr,"\n enter key");
  InputFinished = true;
  }
 break;
 
 case kEventClassApplication:
 //fprintf(stderr,"\nkEventClassApplication");
 break;
 
 case kEventClassWindow:
 //fprintf(stderr,"\nkEventClassWindow");
 break;
 
 case kEventClassCommand:
 //fprintf(stderr,"\nkEventClassCommand");
 break;
 
 case kEventClassAppleEvent:
 //fprintf(stderr,"\nkEventClassAppleEvent");
 break;
 
 case kEventClassMenu:
 //fprintf(stderr,"\nkEventClassMenu");
 break;
 
 case kEventClassControl:
 //fprintf(stderr,"\nkEventClassControl");
 break;

 default:
 //fprintf(stderr,"\n evento diverso");
 break;
 }

 SendEventToEventTarget (REvent, GetEventDispatcherTarget());
 ReleaseEvent(REvent);
 
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

#endif


#endif /* __AQUA_CONSOLE__ */
