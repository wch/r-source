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
#include "Graphics.h"
#include "Fileio.h"
#include "Rdevices.h"		/* KillAllDevices() [nothing else?] */


#define __SYSTEM__
#include "../../unix/devUI.h" /* includes Startup.h */
#undef __SYSTEM__

#include "../../unix/Runix.h"


#ifdef HAVE_AQUA
#include <Carbon/Carbon.h>


OSStatus 	InitMLTE(void);
TXNObject	RConsoleOutObject = NULL;
TXNObject	RConsoleInObject = NULL;
bool 		WeHaveConsole = false;
bool 		InputFinished = false;
TXNFrameID		OutframeID	= 0;
TXNFrameID		InframeID	= 0;

WindowRef	RAboutWindow=NULL;
pascal void RAboutHandler(WindowRef window);
#define kRAppSignature '0FFF'
int 	RFontSize = 12;

void RSetTab(int tabsize);
void RSetFontSize(int size);

/* Items for the Tools menu */
#define kRCmdFileShow		'fshw'

/* Items for the Tools menu */
#define kRCmdShowWSpace		'dols'
#define kRCmdClearWSpace	'dorm'
#define kRCmdBrowseWSpace	'shwb'
#define kRCmdLoadWSpace		'ldws'
#define kRCmdSaveWSpace		'svws'
#define kRCmdLoadHistory	'hstl'
#define kRCmdSaveHistory	'hsts'
#define kRCmdShowHistory	'hstw'
#define kRCmdChangeWorkDir	'scwd'
#define kRCmdShowWorkDir	'shwd'
#define kRCmdResetWorkDir	'rswd'

/* items for the Packages menu */
#define kRCmdInstalledPkgs	'ipkg'
#define kRCmdAvailDatsets	'shdt'
#define kRCmdInstallFromCRAN	'cran'
#define kRCmdInstallFromBioC	'bioc'
#define kRCmdInstallFromSrc	'ipfs'

/* items in the Help Menu */
#define kRHelpStart		'rhlp'
#define kRHelpOnTopic		'rhot'
#define kRSearchHelpOn		'rsho'
#define kRExampleRun		'rexr'
                
                
                
static pascal OSStatus
RCmdHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
static pascal OSStatus
RWinHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
void RescaleInOut(double prop);

OSErr DoSelectDirectory( void );
OSStatus SelectFile(FSSpec *outFSSpec,  char *Title);
OSStatus FSPathMakeFSSpec(const UInt8 *path, FSSpec *spec, Boolean *isDirectory);
OSStatus FSMakePath(SInt16 volRefNum, SInt32 dirID, ConstStr255Param name, UInt8 *path,
	UInt32 maxPathSize);
OSErr FSMakeFSRef(FSVolumeRefNum volRefNum, SInt32 dirID, ConstStr255Param name, FSRef *ref);
        

int RAqua_R_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager);


void 	Raqua_StartConsole(void);
void 	Raqua_WriteConsole(char *buf, int len);
int 	Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);
int NewHelpWindow(char *fileName, char *title, char *WinTitle);
		     
void consolecmd(char *cmd);
void RSetColors(void);
                   
#define kRVersionInfoID 132

static const EventTypeSpec	REvents[] =
{
	{ kEventClassTextInput, kEventTextInputUnicodeForKeyEvent }
};

static const EventTypeSpec	aboutSpec =
	{ kEventClassWindow, kEventWindowClose };


static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon); 
static pascal OSStatus KeybHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
static pascal OSStatus RAboutWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);
OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );

WindowRef			ConsoleWindow=NULL;

static const EventTypeSpec KeybEvents[] =  { kEventClassKeyboard, kEventRawKeyUp };


static const EventTypeSpec	RCmdEvents[] =
{
	{ kEventClassCommand, kEventCommandProcess },
	{ kEventClassCommand, kEventCommandUpdateStatus }
};

static const EventTypeSpec	RGlobalWinEvents[] =
{
        { kEventClassWindow, kEventWindowBoundsChanged }
};

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

MenuRef HelpMenu = NULL; /* Will be the Reference to Apple's Help Menu */
static 	short 	RHelpMenuItem=-1;
static 	short 	RTopicHelpItem=-1;
static	short 	RunExampleItem=-1;
static	short	SearchHelpItem=-1;
static  short  	PreferencesItem=-1;

TXNControlTag	ROutTag[] = {kTXNIOPrivilegesTag, kTXNNoUserIOTag, kTXNWordWrapStateTag};
TXNControlData  ROutData[] = {kTXNReadWrite, kTXNReadOnly, kTXNNoAutoWrap};

TXNControlTag	RInTag[] = { kTXNWordWrapStateTag};
TXNControlData  RInData[] = {kTXNNoAutoWrap};
       
TXNControlTag	RHelpTag[] = {kTXNIOPrivilegesTag, kTXNNoUserIOTag, kTXNWordWrapStateTag};
TXNControlData  RHelpData[] = {kTXNReadWrite, kTXNReadOnly, kTXNNoAutoWrap};
       
void Raqua_StartConsole(void)
{
    IBNibRef 	nibRef = NULL;
    OSErr	err = noErr;
    CFURLRef    bundleURL = NULL;
    CFBundleRef RBundle = NULL;
    Str255	menuStr;
    err = CreateNibReference(CFSTR("main"), &nibRef);
    if(err != noErr) 
     goto noconsole;
      
    err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"));
    if(err != noErr)
     goto noconsole;

     err = CreateWindowFromNib(nibRef,CFSTR("MainWindow"),&ConsoleWindow);
    if(err != noErr)
     goto noconsole;
    
    err = CreateWindowFromNib(nibRef,CFSTR("AboutWindow"),&RAboutWindow);
   if(err != noErr)
     goto noconsole;
   
    if(nibRef)
     DisposeNibReference(nibRef);
  
    ShowWindow(ConsoleWindow);

    InitCursor();
    
    if (TXNVersionInformation == (void*)kUnresolvedCFragSymbolAddress)
        goto noconsole;

    if( InitMLTE() != noErr )
     goto noconsole;


    if (ConsoleWindow != NULL){
        TXNFrameOptions	frameOptions;
        Rect OutFrame, InFrame, WinFrame;
                
        GetWindowPortBounds (ConsoleWindow,&WinFrame);
        SetRect(&OutFrame,0,0,WinFrame.right,WinFrame.bottom-110);
        SetRect(&InFrame,0,WinFrame.bottom-100,WinFrame.right,WinFrame.bottom);
                
        frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask; 
        frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;
		

        err = TXNNewObject(NULL, ConsoleWindow, &OutFrame, frameOptions, kTXNTextEditStyleFrameType,
                            kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleOutObject,
                            &OutframeID, 0);
		
        frameOptions = kTXNShowWindowMask | kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNDrawGrowIconMask;
		
        err = TXNNewObject(NULL, ConsoleWindow, &InFrame, frameOptions, kTXNTextEditStyleFrameType,
                            kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleInObject,
                            &InframeID, 0);

        if (err == noErr){		
            if ( (RConsoleOutObject != NULL) && (RConsoleInObject != NULL) ){
                    /* sets the state of the scrollbars so they are drawn correctly */
                err = TXNActivate(RConsoleOutObject, OutframeID, kScrollBarsAlwaysActive);
                err = TXNActivate(RConsoleInObject, InframeID, kScrollBarsAlwaysActive);
                if (err != noErr)
                    goto noconsole;
		        
			
                err = SetWindowProperty(ConsoleWindow,'GRIT','tFrm',sizeof(TXNFrameID),&OutframeID);
                err = SetWindowProperty(ConsoleWindow,'GRIT','tObj',sizeof(TXNObject),&RConsoleOutObject);
                err = SetWindowProperty(ConsoleWindow,'GRIT','tFrm',sizeof(TXNFrameID),&InframeID);
                err = SetWindowProperty(ConsoleWindow,'GRIT','tObj',sizeof(TXNObject),&RConsoleInObject);
            }

        } 
	
	if(err == noErr){
	 WeHaveConsole = true;
         RescaleInOut(0.8);
         TXNSetTXNObjectControls(RConsoleOutObject, false, 3, ROutTag, ROutData);
         TXNSetTXNObjectControls(RConsoleInObject, false, 1, RInTag, RInData);
        
  	 InstallStandardEventHandler(GetApplicationEventTarget());
         err = InstallApplicationEventHandler( KeybHandler, GetEventTypeCount(KeybEvents), KeybEvents, 0, NULL);
         err = InstallApplicationEventHandler( NewEventHandlerUPP(RCmdHandler), GetEventTypeCount(RCmdEvents),
                                                RCmdEvents, 0, NULL);
	    
         err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)ConsoleWindow, NULL);
                                          
         err = InstallApplicationEventHandler( NewEventHandlerUPP(RWinHandler), GetEventTypeCount(RGlobalWinEvents),
                                                RGlobalWinEvents, 0, NULL);
         err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, NewAEEventHandlerUPP(QuitAppleEventHandler), 
                                    0, false );

        TXNFocus(RConsoleOutObject,true);
        InstallWindowEventHandler(RAboutWindow, NewEventHandlerUPP(RAboutWinHandler), 1, &aboutSpec, 
                                (void *)RAboutWindow, NULL);
        RSetColors();
     
        }
        else
        WeHaveConsole = false; 
    }


    SelectWindow(ConsoleWindow);
    /* SetsUp additional Help Menu items */
    HMGetHelpMenu(&HelpMenu,NULL);
    if (HelpMenu != nil) {
                CopyCStringToPascal("R Help", menuStr);
		AppendMenu(HelpMenu, menuStr);
		RHelpMenuItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, RHelpMenuItem, kRHelpStart); 
                SetMenuItemCommandKey(HelpMenu, RHelpMenuItem, false, '?');
 
                CopyCStringToPascal("Help On Topic...", menuStr);
		AppendMenu(HelpMenu, menuStr);
		RTopicHelpItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, RTopicHelpItem, kRHelpOnTopic); 
 
                CopyCStringToPascal("Search Help On...", menuStr);
		AppendMenu(HelpMenu, menuStr);
		SearchHelpItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, SearchHelpItem, kRSearchHelpOn); 

                CopyCStringToPascal("Run An Example...", menuStr);
		AppendMenu(HelpMenu, menuStr);
		RunExampleItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, RunExampleItem, kRExampleRun); 

	}
  
noconsole:
    if(bundleURL)
     CFRelease( bundleURL );
    if(RBundle)
     CFRelease( RBundle ); 
	return;
}

/* BEWARE: before quitting R via ExitToShell() call TXNTerminateTextension() */


void Aqua_RWrite(char *buf);

RGBColor RFGOutColor = { 0x0000, 0x0000, 0xffff};
RGBColor RFGInColor = { 0xffff, 0x0000, 0x0000};
RGBColor RBGOutColor = {0xeeee, 0xeeee, 0xeeee};
RGBColor RBGInColor = { 0xeeff, 0xeeff, 0xeeff};

TXNTypeAttributes ROutAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &RFGOutColor}};
TXNTypeAttributes RInAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &RFGInColor}};

void Raqua_WriteConsole(char *buf, int len)
{
    OSStatus err;
    TXNOffset oStartOffset; 
    TXNOffset oEndOffset;

  
    if(WeHaveConsole){
        TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
        err =  TXNSetData (RConsoleOutObject, kTXNTextData, buf, strlen(buf), kTXNEndOffset, kTXNEndOffset);
    }
    else{
     fprintf(stderr,"%s", buf);
    }

}


void RSetColors(void)
{
   TXNBackground RBGInfo;   

/* setting FG colors */
   TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
   TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );

/* setting BG colors */
   RBGInfo.bgType = kTXNBackgroundTypeRGB;
   RBGInfo.bg.color = RBGOutColor;        
   TXNSetBackground(RConsoleOutObject, &RBGInfo);

   RBGInfo.bg.color = RBGInColor;                 
   TXNSetBackground(RConsoleInObject, &RBGInfo);
 
 
}
 
OSStatus InitMLTE(void)
{
	OSStatus				status = noErr;
	TXNMacOSPreferredFontDescription	defaults; 
	TXNInitOptions 				options;
        SInt16                            fontID;
        
        GetFNum("\pMonaco",&fontID);
        
	defaults.fontID = fontID;  
	defaults.pointSize = kTXNDefaultFontSize;
  	defaults.encoding = CreateTextEncoding(kTextEncodingMacRoman, kTextEncodingDefaultVariant,
                                                kTextEncodingDefaultFormat);
  	defaults.fontStyle = kTXNDefaultFontStyle;

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
   int i, lg=0, pptlen;
          
   if(!InputFinished)
     Aqua_RWrite(prompt);
   TXNFocus(RConsoleInObject,true);
   TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );

    while(!InputFinished)
     RunApplicationEventLoop();
    
   if(InputFinished){
    TXNGetSelection (RConsoleInObject, &oStartOffset, &oEndOffset);
     err = TXNGetDataEncoded(RConsoleInObject, 0, oEndOffset, &DataHandle, kTXNTextData);
     lg = min(len,oEndOffset);
     HLock( DataHandle );
     for(i=0; i<lg-1; i++){
       buf[i] = (*DataHandle)[i];
       if(buf[i] == '\r') buf[i]= '\n';
     }  
     HUnlock( DataHandle );
     if(DataHandle)
      DisposeHandle( DataHandle );
	
     buf[lg-1] = '\n';
     buf[lg] = '\0';
     InputFinished = false;
     TXNSetData(RConsoleInObject,kTXNTextData,NULL,0,kTXNStartOffset ,kTXNEndOffset );
     Raqua_WriteConsole(buf,strlen(buf));
   }
 
  
   return(1);
}


void Aqua_RWrite(char *buf)
{
    if(WeHaveConsole){
       TXNSetData(RConsoleOutObject, kTXNTextData, buf, strlen(buf), kTXNEndOffset, kTXNEndOffset);
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
  consolecmd("q()\r");
} 
  

/* Changes font size in both Console In and Out 
   default size is 12
*/
void RSetFontSize(int size)
{
    TXNTypeAttributes	typeAttr;
    
    typeAttr.tag = kTXNQDFontSizeAttribute;
    typeAttr.size = kTXNFontSizeAttributeSize;
    typeAttr.data.dataValue = size << 16;

    TXNSetTypeAttributes(RConsoleOutObject, 1, &typeAttr, 0, 100000);
    TXNSetTypeAttributes(RConsoleInObject, 1, &typeAttr, 0, 100000);

}

/* Sets tab space for Console In and Out
   tabsize: number of chars
 */

    
void RSetTab(int tabsize){
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
    
    tabdata.tabValue.value = tabsize*RFontSize;
    tabdata.tabValue.tabType = kTXNRightTab;
         
    TXNSetTXNObjectControls(RConsoleOutObject, false, 1, &tabtag, &tabdata);
    TXNSetTXNObjectControls(RConsoleInObject, false, 1, &tabtag, &tabdata);
}

static pascal OSStatus
RCmdHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
	OSStatus 		err = eventNotHandledErr, result = noErr;
	HICommand		command;
	UInt32			eventKind = GetEventKind( inEvent );
        FSSpec 			tempfss;
        char			buf[300],cmd[2500];

	switch ( GetEventClass( inEvent ) )
	{
         case kEventClassCommand:
            GetEventParameter( inEvent, kEventParamDirectObject, typeHICommand, NULL,
					sizeof( HICommand ), NULL, &command );
            if ( eventKind == kEventCommandProcess ){
             switch(command.commandID){
/* File Menu */
              case kHICommandOpen:
               result = SelectFile(&tempfss,"Select File to Source");
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"source(\"%s\")\r",buf);
               consolecmd(cmd);
              break;
              
              case kRCmdFileShow:
               result = SelectFile(&tempfss,"Select File to Show");
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"file.show(\"%s\")\r",buf);
               consolecmd(cmd);
              break;
              
              case kHICommandNew:
               fprintf(stderr,"\n open a new editable window");
              break;
             
/* Edit Menu */             

              case kHICommandPaste:
               if(TXNIsScrapPastable()){
                 TXNSetSelection(RConsoleInObject,kTXNEndOffset,kTXNEndOffset); 
                 TXNPaste(RConsoleInObject); 
               }
               break;
              
              /*
                 If selection occurs in both RConsole-Out and RConsole-In, only the 
                 text selected in the RConsole-Out is copied to the clipboard.
                 I'm not sure if it should be the contrary.
              */    
              case kHICommandCopy:
               if(!TXNIsSelectionEmpty(RConsoleOutObject)){
                 TXNCopy(RConsoleOutObject); 
               } else 
                  if(!TXNIsSelectionEmpty(RConsoleInObject)){
                 TXNCopy(RConsoleInObject); 
               }               
               break;
          
              case kHICommandAbout:
               RAboutHandler(RAboutWindow);
              break;
              
/* Tools menu */              
              case kRCmdShowWSpace:
               consolecmd("ls()\r");              
              break;          		

              case kRCmdClearWSpace:
               consolecmd("rm(list=ls())\r");              
              break;          		

              case kRCmdBrowseWSpace:
                consolecmd("browseEnv(html=FALSE)\r");
              break;

              case kRCmdLoadWSpace:
                consolecmd("load(\".RData\")\r");
              break;

              case kRCmdSaveWSpace:
                consolecmd("save.image()\r");
              break;

              case kRCmdLoadHistory:
                consolecmd("loadhistory()\r");
              break;

              case kRCmdSaveHistory:
                consolecmd("savehistory()\r");
              break;

              case kRCmdShowHistory:
                consolecmd("history()\r");
              break;

              case kRCmdChangeWorkDir:
               DoSelectDirectory();
              break;

              case kRCmdShowWorkDir:
                consolecmd("getwd()\r");
              break;

              case kRCmdResetWorkDir:
                consolecmd("setwd(\"~/\")\r");
              break;

/* Packages menu */

              case kRCmdInstalledPkgs:
               consolecmd("library()\r");
              break;

              case kRCmdAvailDatsets:
               consolecmd("data()\r");
              break;

              case kRCmdInstallFromCRAN:
               Aqua_RWrite("Install packages from CRAN: not yet implemented");
               consolecmd("\r");
              break;

              case kRCmdInstallFromBioC:
               Aqua_RWrite("Install packages from BioConductor: not yet implemented");
               consolecmd("\r");
              break;

              case kRCmdInstallFromSrc:
               Aqua_RWrite("Install package from source: not yet implemented");
               consolecmd("\r");
              break;

/* Help Menu */
              case kRHelpStart:
                consolecmd("help.start()\r");
              break;
              
              case kRHelpOnTopic:  
               Aqua_RWrite("Help On Topic: not yet implemented");
               consolecmd("\r");
              break;
              
              case kRSearchHelpOn:
               Aqua_RWrite("Search Help On: not yet implemented");
               consolecmd("\r");
              break;
              
              case kRExampleRun:
               Aqua_RWrite("Run An Example: not yet implemented");
               consolecmd("\r");
              break;
                        
              default:
              break;
             }
            }
        }    
 	
	return err;
}

void RescaleInOut(double prop)
{  
  Rect 	WinBounds, InRect, OutRect;

  GetWindowPortBounds(ConsoleWindow, &WinBounds);
  SetRect(&OutRect,0,0,WinBounds.right,(int)( WinBounds.bottom*prop ));
  SetRect(&InRect, 0, (int)( WinBounds.bottom*prop+1 ),WinBounds.right,WinBounds.bottom);           
  TXNSetFrameBounds (RConsoleInObject, InRect.top, InRect.left,  InRect.bottom, InRect.right, InframeID);
  TXNSetFrameBounds (RConsoleOutObject, OutRect.top, OutRect.left, OutRect.bottom, OutRect.right, OutframeID);
  BeginUpdate(ConsoleWindow);
  TXNForceUpdate(RConsoleOutObject);
  TXNForceUpdate(RConsoleInObject);
  TXNDraw(RConsoleOutObject, NULL);
  TXNDraw(RConsoleInObject, NULL);
  EndUpdate(ConsoleWindow); 				 	           
}


static pascal OSStatus
RWinHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
	OSStatus 	err = eventNotHandledErr;
	HICommand	command;
	UInt32		eventKind = GetEventKind( inEvent ), RWinCode, devsize;
        int		devnum;
        NewDevDesc 	*dd;
        WindowRef 	EventWindow;
        EventRef	REvent;
        TXNFrameID	HlpFrameID  = 0;
        UInt32		eventClass;
        TXNObject 	HlpObj = NULL;
              HIPoint where;
            WindowDefPartCode part;
            EventRef hitTest;
    
        eventClass = GetEventClass(inEvent);
        GetEventParameter(inEvent, kEventParamDirectObject, typeWindowRef, NULL, sizeof(EventWindow),
                                NULL, &EventWindow);
       
        switch(eventClass){
         
        case kEventClassWindow:
         
        GetEventParameter (inEvent, kEventParamAttributes, typeUInt32, NULL, sizeof(RWinCode), 
                                NULL, &RWinCode);
         switch(eventKind){
            
            case kEventWindowBoundsChanged:                    
             if( RWinCode != 9){ 
                if( EventWindow == ConsoleWindow){
                    RescaleInOut(0.8);
                    err = noErr;
                }
                
                if( GetWindowProperty(EventWindow, kRAppSignature, 1, sizeof(int), devsize, &devnum) == noErr)
                    if( (dd = ((GEDevDesc*) GetDevice(devnum))->dev) ){
                        dd->size(&(dd->left), &(dd->right), &(dd->bottom), &(dd->top), dd);
                        GEplayDisplayList((GEDevDesc*) GetDevice(devnum));       
                        err = noErr;
                    }
                
                         
             }
            break;
            

            default:
            break;
        }    
      
                 
        default:
        break;
        }
        
	return err;
}

OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
	OSStatus 	err = eventNotHandledErr;
	HICommand	command;
	UInt32		eventKind = GetEventKind( inEvent ), RWinCode, devsize;
        int		devnum;
        char 		cmd[255];
        WindowRef 	EventWindow;
        EventRef	REvent;
        TXNObject	RHlpObj  = NULL;

	
        if( GetEventClass(inEvent) != kEventClassWindow)
         return(err);
         
        GetEventParameter(inEvent, kEventParamDirectObject, typeWindowRef, NULL, sizeof(EventWindow),
                                NULL, &EventWindow);
        switch(eventKind){
            
     
            
                           
            case kEventWindowClose:
             /* Are we closing the R Console window ? */
             if( EventWindow == ConsoleWindow){
                consolecmd("q()\r");
                err = noErr;
              } 
              
             /* Are we closing any quartz device window ? */
            if( GetWindowProperty(EventWindow, kRAppSignature, 1, sizeof(int), devsize, &devnum) == noErr){
                    sprintf(cmd,"dev.off(%d)\r",1+devnum);
                    consolecmd(cmd);
                    err= noErr; 
            }
         
           if( GetWindowProperty(EventWindow, kRAppSignature, 'HLPO', sizeof(TXNObject), NULL, &RHlpObj) == noErr){
                    TXNDeleteObject(RHlpObj);
                    HideWindow(EventWindow);
                    err= noErr; 
            }
           break;
                
            default:
            break;
        }    
 	   
	return noErr;
}

/* consolecmd: is used to write in the input R console
               to send R command via menus. Its argument
               must be a terminated string, possibly with
               '\r'. We don't check for this.
*/               
void consolecmd(char *cmd)
{
    EventRef	REvent;
    UInt32	RKeyCode = 36;

    if(strlen(cmd) < 1)
	return;

   TXNSetData (RConsoleInObject, kTXNTextData, cmd, strlen(cmd), kTXNEndOffset, kTXNEndOffset);
   CreateEvent(NULL, kEventClassKeyboard, kEventRawKeyUp, 0,kEventAttributeNone, &REvent);
   SetEventParameter(REvent, kEventParamKeyCode, typeUInt32, sizeof(RKeyCode), &RKeyCode);
   SendEventToEventTarget (REvent,GetApplicationEventTarget());
}


/* DoSelectDirectory: allows the user to change R current working directory
                    via dialog box
*/
pascal	OSErr	FSpGetFullPath(const FSSpec *spec,
							   short *fullPathLength,
							   Handle *fullPath);

pascal	OSErr	FSMakeFSSpecCompat(short vRefNum,
								   long dirID,
								   ConstStr255Param fileName,
								   FSSpec *spec);

OSErr DoSelectDirectory( void )
{	
	NavReplyRecord		theReply;
	NavDialogOptions	dialogOptions;
	OSErr			theErr = noErr;
	NavEventUPP		eventUPP = nil; 
	SInt16 			pathLen;
        Handle 			pathName=NULL;
        char 			path[300],buf[300];
	OSErr               	anErr = noErr,err;
        
	theErr = NavGetDefaultDialogOptions( &dialogOptions );
		
        CopyCStringToPascal("Choose R Working Directory",dialogOptions.message);

	theErr = NavChooseFolder(NULL,&theReply,&dialogOptions,eventUPP,NULL,nil);

	if ( theReply.validRecord && theErr == noErr)
	{
		FSSpec		finalFSSpec,tempSpec;	
		AEKeyword 	keyWord;
		DescType 	typeCode;
                WDPBRec			wdpb;
		Size 		actualSize = 0;
 
 //               err= FSMakeFSSpec(0, 0, NULL, &finalFSSpec);
 

		if (( theErr = AEGetNthPtr( &(theReply.selection), 1, typeFSS, &keyWord, &typeCode, 
		         &finalFSSpec, sizeof( FSSpec ), &actualSize )) == noErr )		
		{
  			// 'finalFSSpec' is the selected directoryÉ
                        
                        err = FSMakePath(finalFSSpec.vRefNum, finalFSSpec.parID, finalFSSpec.name, buf, 300);

                   //     err = FSMakeFSSpec(finalFSSpec.vRefNum, finalFSSpec.parID, finalFSSpec.name, &tempSpec);
                    //    CopyCStringToPascal(tempSpec.name,path);
                        fprintf(stderr,"\n FSMakePath err =%d, name=%s",err,path);
                        
		      chdir(buf);	
                    fprintf(stderr,"\n newdir=%s",buf);	
		}
		
		theErr = NavDisposeReply( &theReply );
	}
		
	return theErr;
}


int RAqua_R_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager)
{
    int    	i;
    
    if (nfile <=0) return 1;
	
    for (i = 0; i < nfile; i++){
        NewHelpWindow(fileName[i], title[i], WinTitle); 
    }

    return 1;
}


int NewHelpWindow(char *fileName, char *title, char *WinTitle)
{
    Rect	WinBounds;
    OSStatus	err = noErr;
    WindowRef 	HelpWindow =  NULL;
    Str255	Title;
    FSSpec    	fsspec;
    TXNObject	RHelpObject = NULL;
    TXNFrameID	HelpFrameID	= 0;
    SInt16 	refNum = 0;
    TXNFrameOptions	frameOptions;
    Boolean isDirectory;
    char buf[300];
    FInfo             fileInfo;
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
                            
    frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask|kTXNDrawGrowIconMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;

    SetRect(&WinBounds, 400, 400, 400 +400, 400 + 400 ) ;
    
    
     if( (err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute | kWindowStandardDocumentAttributes, &WinBounds, &HelpWindow) != noErr))
     goto fail;
    
    InstallStandardEventHandler( GetWindowEventTarget(HelpWindow));
    CopyCStringToPascal(WinTitle,Title);
    SetWTitle(HelpWindow, Title);


    err = FSPathMakeFSSpec(fileName, &fsspec, &isDirectory);
    if(err != noErr)
     goto fail;
     
    CopyPascalStringToC(fsspec.name,buf);

    err = FSpGetFInfo(&fsspec,&fileInfo);
    if(err != noErr)
     goto fail;
   
    if(fileInfo.fdType == NULL){    
        fileInfo.fdType = kTXNTextFile;
        err = FSpSetFInfo(&fsspec,&fileInfo);
        if(err != noErr)
          goto fail;
    }
    
    err = TXNNewObject(&fsspec, HelpWindow, NULL, frameOptions, kTXNTextEditStyleFrameType,
                            fileInfo.fdType, kTXNSystemDefaultEncoding, &RHelpObject,
                            &HelpFrameID, 0);       
    if(err != noErr)
     goto fail;
                                           
    err = TXNSetTXNObjectControls(RHelpObject, false, 3, RHelpTag, RHelpData);

    
    tabdata.tabValue.value = 10*RFontSize;
    tabdata.tabValue.tabType = kTXNRightTab;
         
    TXNSetTXNObjectControls(RHelpObject, false, 1, &tabtag, &tabdata);
 
    if(err != noErr)
     goto fail;

    err = SetWindowProperty(HelpWindow,kRAppSignature,'HLPO',sizeof(TXNObject), &RHelpObject);
    err = SetWindowProperty(HelpWindow,kRAppSignature,'HLPO',sizeof(TXNFrameID), &HelpFrameID);
    err = InstallWindowEventHandler( HelpWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)HelpWindow, NULL);
                    
    TXNActivate(RHelpObject, HelpFrameID, kScrollBarsAlwaysActive);
    ShowWindow(HelpWindow);
    BeginUpdate(HelpWindow);
    TXNForceUpdate(RHelpObject);
    TXNDraw(RHelpObject, NULL);
    EndUpdate(HelpWindow); 				 	           

    
    return 0;
    
fail:
   
   if( RHelpObject )
    TXNDeleteObject(RHelpObject);

   if( HelpWindow )
    HideWindow(HelpWindow);             
    
    return 1;
}


OSStatus
FSPathMakeFSSpec(
	const UInt8 *path,
	FSSpec *spec,
	Boolean *isDirectory)	/* can be NULL */
{
	OSStatus	result;
	FSRef		ref;
	
	/* check parameters */
	require_action(NULL != spec, BadParameter, result = paramErr);
	
	/* convert the POSIX path to an FSRef */
	result = FSPathMakeRef(path, &ref, isDirectory);
	require_noerr(result, FSPathMakeRef);
	
	/* and then convert the FSRef to an FSSpec */
	result = FSGetCatalogInfo(&ref, kFSCatInfoNone, NULL, NULL, spec, NULL);
	require_noerr(result, FSGetCatalogInfo);
	
FSGetCatalogInfo:
FSPathMakeRef:
BadParameter:

	return ( result );
}

OSStatus
FSMakePath(
	SInt16 volRefNum,
	SInt32 dirID,
	ConstStr255Param name,
	UInt8 *path,
	UInt32 maxPathSize)
{
	OSStatus	result;
	FSRef		ref;
	
	/* check parameters */
	require_action(NULL != path, BadParameter, result = paramErr);
	
	/* convert the inputs to an FSRef */
	result = FSMakeFSRef(volRefNum, dirID, name, &ref);
	require_noerr(result, FSMakeFSRef);
	
	/* and then convert the FSRef to a path */
	result = FSRefMakePath(&ref, path, maxPathSize);
	require_noerr(result, FSRefMakePath);
	
FSRefMakePath:
FSMakeFSRef:
BadParameter:

	return ( result );
}

OSErr
FSMakeFSRef(
	FSVolumeRefNum volRefNum,
	SInt32 dirID,
	ConstStr255Param name,
	FSRef *ref)
{
	OSErr		result;
	FSRefParam	pb;
	
	/* check parameters */
	require_action(NULL != ref, BadParameter, result = paramErr);
	
	pb.ioVRefNum = volRefNum;
	pb.ioDirID = dirID;
	pb.ioNamePtr = (StringPtr)name;
	pb.newRef = ref;
	result = PBMakeFSRefSync(&pb);
	require_noerr(result, PBMakeFSRefSync);
	
PBMakeFSRefSync:
BadParameter:

	return ( result );
}


OSStatus SelectFile(FSSpec *outFSSpec,  char *Title)
{
    NavDialogOptions    dialogOptions;
    NavEventUPP         eventProc = nil; 
    NavObjectFilterUPP  filterProc = nil;
    OSErr               anErr = noErr;
    
    
    /*  Specify default options for dialog box */
    anErr = NavGetDefaultDialogOptions(&dialogOptions);

    CopyCStringToPascal(Title,dialogOptions.message);

         
    if (anErr == noErr)
    {
        /*  Adjust the options to fit our needs
            Set default location option
         */   
        dialogOptions.dialogOptionFlags |= kNavSelectDefaultLocation;
        dialogOptions.dialogOptionFlags |= kNavAllowInvisibleFiles;
        dialogOptions.dialogOptionFlags |= kNavAllFilesInPopup;
                        
        if (anErr == noErr)
        {
            /* Get 'open' resource. A nil handle being returned is OK, */
            /* this simply means no automatic file filtering. */
            NavReplyRecord reply;
            NavTypeListHandle deftypeList = nil; /* we apply no filter for the moment */
            
            /* Call NavGetFile() with specified options and
               declare our app-defined functions and type list
             */
             anErr = NavGetFile (nil, &reply, &dialogOptions,
                                nil, nil, nil,
                                deftypeList, nil);     
               
            if (anErr == noErr && reply.validRecord)
            {
                /*  Deal with multiple file selection */
                long    count;
                
                anErr = AECountItems(&(reply.selection), &count);
                           
                count = 1L; /* we only select one file */
                // Set up index for file list
                if (anErr == noErr)
                {
                    long index;
                    
                    for (index = 1; index <= count; index++)
                    {
                        AEKeyword   theKeyword;
                        DescType    actualType;
                        Size        actualSize;
                        
                        /* Get a pointer to selected file */
                        anErr = AEGetNthPtr(&(reply.selection), index,
                                            typeFSS, &theKeyword,
                                            &actualType,outFSSpec,
                                            sizeof(FSSpec),
                                            &actualSize);
                             
                        
                    }
                }
                /*  Dispose of NavReplyRecord, resources, descriptors */
                NavDisposeReply(&reply);
            }
           
        }
    }

cleanup:  
      return anErr;
}


#endif /* HAVE_AQUA */

#endif /* __AQUA_CONSOLE__ */
