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

#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#  define min(a,b) ((a) < (b) ? (a) : (b))
#endif


OSStatus 	InitMLTE(void);
TXNObject	RConsoleOutObject = NULL;
TXNObject	RConsoleInObject = NULL;
bool 		WeHaveConsole = false;
bool 		InputFinished = false;
TXNFrameID		OutframeID	= 0;
TXNFrameID		InframeID	= 0;

WindowRef	RAboutWindow=NULL;
WindowRef	RPrefsWindow=NULL;

pascal void RAboutHandler(WindowRef window);
pascal void RPrefsHandler(WindowRef window);

#define kRAppSignature '0FFF'
//int 	RFontSize = 12;

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
                
                
enum {kTabMasterSig = 'PRTT', kTabMasterID = 1000,kTabPaneSig= 'PRTB', kPrefControlsSig = 'PREF'};
enum {kDummyValue = 0,kMaxNumTabs= 3};
enum{kApplyPrefsButton = 5000,kCancelPrefsButton = 5001, kDefaultPrefsButton = 5002, kSavePrefsButton = 5003};

#define kConsoleFontButton  1002
#define kOutputColorButton  1009
#define kInputColorButton   1010
#define kOutputBackButton   1011
#define kInputBackButton    1012
#define	kWorkingDirButton   2001
#define	kDeviceFontButton   3001


static void SetInitialTabState(WindowRef theWindow);
                
static pascal OSStatus
RCmdHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
static pascal OSStatus
RWinHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
void RescaleInOut(double prop);
void CallFontPanel(void);

OSErr DoSelectDirectory( void );
OSStatus SelectFile(FSSpec *outFSSpec,  char *Title);
OSStatus FSPathMakeFSSpec(const UInt8 *path, FSSpec *spec, Boolean *isDirectory);
OSStatus FSMakePath(SInt16 volRefNum, SInt32 dirID, ConstStr255Param name, UInt8 *path,
	UInt32 maxPathSize);
OSErr FSMakeFSRef(FSVolumeRefNum volRefNum, SInt32 dirID, ConstStr255Param name, FSRef *ref);
        
ControlRef GrabCRef(WindowRef theWindow,OSType theSig, SInt32 theNum);

int Raqua_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager);
int Raqua_ChooseFile(int new, char *buf, int len);


void 	Raqua_StartConsole(void);
void 	Raqua_WriteConsole(char *buf, int len);
int 	Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);
int NewHelpWindow(char *fileName, char *title, char *WinTitle);
		     
                     
OSStatus MySetFontSelection (WindowRef thisWindow);
OSStatus MyGetFontSelection (EventRef event);
                     
void consolecmd(char *cmd);
void RSetColors(void);
                   
#define kRVersionInfoID 132

static const EventTypeSpec	REvents[] =
{
	{ kEventClassTextInput, kEventTextInputUnicodeForKeyEvent }
};

    EventTypeSpec	tabControlEvents[] ={
    { kEventClassControl, kEventControlHit },
    { kEventClassCommand, kEventCommandProcess }};

static const EventTypeSpec	aboutSpec =
	{ kEventClassWindow, kEventWindowClose };


static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon); 
static pascal OSStatus KeybHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
static pascal OSStatus RAboutWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);
static pascal OSStatus RPrefsWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);

OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );

static pascal OSStatus PrefsTabEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
static  OSStatus GenContEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );

WindowRef			ConsoleWindow=NULL;

static const EventTypeSpec KeybEvents[] = {{ kEventClassKeyboard, kEventRawKeyDown }};


static const EventTypeSpec	RCmdEvents[] =
{
	{ kEventClassCommand, kEventCommandProcess },
	{ kEventClassCommand, kEventCommandUpdateStatus }
};

static const EventTypeSpec	RGlobalWinEvents[] =
{
        { kEventClassWindow, kEventWindowBoundsChanged },
        { kEventClassWindow, kEventWindowFocusAcquired },
        { kEventClassWindow, kEventWindowFocusRelinquish },
        { kEventClassFont, kEventFontPanelClosed},
        { kEventClassFont, kEventFontSelection} 
};

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};

 EventTypeSpec	okcanxControlEvents[] ={{ kEventClassControl, kEventControlHit }};

void HistBack(void);
void HistFwd(void);
void maintain_cmd_History(char *buf);
void mac_savehistory(char *file);
void mac_loadhistory(char *file);
SEXP Raqua_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP Raqua_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);

MenuRef HelpMenu = NULL; /* Will be the Reference to Apple's Help Menu */
static 	short 	RHelpMenuItem=-1;
static 	short 	RTopicHelpItem=-1;
static	short 	RunExampleItem=-1;
static	short	SearchHelpItem=-1;
static  short  	PreferencesItem=-1;

#define IntToFixed(a) ((Fixed)(a) << 16)

void GetRPrefs(void);
void SaveRPrefs(void);
int PrefTabSize, PrefFontSize;
RGBColor PrefRFGOutColor;
RGBColor PrefRBGOutColor;
RGBColor PrefRFGInColor;
RGBColor PrefRBGInColor;

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
   
    err = CreateWindowFromNib(nibRef,CFSTR("PrefsWindow"),&RPrefsWindow);
   if(err != noErr)
     goto noconsole;

    if(nibRef)
     DisposeNibReference(nibRef);
  
    ShowWindow(ConsoleWindow);

    GetRPrefs();
    SaveRPrefs();
    
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
	// fprintf(stderr,"\n err(1) =%d",err);	
        frameOptions = kTXNShowWindowMask | kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNDrawGrowIconMask;
		
        err = TXNNewObject(NULL, ConsoleWindow, &InFrame, frameOptions, kTXNTextEditStyleFrameType,
                            kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleInObject,
                            &InframeID, 0);
//	fprintf(stderr,"\n err(2) =%d",err);	

        if (err == noErr){		
            if ( (RConsoleOutObject != NULL) && (RConsoleInObject != NULL) ){
                    /* sets the state of the scrollbars so they are drawn correctly */
                err = TXNActivate(RConsoleOutObject, OutframeID, kScrollBarsAlwaysActive);
                err = TXNActivate(RConsoleInObject, InframeID, kScrollBarsAlwaysActive);
                if (err != noErr)
                    goto noconsole;
		        
//	fprintf(stderr,"\n err(3) =%d",err);	
			
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
//	fprintf(stderr,"\n err(5) =%d",err);	

        TXNFocus(RConsoleOutObject,true);
        InstallWindowEventHandler(RAboutWindow, NewEventHandlerUPP(RAboutWinHandler), 1, &aboutSpec, 
                                (void *)RAboutWindow, NULL);

        InstallWindowEventHandler(RPrefsWindow, NewEventHandlerUPP(RPrefsWinHandler), 1, &aboutSpec, 
                                (void *)RPrefsWindow, NULL);

    InstallControlEventHandler( GrabCRef(RPrefsWindow,kTabMasterSig,kTabMasterID),  PrefsTabEventHandlerProc , GetEventTypeCount(tabControlEvents), tabControlEvents, RPrefsWindow, NULL );

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kApplyPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );
        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kCancelPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kDefaultPrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kSavePrefsButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
     
        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kConsoleFontButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kOutputColorButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kInputColorButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kOutputBackButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kInputBackButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kWorkingDirButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        

        InstallControlEventHandler( GrabCRef(RPrefsWindow,kPrefControlsSig,kDeviceFontButton),  GenContEventHandlerProc , GetEventTypeCount(okcanxControlEvents), okcanxControlEvents, RPrefsWindow, NULL );        
     

        
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
        
       RSetTab(PrefTabSize);
       RSetFontSize(PrefFontSize);

       EnableMenuCommand(NULL, kHICommandPreferences);
noconsole:
    if(bundleURL)
     CFRelease( bundleURL );
    if(RBundle)
     CFRelease( RBundle ); 
	return;
}

/* BEWARE: before quitting R via ExitToShell() call TXNTerminateTextension() */


void Aqua_RWrite(char *buf);
void Aqua_RnWrite(char *buf, int len);

RGBColor RFGOutColor = { 0x0000, 0x0000, 0xffff};
RGBColor RFGInColor = { 0xffff, 0x0000, 0x0000};
RGBColor RBGOutColor = {0xeeee, 0xeeee, 0xeeee};
RGBColor RBGInColor = { 0xeeff, 0xeeff, 0xeeff};

TXNTypeAttributes RInAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &PrefRFGInColor}};
TXNTypeAttributes ROutAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &PrefRFGOutColor}};

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
   RBGInfo.bg.color = PrefRBGOutColor;        
   TXNSetBackground(RConsoleOutObject, &RBGInfo);

   RBGInfo.bg.color = PrefRBGInColor;                 
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
    //    fprintf(stderr,"\n fontsize=%d, %d",PrefFontSize, (Fixed)PrefFontSize);
  	defaults.encoding = CreateTextEncoding(kTextEncodingMacRoman, kTextEncodingDefaultVariant,
                                                kTextEncodingDefaultFormat);
  	defaults.fontStyle = kTXNDefaultFontStyle;

	options = kTXNWantMoviesMask | kTXNWantSoundMask | kTXNWantGraphicsMask;

	status = TXNInitTextension(&defaults, 1, options);

	return(status);
}




int Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory)
{
   OSStatus err = noErr;
   Handle DataHandle;
   TXNOffset oStartOffset; 
   TXNOffset oEndOffset;
   
   int i, lg=0, pptlen, txtlen;
          
   if(!InputFinished)
     Aqua_RWrite(prompt);
   TXNFocus(RConsoleInObject,true);
   TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );

    while(!InputFinished)
     RunApplicationEventLoop();
    
   if(InputFinished){
     txtlen = TXNDataSize(RConsoleInObject)/2;
     err = TXNGetDataEncoded(RConsoleInObject, 0, txtlen, &DataHandle, kTXNTextData);
     lg = min(len,txtlen+1); /* has to txtlen+1 as the string is no terminated */
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
     if (strlen(buf) > 1)
	maintain_cmd_History(buf);

   }
 
  
   return(1);
}


void Aqua_RWrite(char *buf)
{
    if(WeHaveConsole){
       TXNSetData(RConsoleOutObject, kTXNTextData, buf, strlen(buf), kTXNEndOffset, kTXNEndOffset);
    }
}

void Aqua_RnWrite(char *buf, int len)
{
    if(WeHaveConsole){
       TXNSetData(RConsoleInObject, kTXNTextData, buf, min(len,strlen(buf)), 0, TXNDataSize(RConsoleInObject));
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
 char c;
 
 /* make sure that we're processing a keyboard event */
 if ( GetEventClass( REvent ) == kEventClassKeyboard )
 {
  switch ( GetEventKind( REvent ) )
  {
   
   case kEventRawKeyDown:
    err = GetEventParameter (REvent, kEventParamKeyCode, typeUInt32, NULL, sizeof(RKeyCode), NULL, &RKeyCode);
    switch(RKeyCode){
     
     case 36:
      InputFinished = true;
      QuitApplicationEventLoop();
      err = noErr;
     break;
     
     case 126: /* key up - history back */
      HistBack();
     break;
     
     case 125: /* key down - history forward */
      HistFwd();
     break;
      
     default:
      err = eventNotHandledErr;
     break;
    }
    break;
    
   break;
   
   
   default:
   break;
   
  }
  
 }
 return err;
}
 

pascal void RPrefsHandler(WindowRef window)
{
    CFStringRef	text;
    CFStringRef	appBundle;
    ControlID	versionInfoID = {kRAppSignature, kRVersionInfoID};
    ControlRef	versionControl;
    ControlFontStyleRec	controlStyle;
    
    SetInitialTabState(window);
     
    ShowWindow(window);
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
 
pascal OSStatus RPrefsWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData)
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
  consolecmd("q()");
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
    
    tabdata.tabValue.value = tabsize*PrefFontSize;//RFontSize;
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
/* Apple Menu */
              case kHICommandPreferences:
                   RPrefsHandler(RPrefsWindow);
              break;
              
/* File Menu */
              case kHICommandOpen:
               result = SelectFile(&tempfss,"Select File to Source");
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"source(\"%s\")",buf);
               consolecmd(cmd);
              break;
              
              case kRCmdFileShow:
               result = SelectFile(&tempfss,"Select File to Show");
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"file.show(\"%s\")",buf);
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
               consolecmd("ls()");              
              break;          		

              case kRCmdClearWSpace:
               consolecmd("rm(list=ls())");              
              break;          		

              case kRCmdBrowseWSpace:
                consolecmd("browseEnv(html=FALSE)");
              break;

              case kRCmdLoadWSpace:
                consolecmd("load(\".RData\")");
              break;

              case kRCmdSaveWSpace:
                consolecmd("save.image()");
              break;

              case kRCmdLoadHistory:
                consolecmd("loadhistory()");
              break;

              case kRCmdSaveHistory:
                consolecmd("savehistory()");
              break;

              case kRCmdShowHistory:
                consolecmd("history()");
              break;

              case kRCmdChangeWorkDir:
               DoSelectDirectory();
              break;

              case kRCmdShowWorkDir:
                consolecmd("getwd()");
              break;

              case kRCmdResetWorkDir:
                consolecmd("setwd(\"~/\")");
              break;

/* Packages menu */

              case kRCmdInstalledPkgs:
               consolecmd("library()");
              break;

              case kRCmdAvailDatsets:
               consolecmd("data()");
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
                consolecmd("help.start()");
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
         
         case kEventClassFont:         //6
         {
             switch (eventKind)
             {
                    case kEventFontPanelClosed:                //7
                        fprintf(stderr,"\n font win closed");
                    break;
         
                    case kEventFontSelection:               //8
                        err = MyGetFontSelection (inEvent);
                    break;
                    
                    default:
                    break;
             }
         } 
         break;
 
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
            
            case kEventWindowFocusRelinquish:
             SetFontInfoForSelection(kFontSelectionATSUIType,
                    0, NULL, NULL);
            break;
            
            case kEventWindowFocusAcquired:
              err = SetFontInfoForSelection(kFontSelectionATSUIType, 0, NULL,
                 GetWindowEventTarget(EventWindow));
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
                consolecmd("q()");
                err = noErr;
              } 
              
             /* Are we closing any quartz device window ? */
            if( GetWindowProperty(EventWindow, kRAppSignature, 1, sizeof(int), devsize, &devnum) == noErr){
                    sprintf(cmd,"dev.off(%d)",1+devnum);
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
               to send R command via menus.
*/               
void consolecmd(char *cmd)
{
    EventRef	REvent;
    UInt32	RKeyCode = 36;

    if(strlen(cmd) < 1)
	return;

   TXNSetData (RConsoleInObject, kTXNTextData, cmd, strlen(cmd), 0, TXNDataSize(RConsoleInObject));
   CreateEvent(NULL, kEventClassKeyboard, kEventRawKeyDown, 0,kEventAttributeNone, &REvent);
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

		if (( theErr = AEGetNthPtr( &(theReply.selection), 1, typeFSS, &keyWord, &typeCode, 
		         &finalFSSpec, sizeof( FSSpec ), &actualSize )) == noErr )		
		{
                        err = FSMakePath(finalFSSpec.vRefNum, finalFSSpec.parID, finalFSSpec.name, buf, 300);
     		        chdir(buf);	
    		}
		
		theErr = NavDisposeReply( &theReply );
	}
		
	return theErr;
}


int Raqua_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager)
{
    int    	i;
    
    if (nfile <=0) return 1;
	
    for (i = 0; i < nfile; i++){
        NewHelpWindow(fileName[i], title[i], WinTitle); 
    }

    return 1;
}

int Raqua_ChooseFile(int new, char *buf, int len)
{
  char 		fname[301];
  OSStatus 	err;
  FSSpec	tempfss;
 

  *buf = '\0';
   
  if( SelectFile(&tempfss,"Choose file name") == noErr){
     err = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, fname, 300);  
     strncpy(buf, fname, len);
     buf[len - 1] = '\0';
  }

  return strlen(buf);
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

    
    tabdata.tabValue.value = 10*PrefFontSize;//RFontSize;
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

#define maxhist 500
char                                      *Cmd_Hist[maxhist];
int                                       g_cur_Cmd, g_start_Cmd, g_end_Cmd;
Boolean                                   g_Stop = false;
Boolean                                   g_down = true;
Boolean                                   g_not_first = false;

/* do_Down_Array
This procedure used to maintain the reponse when you click the down array key. (about display
previous command in console window)                                            */
void HistBack(void)
{
    SInt32 textLength;
    char mybuf[40];

    if (g_start_Cmd != g_end_Cmd) {
	if (!g_down){
	    g_cur_Cmd--;
	}
	g_not_first = true;
	g_down = true;
	if (g_start_Cmd == 0){
	    if (g_cur_Cmd < g_start_Cmd){
		SysBeep(10);
	    }else{
		textLength = strlen(Cmd_Hist[g_cur_Cmd]) - 1;
		Aqua_RnWrite(Cmd_Hist[g_cur_Cmd] , textLength);
		g_cur_Cmd--;
	    }
	}else{
	    if (g_cur_Cmd == g_end_Cmd){
		SysBeep(10);
	    }else{
		if(g_cur_Cmd == -1) g_cur_Cmd = maxhist - 1;
		textLength = strlen(Cmd_Hist[g_cur_Cmd]) - 1;
		Aqua_RnWrite(Cmd_Hist[g_cur_Cmd] , textLength);
		g_cur_Cmd--;
		if(g_cur_Cmd == -1) g_cur_Cmd = maxhist - 1;
	    }
	}
    }
}

void HistFwd(void)
{
    SInt32 textLength;

    if (g_start_Cmd != g_end_Cmd) {
	if ((g_down) && (g_not_first)){
	    g_cur_Cmd++;
	    g_down = false;
	}
	if (g_start_Cmd == 0){
	    if (g_cur_Cmd == (g_end_Cmd-1))
		SysBeep(10);
	    else{
		g_cur_Cmd ++;
		textLength = strlen(Cmd_Hist[g_cur_Cmd]) - 1;
		Aqua_RnWrite(Cmd_Hist[g_cur_Cmd] , textLength);
	    }
	}else{
	    if ((g_cur_Cmd == (maxhist -1)) && (g_end_Cmd ==0)){
		SysBeep(10);
	    }else
		if ((g_cur_Cmd == g_end_Cmd) || (g_cur_Cmd == (g_end_Cmd -1))){
		    SysBeep(10);
		}else{
		    g_cur_Cmd ++;
		    if (g_cur_Cmd == maxhist) g_cur_Cmd = 0;
		    textLength = strlen(Cmd_Hist[g_cur_Cmd]) - 1;
		    Aqua_RnWrite(Cmd_Hist[g_cur_Cmd] , textLength);
		}
	}
    }
}


void maintain_cmd_History(char *buf)
{
    char *temp;
    int numberOfChar;

    g_Stop = false;
    numberOfChar = strlen(buf);
    temp = malloc((numberOfChar + 1) * sizeof(char));
    strcpy(temp, (const char *)buf);
    Cmd_Hist[g_end_Cmd] = temp;
    g_not_first = false;
    g_cur_Cmd = g_end_Cmd;
    g_end_Cmd++;
    if (g_end_Cmd <= g_start_Cmd){
	g_start_Cmd++;
    }
    if (g_start_Cmd == maxhist){
	g_start_Cmd = 0;
    }
    if (g_end_Cmd == maxhist){
	g_end_Cmd = 0;
	g_start_Cmd = 1;
    }
 
}



SEXP Raqua_savehistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, "invalid file argument");
    mac_savehistory(CHAR(STRING_ELT(sfile, 0)));
    return R_NilValue;
}

SEXP Raqua_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, "invalid file argument");
    mac_loadhistory(CHAR(STRING_ELT(sfile, 0)));
    return R_NilValue;
}

void mac_savehistory(char *file)
{
    FILE *fp;
    int i;
    char hist_buff[1000];

    if (!file || !g_end_Cmd) return;

    fp = R_fopen(file, "w");
    if (!fp) {
    char msg[256];
	sprintf(msg, "Unable to open history file \"%s\" for writing", file);
	warning(msg);
 	return;
    }

    if (g_start_Cmd < g_end_Cmd)
	for(i = g_start_Cmd ; i < g_end_Cmd ; i++){
	    fprintf(fp, "%s\n", Cmd_Hist[i]);
	}
    else
	for(i = 0; i < maxhist; i++)
	    fprintf(fp, "%s\n", Cmd_Hist[i]);
    fclose(fp);
}

/**********************************************
 mac_loadhistory: load history command from a
 specified file. Adapted from gl_loadhistory
 for Windows. It can read history files of
 Windowds porting.
**********************************************/
void mac_loadhistory(char *file)
{
    FILE *fp;
    int i,buflen,j;
    char buf[1002];

    if (!file || *file==NULL) return;
    fp = R_fopen(file, "r");
    if (!fp) {
     REprintf("\nUnable to open history file \"%s\" for reading\n", file);
 	return;
    }

    for(i = 0;; i++) {
	if(!fgets(buf, 1000, fp))
	    break;
	if( (buflen = strlen(buf)) > 1) {
	    if(buf[buflen-1]==0x0A) {
		if(buf[buflen-2]==0x0D)
		    buf[buflen-1]='\0';
		else {
		    buf[buflen]='\0';
		    buf[buflen-1]=0x0D;
		}
	    }
	    maintain_cmd_History(buf);
	}
    }
    fclose(fp);
}


/*
RGBColor RFGOutColor = { 0x0000, 0x0000, 0xffff};
RGBColor RFGInColor = { 0xffff, 0x0000, 0x0000};
RGBColor RBGOutColor = {0xeeee, 0xeeee, 0xeeee};
RGBColor RBGInColor = { 0xeeff, 0xeeff, 0xeeff};

*/

void GetRPrefs(void)
{
    
    CFStringRef appName = CFSTR("RAqua");
    CFStringRef tabsizeKey = CFSTR("Tab Size");
    CFStringRef fontsizeKey = CFSTR("Font Size");
    CFStringRef outfgKey = CFSTR("OutFg Color");
    CFStringRef outbgKey = CFSTR("OutBg Color");
    CFStringRef infgKey = CFSTR("InFg Color");
    CFStringRef inbgKey = CFSTR("InBg Color");
    CFNumberRef value;
    int tabsize, fontsize;
    CFDataRef	color;
    RGBColor    fgout,bgout,fgin,bgin;
    

    // First retrieve the previous value...

    // CFPreferencesCopyAppValue() and CFPreferencesSetAppValue() are the most straightforward way
    // for an app to read/write preferences that are per user and per app; they will apply on all 
    // machines (on which this user can log in, of course --- for users who are local to a machine,
    // the preferences will end up being restricted to that host). These functions also do a search 
    // through the various cases; if a preference has been set in a less-specific domain (for 
    // instance, "all apps"), its value will be retrieved with this call. This allows globally 
    // setting some preference values (which makes more sense for some preferences than others).

    // Note that you can read/write any "property list" type in preferences; these are
    // CFArray, CFDictionary, CFNumber, CFBoolean, CFData, and CFString.
    // This example just shows CFNumber.

/* Tab Size */
    value = CFPreferencesCopyAppValue(tabsizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &tabsize)) tabsize = 10;
	CFRelease(value);
    } else 
	tabsize = 10; /* set default value */

    PrefTabSize = tabsize;
    if(value)
     CFRelease(value);

/* Font Size */
    value = CFPreferencesCopyAppValue(fontsizeKey, appName);   
    if (value) {
	if (!CFNumberGetValue(value, kCFNumberIntType, &fontsize)) fontsize = 12;
	CFRelease(value);
    } else 
	fontsize = 12; /* set default value */

    PrefFontSize = fontsize;

/*  Console Out Foreground color */
    color = CFPreferencesCopyAppValue(outfgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), &fgout); 
    } else {
     fgout.red = 0x0000;
     fgout.green = 0x0000;
     fgout.blue = 0xffff;
    }

    PrefRFGOutColor.red = fgout.red;
    PrefRFGOutColor.green =  fgout.green;
    PrefRFGOutColor.blue = fgout.blue;

    if(color)
     CFRelease(color);

/*  Console Out Background color */
    color = CFPreferencesCopyAppValue(outbgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), &bgout); 
    } else {
     bgout.red = 0xeeee;
     bgout.green = 0xeeee;
     bgout.blue = 0xeeee;
    }

    PrefRBGOutColor.red = bgout.red;
    PrefRBGOutColor.green =  bgout.green;
    PrefRBGOutColor.blue = bgout.blue;

    if(color)
     CFRelease(color);
   
/*  Console In Foreground color */
    color = CFPreferencesCopyAppValue(infgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), &fgin); 
    } else {
     fgin.red = 0xffff;
     fgin.green = 0x0000;
     fgin.blue = 0x0000;
    }

    PrefRFGInColor.red = fgin.red;
    PrefRFGInColor.green =  fgin.green;
    PrefRFGInColor.blue = fgin.blue;

    if(color)
     CFRelease(color);

/*  Console In Background color */
    color = CFPreferencesCopyAppValue(inbgKey, appName);   
    if (color) {
      CFDataGetBytes (color, CFRangeMake(0,CFDataGetLength(color)), &bgin); 
    } else {
     bgin.red = 0xeeff;
     bgin.green = 0xeeff;
     bgin.blue = 0xeeff;
    }

    PrefRBGInColor.red = bgin.red;
    PrefRBGInColor.green =  bgin.green;
    PrefRBGInColor.blue = bgin.blue;

    if(color)
     CFRelease(color);

}


void SaveRPrefs(void)
{
    
    CFStringRef appName = CFSTR("RAqua");
    CFStringRef tabsizeKey = CFSTR("Tab Size");
    CFStringRef fontsizeKey = CFSTR("Font Size");
    CFStringRef outfgKey = CFSTR("OutFg Color");
    CFStringRef outbgKey = CFSTR("OutBg Color");
    CFStringRef infgKey = CFSTR("InFg Color");
    CFStringRef inbgKey = CFSTR("InBg Color");
    CFNumberRef value;
    int tabsize, fontsize;
    CFDataRef	color;
    RGBColor    fgout,bgout,fgin,bgin;

    tabsize = PrefTabSize;
    fontsize = PrefFontSize;

    fgout.red = PrefRFGOutColor.red;
    fgout.green = PrefRFGOutColor.green;
    fgout.blue = PrefRFGOutColor.blue;

    bgout.red = PrefRBGOutColor.red;
    bgout.green = PrefRBGOutColor.green;
    bgout.blue = PrefRBGOutColor.blue;
   
    fgin.red = PrefRFGInColor.red;
    fgin.green = PrefRFGInColor.green;
    fgin.blue = PrefRFGInColor.blue;
   
    bgin.red = PrefRBGInColor.red;
    bgin.green = PrefRBGInColor.green;
    bgin.blue = PrefRBGInColor.blue;

       
/* Tab Size */
    value = CFNumberCreate(NULL, kCFNumberIntType, &tabsize); 
    CFPreferencesSetAppValue(tabsizeKey, value, appName);
    CFRelease(value);


/* Font Size */
    value = CFNumberCreate(NULL, kCFNumberIntType, &fontsize); 
    CFPreferencesSetAppValue(fontsizeKey, value, appName);
    CFRelease(value);

    color = CFDataCreate (NULL, &fgout, sizeof(fgout));
    if(color){
     CFPreferencesSetAppValue(outfgKey, color, appName);
     CFRelease(color);
    }
    
    color = CFDataCreate (NULL, &bgout, sizeof(bgout));
    if(color){
     CFPreferencesSetAppValue(outbgKey, color, appName);
     CFRelease(color);
    }

    color = CFDataCreate (NULL, &fgin, sizeof(fgin));
    if(color){
     CFPreferencesSetAppValue(infgKey, color, appName);
     CFRelease(color);
    }

    color = CFDataCreate (NULL, &bgin, sizeof(bgin));
    if(color){
     CFPreferencesSetAppValue(inbgKey, color, appName);
     CFRelease(color);
    }

    // Without an explicit synchronize, the saved values actually do not get written out.
    // If you are writing multiple preferences, you might want to sync only after the last one.
    // A preference panel might want to synchronize when the user hits "OK".
    // In some cases you might not want to sync at all until the app quits.
    // The AppKit automatically synchronizes on app termination, so Cocoa apps don't need to do this.

    (void)CFPreferencesAppSynchronize(appName);
}

// Handler for the prefs tabs
// Switches between the 3 panes we have in this sample
static pascal OSStatus PrefsTabEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
    static UInt16 lastPaneSelected = 1; // static, to keep track of it long term (in a more complex application
                                        // you might store this in a data structure in the window refCon)                                            
    WindowRef theWindow = (WindowRef)inUserData;  // get the windowRef, passed around as userData    
    short controlValue = 0;
    int qq;
    //  Get the new value of the tab control now that the user has picked it    
    controlValue = GetControlValue( GrabCRef(theWindow,kTabMasterSig,kTabMasterID) );
    // same as last ?
    if ( controlValue != lastPaneSelected )
    {
        // different from last time.
        // Hide the current pane and make the user selected pane the active one
        // our 3 tab pane IDs.  Put a dummy in so we can index without subtracting 1 (this array is zero based, 
        // control values are 1 based).
        int tabList[] = {kDummyValue, kTabMasterID,kTabMasterID+1,kTabMasterID+2};
                                                                                    
        // hide the current one, and set the new one
        SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[lastPaneSelected]), false, true );
        SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[controlValue]), true, true );    

        // make sure the new configuration is drawn correctly by redrawing the Tab control itself        
        Draw1Control( GrabCRef(theWindow,kTabMasterSig,kTabMasterID) );  
        // and update our tracking
        lastPaneSelected= controlValue;    
    }
    
    return( eventNotHandledErr );
}

static void SetInitialTabState(WindowRef theWindow)
{
    int tabList[] = {kTabMasterID,kTabMasterID+1,kTabMasterID+2}; 
    short qq=0;
    // If we just run without setting the initial state, then the tab control
    // will have both (or all) sets of controls overlapping each other.
    // So we'll fix that by making one pane active right off the bat.
    
    // First pass, turn every pane invisible
    for(qq=0;qq<kMaxNumTabs;qq++)
    SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[qq]), false, true );  
    
    // Set the tab control itself to have a value of 1, the first pane of the tab set
    SetControlValue(GrabCRef(theWindow,kTabMasterSig,kTabMasterID),1 );

    // This is the important bit, of course.  We're setting the currently selected pane
    // to be visible, which makes the associated controls in the pane visible.
    SetControlVisibility( GrabCRef(  theWindow, kTabPaneSig,  tabList[0]), true, true );

}

ControlRef GrabCRef(WindowRef theWindow,OSType theSig, SInt32 theNum)
{   ControlID contID;
    ControlRef theRef = NULL;
    contID.signature= theSig;
    contID.id=theNum;
    GetControlByID( theWindow, &contID, &theRef );
    return(theRef);
}

static  OSStatus GenContEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
    ControlRef theCont = NULL;
    ControlID theID;
    WindowRef theWindow = (WindowRef)inUserData;
    
    // Find out which button was clicked, and what it's ID is
    GetEventParameter (inEvent, kEventParamDirectObject, typeControlRef,NULL, sizeof(ControlRef), NULL, &theCont);
    GetControlID(theCont,&theID); 
    // swtich off the ID
 
    switch(theID.id){
        case kApplyPrefsButton:  
                                        // harvest the controls and close
            HideWindow(theWindow);         break;
        
        case kCancelPrefsButton: 
            HideWindow(theWindow);  // window goes away with no changes
            // Because the only point of this sample is to run the prefs panel, I'll quit 
        break;
      
        case kSavePrefsButton: 
         fprintf(stderr,"\n kSavePrefsButton");
        break;

        case kDefaultPrefsButton: 
         fprintf(stderr,"\n kDefaultPrefsButton");
        break;
  
        case kConsoleFontButton:
         CallFontPanel();
        break;

        case kOutputColorButton:
         fprintf(stderr,"\n kOutputColorButton");
        break;
        
        case kInputColorButton:
         fprintf(stderr,"\n kInputColorButton");
        break;
        
        case kOutputBackButton:
         fprintf(stderr,"\n kOutputBackButton");
        break;
        
        case kInputBackButton:
         fprintf(stderr,"\n kInputBackButton");
        break;

        case kWorkingDirButton:
         fprintf(stderr,"\n kWorkingDirButton");
        break;
        
        case kDeviceFontButton:
         fprintf(stderr,"\nkDeviceFontButton");
        break;
        
        default:
        break;
    }

   return( eventNotHandledErr );
}



void CallFontPanel(void)
{

   fprintf(stderr,"\n console font=%d", FPShowHideFontPanel());
   
}

OSStatus MySetFontSelection (WindowRef thisWindow)
{
    OSStatus            status = noErr;
    ATSUStyle           myStyle;              //1
    ATSUAttributeTag    myTags[2];
    ByteCount           mySizes[2];
    ATSUAttributeValuePtr   myValues[2];
    ATSUFontID          theFontID;
    Fixed               theFontSize;
    HIObjectRef     myHIObjectTarget;             //2

    status = ATSUCreateStyle (&myStyle);             //3
    verify_noerr (ATSUFindFontFromName ("Times Roman", 
                            strlen ("Times Roman"), 
                            kFontFullName, kFontNoPlatform, 
                            kFontNoScript, kFontNoLanguage, 
                            &theFontID) );                //4

    myTags[0] = kATSUFontTag;                //5
    mySizes[0] = sizeof (theFontID);
    myValues[0] = &theFontID;

    theFontSize = Long2Fix (36);                    //6
    myTags[1] = kATSUSizeTag;
    mySizes[1] = sizeof(theFontSize);
    myValues[1] = &theFontSize;

    verify_noerr (ATSUSetAttributes (myStyle, 2, 
                            myTags, mySizes, myValues) );          //7
    myHIObjectTarget = (HIObjectRef) GetWindowEventTarget (thisWindow);              //8
    SetFontInfoForSelection (kFontSelectionATSUIType, 
                            1, 
                            &myStyle, 
                            myHIObjectTarget);            //9
    status = ATSUDisposeStyle (myStyle);             //10
    return status;
}

OSStatus MyGetFontSelection (EventRef event)

{
    OSStatus status = noErr;
    FMFontFamilyInstance    instance;                  //1
    FMFontSize              fontSize;       

    instance.fontFamily = kInvalidFontFamily;
    instance.fontStyle = normal;
    fontSize = 0;
                                        
    status = GetEventParameter (event, kEventParamFMFontFamily,
                                    typeFMFontFamily, NULL,
                                    sizeof (instance.fontFamily), 
                                    NULL, &(instance.fontFamily));             //2
    check_noerr (status);            //3

    status = GetEventParameter (event, kEventParamFMFontStyle,
                                    typeFMFontStyle, NULL,
                                    sizeof (instance.fontStyle), 
                                    NULL, &(instance.fontStyle));           //4
    check_noerr (status);

    status = GetEventParameter (event, kEventParamFMFontSize,
                                    typeFMFontSize, NULL,
                                    sizeof( fontSize), NULL, &fontSize);              //5
                                    
    fprintf(stderr,"\n family=%d, style=%d, size=%d",  instance.fontFamily, instance.fontStyle, fontSize);                              
    check_noerr (status);

    return status;
}

#endif /* HAVE_AQUA */

#endif /* __AQUA_CONSOLE__ */
