/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2003  Robert Gentleman, Ross Ihaka
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
#include <Rversion.h>

#include "Graphics.h"
#include "Fileio.h"
#include "Rdevices.h"		/* KillAllDevices() [nothing else?] */


#define __SYSTEM__
#include "../../unix/devUI.h" /* includes Startup.h */
#undef __SYSTEM__

#include "../../unix/Runix.h"

#include <R_ext/eventloop.h>

#ifdef HAVE_AQUA
#define __DEBUGGING__
#include <Carbon/Carbon.h>

#include "Raqua.h"


unsigned char Lat2Mac[] = { 
 32,  32,  32,  32,  32,  32,  32,  32,  32,  32, 
 32,  32,  32,  32,  32,  32, 245,  96, 171, 246,
247, 248, 249, 250, 172,  32, 251, 252,  32, 253, 
254, 255,  32, 193, 162, 163,  32, 180,  32, 164, 
172, 169, 187, 199, 194,  45, 168, 248, 161, 177,
 32,  32, 171, 181, 166, 225, 252,  32, 188, 200, 
 32,  32,  32, 192, 203, 231, 229, 204, 128, 129,
174, 130, 233, 131, 230, 232, 237, 234, 235, 236,
 32, 132, 241, 238, 239, 205, 133,  32, 175, 244,
242, 243, 134,  32,  32, 167, 136, 135, 137, 139,
138, 140, 190, 141, 143, 142, 144, 145, 147, 146,
148, 149,  32, 150, 152, 151, 153, 155, 154, 214, 
191, 157, 156, 158, 159,  32,  32, 216};

unsigned char Mac2Lat[] = { 
196, 197, 199, 201, 209, 214, 220, 225, 224, 226, 
228, 227, 229, 231, 233, 232, 234, 235, 237, 236, 
238, 239, 241, 243, 242, 244, 246, 245, 250, 249, 
251, 252,  32, 176, 162, 163, 167,  32, 182, 223, 
174, 169,  32, 146, 152,  32, 198, 216,  32, 177,
 32,  32, 165, 181,  32,  32,  32,  32,  32, 170, 
186,  32, 230, 248, 191, 161, 172,  32,  32,  32,
 32, 171, 187,  32,  32, 192, 195, 213,  32,  32,
 32,  32,  32,  32,  96,  39, 247,  32, 255,  32, 
 32,  32,  32,  32,  32,  32,  32, 183,  32,  32,
 32, 194, 202, 193, 203, 200, 205, 206, 207, 204, 
211, 212,  32, 210, 218, 219, 217, 144, 147, 148, 
149, 150, 151, 154, 155, 157, 158, 159};

extern OSStatus OpenPageSetup(WindowRef window);
extern OSStatus OpenPrintDialog(WindowRef window);

extern SA_TYPE SaveAction;
extern SA_TYPE RestoreAction;


void GraphicCopy(WindowPtr window);

/* Items for the Edit menu */
#define	kRCmdEdirObject	'edbj'
/* Items for the Tools menu */
#define kRCmdFileShow		'fshw'
#define kRCmdEditFile		'edtf'

#define kRCmdIsRAquaUpdated	'nraq'

/* Items for the Tools menu */
#define kRCmdShowWSpace		'dols'
#define kRCmdClearWSpace	'dorm'
#define kRCmdBrowseWSpace	'shwb'
#define kRCmdLoadWSpace		'ldws'
#define kRCmdSaveWSpace		'svws'
#define kRCmdLoadWSpaceFile	'lwsf'
#define kRCmdSaveWSpaceFile	'swsf'
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
#define kRCmdBinaryUpdateFromCRAN	'crub'
#define kRCmdBinaryInstallFromCRAN	'crab'
#define kRCmdBinaryInstallFromBioC	'biob'
#define kRCmdUpdateFromCRAN	'crup'
#define kRCmdUpdateFromBioC	'bcup'
#define kRCmdInstallFromBinary	'ipbt'
#define kRCmdInstallFromSrc	'ipfs'
#define kRCmdInstallFromSrcDir	'ipsd'
#define kRCmdBioCBundleAll      'bial'
#define kRCmdBioCBundleAffy     'biaf'
#define kRCmdBioCBundleCDNA     'bicd'


/* item in the Window menu */
#define kRNewQuartz             'nwqz'
#define kRActivateDevice        'awqz'


/* items in the Help Menu */
#define kRHelpStart		'rhlp'
#define kRHelpOnTopic		'rhot'
#define kRSearchHelpOn		'rsho'
#define kRExampleRun		'rexr'
#define kRAquaFAQ		'rfaq'

#define kRDlog	  'RDLG'
#define	kRDlogMsg  1000
#define	kRDlogText 1001
#define	kRDlogProc 1002
#define	kRDlogCanc 1003

#define kRGUI	'RGUI'
#define kRGUIBusy  1000
#define kRGUISep   1001
#define kRGUIText   1002

#define kRCustomEventClass 'revt'
#define kRWakeUpPlease 	'wake'

OSStatus 	InitMLTE(void);
TXNObject	RConsoleOutObject = NULL;
TXNObject	RConsoleInObject = NULL;
bool 		WeHaveConsole = false;
bool 		InputFinished = false;
bool		EditingFinished = true;
bool		HelpSearchBrowserFinished = true;
bool		DataManagerFinished = false;
bool		PackageManagerFinished = false;
bool		DataEntryFinished = false;
bool		BrowsePkgFinished = false;
bool		InputDialogFinished = false;

Boolean HaveContent = false;
Boolean HaveBigBuffer = false;

void Raqua_helpsearchbrowser(void);


TXNFrameID		OutframeID	= 0;
TXNFrameID		InframeID	= 0;

WindowRef	RInputDialog=NULL;
WindowRef	RAboutWindow=NULL;
WindowRef	RPrefsWindow=NULL;
WindowRef	ConsoleWindow=NULL;

ProcessSerialNumber AquaPSN,RPSN;
void RAqua2Front(void);
void SendReturnKey(void);


#define	kCMDEventClass	'DCMD'  /* Event class command AE */
#define	kCMDEvent    	'DCMD'  /* Event command          */
#define CMDLineSize 2048
static char CMDString[CMDLineSize+1];

pascal OSErr  HandleDoCommandLine (AppleEvent *theAppleEvent, AppleEvent* reply, long handlerRefCon);

/* external symbols from aquaprefs.c */
extern pascal void RPrefsHandler(WindowRef window);
extern void ActivatePrefsWindow(void);
extern void DeactivatePrefsWindow(void);
extern void CopyPrefs(RAquaPrefsPointer From, RAquaPrefsPointer To);
extern ControlRef GrabCRef(WindowRef theWindow,OSType theSig, SInt32 theNum);
extern RAquaPrefs CurrentPrefs, TempPrefs;
extern FMFontFamilyInstance    instance;
extern FMFontSize              fontSize;       


void RSetTab(void);
void RSetFontSize(void);
void RSetFont(void);
DialogItemIndex YesOrNot(char *title, char *msg, char *action, char *cancel);
DialogItemIndex WantToSave(WindowRef window, char *title, char *msg);

void Raqua_CleanUp(SA_TYPE saveact, int status, int runLast);
void Raqua_Suicide(char *s);
void Raqua_ShowMessage(char *msg);
void CloseAllHelpWindows(void);
void CloseAllEditWindows(void);

#define	      	MAX_NUM_OF_WINS	1000
WindowRef     	EditWindowsList[MAX_NUM_OF_WINS];
WindowRef     	HelpWindowsList[MAX_NUM_OF_WINS];
int		NumOfEditWindows = 0;
int		NumOfHelpWindows = 0;
                
OSStatus AddEditWindow(WindowRef window);
OSStatus AddHelpWindow(WindowRef window);
OSStatus RemEditWindow(WindowRef window);
OSStatus RemHelpWindow(WindowRef window);

void DestroyHelpWindow(WindowRef window);
void DestroyEditWindow(WindowRef window);

OSStatus AddHelpWindow(WindowRef window){

        if(window == NULL)
         return(-1);

        if(NumOfHelpWindows == MAX_NUM_OF_WINS){
            Raqua_ShowMessage("Too many help windows opened");
            return(-1);
        }

        NumOfHelpWindows++;
        HelpWindowsList[NumOfHelpWindows-1] = window;
        return(noErr);
}

OSStatus RemHelpWindow(WindowRef window){
        int i,j;
        
        if((window == NULL) || (NumOfHelpWindows == 0))
         return(-1);

        for(i=0;i<NumOfHelpWindows;i++)
         if(window == HelpWindowsList[i]){
          for(j=i;j<NumOfHelpWindows-1;j++)
           HelpWindowsList[j] = HelpWindowsList[j+1];
          NumOfHelpWindows--; 
        }

        return(noErr);
}

OSStatus AddEditWindow(WindowRef window){

        if(window == NULL)
         return(-1);

        if(NumOfEditWindows == MAX_NUM_OF_WINS){
            Raqua_ShowMessage("Too many edit windows opened");
            return(-1);
        }

        NumOfEditWindows++;
        EditWindowsList[NumOfEditWindows-1] = window;
        return(noErr);
}

OSStatus RemEditWindow(WindowRef window){
        int i,j;

        if((window == NULL) || (NumOfEditWindows == 0))
         return(-1);

        for(i=0;i<NumOfEditWindows;i++)
         if(window == EditWindowsList[i]){
          for(j=i;j<NumOfEditWindows-1;j++)
           EditWindowsList[j] = EditWindowsList[j+1];
          NumOfEditWindows--; 
        }

        return(noErr);
}


                
static pascal OSStatus
RCmdHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
static pascal OSStatus
RWinHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );
void RescaleInOut(double prop);


OSErr DoSelectDirectory( char *buf, char *title );
OSStatus SelectFile(FSSpec *outFSSpec,  char *Title, Boolean saveit, Boolean HaveFName);
OSStatus FSPathMakeFSSpec(const UInt8 *path, FSSpec *spec, Boolean *isDirectory);
OSStatus FSMakePath(SInt16 volRefNum, SInt32 dirID, ConstStr255Param name, UInt8 *path,
	UInt32 maxPathSize);
OSErr FSMakeFSRef(FSVolumeRefNum volRefNum, SInt32 dirID, ConstStr255Param name, FSRef *ref);


int 	Raqua_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager);
int	Raqua_ChooseFile(int new, char *buf, int len);

int 	Raqua_Edit(char *filename);
void 	Raqua_StartConsole(Rboolean OpenConsole);
void 	CloseRAquaConsole(void);
void 	Raqua_WriteConsole(char *buf, int len);
int 	Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory);
void Raqua_ResetConsole(void);
void Raqua_FlushConsole(void);
void Raqua_ClearerrConsole(void);
int NewHelpWindow(char *fileName, char *title, char *WinTitle);
		     
extern Boolean isConsoleFont;
            
extern OSStatus MySetFontSelection(void);                              
extern OSStatus MyGetFontSelection(EventRef event);
                     

                     
void consolecmd(char *cmd);
void RSetColors(void);
                   
                   
static const EventTypeSpec	REvents[] =
{
	{ kEventClassTextInput, kEventTextInputUnicodeForKeyEvent }
};




static const EventTypeSpec	aboutSpec =
	{ kEventClassWindow, kEventWindowClose };

static const EventTypeSpec	inputSpec =
    { kEventClassControl, kEventControlHit };


static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon); 
static pascal OSStatus KeybHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
static pascal OSStatus RAboutWinHandler(EventHandlerCallRef handlerRef, EventRef event, void *userData);
static  OSStatus RInputDialogHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );


OSStatus DoCloseHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );

extern void CloseDataEntry(void);
extern void CloseBrowsePkg(void);
extern void CloseDataManager(void);
extern void CloseHelpSearchBrowser(void);
extern void ClosePackageManager(void);

static  OSStatus GenContEventHandlerProc( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData );


static const EventTypeSpec KeybEvents[] = {{ kEventClassKeyboard, kEventRawKeyDown }};

EventRef	WakeUpEvent;

static const EventTypeSpec	RCmdEvents[] =
{
	{ kEventClassCommand, kEventCommandProcess },
	{ kEventClassCommand, kEventCommandUpdateStatus }
};

static const EventTypeSpec	RGlobalWinEvents[] =
{
        { kEventClassWindow, kEventWindowBoundsChanged } ,
        { kEventClassWindow, kEventWindowFocusAcquired },
        { kEventClassWindow, kEventWindowFocusRelinquish },
        { kEventClassFont, kEventFontPanelClosed},
        { kEventClassFont, kEventFontSelection},
        { kEventClassMouse, kEventMouseDown},
        { kRCustomEventClass, kRWakeUpPlease }
};

static const EventTypeSpec	RCloseWinEvent[] = 
{
        { kEventClassWindow, kEventWindowClose }        
};



void InitAboutWindow(void);

int GetTextFromWindow(char *msg, char *text, int len);

void HistBack(void);
void HistFwd(void);
void maintain_cmd_History(char *buf);
void Raqua_write_history(char *file);
void Raqua_read_history(char *file);
SEXP Raqua_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env);
SEXP Raqua_savehistory(SEXP call, SEXP op, SEXP args, SEXP env);
OSStatus SaveWindow(WindowRef window, Boolean ForceNewFName);
static void Aqua_FlushBuffer(void);

MenuRef HelpMenu = NULL; /* Will be the Reference to Apple's Help Menu */
static 	short 	RHelpMenuItem=-1;
static 	short 	RAquaFAQMenuItem=-1;
static 	short 	RTopicHelpItem=-1;
static	short 	RunExampleItem=-1;
static	short	SearchHelpItem=-1;
static  short  	PreferencesItem=-1;

int InputDialogAns = kRDlogCanc;      
extern void SetDefaultPrefs(void);
extern void SetUpPrefsWindow(RAquaPrefsPointer Settings);

extern void GetDialogPrefs(void);
extern	void GetRPrefs(void);
extern void SaveRPrefs(void);

void Raqua_GetQuartzParameters(double *width, double *height, double *ps, char *family, Rboolean *antialias, Rboolean *autorefresh);

TXNControlTag	ROutTag[] = {kTXNIOPrivilegesTag, kTXNNoUserIOTag, kTXNWordWrapStateTag};
TXNControlData  ROutData[] = {kTXNReadWrite, kTXNReadOnly, kTXNNoAutoWrap};

TXNControlTag	RReadOnlyTag[] = {kTXNNoUserIOTag};
TXNControlData  RReadOnlyData[] = {kTXNReadOnly};

TXNControlTag	RReadWriteTag[] = {kTXNNoUserIOTag};
TXNControlData  RReadWriteData[] = {kTXNReadWrite};

TXNControlTag	RInTag[] = { kTXNWordWrapStateTag};
TXNControlData  RInData[] = {kTXNNoAutoWrap};
       
TXNControlTag	RHelpTag[] = {kTXNIOPrivilegesTag, kTXNNoUserIOTag, kTXNWordWrapStateTag};
TXNControlData  RHelpData[] = {kTXNReadWrite, kTXNReadOnly, kTXNNoAutoWrap};
       
TXNControlTag	REditTag[] = {kTXNWordWrapStateTag};
TXNControlData  REditData[] = {kTXNNoAutoWrap};
           
TXNControlTag   txnControlTag[1];
TXNControlData  txnControlData[1];
TXNMargins      txnMargins;
           
static	pascal	void 	RIdleTimer(EventLoopTimerRef inTimer, EventLoopIdleTimerMessage inState, void * inUserData);
static	pascal	void	OtherEventLoops( EventLoopTimerRef inTimer, void *inUserData );
static	pascal	void	ReadStdoutTimer( EventLoopTimerRef inTimer, void *inUserData );
static	pascal	void	FlushConsoleTimer( EventLoopTimerRef inTimer, void *inUserData );

EventLoopTimerRef	Inst_RIdleTimer;
EventLoopTimerRef	Inst_OtherEventLoops;
EventLoopTimerRef	Inst_ReadStdoutTimer;
EventLoopTimerRef	Inst_FlushConsoleTimer;


void SetUpRAquaMenu(void);
OSStatus InstallAppHandlers(void);
OSStatus SetUpGUI(void);

OSStatus SetUpGUI(void){
    IBNibRef 	nibRef = NULL;
    OSErr	err = noErr;
    CFURLRef    bundleURL = NULL;
    CFBundleRef RBundle = NULL;
 
    if( (err = CreateNibReference(CFSTR("main"), &nibRef)) != noErr)
       goto guifailure;
    
    if( (err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"))) != noErr)
       goto guifailure;
        
    if( (err = CreateWindowFromNib(nibRef,CFSTR("MainWindow"),&ConsoleWindow)) != noErr)
       goto guifailure;
    
    if( (err = CreateWindowFromNib(nibRef,CFSTR("AboutWindow"),&RAboutWindow)) != noErr)
       goto guifailure;

    if( (err = CreateWindowFromNib(nibRef,CFSTR("PrefsWindow"),&RPrefsWindow)) != noErr)
       goto guifailure;

    if( (err = CreateWindowFromNib(nibRef,CFSTR("InputDialog"),&RInputDialog)) != noErr)
       goto guifailure;
        
guifailure:
    if(nibRef)
        DisposeNibReference(nibRef);

    return(err);
}           

OSStatus SetUPConsole(void);


OSStatus SetUPConsole(void){
    TXNFrameOptions	frameOptions;
    Rect 		OutFrame, InFrame, WinFrame;
    OSStatus		err = noErr;

    if (ConsoleWindow == NULL)
     return(-1);
    
                 
    GetWindowPortBounds (ConsoleWindow,&WinFrame);
    SetRect(&OutFrame,0,26,WinFrame.right,WinFrame.bottom-110);
    SetRect(&InFrame,0,WinFrame.bottom-100,WinFrame.right,WinFrame.bottom);
                
    frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask|kTXNMonostyledTextMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;
		

    err = TXNNewObject(NULL, ConsoleWindow, &OutFrame, frameOptions, kTXNTextEditStyleFrameType,
                            kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleOutObject,
                            &OutframeID, 0);
    frameOptions  = kTXNMonostyledTextMask|kTXNShowWindowMask | kTXNWantHScrollBarMask;
    frameOptions |= kTXNWantVScrollBarMask | kTXNDrawGrowIconMask;
		
    err = TXNNewObject(NULL, ConsoleWindow, &InFrame, frameOptions, kTXNTextEditStyleFrameType,
                            kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleInObject,
                            &InframeID, 0);

    if ( (RConsoleOutObject != NULL) && (RConsoleInObject != NULL) ){
                    /* sets the state of the scrollbars so they are drawn correctly */
        err = TXNActivate(RConsoleOutObject, OutframeID, kScrollBarsAlwaysActive);
        err = TXNActivate(RConsoleInObject, InframeID, kScrollBarsAlwaysActive);
        	        
			
        err = SetWindowProperty(ConsoleWindow,'RCON','rFrm',sizeof(TXNFrameID),&OutframeID);
        err = SetWindowProperty(ConsoleWindow,'RCON','rObj',sizeof(TXNObject),&RConsoleOutObject);
        err = SetWindowProperty(ConsoleWindow,'RCON','rFrm',sizeof(TXNFrameID),&InframeID);
        err = SetWindowProperty(ConsoleWindow,'RCON','rObj',sizeof(TXNObject),&RConsoleInObject);
    }


                
    frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask|kTXNMonostyledTextMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;
		

    if(err != noErr)
     return(err);

    WeHaveConsole = true;
    RSetColors();
    RescaleInOut(0.8);
    TXNSetTXNObjectControls(RConsoleOutObject, false, 3, ROutTag, ROutData);
    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RInTag, RInData);
        
    txnControlTag[0] = kTXNMarginsTag;
    txnControlData[0].marginsPtr = &txnMargins;
  
    txnMargins.leftMargin  = txnMargins.topMargin = 5;
    txnMargins.rightMargin = txnMargins.bottomMargin = 5;
    TXNSetTXNObjectControls(RConsoleOutObject,false,1,txnControlTag,txnControlData);
    TXNSetTXNObjectControls(RConsoleInObject,false,1,txnControlTag,txnControlData);

    err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(KeybHandler), 
                                          GetEventTypeCount(KeybEvents),
                                          KeybEvents, (void *)ConsoleWindow, NULL);
         
    err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)ConsoleWindow, NULL);
    err = InstallStandardEventHandler(GetWindowEventTarget(ConsoleWindow));        
 
    TXNFocus(RConsoleOutObject,true);

    return err;

}

void	Raqua_ProcessEvents(void);


Boolean AlreadyRunning = false;           



FILE *RAquaStdout;
FILE *RAquaStdoutOrig;

FILE *RAquaStdoutBack;
FILE *RAquaStderr;
FILE *RAquaStderrOrig;
FILE *RAquaStderrBack;
const char *StdoutFName;
const char *StderrFName;

int stderr_fd = -1;
int stderr_dup_fd = -1;

void OpenStdoutPipe(void);
void CloseStdoutPipe(void);
void OpenStderrPipe(void);
void CloseStderrPipe(void);
extern RSetPipes(void);


void InitAquaIO(void);
void CloseAquaIO(void);

void InitAquaIO(void){
    StdoutFName = R_tmpnam("RStdout", R_TempDir);
    StderrFName = R_tmpnam("RStderr", R_TempDir);
}
 
void Raqua_StartConsole(Rboolean OpenConsole)
{
    IBNibRef 	nibRef = NULL;
    OSErr	err = noErr, result;
    CFURLRef    bundleURL = NULL;
    CFBundleRef RBundle = NULL;
    
    char	buf[300];
    FSRef 	ref;
   
     
    if(OpenConsole){ 
     if(SetUpGUI() != noErr)
       goto noconsole;
    
     InitAboutWindow();
      
     GetRPrefs();
     RSetPipes();
      
     InitCursor();
    
     if (TXNVersionInformation == (void*)kUnresolvedCFragSymbolAddress)
         goto noconsole;

     if( InitMLTE() != noErr )
      goto noconsole;

     if( SetUPConsole() != noErr)
      goto noconsole;
	
      CreateEvent(NULL, kRCustomEventClass, kRWakeUpPlease, 0, kEventAttributeNone, &WakeUpEvent);
   
     if(err == noErr)
          InstallPrefsHandlers();
    }
           
    InstallAppHandlers();
    
    if(OpenConsole){
     if(ConsoleWindow!= NULL){
        SelectWindow(ConsoleWindow);
        RSetTab();
        RSetFontSize();
        RSetFont();
        SetUpRAquaMenu();
     }   
       
     chdir(R_ExpandFileName("~/"));
       
     if (R_RestoreHistory)
	Raqua_read_history(R_HistoryFile);
    }   
        
    
   InstallEventLoopIdleTimer(GetMainEventLoop(), kEventDurationMillisecond, kEventDurationMillisecond*2, (EventLoopIdleTimerUPP)RIdleTimer, NULL, &Inst_RIdleTimer);


    
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationMillisecond*10, NewEventLoopTimerUPP(OtherEventLoops), NULL, &Inst_OtherEventLoops);
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationSecond /5, NewEventLoopTimerUPP(ReadStdoutTimer), NULL, &Inst_ReadStdoutTimer);
    InstallEventLoopTimer(GetMainEventLoop(),0, kEventDurationSecond*5, NewEventLoopTimerUPP(FlushConsoleTimer), NULL, &Inst_FlushConsoleTimer);

    RAqua2Front();

    if(ConsoleWindow != NULL)
     SelectWindow(ConsoleWindow);
//    otherPolledEventHandler = R_PolledEvents;
 //   R_PolledEvents = Raqua_ProcessEvents;  

    return;
            
noconsole:
    if(bundleURL)
     CFRelease( bundleURL );
    if(RBundle)
     CFRelease( RBundle ); 
    CloseRAquaConsole();
}

OSStatus InstallAppHandlers(void){
  	 OSStatus err = noErr;
         
         InstallStandardEventHandler(GetApplicationEventTarget());

         err = InstallApplicationEventHandler( NewEventHandlerUPP(RCmdHandler), GetEventTypeCount(RCmdEvents),
                                                RCmdEvents, 0, NULL);
	    
                                                  
         err = InstallApplicationEventHandler( NewEventHandlerUPP(RWinHandler), GetEventTypeCount(RGlobalWinEvents),
                                                RGlobalWinEvents, 0, NULL);
                                                
         err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, 
                        NewAEEventHandlerUPP((AEEventHandlerProcPtr)QuitAppleEventHandler), 
                                    0, false);

         err = AEInstallEventHandler(kCMDEventClass, kCMDEvent,
                                NewAEEventHandlerUPP( (AEEventHandlerProcPtr)HandleDoCommandLine ),
                                0, false);

        err = InstallWindowEventHandler(RAboutWindow, NewEventHandlerUPP(RAboutWinHandler), 1, &aboutSpec, 
                                (void *)RAboutWindow, NULL);
                                
        err = InstallWindowEventHandler(RInputDialog, NewEventHandlerUPP(RInputDialogHandler), 1, &inputSpec, 
                                (void *)RInputDialog, NULL);
                                
        err = InstallControlEventHandler( GrabCRef(RInputDialog,kRDlog,kRDlogProc),  
                            RInputDialogHandler , 1,  &inputSpec, RInputDialog, NULL );
        
        err = InstallControlEventHandler( GrabCRef(RInputDialog,kRDlog,kRDlogCanc),  
                            RInputDialogHandler , 1,  &inputSpec,  RInputDialog, NULL );        
                        
        return err;                        
}

void SetUpRAquaMenu(void){
    Str255	menuStr;
    HMGetHelpMenu(&HelpMenu,NULL);

    if (HelpMenu != nil) {
                CopyCStringToPascal("R Help", menuStr);
		AppendMenu(HelpMenu, menuStr);
		RHelpMenuItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, RHelpMenuItem, kRHelpStart); 
                SetMenuItemCommandKey(HelpMenu, RHelpMenuItem, false, '?');
 

                CopyCStringToPascal("RAqua FAQ", menuStr);
		AppendMenu(HelpMenu, menuStr);
		RAquaFAQMenuItem = CountMenuItems(HelpMenu);
                SetMenuItemCommandID(HelpMenu, RAquaFAQMenuItem, kRAquaFAQ); 

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
       EnableMenuCommand(NULL, kHICommandPreferences);
       
       DrawMenuBar();
       	
}

 
static	pascal	void 	RIdleTimer(EventLoopTimerRef inTimer, EventLoopIdleTimerMessage inState, void * inUserData)
{

  PostEventToQueue( GetMainEventQueue(), WakeUpEvent, kEventPriorityHigh);

}
 

static	pascal	void	FlushConsoleTimer( EventLoopTimerRef inTimer, void *inUserData )
{
         Aqua_FlushBuffer();
}

static	pascal	void	OtherEventLoops( EventLoopTimerRef inTimer, void *inUserData )
{
         R_runHandlers(R_InputHandlers, R_checkActivity(0, 1));
}

 

void CloseRAquaConsole(void){

  DisposeWindow(RInputDialog);
  DisposeWindow(RAboutWindow);
  DisposeWindow(RPrefsWindow);
  
  TXNDeleteObject(RConsoleOutObject);
  TXNDeleteObject(RConsoleInObject);
  DisposeWindow(ConsoleWindow);
    
  TXNTerminateTextension();
  
  ReleaseEvent(WakeUpEvent);
  RemoveEventLoopTimer(Inst_RIdleTimer);
  RemoveEventLoopTimer(Inst_OtherEventLoops);
  RemoveEventLoopTimer(Inst_ReadStdoutTimer);
  RemoveEventLoopTimer(Inst_FlushConsoleTimer);

  CloseAquaIO();
}

void OpenStdoutPipe(void){
//  fprintf(stderr,"\nstdout=%x, aquaout=%x",stdout,RAquaStdout);
    RAquaStdout = freopen(StdoutFName, "w", stdout);
    RAquaStdoutBack = fopen(StdoutFName, "r");
//  fprintf(stderr,"\nstdout=%x, aquaout=%x",stdout,RAquaStdout);
}

void OpenStderrPipe(void){
    RAquaStderr = freopen(StderrFName, "w", stderr);
    RAquaStderrBack = fopen(StderrFName, "r");
}

void CloseStdoutPipe(void){
  if(RAquaStdout) {  
  freopen("/dev/null", "w", stdout);
    fclose(RAquaStdout);
}
  RAquaStdout = (FILE *)NULL;  
  if(RAquaStdoutBack){
    freopen("/dev/null", "w", stderr);
    fclose(RAquaStdoutBack);
  }
  RAquaStdoutBack = (FILE *)NULL;  
}

void CloseStderrPipe(void){
  if(RAquaStderr) {
//    freopen ("/dev/null", "w", stderr);
    fclose(RAquaStderr);
 }
  RAquaStderr =   (FILE *)NULL;
  if(RAquaStderrBack)
    fclose(RAquaStderrBack);
  RAquaStderrBack = (FILE *)NULL;  
}


void CloseAquaIO(void){
    CloseStdoutPipe();
    CloseStderrPipe();
    unlink(StdoutFName);  
    unlink(StderrFName);  
}

void Aqua_RWrite(char *buf);
void Aqua_RnWrite(char *buf, int len);


TXNTypeAttributes RInAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &CurrentPrefs.FGInputColor}};
TXNTypeAttributes ROutAttr[] = {{ kTXNQDFontColorAttribute, kTXNQDFontColorAttributeSize, &CurrentPrefs.FGOutputColor}};

/* Buffered output code by Thomas Lumley */

/* buffer whose last character is \0 at index end_of_buffer */
#define AQUA_MAXBUFLEN 32000
static char outputbuffer[AQUA_MAXBUFLEN+2];
static int  end_of_buffer=0;
static int  WeAreBuffering=0;

void Raqua_WriteConsole(char *str, int len)
{
    OSStatus err;
    TXNOffset oStartOffset; 
    TXNOffset oEndOffset;
    EventRef REvent;
    unsigned char tmp;
    int	i;
    char	*buf = NULL;

    if( (buf = malloc(len+1)) != NULL){
      for(i=0;i <len;i++){
        tmp = (unsigned char)str[i];
      if(tmp>127)
       buf[i] = (char)Lat2Mac[tmp-127-1];
      else
       buf[i] = str[i]; 
      }
     } else return;  
     buf[len] = '\0';
     
    if(WeHaveConsole){
 
         if (WeAreBuffering != CurrentPrefs.Buffering){
	   Aqua_FlushBuffer();
	   WeAreBuffering = CurrentPrefs.Buffering;
	 }

        if (WeAreBuffering){
 	if (strlen(buf)+1+end_of_buffer >= CurrentPrefs.BufferSize){
	  TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
 	if (end_of_buffer>0)
            err =  TXNSetData( RConsoleOutObject, kTXNTextData, outputbuffer, end_of_buffer, 
                                kTXNEndOffset, kTXNEndOffset);
        err = TXNSetData(RConsoleOutObject, kTXNTextData, buf, strlen(buf), kTXNEndOffset, 
                                kTXNEndOffset);
 	outputbuffer[0]='\0';
 	end_of_buffer = 0;
 	} else {
 	  strcpy(outputbuffer+end_of_buffer, buf);
 	  end_of_buffer+= strlen(buf);
 	}
       } else {
         TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
         err =  TXNSetData (RConsoleOutObject, kTXNTextData, buf, strlen(buf), kTXNEndOffset, kTXNEndOffset);
       }
         Raqua_ProcessEvents();
     } else {
     fprintf(stderr,"%s", buf);
    }
    
    free(buf);

}


static void Aqua_FlushBuffer(void){
  if (WeHaveConsole) {
     if (WeAreBuffering){
       TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
       if (end_of_buffer>0)
	 TXNSetData (RConsoleOutObject, kTXNTextData, outputbuffer, end_of_buffer, kTXNEndOffset, kTXNEndOffset);
       outputbuffer[0]='\0';
       end_of_buffer = 0;
     }
  }
}
 

SEXP Raqua_doflushconsole(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Aqua_FlushBuffer();
    return R_NilValue;
}

void RSetColors(void)
{
   TXNBackground RBGInfo;   


/* setting FG colors */
   TXNSetTypeAttributes( RConsoleOutObject, 1, ROutAttr, 0, kTXNEndOffset );
   TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );

/* setting BG colors */
   RBGInfo.bgType = kTXNBackgroundTypeRGB;
   RBGInfo.bg.color = CurrentPrefs.BGOutputColor;        
   TXNSetBackground(RConsoleOutObject, &RBGInfo);

   RBGInfo.bg.color = CurrentPrefs.BGInputColor;                 
   TXNSetBackground(RConsoleInObject, &RBGInfo);
 
 
}
 
OSStatus InitMLTE(void)
{
	OSStatus				status = noErr;
	TXNMacOSPreferredFontDescription	defaults; 
	TXNInitOptions 				options;
        SInt16                            fontID;
        Str255	fontname;
        
        CopyCStringToPascal(CurrentPrefs.ConsoleFontName, fontname);
        GetFNum(fontname,&fontID);
        
	defaults.fontID = fontID;  
	defaults.pointSize = Long2Fix(CurrentPrefs.ConsoleFontSize); 
        defaults.encoding = CreateTextEncoding(kTextEncodingMacRoman, kTextEncodingDefaultVariant,
                                                kTextEncodingDefaultFormat);
  	defaults.fontStyle = 0;

	options = kTXNWantMoviesMask | kTXNWantSoundMask | kTXNWantGraphicsMask;

	status = TXNInitTextension(&defaults, 1, options);


        instance.fontStyle = 0;
        instance.fontFamily = fontID;

	return(status);
}



SInt32                                   curBufPos, finalBufPos;

Handle BufDataHandle=NULL;
   
int Raqua_ReadConsole(char *prompt, unsigned char *buf, int len,
		     int addtohistory)
{
   OSStatus 	err = noErr;
   TXNOffset 	oStartOffset; 
   TXNOffset 	oEndOffset;
   char		TempBuf;
   int 		i, j, lg=0, txtlen;
   unsigned char tmp;
          
   if(!InputFinished)
     Aqua_RWrite(prompt);
   TXNFocus(RConsoleInObject,true);
   TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );

     
   while(!InputFinished & !HaveBigBuffer) 
        Raqua_ProcessEvents();

  
     if(!HaveBigBuffer){
       txtlen = TXNDataSize(RConsoleInObject)/2;
       finalBufPos = txtlen;
       curBufPos = 0;
       if(BufDataHandle) { 
                 HUnlock(BufDataHandle);
                 DisposeHandle(BufDataHandle);
                 BufDataHandle =NULL;
                }
       err = TXNGetDataEncoded(RConsoleInObject, 0, txtlen, &BufDataHandle, kTXNTextData);
       TXNSetData(RConsoleInObject,kTXNTextData,NULL,0,kTXNStartOffset ,kTXNEndOffset );
       lg = min(len,txtlen+1); /* has to txtlen+1 as the string is not terminated */
       HLock( BufDataHandle );
       HaveBigBuffer = true;
      }
     if(HaveBigBuffer){
      for (i = curBufPos; i <= finalBufPos; i++) {
        TempBuf = (*BufDataHandle)[i];
        if ((TempBuf == '\r') || (i == finalBufPos)){
                strncpy(buf,*(BufDataHandle)+curBufPos,i-curBufPos+1);
                buf[i-curBufPos] = '\n';
                buf[i-curBufPos+1] = '\0';
		Raqua_WriteConsole(buf,strlen(buf));
                if (strlen(buf) > 1)
                    maintain_cmd_History(buf);
                for(j=0; j<strlen(buf); j++){
                    tmp = (unsigned char)buf[j];
                    if(tmp>127)
                        buf[j] = (char)Mac2Lat[tmp-127-1];
                }
                curBufPos = i+1;
                break;
        }
      } /* for */
      
     if(i != finalBufPos) {
            HaveBigBuffer = true;
            InputFinished = false;
     } else { 	
                
                HaveBigBuffer = false;
                InputFinished = false;
                HUnlock(BufDataHandle);
                DisposeHandle(BufDataHandle);
                BufDataHandle = NULL;
     }
   } /* HaveBigBuffer */

 
  
   return(1);
}


void Aqua_RWrite(char *buf)
{
    if(WeHaveConsole){
       Aqua_FlushBuffer();
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
    Aqua_FlushBuffer();
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
 
  
 if(!EditingFinished)
  return(err);
  
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
 

 
int GetTextFromWindow(char *msg, char *text, int len){
    ControlID		DLogMsgID = {kRDlog, kRDlogMsg};
    ControlID		DLogTextID = {kRDlog, kRDlogText};
    ControlID		DLogProcID = {kRDlog, kRDlogProc};
    ControlID		DLogCancID = {kRDlog, kRDlogCanc};
    CFStringRef		CFMsg, inputText;
    ControlHandle	RDlogControl;
    Size		outActualSize;
    ControlFontStyleRec	controlStyle;
    
    if(text==NULL)
     return(kRDlogCanc);    /* you should provide a valid pointer */



    if(msg != NULL){
        GetControlByID(RInputDialog, &DLogMsgID, &RDlogControl);
        CFMsg = CFStringCreateWithCString(NULL, msg, kCFStringEncodingASCII);
        if(CFMsg){
            SetControlData(RDlogControl, kControlLabelPart, kControlStaticTextCFStringTag, 
                            sizeof(CFStringRef), &CFMsg);
            controlStyle.flags = kControlUseJustMask;
            controlStyle.just = teCenter;
            CFRelease(CFMsg);
        }
    }

    InputDialogAns = kRDlogCanc;
     
    ShowWindow(RInputDialog);
    SelectWindow(RInputDialog);
    
    InputDialogFinished = false;
    
    while(!InputDialogFinished)
            Raqua_ProcessEvents();
                
    if(InputDialogAns == kRDlogProc){
        GetControlByID( RInputDialog, &DLogTextID, &RDlogControl );
        GetControlData( RDlogControl, 0, kControlEditTextCFStringTag, 
                    sizeof(CFStringRef), &inputText, &outActualSize );
        CFStringGetCString(inputText, text, len,  kCFStringEncodingMacRoman);
        CFRelease( inputText );
    }
    
    return InputDialogAns;
}

void InitAboutWindow(void){
    CFStringRef		text;
    CFBundleRef		appBundle;
    ControlID		versionInfoID = {kRAppSignature, kRVersionInfoID};
    ControlID		CopyrightID = {kRAppSignature, kRCopyrightID};
    ControlID		AuthorsID = {kRAppSignature, kRAquaAuthorsID};
    ControlID		ThanksToID = {kRAppSignature, kRAquaThanksToID};
    ControlID		RImageID = {kRAppSignature, kRImageID};
    ControlRef		versionControl;
    ControlFontStyleRec	controlStyle;
    CGDataProviderRef	provider;
    CGImageRef		image = NULL;
    CFStringRef 	fileName = NULL;
    CFURLRef 		url = NULL;
    HIViewRef		contentView, fImageView;
    HIRect		bounds, myViewRect;

    appBundle = CFBundleGetMainBundle();
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("Version %s.%s %s (%s-%s-%s)"), R_MAJOR, 
                                        R_MINOR, R_STATUS, R_YEAR, R_MONTH, R_DAY);
    GetControlByID(RAboutWindow, &versionInfoID, &versionControl);
    SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    controlStyle.flags = kControlUseJustMask;
    controlStyle.just = teCenter;
    CFRelease(text);
    
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("R : Copyright %s, The R Development Core Team"), R_YEAR);
    GetControlByID(RAboutWindow, &CopyrightID, &versionControl);
    SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    controlStyle.flags = kControlUseJustMask;
    controlStyle.just = teCenter;
    CFRelease(text);
    
    text = CFSTR("RAqua GUI by Stefano M. Iacus and Thomas Lumley (2003).\rPlease send feedback to stefano.iacus@unimi.it");
    GetControlByID(RAboutWindow, &AuthorsID, &versionControl);
    SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    controlStyle.flags = kControlUseJustMask;
    controlStyle.just = teCenter;
    CFRelease(text);
    
    text = CFSTR("Thanks to: Jan de Leeuw, Simon Urbanek, Byron Ellis");
    GetControlByID(RAboutWindow, &ThanksToID, &versionControl);
    SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
    controlStyle.flags = kControlUseJustMask;
    controlStyle.just = teCenter;
    CFRelease(text);

     
    if( (fileName = CFStringCreateWithCString(NULL, "RLogo.png", kCFStringEncodingASCII)) != NULL ){
        url = CFBundleCopyResourceURL( appBundle, fileName, NULL, NULL );
        if(url)
         provider = CGDataProviderCreateWithURL( url );
        if(provider)
         image = CGImageCreateWithPNGDataProvider( provider, NULL, false,  kCGRenderingIntentDefault );
        if(provider)
         CGDataProviderRelease( provider );
        if(url)
         CFRelease( url );
        if(fileName)
         CFRelease( fileName );
    }
   
   
    myViewRect.origin.x = 157.0;         
    myViewRect.origin.y = 37.0;
    myViewRect.size.width = 64.0;
    myViewRect.size.height = 64.0;
 
    HIViewFindByID( HIViewGetRoot( RAboutWindow ), kHIViewWindowContentID, &contentView );
    HIImageViewCreate( image, &fImageView );
    HIViewGetBounds( contentView, &bounds );
    HIViewSetFrame( fImageView, &myViewRect );
    HIViewSetVisible( fImageView, true );
    HIViewAddSubview( contentView, fImageView );
    CGImageRelease( image );

}

 
static  OSStatus RInputDialogHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
    ControlRef theCont = NULL;
    ControlID theID;
    WindowRef theWindow = (WindowRef)inUserData;

    GetEventParameter (inEvent, kEventParamDirectObject, typeControlRef,NULL, sizeof(ControlRef), NULL, &theCont);
    GetControlID(theCont,&theID); 
 
    switch(theID.id){
        case kRDlogProc:  
            InputDialogAns = kRDlogProc;
            HideWindow(theWindow); 
            InputDialogFinished = true;
            return(noErr);
        break;
        
        case kRDlogCanc: 
            HideWindow(theWindow); 
            InputDialogAns = kRDlogCanc;
            InputDialogFinished = true;
            return(noErr);
        break;
              
        default:
        break;
    }

   return( eventNotHandledErr );
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
 
 

DialogItemIndex WantToSave(WindowRef window, char *title, char *msg){
    OSStatus				err = noErr;
    DialogRef				WantDialog;
    DialogItemIndex 			userAction = kAlertStdAlertCancelButton;
    DialogItemIndex			itemHit;
    AlertStdCFStringAlertParamRec	paramRec;
    CFStringRef				MsgText, TitleText;
	
    RAqua2Front();
    
    GetStandardAlertDefaultParams(&paramRec,kStdCFStringAlertVersionOne);
    paramRec.movable		= true;
    paramRec.helpButton		= false;
    paramRec.cancelButton	= kAlertStdAlertCancelButton;
    paramRec.defaultText 	= CFSTR("Save");
    paramRec.cancelText 	= CFSTR("Cancel");
    paramRec.otherText 		= CFSTR("Don't Save");

    if(msg != NULL)
      MsgText = CFStringCreateWithCString(NULL, msg, kCFStringEncodingASCII); 
    else
      MsgText = CFSTR("Save changes to the current document?");

    if(title != NULL)
     TitleText = CFStringCreateWithCString(NULL, title, kCFStringEncodingASCII);
    else
     TitleText = CFSTR("Save changes");

    err = CreateStandardAlert(kAlertCautionAlert,TitleText, MsgText,                                                                                                                                                                                           

                                 				&paramRec, &WantDialog);
    if(err == noErr){
        err = RunStandardAlert(WantDialog,NULL,&itemHit);
        if(err == noErr)
          userAction = itemHit;
    }

    if(TitleText != NULL)
		CFRelease(TitleText);
    if(MsgText != NULL)
		CFRelease(MsgText);

    return(userAction);      
}




void RAqua2Front(void){
    if(ConsoleWindow!=NULL)
     SelectWindow(ConsoleWindow);
     
    if (GetCurrentProcess(&AquaPSN) == noErr)
        (void)SetFrontProcess(&AquaPSN);
}


DialogItemIndex YesOrNot(char *title, char *msg, char *actionlab, char *canclab){
    OSStatus				err = noErr;
    DialogRef				WantDialog;
    DialogItemIndex 			userAction = kAlertStdAlertCancelButton;
    DialogItemIndex			itemHit;
    AlertStdCFStringAlertParamRec	paramRec;
    CFStringRef				MsgText, TitleText;
	
    RAqua2Front();
    
    GetStandardAlertDefaultParams(&paramRec,kStdCFStringAlertVersionOne);
    paramRec.movable		= true;
    paramRec.helpButton		= false;
    paramRec.cancelButton	= kAlertStdAlertCancelButton;

    if(actionlab != NULL)
     paramRec.defaultText = CFStringCreateWithCString(NULL, actionlab, kCFStringEncodingASCII); 
 
    if(canclab != NULL)
     paramRec.cancelText = CFStringCreateWithCString(NULL, canclab, kCFStringEncodingASCII); 
    else
     paramRec.cancelText = CFSTR("Cancel");

    if(msg != NULL)
      MsgText = CFStringCreateWithCString(NULL, msg, kCFStringEncodingASCII); 
    else
      MsgText = CFSTR("Save changes to the current document?");

    if(title != NULL)
     TitleText = CFStringCreateWithCString(NULL, title, kCFStringEncodingASCII);
    else
     TitleText = CFSTR("Save changes");

    err = CreateStandardAlert(kAlertCautionAlert,TitleText, MsgText,                                                                                                                                                                                           

                                 				&paramRec, &WantDialog);
    if(err == noErr){
        err = RunStandardAlert(WantDialog,NULL,&itemHit);
        if(err == noErr)
          userAction = itemHit;
    }

    if(TitleText != NULL)
		CFRelease(TitleText);
    if(MsgText != NULL)
		CFRelease(MsgText);

    return(userAction);      
}

static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
                                     AppleEvent* reply, UInt32 refcon) 
{
  Raqua_CleanUp(SA_SAVEASK, 2, 0);
} 


/* Changes font size in both Console In and Out 
   default size is 12
*/

void RSetFont(void)
{
        TXNTypeAttributes	typeAttr;
        SInt16                  fontID;
        Str255			fontname;
        
        CopyCStringToPascal(CurrentPrefs.ConsoleFontName, fontname);
        GetFNum(fontname,&fontID);
    
        typeAttr.tag = kTXNQDFontFamilyIDAttribute;
        typeAttr.size = kTXNQDFontFamilyIDAttributeSize;
        typeAttr.data.dataValue = fontID;
    
        TXNSetTypeAttributes(RConsoleOutObject, 1, &typeAttr, 0, kTXNEndOffset);
        TXNSetTypeAttributes(RConsoleInObject, 1, &typeAttr, 0, kTXNEndOffset);

        instance.fontStyle = 0;
        instance.fontFamily = fontID;
}

void RSetFontSize(void)
{
    TXNTypeAttributes	typeAttr;
    
    typeAttr.tag = kTXNQDFontSizeAttribute;
    typeAttr.size = kTXNFontSizeAttributeSize;
    typeAttr.data.dataValue = Long2Fix(CurrentPrefs.ConsoleFontSize);

    TXNSetTypeAttributes(RConsoleOutObject, 1, &typeAttr, 0, kTXNEndOffset);
    TXNSetTypeAttributes(RConsoleInObject, 1, &typeAttr, 0, kTXNEndOffset);

}

/* Sets tab space for Console In and Out
   tabsize: number of chars
 */

    
void RSetTab(void){
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
 
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.TabSize*CurrentPrefs.ConsoleFontSize);
    tabdata.tabValue.tabType = kTXNRightTab;
    tabdata.tabValue.filler = 0;
    
         
    TXNSetTXNObjectControls(RConsoleOutObject, false, 1, &tabtag, &tabdata);
    TXNSetTXNObjectControls(RConsoleInObject, false, 1, &tabtag, &tabdata);
}

static pascal OSStatus
RCmdHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void* inUserData )
{
	OSStatus 	err = eventNotHandledErr, result = noErr;
	HICommand	command;
	UInt32		eventKind = GetEventKind( inEvent );
        FSSpec 		tempfss;
        char		buf[300],cmd[2500];
        WindowRef	window = NULL; 
        int 		len,devnum;
        TXNObject	tmpObj;
        NavUserAction	userAction;
        
        window = FrontWindow();
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
              
              case kRCmdIsRAquaUpdated:
                consolecmd("is.RAqua.updated()");
              break;
              
/* File Menu */
              case kHICommandOpen:
               result = SelectFile(&tempfss,"Select File to Source", false, false);
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"source(\"%s\")",buf);
               consolecmd(cmd);
              break;
              
              case kHICommandSave:
               SaveWindow(FrontWindow(),false);  
              break;
          
              case kHICommandSaveAs:
               SaveWindow(FrontWindow(),true);  
              break;
                
              case kRCmdFileShow:
               result = SelectFile(&tempfss,"Select File to Show",false, false);
               if(result != noErr)
                return err;
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               sprintf(cmd,"file.show(\"%s\")",buf);
               consolecmd(cmd);
              break;
              
              case kHICommandNew:
               result = NewEditWindow(NULL);
              break;
             
              case kRCmdEditFile:
               result = SelectFile(&tempfss,"Select File to Edit",false, false);
               result = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300);  
               result = NewEditWindow(buf);
              break;

              case kHICommandPrint:
               OpenPrintDialog(FrontWindow());
              break; 
            
               case kHICommandPageSetup:
                OpenPageSetup(FrontWindow());
               break;



/* Edit Menu */             

              case kHICommandPaste:
               if(TXNIsScrapPastable()){
                if(window == ConsoleWindow){
                 TXNSetSelection(RConsoleInObject,kTXNEndOffset,kTXNEndOffset); 
                 TXNPaste(RConsoleInObject); 
                }
                else
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
                   TXNPaste(tmpObj); 
               }
               break;
              
              /*
                 If selection occurs in both RConsole-Out and RConsole-In, only the 
                 text selected in the RConsole-Out is copied to the clipboard.
                 I'm not sure if it should be the contrary.
              */    
              case kHICommandCopy:
                if(window == ConsoleWindow){              
                 if(!TXNIsSelectionEmpty(RConsoleOutObject))
                    TXNCopy(RConsoleOutObject); 
                 else 
                  if(!TXNIsSelectionEmpty(RConsoleInObject))
                    TXNCopy(RConsoleInObject);    
                 break;                
                } 
                
                if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                    if(!TXNIsSelectionEmpty(tmpObj))
                            TXNCopy(tmpObj);
                break;
                }            
                
                if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                        if(!TXNIsSelectionEmpty(tmpObj))
                            TXNCopy(tmpObj);    
                        break;                    
                } 
                
                if( GetWindowProperty(window, kRAppSignature, 'QRTZ', sizeof(int), NULL, &devnum)  == noErr)
                        GraphicCopy(window);                  
                                               
              break;
          
              case kHICommandCut:
                if(window == ConsoleWindow){              
                  if(!TXNIsSelectionEmpty(RConsoleInObject))
                    TXNCut(RConsoleInObject); 
                } else {
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
                  if(!TXNIsSelectionEmpty(tmpObj))
                   TXNCut(tmpObj);
                }                               
               break;

              case kHICommandSelectAll:
                if(window == ConsoleWindow){              
                 if(!TXNIsSelectionEmpty(RConsoleOutObject))
                    TXNSelectAll(RConsoleOutObject); 
                 else 
                    TXNSelectAll(RConsoleInObject); 
                } else {
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
                    TXNSelectAll(tmpObj);
                 else
                  if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
                    TXNSelectAll(tmpObj);                
                }                               
              break;
              
               case kHICommandClear:
                if(window == ConsoleWindow){              
                 if(!TXNIsSelectionEmpty(RConsoleOutObject))
                    TXNClear(RConsoleOutObject); 
                 else 
                  if(!TXNIsSelectionEmpty(RConsoleInObject))
                    TXNClear(RConsoleInObject); 
                } else {
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                   if(!TXNIsSelectionEmpty(tmpObj))
                    TXNClear(tmpObj);
                 } else
                  if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                   if(!TXNIsSelectionEmpty(tmpObj))
                    TXNClear(tmpObj);                
                  }
                }                               
              break;
 
              case kHICommandUndo:
                if(window == ConsoleWindow){              
                  if(TXNCanUndo(RConsoleInObject,NULL))
                    TXNUndo(RConsoleInObject); 
                } else {
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                  if(TXNCanUndo(tmpObj,NULL))
                   TXNUndo(tmpObj);
                  }
                }                               
               break;


              case kHICommandRedo:
                if(window == ConsoleWindow){              
                  if(TXNCanRedo(RConsoleInObject,NULL))
                    TXNRedo(RConsoleInObject); 
                } else {
                 if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                  if(TXNCanRedo(tmpObj,NULL))
                   TXNRedo(tmpObj);
                  }
                }                               
              break;
              
              case kRCmdEdirObject:
                if( GetTextFromWindow("Type the name of the object you want to edit", buf,
                                        255) == kRDlogProc){
                    sprintf(cmd,"%s <- edit(%s)", buf, buf);
                    consolecmd(cmd);
                }    
              break;
               
              case kHICommandAbout:
                ShowWindow(RAboutWindow);
                SelectWindow(RAboutWindow);    
              break;
              
/* Tools menu */              
              case kRCmdShowWSpace:
               consolecmd("ls()");              
              break;          		

	      case kRCmdClearWSpace:  
                userAction = YesOrNot("Clear Workspace", "All objects in the workspace will be removed. Are you sure you want to proceed?","Yes","No");
              if(userAction == kAlertStdAlertOKButton)
                consolecmd("rm(list=ls())");              
              break;          		

              case kRCmdBrowseWSpace:
                consolecmd("browseEnv(html=FALSE)");
              break;

              case kRCmdLoadWSpace:
                consolecmd("load(\".RData\")");
              break;

              case kRCmdLoadWSpaceFile:
                consolecmd("load(file.choose())");
              break;

              case kRCmdSaveWSpace:
                consolecmd("save.image()");
              break;

              case kRCmdSaveWSpaceFile:
              CopyCStringToPascal("image.rda", tempfss.name);
              if( SelectFile(&tempfss,"Choose File Where to Save Image", true, true) == noErr)
               if( FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, buf, 300) == noErr){  
                sprintf(cmd,"save.image(file=\"%s\")",buf);
                consolecmd(cmd);
                }
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
               if( DoSelectDirectory(buf,"Choose R Working Directory") == noErr)
                chdir(buf);
              break;

              case kRCmdShowWorkDir:
                consolecmd("getwd()");
              break;

              case kRCmdResetWorkDir:
                consolecmd("setwd(\"~/\")");
              break;

/* Packages menu */

              case kRCmdInstalledPkgs:
               consolecmd("package.manager()");
              break;

              case kRCmdAvailDatsets:
               consolecmd("data.manager()");
              break;

              case kRCmdInstallFromCRAN:
		  consolecmd("browse.pkgs(type=\"source\")");
              break;

              case kRCmdInstallFromBioC:
		  consolecmd("browse.pkgs(\"BIOC\",type=\"source\")");
              break;
		
             case kRCmdUpdateFromCRAN:
		 consolecmd("browse.update.pkgs(type=\"source\")");
              break;

             case kRCmdBinaryInstallFromCRAN:
	       consolecmd("browse.pkgs()");
              break;

              case kRCmdBinaryInstallFromBioC:
		  consolecmd("browse.pkgs(\"BIOC\")");
              break;
		
             case kRCmdBinaryUpdateFromCRAN:
		 consolecmd("browse.update.pkgs()");
              break;

              case kRCmdInstallFromBinary:
		  consolecmd("install.from.file(binary=TRUE)\r");
	       break;

              case kRCmdInstallFromSrc:
		  consolecmd("install.from.file()\r");
	       break;

	       /* Bioconductor */
              case kRCmdUpdateFromBioC:
		consolecmd("{library(reposTools);update.packages2(getAllDeps=TRUE)}");
              break;

	     case kRCmdBioCBundleAll:
	       consolecmd("local({source(paste(getOption('BIOC'), 'getBioC.R',sep='/'), local=TRUE); getBioC('all', destdir=.libPaths()[1])})");
	       break;
	       
	     case kRCmdBioCBundleAffy:
	       consolecmd("local({source(paste(getOption('BIOC'), 'getBioC.R',sep='/'), local=TRUE); getBioC('affy', destdir=.libPaths()[1])})");
	       break;
	      
	     case kRCmdBioCBundleCDNA:
	       consolecmd("local({source(paste(getOption('BIOC'), 'getBioC.R',sep='/'), local=TRUE); getBioC('cdna', destdir=.libPaths()[1])})");
	       break;

	      /* Local source files */
	     case kRCmdInstallFromSrcDir:
		if(DoSelectDirectory(buf,"Choose Package Directory") == noErr){
                    sprintf(cmd, "install.from.file(pkg=\"%s\")\r", buf);
                    consolecmd(cmd);
                }   
              break;

	      /* Window Menu */
	     case kRNewQuartz:
	       consolecmd("quartz()");
             break;
             
            case kRActivateDevice:
                if( GetWindowProperty(window, kRAppSignature, 'QRTZ', sizeof(int), NULL, &devnum)  == noErr)
                    selectDevice(devnum);
            break;  

/* Help Menu */
              case kRHelpStart:
                consolecmd("help.start()");
              break;
              
              case kRAquaFAQ:
                consolecmd("system(\"open http://cran.r-project.org/bin/macosx/RAqua-FAQ.html\")");
              break;

              case kRHelpOnTopic:  
                if( GetTextFromWindow("Type the name of the R command/object you want to have help", buf,
                                        255) == kRDlogProc){
                    sprintf(cmd,"help(%s)", buf);
                    consolecmd(cmd);
                }    
             break;
              
              case kRSearchHelpOn:
                if( GetTextFromWindow("Type the name of the R command/object you want to find help", buf,
                                        255) == kRDlogProc){
                    sprintf(cmd,"help.search(\"%s\")", buf);
                    consolecmd(cmd);
                }    
              break;
              
              case kRExampleRun:
                if( GetTextFromWindow("Type the name of the R command you want to run the examples described in the help pages", buf, 255) == kRDlogProc){
                    sprintf(cmd,"example(%s)", buf);
                    consolecmd(cmd);
                }    
                  break;
                        
              default:
              break;
             }
            }
        }    
 	
        HiliteMenu(0);
	return err;
}


CGContextRef CreatePDFContext( const CGRect * inMediaBox, CFURLRef url);
CGContextRef CreatePDFContext( const CGRect * inMediaBox, CFURLRef url)
{
    CGContextRef outContext = NULL;
    CGDataConsumerRef dataConsumer;
 
    dataConsumer = CGDataConsumerCreateWithURL( url );
    if( dataConsumer != NULL ){
        outContext = CGPDFContextCreate( dataConsumer, inMediaBox,NULL );
        CGDataConsumerRelease( dataConsumer );
    }
    return outContext;
}

#define kSavingConsoleWin	1
#define kSavingEditWin	2
#define kSavingQuartzWin	3

OSStatus SaveWindow(WindowRef window, Boolean ForceNewFName){
    OSStatus	err = noErr;
    char	filename[300];
    FSSpec 	tempfss;
    Str255	WinTitle;
    char 	*buf;
    int 	fsize, txtlen, i;
    TXNObject	tmpObj;
    Handle 	DataHandle;
    FILE 	*fp;
    ItemCount	changes;
    int		SavingWhat = -1;
    int		devnum;
    NewDevDesc 	*dd;
    CGDataConsumerRef ConsData;
    CFURLRef saveURL;
    FSRef fsRef;
    Rect	rect;
    CGRect	mediaBox;
    if(window == NULL)
     return(-1);
     
    if(window == ConsoleWindow){  
        SavingWhat = kSavingConsoleWin;
        if( GetWindowProperty(window,'RCON', 'fssp', sizeof(FSSpec), NULL, &tempfss) != noErr){
            ForceNewFName = true;
            CopyCStringToPascal("RConsole.txt", tempfss.name);
        }
        if(ForceNewFName) 
            err = SelectFile(&tempfss,"Choose Where to Save File", true, ForceNewFName);
        goto step2;
    } 
    
    if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
        SavingWhat = kSavingEditWin;
        if( (err = GetWindowProperty(window,'REDT', 'fssp', sizeof(FSSpec), NULL, &tempfss)) != noErr){ 
            ForceNewFName = true;
            GetWTitle( window, tempfss.name );
        } 
                   
        if(ForceNewFName) 
            err = SelectFile(&tempfss,"Choose Where to Save File", true, ForceNewFName);
        goto step2;
    } 
        

    if(  GetWindowProperty(window, kRAppSignature, 'QRTZ', sizeof(int), NULL, &devnum) == noErr){
        SavingWhat = kSavingQuartzWin;
        ForceNewFName = true;
        CopyCStringToPascal("RQuartzPlot.pdf", tempfss.name);
        if(ForceNewFName) 
            err = SelectFile(&tempfss,"Choose Where to Save PDF File", true, ForceNewFName);
        goto step2;
    } 

    err = -1;  /* Don't know what to save */

step2:
    if(err != noErr)
        return err;

    err = FSMakePath(tempfss.vRefNum, tempfss.parID, tempfss.name, filename, 300);  
 
    if(SavingWhat == kSavingConsoleWin){
        if(window == ConsoleWindow){
            txtlen = TXNDataSize(RConsoleOutObject)/2;
            err = TXNGetDataEncoded(RConsoleOutObject, 0, txtlen, &DataHandle, kTXNTextData);
        } 
        goto step3;
    }
    
    if(SavingWhat == kSavingEditWin){
        if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
            txtlen = TXNDataSize(tmpObj)/2;
            err = TXNGetDataEncoded(tmpObj, 0, txtlen, &DataHandle, kTXNTextData);
        }
        goto step3;
    }
        
    if(SavingWhat==kSavingQuartzWin){
        if( (err = FSpMakeFSRef(&tempfss, &fsRef)) != noErr)
            goto step3;
        if( (saveURL = CFURLCreateFromFSRef(NULL, &fsRef)) == NULL){
            err = -1;
            goto step3;
        }                     
        FSDeleteObject(&fsRef);
        if( (dd = ((GEDevDesc*) GetDevice(devnum))->dev) ){
            QuartzDesc *xd = (QuartzDesc *) dd-> deviceSpecific;
            mediaBox = CGRectMake(0,0,xd->windowWidth,xd->windowHeight);
            if( (xd->auxcontext  = CreatePDFContext( &mediaBox, saveURL)) == NULL){
                err = -1;
                goto step3;
            }

            xd->where = kOnFilePDF;
            CGContextBeginPage (xd->auxcontext, &mediaBox);
            CGContextTranslateCTM(xd->auxcontext, 0, xd->windowHeight);
            CGContextScaleCTM(xd->auxcontext, 1, -1);
            GEplayDisplayList((GEDevDesc*) GetDevice(devnum)); 
            CGContextEndPage(xd->auxcontext);
            if(xd->auxcontext != NULL)
             CGContextRelease(xd->auxcontext);
            xd->where = kOnScreen;
            return(noErr);                                 
        }       
    }                                                                

step3:        
    if(err != noErr)
     return err;
    
    HLock( DataHandle );
    buf = malloc(txtlen+1);
    if(buf != NULL){
        strncpy(buf,*DataHandle,txtlen);
        for(i=0;i<txtlen;i++){
            if( buf[i] == '\r') 
                buf[i] = '\n';
        } 
        buf[txtlen] = '\0';
    } else {
     err = -1;
     goto  nomem;        
    }
    if( (fp = R_fopen(R_ExpandFileName(filename), "w")) ){
        fprintf(fp, "%s", buf);
        fclose(fp);
    } else fprintf(stderr,"\n no fp");
    free(buf);

nomem:        
    HUnlock( DataHandle );
    if(DataHandle)
        DisposeHandle( DataHandle );

    if(err != noErr)
     return err;
     
    if(window == ConsoleWindow){
         err = SetWindowProperty(window,'RCON', 'fssp', sizeof(FSSpec), &tempfss);
        return err;
    }
    
    if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
        err = SetWindowProperty(window,'REDT', 'fssp', sizeof(FSSpec), &tempfss);
        SetWTitle(window,tempfss.name);
        TXNClearActionChangeCount(tmpObj, kTXNAllCountMask);
        TXNGetActionChangeCount(tmpObj,  kTXNAllCountMask, &changes);
        err = SetWindowProperty(window, 'REDT', 'chgs', sizeof(ItemCount), &changes);
        return err;
     }
           

}

void GraphicCopy(WindowPtr window)
{
    Size 		dataLength;
    SInt32 		errorCode;
    SInt16 		WinIndex;
    NewDevDesc 	*dd;
    PicHandle 	picHandle=nil;
    ScrapRef 	scrap;
 	Rect		tempRect1;
	Rect		resizeRect;
    CGrafPtr    savePort, tempPort;
    RGBColor	oldColor, newColor;
   
   
    GetPort(&savePort);
 
    GetPortBounds(GetWindowPort(window), &tempRect1);
	 	
	SetPortWindowPort(window);

	GetForeColor(&oldColor);
	GetCPixel (tempRect1.right-16,tempRect1.bottom-16,&newColor);
	
	tempPort = CreateNewPort();
    
    SetPort(tempPort);
    
		
	picHandle = OpenPicture(&tempRect1);
	
	CopyBits(GetPortBitMapForCopyBits(GetWindowPort(window)), GetPortBitMapForCopyBits(tempPort),  &tempRect1, 
	   &tempRect1, srcCopy, 0L);
	
	SetRect(&resizeRect, tempRect1.right-15, tempRect1.bottom-15, tempRect1.right,tempRect1.bottom);
	
    RGBForeColor(&newColor);
	PaintRect(&resizeRect);
	RGBForeColor(&oldColor);
 		
	ClosePicture();

    DisposePort(tempPort);
    
    if (ClearCurrentScrap() == noErr) {
	dataLength = GetHandleSize((Handle) picHandle);
	HLock((Handle)picHandle);
    errorCode = GetCurrentScrap(&scrap);
    errorCode = PutScrapFlavor (scrap, 'PICT', 0, 
    GetHandleSize((Handle) picHandle), *picHandle);
	HUnlock((Handle)picHandle);
    }
    
    KillPicture(picHandle);
    
       SetPort(savePort);

}


ControlID	RGUISep = {kRGUI, kRGUISep};
ControlID	RGUIBusy = {kRGUI, kRGUIBusy};
ControlID	RGUIText = {kRGUI, kRGUIText};

void Raqua_showarrow(void);
void Raqua_showarrow(void){
  ControlRef	RGUIControl;

  GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
  ShowControl(RGUIControl);
  GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
  ShowControl(RGUIControl);
}

void Raqua_hidearrow(void);
void Raqua_hidearrow(void){
  ControlRef	RGUIControl;

  GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
  HideControl(RGUIControl);
  GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
  HideControl(RGUIControl);
}
   
void Raqua_Busy(int which);

void Raqua_Busy(int which)
{
    if(which == 1) 
     Raqua_showarrow();
    else 
     Raqua_hidearrow();
}
   
void RescaleInOut(double prop)
{  
  Rect 	WinBounds, InRect, OutRect;
  ControlRef	RGUIControl;

  GetWindowPortBounds(ConsoleWindow, &WinBounds);
  GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
  MoveControl (RGUIControl, WinBounds.right - 20, 4);
  GetControlByID(ConsoleWindow, &RGUISep, &RGUIControl);
  SizeControl(RGUIControl, WinBounds.right, 4);
  GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
  SizeControl(RGUIControl, WinBounds.right-40, 16);
        
  SetRect(&OutRect,0,26,WinBounds.right,(int)( WinBounds.bottom*prop ));
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
        WindowRef 	EventWindow,mywin;
        EventRef	REvent;
        TXNFrameID	HlpFrameID  = 0;
        UInt32		eventClass;
        TXNObject 	HlpObj = NULL;
              HIPoint where;
            WindowDefPartCode part;
            EventRecord outEvent;
    Str255	fontname; 


        eventClass = GetEventClass(inEvent);
        GetEventParameter(inEvent, kEventParamDirectObject, typeWindowRef, NULL, sizeof(EventWindow),
                                NULL, &EventWindow);
       
        switch(eventClass){
         
        case kRCustomEventClass:
         err = noErr;
        break;
 
        case kEventClassMouse:
         if(eventKind == kEventMouseDown){
            if(ConvertEventRefToEventRecord(inEvent, &outEvent))
                if(FindWindow(outEvent.where, &mywin) == inMenuBar){
                    MenuSelect(outEvent.where);
                    err = noErr;
                }
         }
        break;
                  
         case kEventClassFont:         
         {
             switch (eventKind)
             {
                    case kEventFontPanelClosed:                
                        GetFontName(instance.fontFamily,fontname);
                        if(isConsoleFont){
                         CopyPascalStringToC(fontname,TempPrefs.ConsoleFontName);
                         TempPrefs.ConsoleFontSize = fontSize;
                        } else {
                         CopyPascalStringToC(fontname,TempPrefs.DeviceFontName);
                         TempPrefs.DevicePointSize = fontSize;
                        }
                        SetUpPrefsWindow(&TempPrefs);
                        ActivatePrefsWindow();
                    break;
         
                    case kEventFontSelection:              
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
                
/*                if( GetWindowProperty(EventWindow, kRAppSignature, 'QRTZ', sizeof(int), NULL, &devnum) == noErr)
                    if( (dd = ((GEDevDesc*) GetDevice(devnum))->dev) ){
                        dd->size(&(dd->left), &(dd->right), &(dd->bottom), &(dd->top), dd);
                        GEplayDisplayList((GEDevDesc*) GetDevice(devnum));       
                        err = noErr;
                    }
  */              
                         
             }
            break;
            
      /*
            case kEventWindowFocusRelinquish:
             SetFontInfoForSelection(kFontSelectionATSUIType,
                    0, NULL, NULL);
            break;
      */      
            case kEventWindowFocusAcquired:
                 MySetFontSelection();
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
        char 		cmd[255], filename[300], *buf;
        WindowRef 	EventWindow;
        EventRef	REvent;
        TXNObject	RHlpObj  = NULL, REdrObj = NULL;
        SInt16		FileRefNum;
        FSSpec    	fsspec;
        int		fsize, txtlen, i;
        Handle 		DataHandle;
        FILE 		*fp;
        ControlRef 	browser = NULL;
        ItemCount	changes, newchanges;
        DialogItemIndex	userAction;
        Str255		wintitle;
        char		winname[255];


	
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
/*            if( GetWindowProperty(EventWindow, kRAppSignature, 'QRTZ', sizeof(int), NULL, &devnum) == noErr){
                    sprintf(cmd,"dev.off(%d)",1+devnum);
                    consolecmd(cmd);
                    err= noErr; 
            }
  */       
           if( GetWindowProperty(EventWindow, 'RHLP', 'robj', sizeof(TXNObject), NULL, &RHlpObj) == noErr){
                    DestroyHelpWindow(EventWindow);
                    RemHelpWindow(EventWindow);
                    err= noErr; 
            }
            
            if( GetWindowProperty(EventWindow, 'RMAC', 'PKGB', sizeof(browser), NULL, &browser) == noErr){
                    CloseBrowsePkg();
                    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                    BrowsePkgFinished = true;
                    err= noErr; 

            }

            if( GetWindowProperty(EventWindow, 'RMAC', 'DMAN', sizeof(browser), NULL, &browser) == noErr){
                    CloseDataManager();
                    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                    DataManagerFinished = true;
                    err= noErr; 

            }
        
           if( GetWindowProperty(EventWindow, 'RMAC', 'HSBR', sizeof(browser), NULL, &browser) == noErr){
                    CloseHelpSearchBrowser();
                    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                    HelpSearchBrowserFinished = true;
                    err= noErr; 

            }
        
            if( GetWindowProperty(EventWindow, 'RMAC', 'PMAN', sizeof(browser), NULL, &browser) == noErr){
                    ClosePackageManager();
                    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                    PackageManagerFinished = true;
                    err= noErr; 

            }

            if( GetWindowProperty(EventWindow, 'RMAC', 'RDEY', sizeof(browser), NULL, &browser) == noErr){
                    CloseDataEntry();
                    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                    DataEntryFinished = true;
                    err= noErr; 

            }
            
            if( GetWindowProperty(EventWindow, 'REDT', 'robj', sizeof(TXNObject), NULL, &REdrObj) == noErr){
                    err = GetWindowProperty(EventWindow, 'REDT', 'chgs', sizeof(ItemCount), NULL, &changes);
                    TXNGetActionChangeCount(REdrObj,  kTXNAllCountMask, &newchanges);      
                    if( changes != newchanges ){
                     GetWTitle( EventWindow, wintitle );
                     CopyPascalStringToC(wintitle, winname);
                     sprintf(cmd, "Do you want to save changes for \"%s\"?",winname);
                     userAction = WantToSave(EventWindow, NULL, cmd);
                     if(userAction == kAlertStdAlertOKButton)
                        SaveWindow(EventWindow,false);
                    }
                    if((userAction != kAlertStdAlertCancelButton) || (changes == newchanges)){  
                        DestroyEditWindow(EventWindow);
                        RemEditWindow(EventWindow);
                        TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadWriteTag, RReadWriteData);
                        EditingFinished = true;
                     }
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

    if(strlen(cmd) < 1)
	return;

    TXNSetData (RConsoleInObject, kTXNTextData, cmd, strlen(cmd), 0, TXNDataSize(RConsoleInObject));
    SendReturnKey();
}

void SendReturnKey(void){
    EventRef	REvent;
    UInt32	RKeyCode = 36;

    CreateEvent(NULL, kEventClassKeyboard, kEventRawKeyDown, 0,kEventAttributeNone, &REvent);
    SetEventParameter(REvent, kEventParamKeyCode, typeUInt32, sizeof(RKeyCode), &RKeyCode);
    SendEventToEventTarget (REvent,GetWindowEventTarget(ConsoleWindow));
} 


OSErr DoSelectDirectory( char *buf, char *title )
{	
	NavReplyRecord		theReply;
	NavDialogOptions	dialogOptions;
	OSErr			theErr = noErr;
	NavEventUPP		eventUPP = nil; 
	SInt16 			pathLen;
        Handle 			pathName=NULL;
        char 			path[300];
	OSErr               	anErr = noErr,err;
        
	theErr = NavGetDefaultDialogOptions( &dialogOptions );

	if(title != NULL)	
            CopyCStringToPascal(title,dialogOptions.message);

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
    		}
		
		theErr = NavDisposeReply( &theReply );
	}
		
	return theErr;
}





int Raqua_Edit(char *filename)
{
    int rc = 0;
    
    TXNSetTXNObjectControls(RConsoleInObject, false, 1, RReadOnlyTag, RReadOnlyData);
    EditingFinished = false;
    
    rc = NewEditWindow(filename);
    
    while(!EditingFinished)
        Raqua_ProcessEvents();

    return 0;
}

int NewEditWindow(char *fileName)
{
    Rect	WinBounds;
    OSStatus	err = noErr;
    WindowRef 	EditWindow =  NULL;
    Str255	Title;
    FSSpec    	fsspec;
    TXNObject	REdirObject = NULL;
    TXNFrameID	EditFrameID	= 0;
    SInt16 	refNum = 0;
    TXNFrameOptions	frameOptions;
    SInt16      tempFileRefNum;
    Boolean isDirectory, WeHaveFSS = false;
    char buf[300], filenm[300], *fbuf=NULL;
    FInfo             fileInfo;
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
    TXNBackground RBGInfo;   
    TXNTypeAttributes	typeAttr;
    SInt16                  fontID;
    Str255			fontname;
    int fsize,flen;
    FILE *fp;
    ItemCount	changes; 
                          
    frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask|kTXNDrawGrowIconMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask|kTXNMonostyledTextMask;

    SetRect(&WinBounds, 400, 400, 400 +400, 400 + 400 ) ;
    
    
     if( (err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute | kWindowStandardDocumentAttributes, &WinBounds, &EditWindow) != noErr))
     goto fail;
    
    InstallStandardEventHandler( GetWindowEventTarget(EditWindow));

    if(fileName != NULL){
     err = FSPathMakeFSSpec(fileName, &fsspec, &isDirectory);
     if(err != noErr)
      goto fail;
     CopyPascalStringToC(fsspec.name,buf);
     WeHaveFSS = true;
    } else
     strcpy(buf,"New Edit Window");
    
    CopyCStringToPascal(buf,Title);
    SetWTitle(EditWindow, Title);
   
    if(WeHaveFSS){
     err = FSpGetFInfo(&fsspec,&fileInfo);
     if(err != noErr)
      goto fail;
    }
    
    if(fileInfo.fdType == NULL)
        fileInfo.fdType = kTXNTextFile;
    
    
    err = TXNNewObject(NULL, EditWindow, NULL, frameOptions, kTXNTextEditStyleFrameType,
                            fileInfo.fdType, kTXNSystemDefaultEncoding, &REdirObject,
                            &EditFrameID, 0);       
   
     
    if(err != noErr)
     goto fail;
                                           
    err = TXNSetTXNObjectControls(REdirObject, false, 1, REditTag, REditData);
    TXNSetTXNObjectControls(REdirObject,false,1,txnControlTag,txnControlData);

    
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.TabSize*CurrentPrefs.ConsoleFontSize);
    tabdata.tabValue.tabType = kTXNRightTab;
    tabdata.tabValue.filler = 0;
    
    TXNSetTXNObjectControls(REdirObject, false, 1, &tabtag, &tabdata);
         
   
  
/* setting FG colors */
   TXNSetTypeAttributes( REdirObject, 1, RInAttr, 0, kTXNEndOffset );
  
/* setting BG colors */
   RBGInfo.bgType = kTXNBackgroundTypeRGB;
   RBGInfo.bg.color = CurrentPrefs.BGInputColor;        
   TXNSetBackground(REdirObject, &RBGInfo);

    
    typeAttr.tag = kTXNQDFontSizeAttribute;
    typeAttr.size = kTXNFontSizeAttributeSize;
    typeAttr.data.dataValue = Long2Fix(CurrentPrefs.ConsoleFontSize);

    TXNSetTypeAttributes(REdirObject, 1, &typeAttr, 0, kTXNEndOffset);
        
        CopyCStringToPascal(CurrentPrefs.ConsoleFontName, fontname);
        GetFNum(fontname,&fontID);
    
        typeAttr.tag = kTXNQDFontFamilyIDAttribute;
        typeAttr.size = kTXNQDFontFamilyIDAttributeSize;
        typeAttr.data.dataValue = fontID;
    
        TXNSetTypeAttributes(REdirObject, 1, &typeAttr, 0, kTXNEndOffset);

    if(err != noErr)
     goto fail;

    err = SetWindowProperty(EditWindow,'REDT','robj', sizeof(TXNObject), &REdirObject);
    err = SetWindowProperty(EditWindow,'REDT','rfrm', sizeof(TXNFrameID), &EditFrameID);
    if(WeHaveFSS){
     err = SetWindowProperty(EditWindow,'REDT', 'fssp', sizeof(fsspec), &fsspec);
     SetWindowProxyFSSpec(EditWindow,&fsspec);
    }
    if(fileName != NULL){
     fsize = strlen(fileName);
     err = SetWindowProperty(EditWindow, 'REDT', 'fsiz', sizeof(int), &fsize);
     err = SetWindowProperty(EditWindow, 'REDT', 'fnam', fsize, fileName);
    }
    SetWindowModified(EditWindow,false);
    
    err = InstallWindowEventHandler( EditWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)EditWindow, NULL);
                    
    TXNActivate(REdirObject, EditFrameID, kScrollBarsAlwaysActive);
    if(WeHaveFSS){
     if( (fp = R_fopen(R_ExpandFileName(fileName), "r")) ){
        fseek(fp, 0L, SEEK_END);
        flen = ftell(fp);
        rewind(fp);
        fbuf = malloc(flen+1);
        if(fbuf){
         fread(fbuf, 1, flen, fp);
         fbuf[flen] = '\0';
         TXNSetData (REdirObject, kTXNTextData, fbuf, strlen(fbuf), kTXNEndOffset, kTXNEndOffset);
         free(fbuf);
         }
        fclose(fp);
     }
    }
    
    ShowWindow(EditWindow);
    BeginUpdate(EditWindow);
    TXNForceUpdate(REdirObject);
    TXNDraw(REdirObject, NULL);
    EndUpdate(EditWindow); 				 	           

    TXNSetSelection(REdirObject,1,1); 
    TXNShowSelection(REdirObject, false);
    TXNFocus(REdirObject,true);
    
    TXNGetActionChangeCount(REdirObject, kTXNAllCountMask, &changes);
    err = SetWindowProperty(EditWindow, 'REDT', 'chgs', sizeof(ItemCount), &changes);
    AddEditWindow(EditWindow);
    return 0;
    
fail:
   
   if( REdirObject )
    TXNDeleteObject(REdirObject);

   if( EditWindow )
    HideWindow(EditWindow);             
    
    return 1;
}


int Raqua_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean del, char *pager)
{
    int    	i;
    
    if (nfile <=0) return 1;
	
    for (i = 0; i < nfile; i++){
      NewHelpWindow(R_ExpandFileName(fileName[i]), title[i], WinTitle); 
    }

    return 1;
}

int Raqua_ChooseFile(int new, char *buf, int len)
{
  char 		fname[301];
  OSStatus 	err;
  FSSpec	tempfss;
 

  *buf = '\0';
   
  if( SelectFile(&tempfss,"Choose file name",false,false) == noErr){
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
    char buf[300], *fbuf=NULL;
    FInfo             fileInfo;
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
    TXNBackground RBGInfo;   
    TXNTypeAttributes	typeAttr;
        SInt16                  fontID;
        Str255			fontname;
    FILE *fp;
    int flen,i,j;
                          
    frameOptions = kTXNShowWindowMask|kTXNDoNotInstallDragProcsMask|kTXNDrawGrowIconMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask|kTXNMonostyledTextMask;

    SetRect(&WinBounds, 400, 400, 400 +400, 400 + 400 ) ;
    
    
     if( (err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute | 
     kWindowStandardDocumentAttributes, &WinBounds, &HelpWindow) != noErr))
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
   
    if(fileInfo.fdType == NULL)    
        fileInfo.fdType = kTXNTextFile;
    
    
    err = TXNNewObject(NULL, HelpWindow, NULL, frameOptions, kTXNTextEditStyleFrameType,
                            fileInfo.fdType, kTXNSystemDefaultEncoding, &RHelpObject,
                            &HelpFrameID, 0);       
    if(err != noErr)
     goto fail;
                                           
    err = TXNSetTXNObjectControls(RHelpObject, false, 3, RHelpTag, RHelpData);
    

    
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.TabSize*CurrentPrefs.ConsoleFontSize);
    tabdata.tabValue.tabType = kTXNRightTab;
    tabdata.tabValue.filler = 0;
    
    TXNSetTXNObjectControls(RHelpObject, false, 1, &tabtag, &tabdata);
    TXNSetTXNObjectControls(RHelpObject,false,1,txnControlTag,txnControlData);
     
  
/* setting FG colors */
   TXNSetTypeAttributes( RHelpObject, 1, ROutAttr, 0, kTXNEndOffset );
  
/* setting BG colors */
   RBGInfo.bgType = kTXNBackgroundTypeRGB;
   RBGInfo.bg.color = CurrentPrefs.BGOutputColor;        
   TXNSetBackground(RHelpObject, &RBGInfo);

    
    typeAttr.tag = kTXNQDFontSizeAttribute;
    typeAttr.size = kTXNFontSizeAttributeSize;
    typeAttr.data.dataValue = Long2Fix(CurrentPrefs.ConsoleFontSize);

    TXNSetTypeAttributes(RHelpObject, 1, &typeAttr, 0, kTXNEndOffset);
        
        CopyCStringToPascal(CurrentPrefs.ConsoleFontName, fontname);
        GetFNum(fontname,&fontID);
    
        typeAttr.tag = kTXNQDFontFamilyIDAttribute;
        typeAttr.size = kTXNQDFontFamilyIDAttributeSize;
        typeAttr.data.dataValue = fontID;
    
        TXNSetTypeAttributes(RHelpObject, 1, &typeAttr, 0, kTXNEndOffset);

    if(err != noErr)
     goto fail;

    err = SetWindowProperty(HelpWindow, 'RHLP', 'robj', sizeof(TXNObject), &RHelpObject);
    err = SetWindowProperty(HelpWindow, 'RHLP', 'rfrm', sizeof(TXNFrameID), &HelpFrameID);
    
    SetWindowProxyFSSpec(HelpWindow,&fsspec);
    SetWindowModified(HelpWindow,false);
  
    err = InstallWindowEventHandler( HelpWindow, NewEventHandlerUPP(DoCloseHandler), 
                                          GetEventTypeCount(RCloseWinEvent),
                                          RCloseWinEvent, (void *)HelpWindow, NULL);
                    
    TXNActivate(RHelpObject, HelpFrameID, kScrollBarsAlwaysActive);
    
    if( (fp = R_fopen(R_ExpandFileName(fileName), "r")) ){
        fseek(fp, 0L, SEEK_END);
        flen = ftell(fp);
        rewind(fp);
        fbuf = malloc(flen+1);
        if(fbuf){
         fread(fbuf, 1, flen, fp);
         fbuf[flen] = '\0';
         for(i=0;i<flen-1;++i)
          if((fbuf[i] == '_') && (fbuf[i+1] == '\b')){
           for(j=i+2;j<flen;j++)
            fbuf[j-2] = fbuf[j]; 
            flen = flen -2;
            }
         fbuf[flen] = '\0';   
         TXNSetData (RHelpObject, kTXNTextData, fbuf, strlen(fbuf), kTXNEndOffset, kTXNEndOffset);
         free(fbuf);
         }
        fclose(fp);
    }
 
   
    ShowWindow(HelpWindow);
    BeginUpdate(HelpWindow);
    TXNForceUpdate(RHelpObject);
    TXNDraw(RHelpObject, NULL);
    EndUpdate(HelpWindow); 				 	           
    TXNSetSelection(RHelpObject,1,1); 
    TXNShowSelection(RHelpObject, false);
    TXNFocus(RHelpObject,true);
    AddHelpWindow(HelpWindow);
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
	if(spec == NULL) 
         return(paramErr);
         
	
	/* convert the POSIX path to an FSRef */
	if( (result = FSPathMakeRef(path, &ref, isDirectory)) != noErr)
         return(result);
         
	/* and then convert the FSRef to an FSSpec */
	result = FSGetCatalogInfo(&ref, kFSCatInfoNone, NULL, NULL, spec, NULL);

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
	if(path == NULL) 
         return(paramErr);
	
	/* convert the inputs to an FSRef */
	if( (result = FSMakeFSRef(volRefNum, dirID, name, &ref)) != noErr)
         return(result);
         
        /* and then convert the FSRef to a path */
	result = FSRefMakePath(&ref, path, maxPathSize);

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
	if(ref == NULL)
         return(paramErr);
	
	pb.ioVRefNum = volRefNum;
	pb.ioDirID = dirID;
	pb.ioNamePtr = (StringPtr)name;
	pb.newRef = ref;
	result = PBMakeFSRefSync(&pb);

	return ( result );
}


OSStatus SelectFile(FSSpec *outFSSpec,  char *Title, Boolean saveit, Boolean HaveFName)
{
    NavDialogOptions    dialogOptions;
    NavEventUPP         eventProc = nil; 
    NavObjectFilterUPP  filterProc = nil;
    OSErr               anErr = noErr;
    char 		fname[300], outname[300];
    Boolean		ItExists = false;
    
    /*  Specify default options for dialog box */
    anErr = NavGetDefaultDialogOptions(&dialogOptions);

    CopyCStringToPascal(Title,dialogOptions.message);

     
    if( HaveFName ){
     CopyPascalStringToC(outFSSpec->name, fname);
     CopyCStringToPascal(fname,dialogOptions.savedFileName);
    }
     
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
            if(saveit)
             anErr = NavPutFile(nil, &reply, &dialogOptions, nil, 
                                nil, 'ttxt', nil);
            else
             anErr = NavGetFile(nil, &reply, &dialogOptions, nil, nil,
                                nil, deftypeList, nil);     
                                                
            if (anErr == noErr && reply.validRecord)
            {
                /*  Deal with multiple file selection */
                long    count;
                
                anErr = AECountItems(&(reply.selection), &count);
                           
                count = 1L; /* we only select one file */
                /* Set up index for file list */
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

            
    if( FSMakePath(outFSSpec->vRefNum, outFSSpec->parID, outFSSpec->name, outname, 300) != noErr)
     ItExists = false;
    else
     ItExists = true;
     
    if(saveit && !ItExists){
        SInt16	dataForkRefNum = -1 ;
        FSpCreateResFile(outFSSpec, 'ttxt', 'TEXT', smSystemScript);
        if( (anErr = ResError()) != noErr) 
         goto cleanup;
        if( (anErr = FSpOpenDF(outFSSpec, fsRdWrPerm, &dataForkRefNum)) != noErr)
         goto cleanup;
        if ( dataForkRefNum != -1 )
         FSClose ( dataForkRefNum ) ;
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
    Raqua_write_history(CHAR(STRING_ELT(sfile, 0)));
    return R_NilValue;
}

SEXP Raqua_loadhistory(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile;

    checkArity(op, args);
    sfile = CAR(args);
    if (!isString(sfile) || LENGTH(sfile) < 1)
	errorcall(call, "invalid file argument");
    Raqua_read_history(CHAR(STRING_ELT(sfile, 0)));
    return R_NilValue;
}

void Raqua_write_history(char *file)
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
 Raqua_read_history: load history command from a
 specified file. Adapted from gl_loadhistory
 for Windows. It can read history files of
 Windowds porting.
**********************************************/
void Raqua_read_history(char *file)
{
    FILE *fp;
    int i,buflen,j;
    char buf[1002];

    if (!file || *file==NULL) return;
    fp = R_fopen(file, "r");
    if (!fp) 
 	return;
    

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
 

 
void Raqua_GetQuartzParameters(double *width, double *height, double *ps, char *family, Rboolean *antialias, Rboolean *autorefresh){

    if( CurrentPrefs.OverrideRDefaults == 0)
     return; /* we don't touch user's parameters */
     
    *width = CurrentPrefs.DeviceWidth;
    *height = CurrentPrefs.DeviceHeight;
    *ps = (double)CurrentPrefs.DevicePointSize;
    strcpy(family, CurrentPrefs.DeviceFontName);
    *antialias = CurrentPrefs.AntiAlias;
    *autorefresh = CurrentPrefs.AutoRefresh;    
}


void Raqua_CleanUp(SA_TYPE saveact, int status, int runLast)
{
  unsigned char buf[1024];
  char * tmpdir;
  if(saveact == SA_DEFAULT) /* The normal case apart from R_Suicide */
    saveact = SaveAction;

    if(saveact == SA_SAVEASK) {
	if(R_Interactive) {
	    switch (WantToSave(ConsoleWindow,"Closing R Session","Save workspace image?")) {
	    case kAlertStdAlertOKButton:
		saveact = SA_SAVE;
		break;
	    case kAlertStdAlertOtherButton:
		saveact = SA_NOSAVE;
		break;
	    case kAlertStdAlertCancelButton:
		jump_to_toplevel();
		break;

	    }
	} else saveact = SaveAction;
    }

    switch (saveact) {
    case SA_SAVE:
	if(runLast) R_dot_Last();
	if(R_DirtyImage) R_SaveGlobalEnv();
            Raqua_write_history(R_HistoryFile);
	break;
    case SA_NOSAVE:
	if(runLast) R_dot_Last();
	break;
    case SA_SUICIDE:
    default:
	break;
    }
    R_RunExitFinalizers();

    CloseAllHelpWindows();
    CloseAllEditWindows();
    KillAllDevices();
    
    PrintWarnings();
    CloseRAquaConsole();
    if((tmpdir = getenv("R_SESSION_TMPDIR"))) {
	snprintf((char *)buf, 1024, "rm -rf %s", tmpdir);
	R_system((char *)buf);
    }
    exit(status);
}

void DestroyHelpWindow(WindowRef window){
    TXNObject tmpObj;

    if(window == NULL) return;
    
    if( GetWindowProperty(window, 'RHLP', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        TXNDeleteObject(tmpObj);
    DisposeWindow(window);
}

void DestroyEditWindow(WindowRef window){
    TXNObject tmpObj;

    if(window == NULL) return;
    
    if( GetWindowProperty(window, 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr)
        TXNDeleteObject(tmpObj);
    DisposeWindow(window);
}

void CloseAllHelpWindows(void){
    int i;
    
    for(i=0; i<NumOfHelpWindows; i++){
        if(HelpWindowsList[i] != NULL)
         DestroyHelpWindow(HelpWindowsList[i]);
        HelpWindowsList[i] = NULL;
    }
    NumOfHelpWindows = 0;     
}

void CloseAllEditWindows(void){
    int 		i;
    TXNObject  		tmpObj;
    ItemCount		changes, newchanges;
    NavUserAction	userAction;
    char 		msg[1024], winname[255];
    Str255		wintitle;
    
    for(i=0; i<NumOfEditWindows; i++){
        if(EditWindowsList[i] != NULL){
         if( GetWindowProperty(EditWindowsList[i], 'REDT', 'robj', sizeof(TXNObject), NULL, &tmpObj) == noErr){
                    GetWindowProperty(EditWindowsList[i], 'REDT', 'chgs', sizeof(ItemCount), NULL, &changes);
                    TXNGetActionChangeCount(tmpObj,kTXNAllCountMask,&newchanges);
                    if( changes != newchanges ){
                     ShowWindow( EditWindowsList[i] );
                     GetWTitle( EditWindowsList[i], wintitle );
                     CopyPascalStringToC(wintitle, winname);
                     sprintf(msg, "Do you want to save changes for \"%s\"?",winname);
                     userAction = YesOrNot(NULL, msg,"Save","Don't Save");
                     if(userAction == kAlertStdAlertOKButton)
                        SaveWindow(EditWindowsList[i],false);
                    }
         }
         DestroyEditWindow(EditWindowsList[i]);
        }
        EditWindowsList[i] = NULL;
    }
    NumOfEditWindows = 0;     


}

    
void Raqua_ShowMessage(char *msg)
{
	AlertStdAlertParamRec	alertParamRec;
	short itemHit=0;
        Str255	title;
	
	alertParamRec.movable = false;				// Make alert movable modal
	alertParamRec.helpButton = false;			// Is there a help button?
	alertParamRec.filterProc = NULL;			// event filter
	alertParamRec.defaultText = NULL;			// Text for button in OK position
	alertParamRec.cancelText = NULL;			// Text for button in cancel position
	alertParamRec.otherText = NULL;				// Text for button in left position
	alertParamRec.defaultButton = 1;			// Which button behaves as the default
	alertParamRec.cancelButton = 0;				// Which one behaves as cancel (can be 0)
	alertParamRec.position = kWindowAlertPositionParentWindow;	
        
	SysBeep( 5 );
        CopyCStringToPascal(msg,title);
	StandardAlert( kAlertStopAlert, title, NULL, &alertParamRec, &itemHit );
}




void Raqua_Suicide(char *s)
{
    char  pp[1024];

    snprintf(pp, 1024, "Fatal error: %s\n", s);
    Raqua_ShowMessage(pp);
    Raqua_CleanUp(SA_SUICIDE, 2, 0);
}


pascal OSErr  HandleDoCommandLine (AppleEvent *theAppleEvent, AppleEvent* reply, long handlerRefCon)
{
	OSErr		err = 0;
	DescType	returnedType;
	Size		actualSize;

     
	if ((err = AEGetParamPtr(theAppleEvent, keyDirectObject, typeChar, &returnedType,
                                            CMDString, CMDLineSize, &actualSize)) != noErr)
		return err;

	/* check for missing parameters   */

        if(actualSize <= CMDLineSize)
            CMDString[actualSize] = '\0';	
        else
          CMDString[CMDLineSize] = '\0';    	/* Terminate the C string    */
    
        consolecmd(CMDString);
        
        return noErr;
}


void DoNothing(void){
 return;
}
void	Raqua_ProcessEvents(void)
{
    EventRef theEvent;
    EventRecord	outEvent;
    EventTargetRef theTarget = GetEventDispatcherTarget();
    bool	conv = false;

/*    if(WeHaveConsole)
     if(otherPolledEventHandler)
      otherPolledEventHandler();
*/

    

    if(CheckEventQueueForUserCancel())
       onintr();

    
    if(ReceiveNextEvent(0, NULL, kEventDurationForever  ,true,&theEvent)== noErr){
        conv = ConvertEventRefToEventRecord(theEvent, &outEvent);
    
        if(conv && (outEvent.what == kHighLevelEvent))
            AEProcessAppleEvent(&outEvent);
         
        SendEventToEventTarget (theEvent, theTarget);
        ReleaseEvent(theEvent);
            
    } 
//    fprintf(stderr,"\n process events");
}

static	pascal	void	ReadStdoutTimer( EventLoopTimerRef inTimer, void *inUserData )
{
    int len;
    char *tmpbuf;

    if(CurrentPrefs.GrabStdout){
     if( RAquaStdoutBack != NULL){
        fseek(RAquaStdoutBack, 0L, SEEK_END);
        len = ftell(RAquaStdoutBack);
        if(len>1){
            rewind(RAquaStdoutBack);
            if( (tmpbuf = malloc(len+2)) != NULL){
                fread(tmpbuf+1, 1, len, RAquaStdoutBack);
                tmpbuf[0] = '\n';
                tmpbuf[len+1] = '\0';
                Raqua_WriteConsole(tmpbuf,len+1);
                Aqua_FlushBuffer();
                free(tmpbuf);
                CloseStdoutPipe();
                OpenStdoutPipe();
                SendReturnKey();
            }
        }
     }
    }
    
    if(CurrentPrefs.GrabStderr){
     if( RAquaStderrBack != NULL){
        fseek(RAquaStderrBack, 0L, SEEK_END);
        len = ftell(RAquaStderrBack);
        if(len>1){
            rewind(RAquaStderrBack);
            if( (tmpbuf = malloc(len+2)) != NULL){
                fread(tmpbuf+1, 1, len, RAquaStderrBack);
                tmpbuf[0] = '\n';
                tmpbuf[len+1] = '\0';
                Raqua_WriteConsole(tmpbuf,len+1);
                Aqua_FlushBuffer();
                free(tmpbuf);
                CloseStderrPipe();
                OpenStderrPipe();
                SendReturnKey();
            }
        }
     }
    }
}   

#endif /* HAVE_AQUA */

#endif /* __AQUA_CONSOLE__ */
