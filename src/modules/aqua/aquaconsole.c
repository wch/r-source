/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka
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
#include <sys/fcntl.h>
#include "Raqua.h"

/* Cocoa bundle stuff */
static OSStatus appCommandHandler(EventHandlerCallRef inCallRef, EventRef inEvent, void* userData);
EventTypeSpec cmdEvent = {kEventClassCommand, kEventCommandProcess};

/* character coding tables for true 8-bit chars */
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
#define	kRCmdEditObject	'edbj'
/* Items for the Tools menu */
#define kRCmdFileShow		'fshw'
#define kRCmdEditFile		'edtf'

#define kRCmdRappUpdates	'nraq'

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
#define kRMacOSXFAQ		'rfaq'
#define kRAquaWhatsNew		'rwsn'

#define kRDlog	  'RDLG'
#define	kRDlogMsg  1000
#define	kRDlogText 1001
#define	kRDlogProc 1002
#define	kRDlogCanc 1003

#define kRGUI	'RGUI'
#define kRGUIBusy  1000
#define kRGUISep   1001
#define kRGUIText   1002
#define kRWorkingDirText 1045

#define kRCustomEventClass 'revt'
#define kRWakeUpPlease 	'wake'

#define kEventClassRead 'rrdc'
#define kEventRead 'rrde'
#define kEventParamRead 'rrdp'


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
bool		WeHaveCocoa = false;

Boolean HaveContent = false;
Boolean HaveBigBuffer = false;

/* this input buffer is filled by the userInput function (Cocoa callback) and processed in ReadConsole. This effectively limits the size of an input string to 32k. We should make this more flexible at some point FIXME! */
#define inputBufferSize 32768
static char inputBuffer[inputBufferSize];

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
pascal OSErr HandleOpenDocument( const AppleEvent *ae, AppleEvent *reply, SInt32 refCon );

/*
 -------------------------------------------------------- Cocoa interface functions ------
 */

/* feature constants - return value of cocoaInitializeBundle (if positive) is a bit mask of those constants, which define the functionality provided by the bundle */
#define cocoa_basic 0x0001 /* basic functionality */
#define cocoa_loop  0x0002 /* event loop functionality */
#define cocoa_menu  0x0004 /* if set, Cocoa provides its own menu, Carbon should skip menu creation */

int cocoaFeatures=0; /* defines the capabilities of the currently loaded bundle */

CFBundleRef cocoaBundleRef = NULL; /* reference to the currently loaded bundle */

/* Cocoa functions - if the function is optional you (in aquaconsole) must check for its presence before calling! */

/*===feature group: cocoa_basic (mandatory group - all Cocoa bundles must implement this group) */
/* - initialize Cocoa; takes userInput callback function as argument; returns bitmask for cocoaFeatures */
int (*cocoaInitializeBundle)(int (*callBack)(const char *));
void (*cocoaDeInitializeBundle)(void);

/* the following functions return 0 on success and !=0 on failure */
/* - makes Cocoa window active; the window number should be 0 since we have only the main window atm. */
int (*cocoaSelectWindow)(int);
/* - writes string to the console (usually R output) */
int (*cocoaWriteConsole)(CFStringRef);
/* - writes the prompt to the console */
int (*cocoaWritePrompt)(CFStringRef);
/* - writes echo of the input in the console; if not provided, cocoaWriteConsole is used instead */
int (*cocoaWriteUserInput)(CFStringRef); /* optional - use WriteConsole if this one is NULL */
/* - tells Cocoa whether R is busy (parameter=1) or idle (parameter=0) */
int (*cocoaRisBusy)(int); /* optional */

/*===feature group: cocoa_loop */
/* - if defined, this function is called in the internal R event loop; parameter is always 0 atm */
int (*cocoaProcessEvents)(int);

/*===feature group: cocoa_menu */
/* - this function is called after initialization such as that Cocoa may build the application menu */
int (*cocoaSetupMenu)(int); /* optional - menu may already have been created in initializeBundle */

/*
 -------------------------------------------------------- END OF Cocoa IF ------
 */

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
void Raqua_WritePrompt(char *prompt);

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
void RSetConsoleWidth(void);

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

static const EventTypeSpec	inputSpec[] =
{
    { kEventClassControl, kEventControlHit } /* , {kEventClassWindow, kEventWindowClose} */
};

static pascal OSErr QuitAppleEventHandler (const AppleEvent *appleEvt,
										   AppleEvent* reply, UInt32 refcon); 
static pascal OSStatus KeybHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
static pascal OSStatus ReadHandler( EventHandlerCallRef inCallRef, EventRef inEvent, void *inUserData );
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

static const EventTypeSpec RReadEvents[] = {{ kEventClassRead, kEventRead }};


static const EventTypeSpec	RCmdEvents[] =
{
	{ kEventClassCommand, kEventCommandProcess },
	{ kEventClassCommand, kEventCommandUpdateStatus }
};

static const EventTypeSpec	RGlobalWinEvents[] =
{
	{ kEventClassWindow, kEventWindowBoundsChanged } ,
	{ kEventClassWindow, kEventWindowShown} ,
	{ kEventClassWindow,   kEventWindowZoomed   },
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


void Raqua_FocusOnConsole(void);
void Raqua_FocusOnConsole(void){
	if(ConsoleWindow)
		BringToFront(ConsoleWindow);
	
}


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
MenuRef myHelpMenu = NULL; /* This is the ref to the help menu in the nib file */

int InputDialogAns = kRDlogCanc;      
extern void SetDefaultPrefs(void);
extern void SetUpPrefsWindow(RAquaPrefsPointer Settings);

extern void GetDialogPrefs(void);
extern	void GetRPrefs(void);
extern void SaveRPrefs(void);

void Raqua_GetQuartzParameters(double *width, double *height, double *ps, char *family, Rboolean *antialias, Rboolean *autorefresh, int *quartzpos);

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

static	pascal	void	OtherEventLoops( EventLoopTimerRef inTimer, void *inUserData );
static	pascal	void	ReadStdoutTimer( EventLoopTimerRef inTimer, void *inUserData );
static	pascal	void	FlushConsoleTimer( EventLoopTimerRef inTimer, void *inUserData );
static	pascal	void	WorkingDirTimer( EventLoopTimerRef inTimer, void *inUserData );

EventLoopTimerRef	Inst_OtherEventLoops;
EventLoopTimerRef	Inst_ReadStdoutTimer;
EventLoopTimerRef	Inst_FlushConsoleTimer;
EventLoopTimerRef	Inst_WorkingDirTimer;

extern  void	SaveConsolePosToPrefs(void);
extern 	Rect	ConsoleWindowBounds;

static void loadPrivateFrameworkBundle(CFStringRef framework, CFBundleRef *bundlePtr);
static int userInput(const char *text);

void SetUpRAquaMenu(void);
OSStatus InstallAppHandlers(void);
OSStatus SetUpGUI(void);
CFBundleRef RBundle = NULL;
CFURLRef    RbundleURL = NULL;

OSStatus SetUpGUI(void){
    IBNibRef 	nibRef = NULL;
    OSErr	err = noErr;
	
	RBundle = CFBundleGetMainBundle();
	if(RBundle == NULL)
		goto guifailure;
	
    if( (err = CreateNibReferenceWithCFBundle (RBundle, CFSTR("main"), &nibRef)) != noErr)
		goto guifailure;
	
    /* we need at least one Carbon window before loading Cocoa, otherwise Carbon stuff itn's set up properly */
    if( (err = CreateWindowFromNib(nibRef,CFSTR("MainWindow"),&ConsoleWindow)) != noErr)
        goto guifailure;
	
    loadPrivateFrameworkBundle(CFSTR("RGUI.bundle"), &cocoaBundleRef);
    
    /* if the bundle is loaded and at least basic features are provided, we can use the bundle instead of the Carbon window */
    if (cocoaBundleRef && ((cocoaFeatures&cocoa_basic)>0))
        WeHaveCocoa=true; 
    
    /*   if( (err = CreateNibReference(CFSTR("main"), &nibRef)) != noErr) goto guifailure; */
    fprintf(stderr,"\nSetUpGUI:cocomenu=%x",cocoaFeatures&cocoa_menu);
	fprintf(stderr,"\nSetUpGUI:cocobasic=%x",cocoaFeatures&cocoa_basic);
	fprintf(stderr,"\nSetUpGUI:cocoloop=%x",cocoaFeatures&cocoa_loop);
	
    if ((cocoaFeatures&cocoa_menu)==0) { /* if Cocoa bundle doesn't provide the menu, we create it */
        if( (err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"))) != noErr) goto guifailure;
    } else {
        if (cocoaSetupMenu) cocoaSetupMenu(0);
    }

if ((cocoaFeatures&cocoa_menu)==0) { /* if Cocoa bundle doesn't provide the menu, we create it */
if( (err = CreateMenuFromNib(nibRef, CFSTR("MyHelpMenu"), &myHelpMenu)) != noErr) goto guifailure;
}

if (WeHaveCocoa && cocoaSelectWindow) cocoaSelectWindow(0);

if( (err = CreateWindowFromNib(nibRef,CFSTR("AboutWindow"),&RAboutWindow)) != noErr)
goto guifailure;

RepositionWindow(RAboutWindow,  NULL, kWindowCenterOnMainScreen);

if( (err = CreateWindowFromNib(nibRef,CFSTR("PrefsWindow"),&RPrefsWindow)) != noErr)
goto guifailure;
RepositionWindow(RPrefsWindow,  NULL, kWindowCenterOnMainScreen);

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
	
    frameOptions = /* kTXNShowWindowMask|*/ kTXNDoNotInstallDragProcsMask|kTXNMonostyledTextMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;
	
	
    err = TXNNewObject(NULL, ConsoleWindow, &OutFrame, frameOptions, kTXNTextEditStyleFrameType,
					   kTXNTextensionFile, kTXNSystemDefaultEncoding, &RConsoleOutObject,
					   &OutframeID, 0);
    frameOptions  = kTXNMonostyledTextMask /*|kTXNShowWindowMask*/ | kTXNWantHScrollBarMask;
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
	
	
	
    frameOptions = /*kTXNShowWindowMask|*/ kTXNDoNotInstallDragProcsMask|kTXNMonostyledTextMask; 
    frameOptions |= kTXNWantHScrollBarMask | kTXNWantVScrollBarMask | kTXNReadOnlyMask;
	
	
    if(err != noErr)
		return(err);
	
    WeHaveConsole = true;
    if (!WeHaveCocoa) 
		ShowWindow(ConsoleWindow);
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
	
    if (!WeHaveCocoa) {
		err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(KeybHandler), 
                                         GetEventTypeCount(KeybEvents),
                                         KeybEvents, (void *)ConsoleWindow, NULL);
    }
    err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(ReadHandler),
                                     GetEventTypeCount(RReadEvents),
                                     RReadEvents, (void *)ConsoleWindow, NULL);
	
    err = InstallWindowEventHandler( ConsoleWindow, NewEventHandlerUPP(DoCloseHandler), 
									 GetEventTypeCount(RCloseWinEvent),
									 RCloseWinEvent, (void *)ConsoleWindow, NULL);
    err = InstallStandardEventHandler(GetWindowEventTarget(ConsoleWindow));        
	
    TXNFocus(RConsoleOutObject,true);
	
    InstallApplicationEventHandler(NewEventHandlerUPP(appCommandHandler), 1, &cmdEvent, 0, NULL);
	
    return err;
	
}

void	Raqua_ProcessEvents(void);





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

void ChangeStartupDir(void);

void ForceConsoleRefresh(void);
void ForceConsoleRefresh(void){
    EventRef	RConsoleEvent;
	if(ConsoleWindow){
		CreateEvent(NULL, kEventClassWindow, kEventWindowUpdate, 0, kEventAttributeNone, &RConsoleEvent);
		SetEventParameter(RConsoleEvent, kEventParamDirectObject, typeWindowRef, sizeof(typeWindowRef), ConsoleWindow);
		SendEventToEventTarget (RConsoleEvent,GetWindowEventTarget(ConsoleWindow));
		ReleaseEvent(RConsoleEvent);
	}
}

void InitAquaIO(void){
    StdoutFName = R_tmpnam("RStdout", R_TempDir);
    StderrFName = R_tmpnam("RStderr", R_TempDir);
}

void Raqua_StartConsole(Rboolean OpenConsole)
{
    IBNibRef 	nibRef = NULL;
    OSErr	err = noErr, result;
    
    char	buf[300];
    FSRef 	ref;
	
	
    if(OpenConsole){ 
		if(SetUpGUI() != noErr)
			goto noconsole;
		
		InitAboutWindow();
		
		GetRPrefs();
		RSetPipes();
		
		RepositionWindow (ConsoleWindow,  NULL, kWindowCascadeOnMainScreen);
		if(CurrentPrefs.SaveConsolePos == 1){
			if( ConsoleWindowBounds.bottom != 0)
				SetWindowBounds(ConsoleWindow, kWindowStructureRgn, &ConsoleWindowBounds);
		}
		//     InitCursor();
		//    SetThemeCursor(kThemeIBeamCursor);
		
		if (TXNVersionInformation == (void*)kUnresolvedCFragSymbolAddress)
			goto noconsole;
		
		if( InitMLTE() != noErr )
			goto noconsole;
		
		if( SetUPConsole() != noErr)
			goto noconsole;
		
		if(err == noErr)
			InstallPrefsHandlers();
    }
	
    InstallAppHandlers();
    
    if(OpenConsole){
		if(ConsoleWindow!= NULL){
			if (!WeHaveCocoa) SelectWindow(ConsoleWindow);
			RSetTab();
			RSetFontSize();
			RSetFont();
			RSetColors();
			if ((cocoaFeatures&cocoa_menu)==0) SetUpRAquaMenu(); /* if Cocoa provides no menu, we should */
		}   
		
		ChangeStartupDir();
		
		if (R_RestoreHistory)
			Raqua_read_history(R_HistoryFile);
    }   
	
	
    
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationMillisecond*10, NewEventLoopTimerUPP(OtherEventLoops), NULL, &Inst_OtherEventLoops);
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationSecond /5, NewEventLoopTimerUPP(ReadStdoutTimer), NULL, &Inst_ReadStdoutTimer);
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationMillisecond*50, NewEventLoopTimerUPP(FlushConsoleTimer), NULL, &Inst_FlushConsoleTimer);
    InstallEventLoopTimer(GetMainEventLoop(), 0, kEventDurationSecond*2, NewEventLoopTimerUPP(WorkingDirTimer), NULL, &Inst_WorkingDirTimer);
    
    RAqua2Front();
	
    if(ConsoleWindow != NULL)
        if (!WeHaveCocoa) {
			SelectWindow(ConsoleWindow);
			ForceConsoleRefresh();
		}
			InitCursor();
	
    if (WeHaveCocoa) HideWindow(ConsoleWindow);
    
    return;
	
noconsole:
		CloseRAquaConsole();
}

ControlID	RGUISep = {kRGUI, kRGUISep};
ControlID	RGUIBusy = {kRGUI, kRGUIBusy};
ControlID	RGUIText = {kRGUI, kRGUIText};
ControlID   WorkingDirID = {kRGUI, kRWorkingDirText};

void ShowWorkingDir(void);
void ShowWorkingDir(void){
    CFStringRef		text;
    char			buf[301];
    ControlRef		WorkingDirControl;
	ControlFontStyleRec	controlStyle;
	
	getcwd(buf, 300);
	GetControlByID(ConsoleWindow, &WorkingDirID, &WorkingDirControl);
	SetControlData(WorkingDirControl, kControlEntireControl, kControlStaticTextTextTag, strlen(buf), buf);
	controlStyle.flags = kControlUseJustMask;
	controlStyle.just = teFlushLeft;
	DrawOneControl(WorkingDirControl);     
}

/*  This sets the initial working directory according to the
Preferences settings. If the selected directory does not
exists, the startup working dir is set to the the user's
home.
*/	
void ChangeStartupDir(void){
	
	if(chdir(R_ExpandFileName(CurrentPrefs.WorkingDirectory)) < 0) {
		fprintf(stderr,"\nR: Cannot set working directory according to Preferences");
		fprintf(stderr,"\nR: Working directory is now user's home");
		R_ShowMessage("Startup working directory set as user's home.\nPlease, change it in the Preferences.");
	    chdir(R_ExpandFileName("~/"));
	}
    ShowWorkingDir();
}

OSStatus InstallAppHandlers(void){
  	 OSStatus err = noErr;
	
	InstallStandardEventHandler(GetApplicationEventTarget());
	if((cocoaFeatures&cocoa_menu)==0){
		err = InstallApplicationEventHandler( NewEventHandlerUPP(RCmdHandler), GetEventTypeCount(RCmdEvents),
										  RCmdEvents, 0, NULL);
	}
	
	err = InstallApplicationEventHandler( NewEventHandlerUPP(RWinHandler), GetEventTypeCount(RGlobalWinEvents),
										  RGlobalWinEvents, 0, NULL);
	
 	err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, 
								NewAEEventHandlerUPP( HandleOpenDocument ), 0, false );
	
	err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, 
								NewAEEventHandlerUPP((AEEventHandlerProcPtr)QuitAppleEventHandler), 
								0, false);
	
	err = AEInstallEventHandler(kCMDEventClass, kCMDEvent,
                                NewAEEventHandlerUPP( (AEEventHandlerProcPtr)HandleDoCommandLine ),
                                0, false);
	
	err = InstallWindowEventHandler(RAboutWindow, NewEventHandlerUPP(RAboutWinHandler), 1, &aboutSpec, 
									(void *)RAboutWindow, NULL);
	
	err = InstallWindowEventHandler(RInputDialog, NewEventHandlerUPP(RInputDialogHandler), 1, inputSpec, 
									(void *)RInputDialog, NULL);
	
	err = InstallControlEventHandler( GrabCRef(RInputDialog,kRDlog,kRDlogProc),  
									  RInputDialogHandler , 1,  inputSpec, RInputDialog, NULL );
	
	err = InstallControlEventHandler( GrabCRef(RInputDialog,kRDlog,kRDlogCanc),  
									  RInputDialogHandler , 1,  inputSpec,  RInputDialog, NULL );        
	
	return err;                        
}

void SetUpRAquaMenu(void){
    Str255	menuStr;
    HMGetHelpMenu(&HelpMenu,NULL);
	int numItems, i;
	HMHelpContentRec	theContent;
	
    if (HelpMenu != nil) {
	    if(myHelpMenu){
			numItems  = CountMenuItems( myHelpMenu );
			CopyMenuItems (myHelpMenu, 1, numItems , HelpMenu, 0);
			for(i=1; i<=numItems; i++){
				HMGetMenuItemHelpContent(myHelpMenu, i, &theContent);
				HMSetMenuItemHelpContent(HelpMenu, i, &theContent);
			}
		}
	}
	EnableMenuCommand(NULL, kHICommandPreferences);
	
	DrawMenuBar();
	
}




static	pascal	void	WorkingDirTimer( EventLoopTimerRef inTimer, void *inUserData )
{
	ShowWorkingDir();
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
	GetWindowBounds(ConsoleWindow, kWindowStructureRgn, &ConsoleWindowBounds);
    SaveConsolePosToPrefs();	  
	DisposeWindow(ConsoleWindow);
    
	TXNTerminateTextension();
	
	RemoveEventLoopTimer(Inst_OtherEventLoops);
	RemoveEventLoopTimer(Inst_ReadStdoutTimer);
	RemoveEventLoopTimer(Inst_FlushConsoleTimer);
	RemoveEventLoopTimer(Inst_WorkingDirTimer);
	
	CloseAquaIO();
	if(RbundleURL){
		CFRelease(RbundleURL);
		RbundleURL = NULL;
	}
	if(RBundle){
		CFRelease(RBundle); 
		RBundle = NULL;
	}
}

void OpenStdoutPipe(void){
    RAquaStdout = freopen(StdoutFName, "w", stdout);
    RAquaStdoutBack = fopen(StdoutFName, "r");
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
	if(RAquaStderr) 
		fclose(RAquaStderr);
	
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
#define AQUA_MAXBUFLEN 32766
static char outputbuffer[AQUA_MAXBUFLEN+2];
static int  end_of_buffer=0;
static int  WeAreBuffering=1;

/* Writes the prompt in the console. It calls implicitly FlushConsole to make sure the prompt is placed in-sync. */
void Raqua_WritePrompt(char *prompt) {
    Raqua_FlushConsole();
    if (WeHaveCocoa) {
        if (cocoaWritePrompt) {
            CFStringRef text = CFStringCreateWithCString(NULL, prompt, kCFStringEncodingMacRoman);
            if(text){
				cocoaWritePrompt(text);
            	CFRelease(text);
				text = NULL;
			}
        }
    } else {
        Aqua_RWrite(prompt);
    }
}

/* This function is used to echo user input into the console. Currently it uses Raqua_WriteConsole if Cocoa's
writeUserInput is not available. It may implicitly flush the console before sending the user input to make 
sure it arrives in-sync. 
*/
void Raqua_WriteUserInput(char *str) {
    if (WeHaveCocoa && cocoaWriteUserInput) {
        Raqua_FlushConsole();
        CFStringRef text = CFStringCreateWithCString(NULL, str, kCFStringEncodingMacRoman);
        if(text){
			cocoaWriteUserInput(text);
        	CFRelease(text);
			text = NULL;
		}
    } else Raqua_WriteConsole(str, strlen(str));
}

/* writes specified string as-is (no recoding) to console, using events or Cocoa bundle API.
FIXME! curerntly the event system has a fixed buffer of 32k, therefore any larger data are truncated. 
This implies that the string sent via WriteEvent should not exceed 32k. Currently that's not an issue,
since the console buffer is only 32k big, but in the future we should split the string into multiple 
events if it exceeds the limit ... This restriction doesn't apply to cocoaWriteConsole, though. 
*/
void Raqua_WriteEvent(char *str, int len)
{
    if (WeHaveCocoa) {
        if (cocoaWriteConsole) {
            CFStringRef text = CFStringCreateWithCString(NULL, str, kCFStringEncodingMacRoman);
            if(text){
				cocoaWriteConsole(text);
				CFRelease(text);
				text = NULL;
			}
        }
    } else {
        EventRef ReadEvent;
        CreateEvent(NULL, kEventClassRead, kEventRead, 0, kEventAttributeNone, &ReadEvent);
        SetEventParameter(ReadEvent, kEventParamRead, typeChar, len, str);
        SendEventToEventTarget(ReadEvent, GetWindowEventTarget(ConsoleWindow));
    }
}

/* Aqua's WriteConsole. Uses internally Raqua_WriteEvent, 
but performs the following tasks: re-codes the string (Lat2Mac), 
buffers the output 
*/
void Raqua_WriteConsole(char *str, int len)
{
    OSStatus err;
    TXNOffset oStartOffset; 
    TXNOffset oEndOffset;
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
        if (WeAreBuffering){
            if (strlen(buf)+1+end_of_buffer >= inputBufferSize){ //CurrentPrefs.BufferSize){
                if (end_of_buffer>0)
                    Raqua_WriteEvent(outputbuffer, end_of_buffer);
                strcpy(outputbuffer, buf);
                end_of_buffer = strlen(buf);
            } else {
                /* we perform only one WriteEvent per call for performance reasons. If there it still something left in the buffer, leave it for the next WriteConsole or FlushBuffer call. */
                strcpy(outputbuffer+end_of_buffer, buf);
                end_of_buffer+= strlen(buf);
            }
        } else {
            Raqua_WriteEvent(buf, strlen(buf));
        }
    } else {
        fprintf(stderr,"%s", buf);
    }
    
    free(buf);
	
}

/*Flushes the console output buffer. This is a no-op if buffering is disabled. */
static void Aqua_FlushBuffer(void){
	if (WeHaveConsole) {
		if (WeAreBuffering) {
			if (end_of_buffer>0)
				Raqua_WriteEvent(outputbuffer, end_of_buffer);
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
	
	CopyCStringToPascal(RFontFaces[CurrentPrefs.RFontFace-1], fontname);
	GetFNum(fontname,&fontID);
	
	defaults.fontID = fontID;  
	defaults.pointSize = Long2Fix(RFontSizes[CurrentPrefs.RFontSize-1]); 
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
	
	if(!InputFinished) {
		Raqua_WritePrompt(prompt);
		/* Aqua_RWrite(prompt); */
	}
	
	TXNFocus(RConsoleInObject,true);
	TXNSetTypeAttributes( RConsoleInObject, 1, RInAttr, 0, kTXNEndOffset );
	
	while(!InputFinished & !HaveBigBuffer) 
        Raqua_ProcessEvents();
	
	if(!HaveBigBuffer){
		if(WeHaveCocoa)  {
			txtlen = strlen(inputBuffer);
			finalBufPos = txtlen;
			curBufPos = 0;
			HaveBigBuffer = true;           
		} else {
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
	}
	if(HaveBigBuffer){
		for (i = curBufPos; i <= finalBufPos; i++) {
			if(WeHaveCocoa)
				TempBuf = inputBuffer[i];
			else
				TempBuf = (*BufDataHandle)[i];
			
			if ((TempBuf == '\r') || (TempBuf == '\n') || (i == finalBufPos)){
				if (WeHaveCocoa)
					strncpy(buf,inputBuffer+curBufPos,i-curBufPos+1);
				else
					strncpy(buf,*(BufDataHandle)+curBufPos,i-curBufPos+1);
                buf[i-curBufPos] = '\n';
                buf[i-curBufPos+1] = '\0';
				if (WeHaveCocoa)
					Raqua_WriteUserInput(buf);
				else
					Raqua_WriteConsole(buf,strlen(buf));
				if (!WeHaveCocoa && strlen(buf) > 1)
                    maintain_cmd_History(buf);
                for(j=0; j<strlen(buf); j++){
                    tmp = (unsigned char)buf[j];
                    if(tmp>127)
                        buf[j] = (char)Mac2Lat[tmp-127-1];
                }
				if (i == finalBufPos-1) i++;
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
		 if (!WeHaveCocoa) {
			 HUnlock(BufDataHandle);
			 DisposeHandle(BufDataHandle);
			 BufDataHandle = NULL;
		 } else
			 maintain_cmd_History(inputBuffer);
     }
	} /* HaveBigBuffer */

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
	ForceConsoleRefresh();
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

/** this buffer is used by ReadHandler to store received string before passing it to Aqua_RWrite */
static char readHandlerBuffer[32769];

/** ReadHandler handless event kEventRead of the class kEventClassRead. Those are generated by Raqua_WriteEvent which is usually called by Raqua_WriteConsole. The handler writes the received string into the console as R output. */
static OSStatus ReadHandler(EventHandlerCallRef inCallRef, EventRef REvent, void *inUserData)
{
    OSStatus	err = eventNotHandledErr;
    char c;
    UInt32 len;
    
    /* make sure that we're processing a "read" event */
    if ( GetEventClass( REvent ) == kEventClassRead )
    {
        switch ( GetEventKind( REvent ) )
        {
            case kEventRead:
                err = GetEventParameter (REvent, kEventParamRead, typeChar, NULL, 32768, &len, readHandlerBuffer);
                readHandlerBuffer[len]=0;
                Aqua_RWrite(readHandlerBuffer);
                err = noErr;
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
			CFMsg = NULL;
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
        if(inputText){
			CFRelease(inputText);
			inputText = NULL;
		}
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
    if(text){
		GetControlByID(RAboutWindow, &versionInfoID, &versionControl);
		SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
		controlStyle.flags = kControlUseJustMask;
		controlStyle.just = teCenter;
		CFRelease(text);
		text = NULL;
	}
    
    text = CFStringCreateWithFormat( NULL, NULL, CFSTR("R : Copyright %s, The R Development Core Team"), R_YEAR);
    if(text){
		GetControlByID(RAboutWindow, &CopyrightID, &versionControl);
		SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
		controlStyle.flags = kControlUseJustMask;
		controlStyle.just = teCenter;
		CFRelease(text);
		text = NULL;
    }
	
    text = CFSTR("Aqua GUI by Stefano M. Iacus and Thomas Lumley (2003-4).\rPlease send feedback to stefano.iacus@unimi.it");
    if(text){
		GetControlByID(RAboutWindow, &AuthorsID, &versionControl);
		SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
		controlStyle.flags = kControlUseJustMask;
		controlStyle.just = teCenter;
		CFRelease(text);
		text = NULL;
	}
    
	text = CFSTR("Thanks to: Jan de Leeuw, Simon Urbanek, Byron Ellis");
    if(text){
		GetControlByID(RAboutWindow, &ThanksToID, &versionControl);
		SetControlData(versionControl, kControlLabelPart, kControlStaticTextCFStringTag, sizeof(CFStringRef), &text);
		controlStyle.flags = kControlUseJustMask;
		controlStyle.just = teCenter;
		CFRelease(text);
		text = NULL;
	}
	
    if( (fileName = CFStringCreateWithCString(NULL, "RLogo.png", kCFStringEncodingASCII)) != NULL ){
        url = CFBundleCopyResourceURL( appBundle, fileName, NULL, NULL );
        if(url)
			provider = CGDataProviderCreateWithURL(url);
        if(provider)
			image = CGImageCreateWithPNGDataProvider( provider, NULL, false,  kCGRenderingIntentDefault );
        if(provider)
			CGDataProviderRelease(provider);
        if(url){
			CFRelease(url);
			url = NULL;
		}
        if(fileName){
			CFRelease(fileName);
			fileName = NULL;
		}
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
	
    if(TitleText){
		CFRelease(TitleText);
		TitleText = NULL;
	}
    if(MsgText){
		CFRelease(MsgText);
		MsgText = NULL;
	}
	
    return(userAction);      
}




void RAqua2Front(void){
    if (WeHaveCocoa) {
    } else {
		if(ConsoleWindow!=NULL)
			SelectWindow(ConsoleWindow);
    }
	
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
	
    if(TitleText){
		CFRelease(TitleText);
		TitleText = NULL;
	}
    if(MsgText){
		CFRelease(MsgText);
		MsgText = NULL;
	}
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
	
	CopyCStringToPascal(RFontFaces[CurrentPrefs.RFontFace-1], fontname);
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
    typeAttr.data.dataValue = Long2Fix(RFontSizes[CurrentPrefs.RFontSize-1]);
	
    TXNSetTypeAttributes(RConsoleOutObject, 1, &typeAttr, 0, kTXNEndOffset);
    TXNSetTypeAttributes(RConsoleInObject, 1, &typeAttr, 0, kTXNEndOffset);
	
}

/* Sets tab space for Console In and Out
tabsize: number of chars
*/


void RSetTab(void){
    TXNControlTag tabtag = kTXNTabSettingsTag;
    TXNControlData tabdata;
	
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.RTabSize*RFontSizes[CurrentPrefs.RFontSize-1]);
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
						
					case kRCmdRappUpdates:
						consolecmd("Rapp.updates()");
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
						
					case kRCmdEditObject:
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
							ShowWorkingDir();
							break;
							
						case kRCmdShowWorkDir:
							consolecmd("getwd()");
							ShowWorkingDir();
							break;
							
						case kRCmdResetWorkDir:
							ChangeStartupDir();
							ShowWorkingDir();
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
							
						case kRMacOSXFAQ:
							consolecmd("system(\"open $R_HOME/RMacOSX-FAQ.html\")");
							break;
							
						case kRAquaWhatsNew:
							consolecmd("file.show(file.path(R.home(),\"NEWS.aqua\"))");
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
            if(xd->auxcontext){
				CGContextRelease(xd->auxcontext);
				xd->auxcontext = NULL;
			}
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



void Raqua_showarrow(void);
void Raqua_showarrow(void){
	ControlRef	RGUIControl;
	
	GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
	ShowControl(RGUIControl);
	GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
	ShowControl(RGUIControl);
	GetControlByID(ConsoleWindow, &WorkingDirID, &RGUIControl);
	HideControl(RGUIControl);
}

void Raqua_hidearrow(void);
void Raqua_hidearrow(void){
	ControlRef	RGUIControl;
	
	GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
	HideControl(RGUIControl);
	GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
	HideControl(RGUIControl);
	GetControlByID(ConsoleWindow, &WorkingDirID, &RGUIControl);
	ShowControl(RGUIControl);
	
}

void Raqua_Busy(int which);

void Raqua_Busy(int which)
{
    if (WeHaveCocoa)  {
        if(cocoaRisBusy) (*cocoaRisBusy)(which);
    } else {
        if(which == 1) 
            Raqua_showarrow();
        else 
            Raqua_hidearrow();
    }
}


void RescaleInOut(double prop)
{  
	Rect 	WinBounds, InRect, OutRect,bnd;
	ControlRef	RGUIControl;
	FontInfo  finfo;
	
	GetWindowPortBounds(ConsoleWindow, &WinBounds);
	GetControlByID(ConsoleWindow, &RGUIBusy, &RGUIControl);
	MoveControl (RGUIControl, WinBounds.right - 20, 4);
	GetControlByID(ConsoleWindow, &RGUISep, &RGUIControl);
	SizeControl(RGUIControl, WinBounds.right, 4);
	GetControlByID(ConsoleWindow, &RGUIText, &RGUIControl);
	MoveControl(RGUIControl, 0 /*WinBounds.right - 100*/, 4);
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
	RSetConsoleWidth();
	
	
}

void RSetConsoleWidth(void){
	Rect WinBounds;
	TXNGetViewRect(RConsoleOutObject, &WinBounds);
	if(R_Is_Running && CurrentPrefs.SetConsoleWidthOnResize)					 	
		R_SetOptionWidth(floor((double)WinBounds.right / (double)RFontSizes[CurrentPrefs.RFontSize-1] * 1.61));
}


OSStatus ResizeHelpWindow(WindowRef theWindow);

OSStatus ResizeHelpWindow(WindowRef theWindow){
	TXNObject   TXObj;
	TXNFrameID  TXFrameID;
	Rect		bounds;
	OSStatus	err = -1;
	
	if( GetWindowProperty(theWindow, 'RHLP', 'robj', sizeof(TXNObject), NULL, &TXObj) == noErr){
		if( GetWindowProperty(theWindow, 'RHLP', 'rfrm', sizeof(TXNFrameID), NULL, &TXFrameID) == noErr){
			GetWindowPortBounds(theWindow, &bounds);
			TXNSetFrameBounds(TXObj, 0, 0,  bounds.bottom, bounds.right, TXFrameID);
			BeginUpdate(theWindow);
			TXNForceUpdate(TXObj);
			TXNDraw(TXObj,NULL);
			EndUpdate(theWindow);
			err = noErr; 
		}  
	}
	
	return err;
}

OSStatus ResizeEditWindow(WindowRef theWindow);

OSStatus ResizeEditWindow(WindowRef theWindow){
	TXNObject   TXObj;
	TXNFrameID  TXFrameID;
	Rect		bounds;
	OSStatus	err = -1;
	
	if( GetWindowProperty(theWindow, 'REDT', 'robj', sizeof(TXNObject), NULL, &TXObj) == noErr){
		if( GetWindowProperty(theWindow, 'REDT', 'rfrm', sizeof(TXNFrameID), NULL, &TXFrameID) == noErr){
			GetWindowPortBounds(theWindow, &bounds);
			TXNSetFrameBounds(TXObj, 0, 0,  bounds.bottom, bounds.right, TXFrameID);
			BeginUpdate(theWindow);
			TXNForceUpdate(TXObj);
			TXNDraw(TXObj,NULL);
			EndUpdate(theWindow);
			err = noErr; 
		}  
	}
	
	return err;
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
	EventRef	REvent, MyEvent;
	TXNFrameID	HlpFrameID  = 0;
	UInt32		eventClass;
	TXNObject 	HlpObj = NULL;
	HIPoint where;
	WindowPartCode partCode;
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
					partCode = FindWindow(outEvent.where, &mywin);
                if( (partCode == inMenuBar)){
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
					CopyPascalStringToC(fontname,TempPrefs.DeviceFontName);
					TempPrefs.DevicePointSize = fontSize;
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
				case kEventWindowZoomed:
					if( (err = ResizeHelpWindow(EventWindow)) != noErr)
						err = ResizeEditWindow(EventWindow);
				break;
				
				case kEventWindowShown:
					if( EventWindow == ConsoleWindow)
							RescaleInOut(0.8);
				break;
					
				case kEventWindowBoundsChanged:  
					if( RWinCode != 9){ 
						if( EventWindow == ConsoleWindow){
							RescaleInOut(0.8);
							err = noErr;
						} 
						
					}
					break;
					
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
	
    if (WeHaveCocoa) {
        userInput(cmd);
    } else {
        TXNSetData (RConsoleInObject, kTXNTextData, cmd, strlen(cmd), 0, TXNDataSize(RConsoleInObject));
		SendReturnKey();
	}
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
    Rect	WinBounds, mainRect;
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
	
	SetRect(&WinBounds, 0, 0,  (int)(80.0*(double)RFontSizes[CurrentPrefs.RFontSize-1] / 1.61), 400);
	
	if( (err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute | kWindowStandardDocumentAttributes, &WinBounds, &EditWindow) != noErr))
		goto fail;
    
	mainRect = (*GetMainDevice()) -> gdRect;
	RepositionWindow (EditWindow,  NULL, kWindowCascadeOnMainScreen);
	GetWindowBounds(EditWindow, kWindowStructureRgn, &WinBounds);
	WinBounds.left = mainRect.right - WinBounds.right + 1;
	WinBounds.right = mainRect.right;
	SetWindowBounds(EditWindow, kWindowStructureRgn, &WinBounds); 
	
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
	
    
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.RTabSize*RFontSizes[CurrentPrefs.RFontSize-1]);
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
    typeAttr.data.dataValue = Long2Fix(RFontSizes[CurrentPrefs.RFontSize-1]);
	
    TXNSetTypeAttributes(REdirObject, 1, &typeAttr, 0, kTXNEndOffset);
	
	CopyCStringToPascal(RFontFaces[CurrentPrefs.RFontFace-1], fontname);
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
    Rect	WinBounds, mainRect;
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
	
    SetRect(&WinBounds, 0, 0,  (int)(80.0*(double)RFontSizes[CurrentPrefs.RFontSize-1] / 1.61), 400);
	
	if( (err = CreateNewWindow( kDocumentWindowClass, kWindowStandardHandlerAttribute | 
								kWindowStandardDocumentAttributes, &WinBounds, &HelpWindow) != noErr))
		goto fail;
    
	
	mainRect = (*GetMainDevice()) -> gdRect;
	RepositionWindow (HelpWindow,  NULL, kWindowCascadeOnMainScreen);
	GetWindowBounds(HelpWindow, kWindowStructureRgn, &WinBounds);
	WinBounds.left = mainRect.right - WinBounds.right + 1;
	WinBounds.right = mainRect.right;
	SetWindowBounds(HelpWindow, kWindowStructureRgn, &WinBounds); 
	
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
    
	
    
    tabdata.tabValue.value = (SInt16)(CurrentPrefs.RTabSize*RFontSizes[CurrentPrefs.RFontSize-1]);
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
    typeAttr.data.dataValue = Long2Fix(RFontSizes[CurrentPrefs.RFontSize-1]);
	
    TXNSetTypeAttributes(RHelpObject, 1, &typeAttr, 0, kTXNEndOffset);
	
	CopyCStringToPascal(RFontFaces[CurrentPrefs.RFontFace-1], fontname);
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


OSStatus FSPathMakeFSSpec(const UInt8 *path, FSSpec *spec, Boolean *isDirectory)	{
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
char		*Cmd_Hist[maxhist];
int			g_cur_Cmd, g_start_Cmd, g_end_Cmd;
Boolean		g_Stop = false;
Boolean		g_down = true;
Boolean		g_not_first = false;

/*
	do_Down_Array:
	This procedure used to maintain the reponse when you click the down array key. 
	(about display previous command in console window)                                            
 */
void HistBack(void){
	SInt32 textLength;
	
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



void Raqua_GetQuartzParameters(double *width, double *height, double *ps, char *family, Rboolean *antialias, Rboolean *autorefresh, int *quartzpos){
	
	if( CurrentPrefs.OverrideRDefaults == 0)
		return; /* we don't touch user's parameters */
	
	*width = CurrentPrefs.DeviceWidth;
	*height = CurrentPrefs.DeviceHeight;
	*ps = (double)CurrentPrefs.DevicePointSize;
	strcpy(family, CurrentPrefs.DeviceFontName);
	*antialias = CurrentPrefs.AntiAlias;
	*autorefresh = CurrentPrefs.AutoRefresh;   
	*quartzpos = CurrentPrefs.QuartzPos; 
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
	
	alertParamRec.movable = false;				/* Make alert movable modal */
	alertParamRec.helpButton = false;			/* Is there a help button? */
	alertParamRec.filterProc = NULL;			/* event filter */
	alertParamRec.defaultText = NULL;			/* Text for button in OK position */
	alertParamRec.cancelText = NULL;			/* Text for button in cancel position */
	alertParamRec.otherText = NULL;				/* Text for button in left position */
	alertParamRec.defaultButton = 1;			/* Which button behaves as the default */
	alertParamRec.cancelButton = 0;				/* Which one behaves as cancel (can be 0) */
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

OSErr GotRequiredParams( const AppleEvent *ae );

/*  isImageData:
returns -1 on error, 0 if the file is RDX2 or RDX1, 
1 otherwise.
*/	
int isImageData(char *fname);
int isImageData(char *fname){
	FILE * fp;
	int flen;
	
	char buf[5];
	if( (fp = R_fopen(R_ExpandFileName(fname), "r")) ){
		fseek(fp, 0L, SEEK_END);
		flen = ftell(fp);
		rewind(fp);
		if(flen<4) 
			return(1);
		fread(buf, 1, 4, fp);
		buf[4] = '\0';
		if( (strcmp(buf,"RDX2")==0) || ((strcmp(buf,"RDX1")==0))) return(0);
		else return(1);
	} else
		return(-1);
}

/* HandleOpenDocument routine :
Description :
This Event will be generated when you click on a R file icon.
This event can only be depatched by the ProcessEvent routine.
Thus, even you click on the file icon of R to start R, this event
will not be catch until the R_readConsole start.
*/
pascal OSErr HandleOpenDocument( const AppleEvent *ae,
								 AppleEvent *reply, SInt32 refCon )
{
#pragma unused ( reply, refCon )
    AEDescList		docList;
    AEKeyword		keyword;
    DescType		actualType;
    Size		actualSize;
    SInt32		numberOfDocuments;
    FSSpec		fileSpec;
    OSErr		err;
    FInfo		fileInfo;
    SInt16		pathLen;
    Handle		pathName=NULL;
    char InitFile[300];
	char buf[310], cmd[500];
	
    docList.descriptorType = typeNull;
    docList.dataHandle = nil;
    
	/* extracts direct parameter from the Apple event */
	
    if ( ( err = AEGetParamDesc( ae, keyDirectObject, typeAEList, &docList ) ) != noErr )
		goto cleanup;
	
    /* perform the recommended check for additional required parameters */
	
    if ( ( err = GotRequiredParams( ae ) ) != noErr )
		goto cleanup;
	
    if ( ( err = AEGetNthPtr( &docList, 1, typeFSS, &keyword, &actualType,
							  &fileSpec, sizeof( fileSpec ), &actualSize ) ) != noErr )
		goto cleanup;
	
    err = FSpGetFInfo(&fileSpec, &fileInfo);
    if (err != noErr) goto cleanup;
	
	err = FSMakePath(fileSpec.vRefNum, fileSpec.parID, fileSpec.name, buf, 300);  
	if(isImageData(buf)==0){
		sprintf(cmd,"load(\"%s\")",buf);
		consolecmd(cmd);
	} else {
		sprintf(cmd,"source(\"%s\")",buf);
		consolecmd(cmd);
	}
    
cleanup:
		return err;
	
}

OSErr GotRequiredParams( const AppleEvent *ae )
{
    DescType actualType;
    Size actualSize;
    OSErr err;
    
    err = AEGetAttributePtr( ae, keyMissedKeywordAttr, typeWildCard, &actualType, nil, 0, &actualSize );
	
    return	( err == errAEDescNotFound ) ? noErr :
		( err == noErr ) ? errAEParamMissed : err;
	
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
	
	if (cocoaProcessEvents) cocoaProcessEvents(0); 
	
	if(ReceiveNextEvent(0, NULL, kEventDurationForever  ,true,&theEvent)== noErr){
		conv = ConvertEventRefToEventRecord(theEvent, &outEvent);
		
		if(conv && (outEvent.what == kHighLevelEvent))
			AEProcessAppleEvent(&outEvent);
		
		SendEventToEventTarget (theEvent, theTarget);
		ReleaseEvent(theEvent);
		
	} 
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


/* Code for accessing external cocoa bundle */

enum
{
	kOpenCocoaWindow = 'COCO'
};

static int userInput(const char *text) {
	strncpy(inputBuffer,text,inputBufferSize-2);
	InputFinished=true;
	return 0;
}

static void
loadPrivateFrameworkBundle(CFStringRef framework, CFBundleRef *bundlePtr) 
{
	CFURLRef baseURL = NULL;
	CFURLRef CocoabundleURL = NULL;
	OSStatus (*funcPtr)(void *);
	
	baseURL = CFBundleCopyPrivateFrameworksURL(RBundle);
	
	if (baseURL == NULL){
		REprintf("\n CantCopyURL");
		goto CantCopyURL;
	}

	CocoabundleURL = CFURLCreateCopyAppendingPathComponent(kCFAllocatorSystemDefault, baseURL, CFSTR("RGUI.bundle"), false);
	if(CocoabundleURL == NULL){
		REprintf("\n CantCreateCocoaBundleURL");
		goto CantCreateBundleURL;
	}
	*bundlePtr = CFBundleCreate(NULL, CocoabundleURL);
	if (*bundlePtr) {
		/* set pointers to all known Cocoa functions. unsupported functions will be 0 */
		cocoaInitializeBundle = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("initializeBundle"));
		cocoaDeInitializeBundle = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("DeInitializeBundle"));
		cocoaSelectWindow = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("selectWindow"));
		cocoaWriteConsole = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("writeConsole"));
		cocoaWritePrompt = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("writePrompt"));
		cocoaWriteUserInput = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("writeUserInput"));
		cocoaRisBusy = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("RisBusy"));
		cocoaSetupMenu = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("setupMenu"));
		cocoaProcessEvents = CFBundleGetFunctionPointerForName(*bundlePtr, CFSTR("processEvents"));
		
		if (!cocoaInitializeBundle)
			REprintf("Cocoa bundle found, but initializeBundle function is not present. The bundle won't be used.\n");
		else
			cocoaFeatures=(*cocoaInitializeBundle)(userInput);
		
		/* we perform sanity checks for each feature set to prevent segfaults due to undefined functions */
		if (((cocoaFeatures&cocoa_basic)>0) && ((!cocoaWriteConsole)||(!cocoaWritePrompt))) {
			REprintf("Cocoa bundle advertizes basic features, but at least one feature was not found! Disabling bundle.\n");
			cocoaFeatures=0;
		}
		if (((cocoaFeatures&cocoa_loop)>0) && ((!cocoaProcessEvents))) {
			REprintf("Cocoa bundle advertizes event loop features, but at least one feature was not found! Disabling bundle.\n");
			cocoaFeatures=0;
		}
	}

	
	if(CocoabundleURL){
		CFRelease(CocoabundleURL);
		CocoabundleURL = NULL;
	}
CantCreateBundleURL:
		if(baseURL){
			CFRelease(baseURL);
			baseURL = NULL;
		}
CantCopyURL:
CantFindMainBundle:
		return;
}


static OSStatus
appCommandHandler(EventHandlerCallRef inCallRef, EventRef inEvent, void* userData) {
	HICommand command;
	OSStatus err = eventNotHandledErr;
	
	if (GetEventKind(inEvent) == kEventCommandProcess) {
		GetEventParameter( inEvent, kEventParamDirectObject, typeHICommand, NULL, sizeof(HICommand), NULL, &command );
		switch ( command.commandID ) {
			case kOpenCocoaWindow:
				fprintf(stderr, "appcommandHandler: kOpenCocoaWindow received\n");
				err = noErr;
				break;
			default:
				break;
		}
	}
	return err;
}



#endif /* HAVE_AQUA */

#endif /* __AQUA_CONSOLE__ */
