/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File RIntf.h
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *   
 *  This file was originally written by: Wing Kwong (Tiki), WAN 3/2/99
 *  Updated to last version of WasteLib library: Stefano M. Iacus, 2001
 *
 *  Original file was:
 *
 *	WASTE Demo Project:
	Demo Header

	Copyright © 1993-1998 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub
*/

/*
	Due to differences between Pascal and C, this file (nothing like it originally existed
	in the original Pascal code) was neccessary to create.  In it contains various macros,
	constants, type and class declarations, function prototypes, etc..  From the original
	code, some of this material would be spread amongst the source files, but a good
	majority of this file comes from the WEDemoIntf.p file (the declarations, constants, etc).
	There still is a WEDemoIntf.c file in the project tho since the .p file had a few
	utility functions declared and defined in it.
*/

/*	make sure TARGET_API_MAC_CARBON is #defined
*/


#ifndef TARGET_API_MAC_CARBON
#define TARGET_API_MAC_CARBON	0
#endif

/*	Check the version number of the Universal Interfaces we're using
*/

#if TARGET_API_MAC_CARBON
	#if ((!defined(UNIVERSAL_INTERFACES_VERSION))||(UNIVERSAL_INTERFACES_VERSION<0x330))
		#error "You need Universal Headers version 3.3 or newer to compile the R for Carbon"
	#endif
#else
	#if ((!defined(UNIVERSAL_INTERFACES_VERSION))||(UNIVERSAL_INTERFACES_VERSION<0x320))
		#error "You need Universal Headers version 3.2 or newer to compile the R"
	#endif
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Balloons.h>   
#include <Dialogs.h>
#include <Scrap.h>
#include <Sound.h>
#include <Appearance.h>
#include <AERegistry.h>
#include <Devices.h>
#include <Folders.h>
#include <Fonts.h>
#include <LowMem.h>
#include <Processes.h>
#include <Resources.h>
#include <StandardFile.h>
#include <TextUtils.h>
#include <ToolUtils.h>
#include "dirent.h"


/*#include "macdir.h"
*/
#ifndef __WEDEMOAPP__
#define __WEDEMOAPP__

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __RESOURCES__
#include <Resources.h>
#endif

#ifndef __QUICKDRAW__
#include <QuickDraw.h>
#endif

#ifndef __MENUS__
#include <Menus.h>
#endif

#ifndef __WINDOWS__
#include <Windows.h>
#endif

#ifndef __CONTROLS__
#include <Controls.h>
#endif

#ifndef __STANDARDFILE__
#include <StandardFile.h>
#endif

#ifndef _WASTE_
#include "WASTE.h"
#endif

#ifndef _LIMITS
#include "limits.h"
#endif

#ifndef __MACTYPES__
#include <MacTypes.h>
#endif

#ifndef __RESOURCES__
#include <Resources.h>
#endif

#ifndef __QUICKDRAW__
#include <QuickDraw.h>
#endif

#ifndef __MENUS__
#include <Menus.h>
#endif

#ifndef __MACWINDOWS__
#include <MacWindows.h>
#endif

#ifndef __CONTROLS__
#include <Controls.h>
#endif

#if (UNIVERSAL_INTERFACES_VERSION > 0x320)
	#ifndef __CONTROLDEFINITIONS__
	#include <ControlDefinitions.h>
	#endif
#endif

#ifndef __NAVIGATION__
#include <Navigation.h>
#endif

#if TARGET_API_MAC_CARBON
	#ifndef __PMAPPLICATION__
	#include <PMApplication.h>
	#endif
#else
	#ifndef __PRINTING__
	#include <Printing.h>
	#endif
#endif

#ifndef __CARBONCOMPATIBILITY__
#include "CarbonCompatibility.h"
#endif

#include <R_ext/Boolean.h>
#include "Startup.h"

enum
{

	/*	WASTE Demo signature
*/
//	sigWASTEDemo			=		FOUR_CHAR_CODE ( 'OEDE' ),

	/*	resource types, clipboard types, and file types
*/
	kTypeMenuColorTable		=		FOUR_CHAR_CODE ( 'mctb' ),
	kTypeDeskAccessory		=		FOUR_CHAR_CODE ( 'DRVR' ),
	kTypeDialogItemList		=		FOUR_CHAR_CODE ( 'DITL' ),
	kTypeFont				=		FOUR_CHAR_CODE ( 'FONT' ),
	kTypePicture			=		FOUR_CHAR_CODE ( 'PICT' ),
	kTypePrintRecord		=		FOUR_CHAR_CODE ( 'PREC' ),
	kTypePageFormat			=		FOUR_CHAR_CODE ( 'WEpg' ),		/*	Carbon PMPageFormat object */
	kTypePageMargins		=		FOUR_CHAR_CODE ( 'MARG' ),		/*	page margins */
	kTypeSound				=		FOUR_CHAR_CODE ( 'snd ' )
} ;


enum
{
	/*	user tags used in association with WEGetUserInfo / WESetUserInfo
	*/
	kWindowTag				=		FOUR_CHAR_CODE ( 'WIND' ),		/*	reference to owner window */
	kPrintRecordTag			=		kTypePrintRecord,
	kPageFormatTag			=		kTypePageFormat,
	kPageMarginsTag			=		kTypePageMargins
} ;

/* a PageMarginRec is a structure defining the margins to use
 when printing a document page.  Margins are expressed in Fixed (16:16) points.
*/
struct PageMarginRec
{
	Fixed				top ;
	Fixed				bottom ;
	Fixed				left ;
	Fixed				right ;
} ;

typedef struct PageMarginRec PageMarginRec, *PageMarginRecPtr, **PageMarginRecHandle ;


/*
   #include "mac graphics.h"
*/

/*
 *	Some utility macros.
 */

#define kMaxRowBytes	0x3FFE		/* Maximum number of bytes in a row of pixels */
#define kDefaultRes		0x00480000	/* Default resolution is 72 DPI; Fixed type */
#define kITabRes		4			/* Inverse-table resolution */
#define kOffDepth		32			/* Number of bits per pixel in off-screen environment */
#define rGrayClut		1600		/* Resource ID of gray-scale clut */
#define rColorClut		32			/* Resource ID of full-color clut */

#define rTypePrefs	    'PrFn'
#define  kPrefsID	    128

#define kMaxWindows		10

/* 	"Originally," these are things built into the Pascal language
 	but since C doesn't have anything like them, and in an attempt to keep the code readable
 	and similar to the original Pascal source, these #define macros work nice.
*/

#define BTST( FLAGS, BIT )				((FLAGS) &   (1UL << (BIT)))
#define BSET( FLAGS, BIT )  			((FLAGS) |=  (1UL << (BIT)))
#define BCLR( FLAGS, BIT )  			((FLAGS) &= ~(1UL << (BIT)))
#define BCHG( FLAGS, BIT )  			((FLAGS) ^=  (1UL << (BIT)))

#define ABS(A) ((A) > 0 ? (A) : -(A))

#define BSL(A, B)	(((long) (A)) << (B))
#define BSR(A, B)	(((long) (A)) >> (B))
#define BOR(A, B)	((A) | (B))
#define BAND(A, B)	((A) & (B))

#ifndef HiWrd
#define HiWrd(aLong)		(((aLong) >> 16) & 0xFFFF )
#endif
#ifndef LoWrd
#define LoWrd(aLong)		((aLong) & 0xFFFF )
#endif


#define	rAppStringsID			128

enum {
	sApplicationName 		= 1,
	sTranslationLockedErr,
	sTranslationErr,
	sOpeningErr,
	sReadErr,				// 5
	sWriteToBusyFileErr,
	sBusyOpen,
	sChooseFile,
	sChooseFolder,
	sChooseVolume,			// 10
	sCreateFolder,
	sChooseObject,
	sChooseApp
};

#define kSelectFolderPrefKey	4

enum {

/*	R signature
*/
	R_ID		=		'OFFF',

/*	resource types, clipboard types, and file types
*/

/*	kTypeDeskAccessory	=		'DRVR',   
	kTypeFont			=		'FONT',  
	kTypeFontTable		=		'FISH',
	kTypePicture		=		'PICT',    Jago
	kTypeSound			=		'snd ',   
	kTypeSoup			=		'SOUP',
	kTypeStyles			=		'styl',
	kTypeText			=		'TEXT'
*/
};

enum {

/*	virtual key codes for navigation keys found on extended keyboards
*/

	keyPgUp				=		0x74,
	keyPgDn				=		0x79,
	keyHome				=		0x73,
	keyEnd				=		0x77,

/*	virtual key codes generated by some function keys
*/
	keyF1				=		0x7A,
	keyF2				=		0x78,
	keyF3				=		0x63,
	keyF4				=		0x76
};

/*	possible values for HandleOpenDocument refCon parameter
*/
enum {
	kDoOpen		= 0,
	kDoPrint	= 1
};

/* 	other commonly used constants
*/
#define	kBarWidth						16			/* width of a scroll bar*/
#define kTitleHeight					20			/* usual height of a window title bar */
#define kTextMargin						3			/* indent of text rect from a window port rectr */
#define kScrollDelta					11			/* pixels to scroll when the scroll bar arrow is clickedr */
#define kStandardTranslucencyThreshold	16384		/* 16K sq pixels*/

#define  kDel                    0x7F
#define  kBs                     0x08
#define  kReturn                 0x0D

#define kMinSystemVersion				0x08608000	//	Mac OS 8.6
#define kMinWASTEVersion				0x0200400F	//	WASTE 2.0a15
#define kScrapThreshold					4 * 1024	/*	4 KBr */
#define ALLOW_INPUT_LENGTH              5000
#define kWindows                        131
#define MAX_NUM_G_WIN  50                          
#define MAX_NUM_E_WIN  50   
#define MAX_NUM_H_WIN  50                       

/* enumeration types used for closing a window and/or quitting the application
 */
typedef enum {closingWindow, closingApplication} ClosingOption;
typedef enum {savingYes, savingNo, savingAsk,savingCancel} SavingOption;

/* enumeration for orientation (vertical/horizontal)
 */
typedef enum {kVertical, kHorizontal} Orientation;

/*
 *	Resource ID numbers
 */

/* menu IDs
 */
 
enum {
	kMenuApple		= 1,
	kMenuFile,
	kMenuEdit,
	kMenuTools,
	kMenuWindows
};

/*	Apple Menu items
*/
enum {
	kItemAbout		= 1
};

/*	File menu items
*/
enum {
	kItemNew		 =  1,
	kItemEditObject  =  2,       
	kItemOpen		 =  4,
	kItemEditFile	 =  5,
	kItemShow		 =  6,
	kItemLoadW        =  8,
	kItemSaveWSAs	 =  9,
    kItemPageSetup   = 11,
	kItemPrint       = 12,
    kItemSave		 = 14,
    kItemSaveAs		 = 15,
	kItemClose		 = 16,
	kItemQuit		 = 18
};

/*	Edit menu items
*/
enum {
	kItemUndo		= 1,
	kItemCut		= 3,
	kItemCopy		= 4,
	kItemPaste		= 5,
	kItemCopyPaste  = 6,
	kItemClear		= 7,
	kItemSelectAll	= 8,
	kItemLineTo     = 9, 
	kItemPreference = 11
};

/*	Tools menu items
*/
enum {
    kItemShowWSpace  =  1,
	kItemClrWSpace	 =  2,
    kItemLoadWSpace  =  4,
	kItemSaveWSpace	 =  5,
	kItemLoadHistory =  7,
	kItemSaveHistory =  8,
	kItemShowHistory =  9,
	kItemChangeDir   =  11,
	kItemShowDir	 =  12,
	kItemResetDir    =  13,
	kItemShowLibrary =  15,
	kItemShowData	 =  16
};


/*	Alert & dialog template resource IDs
*/
enum {
	kAlertNeedSys7			= 128,
	kAlertNeedNewerWASTE	= 129,
	kAlertGenError			= 130,
	kAlertSaveChanges		= 131,
	kAlertSaveObject		= 142,
	kDialogAboutBox			= 256,
	kLineTo                 = 257,
	kEditObject             = 258,
	kError                  = 259,
	kPreferneces			= 260,
	kAbout                  = 261,
	kHelpObject             = 262,
	kExampleObject          = 263,
	kSearchObject           = 264
};

/*	String list resource IDs
*/
enum {
	kUndoStringsID				= 128,
	kClosingQuittingStringsID	= 129,
	kMiscStringsID				= 130,
	kAlertStringsID				= 131
};



/* miscellaneous resource IDs
*/
enum {
	kMenuBarID				= 128,
	kWindowTemplateID		= 128,
	kScrollBarTemplateID	= 128,
	kPromptStringID			= 128
};

/* a DocumentRecord is a structure associated with each window
 a handle to this structure is kept in the window refCon
*/
struct DocumentRecord
{
	WindowRef			owner;				// the window
	ControlRef			scrollBars [ 2 ];	// its scroll bars
	WEReference 		we;					// its WASTE instance
	OSType				docType;			// traditional text ('TEXT') or Unicode text ('utxt')
} ;

typedef struct DocumentRecord DocumentRecord, *DocumentPtr, **DocumentHandle;


struct History_Entries
{
   SInt16 commandType;
   SInt16 para1;
   SInt16 para2;
};

typedef struct History_Entries History_Entries, *History_EntriesPtr, **History_EntriesHandle;

struct Graphic_Ref
{
 	Handle History;
    SInt16 size;
    SInt16 MenuIndex;     /* You may use this to remeber the Menu Item position.
                           I use it as Boolean in this moment. */
    SInt32 cur_size;
    SInt16 fileRefNum;
    FSSpec fileFSSpec;
    GWorldPtr offScreen2;
    GWorldPtr offScreen;
    PixMapHandle offPixMapHandle;
    Ptr devdesc;  
    CGrafPtr colorPort;
    GDHandle colorDevice;
#if TARGET_API_MAC_CARBON
    CGrafPtr savedPort;		/* Pointer to the saved graphics environment */
#else
    GrafPtr savedPort;		/* Pointer to the saved graphics environment */
#endif
    GDHandle savedDevice;	/* Handle to the saved color environment */
    GrafPtr  printPort;
    GrafPtr  activePort;
    
}; 

typedef struct Graphic_Ref Graphic_Ref, *Graphic_RefPtr, **Graphic_RefHandle;


/*
 *	The external declaration of some global variables.
 */

/*  These are defined in WEDemoIntf.c
 */
extern	Boolean		gHasDragAndDrop;	/* true if Drag Manager is available */
extern	Boolean		gHasTextServices;	/* true is the Text Services Manager is available */
extern	Boolean		gExiting;			/* set this variable to drop out of the event loop and quit */

/*
 *	Function Prototypes
 */

/*	From DialogUtils.c
 */
ModalFilterUPP	GetMyStandardDialogFilter(void);
short			GetDialogItemType(DialogPtr, short);
Handle			GetDialogItemHandle(DialogPtr, short);
void			GetDialogItemRect(DialogPtr, short, Rect *);
void			SetDialogItemProc(DialogPtr, short, UserItemUPP);
void			FlashButton(DialogPtr, short);

/*	From LongControls.c
 */
OSErr			LCAttach( ControlHandle );
void			LCDetach( ControlHandle );
void			LCSetValue( ControlHandle, long );
void			LCSetMin( ControlHandle, long );
void			LCSetMax( ControlHandle, long );
long			LCGetValue( ControlHandle );
long			LCGetMin( ControlHandle );
long			LCGetMax( ControlHandle );
void			LCSynch( ControlHandle );


/*	From WEDemoIntf.c
 */
DocumentHandle	GetWindowDocument(WindowPtr);
WEReference GetWindowWE ( WindowRef window );
void			ErrorAlert( OSErr );
void			ForgetHandle( Handle * );
void			ForgetResource( Handle * );
void			BlockClr ( void *, Size ) ;
OSErr			NewHandleTemp( Size, Handle * );
void			PStringCopy( ConstStr255Param, Str255 );

/*	From WEDemoAbout.c
 */
OSErr			WETextBox( short, const Rect *, WEAlignment );
void			DoPreference( short );

/*	From RDrags.c
 */

OSStatus		InstallDragHandlers( void );
OSStatus		RemoveDragHandlers( void );

/*	From WEDemoEvents.c
 */
void			AdjustCursor( Point, RgnHandle );
void			DoMouseDown( const EventRecord * );
void			DoKeyDown( const EventRecord * );
void			DoDiskEvent( const EventRecord * );
void			DoOSEvent( const EventRecord * );
void			DoHighLevelEvent( const EventRecord * );
void			DoNullEvent( const EventRecord * );
void			DoWindowEvent( const EventRecord *);
void			ProcessEvent( void );
OSErr			GotRequiredParams( const AppleEvent * );
OSErr			InitializeEvents( void );

/* from WEDemoFiles.c
 */
OSStatus		ReadTextFile( const FSSpec *, WEReference );  /*Jago */
OSStatus		WriteTextFile( const FSSpec *, WEReference ); /* Jago */
OSStatus		ReadUnicodeTextFile ( const FSSpec *, WEReference ) ;  /* Jago  */
OSErr			WritePictFile( const FSSpec *, PicHandle );
pascal OSErr	TranslateDrag( DragReference, ItemReference, FlavorType, Handle );
pascal OSErr	CheckObjectLock( short, long, StringPtr );
OSStatus    	FSpCheckObjectLock( const FSSpec * );  /* Jago */

/* from WEDemoInit.c
 */
OSErr			Initialize( void );
void			Finalize( void );

/* from WEDemoMenus.c
 */
void			SetDefaultDirectory( const FSSpec * );
short			FindMenuItemText( MenuHandle, ConstStr255Param );
Boolean			EqualColor( const RGBColor *, const RGBColor * );
void			PrepareMenus( void );
void			DoDeskAcc( UInt16 );
OSErr			DoNew( Boolean editable );
OSStatus		DoOpen( void );
OSErr			OldDoOpen( void );
OSErr			SaveWindow( const FSSpec *, WindowPtr );
OSErr			DoSaveAs( const FSSpec *, WindowPtr );
OSStatus		DoSave( WindowPtr );
OSErr			DoClose( ClosingOption, SavingOption, WindowPtr );
OSErr			DoQuit( SavingOption );
void			DoAppleChoice( short );
void			DoFileChoice( short , WindowPtr);
void			DoEditChoice( short );
/*
void			DoFontChoice( short, EventModifiers );
*/
/*
void			DoSizeChoice( short );
*/
void			DoStyleChoice( short );
void			DoColorChoice( short );
void			DoAlignmentChoice( short );
void			DoDirectionChoice ( short ) ;
void			DoFeatureChoice( short );
void			DoMenuChoice( long, EventModifiers, WindowPtr );
OSErr			InitializeMenus( void );

/* from WEDemoScripting.c
 */
OSStatus		InstallCoreHandlers ( void ) ;
OSStatus		GetContentsOfSelection ( DescType, AEDesc *, WEReference ) ;
OSStatus		SetContentsOfSelection ( const AEDesc *, WEReference ) ;
OSStatus		GetAEDescDataAsHandle ( const AEDesc *, Handle * ) ;
OSStatus		GetFileRect ( const FSSpec *, Rect * ) ;
OSStatus		CreateObjectSpecifier ( const AEDesc *, DescType, AEKeyword, const AEDesc *, AEDesc * ) ;
OSStatus		CreatePropertySpecifier ( const AEDesc *, AEKeyword, AEDesc * ) ;
OSStatus		CreateFinderObjectSpecifier ( const FSSpec *, AEDesc * ) ;
OSStatus		CreatePSNBasedAppleEvent ( const ProcessSerialNumber *, AEEventClass, AEEventID, AppleEvent * ) ;

/* from WEDemoWindows.c
 */
void			DoDrag( Point, WindowPtr );
void			DoGrow( Point, WindowPtr );
void			DoZoom( short, WindowPtr );
Boolean			DoContent( Point, const EventRecord *, WindowPtr );
void			DoKey( short, const EventRecord * );
void			DoUpdate( WindowPtr );
void			DoActivate( Boolean, WindowPtr );
//OSErr			CreateWindow( const FSSpec * );
OSErr 			CreateWindow (const FSSpec * pFileSpec, Boolean editable);
void			DestroyWindow( WindowPtr );
void			Resize( Point, WindowPtr );
static void WindowResized ( const Rect * oldTextRect, WindowRef window );


/* R Editor
 */
void            RWrite(char*);
void            DRWrite(long);
void            eventLoop(void);
void            free_History(void);
void            maintain_cmd_History(char*);
void            do_Down_Array       (void);
void            R_WriteConsole1      (Ptr, SInt32);
void            R_ReadConsole1       (char* ,  char* , int, int);
void            R_WriteConsole2      (Ptr, SInt32);
void            R_ReadConsole2       (char* ,  char* , int, int);
Boolean         inRange             (int start, int end , int back, int length);
void            Change_Color_Range  (SInt32, SInt32,long, long,  long, WEReference);
void            Change_Color(long , long , long , WEReference);
/* ************************************************************************************************
print related functions
************************************************************************************************ */
void           do_Print                             (void);
OSStatus 	DoPageSetup(void);
#endif /*  */


/* ************************************************************************************************
extern Function (plug in function) (printing1.c, printing2.c, print.h)
************************************************************************************************ */
void			doPrinting(void);

void			R_ResetConsole(void);
void			R_FlushConsole(void);
void			R_ClearerrConsole(void);
void			R_Suicide(char *msg);
void			R_Busy(int which);
/* void			R_CleanUp(int ask); */
/*void			R_CleanUp(int, int, int);
 */
void            R_CleanUp(SA_TYPE, int, int);
char			*R_ExpandFileName(char *s);
void			R_RestoreGlobalEnv(void);
void			R_SaveGlobalEnv(void);

/*
SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_quit(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho);
*/

void			doCopyPString(Str255 sourceString,Str255 destinationString);
FILE*			R_OpenLibraryFile1(char *file);
FILE*			R_OpenSysInitFile1(void);

void			lineTo(SInt16, SInt16);
void			moveTo(SInt16, SInt16);
void			lineToWindow(SInt16, SInt16, SInt16);
void			moveToWindow(SInt16, SInt16, SInt16);
WindowPtr		CreateGraphicWindow(int wid, int h);
OSErr			OldnewWindow( const FSSpec* , int);
//OSStatus 		newWindow ( const FSSpec * pFileSpec, WindowRef * outWindow, int graphic );
OSStatus 		newWindow ( const FSSpec * pFileSpec, WindowRef * outWindow, int graphic, Boolean editable );
Boolean			isTextWindow(WindowPtr window);
void			LineFromToWindow(SInt16, SInt16, SInt16, SInt16,WindowPtr);
WindowPtr		Get_Graphic_Window(int);

int				isGraphicWindow(WindowPtr);
void			New_G_History(SInt16);
void			Kill_G_History(SInt16);
Boolean			RedrawWindow(SInt16);

void			AddToHistory( SInt16, History_Entries*);
void			GraUpdate(WindowPtr);
void			GraResize(WindowPtr);
char*			RevertString(char*);
void			GdrawString(char*, double);
void			FRWrite(double in);
double			toRadian(int angle);
void			RnWrite(char* buf, SInt16 len);
void			GWdoErrorAlert(SInt16 errorType);
void			GWdoConcatPStrings(Str255,Str255);
void			changeGWinPtr(WindowPtr , Str255);
FILE*			R_OpenFile(char *file);
void			changeSize(WindowPtr window, SInt16 newSize);
static void		CalcBigTextRect( WindowPtr window, Rect *textRect );

void			GraphicCopy(WindowPtr window);
PicHandle		getWPict(WindowPtr);
OSErr			doSaveGraCommand(void);
OSErr			doSaveAsGraCommand(void);
OSErr			doRSave(Boolean *);
/*OSErr OpenGraphicFile(FSSpec fileSpec, WindowPtr window);
*/
void			do_Up_Array(void);
void			Mac_Dev_Kill(WindowPtr window);
OSErr			doRSaveAs(Boolean *);
int				isEditWindow(WindowPtr window);
int				isHelpWindow(WindowPtr window);

/* int	R_ShowFiles(int nfile, char **fileName, char **title ,char *WinTitle);
*/
/*int				R_ShowFiles(int, char **, char **, char *, int, char *);
 */
 int	R_ShowFiles(int, char **, char **, char *, Rboolean, char *);
void			LoadEditEnvironment();
int				ggetc();
int				gungetc(int c);
void			ParseIncomplete();
void			ParseError();
void			ErrorDia(char* errorMessage);
void			Do_StandardAlert(Str255 LabelText);

   /***************************************************************************/
   /* Each driver can have its own device-specic graphical                    */
   /* parameters and resources.  these should be wrapped                      */
   /* in a structure (like the x11Desc structure below)                       */
   /* and attached to the overall device description via                      */
   /* the dd->deviceSpecific pointer                                          */
   /* NOTE that there are generic graphical parameters                        */
   /* which must be set by the device driver, but are                         */
   /* common to all device types (see Graphics.h)                             */
   /* so go in the GPar structure rather than this device-                    */
   /* specific structure                                                      */
   /***************************************************************************/
 
typedef struct {
    int cex;
    int windowWidth;
    int windowHeight;
    Boolean resize;
    int Text_Font;          /* 0 is system font and 4 is monaco */
    int fontface;           /* Typeface */
    int fontsize;           /* Size in points */
    int usefixed;
    RGBColor rgb[2];	    /* Window-Pict/Pixmap Port ForeColors */
    int col[2];
    WindowPtr window;
    int	lineType;
    SInt16 currentDash;
    SInt16 numDashes;
    short dashList[14];
    short dashStart_x;
    short dashStart_y;
}
MacDesc;

         
 
