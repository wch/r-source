/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RMenus.c
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
 *  Code cleanup and fix for q() based quiting.	Ihaka 30/6/99
 *  Updated to last version of WasteLib library and complete control
 *  over different windows.
 *  Menu 'Windows' implemented correctly: Stefano M. Iacus, 2001
 *
 *  Original file was:
 *  WEDemoMenus.c
 *
 *  WASTE Demo Project:
 *  Macintosh Controls with Long Values
 *
 *  Copyright © 1993-1998 Marco Piovanelli
 *  All Rights Reserved
 *
 *  C port by John C. Daub
 *
 * 	Description
 *
 *  This file is based on the WASTE and WASTE demo, I had do some
 *  modification to make it function as the way R want. The routine in
 *  here is used to handle event (high or low level event.)  There
 *  have a lot of function which is related to the Menu of Console and
 *  edit window in here.  We will not using them, however, it is
 *  pretty good to leave it in here. Because it is fully functional, and
 *  it functions correctly, when you feel that you need to add a size,
 *  font or color Menu..., what you need to do is to change the R.rsrc
 *  file ('MBAR' resource), You can simply overwrite the resource with
 *  ID 130 to ID 128. Then, a fully functional Menu will appear again.
 *
 */


/*            INCLUDE HEADER FILE           */
#ifndef __ALIASES__
#include <Aliases.h>
#endif

#ifndef __DEVICES__
#include <Devices.h>
#endif

#ifndef __ERRORS__
#include <Errors.h>
#endif

#ifndef __LOWMEM__
#include <LowMem.h>
#endif

#ifndef __STANDARDFILE__
#include <StandardFile.h>
#endif

#ifndef __FILETYPESANDCREATORS__
#include <FileTypesAndCreators.h>
#endif

#ifndef __TOOLUTILS__
#include <ToolUtils.h>
#endif
#include <AppleEvents.h>
#include <Processes.h>

#include <PMApplication.h>
//#include <PMDefinitions.h>
//#include <PMCore.h>

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

#include "WETabs.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"
#include <Scrap.h>
#include "Graphics.h"
#include <Rdevices.h>

/*         DEFINE CONSTANTS        */
#define eNoSuchFile                      9
#define eSelectNextTime                  10
#define kTypeMenuColorTable              'mctb'
#define kFinderSig                       'FNDR'
#define kAEFinderEvents                  'FNDR'
#define kSystemType                      'MACS'
#define kAEOpenSelection                 'sope'
#define keySelection                     'fsel'

/*  Global variables   */
Boolean                                  HaveContent = false;
Boolean                                  PrintPicture;
extern Boolean							 OnOpenSource;
extern Boolean							 Interrupt;
Handle	                                 myHandle=NULL;
SInt32                                   curPaste, finalPaste;
static Handle                            sColors;    /* handle to the 'mctb' resource for the Color menu*/
char									mTitle[265], wTitle[265];
extern OSErr DoSelectDirectory( void );
extern char *mac_getenv(const char *name);

MenuRef 		HelpMenu=NULL; /* This Handle willtake care of the Help Menu */
static 	short RHelpMenuItem=-1;
static 	short RTopicHelpItem=-1;
static	short RunExampleItem=-1;
static	short	SearchHelpItem=-1;

//	user structure passed to the NavEventFilter callback

typedef struct NavCallbackData
{
	ControlRef		formatPopup ;
	ControlRef		stationeryCheckbox ;
	SInt16			extraItemsID ;
	SInt16			numItems ;
	OSType			fileType ;
	Boolean			isStationery ;
} NavCallbackData ;


/*    Extern Global variables     */
extern char                              *gbuf;
extern SInt32                            gbuflen;
extern Handle                            gTextHdl;
extern FSSpec                            gHelpFile;
extern WindowPtr                         Console_Window;
extern SInt32                            gChangeAble, gpmtLh;
extern Boolean                           gfinishedInput;
extern Graphic_Ref                       gGReference[MAX_NUM_G_WIN + 1];
extern Boolean gPrintStructureInited;
extern PicHandle	                 gPictureHdl;
extern  char				 InitFile[256];
extern SInt16				 Edit_Window;
extern WindowPtr			 Edit_Windows[MAX_NUM_E_WIN + 1];
extern Boolean            		 defaultPort;
extern SInt16				 Help_Window;
extern WindowPtr			 Help_Windows[MAX_NUM_H_WIN + 1];
extern	SInt16			gTextSize;
extern	int				gScreenRes;
extern	Boolean		finished;
extern	FSSpec tempeditFSS;
/*    Protocols    */
void    assignPString                    (unsigned char* , char* , SInt16);
OSErr   FindAProcess                     (OSType, OSType, ProcessSerialNumber*);
OSErr   OpenSelection                    (FSSpecPtr theDoc);
void    ConsoleCopyAndPaste              (void);
OSErr DoOpenText(Boolean editable);
OSStatus DoOpen(void);
OSErr OldDoOpen(void);
OSErr DoSource(void);
OSErr SourceFile(FSSpec  	*myfss);
int GetTextSize(void);
int GetScreenRes(void);

void consolecmd(char *cmd);
static pascal void NavEventFilter(NavEventCallbackMessage,NavCBRec *,void *);
static OSStatus CreateNavTypeList(OSType,UInt16,const OSType *,NavTypeListHandle *);
void DoHelpChoice(SInt16 menuItem);
extern void Do_HelpOnTopic(void);
extern void Do_RunExample(void);
extern void Do_SearchHelp(void);


/*    Extern Global variables   */
extern  void   doWindowsMenu             (SInt16);
extern  void   doConfigMenu             (SInt16);
extern  void   changeGWinPtr             (WindowPtr, Str255);
extern  void   *dlopen                   (const char*, int);
extern  void   savePreference            (void);
extern void DoGenKeyDown (const EventRecord *event, Boolean Console);
extern Boolean EqualNumString(Str255 Str1, Str255 Str2, SInt16 Num);
extern void adjustEditPtr(SInt16 EditIndex);
extern void adjustHelpPtr(SInt16 HelpIndex);
extern pascal	OSErr	FSpGetFullPath (const FSSpec*, short*, Handle*);
extern void LoadWindow();
extern void DoLineTo();
extern void Do_EditObject();
/*extern void PrintPicture();*/
extern void printLoop(WindowPtr	window);
void DoPaste(WindowPtr window);

extern void DoUpdate (WindowPtr window);
extern void DoActivate (Boolean isActivating, WindowPtr window);
extern void Do_About();

void DoTools(SInt16 menuItem);
SavingOption DoWeSaveIt(WindowPtr window);

OSErr MyOpenDocument(FSSpec *documentFSSpec, SFTypeList *typeList);

/*    enum     */
enum {
    kButtonSave	= 1,
    kButtonCancel,
    kButtonDontSave
};


/* SetDefaultDirectory
 */

void SetDefaultDirectory (const FSSpec * spec)
{
 //   LMSetCurDirStore (spec->parID);
 //   LMSetSFSaveDisk (- spec->vRefNum);
}


/* MySFDialogFilter
 */
static pascal Boolean
MySFDialogFilter(DialogPtr dialog, EventRecord *event, SInt16 *item,
		 void *yourData)
{
#pragma unused (item, yourData)

    /*    intercept window events directed to windows behind the dialog */
    if ((event->what == updateEvt) || (event->what == activateEvt)) {
	if ((WindowPtr) event->message != GetDialogWindow(dialog)) {
	    DoWindowEvent(event);
	}
    }
    return false;
}


/* ModalFilterYDUPP
 */
static ModalFilterYDUPP GetMySFDialogFilter(void)
{
#ifdef __cplusplus
    static ModalFilterYDUPP sFilterUPP =
	NewModalFilterYDProc(MySFDialogFilter);
#else
    static ModalFilterYDUPP sFilterUPP = nil;
    if (sFilterUPP == nil) {
	//sFilterUPP = NewModalFilterYDProc(MySFDialogFilter);
    }
#endif
    return sFilterUPP;
}


/* FindMenuItemText
 */
SInt16 FindMenuItemText(MenuHandle menu, ConstStr255Param stringToFind)
{
    SInt16      item;
    Str255      itemString;

    for (item = CountMenuItems(menu); item >= 1; item--) {
	GetMenuItemText(menu, item, itemString);
	if (EqualString(itemString, stringToFind, false, false))
	    break;
    }
    return item;
}


/* EqualColor : use to maintain the color menus
 */
Boolean  EqualColor(const RGBColor *rgb1, const RGBColor *rgb2)
{
    return ((rgb1->red == rgb2->red) && (rgb1->green == rgb2->green) && (rgb1->blue == rgb2->blue));
}


/* PrepareMenus:
   Used to maintain which menus item ought to be enabled, and which is
   disabled.
   This function will be called when you use a mouse and click on the
   menu bar.
 */
void PrepareMenus(void)
{
    WindowPtr		window;
    WEReference		we;
    MenuHandle		menu=NULL;
    MenuCRsrcPtr	pColors;
    SInt16		item;
    Str255		itemText;
    SInt32		selStart, selEnd;
    SInt32		threshold;
    WEActionKind	actionKind;
    WEStyleMode		mode;
    TextStyle		ts;
    Boolean		temp;
    SInt32		scrapOffset;
    SInt16		i;
    Str255		Cur_Title, Menu_Title;
    MenuHandle		windowsMenu=NULL;
    OSStatus		err;

    /* get a pointer to the frontmost window, if any
     */
    window = FrontWindow ();

    /* get associated WE instance
     */
    we = (window != nil) ? GetWindowWE (window) : nil;


	
    /* *** FILE MENU ***
     */
    menu = GetMenuHandle (kMenuFile);

    /* first disable all items
     */
    for (item = CountMenuItems (menu); item >= 1; item --) {
	DisableMenuItem (menu, item);
    }
    if (isGraphicWindow(window)){
	SetMenuItemText(menu,kItemOpen, "\pActivate Graphic Device");
	SetMenuItemText(menu,kItemNew, "\pNew Graphic Device");
	SetItemCmd(menu, kItemOpen, 'A');
    SetItemCmd(menu, kItemNew, 'N');
    }
    else{
	SetMenuItemText(menu,kItemOpen, "\pSource File...");
	SetMenuItemText(menu,kItemNew, "\pNew Edit Window");
	SetItemCmd(menu, kItemOpen, 'O');
    SetItemCmd(menu, kItemNew, 'N');
    }

    /* New, Open, and Quit are always enabled
     */
    if (isEditWindow(window)){
	EnableMenuItem (menu, kItemLoadW);
	EnableMenuItem (menu, kItemSaveWSAs);
	EnableMenuItem (menu , kItemEditObject);
    }

    if (window == Console_Window)
    {
	EnableMenuItem (menu, kItemLoadW);
    EnableMenuItem (menu, kItemSaveWSAs);
    EnableMenuItem (menu , kItemEditObject);
    }
    EnableMenuItem (menu, kItemNew);
    EnableMenuItem (menu, kItemOpen);
    EnableMenuItem (menu, kItemEditFile);
    EnableMenuItem (menu, kItemShow);
    EnableMenuItem (menu, kItemPageSetup);
    EnableMenuItem (menu, kItemPrint);
    EnableMenuItem (menu, kItemQuit);

    /* Enable "Close" and "Save As" if there is an active window
     */
    if (window != nil) {
	EnableMenuItem (menu, kItemClose);
	if (!isHelpWindow(window)) {
	    EnableMenuItem (menu, kItemSaveAs);
	    EnableMenuItem (menu, kItemSave);
	}
	if(isGraphicWindow(window) || isHelpWindow(window))
	 DisableMenuItem(menu,kItemSave);
    }

    /* *** EDIT MENU ***
     */
    menu = GetMenuHandle (kMenuEdit);

    /* first, disable all items
     */
    for (item = CountMenuItems(menu); item >= 1; item--) {
	DisableMenuItem(menu, item);
    }
    if (isGraphicWindow(window)) {
	EnableMenuItem (menu, kItemCopy);
    }

    /* by default, the Undo menu item should read "Can't Undo"
     */
    GetIndString(itemText, kUndoStringsID, 1);
    SetMenuItemText(menu, kItemUndo, itemText);

    if (window != nil) {

	/* enable Paste if there's anything pasteable on the Clipboard
	 */
	 
       we = GetWindowWE ( window ) ;

		//	enable Paste if there's anything pasteable on the Clipboard
		if ( WECanPaste ( we ) ){
		 if (!isGraphicWindow(window))
    	  EnableMenuItem ( menu, kItemPaste ) ;
		}


	/* enable Undo if anything can be undone
	 */
	actionKind = WEGetUndoInfo (&temp, we);

	if (actionKind != weAKNone) {
	    EnableMenuItem (menu, kItemUndo);

	    /* change the Undo menu item to "Undo/Redo"
	       + name of action to undo/redo
	     */
	    GetIndString (itemText, kUndoStringsID, 2 * actionKind + temp);
	    SetMenuItemText (menu, kItemUndo, itemText);
	}

	/* enable Select All if there is anything to select
	 */
	if (WEGetTextLength (we) > 0)
	{
	    EnableMenuItem (menu, kItemSelectAll);
	}

	/* get the current selection range
	 */
	WEGetSelection (&selStart, &selEnd, we);

	/* enable Cut, Copy, and Clear if the selection range is not empty
	 */
	if (FrontWindow() == Console_Window) {
	    EnableMenuItem (menu, kItemCopyPaste);
	    EnableMenuItem (menu, kItemCopy);
	    EnableMenuItem (menu, kItemCopyPaste);
	}

	if (isEditWindow(window)) {
	    EnableMenuItem (menu, kItemLineTo);
	    EnableMenuItem (menu, kItemCut);
	    EnableMenuItem (menu, kItemCopy);
	    EnableMenuItem (menu, kItemClear);
	    EnableMenuItem (menu, kItemPaste);
	    EnableMenuItem (menu, kItemCopyPaste);
	}
	if (isHelpWindow(window)){
	    DisableMenuItem (menu, kItemCut);
	    DisableMenuItem (menu, kItemCopyPaste);
	    DisableMenuItem (menu, kItemClear);
	    DisableMenuItem (menu, kItemPaste);
	    EnableMenuItem  (menu, kItemCopy);
	    EnableMenuItem 	(menu, kItemCopyPaste);
	}
	/* determine which style attributes are continuous over
	   the current selection range we'll need this information
	   in order to check the Font/Size/Style/Color menus properly
	*/
	mode = weDoAll;  /* query about all attributes
			  */
	WEContinuousStyle (&mode, &ts, we);

    } else {
	mode = 0;        /* no window, so check no items */
    }

    /* *** Window Menu *** */
    windowsMenu = GetMenuHandle(kMenuWindows);
    GetWTitle(window, (unsigned char *) &Cur_Title);

    for(i = 1; i <= CountMenuItems(windowsMenu); i++) {
	GetMenuItemText(windowsMenu, i , (unsigned char*)&Menu_Title);
	CopyPascalStringToC(Cur_Title,wTitle);
    CopyPascalStringToC(Menu_Title,mTitle);    
	CheckMenuItem(windowsMenu, i, false);
	if (strcmp(wTitle,mTitle) == 0) 
		CheckMenuItem(windowsMenu, i, true);
    }

    /* The config menu */ 
    windowsMenu = GetMenuHandle(kMenuConfig);
    if( OnOpenSource )
      CheckMenuItem(windowsMenu, kItemOnOpenSource, true);
    else
      CheckMenuItem(windowsMenu, kItemOnOpenSource, false);
      
  if( Interrupt )
      CheckMenuItem(windowsMenu, kItemAllowInterrupt, true);
    else
      CheckMenuItem(windowsMenu, kItemAllowInterrupt, false);
      

}


void DoDeskAcc ( UInt16 menuItem )
{
/*	if ( menuItem == kItemAbout )
	{
		DoAboutBox( kDialogAboutBox );
	}
*/
#if ! TARGET_API_MAC_CARBON
	else
	{
		//	open desk accessories (this is not required under Carbon)
		Str255 deskAccessoryName ;

		GetMenuItemText ( GetMenuHandle ( kMenuApple ), menuItem, deskAccessoryName ) ;
		OpenDeskAcc ( deskAccessoryName ) ;
	}
#endif
}

/* DoNew
 */
OSErr DoNew(Boolean editable)
{
    /* create a new window from scratch
     */

    return CreateWindow(nil,editable);
}



/* This routine returns a FSSpec with corresponding error. The second
   argument is ignored for the moment. This routine is used to Source
   files and Show files.
   Jago, April 2001, Stefano M.Iacus
*/   


OSErr MyOpenDocument(FSSpec *documentFSSpec, SFTypeList *typeList)
{
    NavDialogOptions    dialogOptions;
    NavEventUPP         eventProc = nil; 
    NavObjectFilterUPP  filterProc = nil;
    OSErr               anErr = noErr;
    
    
    /*  Specify default options for dialog box */
    anErr = NavGetDefaultDialogOptions(&dialogOptions);
  
         
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
            NavTypeListHandle deftypeList = (NavTypeListHandle)GetResource(
                                       'open', 128);     
          deftypeList = nil; /* we apply no filter for the moment */
            
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
                                            &actualType,documentFSSpec,
                                            sizeof(FSSpec),
                                            &actualSize);
                             
                        
                    }
                }
                /*  Dispose of NavReplyRecord, resources, descriptors */
                NavDisposeReply(&reply);
            }
            if (typeList != NULL)
            {
                ReleaseResource( (Handle)typeList);
            }
        }
    }

cleanup:  
      return anErr;
}

/* DoOpen has been updated to use NavServices 
   Jago, April 2001, Stefano M.Iacus
*/

OSStatus DoOpen ( void )
{
	OSErr		err ;
    FSSpec  	myfss;
    SInt16		pathLen;
    Handle		pathName=NULL;
    FILE		*fp;
    SEXP 		img, lst;
    int 		i;
    SFTypeList	typeList;

    typeList[0] = kTypeText;
    typeList[1] = 'BINA';

    err = MyOpenDocument(&myfss, &typeList);
        
    if(err!= noErr)
       return(err);
     
    FSpGetFullPath(&myfss, &pathLen, &pathName);
    HLock((Handle)pathName);
    strncpy(InitFile, *pathName, pathLen);
    InitFile[pathLen] = '\0';
    HUnlock((Handle) pathName);
/*
   Routine now handles XDR object. Nov 2000 (Stefano M. Iacus)
*/
    if(!(fp = fopen(InitFile, "rb"))) { /* binary file */
	warning("File cannot be opened !");
	/* warning here perhaps */
	return;
    }
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
	    lst = VECTOR_ELT(img,i);
	    while (lst != R_NilValue) {
		defineVar(TAG(lst), CAR(lst), R_GlobalEnv);
		lst = CDR(lst);
	    }
	}
	break;
    }
    UNPROTECT(1);
    fclose(fp);


    return(err);

}

/* This routine is responsible to showing (and eventually edit)
   files selected by the user
   Jago, April 2001, Stefano M. Iacus
*/    
OSErr DoOpenText(Boolean editable)
{
    FInfo		fileInfo;
    SFTypeList	typeList;
    OSErr		err = noErr;
    FSSpec  	myfss;

    typeList[0] = kTypeText;
    typeList[1] = ftSimpleTextDocument;
 
    err = MyOpenDocument(&myfss, &typeList);
 
    if(err!= noErr)
     return(err);
    
    err = FSpGetFInfo(&myfss, &fileInfo);
    if (err != noErr) return err;

    DoNew(editable);
       
    RemWinMenuItem();
   
    err = ReadTextFile(&myfss,
		       GetWindowWE(Edit_Windows[Edit_Window-1]));
    
    
//    if(err != noErr)
//	 REprintf("\n ReadTextFile error: %d\n",err);
   	
   	UniqueWinTitle();
	if(Edit_Window>2)
    	RepositionWindow(Edit_Windows[Edit_Window - 1], 
        Edit_Windows[Edit_Window - 2],kWindowCascadeOnParentWindow);

    SetWindowProxyFSSpec(Edit_Windows[Edit_Window - 1], &myfss);
    
    return err;
}

/* DoSource: this routine sources .R files. It is accessibile via menus.
   Jago, January 2001, Stefano M.Iacus
 */
OSErr DoSource(void)
{
    SFTypeList	typeList;
    OSErr		err = noErr;
    FSSpec  	myfss;

    typeList[0] = kTypeText;
    typeList[1] = ftSimpleTextDocument;

    err = MyOpenDocument(&myfss, &typeList);
  
    if(err!= noErr)
      return(err);
   
    return( SourceFile(&myfss) );

}

/* SourceFile: is a completion to DoSource routine. 
   It allows the Alpha editor to run scripts in R
   in the S+/R tcl-mode. 
   Jago Easter 2001, Stefano M. Iacus
*/
   
OSErr SourceFile(FSSpec  	*myfss)
{
 	OSErr		err = noErr;
    char 		sourcefile[FILENAME_MAX];
    char 		cmd[FILENAME_MAX+25];
    SInt16		pathLen;
    Handle		pathName=NULL;
    FInfo		fileInfo;

    if(myfss == NULL)
     return(-1);
     
    err = FSpGetFInfo(myfss, &fileInfo);
    if (err != noErr) return err;
    FSpGetFullPath(myfss, &pathLen, &pathName);
    HLock((Handle)pathName);
    strncpy(sourcefile, *pathName, pathLen);
    sourcefile[pathLen] = '\0';
    HUnlock((Handle) pathName);

    sprintf(cmd,"source(\"%s\")",sourcefile);

    consolecmd(cmd);


}


/* SaveWindow: routine updated to use Navigation Services
   Jago, April 2001, Stefano M. Iacus
 */
OSErr SaveWindow(const FSSpec *pFileSpec, WindowPtr window)
{
    DocumentHandle	hDocument;
    AliasHandle		alias = nil;
    int			menu_item=-1,i;
    OSErr		err;
    MenuHandle 		windowsMenu;
    Str255		Cur_Title,curString;
    Boolean		EqString;

    if(window==NULL)
     window = FrontWindow();
     
    hDocument = GetWindowDocument(window);


    if (isGraphicWindow(window)) {
	/* We don't save these here... */
    }
    else {
	GetWTitle(window, Cur_Title);
	windowsMenu = GetMenuHandle(kMenuWindows);
	for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	    GetMenuItemText(windowsMenu, i , curString);
	    EqString = EqualNumString(Cur_Title, curString, curString[0]);
	    if (EqString) {
		menu_item = i;
		break;
	    }
	}

	/* save the text */
	if ((err = WriteTextFile(pFileSpec, (*hDocument)->we)) == noErr) {
	    if(window != Console_Window){
	    SetWTitle(window, pFileSpec->name);
	    if(menu_item!= -1)
	    {
		DeleteMenuItem(windowsMenu, i);
		AppendMenu(windowsMenu, pFileSpec->name);
	    }

	
		    /* replace the old window alias (if any) with a new one
	       created from pFileSpec
	     */
	    NewAlias(nil, pFileSpec, &alias);
	   } 

	}
    }
    
    return err;
}

OSStatus DoSave ( WindowPtr window )
{
	FSSpec		spec;
	OSStatus	err;

	//	get the file spec associated with this window, if any
	if ( ( err = GetWindowProxyFSSpec( window, & spec )) == noErr )
	{
		err = SaveWindow ( & spec, window ) ;
	}
	else
	{
		//	if no file was previously associated with this window, or if the
		//	alias resolution failed, prompt the user for a new destination
		err = DoSaveAs (nil, window ) ;
	}

	return err;
}


/* DoSaveAs
 */
OSErr DoSaveAs(const FSSpec *suggestedTarget, WindowPtr window)
{
    StringHandle	hPrompt;
    Str255		defaultName;
    Point		where = { -1, -1 }; /* autocenter's dialog */
    OSErr		err = noErr;
    OSErr               anErr = noErr;
    NavReplyRecord      reply;
    NavDialogOptions    dialogOptions;
    OSType              fileTypeToSave = 'TEXT';
      OSType              creatorType = 'ttxt';
    FSSpec 				mytarget; 
 	FInfo						finderInfo ;
 
    hPrompt = GetString(kPromptStringID);
    HLockHi((Handle) hPrompt);

    if (suggestedTarget != nil) {
	/* if a suggested target file is provided,
	   use its name as the default name
	*/
	PStringCopy(suggestedTarget->name, defaultName);
	SetDefaultDirectory(suggestedTarget);
    }
    else {
	/* otherwise use the window title
	   as default name for CustomPutFile
	*/
	GetWTitle(window, defaultName);
    }

    /* put up the standard Save dialog box
     */
    anErr = NavGetDefaultDialogOptions(&dialogOptions); 
    dialogOptions.dialogOptionFlags |= kNavSelectDefaultLocation;

	PStringCopy(defaultName,dialogOptions.savedFileName);
      
    
    anErr = NavPutFile( nil, 
    					&reply, 
    					&dialogOptions, 
    					nil,
                        fileTypeToSave, 
                        creatorType, 
                        nil );
 
    if (anErr == noErr && reply.validRecord)
            {
                        AEKeyword   theKeyword;
                        DescType    actualType;
                        Size        actualSize;
                        
                        /* Get a pointer to selected file */
                        anErr = AEGetNthPtr(&(reply.selection), 1,
                                            typeFSS, &theKeyword,
                                            &actualType, &mytarget,
                                            sizeof(mytarget),
                                            &actualSize);
                             
             err = SaveWindow(&mytarget, window);
             
                /*  Dispose of NavReplyRecord, resources, descriptors */
                NavDisposeReply(&reply);
            }
    else
	 err = userCanceledErr;
    
    HUnlock((Handle)hPrompt);
    
    if(err == noErr)
     SetWindowProxyFSSpec(window,&mytarget);

    return err;
}


 
/* DoClose:
   In here, you need to understand that different windows have
   different close methods.  For a Graphic window, when you close it, you
   need to tell R to remove the corresponding device.  For aConsole
   window, when you close, it implies that you close the
   application. Thus, you need to ensure that the application closed.
   For Console and Edit windows, when you close, you need to ask
   whether they want to save the content or not, but for graphic
   window, it is not necessary.
 */

OSErr DoClose(ClosingOption closing, SavingOption saving, WindowPtr window)
{
    Str255	s1, s2;
    SInt16	alertResult,win_num,i;
    OSErr	err;
    Boolean	haveCancel;
    Str255      Cur_Title,curString;
    MenuHandle  windowsMenu;
    Boolean 	EqString = FALSE;
    Cursor 	arrow;
    static ModalFilterUPP sFilterProc = nil;
     
     



    SelectWindow(window);

    if ( (win_num=isHelpWindow(window)) ) {

		GetWTitle(Help_Windows[win_num], Cur_Title);
		//SelectWindow(Help_Windows[win_num]);
		RemWinMenuItem();
		adjustHelpPtr(win_num);
		DestroyWindow(window);
		return noErr;
    }
    
    err = noErr;
    
    
    if(!finished){
     saving = DoWeSaveIt(window);
     if(saving == savingCancel)
      return;
     if(saving == savingNo){
       finished=true; 
       goto furtherstep;
       }
     goto nextstep;
     }
     
     if(isGraphicWindow(window)) 
      goto furtherstep;
    /* is this window dirty?
     */
    if (WEGetModCount(GetWindowWE(window)) > 0) {
	/* do we have to ask the user whether to save changes?
	 */
	if (saving == savingAsk) {
	    /* prepare the parametric strings to be used in the
	       Save Changes alert box
	     */
	    GetWTitle(window, s1);
	    GetIndString(s2, kClosingQuittingStringsID, 1 + closing);
	    ParamText(s1, s2, nil, nil);

	    /* put up the Save Changes? alert box
	     */
	    
	    SetCursor ( GetQDGlobalsArrow ( & arrow ) ) ;
	    alertResult = Alert(kAlertSaveChanges, sFilterProc);

	    /* exit if the user canceled the alert box
	     */
	    if (alertResult == kButtonCancel)
		return userCanceledErr;

	    if (alertResult == kButtonSave)
		saving = savingYes;
	    else
		saving = savingNo;
	}

nextstep:
	if (saving == savingYes) { 
	    if (isGraphicWindow(window)) 
			err = doSaveAsGraCommand();
		else { /* isGraphic */
		      	if (window == Console_Window) {
		    		err = doRSave(&haveCancel);
		    		if (haveCancel){
						jump_to_toplevel();
		    		}
		    		else {
						if (err == noErr){
			    			R_SaveGlobalEnv();
						}
						else
			    			error("File Corrupt or Memory error. Unrecoverable!");
		    		}
				}
				else {
		    		if(saving != savingNo){
		     		if(!finished){
		     		 err= SaveWindow(&tempeditFSS, window);
		     		 finished=true; 
		     		 
		     		}
		     		else
		     		 DoSave(window); 
		    		}
				}
	    }
	}
    }

furtherstep:
    /* if it is a graphic window, maintain the menus and title first.
     */
    if (isGraphicWindow(window)){
		Mac_Dev_Kill(window); /* Mac_Dev_Kill provides menu deletion */
		return noErr;
	}
    else {
	if ( (win_num=isEditWindow(window)) ) {
	    RemWinMenuItem();
		adjustEditPtr(win_num);
	}
	/* destroy the window */
	
	DestroyWindow(window);
	if (window == Console_Window)
	    ExitToShell();
    }
    return err;
}

SavingOption DoWeSaveIt(WindowPtr window)
{
 SavingOption saving = savingNo;
     Str255	s1, s2;
    SInt16	alertResult,win_num,i;
    OSErr	err;
    Boolean	haveCancel;
    Str255      Cur_Title,curString;
    MenuHandle  windowsMenu;
    Boolean 	EqString = FALSE;
    Cursor 	arrow;
    static ModalFilterUPP sFilterProc = nil;


 	if(!window)
  		return(savingNo);
  
 	if (WEGetModCount(GetWindowWE(window)) > 0) {
	
	    /* put up the Save Changes? alert box
	     */
	    
	    SetCursor ( GetQDGlobalsArrow ( & arrow ) ) ;
	    alertResult = Alert(kAlertSaveObject, sFilterProc);

	    /* exit if the user canceled the alert box
	     */
	    if (alertResult == kButtonCancel)
	    	saving=savingCancel;
        else
	     if (alertResult == kButtonSave)
		 	saving = savingYes;
	    else
		saving = savingNo;
       
       
	}

	return(saving);
}

void MacFinalCleanup()
{
    WindowPtr window;
    OSErr err;
    SavingOption saving = 1;

    /* Close all windows
       query the user about contents
       if "saving" is non-zero.
    */


    do {
	if ((window = FrontWindow()) != nil) {
	    if ((err = DoClose(closingApplication, saving, window)) != noErr) {
		return;
	    }
	}
    }
    while (window != nil);
    /* set a flag so we drop out of the event loop */
    ExitToShell();
}

/* MySFDialogFilter
 */
OSErr DoQuit(SavingOption saving)
{
	WindowPtr	window;
	OSErr		err;

	/* Close all windows
	 query the user about contents
	 if "saving" is non-zero.
	*/

	do {
		if ((window = FrontWindow()) != nil) {
			if ((err = DoClose(closingApplication, saving, window)) != noErr) {
				return err;
			}
		}
	}
	while (window != nil);

	/* set a flag so we drop out of the event loop
	*/
	ExitToShell();
	return noErr;
}

void DoHelpChoice(SInt16 menuItem)
{

  	
  if(menuItem == RHelpMenuItem){
   	consolecmd("help.start()");
    return;
   }
   
  if(menuItem == RTopicHelpItem){
    Do_HelpOnTopic();
    return;
  }
  
   if(menuItem == SearchHelpItem){
    Do_SearchHelp();
    return;
  }

  if(menuItem == RunExampleItem)
    Do_RunExample();
  
}

/* DoAppleChoice :
   Which will be called when you click on the apple icon on the top
   left corner.  When you choice 'R about', it will open the predefined
   html files and launch Netscape.
 */
void DoAppleChoice(SInt16 menuItem)
{
    StandardFileReply        fileReply;
    SFTypeList               fileTypes;
    OSErr                    launchErr;

    if (menuItem == kItemAbout){
	Do_About();
#ifdef  Preference
	/* Test whether the predefine file exists or not.
	   If not, you need to prompt out the stardand
	   Get file dialog to query the user, where they put the file.
	*/
	launchErr = FSpSetFLock(&gHelpFile);
	/* File not found with a reference number -43,
	   However, I would like to check more general.
	   If you have any err, then, you need to select it
	   by yourself
	*/
	if (launchErr){
	    GWdoErrorAlert(eNoSuchFile);
	    fileTypes[0] = 'TEXT';
	    StandardGetFile(nil,1,fileTypes,&fileReply);
	    if (fileReply.sfGood){
		gHelpFile = fileReply.sfFile;
				/* savePreference(); */
		/* not sure whether it is necessary or not. */
		OpenSelection(&gHelpFile);
	    }
	    else
		GWdoErrorAlert(eSelectNextTime);
	}
	else{
	    launchErr = FSpRstFLock(&gHelpFile);
	    launchErr = OpenSelection(&gHelpFile);
	}
#endif
    }
    else
	DoDeskAcc(menuItem);
    /* Do another normal choice, like "control panel' */
}


/* DoFileChoice:

   Which is used to deal with the menus choice on under "File" We still
   have no agreement in this moment. Those I don't know when to chance
   the codes about doNew, DoOpen, DoClose and DoSaveAs.
 */
void DoFileChoice(SInt16 menuItem, WindowPtr window)
{
//    WindowPtr	window = FrontWindow();
    OSErr	osError, err;
    EventRecord	myEvent;
    SInt16	WinIndex;
    Boolean	haveCancel;


    if(window == NULL)
     window = FrontWindow();

    switch(menuItem) {
    
    case kItemNew:
	if (isGraphicWindow(window)) {
	    RWrite("macintosh()\r");
	    myEvent.message = 140301;
	    DoGenKeyDown (&myEvent, true);
	}
	else
	    DoNew(true);
	break;

    case kItemShow:
		DoOpenText(false);
	break;

	case kItemEditFile:
		DoOpenText(true);
	break;
    
    case kItemOpen:
	if (isGraphicWindow(window)){
	    WinIndex = isGraphicWindow(window);
	    selectDevice(deviceNumber((DevDesc *)gGReference[WinIndex].devdesc));
	}
	else
	    DoSource();  
	break;


    case kItemEditObject:
		Do_EditObject();
	break;

    case kItemLoadW:
	if (isEditWindow(window)){
	    LoadEditEnvironment();
	    LoadWindow();
	}
	if (window == Console_Window) {
	    err = DoOpen();
	    if (err)
		jump_to_toplevel();
	}
	break;

	case kItemSaveWSAs:
       err = doRSaveAs(&haveCancel);
		if (haveCancel){
		   // RWrite("\r");
		    jump_to_toplevel();
		}
 	break;

    case kItemClose:
		DoClose(closingWindow, savingAsk, window);
	break;

    
    
    case kItemSave:
     DoSave(window);
    break;
    
    case kItemSaveAs:
	if( isGraphicWindow(window) ) 
	    doSaveAsGraCommand();
	else{
	    if(!finished)
	     SaveWindow(&tempeditFSS,window);
	    else
  		 DoSaveAs(nil, window);
	}
	break;	

    case kItemPageSetup:
		DoPageSetup();
	break;

    case kItemPrint:
		do_Print();
	break;

    case kItemQuit:
		err = DoQuit(savingAsk);
	break;
    }
    HiliteMenu(0);
}


/* DoEditChoice:

   Which is dsigned to handle the Menus choice about "Edit". We have
   different implemenation on different kind of window.  In the
   Preference, you can assign the tab size, the number of History command
   that you can keep and .....
 */
void DoEditChoice(SInt16 menuItem)
{
    WindowPtr		window;
    WEReference		we;
    OSStatus err;
    SInt32		selEnd, selStart, i;
    long		scrapOffset, rc;
    EventRecord		myEvent;
    char		TempChar;
    ScrapRef scrap;
     Size scraplength;
    /* do nothing if no window is active
     */
    if ((window = FrontWindow()) == nil) {
	return;
    }

    we = GetWindowWE(window);

    switch (menuItem) {

    case kItemUndo:
		WEUndo(we);
	break;

    case kItemCut :
	if (window == Console_Window) {
	    WEGetSelection(&selStart, &selEnd, we);
	    if (inRange(selStart, selEnd, gChangeAble -1, gChangeAble - 1))
		SysBeep(10);
	    else{
		if (selStart <= gChangeAble)
		    gChangeAble = gChangeAble + selStart - selEnd;
		WECut(we);
	    }
	}
	else
	    WECut(we);
	break;

    case kItemCopy:
	if (isGraphicWindow(window)) {
	    GraphicCopy(window);
	}
	else
	    WECopy(we);
	break;

    case kItemPaste:
     DoPaste(window);	
	break;

    case kItemClear:
	if (window == Console_Window) {
	    WEGetSelection(&selStart, &selEnd, we);
	    if (inRange(selStart, selEnd, gChangeAble -1, gChangeAble - 1))
		SysBeep(10);
	    else{
		if (selStart <= gChangeAble)
		    gChangeAble = gChangeAble + selStart - selEnd;
		WEDelete(we);
	    }
	}
	else
	    WEDelete (we);
	break;

    case kItemCopyPaste:
		ConsoleCopyAndPaste();
	break;

    case kItemSelectAll:
		WESetSelection(0, LONG_MAX, we);
	break;

    case kItemLineTo:
		DoLineTo();
	break;
/*
  case kItemPreference:
  DoPreference(kDialogAboutBox);
  break;
*/
    }
}


void DoPaste(WindowPtr window)
{
    WEReference	we=nil;
    OSStatus 	err;
    ScrapRef 	scrap;
    Size 		scraplength;
    int 		last =0,i;
    EventRecord	myEvent;
    char		TempChar;
    char 		*buffer;
    char 		strerr[100];
    
	if(window ==NULL)
		return;


    if (isGraphicWindow(window) != 0)
     return;
    
    we = GetWindowWE(window);	
	 
	if(window != Console_Window){
		 WEPaste(we);
		 return;
	}
	
		
	WESetSelection(WEGetTextLength(we), WEGetTextLength(we), we);
 	    
	if( (err = GetCurrentScrap(&scrap)) != noErr)
	 return;
	 

    if( (err = GetScrapFlavorSize(scrap,kScrapFlavorTypeText,&scraplength)) != noErr)
      return;
        
	if (scraplength < 1)
	    	return;		
	 
	ReserveMem(scraplength+1);
	err = MemError();
	if(err != noErr)
	 return;
	     	
	myHandle = NewHandle(scraplength+1);
	if( (err = MemError()) != noErr)
	 return;
	 	    
	if( (err = GetScrapFlavorData(scrap,kScrapFlavorTypeText, &scraplength, *myHandle)) != noErr)
	   return;
	        
	    
	HLock(myHandle);
	(*myHandle)[scraplength] = 0;				/* make it ASCII */
	finalPaste = scraplength;
	for (i = 0; i <= scraplength; i++) {
	    	//if( (*myHandle)[i] == '\n' )  (*myHandle)[i] = '\r';
	    TempChar = (*myHandle)[i];
	    if ((TempChar == '\r') || (i == finalPaste)){
			RnWrite(*myHandle, i);
			if (i != finalPaste) {
		    	myEvent.message = 140301;
		    	DoKeyDown(&myEvent);
			}
			curPaste = i;
			break;
	    }
	}
	finalPaste = scraplength;
	HUnlock(myHandle);
	if (scraplength != i) 
		HaveContent = true;
	else 	
		DisposeHandle(myHandle);
}

/* Only resolutions 72, 144, 300 and 600 are allowable */
int GetScreenRes(void)
{
   gScreenRes = atoi(mac_getenv("ScreenRes"));

  if( gScreenRes != 72 && gScreenRes != 144 &&
      gScreenRes != 300 && gScreenRes != 600)  
    gScreenRes = 72;
  return(gScreenRes); 
}

int GetTextSize(void)
{
   gTextSize = atoi(mac_getenv("TextSize"));

  if(gTextSize < 8 || gTextSize > 14)  
  	 gTextSize = 12;
  return(gTextSize);
}

void changeSize(WindowPtr window, SInt16 newSize)
{

    if(window)
     WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontSize,
      & newSize, sizeof ( Fixed ),	GetWindowWE ( window ) );
}

/* DoTools:

   Some usefule shortcuts
   
 */
void DoTools(SInt16 menuItem)
{
    WindowPtr	window = FrontWindow();
    OSErr	osError, err;
    EventRecord	myEvent;
    SInt16	WinIndex;
    Boolean	haveCancel;

    switch(menuItem) {
    
    case kItemShowWSpace:
    	consolecmd("ls()");
	break;
 
    case kItemClrWSpace:
    	consolecmd("rm(list=ls())");
	break;
  
    case kItemLoadWSpace:
    	consolecmd("load(\".RData\")");
    break;

    case kItemSaveWSpace:
    	consolecmd("save.image()");
    break;

    case kItemLoadHistory:
    	consolecmd("loadhistory()");
    break;

    case kItemSaveHistory:
    	consolecmd("savehistory()");
    break;
    
    case kItemShowHistory:
    	consolecmd("history()");
    break;

    case kItemChangeDir:
		DoSelectDirectory();
   	break;
 	
 	case kItemShowDir:
		consolecmd("getwd()");
   	break;

	case kItemResetDir:
		consolecmd("setwd(R.home())");
   	break;
   	
   	case kItemShowLibrary:
		consolecmd("library()");
   	break;

   	case kItemShowData:
		consolecmd("data()");
   	break;
   	

    }
    HiliteMenu(0);
}


/* DoMenuChoice:
   The main function on RMenus.c, it is used to handle where to dispatch
   the event to the corresponding procedure to handle the corresponding
   menus choice. Thus, if you need some Menus choice, it is the best
   place to start with.
 */
void DoMenuChoice(SInt32 menuChoice, EventModifiers modifiers, WindowPtr window)
{
    SInt16	menuID, menuItem;

    // extract menu ID and menu item from menuChoice

    menuID = HiWord(menuChoice);
    menuItem = LoWord(menuChoice);

    // dispatch on menuID
    switch (menuID) {

	
    case kMenuApple:
	DoAppleChoice(menuItem);
	break;

    case kMenuFile:
	DoFileChoice(menuItem,window);
	break;

    case kMenuEdit:
	DoEditChoice(menuItem);
	break;
	
	case kMenuTools:
	DoTools(menuItem);
	break;


    case kMenuWindows:
	doWindowsMenu(menuItem);
	break;
	
	case kMenuConfig:
	doConfigMenu(menuItem);
	break;

	case kHMHelpMenuID:  /* the help menu */
	DoHelpChoice(menuItem);
	break;


    }
    HiliteMenu(0);
}


/* InitializeMenus
 */
OSErr InitializeMenus(void)
{
    Handle		menuBar = nil ;
	MenuRef		menu ;
	OSErr 		err = noErr;
#if TARGET_API_MAC_CARBON
	ItemCount	submenuCount ;
	ItemCount	itemCount ;
	SInt32		gestaltResponse ;
#endif

 	//	get the 'MBAR' resource
	menuBar = GetNewMBar ( kMenuBarID ) ;
	if ( ( err = ResError ( ) ) != noErr )
	{
		goto cleanup ;
	}
	err = memFullErr ;
	if ( ! menuBar )
	{
		goto cleanup ;
	}

	//	install the menu bar
 	SetMenuBar ( menuBar ) ;

    /* Here we add some items to the Help Menu */
    HMGetHelpMenu(&HelpMenu,NULL);
	if (HelpMenu != nil) {
		AppendMenu(HelpMenu, "\pR Help");
		RHelpMenuItem=CountMenuItems(HelpMenu);
		SetItemCmd(HelpMenu,RHelpMenuItem,'?');
		AppendMenu(HelpMenu, "\pHelp On Topic...");
		RTopicHelpItem=CountMenuItems(HelpMenu);
   	    AppendMenu(HelpMenu,"\pSearch Help On...");
   	    SearchHelpItem=CountMenuItems(HelpMenu);
		AppendMenu(HelpMenu, "\pRun An Example...");
		RunExampleItem=CountMenuItems(HelpMenu);
	}

#if TARGET_API_MAC_CARBON
	if ( ( Gestalt ( gestaltMenuMgrAttr, & gestaltResponse ) == noErr ) &&
		 ( gestaltResponse & gestaltMenuMgrAquaLayoutMask ) )
	{
		if ( ( menu = GetMenuHandle ( kMenuFile ) ) != nil )
		{
			//	assume the Quit item is the last item in the File menu and follows a separator line
			itemCount = CountMenuItems ( menu ) ;
			if ( itemCount > 2 )
			{
				DeleteMenuItem ( menu, itemCount ) ;
				DeleteMenuItem ( menu, itemCount - 1 ) ;
			}
		}
	}
#else
	//	set up the Apple menu (this is not required under Carbon)
	if ( ( menu = GetMenuHandle ( kMenuApple ) ) != nil )
	{
		AppendResMenu ( menu, kTypeDeskAccessory ) ;
	}

#endif


 
    // draw the menu bar
    DrawMenuBar();
    err = noErr;
    
cleanup :
	ForgetHandle ( & menuBar ) ;
	return err ;
}


/* do_Print

  This routine has been completely rewritten.
  If it is a Graphic window then the printLoop
  function passes the control to the Graphic Printing
  procedure, otherwise it passes the contro to
  the DoTextPrint function to print the text 
  windows (Help, Console etc)
  Jago, April 2001, Stefano M. Iacus

*/

void do_Print(void)
{

    WindowPtr	window;
    WEReference	we;
    DevDesc		*dd;
    SInt16		WinIndex;
    GrafPtr		savePort;
    GrafPtr		picPort;
    Point		linesize;
    Point		*lp;
    Rect		portRect;
        
    linesize.h = 10;
    linesize.v = 7;
    lp = &linesize;
    window = FrontWindow();
    we = GetWindowWE(window);
    
    printLoop(window);
 }



extern	PMPageFormat	pageFormat;
extern	PMPrintSettings	printSettings;	
extern	PMPrintSession	printSession;
 

 
/* OpenSelection :
   Given a FSSpecPtr to either an application or a document,
   OpenSelection creates a finder Open Selection Apple event for the
   object described by the FSSpec.
 */
OSErr OpenSelection(FSSpecPtr theDoc)
{
    AppleEvent		aeEvent;		// the event to create;
    AEDesc		myAddressDesc;		// descriptors for the
    AEDesc		aeDirDesc;
    AEDesc		listElem;
    AEDesc		fileList;		// our list
    FSSpec		dirSpec;
    AliasHandle		dirAlias;		// alias to directory with our file
    AliasHandle		fileAlias;		// alias of the file itself
    ProcessSerialNumber	process;		// the finder's psn
    OSErr		myErr;			// duh

    // Get the psn of the Finder and create the target address for the .
    if(FindAProcess(kFinderSig,kSystemType,&process))
	return procNotFound;
    myErr = AECreateDesc(typeProcessSerialNumber,(Ptr) &process,
			 sizeof(process), &myAddressDesc);
    if(myErr)
	return myErr;

    // Create an empty
    myErr = AECreateAppleEvent(kAEFinderEvents, kAEOpenSelection,
			       &myAddressDesc, kAutoGenerateReturnID,
			       kAnyTransactionID, &aeEvent);
    if(myErr)
	return myErr;

    // Make an FSSpec and alias for the parent folder, and an alias for the file
    FSMakeFSSpec(theDoc->vRefNum,theDoc->parID,nil,&dirSpec);
    NewAlias(nil,&dirSpec,&dirAlias);
    NewAlias(nil,theDoc,&fileAlias);

    // Create the file list.
    if(myErr=AECreateList(nil,0,false,&fileList))
	return myErr;

    // Create the folder descriptor
    HLock((Handle)dirAlias);
    AECreateDesc(typeAlias, (Ptr) *dirAlias, GetHandleSize
		 ((Handle) dirAlias), &aeDirDesc);
    HUnlock((Handle)dirAlias);
    DisposeHandle((Handle)dirAlias);

    if((myErr = AEPutParamDesc(&aeEvent,keyDirectObject,&aeDirDesc)) == noErr) {
	AEDisposeDesc(&aeDirDesc);
	HLock((Handle)fileAlias);
	AECreateDesc(typeAlias, (Ptr)*fileAlias,
		     GetHandleSize((Handle)fileAlias), &listElem);
	HUnlock((Handle)fileAlias);
	DisposeHandle((Handle)fileAlias);
	myErr = AEPutDesc(&fileList,0,&listElem);
    }
    if(myErr)
	return myErr;
    AEDisposeDesc(&listElem);

    if(myErr = AEPutParamDesc(&aeEvent,keySelection,&fileList))
	return myErr;

    myErr = AEDisposeDesc(&fileList);

    myErr = AESend(&aeEvent, nil,
		   kAENoReply+kAEAlwaysInteract+kAECanSwitchLayer,
		   kAENormalPriority, kAEDefaultTimeout, nil, nil);
    AEDisposeDesc(&aeEvent);
}


/* FindAProcess
 */
OSErr FindAProcess(OSType typeToFind, OSType creatorToFind,
		   ProcessSerialNumberPtr processSN)
{
    ProcessInfoRec	tempInfo;
    FSSpec		procSpec;
    Str31		processName;
    OSErr		myErr = noErr;

    // start at the beginning of the process list
    processSN->lowLongOfPSN = kNoProcess;
    processSN->highLongOfPSN = kNoProcess;

    // initialize the process information record
    tempInfo.processInfoLength = sizeof(ProcessInfoRec);
    tempInfo.processName = (StringPtr)&processName;
    tempInfo.processAppSpec = &procSpec;

    while((tempInfo.processSignature != creatorToFind ||
	   tempInfo.processType != typeToFind) ||
	  myErr != noErr)
    {
	myErr = GetNextProcess(processSN);
	if (myErr == noErr)
	    GetProcessInformation(processSN, &tempInfo);
    }
    return(myErr);
}


/* assignPString :
   convert char* into unsigned char* with length howLong.
 */
void assignPString(unsigned char* input, char* buf, SInt16 howLong)
{
    SInt16	i;
    input[0] = howLong;
    for (i = 1; i <= howLong; i++){
	input[i] = buf[i - 1];
    }
}


/* ConsoleCopyAndPaste :
   A procedure which will be directly called when you choose
   Copy and Paste from the Menus. It copies from any text window
   to the R Console.
   Fixed on April 2001, Stefano M. Iacus
 */
void ConsoleCopyAndPaste()
{
    WEReference	we;
    SInt32		selStart, selEnd;

    we = GetWindowWE(FrontWindow());
	WECopy(we);
	SelectWindow(Console_Window);
    DoPaste(Console_Window);
    
}




/*
   This routine is intended to pass a command string to R Console
   Nothing special is done, just writing the command string 'cmd'
   as is and then send to the R application an Apple Event of
   carriage return.
   This is mainly tought to run "source" command fom the menu.
   Implemented on 9 Feb 2001, Stefano M. Iacus
*/

void consolecmd(char *cmd)
{
    long	cmdlen;
    EventRecord	myEvent;

    if((cmdlen = strlen(cmd))<1)
	return;

    SelectWindow(Console_Window);
    
    /* we just write the cmd as it is to the console */
    RnWrite(cmd, cmdlen);

    /* We send a '\r' event to the console */
    myEvent.message = 140301;
    DoKeyDown(&myEvent);

}


static pascal void NavEventFilter
	(
		NavEventCallbackMessage		inSelector,
		NavCBRec *					inPB,
		void *						inUserData
	)
{
	NavCallbackData *		cd = ( NavCallbackData * ) inUserData ;

	switch ( inSelector )
	{
		case kNavCBEvent :
		{
			EventRecord *	event = inPB -> eventData . eventDataParms . event ;

			//	intercept window events directed to windows behind the dialog
			if ( ( event->what == updateEvt ) || ( event->what == activateEvt ) )
			{
				if ( ( WindowRef ) event->message != inPB->window )
				{
					DoWindowEvent ( event ) ;
				}
			}
			
			//	intercept clicks in our custom items, if any
	/*		else if ( cd && ( event -> what == mouseDown ) )
			{
				switch ( inPB -> eventData . itemHit - cd -> numItems )
				{
					case kItemFormatPopup :
					{
						if ( cd -> formatPopup )
						{
							switch ( GetControlValue ( cd -> formatPopup ) )
							{
								case kItemTextFormat :
								{
									cd -> fileType = kTypeText ;
									break ;
								}

								case kItemUnicodeTextFormat :
								{
									cd -> fileType = kTypeUnicodeText ;
									break ;
								}
							}
						}
						break ;
					}

		     		case kItemStationeryCheckbox :
					{
						if ( cd -> stationeryCheckbox )
						{
							cd -> isStationery = 1 - cd -> isStationery ;
							SetControlValue ( cd -> stationeryCheckbox, cd -> isStationery ) ;
						}
						break ;
					}
						
				}
			}
			break ;
			*/
		}

		case kNavCBCustomize :
		{
			//	do we need extra items?
			if ( ! cd )
			{
				return ;
			}

			//	request an area for the extra items
			if ( ( inPB -> customRect . right == 0 ) && ( inPB -> customRect . bottom == 0 ) )
			{
				inPB -> customRect . right = inPB -> customRect . left + 240 ;
				inPB -> customRect . bottom = inPB -> customRect . top + 30 ;
			}
			break ;
		}

		case kNavCBStart :
		{
			DialogRef	dialog = GetDialogFromWindow ( inPB -> window ) ;
			Handle		extraItems =NULL;
			OSStatus	err ;

			//	do we need extra items?
			if ( ! cd )
			{
				return ;
			}

			//	get the DITL resource containing the extra items
			if ( ( extraItems = GetResource ( FOUR_CHAR_CODE ( 'DITL' ), cd -> extraItemsID ) ) == nil )
			{
				return ;
			}
			DetachResource ( extraItems ) ;

			//	add it to the nav dialog control list
			err = NavCustomControl ( inPB -> context, kNavCtlAddControlList, extraItems ) ;
			DisposeHandle ( extraItems ) ;
			if ( err != noErr )
			{
				return ;
			}

			//	count existing dialog items
			if ( ( err = NavCustomControl ( inPB -> context, kNavCtlGetFirstControlID, & cd -> numItems ) ) != noErr )
			{
				return ;
			}

			//	get handles to our custom controls
	/*		if ( ( err = GetDialogItemAsControl (dialog, cd -> numItems + kItemFormatPopup, & cd -> formatPopup ) ) != noErr )
			{
				return ;
			}
			if ( ( err = GetDialogItemAsControl ( dialog, cd -> numItems + kItemStationeryCheckbox, & cd -> stationeryCheckbox ) ) != noErr )
			{
				return ;
			}
			//	set up the format popup
			SetControlValue ( cd -> formatPopup, ( cd -> fileType == kTypeText ) ? kItemTextFormat : kItemUnicodeTextFormat ) ;
			break ;
*/			
		}
	}
}

static OSStatus CreateNavTypeList
	(
		OSType					inApplicationSignature,
		UInt16					inNumTypes,
		const OSType *			inSFTypeList,
		NavTypeListHandle *		outNavTypeList
	)
{
	OSStatus	err ;

	//	allocate the type list handle
	* outNavTypeList = ( NavTypeListHandle ) NewHandleClear ( ( sizeof ( NavTypeList ) - sizeof ( OSType ) ) +
		( inNumTypes * sizeof ( OSType ) ) ) ;
	if ( ( err = MemError ( ) ) != noErr )
	{
		return err ;
	}

	//	fill it in
	( ** outNavTypeList ) -> componentSignature = inApplicationSignature ;
	( ** outNavTypeList ) -> osTypeCount = inNumTypes ;
	BlockMoveData ( inSFTypeList, ( ** outNavTypeList ) -> osType, inNumTypes * sizeof ( OSType ) ) ;

	return noErr ;
}
