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
#include "PicComments.h"
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
Handle	                                 myHandle;
SInt32                                   curPaste, finalPaste;
static Handle                            sColors;    /* handle to the 'mctb' resource for the Color menu*/


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

/*    Protocols    */
void    assignPString                    (unsigned char* , char* , SInt16);
OSErr   FindAProcess                     (OSType, OSType, ProcessSerialNumber*);
OSErr   OpenSelection                    (FSSpecPtr theDoc);
void    ConsolePaste                     (void);
void    ConsoleCopyAndPaste              (void);
OSErr DoOpenText(void);
OSErr DoOpen(void);
OSErr DoSource(void);
void consolecmd(char *cmd);

/*    Extern Global variables   */
extern  void   doWindowsMenu             (SInt16);
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
extern void printLoop();
extern void DoUpdate (WindowPtr window);
extern void DoActivate (Boolean isActivating, WindowPtr window);
extern void Do_About();

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
    LMSetCurDirStore (spec->parID);
    LMSetSFSaveDisk (- spec->vRefNum);
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
	if ((WindowPtr) event->message != dialog) {
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
	sFilterUPP = NewModalFilterYDProc(MySFDialogFilter);
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
    MenuHandle		menu;
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
    MenuHandle		windowsMenu;
    Boolean		EqString;


/*
     Boolean *present = nil ;
     Boolean  isContinuous = false ;
*/
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
	DisableItem (menu, item);
    }
    if (isGraphicWindow(window)){
	SetMenuItemText(menu,kItemOpen, "\pActivate");
	SetItemCmd(menu, kItemOpen, 'G');
    }
    else{
	SetMenuItemText(menu,kItemOpen, "\pOpen...");
	SetItemCmd(menu, kItemOpen, 'O');
    }

    /* New, Open, and Quit are always enabled
     */
    if (isEditWindow(window)){
	EnableItem (menu, kItemLoad);
	EnableItem (menu , kItemEditObject);
    }

    if (window == Console_Window)
    {
	EnableItem (menu, kItemLoad);
	EnableItem (menu , kItemEditObject);
    }
    EnableItem (menu, kItemNew);
    EnableItem (menu, kItemOpen);
    EnableItem (menu, kItemShow);
    EnableItem (menu, kItemPageSetup);
    EnableItem (menu, kItemPrint);
    EnableItem (menu, kItemQuit);

    /* Enable "Close" and "Save As" if there is an active window
     */
    if (window != nil) {
	EnableItem (menu, kItemClose);
	if (!isHelpWindow(window)) {
	    EnableItem (menu, kItemSaveAs);
	    EnableItem (menu, kItemSave);
	}
    }

    /* *** EDIT MENU ***
     */
    menu = GetMenuHandle (kMenuEdit);

    /* first, disable all items
     */
    for (item = CountMenuItems(menu); item >= 1; item--) {
	DisableItem(menu, item);
    }
    if (isGraphicWindow(window)) {
	EnableItem (menu, kItemCopy);
    }
/*	EnableItem(menu, kItemPreference);
 */
    /* by default, the Undo menu item should read "Can't Undo"
     */
    GetIndString(itemText, kUndoStringsID, 1);
    SetMenuItemText(menu, kItemUndo, itemText);

    if (window != nil) {

	/* enable Paste if there's anything pasteable on the Clipboard
	 */
	if (GetScrap(NULL,'TEXT',&scrapOffset) > 0) {
	    if (!isGraphicWindow(window)){
		EnableItem (menu, kItemPaste);
	    }
	}

	/* enable Undo if anything can be undone
	 */
	actionKind = WEGetUndoInfo (&temp, we);

	if (actionKind != weAKNone) {
	    EnableItem (menu, kItemUndo);

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
	    EnableItem (menu, kItemSelectAll);
	}

	/* get the current selection range
	 */
	WEGetSelection (&selStart, &selEnd, we);

	/* enable Cut, Copy, and Clear if the selection range is not empty
	 */
	if (FrontWindow() == Console_Window) {
	    EnableItem (menu, kItemCopyPaste);
	    EnableItem (menu, kItemCut);
	    EnableItem (menu, kItemCopy);
	    EnableItem (menu, kItemClear);
	}

	if (isEditWindow(window)) {
	    EnableItem (menu, kItemLineTo);
	    EnableItem (menu, kItemCut);
	    EnableItem (menu, kItemCopy);
	    EnableItem (menu, kItemClear);
	}
	if (isHelpWindow(window)){
	    DisableItem (menu, kItemCut);
	    DisableItem (menu, kItemCopyPaste);
	    DisableItem (menu, kItemClear);
	    DisableItem (menu, kItemPaste);
	    EnableItem  (menu, kItemCopy);
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
    windowsMenu = GetMenu(mWindows);
    GetWTitle(window, (unsigned char *) &Cur_Title);

    for(i = 1; i <= CountMenuItems(windowsMenu); i++) {
	GetMenuItemText(windowsMenu, i , (unsigned char*)&Menu_Title);
	EqString = EqualNumString(Menu_Title, Cur_Title, Menu_Title[0]);
	CheckMenuItem(windowsMenu, i, false);
	if (EqString) CheckMenuItem(windowsMenu, i, true);
    }


}


/* DoDeskAcc
 */
void DoDeskAcc(SInt16 menuItem)
{
    Str255 deskAccessoryName;
    GetMenuItemText(GetMenuHandle(kMenuApple), menuItem, deskAccessoryName);
    OpenDeskAcc(deskAccessoryName);
}


/* DoNew
 */
OSErr DoNew(void)
{
    /* create a new window from scratch
     */

    return CreateWindow(nil);
}


/* DoOpen :
   Now implemented. It is useful to load object .rda-like
   from the "load" menu.
   Based on previous work of Ross Ihaka. Jago Nov 2000 (Stefano M. Iacus)
 */
OSErr DoOpen(void)
{
    StandardFileReply	reply;
    SFTypeList		typeList;
    FInfo		fileInfo;
    OSErr		err = noErr;
    Point		where = { -1, -1 };  /* auto center dialog */
    SInt16		pathLen;
    Handle		pathName;
    FILE		*fp;
    SEXP 		img, lst;
    int 		i;

    typeList[0] = 'BINA';   /* this are usually files coming from Windows XDR*/
    typeList[1] = 'RSES';   /* I will either create files with type R  */
    typeList[2] = 'ROBJ';   /* Session or R Object                         */

    /* put up the standard open dialog box.
       (we use CustomGetFile instead of StandardGetFile because we
       want to provide our own dialog filter procedure that takes
       care of updating our windows)
    */
    CustomGetFile(nil, 3, typeList, &reply, 0, where, nil,
		  GetMySFDialogFilter(), nil, nil, nil);
    err = FSpGetFInfo(&reply.sfFile, &fileInfo);
    if (err != noErr) return err;
    FSpGetFullPath(&reply.sfFile, &pathLen, &pathName);
    HLock((Handle)pathName);
    strncpy(InitFile, *pathName, pathLen);
    InitFile[pathLen] = '\0';
    HUnlock((Handle) pathName);

/*
   Routine now handles XDR object. Jago Nov2000 (Stefano M. Iacus)
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
    return err;
}

/* DoOpenText
 */
OSErr DoOpenText(void)
{
    StandardFileReply	reply;
    SFTypeList		typeList;
    FInfo		fileInfo;
    OSErr		err = noErr;
    Point		where = { -1, -1 };  /* auto center dialog */
    SInt16		pathLen, i;
    Handle		pathName;
    FILE		*fp;
    Str255		Cur_Title, Menu_Title;
    MenuHandle		windowsMenu;
    Boolean		EqString;
    char        	buf[10];

    /* set up a list of file types we can open for StandardGetFile
     */
    typeList[0] = kTypeText;
    typeList[1] = ftSimpleTextDocument;

    /* put up the standard open dialog box.
       (we use CustomGetFile instead of StandardGetFile because we
       want to provide our own dialog filter procedure that takes
       care of updating our windows)
    */
    CustomGetFile(nil, 2, typeList, &reply, 0, where, nil,
		  GetMySFDialogFilter(), nil, nil, nil);

    err = FSpGetFInfo(&reply.sfFile, &fileInfo);
    if (err != noErr) return err;

    DoNew();

    err = ReadTextFile(&reply.sfFile,
		       GetWindowWE(Edit_Windows[Edit_Window-1]));

    if(err != noErr)
	REprintf("\n ReadTextFile error: %d\n",err);

    windowsMenu = GetMenu(mWindows);
    GetWTitle(Edit_Windows[Edit_Window-1], (unsigned char *) &Cur_Title);
    for(i = 1; i <= CountMenuItems(windowsMenu); i++) {
	GetMenuItemText(windowsMenu, i , (unsigned char*)&Menu_Title);
	EqString = EqualNumString(Menu_Title, Cur_Title, Menu_Title[0]);
	if (EqString) {
	    DeleteMenuItem(windowsMenu, i);
	    sprintf((char *)&buf[1]," %d", Edit_Window - 1);
	    buf[0] = strlen(buf)-1;
	    doCopyPString(reply.sfFile.name, Cur_Title);
	    doConcatPStrings(Cur_Title, buf);
	    AppendMenu(windowsMenu, Cur_Title);
	    SetWTitle(Edit_Windows[Edit_Window-1], Cur_Title) ;
	    break;
	}
    }

    return err;
}

/* DoSource
 */
OSErr DoSource(void)
{
    StandardFileReply	reply;
    SFTypeList		typeList;
    FInfo		fileInfo;
    OSErr		err = noErr;
    Point		where = { -1, -1 };  /* auto center dialog */
    SInt16		pathLen;
    Handle		pathName;
    FILE		*fp;
    SEXP 		img, lst;
    int 		i;
    char 		sourcefile[FILENAME_MAX];
    char 		cmd[FILENAME_MAX+15];

    typeList[0] = kTypeText;
    typeList[1] = ftSimpleTextDocument;


    /* put up the standard open dialog box.
       (we use CustomGetFile instead of StandardGetFile because we
       want to provide our own dialog filter procedure that takes
       care of updating our windows)
    */
    CustomGetFile(nil, 1, typeList, &reply, 0, where, nil,
		  GetMySFDialogFilter(), nil, nil, nil);
    err = FSpGetFInfo(&reply.sfFile, &fileInfo);
    if (err != noErr) return err;
    FSpGetFullPath(&reply.sfFile, &pathLen, &pathName);
    HLock((Handle)pathName);
    strncpy(sourcefile, *pathName, pathLen);
    sourcefile[pathLen] = '\0';
    HUnlock((Handle) pathName);

    sprintf(cmd,"source(\"%s\")",sourcefile);

    consolecmd(cmd);

}

/* SaveWindow
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

    hDocument = GetWindowDocument(window);
    ForgetHandle(&(*hDocument)->fileAlias);

    if (isGraphicWindow(window)) {
	/* We don't save these ... */
    }
    else {
	GetWTitle(window, Cur_Title);
	windowsMenu = GetMenu(mWindows);
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
	    SetWTitle(window, pFileSpec->name);
	    if(menu_item!= -1)
	    {
		DeleteMenuItem(windowsMenu, i);
		AppendMenu(windowsMenu, pFileSpec->name);
	    }

	    /*	MenuHandle windowsMenu; */
	    //if(windowsMenu = GetMenu(mWindows))
	    // AppendMenu(windowsMenu, pFileSpec->name);

	    /* replace the old window alias (if any) with a new one
	       created from pFileSpec
	     */
	    NewAlias(nil, pFileSpec, &alias);

	    /* if err, alias will be nil, and it's not fatal,
	       just will make subsequent saves annoying
	     */
	    (* hDocument)->fileAlias = (Handle)alias;
	}
    }
    return err;
}

/* DoSaveAs
 */
OSErr DoSaveAs(const FSSpec *suggestedTarget, WindowPtr window)
{
    StringHandle	hPrompt;
    Str255		defaultName;
    StandardFileReply	reply;
    Point		where = { -1, -1 }; /* autocenter's dialog */
    OSErr		err;

    /* get the prompt string for CustomPutFile from a string resource
       and lock it
     */
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
    CustomPutFile(*hPrompt, defaultName, &reply, 0, where, nil,
		  GetMySFDialogFilter(), nil, nil, nil);

    /* unlock the string resource
     */
    HUnlock((Handle)hPrompt);

    /* if the user ok'ed the dialog, save the window to the specified file
     */
    if (reply.sfGood)
	err = SaveWindow(&reply.sfFile, window);
    else
	err = userCanceledErr;
    return err;
}

/* DoSave
 */
OSErr DoSave(WindowPtr window)
{
    FSSpec		spec;
    FSSpecPtr	suggestedTarget = nil;
    Boolean		promptForNewFile = true;
    Boolean		aliasTargetWasChanged;
    OSErr		err;

    /* resolve the alias associated with this window, if any
     */
    if ((* GetWindowDocument(window))->fileAlias != nil) {
	if ((ResolveAlias(nil, (AliasHandle)(*GetWindowDocument(window))->fileAlias,
			  &spec, &aliasTargetWasChanged) == noErr)) {
	    if (aliasTargetWasChanged)
		suggestedTarget = &spec;
	    else
		promptForNewFile = false;
	}
    }

    /* if no file has been previously associated with this window,
       or if the alias resolution has failed, or if the alias target
       was changed prompt the user for a new destination
    */
    if (promptForNewFile)
	err = DoSaveAs(suggestedTarget, window);
    else
	err = SaveWindow(&spec, window);
    return err;
}


/* SaveChangesDialogFilter
 */
static pascal Boolean
SaveChangesDialogFilter(DialogPtr dialog, EventRecord *event, SInt16 *item)
{
    /* map command + D to the "Don't Save" button
     */
    if ((event->what == keyDown) && (event->modifiers & cmdKey)
	&& ((event->message & charCodeMask) == 'd')) {
	/* flash the button briefly
	 */
	FlashButton(dialog, kButtonDontSave);
	/* fake an event in the button
	 */
	*item = kButtonDontSave;
	return true;
    }
    /* route everything else to our default handler
     */
    return CallModalFilterProc(GetMyStandardDialogFilter(), dialog, event, item);
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

#ifdef __cplusplus
    static ModalFilterUPP sFilterProc = NewModalFilterProc(SaveChangesDialogFilter);
#else
    static ModalFilterUPP sFilterProc = nil;
    if (sFilterProc == nil) {
	sFilterProc = NewModalFilterProc(SaveChangesDialogFilter);
    }
#endif
    if ( (win_num=isHelpWindow(window)) ) {

	GetWTitle(Help_Windows[win_num], Cur_Title);
	windowsMenu = GetMenu(mWindows);
	for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	    GetMenuItemText(windowsMenu, i , curString);
	    EqString = EqualNumString(Cur_Title, curString, curString[0]);
	    if (EqString) {
		DeleteMenuItem(windowsMenu, i);
		break;
	    }
	}
	adjustHelpPtr(win_num);
	DestroyWindow(window);

	return noErr;
    }
    err = noErr;
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
	    SetCursor(&qd.arrow);
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

	if (saving == savingYes) {
	    if (isGraphicWindow(window)) {
		err = doSaveGraCommand();
		if (err != noErr) {
		    /* You can handle error in here!
		       However, there have some warning return,
		       don't treat it as error.
		    */
		}
	    }
	    else {
            	if (window == Console_Window) {
		    err = doRSave(&haveCancel);
		    if (haveCancel){
			RWrite("\r");
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
		    if(saving != savingNo)
		     DoSave(window);
		}
	    }
	}
    }


    /* if it is a graphic window, maintain the menus and title first.
     */
    if (isGraphicWindow(window)){
	Mac_Dev_Kill(window);
    }
    else {
	if ( (win_num=isEditWindow(window)) ) {
	    GetWTitle(Edit_Windows[win_num], Cur_Title);
	    windowsMenu = GetMenu(mWindows);
            for(i = 1; i <= CountMenuItems(windowsMenu); i++){
		GetMenuItemText(windowsMenu, i , curString);
		EqString = EqualNumString(Cur_Title, curString, curString[0]);
		if (EqString) {
		    DeleteMenuItem(windowsMenu, i);
		    break;
		}
	    }
	    adjustEditPtr(win_num);
	}
	/* destroy the window */
	DestroyWindow(window);
	if (window == Console_Window)
	    ExitToShell();
    }
    return err;
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
void DoFileChoice(SInt16 menuItem)
{
    WindowPtr	window = FrontWindow();
    OSErr	osError, err;
    EventRecord	myEvent;
    SInt16	WinIndex;
    Boolean	haveCancel;

    switch(menuItem) {
    case kItemNew:
	if (isGraphicWindow(window)) {
	    RWrite("macintosh()\r");
	    myEvent.message = 140301;
	    DoGenKeyDown (&myEvent, true);
	}
	else
	    DoNew();
	break;

    case kItemShow:
	DoOpenText();
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

    case kItemLoad :
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

    case kItemClose:
	DoClose(closingWindow, savingAsk, window);
	break;

    case kItemSave:
	if (isGraphicWindow(window)) {
	    osError = doSaveGraCommand();
	    if (osError != noErr){
		/* You can handle error in here!
		   However, there have some warning return,
		   don't treat it as error.
		*/
	    }
	}
	else {
	    if (window == Console_Window) {
		err = doRSave(&haveCancel);
		if (haveCancel) {
		    RWrite("\r");
		    jump_to_toplevel();
		}
	    }
	    else {
		DoSave(window);
	    }
	}
	break;

    case kItemSaveAs:
	if (isGraphicWindow(window)) {
	    osError = doSaveAsGraCommand();
	    if (osError != noErr) {
				/* You can handle error in here! */
	    }
	}
	else {
	    if (window == Console_Window) {
		err = doRSaveAs(&haveCancel);
		if (haveCancel){
		    RWrite("\r");
		    jump_to_toplevel();
		}
	    }
	    else {
		DoSaveAs(nil, window);
	    }
	}
	break;

    case kItemPageSetup:
	do_PageSetup();
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
    SInt32		selEnd, selStart, i;
    long		scrapOffset, rc;
    EventRecord		myEvent;
    char		TempChar;

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
	if (FrontWindow() == Console_Window) {
	    WESetSelection(WEGetTextLength(we), WEGetTextLength(we), we);
	    myHandle = NewHandle(0);
	    /* allocate 0-length data area */
	    rc = GetScrap(myHandle, 'TEXT', &scrapOffset);
	    if (rc < 0) {
		/* . . . process the error . . . */ }
	    else {
		SetHandleSize(myHandle, rc+1);	/* prepare for printf() */
		HLock(myHandle);
		(*myHandle)[rc] = 0;		/* make it ASCIIZ */
		finalPaste = rc;
		for (i = 0; i <=rc; i++) {
		    TempChar = (*myHandle)[i];
		    if ((TempChar == '\r') || (i == finalPaste)) {
			RnWrite(*myHandle, i);
			if (i != finalPaste) {
			    myEvent.message = 140301;
			    DoKeyDown (&myEvent);
			}
			curPaste = i;
			break;
		    }
		}
		finalPaste = rc;
		/* RWrite(*myHandle);
		   printf("Scrap contained text: %s'\n", *myHandle);
		*/
		HUnlock(myHandle);
		if (rc != i) HaveContent = true;
		else DisposeHandle(myHandle);
	    }
	    /* WEPaste(we); */
	}
	else {
	    if (isGraphicWindow(window) == 0)
		WEPaste(we);
	}
	/*WEPaste (we);*/
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

void changeSize(WindowPtr window, SInt16 newSize)
{
    if(window)
     WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontSize,
      & newSize, sizeof ( Fixed ),	GetWindowWE ( window ) );

/* Old code: mod Jago 08/28/00

   TextStyle	ts;
   WEStyleMode	mode;
   ts.tsSize = newSize;

   mode = weDoSize;


   WESetStyle(mode, &ts, GetWindowWE(window));
*/
}



/* DoMenuChoice:
   The main function on RMenus.c, it is used to handle where to dispatch
   the event to the corresponding procedure to handle the corresponding
   menus choice. Thus, if you need some Menus choice, it is the best
   place to start with.
 */
void DoMenuChoice(SInt32 menuChoice, EventModifiers modifiers)
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
	DoFileChoice(menuItem);
	break;

    case kMenuEdit:
	DoEditChoice(menuItem);
	break;

    case kWindows:
	doWindowsMenu(menuItem);
	break;

    }
    HiliteMenu(0);
}


/* InitializeMenus
 */
OSErr InitializeMenus(void)
{
    OSErr err = noErr;

    // build up the whole menu bar from the 'MBAR' resource
    SetMenuBar(GetNewMBar(kMenuBarID));

    // add names to the apple and Font menus
    AppendResMenu(GetMenuHandle(kMenuApple), kTypeDeskAccessory);
    AppendResMenu(GetMenuHandle(kMenuFont), kTypeFont);

    // insert the alignment and direction submenus into the hierarchical
    // portion of the menu list
    InsertMenu(GetMenu(kMenuAlignment), -1);
    InsertMenu(GetMenu(kMenuDirection), -1);

    // disable the "Drag and Drop Editing" item in the Features menu
    // once and for all
    // if the Drag Manager isn't available
    if (! gHasDragAndDrop) {
	DisableItem(GetMenuHandle(kMenuFeatures), kItemDragAndDrop);
    }
    // disable the "Other" item in the Color menu if Color QuickDraw
    // isn't available
    if (! gHasColorQD) {
	DisableItem(GetMenuHandle(kMenuColor), kItemOtherColor);
    }
    // load the menu color table for the color menu
    sColors = GetResource(kTypeMenuColorTable, kMenuColor);
    if ((err = ResError()) != noErr) {
	return err;
    }
    HNoPurge(sColors);
    // draw the menu bar
    DrawMenuBar();
    return err;
}


/* do_Print

   do_Print is a independent function, in here, if you set the hdl of
   picture, and call doPrinting, you can print out a picture too.  You
   are not required to know how it work. (printing1.c , printing2.c,
   print.h)
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

    linesize.h = 10;
    linesize.v = 7;
    lp = &linesize;
    window = FrontWindow();
    we = GetWindowWE(window);
    // defaultPort = false;
    if (isGraphicWindow(window)) {
	PrintPicture = true;
	WinIndex = isGraphicWindow(window);
	dd =(DevDesc*)gGReference[WinIndex].devdesc;
	GetPort(&savePort);
	HLock((Handle) gPictureHdl);
	SetPort(window);
	gPictureHdl = OpenPicture(&(window->portRect));
	GetPort(&picPort);
	PicComment(SetLineWidth, 4, (char**)&lp);
	playDisplayList(dd);
	SetPort(picPort);
	ClosePicture();
	SetPort(FrontWindow());
	//PrintPicture();
	printLoop();
	HUnlock((Handle) gPictureHdl);
	//doPrinting();
	SetPort(savePort);
    }
    else {
	GetPort(&savePort);
	PrintPicture = false;
	SetPort(FrontWindow());
	HLock((Handle) gTextHdl);
	/* Get the Handle of the Text which need to be print */
	gTextHdl = WEGetText(we);
	/* Extern Procedure */
	//doPrinting();
	printLoop();
	HUnlock((Handle) gTextHdl);
	SetPort(savePort);
    }
    defaultPort = true;
    DoActivate(false, FrontWindow());
    DoActivate(true, FrontWindow());
    // SetPort(FrontWindow());
    // DoUpdate(FrontWindow());
}



/* do_PageSetup

   This function is a plugin function, it is work together with the
   do_Printing function.
 */
void do_PageSetup(void)
{
    // gInhibitPrintRecordsInfo = false;
    // gInhibitPrintStructuresInfo = false;
    // gPrintRecordInited = false;
    // gPrintStructureInited = false;
    SetPort(FrontWindow());
    doPrStyleDialog();
}


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
   Copy and Paste from the Menus.
 */
void ConsoleCopyAndPaste()
{
    WEReference	we;
    we = GetWindowWE(Console_Window);
    WECopy(we);
    ConsolePaste();
}

/* ConsolePaste :
   This procedure will be either called by procedure
   ConsoleCopyAndPaste or from the Menus Choice We will read the text
   we have line by line.  The trick we use is that we will directly
   send a key event ("\r") to terminate the R_ReadConsole procedure
   after each line.
 */
void ConsolePaste()
{
    WEReference	we;
    SInt16	i;
    long	scrapOffset, rc;
    EventRecord	myEvent;
    char	TempChar;

    we = GetWindowWE(Console_Window);

    WESetSelection(WEGetTextLength(we), WEGetTextLength(we), we);

    myHandle = NewHandle(0);	/* allocate 0-length data area */

    rc = GetScrap(myHandle, 'TEXT', &scrapOffset);
    if (rc < 0) {
	/* . . . process the error . . . */
    }
    else {
	SetHandleSize(myHandle, rc + 1);	/* prepare for printf() */
	HLock(myHandle);
	(*myHandle)[rc] = 0;				/* make it ASCII */
	finalPaste = rc;
	for (i = 0; i <= rc; i++) {
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
	finalPaste = rc;
	HUnlock(myHandle);
	if (rc != i) HaveContent = true;
	else DisposeHandle(myHandle);
    }
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

    /* we just write the cmd as it is to the console */
    RnWrite(cmd, cmdlen);

    /* We send a '\r' event to the console */
    myEvent.message = 140301;
    DoKeyDown(&myEvent);

}
