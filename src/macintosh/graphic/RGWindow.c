/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RGWindow.c
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
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 */

#include "RIntf.h"

#define MIN(a,b) 	((a) < (b) ? (a) : (b))

extern WindowPtr	Console_Window;
extern SInt16		Edit_Window;
extern WindowPtr	Graphic_Window[MAX_NUM_G_WIN + 1];
extern SInt16		Current_Window;
extern Graphic_Ref	gGReference[MAX_NUM_G_WIN + 1];

extern SInt16       Help_Window;
extern WindowPtr    Help_Windows[MAX_NUM_H_WIN + 1];
extern WindowPtr    Edit_Windows[MAX_NUM_E_WIN + 1];
extern char   		wTitle[265], mTitle[265];

void			doWindowsMenu(SInt16 menuItem);
void			doActivate(EventRecord*);
void			doActivateWindow(WindowPtr,Boolean);
Boolean			EqualNumString(Str255 Str1, Str255 Str2, SInt16 Num);


/* isGraphicWindow :
   This function is used to check whether the 'window' is a graphic
   window or not. If not, return 0. If yes, return the WinIndex
 */
int isGraphicWindow(WindowPtr window)
{
    SInt16 i;
    for(i = 1; i < Current_Window; i++) {
	if (window == Graphic_Window[i]) {
	    return i;
	}
    }
    return 0;
}


/* changeGWinPtr :
   Adjust the winIndex when some Graphic window is closed.
 */
void changeGWinPtr(WindowPtr window, Str255 Cur_Title)
{
    MenuHandle windowMenu = NULL;
    Str255 Menu_Title;
    int j, i = isGraphicWindow(window);

    Kill_G_History(i);
    for (j = i; j < Current_Window; j++){
	Graphic_Window[j] = Graphic_Window[j+1];
	gGReference[j] = gGReference[j+1];
    }
    Current_Window--;
/*    windowMenu = GetMenuHandle(kMenuWindows);

    for(i = 1; i <= CountMenuItems(windowMenu); i++){
	GetMenuItemText(windowMenu, i , (unsigned char*)&Menu_Title);
	CopyPascalStringToC(Menu_Title,mTitle);
	CopyPascalStringToC(Menu_Title,wTitle);
	if (strcmp(mTitle,wTitle) == 0) {
	    DeleteMenuItem(windowMenu, i);
	    break;
	}
    }
*/
}


/* GWdoConcatPStrings :
   Attach two Pascal string together, which is used to set the Title of
   the Graphic window
 */
void  GWdoConcatPStrings(Str255 targetString, Str255 appendString)
{
    SInt16 appendLength;
    appendLength = MIN(appendString[0], 255 - targetString[0]);
    if(appendLength > 0) {
	BlockMoveData(appendString+1,targetString+targetString[0]+1,
		      (SInt32) appendLength);
	targetString[0] += appendLength;
    }
}

/* doActivate :
   Handle the addition Activate event of the Graphic window
 */
void doActivate(EventRecord *eventStrucPtr)
{
    WindowPtr windowPtr;
    Boolean	 becomingActive;
    windowPtr = (WindowPtr) eventStrucPtr->message;
    becomingActive = ((eventStrucPtr->modifiers & activeFlag) == activeFlag);
    doActivateWindow(windowPtr, becomingActive);
}


/* doActivateWindow
 */
void  doActivateWindow(WindowPtr windowPtr,Boolean becomingActive)
{
#ifdef FFFFF
    MenuHandle	windowsMenu;
    SInt16			menuItem, a = 1;

    windowsMenu = GetMenuHandle(kMenuWindows);

    while(Graphic_Window[a] != windowPtr)
	a++;
    menuItem = a;

    if(becomingActive)
	CheckMenuItem(windowsMenu,menuItem,true);
    else
	CheckMenuItem(windowsMenu,menuItem,false);
#endif
}

#define	max(a, b) 		((a) < (b) ? (b) : (a))
/* doWindowsMenu
 */
void doWindowsMenu(SInt16 menuItem)
{
    WindowPtr windowPtr;
    SInt16 i;
    Str255 Cur_Title, Menu_Title;
    MenuHandle windowsMenu;
    Boolean EqString=FALSE;
    
    windowsMenu = GetMenuHandle(kMenuWindows);
    GetMenuItemText(windowsMenu, menuItem, Menu_Title);

    CopyPascalStringToC(Menu_Title,mTitle);
    
    /* First we check for the "R Console" Window */

    GetWTitle(Console_Window, (unsigned char *) &Cur_Title);
    CopyPascalStringToC("\pR Console",wTitle);
	if (strcmp(wTitle,mTitle) == 0) {
	SelectWindow(Console_Window);
	return;
    }
    /* Then we check among the graphic windows */

    for(i = 1; i < Current_Window; i++){
	GetWTitle(Graphic_Window[i], (unsigned char *) &Cur_Title);
	CopyPascalStringToC(Cur_Title,wTitle);
	if (strcmp(wTitle,mTitle) == 0) {
	    SelectWindow(Graphic_Window[i]);
	    return;
	}
    }

    /* Then we check among the edit windows */

    for(i = 1; i < Edit_Window; i++){
	GetWTitle(Edit_Windows[i], (unsigned char *) &Cur_Title);
	CopyPascalStringToC(Cur_Title,wTitle);
	if (strcmp(wTitle,mTitle) == 0) {
	    SelectWindow(Edit_Windows[i]);
	    return;
	}
    }

    for(i = 1; i < Help_Window; i++){
	GetWTitle(Help_Windows[i], Cur_Title);
	CopyPascalStringToC(Cur_Title,wTitle);
	if (strcmp(wTitle,mTitle) == 0) {
	    SelectWindow(Help_Windows[i]);
	    return;
	}
    }
}


/* Get_Graphic_Window:
   Using the WinIndex to query the Window Ptr.
 */
WindowPtr Get_Graphic_Window(int windNum)
{
    return Graphic_Window[windNum];
}


Boolean EqualNumString(Str255 Str1, Str255 Str2, SInt16 Num)
{
    Str255 ComStr1, ComStr2;
    ComStr1[0] = ComStr2[0] = Num;
    strncpy((char*)(&ComStr1[1]), (char*)(&Str1[1]), Num);
    strncpy((char*)(&ComStr2[1]), (char*)(&Str2[1]), Num);
    return EqualString(ComStr1, ComStr2, true, true);
}
