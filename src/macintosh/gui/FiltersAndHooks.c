/*
 *  R : A Computer Language for Statistical Data Analysis
 *  FiltersAndHooks.c
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
 *  WASTE Demo Project:
 *  FiltersAndHooks.c
 *
 */


/* FiltersAndHooks.c
 */


/* includes  */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <Balloons.h>
#include <Dialogs.h>
#include <Scrap.h>
#include <Appearance.h>
#include <AERegistry.h>
#include <Devices.h>
#include <Folders.h>
#include <Processes.h>
#include <Resources.h>
#include <StandardFile.h>
#include <TextUtils.h>
#include <ToolUtils.h>



typedef StandardFileReply *standardFileReplyPtr;

/* global variables */
#define rSelectDirectoryDialog   231
#define  iPopupItem              10
#define  iSelectButton           10
#define MIN(a,b)                 ((a) < (b) ? (a) : (b))

SInt16   gCurrentType = 1;
Str255   gPrevSelectedName;
Boolean  gDirectorySelectionFlag;

/*extern SFTypeList gFileTypes;*/

pascal Boolean filterFunctionOpenDialog(CInfoPBPtr,void *);
pascal SInt16 hookFunctionOpenDialog(SInt16,DialogPtr,void *);
StandardFileReply doDirectorySelectionDialog(void);
pascal Boolean filterFunctionDirSelect(CInfoPBPtr,void *);
pascal SInt16 hookFunctionDirSelect(SInt16,DialogPtr,void *);
extern void doCopyPString(Str255,Str255);
extern void doConcatPStrings(Str255,Str255);

/* filterFunctionOpenDialog
 */
/*pascal Boolean  filterFunctionOpenDialog(CInfoPBPtr pbPtr,void *dataPtr)
{
   if(pbPtr->hFileInfo.ioFlFndrInfo.fdType == gFileTypes[gCurrentType - 1])
      return false;
   else
      return true;
}
*/

/* *hookFunctionOpenDialog
 */
pascal SInt16
hookFunctionOpenDialog(SInt16 item,DialogPtr theDialog,void *dataPtr)
{
    SInt16   theType;
    Handle   controlHdl;
    Rect     theRect;

    switch(item)
    {
    case sfHookFirstCall:
	GetDialogItem(theDialog,iPopupItem,&theType,&controlHdl,&theRect);
	SetControlValue((ControlHandle) controlHdl,gCurrentType);
	return sfHookNullEvent;
	break;

    case iPopupItem:
	GetDialogItem(theDialog,iPopupItem,&theType,&controlHdl,&theRect);
	theType = GetControlValue((ControlHandle) controlHdl);
	if(theType != gCurrentType)
	{
            gCurrentType = theType;
            return sfHookRebuildList;
	}
	break;
    }

    return item;
}


/* doDirectorySelectionDialog
 */
StandardFileReply  doDirectorySelectionDialog(void)
{
    StandardFileReply stdFileReplyStruct;
    SFTypeList        fileTypes;
    Point             dialogLocation;
    FileFilterYDUPP   filterFunctionDirSelectUPP;
    DlgHookYDUPP      hookFunctionDirSelectUPP;

    filterFunctionDirSelectUPP =
	NewFileFilterYDProc((ProcPtr) filterFunctionDirSelect);
    hookFunctionDirSelectUPP =
	NewDlgHookYDProc((ProcPtr) hookFunctionDirSelect);

    gPrevSelectedName[0] = 0;
    gDirectorySelectionFlag = true;
    dialogLocation.v = -1;
    dialogLocation.h = -1;

    CustomGetFile(filterFunctionDirSelectUPP, -1, fileTypes,
		  &stdFileReplyStruct, rSelectDirectoryDialog,
		  dialogLocation, hookFunctionDirSelectUPP, NULL, NULL,
		  NULL, &stdFileReplyStruct);

    DisposeRoutineDescriptor(filterFunctionDirSelectUPP);
    DisposeRoutineDescriptor(hookFunctionDirSelectUPP);

    return stdFileReplyStruct;
}


/* filterFunctionDirSelect
 */
pascal Boolean  filterFunctionDirSelect(CInfoPBPtr pbPtr,void *dataPtr)
{
    SInt32   attributes;
    Boolean result;

    attributes = (SInt32) pbPtr->hFileInfo.ioFlAttrib;
    result = !(BitTst(&attributes,31 - 4));
    return result;
}


/* hookFunctionDirSelect
 */
pascal SInt16
hookFunctionDirSelect(SInt16 item, DialogPtr theDialog,void *dataPtr)
{
    SInt16       itemType, width;
    Handle       itemHdl;
    Rect         itemRect;
    Str255       theName, theString =  "\pSelect  '";
    standardFileReplyPtr stdFileReplyPtr;

    stdFileReplyPtr = (standardFileReplyPtr) dataPtr;

    if(stdFileReplyPtr->sfIsFolder || stdFileReplyPtr->sfIsVolume)
    {
	doCopyPString(stdFileReplyPtr->sfFile.name,theName);

	if(IdenticalString(theName,gPrevSelectedName,NULL) != 0)
	{
	    doCopyPString(theName,gPrevSelectedName);
	    GetDialogItem(theDialog, iSelectButton, &itemType,
			  &itemHdl, &itemRect);
	    width = (itemRect.right - itemRect.left) -
		StringWidth("\pSelect  '    ");
	    TruncString(width,theName,smTruncMiddle);
	    doConcatPStrings(theString,theName);
	    doConcatPStrings(theString,"\p'");
	    SetControlTitle((ControlHandle) itemHdl,theString);
	}
    }

    if(item == iSelectButton)
	return sfItemCancelButton;
    else if(item == sfItemCancelButton)
	gDirectorySelectionFlag = false;

    return item;
}


/* doCopyPString
 */
void doCopyPString(Str255 sourceString,Str255 destinationString)
{
    SInt16   stringLength;

    stringLength = sourceString[0];
    BlockMove(sourceString + 1,destinationString + 1,stringLength);
    destinationString[0] = stringLength;
}


/* doConcatPStrings
 */
void  doConcatPStrings(Str255 targetString, Str255 appendString)
{
    SInt16   appendLength;

    appendLength = MIN(appendString[0],255 - targetString[0]);

    if(appendLength > 0)
    {
	BlockMoveData(appendString+1, targetString+targetString[0]+1,
		      (SInt32) appendLength);
	targetString[0] += appendLength;
    }
}

