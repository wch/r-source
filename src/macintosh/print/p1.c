/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File p1.c
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
 *  This file was originally written by: Wing Kwong (Tiki), WAN 3/2/99
 *
 *  This file is adapted from the public demos coming with the Waste library
 *  distribution:  WASTE Text Engine © 1993-2000 Marco Piovanelli.
 *
 *  Original file was:
 *
 *	WASTE Demo Project:
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */

#include <Devices.h>
#include <Dialogs.h>
#include <Fonts.h>
#include <Menus.h>
#include <Printing.h>
#include <Resources.h>
#include <SegLoad.h>
#include <TextEdit.h>
#include <TextUtils.h>
#include <ToolUtils.h>


#define kMargin			90
#define MAXLONG			0x7FFFFFFF

/* global variables
*/
THPrint			gTPrintHdl;
WindowPtr		gWindowPtr;
TEHandle		gEditRecHdl;
Boolean			gDone;
Boolean			gPrintRecordInited = false;
Handle			gTextHdl;
PicHandle		gPictureHdl;
OSErr			err;
extern SInt16	gTextSize;
extern Boolean	PrintPicture;

/* function prototypes
*/
void	doInitManagers(void);
void	doEvents(EventRecord*);
void	doMouseDown(EventRecord*);
void	doMenuChoice(long);
void	printLoop(void);
OSErr	doCreatePrintRecord(void);
void	doPrStyleDialog(void);
void	doDrawPage(Rect, SInt16, SInt16);
SInt16	doGetPageOrientation(void);
Boolean	doIsPrGeneralThere(void);
void	doPrintError(SInt16, Boolean);
SInt16  doCalcNumberOfPages(Rect pageRect);

/*  main
*/
void  printLoop(void)
{
    GrafPtr oldPort;
    SInt16 printError;
    SInt16 numberOfPages, numberOfCopies;
    Boolean userClickedOK;
    SInt16 firstPage, lastPage, copy, page;
    TPPrPort printPortPtr;
    TPrStatus tprStatus;

    GetPort(&oldPort);
    PrOpen();

    if (PrError() == noErr) {
	if (!gPrintRecordInited)
	    printError = doCreatePrintRecord();
	else
	    printError = noErr;
			
	if(printError == noErr) {
	    if (PrintPicture)
		numberOfPages = 1;
	    else
		numberOfPages = doCalcNumberOfPages((*gTPrintHdl)->prInfo.rPage);    
	    userClickedOK = PrJobDialog(gTPrintHdl);
	    if(userClickedOK) {						
		numberOfCopies = (*gTPrintHdl)->prJob.iCopies;
		firstPage = (*gTPrintHdl)->prJob.iFstPage;
		lastPage = (*gTPrintHdl)->prJob.iLstPage;
		(*gTPrintHdl)->prJob.iFstPage = 1;
		(*gTPrintHdl)->prJob.iLstPage = iPrPgMax;

		if(numberOfPages < lastPage)
		    lastPage = numberOfPages;
				
		for (copy = 1; copy < numberOfCopies + 1; copy++) {					
		    for (page = firstPage; page < lastPage + 1; page++) {
			if((page - firstPage) % iPFMaxPgs == 0) {	
			    if(page != firstPage) {
				PrCloseDoc(printPortPtr);
				if(((*gTPrintHdl)->prJob.bJDocLoop == bSpoolLoop)
				   && (PrError() == noErr))
				    PrPicFile(gTPrintHdl,NULL,NULL,NULL,&tprStatus);
			    }
			    printPortPtr = PrOpenDoc(gTPrintHdl,NULL,NULL);
			}	
			if(PrError() == noErr) {
			    PrOpenPage(printPortPtr,NULL);
			    if(PrError() == noErr)
				doDrawPage((*gTPrintHdl)->prInfo.rPage,page,numberOfPages);
			    PrClosePage(printPortPtr);
			}
		    }
		    PrCloseDoc(printPortPtr);
		    if(((*gTPrintHdl)->prJob.bJDocLoop == bSpoolLoop) && (PrError() == noErr))
			PrPicFile(gTPrintHdl,NULL,NULL,NULL,&tprStatus);
		}
	    }
	}
    }
    printError = PrError();
    PrClose();
    if(printError != noErr && printError != iPrAbort)
	doPrintError(printError,false);
    SetPort(oldPort);
}

/* doCreatePrintRecord
*/
OSErr doCreatePrintRecord(void)
{
    SInt16 printError;

    gTPrintHdl = (THPrint) NewHandleClear(sizeof(TPrint));
    if(gTPrintHdl != NULL ) {
	PrintDefault(gTPrintHdl);
	printError = PrError();
	if(printError == noErr)
	    gPrintRecordInited = true;
	return(printError);
    }
    else
	ExitToShell();
}

/* doPrStyleDialog
*/
void doPrStyleDialog(void)
{
    SInt16  printError;
    GrafPtr savePort;

    GetPort(&savePort);
    PrOpen();

    printError = PrError();
    if(printError == noErr) {
	if(!gPrintRecordInited) {
	    printError = doCreatePrintRecord();
	    if(printError != noErr)
		doPrintError(printError, true);
	}
	PrStlDialog(gTPrintHdl);
    }
    else
	doPrintError(printError, false);
    PrClose();
    SetPort(savePort);
}


/* doDrawPage
*/
void doDrawPage(Rect pageRect,SInt16 pageNumber,SInt16 numberOfpages)
{
    Rect destRect, pictureRect;
    SInt16 heightDestRect, linesPerPage, numberOfLines, fontNum;
    TEHandle pageEditRecHdl;
    Handle textHdl;
    SInt32 startOffset, endOffset;
    Str255 theString;
    short LeftVal, RightVal, TopVal, BottomVal;
    TEHandle EditRecHdl;
    SInt16 printfont;

    if (PrintPicture) {	
	SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right, pageRect.bottom);
	LeftVal = ((destRect.right - destRect.left) - ((*gPictureHdl)->picFrame.right -(*gPictureHdl)-> picFrame.left))/2;
	TopVal = ((destRect.bottom - destRect.top) - ((*gPictureHdl)->picFrame.bottom -(*gPictureHdl)->picFrame.top))/2;
	RightVal = LeftVal +((*gPictureHdl)->picFrame.right - (*gPictureHdl)->picFrame.left);
	BottomVal = TopVal + ((*gPictureHdl)->picFrame.bottom - (*gPictureHdl)->picFrame.top);
	SetRect(&pictureRect,LeftVal,TopVal, RightVal, BottomVal);
	DrawPicture(gPictureHdl,&pictureRect);
    }
    else {   
	SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right, pageRect.bottom);
	heightDestRect = destRect.bottom - destRect.top;
	GetFNum("\pmonaco", &printfont);
	TextFont(printfont);
	TextSize(gTextSize);
	linesPerPage = heightDestRect / (*gEditRecHdl)->lineHeight; 
	numberOfLines = (*gEditRecHdl)->nLines;
	pageEditRecHdl = TENew(&destRect,&destRect);
	textHdl = (*gEditRecHdl)->hText;
	startOffset = (*gEditRecHdl)->lineStarts[(pageNumber - 1) * linesPerPage];
	if (pageNumber == numberOfpages)
	    endOffset = (*gEditRecHdl)->lineStarts[numberOfLines];
	else
	    endOffset = (*gEditRecHdl)->lineStarts[pageNumber * linesPerPage];
	HLock(textHdl);
	TEInsert(*textHdl + startOffset,endOffset - startOffset,pageEditRecHdl);
	HUnlock(textHdl);
    }
}


/* doGetPageOrientation
*/
SInt16 doGetPageOrientation(void)
{
    TGetRotnBlk getRotRec;

    if(doIsPrGeneralThere) {
	getRotRec.iOpCode = getRotnOp;
	getRotRec.hPrint = gTPrintHdl;
	PrGeneral((Ptr) &getRotRec);
	if((getRotRec.iError == noErr) && (PrError() == noErr) && getRotRec.fLandscape)
	    return(1);
	else
	    return(2);
    }
    else
	return(3);
}	

/* doIsPrGeneralThere
*/
Boolean doIsPrGeneralThere(void)
{
    TGetRotnBlk	getRotRec;
    OSErr printError;

    printError = 0;
    getRotRec.iOpCode = getRotnOp;
    getRotRec.hPrint = gTPrintHdl;
    PrGeneral((Ptr) &getRotRec);
    printError = PrError();
    PrSetError(noErr);
    if(printError == resNotFound)
	return(false);
    else
	return(true);
}

/* doPrintError
*/
void doPrintError(SInt16 printError,Boolean fatal)
{
    /* Handle Error in the Future */
}


/* doCalcNumberOfPages
*/
SInt16  doCalcNumberOfPages(Rect pageRect)
{
    Rect destRect, pictureRect;
    SInt16 printfont;
    SInt16 heightDestRect, linesPerPage, numberOfPages;
	
    SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right, pageRect.bottom);	
    GetFNum("\pmonaco", &printfont);
    TextFont(printfont);
    TextSize(gTextSize);
    HidePen();
    gEditRecHdl = TENew(&destRect,&destRect);
    HLock(gTextHdl);
    TEInsert(*gTextHdl,GetHandleSize(gTextHdl), gEditRecHdl);
    HUnlock(gTextHdl);
    ShowPen();
    heightDestRect = destRect.bottom - destRect.top;
    linesPerPage = heightDestRect / (*gEditRecHdl)->lineHeight; 
    numberOfPages = ((*gEditRecHdl)->nLines / linesPerPage) + 1;	 
    /* SetRect(&pictureRect,destRect.left,destRect.top,
       destRect.left + ((*gPictureHdl)->picFrame.right - (*gPictureHdl)->picFrame.left),
       destRect.top + ((*gPictureHdl)->picFrame.bottom - (*gPictureHdl)->picFrame.top));	
       DrawPicture(gPictureHdl,&pictureRect);
    */
    return(numberOfPages);
}


