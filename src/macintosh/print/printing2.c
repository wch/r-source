/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file printing2.c
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
//  defines

#define mApple		128
#define mFile		129
#define  iQuit		11
#define  iPageSetup	8
#define  iPrint		9
#define rMenubar	128
#define rWindow		128
#define rText		128
#define rPicture	128
#define rPrintAlert	128
#define kMargin		90
#define MAXLONG		0x7FFFFFFF
#define	gFontNumber     4
#define gFontSize       9
//  global variables

THPrint		gTPrintHdl;
WindowPtr	gWindowPtr;
Boolean		gDone;
Boolean		gPrintRecordInited = false;
Boolean		gInhibitPrintRecordsInfo = false;
TEHandle	gEditRecHdl;
Handle		gTextHdl;
TPPrPort	printPortPtr;
PicHandle	gPictureHdl;

// function prototypes

void		main(void);
void		doInitManagers(void);
void		doEvents(EventRecord *);
void		doMouseDown(EventRecord *);
void		doActivateWindow(void);
void		doMenuChoice(long);
void		printLoop(void);
OSErr		doCreatePrintRecord(void);
void		doPrStyleDialog(void);
SInt16		doCalcNumberOfPages(Rect);
void		doDrawPage(Rect,SInt16,SInt16);
//void		doPrintRecordsInfo(void);
void		doDrawRectStrings(Str255,SInt16,SInt16,Str255,SInt16,SInt16,Str255);
void 		doDrawPageOrientation(void);
SInt16		doGetPageOrientation(void);
Boolean 	doIsPrGeneralThere(void);
void		doPrintError(SInt16,Boolean);

void  printLoop(void)
{
    GrafPtr		oldPort;
    SInt16		printError;
    SInt16		numberOfPages, numberOfCopies;
    Boolean		userClickedOK;
    SInt16		firstPage, lastPage, copy, page;

    TPrStatus	tprStatus;

    GetPort(&oldPort);

    PrOpen();
    if(PrError() == noErr)
    {
	if(!gPrintRecordInited)
	    printError = doCreatePrintRecord();
	else
	    printError = noErr;

	if(printError == noErr)
	{
	    numberOfPages = doCalcNumberOfPages((*gTPrintHdl)->prInfo.rPage);

	    userClickedOK = PrJobDialog(gTPrintHdl);
	    if(userClickedOK)
	    {
		//	doPrintRecordsInfo();
		//	doDrawPageOrientation();
		gInhibitPrintRecordsInfo = true;

		numberOfCopies = (*gTPrintHdl)->prJob.iCopies;
		firstPage = (*gTPrintHdl)->prJob.iFstPage;
		lastPage = (*gTPrintHdl)->prJob.iLstPage;

		(*gTPrintHdl)->prJob.iFstPage = 1;
		(*gTPrintHdl)->prJob.iLstPage = iPrPgMax;

		if(numberOfPages < lastPage)
		    lastPage = numberOfPages;

		for(copy=1;copy<numberOfCopies+1;copy++)
		{
		    for(page=firstPage;page<lastPage+1;page++)
		    {
			if((page - firstPage) % iPFMaxPgs == 0)
			{
			    if(page != firstPage)
			    {
				PrCloseDoc(printPortPtr);

				if(((*gTPrintHdl)->prJob.bJDocLoop == bSpoolLoop) && (PrError() == noErr))
				    PrPicFile(gTPrintHdl,NULL,NULL,NULL,&tprStatus);
			    }
			    printPortPtr = PrOpenDoc(gTPrintHdl,NULL,NULL);
			}
			if(PrError() == noErr)
			{
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

// doCreatePrintRecord

OSErr  doCreatePrintRecord(void)
{
    SInt16 printError;

    gTPrintHdl = (THPrint) NewHandleClear(sizeof(TPrint));
    if(gTPrintHdl != NULL )
    {
	PrintDefault(gTPrintHdl);
	printError = PrError();
	if(printError == noErr)
	    gPrintRecordInited = true;
	return(printError);
    }
    else
	ExitToShell();
}

//  doPrStyleDialog

void  doPrStyleDialog(void)
{
    SInt16  printError;

    PrOpen();

    printError = PrError();
    if(printError == noErr)
    {
	if(!gPrintRecordInited)
	{
	    printError = doCreatePrintRecord();
	    if(printError != noErr)
		doPrintError(printError,true);
	}

	PrStlDialog(gTPrintHdl);
    }
    else
	doPrintError(printError,false);

    PrClose();
}

//  doCalcNumberOfPages

SInt16  doCalcNumberOfPages(Rect pageRect)
{
    Rect		destRect, pictureRect;
    SInt16	heightDestRect, linesPerPage, numberOfPages;

    SetPort(&printPortPtr->gPort);

    SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right,
	    pageRect.bottom);
    OffsetRect(&destRect,- (kMargin - 5),- ((kMargin * 1.5) - 5));
    TextFont(gFontNumber);
    TextSize(gFontSize);

    gEditRecHdl = TENew(&destRect,&destRect);
    TEInsert(*gTextHdl,GetHandleSize(gTextHdl),gEditRecHdl);

    heightDestRect = destRect.bottom - destRect.top;
    linesPerPage = heightDestRect / (*gEditRecHdl)->lineHeight;
    numberOfPages = ((*gEditRecHdl)->nLines / linesPerPage) + 1;

    SetRect(&pictureRect,destRect.left,destRect.top,
	    destRect.left + ((*gPictureHdl)->picFrame.right -
			     (*gPictureHdl)->picFrame.left),
	    destRect.top + ((*gPictureHdl)->picFrame.bottom -
			    (*gPictureHdl)->picFrame.top));
    DrawPicture(gPictureHdl,&pictureRect);

    return(numberOfPages);
}

// doDrawPage

void  doDrawPage(Rect pageRect,SInt16 pageNumber,SInt16 numberOfpages)
{
    Rect	destRect, pictureRect;
    SInt16	heightDestRect, linesPerPage, numberOfLines;
    TEHandle	pageEditRecHdl;
    Handle	textHdl;
    SInt32	startOffset, endOffset;
    Str255	theString;
    short     	LeftVal, RightVal, TopVal, BottomVal;

    SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right,
	    pageRect.bottom);
    heightDestRect = destRect.bottom - destRect.top;
    linesPerPage = heightDestRect / (*gEditRecHdl)->lineHeight;
    numberOfLines = (*gEditRecHdl)->nLines;
    TextFont(gFontNumber);
    TextSize(gFontSize);
    pageEditRecHdl = TENew(&destRect,&destRect);
    textHdl = (*gEditRecHdl)->hText;

    startOffset = (*gEditRecHdl)->lineStarts[(pageNumber - 1) * linesPerPage];
    if(pageNumber == numberOfpages)
	endOffset = (*gEditRecHdl)->lineStarts[numberOfLines];
    else
	endOffset = (*gEditRecHdl)->lineStarts[pageNumber * linesPerPage];

    HLock(textHdl);
    TEInsert(*textHdl + startOffset,endOffset - startOffset,pageEditRecHdl);
    HUnlock(textHdl);

    if(pageNumber == 1)
    {

	LeftVal = ((destRect.right - destRect.left) -
		   ((*gPictureHdl)->picFrame.right -
		    (*gPictureHdl)-> picFrame.left))/2;
	TopVal = ((destRect.bottom - destRect.top) -
		  ((*gPictureHdl)->picFrame.bottom -
		   (*gPictureHdl)->picFrame.top))/2;
	RightVal = LeftVal +((*gPictureHdl)->picFrame.right -
			     (*gPictureHdl)->picFrame.left);
	BottomVal = TopVal + ((*gPictureHdl)->picFrame.bottom -
			      (*gPictureHdl)->picFrame.top);
	SetRect(&pictureRect,LeftVal,TopVal, RightVal, BottomVal);
	DrawPicture(gPictureHdl,&pictureRect);
    }

//  The following three lines is used to draw the page number.
//	MoveTo(destRect.left,pageRect.bottom - 25);
//	NumToString((SInt32) pageNumber,theString);
//	DrawString(theString);
}



// doGetPageOrientation

SInt16  doGetPageOrientation(void)
{
    TGetRotnBlk getRotRec;

    if(doIsPrGeneralThere)
    {
	getRotRec.iOpCode = getRotnOp;
	getRotRec.hPrint = gTPrintHdl;
	PrGeneral((Ptr) &getRotRec);
	if((getRotRec.iError == noErr) && (PrError() == noErr) &&
	   getRotRec.fLandscape)
	    return(1);
	else
	    return(2);
    }
    else
	return(3);
}

// doIsPrGeneralThere

Boolean  doIsPrGeneralThere(void)
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

// doPrintError

void  doPrintError(SInt16 printError,Boolean fatal)
{
    Str255 errorNumberString;

    NumToString((long) printError,errorNumberString);
    ParamText(errorNumberString,NULL,NULL,NULL);
    if(fatal)
    {
	StopAlert(rPrintAlert,NULL);
	ExitToShell();
    }
    else
	CautionAlert(rPrintAlert,NULL);
}


Boolean DoneDrawingPagesOfATextFile()
{
    short	i;
    Str255	aStringOfText;

    TextSize(10);
    TextFont(4);

    MoveTo(10, 14 * i);
    DrawString("\pHappy birthday");

    return TRUE;
}

void PrintStuff(void)
{
    GrafPtr	savedPort;
    TPrStatus	prStatus;
    TPPrPort	printPort;
    THPrint	hPrint;
    Boolean	ok;

    GetPort(&savedPort);
    PrOpen();
    hPrint = (THPrint)NewHandle(sizeof(TPrint));/* *not* sizeof(THPrint) */
    PrintDefault(hPrint);
    //if (DoPageSetUp)
    if (true)
	ok = PrStlDialog(hPrint);
    else
	ok = PrValidate(hPrint);
    ok = PrJobDialog(hPrint);
    if (ok)
    {
	printPort = PrOpenDoc(hPrint, nil, nil);
	SetPort(&printPort->gPort);
	PrOpenPage(printPort, nil);
	/*
	 * Now draw a single page.  You decide how many pages are drawn, and
	 * what is drawn in each page.  QuickDraw commands are automatically
	 * translated into commands to the printer. You could draw multiple pages
	 * like this.
	 */
	while (!DoneDrawingPagesOfATextFile()) {
	    PrClosePage(printPort);	/* Close the currently open page. */
	    PrOpenPage(printPort, nil);	/* and open a new one. */
	}
	PrClosePage(printPort);
	PrCloseDoc(printPort);
	/* Print spooled document, if any. */
	if ((**hPrint).prJob.bJDocLoop == bSpoolLoop && PrError() ==
	    noErr)
	    PrPicFile(hPrint, nil, nil, nil, &prStatus);
    }
    else {
	/* You will want to add error handling here... */
	SysBeep(5);
    }
    PrClose();
    SetPort(savedPort);
}


