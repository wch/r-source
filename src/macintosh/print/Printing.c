/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file Printing.c
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

#include "Printing.h"

/* global variables */

THPrint		gTPrintHdl;
WindowPtr	gWindowPtr;
Boolean		gDone;
Boolean		gPrintStructureInited				= false;
Boolean		gInhibitPrintStructuresInfo	= false;
TEHandle	gEditRecHdl;
Handle		gTextHdl;
PicHandle	gPictureHdl;
RGBColor	gWhiteColour	= { 0xFFFF, 0xFFFF, 0xFFFF };
RGBColor	gBlackColour	= { 0x0000, 0x0000, 0x0000 };
RGBColor	gBlueColour		= { 0x1818, 0x4B4B, 0x8181 };

extern TPPrDlg				gTPrDlgStructurePtr;
/*  extern PDlgInitUPP 		gInitialisationFunctionUPP; */
extern PItemUPP				gNewItemEvaluateFunctionUPP;
extern ModalFilterUPP	gEventFilterUPP;
extern void Do_StandardAlert(Str255);


void  doUpdate(EventRecord *eventStrucPtr)
{
    WindowPtr	windowPtr;

    windowPtr = (WindowPtr) eventStrucPtr->message;

    BeginUpdate(windowPtr);
    /* doPrintRecordsInfo(); */
    EndUpdate(windowPtr);
}


void  doPrinting(void)
{
    SInt16	printError;
    Boolean	userClickedOK;
    SInt16	numberOfPages, numberOfCopies;
    SInt16	firstPage, lastPage, copy, page;
    TPPrPort	printPortPtr;
    TPrStatus	tprStatus;
    GrafPtr     savePort;

    GetPort(&savePort);
    PrOpen();
    if(PrError() == noErr)
    {
	if(!gPrintStructureInited)
	    printError = doCreatePrintRecord();
	else
	    printError = noErr;
        if (gTPrintHdl != nil)
            DisposeHandle((char **)gTPrintHdl);
        gTPrintHdl = (THPrint) NewHandle(sizeof(TPrint));
        PrintDefault(gTPrintHdl);
	gTPrDlgStructurePtr = PrJobInit(gTPrintHdl);
	printError = PrError();

	if(printError == noErr)
	{

	    gEventFilterUPP = NewModalFilterProc((ProcPtr) eventFilter);

	    userClickedOK = PrJobDialog(gTPrintHdl);

	    DisposeRoutineDescriptor(gNewItemEvaluateFunctionUPP);
	    DisposeRoutineDescriptor(gEventFilterUPP);

	    if(userClickedOK)
	    {
		gInhibitPrintStructuresInfo = true;
		numberOfPages = doCalcNumberOfPages((*gTPrintHdl)->prInfo.rPage);

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
			    SetPort(&printPortPtr ->gPort);
			}
			if(PrError() == noErr)
			{
			    PrOpenPage(printPortPtr,NULL);
			    if(PrError() == noErr)
				doDrawPage((*gTPrintHdl)->prInfo.rPage,page,numberOfPages);

			    PrClosePage(printPortPtr);
			    SetPort(savePort);
			}
		    }

		    PrCloseDoc(printPortPtr);
		    SetPort(savePort);
		    if(((*gTPrintHdl)->prJob.bJDocLoop == bSpoolLoop) && (PrError() == noErr))
			PrPicFile(gTPrintHdl,NULL,NULL,NULL,&tprStatus);
		}

	    }
	}
    }

    printError = PrError();
    PrClose();

    if(printError != noErr && printError != iPrAbort)
	doErrorAlert(printError,false);
}

OSErr  doCreatePrintRecord(void)
{
    SInt16	printError;

    gTPrintHdl = (THPrint) NewHandleClear(sizeof(TPrint));
    if(gTPrintHdl != NULL )
    {
	PrintDefault(gTPrintHdl);
	printError = PrError();
	if(printError == noErr)
	    gPrintStructureInited = true;
	return(printError);
    }
    else
	ExitToShell();
}


void  doPrStyleDialog(void)
{
    SInt16	printError;

    PrOpen();

    printError = PrError();
    if(printError == noErr)
    {
	if(!gPrintStructureInited)
	{
	    printError = doCreatePrintRecord();
	    if(printError != noErr)
		doErrorAlert(printError,true);
	}

	PrStlDialog(gTPrintHdl);
    }
    else
	doErrorAlert(printError,false);

    PrClose();
}

SInt16  doCalcNumberOfPages(Rect pageRect)
{
    Rect	destRect, pictureRect;
    SInt16	heightDestRect, linesPerPage, numberOfPages;

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

    /* SetRect(&pictureRect,destRect.left,destRect.top, */
    /* 			destRect.left + ((*gPictureHdl)->picFrame.right -
			(*gPictureHdl)->picFrame.left), */
    /* 			destRect.top + ((*gPictureHdl)->picFrame.bottom -
			(*gPictureHdl)->picFrame.top));	 */
    /* DrawPicture(gPictureHdl,&pictureRect); */
    return 1;
    return(numberOfPages);
}

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
    textHdl = gTextHdl; /* (*gEditRecHdl)->hText; */

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
#ifdef NotToTheCenter
	SetRect(&pictureRect,destRect.left,destRect.top,
		destRect.left + ((*gPictureHdl)->picFrame.right - (*gPictureHdl)->picFrame.left),
		destRect.top + ((*gPictureHdl)->picFrame.bottom - (*gPictureHdl)->picFrame.top));
#endif
	LeftVal = ((destRect.right - destRect.left) - ((*gPictureHdl)->picFrame.right -(*gPictureHdl)-> picFrame.left))/2;
	TopVal = ((destRect.bottom - destRect.top) - ((*gPictureHdl)->picFrame.bottom -(*gPictureHdl)->picFrame.top))/2;
	RightVal = LeftVal +((*gPictureHdl)->picFrame.right - (*gPictureHdl)->picFrame.left);
	BottomVal = TopVal + ((*gPictureHdl)->picFrame.bottom - (*gPictureHdl)->picFrame.top);
	SetRect(&pictureRect,LeftVal,TopVal, RightVal, BottomVal);
	DrawPicture(gPictureHdl,&pictureRect);
    }

/*   The following three lines is used to draw the page number. */
/* 	MoveTo(destRect.left,pageRect.bottom - 25); */
/* 	NumToString((SInt32) pageNumber,theString); */
/* 	DrawString(theString); */
}

void  doDrawPageOrientation(void)
{
#ifdef FFFFF
    SInt16	orientation;

    MoveTo(20,260);
    DrawString("\pOrientation selected:");

    orientation = doGetPageOrientation();

    MoveTo(190,260);
    if(orientation == 1)
	DrawString("\pLandscape");
    else if(orientation == 2)
	DrawString("\pPortrait");
    else
	DrawString("\p(PrGeneral not supported by driver)");
#endif
}


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

void  doDrawRectStrings(Str255 s1, SInt16 x1, SInt16 y1, Str255 s2,
			SInt16 x2, SInt16 y2, Str255 s3)
{
    MoveTo(x1,y1);
    DrawString(s1);
    MoveTo(x2,y2);
    DrawString("\p(");
    DrawString(s2);
    DrawString("\p,");
    DrawString(s3);
    DrawString("\p)");
}


void  doErrorAlert(SInt16 errorType,Boolean fatal)
{
    AlertStdAlertParamRec paramRec;
    Str255 theString, errorString = "\pPrinting Manager Error ";
    SInt16 itemHit;

    paramRec.movable		= true;
    paramRec.helpButton		= false;
    paramRec.filterProc		= NULL;
    paramRec.defaultText	= (StringPtr) kAlertDefaultOKText;
    paramRec.cancelText		= NULL;
    paramRec.otherText		= NULL;
    paramRec.defaultButton	= kAlertStdAlertOKButton;
    paramRec.cancelButton	= 0;
    paramRec.position		= kWindowDefaultPosition;

    NumToString((long) errorType,theString);
    doConcatPStrings(errorString,theString);

    if(!fatal){
	Do_StandardAlert(errorString);
	/* StandardAlert(kAlertCautionAlert,errorString,NULL,&paramRec,&itemHit); */
    }else
    {
	Do_StandardAlert(errorString);
	/* StandardAlert(kAlertStopAlert,errorString,NULL,&paramRec,&itemHit); */
	ExitToShell();
    }
}


void  doConcatPStrings(Str255 targetString,Str255 appendString)
{
    SInt16 appendLength;

    appendLength = MIN(appendString[0],255 - targetString[0]);
    if(appendLength > 0)
    {
	BlockMoveData(appendString+1, targetString+targetString[0]+1,
		      (SInt32) appendLength);
	targetString[0] += appendLength;
    }
}


void PrintPicture()
{
    GrafPtr	savePort;
    TPrStatus	prStatus;
    TPPrPort	printPort;
    OSErr	err, printError;
    THPrint	hPrint;

    GetPort(&savePort);
    PrOpen();
    if(!gPrintStructureInited)
	printError = doCreatePrintRecord();
    else
	printError = noErr;

    /* hPrint = (THPrint) NewHandle(sizeof(TPrint)); */
    PrintDefault(gTPrintHdl);
    /* ClipRect ((*gTPrintHdl)->prInfo.rPage); */

    if (PrJobDialog(hPrint)) {
	printPort = PrOpenDoc(gTPrintHdl, nil, nil);
	SetPort(&printPort->gPort);
	PrOpenPage(printPort, nil);		/*  Open this page ... */
	/*  --------- */
	DrawPicture(gPictureHdl, &((*gTPrintHdl)->prInfo.rPage));
	/*  Or any other drawing */
	/*  commands ... */
	/*  --------- */
	PrClosePage(printPort);          	/*  Close this page ... */
	PrCloseDoc(printPort);
	/*  Handle print spooler */
	if (((*gTPrintHdl)->prJob.bJDocLoop = bSpoolLoop) && (!PrError() ) )
	    PrPicFile(hPrint, nil, nil, nil, &prStatus);
    }
    PrClose();
    SetPort(savePort);
}
