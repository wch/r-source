/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file PrintSetup.c
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

THPrint		gTPrintHdl;
Boolean		gPrintRecordInited = false;
Handle		gTextHdl;

OSErr  doCreatePrintRecord(void);

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
	    //if(printError != noErr)
	    //doPrintError(printError,true);
	}

	PrStlDialog(gTPrintHdl);
    }
    //else
    //doPrintError(printError,false);

    PrClose();
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
	    gPrintRecordInited = true;
	return(printError);
    }
    else
	ExitToShell();
}

void  printLoop(void)
{
    GrafPtr	oldPort;
    SInt16	printError;
    SInt16	numberOfPages, numberOfCopies;
    Boolean	userClickedOK;
    SInt16	firstPage, lastPage, copy, page;

    TPrStatus	tprStatus;

    GetPort(&oldPort);

    PrOpen();
    if(PrError() == noErr)
    {
	// Setup a default Print record if you have no record.
	if(!gPrintRecordInited)
	    printError = doCreatePrintRecord();
	else
	    printError = noErr;

	if(printError == noErr)
	{
	    // Using the rPage info to calculate the number of page.
	    numberOfPages = doCalcNumberOfPages((*gTPrintHdl)->prInfo.rPage);

            // Prompt out the Print dialog
	    userClickedOK = PrJobDialog(gTPrintHdl);

	    //if the user click OK
	    if(userClickedOK)
	    {
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

SInt16  doCalcNumberOfPages(Rect pageRect)
{
    Rect destRect, pictureRect;
    SInt16 heightDestRect, linesPerPage, numberOfPages;
    GrafPtr savePort;

    GetPort(&savePort);
    SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right,
	    pageRect.bottom);
    OffsetRect(&destRect,- (kMargin - 5),- ((kMargin * 1.5) - 5));
    TextFont(4);
    TextSize(9);
    gEditRecHdl = TENew(&destRect,&destRect);
    TEInsert(*gTextHdl,GetHandleSize(gTextHdl),gEditRecHdl);

    heightDestRect = destRect.bottom - destRect.top;
    linesPerPage = heightDestRect / (*gEditRecHdl)->lineHeight;
    numberOfPages = ((*gEditRecHdl)->nLines / linesPerPage) + 1;

    // SetRect(&pictureRect,destRect.left,destRect.top,
    //			destRect.left + ((*gPictureHdl)->picFrame.right - (*gPictureHdl)->picFrame.left),
    //			destRect.top + ((*gPictureHdl)->picFrame.bottom - (*gPictureHdl)->picFrame.top));
    //DrawPicture(gPictureHdl,&pictureRect);
    SetPort(savePort);
    return(numberOfPages);
}
