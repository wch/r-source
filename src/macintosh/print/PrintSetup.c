THPrint		gTPrintHdl;
Boolean		gPrintRecordInited = false;
Handle		gTextHdl;

OSErr  doCreatePrintRecord(void);
// ×××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doPrStyleDialog

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


// ×××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doCreatePrintRecord

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

// ××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doGetPageOrientation

// ×××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doCalcNumberOfPages

SInt16  doCalcNumberOfPages(Rect pageRect)
{
	Rect		destRect, pictureRect;
	SInt16	heightDestRect, linesPerPage, numberOfPages;
    GrafPtr savePort;

    GetPort(&savePort);
    SetRect(&destRect, pageRect.left, pageRect.top, pageRect.right, pageRect.bottom);	
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
