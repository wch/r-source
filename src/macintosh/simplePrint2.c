THPrint		gTPrintHdl2;
Boolean		gPrintStructureInited2				= false;


OSErr  doCreatePrintRecord2(void);
// ×××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doPrStyleDialog

void  doPrStyleDialog2(void)
{
	SInt16	printError;

	PrOpen();

	printError = PrError();
	if(printError == noErr)
	{
		if(!gPrintStructureInited2)
		{
			printError = doCreatePrintRecord2();
			//if(printError != noErr)
			    //RWrite("Print Error!");
				//doErrorAlert(printError,true);
		}

		PrStlDialog(gTPrintHdl2);
	}
//	else
//	    RWrite("Print Error 2!");
//		doErrorAlert(printError,false);

	PrClose();
}

// ×××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××××× doCreatePrintRecord

OSErr  doCreatePrintRecord2(void)
{
	SInt16	printError;

	gTPrintHdl2 = (THPrint) NewHandleClear(sizeof(TPrint));
	if(gTPrintHdl2 != NULL )
	{
		PrintDefault(gTPrintHdl2);
		printError = PrError();
		if(printError == noErr)
			gPrintStructureInited2 = true;
		return(printError);
	}
	else
		ExitToShell();
}
