/*
 *  R : A Computer Language for Statistical Data Analysis
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
 */


THPrint gTPrintHdl2;
Boolean gPrintStructureInited2 = false;


OSErr  doCreatePrintRecord2(void);

void  doPrStyleDialog2(void)
{
    SInt16 printError;

    PrOpen();

    printError = PrError();
    if(printError == noErr)
    {
	if(!gPrintStructureInited2)
	{
	    printError = doCreatePrintRecord2();
	}

	PrStlDialog(gTPrintHdl2);
    }

    PrClose();
}

OSErr  doCreatePrintRecord2(void)
{
    SInt16 printError;

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
