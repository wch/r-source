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
//#include <Printing.h>
#include <Resources.h>
#include <SegLoad.h>
#include <TextEdit.h>
#include <TextUtils.h>
#include <ToolUtils.h>
#include <PMDefinitions.h>
#include <PMApplication.h>

#include <Rintf.h>
#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>

//#include <PMCore.h>




#define kMargin			90
#define MAXLONG			0x7FFFFFFF

/* global variables
*/
//THPrint			gTPrintHdl;
WindowPtr		gWindowPtr;
TEHandle		gEditRecHdl;
Boolean			gDone;
Boolean			gPrintRecordInited = false;
Handle			gTextHdl;
PicHandle		gPictureHdl;
OSErr			err;
extern SInt16	gTextSize;
extern Boolean	PrintPicture;
extern Graphic_Ref		gGReference[MAX_NUM_G_WIN + 1];

/*------------------------------------------------------------------------------
	Globals
------------------------------------------------------------------------------*/
static	Handle	gflatPageFormat = NULL;		// used in FlattenAndSavePageFormat

CGrafPtr	printerPort=NULL;

/*------------------------------------------------------------------------------
	Prototypes
------------------------------------------------------------------------------*/
OSStatus 	DoPageSetup(void);
OSStatus 	DoPrintDialog(void);
OSStatus 	FlattenAndSavePageFormat(PMPageFormat pageFormat);
OSStatus 	LoadAndUnflattenPageFormat(PMPageFormat* pageFormat);
OSStatus	DetermineNumberOfPagesInDoc(PMPageFormat pageFormat, UInt32* numPages);
void 		DrawPage(PMPrintSession printSession, UInt32 pageNumber, Boolean addPostScript);
Boolean		IncludePostScriptInSpoolFile(PMPrintSession printSession);
void 		PostPrintingErrors(OSStatus status);
OSStatus 	DoTextPrint ( WindowRef inWindow );
static void PMRectToRect ( const PMRect * inPMRect, Rect * outRect );
static void CalculatePageRect(const Rect *inPaperRect,
				const PageMarginRec *inPageMargins,Rect *outPageRect);
static void ValidateMargins(const Rect *inPaperRect,const Rect *inMaxPageRect,
				PageMarginRec *ioMargins);
		


Boolean WeArePrinting = false;
Boolean WeArePasting = false;

/* function prototypes
*/
void	doInitManagers(void);
void	doEvents(EventRecord*);
void	doMouseDown(EventRecord*);
void	doMenuChoice(long);
void	printLoop(WindowPtr	window);

extern 	PMPageFormat	pageFormat;
extern 	PMPrintSettings	printSettings;
extern	PMPrintSession	printSession;




//	Define INCLUDE_POSTSCRIPT to exercise the LaserWriter 8 compatibility path
//	on Mac OS X (see IncludePostScriptInSpoolFile and DrawPage functions).
//	define	INCLUDE_POSTSCRIPT 1





/*------------------------------------------------------------------------------

    Function:	DoPageSetup
    
    Global Parameters:
        printSession	-	current printing session
        pageFormat	-	a PageFormat object addr
    
    Description:
        If the caller passes in an empty PageFormat object, DoPageSetupDialog
        creates a new one, otherwise it validates the one provided by the caller.
        It then invokes the Page Setup dialog and checks for Cancel. Finally it
        flattens the PageFormat object so it can be saved with the document.
        Note that the PageFormat object is modified by this function.
	
------------------------------------------------------------------------------*/
OSStatus 	DoPageSetup(void) //PMPrintSession printSession, PMPageFormat* pageFormat)
{
	OSStatus	status;
	Boolean		accepted;
	
	
	if(!printSession)
	 return;
	 
	//	Set up a valid PageFormat object.
	if (pageFormat == kPMNoPageFormat)
            {
            status = PMCreatePageFormat(&pageFormat);
	        //	Note that PMPageFormat is not session-specific, but calling
            //	PMSessionDefaultPageFormat assigns values specific to the printer
            //	associated with the current printing session.
            if ((status == noErr) && (pageFormat != kPMNoPageFormat))
                status = PMSessionDefaultPageFormat(printSession, pageFormat);
    
            }
	else
            status = PMSessionValidatePageFormat(printSession, pageFormat, kPMDontWantBoolean);

	//	Display the Page Setup dialog.	
	if (status == noErr)
            {
            status = PMSessionPageSetupDialog(printSession, pageFormat, &accepted);
   
            if (!accepted)
                status = kPMCancel;		// user clicked Cancel button
            }	
				
	//	If the user did not cancel, flatten and save the PageFormat object
	//	with our document.
	if (status == noErr)
            status = FlattenAndSavePageFormat(pageFormat);


	return status;
	
}	//	DoPageSetup



/*------------------------------------------------------------------------------
	Function:	DoPrintDialog
		
	Parameters:
		printSession	-	current printing session
		pageFormat		-	a PageFormat object addr
		printSettings	-	a PrintSettings object addr
			
	Description:
		If the caller passes an empty PrintSettings object, DoPrintDialog creates
		a new one, otherwise it validates the one provided by the caller.
		It then invokes the Print dialog and checks for Cancel.
		Note that the PrintSettings object is modified by this function.
		
------------------------------------------------------------------------------*/
OSStatus 	DoPrintDialog(void)//PMPrintSession printSession, PMPageFormat pageFormat, PMPrintSettings* printSettings)
{
	OSStatus	status;
	Boolean		accepted;
	UInt32		realNumberOfPagesinDoc;
	
	//	In this sample code the caller provides a valid PageFormat reference but in
	//	your application you may want to load and unflatten the PageFormat object
	//	that was saved at PageSetup time.  See LoadAndUnflattenPageFormat below.

	if(!printSession)
	 return;
	 
	//	Set up a valid PrintSettings object.
	if (printSettings == kPMNoPrintSettings)
            {
            status = PMCreatePrintSettings(&printSettings);	

            //	Note that PMPrintSettings is not session-specific, but calling
            //	PMSessionDefaultPrintSettings assigns values specific to the printer
            //	associated with the current printing session.
            if ((status == noErr) && (printSettings != kPMNoPrintSettings))
                status = PMSessionDefaultPrintSettings(printSession, printSettings);

            }
	else
            status = PMSessionValidatePrintSettings(printSession, printSettings, kPMDontWantBoolean);

             
  	//	Before displaying the Print dialog, we calculate the number of pages in the
	//	document.  On Mac OS X this is useful because we can prime the Print dialog
	//	with the actual page range of the document and prevent the user from entering
	//	out-of-range numbers.  This is not possible on Mac OS 8 and 9 because the driver,
	//	not the printing manager, controls the page range fields in the Print dialog.

	//	Calculate the number of pages required to print the entire document.
	if (status == noErr)
            status = DetermineNumberOfPagesInDoc(pageFormat, &realNumberOfPagesinDoc);


	//	Set a valid page range before displaying the Print dialog
	if (status == noErr)
            status = PMSetPageRange(printSettings, 1, realNumberOfPagesinDoc);


	//	Display the Print dialog.
	if (status == noErr)
            {
            status = PMSessionPrintDialog(printSession, printSettings, pageFormat, &accepted);
            if (!accepted)
                status = kPMCancel;		// user clicked Cancel button
            }
		
	return status;
	
}	//	DoPrintDialog


/*------------------------------------------------------------------------------
	Function:
		DoPrintLoop
	
	Parameters:
		printSession	-	current printing session
		pageFormat		-	a PageFormat object addr
		printSettings	-	a PrintSettings object addr
	
	Description:
		DoPrintLoop calculates which pages to print and executes the print
		loop, calling DrawPage for each page.
				
------------------------------------------------------------------------------*/
void	printLoop(WindowPtr	window)//PMPrintSession printSession, PMPageFormat pageFormat, PMPrintSettings printSettings)
{
    OSStatus	status=noErr, printError;
    Boolean	addPostScript = false;
    GrafPtr	currPort, printingPort;
    Rect	portRect;
    UInt32	realNumberOfPagesinDoc,
                pageNumber,
                firstPage,
                lastPage;
    		PicHandle WPicHandle=NULL;
  						CGrafPtr tempPort;
 						Rect	tempRect;            
    CFStringRef	jobName = CFSTR("R Graphics");

   if(!printSession)
    return;
    
   if (pageFormat == kPMNoPageFormat)
    DoPageSetup();
    
   if(!isGraphicWindow(window)){
   	DoTextPrint(window);
   	return;
   }
 
   if (status == noErr)
        status = DoPrintDialog();//printSession, pageFormat, &printSettings);

    //	Since this sample code doesn't have a window, give the spool file a name.
    status = PMSetJobNameCFString(printSettings, jobName);


    //	Get the user's Print dialog selection for first and last pages to print.
    if (status == noErr)
        {
        status = PMGetFirstPage(printSettings, &firstPage);

        if (status == noErr)
            status = PMGetLastPage(printSettings, &lastPage);


        }

    //	Check that the selected page range does not exceed the actual number of
    //	pages in the document.
    if (status == noErr)
        {
        status = DetermineNumberOfPagesInDoc(pageFormat, &realNumberOfPagesinDoc);


        if (realNumberOfPagesinDoc < lastPage)
            lastPage = realNumberOfPagesinDoc;
        }

    //	Before executing the print loop, tell the Carbon Printing Manager which pages
    //	will be spooled so that the progress dialog can reflect an accurate page count.
    //	This is recommended on Mac OS X.  On Mac OS 8 and 9, we have no control over
    //	what the printer driver displays.
	
    if (status == noErr)
        status = PMSetFirstPage(printSettings, firstPage, false);


    if (status == noErr)
        status = PMSetLastPage(printSettings, lastPage, false);

  
#if	INCLUDE_POSTSCRIPT
    //	Check if we can add PostScript to the spool file
    if (status == noErr)
    	addPostScript = IncludePostScriptInSpoolFile(printSession);
#endif
	
    //	Note, we don't have to worry about the number of copies.  The printing
    //	manager handles this.  So we just iterate through the document from the
    //	first page to be printed, to the last.
    if (status == noErr)
        {
        //	Establish a graphics context for drawing the document's pages.
        status = PMSessionBeginDocument(printSession, printSettings, pageFormat);
        if (status == noErr)
				{
            //	Print the selected range of pages in the document.		
            pageNumber = firstPage;
            while ((pageNumber <= lastPage) && (PMSessionError(printSession) == noErr))
                {
                //	Note, we don't have to deal with the classic Printing Manager's
                //	128-page boundary limit.
				
                //	Set up a page for printing.  Under the classic Printing Manager, applications
                //	could provide a page rect different from the one in the print record to achieve
                //	scaling. This is no longer recommended and on Mac OS X, the PageRect argument
                //	is ignored.
                status = PMSessionBeginPage(printSession, pageFormat, NULL);
                if (status != noErr)
                    break;
                    
                //	Save the current QD grafport.
                GetPort(&currPort);
                    
                //	Get the current graphics context, in this case a Quickdraw grafPort,
                //	for drawing the page.
                status = PMSessionGetGraphicsContext(printSession, kPMGraphicsContextQuickdraw, 
                 (void**) &printingPort);
                if (status == noErr) {
                       SInt16 WinIndex;
        				DevDesc *dd;
						MacDesc *xd;
						
						WinIndex = isGraphicWindow(window);
                       dd = (DevDesc*)gGReference[WinIndex].devdesc;
                       xd = (MacDesc*) dd->deviceSpecific;
					 
                   //  gGReference[WinIndex].printPort = printingPort;
                     gGReference[WinIndex].activePort = printingPort;
                     WeArePrinting = true;
                     xd->resize = true;
                     dd-> dp.resize(dd);
                        
                
 
                     playDisplayList(dd);
                     xd->resize = true;
                     WeArePrinting = false;
                     dd-> dp.resize(dd);  
    
    	        //	Restore the QD grafport.
                    SetPort(currPort);
                    }
				
                //	Close the page.
                status = PMSessionEndPage(printSession);
                if (status != noErr)
                    break;
					
                //	And loop.
                    pageNumber++;
                }
			
            // Close the printing port.  This dismisses the progress dialog on Mac OS X.
            (void)PMSessionEndDocument(printSession);
            }
        }
		
	//	Only report a printing error once we have completed the print loop. This
	//	ensures that every PMBeginXXX call is followed by a matching PMEndXXX
	//	call, so the Printing Manager can release all temporary memory and close
	//	properly.
	printError = PMSessionError(printSession);
	if (printError != noErr && printError != kPMCancel)
            PostPrintingErrors(printError);
		
}	//	DoPrintLoop



/*------------------------------------------------------------------------------
	Function:
		FlattenAndSavePageFormat
	
	Parameters:
		pageFormat	-	a PageFormat object
	
	Description:
		Flattens a PageFormat object so it can be saved with the document.
		Assumes caller passes a validated PageFormat object.
		
------------------------------------------------------------------------------*/
OSStatus FlattenAndSavePageFormat(PMPageFormat pageFormat)
{
    OSStatus	status;
    Handle	flatFormatHandle = NULL;
	
    //	Flatten the PageFormat object to memory.
    status = PMFlattenPageFormat(pageFormat, &flatFormatHandle);
	
    //	Write the PageFormat data to file.
    //	In this sample code we simply copy it to a global.	
    gflatPageFormat = flatFormatHandle;

    return status;
}	//	FlattenAndSavePageFormat



/*------------------------------------------------------------------------------
    Function:	LoadAndUnflattenPageFormat
	
    Parameters:
        pageFormat	- PageFormat object read from document file
	
    Description:
        Gets flattened PageFormat data from the document and returns a PageFormat
        object.
        The function is not called in this sample code but your application
        will need to retrieve PageFormat data saved with documents.
		
------------------------------------------------------------------------------*/
OSStatus	LoadAndUnflattenPageFormat(PMPageFormat* pageFormat)
{
    OSStatus	status;
    Handle	flatFormatHandle = NULL;

    //	Read the PageFormat flattened data from file.
    //	In this sample code we simply copy it from a global.
    flatFormatHandle = gflatPageFormat;

    //	Convert the PageFormat flattened data into a PageFormat object.
    status = PMUnflattenPageFormat(flatFormatHandle, pageFormat);
	
    return status;
}	//	LoadAndUnflattenPageFormat



/*------------------------------------------------------------------------------
    Function:	DetermineNumberOfPagesInDoc
	
    Parameters:
    	pageFormat	- a PageFormat object addr
		numPages	- on return, the size of the document in pages
			
    Description:
    	Calculates the number of pages needed to print the entire document.
		
------------------------------------------------------------------------------*/
OSStatus	DetermineNumberOfPagesInDoc(PMPageFormat pageFormat, UInt32* numPages)
{
    OSStatus	status;
    PMRect		pageRect;

    //	PMGetAdjustedPageRect returns the page size taking into account rotation,
    //	resolution and scaling settings.
    status = PMGetAdjustedPageRect(pageFormat, &pageRect);

    //	In this sample code we simply return a hard coded number.  In your application,
    //	you will need to figure out how many page rects are needed to image the
    //	current document.
    *numPages = 1;

    return status;
    
}	//	DetermineNumberOfPagesinDoc





/*------------------------------------------------------------------------------
    Function:	IncludePostScriptInSpoolFile
	
    Parameters:
        printSession	- current printing session
	
    Description:
    	Check if current printer driver supports embedding of PostScript in the spool file, and
        if it does, instruct the Carbon Printing Manager to generate a PICT w/ PS spool file.
                		
------------------------------------------------------------------------------*/
Boolean IncludePostScriptInSpoolFile(PMPrintSession printSession)
{
    Boolean	includePostScript = false;
    OSStatus	status;
    CFArrayRef	supportedFormats = NULL;
    SInt32	i, numSupportedFormats;

    // Get the list of spool file formats supported by the current driver.
    // PMSessionGetDocumentFormatGeneration returns the list of formats which can be generated
    // by the spooler (client-side) AND converted by the despooler (server-side).
    // PMSessionGetDocumentFormatSupported only returns the list of formats which can be converted
    // by the despooler.  The third argument to PMSessionGetDocumentFormatSupported specifies the
    // maximum number of formats to be returned in the list; zero gets every one.
    
    status = PMSessionGetDocumentFormatGeneration(printSession, &supportedFormats);
    if (status == noErr)
        {
        // Check if PICT w/ PS is in the list of supported formats.
        numSupportedFormats = CFArrayGetCount(supportedFormats);
        
        for (i=0; i<numSupportedFormats; i++)
            {
            if ( CFStringCompare(CFArrayGetValueAtIndex(supportedFormats, i),
                kPMDocumentFormatPICTPS, kCFCompareCaseInsensitive) == kCFCompareEqualTo )
                {
                // PICT w/ PS is supported, so tell the Printing Mgr to generate a PICT w/ PS spool file
                
                // Build an array of graphics contexts containing just one type, Quickdraw,
                // meaning that we will be using a QD port to image our pages in the print loop.
                CFStringRef	strings[1];
                CFArrayRef	arrayOfGraphicsContexts;
				
                strings[0] = kPMGraphicsContextQuickdraw;
                arrayOfGraphicsContexts = CFArrayCreate(CFAllocatorGetDefault(),
                        (const void **)strings, 1, &kCFTypeArrayCallBacks);
										
                if (arrayOfGraphicsContexts != NULL)
                        {
                        // Request a PICT w/ PS spool file
                        status = PMSessionSetDocumentFormatGeneration(printSession, kPMDocumentFormatPICTPS, 
                            arrayOfGraphicsContexts, NULL);
					
                        if (status == noErr)
                            includePostScript = true;	// Enable use of PS PicComments in DrawPage.

                        // Deallocate the array used for the list of graphics contexts.
                            CFRelease(arrayOfGraphicsContexts);
                        }
				
                    break;
                    }
            }
                    
        // Deallocate the array used for the list of supported spool file formats.
        CFRelease(supportedFormats);
	}
            
    return includePostScript;
}	//	IncludePostScriptInSpoolFile



/*------------------------------------------------------------------------------
    Function:	PostPrintingErrors
	
    Parameters:
        status	-	error code
	
    Description:
        This is where we could post an alert to report any problem reported
        by the Printing Manager.
		
------------------------------------------------------------------------------*/
void 	PostPrintingErrors(OSStatus status)
{
#pragma unused (status)	
}	//	PostPrintingErrors


OSStatus DoTextPrint ( WindowRef inWindow )
{
	DocumentHandle			hDocument ;
	WEReference				we ;
	WEPrintOptions			printOptions ;
	Handle					flattenedPageFormat = nil ;
	PMPrintSettings			textprintSettings = nil ;
	WEPrintSession			wasteSession = nil ;
	GrafPtr					printPort = nil ;
	PageMarginRecHandle		hPageMargins = nil ;
	PageMarginRec			pageMargins ;
	PMRect					pmRect ;
	Rect					paperRect ;
	Rect					printableArea ;
	UInt32					firstSelectedPage ;
	UInt32					lastSelectedPage ;
	SInt32					pageIndex ;
	SInt32					pageCount ;
	Boolean					accepted = false ;
	OSStatus				err ;

	hDocument = GetWindowDocument ( inWindow ) ;
	we = ( * hDocument ) -> we ;

    if(!printSession){
	//	start a new print session
		if ( ( err = PMCreateSession ( & printSession ) ) != noErr ){
			Rprintf("\n error create session");
			goto cleanup ;
		}
    }
	//	get the PMPageFormat object associated with our WASTE instance, if any
	if ( ( WEGetUserInfo ( kPageFormatTag, ( SInt32 * ) & flattenedPageFormat, we ) == noErr ) &&
		 ( flattenedPageFormat != nil ) )
	{
		//	pre-existing flattened page format data: unflatten it
		if ( ( err = PMUnflattenPageFormat ( flattenedPageFormat, & pageFormat ) ) != noErr )
		{ 
			goto cleanup ;
		}
	}
	else
	{
	     if (pageFormat == kPMNoPageFormat)
          DoPageSetup();
 	}

	//	get the page margins record associated with our WASTE instance
	if ( ( err = WEGetUserInfo ( kPageMarginsTag, ( SInt32 * ) & hPageMargins, we ) ) != noErr )
	{ 
		goto cleanup ;
	}
	pageMargins = ** hPageMargins ;

	//	create a print settings object
	if ( ( err = PMCreatePrintSettings ( & textprintSettings ) ) != noErr )
	{
		goto cleanup ;
	}
	if ( ( err = PMSessionDefaultPrintSettings ( printSession, textprintSettings ) ) != noErr )
	{
		goto cleanup ;
	}

	//	display the print dialog
	if ( ( err = PMSessionPrintDialog ( printSession, textprintSettings, pageFormat, & accepted ) ) != noErr )
	{
		goto cleanup ;
	}

	//	dialog accepted?
	err = userCanceledErr ;
	if ( ! accepted )
	{
		goto cleanup ;
	}

	//	get paper rectangle, rounded to nearest integral values
	if ( ( err = PMGetAdjustedPaperRect ( pageFormat, & pmRect ) ) != noErr )
	{
		goto cleanup ;
	}
	PMRectToRect ( & pmRect, & paperRect ) ;

	//	get printable page rectangle (actual page rect can't be larger than this)
	if ( ( err = PMGetAdjustedPageRect ( pageFormat, & pmRect ) ) != noErr )
	{
		goto cleanup ;
	}
	PMRectToRect ( & pmRect, & printableArea ) ;

	//	validate page margins
	ValidateMargins ( & paperRect, & printableArea, & pageMargins ) ;

	//	zero out the print options record (this is important for future compatibility!)
	BlockZero ( & printOptions, sizeof ( printOptions ) ) ;

	//	calculate page rectangle
	CalculatePageRect ( & paperRect, & pageMargins, & printOptions . pageRect ) ;

	//	copy it to a PMRect for PMBeginPage
	pmRect . left = printOptions . pageRect . left ;
	pmRect . top = printOptions . pageRect . top ;
	pmRect . right = printOptions . pageRect . right ;
	pmRect . bottom = printOptions . pageRect . bottom ;

	//	create a WASTE print session
	if ( ( err = WENewPrintSession ( & printOptions, we, & wasteSession ) ) != noErr )
	{
		goto cleanup ;
	}

	//	get page count
	pageCount = WECountPages ( wasteSession ) ;

	//	get page range selected by the user
	if ( ( err = PMGetFirstPage ( textprintSettings, & firstSelectedPage ) ) != noErr )
	{
		goto cleanup ;
	}
	if ( ( err = PMGetLastPage ( textprintSettings, & lastSelectedPage ) ) != noErr )
	{
		goto cleanup ;
	}

	//	sanity checks
	if ( lastSelectedPage > pageCount )
	{
		lastSelectedPage = pageCount ;
	}

	//	begin document
	if ( ( err = PMSessionBeginDocument ( printSession, textprintSettings, pageFormat ) ) != noErr )
	{
		goto cleanup ;
	}

	//	WASTE page indices are zero-based, while Printing Manager's are one-based
	firstSelectedPage -= 1 ;
	lastSelectedPage -= 1 ;

	//	page loop
	for ( pageIndex = firstSelectedPage ; pageIndex <= lastSelectedPage ; pageIndex ++ )
	{
		//	begin page
		if ( ( err = PMSessionBeginPage ( printSession, pageFormat, & pmRect ) ) != noErr )
		{
			goto cleanup ;
		}

		//	get the graphics port associated with the print context
		if ( ( err = PMSessionGetGraphicsContext ( printSession, nil, & printPort ) ) != noErr )
		{
			goto cleanup ;
		}

		//	print this page
		if ( ( err = WEPrintPage ( pageIndex, printPort, nil, wasteSession ) ) != noErr )
		{
			goto cleanup ;
		}

		//	end page
		if ( ( err = PMSessionEndPage ( printSession ) ) != noErr )
		{
			goto cleanup ;
		}
	}

	//	end document
	if ( ( err = PMSessionEndDocument ( printSession ) ) != noErr )
	{
		goto cleanup ;
	}

	//	clear result code
	err = noErr ;

cleanup :
	//	clean up
	if ( wasteSession )
	{
		WEDisposePrintSession ( wasteSession ) ;
		wasteSession = nil ;
	}

	if ( textprintSettings )
	{
		PMRelease ( textprintSettings ) ;
		textprintSettings = nil ;
	}

 
	//	return result code
	return err ;
}


static void ValidateMargins
	(
		const Rect *		inPaperRect,		//	paper rectangle
		const Rect *		inMaxPageRect,		//	maximum printable area
		PageMarginRec *		ioMargins			//	page margins
	)
{
	const Fixed			kMinPageWidth = ( 36 << 16 ) ;		//	half inch
	const Fixed			kMinPageHeight = ( 36 << 16 ) ;		//	half inch
	PageMarginRec		minMargins ;
	PageMarginRec		maxMargins ;

	//	find the narrowest possible margins allowed by the specified print record
	minMargins.left = ( inMaxPageRect->left - inPaperRect->left ) << 16 ;
	minMargins.right = ( inPaperRect->right - inMaxPageRect->right ) << 16 ;
	minMargins.top = ( inMaxPageRect->top - inPaperRect->top ) << 16 ;
	minMargins.bottom = ( inPaperRect->bottom - inMaxPageRect->bottom ) << 16 ;

	//	and the widest possible margins
	maxMargins.left = ( ( ( inPaperRect->right - inPaperRect->left ) << 16 ) - kMinPageWidth ) / 2 ;
	maxMargins.top = ( ( ( inPaperRect->bottom - inPaperRect->top ) << 16 ) - kMinPageHeight ) / 2 ;
	maxMargins.right = maxMargins.left ;
	maxMargins.bottom = maxMargins.top ;

	//	make sure the margins aren't too wide or too narrow
	if ( ioMargins->top < minMargins.top )
	{
		ioMargins->top = minMargins.top ;
	}
	else if ( ioMargins->top > maxMargins.top )
	{
		ioMargins->top = maxMargins.top ;
	}

	if ( ioMargins->bottom < minMargins.bottom )
	{
		ioMargins->bottom = minMargins.bottom ;
	}
	else if ( ioMargins->bottom > maxMargins.bottom )
	{
		ioMargins->bottom = maxMargins.bottom ;
	}

	if ( ioMargins->left < minMargins.left )
	{
		ioMargins->left = minMargins.left ;
	}
	else if ( ioMargins->left > maxMargins.left )
	{
		ioMargins->left = maxMargins.left ;
	}

	if ( ioMargins->right < minMargins.right )
	{
		ioMargins->right = minMargins.right ;
	}
	else if ( ioMargins->right > maxMargins.right )
	{
		ioMargins->right = maxMargins.right ;
	}
}

static void CalculatePageRect
	(
		const Rect *				inPaperRect,
		const PageMarginRec *		inPageMargins,
		Rect *						outPageRect
	)
{
	Rect marginRect ;

	//	get page margins rounded to nearest integral values

	marginRect.left = FixRound ( inPageMargins->left ) ;
	marginRect.top = FixRound ( inPageMargins->top ) ;
	marginRect.right = FixRound ( inPageMargins->right ) ;
	marginRect.bottom = FixRound ( inPageMargins->bottom ) ;

	//	calculate page rect (the destination rectangle for printing)
	//	based on the paper rect and page margins
	//	all values are expressed in integral PostScript points (1/72 inch)

	outPageRect->left = ( inPaperRect->left + marginRect.left ) ;
	outPageRect->top = ( inPaperRect->top + marginRect.top ) ;
	outPageRect->right = outPageRect->left + ( ( inPaperRect->right - inPaperRect->left ) - ( marginRect.left + marginRect.right ) ) ;
	outPageRect->bottom = outPageRect-> top + ( ( inPaperRect->bottom - inPaperRect->top ) - ( marginRect.top + marginRect.bottom ) ) ;
}


static void PMRectToRect ( const PMRect * inPMRect, Rect * outRect )
{
	outRect->left = inPMRect->left ;
	outRect->top = inPMRect->top ;
	outRect->right = inPMRect->right ;
	outRect->bottom = inPMRect->bottom ;
}



