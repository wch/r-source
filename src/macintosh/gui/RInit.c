/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RInit.c
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
 * 
 *	WASTE Demo Project:
 *	Initialization and Finalization Routines
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */


#ifndef __DIALOGS__
#include <Dialogs.h>
#endif

#ifndef __FONTS__
#include <Fonts.h>
#endif

#ifndef __CODEFRAGMENTS__
#include <CodeFragments.h>
#endif

#ifndef __GESTALT__
#include <Gestalt.h>
#endif

#ifndef __TEXTSERVICES__
#include <TextServices.h>
#endif

#ifndef __SCRAP__
#include <Scrap.h>
#endif


#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

#ifndef __SMARTSCROLL__
#include "SmartScroll.h"
#endif

#include <PMApplication.h>
//#include <PMDefinitions.h>
//#include <PMCore.h>

/*
#include "WEObjectHandlers.h"
*/

extern WindowPtr            Console_Window;

static Boolean CheckVersion ( SInt16 alertStringIndex, UInt32 installedVersion, UInt32 requiredVersion )
{
	Str255		banner ;
	Str255		explanation ;
	Str15		versionString ;
	SInt16		alertResult ;

	if ( installedVersion < requiredVersion )
	{
		GetIndString ( banner, kAlertStringsID, alertStringIndex ) ;
		GetIndString ( explanation, kAlertStringsID, alertStringIndex + 1 ) ;
		NumToVersionString ( installedVersion, versionString ) ;
		ReplaceParam ( explanation, versionString, 0 ) ;
		NumToVersionString ( requiredVersion, versionString ) ;
		ReplaceParam ( explanation, versionString, 1 ) ;
		StandardAlert ( kAlertStopAlert, banner, explanation, nil, & alertResult ) ;
		return true ;
	}

	return false ;
}

extern WindowPtr gWindowPtrArray[kMaxWindows + 2];

    PMPageFormat	pageFormat = kPMNoPageFormat;
    PMPrintSettings	printSettings = kPMNoPrintSettings;
    PMPrintSession	printSession;

SInt32		systemVersion ;
SInt32		carbonVersion ;

OSErr Initialize( void )
{
	SInt32			response;
	SInt16			i;
	OSErr			err;
    
	/* expand the zone to its maximum size */
	


    if((fileno(stdin)==0) || (fileno(stdout)==1)){

#if ! TARGET_API_MAC_CARBON

	/* allocate some extra master pointer blocks */
	for ( i = 0; i < 30; i++ )
	{
		MoreMasters( );
	}

    MaxApplZone( );

	/* initialize the Toolbox */
	InitGraf( &qd.thePort );
	InitFonts( );
	InitWindows( );
	InitMenus( );
	TEInit( );	/* tho we use WASTE for text stuff, dialogs, etc all use TextEdit so don't remove this! */
	InitDialogs( nil );
	InitCursor( );
	FlushEvents( everyEvent, 0 );


	/* if desk scrap is too large, unload it */
	if ( InfoScrap( )->scrapSize > kScrapThreshold )
	{
		UnloadScrap( );
	}
#else
    MoreMasterPointers(30);	
#endif

    // We need CarbonLib v 1.2.5 or Higher
	Gestalt ( gestaltCarbonVersion, & carbonVersion ) ;

    carbonVersion = ( carbonVersion << 16 ) | 0x8000 ;
    if(carbonVersion < 0x01208000){
     R_ShowMessage("You need CarbonLib 1.2.0 or newer\nto run R");
     return -1;
    } 
    
//	make sure we're using a recent version of the system software
	Gestalt ( gestaltSystemVersion, & systemVersion ) ;
	systemVersion = ( systemVersion << 16 ) | 0x8000 ;
	if ( CheckVersion ( 1, systemVersion, kMinSystemVersion ) )
	{
		return -1 ;
	}

	//	make sure we're using a recent version of WASTELib
	if ( CheckVersion ( 3, WEVersion ( ), kMinWASTEVersion ) )
	{
		return -1 ;
	}

	/* determine whether color Quickdraw is available */

	/* determine whether the Drag Manager is available */
	gHasDragAndDrop = (Gestalt( gestaltDragMgrAttr, &response ) == noErr )
					&& BTST( response, gestaltDragMgrPresent );
					
	/* determine whether the Text Services Manager is available*/
	gHasTextServices = ( Gestalt( gestaltTSMgrVersion, &response ) == noErr );

	/* register this application with the TSM*/
#if ! TARGET_API_MAC_CARBON

	if ( gHasTextServices )
	{
		if ( ( err = InitTSMAwareApplication( ) ) != noErr )
			goto cleanup;
	}
#endif /* ! TARGET_API_MAC_CARBON */

	/* install default drag handlers */
	if ( gHasDragAndDrop )
	{
		if ( ( err = InstallDragHandlers( ) ) != noErr )
			goto cleanup;
	}

	/* perform other initialization chores */
	if ( ( err = InitializeEvents( ) ) != noErr )
		goto cleanup;

	if ( ( err = InitializeMenus( ) ) != noErr )
		goto cleanup;

    /* initialize the print session */
    if( (err = PMCreateSession(&printSession) ) != noErr)
       printSession =NULL;
    
    
	InitSmartScrollAwareApplication ( ) ;

	for(i = 0; i < kMaxWindows+2; i++)
	    gWindowPtrArray[i] = NULL;
	    
    /* clear result code	*/

  	err = noErr;


cleanup:
	if ( err != noErr )
		ErrorAlert( err );
    }
    else
     err = noErr;

	return err;
}

void Finalize( void )
{

    //	Release the PageFormat and PrintSettings objects.  PMRelease decrements the
    //	ref count of the allocated objects.  We let the Printing Manager decide when
    //	to release the allocated memory.
    if (pageFormat != kPMNoPageFormat)
        (void)PMRelease(pageFormat);
    if (printSettings != kPMNoPrintSettings)
        (void)PMRelease(printSettings);
    
    //	Terminate the current printing session. 
    if(printSession)
     (void)PMRelease(printSession);

	/* remove drag handlers
*/
	if ( gHasDragAndDrop )
	{
		RemoveDragHandlers( );
	}

	/* notify text services that we're closing down
*/
#if ! TARGET_API_MAC_CARBON

	if ( gHasTextServices )
	{
		CloseTSMAwareApplication( );
	}
#endif /* ! TARGET_API_MAC_CARBON */

	/* notify SmartScroll we're closing down
	*/
	CloseSmartScrollAwareApplication ( ) ;
}
