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

#ifndef __SCRAP__
#include <Scrap.h>
#endif

#ifndef __TEXTSERVICES__
#include <TextServices.h>
#endif

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

#ifndef __SMARTSCROLL__
#include "SmartScroll.h"
#endif

/*
#include "WEObjectHandlers.h"
*/

OSErr Initialize( void )
{
	SInt32			response;
	SInt16			i;
	OSErr			err;

	/* expand the zone to its maximum size
*/
	MaxApplZone( );

	/* allocate some extra master pointer blocks
*/
	for ( i = 0; i < 30; i++ )
	{
		MoreMasters( );
	}

   if((fileno(stdin)==0) || (fileno(stdout)==1)){
   
	/* initialize the Toolbox
*/
	InitGraf( &qd.thePort );
	InitFonts( );
	InitWindows( );
	InitMenus( );
	TEInit( );	/* tho we use WASTE for text stuff, dialogs, etc all use TextEdit so don't remove this! */
	InitDialogs( nil );
	InitCursor( );
	FlushEvents( everyEvent, 0 );

	/* if desk scrap is too large, unload it
*/
	if ( InfoScrap( )->scrapSize > kScrapThreshold )
	{
		UnloadScrap( );
	}

	/* make sure system software version is 7.0 or newer (classic 68K only)
*/
	#if !GENERATINGCFM
	if ( ( Gestalt( gestaltSystemVersion, &response ) != noErr ) || ( response < kMinSystemVersion ) )
	{
		SetCursor( &qd.arrow );
		response = Alert( kAlertNeedSys7, nil );
		return -1;
	}
	#endif

	/* make sure WASTELib got linked and we're using a recent version
	*/
	#if GENERATINGCFM
	if ( ( UInt32) & WEVersion == kUnresolvedCFragSymbolAddress )
		response = 0;
	else
	#endif
		response = WEVersion ( ) ;

	if ( response < kMinWASTEVersion )
	{
		SetCursor ( & qd . arrow ) ;
		response = Alert ( kAlertNeedNewerWASTE, nil ) ;
		return -1 ;
	}

	/* determine whether color Quickdraw is available
*/
	gHasColorQD = (Gestalt(gestaltQuickdrawVersion, &response) == noErr)
					&& (response >= gestalt8BitQD);

	/* determine whether the Drag Manager is available
*/
	gHasDragAndDrop = (Gestalt( gestaltDragMgrAttr, &response ) == noErr )
					&& BTST( response, gestaltDragMgrPresent );

#if GENERATINGCFM
	/* additional check needed if DragLib is weak-linked
	*/
	gHasDragAndDrop = gHasDragAndDrop && (&NewDrag != nil);
#endif

	/* determine whether the Text Services Manager is available
*/
	gHasTextServices = ( Gestalt( gestaltTSMgrVersion, &response ) == noErr );

	/* register this application with the TSM*/

	if ( gHasTextServices )
	{
		if ( ( err = InitTSMAwareApplication( ) ) != noErr )
			goto cleanup;
	}

	/* install default drag handlers
*/
	if ( gHasDragAndDrop )
	{
		if ( ( err = InstallDragHandlers( ) ) != noErr )
			goto cleanup;
	}

	/* install the sample object handlers for pictures and sounds
*/
/*
	if ((err = WEInstallObjectHandler(kTypePicture, weNewHandler,
				(UniversalProcPtr) NewWENewObjectProc(HandleNewPicture), nil)) != noErr)
		goto cleanup;

	if ((err = WEInstallObjectHandler(kTypePicture, weDisposeHandler,
				(UniversalProcPtr) NewWEDisposeObjectProc(HandleDisposePicture), nil)) != noErr)
		goto cleanup;

	if ((err = WEInstallObjectHandler(kTypePicture, weDrawHandler,
				(UniversalProcPtr) NewWEDrawObjectProc(HandleDrawPicture), nil)) != noErr)
		goto cleanup;
*/
/*	if ((err = WEInstallObjectHandler(kTypeSound, weNewHandler,
				(UniversalProcPtr) NewWENewObjectProc(HandleNewSound), nil)) != noErr)
		goto cleanup;

	if ((err = WEInstallObjectHandler(kTypeSound, weDrawHandler,
				(UniversalProcPtr) NewWEDrawObjectProc(HandleDrawSound), nil)) != noErr)
		goto cleanup;

	if ((err = WEInstallObjectHandler(kTypeSound, weClickHandler,
				(UniversalProcPtr) NewWEClickObjectProc(HandleClickSound), nil)) != noErr)
		goto cleanup;
*/

	/* perform other initialization chores
*/
	if ( ( err = InitializeEvents( ) ) != noErr )
		goto cleanup;

	if ( ( err = InitializeMenus( ) ) != noErr )
		goto cleanup;

	InitSmartScrollAwareApplication ( ) ;

	/* clear result code
	*/
	err = noErr;
/*    SIOUXSetTitle("\pR Console");
*/
	DoNew();


cleanup:
	if ( err != noErr )
		ErrorAlert( err );

}
	return err;
}

void Finalize( void )
{
	/* remove drag handlers
*/
	if ( gHasDragAndDrop )
	{
		RemoveDragHandlers( );
	}

	/* notify text services that we're closing down
*/
	if ( gHasTextServices )
	{
		CloseTSMAwareApplication( );
	}

	/* notify SmartScroll we're closing down
	*/
	CloseSmartScrollAwareApplication ( ) ;
}
