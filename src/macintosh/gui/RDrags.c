/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File WEDrags.c
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
 *   updated to last version of WasteLib library: Stefano M. Iacus, 2001
 *
 *  Original file was:
 *
 *	WASTE Demo Project:
	Drag Handlers

	Copyright © 1993-1998 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub
*/
#include <RCarbon.h>

#include "RIntf.h"

// static variables for the drag handlers UPPs

static DragTrackingHandlerUPP sMyTrackingHandlerUPP = nil;
static DragReceiveHandlerUPP sMyReceiveHandlerUPP = nil;

static pascal OSErr MyTrackingHandler( DragTrackingMessage message, WindowRef window, void *refCon, DragReference drag )
{
#pragma unused (refCon)

	DocumentHandle hDocument;

	if ( ( window != nil ) && ( ( hDocument = GetWindowDocument( window ) ) != nil ) )
	{
		return WETrackDrag( message, drag, (*hDocument)->we );
	}

	return noErr;
}

static pascal OSErr MyReceiveHandler( WindowRef window, void *refCon, DragReference drag )
{
#pragma unused (refCon)

	DocumentHandle	hDocument;

	if ( ( window != nil ) && ( ( hDocument = GetWindowDocument( window ) ) != nil ) )
	{
		return WEReceiveDrag( drag, (*hDocument)->we );
	}

	return noErr;
}

OSStatus InstallDragHandlers( void )
{
	OSStatus err;

	sMyTrackingHandlerUPP = NewDragTrackingHandlerUPP( MyTrackingHandler );
	sMyReceiveHandlerUPP = NewDragReceiveHandlerUPP( MyReceiveHandler );

	if ( ( err = InstallTrackingHandler( sMyTrackingHandlerUPP, nil, nil ) ) != noErr )
	{
		return err;
	}

	if ( ( err = InstallReceiveHandler( sMyReceiveHandlerUPP, nil, nil ) ) != noErr )
	{
		return err;
	}

	return noErr;
}

OSStatus RemoveDragHandlers( void )
{
	OSStatus err;

	if ( ( err = RemoveTrackingHandler( sMyTrackingHandlerUPP, nil ) ) != noErr )
	{
		return err;
	}
	if ( ( err = RemoveReceiveHandler( sMyReceiveHandlerUPP, nil ) ) != noErr )
	{
		return err;
	}

	return noErr;
}
