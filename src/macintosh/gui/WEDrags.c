/*
    Code Adapted from:
    
	WASTE Demo Project:
	Macintosh Controls with Long Values

	Copyright © 1993-1998 Marco Piovanelli
	All Rights Reserved

	C port by John C. Daub
*/

#include "RIntf.h"

// static variables for the drag handlers UPPs

static DragTrackingHandlerUPP sMyTrackingHandlerUPP = nil;
static DragReceiveHandlerUPP sMyReceiveHandlerUPP = nil;

static pascal OSErr MyTrackingHandler( DragTrackingMessage message, WindowPtr window, void *refCon, DragReference drag )
{
#pragma unused (refCon)

	DocumentHandle hDocument;

	if ( ( window != nil ) && ( ( hDocument = GetWindowDocument( window ) ) != nil ) )
	{
		return WETrackDrag( message, drag, (*hDocument)->we );
	}

	return noErr;
}

static pascal OSErr MyReceiveHandler( WindowPtr window, void *refCon, DragReference drag )
{
#pragma unused (refCon)

	DocumentHandle	hDocument;

	if ( ( window != nil ) && ( ( hDocument = GetWindowDocument( window ) ) != nil ) )
	{
		return WEReceiveDrag( drag, (*hDocument)->we );
	}

	return noErr;
}

OSErr InstallDragHandlers( void )
{
	OSErr err;

	sMyTrackingHandlerUPP = NewDragTrackingHandlerProc( MyTrackingHandler );
	sMyReceiveHandlerUPP = NewDragReceiveHandlerProc( MyReceiveHandler );

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

OSErr RemoveDragHandlers( void )
{
	OSErr err;

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
