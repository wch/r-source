/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File DialogUtils.c
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
 *	Dialog Utilities
 *
 *	Copyright © 1993-1998 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 */

#ifndef __DIALOGS__
#include <Dialogs.h>
#endif

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

static pascal Boolean MyStandardDialogFilter( DialogPtr dialog, EventRecord *event, SInt16 *item )
{
	GrafPtr				savePort;
	ModalFilterUPP		stdFilter = nil;
	Boolean				retval = false;
	OSErr				err;

	/* set up the port */
	GetPort( &savePort );
	SetPort( dialog );

	/* 	intercept window events directed to windows behind the dialog */
	if ( ( event->what == updateEvt ) || ( event->what == activateEvt ) )
	{
		if ( ((WindowPtr) event->message) != dialog )
		{
			DoWindowEvent( event );
		}
	}

	/* is the default item a pushbutton? */
	if ( GetDialogItemType( dialog, GetDialogDefaultItem( dialog ) ) == kButtonDialogItem )
	{
		/* yes, so tell the Dialog Manager to care about its outline */
		SetDialogDefaultItem( dialog, GetDialogDefaultItem( dialog ));
	}

 /* this is something not in the original WASTE Demo App, but in the work that I've done
	on my own projects, I've found it useful and helpful.

	let's also make sure the cancel button can be handled...now, the cancel button
	should be dialog item #2.  So, we get dialog item #2, check if it's a button.
	if it fills these 2 criteria, it's cancel.  Even if the default item and the
    cancel item are the same, still let them both be set this way so whatever keyboard
	keys sthe user presses will be handled properly

	pass the number "2" to GetDialogItemType...don't check for the cancel item (cause tho
	cancel is defined as 2, we're not looking for cancel, we're looking for dialog item #2
	this is just more readable code.

	remember, this assumes that your cancel item will be item #2 (or at least the item
	that you want to use for cancelling is #2), and that there are at least 2 items in
	the dialog to begin with!
 */
	if ( GetDialogItemType( dialog, kStdCancelItemIndex ) == kButtonDialogItem )
	{
		SetDialogCancelItem( dialog, kStdCancelItemIndex );
	}

	/*	call the standard Dialog Manager filter procedure */

	if ( ( ( err = GetStdFilterProc( &stdFilter ) ) == noErr ) && ( stdFilter != nil ) )
	{
		retval = CallModalFilterProc( stdFilter, dialog, event, item );
	}

	/*	restore the port */
	SetPort( savePort );

	return retval;
}

ModalFilterUPP GetMyStandardDialogFilter( void )
{
#ifdef __cplusplus
	static ModalFilterUPP sFilterUPP = NewModalFilterProc( MyStandardDialogFilter );
#else
	static ModalFilterUPP sFilterUPP = nil;

	if ( sFilterUPP == nil )
	{
		sFilterUPP = NewModalFilterProc( MyStandardDialogFilter );
	}
#endif

	return sFilterUPP;
}

SInt16 GetDialogItemType( DialogPtr dialog, SInt16 item )
{
	SInt16		itemType;
	Handle		itemHandle;
	Rect		itemRect;

	GetDialogItem( dialog, item, &itemType, &itemHandle, &itemRect );

	return itemType;
}

Handle GetDialogItemHandle( DialogPtr dialog, SInt16 item )
{
	SInt16		itemType;
	Handle		itemHandle;
	Rect		itemRect;

	GetDialogItem( dialog, item, &itemType, &itemHandle, &itemRect );

	return itemHandle;
}

void GetDialogItemRect( DialogPtr dialog, SInt16 item, Rect *itemRect )
{
	SInt16		itemType;
	Handle		itemHandle;

	GetDialogItem( dialog, item, &itemType, &itemHandle, itemRect );
}

void SetDialogItemProc( DialogPtr dialog, SInt16 item, UserItemUPP proc )
{
	SInt16		itemType;
	Handle		itemHandle;
	Rect		itemRect;

	GetDialogItem( dialog, item, &itemType, &itemHandle, &itemRect );

	if ( ( itemType & 0x007F) == userItem )
	{
		SetDialogItem( dialog, item, itemType, (Handle) proc, &itemRect );
	}
}

void FlashButton( DialogPtr dialog, SInt16 item )
{
	ControlHandle button;
#if ( UNIVERSAL_INTERFACES_VERSION >= 0x300 )
	UInt32 finalTicks ;
#else
	SInt32 finalTicks ;
#endif

	button = (ControlHandle) GetDialogItemHandle( dialog, item );
	HiliteControl( button, kControlButtonPart );
	Delay( 8, &finalTicks );
	HiliteControl( button, kControlNoPart );
}
