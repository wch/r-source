/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file RIntf.c
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
 *  WASTE Demo Project:
 *  WEDemoIntf.c
 *
 */

 /*
   In Pascal, an "intf" file is sorta like a .h (header) file in C: it
   contains a lot of the declarations and definitions of things used
   in general by the entirity of the code.

   Most of the code from the WEDemoIntf.p file has been placed into
   the WEDemoHeader.h file except for some general purpose utility
   functions, which have then been placed here.
*/


#include <RCarbon.h>

#ifndef	__WEDEMOAPP__
#include "RIntf.h"
#endif


Boolean		gHasDragAndDrop = false;
Boolean		gHasTextServices = false;
Boolean		gExiting = false;



void ErrorAlert( OSErr err )
{
    Str255 errString;
    Cursor arrow;
    
    NumToString( err, errString );
    ParamText( errString, nil, nil, nil );

    //SetCursor( &qd.arrow );
    SetCursor ( GetQDGlobalsArrow ( & arrow ) ) ;

    Alert( kAlertGenError, GetMyStandardDialogFilter( ) );
}

void ForgetHandle( Handle *h )
{
    Handle theHandle=NULL;

    if ( ( theHandle = *h ) != nil )
    {
	*h = nil;
	DisposeHandle( theHandle );
    }
}

void ForgetResource( Handle *h )
{
    Handle theHandle=NULL;

    if ( ( theHandle = *h ) != nil )
    {
	*h = nil;
	ReleaseResource( theHandle );
    }
}

OSErr NewHandleTemp( Size blockSize, Handle *h )
{
    OSErr err;

    /* allocate a new relocatable block from temporary memory, or
       if that fails, from the current heap

       first try tapping temporary memory
    */
    *h = TempNewHandle( blockSize, &err );

    /* in case of failure, try with current heap
     */
    if ( *h == nil )
    {
	*h = NewHandle( blockSize );
	err = MemError( );
    }

    return err;
}

void BlockClr ( void * blockPtr, register Size blockSize )
{
    register char * p = ( char * ) blockPtr ;

    while ( --blockSize >= 0 )
    {
	* p ++ = 0 ;
    }
}

/* this is a function not originally in the WASTE Demo App, however
   due to the differences between Pascal and C, it's necessary to have
   to accomplish things.
*/
void PStringCopy( ConstStr255Param srcString, Str255 destString )
{
    register SInt16 index = StrLength( srcString );

    do {
	*destString++ = *srcString++;
    } while ( --index >= 0 );
}
