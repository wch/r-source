/*
	In Pascal, an "intf" file is sorta like a .h (header) file in C:  it contains a lot
	of the declarations and definitions of things used in general by the entirity of
	the code.

	Most of the code from the WEDemoIntf.p file has been placed into the WEDemoHeader.h
	file except for some general purpose utility functions, which have then been
	placed here.
*/

#ifndef	__WEDEMOAPP__
#include "RIntf.h"
#endif

Boolean		gHasColorQD = false;
Boolean		gHasDragAndDrop = false;
Boolean		gHasTextServices = false;
Boolean		gExiting = false;


DocumentHandle GetWindowDocument( WindowPtr window )
{
	/* make sure window is not nil and is one of our windows
	*/
	if (( window == nil ) || ( GetWindowKind( window ) != userKind ))
		return nil;

	/* a handle to the document structure is kept in the window refCon
	*/
	return (DocumentHandle) GetWRefCon( window );
}

void ErrorAlert( OSErr err )
{
	Str255 errString;

	NumToString( err, errString );
	ParamText( errString, nil, nil, nil );

	SetCursor( &qd.arrow );

	Alert( kAlertGenError, GetMyStandardDialogFilter( ) );
}

void ForgetHandle( Handle *h )
{
	Handle theHandle;

	if ( ( theHandle = *h ) != nil )
	{
		*h = nil;
		DisposeHandle( theHandle );
	}
}

void ForgetResource( Handle *h )
{
	Handle theHandle;

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

/* this is a function not originally in the WASTE Demo App, however due to the
 differences between Pascal and C, it's necessary to have to accomplish things.
*/
void PStringCopy( ConstStr255Param srcString, Str255 destString )
{
	register SInt16 index = StrLength( srcString );

	do {
		*destString++ = *srcString++;
	} while ( --index >= 0 );
}
