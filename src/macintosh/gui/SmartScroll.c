/*
 *	File:		SmartScroll.c
 *
 *	Contains:	Smart Scroll API glue code
 *
 *	Copyright:	© 1996, 1997 by Marc Moini, portions by Marc Menschenfreund,
 *				Alessandro Levi Montalcini and Mark Shirley (Thanks!),
 *				misc changes for "WASTE Demo" by Marco Piovanelli.
 *				All rights reserved.
 */

#ifndef __SMARTSCROLL__
#include "SmartScroll.h"
#endif

#ifndef __GESTALT__
#include <Gestalt.h>
#endif

#ifndef __TRAPS__
#include <Traps.h>
#endif

#ifndef __OSUTILS__
#include <OSUtils.h>
#endif

#if PRAGMA_ALIGN_SUPPORTED
#pragma options align=mac68k
#endif

#if PRAGMA_IMPORT_SUPPORTED
#pragma import on
#endif

enum
{
	gestaltSmartScroll = 'MMBS'
};

enum
{
	kSetSmartScrollInfo 		= 0L,
	kSetSmartScrollProp			= 1L,
	kGetSmartScrollProp			= 2L,
	kDisposeAllSmartScrolls		= 3L
};

typedef pascal void ( * SmartScrollProcPtr )
	( SInt32 selector, SInt32 * result, ControlHandle theScrollBar, SInt32 param1, SInt32 param2 ) ;

enum
{
	uppSmartScrollProcInfo = kPascalStackBased
		| STACK_ROUTINE_PARAMETER (1, SIZE_CODE(sizeof(SInt32)))
		| STACK_ROUTINE_PARAMETER (2, SIZE_CODE(sizeof(SInt32 *)))
		| STACK_ROUTINE_PARAMETER (3, SIZE_CODE(sizeof(ControlHandle)))
		| STACK_ROUTINE_PARAMETER (4, SIZE_CODE(sizeof(SInt32)))
		| STACK_ROUTINE_PARAMETER (5, SIZE_CODE(sizeof(SInt32)))
};

#if GENERATINGCFM
	typedef UniversalProcPtr SmartScrollUPP ;
#else
	typedef SmartScrollProcPtr SmartScrollUPP ;
#endif

typedef struct
{
	char				ssPrivate [ 16 ] ;
	SmartScrollUPP		ssDispatch ;
	FourCharCode		ssSignature ;
} SmartScrollGestaltRec ;

static SmartScrollUPP sSmartScrollDispatch = nil ;

pascal void InitSmartScrollAwareApplication ( void )
{
	const SmartScrollGestaltRec * rec ;

#if ! ( GENERATINGCFM || SystemSevenOrLater )
	if ( GetOSTrapAddress ( _Gestalt ) != GetToolTrapAddress ( _Unimplemented ) )
#endif
	{
		if ( ( Gestalt ( gestaltSmartScroll, ( SInt32 * ) & rec ) == noErr ) &&
			 ( rec != nil ) &&
			 ( rec -> ssSignature == gestaltSmartScroll ) )
		{
			sSmartScrollDispatch = rec -> ssDispatch ;
		}
	}
}

inline SInt32 __SmartScrollDispatch
	(
		SInt32 inSelector,
		ControlHandle inControl,
		SInt32 inParam1,
		SInt32 inParam2
	)
{
 	SInt32 result = 0L ;

	if ( sSmartScrollDispatch != nil )
	{
#if GENERATINGCFM
		CallUniversalProc ( sSmartScrollDispatch, uppSmartScrollProcInfo, inSelector,
						& result, inControl, inParam1, inParam2 ) ;
#else
		sSmartScrollDispatch ( inSelector, & result, inControl, inParam1, inParam2 ) ;
#endif
	}

	return result ;
}

/****************************************************************************************/
/* Call this routine to set the Visible/Total proportion for a scrollbar. */
/*  amountVisible is a 32bit value representing the size of the portion of the document that is visible now. */
/*  amountTotal is a 32bit value representing the size of the whole document. */
/*  these two parameters must share the same unit (pixels, lines, characters, frames, etc). */

pascal void SetSmartScrollInfo
	(
		ControlHandle inScrollBar,
		SInt32 inAmountVisible,
		SInt32 inAmountTotal
	)
{
	__SmartScrollDispatch ( kSetSmartScrollInfo, inScrollBar, inAmountVisible, inAmountTotal ) ;
}

/****************************************************************************************/
/* Call this routine to set the Visible/Total proportion for a scrollbar. */
/*  proportion is a Fract value (32bit) representing the Visible/Total ratio */
/*  This call has the exact same effect as SetSmartScrollInfo, you may use either one. */

pascal void SetSmartScrollProportion
	(
		ControlHandle inScrollBar,
		Fract inProportion
	)
{
	__SmartScrollDispatch ( kSetSmartScrollProp, inScrollBar, inProportion, 0L ) ;
}

/****************************************************************************************/
/* Call this routine to get the last proportion you stored for this scrollbar. */
/* the value returned will be 0 if there is an error (Smart Scroll not installed, or no value stored)  */

pascal Fract GetSmartScrollProportion ( ControlHandle inScrollBar )
{
	return __SmartScrollDispatch ( kGetSmartScrollProp, inScrollBar, 0L, 0L ) ;
}

/****************************************************************************************/
/* Call this routine before your code Quits, to help SmartScroll */
/* free the memory it reserved for the scrollbars in your Application. */

pascal void CloseSmartScrollAwareApplication ( void )
{
	__SmartScrollDispatch ( kDisposeAllSmartScrolls, nil, 0L, 0L ) ;
}
