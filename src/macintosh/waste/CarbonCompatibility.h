/*
 *	WASTE Demo Project:
 *	Carbon Compatibility
 *
 *	Copyright © 1993-2000 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 *
 *	<mailto:waste@merzwaren.com>
 *	<http://www.merzwaren.com/waste/>
 */

#pragma once

#ifndef __CARBONCOMPATIBILITY__
#define __CARBONCOMPATIBILITY__

#ifndef __CONDITIONALMACROS__
#include <ConditionalMacros.h>
#endif

#ifndef __MACTYPES__
#include <MacTypes.h>
#endif

#ifndef __QUICKDRAW__
#include <Quickdraw.h>
#endif

#ifndef __MACWINDOWS__
#include <MacWindows.h>
#endif

#ifndef __DIALOGS__
#include <Dialogs.h>
#endif

#ifndef __AEDATAMODEL__
#include <AEDataModel.h>
#endif

#ifndef __NAVIGATION__
#include <Navigation.h>
#endif

/*	make sure TARGET_API_MAC_CARBON is #defined, in case we're using old Universal Headers
*/

#ifndef TARGET_API_MAC_CARBON
#define TARGET_API_MAC_CARBON	0
#endif

#if !TARGET_API_MAC_CARBON

/*	Carbon compatibility macros
*/

#ifdef __cplusplus
	inline Rect * GetRegionBounds ( RgnHandle inRegion, Rect * outBounds )
		{ * outBounds = ( * inRegion ) -> rgnBBox ; return outBounds ; }
	inline void SetPortDialogPort ( DialogRef inDialog )
		{ MacSetPort ( ( GrafPtr ) inDialog ) ; }
	inline Rect * GetWindowPortBounds ( WindowRef inWindow, Rect * outBounds )
		{ * outBounds = inWindow -> portRect ; return outBounds ; }
	inline Cursor * GetQDGlobalsArrow ( Cursor * outCursor )
		{ * outCursor = qd . arrow ; return outCursor ; }
	inline WindowRef GetControlOwner ( ControlRef inControl )
		{ return ( * inControl ) -> contrlOwner ; }
	inline DialogRef GetDialogFromWindow ( WindowRef inWindow )
		{ return inWindow ; }
#else
	#define GetRegionBounds(RGN,RECT)		(((*(RECT)) = (*(RGN))->rgnBBox),(RECT))
	#define SetPortDialogPort(PORT)			SetPort(PORT)
	#define GetWindowPortBounds(W,RECT)		(((*(RECT)) = W->portRect),(RECT))
	#define GetQDGlobalsArrow(CURS)			(((*(CURS)) = qd.arrow),(CURS))
	#define GetControlOwner(CONTROL)		((*(CONTROL))->contrlOwner)
	#define GetDialogFromWindow(WINDOW)		(WINDOW)
#endif /*__cplusplus*/

#ifndef NewNavEventUPP
	#define NewNavEventUPP(PROC)		NewNavEventProc(PROC)
#endif
#ifndef DisposeNavEventUPP
	#define DisposeNavEventUPP(UPP)		DisposeRoutineDescriptor(UPP)
#endif

/*	Carbon compatibility calls
*/
extern pascal Size AEGetDescDataSize ( const AEDesc * ) ;
extern pascal OSErr AEGetDescData ( const AEDesc *, void *, Size ) ;

#endif /*!TARGET_API_MAC_CARBON*/

#endif /*__CARBONCOMPATIBILITY__*/
