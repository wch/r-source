/*
 *  R : A Computer Language for Statistical Data Analysis
 *  File RWindows.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus the R core team
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

/*
  This file is based on the WASTE and WASTE demo, I had do some
  modification to make it function as the way R want.In here, we will
  handle most of the things related to Window, the other routine which
  is related to Graphic Window may be appear in another file called
  "RGWindow".

  The R Program will create four different kinds of window in this
  moment, they are Console, Edit, Help and Graphic Window.

  Console Window : It is a Edit Window with a lot of
  functionality. You can only have one console window, when the
  Console window closed, the application closed. You can't remain the
  application open without the console window. This is very important,
  cause there have a huge part of program is based on this assumption.
  Edit Window    : A Sample Edit window, it based on the WASTE DEMO.
  Graphic Window : It is related to GRWindow files. It is used for drawing.
  Help Window    : Exactly like Edit Window. except I disable some of 
  the functions.

  Notes:

  In here, we need to pay attention how WASTE create a window. If you
  using another methods to create another kind of window, you better
  create another event handlers for those window. All the thing like
  windowPtr, DocumentHandle and WE instance is highly related. If you
  using some standard methods to create another kind of winodw. They
  will probably generate linking error when they handling the events
  implement in here.
*/

/*    INCLUDE HEADER FILE     */

#ifndef __ALIASES__
#include <Aliases.h>
#endif

#include <Controls.h>

#ifndef __ERRORS__
#include <Errors.h>
#endif

#ifndef __FIXMATH__
#include <FixMath.h>
#endif

#ifndef __TOOLUTILS__
#include <ToolUtils.h>
#endif

#ifndef __FILETYPESANDCREATORS__
#include <FileTypesAndCreators.h>
#endif

/*#ifndef _LongCoords_
#include "LongCoords.h"
#endif
*/

#ifndef __WEDEMOAPP__
#include "RIntf.h"
#endif

#ifndef __QUICKDRAW__
#include <Quickdraw.h>
#endif

#ifndef __SMARTSCROLLAPI__
#include "SmartScroll.h"
#endif


#include "WETabs.h"
#include "WETabHooks.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Defn.h"
#include "Graphics.h"
#include <Rdevices.h>

#include <R_ext/Boolean.h>

/*   Define Constant    */
#define eGWin       11
extern 	Str255	PostFont, UserFont;
extern	char 	*mac_getenv(const char *name);
extern SInt32		systemVersion ;

void UniqueWinTitle(void);
void RemWinMenuItem(void);


    

/* Constant, Global variables and prototype */
Ptr                  gPreAllocatePointer;
Boolean              Have_Console = false; /* why try to use sioux console */
WindowPtr            Console_Window=NULL;
WindowPtr            Working_Window=NULL;
SInt16               Edit_Window = 1;
WindowPtr            Graphic_Window[MAX_NUM_G_WIN + 1];
WindowPtr            Edit_Windows[MAX_NUM_E_WIN + 1];
SInt16               Current_Window =1;
SInt32               Num_Of_Window =1;
SInt16               Edit_Number = 0;
extern SInt16			gExpose;
extern	int			EIGHTY, F_HEIGHT;

extern Graphic_Ref   gGReference[MAX_NUM_G_WIN + 1];
extern               gtabSize;
extern SInt16        Help_Window, gTextSize;
extern WindowPtr     Help_Windows[MAX_NUM_H_WIN + 1];
// some consts used by DoGrow()
static void          hideTextRect(Rect*);
static SInt32        sScrollStep; // how many pixels to scroll (used by ScrollProc)
extern void          SetTab(void);
extern int           R_SetOptionWidth(int w);

void RnWWin(char* buf, SInt16 len, WEReference we );

enum {
    kMinWindowWidth		= 200,
    kMinWindowHeight	= 80
};

enum
{
	kScrollStepTag		= 'STEP'	//	user tag used to save scroll step information in scrollbars
} ;

/* CalcGrowIconRect
*/
static void CalcGrowIconRect( WindowPtr window, Rect *iconRect )
{
    Rect portRect;
    
    GetWindowPortBounds ( window, & portRect );

    iconRect->top = portRect.bottom - (kBarWidth - 2);
    iconRect->left = portRect.right - (kBarWidth - 2);
    iconRect->bottom = portRect.bottom;
    iconRect->right = portRect.right;
}


/* CalcTextRect
 */
static void CalcTextRect( WindowPtr window, Rect *textRect )
{
    Rect portRect;

    GetWindowPortBounds ( window, & portRect ) ;
    textRect->top = 0;
    textRect->left = 0;
    textRect->bottom = portRect.bottom - (kBarWidth - 1);
    textRect->right = portRect.right - (kBarWidth - 1);
    InsetRect( textRect, kTextMargin, kTextMargin );
}

/* CalcTextRect
 */
static void CalcBigTextRect( WindowPtr window, Rect *textRect )
{
    Rect portRect;


    GetWindowPortBounds ( window, & portRect ) ;
    textRect->top = 0;
    textRect->left = 0;
    textRect->bottom = portRect.bottom - (kBarWidth - 1);
    textRect->right = 2000;
    InsetRect( textRect, kTextMargin, kTextMargin );
}

/* CalcScrollBarRect
 */
static void CalcScrollBarRect( WindowPtr window, Orientation orientation,
			       Rect *barRect )
{
    Rect portRect;


    GetWindowPortBounds ( window, & portRect ) ;

    switch ( orientation )
    {
    case kVertical :
    {
	barRect->top = -1;
	barRect->left = portRect.right - (kBarWidth - 1);
	barRect->bottom = portRect.bottom - (kBarWidth - 2);
	barRect->right = portRect.right + 1;
	break;
    }

    case kHorizontal :
    {
	barRect->top = portRect.bottom - (kBarWidth - 1);
	barRect->left = -1;
	barRect->bottom = portRect.bottom + 1;
	barRect->right = portRect.right - (kBarWidth - 2 );
	break;
    }
    }
}


/*
   the standard Toolbox trap _DrawGrowIcon draws two lines from the
   grow icon to the left and top margins of the window's content area
   these additional lines may create ugly dirt, so we use this routine
   to temporarily set the clip region to the grow icon rect.

   in addition, if validate is true, we call _ValidRect on the icon rect 
*/
static void MyDrawGrowIcon( WindowPtr window, Boolean validate )
{
    GrafPtr		savePort;
    RgnHandle	saveClip;
    Rect		r;

    // save port and set thePort to wind

    GetPort( &savePort );
    SetPortWindowPort( window );

    // save the clip region

    saveClip = NewRgn();
    GetClip( saveClip );

    // calculate the grow icon rect

    CalcGrowIconRect( window, &r );

    // set clip region to grow icon rect

    ClipRect( &r );

    // call _DrawGrowIcon

    DrawGrowIcon( window );

    // if validate is true, remove the grow icon rect from the update region

    if ( validate )
//	ValidRect( &r );
	ValidWindowRect ( window, & r ) ;

    // restore old clip region

    SetClip( saveClip );
    DisposeRgn( saveClip );

    // restore old port

    SetPort( savePort );
}


/* ScrollBarChanged
 */
static void	ScrollBarChanged ( WindowRef window )
{
	// scroll text to reflect new scroll bar setting

	DocumentHandle	hDocument = GetWindowDocument ( window ) ;
	WEReference		we = ( * hDocument ) -> we ;
	LongRect		viewRect ;
	LongRect		destRect ;

	WEGetViewRect ( & viewRect, we ) ;
	WEGetDestRect ( & destRect, we ) ;

	WEScroll
	(
		viewRect.left - destRect.left - GetControl32BitValue( (*hDocument)->scrollBars[ kHorizontal ] ),
		viewRect.top - destRect.top - GetControl32BitValue( (*hDocument)->scrollBars[ kVertical ] ),
		we
	);
}

static void	AdjustBars ( WindowRef window )
{
	DocumentHandle	hDocument = GetWindowDocument ( window ) ;
	WEReference		we = ( * hDocument ) -> we ;
	LongRect		viewRect ;
	LongRect		destRect ;
	SInt32			visible ;
	SInt32			total ;
	SInt32			value ;
	SInt32			max ;
	ControlRef		bar ;

	// get the view and destination rectangle
	WEGetViewRect ( & viewRect, we ) ;
	WEGetDestRect ( & destRect, we ) ;

	//	do the vertical axis

	//	get scroll bar handle
	bar = ( * hDocument ) -> scrollBars [ kVertical ] ;

	//	calculate new scroll bar settings

	//	NOTE:  (destRect.bottom - destRect.top) always equals the total text height because
	//	WASTE automatically updates destRect.bottom whenever line breaks are recalculated

	total = destRect . bottom - destRect . top ;	//	total pixel height
	visible = viewRect . bottom - viewRect . top ;	//	visible pixel height
	max = total - visible ;							//	scrollable range (in pixels)
	value = viewRect . top - destRect . top ;		//	thumb location within scrollable range

	//	make sure max is always non-negative
	if ( max <= 0 ) max = 0 ;

	//	set visible size
	SetControlViewSize ( bar, visible ) ;

	//	reset the scroll bar
	SetControl32BitMaximum ( bar, max ) ;
	SetControl32BitValue ( bar, value ) ;

	//	if value exceeds max then the bottom of the destRect is above
	//	the bottom of the view rectangle:  we need to scroll the text downward
	if ( value > max )
	{
		ScrollBarChanged ( window ) ;
	}

	//	now do the horizontal axis

	//	get scroll bar handle
	bar = ( * hDocument ) -> scrollBars [ kHorizontal ] ;

	//	calculate new scroll bar settings
	total = destRect . right - destRect . left ;	//	total pixel width
	visible = viewRect . right - viewRect . left ;	//	visible pixel width
	max = total - visible ;							//	scrollable range (in pixels)
	value = viewRect . left - destRect . left ;		//	thumb location within scrollable range

	//	make sure max is always non-negative
	if ( max <= 0 ) max = 0 ;

	//	set visible size
	SetControlViewSize ( bar, visible ) ;

	//	reset the scroll bar
	SetControl32BitMaximum ( bar, max ) ;
	SetControl32BitValue ( bar, value ) ;
}

static void	ViewChanged ( WindowRef window )
{
	DocumentHandle	hDocument ;
	ControlRef		bar ;
	Rect			r ;
	LongRect		viewRect ;
	int				orientation ;

	hDocument = GetWindowDocument ( window ) ;

	//	resize the text area

	CalcTextRect ( window, & r ) ;
	WERectToLongRect ( & r, & viewRect ) ;
	WESetViewRect ( & viewRect, ( * hDocument ) -> we ) ;

	//	 move and resize the control bars

	for ( orientation = kVertical; orientation <= kHorizontal; orientation ++ )
	{
		bar = ( * hDocument ) -> scrollBars [ orientation ] ;
		HideControl ( bar ) ;
		CalcScrollBarRect ( window, orientation, & r ) ;
#if TARGET_API_MAC_CARBON
		SetControlBounds ( bar, & r ) ;
#else
		( * bar ) -> contrlRect = r ;
#endif
		ShowControl ( bar ) ;
		ValidWindowRect ( window, & r ) ;
	}

	//	reset the thumb positions and the max values of the control bars
	AdjustBars ( window ) ;
}

DocumentHandle GetWindowDocument ( WindowRef window )
{
	//	make sure window is not nil and is one of our windows
	if ( ( window == nil ) || ( GetWindowKind ( window ) != userKind ) )
	{
		return nil ;
	}

	// a handle to the document structure is kept in the window refCon
	return ( DocumentHandle ) GetWRefCon ( window ) ;
}


WEReference GetWindowWE ( WindowRef window )
{
	DocumentHandle	document ;

	if ( ( document = GetWindowDocument ( window ) ) != nil )
	{
		return ( * document ) -> we ;
	}
	else
	{
		return nil ;
	}
}


 void DoDrag ( Point thePoint, WindowRef window )
{
	Rect	desktopBounds ;
  	DragWindow ( window, thePoint, GetRegionBounds ( GetGrayRgn ( ), & desktopBounds ) ) ;
}

/* DoGrow:
   When the console window grow, you need to reset the option
   width. Unfortunately, the function CharWidth didn't doing the thing we
   expected. We need to multiple a constant 0.75 (by experiment) to get
   the best effect.
 */


 void DoGrow ( Point hitPt, WindowRef window )
{
	const Rect		sizeConstraints = { kMinWindowHeight, kMinWindowWidth, 0x7FFF, 0x7FFF } ;
	Rect			newContentRect ;
	Rect			oldTextRect ;
    SInt32 newSize ;
    Rect sizeRect ;
      
//	remember original text rectangle

   /* if(isGraphicWindow(window))
     hideTextRect(&oldTextRect);
    else
    */ CalcTextRect ( window, & oldTextRect ) ;
	
	//	resize the window
	if ( ! ResizeWindow ( window, hitPt, & sizeConstraints, & newContentRect ) )
   	 return ;

    if(isGraphicWindow(window))
     GraResize ( window ) ;
	else	//	resize the text area
 	 WindowResized ( & oldTextRect, window ) ;
}


/* DoZoom
 */
 void DoZoom ( SInt16 partCode, WindowRef window )
{
	WEReference		we = GetWindowWE ( window ) ;
	Rect			portBounds ;
	Rect			oldTextRect ;
	LongRect		destRect ;
	SInt32			idealHeight ;
	Point			idealSize ;

       
	//	determine the ideal size for this window
	//	the ideal height is the number of pixels needed to see the whole text
	WEGetDestRect ( & destRect, we ) ;
	idealHeight = ( destRect . bottom - destRect . top ) + ( 2 * kTextMargin + ( kBarWidth - 1 ) ) ;

	//	the ideal height should always be at least kMinWindowHeight
	if ( idealHeight < kMinWindowHeight )
	{
		idealHeight = kMinWindowHeight ;
	}
	else if ( idealHeight > 0x7FFF )
	{
		//	and must fit in a SInt16
		idealHeight = 0x7FFF ;
	}
	idealSize . v = idealHeight ;

	//	since the text is automatically soft-wrapped to the window width,
	//	there's no "ideal" window width
	idealSize . h = GetWindowPortBounds ( window, & portBounds ) -> right ;

	//	determine whether the window is currently in "standard" state or in "user" state
	partCode = IsWindowInStandardState ( window, & idealSize, nil ) ? inZoomIn : inZoomOut ;

	//	remember original text rectangle
/*	if(isGraphicWindow(window))
     hideTextRect(&oldTextRect);
    else
  */   CalcTextRect ( window, & oldTextRect ) ;
//CalcTextRect ( window, & oldTextRect ) ;

	//	zoom the window
	ZoomWindowIdeal ( window, partCode, & idealSize ) ;

  if(isGraphicWindow(window))
   GraResize ( window ) ;
  else	//	resize the text area
	WindowResized ( & oldTextRect, window ) ;
}



/* ScrollProc:
   this is a callback routine called by the Toolbox Control Manager move
   the scroll bar thumb and scroll the text accordingly
 */
static pascal void ScrollProc ( ControlRef bar, ControlPartCode partCode )
{
	switch ( partCode )
	{
		case kControlUpButtonPart :
		case kControlDownButtonPart :
		case kControlPageUpPart :
		case kControlPageDownPart :
		{
			SInt32		value = GetControl32BitValue ( bar ) ;
			SInt32		max = GetControl32BitMaximum ( bar ) ;
			SInt32		step = 0 ;
			UInt32		actualSize ;

			//	retrieve precalculated step value from scroll bar
			if ( GetControlProperty ( bar, R_ID, kScrollStepTag, sizeof ( step ),
				& actualSize, & step ) != noErr )
			{
				return ;
			}

			//	move the scroll bar thumb by the precalculated step
			//	and scroll the text accordingly
			if ( ( ( value < max ) && ( step > 0 ) ) ||
				 ( ( value > 0 ) && ( step < 0 ) ) )
			{
				SetControl32BitValue ( bar, value + step ) ;
				ScrollBarChanged ( GetControlOwner ( bar ) ) ;
			}
			break ;
		}

		case kControlIndicatorPart :
		{
			ScrollBarChanged ( GetControlOwner ( bar ) ) ;
			break ;
		}
	}
}


static void WindowResized ( const Rect * oldTextRect, WindowRef window )
{
	WEReference		we = GetWindowWE ( window ) ;
	RgnHandle		tempRgn ;
	RgnHandle		dirtyRgn ;
	Rect			r ;
	LongRect		lr ;
	SInt16			oldTextWidth ;
	SInt16			newTextWidth ;
	SInt32			topCharOffset ;
	LongPt			topCharPosition ;
	Boolean			rewrapText = true ;
    SInt16 			NumofChar;

	//	create temporary regions for calculations
	tempRgn = NewRgn ( ) ;
	dirtyRgn = NewRgn ( ) ;

	//	calculate original text width
	oldTextWidth = ( oldTextRect -> right - oldTextRect -> left ) ;
	RectRgn ( tempRgn, oldTextRect ) ;

	//	get new text rect
	CalcTextRect ( window, & r ) ;
	newTextWidth = ( r . right - r . left ) ;
	
	gTextSize = GetTextSize();

	if(window==Console_Window){
	TextFont(4);
	TextSize(gTextSize);
	NumofChar =    (int)(((newTextWidth - 15) / CharWidth('M')) - 0.5) ;
	R_SetOptionWidth(NumofChar);
	}
	
	RectRgn ( dirtyRgn, & r ) ;

	//	width changed?
	if ( newTextWidth == oldTextWidth )
	{
		//	nope: no need to rewrap text
		rewrapText = false ;
	}

	if ( rewrapText )
	{
		//	remember offset of first visible character
		WEGetViewRect ( & lr, we ) ;
		topCharPosition = * ( LongPt * ) & lr ;
		topCharOffset = WEGetOffset ( & topCharPosition, nil, we ) ;

		//	reset destination rectangle and recalculate line breaks
		WEGetDestRect ( & lr, we ) ;
		lr . right = lr . left + newTextWidth ;
		WESetDestRect ( & lr, we ) ;
		WECalText ( we ) ;		//	should check for errors!

		//	scroll the destination rectangle to keep the previous
		//	first visible character at the top of the view rectangle
		WEGetPoint ( topCharOffset, kHilite, & topCharPosition, nil, we ) ;
		WEGetDestRect ( & lr, we ) ;
		WEOffsetLongRect ( & lr, 0, kTextMargin - topCharPosition . v ) ;
		WESetDestRect ( & lr, we ) ;
	}

	ViewChanged ( window ) ;

	//	calculate the dirty region (to be updated)
	if ( rewrapText )
	{
		InsetRgn ( dirtyRgn, - kTextMargin, - kTextMargin ) ;
	}
	else
	{
		XorRgn ( dirtyRgn, tempRgn, dirtyRgn ) ;
		InsetRect ( & r, - kTextMargin, - kTextMargin ) ;
		RectRgn ( tempRgn, & r ) ;
		SectRgn ( dirtyRgn, tempRgn, dirtyRgn ) ;
	}

	//	mark the dirty region as invalid
	InvalWindowRgn ( window, dirtyRgn ) ;

	//	throw away temporary regions
	DisposeRgn ( tempRgn ) ;
	DisposeRgn ( dirtyRgn ) ;
}



/* DoScrollBar
 */
static void	DoScrollBar ( Point hitPt, EventModifiers modifiers, WindowRef window )
{
	DocumentHandle				hDocument;
	ControlRef					bar = nil;
	LongRect					viewRect;
	ControlPartCode				partCode;
	SInt32						pageSize;
	SInt32						step = 0;
	static ControlActionUPP		sScrollerUPP = nil;

	if ( sScrollerUPP == nil )
	{
		sScrollerUPP = NewControlActionUPP ( ScrollProc ) ;
	}

	hDocument = GetWindowDocument ( window ) ;
	WEGetViewRect ( & viewRect, ( * hDocument ) -> we ) ;

	//	find out which control was hit (if any) and in which part
	partCode = FindControl ( hitPt, window, & bar ) ;

	//	if any control was hit, it must be one of our two scroll bars:
	//	find out which and calculate the page size for it
	if ( bar == ( * hDocument ) -> scrollBars [ kVertical ] )
	{
		pageSize = viewRect.bottom - viewRect.top ;
	}
	else if ( bar == ( * hDocument ) -> scrollBars [ kHorizontal ] )
	{
		pageSize = viewRect.right - viewRect.left ;
	}
	else
	{
		return;		// return immediately if none of our scrollbars was hit
	}

	//	calculate the "scroll step" according to the part hit
	switch ( partCode )
	{
		case kControlUpButtonPart:
		{
			step = - ( ( modifiers & optionKey ) ? 1 : kScrollDelta ) ;
			break ;
		}

		case kControlDownButtonPart:
		{
			step = + ( ( modifiers & optionKey ) ? 1 : kScrollDelta ) ;
			break ;
		}

		case kControlPageUpPart:
		{
			step = - ( pageSize - kScrollDelta ) ;
			break ;
		}

		case kControlPageDownPart:
		{
			step = + ( pageSize - kScrollDelta ) ;
			break ;
		}
	}	// switch

	//	save scroll step as a control property
	SetControlProperty ( bar, R_ID, kScrollStepTag, sizeof ( step ), & step ) ;

	//	track the mouse
	TrackControl ( bar, hitPt, sScrollerUPP ) ;
}


/* TextScrolled
   This is a callback routine called whenever the text is scrolled
   automatically.  Since auto-scrolling is enabled, WEScroll may be
   invoked internally by WASTE in many different circumstances, and we
   want to be notified when this happens so we can adjust the scroll
   bars
 */
static pascal void TextScrolled ( WEReference we )
{
	WindowRef window = nil ;

	//	get window reference associated with WE instance
	if ( WEGetUserInfo ( kWindowTag, ( SInt32 * ) & window, we ) != noErr )
	{
		return ;
	}

	//	make sure the scroll bars are in synch with the destination rectangle
	AdjustBars ( window ) ;
}


/* AddClippingName:
 */
static pascal OSErr AddClippingName ( DragReference drag, WEReference we )
{
	//	add a 'clnm' flavor containing the name of the document originating the drag
	//	this flavor is used by the Finder (version 8.0 and later) to determine the
	//	name of the clipping file
	WindowRef		window = nil ;
	Str255			windowTitle ;

	//	get window reference associated with WE instance
	if ( WEGetUserInfo ( kWindowTag, ( SInt32 * ) & window, we ) != noErr )
	{
		return paramErr ;
	}

	//	get the window title
	GetWTitle ( window, windowTitle ) ;

	//	put the window title into the drag, as a 'clnm' flavor
	//	we add this flavor to the same drag item used by WASTE to add the TEXT
	//	(note that the reference number of this drag item = the WEReference )
	return AddDragItemFlavor ( drag, ( ItemReference ) we, kFlavorTypeClippingName, windowTitle,
		StrLength ( windowTitle ) + 1, flavorNotSaved ) ;
}


/* DoContent:
   This function will be called when you click inside the window
 */
Boolean	DoContent ( Point hitPt, const EventRecord * event, WindowRef window )
{
	WEReference		we = GetWindowWE ( window ) ;
	Rect			textRect ;
	GrafPtr			savePort ;
	Boolean			isMyClick = false ;

	//	set up the port
	GetPort ( & savePort ) ;
	SetPortWindowPort ( window ) ;

	//	convert the point to local coordinates
	GlobalToLocal ( & hitPt ) ;

	//	a click in an inactive window should normally activate it,
	//	but the availability of the Drag Manager introduces an exception to this rule:
	//	a click in the background selection may start a drag gesture,
	//	without activating the window

	if ( IsWindowHilited ( window ) )
	{
		isMyClick = true ;			//	active window -> always handle click
	}
	else
	{
		RgnHandle selRgn = WEGetHiliteRgn ( kCurrentSelection, kCurrentSelection, we ) ;
		isMyClick = PtInRgn ( hitPt, selRgn ) && WaitMouseMoved ( event -> where ) ;
		DisposeRgn ( selRgn ) ;
	}

	if ( isMyClick )
	{
		CalcTextRect ( window, & textRect ) ;

		if ( PtInRect ( hitPt, & textRect ) )
		{
			WEClick ( hitPt, event -> modifiers, event -> when, we ) ;
		}
		else
		{
			DoScrollBar ( hitPt, event -> modifiers, window ) ;
		}
	}

	//	restore the port
	SetPort ( savePort ) ;

	//	return true if the click should activate this window
	return ! isMyClick ;
}


/* DoScrollKey:
   Handle Scroll key
 */

static void	DoScrollKey ( SInt16 keyCode, WindowRef window )
{
	DocumentHandle		hDocument ;
	ControlRef		bar ;
	SInt32				value ;
	LongRect			viewRect ;

	hDocument = GetWindowDocument ( window ) ;
	bar = ( * hDocument ) -> scrollBars [ kVertical ] ;

	//	get current scroll bar value
	value = GetControl32BitValue ( bar ) ;

	//	get text view rect
	WEGetViewRect ( & viewRect, ( * hDocument ) -> we ) ;

	switch ( keyCode )
	{

		case keyPgUp:
		{
			value -= ( viewRect . bottom - viewRect . top ) - kScrollDelta ;
			break ;
		}

		case keyPgDn:
		{
			value += ( viewRect . bottom - viewRect . top ) - kScrollDelta ;
			break ;
		}

		case keyHome:
		{
			value = 0 ;
			break ;
		}

		case keyEnd:
		{
			value = 0x7FFFFFFF ;
			break ;
		}
	}	// switch

	//	set the new scroll bar value and scroll the text pane accordingly

	SetControl32BitValue ( bar, value ) ;
	ScrollBarChanged ( window ) ;
}



/* DoKey :
   handle key event (keyPgUp, keyPgDn, keyHome ...)
 */
void DoKey ( SInt16 key, const EventRecord * event )
{
	WindowRef		window ;
	SInt16			keyCode ;

	//	do nothing if no window is active
	if ( ( window = FrontWindow ( ) ) == nil )
		return;

	//	extract virtual key code from event record
	keyCode = ( event->message & keyCodeMask ) >> 8 ;

	// page movement keys are handled by DoScrollKey()
	switch ( keyCode )
	{
		case keyPgUp:
		case keyPgDn:
		case keyHome:
		case keyEnd:
		{
			DoScrollKey ( keyCode, window ) ;
			break ;
		}

		default:
		{
			WEKey ( key, event -> modifiers, GetWindowWE (window) ) ;
			break ;
		}
	}
}


/* DoUpdate:

   Based on WASTE DEMO. However, when you update a graphic window,
   you need to do something more than Text window.
 */
 

void DoUpdate ( WindowRef window )
{
	GrafPtr		savePort ;
	RgnHandle	updateRgn ;
	SInt16 		WinIndex;
    DevDesc *dd;
  
#if TARGET_API_MAC_CARBON
	Rect		portRect ;
#endif

	// if we have no windows, there's nothing to update!
	if ( window == nil )
	{
		return ;
	}

	// save the old drawing port
	GetPort ( & savePort ) ;
	SetPortWindowPort ( window ) ;

	// notify everything that we're doing an update.
	BeginUpdate ( window ) ;

	updateRgn = NewRgn ( ) ;

#if TARGET_API_MAC_CARBON
	//	set updateRgn to the whole window rectangle
	//	in the future, when Carbon gives us an API to get the "drawable" region
	//	of a window, use that!
	RectRgn ( updateRgn, GetWindowPortBounds ( window, & portRect ) ) ;
#else
	//	in a classic, non-Carbon environment, the visRgn of the window
	//	is set to the region to redraw between BeginUpdate and EndUpdate
	MacCopyRgn ( window -> visRgn, updateRgn ) ;
#endif

	// erase the update region
	if(!isGraphicWindow(window))
     EraseRgn ( updateRgn ) ;

	//	draw scroll bars
	if(!isGraphicWindow(window))
  	 UpdateControls ( window, updateRgn ) ;

	//	draw text
	if(!isGraphicWindow(window))
	 WEUpdate ( updateRgn, GetWindowWE ( window ) ) ;
    else{
    //    if (QDIsPortBuffered(GetWindowPort(window)))
    //    QDFlushPortBuffer(GetWindowPort(window), NULL);
 /* This way of refreshing windows is rather slow */
    WinIndex = isGraphicWindow(window);
      dd = (DevDesc*)gGReference[WinIndex].devdesc;
   
     playDisplayList(dd);
}

	// tell everything we're done updating
	EndUpdate ( window ) ;
	DisposeRgn ( updateRgn ) ;
 
	// restore the old graphics port
	SetPort ( savePort ) ;
}


/* DoActivate :
   Based on WASTE DEMO. However, when you activate a graphic window,
   you need to do something more than Text window.
 */
void DoActivate ( Boolean isActivating, WindowRef window )
{
	DocumentHandle		hDocument ;
	WEReference			we ;
	GrafPtr				savePort ;
	Rect				barRect ;
	ControlPartCode		barHilite ;
	SInt16				menuID ;
	int					orientation ;

	// if this is not one of our document windows, nothing to do here...
	if ( ( hDocument = GetWindowDocument ( window ) ) == nil )
	{
		return ;
	}
	we = ( * hDocument ) -> we ;

	//	sanity check: do nothing if required activation state
	//	is the same as the current activation state
	if ( isActivating == WEIsActive ( we ) )
	{
		return ;
	}

	//	 set up the port
	GetPort ( & savePort ) ;
	SetPortWindowPort ( window  );

	// activate or deactivate the text (and any other relevant stuff) depending on just
	// what we're doing here...
	if ( isActivating )
	{
		WEActivate ( we ) ;
		barHilite = kControlNoPart ;
	}
	else
	{
		WEDeactivate ( we ) ;
		barHilite = kControlDisabledPart ;
	}

	//	redraw the scroll bars with the new highlighting (and validate their rects)
	for ( orientation = kVertical ; orientation <= kHorizontal ; orientation ++ )
	{
		HiliteControl ( ( * hDocument ) -> scrollBars [ orientation ], barHilite ) ;
		CalcScrollBarRect ( window, orientation, & barRect ) ;
		ValidWindowRect ( window, & barRect ) ;
	}

	//	if activating, undim text-related menus
	if ( isActivating )
	{
		for ( menuID = kMenuEdit ; menuID <= kMenuWindows ; menuID ++ )
		{
			EnableMenuItem ( GetMenuHandle ( menuID ), 0 ) ;
		}
	}

	// invalidate the menu bar
	InvalMenuBar ( ) ;

	// restore the old graphics port..
	SetPort ( savePort ) ;
}

void DoIdle ( UInt32 * sleepTime, WindowRef window )
{
	WEReference		we = GetWindowWE ( window ) ;
	FSSpec			spec ;

	//	blink the caret if necessary
	WEIdle ( sleepTime, we ) ;

	//	update the window modification state according to the text modification count
	//	(if this window has not been saved before, the proxy icon should remain dimmed
	//	at all times)
	SetWindowModified ( window, ( WEGetModCount ( we ) > 0 ) ||
		( GetWindowProxyFSSpec ( window, & spec ) != noErr ) ) ;
}


/* CreateGraphicWindow
   This routine is used to create a Graphic window. You need to resize
   the window as the parameter specified.
 */
WindowPtr CreateGraphicWindow (int wid, int h)
{
    OSErr   err;
    SInt16  WinIndex;
    Rect    theWholeScreen;
    BitMap	screenBits;
    err = newWindow (nil, nil, 1, false);
    
    if (!err){
	SizeWindow ( Working_Window, wid, h, false );

    GetQDGlobalsScreenBits(&screenBits);
    
	SetRect(&theWholeScreen, screenBits.bounds.left +4,
		screenBits.bounds.top +24, screenBits.bounds.right -4,
		screenBits.bounds.bottom -4);
	MoveWindow(Working_Window, theWholeScreen.right - wid - 5, theWholeScreen.top + 20, true);
	ShowWindow(Working_Window);
    if(Current_Window>2)
      RepositionWindow(FrontWindow(), Graphic_Window[Current_Window-2],kWindowCascadeOnParentWindow);
    }
    else{
	GWdoErrorAlert(eGWin);
    }
 
     
    return Working_Window;
}

/* CreateWindow:
   Create Text Window
 */
OSErr CreateWindow (const FSSpec * pFileSpec, Boolean editable)
{
    OSErr   err;
    WindowPtr outWindow=nil;
    
 /*   *outWindow = *Working_Window;
    err = newWindow (pFileSpec, &Working_Window, 0, editable);
    if (err)
     *Working_Window = *outWindow;
*/
    outWindow = Working_Window;
    err = newWindow (pFileSpec, &Working_Window, 0, editable);
    if (err)
     Working_Window = outWindow;

    return err;

}

OSStatus newWindow ( const FSSpec * pFileSpec, WindowRef * outWindow, int graphic, Boolean editable )
{
	static WEScrollUPP			sScroller = nil ;
	static WEPreTrackDragUPP	sPreTracker = nil ;
 	Rect					initialWindowBounds = { 48, 12, 48 + 280, 12 + 440 } ;
	const SInt32				translucencyThreshold = kStandardTranslucencyThreshold ;
	Str255						initialWindowTitle ;
	Cursor						arrow ;
	DocumentHandle				hDocument = nil ;
	WindowRef					window = nil ;
	AliasHandle					alias = nil ;
	WEReference					we = nil ;
	ControlRef					bar = nil ;
	Handle						hPageMargins = nil ;
	PageMarginRec				pageMargins ;
	FInfo						finderInfo ;
	Rect						textRect ;
	Rect						fileRect ;
	const Rect *				transitionSrcRect = nil ;
	LongRect					lr ;
	int							orientation ;
	OSStatus					err ;
    Str255						titledString = "\pUntitled ";
    Str255						numberAsString;
    MenuHandle					windowsMenu;
    WEStyleMode    				mode;
    TextStyle      				ts;
    OptionBits					winOptions;
    BitMap						screenBits;
    Rect		 				theWholeScreen, portRect ;
    FMFontFamily	fontFamily = 0 ;



    GetTextSize();
    
    initialWindowBounds.right = (int)(28 + EIGHTY);
     
    if(!Have_Console)
       initialWindowBounds.bottom = (int)(48 + 18*F_HEIGHT);
      
    switch(graphic){
	
	case 0:
	 if(Edit_Window == MAX_NUM_E_WIN - 1){
     R_ShowMessage("Too may edit/help windows");
     err =-1;
 	 return;
 	 }
	break;
	
	default:
	if(Current_Window == MAX_NUM_G_WIN - 1){
    R_ShowMessage("Too many graphic windows");
    err =-1;
 	return;
    }
  	break;
	}

    
	//	allocate UPPs first time thru
	if ( sScroller == nil )
	{
		sScroller = NewWEScrollUPP ( TextScrolled ) ;
		sPreTracker = NewWEPreTrackDragUPP ( AddClippingName ) ;
	}

	//	allocate a relocateable block to hold a document record
	hDocument = ( DocumentHandle ) NewHandleClear ( sizeof ( DocumentRecord ) ) ;
	if ( ( err = MemError( ) ) != noErr )
	{
		goto cleanup ;
	}
	
	//	create the window
	switch(graphic){
	
	case 0:
	( * hDocument ) -> docType = kTypeText ;

	 err = CreateNewWindow ( kDocumentWindowClass,
		kWindowCloseBoxAttribute | kWindowVerticalZoomAttribute | kWindowCollapseBoxAttribute | kWindowResizableAttribute, 
		& initialWindowBounds, & window);
	break;
	
	default:
    window = GetNewCWindow ( kWindowTemplateID, nil, ( WindowPtr ) -1L ) ;
    if ( window == nil )
    {
	err = memFullErr ;
	goto cleanup ;
    }
  /*
	err = CreateNewWindow ( kDocumentWindowClass,
		kWindowCloseBoxAttribute | kWindowResizableAttribute, & initialWindowBounds, & window );
	*/
	break;
	}
	
	if(err!= noErr)
	 goto cleanup;
	 
	GetIndString ( initialWindowTitle, kMiscStringsID, 3 ) ;
	SetWTitle ( window, initialWindowTitle ) ;

	// link the document record to the window and the other way around
	SetWRefCon ( window, ( SInt32 ) hDocument ) ;
	( * hDocument ) -> owner = window ;

	// we got a window, so tell QuickDraw where to draw...
	SetPortWindowPort ( window ) ;

	//	calculate the text rectangle
	 if (graphic)
  	   hideTextRect(&textRect);
     else
  	  CalcTextRect ( window, & textRect ) ;
	
	WERectToLongRect ( & textRect, & lr ) ;

    if(!Have_Console)
     winOptions = weDoAutoScroll | weDoOutlineHilite | weDoUndo | weDoMultipleUndo | weDoIntCutAndPaste |
									/* weDoDragAndDrop | */weDoUseTempMem |	 weDoDrawOffscreen ;
	else
     winOptions = weDoAutoScroll | weDoOutlineHilite | weDoUndo | weDoMultipleUndo | weDoIntCutAndPaste |
									 /*weDoDragAndDrop | */weDoUseTempMem |	 weDoDrawOffscreen | weDoReadOnly ;
	if(Have_Console && editable)
	 winOptions ^= weDoReadOnly ; /* in case a window is editable, we remove the 'readonly' tag */
	 
	//	create a new WASTE instance

	if ( ( err = WENew ( & lr, & lr, winOptions, & we) ) != noErr )
	{
		goto cleanup ;
	}

	//	save a reference to the window in the WE instance
	if ( ( err = WESetUserInfo ( kWindowTag, ( SInt32 ) window, we ) ) != noErr )
	{
		goto cleanup ;
	}

	//	now the other way around:  save the WE reference in the document record
	( * hDocument ) -> we = we ;

  	//	set up our scroll callback
	if ( ( err = WESetInfo ( weScrollProc, & sScroller, we ) ) != noErr )
	{
		goto cleanup ;
	}

	//	set up our pre-TrackDrag callback
	if ( ( err = WESetInfo ( wePreTrackDragHook, & sPreTracker, we ) ) != noErr )
	{
		goto cleanup ;
	}

	//	enable translucent text dragging
	WESetInfo ( weTranslucencyThreshold, & translucencyThreshold, we ) ;
   
	//	associate a default page margins record with the WE reference
	pageMargins . top = 72 << 16 ;			//	one inch
	pageMargins . bottom = 72 << 16 ;
	pageMargins . left = 72 << 16 ;
	pageMargins . right = 72 << 16 ;

	if ( ( err = PtrToHand ( & pageMargins, & hPageMargins, sizeof ( pageMargins ) ) ) != noErr )
	{
		goto cleanup ;
	}
	if ( ( err = WESetUserInfo ( kPageMarginsTag, ( SInt32 ) hPageMargins, we ) ) != noErr )
	{
		goto cleanup ;
	}
	hPageMargins = nil ;

    if(graphic==0){

	//	create the scroll bars from a control template
	for ( orientation = kVertical ; orientation <= kHorizontal; orientation ++ )
	{
		Rect scrollBarRect ;

		CalcScrollBarRect ( window, orientation, & scrollBarRect ) ;

		if ( ( bar = NewControl ( window, & scrollBarRect, "\p", false, 0, 0, 0, kControlScrollBarLiveProc, 0 ) ) == nil )
		{
			err = memFullErr ;
			goto cleanup ;
		}

		HiliteControl ( bar, kControlDisabledPart ) ;
		ShowControl ( bar ) ;

		//	save control handle in the document record
		( * hDocument ) -> scrollBars [ orientation ] = bar ;

	}	// for
} // graphic ==0
	//	ViewChanged adjusts the scroll bars rectangles to the window frame
    if(graphic==0)
 	  ViewChanged ( window ) ;
	//	if pFileSpec is not nil, it points to a file to read, so let's read it!
	if ( pFileSpec )
	{
		// turn the cursor into a wristwatch because this can be a lengthy operation
		SetCursor ( * GetCursor ( watchCursor ) ) ;

		//	retrieve finder information
		if ( ( err = FSpGetFInfo ( pFileSpec, & finderInfo ) ) != noErr )
		{
			goto cleanup ;
		}

		//	read in the file
		switch ( finderInfo . fdType )
		{
			case kTypeText :
			{
				if ( ( err = ReadTextFile ( pFileSpec, we ) ) != noErr )
				{
					goto cleanup ;
				}
				break ;
			}

			case kTypeUnicodeText :
			{
				//	read in the file
				if ( ( err = ReadUnicodeTextFile ( pFileSpec, we ) ) != noErr )
				{
					goto cleanup ;
				}
				break ;
			}

			default :
			{
				err = badFileFormat ;
				goto cleanup ;
			}
		}

		if ( finderInfo . fdFlags & kIsStationery )
		{
			//	the file we just read was a stationery pad, so don't associate it with the window
			SetWindowProxyCreatorAndType ( window, finderInfo . fdCreator, finderInfo . fdType, pFileSpec -> vRefNum ) ;
		}
		else
		{
			//	set the window title
			SetWTitle ( window, pFileSpec -> name ) ;

			//	set the window icon
			SetWindowProxyFSSpec ( window, pFileSpec ) ;
			SetWindowModified ( window, false ) ;
		}

		( * hDocument ) -> docType = finderInfo . fdType ;

		//	get icon rect
		if ( GetFileRect ( pFileSpec, & fileRect ) == noErr )
		{
			transitionSrcRect = & fileRect ;
		}

		//	make the cursor happy
		SetCursor ( GetQDGlobalsArrow ( & arrow ) ) ;
	}
	else
	{
		SetWindowProxyCreatorAndType ( window, R_ID, kTypeText, kOnSystemDisk ) ;
	}

	//	adjust scroll bar settings based on the total text height
	if(!graphic)
	 AdjustBars ( window ) ;


	//	position the window
	RepositionWindow ( window, FrontWindow ( ), kWindowCascadeOnParentWindow ) ;

	//	finally!  show the document window
	TransitionWindow ( window, kWindowZoomTransitionEffect, kWindowShowTransitionAction, transitionSrcRect ) ;

	//	copy window ref for caller
	if ( outWindow )
	{
		* outWindow = window ;
	}


    if (!Have_Console){
	SetWTitle(window, "\pR Console");
	Console_Window = window;
	Have_Console = true;
    }
    else{
	if (graphic == 0) {
	    Edit_Windows[Edit_Window] = window;
	    Edit_Window ++;
	    Edit_Number ++;
	    NumToString( Edit_Number ,numberAsString);
	    GWdoConcatPStrings(titledString,numberAsString);
	    SetWTitle(window, titledString);

	    if(windowsMenu = GetMenuHandle(kMenuWindows))
    	 AppendMenu(windowsMenu, titledString); 
         
	    SetTab();
	    GetQDGlobalsScreenBits(&screenBits);
	    SetRect(&theWholeScreen, screenBits.bounds.left +4,
		    screenBits.bounds.top +24, screenBits.bounds.right -4,
		    screenBits.bounds.bottom -4);
        GetWindowPortBounds ( window, & portRect ) ;
        MoveWindow(window, theWholeScreen.right - (portRect.right + 5), theWholeScreen.top + 20, true);
	}
    }
    if (graphic){
	NumToString(Num_Of_Window,numberAsString);
	GWdoConcatPStrings(titledString,numberAsString);
	SetWTitle(window, titledString);

	/*    We add the corresponding menu item somewhere else     */
  
   
	Graphic_Window[Current_Window] = window;
	New_G_History(Current_Window);
	Current_Window++;
	Num_Of_Window++;

    }
    Working_Window = window;
    //	finally!  show the document window

 
      
    if(systemVersion > kMinSystemVersion)
    fontFamily = FMGetFontFamilyFromName(UserFont);
    else
     GetFNum(UserFont,&fontFamily);
    
    WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontFamily,
			& fontFamily, sizeof ( fontFamily ), GetWindowWE ( window ) ) ;



    changeSize(window, gTextSize);

cleanup :
	if ( err != noErr )
	{
		ErrorAlert ( err ) ;
	}
	ForgetHandle ( & hPageMargins ) ;
	return err ;
}


/* DestroyWindow
 */
 
void DestroyWindow ( WindowRef window )
{
	DocumentHandle	hDocument ;
	WEReference		we ;
#if TARGET_API_MAC_CARBON
	Handle			hPageFormat = nil ;
#else
	Handle			hPrintRecord = nil ;
#endif
	Handle			hPageMargins = nil ;
	FSSpec			fileSpec ;
	Rect			fileRect ;
	const Rect *	transitionDstRect = nil ;
	SInt16			menuID ;

	hDocument = GetWindowDocument ( window ) ;
	we = ( * hDocument ) -> we ;

#if TARGET_API_MAC_CARBON
	//	get rid of the page format, if any
	if ( WEGetUserInfo ( kPageFormatTag, ( SInt32 * ) & hPageFormat, we ) == noErr )
	{
		ForgetHandle ( & hPageFormat ) ;
		WERemoveUserInfo ( kPageFormatTag, we ) ;
	}
#else
	//	get rid of the print record, if any
	if ( WEGetUserInfo ( kPrintRecordTag, ( SInt32 * ) & hPrintRecord, we ) == noErr )
	{
		ForgetHandle ( & hPrintRecord ) ;
		WERemoveUserInfo ( kPrintRecordTag, we ) ;
	}
#endif

	//	get rid of the page margin record, if any
	if ( WEGetUserInfo ( kPageMarginsTag, ( SInt32 * ) & hPageMargins, we ) == noErr )
	{
		ForgetHandle ( & hPageMargins ) ;
		WERemoveUserInfo ( kPageMarginsTag, we ) ;
	}

	//	destroy the WASTE instance
	WEDispose ( we ) ;

	//	if this document has an associated file, get its icon rectangle
	if ( ( GetWindowProxyFSSpec ( window, & fileSpec ) == noErr ) && ( GetFileRect ( & fileSpec, & fileRect ) == noErr ) )
	{
		transitionDstRect = & fileRect ;
	}

	//	hide the window
	TransitionWindow ( window, kWindowZoomTransitionEffect, kWindowHideTransitionAction, transitionDstRect ) ;

	//	destroy the window record and all associated data structures
	DisposeWindow ( window ) ;

	//	finally, dispose of the document record
	DisposeHandle ( ( Handle ) hDocument ) ;

	InvalMenuBar ( ) ;
}
 

/* hideTextRect:
   Used in Graphic window, which hide the corresponding TextEdit
   Field. We hide the TextField instead of creating another kind of
   window, because the event processing procedure is based on the WASTE
   DEMO. Thus, if we have two different kind of window structure. You
   need to rewrite the event handling procedure too.
 */
static void hideTextRect( Rect *textRect )
{
    textRect->top = 0;
    textRect->left = 0;
    textRect->bottom = 0;
    textRect->right = 0;
    InsetRect( textRect, kTextMargin, kTextMargin );
}

int isEditWindow(WindowPtr window)
{
    SInt16     i;
    for(i=1; i < Edit_Window; i++){
	if (Edit_Windows[i] == window)
	    return i;
    }
    return 0;
}

void adjustEditPtr(SInt16 EditIndex)
{
    SInt16    i;
    for (i = EditIndex ; i < Edit_Window ; i++){
	Edit_Windows[i] = Edit_Windows[i+1];
    }
    Edit_Window --;
}

void adjustHelpPtr(SInt16 HelpIndex)
{
    SInt16    i;
    for (i = HelpIndex ; i < Help_Window ; i++){
	Help_Windows[i] = Help_Windows[i+1];
    }
    Help_Window --;
}

int R_ShowFiles(int nfile, char **fileName, char **title,
		char *WinTitle, Rboolean x, char *xx)
{
    Str255    	PWTitle, Cur_Title, curString;
    Str255    	name, numberAsString;
    FSSpec    	fsspec;
    OSErr     	readErr;
    SInt16    	i;
    WEReference we;
    MenuHandle 	windowsMenu = NULL;
    Boolean 	EqString = FALSE;
    
    if (nfile <=0) return 1;
    readErr = DoNew(false);

    // Handle error for opening a new window
    if (readErr != noErr)
	return 1;
	
	RemWinMenuItem();

    Help_Windows[Help_Window] = Edit_Windows[Edit_Window-1];

    if(Help_Window>1)
     RepositionWindow ( Help_Windows[Help_Window], Help_Windows[Help_Window-1], kWindowCascadeOnParentWindow ) ;

    Edit_Window --;
    Edit_Number --;
    Help_Window++;

    we = GetWindowWE ( Help_Windows[Help_Window-1]);
    for (i = 0; i < nfile; i++){
        if (title[i] && *title[i]){
	    RnWWin(title[i], strlen(title[i]), we);
	    RnWWin("\r\r", 2, we);
        }

        name[0] = strlen(fileName[i]);
        strncpy( (char *)(&name[1]),fileName[i], name[0] );
        FSMakeFSSpec(0,0,name,&fsspec);
        readErr = ReadTextFile ( &fsspec, we );


        if (readErr) {
	    DoClose(0, savingNo, Help_Windows[Help_Window-1]);
	    if(readErr == -43)
		warning("File not found");
        }
        
	UniqueWinTitle();
		

    }
    // Handle Error about reading
    return 1;
}

/*  This routine determines a unique title for any text window 
	based on the name of the file just opened
	Jago April 2001, Stefano M. Iacus
*/

void UniqueWinTitle(void)
{
	Str255 		pWTitle, ptestString, pcurString;
	char 		cWTitle[265], ctestString[265];
	MenuHandle 	windowsMenu;
	Boolean		unique = false, EqString;
	int    		w_number = 1, i;
    
	
    GetWTitle(FrontWindow(), pWTitle);
    windowsMenu = GetMenuHandle(kMenuWindows);
           	
    CopyPascalStringToC(pWTitle, cWTitle);
    CopyPascalStringToC(pWTitle, ctestString);
    CopyCStringToPascal(ctestString,ptestString);     
             
    while(!unique){
    	if(w_number>1)
          	sprintf(ctestString,"%s [%d]",cWTitle, w_number);
			
		CopyCStringToPascal(ctestString,ptestString);
         	
        for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	     	GetMenuItemText(windowsMenu, i , pcurString);
	     	EqString = EqualNumString(ptestString, pcurString, ptestString[0]);
		 		if (EqString) {
		 			w_number++; 
		 			unique = false;
		 			break;
		 			}
                else
                 unique = true;
			}
    	}
    	  
    	CopyPascalStringToC(ptestString, ctestString);
        AppendMenu(windowsMenu, ptestString); 
        SetWTitle(FrontWindow(),ptestString);
        
}

/* This routine remove the menu item corresponding to
   the title of the current FrontWindow()
   Jago April 2001, Stefano M. Iacus
*/
   
void RemWinMenuItem(void)
{
  Str255		pWTitle, pcurString;
  int			i;
  MenuHandle	windowsMenu = NULL;
  Boolean		EqString;	
  char 			cWTitle[260],ccurString[260];
  GetWTitle(FrontWindow(),pWTitle);
  CopyPascalStringToC(pWTitle,cWTitle);
  
  windowsMenu = GetMenuHandle(kMenuWindows);
  
  for(i = 1; i <= CountMenuItems(windowsMenu); i++){
  	GetMenuItemText(windowsMenu, i , pcurString);
  	CopyPascalStringToC(pcurString,ccurString);
  	if (strcmp(ccurString,cWTitle) == 0) {
  		DeleteMenuItem(windowsMenu, i);
		break;
  	}
  }

}


void RnWWin(char* buf, SInt16 len, WEReference we )
{
    SInt32 i;

    for ( i = 0; i < len; i++){
	WEKey ( buf[i], NULL, we ) ;
    }
}

int isHelpWindow(WindowPtr window)
{
    SInt16 i;
    for(i=1; i<Help_Window; i++){
	if (window == Help_Windows[i]){
	    return i;
	}
    }
    return 0;
}
