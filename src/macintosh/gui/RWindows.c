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

#include <R_ext/Boolean.h>

/*   Define Constant    */
#define eGWin       11

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


/* CalcGrowIconRect
*/
static void CalcGrowIconRect( WindowPtr window, Rect *iconRect )
{
    Rect portRect = window -> portRect ;

    iconRect->top = portRect.bottom - (kBarWidth - 2);
    iconRect->left = portRect.right - (kBarWidth - 2);
    iconRect->bottom = portRect.bottom;
    iconRect->right = portRect.right;
}


/* CalcTextRect
 */
static void CalcTextRect( WindowPtr window, Rect *textRect )
{
    Rect portRect = window -> portRect ;

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
    Rect portRect = window -> portRect ;

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
    Rect portRect = window -> portRect ;

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
    SetPort( window );

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
	ValidRect( &r );

    // restore old clip region

    SetClip( saveClip );
    DisposeRgn( saveClip );

    // restore old port

    SetPort( savePort );
}


/* ScrollBarChanged
 */
static void ScrollBarChanged( WindowPtr window )
{
    // scroll text to reflect new scroll bar setting

    DocumentHandle hDocument = GetWindowDocument( window );
    WEReference	we;
    LongRect viewRect, destRect;

    we = (*hDocument)->we;
    WEGetViewRect( &viewRect, we );
    WEGetDestRect( &destRect, we );
    WEScroll(
	viewRect.left - destRect.left - LCGetValue( (*hDocument)->scrollBars[ kHorizontal ] ),
	    viewRect.top - destRect.top - LCGetValue( (*hDocument)->scrollBars[ kVertical ] ),
	    we
	    );
}


/* AdjustBars
 */
static void AdjustBars ( WindowPtr window )
{
    DocumentHandle	hDocument ;
    WEReference		we ;
    GrafPtr		savePort ;
    LongRect		viewRect, destRect ;
    SInt32		visible, total, value, max ;
    ControlHandle	bar ;

    GetPort ( & savePort ) ;
    SetPort ( window ) ;

    hDocument = GetWindowDocument ( window ) ;
    we = ( * hDocument ) -> we ;

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

    //	notify SmartScroll
    SetSmartScrollInfo ( bar, visible, total ) ;

    //	reset the scroll bar
    LCSetMax ( bar, max ) ;
    LCSetValue ( bar, value ) ;

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

    //	notify SmartScroll
    SetSmartScrollInfo ( bar, visible, total ) ;

    //	reset the scroll bar
    LCSetMax ( bar, max ) ;
    LCSetValue ( bar, value ) ;

    SetPort ( savePort ) ;
}


/* ViewChanged
 */
static void ViewChanged( WindowPtr window )
{
    DocumentHandle	hDocument;
    GrafPtr		savePort;
    ControlHandle	bar;
    Rect		r;
    LongRect		viewRect;
    Orientation		orientation;

    GetPort( &savePort );
    SetPort( window );

    hDocument = GetWindowDocument( window );

    //	resize the text area

    CalcTextRect( window, &r );
    WERectToLongRect( &r, &viewRect );
    WESetViewRect( &viewRect, (*hDocument)->we );

    //	 move and resize the control bars

    for ( orientation = kVertical; orientation <= kHorizontal; orientation++ )
    {
	bar = (*hDocument)->scrollBars[ orientation ];
	CalcScrollBarRect( window, orientation, &r );
	MoveControl( bar, r.left, r.top );
	SizeControl( bar, r.right - r.left, r.bottom - r.top );
	ValidRect( &r );
    }

    //	reset the thumb positions and the max values of the control bars
    AdjustBars( window );

    //	redraw the control bars

    ShowControl( (*hDocument)->scrollBars[ kVertical ] );
    ShowControl( (*hDocument)->scrollBars[ kHorizontal ] );

    SetPort( savePort );
}


/*
   This is a deviation from the original Pascal WASTE Demo App code.

   This "morally correct" code for window dragging is per an article
   in MacTech Magazine (July 1994, Vol 10, No. 7). by Eric Shapiro (of
   Rock Ridge Enterprises) called "Multiple Monitors vs. Your
   Application"

   Eric addressed numerous things to allow your app to deal nicely
   with multiple monitor setups, one of them is dragging.

   According to Eric, many apps don't let you drag windows to second
   monitors, and though holding down the cmd/opt keys often overrides
   this problem, it should still be updated.  And the only reason
   qd.screenBits.bounds works to allow you to drag to second monitors
   is because of a kludge Apple put in the Window Manager

   So, this is some code from Eric to make our app be "morally correct" :)
 */
void DoDrag ( Point thePoint, WindowPtr window )
{
    Rect desktopBounds ;

    if ( gHasColorQD )
    {
	desktopBounds = ( * GetGrayRgn ( ) ) -> rgnBBox ;
    }
    else
    {
	desktopBounds = qd . screenBits . bounds ;
    }

    DragWindow ( window, thePoint, & desktopBounds ) ;
}


/* Resize :
   when you resize a graphic window, you need to redraw the content.
 */
void Resize ( Point newSize, WindowPtr window )
{
    DocumentHandle	hDocument ;
    GrafPtr			savePort ;
    Rect			r ;
    RgnHandle		tempRgn, dirtyRgn ;

    //	set up the port
    GetPort( & savePort ) ;
    SetPort ( window ) ;

    hDocument = GetWindowDocument ( window ) ;

    //	create temporary regions for calculations
    tempRgn = NewRgn ( ) ;
    dirtyRgn = NewRgn ( ) ;
    GraResize(window);
    //	save old text region
    CalcTextRect ( window, & r ) ;
    RectRgn ( tempRgn, & r ) ;

    //	erase the old grow icon rect
    CalcGrowIconRect ( window, & r ) ;
    EraseRect ( & r ) ;

    //	hide the scroll bars
    HideControl ( ( * hDocument ) -> scrollBars [ kVertical ] ) ;
    HideControl ( ( * hDocument ) -> scrollBars [ kHorizontal ] ) ;

    //	perform the actual resizing of the window, redraw scroll bars and grow icon
    SizeWindow ( window, newSize . h, newSize . v, false ) ;
    if ((window == Console_Window) || (isEditWindow(window) != 0) || (isHelpWindow(window) != 0))
	ViewChanged ( window ) ;
    MyDrawGrowIcon ( window, true ) ;

    //	calculate the dirty region (to be updated)
    CalcTextRect ( window, & r );
    RectRgn ( dirtyRgn, & r ) ;
    XorRgn ( dirtyRgn, tempRgn, dirtyRgn ) ;
    InsetRect ( & r, - kTextMargin, - kTextMargin ) ;
    RectRgn ( tempRgn, & r ) ;
    SectRgn ( dirtyRgn, tempRgn, dirtyRgn ) ;

    //	mark the dirty region as invalid
    InvalRgn ( dirtyRgn ) ;

    //	throw away temporary regions
    DisposeRgn ( tempRgn ) ;
    DisposeRgn ( dirtyRgn ) ;

    //	restore the port
    SetPort ( savePort ) ;
}


/* DoGrow:
   When the console window grow, you need to reset the option
   width. Unfortunately, the function CharWidth didn't doing the thing we
   expected. We need to multiple a constant 0.75 (by experiment) to get
   the best effect.
 */
void DoGrow ( Point hitPt, WindowPtr window )
{
    Rect sizeRect ;
    SInt32 newSize ;
    WEReference we;
    SInt16 Console_Width, NumofChar;
    GrafPtr savePort;
    WEStyleMode mode;
    TextStyle ts;

    SetRect( & sizeRect, kMinWindowWidth, kMinWindowHeight,
	     SHRT_MAX, SHRT_MAX ) ;
    if ( ( newSize = GrowWindow ( window, hitPt, & sizeRect ) ) != 0L )
    {
	//	for some reason, GrowWindow( ) returns a long value,
	//	but it's really a Point

	Resize ( * ( Point * ) & newSize, window ) ;
	if (window == Console_Window){
	    Console_Width = (Console_Window ->portRect).right -
		(Console_Window ->portRect).left ;
	    GetPort(&savePort);
	    SetPort(Console_Window);
	    TextFont(4);
	    TextSize(gTextSize);
	    NumofChar =    (int)(((Console_Width - 15) / CharWidth('M')) - 0.5) ;
	    R_SetOptionWidth(NumofChar);
	    SetPort(savePort);
	}
    }
}


/* DoZoom
 */
void DoZoom ( SInt16 partCode, WindowPtr window )
{
    DocumentHandle	hDocument;
    GrafPtr			savePort;
    Rect			r;

    GetPort( &savePort );
    SetPort( window );

    hDocument = GetWindowDocument(window);

    r = window -> portRect ;
    EraseRect( &r );
    HideControl( (*hDocument)->scrollBars[ kVertical ] );
    HideControl( (*hDocument)->scrollBars[ kHorizontal ] );

    ZoomWindow( window, partCode, false );
    if ((window == Console_Window) || (isEditWindow(window) != 0))
	ViewChanged( window );
    CalcTextRect( window, &r );
    InvalRect( &r );

    SetPort( savePort );
}


/* ScrollProc:
   this is a callback routine called by the Toolbox Control Manager move
   the scroll bar thumb and scroll the text accordingly
 */
static pascal void ScrollProc ( ControlHandle bar, ControlPartCode partCode )
{
    SInt32 value, step ;

    if ( partCode == kControlNoPart )
    {
	return ;
    }

    value = LCGetValue ( bar ) ;
    step = sScrollStep ;

    if ( ( ( value < LCGetMax ( bar ) ) && ( step > 0 ) ) ||
	 ( ( value > 0 ) && ( step < 0 ) ) )
    {
	LCSetValue ( bar, value + step ) ;
	ScrollBarChanged ( FrontWindow ( ) ) ;
    }
}


/* MySendControlMessage
 */
static SInt32 MySendControlMessage ( ControlHandle inControl,
				     SInt16 inMessage, SInt32 inParam )
{
    GrafPtr savePort ;
    Handle cdef ;
    SInt32 result ;
    SInt8 saveState ;

    //	get a handle to the control definition procedure
    cdef = ( * inControl ) -> contrlDefProc ;

    //	make sure the CDEF is loaded
    if ( * cdef == nil )
    {
	LoadResource ( cdef ) ;
	if ( * cdef == nil )
	{
	    return 0 ;		//	emergency exit (couldn't load CDEF)
	}
    }

    //	lock it down
    saveState = HGetState ( cdef ) ;
    HLock ( cdef ) ;

    //	set up the port
    GetPort ( & savePort ) ;
    SetPort ( ( * inControl ) -> contrlOwner ) ;

    //	call the CDEF
    result = CallControlDefProc ( ( ControlDefUPP ) StripAddress ( * cdef ),
				  GetControlVariant ( inControl ), inControl, inMessage, inParam ) ;

    //	unlock the CDEF
    HSetState ( cdef, saveState ) ;

    //	restore the port
    SetPort ( savePort ) ;

    //	return result code
    return result ;
}


/* LiveScroll
 */
static void LiveScroll ( ControlHandle inControl, Point inHitPt,
			 WindowPtr inWindow )
{
    IndicatorDragConstraint constraint ;
    Point mouseLoc ;
    SInt32 initialValue, oldValue, curValue, max ;
    SInt16 scrollRange, delta ;
    Orientation orientation ;

    //	hilite the control thumb
    //	this does nothing with the standard System 7.x scroll bar, but is required for
    //	correct visual feedback with the Apple Grayscale Appearance (as implemented by
    //	the Appearance control panel in MacOS 8, or by Aaron/Kaleidoscope)
    HiliteControl ( inControl, kControlIndicatorPart ) ;

    //	get limit & slop rects that should be used for dragging the indicator
    //	(see IM: Mac Toolbox Essentials, page 5-114)
    * ( Point * ) & constraint . limitRect = inHitPt ;
    MySendControlMessage ( inControl, thumbCntl, ( SInt32 ) & constraint ) ;

    //	determine the orientation of the scroll bar
    orientation = ( constraint . axis == kVerticalConstraint ) ? kHorizontal : kVertical ;

    //	calculate the area in which the thumb can travel
    if ( orientation == kVertical )
    {
	scrollRange = ( constraint . limitRect . bottom - constraint . limitRect . top ) ;
    }
    else
    {
	scrollRange = ( constraint . limitRect . right - constraint . limitRect . left ) ;
    }

    //	get current value & max
    initialValue = oldValue = curValue = LCGetValue ( inControl ) ;
    max = LCGetMax ( inControl ) ;

    //	mouse tracking loop
    while ( StillDown ( ) )
    {
	//	get current mouse location
	GetMouse ( & mouseLoc ) ;

	//	do nothing if the mouse is outside the slop rectangle
	if ( PtInRect ( mouseLoc, & constraint . slopRect ) )
	{
	    //	calculate pixel offset relative to initial hit point
	    if ( orientation == kVertical )
	    {
		delta = mouseLoc . v - inHitPt . v ;
	    }
	    else
	    {
		delta = mouseLoc . h - inHitPt . h ;
	    }

	    //	calculate new control value
	    curValue = initialValue + FixMul ( max, FixRatio ( delta, scrollRange ) ) ;
	    if ( curValue < 0 ) curValue = 0 ;
	    if ( curValue > max ) curValue = max ;
	}

	if ( curValue != oldValue )
	{
	    //	set new control value
	    LCSetValue ( inControl, curValue ) ;
	    ScrollBarChanged ( inWindow ) ;
	    oldValue = curValue ;
	}
    }

    //	unhighlight the thumb
    HiliteControl ( inControl, kControlNoPart ) ;
}


/* DoScrollBar
 */
static void DoScrollBar ( Point hitPt, EventModifiers modifiers,
			  WindowPtr window )
{
    DocumentHandle	hDocument;
    ControlHandle	bar = nil;
    LongRect		viewRect;
    ControlPartCode	partCode;
    SInt32		pageSize;
    SInt32		step = 0;

#ifdef __cplusplus
    static ControlActionUPP sScrollerUPP = NewControlActionProc( ScrollProc );
#else
    static ControlActionUPP sScrollerUPP = nil;
    if (sScrollerUPP == nil)
    {
	sScrollerUPP = NewControlActionProc( ScrollProc );
    }
#endif

    hDocument = GetWindowDocument( window );
    WEGetViewRect( &viewRect, (*hDocument)->we );

    //	find out which control was hit (if any) and in which part
    partCode = FindControl( hitPt, window, &bar );

    //	if any control was hit, it must be one of our two scroll bars:
    //	find out which and calculate the page size for it
    if ( bar == (*hDocument)->scrollBars[ kVertical ] )
    {
	pageSize = viewRect.bottom - viewRect.top;
    }
    else if ( bar == (*hDocument)->scrollBars[ kHorizontal ] )
    {
	pageSize = viewRect.right - viewRect.left;
    }
    else
    {
	return;
	// return immediately if none of our scrollbars was hit
    }

    //	dispatch on partCode
    switch ( partCode )
    {
    case kControlIndicatorPart:
    {
	// click in thumb
	if ( modifiers & optionKey )
	{
	    // call TrackControl with no actionProc and adjust text
	    TrackControl ( bar, hitPt, nil ) ;
	    LCSynch ( bar ) ;
	    ScrollBarChanged ( window ) ;
	}
	else
	{
	    LiveScroll ( bar, hitPt, window ) ;
	}
	return;
    }

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

    //	save step in a static variable for our ScrollProc callback
    sScrollStep = step ;

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
    WindowPtr window = nil ;

    //	retrieve the window pointer stored in the WE instance as a "reference constant"
    if ( WEGetInfo( weRefCon, & window, we ) != noErr )
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
    WindowPtr window = nil ;
    Str255 windowTitle ;

    //	retrieve the window pointer stored in the WE instance as a "reference constant"
    if ( WEGetInfo ( weRefCon, & window, we ) != noErr )
    {
	return paramErr ;
    }

    //	get the window title
    GetWTitle ( window, windowTitle ) ;

    //	put the window title into the drag, as a 'clnm' flavor
    //	we add this flavor to the same drag item used by WASTE to add the TEXT
    //	(note that the reference number of this drag item = the WEReference )
    return AddDragItemFlavor ( drag, ( ItemReference ) we, 'clnm', windowTitle,
			       StrLength ( windowTitle ) + 1, flavorNotSaved ) ;
}


/* DoContent:
   This function will be called when you click inside the window
 */
Boolean	DoContent ( Point hitPt, const EventRecord * event,
		    WindowPtr window )
{
    WEReference		we = GetWindowWE ( window ) ;
    Rect		textRect ;
    GrafPtr		savePort ;
    Boolean		isMyClick = false ;

    //	set up the port
    GetPort ( & savePort ) ;
    SetPort ( window ) ;

    //	convert the point to local coordinates
    GlobalToLocal ( & hitPt ) ;

    //	a click in an inactive window should normally activate it,
    //	but the availability of the Drag Manager introduces an exception to this rule:
    //	a click in the background selection may start a drag gesture,
    //	without activating the window

    if ( IsWindowHilited ( window ) )
    {
	isMyClick = true ; //	active window -> always handle click
    }
    else if ( gHasDragAndDrop )
    {
	SInt32 selStart, selEnd ;
	RgnHandle selRgn ;

	WEGetSelection ( & selStart, & selEnd, we ) ;
	selRgn = WEGetHiliteRgn ( selStart, selEnd, we ) ;
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
static void DoScrollKey ( SInt16 keyCode, WindowPtr window )
{
    DocumentHandle	hDocument ;
    ControlHandle	bar ;
    SInt32		value ;
    LongRect		viewRect ;

    hDocument = GetWindowDocument ( window ) ;
    bar = ( * hDocument ) -> scrollBars [ kVertical ] ;

    //	get current scroll bar value
    value = LCGetValue ( bar ) ;

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
	value = LONG_MAX ;
	break ;
    }
    }	// switch

    //	set the new scroll bar value and scroll the text pane accordingly

    LCSetValue ( bar, value ) ;
    ScrollBarChanged ( window ) ;
}


/* DoKey :
   handle key event (keyPgUp, keyPgDn, keyHome ...)
 */
void DoKey ( SInt16 key, const EventRecord * event )
{
    WindowPtr window ;
    SInt16 keyCode ;

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
void DoUpdate ( WindowPtr window )
{
    GrafPtr		savePort ;
    RgnHandle	updateRgn ;

    // if we have no windows, there's nothing to update!
    if ( window == nil )
    {
	return ;
    }

    // save the old drawing port
    GetPort ( & savePort ) ;
    SetPort ( window ) ;

    // notify everything that we're doing an update.
    BeginUpdate ( window ) ;

    // BeginUpdate sets the window port visRgn to the region to update
    updateRgn = window -> visRgn ;

    if ( ! EmptyRgn ( updateRgn ) )	// if it's not an empty region, let's update it!
    {
	// erase the update region
	EraseRgn ( updateRgn ) ;

	//	draw scroll bars
	UpdateControls ( window, updateRgn ) ;

	//	draw grow icon
	MyDrawGrowIcon ( window, false ) ;

	//	draw text
	WEUpdate ( updateRgn, GetWindowWE ( window ) ) ;
    }

    // tell everything we're done updating
    EndUpdate ( window ) ;
    if (isGraphicWindow(window)){
	GraUpdate(window);
    }
    // restore the old graphics port
    SetPort ( savePort ) ;
}


/* DoActivate :
   Based on WASTE DEMO. However, when you activate a graphic window,
   you need to do something more than Text window.
 */
void DoActivate ( Boolean isActivating, WindowPtr window )
{
    DocumentHandle	hDocument ;
    WEReference		we ;
    GrafPtr		savePort ;
    Rect		barRect ;
    ControlPartCode	barHilite ;
    SInt16		menuID ;
    Orientation		orientation ;

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
    SetPort ( window ) ;

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

    //	redraw the grow icon (and validate its rect)
    MyDrawGrowIcon ( window, true ) ;

    //	redraw the scroll bars with the new highlighting (and validate their rects)
    for ( orientation = kVertical ; orientation <= kHorizontal ; orientation ++ )
    {
	HiliteControl ( ( * hDocument ) -> scrollBars [ orientation ], barHilite ) ;
	CalcScrollBarRect ( window, orientation, & barRect ) ;
	ValidRect ( & barRect ) ;
    }

    //	if activating, undim text-related menus
    if ( isActivating )
    {
	for ( menuID = kMenuEdit ; menuID <= kMenuFeatures ; menuID ++ )
	{
	    EnableItem ( GetMenuHandle ( menuID ), 0 ) ;
	}
    }

    // invalidate the menu bar
    InvalMenuBar ( ) ;

    // restore the old graphics port..
    SetPort ( savePort ) ;
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
    err = newWindow (nil, 1);

    if (!err){
	SizeWindow ( Working_Window, wid, h, false );

	SetRect(&theWholeScreen, (qd.screenBits).bounds.left +4,
		qd.screenBits.bounds.top +24, qd.screenBits.bounds.right -4,
		qd.screenBits.bounds.bottom -4);
	MoveWindow(Working_Window, theWholeScreen.right - wid - 5, theWholeScreen.top + 20, true);
	ShowWindow(Working_Window);
    }
    else{
	GWdoErrorAlert(eGWin);
    }
    return Working_Window;
}

/* CreateWindow:
   Create Text Window
 */
OSErr CreateWindow (const FSSpec * pFileSpec)
{
    OSErr   err;
    err = newWindow (pFileSpec, 0);
    if (!err)
	ShowWindow(Working_Window);
    return err;

}


/* newWindow :
   General Procedure , which used to create a new window
   (all kind of windows)
 */
OSErr newWindow ( const FSSpec * pFileSpec, int graphic)
{
    DocumentHandle	hDocument = nil ;
    WindowPtr		window = nil ;
    AliasHandle		alias = nil ;
    WEReference		we = nil ;
    ControlHandle	bar = nil ;
    FInfo		fileInfo ;
    Rect		textRect, theWholeScreen ;
    LongRect		lr ;
    Orientation		orientation ;
    OSErr		err ;
    Str255		titledString = "\pUntitled ";
    Str255		numberAsString;
    MenuHandle		windowsMenu;
    WEStyleMode    	mode;
    TextStyle      	ts;
#if TARGET_API_MAC_CARBON
    FMFontFamily	fontFamily = 0 ;
#else
    SInt16		fontFamily = 0 ;
#endif


#ifdef __cplusplus
    static WEScrollUPP	sScrollerUPP = NewWEScrollProc ( TextScrolled ) ;
    static WEPreTrackDragUPP	sPreTrackerUPP = NewWEPreTrackDragProc ( AddClippingName ) ;
#else
    static WEScrollUPP	sScrollerUPP = nil ;
    static WEPreTrackDragUPP	sPreTrackerUPP = nil ;
    if ( sScrollerUPP == nil )
    {
	sScrollerUPP = NewWEScrollProc ( TextScrolled ) ;
	sPreTrackerUPP = NewWEPreTrackDragProc ( AddClippingName ) ;
    }
#endif

    //	allocate a relocateable block to hold a document record
    hDocument = ( DocumentHandle ) NewHandleClear ( sizeof ( DocumentRecord ) ) ;
    if ( ( err = MemError( ) ) != noErr )
    {
	goto cleanup ;
    }

    //	create the window from a 'WIND' template: the window is initially invisible
    //	if ColorQuickDraw is available, create a color window
    if ( gHasColorQD )
    {
	window = GetNewCWindow ( kWindowTemplateID, nil, ( WindowPtr ) -1L ) ;
    }
    else
    {
	window = GetNewWindow ( kWindowTemplateID, nil, ( WindowPtr ) -1L ) ;
    }

    //	make sure we got a window
    if ( window == nil )
    {
	err = memFullErr ;
	goto cleanup ;
    }

    // link the document record to the window and the other way around
    SetWRefCon ( window, ( SInt32 ) hDocument ) ;
    ( * hDocument ) -> owner = window ;


    // we got a window, so tell QuickDraw where to draw...
    SetPort ( window ) ;

    //	calculate the text rectangle
    if (graphic){
	hideTextRect(&textRect);
    }else
	CalcBigTextRect ( window, & textRect ) ;
    WERectToLongRect ( & textRect, & lr ) ;

    //	create a new WASTE instance
    if ( ( err = WENew ( & lr, & lr, weDoAutoScroll +
			 weDoOutlineHilite +
			 weDoUndo +
			 weDoIntCutAndPaste +
			 weDoDragAndDrop +
			 weDoUseTempMem +
			 weDoDrawOffscreen, & we) ) != noErr )
    {
	goto cleanup ;
    }

    //	save a reference to the window in the WE instance
    if ( ( err = WESetInfo ( weRefCon, & window, we ) ) != noErr )
    {
	goto cleanup ;
    }

    //	now the other way around:  save the WE reference in the document record
    ( * hDocument ) -> we = we ;

    //	set up our scroll callback
    if ( ( err = WESetInfo ( weScrollProc, & sScrollerUPP, we ) ) != noErr )
    {
	goto cleanup ;
    }

    //	set up our pre-TrackDrag callback
    if ( ( err = WESetInfo ( wePreTrackDragHook, & sPreTrackerUPP, we ) ) != noErr )
    {
	goto cleanup ;
    }

    //	create the scroll bars from a control template
    for ( orientation = kVertical ; orientation <= kHorizontal; orientation ++ )
    {
	if ( ( bar = GetNewControl ( kScrollBarTemplateID, window ) ) == nil )
	{
	    err = memFullErr ;
	    goto cleanup ;
	}
	HiliteControl ( bar, kControlDisabledPart ) ;

	//	attach a LongControl record to the scroll bar:  this allows us to use long
	//	settings and thus scroll text taller than 32,767 pixels
	if ( ( err = LCAttach ( bar ) ) != noErr )
	{
	    goto cleanup;
	}

	//	save control handle in the document record
	( * hDocument ) -> scrollBars [ orientation ] = bar ;

    }	// for

    //	ViewChanged adjusts the scroll bars rectangles to the window frame
    if (graphic == 0)
	ViewChanged ( window ) ;

    //	if pFileSpec is not nil, it points to a file to read, so let's read it!
    if ( pFileSpec != nil )
    {
	// turn the cursor into a wristwatch because this can be a lengthy operation
	SetCursor ( * GetCursor ( watchCursor ) ) ;

	//	retrieve file infomation
	if ( ( err = FSpGetFInfo ( pFileSpec, & fileInfo ) ) != noErr )
	{
	    goto cleanup ;
	}

	//	make sure we recognize the file type
	if ( ( fileInfo . fdType != kTypeText ) && ( fileInfo . fdType != ftSimpleTextDocument ) )
	{
	    err = badFileFormat ;
	    goto cleanup ;
	}

	//	read in the file
	if ( ( err = ReadTextFile ( pFileSpec, we ) ) != noErr )
	{
	    goto cleanup ;
	}

	//	set the window title to the file name
	SetWTitle ( window, pFileSpec -> name ) ;

	//	create an alias to keep track of the file
	if ( ( err = NewAlias ( nil, pFileSpec, & alias ) ) != noErr )
	{
	    goto cleanup ;
	}
	( * hDocument ) -> fileAlias = ( Handle ) alias ;

	//	if the file is a read-only file, go ahead and enable those flags
	if ( fileInfo . fdType == ftSimpleTextDocument )
	{
	    WEFeatureFlag ( weFReadOnly, weBitSet, we ) ;
	}

	//	let's make sure the cursor is happy...
	SetCursor ( & qd . arrow ) ;
    }

    //	adjust scroll bar settings based on the total text height
    if (!graphic)
	AdjustBars ( window ) ;

    if (!Have_Console){
	SetWTitle(window, "\pR Console");
	Console_Window = window;
        // If you think that the console window didn't need to have tab function.
	// You can simply delete these two lines.
	// Also, if you think that different windows ought to have different tab space.
	// what you need to do is replicate the following two lines.
	// and use it into another space.
	// I use char M to represent the char width of each character.
	Have_Console = true;
       	/*	MenuHandle windowsMenu; */
    	if(windowsMenu = GetMenu(mWindows))
	    AppendMenu(windowsMenu, "\pR Console");

    }else{
	if (graphic == 0) {
	    Edit_Windows[Edit_Window] = window;
	    Edit_Window ++;
	    Edit_Number ++;
	    NumToString( Edit_Number ,numberAsString);
	    GWdoConcatPStrings(titledString,numberAsString);
	    SetWTitle(window, titledString);

	    if(windowsMenu = GetMenu(mWindows))
		AppendMenu(windowsMenu, titledString);

	    SetTab();
	    SetRect(&theWholeScreen, (qd.screenBits).bounds.left +4,
		    qd.screenBits.bounds.top +24, qd.screenBits.bounds.right -4,
		    qd.screenBits.bounds.bottom -4);

	    MoveWindow(window, theWholeScreen.right - (window->portRect.right + 5), theWholeScreen.top + 20, true);

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

    GetFNum ( "\pmonaco", & ts . tsFont ) ;

    // use script-preserving mode by default (see WASTE docs)
    // force font change across the whole selection if the option key was held down

/* mod Jago 08/29/00
   mode =  weDoFont ;

   // set the font of the selection
   WESetStyle ( mode, & ts, GetWindowWE ( window ) ) ;
*/

    GetFNum ( "\pmonaco", & fontFamily ) ;

    WESetOneAttribute ( kCurrentSelection, kCurrentSelection, weTagFontFamily,
			& fontFamily, sizeof ( fontFamily ), GetWindowWE ( window ) ) ;



    changeSize(window, gTextSize);

 cleanup:
    if ( err != noErr )
    {
	ErrorAlert ( err ) ;
    }
    return err ;
}


/* DestroyWindow
 */
void DestroyWindow ( WindowPtr window )
{
    DocumentHandle	hDocument;
    SInt16		menuID ;

    hDocument = GetWindowDocument ( window ) ;

    //	destroy the WASTE instance
    WEDispose ( ( * hDocument ) -> we ) ;

    //	destory the LongControl records attached to the scroll bars
    LCDetach ( ( * hDocument ) -> scrollBars [ kVertical ] ) ;
    LCDetach ( ( * hDocument ) -> scrollBars [ kHorizontal ] ) ;

    //	dispose of the file alias, if any
    ForgetHandle ( & ( ( * hDocument ) -> fileAlias ) ) ;

    //	destroy the window record and all associated data structures
    DisposeWindow ( window ) ;

    //	finally, dispose of the document record
    DisposeHandle ( ( Handle ) hDocument ) ;

    // adjust the menus to suit
    for ( menuID = kMenuFont ; menuID <= kMenuFeatures ; menuID ++ )
    {
	DisableItem ( GetMenuHandle ( menuID ), 0 ) ;
    }
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

/* hideTextRect:
   Used in Graphic window, which hide the corresponding TextEdit
   Field. We hide the TextField instead of creating another kind of
   window, because the event processing procedure is based on the WASTE
   DEMO. Thus, if we have two different kind of window structure. You
   need to rewrite the event handling procedure too.
 */
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
    Str255    PWTitle, Cur_Title,curString;
    Str255    name,numberAsString;
    FSSpec    fsspec;
    OSErr     readErr;
    SInt16    i;
    WEReference we;
    MenuHandle windowsMenu;
    Boolean EqString = FALSE;
    if (nfile <=0) return 1;
    readErr = DoNew();

    // Handle error for opening a new window
    if (readErr != noErr)
	return 1;
    PWTitle[0] = strlen(WinTitle);
    strncpy( (char *)(&PWTitle[1]),WinTitle, PWTitle[0] );

    GetWTitle(Edit_Windows[Edit_Window-1], Cur_Title);
    windowsMenu = GetMenu(mWindows);
    for(i = 1; i <= CountMenuItems(windowsMenu); i++){
	GetMenuItemText(windowsMenu, i , curString);
	EqString = EqualNumString(Cur_Title, curString, curString[0]);
	if (EqString) {
	    DeleteMenuItem(windowsMenu, i);
	    break;
	}
    }

    Help_Windows[Help_Window] = Edit_Windows[Edit_Window-1];

    NumToString( Help_Window ,numberAsString);
    GWdoConcatPStrings(PWTitle,"\p ");
    GWdoConcatPStrings(PWTitle,numberAsString);
    if(windowsMenu = GetMenu(mWindows))
	AppendMenu(windowsMenu, PWTitle);

    
    SetWTitle(Help_Windows[Help_Window], PWTitle);

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

    }
    // Handle Error about reading
    return 1;
}

int R_ShowFile(char *fileName, char *title){}

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
