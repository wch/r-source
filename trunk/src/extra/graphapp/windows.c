/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: windows.c -- manipulating on-screen windows.
 * Platform: Windows  Version: 2.35  Date: 1998/03/03
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.50  Changes: New rectangle type implemented.
 * Version: 2.00  Changes: New object class system implemented.
 * Version: 2.15  Changes: Windows are White by default.
 * Version: 2.30  Changes: Now uses atexit for mainloop.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/*
 *  A brief explanation of how Windows handles windows, and how
 *  we make it all work here:
 *
 *  In MS-Windows, a window is a rectangular area on the screen, which
 *  has a winproc and a variety of properties. There are many kinds
 *  of windows, from free-standing application-controlled windows,
 *  to buttons, and scrollbars, to windows within windows (called
 *  MDI windows, Multiple Document Interface).
 *
 *  What we are trying to do is make free-standing windows, and
 *  MDI windows appear as one data type. There is a problem in
 *  doing this within Windows. MDI windows are manipulated in
 *  quite different ways to normal windows. To create MDI windows
 *  one must first create a "Frame window", then create a "Client
 *  window" which sits within the frame window and passes messages
 *  between the application and the MDI windows. The MDI windows
 *  are special kinds of windows which sit on the surface of the
 *  client window.
 *
 *  Our newwindow() function is hence very complex, because it must
 *  handle these three different sorts of windows: normal windows,
 *  "Frame windows" also known as Workspace windows, and "MDI child
 *  windows" also known as Document windows.
 *
 *  The newwindow() function looks at the window creation bit-field
 *  it is given, and determines what sort of window to create and
 *  whether previous windows of this type have been created, since
 *  Windows demands that before a certain kind of window is created
 *  it must first be "registered" using the RegisterClass function.
 *
 *  Another problem we have is in handling Modal windows (windows
 *  which gain exclusive access to user input when they are visible).
 *  We wish to provide the illusion that modal windows are just
 *  another kind of window which can be created, in other words,
 *  modality is a property of a window, not a property of how that
 *  window is manipulated. This is tricky to achieve under Windows.
 */

/*
 *  Windows-version library variables.
 */
	PROTECTED window current_window = NULL;
	PROTECTED int    active_windows = 0;
	PROTECTED intptr_t child_id = MinChildID;

/*
 *  Handles to special windows.
 */
	PROTECTED HWND hwndMain	= 0;	/* normal window */
	PROTECTED HWND hwndClient = 0;	/* MDI client window */
	PROTECTED HWND hwndFrame	= 0;	/* MDI frame window */
	PROTECTED window MDIFrame=0, MDIToolbar=0;
	PROTECTED HWND MDIStatus = 0;

/*
 *  System screen dimensions.
 */
	#define screen_dx	 (GetSystemMetrics(SM_CXSCREEN))
	#define screen_dy	 (GetSystemMetrics(SM_CYSCREEN))

	#define titlebar_height	 (GetSystemMetrics(SM_CYCAPTION)-1)
	#define	menubar_height   (GetSystemMetrics(SM_CYMENU)+1)
	#define	border_width     (GetSystemMetrics(SM_CXBORDER))
	#define	border_height    (GetSystemMetrics(SM_CYBORDER))
	#define	frame_width      (GetSystemMetrics(SM_CXFRAME))
	#define	frame_height     (GetSystemMetrics(SM_CYFRAME))
	#define	dlgframe_width      (GetSystemMetrics(SM_CXDLGFRAME))
	#define	dlgframe_height     (GetSystemMetrics(SM_CYDLGFRAME))
	#define scrollbar_height (GetSystemMetrics(SM_CYHSCROLL))
	#define scrollbar_width  (GetSystemMetrics(SM_CXVSCROLL))

/*
 *  Windows class names.
 */
	static	char *	win_class_name	= NULL;
	static	char *	mdi_class_name  = NULL;
	static	char *	work_class_name	= NULL;

/*
 *  Private variables.
 */
	static	int	mainloop_started = 0;

/*
 *  Find the screen co-ordinates of an object.
 */
PROTECTED
rect screen_coords(object obj)
{
    rect r;
    POINT p;

    r = getrect(obj);
    if (! obj->handle)
	return r;
    p.x = p.y = 0;
    ClientToScreen(obj->handle, &p);
    r.x = p.x;
    r.y = p.y;
    return r;
}

/*
 *  Fix an ordinary window's System Menu to reflect it's window style.
 *  This entails removing unnecessary commands such as Minimize etc.
 */
static void fix_sys_menu(HWND hwnd, unsigned long win_style)
{
    HMENU sys_menu;

    sys_menu = GetSystemMenu(hwnd, FALSE);

    if ((! (win_style & WS_MINIMIZEBOX)) &&
	(! (win_style & WS_MAXIMIZEBOX)))
    {	/* remove the separator above and below Close */
	RemoveMenu(sys_menu, 7, MF_BYPOSITION);
	RemoveMenu(sys_menu, 5, MF_BYPOSITION);
	/* also remove Restore and Task Swap commands */
	RemoveMenu(sys_menu, SC_RESTORE, MF_BYCOMMAND);
	RemoveMenu(sys_menu, SC_TASKLIST, MF_BYCOMMAND);
    }
    /* remove the un-needed commands */
    if (! (win_style & WS_MINIMIZEBOX))
	RemoveMenu(sys_menu, SC_MINIMIZE, MF_BYCOMMAND);
    if (! (win_style & WS_MAXIMIZEBOX))
	RemoveMenu(sys_menu, SC_MAXIMIZE, MF_BYCOMMAND);
    if (! (win_style & WS_THICKFRAME))
	RemoveMenu(sys_menu, SC_SIZE, MF_BYCOMMAND);
}

/*
 *  Make an MDI client window.
 */
static void make_client_window(HWND hwnd)
{
    CLIENTCREATESTRUCT clientcreate;

    clientcreate.hWindowMenu  = 0;
    clientcreate.idFirstChild = MinDocID;
    hwndClient =
	CreateWindow("MDICLIENT", NULL,
		     WS_CHILD | WS_CLIPCHILDREN | WS_VISIBLE
		     | WS_HSCROLL | WS_VSCROLL
		     | WS_CLIPSIBLINGS
		     | MDIS_ALLCHILDSTYLES,
		     CW_USEDEFAULT,
		     CW_USEDEFAULT,
		     CW_USEDEFAULT,
		     CW_USEDEFAULT,
		     hwnd, 0 , this_instance,
		     (void *) & clientcreate);
}

/*
 *  Register a class based on an app_name extension string and function.
 *  Return the new class name.
 */


static char *register_new_class(const char *extra, WNDPROC proc)
{
    WNDCLASS wndclass;
    int length;
    char *new_class_name;
    HICON AppIcon;
    length = strlen(app_name)+strlen(extra)+1;
    new_class_name = array (length, char);
    strcpy(new_class_name, app_name);
    strcat(new_class_name, extra);
    if (! prev_instance)
    {

	if ((AppIcon = LoadIcon(this_instance,app_name)) == 0)
	    AppIcon = LoadIcon(0, IDI_APPLICATION);
	wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	wndclass.lpfnWndProc   = proc;
	wndclass.cbClsExtra    = 0;
	wndclass.cbWndExtra    = 0;
	wndclass.hInstance     = this_instance;
	wndclass.hIcon         = AppIcon;
	wndclass.hCursor       = 0;
	wndclass.hbrBackground = 0;
	wndclass.lpszMenuName  = NULL;
	wndclass.lpszClassName = new_class_name;

	RegisterClass (&wndclass) ;
    }

    return new_class_name;
}

static HWND new_mdi_window(const char *name, rect r, unsigned long sty)
{
    HWND hwnd;
    MDICREATESTRUCT mdi;
    if (! hwndClient)
	return NULL;
    if (! mdi_class_name)
	mdi_class_name = register_new_class(" Document", app_doc_proc);

    mdi.szClass	= mdi_class_name;
    mdi.szTitle	= name;
    mdi.hOwner	= this_instance;
    mdi.x		= r.x;
    mdi.y		= r.y;
    mdi.cx		= r.width;
    mdi.cy		= r.height;
    mdi.style	= sty;
    mdi.lParam	= 0;

    hwnd = (HWND) sendmessage(hwndClient, WM_MDICREATE, 0,
			      (intptr_t) (LPMDICREATESTRUCT) &mdi);


    return hwnd;
}

/*
 *  The fixwinrect function returns the rectangle to be used in the
 *  call to CreateWindow. It adjusts the supplied rectangle to
 *  account for the existence of titlebars, menubars, window borders
 *  etc.
 *
 *  The rectangle passed to this function is the actual area on screen
 *  which the programmer wants available for drawing. So this function
 *  will usually make this rectangle bigger to include the various
 *  window features.
 *
 *  The rectangle can also have negative values in its coordinates,
 *  in which case the coordinates are taken from the edge of the
 *  screen. I.e. a rectangle (-4,-4,-4,-4) will create a window which
 *  is 4 pixels in from the edge of the screen all the way around.
 */
static rect fix_win_rect(rect r, long flags)
{
    rect win_rect;
    rect max_rect;

    win_rect = r;

    if ((r.width==0)||(r.height==0))
	return rect(CW_USEDEFAULT,CW_USEDEFAULT,
		    CW_USEDEFAULT,CW_USEDEFAULT);
    /* Find out the maximum rectangle we are allowed. */
    if ((flags & Document) && (hwndClient)) {
	GetClientRect(hwndClient, rect2RECT(&max_rect));
    }
    else if ((flags & ChildWindow) && (current_window)) {
	GetClientRect(current_window->handle, rect2RECT(&max_rect));
    }
    else {
	max_rect = rect(0,0,screen_dx,screen_dy);
    }
    if (flags & Resize) {
	win_rect = growr(win_rect, frame_width, frame_height);
    }
    else if (flags & Border) {
	win_rect = growr(win_rect, dlgframe_width, dlgframe_height);
    }
    else if (!(flags & ChildWindow)) {
	win_rect = growr(win_rect, border_width, border_height);
    }

    if ((flags & HScrollbar) && !(flags & CanvasSize))
	win_rect.height += scrollbar_height;
    if ((flags & VScrollbar) && !(flags & CanvasSize))
	win_rect.width += scrollbar_width;

    if (flags & Titlebar)
	win_rect.height += titlebar_height;

    if ((flags & Menubar) && !(flags & Document))
	win_rect.height += menubar_height;

    if (flags & Centered) {
	win_rect = rcenter(win_rect, max_rect);
	r = win_rect;
    }

    if (r.x <= 0)
	win_rect.x      = max_rect.x - r.x;
    if (r.y <= 0)
	win_rect.y      = max_rect.y - r.y;
    if (r.width <= 0)
	win_rect.width  = max_rect.width + r.width - win_rect.x;
    if (r.height <= 0)
	win_rect.height = max_rect.height + r.height - win_rect.y;

    return rcanon(win_rect);
}

/*
 *  Fix the window flag and style variables.
 */
static void fix_win_style(long *flags_ptr, long *state_ptr,
			unsigned long *style_ptr)
{
    long flags = *flags_ptr;
    long state = *state_ptr;
    unsigned long style = 0UL;

    state |= GA_Enabled;

    if (flags & Workspace)
	style |= (MDIS_ALLCHILDSTYLES|WS_CLIPCHILDREN|WS_OVERLAPPED);
    /*
      if (flags & Document)
      flags |= StandardWindow;
    */
    if (flags & Resize)
	style |= WS_THICKFRAME;
    else if (flags & Border)
	style |= (WS_DLGFRAME|DS_3DLOOK);
    else if (!(flags & ChildWindow))
	style |= WS_BORDER;
    if (flags & HScrollbar)
	style |= WS_HSCROLL;
    if (flags & VScrollbar)
	style |= WS_VSCROLL;

    if (flags & Titlebar) {
	style |= WS_CAPTION;
	if (flags & Maximize)
	    style |= WS_MAXIMIZEBOX;
	if (flags & Minimize)
	    style |= WS_MINIMIZEBOX;
	if (flags & Closebox)
	    style |= WS_SYSMENU;
    } else if (!(flags & ChildWindow)) {
	style |= WS_POPUP;
	flags &= ~(Maximize | Minimize | Closebox);
    }

    if (flags & ChildWindow) {
	style |= (WS_CHILD | WS_VISIBLE);
	state |= GA_Visible;
    }

    if (state & GA_Visible) {
	style |= WS_VISIBLE;
    }

    *flags_ptr = flags;
    *state_ptr = state;
    *style_ptr = style;
}

/*
 *  Private window destructor.
 */
static void private_delwindow(window obj)
{
    if (obj->call) /* Forced termination - no user interference! */
	if (obj->call->close)
	    obj->call->close = NULL;
    if (isvisible(obj))
	hide(obj);
    if (obj->flags & Document)
	sendmessage(hwndClient, WM_MDIDESTROY, obj->handle, 0L);
    else
	DestroyWindow(obj->handle);
}

/*
 *  Private object constructor.
 */
static object new_window_object(HWND hwnd, const char *name, rect r,
				long flags, long state)
{
    object obj;

    obj = new_object(WindowObject, hwnd,
		     (flags & ChildWindow) ? current_window : NULL);
    if (obj) {
	obj->rect = r;
	obj->flags = flags;
	obj->state = state;
	obj->bg = White;
	obj->die = private_delwindow;
	obj->text = new_string(name);
	obj->drawstate = copydrawstate();
	obj->drawstate->dest = obj;

	if (flags & ChildWindow)
	    obj->id = child_id++;
	else
	    current_window = obj;
    }
    return obj;
}

/*
 *  Create and return a new window.
 */
static int MDIsizeSet=0;
window newwindow(const char *name, rect r, long flags)
{
    object obj;
    HWND hwnd;
    unsigned long win_style;
    unsigned long ex_style;
    long state = flags & 0x0F;

    initapp(0,0);
    if ((flags & Document) && (! hwndClient))
	flags &= ~Document;
    if ((flags & Menubar) && (flags & Document))
	flags &= ~Menubar;
    if (flags & Workspace && r.width != 0) MDIsizeSet = 1;
    fix_win_style(&flags, &state, &win_style);
    r = fix_win_rect(r, flags);
    ex_style = 0L; /* extended style */
    if (flags & Floating)
	ex_style |= WS_EX_TOPMOST;
    if (flags & Modal)
	ex_style |= WS_EX_DLGMODALFRAME;

    if (flags & Document) {
	hwnd = new_mdi_window(name, r, win_style);
    }
    else {
	if ((flags & Workspace) && (! work_class_name)) {
	    work_class_name = register_new_class(" Workspace", app_work_proc);
	}
	else if (! win_class_name)
	    win_class_name = register_new_class("", app_win_proc);

	if(localeCP > 0 && (localeCP != GetACP())) {
	    /* This seems not actually to work */
	    wchar_t wkind[100], wc[1000];
	    mbstowcs(wkind, (flags & Workspace) ? work_class_name
		     : win_class_name, 100);
	    mbstowcs(wc, name, 1000);
	    hwnd = CreateWindowExW(
		ex_style,
		wkind,
		wc, win_style,
		r.x, r.y, r.width, r.height,
		(HWND) ((flags & ChildWindow) ?
			current_window->handle : 0),
		((HMENU) ((flags & ChildWindow) ? child_id : 0)),
		this_instance, NULL);
	} else {
	    hwnd = CreateWindowEx(
		ex_style,
		(flags & Workspace) ? work_class_name : win_class_name,
		name, win_style,
		r.x, r.y, r.width, r.height,
		(HWND) ((flags & ChildWindow) ?
			current_window->handle : 0),
		((HMENU) ((flags & ChildWindow) ? child_id : 0)),
		this_instance, NULL);
	}
    }
    if (! hwnd)
	return NULL;
    if (flags & Closebox)
	fix_sys_menu(hwnd, win_style);
    obj = new_window_object(hwnd, name, r, flags, state);

    if (flags & Workspace) {
	make_client_window(hwnd);
	if (!hwndClient) {
	    del(obj);
	    return NULL;
	}
	MDIFrame = obj;
	hwndFrame  = hwnd;
    }
    return (window) obj;
}

/*
 *  Find the next valid window, start looking from the given one.
 */
PROTECTED
object find_valid_sibling(object obj)
{
    object first = obj;

    if (! obj)
	return NULL;
    do {
	if ((obj->kind & ControlObject)
	    && (isenabled(obj)) && (isvisible(obj)))
	{
	    if (obj->kind != LabelObject)
		return obj;
	}
	obj = obj->next;
    } while (obj != first);

    return first;
}

/*
 *  Set focus to an object. It must be visible and enabled.
 */
static void select_sibling(object obj)
{
    obj = find_valid_sibling(obj);
    if (obj) {
	if (obj->flags & Document)
	    sendmessage(hwndClient, WM_MDIACTIVATE,
			obj->handle, 0L);
	else
	    SetFocus(obj->handle);
    }
}

/*
 *  Disable every other window except the supplied modal window.
 *  If the supplied window is not modal, do nothing.
 *
 *  We accomplish this by stepping through the entire list of
 *  registered windows and disable all the visible ones.
 */
static void disable_one_window(window obj)
{
    if ((obj->kind == WindowObject) && (isvisible(obj)))
	disable(obj);
}

static void disablewindows(object obj)
{
    /* If this window is not modal, do nothing. */
    if (!(obj->flags & Modal))
	return;
    /* Otherwise, move the window to the front of the list. */
    move_to_front(obj);
    /* And disable the ones behind it. */
    if (obj != obj->next)
	apply_to_list(obj->next, disable_one_window);

    /* We must have a modal window if we've gotten this far. */
    menus_active = 0; /* Can't use menus in modal windows. */
}

/*
 *  When you dismiss a modal window it should reactivate all the
 *  windows behind it, unless the first visible window behind it is
 *  a modal window too. In that case, hiding the frontmost modal
 *  window should only reactivate the second modal window.
 */
static void enable_one_window(window obj)
{
    if ((obj->kind == WindowObject) && (isvisible(obj)))
	enable(obj);
}

static void enablewindows(object obj)
{
    object next;

    /* if this window is not modal, we do nothing */
    if (!(obj->flags & Modal))
	return;
    /* Find next window in the list, if any. */
    for (next=obj->next; next!=obj; next=next->next)
	if ((next->kind == WindowObject) && (isvisible(next)))
	    break;
    if (next != obj) {
	if (next->flags & Modal) {
	    enable(next);
	    return;
	}
	else
	    apply_to_list(next, enable_one_window);
    }

    /* Enable menus again, unless we have a modal window again. */
    menus_active = 1;
}

static int is_top_level_window(window w)
{
    if (w->kind != WindowObject)	return 0;
    if (w->flags & Document)	return 0;
    if (w->flags & ChildWindow)	return 0;
    return 1;
}


static int MDIFrameFirstTime = 1;
PROTECTED
void show_window(object obj)
{
    HWND hwnd = obj->handle;
    int incremented_aw = 0;

    if (! mainloop_started) {
	mainloop_started = 1;
	atexit(app_cleanup);
	atexit(gamainloop);
    }

    if (! IsWindowVisible(hwnd))
    {
	/* Disable windows behind a modal window. */
	disablewindows(obj);
	/* Remember how many real windows active. */
	if (is_top_level_window(obj)) {
	    incremented_aw = 1;
	    active_windows ++ ;
	}
    }
    obj->state |= GA_Visible;
    if (hwndClient && (hwnd == hwndFrame) && (MDIFrameFirstTime)) {
	ShowWindow(hwnd, MDIsizeSet ? SW_SHOWNORMAL : SW_SHOWMAXIMIZED);
	MDIFrameFirstTime = 0;
    }
    else
	ShowWindow(hwnd, SW_SHOW /* SW_SHOWNORMAL */);

    /* workaround for Show bug */
    if (incremented_aw && !IsWindowVisible(hwnd) ) active_windows -- ;

    if (obj->menubar) {
	if (hwndClient) {
	    menu mdi = (obj->menubar)->menubar;
	    SendMessage(hwndClient, WM_MDISETMENU,
			(WPARAM)obj->menubar->handle,
			(LPARAM)(mdi?(mdi->handle):0));
	    DrawMenuBar(hwndFrame);
	}
	else
	    DrawMenuBar(hwnd);
    }
#if 0
    if (obj->toolbar) {
	if (MDIToolbar) hide(MDIToolbar);
	MDIToolbar = obj->toolbar;
	SendMessage(hwndFrame,WM_PAINT,(WPARAM) 0,(LPARAM) 0);
    }
#endif
    SetFocus(hwnd);
    UpdateWindow(hwnd);
    select_sibling(obj->child);
}

PROTECTED
void hide_window(object obj)
{
    HWND hwnd = obj->handle;

    if (IsWindowVisible(hwnd))
    {
	del_context(obj);

	/* Re-enable any disabled windows. */
	enablewindows(obj);
	if (obj->flags & Document)
	    sendmessage(hwndClient, WM_MDIRESTORE, hwnd, 0L);
	/* Remember how many real windows active. */
	if (is_top_level_window(obj))
	    active_windows -- ;

	ShowWindow(hwnd, SW_HIDE);
	obj->state &= ~GA_Visible;
    }

    if (current->dest == obj) {
	current->dest = NULL;
	del_context(obj);
    }
}

int ismdi()
{
    return (hwndClient!=NULL);
}

int isUnicodeWindow(object obj)
{
    return(obj->flags & UseUnicode);
}

int isiconic(window w)
{
    return IsIconic(w->handle);
}

PROTECTED
rect GetCurrentWinPos(object obj)
{
    rect r;
    WINDOWPLACEMENT W;

    if (! obj || obj->kind != WindowObject) return rect(0,0,0,0);
    W.length = sizeof(WINDOWPLACEMENT);
    GetWindowPlacement(obj->handle, &W);
    r.x = W.rcNormalPosition.left;
    r.y = W.rcNormalPosition.top;
    r.width = W.rcNormalPosition.right - r.x;
    r.height = W.rcNormalPosition.bottom - r.y;
    return r;
}
