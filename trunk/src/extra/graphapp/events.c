/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: events.c -- winprocs and timers are contained here.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: New class system implemented.
 * Version: 2.20  Changes: Non-native buttons supported.
 * Version: 2.22  Changes: 32-bit fix by Wim Rijnders.
 * Version: 2.35  Changes: New delayed deletion technique.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/* Copyright (C) 2004, 2009	The R Foundation
   Copyright (C) 2013		The R Core Team

   Changes for R, Chris Jackson, 2004
   Handle find-and-replace modeless dialogs
   Menu shortcut keys re-enabled
   Handle WM_CONTEXTMENU events for right-clicking on a (rich) edit control
   Handle mouse wheel scrolling
   Remove assumption that current->dest is non-NULL
   Add waitevent() function
 */

#include "internal.h"

/*
 *  Library variables.
 */
static timerfn do_timer = NULL;
static void *timer_data = NULL;

static MSG    msg;
PROTECTED int keystate = 0; /* state of shift, ctrl and alt keys */

/* a user-timer and a mouse-down timer function can be started */

static	UINT	timer_id	= 0;
static	UINT	mouse_timer_id	= 0;

/* buttons and xy are used to record the state of the mouse */

static	int	buttons = 0;
static	point	xy;

static	long	mouse_msec = 0;

TIMERPROC app_timer_proc;
WNDPROC   app_control_proc;

static object frontwindow = NULL; /* the window receiving events */

static UINT uFindReplaceMsg; // message identifier for FINDMSGSTRING


/* Surrogate Pairs MACRO */
#define SURROGATE_PAIRS_HI_MIN  ((uint16_t)0xd800)
#define SURROGATE_PAIRS_HI_MAX  ((uint16_t)0xdbff)
#define SURROGATE_PAIRS_LO_MIN  ((uint16_t)0xdc00)
#define SURROGATE_PAIRS_LO_MAX  ((uint16_t)0xdfff)
#define SURROGATE_PAIRS_BIT_SZ  ((uint32_t)10)
#define SURROGATE_PAIRS_MASK    (((uint16_t)1 << SURROGATE_PAIRS_BIT_SZ)-1)
#define IsSurrogatePairsHi(_h)  (SURROGATE_PAIRS_HI_MIN == \
		      ((uint16_t)(_h) &~ (uint16_t)SURROGATE_PAIRS_MASK ))
#define IsSurrogatePairsLo(_l)  (SURROGATE_PAIRS_LO_MIN == \
		      ((uint16_t)(_l) &~ (uint16_t)SURROGATE_PAIRS_MASK ))

/*
 *  Call the relevent mouse handler function.
 */
static void handle_mouse(object obj, HWND hwnd, UINT message,
			int param, int x, int y)
{
    menu m;
    POINT wp;
    HWND hw;
    int dble = 1;

    xy.x = x;
    xy.y = y;
    buttons = 0;
    if (!obj) return;
    if (param & MK_LBUTTON)
	buttons |= LeftButton;
    if (param & MK_MBUTTON)
	buttons |= MiddleButton;
    if (param & MK_RBUTTON)
	buttons |= RightButton;

    /* dispatch the mouse event to the relevent handler */
    if (obj && obj->drawstate && obj->drawstate->crsr)
	SetCursor((HCURSOR)obj->drawstate->crsr->handle);

    switch (message)
    {
    case WM_MOUSEMOVE:
	if (obj->call) {
	    if (buttons) {
		if (obj->call->mousedrag)
		    obj->call->mousedrag(obj, buttons, xy);
	    }
	    else if (obj->call->mousemove)
		obj->call->mousemove(obj, buttons, xy);
	}
	break;
    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
    case WM_MBUTTONDOWN:
	dble = 0;
	setmousetimer(mouse_msec); /* restart timer */
	/* fall through to next case */
    case WM_LBUTTONDBLCLK:
    case WM_RBUTTONDBLCLK:
    case WM_MBUTTONDBLCLK:
	if (dble) buttons |= DblClick;
	if ((obj->flags & ChildWindow) &&  (obj->kind != LabelObject))
	    SetFocus(hwnd);
	if (obj->flags & TrackMouse)
	    SetCapture(hwnd);
	if (buttons == RightButton) {
	    m = obj->popup; hw = hwnd;
	    if (!m) {
		m = obj->parent->popup;
		hw = (HWND) obj->parent->handle;
	    }
	    if (m) {
		wp.x = x; wp.y = y;
		ClientToScreen(hw, (LPPOINT) &wp);
		if (m->action) m->action(m);
		TrackPopupMenu(m->handle,
			       TPM_LEFTALIGN|
			       TPM_LEFTBUTTON|TPM_RIGHTBUTTON,
			       wp.x,wp.y,
			       0,
			       obj->handle,
			       NULL);
		break;
	    }
	}
	if (obj->call && obj->call->mousedown)
	    obj->call->mousedown(obj, buttons, xy);
	break;
    case WM_LBUTTONUP:
    case WM_RBUTTONUP:
    case WM_MBUTTONUP:
	if ((obj->flags & TrackMouse) && (buttons == 0))
	    ReleaseCapture();
	if (obj->call && obj->call->mouseup)
	    obj->call->mouseup(obj, buttons, xy);
	break;
    }
}

/*
 *  Some WM_KEYDOWN VK_* messages call the keyaction associated
 *  with a window.
 */
static void handle_virtual_keydown(object obj, int param)
{
    if ((! obj->call) || (! obj->call->keyaction))
	return;

    /* translate arrow key combinations into Unicode arrow symbols */
    if ((param >= VK_LEFT) && (param <= VK_DOWN)) {
	param += (LEFT - VK_LEFT);
    }

    /* translate functions keys into Unicode circled numbers */
    else if ((param >= VK_F1) && (param <= VK_F10)) {
	param += (F1 - VK_F1);
    }

    /* translate other keyboard keys into Unicode 'equivalents' */
    else switch (param) {
	case VK_PRIOR:	param = PGUP; break;
	case VK_NEXT:	param = PGDN; break;
	case VK_END:	param = END;  break;
	case VK_HOME:	param = HOME; break;
	case VK_INSERT:	param = INS;  break;
	case VK_DELETE:	param = DEL;  break;
	default:	return; /* do nothing */
	}

    drawto(obj);
    obj->call->keyaction(obj, param);
}

static void handle_keydown(int param)
{
    if (param == VK_SHIFT)
	keystate |= ShiftKey;
    else if (param == VK_CONTROL)
	keystate |= CtrlKey;
    else if (param == VK_MENU)
	keystate |= AltKey;
}

static void handle_keyup(int param)
{
    if (param == VK_SHIFT)
	keystate &= ~ShiftKey;
    else if (param == VK_CONTROL)
	keystate &= ~CtrlKey;
    else if (param == VK_MENU)
	keystate &= ~AltKey;
}

/*
 *  Handle char messages.
 */
static void handle_char(object obj, int ch)
{
    if (obj->call && obj->call->keydown) {
	if (ch == '\r') /* carriage return becomes newline */
	    ch = '\n';
	drawto(obj);
	obj->call->keydown(obj, ch);
    }
}

static int showMDIToolbar = 1;
void toolbar_show(void)
{
    showMDIToolbar = 1;
    SendMessage(hwndFrame,WM_PAINT, (WPARAM) 0, (LPARAM) 0);
}
void toolbar_hide(void)
{
    showMDIToolbar = 0;
    SendMessage(hwndFrame,WM_PAINT, (WPARAM) 0, (LPARAM) 0);
}

static void handle_mdiframesize(void)
{
    HWND tool=NULL ,status=NULL;
    RECT rFrame,rToolbar;
    int  fw, fh, th=0, sh=0;
    GetClientRect(hwndFrame,&rFrame);
    fw = rFrame.right-rFrame.left;
    fh = rFrame.bottom-rFrame.top;
    if (showMDIToolbar && MDIToolbar) {
	tool = (HWND)MDIToolbar->handle;
	GetWindowRect(tool,&rToolbar);
	th = rToolbar.bottom-rToolbar.top;
    }
    if (MDIStatus) {
	status = (HWND)MDIStatus;
	GetWindowRect(status,&rToolbar);
	sh = rToolbar.bottom-rToolbar.top;
    }
    MoveWindow(hwndClient,0,th+1,fw,fh-sh-th-1,TRUE);
    if (tool) {
	MoveWindow(tool,1,0,fw-2,th,TRUE);
	show(MDIToolbar);
    }
    if (status) {
	MoveWindow(status,1,fh-sh,fw-2,sh,TRUE);
    }
    SetFocus((HWND)SendMessage(hwndClient,
			       WM_MDIGETACTIVE,(WPARAM)0,(LPARAM) 0));
}

/*
 *  The window is being resized for some reason.
 */
static void handle_resize(object obj)
{
    if (obj->call && obj->call->resize) {
	drawto(obj);
	obj->call->resize(obj,
			  rect(0,0,obj->rect.width,obj->rect.height));
    }
    deletion_traversal();  /* We may be called again before
			      returning to doevent */
}

/*
 *  The window is being redrawn for some reason.
 */
static void handle_redraw(object obj, HWND hwnd)
{
    PAINTSTRUCT ps;
    if (obj==MDIFrame)
	handle_mdiframesize();
    del_context(obj);
    add_context(obj, BeginPaint(hwnd, &ps), NULL);
    if (ps.fErase)
	clear(obj);
    draw(obj);
    EndPaint(hwnd, &ps);
    remove_context(obj);
    dc = 0;
}

/*
 *  Hide an application window, or call close() function if possible.
 */
static void handle_close(object obj)
{
    if (obj->call && obj->call->close) {
	drawto(obj);
	obj->call->close(obj);
    } else {
	hide(obj);
    }
}

static void handle_destroy(object obj)
{
    /*
      drawto(obj);
      del_object(obj);
    */
}

static void handle_focus(object obj, int gained_focus)
{
    if (gained_focus) {
	obj->state |= GA_Focus;
	if (obj->caretwidth < 0) {
	    setcaret(obj, 0,0, -obj->caretwidth, obj->caretheight);
	    showcaret(obj, 1);
	}
    } else {
	obj->state &= ~GA_Focus;
	if (obj->caretwidth > 0) {
	    setcaret(obj, 0,0, -obj->caretwidth, obj->caretheight);
	    showcaret(obj, 0);
	}
    }
    if ((! USE_NATIVE_BUTTONS) && (obj->kind == ButtonObject))
	InvalidateRect(obj->handle, NULL, 0);
    if (obj->call && obj->call->focus)
	obj->call->focus(obj);
}

/*
 *  Handle scrollbars. Designed to also work with normal window
 *  scrollbars.
 */
static void handle_scroll(object obj, HWND hwnd, UINT message,
				WPARAM wParam, LPARAM lParam)
{
    int size_shown = 10;
    int max_value = 100;
    int where = 0;
    int prev = 0;
    int which = 0;
    /* we need to look at the recorded values */
    max_value = obj->max;
    size_shown = obj->size;
    if (obj->kind != WindowObject) which = SB_CTL;
    else if (message == WM_VSCROLL) which = SB_VERT;
    else if (message == WM_HSCROLL) {
        which = SB_HORZ;
	max_value = obj->xmax;
	size_shown = obj->xsize;
    }
    prev = where = GetScrollPos(hwnd, which);

    /* next we look at wParam to see what happened */
    switch(LOWORD(wParam))
    {
    case SB_PAGEDOWN:	where += (size_shown-1);
	/* fall through to next case */
    case SB_LINEDOWN:	where = min(max_value, where+1);
	break;
    case SB_PAGEUP:		where -= (size_shown-1);
	/* fall through to next case */
    case SB_LINEUP:		where = max(0, where-1);
	break;
    case SB_TOP:		where = 0;
	break;
    case SB_BOTTOM:		where = max_value;
	break;
    case SB_THUMBPOSITION:
    case SB_THUMBTRACK: 
#ifdef WIN32
	{
	    /* The message only contains a 16 bit position.  We need to query to get 32 bits. */
	    SCROLLINFO si;
	    si.cbSize = sizeof(SCROLLINFO);
	    si.fMask = SIF_TRACKPOS;
	    if (GetScrollInfo(hwnd, which, &si))
		where = si.nTrackPos;
	    else
		where = HIWORD(wParam); /* Just in case this mask is not supported. Not sure when it arrived... */
	}
#else
	where = LOWORD(lParam);
#endif
	break;
    default:		break;
    }
    /* check if something happened */
    if (prev == where)
	return;
    /* now we reset the scrollbar's values */
    SetScrollPos(hwnd, which, where, 1);
    if (message == WM_HSCROLL) {
	where = -(where+1);
    }
    setvalue(obj, where);
    activatecontrol(obj);
}

/*
 *  Perform some brush manipulation to handle background colours
 *  in native checkboxes and radio buttons.
 */
#if USE_NATIVE_TOGGLES
  #ifdef WM_CTLCOLOR
static void handle_colour(HDC dc, object obj)
{
    rgb fg, bg;
    COLORREF wincolour;

    if (obj->drawstate)
	fg = obj->drawstate->hue;
    else
	fg = obj->fg;
    wincolour = RGB((fg&Red)>>16,(fg&Green)>>8,(fg&Blue));
    SetTextColor(dc, wincolour);

    bg = obj->bg;
    wincolour = RGB((bg&Red)>>16,(bg&Green)>>8,(bg&Blue));
    SetBkColor(dc, wincolour);

    fix_brush(dc, obj, obj->bgbrush);
}
   #endif
#endif

static char dfilename[MAX_PATH + 1];
static void handle_drop(object obj, HANDLE dropstruct)
{
    if (obj->call && obj->call->drop) {
	int len = DragQueryFile(dropstruct, 0, NULL, 0);
	if (len > MAX_PATH) {
	    DragFinish(dropstruct);
	    return;
	}
	DragQueryFile(dropstruct, 0, dfilename, MAX_PATH);
	DragFinish(dropstruct);
	obj->call->drop(obj, dfilename);
    }
}

/* Handle a right-click context menu in non-window objects such as text areas */

static void handle_context_menu(object obj, HWND hwnd, int x, int y)
{
    menu m = obj->popup;
    HWND hw = hwnd;
    POINT wp;
    if (!m) {
	m = obj->parent->popup;
	hw = (HWND) obj->parent->handle;
    }
    if (m) {
	wp.x = x; wp.y = y;
	if (m->action) m->action(m);
	TrackPopupMenu(m->handle,
		       TPM_LEFTALIGN|TPM_LEFTBUTTON|TPM_RIGHTBUTTON,
		       wp.x, wp.y, 0, hw, NULL);
    }
}

/*
 *  Shared window procedure code. The pass variable is initially zero.
 *  It can be set to non-zero in this procedure if we wish to pass
 *  the event to the default Windows winprocs.
 */
static long handle_message(HWND hwnd, UINT message,
			WPARAM wParam, LONG lParam, int *pass)
{
    object obj;
    WPARAM upDown;
    static unsigned short altnpad = 0;

    /* Find the library object associated with the hwnd. */
    obj = find_by_handle(hwnd);

    if (! obj) { /* Not a library object ... */
	*pass = 1; /* ... so pass the event. */
	return 0;
    }

    frontwindow = obj; /* Needed for auto-mousedowns. */

    /* Handle mouse messages. */
    if ((message >= WM_MOUSEMOVE) && (message <= WM_MBUTTONDBLCLK))
    {
	handle_mouse(obj, hwnd, message, LOWORD(wParam),
		     LOWORD(lParam), HIWORD(lParam));
	return 0;
    }

    /* Handle other messages. */
    switch (message)
    {
    case WM_MOUSEWHEEL:  /* convert MOUSEWHEEL messages to VSCROLL. Scroll by pairs of lines   */
	upDown = (short)HIWORD(wParam) > 0 ? SB_LINEUP : SB_LINEDOWN;
	PostMessage(hwnd, WM_VSCROLL, upDown, 0);
	PostMessage(hwnd, WM_VSCROLL, upDown, 0);
	break;

    case WM_SYSKEYDOWN:
        if(obj->flags & UseUnicode)
	    if(VK_NUMPAD0 <= LOWORD(wParam) && LOWORD(wParam) <= VK_NUMPAD9) {
	        altnpad *= 10;
		altnpad += LOWORD(wParam) & 0xf;
	    }
        break;

    case WM_KEYDOWN: /* record state of shift and control keys */
	handle_keydown(LOWORD(wParam));
	handle_virtual_keydown(obj, LOWORD(wParam));

	if(obj->flags & UseUnicode) {
	    BYTE           sta[256];
	    wchar_t        wcs[3];
	    HKL            dwhkl;
	    static wchar_t deadkey = L'\0';

	    dwhkl = GetKeyboardLayout((DWORD) 0);
	    GetKeyboardState(sta);
	    if(ToUnicodeEx(wParam, lParam, sta,
			   wcs, /* 3 */ sizeof(wcs)/sizeof(wchar_t),
			   0, dwhkl) == 1) {
		if(deadkey != L'\0') {
		    wchar_t wcs_in[3];
		    wchar_t wcs_out[3];
		    wcs_in[0] = wcs[0];
		    wcs_in[1] = deadkey;
		    wcs_in[2] = L'\0';
		    /* from accent char to unicode */
		    if (FoldStringW(MAP_PRECOMPOSED, wcs_in, 3, wcs_out, 3))
			handle_char(obj, wcs_out[0]);
		    /* deadchar convert failure to skip. */
		} else
		    handle_char(obj, wcs[0]);
		deadkey = L'\0';
	    } else {
		switch(wcs[0]) {
		case 0x5e:          /* circumflex */
		    deadkey = 0x302;  break;
		case 0x60:          /* grave accent */
		    deadkey = 0x300;  break;
		case 0xa8:          /* diaeresis */
		    deadkey = 0x308;  break;
		case 0xb4:          /* acute accent */
		    deadkey = 0x301;  break;
		case 0xb8:          /* cedilla */
		    deadkey = 0x327;  break;
		default:
		    deadkey = wcs[0];
		    break;
		}
	    }
	}
	break;

    case WM_KEYUP: /* record state of shift and control keys */
        if(obj->flags & UseUnicode)
	    if(LOWORD(wParam) == VK_MENU && altnpad) {
	        handle_char(obj, altnpad);
		altnpad = 0;
	    } 
	handle_keyup(LOWORD(wParam));
	break;

    case WM_CHAR: /* SBCS Only */
	if(obj->flags & UseUnicode) return 0;
	else {
	    handle_char(obj, LOWORD(wParam)); 
	    return 0;
	}

    case WM_IME_COMPOSITION: /* DBCS Only */
	if (lParam & GCS_RESULTSTR) { /* is fixed multiGAbyte string */
	    HIMC            himc = ImmGetContext(hwnd);
	    wchar_t         buf[80];
	    wchar_t         *p;
	    int             i;
	    int             len;

	    if(obj->flags & UseUnicode) {
		/* len is GAbyte */
		len = ImmGetCompositionStringW(himc, GCS_RESULTSTR, NULL,0);
		if(NULL == (p=( len > sizeof(buf)-1) ? calloc(len,sizeof(char)) : buf)) {
		    len = sizeof(buf);
		    p = buf;
		}
		ImmGetCompositionStringW(himc,GCS_RESULTSTR, p, len);
		ImmReleaseContext(hwnd,himc);
		/* Surrogate Pairs Block */
		for(i = 0; i < (len/sizeof(wchar_t)); i++)
		    if(IsSurrogatePairsHi(p[i]) &&
		       i+1 < (len/sizeof(wchar_t)) &&
		       IsSurrogatePairsLo(p[i+1]) ) {
			handle_char(obj, L'?');
			handle_char(obj, L'?');
			i++;
		    } else handle_char(obj, p[i]);
		if(p != buf) free(p);
		return 0;
	    }
	}
	break;

    case WM_SETFOCUS:
	handle_focus(obj, 1);
	break;

    case WM_KILLFOCUS:
	handle_focus(obj, 0);
	break;

    case WM_PAINT:
	handle_redraw(obj, hwnd);
	return 0;

    case WM_INITMENUPOPUP:
	if (HIWORD(lParam)) /* true if system menu */
	    return 0; /* else fall through */
    case WM_INITMENU:
	adjust_menu(wParam);
	break;

    case WM_MOVE:
	obj->rect.x = LOWORD(lParam);
	obj->rect.y = HIWORD(lParam);
	break;

    case WM_SIZE:
	obj->rect.width = LOWORD(lParam);
	obj->rect.height = HIWORD(lParam);
	handle_resize(obj);
	break;

    case WM_ACTIVATE:
	/* Keep track of which window is in front. */
	if (LOWORD(wParam) != WA_INACTIVE)
	    move_to_front(obj);
	break;

    case WM_QUERYENDSESSION:
	handle_close(obj);
	return 1L; /* ensure Windows can terminate */

    case WM_CLOSE:
	handle_close(obj);
	return 0;

    case WM_DESTROY:
	handle_destroy(obj);
	break;

	/*case WM_SYSCOMMAND:*/
    case WM_COMMAND:
	if (LOWORD(wParam) >= MinDocID)
	    break; /* MDI Client window will handle it */
	else if (LOWORD(wParam) >= MinChildID) {
#ifdef WIN32
	    handle_control((HWND) (intptr_t) lParam, HIWORD(wParam));
#else
	    handle_control((HWND) LOWORD(lParam), HIWORD(lParam));
#endif /* WIN32 */
	}
	else if ((LOWORD(wParam) >= MinMenuID) && menus_active)
	    handle_menu_id(LOWORD(wParam));
	break;

    case WM_VSCROLL:
    case WM_HSCROLL:
#ifdef WIN32
	if (lParam != 0) { /* scrollbar object */
	    hwnd = (HWND) (intptr_t) lParam;
#else
	if (HIWORD(lParam) != 0) { /* scrollbar object */
	    hwnd = (HWND) HIWORD(lParam);
#endif /* WIN32 */
	    obj = find_by_handle(hwnd);
	    if (! obj)
		return 0;
	}
	handle_scroll(obj, hwnd, message, wParam, lParam);
	return 0;

#if USE_NATIVE_TOGGLES
#ifdef WM_CTLCOLOR
    case WM_CTLCOLOR:
#ifdef WIN32
	hwnd = (HWND) lParam;
#else
	hwnd = (HWND) LOWORD(lParam);
#endif  /* WIN32 */

	obj = find_by_handle(hwnd);
	if (! obj)
	    break;
	if ((obj->kind != CheckboxObject) && (obj->kind != RadioObject))
	    break;
	handle_colour((HDC) wParam, obj);
	return (LRESULT) obj->bgbrush;
#endif
#endif
    case WM_IME_STARTCOMPOSITION:
	if(obj->call && obj->call->im) {
	    HIMC himc ;
	    LOGFONT lf;
	    font f;
	    COMPOSITIONFORM cf;

	    himc = ImmGetContext(hwnd);
	    obj->call->im(obj, &f, (void *) &cf.ptCurrentPos);
	    GetObject(f->handle, sizeof(LOGFONT), &lf);
	    ImmSetCompositionFont(himc, &lf);
		cf.dwStyle = CFS_POINT;
		ImmSetCompositionWindow(himc, &cf);
		ImmReleaseContext(hwnd, himc);
	    }
	break;
    case WM_DROPFILES:
	handle_drop(obj, (HANDLE) wParam);
    }

    /* If we got this far the event must be passed along
     * to the default Windows event handling procedures. */
    *pass = 1;
    return 0;
}

/*
 *  Window procedures call a generic window handling routine.
 *  We need three window procedures since the different calls
 *  tell us which default window procedure to pass the messages
 *  to if we don't wish to handle a message.
 *  If we were to use only one window procedure, we would have to
 *  have a way of determining which is the default window proc
 *  for a window from just knowing the hwnd (which may or may not
 *  belong to us).
 */
LRESULT WINAPI
app_win_proc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    long result;
    int pass = 0;

    result = handle_message(hwnd, message, wParam, lParam, &pass);
    if (pass)
	result = DefWindowProc(hwnd, message, wParam, lParam);
    return result;
}

LRESULT WINAPI
app_doc_proc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    long result;
    int pass = 0;
    object obj;
    if ((message==WM_MDIACTIVATE) && ((HWND)lParam==hwnd)) {
	if (MDIToolbar) hide(MDIToolbar);
	obj = find_by_handle(hwnd);
	MDIToolbar = (obj) ? obj->toolbar : NULL;
	handle_mdiframesize();
	if (obj && obj->menubar) {
	    menu mdi = (obj->menubar)->menubar;
	    SendMessage(hwndClient, WM_MDISETMENU,
			(WPARAM)obj->menubar->handle,
			(LPARAM)(mdi?(mdi->handle):0));
	    DrawMenuBar(hwndFrame);
	}
	if (obj) updatestatus(obj->status);
	RedrawWindow(hwndFrame,NULL,NULL,
		     RDW_UPDATENOW|RDW_ALLCHILDREN);
	SetFocus(hwnd);
	return 1;
    }
    result = handle_message(hwnd, message, wParam, lParam, &pass);
    if (pass)
	result = DefMDIChildProc(hwnd, message, wParam, lParam);
    return result;
}

LRESULT WINAPI
app_work_proc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    long result;
    int pass = 0;
    result = handle_message(hwnd, message, wParam, lParam, &pass);
    if (pass)
	result = DefFrameProc(hwnd, hwndClient, message, wParam, lParam);
    return result;
}

/*
 *  To handle controls correctly, we replace each control's event
 *  handling procedure with our own when we create it. We handle
 *  certain events ourselves, and pass the rest to the default
 *  routines.
 *  Things we do here include: allowing the TAB key to change
 *  input focus to the next control; for one-line text fields
 *  pressing return causes the event to be sent to the parent window.
 */

/* Send a char to an object, or its parent if it has no handler. */
static void send_char(object obj, int ch)
{
    while (obj) {
	if ((obj->call) && (obj->call->keydown))
	    break;
	obj = obj->parent;
    }
    if (! obj)
	return;
    if (ch == '\r')
	ch = '\n';
    drawto(obj);
    obj->call->keydown(obj, ch);
    keystate = 0;
}

long WINAPI
app_control_procedure (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    int prevent_activation = 0;
    int key;
    long result;
    object obj, next;

    /* Find the library object associated with the hwnd. */
    obj = find_by_handle(hwnd);
    key = LOWORD(wParam);

    if (! obj) /* Not a library object ... */
	return 0; /* ... so do nothing. */
    if (! obj->winproc)
	return 0; /* Nowhere to send events! */

    next = find_valid_sibling(obj->next);

    if (message == WM_KEYDOWN)
	handle_keydown(key);
    else if (message == WM_KEYUP)
	handle_keyup(key);

    switch (message)
    {
    case WM_KEYDOWN:
	if (obj->kind == TextboxObject) {
	    handle_virtual_keydown(obj, key); /* call user's virtual key handler */
	    if ((key == VK_TAB) && (keystate & CtrlKey)) {
		SetFocus(next->handle);
		return 0;
	    }
	    break;
	}
	if (key == VK_TAB) {
	    SetFocus(next->handle);
	    return 0;
	}
	else if ((key == VK_RETURN) || (key == VK_ESCAPE)) {
	    send_char(obj, key);
	    return 0;
	}
	break;

    case WM_CHAR:
	switch (obj->kind) {
	case TextboxObject:
	    send_char(obj, key); /* call user's key handler */
	    break;
	case LabelObject:
	case ButtonObject:   case CheckboxObject:
	case RadioObject:    case ScrollbarObject:
	case ListboxObject:  case MultilistObject:
	case DroplistObject: case DropfieldObject:
	    if (key != ' ') {
		send_char(obj, key);
		return 0;
	    }
	case FieldObject:
	    if (key == '\t')
		return 0;
	    if ((key == '\n') || (key == ESC))
		return 0;
	    break;
	}
	break;

    case WM_SETFOCUS:
	if (obj->kind == RadioObject) {
	    /* Temporarily disable the control manually.
	     * We do this to work around the way Windows
	     * sends WM_COMMAND messages to radio buttons
	     * when we use TAB to set focus to them. */
#if USE_NATIVE_TOGGLES
	    if (isenabled(obj)) {
		obj->state &= ~GA_Enabled;
		prevent_activation = 1;
	    }
#endif
	}
	else if (obj->kind == FieldObject) {
#ifdef WIN32
	    sendmessage(hwnd, EM_SETSEL, 32767, 32767);
#else
	    sendmessage(hwnd, EM_SETSEL, 0, MAKELONG(32767,32767));
#endif /* WIN32 */
	}
	break;


    case WM_CONTEXTMENU:
	handle_context_menu(obj, hwnd, LOWORD(lParam), HIWORD(lParam)); /* Handles right-click menus in, for example, edit controls */
	break;
    }

    if (message == uFindReplaceMsg) {
	handle_findreplace(hwnd, (LPFINDREPLACE) lParam);
	return 0;
    }

    result = CallWindowProc((obj->winproc), hwnd, message, wParam, lParam);

    /* Re-activate the control if necessary. */
    if (prevent_activation)
	obj->state |= GA_Enabled;
    return result;
}

/*
 *  Timer functions use a timer procedure not associated with a window.
 *  We use this one procedure to handle both timer events and mouse-down
 *  timer events. The mouse-down timer happens when the user has held
 *  a mouse button down for longer than mouse_msec milliseconds, and
 *  it causes the last mouse event to repeat.
 */
UINT WINAPI
app_timer_procedure(HWND hwnd, UINT message, UINT tid, DWORD time)
{
    object obj;
    UINT id = LOWORD(tid);

    if ((id == 0) || (message != WM_TIMER))
	return 0;

    if (id == mouse_timer_id) {
	obj = frontwindow;
	if ((buttons == 0) || (! obj) || (! obj->call)
	    || (! obj->call->mouserepeat))
	    setmousetimer(0);
	else
	    obj->call->mouserepeat(obj, buttons, xy);
    }
    else if (id == timer_id) {
	if (do_timer)
	    do_timer(timer_data);
    }

    return 0;
}

/*
 *  Set the timer function.
 */
void settimerfn(timerfn timeout, void *data)
{
    do_timer = timeout;
    timer_data = data;
}

/*
 * Start the timer with a period of msec milliseconds.
 */
int settimer(unsigned msec)
{
    if (timer_id != 0) {
	KillTimer(0, timer_id);
	timer_id = 0;
    }
    if (msec == 0)
	timer_id = 0;
    else {
	timer_id = SetTimer(0, 0, (UINT) msec, app_timer_proc);
	if (timer_id == 0)
	    return 0;
    }
    return 1;
}

/*
 * Start the mouse-down timer with a period of msec milliseconds.
 *
 * Notes: setmousetimer() starts the mouse-down auto-repeat timer.
 *  The timer will not do anything unless the user holds down a
 *  mouse button for longer than msec milliseconds, in which case
 *  it will call the mouserepeat function associated with which ever
 *  window is currently active.
 *  Also, an interval of zero should stop the timer without
 *  destroying the previous interval recorded in mouse_msec.
 */
int setmousetimer(unsigned msec)
{
    if (mouse_timer_id != 0) {
	KillTimer(0, mouse_timer_id);
	mouse_timer_id = 0;
    }
    if (msec == 0) {
	mouse_timer_id = 0;
	return 1;
    }
    else {
	mouse_timer_id = SetTimer(0, 0, (UINT) msec, app_timer_proc);
	if (mouse_timer_id == 0)
	    return 0;
    }
    mouse_msec = msec;
    return 1;
}

/*
 *  Delay execution for a given number of milliseconds.
 *  This is a blocking function which should be used sparingly.
 */
void delay(unsigned msec)
{
    unsigned long now;
    unsigned long stop;

    stop = msec;
    now = GetTickCount();
    stop += now;
    while(now < stop)
	now = GetTickCount();
}

/*
 *  Report current time in milliseconds since initialisation of
 *  the graphics interface. Not reliable for timing events.
 */
long currenttime(void)
{
    return GetTickCount();
}

/*
 *  Intercept menu keys, since we don't always have accelerator tables.
 *  Return 1 if doing something which should not go to the winproc, else 0.
 */
static int TranslateMenuKeys(MSG *msg)
{
    int key = LOWORD(msg->wParam);

    /* Translate F10 from syskey to normal keydown message. */
    if ((key == VK_F10) && (msg->message == WM_SYSKEYDOWN))
	msg->message = WM_KEYDOWN;

    /* Check for menu control keys. */
    /* disabled for R 0.9.1 to 1.9.1.
       Added proper AltGr fix for 2.5.0 */

    if ((GetKeyState(VK_CONTROL) & 0x8000)
	&& (msg->message == WM_KEYDOWN)
	&& !(GetKeyState(VK_RMENU) & 0x8000))
    {
	/* ctrl-letter or ctrl-number is a menu key */
	if (((key >= 'A') && (key <= 'Z')) ||
	    ((key >= '0') && (key <= '9')))
	{
	    if (menus_active && handle_menu_key(key))
		return 1;
	}
    }
    return 0; /* 0 = pass to TranslateMessage and DispatchMessage */
}

/*
 *  Return zero if there are no messages, non-zero otherwise.
 */
int peekevent(void)
{
    return PeekMessage(&msg, 0, 0, 0, PM_NOREMOVE);
}

/*
 *  Wait for the next message
 */
void waitevent(void)
{
    if (!peekevent()) WaitMessage();
}


/*
 *  Handle one event.
 */
int doevent(void)
{
    int result = PeekMessage(&msg, 0, 0, 0, PM_REMOVE);
    HWND modeless = get_modeless();

    if (result)
    {
	/*		del_all_contexts();*/
	if (TranslateMenuKeys(&msg))
	    return result;
	if ((hwndClient) &&
	    TranslateMDISysAccel(hwndClient, &msg))
	    return result;
	if ((hwndFrame) && (hAccel) &&
	    TranslateAccelerator(hwndFrame, hAccel, &msg))
	    return result;
	if ((modeless) && IsDialogMessage(modeless, &msg))
	    return result;
	TranslateMessage(&msg);
	DispatchMessage(&msg);
    }
    deletion_traversal();
    if ((active_windows <= 0) || (msg.message == WM_QUIT))
	return 0;
    else
	return 1;
}

/*
 *  Handle events until the program has finished receiving events,
 *  or until there are no windows open to receive events.
 */
void gamainloop(void)
{
    while (doevent())
	continue;
}

/*
 *  Finish all pending graphics requests.
 */
void drawall(void)
{
    /* Do nothing here. */
}

/*
 *  Initialise the timer and make some instance 'thunks' for
 *  the event callbacks.
 */
PROTECTED
void init_events(void)
{
    uFindReplaceMsg = RegisterWindowMessage(FINDMSGSTRING);
    app_timer_proc = (TIMERPROC) MakeProcInstance((FARPROC) app_timer_procedure,
						  this_instance);
    setmousetimer(100); /* start 1/10 second mouse-down auto-repeat */

    app_control_proc = (WNDPROC) MakeProcInstance((FARPROC) app_control_procedure,
						  this_instance);
}

/*
 *  Stop all timers and release the memory requirements of
 *  the proc instance 'thunks'.
 */
PROTECTED
void finish_events(void)
{
    settimer(0);
    setmousetimer(0);
}
