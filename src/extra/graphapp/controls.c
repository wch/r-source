/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: controls.c -- manipulating scrollbars, buttons, etc.
 * Platform: Windows  Version: 2.40  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.60  Changes: Added clear(), draw(), flashcontrol().
 * Version: 2.00  Changes: New object class system.
 * Version: 2.15  Changes: Transparent backgrounds added.
 * Version: 2.20  Changes: Non-native buttons supported.
 * Version: 2.35  Changes: New reference count technique.
 * Version: 2.40  Changes: Support for new controls.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/*  Copyright (C) 2004	The R Foundation

    Changes for R:

    sort out resize (confused screen and client coords)
    add printer and metafile handling
    Remove assumption of current->dest being non-NULL

 */

#include "internal.h"
#include <richedit.h>

# define alloca(x) __builtin_alloca((x))

/*
 *  Setting control call-backs.
 */
void setaction(control obj, actionfn fn)
{
    if (obj)
	obj->action = fn;
}

void sethit(control obj, intfn fn)
{
    if (obj)
	obj->hit = fn;
}

void setdel(control obj, actionfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->die = fn;
}

void setclose(control obj, actionfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->close = fn;
}

void setredraw(control obj, drawfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->redraw = fn;
}

void setresize(control obj, drawfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->resize = fn;
}

void setkeydown(control obj, keyfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->keydown = fn;
}

void setkeyaction(control obj, keyfn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->keyaction = fn;
}

void setmousedown(control obj, mousefn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->mousedown = fn;
}

void setmouseup(control obj, mousefn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->mouseup = fn;
}

void setmousemove(control obj, mousefn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->mousemove = fn;
}

void setmousedrag(control obj, mousefn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->mousedrag = fn;
}

void setmouserepeat(control obj, mousefn fn)
{
    if (obj)
	if (obj->call)
	    obj->call->mouserepeat = fn;
}

void setdrop(control obj, dropfn fn)
{
    if (obj)
	if (obj->call) {
	    DragAcceptFiles(obj->handle, TRUE);
	    obj->call->drop = fn;
	}
}

void setonfocus(control obj, actionfn fn)
{
    if (obj)
	obj->call->focus = fn;
}

void setim(control obj, imfn fn)
{
    if (obj)
	obj->call->im = fn;
}

/*
 *  Drawing controls and windows.
 */
void clear(control obj)
{
    drawing prev;
    rgb old;

    if (! obj)
	return;
    if (! isvisible(obj))
	return;
    if (! isvisible(parentwindow(obj)))
	return;
    if (obj->bg == Transparent)
	return;
    prev = current->dest;
    drawto(obj);
    old = currentrgb();
    setrgb(obj->bg);
    fillrect(getrect(obj));
    if (prev) {
	setrgb(old);
	drawto(prev);
    }
}

void draw(control obj)
{
    drawing prev;
    drawstate old = NULL;

    if (! obj)
	return;
    if (obj->kind == MenubarObject) {
	DrawMenuBar(obj->parent->handle);
	return;
    }

    if (! isvisible(obj))
	return;
    if (! isvisible(parentwindow(obj)))
	return;
    if ((obj->call == NULL) || (obj->call->redraw == NULL))
	return;
    prev = current->dest;
    drawto(obj);
    if (prev) old = copydrawstate();
    moveto(pt(0,0));
    obj->call->redraw(obj, getrect(obj));
    if (prev) {
	restoredrawstate(old);
	drawto(prev);
    }
}

void redraw(control obj)
{
    clear(obj);
    draw(obj);
}

/*  void getscreenrect(control obj, rect *r) */
/*  { */
/*      RECT W; */
/*      GetWindowRect(obj->handle, &W); */
/*      r->x = W.left; */
/*      r->y = W.top; */
/*      r->width = W.right - W.left; */
/*      r->height = W.bottom - W.top; */
/*  } */


/* The original here used GetWindowRect (which used screen coordinates)
   and MoveWindow (which uses client coordinates) so got the positioning
   hopelessly wrong.  This version works for WindowObjects, but I would be
   suspicious of it for other cases.  BDR 2000/04/05
*/
void resize(control obj, rect r)
{
    RECT R;
    WINDOWPLACEMENT W;
    int dw, dh, dx, dy;

    if (! obj)
	return;
    r = rcanon(r);
    if (obj->kind == WindowObject) {
	W.length = sizeof(WINDOWPLACEMENT);
	r.x = obj->rect.x;
	r.y = obj->rect.y;
	if (!equalr(r, obj->rect)) {
	    GetWindowPlacement(obj->handle, &W);
	    if (!isvisible(obj)) W.showCmd = SW_HIDE;  /* stops the resize from revealing the window */
	    dx = r.x - obj->rect.x;
	    dy = r.y - obj->rect.y;
	    /* don't believe current sizes!
	       dw = r.width - obj->rect.width;
	       dh = r.height - obj->rect.height;
	       Rprintf("dw %d dh %d\n", dw, dh); */
	    GetClientRect(obj->handle, &R);
	    dw = r.width - (R.right - R.left);
	    dh = r.height - (R.bottom - R.top);
	    W.rcNormalPosition.left += dx;
	    W.rcNormalPosition.top += dy;
	    W.rcNormalPosition.right += dx + dw;
	    W.rcNormalPosition.bottom += dy + dh;
	    SetWindowPlacement(obj->handle, &W);
	}
    }
    else {
	if (! equalr(r, obj->rect))
	    MoveWindow(obj->handle, r.x, r.y, r.width, r.height, 1);
	obj->rect.x = r.x;
	obj->rect.y = r.y;
    }
    obj->rect.width = r.width;
    obj->rect.height = r.height;
}

/*
 *  Showing and hiding controls and windows.
 */
void show(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject: case MenuitemObject:
	break;
    case WindowObject:
	obj->state |= Visible;
	show_window(obj);
	break;
    default:
	ShowWindow(obj->handle, SW_SHOWNORMAL);
	SetFocus(obj->handle);
	UpdateWindow(obj->handle);
    }
    obj->state |= Visible;
}

void hide(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject: case MenuitemObject:
	break;
    case WindowObject:
	hide_window(obj);
	break;
    default:
	ShowWindow(obj->handle, SW_HIDE);
    }
    obj->state &= ~Visible;
}

int isvisible(control obj)
{
    if (! obj)
	return 0;
    return (obj->state & Visible) ? 1 : 0;
}

/*
 *  Enabling and disabling controls and windows.
 */
void enable(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject:
	break;
    case MenuObject: case MenuitemObject:
	EnableMenuItem(obj->parent->handle,
		       obj->id, MF_ENABLED | MF_BYCOMMAND);
	break;
    case FieldObject: case TextboxObject:
	sendmessage(obj->handle, EM_SETREADONLY, FALSE, 0L);
	break;
    default:
	if (! isenabled(obj)) {
	    EnableWindow(obj->handle, 1);
	    obj->state |= Enabled;
	    draw(obj);
	}
    }
    obj->state |= Enabled;
}

void disable(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject:
	break;
    case MenuitemObject:
	EnableMenuItem(obj->parent->handle,
		       obj->id, MF_GRAYED | MF_BYCOMMAND);
	break;
    case FieldObject: case TextboxObject:
	sendmessage(obj->handle, EM_SETREADONLY, TRUE, 0L);
	break;
    default:
	if (isenabled(obj)) {
	    EnableWindow(obj->handle, 0);
	    obj->state &= ~Enabled;
	    draw(obj);
	}
    }
    obj->state &= ~Enabled;
}

int isenabled(control obj)
{
    if (! obj)
	return 0;
    return (obj->state & Enabled) ? 1 : 0;
}

/*
 *  Checking and unchecking controls.
 */
void check(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject:
	break;
    case MenuitemObject:
	CheckMenuItem(obj->parent->handle, obj->id,
		      MF_CHECKED | MF_BYCOMMAND);
	break;
#if USE_NATIVE_BUTTONS
    case ButtonObject:
	sendmessage(obj->handle, BM_SETCHECK, 1, 0L);
	break;
#endif
#if USE_NATIVE_TOGGLES
    case CheckboxObject: case RadioObject:
	sendmessage(obj->handle, BM_SETCHECK, 1, 0L);
	break;
#endif
    default:
	if (! ischecked(obj)) {
	    obj->state |= Checked;
	    draw(obj);
	}
    }
    obj->state |= Checked;
}

void uncheck(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject:
	break;
    case MenuitemObject:
	CheckMenuItem(obj->parent->handle, obj->id,
		      MF_UNCHECKED | MF_BYCOMMAND);
	break;
#if USE_NATIVE_BUTTONS
    case ButtonObject:
	sendmessage(obj->handle, BM_SETCHECK, 0, 0L);
	break;
#endif
#if USE_NATIVE_TOGGLES
    case CheckboxObject: case RadioObject:
	sendmessage(obj->handle, BM_SETCHECK, 0, 0L);
	break;
#endif
    default:
	if (ischecked(obj)) {
	    obj->state &= ~Checked;
	    draw(obj);
	}
    }
    obj->state &= ~Checked;
}

int ischecked(control obj)
{
    if (! obj)
	return 0;
    return (obj->state & Checked) ? 1 : 0;
}

/*
 *  Highlighting and unhighlighting controls.
 */
void highlight(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject: case MenuitemObject:
	break;
#if USE_NATIVE_BUTTONS
    case ButtonObject:
	sendmessage(obj->handle, BM_SETSTATE, 1, 0L);
	break;
#endif
#if USE_NATIVE_TOGGLES
    case CheckboxObject: case RadioObject:
	sendmessage(obj->handle, BM_SETSTATE, 1, 0L);
	break;
#endif
    case FieldObject: case TextboxObject:
	sendmessage(obj->handle, EM_SETSEL, 0, MAKELONG(0,-1));
	break;
    default:
	if (! ishighlighted(obj)) {
	    obj->state |= Highlighted;
	    draw(obj);
	}
    }
    obj->state |= Highlighted;
}

void unhighlight(control obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case CursorObject: case FontObject: case BitmapObject:
    case MenubarObject: case MenuObject: case MenuitemObject:
	break;
#if USE_NATIVE_BUTTONS
    case ButtonObject:
	sendmessage(obj->handle, BM_SETSTATE, 0, 0L);
	break;
#endif
#if USE_NATIVE_TOGGLES
    case CheckboxObject: case RadioObject:
	sendmessage(obj->handle, BM_SETSTATE, 0, 0L);
	break;
#endif
    case FieldObject: case TextboxObject:
	sendmessage(obj->handle, EM_SETSEL, 0, MAKELONG(0,0));
	break;
    default:
	if (ishighlighted(obj)) {
	    obj->state &= ~Highlighted;
	    draw(obj);
	}
    }
    obj->state &= ~Highlighted;
}

int ishighlighted(control obj)
{
    if (! obj)
	return 0;
    return (obj->state & Highlighted) ? 1 : 0;
}

/*
 *  The flashcontrol function highlights a control as if the user
 *  just used it, e.g. a button will become pressed down for a
 *  moment. Used in conjunction with activatecontrol() below,
 *  it can simulate a mouse-click on a button.
 */
void flashcontrol(control obj)
{
    highlight(obj);
    delay(150);
    unhighlight(obj);
}

/*
 *  The activatecontrol function is really useful. It causes
 *  a variety of controls to call their 'action functions.'
 *
 *  There are two types of action functions which a control can use.
 *  The first (obj->action) takes one argument, the control itself,
 *  and is used in buttons, checkboxes and the like as a response
 *  to a user clicking on these controls. The second function,
 *  (obj->hit) function, takes two arguments: the control and its
 *  current 'value'. A scrollbar's value changes as the user scrolls,
 *  and so its hit function is called whenever that happens.
 *
 *  The activatecontrol function tries to call the action function,
 *  and if that doesn't exist, calls the hit function, passing the
 *  object's value to it.
 */
void activatecontrol(control obj)
{
    if (! obj)
	return;
    drawto(obj);
    if (obj->action != NULL)
	obj->action(obj);
    else if (obj->hit != NULL)
	obj->hit(obj, obj->value);
}

/*
 *  Changing and determining the state of an object.
 *  These functions do not automatically redraw the object.
 */
#if 0
void setstate(control obj, long state)
{
    if (obj)
	obj->state = state;
}

long getstate(control obj)
{
    if (obj)
	return obj->state;
    else
	return 0;
}
#endif

void setvalue(control obj, int value)
{
    if (obj)
	obj->value = value;
}

int getvalue(control obj)
{
    if (obj)
	return obj->value;
    else
	return 0;
}

void setforeground(control obj, rgb fg)
{
    if (! obj)
	return;
    obj->fg = fg;
    if (obj->kind == TextboxObject) {
    	if (obj->handle) {    
    	    CHARFORMAT format;
    	    COLORREF wincolour = RGB((fg&gaRed)>>16,(fg&gaGreen)>>8,(fg&gaBlue)); 
    	    format.cbSize = sizeof(format);
    	    format.dwMask = CFM_COLOR;
    	    format.dwEffects = 0;
    	    format.crTextColor = wincolour;

	    sendmessage(obj->handle, EM_SETCHARFORMAT, 0, (LPARAM)&format);
	}
    } else {
    	InvalidateRect(obj->handle, NULL, TRUE);
    	redraw(obj);
    }
}

rgb getforeground(control obj)
{
    if (obj)
	return obj->fg;
    else
	return Black;
}

void setbackground(control obj, rgb bg)
{
    COLORREF wincolour = RGB((bg&gaRed)>>16,(bg&gaGreen)>>8,(bg&gaBlue));

    if (! obj)
	return;
    obj->bg = bg;
    if (obj->kind == TextboxObject) {
    	if (obj->handle)
	    sendmessage(obj->handle, EM_SETBKGNDCOLOR, 0, wincolour);
    } else {
    	if (obj->bgbrush)
	    DeleteObject(obj->bgbrush);
    	obj->bgbrush = CreateSolidBrush(wincolour);

    	InvalidateRect(obj->handle, NULL, TRUE);
    	redraw(obj);
    }
}

rgb getbackground(control obj)
{
    if (obj)
	return obj->bg;
    else
	return Transparent;
}

void setdata(control obj, void *data)
{
    if (obj)
	obj->data = data;
}

void *getdata(control obj)
{
    if (obj)
	return obj->data;
    else
	return NULL;
}

/* These two are in none of the headers */
#ifdef UNUSED
void _setextradata(control obj, void *data)
{
    if (obj)
	obj->extra = data;
}

void *_getextradata(control obj)
{
    if (obj)
	return obj->extra;
    else
	return NULL;
}
#endif

/*
 *  Set the text of an object. This will set the names appearing
 *  in a window's title bar, a button, checkbox or radio button,
 *  or the value in a textbox or a text field.
 */
void settext(control obj, const char *text)
{
    char *old_text;

    if (! obj)
	return;
    if (! text)
	text = "";
    old_text = GA_gettext(obj);
    if (old_text && strcmp(old_text, text) == 0)
	return; /* no changes to be made */
    if (obj->text) {
	/* discard prior information */
	discard(obj->text);
	obj->text = NULL;
    }
    /* Set the new text. */
    obj->text = new_string(text);
    if (text) {
	if (obj->kind & ControlObject) {
	    text = to_dos_string(text);
	    if(localeCP > 0 && (localeCP != GetACP())) {
		/* This seems not actually to work */
		wchar_t *wc;
		int nc = strlen(text) + 1;
		wc = (wchar_t*) alloca(nc*sizeof(wchar_t));
		mbstowcs(wc, text, nc);
		SetWindowTextW(obj->handle, wc);
	    } else SetWindowText(obj->handle, text);
	    discard(text);
	}
	if (obj->kind == MenuitemObject) {
	    if(localeCP > 0 && (localeCP != GetACP())) {
		/* But this does */
		wchar_t wc[1000];
		mbstowcs(wc, text, 1000);
		ModifyMenuW(obj->parent->handle, obj->id,
			    MF_BYCOMMAND|MF_STRING, obj->id, wc);

	    } else
		ModifyMenu(obj->parent->handle, obj->id,
			   MF_BYCOMMAND|MF_STRING, obj->id, text);
	}

    }
    /* Redraw it if it's a redrawable object. */
    if (obj->call && obj->call->redraw)
	redraw(obj);
}

/*
 *  Get the text string from a window's title bar or from a
 *  control. This may be a button's name, for example, or
 *  the value inside a text field.
 */
char *GA_gettext(control obj)
{
    static char *empty = "";
    char *text;
    int length, index;
    HWND hwnd;
    UINT len_msg, gettext_msg;
    WPARAM arg1, arg2;

    if (! obj)
	return empty;
    if ((obj->kind & ControlObject) == 0)
	return obj->text ? obj->text : empty;

    hwnd = obj->handle;

    switch (obj->kind) {
    case ListboxObject:
    case MultilistObject:
	len_msg = LB_GETTEXTLEN;
	gettext_msg = LB_GETTEXT;
	index = getlistitem(obj);
	if (index < 0) return empty;
	arg1 = arg2 = index;
	break;
    case DroplistObject:
	len_msg = CB_GETLBTEXTLEN;
	gettext_msg = CB_GETLBTEXT;
	index = getlistitem(obj);
	if (index < 0) return empty;
	arg1 = arg2 = index;
	break;
    case DropfieldObject:
	// use default mechanism
    default:
	len_msg = WM_GETTEXTLENGTH;
	gettext_msg = WM_GETTEXT;
	arg1 = 0;
	arg2 = sendmessage(hwnd, len_msg, 0, 0L)+1;
	break;
    }

    /* Free any previous information. */
    if (obj->text)
	discard(obj->text);
    /* Find the length of the string. */
    length = sendmessage(obj->handle, len_msg, arg1, 0L);
    if (length == 0)
	return (obj->text = new_string(NULL));
    /* Copy the text from the object. */
    text = array(length+2, char);
    sendmessage(obj->handle, gettext_msg, arg2, (LPCSTR) text);
    obj->text = to_c_string(text);
    discard(text);
    /* Return the resultant string. */
    if (! obj->text)
	obj->text = new_string(NULL);
    return obj->text;
}

/*
 *  Set the font used by a control.
 */
void settextfont(object obj, font f)
{
    if (! obj)
	return;
    if (! f)
	f = SystemFont;
    if (obj->drawstate) {
	decrease_refcount(obj->drawstate->fnt);
	obj->drawstate->fnt = f;
	increase_refcount(f);
    }
    else {
	sendmessage(obj->handle, WM_SETFONT, f->handle, 0L);
    }
}

/*
 *  Get the font used by a control.
 */
font gettextfont(object obj)
{
    font f = NULL;
    if (obj) {
	if (obj->drawstate)
	    f = obj->drawstate->fnt;
    }
    if (! f)
	f = SystemFont;
    return f;
}

/*
 *  Control and window functions.
 *  Parentwindow of a window is itself.
 */
window parentwindow(control obj)
{
    while (obj) {
	if (obj->kind == WindowObject)
	    break;
	obj = obj->parent;
    }
    return (window) obj;
}

/*
 *  Polymorphic functions:
 */
rect objrect(object obj)
{
    rect r;
    image img;

    if (! obj)
	return rect(0,0,0,0);

    switch (obj->kind)
    {
    case Image8: case Image32:
	img = (image) obj;
	r = rect(0,0,img->width,img->height);
	break;
    case BitmapObject:
    case FontObject:
    case CursorObject:
    case PrinterObject:
	r = obj->rect;
	break;
    case MetafileObject:
	r = obj->rect;
	break;
    default:
	GetClientRect(obj->handle, rect2RECT(&r));
	break;
    }
    return r;
}

int objwidth(object obj)
{
    rect r = objrect(obj);
    return r.width;
}

int objheight(object obj)
{
    rect r = objrect(obj);
    return r.height;
}

int objdepth(object obj)
{
    HDC screendc;
    int depth;

    if (! obj)
	return 0;

    switch (obj->kind)
    {
    case Image8: case Image32:
	depth = ((image)obj)->depth;
	break;
    case BitmapObject:
    case FontObject: case CursorObject:
	depth = obj->depth;
	break;
    default:
	screendc = GetDC(NULL);
	depth = GetDeviceCaps(screendc, BITSPIXEL) *
	    GetDeviceCaps(screendc, PLANES);
	ReleaseDC(NULL, screendc);
	break;
    }

    return depth;
}

void delobj(object obj)
{
    if (! obj)
	return;
    switch (obj->kind)
    {
    case Image8:
    case Image32:
	delimage((image)obj);
	break;
    default:
	/* if (obj->refcount == 1)   why would this test be here?? */
	decrease_refcount(obj);
	break;
    }
}

void setcaret(object obj, int x, int y, int width, int height)
{
    if (! obj)
    	return;
    if (width != obj->caretwidth || height != obj->caretheight) {
	if (obj->caretwidth > 0 && (obj->state & Focus)) DestroyCaret();
	obj->caretwidth = width;
	obj->caretheight = height;
	if (width > 0) {
	    if (obj->state & Focus)
		CreateCaret(obj->handle, (HBITMAP) NULL, width, height);
	    obj->caretshowing = 0;
	}
    }
    if (obj->state & Focus)
    	SetCaretPos(x, y);
}

void showcaret(object obj, int showing)
{
    if (! obj || showing == obj->caretshowing)
    	return;
    obj->caretshowing = showing;
    if (showing)
    	ShowCaret(obj->handle);
    else
    	HideCaret(obj->handle);
}
