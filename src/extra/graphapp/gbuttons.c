/* Copyright (C) 1999, 2000 Guido Masarotto
   Copyright (C) 2004, The R Foundation

   Generalized version of widgets in buttons.c


   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/* Win32 style scrollbars */
void
gchangescrollbar(scrollbar obj, int which, int where, int max, int pagesize,
		 int disablenoscroll)
{
    HWND hwnd;
    SCROLLINFO si;

    if (! obj) return;
    hwnd = obj->handle;
    switch (which) {
    case HWINSB:
	obj->xmax = max;
	obj->xsize = pagesize;
	which = SB_HORZ;
	break;
    case VWINSB:
	obj->max = max;
	obj->size = pagesize;
	which = SB_VERT;
	break;
    default:
	obj->max = max;
	obj->size = pagesize;
	which = SB_CTL;
	break;
    }
    si.cbSize = sizeof(si);
    si.fMask  = disablenoscroll ? (SIF_ALL | SIF_DISABLENOSCROLL) : SIF_ALL;
    si.nMin   = 0;
    si.nMax   = max;
    si.nPage  = pagesize;
    si.nPos   = where;
    SetScrollInfo(hwnd, which, &si, 1);
}

void gsetcursor(drawing d, cursor c)
{
    POINT pt;

    decrease_refcount(d->drawstate->crsr);
    d->drawstate->crsr = c;
    increase_refcount(c);
    /* ensure new cursor is shown */
    GetCursorPos(&pt);
    SetCursorPos(pt.x, pt.y);
}

control newtoolbar(int height)
{
    window c = current_window;
    if (!ismdi() || !c || (c==MDIFrame)) return NULL;
    addto(MDIFrame);
    c->toolbar = newwindow("TOOLBAR", rect(0, 0, 100, height),
			   ChildWindow | Border);
    if (c->toolbar) {
	DWORD wcol = GetSysColor(COLOR_MENU);
	hide(c->toolbar);
	setbackground(c->toolbar,
		      rgb( (wcol >> 0) &  0x000000FFL,
			   (wcol >> 8) &  0x000000FFL,
			   (wcol >> 16) &  0x000000FFL));
    }
    addto(c);
    return (control) c->toolbar;
}

/* Fix background color for image on the toolbar.
   Designed to work with image in stdimc.c:
   (a) background is pixel (0,0);
   (b) image depth is 8 bits;
   (c) image is changed not copied.
*/
button newtoolbutton(image img, rect r, actionfn fn) {
    DWORD wcol = GetSysColor(COLOR_MENU);
    rgb    col = rgb( (wcol >> 0) &  0x000000FFL,
		      (wcol >> 8) &  0x000000FFL,
		      (wcol >> 16) &  0x000000FFL);
    img->cmap[img->pixels[0]] = col;
    return newimagebutton(img, r, fn);
}


void scrolltext(textbox c, int lines)
{
    int linecount = sendmessage(c->handle, EM_GETLINECOUNT, 0, 0);
    sendmessage(c->handle, EM_LINESCROLL, 0, linecount - lines);
}


int ggetkeystate()
{
    int k = 0;
    if (GetKeyState(VK_CONTROL)&0x8000)
	k |= CtrlKey;
    if (GetKeyState(VK_MENU)&0x8000)
	k |= AltKey;
    if (GetKeyState(VK_SHIFT)&0x8000)
	k |= ShiftKey;
    return k;
}


/* Extra text editing functions for R, Chris Jackson */
#include <richedit.h>

/* Move the editor caret position lines down.
   If lines is negative moves caret up.
   Stops at the top or the bottom if lines is too big or small.
*/

void scrollcaret(textbox t, int lines)
{
    long currentline, charindex;
    int linecount = sendmessage(t->handle, EM_GETLINECOUNT, 0, 0);
    currentline = sendmessage(t->handle, EM_LINEFROMCHAR, -1, 0);
    lines = (currentline + lines > linecount - 1  ?  linecount - currentline - 1 :  lines);
    lines = (currentline + lines < 0  ?  - currentline  :  lines);
    charindex = sendmessage(t->handle, EM_LINEINDEX, currentline + lines, 0);
    sendmessage(t->handle, EM_SETSEL, charindex, charindex);
}

/* Modification flags for the text editor */

void gsetmodified(textbox t, int modified)
{
    sendmessage(t->handle, EM_SETMODIFY, (WPARAM) modified, 0);
}

int ggetmodified(textbox t)
{
    return sendmessage(t->handle, EM_GETMODIFY, 0, 0);
}

/* Get the length of the current line in an editing control */

int getlinelength(textbox t)
{
    long charindex = sendmessage(t->handle, EM_LINEINDEX, -1, 0);
    return sendmessage(t->handle, EM_LINELENGTH, charindex, 0);
}

/* Copy the current line in the editor to a buffer */

void getcurrentline(textbox t, char *line, int length)
{
    long currentline = sendmessage(t->handle, EM_LINEFROMCHAR, -1, 0);
    *((LPWORD) line) = length*sizeof(WCHAR)+2; /* set first word of buffer to line length in TCHARs as required by EM_GETLINE */
    sendmessage(t->handle, EM_GETLINE, currentline, line);
    /* line[length] = 0; */
}

/* Copy the current selection in the editor to a buffer */

void getseltext(textbox t, char *text)
{
    sendmessage(t->handle, EM_GETSELTEXT, 0, text);
}

/* Change the maximum number of characters (in bytes) allowed in an editor */

void setlimittext(textbox t, long limit)
{
    sendmessage(t->handle, EM_EXLIMITTEXT, 0, limit);
}

long getlimittext(textbox t)
{
    return sendmessage(t->handle, EM_GETLIMITTEXT, 0, 0);
}

/* Test whether the text limit needs to be increased to accommodate
   some new text of length n. If so increase it by required amount
   plus 32K.  */

void checklimittext(textbox t, long n)
{
    long limit = getlimittext(t);
    long len =  GetWindowTextLength(t->handle);
    if (len + n >= limit)
	setlimittext(t, len + n + 0x8000);
}

/* Length of text in the clipboard */

long getpastelength()
{
    HGLOBAL hglb;
    char *text;
    long pastelen;
    if ( OpenClipboard(NULL) &&
	 (hglb = GetClipboardData(CF_TEXT)) &&
	 (text = (char *)GlobalLock(hglb))) {
	pastelen = strlen(text);
	GlobalUnlock(hglb);
	CloseClipboard();
    } else pastelen = 0;
    return pastelen;
}

/* Modification of the text selection functions in buttons.c to work
   with the EX messages used in rich edit controls. The selection
   messages without EX cannot handle more than 64K of text */

void textselectionex(control obj, long *start, long *end)
{
    CHARRANGE sel;
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, EM_EXGETSEL, 0, &sel);
    *start = sel.cpMin;
    *end = sel.cpMax;
}

void selecttextex(control obj, long start, long end)
{
    CHARRANGE sel;
    long length;

    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    length = GetWindowTextLength(obj->handle);
    sel.cpMin = (start < 0) ? length : start;
    sel.cpMax = (end < 0) ? length : end;
    sendmessage(obj->handle, EM_EXSETSEL, 0, &sel);
}
