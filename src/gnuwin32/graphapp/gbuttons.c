/* Copyright (C) 1999 Guido Masarotto

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
    obj->max = max;
    obj->size = pagesize;
    switch (which) {
    case HWINSB: which = SB_HORZ; break;
    case VWINSB: which = SB_VERT; break;
    default: which = SB_CTL; break;
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
	hide(c->toolbar);
	setbackground(c->toolbar, GetSysColor(COLOR_MENU));
    }
    addto(c);
    return (control) c->toolbar;
}
