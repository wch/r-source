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


