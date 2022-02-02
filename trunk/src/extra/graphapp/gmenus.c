/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--1999  Guido Masarotto
 *  Copyright (C) 2004 The R Foundation
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*
   Pop-up menus support + convenience methods for definining
   menubar (and pop-up) from static array of MenuItem (defined in
   ga.h)
 */

#include "win-nls.h"
#include "internal.h"

static void mdimenu(menuitem m)
{
    switch (getvalue(m)) {
    case 1:
	SendMessage(hwndClient,WM_MDICASCADE,0,0);
	break;
    case 2:
	SendMessage(hwndClient,WM_MDITILE,MDITILE_HORIZONTAL,0);
	break;
    case 3:
	SendMessage(hwndClient,WM_MDITILE,MDITILE_VERTICAL,0);
	break;	
    case 4:
	SendMessage(hwndClient,WM_MDIICONARRANGE,0,0);
	break;
    }
}

menu newmdimenu()
{
    menu m ;
    if (!ismdi()) return NULL;
    m = newmenu(G_("Windows"));
    setvalue(newmenuitem(G_("Cascade"),0,mdimenu),1);
    setvalue(newmenuitem(G_("Tile &Horizontally"),0,mdimenu),2);
    setvalue(newmenuitem(G_("Tile &Vertically"),0,mdimenu),3);
    setvalue(newmenuitem(G_("Arrange Icons"),0,mdimenu),4);
    current_menubar->menubar = m;
    return m;
}

menu newpopup(actionfn fn)
{
    menu mnew;
    if (!current_window) return 0;
    mnew = newsubmenu(current_window,"");
    mnew->action = fn;
    current_window->popup = mnew;
    current_menu = mnew;
    return mnew;
}

void gchangepopup(window w, menu m)
{
    w->popup = m;
}

void gchangemenubar(menubar mb)
{
    window w = current_window;
    if (!w) return;
    w->menubar = mb;
    SetMenu(w->handle, mb->handle);
    if (ismdi()) {
	menu mdi = (w->menubar)->menubar;
	SendMessage(hwndClient, WM_MDISETMENU,
		    (WPARAM)w->menubar->handle,
		    (LPARAM)(mdi ? (mdi->handle) : 0));
	DrawMenuBar(hwndFrame);
    } else
	DrawMenuBar(w->handle);
}

/* FIXME: only one level of sub-menu - no checks -*/
static int addmenuitemarray(menu m,MenuItem a[])
{
    menu ma  = m;
    int i = 0;
    while (a[i].nm) {
	if (!strcmp(a[i].nm,"@STARTMENU")) {
	    i += 1;
	    ma = newsubmenu(m, G_(a[i].nm));
	}
	else if (!strcmp(a[i].nm,"@ENDMENU")) {
	    i += 1;
	    ma = m;
	}
	else if (!strcmp(a[i].nm,"@STARTSUBMENU")) {
	    i += 1;
	    newsubmenu(ma,a[i].nm);
	}
	else if (!strcmp(a[i].nm,"@ENDSUBMENU")) {
	    i += 1;
	}
	else if (!strcmp(a[i].nm,"@MDIMENU")) {
	    if (!(a[i].m = newmdimenu())) return 0;
	    ma = a[i].m;
	}
	else {
	    if (!(a[i].m = newmenuitem(G_(a[i].nm), a[i].key, a[i].fn))) return 0;
	}
	i += 1;
    }
    return 1;
}

menubar gmenubar(actionfn fn, MenuItem a[])
{
    menubar m = newmenubar(fn);
    if (m)  {
	if (!addmenuitemarray(m,a)) {
	    del(m);
	    m = NULL;
	}
    }
    return m;
}


popup gpopup(actionfn fn, MenuItem a[])
{
    popup m = newpopup(fn);
    if (m)  {
	if (!addmenuitemarray(m,a)) {
	    del(m);
	    m = NULL;
	}
    }
    return m;
}
