/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: menus.c -- creating menus, menubars, etc.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: New object class system.
 * Version: 2.15  Changes: Added submenu creation function.
 * Version: 2.20  Changes: New menuitem constructor.
 * Version: 2.30  Changes: Modified the shortcut key selector.
 * Version: 2.35  Changes: New reference count technique.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/*
 *  When you create menubars, menus and menuitems, they are added
 *  to the currentwindow. Menus are added to the current_menubar and
 *  menuitems are added to the current_menu.
 */

/* Copyright (C) 2004 	The R Foundation

   Change for R - Chris Jackson.  Enabled menu shortcut keys, don't
   execute shortcut if menu item is grayed out */

#include "internal.h"
/*#include "config.h" */
#include <wchar.h>
#define mbs_init(x) memset(&x,0,sizeof(x))
size_t Rf_mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);

/*
 *  Menu variables.
 */

	PROTECTED int     menus_active = 1;

	PROTECTED menubar current_menubar = NULL;
	PROTECTED menu    current_menu    = NULL;

	static    int     id = MinMenuID;

	PROTECTED HACCEL hAccel = 0;

/*
 *  Initialise menus. Actually, all we do here is load accelerators.
 *  The accelarator resource must have the name "ACCELS".
 *  If there are no accelerator resources, hAccel will be zero.
 */
PROTECTED
void init_menus(void)
{
	if (this_instance != 0)
		hAccel = LoadAccelerators(this_instance, "ACCELS");
}

/*
 *  Find the position of a menu on its parent menubar.
 *  Return zero for the first menu, one for the next etc.
 */
static int find_menu_position(menu parent, char *name)
{
	menu m, first;
	int which = 0;

	first = m = parent->child;
	if (first) {
		do {
			if (strcmp(m->text, name) == 0)
				break;
			which ++;
			m = m->next;
		} while (m != first);
	}
	return which;
}

/*
 *  Private menu deletion function.
 */
static void private_delmenu(menu m)
{
	window w = parentwindow(m);
	if (m->kind == MenuitemObject)
		RemoveMenu(m->parent->handle, m->id, MF_BYCOMMAND);
	else if (m->kind == MenuObject) {
		DeleteMenu(m->parent->handle,
			find_menu_position(m->parent, m->text),
			MF_BYPOSITION);
	}
	else
		DestroyMenu(m->handle);
	if (w)
		DrawMenuBar(w->handle);
}

/*
 *  Find a char within a string.
 *  Return -1 if not found, or a number from 0 to strlen(str)-1
 *  to indicate where the char is.
 */
char *Rf_strchr(const char *s, int c); /* from util.c, MBCS-aware */

static int find_char(int ch, const char *str)
{
	char *p;
	p = Rf_strchr(str, ch);
	if(!p) return -1; else return p - str;
}

/*
 *  This function forms a search string, to look for the best
 *  letter to underline in a menu item. It first tries the
 *  accelerator key, then the uppercase letters from the menu name,
 *  then the digits, and finally the lowercase letters.
 *  It ignores spaces.
 */
static void set_search_string(char *search, const char *name, int key)
{
	int source;
	int dest = 0;
	int mb_len;
	mbstate_t mb_st;

	/* handle a couple of special cases first */
	if (! string_diff(name, "Cut"))  search[dest++] = 't';
	if (! string_diff(name, "Exit")) search[dest++] = 'x';
	if (! string_diff(name, "?")) search[dest++] = '?';

	/* add the accelerator key if it is in the name string */
	if (key) {
		key = toupper(key);
		if (find_char(key, name) >= 0)
			search[dest++] = (char) key;
		else {
			key = tolower(key);
			if (find_char(key, name) >= 0)
				search[dest++] = (char) key;
		}
	}
	/* add the uppercase letters */
	for (source=0; name[source]; source++) {
		mbs_init(mb_st);
	        mb_len = Rf_mbrtowc(NULL, name + source, MB_CUR_MAX,&mb_st);
	        if (mb_len > 1) source += mb_len-1;
	        else
		if (isupper(name[source])) search[dest++] = name[source];
	}
	/* add the digits */
	for (source=0; name[source]; source++) {
                mbs_init(mb_st);
	        mb_len = Rf_mbrtowc(NULL, name + source, MB_CUR_MAX,&mb_st);
	        if (mb_len > 1) source += mb_len-1;
	        else
		if (isdigit(name[source])) search[dest++] = name[source];
	}
	/* add the lowercase letters */
	for (source=0; name[source]; source++) {
                mbs_init(mb_st);
	        mb_len = Rf_mbrtowc(NULL, name + source, MB_CUR_MAX,&mb_st);
	        if (mb_len > 1) source += mb_len-1;
	        else
		    if (islower(name[source])) search[dest++] = name[source];
	}
	/* end the search string */
	search[dest] = '\0';
}

/*
 *  Look through the list of siblings of the given object, and
 *  find a shortcut key from the search string which hasn't already
 *  been used by some other object. Return the valid shortcut char,
 *  or else zero if we can't find one.
 */
static int find_shortcut(object me, char *search)
{
	int source;
	object first, obj;

	first = me->parent->child;

	for (source = 0; search[source]; source++)
	{
	    int mb_len;
	    mbstate_t mb_st;
	    mbs_init(mb_st);
	    mb_len = Rf_mbrtowc(NULL,search + source,MB_CUR_MAX,&mb_st);
	    if ( mb_len > 1 ){
	      source += mb_len-1;
	    } else   
		/* for each character in the search string */
		/* look through every sibling object */

		for (obj = first; obj; obj = obj->next)
		{
			if (obj == me) /* at end of list, success! */
				return search[source];

			/* use uppercase comparisons */
			if (obj->shortcut == toupper(search[source]))
				break; /* can't use this shortcut */

		}
	}

	return 0;
}

/*
 *  Take "Open..." and 'O' and produce the string "Open...\tCtrl+O".
 *  This function also sets an object's shortcut key, which is the
 *  underlined letter in a menu item in Windows or X-Windows.
 */
static void setmenustring(object obj, char *buf, const char *name, int key)
{
	char search[256];
	int ch, where, source, dest = 0;
	char *extra = "\tCtrl+";

	set_search_string(search, name, key);
	ch = find_shortcut(obj, search);

	if (ch) /* found a valid shortcut key */
	{
		obj->shortcut = toupper(ch); /* case-insensitive */
		where = find_char(ch, name);

		for (source=0; source < where; source++)
		  {
		    int mb_len;
		    int i;
		    mbstate_t mb_st;
		    mbs_init(mb_st);
		    mb_len = Rf_mbrtowc(NULL,name + source,MB_CUR_MAX,&mb_st);
		    if ( mb_len > 1 ){
		      for ( i=0 ; i<mb_len ; i++){
			buf[dest++] = name[source+i];
		      }
			source += mb_len-1;
		    } else   
			buf[dest++] = name[source];
		  }
		buf[dest++] = '&';
		for (; name[source]; source++)
			buf[dest++] = name[source];
	}
	else /* no shortcut key, just copy the name string */
	{
		for (source=0; name[source]; source++)
			buf[dest++] = name[source];
	}

	if (key) {
		for (source=0; extra[source]; source++)
			buf[dest++] = extra[source];
		buf[dest++] = key;
	}

	buf[dest] = '\0';
}

/*
 *  Menu functions.
 */
menubar newmenubar(actionfn adjust_menus)
{
	object obj;
	HMENU hm;

	if (! current_window) {
		current_window = simple_window();
		show(current_window);
	}

	hm = CreateMenu();
	obj = new_object(MenubarObject, hm, current_window);
	if (obj) {
                current_window->menubar = obj;
                obj->menubar = NULL;
		obj->die = private_delmenu;
		obj->id = id++;
		obj->action = adjust_menus;
		obj->text = new_string("Menubar");
		current_menubar = obj;
		SetMenu(current_window->handle, hm);
	}
	return (menubar) obj;
}

BOOL myAppendMenu(HMENU h, UINT flags, UINT id, LPCTSTR name)
{
    if(is_NT && (localeCP != GetACP())) {
	wchar_t wc[100];
	mbstowcs(wc, name, 100);
	return AppendMenuW(h, flags, id, wc);
    } else
	return AppendMenuA(h, flags, id, name);
}


menu newsubmenu(menu parent, const char *name)
{
	object obj;
	HMENU hm;
	UINT flags = MF_POPUP;
	char str[256];
	if (! parent) {
		if (! current_menubar)
			current_menubar = newmenubar(NULL);
		parent = current_menubar;
	}

	if (! parent)
		return NULL;
	if (! name)
		name = "";
	if (name[0] == '\t') {
		name += 1;
		flags |= MF_HELP;
	}

	if (parent->kind == WindowObject)
           hm = CreatePopupMenu();
        else
           hm = CreateMenu();
	obj = new_object(MenuObject, hm, parent);
	if (obj) {
		obj->die = private_delmenu;
		obj->id = id++;
		obj->text = new_string(name);
		setmenustring(obj, str, name, 0);
		name = str;
		current_menu = obj;
	}
	if (parent->kind != WindowObject)
             myAppendMenu(parent->handle, flags, (UINT) hm, name);
	if (parent == current_menubar)
		DrawMenuBar(current_menubar->parent->handle);

	return (menu) obj;
}

menu newmenu(const char *name)
{
	return newsubmenu(current_menubar, name);
}


menuitem newmenuitem(const char *name, int key, menufn fn)
{
	object obj;
	UINT flags;
	char str[256];

	if (! current_menu)
		current_menu = newmenu("Special");
	if (! current_menu)
		return NULL;
	if (! name)
		name = "-"; /* separator */

	key = toupper(key); /* make it uppercase */

	obj = new_object(MenuitemObject, 0, current_menu);
	if (obj) {
		obj->die = private_delmenu;
		obj->id = id++;
		obj->key = key;
		obj->action = fn;
		obj->value = 0;
		obj->text = new_string(name);
		obj->state |= Enabled;

		if (name[0] == '-') {
			flags = MF_SEPARATOR;
			name = NULL;
		} else {
			flags = MF_STRING;
			setmenustring(obj, str, name, key);
			name = str;
		}

		myAppendMenu(current_menu->handle, flags, obj->id, name);
	}
	return (menuitem) obj;
}

/*
 *  Find various parent objects of a menu (or any object).
 */
#if 0
PROTECTED
object parent_menubar(object obj)
{
	while (obj) {
		if (obj->kind == MenubarObject)
			break;
		obj = obj->parent;
	}
	return obj;
}

PROTECTED
object parent_menu(object obj)
{
	while (obj) {
		if (obj->kind == MenuObject)
			break;
		obj = obj->parent;
	}
	return obj;
}
#endif

/*
 *  The adjust_menu function is called just after the program
 *  receives a WM_INITMENU message.  This message is sent even if
 *  the user is attempting to use the system menu, which is nice.
 *  We use this function to keep menubars and menus up-to-date by
 *  calling their respective action functions.
 */
PROTECTED
void adjust_menu(WPARAM wParam)
{
	object obj;
	obj = find_by_handle((HANDLE)wParam);
	if (obj) {
		activatecontrol(obj);
		if (obj->kind == MenubarObject)
			DrawMenuBar(obj->parent->handle);
	}
}

/*
 *  Handle the menu selection. wParam is what WM_[SYS]COMMAND sets
 *  it to i.e. the object id number. The menus will already have
 *  received WM_INITMENU messages so they will be up-to-date.
 */
PROTECTED
void handle_menu_id (WPARAM wParam)
{
	object obj;

	obj = find_by_id(wParam);
	if (obj)
		activatecontrol(obj);
}

/*
 *  Handle a menu accelerator key. The key will be a normal char,
 *  in the range of 0..9 or A..Z (not a..z because we use KEYDOWNs).
 *  Uses a recursive function to call the menu adjustor functions
 *  in order from menubar down to popup menu.
 */

static void adjust_menus_top_down(object obj)
{
	/* Recursively step up the list. */
	if (! obj)
		return;
	adjust_menus_top_down(obj->parent);

	/* Adjust menubar then child menus as we descend. */
	if (obj->kind == MenubarObject) {
		activatecontrol(obj);
		DrawMenuBar(obj->parent->handle);
	}
	else if (obj->kind == MenuObject)
		activatecontrol(obj);
}

/* Only search for the key within the menus of the focused window.
   If key is not found fall through to user's key handler. CJ */

PROTECTED
int handle_menu_key(WPARAM wParam)
{
    object win, obj;
    win = find_by_handle(GetFocus());
    if (win) {
    	if (win->kind != WindowObject)
	    win = win->parent;
    	obj = find_by_key(win, wParam);
        if (obj) {
	    adjust_menus_top_down(obj);
	    if (isenabled(obj)) /* Don't do menu actions which are greyed out. CJ */
		activatecontrol(obj);
	    return 1;
	}
    }
    return 0;
}
