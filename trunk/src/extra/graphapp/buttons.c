/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: buttons.c -- creating buttons, scrollbars, etc.
 * Platform: Windows  Version: 2.41  Date: 1998/06/06
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.60  Changes: Minor changes.
 * Version: 2.00  Changes: New object class system.
 * Version: 2.10  Changes: Radiogroups fully implemented.
 * Version: 2.20  Changes: Added non-native buttons.
 * Version: 2.21  Changes: 32-bit fix by Wim Rijnders.
 * Version: 2.22  Changes: Added newtextarea function.
 * Version: 2.23  Changes: Added newpassword function.
 * Version: 2.30  Changes: Added imagebuttons.
 * Version: 2.35  Changes: Added newpicture function.
 * Version: 2.40  Changes: Added new list controls.
 * Version: 2.41  Changes: Updated imagebuttons.
 * Version: 2.42  Changes: Fixed drop-down list height.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/* Copyright (C) 2004-8  The R Foundation

   Changes for R:

   set the system font for labels
   add newscrollbar, field_no_border
   add extended selections, pass on de-selections

 */

#include "internal.h"
#include "ga.h"
#include <richedit.h>

#define SHADOW_WIDTH 1

#define isarmed(obj) ((obj)->state & GA_Armed)
#define arm(obj)     ((obj)->state |= GA_Armed)
#define disarm(obj)  ((obj)->state &= ~GA_Armed)

#define hasfocus(obj) ((obj)->state & GA_Focus)
#define setfocus(obj) SetFocus((obj)->handle)

/*
 *  Ensure there is a current window, create one if there isn't.
 *  If a window is created, it is also shown on the screen.
 */
static void ensure_window(void)
{
    if (! current_window) {
	current_window = simple_window();
	show(current_window);
    }
}

/*
 *  Save the winproc for later and set our own winproc instead.
 */
static void set_new_winproc(object obj)
{
    HWND hwnd;
    hwnd = obj->handle;
#ifdef _WIN64
    obj->winproc = (WNDPROC) GetWindowLongPtr(hwnd, GWLP_WNDPROC);
    SetWindowLongPtr(hwnd, GWLP_WNDPROC, (LONG_PTR) app_control_proc);
#else
    obj->winproc = (WNDPROC) GetWindowLong(hwnd, GWL_WNDPROC);
    SetWindowLongPtr(hwnd, GWL_WNDPROC, (LONG) app_control_proc);
#endif
}

/*
 *  Private object destructor.
 */
static void private_delcontrol(control obj)
{
    ShowWindow(obj->handle, SW_HIDE);
#if USE_NATIVE_CONTROLS
    if (obj->bgbrush)
	DeleteObject(obj->bgbrush);
#endif
    DestroyWindow(obj->handle);
}

/*
 *  Creating controls.
 */
control newcontrol(const char *text, rect r)
{
    control obj;

    ensure_window();
    obj = newwindow(text, r, ChildWindow | TrackMouse);
    if (obj) {
	obj->kind = UserObject;
	obj->die = private_delcontrol;
    }
    set_new_winproc(obj); /* set custom winproc */
    show(obj);

    return obj;
}

drawing newdrawing(rect r, drawfn fn)
{
    drawing obj = newcontrol(NULL, r);
    if (obj) {
	setredraw(obj, fn);
	show(obj);
	draw(obj);
    }
    return obj;
}

static object newchildwin(const char *kind, const char *text,
			  unsigned long style, rect r, actionfn fn)
{
    HWND hwnd;
    object obj;

    ensure_window();
    r = rcanon(r);

    if(localeCP > 0 && (localeCP != GetACP())) {
	/* This seems not actually to work */
	wchar_t wkind[100], wc[1000];
	mbstowcs(wkind, kind, 100);
	mbstowcs(wc, text, 1000);
	hwnd = CreateWindowW(wkind, wc,
			     (WS_CHILD | WS_VISIBLE) | style,
			     r.x, r.y, r.width, r.height,
			     current_window->handle,
			     (HMENU) child_id, this_instance, NULL);
    } else
	hwnd = CreateWindow(kind, text,
			    (WS_CHILD | WS_VISIBLE) | style,
			    r.x, r.y, r.width, r.height,
			    current_window->handle,
			    (HMENU) child_id, this_instance, NULL);


    obj = new_object(ControlObject, hwnd, current_window);
    if (! obj) {
	DestroyWindow(hwnd);
	return NULL;
    }
    obj->die = private_delcontrol;
    obj->rect = r;
    obj->id = child_id++;
    obj->action = fn;
    obj->state = (GA_Visible | GA_Enabled);
    obj->flags = ChildWindow;
    obj->text = new_string(text);
    set_new_winproc(obj); /* set custom winproc */
    settextfont(obj, SystemFont);
    return obj;
}

/*
 *  The old native version of newlabel.
 */
#if USE_NATIVE_LABELS
label newlabel(const char *text, rect r, int alignment)
{
    label obj;
    unsigned long style = SS_LEFT;

    if ((alignment & AlignRight) == AlignRight)
	style = SS_RIGHT;
    if ((alignment & Center) == Center)
	style = SS_CENTER;

    obj = newchildwin("static", text, style, r, NULL);
    obj->kind = LabelObject;
    setbackground(obj, getbackground(parentwindow(obj)));
    return obj;
}
#else
/*
 *  Private label redraw function.
 */
static void draw_label(control c, rect r)
{
    /* Draw the label. */
    if (!isenabled(c))
	setcolour(Grey);
    else
	setcolour(Black);
    setfont(gettextfont(c));
    drawtext(r, getvalue(c), getname(c));
}

/*
 *  Create a new static text label. Now implemented using
 *  GraphApp code instead of native MS-Windows "static"
 *  text. This gives more flexibility, better cross-platform
 *  support and there are no 'look and feel' issues with
 *  labels, so no problems are introduced by doing this.
 */
control newlabel(const char *text, rect r, int alignment)
{
    control obj = newcontrol(text, r);
    if (obj) {
	obj->kind = LabelObject;
	setredraw(obj, draw_label);
	setvalue(obj, alignment);
	setbackground(obj, getbackground(parentwindow(obj)));
	settextfont(obj, SystemFont);
	show(obj);
    }
    return obj;
}
#endif

/*
 *  Uncheck any neighbouring radio buttons, up to any bounding
 *  radiogroup objects, or to the start or end of the list of
 *  siblings.
 */
static void uncheck_neighbours(object obj)
{
    object first = obj->parent->child;
    object last  = first->prev;
    object which;

    for (which=obj; ; which=which->prev) {
	if (which->kind == RadiogroupObject)
	    break;
	if ((which->kind == RadioObject) && (which != obj))
	    uncheck(which);
	if (which == first)
	    break;
    }
    for (which=obj; ; which=which->next) {
	if (which->kind == RadiogroupObject)
	    break;
	if ((which->kind == RadioObject) && (which != obj))
	    uncheck(which);
	if (which == last)
	    break;
    }
}

/*
 *  A radiogroup separates radiobuttons from each other.
 */
object newradiogroup(void)
{
    object obj;
    ensure_window();
    obj = new_object(RadiogroupObject, NULL, current_window);
    return obj;
}

/*
 *  Draw a button shadow.
 */
static void draw_shadow(rect r, rgb col1, rgb col2, int size)
{
    rgb hue = current->hue;

    /* Draw top-left button border. */
    setcolour(col1);
    fillrect(rect(r.x,r.y,r.width,size));
    fillrect(rect(r.x,r.y,size,r.height));

    /* Draw bottom-right button border. */
    setcolour(col2);
    while (size > 0) {
	fillrect(rect(r.x+r.width-1,r.y,1,r.height));
	fillrect(rect(r.x,r.y+r.height-1,r.width,1));
	r = insetr(r,1);
	size--;
    }
    setcolour(hue);
}

/*
 *  Various button call-backs.
 */
static void button_mousedown(control c, int buttons, point xy)
{
    arm(c);
    highlight(c);
}

static void button_mousemove(control c, int buttons, point xy)
{
    if (!isarmed(c))
	return;
    if (buttons && ptinr(xy,getrect(c)))
	highlight(c);
    else
	unhighlight(c);
}

static void button_mouseup(control c, int buttons, point xy)
{
    if (!isarmed(c))
	return;
    disarm(c);
    unhighlight(c);
    if (ptinr(xy, getrect(c)))
	activatecontrol(c);
}

static void button_keydown(control c, int ch)
{
    if (ch == ' ') {
	flashcontrol(c);
	activatecontrol(c);
    }
}

/*
 *  Various checkbox call-backs.
 */
static void checkbox_mousedown(control c, int buttons, point xy)
{
    arm(c);
    highlight(c);
}

static void checkbox_mousemove(control c, int buttons, point xy)
{
    if (!isarmed(c))
	return;
    if (buttons && ptinr(xy,getrect(c)))
	highlight(c);
    else
	unhighlight(c);
}

static void checkbox_mouseup(control c, int buttons, point xy)
{
    if (! isenabled(c))
	return;
    disarm(c);
    unhighlight(c);
    if (! ptinr(xy, getrect(c)))
	return;
    if (ischecked(c))
	uncheck(c);
    else
	check(c);
    activatecontrol(c);
}

static void checkbox_keydown(control c, int ch)
{
    if (ch == ' ') {
	if (ischecked(c))
	    uncheck(c);
	else
	    check(c);
	activatecontrol(c);
    }
}

/*
 *  The imagebutton type:
 */

void setimage(control obj, image img)
{
    /* Change the stored image and draw it. */
    obj->img = img;
    draw(obj);
}

static void draw_image_button(button obj, rect r)
{
    image   img;
    bitmap	store = NULL;
    rect    ir;
    rgb     up, down;
    rgb     old = currentcolour();

    img = obj->img;
    if (has_transparent_pixels(img)) {
	store = newbitmap(r.width, r.height, 0);
	drawto(store);
	setcolour(getbackground(obj));
	fillrect(r);
    }

    if (img) {
	ir = insetr(r,2);
	if (ishighlighted(obj)) /* button is pressed */
	    ir.x += 1, ir.y += 1;

	/* Draw the button image. */
	if (ischecked(obj))
	    drawdarker(img, ir, getrect(img));
	else if (isenabled(obj))
	    drawimage(img, ir, getrect(img));
	else
	    drawgreyscale(img, ir, getrect(img));

	if (ishighlighted(obj)) { /* fill the gap */
	    ir.x -= 1, ir.y -= 1;
	    setcolour(getbackground(obj));
	    drawline(topleft(ir),topright(ir));
	    drawline(topleft(ir),bottomleft(ir));
	}
    }

    /* Draw button border. */
    setcolour(getforeground(obj));
    setlinewidth(1);
    drawrect(r);

    /* Draw button shadow. */
    up = White, down = Grey;
    if (ishighlighted(obj))
	up = Grey, down = LightGrey;
    draw_shadow(insetr(r,1), up, down, 1);

    if (store != NULL) {
	drawto(obj);
	copyrect(store, pt(0,0), getrect(store));
	del(store);
    }

    setcolour(old);
}


button newimagebutton(image img, rect r, actionfn fn)
{
    button obj;

    obj = newcontrol(NULL, r);
    if (! obj)
	return NULL;

    setredraw(obj, draw_image_button);
    setmousedown(obj, button_mousedown);
    setmousemove(obj, button_mousemove);
    setmousedrag(obj, button_mousemove);
    setmouseup(obj, button_mouseup);
    setkeydown(obj, button_keydown);
    setaction(obj, fn);
    setbackground(obj, LightGrey);
    settextfont(obj, SystemFont);

    setimage(obj, img);

    show(obj);
    return obj;
}

static void draw_picture(control obj, rect r)
{
    image   img;
    bitmap	store = NULL;
    rgb     old = currentcolour();

    img = obj->img;
    if (has_transparent_pixels(img)) {
	store = newbitmap(r.width, r.height, 0);
	drawto(store);
	setcolour(getbackground(obj));
	fillrect(r);
    }

    if (img) {
	/* Draw the button image. */
	if (ischecked(obj))
	    drawdarker(img, r, getrect(img));
	else if (isenabled(obj))
	    drawimage(img, r, getrect(img));
	else
	    drawimage(img, r, getrect(img)); /* never grey */
    }

    if (store != NULL) {
	drawto(obj);
	copyrect(store, pt(0,0), getrect(store));
	del(store);
    }

    setcolour(old);
}

control newpicture(image img, rect r)
{
    control obj;

    obj = newcontrol(NULL, r);
    if (! obj)
	return NULL;

    setredraw(obj, draw_picture);
    setimage(obj, img);
    disable(obj);

    show(obj);
    return obj;
}

button newimagecheckbox(image img, rect r, actionfn fn)
{
    button obj;

    obj = newdrawing(r, draw_image_button);
    if (! obj)
	return NULL;

    setmousedown(obj, checkbox_mousedown);
    setmousemove(obj, checkbox_mousemove);
    setmousedrag(obj, checkbox_mousemove);
    setmouseup(obj, checkbox_mouseup);
    setkeydown(obj, checkbox_keydown);
    setaction(obj, fn);
    setbackground(obj, LightGrey);
    settextfont(obj, SystemFont);

    setimage(obj, img);

    show(obj);
    return obj;
}


#if USE_NATIVE_BUTTONS
button newbutton(const char *text, rect r, actionfn fn)
{
    button obj;
    obj = newchildwin("button", text, BS_PUSHBUTTON, r, fn);
    if (obj)
	obj->kind = ButtonObject;
    return obj;
}
#else
static void draw_button(control c, rect r)
{
    rect textrect;
    rgb up, down;
    font f;
    rgb old = currentcolour();

    clear(c);

    /* Draw the button name. */
    if (isenabled(c))
	setcolour(getforeground(c));
    else
	setcolour(Grey);
    f = gettextfont(c);
    setfont(f);
    textrect = r;
    if (ishighlighted(c))
	textrect.x += 1, textrect.y += 1;
    drawtext(textrect, Center|VCenter, getname(c));

    /* Draw button border. */
    setlinewidth(1);
    drawrect(r);
    r = insetr(r,1);

    /* Draw button shadow. */
    up = White, down = Grey;
    if (ishighlighted(c))
	up = Grey, down = LightGrey;
    else if (hasfocus(c)) {
	setcolour(Black);
	drawrect(r);
	r = insetr(r,1);
    }
    draw_shadow(r, up, down, SHADOW_WIDTH);

    setcolour(old);
}

button newbutton(const char *text, rect r, actionfn fn)
{
    button obj = newcontrol(text, r);
    if (obj) {
	obj->kind = ButtonObject;
	setredraw(obj, draw_button);
	setmousedown(obj, button_mousedown);
	setmousemove(obj, button_mousemove);
	setmousedrag(obj, button_mousemove);
	setmouseup(obj, button_mouseup);
	setkeydown(obj, button_keydown);
	setaction(obj, fn);
	setbackground(obj, LightGrey);
	settextfont(obj, SystemFont);
	show(obj);
    }
    return obj;
}
#endif

#if USE_NATIVE_TOGGLES
checkbox newcheckbox(const char *text, rect r, actionfn fn)
{
    checkbox obj = newchildwin("button", text, BS_CHECKBOX, r, fn);
    if (obj) {
	obj->kind = CheckboxObject;
	setbackground(obj, getbackground(parentwindow(obj)));
    }
    return obj;
}

radiobutton newradiobutton(const char *text, rect r, actionfn fn)
{
    radiobutton obj = newchildwin("button", text, BS_RADIOBUTTON, r, fn);
    if (obj) {
	obj->kind = RadioObject;
	setbackground(obj, getbackground(parentwindow(obj)));
    }
    return obj;
}
#else
static void draw_checkmark(rect r)
{
    int len;

    if (r.width < 8) {
	r = insetr(r,1);
	fillrect(r);
	return;
    }

    len = r.width/3;
    if (len < 3)
	len = 3;

    drawline(pt(r.x+1, r.y+r.height-len-2),
	     pt(r.x+len, r.y+r.height-3));
    drawline(pt(r.x+2, r.y+r.height-len-2),
	     pt(r.x+len, r.y+r.height-4));
    drawline(pt(r.x+len, r.y+r.height-3),
	     pt(r.x+r.width-1, r.y+len-2));
    drawline(pt(r.x+len, r.y+r.height-4),
	     pt(r.x+r.width-2, r.y+len-2));
}

static void draw_checkbox(control c, rect r)
{
    int w;
    rect box, textrect;
    char *name;
    int style = (AlignLeft | AlignTop);
    font f;
    rgb old = currentcolour();

    /* Calculate rectangles. */
    f = gettextfont(c);
    setfont(f);
    w = strwidth(f,"W");
    if (w > r.width)  w = r.width;
    if (w > r.height) w = r.height;
    box = rect(r.x,r.y+1,w,w);
    if (w < getheight(f) - getdescent(f))
	box.y += getheight(f) - getdescent(f) - w;
    textrect = rect(r.x+w+w/2,r.y,r.width-(w+w/2),r.height);

    /* Clear check area. */
    setlinewidth(1);
    setcolour(White);
    fillrect(insetr(box,1));

    /* Draw check area */
    if (isenabled(c))
	setcolour(Black);
    else
	setcolour(Grey);
    drawrect(box);

    /* 'Pressed button' effect by black border in box. */
    if (ishighlighted(c))
	drawrect(insetr(box,1));

    /* Put tick in box if checked. */
    if (ischecked(c))
	draw_checkmark(insetr(box,1));

    name = getname(c);
    if (isenabled(c)) {
	/* if (hasfocus(c)) {
	   style |= Underline;
	   setlinewidth(2);
	   } */
	setcolour(getforeground(c));
    }
    drawtext(textrect, style, name);

    setcolour(old);
}

checkbox newcheckbox(const char *text, rect r, actionfn fn)
{
    checkbox obj = newcontrol(text, r);
    if (obj) {
	obj->kind = CheckboxObject;
	setredraw(obj, draw_checkbox);
	setmousedown(obj, checkbox_mousedown);
	setmousemove(obj, checkbox_mousemove);
	setmousedrag(obj, checkbox_mousemove);
	setmouseup(obj, checkbox_mouseup);
	setkeydown(obj, checkbox_keydown);
	setaction(obj, fn);
	setbackground(obj, getbackground(parentwindow(obj)));
	settextfont(obj, SystemFont);
	show(obj);
    }
    return obj;
}

static void draw_radio(control c, rect r)
{
    int w;
    rect box, textrect;
    char *name;
    int style = (AlignLeft | AlignTop);
    font f;
    rgb old = currentcolour();

    /* Calculate rectangles. */
    f = gettextfont(c);
    setfont(f);
    w = strwidth(f,"W");
    if (w > r.width)  w = r.width;
    if (w > r.height) w = r.height;
    box = rect(r.x,r.y+1,w,w);
    if (w < getheight(f) - getdescent(f))
	box.y += getheight(f) - getdescent(f) - w;
    textrect = rect(r.x+w+w/2,r.y,r.width-(w+w/2),r.height);

    /* Clear the check area. */
    setlinewidth(1);
    setcolour(White);
    fillellipse(insetr(box,1));

    /* Draw the check area */
    if (isenabled(c))
	setcolour(Black);
    else
	setcolour(Grey);
    drawellipse(box);

    /* Provide 'pressed button' effect by black border. */
    if (ishighlighted(c)) {
	setlinewidth(2);
	drawellipse(box);
	setlinewidth(1);
    }

    /* Put o in circle if checked. */
    if (ischecked(c))
	fillellipse(insetr(box,3));

    name = getname(c);
    if (isenabled(c)) {
	/* if (hasfocus(c)) {
	   style |= Underline;
	   setlinewidth(2);
	   } */
	setcolour(getforeground(c));
    }
    drawtext(textrect, style, name);

    setcolour(old);
}

static void radio_hit(control c)
{
    if (!ischecked(c)) {
	uncheck_neighbours(c);
	check(c);
	activatecontrol(c);
    }
}

static void radio_mouseup(control c, int buttons, point xy)
{
    if (! isarmed(c))
	return;
    disarm(c);
    unhighlight(c);
    if (! ptinr(xy, getrect(c)))
	return;
    radio_hit(c);
}

static void radio_keydown(control c, int ch)
{
    if (ch == ' ')
	radio_hit(c);
}

radiobutton newradiobutton(const char *text, rect r, actionfn fn)
{
    radiobutton obj = newcontrol(text, r);
    if (obj) {
	obj->kind = RadioObject;
	setredraw(obj, draw_radio);
	setmousedown(obj, checkbox_mousedown);
	setmousemove(obj, checkbox_mousemove);
	setmousedrag(obj, checkbox_mousemove);
	setmouseup(obj, radio_mouseup);
	setkeydown(obj, radio_keydown);
	setaction(obj, fn);
	setbackground(obj, getbackground(parentwindow(obj)));
	settextfont(obj, SystemFont);
	show(obj);
    }
    return obj;
}
#endif

void undotext(control obj)  /* Why was this previously commented out? CJ */
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, EM_UNDO, 0, 0L);
}

void cuttext(control obj)
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, WM_CUT, 0, 0L);
}

void copytext(control obj)
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, WM_COPY, 0, 0L);
}

void cleartext(control obj)
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, WM_CLEAR, 0, 0L);
}

void pastetext(control obj)
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sendmessage(obj->handle, WM_PASTE, 0, 0L);
}

void inserttext(control obj, const char *text)
{
    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    text = to_dos_string(text);
    sendmessage(obj->handle, EM_REPLACESEL, 0, (intptr_t) text);
    if (text)
	discard(text);
}

void selecttext(control obj, long start, long end)
{
    int left, right;
    long length;

    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    length = GetWindowTextLength(obj->handle);
    left = (start < 0) ? length : start;
    right = (end < 0) ? length : end;
#ifdef WIN32
    sendmessage(obj->handle, EM_SETSEL, left,right);
#else
    sendmessage(obj->handle, EM_SETSEL, 0, MAKELONG(left,right));
#endif
}

void textselection(control obj, long *start, long *end)
{
    unsigned long sel;

    if (! obj)
	return;
    if ((obj->kind != FieldObject) && (obj->kind != TextboxObject))
	return;
    sel = sendmessage(obj->handle, EM_GETSEL, 0, 0);
    if (start) *start = LOWORD(sel);
    if (end) *end = HIWORD(sel);
}


field newfield(const char *text, rect r)
{
    field obj = newchildwin("edit", NULL,
			    WS_BORDER | ES_LEFT | ES_AUTOHSCROLL,
			    r, NULL);
    if (obj) {
	obj->kind = FieldObject;
	settext(obj, text);
    }
    return obj;
}


field newfield_no_border(const char *text, rect r)
{
    field obj = newchildwin("edit", NULL,
			    ES_LEFT | ES_AUTOHSCROLL,
			    r, NULL);
    if (obj) {
	obj->kind = FieldObject;
	settext(obj, text);
    }
    return obj;
}

field newpassword(const char *text, rect r)
{
    field obj = newchildwin("edit", NULL,
			    WS_BORDER | ES_LEFT | ES_AUTOHSCROLL
			    | ES_PASSWORD, r, NULL);
    if (obj) {
	obj->kind = FieldObject;
	settextfont(obj, SystemFont);
	settext(obj, text);
    }
    return obj;
}

textbox newtextbox(const char *text, rect r)
{
    textbox obj = newchildwin("edit", NULL,
			      /* WS_HSCROLL | ES_AUTOHSCROLL | */
			      WS_VSCROLL | ES_AUTOVSCROLL |
			      WS_BORDER | ES_LEFT |
			      ES_MULTILINE,
			      r, NULL);
    if (obj) {
	obj->kind = TextboxObject;
	settext(obj, text);
    }
    return obj;
}

textbox newtextarea(const char *text, rect r)
{
    textbox obj = newchildwin("edit", NULL,
			      WS_HSCROLL | ES_AUTOHSCROLL |
			      WS_VSCROLL | ES_AUTOVSCROLL |
			      WS_BORDER | ES_LEFT |
			      ES_MULTILINE,
			      r, NULL);
    if (obj) {
	obj->kind = TextboxObject;
	settext(obj, text);
    }
    return obj;
}

textbox newrichtextarea(const char *text, rect r)
{
    textbox obj;
    if (!LoadLibrary("riched20.dll")) /* RichEdit version 2.0, not included in Win95 */
	LoadLibrary("riched32.dll");  /* RichEdit version 1.0 */
    obj = newchildwin(RICHEDIT_CLASS, NULL,
		      WS_HSCROLL | ES_AUTOHSCROLL |
		      WS_VSCROLL | ES_AUTOVSCROLL |
		      ES_LEFT | ES_MULTILINE | ES_NOHIDESEL,
		      r, NULL);
    if (obj) {
	sendmessage(obj->handle, EM_SETTEXTMODE, TM_PLAINTEXT, 0);
	obj->kind = TextboxObject;
	settext(obj, text);
    }
    return obj;
}

static SCROLLINFO si;

scrollbar newscrollbar(rect r, int max, int pagesize, scrollfn fn)
{
    scrollbar obj;
    HWND hwnd;

    r = rcanon(r);

    obj = newchildwin("scrollbar", NULL,
		      (r.width > r.height) ? SBS_HORZ : SBS_VERT,
		      r, NULL);
    if (obj) {
	obj->kind = ScrollbarObject;
	obj->hit = fn;
	obj->value = 0;
	obj->max = max;
	obj->size = pagesize;

	hwnd = obj->handle;
	si.cbSize = sizeof(si);
	si.fMask = SIF_ALL;
	si.nMin = 0;
	si.nMax = max + pagesize - 1;
	si.nPage = pagesize;
	si.nPos = 0;
	SetScrollInfo(hwnd, SB_CTL, &si, 1);
    }
    return obj;
}

void changescrollbar(scrollbar obj, int where, int max, int pagesize)
{
    HWND hwnd;

    if (! obj)
	return;
    hwnd = obj->handle;
    obj->max = max;
    obj->size = pagesize;

    si.cbSize = sizeof(si);
    si.fMask = SIF_ALL;
    si.nMin = 0;
    si.nMax = max + pagesize - 1;
    si.nPage = pagesize;
    si.nPos = where;
    SetScrollInfo(hwnd, SB_CTL, &si, 1);
}

listbox newlistbox(const char *list[], rect r, scrollfn fn, actionfn dble)
{
    listbox obj;

    obj = newchildwin("listbox", NULL,
		      LBS_NOTIFY | WS_BORDER |
		      WS_VSCROLL | WS_HSCROLL,
		      r, NULL);
    if (! obj)
	return obj;
    obj->kind = ListboxObject;
    obj->hit = fn;
    obj->dble = dble;

    changelistbox(obj, list);

    return obj;
}

listbox newmultilist(const char *list[], rect r, scrollfn fn, actionfn dble)
{
    listbox obj;

    obj = newchildwin("listbox", NULL,
		      LBS_NOTIFY |
		      LBS_MULTIPLESEL | LBS_EXTENDEDSEL |
		      WS_BORDER |
		      WS_VSCROLL | WS_HSCROLL,
		      r, NULL);
    if (! obj)
	return obj;
    obj->kind = MultilistObject;
    obj->hit = fn;
    obj->dble = dble;

    changelistbox(obj, list);

    return obj;
}

listbox newdroplist(const char *list[], rect r, scrollfn fn)
{
    listbox obj;
    int h, i;

    initapp(0,0);
    h = getheight(SystemFont);
    r.height = h+h;
    for (i = 0; list && list[i]; i++)
	r.height += h;

    obj = newchildwin("combobox", NULL,
		      CBS_DROPDOWNLIST | CBS_AUTOHSCROLL |
		      //CBS_DISABLENOSCROLL |
		      WS_BORDER |
		      WS_VSCROLL | WS_HSCROLL,
		      r, NULL);
    if (! obj)
	return obj;
    obj->kind = DroplistObject;
    obj->hit = fn;

    changelistbox(obj, list);

    return obj;
}

listbox newdropfield(const char *list[], rect r, scrollfn fn)
{
    listbox obj;
    int h, i;

    initapp(0,0);
    h = getheight(SystemFont);
    r.height = h+h;
    for (i = 0; list && list[i]; i++)
	r.height += h;

    obj = newchildwin("combobox", NULL,
		      CBS_DROPDOWN | CBS_AUTOHSCROLL |
		      // CBS_DISABLENOSCROLL |
		      WS_BORDER |
		      WS_VSCROLL | WS_HSCROLL,
		      r, NULL);
    if (! obj)
	return obj;
    obj->kind = DropfieldObject;
    obj->hit = fn;

    changelistbox(obj, list);

    return obj;
}

void setlistitem(listbox obj, int index)
{
    int count;

    if (! obj)
	return;
    if (index < 0)
	index = -1;
    switch (obj->kind)
    {
    case ListboxObject:
	sendmessage(obj->handle, LB_SETCURSEL, index, 0L);
	break;
    case MultilistObject:
	if (index >= 0)
	    sendmessage(obj->handle, LB_SETSEL, TRUE, MAKELPARAM(index, 0));
	else {
	    count = sendmessage(obj->handle, LB_GETCOUNT, 0, 0L);
	    sendmessage(obj->handle, LB_SELITEMRANGE, FALSE, MAKELPARAM(0,count-1));
	}
    case DroplistObject:
    case DropfieldObject:
	sendmessage(obj->handle, CB_SETCURSEL, index, 0L);
	break;
    default:
	break;
    }
}

int isselected(listbox obj, int index)
{
    if (! obj)
	return -1;
    switch (obj->kind)
    {
    case ListboxObject:
	return (index == sendmessage(obj->handle, LB_GETCURSEL, 0, 0L));
    case MultilistObject:
	return sendmessage(obj->handle, LB_GETSEL, index, 0L);
    case DroplistObject:
    case DropfieldObject:
	return (index == sendmessage(obj->handle, CB_GETCURSEL, 0, 0L));
    default:
	return 0;
    }
}

int getlistitem(listbox obj)
{
    int index, count;

    if (! obj)
	return -1;
    switch (obj->kind)
    {
    case ListboxObject:
	return sendmessage(obj->handle, LB_GETCURSEL, 0, 0L);
    case MultilistObject:
	count = sendmessage(obj->handle, LB_GETCOUNT, 0, 0L);
	for (index=0; index < count; index++)
	    if (isselected(obj, index))
		return index;
	return -1;
    case DroplistObject:
    case DropfieldObject:
	return sendmessage(obj->handle, CB_GETCURSEL, 0, 0L);
    default:
	return -1;
    }
}

void changelistbox(listbox obj, const char **list)
{
    int i;
    HWND hwnd;
    UINT reset_msg, add_msg;

    if (! obj)
	return;
    hwnd = obj->handle;

    switch (obj->kind) {
    case ListboxObject: case MultilistObject:
	reset_msg = LB_RESETCONTENT;
	add_msg = LB_ADDSTRING;
	break;
    case DroplistObject: case DropfieldObject:
	reset_msg = CB_RESETCONTENT;
	add_msg = CB_ADDSTRING;
	break;
    default:
	return;
    }

    sendmessage(hwnd, WM_SETREDRAW, FALSE, 0L);
    sendmessage(hwnd, reset_msg, 0, 0L);
    for (i=0; list && list[i]; i++)
	sendmessage(hwnd, add_msg, 0, (LPSTR) list[i]);
    sendmessage(hwnd, WM_SETREDRAW, TRUE, 0L);
    if (obj->kind == ListboxObject)
	sendmessage(hwnd, LB_SETCURSEL, 0, 0L);
}

/*
 *  Activate a control's action function. We do several things here.
 *  Checking to see what kind of control it is, we handle some
 *  events and discard others. We automatically toggle the state of
 *  checkboxes and radio buttons, and handle listbox changes and
 *  allow text box update events to call the control's action.
 */
PROTECTED
void handle_control(HWND hwnd, UINT message)
{
    object obj;
    int index;

    obj = find_by_handle(hwnd);

    if ((! obj) || (! (obj->state & GA_Enabled)))
	return;

    /* Only let certain events cause activation. */
    switch (obj->kind)
    {
    case CheckboxObject:
	if (obj->state & GA_Checked)
	    uncheck(obj);
	else
	    check(obj);
	break;

    case RadioObject:
	if (!(obj->state & GA_Checked)) {
	    uncheck_neighbours(obj);
	    check(obj);
	}
	break;

    case ListboxObject:
	if (message == LBN_DBLCLK) {
	    if(obj->dble) obj->dble(obj);
	    return;
	}
	/* Ignore all but selection-change events. */
	if (message != LBN_SELCHANGE) return;

	index = sendmessage(hwnd, LB_GETCURSEL, 0, 0L);
	obj->value = index;
	break;

    case MultilistObject:
	if (message == LBN_DBLCLK) {
	    if(obj->dble) obj->dble(obj);
	    return;
	}
	/* Ignore all but selection-change events. */
	if (message != LBN_SELCHANGE)
	    return;
	index = sendmessage(hwnd, LB_GETCARETINDEX, 0, 0L);
	/* We do want to see de-selection events too
	   if (! sendmessage(hwnd, LB_GETSEL, index, 0L))
	   return;*/
	obj->value = index;
	break;

    case DroplistObject:
    case DropfieldObject:
	/* Ignore all but selection-change events. */
	if (message != CBN_SELCHANGE)
	    return;
	index = sendmessage(hwnd, CB_GETCURSEL, 0, 0L);
	obj->value = index;
	break;

    case FieldObject:
    case TextboxObject:
	if (message == EN_MAXTEXT) {
	    /* increase the character limit in the editor by 50%
	       if the limit is reached, but this should not
	       happen */
	    setlimittext(obj, 1.5 * getlimittext(obj));
	}
	/* Ignore everything else but killfocus. */
	else if (message != EN_KILLFOCUS)
	    return;
	break;

    default:
	break;
    }
    /* activate the control's callback */
    activatecontrol(obj);
}

#include <commctrl.h>
/* smooth  != 0 gives continuous not segmented bar */
progressbar newprogressbar(rect r, int pbmin, int pbmax, int incr, int smooth)
{
    HWND hwnd;
    progressbar obj;
    int sm;

    ensure_window();
    r = rcanon(r);
    sm = smooth ? PBS_SMOOTH : 0 ;
    hwnd = CreateWindowEx(0, PROGRESS_CLASS, NULL,
			  (WS_CHILD | WS_VISIBLE | sm),
			  r.x, r.y, r.width, r.height,
			  current_window->handle,
			  (HMENU) child_id, this_instance, NULL);
    obj = new_object(ControlObject, hwnd, current_window);
    if (! obj) {
	DestroyWindow(hwnd);
	return NULL;
    }
    obj->die = private_delcontrol;
    obj->rect = r;
    obj->id = child_id++;
    obj->action = NULL;
    obj->state = (GA_Visible | GA_Enabled);
    obj->flags = ChildWindow;
    set_new_winproc(obj); /* set custom winproc */
    settextfont(obj, SystemFont);
    obj->kind = ListboxObject;
    SendMessage(hwnd, PBM_SETRANGE32, (WPARAM) pbmin, (LPARAM) pbmax);
    SendMessage(hwnd, PBM_SETSTEP, (WPARAM) incr, 0);

    return obj;
}

void setprogressbar(progressbar obj, int n)
{
    if (! obj) return;
    SendMessage(obj->handle, PBM_SETPOS, (WPARAM) n, 0);
}

void stepprogressbar(progressbar obj, int n)
{
    if (! obj) return;
    SendMessage(obj->handle, PBM_STEPIT, 0, 0);
}

void setprogressbarrange(progressbar obj, int pbmin, int pbmax)
{
    if (! obj) return;
    SendMessage(obj->handle, PBM_SETRANGE32, (WPARAM) pbmin,
		(LPARAM) pbmax);
}
