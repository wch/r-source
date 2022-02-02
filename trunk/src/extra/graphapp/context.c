/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: context.c -- internal functions for manipulating DCs.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.05  Changes: Added drawstate information.
 * Version: 1.50  Changes: New colour system, dithered grey.
 * Version: 2.00  Changes: New object class and context sub-system.
 * Version: 2.01  Changes: get_context is now more bulletproof.
 * Version: 2.02  Changes: drawto and setwinrgb have been updated.
 * Version: 2.20  Changes: Added currentrgb, currentfont etc.
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

/* Copyright (C) 2004	The R Foundation

      Changes for R:  allow 32 contexts not 4
      Remove assumption that current->dest is non-NULL
*/

#include "internal.h"

/*
 *  Private library variables.
 */

PROTECTED drawstruct app_drawstate =
{
    NULL,	/* drawing destination */
    Black,	/* colour */
    S,	/* drawing mode */
    {0,0},	/* point */
    1,	/* line width */
    NULL,	/* font */
    NULL,	/* cursor */
};

PROTECTED drawstate current = & app_drawstate;
PROTECTED HDC       dc;	/* shared DC variable */
PROTECTED HPEN      the_pen   = 0;
PROTECTED HBRUSH    the_brush = 0;

PROTECTED COLORREF win_rgb  = 0L;

/*
 *  Private drawing context variables.
 */

static	rgb	prev_pixval	= Black;
static	int	prev_width	= 1;

static	bitmap	grey_bitmap	= NULL;

static rgb grey_cmap [] = {
    0x00000000UL,
    0x00FFFFFFUL,
};
static GAbyte grey_pixels [] = {
    0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 1, 0, 1, 0, 1, 0,
    0, 1, 0, 1, 0, 1, 0, 1,
    1, 0, 1, 0, 1, 0, 1, 0,
};
static imagedata grey_imagedata = {
    8,	/* depth */
    8,	/* width */
    8,	/* height */
    2,	/* cmapsize */
    grey_cmap,
    grey_pixels
};
static image grey_image = & grey_imagedata;

/*
 *  Private context list structure
 */
typedef struct contextinfo  contextinfo;

struct contextinfo
{
    object	obj;	/* back pointer to drawing object */
    HDC	dc;	/* handle to DC used when drawing */
    HGDIOBJ old_bitmap; /* bitmap returned by SelectObject() */
};

/*
 *  The constant MAX_CONTEXTS controls how large the
 *  circular array of contexts is, and thus how many
 *  concurrently active contexts there can be.  This
 *  number should be at least 2,  since bitblt needs
 *  two contexts for source and destination bitmaps.
 */
#define MAX_CONTEXTS 32

static contextinfo context[MAX_CONTEXTS];
static int num_contexts = 0;
static int empty_slot = 0;

/*
 *  Create a pen and a brush for use in drawing.
 */
static void create_pen_and_brush(rgb c, unsigned long winrgb,
		int width, int depth)
{
    the_pen = CreatePen(PS_INSIDEFRAME, width, winrgb);

    if ((depth == 1) && (c == Grey))
	the_brush = CreatePatternBrush(grey_bitmap->handle);
    else
	the_brush = CreateSolidBrush(winrgb);
}

/*
 *  Delete any created pens and brushes.
 *  Assumes they are not selected into any valid DC.
 */
static void delete_pen_and_brush(void)
{
    DeleteObject(the_pen);
    DeleteObject(the_brush);
}

/*
 *  Set Windows drawing globals up.
 */
PROTECTED
void init_contexts(void)
{
    grey_bitmap = imagetobitmap(grey_image);
    grey_bitmap->text = new_string("grey_bitmap");

    current = & app_drawstate;
    app_drawstate.fnt = SystemFont;
    app_drawstate.crsr = ArrowCursor;

    create_pen_and_brush(Black, Black, 1, 0);
}

static void free_context(contextinfo *c)
{
    if (! c->dc)
	return; /* already been deleted */

    SelectObject(c->dc, GetStockObject(NULL_PEN));
    SelectObject(c->dc, GetStockObject(NULL_BRUSH));

    if (c->obj->kind & ControlObject)
	ReleaseDC(c->obj->handle, c->dc);
    else if (c->obj->kind == BitmapObject) {
	SelectObject(c->dc, c->old_bitmap);
	DeleteDC(c->dc);
    }

    if (dc == c->dc)
	dc = 0;

    c->dc = 0;
    c->obj = NULL;
    c->old_bitmap = 0;
}

/*
 *  Add a new DC into our list of DCs.  This is kind of ugly:  num_contexts just keeps growing, unless
 *  del_all_contexts is called, when it is set to 0.  The free ones will be scattered through the list.
 */
PROTECTED
void add_context(object obj, HDC dc, HGDIOBJ old)
{
    contextinfo *c;

    /* Search for or clear a spot for the new DC if the array is full. */
    if (num_contexts == MAX_CONTEXTS) {
        int i = empty_slot;
        while ( context[i].dc ) {
             i = (i+1) % MAX_CONTEXTS;
             if (i == empty_slot) {
		 free_context(&context[i]);
		 break;
	     }
	}
        c = & context[i];
	empty_slot = (i+1) % MAX_CONTEXTS;
    } else {
	c = & context[num_contexts];
	num_contexts++;
    }
    /* Add this context to the list. */
    c->obj = obj;
    c->dc = dc;
    c->old_bitmap = old;
}

/*
 *  Find and return a DC for a window or a bitmap.
 *  Also keep track of which DCs have been created.
 */
PROTECTED
HDC get_context(object obj)
{
    int i;
    HDC dc;
    HGDIOBJ old;

    /* Determine if a DC for this object already exists. */
    for (i = 0; i < num_contexts; i++) {
	if (context[i].obj == obj)
	    return context[i].dc;
    }

    /* Use GetDC or CreateCompatibleDC to return a DC. */
    if (obj->kind & ControlObject) {
	dc = GetDC(obj->handle);
	add_context(obj, dc, 0);
    }
    else if (obj->kind == BitmapObject) {
	dc = CreateCompatibleDC(0);
	old = SelectObject(dc, obj->handle);
	add_context(obj, dc, old);
    }
    else {
	return NULL;
	/* apperror("Cannot find DC for non-drawable object."); */
    }

    /* We only select in the brush or pen we need, when we
     * need it. Thus at all other times, they are NULL.
     * This ensures we can delete objects when we want, and
     * also makes correct context possible. */
    SelectObject(dc, GetStockObject(NULL_PEN));
    SelectObject(dc, GetStockObject(NULL_BRUSH));

    return dc;
}

/*
 *  Remove the context from the list by blanking its fields.
 *  It is assumed that the DC has previously been released,
 *  for example by EndPaint() in the events.c file.
 */
PROTECTED
void remove_context(object obj)
{
    int i;
    contextinfo *c;

    for (i = 0; i < num_contexts; i++) {
	c = & context[i];
	if (c->obj == obj) {
	    c->obj = NULL;
	    c->dc = 0;
	    c->old_bitmap = 0;
	    /* If we delete the last one, reduce the count:  avoid 
	       a search next time. */
	    if (i == num_contexts - 1)
	    	num_contexts--;
	    else
	    	empty_slot = i;
	}
    }
}

/*
 *  Free the DC associated with a given object.
 */
PROTECTED
void del_context(object obj)
{
    int i;
    contextinfo *c;

    for (i = 0; i < num_contexts; i++) {
	c = & context[i];
	if (c->obj == obj) {
	    free_context(c);
	    c->obj = NULL;
	    c->dc = 0;
	    c->old_bitmap = 0;
	    /* If we delete the last one, reduce the count:  avoid 
	       a search next time. */
	    if (i == num_contexts - 1)
	    	num_contexts--;
	    else
	    	empty_slot = i;

	}
    }
}

/*
 *  Get rid of all DCs.
 */
PROTECTED
void del_all_contexts(void)
{
    int i;
    contextinfo *c;

    for (i = 0; i < num_contexts; i++) {
	c = & context[i];
	free_context(c);
	c->obj = NULL;
	c->dc = 0;
	c->old_bitmap = 0;
    }
    num_contexts = 0;
    empty_slot = 0;
}

/*
 *  De-initialise drawing variables.
 */
PROTECTED
void finish_contexts(void)
{
    del_all_contexts();
    DeleteObject(the_pen);
    DeleteObject(the_brush);
}

/*
 *  Set up a pen and a brush for use in colouring things, and return
 *  the Windows RGB value equivalent to the library rgb value.
 */
static unsigned long set_win_rgb(rgb c, int width)
{
    int r, g, b;
    long luminance;
    int depth;
    unsigned long winrgb;

    if (current->mode == Ones)
	c = White;
    else if (current->mode == Zeros)
	c = Black;

    r = (int) ((c >> 16) & 0x000000FFL);
    g = (int) ((c >>  8) & 0x000000FFL);
    b = (int) ((c >>  0) & 0x000000FFL);

    if (current->dest) depth = getdepth(current->dest);
    else depth = 2;  /* set default minimal depth if no current window */

    if (depth <= 2)	/* map to black or white, or grey if c == Grey */
    {
	luminance = (r*3 + g*5 + b) / 9;
	if (luminance > 0x0087)		r = g = b = 0x00FF;
	else if (luminance <= 0x0077)	r = g = b = 0x0000;
	else				r = g = b = 0x0080;
	c = rgb(r,g,b);
    }

    winrgb = RGB(r, g, b);

    /* Has a colour or width change occured? */
    if ((c != prev_pixval) || (width != prev_width))
    {
	prev_pixval = c;
	prev_width = width;

	/* delete any old objects */
	delete_pen_and_brush();

	/* set up new objects */
	create_pen_and_brush(c, winrgb, width, depth);
    }
    return winrgb;
}

void setcursor(cursor c)
{
    decrease_refcount(current->crsr);
    current->crsr = c;
    if (c) SetCursor((HCURSOR) c->handle);
    increase_refcount(c);
}

void setfont(font f)
{
    decrease_refcount(current->fnt);
    current->fnt = f;
    increase_refcount(f);
}

/*
 *  Set the way that source and destination pixels are combined
 *  when drawing.
 */
void setdrawmode(int mode)
{
    current->mode = mode & 0x0F; /* must be between 0x00 and 0x0F */
}

/*
 *  Set the colour.
 */
void setrgb(rgb hue)
{
    current->hue = hue;
    win_rgb = set_win_rgb(hue, current->linewidth);
}

/*
 *  Set the line width.
 */
void setlinewidth(int width)
{
    if (width < 1)
	width = 1;
    current->linewidth = width;
    win_rgb = set_win_rgb(current->hue, current->linewidth);
}

/*
 *  Set which window/menubar/menu to add new objects too.
 */
void addto(object obj)
{
    if (! obj)
	return;
    switch (obj->kind) {
    case WindowObject:	current_window = obj;	break;
    case MenubarObject:	current_menubar = obj;	break;
    case MenuObject:	current_menu = obj;	break;
    }
}

/*
 *  Set which bitmap or window to draw to; allocate a DC too.
 */
void drawto(drawing d)
{
    if (! d) {
	current = & app_drawstate;
	current->dest = NULL;
	return;
    }

    dc = get_context(d);

    if (d->drawstate) {
	/* Change the current drawing state to this one. */
	current = d->drawstate;
	current->dest = d;
	win_rgb = set_win_rgb(current->hue, current->linewidth);
    }
    else {
	/* Otherwise just use the current drawing state. */
	current->dest = d;
    }
}

/*
 *  Ensure drawing variables are set up.
 */
void setdrawstate(drawstate s)
{
    if (! s)
	return;
    moveto(s->p);
    setdrawmode(s->mode);
    setcursor(s->crsr);
    setfont(s->fnt);
    setlinewidth(s->linewidth);
    setrgb(s->hue);
}

/*
 *  Change the current drawstate to the specified one.
 */
void restoredrawstate(drawstate s)
{
    if (! s)
	return;
    setdrawstate(s);
    discard(s);
}

/*
 *  Reset drawing variables to initial values.
 */
void resetdrawstate(void)
{
    setrgb(Black);
    setlinewidth(1);
    setcursor(ArrowCursor);
    moveto(pt(0,0));
    setfont(SystemFont);
    setdrawmode(S);
}

/*
 *  Return a new copy of the current drawing state.
 */
drawstate copydrawstate(void)
{
    drawstate s = NULL;

    if (current) {
	s = create(drawstruct);
	if (s)
	    *s = *current;
    }
    return s;
}

/*
 *  Return drawing state information.
 */

drawing	currentdrawing(void)    { return current->dest; }
rgb	currentrgb(void)        { return current->hue; }
int	currentmode(void)       { return current->mode; }
point	currentpoint(void)      { return current->p; }
int	currentlinewidth(void)  { return current->linewidth; }
font	currentfont(void)       { return current->fnt; }
cursor	currentcursor(void)     { return current->crsr; }

int	getkeystate(void)       { return keystate; }
