/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: cursors.c -- change the mouse cursor's shape.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 1.05  Changes: Moved setcursor() into context.c
 * Version: 2.00  Changes: New object class mechanism.
 * Version: 2.30  Changes: Now uses cursor_base.
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

/* Changes for Windows:

   add cross cursor

 */

#define __MAIN__
#include "internal.h"
#undef __MAIN__

__declspec(dllexport) cursor	ArrowCursor = NULL;
__declspec(dllexport) cursor	BlankCursor = NULL;
__declspec(dllexport) cursor	WatchCursor = NULL;
__declspec(dllexport) cursor	CaretCursor = NULL;
__declspec(dllexport) cursor	TextCursor  = NULL;
__declspec(dllexport) cursor	HandCursor  = NULL;
__declspec(dllexport) cursor	CrossCursor  = NULL;

/*
 *  Define the 'Hand' image shape:
 */

static rgb cursor_cmap [] = {
    0x00000000L,
    0x00FFFFFFL,
    0xFFFFFFFFL,
};

static byte hand_pixels [] = {
    2,2,2,2,2,2,0,0,0,2,2,2,2,2,2,2,
    2,2,2,2,2,0,1,1,0,0,2,2,2,2,2,2,
    2,2,2,2,2,0,1,1,0,0,2,2,2,2,2,2,
    2,2,2,2,2,0,1,1,0,0,2,2,2,2,2,2,
    2,2,2,2,2,0,1,1,0,0,2,2,2,2,2,2,
    2,2,2,2,2,0,1,1,0,0,2,2,2,2,2,2,
    2,0,0,0,2,0,1,1,0,0,0,0,0,2,2,2,
    0,1,1,0,0,0,1,1,0,1,0,1,0,0,0,2,
    2,0,1,1,0,0,1,1,0,1,0,1,0,1,0,0,
    2,2,0,1,1,0,1,1,1,1,1,1,0,1,0,0,
    2,2,0,1,1,0,1,1,1,1,1,1,1,1,0,0,
    2,2,2,0,1,1,1,1,1,1,1,1,1,1,0,0,
    2,2,2,2,0,1,1,1,1,1,1,1,1,1,0,0,
    2,2,2,2,0,1,1,1,1,1,1,1,1,0,0,2,
    2,2,2,2,2,0,1,1,1,1,1,1,1,0,0,2,
    2,2,2,2,2,0,1,1,1,1,1,1,1,0,0,2,
};

static imagedata hand_imagedata = {
    8,	/* depth */
    16,	/* width */
    16,	/* height */
    3,	/* cmapsize */
    cursor_cmap,
    hand_pixels
};

static image hand_image = & hand_imagedata;
static point hand_hotspot = {7,0};

/*
 *  Private cursor destructor.
 */
static void private_delcursor(cursor c)
{
    DestroyCursor(c->handle);
}

/*
 *  Private constructor.
 */
static object get_cursor_base(void)
{
    static object cursor_base = NULL;

    if (! cursor_base)
	cursor_base = new_object(BaseObject, 0, NULL);
    return cursor_base;
}

static cursor new_cursor_object(HCURSOR hc)
{
    cursor c = new_object(CursorObject, hc, get_cursor_base());
    if (c) {
	c->rect = rect(0,0,16,16);
	c->depth = 1;
	c->die = private_delcursor;
    }
    return c;
}

/*
 *  Public constructors.
 */
cursor createcursor(point offset, byte *white, byte *black)
{
    cursor c;
    HCURSOR hc;
    byte *andmask;	/* andmask = ~blackmask & ~whitemask */
    byte *xormask;	/* xormask = ~blackmask &  whitemask */
    int w, y;
    int max_width, max_height, row_bytes;

    /* Library cursors: D = (D | whitemask) & ~blackmask */
    /* MS-Windows does: D = (D & andmask) ^ xormask */

    /* Determine the best cursor size: */
    max_width  = GetSystemMetrics(SM_CXCURSOR);
    max_height = GetSystemMetrics(SM_CYCURSOR);
    row_bytes  = (max_width + 7) / 8;

    /* Create the data arrays: */
    andmask = array(max_height * row_bytes, byte);
    xormask = array(max_height * row_bytes, byte);

    /* Assign the data into the arrays: */
    for (y=0; y < max_height; y++) {
	for (w=0; w < row_bytes; w++) {
	    if ((w<2) && (y<16)) {
		andmask[w+y*row_bytes] = ~(black[w+y*2]) & ~(white[w+y*2]);
		xormask[w+y*row_bytes] = ~(black[w+y*2]) &  (white[w+y*2]);
	    }
	    else {
		andmask[w+y*row_bytes] = 0xFF;
		xormask[w+y*row_bytes] = 0x00;
	    }
	}
    }

    /* Create the cursor: */
    hc = CreateCursor (this_instance, -offset.x, -offset.y,
		       max_width, max_height,
		       (void FAR *) andmask, (void FAR *) xormask);

    c = new_cursor_object(hc);

    /* Clean up: */
    discard(andmask);
    discard(xormask);

    return c;
}

static byte form_and_byte(image img, int x, int y)
{
    int i;
    rgb pixel;
    byte result = 0x00;

    for (i=0; i<8; i++) {
	result <<= 1;
	pixel = get_monochrome_pixel(img, x+i, y);
	if (pixel == Transparent)
	    result |= 0x01;
    }
    return result;
}

static byte form_xor_byte(image img, int x, int y)
{
    int i;
    rgb pixel;
    byte result = 0x00;

    for (i=0; i<8; i++) {
	result <<= 1;
	pixel = get_monochrome_pixel(img, x+i, y);
	if (pixel == White)
	    result |= 0x01;
    }
    return result;
}

cursor newcursor(point p, image img)
{
    cursor c;
    HCURSOR hc;
    byte *andmask;
    byte *xormask;
    int w, y;
    int max_width, max_height, row_bytes;

    if (! img) return NULL;

    /* Determine the best cursor size: */
    max_width  = GetSystemMetrics(SM_CXCURSOR);
    max_height = GetSystemMetrics(SM_CYCURSOR);
    row_bytes  = (max_width + 7) / 8;

    /* Create the data arrays: */
    andmask = array(max_height * row_bytes, byte);
    xormask = array(max_height * row_bytes, byte);

    /* Assign the data into the arrays: */
    for (y=0; y < max_height; y++) {
	for (w=0; w < row_bytes; w++) {
	    andmask[w+y*row_bytes] = form_and_byte(img, w*8, y);
	    xormask[w+y*row_bytes] = form_xor_byte(img, w*8, y);
	}
    }

    /* Create the cursor: */
    hc = CreateCursor (this_instance, p.x, p.y,
		       max_width, max_height,
		       (void FAR *) andmask, (void FAR *) xormask);

    c = new_cursor_object(hc);

    /* Clean up: */
    discard(andmask);
    discard(xormask);

    return c;
}

#ifdef UNUSED
/*
 *  Load a cursor from resources.
 */
static point load_hotspot(const char *filename)
{
    FILE *file;
    char line[100];
    int i, x, y;
    point p = pt(0,0);

    file = fopen(filename, "rt");
    while (fgets(line, sizeof(line)-2, file)) {
	if ( (! strncmp(line, "point", 5))
	     || (! strncmp(line, "/* point", 8)) )
	{
	    i = 5;
	    x = y = 0;
	    while (line[i] && (! isdigit(line[i])) )
		i++; /* skip "hotspot = {" */
	    x = atoi(line+i);
	    while (line[i] && isdigit(line[i]))
		i++; /* skip x-location */
	    while (line[i] && (! isdigit(line[i])) )
		i++; /* skip comma */
	    y = atoi(line+i);
	    p = pt(x,y);
	    break;
	}
    }
    fclose(file);
    return p;
}

static cursor load_image_cursor(const char *filename)
{
    cursor c = NULL;
    image img;
    point p;

    img = loadimage(filename);
    if (img) {
	p = load_hotspot(filename);
	c = newcursor(p, img);
	delimage(img);
    }
    return c;
}

cursor loadcursor(const char *name)
{
    HCURSOR hc;
    cursor c;

    if (this_instance == 0)
	return ArrowCursor;

    hc = LoadCursor(this_instance, name);

    if (hc)	c = new_cursor_object(hc);
    else	c = load_image_cursor(name);

    if (c)	c->text = new_string(name);

    return c;
}
#endif

/*
 *  Private cursor initialisation routines.
 */
static cursor make_special_cursor(const char *name, LPCSTR idc)
{
    cursor c;

    if (idc) c = new_cursor_object(LoadCursor(0, idc));
    else	 c = new_cursor_object(NULL);

    if (c) {
	c->text = new_string(name);
	protect_object(c);
    }
    return c;
}

PROTECTED
void init_cursors(void)
{
    ArrowCursor = make_special_cursor("ArrowCursor", IDC_ARROW);
    BlankCursor = make_special_cursor("BlankCursor", NULL);
    WatchCursor = make_special_cursor("WatchCursor", IDC_WAIT);
    CaretCursor = make_special_cursor("CaretCursor", IDC_IBEAM);
    TextCursor  = make_special_cursor("TextCursor",  IDC_IBEAM);
    CrossCursor = make_special_cursor("CrossCursor",  IDC_CROSS);       

    HandCursor  = newcursor(hand_hotspot, hand_image);
    if (HandCursor) {
	settext(HandCursor, "HandCursor");
	protect_object(HandCursor);
    }

    setcursor(ArrowCursor);
}
