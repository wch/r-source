/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: fonts.c -- font selection functions.
 * Platform: Windows  Version: 2.35  Date: 1998/04/04
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: New object class system.
 * Version: 2.30  Changes: Now uses font_base.
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

/* Changes for R:

   Use default GUI font
   Do not delete FixedFont

 */

#include "internal.h"

/*
 *  Pre-defined library fonts.
 */
	font SystemFont = NULL;
	font FixedFont  = NULL;
	font Times      = NULL;
	font Helvetica  = NULL;
	font Courier    = NULL;

/*
 *  Dots per inch for the screen.
 */
	static int screen_dpix = 96;	/* logical dpi for VGA */
	static int screen_dpiy = 96;	/* logical dpi for VGA */

/*
 *  Private font destructor. Calls the Windows functions needed
 *  to free the object from GDI memory.
 */
static void private_delfont(font f)
{
	if (f && (f != SystemFont) && (f != FixedFont) && (f->handle))
			DeleteObject(f->handle);
}

/*
 *  Private object constructor.
 */
static object get_font_base(void)
{
	static object font_base = NULL;

	if (! font_base)
		font_base = new_object(BaseObject, 0, NULL);
	return font_base;
}

PROTECTED font new_font_object(HFONT hf)
{
	TEXTMETRIC tm;
	HDC dc;
	HFONT old;
	object obj;

	obj = new_object(FontObject, hf, get_font_base());
	if (! obj) {
		DeleteObject(hf);
		return NULL;
	}
	obj->die = private_delfont;

	dc = GetDC(0);
	old = SelectObject(dc, hf);
	GetTextMetrics(dc, &tm);

	obj->depth = 1;
	obj->rect.width = tm.tmAveCharWidth;
	obj->rect.height = tm.tmHeight;
	obj->rect.x = tm.tmAscent - tm.tmInternalLeading;
	obj->rect.y = tm.tmDescent;

	SelectObject(dc, old);
	ReleaseDC(0, dc);

	return (font) obj;
}

rect getSysFontSize()
{
    return  SystemFont->rect;
}


/*
 *  Private font initialisation function.
 */
PROTECTED
void init_fonts(void)
{
	HDC info;
	NONCLIENTMETRICS ncm;

	/* get system information */
	info = CreateIC("DISPLAY", NULL, NULL, NULL);
	screen_dpix = GetDeviceCaps(info, LOGPIXELSX);
	screen_dpiy = GetDeviceCaps(info, LOGPIXELSY);
	DeleteDC(info);

	/* set up standard fonts */
	/* Claim that this was wrong:
	   http://blogs.msdn.com/oldnewthing/archive/2005/07/07/436435.aspx

	   SystemFont = new_font_object(GetStockObject(DEFAULT_GUI_FONT));
	*/
	ncm.cbSize = sizeof(NONCLIENTMETRICS);
	SystemParametersInfo(SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0);
	SystemFont = new_font_object(CreateFontIndirect(&ncm.lfMenuFont));
	if (SystemFont) SystemFont->text = new_string("SystemFont");

	FixedFont = new_font_object(GetStockObject(OEM_FIXED_FONT));
	Times = newfont("Times New Roman", Plain, -10);
	Helvetica = newfont("Arial", SansSerif, -10);
	Courier = newfont("Courier New", FixedWidth, -10);

	protect_object(SystemFont);
	protect_object(FixedFont);
	protect_object(Times);
	protect_object(Helvetica);
	protect_object(Courier);
}

/*
 *  Load a font by name.
 */
font newfont(const char *name, int style, int size)
{
	font obj;
	HFONT hf;
	LOGFONT lf;

	initapp(0,NULL);

	/* This next calculation should convert from point size to
	   pixels.  We use this since we always use the MM_TEXT mode,
	   which is in pixels. */
	/* Windows requires the lfHeight field must be negative
	   to specify point size, positive to specify pixel size. */

	if (size < 0) /* negative size indicates this is a point size */
		lf.lfHeight = ((screen_dpiy * size)/72);
	else /* positive size indicates a pixel height for the font */
		lf.lfHeight = size;

	lf.lfWidth = lf.lfEscapement = lf.lfOrientation = 0;
	lf.lfWeight = FW_NORMAL;
	lf.lfItalic = lf.lfUnderline = lf.lfStrikeOut = 0;
	lf.lfCharSet = ANSI_CHARSET;
	if ((! string_diff(name, "Symbol"))
		|| (! string_diff(name, "Wingdings")))
			lf.lfCharSet = SYMBOL_CHARSET;
	lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
	lf.lfQuality = DEFAULT_QUALITY;
	lf.lfPitchAndFamily = DEFAULT_PITCH | FF_DONTCARE;
	if ((name != 0) && (*name != '\0'))
		strncpy(lf.lfFaceName, name, LF_FACESIZE-1);

	if (style & Italic)
		lf.lfItalic = 1;
	if (style & Bold)
		lf.lfWeight = FW_BOLD;
	if (style & FixedWidth)
		lf.lfPitchAndFamily |= FIXED_PITCH;
	if (style & SansSerif)
		lf.lfPitchAndFamily |= FF_SWISS;

	if ((hf = CreateFontIndirect(&lf)) == 0)
		return NULL;

	obj = new_font_object(hf);
	if (obj)
		obj->text = new_string(name);

	return (font) obj;
}

/*
 *  Discover the sizes of a font.
 */
int fontwidth(font obj)   { return obj->rect.width; }
int fontheight(font obj)  { return obj->rect.height; }
int fontascent(font obj)  { return obj->rect.x; }
int fontdescent(font obj) { return obj->rect.y; }
