/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--1999  Guido Masarotto
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 *
 * file: clipboard.c --
 * Guido Masarotto (December, 1998)
 *
 */

/*
   This file is an add-on  to GraphApp, a cross-platform C graphics library.
 */

#include "internal.h"
#include "rui.h"


void copytoclipboard(drawing sb)
{
    HBITMAP hbmpOldDest, hbmpNew;
    HDC     hdcSrc, hdcDest;
    rect r;

    r = getrect(sb);
    hdcSrc =  get_context((object)sb);
    hdcDest = CreateCompatibleDC(hdcSrc);

    hbmpNew = CreateCompatibleBitmap(hdcSrc, r.width, r.height);
    hbmpOldDest = SelectObject(hdcDest, hbmpNew);
    BitBlt(hdcDest, 0, 0, r.width, r.height, hdcSrc, 0, 0, SRCCOPY);
    SelectObject(hdcDest, hbmpOldDest);
    DeleteDC(hdcDest);

    if (!OpenClipboard(NULL) || !EmptyClipboard()) {
	R_ShowMessage("Unable to open the clipboard");
	DeleteObject(hbmpNew);
	return;
    }
    SetClipboardData(CF_BITMAP, hbmpNew);
    CloseClipboard();
    return;
}
