/*
 *
 *  R : A Computer Language for Statistical Data Analysis
 *  file winalloc.c
 *  Copyright (C) 1999  Guido Masarotto
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
/*
   Win32 Api version of malloc and friends

   This file is an add-on  to GraphApp, a cross-platform C graphics library.
 */

#include <winbase.h>
#include "ga.h"

void *winmalloc(size_t size)
{
    void *u = NULL;
    HGLOBAL h = GlobalAlloc(GMEM_MOVEABLE | GMEM_DISCARDABLE, (DWORD)size);
    if (h) u = (void *)GlobalLock(h);
    return u;
}

void winfree(void *u)
{
    HGLOBAL h;
    if (u && (h = GlobalHandle((LPCVOID) u))) {
	GlobalUnlock(h);
	GlobalFree(h);
    }
}

void *winrealloc(void *u, size_t newsize)
{
    HGLOBAL hold, hnew;
    void *nu = NULL;
    if (!u) return winmalloc(newsize);
    if (!(hold = GlobalHandle((LPCVOID) u))) return NULL;
    hnew = GlobalReAlloc(hold, (DWORD) newsize, GMEM_MOVEABLE);
    if (hnew) nu = (void *) GlobalLock(hnew);
    return nu;
}

char *winstrdup(char *s)
{
    char *new = winmalloc(strlen(s) + 1);
    if (new) strcpy(new, s);
    return new;
}

#ifdef DANGER

void *malloc(size_t size)
{
    return winmalloc(size);
}

void free(void *u)
{
    winfree(u);
}

void *realloc(void *u, size_t newsize)
{
    return winrealloc(u, newsize);
}

void *calloc(size_t n, size_t m)
{
    size_t size = n * m;
    void   *u   = winmalloc(size);
    if (u) ZeroMemory( (PVOID) u, (DWORD) size);
    return u; 
}
#endif

