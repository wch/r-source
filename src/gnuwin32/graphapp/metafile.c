/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  metafile newmetafile(char *name,window friend)
 *     return a metafile object - to draw to the metafile use drawto(...)
 *     and the drawXXX functions. if "name"=="" metafile is in memory
 *  del(metafile)  finalizes/closes the metafile. Closed in memory
 *     metafile are saved to the clipboard
*/


#include "internal.h"

/*
 *  Internal printer deletion function.
 */

static int nummeta = 0;
static HDC wHDC;

static void private_delmetafile(metafile obj)
{
    HENHMETAFILE hm;

    if (!obj || (obj->kind != MetafileObject)) return;
    hm = (HENHMETAFILE) CloseEnhMetaFile((HDC) obj->handle);
    nummeta -= 1;
    if (!nummeta) ReleaseDC(NULL,wHDC);
    if (strlen(gettext(obj))) { /* real file*/
	DeleteEnhMetaFile(hm);
	return;
    }
    if (OpenClipboard(NULL) && EmptyClipboard() && /* try to save to the*/
	SetClipboardData(CF_ENHMETAFILE, hm) &&     /*clipboard */
	CloseClipboard())
	return;
    else {
	askok("Unable to save metafile to the clipboard");
	DeleteEnhMetaFile(hm);
	return;
    }
    if (!nummeta) ReleaseDC(NULL,wHDC);
}

/*
 *  Create/return the base printer object.
 */
static object get_metafile_base(void)
{
    static object metafile_base = NULL;

    if (! metafile_base)
	metafile_base = new_object(BaseObject, 0, NULL);
    return metafile_base;
}


metafile newmetafile(char *name,rect r)
{
    float iWidthPels, iHeightPels, iMMPerPelX, iMMPerPelY;
    metafile obj;
    HDC hDC;
    RECT wr;

    wr.left = r.x;
    wr.top =  r.y;
    wr.right = r.x + r.width;
    wr.bottom = r.y + r.height;
    if (!nummeta) {
        wHDC = GetDC(NULL);
        if (!wHDC) {
	    askok("Unable to create reference DC for metafiles");
	    return NULL;
        }
    }
    hDC = CreateEnhMetaFile(wHDC, strlen(name) ? name:NULL, &wr, "GraphApp");
    if ( !hDC ) {
	askok("Unable to create metafile");
	if (!nummeta) ReleaseDC(NULL, wHDC);
	return NULL;
    }
    obj = new_object(MetafileObject, (HANDLE) hDC, get_metafile_base());
    if ( !obj ) {
	askok("Insufficient memory to create metafile");
	DeleteEnhMetaFile(CloseEnhMetaFile(hDC));
	if (!nummeta) ReleaseDC(NULL, wHDC);
	return NULL;
    }
    nummeta += 1;
    iWidthPels = GetDeviceCaps(wHDC, LOGPIXELSX);
    iHeightPels = GetDeviceCaps(wHDC, LOGPIXELSY);
    iMMPerPelX = 2540 /iWidthPels;
    iMMPerPelY = 2540 /iHeightPels;

    obj->rect.x = r.x/iMMPerPelX;
    obj->rect.width = r.width/iMMPerPelX;
    obj->rect.y = r.y/iMMPerPelY;
    obj->rect.height = r.height/iMMPerPelY;
    obj->depth=GetDeviceCaps(hDC, BITSPIXEL)* GetDeviceCaps(hDC, PLANES);
    obj->die = private_delmetafile ;
    obj->drawstate = copydrawstate();
    obj->drawstate->dest = obj;
    settext(obj,name ? name : "");
    return obj;
}
