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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 *  metafile newmetafile(char *name,rect r)
 *     return a metafile object of 'nominal' size (r.width)x(r.height)
 *     in 0.01mm. Use drawto(...)/drawXXX/gdrawXXX to draw to the 
 *     metafile. If "name"=="" metafile is in memory.
 *     
 *  del(metafile)  finalizes/closes the metafile. Closed in memory
 *     metafile are saved to the clipboard.
 *
*/


#include "internal.h"
#include "rui.h"

/*
 *  Internal metafile deletion function.
 */

static void private_delmetafile(metafile obj)
{
    HENHMETAFILE hm;

    if (!obj || (obj->kind != MetafileObject)) return;
    hm = (HENHMETAFILE) CloseEnhMetaFile((HDC) obj->handle);
    if (strlen(gettext(obj))) { /* real file*/
	DeleteEnhMetaFile(hm);
	return;
    }
    if (OpenClipboard(NULL) && EmptyClipboard() && /* try to save to the*/
	SetClipboardData(CF_ENHMETAFILE, hm) &&     /*clipboard */
	CloseClipboard())
	return;
    else {
	R_ShowMessage("Unable to save metafile to the clipboard");
	DeleteEnhMetaFile(hm);
	return;
    }
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
    metafile obj;
    HDC hDC;
    RECT wr;
    static double cppix=-1, ppix, cppiy, ppiy;

    /* 
     * In theory, (cppix=ppix) and (cppiy=ppiy). However, we
     * use the ratio to adjust the 'reference dimension'
     * in case.... ("Importing graph in MsWord" thread)
    */
    if (cppix<0) {
       cppix = 25.40 * devicewidth(NULL) / devicewidthmm(NULL);
       ppix  = 100 * devicepixelsx(NULL);
       cppiy = 25.40 * deviceheight(NULL) / deviceheightmm(NULL);
       ppiy = 100 * devicepixelsy(NULL);
    }

    wr.left = 0;
    wr.top =  0 ;
    wr.right =  (ppix * r.width) / cppix ;
    wr.bottom = (ppiy * r.height) / cppiy ;

    hDC = CreateEnhMetaFile(NULL, strlen(name) ? name : NULL, &wr, "GraphApp");
    if ( !hDC ) {
	R_ShowMessage("Unable to create metafile");
	return NULL;
    }
    obj = new_object(MetafileObject, (HANDLE) hDC, get_metafile_base());
    if ( !obj ) {
	R_ShowMessage("Insufficient memory to create metafile");
	DeleteEnhMetaFile(CloseEnhMetaFile(hDC));
	return NULL;
    }
    obj->rect = rect(0, 0, (ppix * r.width)/2540, (ppiy * r.height)/2540);
    obj->depth=GetDeviceCaps(hDC, BITSPIXEL)* GetDeviceCaps(hDC, PLANES);
    obj->die = private_delmetafile ;
    obj->drawstate = copydrawstate();
    obj->drawstate->dest = obj;
    settext(obj,name ? name : "");
    return obj;
}







