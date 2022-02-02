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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*
   bitmap -> image conversion
   Very easy to write: routines here are the graphapp
   image to image conversion routines with assignement
   changed.
 */

#include "ga.h"

/*
 *  Try to generate an 8-bit version.
 *  If there are less than 256 unique colours
 *  this routine will return the corresponding
 *  indexed 8-bit image.
 *  Returns NULL if more than 256 colours are found.
 */
static image copy2image8 (drawing dw)
{
    image new_img;
    long   x, y, w, h, j;
    GAbyte * pixel8;
    int    cmapsize;
    int low, high, mid;
    rgb    col;
    rgb    cmap[256];
    point p;
    w = getwidth(dw);
    h = getheight(dw);
    /* the first colour goes into the cmap automatically: */
    cmapsize = 0;  mid = 0;

    for (y = 0; y < h; y++) {
	p.y = y;
	for (x = 0; x < w; x++) {
	    p.x = x;
	    col = ggetpixel(dw, p);
	    /* only allow one transparent colour in the cmap: */
	    if (col & 0xF0000000UL)
		col  = 0xFFFFFFFFUL;	/* transparent */
	    else
		col &= 0x00FFFFFFUL;	/* opaque */
	    /* binary search the cmap: */
	    low = 0;  high = cmapsize - 1;
	    while (low <= high) {
		mid = (low + high)/2;
		if      (col < cmap[mid]) high = mid - 1;
		else if (col > cmap[mid]) low  = mid + 1;
		else break;
	    }

	    if (high < low) {
		/* didn't find colour in cmap, insert it: */
		if (cmapsize >= 256)
		    return NULL;
		for (j = cmapsize; j > low; j--)
		    cmap[j] = cmap[j-1];
		cmap[low] = col;
		cmapsize ++;
	    }
	}
    }

    /* now create the 8-bit indexed image: */

    new_img = newimage(w, h, 8);
    if (! new_img)
	return new_img;
    setpalette(new_img, cmapsize, cmap);
    /* now convert each 32-bit pixel into an 8-bit pixel: */

    pixel8 = (GAbyte *) new_img->pixels;

    for (y = 0; y < h; y++) {
	p.y = y;
	for (x = 0; x < w; x++) {
	    p.x =  x;
	    col = ggetpixel(dw, p);
	    /* only allow one transparent colour in the cmap: */
	    if (col & 0xF0000000UL)
		col  = 0xFFFFFFFFUL;	/* transparent */
	    else
		col &= 0x00FFFFFFUL;	/* opaque */
	    /* binary search the cmap (the colour must be there): */
	    low = 0;  high = cmapsize - 1;
	    while (low <= high) {
		mid = (low + high)/2;
		if      (col < cmap[mid]) high = mid - 1;
		else if (col > cmap[mid]) low  = mid + 1;
		else break;
	    }

	    if (high < low) {
		/* impossible situation */
		delimage(new_img);
		return NULL;
	    }
	    *(pixel8++) = mid;
	}
    }
    return new_img;
}


/*
 *  Try to generate a 32-bit version.
 *  Return NULL if there is no memory left.
 */
static image copy2image32 (drawing dw)
{
    image new_img;
    rgb *pixel32;
    long  x, y, w, h;
    point p;
    w = getwidth(dw);
    h = getheight(dw);
    new_img = newimage(w, h, 32);
    if (! new_img)
	return new_img;

    pixel32 = (rgb *) new_img->pixels;
    for (y = 0; y < h; y++) {
	p.y = y;
	for (x = 0; x < w; x++) {
	    p.x = x;
	    *(pixel32++) = ggetpixel(dw, p);
	}
    }
    return new_img;
}



/*
 *  Try to generate an image (with depth 8 or 32)  from drawing dw:
 *  Return NULL on failure.
 */
image bitmaptoimage (drawing dw)
{
    image new_img;
    rect r = ggetcliprect(dw);

    gsetcliprect(dw, getrect(dw));
    new_img = copy2image8(dw);
    if (!new_img)
	new_img = copy2image32(dw);
    gsetcliprect(dw, r);
    return new_img;
}
