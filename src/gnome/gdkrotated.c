/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-1999   Lyndon Drake
 *                            and the R Development Core Team
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

#include "gdkrotated.h"

#include <math.h>



void gdk_draw_text_rot(GdkDrawable *drawable,
		       GdkFont *font,
		       GdkGC *gc,
		       int x, int y,
		       int maxx, int maxy,
		       const gchar *text,
		       gint text_length,
		       double angle)
{
    GdkColor black, white;
    GdkPixmap *pixmap;
    GdkGC *rotgc;
    GdkImage *image;

    int lbearing, rbearing, width, ascent, descent, height;
    int dx, dy;
    int i, j, mini, minj, maxi, maxj;

    double sintheta, costheta;

    /* sanity check */
    if((text == NULL) || (*text == '\0'))
	return;

    /* shortcut horizontal text */
    if(angle == 0.0) {
	gdk_draw_text(drawable, font, gc, x, y, text, text_length);
    }
    else {
	/* text metrics */
	gdk_text_extents(font, text, text_length,
			 &lbearing, &rbearing,
			 &width, &ascent, &descent);
	
	height = ascent + descent;
	
	/* draw text into pixmap */
	pixmap = gdk_pixmap_new(drawable, width, height, 1);
	rotgc = gdk_gc_new(pixmap);
	gdk_gc_set_font(rotgc, font);

	white.pixel = gdk_rgb_xpixel_from_rgb(0xffffffff);
	black.pixel = gdk_rgb_xpixel_from_rgb(0);

	gdk_gc_set_foreground(rotgc, &white);
	gdk_draw_rectangle (pixmap, rotgc, 1, 0, 0, width, height);

	gdk_gc_set_foreground(rotgc, &black);
	gdk_draw_text(pixmap, font, rotgc, 0, ascent, text, text_length);
	image = gdk_image_get(pixmap, 0, 0, width, height); 

	/* precalc cos/sin of angle */
	/* the floor(x * 1000.0 + 0.5) / 1000.0 is a hack to round things off */
	costheta = floor(cos(angle) * 1000.0 + 0.5) / 1000.0;
	sintheta = floor(sin(angle) * 1000.0 + 0.5) / 1000.0;

	/* calculate bounding box for i and j iteration */
	mini = maxi = floor((double)(0 - ascent) * sintheta) + x;
	minj = maxj = floor((double)(0 - ascent) * costheta) + y;

	i = floor((double)width * costheta + (double)(height - ascent) * sintheta) + x;
	j = floor(- (double)width * sintheta + (double)(height - ascent) * costheta) + y;
	if(i < mini) mini = i;
	if(i > maxi) maxi = i;
	if(j < minj) minj = j;
	if(j > maxj) maxj = j;

	i = floor((double)(height - ascent) * sintheta) + x;
	j = floor((double)(height - ascent) * costheta) + y;
	if(i < mini) mini = i;
	if(i > maxi) maxi = i;
	if(j < minj) minj = j;
	if(j > maxj) maxj = j;

	i = floor((double)width * costheta + (double)(0 - ascent) * sintheta) + x;
	j = floor(- (double)width * sintheta + (double)(0 - ascent) * costheta) + y;
	if(i < mini) mini = i;
	if(i > maxi) maxi = i;
	if(j < minj) minj = j;
	if(j > maxj) maxj = j;

	maxi++; maxj++;

	if(mini < 0) mini = 0;
	if(maxi > maxx) maxi = maxx;
	if(minj < 0) minj = 0;
	if(maxj > maxy) maxj = maxy;

	/* copy pixels */
	for(j = minj; j < maxj; j++) {
	    for(i = mini; i < maxi; i++) {
		dx = floor((double)(i - x) * costheta - (double)(j - y) * sintheta);
		dy = floor((double)(i - x) * sintheta + (double)(j - y) * costheta) + ascent;
		
		if((dx >= 0) && (dx < width) && (dy >= 0) && (dy < height) &&
		   (gdk_image_get_pixel(image, dx, dy) == black.pixel)) {
		    gdk_draw_point(drawable, gc, i, j);
		}
	    }
	}

	/* clean up */
	gdk_pixmap_unref(pixmap);
	gdk_gc_unref(rotgc);
    }
}


