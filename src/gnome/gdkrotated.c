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

extern double deg2rad;

void gdk_draw_text_rot	 (GdkDrawable  *drawable,
			  GdkFont      *font,
			  GdkGC	       *gc,
			  gint		x,
			  gint		y,
			  const gchar  *text,
			  gint		text_length,
			  double        angle)
{
  GdkGC my_gc;
  double sin_angle, cos_angle;

  if(text == NULL || *text == '\0')
    return;

  while(angle < 0)
    angle += 360;

  while(angle >= 360)
    angle -= 360;

  angle *= deg2rad;

  if(angle == 0) {
    gdk_draw_text(drawable, font, gc, x, y, text, text_length);
    return;
  }

  gdk_gc_copy(&my_gc, gc);

  /*  sin_angle = round(sin(angle) * 1000.0) / 1000.0;
      cos_angle = round(cos(angle) * 1000.0) / 1000.0;*/
} 
