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
