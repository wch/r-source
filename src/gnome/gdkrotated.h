#include <gdk/gdk.h>
#include <math.h>

void gdk_draw_text_rot	 (GdkDrawable  *drawable,
			  GdkFont      *font,
			  GdkGC	       *gc,
			  gint		x,
			  gint		y,
			  const gchar  *text,
			  gint		text_length,
			  double angle);
