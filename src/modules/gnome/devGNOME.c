/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2000   Lyndon Drake
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

#include <gnome.h>
#include <math.h>

#include "Defn.h"
#include "Graphics.h"
#include "Rdevices.h"
#include "devGNOME.h"
#include "terminal.h"
#include "device-support.h"

#define CURSOR		GDK_CROSSHAIR		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

typedef struct {
  /* R Graphics Parameters */
  /* local device copy so that we can detect */
  /* when parameter changes */

  double cex;				/* Character expansion */
  double srt;				/* String rotation */

  int fontface;                         /* Typeface */
  int fontsize;                         /* Size in points */

  gint lty, lwd;                        /* line params */

  /* GNOME Driver Specific */

  GdkGC *wgc;

  int windowWidth;			/* Window width (pixels) */
  int windowHeight;			/* Window height (pixels) */
  Rboolean resize;			/* Window resized */
  GtkWidget *window;			/* Graphics Window */

  GtkWidget *canvas;                    /* Gnome canvas */
  GnomeCanvasGroup *group;              /* Canvas group for plotting */
  GnomeCanvasItem *plotarea;            /* Plotting area */

  GdkCursor *gcursor;

  Rboolean usefixed;                    /* not yet used (??) */
  GdkFont *fixedfont;
  GdkFont *font;

} gnomeDesc;

/* routines from here */
Rboolean GnomeDeviceDriver(DevDesc *dd, char *display, 
			   double width, double height, double pointsize);

/* Device driver actions */
static void   GNOME_Activate(DevDesc *);
static void   GNOME_Circle(double, double, int, double, int, int, DevDesc*);
static void   GNOME_Clip(double, double, double, double, DevDesc*);
static void   GNOME_Close(DevDesc*);
static void   GNOME_Deactivate(DevDesc *);
static void   GNOME_old(DevDesc*);
static void   GNOME_Line(double, double, double, double, int, DevDesc*);
static Rboolean GNOME_Locator(double*, double*, DevDesc*);
static void   GNOME_Mode(int);
static void   GNOME_NewPage(DevDesc*);
static Rboolean GNOME_Open(DevDesc*, gnomeDesc*, char*, double, double);
static void   GNOME_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   GNOME_Polyline(int, double*, double*, int, DevDesc*);
static void   GNOME_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   GNOME_Resize(DevDesc*);
static double GNOME_StrWidth(char*, DevDesc*);
static void   GNOME_Text(double, double, int, char*, double, double, DevDesc*);
static void   GNOME_MetricInfo(int, double*, double*, double*, DevDesc*);

			/* Pixel Dimensions (Inches) */

static double pixelWidth(void)
{
    double width, widthMM;
    width = gdk_screen_width();
    widthMM = gdk_screen_width_mm();
    return ((double)widthMM / (double)width) / MM_PER_INCH;
}

static double pixelHeight(void)
{
    double height, heightMM;
    height = gdk_screen_height();
    heightMM = gdk_screen_height_mm();
    return ((double)heightMM / (double)height) / MM_PER_INCH;
}

/* font stuff */

static char *fontname_R6 = "-adobe-helvetica-%s-%s-*-*-*-%d-*-*-*-*-*-*";
static char *symbolname = "-adobe-symbol-*-*-*-*-*-%d-*-*-*-*-*-*";
static char *fixedname = "fixed";

static char *slant[] = {"r", "o"};
static char *weight[] = {"medium", "bold"};

static char *fontname = NULL;
static GHashTable *font_htab = NULL;

static GdkFont *RGTKLoadFont(char *font)
{
  GdkFont *tmp_font;
  
  g_return_val_if_fail(font_htab, NULL);
  tmp_font = g_hash_table_lookup(font_htab, (gpointer) font);

  if(!tmp_font)
    {
      if((tmp_font = gdk_font_load(font)))
	{
	  g_hash_table_insert(font_htab, (gpointer) g_strdup(font),
			      (gpointer) tmp_font);
	}
    }

  return tmp_font;
}

static gboolean SetBaseFont (gnomeDesc *gd)
{
  gd->fontface = 1;
  gd->fontsize = 12;
  gd->usefixed = 0;
  
  if (font_htab == NULL)
    {
      font_htab = g_hash_table_new(g_str_hash, g_str_equal);
    }

  fontname = g_strdup_printf(fontname_R6, weight[0], slant[0], 
			     gd->fontsize * 10);
  gd->font = RGTKLoadFont(fontname);
  g_free(fontname);

  if(gd->font)
    return TRUE;

  gd->usefixed = 1;
  gd->font = RGTKLoadFont(fixedname);

  if(gd->font)
    return TRUE;
  
  return FALSE;
}

#define SMALLEST 8
#define LARGEST 24

static void SetFont(DevDesc *dd, gint face, gint size)
{
  GdkFont *tmp_font;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  if (face < 1 || face > 5)
    face = 1;
  
  size = 2 * size / 2;
  if (size < SMALLEST)
    size = SMALLEST;
  else if (size > LARGEST)
    size = LARGEST;
  
  if (!gd->usefixed)
    {
      if (face == 5)
	fontname = g_strdup_printf(symbolname, 10 * size);
      else
	fontname = g_strdup_printf(fontname_R6,
				   weight[(face-1)%2],
				   slant[((face-1)/2)%2],
				   10 * size);
      
      tmp_font = RGTKLoadFont(fontname);
      g_free(fontname);

      if (tmp_font)
	{
	  gd->font = tmp_font;
	  gd->fontface = face;
	  gd->fontsize = size;
	}
      else
	{
	  error("gdk font at size %d could not be loaded", size);
	}
    }
}

/* Get GNOME color specification from R color  */
static guint32 Color_RGBA(int color)
{
  int red, green, blue, alpha;
  
  red = R_RED(color);
  green = R_GREEN(color);
  blue = R_BLUE(color);
  alpha = 255 - R_ALPHA(color);

  return GNOME_CANVAS_COLOR_A(red, green, blue, alpha);
}

/* line type */
/* fixme: not currently used */
static void SetLineType(DevDesc *dd, int newlty, int newlwd)
{
  static gchar dashlist[8];
  gint i, j;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
  
  if (newlty != gd->lty || newlwd != gd->lwd)
    {
      if (newlty == 0)
	{
	  if(newlwd <= 1)
	    newlwd = 0;
	  
	  gdk_gc_set_line_attributes(gd->wgc, newlwd,
				     GDK_LINE_SOLID,
				     GDK_CAP_BUTT,
				     GDK_JOIN_ROUND);
	}
      else
	{
	  if (newlwd < 1)
	    newlwd = 1;

	  for(i = 0; (i < 8) && (newlty != 0); i++) {
	    j = newlty & 15;
	    if(j == 0) j = 1;
	    j = j * newlwd;
	    if(j > 255) j = 255;
	    dashlist[i] = j;
	    newlty = newlty >> 4;
	  }

	  /*  set dashes */
	  gdk_gc_set_dashes(gd->wgc, 0, dashlist, i);
	  gdk_gc_set_line_attributes(gd->wgc, newlwd,
				     GDK_LINE_ON_OFF_DASH,
				     GDK_CAP_BUTT,
				     GDK_JOIN_ROUND);
	}
    }
}

/* signal functions */

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event,
			    gpointer data) 
{
  DevDesc *dd;
  gnomeDesc *gd;

  /*  g_print("configure\n"); */

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  gd = (gnomeDesc *) dd->deviceSpecific;
  g_return_val_if_fail(gd != NULL, FALSE);
  g_return_val_if_fail(gd->canvas != NULL, FALSE);
  g_return_val_if_fail(GNOME_IS_CANVAS(gd->canvas), FALSE);

  /* resize */
  if((gd->windowWidth != event->width) || (gd->windowHeight != event->height))
    {
      gd->windowWidth = event->width;
      gd->windowHeight = event->height;
      
      gd->resize = TRUE;
    }

  return FALSE;
}

static gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) 
{
    DevDesc *dd;

    dd = (DevDesc *) data;
    g_return_val_if_fail(dd != NULL, FALSE);

    KillDevice(dd);

    return TRUE;
}

static void toolbar_activate_cb(GtkWidget *widget, gpointer data) 
{
    DevDesc *dd;

    dd = (DevDesc *) data;
    g_return_if_fail(dd != NULL);

    selectDevice(deviceNumber(dd));
}

static void toolbar_close_cb(GtkWidget *widget, gpointer data) 
{
    DevDesc *dd;

    dd = (DevDesc *) data;
    g_return_if_fail(dd != NULL);

    KillDevice(dd);
}

static void
save_ok (GtkWidget *ok_button, gpointer data)
{
  GtkFileSelection *fsel = GTK_FILE_SELECTION(data);
  char *filename = gtk_file_selection_get_filename(fsel);
  DevDesc *dd;

  if (!filename)
    return;
  dd = gtk_object_get_user_data(GTK_OBJECT(fsel));
  SaveAsPostscript(dd, filename);
  gtk_widget_destroy (GTK_WIDGET(fsel));
}


static void toolbar_save_as_cb(GtkWidget *widget, gpointer data)
{
  DevDesc *dd = (DevDesc*) data;
  gnomeDesc *gd = (gnomeDesc*) dd->deviceSpecific;

  GtkFileSelection *fsel = 
    GTK_FILE_SELECTION(gtk_file_selection_new("Save as PostScript"));

  gtk_object_set_user_data(GTK_OBJECT(fsel), dd);

  gtk_signal_connect(GTK_OBJECT(fsel->ok_button), "clicked",
                     GTK_SIGNAL_FUNC(save_ok), fsel);
  gtk_signal_connect_object(GTK_OBJECT(fsel->cancel_button), "clicked",
                            GTK_SIGNAL_FUNC(gtk_widget_destroy),
                            GTK_OBJECT(fsel));


  gtk_window_position(GTK_WINDOW(fsel), GTK_WIN_POS_MOUSE);
  gtk_window_set_transient_for(GTK_WINDOW(fsel), GTK_WINDOW(gd->window));
  gtk_widget_show(GTK_WIDGET(fsel));

}

static void toolbar_print_cb (GtkWidget *widget, gpointer data)
{
  DevDesc *dd = (DevDesc*) data;
  SaveAsPostscript(dd, "");
}

static GnomeUIInfo graphics_toolbar[] =
{
  { GNOME_APP_UI_ITEM, "Activate", "Make this window the current device",
    toolbar_activate_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK,
    GNOME_STOCK_PIXMAP_JUMP_TO, 0, (GdkModifierType) 0, NULL },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "Save As", "Save as a PostScript file", 
    toolbar_save_as_cb, NULL, NULL, 
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SAVE_AS, 0, 
    (GdkModifierType) 0, NULL },
  { GNOME_APP_UI_ITEM, "Print", "Print graphics", 
    toolbar_print_cb, NULL, NULL, 
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PRINT, 0, 
    (GdkModifierType) 0, NULL },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "Close", "Close this graphics device",
    toolbar_close_cb, NULL, NULL,
    GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE, 0, 
    (GdkModifierType) 0, NULL },
  GNOMEUIINFO_END
};

static void GNOME_NewPage(DevDesc *dd)
{
  GnomeCanvasItem *group;
  gnomeDesc *gd = (gnomeDesc*) dd->deviceSpecific;

  g_return_if_fail(gd->canvas);

  if (gd->group)
    {
      gtk_object_destroy(GTK_OBJECT(gd->group));
      gd->plotarea = NULL;
    }
  
  /* create group for plotting objects */
  group = gnome_canvas_item_new (gnome_canvas_root(GNOME_CANVAS(gd->canvas)),
				 gnome_canvas_group_get_type(),
				 "x", (double) 0.0,
				 "y", (double) 0.0,
				 NULL);
  gd->group = GNOME_CANVAS_GROUP(group);
  
  /* create plotting area */
  gd->plotarea = 
    gnome_canvas_item_new (gd->group,
			   gnome_canvas_rect_get_type(),
			   "x1", (double) 0.0,
			   "y1", (double) 0.0,
			   "x2", (double) gd->windowWidth,
			   "y2", (double) gd->windowHeight,
			   "fill_color_rgba", Color_RGBA(dd->dp.bg),
			   NULL);
}

/* create window etc */
static Rboolean
GNOME_Open(DevDesc *dd, gnomeDesc *gd, char *dsp, double w, double h)
{
    GnomeCanvasItem *group;

    gint iw, ih;

    /*gdk_rgb_set_install(TRUE); */
    /* gdk_rgb_set_verbose(TRUE);*/
    gdk_rgb_init();
    gtk_widget_push_visual(gdk_rgb_get_visual());
    gtk_widget_push_colormap(gdk_rgb_get_cmap());

    /* initialise pointers */
    gd->canvas = NULL;
    gd->group = NULL;
    gd->plotarea = NULL;

    if(!SetBaseFont(gd))
      {
	Rprintf("can't find X11 font\n");
	return FALSE;
      }

    /* create window etc */
    gd->windowWidth = iw = w / pixelWidth();
    gd->windowHeight = ih = h / pixelHeight();

    gd->window = gnome_app_new("R.gnome.graphics", "R Graphics");

    gtk_window_set_policy(GTK_WINDOW(gd->window), TRUE, TRUE, FALSE);
    gtk_widget_realize(gd->window);

    /* create toolbar */
    gnome_app_create_toolbar_with_data(GNOME_APP(gd->window), graphics_toolbar,
				       (gpointer) dd);

    /* create canvas */
    gd->canvas = gnome_canvas_new_aa();
    gtk_widget_set_usize(gd->canvas, iw, ih);
    gnome_canvas_set_scroll_region(GNOME_CANVAS(gd->canvas), 0, 0, iw, ih);

    /* create new plot area on canvas */
    dd->dp.bg = R_RGB(255,255,255);
    GNOME_NewPage(dd);

    /* place and realize the canvas */
    gnome_app_set_contents(GNOME_APP(gd->window), gd->canvas);
    gtk_widget_realize(gd->canvas);

    /* same as XSynch() */
    gdk_flush();

    /* set the cursor */
    gd->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
    gdk_window_set_cursor(gd->canvas->window, gd->gcursor);

    /* connect to delete signal handler, etc */
    gtk_signal_connect(GTK_OBJECT(gd->canvas), "configure_event",
		       (GtkSignalFunc) configure_event, (gpointer) dd);
    gtk_signal_connect(GTK_OBJECT(gd->window), "delete_event",
		       (GtkSignalFunc) delete_event, (gpointer) dd);

    /* show everything */
    gtk_widget_show_all(gd->window);

    /* let other widgets use the default settings */
    gtk_widget_pop_visual();
    gtk_widget_pop_colormap();

    /* we made it! */
    return TRUE;
}

static double GNOME_StrWidth(char *str, DevDesc *dd)
{
  int size;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  size = dd->gp.cex * dd->gp.ps + 0.5;
  SetFont(dd, dd->gp.font, size);

  return (double) gdk_string_width(gd->font, str);
}

static void GNOME_MetricInfo(int c, double *ascent, double *descent,
			     double *width, DevDesc *dd)
{
  gint size;
  gint lbearing, rbearing, iascent, idescent, iwidth;
  gint maxwidth;
  gchar tmp[2];
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  size = dd->gp.cex * dd->gp.ps + 0.5;
  SetFont(dd, dd->gp.font, size);

  if(c == 0) {
    maxwidth = 0;

    for(c = 0; c <= 255; c++) {
      g_snprintf(tmp, 2, "%c", (gchar) c);
      iwidth = gdk_string_width(gd->font, tmp);
      if (iwidth > maxwidth)
	maxwidth = iwidth;
    }

    *ascent = (double) gd->font->ascent;
    *descent = (double) gd->font->descent;
    *width = (double) maxwidth;
  }
  else {
    g_snprintf(tmp, 2, "%c", (gchar) c);
    gdk_string_extents(gd->font, tmp,
		       &lbearing, &rbearing,
		       &iwidth, &iascent, &idescent);

    *ascent = (double) iascent;
    *descent = (double) idescent;
    *width = (double) (lbearing+rbearing);
  }
}

static void GNOME_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
  /* 
     Clipping is theoretically possible: there is an implementation of
     it in gnome-print-preview, which has a class called clipped-group. As
     the name suggests, this is a canvas group which defines a
     clipping path for the items it contains.  The notes say that this
     will be folded into the standard GnomeCanvasGroup, so I'm waiting
     for this to happen instead of duplicating it here. MTP 6/9/2001
  */
}

static void GNOME_Resize(DevDesc *dd)
{
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

    if (gd->resize) {
	dd->dp.left = dd->gp.left = 0.0;
	dd->dp.right = dd->gp.right =  gd->windowWidth;
	dd->dp.bottom = dd->gp.bottom = gd->windowHeight;
	dd->dp.top = dd->gp.top = 0.0;
	gd->resize = FALSE;
    }
}


/* kill off the window etc */
static void GNOME_Close(DevDesc *dd)
{
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

    gtk_widget_destroy(gd->window);

    free(gd);
}

#define title_text_inactive "R graphics device %d"
#define title_text_active "R graphics device %d - Active"

static void GNOME_Activate(DevDesc *dd)
{
    gnomeDesc *gd;
    gint devnum, numdigits;
    gchar *title_text;
    gint text_len;

    text_len = strlen(title_text_active);

    gd = (gnomeDesc *) dd->deviceSpecific;

    g_return_if_fail(gd != NULL);

    devnum = deviceNumber(dd);
    devnum++;
    numdigits = floor(R_Log10(devnum)) + 1;
    title_text = g_malloc(text_len + numdigits + 1);

    g_return_if_fail(title_text != NULL);

    snprintf(title_text, text_len + numdigits + 1, title_text_active, devnum);
    *(title_text + text_len + numdigits) = '\0';

    gtk_window_set_title(GTK_WINDOW(gd->window), title_text);

    g_free(title_text);

    gtk_widget_set_sensitive(GTK_WIDGET(graphics_toolbar[0].widget),
			     FALSE);
}

static void GNOME_Deactivate(DevDesc *dd)
{
    gnomeDesc *gd;
    gint devnum, numdigits;
    gchar *title_text;
    gint text_len;
    GtkWidget *button;

    text_len = strlen(title_text_inactive);

    gd = (gnomeDesc *) dd->deviceSpecific;

    g_return_if_fail(gd != NULL);

    devnum = deviceNumber(dd);
    devnum++;
    numdigits = floor(R_Log10(devnum)) + 1;
    title_text = g_malloc(text_len + numdigits + 1);

    g_return_if_fail(title_text != NULL);

    snprintf(title_text, text_len + numdigits + 1, title_text_inactive, devnum);
    *(title_text + text_len + numdigits) = '\0';

    gtk_window_set_title(GTK_WINDOW(gd->window), title_text);

    g_free(title_text);

    button = GTK_WIDGET(graphics_toolbar[0].widget);
    gtk_widget_set_sensitive(button, TRUE);
}

/* drawing stuff */

static void GNOME_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
    double tmp;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GdkColor gcol_fill, gcol_outline;
    GnomeCanvasItem *item;

    GConvert(&x0, &y0, coords, DEVICE, dd);
    GConvert(&x1, &y1, coords, DEVICE, dd);

    if(x0 > x1) {
	tmp = x0;
	x0 = x1;
	x1 = tmp;
    }
    if(y0 > y1) {
	tmp = y0;
	y0 = y1;
	y1 = tmp;
    }

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_rect_get_type(),
				 "x1", x0,
				 "y1", y0,
				 "x2", x1,
				 "y2", y1,
				 "width_units", (double) dd->gp.lwd,
				 NULL);

    if(bg != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(bg),
			      NULL);
    }
    if(fg != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(fg),
			      NULL);
    }
}

static void GNOME_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
    GdkColor gcol_fill, gcol_outline;
    double x1, y1, x2, y2;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

    GConvert(&x, &y, coords, DEVICE, dd);

    x1 = x - r;  y1 = y - r;
    x2 = x + r;  y2 = y + r;

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_ellipse_get_type(),
				 "x1", x1,
				 "y1", y1,
				 "x2", x2,
				 "y2", y2,
				 "width_units", (double) dd->gp.lwd,
				 NULL);

    if(col != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(col),
			      NULL);
    }
    if(border != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(border),
			      NULL);
    }
}

static void GNOME_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;
    GdkColor gcol_fill;

    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);

    points = gnome_canvas_points_new(2);
    points->coords[0] = x1;
    points->coords[1] = y1;
    points->coords[2] = x2;
    points->coords[3] = y2;

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_line_get_type(),
				 "points", points,
				 "width_units", (double) dd->gp.lwd,
				 NULL);

    if(dd->gp.col != NA_INTEGER) {
	if(dd->gp.lty != 0) {
	    /*SetLineType(GNOME_CANVAS_LINE(item)->gc, dd->gp.lty, dd->gp.lwd);*/
	    gnome_canvas_item_set(item,
				  "line_style", GDK_LINE_ON_OFF_DASH,
				  NULL);
	}
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(dd->gp.col),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
    double devx, devy;
    int i;
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;
    GdkColor gcol_fill;
    GdkBitmap stipple;

    points = gnome_canvas_points_new(n);

    for(i = 0; i < n; i++) {
	devx = x[i]; devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points->coords[i * 2] = devx;
	points->coords[i * 2 + 1] = devy;
    }

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_line_get_type(),
				 "points", points,
				 "width_units", (double) dd->gp.lwd,
				 NULL);

    if(dd->gp.col != NA_INTEGER) {
	if(dd->gp.lty != 0) {
	  /*
	    SetLineType(&stipple, dd->gp.lty, dd->gp.lwd);
	    gnome_canvas_item_set(item,
				  "fill_stipple", &stipple,
				  NULL);
	    */
	    gnome_canvas_item_set(item,
				  "line_style", GDK_LINE_ON_OFF_DASH,
				  NULL);
	}
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(dd->gp.col),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
    double devx, devy;
    int i;
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;
    GdkColor gcol_fill, gcol_outline;

    points = gnome_canvas_points_new(n);

    for(i = 0; i < n; i++) {
	devx = x[i]; devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points->coords[i * 2] = floor(devx);
	points->coords[i * 2 + 1] = floor(devy);
    }

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_polygon_get_type(),
				 "points", points,
				 "width_units", (double) 2,
				 NULL);

    if(bg != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(bg),
			      NULL);
    }
    if(fg != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(fg),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Text(double x, double y, int coords,
		       char *str, double rot, double hadj, DevDesc *dd)
{
  double affine[6] = {1, 0, 0, 1, 0, 0};
  double rrot = DEG2RAD * rot;
  GnomeCanvasItem *item;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
  int size;

  GConvert(&x, &y, coords, DEVICE, dd);

  size = dd->gp.cex * dd->gp.ps + 0.5;
  SetFont(dd, dd->gp.font, size);
  
  if (rot != 0)
    {
      affine[0] = affine[3] = cos(rrot);
      affine[1] = -sin(rrot);
      affine[2] = -affine[1];
      affine[4] = x;
      affine[5] = y;
      x = y = 0.0;
    }

  item = gnome_canvas_item_new (gd->group,
				gnome_canvas_text_get_type(),
				"text", str,
				"anchor", GTK_ANCHOR_SOUTH_WEST,
				"x", x,
				"y", y,
				"font_gdk", gd->font,
				"fill_color_rgba", Color_RGBA(dd->gp.col),
				NULL);

  if (rot != 0)
    gnome_canvas_item_affine_relative (item, affine);
}

/* locator */

static gboolean locator_cb (GtkWidget * canvas, GdkEventButton * event,
			    gpointer data)
{
  GdkEventButton *info = (GdkEventButton*) data;
  
  *info = *event;
  gtk_main_quit();
}

static Rboolean GNOME_Locator(double *x, double *y, DevDesc *dd)
{
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
  GdkEventButton info;
  guint handler_id;

  /* Flush any pending events */
  while (gtk_events_pending())
    gtk_main_iteration();

  /* connect signal */
  handler_id = gtk_signal_connect (GTK_OBJECT (gd->canvas),
				   "button-press-event",
				   GTK_SIGNAL_FUNC (locator_cb), &info);

  /* run the handler */
  gtk_main();

  *x = (double) info.x;
  *y = (double) info.y;

  /* clean up */
  gtk_signal_disconnect(GTK_OBJECT(gd->canvas), handler_id);

  if (info.button == 1)
    {
      gdk_beep();
      return TRUE;
    }
  else
    return FALSE;
}

/* useless stuff */

static void GNOME_Mode(gint mode)
{
}

static void GNOME_Hold(DevDesc *dd)
{
}


/* Device driver entry point */
Rboolean GnomeDeviceDriver(DevDesc *dd, char *display, 
			   double width, double height, double pointsize)
{
    int ps;
    gnomeDesc *gd;

    if(!(gd = (gnomeDesc *) malloc(sizeof(gnomeDesc))))
	return FALSE;

    dd->deviceSpecific = (void *) gd;

    /* FIXME: font loading */
    ps = pointsize;
    if(ps < 6 || ps > 24) ps = 12;
    ps = 2 * (ps / 2);
    gd->fontface = -1;
    gd->fontsize = -1;
    dd->dp.font = 1;
    dd->dp.ps = ps;

    /* device driver start */
    if(!GNOME_Open(dd, gd, display, width, height)) {
	free(gd);
	return FALSE;
    }

    /* setup data structure */
    dd->dp.open = GNOME_Open;
    dd->dp.close = GNOME_Close;
    dd->dp.activate = GNOME_Activate;
    dd->dp.deactivate = GNOME_Deactivate;
    dd->dp.resize = GNOME_Resize;
    dd->dp.newPage = GNOME_NewPage;
    dd->dp.clip = GNOME_Clip;
    dd->dp.strWidth = GNOME_StrWidth;
    dd->dp.text = GNOME_Text;
    dd->dp.rect = GNOME_Rect;
    dd->dp.circle = GNOME_Circle;
    dd->dp.line = GNOME_Line;
    dd->dp.polyline = GNOME_Polyline;
    dd->dp.polygon = GNOME_Polygon;
    dd->dp.locator = GNOME_Locator;
    dd->dp.mode = GNOME_Mode;
    dd->dp.hold = GNOME_Hold;
    dd->dp.metricInfo = GNOME_MetricInfo;

    dd->dp.left = 0;
    dd->dp.right = gd->windowWidth;
    dd->dp.bottom = gd->windowHeight;
    dd->dp.top = 0;

    /* FIXME: nominal character sizes */
    dd->dp.cra[0] = 10;
    dd->dp.cra[1] = 10;

    /* FIXME: character addressing offsets */
    dd->dp.xCharOffset = 0.4900;
    dd->dp.yCharOffset = 0.3333;
    dd->dp.yLineBias = 0.1;

    /* inches per raster unit */
    dd->dp.ipr[0] = pixelWidth();
    dd->dp.ipr[1] = pixelHeight();

    /* device capabilities */
    dd->dp.canResizePlot= TRUE;
    dd->dp.canChangeFont= FALSE; /* FIXME: surely this is possible */
    dd->dp.canRotateText= TRUE;
    dd->dp.canResizeText= TRUE;
    dd->dp.canClip = FALSE;/* FIXME: really? */
    dd->dp.canHAdj = 0;/* not better? {0, 0.5, 1} */

 
    /* x11 device description stuff */
    gd->cex = 1.0;
    gd->srt = 0.0;
    gd->resize = FALSE;

    dd->displayListOn = TRUE;

    /* finish */
    return TRUE;
}



















