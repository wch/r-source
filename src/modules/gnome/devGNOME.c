/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2002   Lyndon Drake
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* Avoid braced-groups warning from -Wall */
#define G_STMT_START do
#define G_STMT_END   while(0)

#include <gnome.h>
#include <math.h>

#include "Defn.h"
#include "Graphics.h"
#include "Rdevices.h"
#include "devGNOME.h"
#include "terminal.h"
#include "terminal-functions.h"

#define CURSOR		GDK_CROSSHAIR		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

/* routines from here */

/* Device driver actions */
static void GNOME_Activate(NewDevDesc *dd);
static void GNOME_Circle(double x, double y, double r,
		       int col, int fill, double gamma, int lty, double lwd,
		       NewDevDesc *dd);
static void GNOME_Clip(double x0, double x1, double y0, double y1, 
		     NewDevDesc *dd);
static void GNOME_Close(NewDevDesc *dd);
static void GNOME_Deactivate(NewDevDesc *dd);
static void GNOME_Hold(NewDevDesc *dd);
static Rboolean GNOME_Locator(double *x, double *y, NewDevDesc *dd);
static void GNOME_Line(double x1, double y1, double x2, double y2,
		     int col, double gamma, int lty, double lwd,
		     NewDevDesc *dd);
static void GNOME_MetricInfo(int c, int font, double cex, double ps,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd);
static void GNOME_Mode(int mode, NewDevDesc *dd);
static void GNOME_NewPage(int fill, double gamma, NewDevDesc *dd);
static void GNOME_Polygon(int n, double *x, double *y, 
			int col, int fill, double gamma, int lty, double lwd,
			NewDevDesc *dd);
static void GNOME_Polyline(int n, double *x, double *y, 
			    int col, double gamma, int lty, double lwd,
			    NewDevDesc *dd);
static void GNOME_Rect(double x0, double y0, double x1, double y1,
		     int col, int fill, double gamma, int lty, double lwd,
		     NewDevDesc *dd);
static void GNOME_Resize(NewDevDesc *dd);
static void GNOME_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd);
static double GNOME_StrWidth(char *str, int font,
			      double cex, double ps, NewDevDesc *dd);
static void GNOME_Text(double x, double y, char *str, 
		     double rot, double hadj, 
		     int col, double gamma, int font, double cex, double ps,
		     NewDevDesc *dd);
static Rboolean GNOME_Open(NewDevDesc*, gnomeDesc*, char*, double, double);

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

static int RDeviceNumber(NewDevDesc *dd)
{
    return devNumber((DevDesc*) dd) + 1;
}

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

static void SetFont(NewDevDesc *dd, gint face, gint size)
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

/* line type 
   fixme: not currently used 
static void SetLineType(NewDevDesc *dd, int newlty, int newlwd)
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

	  // set dashes
	  gdk_gc_set_dashes(gd->wgc, 0, dashlist, i);
	  gdk_gc_set_line_attributes(gd->wgc, newlwd,
				     GDK_LINE_ON_OFF_DASH,
				     GDK_CAP_BUTT,
				     GDK_JOIN_ROUND);
	}
    }
}
*/

/* signal functions */

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event,
			    gpointer data) 
{
  NewDevDesc *dd;
  gnomeDesc *gd;

  dd = (NewDevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  gd = (gnomeDesc *) dd->deviceSpecific;
  g_return_val_if_fail(gd != NULL, FALSE);
  g_return_val_if_fail(gd->canvas != NULL, FALSE);
  g_return_val_if_fail(GNOME_IS_CANVAS(gd->canvas), FALSE);

  if((gd->windowWidth != gd->canvas->allocation.width) ||
     (gd->windowHeight != gd->canvas->allocation.height))
    {
      gd->windowWidth = gd->canvas->allocation.width;
      gd->windowHeight = gd->canvas->allocation.height;
      gd->resize = TRUE;
    }

  return FALSE;
}

static gint expose_event(GtkWidget *widget, GdkEventExpose *event,
			 gpointer data)
{
  NewDevDesc *dd;
  gnomeDesc *gd;

  dd = (NewDevDesc *) data;
  g_return_val_if_fail(dd, FALSE);
  gd = (gnomeDesc *) dd->deviceSpecific;

  if (gd->resize)
    {
      GNOME_Resize(dd); 
    }
  
  GEplayDisplayList((GEDevDesc*) GetDevice(devNumber((DevDesc*)dd)));

  return FALSE;
}

static gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) 
{
    NewDevDesc *dd;
    gchar *cmd;

    dd = (NewDevDesc *) data;
    g_return_val_if_fail(dd != NULL, FALSE);

    cmd = g_strdup_printf("dev.off(%d)\n", RDeviceNumber(dd));
    R_gtk_terminal_run(cmd);
    g_free(cmd);

    return TRUE;
}

static void toolbar_activate_cb(GtkWidget *widget, gpointer data) 
{
    NewDevDesc *dd;
    gchar *cmd;

    g_return_if_fail(data);
    dd = (NewDevDesc *) data;

    cmd = g_strdup_printf("dev.set(%d)\n", RDeviceNumber(dd));
    R_gtk_terminal_run(cmd);
    g_free(cmd);
}

static void toolbar_close_cb(GtkWidget *widget, gpointer data) 
{
    NewDevDesc *dd;
    gchar *cmd;

    g_return_if_fail(data);

    dd = (NewDevDesc *) data;
    cmd = g_strdup_printf("dev.off(%d)\n", RDeviceNumber(dd));
    R_gtk_terminal_run(cmd);
    g_free(cmd);
}

static void
save_ok (GtkWidget *ok_button, gpointer data)
{
    NewDevDesc *dd;
    GtkFileSelection *fsel = GTK_FILE_SELECTION(data);
    char *filename = gtk_file_selection_get_filename(fsel);
    char *cmd;

    if (!filename)
	return;
    
    dd = gtk_object_get_user_data(GTK_OBJECT(fsel));
    cmd = g_strdup_printf("dev.set(%d)\ndev.print(file=\"%s\")\n",
			  RDeviceNumber(dd), filename);
    R_gtk_terminal_run(cmd);
    
    g_free(cmd);
    gtk_widget_destroy (GTK_WIDGET(fsel));

}


static void toolbar_save_as_cb(GtkWidget *widget, gpointer data)
{
    NewDevDesc *dd = (NewDevDesc*) data;
    gnomeDesc *gd = (gnomeDesc*) dd->deviceSpecific;

    GtkFileSelection *fsel = 
	GTK_FILE_SELECTION(gtk_file_selection_new("Save as PostScript"));

    gtk_object_set_user_data(GTK_OBJECT(fsel), dd);
    gtk_file_selection_set_filename(fsel, "Rplots.ps");
  
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
    NewDevDesc *dd = (NewDevDesc*) data;
    gchar *cmd;

    cmd = g_strdup_printf("dev.set(%d)\ndev.print()",
			  RDeviceNumber(dd));
    R_gtk_terminal_run(cmd);
    g_free(cmd);
}

static GnomeUIInfo graphics_toolbar[] =
{
    { GNOME_APP_UI_ITEM, "Activate", "Make this window the current device",
      (gpointer) toolbar_activate_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK,
      GNOME_STOCK_PIXMAP_JUMP_TO, 0, (GdkModifierType) 0, NULL },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM, "Save As", "Save as a PostScript file", 
      (gpointer) toolbar_save_as_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SAVE_AS, 0, 
      (GdkModifierType) 0, NULL },
    { GNOME_APP_UI_ITEM, "Print", "Print graphics", 
      (gpointer) toolbar_print_cb, NULL, NULL, 
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PRINT, 0, 
      (GdkModifierType) 0, NULL },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM, "Close", "Close this graphics device",
      (gpointer) toolbar_close_cb, NULL, NULL,
      GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE, 0, 
      (GdkModifierType) 0, NULL },
    GNOMEUIINFO_END
};

static void GNOME_NewPage(int fill, double gamma, NewDevDesc *dd)
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
			   NULL);

  if (R_OPAQUE(fill))
    {
      gnome_canvas_item_set(gd->plotarea,
			    "fill_color_rgba", Color_RGBA(fill),
			    NULL);
    }
  else
    {
      gnome_canvas_item_set(gd->plotarea,
			    "fill_color", "white",
			    NULL);
    }
}

/* create window etc */
static Rboolean
GNOME_Open(NewDevDesc *dd, gnomeDesc *gd, char *dsp, double w, double h)
{
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
    /* FIXME: We should have a canvas color argument */
    /* gamma is defaulted to 1 here */
    GNOME_NewPage(R_RGB(255,255,255), 1, dd);

    /* place and realize the canvas */
    gnome_app_set_contents(GNOME_APP(gd->window), gd->canvas);
    gtk_widget_realize(gd->canvas);

    /* same as XSynch() */
    gdk_flush();

    /* set the cursor */
    gd->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
    gdk_window_set_cursor(gd->canvas->window, gd->gcursor);
    
    /* connect to delete signal handler, etc */
    gtk_signal_connect(GTK_OBJECT(gd->window), "configure_event",
		       (GtkSignalFunc) configure_event, (gpointer) dd);
    gtk_signal_connect(GTK_OBJECT(gd->canvas), "expose_event",
		       (GtkSignalFunc) expose_event, (gpointer) dd);
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

static double GNOME_StrWidth(char *str, int font,
			      double cex, double ps, NewDevDesc *dd)
{
  int size;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  size = cex * ps + 0.5;
  SetFont(dd, font, size);

  return (double) gdk_string_width(gd->font, str);
}

static void GNOME_MetricInfo(int c, int font, double cex, double ps,
			      double* ascent, double* descent,
			      double* width, NewDevDesc *dd)
{
  gint size;
  gint lbearing, rbearing, iascent, idescent, iwidth;
  gint maxwidth;
  gchar tmp[2];
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  size = cex * ps + 0.5;
  SetFont(dd, font, size);

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

static void GNOME_Resize(NewDevDesc *dd)
{
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

  if (gd->resize) {
    dd->left = 0.0;
    dd->right =  gd->windowWidth;
    dd->bottom = gd->windowHeight;
    dd->top = 0.0;
    gd->resize = FALSE;

    gtk_widget_set_usize(gd->canvas, gd->windowWidth, gd->windowHeight);
    gnome_canvas_set_scroll_region(GNOME_CANVAS(gd->canvas), 0, 0,
				   gd->windowWidth, gd->windowHeight);
    gnome_canvas_item_set(gd->plotarea,
			  "x2", (double) gd->windowWidth,
			  "y2", (double) gd->windowHeight,
			  NULL);
  }
}

static void GNOME_Clip(double x0, double x1, double y0, double y1, NewDevDesc *dd)
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

static void GNOME_Size(double *left, double *right,
		     double *bottom, double *top,
		     NewDevDesc *dd)
{
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

    *left = 0.0;
    *right =  gd->windowWidth;
    *bottom = gd->windowHeight;
    *top = 0.0;
}


/* kill off the window etc */
static void GNOME_Close(NewDevDesc *dd)
{
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;

    gtk_widget_destroy(gd->window);

    free(gd);
}

#define title_text_inactive "R graphics device %d"
#define title_text_active "R graphics device %d - Active"

static void GNOME_Activate(NewDevDesc *dd)
{
    gnomeDesc *gd;
    gint devnum, numdigits;
    gchar *title_text;
    gint text_len;

    text_len = strlen(title_text_active);

    gd = (gnomeDesc *) dd->deviceSpecific;

    g_return_if_fail(gd != NULL);

    devnum = devNumber((DevDesc*)dd);
    devnum++;
    numdigits = floor(R_Log10(devnum)) + 1;
    title_text = g_malloc(text_len + numdigits + 1);

    g_return_if_fail(title_text != NULL);

    g_snprintf(title_text, text_len + numdigits + 1, title_text_active, devnum);
    *(title_text + text_len + numdigits) = '\0';

    gtk_window_set_title(GTK_WINDOW(gd->window), title_text);

    g_free(title_text);
}

static void GNOME_Deactivate(NewDevDesc *dd)
{
    gnomeDesc *gd;
    gint devnum, numdigits;
    gchar *title_text;
    gint text_len;

    text_len = strlen(title_text_inactive);

    gd = (gnomeDesc *) dd->deviceSpecific;

    g_return_if_fail(gd != NULL);

    devnum = devNumber((DevDesc*)dd);
    devnum++;
    numdigits = floor(R_Log10(devnum)) + 1;
    title_text = g_malloc(text_len + numdigits + 1);

    g_return_if_fail(title_text != NULL);

    g_snprintf(title_text, text_len + numdigits + 1, title_text_inactive, devnum);
    *(title_text + text_len + numdigits) = '\0';

    gtk_window_set_title(GTK_WINDOW(gd->window), title_text);

    g_free(title_text);
}

/* drawing stuff */

static void GNOME_Rect(double x0, double y0, double x1, double y1,
		     int col, int fill, double gamma, int lty, double lwd,
		     NewDevDesc *dd)
{
    double tmp;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

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
				 "width_units", (double) lwd,
				 NULL);

    if(fill != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(fill),
			      NULL);
    }
    if(col != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(col),
			      NULL);
    }
}

static void GNOME_Circle(double x, double y, double r,
		       int col, int fill, double gamma, int lty, double lwd,
		       NewDevDesc *dd)
{
    double x1, y1, x2, y2;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

    x1 = x - r;  y1 = y - r;
    x2 = x + r;  y2 = y + r;

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_ellipse_get_type(),
				 "x1", x1,
				 "y1", y1,
				 "x2", x2,
				 "y2", y2,
				 "width_units", (double) lwd,
				 NULL);

    if(fill != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(fill),
			      NULL);
    }
    if(col != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(col),
			      NULL);
    }
}

static void GNOME_Line(double x1, double y1, double x2, double y2,
		     int col, double gamma, int lty, double lwd,
		     NewDevDesc *dd)
{
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

    points = gnome_canvas_points_new(2);
    points->coords[0] = x1;
    points->coords[1] = y1;
    points->coords[2] = x2;
    points->coords[3] = y2;

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_line_get_type(),
				 "points", points,
				 "width_units", (double) lwd,
				 NULL);

    if(col != NA_INTEGER) {
	if(lty != 0) {
	    /*SetLineType(GNOME_CANVAS_LINE(item)->gc, lty, lwd);*/
	    gnome_canvas_item_set(item,
				  "line_style", GDK_LINE_ON_OFF_DASH,
				  NULL);
	}
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(col),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Polyline(int n, double *x, double *y, 
			    int col, double gamma, int lty, double lwd,
			    NewDevDesc *dd)
{
    int i;
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

    points = gnome_canvas_points_new(n);

    for(i = 0; i < n; i++) {
	points->coords[i * 2] = x[i];
	points->coords[i * 2 + 1] = y[i];
    }

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_line_get_type(),
				 "points", points,
				 "width_units", (double) lwd,
				 NULL);

    if(col != NA_INTEGER) {
	if(lty != 0) {
	  /*
	    SetLineType(&stipple, lty, lwd);
	    gnome_canvas_item_set(item,
				  "fill_stipple", &stipple,
				  NULL);
	    */
	    gnome_canvas_item_set(item,
				  "line_style", GDK_LINE_ON_OFF_DASH,
				  NULL);
	}
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(col),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Polygon(int n, double *x, double *y, 
			int col, int fill, double gamma, int lty, double lwd,
			NewDevDesc *dd)
{
    int i;
    GnomeCanvasPoints *points;
    gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
    GnomeCanvasItem *item;

    points = gnome_canvas_points_new(n);

    for(i = 0; i < n; i++) {
	points->coords[i * 2] = floor(x[i]);
	points->coords[i * 2 + 1] = floor(y[i]);
    }

    item = gnome_canvas_item_new(gd->group,
				 gnome_canvas_polygon_get_type(),
				 "points", points,
				 "width_units", (double) 2,
				 NULL);

    if(fill != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "fill_color_rgba", Color_RGBA(fill),
			      NULL);
    }
    if(col != NA_INTEGER) {
	gnome_canvas_item_set(item,
			      "outline_color_rgba", Color_RGBA(col),
			      NULL);
    }

    gnome_canvas_points_free(points);
}

static void GNOME_Text(double x, double y, char *str, 
		     double rot, double hadj, 
		     int col, double gamma, int font, double cex, double ps,
		     NewDevDesc *dd)
{
  double affine[6] = {1, 0, 0, 1, 0, 0};
  double rrot = DEG2RAD * rot;
  GnomeCanvasItem *item;
  gnomeDesc *gd = (gnomeDesc *) dd->deviceSpecific;
  int size;

  size = cex * ps + 0.5;
  SetFont(dd, font, size);
  
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
				"fill_color_rgba", Color_RGBA(col),
				NULL);

  if (rot != 0)
    gnome_canvas_item_affine_relative (item, affine);
}

/* locator */

static gint locator_cb (GtkWidget * canvas, GdkEventButton * event,
			gpointer data)
{
  GdkEventButton *info = (GdkEventButton*) data;
  
  *info = *event;
  gtk_main_quit();

  return FALSE;
}

static Rboolean GNOME_Locator(double *x, double *y, NewDevDesc *dd)
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

static void GNOME_Mode(gint mode, NewDevDesc *dd)
{
}

static void GNOME_Hold(NewDevDesc *dd)
{
}


/* Device driver entry point */
Rboolean GnomeDeviceDriver(DevDesc *odd, char *display, 
			   double width, double height, double pointsize)
{
    int ps;
    gnomeDesc *gd;
    NewDevDesc *dd;

    if(!(gd = (gnomeDesc *) malloc(sizeof(gnomeDesc))))
	return FALSE;

    dd = (NewDevDesc*) odd;  
    dd->deviceSpecific = (void *) gd;

    /* FIXME: font loading */
    ps = pointsize;
    if(ps < 6 || ps > 24) ps = 12;
    ps = 2 * (ps / 2);
    gd->fontface = -1;
    gd->fontsize = -1;
    
    dd->startfont = 1; 
    dd->startps = ps;
    dd->startcol = 0;
    dd->startfill = NA_INTEGER;
    dd->startlty = LTY_SOLID; 
    dd->startgamma = 1;

    /* device driver start */
    if(!GNOME_Open(dd, gd, display, width, height)) {
	free(gd);
	return FALSE;
    }

    dd->newDevStruct = 1;

    /* setup data structure */
    dd->open = GNOME_Open;
    dd->close = GNOME_Close;
    dd->activate = GNOME_Activate;
    dd->deactivate = GNOME_Deactivate;
    dd->size = GNOME_Size;
    dd->newPage = GNOME_NewPage;
    dd->clip = GNOME_Clip;
    dd->strWidth = GNOME_StrWidth;
    dd->text = GNOME_Text;
    dd->rect = GNOME_Rect;
    dd->circle = GNOME_Circle;
    dd->line = GNOME_Line;
    dd->polyline = GNOME_Polyline;
    dd->polygon = GNOME_Polygon;
    dd->locator = GNOME_Locator;
    dd->mode = GNOME_Mode;
    dd->hold = GNOME_Hold;
    dd->metricInfo = GNOME_MetricInfo;

    dd->left = 0;
    dd->right = gd->windowWidth;
    dd->bottom = gd->windowHeight;
    dd->top = 0;

    /* FIXME: nominal character sizes */
    dd->cra[0] = 10;
    dd->cra[1] = 10;

    /* FIXME: character addressing offsets */
    dd->xCharOffset = 0.4900;
    dd->yCharOffset = 0.3333;
    dd->yLineBias = 0.1;

    /* inches per raster unit */
    dd->ipr[0] = pixelWidth();
    dd->ipr[1] = pixelHeight();

    /* device capabilities */
    dd->canResizePlot= TRUE;
    dd->canChangeFont= FALSE; /* FIXME: surely this is possible */
    dd->canRotateText= TRUE;
    dd->canResizeText= TRUE;
    dd->canClip = FALSE;/* FIXME: really? */
    dd->canHAdj = 0;/* not better? {0, 0.5, 1} */
    dd->canChangeGamma = FALSE;

    /* x11 device description stuff */
    gd->cex = 1.0;
    gd->srt = 0.0;
    gd->resize = FALSE;

    dd->displayListOn = TRUE;

    /* finish */
    return TRUE;
}



















