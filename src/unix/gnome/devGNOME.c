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

#include <gnome.h>

#include "Defn.h"
#include "Graphics.h"
#include "devGNOME.h"
#include "terminal.h"

#define CURSOR		GDK_CROSSHAIR		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

typedef struct {
    /* R Graphics Parameters */
    /* local device copy so that we can detect */
    /* when parameter changes */

    double cex;				/* Character expansion */
    double srt;				/* String rotation */

    gint bg;					/* Background */

    int fontface;				/* Typeface */
    int fontsize;				/* Size in points */

    /* GNOME Driver Specific */

    int windowWidth;			/* Window width (pixels) */
    int windowHeight;			/* Window height (pixels) */
    int resize;				/* Window resized */
    GtkWidget *window;				/* Graphics Window */
    GtkWidget *canvas;

    GdkColor *gcol_bg;

    GdkCursor *gcursor;

    int usefixed;
    GdkFont *fixedfont;
    GdkFont *font;

} gnomeDesc;


static int numGNOMEDevices = 0;



/* Device driver actions */
static void   GNOME_Activate(DevDesc *);
static void   GNOME_Circle(double, double, int, double, int, int, DevDesc*);
static void   GNOME_Clip(double, double, double, double, DevDesc*);
static void   GNOME_Close(DevDesc*);
static void   GNOME_Deactivate(DevDesc *);
static void   GNOME_Hold(DevDesc*);
static void   GNOME_Line(double, double, double, double, int, DevDesc*);
static int    GNOME_Locator(double*, double*, DevDesc*);
static void   GNOME_Mode(int);
static void   GNOME_NewPage(DevDesc*);
static int    GNOME_Open(DevDesc*, gnomeDesc*, char*, double, double);
static void   GNOME_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   GNOME_Polyline(int, double*, double*, int, DevDesc*);
static void   GNOME_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   GNOME_Resize(DevDesc*);
static double GNOME_StrWidth(char*, DevDesc*);
static void   GNOME_Text(double, double, int, char*, double, DevDesc*);
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



/* set the r, g, b, and pixel values of gcol to color */
static void SetColor(GdkColor *gcol, int color)
{
  int red, green, blue;

  red = R_RED(color);
  green = R_GREEN(color);
  blue = R_BLUE(color);
  gcol->red = red * 257;
  gcol->green = green * 257;
  gcol->blue = blue * 257;
  gcol->pixel = gdk_rgb_xpixel_from_rgb((red << 16)|(green << 8)|(blue));
}

/* line type */
static void SetLineType(GdkBitmap *stipple, int newlty, int newlwd)
{
  /*  static gchar dashlist[8];
  gint i, j;

  if(newlwd < 1)
    newlwd = 1;

  for(i = 0; (i < 8) && (newlty != 0); i++) {
    j = newlty & 15;

    if(j == 0)
      j = 1;

    j = j * newlwd;

    if(j > 255)
      j = 255;

    dashlist[i] = j;
    newlty = newlty >> 4;
  }
  */
  /*  set dashes */
  /*  gdk_gc_set_dashes(gc, 0, dashlist, i);*/
}

/* signal functions */

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data) {
  DevDesc *dd;
  gnomeDesc *gtkd;

  /*  g_print("configure\n"); */

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  gtkd = (gnomeDesc *) dd->deviceSpecific;
  g_return_val_if_fail(gtkd != NULL, FALSE);
  g_return_val_if_fail(gtkd->canvas != NULL, FALSE);
  g_return_val_if_fail(GNOME_IS_CANVAS(gtkd->canvas), FALSE);

  /* resize */
  if((gtkd->windowWidth != event->width) || (gtkd->windowHeight != event->height)) {
    gtkd->windowWidth = event->width;
    gtkd->windowHeight = event->height;

    gtkd->resize = 1;
  }

  return FALSE;
 }

static gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
  DevDesc *dd;

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  KillDevice(dd);

  return TRUE;
}

static void tb_activate_cb(GtkWidget *widget, gpointer data) {
  DevDesc *dd;

  dd = (DevDesc *) data;
  g_return_if_fail(dd != NULL);

  selectDevice(deviceNumber(dd));
}

static void tb_close_cb(GtkWidget *widget, gpointer data) {
  DevDesc *dd;

  dd = (DevDesc *) data;
  g_return_if_fail(dd != NULL);

  KillDevice(dd);
}

static GnomeUIInfo graphics_toolbar[] =
{
  { GNOME_APP_UI_ITEM, "Activate", "Make this window the current device", tb_activate_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO, 0, (GdkModifierType) 0, NULL },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "Save As", "Save as a PS file", NULL, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_SAVE_AS, 0, (GdkModifierType) 0, NULL },
  { GNOME_APP_UI_ITEM, "Print", "Print graphics", NULL, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_PRINT, 0, (GdkModifierType) 0, NULL },
  GNOMEUIINFO_SEPARATOR,
  { GNOME_APP_UI_ITEM, "Close", "Close this graphics device", tb_close_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE, 0, (GdkModifierType) 0, NULL },
  GNOMEUIINFO_END
};

/* create window etc */
static int GNOME_Open(DevDesc *dd, gnomeDesc *gtkd, char *dsp, double w, double h)
{
  GdkColor bg;
  GtkStyle *wstyle;

  gint iw, ih;

  /*gdk_rgb_set_install(TRUE); */
  gdk_rgb_set_verbose(TRUE);
  gdk_rgb_init();
  gtk_widget_push_visual(gdk_rgb_get_visual());
  gtk_widget_push_colormap(gdk_rgb_get_cmap());

  /* initialise pointers */
  gtkd->canvas = NULL;

  /* FIXME: SetBaseFont */

  /* create window etc */
  gtkd->windowWidth = iw = w / pixelWidth();
  gtkd->windowHeight = ih = h / pixelHeight();

  gtkd->window = gnome_app_new("R.gnome.graphics", "R Graphics");

  gtk_window_set_policy(GTK_WINDOW(gtkd->window), TRUE, TRUE, FALSE);
  gtk_widget_realize(gtkd->window);

  /* create toolbar */
  gnome_app_create_toolbar_with_data(GNOME_APP(gtkd->window), graphics_toolbar, (gpointer) dd);

  /* create canvas */
  gtkd->canvas = gnome_canvas_new_aa();

  /* set window background color */
  gtkd->bg = dd->dp.bg = R_RGB(255, 255, 255);
  gtkd->gcol_bg = gdk_color_copy(&bg);
  SetColor(gtkd->gcol_bg, gtkd->bg);

  wstyle = gtk_style_copy(gtk_widget_get_style(gtkd->canvas));
  wstyle->bg[GTK_STATE_NORMAL] = *(gtkd->gcol_bg);
  gtk_widget_set_style(gtkd->canvas, wstyle);

  /* canvas properties */
  gtk_widget_set_usize(gtkd->canvas, iw, ih);
  gnome_canvas_set_scroll_region(GNOME_CANVAS(gtkd->canvas), 0, 0, iw, ih);

  /* place and realize the canvas */
  gnome_app_set_contents(GNOME_APP(gtkd->window), gtkd->canvas);
  gtk_widget_realize(gtkd->canvas);

  /* same as XSynch() */
  gdk_flush();

  /* set the cursor */
  gtkd->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
  gdk_window_set_cursor(gtkd->canvas->window, gtkd->gcursor);

  /* connect to delete signal handler, etc */
  gtk_signal_connect(GTK_OBJECT(gtkd->canvas), "configure_event",
		     (GtkSignalFunc) configure_event, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(gtkd->window), "delete_event",
		     (GtkSignalFunc) delete_event, (gpointer) dd);

  /* show everything */
  gtk_widget_show_all(gtkd->window);

  /* let other widgets use the default settings */
  gtk_widget_pop_visual();
  gtk_widget_pop_colormap();

  /* we made it! */
  return 1;
}

static double GNOME_StrWidth(char *str, DevDesc *dd)
{
  return 6.0;
}

static void GNOME_MetricInfo(int c, double *ascent, double *descent, double *width, DevDesc *dd)
{
    /* metric information not available => return 0,0,0 */
    *ascent = 0.0;
    *descent = 0.0;
    *width = 0.0;
}

static void GNOME_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
}

static void GNOME_Resize(DevDesc *dd)
{
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;

  if (gtkd->resize != 0) {
    dd->dp.left = dd->gp.left = 0.0;
    dd->dp.right = dd->gp.right =  gtkd->windowWidth;
    dd->dp.bottom = dd->gp.bottom = gtkd->windowHeight;
    dd->dp.top = dd->gp.top = 0.0;
    gtkd->resize = 0;
  }
}

/* clear the drawing area */
void GNOME_NewPage_iterator(gpointer data, gpointer user_data)
{
  GList *list;
  GnomeCanvasItem *child;

  list = (GList *) data;
  child = (GnomeCanvasItem *) list->data;

  gtk_object_destroy(GTK_OBJECT(child));
}

static void GNOME_NewPage(DevDesc *dd)
{
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;

  /* reset background color */
  if(gtkd->bg != dd->dp.bg) {
    gtkd->bg = dd->dp.bg;
    SetColor(gtkd->gcol_bg, gtkd->bg);

    /* FIXME: change window style */
  }

  /* clear off items */
  /*  list = GNOME_CANVAS_GROUP(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)))->item_list;
  if(list != NULL) {
    g_list_foreach(list, GNOME_NewPage_iterator, NULL);
    g_list_free(list);
    GNOME_CANVAS_GROUP(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)))->item_list = NULL;
    GNOME_CANVAS_GROUP(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)))->item_list_end = NULL;
    }*/
}

/* kill off the window etc */
static void GNOME_Close(DevDesc *dd)
{
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;

  gtk_widget_destroy(gtkd->window);

  numGNOMEDevices--;

  free(gtkd);
}

#define title_text_inactive "R graphics device %d"
#define title_text_active "R graphics device %d - Active"

static void GNOME_Activate(DevDesc *dd)
{
  gnomeDesc *gtkd;
  gint devnum, numdigits;
  gchar *title_text;
  gint text_len;

  text_len = strlen(title_text_active);

  gtkd = (gnomeDesc *) dd->deviceSpecific;

  g_return_if_fail(gtkd != NULL);

  devnum = deviceNumber(dd);
  devnum++;
  numdigits = floor(R_Log10(devnum)) + 1;
  title_text = g_malloc(text_len + numdigits + 1);

  g_return_if_fail(title_text != NULL);

  snprintf(title_text, text_len + numdigits + 1, title_text_active, devnum);
  *(title_text + text_len + numdigits) = '\0';

  gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

  g_free(title_text);
}

static void GNOME_Deactivate(DevDesc *dd)
{
  gnomeDesc *gtkd;
  gint devnum, numdigits;
  gchar *title_text;
  gint text_len;

  text_len = strlen(title_text_inactive);

  gtkd = (gnomeDesc *) dd->deviceSpecific;

  g_return_if_fail(gtkd != NULL);

  devnum = deviceNumber(dd);
  devnum++;
  numdigits = floor(R_Log10(devnum)) + 1;
  title_text = g_malloc(text_len + numdigits + 1);

  g_return_if_fail(title_text != NULL);

  snprintf(title_text, text_len + numdigits + 1, title_text_inactive, devnum);
  *(title_text + text_len + numdigits) = '\0';

  gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

  g_free(title_text);
}

/* drawing stuff */

static void GNOME_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
  double tmp;
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
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

  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_rect_get_type(),
			       "x1", x0,
			       "y1", y0,
			       "x2", x1,
			       "y2", y1,
			       "width_pixels", (guint) dd->gp.lwd,
			       NULL);

  if(bg != NA_INTEGER) {
    SetColor(&gcol_fill, bg);
    gnome_canvas_item_set(item,
			  "fill_color_gdk", &gcol_fill,
			  NULL);
  }
  if(fg != NA_INTEGER) {
    SetColor(&gcol_outline, fg);
    gnome_canvas_item_set(item,
			  "outline_color_gdk", &gcol_outline,
			  NULL);
  }
}

static void GNOME_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
  GdkColor gcol_fill, gcol_outline;
  double x1, y1, x2, y2;
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
  GnomeCanvasItem *item;

  GConvert(&x, &y, coords, DEVICE, dd);

  x1 = x - r;  y1 = y - r;
  x2 = x + r;  y2 = y + r;

  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_ellipse_get_type(),
			       "x1", x1,
			       "y1", y1,
			       "x2", x2,
			       "y2", y2,
			       "width_pixels", (guint) dd->gp.lwd,
			       NULL);

  if(col != NA_INTEGER) {
    SetColor(&gcol_fill, col);
    gnome_canvas_item_set(item,
			  "fill_color_gdk", &gcol_fill,
			  NULL);
  }
  if(border != NA_INTEGER) {
    SetColor(&gcol_outline, border);
    gnome_canvas_item_set(item,
			  "outline_color_gdk", &gcol_outline,
			  NULL);
  }
}

static void GNOME_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
  GnomeCanvasPoints *points;
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
  GnomeCanvasItem *item;
  GdkColor gcol_fill;

  GConvert(&x1, &y1, coords, DEVICE, dd);
  GConvert(&x2, &y2, coords, DEVICE, dd);

  points = gnome_canvas_points_new(2);
  points->coords[0] = x1;
  points->coords[1] = y1;
  points->coords[2] = x2;
  points->coords[3] = y2;

  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_line_get_type(),
			       "points", points,
			       "width_pixels", (guint) dd->gp.lwd,
			       NULL);

  if(dd->gp.col != NA_INTEGER) {
    SetColor(&gcol_fill, dd->gp.col);
    if(dd->gp.lty != 0) {
      /*      SetLineType(GNOME_CANVAS_LINE(item)->gc, dd->gp.lty, dd->gp.lwd); */
      gnome_canvas_item_set(item,
			    "line_style", GDK_LINE_ON_OFF_DASH,
			    NULL);
    }
    gnome_canvas_item_set(item,
			  "fill_color_gdk", &gcol_fill,
			  NULL);
  }

  gnome_canvas_points_free(points);
}

static void GNOME_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
  double devx, devy;
  int i;
  GnomeCanvasPoints *points;
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
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

  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_line_get_type(),
			       "points", points,
			       "width_pixels", (guint) dd->gp.lwd,
			       NULL);

  if(dd->gp.col != NA_INTEGER) {
    SetColor(&gcol_fill, dd->gp.col);
    if(dd->gp.lty != 0) {
      SetLineType(&stipple, dd->gp.lty, dd->gp.lwd);
      gnome_canvas_item_set(item,
			    "fill_stipple", &stipple,
			    NULL);
    }
    gnome_canvas_item_set(item,
			  "fill_color_gdk", &gcol_fill,
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
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
  GnomeCanvasItem *item;
  GdkColor gcol_fill, gcol_outline;

  points = gnome_canvas_points_new(n);

  for(i = 0; i < n; i++) {
    devx = x[i]; devy = y[i];
    GConvert(&devx, &devy, coords, DEVICE, dd);
    points->coords[i * 2] = floor(devx);
    points->coords[i * 2 + 1] = floor(devy);
  }

  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_polygon_get_type(),
			       "points", points,
			       "width_units", (double) 2,
			       NULL);

  if(bg != NA_INTEGER) {
    SetColor(&gcol_fill, bg);
    gnome_canvas_item_set(item,
			  "fill_color_gdk", &gcol_fill,
			  NULL);
  }
  if(fg != NA_INTEGER) {
    SetColor(&gcol_outline, fg);
    gnome_canvas_item_set(item,
			  "outline_color", "black",
			  NULL);
  }

  gnome_canvas_points_free(points);
}

static void GNOME_Text(double x, double y, int coords,
		       char *str, double rot, DevDesc *dd)
{
  GnomeCanvasItem *item;
  gnomeDesc *gtkd = (gnomeDesc *) dd->deviceSpecific;
  GdkColor gcol_fill;

  GConvert(&x, &y, coords, DEVICE, dd);

  SetColor(&gcol_fill, dd->gp.col);
  item = gnome_canvas_item_new(gnome_canvas_root(GNOME_CANVAS(gtkd->canvas)),
			       gnome_canvas_text_get_type(),
			       "text", str,
			       "anchor", GTK_ANCHOR_NW,
			       "x", x,
			       "y", y,
			       /*"font", "-adobe-times-medium-r-normal-*-*-120-*-*-p-*-iso8859-1",*/
			       "font", "-*-times-medium-r-*-*-120-*-*-*-*-*-*-*",
			       "fill_color_gdk", &gcol_fill,
			       NULL);
}

/* locator */
static int GNOME_Locator(double *x, double *y, DevDesc *dd)
{
  return 0;
}

/* useless stuff */

static void GNOME_Mode(gint mode)
{
}

static void GNOME_Hold(DevDesc *dd)
{
}


/* Device driver entry point */
int GnomeDeviceDriver(DevDesc *dd, char *display, double width, double height, double pointsize)
{
  int ps;
  gnomeDesc *gtkd;

  if(!(gtkd = (gnomeDesc *) malloc(sizeof(gnomeDesc))))
    return 0;

  dd->deviceSpecific = (void *) gtkd;

  /* FIXME: font loading */
  ps = pointsize;
  if(ps < 6 || ps > 24) ps = 12;
  ps = 2 * (ps / 2);
  gtkd->fontface = -1;
  gtkd->fontsize = -1;
  dd->dp.font = 1;
  dd->dp.ps = ps;

  /* device driver start */
  if(!GNOME_Open(dd, gtkd, display, width, height)) {
    free(gtkd);
    return 0;
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
  dd->dp.right = gtkd->windowWidth;
  dd->dp.bottom = gtkd->windowHeight;
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
  dd->dp.canResizePlot = 1;
  dd->dp.canChangeFont = 0; /* FIXME: surely this is possible */
  dd->dp.canRotateText = 1;
  dd->dp.canResizeText = 1;
  dd->dp.canClip = 0;

  /* x11 device description stuff */
  gtkd->cex = 1.0;
  gtkd->srt = 0.0;
  gtkd->resize = 0;

  dd->displayListOn = 1;

  /* finish */
  return 1;
}

