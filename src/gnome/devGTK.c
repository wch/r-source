
#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include "pixmaps/close.xpm"
#include "pixmaps/jump_to.xpm"
#include "pixmaps/print.xpm"
#include "pixmaps/save-as.xpm"

#include "Defn.h"
#include "Graphics.h"
#include "devGTK.h"
//#include "rotated.h"

#define CURSOR		GDK_CROSSHAIR		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

typedef struct {
    /* R Graphics Parameters */
    /* local device copy so that we can detect */
    /* when parameter changes */

    double cex;				/* Character expansion */
    double srt;				/* String rotation */
    int lty;				/* Line type */
    double lwd;
    gint col;				/* Color */
    gint fg;					/* Foreground */
    gint bg;					/* Background */
    int fontface;				/* Typeface */
    int fontsize;				/* Size in points */

    /* GTK Driver Specific */
    /* parameters with copy per x11 device */

    int windowWidth;			/* Window width (pixels) */
    int windowHeight;			/* Window height (pixels) */
    int resize;				/* Window resized */
    GtkWidget *window;				/* Graphics Window */
    GtkWidget *vbox;
    GtkWidget *handlebox;
    GtkWidget *toolbar;
    GtkWidget *drawingarea;
    GdkPixmap *pixmap;

    GdkGC *wgc;
    GdkColorContext *cc;
    GdkCursor *gcursor;
    GdkColor fgcolor;
    GdkColor bgcolor;
    GdkRectangle clip;
  
  //    XSetWindowAttributes attributes;	/* Window attributes */

    int usefixed;
    GdkFont *fixedfont;
    GdkFont *font;

} gtkDesc;


static int numGTKDevices = 0;



// Device driver actions
static void   GTK_Activate(DevDesc *);
static void   GTK_Circle(double, double, int, double, int, int, DevDesc*);
static void   GTK_Clip(double, double, double, double, DevDesc*);
static void   GTK_Close(DevDesc*);
static void   GTK_Deactivate(DevDesc *);
static void   GTK_Hold(DevDesc*);
static void   GTK_Line(double, double, double, double, int, DevDesc*);
static int    GTK_Locator(double*, double*, DevDesc*);
static void   GTK_Mode(int);
static void   GTK_NewPage(DevDesc*);
static int    GTK_Open(DevDesc*, gtkDesc*, char*, double, double);
static void   GTK_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   GTK_Polyline(int, double*, double*, int, DevDesc*);
static void   GTK_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   GTK_Resize(DevDesc*);
static double GTK_StrWidth(char*, DevDesc*);
static void   GTK_Text(double, double, int, char*, double, double, double,
		       DevDesc*);
static void   GTK_MetricInfo(int, double*, double*, double*, DevDesc*);

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

// font stuff



// color stuff

static void SetColor(DevDesc *dd, int color)
{
  gint i, r, g, b;
  gboolean found = FALSE;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
  gulong tmpcol;
  gint failed;

  //  g_print("SetColor: ");

  //  g_print("r %d, g %d, b %d ", R_RED(color), R_GREEN(color), R_BLUE(color));

  if(dd->dp.gamma != 1) {
    r = (gint) (255 * pow(R_RED(color) / 255.0, dd->gp.gamma));
    g = (gint) (255 * pow(R_GREEN(color) / 255.0, dd->gp.gamma));
    b = (gint) (255 * pow(R_BLUE(color) / 255.0, dd->gp.gamma));
  }
  else {
    r = R_RED(color);
    g = R_GREEN(color);
    b = R_BLUE(color);
  }

  tmpcol = (r << 16) | (g << 8) | b;

  gtkd->fgcolor.red = r * 257;
  gtkd->fgcolor.green = g * 257;
  gtkd->fgcolor.blue = b * 257;

  gtkd->fgcolor.pixel = gdk_rgb_xpixel_from_rgb(tmpcol);

  //  gdk_colormap_alloc_color(gdk_window_get_colormap(gtkd->drawingarea->window),
  //  			   &gtkd->fgcolor, FALSE, FALSE);

  //  gtkd->fgcolor.pixel = gdk_color_context_get_pixel(gtkd->cc, r, g, b, &failed);

  // found:
  gtkd->col = color;
  gdk_gc_set_foreground(gtkd->wgc, &gtkd->fgcolor);
  //  gdk_gc_set_background(gtkd->wgc, &gtkd->bgcolor);
}

static void FreeColors(DevDesc *dd)
{
}

// line type

static void SetLineType(DevDesc *dd, int newlty, double nlwd)
{
  static gchar dashlist[8];
  gint i, j, newlwd;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  newlwd = nlwd;

  if(newlty != gtkd->lty || newlwd != gtkd->lwd) {
    gtkd->lty = newlty;
    gtkd->lwd = newlwd;
    
    if(newlty == 0) {
      // FIXME: if we have antialiasing, change to
      // if(newlwd < 1) newlwd = 1;
      if(newlwd <= 1)
	newlwd = 0;
      // set line attributes to solid
      gdk_gc_set_line_attributes(gtkd->wgc, newlwd, 
				 GDK_LINE_SOLID,
				 GDK_CAP_BUTT,
				 GDK_JOIN_ROUND);
    }
    else {
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

      // set dashes
      gdk_gc_set_dashes(gtkd->wgc, 0, dashlist, i);
      // set line attributes to dashed
      gdk_gc_set_line_attributes(gtkd->wgc, newlwd, 
				 GDK_LINE_ON_OFF_DASH,
				 GDK_CAP_BUTT,
				 GDK_JOIN_ROUND);
    }
  }
}

// signal functions

// FIXME: make sure return values are correct

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data) {  
  DevDesc *dd;
  gtkDesc *gtkd;

  //  g_print("configure\n");

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  gtkd = (gtkDesc *) dd->deviceSpecific;
  g_return_val_if_fail(gtkd != NULL, FALSE);
  g_return_val_if_fail(gtkd->drawingarea != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawingarea), FALSE);

  // resize
  if((gtkd->windowWidth != event->width) || (gtkd->windowHeight != event->height)) {
    gtkd->windowWidth = event->width;
    gtkd->windowHeight = event->height;

    gtkd->resize = 1;
  }

  if(gtkd->pixmap != NULL)
    gdk_pixmap_unref(gtkd->pixmap);

  // create pixmap
  gtkd->pixmap = gdk_pixmap_new(gtkd->drawingarea->window,
			        gtkd->drawingarea->allocation.width,
				gtkd->drawingarea->allocation.height,
				-1);

  // clear the pixmap to background color
  if(gtkd->wgc != NULL) {
    gdk_gc_set_foreground(gtkd->wgc, &gtkd->bgcolor);
    gdk_draw_rectangle(gtkd->pixmap,
		       gtkd->wgc,
		       TRUE,
		       0, 0,
		       gtkd->drawingarea->allocation.width,
		       gtkd->drawingarea->allocation.height);
    gdk_gc_set_foreground(gtkd->wgc, &gtkd->fgcolor);
  }

  return FALSE;
 }

static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data) {
  DevDesc *dd;
  gtkDesc *gtkd;
  GdkRectangle clip_tmp;

  //  g_print("expose\n");

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  gtkd = (gtkDesc *) dd->deviceSpecific;
  g_return_val_if_fail(gtkd != NULL, FALSE);
  g_return_val_if_fail(gtkd->drawingarea != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawingarea), FALSE);

  // if we've been resized, replay the display list
  if(gtkd->resize != 0) {
    dd->dp.resize(dd);
    playDisplayList(dd);
  }
  // else blit the backing pixmap
  else {
    clip_tmp.x = 0;
    clip_tmp.y = 0;
    clip_tmp.width = gtkd->drawingarea->allocation.width;
    clip_tmp.height = gtkd->drawingarea->allocation.height;

    gdk_gc_set_clip_rectangle(gtkd->wgc, &clip_tmp);

    // blit backing pixmap to screen
    gdk_draw_pixmap(gtkd->drawingarea->window,
		    gtkd->wgc,
		    gtkd->pixmap,
		    event->area.x, event->area.y,
		    event->area.x, event->area.y,
		    event->area.width, event->area.height);
    
    gdk_gc_set_clip_rectangle(gtkd->wgc, &gtkd->clip);
  }

  return FALSE;
}

static gint delete_event(GtkWidget *widget, GdkEvent *event, gpointer data) {
  DevDesc *dd;
  gtkDesc *gtkd;

  //  g_print("delete\n");

  dd = (DevDesc *) data;
  g_return_val_if_fail(dd != NULL, FALSE);

  KillDevice(dd);

  return TRUE;
}

static void button_active_event(GtkWidget *widget, gpointer data) {
  DevDesc *dd;
  gtkDesc *gtkd;
  gint devnum;
  guint numdigits;
  gchar *digit_text;

  //  g_print("active\n");

  dd = (DevDesc *) data;
  g_return_if_fail(dd != NULL);

  gtkd = dd->deviceSpecific;
  g_return_if_fail(gtkd != NULL);

  devnum = deviceNumber(dd);
  devnum++;
  numdigits = floor(Log10(devnum)) + 1;

  digit_text = g_malloc(numdigits + 1);
  
  g_return_if_fail(digit_text != NULL);

  snprintf(digit_text, numdigits + 1, "%d", devnum);
  *(digit_text + numdigits) = '\0';
    
  R_gtk_terminal_run_initial();
  R_gtk_terminal_run_partial("dev.set(");
  R_gtk_terminal_run_partial(digit_text);
  R_gtk_terminal_run_final(")");

  g_free(digit_text);
}

static void button_close_event(GtkWidget *widget, gpointer data) {
  DevDesc *dd;

  //  g_print("close\n");

  dd = (DevDesc *) data;
  g_return_if_fail(dd != NULL);

  KillDevice(dd);
}

// create window etc
static int GTK_Open(DevDesc *dd, gtkDesc *gtkd, char *dsp, double w, double h)
{
  GdkPixmap *icon;
  GdkBitmap *mask;
  GtkWidget *iconw;

  gint iw, ih, result;

  gtkd->pixmap = NULL;
  gtkd->wgc = NULL;
  gtkd->drawingarea = NULL;

  // FIXME: display stuff?
  gdk_rgb_set_install(TRUE);
  gdk_rgb_init();
  gtk_widget_push_visual(gdk_rgb_get_visual());
  gtk_widget_push_colormap(gdk_rgb_get_cmap());
  //gtk_widget_push_visual(gdk_visual_get_system());
  //  gtk_widget_push_colormap(gdk_colormap_new(gdk_visual_get_system(), TRUE));

  //  gtkd->cc = gdk_color_context_new(gdk_visual_get_system(), gdk_colormap_get_system());

  // FIXME: SetBaseFont

  // FIXME: window attributes

  // create window etc
  gtkd->windowWidth = iw = w / pixelWidth();
  gtkd->windowHeight = ih = h / pixelHeight();

  gtkd->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  gtk_window_set_title(GTK_WINDOW(gtkd->window), "R Graphics");
  gtk_window_set_policy(GTK_WINDOW(gtkd->window), TRUE, TRUE, FALSE);
  gtk_container_border_width(GTK_CONTAINER(gtkd->window), 0);
  //  g_print("realize window\n");
  gtk_widget_realize(gtkd->window);
  
  gtkd->vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(gtkd->window), gtkd->vbox);

  gtkd->handlebox = gtk_handle_box_new();
  gtk_box_pack_start(GTK_BOX(gtkd->vbox), gtkd->handlebox, FALSE, TRUE, 0);
  
  gtkd->toolbar = gtk_toolbar_new(GTK_ORIENTATION_HORIZONTAL, GTK_TOOLBAR_BOTH);
  gtk_container_border_width(GTK_CONTAINER(gtkd->toolbar), 5);
  gtk_toolbar_set_space_size(GTK_TOOLBAR(gtkd->toolbar), 5);

  // add toolbar items
  icon = gdk_pixmap_create_from_xpm_d (gtkd->window->window, &mask, &gtkd->window->style->white, jump_to_xpm);
  iconw = gtk_pixmap_new(icon, mask);
  gtk_toolbar_append_item(GTK_TOOLBAR(gtkd->toolbar),
			  "Activate",
			  "Activate this device",
			  NULL,
			  iconw,
			  GTK_SIGNAL_FUNC(button_active_event),
			  (gpointer) dd);

  gtk_toolbar_append_space(GTK_TOOLBAR(gtkd->toolbar));

  icon = gdk_pixmap_create_from_xpm_d (gtkd->window->window, &mask, &gtkd->window->style->white, print_xpm);
  iconw = gtk_pixmap_new(icon, mask);
  gtk_toolbar_append_item(GTK_TOOLBAR(gtkd->toolbar),
			  "Print",
			  "Print the plot",
			  NULL,
			  iconw,
			  NULL,
			  NULL);

  icon = gdk_pixmap_create_from_xpm_d (gtkd->window->window, &mask, &gtkd->window->style->white, save_as_xpm);
  iconw = gtk_pixmap_new(icon, mask);
  gtk_toolbar_append_item(GTK_TOOLBAR(gtkd->toolbar),
			  "Save as",
			  "Save the plot",
			  NULL,
			  iconw,
			  NULL,
			  NULL);

  gtk_toolbar_append_space(GTK_TOOLBAR(gtkd->toolbar));

  icon = gdk_pixmap_create_from_xpm_d (gtkd->window->window, &mask, &gtkd->window->style->white, close_xpm);
  iconw = gtk_pixmap_new(icon, mask);
  gtk_toolbar_append_item(GTK_TOOLBAR(gtkd->toolbar),
			  "Close",
			  "Close device",
			  NULL,
			  iconw,
			  GTK_SIGNAL_FUNC(button_close_event),
			  (gpointer) dd);

  // finished adding toolbar items

  gtk_toolbar_set_tooltips(GTK_TOOLBAR(gtkd->toolbar), TRUE);

  gtk_container_add(GTK_CONTAINER(gtkd->handlebox), gtkd->toolbar);

  // create drawingarea
  gtkd->drawingarea = gtk_drawing_area_new();

  gtk_drawing_area_size(GTK_DRAWING_AREA(gtkd->drawingarea), iw, ih);
  gtk_box_pack_start(GTK_BOX(gtkd->vbox), gtkd->drawingarea, TRUE, TRUE, 0);

  // make sure it exists
  gtk_widget_realize(gtkd->drawingarea);

  // same as XSynch()
  gdk_flush();

  // set the cursor
  gtkd->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
  gdk_window_set_cursor(gtkd->drawingarea->window, gtkd->gcursor);

  // colors
  gtkd->bg = dd->dp.bg = R_RGB(255, 255, 255);
  gtkd->fg = dd->dp.fg = R_RGB(0, 0, 0);
  gtkd->col = dd->dp.col = gtkd->fg;

  // FIXME: might have to change this if we create a new cmap
  gtkd->fgcolor = gtkd->drawingarea->style->black;
  gtkd->bgcolor = gtkd->drawingarea->style->white;

  // set window background color
  gdk_window_set_background(gtkd->drawingarea->window, &gtkd->bgcolor);

  // gc
  gtkd->wgc = gdk_gc_new(gtkd->drawingarea->window);

  gdk_gc_set_foreground(gtkd->wgc, &gtkd->fgcolor);

  // connect to delete signal handler, etc
  gtk_signal_connect(GTK_OBJECT(gtkd->drawingarea), "configure_event",
		     (GtkSignalFunc) configure_event, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(gtkd->drawingarea), "expose_event",
		     (GtkSignalFunc) expose_event, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(gtkd->window), "delete_event",
		     (GtkSignalFunc) delete_event, (gpointer) dd);
		     
  // show everything
  //  g_print("show all\n");
  gtk_widget_show_all(gtkd->window);
  //  g_print("show all end\n");

  // clipping
  gtkd->clip.x = gtkd->clip.y = 0;
  gtkd->clip.width = gtkd->drawingarea->allocation.width;
  gtkd->clip.height = gtkd->drawingarea->allocation.height;

  // finish up
  gtkd->lty = -1;
  gtkd->lwd = -1;


  gtk_widget_pop_visual();
  gtk_widget_pop_colormap();

  return 1;
}

static double GTK_StrWidth(char *str, DevDesc *dd)
{
  return 6.0;
}

static void GTK_MetricInfo(int c, double *ascent, double *descent, double *width, DevDesc *dd)
{
}

// set clipping on the drawing area
static void GTK_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  gtkd->clip.x = (int) MIN(x0, x1);
  gtkd->clip.width = abs((int) x0 - x1);

  gtkd->clip.y = (int) MIN(y0, y1);
  gtkd->clip.height = abs((int) y0 - y1);

  //  g_print("clip x %d, y %d, width %d, height %d\n",
  //	  gtkd->clip.x, gtkd->clip.y,
  //	  gtkd->clip.width, gtkd->clip.height);

  gdk_gc_set_clip_rectangle(gtkd->wgc, &gtkd->clip);
}

static void GTK_Resize(DevDesc *dd)
{
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  if (gtkd->resize != 0) {
    dd->dp.left = dd->gp.left = 0.0;
    dd->dp.right = dd->gp.right =  gtkd->windowWidth;
    dd->dp.bottom = dd->gp.bottom = gtkd->windowHeight;
    dd->dp.top = dd->gp.top = 0.0;
    gtkd->resize = 0;
  }

  //  g_print("resize\n");
}

// clear the drawing area
static void GTK_NewPage(DevDesc *dd)
{
  int result;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
  GdkRectangle clip_tmp;

  //  g_print("new page\n");

  FreeColors(dd);

  // FIXME: set window background
  /*  if(gtkd->bg != dd->dp.bg) {
    gtkd->bg = dd->dp.bg;
    gtkd->bgcolor.red = R_RED(gtkd->bg) * 257;
    gtkd->bgcolor.green = R_GREEN(gtkd->bg) * 257;
    gtkd->bgcolor.blue = R_BLUE(gtkd->bg) * 257;
    
    result = gdk_colormap_alloc_color(cmap, &gtkd->bgcolor,
				      TRUE, TRUE);
    if(result == FALSE)
      error("color allocation error\n");

    gdk_window_set_background(gtkd->drawingarea->window, &gtkd->bgcolor);
    }*/

  // clear the pixmap
  clip_tmp.x = 0;
  clip_tmp.y = 0;
  clip_tmp.width = gtkd->drawingarea->allocation.width;
  clip_tmp.height = gtkd->drawingarea->allocation.height;

  gdk_gc_set_clip_rectangle(gtkd->wgc, &clip_tmp);

  gdk_gc_set_foreground(gtkd->wgc, &gtkd->bgcolor);
  gdk_draw_rectangle(gtkd->pixmap,
		     gtkd->wgc,
		     TRUE,
		     0, 0,
		     gtkd->drawingarea->allocation.width,
  		     gtkd->drawingarea->allocation.height);
  gdk_gc_set_foreground(gtkd->wgc, &gtkd->fgcolor);
 
  gdk_gc_set_clip_rectangle(gtkd->wgc, &gtkd->clip);

  // clear the window
  gdk_window_clear(gtkd->drawingarea->window);
}

// kill off the window etc
static void GTK_Close(DevDesc *dd)
{
  gint i, j;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  if(gtkd->wgc != NULL)
    gdk_gc_unref(gtkd->wgc);

  gtk_widget_destroy(gtkd->window);

  numGTKDevices--;
  if(numGTKDevices == 0) {
    // FIXME: unload fonts
  }

  free(gtkd);
}

// These functions need to:
//   - modify the title
//   - toggle the active toolbar button

#define title_text_inactive "R graphics device %d"
#define title_text_active "R graphics device %d - Active"

static void GTK_Activate(DevDesc *dd)
{
  gtkDesc *gtkd;
  gint devnum, numdigits;
  gchar *title_text;
  gint text_len;

  text_len = strlen(title_text_active);

  gtkd = (gtkDesc *) dd->deviceSpecific;

  g_return_if_fail(gtkd != NULL);

  devnum = deviceNumber(dd);
  devnum++;
  numdigits = floor(Log10(devnum)) + 1;
  title_text = g_malloc(text_len + numdigits + 1);

  g_return_if_fail(title_text != NULL);

  snprintf(title_text, text_len + numdigits + 1, title_text_active, devnum);
  *(title_text + text_len + numdigits) = '\0';

  gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

  g_free(title_text);
}

static void GTK_Deactivate(DevDesc *dd)
{
  gtkDesc *gtkd;
  gint devnum, numdigits;
  gchar *title_text;
  gint text_len;

  text_len = strlen(title_text_inactive);

  gtkd = (gtkDesc *) dd->deviceSpecific;

  g_return_if_fail(gtkd != NULL);

  devnum = deviceNumber(dd);
  devnum++;
  numdigits = floor(Log10(devnum)) + 1;
  title_text = g_malloc(text_len + numdigits + 1);

  g_return_if_fail(title_text != NULL);

  snprintf(title_text, text_len + numdigits + 1, title_text_inactive, devnum);
  *(title_text + text_len + numdigits) = '\0';

  gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

  g_free(title_text);
}

// drawing stuff

static void GTK_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
  double tmp;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

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

  //  g_print("rect\n");

  if(bg != NA_INTEGER) {
    SetColor(dd, bg);
    gdk_draw_rectangle(gtkd->drawingarea->window, gtkd->wgc,
		       TRUE,
		       (gint) x0, (gint) y0,
		       (gint) x1 - (gint) x0,
		       (gint) y1 - (gint) y0);
    gdk_draw_rectangle(gtkd->pixmap, gtkd->wgc,
		       TRUE,
		       (gint) x0, (gint) y0,
		       (gint) x1 - (gint) x0,
		       (gint) y1 - (gint) y0);
  }
  if(fg != NA_INTEGER) {
    SetColor(dd, fg);
    SetLineType(dd, dd->gp.lty, dd->gp.lwd);
    gdk_draw_rectangle(gtkd->drawingarea->window, gtkd->wgc,
		       FALSE,
		       (gint) x0, (gint) y0,
		       (gint) x1 - (gint) x0,
		       (gint) y1 - (gint) y0);
    gdk_draw_rectangle(gtkd->pixmap, gtkd->wgc,
		       FALSE,
		       (gint) x0, (gint) y0,
		       (gint) x1 - (gint) x0,
		       (gint) y1 - (gint) y0);
  }
}

static void GTK_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
  gint ir, ix, iy;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

#ifdef OLD
  ir = ceil(r);
#else
  ir = floor(r + 0.5);
#endif

  GConvert(&x, &y, coords, DEVICE, dd);
  ix = (gint) x;
  iy = (gint) y;

  //  g_print("circle x %d, y %d, r %d\n", ix, iy, ir);

  if(col != NA_INTEGER) {
    SetColor(dd, col);
    gdk_draw_arc(gtkd->drawingarea->window, gtkd->wgc,
		 TRUE,
		 ix - ir, iy - ir,
		 2 * ir, 2 * ir,
		 0, 23040);
    gdk_draw_arc(gtkd->pixmap, gtkd->wgc,
		 TRUE,
		 ix - ir, iy - ir,
		 2 * ir, 2 * ir,
		 0, 23040);
  }
  if(border != NA_INTEGER) {
    SetLineType(dd, dd->gp.lty, dd->gp.lwd);
    SetColor(dd, border);
    gdk_draw_arc(gtkd->drawingarea->window, gtkd->wgc,
		 FALSE,
		 ix - ir, iy - ir,
		 2 * ir, 2 * ir,
		 0, 23040);
    gdk_draw_arc(gtkd->pixmap, gtkd->wgc,
		 FALSE,
		 ix - ir, iy - ir,
		 2 * ir, 2 * ir,
		 0, 23040);
  }
}

static void GTK_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
  gint xx1, yy1, xx2, yy2;
  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  GConvert(&x1, &y1, coords, DEVICE, dd);
  GConvert(&x2, &y2, coords, DEVICE, dd);

  xx1 = (gint) x1;
  yy1 = (gint) y1;
  xx2 = (gint) x2;
  yy2 = (gint) y2;

  //  g_print("line device %d, x1 %d, y1 %d, x2 %d, y2 %d\n", DEVICE, xx1, yy1, xx2, yy2);

  SetColor(dd, dd->gp.col);
  SetLineType(dd, dd->gp.lty, dd->gp.lwd);
  gdk_draw_line(gtkd->drawingarea->window, gtkd->wgc,
		xx1, yy1, xx2, yy2);
  gdk_draw_line(gtkd->pixmap, gtkd->wgc,
		xx1, yy1, xx2, yy2);
}

static void GTK_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
  GdkPoint *points;
  double devx, devy;
  int i;

  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  points = (GdkPoint *) C_alloc(n, sizeof(GdkPoint));

  for(i = 0; i < n; i++) {
    devx = x[i]; devy = y[i];
    GConvert(&devx, &devy, coords, DEVICE, dd);
    points[i].x = (gint16) devx;
    points[i].y = (gint16) devy;
  }

  //  g_print("polyline\n");

  SetColor(dd, dd->gp.col);
  SetLineType(dd, dd->gp.lty, dd->gp.lwd);
  gdk_draw_lines(gtkd->drawingarea->window, gtkd->wgc,
		 points, n);
  gdk_draw_lines(gtkd->pixmap, gtkd->wgc,
		 points, n);

  C_free((char *) points);
}

static void GTK_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
  GdkPoint *points;
  double devx, devy;
  int i;

  gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

  points = (GdkPoint *) C_alloc(n + 1, sizeof(GdkPoint));

  for(i = 0; i < n; i++) {
    devx = x[i]; devy = y[i];
    GConvert(&devx, &devy, coords, DEVICE, dd);
    points[i].x = (gint16) devx;
    points[i].y = (gint16) devy;
  }

  //  g_print("polygon\n");

  if(bg != NA_INTEGER) {
    SetColor(dd, bg);
    gdk_draw_polygon(gtkd->drawingarea->window, gtkd->wgc,
		     TRUE,
		     points,
		     n);
    gdk_draw_polygon(gtkd->pixmap, gtkd->wgc,
		     TRUE,
		     points,
		     n);
  }
  if(fg != NA_INTEGER) {
    SetColor(dd, fg);
    SetLineType(dd, dd->gp.lty, dd->gp.lwd);
    gdk_draw_polygon(gtkd->drawingarea->window, gtkd->wgc,
		     FALSE,
		     points,
		     n);
    gdk_draw_polygon(gtkd->pixmap, gtkd->wgc,
		     FALSE,
		     points,
		     n);
  }

  C_free((char *) points);
}

double deg2rad = 0.01745329251994329576;

static void GTK_Text(double x, double y, int coords,
		     char *str, double xc, double yc, double rot, DevDesc *dd)
{
}

// locator
static int GTK_Locator(double *x, double *y, DevDesc *dd)
{
}

// useless stuff

static void GTK_Mode(gint mode)
{
}

static void GTK_Hold(DevDesc *dd)
{
}


// Device driver entry point
int X11DeviceDriver(DevDesc *dd, char *display, double width, double height, double pointsize)
{
  return GTKDeviceDriver(dd, display, width, height, pointsize);
}

int GTKDeviceDriver(DevDesc *dd, char *display, double width, double height, double pointsize)
{
  int ps;
  gtkDesc *gtkd;

  if(!(gtkd = (gtkDesc *) malloc(sizeof(gtkDesc))))
    return 0;

  dd->deviceSpecific = (void *) gtkd;

  // FIXME: font loading
  ps = pointsize;
  if(ps < 6 || ps > 24) ps = 12;
  ps = 2 * (ps / 2);
  gtkd->fontface = -1;
  gtkd->fontsize = -1;
  dd->dp.font = 1;
  dd->dp.ps = ps;
  
  // device driver start
  if(!GTK_Open(dd, gtkd, display, width, height)) {
    free(gtkd);
    return 0;
  }

  // setup data structure
  dd->dp.open = GTK_Open;
  dd->dp.close = GTK_Close;
  dd->dp.activate = GTK_Activate;
  dd->dp.deactivate = GTK_Deactivate;
  dd->dp.resize = GTK_Resize;
  dd->dp.newPage = GTK_NewPage;
  dd->dp.clip = GTK_Clip;
  dd->dp.strWidth = GTK_StrWidth;
  dd->dp.text = GTK_Text;
  dd->dp.rect = GTK_Rect;
  dd->dp.circle = GTK_Circle;
  dd->dp.line = GTK_Line;
  dd->dp.polyline = GTK_Polyline;
  dd->dp.polygon = GTK_Polygon;
  dd->dp.locator = GTK_Locator;
  dd->dp.mode = GTK_Mode;
  dd->dp.hold = GTK_Hold;
  dd->dp.metricInfo = GTK_MetricInfo;

  dd->dp.left = 0;
  dd->dp.right = gtkd->windowWidth;
  dd->dp.bottom = gtkd->windowHeight;
  dd->dp.top = 0;

  // FIXME: nominal character sizes
  dd->dp.cra[0] = 10;
  dd->dp.cra[1] = 10;

  // FIXME: character addressing offsets
  dd->dp.xCharOffset = 0.4900;
  dd->dp.yCharOffset = 0.3333;
  dd->dp.yLineBias = 0.1;

  // inches per raster unit
  dd->dp.ipr[0] = pixelWidth();
  dd->dp.ipr[1] = pixelHeight();

  // device capabilities
  dd->dp.canResizePlot = 1;
  dd->dp.canChangeFont = 0; // FIXME: surely this is possible
  dd->dp.canRotateText = 1;
  dd->dp.canResizeText = 1;
  dd->dp.canClip = 1;

  // x11 device description stuff
  gtkd->cex = 1.0;
  gtkd->srt = 0.0;
  gtkd->lty = 0;
  gtkd->resize = 0;

  dd->displayListOn = 1;

  // finish
  return 1;
}

int X11ConnectionNumber()
{
  return 0;
}
