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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <gnome.h>

#include <Defn.h>
#include <Graphics.h>
#include <Devices.h>
#include "devGTK.h"
#include "terminal.h"
#include "gdkrotated.h"

#define CURSOR		GDK_CROSSHAIR		/* Default cursor */
#define MM_PER_INCH	25.4			/* mm -> inch conversion */

typedef struct {
    /* R Graphics Parameters */
    /* Local device copy so that we can detect */
    /* when parameter changes. */

    double cex;				/* Character expansion */
    double srt;				/* String rotation */

    gint bg;				/* Background */

    int fontface;			/* Typeface */
    int fontsize;			/* Size in points */

    gint lty, lwd;                      /* line params */

    /* GTK Driver Specific */

    int windowWidth;			/* Window width (pixels) */
    int windowHeight;			/* Window height (pixels) */
    Rboolean resize;			/* Window resized */
    GtkWidget *window;			/* Graphics frame */
    GtkWidget *drawing;                 /* Drawable window */

    GdkPixmap *pixmap;                  /* Backing store */

    GdkGC *wgc;
    GdkColor gcol_bg;
    GdkRectangle clip;
    GdkCursor *gcursor;

    Rboolean usefixed;
    GdkFont *font;

} gtkDesc;

/* routines from here */
Rboolean GTKDeviceDriver(DevDesc *dd, char *display, double width, 
			 double height, double pointsize);

/* Device driver actions */
static void   GTK_Activate(DevDesc *);
static void   GTK_Circle(double, double, int, double, int, int, DevDesc*);
static void   GTK_Clip(double, double, double, double, DevDesc*);
static void   GTK_Close(DevDesc*);
static void   GTK_Deactivate(DevDesc *);
static void   GTK_Hold(DevDesc*);
static void   GTK_Line(double, double, double, double, int, DevDesc*);
static Rboolean GTK_Locator(double*, double*, DevDesc*);
static void   GTK_Mode(int, DevDesc*);
static void   GTK_NewPage(DevDesc*);
static Rboolean GTK_Open(DevDesc*, gtkDesc*, char*, double, double);
static void   GTK_Polygon(int, double*, double*, int, int, int, DevDesc*);
static void   GTK_Polyline(int, double*, double*, int, DevDesc*);
static void   GTK_Rect(double, double, double, double, int, int, int, DevDesc*);
static void   GTK_Resize(DevDesc*);
static double GTK_StrWidth(char*, DevDesc*);
static void   GTK_Text(double, double, int, char*, double, double, DevDesc*);
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

/* font stuff */

static char *fontname_R6 = "-adobe-helvetica-%s-%s-*-*-*-%d-*-*-*-*-*-*";
static char *symbolname = "-adobe-symbol-*-*-*-*-*-%d-*-*-*-*-*-*";
static char *fixedname = "fixed";

static char *slant[] = {"r", "o"};
static char *weight[] = {"medium", "bold"};

char *fontname;
GHashTable *font_htab = NULL;

struct _FontMetricCache {
    gint ascent[255];
    gint descent[255];
    gint width[255];
    gint font_ascent;
    gint font_descent;
    gint max_width;
};

static GdkFont *RGTKLoadFont(char *font)
{
    GdkFont *tmp_font;

    tmp_font = g_hash_table_lookup(font_htab, (gpointer) font);

    if(tmp_font == NULL) {
        tmp_font = gdk_font_load(font);

        if(tmp_font != NULL) {
            g_hash_table_insert(font_htab, (gpointer) g_strdup(font),
				(gpointer) tmp_font);
        }
    }

    return tmp_font;
}

static gint SetBaseFont(gtkDesc *gtkd)
{
    gtkd->fontface = 1;
    gtkd->fontsize = 12;
    gtkd->usefixed = 0;

    if(font_htab == NULL) {
	font_htab = g_hash_table_new(g_str_hash, g_str_equal);
    }

    fontname = g_strdup_printf(fontname_R6, weight[0], slant[0], 
			       gtkd->fontsize * 10);
    gtkd->font = RGTKLoadFont(fontname);
    g_free(fontname);

    if(gtkd->font != NULL)
	return 1;

    gtkd->usefixed = 1;
    gtkd->font = RGTKLoadFont(fixedname);

    if(gtkd->font != NULL)
	return 1;

    return 0;
}

#define SMALLEST 8
#define LARGEST 24

static void SetFont(DevDesc *dd, gint face, gint size)
{
    GdkFont *tmp_font;
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    if(face < 1 || face > 5)
	face = 1;

    size = 2 * size / 2;
    if(size < SMALLEST)
	size = SMALLEST;
    else if(size > LARGEST)
	size = LARGEST;

    gtkd->fontface = face;
    gtkd->fontsize = size;

    if(gtkd->usefixed == 0){
	if(face == 5)
	    fontname = g_strdup_printf(symbolname, 10 * size);
	else
	    fontname = g_strdup_printf(fontname_R6,
				       weight[(face-1)%2],
				       slant[((face-1)/2)%2],
				       10 * size);

	tmp_font = RGTKLoadFont(fontname);
	g_free(fontname);

	if(tmp_font != NULL) {
	    gtkd->font = tmp_font;
	}
    }
}


/* set the r, g, b, and pixel values of gcol to color */
static void SetColor(GdkColor *gcol, int color)
{
    int red, green, blue;

    red = R_RED(color);
    green = R_GREEN(color);
    blue = R_BLUE(color);
    gcol->red = 0;
    gcol->green = 0;
    gcol->blue = 0;
    gcol->pixel = gdk_rgb_xpixel_from_rgb((red << 16)|(green << 8)|(blue));
}

/* set the line type */
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
	    if(newlwd <= 1)
		newlwd = 0;

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
		if(j == 0) j = 1;
		j = j * newlwd;
		if(j > 255) j = 255;
		dashlist[i] = j;
		newlty = newlty >> 4;
	    }

	    /* set dashes */
	    gdk_gc_set_dashes(gtkd->wgc, 0, dashlist, i);
	    gdk_gc_set_line_attributes(gtkd->wgc, newlwd,
				       GDK_LINE_ON_OFF_DASH,
				       GDK_CAP_BUTT,
				       GDK_JOIN_ROUND);
	}
    }
}

/* signal functions */

static gint realize_event(GtkWidget *widget, gpointer data)
{
    DevDesc *dd;
    gtkDesc *gtkd;

    dd = (DevDesc *) data;
    g_return_val_if_fail(dd != NULL, FALSE);

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_val_if_fail(gtkd != NULL, FALSE);
    g_return_val_if_fail(gtkd->drawing != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawing), FALSE);

    /* create gc */
    gtkd->wgc = gdk_gc_new(gtkd->drawing->window);

    /* set the cursor */
    gtkd->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
    gdk_window_set_cursor(gtkd->drawing->window, gtkd->gcursor);

    /* set window bg */
    gdk_window_set_background(gtkd->drawing->window, &gtkd->gcol_bg);

    return FALSE;
}

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
    DevDesc *dd;
    gtkDesc *gtkd;

    dd = (DevDesc *) data;
    g_return_val_if_fail(dd != NULL, FALSE);

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_val_if_fail(gtkd != NULL, FALSE);
    g_return_val_if_fail(gtkd->drawing != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawing), FALSE);

    /* check for resize */
    if((GTK_WIDGET_REALIZED(gtkd->drawing)) && ((gtkd->windowWidth != event->width) || (gtkd->windowHeight != event->height))) {
	gtkd->windowWidth = event->width;
	gtkd->windowHeight = event->height;

	gtkd->resize = TRUE;
    }

    return FALSE;
}

static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
    DevDesc *dd;
    gtkDesc *gtkd;

    dd = (DevDesc *) data;
    g_return_val_if_fail(dd != NULL, FALSE);

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_val_if_fail(gtkd != NULL, FALSE);
    g_return_val_if_fail(gtkd->drawing != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawing), FALSE);

    if(gtkd->resize != 0) {
	dd->dp.resize(dd);
    }

  
    gdk_draw_pixmap(gtkd->drawing->window, gtkd->wgc, gtkd->pixmap,
		    event->area.x, event->area.y, event->area.x, event->area.y,
		    event->area.width, event->area.height);

#if 0
    playDisplayList(dd);
#endif

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

static void tb_activate_cb(GtkWidget *widget, gpointer data)
{
    DevDesc *dd;

    dd = (DevDesc *) data;
    g_return_if_fail(dd != NULL);

    selectDevice(deviceNumber(dd));
}

static void tb_close_cb(GtkWidget *widget, gpointer data)
{
    DevDesc *dd;

    dd = (DevDesc *) data;
    g_return_if_fail(dd != NULL);

    KillDevice(dd);
}

static GnomeUIInfo graphics_toolbar[] =
{
    { GNOME_APP_UI_ITEM, "Activate", "Make this window the current device", tb_activate_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_JUMP_TO, 0, (GdkModifierType) 0, NULL },
    GNOMEUIINFO_SEPARATOR,
    { GNOME_APP_UI_ITEM, "Close", "Close this graphics device", tb_close_cb, NULL, NULL, GNOME_APP_PIXMAP_STOCK, GNOME_STOCK_PIXMAP_CLOSE, 0, (GdkModifierType) 0, NULL },
    GNOMEUIINFO_END
};

/* create window etc */
static Rboolean GTK_Open(DevDesc *dd, gtkDesc *gtkd, char *dsp, double w, double h)
{
    gint iw, ih;

    /* initialise pointers */
    gtkd->drawing = NULL;
    gtkd->wgc = NULL;
    gtkd->gcursor = NULL;

    /* initialise colour */
    gdk_rgb_init();
    gtk_widget_push_visual(gdk_rgb_get_visual());
    gtk_widget_push_colormap(gdk_rgb_get_cmap());

    /* create window etc */
    gtkd->windowWidth = iw = w / pixelWidth();
    gtkd->windowHeight = ih = h / pixelHeight();

    gtkd->window = gnome_app_new("R.graphics", "R Graphics");

    gtk_window_set_policy(GTK_WINDOW(gtkd->window), TRUE, TRUE, FALSE);
    gtk_widget_realize(gtkd->window);

    /* create toolbar */
    gnome_app_create_toolbar_with_data(GNOME_APP(gtkd->window), graphics_toolbar, (gpointer) dd);

    /* create drawingarea */
    gtkd->drawing = gtk_drawing_area_new();
    gtk_widget_set_events(gtkd->drawing,
			  GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK);

    /* connect to signal handlers, etc */
    gtk_signal_connect(GTK_OBJECT(gtkd->drawing), "realize",
		       (GtkSignalFunc) realize_event, (gpointer) dd);

    /* drawingarea properties */
    gtk_widget_set_usize(gtkd->drawing, iw, ih);

    /* setup background color */
    gtkd->bg = dd->dp.bg = R_RGB(255, 255, 255);
    SetColor(&gtkd->gcol_bg, gtkd->bg);

    /* place and realize the drawing area */
    gnome_app_set_contents(GNOME_APP(gtkd->window), gtkd->drawing);
    gtk_widget_realize(gtkd->drawing);

    /* connect to signal handlers, etc */
    gtk_signal_connect(GTK_OBJECT(gtkd->drawing), "configure_event",
		       (GtkSignalFunc) configure_event, (gpointer) dd);
    gtk_signal_connect(GTK_OBJECT(gtkd->drawing), "expose_event",
		       (GtkSignalFunc) expose_event, (gpointer) dd);
    gtk_signal_connect(GTK_OBJECT(gtkd->window), "delete_event",
		       (GtkSignalFunc) delete_event, (gpointer) dd);

    /* show everything */
    gtk_widget_show_all(gtkd->window);
  
    /* initialise line params */
    gtkd->lty = -1;
    gtkd->lwd = -1;

    /* create offscreen drawable */
    gtkd->pixmap = gdk_pixmap_new(gtkd->drawing->window,
				  gtkd->windowWidth, gtkd->windowHeight,
				  -1);
    gdk_gc_set_foreground(gtkd->wgc, &gtkd->gcol_bg);
    gdk_draw_rectangle(gtkd->pixmap, gtkd->wgc, TRUE, 0, 0,
		       gtkd->windowWidth, gtkd->windowHeight);


    /* let other widgets use the default colour settings */
    gtk_widget_pop_visual();
    gtk_widget_pop_colormap();

    /* Set base font */
    if(!SetBaseFont(gtkd)) {
	Rprintf("can't find X11 font\n");
	return FALSE;
    }

    /* we made it! */
    return TRUE;
}

static double GTK_StrWidth(char *str, DevDesc *dd)
{
    int size;
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd, dd->gp.font, size);

    return (double) gdk_string_width(gtkd->font, str);
}

static void GTK_MetricInfo(int c, double *ascent, double *descent, 
			   double *width, DevDesc *dd)
{
    gint size;
    gint lbearing, rbearing, iascent, idescent, iwidth;
    gint maxwidth;
    gchar tmp[2];
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd, dd->gp.font, size);

    if(c == 0) {
	maxwidth = 0;

	for(c = 0; c <= 255; c++) {
	    g_snprintf(tmp, 2, "%c", (gchar) c);
	    iwidth = gdk_string_width(gtkd->font, tmp);
	    if (iwidth > maxwidth)
		maxwidth = iwidth;
	}

	*ascent = (double) gtkd->font->ascent;
	*descent = (double) gtkd->font->descent;
	*width = (double) maxwidth;
    }
    else {
	g_snprintf(tmp, 2, "%c", (gchar) c);
	gdk_string_extents(gtkd->font, tmp,
			   &lbearing, &rbearing,
			   &iwidth, &iascent, &idescent);

	*ascent = (double) iascent;
	*descent = (double) idescent;
#ifdef OLD
	/* This was always returning a width of zero */
	*width = (double) iwidth;
#else
	*width = (double) (lbearing+rbearing);
#endif
    }
}

/* set clipping */
static void GTK_Clip(double x0, double x1, double y0, double y1, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    gtkd->clip.x = MIN(x0, x1);
    gtkd->clip.width = abs(x0 - x1);

    gtkd->clip.y = MIN(y0, y1);
    gtkd->clip.height = abs(y0 - y1);

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

	gdk_pixmap_unref(gtkd->pixmap);
	gtkd->pixmap = gdk_pixmap_new(gtkd->drawing->window,
				      gtkd->windowWidth, gtkd->windowHeight,
				      -1);
	gdk_gc_set_foreground(gtkd->wgc, &gtkd->gcol_bg);
	gdk_draw_rectangle(gtkd->pixmap, gtkd->wgc, TRUE, 0, 0,
			   gtkd->windowWidth, gtkd->windowHeight);
    }
}

/* clear the drawing area */
static void GTK_NewPage(DevDesc *dd)
{
    gtkDesc *gtkd;

    g_return_if_fail(dd != NULL);

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_if_fail(gtkd != NULL);
    g_return_if_fail(gtkd->drawing != NULL);
    g_return_if_fail(GTK_IS_DRAWING_AREA(gtkd->drawing));

    if(gtkd->bg != dd->dp.bg) {
	SetColor(&gtkd->gcol_bg, dd->dp.bg);
	gtkd->bg = dd->dp.bg;
	gdk_window_set_background(gtkd->drawing->window, &gtkd->gcol_bg);
    }

    gdk_window_clear(gtkd->drawing->window);

    gdk_gc_set_foreground(gtkd->wgc, &gtkd->gcol_bg);
    gdk_draw_rectangle(gtkd->pixmap, gtkd->wgc, TRUE, 0, 0,
		       gtkd->windowWidth, gtkd->windowHeight);
}

/* kill off the window etc */
static void GTK_Close(DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    gtk_widget_destroy(gtkd->window);

    gdk_pixmap_unref(gtkd->pixmap);

    free(gtkd);
}

#define title_text_inactive "R graphics device %d"
#define title_text_active "R graphics device %d - Active"

static void GTK_Activate(DevDesc *dd)
{
    gtkDesc *gtkd;
    gint devnum;
    gchar *title_text;

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_if_fail(gtkd != NULL);

    devnum = deviceNumber(dd) + 1;

    title_text = g_strdup_printf(title_text_active, devnum);

    gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

    g_free(title_text);
}

static void GTK_Deactivate(DevDesc *dd)
{
    gtkDesc *gtkd;
    gint devnum;
    gchar *title_text;

    gtkd = (gtkDesc *) dd->deviceSpecific;
    g_return_if_fail(gtkd != NULL);

    devnum = deviceNumber(dd) + 1;

    title_text = g_strdup_printf(title_text_inactive, devnum);

    gtk_window_set_title(GTK_WINDOW(gtkd->window), title_text);

    g_free(title_text);
}

/* drawing stuff */

static void GTK_Rect(double x0, double y0, double x1, double y1,
		     int coords, int bg, int fg, DevDesc *dd)
{
    double tmp;
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GdkColor gcol_fill, gcol_outline;

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


    if(bg != NA_INTEGER) {
	SetColor(&gcol_fill, bg);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_rectangle(gtkd->drawing->window,
			   gtkd->wgc, TRUE,
			   (gint) x0, (gint) y0,
			   (gint) x1 - (gint) x0,
			   (gint) y1 - (gint) y0);
	gdk_draw_rectangle(gtkd->pixmap,
			   gtkd->wgc, TRUE,
			   (gint) x0, (gint) y0,
			   (gint) x1 - (gint) x0,
			   (gint) y1 - (gint) y0);
    }
    if(fg != NA_INTEGER) {
	SetColor(&gcol_outline, fg);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_outline);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_rectangle(gtkd->drawing->window,
			   gtkd->wgc, FALSE,
			   (gint) x0, (gint) y0,
			   (gint) x1 - (gint) x0,
			   (gint) y1 - (gint) y0);
	gdk_draw_rectangle(gtkd->pixmap,
			   gtkd->wgc, FALSE,
			   (gint) x0, (gint) y0,
			   (gint) x1 - (gint) x0,
			   (gint) y1 - (gint) y0);
    }
}

static void GTK_Circle(double x, double y, int coords,
		       double r, int col, int border, DevDesc *dd)
{
    GdkColor gcol_fill, gcol_outline;
    gint ix, iy, ir;
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;

    GConvert(&x, &y, coords, DEVICE, dd);

    ix = x - r;
    iy = y - r;
    ir = 2 * floor(r + 0.5);

    if(col != NA_INTEGER) {
	SetColor(&gcol_fill, col);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

	gdk_draw_arc(gtkd->drawing->window,
		     gtkd->wgc, TRUE,
		     ix, iy, ir, ir,
		     0, 23040);
	gdk_draw_arc(gtkd->pixmap,
		     gtkd->wgc, TRUE,
		     ix, iy, ir, ir,
		     0, 23040);
    }
    if(border != NA_INTEGER) {
	SetColor(&gcol_outline, border);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_outline);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_arc(gtkd->drawing->window,
		     gtkd->wgc, FALSE,
		     ix, iy, ir, ir,
		     0, 23040);
	gdk_draw_arc(gtkd->pixmap,
		     gtkd->wgc, FALSE,
		     ix, iy, ir, ir,
		     0, 23040);
    }
}

static void GTK_Line(double x1, double y1, double x2, double y2,
		     int coords, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GdkColor gcol_fill;
    gint ix1, iy1, ix2, iy2;

    GConvert(&x1, &y1, coords, DEVICE, dd);
    GConvert(&x2, &y2, coords, DEVICE, dd);

    ix1 = (gint) x1;  iy1 = (gint) y1;
    ix2 = (gint) x2;  iy2 = (gint) y2;

    if(dd->gp.col != NA_INTEGER) {
	SetColor(&gcol_fill, dd->gp.col);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_line(gtkd->drawing->window,
		      gtkd->wgc, ix1, iy1, ix2, iy2);
	gdk_draw_line(gtkd->pixmap,
		      gtkd->wgc, ix1, iy1, ix2, iy2);
    }
}

static void GTK_Polyline(int n, double *x, double *y, int coords, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GdkColor gcol_fill;
    GdkPoint *points;
    double devx, devy;
    int i;

    points = g_new0(GdkPoint, n);

    for(i = 0; i < n; i++) {
	devx = x[i]; devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points[i].x = (gint16) devx;
	points[i].y = (gint16) devy;
    }

    if(dd->gp.col != NA_INTEGER) {
	SetColor(&gcol_fill, dd->gp.col);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_lines(gtkd->drawing->window,
		       gtkd->wgc, points, n);
	gdk_draw_lines(gtkd->pixmap,
		       gtkd->wgc, points, n);
    }

    g_free(points);
}

static void GTK_Polygon(int n, double *x, double *y, int coords,
			int bg, int fg, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GdkColor gcol_fill, gcol_outline;
    GdkPoint *points;
    double devx, devy;
    int i;

    points = g_new0(GdkPoint, n + 1);

    for(i = 0; i < n; i++) {
	devx = x[i]; devy = y[i];
	GConvert(&devx, &devy, coords, DEVICE, dd);
	points[i].x = (gint16) devx;
	points[i].y = (gint16) devy;
    }

    if(bg != NA_INTEGER) {
	SetColor(&gcol_fill, bg);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

	gdk_draw_polygon(gtkd->drawing->window,
			 gtkd->wgc, TRUE, points, n);
	gdk_draw_polygon(gtkd->pixmap,
			 gtkd->wgc, TRUE, points, n);
    }
    if(fg != NA_INTEGER) {
	SetColor(&gcol_outline, fg);
	gdk_gc_set_foreground(gtkd->wgc, &gcol_outline);

	SetLineType(dd, dd->gp.lty, dd->gp.lwd);

	gdk_draw_polygon(gtkd->drawing->window,
			 gtkd->wgc, FALSE, points, n);
	gdk_draw_polygon(gtkd->pixmap,
			 gtkd->wgc, FALSE, points, n);
    }

    g_free(points);
}

static void GTK_Text(double x, double y, int coords,
		     char *str, double rot, double hadj, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GdkColor gcol_fill;
    gint size;
    double rrot = DEG2RAD * rot;

    GConvert(&x, &y, coords, DEVICE, dd);

    size = dd->gp.cex * dd->gp.ps + 0.5;
    SetFont(dd, dd->gp.font, size);
    gdk_gc_set_font(gtkd->wgc, gtkd->font);

    SetColor(&gcol_fill, dd->gp.col);
    gdk_gc_set_foreground(gtkd->wgc, &gcol_fill);

    gdk_draw_text_rot(gtkd->drawing->window,
		      gtkd->font, gtkd->wgc,
		      (int) x, (int) y,
		      gtkd->windowWidth, gtkd->windowHeight,
		      str, strlen(str), rrot);
    gdk_draw_text_rot(gtkd->pixmap,
		      gtkd->font, gtkd->wgc, 
		      (int) x, (int) y,
		      gtkd->windowWidth, gtkd->windowHeight,
		      str, strlen(str), rrot);

}


typedef struct _GTK_locator_info GTK_locator_info;

struct _GTK_locator_info {
    guint x;
    guint y;
    gboolean button1;
};

static gboolean locator_button_press(GtkWidget *widget,
				     GdkEventButton *event,
				     gpointer user_data)
{
    GTK_locator_info *info;

    info = (GTK_locator_info *) user_data;

    info->x = event->x;
    info->y = event->y;
    if(event->button == 1)
	info->button1 = TRUE;
    else
	info->button1 = FALSE;

    gtk_main_quit();
}

static Rboolean GTK_Locator(double *x, double *y, DevDesc *dd)
{
    gtkDesc *gtkd = (gtkDesc *) dd->deviceSpecific;
    GTK_locator_info *info;
    guint handler_id;
    gboolean button1;

    info = g_new(GTK_locator_info, 1);

    /* Flush any pending events */
    while(gtk_events_pending())
	gtk_main_iteration();

    /* connect signal */
    handler_id = gtk_signal_connect(GTK_OBJECT(gtkd->drawing), "button-press-event",
				    (GtkSignalFunc) locator_button_press, (gpointer) info);

    /* run the handler */
    gtk_main();

    *x = (double) info->x;
    *y = (double) info->y;
    button1 = info->button1;

    /* clean up */
    gtk_signal_disconnect(GTK_OBJECT(gtkd->drawing), handler_id);
    g_free(info);

    if(button1)
	return TRUE;

    return FALSE;
}

static void GTK_Mode(gint mode, DevDesc *dd)
{
#ifdef XSYNC
    if(mode == 0)
	gdk_flush();
#else
    gdk_flush();
#endif
}

static void GTK_Hold(DevDesc *dd)
{
}


/* Device driver entry point */
Rboolean
GTKDeviceDriver(DevDesc *dd, char *display, double width, 
		double height, double pointsize)
{
    int ps;
    gchar tmp[2];
    gint cumwidth, c, rbearing, lbearing;
    double max_rbearing, min_lbearing;
    gtkDesc *gtkd;

    if(!(gtkd = (gtkDesc *) malloc(sizeof(gtkDesc))))
	return FALSE;

    dd->deviceSpecific = (void *) gtkd;

    /* font loading */
    ps = pointsize;
    if(ps < 6 || ps > 24) ps = 12;
    ps = 2 * (ps / 2);
    gtkd->fontface = -1;
    gtkd->fontsize = -1;
    dd->dp.font = 1;
    dd->dp.ps = ps;

    /* device driver start */
    if(!GTK_Open(dd, gtkd, display, width, height)) {
	free(gtkd);
	return FALSE;
    }

    /* setup data structure */
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

    /* nominal character sizes */
    cumwidth = 0;
    max_rbearing = 0;
    min_lbearing = 10000; /* just a random big number */
    for(c = 0; c <= 255; c++) {
	g_snprintf(tmp, 2, "%c", (gchar) c);
	gdk_string_extents(gtkd->font, tmp,
			   &lbearing, &rbearing,
			   NULL, NULL, NULL);
	if(lbearing < min_lbearing || c == 0)
	    min_lbearing = lbearing;
	if(rbearing > max_rbearing)
	    max_rbearing = rbearing;
    }

    dd->dp.cra[0] = max_rbearing - min_lbearing;
    dd->dp.cra[1] = (double) gtkd->font->ascent + (double) gtkd->font->descent;

    /* character addressing offsets */
    dd->dp.xCharOffset = 0.4900;
    dd->dp.yCharOffset = 0.3333;
    dd->dp.yLineBias = 0.1;

    /* inches per raster unit */
    dd->dp.ipr[0] = pixelWidth();
    dd->dp.ipr[1] = pixelHeight();

    /* device capabilities */
    dd->dp.canResizePlot= TRUE;
    dd->dp.canChangeFont= FALSE;
    dd->dp.canRotateText= TRUE;
    dd->dp.canResizeText= TRUE;
    dd->dp.canClip = FALSE;/* FIXME: really? */
    dd->dp.canHAdj = 0;/* not better? {0, 0.5, 1} */

    /* gtk device description stuff */
    gtkd->cex = 1.0;
    gtkd->srt = 0.0;
    gtkd->resize = FALSE;

    dd->displayListOn = TRUE;

    /* finish */
    return TRUE;
}

