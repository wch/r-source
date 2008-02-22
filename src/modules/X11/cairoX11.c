/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  R Development Core Team
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
 *  http://www.r-project.org/Licenses/
 */


/* Entry points used

    cairo_arc
    cairo_clip
    cairo_close_path
    cairo_create
    cairo_destroy
    cairo_fill_preserve
    cairo_line_to
    cairo_move_to
    cairo_new_path
    cairo_paint
    cairo_rectangle
    cairo_rel_move_to
    cairo_reset_clip
    cairo_restore
    cairo_rotate
    cairo_save
    cairo_set_dash
    cairo_set_line_cap
    cairo_set_line_join
    cairo_set_line_width
    cairo_set_miter_limit
    cairo_set_operator
    cairo_set_source_rgb
    cairo_set_source_rgba
    cairo_status
    cairo_stroke
    cairo_surface_destroy
    cairo_surface_status
    cairo_xlib_surface_create
    cairo_xlib_surface_set_size

    g_object_unref  (glib)

    pango_cairo_create_layout
    pango_cairo_show_layout
    pango_font_description_free
    pango_font_description_new
    pango_font_description_set_family
    pango_font_description_set_size
    pango_font_description_set_style
    pango_font_description_set_weight
    pango_layout_get_line
    pango_layout_line_get_pixel_extents
    pango_layout_set_font_description
    pango_layout_set_text

*/

static void Cairo_Circle(double x, double y, double r,
			 const pGEcontext gc, pDevDesc dd);
static void Cairo_Clip(double x0, double x1, double y0, double y1,
		       pDevDesc dd);
static void Cairo_Line(double x1, double y1, double x2, double y2,
		       const pGEcontext gc, pDevDesc dd);
static void Cairo_MetricInfo(int c, const pGEcontext gc,
			     double* ascent, double* descent,
			     double* width, pDevDesc dd);
static void Cairo_NewPage(const pGEcontext gc, pDevDesc dd);
static void Cairo_Polygon(int n, double *x, double *y,
			  const pGEcontext gc, pDevDesc dd);
static void Cairo_Polyline(int n, double *x, double *y,
			   const pGEcontext gc, pDevDesc dd);
static void Cairo_Rect(double x0, double y0, double x1, double y1,
		       const pGEcontext gc, pDevDesc dd);
static double Cairo_StrWidth(const char *str, const pGEcontext gc,
			     pDevDesc dd);
static void Cairo_Text(double x, double y, const char *str,
		       double rot, double hadj,
		       const pGEcontext gc, pDevDesc dd);

static void Cairo_update(pX11Desc xd)
{
    cairo_set_source_surface (xd->xcc, xd->cs, 0, 0);
    cairo_paint(xd->xcc);
}

static void CairoColor(unsigned int col, pX11Desc xd)
{
    unsigned int alpha = R_ALPHA(col);
    double red, blue, green;

    red = R_RED(col)/255.0;
    green = R_GREEN(col)/255.0;
    blue = R_BLUE(col)/255.0;
    red = pow(red, RedGamma);
    green = pow(green, GreenGamma);
    blue = pow(blue, BlueGamma);
    
    cairo_set_source_rgba(xd->cc, red, green, blue, alpha/255.0); 
}


/* --> See "Notes on Line Textures" in GraphicsEngine.h
 *
 *	27/5/98 Paul - change to allow lty and lwd to interact:
 *	the line texture is now scaled by the line width so that,
 *	for example, a wide (lwd=2) dotted line (lty=2) has bigger
 *	dots which are more widely spaced.  Previously, such a line
 *	would have "dots" which were wide, but not long, nor widely
 *	spaced.
 */

static void CairoLineType(const pGEcontext gc, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    cairo_line_cap_t lcap = CAIRO_LINE_CAP_SQUARE;
    cairo_line_join_t ljoin = CAIRO_LINE_JOIN_ROUND;
    switch(gc->lend){
    case GE_ROUND_CAP: lcap = CAIRO_LINE_CAP_ROUND; break;
    case GE_BUTT_CAP: lcap = CAIRO_LINE_CAP_BUTT; break;
    case GE_SQUARE_CAP: lcap = CAIRO_LINE_CAP_SQUARE; break;
    }
    switch(gc->ljoin){
    case GE_ROUND_JOIN: ljoin = CAIRO_LINE_JOIN_ROUND; break;
    case GE_MITRE_JOIN: ljoin = CAIRO_LINE_JOIN_MITER; break;
    case GE_BEVEL_JOIN: ljoin = CAIRO_LINE_JOIN_BEVEL; break;
    } 
    cairo_set_line_width(cc, gc->lwd);
    cairo_set_line_cap(cc, lcap);
    cairo_set_line_join(cc, ljoin);
    cairo_set_miter_limit(cc, gc->lmitre);

    if (gc->lty == 0 || gc->lty == -1)
	cairo_set_dash(cc, 0, 0, 0);
    else {
	double ls[16]; /* max 16x4=64 bit */
	int l = 0, dt = gc->lty;
	while (dt > 0) {
	    ls[l] = (double)(dt&15);
	    dt >>= 4;
	    l++;
	}
	cairo_set_dash(cc, ls, l, 0);
    }
}

static void Cairo_Clip(double x0, double x1, double y0, double y1,
			pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (x1 < x0) { double h = x1; x1 = x0; x0 = h; };
    if (y1 < y0) { double h = y1; y1 = y0; y0 = h; };

    cairo_reset_clip(xd->cc);
    cairo_new_path(xd->cc);
    /* Add 1 per X11_Clip */
    cairo_rectangle(xd->cc, x0, y0, x1 - x0 + 1, y1 - y0 + 1);
    cairo_clip(xd->cc);
}


static void Cairo_NewPage(const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    cairo_reset_clip(xd->cc);
    xd->fill = R_OPAQUE(gc->fill) ? gc->fill: xd->canvas;
    CairoColor(xd->fill, xd);
    cairo_new_path(xd->cc);
    cairo_paint(xd->cc);
    Cairo_update(xd);
    /* Apparently needed */
    XSync(display, 0);
}

static void Cairo_Rect(double x0, double y0, double x1, double y1,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    cairo_new_path(xd->cc);
    cairo_rectangle(xd->cc, x0, y0, x1 - x0, y1 - y0);

    if (R_ALPHA(gc->fill) > 0) {
	cairo_set_antialias(xd->cc, CAIRO_ANTIALIAS_NONE);
	CairoColor(gc->fill, xd);
	cairo_fill_preserve(xd->cc);
	cairo_set_antialias(xd->cc, xd->antialias);
    }

    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	CairoColor(gc->col, xd);
	CairoLineType(gc, xd);
	cairo_stroke(xd->cc);
    }
}

static void Cairo_Circle(double x, double y, double r,
			 const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    cairo_new_path(xd->cc);
    /* FIXME: do we want +0.5, or minimum 0.5 or 1? */
    cairo_arc(xd->cc, x, y, r + 0.5, 0.0, 2 * M_PI);

    if (R_ALPHA(gc->fill) > 0) {
	cairo_set_antialias(xd->cc, CAIRO_ANTIALIAS_NONE);
	CairoColor(gc->fill, xd);
	cairo_fill_preserve(xd->cc);
	cairo_set_antialias(xd->cc, xd->antialias);
   }
    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	CairoColor(gc->col, xd);
	CairoLineType(gc, xd);
	cairo_stroke(xd->cc);
    }
}

static void Cairo_Line(double x1, double y1, double x2, double y2,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (R_ALPHA(gc->col) > 0) {
	CairoColor(gc->col, xd);
	CairoLineType(gc, xd);
	cairo_new_path(xd->cc);
	cairo_move_to(xd->cc, x1, y1);
	cairo_line_to(xd->cc, x2, y2);
	cairo_stroke(xd->cc);
    }
}

static void Cairo_Polyline(int n, double *x, double *y,
			   const pGEcontext gc, pDevDesc dd)
{
    int i;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;

    if (R_ALPHA(gc->col) > 0) {
	CairoColor(gc->col, xd);
	CairoLineType(gc, xd);
	cairo_new_path(xd->cc);
	cairo_move_to(xd->cc, x[0], y[0]);
	for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
	cairo_stroke(xd->cc);
    }
}

static void Cairo_Polygon(int n, double *x, double *y,
			  const pGEcontext gc, pDevDesc dd)
{
    int i;
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    cairo_new_path(xd->cc);
    cairo_move_to(xd->cc, x[0], y[0]);
    for(i = 0; i < n; i++) cairo_line_to(xd->cc, x[i], y[i]);
    cairo_close_path(xd->cc);

    if (R_ALPHA(gc->fill) > 0) {
	cairo_set_antialias(xd->cc, CAIRO_ANTIALIAS_NONE);
	CairoColor(gc->fill, xd);
	cairo_fill_preserve(xd->cc);
	cairo_set_antialias(xd->cc, xd->antialias);
    }
    if (R_ALPHA(gc->col) > 0 && gc->lty != -1) {
	CairoColor(gc->col, xd);
	CairoLineType(gc, xd);
	cairo_stroke(xd->cc);
    }
}

static PangoFontDescription *getFont(const pGEcontext gc)
{
    PangoFontDescription *fontdesc;
    gint face = gc->fontface;
    double size = gc->cex * gc->ps;
    
    if (face < 1 || face > 5) face = 1;
	
    fontdesc = pango_font_description_new();
    if (face == 5)
	pango_font_description_set_family(fontdesc, "symbol");
    else {
	char *fm = gc->fontfamily;
	if(streql(fm, "mono")) fm = "courier";
	else if(streql(fm, "serif")) fm = "times";
	else if(streql(fm, "sans")) fm = "helvetica";
	pango_font_description_set_family(fontdesc, fm[0] ? fm : "helvetica");
	if(face == 2 || face == 4)
	    pango_font_description_set_weight(fontdesc, PANGO_WEIGHT_BOLD);
	if(face == 3 || face == 4)
	    pango_font_description_set_style(fontdesc, PANGO_STYLE_OBLIQUE);
    }
    pango_font_description_set_size(fontdesc, PANGO_SCALE * size);
	
    return fontdesc;
}

static PangoLayout 
*layoutText(PangoFontDescription *desc, cairo_t *cc, const char *str)
{
    PangoLayout *layout;
	
    layout = pango_cairo_create_layout(cc);
    pango_layout_set_font_description(layout, desc);
    pango_layout_set_text(layout, str, -1);
    return layout;
}

static void
text_extents(PangoFontDescription *desc, cairo_t *cc,
	     const pGEcontext gc, const gchar *str,
	     gint *lbearing, gint *rbearing, 
	     gint *width, gint *ascent, gint *descent, int ink)
{
    PangoLayout *layout;
    PangoRectangle rect, lrect;
	
    layout = layoutText(desc, cc, str);

    pango_layout_line_get_pixel_extents(pango_layout_get_line(layout, 0),
					&rect, &lrect);

    if(width) *width = lrect.width;
    if(ink) {
	if(ascent) *ascent = PANGO_ASCENT(rect);
	if(descent) *descent = PANGO_DESCENT(rect);
	if(lbearing) *lbearing = PANGO_LBEARING(rect);
	if(rbearing) *rbearing = PANGO_RBEARING(rect);	
    } else {
	if(ascent) *ascent = PANGO_ASCENT(lrect);
	if(descent) *descent = PANGO_DESCENT(lrect);
	if(lbearing) *lbearing = PANGO_LBEARING(lrect);
	if(rbearing) *rbearing = PANGO_RBEARING(lrect);
    }
    g_object_unref(layout);
}

static void Cairo_MetricInfo(int c, const pGEcontext gc,
			   double* ascent, double* descent,
			   double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    char str[16];
    int Unicode = mbcslocale;
    PangoFontDescription *desc = getFont(gc);
    gint iascent, idescent, iwidth;
	
    if(c == 0) c = 77;
    if(c < 0) {c = -c; Unicode = 1;}

    if(Unicode) {
	Rf_ucstoutf8(str, (unsigned int) c);
    } else {
	str[0] = c; str[1] = 0;
	/* Here, we assume that c < 256 */
    }
    text_extents(desc, xd->cc, gc, str, NULL, NULL, 
		 &iwidth, &iascent, &idescent, 1);
    *ascent = iascent;
    *descent = idescent;
    *width = iwidth;
#if 0
    printf("c = %d, '%s', face %d %f %f %f\n", 
	   c, str, gc->fontface, *width, *ascent, *descent);
#endif
}


static double Cairo_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint width;
    PangoFontDescription *desc = getFont(gc);

    text_extents(desc, xd-> cc, gc, str, NULL, NULL, &width, NULL, NULL, 0);
    pango_font_description_free(desc);
    return (double) width;
}

static void Cairo_Text(double x, double y,
		       const char *str, double rot, double hadj,
		       const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint ascent, lbearing, width;
    PangoLayout *layout;
    
    if (R_ALPHA(gc->col) > 0) {
	PangoFontDescription *desc = getFont(gc);
	cairo_save(xd->cc);
	text_extents(desc, xd->cc, gc, str, &lbearing, NULL, &width, 
		     &ascent, NULL, 0);
	cairo_move_to(xd->cc, x, y);
	if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	/* pango has a coord system at top left */
	cairo_rel_move_to(xd->cc, -lbearing - width*hadj, -ascent);
	CairoColor(gc->col, xd);
	layout = layoutText(desc, xd->cc, str);
	pango_cairo_show_layout(xd->cc, layout);
	cairo_restore(xd->cc);
	g_object_unref(layout);
	pango_font_description_free(desc);
    }
}

