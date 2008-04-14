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
    cairo_image_surface_create
    cairo_image_surface_get_data (1.2)
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
    cairo_set_antialias
    cairo_set_dash
    cairo_set_line_cap
    cairo_set_line_join
    cairo_set_line_width
    cairo_set_miter_limit
    cairo_set_operator
    cairo_set_source_rgb
    cairo_set_source_rgba
    cairo_set_source_surface
    cairo_status
    cairo_status_to_string
    cairo_stroke
    cairo_surface_destroy
    cairo_surface_status
    cairo_surface_write_to_png
    cairo_xlib_surface_create
    cairo_xlib_surface_set_size

    cairo_show_text
    cairo_text_extents

    cairo_pdf_surface_create (1.2)
    cairo_ps_surface_create  (1.2)
    cairo_svg_surface_create (1.2)

    cairo_ft_font_face_create_for_ft_face [OSX]

    g_object_unref  (glib)

    pango_cairo_create_layout (1.10)
    pango_cairo_show_layout (1.10)
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

static void Cairo_update(pX11Desc xd)
{
    /* We could first paint the canvas colour and 
       then the backing surface. */
    if(xd->xcc) {
	cairo_set_source_surface (xd->xcc, xd->cs, 0, 0);
	cairo_paint(xd->xcc);
    }
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
    
    /* These optimizations should not be necessary, but alpha = 1
       seems to cause image fallback in some backends */
    if (alpha == 255)
	cairo_set_source_rgb(xd->cc, red, green, blue); 
    else
	cairo_set_source_rgba(xd->cc, red, green, blue, alpha/255.0); 
}

static void CairoLineType(const pGEcontext gc, pX11Desc xd)
{
    cairo_t *cc = xd->cc;
    double lwd = gc->lwd;
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
    cairo_set_line_width(cc, (lwd > 0.01 ? lwd : 0.01) * xd->lwdscale);
    cairo_set_line_cap(cc, lcap);
    cairo_set_line_join(cc, ljoin);
    cairo_set_miter_limit(cc, gc->lmitre);

    if (gc->lty == 0 || gc->lty == -1)
	cairo_set_dash(cc, 0, 0, 0);
    else {
	double ls[16], lwd = (gc->lwd > 1) ? gc->lwd : 1;
	int l, dt = gc->lty;
	for (l = 0; dt != 0; dt >>= 4, l++)  
	    ls[l] = (dt & 0xF) * lwd * xd->lwdscale;
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
    /* radius 0.5 seems to be visible */
    cairo_arc(xd->cc, x, y, (r > 0.5 ? r : 0.5), 0.0, 2 * M_PI);

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

#ifdef HAVE_PANGOCAIRO
/* ------------- pangocairo section --------------- */

static PangoFontDescription *PG_getFont(const pGEcontext gc)
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
*PG_layout(PangoFontDescription *desc, cairo_t *cc, const char *str)
{
    PangoLayout *layout;
	
    layout = pango_cairo_create_layout(cc);
    pango_layout_set_font_description(layout, desc);
    pango_layout_set_text(layout, str, -1);
    return layout;
}

static void
PG_text_extents(cairo_t *cc, PangoLayout *layout,
		gint *lbearing, gint *rbearing, 
		gint *width, gint *ascent, gint *descent, int ink)
{
    PangoRectangle rect, lrect;

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
}

static void 
PangoCairo_MetricInfo(int c, const pGEcontext gc,
		      double* ascent, double* descent,
		      double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    char str[16];
    int Unicode = mbcslocale;
    PangoFontDescription *desc = PG_getFont(gc);
    PangoLayout *layout;
    gint iascent, idescent, iwidth;
	
    if(c == 0) c = 77;
    if(c < 0) {c = -c; Unicode = 1;}

    if(Unicode) {
	Rf_ucstoutf8(str, (unsigned int) c);
    } else {
	/* Here we assume that c < 256 */
	str[0] = c; str[1] = 0;
    }
    layout = PG_layout(desc, xd->cc, str);
    PG_text_extents(xd->cc, layout, NULL, NULL, &iwidth, 
		    &iascent, &idescent, 1);
    g_object_unref(layout);
    pango_font_description_free(desc);
    *ascent = iascent;
    *descent = idescent;
    *width = iwidth;
#if 0
    printf("c = %d, '%s', face %d %f %f %f\n", 
	   c, str, gc->fontface, *width, *ascent, *descent);
#endif
}


static double
PangoCairo_StrWidth(const char *str, const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint width;
    PangoFontDescription *desc = PG_getFont(gc);
    PangoLayout *layout = PG_layout(desc, xd->cc, str);

    PG_text_extents(xd->cc, layout, NULL, NULL, &width, NULL, NULL, 0);
    g_object_unref(layout);
    pango_font_description_free(desc);
    return (double) width;
}

static void
PangoCairo_Text(double x, double y,
		const char *str, double rot, double hadj,
		const pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    gint ascent, lbearing, width;
    PangoLayout *layout;
    
    if (R_ALPHA(gc->col) > 0) {
	PangoFontDescription *desc = PG_getFont(gc);
	cairo_save(xd->cc);
	layout = PG_layout(desc, xd->cc, str);
	PG_text_extents(xd->cc, layout, &lbearing, NULL, &width,
			&ascent, NULL, 0);
	cairo_move_to(xd->cc, x, y);
	if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	/* pango has a coord system at top left */
	cairo_rel_move_to(xd->cc, -lbearing - width*hadj, -ascent);
	CairoColor(gc->col, xd);
	pango_cairo_show_layout(xd->cc, layout);
	cairo_restore(xd->cc);
	g_object_unref(layout);
	pango_font_description_free(desc);
    }
}

#else
/* ------------- cairo-ft section --------------- */

/* FIXME: although this should work on all platforms, I didn't get to
   test it (yet) anywhere else, hence the __APPLE__ condition for now [SU] 
   r44621: now works on Linux, just finds the same fonts as before. [BDR]
*/
#if CAIRO_HAS_FT_FONT && __APPLE__

/* FT implies FC in Cairo */
#include <cairo-ft.h>

/* cairo font cache - to prevent unnecessary font look ups */
typedef struct Rc_font_cache_s {
    const char *family;
    int face;
    cairo_font_face_t *font;
    struct Rc_font_cache_s *next;
} Rc_font_cache_t;

static Rc_font_cache_t *cache, *cache_tail;

static cairo_font_face_t *Rc_findFont(const char *family, int face)
{
    Rc_font_cache_t *here = cache;
    while (here) {
	if (here->face == face && streql(here->family, family))
	    return here->font;
	here = here->next;
    }
    return NULL;
}

static void Rc_addFont(const char *family, int face, cairo_font_face_t* font)
{
    Rc_font_cache_t *fc = (Rc_font_cache_t*) malloc(sizeof(Rc_font_cache_t));
    if (!fc) return;
    fc->family = strdup(family);
    fc->face = face;
    fc->font = font;
    fc->next = NULL;
    if (cache)
	cache_tail = cache_tail->next = fc;
    else
	cache = cache_tail = fc;
}

/* FC patterns to append to font family names */
static const char *face_styles[4] = {
    ":style=Regular", 
    ":style=Bold",
    ":style=Italic", 
    ":style=Bold Italic,BoldItalic"
};

static int fc_loaded;
static FT_Library ft_library;

/* use FC to find a font, load it in FT and return the Cairo FT font face */
static cairo_font_face_t *FC_getFont(const char *family, int style) 
{
    FcFontSet *fs;
    FcPattern *pat, *match;
    FcResult  result;
    FcChar8   *file;
    char      fcname[250]; /* 200 for family + 50 for style */

    /* find candidate fonts via FontConfig */
    if (!fc_loaded) {
        if (!FcInit()) return NULL;
	fc_loaded = 1;
    }
    style &= 3;
    strcpy(fcname, family);
    strcat(fcname, face_styles[style]);
    pat = FcNameParse((FcChar8 *)fcname);
    if (!pat) return NULL;
    FcConfigSubstitute (0, pat, FcMatchPattern);
    FcDefaultSubstitute (pat);
    fs = FcFontSetCreate ();
    match = FcFontMatch (0, pat, &result);
    FcPatternDestroy (pat);
    if (!match) { 
        FcFontSetDestroy (fs);
	return NULL;
    }
    FcFontSetAdd (fs, match);

    /* then try to load the font into FT */
    if (fs) {
	int j = 0, index = 0;
	while (j < fs->nfont) { /* find the font file + face index and use it with FreeType */
	    if (FcPatternGetString (fs->fonts[j], FC_FILE, 0, &file) 
		== FcResultMatch &&
		FcPatternGetInteger(fs->fonts[j], FC_INDEX, 0, &index)
		== FcResultMatch) {
	        FT_Face face;
		if (!ft_library && FT_Init_FreeType(&ft_library)) {
		    FcFontSetDestroy (fs);  
		    return NULL;
		}
		/* some FreeType versions have broken index support, fall back to index 0 */
		if (!FT_New_Face(ft_library, (const char *) file, index, &face) ||
		    (index && !FT_New_Face(ft_library, (const char *) file, 0, &face))) {
		    FcFontSetDestroy (fs);
#ifdef __APPLE__ /* FreeType is broken on OS X in that face index is often wrong (unfortunately
		    even for Helvetica!) - we try to find the best match through enumeration */
		    if (face->num_faces > 1 && (face->style_flags & 3) != style) {
			FT_Face alt_face;
			int i = 0;
			while (i < face->num_faces)
			    if (!FT_New_Face(ft_library, (const char *) file, i++, &alt_face)) {
				if ((alt_face->style_flags & 3) == style) {
				    FT_Done_Face(face);
				    face = alt_face;
				    break;
				} else
				    FT_Done_Face(alt_face);
			    }
		    }
#endif
		    return cairo_ft_font_face_create_for_ft_face(face, FT_LOAD_DEFAULT);
		}
	    }
	    j++;
	}
	FcFontSetDestroy (fs);
    }
    return NULL;
}

static void FT_getFont(pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int face = gc->fontface;
    double size = gc->cex * gc->ps;
    cairo_font_face_t *cairo_face = NULL;
    const char *family;

    if (face < 1 || face > 5) face = 1;
    family = gc->fontfamily;
    if (face == 5) {
	if (!*family) family = "Symbol";
    } else {
	if (!*family || streql(family, "sans")) family = "Helvetica";
	else if (streql(family, "serif")) family = "Times";
	else if (streql(family, "mono")) family = "Courier";
    }
    cairo_face = Rc_findFont(family, face);
    if (!cairo_face) {
	cairo_face = FC_getFont(family, face - 1);
	if (!cairo_face) return;
	Rc_addFont(family, face, cairo_face);
    }
    cairo_set_font_face (xd->cc, cairo_face);
    /* FIXME: this should really use a matrix if pixels are non-square */
    cairo_set_font_size (xd->cc, size);
}

#else

static void FT_getFont(pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    int face = gc->fontface;
    double size = gc->cex * gc->ps;
    char *family = "Helvetica";
    int slant = CAIRO_FONT_SLANT_NORMAL, wt  = CAIRO_FONT_WEIGHT_NORMAL;

    char *fm = gc->fontfamily;
    if(streql(fm, "mono")) family = "courier";
    else if(streql(fm, "serif")) family = "times";
    else if(streql(fm, "sans")) family = "helvetica";
    else if(fm[0]) family = fm;
    if (face < 1 || face > 5) face = 1;
    if (face == 5) family = "Symbol";
    if (face == 2 || face == 4) wt = CAIRO_FONT_WEIGHT_BOLD;
    if (face == 3 || face == 4) slant = CAIRO_FONT_SLANT_ITALIC;
  
    cairo_select_font_face (xd->cc, family, slant, wt);
    /* FIXME: this should really use a matrix if pixels are non-square */
    cairo_set_font_size (xd->cc, size);
}
#endif

static void Cairo_MetricInfo(int c, pGEcontext gc,
			  double* ascent, double* descent,
			  double* width, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    cairo_text_extents_t exts;
    char str[16];
    int Unicode = mbcslocale;
	
    if(c == 0) c = 77;
    if(c < 0) {c = -c; Unicode = 1;}

    if(Unicode) {
	Rf_ucstoutf8(str, (unsigned int) c);
    } else {
	/* Here, we assume that c < 256 */
	str[0] = c; str[1] = 0;
    }
    
    FT_getFont(gc, dd);
    cairo_text_extents(xd->cc, str, &exts);
    *ascent  = -exts.y_bearing; 
    *descent = exts.height + exts.y_bearing;
    *width = exts.x_advance;
#if 0
    printf("c = %d, '%s', face %d %f %f %f\n", 
	   c, str, gc->fontface, *width, *ascent, *descent);
#endif
}

static double Cairo_StrWidth(const char *str, pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    cairo_text_extents_t exts;

    FT_getFont(gc, dd);
    cairo_text_extents(xd->cc, str, &exts);
    return exts.x_advance;
}

static void Cairo_Text(double x, double y,
		    const char *str, double rot, double hadj,
		    pGEcontext gc, pDevDesc dd)
{
    pX11Desc xd = (pX11Desc) dd->deviceSpecific;
    
    if (R_ALPHA(gc->col) > 0) {
	cairo_save(xd->cc);
	FT_getFont(gc, dd);
	cairo_move_to(xd->cc, x, y);
	if (hadj != 0.0 || rot != 0.0) {
	    cairo_text_extents_t te;
	    cairo_text_extents(xd->cc, str, &te);
	    if (rot != 0.0) cairo_rotate(xd->cc, -rot/180.*M_PI);
	    if (hadj != 0.0) 
		cairo_rel_move_to(xd->cc, -te.x_advance * hadj, 0);
	}
	CairoColor(gc->col, xd);
	cairo_show_text(xd->cc, str);
	cairo_restore(xd->cc);
    }
}
#endif
