/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2015  R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_DEV_CAIRO_H
#define R_DEV_CAIRO_H

#define SYMBOL_FONTFACE 5

typedef enum {
    WINDOW,
    XIMAGE,
    PNG,
    JPEG,
    TIFF,
    PNGdirect,
    SVG,
    PDF,
    PS,
    BMP
} X_GTYPE;


#include <stdio.h>

#ifdef HAVE_PANGOCAIRO
#  include <pango/pango.h>
#  include <pango/pangocairo.h>
#else
#  include <cairo.h>
#endif
#ifdef HAVE_CAIRO_SVG
#  include <cairo-svg.h>
# endif
#ifdef HAVE_CAIRO_PDF
#  include <cairo-pdf.h>
# endif
#ifdef HAVE_CAIRO_PS
#  include <cairo-ps.h>
# endif

typedef struct {
    /* Graphics Parameters */
    /* Local device copy so that we can detect */
    /* when parameter changes. */

    /* Used to detect changes */
    int lty;				/* Line type */
    double lwd;
    R_GE_lineend lend;
    R_GE_linejoin ljoin;

    double lwdscale;                    /* scaling to get a multiple
					   of 1/96" */

    int col;				/* Color */
    int fill;
    int bg;				/* bg */
    int canvas;				/* Canvas colour */
    int fontface;			/* Typeface 1:5 */
    int fontsize;			/* Size in points */
    double pointsize;			/* Size in points */
    char basefontfamily[500];           /* Initial font family */

    int windowWidth;			/* Window width (pixels) */
    int windowHeight;			/* Window height (pixels) */
    X_GTYPE type;			/* Window or pixmap? */
    int npages;				/* counter for a pixmap */
    FILE *fp;				/* file for a bitmap device */
    char filename[PATH_MAX];		/* filename for a bitmap device */
    int quality;			/* JPEG quality/TIFF compression */

    int res_dpi;			/* used for png/jpeg */
    double fallback_dpi;		/* used for ps/pdf */
    char title[101];
    Rboolean onefile;

    Rboolean useCairo, buffered;
    cairo_t *cc, *xcc;
    cairo_surface_t *cs, *xcs;
    cairo_antialias_t antialias;

    double fontscale;
} X11Desc;

typedef X11Desc* pX11Desc;
#endif
