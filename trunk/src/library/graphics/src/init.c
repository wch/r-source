/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2017   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>

#include "graphics.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(C_contourDef, 0),
    CALLDEF(C_StemLeaf, 4),
    CALLDEF(C_BinCount, 4),
    CALLDEF(RunregisterBase, 0),
    {NULL, NULL, 0}
};


#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(C_contour, -1),
    EXTDEF(C_filledcontour, 5),
    EXTDEF(C_image, 4),
    EXTDEF(C_persp, -1),

    EXTDEF(C_abline, -1),
    EXTDEF(C_axis, -1),
    EXTDEF(C_arrows, -1),
    EXTDEF(C_box, -1),
    EXTDEF(C_clip, -1),
    EXTDEF(C_convertX, 3),
    EXTDEF(C_convertY, 3),
    EXTDEF(C_dend, -1),
    EXTDEF(C_dendwindow, -1),
    EXTDEF(C_erase, -1),
    EXTDEF(C_layout, -1),
    EXTDEF(C_mtext, -1),
    EXTDEF(C_par, -1),
    EXTDEF(C_path, -1),
    EXTDEF(C_plotXY, -1),
    EXTDEF(C_plot_window, -1),
    EXTDEF(C_polygon, -1),
    EXTDEF(C_raster, -1),
    EXTDEF(C_rect, -1),
    EXTDEF(C_segments, -1),
    EXTDEF(C_strHeight, -1),
    EXTDEF(C_strWidth, -1),
    EXTDEF(C_symbols, -1),
    EXTDEF(C_text, -1),
    EXTDEF(C_title, -1),
    EXTDEF(C_xspline, -1),

    EXTDEF(C_plot_new, 0),
    EXTDEF(C_locator, -1),
    EXTDEF(C_identify, -1),
    {NULL, NULL, 0}
};


void attribute_visible
R_init_graphics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
    registerBase();
}
