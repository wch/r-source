/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3  Paul Murrell
 *                2003-2016 The R Core Team
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

/* Code to register grid functions with R
 */
#include <R_ext/Rdynload.h>
#include "grid.h"

static const R_CallMethodDef callMethods[] = {
    {"initGrid", (DL_FUNC) &L_initGrid, 1},
    {"killGrid", (DL_FUNC) &L_killGrid, 0},
    {"gridDirty", (DL_FUNC) &L_gridDirty, 0},
    {"currentViewport", (DL_FUNC) &L_currentViewport, 0},
    {"setviewport", (DL_FUNC) &L_setviewport, 2}, 
    {"downviewport", (DL_FUNC) &L_downviewport, 2}, 
    {"downvppath", (DL_FUNC) &L_downvppath, 3}, 
    {"unsetviewport", (DL_FUNC) &L_unsetviewport, 1},  
    {"upviewport", (DL_FUNC) &L_upviewport, 1},  
    {"getDisplayList", (DL_FUNC) &L_getDisplayList, 0},
    {"setDisplayList", (DL_FUNC) &L_setDisplayList, 1},
    {"getDLelt", (DL_FUNC) &L_getDLelt, 1},
    {"setDLelt", (DL_FUNC) &L_setDLelt, 1},
    {"getDLindex", (DL_FUNC) &L_getDLindex, 0}, 
    {"setDLindex", (DL_FUNC) &L_setDLindex, 1},
    {"getDLon", (DL_FUNC) &L_getDLon, 0},
    {"setDLon", (DL_FUNC) &L_setDLon, 1},
    {"getEngineDLon", (DL_FUNC) &L_getEngineDLon, 0},
    {"setEngineDLon", (DL_FUNC) &L_setEngineDLon, 1},
    {"getCurrentGrob", (DL_FUNC) &L_getCurrentGrob, 0},
    {"setCurrentGrob", (DL_FUNC) &L_setCurrentGrob, 1},
    {"getEngineRecording", (DL_FUNC) &L_getEngineRecording, 0},
    {"setEngineRecording", (DL_FUNC) &L_setEngineRecording, 1},
    {"currentGPar", (DL_FUNC) &L_currentGPar, 0},
    {"newpagerecording", (DL_FUNC) &L_newpagerecording, 0},
    {"newpage", (DL_FUNC) &L_newpage, 0},
    {"initGPar", (DL_FUNC) &L_initGPar, 0},
    {"initViewportStack", (DL_FUNC) &L_initViewportStack, 0},
    {"initDisplayList", (DL_FUNC) &L_initDisplayList, 0},
    {"moveTo", (DL_FUNC) &L_moveTo, 2},
    {"lineTo", (DL_FUNC) &L_lineTo, 3}, 
    {"lines", (DL_FUNC) &L_lines, 4}, 
    {"segments", (DL_FUNC) &L_segments, 5}, 
    {"arrows", (DL_FUNC) &L_arrows, 12}, 
    {"path", (DL_FUNC) &L_path, 4},
    {"polygon", (DL_FUNC) &L_polygon, 3},
    {"xspline", (DL_FUNC) &L_xspline, 7},
    {"circle", (DL_FUNC) &L_circle, 3},
    {"rect", (DL_FUNC) &L_rect, 6},
    {"raster", (DL_FUNC) &L_raster, 8},
    {"cap", (DL_FUNC) &L_cap, 0},
    {"text", (DL_FUNC) &L_text, 7},
    {"points", (DL_FUNC) &L_points, 4},
    {"clip", (DL_FUNC) &L_clip, 6},
    {"pretty", (DL_FUNC) &L_pretty, 1},
    {"locator", (DL_FUNC) &L_locator, 0},
    {"convert", (DL_FUNC) &L_convert, 4},
    {"layoutRegion", (DL_FUNC) &L_layoutRegion, 2},
    {"validUnits", (DL_FUNC) &validUnits, 1},
    {"getGPar", (DL_FUNC) &L_getGPar, 0},
    {"setGPar", (DL_FUNC) &L_setGPar, 1},
    {"circleBounds", (DL_FUNC) &L_circleBounds, 4},
    {"locnBounds", (DL_FUNC) &L_locnBounds, 3},
    {"rectBounds", (DL_FUNC) &L_rectBounds, 7},
    {"textBounds", (DL_FUNC) &L_textBounds, 7},
    {"xsplineBounds", (DL_FUNC) &L_xsplineBounds, 8},
    {"xsplinePoints", (DL_FUNC) &L_xsplinePoints, 8},
    {"stringMetric", (DL_FUNC) &L_stringMetric, 1},
    { NULL, NULL, 0 }
};


void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_grid(DllInfo *dll) 
{
    /* No .C, .Fortran, or .External routines => NULL
     */
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, FALSE);
}
