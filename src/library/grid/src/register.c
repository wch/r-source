/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3  Paul Murrell
 *                2003-2017 The R Core Team
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
#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
#include "grid.h"

#define LCALLDEF(name, n)  {#name, (DL_FUNC) &L_##name, n}

static const R_CallMethodDef callMethods[] = {
    LCALLDEF(initGrid, 1),
    LCALLDEF(killGrid, 0),
    LCALLDEF(gridDirty, 0),
    LCALLDEF(currentViewport, 0),
    LCALLDEF(setviewport, 2), 
    LCALLDEF(downviewport, 2), 
    LCALLDEF(downvppath, 3), 
    LCALLDEF(unsetviewport, 1), 
    LCALLDEF(upviewport, 1), 
    LCALLDEF(getDisplayList, 0),
    LCALLDEF(setDisplayList, 1),
    LCALLDEF(getDLelt, 1),
    LCALLDEF(setDLelt, 1),
    LCALLDEF(getDLindex, 0), 
    LCALLDEF(setDLindex, 1),
    LCALLDEF(getDLon, 0),
    LCALLDEF(setDLon, 1),
    LCALLDEF(getEngineDLon, 0),
    LCALLDEF(setEngineDLon, 1),
    LCALLDEF(getCurrentGrob, 0),
    LCALLDEF(setCurrentGrob, 1),
    LCALLDEF(getEngineRecording, 0),
    LCALLDEF(setEngineRecording, 1),
    LCALLDEF(currentGPar, 0),
    LCALLDEF(newpagerecording, 0),
    LCALLDEF(newpage, 0),
    LCALLDEF(initGPar, 0),
    LCALLDEF(initViewportStack, 0),
    LCALLDEF(initDisplayList, 0),
    LCALLDEF(moveTo, 2),
    LCALLDEF(lineTo, 3), 
    LCALLDEF(lines, 4), 
    LCALLDEF(segments, 5), 
    LCALLDEF(arrows, 12), 
    LCALLDEF(path, 4),
    LCALLDEF(polygon, 3),
    LCALLDEF(xspline, 7),
    LCALLDEF(circle, 3),
    LCALLDEF(rect, 6),
    LCALLDEF(raster, 8),
    LCALLDEF(cap, 0),
    LCALLDEF(text, 7),
    LCALLDEF(points, 4),
    LCALLDEF(clip, 6),
    LCALLDEF(pretty, 1),
    LCALLDEF(locator, 0),
    LCALLDEF(convert, 4),
    LCALLDEF(devLoc, 2),
    LCALLDEF(devDim, 2),
    LCALLDEF(layoutRegion, 2),
    LCALLDEF(getGPar, 0),
    LCALLDEF(setGPar, 1),
    LCALLDEF(circleBounds, 4),
    LCALLDEF(locnBounds, 3),
    LCALLDEF(rectBounds, 7),
    LCALLDEF(textBounds, 7),
    LCALLDEF(xsplineBounds, 8),
    LCALLDEF(xsplinePoints, 8),
    LCALLDEF(stringMetric, 1),
    {"validUnits", (DL_FUNC) &validUnits, 1},
    {"constructUnits", (DL_FUNC) &constructUnits, 3},
    {"asUnit", (DL_FUNC) &asUnit, 1},
    {"conformingUnits", (DL_FUNC) &conformingUnits, 1},
    {"matchUnit", (DL_FUNC) &matchUnit, 2},
    {"addUnits", (DL_FUNC) &addUnits, 2},
    {"multUnits", (DL_FUNC) &multUnits, 2},
    {"flipUnits", (DL_FUNC) &flipUnits, 1},
    {"absoluteUnits", (DL_FUNC) &absoluteUnits, 1},
    {"summaryUnits", (DL_FUNC) &summaryUnits, 2},
    { NULL, NULL, 0 }
};


void attribute_visible R_init_grid(DllInfo *dll) 
{
    /* No .C, .Fortran, or .External routines => NULL
     */
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, FALSE);
}
