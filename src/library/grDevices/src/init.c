/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-8   The R Development Core Team.
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

#include <R.h>
#include <Rinternals.h>
#include <R_ext/GraphicsEngine.h>

#include "grDevices.h"
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType R_chull_t[] = {INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP};

#ifndef WIN32
void *getQuartzAPI();
#endif

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

static R_CMethodDef CEntries [] = {
    CDEF(R_chull),
    {"getQuartzAPI", (DL_FUNC) getQuartzAPI, 0},
    {NULL, NULL, 0}
};

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(Type1FontInUse, 2),
    CALLDEF(CIDFontInUse, 2),
    {"R_GD_nullDevice", (DL_FUNC) &R_GD_nullDevice, 0},
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(PicTeX, 6),
    EXTDEF(PostScript, 16),
    EXTDEF(XFig, 11),
    EXTDEF(PDF, 13),
#ifdef WIN32
    EXTDEF(devga, 16),
    EXTDEF(savePlot, 3),
#else
    EXTDEF(Quartz, -1),
#endif
    {NULL, NULL, 0}
};

void R_init_grDevices(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
}
