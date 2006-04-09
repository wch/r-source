/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-5   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <R.h>
#include <Rinternals.h>
#include <Graphics.h>
#include <Rdevices.h>

#include "grDevices.h"
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType R_chull_t[] = {INTSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP};

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

static R_CMethodDef CEntries [] = {
    CDEF(R_chull),
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
    EXTDEF(devga, 15),
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
