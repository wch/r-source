/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R.h>
#include <Rinternals.h>

#include "graphics.h"
#include <R_ext/Rdynload.h>

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(FilledContour, 4),
    EXTDEF(Image, 4),
    EXTDEF(Persp, -1),
    {NULL, NULL, 0}
};


void R_init_graphics(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
}
