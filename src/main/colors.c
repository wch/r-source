/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2012  The R Core Team
 *  Copyright (C) 2003	     The R Foundation
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/GraphicsEngine.h>
//#include <R_ext/Rdynload.h>

//DL_FUNC ptr_RGBpar3, ptr_col2name, ptr_R_GE_str2col;


typedef unsigned int (*F1)(SEXP x, int i, unsigned int bg);
typedef const char * (*F2)(unsigned int col);
typedef unsigned int (*F3)(const char *s);

static F1 ptr_RGBpar3;
static F2 ptr_col2name;
static F3 ptr_R_GE_str2col;

void Rg_set_col_ptrs(F1 f1, F2 f2, F3 f3)
{
    ptr_RGBpar3 = f1;
    ptr_col2name = f2;
    ptr_R_GE_str2col = f3;
}

/* used in grid/src/gpar.c with bg = R_TRANWHITE,
   in packages Cairo, canvas and jpeg */
/* in GraphicsEngine.h */
unsigned int RGBpar3(SEXP x, int i, unsigned int bg)
{
    if (!ptr_RGBpar3) error("package grDevices must be loaded");
    return (unsigned int)(ptr_RGBpar3)(x, i, bg);
}

/* in GraphicsEngine.h, used by devices */
unsigned int RGBpar(SEXP x, int i)
{
    return RGBpar3(x, i, R_TRANWHITE);
}

/* used in grid */
/* in GraphicsEngine.h */
const char *col2name(unsigned int col)
{
    if (!ptr_col2name) error("package grDevices must be loaded");
    return (const char *)(ptr_col2name)(col);
}

/* used in grDevices for fg and bg of devices */
/* in GraphicsEngine.h */
unsigned int R_GE_str2col(const char *s)
{
    if (!ptr_R_GE_str2col) error("package grDevices must be loaded");
    return (unsigned int)(ptr_R_GE_str2col)(s);
}
