/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2019	     The R Foundation
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

/* This should be regarded as part of the graphics engine */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <R_ext/GraphicsEngine.h>

/*
 * C API for graphics devices to interrogate gradient SEXPs
 */

#define gradient_x1 0
#define gradient_y1 1
#define gradient_x2 2
#define gradient_y2 3
#define gradient_stops 4
#define gradient_colours 5
#define gradient_extend 6

double R_GE_gradientX1(SEXP gradient)
{
    return REAL(VECTOR_ELT(gradient, gradient_x1))[0];
}

double R_GE_gradientY1(SEXP gradient)
{
    return REAL(VECTOR_ELT(gradient, gradient_y1))[0];
}

double R_GE_gradientX2(SEXP gradient)
{
    return REAL(VECTOR_ELT(gradient, gradient_x2))[0];
}

double R_GE_gradientY2(SEXP gradient)
{
    return REAL(VECTOR_ELT(gradient, gradient_y2))[0];
}

int R_GE_gradientNumStops(SEXP gradient) 
{
    return LENGTH(VECTOR_ELT(gradient, gradient_stops));
}

double R_GE_gradientStop(SEXP gradient, int i) 
{
    return REAL(VECTOR_ELT(gradient, gradient_stops))[i];
}

rcolor R_GE_gradientColour(SEXP gradient, int i) 
{
    return RGBpar(VECTOR_ELT(gradient, gradient_colours), i);
}

int R_GE_gradientExtend(SEXP gradient) 
{
    return INTEGER(VECTOR_ELT(gradient, gradient_extend))[0];
}
