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
 *
 * MUST match R structures in ../library/grDevices/R/patterns.R
 */

Rboolean R_GE_isPattern(SEXP x) {
    return Rf_inherits(x, "Pattern");
}

/* Pattern type is always component 0 */
int R_GE_patternType(SEXP pattern) 
{
    return INTEGER(VECTOR_ELT(pattern, 0))[0];
}

/* Linear gradients */
#define linear_gradient_x1 1
#define linear_gradient_y1 2
#define linear_gradient_x2 3
#define linear_gradient_y2 4
#define linear_gradient_stops 5
#define linear_gradient_colours 6
#define linear_gradient_extend 7

#define checkLinearGradient() \
    if (!(R_GE_patternType(pattern) == R_GE_linearGradientPattern)) \
        error(_("pattern is not a linear gradient"))

double R_GE_linearGradientX1(SEXP pattern)
{
    checkLinearGradient();
    return REAL(VECTOR_ELT(pattern, linear_gradient_x1))[0];
}

double R_GE_linearGradientY1(SEXP pattern)
{
    checkLinearGradient();
    return REAL(VECTOR_ELT(pattern, linear_gradient_y1))[0];
}

double R_GE_linearGradientX2(SEXP pattern)
{
    checkLinearGradient();
    return REAL(VECTOR_ELT(pattern, linear_gradient_x2))[0];
}

double R_GE_linearGradientY2(SEXP pattern)
{
    checkLinearGradient();
    return REAL(VECTOR_ELT(pattern, linear_gradient_y2))[0];
}

int R_GE_linearGradientNumStops(SEXP pattern) 
{
    checkLinearGradient();
    return LENGTH(VECTOR_ELT(pattern, linear_gradient_stops));
}

double R_GE_linearGradientStop(SEXP pattern, int i) 
{
    checkLinearGradient();
    return REAL(VECTOR_ELT(pattern, linear_gradient_stops))[i];
}

rcolor R_GE_linearGradientColour(SEXP pattern, int i) 
{
    checkLinearGradient();
    return RGBpar(VECTOR_ELT(pattern, linear_gradient_colours), i);
}

int R_GE_linearGradientExtend(SEXP pattern) 
{
    checkLinearGradient();
    return INTEGER(VECTOR_ELT(pattern, linear_gradient_extend))[0];
}

/* Radial gradients */
#define radial_gradient_cx1 1
#define radial_gradient_cy1 2
#define radial_gradient_r1 3
#define radial_gradient_cx2 4
#define radial_gradient_cy2 5
#define radial_gradient_r2 6
#define radial_gradient_stops 7
#define radial_gradient_colours 8
#define radial_gradient_extend 9

#define checkRadialGradient() \
    if (!(R_GE_patternType(pattern) == R_GE_radialGradientPattern)) \
        error(_("pattern is not a radial gradient"))

double R_GE_radialGradientCX1(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_cx1))[0];
}

double R_GE_radialGradientCY1(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_cy1))[0];
}

double R_GE_radialGradientR1(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_r1))[0];
}

double R_GE_radialGradientCX2(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_cx2))[0];
}

double R_GE_radialGradientCY2(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_cy2))[0];
}

double R_GE_radialGradientR2(SEXP pattern)
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_r2))[0];
}

int R_GE_radialGradientNumStops(SEXP pattern) 
{
    checkRadialGradient();
    return LENGTH(VECTOR_ELT(pattern, radial_gradient_stops));
}

double R_GE_radialGradientStop(SEXP pattern, int i) 
{
    checkRadialGradient();
    return REAL(VECTOR_ELT(pattern, radial_gradient_stops))[i];
}

rcolor R_GE_radialGradientColour(SEXP pattern, int i) 
{
    checkRadialGradient();
    return RGBpar(VECTOR_ELT(pattern, radial_gradient_colours), i);
}

int R_GE_radialGradientExtend(SEXP pattern) 
{
    checkRadialGradient();
    return INTEGER(VECTOR_ELT(pattern, radial_gradient_extend))[0];
}
