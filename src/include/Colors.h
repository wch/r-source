/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2008  R Core Team
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

#ifndef COLORS_H_
#define COLORS_H_

/* This is a private header, possibly temporary */

#define COLOR_TABLE_SIZE 1024

/* always remap private functions */
#define isNAcol			Rf_isNAcol

/* hsv2rgb and rgb2hsv are in Utils.h */

		/* Miscellaneous (from colors.c) */

Rboolean isNAcol(SEXP col, int index, int ncol); /* used in plot[3d].c */


#ifdef UNUSED
#define char2col		Rf_char2col
#define CheckColor		Rf_CheckColor
#define rgb2col			Rf_rgb2col
#define RGB2rgb			Rf_RGB2rgb
#define RGBA2rgb		Rf_RGBA2rgb
#define ScaleColor		Rf_ScaleColor
unsigned int char2col(const char *); /* rgb2col() or name2col() */
unsigned int rgb2col(const char *);
unsigned int ScaleColor(double x);
unsigned int CheckColor(int x);
char *RGB2rgb(unsigned int, unsigned int, unsigned int);
char *RGBA2rgb(unsigned int, unsigned int, unsigned int, unsigned int);
#endif

#endif

