/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998, 1999   Robert Gentleman, Ross Ihaka 
 *                             and the R Development Core Team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 * Generally useful print utilities *NOT* relying on R internals (from Defn.h)
 */

#ifndef PRTUTIL_H_
#define PRTUTIL_H_

#include "Complex.h"
#include <stdarg.h>

#define PRINT_GAP	print_gap
#define PRINT_WIDTH	R_print_width

#define adj_right 1
#define adj_left  0

/* These should all be in a struct ! */
extern int  R_print_width;
extern int  print_na_width;
extern int  print_quote;
extern int  print_digits;
extern int  print_gap;

/* Computation of printing formats */
void formatLogical(int*,int,int*);
void formatInteger(int*,int,int*);
void formatReal(double*, int, int*, int*, int*);
void formatComplex(complex*, int, int*, int*, int*, int*, int*, int*);

long Decode2Long(char *p, int *ierr);
/* Formating of values */
char *EncodeLogical(int, int);
char *EncodeInteger(int, int);
char *EncodeReal(double, int, int, int);
char *EncodeComplex(complex, int, int, int, int, int, int);
char *EncodeString(char*, int, int, int);

/* Printing */
void VectorIndex(int, int);

void printIntegerVector(int *x,	   int n, int index);
void printRealVector   (double *x, int n, int index);
void printComplexVector(complex *x,int n, int index);

char *Rsprintf(char*, ...);
void Rprintf(char*, ...);
void REprintf(char*, ...);
void Rvprintf(const char*, va_list);
void REvprintf(const char*, va_list);

#endif
