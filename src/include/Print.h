/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#ifndef PRINT_H_
#define PRINT_H_

#include "PrtUtil.h"
#include "Defn.h"

extern SEXP print_na_string;

/* Computation of printing formats */
void formatString(SEXP*, int, int*, int);

/* Formating of values */
char *EncodeFactor(int, int, int, SEXP);
char *EncodeElement(SEXP, int, int);

/* Printing */
void MatrixColumnLabel(SEXP, int, int);
void RightMatrixColumnLabel(SEXP, int, int);
void LeftMatrixColumnLabel(SEXP, int, int);
void MatrixRowLabel(SEXP, int, int);

/* In Defn.h (and MUST be there):
   CustomPrintValue,  PrintValue, PrintValueRec */
void printArray(SEXP, SEXP, int, SEXP);
void printMatrix(SEXP, int, SEXP, int, int, SEXP, SEXP);
void printNamedVector(SEXP, SEXP, int);
void printVector(SEXP, int, int);

#endif
