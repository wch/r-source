/*
 *  R : A Computer Langage for Statistical Data Analysis
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

#define COMPLEX_DATA/* as in ./Defn.h */
#define PRINT_GAP	print_gap
#define PRINT_WIDTH	R_print_width

#define adj_right 1
#define adj_left  0

extern int  R_print_width;
extern SEXP print_na_string;
extern int  print_na_width;
extern int  print_quote;
extern int  print_digits;
extern int  print_gap;

/* Computation of printing formats */
void formatLogical(int*,int,int*);
void formatFactor(int*, int, int*, SEXP, int);
void formatInteger(int*,int,int*);
void formatReal(double*, int, int*, int*, int*);
void formatComplex(complex*, int, int*, int*, int*, int*, int*, int*);
void formatString(SEXP*, int, int*, int);

/* Formating of values */
char *EncodeLogical(int, int);
char *EncodeFactor(int, int, int, SEXP);
char *EncodeInteger(int, int);
char *EncodeReal(double, int, int, int);
char *EncodeComplex(complex, int, int, int, int, int, int);
char *EncodeString(char*, int, int, int);
char *EncodeElement(SEXP, int, int);

/* Printing */
int IndexWidth(int n);
void MatrixColumnLabel(SEXP, int, int);
void RightMatrixColumnLabel(SEXP, int, int);
void LeftMatrixColumnLabel(SEXP, int, int);
void MatrixRowLabel(SEXP, int, int);
void VectorIndex(int, int);

void CustomPrintValue(SEXP s ,SEXP env);
void PrintValue(SEXP);
void PrintValueRec(SEXP,SEXP);

/*  REprintf(char*, ...); ... --> all in Defn.h */

void printArray(SEXP, int);
void printDataFrame(SEXP);
void printMatrix(SEXP, int, SEXP, int, int);
void printNamedVector(SEXP, SEXP, int);
void printVector(SEXP, int, int);

#endif
