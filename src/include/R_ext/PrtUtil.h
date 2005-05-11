/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2002    Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*
 * Generally useful print utilities *NOT* relying on R internals (from Defn.h)
 *
 * (not useful anymore; use  R_print struct with SEXP) --> Print.h
 */
#ifndef PRTUTIL_H_
#define PRTUTIL_H_

#include <R_ext/Complex.h>
#include <R_ext/Print.h>

#define formatLogical      Rf_formatLogical
#define formatInteger      Rf_formatInteger
#define formatReal         Rf_formatReal
#define formatComplex      Rf_formatComplex
#define EncodeLogical      Rf_EncodeLogical
#define EncodeInteger      Rf_EncodeInteger
#define EncodeReal         Rf_EncodeReal
#define EncodeComplex      Rf_EncodeComplex
#define VectorIndex        Rf_VectorIndex
#define printIntegerVector Rf_printIntegerVector
#define printRealVector    Rf_printRealVector
#define printComplexVector Rf_printComplexVector

#ifdef  __cplusplus
extern "C" {
#endif

/* Computation of printing formats */
void formatLogical(int*,int,int*);
void formatInteger(int*,int,int*);
void formatReal(double*, int, int*, int*, int*, int);
void formatComplex(Rcomplex*, int, int*, int*, int*, int*, int*, int*, int);

/* Formating of values */
char *EncodeLogical(int, int);
char *EncodeInteger(int, int);
char *EncodeReal(double, int, int, int, char);
char *EncodeComplex(Rcomplex, int, int, int, int, int, int, char);

/* Printing */
void VectorIndex(int, int);

void printLogicalVector(int *x,	   int n, int indx);
void printIntegerVector(int *x,	   int n, int indx);
void printRealVector   (double *x, int n, int indx);
void printComplexVector(Rcomplex *x,int n, int indx);

char *Rsprintf(char*, ...);
#ifdef  __cplusplus
}
#endif

#endif /* PRTUTIL_H_ */
