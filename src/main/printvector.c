/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1997, 1998  Robert Gentleman and Ross Ihaka
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
 *  EXPORTS	printVector()
 *	     	printNamedVector()
 *	     	printRealVector()
 *	     	printIntegerVector()
 *	     	printComplexVector()
 *
 *  See ./printutils.c	 for remarks on Printing and the Encoding utils.
 *  See ./format.c	 for the formatXXXX functions used below.
 */

#include "Defn.h"
#include "Print.h"

static void printLogicalVector(int *x, int n, int index)
{
    int i, w, labwidth=0, width;

    if (index) {
	labwidth = IndexWidth(n) + 2;
	VectorIndex(1, labwidth);
	width = labwidth;
    }
    else width = 0;

    formatLogical(x, n, &w);
    w += PRINT_GAP;

    for (i = 0; i < n; i++) {
	if (width + w > PRINT_WIDTH) {
	    Rprintf("\n");
	    if (index) {
		VectorIndex(i + 1, labwidth);
		width = labwidth;
	    }
	    else
		width = 0;
	}
	Rprintf("%s", EncodeLogical(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

void printIntegerVector(int *x, int n, int index)
{
    int i, w, labwidth=0, width;

    if (index) {
	labwidth = IndexWidth(n) + 2;
	VectorIndex(1, labwidth);
	width = labwidth;
    }
    else width = 0;

    formatInteger(x, n, &w);
    w += PRINT_GAP;

    for (i = 0; i < n; i++) {
	if (width + w > PRINT_WIDTH) {
	    Rprintf("\n");
	    if (index) {
		VectorIndex(i + 1, labwidth);
		width = labwidth;
	    }
	    else
		width = 0;
	}
	Rprintf("%s", EncodeInteger(x[i], w));
	width += w;
    }
    Rprintf("\n");
}

void printRealVector(double *x, int n, int index)
{
    int i, w, d, e, labwidth=0, width;

    if (index) {
	labwidth = IndexWidth(n) + 2;
	VectorIndex(1, labwidth);
	width = labwidth;
    }
    else width = 0;

    formatReal(x, n, &w, &d, &e);
    w += PRINT_GAP;

    for (i = 0; i < n; i++) {
	if (width + w > PRINT_WIDTH) {
	    Rprintf("\n");
	    if (index) {
		VectorIndex(i + 1, labwidth);
		width = labwidth;
	    }
	    else
		width = 0;
	}
	Rprintf("%s", EncodeReal(x[i], w, d, e));
	width += w;
    }
    Rprintf("\n");
}

void printComplexVector(complex *x, int n, int index)
{
    int i, w, wr, dr, er, wi, di, ei, labwidth=0, width;

    if (index) {
	labwidth = IndexWidth(n) + 2;
	VectorIndex(1, labwidth);
	width = labwidth;
    }
    else width = 0;

    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei);

    w = wr + wi + 2;	/* +2 for "+" and "i" */
    w += PRINT_GAP;

    for (i = 0; i < n; i++) {
	if (width + w > PRINT_WIDTH) {
	    Rprintf("\n");
	    if (index) {
		VectorIndex(i + 1, labwidth);
		width = labwidth;
	    }
	    else
		width = 0;
	}
	if (ISNA(x[i].r) || ISNA(x[i].i)) {
	    Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0));
	}
	else {
	    Rprintf("%s", EncodeComplex(x[i], wr + PRINT_GAP , dr, er, wi, di, ei));
	}
	width += w;
    }
    Rprintf("\n");
}

static void printStringVector(SEXP * x, int n, int quote, int index)
{
    int i, w, labwidth=0, width;

    if (index) {
	labwidth = IndexWidth(n) + 2;
	VectorIndex(1, labwidth);
	width = labwidth;
    }
    else width = 0;

    formatString(x, n, &w, quote);

    for (i = 0; i < n; i++) {
	if (i > 0 && width + w + PRINT_GAP > PRINT_WIDTH) {
	    Rprintf("\n");
	    if (index) {
		VectorIndex(i + 1, labwidth);
		width = labwidth;
	    }
	    else
		width = 0;
	}
	Rprintf("%*s%s", PRINT_GAP, "", EncodeString(CHAR(x[i]), w, quote, adj_left));
	width += w + PRINT_GAP;
    }
    Rprintf("\n");
}

void printVector(SEXP x, int index, int quote)
{
    int n;

    if ((n = LENGTH(x)) != 0)
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printLogicalVector(LOGICAL(x), n, index);
	    break;
	case INTSXP:
	    printIntegerVector(INTEGER(x), n, index);
	    break;
	case REALSXP:
	    printRealVector(REAL(x), n, index);
	    break;
	case STRSXP:
	    if (quote)
		printStringVector(STRING(x), n, '"', index);
	    else
		printStringVector(STRING(x), n, 0, index);
	    break;
	case CPLXSXP:
	    printComplexVector(COMPLEX(x), n, index);
	    break;
	}
    else
	switch (TYPEOF(x)) {
	case LGLSXP:
	    Rprintf("logical(0)\n");
	    break;
	case INTSXP:
	case REALSXP:
	    Rprintf("numeric(0)\n");
	    break;
	case CPLXSXP:
	    Rprintf("complex(0)\n");
	    break;
	case STRSXP:
	    Rprintf("character(0)\n");
	    break;
	}
}

/* The following code prints vectors which have every element named */
/* Primitives for each type of vector are presented first, followed */
/* by the main (despatching) function */

static void printNamedLogicalVector(int * x, int n, SEXP * names)
{
    int i, j, k, nlines, nperline, w, wn;

    formatLogical(x, n, &w);
    formatString(names, n, &wn, 0);

    if (w < wn) w = wn;
    nperline = PRINT_WIDTH / (w + PRINT_GAP);
    if (nperline <= 0) nperline = 1;
    nlines = n / nperline;
    if (n % nperline) nlines += 1;

    for (i = 0; i < nlines; i++) {
	if (i) Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeString(CHAR(names[k]), w, 0, adj_right),
		    PRINT_GAP, "");
	}
	Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeLogical(x[k], w), PRINT_GAP, "");
	}
    }
    Rprintf("\n");
}

static void printNamedIntegerVector(int * x, int n, SEXP * names)
{
    int i, j, k, w, wn, nlines, nperline;

    formatInteger(x, n, &w);
    formatString(names, n, &wn, 0);
    if (w < wn) w = wn;
    nperline = PRINT_WIDTH / (w + PRINT_GAP);
    if (nperline <= 0) nperline = 1;
    nlines = n / nperline;
    if (n % nperline) nlines += 1;

    for (i = 0; i < nlines; i++) {
	if (i) Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeString(CHAR(names[k]), w, 0, adj_right),
		    PRINT_GAP, "");
	}
	Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)
	    Rprintf("%s%*s", EncodeInteger(x[k], w), PRINT_GAP, "");
    }
    Rprintf("\n");
}

static void printNamedRealVector(double * x, int n, SEXP * names)
{
    int i, j, k, w, wn, d, e, nlines, nperline;

    formatReal(x, n, &w, &d, &e);
    formatString(names, n, &wn, 0);
    if (w < wn) w = wn;
    nperline = PRINT_WIDTH / (w + PRINT_GAP);
    if (nperline <= 0) nperline = 1;
    nlines = n / nperline;
    if (n % nperline) nlines += 1;

    for (i = 0; i < nlines; i++) {
	if (i) Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeString(CHAR(names[k]), w, 0, adj_right), PRINT_GAP, "");
	}
	Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)
	    Rprintf("%s%*s", EncodeReal(x[k], w, d, e), PRINT_GAP, "");
    }
    Rprintf("\n");
}

static void printNamedComplexVector(complex *x, int n, SEXP *names)
{
    int i, j, k, w, wn, wr, dr, er, wi, di, ei, nlines, nperline;

    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei);
    w = wr + wi + 2;
    formatString(names, n, &wn, 0);
    if (w < wn) w = wn;
    nperline = PRINT_WIDTH / (w + PRINT_GAP);
    if (nperline <= 0) nperline = 1;
    nlines = n / nperline;
    if (n % nperline) nlines += 1;

    for (i = 0; i < nlines; i++) {
	if (i) Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeString(CHAR(names[k]), w, 0, adj_right), PRINT_GAP, "");
	}
	Rprintf("\n");
	for (j=0; j<nperline && (k =i*nperline+j) < n; j++) {
	    if (ISNA(x[j].r) || ISNA(x[j].i)) {
		Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0));
	    }
	    else {
		Rprintf("%*s%s", PRINT_GAP, "", EncodeReal(x[k].r, wr, dr, er));
#ifdef IEEE_754
		if(ISNAN(x[k].i))
		    Rprintf("+%si", "NaN");
		else
#endif
		    if(x[k].i >= 0)
			Rprintf("+%si", EncodeReal(x[k].i, wi, di, ei));
		    else
			Rprintf("-%si", EncodeReal(-x[k].i, wi, di, ei));
	    }
	}
    }
    Rprintf("\n");
}

static void printNamedStringVector(SEXP * x, int n, int quote, SEXP * names)
{
    int i, j, k, w, wn, nlines, nperline;


    formatString(x, n, &w, quote);
    formatString(names, n, &wn, 0);
    if (w < wn) w = wn;
    nperline = PRINT_WIDTH / (w + PRINT_GAP);
    if (nperline <= 0) nperline = 1;
    nlines = n / nperline;
    if (n % nperline) nlines += 1;

    for (i = 0; i < nlines; i++) {
	if (i) Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) {
	    Rprintf("%s%*s", EncodeString(CHAR(names[k]), w, 0, adj_right), PRINT_GAP, "");
	}
	Rprintf("\n");
	for (j = 0; j < nperline && (k = i * nperline + j) < n; j++)
	    Rprintf("%s%*s", EncodeString(CHAR(x[k]), w, quote, adj_right), PRINT_GAP, "");
    }
    Rprintf("\n");
}

void printNamedVector(SEXP x, SEXP names, int quote)
{
    int n;

    if ((n = LENGTH(x)) != 0)
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printNamedLogicalVector(LOGICAL(x), n, STRING(names));
	    break;
	case INTSXP:
	    printNamedIntegerVector(INTEGER(x), n, STRING(names));
	    break;
	case REALSXP:
	    printNamedRealVector(REAL(x), n, STRING(names));
	    break;
	case CPLXSXP:
	    printNamedComplexVector(COMPLEX(x), n, STRING(names));
	    break;
	case STRSXP:
	    if(quote) quote = '"';
	    printNamedStringVector(STRING(x), n, quote, STRING(names));
	    break;
	}
    else
	switch (TYPEOF(x)) {
	case LGLSXP:
	    Rprintf("logical(0)\n");
	    break;
	case INTSXP:
	    Rprintf("integer(0)\n");
	    break;
	case REALSXP:
	    Rprintf("real(0)\n");
	    break;
	case STRSXP:
	    Rprintf("character(0)\n");
	    break;
	}
}
