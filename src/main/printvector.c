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

/*== see ./printutils.c	 for general remarks on Printing and the Encode.. utils.
 *== see ./format.c	 for the  format_FOO_  functions used below.
 */
#include "Defn.h"
#include "Print.h"

static void printLogicalVector(int * x, int n, int index)
{
	int i, w, labwidth, width;

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

static void printFactorVector(int * x, int n, int index, SEXP levels, int nlev)
{
	int i, j, w, labwidth, width;
	char *lev;

	if (index) {
		labwidth = IndexWidth(n) + 2;
		VectorIndex(1, labwidth);
		width = labwidth;
	}
	else width = 0;

	formatString(STRING(levels), nlev, &w, 0);
	for (i = 0; i < n; i++) {
		if (x[i] < 1 || x[i] > nlev) {
			if (w < 2)
				w = 2;
			break;
		}
	}

	for (i = 0; i < n; i++) {
		if (width + w + PRINT_GAP > PRINT_WIDTH) {
			Rprintf("\n");
			if (index) {
				VectorIndex(i + 1, labwidth);
				width = labwidth;
			}
			else
				width = 0;
		}
		Rprintf("%*s", PRINT_GAP, "");
		j = x[i];
		if (1 <= j && j <= nlev) {
			Rprintf("%s", EncodeFactor(j, nlev, w, levels));
		}
		else
			Rprintf("%s", EncodeString(CHAR(NA_STRING), w, 0, adj_left));
		width += w + PRINT_GAP;
	}
	Rprintf("\n");
}

static void printIntegerVector(int * x, int n, int index)
{
	int i, w, labwidth, width;

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

void printRealVector(double * x, int n, int index)
{
	int i, w, d, e, labwidth, width;

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

#ifdef COMPLEX_DATA
void printComplexVector(complex *x, int n, int index)
{
	int i, w, wr, dr, er, wi, di, ei, labwidth, width;

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
		if(FINITE(x[i].r) && FINITE(x[i].i)) {
			Rprintf("%*s%s", PRINT_GAP, "", EncodeReal(x[i].r, wr, dr, er));
			if(x[i].i >= 0)
				Rprintf("+%si", EncodeReal(x[i].i, wi, di, ei));
			else
				Rprintf("-%si", EncodeReal(-x[i].i, wi, di, ei));
		}
		else Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0));
		width += w;
	}
	Rprintf("\n");
}
#endif

static void printStringVector(SEXP * x, int n, int quote, int index)
{
	int i, w, labwidth, width;

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
	SEXP l;
	int n;

	if ((n = LENGTH(x)) != 0)
		switch (TYPEOF(x)) {
		case LGLSXP:
			printLogicalVector(LOGICAL(x), n, index);
			break;
		case FACTSXP:
		case ORDSXP:
			if ((l = getAttrib(x, R_LevelsSymbol)) != R_NilValue
			    && TYPEOF(l) == STRSXP
			    && LENGTH(l) == LEVELS(x)) {
				printFactorVector(FACTOR(x), n, index, l, LEVELS(x));
			}
			else
				printIntegerVector(INTEGER(x), n, index);
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
#ifdef COMPLEX_DATA
		case CPLXSXP:
			printComplexVector(COMPLEX(x), n, index);
			break;
#endif
		}
	else
		switch (TYPEOF(x)) {
		case LGLSXP:
			Rprintf("logical(0)\n");
			break;
		case FACTSXP:
			Rprintf("unordered(0)\n");
			break;
		case ORDSXP:
			Rprintf("ordered(0)\n");
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
#ifdef COMPLEX_DATA
		case CPLXSXP:
			Rprintf("complex(0)\n");
			break;
#endif
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

static void printNamedFactorVector(int * x, int n, SEXP * names, SEXP * levels, int nlev)
{
	int i, j, k, l, w, wn, nlines, nperline;

	formatString(levels, nlev, &w, 0);
	for (i = 0; i < n; i++) {
		if (x[i] < 1 || x[i] > nlev)
			if (w < 2) w = 2;
	}
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
			l = x[k];
			if (1 <= l && l <= nlev)
				Rprintf("%s%*s", EncodeString(CHAR(levels[l - 1]), w, 0, adj_right), PRINT_GAP, "");
			else
				Rprintf("%s%*s", EncodeString(CHAR(NA_STRING), w, 0, adj_right), PRINT_GAP, "");
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

#ifdef COMPLEX_DATA
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
			if(FINITE(x[k].r) && FINITE(x[k].i)) {
				Rprintf("%*s%s", PRINT_GAP, "", EncodeReal(x[k].r, wr, dr, er));
				if(x[k].i >= 0)
					Rprintf("+%si", EncodeReal(x[k].i, wi, di, ei));
				else
					Rprintf("-%si", EncodeReal(-x[k].i, wi, di, ei));
			}
			else Rprintf("%s", EncodeReal(NA_REAL, w, 0, 0));
		}
	}
	Rprintf("\n");
}
#endif

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
	SEXP l;
	int n;

	if ((n = LENGTH(x)) != 0)
		switch (TYPEOF(x)) {
		case LGLSXP:
			printNamedLogicalVector(LOGICAL(x), n, STRING(names));
			break;
		case FACTSXP:
			if ((l = getAttrib(x, install("levels"))) != R_NilValue
			    && TYPEOF(l) == STRSXP
			    && LENGTH(l) == LEVELS(x)) {
				printNamedFactorVector(FACTOR(x), n, STRING(names), STRING(l), LEVELS(x));
			}
			else {
				printNamedIntegerVector(INTEGER(x), n, STRING(names));
			}
			break;
		case INTSXP:
			printNamedIntegerVector(INTEGER(x), n, STRING(names));
			break;
		case REALSXP:
			printNamedRealVector(REAL(x), n, STRING(names));
			break;
#ifdef COMPLEX_DATA
		case CPLXSXP:
			printNamedComplexVector(COMPLEX(x), n, STRING(names));
			break;
#endif
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
		case FACTSXP:
			Rprintf("unordered(0)\n");
			break;
		case ORDSXP:
			Rprintf("ordered(0)\n");
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
