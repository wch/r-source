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

static void printLogicalMatrix(SEXP sx, int offset, int r, int c, SEXP rl, SEXP cl)
{
	SEXP sw;
	int *x, *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	sw = allocVector(INTSXP, c);
	x = INTEGER(sx)+offset;
	w = INTEGER(sw);

	for (j=0; j<c; j++) {
		formatLogical(&x[j * r], r, &w[j]);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;	/* changed j+1 to c and back */
		if (w[j] < clabw) w[j] = clabw;
		w[j] += PRINT_GAP;
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax];
			jmax++;
		}while(jmax < c && width+w[jmax] < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
		for(j=jmin; j<jmax ; j++)
			MatrixColumnLabel(cl, j, w[j]);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = jmin; j < jmax; j++) {
				Rprintf("%s",
					EncodeLogical(x[i+j*r], w[j]));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}
}

static void printFactorMatrix(SEXP sx, int offset, int r, int c, SEXP rl, SEXP cl, SEXP levels, int nlev)
{
	SEXP sw;
	int *x, *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax, k;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	sw = allocVector(INTSXP, c);
	x = INTEGER(sx)+offset;
	w = INTEGER(sw);

	for (j=0; j<c; j++) {
		formatFactor(&x[j * r], r, &w[j], levels, nlev);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;		/* replaced j+1 by c and back */
		if (w[j] < clabw) w[j] = clabw;
		/* w[j] += PRINT_GAP; */
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax]+PRINT_GAP;
			jmax++;
		}while(jmax < c && width+PRINT_GAP+w[jmax] < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
			for(j=jmin; j<jmax ; j++)
				MatrixColumnLabel(cl, j, w[j]+PRINT_GAP);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = jmin; j < jmax; j++) {
				Rprintf("%*s%s",PRINT_GAP, " ",
					EncodeFactor(x[i+j*r], nlev, w[j], levels));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}

}

static void printIntegerMatrix(SEXP sx, int offset, int r, int c, SEXP rl, SEXP cl)
{
	SEXP sw;
	int *x, *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	sw = allocVector(INTSXP, c);
	x = INTEGER(sx)+offset;
	w = INTEGER(sw);

	for (j=0; j<c; j++) {
		formatInteger(&x[j * r], r, &w[j]);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;		/* replaced j+1 by c and back */
		if (w[j] < clabw) w[j] = clabw;
		w[j] += PRINT_GAP;
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax];
			jmax++;
		}while(jmax < c && width+w[jmax] < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
		for(j=jmin; j<jmax ; j++)
			MatrixColumnLabel(cl, j, w[j]);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = jmin; j < jmax; j++) {
				Rprintf("%s",
					EncodeInteger(x[i+j*r], w[j]));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}
}

static void printRealMatrix(SEXP sx, int offset, int r, int c, SEXP rl, SEXP cl)
{
	SEXP sd, se, sw;
	double *x;
	int *d, *e, *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	PROTECT(sd = allocVector(INTSXP, c));
	PROTECT(se = allocVector(INTSXP, c));
	sw = allocVector(INTSXP, c);
	UNPROTECT(2);
	x = REAL(sx)+offset;
	d = INTEGER(sd);
	e = INTEGER(se);
	w = INTEGER(sw);

	for (j=0; j<c; j++) {
		formatReal(&x[j * r], r, &w[j], &d[j], &e[j]);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;	/* replaced j+1 by c and back */
		if (w[j] < clabw) w[j] = clabw;
		w[j] += PRINT_GAP;
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax];
			jmax++;
		}while(jmax < c && width+w[jmax] < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
		for(j=jmin; j<jmax ; j++)
			MatrixColumnLabel(cl, j, w[j]);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = jmin; j < jmax; j++) {
				Rprintf("%s",
					EncodeReal(x[i+j*r], w[j], d[j], e[j]));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}
}

#ifdef COMPLEX_DATA
static void printComplexMatrix(SEXP sx, int offset, int r, int c, SEXP rl, SEXP cl)
{
	SEXP sdr, ser, swr, sdi, sei, swi, sw;
	complex *x;
	int *dr, *er, *wr, *di, *ei, *wi, *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	PROTECT(sdr = allocVector(INTSXP, c));
	PROTECT(ser = allocVector(INTSXP, c));
	PROTECT(swr = allocVector(INTSXP, c));
	PROTECT(sdi = allocVector(INTSXP, c));
	PROTECT(sei = allocVector(INTSXP, c));
	PROTECT(swi = allocVector(INTSXP, c));
	PROTECT(sw = allocVector(INTSXP, c));
	UNPROTECT(7);
	x = COMPLEX(sx)+offset;
	dr = INTEGER(sdr);
	er = INTEGER(ser);
	wr = INTEGER(swr);
	di = INTEGER(sdi);
	ei = INTEGER(sei);
	wi = INTEGER(swi);
	w = INTEGER(sw);

		/* Determine the column widths */

	for (j=0; j<c; j++) {
		formatComplex(&x[j * r], r, &wr[j], &dr[j], &er[j],
			&wi[j], &di[j], &ei[j]);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;
		w[j] = wr[j] + wi[j] + 2;
		if (w[j] < clabw) w[j] = clabw;
		w[j] += PRINT_GAP;
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax];
			jmax++;
		} while(jmax < c && width+w[jmax] < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
		for(j=jmin; j<jmax ; j++)
			MatrixColumnLabel(cl, j, w[j]);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j=jmin; j<jmax; j++) {
				if(FINITE(x[i+j*r].r) && FINITE(x[i+j*r].i)) {
					Rprintf("%*s%s", PRINT_GAP, "", EncodeReal(x[i+j*r].r, wr[j], dr[j], er[j]));
					if(x[i+j*r].i >= 0)
						Rprintf("+%si", EncodeReal(x[i+j*r].i, wi[j], di[j], ei[j]));
					else
						Rprintf("-%si", EncodeReal(-x[i+j*r].i, wi[j], di[j], ei[j]));
				}
				else Rprintf("%s", EncodeReal(NA_REAL, w[j], 0, 0));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}
}
#endif

static void printStringMatrix(SEXP sx, int offset, int r, int c, int quote, SEXP rl, SEXP cl)
{
	SEXP sw;
	SEXP *x;
	int *w;
	int width, rlabw, clabw;
	int i, j, jmin, jmax;

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	sw = allocVector(INTSXP, c);
	x = STRING(sx)+offset;
	w = INTEGER(sw);

	for (j=0; j<c; j++) {
		formatString(&x[j * r], r, &w[j], quote);
		if (!isNull(cl)) clabw = strlen(CHAR(STRING(cl)[j]));
		else clabw = IndexWidth(j+1) + 3;	/* replaced j+1 by c and back */
		if (w[j] < clabw) w[j] = clabw;
		/* w[j] += PRINT_GAP; */
	}

	jmin = 0;
	jmax = 0;
	while(jmin < c) {
		width = rlabw;
		do {
			width += w[jmax]+PRINT_GAP;
			jmax++;
		}while(jmax < c && width+w[jmax]+PRINT_GAP < PRINT_WIDTH);

		Rprintf("%*s", rlabw, " ");
		for(j=jmin; j<jmax ; j++)
			LeftMatrixColumnLabel(cl, j, w[j]);
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = jmin; j < jmax; j++) {
				Rprintf("%*s%s", PRINT_GAP, "",
					EncodeString(CHAR(x[i+j*r]), w[j], quote, adj_left));
			}
		}
		Rprintf("\n");
		jmin = jmax;
	}
}

void printMatrix(SEXP x, int offset, SEXP dim, int quote)
{
	SEXP l, dimnames, rl, cl;
	int r, c;

	r = INTEGER(dim)[0];
	c = INTEGER(dim)[1];
	rl = R_NilValue;
	cl = R_NilValue;
	dimnames = getAttrib(x, R_DimNamesSymbol);
	if (dimnames != R_NilValue) {
		if (!isNull(CAR(dimnames))) rl = CAR(dimnames);
		if (!isNull(CADR(dimnames))) cl = CADR(dimnames);
	}

	switch (TYPEOF(x)) {
	case LGLSXP:
		printLogicalMatrix(x, offset, r, c, rl, cl);
		break;
	case FACTSXP:
	case ORDSXP:
		if ((l = getAttrib(x, install("levels"))) != R_NilValue
		    && TYPEOF(l) == STRSXP
		    && LENGTH(l) == LEVELS(x)) {
			printFactorMatrix(x, offset, r, c, rl, cl, l, LEVELS(x));
		}
		else
			printIntegerMatrix(x, offset, r, c, rl, cl);
		break;
	case INTSXP:
		printIntegerMatrix(x, offset, r, c, rl, cl);
		break;
	case REALSXP:
		printRealMatrix(x, offset, r, c, rl, cl);
		break;
#ifdef COMPLEX_DATA
	case CPLXSXP:
		printComplexMatrix(x, offset, r, c, rl, cl);
		break;
#endif
	case STRSXP:
		if (quote) quote = '"';
		printStringMatrix(x, offset, r, c, quote, rl, cl);
		break;
	}
}

static void printArrayGeneral(SEXP x, SEXP dim, int quote)
{
	SEXP dimnames, levels, ii, nn, dn;
	int i, j, k, l, b, nb, ndim, nlevs;
	int nr, nc;

	ndim = LENGTH(dim);
	if (ndim == 1)
		printVector(x, 1, quote);
	else if (ndim == 2)
		printMatrix(x, 0, dim, quote);
	else {
		dimnames = getAttrib(x, R_DimNamesSymbol);
		PROTECT(ii = allocVector(INTSXP, ndim));
		PROTECT(nn = allocVector(INTSXP, ndim));
		/* use the above to do higher indexing */
		nr = INTEGER(dim)[0];
		nc = INTEGER(dim)[1];
		b = nr * nc;
		nb = 1;
		for (i = 2; i < ndim; i++)
			nb *= INTEGER(dim)[i];
		levels = getAttrib(x, R_LevelsSymbol);
		nlevs = LEVELS(x);
		for (i = 0; i < nb; i++) {
			Rprintf(", ");
			k = 1;
			dn = CDDR(dimnames);
			for(j=2 ; j< ndim; j++) {
				l = (i/k)%INTEGER(dim)[j]+1;
				if(CAR(dn) != R_NilValue)
					Rprintf(", %s",CHAR(STRING(CAR(dn))[l-1]));
				else
					Rprintf(", %d", l);
				k = k*INTEGER(dim)[j];
				dn = CDR(dn);
			}
			printf("\n\n");

			switch (TYPEOF(x)) {
			case LGLSXP:
				printLogicalMatrix(x, i*b, nr, nc, CAR(dimnames), CADR(dimnames));
				break;
			case FACTSXP:
			case ORDSXP:
				printFactorMatrix(x, i*b, nr, nc, CAR(dimnames), CADR(dimnames), levels, nlevs);
				break;
			case INTSXP:
				printIntegerMatrix(x, i*b, nr, nc, CAR(dimnames), CADR(dimnames));
				break;
			case REALSXP:
				printRealMatrix(x, i*b, nr, nc, CAR(dimnames), CADR(dimnames));
				break;
#ifdef COMPLEX_DATA
			case CPLXSXP:
				printComplexMatrix(x, i*b, nr, nc, CAR(dimnames), CADR(dimnames));
				break;
#endif
			case STRSXP:
				if (quote) quote = '"';
				printStringMatrix(x, i*b, nr, nc, quote, CAR(dimnames), CADR(dimnames));
				break;
			}
			Rprintf("\n");
		}
		UNPROTECT(2);
	}
}

void printArray(SEXP x, int quote)
{
	printArrayGeneral(x, getAttrib(x, R_DimSymbol), quote);
}

static int CountColumns(SEXP x)
{
	int k =0;
	while(x != R_NilValue && isList(x) ) {
		k += ncols(CAR(x));
		x = CDR(x);
	}
	return k;
}

void printDataFrame(SEXP x)
{
	SEXP s, t, rl, cl, sd, se, sw;
	SEXP object, offset, clabel, levels;
	int *w, *d, *e, r, c, k, itmp;
	int rlabw;
	int i, j, j1, j2, nc, tc;
	char *p;

	PROTECT(rl = getAttrib(x, install("row.names")));
	PROTECT(cl = getAttrib(x, R_NamesSymbol));
	if(isNull(cl))
		error("names lost from data frame\n");
	/* since anything can be a data.frame we need some protection */
	if( isList(x) ) {
		r = nrows(CAR(x));
		c = length(x);
	}
	else {
		r = 0;
		c = 0;
	}

		/* Compute the total number of columns. */

	tc = CountColumns(x);

		/* Quick access to the elements of individual */
		/* columns is obtained by recording the object, */
		/* its number of columns and the offset of this */
		/* particular column from the start of the matrix. */

	PROTECT(object = allocVector(STRSXP, tc));
	PROTECT(offset = allocVector(INTSXP, tc));
	PROTECT(clabel = allocVector(STRSXP, tc));
	PROTECT(levels = allocVector(STRSXP, tc));

	i = 0;
	k = 0;
	for(s=x ; s!=R_NilValue; s=CDR(s)) {
		if(isMatrix(CAR(s))) {
			nc = ncols(CAR(s));
			for(j=0 ; j<nc ; j++) {
				STRING(object)[i] = CAR(s);
				INTEGER(offset)[i] = j;
				p = Rsprintf("%s[,%d]", CHAR(STRING(cl)[k]), j+1);
				STRING(clabel)[i] = mkChar(p);
				i++;
			}
		}
		else {
			STRING(object)[i] = CAR(s);
			INTEGER(offset)[i] = 0;
			STRING(clabel)[i] = STRING(cl)[k];
			i++;
		}
		k++;
	}

		/* The following arrays are used to store format */
		/* information for the individual columns. */

	PROTECT(sw = allocVector(INTSXP, tc));
	PROTECT(sd = allocVector(INTSXP, tc));
	PROTECT(se = allocVector(INTSXP, tc));

		/* Ensure no allocing takes place from here */
		/* to the end because we assume that there will */
		/* be no compaction of the vector heap. */
		/* We could use double indirection rather than */
		/* the pointers below to avoid this. */

	w = INTEGER(sw);
	d = INTEGER(sd);
	e = INTEGER(se);

	if (!isNull(rl)) formatString(STRING(rl), r, &rlabw, 0);
	else rlabw = IndexWidth(r + 1) + 3;

	for (i=0 ; i<tc; i++) {
		if (clabel != R_NilValue) {
			if (CHAR(STRING(clabel)[i]) == NULL)
				w[i] = 2;
			else
				w[i] = Rstrlen(CHAR(STRING(clabel)[i]));
		}
		else w[i] = IndexWidth(i + 1) + 3;

		s = STRING(object)[i];
		k = INTEGER(offset)[i];
		switch (TYPEOF(s)) {
		case INTSXP:
			formatInteger(&INTEGER(s)[k*r], r, &itmp);
			break;
		case REALSXP:
			formatReal(&REAL(s)[k*r], r, &itmp, &d[i], &e[i]);
			break;
		case LGLSXP:
			formatLogical(&LOGICAL(s)[k*r], r, &itmp);
			break;
		case FACTSXP:
		case ORDSXP:
			STRING(levels)[i] = getAttrib(s, R_LevelsSymbol);
			formatFactor(&FACTOR(s)[k*r], r, &itmp, STRING(levels)[i], LEVELS(s));
			break;
		case STRSXP:
			formatString(&STRING(s)[k*r], r, &itmp, '"');
			break;
		default:
			error("data.frame has an element of the invalid type\n");
		}
		if (itmp > w[i]) w[i] = itmp;
	}

	j2 = 0;
	while (j2 < tc) {
		nc = PRINT_WIDTH - rlabw;
		Rprintf("%*s", rlabw, " ");
		j1 = j2;
		do {
			if ((nc -= (w[j2] + PRINT_GAP)) < 0)
				break;
			MatrixColumnLabel(clabel, j2, w[j2]+PRINT_GAP);
			j2++;
		}
		while (j2 < tc);
		if (j1 == j2)
			error("PRINT_WIDTH is too narrow\n");
		for (i = 0; i < r; i++) {
			MatrixRowLabel(rl, i, rlabw);
			for (j = j1; j < j2; j++, s = CDR(s)) {
				s = STRING(object)[j];
				k = INTEGER(offset)[j];
				Rprintf("%*s", PRINT_GAP, " ");
				switch (TYPEOF(s)) {
				case INTSXP:
					Rprintf("%s", EncodeInteger(INTEGER(s)[i+k*r], w[j]));
					break;
				case LGLSXP:
					Rprintf("%s", EncodeLogical(LOGICAL(s)[i+k*r], w[j]));
					break;
				case REALSXP:
					Rprintf("%s", EncodeReal(REAL(s)[i+k*r], w[j], d[j], e[j]));
					break;
				case FACTSXP:
				case ORDSXP:
					Rprintf("%s", EncodeFactor(FACTOR(s)[i+k*r], LEVELS(s), w[j], STRING(levels)[j]));
					break;
				case STRSXP:
					Rprintf("%s", EncodeString(CHAR(STRING(s)[i]), w[j], '"', adj_left));
					break;
				default:
					error("invalid data type\n");
				}
			}
		}
		Rprintf("\n");
	}
	UNPROTECT(9);
}
