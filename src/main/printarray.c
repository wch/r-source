/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000--2002	The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  EXPORTS	printMatrix()
 *		printArray()
 *
 *  See ./printutils.c	 for general remarks on Printing
 *			 and the Encode.. utils.
 *
 *  See ./format.c	 for the  format_FOO_  functions used below.
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"

static void printLogicalMatrix(SEXP sx, int offset, int r, int c,
			       SEXP rl, SEXP cl, char *rn, char *cn)
{
    SEXP sw;
    int *x, *w;
    int width, rlabw, clabw, rnw;
    int i, j, jmin, jmax, lbloff = 0;

    if (!isNull(rl))
	formatString(STRING_PTR(rl), r, &rlabw, 0);
    else
	rlabw = IndexWidth(r + 1) + 3;

    if (rn) {
	rnw = strlen(rn);
	if ( rnw < rlabw + R_MIN_LBLOFF )
	    lbloff = R_MIN_LBLOFF;
	else
	    lbloff = rnw - rlabw;

	rlabw += lbloff;
    }

    sw = allocVector(INTSXP, c);
    x = INTEGER(sx) + offset;
    w = INTEGER(sw);

    for (j = 0; j < c; j++) {
	formatLogical(&x[j * r], r, &w[j]);
	if (!isNull(cl)) {
	    if(STRING_ELT(cl, j) == NA_STRING) 
		clabw = R_print.na_width_noquote;
	    else clabw = strlen(CHAR(STRING_ELT(cl, j)));
	} else
	    clabw = IndexWidth(j + 1) + 3;
	if (w[j] < clabw)
	    w[j] = clabw;
	w[j] += R_print.gap;
    }
    jmin = 0;
    jmax = 0;
    if (c == 0) {
	for (i = 0; i < r; i++)
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	Rprintf("\n");
	return;
    }
    while (jmin < c) {
	width = rlabw;
	do {
	    width += w[jmax];
	    jmax++;
	}
	while (jmax < c && width + w[jmax] < R_print.width);

	if (cn != NULL)
	    Rprintf("%*s%s\n", rlabw, "", cn);

	if (rn != NULL)
	    Rprintf("%*s", -rlabw, rn);
	else
	    Rprintf("%*s", rlabw, "");

	for (j = jmin; j < jmax ; j++)
	    MatrixColumnLabel(cl, j, w[j]);
	for (i = 0; i < r; i++) {
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	    for (j = jmin; j < jmax; j++) {
		Rprintf("%s", EncodeLogical(x[i + j * r], w[j]));
	    }
	}
	Rprintf("\n");
	jmin = jmax;
    }
}

static void printIntegerMatrix(SEXP sx, int offset, int r, int c,
			       SEXP rl, SEXP cl, char *rn, char *cn)
{
    SEXP sw;
    int *x, *w;
    int width, rlabw, clabw, rnw;
    int i, j, jmin, jmax, lbloff = 0;

    if (!isNull(rl))
	formatString(STRING_PTR(rl), r, &rlabw, 0);
    else
	rlabw = IndexWidth(r + 1) + 3;

    if (rn) {
	rnw = strlen(rn);
	if ( rnw < rlabw + R_MIN_LBLOFF )
	    lbloff = R_MIN_LBLOFF;
	else
	    lbloff = rnw - rlabw;

	rlabw += lbloff;
    }

    sw = allocVector(INTSXP, c);
    x = INTEGER(sx) + offset;
    w = INTEGER(sw);
    for (j = 0; j < c; j++) {
	formatInteger(&x[j * r], r, &w[j]);
	if (!isNull(cl)) {
	    if(STRING_ELT(cl, j) == NA_STRING) 
		clabw = R_print.na_width_noquote;
	    else clabw = strlen(CHAR(STRING_ELT(cl, j)));
	} else
	    clabw = IndexWidth(j + 1) + 3;
	if (w[j] < clabw)
	    w[j] = clabw;
	w[j] += R_print.gap;
    }
    jmin = 0;
    jmax = 0;
    if (c == 0) {
	for (i = 0; i < r; i++)
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	Rprintf("\n");
	return;
    }
    while (jmin < c) {
	width = rlabw;
	do {
	    width += w[jmax];
	    jmax++;
	}
	while (jmax < c && width + w[jmax] < R_print.width);

	if (cn != NULL)
	    Rprintf("%*s%s\n", rlabw, "", cn);

	if (rn != NULL)
	    Rprintf("%*s", -rlabw, rn);
	else
	    Rprintf("%*s", rlabw, "");

	for (j = jmin; j < jmax ; j++)
	    MatrixColumnLabel(cl, j, w[j]);
	for (i = 0; i < r; i++) {
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	    for (j = jmin; j < jmax; j++) {
		Rprintf("%s", EncodeInteger(x[i + j * r], w[j]));
	    }
	}
	Rprintf("\n");
	jmin = jmax;
    }
}

static void printRealMatrix(SEXP sx, int offset, int r, int c,
			    SEXP rl, SEXP cl, char *rn, char *cn)
{
    SEXP sd, se, sw;
    double *x;
    int *d, *e, *w;
    int width, rlabw, clabw, rnw;
    int i, j, jmin, jmax, lbloff = 0;

    if (!isNull(rl))
	formatString(STRING_PTR(rl), r, &rlabw, 0);
    else
	rlabw = IndexWidth(r + 1) + 3;

    if (rn) {
	rnw = strlen(rn);
	if ( rnw < rlabw + R_MIN_LBLOFF )
	    lbloff = R_MIN_LBLOFF;
	else
	    lbloff = rnw - rlabw;

	rlabw += lbloff;
    }

    PROTECT(sd = allocVector(INTSXP, c));
    PROTECT(se = allocVector(INTSXP, c));
    sw = allocVector(INTSXP, c);
    UNPROTECT(2);
    x = REAL(sx) + offset;
    d = INTEGER(sd);
    e = INTEGER(se);
    w = INTEGER(sw);

    for (j = 0; j < c; j++) {
	formatReal(&x[j * r], r, &w[j], &d[j], &e[j], 0);
	if (!isNull(cl)) {
	    if(STRING_ELT(cl, j) == NA_STRING) 
		clabw = R_print.na_width_noquote;
	    else clabw = strlen(CHAR(STRING_ELT(cl, j)));
	} else
	    clabw = IndexWidth(j + 1) + 3;
	if (w[j] < clabw)
	    w[j] = clabw;
	w[j] += R_print.gap;
    }
    jmin = 0;
    jmax = 0;
    if (c == 0) {
	for (i = 0; i < r; i++)
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	Rprintf("\n");
	return;
    }
    while (jmin < c) {
	width = rlabw;
	do {
	    width += w[jmax];
	    jmax++;
	}
	while (jmax < c && width + w[jmax] < R_print.width);

	if (cn != NULL)
	    Rprintf("%*s%s\n", rlabw, "", cn);

	if (rn != NULL)
	    Rprintf("%*s", -rlabw, rn);
	else
	    Rprintf("%*s", rlabw, "");

	for (j = jmin; j < jmax ; j++)
	    MatrixColumnLabel(cl, j, w[j]);
	for (i = 0; i < r; i++) {
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	    for (j = jmin; j < jmax; j++) {
		Rprintf("%s", EncodeReal(x[i + j * r], w[j], d[j], e[j]));
	    }
	}
	Rprintf("\n");
	jmin = jmax;
    }
}

static void printComplexMatrix(SEXP sx, int offset, int r, int c,
			       SEXP rl, SEXP cl, char *rn, char *cn)
{
    SEXP sdr, ser, swr, sdi, sei, swi, sw;
    Rcomplex *x;
    int *dr, *er, *wr, *di, *ei, *wi, *w;
    int width, rlabw, clabw, rnw;
    int i, j, jmin, jmax, lbloff = 0;

    if (!isNull(rl))
	formatString(STRING_PTR(rl), r, &rlabw, 0);
    else
	rlabw = IndexWidth(r + 1) + 3;

    if (rn) {
	rnw = strlen(rn);
	if ( rnw < rlabw + R_MIN_LBLOFF )
	    lbloff = R_MIN_LBLOFF;
	else
	    lbloff = rnw - rlabw;

	rlabw += lbloff;
    }

    PROTECT(sdr = allocVector(INTSXP, c));
    PROTECT(ser = allocVector(INTSXP, c));
    PROTECT(swr = allocVector(INTSXP, c));
    PROTECT(sdi = allocVector(INTSXP, c));
    PROTECT(sei = allocVector(INTSXP, c));
    PROTECT(swi = allocVector(INTSXP, c));
    PROTECT(sw  = allocVector(INTSXP, c));
    UNPROTECT(7);
    x = COMPLEX(sx) + offset;
    dr = INTEGER(sdr);
    er = INTEGER(ser);
    wr = INTEGER(swr);
    di = INTEGER(sdi);
    ei = INTEGER(sei);
    wi = INTEGER(swi);
    w = INTEGER(sw);

    /* Determine the column widths */

    for (j = 0; j < c; j++) {
	formatComplex(&x[j * r], r,
		      &wr[j], &dr[j], &er[j],
		      &wi[j], &di[j], &ei[j], 0);
	if (!isNull(cl)) {
	    if(STRING_ELT(cl, j) == NA_STRING) 
		clabw = R_print.na_width_noquote;
	    else clabw = strlen(CHAR(STRING_ELT(cl, j)));
	} else
	    clabw = IndexWidth(j + 1) + 3;
	w[j] = wr[j] + wi[j] + 2;
	if (w[j] < clabw)
	    w[j] = clabw;
	w[j] += R_print.gap;
    }

    jmin = 0;
    jmax = 0;
    if (c == 0) {
	for (i = 0; i < r; i++)
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	Rprintf("\n");
	return;
    }
    while (jmin < c) {
	width = rlabw;
	do {
	    width += w[jmax];
	    jmax++;
	}
	while (jmax < c && width+w[jmax] < R_print.width);

	if (cn != NULL)
	    Rprintf("%*s%s\n", rlabw, "", cn);

	if (rn != NULL)
	    Rprintf("%*s", -rlabw, rn);
	else
	    Rprintf("%*s", rlabw, "");

	for (j = jmin; j < jmax ; j++)
	    MatrixColumnLabel(cl, j, w[j]);
	for (i = 0; i < r; i++) {
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	    for (j = jmin; j < jmax; j++) {
		if (ISNA(x[i + j * r].r) || ISNA(x[i + j * r].i))
		    Rprintf("%s", EncodeReal(NA_REAL, w[j], 0, 0));
		else
		    Rprintf("%s",
			    EncodeComplex(x[i + j * r],
					  wr[j] + R_print.gap, dr[j], er[j],
					  wi[j], di[j], ei[j]));
	    }
	}
	Rprintf("\n");
	jmin = jmax;
    }
}

static void printStringMatrix(SEXP sx, int offset, int r, int c,
			      int quote, int right, SEXP rl, SEXP cl,
			      char *rn, char *cn)
{
    SEXP sw;
    SEXP *x;
    int *w;
    int width, rlabw, clabw, rnw;
    int i, j, jmin, jmax, lbloff = 0;

    if (!isNull(rl))
	formatString(STRING_PTR(rl), r, &rlabw, 0);
    else
	rlabw = IndexWidth(r + 1) + 3;

    if (rn) {
	rnw = strlen(rn);
	if ( rnw < rlabw + R_MIN_LBLOFF )
	    lbloff = R_MIN_LBLOFF;
	else
	    lbloff = rnw - rlabw;

	rlabw += lbloff;
    }

    sw = allocVector(INTSXP, c);
    x = STRING_PTR(sx)+offset;
    w = INTEGER(sw);
    for (j = 0; j < c; j++) {
	formatString(&x[j * r], r, &w[j], quote);
	if (!isNull(cl)) {
	    if(STRING_ELT(cl, j) == NA_STRING) 
		clabw = R_print.na_width_noquote;
	    else clabw = strlen(CHAR(STRING_ELT(cl, j)));
	} else
	    clabw = IndexWidth(j + 1) + 3;
	if (w[j] < clabw)
	    w[j] = clabw;
    }
    jmin = 0;
    jmax = 0;
    if (c == 0) {
	for (i = 0; i < r; i++)
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	Rprintf("\n");
	return;
    }
    while (jmin < c) {
	width = rlabw;
	do {
	    width += w[jmax] + R_print.gap;
	    jmax++;
	}
	while (jmax < c && width + w[jmax] + R_print.gap < R_print.width);

	if (cn != NULL)
	    Rprintf("%*s%s\n", rlabw, "", cn);

	if (rn != NULL)
	    Rprintf("%*s", -rlabw, rn);
	else
	    Rprintf("%*s", rlabw, "");

	if (right) {
	    for (j = jmin; j < jmax ; j++)
		RightMatrixColumnLabel(cl, j, w[j]);
	}
	else {
	    for (j = jmin; j < jmax ; j++)
		LeftMatrixColumnLabel(cl, j, w[j]);
	}
	for (i = 0; i < r; i++) {
	    MatrixRowLabel(rl, i, rlabw, lbloff);
	    for (j = jmin; j < jmax; j++) {
		Rprintf("%*s%s", R_print.gap, "",
			EncodeString(CHAR(x[i + j * r]), w[j], quote, right));
	    }
	}
	Rprintf("\n");
	jmin = jmax;
    }
}

void printMatrix(SEXP x, int offset, SEXP dim, int quote, int right,
		 SEXP rl, SEXP cl, char *rn, char *cn)
{
    int r, c;

    r = INTEGER(dim)[0];
    c = INTEGER(dim)[1];
    /* PR#850 */
    if ((rl!=R_NilValue) && (r>length(rl)))
	error("too few row labels");
    if ((cl!=R_NilValue) && (c>length(cl)))
	error("too few column labels");
    if (r == 0 && c == 0) {
	Rprintf("<0 x 0 matrix>\n");
	return;
    }
    switch (TYPEOF(x)) {
    case LGLSXP:
	printLogicalMatrix(x, offset, r, c, rl, cl, rn, cn);
	break;
    case INTSXP:
	printIntegerMatrix(x, offset, r, c, rl, cl, rn, cn);
	break;
    case REALSXP:
	printRealMatrix(x, offset, r, c, rl, cl, rn, cn);
	break;
    case CPLXSXP:
	printComplexMatrix(x, offset, r, c, rl, cl, rn, cn);
	break;
    case STRSXP:
	if (quote) quote = '"';
	printStringMatrix(x, offset, r, c, quote, right, rl, cl, rn, cn);
	break;
    }
}

static void printArrayGeneral(SEXP x, SEXP dim, int quote, SEXP dimnames)
{
/* == printArray(.) */
    SEXP dn, dnn;
    int i, j, k, l, b, nb, ndim;
    int nr, nc;
    int has_dimnames = 0, has_dnn = 0;
    char *rn = NULL, *cn = NULL;

    ndim = LENGTH(dim);
    if (ndim == 1)
	printVector(x, 1, quote);
    else if (ndim == 2) {
	SEXP rl, cl;
	GetMatrixDimnames(x, &rl, &cl, &rn, &cn);
	printMatrix(x, 0, dim, quote, 0, rl, cl, rn, cn);
    }
    else {
	SEXP dn0, dn1;
	nr = INTEGER(dim)[0];
	nc = INTEGER(dim)[1];
	b = nr * nc;
	nb = 1;
	for (i = 2 ; i < ndim ; i++)
	    nb *= INTEGER(dim)[i];
	dnn = R_NilValue;	/* -Wall */
	if (dimnames == R_NilValue) {
	    dn0 = R_NilValue;
	    dn1 = R_NilValue;
	}
	else {
	    dn0 = VECTOR_ELT(dimnames, 0);
	    dn1 = VECTOR_ELT(dimnames, 1);
	    has_dimnames = 1;
	    dnn = getAttrib(dimnames, R_NamesSymbol);
	    has_dnn = !isNull(dnn);
	    if ( has_dnn ) {
		rn = CHAR(STRING_ELT(dnn, 0));
		cn = CHAR(STRING_ELT(dnn, 1));
	    }
	}
	for (i = 0; i < nb; i++) {
	    Rprintf(", ");
	    k = 1;
	    for (j = 2 ; j < ndim; j++) {
		l = (i / k) % INTEGER(dim)[j] + 1;
		if (has_dimnames &&
		    ((dn = VECTOR_ELT(dimnames, j)) != R_NilValue)) {
		    if ( has_dnn )
			Rprintf(", %s = %s",
				CHAR(STRING_ELT(dnn, j)),
				CHAR(STRING_ELT(dn, l - 1)));
		    else
			Rprintf(", %s", CHAR(STRING_ELT(dn, l - 1)));
		} else
		    Rprintf(", %d", l);
		k = k * INTEGER(dim)[j];
	    }
	    Rprintf("\n\n");
	    switch (TYPEOF(x)) {
	    case LGLSXP:
		printLogicalMatrix(x, i * b, nr, nc, dn0, dn1, rn, cn);
		break;
	    case INTSXP:
		printIntegerMatrix(x, i * b, nr, nc, dn0, dn1, rn, cn);
		break;
	    case REALSXP:
		printRealMatrix(x, i * b, nr, nc, dn0, dn1, rn, cn);
		break;
	    case CPLXSXP:
		printComplexMatrix(x, i * b, nr, nc, dn0, dn1, rn, cn);
		break;
	    case STRSXP:
		if (quote) quote = '"';
		printStringMatrix(x, i * b, nr, nc, quote, 0, dn0, dn1, rn, cn);
		break;
	    }
	    Rprintf("\n");
	}
    }
}

void printArray(SEXP x, SEXP dim, int quote, SEXP dimnames)
{
    printArrayGeneral(x, dim, quote, dimnames);
}
