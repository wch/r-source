/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2017  The R Core Team.
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
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
 *
 *  EXPORTS	printVector()
 *		printNamedVector()
 *		printRealVector()
 *		printRealVectorS()
 *		printIntegerVector()
 *		printIntegerVectorS()
 *		printComplexVector()
 *		printComplexVectorS()
 *
 *  See ./printutils.c	 for remarks on Printing and the Encoding utils.
 *  See ./format.c	 for the formatXXXX functions used below.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Rinternals.h"
#include "Print.h"
#include <R_ext/Itermacros.h> /* for ITERATE_BY_REGION */

#define DO_first_lab			\
    if (indx) {				\
	labwidth = IndexWidth(n) + 2;	\
	/* labwidth may well be		\
	   one more than desired ..*/	\
	VectorIndex(1, labwidth);	\
	width = labwidth;		\
    }					\
    else width = 0

#define DO_newline			\
    Rprintf("\n");			\
    if (indx) {				\
	VectorIndex(i + 1, labwidth);	\
	width = labwidth;		\
    }					\
    else				\
	width = 0

/* print*Vector (* in {Real, Integer, Complex}) are exported, but no
   longer directly called by internal R sources (which now call
   print*VectorS for ALTREP support). Macros are used to prevent drift
   between print*Vector and print*VectorS. 

   printIntegerVector(INTEGER(x)) and printIntegerVector(x) must
   always give identical output, unless INTEGER(x) fails, en.g. during
   allocation. */

/* i must be defined and contain the overall position in the vector
   because DO_newline uses it
   ENCCALL is the full invocation of Encode*() which 
   is passed to Rprintf
*/

/* used for logical, integer, numeric and complex vectors */
#define NUMVECTOR_TIGHTLOOP(ENCCALL) do {	\
	if (i > 0 && width + w > R_print.width) {	\
	    DO_newline;					\
	}						\
	Rprintf("%s", ENCCALL);				\
	width += w;					\
    } while(0)

/* used when printing character vectors */
#define CHARVECTOR_TIGHTLOOP(ENCCALL) do {			\
	if (i > 0 && width + w + R_print.gap > R_print.width) {	\
	    DO_newline;						\
	}							\
	Rprintf("%*s%s", R_print.gap, "",			\
		ENCCALL);					\
	width += w + R_print.gap;				\
    } while (0)

/* used for raw vectors. Could be combined with character vectors
   above but NB the different second conditions for the if
   (width + w vs width + w + R_print.gap) and the different increment
   on width.
*/
#define RAWVECTOR_TIGHTLOOP(ptr, pos) do {				\
	if (i > 0 && width + w > R_print.width) {			\
	    DO_newline;							\
	}								\
	Rprintf("%*s%s", R_print.gap, "", EncodeRaw(ptr[pos], ""));	\
	width += w;							\
    } while (0)

static
void printLogicalVectorS(SEXP x, R_xlen_t n, int indx) {
    int w, labwidth=0, width;
    R_xlen_t i;
    DO_first_lab;
    formatLogicalS(x,XLENGTH(x), &w);
    w += R_print.gap;

    ITERATE_BY_REGION(x, px, idx, nb, int, LOGICAL,
		      for(R_xlen_t j = 0; j < nb; j++) {
			  i = idx + j; /* for Do_newline */
			  NUMVECTOR_TIGHTLOOP( EncodeLogical(px[j], w) );
		      });
    Rprintf("\n");
}

attribute_hidden
void printIntegerVector(const int *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatInteger(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	NUMVECTOR_TIGHTLOOP(EncodeInteger(x[i], w));
    }
    Rprintf("\n");
}

attribute_hidden
void printIntegerVectorS(SEXP x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;
    R_xlen_t i;
    DO_first_lab;
    formatIntegerS(x, XLENGTH(x), &w);
    w += R_print.gap;

    ITERATE_BY_REGION(x, px, idx, nb, int, INTEGER,
		      for (R_xlen_t j = 0; j < nb; j++) {
			  i = idx + j; /* for macros */
			  NUMVECTOR_TIGHTLOOP(EncodeInteger(px[j], w));
		      });
    
    Rprintf("\n");
}

// used in uncmin.c
// Not easily converted to printRealVectorS calls
attribute_hidden
void printRealVector(const double *x, R_xlen_t n, int indx)
{
    int w, d, e, labwidth=0, width;

    DO_first_lab;
    formatReal(x, n, &w, &d, &e, 0);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	NUMVECTOR_TIGHTLOOP( EncodeReal0(x[i], w, d, e, OutDec) );
    }
    Rprintf("\n");
}

attribute_hidden
void printRealVectorS(SEXP x, R_xlen_t n, int indx)
{
    int w, d, e, labwidth=0, width;
    R_xlen_t i;
    DO_first_lab;
    formatRealS(x, n, &w, &d, &e, 0);
    w += R_print.gap;

    ITERATE_BY_REGION(x, px, idx, nb, double, REAL,
		      for(R_xlen_t j = 0; j < nb; j++) {
			  i = idx + j; /* for macros */
			  NUMVECTOR_TIGHTLOOP(EncodeReal0(px[j], w, d, e, OutDec));
		      });
    
    Rprintf("\n");
}

#define CMPLX_ISNA(cplx) (ISNA(cplx.r) || ISNA(cplx.i))
attribute_hidden
void printComplexVector(const Rcomplex *x, R_xlen_t n, int indx)
{
    int w, wr, dr, er, wi, di, ei, labwidth=0, width;

    DO_first_lab;
    formatComplex(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);

    w = wr + wi + 2;	/* +2 for "+" and "i" */
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	NUMVECTOR_TIGHTLOOP(CMPLX_ISNA(x[i]) ?
			EncodeReal0(NA_REAL, w, 0, 0, OutDec) :
			EncodeComplex(x[i], wr + R_print.gap,
				      dr, er, wi, di, ei, OutDec));
    }
    Rprintf("\n");
}

attribute_hidden
void printComplexVectorS(SEXP x, R_xlen_t n, int indx)
{
    int w, wr, dr, er, wi, di, ei, labwidth=0, width;
    R_xlen_t i;
    DO_first_lab;
    formatComplexS(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);

    w = wr + wi + 2;	/* +2 for "+" and "i" */
    w += R_print.gap;

    ITERATE_BY_REGION(x, px, idx, nb, Rcomplex, COMPLEX,
		      for(R_xlen_t j = 0; j < nb; j++) {
			  i = idx + j; /* for macros */
			  NUMVECTOR_TIGHTLOOP(CMPLX_ISNA(px[j]) ?
					  EncodeReal0(NA_REAL, w, 0, 0, OutDec) :
					  EncodeComplex(px[j], wr + R_print.gap , dr, er, wi, di, ei, OutDec));
		      });
    Rprintf("\n");
}


static void printStringVector(const SEXP *x, R_xlen_t n, int quote, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatString(x, n, &w, quote);

    for (R_xlen_t i = 0; i < n; i++) {
	if (i > 0 && width + w + R_print.gap > R_print.width) {
	    DO_newline;
	}
	Rprintf("%*s%s", R_print.gap, "",
		EncodeString(x[i], w, quote, R_print.right));
	width += w + R_print.gap;
    }
    Rprintf("\n");
}

static void printStringVectorS(SEXP x, R_xlen_t n, int quote, int indx)
{
    /* because there's no get_region method for ALTSTRINGs
       we hit the old version if we can to avoid the
       STRING_ELT in the tight loop. 

       This will work for all nonALTREP STRSXPs as well as whenever
       the ALTSTRING class is willing to give us a full dataptr from
       Dataptr_or_null method. */

    const SEXP *xptr = (const SEXP *) DATAPTR_OR_NULL(x);
    if(xptr != NULL) {
	printStringVector(xptr, n, quote, indx);
	return;
    }
    
    int w, labwidth=0, width;

    DO_first_lab;
    formatStringS(x, n, &w, quote);

    for (R_xlen_t i = 0; i < n; i++) {
	CHARVECTOR_TIGHTLOOP(
	    EncodeString(STRING_ELT(x, i), w, quote, R_print.right)
	    );
    }
    Rprintf("\n");
}




attribute_hidden
void printRawVector(const Rbyte *x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;

    DO_first_lab;
    formatRaw(x, n, &w);
    w += R_print.gap;

    for (R_xlen_t i = 0; i < n; i++) {
	RAWVECTOR_TIGHTLOOP(x, i);
    }
    Rprintf("\n");
}


static
void printRawVectorS(SEXP x, R_xlen_t n, int indx)
{
    int w, labwidth=0, width;
    R_xlen_t i;
    DO_first_lab;
    formatRawS(x, n, &w);
    w += R_print.gap;

    ITERATE_BY_REGION(x, px, idx, nb, Rbyte, RAW,
		      for(R_xlen_t j = 0; j < nb; j++) {
			  i = idx + j; /* for macros */
			  RAWVECTOR_TIGHTLOOP(px, j);
		      });
    Rprintf("\n");
}


void printVector(SEXP x, int indx, int quote)
{
/* print R vector x[];	if(indx) print indices; if(quote) quote strings */
    R_xlen_t n;

    if ((n = XLENGTH(x)) != 0) {
	R_xlen_t n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printLogicalVectorS(x, n_pr, indx);
	    break;
	case INTSXP:
	    printIntegerVectorS(x, n_pr, indx);
	    break;
	case REALSXP:
	    printRealVectorS(x, n_pr, indx);
	    break;
	case STRSXP:
	    if (quote)
		printStringVectorS(x, n_pr, '"', indx);
	    else
		printStringVectorS(x, n_pr, 0, indx);
	    break;
	case CPLXSXP:
	    printComplexVectorS(x, n_pr, indx);
	    break;
	case RAWSXP:
	    printRawVectorS(x, n_pr, indx);
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);
    }
    else
#define PRINT_V_0						\
	switch (TYPEOF(x)) {					\
	case LGLSXP:	Rprintf("logical(0)\n");	break;	\
	case INTSXP:	Rprintf("integer(0)\n");	break;	\
	case REALSXP:	Rprintf("numeric(0)\n");	break;	\
	case CPLXSXP:	Rprintf("complex(0)\n");	break;	\
	case STRSXP:	Rprintf("character(0)\n");	break;	\
	case RAWSXP:	Rprintf("raw(0)\n");		break;	\
	}
	PRINT_V_0;
}

#undef DO_first_lab
#undef DO_newline


/* The following code prints vectors which have every element named.

 * Primitives for each type of vector are presented first, followed
 * by the main (dispatching) function.
 * 1) These primitives are almost identical... ==> use PRINT_N_VECTOR_SEXP macro
 * 2) S prints a _space_ in the first column for named vectors; we dont.
 */

#define PRINT_N_VECTOR_SEXP(INI_FORMAT, PRINT_1)			\
    {									\
	int i, j, k, nlines, nperline, w, wn;				\
	INI_FORMAT;							\
									\
	formatStringS(names, n, &wn, 0);				\
	if (w < wn) w = wn;						\
	nperline = R_print.width / (w + R_print.gap);			\
	if (nperline <= 0) nperline = 1;				\
	nlines = n / nperline;						\
	if (n % nperline) nlines += 1;					\
									\
	for (i = 0; i < nlines; i++) {					\
	    if (i) Rprintf("\n");					\
	    for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) \
		Rprintf("%s%*s",					\
			EncodeString(STRING_ELT(names, k), w, 0,	\
				     Rprt_adj_right),			\
			R_print.gap, "");				\
	    Rprintf("\n");						\
	    for (j = 0; j < nperline && (k = i * nperline + j) < n; j++) \
		PRINT_1;						\
	}								\
	Rprintf("\n");							\
    }

static void printNamedLogicalVectorS(SEXP x, int n, SEXP names)
    PRINT_N_VECTOR_SEXP(formatLogicalS(x, n, &w),
			Rprintf("%s%*s", EncodeLogical(LOGICAL_ELT(x, k), w),
				R_print.gap,""))

static void printNamedIntegerVectorS(SEXP x, int n, SEXP names)
    PRINT_N_VECTOR_SEXP(formatIntegerS(x, n, &w),
			Rprintf("%s%*s", EncodeInteger(INTEGER_ELT(x, k), w),
				R_print.gap,""))

#undef INI_F_REAL_S
#define INI_F_REAL_S	int d, e; formatRealS(x, n, &w, &d, &e, 0)

static void printNamedRealVectorS(SEXP x, int n, SEXP names)
    PRINT_N_VECTOR_SEXP(INI_F_REAL_S,
			Rprintf("%s%*s",
				EncodeReal0(REAL_ELT(x, k), w, d, e, OutDec),
				R_print.gap,""))

#undef INI_F_CPLX_S
#define INI_F_CPLX_S						\
    int wr, dr, er, wi, di, ei;					\
    formatComplexS(x, n, &wr, &dr, &er, &wi, &di, &ei, 0);	\
    w = wr + wi + 2;						\
    Rcomplex tmp

#undef P_IMAG_NA
#define P_IMAG_NA(VALUE)			\
	    if(ISNAN(VALUE.i))			\
		Rprintf("+%si", "NaN");		\
	    else

static void printNamedComplexVectorS(SEXP x, int n, SEXP names)
    PRINT_N_VECTOR_SEXP(INI_F_CPLX_S,
	{ /* PRINT_1 */
	    tmp = COMPLEX_ELT(x, j);
	    if(j) Rprintf("%*s", R_print.gap, "");
	    if (ISNA(tmp.r) || ISNA(tmp.i)) {
		Rprintf("%s", EncodeReal0(NA_REAL, w, 0, 0, OutDec));
	    }
	    else {
		Rprintf("%s", EncodeReal0(tmp.r, wr, dr, er, OutDec));
		P_IMAG_NA(tmp)
		if (tmp.i >= 0)
		    Rprintf("+%si", EncodeReal0(tmp.i, wi, di, ei, OutDec));
		else
		    Rprintf("-%si", EncodeReal0(-tmp.i, wi, di, ei, OutDec));
	    }
	})

static void printNamedStringVectorS(SEXP x, int n, int quote, SEXP names)
    PRINT_N_VECTOR_SEXP(formatStringS(x, n, &w, quote),
		   Rprintf("%s%*s",
			   EncodeString(STRING_ELT(x, k), w, quote,
					Rprt_adj_right),
			   R_print.gap, ""))

static void printNamedRawVectorS(SEXP x, int n, SEXP names)
    PRINT_N_VECTOR_SEXP(formatRawS(x, n, &w),
		   Rprintf("%*s%s%*s", w - 2, "",
			   EncodeRaw(RAW_ELT(x, k), ""), R_print.gap,""))

attribute_hidden
void printNamedVector(SEXP x, SEXP names, int quote, const char *title)
{
    int n;

    if (title != NULL)
	 Rprintf("%s\n", title);

    if ((n = LENGTH(x)) != 0) {
	int n_pr = (n <= R_print.max +1) ? n : R_print.max;
	/* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	switch (TYPEOF(x)) {
	case LGLSXP:
	    printNamedLogicalVectorS(x, n_pr, names);
	    break;
	case INTSXP:
	    printNamedIntegerVectorS(x, n_pr, names);
	    break;
	case REALSXP:
	    printNamedRealVectorS(x, n_pr, names);
	    break;
	case CPLXSXP:
	    printNamedComplexVectorS(x, n_pr, names);
	    break;
	case STRSXP:
	    if(quote) quote = '"';
	    printNamedStringVectorS(x, n_pr, quote, names);
	    break;
	case RAWSXP:
	    printNamedRawVectorS(x, n_pr, names);
	    break;
	}
	if(n_pr < n)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			n - n_pr);

    }
    else {
	Rprintf("named ");
	PRINT_V_0;
    }
}
