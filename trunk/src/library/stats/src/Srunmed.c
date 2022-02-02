/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012-2019  The R Core Team
 *  Copyright (C) 2003       The R Foundation
 *  Copyright (C) 1995-2002  Martin Maechler <maechler@stat.math.ethz.ch>
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
 */

#include "modreg.h"

// Large value, to replace { NaN | NA } values with for NA_BIG_alternate_* :
static double
BIG_dbl = 8.888888888e307;
//          1 3 5 7 9, // ~ < 2^1023 [such that +/- 2*BIG_dbl is still normalized]

enum { NA_BIG_alternate_P = 1, NA_BIG_alternate_M, NA_OMIT, NA_FAIL };
// == 1,2,..., defined by order in formals(runmed)$na.action -->  ../R/runmed.R

#include "Trunmed.c"
//        ---------

static void Srunmed(const double* y, double* smo, R_xlen_t n, int bw,
		    int end_rule, int print_level)
{
/*
 *  Computes "Running Median" smoother ("Stuetzle" algorithm) with medians of 'band'

 *  Input:
 *	y(n)	- responses in order of increasing predictor values
 *	n	- number of observations
 *	bw	- span of running medians (MUST be ODD !!)
 *	end_rule -- 0: Keep original data at ends {j; j < b2 | j > n-b2}
 *		 -- 1: Constant ends = median(y[1],..,y[bw]) "robust"
 *  Output:
 *	smo(n)	- smoothed responses

 * NOTE:  The 'end' values are just copied !! this is fast but not too nice !
 */

/* Local variables */
    double rmed, rmin, temp, rnew, yout, yi;
    double rbe, rtb, rse, yin, rts;
    int imin, ismo, i, j, first, last, band2, kminus, kplus;


    double *scrat = (double *) R_alloc(bw, sizeof(double));
    /*was  malloc( (unsigned) bw * sizeof(double));*/

    if(bw > n)
	error(_("bandwidth/span of running medians is larger than n"));

/* 1. Compute  'rmed' := Median of the first 'band' values
   ======================================================== */

    for (int i = 0; i < bw; ++i)
	scrat[i] = y[i];

    /* find minimal value  rmin = scrat[imin] <= scrat[j] */
    rmin = scrat[0]; imin = 0;
    for (int i = 1; i < bw; ++i)
	if (scrat[i] < rmin) {
	    rmin = scrat[i]; imin = i;
	}
    /* swap scrat[0] <-> scrat[imin] */
    temp = scrat[0]; scrat[0] = rmin; scrat[imin] = temp;
    /* sort the rest of of scrat[] by bubble (?) sort -- */
    for (int i = 2; i < bw; ++i) {
	if (scrat[i] < scrat[i - 1]) {/* find the proper place for scrat[i] */
	    temp = scrat[i];
	    j = i;
	    do {
		scrat[j] = scrat[j - 1];
		--j;
	    } while (scrat[j - 1] > temp); /* now:  scrat[j-1] <= temp */
	    scrat[j] = temp;
	}
    }

    band2 = bw / 2;
    rmed = scrat[band2];/* == Median( y[(1:band2)-1] ) */
    /* "malloc" had  free( (char*) scrat);*/ /*-- release scratch memory --*/

    if(end_rule == 0) { /*-- keep DATA at end values */
	for (i = 0; i < band2; ++i)
	    smo[i] = y[i];
    }
    else { /* if(end_rule == 1)  copy median to CONSTANT end values */
	for (i = 0; i < band2; ++i)
	    smo[i] = rmed;
    }
    smo[band2] = rmed;
    band2++; /* = bw / 2 + 1*/;

    if(print_level >= 1) REprintf("(bw,b2)= (%d,%d)\n", bw, band2);

/* Big	FOR Loop: RUNNING median, update the median 'rmed'
   ======================================================= */
    for (first = 1, last = bw, ismo = band2;
	 last < n;
	 ++first, ++last, ++ismo) {

	yin = y[last];
	yout = y[first - 1];

	if(print_level >= 2) REprintf(" is=%d, y(in/out)= %10g, %10g", ismo, yin, yout);

	rnew = rmed; /* New median = old one   in all the simple cases --*/

	if (yin < rmed) {
	    if (yout >= rmed) {
		kminus = 0;
		if (yout > rmed) {/*	--- yin < rmed < yout --- */
		    if(print_level >= 2) REprintf(": yin < rmed < yout ");
		    rnew = yin;/* was -rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi < rmed) {
			    ++kminus;
			    if (yi > rnew)	rnew = yi;
			}
		    }
		    if (kminus < band2)		rnew = rmed;
		}
		else {/*		--- yin < rmed = yout --- */
		    if(print_level >= 2) REprintf(": yin < rmed == yout ");
		    rse = rts = yin;/* was -rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi <= rmed) {
			    if (yi < rmed) {
				++kminus;
				if (yi > rts)	rts = yi;
				if (yi > rse)	rse = yi;
			    } else		rse = yi;

			}
		    }
		    rnew = (kminus == band2) ? rts : rse ;
		    if(print_level >= 2) REprintf("k- : %d,", kminus);
		}
	    } /* else: both  yin, yout < rmed -- nothing to do .... */
	}
	else if (yin != rmed) { /* yin > rmed */
	    if (yout <= rmed) {
		kplus = 0;
		if (yout < rmed) {/* -- yout < rmed < yin --- */
		    if(print_level >= 2) REprintf(": yout < rmed < yin ");
		    rnew = yin; /* was rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi > rmed) {
			    ++kplus;
			    if (yi < rnew)	rnew = yi;
			}
		    }
		    if (kplus < band2)	rnew = rmed;

		} else { /* -- yout = rmed < yin --- */
		    if(print_level >= 2) REprintf(": yout == rmed < yin ");
		    rbe = rtb = yin; /* was rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi >= rmed) {
			    if (yi > rmed) {
				++kplus;
				if (yi < rtb)	rtb = yi;
				if (yi < rbe)	rbe = yi;
			    } else		rbe = yi;
			}
		    }
		    rnew = (kplus == band2) ? rtb : rbe;
		    if(print_level >= 2) REprintf("k+ : %d,", kplus);
		}
	    } /* else: both  yin, yout > rmed --> nothing to do */
	} /* else: yin == rmed -- nothing to do .... */
	if(print_level >= 2) REprintf("=> %12g, %12g\n", rmed, rnew);
	rmed = rnew;
	smo[ismo] = rmed;
    } /*     END FOR ------------ big Loop -------------------- */

    if(end_rule == 0) { /*-- keep DATA at end values */
	for (i = ismo; i < n; ++i)
	    smo[i] = y[i];
    }
    else { /* if(end_rule == 1)  copy median to CONSTANT end values */
	for (i = ismo; i < n; ++i)
	    smo[i] = rmed;
    }
} /* Srunmed */

// anyNA() like [ see ../../../main/coerce.c ]
static
R_xlen_t R_firstNA_dbl(const double x[], R_xlen_t n) {
    for(R_xlen_t k = 0; k < n; k++)
	if(ISNAN(x[k]))
	    return k+1; // 1-index
    return 0; // no NaN|NA found
}

// .Call()ed from ../R/runmed.R
SEXP runmed(SEXP sx, SEXP stype, SEXP sk, SEXP end, SEXP naAct, SEXP printLev)
{
    if (TYPEOF(sx) != REALSXP) error("numeric 'x' required");
    double *x = REAL(sx), *xx;
    R_xlen_t n = XLENGTH(sx);
    int type        = asInteger(stype),
	k           = asInteger(sk),
	end_rule    = asInteger(end),
	na_action   = asInteger(naAct),
	print_level = asInteger(printLev);
    R_xlen_t
	firstNA = R_firstNA_dbl(x, n),
	nn = n;
    if(print_level)
	Rprintf("firstNA = %d%s.\n", firstNA,
		(firstNA == 0) ? " <=> *no* NA/NaN" : "");
    if(firstNA) { // anyNA(x)
	Rboolean NA_pos = TRUE;
	switch(na_action) {
        case NA_BIG_alternate_M:
	    NA_pos = FALSE; // <<-- "M"inus: *not* positive
	    // no break; --> continue
        case NA_BIG_alternate_P: {
	    xx = (double *) R_alloc(n, sizeof(double));
	    for(R_xlen_t i=0; i < n; i++) {
		if(ISNAN(x[i])) {
		    // replace NaN with +/- BIG (< Inf!), switching sign every time:
		    xx[i] = (NA_pos ? BIG_dbl : -BIG_dbl);
		    NA_pos = !NA_pos; // switch
		} else
		    xx[i] = x[i];
	    }
	    break;
	}
	case NA_OMIT: {
	    R_xlen_t i1 = firstNA-1; // firstNA is "1-index"
	    // xx <- x[!is.na(x)] :
	    xx = (double *) R_alloc(n-1, sizeof(double)); // too much if sum(is.na(.)) > 1
	    if(i1 > 1) Memcpy(xx, x, i1-1);
	    for(R_xlen_t i=i1, ix=i; i < n; i++) { //  i-ix == n-nn  {identity}
		if(ISNAN(x[i])) // drop NA/NaN and shift all to the left by 1 :
		    nn--; // nn + (i-ix) == n
		else
		    xx[ix++] = x[i];
	    } // --> now  xx[1:nn] == x[!is.na(x)]
	    break;
	}
	case NA_FAIL:
	    error(_("runmed(x, .., na.action=\"na.fail\"): have NAs starting at x[%ld]"),
		  firstNA);
	default:
	    error(_("runmed(): invalid 'na.action'"));
	}

    } else { // no NAs: xx just points to x; wont be modified
	xx = x;
    }

    SEXP ans = PROTECT(allocVector(REALSXP, n));

    if (type == 1) {
	if (IS_LONG_VEC(sx))
	    error("long vectors are not supported for algorithm = \"Turlach\"");
	Trunmed(xx, REAL(ans), nn, k, end_rule, print_level);
    } else {
	Srunmed(xx, REAL(ans), nn, k, end_rule, print_level);
    }
    if(firstNA) {
	double *median = REAL(ans);
	switch(na_action) {
        case NA_BIG_alternate_P:
        case NA_BIG_alternate_M: { // revert the +-BIG replacements
	    for(R_xlen_t i=firstNA-1; i < n; i++)
		if(ISNAN(x[i]) && !ISNAN(median[i]) && fabs(median[i]) == BIG_dbl)
		    median[i] = x[i];
	    break;
	}
	case NA_OMIT: { /* fill the shortened median[] series into the result,
			   putting x[i] into places i where  ISNAN(x[i]) */
	    if(print_level) {
		Rprintf("na.omit: reduced n = nn = %d.\n", nn);
		if(print_level >= 2) {
		    Rprintf("median[] = ");
		    for(R_xlen_t i=0; i < nn; i++) {
			if(i % 20 == 0) Rprintf("\n[%d] ", i);
			Rprintf("%5g", median[i]);
		    }
		    Rprintf("\n");
		}
	    }
	    double *med = (double *)R_alloc(nn, sizeof(double));
	    if(nn) Memcpy(med, median, nn);
	    for(R_xlen_t i=firstNA-1, ix=i; i < n; i++) {
		if(ISNAN(x[i])) {
		    median[i] = x[i];
		} else {
		    median[i] = med[ix++];
		}
	    }
	    break;
	}
	default: error(_("na_action logic error (%d), please report!"), na_action);
	}
    }
    UNPROTECT(1);
    return ans;
}
