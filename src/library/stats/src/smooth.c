/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2016   The R Core Team.
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

/* Tukey Median Smoothing */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for abs */
#include <math.h>

#include <Rinternals.h> /* Arith.h, Boolean.h, Error.h, Memory.h .. */

typedef enum {
    sm_NO_ENDRULE, sm_COPY_ENDRULE, sm_TUKEY_ENDRULE
} R_SM_ENDRULE;

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

static double med3(double u, double v, double w)
{
    /* Median(u,v,w): */
    if((u <= v && v <= w) ||
       (u >= v && v >= w)) return v;
    if((u <= w && w <= v) ||
       (u >= w && w >= v)) return w;
    /* else */ return u;
}
/* Note: Velleman & Hoaglin use a smarter version,  which returns "change"
   ----
   and change = TRUE, when  med3(u,v,w) != v   ==> makes "R" (in "3R") faster
*/
static int imed3(double u, double v, double w)
{
    /* Return (Index-1) of  median(u,v,w) , i.e.,
       -1 : u
	0 : v
	1 : w
     */
    if((u <= v && v <= w) ||
       (u >= v && v >= w)) return 0;
    if((u <= w && w <= v) ||
       (u >= w && w >= v)) return 1;
    /* else */ return -1;
}

static Rboolean sm_3(double *x, double *y, R_xlen_t n, int end_rule)
{
    /* y[] := Running Median of three (x) = "3 (x[])" with "copy ends"
     * ---  return chg := ( y != x ) */
    R_xlen_t i;
    int j;
    Rboolean chg = FALSE;

    if (n <= 2) {
        for(i=0; i < n; i++)
	   y[i] = x[i];
        return FALSE;
    }

    for(i = 1; i < n-1; i++) {
	j = imed3(x[i-1], x[i], x[i+1]);
	y[i] = x[i + j];
	chg = chg || j;
    }
/* [y, chg]  :=  sm_DO_ENDRULE(x, y, end_rule, chg) : */
#define sm_DO_ENDRULE(y)						\
    switch(end_rule) {							\
    case sm_NO_ENDRULE:							\
	   /* do nothing : don't even assign them */ break;		\
									\
    case sm_COPY_ENDRULE: /* 1 */					\
	   y[0] = x[0];							\
	   y[n-1] = x[n-1];						\
	   break;							\
									\
    case sm_TUKEY_ENDRULE: /* 2 */					\
	   y[0] = med3(3*y[1] - 2*y[2], x[0], y[1]);			\
	   chg = chg || (y[0] != x[0]);					\
	   y[n-1] = med3(y[n-2], x[n-1], 3*y[n-2] - 2*y[n-3]);		\
	   chg = chg || (y[n-1] != x[n-1]);				\
	   break;							\
									\
    default:								\
	   error(_("invalid end-rule for running median of 3: %d"),	\
		 end_rule);						\
    }

    sm_DO_ENDRULE(y);

    return chg;
}

static int sm_3R(double *x, double *y, double *z, R_xlen_t n, int end_rule)
{
    /* y[] := "3R"(x) ; 3R = Median of three, repeated until convergence */
    int iter;
    Rboolean chg;

    iter = chg = sm_3(x, y, n, sm_COPY_ENDRULE);

    while(chg) {
	if((chg = sm_3(y, z, n, sm_NO_ENDRULE))) {
	    iter++;
	    for(R_xlen_t i=1; i < n-1; i++)
		y[i] = z[i];
	}
    }

    if (n > 2) sm_DO_ENDRULE(y);/* =>  chg = TRUE  iff  ends changed */

    return(iter ? iter : chg);
    /* = 0   <==>  only one "3" w/o any change
       = 1   <==>  either ["3" w/o change + endchange]
		   or	  [two "3"s, 2nd w/o change  ]
    */
}


static Rboolean sptest(double *x, R_xlen_t i)
{
    /* Split test:
       Are we at a /-\ or \_/ location => split should be made ?
     */
    if(x[i] != x[i+1]) return FALSE;
    if((x[i-1] <= x[i] && x[i+1] <= x[i+2]) ||
       (x[i-1] >= x[i] && x[i+1] >= x[i+2])) return FALSE;
    /* else */ return TRUE;
}


static Rboolean sm_split3(double *x, double *y, R_xlen_t n, Rboolean do_ends)
{
    /* y[] := S(x[])  where S() = "sm_split3"  */
    R_xlen_t i;
    Rboolean chg = FALSE;

    for(i=0; i < n; i++)
	y[i] = x[i];

    if (n <= 4) return FALSE;

    /* Colin Goodall doesn't do splits near ends
       in spl() in Statlib's "smoother" code !! */
    if(do_ends && sptest(x, 1)) {
	chg = TRUE;
	y[1] = x[0];
	y[2] = med3(x[2], x[3], 3*x[3] - 2*x[4]);
    }

    for(i=2; i < n-3; i++)
	if(sptest(x, i)) { /* plateau at x[i] == x[i+1] */
	    int j;
	    /* at left : */
	    if(-1 < (j = imed3(x[i ], x[i-1], 3*x[i-1] - 2*x[i-2]))) {
		y[i]   = /* med3(.) = */ (j == 0)? x[i-1] : 3*x[i-1] - 2*x[i-2];
		chg = y[i] != x[i];
	    }
	    /* at right : */
	    if(-1 < (j = imed3(x[i+1], x[i+2], 3*x[i+2] - 2*x[i+3]))) {
		y[i+1] = /* med3(.) = */ (j == 0)? x[i+2] : 3*x[i+2] - 2*x[i+3];
		chg = y[i+1] != x[i+1];
	    }
	}
    if(do_ends && sptest(x, n-3)) {
	chg = TRUE;
	y[n-2] = x[n-1];
	y[n-3] = med3(x[n-3], x[n-4], 3*x[n-4] - 2*x[n-5]);
    }
    return(chg);
}

static int sm_3RS3R(double *x, double *y, double *z, double *w, R_xlen_t n,
	     int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3R S 3R"(x[1:n]);  z = "work"; */
    int iter;
    Rboolean chg;

    iter =  sm_3R    (x, y, z, n, end_rule);
    chg  =  sm_split3(y, z, n, split_ends);
    if(chg)
	iter += sm_3R(z, y, w, n, end_rule);
    /* else y == z already */
    return(iter + (int)chg);
}

static int sm_3RSS(double *x, double *y, double *z, R_xlen_t n,
	    int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3RSS"(x[1:n]);  z = "work"; */
    int iter;
    Rboolean chg;

    iter = sm_3R    (x, y, z, n, end_rule);
    chg =  sm_split3(y, z, n, split_ends);
    if(chg)
	sm_split3(z, y, n, split_ends);
    /* else  y == z already */
    return(iter + (int)chg);
}

static int sm_3RSR(double *x, double *y, double *z, double *w, R_xlen_t n,
	    int end_rule, Rboolean split_ends)
{
    /* y[1:n] := "3RSR"(x[1:n]);  z := residuals; w = "work"; */

/*== "SR" (as follows) is stupid ! (MM) ==*/

    R_xlen_t i;
    int iter;
    Rboolean chg, ch2;

    iter = sm_3R(x, y, z, n, end_rule);

    do {
	iter++;
	chg = sm_split3(y, z, n, split_ends);
	ch2 = sm_3R(z, y, w, n, end_rule);
	chg = chg || ch2;

	if(!chg) break;
	if(iter > 2*n) break;/* INF.LOOP stopper */
	for(i=0; i < n; i++)
	    z[i] = x[i] - y[i];

    } while (chg);

    return(iter);
}


/*-------- These are  called from R : -----------*/

#include <Rinternals.h>
SEXP Rsm(SEXP x, SEXP stype, SEXP send)
{
    int iend = asInteger(send), type = asInteger(stype);
    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SEXP y = allocVector(REALSXP, n);
    SET_VECTOR_ELT(ans, 0, y);
    SEXP nm = allocVector(STRSXP, 2);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("y"));
    if (type <= 5) {
	int iter = 0 /* -Wall */;
	switch(type){
	case 1:
	{
	    double *z = (double *) R_alloc(n, sizeof(double));
	    double *w = (double *) R_alloc(n, sizeof(double));
	    iter = sm_3RS3R(REAL(x), REAL(y), z, w, n, abs(iend),
			      iend ? TRUE : FALSE);
	    break;
	}
	case 2:
	{
	    double *z = (double *) R_alloc(n, sizeof(double));
	    iter = sm_3RSS(REAL(x), REAL(y), z, n, abs(iend),
			      iend ? TRUE : FALSE);
	    break;
	}
	case 3:
	{
	    double *z = (double *) R_alloc(n, sizeof(double));
	    double *w = (double *) R_alloc(n, sizeof(double));
	    iter = sm_3RSR(REAL(x), REAL(y), z, w, n, abs(iend),
			   iend ? TRUE : FALSE);
	    break;
	}
	case 4: // "3R"
	{
	    double *z = (double *) R_alloc(n, sizeof(double));
	    iter = sm_3R(REAL(x), REAL(y), z, n, iend);
	}
	    break;
	case 5: // "3"
	    iter = sm_3(REAL(x), REAL(y), n, iend);
	}
	SET_VECTOR_ELT(ans, 1, ScalarInteger(iter));
	SET_STRING_ELT(nm, 1, mkChar("iter"));
    } else { // type > 5  ==> =~ "S"
	int changed = sm_split3(REAL(x), REAL(y), n, (Rboolean) iend);
	SET_VECTOR_ELT(ans, 1, ScalarLogical(changed));
	SET_STRING_ELT(nm, 1, mkChar("changed"));
    }
    UNPROTECT(1);
    return ans;
}
