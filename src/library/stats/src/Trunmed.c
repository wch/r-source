/*
 * Copyright (C) 2012-2019  The R Core Team
 * Copyright (C) 2003 ff.   The R Foundation
 * Copyright (C) 2000-2 Martin Maechler <maechler@stat.math.ethz.ch>
 * Copyright (C) 1995   Berwin A. Turlach <berwin@alphasun.anu.edu.au>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, a copy is available at
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* These routines implement a running median smoother according to the
 * algorithm described in Haerdle und Steiger (1995),
 *			  DOI:10.2307/2986349 , see ../man/runmed.Rd
 *
 * The current implementation does not use any global variables!
 */

/* Changes for R port by Martin Maechler ((C) above):
 *
 *  s/long/int/			R uses int, not long (as S does)
 *  s/void/static void/		most routines are internal
 *
 * - Added  print_level  and end_rule  arguments
 * - Allow to deal with  NA / NaN {via ISNAN()}
 */

/* Variable	name	descri- | Identities from paper
 * name here	paper	ption   | (1-indexing)
 * ---------    -----	-----------------------------------
 * window[]      H      the array containing the double heap
 * data[]        X      the data (left intact)
 * ...		 i	1st permuter  H[i[m]]    == X[i + m]
 * ...		 j	2nd permuter  X[i +j[m]] == H[m]
 */

#include <math.h>
#include <R_ext/RS.h>	       	/* for Memcpy */

static void
swap(int l, int r, double *window, int *outlist, int *nrlist, int print_level)
{
    /* swap positions `l' and `r' in window[] and nrlist[]
     *
     * ---- Used in R_heapsort() and many other routines
     */
    if(print_level >= 3) Rprintf(" SW(%d,%d) ", l,r);
    double tmp = window[l]; window[l] = window[r];  window[r] = tmp;
    int nl = nrlist[l],
	nr = nrlist[r];
    nrlist[l] = nr; outlist[nr] = l;
    nrlist[r] = nl; outlist[nl] = r;
}

//------------------------ 1. inittree() and auxiliaries ----------------------
static void
siftup(int l, int r, double *window, int *outlist, int *nrlist, int print_level)
{
    /* Used only in R_heapsort() */
    int i = l, j,
	nrold = nrlist[i];
    double x  = window[i];
    if(print_level >= 2) Rprintf("siftup(%d,%d): x=%g: ", l,r, x);
    while ((j = 2*i) <= r) {
	if (j < r)
	    if (window[j] < window[j+1])
		j++;
	if (x >= window[j])
	    break;

	window[i]	   = window[j];
	outlist[nrlist[j]] = i;
	nrlist[i]	   = nrlist[j];
	i = j;
    }
    window[i]	   = x;
    outlist[nrold] = i;
    nrlist[i]	   = nrold;
    if(print_level >= 2) Rprintf("-> nrlist[i=%d] := %d\n", i, nrold);
}

static void
R_heapsort(int low, int up, double *window, int *outlist, int *nrlist,
	   int print_level)
{
    int l = (up/2) + 1,
	u = up;
    if(print_level)
	Rprintf("R_heapsort(%d, %d,..): l=%d:\n", low, up, l);
    while(l > low) {
	if(print_level >= 2) Rprintf(" l > low: ");
	l--; // l >= low :
	siftup(l, u, window, outlist, nrlist, print_level);
    } // => l <= low
    while(u > low) {
	if(print_level >= 2) Rprintf(" u > low: ");
	swap(l, u, window, outlist, nrlist, print_level);
	u--; // u >= low :
	siftup(l, u, window, outlist, nrlist, print_level);
    }
}

static void
inittree(R_xlen_t n, int k, int k2,
	 const double *data,
	 // --> initialize these three vectors:
	 double *window, int *outlist, int *nrlist,
	 int print_level)
{
    // use 1-indexing for our 3 arrays {window, nrlist, outlist}
    for(int i=1; i <= k; i++) {
	window[i] = data[i-1];
	nrlist[i] = (outlist[i] = i);
    }
    // MM: not all  2k+1  entries of nrlist[] are used, it seems
    if(print_level >= 1) {
	int    n0 = -12345; // to be recognized easily ...
	double w0 = -80.08; //  (ditto)
	nrlist[0] = n0;
	window[0] = w0;
	for(int j=k+1; j <= 2*k; j++) {
	    nrlist[j] = n0;
	    window[j] = w0;
	}
    }

    // sort window[1:k] = data["1:k"]   [*only* called here] :
    R_heapsort(1, k, window, outlist, nrlist, print_level);

    double big = fabs(window[k]);
    if (big < fabs(window[1]))
	big = fabs(window[1]);
    // now   big := max |X[1..k]|  or +BIG  if(first_NA <= k), i.e., data had NA|NaN
    for(int i=k; i > 0; i--) {
	window[i+k2] = window[i];
	nrlist[i+k2] = nrlist[i]-1;
    }
    // outlist[0:(k-1)] :=  (shift down by 1  and offset by k2)
    for(int i=0; i < k; i++)
	outlist[i] = outlist[i+1] + k2;

    // maybe increase 'big'
    for(R_xlen_t i=k; i < n; i++)
	if (big < fabs(data[i]))
	    big = fabs(data[i]);

    /* big == max(|data_i|,  i = 1,..,n) */
    big = 1 + 2. * big;/* such that -big < data[] < +big
			  (strictly, only if no +/-Inf in data! */
    int k2p1 = k2+1;
    // window[0] := ..
    for(int i=0; i < k2p1; i++) {
	window[i]	 = -big;
	window[k+k2p1+i] =  big;
    }

/* For Printing `diagnostics' : */
#define Rm_PR(_F_,_A_, _k_)	for(int j = 0; j <= _k_; j++) Rprintf(_F_, _A_)
#define RgPRINT_j(A_J, _k_)	Rm_PR("% 6.4g", A_J, _k_); Rprintf("\n")
#define RdPRINT_j(A_J, _k_)	Rm_PR("% 6d"  , A_J, _k_); Rprintf("\n")
//--- smart printing of "Big" / "2B"  [BIG_dbl := .... -> ./Srunmed.c ]
#define RwwPRINTj(A_J, _k_)					\
    for(int j = 0; j <= _k_; j++) {				\
	if     (A_J ==  BIG_dbl  ) Rprintf("%6s", "+BIG");	\
	else if(A_J == -BIG_dbl  ) Rprintf("%6s", "-BIG");	\
	else if(A_J ==  2*BIG_dbl) Rprintf("%6s", "+2B");	\
	else if(A_J == -2*BIG_dbl) Rprintf("%6s", "-2B");	\
	else                       Rprintf("% 6.4g", A_J);	\
    }								\
    Rprintf("\n")

#define R_PRINT_4vec()						\
    Rprintf(" %9s: ","j");         RdPRINT_j(        j, 2*k);	\
    Rprintf(" %9s: ","window []"); RwwPRINTj(window[j], 2*k);	\
    Rprintf(" %9s: "," nrlist[]"); RdPRINT_j(nrlist[j], 2*k);	\
    Rprintf(" %9s: ","outlist[]"); RdPRINT_j(outlist[j], k )

    /* window[], outlist[], and nrlist[] are all 1-based (indices) */
    if(print_level) {
	R_PRINT_4vec();
    }

} /* inittree*/

//------------------------ 2. runmedint() and auxiliaries -------------------
static void
toroot(int outvirt, int k, R_xlen_t nrnew, int outnext,
       const double *data, double *window, int *outlist, int *nrlist,
       int print_level)
{
    if(print_level >= 2)
	Rprintf("  toroot(%d,%d, nn=%d, outn=%d) ", outvirt, k, (int) nrnew, outnext);
    int father;
    do {
	father                    = outvirt/2;
	window[outvirt+k]	  = window[father+k];
	outlist[nrlist[father+k]] = outvirt+k;
	if(print_level >= 3)
	    Rprintf(" nrl[%d] := nrl[%d] = %d;", outvirt+k, father+k, nrlist[father+k]);
	nrlist[outvirt+k]	  = nrlist[father+k];
	outvirt			  = father;
    } while (father != 0);
    if(print_level >= 2) Rprintf("\n  ");
    window[k]	     = data[nrnew];
    outlist[outnext] = k;
    nrlist[k]	     = outnext;
}

static void
downtoleave(int outvirt, int k,
	    double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf(" downtoleave(%d, %d)  ", outvirt,k);
    for(;;) {
	int childl = outvirt*2,
	    childr = childl -1;
	if (window[childl+k] < window[childr+k])
	    childl = childr;
	if (window[outvirt+k] >= window[childl+k])
	    break;
	/* seg.fault may happen here: invalid outvirt+k/ childl+k ? */
	swap(outvirt+k, childl+k, window, outlist, nrlist, print_level);
	outvirt = childl;
    }
    if(print_level >= 2) Rprintf("\n ");
}

static void
uptoleave(int outvirt, int k,
	  double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf(" uptoleave(%d, %d)  ", outvirt,k);
    for(;;) {
	int childl = outvirt*2,
	    childr = childl +1;
	if (window[childl+k] > window[childr+k])
	    childl = childr;
	if (window[outvirt+k] <= window[childl+k])
	    break;
	/* seg.fault may happen here: invalid outvirt+k/ childl+k ? */
	swap(outvirt+k, childl+k, window, outlist, nrlist, print_level);
	outvirt = childl;
    }
    if(print_level >= 2) Rprintf("\n ");
}

static void
upperoutupperin(int outvirt, int k,
		double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf("UpperoutUPPERin(%d, %d)\n  ", outvirt,k);
    uptoleave(outvirt, k, window, outlist, nrlist, print_level);
    int father = outvirt/2;
    while (window[outvirt+k] < window[father+k]) {
	if(print_level >= 2) Rprintf(" UpperoutUP(win[%d]):  ", outvirt+k);
	swap(outvirt+k, father+k, window, outlist, nrlist, print_level);
	outvirt = father;
	father	= outvirt/2;
    }
    if(print_level >= 2) Rprintf("\n");
}

static void
upperoutdownin(int outvirt, int k, R_xlen_t nrnew, int outnext,
	       const double *data, double *window, int *outlist, int *nrlist,
	       int print_level)
{
    if(print_level >= 2) Rprintf("__upperoutDOWNin(%d, %d, ..)\n  ", outvirt,k);
    toroot(outvirt, k, nrnew, outnext, data, window, outlist, nrlist, print_level);
    if(window[k] < window[k-1]) {
	if(print_level >= 2) Rprintf(" upperoutDN(win[%d]):  ", k);
	swap(k, k-1, window, outlist, nrlist, print_level);
	downtoleave(/*outvirt = */ -1, k, window, outlist, nrlist, print_level);
    }
}

static void
downoutdownin(int outvirt, int k,
	      double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf("DownoutDOWNin(%d, %d)\n  ", outvirt,k);
    downtoleave(outvirt, k, window, outlist, nrlist, print_level);
    int father = outvirt/2;
    while (window[outvirt+k] > window[father+k]) {
	swap(outvirt+k, father+k, window, outlist, nrlist, print_level);
	outvirt = father;
	father	= outvirt/2;
    }
    // if(print_level >= 2) Rprintf("\n");
}

static void
downoutupperin(int outvirt, int k, R_xlen_t nrnew, int outnext,
	       const double *data, double *window, int *outlist, int *nrlist,
	       int print_level)
{
    if(print_level >= 2) Rprintf("__downoutUPPERin(%d, %d, ..)\n  ", outvirt,k);
    toroot(outvirt, k, nrnew, outnext, data, window, outlist, nrlist, print_level);
    if(window[k] > window[k+1]) {
	swap(k, k+1, window, outlist, nrlist, print_level);
	uptoleave(/*outvirt = */ +1, k, window, outlist, nrlist, print_level);
    }
}

static void
wentoutone(int k, double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf(" wentOUT_1(%d)\n  ", k);
    swap(k, k+1, window, outlist, nrlist, print_level);
    uptoleave(/*outvirt = */ +1, k, window, outlist, nrlist, print_level);
}

static void
wentouttwo(int k, double *window, int *outlist, int *nrlist, int print_level)
{
    if(print_level >= 2) Rprintf(" wentOUT_2(%d)\n  ", k);
    swap(k, k-1, window, outlist, nrlist, print_level);
    downtoleave(/*outvirt = */ -1, k, window, outlist, nrlist, print_level);
}


static void
runmedint(R_xlen_t n, int k, int k2, const double *data, double *median,
	  double *window, int *outlist, int *nrlist,
	  int end_rule, int print_level)
{
    /* Running Median of `k' ,  k == 2*k2 + 1 *
     * end_rule == 0: leave values at the end,
     *          otherwise: "constant" end values
     */
    int outnext, out, outvirt;

    if(end_rule)
	for(int i = 0; i <= k2; median[i++] = window[k]);
    else {
	for(int i = 0; i <  k2; median[i] = data[i], i++);
	median[k2] = window[k];
    }
    R_xlen_t every_i;
    if(print_level == 2)
	every_i = (n > 100) ? n/10 : 10;

    outnext = 0;
    for(R_xlen_t i = k2+1; i < n-k2; i++) {/* compute (0-index) median[i] == X*_{i+1} */
	out  = outlist[outnext];
	R_xlen_t nrnew = i+k2;
	window[out] = data[nrnew];
	outvirt	= out-k;
	if (out > k)
	    if(data[nrnew] >= window[k])
		upperoutupperin(outvirt, k, window, outlist, nrlist, print_level);
	    else
		upperoutdownin(outvirt, k, nrnew, outnext,
			       data, window, outlist, nrlist, print_level);
	else if(out < k)
	    if(data[nrnew] < window[k])
		downoutdownin(outvirt, k, window, outlist, nrlist, print_level);
	    else
		downoutupperin(outvirt, k, nrnew, outnext,
			       data, window, outlist, nrlist, print_level);
	else if(window[k] > window[k+1])
	    wentoutone(k, window, outlist, nrlist, print_level);
	else if(window[k] < window[k-1])
	    wentouttwo(k, window, outlist, nrlist, print_level);
	median[i] = window[k];
	outnext	  = (outnext+1) % k;
	if(print_level >= 2) {
	    Rprintf("i=%2d (out=%2d, *virt=%2d): med[i] := window[k]=%11g, outnext=%3d\n",
		    i, out, outvirt, median[i], outnext);
	    if(print_level >= 3 || (i % every_i) == 0) {
		R_PRINT_4vec();
	    }
	}
    }
    if(print_level >= 2) Rprintf("\n");

    if(end_rule)
	for(R_xlen_t i = n-k2; i < n; median[i++] = window[k]);
    else
	for(R_xlen_t i = n-k2; i < n; median[i] = data[i], i++);

}/* runmedint() */

//------------------------ 3. Trunmed() --------------------------------------

// Main function called from runmed() in ./Srunmed.c :
static void Trunmed(const double *x,// (n)
		    double *median, // (n)
		    R_xlen_t n,/* = length(x) */
		    int k, /* is odd <= n */
		    int end_rule, int print_level)
{
    int k2 = (k - 1)/2; // always odd  k == 2 * k2 + 1
    double *window = (double *) R_alloc(2*k + 1, sizeof(double));
    int    *nrlist = (int *)    R_alloc(2*k + 1, sizeof(int)),
	  *outlist = (int *)    R_alloc(  k + 1, sizeof(int));

    inittree(n, k, k2, x,
	     /* initialize the 3 vectors: */ window, outlist, nrlist,
	     print_level);

    runmedint(n, k, k2, x, median, window, outlist, nrlist,
	      end_rule, print_level);

}
