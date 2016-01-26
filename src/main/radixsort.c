/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016   The R Core Team
 *
 *  Based on code donated from the data.table package
 *  (C) 2006-2015 Matthew Dowle and Arun Srinivasan.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

// gs = groupsizes e.g.23, 12, 87, 2, 1, 34,...
static int *gs[2] = { NULL };
//two vectors flip flopped:flip and 1 - flip
static int flip = 0;
//allocated stack size
static int gsalloc[2] = { 0 };
static int gsngrp[2] = { 0 };
//max grpn so far
static int gsmax[2] = { 0 };
//max size of stack, set by do_radixsort to nrows
static int gsmaxalloc = 0;
//switched off for last arg unless retGrp==TRUE
static Rboolean stackgrps = TRUE;
// TRUE for setkey, FALSE for by=
static Rboolean sortStr = TRUE;
// used by do_radixsort and [i|d|c]sort to reorder order.
// not needed if narg==1
static int *newo = NULL;
// =1, 0, -1 for TRUE, NA, FALSE respectively.
// Value rewritten inside do_radixsort().
static int nalast = -1;
// =1, -1 for ascending and descending order respectively
static int order = 1;

//replaced n < 200 with n < N_SMALL.Easier to change later
#define N_SMALL 200
// range limit for counting sort. Should be less than INT_MAX
// (see setRange for details)
#define N_RANGE 100000

static SEXP *saveds = NULL;
static R_len_t *savedtl = NULL, nalloc = 0, nsaved = 0;

static void savetl_init()
{
    if (nsaved || nalloc || saveds || savedtl)
	error("Internal error: savetl_init checks failed (%d %d %p %p).",
	      nsaved, nalloc, saveds, savedtl);
    nsaved = 0;
    nalloc = 100;
    saveds = (SEXP *) malloc(nalloc * sizeof(SEXP));
    if (saveds == NULL)
	error("Could not allocate saveds in savetl_init");
    savedtl = (R_len_t *) malloc(nalloc * sizeof(R_len_t));
    if (savedtl == NULL) {
	free(saveds);
	error("Could not allocate saveds in savetl_init");
    }
}

static void savetl_end()
{
    // Can get called if nothing has been saved yet (nsaved == 0), or
    // even if _init() has not been called yet (pointers NULL). Such as
    // to clear up before error. Also, it might be that nothing needed
    // to be saved anyway.
    for (int i = 0; i < nsaved; i++)
	SET_TRUELENGTH(saveds[i], savedtl[i]);
    free(saveds);  // does nothing on NULL input
    free(savedtl);
    nsaved = nalloc = 0;
    saveds = NULL;
    savedtl = NULL;
}


static void savetl(SEXP s)
{
    if (nsaved >= nalloc) {
	nalloc *= 2;
	char *tmp;
	tmp = (char *) realloc(saveds, nalloc * sizeof(SEXP));
	if (tmp == NULL) {
	    savetl_end();
	    error("Could not realloc saveds in savetl");
	}
	saveds = (SEXP *) tmp;
	tmp = (char *) realloc(savedtl, nalloc * sizeof(R_len_t));
	if (tmp == NULL) {
	    savetl_end();
	    error("Could not realloc savedtl in savetl");
	}
	savedtl = (R_len_t *) tmp;
    }
    saveds[nsaved] = s;
    savedtl[nsaved] = TRUELENGTH(s);
    nsaved++;
}

// http://gcc.gnu.org/onlinedocs/cpp/Swallowing-the-Semicolon.html#Swallowing-the-Semicolon
#define Error(...) do {savetl_end(); error(__VA_ARGS__);} while(0)
#undef warning
// since it can be turned to error via warn = 2
#define warning(...) Do not use warning in this file
/* use malloc/realloc (not Calloc/Realloc) so we can trap errors
   and call savetl_end() before the error(). */

static void growstack(int newlen)
{
    // no link to icount range restriction,
    // just 100,000 seems a good minimum at 0.4MB
    if (newlen == 0) newlen = 100000;
    if (newlen > gsmaxalloc) newlen = gsmaxalloc;
    gs[flip] = realloc(gs[flip], newlen * sizeof(int));
    if (gs[flip] == NULL)
	Error("Failed to realloc working memory stack to %d*4bytes (flip=%d)",
	      newlen, flip);
    gsalloc[flip] = newlen;
}

static void push(int x)
{
    if (!stackgrps || x == 0)
	return;
    if (gsalloc[flip] == gsngrp[flip])
	growstack(gsngrp[flip] * 2);
    gs[flip][gsngrp[flip]++] = x;
    if (x > gsmax[flip])
	gsmax[flip] = x;
}

static void mpush(int x, int n)
{
    if (!stackgrps || x == 0)
	return;
    if (gsalloc[flip] < gsngrp[flip] + n)
	growstack((gsngrp[flip] + n) * 2);
    for (int i = 0; i < n; i++)
	gs[flip][gsngrp[flip]++] = x;
    if (x > gsmax[flip])
	gsmax[flip] = x;
}

static void flipflop()
{
    flip = 1 - flip;
    gsngrp[flip] = 0;
    gsmax[flip] = 0;
    if (gsalloc[flip] < gsalloc[1 - flip])
	growstack(gsalloc[1 - flip] * 2);
}

static void gsfree()
{
    free(gs[0]);
    free(gs[1]);
    gs[0] = NULL;
    gs[1] = NULL;
    flip = 0;
    gsalloc[0] = gsalloc[1] = 0;
    gsngrp[0] = gsngrp[1] = 0;
    gsmax[0] = gsmax[1] = 0;
    gsmaxalloc = 0;
}

#ifdef TIMING_ON
// many calls to clock() can be expensive,
// hence compiled out rather than switch(verbose)
#include <time.h>
#define NBLOCK 20
static clock_t tblock[NBLOCK], tstart;
static int nblock[NBLOCK];
#define TBEG() tstart = clock();
#define TEND(i) tblock[i] += clock()-tstart; nblock[i]++; tstart = clock();
#else
#define TBEG()
#define TEND(i)
#endif

static int range, off; // used by both icount and do_radixsort
static void setRange(int *x, int n)
{
    int xmin = NA_INTEGER, xmax = NA_INTEGER;
    double overflow;

    off = (nalast == 1) ? 0 : 1;   // nalast^decreasing ? 0 : 1;
    // off = 0 will store values starting from index 0. NAs will go last.
    // off = 1 will store values starting from index 1. NAs will be at 0th index.
    int i = 0;
    while(i < n && x[i] == NA_INTEGER) i++;
    if (i < n) xmax = xmin = x[i];
    for (; i < n; i++) {
	int tmp = x[i];
	if (tmp == NA_INTEGER)
	    continue;
	if (tmp > xmax)
	    xmax = tmp;
	else if (tmp < xmin)
	    xmin = tmp;
    }
    // all NAs, nothing to do
    if (xmin == NA_INTEGER) {
	range = NA_INTEGER;
	return;
    }
    // ex: x=c(-2147483647L, NA_integer_, 1L) results in overflowing int range.
    overflow = (double) xmax - (double) xmin + 1;
    // detect and force iradix here, since icount is out of the picture
    if (overflow > INT_MAX) {
	range = INT_MAX;
	return;
    }

    range = xmax - xmin + 1;
    // so that  off+order*x[i]  (below in icount)
    // => (x[i]-xmin)+0|1  or  (xmax-x[i])+0|1
    off = order == 1 ? -xmin + off : xmax + off;

    return;
}

// x*order results in integer overflow when -1*NA,
// so careful to avoid that here :
static inline int icheck(int x)
{
    // if nalast == 1, NAs must go last.
    return ((nalast != 1) ? ((x != NA_INTEGER) ? x*order : x) :
	    ((x != NA_INTEGER) ? (x*order) - 1 : INT_MAX));
}


static void icount(int *x, int *o, int n)
/* Counting sort:
   1. Places the ordering into o directly, overwriting whatever was there
   2. Doesn't change x
   3. Pushes group sizes onto stack
*/
{
    int napos = (nalast == 1) ? range : 0;  // take care of 'nalast' argument
    // static is IMPORTANT, counting sort is called repetitively.
    static unsigned int counts[N_RANGE + 1] = { 0 };
    /* counts are set back to 0 at the end efficiently. 1e5 = 0.4MB i.e
       tiny. We'll only use the front part of it, as large as range. So it's
       just reserving space, not using it. Have defined N_RANGE to be 100000.*/
    if (range > N_RANGE)
	Error("Internal error: range = %d; isorted cannot handle range > %d",
	      range, N_RANGE);
    for (int i = 0; i < n; i++) {
	// For nalast=NA case, we won't remove/skip NAs, rather set 'o' indices
	// to 0. subset will skip them. We can't know how many NAs to skip
	// beforehand - i.e. while allocating "ans" vector
	if (x[i] == NA_INTEGER)
	    counts[napos]++;
	else
	    counts[off + order * x[i]]++;
    }
    
    int tmp = 0;
    for (int i = 0; i <= range; i++) 
        /* no point in adding tmp < n && i <= range, since range includes max, 
           need to go to max, unlike 256 loops elsewhere in radixsort.c */
    {
	if (counts[i]) {
	    // cumulate but not through 0's.
	    // Helps resetting zeros when n < range, below.
	    push(counts[i]);
	    counts[i] = (tmp += counts[i]);
	}
    }
    for (int i = n - 1; i >= 0; i--) {
	// This way na.last=TRUE/FALSE cases will have just a
	// single if-check overhead.
	o[--counts[(x[i] == NA_INTEGER) ? napos :
		   off + order * x[i]]] = (int) (i + 1);
    }
    // nalast = 1, -1 are both taken care already.
    if (nalast == 0)
	// nalast = 0 is dealt with separately as it just sets o to 0
	for (int i = 0; i < n; i++)
	    o[i] = (x[o[i] - 1] == NA_INTEGER) ? 0 : o[i];
    // at those indices where x is NA. x[o[i]-1] because x is not modifed here.

    /* counts were cumulated above so leaves non zero.
       Faster to clear up now ready for next time. */
    if (n < range) {
	/* Many zeros in counts already. Loop through n instead,
	   doesn't matter if we set to 0 several times on any repeats */
	counts[napos] = 0;
	for (int i = 0; i < n; i++) {
	    if (x[i] != NA_INTEGER)
		counts[off + order * x[i]] = 0;
	}
    } else
	memset(counts, 0, (range + 1) * sizeof(int));
    return;
}

static void iinsert(int *x, int *o, int n)
/*  orders both x and o by reference in-place. Fast for small vectors,
    low overhead.  don't be tempted to binsearch backwards here, have
    to shift anyway; many memmove would have overhead and do the same
    thing. */
/*  when nalast == 0, iinsert will be called only from within iradix,
    where o[.] = 0 for x[.]=NA is already taken care of */
{
    for (int i = 1; i < n; i++) {
	int xtmp = x[i];
	if (xtmp < x[i - 1]) {
	    int j = i - 1;
	    int otmp = o[i];
	    while (j >= 0 && xtmp < x[j]) {
		x[j + 1] = x[j];
		o[j + 1] = o[j];
		j--;
	    }
	    x[j + 1] = xtmp;
	    o[j + 1] = otmp;
	}
    }
    int tt = 0;
    for (int i = 1; i < n; i++)
	if (x[i] == x[i - 1])
	    tt++;
	else {
	    push(tt + 1);
	    tt = 0;
	}
    push(tt + 1);
}

/*
  iradix is a counting sort performed forwards from MSB to LSB, with
  some tricks and short circuits building on Terdiman and Herf.
  http://codercorner.com/RadixSortRevisited.htm
  http://stereopsis.com/radix.html

  ~ Note they are LSD, but we do MSD here which is more complicated,
    for efficiency.
  ~ NAs need no special treatment as NA is the most negative integer
    in R (checked in init.c once,  for efficiency) so NA naturally sort
    to the front.
  ~ Using 4-pass 1-byte radix for the following reasons :

  * 11-bit (Herf) reduces to 3-passes (3*11=33) yes, and LSD need
    random access to o vector in each pass 1:n so reduction in passes is
    good, but Terdiman's idea to skip a radix if all values are equal
    occurs less the wider the radix. A narrower radix benefits more from that.
    * That's detected here using a single 'if', an improvement on
      Terdiman's exposition of a single loop to find if any count==n
    * The pass through counts bites when radix is wider,
      because we repetitively call this iradix from fastorder forwards.
  *  Herf's parallel histogramming is neat. In 4-pass 1-byte it needs
     4*256 storage, that's  tiny, and can be static. 4*256 << 3*2048.
     4-pass 1-byte is simpler and tighter code than 3-pass 11-bit,
     giving modern optimizers and modern CPUs a better chance.
     We may get lucky anyway, if one or two of the 4-passes are skipped.

  Recall: there are no comparisons at all in counting and radix,
  there is wide random access in each LSD radix pass, though.
*/

// 4 are used for iradix, 8 for dradix and i64radix
static unsigned int radixcounts[8][257] = { {0} };

static int skip[8];
/* global because iradix and iradix_r interact and are called repetitively.
   counts are set back to 0 after each use, to benefit from skipped radix. */
static void *radix_xsub = NULL;
static size_t radix_xsuballoc = 0;

static int *otmp = NULL, otmp_alloc = 0;
static void alloc_otmp(int n)
{
    if (otmp_alloc >= n)
	return;
    otmp = (int *) realloc(otmp, n * sizeof(int));
    if (otmp == NULL)
	Error("Failed to allocate working memory for otmp. Requested %d * %d bytes",
	      n, sizeof(int));
    otmp_alloc = n;
}

// TO DO: save xtmp if possible, see allocs in do_radixsort
static void *xtmp = NULL;
static int xtmp_alloc = 0;
// TO DO: currently always the largest type (double) but
//        could be int if that's all that's needed
static void alloc_xtmp(int n)
{
    if (xtmp_alloc >= n)
	return;
    xtmp = (double *) realloc(xtmp, n * sizeof(double));
    if (xtmp == NULL)
	Error("Failed to allocate working memory for xtmp. Requested %d * %d bytes",
	      n, sizeof(double));
    xtmp_alloc = n;
}

static void iradix_r(int *xsub, int *osub, int n, int radix);

static void iradix(int *x, int *o, int n)
/* As icount :
   Places the ordering into o directly, overwriting whatever was there
   Doesn't change x
   Pushes group sizes onto stack */
{
    int nextradix, itmp, thisgrpn, maxgrpn;
    unsigned int thisx = 0, shift, *thiscounts;

    for (int i = 0; i < n;i++) {
	/* parallel histogramming pass; i.e. count occurrences of
	   0:255 in each byte.  Sequential so almost negligible. */
	// relies on overflow behaviour. And shouldn't -INT_MIN be up in iradix?
	thisx = (unsigned int) (icheck(x[i])) - INT_MIN;
	// unrolled since inside n-loop
	radixcounts[0][thisx & 0xFF]++;
	radixcounts[1][thisx >> 8 & 0xFF]++;
	radixcounts[2][thisx >> 16 & 0xFF]++;
	radixcounts[3][thisx >> 24 & 0xFF]++;
    }
    for (int radix = 0; radix < 4; radix++) {
	/* any(count == n) => all radix must have been that value =>
	   last x (still thisx) was that value */
	int i = thisx >> (radix*8) & 0xFF;
	skip[radix] = radixcounts[radix][i] == n;
	// clear it now, the other counts must be 0 already
	if (skip[radix])
	    radixcounts[radix][i] = 0;
    }

    int radix = 3;  // MSD
    while (radix >= 0 && skip[radix]) radix--;
    if (radix == -1) { // All radix are skipped; one number repeated n times.
	if (nalast == 0 && x[0] == NA_INTEGER)
	    // all values are identical. return 0 if nalast=0 & all NA
	    // because of 'return', have to take care of it here.
	    for (int i = 0; i < n; i++)
		o[i] = 0;
	else
	    for (int i = 0; i < n; i++)
		o[i] = (i + 1);
	push(n);
	return;
    }
    for (int i = radix - 1; i >= 0; i--) {
	if (!skip[i])
	    memset(radixcounts[i], 0, 257 * sizeof(unsigned int));
	/* clear the counts as we only needed the parallel pass for skip[]
	   and we're going to use radixcounts again below. Can't use parallel
	   lower counts in MSD radix, unlike LSD. */
    }
    thiscounts = radixcounts[radix];
    shift = radix * 8;

    itmp = thiscounts[0];
    maxgrpn = itmp;
    for (int i = 1; itmp < n && i < 256; i++) {
	thisgrpn = thiscounts[i];
	if (thisgrpn) {
	    // don't cummulate through 0s, important below.
	    if (thisgrpn > maxgrpn)
		maxgrpn = thisgrpn;
	    thiscounts[i] = (itmp += thisgrpn);
	}
    }
    for (int i = n - 1; i >= 0; i--) {
	thisx = ((unsigned int) (icheck(x[i])) - INT_MIN) >> shift & 0xFF;
	o[--thiscounts[thisx]] = i + 1;
    }

    if (radix_xsuballoc < maxgrpn) {
        // The largest group according to the first non-skipped radix,
        // so could be big (if radix is needed on first arg)
        // TO DO: could include extra bits to divide the first radix
        // up more. Often the MSD has groups in just 0-4 out of 256.
        // free'd at the end of do_radixsort once we're done calling iradix
        // repetitively
        radix_xsub = (int *) realloc(radix_xsub, maxgrpn * sizeof(double));
        if (!radix_xsub)
            Error("Failed to realloc working memory %d*8bytes (xsub in iradix), radix=%d",
                  maxgrpn, radix);
        radix_xsuballoc = maxgrpn;
    }

    // TO DO: can we leave this to do_radixsort and remove these calls??
    alloc_otmp(maxgrpn);
    // TO DO: doesn't need to be sizeof(double) always, see inside
    alloc_xtmp(maxgrpn);

    nextradix = radix - 1;
    while (nextradix >= 0 && skip[nextradix]) nextradix--;
    if (thiscounts[0] != 0)
	Error("Internal error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d",
	      thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (int i = 1; itmp < n && i <= 256; i++) {
        if (thiscounts[i] == 0) continue;
        // undo cumulate; i.e. diff
        thisgrpn = thiscounts[i] - itmp;
        if (thisgrpn == 1 || nextradix == -1) {
            push(thisgrpn);
        } else {
            for (int j = 0; j < thisgrpn; j++)
                // this is why this xsub here can't be the same memory as
                // xsub in do_radixsort.
                ((int *)radix_xsub)[j] = icheck(x[o[itmp+j]-1]);
            // changes xsub and o by reference recursively.
            iradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);
        }
        itmp = thiscounts[i];
        thiscounts[i] = 0;
    }
    if (nalast == 0) // nalast = 1, -1 are both taken care already.
	// nalast = 0 is dealt with separately as it just sets o to 0
	for (int i = 0; i < n; i++)
	    o[i] = (x[o[i] - 1] == NA_INTEGER) ? 0 : o[i];
    // at those indices where x is NA. x[o[i]-1] because x is not
    // modified by reference unlike iinsert or iradix_r
}

static void iradix_r(int *xsub, int *osub, int n, int radix)
// xsub is a recursive offset into xsub working memory above in
// iradix, reordered by reference.  osub is a an offset into the main
// answer o, reordered by reference.  radix iterates 3,2,1,0
{
    int j, itmp, thisx, thisgrpn, nextradix, shift;
    unsigned int *thiscounts;

    // N_SMALL=200 is guess based on limited testing. Needs
    // calibrate().  Was 50 based on sum(1:50)=1275 worst -vs- 256
    // cummulate + 256 memset + allowance since reverse order is
    // unlikely.  when nalast==0, iinsert will be called only from
    // within iradix.
    if (n < N_SMALL) {
	iinsert(xsub, osub, n);
	return;
    }

    shift = radix * 8;
    thiscounts = radixcounts[radix];

    for (int i = 0; i < n; i++) {
	thisx = (unsigned int) xsub[i] - INT_MIN; // sequential in xsub
	thiscounts[thisx >> shift & 0xFF]++;
    }
    itmp = thiscounts[0];
    for (int i = 1; itmp < n && i < 256; i++)
	// don't cummulate through 0s, important below
	if (thiscounts[i])
	    thiscounts[i] = (itmp += thiscounts[i]);
    for (int i = n - 1; i >= 0; i--) {
	thisx = ((unsigned int) xsub[i] - INT_MIN) >> shift & 0xFF;
	j = --thiscounts[thisx];
	otmp[j] = osub[i];
	((int *) xtmp)[j] = xsub[i];
    }
    memcpy(osub, otmp, n * sizeof(int));
    memcpy(xsub, xtmp, n * sizeof(int));

    nextradix = radix - 1;
    while (nextradix >= 0 && skip[nextradix]) nextradix--;
    /* TO DO: If nextradix == -1 AND no further args from do_radixsort AND
       !retGrp, we're done. We have o. Remember to memset thiscounts
       before returning. */

    if (thiscounts[0] != 0)
	Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d",
	      thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (int i = 1; itmp < n && i <= 256; i++) {
	if (thiscounts[i] == 0)
	    continue;
	thisgrpn = thiscounts[i] - itmp;        // undo cummulate; i.e. diff
	if (thisgrpn == 1 || nextradix == -1) {
	    push(thisgrpn);
	} else {
	    iradix_r(xsub+itmp, osub+itmp, thisgrpn, nextradix);
	}
	itmp = thiscounts[i];
	thiscounts[i] = 0;
    }
}

// dradix from Arun's fastradixdouble.c
// + changed to MSD and hooked into do_radixsort framework here.
// + replaced tolerance with rounding s.f.

static int dround = 2;
static unsigned long long dmask1;
static unsigned long long dmask2;

static void setNumericRounding(int dround)
{
    dmask1 = dround ? 1 << (8 * dround - 1) : 0;
    dmask2 = 0xffffffffffffffff << dround * 8;
}

SEXP attribute_hidden do_setNumericRounding(SEXP droundArg)
{
    if (!isInteger(droundArg) || LENGTH(droundArg) != 1)
	error("Must an integer or numeric vector length 1");
    if (INTEGER(droundArg)[0] < 0 || INTEGER(droundArg)[0] > 2)
	error("Must be 2 (default) or 1 or 0");
    dround = INTEGER(droundArg)[0];
    setNumericRounding(dround);
    return R_NilValue;
}

SEXP attribute_hidden do_getNumericRounding()
{
    return ScalarInteger(dround);
}

static union {
    double d;
    unsigned long long ull;
} u;

static
unsigned long long dtwiddle(void *p, int i, int order)
{
    u.d = order * ((double *)p)[i]; // take care of 'order' at the beginning
    if (R_FINITE(u.d)) {
	u.ull = (u.d) ? u.ull + ((u.ull & dmask1) << 1) : 0;
    } else if (ISNAN(u.d)) {
	/* 1. NA twiddled to all bits 0, sorts first.  R's value 1954 cleared.

	   2. NaN twiddled to set just bit 13, sorts immediately after
	   NA. 13th bit to be consistent with "quiet" na bit but any
	   bit outside last 2 bytes would do.  (ref:
	   http://r.789695.n4.nabble.com/Question-re-NA-NaNs-in-R-td4685014.html)

	   3. This also normalises a difference between NA on 32bit R
	   (bit 13 set) and 64bit R (bit 13 not set)

	   4. -Inf twiddled to : 0 sign, exponent all 0, mantissa all
	   1, sorts after NaN

	   5. +Inf twiddled to : 1 sign, exponent all 1, mantissa all
	   0, sorts last since finite numbers are defined by not-all-1
	   in exponent
	*/
	u.ull = (ISNA(u.d) ? 0 : (1ULL << 51));
	return (nalast == 1 ? ~u.ull : u.ull);
    }
    unsigned long long mask = (u.ull & 0x8000000000000000) ?
	// always flip sign bit and if negative (sign bit was set)
	// flip other bits too
	0xffffffffffffffff : 0x8000000000000000;
    return ((u.ull ^ mask) & dmask2);
}

static Rboolean dnan(void *p, int i)
{
    u.d = ((double *) p)[i];
    return (ISNAN(u.d));
}

static unsigned long long (*twiddle) (void *, int, int);
static Rboolean(*is_nan) (void *, int);
// the size of the arg type (4 or 8). Just 8 currently until iradix is
// merged in.
static size_t colSize = 8;

static void dradix_r(unsigned char *xsub, int *osub, int n, int radix);

#ifdef WORDS_BIGENDIAN
#define RADIX_BYTE colSize - radix - 1
#else
#define RADIX_BYTE radix
#endif

static void dradix(unsigned char *x, int *o, int n)
{
    int radix, nextradix, itmp, thisgrpn, maxgrpn;
    unsigned int *thiscounts;
    unsigned long long thisx = 0;
    // see comments in iradix for structure.  This follows the same.
    // TO DO: merge iradix in here (almost ready)
    for (int i = 0; i < n; i++) {
	thisx = twiddle(x, i, order);
	for (radix = 0; radix < colSize; radix++)
	    // if dround == 2 then radix 0 and 1 will be all 0 here and skipped.
	    /* on little endian, 0 is the least significant bits (the right)
	       and 7 is the most including sign (the left); i.e. reversed. */
	    radixcounts[radix][((unsigned char *)&thisx)[RADIX_BYTE]]++;
    }
    for (radix = 0; radix < colSize; radix++) {
	// thisx is the last x after loop above
	int i = ((unsigned char *) &thisx)[RADIX_BYTE];
	skip[radix] = radixcounts[radix][i] == n;
	// clear it now, the other counts must be 0 already
	if (skip[radix])
	    radixcounts[radix][i] = 0;
    }
    radix = (int) colSize - 1;  // MSD
    while (radix >= 0 && skip[radix]) radix--;
    if (radix == -1) {
	// All radix are skipped; i.e. one number repeated n times.
	if (nalast == 0 && is_nan(x, 0))
	    // all values are identical. return 0 if nalast=0 & all NA
	    // because of 'return', have to take care of it here.
	    for (int i = 0; i < n; i++)
		o[i] = 0;
	else
	    for (int i = 0; i < n; i++)
		o[i] = (i + 1);
	push(n);
	return;
    }
    for (int i = radix - 1; i >= 0; i--) {
	// clear the lower radix counts, we only did them to know
	// skip. will be reused within each group
	if (!skip[i])
	    memset(radixcounts[i], 0, 257 * sizeof(unsigned int));
    }
    thiscounts = radixcounts[radix];
    itmp = thiscounts[0];
    maxgrpn = itmp;
    for (int i = 1; itmp < n && i < 256; i++) {
	thisgrpn = thiscounts[i];
	if (thisgrpn) {  // don't cummulate through 0s, important below
	    if (thisgrpn > maxgrpn)
		maxgrpn = thisgrpn;
	    thiscounts[i] = (itmp += thisgrpn);
	}
    }
    for (int i = n - 1; i >= 0; i--) {
	thisx = twiddle(x, i, order);
	o[ --thiscounts[((unsigned char *)&thisx)[RADIX_BYTE]] ] = i + 1;
    }

    if (radix_xsuballoc < maxgrpn) {
        // TO DO: centralize this alloc
        // The largest group according to the first non-skipped radix,
        // so could be big (if radix is needed on first arg) TO DO:
        // could include extra bits to divide the first radix up
        // more. Often the MSD has groups in just 0-4 out of 256.
        // free'd at the end of do_radixsort once we're done calling iradix
        // repetitively
        radix_xsub = (double *) realloc(radix_xsub, maxgrpn * sizeof(double));
        if (!radix_xsub)
            Error("Failed to realloc working memory %d*8bytes (xsub in dradix), radix=%d",
                  maxgrpn, radix);
        radix_xsuballoc = maxgrpn;
    }

    alloc_otmp(maxgrpn);   // TO DO: leave to do_radixsort and remove these?
    alloc_xtmp(maxgrpn);

    nextradix = radix - 1;
    while (nextradix >= 0 && skip[nextradix])
	nextradix--;
    if (thiscounts[0] != 0)
	Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. dradix=%d",
	      thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (int i = 1; itmp < n && i <= 256; i++) {
        if (thiscounts[i] == 0)
            continue;
        thisgrpn = thiscounts[i] - itmp;  // undo cummulate; i.e. diff
        if (thisgrpn == 1 || nextradix == -1) {
            push(thisgrpn);
        } else {
            if (colSize == 4) { // ready for merging in iradix ...
                error("Not yet used, still using iradix instead");
                for (int j = 0; j < thisgrpn; j++)
                    ((int *)radix_xsub)[j] = (int)twiddle(x, o[itmp+j]-1, order);
                // this is why this xsub here can't be the same memory
                // as xsub in do_radixsort
            } else 
		for (int j = 0; j < thisgrpn; j++)
		    ((unsigned long long *)radix_xsub)[j] =
			twiddle(x, o[itmp+j]-1, order);
	    // changes xsub and o by reference recursively.
	    dradix_r(radix_xsub, o+itmp, thisgrpn, nextradix);
	}
	itmp = thiscounts[i];
	thiscounts[i] = 0;
    }
    if (nalast == 0) // nalast = 1, -1 are both taken care already.
	for (int i = 0; i < n; i++)
	    o[i] = is_nan(x, o[i] - 1) ? 0 : o[i];
    // nalast = 0 is dealt with separately as it just sets o to 0
    // at those indices where x is NA. x[o[i]-1] because x is not
    // modified by reference unlike iinsert or iradix_r

}

static void dinsert(unsigned long long *x, int *o, int n)
// orders both x and o by reference in-place. Fast for small vectors,
// low overhead.  don't be tempted to binsearch backwards here, have
// to shift anyway; many memmove would have overhead and do the same
// thing 'dinsert' will not be called when nalast = 0 and o[0] = -1.
{
    int otmp, tt;
    unsigned long long xtmp;
    for (int i = 1; i < n; i++) {
	xtmp = x[i];
	if (xtmp < x[i - 1]) {
	    int j = i - 1;
	    otmp = o[i];
	    while (j >= 0 && xtmp < x[j]) {
		x[j + 1] = x[j];
		o[j + 1] = o[j];
		j--;
	    }
	    x[j + 1] = xtmp;
	    o[j + 1] = otmp;
	}
    }
    tt = 0;
    for (int i = 1; i < n; i++)
	if (x[i] == x[i - 1])
	    tt++;
	else {
	    push(tt + 1);
	    tt = 0;
	}
    push(tt + 1);
}

static void dradix_r(unsigned char *xsub, int *osub, int n, int radix)
/* xsub is a recursive offset into xsub working memory above in
   dradix, reordered by reference.  osub is a an offset into the main
   answer o, reordered by reference.  dradix iterates
   7,6,5,4,3,2,1,0 */
{
    int itmp, thisgrpn, nextradix;
    unsigned int *thiscounts;
    unsigned char *p;
    if (n < 200) {
	/* 200 is guess based on limited testing. Needs calibrate(). Was 50
	   based on sum(1:50)=1275 worst -vs- 256 cummulate + 256 memset +
	   allowance since reverse order is unlikely */
	// order=1 here because it's already taken care of in iradix
	dinsert((void *)xsub, osub, n);

	return;
    }
    thiscounts = radixcounts[radix];
    p = xsub + RADIX_BYTE;
    for (int i = 0; i < n; i++) {
	thiscounts[*p]++;
	p += colSize;
    }
    itmp = thiscounts[0];
    for (int i = 1; itmp < n && i < 256; i++)
	// don't cummulate through 0s, important below
	if (thiscounts[i])
	    thiscounts[i] = (itmp += thiscounts[i]);
    p = xsub + (n - 1) * colSize;
    if (colSize == 4) {
	error("Not yet used, still using iradix instead");
	for (int i = n - 1; i >= 0; i--) {
	    int j = --thiscounts[*(p + RADIX_BYTE)];
	    otmp[j] = osub[i];
	    ((int *) xtmp)[j] = *(int *) p;
	    p -= colSize;
	}
    } else {
	for (int i = n - 1; i >= 0; i--) {
	    int j = --thiscounts[*(p + RADIX_BYTE)];
	    otmp[j] = osub[i];
	    ((unsigned long long *) xtmp)[j] = *(unsigned long long *) p;
	    p -= colSize;
	}
    }
    memcpy(osub, otmp, n * sizeof(int));
    memcpy(xsub, xtmp, n * colSize);

    nextradix = radix - 1;
    while (nextradix >= 0 && skip[nextradix])
	nextradix--;
    // TO DO: If nextradix==-1 and no further args from do_radixsort,
    // we're done. We have o. Remember to memset thiscounts before
    // returning.

    if (thiscounts[0] != 0)
	Error("Logical error. thiscounts[0]=%d but should have been decremented to 0. radix=%d",
	      thiscounts[0], radix);
    thiscounts[256] = n;
    itmp = 0;
    for (int i = 1; itmp < n && i <= 256; i++) {
	if (thiscounts[i] == 0)
	    continue;
	thisgrpn = thiscounts[i] - itmp;        // undo cummulate; i.e. diff
	if (thisgrpn == 1 || nextradix == -1)
	    push(thisgrpn);
	else
	    dradix_r(xsub + itmp * colSize, osub + itmp, thisgrpn,
		     nextradix);
	itmp = thiscounts[i];
	thiscounts[i] = 0;
    }
}

// TO DO?: dcount. Find step size, then range = (max-min)/step and
// proceed as icount. Many fixed precision floats (such as prices) may
// be suitable. Fixed precision such as 1.10, 1.15, 1.20, 1.25, 1.30
// ... do use all bits so dradix skipping may not help.

static int *cradix_counts = NULL;
static int cradix_counts_alloc = 0;
static int maxlen = 1;
static SEXP *cradix_xtmp = NULL;
static int cradix_xtmp_alloc = 0;

// same as StrCmp but also takes into account 'decreasing' and 'na.last' args.
static int StrCmp2(SEXP x, SEXP y)
{
    // same cached pointer (including NA_STRING == NA_STRING)
    if (x == y) return 0;
    // if x=NA, nalast=1 ? then x > y else x < y (Note: nalast == 0 is
    // already taken care of in 'csorted', won't be 0 here)
    if (x == NA_STRING) return nalast;
    if (y == NA_STRING) return -nalast;     // if y=NA, nalast=1 ? then y > x
    return order*strcmp(CHAR(x), CHAR(y));  // same as explanation in StrCmp
}

static int StrCmp(SEXP x, SEXP y)            // also used by bmerge and chmatch
{
    // same cached pointer (including NA_STRING == NA_STRING)
    if (x == y) return 0;
    if (x == NA_STRING) return -1;    // x < y
    if (y == NA_STRING) return 1;     // x > y
    // assumes strings are in same encoding
    return strcmp(CHAR(x), CHAR(y));
}

#ifdef UNUSED
#define CHAR_ENCODING(x) (IS_ASCII(x) ? CE_UTF8 : getCharCE(x))

static void checkEncodings(SEXP x)
{
    cetype_t ce;

    if (length(x) == 0)
        return;

    ce = CHAR_ENCODING(STRING_ELT(x, 0));
    if (ce == CE_NATIVE) {
        error(_("Character encoding must be UTF-8, Latin-1 or bytes"));
    }

    /* Disabled for now -- doubles the time (for already sorted vectors): why?
    for (int i = 1; i < length(x); i++) {
        if (ce != CHAR_ENCODING(STRING_ELT(x, i))) {
            error(_("Mixed character encodings are not supported"));
        }
    }
    */
}

static SEXP normalizeEncodings(SEXP x)
{
    SEXP ans = x;

    if (length(ans) == 0)
        return(ans);

    ce = CHAR_ENCODING(STRING_ELT(ans, 0));
    if (ce == CE_NATIVE) {
        PROTECT(ans = duplicate(ans));
        SET_STRING_ELT(ans, 0,
                       mkCharCE(translateCharUTF8(STRING_ELT(ans, 0)),
                                CE_UTF8));
        ce = CE_UTF8;
    }
    
    for (int i = 1; i < length(ans); i++) {
        cetype_t cei = CHAR_ENCODING(STRING_ELT(ans, i));
        if (ce != cei) {
            if (ce == CE_UTF8 && cei != CE_BYTES) {
                if (ans == x)
                    PROTECT(ans = duplicate(ans));
                SET_STRING_ELT(ans, i,
                               mkCharCE(translateCharUTF8(STRING_ELT(ans, i)),
                                        CE_UTF8));
            } else {
                error(_("Unable to normalize character encodings"));
            }
        }
    }

    if (ans != x)
        UNPROTECT(1);
    return(ans);
}
#endif

static void cradix_r(SEXP * xsub, int n, int radix)
// xsub is a unique set of CHARSXP, to be ordered by reference

// First time, radix == 0, and xsub == x. Then recursively moves SEXP together
// for L1 cache efficiency.

// Quite different to iradix because
//   1) x is known to be unique so fits in cache
//      (wide random access not an issue)
//   2) they're variable length character strings
//   3) no need to maintain o.  Just simply reorder x. No grps or push.

// Fortunately, UTF sorts in the same order if treated as ASCII, so we
// can simplify by doing it by bytes.

// TO DO: confirm a forwards (MSD) radix for efficiency, although more
// complicated.

// This part has nothing to do with truelength. The
// truelength stuff is to do with finding the unique strings.  We may
// be able to improve CHARSXP derefencing by submitting patch to R to
// make R's string cache contiguous but would likely be difficult. If
// we strxfrm, then it'll then be contiguous and compact then anyway.
{
    int itmp, *thiscounts, thisgrpn=0, thisx=0;
    SEXP stmp;

    // TO DO?: chmatch to existing sorted vector, then grow it.
    // TO DO?: if (n<N_SMALL = 200) insert sort, then loop through groups via ==
    if (n <= 1) return;
    if (n == 2) {
	if (StrCmp(xsub[1], xsub[0]) < 0) {
	    stmp = xsub[0];
	    xsub[0] = xsub[1];
	    xsub[1] = stmp;
	}
	return;
    }
    // TO DO: if (n < 50) cinsert (continuing from radix offset into
    // CHAR) or using StrCmp. But 256 is narrow, so quick and not too
    // much an issue.

    thiscounts = cradix_counts + radix * 256;
    for (int i = 0; i < n; i++) {
	thisx = xsub[i] == NA_STRING ?
	    0 : (radix < LENGTH(xsub[i]) ?
		 (unsigned char) (CHAR(xsub[i])[radix]) : 1);
	thiscounts[ thisx ]++;   // 0 for NA,  1 for ""
    }
    // this also catches when subx has shorter strings than the rest,
    // thiscounts[0] == n and we'll recurse very quickly through to the
    // overall maxlen with no 256 overhead each time
    if (thiscounts[thisx] == n && radix < maxlen - 1) {
	cradix_r(xsub, n, radix + 1);
	thiscounts[thisx] = 0;  // the rest must be 0 already, save the memset
	return;
    }
    itmp = thiscounts[0];
    for (int i = 1; i < 256; i++)
	// don't cummulate through 0s, important below
	if (thiscounts[i])
	    thiscounts[i] = (itmp += thiscounts[i]);
    for (int i = n - 1; i >= 0; i--) {
	thisx = xsub[i] == NA_STRING ?
	    0 : (radix < LENGTH(xsub[i]) ?
		 (unsigned char) (CHAR(xsub[i])[radix]) : 1);
	int j = --thiscounts[thisx];
	cradix_xtmp[j] = xsub[i];
    }
    memcpy(xsub, cradix_xtmp, n * sizeof(SEXP));
    if (radix == maxlen - 1) {
	memset(thiscounts, 0, 256 * sizeof(int));
	return;
    }
    if (thiscounts[0] != 0)
	Error("Logical error. counts[0]=%d in cradix but should have been decremented to 0. radix=%d",
	      thiscounts[0], radix);
    itmp = 0;
    for (int i = 1; i < 256; i++) {
	if (thiscounts[i] == 0)
	    continue;
	thisgrpn = thiscounts[i] - itmp;        // undo cummulate; i.e. diff
	cradix_r(xsub + itmp, thisgrpn, radix + 1);
	itmp = thiscounts[i];
	// set to 0 now since we're here, saves memset
	// afterwards. Important to clear! Also more portable for
	// machines where 0 isn't all bits 0 (?!)
	thiscounts[i] = 0;
    }
    if (itmp < n - 1)
	cradix_r(xsub + itmp, n - itmp, radix + 1);     // final group
}

static SEXP *ustr = NULL;
static int ustr_alloc = 0, ustr_n = 0;

static void cgroup(SEXP * x, int *o, int n)
// As icount :
//   Places the ordering into o directly, overwriting whatever was there
//   Doesn't change x
//   Pushes group sizes onto stack

// Only run when sortStr == FALSE. Basically a counting sort, in first
// appearance order, directly.  Since it doesn't sort the strings, the
// name is cgroup.  there is no _pre for this.  ustr created and
// cleared each time.
{
    // savetl_init() is called once at the start of do_radixsort
    if (ustr_n != 0)
	Error
	    ("Internal error. ustr isn't empty when starting cgroup: ustr_n=%d, ustr_alloc=%d",
	     ustr_n, ustr_alloc);
    for (int i = 0; i < n; i++) {
	SEXP s = x[i];
	if (TRUELENGTH(s) < 0) {        // this case first as it's the most frequent
	    SET_TRUELENGTH(s, TRUELENGTH(s) - 1);
	    // use negative counts so as to detect R's own (positive)
	    // usage of tl on CHARSXP
	    continue;
	}
	if (TRUELENGTH(s) > 0) {
	    // Save any of R's own usage of tl (assumed positive, so
	    // we can both count and save in one scan), to restore
	    // afterwards. From R 2.14.0, tl is initialized to 0,
	    // prior to that it was random so this step saved too much.
	    savetl(s);
	    SET_TRUELENGTH(s, 0);
	}
	if (ustr_alloc <= ustr_n) {
	    // 10000 = 78k of 8byte pointers. Small initial guess,
	    // negligible time to alloc.
	    ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;
	    if (ustr_alloc > n)
		ustr_alloc = n;
	    ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
	    if (ustr == NULL)
		Error("Unable to realloc %d * %d bytes in cgroup", ustr_alloc,
		      sizeof(SEXP));
	}
	SET_TRUELENGTH(s, -1);
	ustr[ustr_n++] = s;
    }
    // TO DO: the same string in different encodings will be
    // considered different here. Sweep through ustr and merge counts
    // where equal (sort needed therefore, unfortunately?, only if
    // there are any marked encodings present)
    int cumsum = 0;
    for (int i = 0; i < ustr_n; i++) {      // 0.000
	push(-TRUELENGTH(ustr[i]));
	SET_TRUELENGTH(ustr[i], cumsum += -TRUELENGTH(ustr[i]));
    }
    int *target = (o[0] != -1) ? newo : o;
    for (int i = n - 1; i >= 0; i--) {
	SEXP s = x[i];           // 0.400 (page fetches on string cache)
	int k = TRUELENGTH(s) - 1;
	SET_TRUELENGTH(s, k);
	target[k] = i + 1;      // 0.800 (random access to o)
    }
    // The cummulate meant counts are left non zero, so reset for next
    // time (0.00s).
    for (int i = 0; i < ustr_n; i++)
	SET_TRUELENGTH(ustr[i], 0);
    ustr_n = 0;
}

static int *csort_otmp = NULL, csort_otmp_alloc = 0;
static void alloc_csort_otmp(int n)
{
    if (csort_otmp_alloc >= n)
	return;
    csort_otmp = (int *) realloc(csort_otmp, n * sizeof(int));
    if (csort_otmp == NULL)
	Error
	    ("Failed to allocate working memory for csort_otmp. Requested %d * %d bytes",
	     n, sizeof(int));
    csort_otmp_alloc = n;
}

static void csort(SEXP * x, int *o, int n)
/*
   As icount :
   Places the ordering into o directly, overwriting whatever was there
   Doesn't change x
   Pushes group sizes onto stack
   Requires csort_pre() to have created and sorted ustr already
*/
{
    /* can't use otmp, since iradix might be called here and that uses
       otmp (and xtmp).  alloc_csort_otmp(n) is called from do_radixsort for
       either n=nrow if 1st arg, or n=maxgrpn if onwards args */
    for (int i = 0; i < n; i++)
	csort_otmp[i] = (x[i] == NA_STRING) ? NA_INTEGER : -TRUELENGTH(x[i]);
    if (nalast == 0 && n == 2) {
        // special case for nalast == 0. n == 1 is handled inside
        // do_radixsort. at least 1 will be NA here else use o from caller
        // directly (not 1st arg)
        if (o[0] == -1)
            for (int i = 0; i < n; i++)
                o[i] = i + 1;
        for (int i = 0;  i < n; i++)
            if (csort_otmp[i] == NA_INTEGER)
                o[i] = 0;
        push(1); push(1);
        return; 
    }
    if (n < N_SMALL && nalast != 0) { // TO DO: calibrate() N_SMALL=200
        if (o[0] == -1)
            for (int i = 0; i < n; i++)
                o[i] = i + 1;
        // else use o from caller directly (not 1st arg)
        for (int i = 0; i < n; i++)
            csort_otmp[i] = icheck(csort_otmp[i]);
        iinsert(csort_otmp, o, n);
    } else {
	setRange(csort_otmp, n);
	if (range == NA_INTEGER)
	    Error("Internal error. csort's otmp contains all-NA");
	int *target = (o[0] != -1) ? newo : o;
	if (range <= N_RANGE)
	    // TO DO: calibrate(). radix was faster (9.2s
	    // "range<=10000" instead of 11.6s "range<=N_RANGE &&
	    // range<n") for run(7) where range=N_RANGE n=10000000
	    icount(csort_otmp, target, n);
	else
	    iradix(csort_otmp, target, n);
    }
    // all i* push onto stack. Using their counts may be faster here
    // than thrashing SEXP fetches over several passes as cgroup does
    // (but cgroup needs that to keep orginal order, and cgroup saves
    // the sort in csort_pre).
}

static void csort_pre(SEXP * x, int n)
// Finds ustr and sorts it.  Runs once for each arg (if
// sortStr == TRUE), then ustr is used by csort within each group ustr
// is grown on each character arg, to save sorting the same strings
// again if several args contain the same strings
{
    SEXP s;
    int old_un, new_un;
    // savetl_init() is called once at the start of do_radixsort
    old_un = ustr_n;
    for (int i = 0; i < n; i++) {
	s = x[i];
	// this case first as it's the most frequent. Already in ustr,
	// this negative is its ordering.
	if (TRUELENGTH(s) < 0)
	    continue;
	// Save any of R's own usage of tl (assumed positive, so we
	// can both count and save in one scan), to restore
	// afterwards. From R 2.14.0, tl is initialized to 0, prior to
	// that it was random so this step saved too much.
	if (TRUELENGTH(s) > 0) {
	    savetl(s);
	    SET_TRUELENGTH(s, 0);
	}
	if (ustr_alloc <= ustr_n) {
	    // 10000 = 78k of 8byte pointers. Small initial guess,
	    // negligible time to alloc.
	    ustr_alloc = (ustr_alloc == 0) ? 10000 : ustr_alloc*2;
	    if (ustr_alloc > old_un+n)
		ustr_alloc = old_un + n;
	    ustr = realloc(ustr, ustr_alloc * sizeof(SEXP));
	    if (ustr == NULL)
		Error("Failed to realloc ustr. Requested %d * %d bytes",
		      ustr_alloc, sizeof(SEXP));
	}
	SET_TRUELENGTH(s, -1);  // this -1 will become its ordering later below
	ustr[ustr_n++] = s;
	// length on CHARSXP is the nchar of char * (excluding \0),
	// and treats marked encodings as if ascii.
	if (s != NA_STRING && LENGTH(s) > maxlen)
	    maxlen = LENGTH(s);
    }
    new_un = ustr_n;
    if (new_un == old_un)
	return;
    // No new strings observed, seen them all before in previous
    // arg. ustr already sufficient.  If we ever make ustr
    // permanently held by data.table, we'll just need to make the
    // final loop to set -i-1 before returning here.  sort ustr.

    // TODO: just sort new ones and merge them in.  These allocs are
    // here, to save them being in the recursive cradix_r()
    if (cradix_counts_alloc < maxlen) {
	cradix_counts_alloc = maxlen + 10;   // +10 to save too many reallocs
	cradix_counts = (int *)realloc(cradix_counts,
				       cradix_counts_alloc * 256 * sizeof(int));
	if (!cradix_counts)
	    Error("Failed to alloc cradix_counts");
	memset(cradix_counts, 0, cradix_counts_alloc * 256 * sizeof(int));
    }
    if (cradix_xtmp_alloc < ustr_n) {
        cradix_xtmp = (SEXP *) realloc(cradix_xtmp,  ustr_n * sizeof(SEXP));
        // TO DO: Reuse the one we have in do_radixsort.
        // Does it need to be n length?
        if (!cradix_xtmp)
            Error("Failed to alloc cradix_tmp");
        cradix_xtmp_alloc = ustr_n;
    }
    // sorts ustr in-place by reference save ordering in the
    // CHARSXP. negative so as to distinguish with R's own usage.
    cradix_r(ustr, ustr_n, 0);
    for (int i = 0; i < ustr_n; i++)
	SET_TRUELENGTH(ustr[i], -i - 1);
}

// functions to test vectors for sortedness: isorted, dsorted and csorted

// base:is.unsorted returns NA in the presence of any NA, but we need
// to consider na.last, and we also return -1 if x is sorted in
// _strictly_ reverse order; a common case we optimize.  If a vector
// is in decreasing order *with ties*, then an in-place reverse (no
// sort) would result in instability of ties, so we are strict. We
// also save grouping information during the check; that information
// is required when sorting by multiple arguments.

// TO DO: test in big steps first to return faster if unsortedness is
// at the end (a common case of rbind'ing data to end) These are all
// sequential access to x, so very quick and cache efficient.

// order = 1 is ascending and order=-1 is descending; also takes care
// of na.last argument with check through 'icheck' Relies on
// NA_INTEGER == INT_MIN, checked in init.c
static int isorted(int *x, int n)
{
    int i = 1, j = 0;
    // when nalast = NA,
    // all NAs ? return special value to replace all o's values with '0'
    // any NAs ? return 0 = unsorted and leave it
    //   to sort routines to replace o's with 0's
    // no NAs ? continue to check rest of isorted - the same routine as usual
    if (nalast == 0) {
	for (int k = 0; k < n; k++)
	    if (x[k] != NA_INTEGER)
		j++;
	if (j == 0) {
	    push(n);
	    return (-2);
	}
	if (j != n)
	    return (0);
    }
    if (n <= 1) {
	push(n);
	return (1);
    }
    if (icheck(x[1]) < icheck(x[0])) {
	i = 2;
	while (i < n && icheck(x[i]) < icheck(x[i - 1]))
	    i++;
	// strictly opposite to expected 'order', no ties;
	if (i == n) {
	    mpush(1, n);
	    return (-1);
	}
	// e.g. no more than one NA at the beginning/end (for order=-1/1)
	else return (0);
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (int i = 1; i < n; i++) {
	if (icheck(x[i]) < icheck(x[i - 1])) {
	    gsngrp[flip] = old;
	    return (0);
	}
	if (x[i] == x[i - 1])
	    tt++;
	else {
	    push(tt); tt = 1;
	}
    }
    push(tt);
    // same as 'order', NAs at the beginning for order=1, at end for
    // order=-1, possibly with ties
    return(1);
}

// order=1 is ascending and -1 is descending
// also accounts for nalast=0 (=NA), =1 (TRUE), -1 (FALSE) (in twiddle)
static int dsorted(double *x, int n)
{
    int i = 1, j = 0;
    unsigned long long prev, this;
    if (nalast == 0) {
	// when nalast = NA,
	// all NAs ? return special value to replace all o's values with '0'
	// any NAs ? return 0 = unsorted and leave it to sort routines to
	//           replace o's with 0's
	// no NAs  ? continue to check the rest of isorted -
	//           the same routine as usual
	for (int k = 0; k < n; k++)
	    if (!is_nan(x, k))
		j++;
	if (j == 0) {
	    push(n);
	    return (-2);
	}
	if (j != n)
	    return (0);
    }
    if (n <= 1) {
	push(n);
	return (1);
    }
    prev = twiddle(x, 0, order);
    this = twiddle(x, 1, order);
    if (this < prev) {
	i = 2;
	prev = this;
	while (i < n && (this = twiddle(x, i, order)) < prev) {
	    i++;
	    prev = this;
	}
	if (i == n) {
	    mpush(1, n);
	    return (-1);
	}
	// strictly opposite of expected 'order', no ties; e.g. no
	// more than one NA at the beginning/end (for order=-1/1)

	// TO DO: improve to be stable for ties in reverse
	else return(0);
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (int i = 1; i < n; i++) {
	// TO DO: once we get past -Inf, NA and NaN at the bottom, and
	//        +Inf at the top, the middle only need be twiddled
	//        for tolerance (worth it?)
	this = twiddle(x, i, order);
	if (this < prev) {
	    gsngrp[flip] = old;
	    return (0);
	}
	if (this == prev)
	    tt++;
	else {
	    push(tt);
	    tt = 1;
	}
	prev = this;
    }
    push(tt);
    // exactly as expected in 'order' (1=increasing, -1=decreasing),
    // possibly with ties
    return (1);
}

// order=1 is ascending and -1 is descending
// also accounts for nalast=0 (=NA), =1 (TRUE), -1 (FALSE)
static int csorted(SEXP *x, int n)
{
    int i = 1, j = 0, tmp;
    if (nalast == 0) {
	// when nalast = NA,
	// all NAs ? return special value to replace all o's values with '0'
	// any NAs ? return 0 = unsorted and leave it to sort routines
	//           to replace o's with 0's
	// no NAs  ? continue to check the rest of isorted -
	//           the same routine as usual
	for (int k = 0; k < n; k++)
	    if (x[k] != NA_STRING)
		j++;
	if (j == 0) {
	    push(n);
	    return (-2);
	}
	if (j != n)
	    return (0);
    }
    if (n <= 1) {
	push(n);
	return (1);
    }
    if (StrCmp2(x[1], x[0]) < 0) {
	i = 2;
	while (i < n && StrCmp2(x[i], x[i - 1]) < 0)
	    i++;
	if (i == n) {
	    mpush(1, n);
	    return (-1);
	}
	// strictly opposite of expected 'order', no ties;
	// e.g. no more than one NA at the beginning/end (for order=-1/1)
	else
	    return (0);
    }
    int old = gsngrp[flip];
    int tt = 1;
    for (int i = 1; i < n; i++) {
	tmp = StrCmp2(x[i], x[i - 1]);
	if (tmp < 0) {
	    gsngrp[flip] = old;
	    return (0);
	}
	if (tmp == 0)
	    tt++;
	else {
	    push(tt);
	    tt = 1;
	}
    }
    push(tt);
    // exactly as expected in 'order', possibly with ties
    return (1);
}

static void isort(int *x, int *o, int n)
{
    if (n <= 2) {
	// nalast = 0 and n == 2 (check bottom of this file for explanation)
	if (nalast == 0 && n == 2) {
	    for (int i = 0; i < n; i++)
		if (x[i] == NA_INTEGER)
		    o[i] = 0;
	    push(1); push(1);
	    return;
	} else Error("Internal error: isort received n=%d. isorted should have dealt with this (e.g. as a reverse sorted vector) already",n);
    }
    if (n < N_SMALL && o[0] != -1 && nalast != 0) {
        // see comment above in iradix_r on N_SMALL=200.
        /* if not o[0] then can't just populate with 1:n here, since x
           is changed by ref too (so would need to be copied). */
        /* pushes inside too. Changes x and o by reference, so not
           suitable in first arg when o hasn't been populated yet
           and x is an actual argument (hence check on o[0]). */
        if (order != 1 || nalast != -1)
            // so that default case, i.e., order=1, nalast=FALSE will
            // not be affected (ex: `setkey`)
            for (int i = 0; i < n; i++)
                x[i] = icheck(x[i]);
        iinsert(x, o, n);
    } else {
        /* Tighter range (e.g. copes better with a few abormally large
           values in some groups), but also, when setRange was once at
           arg level that caused an extra scan of (long) x
           first. 10,000 calls to setRange takes just 0.04s
           i.e. negligible. */
        setRange(x, n);
        if (range == NA_INTEGER)
            Error("Internal error: isort passed all-NA. isorted should have caught this before this point");
        int *target = (o[0] != -1) ? newo : o;
        // was range < 10000 for subgroups, but 1e5 for the first
        // arg, tried to generalise here.  1e4 rather than 1e5 here
        // because iterated was (thisgrpn < 200 || range > 20000) then
        // radix a short vector with large range can bite icount when
        // iterated (BLOCK 4 and 6)
        if (range <= N_RANGE && range <= n) {
            icount(x, target, n);
        } else {
            iradix(x, target, n);
        }
    }
}

static void dsort(double *x, int *o, int n)
{
    if (n <= 2) {
	if (nalast == 0 && n == 2) {
	    // don't have to twiddle here.. at least one will be NA
	    // and 'n' WILL BE 2.
	    for (int i = 0; i < n; i++)
		if (is_nan(x, i))
		    o[i] = 0;
	    push(1); push(1);
	    return;
	}
	Error("Internal error: dsort received n=%d. dsorted should have dealt with this (e.g. as a reverse sorted vector) already",n);
    }
    if (n < N_SMALL && o[0] != -1 && nalast != 0) {
	// see comment above in iradix_r re N_SMALL=200,  and isort for o[0]
	for (int i = 0; i < n; i++)
	    ((unsigned long long *)x)[i] = twiddle(x, i, order);
	// have to twiddle here anyways, can't speed up default case
	// like in isort
	dinsert((unsigned long long *)x, o, n);
    } else {
	dradix((unsigned char *) x, (o[0] != -1) ? newo : o, n);
    }
}

SEXP attribute_hidden do_radixsort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n = -1, narg = 0, ngrp, tmp, *osub, thisgrpn;
    R_xlen_t nl = n;
    Rboolean isSorted = TRUE, retGrp;
    void *xd;
    int *o = NULL;

    /* ML: FIXME: Here are just two of the dangerous assumptions here */
    if (sizeof(int) != 4) {
        error("radix sort assumes sizeof(int) == 4");
    }
    if (sizeof(double) != 8) {
        error("radix sort assumes sizeof(double) == 8");
    }

    /* (ML) Controls the precision of numeric vector sorting; may want
       to make this a parameter */
    setNumericRounding(dround);

    nalast = (asLogical(CAR(args)) == NA_LOGICAL) ? 0 :
	(asLogical(CAR(args)) == TRUE) ? 1 : -1; // 1=TRUE, -1=FALSE, 0=NA
    args = CDR(args);
    SEXP decreasing = CAR(args);
    args = CDR(args);

    /* If TRUE, return starts of runs of identical values + max group size. */
    retGrp = asLogical(CAR(args));
    args = CDR(args);

    /* If FALSE, get order of strings in appearance order. Essentially
       abuses the CHARSXP table to group strings without hashing
       them. Only makes sense when retGrp=TRUE.
    */
    sortStr = asLogical(CAR(args));
    args = CDR(args);

    if (args == R_NilValue)
	return R_NilValue;
    if (isVector(CAR(args)))
	nl = XLENGTH(CAR(args));
    for (SEXP ap = args; ap != R_NilValue; ap = CDR(ap), narg++) {
	if (!isVector(CAR(ap)))
	    error(_("argument %d is not a vector"), narg + 1);
        //Rprintf("%d, %d\n", XLENGTH(CAR(ap)), nl);
	if (XLENGTH(CAR(ap)) != nl)
	    error(_("argument lengths differ"));
    }

    if (narg != length(decreasing))
	error(_("length(decreasing) must match the number of order arguments"));
    for (int i = 0; i < narg; i++) {
	if (LOGICAL(decreasing)[i] == NA_LOGICAL)
	    error(_("'decreasing' elements must be TRUE or FALSE"));
    }
    order = asLogical(decreasing) ? -1 : 1;

    SEXP x = CAR(args);
    args = CDR(args);

    // (ML) FIXME: need to support long vectors
    if (nl > INT_MAX) {
	error(_("long vectors not supported"));
    }
    n = (int) nl;

    // upper limit for stack size (all size 1 groups). We'll detect
    // and avoid that limit, but if just one non-1 group (say 2), that
    // can't be avoided.
    gsmaxalloc = n;

    // once for the result, needs to be length n.

    // TO DO: save allocation if NULL is returned (isSorted = =TRUE) so
    // [i|c|d]sort know they can populate o directly with no working
    // memory needed to reorder existing order had to repace this from
    // '0' to '-1' because 'nalast = 0' replace 'o[.]' with 0 values.

    SEXP ans = PROTECT(allocVector(INTSXP, n));
    o = INTEGER(ans);
    o[0] = -1;
    xd = DATAPTR(x);

    stackgrps = narg > 1 || retGrp;

    /* if (TYPEOF(x) == STRSXP) { */
    /*     checkEncodings(x); */
    /* } */
    
    savetl_init();   // from now on use Error not error.

    switch (TYPEOF(x)) {
    case INTSXP:
    case LGLSXP:
	tmp = isorted(xd, n);
	break;
    case REALSXP :
	twiddle = &dtwiddle;
	is_nan  = &dnan;
	tmp = dsorted(xd, n);
	break;
    case STRSXP :
	tmp = csorted(xd, n);
	break;
    default :
        Error("First arg is type '%s', not yet supported",
              type2char(TYPEOF(x)));
    }
    if (tmp) {
	// -1 or 1. NEW: or -2 in case of nalast == 0 and all NAs
	if (tmp == 1) {
	    // same as expected in 'order' (1 = increasing, -1 = decreasing)
	    isSorted = TRUE;
	    for (int i = 0; i < n; i++)
		o[i] = i + 1;
	    // TO DO: we don't need this if returning NULL? Save it?
	    // Unlikely huge gain, though.
	} else if (tmp == -1) {
	    // -1 (or -n for result of strcmp), strictly opposite to
	    // -expected 'order'
	    isSorted = FALSE;
	    for (int i = 0; i < n; i++)
		o[i] = n - i;
	} else if (nalast == 0 && tmp == -2) {
	    // happens only when nalast=NA/0. Means all NAs, replace
	    // with 0's therefore!
	    isSorted = FALSE;
	    for (int i = 0; i < n; i++)
		o[i] = 0;
	}
    } else {
	isSorted = FALSE;
	switch (TYPEOF(x)) {
	case INTSXP:
	case LGLSXP:
	    isort(xd, o, n);
	    break;
	case REALSXP :
	    dsort(xd, o, n);
	    break;
	case STRSXP :
	    if (sortStr) {
		csort_pre(xd, n);
		alloc_csort_otmp(n);
		csort(xd, o, n);
	    } else
		cgroup(xd, o, n);
	    break;
	default:
	    Error
		("Internal error: previous default should have caught unsupported type");
	}
    }
    
    int maxgrpn = gsmax[flip];   // biggest group in the first arg
    void *xsub = NULL;           // local
    int (*f) ();
    void (*g) ();
    
    if (narg > 1 && gsngrp[flip] < n) {
        // double is the largest type, 8
        xsub = (void *) malloc(maxgrpn * sizeof(double));
        if (xsub == NULL)
            Error("Couldn't allocate xsub in do_radixsort, requested %d * %d bytes.",
                  maxgrpn, sizeof(double));
        // global variable, used by isort, dsort, sort and cgroup
        newo = (int *) malloc(maxgrpn * sizeof(int));
        if (newo == NULL)
            Error("Couldn't allocate newo in do_radixsort, requested %d * %d bytes.",
                  maxgrpn, sizeof(int));
    }

    for (int col = 2; col <= narg; col++) {
	x = CAR(args);
	args = CDR(args);
	xd = DATAPTR(x);
	ngrp = gsngrp[flip];
	if (ngrp == n && nalast != 0)
	    break;
	flipflop();
	stackgrps = col != narg || retGrp;
	order = LOGICAL(decreasing)[col - 1] ? -1 : 1;
	switch (TYPEOF(x)) {
	case INTSXP:
	case LGLSXP:
	    f = &isorted;
	    g = &isort;
	    break;
	case REALSXP:
	    twiddle = &dtwiddle;
	    is_nan = &dnan;
	    f = &dsorted;
	    g = &dsort;
	    break;
	case STRSXP:
	    f = &csorted;
	    if (sortStr) {
		csort_pre(xd, n);
		alloc_csort_otmp(gsmax[1 - flip]);
		g = &csort;
	    }
	    // no increasing/decreasing order required if sortStr = FALSE,
	    // just a dummy argument
	    else
		g = &cgroup;
	    break;
	default:
	    Error("Arg %d is type '%s', not yet supported",
		  col, type2char(TYPEOF(x)));
	}
	int i = 0;
	for (int grp = 0; grp < ngrp; grp++) {
	    thisgrpn = gs[1 - flip][grp];
	    if (thisgrpn == 1) {
		if (nalast == 0) {
		    // this edge case had to be taken care of
		    // here.. (see the bottom of this file for
		    // more explanation)
		    switch (TYPEOF(x)) {
		    case INTSXP:
			if (INTEGER(x)[o[i] - 1] == NA_INTEGER) {
			    isSorted = FALSE;
			    o[i] = 0;
			}
			break;
		    case LGLSXP:
			if (LOGICAL(x)[o[i] - 1] == NA_LOGICAL) {
			    isSorted = FALSE;
			    o[i] = 0;
			}
			break;
		    case REALSXP:
			if (ISNAN(REAL(x)[o[i] - 1])) {
			    isSorted = FALSE;
			    o[i] = 0;
			}
			break;
		    case STRSXP:
			if (STRING_ELT(x, o[i] - 1) == NA_STRING) {
			    isSorted = FALSE;
			    o[i] = 0;
                        } break;
                    default :
                        Error("Internal error: previous default should have caught unsupported type");
                    }
                }
                i++;
                push(1);
                continue;
            }
            osub = o+i;
            // ** TO DO **: if isSorted, we can just point xsub
            //        into x directly. If (*f)() returns 0,
            //        though, will have to copy x at that point
            //        When doing this, xsub could be allocated at
            //        that point for the first time.
            if (TYPEOF(x) == STRSXP)
                for (int j = 0; j < thisgrpn; j++)
                    ((SEXP *) xsub)[j] = ((SEXP *) xd)[o[i++] - 1];
            else if (TYPEOF(x) == REALSXP)
                for (int j = 0; j < thisgrpn; j++)
                    ((double *) xsub)[j] = ((double *) xd)[o[i++] - 1];
            else
                for (int j = 0; j < thisgrpn; j++)
                    ((int *) xsub)[j] = ((int *) xd)[o[i++] - 1];
                
            // continue; // BASELINE short circuit timing
            // point. Up to here is the cost of creating xsub.
            // [i|d|c]sorted(); very low cost, sequential
            tmp = (*f)(xsub, thisgrpn);
            if (tmp) {
                // *sorted will have already push()'d the groups
                if (tmp == -1) {
		    isSorted = FALSE;
		    for (int k = 0; k < thisgrpn / 2; k++) {
			// reverse the order in-place using no
			// function call or working memory
			// isorted only returns -1 for
			// _strictly_ decreasing order,
			// otherwise ties wouldn't be stable
			tmp = osub[k];
			osub[k] = osub[thisgrpn - 1 - k];
			osub[thisgrpn - 1 - k] = tmp;
		    }
		} else if (nalast == 0 && tmp == -2) {
		    // all NAs, replace osub[.] with 0s.
		    isSorted = FALSE;
		    for (int k = 0; k < thisgrpn; k++) osub[k] = 0;
		}
		continue;
	    }
	    isSorted = FALSE;
	    // nalast=NA will result in newo[0] = 0. So had to change to -1.
	    newo[0] = -1;
	    // may update osub directly, or if not will put the
	    // result in global newo
	    (*g)(xsub, osub, thisgrpn);

	    if (newo[0] != -1) {
		if (nalast != 0)
		    for (int j = 0; j < thisgrpn; j++)
			// reuse xsub to reorder osub
			((int *) xsub)[j] = osub[newo[j] - 1];
		else
		    for (int j = 0; j < thisgrpn; j++)
			// final nalast case to handle!
			((int *) xsub)[j] = (newo[j] == 0) ? 0 :
			    osub[newo[j] - 1];
		memcpy(osub, xsub, thisgrpn * sizeof(int));
	    }
	}
    }

    if (!sortStr && ustr_n != 0)
        Error("Internal error: at the end of do_radixsort sortStr == FALSE but ustr_n !=0 [%d]",
              ustr_n);
    for(int i = 0; i < ustr_n; i++)
        SET_TRUELENGTH(ustr[i], 0);
    maxlen = 1;  // reset global. Minimum needed to count "" and NA
    ustr_n = 0;
    savetl_end();
    free(ustr);
    ustr = NULL;
    ustr_alloc = 0;

    if (retGrp) {
        ngrp = gsngrp[flip];
        setAttrib(ans, install("ends"), x = allocVector(INTSXP, ngrp));
        INTEGER(x)[0] = gs[flip][0];
        for (int i = 1; i < ngrp; i++)
            INTEGER(x)[i] = INTEGER(x)[i - 1] + gs[flip][i];
        setAttrib(ans, install("maxgrpn"), ScalarInteger(gsmax[flip]));
        setAttrib(ans, R_ClassSymbol, mkString("grouping"));
    }

    Rboolean dropZeros = !retGrp && !isSorted && nalast == 0;
    if (dropZeros) {
        int zeros = 0;
        for (int i = 0; i < n; i++) {
            if (o[i] == 0)
                zeros++;
        }
        if (zeros > 0) {
            PROTECT(ans = allocVector(INTSXP, n - zeros));
            int *o2 = INTEGER(ans);
            for (int i = 0, i2 = 0; i < n; i++) {
                if (o[i] > 0)
                    o2[i2++] = o[i];
            }
            UNPROTECT(1);
        }
    }
    
    gsfree();
    free(radix_xsub);          radix_xsub=NULL;    radix_xsuballoc=0;
    free(xsub); free(newo);    xsub=newo=NULL;
    free(xtmp);                xtmp=NULL;          xtmp_alloc=0;
    free(otmp);                otmp=NULL;          otmp_alloc=0;
    free(csort_otmp);          csort_otmp=NULL;    csort_otmp_alloc=0;

    free(cradix_counts);       cradix_counts=NULL; cradix_counts_alloc=0;
    free(cradix_xtmp);         cradix_xtmp=NULL;   cradix_xtmp_alloc=0;
    // TO DO: use xtmp already got

    UNPROTECT(1);
    return ans;
}
