/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2012   The R Core Team.
 *
 *  Based on CACM algorithm #347 by R. C. Singleton (1969)
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

/*====== BODY of R_qsort() and R_qsorti() functions ====================
 *
 * is included in ./qsort.c  with and without ``qsort_Index'' defined
 *======================================================================
*/
{
/* Orders v[] increasingly. Puts into I[] the permutation vector:
 *  new v[k] = old v[I[k]]
 * Only elements [i : j]  (in 1-indexing !)  are considered.

 * This is a modification of CACM algorithm #347 by R. C. Singleton,
 * which is a modified Hoare quicksort.
 * This version incorporates the modification in the remark by Peto.
*/

#ifndef INTt
# define INTt size_t
#endif

    INTt il[40], iu[40]; /* was 31 */
    /* Arrays iu[k] and il[k] permit sorting up to 2^(k+1)-1 elements;
     * originally k = 20 -> n_max =    2'097'151
     * now        k = 31 -> n_max = 4294'967'295
     */
    NUMERIC vt, vtt;
    double R = 0.375;
    INTt ii, ij, k, l, m;
#ifdef qsort_Index
    INDt it, tt;
#endif


    /* 1-indexing for I[], v[]  (and `i' and `j') : */
    --v;
#ifdef qsort_Index
    --I;
#endif

    ii = i;/* save */
    m = 1;

  L10:
    if (i < j) {
	if (R < 0.5898437) R += 0.0390625; else R -= 0.21875;
      L20:
	k = i;
	/* ij = (j + i) >> 1; midpoint */
	ij = (INTt)(i + (INTt)((j - i)*R));
#ifdef qsort_Index
	it = I[ij];
#endif
	vt = v[ij];
	if (v[i] > vt) {
#ifdef qsort_Index
	    I[ij] = I[i]; I[i] = it; it = I[ij];
#endif
	    v[ij] = v[i]; v[i] = vt; vt = v[ij];
	}
	/* L30:*/
	l = j;
	if (v[j] < vt) {
#ifdef qsort_Index
	    I[ij] = I[j]; I[j] = it; it = I[ij];
#endif
	    v[ij] = v[j]; v[j] = vt; vt = v[ij];
	    if (v[i] > vt) {
#ifdef qsort_Index
		I[ij] = I[i]; I[i] = it; it = I[ij];
#endif
		v[ij] = v[i]; v[i] = vt; vt = v[ij];
	    }
	}

	for(;;) { /*L50:*/
	    do l--;  while (v[l] > vt);

#ifdef qsort_Index
	     tt = I[l];
#endif
	    vtt = v[l];
	    /*L60:*/ do k++;  while (v[k] < vt);

	    if (k > l) break;

	    /* else (k <= l) : */
#ifdef qsort_Index
	    I[l] = I[k]; I[k] =  tt;
#endif
	    v[l] = v[k]; v[k] = vtt;
	}

	m++;
	if (l - i <= j - k) {
	    /*L70: */
	    il[m] = k;
	    iu[m] = j;
	    j = l;
	}
	else {
	    il[m] = i;
	    iu[m] = l;
	    i = k;
	}
    }
    else { /* i >= j : */

    L80:
	if (m == 1)	return;

	/* else */
	i = il[m];
	j = iu[m];
	m--;
    }

    if (j - i > 10)	goto L20;

    if (i == ii)	goto L10;

    --i;
  L100:
    do {
	++i;
	if (i == j) {
	    goto L80;
	}
#ifdef qsort_Index
	it = I[i + 1];
#endif
	vt = v[i + 1];
    } while (v[i] <= vt);

    k = i;

    do { /*L110:*/
#ifdef qsort_Index
	I[k + 1] = I[k];
#endif
	v[k + 1] = v[k];
	--k;
    } while (vt < v[k]);

#ifdef qsort_Index
    I[k + 1] = it;
#endif
    v[k + 1] = vt;
    goto L100;
} /* R_qsort{i} */
