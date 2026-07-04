/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2026	The R Core Team
 *  Copyright (C) 2003		The R Foundation
 *  Copyright (C) 2010		David Simcha
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

#include <limits.h>
#include <stdint.h>
#include <string.h>

#include <R.h>
#include <Rmath.h>

/* Kendall's tau computation uses the O(n log n) merge-sort algorithm
 * based on Knight, W.R. (1966): "A Computer Method for Calculating Kendall's
 * Tau with Ungrouped Data", JASA 61(314):436-439.
 * Implementation adapted from David Simcha's contribution (2010).
 */

 /* Sorts in place, returns the bubble sort distance between the input array
 * and the sorted array.
 */
uint64_t insertionSort(double* arr, size_t len) {
    size_t maxJ, i;
    uint64_t swapCount = 0;

    if(len < 2) {
	return 0;
    }

    maxJ = len - 1;
    for(i = len - 2; i < len; --i) {
	size_t j = i;
	double val = arr[i];

	for(; j < maxJ && arr[j + 1] < val; ++j) {
	    arr[j] = arr[j + 1];
	}

	arr[j] = val;
	swapCount += (j - i);
    }

    return swapCount;
}

/* Is it okay to use unsigned int64 in this code? */
static uint64_t merge(double* from, double* to, size_t middle, size_t len) {
    size_t bufIndex, leftLen, rightLen;
    uint64_t swaps;
    double* left;
    double* right;

    bufIndex = 0;
    swaps = 0;

    left = from;
    right = from + middle;
    rightLen = len - middle;
    leftLen = middle;

    while(leftLen && rightLen) {
	if(right[0] < left[0]) {
	    to[bufIndex] = right[0];
	    swaps += leftLen;
	    rightLen--;
	    right++;
	} else {
	    to[bufIndex] = left[0];
	    leftLen--;
	    left++;
	}
	bufIndex++;
    }

    if(leftLen) {
	memcpy(to + bufIndex, left, leftLen * sizeof(double));
    } else if(rightLen) {
	memcpy(to + bufIndex, right, rightLen * sizeof(double));
    }

    return swaps;
}

/* Sorts in place, returns the bubble sort distance between the input array
 * and the sorted array.
 */
uint64_t mergeSort(double* x, double* buf, size_t len) {

    if(len < 10)
	return insertionSort(x, len);

    if(len < 2) {
	return 0;
    }

    size_t half = len / 2;
    uint64_t
	swaps = mergeSort(x, buf, half);
    swaps += mergeSort(x + half, buf + half, len - half);
    swaps += merge(x, buf, half, len);

    memcpy(x, buf, len * sizeof(double));
    return swaps;
}

static uint64_t getMs(double* data, size_t len) {  /* Assumes data is sorted.*/

    uint64_t Ms = 0, tieCount = 0;
    for (size_t i = 1; i < len; i++) {
	if (data[i] == data[i-1]) {
	    tieCount++;
	} else if (tieCount) {
	    Ms += (tieCount * (tieCount + 1)) / 2;
	    tieCount++;
	    tieCount = 0;
	}
    }
    if (tieCount) {
	Ms += (tieCount * (tieCount + 1)) / 2;
	tieCount++;
    }
    return Ms;
}

/* This function calculates the Kendall covariance (if cor == 0) or
 * correlation (if cor != 0), but assumes arr1 has already been sorted and
 * arr2 has already been reordered in lockstep.  This can be done within R
 * before calling this function by doing something like:
 *
 * perm <- order(arr1)
 * arr1 <- arr1[perm]
 * arr2 <- arr2[perm]
 */
 double kendallNlogN(double* arr1, double* arr2, size_t len, int cor) {
     uint64_t m1 = 0, m2 = 0, tieCount, swapCount, nPair;
     int64_t s;
     size_t i;

     nPair = (uint64_t) len * ((uint64_t) len - 1) / 2;
     s = nPair;

     tieCount = 0;
     // 'INT_MAX' below ==> *NOT* called for 'large vectors' ==> (int)(tieCount + 1) "ok"
     for (i = 1; i < len; i++) {
	 if (arr1[i - 1] == arr1[i]) {
	     tieCount++;
	 } else if (tieCount > 0) {
#define SORT_getMS							\
	     R_rsort(arr2 + i - tieCount - 1, (int)(tieCount + 1));	\
	     m1 += tieCount * (tieCount + 1) / 2;			\
	     s += getMs(arr2 + i - tieCount - 1, tieCount + 1)

	     SORT_getMS;
	     tieCount = 0;
	 }
     }
     if (tieCount > 0) {
	 SORT_getMS;
	 tieCount++;
     }
#undef SORT_getMS

     swapCount = mergeSort(arr2, arr1, len);

     m2 = getMs(arr2, len);
     s -= (m1 + m2) + 2 * swapCount;

     if (cor) {
	 return (double)s / sqrt((double)(nPair - m1)) / sqrt((double)(nPair - m2));
     } else {
	 /* Return covariance. */
	 return (double)(2 * s);
     }
 }

double kendall_wrapper(double* arr1, double* arr2, size_t len) {
    if (len > INT_MAX -1) error("Kendall wrapper not implemented for large vectors");

    // Count the number of non-NA pairs
    int n_notNAs = 0;
    for (int i = 0; i < len; i++) {
	if (!ISNAN(arr1[i]) && !ISNAN(arr2[i])) n_notNAs++;
    }

    // Only keep non-NA pairs from arr1 and arr2
    double *a1 = (double *) R_alloc(n_notNAs, sizeof(double));
    double *a2 = (double *) R_alloc(n_notNAs, sizeof(double));
    int j = 0;
    for (int i = 0; i < len; i++) {
	if (!ISNAN(arr1[i]) && !ISNAN(arr2[i])) {
	    a1[j] = arr1[i];
	    a2[j] = arr2[i];
	    j++;
	}
    }

    // Create array of indices
    int *ind = (int *) R_alloc(n_notNAs, sizeof(int));
    for (int i = 0; i < n_notNAs; i++) ind[i] = i;

    // Order arrays with the same permutation
    rsort_with_index(a1, ind, n_notNAs);
    double *tmp2 = (double *) R_alloc(n_notNAs, sizeof(double));
    for (int i = 0; i < n_notNAs; i++) {
	tmp2[i] = a2[ind[i]];
    }
    return kendallNlogN(a1, tmp2, n_notNAs, /* cor = */ 0);
}
