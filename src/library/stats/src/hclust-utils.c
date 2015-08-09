/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1999-2014   The R Core Team
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
 *  https://www.R-project.org/Licenses/.
 */

#include <R_ext/Boolean.h>
#include <Rinternals.h>
#include "statsR.h"

SEXP cutree(SEXP merge, SEXP which)
{
/* Return grouping vector from cutting a (binary) (cluster) tree
 * into which[j] groups.
 * merge = (n-1) x 2  matrix, described in help(hclust) */
    SEXP ans;
    int n, k, l, nclust, m1, m2, j, mm = 0;
    Rboolean found_j, *sing;
    int *m_nr, *z, *i_merge, *i_which, *i_ans;

    PROTECT(merge = coerceVector(merge, INTSXP)); i_merge = INTEGER(merge);
    PROTECT(which = coerceVector(which, INTSXP)); i_which = INTEGER(which);

    n = nrows(merge)+1;
    /* using 1-based indices ==> "--" */
    sing = (Rboolean *) R_alloc(n, sizeof(Rboolean)); sing--;
    m_nr = (int *) R_alloc(n, sizeof(int)); m_nr--;
    z	 = (int *) R_alloc(n, sizeof(int)); z--;
    PROTECT(ans = allocMatrix(INTSXP, n, LENGTH(which)));
    i_ans = INTEGER(ans);

    for(k = 1; k <= n; k++) {
	sing[k] = TRUE;/* is k-th obs. still alone in cluster ? */
	m_nr[k] = 0;/* containing last merge-step number of k-th obs. */
    }

    for(k = 1; k <= n-1; k++) {
	/* k-th merge, from n-k+1 to n-k atoms: (m1,m2) = merge[ k , ] */
	m1 = i_merge[k-1];
	m2 = i_merge[n-1+k-1];

	if(m1 < 0 && m2 < 0) {/* merging atoms [-m1] and [-m2] */
	    m_nr[-m1] = m_nr[-m2] = k;
	    sing[-m1] = sing[-m2] = FALSE;
	}
	else if(m1 < 0 || m2 < 0) {/* the other >= 0 */
	    if(m1 < 0) { j = -m1; m1 = m2; } else j = -m2;
	    /* merging atom j & cluster m1 */
	    for(l = 1; l <= n; l++)
		if (m_nr[l] == m1)
		    m_nr[l] = k;
	    m_nr[j] = k;
	    sing[j] = FALSE;
	}
	else { /* both m1, m2 >= 0 */
	    for(l=1; l <= n; l++) {
		if( m_nr[l] == m1 || m_nr[l] == m2 )
		    m_nr[l] = k;
	    }
	}

	/* does this k-th merge belong to a desired group size which[j] ?
	 * if yes, find j (maybe multiple ones): */
	found_j = FALSE;
	for(j = 0; j < LENGTH(which); j++) {
	    if(i_which[j] == n - k) {
		if(!found_j) { /* first match (and usually only one) */
		    found_j = TRUE;
		    for(l = 1; l <= n; l++)
			z[l] = 0;
		    nclust = 0;
		    mm = j*n; /*may want to copy this column of ans[] */
		    for(l = 1, m1 = mm; l <= n; l++, m1++) {
			if(sing[l])
			    i_ans[m1] = ++nclust;
			else {
			    if (z[m_nr[l]] == 0)
				z[m_nr[l]] = ++nclust;
			    i_ans[m1] = z[m_nr[l]];
			}
		    }
		}
		else { /* found_j: another which[j] == n-k : copy column */
		    for(l = 1, m1 = j*n, m2 = mm; l <= n; l++, m1++, m2++)
			i_ans[m1] = i_ans[m2];
		}
	    } /* if ( match ) */
	} /* for(j .. which[j] ) */
    } /* for(k ..) {merge} */

    /* Dealing with trivial case which[] = n : */
    for(j = 0; j < LENGTH(which); j++)
	if(i_which[j] == n)
	    for(l = 1, m1 = j*n; l <= n; l++, m1++)
		i_ans[m1] = l;

    UNPROTECT(3);
    return(ans);
}
