/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *                2002  R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Error.h>
#include <R_ext/Arith.h>
#include <R_ext/Applic.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

/* bincode  cuts up the data using half open intervals defined as [a,b)
   (if right = FALSE) or (a, b] (if right = TRUE)
*/
void bincode(double *x, int *pn, double *breaks, int *pnb, int *code,
	     int *right, int *include_border, int *naok)
{
    int i, lo, hi;
    int n, nb1, new, lft;

    n = *pn;
    nb1 = *pnb - 1;
    lft = !(*right);

    for(i = 0; i < n; i++) {
	code[i] = NA_INTEGER;
	if(!ISNAN(x[i])) {
	    lo = 0;
	    hi = nb1;
	    if(x[i] <  breaks[lo] || breaks[hi] < x[i] ||
	       (x[i] == breaks[lft ? hi : lo] && ! *include_border)) ;
	    else {
		while(hi - lo >= 2) {
		    new = (hi + lo)/2;
		    if(x[i] > breaks[new] || (lft && x[i] == breaks[new]))
			lo = new;
		    else
			hi = new;
		}
		code[i] = lo + 1;
	    }
	} else if (! *naok)
	    error(_("NA's in .C(\"bincode\",... NAOK=FALSE)"));
    }
}

/* bincount is called by  hist(.)  [only]
 */

void bincount(double *x, int *pn, double *breaks, int *pnb, int *count,
	      int *right, int *include_border, int *naok)
{
    int i, lo, hi;
    int n, nb1, new, lft;

    n = *pn;
    nb1 = *pnb - 1;
    lft = !(*right);

    for(i=0; i<nb1; i++)
	count[i] = 0;

    for(i=0 ; i<n ; i++)
	if(R_FINITE(x[i])) {
	    lo = 0;
	    hi = nb1;
	    if(breaks[lo] <= x[i] &&
	       (x[i] < breaks[hi] ||
		(x[i]==breaks[hi] && *include_border))) {
		while(hi-lo >= 2) {
		    new = (hi+lo)/2;
		    if(x[i] > breaks[new] || (lft && x[i] == breaks[new]))
			lo = new;
		    else
			hi = new;
		}
		count[lo] += 1;
	    }
	} else if (! *naok)
	    error(_("NA's in .C(\"bincount\",... NAOK=FALSE)"));
}
