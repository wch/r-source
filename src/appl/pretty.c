/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998  Robert Gentleman, Ross Ihaka and R core team.
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Pretty Intervals
 * ----------------
 * Constructs "pretty" values which cover the given interval
 *
 * It is not quite clear what should happen for	 *lo = *up;
 * S itself behaves quite funilly, then.
 *
 * In my opinion, a proper 'pretty' should always ensure
 * *lo < *up, and hence *ndiv >=1 in the result.
 * However, in S and here, we allow  *lo == *up, and *ndiv = 0.
 * Note however, that we are NOT COMPLETELY COMPATIBLE to S. [Martin M.]
 *
 * We determine
 * if the interval (up - lo) is ``small'' [<==>	 i_small == 1, below].
 * In that case integer overflow might occur in *lo/unit or *up/unit.

 * For the ``i_small'' situation, there is a NEW PARAMETER `shrink_sml',
 * the factor by which the "scale" is shrunk.		    ~~~~~~~~~~
 * It is advisable to set it to some (smaller) integer power of 2,
 * since this enables exact floating point division.
 */

#include "Mathlib.h"
#ifdef DEBUG
# include <stdio.h>
#endif

int pretty(double *lo, double *up, int *ndiv, double *shrink_sml)
{
	double	dx, cell, unit, base, U;
	int	ns, nu;
	short	i_small;

	dx = *up - *lo;
	/* cell := "scale"  here */
	if(dx == 0 && *up == 0) { /*  up == lo == 0  */
		cell = i_small = 1;
	} else {
		cell = fmax2(fabs(*lo),fabs(*up));
		i_small = dx < cell * 10/(double)INT_MAX;
	}

	/*OLD: cell = FLT_EPSILON+ dx / *ndiv; FLT_EPSILON = 1.192e-07 */
	if(i_small)
		cell *= *shrink_sml;
	else
		cell = dx;
	cell /= *ndiv;

	base = pow(10, ((int)(999+log10(cell)))-999);
	/* unit :=  arg min _u { |u - cell| ;  u = c(1,2,5,10) * base } */
	unit = base;
	if(fabs((U = 2*base)-cell) < fabs(unit-cell)) unit = U;
	if(fabs((U = 5*base)-cell) < fabs(unit-cell)) unit = U;
	if(fabs((U =10*base)-cell) < fabs(unit-cell)) unit = U;

#ifdef DEBUG
	fprintf(stderr, "pretty(lo=%g,up=%g,ndiv=%d)\n", *lo, *up, *ndiv);
	fprintf(stderr, "\t dx=%g; is.small:%d. ==> cell=%g; unit=%g\n",
		dx, (int)i_small, cell, unit);
#endif
	ns = ((int)(999+ *lo/unit))-999;
	while( ns*unit > *lo )
		ns--;
	nu = 999-(int)(999- *up/unit);
	while( nu*unit < *up )
		nu++;
	*lo = ns * unit;
	*up = nu * unit;
	*ndiv = nu - ns;
#ifdef DEBUG
	fprintf(stderr, "\t ns=%d ==> lo=%g\n", ns, *lo);
	fprintf(stderr, "\t nu=%d ==> up=%g  ==> ndiv = %d\n", nu, *up, *ndiv);
#endif
	return 0;
}

