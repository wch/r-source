/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2000   Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* finds dsqrt(a**2+b**2) without overflow or destructive underflow */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Mathlib.h"/* fmax2  fmin2 */
#include "Applic.h"

double pythag(double a, double b)
{
    double p, r, s, t, tmp, u;

    p = fmax2(fabs(a), fabs(b));
    if (p != 0.0) {

	/* r = (fmin(fabs(a), fabs(b))/p)**2 */

	tmp = fmin2(fabs(a), fabs(b))/p;
	r = tmp * tmp;
	for(;;) {
	    t = 4.0 + r;
	    if (t == 4.0)
		break;
	    s = r / t;
	    u = 1.0 + 2.0 * s;
	    p = u * p;

	    /* r = (s / u)**2 * r */

	    tmp = (s / u);
	    r = tmp * tmp * r;
	}
    }
    return p;
}
