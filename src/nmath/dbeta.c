/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *
 *  DESCRIPTION
 *    Beta density,
 *                   (a+b-1)!     a-1       b-1
 *      p(x;a,b) = ------------ x     (1-x)
 *                 (a-1)!(b-1)!
 *
 *               = (a+b-1) dbinom(a-1; a+b-2,x)
 *
 *    We must modify this when a<1 or b<1, to avoid passing negative
 *    arguments to dbinom_raw. Note that the modifications require
 *    division by x and/or 1-x, so cannot be used except where necessary.
 */

#include "nmath.h"
#include "dpq.h"

double dbeta(double x, double a, double b, int give_log)
{ 
    double f, p;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(a) || ISNAN(b)) return x + a + b;
#endif

    if (a <= 0 || b <= 0) ML_ERR_return_NAN;
    if (x < 0 || x > 1) return(R_D__0);
    if (x == 0) {
	if(a > 1) return(R_D__0);
	if(a < 1) return(ML_POSINF);
	/* a == 1 : */ return(R_D_val(b));
    } 
    if (x == 1) {
	if(b > 1) return(R_D__0);
	if(b < 1) return(ML_POSINF);
	/* b == 1 : */ return(R_D_val(a));
    } 
    if (a < 1) { 
	if (b < 1) {		/* a,b < 1 */
	    f = a*b/((a+b)*x*(1-x));
	    p = dbinom_raw(a,a+b, x,1-x, give_log);
	}
	else {			/* a < 1 <= b */
	    f = a/x;
	    p = dbinom_raw(a,a+b-1, x,1-x, give_log);
	}
    }
    else { 
	if (b < 1) {		/* a >= 1 > b */
	    f = b/(1-x);
	    p = dbinom_raw(a-1,a+b-1, x,1-x, give_log);
	}
	else {			/* a,b >= 1 */
	    f = a+b-1;
	    p = dbinom_raw(a-1,a+b-2, x,1-x, give_log);
	}
    }
    return( (give_log) ? p + log(f) : p*f );
}
