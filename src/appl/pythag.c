/* finds dsqrt(a**2+b**2) without overflow or destructive underflow */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Mathlib.h"/*fmax2 ..*/
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
