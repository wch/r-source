#include "Mathlib.h"

static int c10 = 10;
static int c14 = 14;

double F77_SYMBOL(d1mach)(int *i)
{
	switch(*i) {
	case 1:
		return DBL_MIN;
	case 2:
		return DBL_MAX;
	case 3:
		return pow((double)F77_SYMBOL(i1mach)(&c10), -(double)F77_SYMBOL(i1mach)(&c14));
	case 4:
		return pow((double)F77_SYMBOL(i1mach)(&c10), 1-(double)F77_SYMBOL(i1mach)(&c14));
	case 5:
		return log10(2.0);
	}
	return 0.0;
}
