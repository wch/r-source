#include "Mathlib.h"

int F77_SYMBOL(i1mach)(int *i)
{
	switch(*i) {

	case  1: return 5;
	case  2: return 6;
	case  3: return 0;
	case  4: return 0;

	case  5: return CHAR_BIT * sizeof(int);
	case  6: return sizeof(int)/sizeof(char);

	case  7: return 2;
	case  8: return CHAR_BIT * sizeof(int) - 1;
	case  9: return INT_MAX;

	case 10: return FLT_RADIX;

	case 11: return FLT_MANT_DIG;
	case 12: return FLT_MIN_EXP;
	case 13: return FLT_MAX_EXP;

	case 14: return DBL_MANT_DIG;
	case 15: return DBL_MIN_EXP;
	case 16: return DBL_MAX_EXP;

	default: return 0;
	}
}
