/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#include "nmath.h"

#ifndef IEEE_754

void ml_error(int n)
{
    switch(n) {

    case ME_NONE:
	errno = 0;
	break;

    case ME_DOMAIN:
    case ME_NOCONV:
	errno = EDOM;
	break;

    case ME_RANGE:
	errno = ERANGE;
	break;

    default:
	break;
    }
}

#endif

#ifdef MATHLIB_STANDALONE
#ifdef IEEE_754
/* These are used in IEEE exception handling */
double m_zero = 0;
double m_one = 1;
#endif

/*
 *  based on code in ../main/arithmetic.c
 */


#ifdef IEEE_754

int R_IsNaNorNA(double x)
{
/* NOTE: some systems do not return 1 for TRUE. */
    return (isnan(x) != 0);
}

/* Include the header file defining finite() */
#ifdef HAVE_IEEE754_H
# include <ieee754.h>		/* newer Linuxen */
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>		/* others [Solaris 2.5.x], .. */
# endif
#endif
#if defined(Win32) && defined(_MSC_VER)
#include <float.h>
#endif

int R_finite(double x)
{
#ifdef Macintosh
    return isfinite(x);
#endif
#ifndef FINITE_BROKEN
    return finite(x);
# else
#  ifdef _AIX
#   include <fp.h>
     return FINITE(x);
#  else
    return (!isnan(x) & (x != ML_POSINF) & (x != ML_NEGINF));
#  endif
#endif
}

#else /* not IEEE_754 */

int R_IsNaNorNA(double x)
{
# ifndef HAVE_ISNAN
    return (x == ML_NAN);
# else
    return (isnan(x) != 0 || x == ML_NAN);
# endif
}

int R_finite(double x)
{
# ifndef HAVE_FINITE
    return (x !=  ML_NAN && x < ML_POSINF && x > ML_NEGINF);
# else
    int finite(double);
    return finite(x);
# endif
}
#endif /* IEEE_754 */

#endif /* MATHLIB_STANDALONE */
