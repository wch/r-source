/* void machar()  -- computes ALL `machine constants' at once.
 * -------------  -- compare with ../nmath/i1mach.c & ../nmath/d1mach.c
 *		     which use the C  <float.h> constants !
 */
/*
 *      algorithm 665, collected algorithms from acm.
 *      this work published in transactions on mathematical software,
 *      vol. 14, no. 4, pp. 303-311.
 *
 *  this fortran 77 subroutine is intended to determine the parameters
 *   of the floating-point arithmetic system specified below.  the
 *   determination of the first three uses an extension of an algorithm
 *   due to m. malcolm, cacm 15 (1972), pp. 949-951, incorporating some,
 *   but not all, of the improvements suggested by m. gentleman and s.
 *   marovich, cacm 17 (1974), pp. 276-277.  an earlier version of this
 *   program was published in the book software manual for the
 *   elementary functions by w. j. cody and w. waite, prentice-hall,
 *   englewood cliffs, nj, 1980.
 *
 *  the program as given here must be modified before compiling.  if
 *   a single (double) precision version is desired, change all
 *   occurrences of cs (  ) in columns 1 and 2 to blanks.
 *
 *  parameter values reported are as follows:
 *
 *       ibeta   - the radix for the floating-point representation
 *       it      - the number of base ibeta digits in the floating-point
 *                 significand
 *       irnd    - 0 if floating-point addition chops
 *                 1 if floating-point addition rounds, but not in the
 *                   ieee style
 *                 2 if floating-point addition rounds in the ieee style
 *                 3 if floating-point addition chops, and there is
 *                   partial underflow
 *                 4 if floating-point addition rounds, but not in the
 *                   ieee style, and there is partial underflow
 *                 5 if floating-point addition rounds in the ieee style,
 *                   and there is partial underflow
 *       ngrd    - the number of guard digits for multiplication with
 *                 truncating arithmetic.  it is
 *                 0 if floating-point arithmetic rounds, or if it
 *                   truncates and only  it  base  ibeta digits
 *                   participate in the post-normalization shift of the
 *                   floating-point significand in multiplication;
 *                 1 if floating-point arithmetic truncates and more
 *                   than  it  base  ibeta  digits participate in the
 *                   post-normalization shift of the floating-point
 *                   significand in multiplication.
 *       machep  - the largest negative integer such that
 *                 1.0+float(ibeta)**machep .ne. 1.0, except that
 *                 machep is bounded below by  -(it+3)
 *       negeps  - the largest negative integer such that
 *                 1.0-float(ibeta)**negeps .ne. 1.0, except that
 *                 negeps is bounded below by  -(it+3)
 *       iexp    - the number of bits (decimal places if ibeta = 10)
 *                 reserved for the representation of the exponent
 *                 (including the bias or sign) of a floating-point
 *                 number
 *       minexp  - the largest in magnitude negative integer such that
 *                 float(ibeta)**minexp is positive and normalized
 *       maxexp  - the smallest positive power of  beta  that overflows
 *       eps     - the smallest positive floating-point number such
 *                 that  1.0+eps .ne. 1.0. in particular, if either
 *                 ibeta = 2  or  irnd = 0, eps = float(ibeta)**machep.
 *                 otherwise,  eps = (float(ibeta)**machep)/2
 *       epsneg  - a small positive floating-point number such that
 *                 1.0-epsneg .ne. 1.0. in particular, if ibeta = 2
 *                 or  irnd = 0, epsneg = float(ibeta)**negeps.
 *                 otherwise,  epsneg = (ibeta**negeps)/2.  because
 *                 negeps is bounded below by -(it+3), epsneg may not
 *                 be the smallest number that can alter 1.0 by
 *                 subtraction.
 *       xmin    - the smallest non-vanishing normalized floating-point
 *                 power of the radix, i.e.,  xmin = float(ibeta)**minexp
 *       xmax    - the largest finite floating-point number.  in
 *                 particular  xmax = (1.0-epsneg)*float(ibeta)**maxexp
 *                 note - on some machines  xmax  will be only the
 *                 second, or perhaps third, largest number, being
 *                 too small by 1 or 2 units in the last digit of
 *                 the significand.
 *
 *     latest revision - april 20, 1987
 *
 *     author - w. j. cody
 *              argonne national laboratory
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <math.h>
#include <R_ext/Applic.h>

void machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep, int *negep,
	int *iexp, int *minexp, int *maxexp, double *eps,
	double *epsneg, double *xmin, double *xmax)
{
	volatile double a, b, beta, betain, betah, one,
		t, temp, tempa, temp1, two, y, z, zero;
	int i, itemp, iz, j, k, mx, nxres;

	one = 1;
	two = one+one;
	zero = one-one;

		/* determine ibeta, beta ala malcolm. */

	a = one;
	do {
		a = a + a;
		temp = a + one;
		temp1 = temp - a;
	}
	while(temp1 - one == zero);
	b = one;
	do {
		b = b + b;
		temp = a + b;
		itemp = temp - a;
	}
	while (itemp == 0);
	*ibeta = itemp;
	beta = *ibeta;

		/* determine it, irnd */

	*it = 0;
	b = one;
	do {
		*it = *it + 1;
		b = b * beta;
		temp = b + one;
		temp1 = temp - b;
	}
	while(temp1 - one == zero);
	*irnd = 0;
	betah = beta / two;
	temp = a + betah;
	if (temp - a != zero)
		*irnd = 1;
	tempa = a + beta;
	temp = tempa + betah;
	if (*irnd == 0 && temp - tempa != zero)
		*irnd = 2;

		/* determine negep, epsneg */

	*negep = *it + 3;
	betain = one / beta;
	a = one;
	for(i=1 ; i<=*negep ; i++)
		a = a * betain;
	b = a;
	for(;;) {
		temp = one - a;
		if (temp - one != zero)
			break;
		a = a * beta;
		*negep = *negep - 1;
	}
	*negep = -*negep;
	*epsneg = a;
	if (*ibeta != 2 && *irnd != 0) {
		a = (a * (one + a)) / two;
		temp = one - a;
		if (temp - one != zero)
			*epsneg = a;
	}

		/* determine machep, eps */

	*machep = -*it - 3;
	a = b;
	for(;;) {
		temp = one + a;
		if (temp - one != zero)
			break;
		a = a * beta;
		*machep = *machep + 1;
	}
	*eps = a;
	temp = tempa + beta * (one + *eps);
	if (*ibeta != 2 && *irnd != 0) {
		a = (a * (one + a)) / two;
		temp = one + a;
		if (temp - one != zero)
			*eps = a;
	}

		/* determine ngrd */

	*ngrd = 0;
	temp = one + *eps;
	if (*irnd == 0 && temp * one - one != zero)
		*ngrd = 1;

	/* determine iexp, minexp, xmin */

	/* loop to determine largest i and k = 2**i such that */
	/*        (1/beta) ** (2**(i)) */
	/* does not underflow. */
	/* exit from loop is signaled by an underflow. */

	i = 0;
	k = 1;
	z = betain;
	t = one + *eps;
	nxres = 0;
	for(;;) {
		y = z;
		z = y * y;

		/* check for underflow here */

		a = z * one;
		temp = z * t;
		if (a+a == zero || fabs(z) >= y)
			break;
		temp1 = temp * betain;
		if (temp1 * beta == z)
			break;
		i = i+1;
		k = k+k;
	}
	if (*ibeta != 10) {
		*iexp = i + 1;
		mx = k + k;
	}
	else {
		/* this segment is for decimal machines only */

		*iexp = 2;
		iz = *ibeta;
		while (k >= iz) {
			iz = iz * *ibeta;
			iexp = iexp + 1;
		}
		mx = iz + iz - 1;
	}
	do {
		/* loop to determine minexp, xmin */
		/* exit from loop is signaled by an underflow */

		*xmin = y;
		y = y * betain;

		/* check for underflow here */

		a = y * one;
		temp = y * t;
		if (a+a == zero || fabs(y) >= *xmin)
			goto L10;
		k = k + 1;
		temp1 = temp * betain;
	}
	while(temp1 * beta != y);
	nxres = 3;
	*xmin = y;
L10:	*minexp = -k;

	/* determine maxexp, xmax */

	if (mx <= k + k - 3 && *ibeta != 10) {
		mx = mx + mx;
		*iexp = *iexp + 1;
	}
	*maxexp = mx + *minexp;

	/* adjust irnd to reflect partial underflow */

	*irnd = *irnd + nxres;

	/* adjust for ieee-style machines */

	if (*irnd == 2 || *irnd == 5)
		*maxexp = *maxexp - 2;

	/* adjust for non-ieee machines with partial underflow */

	if (*irnd == 3 || *irnd == 4)
		*maxexp = *maxexp - *it;

	/* adjust for machines with implicit leading bit in binary */
	/* significand, and machines with radix point at extreme */
	/* right of significand. */

	i = *maxexp + *minexp;
	if (*ibeta == 2 && i == 0)
		*maxexp = *maxexp - 1;
	if (i > 20)
		*maxexp = *maxexp - 1;
	if (a != y)
		*maxexp = *maxexp - 2;
	*xmax = one - *epsneg;
	if (*xmax * one != *xmax)
		*xmax = one - beta * *epsneg;
	*xmax = *xmax / (beta * beta * beta * *xmin);
	i = *maxexp + *minexp + 3;
	if (i>0)
		for(j=1 ; j<=i ; j++) {
			if (*ibeta == 2)
				*xmax = *xmax + *xmax;
			if (*ibeta != 2)
				*xmax = *xmax * beta;
		}
}
