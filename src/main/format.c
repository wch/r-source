/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

/*== see ./printutils.c	 for general remarks on Printing and the Encode.. utils.
 *== see ./paste.c	 for do_paste() , do_format() and  do_formatinfo()
 *
 * These  formatFOO (.)	 functions determine the proper	 width, digits, etc:
 *
 */


#include "Defn.h"
#include "Mathlib.h"
#include "Print.h"

void formatString(SEXP *x, int n, int *fieldwidth, int quote)
{
	int xmax = 0, naflag = 0;
	int i, l;

	for (i = 0; i < n; i++) {
		if (CHAR(x[i]) == NULL)
			naflag = 1;
		else {
			l = Rstrlen(CHAR(x[i]));
			if (l > xmax)
				xmax = l;
		}
	}
	*fieldwidth = xmax;
	if (quote)
		*fieldwidth += 2;
	if (naflag && *fieldwidth < print_na_width)
		*fieldwidth = print_na_width;
}

void formatLogical(int *x, int n, int *fieldwidth)
{
	int i;

	*fieldwidth = 1;
	for(i=0 ; i<n ; i++) {
		if (x[i] == 1 && *fieldwidth < 4)
			*fieldwidth = 4;
		if (x[i] == 0 && *fieldwidth < 5 ) {
			*fieldwidth = 5;
			break;
			/* this is the widest it can be so stop */
		}
		if (x[i] == NA_LOGICAL && *fieldwidth <	 print_na_width)
			*fieldwidth =  print_na_width;
	}
}

void formatFactor(int *x, int n, int *fieldwidth, SEXP levels, int nlevs)
{
	int xmax = INT_MIN, naflag = 0;
	int i, l = 0;

	if(isNull(levels)) {
		for(i=0 ; i<n ; i++) {
			if (x[i] == NA_INTEGER || x[i] < 1 || x[i] > nlevs)
				naflag = 1;
			else if (x[i] > xmax)
				xmax = x[i];
		}
		if (xmax > 0)
			l = IndexWidth(xmax);
	}
	else {
		l = 0;
		for(i=0 ; i<n ; i++) {
			if (x[i] == NA_INTEGER || x[i] < 1 || x[i] > nlevs)
				naflag = 1;
			else {
				xmax = strlen(CHAR(STRING(levels)[x[i]-1]));
				if (xmax > l) l = xmax;
			}
		}
	}
	if (naflag) *fieldwidth = print_na_width;
	else *fieldwidth = 1;
	if (l > *fieldwidth) *fieldwidth = l;
}

void formatInteger(int *x, int n, int *fieldwidth)
{
	int xmin = INT_MAX, xmax = INT_MIN, naflag = 0;
	int i, l;

	for (i = 0; i < n; i++) {
		if (x[i] == NA_INTEGER)
			naflag = 1;
		else {
			if (x[i] < xmin) xmin = x[i];
			if (x[i] > xmax) xmax = x[i];
		}
	}

	if (naflag) *fieldwidth = print_na_width;
	else *fieldwidth = 1;

	if (xmin < 0) {
		l = IndexWidth(-xmin) + 1;	/* +1 for sign */
		if (l > *fieldwidth) *fieldwidth = l;
	}
	if (xmax > 0) {
		l = IndexWidth(xmax);
		if (l > *fieldwidth) *fieldwidth = l;
	}
}

/*---------------------------------------------------------------------------
 * scientific format determination for real numbers.
 * This is time-critical code.	 It is worth optimizing.

 *    nsig		digits altogether
 *    kpower+1		digits to the left of "."
 *    kpower+1+sgn	including sign
 */

/*old #define MAXDIG print_digits */

static double tbl[] =
{
	0.e0, 1.e0, 1.e1, 1.e2, 1.e3, 1.e4, 1.e5, 1.e6, 1.e7, 1.e8, 1.e9
};

static double eps;/* = 10^{- print_digits};
			set in formatReal/Complex,  used in scientific() */

static void scientific(double *x, int *sgn, int *kpower, int *nsig)
{
	/* for 1 number	 x , return
	 *	sgn    = 1_{x < 0}  {0/1}
	 *	kpower = Exponent of 10;
	 *	nsig   = min(print_digits, #{significant digits of alpha}
	 *
	 * where  |x| = alpha * 10^kpower   and	 1 <= alpha < 10
	 */
	register double alpha;
	register double r;
	register int kp;
	int j;

	if (*x == 0.0) {
		*kpower = 0;
		*nsig = 1;
		*sgn = 0;
	}
	else {
		if(*x < 0.0) {
			*sgn = 1; r = -*x;
		} else {
			*sgn = 0; r = *x;
		}
		kp = floor(log10(r));/*-->	 r = |x| ;  10^k <= r */
		if (abs(kp) < 10) {
			if (kp >= 0)
				alpha = r / tbl[kp + 1]; /* division slow ? */
			else
				alpha = r * tbl[-kp + 1];
		}
		else alpha = r / pow(10.0, (double)kp);

		/* make sure that alpha is in [1,10) */

		if (10.0 - alpha < eps) {
			alpha /= 10.0;
			kp += 1;
		}
		*kpower = kp;

		/* compute number of digits */

		*nsig = print_digits;
		for (j=1; j <= *nsig; j++) {
			if (fabs(alpha - floor(alpha+0.5)) < eps * alpha) {
				*nsig = j;
				break;
			}
			alpha *= 10.0;
		}
	}
}

void formatReal(double *x, int l, int *m, int *n, int *e)
{
	int left, right, sleft;
	int mnl, mxl, rt, mxsl, mxns, mF;
	int neg, sgn, kpower, nsig;
	int i, naflag;

	eps = pow(10.0, -(double)print_digits);

	naflag = 0;
	neg = 0;
	rt = mxl = mxsl = mxns = INT_MIN;
	mnl = INT_MAX;

	for (i=0; i<l; i++) {
	 if (!FINITE(x[i])) {
		naflag = 1;
	 } else {
		scientific(&x[i], &sgn, &kpower, &nsig);

		left = kpower + 1;
		sleft = sgn + ((left <= 0) ? 1 : left); /* >= 1 */
		right = nsig - left; /* #{digits} right of '.' ( > 0 often)*/
		if (sgn) neg = 1; /* if any < 0, need extra space for sign */

					   /* Infinite precision "F" Format : */
		if (right > rt) rt = right;	/* max digits to right of . */
		if (left > mxl) mxl = left;	/* max digits to  left of . */
		if (left < mnl) mnl = left;	/* min digits to  left of . */
		if (sleft> mxsl) mxsl = sleft;	/* max left including sign(s)*/
		if (nsig > mxns) mxns = nsig;	/* max sig digits */
	 }
	}
	/* F Format (NEW):  use "F" format
	 *	    WHENEVER we use not more space than 'E'
	 *		and still satisfy 'print_digits'

	 * E Format has the form   [S]X[.XXX]E+XX[X]
	 *
	 * This is indicated by setting *e to non-zero (usually 1)
	 * If the additional exponent digit is required *e is set to 2
	 */

	/*-- These  'mxsl' & 'rt'  are	used in	 F Format
	 *   AND in the	 ____ if(.) "F" else "E" ___   below: */
	if (mxl < 0) mxsl = 1 + neg;
	/* old?? if (mxl != mnl && mxl + rt > MAXDIG) rt = MAXDIG - mxl; */
	if (rt < 0)		rt = 0;
	/* NO! else if (rt > MAXDIG)	rt = MAXDIG; */
	mF = mxsl + rt + (rt != 0);	   /* width m for F  format */

	/*-- 'see' how	"E" Exponential format would be like : */
	if (mxl > 100 || mnl < -99) *e = 2;/* 3 digit exponent */
	else *e = 1;
	*n = mxns - 1;
	*m = neg + (*n > 0) + *n + 4 + *e; /* width m for E  format */

	if (mF <= *m) { /* IFF it needs less space : "F" (Fixpoint) format */
		*e = 0;
		*n = rt;
		*m = mF;
	} /* else : "E" Exponential format -- all done above */
	if (naflag && *m < print_na_width)
		*m = print_na_width;
}

#ifdef COMPLEX_DATA
void formatComplex(complex *x, int l, int *mr, int *nr, int *er,
				      int *mi, int *ni, int *ei)
{
	int left, right, sleft;
	int rt, mnl, mxl, mxsl, mxns, mF;
	int i_rt, i_mnl, i_mxl, i_mxsl, i_mxns;
	int neg, sgn;
	int i, kpower, nsig;
	int naflag;

	eps = pow(10.0, -(double)print_digits);

	naflag = 0;
	neg = 0;

	rt  =  mxl =  mxsl =  mxns = INT_MIN;
	i_rt= i_mxl= i_mxsl= i_mxns= INT_MIN;
	i_mnl = mnl = INT_MAX;

	for (i=0; i<l; i++) {
	 if (!FINITE(x[i].r) || !FINITE(x[i].i)) {
		naflag = 1;
	 } else {
	   /* real part */

		scientific(&(x[i].r), &sgn, &kpower, &nsig);

		left = kpower + 1;
		sleft = sgn + ((left <= 0) ? 1 : left); /* >= 1 */
		right = nsig - left; /* #{digits} right of '.' ( > 0 often)*/
		if (sgn) neg = 1; /* if any < 0, need extra space for sign */

		if (right > rt) rt = right;	/* max digits to right of . */
		if (left > mxl) mxl = left;	/* max digits to left of . */
		if (left < mnl) mnl = left;	/* min digits to left of . */
		if (sleft> mxsl) mxsl = sleft;	/* max left including sign(s) */
		if (nsig > mxns) mxns = nsig;	/* max sig digits */

	  /* imaginary part */
		/* this is always unsigned */
		/* we explicitly put the sign in when we print */

		scientific(&(x[i].i), &sgn, &kpower, &nsig);

		left = kpower + 1;
		sleft = ((left <= 0) ? 1 : left);
		right = nsig - left;

		if (right > i_rt) i_rt = right;
		if (left > i_mxl) i_mxl = left;
		if (left < i_mnl) i_mnl = left;
		if (sleft> i_mxsl) i_mxsl = sleft;
		if (nsig > i_mxns) i_mxns = nsig;
	 }
	}

/* overall format for real part	  ---  comments see in	formatReal(.) --*/

	if (mnl == INT_MAX) {
		*er = 0; *ei = 0;
		*mr = print_na_width - 2;
		*mi = 0;
		*nr = 0; *ni = 0;
		return;
	}

	if (mxl < 0) mxsl = 1 + neg;
	if (rt < 0) rt = 0;
	mF = mxsl + rt + (rt != 0);

	if (mxl > 100 || mnl < -99) *er = 2;
	else *er = 1;
	*nr = mxns - 1;
	*mr = neg + (*nr > 0) + *nr + 4 + *er;
	if (mF <= *mr) { /* IFF it needs less space : "F" (Fixpoint) format */
		*er = 0;
		*nr = rt;
		*mr = mF;
	}


/* overall format for imaginary part */

	if (i_mxl < 0) i_mxsl = 1;
	if (i_rt < 0) i_rt = 0;
	mF = i_mxsl + i_rt + (i_rt != 0);

	if (i_mxl > 100 || i_mnl < -99) *ei = 2;
	else *ei = 1;
	*ni = i_mxns - 1;
	*mi = (*ni > 0) + *ni + 4 + *ei;
	if (mF <= *mi) { /* IFF it needs less space : "F" (Fixpoint) format */
		*ei = 0;
		*ni = i_rt;
		*mi = mF;
	}
}
#endif
