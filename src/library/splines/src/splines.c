/*  Routines for manipulating B-splines.  These are intended for use with
 *  S or S-PLUS or R.
 *  
 *     Copyright (C) 1998 Douglas M. Bates and William N. Venables.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * These functions are distributed in the hope that they will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * The text of the GNU General Public License, version 2, is available
 * as http://www.gnu.org/copyleft or by writing to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * The routines are loosely based on the pseudo-code in Schumaker (Wiley,
 * 1981) and the CMLIB library DBSPLINES.
 */

#include "S.h"
#ifndef R_S_H
#define longint long int
#endif /* R_S_H */

static double *ldel, *rdel;
static long orderm1;

static void
diff_table(double *ti, double x, longint ndiff)
{
  register double *r = rdel, *l = ldel, *dpt = ti;
  
  while (ndiff--) {
    *r++ = *dpt++ - x;
    *l++ = x - *--ti;
  }
}
      
static void
basis_funcs(double *ti, double x, double *b)
{
  longint j, r;
  double saved, term;

  diff_table(ti, x, orderm1);
  b[0] = 1.;
  for (j = 1; j <= orderm1; j++) {
    saved = 0.;
    for (r = 0; r < j; r++) {
      term = b[r]/(rdel[r] + ldel[j - 1 - r]);
      b[r] = saved + rdel[r] * term;
      saved = ldel[j - 1 - r] * term;
    }
    b[j] = saved;
  }
}  

static double
evaluate(double *ti, double x, double *a, longint nder)
{
  register double *lpt, *rpt, *apt;
  register longint inner;
  longint outer = orderm1;

  while(nder--) {
    for(inner = outer, apt = a, lpt = ti - outer; inner--; apt++, lpt++)
      *apt = outer * (*(apt + 1) - *apt)/(*(lpt + outer) - *lpt);
    outer--;
  }
  diff_table(ti, x, (long) outer);
  while(outer--)
    for(apt = a, lpt = ldel + outer, rpt = rdel, inner = outer + 1;
	inner--; lpt--, rpt++, apt++)
      *apt = (*(apt + 1) * *lpt + *apt * *rpt)/(*rpt + *lpt);
  return(*a);
}  
  
void
spline_value(double *knots, double *coeff, longint *ncoeff,
	     longint *order, double *x, longint *nx, longint *deriv,
	     double *y)
{
  long n = *nx;
  double *a, *last = knots + *ncoeff;

  a = Calloc(*order, double);
  orderm1 = *order - 1L;	/* allocate difference tables */
  rdel = Calloc(orderm1, double);
  ldel = Calloc(orderm1, double);

  knots += *order;		/* First *order knots must be <= all x's */
  while(n--) {
    while(knots <= last && *knots <= *x) {knots++; coeff++;}
    Memcpy(a, coeff, *order);
    *y++ = evaluate(knots, *x++, a, (longint) *deriv);
  }
  Free(ldel); Free(rdel); Free(a);
}

void spline_basis(double *knots, longint *ncoeff, longint *order,
		  double *xvals, longint *derivs, longint *nx,
		  double *basis, longint *offsets)
{				/* evaluate the non-zero B-spline basis */
				/* functions (or their derivatives) at */
				/* xvals.  */
  longint n = *nx, i, j;
  double *dpt, *coeff, *last = knots + *ncoeff;

  orderm1 = *order - 1L;
  rdel = Calloc(orderm1, double);
  ldel = Calloc(orderm1, double);
  coeff = Calloc(*order, double);
  dpt = (knots += *order);	/* first *order knots must be <= all xvals */
  for( ; n--; xvals++, derivs++) {
    while(dpt < last && *dpt <= *xvals) dpt++;
    if (*derivs) {		/* slow method for derivatives */
      for(i = 0; i < *order; i++) {
	for(j = 0; j < *order; j++) coeff[j] = 0;
	coeff[i] = 1;
	*basis++ = evaluate(dpt, *xvals, coeff, (longint) *derivs);
      }
    }
    else {
      basis_funcs(dpt, *xvals, basis); /* fast method for value */
      basis += *order;
    }
    *offsets++ = (long)(dpt - knots);
  }
  Free(ldel); Free(rdel); Free(coeff);
}

void lin_interp(double *x, double *y, double *x0, double *y0, longint *nvals)
{
  longint n = *nvals;
  double *firstx = x;

  while(n--) {
    while (*x < *x0) {x++; y++;}
    if (x > firstx) {x--; y--;}
    if (*x > *x0) *y0++ = *y + (*(y+1) - *y)*(*x0 - *x)/(*(x+1) - *x);
    else *y0++ = *y;
    x0++;
  }
}
