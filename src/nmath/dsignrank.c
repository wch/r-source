/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 R Core Team
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
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double dsignrank(double x, double n)
 *
 *  DESCRIPTION
 *
 *    The density of the Wilcoxon Signed Rank distribution.
 */

#include "Mathlib.h"

static double *w[SIGNRANK_NMAX];

static double csignrank(int k, int n) {
  int c, u, i;

  u = n * (n + 1) / 2;
  c = (int) (u / 2);

  if ((k < 0) || (k > u))
      return(0);
  if (k > c)
      k = u - k;
  if (w[n] == 0) {
      w[n] = (double *) calloc(c + 1, sizeof(double));
      for (i = 0; i <= c; i++)
	  w[n][i] = -1;
  }
  if (w[n][k] < 0) {
      if (n == 0)
	  w[n][k] = (k == 0);
      else
	  w[n][k] = csignrank(k - n, n - 1) + csignrank(k, n - 1);
  }
  return(w[n][k]);
}

double dsignrank(double x, double n) {
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(n)) return x + n;
#endif
    n = floor(n + 0.5);
    if (n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    } else if (n >= SIGNRANK_NMAX) {
	MATHLIB_WARNING("n should be less than %d\n", SIGNRANK_NMAX);
	return ML_NAN;
    }
    x = floor(x + 0.5);
    if ((x < 0) || (x > (n * (n + 1) / 2)))
	return 0;
    return(exp(log(csignrank(x, n)) - n * log(2)));
}
