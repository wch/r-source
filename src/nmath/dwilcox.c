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
 *    double dwilcox(double x, double m, double n)
 *
 *  DESCRIPTION
 *
 *    The density of the Wilcoxon distribution.
 */

#include "Mathlib.h"
#include "Errormsg.h"/* for warning() */

static double *w[WILCOX_MMAX][WILCOX_NMAX];

static double cwilcox(int k, int m, int n) {
  int c, u, i, j, l;

  u = m * n;
  c = (int)(u / 2);

  if ((k < 0) || (k > u))
    return(0);
  if (k > c)
    k = u - k;
  if (m < n) {
    i = m; j = n;
  } else {
    i = n; j = m;
  }
  if (w[i][j] == 0) {
    w[i][j] = (double *) calloc(c + 1, sizeof(double));
    for (l = 0; l <= c; l++)
      w[i][j][l] = -1;
  }
  if (w[i][j][k] < 0) {
    if ((i == 0) || (j == 0))
      w[i][j][k] = (k == 0);
    else
      w[i][j][k] = cwilcox(k - n, m - 1, n) + cwilcox(k, m, n - 1);
  }
  return(w[i][j][k]);
}

double dwilcox(double x, double m, double n) {
#ifdef IEEE_754
  /* NaNs propagated correctly */
  if (ISNAN(x) || ISNAN(m) || ISNAN(n)) return x + m + n;
#endif
  m = floor(m + 0.5);
  n = floor(n + 0.5);
  if (m <= 0 || n <= 0) {
    ML_ERROR(ME_DOMAIN);
    return ML_NAN;
  }
  if (m >= WILCOX_MMAX) {
    warning("m should be less than %d\n", WILCOX_MMAX);
    return ML_NAN;
  }
  if (n >= WILCOX_NMAX) {
    warning("n should be less than %d\n", WILCOX_NMAX);
    return ML_NAN;
  }
  x = floor(x + 0.5);
  if ((x < 0) || (x > m * n))
    return 0;
  return(cwilcox(x, m, n) / choose(m + n, n));
}
