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
 *    double pwilcox(double x, double m, double n)
 *
 *  DESCRIPTION
 *
 *    The distribution function of the Wilcoxon distribution.
 */

#include "Mathlib.h"
#include "Errormsg.h"/* for warning() */

double pwilcox(double x, double m, double n) {
  int i;
  double p = 0.0;

#ifdef IEEE_754
  if (ISNAN(x) || ISNAN(m) || ISNAN(n))
    return x + n + p;
  if (!FINITE(m) || !FINITE(n)) {
    ML_ERROR(ME_DOMAIN);
    return ML_NAN;
  }
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
  if (x < 0.0)
    return 0;
  if (x >= m * n)
    return 1;
  for (i = 0; i <= x; i++)
    p += dwilcox(i, m, n);
  return(p);
}
