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
 *    double rwilcox(double m, double n)
 *    
 *  DESCRIPTION
 *
 *    Random variates from the Wilcoxon distribution.
 *
 */

#include "Mathlib.h"

double rwilcox(double m, double n)
{
  int i, j, k, *x;
  double r;
  
#ifdef IEEE_754
  /* NaNs propagated correctly */
  if (ISNAN(m) || ISNAN(n)) return(m + n);
#endif
  m = floor(m + 0.5);
  n = floor(n + 0.5);
  if ((m < 0) || (n < 0)) {
    ML_ERROR(ME_DOMAIN);
    return ML_NAN;
  }
  if ((m == 0) || (n == 0))
    return(0);
  r = 0.0;
  k = (int) (m + n);
  x = (int *) calloc(k, sizeof(int));
  for (i = 0; i < k; i++)
    x[i] = i;
  for (i = 0; i < n; i++) {
    j = floor(k * sunif());
    r += x[j];
    x[j] = x[--k];
  }
  return(r - n * (n - 1) / 2);
}
