/* Copyright (C) 1997-1999  Adrian Trapletti
  
   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
  
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
  
   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
*/

#include <R.h>
#include "ts.h"

void R_pp_sum (double *u, int *n, int *l, double *sum)
{
  int i, j;
  double tmp1, tmp2;
  
  tmp1 = 0.0;
  for (i=1; i<=(*l); i++)
  {
    tmp2 = 0.0;
    for (j=i; j<(*n); j++)  
    {
      tmp2 += u[j]*u[j-i];
    }
    tmp2 *= 1.0-((double)i/((double)(*l)+1.0));
    tmp1 += tmp2;
  }
  tmp1 /= (double)(*n);
  tmp1 *= 2.0;
  (*sum) += tmp1;
}

void R_intgrt_vec (double *x, double *y, int *lag, int *n)
{
  int i;

  for (i=*lag; i<*lag+*n; i++)
    y[i] = x[i-*lag]+y[i-*lag];
}
