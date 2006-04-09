/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999        The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Factor Tabulation */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h>
#include <R_ext/Applic.h>

void R_tabulate(int *x, int *n, int *nbin, int *ans)
{
    int i;
    if(*n < 1) return;
    for(i = 0 ; i < *n ; i++)
	if(x[i] != R_NaInt && x[i] > 0 && x[i] <= *nbin)
	    ans[x[i] - 1]++;
}
