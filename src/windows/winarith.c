/*
 *  R : A Computer Langage for Statistical Data Analysis
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

#include "Mathlib.h"

int isnan(double x)
{
        int hx, lx;
        hx = ((int*)&x)[1];
        lx = ((int*)&x)[0];
        hx &= 0x7fffffff;
        hx |= (unsigned)(lx|(-lx))>>31;
        hx = 0x7ff00000 - hx;
        return (int)((unsigned)(hx))>>31;

}

int finite(double x)
{
        int hx = ((int*)&x)[1];
        return (int)((unsigned)((hx&0x7fffffff)-0x7ff00000)>>31);
}


/* quick hacks at erf and erfc these need to be fixed up */

double erf(double x)
{
        return(pnorm(x, 0, 1));
}




/* 1-erf(x); should be done better than this */
double erfc(double x)
{
        return (pnorm(-x,0,1));
}
