/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *    double fmod(double x, double y);
 *
 *  DESCRIPTION
 *
 *    Floating-point remainder of x / y;
 *
 *  NOTES
 *
 *    It may be better to use the system version of this function,
 *    but this version is portable.
 */

#include "Mathlib.h"

double fmod(double x, double y)
{
    double quot;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(y))
	return x + y;
#endif
    quot = x / y;
    return x - (quot < 0.0 ? ceil(quot) : floor(quot)) * y;
}
