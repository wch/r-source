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
 *    double sunif(void);
 *
 *  DESCRIPTION
 *
 *     Random variates from the standard uniform distribution, U(0,1).
 *
 *  REFERENCE
 *
 *    Wichmann, B. A. and I. D. Hill (1982).
 *    Algorithm AS 183: An efficient and portable
 *    pseudo-random number generator,
 *    Applied Statistics, 31, 188.
 */

#include "Mathlib.h"

#define WICHMANN_HILL

#ifdef WICHMANN_HILL

int ix_seed = 123;
int iy_seed = 1234;
int iz_seed = 12345;

double sunif(void)
{
	double value;

	ix_seed = ix_seed * 171 % 30269;
	iy_seed = iy_seed * 172 % 30307;
	iz_seed = iz_seed * 170 % 30323;
	value = ix_seed / 30269.0 + iy_seed / 30307.0 + iz_seed / 30323.0;
	return value - (int) value;
}

#endif
