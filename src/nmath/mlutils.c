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
 */

#include "Mathlib.h"

#ifdef IEEE_754
/* These are used in IEEE exception handling */
double m_zero = 0;
double m_one = 1;
double m_tiny = DBL_MIN;
#endif

#ifndef IEEE_754

void ml_error(int n)
{
    switch(n) {

    case ME_NONE:
	errno = 0;
	break;

    case ME_DOMAIN:
    case ME_NOCONV:
	errno = EDOM;
	break;

    case ME_RANGE:
	errno = ERANGE;
	break;

    default:
	break;
    }
}

#endif
