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

/* S compatibility library headers - maps internal functions in S to R 
   equivalents
 *
 * $Id: S_compat.h,v 1.1 1997/12/11 23:38:46 ihaka Exp $
 */

#ifndef S_COMPAT_H_
#define S_COMPAT_H_

#include "S.h"
#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

extern void
F77_SYMBOL(dqrdca) (double*, long int*, long int*, long int*,
		    double*, long int*, double*, long int*, double*);

#endif
