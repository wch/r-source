/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998, The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Much of this is from Doug Bates.
 */

#ifndef R_R_H
#define R_R_H

#ifndef USING_R
#define USING_R
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>
#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#endif

#include "R_ext/Arith.h"
#include "R_ext/Complex.h"
#include "R_ext/Constants.h"
#include "R_ext/Error.h"
#include "R_ext/F77.h"
#include "R_ext/Memory.h"
#include "R_ext/Random.h"
#include "R_ext/Sort.h"

void	call_R(char*, long, void**, char**, long*, char**, long, char**);

typedef double Sfloat;
typedef int Sint;
# define SINT_MAX INT_MAX
    

#ifdef __cplusplus
}
#endif

#endif /* !R_R_H */
