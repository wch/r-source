/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2001   Robert Gentleman, Ross Ihaka 
 *                            and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_COMPLEX_H
#define R_COMPLEX_H

#ifdef  __cplusplus
extern "C" {
#endif

typedef struct {
	double r;
	double i;
} Rcomplex;

#ifdef  __cplusplus
}
#endif

#endif /* R_COMPLEX_H */
