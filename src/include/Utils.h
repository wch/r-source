/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998, 1999   Robert Gentleman, Ross Ihaka
 *                             and the R Development Core Team
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
 *
 * Generally useful  UTILITIES  *NOT* relying on R internals (from Defn.h)
 */

#ifndef UTILS_H_
#define UTILS_H_

#include "Complex.h"

/* ../main/sort.c : */
void	isort(int*,     int);
void	rsort(double*, int);
void	csort(Rcomplex*, int);
void    rsort_with_index(double *, int *, int);
void	revsort(double*, int*, int);/* reverse; sort i[] alongside */
void	iPsort(int*,    int, int);
void	rPsort(double*, int, int);
void	cPsort(Rcomplex*, int, int);

int	IndexWidth(int);
int	Rstrlen(char*);
char*	R_ExpandFileName(char*);
void	setIVector(int*, int, int);
void	setRVector(double*, int, double);
int	StringFalse(char*);
int	StringTrue(char*);
int	isBlankString(unsigned char *);

void	hsv2rgb(double *h, double *s, double *v,/* in */
		double *r, double *g, double *b);/* out */

#endif
