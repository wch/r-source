/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998 ff  Robert Gentleman, Ross Ihaka and the R core team
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
 *
 * Application Routines, typically implemented in  ../appl/
 */

#ifndef APPLIC_H_
#define APPLIC_H_

/*#include "Complex.h"*/

double euclidean(double *, int, int, int, int);
double maximum(double *, int, int, int, int);
double manhattan(double *, int, int, int, int);
double canberra(double *, int, int, int, int);
double binary(double *, int, int, int, int);
#define EUCLIDEAN 1
#define MAXIMUM   2
#define MANHATTAN 3
#define CANBERRA  4
#define BINARY	  5
void distance(double *, int *, int *, double *, int *, int *);

void approx(double *, double *, int *, double *, int *,
	    int *, double *, double *, double *);

void bakslv(double *, int *, int *,
	    double *, int *, int *,
	    double *, int *, int *);

void bincode(double *, int *, double *, int *, int *,
	     int *, int *);
void bincode2(double *, int *, double *, int *, int *,
	      int *, int *);

/* cpoly.c : */
void polyev(int *,
	    double *, double *, double *, double *,
	    double *, double *, double *, double *);
double errev(int *, double *, double *, double *,
	     double *, double *, double *);
double cauchy(int *, double *, double *);
void scale(int *, double *, double *, double *, double *, double *, double *);
void cdivid(double *, double *,double *, double *, double *, double *);
double cmod(double *, double *);



#endif
