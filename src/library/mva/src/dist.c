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

#include <math.h>
#include <float.h>

double realNA(void);
int isFinite(double);

#define NA_REAL   	realNA()
#define FINITE(x)	isFinite(x)

#define EUCLIDEAN 1
#define MAXIMUM   2
#define MANHATTAN 3
#define CANBERRA  4
#define BINARY    5

static double euclidean(double *x, int nr, int nc, int i1, int i2)
{
	double count, dev, dist;
	int j;

	count = 0;
	dist = 0;
	for(j=0 ; j<nc ; j++) {
		if(FINITE(x[i1]) && FINITE(x[i2])) {
			dev = (x[i1] - x[i2]);
			dist += dev * dev;
			count++;
		}
		i1 += nr;
		i2 += nr;
	}
	if(count == 0) return NA_REAL;
	if(count != nc) dist *= (count/nc);
	return sqrt(dist);
}

static double maximum(double *x, int nr, int nc, int i1, int i2)
{
	double count, dev, dist;
	int j;

	count = 0;
	dist = -DBL_MAX;
	for(j=0 ; j<nc ; j++) {
		if(FINITE(x[i1]) && FINITE(x[i2])) {
			dev = fabs(x[i1] - x[i2]);
			if(dev > dist)
				dist = dev;
			count++;
		}
		i1 += nr;
		i2 += nr;
	}
	if(count == 0) return NA_REAL;
	return dist;
}

static double manhattan(double *x, int nr, int nc, int i1, int i2)
{
	double count, dist;
	int j;

	count = 0;
	dist = 0;
	for(j=0 ; j<nc ; j++) {
		if(FINITE(x[i1]) && FINITE(x[i2])) {
			dist += fabs(x[i1] - x[i2]);
			count++;
		}
		i1 += nr;
		i2 += nr;
	}
	if(count == 0) return NA_REAL;
	if(count != nc) dist *= (count/nc);
	return dist;
}

static double canberra(double *x, int nr, int nc, int i1, int i2)
{
	double count, dist;
	int j;

	count = 0;
	dist = 0;
	for(j=0 ; j<nc ; j++) {
		if(FINITE(x[i1]) && FINITE(x[i2])) {
			dist += fabs(x[i1] - x[i2])/(x[i1] + x[i2]);
			count++;
		}
		i1 += nr;
		i2 += nr;
	}
	if(count == 0) return NA_REAL;
	if(count != nc) dist /= count;
	return dist;
}

static double binary(double *x, int nr, int nc, int i1, int i2)
{
	double total, count, dist;
	int j;
	count = 0;
	dist = 0;
	for(j=0 ; j<nc ; j++) {
		if(FINITE(x[i1]) && FINITE(x[i2])) {
			if(x[i1] == 0 && x[i2] == 0)
				continue;
			else if(x[i1] == 0 || x[i2] == 0) {
				dist += 1;
				count += 1;
			}
			else {
				count += 1;
			}
			total += 1;
		}
		i1 += nr;
		i2 += nr;
	}
	if(total == 0) return NA_REAL;
	if(count == 0) return 0;
	return dist / count;
}


void dist(double *x, int *nr, int *nc, double *d, int *method)
{
	double (*distfun)(double*, int, int, int, int);
	int dc, i, j, ij;

	switch(*method) {
	case EUCLIDEAN:
		distfun = euclidean;
		break;
	case MAXIMUM:
		distfun = maximum;
		break;
	case MANHATTAN:
		distfun = manhattan;
		break;
	case CANBERRA:
		distfun = canberra;
		break;
	case BINARY:
		distfun = binary;
	default:
		error("invalid distance");
	}

	ij = 0;
	for(j=0 ; j<=*nr ; j++)
		for(i=j+1 ; i<*nr ; i++)
			d[ij++] = distfun(x, *nr, *nc, i, j);
}
