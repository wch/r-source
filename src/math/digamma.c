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

double digamma(double x)
{
	double ans;
	int nz, ierr;
	dpsifn(x, 0, 1, 1, &ans, &nz, &ierr);
	if(ierr != 0)
		DOMAIN_ERROR;
	return -ans;
}

double trigamma(double x)
{
	double ans;
	int nz, ierr;
	dpsifn(x, 1, 1, 1, &ans, &nz, &ierr);
	if(ierr != 0) {
		errno = EDOM;
		return -DBL_MAX;
	}
	return ans;
}

double tetragamma(double x)
{
	double ans;
	int nz, ierr;
	dpsifn(x, 2, 1, 1, &ans, &nz, &ierr);
	if(ierr != 0) {
		errno = EDOM;
		return -DBL_MAX;
	}
	return -2.0 * ans;
}

double pentagamma(double x)
{
	double ans;
	int nz, ierr;
	dpsifn(x, 3, 1, 1, &ans, &nz, &ierr);
	if(ierr != 0) {
		errno = EDOM;
		return -DBL_MAX;
	}
	return 6.0 * ans;
}
