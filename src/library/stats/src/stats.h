/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-12   The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_STATS_H
#define R_STATS_H

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

#include <R_ext/RS.h>

void rcont2(int *nrow, int *ncol, int *nrowt, int *ncolt, int *ntotal,
	    double *fact, int *jwork, int *matrix);

void F77_NAME(lminfl)(double *x, int *ldx, int *n, int *k, int *docoef,
		      double *qraux, double *resid, double *hat,
		      double *coef, double *sigma, double *tol);

void stats_signrank_free(void);
void stats_wilcox_free(void);

#endif
