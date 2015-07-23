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

/* definitions not involving SEXPs, plus _() */

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

#include <R_ext/RS.h>
void
F77_SUB(hclust)(int *n, int *len, int *iopt, int *ia, int *ib,
		double *crit, double *membr, int *nn,
		double *disnn, int *flag, double *diss);

void
F77_SUB(hcass2)(int *n, int *ia, int *ib, int *iorder, int *iia, int *iib);

void
F77_SUB(kmns)(double *a, int *m, int *n, double *c, int *k,
	      int *ic1, int *ic2, int *nc, double * an1, double *an2,
	      int *ncp, double *d, int *itran,
	      int *live, int *iter, double *wss, int *ifault);


void rcont2(int *nrow, int *ncol, int *nrowt, int *ncolt, int *ntotal,
	    double *fact, int *jwork, int *matrix);

double R_zeroin2(double ax, double bx, double fa, double fb, 
		 double (*f)(double x, void *info), void *info, 
		 double *Tol, int *Maxit);


#endif
