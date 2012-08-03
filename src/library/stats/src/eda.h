/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2005   The R Core Team.
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

/* ./line.c : */

void tukeyline(double *x, double *y, double *z, double *w, 
	       int *n, double *coef);

/* ./smooth.c : */

typedef enum { 
    sm_NO_ENDRULE, sm_COPY_ENDRULE, sm_TUKEY_ENDRULE 
} R_SM_ENDRULE;


/* Callable from R : */
void Rsm_3RSR (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RSS (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RS3R(double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3R   (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3    (double *x, double *y, int *n, int *end_rule, int *changed);

void Rsm_S    (double *x, double *y, int *n, int *do_ends, int *changed);
