/*
  Mathlib : A C Library of Special Functions
  Copyright (C) 1999 R Development Core Team
 
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.
 
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 
  SYNOPSIS
 
    #include "Mathlib.h"
    double dwilcox(double x, double m, double n)    
    double pwilcox(double x, double m, double n)
    double qwilcox(double x, double m, double n);    
    double rwilcox(double m, double n)

  DESCRIPTION

    dwilcox	The density of the Wilcoxon distribution.
    pwilcox	The distribution function of the Wilcoxon distribution.
    qwilcox	The quantile function of the Wilcoxon distribution.
    rwilcox	Random variates from the Wilcoxon distribution.

 */

#include "Mathlib.h"

static double ***w;

static void
w_free(int m, int n)
{
    int i, j;

    if (m > n) {
	i = n; n = m; m = i;
    }
    m = imax2(m, WILCOX_MAX);
    n = imax2(n, WILCOX_MAX);

    for (i = m; i >= 0; i--) {
	for (j = n; j >= 0; j--) {
	    if (w[i][j] != 0)
		free((void *) w[i][j]);
	}
	free((void *) w[i]);
    }
    free((void *) w);
    w = 0;
}

static void
w_init_maybe(int m, int n)
{
    int i;
    
    if (w && (m > WILCOX_MAX || n > WILCOX_MAX))
	w_free(WILCOX_MAX, WILCOX_MAX);

    if (!w) {
	if (m > n) {
	    i = n; n = m; m = i;
	}
	m = imax2(m, WILCOX_MAX);
	n = imax2(n, WILCOX_MAX);
	w = (double ***) calloc(m + 1, sizeof(double **));
	if (!w)
	    error("wilcox allocation error 1");
	for (i = 0; i <= m; i++) {
	    w[i] = (double **) calloc(n + 1, sizeof(double *));
	    if (!w[i])
		error("wilcox allocation error 2");
	}
    }
}

static void
w_free_maybe(int m, int n)
{
    if (m > WILCOX_MAX || n > WILCOX_MAX)
	w_free(m, n);
}

static double
cwilcox(int k, int m, int n)
{
    int c, u, i, j, l;

    u = m * n;
    c = (int)(u / 2);
    
    if ((k < 0) || (k > u))
	return(0);
    if (k > c)
	k = u - k;
    if (m < n) {
	i = m; j = n;
    } else {
	i = n; j = m;
    }
    
    if (w[i][j] == 0) {
	w[i][j] = (double *) calloc(c + 1, sizeof(double));
	for (l = 0; l <= c; l++)
	    w[i][j][l] = -1;
    }
    if (w[i][j][k] < 0) {
	if ((i == 0) || (j == 0))
	    w[i][j][k] = (k == 0);
	else
	    w[i][j][k] = cwilcox(k - n, m - 1, n)
		+ cwilcox(k, m, n - 1);
	
    }
    return(w[i][j][k]);
}

double
dwilcox(double x, double m, double n)
{
    double d;
    
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (m <= 0 || n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }

    if (fabs(x - floor(x + 0.5)) > 1e-7)
	return(0);
    x = floor(x + 0.5);
    if ((x < 0) || (x > m * n))
	return(0);

    w_init_maybe(m, n);
    d = cwilcox(x, m, n) / choose(m + n, n);
    w_free_maybe(m, n);

    return(d);
}

double
pwilcox(double x, double m, double n)
{
    int i;
    double c, p;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
    if (!FINITE(m) || !FINITE(n)) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (m <= 0 || n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }

    x = floor(x + 1e-7);

    if (x < 0.0)
	return(0);
    if (x >= m * n)
	return(1);

    w_init_maybe(m, n);
    c = choose(m + n, n);
    p = 0;
    if (x <= (m * n / 2)) {
	for (i = 0; i <= x; i++)
	    p += cwilcox(i, m, n) / c;
    }
    else {
	x = m * n - x;
	for (i = 0; i < x; i++)
	    p += cwilcox(i, m, n) / c;
	p = 1 - p;
    }
    w_free_maybe(m, n);
    
    return(p);
}

double
qwilcox(double x, double m, double n)
{
    double c, p, q;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
    if(!FINITE(x) || !FINITE(m) || !FINITE(n)) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if (x < 0 || x > 1 || m <= 0 || n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }

    if (x == 0)
	return(0);
    if (x == 1)
	return(m * n);

    w_init_maybe(m, n);
    c = choose(m + n, n);
    p = 0;
    q = 0;
    if (x <= 0.5) {
	for (;;) {
	    p += cwilcox(q, m, n) / c;
	    if (p >= x)
		break;
	    q++;
	}
    }
    else {
	x = 1 - x;
	for (;;) {
	    p += cwilcox(q, m, n) / c;
	    if (p > x) {
		q = m * n - q;
		break;
	    }
	    q++;
	}
    }
    w_free_maybe(m, n);

    return(q);
    
}

double
rwilcox(double m, double n)
{
    int i, j, k, *x;
    double r;
  
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(m) || ISNAN(n))
	return(m + n);
#endif
    m = floor(m + 0.5);
    n = floor(n + 0.5);
    if ((m < 0) || (n < 0)) {
	ML_ERROR(ME_DOMAIN);
	return(ML_NAN);
    }

    if ((m == 0) || (n == 0))
	return(0);

    r = 0.0;
    k = (int) (m + n);
    x = (int *) calloc(k, sizeof(int));
    for (i = 0; i < k; i++)
	x[i] = i;
    for (i = 0; i < n; i++) {
	j = floor(k * sunif());
	r += x[j];
	x[j] = x[--k];
    }
    return(r - n * (n - 1) / 2);
}
