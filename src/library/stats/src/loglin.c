/* Algorithm AS 51 Appl. Statist. (1972), vol. 21, p. 218
   original (C) Royal Statistical Society 1972

   Performs an iterative proportional fit of the marginal totals of a
   contingency table.
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <math.h>

#include <stdio.h>
#include <R_ext/Memory.h>
#include <R_ext/Applic.h>

#undef max
#undef min
#undef abs
#define	max(a, b) 		((a) < (b) ? (b) : (a))
#define	min(a, b) 		((a) > (b) ? (b) : (a))
#define	abs(x)			((x) >= 0 ? (x) : -(x))

static void collap(int *nvar, double *x, double *y, int *locy,
		   int *nx, int *ny, int *dim, int *config);
static void adjust(int *nvar, double *x, double *y, double *z,
		   int *locz, int *nx, int *ny, int *nz, int *dim,
		   int *config, double *d);
static int *lvector(int n);

/* Table of constant values */

static int c__1 = 1;

void loglin(int *nvar, int *dim, int *ncon, int *config, int *ntab,
	    double *table, double *fit, int *locmar, int *nmar, double *marg,
	    int *nu, double *u, double *maxdev, int *maxit,
	    double *dev, int *nlast, int *ifault)
{
    int i, j, k, n, point, size, *icon, *check;
    double x, y, xmax;

    check = lvector(*nvar);
    icon = lvector(*nvar);

    /* Parameter adjustments */
    --dim;
    --locmar;
    config -= *nvar + 1;
    --fit;
    --table;
    --marg;
    --u;
    --dev;

    /* Function body */

    *ifault = 0;
    *nlast = 0;

    /* Check validity of NVAR, the number of variables, and of maxit,
       the maximum number of iterations */

    if (*nvar > 0 && *maxit > 0) {
	goto L10;
    }
L5:
    *ifault = 4;
    return;

    /* Look at table and fit constants */

L10:
    size = 1;
    for (j = 1; j <= *nvar; ++j) {
	if (dim[j] <= 0) {
	    goto L5;
	}
	size *= dim[j];
    }
    if (size <= *ntab) {
	goto L40;
    }
L35:
    *ifault = 2;
    return;
L40:
    x = 0.;
    y = 0.;
    for (i = 1; i <= size; ++i) {
	if (table[i] < 0. || fit[i] < 0.) {
	    goto L5;
	}
	x += table[i];
	y += fit[i];
    }

    /* Make a preliminary adjustment to obtain the fit to an empty
       configuration list */

    if (y == 0.) {
	goto L5;
    }
    x /= y;
    for (i = 1; i <= size; ++i) {
	fit[i] = x * fit[i];
    }
    if (*ncon <= 0 || config[*nvar + 1] == 0) {
	return;
    }

    /* Allocate marginal tables */

    point = 1;
    for (i = 1; i <= *ncon; ++i) {
	/* A zero beginning a configuration indicates that the list is
	   completed */
	if (config[i * *nvar + 1] == 0) {
	    goto L160;
	}
	/* Get marginal table size.  While doing this task, see if the
	   configuration list contains duplications or elements out of
	   range. */
	size = 1;
	for (j = 0; j < *nvar; ++j) {
	    check[j] = 0;
	}
	for (j = 1; j <= *nvar; ++j) {
	    k = config[j + i * *nvar];
	    /* A zero indicates the end of the string. */
	    if (k == 0) {
		goto L130;
	    }
	    /* See if element is valid. */
	    if (k >= 0 && k <= *nvar) {
		goto L100;
	    }
L95:
	    *ifault = 1;
	    return;
	    /* Check for duplication */
L100:
	    if (check[k - 1]) {
		goto L95;
	    }
	    check[k - 1] = (1);
	    /* Get size */
	    size *= dim[k];
	}

	/* Since U is used to store fitted marginals, size must not
	   exceed NU */
L130:
	if (size > *nu) {
	    goto L35;
	}

	/* LOCMAR points to marginal tables to be placed in MARG */
	locmar[i] = point;
	point += size;
    }

    /* Get N, number of valid configurations */

    i = *ncon + 1;
L160:
    n = i - 1;

    /* See if MARG can hold all marginal tables */

    if (point > *nmar + 1) {
	goto L35;
    }

    /* Obtain marginal tables */

    for (i = 1; i <= n; ++i) {
	for (j = 1; j <= *nvar; ++j) {
	    icon[j - 1] = config[j + i * *nvar];
	}
	collap(nvar, &table[1], &marg[1], &locmar[i], ntab, nmar,
	       &dim[1], icon);
    }

    /* Perform iterations */

    for (k = 1; k <= *maxit; ++k) {
	/* XMAX is maximum deviation observed between fitted and true
	   marginal during a cycle */
	xmax = 0.;
	for (i = 1; i <= n; ++i) {
	    for (j = 1; j <= *nvar; ++j) {
		icon[j - 1] = config[j + i * *nvar];
	    }
	    collap(nvar, &fit[1], &u[1], &c__1, ntab, nu, &dim[1],
		   icon);
	    adjust(nvar, &fit[1], &u[1], &marg[1], &locmar[i], ntab, nu,
		   nmar, &dim[1], icon, &xmax);
	}
	/* Test convergence */
	dev[k] = xmax;
	if (xmax < *maxdev) {
	    goto L240;
	}
    }
    if (*maxit > 1) {
	goto L230;
    }
    *nlast = 1;
    return;

    /* No convergence */
L230:
    *ifault = 3;
    *nlast = *maxit;
    return;

    /* Normal termination */
L240:
    *nlast = k;

    return;
}

/* Algorithm AS 51.1 Appl. Statist. (1972), vol. 21, p. 218

   Computes a marginal table from a complete table.
   All parameters are assumed valid without test.

   The larger table is X and the smaller one is Y.
*/

void collap(int *nvar, double *x, double *y, int *locy, int *nx, int
	   *ny, int *dim, int *config)
{
    int i, j, k, l, n, locu, *coord, *size;

    size = lvector(*nvar + 1);
    coord = lvector(*nvar);

    /* Parameter adjustments */
    --config;
    --dim;
    --x;
    --y;

    /* Initialize arrays */

    size[0] = 1;
    for (k = 1; k <= *nvar; ++k) {
	l = config[k];
	if (l == 0) {
	    goto L20;
	}
	size[k] = size[k - 1] * dim[l];
    }

    /* Find number of variables in configuration */

    k = *nvar + 1;
L20:
    n = k - 1;

    /* Initialize Y.  First cell of marginal table is at Y(LOCY) and
       table has SIZE(K) elements */

    locu = *locy + size[k - 1] - 1;
    for (j = *locy; j <= locu; ++j) {
	y[j] = 0.;
    }

    /* Initialize coordinates */

    for (k = 0; k < *nvar; ++k) {
	coord[k] = 0;
    }

    /* Find locations in tables */
    i = 1;
L60:
    j = *locy;
    for (k = 1; k <= n; ++k) {
	l = config[k];
	j += coord[l - 1] * size[k - 1];
    }
    y[j] += x[i];

    /* Update coordinates */

    ++i;
    for (k = 1; k <= *nvar; ++k) {
	++coord[k - 1];
	if (coord[k - 1] < dim[k]) {
	    goto L60;
	}
	coord[k - 1] = 0;
    }

    return;
}


/* Algorithm AS 51.2 Appl. Statist. (1972), vol. 21, p. 218

   Makes proportional adjustment corresponding to CONFIG.
   All parameters are assumed valid without test.
   */

void adjust(int *nvar, double *x, double *y, double *z, int *locz, int
	   *nx, int *ny, int *nz, int *dim, int *config, double *d)
{
    int i, j, k, l, n, *coord, *size;
    double e;

    size = lvector(*nvar + 1);
    coord = lvector(*nvar);

    /* Parameter adjustments */
    --config;
    --dim;
    --x;
    --y;
    --z;

    /* Set size array */

    size[0] = 1;
    for (k = 1; k <= *nvar; ++k) {
	l = config[k];
	if (l == 0) {
	    goto L20;
	}
	size[k] = size[k - 1] * dim[l];
    }

    /* Find number of variables in configuration */

    k = *nvar + 1;
L20:
    n = k - 1;

    /* Test size of deviation */

    l = size[k - 1];
    j = 1;
    k = *locz;
    for (i = 1; i <= l; ++i) {
	e = abs(z[k] - y[j]);
	if (e > *d) {
	    *d = e;
	}
	++j;
	++k;
    }

    /* Initialize coordinates */

    for (k = 0; k < *nvar; ++k) {
	coord[k] = 0;
    }
    i = 1;

    /* Perform adjustment */

L50:
    j = 0;
    for (k = 1; k <= n; ++k) {
	l = config[k];
	j += coord[l - 1] * size[k - 1];
    }
    k = j + *locz;
    ++j;

    /* Note that Y(J) should be non-negative */

    if (y[j] <= 0.) {
	x[i] = 0.;
    }
    if (y[j] > 0.) {
	x[i] = x[i] * z[k] / y[j];
    }

    /* Update coordinates */

    ++i;
    for (k = 1; k <= *nvar; ++k) {
	++coord[k - 1];
	if (coord[k - 1] < dim[k]) {
	    goto L50;
	}
	coord[k - 1] = 0;
    }

    return;
}

/* Auxiliary routine to get rid of limitations on the number of factors
   in the model. 

   Changed to use R_alloc to avoid memory leak if routine was
   interrupted.
*/

static int *lvector(int n) {
    return (int *) R_alloc(n, sizeof(int));
}
