#ifndef PORT_PORT_H
#define PORT_PORT_H

/* Header file for the C utilities to accompany the Fortran
 * optimization routines for the port library.
 *
 * Copyright (C) 2005-5  the R Development Core Team
 * Licensed under the GNU General Public License, version 2 or later.
 */

#include <Rinternals.h>
#include <R_ext/RS.h>
#include <Rmath.h>  /* for d1mach and i1mach */
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

/* PORT interface functions - reverse communication */

/* DRMNF(D, FX, IV, LIV, LV, N, V, X) */
extern void F77_NAME(drmnf)(double[], double*,
			    int[], int*, int*, int*, double[], double[]);

/* DRMNG(D, FX, G, IV, LIV, LV, N, V, X) */
extern void F77_NAME(drmng)(double[], double*, double[],
			    int[], int*, int*, int*, double[], double[]);

/* DRMNH(D, FX, G, H, IV, LH, LIV, LV, N, V, X) */
extern void F77_NAME(drmnh)(double[], double*, double[], double[],
			    int[], int*, int*, int*, int*, double[], double[]);

/* DRMNFB(B, D, FX, IV, LIV, LV, N, V, X) */
extern void F77_NAME(drmnfb)(double[], double[], double*,
			    int[], int*, int*, int*, double[], double[]);

/* DRMNGB(B, D, FX, G, IV, LIV, LV, N, V, X) */
extern void F77_NAME(drmngb)(double[], double[], double*, double[],
			     int[], int*, int*, int*, double[], double[]);

/* DRMNH(B, D, FX, G, H, IV, LH, LIV, LV, N, V, X) */
extern void F77_NAME(drmnhb)(double[], double[], double*, double[], double[],
			     int[], int*, int*, int*, int*, double[], double[]);

/* DRN2GB(B, D, DR, IV, LIV, LV, N, ND, N1, N2, P, R, RD, V, X) */
extern void F77_NAME(drn2gb)(double[], double[], double[],
			     int[], int*, int*, int*, int*, int*, int*, int*,
			     double[], double[], double[], double[]);
/* DRN2G(D, DR, IV, LIV, LV, N, ND, N1, N2, P, R, RD, V, X) */
extern void F77_NAME(drn2g)(double[], double[],
			    int[], int*, int*, int*, int*, int*, int*, int*,
			    double[], double[], double[], double[]);
/* DRNSGB(A, ALF, B, C, DA, IN, IV, L, L1, LA, LIV, LV, N, NDA, P, V, Y) */
extern void F77_NAME(drnsgb)(double[], double[], double[], double[], double[],
			     int[], int[], int*, int*, int*, int*,
			     int*, int*, int*, int*, int*,
			     double[], double[]);
/* DRNSG(A, ALF, C, DA, IN, IV, L, L1, LA, LIV, LV, N, NDA, P, V, Y) */
extern void F77_NAME(drnsg)(double[], double[], double[], double[],
			    int[], int[], int*, int*, int*, int*,
			    int*, int*, int*, int*, int*,
			    double[], double[]);

SEXP port_ivset(SEXP kind, SEXP iv, SEXP v);

SEXP port_nlminb(SEXP fn, SEXP gr, SEXP hs, SEXP rho,
		 SEXP lowerb, SEXP upperb, SEXP d, SEXP iv, SEXP v);

SEXP port_nlsb(SEXP m, SEXP d, SEXP gg, SEXP iv, SEXP v,
	       SEXP lowerb, SEXP upperb);

#endif
