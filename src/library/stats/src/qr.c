/*
 *  Copyright (C) 1999        Martyn Plummer
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
 */

#include <math.h>
#include <R.h>			/* vmaxget(), vmaxset() error */
#include <R_ext/Applic.h>	/* Fortran routines */
#include "carray.h"

#include "qr.h"

static void assert(int bool)
{
    if(!bool) {
	error("assert failed in src/library/ts/src/carray.c");
    }
}

void qr_solve(Array x, Array y, Array coef)
/* Translation of the R function qr.solve into pure C
   NB We have to transpose the matrices since the ordering of an array is different in Fortran
   NB2 We have to copy x to avoid it being overwritten.
*/
{
    int i, info = 0, rank, *pivot, n, p;
    char *vmax;
    double tol = 1.0E-7, *qraux, *work;
    Array xt, yt, coeft;

    assert(NROW(x) == NROW(y));
    assert(NCOL(coef) == NCOL(y));
    assert(NCOL(x) == NROW(coef));

    vmax = vmaxget();

    qraux = (double *) R_alloc(NCOL(x), sizeof(double));
    pivot = (int *) R_alloc(NCOL(x), sizeof(int));
    work  = (double *) R_alloc(2*NCOL(x), sizeof(double));

    for(i = 0; i < NCOL(x); i++)
        pivot[i] = i+1;

    xt = make_zero_matrix(NCOL(x), NROW(x));
    transpose_matrix(x,xt);

    n = NROW(x);
    p = NCOL(x);

    F77_CALL(dqrdc2)(VECTOR(xt), &n, &n, &p, &tol, &rank,
                       qraux, pivot, work);

    if (rank != p)
        error("Singular matrix in qr_solve\n");

    yt = make_zero_matrix(NCOL(y), NROW(y));
    coeft = make_zero_matrix(NCOL(coef), NROW(coef));
    transpose_matrix(y, yt);

    F77_CALL(dqrcf)(VECTOR(xt), &NROW(x), &rank, qraux,
        yt.vec, &NCOL(y), coeft.vec, &info);

    transpose_matrix(coeft,coef);

    vmaxset(vmax);
}

double ldet(Array x)
/* Log determinant of square matrix */
{
    int i, rank, *pivot, n, p;
    char *vmax;
    double ll, tol = 1.0E-7, *qraux, *work;
    Array xtmp;

    assert(DIM_LENGTH(x) == 2); /* is x a matrix? */
    assert(NROW(x) == NCOL(x)); /* is x square? */

    vmax = vmaxget();

    qraux = (double *) R_alloc(NCOL(x), sizeof(double));
    pivot = (int *) R_alloc(NCOL(x), sizeof(int));
    work  = (double *) R_alloc(2*NCOL(x), sizeof(double));

    xtmp = make_zero_matrix(NROW(x), NCOL(x));
    copy_array(x, xtmp);

    for(i = 0; i < NCOL(x); i++)
        pivot[i] = i+1;

    p = n = NROW(x);

    F77_CALL(dqrdc2)(VECTOR(xtmp), &n, &n, &p, &tol, &rank,
                       qraux, pivot, work);

    if (rank != p)
        error("Singular matrix in ldet\n");

    for (i = 0, ll=0.0; i < rank; i++) {
         ll += log(fabs(MATRIX(xtmp)[i][i]));
    }

    vmaxset(vmax);

    return ll;
}
