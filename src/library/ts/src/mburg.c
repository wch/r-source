/*
 *  Copyright (C) 1999 Martyn Plummer
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

/* Burg's algorithm for autoregression estimation

   multi_burg  is the interface to R. It also handles model selection
               using AIC

   burg        implements the main part of the algorithm

   burg2       estimates the partial correlation coefficient. This
               requires iteration in the multivariate case.

   Notation

   resid	residuals (forward and backward)
   A		Estimates of autocorrelation coefficients
   V		Prediction Variance
   K		Partial correlation coefficient
*/

#include "R.h"
#include "carray.h"
#include "qr.h"

#define BURG_MAX_ITER 20
#define BURG_TOL      1.0E-8

void multi_burg(int *pn, double *x, int *pomax, int *pnser, double *coef,
    double *pacf, double *var, double *aic, int *porder, int *useaic,
    int *vmethod);
static void burg(int omax, Array resid_f, Array resid_b, Array *A, Array *B,
    Array P, Array V, int vmethod);
static void burg2(Array ss_ff, Array ss_bb, Array ss_fb, Array E,
    Array KA, Array KB);

void multi_burg(int *pn, double *x, int *pomax, int *pnser, double *coef,
	double *pacf, double *var, double *aic, int *porder, int *useaic,
        int *vmethod)
{
    int i, j, m, omax = *pomax, n = *pn, nser=*pnser, order=*porder;
    int dim1[3];
    double aicmin;
    Array xarr, resid_f, resid_b, resid_f_tmp;
    Array *A, *B, P, V;

    dim1[0] = omax+1; dim1[1] = dim1[2] = nser;
    A = (Array *) R_alloc(omax+1, sizeof(Array));
    B = (Array *) R_alloc(omax+1, sizeof(Array));
    for (i = 0; i <= omax; i++) {
	A[i] = make_zero_array(dim1, 3);
	B[i] = make_zero_array(dim1, 3);
    }
    P = make_array(pacf, dim1, 3); 
    V = make_array(var, dim1, 3); 

    xarr = make_matrix(x, nser, n);
    resid_f = make_zero_matrix(nser, n);
    resid_b = make_zero_matrix(nser, n);
    set_array_to_zero(resid_b);
    copy_array(xarr, resid_f);
    copy_array(xarr, resid_b);
    resid_f_tmp = make_zero_matrix(nser, n);

    burg(omax, resid_f, resid_b, A, B, P, V, *vmethod);

    /* Model order selection */

    for (i = 0; i <= omax; i++) {
	aic[i] = n * ldet(subarray(V,i)) + 2 * i * nser * nser;
    }
    if (*useaic) {
        order = 0;
        aicmin = aic[0];
        for (i = 1; i <= omax; i++) {
	    if (aic[i] < aicmin) {
		aicmin = aic[i];
		order = i;
	    }
        }
    }
    else order = omax;
    *porder = order;

    for(i = 0; i < vector_length(A[order]); i++)
	coef[i] = VECTOR(A[order])[i];

    if (*useaic) {
	/* Recalculate residuals for chosen model */
	set_array_to_zero(resid_f);
	set_array_to_zero(resid_f_tmp);
	for (m = 0; m <= order; m++) {
	    for (i = 0; i < NROW(resid_f_tmp); i++) {
		for (j = 0; j < NCOL(resid_f_tmp) - order; j++) {
		    MATRIX(resid_f_tmp)[i][j + order] = MATRIX(xarr)[i][j + order - m];
		}
	    }
	    matrix_prod(subarray(A[order],m), resid_f_tmp, 0, 0, resid_f_tmp);
	    array_op(resid_f_tmp, resid_f, '+', resid_f);
	}
    }
    copy_array(resid_f, xarr);
     
}


static void burg(int omax, Array resid_f, Array resid_b, Array *A, Array *B,
    Array P, Array V, int vmethod)
{
    int i, j, m, n = NCOL(resid_f), nser=NROW(resid_f);
    Array ss_ff, ss_bb, ss_fb;
    Array resid_f_tmp, resid_b_tmp;
    Array KA, KB, E;
    Array id, tmp;

    ss_ff = make_zero_matrix(nser, nser);
    ss_fb = make_zero_matrix(nser, nser);
    ss_bb = make_zero_matrix(nser, nser);

    resid_f_tmp = make_zero_matrix(nser, n);
    resid_b_tmp = make_zero_matrix(nser, n);

    id    = make_identity_matrix(nser);

    tmp   = make_zero_matrix(nser, nser);

    E = make_zero_matrix(nser, nser);
    KA = make_zero_matrix(nser, nser);
    KB = make_zero_matrix(nser, nser);

    set_array_to_zero(A[0]);
    set_array_to_zero(B[0]);
    copy_array(id, subarray(A[0],0));
    copy_array(id, subarray(B[0],0));

    matrix_prod(resid_f, resid_f, 0, 1, E);
    scalar_op(E, n, '/',  E);
    copy_array(E, subarray(V,0));

    for (m = 0; m < omax; m++) {

        for(i = 0; i < nser; i++) {
            for (j = n - 1; j > m; j--) {
                MATRIX(resid_b)[i][j] = MATRIX(resid_b)[i][j-1];
            }
            MATRIX(resid_f)[i][m] = 0.0;
            MATRIX(resid_b)[i][m] = 0.0;
        }
        matrix_prod(resid_f, resid_f, 0, 1, ss_ff);
        matrix_prod(resid_b, resid_b, 0, 1, ss_bb);
        matrix_prod(resid_f, resid_b, 0, 1, ss_fb);
    
        burg2(ss_ff, ss_bb, ss_fb, E, KA, KB);		/* Update K */

        for (i = 0; i <= m + 1; i++) {

            matrix_prod(KA, subarray(B[m], m + 1 - i), 0, 0, tmp); 
            array_op(subarray(A[m], i), tmp, '-', subarray(A[m+1], i));

            matrix_prod(KB, subarray(A[m], m + 1 - i), 0, 0, tmp); 
            array_op(subarray(B[m], i), tmp, '-', subarray(B[m+1], i));

        }
       
        matrix_prod(KA, resid_b, 0, 0, resid_f_tmp);
        matrix_prod(KB, resid_f, 0, 0, resid_b_tmp);
        array_op(resid_f, resid_f_tmp, '-', resid_f);
        array_op(resid_b, resid_b_tmp, '-', resid_b);

        if (vmethod == 1) {
	    matrix_prod(KA, KB, 0, 0, tmp);
	    array_op(id, tmp, '-', tmp);
	    matrix_prod(tmp, E, 0, 0, E);
        }
        else if (vmethod == 2) {
	    matrix_prod(resid_f, resid_f, 0, 1, E);
	    matrix_prod(resid_b, resid_b, 0, 1, tmp);
	    array_op(E, tmp, '+', E);
	    scalar_op(E, 2.0*(n - m - 1), '/', E);
        }
        else error("Invalid vmethod");

        copy_array(E, subarray(V,m+1));
        copy_array(KA, subarray(P,m+1));
    }
}


static void burg2(Array ss_ff, Array ss_bb, Array ss_fb, Array E,
   Array KA, Array KB)
/* 
   Estimate partial correlation by minimizing (1/2)*log(det(s)) where
   "s" is the the sum of the forward and backward prediction errors.

   In the multivariate case, the forward (KA) and backward (KB) partial
   correlation coefficients are related by

      KA = solve(E) %*% t(KB) %*% E

   where E is the prediction variance.

*/
{
    int i, j, k, l, nser = NROW(ss_ff);
    int iter;
    Array ss_bf;
    Array s, s_inv, tmp, d1;
    Array D1, D2, THETA, THETAOLD, THETADIFF, TMP;
    Array obj;
    Array id;
    Array e, f, g, h, sg, sh;
    Array theta;

    ss_bf = make_zero_matrix(nser,nser);
    transpose_matrix(ss_fb, ss_bf);
    s = make_zero_matrix(nser, nser);
    s_inv = make_zero_matrix(nser, nser);
    tmp = make_zero_matrix(nser, nser);
    d1 = make_zero_matrix(nser, nser);

    e = make_zero_matrix(nser, nser);
    f = make_zero_matrix(nser, nser);
    g = make_zero_matrix(nser, nser);
    h = make_zero_matrix(nser, nser);
    sg = make_zero_matrix(nser, nser);
    sh = make_zero_matrix(nser, nser);

    theta = make_zero_matrix(nser, nser);

    D1 = make_zero_matrix(nser*nser, 1);
    D2 = make_zero_matrix(nser*nser, nser*nser);
    THETA = make_zero_matrix(nser*nser, 1);	/* theta in vector form */
    THETAOLD = make_zero_matrix(nser*nser, 1);
    THETADIFF = make_zero_matrix(nser*nser, 1);
    TMP = make_zero_matrix(nser*nser, 1);

    obj = make_zero_matrix(1,1);
    id = make_identity_matrix(nser);

    /* utility matrices e,f,g,h */
    qr_solve(E, ss_bf, e);
    qr_solve(E, ss_fb, f);
    qr_solve(E, ss_bb, tmp);
    transpose_matrix(tmp, tmp);
    qr_solve(E, tmp, g);
    qr_solve(E, ss_ff, tmp);
    transpose_matrix(tmp, tmp);
    qr_solve(E, tmp, h);

    for(iter = 0; iter < BURG_MAX_ITER; iter++)
    {
        /* Forward and backward partial correlation coefficients */
        transpose_matrix(theta, tmp);
        qr_solve(E, tmp, tmp);
        transpose_matrix(tmp, KA);

        qr_solve(E, theta, tmp);
        transpose_matrix(tmp, KB);
 
        /* Sum of forward and backward prediction errors ... */
        set_array_to_zero(s);
 
        /* Forward */
        array_op(s, ss_ff, '+', s);
        matrix_prod(KA, ss_bf, 0, 0, tmp);
        array_op(s, tmp, '-', s);
        transpose_matrix(tmp, tmp);
        array_op(s, tmp, '-', s);
        matrix_prod(ss_bb, KA, 0, 1, tmp);
        matrix_prod(KA, tmp, 0, 0, tmp);
        array_op(s, tmp, '+', s);
   
        /* Backward */
        array_op(s, ss_bb, '+', s);
        matrix_prod(KB, ss_fb, 0, 0, tmp);
        array_op(s, tmp, '-', s);
        transpose_matrix(tmp, tmp);	
        array_op(s, tmp, '-', s);
        matrix_prod(ss_ff, KB, 0, 1, tmp);
        matrix_prod(KB, tmp, 0, 0, tmp);
        array_op(s, tmp, '+', s);
        
        matrix_prod(s, f, 0, 0, d1);
        matrix_prod(e, s, 1, 0, tmp);
        array_op(d1, tmp, '+', d1);

        /*matrix_prod(g,s,0,0,sg);*/
        matrix_prod(s,g,0,0,sg);
        matrix_prod(s,h,0,0,sh);
 
        for (i = 0; i < nser; i++) {
            for (j = 0; j < nser; j++) {
		MATRIX(D1)[nser*i+j][0] = MATRIX(d1)[i][j];
                for (k = 0; k < nser; k++)
                    for (l = 0; l < nser; l++) {
                        MATRIX(D2)[nser*i+j][nser*k+l] = 
			    (i == k) * MATRIX(sg)[j][l] +
			    MATRIX(sh)[i][k] * (j == l); 
                    }
            }
        }

        copy_array(THETA, THETAOLD);
        qr_solve(D2, D1, THETA);

        for (i = 0; i < vector_length(theta); i++)
            VECTOR(theta)[i] = VECTOR(THETA)[i];

        matrix_prod(D2, THETA, 0, 0, TMP);

        array_op(THETAOLD, THETA, '-', THETADIFF);
        matrix_prod(D2, THETADIFF, 0, 0, TMP);
        matrix_prod(THETADIFF, TMP, 1, 0, obj);
        if (VECTOR(obj)[0] < BURG_TOL)
            break;

    }

    if (iter == BURG_MAX_ITER)
        error("Burg's algorithm failed to find partial correlation");
}
