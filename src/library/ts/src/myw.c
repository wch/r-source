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

/* Whittle's algorithm for autoregression estimation

   multi_yw  is the interface to R. It also handles model selection using AIC

   whittle,whittle2     implement Whittle's recursion for solving the multivariate
                        Yule-Walker equations.

   Notation

   resid        residuals (forward and backward)
   A            Estimates of forward autocorrelation coefficients
   B            Estimates of backward autocorrelation coefficients
   EA,EB        Prediction Variance
   KA,KB        Partial correlation coefficient
*/

#include <R.h>
#include "ts.h"
#include "carray.h"
#include "qr.h"

void multi_yw(double *acf, int *pn, int *pomax, int *pnser, double *coef,
   double *pacf, double *var, double *aic, int *porder, int *puseaic);
static void whittle(Array acf, int nlag, Array *A, Array *B, Array p_forward,
   Array v_forward, Array p_back, Array v_back);
static void whittle2 (Array acf, Array Aold, Array Bold, int lag, char *direction,
    Array A, Array K, Array E);


void multi_yw(double *acf, int *pn, int *pomax, int *pnser, double *coef,
   double *pacf, double *var, double *aic, int *porder, int *useaic)
{
    int i, m;
    int  omax = *pomax, n = *pn, nser=*pnser, order=*porder;
    double aicmin;
    Array acf_array, p_forward, p_back, v_forward, v_back;
    Array *A, *B;
    int dim[3];

    dim[0] = omax+1; dim[1] = dim[2] = nser;
    acf_array = make_array(acf, dim, 3);
    p_forward = make_array(pacf, dim, 3);
    v_forward = make_array(var, dim, 3);

    /* Backward equations (discarded) */
    p_back= make_zero_array(dim, 3);
    v_back= make_zero_array(dim, 3);

    A = (Array *) R_alloc(omax+2, sizeof(Array));
    B = (Array *) R_alloc(omax+2, sizeof(Array));
    for (i = 0; i <= omax; i++) {
	A[i] = make_zero_array(dim, 3);
	B[i] = make_zero_array(dim, 3);
    }
    whittle(acf_array, omax, A, B, p_forward, v_forward, p_back, v_back);

    /* Model order selection */

    for (m = 0; m <= omax; m++) {
	aic[m] = n * ldet(subarray(v_forward,m)) + 2 * m * nser * nser;
    }
    if (*useaic) {
        order = 0;
        aicmin = aic[0];
        for (m = 0; m <= omax; m++) {
	    if (aic[m] < aicmin) {
		aicmin = aic[m];
		order = m;
	    }
        }
    }
    else order = omax;
    *porder = order;

    for(i = 0; i < vector_length(A[order]); i++)
	coef[i] = VECTOR(A[order])[i];
}

static void whittle(Array acf, int nlag, Array *A, Array *B, Array p_forward,
    Array v_forward, Array p_back, Array v_back)
{

    int lag, nser = DIM(acf)[1];
    char *vmax;
    Array EA, EB;	/* prediction variance */
    Array KA, KB;	/* partial correlation coefficient */
    Array id, tmp;
   
    vmax = vmaxget();

    KA = make_zero_matrix(nser, nser);
    EA = make_zero_matrix(nser, nser);

    KB = make_zero_matrix(nser, nser);
    EB = make_zero_matrix(nser, nser);

    id = make_identity_matrix(nser);

    copy_array(id, subarray(A[0],0));
    copy_array(id, subarray(B[0],0));
    copy_array(id, subarray(p_forward,0));
    copy_array(id, subarray(p_back,0));

    for (lag = 1; lag <= nlag; lag++) {

        whittle2(acf, A[lag-1], B[lag-1], lag, "forward", A[lag], KA, EB);
        whittle2(acf, B[lag-1], A[lag-1], lag, "back", B[lag], KB, EA);
        
        copy_array(EA, subarray(v_forward,lag-1));
        copy_array(EB, subarray(v_back,lag-1));

        copy_array(KA, subarray(p_forward,lag));
        copy_array(KB, subarray(p_back,lag));
        
    }

    tmp = make_zero_matrix(nser,nser);

    matrix_prod(KB,KA, 1, 1, tmp);
    array_op(id, tmp, '-', tmp);
    matrix_prod(EA, tmp, 0, 0, subarray(v_forward, nlag));

    vmaxset(vmax);

}

static void whittle2 (Array acf, Array Aold, Array Bold, int lag, char *direction,
    Array A, Array K, Array E)
{

    int d, i, nser=DIM(acf)[1];
    char *vmax;
    Array beta, tmp, id;

    d = direction == "forward";

    vmax = vmaxget();

    beta = make_zero_matrix(nser,nser);
    tmp = make_zero_matrix(nser, nser);
    id = make_identity_matrix(nser);

    set_array_to_zero(E);
    copy_array(id, subarray(A,0));

    for(i = 0; i < lag; i++) {
       matrix_prod(subarray(acf,lag - i), subarray(Aold,i), d, 1, tmp);
       array_op(beta, tmp, '+', beta);
       matrix_prod(subarray(acf,i), subarray(Bold,i), d, 1, tmp);
       array_op(E, tmp, '+', E);
    }
    qr_solve(E, beta, K);       
    transpose_matrix(K,K); 
    for (i = 1; i <= lag; i++) {
        matrix_prod(K, subarray(Bold,lag - i), 0, 0, tmp);
        array_op(subarray(Aold,i), tmp, '-', subarray(A,i));
    }

    vmaxset(vmax);
}
