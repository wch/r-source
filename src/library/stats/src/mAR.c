/*
 *  Copyright (C) 1999 Martyn Plummer
 *  Copyright (C) 1999-2016 The R Core Team
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
 *  https://www.R-project.org/Licenses/.
 */

#include <math.h>
#include <string.h>
#include <R.h>
#include <R_ext/Applic.h>	/* Fortran routines */
#include "ts.h"
#include "stats.h"


#define MAX_DIM_LENGTH 4

#define VECTOR(x) (x.vec)
#define MATRIX(x) (x.mat)
#define ARRAY1(x) (x.vec)
#define ARRAY2(x) (x.mat)
#define ARRAY3(x) (x.arr3)
#define ARRAY4(x) (x.arr4)
#define DIM(x)    (x.dim)
#define NROW(x)   (x.dim[0])
#define NCOL(x)   (x.dim[1])
#define DIM_LENGTH(x) (x.ndim)


typedef struct array {
    double *vec;
    double **mat;
    double ***arr3;
    double ****arr4;
    int dim[MAX_DIM_LENGTH];
    int ndim;
} Array;

static Array make_array(double vec[], int dim[], int ndim);
static Array make_zero_array(int dim[], int ndim);
static Array make_matrix(double vec[], int nrow, int ncol);
static Array make_zero_matrix(int nrow, int ncol);
static Array make_identity_matrix(int n);

static Array subarray(Array a, int index);

static int vector_length(Array a);

static void set_array_to_zero(Array arr);
static void copy_array (Array orig, Array ans);
static void array_op(Array arr1, Array arr2, char op, Array ans);
static void scalar_op(Array arr, double s, char op, Array ans);

static void transpose_matrix(Array mat, Array ans);
static void matrix_prod(Array mat1, Array mat2, int trans1, int trans2,
			Array ans);


/* Functions for dynamically allocating arrays

   The Array structure contains pointers to arrays which are allocated
   using the R_alloc function.	Although the .C() interface cleans up
   all memory assigned with R_alloc, judicious use of vmaxget() vmaxset()
   to free this memory is probably wise. See memory.c in R core.

*/

static void assert(int bool)
{
    if(!bool)
	error(("assert failed in src/library/ts/src/carray.c"));
}

static Array init_array(void)
{
    int i;
    Array a;

    /* Initialize everything to zero.  Useful for debugging */
    ARRAY1(a) = (double *) '\0';
    ARRAY2(a) = (double **) '\0';
    ARRAY3(a) = (double ***) '\0';
    ARRAY4(a) = (double ****) '\0';
    for (i = 0; i < MAX_DIM_LENGTH; i++)
	DIM(a)[i] = 0;
    DIM_LENGTH(a) = 0;

    return a;
}

static int vector_length(Array a)
{
    int i, len;

    for (i = 0, len = 1; i < DIM_LENGTH(a); i++) {
	len *= DIM(a)[i];
    }

    return len;
}


static Array make_array(double vec[], int dim[], int ndim)
{
    int d, i, j;
    int len[MAX_DIM_LENGTH + 1];
    Array a;

    assert(ndim <= MAX_DIM_LENGTH);

    a = init_array();

    len[ndim] = 1;
    for (d = ndim; d >= 1; d--) {
	len[d-1] = len[d] * dim[ndim - d];
    }

    for (d = 1; d <= ndim; d++) {
       switch(d) {
	   case 1:
	       VECTOR(a) = vec;
	       break;
	   case 2:
	       ARRAY2(a) = (double**) R_alloc(len[2 - 1],sizeof(double*));
	       for(i = 0, j = 0; i < len[2 - 1]; i++, j+=dim[ndim - 2 + 1]) {
		  ARRAY2(a)[i] = ARRAY1(a) + j;
	       }
	       break;
	   case 3:
	       ARRAY3(a) = (double***) R_alloc(len[3 - 1],sizeof(double**));
	       for(i = 0, j = 0; i < len[3 - 1]; i++, j+=dim[ndim - 3 + 1]) {
		  ARRAY3(a)[i] = ARRAY2(a) + j;
	       }
	       break;
	   case 4:
	       ARRAY4(a) = (double****) R_alloc(len[4 - 1],sizeof(double***));
	       for(i = 0, j = 0; i < len[4 - 1]; i++, j+=dim[ndim - 4 + 1]) {
		  ARRAY4(a)[i] = ARRAY3(a) + j;
	       }
	       break;
	   default:
	       break;
       }
    }

    for (i = 0; i < ndim; i++) {
       DIM(a)[i] = dim[i];
    }
    DIM_LENGTH(a) = ndim;

    return a;
}

static Array make_zero_array(int dim[], int ndim)
{
    int i;
    int len;
    double *vec;

    for (i = 0, len = 1; i < ndim; i++) {
	len *= dim[i];
    }

    vec = (double *) R_alloc(len, sizeof(double));
    for (i = 0; i < len; i++) {
	vec[i] = 0.0;
    }

    return make_array(vec, dim, ndim);

}

static Array make_matrix(double vec[], int nrow, int ncol)
{
   int dim[2];

   dim[0] = nrow;
   dim[1] = ncol;
   return make_array(vec, dim, 2);
}

static Array make_zero_matrix(int nrow, int ncol)
{
   int dim[2];
   Array a;

   dim[0] = nrow;
   dim[1] = ncol;
   a = make_zero_array(dim, 2);
   return a;
}

static Array subarray(Array a, int index)
/* Return subarray of array a in the form of an Array
   structure so it can be manipulated by other functions
   NB The data are not copied, so any changes made to the
      subarray will affect the original array.
*/
{
    int i, offset;
    Array b;

    b = init_array();

    /* is index in range? */
    assert( index >= 0 && index < DIM(a)[0] );

    offset = index;
    switch(DIM_LENGTH(a)) {
    /* NB Falling through here */
	case 4:
	    offset *= DIM(a)[DIM_LENGTH(a) - 4 + 1];
	    ARRAY3(b) = ARRAY3(a) + offset;
	case 3:
	    offset *= DIM(a)[DIM_LENGTH(a) - 3 + 1];
	    ARRAY2(b) = ARRAY2(a) + offset;
	case 2:
	    offset *= DIM(a)[DIM_LENGTH(a) - 2 + 1];
	    ARRAY1(b) = ARRAY1(a) + offset;
	    break;
	default:
	    break;
    }


    DIM_LENGTH(b) = DIM_LENGTH(a) - 1;

    for (i = 0; i < DIM_LENGTH(b); i++)
	DIM(b)[i] = DIM(a)[i+1];

    return b;

}

static int test_array_conform(Array a1, Array a2)
{
   int i, ans = FALSE;

   if (DIM_LENGTH(a1) != DIM_LENGTH(a2))
      return FALSE;
   else
      for (i = 0; i < DIM_LENGTH(a1); i++) {
	 if (DIM(a1)[i] == DIM(a2)[i])
	    ans = TRUE;
	 else
	    return FALSE;
      }

   return ans;
}

static void copy_array (Array orig, Array ans)
/* copy matrix orig to ans */
{
    int i;

    assert (test_array_conform(orig, ans));

    for(i = 0; i < vector_length(orig); i++)
	VECTOR(ans)[i] = VECTOR(orig)[i];
}

static void transpose_matrix(Array mat, Array ans)
{
    int i,j;
    const void *vmax;
    Array tmp;

    tmp = init_array();

    assert(DIM_LENGTH(mat) == 2 && DIM_LENGTH(ans) == 2);
    assert(NCOL(mat) == NROW(ans));
    assert(NROW(mat) == NCOL(ans));

    vmax = vmaxget();

    tmp = make_zero_matrix(NROW(ans), NCOL(ans));
    for(i = 0; i < NROW(mat); i++)
	for(j = 0; j < NCOL(mat); j++)
	   MATRIX(tmp)[j][i] = MATRIX(mat)[i][j];
    copy_array(tmp, ans);

    vmaxset(vmax);
}

static void array_op(Array arr1, Array arr2, char op, Array ans)
/* Element-wise array operations */
{
    int i;

    assert (test_array_conform(arr1, arr2));
    assert (test_array_conform(arr2, ans));

    switch (op) {
	case '*':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr1)[i] * VECTOR(arr2)[i];
	    break;
	case '+':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr1)[i] + VECTOR(arr2)[i];
	    break;
	case '/':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr1)[i] / VECTOR(arr2)[i];
	    break;
	case '-':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr1)[i] - VECTOR(arr2)[i];
	    break;
	default:
	    printf("Unknown op in array_op");
    }
}


static void scalar_op(Array arr, double s, char op, Array ans)
/* Elementwise scalar operations */
{
    int i;

    assert (test_array_conform(arr, ans));

    switch (op) {
	case '*':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr)[i] * s;
	    break;
	case '+':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr)[i] + s;
	    break;
	case '/':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr)[i] / s;
	    break;
	case '-':
	    for (i = 0; i < vector_length(ans); i++)
		VECTOR(ans)[i] = VECTOR(arr)[i] - s;
	    break;
	default:
	    printf("Unknown op in array_op");
    }
}

static void matrix_prod(Array mat1, Array mat2, int trans1, int trans2, Array ans)
/*
    General matrix product between mat1 and mat2. Put answer in ans.
    trans1 and trans2 are logical flags which indicate if the matrix is
    to be transposed. Normal matrix multiplication has trans1 = trans2 = 0.
*/
{
    int i,j,k,K1,K2;
    const void *vmax;
    double m1, m2;
    Array tmp;

    /* Test whether everything is a matrix */
    assert(DIM_LENGTH(mat1) == 2 &&
	   DIM_LENGTH(mat2) == 2 && DIM_LENGTH(ans) == 2);

    /* Test whether matrices conform. K is the dimension that is
       lost by multiplication */
    if (trans1) {
	assert ( NCOL(mat1) == NROW(ans) );
	K1 = NROW(mat1);
    }
    else {
	assert ( NROW(mat1) == NROW(ans) );
	K1 = NCOL(mat1);
    }
    if (trans2) {
	assert ( NROW(mat2) == NCOL(ans) );
	K2 = NCOL(mat2);
    }
    else {
	assert ( NCOL(mat2) == NCOL(ans) );
	K2 = NROW(mat2);
    }
    assert (K1 == K2);

    tmp = init_array();

    /* In case ans is the same as mat1 or mat2, we create a temporary
       matrix to hold the answer, then copy it to ans
    */
    vmax = vmaxget();

    tmp = make_zero_matrix(NROW(ans), NCOL(ans));
    for (i = 0; i < NROW(tmp); i++) {
	for (j = 0; j < NCOL(tmp); j++) {
	    for(k = 0; k < K1; k++) {
		    m1 = (trans1) ? MATRIX(mat1)[k][i] : MATRIX(mat1)[i][k];
		    m2 = (trans2) ? MATRIX(mat2)[j][k] : MATRIX(mat2)[k][j];
		    MATRIX(tmp)[i][j] += m1 * m2;
	    }
	}
    }
    copy_array(tmp, ans);

    vmaxset(vmax);
}

static void set_array_to_zero(Array arr)
{
    int i;

    for (i = 0; i < vector_length(arr); i++)
	VECTOR(arr)[i] = 0.0;
}

static Array make_identity_matrix(int n)
{
    int i;
    Array a;

    a = make_zero_matrix(n,n);
    for(i = 0; i < n; i++)
	MATRIX(a)[i][i] = 1.0;

    return a;
}

static void qr_solve(Array x, Array y, Array coef)
/* Translation of the R function qr.solve into pure C
   NB We have to transpose the matrices since the ordering of an array is different in Fortran
   NB2 We have to copy x to avoid it being overwritten.
*/
{
    int i, info = 0, rank, *pivot, n, p;
    const void *vmax;
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
	error(_("Singular matrix in qr_solve"));

    yt = make_zero_matrix(NCOL(y), NROW(y));
    coeft = make_zero_matrix(NCOL(coef), NROW(coef));
    transpose_matrix(y, yt);

    F77_CALL(dqrcf)(VECTOR(xt), &NROW(x), &rank, qraux,
	yt.vec, &NCOL(y), coeft.vec, &info);

    transpose_matrix(coeft,coef);

    vmaxset(vmax);
}

static double ldet(Array x)
/* Log determinant of square matrix */
{
    int i, rank, *pivot, n, p;
    const void *vmax;
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
	error(_("Singular matrix in ldet"));

    for (i = 0, ll=0.0; i < rank; i++) {
	 ll += log(fabs(MATRIX(xtmp)[i][i]));
    }

    vmaxset(vmax);

    return ll;
}



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


#define BURG_MAX_ITER 20
#define BURG_TOL      1.0E-8

void multi_burg(int *pn, double *x, int *pomax, int *pnser, double *coef,
    double *pacf, double *var, double *aic, int *porder, int *useaic,
    int *vmethod);
static void burg0(int omax, Array resid_f, Array resid_b, Array *A, Array *B,
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

    burg0(omax, resid_f, resid_b, A, B, P, V, *vmethod);

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


static void burg0(int omax, Array resid_f, Array resid_b, Array *A, Array *B,
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
	else error(_("Invalid vmethod"));

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
    Array s, tmp, d1;
    Array D1, D2, THETA, THETAOLD, THETADIFF, TMP;
    Array obj;
    Array e, f, g, h, sg, sh;
    Array theta;

    ss_bf = make_zero_matrix(nser,nser);
    transpose_matrix(ss_fb, ss_bf);
    s = make_zero_matrix(nser, nser);
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
	error(_("Burg's algorithm failed to find partial correlation"));
}

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

void multi_yw(double *acf, int *pn, int *pomax, int *pnser, double *coef,
	      double *pacf, double *var, double *aic, int *porder,
	      int *puseaic);
static void whittle(Array acf, int nlag, Array *A, Array *B, Array p_forward,
		    Array v_forward, Array p_back, Array v_back);
static void whittle2 (Array acf, Array Aold, Array Bold, int lag,
		      char *direction, Array A, Array K, Array E);


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
    const void *vmax;
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

static void whittle2 (Array acf, Array Aold, Array Bold, int lag,
		      char *direction, Array A, Array K, Array E)
{

    int d, i, nser=DIM(acf)[1];
    const void *vmax;
    Array beta, tmp, id;

    d = strcmp(direction, "forward") == 0;

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
