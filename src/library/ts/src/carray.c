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

/* Functions for dynamically allocating arrays

   The Array structure contains pointers to arrays which are allocated
   using the R_alloc function.  Although the .C() interface cleans up
   all memory assigned with R_alloc, judicious use of getvmax() setvmax()
   to free this memory is probably wise. See memory.c in R core.

*/

#include "carray.h"
#include "Error.h"
#include <stdio.h>

#define TRUE 1
#define FALSE 0

static void assert(int bool)
{
    if(!bool) {
	error("assert failed in src/library/ts/src/carray.c");
    }
}


static Array init_array()
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

long int vector_length(Array a) 
{
    int i;
    long len;

    for (i = 0, len = 1; i < DIM_LENGTH(a); i++) {
        len *= DIM(a)[i];
    }

    return len;
}


Array make_array(double vec[], int dim[], int ndim)
{
    int d, i, j;
    long len[MAX_DIM_LENGTH + 1];
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

Array make_zero_array(int dim[], int ndim)
{
    int i;
    long len;
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

Array make_matrix(double vec[], int nrow, int ncol)
{
   int dim[2];

   dim[0] = nrow;
   dim[1] = ncol;
   return make_array(vec, dim, 2);
}

Array make_zero_matrix(nrow, ncol)
{
   int dim[2];
   Array a;

   dim[0] = nrow;
   dim[1] = ncol;
   a = make_zero_array(dim, 2);
   return a;
}

Array subarray(Array a, int index)
/* Return subarray of array a in the form of an Array
   structure so it can be manipulated by other functions
   NB The data are not copied, so any changes made to the
      subarray will affect the original array.
*/
{
    int i;
    long offset;
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

int test_array_conform(Array a1, Array a2)
{
   int i, ans = FALSE;

   if (DIM_LENGTH(a1) != DIM_LENGTH(a2)) {
      ans = FALSE;
   }
   else {
      for (i = 0; i < DIM_LENGTH(a1); i++) {
         if (DIM(a1)[i] == DIM(a2)[i]) {
            ans = TRUE;
         } else {
            ans = FALSE;
            break;
         }
      }
   }

   return ans;
}

void copy_array (Array orig, Array ans)
/* copy matrix orig to ans */
{
    int i;

    assert (test_array_conform(orig, ans));

    for(i = 0; i < vector_length(orig); i++)
        VECTOR(ans)[i] = VECTOR(orig)[i];
}

void transpose_matrix(Array mat, Array ans)
{
    int i,j;
    char *vmax;
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

void array_op(Array arr1, Array arr2, char op, Array ans)
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


void scalar_op(Array arr, double s, char op, Array ans)
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

void matrix_prod(Array mat1, Array mat2, int trans1, int trans2, Array ans)
/*
    General matrix product between mat1 and mat2. Put answer in ans.
    trans1 and trans2 are logical flags which indicate if the matrix is
    to be transposed. Normal matrix multiplication has trans1 = trans2 = 0.
*/
{
    int i,j,k,K1,K2;
    char *vmax;
    double m1, m2;
    Array tmp;

    /* Test whether everything is a matrix */
    assert( DIM_LENGTH(mat1) == 2 && DIM_LENGTH(mat2) == 2 && DIM_LENGTH(ans) == 2);

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

void set_array_to_zero(Array arr)
{
	int i;

	for (i = 0; i < vector_length(arr); i++)
		VECTOR(arr)[i] = 0.0;
}

Array make_identity_matrix(int n)
{
	int i;
	Array a;

	a = make_zero_matrix(n,n);
	for(i = 0; i < n; i++)
		MATRIX(a)[i][i] = 1.0;

        return a;
}
