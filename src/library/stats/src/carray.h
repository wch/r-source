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

Array make_array(double vec[], int dim[], int ndim);
Array make_zero_array(int dim[], int ndim);
Array make_matrix(double vec[], int nrow, int ncol);
Array make_zero_matrix(int nrow, int ncol);
Array make_identity_matrix(int n);

Array subarray(Array a, int index);

int vector_length(Array a);

void set_array_to_zero(Array arr);
void copy_array (Array orig, Array ans);
void array_op(Array arr1, Array arr2, char op, Array ans);
void scalar_op(Array arr, double s, char op, Array ans);

void transpose_matrix(Array mat, Array ans);
void matrix_prod(Array mat1, Array mat2, int trans1, int trans2, Array ans);
