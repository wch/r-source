#ifndef R_LAPACK_MODULE_H
#define R_LAPACK_MODULE_H

#include <Rinternals.h>

typedef SEXP (*Rf_La_svd)(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, 
			  SEXP v, SEXP method);
typedef SEXP (*Rf_La_rs)(SEXP x, SEXP only_values, SEXP method);
typedef SEXP (*Rf_La_rg)(SEXP x, SEXP only_values);
typedef SEXP (*Rf_La_zgesv)(SEXP A, SEXP B);
typedef SEXP (*Rf_La_zgeqp3)(SEXP A);
typedef SEXP (*Rf_qr_coef_cmplx)(SEXP Q, SEXP B);
typedef SEXP (*Rf_qr_qy_cmplx)(SEXP Q, SEXP B, SEXP trans);
typedef SEXP (*Rf_La_svd_cmplx)(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v);
typedef SEXP (*Rf_La_rs_complex)(SEXP x, SEXP only_values);
typedef SEXP (*Rf_La_rg_complex)(SEXP x, SEXP only_values);
typedef SEXP (*Rf_La_chol)(SEXP A);
typedef SEXP (*Rf_La_chol2inv)(SEXP x, SEXP size);

typedef struct {
    Rf_La_svd svd;
    Rf_La_rs  rs;
    Rf_La_rg  rg;
    Rf_La_zgesv zgesv;
    Rf_La_zgeqp3 zgeqp3;
    Rf_qr_coef_cmplx qr_coef_cmplx;
    Rf_qr_qy_cmplx qr_qy_cmplx;
    Rf_La_svd_cmplx svd_cmplx;
    Rf_La_rs_complex rs_cmplx;
    Rf_La_rg_complex rg_cmplx;
    Rf_La_chol chol;
    Rf_La_chol2inv chol2inv;
} R_LapackRoutines;

R_LapackRoutines *R_setLapackRoutines(R_LapackRoutines *routines);


#endif /* R_LAPACK_MODULE_H */
