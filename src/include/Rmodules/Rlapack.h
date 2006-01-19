/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-6 The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* Definitions for the Lapack module.  Not intended for end-user use */

#ifndef R_LAPACK_MODULE_H
#define R_LAPACK_MODULE_H

#include <Rinternals.h>

typedef SEXP (*Rf_La_svd)(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, 
			  SEXP v, SEXP method);
typedef SEXP (*Rf_La_rs)(SEXP x, SEXP only_values);
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
typedef SEXP (*Rf_La_dgesv)(SEXP A, SEXP B, SEXP tol);
typedef SEXP (*Rf_La_dgeqp3)(SEXP A);
typedef SEXP (*Rf_qr_coef_real)(SEXP Q, SEXP B);
typedef SEXP (*Rf_qr_qy_real)(SEXP Q, SEXP B, SEXP trans);
typedef SEXP (*Rf_det_ge_real)(SEXP A, SEXP logarithm);

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
    Rf_La_dgesv dgesv;
    Rf_La_dgeqp3 dgeqp3;
    Rf_qr_coef_real qr_coef_real;
    Rf_qr_qy_real qr_qy_real;
    Rf_det_ge_real det_ge_real;
} R_LapackRoutines;

R_LapackRoutines *R_setLapackRoutines(R_LapackRoutines *routines);


#endif /* R_LAPACK_MODULE_H */
