/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002-2003	The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  This header contains the declarations of code to be used by
 *  .C, .Fortran, .Call or .External within the base package.
 *  These routines are `registered' in registration.c.
 */

void Rsockconnect(int *, char **);
void Rsockopen(int *);
void Rsocklisten(int *, char **, int *);
void Rsockclose(int *);
void Rsockread(int *, char **, int *);
void Rsockwrite(int *, char **, int *, int *, int *);
SEXP call_dqagi(SEXP);
SEXP call_dqags(SEXP);
SEXP La_svd(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP La_rs(SEXP, SEXP, SEXP);
SEXP La_rg(SEXP, SEXP);
SEXP La_zgesv(SEXP, SEXP);
SEXP La_zgeqp3(SEXP);
SEXP qr_coef_cmplx(SEXP, SEXP);
SEXP qr_qy_cmplx(SEXP, SEXP, SEXP);
SEXP La_svd_cmplx(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP La_rs_cmplx(SEXP, SEXP);
SEXP La_rg_cmplx(SEXP, SEXP);
SEXP La_chol(SEXP);
SEXP La_chol2inv(SEXP, SEXP);
SEXP La_dgesv(SEXP, SEXP, SEXP);
SEXP La_dgeqp3(SEXP);
SEXP qr_coef_real(SEXP, SEXP);
SEXP qr_qy_real(SEXP, SEXP, SEXP);
SEXP det_ge_real(SEXP, SEXP);

void signrank_free();
void wilcox_free();
void band_ucv_bin(int *, int *, double *, int *, double *, double *);
void band_bcv_bin(int *, int *, double *, int *, double *, double *);
void band_phi4_bin(int *, int *, double *, int *, double *, double *);
void band_phi6_bin(int *, int *, double *, int *, double *, double *);
void band_den_bin(int *, int *, double *, int *, double *, double *);

SEXP R_getTaskCallbackNames();
SEXP R_removeTaskCallback(SEXP);
SEXP R_addTaskCallback(SEXP, SEXP, SEXP, SEXP);

SEXP R_getSymbolInfo(SEXP, SEXP);
SEXP R_getDllTable();
SEXP R_getRegisteredRoutines(SEXP);

SEXP R_isMethodsDispatchOn(SEXP);
SEXP R_traceOnOff(SEXP);

SEXP Rrowsum_matrix(SEXP, SEXP, SEXP, SEXP);
SEXP Rrowsum_df(SEXP, SEXP, SEXP, SEXP);
SEXP do_mapply(SEXP, SEXP, SEXP, SEXP);

SEXP R_r2dtable(SEXP, SEXP, SEXP);

void F77_SYMBOL(dchdc)(double *, int *, int *, double *, int *, int *, int *);
void F77_SYMBOL(dpbfa)(double *, int *, int *, int *, int *);
void F77_SYMBOL(dpbsl)(double *, int *, int *, int *, double *);

void Rf_InitGraphics(void);
void Rf_InitColors(void);

SEXP R_compress1(SEXP);
SEXP R_decompress1(SEXP);

SEXP R_serializeb(SEXP, SEXP, SEXP);
SEXP R_serialize(SEXP, SEXP, SEXP, SEXP);
SEXP R_unserialize(SEXP, SEXP);

SEXP R_getVarsFromFrame(SEXP, SEXP, SEXP);
SEXP R_lazyLoadDBinsertValue(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP R_lazyLoadDBfetch(SEXP, SEXP, SEXP, SEXP);
SEXP R_lazyLoadDBflush(SEXP);

#ifdef BYTECODE
SEXP R_getbcprofcounts();
SEXP R_startbcprof();
SEXP R_stopbcprof();
#endif

SEXP PostScript(SEXP);
SEXP XFig(SEXP);
SEXP PDF(SEXP);
SEXP Type1FontInUse(SEXP);
