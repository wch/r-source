/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-5   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <R.h>
#include <Rinternals.h>

#include "ctest.h"
#include "eda.h"
#include "family.h"
#include "modreg.h"
#include "mva.h"
#include "nls.h"
#include "port.h"
#include "stats.h"
#include "ts.h"
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType chisqsim_t[11] = {INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
					   INTSXP, REALSXP, INTSXP, REALSXP, INTSXP, REALSXP};
static R_NativePrimitiveArgType fishersim_t[10] = {INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
					   INTSXP, INTSXP, REALSXP, INTSXP, REALSXP};
static R_NativePrimitiveArgType d2_t[5] = {INTSXP, REALSXP, REALSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType dansari_t[4] = {INTSXP, REALSXP, INTSXP, INTSXP};
static R_NativePrimitiveArgType pansari_t[4] = {INTSXP, REALSXP, INTSXP, INTSXP};
static R_NativePrimitiveArgType qansari_t[4] = {INTSXP, REALSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType fexact_t[11] = {INTSXP, INTSXP, INTSXP, INTSXP, REALSXP,
					 REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType pkolmogorov2x_t[2] = {REALSXP, INTSXP};

static R_NativePrimitiveArgType pkendall_t[3] = {INTSXP, REALSXP, INTSXP};

static R_NativePrimitiveArgType pkstwo_t[3] = {INTSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType prho_t[5] = {INTSXP, REALSXP, REALSXP, INTSXP, LGLSXP};
static R_NativePrimitiveArgType psmirnov2x_t[3] = {REALSXP, INTSXP, INTSXP};

static R_NativePrimitiveArgType swilk_t[9] = {LGLSXP, SINGLESXP, INTSXP, INTSXP, INTSXP,
				       SINGLESXP, REALSXP, REALSXP, INTSXP};

static R_NativePrimitiveArgType Srunmed_t[6] = {REALSXP,REALSXP,INTSXP,INTSXP,
					 INTSXP,LGLSXP};
static R_NativePrimitiveArgType Trunmed_t[9] = {INTSXP,INTSXP, REALSXP,REALSXP,
					 INTSXP,INTSXP, REALSXP,INTSXP,INTSXP};

static R_NativePrimitiveArgType band_ucv_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType band_bcv_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType band_phi4_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType band_phi6_bin_t[] = {INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, REALSXP};
static R_NativePrimitiveArgType band_den_bin_t[] = {INTSXP, INTSXP, REALSXP, REALSXP, INTSXP};

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static const R_CMethodDef CEntries[]  = {
    {"chisqsim", (DL_FUNC) &chisqsim, 11, chisqsim_t},
    {"fisher_sim", (DL_FUNC) &fisher_sim, 10, fishersim_t},
    {"d2x2xk", (DL_FUNC) &d2x2xk, 5, d2_t},
    {"dansari", (DL_FUNC) &dansari, 4, dansari_t},
    {"fexact",   (DL_FUNC) &fexact, 11, fexact_t},
    {"pansari",  (DL_FUNC)&pansari, 4, pansari_t},
    {"pkolmogorov2x", (DL_FUNC) &pkolmogorov2x, 2, pkolmogorov2x_t},
    {"pkendall", (DL_FUNC)  &pkendall, 3, pkendall_t},
    {"pkstwo", (DL_FUNC) &pkstwo, 3, pkstwo_t},
    {"prho", (DL_FUNC) &prho, 5, prho_t},
    {"psmirnov2x", (DL_FUNC) &psmirnov2x, 3, psmirnov2x_t},
    {"qansari",  (DL_FUNC) &qansari, 4, qansari_t},
    {"swilk", (DL_FUNC) &swilk, 9, swilk_t},
    {"BDRksmooth", (DL_FUNC) &BDRksmooth, 8},
    {"loess_raw", (DL_FUNC) &loess_raw, 24},
    {"loess_dfit", (DL_FUNC) &loess_dfit, 13},
    {"loess_dfitse", (DL_FUNC) &loess_dfitse, 16},
    {"loess_ifit", (DL_FUNC) &loess_ifit, 8},
    {"loess_ise", (DL_FUNC) &loess_ise, 15},
    {"Srunmed", (DL_FUNC) &Srunmed, 6, Srunmed_t},
    {"Trunmed", (DL_FUNC) &Trunmed, 9, Trunmed_t},
    {"Rsm_3RSR", (DL_FUNC) &Rsm_3RSR, 5},
    {"Rsm_3RSS", (DL_FUNC) &Rsm_3RSS, 5},
    {"Rsm_3RS3R", (DL_FUNC) &Rsm_3RS3R, 5},
    {"Rsm_3R", (DL_FUNC) &Rsm_3R, 5},
    {"Rsm_3", (DL_FUNC) &Rsm_3, 5},
    {"Rsm_S", (DL_FUNC) &Rsm_S, 5},
    {"tukeyline", (DL_FUNC) &tukeyline, 6},
    {"dblcen", (DL_FUNC) &dblcen, 2},
    {"R_distance", (DL_FUNC) &R_distance, 7},
    {"acf", (DL_FUNC) &acf, 6},
    {"uni_pacf", (DL_FUNC) &uni_pacf, 3},
    {"artoma", (DL_FUNC) &artoma, 4},
    {"burg", (DL_FUNC) &burg, 6},
    {"multi_burg", (DL_FUNC) &multi_burg, 11},
    {"multi_yw", (DL_FUNC) &multi_yw, 10},
    {"R_intgrt_vec", (DL_FUNC) &R_intgrt_vec, 4},
    {"filter1", (DL_FUNC) &filter1, 7},
    {"filter2", (DL_FUNC) &filter2, 5},
    {"R_pp_sum", (DL_FUNC) &R_pp_sum, 4},
    {"HoltWinters", (DL_FUNC) &HoltWinters, 15},
    {"kmeans_Lloyd", (DL_FUNC) &kmeans_Lloyd, 9},
    {"kmeans_MacQueen", (DL_FUNC) &kmeans_MacQueen, 9},
    CDEF(band_ucv_bin),
    CDEF(band_bcv_bin),
    CDEF(band_phi4_bin),
    CDEF(band_phi6_bin),
    CDEF(band_den_bin),
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"R_cutree", (DL_FUNC) &R_cutree, 2},
    {"R_isoreg", (DL_FUNC) &R_isoreg, 1},
    {"numeric_deriv", (DL_FUNC)&numeric_deriv, 4},
    {"nls_iter", (DL_FUNC)&nls_iter, 3},
    {"setup_starma", (DL_FUNC) &setup_starma, 8},
    {"free_starma", (DL_FUNC) &free_starma, 1},
    {"set_trans", (DL_FUNC) &set_trans, 2},
    {"arma0fa", (DL_FUNC) &arma0fa, 2},
    {"get_s2", (DL_FUNC) &get_s2, 1},
    {"get_resid", (DL_FUNC) &get_resid, 1},
    {"Dotrans", (DL_FUNC) &Dotrans, 2},
    {"arma0_kfore", (DL_FUNC) &arma0_kfore, 4},
    {"Starma_method", (DL_FUNC) &Starma_method, 2},
    {"Invtrans", (DL_FUNC) &Invtrans, 2},
    {"Gradtrans", (DL_FUNC) &Gradtrans, 2},
    {"ARMAtoMA", (DL_FUNC) &ARMAtoMA, 3},
    {"KalmanLike", (DL_FUNC) &KalmanLike, 11},
    {"KalmanFore", (DL_FUNC) &KalmanFore, 8},
    {"KalmanSmooth", (DL_FUNC) &KalmanSmooth, 9},
    {"ARIMA_undoPars", (DL_FUNC) &ARIMA_undoPars, 2},
    {"ARIMA_transPars", (DL_FUNC) &ARIMA_transPars, 3},
    {"ARIMA_Invtrans", (DL_FUNC) &ARIMA_Invtrans, 2},
    {"ARIMA_Gradtrans", (DL_FUNC) &ARIMA_Gradtrans, 2},
    {"ARIMA_Like", (DL_FUNC) &ARIMA_Like, 9},
    {"ARIMA_CSS", (DL_FUNC) &ARIMA_CSS, 6},
    {"TSconv", (DL_FUNC) &TSconv, 2},
    {"getQ0", (DL_FUNC) &getQ0, 2},
    {"port_ivset", (DL_FUNC) &port_ivset, 3},
    {"port_nlminb", (DL_FUNC) &port_nlminb, 9},
    {"port_nlsb", (DL_FUNC) &port_nlsb, 7},
    {"logit_link", (DL_FUNC) &logit_link, 1},
    {"logit_linkinv", (DL_FUNC) &logit_linkinv, 1},
    {"logit_mu_eta", (DL_FUNC) &logit_mu_eta, 1},
    {"binomial_dev_resids", (DL_FUNC) &binomial_dev_resids, 3},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortEntries[] = {
    {"lowesw", (DL_FUNC) &F77_SUB(lowesw), 4},
    {"lowesp", (DL_FUNC) &F77_SUB(lowesp), 7},
    {"setppr", (DL_FUNC) &F77_SUB(setppr), 6},
    {"smart", (DL_FUNC) &F77_SUB(smart), 16},
    {"pppred", (DL_FUNC) &F77_SUB(pppred), 5},
    {"setsmu", (DL_FUNC) &F77_SUB(setsmu), 0},
    {"qsbart", (DL_FUNC) &F77_SUB(qsbart), 21},
    {"bvalus", (DL_FUNC) &F77_SUB(bvalus), 7},
    {"supsmu", (DL_FUNC) &F77_SUB(supsmu), 10},
    {"hclust", (DL_FUNC) &F77_SUB(hclust), 11},
    {"hcass2", (DL_FUNC) &F77_SUB(hcass2), 6},
    {"kmns", (DL_FUNC) &F77_SUB(kmns), 17},
    {"eureka", (DL_FUNC) &F77_SUB(eureka), 6},
    {"stl", (DL_FUNC) &F77_SUB(stl), 18},
    {NULL, NULL, 0}
};

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_stats(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, FortEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("stats", "nlminb_iterate", (DL_FUNC)nlminb_iterate);
    R_RegisterCCallable("stats", "nlsb_iterate", (DL_FUNC)nlsb_iterate);
    R_RegisterCCallable("stats", "Rf_divset", (DL_FUNC)Rf_divset);
}
