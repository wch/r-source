/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2   The R Development Core Team.
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
 */

#include <R.h>
#include "ts.h"
#include <R_ext/Rdynload.h>

R_CMethodDef CEntries[] = {
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
    {"HoltWinter", (DL_FUNC) &HoltWinters, 13},
    {NULL, NULL, 0}
};

R_CallMethodDef CallEntries[] = {
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
    {"KalmanLike", (DL_FUNC) &KalmanLike, 10},
    {"KalmanFore", (DL_FUNC) &KalmanFore, 7},
    {"ARIMA_undoPars", (DL_FUNC) &ARIMA_undoPars, 2},
    {"ARIMA_transPars", (DL_FUNC) &ARIMA_transPars, 3},
    {"ARIMA_Invtrans", (DL_FUNC) &ARIMA_Invtrans, 2},
    {"ARIMA_Gradtrans", (DL_FUNC) &ARIMA_Gradtrans, 2},
    {"ARIMA_Like", (DL_FUNC) &ARIMA_Like, 9},
    {"ARIMA_CSS", (DL_FUNC) &ARIMA_CSS, 6},
    {"convolve", (DL_FUNC) &convolve, 2},
    {NULL, NULL, 0}
};

R_FortranMethodDef FortEntries[] = {
    {"eureka", (DL_FUNC) &F77_SUB(eureka), 6},
    {"stl", (DL_FUNC) &F77_SUB(stl), 18},
    {NULL, NULL, 0}
};

void R_init_ts(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, FortEntries, NULL);
}
