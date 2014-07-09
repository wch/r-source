/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2012   The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 */

#include <R.h>
#include <Rinternals.h>

#include "modreg.h"
#include "nls.h"
#include "port.h"
#include "stats.h"
#include "statsR.h"
#include "ts.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static const R_CMethodDef CEntries[]  = {
    {"loess_raw", (DL_FUNC) &loess_raw, 24},
    {"loess_dfit", (DL_FUNC) &loess_dfit, 13},
    {"loess_dfitse", (DL_FUNC) &loess_dfitse, 16},
    {"loess_ifit", (DL_FUNC) &loess_ifit, 8},
    {"loess_ise", (DL_FUNC) &loess_ise, 15},
    {"multi_burg", (DL_FUNC) &multi_burg, 11},
    {"multi_yw", (DL_FUNC) &multi_yw, 10},
    {"HoltWinters", (DL_FUNC) &HoltWinters, 17},
    {"kmeans_Lloyd", (DL_FUNC) &kmeans_Lloyd, 9},
    {"kmeans_MacQueen", (DL_FUNC) &kmeans_MacQueen, 9},
    {NULL, NULL, 0}
};

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

#define CALLDEF_DO(name, n) {#name, (DL_FUNC) &do_##name, n}
#define CALLDEF_MATH2_1(name) CALLDEF_DO(name, 3)
#define CALLDEF_MATH2_2(name) CALLDEF_DO(name, 4)
#define CALLDEF_MATH3_1(name) CALLDEF_DO(name, 4)
#define CALLDEF_MATH3_2(name) CALLDEF_DO(name, 5)
#define CALLDEF_MATH4_1(name) CALLDEF_DO(name, 5)
#define CALLDEF_MATH4_2(name) CALLDEF_DO(name, 6)

#define CALLDEF_RAND1(name) CALLDEF_DO(name, 2)
#define CALLDEF_RAND2(name) CALLDEF_DO(name, 3)
#define CALLDEF_RAND3(name) CALLDEF_DO(name, 4)

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(cutree, 2),
    CALLDEF(isoreg, 1),
    CALLDEF(monoFC_m, 2),
    CALLDEF(numeric_deriv, 4),
    CALLDEF(nls_iter, 3),
    CALLDEF(setup_starma, 8),
    CALLDEF(free_starma, 1),
    CALLDEF(set_trans, 2),
    CALLDEF(arma0fa, 2),
    CALLDEF(get_s2, 1),
    CALLDEF(get_resid, 1),
    CALLDEF(Dotrans, 2),
    CALLDEF(arma0_kfore, 4),
    CALLDEF(Starma_method, 2),
    CALLDEF(Invtrans, 2),
    CALLDEF(Gradtrans, 2),
    CALLDEF(ARMAtoMA, 3),
    CALLDEF(KalmanLike, 5),
    CALLDEF(KalmanFore, 3),
    CALLDEF(KalmanSmooth, 3),
    CALLDEF(ARIMA_undoPars, 2),
    CALLDEF(ARIMA_transPars, 3),
    CALLDEF(ARIMA_Invtrans, 2),
    CALLDEF(ARIMA_Gradtrans, 2),
    CALLDEF(ARIMA_Like, 4),
    CALLDEF(ARIMA_CSS, 6),
    CALLDEF(TSconv, 2),
    CALLDEF(getQ0, 2),
    CALLDEF(getQ0bis, 3),
    CALLDEF(port_ivset, 3),
    CALLDEF(port_nlminb, 9),
    CALLDEF(port_nlsb, 7),
    CALLDEF(logit_link, 1),
    CALLDEF(logit_linkinv, 1),
    CALLDEF(logit_mu_eta, 1),
    CALLDEF(binomial_dev_resids, 3),
    CALLDEF(rWishart, 3),
    CALLDEF(Cdqrls, 4),
    CALLDEF(Cdist, 4),
    CALLDEF(cor, 4),
    CALLDEF(cov, 4),
    CALLDEF(updateform, 2),
    CALLDEF(fft, 2),
    CALLDEF(mvfft, 2),
    CALLDEF(nextn, 2),
    CALLDEF(r2dtable, 3),
    CALLDEF(cfilter, 4),
    CALLDEF(rfilter, 3),
    CALLDEF(lowess, 5),
    CALLDEF(DoubleCentre, 1),
    CALLDEF(BinDist, 5),
    CALLDEF(Rsm, 3),
    CALLDEF(tukeyline, 3),
    CALLDEF(runmed, 5),
    CALLDEF(influence, 4),
    CALLDEF(pSmirnov2x, 3),
    CALLDEF(pKolmogorov2x, 2),
    CALLDEF(pKS2, 2),
    CALLDEF(ksmooth, 5),
    CALLDEF(SplineCoef, 3),
    CALLDEF(SplineEval, 2),
    CALLDEF(Approx, 7),
    CALLDEF(ApproxTest, 4),
    CALLDEF(LogLin, 7),
    CALLDEF(pAnsari, 3),
    CALLDEF(qAnsari, 3),
    CALLDEF(pKendall, 2),
    CALLDEF(pRho, 3),
    CALLDEF(SWilk, 1),
    CALLDEF(bw_den, 2),
    CALLDEF(bw_ucv, 4),
    CALLDEF(bw_bcv, 4),
    CALLDEF(bw_phi4, 4),
    CALLDEF(bw_phi6, 4),
    CALLDEF(acf, 3),
    CALLDEF(pacf1, 2),
    CALLDEF(ar2ma, 2),
    CALLDEF(Burg, 2),
    CALLDEF(intgrt_vec, 3),
    CALLDEF(pp_sum, 2),
    CALLDEF(Fexact, 4),
    CALLDEF(Fisher_sim, 3),
    CALLDEF(chisq_sim, 4),
    CALLDEF(d2x2xk, 5),

    CALLDEF_MATH2_1(dchisq),
    CALLDEF_MATH2_1(dexp),
    CALLDEF_MATH2_1(dgeom),
    CALLDEF_MATH2_1(dpois),
    CALLDEF_MATH2_1(dt),
    CALLDEF_MATH2_1(dsignrank),
    CALLDEF_MATH2_2(pchisq),
    CALLDEF_MATH2_2(qchisq),
    CALLDEF_MATH2_2(pexp),
    CALLDEF_MATH2_2(qexp),
    CALLDEF_MATH2_2(pgeom),
    CALLDEF_MATH2_2(qgeom),
    CALLDEF_MATH2_2(ppois),
    CALLDEF_MATH2_2(qpois),
    CALLDEF_MATH2_2(pt),
    CALLDEF_MATH2_2(qt),
    CALLDEF_MATH2_2(psignrank),
    CALLDEF_MATH2_2(qsignrank),

    CALLDEF_MATH3_1(dbeta),
    CALLDEF_MATH3_1(dbinom),
    CALLDEF_MATH3_1(dcauchy),
    CALLDEF_MATH3_1(df),
    CALLDEF_MATH3_1(dgamma),
    CALLDEF_MATH3_1(dlnorm),
    CALLDEF_MATH3_1(dlogis),
    CALLDEF_MATH3_1(dnbinom),
    CALLDEF_MATH3_1(dnbinom_mu),
    CALLDEF_MATH3_1(dnorm),
    CALLDEF_MATH3_1(dweibull),
    CALLDEF_MATH3_1(dunif),
    CALLDEF_MATH3_1(dnt),
    CALLDEF_MATH3_1(dnchisq),
    CALLDEF_MATH3_1(dwilcox),
    CALLDEF_MATH3_2(pbeta),
    CALLDEF_MATH3_2(qbeta),
    CALLDEF_MATH3_2(pbinom),
    CALLDEF_MATH3_2(qbinom),
    CALLDEF_MATH3_2(pcauchy),
    CALLDEF_MATH3_2(qcauchy),
    CALLDEF_MATH3_2(pf),
    CALLDEF_MATH3_2(qf),
    CALLDEF_MATH3_2(pgamma),
    CALLDEF_MATH3_2(qgamma),
    CALLDEF_MATH3_2(plnorm),
    CALLDEF_MATH3_2(qlnorm),
    CALLDEF_MATH3_2(plogis),
    CALLDEF_MATH3_2(qlogis),
    CALLDEF_MATH3_2(pnbinom),
    CALLDEF_MATH3_2(qnbinom),
    CALLDEF_MATH3_2(pnbinom_mu),
    CALLDEF_MATH3_2(qnbinom_mu),
    CALLDEF_MATH3_2(pnorm),
    CALLDEF_MATH3_2(qnorm),
    CALLDEF_MATH3_2(pweibull),
    CALLDEF_MATH3_2(qweibull),
    CALLDEF_MATH3_2(punif),
    CALLDEF_MATH3_2(qunif),
    CALLDEF_MATH3_2(pnt),
    CALLDEF_MATH3_2(qnt),
    CALLDEF_MATH3_2(pnchisq),
    CALLDEF_MATH3_2(qnchisq),
    CALLDEF_MATH3_2(pwilcox),
    CALLDEF_MATH3_2(qwilcox),
        // {"qnbinom_mu", (DL_FUNC) &distn3, 5},  // exists but currently unused

    CALLDEF_MATH4_1(dhyper),
    CALLDEF_MATH4_1(dnbeta),
    CALLDEF_MATH4_1(dnf),
    CALLDEF_MATH4_2(phyper),
    CALLDEF_MATH4_2(qhyper),
    CALLDEF_MATH4_2(pnbeta),
    CALLDEF_MATH4_2(qnbeta),
    CALLDEF_MATH4_2(pnf),
    CALLDEF_MATH4_2(qnf),
    CALLDEF_MATH4_2(ptukey),
    CALLDEF_MATH4_2(qtukey),

    CALLDEF_RAND1(rchisq),
    CALLDEF_RAND1(rexp),
    CALLDEF_RAND1(rgeom),
    CALLDEF_RAND1(rpois),
    CALLDEF_RAND1(rt),
    CALLDEF_RAND1(rsignrank),

    CALLDEF_RAND2(rbeta),
    CALLDEF_RAND2(rbinom),
    CALLDEF_RAND2(rcauchy),
    CALLDEF_RAND2(rf),
    CALLDEF_RAND2(rgamma),
    CALLDEF_RAND2(rlnorm),
    CALLDEF_RAND2(rlogis),
    CALLDEF_RAND2(rnbinom),
    CALLDEF_RAND2(rnorm),
    CALLDEF_RAND2(runif),
    CALLDEF_RAND2(rweibull),
    CALLDEF_RAND2(rwilcox),
    CALLDEF_RAND2(rnchisq),
    CALLDEF_RAND2(rnbinom_mu),

    CALLDEF_RAND3(rhyper),

    CALLDEF_DO(rmultinom, 3),

    {NULL, NULL, 0}
};

#define FDEF(name)  {#name, (DL_FUNC) &F77_SUB(name), sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static R_NativePrimitiveArgType lowesw_t[] = { REALSXP, INTSXP, REALSXP, INTSXP};
static R_NativePrimitiveArgType lowesp_t[] = {
    INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, REALSXP};


static const R_FortranMethodDef FortEntries[] = {
    FDEF(lowesw),
    FDEF(lowesp),
    {"setppr", (DL_FUNC) &F77_SUB(setppr), 6},
    {"smart", (DL_FUNC) &F77_SUB(smart), 16},
    {"pppred", (DL_FUNC) &F77_SUB(pppred), 5},
    {"setsmu", (DL_FUNC) &F77_SUB(setsmu), 0},
    {"rbart", (DL_FUNC) &F77_SUB(rbart), 20},
    {"bvalus", (DL_FUNC) &F77_SUB(bvalus), 7},
    {"supsmu", (DL_FUNC) &F77_SUB(supsmu), 10},
    {"hclust", (DL_FUNC) &F77_SUB(hclust), 11},
    {"hcass2", (DL_FUNC) &F77_SUB(hcass2), 6},
    {"kmns", (DL_FUNC) &F77_SUB(kmns), 17},
    {"eureka", (DL_FUNC) &F77_SUB(eureka), 6},
    {"stl", (DL_FUNC) &F77_SUB(stl), 18},
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}
// These argument counts are not checked
static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(compcases, -1),
    EXTDEF(doD, 2),
    EXTDEF(deriv, 5),
    EXTDEF(modelframe, 8),
    EXTDEF(modelmatrix, 2),
    EXTDEF(termsform, 5),
    EXTDEF(do_fmin, 4),
    EXTDEF(nlm, 11),
    EXTDEF(zeroin2, 7),
    EXTDEF(optim, 7),
    EXTDEF(optimhess, 4),
    EXTDEF(call_dqags, 7),
    EXTDEF(call_dqagi, 7),

    {"signrank_free", (DL_FUNC) &stats_signrank_free, 0},
    {"wilcox_free", (DL_FUNC) &stats_wilcox_free, 0},
    {NULL, NULL, 0}
};


void attribute_visible R_init_stats(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, FortEntries, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);

    R_RegisterCCallable("stats", "nlminb_iterate", (DL_FUNC) nlminb_iterate);
    R_RegisterCCallable("stats", "nlsb_iterate", (DL_FUNC) nlsb_iterate);
    R_RegisterCCallable("stats", "Rf_divset", (DL_FUNC) Rf_divset);
}
