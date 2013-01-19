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
    CALLDEF(KalmanLike, 11),
    CALLDEF(KalmanFore, 8),
    CALLDEF(KalmanSmooth, 9),
    CALLDEF(ARIMA_undoPars, 2),
    CALLDEF(ARIMA_transPars, 3),
    CALLDEF(ARIMA_Invtrans, 2),
    CALLDEF(ARIMA_Gradtrans, 2),
    CALLDEF(ARIMA_Like, 9),
    CALLDEF(ARIMA_CSS, 6),
    CALLDEF(TSconv, 2),
    CALLDEF(getQ0, 2),
    CALLDEF(port_ivset, 3),
    CALLDEF(port_nlminb, 9),
    CALLDEF(port_nlsb, 7),
    CALLDEF(logit_link, 1),
    CALLDEF(logit_linkinv, 1),
    CALLDEF(logit_mu_eta, 1),
    CALLDEF(binomial_dev_resids, 3),
    CALLDEF(rWishart, 3),
    CALLDEF(Cdqrls, 3),
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

    // 1-arg distributions
    {"dchisq", (DL_FUNC) &distn2, 3},
    {"pchisq", (DL_FUNC) &distn2, 4},
    {"qchisq", (DL_FUNC) &distn2, 4},
    {"rchisq", (DL_FUNC) &Random1, 2},
    {"dexp", (DL_FUNC) &distn2, 3},
    {"pexp", (DL_FUNC) &distn2, 4},
    {"qexp", (DL_FUNC) &distn2, 4},
    {"rexp", (DL_FUNC) &Random1, 2},
    {"dgeom", (DL_FUNC) &distn2, 3},
    {"pgeom", (DL_FUNC) &distn2, 4},
    {"qgeom", (DL_FUNC) &distn2, 4},
    {"rgeom", (DL_FUNC) &Random1, 2},
    {"dpois", (DL_FUNC) &distn2, 3},
    {"ppois", (DL_FUNC) &distn2, 4},
    {"qpois", (DL_FUNC) &distn2, 4},
    {"rpois", (DL_FUNC) &Random1, 2},
    {"dt", (DL_FUNC) &distn2, 3},
    {"pt", (DL_FUNC) &distn2, 4},
    {"qt", (DL_FUNC) &distn2, 4},
    {"rt", (DL_FUNC) &Random1, 2},
    {"dsignrank", (DL_FUNC) &distn2, 3},
    {"psignrank", (DL_FUNC) &distn2, 4},
    {"qsignrank", (DL_FUNC) &distn2, 4},
    {"rsignrank", (DL_FUNC) &Random1, 2},

    // 2-arg distributions
    {"dbeta", (DL_FUNC) &distn3, 4},
    {"pbeta", (DL_FUNC) &distn3, 5},
    {"qbeta", (DL_FUNC) &distn3, 5},
    {"rbeta", (DL_FUNC) &Random2, 3},
    {"dbinom", (DL_FUNC) &distn3, 4},
    {"pbinom", (DL_FUNC) &distn3, 5},
    {"qbinom", (DL_FUNC) &distn3, 5},
    {"rbinom", (DL_FUNC) &Random2, 3},
    {"dcauchy", (DL_FUNC) &distn3, 4},
    {"pcauchy", (DL_FUNC) &distn3, 5},
    {"qcauchy", (DL_FUNC) &distn3, 5},
    {"rcauchy", (DL_FUNC) &Random2, 3},
    {"df", (DL_FUNC) &distn3, 4},
    {"pf", (DL_FUNC) &distn3, 5},
    {"qf", (DL_FUNC) &distn3, 5},
    {"rf", (DL_FUNC) &Random2, 3},
    {"dgamma", (DL_FUNC) &distn3, 4},
    {"pgamma", (DL_FUNC) &distn3, 5},
    {"qgamma", (DL_FUNC) &distn3, 5},
    {"rgamma", (DL_FUNC) &Random2, 3},
    {"dlnorm", (DL_FUNC) &distn3, 4},
    {"plnorm", (DL_FUNC) &distn3, 5},
    {"qlnorm", (DL_FUNC) &distn3, 5},
    {"rlnorm", (DL_FUNC) &Random2, 3},
    {"dlogis", (DL_FUNC) &distn3, 4},
    {"plogis", (DL_FUNC) &distn3, 5},
    {"qlogis", (DL_FUNC) &distn3, 5},
    {"rlogis", (DL_FUNC) &Random2, 3},
    {"dnbinom", (DL_FUNC) &distn3, 4},
    {"pnbinom", (DL_FUNC) &distn3, 5},
    {"qnbinom", (DL_FUNC) &distn3, 5},
    {"rnbinom", (DL_FUNC) &Random2, 3},
    {"dnbinom_mu", (DL_FUNC) &distn3, 4},
    {"pnbinom_mu", (DL_FUNC) &distn3, 5},
    {"qnbinom_mu", (DL_FUNC) &distn3, 5},  // exists but currently unused
    {"rnbinom_mu", (DL_FUNC) &Random2, 3},
    {"dnchisq", (DL_FUNC) &distn3, 4},
    {"pnchisq", (DL_FUNC) &distn3, 5},
    {"qnchisq", (DL_FUNC) &distn3, 5},
    {"rnchisq", (DL_FUNC) &Random2, 3},
    {"dnorm", (DL_FUNC) &distn3, 4},
    {"pnorm", (DL_FUNC) &distn3, 5},
    {"qnorm", (DL_FUNC) &distn3, 5},
    {"rnorm", (DL_FUNC) &Random2, 3},
    {"dnt", (DL_FUNC) &distn3, 4},
    {"pnt", (DL_FUNC) &distn3, 5},
    {"qnt", (DL_FUNC) &distn3, 5},
    {"ptukey", (DL_FUNC) &distn4, 6},
    {"qtukey", (DL_FUNC) &distn4, 6},
    {"dunif", (DL_FUNC) &distn3, 4},
    {"punif", (DL_FUNC) &distn3, 5},
    {"qunif", (DL_FUNC) &distn3, 5},
    {"runif", (DL_FUNC) &Random2, 3},
    {"dweibull", (DL_FUNC) &distn3, 4},
    {"pweibull", (DL_FUNC) &distn3, 5},
    {"qweibull", (DL_FUNC) &distn3, 5},
    {"rweibull", (DL_FUNC) &Random2, 3},
    {"dwilcox", (DL_FUNC) &distn3, 4},
    {"pwilcox", (DL_FUNC) &distn3, 5},
    {"qwilcox", (DL_FUNC) &distn3, 5},
    {"rwilcox", (DL_FUNC) &Random2, 3},

    // 3-arg distributions
    {"dhyper", (DL_FUNC) &distn4, 5},
    {"phyper", (DL_FUNC) &distn4, 6},
    {"qhyper", (DL_FUNC) &distn4, 6},
    {"rhyper", (DL_FUNC) &Random3, 4},
    {"dnbeta", (DL_FUNC) &distn4, 5},
    {"pnbeta", (DL_FUNC) &distn4, 6},
    {"qnbeta", (DL_FUNC) &distn4, 6},
    {"dnf", (DL_FUNC) &distn4, 5},
    {"pnf", (DL_FUNC) &distn4, 6},
    {"qnf", (DL_FUNC) &distn4, 6},

    {"rmultinom", (DL_FUNC) &Rmultinom, 3},

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
