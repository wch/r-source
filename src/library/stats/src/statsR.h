/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012   The R Core Team.
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
 *  https://www.R-project.org/Licenses/
 */

/* auxiliary */
SEXP getListElement(SEXP list, char *str);

/* Declarations for .Call entry points */

SEXP logit_link(SEXP mu);
SEXP logit_linkinv(SEXP eta);
SEXP logit_mu_eta(SEXP eta);
SEXP binomial_dev_resids(SEXP y, SEXP mu, SEXP wt);

SEXP cutree(SEXP merge, SEXP which);
SEXP rWishart(SEXP ns, SEXP nuP, SEXP scal);
SEXP Cdqrls(SEXP x, SEXP y, SEXP tol, SEXP chk);
SEXP Cdist(SEXP x, SEXP method, SEXP attrs, SEXP p);
SEXP r2dtable(SEXP n, SEXP r, SEXP c);
SEXP cor(SEXP x, SEXP y, SEXP na_method, SEXP method);
SEXP cov(SEXP x, SEXP y, SEXP na_method, SEXP method);
SEXP updateform(SEXP old, SEXP new);
SEXP fft(SEXP z, SEXP inverse);
SEXP mvfft(SEXP z, SEXP inverse);
SEXP nextn(SEXP n, SEXP factors);

SEXP cfilter(SEXP sx, SEXP sfilter, SEXP ssides, SEXP scircular);
SEXP rfilter(SEXP x, SEXP filter, SEXP out);
SEXP lowess(SEXP x, SEXP y, SEXP sf, SEXP siter, SEXP sdelta);
SEXP DoubleCentre(SEXP A);
SEXP BinDist(SEXP x, SEXP weights, SEXP slo, SEXP sup, SEXP sn);

SEXP do_dchisq(SEXP sa, SEXP sb, SEXP sI);
SEXP do_dexp(SEXP sa, SEXP sb, SEXP sI);
SEXP do_dgeom(SEXP sa, SEXP sb, SEXP sI);
SEXP do_dpois(SEXP sa, SEXP sb, SEXP sI);
SEXP do_dt(SEXP sa, SEXP sb, SEXP sI);
SEXP do_dsignrank(SEXP sa, SEXP sb, SEXP sI);
SEXP do_pchisq(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qchisq(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_pexp(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qexp(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_pgeom(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qgeom(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_ppois(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qpois(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_pt(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qt(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_psignrank(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);
SEXP do_qsignrank(SEXP sa, SEXP sb, SEXP sI, SEXP sJ);

SEXP do_dbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dcauchy(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_df(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dgamma(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dlnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dlogis(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dnbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dnbinom_mu(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dweibull(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dunif(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dnt(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dnchisq(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_dwilcox(SEXP sa, SEXP sb, SEXP sc, SEXP sI);
SEXP do_pbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pcauchy(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qcauchy(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pf(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qf(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pgamma(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qgamma(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_plnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qlnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_plogis(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qlogis(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pnbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qnbinom(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pnbinom_mu(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qnbinom_mu(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qnorm(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pweibull(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qweibull(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_punif(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qunif(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pnt(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qnt(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pnchisq(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qnchisq(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_pwilcox(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);
SEXP do_qwilcox(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ);

SEXP do_dhyper(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI);
SEXP do_dnbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI);
SEXP do_dnf(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI);
SEXP do_phyper(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_qhyper(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_pnbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_qnbeta(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_pnf(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_qnf(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_ptukey(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);
SEXP do_qtukey(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ);

SEXP do_rchisq(SEXP sn, SEXP sa);
SEXP do_rexp(SEXP sn, SEXP sa);
SEXP do_rgeom(SEXP sn, SEXP sa);
SEXP do_rpois(SEXP sn, SEXP sa);
SEXP do_rt(SEXP sn, SEXP sa);
SEXP do_rsignrank(SEXP sn, SEXP sa);

SEXP do_rbeta(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rbinom(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rcauchy(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rf(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rgamma(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rlnorm(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rlogis(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rnbinom(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rnorm(SEXP sn, SEXP sa, SEXP sb);
SEXP do_runif(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rweibull(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rwilcox(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rnchisq(SEXP sn, SEXP sa, SEXP sb);
SEXP do_rnbinom_mu(SEXP sn, SEXP sa, SEXP sb);

SEXP do_rhyper(SEXP sn, SEXP sa, SEXP sb, SEXP sc);

SEXP do_rmultinom(SEXP sn, SEXP ssize, SEXP sprob);

/* Declarations for .External[2] entry points */

SEXP compcases(SEXP args);
SEXP doD(SEXP args);
SEXP deriv(SEXP args);
SEXP modelframe(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP modelmatrix(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP termsform(SEXP args);
SEXP do_fmin(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP nlm(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP zeroin2(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP optim(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP optimhess(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP call_dqagi(SEXP);
SEXP call_dqags(SEXP);

SEXP Rsm(SEXP x, SEXP stype, SEXP send);
SEXP tukeyline(SEXP x, SEXP y, SEXP iter, SEXP call);
SEXP runmed(SEXP x, SEXP stype, SEXP sk, SEXP end, SEXP print_level);
SEXP influence(SEXP mqr, SEXP do_coef, SEXP e, SEXP stol);

SEXP pSmirnov2x(SEXP statistic, SEXP snx, SEXP sny);
SEXP pKolmogorov2x(SEXP statistic, SEXP sn);
SEXP pKS2(SEXP sn, SEXP stol);

SEXP ksmooth(SEXP x, SEXP y, SEXP snp, SEXP skrn, SEXP sbw);

SEXP SplineCoef(SEXP method, SEXP x, SEXP y);
SEXP SplineEval(SEXP xout, SEXP z);

SEXP ApproxTest(SEXP x, SEXP y, SEXP method, SEXP sf);
SEXP Approx(SEXP x, SEXP y, SEXP v, SEXP method,
	    SEXP yleft, SEXP yright, SEXP sf);

SEXP LogLin(SEXP dtab, SEXP conf, SEXP table, SEXP start,
	    SEXP snmar, SEXP eps, SEXP iter);

SEXP pAnsari(SEXP q, SEXP sm, SEXP sn);
SEXP qAnsari(SEXP p, SEXP sm, SEXP sn);
SEXP pKendall(SEXP q, SEXP sn);
SEXP pRho(SEXP q, SEXP sn, SEXP lower);
SEXP SWilk(SEXP x);

SEXP bw_den(SEXP nbin, SEXP sx);
SEXP bw_den_binned(SEXP sx);
SEXP bw_ucv(SEXP sn, SEXP sd, SEXP cnt, SEXP sh);
SEXP bw_bcv(SEXP sn, SEXP sd, SEXP cnt, SEXP sh);
SEXP bw_phi4(SEXP sn, SEXP sd, SEXP cnt, SEXP sh);
SEXP bw_phi6(SEXP sn, SEXP sd, SEXP cnt, SEXP sh);

SEXP Fexact(SEXP x, SEXP pars, SEXP work, SEXP smult);
SEXP Fisher_sim(SEXP sr, SEXP sc, SEXP sB);
SEXP chisq_sim(SEXP sr, SEXP sc, SEXP sB, SEXP E);
SEXP d2x2xk(SEXP sK, SEXP sm, SEXP sn, SEXP st, SEXP srn);

SEXP stats_signrank_free(void);
SEXP stats_wilcox_free(void);
