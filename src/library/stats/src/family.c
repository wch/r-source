#include <Rinternals.h>
#include <Rconfig.h>
#include <R_ext/Constants.h>
#include "family.h"

SEXP logit_link(SEXP mu)
{
    int i, n = LENGTH(mu);
    SEXP ans = PROTECT(duplicate(mu));

    if (!n || !isReal(mu))
	error(_("Argument %s must be a nonempty numeric vector"), "mu");
    for (i = 0; i < n; i++)
	REAL(ans)[i] = log(REAL(mu)[i]/(1 - REAL(mu)[i]));
    UNPROTECT(1);
    return ans;
}

SEXP logit_linkinv(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);
    double thresh;

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    thresh = -log(DOUBLE_EPS);
    for (i = 0; i < n; i++) {
	double etai = REAL(eta)[i];
	
	if (etai > thresh) etai = thresh;
	if (etai < -thresh) etai = -thresh;
	REAL(ans)[i] = exp(etai)/(1 + exp(etai));
    }
    UNPROTECT(1);
    return ans;
}

SEXP logit_mu_eta(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);
    double thresh;

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    thresh = -log(DOUBLE_EPS);
    for (i = 0; i < n; i++) {
	double etai = REAL(eta)[i];
	double opexp = 1 + exp(etai);
	
	REAL(ans)[i] = (etai > thresh || etai < -thresh) ? DOUBLE_EPS :
	    exp(etai)/(opexp * opexp);
    }
    UNPROTECT(1);
    return ans;
}

static R_INLINE
double y_log_y(double y, double mu)
{
    return (y) ? (y * log(y/mu)) : 0;
}

SEXP binomial_dev_resids(SEXP y, SEXP mu, SEXP wt)
{
    int i, n = LENGTH(y), lmu = LENGTH(mu), lwt = LENGTH(wt), nprot = 1;
    SEXP ans;
    
    if (!isReal(y)) {y = PROTECT(coerceVector(y, REALSXP)); nprot++;}
    ans = PROTECT(duplicate(y));
    if (!isReal(mu)) {mu = PROTECT(coerceVector(mu, REALSXP)); nprot++;}
    if (!isReal(wt)) {wt = PROTECT(coerceVector(wt, REALSXP)); nprot++;}
    if (lmu != n && lmu != 1)
	error(_("argument %s must be a numeric vector of length 1 or length %d"),
	      "mu", n);
    if (lwt != n && lwt != 1)
	error(_("argument %s must be a numeric vector of length 1 or length %d"),
	      "wt", n);
    for (i = 0; i < n; i++) {
	double mui = REAL(mu)[lmu > 1 ? i : 0], yi = REAL(y)[i];

	REAL(ans)[i] = 2 * REAL(wt)[lwt > 1 ? i : 0] *
	    (y_log_y(yi, mui) + y_log_y(1 - yi, 1 - mui));
    }
    UNPROTECT(nprot);
    return ans;
}

