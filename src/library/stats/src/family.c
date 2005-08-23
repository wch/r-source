#include <Rinternals.h>
#include <Rconfig.h>
#include <R_ext/Constants.h>
#include "family.h"

static const double THRESH = 30.;
static const double MTHRESH = -30.;
static const double INVEPS = 1/DOUBLE_EPS;

/** 
 * Evaluate x/(1 - x). An inline function is used so that x is
 * evaluated once only. 
 * 
 * @param x input in the range (0, 1)
 * 
 * @return x/(1 - x) 
 */
static R_INLINE double x_d_omx(double x) {
    if (x < 0 || x > 1)
	error(_("Value %d out of range (0, 1)"), x);
    return x/(1 - x);
}

/** 
 * Evaluate x/(1 + x). An inline function is used so that x is
 * evaluated once only. 
 * 
 * @param x input
 * 
 * @return x/(1 + x) 
 */
static R_INLINE double x_d_opx(double x) {return x/(1 + x);}

SEXP logit_link(SEXP mu)
{
    int i, n = LENGTH(mu);
    SEXP ans = PROTECT(duplicate(mu));

    if (!n || !isReal(mu))
	error(_("Argument %s must be a nonempty numeric vector"), "mu");
    for (i = 0; i < n; i++)
	REAL(ans)[i] = log(x_d_omx(REAL(mu)[i]));
    UNPROTECT(1);
    return ans;
}

SEXP logit_linkinv(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    for (i = 0; i < n; i++) {
	double etai = REAL(eta)[i];

	REAL(ans)[i] = x_d_opx((etai < MTHRESH) ? DOUBLE_EPS :
			       ((etai > THRESH) ? INVEPS : exp(etai)));
    }
    UNPROTECT(1);
    return ans;
}

SEXP logit_mu_eta(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    for (i = 0; i < n; i++) {
	double etai = REAL(eta)[i];
	double opexp = 1 + exp(etai);

	REAL(ans)[i] = (etai > THRESH || etai < MTHRESH) ? DOUBLE_EPS :
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
    double mui, yi;

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
    /* Written separately to avoid an optimization bug on Solaris cc */
    if(lmu > 1) {
	for (i = 0; i < n; i++) {
	    mui = REAL(mu)[i];
	    yi = REAL(y)[i];
	    REAL(ans)[i] = 2 * REAL(wt)[lwt > 1 ? i : 0] *
		(y_log_y(yi, mui) + y_log_y(1 - yi, 1 - mui));
	}
    } else {
	mui = REAL(mu)[0];
	for (i = 0; i < n; i++) {
	    yi = REAL(y)[i];
	    REAL(ans)[i] = 2 * REAL(wt)[lwt > 1 ? i : 0] *
		(y_log_y(yi, mui) + y_log_y(1 - yi, 1 - mui));
	}
    }

    UNPROTECT(nprot);
    return ans;
}

