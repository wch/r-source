/*
 *  Routines used in calculating least squares solutions in a
 *  nonlinear model in nls library for R.
 *
 *  Copyright 2005-2020 The R Core Team
 *  Copyright 2006      The R Foundation
 *  Copyright 1999-2001 Douglas M. Bates
 *                      Saikat DebRoy
 *
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public
 *  License along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <R.h>
#include <Rinternals.h>
#include "nls.h"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

/*
 * get the list element named str. names is the name attribute of list
 */

static SEXP
getListElement(SEXP list, SEXP names, const char *str)
{
    SEXP elmt = (SEXP) NULL;
    const char *tempChar;
    int i;

    for (i = 0; i < length(list); i++) {
	tempChar = CHAR(STRING_ELT(names, i)); /* ASCII only */
	if( strcmp(tempChar,str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    }
    return elmt;
}

/*
 * put some convergence-related information into list
 */
static SEXP
ConvInfoMsg(char* msg, int iter, int whystop, double fac,
	    double minFac, int maxIter, double convNew)
{
    const char *nms[] = {"isConv", "finIter", "finTol",
			 "stopCode", "stopMessage",  ""};
    SEXP ans;
    PROTECT(ans = mkNamed(VECSXP, nms));

    SET_VECTOR_ELT(ans, 0, ScalarLogical(whystop == 0)); /* isConv */
    SET_VECTOR_ELT(ans, 1, ScalarInteger(iter));	 /* finIter */
    SET_VECTOR_ELT(ans, 2, ScalarReal   (convNew));	 /* finTol */
    SET_VECTOR_ELT(ans, 3, ScalarInteger(whystop));      /* stopCode */
    SET_VECTOR_ELT(ans, 4, mkString(msg));               /* stopMessage */

    UNPROTECT(1);
    return ans;
}


/*
 *  call to nls_iter from R --- .Call("nls_iter", m, control, doTrace)
 *  where m and control are nlsModel and nlsControl objects
 *             doTrace is a logical value.
 *  m is modified; the return value is a "convergence-information" list.
 */
SEXP
nls_iter(SEXP m, SEXP control, SEXP doTraceArg)
{
    int doTrace = asLogical(doTraceArg);

    if(!isNewList(control))
	error(_("'control' must be a list"));
    if(!isNewList(m))
	error(_("'m' must be a list"));

    SEXP tmp, conv;
    PROTECT(tmp = getAttrib(control, R_NamesSymbol));

    conv = getListElement(control, tmp, "maxiter");
    if(conv == NULL || !isNumeric(conv))
	error(_("'%s' absent"), "control$maxiter");
    int maxIter = asInteger(conv);

    conv = getListElement(control, tmp, "tol");
    if(conv == NULL || !isNumeric(conv))
	error(_("'%s' absent"), "control$tol");
    double tolerance = asReal(conv);

    conv = getListElement(control, tmp, "minFactor");
    if(conv == NULL || !isNumeric(conv))
	error(_("'%s' absent"), "control$minFactor");
    double minFac = asReal(conv);

    conv = getListElement(control, tmp, "warnOnly");
    if(conv == NULL || !isLogical(conv))
	error(_("'%s' absent"), "control$warnOnly");
    int warnOnly = asLogical(conv);

    conv = getListElement(control, tmp, "printEval");
    if(conv == NULL || !isLogical(conv))
	error(_("'%s' absent"), "control$printEval");
    Rboolean printEval = asLogical(conv);

    // now get parts from 'm'  ---------------------------------
    tmp = getAttrib(m, R_NamesSymbol);

    conv = getListElement(m, tmp, "conv");
    if(conv == NULL || !isFunction(conv))
	error(_("'%s' absent"), "m$conv()");
    PROTECT(conv = lang1(conv));

    SEXP incr = getListElement(m, tmp, "incr");
    if(incr == NULL || !isFunction(incr))
	error(_("'%s' absent"), "m$incr()");
    PROTECT(incr = lang1(incr));

    SEXP deviance = getListElement(m, tmp, "deviance");
    if(deviance == NULL || !isFunction(deviance))
	error(_("'%s' absent"), "m$deviance()");
    PROTECT(deviance = lang1(deviance));

    SEXP trace = getListElement(m, tmp, "trace");
    if(trace == NULL || !isFunction(trace))
	error(_("'%s' absent"), "m$trace()");
    PROTECT(trace = lang1(trace));

    SEXP setPars = getListElement(m, tmp, "setPars");
    if(setPars == NULL || !isFunction(setPars))
	error(_("'%s' absent"), "m$setPars()");
    PROTECT(setPars);

    SEXP getPars = getListElement(m, tmp, "getPars");
    if(getPars == NULL || !isFunction(getPars))
	error(_("'%s' absent"), "m$getPars()");
    PROTECT(getPars = lang1(getPars));

    SEXP pars = PROTECT(eval(getPars, R_GlobalEnv));
    int nPars = LENGTH(pars);

    double dev = asReal(eval(deviance, R_GlobalEnv));
    if(doTrace) eval(trace,R_GlobalEnv);

    double fac = 1.0;
    Rboolean hasConverged = FALSE;
    SEXP newPars = PROTECT(allocVector(REALSXP, nPars));
    int evaltotCnt = 1;
    double convNew = -1. /* -Wall */;
    int i;
#define CONV_INFO_MSG(_STR_, _I_)				\
	ConvInfoMsg(_STR_, i, _I_, fac, minFac, maxIter, convNew)

#define NON_CONV_FINIS(_ID_, _MSG_)		\
    if(warnOnly) {				\
	warning(_MSG_);				\
	return CONV_INFO_MSG(_MSG_, _ID_);      \
    }						\
    else					\
	error(_MSG_);

#define NON_CONV_FINIS_1(_ID_, _MSG_, _A1_)	\
    if(warnOnly) {				\
	char msgbuf[1000];			\
	warning(_MSG_, _A1_);			\
	snprintf(msgbuf, 1000, _MSG_, _A1_);	\
	return CONV_INFO_MSG(msgbuf, _ID_);	\
    }						\
    else					\
	error(_MSG_, _A1_);

#define NON_CONV_FINIS_2(_ID_, _MSG_, _A1_, _A2_)	\
    if(warnOnly) {					\
	char msgbuf[1000];				\
	warning(_MSG_, _A1_, _A2_);			\
	snprintf(msgbuf, 1000, _MSG_, _A1_, _A2_);	\
	return CONV_INFO_MSG(msgbuf, _ID_);		\
    }							\
    else						\
	error(_MSG_, _A1_, _A2_);

    for (i = 0; i < maxIter; i++) { // ---------------------------------------------

	if((convNew = asReal(eval(conv, R_GlobalEnv))) <= tolerance) {
	    hasConverged = TRUE;
	    break;
	}

	SEXP newIncr = PROTECT(eval(incr, R_GlobalEnv));
	double
	    *par   = REAL(pars),
	    *npar  = REAL(newPars),
	    *nIncr = REAL(newIncr);
	int evalCnt = -1;
	if(printEval)
	    evalCnt = 1;

	while(fac >= minFac) { // 1-dim "line search"
	    if(printEval) {
		Rprintf("  It. %3d, fac= %11.6g, eval (no.,total): (%2d,%3d):",
			i+1, fac, evalCnt, evaltotCnt);
		evalCnt++;
		evaltotCnt++;
	    }
	    for(int j = 0; j < nPars; j++)
		npar[j] = par[j] + fac * nIncr[j];

	    PROTECT(tmp = lang2(setPars, newPars));
	    if (asLogical(eval(tmp, R_GlobalEnv))) { /* singular gradient */
		UNPROTECT(11);

		NON_CONV_FINIS(1, _("singular gradient"));
	    }
	    UNPROTECT(1);

	    double newDev = asReal(eval(deviance, R_GlobalEnv));
	    if(printEval)
		Rprintf(" new dev = %g\n", newDev);
	    if(newDev <= dev) {
		dev = newDev;
		tmp = newPars;
		newPars = pars;
		pars = tmp;
		fac = MIN(2*fac, 1);
		break;
	    } // else
	    fac /= 2.;
	}
	UNPROTECT(1);
	if(doTrace) eval(trace, R_GlobalEnv);
	if( fac < minFac ) {
	    UNPROTECT(9);
	    NON_CONV_FINIS_2(2,
			     _("step factor %g reduced below 'minFactor' of %g"),
			     fac, minFac);
	}
    }

    UNPROTECT(9);
    if(!hasConverged) {
	NON_CONV_FINIS_1(3,
			 _("number of iterations exceeded maximum of %d"),
			 maxIter);
    }
    else
	return CONV_INFO_MSG(_("converged"), 0);
}
#undef CONV_INFO_MSG
#undef NON_CONV_FINIS
#undef NON_CONV_FINIS_1
#undef NON_CONV_FINIS_2


/*
 *  call to numeric_deriv from R -
 *  .Call("numeric_deriv", expr, theta, rho, dir = 1., eps = .Machine$double.eps, central=FALSE)
 *  Returns: ans
 */
SEXP
numeric_deriv(SEXP expr, SEXP theta, SEXP rho, SEXP dir, SEXP eps_, SEXP centr)
{
    if(!isString(theta))
	error(_("'theta' should be of type character"));
    if (isNull(rho)) {
	error(_("use of NULL environment is defunct"));
	rho = R_BaseEnv;
    } else
	if(!isEnvironment(rho))
	    error(_("'rho' should be an environment"));
    int nprot = 3;
    if(TYPEOF(dir) != REALSXP) {
	PROTECT(dir = coerceVector(dir, REALSXP)); nprot++;
    }
    if(LENGTH(dir) != LENGTH(theta))
	error(_("'dir' is not a numeric vector of the correct length"));
    Rboolean central = asLogical(centr);
    if(central == NA_LOGICAL)
	error(_("'central' is NA, but must be TRUE or FALSE"));
    SEXP rho1 = PROTECT(R_NewEnv(rho, FALSE, 0));
    nprot++;
    SEXP
	pars = PROTECT(allocVector(VECSXP, LENGTH(theta))),
	ans  = PROTECT(duplicate(eval(expr, rho1)));
    double *rDir = REAL(dir),  *res = NULL; // -Wall
#define CHECK_FN_VAL(_r_, _ANS_) do {					\
    if(!isReal(_ANS_)) {						\
	SEXP temp = coerceVector(_ANS_, REALSXP);			\
	UNPROTECT(1);/*: _ANS_ *must* have been the last PROTECT() ! */ \
	PROTECT(_ANS_ = temp);						\
    }									\
    _r_ = REAL(_ANS_);							\
    for(int i = 0; i < LENGTH(_ANS_); i++) {				\
	if (!R_FINITE(_r_[i]))						\
	    error(_("Missing value or an infinity produced when evaluating the model")); \
    }									\
} while(0)

    CHECK_FN_VAL(res, ans);

    const void *vmax = vmaxget();
    int lengthTheta = 0;
    for(int i = 0; i < LENGTH(theta); i++) {
	const char *name = translateChar(STRING_ELT(theta, i));
	SEXP s_name = install(name);
	SEXP temp = findVar(s_name, rho1);
	if(isInteger(temp))
	    error(_("variable '%s' is integer, not numeric"), name);
	if(!isReal(temp))
	    error(_("variable '%s' is not numeric"), name);
	// We'll be modifying the variable, so need to make a copy PR#15849
	defineVar(s_name, temp = duplicate(temp), rho1);
	MARK_NOT_MUTABLE(temp);
	SET_VECTOR_ELT(pars, i, temp);
	lengthTheta += LENGTH(VECTOR_ELT(pars, i));
    }
    vmaxset(vmax);
    SEXP gradient = PROTECT(allocMatrix(REALSXP, LENGTH(ans), lengthTheta));
    double *grad = REAL(gradient);
    double eps = asReal(eps_); // was hardcoded sqrt(DOUBLE_EPS) { ~= 1.49e-08, typically}
    for(int start = 0, i = 0; i < LENGTH(theta); i++) {
	double *pars_i = REAL(VECTOR_ELT(pars, i));
	for(int j = 0; j < LENGTH(VECTOR_ELT(pars, i)); j++, start += LENGTH(ans)) {
	    double
		origPar = pars_i[j],
		xx = fabs(origPar),
		delta = (xx == 0) ? eps : xx*eps;
	    pars_i[j] += rDir[i] * delta;
	    SEXP ans_del = PROTECT(eval(expr, rho1));
	    double *rDel = NULL;
	    CHECK_FN_VAL(rDel, ans_del);
	    if(central) {
		pars_i[j] = origPar - rDir[i] * delta;
		SEXP ans_de2 = PROTECT(eval(expr, rho1));
		double *rD2 = NULL;
		CHECK_FN_VAL(rD2, ans_de2);
		for(int k = 0; k < LENGTH(ans); k++) {
		    grad[start + k] = rDir[i] * (rDel[k] - rD2[k])/(2 * delta);
		}
	    } else { // forward difference  (previously hardwired):
		for(int k = 0; k < LENGTH(ans); k++) {
		    grad[start + k] = rDir[i] * (rDel[k] - res[k])/delta;
		}
	    }
	    UNPROTECT(central ? 2 : 1); // ansDel & possibly ans
	    pars_i[j] = origPar;
	}
    }
    setAttrib(ans, install("gradient"), gradient);
    UNPROTECT(nprot);
    return ans;
}
