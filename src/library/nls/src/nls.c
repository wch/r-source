/*
 *  $Id: nls.c,v 1.1 1999/11/11 21:09:49 bates Exp $ 
 *
 *  Routines used in calculating least squares solutions in a
 *  nonlinear model in nls library for R.
 *
 *  Copyright 1999-1999 Douglas M. Bates <bates@stat.wisc.edu>,
 *                      Saikat DebRoy <saikat@stat.wisc.edu>
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
 *  License along with this program; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 */

#include "S.h"
#include "Rinternals.h"
#include <stdlib.h>

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

/*
 * get the list element named str. names is the name attribute of list
 */

static SEXP
getListElement(SEXP list, SEXP names, char *str) {
  SEXP elmt = (SEXP) NULL;
  char *tempChar;
  int i;

  for (i = 0; i < LENGTH(list); i++) {
    tempChar = CHAR(STRING(names)[i]);
    if( strcmp(tempChar,str) == 0) {
      elmt = VECTOR(list)[i];
      break;
    }
  }
  return elmt;
}

/*
 *  call to nls_iter from R -
 *  .External("nls_iter", m, control, doTrace)
 *  where m and control are nlsModel and nlsControl objects.
 *  doTrace is a logical value.  The returned value is m.
 */

SEXP
nls_iter(SEXP args) {

  double dev, fac, minFac, tolerance, newDev, convNew;
  int i, j, maxIter, hasConverged, nPars, doTrace;
  SEXP m, control, tmp, conv, incr, deviance, setPars, getPars, pars,
    newPars, newIncr, trace;

  args = CDR(args);
  m = CAR(args);
  args = CDR(args);
  control = CAR(args);
  args = CDR(args);
  doTrace = asLogical(CAR(args));
  
  PROTECT(m);

  PROTECT(control);
  if(!isNewList(control))
    error("control must be a list\n");
  if(!isNewList(m))
    error("m must be a list\n");

  PROTECT(tmp = getAttrib(control, R_NamesSymbol));

  conv = getListElement(control, tmp, "maxiter");
  if(conv == NULL || !isNumeric(conv))
    error("control$maxiter absent");
  maxIter = asInteger(conv);

  conv = getListElement(control, tmp, "tol");
  if(conv == NULL || !isNumeric(conv))
    error("control$tol absent");
  tolerance = asReal(conv);

  conv = getListElement(control, tmp, "minFactor");
  if(conv == NULL || !isNumeric(conv))
    error("control$minFactor absent");
  minFac = asReal(conv);

  UNPROTECT(2);

  PROTECT(tmp = getAttrib(m, R_NamesSymbol));

  conv = getListElement(m, tmp, "conv");
  if(conv == NULL || !isFunction(conv))
    error("m$conv() absent");
  PROTECT(conv = lang1(conv));

  incr = getListElement(m, tmp, "incr");
  if(incr == NULL || !isFunction(incr))
     error("m$incr() absent");
  PROTECT(incr = lang1(incr));

  deviance = getListElement(m, tmp, "deviance");
  if(deviance == NULL || !isFunction(deviance))
    error("m$deviance() absent");
  PROTECT(deviance = lang1(deviance));

  trace = getListElement(m, tmp, "trace");
  if(trace == NULL || !isFunction(trace))
    error("m$trace() absent");
  PROTECT(trace = lang1(trace));

  setPars = getListElement(m, tmp, "setPars");
  if(setPars == NULL || !isFunction(setPars))
    error("m$setPars() absent");
  PROTECT(setPars);

  getPars = getListElement(m, tmp, "getPars");
  if(getPars == NULL || !isFunction(getPars))
    error("m$getPars() absent");
  PROTECT(getPars = lang1(getPars));

  PROTECT(pars = eval(getPars, R_GlobalEnv));
  nPars = LENGTH(pars);
  
  dev = asReal(eval(deviance, R_GlobalEnv));
  if(doTrace) eval(trace,R_GlobalEnv); 

  fac = 1.0;
  hasConverged = FALSE;

  PROTECT(newPars = allocVector(REALSXP, nPars));
  for (i = 0; i < maxIter; i++) {
    if((convNew = asReal(eval(conv,R_GlobalEnv))) < tolerance) {
      hasConverged = TRUE;
      break;
    }
    PROTECT(newIncr = eval(incr,R_GlobalEnv));
    
    while(fac >= minFac) {
      for(j = 0; j < nPars; j++) REAL(newPars)[j] = REAL(pars)[j] +
				   fac * REAL(newIncr)[j];

      PROTECT(tmp = lang2(setPars, newPars));
      if (asLogical(eval(tmp, R_GlobalEnv))) { /* singular gradient */
	UNPROTECT(12);
        error("singular gradient");
      }
      UNPROTECT(1);

      newDev = asReal(eval(deviance, R_GlobalEnv));

      if(newDev <= dev) {
	dev = newDev;
	fac = MIN(2*fac, 1);
	tmp = newPars;
	newPars = pars;
	pars = tmp;
	break;
      }
      fac = fac*0.5;
    }
    UNPROTECT(1);
    if( fac < minFac ) {
      UNPROTECT(10);
      error("step factor reduced below minimum");
    }
    if(doTrace) eval(trace,R_GlobalEnv); 
  }

  if(!hasConverged) {
    UNPROTECT(10);
    error("maximum number of iterations exceeded");
  }

  UNPROTECT(10);
  return m;
}

/*
 *  call to numeric_deriv from R -
 *  .External("numeric_deriv", expr, theta, rho)
 *  Returns: ans
 */

SEXP
numeric_deriv(SEXP args) {

  SEXP theta, expr, rho, ans, ans_del, gradient, pars,
    gradNames, gradDims, dims, temp;
  double origPar, xx, delta, eps = sqrt(DOUBLE_EPS);
  long start, i, j, k, lengthDims, nGradNames;

  args = CDR(args);
  expr = CAR(args);
  args = CDR(args);
  theta = CAR(args);
  if(!isString(theta))
    error("theta should be of type character");
  args = CDR(args);
  rho = CAR(args);
  if(!isEnvironment(rho))
    error("rho should be an environment");

  PROTECT(pars = allocVector(VECSXP, LENGTH(theta)));

  PROTECT(ans = eval(expr, rho));
  if(!isReal(ans)) {
    temp = coerceVector(ans, REALSXP);
    UNPROTECT(1);
    PROTECT(ans = temp);
  }
  PROTECT(dims = getAttrib(ans, R_DimSymbol));
  lengthDims = length(dims);
  PROTECT(gradDims = allocVector(INTSXP, lengthDims?(lengthDims+1):2));
  for(i = 0; i < lengthDims; i++)
    INTEGER(gradDims)[i] = INTEGER(dims)[i];
  if(lengthDims == 0) {
    INTEGER(gradDims)[0] = LENGTH(ans);
    lengthDims = 1;
  }
  INTEGER(gradDims)[lengthDims] = 0;
  for(i = 0; i < LENGTH(theta); i++) {
    VECTOR(pars)[i] = findVar(install(CHAR(STRING(theta)[i])), rho);
    INTEGER(gradDims)[lengthDims] += LENGTH(VECTOR(pars)[i]);
  }
  PROTECT(gradient = allocArray(REALSXP, gradDims));
  PROTECT(gradNames = allocVector(STRSXP, INTEGER(gradDims)[lengthDims]));

  nGradNames = 0;
  for(i = 0, start = 0; i < LENGTH(theta); i++) {
    for(j = 0; j < LENGTH(VECTOR(pars)[i]); j++, start += LENGTH(ans)) {
      origPar = REAL(VECTOR(pars)[i])[j];
      xx = fabs(origPar);
      delta = (xx == 0) ? eps : xx*eps;
      REAL(VECTOR(pars)[i])[j] += delta;
      ans_del = eval(expr, rho);
      if(!isReal(ans_del)) ans_del = coerceVector(ans_del, REALSXP);
      for(k = 0; k < LENGTH(ans); k++)
	REAL(gradient)[start + k] = (REAL(ans_del)[k] -
				     REAL(ans)[k])/delta;
      REAL(VECTOR(pars)[i])[j] = origPar;
      /*      if(LENGTH(VECTOR(pars)[i]) > 1) {
	tempChar = allocString(strlen(CHAR(STRING(theta)[i])) + 2);
	sprintf(CHAR(tempChar), "%s%c", CHAR(STRING(theta)[i]), '0' + j);
	STRING(gradNames)[nGradNames] = mkChar(CHAR(tempChar));
      } else
      STRING(gradNames)[nGradNames] = STRING(theta)[i];*/

      ++nGradNames;
    }
  }
  /*  dimnames = allocVector(VECSXP, lengthDims + 1);
  VECTOR(dimnames)[lengthDims] = gradNames;
  eval(lang2(install("print"),gradNames), rho);
  setAttrib(gradient, R_DimNamesSymbol, dimnames); */
  setAttrib(ans, install("gradient"), gradient);
  UNPROTECT(6);
  return ans;
}
