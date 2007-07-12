/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2004  the R Development Core Team
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

 * C backend of R's integrate() --- via
 *	 .External("call_dqags", ...) -> Rdqags()  -- for finite     interval
 *	 .External("call_dqagi", ...) -> Rdqagi()  -- for indefinite interval
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Applic.h> /* exporting the API , particularly */
/*--- typedef void integr_fn(double *x, int n, void *ex) ---
 * vectorizing function   f(x[1:n], ...) -> x[]  {overwriting x[]}.
 * Vectorization can be used to speed up the integrand
 * instead of calling it  n  times.
*/

/* Only these two are called via .External(.) :*/
SEXP call_dqags(SEXP args);
SEXP call_dqagi(SEXP args);

typedef struct int_struct
{
    SEXP f;    /* function */
    SEXP env;  /* where to evaluate the calls */
} int_struct, *IntStruct;


/* This is *the* ``integr_fn f'' used when called from R : */
static void Rintfn(double *x, int n, void *ex)
{
    SEXP args, resultsxp, tmp;
    int i;
    IntStruct IS = (IntStruct) ex;

    PROTECT(args = allocVector(REALSXP, n));
    for(i = 0; i < n; i++) REAL(args)[i] = x[i];

    PROTECT(tmp = lang2(IS->f , args));
    PROTECT(resultsxp = eval(tmp, IS->env));

    if(length(resultsxp) != n)
	error("evaluation of function gave a result of wrong length");
    for(i = 0; i < n; i++) {
	x[i] = REAL(resultsxp)[i];
	if(!R_FINITE(x[i]))
	    error("non-finite function value");
    }
    UNPROTECT(3);
    return;
}

SEXP call_dqags(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double lower, upper, epsabs, epsrel, result, abserr, *work;
    int neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    lower = asReal(CAR(args)); args = CDR(args);
    upper = asReal(CAR(args)); args = CDR(args);
    epsabs = asReal(CAR(args)); args = CDR(args);
    epsrel = asReal(CAR(args)); args = CDR(args);
    limit = asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc(limit, sizeof(int));
    work = (double *) R_alloc(lenw, sizeof(double));

    Rdqags(Rintfn, (void*)&is,
	   &lower, &upper, &epsabs, &epsrel, &result,
	   &abserr, &neval, &ier, &limit, &lenw, &last, iwork, work);

    PROTECT(ans = allocVector(VECSXP, 4));
    PROTECT(ansnames = allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, mkChar("value"));
    SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

SEXP call_dqagi(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double bound, epsabs, epsrel, result, abserr, *work;
    int inf, neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    bound = asReal(CAR(args)); args = CDR(args);
    inf = asInteger(CAR(args)); args = CDR(args);
    epsabs = asReal(CAR(args)); args = CDR(args);
    epsrel = asReal(CAR(args)); args = CDR(args);
    limit = asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc(limit, sizeof(int));
    work = (double *) R_alloc(lenw, sizeof(double));

    Rdqagi(Rintfn, (void*)&is, &bound,&inf,&epsabs,&epsrel,&result,
	   &abserr,&neval,&ier,&limit,&lenw,&last,iwork,work);

    PROTECT(ans = allocVector(VECSXP, 4));
    PROTECT(ansnames = allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, mkChar("value"));
    SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


/* f2c-ed translations + modifications of QUADPACK functions from here down */

static void rdqagie(integr_fn f, void *ex,
		    double *, int *, double * , double *, int *,
		    double *, double *, int *,
		    int *, double *, double *, double *, double *,
		    int *, int *);

static void rdqk15i(integr_fn f, void *ex,
		    double *, int *, double * , double *,
		    double *, double *, double *, double *);

static void rdqagse(integr_fn f, void *ex, double *, double *,
		    double *, double *, int *, double *, double *,
		    int *, int *, double *, double *, double *,
		    double *, int *, int *);

static void rdqk21(integr_fn f, void *ex,
		   double *, double *, double *, double *, double *, double *);

static void rdqpsrt(int *, int *, int *, double *, double *, int *, int *);

static void rdqelg(int *, double *, double *, double *, double *, int *);

/* Table of constant values */

static double c_b6 = 0.;
static double c_b7 = 1.;

void Rdqagi(integr_fn f, void *ex, double *bound, int *inf,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last,
	    int *iwork, double *work)
{
    int l1, l2, l3;

/*
***begin prologue  dqagi
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a3a1,h2a4a1
***keywords  automatic integrator, infinite intervals,
            general-purpose, transformation, extrapolation,
            globally adaptive
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math. & progr. div. -k.u.leuven
***purpose  the routine calculates an approximation result to a given
           integral   i = integral of f over (bound,+infinity)
           or i = integral of f over (-infinity,bound)
           or i = integral of f over (-infinity,+infinity)
           hopefully satisfying following claim for accuracy
           abs(i-result) <= max(epsabs,epsrel*abs(i)).
***description

       integration over infinite intervals
       standard fortran subroutine

       parameters
        on entry
           f      - double precision
                    function subprogram defining the integrand
                    function f(x). the actual name for f needs to be
                    declared e x t e r n a l in the driver program.

           bound  - double precision
                    finite bound of integration range
                    (has no meaning if interval is doubly-infinite)

           inf    - int
                    indicating the kind of integration range involved
                    inf = 1 corresponds to  (bound,+infinity),
                    inf = -1            to  (-infinity,bound),
                    inf = 2             to (-infinity,+infinity).

           epsabs - double precision
                    absolute accuracy requested
           epsrel - double precision
                    relative accuracy requested
                    if  epsabs <= 0
                    and epsrel < max(50*rel.mach.acc.,0.5d-28),
                    the routine will end with ier = 6.


        on return
           result - double precision
                    approximation to the integral

           abserr - double precision
                    estimate of the modulus of the absolute error,
                    which should equal or exceed abs(i-result)

           neval  - int
                    number of integrand evaluations

           ier    - int
                    ier = 0 normal and reliable termination of the
                            routine. it is assumed that the requested
                            accuracy has been achieved.
                  - ier > 0 abnormal termination of the routine. the
                            estimates for result and error are less
                            reliable. it is assumed that the requested
                            accuracy has not been achieved.
           error messages
                    ier = 1 maximum number of subdivisions allowed
                            has been achieved. one can allow more
                            subdivisions by increasing the value of
                            limit (and taking the according dimension
                            adjustments into account). however, if
                            this yields no improvement it is advised
                            to analyze the integrand in order to
                            determine the integration difficulties. if
                            the position of a local difficulty can be
                            determined (e.g. singularity,
                            discontinuity within the interval) one
                            will probably gain from splitting up the
                            interval at this point and calling the
                            integrator on the subranges. if possible,
                            an appropriate special-purpose integrator
                            should be used, which is designed for
                            handling the type of difficulty involved.
                        = 2 the occurrence of roundoff error is
                            detected, which prevents the requested
                            tolerance from being achieved.
                            the error may be under-estimated.
                        = 3 extremely bad integrand behaviour occurs
                            at some points of the integration
                            interval.
                        = 4 the algorithm does not converge.
                            roundoff error is detected in the
                            extrapolation table.
                            it is assumed that the requested tolerance
                            cannot be achieved, and that the returned
                            result is the best which can be obtained.
                        = 5 the integral is probably divergent, or
                            slowly convergent. it must be noted that
                            divergence can occur with any other value
                            of ier.
                        = 6 the input is invalid, because
                            (epsabs <= 0 and
                             epsrel < max(50*rel.mach.acc.,0.5d-28))
                             or limit < 1 or leniw < limit*4.
                            result, abserr, neval, last are set to
                            zero. exept when limit or leniw is
                            invalid, iwork(1), work(limit*2+1) and
                            work(limit*3+1) are set to zero, work(1)
                            is set to a and work(limit+1) to b.

        dimensioning parameters
           limit - int
                   dimensioning parameter for iwork
                   limit determines the maximum number of subintervals
                   in the partition of the given integration interval
                   (a,b), limit >= 1.
                   if limit < 1, the routine will end with ier = 6.

           lenw  - int
                   dimensioning parameter for work
                   lenw must be at least limit*4.
                   if lenw < limit*4, the routine will end
                   with ier = 6.

           last  - int
                   on return, last equals the number of subintervals
                   produced in the subdivision process, which
                   determines the number of significant elements
                   actually in the work arrays.

        work arrays
           iwork - int
                   vector of dimension at least limit, the first
                   k elements of which contain pointers
                   to the error estimates over the subintervals,
                   such that work(limit*3+iwork(1)),... ,
                   work(limit*3+iwork(k)) form a decreasing
                   sequence, with k = last if last <= (limit/2+2), and
                   k = limit+1-last otherwise

           work  - double precision
                   vector of dimension at least lenw
                   on return
                   work(1), ..., work(last) contain the left
                    end points of the subintervals in the
                    partition of (a,b),
                   work(limit+1), ..., work(limit+last) contain
                    the right end points,
                   work(limit*2+1), ...,work(limit*2+last) contain the
                    integral approximations over the subintervals,
                   work(limit*3+1), ..., work(limit*3)
                    contain the error estimates.
***references  (none)
***routines called  dqagie
***end prologue  dqagi */

    /* Parameter adjustments */
    --iwork;
    --work;

    /* Function Body */
    *ier = 6;
    *neval = 0;
    *last = 0;
    *result = 0.;
    *abserr = 0.;
    if (*limit < 1 || *lenw < *limit << 2) return;

    l1 = *limit + 1;
    l2 = *limit + l1;
    l3 = *limit + l2;

    rdqagie(f, ex, bound, inf, epsabs, epsrel, limit, result, abserr, neval, ier,
	    &work[1], &work[l1], &work[l2], &work[l3], &iwork[1], last);

    return;
} /* Rdqagi */

static
void rdqagie(integr_fn f, void *ex, double *bound, int *inf, double *
	     epsabs, double *epsrel, int *limit, double *result,
	     double *abserr, int *neval, int *ier, double *alist__,
	     double *blist, double *rlist, double *elist, int *
	     iord, int *last)
{
    /* System generated locals */
    double d__1, d__2;

    /* Local variables */
    double area, dres;
    int ksgn;
    double boun;
    int nres;
    double area1, area2, area12;
    int k;
    double small = 0.0, erro12;
    int ierro;
    double a1, a2, b1, b2, defab1, defab2, oflow;
    int ktmin, nrmax;
    double uflow;
    Rboolean noext;
    int iroff1, iroff2, iroff3;
    double res3la[3], error1, error2;
    int id;
    double rlist2[52];
    int numrl2;
    double defabs, epmach, erlarg = 0.0, abseps, correc = 0.0, errbnd, resabs;
    int jupbnd;
    double erlast, errmax;
    int maxerr;
    double reseps;
    Rboolean extrap;
    double ertest = 0.0, errsum;

/**begin prologue  dqagie
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a3a1,h2a4a1
***keywords  automatic integrator, infinite intervals,
            general-purpose, transformation, extrapolation,
            globally adaptive
***author  piessens,robert,appl. math & progr. div - k.u.leuven
          de doncker,elise,appl. math & progr. div - k.u.leuven
***purpose  the routine calculates an approximation result to a given
           integral   i = integral of f over (bound,+infinity)
           or i = integral of f over (-infinity,bound)
           or i = integral of f over (-infinity,+infinity),
           hopefully satisfying following claim for accuracy
           abs(i-result) <= max(epsabs,epsrel*abs(i))
***description

integration over infinite intervals
standard fortran subroutine

           f      - double precision
                    function subprogram defining the integrand
                    function f(x). the actual name for f needs to be
                    declared e x t e r n a l in the driver program.

           bound  - double precision
                    finite bound of integration range
                    (has no meaning if interval is doubly-infinite)

           inf    - double precision
                    indicating the kind of integration range involved
                    inf = 1 corresponds to  (bound,+infinity),
                    inf = -1            to  (-infinity,bound),
                    inf = 2             to (-infinity,+infinity).

           epsabs - double precision
                    absolute accuracy requested
           epsrel - double precision
                    relative accuracy requested
                    if  epsabs <= 0
                    and epsrel < max(50*rel.mach.acc.,0.5d-28),
                    the routine will end with ier = 6.

           limit  - int
                    gives an upper bound on the number of subintervals
                    in the partition of (a,b), limit >= 1

        on return
           result - double precision
                    approximation to the integral

           abserr - double precision
                    estimate of the modulus of the absolute error,
                    which should equal or exceed abs(i-result)

           neval  - int
                    number of integrand evaluations

           ier    - int
                    ier = 0 normal and reliable termination of the
                            routine. it is assumed that the requested
                            accuracy has been achieved.
                  - ier > 0 abnormal termination of the routine. the
                            estimates for result and error are less
                            reliable. it is assumed that the requested
                            accuracy has not been achieved.
           error messages
                    ier = 1 maximum number of subdivisions allowed
                            has been achieved. one can allow more
                            subdivisions by increasing the value of
                            limit (and taking the according dimension
                            adjustments into account). however,if
                            this yields no improvement it is advised
                            to analyze the integrand in order to
                            determine the integration difficulties.
                            if the position of a local difficulty can
                            be determined (e.g. singularity,
                            discontinuity within the interval) one
                            will probably gain from splitting up the
                            interval at this point and calling the
                            integrator on the subranges. if possible,
                            an appropriate special-purpose integrator
                            should be used, which is designed for
                            handling the type of difficulty involved.
                        = 2 the occurrence of roundoff error is
                            detected, which prevents the requested
                            tolerance from being achieved.
                            the error may be under-estimated.
                        = 3 extremely bad integrand behaviour occurs
                            at some points of the integration
                            interval.
                        = 4 the algorithm does not converge.
                            roundoff error is detected in the
                            extrapolation table.
                            it is assumed that the requested tolerance
                            cannot be achieved, and that the returned
                            result is the best which can be obtained.
                        = 5 the integral is probably divergent, or
                            slowly convergent. it must be noted that
                            divergence can occur with any other value
                            of ier.
                        = 6 the input is invalid, because
                            (epsabs <= 0 and
                             epsrel < max(50*rel.mach.acc.,0.5d-28),
                            result, abserr, neval, last, rlist(1),
                            elist(1) and iord(1) are set to zero.
                            alist(1) and blist(1) are set to 0
                            and 1 respectively.

           alist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the left
                    end points of the subintervals in the partition
                    of the transformed integration range (0,1).

           blist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the right
                    end points of the subintervals in the partition
                    of the transformed integration range (0,1).

           rlist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the integral
                    approximations on the subintervals

           elist  - double precision
                    vector of dimension at least limit,  the first
                    last elements of which are the moduli of the
                    absolute error estimates on the subintervals

           iord   - int
                    vector of dimension limit, the first k
                    elements of which are pointers to the
                    error estimates over the subintervals,
                    such that elist(iord(1)), ..., elist(iord(k))
                    form a decreasing sequence, with k = last
                    if last <= (limit/2+2), and k = limit+1-last
                    otherwise

           last   - int
                    number of subintervals actually produced
                    in the subdivision process

***references  (none)
***routines called  dqelg,dqk15i,dqpsrt
***end prologue  dqagie


           the dimension of rlist2 is determined by the value of
           limexp in subroutine dqelg.

           list of major variables
           -----------------------

          alist     - list of left end points of all subintervals
                      considered up to now
          blist     - list of right end points of all subintervals
                      considered up to now
          rlist(i)  - approximation to the integral over
                      (alist(i),blist(i))
          rlist2    - array of dimension at least (limexp+2),
                      containing the part of the epsilon table
                      wich is still needed for further computations
          elist(i)  - error estimate applying to rlist(i)
          maxerr    - pointer to the interval with largest error
                      estimate
          errmax    - elist(maxerr)
          erlast    - error on the interval currently subdivided
                      (before that subdivision has taken place)
          area      - sum of the integrals over the subintervals
          errsum    - sum of the errors over the subintervals
          errbnd    - requested accuracy max(epsabs,epsrel*
                      abs(result))
          *****1    - variable for the left subinterval
          *****2    - variable for the right subinterval
          last      - index for subdivision
          nres      - number of calls to the extrapolation routine
          numrl2    - number of elements currently in rlist2. if an
                      appropriate approximation to the compounded
                      integral has been obtained, it is put in
                      rlist2(numrl2) after numrl2 has been increased
                      by one.
          small     - length of the smallest interval considered up
                      to now, multiplied by 1.5
          erlarg    - sum of the errors over the intervals larger
                      than the smallest interval considered up to now
          extrap    - logical variable denoting that the routine
                      is attempting to perform extrapolation. i.e.
                      before subdividing the smallest interval we
                      try to decrease the value of erlarg.
          noext     - logical variable denoting that extrapolation
                      is no longer allowed (true-value)

           machine dependent constants
           ---------------------------

          epmach is the largest relative spacing.
          uflow is the smallest positive magnitude.
          oflow is the largest positive magnitude. */

/* ***first executable statement  dqagie */
    /* Parameter adjustments */
    --iord;
    --elist;
    --rlist;
    --blist;
    --alist__;

    /* Function Body */
    epmach = DBL_EPSILON;

/*           test on validity of parameters */
/*           ----------------------------- */

    *ier = 0;
    *neval = 0;
    *last = 0;
    *result = 0.;
    *abserr = 0.;
    alist__[1] = 0.;
    blist[1] = 1.;
    rlist[1] = 0.;
    elist[1] = 0.;
    iord[1] = 0;
    if (*epsabs <= 0. && (*epsrel < fmax2(epmach * 50., 5e-29))) *ier = 6;
    if (*ier == 6) return;

/*           first approximation to the integral */
/*           ----------------------------------- */

/*         determine the interval to be mapped onto (0,1).
           if inf = 2 the integral is computed as i = i1+i2, where
           i1 = integral of f over (-infinity,0),
           i2 = integral of f over (0,+infinity). */

    boun = *bound;
    if (*inf == 2) {
	boun = 0.;
    }
    rdqk15i(f, ex, &boun, inf, &c_b6, &c_b7, result, abserr, &defabs, &resabs);

/*           test on accuracy */

    *last = 1;
    rlist[1] = *result;
    elist[1] = *abserr;
    iord[1] = 1;
    dres = fabs(*result);
    errbnd = fmax2(*epsabs, *epsrel * dres);
    if (*abserr <= epmach * 100. * defabs && *abserr > errbnd) *ier = 2;
    if (*limit == 1) *ier = 1;
    if (*ier != 0 || (*abserr <= errbnd && *abserr != resabs)
	|| *abserr == 0.) goto L130;

/*           initialization */
/*           -------------- */

    uflow = DBL_MIN;
    oflow = DBL_MAX;
    rlist2[0] = *result;
    errmax = *abserr;
    maxerr = 1;
    area = *result;
    errsum = *abserr;
    *abserr = oflow;
    nrmax = 1;
    nres = 0;
    ktmin = 0;
    numrl2 = 2;
    extrap = FALSE;
    noext = FALSE;
    ierro = 0;
    iroff1 = 0;
    iroff2 = 0;
    iroff3 = 0;
    ksgn = -1;
    if (dres >= (1. - epmach * 50.) * defabs) {
	ksgn = 1;
    }

/*           main do-loop */
/*           ------------ */

    for (*last = 2; *last <= *limit; ++(*last)) {

/*           bisect the subinterval with nrmax-th largest error estimate. */

	a1 = alist__[maxerr];
	b1 = (alist__[maxerr] + blist[maxerr]) * .5;
	a2 = b1;
	b2 = blist[maxerr];
	erlast = errmax;
	rdqk15i(f, ex, &boun, inf, &a1, &b1, &area1, &error1, &resabs, &defab1);
	rdqk15i(f, ex, &boun, inf, &a2, &b2, &area2, &error2, &resabs, &defab2);

/*           improve previous approximations to integral
	     and error and test for accuracy. */

	area12 = area1 + area2;
	erro12 = error1 + error2;
	errsum = errsum + erro12 - errmax;
	area = area + area12 - rlist[maxerr];
	if (defab1 == error1 || defab2 == error2) {
	    goto L15;
	}
	if (fabs(rlist[maxerr] - area12) > fabs(area12) * 1e-5 ||
		 erro12 < errmax * .99) {
	    goto L10;
	}
	if (extrap) {
	    ++iroff2;
	}
	if (! extrap) {
	    ++iroff1;
	}
L10:
	if (*last > 10 && erro12 > errmax) {
	    ++iroff3;
	}
L15:
	rlist[maxerr] = area1;
	rlist[*last] = area2;
	errbnd = fmax2(*epsabs, *epsrel * fabs(area));

/*           test for roundoff error and eventually set error flag. */

	if (iroff1 + iroff2 >= 10 || iroff3 >= 20)
	    *ier = 2;
	if (iroff2 >= 5)
	    ierro = 3;

/*           set error flag in the case that the number of
	     subintervals equals limit. */

	if (*last == *limit)
	    *ier = 1;

/*           set error flag in the case of bad integrand behaviour
	     at some points of the integration range. */

	if (fmax2(fabs(a1), fabs(b2)) <=
	    (epmach * 100. + 1.) * (fabs(a2) + uflow * 1e3))
		{
	    *ier = 4;
	}

/*           append the newly-created intervals to the list. */

	if (error2 > error1) {
	    goto L20;
	}
	alist__[*last] = a2;
	blist[maxerr] = b1;
	blist[*last] = b2;
	elist[maxerr] = error1;
	elist[*last] = error2;
	goto L30;
L20:
	alist__[maxerr] = a2;
	alist__[*last] = a1;
	blist[*last] = b1;
	rlist[maxerr] = area2;
	rlist[*last] = area1;
	elist[maxerr] = error2;
	elist[*last] = error1;

/*           call subroutine dqpsrt to maintain the descending ordering
	     in the list of error estimates and select the subinterval
	     with nrmax-th largest error estimate (to be bisected next). */

L30:
	rdqpsrt(limit, last, &maxerr, &errmax, &elist[1], &iord[1], &nrmax);
	if (errsum <= errbnd) {
	    goto L115;
	}
	if (*ier != 0)	    goto L100;
	if (*last == 2)     goto L80;
	if (noext) 	    goto L90;

	erlarg -= erlast;
	if (fabs(b1 - a1) > small) {
	    erlarg += erro12;
	}
	if (extrap) {
	    goto L40;
	}

/*           test whether the interval to be bisected next is the
	     smallest interval. */

	if (fabs(blist[maxerr] - alist__[maxerr]) > small) {
	    goto L90;
	}
	extrap = TRUE;
	nrmax = 2;
L40:
	if (ierro == 3 || erlarg <= ertest) {
	    goto L60;
	}

/*           the smallest interval has the largest error.
	     before bisecting decrease the sum of the errors over the
	     larger intervals (erlarg) and perform extrapolation. */

	id = nrmax;
	jupbnd = *last;
	if (*last > *limit / 2 + 2) {
	    jupbnd = *limit + 3 - *last;
	}
	for (k = id; k <= jupbnd; ++k) {
	    maxerr = iord[nrmax];
	    errmax = elist[maxerr];
	    if (fabs(blist[maxerr] - alist__[maxerr]) > small) {
		goto L90;
	    }
	    ++nrmax;
/* L50: */
	}

/*           perform extrapolation. */

L60:
	++numrl2;
	rlist2[numrl2 - 1] = area;
	rdqelg(&numrl2, rlist2, &reseps, &abseps, res3la, &nres);
	++ktmin;
	if (ktmin > 5 && *abserr < errsum * .001) {
	    *ier = 5;
	}
	if (abseps >= *abserr) {
	    goto L70;
	}
	ktmin = 0;
	*abserr = abseps;
	*result = reseps;
	correc = erlarg;
/* Computing MAX */
	d__1 = *epsabs, d__2 = *epsrel * fabs(reseps);
	ertest = fmax2(d__1,d__2);
	if (*abserr <= ertest) {
	    goto L100;
	}

/*            prepare bisection of the smallest interval. */

L70:
	if (numrl2 == 1) {
	    noext = TRUE;
	}
	if (*ier == 5) {
	    goto L100;
	}
	maxerr = iord[1];
	errmax = elist[maxerr];
	nrmax = 1;
	extrap = FALSE;
	small *= .5;
	erlarg = errsum;
	goto L90;
L80:
	small = .375;
	erlarg = errsum;
	ertest = errbnd;
	rlist2[1] = area;
L90:
	;
    }

/*           set final result and error estimate. */
/*           ------------------------------------ */

L100:
    if (*abserr == oflow) {
	goto L115;
    }
    if (*ier + ierro == 0) {
	goto L110;
    }
    if (ierro == 3) {
	*abserr += correc;
    }
    if (*ier == 0) {
	*ier = 3;
    }
    if (*result != 0. && area != 0.) {
	goto L105;
    }
    if (*abserr > errsum) {
	goto L115;
    }
    if (area == 0.) {
	goto L130;
    }
    goto L110;
L105:
    if (*abserr / fabs(*result) > errsum / fabs(area)) {
	goto L115;
    }

/*           test on divergence */

L110:
/* Computing MAX */
    d__1 = fabs(*result), d__2 = fabs(area);
    if (ksgn == -1 && fmax2(d__1,d__2) <= defabs * .01) {
	goto L130;
    }
    if (.01 > *result / area || *result / area > 100. || errsum > fabs(area)) {
	*ier = 6;
    }
    goto L130;

/*           compute global integral sum. */

L115:
    *result = 0.;
    for (k = 1; k <= *last; ++k)
	*result += rlist[k];

    *abserr = errsum;
L130:
    *neval = *last * 30 - 15;
    if (*inf == 2) {
	*neval <<= 1;
    }
    if (*ier > 2) {
	--(*ier);
    }
    return;
} /* rdqagie_ */

void Rdqags(integr_fn f, void *ex, double *a, double *b,
	    double *epsabs, double *epsrel,
	    double *result, double *abserr, int *neval, int *ier,
	    int *limit, int *lenw, int *last, int *iwork, double *work)
{
    int l1, l2, l3;

/*
***begin prologue  dqags
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a1a1
***keywords  automatic integrator, general-purpose,
            (end-point) singularities, extrapolation,
            globally adaptive
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math. & prog. div. - k.u.leuven
***purpose  the routine calculates an approximation result to a given
           definite integral  i = integral of f over (a,b),
           hopefully satisfying following claim for accuracy
           abs(i-result) <= max(epsabs,epsrel*abs(i)).
***description

       computation of a definite integral
       standard fortran subroutine
       double precision version


       parameters
        on entry
           f      - double precision
                    function subprogram defining the integrand
                    function f(x). the actual name for f needs to be
                    declared e x t e r n a l in the driver program.

           a      - double precision
                    lower limit of integration

           b      - double precision
                    upper limit of integration

           epsabs - double precision
                    absolute accuracy requested
           epsrel - double precision
                    relative accuracy requested
                    if  epsabs <= 0
                    and epsrel < max(50*rel.mach.acc.,0.5d-28),
                    the routine will end with ier = 6.

        on return
           result - double precision
                    approximation to the integral

           abserr - double precision
                    estimate of the modulus of the absolute error,
                    which should equal or exceed abs(i-result)

           neval  - int
                    number of integrand evaluations

           ier    - int
                    ier = 0 normal and reliable termination of the
                            routine. it is assumed that the requested
                            accuracy has been achieved.
                    ier > 0 abnormal termination of the routine
                            the estimates for integral and error are
                            less reliable. it is assumed that the
                            requested accuracy has not been achieved.
           error messages
                    ier = 1 maximum number of subdivisions allowed
                            has been achieved. one can allow more sub-
                            divisions by increasing the value of limit
                            (and taking the according dimension
                            adjustments into account. however, if
                            this yields no improvement it is advised
                            to analyze the integrand in order to
                            determine the integration difficulties. if
                            the position of a local difficulty can be
                            determined (e.g. singularity,
                            discontinuity within the interval) one
                            will probably gain from splitting up the
                            interval at this point and calling the
                            integrator on the subranges. if possible,
                            an appropriate special-purpose integrator
                            should be used, which is designed for
                            handling the type of difficulty involved.
                        = 2 the occurrence of roundoff error is detec-
                            ted, which prevents the requested
                            tolerance from being achieved.
                            the error may be under-estimated.
                        = 3 extremely bad integrand behaviour
                            occurs at some points of the integration
                            interval.
                        = 4 the algorithm does not converge.
                            roundoff error is detected in the
                            extrapolation table. it is presumed that
                            the requested tolerance cannot be
                            achieved, and that the returned result is
                            the best which can be obtained.
                        = 5 the integral is probably divergent, or
                            slowly convergent. it must be noted that
                            divergence can occur with any other value
                            of ier.
                        = 6 the input is invalid, because
                            (epsabs <= 0 and
                             epsrel < max(50*rel.mach.acc.,0.5d-28)
                            or limit < 1 or lenw < limit*4.
                            result, abserr, neval, last are set to
                            zero.except when limit or lenw is invalid,
                            iwork(1), work(limit*2+1) and
                            work(limit*3+1) are set to zero, work(1)
                            is set to a and work(limit+1) to b.

        dimensioning parameters
           limit - int
                   dimensioning parameter for iwork
                   limit determines the maximum number of subintervals
                   in the partition of the given integration interval
                   (a,b), limit >= 1.
                   if limit < 1, the routine will end with ier = 6.

           lenw  - int
                   dimensioning parameter for work
                   lenw must be at least limit*4.
                   if lenw < limit*4, the routine will end
                   with ier = 6.

           last  - int
                   on return, last equals the number of subintervals
                   produced in the subdivision process, detemines the
                   number of significant elements actually in the work
                   arrays.

        work arrays
           iwork - int
                   vector of dimension at least limit, the first k
                   elements of which contain pointers
                   to the error estimates over the subintervals
                   such that work(limit*3+iwork(1)),... ,
                   work(limit*3+iwork(k)) form a decreasing
                   sequence, with k = last if last <= (limit/2+2),
                   and k = limit+1-last otherwise

           work  - double precision
                   vector of dimension at least lenw
                   on return
                   work(1), ..., work(last) contain the left
                    end-points of the subintervals in the
                    partition of (a,b),
                   work(limit+1), ..., work(limit+last) contain
                    the right end-points,
                   work(limit*2+1), ..., work(limit*2+last) contain
                    the integral approximations over the subintervals,
                   work(limit*3+1), ..., work(limit*3+last)
                    contain the error estimates.

***references  (none)
***routines called  dqagse
***end prologue  dqags */




/*         check validity of limit and lenw. */

/* ***first executable statement  dqags */
    /* Parameter adjustments */
    --iwork;
    --work;

    /* Function Body */
    *ier = 6;
    *neval = 0;
    *last = 0;
    *result = 0.;
    *abserr = 0.;
    if (*limit < 1 || *lenw < *limit *4) return;

/*         prepare call for dqagse. */

    l1 = *limit + 1;
    l2 = *limit + l1;
    l3 = *limit + l2;

    rdqagse(f, ex, a, b, epsabs, epsrel, limit, result, abserr, neval, ier,
	    &work[1], &work[l1], &work[l2], &work[l3], &iwork[1], last);

    return;
} /* rdqags_ */

static
void rdqagse(integr_fn f, void *ex, double *a, double *b, double *
	     epsabs, double *epsrel, int *limit, double *result,
	     double *abserr, int *neval, int *ier, double *alist__,
	     double *blist, double *rlist, double *elist, int *
	     iord, int *last)
{
    /* Local variables */
    Rboolean noext, extrap;
    int k,ksgn, nres;
    int ierro;
    int ktmin, nrmax;
    int iroff1, iroff2, iroff3;
    int id;
    int numrl2;
    int jupbnd;
    int maxerr;
    double res3la[3];
    double rlist2[52];
    double abseps, area, area1, area2, area12, dres, epmach;
    double a1, a2, b1, b2, defabs, defab1, defab2, oflow, uflow, resabs, reseps;
    double error1, error2, erro12, errbnd, erlast, errmax, errsum;

    double correc = 0.0, erlarg = 0.0, ertest = 0.0, small = 0.0;
/*
***begin prologue  dqagse
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a1a1
***keywords  automatic integrator, general-purpose,
            (end point) singularities, extrapolation,
            globally adaptive
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math. & progr. div. - k.u.leuven
***purpose  the routine calculates an approximation result to a given
           definite integral i = integral of f over (a,b),
           hopefully satisfying following claim for accuracy
           abs(i-result) <= max(epsabs,epsrel*abs(i)).
***description

       computation of a definite integral
       standard fortran subroutine
       double precision version

       parameters
        on entry
           f      - double precision
                    function subprogram defining the integrand
                    function f(x). the actual name for f needs to be
                    declared e x t e r n a l in the driver program.

           a      - double precision
                    lower limit of integration

           b      - double precision
                    upper limit of integration

           epsabs - double precision
                    absolute accuracy requested
           epsrel - double precision
                    relative accuracy requested
                    if  epsabs <= 0
                    and epsrel < max(50*rel.mach.acc.,0.5d-28),
                    the routine will end with ier = 6.

           limit  - int
                    gives an upperbound on the number of subintervals
                    in the partition of (a,b)

        on return
           result - double precision
                    approximation to the integral

           abserr - double precision
                    estimate of the modulus of the absolute error,
                    which should equal or exceed abs(i-result)

           neval  - int
                    number of integrand evaluations

           ier    - int
                    ier = 0 normal and reliable termination of the
                            routine. it is assumed that the requested
                            accuracy has been achieved.
                    ier > 0 abnormal termination of the routine
                            the estimates for integral and error are
                            less reliable. it is assumed that the
                            requested accuracy has not been achieved.
           error messages
                        = 1 maximum number of subdivisions allowed
                            has been achieved. one can allow more sub-
                            divisions by increasing the value of limit
                            (and taking the according dimension
                            adjustments into account). however, if
                            this yields no improvement it is advised
                            to analyze the integrand in order to
                            determine the integration difficulties. if
                            the position of a local difficulty can be
                            determined (e.g. singularity,
                            discontinuity within the interval) one
                            will probably gain from splitting up the
                            interval at this point and calling the
                            integrator on the subranges. if possible,
                            an appropriate special-purpose integrator
                            should be used, which is designed for
                            handling the type of difficulty involved.
                        = 2 the occurrence of roundoff error is detec-
                            ted, which prevents the requested
                            tolerance from being achieved.
                            the error may be under-estimated.
                        = 3 extremely bad integrand behaviour
                            occurs at some points of the integration
                            interval.
                        = 4 the algorithm does not converge.
                            roundoff error is detected in the
                            extrapolation table.
                            it is presumed that the requested
                            tolerance cannot be achieved, and that the
                            returned result is the best which can be
                            obtained.
                        = 5 the integral is probably divergent, or
                            slowly convergent. it must be noted that
                            divergence can occur with any other value
                            of ier.
                        = 6 the input is invalid, because
                            epsabs <= 0 and
                            epsrel < max(50*rel.mach.acc.,0.5d-28).
                            result, abserr, neval, last, rlist(1),
                            iord(1) and elist(1) are set to zero.
                            alist(1) and blist(1) are set to a and b
                            respectively.

           alist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the left end points
                    of the subintervals in the partition of the
                    given integration range (a,b)

           blist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the right end points
                    of the subintervals in the partition of the given
                    integration range (a,b)

           rlist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the integral
                    approximations on the subintervals

           elist  - double precision
                    vector of dimension at least limit, the first
                     last  elements of which are the moduli of the
                    absolute error estimates on the subintervals

           iord   - int
                    vector of dimension at least limit, the first k
                    elements of which are pointers to the
                    error estimates over the subintervals,
                    such that elist(iord(1)), ..., elist(iord(k))
                    form a decreasing sequence, with k = last
                    if last <= (limit/2+2), and k = limit+1-last
                    otherwise

           last   - int
                    number of subintervals actually produced in the
                    subdivision process

***references  (none)
***routines called  dqelg,dqk21,dqpsrt
***end prologue  dqagse



           the dimension of rlist2 is determined by the value of
           limexp in subroutine dqelg (rlist2 should be of dimension
           (limexp+2) at least).

           list of major variables
           -----------------------

          alist     - list of left end points of all subintervals
                      considered up to now
          blist     - list of right end points of all subintervals
                      considered up to now
          rlist(i)  - approximation to the integral over
                      (alist(i),blist(i))
          rlist2    - array of dimension at least limexp+2 containing
                      the part of the epsilon table which is still
                      needed for further computations
          elist(i)  - error estimate applying to rlist(i)
          maxerr    - pointer to the interval with largest error
                      estimate
          errmax    - elist(maxerr)
          erlast    - error on the interval currently subdivided
                      (before that subdivision has taken place)
          area      - sum of the integrals over the subintervals
          errsum    - sum of the errors over the subintervals
          errbnd    - requested accuracy max(epsabs,epsrel*
                      abs(result))
          *****1    - variable for the left interval
          *****2    - variable for the right interval
          last      - index for subdivision
          nres      - number of calls to the extrapolation routine
          numrl2    - number of elements currently in rlist2. if an
                      appropriate approximation to the compounded
                      integral has been obtained it is put in
                      rlist2(numrl2) after numrl2 has been increased
                      by one.
          small     - length of the smallest interval considered up
                      to now, multiplied by 1.5
          erlarg    - sum of the errors over the intervals larger
                      than the smallest interval considered up to now
          extrap    - logical variable denoting that the routine is
                      attempting to perform extrapolation i.e. before
                      subdividing the smallest interval we try to
                      decrease the value of erlarg.
          noext     - logical variable denoting that extrapolation
                      is no longer allowed (true value)

           machine dependent constants
           ---------------------------

          epmach is the largest relative spacing.
          uflow is the smallest positive magnitude.
          oflow is the largest positive magnitude. */

/* ***first executable statement  dqagse */
    /* Parameter adjustments */
    --iord;
    --elist;
    --rlist;
    --blist;
    --alist__;

    /* Function Body */
    epmach = DBL_EPSILON;

/*            test on validity of parameters */
/*            ------------------------------ */
    *ier = 0;
    *neval = 0;
    *last = 0;
    *result = 0.;
    *abserr = 0.;
    alist__[1] = *a;
    blist[1] = *b;
    rlist[1] = 0.;
    elist[1] = 0.;
    if (*epsabs <= 0. && *epsrel < fmax2(epmach * 50., 5e-29)) {
	*ier = 6;
	return;
    }

/*           first approximation to the integral */
/*           ----------------------------------- */

    uflow = DBL_MIN;
    oflow = DBL_MAX;
    ierro = 0;
    rdqk21(f, ex, a, b, result, abserr, &defabs, &resabs);

/*           test on accuracy. */

    dres = fabs(*result);
    errbnd = fmax2(*epsabs, *epsrel * dres);
    *last = 1;
    rlist[1] = *result;
    elist[1] = *abserr;
    iord[1] = 1;
    if (*abserr <= epmach * 100. * defabs && *abserr > errbnd)
	*ier = 2;
    if (*limit == 1)
	*ier = 1;
    if (*ier != 0 || (*abserr <= errbnd && *abserr != resabs)
	|| *abserr == 0.) goto L140;

/*           initialization */
/*           -------------- */

    rlist2[0] = *result;
    errmax = *abserr;
    maxerr = 1;
    area = *result;
    errsum = *abserr;
    *abserr = oflow;
    nrmax = 1;
    nres = 0;
    numrl2 = 2;
    ktmin = 0;
    extrap = FALSE;
    noext = FALSE;
    iroff1 = 0;
    iroff2 = 0;
    iroff3 = 0;
    ksgn = -1;
    if (dres >= (1. - epmach * 50.) * defabs) {
	ksgn = 1;
    }

/*           main do-loop */
/*           ------------ */

    for (*last = 2; *last <= *limit; ++(*last)) {

/*           bisect the subinterval with the nrmax-th largest error estimate. */

	a1 = alist__[maxerr];
	b1 = (alist__[maxerr] + blist[maxerr]) * .5;
	a2 = b1;
	b2 = blist[maxerr];
	erlast = errmax;
	rdqk21(f, ex, &a1, &b1, &area1, &error1, &resabs, &defab1);
	rdqk21(f, ex, &a2, &b2, &area2, &error2, &resabs, &defab2);

/*           improve previous approximations to integral
	     and error and test for accuracy. */

	area12 = area1 + area2;
	erro12 = error1 + error2;
	errsum = errsum + erro12 - errmax;
	area = area + area12 - rlist[maxerr];
	if (defab1 == error1 || defab2 == error2) {
	    goto L15;
	}
	if (fabs(rlist[maxerr] - area12) > fabs(area12) * 1e-5 ||
		 erro12 < errmax * .99) {
	    goto L10;
	}
	if (extrap) {
	    ++iroff2;
	}
	if (! extrap) {
	    ++iroff1;
	}
L10:
	if (*last > 10 && erro12 > errmax) {
	    ++iroff3;
	}
L15:
	rlist[maxerr] = area1;
	rlist[*last] = area2;
	errbnd = fmax2(*epsabs, *epsrel * fabs(area));

/*           test for roundoff error and eventually set error flag. */

	if (iroff1 + iroff2 >= 10 || iroff3 >= 20)
	    *ier = 2;
	if (iroff2 >= 5)
	    ierro = 3;

/* set error flag in the case that the number of subintervals equals limit. */
	if (*last == *limit)
	    *ier = 1;

/*           set error flag in the case of bad integrand behaviour
	     at a point of the integration range. */

	if (fmax2(fabs(a1), fabs(b2)) <=
	    (epmach * 100. + 1.) * (fabs(a2) + uflow * 1e3)) {
	    *ier = 4;
	}

/*           append the newly-created intervals to the list. */

	if (error2 > error1) {
	    alist__[maxerr] = a2;
	    alist__[*last] = a1;
	    blist[*last] = b1;
	    rlist[maxerr] = area2;
	    rlist[*last] = area1;
	    elist[maxerr] = error2;
	    elist[*last] = error1;
	} else {
	    alist__[*last] = a2;
	    blist[maxerr] = b1;
	    blist[*last] = b2;
	    elist[maxerr] = error1;
	    elist[*last] = error2;
	}

/*           call subroutine dqpsrt to maintain the descending ordering
	     in the list of error estimates and select the subinterval
	     with nrmax-th largest error estimate (to be bisected next). */

/*L30:*/
	rdqpsrt(limit, last, &maxerr, &errmax, &elist[1], &iord[1], &nrmax);

	if (errsum <= errbnd)   goto L115;/* ***jump out of do-loop */
	if (*ier != 0)		goto L100;/* ***jump out of do-loop */

	if (*last == 2)		goto L80;
	if (noext)		goto L90;

	erlarg -= erlast;
	if (fabs(b1 - a1) > small) {
	    erlarg += erro12;
	}
	if (extrap) {
	    goto L40;
	}

/*           test whether the interval to be bisected next is the
	     smallest interval. */

	if (fabs(blist[maxerr] - alist__[maxerr]) > small) {
	    goto L90;
	}
	extrap = TRUE;
	nrmax = 2;
L40:
	if (ierro == 3 || erlarg <= ertest) {
	    goto L60;
	}

/*           the smallest interval has the largest error.
	     before bisecting decrease the sum of the errors over the
	     larger intervals (erlarg) and perform extrapolation. */

	id = nrmax;
	jupbnd = *last;
	if (*last > *limit / 2 + 2) {
	    jupbnd = *limit + 3 - *last;
	}
	for (k = id; k <= jupbnd; ++k) {
	    maxerr = iord[nrmax];
	    errmax = elist[maxerr];
	    if (fabs(blist[maxerr] - alist__[maxerr]) > small) {
		goto L90;/* ***jump out of do-loop */
	    }
	    ++nrmax;
/* L50: */
	}

/*           perform extrapolation. */

L60:
	++numrl2;
	rlist2[numrl2 - 1] = area;
	rdqelg(&numrl2, rlist2, &reseps, &abseps, res3la, &nres);
	++ktmin;
	if (ktmin > 5 && *abserr < errsum * .001) {
	    *ier = 5;
	}
	if (abseps >= *abserr) {
	    goto L70;
	}
	ktmin = 0;
	*abserr = abseps;
	*result = reseps;
	correc = erlarg;
	ertest = fmax2(*epsabs, *epsrel * fabs(reseps));
	if (*abserr <= ertest) {
	    goto L100;/* ***jump out of do-loop */
	}

/*           prepare bisection of the smallest interval. */

L70:
	if (numrl2 == 1) {
	    noext = TRUE;
	}
	if (*ier == 5) {
	    goto L100;
	}
	maxerr = iord[1];
	errmax = elist[maxerr];
	nrmax = 1;
	extrap = FALSE;
	small *= .5;
	erlarg = errsum;
	goto L90;
L80:
	small = fabs(*b - *a) * .375;
	erlarg = errsum;
	ertest = errbnd;
	rlist2[1] = area;
L90:
	;
    }


L100:/*		set final result and error estimate. */
/*		------------------------------------ */
    if (*abserr == oflow) 	goto L115;
    if (*ier + ierro == 0) 	goto L110;
    if (ierro == 3)
	*abserr += correc;
    if (*ier == 0)
	*ier = 3;
    if (*result != 0. && area != 0.) goto L105;
    if (*abserr > errsum) 	goto L115;
    if (area == 0.) 		goto L130;
    goto L110;

L105:
    if (*abserr / fabs(*result) > errsum / fabs(area)) {
	goto L115;
    }

L110:/*		test on divergence. */
    if (ksgn == -1 && fmax2(fabs(*result), fabs(area)) <= defabs * .01) {
	goto L130;
    }
    if (.01 > *result / area || *result / area > 100. || errsum > fabs(area)) {
	*ier = 5;
    }
    goto L130;

L115:/*		compute global integral sum. */
    *result = 0.;
    for (k = 1; k <= *last; ++k)
	*result += rlist[k];
    *abserr = errsum;
L130:
    if (*ier > 2)
L140:
    *neval = *last * 42 - 21;
    return;
} /* rdqagse_ */


static void rdqk15i(integr_fn f, void *ex,
		    double *boun, int *inf, double *a, double *b,
		    double *result,
		    double *abserr, double *resabs, double *resasc)
{
    /* Initialized data */

    static double wg[8] = {
	    0., .129484966168869693270611432679082,
	    0., .27970539148927666790146777142378,
	    0., .381830050505118944950369775488975,
	    0., .417959183673469387755102040816327 };
    static double xgk[8] = {
	    .991455371120812639206854697526329,
	    .949107912342758524526189684047851,
	    .864864423359769072789712788640926,
	    .741531185599394439863864773280788,
	    .58608723546769113029414483825873,
	    .405845151377397166906606412076961,
	    .207784955007898467600689403773245, 0. };
    static double wgk[8] = {
	    .02293532201052922496373200805897,
	    .063092092629978553290700663189204,
	    .104790010322250183839876322541518,
	    .140653259715525918745189590510238,
	    .16900472663926790282658342659855,
	    .190350578064785409913256402421014,
	    .204432940075298892414161999234649,
	    .209482141084727828012999174891714 };

    /* Local variables */
    double absc, dinf, resg, resk, fsum, absc1, absc2, fval1, fval2;
    int j;
    double hlgth, centr, reskh, uflow;
    double tabsc1, tabsc2, fc, epmach;
    double fv1[7], fv2[7], vec[15], vec2[15];

/*
***begin prologue  dqk15i
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a3a2,h2a4a2
***keywords  15-point transformed gauss-kronrod rules
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math. & progr. div. - k.u.leuven
***purpose  the original (infinite integration range is mapped
           onto the interval (0,1) and (a,b) is a part of (0,1).
           it is the purpose to compute
           i = integral of transformed integrand over (a,b),
           j = integral of abs(transformed integrand) over (a,b).
***description

          integration rule
          standard fortran subroutine
          double precision version

          parameters
           on entry
             f      - double precision
                      fuction subprogram defining the integrand
                      function f(x). the actual name for f needs to be
                      declared e x t e r n a l in the calling program.

             boun   - double precision
                      finite bound of original integration
                      range (set to zero if inf = +2)

             inf    - int
                      if inf = -1, the original interval is
                                  (-infinity,bound),
                      if inf = +1, the original interval is
                                  (bound,+infinity),
                      if inf = +2, the original interval is
                                  (-infinity,+infinity) and
                      the integral is computed as the sum of two
                      integrals, one over (-infinity,0) and one over
                      (0,+infinity).

             a      - double precision
                      lower limit for integration over subrange
                      of (0,1)

             b      - double precision
                      upper limit for integration over subrange
                      of (0,1)

           on return
             result - double precision
                      approximation to the integral i
                      result is computed by applying the 15-point
                      kronrod rule(resk) obtained by optimal addition
                      of abscissae to the 7-point gauss rule(resg).

             abserr - double precision
                      estimate of the modulus of the absolute error,
                      which should equal or exceed abs(i-result)

             resabs - double precision
                      approximation to the integral j

             resasc - double precision
                      approximation to the integral of
                      abs((transformed integrand)-i/(b-a)) over (a,b)

***references  (none)
***end prologue  dqk15i


          the abscissae and weights are supplied for the interval
          (-1,1).  because of symmetry only the positive abscissae and
          their corresponding weights are given.

          xgk    - abscissae of the 15-point kronrod rule
                   xgk(2), xgk(4), ... abscissae of the 7-point
                   gauss rule
                   xgk(1), xgk(3), ...  abscissae which are optimally
                   added to the 7-point gauss rule

          wgk    - weights of the 15-point kronrod rule

          wg     - weights of the 7-point gauss rule, corresponding
                   to the abscissae xgk(2), xgk(4), ...
                   wg(1), wg(3), ... are set to zero.





          list of major variables
          -----------------------

          centr  - mid point of the interval
          hlgth  - half-length of the interval
          absc*  - abscissa
          tabsc* - transformed abscissa
          fval*  - function value
          resg   - result of the 7-point gauss formula
          resk   - result of the 15-point kronrod formula
          reskh  - approximation to the mean value of the transformed
                   integrand over (a,b), i.e. to i/(b-a)

          machine dependent constants
          ---------------------------

          epmach is the largest relative spacing.
          uflow is the smallest positive magnitude.
*/

/* ***first executable statement  dqk15i */
    epmach = DBL_EPSILON;
    uflow = DBL_MIN;
    dinf = (double) imin2(1, *inf);

    centr = (*a + *b) * .5;
    hlgth = (*b - *a) * .5;
    tabsc1 = *boun + dinf * (1. - centr) / centr;
    vec[0] = tabsc1;
    if (*inf == 2) {
	vec2[0] = -tabsc1;
    }
    for (j = 1; j <= 7; ++j) {
	absc = hlgth * xgk[j - 1];
	absc1 = centr - absc;
	absc2 = centr + absc;
	tabsc1 = *boun + dinf * (1. - absc1) / absc1;
	tabsc2 = *boun + dinf * (1. - absc2) / absc2;
	vec[(j << 1) - 1] = tabsc1;
	vec[j * 2] = tabsc2;
	if (*inf == 2) {
	    vec2[(j << 1) - 1] = -tabsc1;
	    vec2[j * 2] = -tabsc2;
	}
/* L5: */
    }
    f(vec, 15, ex); /* -> new vec[] overwriting old vec[] */
    if (*inf == 2) f(vec2, 15, ex);
    fval1 = vec[0];
    if (*inf == 2) fval1 += vec2[0];
    fc = fval1 / centr / centr;

/*           compute the 15-point kronrod approximation to
	     the integral, and estimate the error. */

    resg = wg[7] * fc;
    resk = wgk[7] * fc;
    *resabs = fabs(resk);
    for (j = 1; j <= 7; ++j) {
	absc = hlgth * xgk[j - 1];
	absc1 = centr - absc;
	absc2 = centr + absc;
	tabsc1 = *boun + dinf * (1. - absc1) / absc1;
	tabsc2 = *boun + dinf * (1. - absc2) / absc2;
	fval1 = vec[(j << 1) - 1];
	fval2 = vec[j * 2];
	if (*inf == 2) {
	    fval1 += vec2[(j << 1) - 1];
	}
	if (*inf == 2) {
	    fval2 += vec2[j * 2];
	}
	fval1 = fval1 / absc1 / absc1;
	fval2 = fval2 / absc2 / absc2;
	fv1[j - 1] = fval1;
	fv2[j - 1] = fval2;
	fsum = fval1 + fval2;
	resg += wg[j - 1] * fsum;
	resk += wgk[j - 1] * fsum;
	*resabs += wgk[j - 1] * (fabs(fval1) + fabs(fval2));
/* L10: */
    }
    reskh = resk * .5;
    *resasc = wgk[7] * fabs(fc - reskh);
    for (j = 1; j <= 7; ++j) {
	*resasc += wgk[j - 1] * (fabs(fv1[j - 1] - reskh) +
				 fabs(fv2[j - 1] - reskh));
/* L20: */
    }
    *result = resk * hlgth;
    *resasc *= hlgth;
    *resabs *= hlgth;
    *abserr = fabs((resk - resg) * hlgth);
    if (*resasc != 0. && *abserr != 0.) {
	*abserr = *resasc * fmin2(1., pow(*abserr * 200. / *resasc, 1.5));
    }
    if (*resabs > uflow / (epmach * 50.)) {
	*abserr = fmax2(epmach * 50. * *resabs, *abserr);
    }
    return;
} /* rdqk15i_ */

static void rdqelg(int *n, double *epstab, double *
		   result, double *abserr, double *res3la, int *nres)
{
    /* Local variables */
    int i__, indx, ib, ib2, ie, k1, k2, k3, num, newelm, limexp;
    double delta1, delta2, delta3, e0, e1, e1abs, e2, e3, epmach, epsinf;
    double oflow, ss, res;
    double errA, err1, err2, err3, tol1, tol2, tol3;

/* ***begin prologue  dqelg
***refer to  dqagie,dqagoe,dqagpe,dqagse
***revision date  830518   (yymmdd)
***keywords  epsilon algorithm, convergence acceleration,
            extrapolation
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math & progr. div. - k.u.leuven
***purpose  the routine determines the limit of a given sequence of
           approximations, by means of the epsilon algorithm of
           p.wynn. an estimate of the absolute error is also given.
           the condensed epsilon table is computed. only those
           elements needed for the computation of the next diagonal
           are preserved.
***description

          epsilon algorithm
          standard fortran subroutine
          double precision version

          parameters
             n      - int
                      epstab(n) contains the new element in the
                      first column of the epsilon table.

             epstab - double precision
                      vector of dimension 52 containing the elements
                      of the two lower diagonals of the triangular
                      epsilon table. the elements are numbered
                      starting at the right-hand corner of the
                      triangle.

             result - double precision
                      resulting approximation to the integral

             abserr - double precision
                      estimate of the absolute error computed from
                      result and the 3 previous results

             res3la - double precision
                      vector of dimension 3 containing the last 3
                      results

             nres   - int
                      number of calls to the routine
                      (should be zero at first call)

***end prologue  dqelg


          list of major variables
          -----------------------

          e0     - the 4 elements on which the computation of a new
          e1       element in the epsilon table is based
          e2
          e3                 e0
                       e3    e1    new
                             e2

          newelm - number of elements to be computed in the new diagonal
          errA   - errA = abs(e1-e0)+abs(e2-e1)+abs(new-e2)
          result - the element in the new diagonal with least value of errA

          machine dependent constants
          ---------------------------

          epmach is the largest relative spacing.
          oflow is the largest positive magnitude.
          limexp is the maximum number of elements the epsilon
          table can contain. if this number is reached, the upper
          diagonal of the epsilon table is deleted. */

/* ***first executable statement  dqelg */
    /* Parameter adjustments */
    --res3la;
    --epstab;

    /* Function Body */
    epmach = DBL_EPSILON;
    oflow = DBL_MAX;
    ++(*nres);
    *abserr = oflow;
    *result = epstab[*n];
    if (*n < 3) {
	goto L100;
    }
    limexp = 50;
    epstab[*n + 2] = epstab[*n];
    newelm = (*n - 1) / 2;
    epstab[*n] = oflow;
    num = *n;
    k1 = *n;
    for (i__ = 1; i__ <= newelm; ++i__) {
	k2 = k1 - 1;
	k3 = k1 - 2;
	res = epstab[k1 + 2];
	e0 = epstab[k3];
	e1 = epstab[k2];
	e2 = res;
	e1abs = fabs(e1);
	delta2 = e2 - e1;
	err2 = fabs(delta2);
	tol2 = fmax2(fabs(e2), e1abs) * epmach;
	delta3 = e1 - e0;
	err3 = fabs(delta3);
	tol3 = fmax2(e1abs, fabs(e0)) * epmach;
	if (err2 <= tol2 && err3 <= tol3) {
	    /*           if e0, e1 and e2 are equal to within machine
			 accuracy, convergence is assumed. */
	    *result = res;/*		result = e2 */
	    *abserr = err2 + err3;/*	abserr = fabs(e1-e0)+fabs(e2-e1) */

	    goto L100;	/* ***jump out of do-loop */
	}

	e3 = epstab[k1];
	epstab[k1] = e1;
	delta1 = e1 - e3;
	err1 = fabs(delta1);
	tol1 = fmax2(e1abs, fabs(e3)) * epmach;

/*           if two elements are very close to each other, omit
	     a part of the table by adjusting the value of n */

	if (err1 > tol1 && err2 > tol2 && err3 > tol3) {
	    ss = 1. / delta1 + 1. / delta2 - 1. / delta3;
	    epsinf = fabs(ss * e1);

/*           test to detect irregular behaviour in the table, and
	     eventually omit a part of the table adjusting the value of n. */

	    if (epsinf > 1e-4) {
		goto L30;
	    }
	}

	*n = i__ + i__ - 1;
	goto L50;/* ***jump out of do-loop */


L30:/* compute a new element and eventually adjust the value of result. */

	res = e1 + 1. / ss;
	epstab[k1] = res;
	k1 += -2;
	errA = err2 + fabs(res - e2) + err3;
	if (errA <= *abserr) {
	    *abserr = errA;
	    *result = res;
	}
    }

/*           shift the table. */

L50:
    if (*n == limexp) {
	*n = (limexp / 2 << 1) - 1;
    }

    if (num / 2 << 1 == num) ib = 2; else ib = 1;
    ie = newelm + 1;
    for (i__ = 1; i__ <= ie; ++i__) {
	ib2 = ib + 2;
	epstab[ib] = epstab[ib2];
	ib = ib2;
    }
    if (num != *n) {
	indx = num - *n + 1;
	for (i__ = 1; i__ <= *n; ++i__) {
	    epstab[i__] = epstab[indx];
	    ++indx;
	}
    }
    /*L80:*/
    if (*nres >= 4) {
	/* L90: */
	*abserr = fabs(*result - res3la[3]) +
	          fabs(*result - res3la[2]) +
	          fabs(*result - res3la[1]);
	res3la[1] = res3la[2];
	res3la[2] = res3la[3];
	res3la[3] = *result;
    } else {
	res3la[*nres] = *result;
	*abserr = oflow;
    }

L100:/* compute error estimate */
    *abserr = fmax2(*abserr, epmach * 5. * fabs(*result));
    return;
} /* rdqelg_ */

static void  rdqk21(integr_fn f, void *ex, double *a, double *b, double *result,
		    double *abserr, double *resabs, double *resasc)
{
    /* Initialized data */

    static double wg[5] = { .066671344308688137593568809893332,
	    .149451349150580593145776339657697,
	    .219086362515982043995534934228163,
	    .269266719309996355091226921569469,
	    .295524224714752870173892994651338 };
    static double xgk[11] = { .995657163025808080735527280689003,
	    .973906528517171720077964012084452,
	    .930157491355708226001207180059508,
	    .865063366688984510732096688423493,
	    .780817726586416897063717578345042,
	    .679409568299024406234327365114874,
	    .562757134668604683339000099272694,
	    .433395394129247190799265943165784,
	    .294392862701460198131126603103866,
	    .14887433898163121088482600112972,0. };
    static double wgk[11] = { .011694638867371874278064396062192,
	    .03255816230796472747881897245939,
	    .05475589657435199603138130024458,
	    .07503967481091995276704314091619,
	    .093125454583697605535065465083366,
	    .109387158802297641899210590325805,
	    .123491976262065851077958109831074,
	    .134709217311473325928054001771707,
	    .142775938577060080797094273138717,
	    .147739104901338491374841515972068,
	    .149445554002916905664936468389821 };


    /* Local variables */
    double fv1[10], fv2[10], vec[21];
    double absc, resg, resk, fsum, fval1, fval2;
    double hlgth, centr, reskh, uflow;
    double fc, epmach, dhlgth;
    int j, jtw, jtwm1;

/* ***begin prologue  dqk21
***date written   800101   (yymmdd)
***revision date  830518   (yymmdd)
***category no.  h2a1a2
***keywords  21-point gauss-kronrod rules
***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
          de doncker,elise,appl. math. & progr. div. - k.u.leuven
***purpose  to compute i = integral of f over (a,b), with error
                          estimate
                      j = integral of abs(f) over (a,b)
***description

          integration rules
          standard fortran subroutine
          double precision version

          parameters
           on entry
             f      - double precision
                      function subprogram defining the integrand
                      function f(x). the actual name for f needs to be
                      declared e x t e r n a l in the driver program.

             a      - double precision
                      lower limit of integration

             b      - double precision
                      upper limit of integration

           on return
             result - double precision
                      approximation to the integral i
                      result is computed by applying the 21-point
                      kronrod rule (resk) obtained by optimal addition
                      of abscissae to the 10-point gauss rule (resg).

             abserr - double precision
                      estimate of the modulus of the absolute error,
                      which should not exceed abs(i-result)

             resabs - double precision
                      approximation to the integral j

             resasc - double precision
                      approximation to the integral of abs(f-i/(b-a))
                      over (a,b)

***references  (none)
***end prologue  dqk21



          the abscissae and weights are given for the interval (-1,1).
          because of symmetry only the positive abscissae and their
          corresponding weights are given.

          xgk    - abscissae of the 21-point kronrod rule
                   xgk(2), xgk(4), ...  abscissae of the 10-point
                   gauss rule
                   xgk(1), xgk(3), ...  abscissae which are optimally
                   added to the 10-point gauss rule

          wgk    - weights of the 21-point kronrod rule

          wg     - weights of the 10-point gauss rule


gauss quadrature weights and kronron quadrature abscissae and weights
as evaluated with 80 decimal digit arithmetic by l. w. fullerton,
bell labs, nov. 1981.





          list of major variables
          -----------------------

          centr  - mid point of the interval
          hlgth  - half-length of the interval
          absc   - abscissa
          fval*  - function value
          resg   - result of the 10-point gauss formula
          resk   - result of the 21-point kronrod formula
          reskh  - approximation to the mean value of f over (a,b),
                   i.e. to i/(b-a)


          machine dependent constants
          ---------------------------

          epmach is the largest relative spacing.
          uflow is the smallest positive magnitude. */

/* ***first executable statement  dqk21 */
    epmach = DBL_EPSILON;
    uflow = DBL_MIN;

    centr = (*a + *b) * .5;
    hlgth = (*b - *a) * .5;
    dhlgth = fabs(hlgth);

/*           compute the 21-point kronrod approximation to
	     the integral, and estimate the absolute error. */

    resg = 0.;
    vec[0] = centr;
    for (j = 1; j <= 5; ++j) {
	jtw = j << 1;
	absc = hlgth * xgk[jtw - 1];
	vec[(j << 1) - 1] = centr - absc;
/* L5: */
	vec[j * 2] = centr + absc;
    }
    for (j = 1; j <= 5; ++j) {
	jtwm1 = (j << 1) - 1;
	absc = hlgth * xgk[jtwm1 - 1];
	vec[(j << 1) + 9] = centr - absc;
	vec[(j << 1) + 10] = centr + absc;
    }
    f(vec, 21, ex);
    fc = vec[0];
    resk = wgk[10] * fc;
    *resabs = fabs(resk);
    for (j = 1; j <= 5; ++j) {
	jtw = j << 1;
	absc = hlgth * xgk[jtw - 1];
	fval1 = vec[(j << 1) - 1];
	fval2 = vec[j * 2];
	fv1[jtw - 1] = fval1;
	fv2[jtw - 1] = fval2;
	fsum = fval1 + fval2;
	resg += wg[j - 1] * fsum;
	resk += wgk[jtw - 1] * fsum;
	*resabs += wgk[jtw - 1] * (fabs(fval1) + fabs(fval2));
/* L10: */
    }
    for (j = 1; j <= 5; ++j) {
	jtwm1 = (j << 1) - 1;
	absc = hlgth * xgk[jtwm1 - 1];
	fval1 = vec[(j << 1) + 9];
	fval2 = vec[(j << 1) + 10];
	fv1[jtwm1 - 1] = fval1;
	fv2[jtwm1 - 1] = fval2;
	fsum = fval1 + fval2;
	resk += wgk[jtwm1 - 1] * fsum;
	*resabs += wgk[jtwm1 - 1] * (fabs(fval1) + fabs(fval2));
/* L15: */
    }
    reskh = resk * .5;
    *resasc = wgk[10] * fabs(fc - reskh);
    for (j = 1; j <= 10; ++j) {
	*resasc += wgk[j - 1] * (fabs(fv1[j - 1] - reskh) +
				 fabs(fv2[j - 1] - reskh));
/* L20: */
    }
    *result = resk * hlgth;
    *resabs *= dhlgth;
    *resasc *= dhlgth;
    *abserr = fabs((resk - resg) * hlgth);
    if (*resasc != 0. && *abserr != 0.) {
	*abserr = *resasc * fmin2(1., pow(*abserr * 200. / *resasc, 1.5));
    }
    if (*resabs > uflow / (epmach * 50.)) {
	*abserr = fmax2(epmach * 50. * *resabs, *abserr);
    }
    return;
} /* rdqk21_ */

static void rdqpsrt(int *limit, int *last, int *maxerr,
		    double *ermax, double *elist, int *iord, int *nrmax)
{
    /* Local variables */
    int i, j, k, ido, jbnd, isucc, jupbn;
    double errmin, errmax;

/* ***begin prologue  dqpsrt
 ***refer to  dqage,dqagie,dqagpe,dqawse
 ***routines called  (none)
 ***revision date  810101   (yymmdd)
 ***keywords  sequential sorting
 ***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
           de doncker,elise,appl. math. & progr. div. - k.u.leuven
 ***purpose  this routine maintains the descending ordering in the
            list of the local error estimated resulting from the
            interval subdivision process. at each call two error
            estimates are inserted using the sequential search
            method, top-down for the largest error estimate and
            bottom-up for the smallest error estimate.
 ***description

           ordering routine
           standard fortran subroutine
           double precision version

           parameters (meaning at output)
              limit  - int
                       maximum number of error estimates the list
                       can contain

              last   - int
                       number of error estimates currently in the list

              maxerr - int
                       maxerr points to the nrmax-th largest error
                       estimate currently in the list

              ermax  - double precision
                       nrmax-th largest error estimate
                       ermax = elist(maxerr)

              elist  - double precision
                       vector of dimension last containing
                       the error estimates

              iord   - int
                       vector of dimension last, the first k elements
                       of which contain pointers to the error
                       estimates, such that
                       elist(iord(1)),...,  elist(iord(k))
                       form a decreasing sequence, with
                       k = last if last <= (limit/2+2), and
                       k = limit+1-last otherwise

              nrmax  - int
                       maxerr = iord(nrmax)

***end prologue  dqpsrt
*/


    /* Parameter adjustments */
    --iord;
    --elist;

    /* Function Body */

/*           check whether the list contains more than
	     two error estimates. */
    if (*last <= 2) {
	iord[1] = 1;
	iord[2] = 2;
	goto Last;
    }
/*           this part of the routine is only executed if, due to a
	     difficult integrand, subdivision increased the error
	     estimate. in the normal case the insert procedure should
	     start after the nrmax-th largest error estimate. */

    errmax = elist[*maxerr];
    if (*nrmax > 1) {
	ido = *nrmax - 1;
	for (i = 1; i <= ido; ++i) {
	    isucc = iord[*nrmax - 1];
	    if (errmax <= elist[isucc])
		break; /* out of for-loop */
	    iord[*nrmax] = isucc;
	    --(*nrmax);
	    /* L20: */
	}
    }

/*L30:       compute the number of elements in the list to be maintained
	     in descending order. this number depends on the number of
	     subdivisions still allowed. */
    if (*last > *limit / 2 + 2)
	jupbn = *limit + 3 - *last;
    else
	jupbn = *last;

    errmin = elist[*last];

/*           insert errmax by traversing the list top-down,
	     starting comparison from the element elist(iord(nrmax+1)). */

    jbnd = jupbn - 1;
    for (i = *nrmax + 1; i <= jbnd; ++i) {
	isucc = iord[i];
	if (errmax >= elist[isucc]) {/* ***jump out of do-loop */
	    /* L60: insert errmin by traversing the list bottom-up. */
	    iord[i - 1] = *maxerr;
	    for (j = i, k = jbnd; j <= jbnd; j++, k--) {
		isucc = iord[k];
		if (errmin < elist[isucc]) {
		    /* goto L80; ***jump out of do-loop */
		    iord[k + 1] = *last;
		    goto Last;
		}
		iord[k + 1] = isucc;
	    }
	    iord[i] = *last;
	    goto Last;
	}
	iord[i - 1] = isucc;
    }

    iord[jbnd] = *maxerr;
    iord[jupbn] = *last;

Last:/* set maxerr and ermax. */

    *maxerr = iord[*nrmax];
    *ermax = elist[*maxerr];
    return;
} /* rdqpsrt_ */
