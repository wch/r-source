/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2021 The R Core Team
 *  Copyright (C) 2005-2021 The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
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

/* This is #included  from ./qnbinom.c , .........
*/

#define PST_0(a, b) a ## b
#define PASTE(a, b) PST_0(a, b)

#define CHR_0(x) #x
#define AS_CHAR(x) CHR_0(x)

#define _pDIST_  PASTE(p, _thisDIST_)
#define _qDIST_  PASTE(q, _thisDIST_)
/**
   For a discrete distribution on  the integers,
   For P(x) := <pDist>(x, <distPars>),  find p-quantile  y(p)   :<==>   P(y) < p <= P(y)

   @param y    current guess
   @param *z   := <pDist>(y, ..)
   @param p    target probability
   @param <distPars>  parameters of the  respective distribution function
   @param incr increment, integer valued >= 1.
   @param lower_tail  "logical" int;  if 1 (true), have lower tail probabilities; otherwise upper tail
   @param log         "logical" int;  if 1 (true) the probabilities are  log(<probability>)

   @return  root 'y'  and  z[0] = <pDist>(y, ..)
 */
#define DO_SEARCH_FUN(...)					\
    do_search(double y, double *z, double p, __VA_ARGS__,	\
              double incr, int lower_tail, int log_p)

// this is used in the caller file qnbinom.c etc :
#define DO_SEARCH_(Y_, incr_, ...)  do_search(Y_, &z, p, __VA_ARGS__, incr_, lower_tail, log_p)

#define P_DIST(Y_, ...) _pDIST_(Y_, __VA_ARGS__, lower_tail, log_p)

#ifdef MATHLIB_STANDALONE
# define MAYBE_R_CheckUserInterrupt()
#else
# define MAYBE_R_CheckUserInterrupt() R_CheckUserInterrupt()
#endif

static double DO_SEARCH_FUN(_dist_PARS_DECL_)
{
    Rboolean left = (lower_tail ? (*z >= p) : (*z < p));
    R_DBG_printf(" do_search(y=%g, z=%15.10g %s p = %15.10g) --> search to %s",
		 y, *z, (lower_tail ? ">=" : "<"), p, (left ? "left" : "right"));
    if(incr > 1)
	 R_DBG_printf(", incr = %.0f\n", incr);
    else R_DBG_printf("\n");

    if(left) {	// (lower_tail, *z >= p)  or  (upper tail, *z < p): search to the __left__
	for(int iter = 0; ; iter++) {
	    double newz = -1.; // -Wall
#ifndef MATHLIB_STANDALONE
	    if(iter % 10000 == 0) R_CheckUserInterrupt();// have seen inf.loops
#endif
	    if(y > 0)
		newz = P_DIST(y - incr, _dist_PARS_);
	    else if(y < 0)
		y = 0;
	    // note that newz may be NaN because of remaining border line bugs in _pDIST_() {bug from pbeta()}
	    if(y == 0 || ISNAN(newz) || (lower_tail ? (newz < p) : (newz >= p))) {
	 	R_DBG_printf("  new y=%.15g, " AS_CHAR(_pDIST_) "(y-incr,*) %s;"
			     " ==> search() returns previous z=%g after %d iter.\n", y,
			     ISNAN(newz) ? "is NaN" : (lower_tail ? "< p" : ">= p"), *z, iter);
		return y; // and previous *z
	    }
	    y = fmax2(0, y - incr);
	    *z = newz;
	}
    }
    else { // (lower_tail, *z < p)  or  (upper tail, *z >= p): search to the __right__
	for(int iter = 0; ; iter++) {
#ifndef MATHLIB_STANDALONE
	    if(iter % 10000 == 0) R_CheckUserInterrupt();
#endif
	    y += incr;
#ifdef _dist_MAX_y
	    if(y < _dist_MAX_y)
		*z = P_DIST(y, _dist_PARS_);
	    else if(y > _dist_MAX_y)
		y = _dist_MAX_y;
#else
	    *z = P_DIST(y, _dist_PARS_);
#endif

	    if(
#ifdef _dist_MAX_y
		y == _dist_MAX_y ||
#endif
		ISNAN(*z) || (lower_tail ? (*z >= p) : (*z < p)))
	    {
		R_DBG_printf("  new y=%.15g, z=%g = " AS_CHAR(_pDIST_) "(y,*) %s;"
			     " ==> search() returns after %d iter.\n", y, *z,
			     ISNAN(*z) ? "is NaN" : (lower_tail ? ">= p" : "< p"), iter);
		return y;
	    }
	}
    }
} // do_search()


/*
* Note : "same" code in qbinom.c, qnbinom.c __FIXME__ NOT YET for qpois() ??
* FIXME: This is far from optimal [cancellation for p ~= 1, etc]:
*/
#define q_DISCRETE_01_CHECKS() do {					\
    double p_n; /* p  in the "normal" (lower_tail=TRUE, log.p=FALSE) scale */ \
    if(!lower_tail || log_p) {						\
	p_n = R_DT_qIv(p); /* need check again (cancellation!): */	\
	R_DBG_printf("  upper tail or log_p: (p_n, 1-p_n) = (%.15g, %.15g)\n", p_n, 1-p_n); \
      /* _OLD_ (R <= 4.0.x): */						\
      /*   if (p_n == 0) return 0;		*/			\
      /*  if (p_n == 1) return ML_POSINF;	*/			\
      /* end{_OLD_} */							\
	if (p_n == 0) R_DBG_printf("** p_n == 0: _NO LONGER_ returning 0"); \
	if (p_n == 1) R_DBG_printf("** p_n == 1: _NO LONGER_ returning +Inf"); \
    } else								\
	p_n = p;							\
    /* temporary hack :  __New: On 2020-08-26,  only give message but do *NOT* early return: */	\
    /* seen (only @sfs, not 'nb-mm' (???): infinite loop for    chkQnbinom(1e-19, k.max=1e4) */	\
    if (p_n + 1.01*DBL_EPSILON >= 1.) {					\
        R_DBG_printf("p_n + 1.01*eps >= 1 ; (1-p_n = %g): for now *NO LONGER* returning Inf\n",	\
		     1-p_n);						\
	/* return ML_POSINF; */						\
    }									\
} while(0)


#ifdef _dist_MAX_y
# define q_DISCR_CHECK_BOUNDARY(Y_) do {			\
    if(Y_ > _dist_MAX_y) /* way off */ Y_ = _dist_MAX_y;	\
    else if(Y_ < 0) Y_ = 0.;					\
} while(0)
#else
# define q_DISCR_CHECK_BOUNDARY(Y_) if(Y_ < 0) Y_ = 0.
 /* e.g., for qnbinom(0.5, mu = 3, size = 1e-10) */
#endif

#define q_DISCRETE_BODY() do {						\
    /* y := approx.value (Cornish-Fisher expansion) :  */		\
    double								\
	z = qnorm(p, 0., 1., lower_tail, log_p),			\
	y = R_forceint(mu + sigma * (z + gamma * (z*z - 1) / 6));	\
    R_DBG_printf(" Cornish-Fisher: initial z = qnorm(p, l.t, log)= %g,  y = %g;\n", z,y); \
									\
    q_DISCR_CHECK_BOUNDARY(y);						\
									\
    z = P_DIST(y, _dist_PARS_);						\
									\
    /* Algorithmic "tuning parameters", used to be hardwired; changed for speed &| precision */	\
    const double							\
	_pf_n_  = 8,     /* was hardwired to 64 */			\
	_pf_L_  = 2,     /* was hardwired to 64 */			\
	_yLarge_ = 4096, /* was hardwired to 1e5 */			\
	_incF_ = (1./64),/* was hardwired to 0.001 (= 1./1000 ) */	\
	_iShrink_ = 8,   /* was hardwired to  100 */			\
	_relTol_ = 1e-15,/* was hardwired to 1e-15 */			\
	_xf_ = 4; /* extra factor, *must* be >= 1 (new anyway) */	\
									\
    R_DBG_printf(" algo. tuning: fuzz factors _pf_{n,L}: {%.0f, %.0f};  yLarge = %g\n" \
		 "      large case: _incF_=%g, _iShrink_=%g; _relTol_=%g, _xf_=%g\n", \
		 _pf_n_, _pf_L_, _yLarge_,   _incF_, _iShrink_, _relTol_, _xf_); \
									\
    /* fuzz to ensure left continuity: do not loose too much (=> error in upper tail) */ \
    if(log_p) { /* <==> p \in [-Inf, 0]  different adjustment: "other sign" */ \
	double e = _pf_L_ * DBL_EPSILON;				\
	if(lower_tail && p > - DBL_MAX) /* prevent underflow to -Inf */	\
    	    p *= 1 + e;							\
    	else /* if(p < - DBL_MIN) // not too close to -0 */		\
    	    p *= 1 - e;							\
									\
    } else { /* not log scale */					\
	double e = _pf_n_ * DBL_EPSILON;				\
	if(lower_tail)							\
	    p *= 1 - e;							\
	else if(1 - p > _xf_*e) /* otherwise get p > 1 */		\
	    p *= 1 + e;							\
    }									\
    R_DBG_printf("  new z := " AS_CHAR(_pDIST_) "(y, *) = %.11g; left-cont. fuzz => p = %.11g\n", z, p); \
									\
    /* If the C-F value  y   is not too large a simple search is OK */	\
    if(y < _yLarge_) return DO_SEARCH_(y, 1, _dist_PARS_);		\
    /* Otherwise be a bit cleverer in the search: use larger increments, notably initially: */ \
    {  /* y >= _yLarge_ */						\
	double oldincr, incr = floor(y * _incF_);			\
	int qIt = 0;							\
									\
	R_DBG_printf(" large y: --> use larger increments than 1: incr=%.0f\n", incr); \
	do {								\
	    oldincr = incr;						\
	    y = DO_SEARCH_(y, incr, _dist_PARS_); /* also updating *z */ \
	    if(++qIt % 10000 == 0) MAYBE_R_CheckUserInterrupt();	\
	    incr = fmax2(1, floor(incr / _iShrink_));			\
	} while(oldincr > 1 && incr > y * _relTol_);			\
	R_DBG_printf("  \\--> oldincr=%.0f, after %d \"outer\" search() iterations\n", \
		     oldincr, qIt);					\
	return y;							\
    }									\
} while(0)
