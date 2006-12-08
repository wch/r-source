/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2006	    The R Development Core Team.
 *  Copyright (C) 2003-4       	    The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(HAVE_GLIBC2)
/* for matherr etc */
# define _SVID_SOURCE 1
#endif

#ifdef __OpenBSD__
/* for definition of "struct exception" in math.h */
# define __LIBM_PRIVATE
#endif
#include "Defn.h"		/*-> Arith.h -> math.h */
#ifdef __OpenBSD__
# undef __LIBM_PRIVATE
#endif

#define MATHLIB_PRIVATE
#include <Rmath.h>
#undef MATHLIB_PRIVATE
#include "arithmetic.h"

#ifdef HAVE_MATHERR

/* Override the SVID matherr function:
   the main difference here is not to print warnings.
 */
#ifndef __cplusplus
int matherr(struct exception *exc)
{
    switch (exc->type) {
    case DOMAIN:
    case SING:
	errno = EDOM;
	break;
    case OVERFLOW:
	errno = ERANGE;
	break;
    case UNDERFLOW:
	exc->retval = 0.0;
	break;
	/*
	   There are cases TLOSS and PLOSS which are ignored here.
	   According to the Solaris man page, there are for
	   trigonometric algorithms and not needed for good ones.
	 */
    }
    return 1;
}
#endif
#endif

#ifndef _AIX
const double R_Zero_Hack = 0.0;	/* Silence the Sun compiler */
#else
double R_Zero_Hack = 0.0;
#endif
typedef union
{
    double value;
    unsigned int word[2];
} ieee_double;

/* These variables hw and lw are only used if IEEE_754 is defined.
   The value of each is fixed once we determine the endianness
   of the machine, and this can be done via WORDS_BIGENDIAN.

   Earlier code used to use establish_endianness()
   to compute these, but this is uncessary and makes them
   non-constant, but initialize once. This would involve
   a song and dance in a threaded application (e.g pthread_once(),
   etc.).
 */

/* gcc has problems with static const on AIX and Solaris
   Solaris is for gcc 3.1 and 3.2 under -O2 32-bit on 64-bit kernel */
#ifdef _AIX
#define CONST
#elif defined(sparc) && defined (__GNUC__) && __GNUC__ == 3
#define CONST
#else
#define CONST const
#endif

#ifdef WORDS_BIGENDIAN
static CONST int hw = 0;
static CONST int lw = 1;
#else  /* !WORDS_BIGENDIAN */
static CONST int hw = 1;
static CONST int lw = 0;
#endif /* WORDS_BIGENDIAN */


static double R_ValueOfNA(void)
{
    /* The gcc shipping with RedHat 9 gets this wrong without
     * the volatile declaration. Thanks to Marc Schwartz. */
    volatile ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

int R_IsNA(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

int R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] != 1954);
    }
    return 0;
}

/* ISNAN uses isnan, which is undefined by C++ headers
   This workaround is called only when ISNAN() is used
   in a user code in a file with __cplusplus defined */

int R_isnancpp(double x)
{
   return (isnan(x)!=0);
}


/* <FIXME> Simplify this mess.  Not used inside R
   if isfinite works, and if finite works only in packages */
int R_finite(double x)
{
#ifdef HAVE_WORKING_ISFINITE
    return isfinite(x);
#elif HAVE_WORKING_FINITE
    return finite(x);
#else
/* neither finite nor isfinite work. Do we really need the AIX exception? */
# ifdef _AIX
#  include <fp.h>
     return FINITE(x);
# else
    return (!isnan(x) & (x != R_PosInf) & (x != R_NegInf));
# endif
#endif
}


/* Arithmetic Initialization */

void attribute_hidden InitArithmetic()
{
    R_NaInt = INT_MIN;
    R_NaN = 0.0/R_Zero_Hack;
    R_NaReal = R_ValueOfNA();
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
}

/* Keep these two in step */
static double myfmod(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return R_NaN;
    tmp = x1 - floor(q) * x2;
    if(R_FINITE(q) && (fabs(q) > 1/R_AccuracyInfo.eps))
	warning(_("probable complete loss of accuracy in modulus"));
    q = floor(tmp/x2);
    return tmp - q * x2;
}

static double myfloor(double x1, double x2)
{
    double q = x1 / x2, tmp;

    if (x2 == 0.0) return q;
    tmp = x1 - floor(q) * x2;
    return floor(q) + floor(tmp/x2);
}


#ifdef HAVE_WORKING_LOG
# define R_log	log
#else
double R_log(double x) { return(x > 0 ? log(x) : x < 0 ? R_NaN : R_NegInf); }
#endif

#ifdef POW_DIRTY

# define R_pow	pow

#else
double R_pow(double x, double y) /* = x ^ y */
{
    if(x == 1. || y == 0.)
	return(1.);
    if(x == 0.) {
	if(y > 0.) return(0.);
	/* y < 0 */return(R_PosInf);
    }
    if (R_FINITE(x) && R_FINITE(y)){
      if (y == 2.0)  /* common special case */
	return x*x;
      else if (y == 0.5)  /* another common special case */
	return sqrt(x);
      else
	return pow(x,y);
    }
    if (ISNAN(x) || ISNAN(y))
	return(x + y);
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return((y < 0.)? 0. : R_PosInf);
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return((y < 0.) ? 0. : (myfmod(y,2.) ? x  : -x));
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return((x >= 1)? R_PosInf : 0.);
	    else		/* y == -Inf */
		return((x < 1) ? R_PosInf : 0.);
	}
    }
    return(R_NaN);		/* all other cases: (-Inf)^{+-Inf,
				   non-int}; (neg)^{+-Inf} */
}
#endif

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;
    if (n != 0) {
	if (!R_FINITE(x)) return R_pow(x, (double)n);
	if (n < 0) { n = -n; x = 1/x; }
	for(;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
    }
    return xn;
}


/* General Base Logarithms */

static double logbase(double x, double base)
{
#if defined(HAVE_WORKING_LOG)  && defined(HAVE_LOG10)
    if(base == 10) return log10(x);
#endif
#if defined(HAVE_WORKING_LOG)  && defined(HAVE_LOG2)
    if(base == 2) return log2(x);
#endif
    return R_log(x) / log(base);
}

SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
static SEXP integer_unary(ARITHOP_TYPE, SEXP);
static SEXP real_unary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP real_binary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_binary(ARITHOP_TYPE, SEXP, SEXP, SEXP);

#if 0
static int naflag;
static SEXP lcall;
#endif


/* Unary and Binary Operators */

SEXP attribute_hidden do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    switch (length(args)) {
    case 1:
	return R_unary(call, op, CAR(args));
    case 2:
	return R_binary(call, op, CAR(args), CADR(args));
    default:
	error(_("operator needs one or two arguments"));
    }
    return ans;			/* never used; to keep -Wall happy */
}

#define COERCE_IF_NEEDED(v, tp, vpi) do { \
    if (TYPEOF(v) != (tp)) { \
        int __vo__ = OBJECT(v); \
	REPROTECT(v = coerceVector(v, (tp)), vpi); \
	if (__vo__) SET_OBJECT(v, 1); \
    } \
} while (0)

#define FIXUP_NULL_AND_CHECK_TYPES(v, vpi) do { \
    switch (TYPEOF(v)) { \
    case NILSXP: REPROTECT(v = allocVector(REALSXP,0), vpi); break; \
    case CPLXSXP: case REALSXP: case INTSXP: case LGLSXP: break; \
    default: errorcall(lcall, _("non-numeric argument to binary operator")); \
    } \
} while (0)

SEXP attribute_hidden R_binary(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP klass, dims, tsp, xnames, ynames, val;
    int mismatch = 0, nx, ny, xarray, yarray, xts, yts;
    int xattr, yattr;
    SEXP lcall = call;
    PROTECT_INDEX xpi, ypi;
    ARITHOP_TYPE oper = (ARITHOP_TYPE) PRIMVAL(op);
    int nprotect = 2; /* x and y */


    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    FIXUP_NULL_AND_CHECK_TYPES(x, xpi);
    FIXUP_NULL_AND_CHECK_TYPES(y, ypi);

    nx = LENGTH(x);
    if (ATTRIB(x) != R_NilValue) {
	xattr = TRUE;
	xarray = isArray(x);
	xts = isTs(x);
    }
    else xarray = xts = xattr = FALSE;
    ny = LENGTH(y);
    if (ATTRIB(y) != R_NilValue) {
	yattr = TRUE;
	yarray = isArray(y);
	yts = isTs(y);
    }
    else yarray = yts = yattr = FALSE;

    /* If either x or y is a matrix with length 1 and the other is a
       vector, we want to coerce the matrix to be a vector.
       Do we want to?  We don't do it!  BDR 2004-03-06
    */

    /* FIXME: Danger Will Robinson.
     * -----  We might be trashing arguments here.
     * If we have NAMED(x) or NAMED(y) we should duplicate!
     */
    if (xarray != yarray) {
	if (xarray && nx==1 && ny!=1) {
	    REPROTECT(x = duplicate(x), xpi);
	    setAttrib(x, R_DimSymbol, R_NilValue);
	}
	if (yarray && ny==1 && nx!=1) {
	    REPROTECT(y = duplicate(y), ypi);
	    setAttrib(y, R_DimSymbol, R_NilValue);
	}
    }

    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(lcall, _("non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else {			/* (yarray) */
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	nprotect++;
	if (xattr) {
	    PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	    nprotect++;
	}
	else xnames = R_NilValue;
	if (yattr) {
	    PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
	    nprotect++;
	}
	else ynames = R_NilValue;
    }
    else {
	dims = R_NilValue;
	if (xattr) {
	    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	    nprotect++;
	}
	else xnames = R_NilValue;
	if (yattr) {
	    PROTECT(ynames = getAttrib(y, R_NamesSymbol));
	    nprotect++;
	}
	else ynames = R_NilValue;
    }
    if (nx == ny || nx == 1 || ny == 1) mismatch = 0;
    else if (nx > 0 && ny > 0) {
	if (nx > ny) mismatch = nx % ny;
	else mismatch = ny % nx;
    }

    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(lcall, _("non-conformable time-series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (nx < ny)
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else {			/* (yts) */
	    if (ny < nx)
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
	nprotect += 2;
    }
    else klass = tsp = NULL; /* -Wall */

    if (mismatch)
	warningcall(lcall,
		    _("longer object length\n\tis not a multiple of shorter object length"));

    /* need to preserve object here, as *_binary copies class attributes */
    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
	COERCE_IF_NEEDED(x, CPLXSXP, xpi);
	COERCE_IF_NEEDED(y, CPLXSXP, ypi);
	val = complex_binary(oper, x, y);
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	if(!(TYPEOF(x) == INTSXP || TYPEOF(y) == INTSXP
	     /* || TYPEOF(x) == LGLSXP || TYPEOF(y) == LGLSXP*/)) {
            /* Can get a LGLSXP. In base-Ex.R on 24 Oct '06, got 8 of these. */
	    COERCE_IF_NEEDED(x, REALSXP, xpi);
	    COERCE_IF_NEEDED(y, REALSXP, ypi);
	}
	val = real_binary(oper, x, y);
    }
    else val = integer_binary(oper, x, y, lcall);

    /* quick return if there are no attributes */
    if (! xattr && ! yattr) {
	UNPROTECT(nprotect);
	return val;
    }

    PROTECT(val);
    nprotect++;

    /* Don't set the dims if one argument is an array of size 0 and the
       other isn't of size zero, cos they're wrong */
    /* Not if the other argument is a scalar (PR#1979) */
    if (dims != R_NilValue) {
	if (!((xarray && (nx == 0) && (ny > 1)) ||
	      (yarray && (ny == 0) && (nx > 1)))){
	    setAttrib(val, R_DimSymbol, dims);
	    if (xnames != R_NilValue)
		setAttrib(val, R_DimNamesSymbol, xnames);
	    else if (ynames != R_NilValue)
		setAttrib(val, R_DimNamesSymbol, ynames);
	}
    }
    else {
	if (length(val) == length(xnames))
	    setAttrib(val, R_NamesSymbol, xnames);
	else if (length(val) == length(ynames))
	    setAttrib(val, R_NamesSymbol, ynames);
    }

    if (xts || yts) {		/* must set *after* dims! */
	setAttrib(val, R_TspSymbol, tsp);
	setAttrib(val, R_ClassSymbol, klass);
    }

    UNPROTECT(nprotect);
    return val;
}

SEXP attribute_hidden R_unary(SEXP call, SEXP op, SEXP s1)
{
    ARITHOP_TYPE operation = (ARITHOP_TYPE) PRIMVAL(op);
    switch (TYPEOF(s1)) {
    case LGLSXP:
    case INTSXP:
	return integer_unary(operation, s1);
    case REALSXP:
	return real_unary(operation, s1, call);
    case CPLXSXP:
	return complex_unary(operation, s1);
    default:
	errorcall(call, _("invalid argument to unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP integer_unary(ARITHOP_TYPE code, SEXP s1)
{
    int i, n, x;
    SEXP ans;

    switch (code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	ans = duplicate(s1);
	SET_TYPEOF(ans, INTSXP);
	n = LENGTH(s1);
	for (i = 0; i < n; i++) {
	    x = INTEGER(s1)[i];
	    INTEGER(ans)[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	return ans;
    default:
	error(_("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP real_unary(ARITHOP_TYPE code, SEXP s1, SEXP lcall)
{
    int i, n;
    SEXP ans;

    switch (code) {
    case PLUSOP: return s1;
    case MINUSOP:
	ans = duplicate(s1);
	n = LENGTH(s1);
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = -REAL(s1)[i];
	return ans;
    default:
	errorcall(lcall, _("invalid unary operator"));
    }
    return s1;			/* never used; to keep -Wall happy */
}

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)



/* The tests using integer comparisons are a bit faster than the tests
   using doubles, but they depend on a two's complement representation
   (but that is almost universal).  The tests that compare results to
   double's depend on being able to accurately represent all int's as
   double's.  Since int's are almost universally 32 bit that should be
   OK. */

#ifndef INT_32_BITS
/* configure checks whether int is 32 bits.  If not this code will
   need to be rewritten.  Since 32 bit ints are pretty much universal,
   we can worry about writing alternate code when the need arises.
   To be safe, we signal a compiler error if int is not 32 bits. */
# error code requires that int have 32 bits
#else
/* Just to be on the safe side, configure ought to check that the
   mashine uses two's complement. A define like
#define USES_TWOS_COMPLEMENT (~0 == (unsigned) -1)
   might work, but at least one compiler (CodeWarrior 6) chokes on it.
   So for now just assume it is true.
*/
#define USES_TWOS_COMPLEMENT 1

#if USES_TWOS_COMPLEMENT
# define OPPOSITE_SIGNS(x, y) ((x < 0) ^ (y < 0))
# define GOODISUM(x, y, z) (((x) > 0) ? ((y) < (z)) : ! ((y) < (z)))
# define GOODIDIFF(x, y, z) (!(OPPOSITE_SIGNS(x, y) && OPPOSITE_SIGNS(x, z)))
#else
# define GOODISUM(x, y, z) ((double) (x) + (double) (y) == (z))
# define GOODIDIFF(x, y, z) ((double) (x) - (double) (y) == (z))
#endif
#define GOODIPROD(x, y, z) ((double) (x) * (double) (y) == (z))
#define INTEGER_OVERFLOW_WARNING _("NAs produced by integer overflow")
#endif

static SEXP integer_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2, SEXP lcall)
{
    int i, i1, i2, n, n1, n2;
    int x1, x2;
    SEXP ans;
    Rboolean naflag = FALSE;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) n = 0; else n = (n1 > n2) ? n1 : n2;

    if (code == DIVOP || code == POWOP)
	ans = allocVector(REALSXP, n);
    else
	ans = allocVector(INTSXP, n);
    if (n1 == 0 || n2 == 0) return(ans);
    /* note: code below was surely wrong in DIVOP and POWOP cases,
       since ans was a REALSXP.
     */

/*    if (n1 < 1 || n2 < 1) {
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = NA_INTEGER;
	return ans;
	} */
#ifdef R_MEMORY_PROFILING
    if (TRACE(s1) || TRACE(s2)){
       if (TRACE(s1) && TRACE(s2)){
	  if (n1>n2)
	      memtrace_report(s1,ans);
	  else
	      memtrace_report(s2, ans);
       } else if (TRACE(s1))
	   memtrace_report(s1,ans);
       else /* only s2 */
	   memtrace_report(s2, ans);
       SET_TRACE(ans, 1);
    }
#endif

    switch (code) {
    case PLUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else {
		int val = x1 + x2;
		if (val != NA_INTEGER && GOODISUM(x1, x2, val))
		    INTEGER(ans)[i] = val;
		else {
		    INTEGER(ans)[i] = NA_INTEGER;
		    naflag = TRUE;
		}
	    }
	}
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case MINUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else {
		int val = x1 - x2;
		if (val != NA_INTEGER && GOODIDIFF(x1, x2, val))
		    INTEGER(ans)[i] = val;
		else {
		    naflag = TRUE;
		    INTEGER(ans)[i] = NA_INTEGER;
		}
	    }
	}
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case TIMESOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else {
		int val = x1 * x2;
		if (val != NA_INTEGER && GOODIPROD(x1, x2, val))
		    INTEGER(ans)[i] = val;
		else {
		    naflag = TRUE;
		    INTEGER(ans)[i] = NA_INTEGER;
		}
	    }
	}
	if (naflag)
	    warningcall(lcall, INTEGER_OVERFLOW_WARNING);
	break;
    case DIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		    REAL(ans)[i] = NA_REAL;
		else
		    REAL(ans)[i] = (double) x1 / (double) x2;
	}
	break;
    case POWOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		REAL(ans)[i] = NA_REAL;
	    else {
		REAL(ans)[i] = R_pow((double) x1, (double) x2);
	    }
	}
	break;
    case MODOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
		INTEGER(ans)[i] = NA_INTEGER;
	    else {
		INTEGER(ans)[i] = /* till 0.63.2:	x1 % x2 */
		    (x1 >= 0 && x2 > 0) ? x1 % x2 :
		    (int)myfmod((double)x1,(double)x2);
	    }
	}
	break;
    case IDIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else if (x2 == 0)
		INTEGER(ans)[i] = 0;
	    else
		INTEGER(ans)[i] = floor((double)x1 / (double)x2);
	}
	break;
    }


    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (n1 > n2)
	copyMostAttrib(s1, ans);
    else if (n1 == n2) {
	copyMostAttrib(s2, ans);
	copyMostAttrib(s1, ans);
    }
    else
	copyMostAttrib(s2, ans);

    return ans;
}

#define R_INTEGER(robj, i) (double) (INTEGER(robj)[i] == NA_INTEGER ? NA_REAL : INTEGER(robj)[i])

static SEXP real_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    SEXP ans;

    /* Note: "s1" and "s2" are protected above. */
    n1 = LENGTH(s1);
    n2 = LENGTH(s2);

    /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) return(allocVector(REALSXP, 0));

    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(REALSXP, n));
#ifdef R_MEMORY_PROFILING
    if (TRACE(s1) || TRACE(s2)){
       if (TRACE(s1) && TRACE(s2)){
	  if (n1>n2)
	      memtrace_report(s1,ans);
	  else
	      memtrace_report(s2, ans);
       } else if (TRACE(s1))
	   memtrace_report(s1,ans);
       else /* only s2 */
	   memtrace_report(s2, ans);
       SET_TRACE(ans, 1);
    }
#endif

/*    if (n1 < 1 || n2 < 1) {
      for (i = 0; i < n; i++)
      REAL(ans)[i] = NA_REAL;
      return ans;
      } */

    switch (code) {
    case PLUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
   	   mod_iterate(n1, n2, i1, i2) {
   	         REAL(ans)[i] = REAL(s1)[i1] + REAL(s2)[i2];
	     }
	} else	if(TYPEOF(s1) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_INTEGER(s1, i1) + REAL(s2)[i2];
	     }
	} else	if(TYPEOF(s2) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = REAL(s1)[i1] + R_INTEGER(s2, i2);
	     }
	}

	break;
    case MINUSOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
   	   mod_iterate(n1, n2, i1, i2) {
	      REAL(ans)[i] = REAL(s1)[i1] - REAL(s2)[i2];
	   }
	} else	if(TYPEOF(s1) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_INTEGER(s1, i1) - REAL(s2)[i2];
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = REAL(s1)[i1] - R_INTEGER(s2, i2);
	   }
	}
	break;
    case TIMESOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	   mod_iterate(n1, n2, i1, i2) {
		REAL(ans)[i] = REAL(s1)[i1] * REAL(s2)[i2];
	    }
	} else if(TYPEOF(s1) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_INTEGER(s1, i1) * REAL(s2)[i2];
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = REAL(s1)[i1] * R_INTEGER(s2, i2);
	   }
	}
	break;
    case DIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	   mod_iterate(n1, n2, i1, i2) {
		REAL(ans)[i] = REAL(s1)[i1] / REAL(s2)[i2];
	    }
	} else if(TYPEOF(s1) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_INTEGER(s1, i1) / REAL(s2)[i2];
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = REAL(s1)[i1] / R_INTEGER(s2, i2);
	   }
	}
	break;
    case POWOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_pow(REAL(s1)[i1], REAL(s2)[i2]);
	    }
	} else if(TYPEOF(s1) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_pow( R_INTEGER(s1, i1), REAL(s2)[i2]);
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = R_pow(REAL(s1)[i1], R_INTEGER(s2, i2));
	   }
	}
	break;
    case MODOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfmod(REAL(s1)[i1], REAL(s2)[i2]);
	    }
	} else if(TYPEOF(s1) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfmod( R_INTEGER(s1, i1), REAL(s2)[i2]);
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfmod(REAL(s1)[i1], R_INTEGER(s2, i2));
	   }
	}
	break;
    case IDIVOP:
	if(TYPEOF(s1) == REALSXP && TYPEOF(s2) == REALSXP) {
	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfloor(REAL(s1)[i1], REAL(s2)[i2]);
	    }
	} else if(TYPEOF(s1) == INTSXP ) {
   	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfloor(R_INTEGER(s1, i1), REAL(s2)[i2]);
	   }
	} else	if(TYPEOF(s2) == INTSXP ) {
  	   mod_iterate(n1, n2, i1, i2) {
	       REAL(ans)[i] = myfloor(REAL(s1)[i1], R_INTEGER(s2,i2));
	   }
	}
	break;
    }

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue) {
	UNPROTECT(1);
	return ans;
    }

    /* Copy attributes from longer argument. */

    if (n1 > n2)
	copyMostAttrib(s1, ans);
    else if (n1 == n2) {
	copyMostAttrib(s2, ans);
	copyMostAttrib(s1, ans);
    }
    else
	copyMostAttrib(s2, ans);

    UNPROTECT(1);
    return ans;
}


/* Mathematical Functions of One Argument */

static SEXP math1(SEXP sa, double(*f)(double), SEXP lcall)
{
    SEXP sy;
    double *y, *a;
    int i, n, sao = OBJECT(sa);
    int naflag;

    if (!isNumeric(sa))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    n = length(sa);
    /* coercion can lose the object bit */
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sy = allocVector(REALSXP, n));
#ifdef R_MEMORY_PROFILING
    if (TRACE(sa)){
       memtrace_report(sa, sy);
       SET_TRACE(sy, 1);
    }
#endif
    a = REAL(sa);
    y = REAL(sy);
    naflag = 0;
    for (i = 0; i < n; i++) {
	if (ISNAN(a[i]))
	    y[i] = a[i];
	else {
	    y[i] = f(a[i]);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    if(naflag)
	warningcall(lcall, R_MSG_NA);

    SET_ATTRIB(sy, duplicate(ATTRIB(sa)));
    SET_OBJECT(sy, sao);
    UNPROTECT(2);
    return sy;
}


SEXP attribute_hidden do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    checkArity(op, args);

    if (isComplex(CAR(args)))

	return complex_math1(call, op, args, env);

#define MATH1(x) math1(CAR(args), x, call);
    switch (PRIMVAL(op)) {
    case 1: return MATH1(floor);
    case 2: return MATH1(ceil);
    case 3: return MATH1(sqrt);
    case 4: return MATH1(sign);
    case 5: return MATH1(trunc);

    case 10: return MATH1(exp);
    case 11: return MATH1(expm1);
    case 12: return MATH1(log1p);
    case 20: return MATH1(cos);
    case 21: return MATH1(sin);
    case 22: return MATH1(tan);
    case 23: return MATH1(acos);
    case 24: return MATH1(asin);

    case 30: return MATH1(cosh);
    case 31: return MATH1(sinh);
    case 32: return MATH1(tanh);
    case 33: return MATH1(acosh);
    case 34: return MATH1(asinh);
    case 35: return MATH1(atanh);

    case 40: return MATH1(lgammafn);
    case 41: return MATH1(gammafn);

    case 42: return MATH1(digamma);
    case 43: return MATH1(trigamma);
	/* case 44: return MATH1(tetragamma);
	   case 45: return MATH1(pentagamma);
	   removed in 2.0.0
	*/

    case 46: return MATH1(gamma_cody);

    default:
	errorcall(call, _("unimplemented real function of 1 argument"));
    }
    return s; /* never used; to keep -Wall happy */
}

SEXP attribute_hidden do_abs(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;
    checkArity(op, args);
    if (isComplex(CAR(args)) || !(isInteger(CAR(args)) || isLogical(CAR(args))))
	return do_cmathfuns(call, op, args, env);
    else { /* integer or logical ==> return integer */
	SEXP x = CAR(args);
	int i, n;
	n = length(x);
	PROTECT(s = allocVector(INTSXP, n));
	/* Note: relying on INTEGER(.) === LOGICAL(.) : */
	for(i = 0 ; i < n ; i++)
	    INTEGER(s)[i] = abs(INTEGER(x)[i]);
	SET_ATTRIB(s, duplicate(ATTRIB(x)));
	SET_OBJECT(s, OBJECT(x));
	UNPROTECT(1);
	return s;
    }
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;

static SEXP math2(SEXP sa, SEXP sb, double (*f)(double, double),
		  SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, n, na, nb, sao = OBJECT(sa), sbo = OBJECT(sb);
    double ai, bi, *a, *b, *y;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    /* for 0-length a we want the attributes of a, not those of b
       as no recycling will occur */
#define SETUP_Math2				\
    na = LENGTH(sa);				\
    nb = LENGTH(sb);				\
    if ((na == 0) || (nb == 0))	{		\
        PROTECT(sy = allocVector(REALSXP, 0));	\
        if (na == 0) {				\
	    SET_ATTRIB(sy, duplicate(ATTRIB(sa)));\
	    SET_OBJECT(sy, sao);		\
        }					\
        UNPROTECT(1);				\
	return(sy);				\
    }						\
    n = (na < nb) ? nb : na;			\
    PROTECT(sa = coerceVector(sa, REALSXP));	\
    PROTECT(sb = coerceVector(sb, REALSXP));	\
    PROTECT(sy = allocVector(REALSXP, n));	\
    a = REAL(sa);				\
    b = REAL(sb);				\
    y = REAL(sy);				\
    naflag = 0

    SETUP_Math2;

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb)){
       if (TRACE(sa) && TRACE(sb)){
	  if (na>nb)
	      memtrace_report(sa, sy);
	  else
	      memtrace_report(sb, sy);
       } else if (TRACE(sa))
	   memtrace_report(sa, sy);
       else /* only s2 */
	   memtrace_report(sb, sy);
       SET_TRACE(sy, 1);
    }
#endif

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math2				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, sao);		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, sbo);		\
    }						\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI,
		    double (*f)(double, double, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, n, na, nb, sao = OBJECT(sa), sbo = OBJECT(sb);
    double ai, bi, *a, *b, *y;
    int m_opt;
    int naflag;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    m_opt = asInteger(sI);

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb)){
       if (TRACE(sa) && TRACE(sb)){
	  if (na>nb)
	      memtrace_report(sa, sy);
	  else
	      memtrace_report(sb, sy);
       } else if (TRACE(sa))
	   memtrace_report(sa, sy);
       else /* only s2 */
	   memtrace_report(sb, sy);
       SET_TRACE(sy, 1);
    }
#endif

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, m_opt);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2,
		    double (*f)(double, double, int, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, n, na, nb, sao = OBJECT(sa), sbo = OBJECT(sb);
    double ai, bi, *a, *b, *y;
    int i_1, i_2;
    int naflag;
    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    i_1 = asInteger(sI1);
    i_2 = asInteger(sI2);

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb)){
       if (TRACE(sa) && TRACE(sb)){
	  if (na>nb)
	      memtrace_report(sa, sy);
	  else
	      memtrace_report(sb, sy);
       } else if (TRACE(sa))
	   memtrace_report(sa, sy);
       else /* only s2 */
	   memtrace_report(sb, sy);
       SET_TRACE(sy, 1);
    }
#endif

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = f(ai, bi, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_2() */

#define Math2(A, FUN)	  math2(CAR(A), CADR(A), FUN, call);
#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN, call);
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call)

SEXP attribute_hidden do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if (isComplex(CAR(args)) ||
	(PRIMVAL(op) == 0 && isComplex(CADR(args))))
	return complex_math2(call, op, args, env);


    switch (PRIMVAL(op)) {

    case  0: return Math2(args, atan2);
    case 10001: return Math2(args, rround); /* #defined to fround in Rmath.h */
    case 10004: return Math2(args, prec);   /* #defined to fprec in Rmath.h */

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case  6: return Math2_1(args, dchisq);
    case  7: return Math2_2(args, pchisq);
    case  8: return Math2_2(args, qchisq);

    case  9: return Math2_1(args, dexp);
    case 10: return Math2_2(args, pexp);
    case 11: return Math2_2(args, qexp);

    case 12: return Math2_1(args, dgeom);
    case 13: return Math2_2(args, pgeom);
    case 14: return Math2_2(args, qgeom);

    case 15: return Math2_1(args, dpois);
    case 16: return Math2_2(args, ppois);
    case 17: return Math2_2(args, qpois);

    case 18: return Math2_1(args, dt);
    case 19: return Math2_2(args, pt);
    case 20: return Math2_2(args, qt);

    case 21: return Math2_1(args, dsignrank);
    case 22: return Math2_2(args, psignrank);
    case 23: return Math2_2(args, qsignrank);

    case 24: return Math2(args, bessel_j);
    case 25: return Math2(args, bessel_y);
    case 26: return Math2(args, psigamma);

    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 2);
    }
    return op;			/* never used; to keep -Wall happy */
}


SEXP attribute_hidden do_atan(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    switch (n = length(args)) {
    case 1:
	if (isComplex(CAR(args)))
	    return complex_math1(call, op, args, env);
	else
	    return math1(CAR(args), atan, call);
    /* prior to 2.3.0, 2 args were allowed,
       but this was never documented */
    default:
	error(_("%d arguments passed to 'atan' which requires 1"), n);
    }
    return s;			/* never used; to keep -Wall happy */
}


/* The S4 Math2 group, round and signif */
SEXP attribute_hidden do_Math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a;
    if (DispatchGroup("Math", call, op, args, env, &a))
	return a;

    checkArity(op, args);
    if (length(CADR(args)) == 0)
	errorcall(call, _("invalid second argument of length 0"));
    return do_math2(call, op, args, env);
}

SEXP attribute_hidden do_log(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    switch (n = length(args)) {
    case 1:
	if (isComplex(CAR(args)))
	    return complex_math1(call, op, args, env);
	else
	    return math1(CAR(args), R_log, call);
    case 2:
	if (length(CADR(args)) == 0)
	    errorcall(call, _("invalid second argument of length 0"));
	if (isComplex(CAR(args)) || isComplex(CDR(args)))
	    return complex_math2(call, op, args, env);
	else
	    return math2(CAR(args), CADR(args), logbase, call);
    default:
	error(_("%d arguments passed to 'log' which requires 1 or 2"), n);
    }
    return s;			/* never used; to keep -Wall happy */
}


/* Mathematical Functions of Three (Real) Arguments */

#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define mod_iterate3(n1,n2,n3,i1,i2,i3) for (i=i1=i2=i3=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,				\
	i2 = (++i2==n2) ? 0 : i2,				\
	i3 = (++i3==n3) ? 0 : i3,				\
	++i)

static SEXP math3(SEXP sa, SEXP sb, SEXP sc,
		  double (*f)(double, double, double), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc);
    double ai, bi, ci, *a, *b, *c, *y;
    int naflag;

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	errorcall(lcall, R_MSG_NONNUM_MATH);			\
								\
    na = LENGTH(sa);						\
    nb = LENGTH(sb);						\
    nc = LENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0))			\
	return(allocVector(REALSXP, 0));			\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    a = REAL(sa);						\
    b = REAL(sb);						\
    c = REAL(sc);						\
    y = REAL(sy);						\
    naflag = 0

    SETUP_Math3;

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb) || TRACE(sc)){
       if (TRACE(sa))
	  memtrace_report(sa,sy);
       else if (TRACE(sb))
	  memtrace_report(sb, sy);
       else if (TRACE(sc))
	  memtrace_report(sc,sy);
       SET_TRACE(sy, 1);
    }
#endif

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math3				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, sao);		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, sbo);		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, sco);		\
    }						\
    UNPROTECT(4)

    FINISH_Math3;

    return sy;
} /* math3 */

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI,
		    double (*f)(double, double, double, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc);
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb) || TRACE(sc)){
       if (TRACE(sa))
	  memtrace_report(sa,sy);
       else if (TRACE(sb))
	  memtrace_report(sb, sy);
       else if (TRACE(sc))
	  memtrace_report(sc,sy);
       SET_TRACE(sy, 1);
    }
#endif

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc);
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1,i_2;
    int naflag;

    SETUP_Math3;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

#ifdef R_MEMORY_PROFILING
    if (TRACE(sa) || TRACE(sb) || TRACE(sc)){
       if (TRACE(sa))
	  memtrace_report(sa,sy);
       else if (TRACE(sb))
	  memtrace_report(sb, sy);
       else if (TRACE(sc))
	  memtrace_report(sc,sy);
       SET_TRACE(sy, 1);
    }
#endif


    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = f(ai, bi, ci, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_2 */

#define Math3(A, FUN)   math3  (CAR(A), CADR(A), CADDR(A), FUN, call);
#define Math3_1(A, FUN)	math3_1(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN, call);
#define Math3_2(A, FUN) math3_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN, call)

SEXP attribute_hidden do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    switch (PRIMVAL(op)) {

    case  1:  return Math3_1(args, dbeta);
    case  2:  return Math3_2(args, pbeta);
    case  3:  return Math3_2(args, qbeta);

    case  4:  return Math3_1(args, dbinom);
    case  5:  return Math3_2(args, pbinom);
    case  6:  return Math3_2(args, qbinom);

    case  7:  return Math3_1(args, dcauchy);
    case  8:  return Math3_2(args, pcauchy);
    case  9:  return Math3_2(args, qcauchy);

    case 10:  return Math3_1(args, df);
    case 11:  return Math3_2(args, pf);
    case 12:  return Math3_2(args, qf);

    case 13:  return Math3_1(args, dgamma);
    case 14:  return Math3_2(args, pgamma);
    case 15:  return Math3_2(args, qgamma);

    case 16:  return Math3_1(args, dlnorm);
    case 17:  return Math3_2(args, plnorm);
    case 18:  return Math3_2(args, qlnorm);

    case 19:  return Math3_1(args, dlogis);
    case 20:  return Math3_2(args, plogis);
    case 21:  return Math3_2(args, qlogis);

    case 22:  return Math3_1(args, dnbinom);
    case 23:  return Math3_2(args, pnbinom);
    case 24:  return Math3_2(args, qnbinom);

    case 25:  return Math3_1(args, dnorm);
    case 26:  return Math3_2(args, pnorm);
    case 27:  return Math3_2(args, qnorm);

    case 28:  return Math3_1(args, dunif);
    case 29:  return Math3_2(args, punif);
    case 30:  return Math3_2(args, qunif);

    case 31:  return Math3_1(args, dweibull);
    case 32:  return Math3_2(args, pweibull);
    case 33:  return Math3_2(args, qweibull);

    case 34:  return Math3_1(args, dnchisq);
    case 35:  return Math3_2(args, pnchisq);
    case 36:  return Math3_2(args, qnchisq);

    case 37:  return Math3_1(args, dnt);
    case 38:  return Math3_2(args, pnt);
    case 39:  return Math3_2(args, qnt);

    case 40:  return Math3_1(args, dwilcox);
    case 41:  return Math3_2(args, pwilcox);
    case 42:  return Math3_2(args, qwilcox);

    case 43:  return Math3(args, bessel_i);
    case 44:  return Math3(args, bessel_k);


    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 3);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

#define if_NA_Math4_set(y,a,b,c,d)			        	\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;

#define mod_iterate4(n1,n2,n3,n4,i1,i2,i3,i4) for (i=i1=i2=i3=i4=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,					\
	i2 = (++i2==n2) ? 0 : i2,					\
	i3 = (++i3==n3) ? 0 : i3,					\
	i4 = (++i4==n4) ? 0 : i4,					\
	++i)

static SEXP math4(SEXP sa, SEXP sb, SEXP sc, SEXP sd,
		  double (*f)(double, double, double, double), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc), sdo = OBJECT(sd);
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int naflag;

#define SETUP_Math4							\
    if(!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = LENGTH(sa);							\
    nb = LENGTH(sb);							\
    nc = LENGTH(sc);							\
    nd = LENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))		\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math4;

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math4				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, sao);		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, sbo);		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, sco);		\
    }						\
    else if (n == nd) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sd)));	\
	SET_OBJECT(sy, sdo);		\
    }						\
    UNPROTECT(5)

    FINISH_Math4;

    return sy;
} /* math4() */

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)(double, double, double, double, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc), sdo = OBJECT(sd);
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)(double, double, double, double, int, int), SEXP lcall)
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc), sdo = OBJECT(sd);
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1, i_2;
    int naflag;

    SETUP_Math4;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = f(ai, bi, ci, di, i_1, i_2);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_2() */


#define CAD3R	CADDDR
/* This is not (yet) in Rinternals.h : */
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))

#define Math4(A, FUN)   math4  (CAR(A), CADR(A), CADDR(A), CAD3R(A), FUN, call)
#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN, call)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN, call)


SEXP attribute_hidden do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);


    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- math4() at all! : */
    case -99: return Math4(args, (double (*)(double, double, double, double))NULL);

    case  1: return Math4_1(args, dhyper);
    case  2: return Math4_2(args, phyper);
    case  3: return Math4_2(args, qhyper);

    case  4: return Math4_1(args, dnbeta);
    case  5: return Math4_2(args, pnbeta);
    case  6: return Math4_2(args, qnbeta);
    case  7: return Math4_1(args, dnf);
    case  8: return Math4_2(args, pnf);
    case  9: return Math4_2(args, qnf);
#ifdef UNIMP
    case 10: return Math4_1(args, dtukey);
#endif
    case 11: return Math4_2(args, ptukey);
    case 12: return Math4_2(args, qtukey);
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 4);
    }
    return op;			/* never used; to keep -Wall happy */
}


#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if     (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e)) 	\
    		y = NA_REAL;						\
	else if(ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

#define mod_iterate5(n1,n2,n3,n4,n5, i1,i2,i3,i4,i5)	\
 for (i=i1=i2=i3=i4=i5=0; i<n;				\
	i1 = (++i1==n1) ? 0 : i1,			\
	i2 = (++i2==n2) ? 0 : i2,			\
	i3 = (++i3==n3) ? 0 : i3,			\
	i4 = (++i4==n4) ? 0 : i4,			\
	i5 = (++i5==n5) ? 0 : i5,			\
	++i)

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne,
	sao = OBJECT(sa), sbo = OBJECT(sb), sco = OBJECT(sc),
	sdo = OBJECT(sd), seo = OBJECT(se);
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = LENGTH(sa);							\
    nb = LENGTH(sb);							\
    nc = LENGTH(sc);							\
    nd = LENGTH(sd);							\
    ne = LENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))	\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    e = REAL(se);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math5;

    mod_iterate5 (na, nb, nc, nd, ne,
		  ia, ib, ic, id, ie) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = f(ai, bi, ci, di, ei);
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math5				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, sao);		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, sbo);		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, sco);		\
    }						\
    else if (n == nd) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sd)));	\
	SET_OBJECT(sy, sdo);		\
    }						\
    else if (n == ne) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(se)));	\
	SET_OBJECT(sy, seo);		\
    }						\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP attribute_hidden do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	errorcall(call,
		  _("unimplemented real function of %d numeric arguments"), 5);
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */
