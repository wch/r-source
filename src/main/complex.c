/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-2013	    The R Core Team
 *  Copyright (C) 2005		    The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Note: gcc may warn in several places about C99 features as extensions.
   This is a very-long-standing GCC bug, http://gcc.gnu.org/PR7263
   The system <complex.h> header can work around it: some do.
*/

#if 0
/* For testing substitute fns */
#undef HAVE_CARG
#undef HAVE_CABS
#undef HAVE_CPOW
#undef HAVE_CEXP
#undef HAVE_CLOG
#undef HAVE_CSQRT
#undef HAVE_CSIN
#undef HAVE_CCOS
#undef HAVE_CTAN
#undef HAVE_CASIN
#undef HAVE_CACOS
#undef HAVE_CATAN
#undef HAVE_CSINH
#undef HAVE_CCOSH
#undef HAVE_CTANH
#endif

#ifdef __CYGWIN__
/* as of 1.7.8 it had cacos, but it does not work */
#undef HAVE_CACOS
#endif

#ifdef __SUNPRO_C
/* segfaults in Solaris Studio 12.3 */
#undef HAVE_CPOW
#endif

#include <Defn.h>		/* -> ../include/R_ext/Complex.h */
#include <Internal.h>
#include <Rmath.h>

#include "arithmetic.h"		/* complex_*  */
#include <complex.h>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000


/* GCC has problems with header files on e.g. Solaris.
   That OS defines the imaginary type, but GCC does not.
   Probably needed elsewhere, e.g. AIX, HP-UX (PR#15083)
   And use on Win32/64 suppresses warnings.
   The warning is also seen on Mac OS 10.5, but not later.
*/
#if defined(__GNUC__) && (defined(__sun__) || defined(__hpux__) || defined(Win32))
# undef  I
# define I (__extension__ 1.0iF)
#endif


/* 
   Note: this could use the C11 CMPLX() macro.
   As could mycpow, z_tan and some of the substitutes.
 */
static R_INLINE double complex toC99(Rcomplex *x)
{
#if __GNUC__
    double complex ans = (double complex) 0; /* -Wall */
    __real__ ans = x->r;
    __imag__ ans = x->i;
    return ans;
#else
    return x->r + x->i * I;
#endif
}
#define C99_COMPLEX2(x, i) toC99(COMPLEX(x) + i)

static R_INLINE void 
SET_C99_COMPLEX(Rcomplex *x, R_xlen_t i, double complex value)
{
    Rcomplex *ans = x+i;
    ans->r = creal(value);
    ans->i = cimag(value);
}

SEXP attribute_hidden complex_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    R_xlen_t i, n;
    SEXP ans;

    switch(code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	ans = NAMED(s1) == 0 ? s1 : duplicate(s1);
	n = XLENGTH(s1);
	for (i = 0; i < n; i++) {
	    Rcomplex x = COMPLEX(s1)[i];
	    COMPLEX(ans)[i].r = -x.r;
	    COMPLEX(ans)[i].i = -x.i;
	}
	return ans;
    default:
	errorcall(call, _("invalid complex unary operator"));
    }
    return R_NilValue; /* -Wall */
}

static R_INLINE double complex R_cpow_n(double complex X, int k)
{
    if(k == 0) return (double complex) 1.;
    else if(k == 1) return X;
    else if(k < 0) return 1. / R_cpow_n(X, -k);
    else {/* k > 0 */
	double complex z = (double complex) 1.;;
	while (k > 0) {
	    if (k & 1) z = z * X;
	    if (k == 1) break;
	    k >>= 1; /* efficient division by 2; now have k >= 1 */
	    X = X * X;
	}
	return z;
    }
}

#if defined(Win32)
# undef HAVE_CPOW
#endif
/* reason for this:
  1) X^n  (e.g. for n = +/- 2, 3) is unnecessarily inaccurate in glibc;
     cut-off 65536 : guided from empirical speed measurements

  2) On Mingw (but not Mingw-w64) the system cpow is explicitly linked
     against the (slow) MSVCRT pow, and gets (0+0i)^Y as 0+0i for all Y.

  3) PPC Mac OS X crashes on powers of 0+0i (at least under Rosetta).
  Really 0i^-1 should by Inf+NaNi, but getting that portably seems too hard.
  (C1x's CMPLX will eventually be possible.)
*/

static double complex mycpow (double complex X, double complex Y)
{
    double complex Z;
    double yr = creal(Y), yi = cimag(Y); 
    int k;
    if (X == 0.0) {
	if (yi == 0.0) Z = R_pow(0.0, yr); else Z = R_NaN + R_NaN*I;
    } else if (yi == 0.0 && yr == (k = (int) yr) && abs(k) <= 65536)
	Z = R_cpow_n(X, k);
    else
#ifdef HAVE_CPOW
	Z = cpow(X, Y);
#else
    {
	/* Used for FreeBSD and MingGW, hence mainly with gcc */
	double rho, r, i, theta;
	r = hypot(creal(X), cimag(X));
	i = atan2(cimag(X), creal(X));
	theta = i * yr;
	if (yi == 0.0)
	    rho = pow(r, yr);
	else {
	    /* rearrangement of cexp(X * clog(Y)) */
	    r = log(r);
	    theta += r * yi;
	    rho = exp(r * yr - i * yi);
	}
#ifdef __GNUC__
	__real__ Z = rho * cos(theta);
	__imag__ Z = rho * sin(theta);
#else
	Z = rho * cos(theta) + (rho * sin(theta)) * I;
#endif
    }
#endif
    return Z;
}

/* See arithmetic.c */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

SEXP attribute_hidden complex_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i,i1, i2, n, n1, n2;
    SEXP ans;

    /* Note: "s1" and "s2" are protected in the calling code. */
    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
     /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) return(allocVector(CPLXSXP, 0));

    n = (n1 > n2) ? n1 : n2;
    ans = R_allocOrReuseVector(s1, s2, CPLXSXP, n);
    PROTECT(ans);

    switch (code) {
    case PLUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
	    COMPLEX(ans)[i].r = x1.r + x2.r;
	    COMPLEX(ans)[i].i = x1.i + x2.i;
	}
	break;
    case MINUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
	    COMPLEX(ans)[i].r = x1.r - x2.r;
	    COMPLEX(ans)[i].i = x1.i - x2.i;
	}
	break;
    case TIMESOP:
	mod_iterate(n1, n2, i1, i2) {
	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_C99_COMPLEX(COMPLEX(ans), i,
			    C99_COMPLEX2(s1, i1) * C99_COMPLEX2(s2, i2));
	}
	break;
    case DIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_C99_COMPLEX(COMPLEX(ans), i,
			    C99_COMPLEX2(s1, i1) / C99_COMPLEX2(s2, i2));
	}
	break;
    case POWOP:
	mod_iterate(n1, n2, i1, i2) {
	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_C99_COMPLEX(COMPLEX(ans), i,
			    mycpow(C99_COMPLEX2(s1, i1), C99_COMPLEX2(s2, i2)));
	}
	break;
    default:
	error(_("unimplemented complex operation"));
    }
    UNPROTECT(1);

    /* quick return if there are no attributes */
    if (ATTRIB(s1) == R_NilValue && ATTRIB(s2) == R_NilValue)
	return ans;

    /* Copy attributes from longer argument. */

    if (ans != s2 && n == n2 && ATTRIB(s2) != R_NilValue)
        copyMostAttrib(s2, ans);
    if (ans != s1 && n == n1 && ATTRIB(s1) != R_NilValue)
        copyMostAttrib(s1, ans); /* Done 2nd so s1's attrs overwrite s2's */

    return ans;
}

SEXP attribute_hidden do_cmathfuns(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y = R_NilValue;	/* -Wall*/
    R_xlen_t i, n;

    checkArity(op, args);
    check1arg(args, call, "z");
    if (DispatchGroup("Complex", call, op, args, env, &x))
	return x;
    x = CAR(args);
    if (isComplex(x)) {
	n = XLENGTH(x);
	switch(PRIMVAL(op)) {
	case 1:	/* Re */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = COMPLEX(x)[i].r;
	    break;
	case 2:	/* Im */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = COMPLEX(x)[i].i;
	    break;
	case 3:	/* Mod */
	case 6:	/* abs */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
#if HAVE_CABS
		REAL(y)[i] = cabs(C99_COMPLEX2(x, i));
#else
		REAL(y)[i] = hypot(COMPLEX(x)[i].r, COMPLEX(x)[i].i);
#endif
	    break;
	case 4:	/* Arg */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
#if HAVE_CARG
		REAL(y)[i] = carg(C99_COMPLEX2(x, i));
#else
		REAL(y)[i] = atan2(COMPLEX(x)[i].i, COMPLEX(x)[i].r);
#endif
	    break;
	case 5:	/* Conj */
	    y = NAMED(x) == 0 ? x : allocVector(CPLXSXP, n);
	    for(i = 0 ; i < n ; i++) {
		COMPLEX(y)[i].r = COMPLEX(x)[i].r;
		COMPLEX(y)[i].i = -COMPLEX(x)[i].i;
	    }
	    break;
	}
    }
    else if(isNumeric(x)) { /* so no complex numbers involved */
	n = XLENGTH(x);
	if(isReal(x)) PROTECT(x);
	else PROTECT(x = coerceVector(x, REALSXP));
        y = NAMED(x) == 0 ? x : allocVector(REALSXP, n);

	switch(PRIMVAL(op)) {
	case 1:	/* Re */
	case 5:	/* Conj */
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = REAL(x)[i];
	    break;
	case 2:	/* Im */
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = 0.0;
	    break;
	case 4:	/* Arg */
	    for(i = 0 ; i < n ; i++)
		if(ISNAN(REAL(x)[i]))
		    REAL(y)[i] = REAL(x)[i];
		else if (REAL(x)[i] >= 0)
		    REAL(y)[i] = 0;
		else
		    REAL(y)[i] = M_PI;
	    break;
	case 3:	/* Mod */
	case 6:	/* abs */
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = fabs(REAL(x)[i]);
	    break;
	}
	UNPROTECT(1);
    }
    else errorcall(call, _("non-numeric argument to function"));

    if (x != y && ATTRIB(x) != R_NilValue) {
        PROTECT(x);
        PROTECT(y);
        DUPLICATE_ATTRIB(y, x);
        UNPROTECT(2);
    }
    return y;
}

/* used in format.c and printutils.c */
#define MAX_DIGITS 22
void attribute_hidden z_prec_r(Rcomplex *r, Rcomplex *x, double digits)
{
    double m = 0.0, m1, m2;
    int dig, mag;

    r->r = x->r; r->i = x->i;
    m1 = fabs(x->r); m2 = fabs(x->i);
    if(R_FINITE(m1)) m = m1;
    if(R_FINITE(m2) && m2 > m) m = m2;
    if (m == 0.0) return;
    if (!R_FINITE(digits)) {
	if(digits > 0) return; else {r->r = r->i = 0.0; return ;}
    }
    dig = (int)floor(digits+0.5);
    if (dig > MAX_DIGITS) return; else if (dig < 1) dig = 1;
    mag = (int)floor(log10(m));
    dig = dig - mag - 1;
    if (dig > 306) {
	double pow10 = 1.0e4;
	digits = (double)(dig - 4);
	r->r = fround(pow10 * x->r, digits)/pow10;
	r->i = fround(pow10 * x->i, digits)/pow10;
    } else {
	digits = (double)(dig);
	r->r = fround(x->r, digits);
	r->i = fround(x->i, digits);
    }
}

/* These substitute functions are rarely used, and not extensively
   tested, e.g. over CRAN.  Please do not change without very good
   reason!

   Currently (Feb 2011) they are used on FreeBSD.
*/

#ifndef HAVE_CLOG
#define clog R_clog
/* FIXME: maybe add full IEC60559 support */
static double complex clog(double complex x)
{
    double xr = creal(x), xi = cimag(x);
    return log(hypot(xr, xi)) + atan2(xi, xr)*I;
}
#endif

#ifndef HAVE_CSQRT
#define csqrt R_csqrt
/* FreeBSD does have this one */
static double complex csqrt(double complex x)
{
    return mycpow(x, 0.5+0.0*I);
}
#endif

#ifndef HAVE_CEXP
#define cexp R_cexp
/* FIXME: check/add full IEC60559 support */
static double complex cexp(double complex x)
{
    double expx = exp(creal(x)), y = cimag(x);
    return expx * cos(y) + (expx * sin(y)) * I;
}
#endif

#ifndef HAVE_CCOS
#define ccos R_ccos
static double complex ccos(double complex x)
{
    double xr = creal(x), xi = cimag(x);
    return cos(xr)*cosh(xi) - sin(xr)*sinh(xi)*I; /* A&S 4.3.56 */
}
#endif

#ifndef HAVE_CSIN
#define csin R_csin
static double complex csin(double complex x)
{
    double xr = creal(x), xi = cimag(x);
    return sin(xr)*cosh(xi) + cos(xr)*sinh(xi)*I; /* A&S 4.3.55 */
}
#endif

#ifndef HAVE_CTAN
#define ctan R_ctan
static double complex ctan(double complex z)
{
    /* A&S 4.3.57 */
    double x2, y2, den, ri;
    x2 = 2.0 * creal(z);
    y2 = 2.0 * cimag(z);
    den = cos(x2) + cosh(y2);
    /* any threshold between -log(DBL_EPSILON) and log(DBL_XMAX) will do*/
    if (ISNAN(y2) || fabs(y2) < 50.0) ri = sinh(y2)/den;
    else ri = (y2 < 0 ? -1.0 : 1.0);
    return sin(x2)/den + ri * I;
}
#endif

#ifndef HAVE_CASIN
#define casin R_casin
static double complex casin(double complex z)
{
    /* A&S 4.4.37 */
    double alpha, t1, t2, x = creal(z), y = cimag(z), ri;
    t1 = 0.5 * hypot(x + 1, y);
    t2 = 0.5 * hypot(x - 1, y);
    alpha = t1 + t2;
    ri = log(alpha + sqrt(alpha*alpha - 1));
    /* This comes from 
       'z_asin() is continuous from below if x >= 1
        and continuous from above if x <= -1.'
    */
    if(y < 0 || (y == 0 && x > 1)) ri *= -1;
    return asin(t1  - t2) + ri*I;
}
#endif

#ifndef HAVE_CACOS
#define cacos R_cacos
static double complex cacos(double complex z)
{
    return M_PI_2 - casin(z);
}
#endif

#ifndef HAVE_CATAN
#define catan R_catan
static double complex catan(double complex z)
{
    double x = creal(z), y = cimag(z), rr, ri;
    rr = 0.5 * atan2(2 * x, (1 - x * x - y * y));
    ri = 0.25 * log((x * x + (y + 1) * (y + 1)) /
		    (x * x + (y - 1) * (y - 1)));
    return rr + ri*I;
}
#endif

#ifndef HAVE_CCOSH
#define ccosh R_ccosh
static double complex ccosh(double complex z)
{
    return ccos(z * I); /* A&S 4.5.8 */
}
#endif

#ifndef HAVE_CSINH
#define csinh R_csinh
static double complex csinh(double complex z)
{
    return -I * csin(z * I); /* A&S 4.5.7 */
}
#endif

#ifndef HAVE_CTANH
#define ctanh R_ctanh
static double complex ctanh(double complex z)
{
    return -I * ctan(z * I); /* A&S 4.5.9 */
}
#endif

static double complex z_tan(double complex z)
{
    double y = cimag(z);
    double complex r = ctan(z);
    if(R_FINITE(y) && fabs(y) > 25.0) {
	/* at this point the real part is nearly zero, and the
	   imaginary part is one: but some OSes get the imag as NaN */
#if __GNUC__
	__imag__ r = y < 0 ? -1.0 : 1.0;
#else
	r = creal(r) + (y < 0 ? -1.0 : 1.0) * I;
#endif
    }
    return r;
}

/* Don't rely on the OS at the branch cuts */

static double complex z_asin(double complex z)
{
    if(cimag(z) == 0 && fabs(creal(z)) > 1) {
	double alpha, t1, t2, x = creal(z), ri;
	t1 = 0.5 * fabs(x + 1);
	t2 = 0.5 * fabs(x - 1);
	alpha = t1 + t2;
	ri = log(alpha + sqrt(alpha*alpha - 1));
	if(x > 1) ri *= -1;
	return asin(t1  - t2) + ri*I;
    }
    return casin(z);
}

static double complex z_acos(double complex z)
{
    if(cimag(z) == 0 && fabs(creal(z)) > 1) return M_PI_2 - z_asin(z);
    return cacos(z);
}

static double complex z_atan(double complex z)
{
    if(creal(z) == 0 && fabs(cimag(z)) > 1) {
	double y = cimag(z), rr, ri;
	rr = (y > 0) ? M_PI_2 : -M_PI_2;
	ri = 0.25 * log(((y + 1) * (y + 1))/((y - 1) * (y - 1)));
	return rr + ri*I;
    }
    return catan(z);
}

static double complex z_acosh(double complex z)
{
    return z_acos(z) * I;
}

static double complex z_asinh(double complex z)
{
    return -I * z_asin(z * I);
}

static double complex z_atanh(double complex z)
{
    return -I * z_atan(z * I);
}

static Rboolean cmath1(double complex (*f)(double complex),
		       Rcomplex *x, Rcomplex *y, R_xlen_t n)
{
    R_xlen_t i;
    Rboolean naflag = FALSE;
    for (i = 0 ; i < n ; i++) {
	if (ISNA(x[i].r) || ISNA(x[i].i)) {
	    y[i].r = NA_REAL; y[i].i = NA_REAL;
	} else {
	    SET_C99_COMPLEX(y, i, f(toC99(x + i)));
	    if ( (ISNAN(y[i].r) || ISNAN(y[i].i)) &&
		!(ISNAN(x[i].r) || ISNAN(x[i].i)) ) naflag = TRUE;
	}
    }
    return naflag;
}

SEXP attribute_hidden complex_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    R_xlen_t n;
    Rboolean naflag = FALSE;

    PROTECT(x = CAR(args));
    n = xlength(x);
    PROTECT(y = allocVector(CPLXSXP, n));

    switch (PRIMVAL(op)) {
    case 10003: naflag = cmath1(clog, COMPLEX(x), COMPLEX(y), n); break;
    case 3: naflag = cmath1(csqrt, COMPLEX(x), COMPLEX(y), n); break;
    case 10: naflag = cmath1(cexp, COMPLEX(x), COMPLEX(y), n); break;
    case 20: naflag = cmath1(ccos, COMPLEX(x), COMPLEX(y), n); break;
    case 21: naflag = cmath1(csin, COMPLEX(x), COMPLEX(y), n); break;
    case 22: naflag = cmath1(z_tan, COMPLEX(x), COMPLEX(y), n); break;
    case 23: naflag = cmath1(z_acos, COMPLEX(x), COMPLEX(y), n); break;
    case 24: naflag = cmath1(z_asin, COMPLEX(x), COMPLEX(y), n); break;
    case 25: naflag = cmath1(z_atan, COMPLEX(x), COMPLEX(y), n); break;
    case 30: naflag = cmath1(ccosh, COMPLEX(x), COMPLEX(y), n); break;
    case 31: naflag = cmath1(csinh, COMPLEX(x), COMPLEX(y), n); break;
    case 32: naflag = cmath1(ctanh, COMPLEX(x), COMPLEX(y), n); break;
    case 33: naflag = cmath1(z_acosh, COMPLEX(x), COMPLEX(y), n); break;
    case 34: naflag = cmath1(z_asinh, COMPLEX(x), COMPLEX(y), n); break;
    case 35: naflag = cmath1(z_atanh, COMPLEX(x), COMPLEX(y), n); break;

    default:
	/* such as sign, gamma */
	errorcall(call, _("unimplemented complex function"));
    }
    if (naflag)
	warningcall(call, "NaNs produced in function \"%s\"", PRIMNAME(op));
    DUPLICATE_ATTRIB(y, x);
    UNPROTECT(2);
    return y;
}

static void z_rround(Rcomplex *r, Rcomplex *x, Rcomplex *p)
{
    r->r = fround(x->r, p->r);
    r->i = fround(x->i, p->r);
}

static void z_prec(Rcomplex *r, Rcomplex *x, Rcomplex *p)
{
    z_prec_r(r, x, p->r);
}

static void z_logbase(Rcomplex *r, Rcomplex *z, Rcomplex *base)
{
    double complex dz = toC99(z), dbase = toC99(base);
    SET_C99_COMPLEX(r, 0, clog(dz)/clog(dbase));
}

static void z_atan2(Rcomplex *r, Rcomplex *csn, Rcomplex *ccs)
{
    double complex dr, dcsn = toC99(csn), dccs = toC99(ccs);
    if (dccs == 0) {
	if(dcsn == 0) {
	    r->r = NA_REAL; r->i = NA_REAL; /* Why not R_NaN? */
	    return;
	} else {
	    double y = creal(dcsn);
	    if (ISNAN(y)) dr = y;
	    else dr = ((y >= 0) ? M_PI_2 : -M_PI_2);
	}
    } else {
	dr = catan(dcsn / dccs);
	if(creal(dccs) < 0) dr += M_PI;
	if(creal(dr) > M_PI) dr -= 2 * M_PI;
    }
    SET_C99_COMPLEX(r, 0, dr);
}


	/* Complex Functions of Two Arguments */

typedef void (*cm2_fun)(Rcomplex *, Rcomplex *, Rcomplex *);
SEXP attribute_hidden complex_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    R_xlen_t i, n, na, nb;
    Rcomplex ai, bi, *a, *b, *y;
    SEXP sa, sb, sy;
    Rboolean naflag = FALSE;
    cm2_fun f;

    switch (PRIMVAL(op)) {
    case 0: /* atan2 */
	f = z_atan2; break;
    case 10001: /* round */
	f = z_rround; break;
    case 2: /* passed from do_log1arg */
    case 10:
    case 10003: /* passed from do_log */
	f = z_logbase; break;
    case 10004: /* signif */
	f = z_prec; break;
    default:
	errorcall_return(call, _("unimplemented complex function"));
    }

    PROTECT(sa = coerceVector(CAR(args), CPLXSXP));
    PROTECT(sb = coerceVector(CADR(args), CPLXSXP));
    na = XLENGTH(sa); nb = XLENGTH(sb);
    if ((na == 0) || (nb == 0)) return(allocVector(CPLXSXP, 0));
    n = (na < nb) ? nb : na;
    PROTECT(sy = allocVector(CPLXSXP, n));
    a = COMPLEX(sa); b = COMPLEX(sb); y = COMPLEX(sy);
    for (i = 0; i < n; i++) {
	ai = a[i % na]; bi = b[i % nb];
	if(ISNA(ai.r) && ISNA(ai.i) &&
	   ISNA(bi.r) && ISNA(bi.i)) {
	    y[i].r = NA_REAL; y[i].i = NA_REAL;
	} else {
	    f(&y[i], &ai, &bi);
	    if ( (ISNAN(y[i].r) || ISNAN(y[i].i)) &&
		 !(ISNAN(ai.r) || ISNAN(ai.i) || ISNAN(bi.r) || ISNAN(bi.i)) )
		naflag = TRUE;
	}
    }
    if (naflag)
	warningcall(call, "NaNs produced in function \"%s\"", PRIMNAME(op));
    if(n == na) {
	DUPLICATE_ATTRIB(sy, sa);
    } else if(n == nb) {
	DUPLICATE_ATTRIB(sy, sb);
    }
    UNPROTECT(3);
    return sy;
}

SEXP attribute_hidden do_complex(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* complex(length, real, imaginary) */
    SEXP ans, re, im;
    R_xlen_t i, na, nr, ni;
    na = asInteger(CAR(args));
    if(na == NA_INTEGER || na < 0)
	error(_("invalid length"));
    PROTECT(re = coerceVector(CADR(args), REALSXP));
    PROTECT(im = coerceVector(CADDR(args), REALSXP));
    nr = XLENGTH(re);
    ni = XLENGTH(im);
    /* is always true: if (na >= 0) {*/
    na = (nr > na) ? nr : na;
    na = (ni > na) ? ni : na;
    /* }*/
    ans = allocVector(CPLXSXP, na);
    for(i=0 ; i<na ; i++) {
	COMPLEX(ans)[i].r = 0;
	COMPLEX(ans)[i].i = 0;
    }
    UNPROTECT(2);
    if(na > 0 && nr > 0) {
	for(i=0 ; i<na ; i++)
	    COMPLEX(ans)[i].r = REAL(re)[i%nr];
    }
    if(na > 0 && ni > 0) {
	for(i=0 ; i<na ; i++)
	    COMPLEX(ans)[i].i = REAL(im)[i%ni];
    }
    return ans;
}

static void R_cpolyroot(double *opr, double *opi, int *degree,
			double *zeror, double *zeroi, Rboolean *fail);

SEXP attribute_hidden do_polyroot(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP z, zr, zi, r, rr, ri;
    Rboolean fail;
    int degree, i, n;

    checkArity(op, args);
    z = CAR(args);
    switch(TYPEOF(z)) {
    case CPLXSXP:
	PROTECT(z);
	break;
    case REALSXP:
    case INTSXP:
    case LGLSXP:
	PROTECT(z = coerceVector(z, CPLXSXP));
	break;
    default:
	UNIMPLEMENTED_TYPE("polyroot", z);
    }
#ifdef LONG_VECTOR_SUPPORT
    R_xlen_t nn = XLENGTH(z);
    if (nn > R_SHORT_LEN_MAX) error("long vectors are not supported");
    n = (int) nn;
#else
    n = LENGTH(z);
#endif
    degree = 0;
    for(i = 0; i < n; i++) {
	if(COMPLEX(z)[i].r!= 0.0 || COMPLEX(z)[i].i != 0.0) degree = i;
    }
    n = degree + 1; /* omit trailing zeroes */
    if(degree >= 1) {
	PROTECT(rr = allocVector(REALSXP, n));
	PROTECT(ri = allocVector(REALSXP, n));
	PROTECT(zr = allocVector(REALSXP, n));
	PROTECT(zi = allocVector(REALSXP, n));

	for(i = 0 ; i < n ; i++) {
	    if(!R_FINITE(COMPLEX(z)[i].r) || !R_FINITE(COMPLEX(z)[i].i))
		error(_("invalid polynomial coefficient"));
	    REAL(zr)[degree-i] = COMPLEX(z)[i].r;
	    REAL(zi)[degree-i] = COMPLEX(z)[i].i;
	}
	R_cpolyroot(REAL(zr), REAL(zi), &degree, REAL(rr), REAL(ri), &fail);
	if(fail) error(_("root finding code failed"));
	UNPROTECT(2);
	r = allocVector(CPLXSXP, degree);
	for(i = 0 ; i < degree ; i++) {
	    COMPLEX(r)[i].r = REAL(rr)[i];
	    COMPLEX(r)[i].i = REAL(ri)[i];
	}
	UNPROTECT(3);
    }
    else {
	UNPROTECT(1);
	r = allocVector(CPLXSXP, 0);
    }
    return r;
}

/* Formerly src/appl/cpoly.c:
 *
 *  Copyright (C) 1997-1998 Ross Ihaka
 *  Copyright (C) 1999-2001 R Core Team
 *
 *	cpoly finds the zeros of a complex polynomial.
 *
 *	On Entry
 *
 *	opr, opi      -	 double precision vectors of real and
 *			 imaginary parts of the coefficients in
 *			 order of decreasing powers.
 *
 *	degree	      -	 int degree of polynomial.
 *
 *
 *	On Return
 *
 *	zeror, zeroi  -	 output double precision vectors of
 *			 real and imaginary parts of the zeros.
 *
 *	fail	      -	 output int parameter,	true  only if
 *			 leading coefficient is zero or if cpoly
 *			 has found fewer than degree zeros.
 *
 *	The program has been written to reduce the chance of overflow
 *	occurring. If it does occur, there is still a possibility that
 *	the zerofinder will work provided the overflowed quantity is
 *	replaced by a large number.
 *
 *	This is a C translation of the following.
 *
 *	TOMS Algorithm 419
 *	Jenkins and Traub.
 *	Comm. ACM 15 (1972) 97-99.
 *
 *	Ross Ihaka
 *	February 1997
 */

#include <R_ext/Arith.h> /* for declaration of hypot */
#include <R_ext/Memory.h> /* for declaration of R_alloc */

#include <float.h> /* for FLT_RADIX */

#include <Rmath.h> /* for R_pow_di */

static void calct(Rboolean *);
static Rboolean fxshft(int, double *, double *);
static Rboolean vrshft(int, double *, double *);
static void nexth(Rboolean);
static void noshft(int);

static void polyev(int, double, double,
		   double *, double *, double *, double *, double *, double *);
static double errev(int, double *, double *, double, double, double, double);
static double cpoly_cauchy(int, double *, double *);
static double cpoly_scale(int, double *, double, double, double, double);
static void cdivid(double, double, double, double, double *, double *);

/* Global Variables (too many!) */

static int nn;
static double *pr, *pi, *hr, *hi, *qpr, *qpi, *qhr, *qhi, *shr, *shi;
static double sr, si;
static double tr, ti;
static double pvr, pvi;

static const double eta =  DBL_EPSILON;
static const double are = /* eta = */DBL_EPSILON;
static const double mre = 2. * M_SQRT2 * /* eta, i.e. */DBL_EPSILON;
static const double infin = DBL_MAX;

static void R_cpolyroot(double *opr, double *opi, int *degree,
			double *zeror, double *zeroi, Rboolean *fail)
{
    static const double smalno = DBL_MIN;
    static const double base = (double)FLT_RADIX;
    static int d_n, i, i1, i2;
    static double zi, zr, xx, yy;
    static double bnd, xxx;
    Rboolean conv;
    int d1;
    double *tmp;
    static const double cosr =/* cos 94 */ -0.06975647374412529990;
    static const double sinr =/* sin 94 */  0.99756405025982424767;
    xx = M_SQRT1_2;/* 1/sqrt(2) = 0.707.... */

    yy = -xx;
    *fail = FALSE;

    nn = *degree;
    d1 = nn - 1;

    /* algorithm fails if the leading coefficient is zero. */

    if (opr[0] == 0. && opi[0] == 0.) {
	*fail = TRUE;
	return;
    }

    /* remove the zeros at the origin if any. */

    while (opr[nn] == 0. && opi[nn] == 0.) {
	d_n = d1-nn+1;
	zeror[d_n] = 0.;
	zeroi[d_n] = 0.;
	nn--;
    }
    nn++;
    /*-- Now, global var.  nn := #{coefficients} = (relevant degree)+1 */

    if (nn == 1) return;

    /* Use a single allocation as these as small */
    const void *vmax = vmaxget();
    tmp = (double *) R_alloc((size_t) (10*nn), sizeof(double));
    pr = tmp; pi = tmp + nn; hr = tmp + 2*nn; hi = tmp + 3*nn;
    qpr = tmp + 4*nn; qpi = tmp + 5*nn; qhr = tmp + 6*nn; qhi = tmp + 7*nn;
    shr = tmp + 8*nn; shi = tmp + 9*nn;
    
    /* make a copy of the coefficients and shr[] = | p[] | */
    for (i = 0; i < nn; i++) {
	pr[i] = opr[i];
	pi[i] = opi[i];
	shr[i] = hypot(pr[i], pi[i]);
    }

    /* scale the polynomial with factor 'bnd'. */
    bnd = cpoly_scale(nn, shr, eta, infin, smalno, base);
    if (bnd != 1.) {
	for (i=0; i < nn; i++) {
	    pr[i] *= bnd;
	    pi[i] *= bnd;
	}
    }

    /* start the algorithm for one zero */

    while (nn > 2) {

	/* calculate bnd, a lower bound on the modulus of the zeros. */

	for (i=0 ; i < nn ; i++)
	    shr[i] = hypot(pr[i], pi[i]);
	bnd = cpoly_cauchy(nn, shr, shi);

	/* outer loop to control 2 major passes */
	/* with different sequences of shifts */

	for (i1 = 1; i1 <= 2; i1++) {

	    /* first stage calculation, no shift */

	    noshft(5);

	    /*	inner loop to select a shift */
	    for (i2 = 1; i2 <= 9; i2++) {

		/* shift is chosen with modulus bnd */
		/* and amplitude rotated by 94 degrees */
		/* from the previous shift */

		xxx= cosr * xx - sinr * yy;
		yy = sinr * xx + cosr * yy;
		xx = xxx;
		sr = bnd * xx;
		si = bnd * yy;

		/*  second stage calculation, fixed shift */

		conv = fxshft(i2 * 10, &zr, &zi);
		if (conv)
		    goto L10;
	    }
	}

	/* the zerofinder has failed on two major passes */
	/* return empty handed */

	*fail = TRUE;
	vmaxset(vmax);
	return;

	/* the second stage jumps directly to the third stage iteration.
	 * if successful, the zero is stored and the polynomial deflated.
	 */
    L10:
	d_n = d1+2 - nn;
	zeror[d_n] = zr;
	zeroi[d_n] = zi;
	--nn;
	for (i=0; i < nn ; i++) {
	    pr[i] = qpr[i];
	    pi[i] = qpi[i];
	}
    }/*while*/

    /*	calculate the final zero and return */
    cdivid(-pr[1], -pi[1], pr[0], pi[0], &zeror[d1], &zeroi[d1]);

    vmaxset(vmax);
    return;
}


/*  Computes the derivative polynomial as the initial
 *  polynomial and computes l1 no-shift h polynomials.	*/

static void noshft(int l1)
{
    int i, j, jj, n = nn - 1, nm1 = n - 1;

    double t1, t2, xni;

    for (i=0; i < n; i++) {
	xni = (double)(nn - i - 1);
	hr[i] = xni * pr[i] / n;
	hi[i] = xni * pi[i] / n;
    }

    for (jj = 1; jj <= l1; jj++) {

	if (hypot(hr[n-1], hi[n-1]) <=
	    eta * 10.0 * hypot(pr[n-1], pi[n-1])) {
	    /*	If the constant term is essentially zero, */
	    /*	shift h coefficients. */

	    for (i = 1; i <= nm1; i++) {
		j = nn - i;
		hr[j-1] = hr[j-2];
		hi[j-1] = hi[j-2];
	    }
	    hr[0] = 0.;
	    hi[0] = 0.;
	}
	else {
	    cdivid(-pr[nn-1], -pi[nn-1], hr[n-1], hi[n-1], &tr, &ti);
	    for (i = 1; i <= nm1; i++) {
		j = nn - i;
		t1 = hr[j-2];
		t2 = hi[j-2];
		hr[j-1] = tr * t1 - ti * t2 + pr[j-1];
		hi[j-1] = tr * t2 + ti * t1 + pi[j-1];
	    }
	    hr[0] = pr[0];
	    hi[0] = pi[0];
	}
    }
}


/*  Computes l2 fixed-shift h polynomials and tests for convergence.
 *  initiates a variable-shift iteration and returns with the
 *  approximate zero if successful.
 */
static Rboolean fxshft(int l2, double *zr, double *zi)
{
/*  l2	  - limit of fixed shift steps
 *  zr,zi - approximate zero if convergence (result TRUE)
 *
 * Return value indicates convergence of stage 3 iteration
 *
 * Uses global (sr,si), nn, pr[], pi[], .. (all args of polyev() !)
*/

    Rboolean pasd, bool, test;
    static double svsi, svsr;
    static int i, j, n;
    static double oti, otr;

    n = nn - 1;

    /* evaluate p at s. */

    polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);

    test = TRUE;
    pasd = FALSE;

    /* calculate first t = -p(s)/h(s). */

    calct(&bool);

    /* main loop for one second stage step. */

    for (j=1; j<=l2; j++) {

	otr = tr;
	oti = ti;

	/* compute next h polynomial and new t. */

	nexth(bool);
	calct(&bool);
	*zr = sr + tr;
	*zi = si + ti;

	/* test for convergence unless stage 3 has */
	/* failed once or this is the last h polynomial. */

	if (!bool && test && j != l2) {
	    if (hypot(tr - otr, ti - oti) >= hypot(*zr, *zi) * 0.5) {
		pasd = FALSE;
	    }
	    else if (! pasd) {
		pasd = TRUE;
	    }
	    else {

		/* the weak convergence test has been */
		/* passed twice, start the third stage */
		/* iteration, after saving the current */
		/* h polynomial and shift. */

		for (i = 0; i < n; i++) {
		    shr[i] = hr[i];
		    shi[i] = hi[i];
		}
		svsr = sr;
		svsi = si;
		if (vrshft(10, zr, zi)) {
		    return TRUE;
		}

		/* the iteration failed to converge. */
		/* turn off testing and restore */
		/* h, s, pv and t. */

		test = FALSE;
		for (i=1 ; i<=n ; i++) {
		    hr[i-1] = shr[i-1];
		    hi[i-1] = shi[i-1];
		}
		sr = svsr;
		si = svsi;
		polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);
		calct(&bool);
	    }
	}
    }

    /* attempt an iteration with final h polynomial */
    /* from second stage. */

    return(vrshft(10, zr, zi));
}


/* carries out the third stage iteration.
 */
static Rboolean vrshft(int l3, double *zr, double *zi)
{
/*  l3	    - limit of steps in stage 3.
 *  zr,zi   - on entry contains the initial iterate;
 *	      if the iteration converges it contains
 *	      the final iterate on exit.
 * Returns TRUE if iteration converges
 *
 * Assign and uses  GLOBAL sr, si
*/
    Rboolean bool, b;
    static int i, j;
    static double r1, r2, mp, ms, tp, relstp;
    static double omp;

    b = FALSE;
    sr = *zr;
    si = *zi;

    /* main loop for stage three */

    for (i = 1; i <= l3; i++) {

	/* evaluate p at s and test for convergence. */
	polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);

	mp = hypot(pvr, pvi);
	ms = hypot(sr, si);
	if (mp <=  20. * errev(nn, qpr, qpi, ms, mp, /*are=*/eta, mre)) {
	    goto L_conv;
	}

	/* polynomial value is smaller in value than */
	/* a bound on the error in evaluating p, */
	/* terminate the iteration. */

	if (i != 1) {

	    if (!b && mp >= omp && relstp < .05) {

		/* iteration has stalled. probably a */
		/* cluster of zeros. do 5 fixed shift */
		/* steps into the cluster to force */
		/* one zero to dominate. */

		tp = relstp;
		b = TRUE;
		if (relstp < eta)
		    tp = eta;
		r1 = sqrt(tp);
		r2 = sr * (r1 + 1.) - si * r1;
		si = sr * r1 + si * (r1 + 1.);
		sr = r2;
		polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);
		for (j = 1; j <= 5; ++j) {
		    calct(&bool);
		    nexth(bool);
		}
		omp = infin;
		goto L10;
	    }
	    else {

		/* exit if polynomial value */
		/* increases significantly. */

		if (mp * .1 > omp)
		    return FALSE;
	    }
	}
	omp = mp;

	/* calculate next iterate. */

    L10:
	calct(&bool);
	nexth(bool);
	calct(&bool);
	if (!bool) {
	    relstp = hypot(tr, ti) / hypot(sr, si);
	    sr += tr;
	    si += ti;
	}
    }
    return FALSE;

L_conv:
    *zr = sr;
    *zi = si;
    return TRUE;
}

static void calct(Rboolean *bool)
{
    /* computes	 t = -p(s)/h(s).
     * bool   - logical, set true if h(s) is essentially zero.	*/

    int n = nn - 1;
    double hvi, hvr;

    /* evaluate h(s). */
    polyev(n, sr, si, hr, hi,
	   qhr, qhi, &hvr, &hvi);

    *bool = hypot(hvr, hvi) <= are * 10. * hypot(hr[n-1], hi[n-1]);
    if (!*bool) {
	cdivid(-pvr, -pvi, hvr, hvi, &tr, &ti);
    }
    else {
	tr = 0.;
	ti = 0.;
    }
}

static void nexth(Rboolean bool)
{
    /* calculates the next shifted h polynomial.
     * bool :	if TRUE  h(s) is essentially zero
     */
    int j, n = nn - 1;
    double t1, t2;

    if (!bool) {
	for (j=1; j < n; j++) {
	    t1 = qhr[j - 1];
	    t2 = qhi[j - 1];
	    hr[j] = tr * t1 - ti * t2 + qpr[j];
	    hi[j] = tr * t2 + ti * t1 + qpi[j];
	}
	hr[0] = qpr[0];
	hi[0] = qpi[0];
    }
    else {
	/* if h(s) is zero replace h with qh. */

	for (j=1; j < n; j++) {
	    hr[j] = qhr[j-1];
	    hi[j] = qhi[j-1];
	}
	hr[0] = 0.;
	hi[0] = 0.;
    }
}

/*--------------------- Independent Complex Polynomial Utilities ----------*/

static
void polyev(int n,
	    double s_r, double s_i,
	    double *p_r, double *p_i,
	    double *q_r, double *q_i,
	    double *v_r, double *v_i)
{
    /* evaluates a polynomial  p  at  s	 by the horner recurrence
     * placing the partial sums in q and the computed value in v_.
     */
    int i;
    double t;

    q_r[0] = p_r[0];
    q_i[0] = p_i[0];
    *v_r = q_r[0];
    *v_i = q_i[0];
    for (i = 1; i < n; i++) {
	t = *v_r * s_r - *v_i * s_i + p_r[i];
	q_i[i] = *v_i = *v_r * s_i + *v_i * s_r + p_i[i];
	q_r[i] = *v_r = t;
    }
}

static
double errev(int n, double *qr, double *qi,
	     double ms, double mp, double a_re, double m_re)
{
    /*	bounds the error in evaluating the polynomial by the horner
     *	recurrence.
     *
     *	qr,qi	 - the partial sum vectors
     *	ms	 - modulus of the point
     *	mp	 - modulus of polynomial value
     * a_re,m_re - error bounds on complex addition and multiplication
     */
    double e;
    int i;

    e = hypot(qr[0], qi[0]) * m_re / (a_re + m_re);
    for (i=0; i < n; i++)
	e = e*ms + hypot(qr[i], qi[i]);

    return e * (a_re + m_re) - mp * m_re;
}


static
double cpoly_cauchy(int n, double *pot, double *q)
{
    /* Computes a lower bound on the moduli of the zeros of a polynomial
     * pot[1:nn] is the modulus of the coefficients.
     */
    double f, x, delf, dx, xm;
    int i, n1 = n - 1;

    pot[n1] = -pot[n1];

    /* compute upper estimate of bound. */

    x = exp((log(-pot[n1]) - log(pot[0])) / (double) n1);

    /* if newton step at the origin is better, use it. */

    if (pot[n1-1] != 0.) {
	xm = -pot[n1] / pot[n1-1];
	if (xm < x)
	    x = xm;
    }

    /* chop the interval (0,x) unitl f le 0. */

    for(;;) {
	xm = x * 0.1;
	f = pot[0];
	for (i = 1; i < n; i++)
	    f = f * xm + pot[i];
	if (f <= 0.0) {
	    break;
	}
	x = xm;
    }

    dx = x;

    /* do Newton iteration until x converges to two decimal places. */

    while (fabs(dx / x) > 0.005) {
	q[0] = pot[0];
	for(i = 1; i < n; i++)
	    q[i] = q[i-1] * x + pot[i];
	f = q[n1];
	delf = q[0];
	for(i = 1; i < n1; i++)
	    delf = delf * x + q[i];
	dx = f / delf;
	x -= dx;
    }
    return x;
}

static
double cpoly_scale(int n, double *pot,
		   double eps, double BIG, double small, double base)
{
    /* Returns a scale factor to multiply the coefficients of the polynomial.
     * The scaling is done to avoid overflow and to avoid
     *	undetected underflow interfering with the convergence criterion.
     * The factor is a power of the base.

     * pot[1:n] : modulus of coefficients of p
     * eps,BIG,
     * small,base - constants describing the floating point arithmetic.
     */

    int i, ell;
    double x, high, sc, lo, min_, max_;

    /* find largest and smallest moduli of coefficients. */
    high = sqrt(BIG);
    lo = small / eps;
    max_ = 0.;
    min_ = BIG;
    for (i = 0; i < n; i++) {
	x = pot[i];
	if (x > max_) max_ = x;
	if (x != 0. && x < min_)
	    min_ = x;
    }

    /* scale only if there are very large or very small components. */

    if (min_ < lo || max_ > high) {
	x = lo / min_;
	if (x <= 1.)
	    sc = 1. / (sqrt(max_) * sqrt(min_));
	else {
	    sc = x;
	    if (BIG / sc > max_)
		sc = 1.0;
	}
	ell = (int) (log(sc) / log(base) + 0.5);
	return R_pow_di(base, ell);
    }
    else return 1.0;
}


static
void cdivid(double ar, double ai, double br, double bi,
	    double *cr, double *ci)
{
/* complex division c = a/b, i.e., (cr +i*ci) = (ar +i*ai) / (br +i*bi),
   avoiding overflow. */

    double d, r;

    if (br == 0. && bi == 0.) {
	/* division by zero, c = infinity. */
	*cr = *ci = R_PosInf;
    }
    else if (fabs(br) >= fabs(bi)) {
	r = bi / br;
	d = br + r * bi;
	*cr = (ar + ai * r) / d;
	*ci = (ai - ar * r) / d;
    }
    else {
	r = br / bi;
	d = bi + r * br;
	*cr = (ar * r + ai) / d;
	*ci = (ai * r - ar) / d;
    }
}

/* static double cpoly_cmod(double *r, double *i)
 * --> replaced by hypot() everywhere
*/
