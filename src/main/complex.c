/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000-11	    The R Development Core Team.
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

#include <Defn.h>		/* -> ../include/R_ext/Complex.h */
#include <Rmath.h>
#include <R_ext/Applic.h>	/* R_cpoly */

static R_INLINE double fsign_int(double x, double y)
{
    if (ISNAN(x) || ISNAN(y))
	return x + y;
    return ((y >= 0) ? fabs(x) : -fabs(x));
}


#include "arithmetic.h"		/* complex_*  */
#include <complex.h>

#ifdef HAVE_COMPATIBLE_C99_COMPLEX
# define C99_COMPLEX(x) ((double complex *) COMPLEX(x))
# define toC99(x) *((double complex *) x)
# define C99_COMPLEX2(x, i) (((double complex *) COMPLEX(x))[i])
static R_INLINE void 
SET_C99_COMPLEX(Rcomplex *x, int i, double complex value)
{
    ((double complex *) x)[i] = value;
}
#else
static R_INLINE double complex toC99(Rcomplex *x)
{
#if __GNUC__
    double complex ans = (double complex) 0; /* -Wall */
    __real__ ans = x->r;
    __imag__ ans = x->i;
    return ans;
#else
    /* gcc 4.2.1 incorrectly reports this as a GNU extension: it is C99.
       And it seems that gcc on some platforms (e.g. AIX) does not
       handle this correctly with the platform's complex.h. */
    return x->r + x->i * I;
#endif
}
static R_INLINE double complex C99_COMPLEX2(SEXP x, int i)
{
    Rcomplex *r = COMPLEX(x) + i;
    return toC99(r);
}
static R_INLINE void 
SET_C99_COMPLEX(Rcomplex *x, int i, double complex value)
{
    Rcomplex *ans = x+i;
    ans->r = creal(value);
    ans->i = cimag(value);
}
#endif

SEXP attribute_hidden complex_unary(ARITHOP_TYPE code, SEXP s1, SEXP call)
{
    int i, n;
    SEXP ans;

    switch(code) {
    case PLUSOP:
	return s1;
    case MINUSOP:

	ans = duplicate(s1);
	n = LENGTH(s1);
	for (i = 0; i < n; i++) {
	    /* Maybe not worth optimizing */
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
	    C99_COMPLEX(ans)[i] = - C99_COMPLEX(s1)[i];
#else
	    Rcomplex x = COMPLEX(s1)[i];
	    COMPLEX(ans)[i].r = -x.r;
	    COMPLEX(ans)[i].i = -x.i;
#endif
	}
	return ans;
    default:
	errorcall(call, _("invalid complex unary operator"));
    }
    return R_NilValue; /* -Wall */
}

static double complex R_cpow_n(double complex X, int k) {
    if(k == 0) return (double complex) 1.;
    else if(k == 1) return X;
    else if(k < 0) return 1. / R_cpow_n(X, -k);
    else {/* k > 0 */
	double complex z = (double complex) 1.;;
	while (k > 0) {
	    if (k & 1) z = z * X;
	    if(k == 1) break;
	    k >>= 1; /* efficient division by 2; now have k >= 1 */
	    X = X * X;
	}
	return z;
    }
}

#ifdef Win32
/* Need this because the system one is explicitly linked
   against MSVCRT's pow, and gets (0+0i)^Y as 0+0i for all Y */
static double complex mycpow (double complex X, double complex Y)
{
    double complex Res; int k;
    if (X == 0.0) {
	if(cimag(Y) == 0.0){
	    __real__ Res = R_pow(0.0, __real__ Y);
	    __imag__ Res = 0.0;
	} else {
	    __real__ Res = R_NaN;
	    __imag__ Res = R_NaN;
	}
    } else if (__imag__ Y == 0.0 && __real__ Y == (k = (int)__real__ Y)
	       && abs(k) <= 65536) {
	return R_cpow_n(X, k);
    } else {
	double rho, r,i, theta;
	r = hypot (__real__ X, __imag__ X);
	i = carg (X);
	theta = i * __real__ Y;

	if (__imag__ Y == 0.0)
	    rho = pow (r, __real__ Y);
	else {
	    r = log (r);
	    /* rearrangement of cexp(X * clog(Y)) */
	    theta += r * __imag__ Y;
	    rho = exp (r * __real__ Y - i * __imag__ Y);
	}
	__real__ Res = rho * cos (theta);
	__imag__ Res = rho * sin (theta);
    }
    return  Res;
}
#else /* not Win32 */
/* reason for this:
 * 1) glibc gets (0+0i)^y = Inf+NaNi for y < 0
 * 2)   X ^ n  (e.g. for n = 2, 3)  is unnecessarily inaccurate;
 *	cut-off 65536 : guided from empirical speed measurements
*/

static double complex mycpow (double complex X, double complex Y)
{
    double iY = cimag(Y);
    if (iY == 0.0) {/* X ^ <real> */
	double complex Z; int k;
	if (X == 0.0) {
	    Z = R_pow(0., creal(Y));
	} else if(creal(Y) == (k = (int)creal(Y)) && abs(k) <= 65536) {
	    Z = R_cpow_n(X, k);
	} else {
	    Z = cpow(X, Y);
	}
	return Z;
    } else
	return cpow(X, Y);
}
#endif

/* See arithmetic.c */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

SEXP attribute_hidden complex_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
{
    int i,i1, i2, n, n1, n2;
    SEXP ans;

    /* Note: "s1" and "s1" are protected in the calling code. */
    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
     /* S4-compatibility change: if n1 or n2 is 0, result is of length 0 */
    if (n1 == 0 || n2 == 0) return(allocVector(CPLXSXP, 0));

    n = (n1 > n2) ? n1 : n2;
    ans = allocVector(CPLXSXP, n);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(s1) || RTRACE(s2)){
       if (RTRACE(s1) && RTRACE(s2)){
	  if (n1>n2)
	      memtrace_report(s1,ans);
	  else
	      memtrace_report(s2, ans);
       } else if (RTRACE(s1))
	   memtrace_report(s1,ans);
       else /* only s2 */
	   memtrace_report(s2, ans);
       SET_RTRACE(ans, 1);
    }
#endif

    switch (code) {
    case PLUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    /* Maybe not worth optimizing */
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
	    C99_COMPLEX(ans)[i] = C99_COMPLEX(s1)[i1] + C99_COMPLEX(s2)[i2];
#else
	    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
	    COMPLEX(ans)[i].r = x1.r + x2.r;
	    COMPLEX(ans)[i].i = x1.i + x2.i;
#endif
	}
	break;
    case MINUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    /* Maybe not worth optimizing */
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
	    C99_COMPLEX(ans)[i] = C99_COMPLEX(s1)[i1] - C99_COMPLEX(s2)[i2];
#else
	    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
	    COMPLEX(ans)[i].r = x1.r - x2.r;
	    COMPLEX(ans)[i].i = x1.i - x2.i;
#endif
	}
	break;
    case TIMESOP:
	mod_iterate(n1, n2, i1, i2) {
	    /* Maybe not worth optimizing */
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
	    C99_COMPLEX(ans)[i] = C99_COMPLEX(s1)[i1] * C99_COMPLEX(s2)[i2];
#else
	    Rcomplex x1 = COMPLEX(s1)[i1], x2 = COMPLEX(s2)[i2];
	    COMPLEX(ans)[i].r = x1.r * x2.r - x1.i * x2.i;
	    COMPLEX(ans)[i].i = x1.r * x2.i + x1.i * x2.r;
#endif
	}
	break;
    case DIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    SET_C99_COMPLEX(COMPLEX(ans), i,
			    C99_COMPLEX2(s1, i1) / C99_COMPLEX2(s2, i2));
	}
	break;
    case POWOP:
	mod_iterate(n1, n2, i1, i2) {
	    SET_C99_COMPLEX(COMPLEX(ans), i,
			    mycpow(C99_COMPLEX2(s1, i1), C99_COMPLEX2(s2, i2)));
	}
	break;
    default:
	error(_("unimplemented complex operation"));
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

SEXP attribute_hidden do_cmathfuns(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y = R_NilValue;	/* -Wall*/
    int i, n;

    checkArity(op, args);
    check1arg(args, call, "z");
    if (DispatchGroup("Complex", call, op, args, env, &x))
	return x;
    x = CAR(args);
    n = length(x);
    if (isComplex(x)) {
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
		REAL(y)[i] = cabs(C99_COMPLEX2(x, i));
	    break;
	case 4:	/* Arg */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		/* Maybe not worth optimizing */
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
		REAL(y)[i] = carg(C99_COMPLEX(x)[i]);
#else
		REAL(y)[i] = atan2(COMPLEX(x)[i].i, COMPLEX(x)[i].r);
#endif
	    break;
	case 5:	/* Conj */
	    y = allocVector(CPLXSXP, n);
	    for(i = 0 ; i < n ; i++) {
		COMPLEX(y)[i].r = COMPLEX(x)[i].r;
		COMPLEX(y)[i].i = -COMPLEX(x)[i].i;
	    }
	    break;
	}
    }
    else if(isNumeric(x)) { /* so no complex numbers involved */
	if(isReal(x)) PROTECT(x);
	else PROTECT(x = coerceVector(x, REALSXP));
	switch(PRIMVAL(op)) {
	case 1:	/* Re */
	case 5:	/* Conj */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = REAL(x)[i];
	    break;
	case 2:	/* Im */
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = 0.0;
	    break;
	case 4:	/* Arg */
	    y = allocVector(REALSXP, n);
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
	    y = allocVector(REALSXP, n);
	    for(i = 0 ; i < n ; i++)
		REAL(y)[i] = fabs(REAL(x)[i]);
	    break;
	}
	UNPROTECT(1);
    }
    else errorcall(call, _("non-numeric argument to function"));
    PROTECT(x);
    PROTECT(y);
    DUPLICATE_ATTRIB(y, x);
    UNPROTECT(2);
    return y;
}

static void z_rround(Rcomplex *r, Rcomplex *x, Rcomplex *p)
{
    r->r = rround(x->r, p->r); /* #defined to fround in Rmath.h */
    r->i = rround(x->i, p->r);
}

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
	r->r = rround(pow10 * x->r, digits)/pow10;
	r->i = rround(pow10 * x->i, digits)/pow10;
    } else {
	digits = (double)(dig);
	r->r = rround(x->r, digits);
	r->i = rround(x->i, digits);
    }
}
static void z_prec(Rcomplex *r, Rcomplex *x, Rcomplex *p)
{
    z_prec_r(r, x, p->r);
}

static void z_log(double complex *r, double complex *z)
{
    *r = clog(*z);
}

static void z_logbase(Rcomplex *r, Rcomplex *z, Rcomplex *base)
{
    double complex dz = toC99(z), dbase = toC99(base);
    SET_C99_COMPLEX(r, 0, clog(dz)/clog(dbase));
}

static void z_exp(double complex *r, double complex *z)
{
    *r = cexp(*z);
}

static void z_sqrt(double complex *r, double complex *z)
{
    *r = csqrt(*z);
}

static void z_cos(double complex *r, double complex *z)
{
    *r = ccos(*z);
}

static void z_sin(double complex *r, double complex *z)
{
    *r = csin(*z);
}

static void z_tan(double complex *r, double complex *z)
{
    double y = cimag(*z);
    *r = ctan(*z);
    if(R_FINITE(y) && fabs(y) > 25.0) {
	/* at this point the real part is nearly zero, and the
	   imaginary part is one: but some OSes get the imag wrong */
#if __GNUC__
	__imag__ *r = y < 0 ? -1.0 : 1.0;
#else
	*r = creal(*r) + (y < 0 ? -1.0 : 1.0) * I;
#endif
    }
}

static void z_atan2(Rcomplex *r, Rcomplex *csn, Rcomplex *ccs)
{
    double complex dr, dcsn = toC99(csn), dccs = toC99(ccs);
			    
    if (dccs == 0) {
	if(dcsn == 0) {
	    r->r = NA_REAL; r->i = NA_REAL;
	    return;
	} else
	    dr = fsign_int(M_PI_2, creal(dcsn));
    } else {
	dr = catan(dcsn / dccs);
	if(creal(dccs) < 0) dr += M_PI;
	if(creal(dr) > M_PI) dr -= 2 * M_PI;
    }
    SET_C99_COMPLEX(r, 0, dr);
}

static void z_asin(double complex *r, double complex *z)
{
#ifdef Win32
    /* broken for cabs(*z) >= 1 */
    double alpha, t1, t2, x = __real__ *z, y = __imag__ *z;
    t1 = 0.5 * hypot(x + 1, y);
    t2 = 0.5 * hypot(x - 1, y);
    alpha = t1 + t2;
    __real__ *r = asin(t1 - t2);
    __imag__ *r = log(alpha + sqrt(alpha*alpha - 1));
    if(y < 0 || (y == 0 && x > 1)) __imag__ *r *= -1;
#else
    *r = casin(*z);
#endif
}

static void z_acos(double complex *r, double complex *z)
{
#ifdef Win32
    /* broken for cabs(*z) >= 1 */
    double complex Asin;
    z_asin(&Asin, z);
    *r = M_PI_2 - Asin;
#else
    *r = cacos(*z);
#endif
}

static void z_atan(double complex *r, double complex *z)
{
    *r = catan(*z);
}

static void z_acosh(double complex *r, double complex *z)
{
#ifdef Win32
    /* workaround for PR#9403 */
    if(__imag__ *z == 0.0) {
	__real__ *r = acosh(__real__ *z);
	__imag__ *r = 0.0;
    } else
#endif
    *r = cacosh(*z);
}

static void z_asinh(double complex *r, double complex *z)
{
    *r = casinh(*z);
}

static void z_atanh(double complex *r, double complex *z)
{
    *r = catanh(*z);
}

static void z_cosh(double complex *r, double complex *z)
{
    *r = ccosh(*z);
}

static void z_sinh(double complex *r, double complex *z)
{
    *r = csinh(*z);
}

static void z_tanh(double complex *r, double complex *z)
{
    *r = ctanh(*z);
}


static Rboolean cmath1(void (*f)(double complex *, double complex *),
		       Rcomplex *x, Rcomplex *y, int n)
{
    int i;
    Rboolean naflag = FALSE;
    for (i = 0 ; i < n ; i++) {
	if (ISNA(x[i].r) || ISNA(x[i].i)) {
	    y[i].r = NA_REAL;
	    y[i].i = NA_REAL;
	} else {
#ifdef HAVE_COMPATIBLE_C99_COMPLEX
	    f((double complex *) y + i, (double complex *) x + i);
#else
	    double complex dx = toC99(x + i), dy;
	    f(&dy, &dx);
	    SET_C99_COMPLEX(y, i, dy);
#endif
	}
    }

    return(naflag);
}

SEXP attribute_hidden complex_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    int n;
    Rboolean naflag = FALSE;
    PROTECT(x = CAR(args));
    n = length(x);
    PROTECT(y = allocVector(CPLXSXP, n));

    switch (PRIMVAL(op)) {
    case 10003: naflag = cmath1(z_log, COMPLEX(x), COMPLEX(y), n); break;

    case 3: naflag = cmath1(z_sqrt, COMPLEX(x), COMPLEX(y), n); break;

    case 10: naflag = cmath1(z_exp, COMPLEX(x), COMPLEX(y), n); break;

    case 20: naflag = cmath1(z_cos, COMPLEX(x), COMPLEX(y), n); break;
    case 21: naflag = cmath1(z_sin, COMPLEX(x), COMPLEX(y), n); break;
    case 22: naflag = cmath1(z_tan, COMPLEX(x), COMPLEX(y), n); break;
    case 23: naflag = cmath1(z_acos, COMPLEX(x), COMPLEX(y), n); break;
    case 24: naflag = cmath1(z_asin, COMPLEX(x), COMPLEX(y), n); break;
    case 25: naflag = cmath1(z_atan, COMPLEX(x), COMPLEX(y), n); break;

    case 30: naflag = cmath1(z_cosh, COMPLEX(x), COMPLEX(y), n); break;
    case 31: naflag = cmath1(z_sinh, COMPLEX(x), COMPLEX(y), n); break;
    case 32: naflag = cmath1(z_tanh, COMPLEX(x), COMPLEX(y), n); break;
    case 33: naflag = cmath1(z_acosh, COMPLEX(x), COMPLEX(y), n); break;
    case 34: naflag = cmath1(z_asinh, COMPLEX(x), COMPLEX(y), n); break;
    case 35: naflag = cmath1(z_atanh, COMPLEX(x), COMPLEX(y), n); break;

#ifdef NOTYET
	MATH1(40, lgammafn);
	MATH1(41, gammafn);
#endif

    default:
	/* such as sign, gamma */
	errorcall(call, _("unimplemented complex function"));
    }
    if (naflag)
	warningcall(call, "NAs produced in function \"%s\"", PRIMNAME(op));
    DUPLICATE_ATTRIB(y, x);
    UNPROTECT(2);
    return y;
}

/* FIXME : Use the trick in arithmetic.c to eliminate "modulo" ops */

static SEXP cmath2(SEXP op, SEXP sa, SEXP sb, 
		   void (*f)(Rcomplex *, Rcomplex *, Rcomplex *))
{
    int i, n, na, nb;
    Rcomplex ai, bi, *a, *b, *y;
    SEXP sy;
    int naflag = 0;
    na = length(sa);
    nb = length(sb);
    if ((na == 0) || (nb == 0))
	return(allocVector(CPLXSXP, 0));
    n = (na < nb) ? nb : na;
    PROTECT(sa = coerceVector(sa, CPLXSXP));
    PROTECT(sb = coerceVector(sb, CPLXSXP));
    PROTECT(sy = allocVector(CPLXSXP, n));
    a = COMPLEX(sa);
    b = COMPLEX(sb);
    y = COMPLEX(sy);
    naflag = 0;
    for (i = 0; i < n; i++) {
	ai = a[i % na];
	bi = b[i % nb];
	if(ISNA(ai.r) && ISNA(ai.i) &&
	   ISNA(bi.r) && ISNA(bi.i)) {
	    y[i].r = NA_REAL;
	    y[i].i = NA_REAL;
	} else {
	    f(&y[i], &ai, &bi);
	}
    }
    /* should really be warningcall */
    if (naflag)
	warning("NAs produced in function \"%s\"", PRIMNAME(op));
    if(n == na) {
	DUPLICATE_ATTRIB(sy, sa);
    } else if(n == nb) {
	DUPLICATE_ATTRIB(sy, sb);
    }
    UNPROTECT(3);
    return sy;
}

	/* Complex Functions of Two Arguments */

SEXP attribute_hidden complex_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    switch (PRIMVAL(op)) {
    case 10001:
	return cmath2(op, CAR(args), CADR(args), z_rround);
    case 10002:
	return cmath2(op, CAR(args), CADR(args), z_atan2);
    case 10003:
    case 2: /* passed from do_log1arg */
    case 10:
	return cmath2(op, CAR(args), CADR(args), z_logbase);
    case 10004:
	return cmath2(op, CAR(args), CADR(args), z_prec);
    case 0:
	return cmath2(op, CAR(args), CADR(args), z_atan2);
    default:
	errorcall_return(call, _("unimplemented complex function"));
    }
}

SEXP attribute_hidden do_complex(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    /* complex(length, real, imaginary) */
    SEXP ans, re, im;
    int i, na, nr, ni;
    na = asInteger(CAR(args));
    if(na == NA_INTEGER || na < 0)
	error(_("invalid length"));
    PROTECT(re = coerceVector(CADR(args), REALSXP));
    PROTECT(im = coerceVector(CADDR(args), REALSXP));
    nr = length(re);
    ni = length(im);
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
    n = length(z);
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

	for(i=0 ; i<n ; i++) {
	    if(!R_FINITE(COMPLEX(z)[i].r) || !R_FINITE(COMPLEX(z)[i].i))
		error(_("invalid polynomial coefficient"));
	    REAL(zr)[degree-i] = COMPLEX(z)[i].r;
	    REAL(zi)[degree-i] = COMPLEX(z)[i].i;
	}
	R_cpolyroot(REAL(zr), REAL(zi), &degree, REAL(rr), REAL(ri), &fail);
	if(fail) error(_("root finding code failed"));
	UNPROTECT(2);
	r = allocVector(CPLXSXP, degree);
	for(i=0 ; i<degree ; i++) {
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
