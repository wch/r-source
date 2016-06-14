/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2014	    The R Core Team.
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

SEXP do_math1(SEXP, SEXP, SEXP, SEXP);
SEXP do_math2(SEXP, SEXP, SEXP, SEXP);
SEXP do_math3(SEXP, SEXP, SEXP, SEXP);
SEXP do_math4(SEXP, SEXP, SEXP, SEXP);
#ifdef WHEN_MATH5_IS_THERE
 SEXP do_math5(SEXP, SEXP, SEXP, SEXP);
#endif
SEXP do_cmathfuns(SEXP, SEXP, SEXP, SEXP);

SEXP complex_math1(SEXP, SEXP, SEXP, SEXP);
SEXP complex_math2(SEXP, SEXP, SEXP, SEXP);
SEXP complex_unary(ARITHOP_TYPE, SEXP, SEXP);
SEXP complex_binary(ARITHOP_TYPE, SEXP, SEXP);

double R_pow(double x, double y);
static R_INLINE double R_POW(double x, double y) /* handle x ^ 2 inline */
{
    return y == 2.0 ? x * x : R_pow(x, y);
}

/* some systems get this wrong, possibly depend on what libs are loaded */
static R_INLINE double R_log(double x) {
    return x > 0 ? log(x) : x == 0 ? R_NegInf : R_NaN;
}

/* Note that the behaviour of log(0) required is not necessarily that
   mandated by C99 (-HUGE_VAL), and the behaviour of log(x < 0) is
   optional in C99.  Some systems return -Inf for log(x < 0), e.g.
   libsunmath on Solaris.
*/
static R_INLINE double logbase(double x, double base)
{
#ifdef HAVE_LOG10
    if(base == 10) return x > 0 ? log10(x) : x == 0 ? R_NegInf : R_NaN;
#endif
#ifdef HAVE_LOG2
    if(base == 2) return x > 0 ? log2(x) : x == 0 ? R_NegInf : R_NaN;
#endif
    return R_log(x) / R_log(base);
}

SEXP do_log_builtin(SEXP call, SEXP op, SEXP args, SEXP env);

/* for binary operations */
/* adapted from Radford Neal's pqR */
static R_INLINE SEXP R_allocOrReuseVector(SEXP s1, SEXP s2,
					  SEXPTYPE type , R_xlen_t n)
{
    R_xlen_t n1 = XLENGTH(s1);
    R_xlen_t n2 = XLENGTH(s2);

    /* Try to use space for 2nd arg if both same length, so 1st argument's
       attributes will then take precedence when copied. */

    if (n == n2) {
        if (TYPEOF(s2) == type && NO_REFERENCES(s2)) {
	    if (ATTRIB(s2) != R_NilValue)
		/* need to remove 'names' attribute if present to
		   match what copyMostAttrib does. copyMostAttributes
		   also skips 'dim' and 'dimnames' but those, here
		   since those, if present, will be replaced by
		   attribute cleanup code in R_Binary) */
		setAttrib(s2, R_NamesSymbol, R_NilValue);
            return s2;
	}
        else
            /* Can use 1st arg's space only if 2nd arg has no attributes, else
               we may not get attributes of result right. */
            if (n == n1 && TYPEOF(s1) == type && NO_REFERENCES(s1)
		&& ATTRIB(s2) == R_NilValue)
                return s1;
    }
    else if (n == n1 && TYPEOF(s1) == type && NO_REFERENCES(s1))
	return s1;

    return allocVector(type, n);
}

#if defined(HAVE_TANPI) || defined(HAVE___TANPI)
// we document that tanpi(0.5) is NaN, but TS 18661-4:2015
// does not require this and the Solaris and macOS versions give Inf.
double Rtanpi(double);
#endif
