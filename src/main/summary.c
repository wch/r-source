/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Mathlib.h"

extern int errno;
static int narm;
static int count;

static void isum(int *x, int n, double *value)
{
	double s;
	int i;
	s = 0;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			s += x[i];
			count += 1;
		}
		else if (!narm) {
			count += 1;
			*value = NA_REAL;
			return;
		}
	}
	*value = s;
}

static void rsum(double *x, int n, double *value)
{
	double s;
	int i;
	s = 0;
	for (i=0; i<n; i++) {
		if (!ISNAN(x[i])) {
			count += 1;
			s += x[i];
		}
		else if (!narm) {
			count += 1;
#ifdef IEEE_754
			s += x[i];
#else
			*value = NA_REAL;
			return;
#endif
		}
	}
	*value = s;
}

static void csum(complex *x, int n, complex *value)
{
	complex s;
	int i;
	s.r = 0;
	s.i = 0;
	for (i=0; i<n; i++) {
#ifdef IEEE_754
		if ((!ISNAN(x[i].r) && !ISNAN(x[i].i)) || !narm) {
			count += 1;
			s.r = x[i].r;
			s.i = x[i].i;
		}
#else
		if (!ISNAN(x[i].r) && !ISNAN(x[i].i)) {
			count += 1;
			s.r += x[i].r;
			s.i += x[i].i;
		}
		else if (!narm) {
			count += 1;
			value->r = NA_REAL;
			value->i = NA_REAL;
			return;
		}
#endif
	}
	value->r = s.r;
	value->i = s.i;
}

static void imin(int *x, int n, double *value)
{
	int i, s;
	s = NA_INTEGER;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			if (s == NA_INTEGER || s > x[i])
				s = x[i];
			count += 1;
		}
		else if (!narm) {
			*value = NA_REAL;
			return;
		}
	}
	*value = s;
}

static void rmin(double *x, int n, double *value)
{
	double s;
	int i;
#ifdef IEEE_754
	s = R_PosInf;
	for (i=0; i<n; i++) {
		if (ISNAN(x[i])) {
			if (!narm) {
				s = s + x[i];
				count += 1;
			}
		}
		else if (x[i] < s) {
			s = x[i];
			count += 1;
		}
	}
	*value = (count == 0) ? NA_REAL : s;
#else
	s = NA_REAL;
	for (i=0; i<n; i++) {
		if (!ISNAN(x[i])) {
			if (ISNAN(s) || s > x[i])
				s = x[i];
			count += 1;
		}
		else if (!narm) {
			*value = NA_REAL;
			return;
		}
	}
	*value = s;
#endif
}

static void imax(int *x, int n, double *value)
{
        int i, s;
        s = NA_INTEGER;
        for (i=0; i<n; i++) {
                if (x[i] != NA_INTEGER) {
                        if (s == NA_INTEGER || s < x[i])
                                s = x[i];
                        count += 1;
                }
                else if (!narm) {
                        *value = NA_REAL;
                        return;
                }
        }
        *value = s;
}

static void rmax(double *x, int n, double *value)
{
        double s;
        int i;
#ifdef IEEE_754
	s = R_NegInf;
	for (i=0; i<n; i++) {
		if (ISNAN(x[i])) {
			if (!narm) {
				s = s + x[i];
				count += 1;
			}
		}
		else if (x[i] > s) {
			s = x[i];
			count += 1;
		}
	}
	*value = (count == 0) ? NA_REAL : s;
#else
        s = NA_REAL;
        for (i=0; i<n; i++) {
                if (!ISNAN(x[i])) {
                        if (ISNAN(s) || s < x[i])
                                s = x[i];
                        count += 1;
                }
                else if (!narm) {
                        *value = NA_REAL;
                        return;
                }
        }
        *value = s;
#endif
}

static void iprod(int *x, int n, double *value)
{
	double s;
	int i;
	s = 1;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			s = MATH_CHECK(s * x[i]);
			count += 1;
		}
		else if (!narm) {
			count += 1;
			*value = NA_REAL;
			return;
		}
		if(ISNAN(s)) {
			*value = NA_REAL;
			return;
		}
	}
	*value = s;
}

static void rprod(double *x, int n, double *value)
{
	double s;
	int i;
	s = 1;
	for (i=0; i<n; i++) {
#ifdef IEEE_754
		s *= x[i];
		count += 1;
#else
		if (!ISNAN(x[i])) {
			s = MATH_CHECK(s * x[i]);
			count += 1;
		}
		else if (!narm) {
			count += 1;
			*value = NA_REAL;
			return;
		}
		if(ISNAN(s)) {
			*value = NA_REAL;
			return;
		}
#endif
	}
	*value = s;
}

static void cprod(complex *x, int n, complex *value)
{
	complex s, t;
	int i;
	s.r = 1;
	s.i = 0;
	for (i=0; i<n; i++) {
#ifdef IEEE_754
		if ((!ISNAN(x[i].r) && !ISNAN(x[i].i)) || !narm) {
			count += 1;
			t.r = s.r;
			t.i = s.i;
			s.r = t.r * x[i].r - t.i * x[i].i;
			s.i = t.r * x[i].i + t.i * x[i].r;
		}
#else
		if (!ISNAN(x[i].r) && !ISNAN(x[i].i)) {
			count += 1;
			t.r = s.r;
			t.i = s.i;
			s.r = MATH_CHECK(t.r * x[i].r - t.i * x[i].i);
			s.i = MATH_CHECK(t.r * x[i].i + t.i * x[i].r);
		}
		else if (!narm) {
			count += 1;
			value->r = NA_REAL;
			value->i = NA_REAL;
			return;
		}
		if(ISNAN(s.r) || ISNAN(s.i)) {
			value->r = NA_REAL;
			value->i = NA_REAL;
			return;
		}
#endif
	}
	value->r = s.r;
	value->i = s.i;
}

	/* do_summary provides a variety of data summaries */
	/* note that mean is no longer processed by this code */
	/* 0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod */

SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, a;
	double tmp;
	complex z, ztmp, zcum;
	int complex_ans, oldcount;

	 if( DispatchGroup("Summary",call, op, args, env, &ans) )
                return ans;

	ans = matchArg(R_NaRmSymbol, &args);
	narm = asLogical(ans);
	oldcount = 0;
	complex_ans = 0;

	if(PRIMVAL(op) == 0 || PRIMVAL(op) == 1) {	/* "sum" and "mean" */
		zcum.r = 0.0;
		zcum.i = 0.0;
		while(args != R_NilValue) {
			a = CAR(args);
			if(length(a) > 0) {
				count = oldcount;
				switch(TYPEOF(a)) {
				case LGLSXP:
				case INTSXP:
					isum(INTEGER(a), length(a), &tmp);
					if(count != oldcount) {
#ifndef IEEE_754
						if(ISNAN(tmp))
							goto na_answer;
#endif
						zcum.r = zcum.r + tmp;
					}
					break;
				case REALSXP:
					rsum(REAL(a), length(a), &tmp);
					if(count != oldcount) {
#ifndef IEEE_754
						if(ISNAN(tmp))
							goto na_answer;
#endif
						zcum.r = zcum.r + tmp;
					}
					break;
				case CPLXSXP:
					complex_ans = 1;
					csum(COMPLEX(a), length(a), &ztmp);
					if(count != oldcount) {
#ifndef IEEE_754
						if(ISNAN(ztmp.r))
							goto na_answer;
#endif
						zcum.r = zcum.r + ztmp.r;
						zcum.i = zcum.i + ztmp.i;
					}
					break;
				default:
					goto badarg;
				}
			}
			args = CDR(args);
		}
		if(PRIMVAL(op) == 1) {
			if(count > 0) {
				zcum.r /= count;
				zcum.i /= count;
			}
			else {
				zcum.r = NA_REAL;
				zcum.i = NA_REAL;
			}
		}
	}
	else if(PRIMVAL(op) == 2) {	/* min */
#ifdef IEEE_754
		zcum.r = R_PosInf;
#else
		zcum.r = NA_REAL;
#endif
		while(args != R_NilValue) {
			a = CAR(args);
			if(length(a) > 0) {
				oldcount = count;
				switch(TYPEOF(a)) {
				case LGLSXP:
				case INTSXP:
					imin(INTEGER(a), length(a), &tmp);
					break;
				case REALSXP:
					rmin(REAL(a), length(a), &tmp);
					break;
				default:
					goto badarg;
				}
#ifdef IEEE_754
				if (ISNAN(tmp))
					zcum.r = zcum.r + tmp;
				else if (tmp < zcum.r)
					zcum.r = tmp;
#else
				if(ISNAN(tmp)) goto na_answer;
				if(count != oldcount
				   && (ISNAN(zcum.r) || tmp < zcum.r))
					zcum.r = tmp;
#endif
			}
			args = CDR(args);
		}
	}
	else if(PRIMVAL(op) == 3) {	/* max */
#ifdef IEEE_754
		zcum.r = R_NegInf;;
#else
		zcum.r = NA_REAL;
#endif
		while(args != R_NilValue) {
			a = CAR(args);
			if(length(a) > 0) {
				oldcount = count;
				switch(TYPEOF(a)) {
				case LGLSXP:
				case INTSXP:
					imax(INTEGER(a), length(a), &tmp);
					break;
				case REALSXP:
					rmax(REAL(a), length(a), &tmp);
					break;
				default:
					goto badarg;
				}
#ifdef IEEE_754
				if (ISNAN(tmp))
					zcum.r = zcum.r + tmp;
				else if (tmp > zcum.r)
					zcum.r = tmp;
#else
				if(ISNAN(tmp)) goto na_answer;
				if(count != oldcount
				   && (ISNAN(zcum.r) || tmp > zcum.r))
					zcum.r = tmp;
#endif
			}
			args = CDR(args);
		}
	}
	else if(PRIMVAL(op) == 4) {	/* prod */
		zcum.r = 1;
		zcum.i = 0;
		while(args != R_NilValue) {
			a = CAR(args);
			if(length(a) > 0) {
				oldcount = count;
				switch(TYPEOF(a)) {
				case LGLSXP:
				case INTSXP:
					iprod(INTEGER(a), length(a), &tmp);
					if(count != oldcount ) {
#ifndef IEEE_754
						if(ISNAN(tmp))
							goto na_answer;
#endif
						zcum.r = zcum.r * tmp;
						zcum.i = zcum.i * tmp;
					}
					break;
				case REALSXP:
					rprod(REAL(a), length(a), &tmp);
					if(count != oldcount ) {
#ifndef IEEE_754
						if(ISNAN(tmp))
							goto na_answer;
#endif
						zcum.r = zcum.r * tmp;
						zcum.i = zcum.i * tmp;
					}
					break;
				case CPLXSXP:
					complex_ans = 1;
					cprod(COMPLEX(a), length(a), &ztmp);
					if(count != oldcount ) {
#ifndef IEEE_754
						if(ISNAN(ztmp.r))
							goto na_answer;
#endif
						z.r = zcum.r;
						z.i = zcum.i;
						zcum.r = MATH_CHECK(z.r * ztmp.r - z.i * ztmp.i);
						zcum.i = MATH_CHECK(z.r * ztmp.i + z.i * ztmp.r);
					}
					break;
				default:
					goto badarg;
				}
			}
			args = CDR(args);
		}
	}
	else errorcall(call, "internal error.  Call a Guru\n");

	if(complex_ans) {
		ans = allocVector(CPLXSXP, 1);
		COMPLEX(ans)[0].r = zcum.r;
		COMPLEX(ans)[0].i = zcum.i;
	}
	else {
		ans = allocVector(REALSXP, 1);
		REAL(ans)[0] = zcum.r;
	}
	return ans;

/* na_answer: */
	if(complex_ans) {
		ans = allocVector(CPLXSXP, 1);
		COMPLEX(ans)[0].r = NA_REAL;
		COMPLEX(ans)[0].i = NA_REAL;
	}
	else {
		ans = allocVector(REALSXP, 1);
		REAL(ans)[0] = NA_REAL;
	}
	return ans;

badarg:
	errorcall(call, "invalid argument type\n");
	return R_NilValue;/* for -Wall */
}

SEXP do_compcases(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP s, t, u, rval;
	int i, len;

	/* checkArity(op, args); */
	len = -1;

	for (s = args; s != R_NilValue; s = CDR(s)) {
		if (isList(CAR(s)) || isFrame(CAR(s))) {
			for (t = CAR(s); t != R_NilValue; t = CDR(t))
				if (isMatrix(CAR(t))) {
					u = getAttrib(CAR(t), R_DimSymbol);
					if (len < 0)
						len = INTEGER(u)[0];
					else if (len != INTEGER(u)[0])
						goto bad;
				}
				else if (isVector(CAR(t))) {
					if (len < 0)
						len = LENGTH(CAR(t));
					else if (len != LENGTH(CAR(t)))
						goto bad;
				}
				else
					goto bad2;
		}
		else if (isMatrix(CAR(s))) {
			u = getAttrib(CAR(s), R_DimSymbol);
			if (len < 0)
				len = INTEGER(u)[0];
			else if (len != INTEGER(u)[0])
				goto bad;
		}
		else if (isVector(CAR(s))) {
			if (len < 0)
				len = LENGTH(CAR(s));
			else if (len != LENGTH(CAR(s)))
				goto bad;
		}
		else
			goto bad2;
	}
	PROTECT(rval = allocVector(LGLSXP, len));
	for (i = 0; i < len; i++)
		INTEGER(rval)[i] = 1;
	for (s = args; s != R_NilValue; s = CDR(s)) {
		if (isList(CAR(s)) || isFrame(CAR(s))) {
			/* now we only need to worry about vectors, use mod to handle arrays */
			for (t = CAR(s); t != R_NilValue; t = CDR(t))
				for (i = 0; i < LENGTH(CAR(t)); i++) {
					u = CAR(t);
					switch (TYPEOF(u)) {
					case INTSXP:
					case LGLSXP:
					case FACTSXP:
					case ORDSXP:
						if (INTEGER(u)[i] == NA_INTEGER)
							INTEGER(rval)[i % len] = 0;
						break;
					case REALSXP:
						if (ISNAN(REAL(u)[i]))
							INTEGER(rval)[i % len] = 0;
						break;
					case CPLXSXP:
						if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
							INTEGER(rval)[i % len] = 0;
						break;
					case STRSXP:
						if (STRING(u)[i] == NA_STRING)
							INTEGER(rval)[i % len] = 0;
						break;
					default:
						goto bad2;
					}
				}
		}
		else {
			for (i = 0; i < LENGTH(CAR(s)); i++) {
				u = CAR(s);
				switch (TYPEOF(u)) {
				case INTSXP:
				case LGLSXP:
				case FACTSXP:
				case ORDSXP:
					if (INTEGER(u)[i] == NA_INTEGER)
						INTEGER(rval)[i % len] = 0;
					break;
				case REALSXP:
					if (ISNAN(REAL(u)[i]))
						INTEGER(rval)[i % len] = 0;
					break;
				case CPLXSXP:
					if (ISNAN(COMPLEX(u)[i].r) || ISNAN(COMPLEX(u)[i].i))
						INTEGER(rval)[i % len] = 0;
					break;
				case STRSXP:
					if (STRING(u)[i] == NA_STRING)
						INTEGER(rval)[i % len] = 0;
					break;
				default:
					goto bad2;
				}
			}
		}
	}
	UNPROTECT(1);
	return rval;
bad:	error("complete.cases: not all arguments have the same length\n");
bad2:	error("complete.cases: invalid argument type\n");
	return R_NilValue;/* NOTREACHED; for -Wall */
}
