/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the R core team.
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

/*extern int errno;*/

/* These GLOBALS are set/initialized in do_summary: */
static int narm;
static int count;

static void isum(int *x, int n, int *value)
{
	double s;
	int i;
	s = 0;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			s += x[i];
			count++;
		}
		else if (!narm) {
			count++;
			*value = NA_INTEGER;
			return;
		}
	}
	if(s > INT_MAX || s < INT_MIN){
	   REprintf("Integer overflow in sum(..); use  sum(as.numeric(..))\n");
	   *value = NA_INTEGER;
	} else
	  *value = s;
}

static void rsum(double *x, int n, double *value)
{
	double s;
	int i;
	s = 0;
	for (i=0; i<n; i++) {
		if (!ISNAN(x[i])) {
			count++;
			s += x[i];
		}
		else if (!narm) {
			count++;
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
			count++;
			s.r = x[i].r;
			s.i = x[i].i;
		}
#else
		if (!ISNAN(x[i].r) && !ISNAN(x[i].i)) {
			count++;
			s.r += x[i].r;
			s.i += x[i].i;
		}
		else if (!narm) {
			count++;
			value->r = NA_REAL;
			value->i = NA_REAL;
			return;
		}
#endif
	}
	value->r = s.r;
	value->i = s.i;
}

static void imin(int *x, int n, int *value)
{
	int i, s;
	s = NA_INTEGER;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			if (s == NA_INTEGER || s > x[i])
				s = x[i];
			count++;
		}
		else if (!narm) {
			*value = NA_INTEGER;
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
				count++;
			}
		}
		else if (x[i] < s) {
			s = x[i];
			count++;
		}
	}
/* FIXME??:  in do_summary(.) [below], we have	count = oldcount !!
 * -------   should not test  (count == 0) , but rather
 *		(count == oldcount)   [in  do_summary, not here] !?!
 *
 * BTW:	 MM thinks the whole 'count' thing should be drastically cleaned,
 * ---	 at the same time, dropping   mean(.) which is not used anymore...
 */
	*value = (count == 0) ? NA_REAL : s;
#else
	s = NA_REAL;
	for (i=0; i<n; i++) {
		if (!ISNAN(x[i])) {
			if (ISNAN(s) || s > x[i])
				s = x[i];
			count++;
		}
		else if (!narm) {
			*value = NA_REAL;
			return;
		}
	}
	*value = s;
#endif
}

static void imax(int *x, int n, int *value)
{
	int i, s;
	s = NA_INTEGER;
	for (i=0; i<n; i++) {
		if (x[i] != NA_INTEGER) {
			if (s == NA_INTEGER || s < x[i])
				s = x[i];
			count++;
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
				count++;
			}
		}
		else if (x[i] > s) {
			s = x[i];
			count++;
		}
	}
	*value = (count == 0) ? NA_REAL : s;
#else
	s = NA_REAL;
	for (i=0; i<n; i++) {
		if (!ISNAN(x[i])) {
			if (ISNAN(s) || s < x[i])
				s = x[i];
			count++;
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
			count++;
		}
		else if (!narm) {
			count++;
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
		count++;
#else
		if (!ISNAN(x[i])) {
			s = MATH_CHECK(s * x[i]);
			count++;
		}
		else if (!narm) {
			count++;
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
			count++;
			t.r = s.r;
			t.i = s.i;
			s.r = t.r * x[i].r - t.i * x[i].i;
			s.i = t.r * x[i].i + t.i * x[i].r;
		}
#else
		if (!ISNAN(x[i].r) && !ISNAN(x[i].i)) {
			count++;
			t.r = s.r;
			t.i = s.i;
			s.r = MATH_CHECK(t.r * x[i].r - t.i * x[i].i);
			s.i = MATH_CHECK(t.r * x[i].i + t.i * x[i].r);
		}
		else if (!narm) {
			count++;
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

SEXP do_summary(SEXP call, SEXP op, SEXP args, SEXP env)
{
	/* do_summary provides a variety of data summaries */

	/* op :	  0 = sum, 1 = mean, 2 = min, 3 = max, 4 = prod */

/* NOTE: mean() [op = 1]  is no longer processed by this code
 * ----
 * I.e., all the fuzz with  'count' could be replaced by
 * a simple "logical" named ``non_null''
 *	  (=1 if there is >=1 non-null argument) [MM]
 */

	SEXP ans, a;
	double tmp;
	complex z, ztmp, zcum;
	int oldcount, itmp, icum=0, int_a;
	SEXPTYPE ans_type=INTSXP; /* only INTEGER, REAL, or COMPLEX here */

	 if( DispatchGroup("Summary",call, op, args, env, &ans) )
		return ans;

	ans = matchArg(R_NaRmSymbol, &args);
	narm = asLogical(ans);
	oldcount = 0;
	switch(PRIMVAL(op)) {

	case 0:/* sum */
	case 2:/* min */
	case 3:/* max */
	  ans_type = INTSXP;/* try to keep if possible.. */
	  break;

	case 1:/* mean */
	case 4:/* prod */
	  ans_type = REALSXP;
	  break;

	default:
	  errorcall(call,"internal error ('op' in do_summary).	Call a Guru\n");
	}

	if(PRIMVAL(op) == 0 || PRIMVAL(op) == 1) {	/* "sum" and "mean" */
	  zcum.r = zcum.i = 0.0;
	  icum = 0;
	  while(args != R_NilValue) {
		a = CAR(args);
		if(length(a) > 0) {
			count = oldcount;
			switch(TYPEOF(a)) {
			case LGLSXP:
			case INTSXP:
				isum(INTEGER(a), length(a), &itmp);
				if(count != oldcount) {
#ifndef IEEE_754
					if(ISNAN(tmp))
						goto na_answer;
#endif
					if(ans_type == INTSXP)
					  icum += itmp;
					else
					  zcum.r += (double)itmp;
				}
				break;
			case REALSXP:
				if(ans_type == INTSXP) {
					ans_type = REALSXP;
					zcum.r = (double)icum;
				}
				rsum(REAL(a), length(a), &tmp);
				if(count != oldcount) {
#ifndef IEEE_754
					if(ISNAN(tmp))
						goto na_answer;
#endif
						zcum.r += tmp;
				}
				break;
			case CPLXSXP:
				ans_type = CPLXSXP;
				csum(COMPLEX(a), length(a), &ztmp);
				if(count != oldcount) {
#ifndef IEEE_754
					if(ISNAN(ztmp.r))
						goto na_answer;
#endif
					zcum.r += ztmp.r;
					zcum.i += ztmp.i;
				}
				break;
			default:
				goto badarg;
			}
		}
		args = CDR(args);
	  }
	  if(PRIMVAL(op) == 1) {/* mean(.), not sum(.) */
		if(count > 0) {
			zcum.r /= count;
			zcum.i /= count;
		} else {
			zcum.r = NA_REAL;
			zcum.i = NA_REAL;
		}
	  }
	}
	else if(PRIMVAL(op) == 2) {	/* min */
	  icum = INT_MAX;
#ifdef IEEE_754
	  zcum.r = R_PosInf;
#else
	  zcum.r = NA_REAL;
#endif
	  while(args != R_NilValue) {
		a = CAR(args); int_a = 0;
		if(length(a) > 0) {
			oldcount = count;
			switch(TYPEOF(a)) {
			case LGLSXP:
			case INTSXP:
				imin(INTEGER(a), length(a), &itmp);
				int_a = 1;
				break;
			case REALSXP:
				if(ans_type == INTSXP) {/* change to REAL */
					ans_type = REALSXP;
					zcum.r = (double)icum;
				}
				rmin(REAL(a), length(a), &tmp);
				break;
			default:
				goto badarg;
			}
			if(ans_type == INTSXP) {
			  if(itmp < icum)
				icum = itmp;
			} else { /* real */
#ifdef IEEE_754
			  if (ISNAN(tmp))
				zcum.r = zcum.r + tmp;
			  else if (int_a && itmp < zcum.r)
				zcum.r = (double)itmp;
			  else if (tmp < zcum.r)
				zcum.r = tmp;
#else
			  if(ISNAN(tmp)) goto na_answer;
			  if(count != oldcount) {
			    if (int_a && itmp < zcum.r)
				zcum.r = (double)itmp;
			    else if(ISNAN(zcum.r) || tmp < zcum.r)
				zcum.r = tmp;
			  }
#endif
			}
		}
		args = CDR(args);
	  }
	}
	else if(PRIMVAL(op) == 3) {	/* max */
	  icum = 1+INT_MIN;/* since INT_MIN is the NA_INTEGER value !! */
#ifdef IEEE_754
	  zcum.r = R_NegInf;;
#else
	  zcum.r = NA_REAL;
#endif
	  while(args != R_NilValue) {
		a = CAR(args); int_a = 0;
		if(length(a) > 0) {
			oldcount = count;
			switch(TYPEOF(a)) {
			case LGLSXP:
			case INTSXP:
				imax(INTEGER(a), length(a), &itmp);
				int_a = 1;
				break;
			case REALSXP:
				if(ans_type == INTSXP) {/* change to REAL */
					ans_type = REALSXP;
					zcum.r = (double)icum;
				}
				rmax(REAL(a), length(a), &tmp);
				break;
			default:
				goto badarg;
			}
			if(ans_type == INTSXP) {
			  if(itmp > icum)
				icum = itmp;
			} else { /* real */
#ifdef IEEE_754
			  if (ISNAN(tmp))
				zcum.r = zcum.r + tmp;
			  else if (int_a && itmp > zcum.r)
				zcum.r = (double)itmp;
			  else if (tmp > zcum.r)
				zcum.r = tmp;
#else
			  if(ISNAN(tmp)) goto na_answer;
			  if(count != oldcount) {
			    if (int_a && itmp > zcum.r)
				zcum.r = (double)itmp;
			    else if(ISNAN(zcum.r) || tmp > zcum.r)
				zcum.r = tmp;
			  }
#endif
			}
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
					zcum.r *= tmp;
					zcum.i *= tmp;
				}
				break;
			case REALSXP:
				rprod(REAL(a), length(a), &tmp);
				if(count != oldcount ) {
#ifndef IEEE_754
					if(ISNAN(tmp))
						goto na_answer;
#endif
					zcum.r *= tmp;
					zcum.i *= tmp;
				}
				break;
			case CPLXSXP:
				ans_type = CPLXSXP;
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
	else errorcall(call, "internal error (do_summary).  Call a Guru\n");

	ans = allocVector(ans_type, 1);
	switch(ans_type) {
	case INTSXP:	INTEGER(ans)[0] = icum; break;
	case REALSXP:	REAL(ans)[0] = zcum.r; break;
	case CPLXSXP:
	  COMPLEX(ans)[0].r = zcum.r;
	  COMPLEX(ans)[0].i = zcum.i;
	}
	return ans;

#ifndef IEEE_754
na_answer:
	ans = allocVector(ans_type, 1);
	switch(ans_type) {
	case INTSXP:	INTEGER(ans)[0] = NA_INTEGER; break;
	case REALSXP:	REAL(ans)[0] = NA_REAL; break;
	case CPLXSXP:
		COMPLEX(ans)[0].r = NA_REAL;
		COMPLEX(ans)[0].i = NA_REAL;
	}
	return ans;
#endif

badarg:
	errorcall(call, "invalid mode of argument\n");
	return R_NilValue;/* for -Wall */
}/* do_summary */

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
bad2:	error("complete.cases: invalid mode of argument\n");
	return R_NilValue;/* NOTREACHED; for -Wall */
}
