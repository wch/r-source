/*
 *  R : A Computer Language for Statistical Data Analysis
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

static SEXP cbind(SEXP, SEXP, SEXPTYPE);
static SEXP rbind(SEXP, SEXP, SEXPTYPE);

	/* The following code establishes the return type for the */
	/* functions  unlist, c, cbind, and rbind and also determines */
	/* whether the returned object is to have a names attribute. */

static int ans_flags;
static SEXP ans_ptr;
static int ans_length;
static SEXP ans_names;
static int ans_nnames;

static int HasNames(SEXP x)
{
	if(isVector(x) && !isNull(getAttrib(x, R_NamesSymbol)))
		return 1;
	else if(isList(x)) {
		while(!isNull(x)) {
			if(!isNull(TAG(x))) return 1;
			x = CDR(x);
		}
	}
	return 0;
}

static void answertype(SEXP x, int recurse, int usenames)
{
	SEXP t;

	for (t=x ; t != R_NilValue; t = CDR(t)) {
		if (usenames && !ans_nnames) {
			if (!isNull(TAG(t))) ans_nnames = 1;
			else ans_nnames = HasNames(CAR(t));
		}
		if (!isNull(CAR(t))) {
			switch (TYPEOF(CAR(t))) {
			case NILSXP:
				break;
			case LGLSXP:
				ans_flags |= 1;
				ans_length += LENGTH(CAR(t));
				break;

			/* factors here previously */

			case INTSXP:
				ans_flags |= 8;
				ans_length += LENGTH(CAR(t));
				break;
			case REALSXP:
				ans_flags |= 16;
				ans_length += LENGTH(CAR(t));
				break;
			case CPLXSXP:
				ans_flags |= 32;
				ans_length += LENGTH(CAR(t));
				break;
			case STRSXP:
				ans_flags |= 64;
				ans_length += LENGTH(CAR(t));
				break;
			case LISTSXP:
				if(recurse) answertype(CAR(t), recurse, usenames);
				else {
					ans_flags |= 128;
					ans_length += length(CAR(t));
				}
				break;
			default:
				ans_flags |= 128;
				ans_length += 1;
				break;
			}
		}
	}
}

	/* The following blocks of code are used to coerce arguments */
	/* to the appropriate type for inclusion in the returned value. */

static void listanswer(SEXP x, int recurse)
{
	int i;

	switch(TYPEOF(x)) {
	case NILSXP:
		break;
	case LGLSXP:
	case INTSXP:
		for(i=0 ; i<LENGTH(x) ; i++) {
			CAR(ans_ptr) = allocVector(TYPEOF(x), 1);
			INTEGER(CAR(ans_ptr))[0] = INTEGER(x)[i];
			ans_ptr = CDR(ans_ptr);
		}
		break;
	case REALSXP:
		for(i=0 ; i<LENGTH(x) ; i++) {
			CAR(ans_ptr) = allocVector(REALSXP, 1);
			REAL(CAR(ans_ptr))[0] = REAL(x)[i];
			ans_ptr = CDR(ans_ptr);
		}
		break;
	case CPLXSXP:
		for(i=0 ; i<LENGTH(x) ; i++) {
			CAR(ans_ptr) = allocVector(CPLXSXP, 1);
			COMPLEX(CAR(ans_ptr))[0] = COMPLEX(x)[i];
			ans_ptr = CDR(ans_ptr);
		}
		break;
	case STRSXP:
		for(i=0 ; i<LENGTH(x) ; i++) {
			CAR(ans_ptr) = allocVector(STRSXP, 1);
			STRING(CAR(ans_ptr))[0] = STRING(x)[i];
			ans_ptr = CDR(ans_ptr);
		}
		break;
	case LISTSXP:
		if(recurse) {
			while(x != R_NilValue) {
				listanswer(CAR(x), recurse);
				x = CDR(x);
			}
		}
		else {
			while(x != R_NilValue) {
				CAR(ans_ptr) = duplicate(CAR(x));
				TAG(ans_ptr) = TAG(x);
				ans_ptr = CDR(ans_ptr);
				x = CDR(x);
			}
		}
		break;
	default:
		CAR(ans_ptr) = duplicate(x);
		ans_ptr = CDR(ans_ptr);
		break;
	}
}

static void stringanswer(SEXP x)
{
	int i, nx;

	switch(TYPEOF(x)) {
	case NILSXP:
		break;
	case LISTSXP:
		while(x != R_NilValue) {
			stringanswer(CAR(x));
			x = CDR(x);
		}
		break;
	default:
		PROTECT(x = coerceVector(x, STRSXP));
		nx = LENGTH(x);
		for (i = 0; i < nx; i++)
			STRING(ans_ptr)[ans_length++] = STRING(x)[i];
		UNPROTECT(1);
		break;
	}
}

static void integeranswer(SEXP x)
{
	int i, nx;

	switch(TYPEOF(x)) {
	case NILSXP:
		break;
	case LISTSXP:
		while(x != R_NilValue) {
			integeranswer(CAR(x));
			x = CDR(x);
		}
		break;
	default:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++)
			INTEGER(ans_ptr)[ans_length++] = INTEGER(x)[i];
		break;
	}
}

static void realanswer(SEXP x)
{
	int i, nx, xi;

	switch(TYPEOF(x)) {
	case NILSXP:
		break;
	case LISTSXP:
		while(x != R_NilValue) {
			realanswer(CAR(x));
			x = CDR(x);
		}
		break;
	case REALSXP:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++)
			REAL(ans_ptr)[ans_length++] = REAL(x)[i];
		break;
	default:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++) {
			xi = INTEGER(x)[i];
			if(xi == NA_INTEGER)
				REAL(ans_ptr)[ans_length++] = NA_REAL;
			else REAL(ans_ptr)[ans_length++] = xi;
		}
		break;
	}
}

static void complexanswer(SEXP x)
{
	int i, nx, xi;

	switch(TYPEOF(x)) {
	case NILSXP:
		break;
	case LISTSXP:
		while(x != R_NilValue) {
			complexanswer(CAR(x));
			x = CDR(x);
		}
		break;
	case REALSXP:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++) {
			COMPLEX(ans_ptr)[ans_length].r = REAL(x)[i];
			COMPLEX(ans_ptr)[ans_length].i = 0.0;
			ans_length++;
		}
		break;
	case CPLXSXP:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++)
			COMPLEX(ans_ptr)[ans_length++] = COMPLEX(x)[i];
		break;
	default:
		nx = LENGTH(x);
		for (i = 0; i < nx; i++) {
			xi = INTEGER(x)[i];
			if(xi == NA_INTEGER)
				REAL(ans_ptr)[ans_length++] = NA_REAL;
			else REAL(ans_ptr)[ans_length++] = xi;
		}
		break;
	}
}

/*
   TagName can get either a SYMSXP, a STRSXP or a CHARSXP or NILSXP
   for either tag or base; the first part extracts the CHARSXP needed;
   the remainer acts on that
*/
static SEXP TagName(SEXP tag, SEXP base, int i)
{
	SEXP ans, t;

	PROTECT(t=mkChar(""));

	switch(TYPEOF(tag)) {
	case SYMSXP:
		tag = PRINTNAME(tag);
		break;
	case STRSXP:
		tag = STRING(tag)[0];
		break;
	case CHARSXP:
	case NILSXP:
		tag = t;
		break;
	default:
		error("wrong tag argument to TagName\n");
	}
	switch(TYPEOF(base)) {
	case SYMSXP:
		base = PRINTNAME(base);
		break;
	case STRSXP:
		base = STRING(base)[0];
		break;
	case CHARSXP:
	case NILSXP:
		break;
	default:
		error("wrong base argument to TagName\n");
	}

	if(i) {
		if( base == R_NilValue) {
			ans = allocString(strlen(CHAR(tag))+IndexWidth(i));
			sprintf(CHAR(ans), "%s%d", CHAR(tag), i);
		}
		else {
			ans = allocString(strlen(CHAR(base))+
				strlen(CHAR(tag))+IndexWidth(i)+1);
			sprintf(CHAR(ans), "%s.%s%d",CHAR(base),CHAR(tag), i);
		}
	}
	else {
		if( base == R_NilValue) {
			ans = allocString(strlen(CHAR(tag)));
			strcpy(CHAR(ans), CHAR(tag));
		}
		else {
			ans = allocString(strlen(CHAR(tag))+1+strlen(CHAR(base)));
			sprintf(CHAR(ans), "%s.%s",CHAR(base),CHAR(tag));
		}
	}
	UNPROTECT(1);
	return ans;
}

/*
   since ExtractNames is done recursively and it contains a check on the
   names found there must be some mechanism for saying not to check to them;
   hence check
*/
static void ExtractNames(SEXP args, int recurse, int check, SEXP base)
{
	SEXP blank, s;
	int i, offset=0;

	PROTECT(blank=mkChar(""));

	while(args != R_NilValue) {
	  if(!isNull(CAR(args))) {
	    if(isVector(CAR(args))) {
		if(!isNull(TAG(args))) {
		  switch(length(CAR(args))) {
		  case 0:
		    break;
		  case 1:
		    STRING(ans_names)[ans_nnames++] = TagName(TAG(args),base, offset);
		    break;
		  default:
		    for(i=0 ; i<length(CAR(args)) ; i++)
		      STRING(ans_names)[ans_nnames++] = TagName(TAG(args), base, i+1+offset);
		  }
		} else {
		  if(base == R_NilValue) {
		    if(isNull(s = getAttrib(CAR(args), R_NamesSymbol))) {
		      for(i=0 ; i<length(CAR(args)) ; i++)
			STRING(ans_names)[ans_nnames++] = blank;
		    }
		    else {
		      for(i=0 ; i<length(CAR(args)) ; i++)
			STRING(ans_names)[ans_nnames++] = STRING(s)[i];
		    }
		  } else {
		    if(isNull(s = getAttrib(CAR(args), R_NamesSymbol))) {
		      for(i=0 ; i<length(CAR(args)) ; i++)
			STRING(ans_names)[ans_nnames++] = TagName(base,R_NilValue,i+1+offset);
		    } else {
		      for(i=0 ; i<length(CAR(args)) ; i++)
			STRING(ans_names)[ans_nnames++] = TagName(STRING(s)[i],base,offset);
		    }
		    offset+=i;
		  }
		}
	    } else if(isList(CAR(args))) {
		base = TAG(args);
		if(recurse) {
		  ExtractNames(CAR(args), recurse, 0, base);
		} else {
		  i = 1;
		  s = CAR(args);
		  while(s != R_NilValue) {
		    if( isNull(base) ) {
		      if(!isNull(TAG(s)))
			STRING(ans_names)[ans_nnames++] = TagName(TAG(s),base,0);
		      else
			STRING(ans_names)[ans_nnames++] = blank;
		    } else {
		      if( !isNull(TAG(s)) )
			STRING(ans_names)[ans_nnames++] = TagName(TAG(s),base,0);
		      else
			STRING(ans_names)[ans_nnames++] = TagName(TAG(s),base,i);
		    }
		    s = CDR(s);
		    i++;
		  }
		}
		base = R_NilValue;
	    } else { /* neither	 Vector	 nor  List */
		if(!isNull(TAG(args)))
		  STRING(ans_names)[ans_nnames++] = TAG(args);
		else
		  STRING(ans_names)[ans_nnames++] = blank;
	    }
	  } /* if(! null...) */
	  args = CDR(args);
	} /* while */
	if( check && ans_nnames != ans_length) {
		printf("INTERNAL ERROR: ans_nnames = %d	   ans_length = %d\n",
		       ans_nnames, ans_length);
		error("incorrect names vector length\n");
	}
	UNPROTECT(1);
}

static SEXP ExtractOptionals(SEXP ans, int *recurse, int *usenames)
{
	SEXP a, n, r, u;
	int v;
	PROTECT(a = ans = CONS(R_NilValue, ans));
	r = install("recursive");
	u = install("use.names");
	while(a != R_NilValue && CDR(a) != R_NilValue) {
		n = TAG(CDR(a));
		if( n != R_NilValue && pmatch(r, n, 1) ) {
			if((v = asLogical(CADR(a))) != NA_INTEGER) {
				*recurse = v;
			}
			CDR(a) = CDDR(a);
		}
		else if(n != R_NilValue &&  pmatch(u, n, 1) ) {
			if((v = asLogical(CADR(a))) != NA_INTEGER) {
				*usenames = v;
			}
			CDR(a) = CDDR(a);
		}
		a = CDR(a);
	}
	UNPROTECT(1);
	return CDR(ans);
}

	/* do_c provides the internal code for both "c" and "unlist". */
	/* The only real difference is the value of the "recursive" */
	/* argument which is FALSE by default for "c" and TRUE by */
	/* default for "unlist". */
	/* and of course that list takes ... while unlist takes a single
	   arg, and unlist has two optional args, while list has none */

SEXP do_c(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, t;
	int mode, recurse, usenames;

	checkArity(op, args);

	if(DispatchOrEval(call, op, args, env, &ans, 1)) {
		R_Visible = 1;
		return(ans);
	}
	R_Visible = 1;

		/* Method dispatch has failed */
		/* run the generic code */

		/* First extract the optional arguments */
		/* Note the resetting of the argument list */
		/* Only done if the op is 1 (ie. unlist) */

	usenames = 1;
	recurse = PRIMVAL(op);

	if(length(args) > 1)
		PROTECT(args = ExtractOptionals(ans, &recurse, &usenames));
	else
		PROTECT(args = ans);

	if(PRIMVAL(op)) args = CAR(args);

	if(!isList(args)) {
		UNPROTECT(1);
		if(isVector(args)) return args;
		else errorcall(call, "invalid argument\n");
	}

	ans_flags = 0;
	ans_length = 0;
	ans_nnames = 0;
	answertype(args, recurse, usenames);

		/* If there was a non-vector argument encountered */
		/* (perhaps a list if recursive=F) then we must */
		/* return a list.  Otherwise, if there was a character */
		/* mode argument we must return a character vector. */
		/* and otherwise, we use the natural coercion. */
		/* The exception is "factor" vectors.  These can */
		/* only be combined with factors of exactly the */
		/* same "orderedness", number of levels and */
		/* levels themselves. */

	mode = NILSXP;
	if (ans_flags & 128) mode = LISTSXP;
	else if (ans_flags & 64) mode = STRSXP;
	else {
		if (ans_flags & 1)
			mode = LGLSXP;
		if (ans_flags & 8)
			mode = INTSXP;
		if (ans_flags & 16)
			mode = REALSXP;
		if (ans_flags & 32)
			mode = CPLXSXP;
	}
	PROTECT(ans = allocVector(mode, ans_length));
	ans_ptr = ans;
	t = args;
	ans_length = 0;

	if (mode == LISTSXP) {
		if(!recurse) {
			while(args != R_NilValue) {
				listanswer(CAR(args), 0);
				args = CDR(args);
			}
		}
		else listanswer(args, recurse);
		ans_length = length(ans);
	}
	else if(mode == STRSXP)
		stringanswer(args);
	else if(mode == CPLXSXP)
		complexanswer(args);
	else if(mode == REALSXP)
		realanswer(args);
	else
		integeranswer(args);
	args = t;
	if(ans_nnames && ans_length > 0) {
		PROTECT(ans_names = allocVector(STRSXP, ans_length));
		ans_nnames = 0;
		ExtractNames(args, recurse, 1, R_NilValue);
		setAttrib(ans, R_NamesSymbol, ans_names);
		UNPROTECT(1);
	}
	UNPROTECT(2);
	return ans;
}

static SEXP rho;
SEXP substituteList(SEXP, SEXP);

	/* We use substituteList to expand the ... which */
	/* is passed down by the wrapper function to cbind */

SEXP do_bind(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int mode=ANYSXP;	/* for -Wall; none from the ones below */
	SEXP a, p, t;

		/* First we check to see if any of the */
		/* arguments are data frames.  If there */
		/* are, we need to a special dispatch */
		/* to the interpreted data.frame functions. */

	mode = 0;
	for (a = args; a != R_NilValue ; a = CDR(a)) {
		if (isFrame(CAR(a)))
			mode = 1;
	}
	if (mode) {
		a = args;
		t = CDR(call);
		while (a != R_NilValue) {
			if(t == R_NilValue)
				errorcall(call, "corrupt data frame args!\n");
			p = mkPROMISE(CAR(t), rho);
			PRVALUE(p) = CAR(a);
			CAR(a) = p;
			t = CDR(t);
			a = CDR(a);
		}
		switch(PRIMVAL(op)) {
		    case 1:
			op = install("cbind.data.frame");
			break;
		    case 2:
			op = install("rbind.data.frame");
			break;
		}
		PROTECT(op = findFun(op, env));
		if (TYPEOF(op) != CLOSXP)
			errorcall(call, "non closure invoked in rbind/cbind\n");
		args = applyClosure(call, op, args, env, R_NilValue);
		UNPROTECT(1);
		return args;
	}

		/* There are no data frames in the argument list. */
		/*  Perform default action */

	rho = env;
	ans_flags = 0;
	ans_length = 0;
	ans_nnames = 0;
	answertype(args, 0, 0);
	/* zero-extent matrices shouldn't give NULL
	if (ans_length == 0)
		return R_NilValue;
		*/
	if (ans_flags >= 128) {
		if (ans_flags & 128)
			mode = LISTSXP;
	}
	else if (ans_flags >= 64) {
		if (ans_flags & 64)
			mode = STRSXP;
	}
	else {
		if (ans_flags & 1)
			mode = LGLSXP;
		if (ans_flags & 8)
			mode = INTSXP;
		if (ans_flags & 16)
			mode = REALSXP;
		if (ans_flags & 32)
			mode = CPLXSXP;
	}

	switch(mode) {
		case NILSXP:
		case LGLSXP:
		case INTSXP:
		case REALSXP:
		case CPLXSXP:
		case STRSXP:
			break;
		default:
			errorcall(call, "cannot create a matrix from these types\n");
	}

	switch (PRIMVAL(op)) {
	case 1:
		return cbind(call, args, mode);
	case 2:
		return rbind(call, args, mode);
	}
	return call;/* never used; just for -Wall */
}


static int imax(int i, int j)
{
	return (i > j) ? i : j;
}

static SEXP cbind(SEXP call, SEXP args, SEXPTYPE mode)
{
	int i, j, k, idx, n;
	int have_rnames, have_cnames;
	int nrnames, mrnames;
	int rows, cols, mrows;
	int warned;
	SEXP blank, dn, t, u, result, dims;

	have_rnames = 0;
	have_cnames = 0;
	nrnames = 0;
	mrnames = 0;
	rows = 0;
	cols = 0;
	/* mrows = 0;*/
	mrows = -1;

		/* check conformability of matrix arguments */

	n = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
		if( length(CAR(t)) >= 0 ) {
			dims = getAttrib(CAR(t), R_DimSymbol);
			if (length(dims) == 2) {
				if (mrows == -1)
					mrows = INTEGER(dims)[0];
				else if (mrows != INTEGER(dims)[0])
					errorcall(call, "number of rows of matrices must match (see arg %d)\n", n + 1);
				cols += INTEGER(dims)[1];
			}
			else if (length(CAR(t))>0) {
				rows = imax(rows, length(CAR(t)));
				cols += 1;
			}
		}
		n++;
	}
	if (mrows != -1) rows = mrows;

		/* check conformability of vector arguments */
		/* look for dimnames */

	n = 0;
	warned = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
		n++;
		if (length(CAR(t)) >= 0 ) {
			dims = getAttrib(CAR(t), R_DimSymbol);
			if (length(dims) == 2) {
				dn = getAttrib(CAR(t), R_DimNamesSymbol);
				if(CADR(dn) != R_NilValue)
					have_cnames = 1;
				if(CAR(dn) != R_NilValue)
					mrnames = mrows;
			}
			else {
				k = length(CAR(t));
				if(!warned && k>0 && (k > rows || rows % k)) {
					warned = 1;
					PROTECT(call = substituteList(call, rho));
					warningcall(call, "number of rows of result\n\tis not a multiple of vector length (arg %d)\n", n);
					UNPROTECT(1);
				}
				dn = getAttrib(CAR(t), R_NamesSymbol);
				if(TAG(t) != R_NilValue)
					have_cnames = 1;
				nrnames = imax(nrnames, length(dn));
			}
		}
	}
	if(mrnames || nrnames == rows)
		have_rnames = 1;

	PROTECT(result = allocMatrix(mode, rows, cols));
	n = 0;

	if (mode == STRSXP) {
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if( length(CAR(t)) > 0 ) {
				u = CAR(t) = coerceVector(CAR(t), STRSXP);
				k = LENGTH(u);
				idx = (!isMatrix(CAR(t))) ? rows : k;
				for (i = 0; i < idx; i++)
					STRING(result)[n++] = STRING(u)[i % k];
			}
		}
	}
	else if(mode == CPLXSXP) {
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if( length(CAR(t)) > 0 ) {
				u = CAR(t) = coerceVector(CAR(t), CPLXSXP);
				k = LENGTH(u);
				idx = (!isMatrix(CAR(t))) ? rows : k;
				for (i = 0; i < idx; i++)
					COMPLEX(result)[n++] = COMPLEX(u)[i % k];
			}
		}
	}
	else {
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if( length(CAR(t)) > 0 ) {
				u = CAR(t);
				k = LENGTH(u);
				idx = (!isMatrix(CAR(t))) ? rows : k;
				if (TYPEOF(u) <= INTSXP) {
					if (mode <= INTSXP) {
						for (i = 0; i < idx; i++)
							INTEGER(result)[n++] = INTEGER(u)[i % k];
					}
					else {
						for (i = 0; i < idx; i++)
							REAL(result)[n++] = (INTEGER(u)[i % k]) == NA_INTEGER ? NA_REAL : INTEGER(u)[i % k];
					}
				}
				else {
					for (i = 0; i < idx; i++)
						REAL(result)[n++] = REAL(u)[i % k];
				}
			}
		}
	}
	if(have_cnames | have_rnames) {
		PROTECT(blank = mkChar(""));
		PROTECT(dn = allocList(2));
		if(have_cnames) CADR(dn) = allocVector(STRSXP, cols);
		j = 0;
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if( length(CAR(t))>=0) {
				if (isMatrix(CAR(t))) {
					u = getAttrib(CAR(t), R_DimNamesSymbol);
					if(have_rnames && CAR(dn) == R_NilValue
					   && CAR(u) != R_NilValue)
						CAR(dn) = duplicate(CAR(u));
					if(CADR(u) != R_NilValue) {
						for(i=0 ; i<length(CADR(u)) ; i++)
							STRING(CADR(dn))[j++] = STRING(CADR(u))[i];
					}
					else if( have_cnames ) {
							for(i=0 ; i<ncols(CAR(t)) ; i++)
								STRING(CADR(dn))[j++] = blank;
					}
				}
				else if (length(CAR(t))>0) {
					u = getAttrib(CAR(t), R_NamesSymbol);
					if(have_rnames && CAR(dn) == R_NilValue
					   && u != R_NilValue && length(u) == rows)
						CAR(dn) = duplicate(u);
					if(TAG(t) != R_NilValue)
						STRING(CADR(dn))[j++] = PRINTNAME(TAG(t));
					else if(have_cnames)
						STRING(CADR(dn))[j++] = blank;
				}
			}
		}
		setAttrib(result, R_DimNamesSymbol, dn);
		UNPROTECT(2);
	}
	UNPROTECT(1);
	return result;
}

static SEXP rbind(SEXP call, SEXP args, SEXPTYPE mode)
{
	int i, j, k, n;
	int have_rnames, have_cnames;
	int ncnames, mcnames;
	int rows, cols, mcols, mrows;
	int warned;
	SEXP blank, dims, dn, result, t, u;

	have_rnames = 0;
	have_cnames = 0;
	ncnames = 0;
	mcnames = 0;
	rows = 0;
	cols = 0;
	mcols = 0;

		/* check conformability of matrix arguments */

	n = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
		if (length(CAR(t))>=0) {
			dims = getAttrib(CAR(t), R_DimSymbol);
			if (length(dims) == 2) {
				if (mcols == 0)
					mcols = INTEGER(dims)[1];
				else if (mcols != INTEGER(dims)[1])
					errorcall(call, "number of columns of matrices must match (see arg %d)\n", n + 1);
				rows += INTEGER(dims)[0];
			}
			else if(length(CAR(t))>0){
				cols = imax(cols, length(CAR(t)));
				rows += 1;
			}
		}
		n++;
	}
	if (mcols != 0) cols = mcols;

		/* check conformability of vector arguments */
		/* look for dimnames */

	n = 0;
	warned = 0;
	for (t = args; t != R_NilValue; t = CDR(t)) {
		n++;
		if (length(CAR(t))>=0) {
			dims = getAttrib(CAR(t), R_DimSymbol);
			if (length(dims) == 2) {
				dn = getAttrib(CAR(t), R_DimNamesSymbol);
				if(CAR(dn) != R_NilValue)
					have_rnames = 1;
				if(CADR(dn) != R_NilValue)
					mcnames = mcols;
			}
			else {
				k = length(CAR(t));
				if(!warned && k>0 && (k > cols || cols % k)) {
					warned = 1;
					PROTECT(call = substituteList(call, rho));
					warningcall(call, "number of columns of result\n\tnot a multiple of vector length (arg %d)\n", n);
					UNPROTECT(1);
				}
				dn = getAttrib(CAR(t), R_NamesSymbol);
				if(TAG(t) != R_NilValue)
					have_rnames = 1;
				ncnames = imax(ncnames, length(dn));
			}
		}
	}
	if(mcnames || ncnames == cols)
		have_cnames = 1;

	PROTECT(result = allocMatrix(mode, rows, cols));
	n = 0;
	if (mode == STRSXP) {
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if (length(CAR(t))>=0) {
				CAR(t) = coerceVector(CAR(t), STRSXP);
				u = CAR(t);
				k = LENGTH(u);
				mrows = (isMatrix(u)) ? nrows(u) : 1;
				for (i = 0; i < mrows; i++)
					for (j = 0; j < cols; j++)
						STRING(result)[i + n + (j * rows)] = STRING(u)[(i + j * mrows) % k];
				n += mrows;
			}
		}
		UNPROTECT(1);
		return result;
	}
	else if (mode == CPLXSXP) {
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if (length(CAR(t))>0) {
				CAR(t) = coerceVector(CAR(t), CPLXSXP);
				u = CAR(t);
				k = LENGTH(u);
				mrows = (isMatrix(u)) ? nrows(u) : 1;
				for (i = 0; i < mrows; i++)
					for (j = 0; j < cols; j++)
						COMPLEX(result)[i + n + (j * rows)] = COMPLEX(u)[(i + j * mrows) % k];
				n += mrows;
			}
		}
		UNPROTECT(1);
		return result;
	}
	for (t = args; t != R_NilValue; t = CDR(t)) {
		if (length(CAR(t))>=0) {
			u = CAR(t);
			k = LENGTH(u);
			/*
			mrows = (isMatrix(u)) ? nrows(u) : 1;
			*/
			if (isMatrix(u)) {
			  mrows=nrows(u);
			}
			else {
			  if (length(u)==0) mrows=0; else mrows=1;
			}
			if (TYPEOF(u) <= INTSXP) {
				if (mode <= INTSXP) {
					for (i = 0; i < mrows; i++)
						for (j = 0; j < cols; j++)
							INTEGER(result)[i + n + (j * rows)] = INTEGER(u)[(i + j * mrows) % k];
					n += mrows;
				}
				else {
					for (i = 0; i < mrows; i++)
						for (j = 0; j < cols; j++)
							REAL(result)[i + n + (j * rows)] = (INTEGER(u)[(i + j * mrows) % k]) == NA_INTEGER ? NA_REAL : INTEGER(u)[(i + j * mrows) % k];
					n += mrows;
				}
			}
			else {
				for (i = 0; i < mrows; i++)
					for (j = 0; j < cols; j++)
						REAL(result)[i + n + (j * rows)] = REAL(u)[(i + j * mrows) % k];
				n += mrows;
			}
		}
	}
	if(have_rnames | have_cnames) {
		PROTECT(blank = mkChar(""));
		PROTECT(dn = allocList(2));
		if(have_rnames) CAR(dn) = allocVector(STRSXP, rows);
		j = 0;
		for (t = args; t != R_NilValue; t = CDR(t)) {
			if (length(CAR(t))>=0) {
				if (isMatrix(CAR(t))) {
					u = getAttrib(CAR(t), R_DimNamesSymbol);
					if(have_cnames && CADR(dn) == R_NilValue
					   && CADR(u) != R_NilValue)
						CADR(dn) = duplicate(CADR(u));
					if(have_rnames) {
						if(CAR(u) != R_NilValue) {
							for(i=0 ; i<length(CAR(u)) ; i++)
								STRING(CAR(dn))[j++] = STRING(CAR(u))[i];
						}
						else {
							for(i=0 ; i<nrows(CAR(t)) ; i++)
								STRING(CAR(dn))[j++] = blank;
						}
					}
				}
				else if (length(CAR(t))>0){
					u = getAttrib(CAR(t), R_NamesSymbol);
					if(have_cnames && CADR(dn) == R_NilValue
					   && u != R_NilValue && length(u) == cols)
						CADR(dn) = duplicate(u);
					if(TAG(t) != R_NilValue)
						STRING(CAR(dn))[j++] = PRINTNAME(TAG(t));
					else if(have_rnames)
						STRING(CAR(dn))[j++] = blank;
				}
			}
		}
		setAttrib(result, R_DimNamesSymbol, dn);
		UNPROTECT(2);
	}
	UNPROTECT(1);
	return result;
}
