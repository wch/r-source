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
#include "Print.h"

#define MAX_WIDTH	200
#define MIN_WIDTH	10
#define MIN_DIGITS	1
#define MAX_DIGITS	22
#define MIN_EXPRESSIONS 25
#define MAX_EXPRESSIONS 100000

/*
"prompt"
"continue"
"editor"
"expressions"
"width"
"digits"
"contrasts"

"echo"
"error"
"free"
"keep"
"length"
"memory"
"object.size"
"pager"
"reference"
"scrap"
"show"
"ts.eps"
"warn"
*/

static SEXP Options(void)
{
	return install(".Options");
}

static SEXP optInteger(int k)
{
	SEXP v = allocVector(INTSXP, 1);
	INTEGER(v)[0] = k;
	return v;
}

static SEXP optString(SEXP c)
{
	SEXP v;
	PROTECT(c);
	v = allocVector(STRSXP, 1);
	STRING(v)[0] = c;
	UNPROTECT(1);
	return v;
}

static SEXP FindTaggedItem(SEXP lst, SEXP tag)
{
	for( ; lst!=R_NilValue ; lst=CDR(lst)) {
		if(TAG(lst) == tag)
			return lst;
	}
	return R_NilValue;
}

SEXP GetOption(SEXP tag, SEXP rho)
{
#ifdef OLD
	SEXP opt = findVar(Options(), rho);
#endif
	SEXP opt = findVar(Options(), R_NilValue);
	if(!isList(opt))
		error("corrupted options list\n");
	opt = FindTaggedItem(opt, tag);
	return CAR(opt);
}

int GetOptionWidth(SEXP rho)
{
	int w;

	w = asInteger(GetOption(install("width"), rho));
	if( w < MIN_WIDTH || w > MAX_WIDTH ) {
		warning("invalid printing width, used 80\n");
		return 80;
	}
	return w;
}

int GetOptionDigits(SEXP rho)
{
	int d;

	d = asInteger(GetOption(install("digits"), rho));
	if( d < MIN_DIGITS || d > MAX_DIGITS ) {
		warning("invalid printing digits, used 7\n");
		return 7;
	}
	return d;
}
		
/* change the value of an option or add a new option or,
   if called with value R_NilValue remove that option ;
   
*/
static SEXP SetOption(SEXP tag, SEXP value)
{
	SEXP opt, old, s, t;

	t = opt = SYMVALUE(Options());
	if(!isList(opt))
		error("corrupted options list\n");
	opt = FindTaggedItem(opt, tag);

	if( value == R_NilValue ) {  /* we're removing */
		for( ; t!=R_NilValue ; t=CDR(t)) 
			if(TAG(CDR(t)) == tag) {
				old = CAR(t);
				CDR(t)=CDDR(t);
				return old;
			}
		return R_NilValue;
	}

	/* if the option is new a new slot is added to the end of .Options */
	if ( opt == R_NilValue ) {
		while (CDR(t) != R_NilValue)
			t=CDR(t);
		CDR(t) = allocList(1);
		opt = CDR(t);
		TAG(opt) = tag;
	}
	old = CAR(opt);
	CAR(opt) = value;
	return old;
}

void InitOptions(void)
{
	SEXP t, val, v;
	
	PROTECT(v = val = allocList(7));

	TAG(v) = install("prompt"); CAR(v) = mkString("> "); v = CDR(v);
	TAG(v) = install("continue"); CAR(v) = mkString("+ "); v = CDR(v);
	TAG(v) = install("editor"); CAR(v) = mkString("vi"); v = CDR(v);
	TAG(v) = install("expressions"); CAR(v) = optInteger(100); v = CDR(v);
	TAG(v) = install("width"); CAR(v) = optInteger(80); v = CDR(v);
	TAG(v) = install("digits"); CAR(v) = optInteger(7); v = CDR(v);
	TAG(v) = install("contrasts"); CAR(v) = allocVector(STRSXP,2);
		STRING(CAR(v))[0] = mkChar("contr.treatment");
		STRING(CAR(v))[1] = mkChar("contr.poly");
	PROTECT(t = allocVector(STRSXP,2));
		STRING(t)[0] = mkChar("unordered");
		STRING(t)[1] = mkChar("ordered");
	namesgets(CAR(v), t);

	SYMVALUE(install(".Options")) = val;
	UNPROTECT(2);
}

SEXP do_options(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP a, opt, s, t;
	char *what;
	int k;

	opt = SYMVALUE(Options());

	if (args == R_NilValue) {
		NAMED(opt) = 1;
		return opt;
	}

	if (isList(CAR(args)) && length(args)==1 )
		args = CAR(args);

	for (a=args; a!=R_NilValue; a=CDR(a)) {
		if(isNull(t = TAG(a))) {
			if(!isString(CAR(a)) || LENGTH(CAR(a)) <= 0)
				errorcall(call, "invalid argument\n");
			t = install(CHAR(STRING(CAR(a))[0]));
			s = FindTaggedItem(opt, t);
			TAG(a) = t;
			CAR(a) = duplicate(CAR(s));
		}
		else {
			what = CHAR(PRINTNAME(t));
			if (streql(what, "width")) {
				k = asInteger(CAR(a));
				if (k < MIN_WIDTH || k > MAX_WIDTH)
					errorcall(call, "invalid width parameter\n");
				CAR(a) = SetOption(install("width"), optInteger(k));
			}
			else if (streql(what, "digits")) {
				k = asInteger(CAR(a));
				if (k < MIN_DIGITS || k > MAX_DIGITS)
					errorcall(call, "invalid digits parameter\n");
				CAR(a) = SetOption(install("digits"), optInteger(k));
			}
			else if (streql(what, "expressions")) {
				k = asInteger(CAR(a));
				if (k < 25 || k > MAX_EXPRESSIONS)
					errorcall(call, "expressions parameter invalid\n");
				CAR(a) = SetOption(install("expressions"), optInteger(k));
			}
			else if (streql(what, "editor")) {
				s = asChar(CAR(a));
				if (s == NA_STRING || length(s) == 0)
					errorcall(call, "invalid editor parameter\n");
				CAR(a) = SetOption(install("editor"), optString(s));
			}
			else if (streql(what, "continue")) {
				s = asChar(CAR(a));
				if (s == NA_STRING || length(s) == 0)
					errorcall(call, "invalid continue parameter\n");
				CAR(a) = SetOption(install("continue"), optString(s));
			}
			else if (streql(what, "prompt")) {
				s = asChar(CAR(a));
				if (s == NA_STRING || length(s) == 0)
					errorcall(call, "prompt parameter invalid\n");
				CAR(a) = SetOption(install("prompt"), optString(s));
			}
			else if (streql(what, "contrasts")) {
				s = CAR(a);
				if(TYPEOF(s) != STRSXP || LENGTH(s) != 2)
					errorcall(call, "contrasts parameter invalid\n");
				CAR(a) = SetOption(install("contrasts"), s);
			}
			else
				SetOption(t,duplicate(CAR(a)));
			R_Visible = 0;
		}
	}
	return (args);
}
