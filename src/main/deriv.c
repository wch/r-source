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


	/* Symbolic Differentiation */

	/* note: the code below makes use of "expression" objects */
	/* maybe it is time for us to introduce mode "expression" */

#include "Defn.h"

static SEXP ParenSymbol;
static SEXP PlusSymbol;
static SEXP MinusSymbol;
static SEXP TimesSymbol;
static SEXP DivideSymbol;
static SEXP PowerSymbol;
static SEXP ExpSymbol;
static SEXP LogSymbol;
static SEXP SinSymbol;
static SEXP CosSymbol;
static SEXP TanSymbol;
static SEXP SinhSymbol;
static SEXP CoshSymbol;
static SEXP TanhSymbol;
static SEXP SqrtSymbol;

static void InitDerivSymbols()
{
	ParenSymbol = install("(");
	PlusSymbol = install("+");
	MinusSymbol = install("-");
	TimesSymbol = install("*");
	DivideSymbol = install("/");
	PowerSymbol = install("^");
	ExpSymbol = install("exp");
	LogSymbol = install("log");
	SinSymbol = install("sin");
	CosSymbol = install("cos");
	TanSymbol = install("tan");
	SinhSymbol = install("sinh");
	CoshSymbol = install("cosh");
	TanhSymbol = install("tanh");
	SqrtSymbol = install("sqrt");
}

static SEXP Constant(double x)
{
	SEXP s = allocVector(REALSXP, 1);
	REAL(s)[0] = x;
	return s;
}

static int isZero(SEXP s)
{
	return asReal(s) == 0.0;
}

static int isOne(SEXP s)
{
	return asReal(s) == 1.0;
}

static int isUminus(SEXP s)
{
	if(TYPEOF(s) == LANGSXP && CAR(s) == MinusSymbol) {
		switch(length(s)) {
		case 2:
			return 1;
		case 3:
			if(CADDR(s) == R_MissingArg)
				return 1;
			else return 0;
		default:
			error("invalid form in unary minus check\n");
			return -1;/* for -Wall */
		}
	}
	else return 0;
}

static SEXP PP(SEXP s)
{
	PROTECT(s);
	return s;
}

static SEXP simplify(SEXP fun, SEXP arg1, SEXP arg2)
{
	SEXP ans;
	if(fun == PlusSymbol) {
		if(isZero(arg1))
			ans = arg2;
		else if(isZero(arg2))
			ans = arg1;
		else if(isUminus(arg1))
			ans = simplify(MinusSymbol, arg2, CADR(arg1));
		else if(isUminus(arg2))
			ans = simplify(MinusSymbol, arg1, CADR(arg2));
		else
			ans = lang3(PlusSymbol, arg1, arg2);
	}
	else if(fun == MinusSymbol) {
		if(arg2 == R_MissingArg) {
			if(isZero(arg1)) {
				ans = Constant(0.0);
			}
			else if(isUminus(arg1)) {
				ans = CADR(arg1);
			}
			else ans = lang2(MinusSymbol, arg1);
		}
		else {
			if(isZero(arg2)) {
				ans = arg1;
			}
			else if(isZero(arg1)) {
				ans = simplify(MinusSymbol,
					arg2,
					R_MissingArg);
			}
			else if(isUminus(arg1)) {
				ans = simplify(MinusSymbol,
					PP(simplify(PlusSymbol,
						CADR(arg1),
						arg2)),
					R_MissingArg);
				UNPROTECT(1);
			}
			else if(isUminus(arg2)) {
				ans = simplify(PlusSymbol,
					arg1,
					CADR(arg2));
			}
			else ans = lang3(MinusSymbol, arg1, arg2);
		}
	}
	else if(fun == TimesSymbol) {
		if(isZero(arg1) || isZero(arg2)) {
			ans = Constant(0.0);
		}
		else if(isOne(arg1)) {
			ans = arg2;
		}
		else if(isOne(arg2)) {
			ans = arg1;
		}
		else if(isUminus(arg1)) {
			ans = simplify(MinusSymbol,
				PP(simplify(TimesSymbol, CADR(arg1), arg2)),
				R_MissingArg);
			UNPROTECT(1);
		}
		else if(isUminus(arg2)) {
			ans = simplify(MinusSymbol,
				PP(simplify(TimesSymbol, arg1, CADR(arg2))),
				R_MissingArg);
			UNPROTECT(1);
		}
		else ans = lang3(TimesSymbol, arg1, arg2);
	}
	else if(fun == DivideSymbol) {
		if(isZero(arg1)) {
			ans = Constant(0.0);
		}
		else if(isZero(arg2)) {
			ans = Constant(NA_REAL);
		}
		else if(isOne(arg2)) {
			ans = arg1;
		}
		else if(isUminus(arg1)) {
			ans = simplify(MinusSymbol,
				PP(simplify(DivideSymbol, CADR(arg1), arg2)),
				R_MissingArg);
			UNPROTECT(1);
		}
		else if(isUminus(arg2)) {
			ans = simplify(MinusSymbol,
				PP(simplify(DivideSymbol, arg1, CADR(arg2))),
				R_MissingArg);
			UNPROTECT(1);
		}
		else ans = lang3(DivideSymbol, arg1, arg2);
	}
	else if(fun == PowerSymbol) {
		if(isZero(arg2)) {
			ans = Constant(1.0);
		}
		else if(isZero(arg1)) {
			ans = Constant(0.0);
		}
		else if(isOne(arg1)) {
			ans = Constant(1.0);
		}
		else if(isOne(arg2)) {
			ans = arg1;
		}
		else ans = lang3(PowerSymbol, arg1, arg2);
	}
	else if(fun == ExpSymbol) {
		ans = lang2(ExpSymbol, arg1);
	}
	else if(fun == LogSymbol) {
		ans = lang2(LogSymbol, arg1);
	}
	else if(fun == CosSymbol) {
		ans = lang2(CosSymbol, arg1);
	}
	else if(fun == SinSymbol) {
		ans = lang2(SinSymbol, arg1);
	}
	else if(fun == TanSymbol) {
		ans = lang2(TanSymbol, arg1);
	}
	else if(fun == CoshSymbol) {
		ans = lang2(CoshSymbol, arg1);
	}
	else if(fun == SinhSymbol) {
		ans = lang2(SinhSymbol, arg1);
	}
	else if(fun == TanhSymbol) {
		ans = lang2(TanhSymbol, arg1);
	}
	else ans = Constant(NA_REAL);

#ifdef NOTYET
	if(length(ans) == 2 && isAtomic(CADR(ans)) && CAR(ans) != MinusSymbol)
		c = eval(c, rho);
	if(length(c) == 3 && isAtomic(CADR(ans)) && isAtomic(CADDR(ans)))
		c = eval(c, rho)
#endif
	return ans;
}

static SEXP D(SEXP expr, SEXP var)
{
	SEXP ans, expr1, expr2;

	switch(TYPEOF(expr)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
		ans = Constant(0);
		break;
	case SYMSXP:
		if(expr == var) ans = Constant(1.0);
		else ans = Constant(0.0);
		break;
	case LISTSXP:
		if(inherits(expr, "expression")) ans = D(CAR(expr), var);
		else ans = Constant(NA_REAL);
		break;
	case LANGSXP:
		if(CAR(expr) == ParenSymbol) {
			ans = D(CADR(expr), var);
		}
		else if(CAR(expr) == PlusSymbol) {
			if(length(expr) == 2)
				ans = D(CADR(expr), var);
			else {
				ans = simplify(PlusSymbol,
					PP(D(CADR(expr), var)),
					PP(D(CADDR(expr), var)));
				UNPROTECT(2);
			}
		}
		else if(CAR(expr) == MinusSymbol) {
			if(length(expr) == 2) {
				ans = simplify(MinusSymbol,
					PP(D(CADR(expr), var)),
					R_MissingArg);
				UNPROTECT(1);
			}
			else {
				ans = simplify(MinusSymbol,
					PP(D(CADR(expr), var)),
					PP(D(CADDR(expr), var)));
				UNPROTECT(2);
			}
		}
		else if(CAR(expr) == TimesSymbol) {
			ans = simplify(PlusSymbol,
				PP(simplify(TimesSymbol,
					PP(D(CADR(expr), var)),
					CADDR(expr))),
				PP(simplify(TimesSymbol,
					CADR(expr),
					PP(D(CADDR(expr), var)))));
			UNPROTECT(4);
		}
		else if(CAR(expr) == DivideSymbol) {
			PROTECT(expr1 = D(CADR(expr), var));
			PROTECT(expr2 = D(CADDR(expr), var));
			ans = simplify(MinusSymbol,
				PP(simplify(DivideSymbol,
					expr1,
					CADDR(expr))),
				PP(simplify(DivideSymbol,
					PP(simplify(TimesSymbol,
						CADR(expr),
						expr2)),
					PP(simplify(PowerSymbol,
						CADDR(expr),
						PP(Constant(2.0)))))));
			UNPROTECT(7);
		}
		else if(CAR(expr) == PowerSymbol) {
			if(isLogical(CADDR(expr)) || isNumeric(CADDR(expr))) {
				ans = simplify(TimesSymbol,
					CADDR(expr),
					PP(simplify(TimesSymbol,
						PP(D(CADR(expr), var)),
						PP(simplify(PowerSymbol,
							CADR(expr),
							PP(Constant(asReal(CADDR(expr))-1.0)))))));
				UNPROTECT(4);
			}
			else {
				expr1 = simplify(TimesSymbol,
					PP(simplify(PowerSymbol,
						CADR(expr),
						PP(simplify(MinusSymbol,
							CADDR(expr),
							PP(Constant(1.0)))))),
					PP(simplify(TimesSymbol,
						CADDR(expr),
						PP(D(CADR(expr), var))))),
				UNPROTECT(5);
				PROTECT(expr1);
				expr2 = simplify(TimesSymbol,
					PP(simplify(PowerSymbol,
						CADR(expr),
						CADDR(expr))),
					PP(simplify(TimesSymbol,
						PP(simplify(LogSymbol,
							CADR(expr),
							R_MissingArg)),
						PP(D(CADDR(expr), var)))));
				UNPROTECT(4);
				PROTECT(expr2);
				ans = simplify(PlusSymbol, expr1, expr2);
				UNPROTECT(2);
			}
		}
		else if(CAR(expr) == ExpSymbol) {
			ans = simplify(TimesSymbol,
				expr,
				PP(D(CADR(expr), var)));
			UNPROTECT(1);
		}
		else if(CAR(expr) == LogSymbol) {
			ans = simplify(DivideSymbol,
				PP(D(CADR(expr), var)),
				CADR(expr));
			UNPROTECT(1);
		}
		else if(CAR(expr) == CosSymbol) {
			ans = simplify(TimesSymbol,
				PP(simplify(SinSymbol, CADR(expr), R_MissingArg)),
				PP(simplify(MinusSymbol,
					PP(D(CADR(expr), var)),
					R_MissingArg)));
			UNPROTECT(3);
		}
		else if(CAR(expr) == SinSymbol) {
			ans = simplify(TimesSymbol,
				PP(simplify(CosSymbol, CADR(expr), R_MissingArg)),
				PP(D(CADR(expr), var)));
			UNPROTECT(2);
		}
		else if(CAR(expr) == TanSymbol) {
			ans = simplify(DivideSymbol,
				PP(D(CADR(expr), var)),
				PP(simplify(PowerSymbol,
					PP(simplify(CosSymbol,
						CADR(expr),
						R_MissingArg)),
					PP(Constant(2.0)))));
			UNPROTECT(4);
		}
		else if(CAR(expr) == CoshSymbol) {
			ans = simplify(TimesSymbol,
				PP(simplify(SinhSymbol, CADR(expr), R_MissingArg)),
				PP(D(CADR(expr), var)));
			UNPROTECT(2);
		}
		else if(CAR(expr) == SinhSymbol) {
			ans = simplify(TimesSymbol,
				PP(simplify(CoshSymbol, CADR(expr), R_MissingArg)),
				PP(D(CADR(expr), var))),
			UNPROTECT(2);
		}
		else if(CAR(expr) == TanhSymbol) {
			ans = simplify(DivideSymbol,
				PP(D(CADR(expr), var)),
				PP(simplify(PowerSymbol,
					PP(simplify(CoshSymbol, CADR(expr), R_MissingArg)),
					PP(Constant(2.0)))));
			UNPROTECT(4);
		}
		else if(CAR(expr) == SqrtSymbol) {
			PROTECT(expr1 = allocList(3));
			TYPEOF(expr1) = LANGSXP;
			CAR(expr1) = PowerSymbol;
			CADR(expr1) = CADR(expr);
			CADDR(expr1) = Constant(0.5);
			ans = D(expr1, var);
			UNPROTECT(1);
		}
		else error("Function %s is not in the derivatives table\n",
			PRINTNAME(CAR(expr)));
		break;
	default:
		ans = Constant(NA_REAL);
	}
	return ans;
}

static int isPlusForm(SEXP expr)
{
	return TYPEOF(expr) == LANGSXP
		&& length(expr) == 3
		&& CAR(expr) == PlusSymbol;
}

static int isMinusForm(SEXP expr)
{
	return TYPEOF(expr) == LANGSXP
		&& length(expr) == 3
		&& CAR(expr) == MinusSymbol;
}

static int isTimesForm(SEXP expr)
{
	return TYPEOF(expr) == LANGSXP
		&& length(expr) == 3
		&& CAR(expr) == TimesSymbol;
}

static int isDivideForm(SEXP expr)
{
	return TYPEOF(expr) == LANGSXP
		&& length(expr) == 3
		&& CAR(expr) == DivideSymbol;
}

static int isPowerForm(SEXP expr)
{
	return (TYPEOF(expr) == LANGSXP
		&& length(expr) == 3
		&& CAR(expr) == PowerSymbol);
}

static SEXP AddParens(SEXP expr)
{
	SEXP e;

	if(TYPEOF(expr) == LANGSXP) {
		e = CDR(expr);
		while(e != R_NilValue) {
			CAR(e) = AddParens(CAR(e));
			e = CDR(e);
		}
	}

	if(isPlusForm(expr)) {
		if(isPlusForm(CADDR(expr))) {
			CADDR(expr) = lang2(ParenSymbol, CADDR(expr));
		}
	}
	else if(isMinusForm(expr)) {
		if(isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))) {
			CADDR(expr) = lang2(ParenSymbol, CADDR(expr));
		}
	}
	else if(isTimesForm(expr)) {
		if(isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
		|| isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
			CADDR(expr) = lang2(ParenSymbol, CADDR(expr));
		}
		if(isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
			CADR(expr) = lang2(ParenSymbol, CADR(expr));
		}
	}
	else if(isDivideForm(expr)) {
		if(isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
		|| isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
			CADDR(expr) = lang2(ParenSymbol, CADDR(expr));
		}
		if(isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
			CADR(expr) = lang2(ParenSymbol, CADR(expr));
		}
	}
	else if(isPowerForm(expr)) {
		if(isPowerForm(CADR(expr))) {
			CADR(expr) = lang2(ParenSymbol, CADR(expr));
		}
		if(isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
		|| isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
			CADDR(expr) = lang2(ParenSymbol, CADDR(expr));
		}
	}
	return expr;
}

SEXP do_D(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP expr, var;

	checkArity(op, args);

	if(isExpression(CAR(args))) expr = VECTOR(CAR(args))[0];
	else expr = CAR(args);
	var = CADR(args);
	if(!isString(var) || length(var) < 1)
		errorcall(call, "variable must be a character string\n");
	var = install(CHAR(STRING(var)[0]));
	InitDerivSymbols();
	PROTECT(expr = D(expr, var));
	expr = AddParens(expr);
	UNPROTECT(1);
	return expr;
}

	/* ------ FindSubexprs ------ and ------ Accumulate ------ */

static void InvalidExpression(char *where)
{
	error("invalid expression in \"%s\"\n", where);
}

static int equal(SEXP expr1, SEXP expr2)
{
	if(TYPEOF(expr1) == TYPEOF(expr2)) {
		switch(TYPEOF(expr1)) {
		case NILSXP:
			return 1;
		case SYMSXP:
			return expr1 == expr2;
		case LGLSXP:
		case INTSXP:
			return INTEGER(expr1)[0] == INTEGER(expr2)[0];
		case REALSXP:
			return REAL(expr1)[0] == REAL(expr2)[0];
		case CPLXSXP:
			return COMPLEX(expr1)[0].r == COMPLEX(expr2)[0].r
				&& COMPLEX(expr1)[0].i == COMPLEX(expr2)[0].i;
		case LANGSXP:
		case LISTSXP:
			return equal(CAR(expr1), CAR(expr2))
				&& equal(CDR(expr1), CDR(expr2));
		default:
			InvalidExpression("equal");
		}
	}
	return 0;
}

static SEXP exprlist;

static int Accumulate(SEXP expr)
{
	SEXP e;
	int k;
	e = exprlist;
	k = 0;
	while(CDR(e) != R_NilValue) {
		e = CDR(e);
		k = k + 1;
		if(equal(expr, CAR(e)))
			return k;
	}
	CDR(e) = CONS(expr, R_NilValue);
	return k + 1;
}

static int Accumulate2(SEXP expr)
{
	SEXP e;
	int k;
	e = exprlist;
	k = 0;
	while(CDR(e) != R_NilValue) {
		e = CDR(e);
		k = k + 1;
	}
	CDR(e) = CONS(expr, R_NilValue);
	return k + 1;
}

static SEXP tag;

static SEXP MakeVariable(int k)
{
	char buf[64];
	sprintf(buf, "%s%d", CHAR(STRING(tag)[0]), k);
	return install(buf);
}

static int FindSubexprs(SEXP expr)
{
	SEXP e;
	int k;
	switch(TYPEOF(expr)) {
	case SYMSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
		return 0;
		break;
	case LISTSXP:
		if(inherits(expr, "expression"))
			return FindSubexprs(CAR(expr));
		else InvalidExpression("FindSubexprs");
		break;
	case LANGSXP:
		if(CAR(expr) == install("(")) {
			return FindSubexprs(CADR(expr));
		}
		else {
			e = CDR(expr);
			while(e != R_NilValue) {
				if((k = FindSubexprs(CAR(e))) != 0)
					CAR(e) = MakeVariable(k);
				e = CDR(e);
			}
			return Accumulate(expr);
		}
		break;
	default:
		InvalidExpression("FindSubexprs");
		return -1/*-Wall*/;
	}
}

static int CountOccurrences(SEXP sym, SEXP lst)
{
	switch(TYPEOF(lst)) {
	case SYMSXP:
		return lst == sym;
	case LISTSXP:
	case LANGSXP:
		return CountOccurrences(sym, CAR(lst))
			+ CountOccurrences(sym, CDR(lst));
	default:
		return 0;
	}
}

static SEXP Replace(SEXP sym, SEXP expr, SEXP lst)
{
	switch(TYPEOF(lst)) {
	case SYMSXP:
		if(lst == sym) return expr;
		else return lst;
	case LISTSXP:
	case LANGSXP:
		CAR(lst) = Replace(sym, expr, CAR(lst));
		CDR(lst) = Replace(sym, expr, CDR(lst));
		return lst;
	default:
		return lst;
	}
}

static SEXP CreateGrad(SEXP names)
{
	SEXP p, q, data, dim, dimnames;
	int i, n;

	n = length(names);
	PROTECT(dimnames = lang3(R_NilValue, R_NilValue, R_NilValue));
	CAR(dimnames) = install("list");
	p = install("c");
	PROTECT(q = allocList(n));
	CADDR(dimnames) = LCONS(p, q);
	UNPROTECT(1);
	for(i=0 ; i<n ; i++) {
		CAR(q) = allocVector(STRSXP, 1);
		STRING(CAR(q))[0] = STRING(names)[i];
		q = CDR(q);
	}

	PROTECT(dim = lang3(R_NilValue, R_NilValue, R_NilValue));
	CAR(dim) = install("c");
	CADR(dim) = lang2(install("length"), install(".value"));
	CADDR(dim) = allocVector(REALSXP, 1);
	REAL(CADDR(dim))[0] = length(names);

	PROTECT(data = allocVector(REALSXP, 1));
	REAL(data)[0] = 0;

	PROTECT(p = lang4(install("array"), data, dim, dimnames));
	p = lang3(install("<-"), install(".grad"), p);
	UNPROTECT(4);
	return p;
}

static SEXP DerivAssign(SEXP name, SEXP expr)
{
	SEXP ans, newname;

	PROTECT(ans = lang3(install("<-"), R_NilValue, expr));
	PROTECT(newname = allocVector(STRSXP, 1));
	STRING(newname)[0] = name;
	CADR(ans) = lang4(install("["), install(".grad"), R_MissingArg, newname);
	UNPROTECT(2);
	return ans;
}

		/* attr(.value, "gradient") <- .grad */
static SEXP AddGrad()
{
	SEXP ans;
	PROTECT(ans = mkString("gradient"));
	PROTECT(ans = lang3(install("attr"), install(".value"), ans));
	ans = lang3(install("<-"), ans, install(".grad"));
	UNPROTECT(2);
	return ans;
}

static SEXP Prune(SEXP lst)
{
	if(lst == R_NilValue)
		return lst;
	CDR(lst) = Prune(CDR(lst));
	if(CAR(lst) == R_MissingArg)
		return CDR(lst);
	else return lst ;
}

SEXP do_deriv(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans, expr, funarg, names;
	int f_index, *d_index;
	int i, nexpr, nderiv;
	char *vmax;

	checkArity(op, args);
	vmax = vmaxget();

	InitDerivSymbols();
	PROTECT(exprlist = LCONS(install("{"), R_NilValue));

	if(isExpression(CAR(args))) PROTECT(expr = VECTOR(CAR(args))[0]);
	else PROTECT(expr = CAR(args));
	args = CDR(args);

	names = CAR(args);
	if(!isString(names) || (nderiv = length(names)) < 1)
		errorcall(call, "invalid variable names\n");
	args = CDR(args);

	PROTECT(funarg = duplicate(CAR(args)));
	args = CDR(args);

	tag = CAR(args);
	if(!isString(tag) || length(tag) < 1
	|| length(STRING(tag)[0]) < 1 || length(STRING(tag)[0]) > 60)
		errorcall(call, "invalid tag\n");

	/* NOTE: FindSubexprs is destructive, hence the duplication */

	PROTECT(ans = duplicate(expr));
	f_index = FindSubexprs(ans);
	UNPROTECT(1);

	d_index = (int*)R_alloc(nderiv, sizeof(int));
	for(i=0 ; i<nderiv ; i++) {
		PROTECT(ans = duplicate(expr));
		PROTECT(ans = D(ans, install(CHAR(STRING(names)[i]))));
		d_index[i] = FindSubexprs(ans);
		UNPROTECT(2);
	}
	nexpr = length(exprlist) - 1;
	if(f_index) {
		Accumulate2(MakeVariable(f_index));
	}
	else {
		PROTECT(ans = duplicate(expr));
		Accumulate2(expr);
		UNPROTECT(1);
	}

	Accumulate2(R_NilValue);

	for(i=0 ; i<nderiv ; i++) {
		if(d_index[i]) {
			Accumulate2(MakeVariable(d_index[i]));
		}
		else {
			PROTECT(ans = duplicate(expr));
			PROTECT(ans = D(ans, install(CHAR(STRING(names)[i]))));
			Accumulate2(ans);
			UNPROTECT(2);
		}
	}
	Accumulate2(R_NilValue);
	Accumulate2(R_NilValue);

	i = 0;
	ans = CDR(exprlist);
	while(i < nexpr) {
		if(CountOccurrences(MakeVariable(i+1), CDR(ans)) < 2) {
			CDR(ans) = Replace(MakeVariable(i+1), CAR(ans), CDR(ans));
			CAR(ans) = R_MissingArg;
		}
		else CAR(ans) = lang3(install("<-"), MakeVariable(i+1), AddParens(CAR(ans)));
		i = i + 1;
		ans = CDR(ans);
	}

		/* .value <- ... */

	CAR(ans) = lang3(install("<-"), install(".value"), AddParens(CAR(ans)));
	ans = CDR(ans);

		/* .grad <- ... */

	CAR(ans) = CreateGrad(names);
	ans = CDR(ans);

		/* .grad[, "..."] <- ... */

	for(i=0 ; i<nderiv ; i++) {
		CAR(ans) = DerivAssign(STRING(names)[i], AddParens(CAR(ans)));
		ans = CDR(ans);
	}

		/* attr(.value, "gradient") <- .grad */

	CAR(ans) = AddGrad();
	ans = CDR(ans);

		/* .value */

	CAR(ans) = install(".value");


		/* Prune the expression list */
		/* removing eliminated sub-expressions */

	CDR(exprlist) = Prune(CDR(exprlist));

	if(TYPEOF(funarg) == CLOSXP) {
		BODY(funarg) = exprlist;
	}
	else if(isString(funarg)) {
		PROTECT(names = duplicate(funarg));
		funarg = allocSExp(CLOSXP);
		FORMALS(funarg) = ans = allocList(length(names));
		for(i=0 ; i<length(names) ; i++) {
			TAG(ans) = install(CHAR(STRING(names)[i]));
			CAR(ans) = R_MissingArg;
			ans = CDR(ans);
		}
		BODY(funarg) = exprlist;
		CLOENV(funarg) = R_GlobalEnv;
		UNPROTECT(1);
	}
	else {
		funarg = allocVector(EXPRSXP, 1);
		VECTOR(funarg)[0] = exprlist;
		/* funarg = lang2(install("expression"), exprlist); */
	}
	UNPROTECT(3);
	vmaxset(vmax);
	return funarg;
}
