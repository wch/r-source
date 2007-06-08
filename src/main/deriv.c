/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
 *  Copyright (C) 2004-5        The R Foundation
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
 *
 *
 *  Symbolic Differentiation
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
static SEXP PnormSymbol;
static SEXP DnormSymbol;
static SEXP AsinSymbol;
static SEXP AcosSymbol;
static SEXP AtanSymbol;
static SEXP GammaSymbol;
static SEXP LGammaSymbol;
static SEXP PsiSymbol;

static Rboolean Initialized = FALSE;


static void InitDerivSymbols()
{
    /* Called from do_D() and do_deriv() */
    if(Initialized) return;
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
    PnormSymbol = install("pnorm");
    DnormSymbol = install("dnorm");
    AsinSymbol = install("asin");
    AcosSymbol = install("acos");
    AtanSymbol = install("atan");
    GammaSymbol = install("gamma");
    LGammaSymbol = install("lgamma");
    PsiSymbol = install("psigamma");

    Initialized = TRUE;
}

static SEXP Constant(double x)
{
    return ScalarReal(x);
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
    if (TYPEOF(s) == LANGSXP && CAR(s) == MinusSymbol) {
	switch(length(s)) {
	case 2:
	    return 1;
	case 3:
	    if (CADDR(s) == R_MissingArg)
		return 1;
	    else return 0;
	default:
	    error(_("invalid form in unary minus check"));
	    return -1;/* for -Wall */
	}
    }
    else return 0;
}

/* Pointer protect and return the argument */

static SEXP PP(SEXP s)
{
    PROTECT(s);
    return s;
}

static SEXP simplify(SEXP fun, SEXP arg1, SEXP arg2)
{
    SEXP ans;
    if (fun == PlusSymbol) {
	if (isZero(arg1))
	    ans = arg2;
	else if (isZero(arg2))
	    ans = arg1;
	else if (isUminus(arg1))
	    ans = simplify(MinusSymbol, arg2, CADR(arg1));
	else if (isUminus(arg2))
	    ans = simplify(MinusSymbol, arg1, CADR(arg2));
	else
	    ans = lang3(PlusSymbol, arg1, arg2);
    }
    else if (fun == MinusSymbol) {
	if (arg2 == R_MissingArg) {
	    if (isZero(arg1))
		ans = Constant(0.);
	    else if (isUminus(arg1))
		ans = CADR(arg1);
	    else
		ans = lang2(MinusSymbol, arg1);
	}
	else {
	    if (isZero(arg2))
		ans = arg1;
	    else if (isZero(arg1))
		ans = simplify(MinusSymbol, arg2, R_MissingArg);
	    else if (isUminus(arg1)) {
		ans = simplify(MinusSymbol,
			       PP(simplify(PlusSymbol, CADR(arg1), arg2)),
			       R_MissingArg);
		UNPROTECT(1);
	    }
	    else if (isUminus(arg2))
		ans = simplify(PlusSymbol, arg1, CADR(arg2));
	    else
		ans = lang3(MinusSymbol, arg1, arg2);
	}
    }
    else if (fun == TimesSymbol) {
	if (isZero(arg1) || isZero(arg2))
	    ans = Constant(0.);
	else if (isOne(arg1))
	    ans = arg2;
	else if (isOne(arg2))
	    ans = arg1;
	else if (isUminus(arg1)) {
	    ans = simplify(MinusSymbol,
			   PP(simplify(TimesSymbol, CADR(arg1), arg2)),
			   R_MissingArg);
	    UNPROTECT(1);
	}
	else if (isUminus(arg2)) {
	    ans = simplify(MinusSymbol,
			   PP(simplify(TimesSymbol, arg1, CADR(arg2))),
			   R_MissingArg);
	    UNPROTECT(1);
	}
	else
	    ans = lang3(TimesSymbol, arg1, arg2);
    }
    else if (fun == DivideSymbol) {
	if (isZero(arg1))
	    ans = Constant(0.);
	else if (isZero(arg2))
	    ans = Constant(NA_REAL);
	else if (isOne(arg2))
	    ans = arg1;
	else if (isUminus(arg1)) {
	    ans = simplify(MinusSymbol,
			   PP(simplify(DivideSymbol, CADR(arg1), arg2)),
			   R_MissingArg);
	    UNPROTECT(1);
	}
	else if (isUminus(arg2)) {
	    ans = simplify(MinusSymbol,
			   PP(simplify(DivideSymbol, arg1, CADR(arg2))),
			   R_MissingArg);
	    UNPROTECT(1);
	}
	else ans = lang3(DivideSymbol, arg1, arg2);
    }
    else if (fun == PowerSymbol) {
	if (isZero(arg2))
	    ans = Constant(1.);
	else if (isZero(arg1))
	    ans = Constant(0.);
	else if (isOne(arg1))
	    ans = Constant(1.);
	else if (isOne(arg2))
	    ans = arg1;
	else
	    ans = lang3(PowerSymbol, arg1, arg2);
    }
    else if (fun == ExpSymbol) {
	/* FIXME: simplify exp(lgamma( E )) = gamma( E ) */
	ans = lang2(ExpSymbol, arg1);
    }
    else if (fun == LogSymbol) {
	/* FIXME: simplify log(gamma( E )) = lgamma( E ) */
	ans = lang2(LogSymbol, arg1);
    }
    else if (fun == CosSymbol)	ans = lang2(CosSymbol, arg1);
    else if (fun == SinSymbol)	ans = lang2(SinSymbol, arg1);
    else if (fun == TanSymbol)	ans = lang2(TanSymbol, arg1);
    else if (fun == CoshSymbol) ans = lang2(CoshSymbol, arg1);
    else if (fun == SinhSymbol) ans = lang2(SinhSymbol, arg1);
    else if (fun == TanhSymbol) ans = lang2(TanhSymbol, arg1);
    else if (fun == SqrtSymbol) ans = lang2(SqrtSymbol, arg1);
    else if (fun == PnormSymbol)ans = lang2(PnormSymbol, arg1);
    else if (fun == DnormSymbol)ans = lang2(DnormSymbol, arg1);
    else if (fun == AsinSymbol) ans = lang2(AsinSymbol, arg1);
    else if (fun == AcosSymbol) ans = lang2(AcosSymbol, arg1);
    else if (fun == AtanSymbol) ans = lang2(AtanSymbol, arg1);
    else if (fun == GammaSymbol)ans = lang2(GammaSymbol, arg1);
    else if (fun == LGammaSymbol)ans = lang2(LGammaSymbol, arg1);
    else if (fun == PsiSymbol)	ans = lang2(PsiSymbol, arg1);
    else ans = Constant(NA_REAL);
    /* FIXME */
#ifdef NOTYET
    if (length(ans) == 2 && isAtomic(CADR(ans)) && CAR(ans) != MinusSymbol)
	c = eval(c, rho);
    if (length(c) == 3 && isAtomic(CADR(ans)) && isAtomic(CADDR(ans)))
	c = eval(c, rho);
#endif
    return ans;
}/* simplify() */


/* D() implements the "derivative table" : */
static SEXP D(SEXP expr, SEXP var)
{

#define PP_S(F,a1,a2) PP(simplify(F,a1,a2))
#define PP_S2(F,a1)   PP(simplify(F,a1, R_MissingArg))

    SEXP ans=R_NilValue, expr1, expr2;
    switch(TYPEOF(expr)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	ans = Constant(0);
	break;
    case SYMSXP:
	if (expr == var) ans = Constant(1.);
	else ans = Constant(0.);
	break;
    case LISTSXP:
	if (inherits(expr, "expression")) ans = D(CAR(expr), var);
	else ans = Constant(NA_REAL);
	break;
    case LANGSXP:
	if (CAR(expr) == ParenSymbol) {
	    ans = D(CADR(expr), var);
	}
	else if (CAR(expr) == PlusSymbol) {
	    if (length(expr) == 2)
		ans = D(CADR(expr), var);
	    else {
		ans = simplify(PlusSymbol,
			       PP(D(CADR(expr), var)),
			       PP(D(CADDR(expr), var)));
		UNPROTECT(2);
	    }
	}
	else if (CAR(expr) == MinusSymbol) {
	    if (length(expr) == 2) {
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
	else if (CAR(expr) == TimesSymbol) {
	    ans = simplify(PlusSymbol,
			   PP_S(TimesSymbol,PP(D(CADR(expr),var)), CADDR(expr)),
			   PP_S(TimesSymbol,CADR(expr), PP(D(CADDR(expr),var))));
	    UNPROTECT(4);
	}
	else if (CAR(expr) == DivideSymbol) {
	    PROTECT(expr1 = D(CADR(expr), var));
	    PROTECT(expr2 = D(CADDR(expr), var));
	    ans = simplify(MinusSymbol,
			   PP_S(DivideSymbol, expr1, CADDR(expr)),
			   PP_S(DivideSymbol,
				PP_S(TimesSymbol, CADR(expr), expr2),
				PP_S(PowerSymbol,CADDR(expr),PP(Constant(2.)))));
	    UNPROTECT(7);
	}
	else if (CAR(expr) == PowerSymbol) {
	    if (isLogical(CADDR(expr)) || isNumeric(CADDR(expr))) {
		ans = simplify(TimesSymbol,
			       CADDR(expr),
			       PP_S(TimesSymbol,
				    PP(D(CADR(expr), var)),
				    PP_S(PowerSymbol,
					 CADR(expr),
					 PP(Constant(asReal(CADDR(expr))-1.)))));
		UNPROTECT(4);
	    }
	    else {
		expr1 = simplify(TimesSymbol,
				 PP_S(PowerSymbol,
				      CADR(expr),
				      PP_S(MinusSymbol,
					   CADDR(expr),
					   PP(Constant(1.0)))),
				 PP_S(TimesSymbol,
				      CADDR(expr),
				      PP(D(CADR(expr), var))));
		UNPROTECT(5);
		PROTECT(expr1);
		expr2 = simplify(TimesSymbol,
				 PP_S(PowerSymbol, CADR(expr), CADDR(expr)),
				 PP_S(TimesSymbol,
				      PP_S2(LogSymbol, CADR(expr)),
				      PP(D(CADDR(expr), var))));
		UNPROTECT(4);
		PROTECT(expr2);
		ans = simplify(PlusSymbol, expr1, expr2);
		UNPROTECT(2);
	    }
	}
	else if (CAR(expr) == ExpSymbol) {
	    ans = simplify(TimesSymbol,
			   expr,
			   PP(D(CADR(expr), var)));
	    UNPROTECT(1);
	}
	else if (CAR(expr) == LogSymbol) {
	    ans = simplify(DivideSymbol,
			   PP(D(CADR(expr), var)),
			   CADR(expr));
	    UNPROTECT(1);
	}
	else if (CAR(expr) == CosSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(SinSymbol, CADR(expr)),
			   PP_S2(MinusSymbol, PP(D(CADR(expr), var))));
	    UNPROTECT(3);
	}
	else if (CAR(expr) == SinSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(CosSymbol, CADR(expr)),
			   PP(D(CADR(expr), var)));
	    UNPROTECT(2);
	}
	else if (CAR(expr) == TanSymbol) {
	    ans = simplify(DivideSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S(PowerSymbol,
				PP_S2(CosSymbol, CADR(expr)),
				PP(Constant(2.0))));
	    UNPROTECT(4);
	}
	else if (CAR(expr) == CoshSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(SinhSymbol, CADR(expr)),
			   PP(D(CADR(expr), var)));
	    UNPROTECT(2);
	}
	else if (CAR(expr) == SinhSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(CoshSymbol, CADR(expr)),
			   PP(D(CADR(expr), var))),
		UNPROTECT(2);
	}
	else if (CAR(expr) == TanhSymbol) {
	    ans = simplify(DivideSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S(PowerSymbol,
				PP_S2(CoshSymbol, CADR(expr)),
				PP(Constant(2.0))));
	    UNPROTECT(4);
	}
	else if (CAR(expr) == SqrtSymbol) {
	    PROTECT(expr1 = allocList(3));
	    SET_TYPEOF(expr1, LANGSXP);
	    SETCAR(expr1, PowerSymbol);
	    SETCADR(expr1, CADR(expr));
	    SETCADDR(expr1, Constant(0.5));
	    ans = D(expr1, var);
	    UNPROTECT(1);
	}
	else if (CAR(expr) == PnormSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(DnormSymbol, CADR(expr)),
			   PP(D(CADR(expr), var)));
	    UNPROTECT(2);
	}
	else if (CAR(expr) == DnormSymbol) {
	    ans = simplify(TimesSymbol,
			   PP_S2(MinusSymbol, CADR(expr)),
			   PP_S(TimesSymbol,
				PP_S2(DnormSymbol, CADR(expr)),
				PP(D(CADR(expr), var))));
	    UNPROTECT(4);
	}
	else if (CAR(expr) == AsinSymbol) {
	    ans = simplify(DivideSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S(SqrtSymbol,
				PP_S(MinusSymbol, Constant(1.),
				     PP_S(PowerSymbol,CADR(expr),Constant(2.))),
				R_MissingArg));
	    UNPROTECT(4);
	}
	else if (CAR(expr) == AcosSymbol) {
	    ans = simplify(MinusSymbol,
			   PP_S(DivideSymbol,
				PP(D(CADR(expr), var)),
				PP_S(SqrtSymbol,
				     PP_S(MinusSymbol,Constant(1.),
					  PP_S(PowerSymbol,
					       CADR(expr),Constant(2.))),
				     R_MissingArg)), R_MissingArg);
	    UNPROTECT(5);
	}
	else if (CAR(expr) == AtanSymbol) {
	    ans = simplify(DivideSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S(PlusSymbol,Constant(1.),
				PP_S(PowerSymbol, CADR(expr),Constant(2.))));
	    UNPROTECT(3);
	}
	else if (CAR(expr) == LGammaSymbol) {
	    ans = simplify(TimesSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S2(PsiSymbol, CADR(expr)));
	    UNPROTECT(2);
	}
	else if (CAR(expr) == GammaSymbol) {
	    ans = simplify(TimesSymbol,
			   PP(D(CADR(expr), var)),
			   PP_S(TimesSymbol,
				expr,
				PP_S2(PsiSymbol, CADR(expr))));
	    UNPROTECT(3);
	}

	else {
	    SEXP u = deparse1(CAR(expr), 0, SIMPLEDEPARSE);
	    error(_("Function '%s' is not in the derivatives table"),
		  translateChar(STRING_ELT(u, 0)));
	}

	break;
    default:
	ans = Constant(NA_REAL);
    }
    return ans;

#undef PP_S

} /* D() */

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
    if (TYPEOF(expr) == LANGSXP) {
	e = CDR(expr);
	while(e != R_NilValue) {
	    SETCAR(e, AddParens(CAR(e)));
	    e = CDR(e);
	}
    }
    if (isPlusForm(expr)) {
	if (isPlusForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    else if (isMinusForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    else if (isTimesForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
	if (isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
    }
    else if (isDivideForm(expr)) {
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
	if (isPlusForm(CADR(expr)) || isMinusForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
    }
    else if (isPowerForm(expr)) {
	if (isPowerForm(CADR(expr))) {
	    SETCADR(expr, lang2(ParenSymbol, CADR(expr)));
	}
	if (isPlusForm(CADDR(expr)) || isMinusForm(CADDR(expr))
	    || isTimesForm(CADDR(expr)) || isDivideForm(CADDR(expr))) {
	    SETCADDR(expr, lang2(ParenSymbol, CADDR(expr)));
	}
    }
    return expr;
}

SEXP attribute_hidden do_D(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP expr, var;
    checkArity(op, args);
    if (isExpression(CAR(args))) expr = VECTOR_ELT(CAR(args), 0);
    else expr = CAR(args);
    var = CADR(args);
    if (!isString(var) || length(var) < 1)
	error(_("variable must be a character string"));
    if (length(var) > 1)
	warning(_("only the first element is used as variable name"));
    var = install(translateChar(STRING_ELT(var, 0)));
    InitDerivSymbols();
    PROTECT(expr = D(expr, var));
    expr = AddParens(expr);
    UNPROTECT(1);
    return expr;
}

/* ------ FindSubexprs ------ and ------ Accumulate ------ */

static void InvalidExpression(char *where)
{
    error(_("invalid expression in \"%s\""), where);
}

static int equal(SEXP expr1, SEXP expr2)
{
    if (TYPEOF(expr1) == TYPEOF(expr2)) {
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

static int Accumulate(SEXP expr, SEXP exprlist)
{
    SEXP e;
    int k;
    e = exprlist;
    k = 0;
    while(CDR(e) != R_NilValue) {
	e = CDR(e);
	k = k + 1;
	if (equal(expr, CAR(e)))
	    return k;
    }
    SETCDR(e, CONS(expr, R_NilValue));
    return k + 1;
}

static int Accumulate2(SEXP expr, SEXP exprlist)
{
    SEXP e;
    int k;
    e = exprlist;
    k = 0;
    while(CDR(e) != R_NilValue) {
	e = CDR(e);
	k = k + 1;
    }
    SETCDR(e, CONS(expr, R_NilValue));
    return k + 1;
}

static SEXP MakeVariable(int k, SEXP tag)
{
    char buf[64];
    snprintf(buf, 64, "%s%d", translateChar(STRING_ELT(tag, 0)), k);
    return install(buf);
}

static int FindSubexprs(SEXP expr, SEXP exprlist, SEXP tag)
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
	if (inherits(expr, "expression"))
	    return FindSubexprs(CAR(expr), exprlist, tag);
	else { InvalidExpression("FindSubexprs"); return -1/*-Wall*/; }
	break;
    case LANGSXP:
	if (CAR(expr) == install("(")) {
	    return FindSubexprs(CADR(expr), exprlist, tag);
	}
	else {
	    e = CDR(expr);
	    while(e != R_NilValue) {
		if ((k = FindSubexprs(CAR(e), exprlist, tag)) != 0)
		    SETCAR(e, MakeVariable(k, tag));
		e = CDR(e);
	    }
	    return Accumulate(expr, exprlist);
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
	if (lst == sym) return expr;
	else return lst;
    case LISTSXP:
    case LANGSXP:
	SETCAR(lst, Replace(sym, expr, CAR(lst)));
	SETCDR(lst, Replace(sym, expr, CDR(lst)));
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
    SETCAR(dimnames, install("list"));
    p = install("c");
    PROTECT(q = allocList(n));
    SETCADDR(dimnames, LCONS(p, q));
    UNPROTECT(1);
    for(i = 0 ; i < n ; i++) {
	SETCAR(q, ScalarString(STRING_ELT(names, i)));
	q = CDR(q);
    }
    PROTECT(dim = lang3(R_NilValue, R_NilValue, R_NilValue));
    SETCAR(dim, install("c"));
    SETCADR(dim, lang2(install("length"), install(".value")));
    SETCADDR(dim, ScalarInteger(length(names))); /* was real? */
    PROTECT(data = ScalarReal(0.));
    PROTECT(p = lang4(install("array"), data, dim, dimnames));
    p = lang3(install("<-"), install(".grad"), p);
    UNPROTECT(4);
    return p;
}

static SEXP CreateHess(SEXP names)
{
    SEXP p, q, data, dim, dimnames;
    int i, n;
    n = length(names);
    PROTECT(dimnames = lang4(R_NilValue, R_NilValue, R_NilValue, R_NilValue));
    SETCAR(dimnames, install("list"));
    p = install("c");
    PROTECT(q = allocList(n));
    SETCADDR(dimnames, LCONS(p, q));
    UNPROTECT(1);
    for(i = 0 ; i < n ; i++) {
	SETCAR(q, ScalarString(STRING_ELT(names, i)));
	q = CDR(q);
    }
    SETCADDDR(dimnames, duplicate(CADDR(dimnames)));
    PROTECT(dim = lang4(R_NilValue, R_NilValue, R_NilValue,R_NilValue));
    SETCAR(dim, install("c"));
    SETCADR(dim, lang2(install("length"), install(".value")));
    SETCADDR(dim, ScalarInteger(length(names)));
    SETCADDDR(dim, ScalarInteger(length(names)));
    PROTECT(data = ScalarReal(0.));
    PROTECT(p = lang4(install("array"), data, dim, dimnames));
    p = lang3(install("<-"), install(".hessian"), p);
    UNPROTECT(4);
    return p;
}

static SEXP DerivAssign(SEXP name, SEXP expr)
{
    SEXP ans, newname;
    PROTECT(ans = lang3(install("<-"), R_NilValue, expr));
    PROTECT(newname = ScalarString(name));
    SETCADR(ans, lang4(install("["), install(".grad"), R_MissingArg, newname));
    UNPROTECT(2);
    return ans;
}

static SEXP lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = LCONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

static SEXP HessAssign1(SEXP name, SEXP expr)
{
    SEXP ans, newname;
    PROTECT(ans = lang3(install("<-"), R_NilValue, expr));
    PROTECT(newname = ScalarString(name));
    SETCADR(ans, lang5(install("["), install(".hessian"), R_MissingArg,
		       newname, newname));
    UNPROTECT(2);
    return ans;
}

static SEXP HessAssign2(SEXP name1, SEXP name2, SEXP expr)
{
    SEXP ans, newname1, newname2;
    PROTECT(newname1 = ScalarString(name1));
    PROTECT(newname2 = ScalarString(name2));
    ans = lang3(install("<-"),
		lang5(install("["), install(".hessian"), R_MissingArg,
		      newname1, newname2),
		lang3(install("<-"),
		      lang5(install("["), install(".hessian"), R_MissingArg,
			    newname2, newname1),
		      expr));
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

static SEXP AddHess()
{
    SEXP ans;
    PROTECT(ans = mkString("hessian"));
    PROTECT(ans = lang3(install("attr"), install(".value"), ans));
    ans = lang3(install("<-"), ans, install(".hessian"));
    UNPROTECT(2);
    return ans;
}

static SEXP Prune(SEXP lst)
{
    if (lst == R_NilValue)
	return lst;
    SETCDR(lst, Prune(CDR(lst)));
    if (CAR(lst) == R_MissingArg)
	return CDR(lst);
    else return lst ;
}

SEXP attribute_hidden do_deriv(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* deriv.default(expr, namevec, function.arg, tag, hessian) */
    SEXP ans, ans2, expr, funarg, names, s;
    int f_index, *d_index, *d2_index;
    int i, j, k, nexpr, nderiv=0, hessian;
    SEXP exprlist, tag;
    checkArity(op, args);
    InitDerivSymbols();
    PROTECT(exprlist = LCONS(install("{"), R_NilValue));
    /* expr: */
    if (isExpression(CAR(args)))
	PROTECT(expr = VECTOR_ELT(CAR(args), 0));
    else PROTECT(expr = CAR(args));
    args = CDR(args);
    /* namevec: */
    names = CAR(args);
    if (!isString(names) || (nderiv = length(names)) < 1)
	error(_("invalid variable names"));
    args = CDR(args);
    /* function.arg: */
    funarg = CAR(args);
    args = CDR(args);
    /* tag: */
    tag = CAR(args);
    if (!isString(tag) || length(tag) < 1
	|| length(STRING_ELT(tag, 0)) < 1 || length(STRING_ELT(tag, 0)) > 60)
	error(_("invalid tag"));
    args = CDR(args);
    /* hessian: */
    hessian = asLogical(CAR(args));
    /* NOTE: FindSubexprs is destructive, hence the duplication */
    PROTECT(ans = duplicate(expr));
    f_index = FindSubexprs(ans, exprlist, tag);
    d_index = (int*)R_alloc(nderiv, sizeof(int));
    if (hessian)
	d2_index = (int*)R_alloc((nderiv * (1 + nderiv))/2, sizeof(int));
    else d2_index = d_index;/*-Wall*/
    UNPROTECT(1);
    for(i=0, k=0; i<nderiv ; i++) {
	PROTECT(ans = duplicate(expr));
	PROTECT(ans = D(ans, install(translateChar(STRING_ELT(names, i)))));
	ans2 = duplicate(ans);	/* keep a temporary copy */
	d_index[i] = FindSubexprs(ans, exprlist, tag); /* examine the derivative first */
	ans = duplicate(ans2);	/* restore the copy */
	if (hessian) {
	    for(j = i; j < nderiv; j++) {
		PROTECT(ans2 = duplicate(ans));
		PROTECT(ans2 = D(ans2, install(translateChar(STRING_ELT(names, j)))));
		d2_index[k] = FindSubexprs(ans2, exprlist, tag);
		k++;
		UNPROTECT(2);
	    }
	}
	UNPROTECT(2);
    }
    nexpr = length(exprlist) - 1;
    if (f_index) {
	Accumulate2(MakeVariable(f_index, tag), exprlist);
    }
    else {
	PROTECT(ans = duplicate(expr));
	Accumulate2(expr, exprlist);
	UNPROTECT(1);
    }
    Accumulate2(R_NilValue, exprlist);
    if (hessian) { Accumulate2(R_NilValue, exprlist); }
    for (i = 0, k = 0; i < nderiv ; i++) {
	if (d_index[i]) {
	    Accumulate2(MakeVariable(d_index[i], tag), exprlist);
	    if (hessian) {
		PROTECT(ans = duplicate(expr));
		PROTECT(ans = D(ans, install(translateChar(STRING_ELT(names, i)))));
		for (j = i; j < nderiv; j++) {
		    if (d2_index[k]) {
			Accumulate2(MakeVariable(d2_index[k], tag), exprlist);
		    } else {
			PROTECT(ans2 = duplicate(ans));
			PROTECT(ans2 = D(ans2, install(translateChar(STRING_ELT(names, j)))));
			Accumulate2(ans2, exprlist);
			UNPROTECT(2);
		    }
		    k++;
		}
		UNPROTECT(2);
	    }
	} else { /* the first derivative is constant or simple variable */
	    PROTECT(ans = duplicate(expr));
	    PROTECT(ans = D(ans, install(translateChar(STRING_ELT(names, i)))));
	    Accumulate2(ans, exprlist);
	    UNPROTECT(2);
	    if (hessian) {
		for (j = i; j < nderiv; j++) {
		    if (d2_index[k]) {
			Accumulate2(MakeVariable(d2_index[k], tag), exprlist);
		    } else {
			PROTECT(ans2 = duplicate(ans));
			PROTECT(ans2 = D(ans2, install(translateChar(STRING_ELT(names, j)))));
			if(isZero(ans2)) Accumulate2(R_MissingArg, exprlist);
			else Accumulate2(ans2, exprlist);
			UNPROTECT(2);
		    }
		    k++;
		}
	    }
	}
    }
    Accumulate2(R_NilValue, exprlist);
    Accumulate2(R_NilValue, exprlist);
    if (hessian) { Accumulate2(R_NilValue, exprlist); }

    i = 0;
    ans = CDR(exprlist);
    while (i < nexpr) {
	if (CountOccurrences(MakeVariable(i+1, tag), CDR(ans)) < 2) {
	    SETCDR(ans, Replace(MakeVariable(i+1, tag), CAR(ans), CDR(ans)));
	    SETCAR(ans, R_MissingArg);
	}
	else SETCAR(ans, lang3(install("<-"), MakeVariable(i+1, tag), AddParens(CAR(ans))));
	i = i + 1;
	ans = CDR(ans);
    }
    /* .value <- ... */
    SETCAR(ans, lang3(install("<-"), install(".value"), AddParens(CAR(ans))));
    ans = CDR(ans);
    /* .grad <- ... */
    SETCAR(ans, CreateGrad(names));
    ans = CDR(ans);
    /* .hessian <- ... */
    if (hessian) { SETCAR(ans, CreateHess(names)); ans = CDR(ans); }
    /* .grad[, "..."] <- ... */
    for (i = 0; i < nderiv ; i++) {
	SETCAR(ans, DerivAssign(STRING_ELT(names, i), AddParens(CAR(ans))));
	ans = CDR(ans);
	if (hessian) {
	    for (j = i; j < nderiv; j++) {
		if (CAR(ans) != R_MissingArg) {
		    if (i == j) {
			SETCAR(ans, HessAssign1(STRING_ELT(names, i),
						AddParens(CAR(ans))));
		    } else {
			SETCAR(ans, HessAssign2(STRING_ELT(names, i),
						STRING_ELT(names, j),
						AddParens(CAR(ans))));
		    }
		}
		ans = CDR(ans);
	    }
	}
    }
    /* attr(.value, "gradient") <- .grad */
    SETCAR(ans, AddGrad());
    ans = CDR(ans);
    if (hessian) { SETCAR(ans, AddHess()); ans = CDR(ans); }
    /* .value */
    SETCAR(ans, install(".value"));
    /* Prune the expression list removing eliminated sub-expressions */
    SETCDR(exprlist, Prune(CDR(exprlist)));

    if (TYPEOF(funarg) == LGLSXP && LOGICAL(funarg)[0]) { /* fun = TRUE */
	funarg = names;
    }

    if (TYPEOF(funarg) == CLOSXP)
    {
	s = allocSExp(CLOSXP);
	SET_FORMALS(s, FORMALS(funarg));
	SET_CLOENV(s, CLOENV(funarg));
	funarg = s;
	SET_BODY(funarg, exprlist);
    }
    else if (isString(funarg)) {
	PROTECT(names = duplicate(funarg));
	PROTECT(funarg = allocSExp(CLOSXP));
	PROTECT(ans = allocList(length(names)));
	SET_FORMALS(funarg, ans);
	for(i = 0; i < length(names); i++) {
	    SET_TAG(ans, install(translateChar(STRING_ELT(names, i))));
	    SETCAR(ans, R_MissingArg);
	    ans = CDR(ans);
	}
	UNPROTECT(3);
	SET_BODY(funarg, exprlist);
	SET_CLOENV(funarg, R_GlobalEnv);
    }
    else {
	funarg = allocVector(EXPRSXP, 1);
	SET_VECTOR_ELT(funarg, 0, exprlist);
	/* funarg = lang2(install("expression"), exprlist); */
    }
    UNPROTECT(2);
    return funarg;
}
