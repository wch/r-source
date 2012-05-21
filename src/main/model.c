/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
 *
 *
 *  Model Formula Manipulation
 *
 *  Can you say ``recurse your brains out'';
 *  I knew you could. -- Mr Ro(ss)gers
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

/* inline-able versions */
static R_INLINE Rboolean isUnordered_int(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && !inherits(s, "ordered"));
}

static R_INLINE Rboolean isOrdered_int(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && inherits(s, "ordered"));
}



#define WORDSIZE (8*sizeof(int))

static SEXP tildeSymbol = NULL;
static SEXP plusSymbol  = NULL;
static SEXP minusSymbol = NULL;
static SEXP timesSymbol = NULL;
static SEXP slashSymbol = NULL;
static SEXP colonSymbol = NULL;
static SEXP powerSymbol = NULL;
static SEXP dotSymbol   = NULL;
static SEXP parenSymbol = NULL;
static SEXP inSymbol    = NULL;


static int intercept;		/* intercept term in the model */
static int parity;		/* +/- parity */
static int response;		/* response term in the model */
static int nvar;		/* Number of variables in the formula */
static int nwords;		/* # of words (ints) to code a term */
static int nterm;		/* # of model terms */
static SEXP varlist;		/* variables in the model */
static PROTECT_INDEX vpi;
static SEXP framenames;		/* variables names for specified frame */
static Rboolean haveDot;	/* does RHS of formula contain `.'? */

static int isZeroOne(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return (asReal(x) == 0.0 || asReal(x) == 1.0);
}

static int isZero(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return asReal(x) == 0.0;
}

static int isOne(SEXP x)
{
    if (!isNumeric(x)) return 0;
    return asReal(x) == 1.0;
}


/* MatchVar determines whether two ``variables'' are */
/* identical.  Expressions are identical if they have */
/* the same list structure and their atoms are identical. */
/* This is just EQUAL from lisp. */

static int MatchVar(SEXP var1, SEXP var2)
{
    /* For expedience, and sanity... */
    if ( var1 == var2 )
	return 1;
    /* Handle Nulls */
    if (isNull(var1) && isNull(var2))
	return 1;
    if (isNull(var1) || isNull(var2))
	return 0;
    /* Non-atomic objects - compare CARs & CDRs */
    if ((isList(var1) || isLanguage(var1)) &&
	(isList(var2) || isLanguage(var2)))
	return MatchVar(CAR(var1), CAR(var2)) &&
	       MatchVar(CDR(var1), CDR(var2));
    /* Symbols */
    if (isSymbol(var1) && isSymbol(var2))
	return (var1 == var2);
    /* Literal Numerics */
    if (isNumeric(var1) && isNumeric(var2))
	return (asReal(var1) == asReal(var2));
    /* Literal Strings */
    if (isString(var1) && isString(var2))
	return Seql(STRING_ELT(var1, 0), STRING_ELT(var2, 0));
    /* Nothing else matches */
    return 0;
}


/* InstallVar locates a ``variable'' in the model variable list;
   adding it to the global varlist if not found. */

static int InstallVar(SEXP var)
{
    SEXP v;
    int indx;
    /* Check that variable is legitimate */
    if (!isSymbol(var) && !isLanguage(var) && !isZeroOne(var))
	error(_("invalid term in model formula"));
    /* Lookup/Install it */
    indx = 0;
    for (v = varlist; CDR(v) != R_NilValue; v = CDR(v)) {
	indx++;
	if (MatchVar(var, CADR(v)))
	    return indx;
    }
    SETCDR(v, CONS(var, R_NilValue));
    return indx + 1;
}


/* If there is a dotsxp being expanded then we need to see
   whether any of the variables in the data frame match with
   the variable on the lhs. If so they shouldn't be included
   in the factors */

static void CheckRHS(SEXP v)
{
    int i, j;
    SEXP s, t;
    while ((isList(v) || isLanguage(v)) && v != R_NilValue) {
	CheckRHS(CAR(v));
	v = CDR(v);
    }
    if (isSymbol(v)) {
	for (i = 0; i < length(framenames); i++) {
	    s = install(translateChar(STRING_ELT(framenames, i)));
	    if (v == s) {
		t = allocVector(STRSXP, length(framenames) - 1);
		for (j = 0; j < length(t); j++) {
		    if (j < i)
			SET_STRING_ELT(t, j, STRING_ELT(framenames, j));
		    else
			SET_STRING_ELT(t, j, STRING_ELT(framenames, j+1));
		}
		REPROTECT(framenames = t, vpi);
	    }
	}
    }
}


/* ExtractVars recursively extracts the variables
   in a model formula.  It calls InstallVar to do
   the installation.  The code takes care of unary/
   + and minus.  No checks are made of the other
   ``binary'' operators.  Maybe there should be some. */

static void ExtractVars(SEXP formula, int checkonly)
{
    int len, i;
    SEXP v;

    if (isNull(formula) || isZeroOne(formula))
	return;
    if (isSymbol(formula)) {
	if (formula == dotSymbol) haveDot = TRUE;
	if (!checkonly) {
	    if (formula == dotSymbol && framenames != R_NilValue) {
		haveDot = TRUE;
		for (i = 0; i < length(framenames); i++) {
		    v = install(translateChar(STRING_ELT(framenames, i)));
		    if (!MatchVar(v, CADR(varlist))) InstallVar(v);
		}
	    } else
		InstallVar(formula);
	}
	return;
    }
    if (isLanguage(formula)) {
	len = length(formula);
	if (CAR(formula) == tildeSymbol) {
	    if (response)
		error(_("invalid model formula"));
	    if (isNull(CDDR(formula))) {
		response = 0;
		ExtractVars(CADR(formula), 0);
	    }
	    else {
		response = 1;
		InstallVar(CADR(formula));
		ExtractVars(CADDR(formula), 0);
	    }
	    return;
	}
	if (CAR(formula) == plusSymbol) {
	    if (length(formula) > 1)
		ExtractVars(CADR(formula), checkonly);
	    if (length(formula) > 2)
		ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == colonSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == powerSymbol) {
	    if (!isNumeric(CADDR(formula)))
		error(_("invalid power in formula"));
	    ExtractVars(CADR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == timesSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == inSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == slashSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    ExtractVars(CADDR(formula), checkonly);
	    return;
	}
	if (CAR(formula) == minusSymbol) {
	    if (len == 2) {
		ExtractVars(CADR(formula), 1);
	    }
	    else {
		ExtractVars(CADR(formula), checkonly);
		ExtractVars(CADDR(formula), 1);
	    }
	    return;
	}
	if (CAR(formula) == parenSymbol) {
	    ExtractVars(CADR(formula), checkonly);
	    return;
	}
	InstallVar(formula);
	return;
    }
    error(_("invalid model formula in ExtractVars"));
}


/* AllocTerm allocates an integer array for
   bit string representation of a model term */

static SEXP AllocTerm(void)
{
    int i;
    SEXP term = allocVector(INTSXP, nwords);
    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = 0;
    return term;
}


/* SetBit sets bit ``whichBit'' to value ``value''
   in the bit string representation of a term. */

static void SetBit(SEXP term, int whichBit, int value)
{
    int word, offset;
    word = (int)((whichBit - 1) / WORDSIZE);
    offset = (WORDSIZE - whichBit) % WORDSIZE;
    if (value)
	((unsigned *) INTEGER(term))[word] |= ((unsigned) 1 << offset);
    else
	((unsigned *) INTEGER(term))[word] &= ~((unsigned) 1 << offset);
}


/* GetBit gets bit ``whichBit'' from the */
/* bit string representation of a term. */

static int GetBit(SEXP term, int whichBit)
{
    unsigned int word, offset;
    word = (int)((whichBit - 1) / WORDSIZE);
    offset = (WORDSIZE - whichBit) % WORDSIZE;
    return ((((unsigned *) INTEGER(term))[word]) >> offset) & 1;
}


/* OrBits computes a new (bit string) term */
/* which contains the logical OR of the bits */
/* in ``term1'' and ``term2''. */

static SEXP OrBits(SEXP term1, SEXP term2)
{
    SEXP term;
    int i;
    term = AllocTerm();
    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = INTEGER(term1)[i] | INTEGER(term2)[i];
    return term;
}


/* BitCount counts the number of ``on'' */
/* bits in a term */

static int BitCount(SEXP term)
{
    int i, sum;
    sum = 0;
    for (i = 1; i <= nvar; i++)
	sum += GetBit(term, i);
    return sum;
}


/* TermZero tests whether a (bit string) term is zero */

static int TermZero(SEXP term)
{
    int i, val;
    val = 1;
    for (i = 0; i < nwords; i++)
	val = val && (INTEGER(term)[i] == 0);
    return val;
}


/* TermEqual tests two (bit string) terms for equality. */

static int TermEqual(SEXP term1, SEXP term2)
{
    int i, val;
    val = 1;
    for (i = 0; i < nwords; i++)
	val = val && (INTEGER(term1)[i] == INTEGER(term2)[i]);
    return val;
}


/* StripTerm strips the specified term from */
/* the given list.  This mutates the list. */

static SEXP StripTerm(SEXP term, SEXP list)
{
    SEXP tail;
    if (TermZero(term))
	intercept = 0;
    if (list == R_NilValue)
	return list;
    /* This can be highly recursive */
    R_CheckStack();
    tail = StripTerm(term, CDR(list));
    if (TermEqual(term, CAR(list)))
	return tail;
    SETCDR(list, tail);
    return list;
}


/* TrimRepeats removes duplicates of (bit string) terms 
   in a model formula by repeated use of ``StripTerm''.
   Also drops zero terms. */

static SEXP TrimRepeats(SEXP list)
{
    /* Highly recursive, but StripTerm does the checking */
    if (list == R_NilValue)
	return R_NilValue;
    if (TermZero(CAR(list)))
	return TrimRepeats(CDR(list));
    SETCDR(list, TrimRepeats(StripTerm(CAR(list), CDR(list))));
    return list;
}



/*==========================================================================*/

/* Model Formula Manipulation */
/* These functions take a numerically coded */
/* formula and fully expand it. */

static SEXP EncodeVars(SEXP);/* defined below */


/* PlusTerms expands ``left'' and ``right'' and */
/* concatenates their terms (removing duplicates). */

static SEXP PlusTerms(SEXP left, SEXP right)
{
    PROTECT(left = EncodeVars(left));
    right = EncodeVars(right);
    UNPROTECT(1);
    return TrimRepeats(listAppend(left, right));
}


/* InteractTerms expands ``left'' and ``right'' */
/* and forms a new list of terms containing the bitwise */
/* OR of each term in ``left'' with each term in ``right''. */

static SEXP InteractTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = allocList(length(left) * length(right)));
    t = term;
    for (l = left; l != R_NilValue; l = CDR(l))
	for (r = right; r != R_NilValue; r = CDR(r)) {
	    SETCAR(t, OrBits(CAR(l), CAR(r)));
	    t = CDR(t);
	}
    UNPROTECT(3);
    return TrimRepeats(term);
}


/* CrossTerms expands ``left'' and ``right'' */
/* and forms the ``cross'' of the list of terms.  */
/* Duplicates are removed. */

static SEXP CrossTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = allocList(length(left) * length(right)));
    t = term;
    for (l = left; l != R_NilValue; l = CDR(l))
	for (r = right; r != R_NilValue; r = CDR(r)) {
	    SETCAR(t, OrBits(CAR(l), CAR(r)));
	    t = CDR(t);
	}
    UNPROTECT(3);
    listAppend(right, term);
    listAppend(left, right);
    return TrimRepeats(left);
}


/* PowerTerms expands the ``left'' form and then */
/* raises it to the power specified by the right term. */
/* Allocation here is wasteful, but so what ... */

static SEXP PowerTerms(SEXP left, SEXP right)
{
    SEXP term, l, r, t;
    int i, ip;
    ip = asInteger(right);
    if (ip==NA_INTEGER || ip <= 1)
	error(_("invalid power in formula"));
    term = R_NilValue;		/* -Wall */
    PROTECT(left = EncodeVars(left));
    right = left;
    for (i=1; i < ip; i++)  {
	PROTECT(right);
	PROTECT(term = allocList(length(left) * length(right)));
	t = term;
	for (l = left; l != R_NilValue; l = CDR(l))
	    for (r = right; r != R_NilValue; r = CDR(r)) {
		SETCAR(t, OrBits(CAR(l), CAR(r)));
		t = CDR(t);
	    }
	UNPROTECT(2);
	right = TrimRepeats(term);
    }
    UNPROTECT(1);
    return term;
}


/* InTerms expands ``left'' and ``right'' and */
/* forms the ``nest'' of the the left in the */
/* interaction of the right */

static SEXP InTerms(SEXP left, SEXP right)
{
    SEXP term, t;
    int i;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = AllocTerm());
    /* Bitwise or of all terms on right */
    for (t = right; t != R_NilValue; t = CDR(t)) {
	for (i = 0; i < nwords; i++)
	    INTEGER(term)[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    }
    /* Now bitwise or with each term on the left */
    for (t = left; t != R_NilValue; t = CDR(t))
	for (i = 0; i < nwords; i++)
	    INTEGER(CAR(t))[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    UNPROTECT(3);
    return TrimRepeats(left);
}

/* NestTerms expands ``left'' and ``right'' */
/* and forms the ``nest'' of the list of terms.  */
/* Duplicates are removed. */

static SEXP NestTerms(SEXP left, SEXP right)
{
    SEXP term, t;
    int i;
    PROTECT(left = EncodeVars(left));
    PROTECT(right = EncodeVars(right));
    PROTECT(term = AllocTerm());
    /* Bitwise or of all terms on left */
    for (t = left; t != R_NilValue; t = CDR(t)) {
	for (i = 0; i < nwords; i++)
	    INTEGER(term)[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    }
    /* Now bitwise or with each term on the right */
    for (t = right; t != R_NilValue; t = CDR(t))
	for (i = 0; i < nwords; i++)
	    INTEGER(CAR(t))[i] = INTEGER(term)[i] | INTEGER(CAR(t))[i];
    UNPROTECT(3);
    listAppend(left, right);
    return TrimRepeats(left);
}


/* DeleteTerms expands ``left'' and ``right'' */
/* and then removes any terms which appear in */
/* ``right'' from ``left''. */

static SEXP DeleteTerms(SEXP left, SEXP right)
{
    SEXP t;
    PROTECT(left = EncodeVars(left));
    parity = 1-parity;
    PROTECT(right = EncodeVars(right));
    parity = 1-parity;
    for (t = right; t != R_NilValue; t = CDR(t))
	left = StripTerm(CAR(t), left);
    UNPROTECT(2);
    return left;
}


/* EncodeVars performs  model expansion and bit string encoding. */
/* This is the real workhorse of model expansion. */

static SEXP EncodeVars(SEXP formula)
{
    SEXP term;
    int len;

    if (isNull(formula))
	return R_NilValue;

    if (isOne(formula)) {
	if (parity) intercept = 1;
	else intercept = 0;
	return R_NilValue;
    }
    else if (isZero(formula)) {
	if (parity) intercept = 0;
	else intercept = 1;
	return R_NilValue;
    }
    if (isSymbol(formula)) {
	if (formula == dotSymbol && framenames != R_NilValue) {
	    /* prior to 1.7.0 this made term.labels in reverse order. */
	    SEXP r = R_NilValue, v = R_NilValue; /* -Wall */
	    int i, j; const char *c;

	    if (!LENGTH(framenames)) return r;
	    for (i = 0; i < LENGTH(framenames); i++) {
		/* change in 1.6.0 do not use duplicated names */
		c = translateChar(STRING_ELT(framenames, i));
		for(j = 0; j < i; j++)
		    if(!strcmp(c, translateChar(STRING_ELT(framenames, j))))
			error(_("duplicated name '%s' in data frame using '.'"),
			      c);
		term = AllocTerm();
		SetBit(term, InstallVar(install(c)), 1);
		if(i == 0) PROTECT(v = r = cons(term, R_NilValue));
		else {SETCDR(v, CONS(term, R_NilValue)); v = CDR(v);}
	    }
	    UNPROTECT(1);
	    return r;
	}
	else {
	    term = AllocTerm();
	    SetBit(term, InstallVar(formula), 1);
	    return CONS(term, R_NilValue);
	}
    }
    if (isLanguage(formula)) {
	len = length(formula);
	if (CAR(formula) == tildeSymbol) {
	    if (isNull(CDDR(formula)))
		return EncodeVars(CADR(formula));
	    else
		return EncodeVars(CADDR(formula));
	}
	if (CAR(formula) == plusSymbol) {
	    if (len == 2)
		return EncodeVars(CADR(formula));
	    else
		return PlusTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == colonSymbol) {
	    return InteractTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == timesSymbol) {
	    return CrossTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == inSymbol) {
	    return InTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == slashSymbol) {
	    return NestTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == powerSymbol) {
	    return PowerTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == minusSymbol) {
	    if (len == 2)
		return DeleteTerms(R_NilValue, CADR(formula));
	    return DeleteTerms(CADR(formula), CADDR(formula));
	}
	if (CAR(formula) == parenSymbol) {
	    return EncodeVars(CADR(formula));
	}
	term = AllocTerm();
	SetBit(term, InstallVar(formula), 1);
	return CONS(term, R_NilValue);
    }
    error(_("invalid model formula in EncodeVars"));
    return R_NilValue;/*NOTREACHED*/
}


/* TermCode decides on the encoding of a model term. */
/* Returns 1 if variable ``whichBit'' in ``thisTerm'' */
/* is to be encoded by contrasts and 2 if it is to be */
/* encoded by dummy variables.  This is decided using */
/* the heuristic described in Statistical Models in S, page 38. */

static int TermCode(SEXP termlist, SEXP thisterm, int whichbit, SEXP term)
{
    SEXP t;
    int allzero, i;

    for (i = 0; i < nwords; i++)
	INTEGER(term)[i] = INTEGER(CAR(thisterm))[i];

    /* Eliminate factor ``whichbit'' */

    SetBit(term, whichbit, 0);

    /* Search preceding terms for a match */
    /* Zero is a possibility - it is a special case */

    allzero = 1;
    for (i = 0; i < nwords; i++) {
	if (INTEGER(term)[i]) {
	    allzero = 0;
	    break;
	}
    }
    if (allzero)
	return 1;

    for (t = termlist; t != thisterm; t = CDR(t)) {
	allzero = 1;
	for (i = 0; i < nwords; i++) {
	    if ((~(INTEGER(CAR(t))[i])) & INTEGER(term)[i])
		allzero = 0;
	}
	if (allzero)
	    return 1;
    }
    return 2;
}


/* Internal code for the ``terms'' function */
/* The value is a formula with an assortment */
/* of useful attributes. */

/* .Internal(terms.formula(x, new.specials, abb, data, keep.order)) */

static SEXP ExpandDots(SEXP object, SEXP value);

SEXP attribute_hidden do_termsform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, v, pattern, formula, varnames, term, termlabs, ord;
    SEXP specials, t, data, rhs;
    int i, j, k, l, n, keepOrder, allowDot;

    Rboolean hadFrameNames = FALSE;

    checkArity(op, args);

    /* Always fetch these values rather than trying to remember them
       between calls.  The overhead is minimal. */

    tildeSymbol = install("~");
    plusSymbol  = install("+");
    minusSymbol = install("-");
    timesSymbol = install("*");
    slashSymbol = install("/");
    colonSymbol = install(":");
    powerSymbol = install("^");
    dotSymbol   = install(".");
    parenSymbol = install("(");
    inSymbol = install("%in%");

    /* Do we have a model formula? */
    /* Check for unary or binary ~ */

    if (!isLanguage(CAR(args)) ||
	CAR(CAR(args)) != tildeSymbol ||
	(length(CAR(args)) != 2 && length(CAR(args)) != 3))
	error(_("argument is not a valid model"));

    haveDot = FALSE;

    PROTECT(ans = duplicate(CAR(args)));

    /* The formula will be returned, modified if haveDot becomes TRUE */

    specials = CADR(args);
    if(length(specials) && !isString(specials))
	error(_("'specials' must be NULL or a character vector"));
    a = CDDR(args);

    /* We use data to get the value to substitute for "." in formulae */

    data = CAR(a);
    a = CDR(a);
    if (isNull(data) || isEnvironment(data))
	framenames = R_NilValue;
    else if (isFrame(data))
	framenames = getAttrib(data, R_NamesSymbol);
    else
	error(_("'data' argument is of the wrong type"));
    PROTECT_WITH_INDEX(framenames, &vpi);

    if (framenames != R_NilValue) {
	if(length(framenames)) hadFrameNames = TRUE;
	if (length(CAR(args)) == 3)
	    CheckRHS(CADR(CAR(args)));
    }

    /* Preserve term order? */

    keepOrder = asLogical(CAR(a));
    if (keepOrder == NA_LOGICAL)
	keepOrder = 0;

    a = CDR(a);
    allowDot = asLogical(CAR(a));
    if (allowDot == NA_LOGICAL) allowDot = 0;

    if (specials == R_NilValue) {
	a = allocList(8);
	SET_ATTRIB(ans, a);
    }
    else {
	a = allocList(9);
	SET_ATTRIB(ans, a);
    }

    /* Step 1: Determine the ``variables'' in the model */
    /* Here we create an expression of the form */
    /* list(...).  You can evaluate it to get */
    /* the model variables or use substitute and then */
    /* pull the result apart to get the variable names. */

    intercept = 1;
    parity = 1;
    response = 0;
    PROTECT(varlist = LCONS(install("list"), R_NilValue));
    ExtractVars(CAR(args), 1);
    UNPROTECT(1);
    SETCAR(a, varlist);
    SET_TAG(a, install("variables"));
    a = CDR(a);

    nvar = length(varlist) - 1;
    nwords = (int)((nvar - 1) / WORDSIZE + 1);

    /* Step 2: Recode the model terms in binary form */
    /* and at the same time, expand the model formula. */

    /* FIXME: this includes specials in the model */
    /* There perhaps needs to be a an extra pass */
    /* through the model to delete any terms which */
    /* contain specials.  Actually, specials should */
    /* only enter additively so this should also be */
    /* checked and abort forced if not. */

    /* BDR 2002-01-29: S does include specials, so code may rely on this */

    /* FIXME: this is also the point where nesting */
    /* needs to be taken care of. */

    PROTECT(formula = EncodeVars(CAR(args)));

    nvar = length(varlist) - 1; /* need to recompute, in case
				   EncodeVars stretched it */

    /* Step 2a: Compute variable names */

    PROTECT(varnames = allocVector(STRSXP, nvar));
    for (v = CDR(varlist), i = 0; v != R_NilValue; v = CDR(v))
	SET_STRING_ELT(varnames, i++, STRING_ELT(deparse1line(CAR(v), 0), 0));

    /* Step 2b: Find and remove any offset(s) */

    /* first see if any of the variables are offsets */
    for (l = response, k = 0; l < nvar; l++)
	if (!strncmp(CHAR(STRING_ELT(varnames, l)), "offset(", 7)) k++;
    if (k > 0) {
	Rboolean foundOne = FALSE; /* has there been a non-offset term? */
	/* allocate the "offsets" attribute */
	SETCAR(a, v = allocVector(INTSXP, k));
	for (l = response, k = 0; l < nvar; l++)
	    if (!strncmp(CHAR(STRING_ELT(varnames, l)), "offset(", 7))
		INTEGER(v)[k++] = l + 1;
	SET_TAG(a, install("offset"));
	a = CDR(a);
	/* now remove offset terms from the formula */
	call = formula; /* call is to be the previous term once one is found */
	while (1) {
	    SEXP thisterm = foundOne ? CDR(call) : call;
	    Rboolean have_offset = FALSE;
	    if(length(thisterm) == 0) break;
	    for (i = 1; i <= nvar; i++)
		if (GetBit(CAR(thisterm), i) &&
		    !strncmp(CHAR(STRING_ELT(varnames, i-1)), "offset(", 7)) {
		    have_offset = TRUE;
		    break;
		}
	    if (have_offset) {
		if (!foundOne) call = formula = CDR(formula);
		else SETCDR(call, CDR(thisterm));
	    } else {
		if (foundOne) call = CDR(call);
		foundOne = TRUE;
	    }
	}
    }
    nterm = length(formula);

    /* Step 3: Reorder the model terms by BitCount, otherwise
       preserving their order. */

    PROTECT(ord = allocVector(INTSXP, nterm));
    {
	SEXP sCounts;
	int *counts, bitmax = 0, *iord = INTEGER(ord), m = 0;

	PROTECT(pattern = allocVector(VECSXP, nterm));
	PROTECT(sCounts = allocVector(INTSXP, nterm));
	counts = INTEGER(sCounts);
	for (call = formula, n = 0; call != R_NilValue; call = CDR(call)) {
	    SET_VECTOR_ELT(pattern, n, CAR(call));
	    counts[n++] = BitCount(CAR(call));
	}
	for (n = 0; n < nterm; n++)
	    if(counts[n] > bitmax) bitmax = counts[n];
	if(keepOrder) {
	    for (n = 0; n < nterm; n++)
		iord[n] = counts[n];
	} else {
	    call = formula;
	    m = 0;
	    for (i = 0; i <= bitmax; i++) /* can order 0 occur? */
		for (n = 0; n < nterm; n++)
		    if (counts[n] == i) {
			SETCAR(call, VECTOR_ELT(pattern, n));
			call = CDR(call);
			iord[m++] = i;
		    }
	}
	UNPROTECT(2);
    }


    /* Step 4: Compute the factor pattern for the model. */
    /* 0 - the variable does not appear in this term. */
    /* 1 - code the variable by contrasts in this term. */
    /* 2 - code the variable by indicators in this term. */

    if (nterm > 0) {
	SETCAR(a, pattern = allocMatrix(INTSXP, nvar, nterm));
	SET_TAG(a, install("factors"));
	a = CDR(a);
	for (i = 0; i < nterm * nvar; i++)
	    INTEGER(pattern)[i] = 0;
	PROTECT(term = AllocTerm());
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call)) {
	    for (i = 1; i <= nvar; i++) {
		if (GetBit(CAR(call), i))
		    INTEGER(pattern)[i-1+n*nvar] =
			TermCode(formula, call, i, term);
	    }
	    n++;
	}
	UNPROTECT(1);
    }
    else {
	SETCAR(a, pattern = allocVector(INTSXP,0));
	SET_TAG(a, install("factors"));
	a = CDR(a);
    }

    /* Step 5: Compute term labels */

    PROTECT(termlabs = allocVector(STRSXP, nterm));
    n = 0;
    for (call = formula; call != R_NilValue; call = CDR(call)) {
	l = 0;
	for (i = 1; i <= nvar; i++) {
	    if (GetBit(CAR(call), i)) {
		if (l > 0)
		    l += 1;
		l += (int) strlen(CHAR(STRING_ELT(varnames, i - 1)));
	    }
	}
	char cbuf[l+1];
	cbuf[0] = '\0';
	l = 0;
	for (i = 1; i <= nvar; i++) {
	    if (GetBit(CAR(call), i)) {
		if (l > 0)
		    strcat(cbuf, ":");
		strcat(cbuf, CHAR(STRING_ELT(varnames, i - 1)));
		l++;
	    }
	}
	SET_STRING_ELT(termlabs, n, mkChar(cbuf));
	n++;
    }
    PROTECT(v = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(v, 0, varnames);
    SET_VECTOR_ELT(v, 1, termlabs);
    if (nterm > 0)
	setAttrib(pattern, R_DimNamesSymbol, v);

    SETCAR(a, termlabs);
    SET_TAG(a, install("term.labels"));
    a = CDR(a);

    /* If there are specials stick them in here */

    if (specials != R_NilValue) {
	i = length(specials);
	PROTECT(v = allocList(i));
	for (j = 0, t = v; j < i; j++, t = CDR(t)) {
	    const char *ss = translateChar(STRING_ELT(specials, j));
	    SET_TAG(t, install(ss));
	    n = (int) strlen(ss);
	    SETCAR(t, allocVector(INTSXP, 0));
	    k = 0;
	    for (l = 0; l < nvar; l++) {
		if (!strncmp(CHAR(STRING_ELT(varnames, l)), ss, n))
		    if (CHAR(STRING_ELT(varnames, l))[n] == '(')
			k++;
	    }
	    if (k > 0) {
		SETCAR(t, allocVector(INTSXP, k));
		k = 0;
		for (l = 0; l < nvar; l++) {
		    if (!strncmp(CHAR(STRING_ELT(varnames, l)), ss, n))
			if (CHAR(STRING_ELT(varnames, l))[n] == '('){
			    INTEGER(CAR(t))[k++] = l+1;
			}
		}
	    }
	    else SETCAR(t, R_NilValue);
	}
	SETCAR(a, v);
	SET_TAG(a, install("specials"));
	a = CDR(a);
	UNPROTECT(1);
    }

    UNPROTECT(2);	/* keep termlabs until here */

    /* Step 6: Fix up the formula by substituting for dot, which should be
       the framenames joined by + */

    if (haveDot) {
	if(length(framenames)) {
	    PROTECT_INDEX ind;
	    PROTECT_WITH_INDEX(rhs = install(translateChar(STRING_ELT(framenames, 0))),
			       &ind);
	    for (i = 1; i < LENGTH(framenames); i++) {
		REPROTECT(rhs = lang3(plusSymbol, rhs,
				      install(translateChar(STRING_ELT(framenames, i)))),
			  ind);
	    }
	    if (!isNull(CADDR(ans)))
		SETCADDR(ans, ExpandDots(CADDR(ans), rhs));
	    else
		SETCADR(ans, ExpandDots(CADR(ans), rhs));
	    UNPROTECT(1);
	} else if(!allowDot && !hadFrameNames) {
	    error(_("'.' in formula and no 'data' argument"));
	}
    }

    SETCAR(a, allocVector(INTSXP, nterm));
    n = 0;
    {
	int *ia = INTEGER(CAR(a)), *iord = INTEGER(ord);
	for (call = formula; call != R_NilValue; call = CDR(call), n++)
	    ia[n] = iord[n];
    }

    SET_TAG(a, install("order"));
    a = CDR(a);

    SETCAR(a, ScalarInteger(intercept != 0));
    SET_TAG(a, install("intercept"));
    a = CDR(a);

    SETCAR(a, ScalarInteger(response != 0));
    SET_TAG(a, install("response"));
    a = CDR(a);

    SETCAR(a, mkString("terms"));
    SET_TAG(a, install("class"));
    SET_OBJECT(ans, 1);

    SETCDR(a, R_NilValue);  /* truncate if necessary */

    UNPROTECT(5);
    return ans;
}

/* Update a model formula by the replacement of "." templates. */

static SEXP ExpandDots(SEXP object, SEXP value)
{
    SEXP op;

    if (TYPEOF(object) == SYMSXP) {
	if (object == dotSymbol)
	    object = duplicate(value);
	return object;
    }

    if (TYPEOF(object) == LANGSXP) {
	if (TYPEOF(value) == LANGSXP) op = CAR(value);
	else op = NULL;
	PROTECT(object);
	if (CAR(object) == plusSymbol) {
	    if (length(object) == 2) {
		SETCADR(object, ExpandDots(CADR(object), value));
	    }
	    else if (length(object) == 3) {
		SETCADR(object, ExpandDots(CADR(object), value));
		SETCADDR(object, ExpandDots(CADDR(object), value));
	    }
	    else goto badformula;
	}
	else if (CAR(object) == minusSymbol) {
	    if (length(object) == 2) {
		if (CADR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADR(object, lang2(parenSymbol,
					  ExpandDots(CADR(object), value)));
		else
		    SETCADR(object, ExpandDots(CADR(object), value));
	    }
	    else if (length(object) == 3) {
		if (CADR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADR(object, lang2(parenSymbol,
					  ExpandDots(CADR(object), value)));
		else
		    SETCADR(object, ExpandDots(CADR(object), value));
		if (CADDR(object) == dotSymbol &&
		   (op == plusSymbol || op == minusSymbol))
		    SETCADDR(object, lang2(parenSymbol,
					   ExpandDots(CADDR(object), value)));
		else
		    SETCADDR(object, ExpandDots(CADDR(object), value));
	    }
	    else goto badformula;
	}
	else if (CAR(object) == timesSymbol || CAR(object) == slashSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else if (CAR(object) == colonSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol ||
		op == timesSymbol || op == slashSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else if (CAR(object) == powerSymbol) {
	    if (length(object) != 3)
		goto badformula;
	    if (CADR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol ||
		op == timesSymbol || op == slashSymbol ||
		op == colonSymbol))
		SETCADR(object, lang2(parenSymbol,
				      ExpandDots(CADR(object), value)));
	    else
		SETCADR(object, ExpandDots(CADR(object), value));
	    if (CADDR(object) == dotSymbol &&
	       (op == plusSymbol || op == minusSymbol))
		SETCADDR(object, lang2(parenSymbol,
				       ExpandDots(CADDR(object), value)));
	    else
		SETCADDR(object, ExpandDots(CADDR(object), value));
	}
	else {
	    op = object;
	    while(op != R_NilValue) {
		SETCAR(op, ExpandDots(CAR(op), value));
		op = CDR(op);
	    }
	}
	UNPROTECT(1);
	return object;
    }
    else return object;

 badformula:
    error(_("invalid formula in 'update'"));
    return R_NilValue; /*NOTREACHED*/
}


	/* Internal code for the ~ operator */

SEXP attribute_hidden do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (isObject(call))
	return duplicate(call);
    else {
	SEXP klass;
	PROTECT(call = duplicate(call));
	PROTECT(klass = mkString("formula"));
	setAttrib(call, R_ClassSymbol, klass);
	setAttrib(call, R_DotEnvSymbol, rho);
	UNPROTECT(2);
	return call;
    }
}
