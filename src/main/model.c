/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997 Robert Gentleman, Ross Ihaka and the R Core Team
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

	/* Model Formula Manipulation */
	/* Can you say ``recurse your brains out''; */
	/* I knew you could. -- Mr Ro(ss)gers */

#include "Defn.h"

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
static SEXP identSymbol = NULL;


static int intercept;		/* intercept term in the model */
static int parity;		/* +/- parity */
static int response;		/* response term in the model */
static int nvar;		/* Number of variables in the formula */
static int nwords;		/* # of words (ints) to code a term */
static int nterm;		/* # of model terms */
static SEXP varlist;		/* variables in the model */
static SEXP framenames;		/* variables names for specified frame */


static int isZeroOne(SEXP x)
{
	if(!isNumeric(x)) return 0;
	return (asReal(x) == 0.0 || asReal(x) == 1.0);
}

static int isZero(SEXP x)
{
	if(!isNumeric(x)) return 0;
	return asReal(x) == 0.0;
}

static int isOne(SEXP x)
{
	if(!isNumeric(x)) return 0;
	return asReal(x) == 1.0;
}

	/* MatchVar - Determine whether two ``variables'' are */
	/* identical.  Expressions are identical if they have */
	/* the same list structure and their atoms are identical. */
	/* This is just EQUAL from lisp. */

static int MatchVar(SEXP var1, SEXP var2)
{
	/* Handle Nulls */
	if (isNull(var1) && isNull(var2))
		return 1;
	if (isNull(var1) || isNull(var2))
		return 0;

		/* Non-atomic objects - compare CARs & CDRs */

	if ((isList(var1) || isLanguage(var1)) && (isList(var2) || isLanguage(var2)))
		return MatchVar(CAR(var1), CAR(var2)) && MatchVar(CDR(var1), CDR(var2));

		/* Symbols */

	if (isSymbol(var1) && isSymbol(var2))
		return (var1 == var2);

		/* Literal Numerics */

	if (isNumeric(var1) && isNumeric(var2))
		return (asReal(var1) == asReal(var2));

		/* Nothing else matches */

	return 0;
}


	/* InstallVar - Locate a ``variable'' in the model */
	/* variable list.  Add it to the list if not found. */

static int InstallVar(SEXP var)
{
	SEXP v;
	int index;

		/* Check that variable is legitimate */

	if (!isSymbol(var) && !isLanguage(var) && !isZeroOne(var))
		error("invalid term in model formula\n");

		/* Lookup/Install it */

	index = 0;
	for (v = varlist; CDR(v) != R_NilValue; v = CDR(v)) {
		index++;
		if (MatchVar(var, CADR(v)))
			return index;
	}
	CDR(v) = CONS(var, R_NilValue);
	return index + 1;
}

	/* if there is a dotsxp being expanded then we need to see */
	/* whether any of the variables in the data frame match with */
        /* the variable on the lhs. If so they shouldn't be included */
        /* in the factors */

void CheckRHS(SEXP v)
{
	int i, j;
	SEXP s, t;

	while ((isList(v) || isLanguage(v)) && v != R_NilValue ) {
		CheckRHS(CAR(v));
		v = CDR(v);
	}
	if ( isSymbol(v) ) {
		for(i=0 ; i< length(framenames) ; i++ ) {
			s=install(CHAR(STRING(framenames)[i]));
			if( v == s ) {
				t=allocVector(STRSXP, length(framenames)-1);
				for(j=0 ; j< length(t) ; j++) {
					if (j<i)
						STRING(t)[j]=STRING(framenames)[j];
					else
						STRING(t)[j]=STRING(framenames)[j+1];
				}
				framenames=t;
			}
		}
	}
}

	/* ExtractVars - Recursively extract the variables */
	/* in a model formula.  It calls InstallVar to do */
	/* the installation.  The code takes care of unary */
	/* + and minus.  No checks are made of the other */
	/* ``binary'' operators.  Maybe there should be some. */


static void ExtractVars(SEXP formula, int checkonly)
{
	int len, i;
	SEXP v;

	if (isNull(formula) || isZeroOne(formula))
		return;
	if (isSymbol(formula)) {
		if (!checkonly) {
			if( formula == dotSymbol && framenames != R_NilValue )
				for( i=0 ; i<length(framenames) ; i++ ) {
					v=install(CHAR(STRING(framenames)[i]));
					if( !MatchVar(v, CADR(varlist)) )
						InstallVar(install(CHAR(STRING(framenames)[i])));
				}
			else
				InstallVar(formula);
		}
		return;
	}
	if (isLanguage(formula)) {
		len = length(formula);
		if (CAR(formula) == tildeSymbol) {
			if (response)
				error("invalid model formula\n");
			if(isNull(CDDR(formula))) {
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
			if(length(formula) > 1)
				ExtractVars(CADR(formula), checkonly);
			if(length(formula) > 2)
				ExtractVars(CADDR(formula), checkonly);
			return;
		}
		if (CAR(formula) == colonSymbol) {
			ExtractVars(CADR(formula), checkonly);
			ExtractVars(CADDR(formula), checkonly);
			return;
		}
		if (CAR(formula) == powerSymbol) {
			if(!isNumeric(CADDR(formula)))
				error("invalid power in formula\n");
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
			if(len == 2) {
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
	error("invalid model formula\n");
}


	/* AllocTerm - allocate an integer array for */
	/* bit string representation of a model term */

static SEXP AllocTerm()
{
	int i;
	SEXP term = allocVector(INTSXP, nwords);
	for (i = 0; i < nwords; i++)
		INTEGER(term)[i] = 0;
	return term;
}


	/* SetBit - set bit ``whichBit'' to value ``value'' */
	/* in the bit string representation of a term. */

static void SetBit(SEXP term, int whichBit, int value)
{
	int word, offset;
	unsigned tmp;
	word = (whichBit - 1) / WORDSIZE;
	offset = (WORDSIZE - whichBit) % WORDSIZE;
	tmp = ((unsigned *) INTEGER(term))[word];
	if (value)
		((unsigned *) INTEGER(term))[word] |= ((unsigned) 1 << offset);
	else
		((unsigned *) INTEGER(term))[word] &= ~((unsigned) 1 << offset);
	tmp = ((unsigned *) INTEGER(term))[word];
}


	/* GetBit - get bit ``whichBit'' from the */
	/* bit string representation of a term. */

static int GetBit(SEXP term, int whichBit)
{
	unsigned int word, offset;
	word = (whichBit - 1) / WORDSIZE;
	offset = (WORDSIZE - whichBit) % WORDSIZE;
	return ((((unsigned *) INTEGER(term))[word]) >> offset) & 1;
}


	/* OrBits - compute a new (bit string) term */
	/* which contains the logcial Or of the bits */
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

	/* BitCount - count the number of ``on'' */
	/* bits in a term */

static int BitCount(SEXP term)
{
	int i, sum;

	sum = 0;
	for (i=1 ; i <= nvar; i++)
		sum += GetBit(term, i);
	return sum;
}


	/* TermZero - test whether a (bit string) term is zero */

static int TermZero(SEXP term)
{
	int i, val;
	val = 1;
	for (i = 0; i < nwords; i++)
		val = val && (INTEGER(term)[i] == 0);
	return val;
}


	/* TermEqual - test two (bit string) terms for equality. */

static int TermEqual(SEXP term1, SEXP term2)
{
	int i, val;
	val = 1;
	for (i = 0; i < nwords; i++)
		val = val && (INTEGER(term1)[i] == INTEGER(term2)[i]);
	return val;
}


	/* StripTerm - strip the specified term from */
	/* the given list.  This mutates the list. */

static SEXP StripTerm(SEXP term, SEXP list)
{
	SEXP tail;

	if (TermZero(term))
		intercept = 0;
	if (list == R_NilValue)
		return list;
	tail = StripTerm(term, CDR(list));
	if (TermEqual(term, CAR(list)))
		return tail;
	CDR(list) = tail;
	return list;
}


	/* TrimRepeats - removes duplicates of (bit string) terms */
	/* in a model formula by repeated use of ``StripTerm''. */
	/* Also drops zero terms */

static SEXP TrimRepeats(SEXP list)
{
	if (list == R_NilValue)
		return R_NilValue;
	if(TermZero(CAR(list)))
		return TrimRepeats(CDR(list));
	CDR(list) = TrimRepeats(StripTerm(CAR(list), CDR(list)));
	return list;
}

/*==========================================================================*/

	/* Model Formula Manipulation */
	/* These functions take a numerically coded */
	/* formula and fully expand it. */

static SEXP EncodeVars(SEXP);/* defined below */


	/* PlusTerms - expands ``left'' and ``right'' and */
	/* concatenates their terms (removing duplicates). */

static SEXP PlusTerms(SEXP left, SEXP right)
{
	PROTECT(left = EncodeVars(left));
	right = EncodeVars(right);
	UNPROTECT(1);
	return TrimRepeats(listAppend(left, right));
}


	/* InteractTerms - expands ``left'' and ``right'' */
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
			CAR(t) = OrBits(CAR(l), CAR(r));
			t = CDR(t);
		}
	UNPROTECT(3);
	return TrimRepeats(term);
}


	/* CrossTerms - expands ``left'' and ``right'' */
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
			CAR(t) = OrBits(CAR(l), CAR(r));
			t = CDR(t);
		}
	UNPROTECT(3);
	listAppend(right, term);
	listAppend(left, right);
	return TrimRepeats(left);
}


	/* PowerTerms - Expands the ``left'' form and then */
	/* raises it to the power specified by the right term. */
	/* Allocation here is wasteful, but so what ... */

static SEXP PowerTerms(SEXP left, SEXP right)
{
	SEXP term, l, r, t;
	int i, pow;
	pow = asInteger(right);
	if(pow==NA_INTEGER || pow <= 1)
		error("Invalid power in formula\n");
	PROTECT(left = EncodeVars(left));
	right = left;
	for(i=1 ; i<pow ; i++)  {
		PROTECT(right);
		PROTECT(term = allocList(length(left) * length(right)));
		t = term;
		for (l = left; l != R_NilValue; l = CDR(l))
			for (r = right; r != R_NilValue; r = CDR(r)) {
				CAR(t) = OrBits(CAR(l), CAR(r));
				t = CDR(t);
			}
		UNPROTECT(2);
		right = TrimRepeats(term);
	}
	UNPROTECT(1);
	return term;
}


	/* InTerms - expands ``left'' and ``right'' and */
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

	/* NestTerms - expands ``left'' and ``right'' */
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


	/* DeleteTerms - expands ``left'' and ``right'' */
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


	/* EncodeVars - model expansion and bit string encoding. */
	/* This is the real workhorse of model expansion. */

static SEXP EncodeVars(SEXP formula)
{
	SEXP term, r;
	int len, i;

	if (isNull(formula))
		return R_NilValue;

	if (isOne(formula)) {
		if(parity) intercept = 1;
		else intercept = 0;
		return R_NilValue;
	}
	else if(isZero(formula)) {
		if(parity) intercept = 0;
		else intercept = 1;
		return R_NilValue;
	}
	if (isSymbol(formula)) {
		if( formula == dotSymbol && framenames != R_NilValue ) {
			r = R_NilValue;
			for( i=0 ; i< LENGTH(framenames) ; i++) {
				PROTECT(r);
				term = AllocTerm();
				SetBit(term, InstallVar(install(CHAR(STRING(framenames)[i]))), 1);
				r = CONS(term, r);
				UNPROTECT(1);
			}
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
			if(isNull(CDDR(formula)))
				return EncodeVars(CADR(formula));
			else
				return EncodeVars(CADDR(formula));
		}
		if (CAR(formula) == plusSymbol) {
			if(len == 2)
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
			if(len == 2)
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
	error("invalid model formula\n");
	return R_NilValue;/*NOTREACHED*/
}


	/* TermCode - decide on the encoding of a model term. */
	/* Returns 1 if variable ``whichBit'' in ``thisTerm'' */
	/* is to be encoded by contrasts and 2 if it is to be */
	/* encoded by dummy variables.  This is decided using */
	/* the heuristric of Chambers and Heiberger described */
	/* in Statistical Models in S, Page 38. */

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

	/* SortTerms - sort a ``vector'' of terms */

static int TermGT(SEXP s, SEXP t)
{
	unsigned int si, ti;
	int i;
	if(LEVELS(s) > LEVELS(t)) return 1;
	if(LEVELS(s) < LEVELS(t)) return 0;
	for (i = 0; i < nwords; i++) {
		si = ((unsigned*)INTEGER(s))[i];
		ti = ((unsigned*)INTEGER(t))[i];
		if(si > ti) return 0;
		if(si < ti) return 1;
	}
	return 0;
}

static void SortTerms(SEXP *x, int n)
{
	int i, j, h;
	SEXP xtmp;

	h = 1;
	do {
		h = 3 * h + 1;
	}
	while (h <= n);

	do {
		h = h / 3;
		for (i = h; i < n; i++) {
			xtmp = x[i];
			j = i;
			while (TermGT(x[j - h], xtmp)) {
				x[j] = x[j - h];
				j = j - h;
				if (j < h)
					goto end;
			}
		end:	x[j] = xtmp;
		}
	} while (h != 1);
}


	/* Internal code for the ``terms'' function */
	/* The value is a formula with an assortment */
	/* of useful attributes. */

/* .Internal(terms.formula(x, new.specials, abb, data, keep.order)) */
SEXP do_termsform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP a, ans, v, pattern, formula, varnames, term, termlabs;
	SEXP specials, t, abb, data;
	int i, j, k, l, n, keepOrder;

	checkArity(op, args);

		/* Always fetch these values rather than trying */
		/* to remember them between calls.  The overhead */
		/* is minimal and we don't have to worry about */
		/* intervening dump/restore problems. */

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
	identSymbol = install("I");

		/* Do we have a model formula? */
		/* Check for unary or binary ~ */

	if (!isLanguage(CAR(args)) ||
		CAR(CAR(args)) != tildeSymbol ||
		(length(CAR(args)) != 2 && length(CAR(args)) != 3 ) )
			error("argument is not a valid model\n");

	PROTECT(ans = duplicate(CAR(args)));

		/* The formula will be returned */

	specials = CADR(args);
	a = CDDR(args);

		/* abb = is unimplemented */
		/* FIXME: in any case it should be handled */
		/* in a separate "abbreviation expansion" */
		/* made before entry to this function. */

	abb = CAR(a);
	a=CDR(a);

		/* We use data to get the value to */
		/* substitute for "." in formulae */

	data = CAR(a);
	a = CDR(a);
	if(isNull(data) || isEnvironment(data))
		framenames = R_NilValue;
	else if (isFrame(data))
		framenames = getAttrib(data, R_NamesSymbol);
	else
		errorcall(call,"data argument is of the wrong type\n");
	if( framenames != R_NilValue )
		if( length(CAR(args))== 3 )
			CheckRHS(CADR(CAR(args)));

		/* Preserve term order? */

	keepOrder = asLogical(CAR(a));
	if(keepOrder == NA_LOGICAL)
		keepOrder = 0;

	if(specials == R_NilValue)
		ATTRIB(ans) = a = allocList(7);
	else
		ATTRIB(ans) = a = allocList(8);

		/* Step 1: Determine the ``variables'' in the model */
		/* Here we create an expression of the form */
		/* list(...).  You can evaluate it to get */
		/* the model variables or use substitute and then */
		/* pull the result apart to get the variable names. */

	intercept = 1;
	parity = 1;
	response = 0;
	PROTECT(varlist = lcons(install("list"), R_NilValue));
	ExtractVars(CAR(args), 1);
	UNPROTECT(1);
	CAR(a) = varlist;
	TAG(a) = install("variables");
	a = CDR(a);

	nvar = length(varlist) - 1;
	nwords = (nvar - 1) / WORDSIZE + 1;

		/* Step 2: Recode the model terms in binary form */
		/* and at the same time, expand the model formula. */

		/* FIXME: this includes specials in the model */
		/* There perhaps needs to be a an extra pass */
		/* through the model to delete any terms which */
		/* contain specials.  Actually, specials should */
		/* only enter additively so this should also be */
		/* checked and abort forced if not. */

		/* FIXME: this is also the point where nesting */
		/* needs to be taken care of. */

	PROTECT(formula = EncodeVars(CAR(args)));
	nterm = length(formula);

		/* Step 3: Reorder the model terms. */
		/* Horrible kludge -- write the addresses */
		/* into a vector, simultaneously computing the */
		/* the bitcount for each term.  Use a regular */
		/* (stable) sort of the vector based on bitcounts. */

	PROTECT(pattern = allocVector(STRSXP, nterm));
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call)) {
		LEVELS(CAR(call)) = BitCount(CAR(call));
		STRING(pattern)[n++] = CAR(call);
	}
	if(!keepOrder)
		SortTerms(STRING(pattern), nterm);
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call)) {
		CAR(call) = STRING(pattern)[n++];
	}
	UNPROTECT(1);

		/* Step 4: Compute the factor pattern for the model. */
		/* 0 - the variable does not appear in this term. */
		/* 1 - code the variable by contrasts in this term. */
		/* 2 - code the variable by indicators in this term. */

	if(nterm > 0) {
		CAR(a) = pattern = allocMatrix(INTSXP, nvar, nterm);
		TAG(a) = install("factors");
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
		CAR(a) = pattern = allocVector(INTSXP,0);
		TAG(a) = install("factors");
		a = CDR(a);
	}

		/* Step 5: Compute variable and term labels */
		/* These are glued immediately to the pattern matrix */

	PROTECT(varnames = allocVector(STRSXP, nvar));
	for (v = CDR(varlist), i = 0; v != R_NilValue; v = CDR(v)) {
		if (isSymbol(CAR(v)))
			STRING(varnames)[i++] = PRINTNAME(CAR(v));
		else
			STRING(varnames)[i++] = STRING(deparse1(CAR(v), 0))[0];
	}
	PROTECT(termlabs = allocVector(STRSXP, nterm));
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call)) {
		l = 0;
		for (i = 1; i <= nvar; i++) {
			if (GetBit(CAR(call), i)) {
				if (l > 0)
					l += 1;
				l += strlen(CHAR(STRING(varnames)[i - 1]));
			}
		}
		STRING(termlabs)[n] = allocString(l);
		CHAR(STRING(termlabs)[n])[0] = '\0';
		l = 0;
		for (i = 1; i <= nvar; i++) {
			if (GetBit(CAR(call), i)) {
				if (l > 0)
					strcat(CHAR(STRING(termlabs)[n]), ":");
				strcat(CHAR(STRING(termlabs)[n]), CHAR(STRING(varnames)[i - 1]));
				l++;
			}
		}
		n++;
	}
	PROTECT(v = allocList(2));
	CAR(v) = varnames;
	CADR(v) = termlabs;
	if(nterm > 0)
		setAttrib(pattern, R_DimNamesSymbol, v);

	CAR(a) = termlabs;
	TAG(a) = install("term.labels");
	a = CDR(a);

		/* If there are specials stick them in here */

	if(specials != R_NilValue) {
		i = length(specials);
		PROTECT(v = allocList(i));
		for( j=0, t=v ; j<i ; j++, t=CDR(t) ) {
			TAG(t) = install(CHAR(STRING(specials)[j]));
			n = strlen(CHAR(STRING(specials)[j]));
			CAR(t) = allocVector(INTSXP, 0);
			k = 0;
			for(l=0 ; l<nvar ; l++) {
				if(!strncmp(CHAR(STRING(varnames)[l]),
				            CHAR(STRING(specials)[j]), n))
					if(CHAR(STRING(varnames)[l])[n] == '(')
						k++;
			}
			if(k > 0) {
				CAR(t) = allocVector(INTSXP, k);
				k = 0;
				for(l=0 ; l<nvar ; l++) {
					if(!strncmp(CHAR(STRING(varnames)[l]),
						    CHAR(STRING(specials)[j]), n))
						if(CHAR(STRING(varnames)[l])[n] == '('){
							INTEGER(CAR(t))[k++] = l+1;
						}
				}
			}
			else CAR(t) = R_NilValue;
		}
		CAR(a)=v;
		TAG(a)=install("specials");
		a=CDR(a);
		UNPROTECT(1);
	}

	UNPROTECT(3);	/* keep termlabs until here */

	CAR(a) = allocVector(INTSXP, nterm);
	n = 0;
	for (call = formula; call != R_NilValue; call = CDR(call))
		INTEGER(CAR(a))[n++] = LEVELS(CAR(call));
	TAG(a) = install("order");
	a = CDR(a);

	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = (intercept != 0);
	TAG(a) = install("intercept");
	a = CDR(a);

	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = (response != 0);
	TAG(a) = install("response");
	a = CDR(a);

	CAR(a) = mkString("terms");
	TAG(a) = install("class");
	OBJECT(ans) = 1;

	UNPROTECT(2);
	return ans;
}

	/* Update a model formula by the replacement of "." templates. */

static SEXP ExpandDots(SEXP object, SEXP value)
{
	SEXP op;

	if(TYPEOF(object) == SYMSXP) {
		if(object == dotSymbol)
			object = duplicate(value);
		return object;
	}

	if(TYPEOF(object) == LANGSXP) {
		if(TYPEOF(value) == LANGSXP) op = CAR(value);
		else op = NULL;
		PROTECT(object);
		if(CAR(object) == plusSymbol) {
			if(length(object) == 2) {
				CADR(object) = ExpandDots(CADR(object), value);
			}
			else if(length(object) == 3) {
				CADR(object) = ExpandDots(CADR(object), value);
				CADDR(object) = ExpandDots(CADDR(object), value);
			}
			else goto badformula;
		}
		else if(CAR(object) == minusSymbol) {
			if(length(object) == 2) {
				if(CADR(object) == dotSymbol &&
				(op == plusSymbol || op == minusSymbol))
					CADR(object) = lang2(parenSymbol, ExpandDots(CADR(object), value));
				else
					CADR(object) = ExpandDots(CADR(object), value);
			}
			else if(length(object) == 3) {
				if(CADR(object) == dotSymbol &&
				(op == plusSymbol || op == minusSymbol))
					CADR(object) = lang2(parenSymbol, ExpandDots(CADR(object), value));
				else
					CADR(object) = ExpandDots(CADR(object), value);
				if(CADDR(object) == dotSymbol &&
				(op == plusSymbol || op == minusSymbol))
					CADDR(object) = lang2(parenSymbol, ExpandDots(CADDR(object), value));
				else
					CADDR(object) = ExpandDots(CADDR(object), value);
			}
			else goto badformula;
		}
		else if(CAR(object) == timesSymbol || CAR(object) == slashSymbol) {
			if(length(object) != 3)
				goto badformula;
			if(CADR(object) == dotSymbol &&
			(op == plusSymbol || op == minusSymbol))
				CADR(object) = lang2(parenSymbol, ExpandDots(CADR(object), value));
			else
				CADR(object) = ExpandDots(CADR(object), value);
			if(CADDR(object) == dotSymbol &&
			(op == plusSymbol || op == minusSymbol))
				CADDR(object) = lang2(parenSymbol, ExpandDots(CADDR(object), value));
			else
				CADDR(object) = ExpandDots(CADDR(object), value);
		}
		else if(CAR(object) == colonSymbol) {
			if(length(object) != 3)
				goto badformula;
			if(CADR(object) == dotSymbol && (
			   op == plusSymbol || op == minusSymbol ||
			   op == timesSymbol || op == slashSymbol))
				CADR(object) = lang2(parenSymbol, ExpandDots(CADR(object), value));
			else
				CADR(object) = ExpandDots(CADR(object), value);
			if(CADDR(object) == dotSymbol &&
			(op == plusSymbol || op == minusSymbol))
				CADDR(object) = lang2(parenSymbol, ExpandDots(CADDR(object), value));
			else
				CADDR(object) = ExpandDots(CADDR(object), value);
		}
		else if(CAR(object) == powerSymbol) {
			if(length(object) != 3)
				goto badformula;
			if(CADR(object) == dotSymbol && (
                           op == plusSymbol || op == minusSymbol ||
			   op == timesSymbol || op == slashSymbol ||
			   op == colonSymbol))
				CADR(object) = lang2(parenSymbol, ExpandDots(CADR(object), value));
			else
				CADR(object) = ExpandDots(CADR(object), value);
			if(CADDR(object) == dotSymbol &&
			(op == plusSymbol || op == minusSymbol))
				CADDR(object) = lang2(parenSymbol, ExpandDots(CADDR(object), value));
			else
				CADDR(object) = ExpandDots(CADDR(object), value);
		}
		else {
			op = object;
			while(op != R_NilValue) {
				CAR(op) = ExpandDots(CAR(op), value);
				op = CDR(op);
			}
		}
		UNPROTECT(1);
		return object;
	}
	else return object;

badformula:
	error("invalid formula in update\n");
	return R_NilValue;/*NOTREACHED*/
}

SEXP do_updateform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP new, old, lhs, rhs;

	checkArity(op, args);

		/* Always fetch these values rather than trying */
		/* to remember them between calls.  The overhead */
		/* is minimal and we don't have to worry about */
		/* intervening dump/restore problems. */

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
	identSymbol = install("I");

		/* We must duplicate here because the */
		/* formulae may be part of the parse tree */
		/* and we don't want to modify it. */

	old = CAR(args);
	new = CADR(args) = duplicate(CADR(args));

		/* Check of new and old formulae. */
		/* The old one must be a valid model */
		/* formula with an lhs and rhs. */

	if(TYPEOF(old) != LANGSXP ||
	   (TYPEOF(new) != LANGSXP && CAR(old) != tildeSymbol) ||
	   CAR(new) != tildeSymbol)
			errorcall(call, "formula expected\n");
	if(length(old) != 3)
		errorcall(call, "invalid first formula\n");
	lhs = CADR(old);
	rhs = CADDR(old);

		/* We now check that new formula has a */
		/* valid lhs.  If it doesn't, we add one */
		/* and set it to the rhs of the old formula. */

	if(length(new) == 2)
		CDR(new) = CONS(lhs, CDR(new));

		/* Now we check the left and right sides */
		/* of the new formula and substitute the */
		/* correct value for any "." templates. */
		/* We must parenthesize the rhs or we */
		/* might upset arity and precedence. */

	PROTECT(rhs);

	CADR(new) = ExpandDots(CADR(new), lhs);
	CADDR(new) = ExpandDots(CADDR(new), rhs);
	UNPROTECT(1);

		/* It might be overkill to zero the */
		/* the attribute list of the returned */
		/* value, but it can't hurt. */

	ATTRIB(new) = R_NilValue;
	return new;
}


/*
 *  model.frame
 *
 *  The argument "terms" contains the terms object generated from the
 *  model formula.  We first evaluate the "variables" attribute of
 *  "terms" in the "data" environment.  This gives us a list of basic
 *  variables to be in the model frame.  We do some basic sanity
 *  checks on these to ensure that resulting object make sense.
 *
 *  The argument "dots" gives additional things like "weights", "offsets"
 *  and "subset" which will also go into the model frame so that they can
 *  be treated in parallel.
 *
 *  Next we subset the data frame according to "subset" and finally apply
 *  "na.action" to get the final data frame.
 *
 *  Note that the "terms" argument is glued to the model frame as an
 *  attribute.  Code downstream appears to need this.
 *
 *  Q: Is this really needed, or can we get by with less info?
 */

static SEXP ProcessDots(SEXP dots, char *buf)
{
	if(dots == R_NilValue)
		return dots;
	if(TAG(dots) == R_NilValue)
		error("unnamed list element in model.frame.default");
	CDR(dots) = ProcessDots(CDR(dots), buf);
	sprintf(buf, "(%s)", CHAR(PRINTNAME(TAG(dots))));
	TAG(dots) = install(buf);
	return dots;
}

/* .Internal(model.frame(formula, data, dots, subset, na.action)) */

SEXP do_modelframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP terms, data, dots, envir, na_action;
	SEXP ans, row_names, subset, tmp, variables;
	char buf[256];
	int i, nr, nc;

	checkArity(op, args);
	terms = CAR(args); args = CDR(args);
	envir = CAR(args); args = CDR(args);
	dots = CAR(args); args = CDR(args);
	subset = CAR(args); args = CDR(args);
	na_action = CAR(args); args = CDR(args);

		/* Save the row names for later use. */

	PROTECT(row_names = getAttrib(envir, R_RowNamesSymbol));

		/* Assemble the base data frame. */

	PROTECT(variables = getAttrib(terms, install("variables")));
	if(isNull(variables) || !isLanguage(variables))
		errorcall(call, "invalid terms object\n");
	if(isList(envir)) {
		tmp = emptyEnv();
		FRAME(tmp) = envir;
		ENCLOS(tmp) = R_GlobalEnv;
		envir = tmp;
	}
	else if(!isEnvironment(envir))
		errorcall(call, "Invalid data argument\n");
	PROTECT(envir);
	data = eval(variables, envir);
	UNPROTECT(2);
	PROTECT(envir);
	PROTECT(data);

		/* Create the names for the variables. */
		/* To do this construct a call to */
		/* as.character(substitute(list(...))) */
		/* and ignore the first element of the */
		/* resulting character string vector. */

	PROTECT(tmp = lang2(install("substitute"), variables));
	PROTECT(tmp = lang2(install("as.character"), tmp));
	tmp = eval(tmp, rho);
	UNPROTECT(2);
	PROTECT(tmp);
	i = 1;
	ans = data;
	while(ans != R_NilValue) {
		TAG(ans) = install(CHAR(STRING(tmp)[i]));
		ans = CDR(ans);
		i = i + 1;
	}
	UNPROTECT(1);

		/* Sanity checks to ensure that the */
		/* the answer can become a data frame. */
		/* Be deeply suspicious here! */

	if(!isList(data))
		errorcall(call, "variables not in list form\n");
	nc = 0;
	if(!isNull(data)) {
		nr = nrows(CAR(data));
		for(ans=data ; ans!=R_NilValue ; ans=CDR(ans)) {
			if(TYPEOF(CAR(ans)) < LGLSXP ||
					TYPEOF(CAR(ans)) > REALSXP)
				errorcall(call, "invalid variable type\n");
			if(nrows(CAR(ans)) != nr)
				errorcall(call, "variable lengths differ\n");
			nc++;
		}
	}

		/* Evaluate the additional frame components. */
		/* Things like weights, subset, offset, etc. */

	if(isNull(dots) || !isLanguage(dots))
		errorcall(call, "invalid dots object\n");
	dots = eval(dots, envir);
	PROTECT(dots);

		/* Glue the data object and the dots objects */
		/* together, checking that dimensions and */
		/* types are sensible.  Note that any "subset" */
		/* component in the dots object is treated */
		/* specially.  It will be used for subsetting */
		/* not returned in the data frame. */

	if(!isList(dots))
		errorcall(call, "variables not in list form\n");

	if(!isNull(dots)) {
		if(nr == 0) nr = nrows(CAR(dots));
		for(ans=dots ; ans!=R_NilValue ; ans=CDR(ans)) {
			if(TYPEOF(CAR(ans)) < LGLSXP ||
					TYPEOF(CAR(ans)) > REALSXP)
				errorcall(call, "invalid variable type\n");
			if(nrows(CAR(ans)) != nr)
				errorcall(call, "variable lengths differ\n");
		}
		dots = ProcessDots(dots, buf);
		if(!isNull(data)) {
			ans = data;
			while(CDR(ans) != R_NilValue)
				 ans = CDR(ans);
			CDR(ans) = dots;
		}
		else data = dots;
	}
	UNPROTECT(3);
	PROTECT(data);
	PROTECT(subset);

		/* Turn the data "list" into a "data.frame" */
		/* so that subsetting methods will work. */
		/* To do this we must attach "class"  and */
		/* "row.names" attributes */

	PROTECT(tmp = mkString("data.frame"));
	setAttrib(data, R_ClassSymbol, tmp);
	UNPROTECT(1);
	if(length(row_names) == nr) {
		setAttrib(data, R_RowNamesSymbol, row_names);
	}
	else {
		PROTECT(row_names = allocVector(STRSXP, nr));
		for(i=0 ; i<nr ; i++) {
			sprintf(buf, "%d", i+1);
			STRING(row_names)[i] = mkChar(buf);
		}
		setAttrib(data, R_RowNamesSymbol, row_names);
		UNPROTECT(1);
	}

		/* Do the subsetting, if required. */

	if(subset != R_NilValue) {
		PROTECT(tmp = lang4(install("["), data, subset, R_MissingArg));
		data = eval(tmp, rho);
		UNPROTECT(1);
	}
	UNPROTECT(3);
	PROTECT(data);

		/* finally, we run na.action on the data frame */
		/* usually, this will be na.fail which should */
		/* just be a check and should not use memory */

	if(na_action != R_NilValue) {
		if(isString(na_action) && length(na_action) > 0)
			na_action = install(CHAR(STRING(na_action)[0]));
		PROTECT(na_action);
		PROTECT(tmp = lang2(na_action, data));
		ans = eval(tmp, rho);
		UNPROTECT(2);
	}
	else ans = data;
	UNPROTECT(1);
	PROTECT(ans);

		/* Finally, tack on a terms attribute */

	setAttrib(ans, install("terms"), terms);
	UNPROTECT(1);
	return ans;
}

	/* Internal code for the ~ operator */
	/* Just returns the unevaluated call */
	/* No longer needed??? */

SEXP do_tilde(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP class;
	PROTECT(call = duplicate(call));
	PROTECT(class = allocVector(STRSXP, 1));
	STRING(class)[0] = mkChar("formula");
	setAttrib(call, R_ClassSymbol, class);
	UNPROTECT(2);
	return call;
}


	/* The code below is related to model expansion */
	/* and is ultimately called by do_modelmatrix. */

static void firstfactor(double *x, int nrx, int ncx, double *c, int nrc, int ncc, int *v)
{
	double *cj, *xj;
	int i, j;

	for(j=0 ; j<ncc ; j++) {
		xj = &x[j*nrx];
		cj = &c[j*nrc];
		for(i=0 ; i<nrx ; i++)
			xj[i] = cj[v[i]-1];
	}
}

static void addfactor(double *x, int nrx, int ncx, double *c, int nrc, int ncc, int *v)
{
	int i, j, k;
	double *ck, *xj, *yj;

	for(k=ncc-1 ; k>=0 ; k--) {
		for(j=0 ; j<ncx ; j++) {
			xj = &x[j*nrx];
			yj = &x[(k*ncx+j)*nrx];
			ck = &c[k*nrc];
			for(i=0; i<nrx ; i++)
				yj[i] = ck[v[i]-1] * xj[i];
		}
	}
}

static void firstvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
	double *cj, *xj;
	int i, j;

	for(j=0 ; j<ncc ; j++) {
		xj = &x[j*nrx];
		cj = &c[j*nrc];
		for(i=0 ; i<nrx ; i++)
			xj[i] = cj[i];
	}
}

static void addvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
	int i, j, k;
	double *ck, *xj, *yj;

	for(k=ncc-1 ; k>=0 ; k--) {
		for(j=0 ; j<ncx ; j++) {
			xj = &x[j*nrx];
			yj = &x[(k*ncx+j)*nrx];
			ck = &c[k*nrc];
			for(i=0; i<nrx ; i++)
				yj[i] = ck[i] * xj[i];
		}
	}
}

#define BUFSIZE 128

static char *AppendString(char *buf, char *str)
{
	while (*str)
		*buf++ = *str++;
	*buf = '\0';
	return buf;
}

static char *AppendInteger(char *buf, int i)
{
	sprintf(buf, "%d", i);
	while(*buf) buf++;
	return buf;
}

SEXP do_modelmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP expr, factors, terms, v, vars, vnames, assign, xnames, tnames;
	SEXP count, contrast, contr1, contr2, nlevs, ordered, columns, x;
	SEXP variable, var_i;
	int fik, first, i, j, k, kk, ll, n, nc, nterms, nvar;
	int intercept, jstart, jnext, response, index;
	char buf[BUFSIZE], *bufp;

	checkArity(op, args);

		/* Get the "terms" structure and extract */
		/* the intercept and response attributes. */

	terms = CAR(args);

	intercept = asLogical(getAttrib(terms, install("intercept")));
	if(intercept == NA_INTEGER) intercept = 0;

	response = asLogical(getAttrib(terms, install("response")));
	if(response == NA_INTEGER) response = 0;

		/* Get the factor pattern matrix.  We duplicate */
		/* this because we may want to alter it if we */
		/* are in the no-intercept case. */
		/* Note: the values of "nvar" and "nterms" are the */
		/* REAL number of variables in the model data frame */
		/* and the number of model terms. */

	PROTECT(factors = duplicate(getAttrib(terms, install("factors"))));
	if(length(factors) == 0) {
		if(intercept == 0)
			errorcall(call, "illegal model (zero parameters).\n");
		nvar = 1;
		nterms = 0;
	}
	else if(isInteger(factors) && isMatrix(factors)) {
		nvar = nrows(factors);
		nterms = ncols(factors);
	}
	else errorcall(call, "invalid terms argument\n");

		/* Get the variable names from the factor matrix */

	vnames = CAR(getAttrib(factors, R_DimNamesSymbol));
	if(nvar - intercept > 0 && !isString(vnames))
		errorcall(call, "invalid terms argument\n");

		/* Get the variables from the model frame. */
		/* First perform elementary sanity checks. */
		/* Notes: 1) We need at least one variable */
		/* (lhs or rhs) to compute the number of cases. */
		/* 2) We don't type-check the response. */

	vars = CADR(args);
	if(!(isList(vars) || isFrame(vars)) || length(vars) < nvar)
		errorcall(call, "invalid model frame\n");
	if(length(vars) == 0)
		errorcall(call, "don't know how many cases\n");
	n = nrows(CAR(vars));

		/* VECTOR ME */
		/* We want random access to the variables */
		/* so we transfer the variables to a generic */
		/* vector as we type check them.  When we */
		/* switch to generic vectors this transfer */
		/* can be taken out */

		/* This section of the code checks the */
		/* types of the variables in the model */
		/* frame.  Note that it should really */
		/* only check the variables if they appear */
		/* in a term in the model. */

	PROTECT(variable = allocVector(VECSXP, nvar));
	PROTECT(nlevs = allocVector(INTSXP, nvar));
	PROTECT(ordered = allocVector(LGLSXP, nvar));
	PROTECT(columns = allocVector(INTSXP, nvar));

	v = vars;
	for(i=0 ; i<nvar ; i++) {
		var_i = VECTOR(variable)[i] = CAR(v);
		if(nrows(var_i) != n)
			errorcall(call, "variable lengths differ\n");
		if(i == response - 1) {
			LOGICAL(ordered)[0] = 0;
			INTEGER(nlevs)[0] = 0;
			INTEGER(columns)[0] = 0;
		}
		else if(isOrdered(var_i)) {
			LOGICAL(ordered)[i] = 1;
			INTEGER(nlevs)[i] = nlevels(var_i);
			INTEGER(columns)[i] = ncols(var_i);
		}
		else if(isUnordered(var_i)) {
			LOGICAL(ordered)[i] = 0;
			INTEGER(nlevs)[i] = nlevels(var_i);
			INTEGER(columns)[i] = ncols(var_i);
		}
		else if(isNumeric(var_i)) {
			VECTOR(variable)[i] = coerceVector(var_i, REALSXP);
			var_i = VECTOR(variable)[i];
			LOGICAL(ordered)[i] = 0;
			INTEGER(nlevs)[i] = 0;
			INTEGER(columns)[i] = ncols(var_i);
		}
		else errorcall(call, "invalid variable type\n");
		v = CDR(v);
	}

		/* If there is no intercept we look through the */
		/* factor pattern matrix and adjust the code */
		/* for the first factor found so that it will be */
		/* coded by dummy variables rather than contrasts. */

	if(!intercept) {
		for(j=0 ; j<nterms ; j++) {
			for(i=response ; i<nvar ; i++) {
				if(INTEGER(nlevs)[i] > 1
				&& INTEGER(factors)[i+j*nvar] == 1) {
					INTEGER(factors)[i+j*nvar] = 2;
					goto alldone;
				}
			}
		}
	}
alldone:
	;

		/* Compute the required contrast or dummy */
		/* variable matrices.  We set up a symbolic */
		/* expression to evaluate these, substituting */
		/* the required arguments at call time. */
		/* The calls have the following form: */
		/* (contrast.type nlevs contrasts) */

	PROTECT(contr1 = allocVector(VECSXP, nvar));
	PROTECT(contr2 = allocVector(VECSXP, nvar));

	PROTECT(expr = allocList(3));
	TYPEOF(expr) = LANGSXP;
	CAR(expr) = install("contrasts");
	CADDR(expr) = allocVector(LGLSXP, 1);

		/* FIXME: We need to allow a third argument */
		/* to this function which allows us to specify */
		/* contrasts directly.  That argument would be used */
		/* here in exactly the same way as the below */
		/* I.e. we would search the list of constrast */
		/* specs before we try the evaluation below. */

	for(i=0 ; i<nvar ; i++) {
		if(INTEGER(nlevs)[i]) {
			k = 0;
			for(j=0 ; j<nterms ; j++) {
				if(INTEGER(factors)[i+j*nvar] == 1)
					k |= 1;
				else if(INTEGER(factors)[i+j*nvar] == 2)
					k |= 2;
			}
			CADR(expr) = VECTOR(variable)[i];
			if(k & 1) {
				LOGICAL(CADDR(expr))[0] = 1;
				VECTOR(contr1)[i] = eval(expr, rho);
			}
			if(k & 2) {
				LOGICAL(CADDR(expr))[0] = 0;
				VECTOR(contr2)[i] = eval(expr, rho);
			}
		}
	}


		/* We now have everything needed to build the */
		/* design matrix.  So let's do it.  The first */
		/* step is to compute the matrix size and */
		/* allocate it. */

	PROTECT(count = allocVector(INTSXP, nterms));
	if(intercept) nc = 1; else nc = 0;
	for(j=0 ; j<nterms ; j++) {
		k = 1;
		for(i=0 ; i<nvar ; i++) {
			if(INTEGER(factors)[i+j*nvar]) {
				if(INTEGER(nlevs)[i]) {
					switch(INTEGER(factors)[i+j*nvar]) {
					case 1:
						k *= ncols(VECTOR(contr1)[i]);
						break;
					case 2:
						k *= ncols(VECTOR(contr2)[i]);
						break;
					}
				}
				else k *= INTEGER(columns)[i];
			}
		}
		INTEGER(count)[j] = k;
		nc = nc + k;
	}

		/* Record which columns of the design matrix */
		/* are associated which which model terms. */

	PROTECT(assign = allocVector(INTSXP, nc));
	k = 0;
	if(intercept) INTEGER(assign)[k++] = 0;
	for(j=0 ; j<nterms ; j++)
		for(i=0 ; i<INTEGER(count)[j] ; i++)
			INTEGER(assign)[k++] = j+1;


		/* Create column labels for the matrix columns. */

	PROTECT(xnames = allocVector(STRSXP, nc));
	tnames = getAttrib(factors, R_DimNamesSymbol);
	if(nterms > 0) {
		if(isNull(tnames))
			errorcall(call, "invalid terms object!\n");
		tnames = CADR(tnames);
	}
	else tnames = R_NilValue;

	k = 0;
	if(intercept) STRING(xnames)[k++] = mkChar("(Intercept)");

	for(j=0 ; j<nterms ; j++) {
		for(kk=0 ; kk<INTEGER(count)[j] ; kk++) {
			first = 1;
			index = kk;
			bufp = &buf[0];
			for(i=0 ; i<nvar ; i++) {
				var_i = VECTOR(variable)[i];
				if(ll = INTEGER(factors)[i+j*nvar]) {
					if(!first) bufp = AppendString(bufp, ".");
					first = 0;
					if(isFactor(var_i)) {
						if(ll == 1) {
							x = CADR(getAttrib(VECTOR(contr1)[i], R_DimNamesSymbol));
							ll = ncols(VECTOR(contr1)[i]);
						}
						else {
							x = CADR(getAttrib(VECTOR(contr2)[i], R_DimNamesSymbol));
							ll = ncols(VECTOR(contr2)[i]);
						}
						bufp = AppendString(bufp, CHAR(STRING(vnames)[i]));
						if(x == R_NilValue)
							bufp = AppendInteger(bufp, index%ll+1);
						else
							bufp = AppendString(bufp, CHAR(STRING(x)[index%ll]));
					}
					else {
						x = CADR(getAttrib(var_i, R_DimNamesSymbol));
						ll = ncols(var_i);
						bufp = AppendString(bufp, CHAR(STRING(vnames)[i]));
						if(ll > 1) {
							if(x == R_NilValue)
								bufp = AppendInteger(bufp, index%ll+1);
							else
								bufp = AppendString(bufp, CHAR(STRING(x)[index%ll]));
						}
					}
					index = index/ll;
				}
			}
			STRING(xnames)[k++] = mkChar(buf);
		}
	}

		/* Allocate and compute the design matrix. */

	PROTECT(x = allocMatrix(REALSXP, n, nc));

		/* a) Begin with a column of 1s for the intercept. */

	if((jnext = jstart = intercept) != 0) {
		for(i=0 ; i<n ; i++) {
			REAL(x)[i] = 1.0;
		}
	}

		/* b) Now loop over the model terms */

	for(k=0 ; k<nterms ; k++) {
		for(i=0 ; i<nvar ; i++) {
			var_i = VECTOR(variable)[i];
			fik = INTEGER(factors)[i+k*nvar];
			if(fik) {
				switch(fik) {
				case 1:
					contrast = VECTOR(contr1)[i];
					break;
				case 2:
					contrast = VECTOR(contr2)[i];
					break;
				}
				if(jnext == jstart) {
					if(INTEGER(nlevs)[i] > 0) {
						firstfactor(&REAL(x)[jstart*n], n, jnext-jstart, REAL(contrast), nrows(contrast), ncols(contrast), INTEGER(var_i));
						jnext = jnext+ncols(contrast);
					}
					else {
						firstvar(&REAL(x)[jstart*n], n, jnext-jstart, REAL(var_i), n, ncols(var_i));
						jnext = jnext+ncols(var_i);
					}
				}
				else {
					if(INTEGER(nlevs)[i] > 0) {
						addfactor(&REAL(x)[jstart*n], n, jnext-jstart, REAL(contrast), nrows(contrast), ncols(contrast), INTEGER(var_i));
						jnext = jnext+(jnext-jstart)*(ncols(contrast)-1);
					}
					else {
						addvar(&REAL(x)[jstart*n], n, jnext-jstart, REAL(var_i), n, ncols(var_i));
						jnext = jnext+(jnext-jstart)*(ncols(var_i)-1);
					}
				}
			}
		}
		jstart = jnext;
	}
	PROTECT(tnames = allocList(2));
	CADR(tnames) = xnames;
	setAttrib(x, R_DimNamesSymbol, tnames);
	setAttrib(x, install("assign"), assign);
	UNPROTECT(13);
	return x;
}
