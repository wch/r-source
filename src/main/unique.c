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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"

#define NIL -1
#define ARGUSED(x) LEVELS(x)

/* Hash function and equality test for keys */
static int K, M;
static int(*hash) (SEXP, int);
static int(*equal) (SEXP, int, SEXP, int);
static SEXP HashTable;

static int nomatch;

/* Integer keys are hashed via a random number generator */
/* based on Knuth's recommendations.  The high order K bits */
/* are used as the hash code. */

/* WARNING / FIXME : this doesn't work if K = 0 so some */
/* fixes/warnings probably need to be installed somewhere. (RG) */

static int scatter(unsigned int key)
{
    return 3141592653U * key >> (32 - K);
}

static int lhash(SEXP x, int index)
{
    if (LOGICAL(x)[index] == NA_LOGICAL)
	return 2;
    return LOGICAL(x)[index];
}

static int ihash(SEXP x, int index)
{
    if (INTEGER(x)[index] == NA_INTEGER)
	return 0;
    return scatter((unsigned int) (INTEGER(x)[index]));
}

static int rhash(SEXP x, int index)
{
    /* There is a problem with signed 0s under IEEE */
    double tmp = (REAL(x)[index] == 0.0) ? 0.0 : REAL(x)[index];
    return scatter(*((unsigned int *) (&tmp)));
}

static int chash(SEXP x, int index)
{
    complex tmp;
    tmp.r = (COMPLEX(x)[index].r == 0.0) ? 0.0 : COMPLEX(x)[index].r;
    tmp.i = (COMPLEX(x)[index].i == 0.0) ? 0.0 : COMPLEX(x)[index].i;
    return scatter((*((unsigned int *)(&tmp.r)) |
		    (*((unsigned int *)(&tmp.r)))));
}

static int shash(SEXP x, int index)
{
    unsigned int k;
    char *p = CHAR(STRING(x)[index]);
    k = 0;
    while (*p++)
	k = 8 * k + *p;
    return scatter(k);
}

static int iequal(SEXP x, int i, SEXP y, int j)
{
    return (INTEGER(x)[i] == INTEGER(y)[j]);
}

static int requal(SEXP x, int i, SEXP y, int j)
{
    if (!ISNAN(REAL(x)[i]) && !ISNAN(REAL(y)[j])) {
	return (REAL(x)[i] == REAL(y)[j]);
    }
    else if (ISNAN(REAL(x)[i]) && ISNAN(REAL(y)[j])) {
	return 1;
    }
    return 0;
}

static int cequal(SEXP x, int i, SEXP y, int j)
{
    if (!ISNAN(COMPLEX(x)[i].r) && !ISNAN(COMPLEX(x)[i].i)
       && !ISNAN(COMPLEX(y)[j].r) && !ISNAN(COMPLEX(y)[j].i)) {
	return COMPLEX(x)[i].r == COMPLEX(y)[j].r &&
	    COMPLEX(x)[i].i == COMPLEX(y)[j].i;
    }
    else if ((ISNAN(COMPLEX(x)[i].r) || ISNAN(COMPLEX(x)[i].i))
	    && (ISNAN(COMPLEX(y)[j].r) || ISNAN(COMPLEX(y)[j].i))) {
	return 1;
    }
    return 0;
}

static int sequal(SEXP x, int i, SEXP y, int j)
{
    return !strcmp(CHAR(STRING(x)[i]), CHAR(STRING(y)[j]));
}

/* Choose M to be the smallest power of 2 */
/* not less than 2*n and set K = log2(M) */
static void MKsetup(int n)
{
    int n2 = 2 * n;
    M = 1;
    K = 0;
    while (M < n2) {
	M *= 2;
	K += 1;
    }
}

void HashTableSetup(SEXP x)
{
    switch (TYPEOF(x)) {
    case LGLSXP:
	hash = lhash;
	equal = iequal;
	MKsetup(3);
	break;
    case INTSXP:
	hash = ihash;
	equal = iequal;
	MKsetup(LENGTH(x));
	break;
    case REALSXP:
	hash = rhash;
	equal = requal;
	MKsetup(LENGTH(x));
	break;
    case CPLXSXP:
	hash = chash;
	equal = cequal;
	MKsetup(LENGTH(x));
	break;
    case STRSXP:
	hash = shash;
	equal = sequal;
	MKsetup(LENGTH(x));
	break;
    }
    HashTable = allocVector(INTSXP, M);
}

/* Open address hashing */
/* Collision resolution is by linear probing */
/* The table is guaranteed large so this is sufficient */

static int isDuplicated(SEXP x, int index)
{
    int i, *h;

    h = INTEGER(HashTable);
    i = hash(x, index);
    while (h[i] != NIL) {
	if (equal(x, h[i], x, index))
	    return 1;
	i = (i + 1) % M;
    }
    h[i] = index;
    return 0;
}

SEXP duplicated(SEXP x)
{
    SEXP ans;
    int *h, *v;
    int i, n;

    n = LENGTH(x);
    HashTableSetup(x);
    PROTECT(HashTable);
    ans = allocVector(LGLSXP, n);
    UNPROTECT(1);
    h = INTEGER(HashTable);
    v = LOGICAL(ans);

    for (i = 0; i < M; i++)
	h[i] = NIL;

    for (i = 0; i < n; i++)
	v[i] = isDuplicated(x, i);

    return ans;
}

SEXP do_duplicated(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, dup, ans;
    int i, k, n;

    checkArity(op, args);
    x = CAR(args);
    if (!(isVector(x) || isNull(x)))
	error("duplicated applies only to vectors\n");

    /* handle zero length vectors */
    if (length(x) == 0) {
	if (PRIMVAL(op) == 0)
	    return (allocVector(LGLSXP, 0));
	else
	    return (allocVector(TYPEOF(x), 0));
    }

    /* code for "duplicated" */
    dup = duplicated(x);
    if (PRIMVAL(op) == 0)
	return dup;

    /* use the results of "duplicated" to get "unique" */
    n = LENGTH(x);

    /* count unique entries */
    k = 0;
    for (i = 0; i < n; i++)
	if (LOGICAL(dup)[i] == 0)
	    k++;

    PROTECT(dup);
    ans = allocVector(TYPEOF(x), k);
    UNPROTECT(1);

    k = 0;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		INTEGER(ans)[k++] = INTEGER(x)[i];
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		REAL(ans)[k++] = REAL(x)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0) {
		COMPLEX(ans)[k].r = COMPLEX(x)[i].r;
		COMPLEX(ans)[k].i = COMPLEX(x)[i].i;
		k++;
	    }
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (LOGICAL(dup)[i] == 0)
		STRING(ans)[k++] = STRING(x)[i];
	break;
    }
    return ans;
}

/* Build a hash table, ignoring information on duplication */
static void DoHashing(SEXP table)
{
    int *h, i, n;

    n = LENGTH(table);
    h = INTEGER(HashTable);

    for (i = 0; i < M; i++)
	h[i] = NIL;

    for (i = 0; i < n; i++)
	(void) isDuplicated(table, i);
}

static int Lookup(SEXP table, SEXP x, int index)
{
    int i, *h;

    h = INTEGER(HashTable);
    i = hash(x, index);
    while (h[i] != NIL) {
	if (equal(table, h[i], x, index))
	    return h[i] + 1;
	i = (i + 1) % M;
    }
    return nomatch;
}

/* Now do the table lookup */
static SEXP HashLookup(SEXP table, SEXP x)
{
    SEXP ans;
    int i, n;

    n = LENGTH(x);
    ans = allocVector(INTSXP, n);
    for (i = 0; i < n; i++) {
	INTEGER(ans)[i] = Lookup(table, x, i);
    }
    return ans;
}

SEXP do_match(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, table, ans;
    SEXPTYPE type;
    int n, i;

    checkArity(op, args);

    if ((!isVector(CAR(args)) && !isNull(CAR(args)))
	|| (!isVector(CADR(args)) && !isNull(CADR(args))))
	error("match requires vector arguments\n");

    /* Coerce to a common type */
    /* Note: type == NILSXP is ok here */

    type = TYPEOF(CAR(args)) < TYPEOF(CADR(args)) ?
	TYPEOF(CADR(args)) : TYPEOF(CAR(args));
    x = CAR(args) = coerceVector(CAR(args), type);
    table = CADR(args) = coerceVector(CADR(args), type);
    nomatch = asInteger(CAR(CDDR(args)));
    n = length(x);

    /* handle zero length arrays */
    if (n == 0) return allocVector(INTSXP, 0);
    if (length(table) == 0) {
	ans = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = nomatch;
	return ans;
    }

    HashTableSetup(table);
    PROTECT(HashTable);
    DoHashing(table);
    ans = HashLookup(table, x);
    UNPROTECT(1);
    return ans;
}

SEXP match(SEXP table, SEXP x, int nmatch)
{
    SEXP ans;

    nomatch=nmatch;
    HashTableSetup(table);
    PROTECT(HashTable);
    DoHashing(table);
    ans = HashLookup(table, x);
    UNPROTECT(1);
    return ans;
}

/* Partial Matching of Strings */
/* Fully S Compatible version. */

SEXP do_pmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    int i, j, k, match, n_input, n_target, match_count, temp, dups_ok;
    checkArity(op, args);

    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);
    dups_ok = asLogical(CADDR(args));
    if (dups_ok == NA_LOGICAL)
	errorcall(call, "invalid \"duplicates.ok\" argument\n");

    if (!isString(input) || !isString(target))
	errorcall(call, "argument is not of mode character\n");

    ans = allocVector(INTSXP, n_input);

    for (i = 0; i < n_input; i++) {
	temp = strlen(CHAR(STRING(input)[i]));
	match = 0;
	match_count = 0;
	if (temp) {
	    for (j = 0; j < n_target; j++) {
		k = strncmp(CHAR(STRING(input)[i]),
			    CHAR(STRING(target)[j]), temp);
		if (k == 0) {
		    match = j + 1;
		    if (dups_ok || 
		    	strlen(CHAR(STRING(target)[j])) == temp) 
			/* This is odd, effectively sets dups.ok
			 * for perfect matches, but that's what
			 * Splus 3.4 does  --pd
			 */
		        break; 
		    if (match_count++ && !dups_ok)
			match = 0;
		}
	    }
	}
	INTEGER(ans)[i] = match;
    }
    return ans;
}


/* Partial Matching of Strings */
/* Based on Therneau's charmatch. */

SEXP do_charmatch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, input, target;
    int i, j, k, match, n_input, n_target, perfect, temp;
    checkArity(op, args);

    input = CAR(args);
    n_input = LENGTH(input);
    target = CADR(args);
    n_target = LENGTH(target);

    if (!isString(input) || !isString(target))
	errorcall(call, "argument is not of mode character\n");

    ans = allocVector(INTSXP, n_input);

    for (i = 0; i < n_input; i++) {
	temp = strlen(CHAR(STRING(input)[i]));
	match = NA_INTEGER;
	perfect = 0;
	for (j = 0; j < n_target; j++) {
	    k = strncmp(CHAR(STRING(input)[i]),
			CHAR(STRING(target)[j]), temp);
	    if (k == 0) {
		if (strlen(CHAR(STRING(target)[j])) == temp) {
		    if (perfect == 1)
			match = 0;
		    else {
			perfect = 1;
			match = j + 1;
		    }
		}
		else if (perfect == 0) {
		    if (match == NA_INTEGER)
			match = j + 1;
		    else
			match = 0;
		}
	    }
	}
	INTEGER(ans)[i] = match;
    }
    return ans;
}


/* Functions for matching the supplied arguments to the */
/* formal arguments of functions.  The returned value */
/* is a list with all components named. */

#define ARGUSED(x) LEVELS(x)

static SEXP StripUnmatched(SEXP s)
{
    if (s == R_NilValue) return s;

    if (CAR(s) == R_MissingArg && !ARGUSED(s) ) {
	return StripUnmatched(CDR(s));
    }
    else if (CAR(s) == R_DotsSymbol ) {
	return StripUnmatched(CDR(s));
    }
    else {
	CDR(s) = StripUnmatched(CDR(s));
	return s;
    }
}

static SEXP ExpandDots(SEXP s, int expdots)
{
    SEXP r;
    if (s == R_NilValue)
	return s;
    if (TYPEOF(CAR(s)) == DOTSXP ) {
	TYPEOF(CAR(s)) = LISTSXP;	/* a safe mutation */
	if (expdots) {
	    r = CAR(s);
	    while (CDR(r) != R_NilValue ) {
		ARGUSED(r) = 1;
		r = CDR(r);
	    }
	    ARGUSED(r) = 1;
	    CDR(r)= ExpandDots(CDR(s), expdots);
	    return CAR(s);
	}
    }
    else
	ARGUSED(s) = 0;
    CDR(s) = ExpandDots(CDR(s), expdots);
    return s;
}
static SEXP subDots(SEXP rho)
{
    SEXP rval, dots, a, b;
    int len,i;
    char tbuf[10];

    dots = findVarInFrame(rho, R_DotsSymbol);

    if (dots == R_MissingArg) 
	return dots;

    len = length(dots);
    PROTECT(rval=allocList(len));
    for(a=dots, b=rval, i=1; a!=R_NilValue; a=CDR(a), b=CDR(b), i++) {
	sprintf(tbuf,"..%d",i);
	if( TAG(a) != R_NilValue )
	    TAG(b) = TAG(a);
	else
	    TAG(b) = install(tbuf);
	if( isSymbol(PREXPR(CAR(a))) || isLanguage(PREXPR(CAR(a))) ) {
		CAR(b) = mkSYMSXP(mkChar(tbuf), R_UnboundValue);
	}
	else
		CAR(b) = PREXPR(CAR(a));
    }
    UNPROTECT(1);
    return rval;
}


SEXP do_matchcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP formals, actuals, rlist;
    SEXP funcall, f, b, rval, sysp, t1, t2, tail;
    RCNTXT *cptr;
    int expdots;

    checkArity(op,args);

    funcall = CADR(args);

    if (TYPEOF(funcall) == EXPRSXP)
	funcall = VECTOR(funcall)[0];

    if (TYPEOF(funcall) != LANGSXP) {
	b = deparse1(funcall, 1);
	errorcall(call, "%s is not a valid call\n", CHAR(STRING(b)[0]));
    }

    /* Get the function definition */
    sysp = R_GlobalContext->sysparent;

    if (TYPEOF(CAR(args)) == NILSXP) {
	/* Get the env that the function containing */
	/* matchcall was called from. */
	cptr = R_GlobalContext;
	while (cptr != NULL) {
	    if (cptr->callflag == CTXT_RETURN && cptr->cloenv == sysp)
		break;
	    cptr = cptr->nextcontext;
	}
	if ( cptr == NULL )
	    sysp = R_GlobalEnv;
	else
	    sysp = cptr->sysparent;
	if ( TYPEOF(CAR(funcall)) == SYMSXP )
	    PROTECT(b = findFun(CAR(funcall), sysp));
	else
	    PROTECT(b = eval(CAR(funcall), sysp));
    }
    else PROTECT(b = CAR(args));

    /* It must be a closure! */

    if (TYPEOF(b) != CLOSXP) {
	b = deparse1(b, 1);
	errorcall(call, "%s is not a function\n", CHAR(STRING(b)[0]));
    }

    /* Do we expand ... ? */

    expdots = asLogical(CAR(CDDR(args)));
    if (expdots == NA_LOGICAL) {
	b = deparse1(CADDR(args), 1);
	errorcall(call, "%s is not a logical\n", CHAR(STRING(b)[0]));
    }

    /* Get the formals and match the actual args */

    formals = FORMALS(b);
    PROTECT(actuals = duplicate(CDR(funcall))); 

    /* If there is a ... symbol then expand it out in the sysp env
       We need to take some care since the ... might be in the middle
       of the actuals  */

    t2 = R_MissingArg;
    for (t1=actuals ; t1!=R_NilValue ; t1 = CDR(t1) ) {
	if (CAR(t1) == R_DotsSymbol) {
		t2 = subDots(sysp);
		break;
#ifdef NEWDOTS
		t2 = findVarInFrame( sysp, R_DotsSymbol);
		b = allocList(length(t2));
		for(f=b; f!=R_NilValue; f=CDR(f)) {
			CAR(b)=PREXPR(CAR(t2));
			t2 = CDR(t2);
		}
#endif
	}
    }
    /* now to splice t2 into the correct spot in actuals */
    if (t2 != R_MissingArg ) {  /* so we did something above */
	if( CAR(actuals) == R_DotsSymbol )
		actuals = listAppend(t2, CDR(actuals));
	else {
		for(t1=actuals; t1!=R_NilValue; t1=CDR(t1)) {
			if( CADR(t1) == R_DotsSymbol ) {
				tail = CDDR(t1);
				CDR(t1) = t2;
				listAppend(actuals,tail);
				break;
			}
		}
	}
    }

    rlist = matchArgs(formals, actuals);

    /* Attach the argument names as tags */

    for (f = formals, b = rlist; b != R_NilValue; b = CDR(b), f = CDR(f)) {
	TAG(b) = TAG(f);
    }


    /* Handle the dots */

    PROTECT(rlist = ExpandDots(rlist, expdots));

    /* Eliminate any unmatched formals and any that match R_DotSymbol */
    /* This needs to be after ExpandDots as the DOTSXP might match ... */

    rlist = StripUnmatched(rlist); 

    PROTECT(rval = allocSExp(LANGSXP));
    CAR(rval) = duplicate(CAR(funcall));
    CDR(rval) = rlist;
    UNPROTECT(4);
    return rval;
}
