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

/* Code to handle list / vector switch */

#ifdef NEWLIST
#define LIST_ASSIGN(x) {VECTOR(ans_ptr)[ans_length] = x; ans_length++;}
#define LIST_MODE VECSXP
#else
#define LIST_ASSIGN(x) {CAR(ans_ptr) = x; ans_ptr = CDR(ans_ptr);}
#define LIST_MODE LISTSXP
#endif


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
    if(isVector(x)) {
	if (!isNull(getAttrib(x, R_NamesSymbol)))
	    return 1;
    }
    else if(isList(x)) {
	while(!isNull(x)) {
	    if(!isNull(TAG(x))) return 1;
	    x = CDR(x);
	}
    }
    return 0;
}

static void AnswerType(SEXP x, int recurse, int usenames)
{
    switch (TYPEOF(x)) {
    case NILSXP:
	break;
    case LGLSXP:
	ans_flags |= 1;
	ans_length += LENGTH(x);
	break;
    case INTSXP:
	ans_flags |= 8;
	ans_length += LENGTH(x);
	break;
    case REALSXP:
	ans_flags |= 16;
	ans_length += LENGTH(x);
	break;
    case CPLXSXP:
	ans_flags |= 32;
	ans_length += LENGTH(x);
	break;
    case STRSXP:
	ans_flags |= 64;
	ans_length += LENGTH(x);
	break;
#ifdef NEWLIST
    case VECSXP:
	if(recurse) {
	    int i, n;
	    n = length(x);
	    if(usenames && !ans_names && !isNull(getAttrib(x, R_NamesSymbol)))
		ans_nnames = 1;
	    for(i = 0 ; i < n ; i++) {
		if (usenames && !ans_nnames)
		    ans_nnames = HasNames(VECTOR(x)[i]);
		AnswerType(VECTOR(x)[i], recurse, usenames);
	    }
	}
	else {
	    ans_flags |= 128;
	    ans_length += length(x);
	}
	break;
#endif
    case LISTSXP:
	if(recurse) {
	    while(x != R_NilValue) {
		if (usenames && !ans_nnames) {
		    if (!isNull(TAG(x))) ans_nnames = 1;
		    else ans_nnames = HasNames(CAR(x));
		}
		AnswerType(CAR(x), recurse, usenames);
		x = CDR(x);
	    }
	}
	else {
	    ans_flags |= 128;
	    ans_length += length(x);
	}
	break;
    default:
	ans_flags |= 128;
	ans_length += 1;
	break;
    }
}

static void answertype(SEXP x, int recurse, int usenames)
{
    while (x != R_NilValue) {
	if (usenames && !ans_nnames) {
	    if (!isNull(TAG(x))) ans_nnames = 1;
	    else ans_nnames = HasNames(CAR(x));
	}
	AnswerType(CAR(x), recurse, usenames);
	x = CDR(x);
    }
}


/* The following functions are used to coerce arguments to */
/* the appropriate type for inclusion in the returned value. */

static void ListAnswer(SEXP x, int recurse)
{
    int i;

    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LGLSXP:
	for(i = 0 ; i < LENGTH(x) ; i++)
	    LIST_ASSIGN(ScalarLogical(LOGICAL(x)[i]));
	break;
    case INTSXP:
	for(i = 0 ; i < LENGTH(x) ; i++)
	    LIST_ASSIGN(ScalarInteger(INTEGER(x)[i]));
	break;
    case REALSXP:
	for(i = 0 ; i < LENGTH(x) ; i++)
	    LIST_ASSIGN(ScalarReal(REAL(x)[i]));
	break;
    case CPLXSXP:
	for(i = 0 ; i < LENGTH(x) ; i++)
	    LIST_ASSIGN(ScalarComplex(COMPLEX(x)[i]));
	break;
    case STRSXP:
	for(i = 0 ; i < LENGTH(x) ; i++)
	    LIST_ASSIGN(ScalarString(STRING(x)[i]));
	break;
    case VECSXP:
	if (recurse) {
	    for(i = 0 ; i < LENGTH(x) ; i++)
		ListAnswer(VECTOR(x)[i], recurse);
	}
	else {
	    for(i = 0 ; i < LENGTH(x) ; i++)
		LIST_ASSIGN(duplicate(VECTOR(x)[i]));
	}
	break;
    case LISTSXP:
	if(recurse) {
	    while(x != R_NilValue) {
		ListAnswer(CAR(x), recurse);
		x = CDR(x);
	    }
	}
	else {
	    while(x != R_NilValue) {
		LIST_ASSIGN(duplicate(CAR(x)));
		x = CDR(x);
	    }
	}
	break;
    default:
	LIST_ASSIGN(duplicate(x));
	break;
    }
}

static void StringAnswer(SEXP x)
{
    int i, n;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while(x != R_NilValue) {
	    StringAnswer(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    StringAnswer(VECTOR(x)[i]);
	break;
    default:
	PROTECT(x = coerceVector(x, STRSXP));
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    STRING(ans_ptr)[ans_length++] = STRING(x)[i];
	UNPROTECT(1);
	break;
    }
}

static void IntegerAnswer(SEXP x)
{
    int i, n;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while(x != R_NilValue) {
	    IntegerAnswer(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    IntegerAnswer(VECTOR(x)[i]);
	break;
    default:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    INTEGER(ans_ptr)[ans_length++] = INTEGER(x)[i];
	break;
    }
}

static void RealAnswer(SEXP x)
{
    int i, n, xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while(x != R_NilValue) {
	    RealAnswer(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    RealAnswer(VECTOR(x)[i]);
	break;
    case REALSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    REAL(ans_ptr)[ans_length++] = REAL(x)[i];
	break;
    default:
	n = LENGTH(x);
	for (i = 0; i < n; i++) {
	    xi = INTEGER(x)[i];
	    if(xi == NA_INTEGER)
		REAL(ans_ptr)[ans_length++] = NA_REAL;
	    else REAL(ans_ptr)[ans_length++] = xi;
	}
	break;
    }
}

static void ComplexAnswer(SEXP x)
{
    int i, n, xi;
    switch(TYPEOF(x)) {
    case NILSXP:
	break;
    case LISTSXP:
	while(x != R_NilValue) {
	    ComplexAnswer(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    ComplexAnswer(VECTOR(x)[i]);
	break;
    case REALSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++) {
	    COMPLEX(ans_ptr)[ans_length].r = REAL(x)[i];
	    COMPLEX(ans_ptr)[ans_length].i = 0.0;
	    ans_length++;
	}
	break;
    case CPLXSXP:
	n = LENGTH(x);
	for (i = 0; i < n; i++)
	    COMPLEX(ans_ptr)[ans_length++] = COMPLEX(x)[i];
	break;
    default:
	n = LENGTH(x);
	for (i = 0; i < n; i++) {
	    xi = INTEGER(x)[i];
	    if(xi == NA_INTEGER)
		REAL(ans_ptr)[ans_length++] = NA_REAL;
	    else REAL(ans_ptr)[ans_length++] = xi;
	}
	break;
    }
}


/* TagName manufactures names for elements of the answer.  Note that */
/* TagName can get either a SYMSXP, a STRSXP or a CHARSXP or NILSXP */
/* for either tag or base; the first part extracts the CHARSXP needed; */
/* the remainer acts on that. */

static SEXP TagName(SEXP tag, SEXP base, int i)
{
    SEXP ans, t;

    PROTECT(t = mkChar(""));

    switch(TYPEOF(tag)) {
    case SYMSXP:
	tag = PRINTNAME(tag);
	break;
    case STRSXP:
	tag = STRING(tag)[0];
	break;
    case CHARSXP:
	break;
    case NILSXP:
	tag = t;
	break;
    default:
	error("invalid tag argument to TagName\n");
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
	error("invalid base argument to TagName\n");
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

static SEXP blank;      /* defined on entry into ExtractNames */
static int offset;

static void ExtractNames(SEXP args, int recurse, int check, SEXP base);

static void ExtractVectorNames(SEXP v, SEXP tag, SEXP base)
{
    int i;
    SEXP s;
    if (!isNull(tag)) {
	if(isNull(s = getAttrib(v, R_NamesSymbol))) {
	    switch(length(v)) {
	    case 0:
		break;
	    case 1:
		STRING(ans_names)[ans_nnames++] = TagName(tag, base, offset);
		break;
	    default:
		for(i=0 ; i<length(v) ; i++)
		    STRING(ans_names)[ans_nnames++] 
			= TagName(tag, base, i+1+offset);
	    }
	}
	else {
	    base = TagName(tag, base, 0);
	    for(i=0 ; i<length(v) ; i++)
		STRING(ans_names)[ans_nnames++] 
		    = TagName(STRING(s)[i], base, offset);
	}
    }
    else {
	if(base == R_NilValue) {
	    if(isNull(s = getAttrib(v, R_NamesSymbol))) {
		for(i=0 ; i<length(v) ; i++)
		    STRING(ans_names)[ans_nnames++] = blank;
	    }
	    else {
		for(i=0 ; i<length(v) ; i++)
		    STRING(ans_names)[ans_nnames++] = STRING(s)[i];
	    }
	} else {
	    if(isNull(s = getAttrib(v, R_NamesSymbol))) {
		for(i=0 ; i<length(v) ; i++)
		    STRING(ans_names)[ans_nnames++] 
			= TagName(base, R_NilValue, i+1+offset);
	    } else {
		for(i=0 ; i<length(v) ; i++)
		    STRING(ans_names)[ans_nnames++] 
			= TagName(STRING(s)[i], base, offset);
	    }
	    offset+=i;
	}
    }
}


static void ExtractListNames(SEXP l, SEXP tag, int recurse, SEXP base)
{
    int i;
    if (!isNull(base))
	base = TagName(tag, base, 0);
    else
	base = tag;
    if(recurse) {
	ExtractNames(l, recurse, 0, base);
    }
    else {
	for (i = 1 ; l != R_NilValue ; i++, l = CDR(l)) {
	    if(isNull(base)) {
		if(!isNull(TAG(l)))
		    STRING(ans_names)[ans_nnames++] = TagName(TAG(l),base,0);
		else
		    STRING(ans_names)[ans_nnames++] = blank;
	    }
	    else {
		if(!isNull(TAG(l)))
		    STRING(ans_names)[ans_nnames++] = TagName(TAG(l),base,0);
		else
		    STRING(ans_names)[ans_nnames++] = TagName(TAG(l),base,i);
	    }
	}
    }
}


/* Since ExtractNames proceeds recursively and it contains a check */
/* on the names found, there must be some mechanism for saying not */
/* to check to them; hence check. */

static void ExtractNames(SEXP args, int recurse, int check, SEXP base)
{
    PROTECT(blank = mkChar(""));
    offset = 0;
#ifdef NEWLIST
    if (isVectorList(args)) {
	SEXP names, namei;
	int i, n;
	n = length(args);
	names = getAttrib(args, R_NamesSymbol);
	for (i = 0 ; i < n ; i++) {

	    if (isNull(names)) namei = R_NilValue;
	    else namei = STRING(names)[i];

	    if(isNull(VECTOR(args)[i])) {
		continue;
	    }
	    else if(isVector(VECTOR(args)[i])) {
		ExtractVectorNames(VECTOR(args)[i], namei, base);
	    }
	    else if(isList(VECTOR(args)[i])) {
		ExtractListNames(VECTOR(args)[i], namei, recurse, base);
	    }
	    else { /* neither vector nor list */
		if(!isNull(namei))
		    STRING(ans_names)[ans_nnames++] = namei;
		else
		    STRING(ans_names)[ans_nnames++] = blank;
	    }
	} /* for */
    }
    else
#endif
    for ( ; args != R_NilValue ; args = CDR(args) ) {

	if(isNull(CAR(args)))
	    continue;

	if(isVector(CAR(args))) {
	    ExtractVectorNames(CAR(args), TAG(args), base);
	}   
	else if(isList(CAR(args))) { 
	    ExtractListNames(CAR(args), TAG(args), recurse, base);
	}
	else { /* neither	 Vector	 nor  List */
	    if(!isNull(TAG(args)))
		STRING(ans_names)[ans_nnames++] = PRINTNAME(TAG(args));
	    else
		STRING(ans_names)[ans_nnames++] = blank;
	}
    } /* for */

    if( check && ans_nnames != ans_length) {
	printf("INTERNAL ERROR: ans_nnames = %d	   ans_length = %d\n",
	       ans_nnames, ans_length);
	error("incorrect names vector length\n");
    }
    UNPROTECT(1);
}

/* Code to extract the optional arguments to c() and unlist(). */

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


/* The change to lists based on dotted pairs has meant that it was */
/* necessary to separate the internal code for "c" and "unlist". */
/* Although the functions are quite similar, they operate on very */
/* different data structures. */

/* The major difference between the two functions is that the value of */
/* the "recursive" argument is FALSE by default for "c" and TRUE for */
/* "unlist".  In addition, "list" takes ... while "unlist" takes a single */
/* argument, and unlist has two optional arguments, while list has none. */

SEXP do_c(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, t;
    int mode, recurse, usenames;

    checkArity(op, args);

    /* Attempt method dispatch. */

    if(DispatchOrEval(call, op, args, env, &ans, 1)) {
	R_Visible = 1;
	return(ans);
    }
    R_Visible = 1;

    /* Method dispatch has failed; run the default code. */
    /* By default we do not recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    usenames = 1;
    recurse = 0;
    if(length(args) > 1)
	PROTECT(args = ExtractOptionals(ans, &recurse, &usenames));
    else
	PROTECT(args = ans);

    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a pair based list. */

    ans_flags  = 0;
    ans_length = 0;
    ans_nnames = 0;

#ifdef OLD
    answertype(args, recurse, usenames);
#else
    for (t = args ; t != R_NilValue ; t = CDR(t)) {
	if (usenames && !ans_nnames) {
	    if (!isNull(TAG(t))) ans_nnames = 1;
	    else ans_nnames = HasNames(CAR(t));
	}
	AnswerType(CAR(t), recurse, usenames);
    }
#endif

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive is FALSE) then we must return a list.  Otherwise, */
    /* we use the natural coercion for vector types. */

    mode = NILSXP;
    if (ans_flags & 128) mode = LIST_MODE;
    else if (ans_flags & 64) mode = STRSXP;
    else if (ans_flags & 32) mode = CPLXSXP;
    else if (ans_flags & 16) mode = REALSXP;
    else if (ans_flags &  8) mode = INTSXP;
    else if (ans_flags &  1) mode = LGLSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = allocVector(mode, ans_length));
    ans_ptr = ans;
    ans_length = 0;
    t = args;

    if (mode == LIST_MODE) {
	if(!recurse) {
	    while(args != R_NilValue) {
		ListAnswer(CAR(args), 0);
		args = CDR(args);
	    }
	}
	else ListAnswer(args, recurse);
	ans_length = length(ans);
    }
    else if(mode == STRSXP)
	StringAnswer(args);
    else if(mode == CPLXSXP)
	ComplexAnswer(args);
    else if(mode == REALSXP)
	RealAnswer(args);
    else
	IntegerAnswer(args);
    args = t;

    /* Build and attach the names attribute for the returned object. */

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


SEXP do_unlist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, t;
    int mode, recurse, usenames;
    int i, n;

    checkArity(op, args);

    /* Attempt method dispatch. */

    if(DispatchOrEval(call, op, args, env, &ans, 1)) {
	R_Visible = 1;
	return(ans);
    }
    R_Visible = 1;

    /* Method dispatch has failed; run the default code. */
    /* By default we recurse, but this can be over-ridden */
    /* by an optional "recursive" argument. */

    usenames = 1;
    recurse = 1;
    if(length(args) > 1)
	PROTECT(args = ExtractOptionals(ans, &recurse, &usenames));
    else
	PROTECT(args = ans);
    args = CAR(args);
    
    /* Determine the type of the returned value. */
    /* The strategy here is appropriate because the */
    /* object being operated on is a generic vector. */

    ans_flags  = 0;
    ans_length = 0;
    ans_nnames = 0;

#ifdef NEWLIST
    if(isNewList(args)) {
	n = length(args);
	if (usenames && getAttrib(args, R_NamesSymbol) != R_NilValue)
	    ans_nnames = 1;
	for (i = 0 ; i < n ; i++) {
	    if(usenames && !ans_nnames)
		ans_nnames = HasNames(VECTOR(args)[i]);
	    AnswerType(VECTOR(args)[i], recurse, usenames);
	}
    }
    else
#endif
    if(isList(args)) {
	for (t = args ; t != R_NilValue ; t = CDR(t)) {
	    if (usenames && !ans_nnames) {
		if (!isNull(TAG(t))) ans_nnames = 1;
		else ans_nnames = HasNames(CAR(t));
	    }
	    AnswerType(CAR(t), recurse, usenames);
	}
    }
    else {
	UNPROTECT(1);
	if(isVector(args)) return args;
	else errorcall(call, "invalid argument \n");
    }

    /* If a non-vector argument was encountered (perhaps a list if */
    /* recursive=F) then we must return a list.  Otherwise, we use */
    /* the natural coercion for vector types. */

    mode = NILSXP;
    if (ans_flags & 128) mode = LIST_MODE;
    else if (ans_flags & 64) mode = STRSXP;
    else if (ans_flags & 32) mode = CPLXSXP;
    else if (ans_flags & 16) mode = REALSXP;
    else if (ans_flags &  8) mode = INTSXP;
    else if (ans_flags &  1) mode = LGLSXP;

    /* Allocate the return value and set up to pass through */
    /* the arguments filling in values of the returned object. */

    PROTECT(ans = allocVector(mode, ans_length));
    ans_ptr = ans;
    ans_length = 0;
    t = args;

    /* FIXME : The following assumes one of pair or vector */
    /* based lists applies.  It needs to handle both */

#ifdef NEWLIST
    if (mode == VECSXP) {
	if(!recurse) {
	    for (i = 0; i < n ; i++)
		ListAnswer(VECTOR(args)[i], 0);
	}
	else ListAnswer(args, recurse);
	ans_length = length(ans);
    }
#else
    if (mode == LISTSXP) {
	if(!recurse) {
	    while(args != R_NilValue) {
		ListAnswer(CAR(args), 0);
		args = CDR(args);
	    }
	}
	else ListAnswer(args, recurse);
	ans_length = length(ans);
    }
#endif
    else if(mode == STRSXP)
	StringAnswer(args);
    else if(mode == CPLXSXP)
	ComplexAnswer(args);
    else if(mode == REALSXP)
	RealAnswer(args);
    else
	IntegerAnswer(args);
    args = t;

    /* Build and attach the names attribute for the returned object. */

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


/* Note: We use substituteList to expand the ... which */
/* is passed down by the wrapper function to cbind */

static SEXP rho;
SEXP substituteList(SEXP, SEXP);

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
	/* FIXME KH 1998/06/23
	   This should obviously do something useful, but
	   currently breaks [cr]bind() if one arg is a df
	   while (a != R_NilValue) {
	       if(t == R_NilValue)
	           errorcall(call, "corrupt data frame args!\n");
	       p = mkPROMISE(CAR(t), rho);
	       PRVALUE(p) = CAR(a);
	       CAR(a) = p;
	       t = CDR(t);
	       a = CDR(a);
	   }
	*/
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
    /* Perform default action */

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

    if (PRIMVAL(op) == 1) 
	a = cbind(call, args, mode);
    else
	a = rbind(call, args, mode);
    return a;
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
		if ( k == 0 )
		    mrows = 0;
		for (i = 0; i < mrows; i++)
		    for (j = 0; j < cols; j++)
			STRING(result)[i + n + (j * rows)] =  STRING(u)[(i + j * mrows) % k] ;
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
