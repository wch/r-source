/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2001  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

SEXP ScalarLogical(int x)
{
    SEXP ans = allocVector(LGLSXP, 1);
    INTEGER(ans)[0] = x;
    return ans;
}

SEXP ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = x;
    return ans;
}

SEXP ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, 1);
    REAL(ans)[0] = x;
    return ans;
}

SEXP ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, 1);
    COMPLEX(ans)[0] = x;
    return ans;
}

SEXP ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, 1);
    SET_STRING_ELT(ans, 0, x);
    UNPROTECT(1);
    return ans;
}

static char *truenames[] = {
    "T",
    "True",
    "TRUE",
    "true",
    (char *) 0,
};

static char *falsenames[] = {
    "F",
    "False",
    "FALSE",
    "false",
    (char *) 0,
};

/* int, not Rboolean, for NA_LOGICAL : */
int asLogical(SEXP x)
{
    if (isVectorAtomic(x)) {
	if (LENGTH(x) < 1)
	    return NA_LOGICAL;
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return LOGICAL(x)[0];
	case INTSXP:
	    return (INTEGER(x)[0] == NA_INTEGER) ?
		NA_LOGICAL : (INTEGER(x)[0]) != 0;
	case REALSXP:
	    return R_FINITE(REAL(x)[0]) ?
		(REAL(x)[0] != 0.0) : NA_LOGICAL;
	case CPLXSXP:
	    return R_FINITE(COMPLEX(x)[0].r) ?
		(COMPLEX(x)[0].r != 0.0) : NA_LOGICAL;
	}
    }
    return NA_LOGICAL;
}

int asInteger(SEXP x)
{
    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return (LOGICAL(x)[0] == NA_LOGICAL) ?
		NA_INTEGER : ((LOGICAL(x)[0]) != 0);
	case INTSXP:
	    return (INTEGER(x)[0]);
	case REALSXP:
	    return R_FINITE(REAL(x)[0]) ?
		((int)(REAL(x)[0])) : NA_INTEGER;
	case CPLXSXP:
	    return R_FINITE(COMPLEX(x)[0].r) ?
		((int)(COMPLEX(x)[0].r)) : NA_INTEGER;
	}
    }
    return NA_INTEGER;
}

double asReal(SEXP x)
{
    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    return (INTEGER(x)[0] == NA_INTEGER) ?
		NA_REAL : (INTEGER(x)[0]);
	case REALSXP:
	    return REAL(x)[0];
	case CPLXSXP:
	    return COMPLEX(x)[0].r;
	}
    }
    return NA_REAL;
}


Rcomplex asComplex(SEXP x)
{
    Rcomplex z;
    z.r = NA_REAL;
    z.i = NA_REAL;
    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    if (INTEGER(x)[0] != NA_INTEGER) {
		z.r = INTEGER(x)[0];
		z.i = 0;
	    }
	    return z;
	case REALSXP:
	    if (REAL(x)[0] != NA_REAL) {
		z.r = REAL(x)[0];
		z.i = 0;
	    }
	    return z;
	case CPLXSXP:
	    return COMPLEX(x)[0];
	}
    }
    return z;
}

SEXP asChar(SEXP x)
{
    int w, d, e, wi, di, ei;
    char buf[MAXELTSIZE];

    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    if (LOGICAL(x)[0] == NA_LOGICAL)
		return NA_STRING;
	    if (LOGICAL(x)[0])
		sprintf(buf, "T");
	    else
		sprintf(buf, "F");
	    return mkChar(buf);
	case INTSXP:
	    if (INTEGER(x)[0] == NA_INTEGER)
		return NA_STRING;
	    sprintf(buf, "%d", INTEGER(x)[0]);
	    return mkChar(buf);
	case REALSXP:
	    formatReal(REAL(x), 1, &w, &d, &e);
#ifdef OLD
	    if (e)
		sprintf(buf, "%*.*e", w, d, REAL(x)[0]);
	    else
		sprintf(buf, "%*.*f", w, d, REAL(x)[0]);
	    return mkChar(buf);
#else
	    return mkChar(EncodeReal(REAL(x)[0], w, d, e));
#endif
        case CPLXSXP:
	    formatComplex(COMPLEX(x), 1, &w, &d, &e, &wi, &di, &ei);
	    return mkChar(EncodeComplex(COMPLEX(x)[0], w, d, e, wi, di, ei));
	case STRSXP:
	    return STRING_ELT(x, 0);
	default:
	    return NA_STRING;
	}
    }
    return NA_STRING;
}


static char type_msg[] = "invalid type passed to internal function\n";


void internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    errorcall(call, type_msg);
	else
	    error(type_msg);
    }
}

Rboolean isValidString(SEXP x)
{
    return isString(x) && LENGTH(x) > 0 && !isNull(STRING_ELT(x, 0));
}

/* non-empty ("") valid string :*/
Rboolean isValidStringF(SEXP x)
{
    return isValidString(x) && CHAR(STRING_ELT(x, 0))[0];
}

Rboolean isSymbol(SEXP s)
{
    return TYPEOF(s) == SYMSXP;
}


Rboolean isUserBinop(SEXP s)
{
    if (isSymbol(s)) {
	char *str = CHAR(PRINTNAME(s));
	if (str[0] == '%' && str[strlen(str)-1] == '%')
	    return TRUE;
    }
    return FALSE;
}


Rboolean isNull(SEXP s)
{
    return (s == R_NilValue ||
	    (TYPEOF(s) == EXPRSXP && LENGTH(s) == 0));
}


Rboolean isFunction(SEXP s)
{
    return (TYPEOF(s) == CLOSXP ||
	    TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}


Rboolean isList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}


Rboolean isNewList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == VECSXP);
}

Rboolean isPairList(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

Rboolean isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

Rboolean isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

Rboolean isVector(SEXP s)/* === isVectorList() or isVectorAtomic() */
{
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:

    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}



Rboolean isFrame(SEXP s)
{
    SEXP class;
    int i;
    if (isObject(s)) {
	class = getAttrib(s, R_ClassSymbol);
	for (i = 0; i < length(class); i++)
	    if (!strcmp(CHAR(STRING_ELT(class, i)), "data.frame")) return TRUE;
    }
    return FALSE;
}


Rboolean isEnvironment(SEXP s)
{
    return (TYPEOF(s) == NILSXP || TYPEOF(s) == ENVSXP);
}


Rboolean isExpression(SEXP s)
{
    return TYPEOF(s) == EXPRSXP;
}


Rboolean isLanguage(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LANGSXP);
}


Rboolean isMatrix(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
	    return TRUE;
    }
    return FALSE;
}


Rboolean isArray(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
	    return TRUE;
    }
    return FALSE;
}


Rboolean isTs(SEXP s)
{
    return (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue);
}


Rboolean tsConform(SEXP x, SEXP y)
{
    if ((x = getAttrib(x, R_TspSymbol)) != R_NilValue &&
	(y = getAttrib(y, R_TspSymbol)) != R_NilValue)
	return INTEGER(x)[0] == INTEGER(x)[0] &&
	    INTEGER(x)[1] == INTEGER(x)[1] &&
	    INTEGER(x)[2] == INTEGER(x)[2];
    return FALSE;
}


/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */

Rboolean isVectorizable(SEXP s)
{
    int mode = 0;
    if (isNull(s)) {
	return TRUE;
    }
    else if (isNewList(s)) {
	int i, n;
	n = LENGTH(s);
	for (i = 0 ; i < n; i++) {
	    if (!isVector(VECTOR_ELT(s, i)) || LENGTH(VECTOR_ELT(s, i)) > 1)
		return FALSE;
	    mode = (mode >= TYPEOF(VECTOR_ELT(s, i))) ?
		mode : TYPEOF(VECTOR_ELT(s, i));
	}
	return mode;
    }
    else if (isList(s)) {
	for ( ; s != R_NilValue; s = CDR(s)) {
	    if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1)
		return FALSE;
	    mode = (mode >= (int) TYPEOF(CAR(s))) ? mode : TYPEOF(CAR(s));
	}
	return mode;
    }
    else return FALSE;
}


/* Check to see if the arrays "x" and "y" have the identical extents */

Rboolean conformable(SEXP x, SEXP y)
{
    int i, n;
    PROTECT(x = getAttrib(x, R_DimSymbol));
    y = getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = length(x)) != length(y))
	return FALSE;
    for (i = 0; i < n; i++)
	if (INTEGER(x)[i] != INTEGER(y)[i])
	    return FALSE;
    return TRUE;
}


int nrows(SEXP s)
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (t == R_NilValue) return LENGTH(s);
	return INTEGER(t)[0];
    }
    else if (isFrame(s)) {
	return nrows(CAR(s));
    }
    else error("object is not a matrix");
    return -1;
}


int ncols(SEXP s)
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (t == R_NilValue) return 1;
	return INTEGER(t)[1];
    }
    else if (isFrame(s)) {
	return length(s);
    }
    else error("object is not a matrix");
    return -1;/*NOTREACHED*/
}


int nlevels(SEXP f)
{
    if (!isFactor(f))
	return 0;
    return LENGTH(getAttrib(f, R_LevelsSymbol));
}

/* Is an object of numeric type. */
/* FIXME:  the LGLSXP case should be excluded here. */

Rboolean isNumeric(SEXP s)
{
    if (inherits(s,"factor")) return FALSE;

    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

Rboolean isString(SEXP s)
{
    return (TYPEOF(s) == STRSXP);
}


Rboolean isLogical(SEXP s)
{
    return (TYPEOF(s) == LGLSXP);
}


Rboolean isInteger(SEXP s)
{
    return (TYPEOF(s) == INTSXP && !inherits(s, "factor"));
}


Rboolean isReal(SEXP s)
{
    return (TYPEOF(s) == REALSXP);
}


Rboolean isComplex(SEXP s)
{
    return (TYPEOF(s) == CPLXSXP);
}


Rboolean isUnordered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && !inherits(s, "ordered"));
}


Rboolean isOrdered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && inherits(s, "ordered"));
}


Rboolean isFactor(SEXP s)
{
    return (TYPEOF(s) == INTSXP  && inherits(s, "factor"));
}


Rboolean isObject(SEXP s)
{
    return OBJECT(s);/* really `1-bit unsigned int' */
}


Rboolean inherits(SEXP s, char *name)
{
    SEXP class;
    int i, nclass;
    if (isObject(s)) {
	class = getAttrib(s, R_ClassSymbol);
	nclass = length(class);
	for (i = 0; i < nclass; i++) {
	    if (!strcmp(CHAR(STRING_ELT(class, i)), name))
		return TRUE;
	}
    }
    return FALSE;
}


#ifdef neverUser
double realNA()
{
    return NA_REAL;
}
#endif

static struct {
    char *str;
    int type;
}
TypeTable[] = {
    { "NULL",		NILSXP	   },  /* real types */
    { "symbol",		SYMSXP	   },
    { "pairlist",	LISTSXP	   },
    { "closure",	CLOSXP	   },
    { "environment",	ENVSXP	   },
    { "promise",	PROMSXP	   },
    { "language",	LANGSXP	   },
    { "special",	SPECIALSXP },
    { "builtin",	BUILTINSXP },
    { "char",		CHARSXP	   },
    { "logical",	LGLSXP	   },
    { "integer",	INTSXP	   },
    { "double",		REALSXP	   }, /*-  "real", for R <= 0.61.x */
    { "complex",	CPLXSXP	   },
    { "character",	STRSXP	   },
    { "...",		DOTSXP	   },
    { "any",		ANYSXP	   },
    { "expression",	EXPRSXP	   },
    { "list",		VECSXP	   },
    { "externalptr",	EXTPTRSXP  },
    /* aliases : */
    { "numeric",	REALSXP	   },
    { "name",		SYMSXP	   },

    { (char *)0,	-1	   }
};


SEXPTYPE str2type(char *s)
{
    int i;
    for (i = 0; TypeTable[i].str; i++) {
	if (!strcmp(s, TypeTable[i].str))
	    return TypeTable[i].type;
    }
    return -1;
}


SEXP type2str(SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return mkChar(TypeTable[i].str);
    }
    UNIMPLEMENTED("type2str");
    return R_NilValue; /* for -Wall */
}

Rboolean isBlankString(char *s)
{
    while (*s)
	if (!isspace((int)*s++)) return FALSE;
    return TRUE;
}

Rboolean StringBlank(SEXP x)
{
    if (x == R_NilValue) return TRUE;
    else return CHAR(x)[0] == '\0';
}

/* Function to test whether a string is a true value */

Rboolean StringTrue(char *name)
{
    int i;
    for (i = 0; truenames[i]; i++)
	if (!strcmp(name, truenames[i]))
	    return TRUE;
    return FALSE;
}

Rboolean StringFalse(char *name)
{
    int i;
    for (i = 0; falsenames[i]; i++)
	if (!strcmp(name, falsenames[i]))
	    return TRUE;
    return FALSE;
}

SEXP EnsureString(SEXP s)
{
    switch(TYPEOF(s)) {
    case SYMSXP:
	s = PRINTNAME(s);
	break;
    case STRSXP:
	s = STRING_ELT(s, 0);
	break;
    case CHARSXP:
	break;
    case NILSXP:
	s = R_BlankString;
	break;
    default:
	error("invalid tag in name extraction");
    }
    return s;
}


void checkArity(SEXP op, SEXP args)
{
    if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args))
	error("%d argument%s passed to \"%s\" which requires %d.",
	      length(args), (length(args) == 1 ? "" : "s"),
	      PRIMNAME(op), PRIMARITY(op));
}


SEXP nthcdr(SEXP s, int n)
{
    if (isList(s) || isLanguage(s) || isFrame(s) || TYPEOF(s) == DOTSXP ) {
	while( n-- > 0 ) {
	    if (s == R_NilValue)
		error("\"nthcdr\" list shorter than %d", n);
	    s = CDR(s);
	}
	return s;
    }
    else error("\"nthcdr\" needs a list to CDR down");
    return R_NilValue;/* for -Wall */
}


SEXP do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP t;
    RCNTXT *cptr;
    int nargs = NA_INTEGER;
    for (cptr = R_GlobalContext; cptr != NULL; cptr = cptr->nextcontext) {
	if ((cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == rho) {
	    nargs = length(cptr->promargs);
	    break;
	}
    }
    t = allocVector(INTSXP, 1);
    *INTEGER(t) = nargs;
    return (t);
}


void setIVector(int * vec, int len, int val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


void setRVector(double * vec, int len, double val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


void setSVector(SEXP * vec, int len, SEXP val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


Rboolean isFree(SEXP val)
{
    SEXP t;
    for (t = R_FreeSEXP; t != R_NilValue; t = CAR(t))
	if (val == t)
	    return TRUE;
    return FALSE;
}


/* Debugging functions (hence the d-prefix). */
/* These are intended to be called interactively from */
/* a debugger such as gdb, so you don't have to remember */
/* the names of the data structure components. */

int dtype(SEXP q)
{
    return((int)TYPEOF(q));
}


SEXP dcar(SEXP l)
{
    return(CAR(l));
}


SEXP dcdr(SEXP l)
{
    return(CDR(l));
}

SEXP do_merge(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP xi, yi, ansx, ansy, ans, ansnames;
    int y, nx = 0, ny = 0, i, j, k, nans = 0;

    checkArity(op, args);
    xi = CAR(args);
    if ( !isInteger(xi) || !(nx = LENGTH(xi)) )
	error("invalid `xinds' argument");
    yi = CADR(args);
    if ( !isInteger(yi) || !(ny = LENGTH(yi)) )
	error("invalid `yinds' argument");
    for (j = 0; j < ny; j++)
	if ((y = INTEGER(yi)[j]) > 0)
	    for (i = 0; i < nx; i++) {
		if (INTEGER(xi)[i] == y) nans++;
	    }
    PROTECT(ans = allocVector(VECSXP, 2));
    ansx = allocVector(INTSXP, nans);
    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);
    SET_VECTOR_ELT(ans, 1, ansy);
    for (j = 0, k = 0; j < ny; j++)
	if ((y = INTEGER(yi)[j]) > 0)
	    for (i = 0; i < nx; i++) 
		if (INTEGER(xi)[i] == y) {
		INTEGER(ansx)[k]   = i + 1;
		INTEGER(ansy)[k++] = j + 1;
	    }
    PROTECT(ansnames = allocVector(STRSXP, 2));
    SET_STRING_ELT(ansnames, 0, mkChar("xi"));
    SET_STRING_ELT(ansnames, 1, mkChar("yi"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


/* Functions for getting and setting the working directory. */
#ifdef Win32
#include <windows.h>
#endif

SEXP do_getwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval = R_NilValue;
    char buf[2 * PATH_MAX];

    checkArity(op, args);

#ifdef R_GETCWD
    R_GETCWD(buf, PATH_MAX);
    rval = mkString(buf);
#endif
    return(rval);
}

#if defined(Win32) && defined(_MSC_VER)
#include <direct.h> /* for chdir */
#endif

SEXP do_setwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;	/* -Wall */
    const char *path;

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	errorcall(call, "character argument expected");
    path = R_ExpandFileName(CHAR(STRING_ELT(s, 0)));
    if(chdir(path) < 0)
	errorcall(call, "cannot change working directory");
    return(R_NilValue);
}

/* remove portion of path before file separator if one exists */

SEXP do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	errorcall(call, "character argument expected");
    strcpy (buf, R_ExpandFileName(CHAR(STRING_ELT(s, 0))));
#ifdef Win32
    for (p = buf; *p != '\0'; p++)
	if (*p == '\\') *p = '/';
#endif
    /* remove trailing file separator(s) */
    while ( *(p = buf + strlen(buf) - 1) == fsp ) *p = '\0';
    if ((p = strrchr(buf, fsp)))
	p++;
    else
	p = buf;
    return(mkString(p));
}

/* remove portion of path after last file separator if one exists, else
   return "."
   */

SEXP do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	errorcall(call, "character argument expected");
    strcpy(buf, R_ExpandFileName(CHAR(STRING_ELT(s, 0))));
#ifdef Win32
    for(p = buf; *p != '\0'; p++)
	if(*p == '\\') *p = '/';
#endif
    /* remove trailing file separator(s) */
    while ( *(p = buf + strlen(buf) - 1) == fsp ) *p = '\0';
    if((p = strrchr(buf, fsp))) {
	*p = '\0';
	/* remove excess trailing file separator(s), as in /a///b  */
	while ( *(--p) == fsp ) *p = '\0';
    } else
	strcpy(buf, ".");
    return(mkString(buf));
}
