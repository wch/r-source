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
#include "Mathlib.h"
#include "Print.h"

SEXP mkChar(char *);

static char *truenames[] =
{
	"T",
	"True",
	"TRUE",
	"true",
	(char *) 0,
};

static char *falsenames[] =
{
	"F",
	"False",
	"FALSE",
	"false",
	(char *) 0,
};

int asInteger(SEXP x)
{
	if (isVector(x)) {
		if (LENGTH(x) < 1)
			return NA_INTEGER;
		switch (TYPEOF(x)) {
		case LGLSXP:
			return (LOGICAL(x)[0] == NA_LOGICAL) ? NA_INTEGER : ((LOGICAL(x)[0]) != 0);
		case INTSXP:
			return (INTEGER(x)[0]);
		case REALSXP:
			return FINITE(REAL(x)[0]) ? ((int)(REAL(x)[0])) : NA_INTEGER;
		default:
			return NA_INTEGER;
		}
	}
	return NA_INTEGER;
}

int asLogical(SEXP x)
{
	if (isVector(x)) {
		if (LENGTH(x) < 1)
			return NA_INTEGER;
		switch (TYPEOF(x)) {
		case LGLSXP:
			return LOGICAL(x)[0];
		case INTSXP:
			return (INTEGER(x)[0] == NA_INTEGER) ? NA_LOGICAL : (INTEGER(x)[0]) != 0;
		case REALSXP:
			return FINITE(REAL(x)[0]) ? (REAL(x)[0] != 0.0) : NA_LOGICAL;
		default:
			return NA_LOGICAL;
		}
	}
	return NA_LOGICAL;
}

double asReal(SEXP x)
{
	if (isVector(x)) {
		if (LENGTH(x) < 1)
			return NA_INTEGER;
		switch (TYPEOF(x)) {
		case LGLSXP:
		case INTSXP:
			return (INTEGER(x)[0] == NA_INTEGER) ? NA_REAL : (INTEGER(x)[0]);
		case REALSXP:
			return REAL(x)[0];
		default:
			return NA_REAL;
		}
	}
	return NA_REAL;
}

SEXP asChar(SEXP x)
{
	int w, d, e;
	char buf[MAXELTSIZE];

	if (isVector(x)) {
		if (LENGTH(x) < 1)
			return NA_STRING;
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
			if (e)
				sprintf(buf, "%*.*e", w, d, REAL(x)[0]);
			else
				sprintf(buf, "%*.*f", w, d, REAL(x)[0]);
			return mkChar(buf);
		case STRSXP:
			return STRING(x)[0];
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

int isSymbol(SEXP s)
{
	return TYPEOF(s) == SYMSXP;
}

int isUserBinop(SEXP s)
{
	if(isSymbol(s)) {
		char *str = CHAR(PRINTNAME(s));
		if(str[0] == '%' && str[strlen(str)-1] == '%')
			return 1;
	}
	return 0;
}

int isNull(SEXP s)
{
	return s == R_NilValue;
}

int isFunction(SEXP s)
{
	return (TYPEOF(s) == CLOSXP || TYPEOF(s) == BUILTINSXP || TYPEOF(s) == SPECIALSXP);
}

int isList(SEXP s)
{
	return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}

int isFrame(SEXP s)
{
	SEXP class;
	int i;
	if(isObject(s)) {
		class = getAttrib(s, R_ClassSymbol);
		for(i=0 ; i<length(class) ; i++)
			if(!strcmp(CHAR(STRING(class)[i]), "data.frame")) return 1;
	}
	return 0;
}

int isEnvironment(SEXP s)
{
	if(TYPEOF(s) == NILSXP || TYPEOF(s) == ENVSXP)
		return 1;
	else
		return 0;
}

int isExpression(SEXP s)
{
	return TYPEOF(s) == EXPRSXP;
}

int isLanguage(SEXP s)
{
	return (s == R_NilValue || TYPEOF(s) == LANGSXP);
}

int isVector(SEXP s)
{
	switch(TYPEOF(s)) {
	    case LGLSXP:
	    case FACTSXP:
	    case ORDSXP:
	    case INTSXP:
	    case REALSXP:
	    case CPLXSXP:
	    case STRSXP:
	    case EXPRSXP:
		return 1;
		break;
	    default:
		return 0;
		break;
	}
}

int isMatrix(SEXP s)
{
	SEXP t;
	if (isVector(s)) {
		t = getAttrib(s, R_DimSymbol);
		if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
			return 1;
	}
	return 0;
}

int isArray(SEXP s)
{
	SEXP t;
	if (isVector(s)) {
		t = getAttrib(s, R_DimSymbol);
		if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
			return 1;
	}
	return 0;
}

int isTs(SEXP s)
{
	if (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue)
		return 1;
	return 0;
}

int tsConform(SEXP x, SEXP y)
{
	if ((x = getAttrib(x, R_TspSymbol)) != R_NilValue &&
	    (y = getAttrib(y, R_TspSymbol)) != R_NilValue)
		return INTEGER(x)[0] == INTEGER(x)[0] &&
		    INTEGER(x)[1] == INTEGER(x)[1] &&
		    INTEGER(x)[2] == INTEGER(x)[2];
	return 0;
}

int factorsConform(SEXP x, SEXP y)
{
        SEXP xlevels, ylevels;
        int i, n;

        if((isUnordered(x) && isUnordered(y)) || (isOrdered(x) && isOrdered(y)))
 {
                if(LEVELS(x) == LEVELS(y)) {
                        xlevels = getAttrib(x, R_LevelsSymbol);
                        ylevels = getAttrib(y, R_LevelsSymbol);
                        if(xlevels == R_NilValue && ylevels == R_NilValue)
                                return 1;
                        if(xlevels != R_NilValue && ylevels != R_NilValue) {
                                n = LEVELS(x);
                                for(i=0 ; i<n ; i++)
                                        if(strcmp(CHAR(STRING(xlevels)[i]),
                                            CHAR(STRING(ylevels)[i])))
                                                return 0;
                                return 1;
                        }
                }
        }
	return 0;
}

/* check to see if a list can be made into a vector */
/* it must have every elt being a vector of length 1 */

int isVectorizable(SEXP s)
{
	int mode = 0;

	if (isNull(s)) return 1;
	else if (!isList(s)) return 0;
	for ( ; s != R_NilValue; s = CDR(s)) {
		if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1)
			return 0;
		mode = (mode >= (int) TYPEOF(CAR(s))) ? mode : TYPEOF(CAR(s));
	}
	return mode;
}

int conformable(SEXP x, SEXP y)
{
	int i, n;
	PROTECT(x = getAttrib(x, R_DimSymbol));
	y = getAttrib(y, R_DimSymbol);
	UNPROTECT(1);
	if ((n = LENGTH(x)) != LENGTH(y))
		return 0;
	for (i = 0; i < n; i++)
		if (INTEGER(x)[i] != INTEGER(y)[i])
			return 0;
	return 1;
}

int nrows(SEXP s)
{
	SEXP t;
	if(isVector(s) || isList(s)) {
		t = getAttrib(s, R_DimSymbol);
		if(t == R_NilValue) return LENGTH(s);
		return INTEGER(t)[0];
	}
	else if(isFrame(s)) {
		return nrows(CAR(s));
	}
	else error("object is not a matrix\n");
}

int ncols(SEXP s)
{
	SEXP t;
	if(isVector(s) || isList(s)) {
		t = getAttrib(s, R_DimSymbol);
		if(t == R_NilValue) return 1;
		return INTEGER(t)[1];
	}
	else if(isFrame(s)) {
		return length(s);
	}
	else error("object is not a matrix\n");
}

int isNumeric(SEXP s)
{
	switch(TYPEOF(s)) {
		case LGLSXP:
		case INTSXP:
		case REALSXP:
			return 1;
		default:
			return 0;
	}
}

int isString(SEXP s)
{
	if (TYPEOF(s) == STRSXP)
		return 1;
	else
		return 0;
}

int isLogical(SEXP s)
{
	return (TYPEOF(s) == LGLSXP);
}

int isInteger(SEXP s)
{
	return (TYPEOF(s) == INTSXP);
}

int isReal(SEXP s)
{
	return (TYPEOF(s) == REALSXP);
}

#ifdef COMPLEX_DATA
int isComplex(SEXP s)
{
	return (TYPEOF(s) == CPLXSXP);
}
#endif

int isUnordered(SEXP s)
{
	return (TYPEOF(s) == FACTSXP);
}

int isOrdered(SEXP s)
{
	return (TYPEOF(s) == ORDSXP);
}

int isFactor(SEXP s)
{
	return (TYPEOF(s) == FACTSXP || TYPEOF(s) == ORDSXP);
}

int isObject(SEXP s)
{
	return OBJECT(s);
}

int inherits(SEXP s, char *name)
{
	SEXP class;
	int i, nclass;
	if(isObject(s)) {
		class = getAttrib(s, R_ClassSymbol);
		nclass = length(class);
		for(i=0 ; i<nclass ; i++) {
			if(!strcmp(CHAR(STRING(class)[i]), name))
				return 1;
		}
		return 0;
	}
	else return 0;
}

int isFinite(double x)
{
	return FINITE(x);
}
 
double realNA()
{
	return NA_REAL;
}


static struct {
	char *str;
	int type;
}
TypeTable[] = {
	{ "NULL",		NILSXP     },	/* real types */
	{ "symbol",		SYMSXP     },
	{ "list",		LISTSXP    },
	{ "closure",		CLOSXP     },
	{ "environment",	ENVSXP     },
	{ "promise",		PROMSXP    },
	{ "language",		LANGSXP    },
	{ "special",		SPECIALSXP },
	{ "builtin",		BUILTINSXP },
	{ "char",		CHARSXP    },
	{ "logical",		LGLSXP     },
	{ "factor",		FACTSXP    },
	{ "ordered",		ORDSXP     },
	{ "integer",		INTSXP     },
	{ "real",		REALSXP    },
	{ "complex",		CPLXSXP    },
	{ "character",		STRSXP     },
	{ "...",		DOTSXP     },
	{ "any",		ANYSXP     },
	{ "expression",		EXPRSXP    },

	{ "numeric",		REALSXP    },	/* aliases */
	{ "unordered",		FACTSXP    },

	{ (char *)0,            -1         }
};


SEXPTYPE str2type(char *s)
{
	int i;

	for (i=0; TypeTable[i].str ; i++) {
		if (!strcmp(s, TypeTable[i].str))
#ifdef COMPLEX_DATA
			return TypeTable[i].type;
#else
			if (TypeTable[i].type == CPLXSXP)
				error("no complex data in this R version\n");
			else
				return TypeTable[i].type;
#endif
	}
	return -1;
}

SEXP type2str(SEXPTYPE t)
{
	int i;

	for (i=0; TypeTable[i].str ; i++) {
		if (TypeTable[i].type == t)
			return mkChar(TypeTable[i].str);
	}
	UNIMPLEMENTED("type2str");
}

/* function to test whether a string is a true value */

int StringTrue(char *name)
{
	int i;

	for (i = 0; truenames[i]; i++)
		if (!strcmp(name, truenames[i]))
			return (1);
	return (0);
}

int StringFalse(char *name)
{
	int i;

	for (i = 0; falsenames[i]; i++)
		if (!strcmp(name, falsenames[i]))
			return (1);
	return (0);
}

void checkArity(SEXP op, SEXP args)
{
	if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args))
		error("%d argument%s passed to \"%s\" which requires %d.\n",
		      length(args), (length(args) == 1 ? "" : "s"),
		      PRIMNAME(op), PRIMARITY(op));
}

SEXP nthcdr(SEXP s, int n)
{
	if (isList(s) || isLanguage(s) || isFrame(s) || TYPEOF(s) == DOTSXP ) {
		while( n-- > 0 ) {
			if(s == R_NilValue)
				error("\"nthcdr\" list shorter than %d\n", n);
			s = CDR(s);
		}
		return s;
	}
	else error("\"nthcdr\" need a list to CDR down\n");
}

/* mfindVarInFrame - look up symbol in a single environment frame */
static SEXP mfindVarInFrame(SEXP frame, SEXP symbol)
{
	while (frame != R_NilValue) {
		if (TAG(frame) == symbol)
			return frame;
		frame = CDR(frame);
	}
	return R_NilValue;
}

static int isMissing(SEXP symbol, SEXP rho)
{
	SEXP vl;

	while (rho != R_NilValue) {
		vl = mfindVarInFrame(FRAME(rho), symbol);
		if (vl != R_NilValue) {
			if(MISSING(vl) == 1) return 1;
			if(TYPEOF(CAR(vl)) == PROMSXP && TYPEOF(PREXPR(CAR(vl))) == SYMSXP)
				return isMissing(PREXPR(CAR(vl)), PRENV(CAR(vl)));
			else
				return 0;
		}
		rho = ENCLOS(rho);
	}
	return 0;
}


SEXP do_missing(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP rval, s, t;
	int ind;

	checkArity(op, args);
	s = CAR(args);
	if (!isSymbol(s))
		error("\"missing\" illegal use of missing\n");

	ind = 0;
	rval=allocVector(LGLSXP,1);

	while(rho != R_NilValue) {
		t = mfindVarInFrame(FRAME(rho), s);
		if(t != R_NilValue) {
			if(MISSING(t)) {
				LOGICAL(rval)[0] = 1;
				return rval;
			}
			else goto havebinding;
		}
		rho = ENCLOS(rho);
	}
	errorcall(call, "missing applied to non-argument\n");

havebinding:

	t = CAR(t);
	if(TYPEOF(t) != PROMSXP) {
		LOGICAL(rval)[0] = 0;
		return rval;
		/*
		errorcall(call, "non-promise bound to argument\n");
		*/
	}

	if(!isSymbol(PREXPR(t))) LOGICAL(rval)[0] = 0;
	else LOGICAL(rval)[0] = isMissing(PREXPR(t), PRENV(t));
	return rval;
}

SEXP do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP t;

	t = allocVector(INTSXP, 1);
	*INTEGER(t) = NARGS(rho);
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

int isFree(SEXP val)
{
	SEXP t = R_FreeSEXP;

	for (t = R_FreeSEXP; t != R_NilValue; t = CAR(t))
		if (val == t)
			return 1;
	return (0);
}

/*
  here are some debugging functions-hence the d-prefix
*/

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
