/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2001  The R Development Core Team
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

#include <string.h>
#include <stdlib.h>

#include <Defn.h>
#include <Rmath.h>
#include <Rdevices.h>

#include <R_ext/RConverters.h>

#ifndef max
#define max(a, b) ((a > b)?(a):(b))
#endif


/* These are set during each call to do_dotCode() below. */

static SEXP NaokSymbol = NULL;
static SEXP DupSymbol = NULL;
static SEXP PkgSymbol = NULL;

static char DLLname[PATH_MAX];

/* This looks up entry points in DLLs in a platform specific way. */
#include <Rdynpriv.h>

/* Convert an R object to a non-moveable C/Fortran object and return
   a pointer to it.  This leaves pointers for anything other
   than vectors and lists unaltered.
*/

static void *RObjToCPtr(SEXP s, int naok, int dup, int narg, int Fort, const char *name, R_toCConverter **converter)
{
    int *iptr;
    float *sptr;
    double *rptr;
    char **cptr, *fptr;
    Rcomplex *zptr;
    SEXP *lptr, CSingSymbol=install("Csingle");
    int i, l, n;

    if(converter)
	*converter = NULL;

    if(length(getAttrib(s, R_ClassSymbol))) {
	R_CConvertInfo info;
	int success;
        void *ans;

	info.naok = naok;
	info.dup = dup;
	info.narg = narg;
	info.Fort = Fort;
        info.name = name;

	ans = Rf_convertToC(s, &info, &success, converter);
	if(success)
	    return(ans);
    }

    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	n = LENGTH(s);
	iptr = INTEGER(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && iptr[i] == NA_INTEGER)
		error("NAs in foreign function call (arg %d)", narg);
	}
	if (dup) {
	    iptr = (int*)R_alloc(n, sizeof(int));
	    for (i = 0 ; i < n ; i++)
		iptr[i] = INTEGER(s)[i];
	}
	return (void*)iptr;
	break;
    case REALSXP:
	n = LENGTH(s);
	rptr = REAL(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && !R_FINITE(rptr[i]))
		error("NA/NaN/Inf in foreign function call (arg %d)", narg);
	}
	if (dup) {
	    if(asLogical(getAttrib(s, CSingSymbol)) == 1) {
		sptr = (float*)R_alloc(n, sizeof(float));
		for (i = 0 ; i < n ; i++)
		    sptr[i] = (float) REAL(s)[i];
		return (void*)sptr;
	    } else {
		rptr = (double*)R_alloc(n, sizeof(double));
		for (i = 0 ; i < n ; i++)
		    rptr[i] = REAL(s)[i];
		return (void*)rptr;
	    }
	} else
	    return (void*)rptr;
	break;
    case CPLXSXP:
	n = LENGTH(s);
	zptr = COMPLEX(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && (!R_FINITE(zptr[i].r) || !R_FINITE(zptr[i].i)))
		error("Complex NA/NaN/Inf in foreign function call (arg %d)", narg);
	}
	if (dup) {
	    zptr = (Rcomplex*)R_alloc(n, sizeof(Rcomplex));
	    for (i = 0 ; i < n ; i++)
		zptr[i] = COMPLEX(s)[i];
	}
	return (void*)zptr;
	break;
    case STRSXP:
	if(!dup)
	    error("character variables must be duplicated in .C/.Fortran");
	n = LENGTH(s);
	if(Fort) {
	    if(n > 1) warning("only first string in char vector used in .Fortran");
	    l = strlen(CHAR(STRING_ELT(s, 0)));
	    fptr = (char*)R_alloc(max(255, l) + 1, sizeof(char));
	    strcpy(fptr, CHAR(STRING_ELT(s, 0)));
	    return (void*)fptr;
	} else {
	    cptr = (char**)R_alloc(n, sizeof(char*));
	    for (i = 0 ; i < n ; i++) {
		l = strlen(CHAR(STRING_ELT(s, i)));
		cptr[i] = (char*)R_alloc(l + 1, sizeof(char));
		strcpy(cptr[i], CHAR(STRING_ELT(s, i)));
	    }
	    return (void*)cptr;
	}
	break;
    case VECSXP:
	if (!dup) return (void*)VECTOR_PTR(s); /***** Dangerous to GC!!! */
  	n = length(s);
	lptr = (SEXP*)R_alloc(n, sizeof(SEXP));
	for (i = 0 ; i < n ; i++) {
	    lptr[i] = VECTOR_ELT(s, i);
	}
 	return (void*)lptr;
	break;
    case LISTSXP:
	if(Fort) error("invalid mode to pass to Fortran (arg %d)", narg);
	/* Warning : The following looks like it could bite ... */
	if(!dup) return (void*)s;
	n = length(s);
	cptr = (char**)R_alloc(n, sizeof(char*));
	for(i=0 ; i<n ; i++) {
	    cptr[i] = (char*)s;
	    s = CDR(s);
	}
	return (void*)cptr;
	break;
    default:
	if(Fort) error("invalid mode to pass to Fortran (arg %d)", narg);
	return (void*)s;
    }
}


static SEXP CPtrToRObj(void *p, SEXP arg, int Fort)
{
    int *iptr, n=length(arg);
    float *sptr;
    double *rptr;
    char **cptr, buf[256];
    Rcomplex *zptr;
    SEXP *lptr, CSingSymbol = install("Csingle");
    int i;
    SEXP s, t;
    SEXPTYPE type =TYPEOF(arg);

    switch(type) {
    case LGLSXP:
    case INTSXP:
	s = allocVector(type, n);
	iptr = (int*)p;
	for(i=0 ; i<n ; i++) {
	    INTEGER(s)[i] = iptr[i];
	}
	break;
    case REALSXP:
	s = allocVector(type, n);
	if(asLogical(getAttrib(arg, CSingSymbol)) == 1) {
	    sptr = (float*) p;
	    for(i=0 ; i<n ; i++) REAL(s)[i] = (double) sptr[i];
	} else {
	    rptr = (double*) p;
	    for(i=0 ; i<n ; i++) REAL(s)[i] = rptr[i];
	}
	break;
    case CPLXSXP:
	s = allocVector(type, n);
	zptr = (Rcomplex*)p;
	for(i=0 ; i<n ; i++) {
	    COMPLEX(s)[i] = zptr[i];
	}
	break;
    case STRSXP:
	if(Fort) {
	    /* only return one string: warned on the R -> Fortran step */
	    strncpy(buf, (char*)p, 255);
	    buf[256] = '\0';
	    PROTECT(s = allocVector(type, 1));
	    SET_STRING_ELT(s, 0, mkChar(buf));
	    UNPROTECT(1);
	} else {
	    PROTECT(s = allocVector(type, n));
	    cptr = (char**)p;
	    for(i = 0 ; i < n ; i++) {
		SET_STRING_ELT(s, i, mkChar(cptr[i]));
	    }
	    UNPROTECT(1);
	}
	break;
    case VECSXP:
	PROTECT(s = allocVector(VECSXP, n));
	lptr = (SEXP*)p;
	for (i = 0 ; i < n ; i++) {
	    SET_VECTOR_ELT(s, i, lptr[i]);
	}
	UNPROTECT(1);
	break;
    case LISTSXP:
	PROTECT(t = s = allocList(n));
	lptr = (SEXP*)p;
	for(i=0 ; i<n ; i++) {
	    SETCAR(t, lptr[i]);
	    t = CDR(t);
	}
	UNPROTECT(1);
    default:
	s = (SEXP)p;
    }
    return s;
}


/* Foreign Function Interface.  This code allows a user to call C */
/* or Fortran code which is either statically or dynamically linked. */

/* NB: despite its name, this leaves NAOK and DUP arguments on the list */
#ifdef OLD
static SEXP naoktrim(SEXP s, int * len, int *naok, int *dup)
{
    SEXP value;
    if(s == R_NilValue) {
	value = R_NilValue;
	*naok = 0;
	*dup = 1;
	*len = 0;
    }
    else if(TAG(s) == NaokSymbol) {
	value = naoktrim(CDR(s), len, naok, dup);
	*naok = asLogical(CAR(s));
	(*len)++;
    }
    else if(TAG(s) == DupSymbol) {
	value = naoktrim(CDR(s), len, naok, dup);
	*dup = asLogical(CAR(s));
	(*len)++;
    }
    else if(TAG(s) == PkgSymbol) {
	value = naoktrim(CDR(s), len, naok, dup);
	strcpy(DLLname, CHAR(STRING_ELT(CAR(s), 0)));
	return value;
    }
    else {
	CDR(s) = naoktrim(CDR(s), len, naok, dup);
	(*len)++;
    }
    return s;
}
#endif

/* find NAOK and DUP, find and remove PACKAGE */
static SEXP naokfind(SEXP args, int * len, int *naok, int *dup)
{
    SEXP s, prev;
    int nargs=0, naokused=0, dupused=0, pkgused=0;

    *naok = 0;
    *dup = 1;
    *len = 0;
    for(s = args, prev=args; s != R_NilValue;) {
	if(TAG(s) == NaokSymbol) {
	    *naok = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(naokused++ == 1) warning("NAOK used more than once");
	} else if(TAG(s) == DupSymbol) {
	    *dup = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(dupused++ == 1) warning("DUP used more than once");
	} else if(TAG(s) == PkgSymbol) {
	    strcpy(DLLname, CHAR(STRING_ELT(CAR(s), 0)));
	    if(pkgused++ > 1) warning("PACKAGE used more than once");
	    /* More generally, this should allow us to process
               any additional arguments and not insist that PACKAGE
               be the last argument.
             */
             if(s == args) 
		 args = s = CDR(s);
             else 
		 SETCDR(prev, s = CDR(s));
             continue;
	} 
        nargs++;
	prev = s;
	s = CDR(s);
    }
    *len = nargs;
    return args;
}

static void setDLLname(SEXP s, char *DLLName) {
  SEXP ss = CAR(s); char *name;
  if(TYPEOF(ss) != STRSXP || length(ss) != 1)
    error("PACKAGE argument must be a single character string");
  name = CHAR(STRING_ELT(ss, 0));
  /* allow the package: form of the name, as returned by find */
  if(strncmp(name, "package:", 8) == 0)
    name += 8;
  strcpy(DLLname, name);
}

static SEXP pkgtrim(SEXP args)
{
    SEXP s, ss;
    int pkgused=0;

    for(s = args ; s != R_NilValue;) {
	ss = CDR(s);
	/* Look for PACKAGE=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == R_NilValue && TAG(s) == PkgSymbol) {
	    if(pkgused++ == 1) warning("PACKAGE used more than once");
	    setDLLname(s, DLLname);
	    return R_NilValue;
	}
	if(TAG(ss) == PkgSymbol) {
	    if(pkgused++ == 1) warning("PACKAGE used more than once");
	    setDLLname(ss, DLLname);
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}

#define MAX_ARGS 65

SEXP do_symbol(SEXP call, SEXP op, SEXP args, SEXP env)
{
    char buf[128], *p, *q;
    checkArity(op, args);
    if(!isValidString(CAR(args)))
	errorcall(call, R_MSG_IA);
    p = CHAR(STRING_ELT(CAR(args), 0));
    q = buf;
    while ((*q = *p) != '\0') {
	p++;
	q++;
    }
#ifdef HAVE_F77_UNDERSCORE
    if(PRIMVAL(op)) {
	*q++ = '_';
	*q = '\0';
    }
#endif
    return mkString(buf);
}

SEXP do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    char *sym, *pkg= "";
    int val = 1, nargs = length(args);

    if (nargs < 1) errorcall(call, "no arguments supplied");
    if (nargs > 2) errorcall(call, "too many arguments");

    if(!isValidString(CAR(args)))
	errorcall(call, R_MSG_IA);
    sym = CHAR(STRING_ELT(CAR(args), 0));
    if(nargs == 2) {
	if(!isValidString(CADR(args)))
	    errorcall(call, R_MSG_IA);
	pkg = CHAR(STRING_ELT(CADR(args), 0));
    }
    if (!(R_FindSymbol(sym, pkg, NULL))) val = 0;
    ans = allocVector(LGLSXP, 1);
    LOGICAL(ans)[0] = val;
    return ans;
}

/*   Call dynamically loaded "internal" functions */
/*   code by Jean Meloche <jean@stat.ubc.ca> */

SEXP do_External(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC fun;
    SEXP retval;
    R_RegisteredNativeSymbol symbol = {R_EXTERNAL_SYM, {NULL}, NULL};
    /* I don't like this messing with vmax <TSL> */
    /* But it is needed for clearing R_alloc and to be like .Call <BDR>*/
    char *vmax = vmaxget();

    op = CAR(args);
    if (!isValidString(op))
	errorcall(call, "function name must be a string (of length 1)");
    if (PkgSymbol == NULL) PkgSymbol = install("PACKAGE");
    strcpy(DLLname, "");
    args = pkgtrim(args);

    if (!(fun=R_FindSymbol(CHAR(STRING_ELT(op, 0)), DLLname, NULL)))
	errorcall(call, "C function name not in load table");

    if(symbol.symbol.external && symbol.symbol.external->numArgs > -1) {
	if(symbol.symbol.external->numArgs != length(args))
	    error("Incorrect number of arguments (%d), expecting %d for %s", 
		  length(args), symbol.symbol.external->numArgs, CHAR(STRING_ELT(op, 0)));
    }
    retval = (SEXP)fun(args);
    vmaxset(vmax);
    return retval;
}


/* .Call(name, <args>) */
SEXP do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC fun;
    SEXP retval, cargs[MAX_ARGS], pargs;
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {NULL}, NULL};
    int nargs;
    char *vmax = vmaxget();
    op = CAR(args);
    if (!isValidString(op))
	errorcall(call, "function name must be a string (of length 1)");

    if (PkgSymbol == NULL) PkgSymbol = install("PACKAGE");
    strcpy(DLLname, "");
    args = pkgtrim(args);

    if (!(fun=R_FindSymbol(CHAR(STRING_ELT(op, 0)), DLLname, &symbol)))
        errorcall(call, ".Call function name not in load table");
    args = CDR(args);

    for(nargs = 0, pargs = args ; pargs != R_NilValue; pargs = CDR(pargs)) {
        if (nargs == MAX_ARGS)
            errorcall(call, "too many arguments in foreign function call");
	cargs[nargs] = CAR(pargs);
	nargs++;
    }
    if(symbol.symbol.call && symbol.symbol.call->numArgs > -1) {
	if(symbol.symbol.call->numArgs != nargs)
	    error("Incorrect number of arguments (%d), expecting %d for %s", 
		  nargs, symbol.symbol.call->numArgs, CHAR(STRING_ELT(op, 0)));
    }

    retval = R_NilValue;	/* -Wall */
    switch (nargs) {
    case 0:
	retval = (SEXP)fun();
	break;
    case 1:
	retval = (SEXP)fun(cargs[0]);
	break;
    case 2:
	retval = (SEXP)fun(cargs[0], cargs[1]);
	break;
    case 3:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    default:
	errorcall(call, "too many arguments, sorry");
    }
    vmaxset(vmax);
    return retval;
}

/*  Call dynamically loaded "internal" graphics functions */
/*  .External.gr  and  .Call.gr */

SEXP do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval = do_External(call, op, args, env);
    if (call != R_NilValue) {
        DevDesc *dd = CurrentDevice();
        GCheckState(dd);
	recordGraphicOperation(op, args, dd);
    }
    return retval;
}

SEXP do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval = do_dotcall(call, op, args, env);
    if (call != R_NilValue) {
        DevDesc *dd = CurrentDevice();
	GCheckState(dd);
	recordGraphicOperation(op, args, dd);
    }
    return retval;
}


/* .C() {op=0}  or  .Fortran() {op=1} */
SEXP do_dotCode(SEXP call, SEXP op, SEXP args, SEXP env)
{
    void **cargs;
    int dup, havenames, naok, nargs, which;
    DL_FUNC fun;
    SEXP ans, pargs, s;
    R_toCConverter  *argConverters[65];
    R_RegisteredNativeSymbol symbol = {R_C_SYM, {NULL}, NULL};

    char buf[128], *p, *q, *vmax;
    if (NaokSymbol == NULL || DupSymbol == NULL || PkgSymbol == NULL) {
	NaokSymbol = install("NAOK");
	DupSymbol = install("DUP");
	PkgSymbol = install("PACKAGE");
    }
    vmax = vmaxget();
    which = PRIMVAL(op);
    if(which)
	symbol.type = R_FORTRAN_SYM;
    op = CAR(args);
    if (!isValidString(op))
	errorcall(call, "function name must be a string (of length 1)");

    /* The following code modifies the argument list */
    /* We know this is ok because do_dotCode is entered */
    /* with its arguments evaluated. */

    strcpy(DLLname, "");
    args = naokfind(CDR(args), &nargs, &naok, &dup);
    if(naok == NA_LOGICAL)
	errorcall(call, "invalid naok value");
    if(nargs > MAX_ARGS)
	errorcall(call, "too many arguments in foreign function call");

    /* Make up the load symbol and look it up. */

    p = CHAR(STRING_ELT(op, 0));
    q = buf;
    while ((*q = *p) != '\0') {
	p++;
	q++;
    }
#ifdef HAVE_F77_UNDERSCORE
    if (which)
	*q++ = '_';
    *q = '\0';
#endif
#ifdef Macintosh
    if (!(fun = R_FindSymbol(buf, "", &symbol)))
#else
    if (!(fun = R_FindSymbol(buf, DLLname, &symbol)))
#endif /* Macintosh */
	errorcall(call, "C/Fortran function name not in load table");

    if(symbol.symbol.c && symbol.symbol.c->numArgs > -1) {
	if(symbol.symbol.c->numArgs != nargs)
	    error("Incorrect number of arguments (%d), expecting %d for %s", 
		  nargs, symbol.symbol.c->numArgs, buf);
    }

    /* Convert the arguments for use in foreign */
    /* function calls.  Note that we copy twice */
    /* once here, on the way into the call, and */
    /* once below on the way out. */
    cargs = (void**)R_alloc(nargs, sizeof(void*));
    nargs = 0;
    for(pargs = args ; pargs != R_NilValue; pargs = CDR(pargs)) {
	cargs[nargs] = RObjToCPtr(CAR(pargs), naok, dup, nargs + 1, which, buf, argConverters + nargs);
	nargs++;
    }


    switch (nargs) {
    case 0:
	/* Silicon graphics C chokes here */
	/* if there is no argument to fun. */
	fun(0);
	break;
    case 1:
	fun(cargs[0]);
	break;
    case 2:
	fun(cargs[0], cargs[1]);
	break;
    case 3:
	fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    default:
	errorcall(call, "too many arguments, sorry");
    }
    PROTECT(ans = allocVector(VECSXP, nargs));
    havenames = 0;
    if (dup) {
	R_FromCConvertInfo info;
	info.cargs = cargs;
	info.allArgs = args;
	info.nargs = nargs;
	info.functionName = buf;
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
            if(argConverters[nargs]) {
                if(argConverters[nargs]->reverse) {
		    info.argIndex = nargs;
		    s = argConverters[nargs]->reverse(cargs[nargs], CAR(pargs), 
                                                       &info, argConverters[nargs]);
		} else
		    s = R_NilValue;
		PROTECT(s);
	    } else {
		PROTECT(s = CPtrToRObj(cargs[nargs], CAR(pargs), which));
		SET_ATTRIB(s, duplicate(ATTRIB(CAR(pargs))));
		SET_OBJECT(s, OBJECT(CAR(pargs)));
	    }
	    if (TAG(pargs) != R_NilValue)
		havenames = 1;
	    SET_VECTOR_ELT(ans, nargs, s);
	    nargs++;
	    UNPROTECT(1);
	}
    }
    else {
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
	    if (TAG(pargs) != R_NilValue)
		havenames = 1;
	    SET_VECTOR_ELT(ans, nargs, CAR(pargs));
	    nargs++;
	}
    }
    if (havenames) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, nargs));
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
	    if (TAG(pargs) == R_NilValue)
		SET_STRING_ELT(names, nargs++, R_BlankString);
	    else
		SET_STRING_ELT(names, nargs++, PRINTNAME(TAG(pargs)));
        }
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return (ans);
}

/* FIXME : Must work out what happens here when we replace LISTSXP by
   VECSXP. */

static struct {
    char *name;
    SEXPTYPE type;
}
typeinfo[] = {
    {"logical",	  LGLSXP },
    {"integer",	  INTSXP },
    {"double",	  REALSXP},
    {"complex",	  CPLXSXP},
    {"character", STRSXP },
    {"list",	  VECSXP },
    {NULL,	  0      }
};

static int string2type(char *s)
{
    int i;
    for (i = 0 ; typeinfo[i].name ; i++) {
	if(!strcmp(typeinfo[i].name, s)) {
	    return typeinfo[i].type;
	}
    }
    error("type \"%s\" not supported in interlanguage calls", s);
    return 1; /* for -Wall */
}

void call_R(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    SEXP call, pcall, s;
    SEXPTYPE type;
    int i, j, n;

    if (!isFunction((SEXP)func))
	error("invalid function in call_R");
    if (nargs < 0)
	error("invalid argument count in call_R");
    if (nres < 0)
	error("invalid return value count in call_R");
    PROTECT(pcall = call = allocList(nargs + 1));
    SET_TYPEOF(call, LANGSXP);
    SETCAR(pcall, (SEXP)func);
    s = R_NilValue;		/* -Wall */
    for (i = 0 ; i < nargs ; i++) {
	pcall = CDR(pcall);
	type = string2type(modes[i]);
	switch(type) {
	case LGLSXP:
	case INTSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(type, n));
	    memcpy(INTEGER(CAR(pcall)), arguments[i], n * sizeof(int));
	    break;
	case REALSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(REALSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(double));
	    break;
	case CPLXSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(CPLXSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(Rcomplex));
	    break;
	case STRSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(STRSXP, n));
	    for (j = 0 ; j < n ; j++) {
		char *str = (char*)(arguments[i]);
		s = allocString(strlen(str));
		SET_STRING_ELT(CAR(pcall), i, s);
		strcpy(CHAR(s), str);
 	    }
	    break;
	    /* FIXME : This copy is unnecessary! */
	    /* FIXME : This is obviously incorrect so disable
	case VECSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(VECSXP, n));
	    for (j = 0 ; j < n ; j++) {
		SET_VECTOR_ELT(s, i, (SEXP)(arguments[i]));
	    }
	    break; */
	default:
	    error("Mode `%s' is not supported in call_R", modes[i]);
	}
	if(names && names[i])
	    SET_TAG(pcall, install(names[i]));
	SET_NAMED(CAR(pcall), 2);
    }
    PROTECT(s = eval(call, R_GlobalEnv));
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	if(nres > 0)
	    results[0] = RObjToCPtr(s, 1, 1, 0, 0, (const char *)NULL, NULL);
	break;
    case VECSXP:
	n = length(s);
	if (nres < n) n = nres;
	for (i = 0 ; i < n ; i++) {
	    results[i] = RObjToCPtr(VECTOR_ELT(s, i), 1, 1, 0, 0, (const char *)NULL, NULL);
	}
	break;
    case LISTSXP:
	n = length(s);
	if(nres < n) n = nres;
	for(i=0 ; i<n ; i++) {
	    results[i] = RObjToCPtr(s, 1, 1, 0, 0, (const char *)NULL, NULL);
	    s = CDR(s);
	}
	break;
    }
    UNPROTECT(2);
    return;
}

void call_S(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    call_R(func, nargs, arguments, modes, lengths, names, nres, results);
}
