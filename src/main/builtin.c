/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2000  The R Development Core Team.
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
#include <Print.h>
#include <Fileio.h>
#include <Rconnections.h>

SEXP do_delay(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, env;
    checkArity(op, args);
    expr = CAR(args);
    env = eval(CADR(args), rho);
    if (!isEnvironment(env))
	errorcall(call, R_MSG_IA);
    return mkPROMISE(expr, env);
}

SEXP do_onexit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *ctxt;
    SEXP code, add, oldcode, tmp;
    int addit = 0;

    switch (length(args)) {
    case 0:
	code = R_NilValue;
	break;
    case 1:
	code = CAR(args);
	break;
    case 2:
	code = CAR(args);
	add = eval(CADR(args), rho);
	if ( TYPEOF(add) != LGLSXP || length(add) != 1 )
	    errorcall(call, "invalid add argument");
	addit = (LOGICAL(add)[0] == 1);
	break;
    default:
	errorcall_return(call, "invalid number of arguments");
    }
    ctxt = R_GlobalContext;
    while (ctxt != R_ToplevelContext && !(ctxt->callflag & CTXT_FUNCTION) )
	ctxt = ctxt->nextcontext;
    if (ctxt->callflag & CTXT_FUNCTION)
    {
	if (addit && (oldcode = ctxt->conexit) != R_NilValue ) {
	    if ( CAR(oldcode) != R_BraceSymbol )
	    {
		PROTECT(tmp = allocList(3));
		SETCAR(tmp, R_BraceSymbol);
		SETCADR(tmp, oldcode);
		SETCADDR(tmp, code);
		SET_TYPEOF(tmp, LANGSXP);
		ctxt->conexit = tmp;
		UNPROTECT(1);
	    }
	    else
	    {
		PROTECT(tmp=allocList(1));
		SETCAR(tmp, code);
		ctxt->conexit = listAppend(oldcode,tmp);
		UNPROTECT(1);
	    }
	}
	else
	    ctxt->conexit = code;
    }
    return R_NilValue;
}

SEXP do_args(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s;
    checkArity(op,args);
    if (TYPEOF(CAR(args)) == STRSXP && length(CAR(args))==1) {
	PROTECT(s = install(CHAR(STRING_ELT(CAR(args), 0))));
	SETCAR(args, findFun(s, rho));
	UNPROTECT(1);
    }
    if (TYPEOF(CAR(args)) == CLOSXP) {
	s = allocSExp(CLOSXP);
	SET_FORMALS(s, FORMALS(CAR(args)));
	SET_BODY(s, R_NilValue);
	SET_CLOENV(s, R_GlobalEnv);
	return(s);
    }
    return R_NilValue;
}

SEXP do_formals(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return duplicate(FORMALS(CAR(args)));
    else
	return R_NilValue;
}

SEXP do_body(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return duplicate(BODY(CAR(args)));
    else return R_NilValue;
}

SEXP do_envir(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return CLOENV(CAR(args));
    else if (CAR(args) == R_NilValue)
	return R_GlobalContext->sysparent;
    else return getAttrib(CAR(args), R_DotEnvSymbol);
}

SEXP do_envirgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP && isEnvironment(CADR(args)))
	SET_CLOENV(CAR(args), CADR(args));
    else if (isEnvironment(CADR(args)))
	setAttrib(CAR(args), R_DotEnvSymbol, CADR(args));
    else
	errorcall(call, "replacement object is not an environment");
    return CAR(args);
}

static void cat_newline(SEXP labels, int *width, int lablen, int ntot)
{
    Rprintf("\n");
    *width = 0;
    if (labels != R_NilValue) {
	Rprintf("%s ", EncodeString(CHAR(STRING_ELT(labels, ntot % lablen)),
				    1, 0, Rprt_adj_left));
	*width += Rstrlen(CHAR(STRING_ELT(labels, ntot % lablen)), 0) + 1;
    }
}

static void cat_sepwidth(SEXP sep, int *width, int ntot)
{
    if (sep == R_NilValue || LENGTH(sep) == 0)
	*width = 0;
    else
	*width = Rstrlen(CHAR(STRING_ELT(sep, ntot % LENGTH(sep))), 0);
}

static void cat_printsep(SEXP sep, int ntot)
{
    char *sepchar;
    if (sep == R_NilValue || LENGTH(sep) == 0)
	return;

    sepchar = CHAR(STRING_ELT(sep, ntot % LENGTH(sep)));
    Rprintf("%s",sepchar);
    return;
}

typedef struct cat_info {
    Rboolean wasopen;
    int changedcon;
    Rconnection con;
} cat_info;

static void cat_cleanup(void *data)
{
    cat_info *pci = data;
    Rconnection con = pci->con;
    Rboolean wasopen = pci->wasopen;
    int changedcon = pci->changedcon;

    con->fflush(con);
    if(!wasopen) con->close(con);  /**** do this second? */
    if(changedcon) switch_stdout(-1, 0);
}

SEXP do_cat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    cat_info ci;
    RCNTXT cntxt;
    SEXP objs, file, fill, sepr, labs, s;
    int ifile;
    Rconnection con;
    int append;
    int w, i, iobj, n, nobjs, pwidth, width, sepw, lablen, ntot, nlsep, nlines;
    char *p = "", buf[512];

    checkArity(op, args);

    /* Use standard printing defaults */
    PrintDefaults(rho);

    objs = CAR(args);
    args = CDR(args);

    file = CAR(args);
    ifile = asInteger(file);
    con = getConnection(ifile);
    args = CDR(args);

    sepr = CAR(args);
    if (!isString(sepr))
	errorcall(call, "invalid sep= specification");
    nlsep = 0;
    for (i = 0; i < LENGTH(sepr); i++)
	if (strstr(CHAR(STRING_ELT(sepr, i)), "\n")) nlsep = 1;
    args = CDR(args);

    fill = CAR(args);
    if ((!isNumeric(fill) && !isLogical(fill)) || (length(fill) != 1))
	errorcall(call, "invalid fill argument");
    if (isLogical(fill)) {
	if (asLogical(fill) == 1)
	    pwidth = R_print.width;
	else
	    pwidth = INT_MAX;
    }
    else pwidth = asInteger(fill);
    args = CDR(args);

    labs = CAR(args);
    if (!isString(labs) && labs != R_NilValue)
	errorcall(call, "invalid label argument");
    lablen = length(labs);
    args = CDR(args);

    append = asLogical(CAR(args));
    if (append == NA_LOGICAL)
	errorcall(call, "invalid append specification");

    ci.wasopen = con->isopen;

    ci.changedcon = switch_stdout(ifile, 0); 
    /* will open new connection if required */

    ci.con = con;

    /* set up a context which will close the window if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_NilValue, R_NilValue,
		 R_NilValue);
    cntxt.cend = &cat_cleanup;
    cntxt.cenddata = &ci;

    nobjs = length(objs);
    /*
    for (i = 0; i < nobjs; i++) {
	if (!isVector(VECTOR_ELT(objs, i)) && !isNull(VECTOR_ELT(objs, i)))
	    errorcall(call, "argument %d has invalid type", i + 1);
    }
    */
    width = 0;
    ntot = 0;
    nlines = 0;
    for (iobj = 0; iobj < nobjs; iobj++) {
	s = VECTOR_ELT(objs, iobj);
	if (iobj != 0 && !isNull(s))
	    cat_printsep(sepr, 0);
	n = length(s);
	if (n > 0) {
	    if (labs != R_NilValue && (iobj == 0)
		&& (asInteger(fill) > 0)) {
		Rprintf("%s ", CHAR(STRING_ELT(labs, nlines)));
		width += strlen(CHAR(STRING_ELT(labs, nlines % lablen))) + 1;
		nlines++;
	    }
	    if (isString(s))
		p = CHAR(STRING_ELT(s, 0));
            else if (isSymbol(s))
                p = CHAR(PRINTNAME(s));
	    else if (isVectorAtomic(s)) {
		p = EncodeElement(s, 0, 0);
		strcpy(buf,p);
		p=buf;
	    }
#ifdef fixed_cat
	    else if (isVectorList(s)) {
	      /* FIXME:	 call EncodeElement() for every element of  s.

		 Real Problem: `s' can be large;
		 should do line breaking etc.. (buf is of limited size)
	      */
	    }
#endif
	    else
		errorcall(call, "argument %d not yet handled by cat",1+iobj);
	    /* FIXME : cat(...) should handle ANYTHING */
	    w = strlen(p);
	    cat_sepwidth(sepr, &sepw, ntot);
	    if ((iobj > 0) && (width + w + sepw > pwidth)) {
		cat_newline(labs, &width, lablen, nlines);
		nlines++;
	    }
	    for (i = 0; i < n; i++, ntot++) {
		Rprintf("%s", p);
		width += w + sepw;
		if (i < (n - 1)) {
		    cat_printsep(sepr, ntot);
		    if (isString(s))
			p = CHAR(STRING_ELT(s, i+1));
		    else {
			p = EncodeElement(s, i+1, 0);
			strcpy(buf,p);
			p = buf;
		    }
		    w = strlen(p);
		    cat_sepwidth(sepr, &sepw, ntot);
		    if ((width + w + sepw > pwidth) && pwidth) {
			cat_newline(labs, &width, lablen, nlines);
			nlines++;
		    }
		}
	    }
	}
    }
    if ((pwidth != INT_MAX) || nlsep)
	Rprintf("\n");

    /* end the context after anything that could raise an error but before
       doing the cleanup so the cleanup doesn't get done twice */
    endcontext(&cntxt);

    cat_cleanup(&ci);

    return R_NilValue;
}

SEXP do_makelist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP list, names;
    int i, n, havenames;
    havenames = 0;
    n = length(args);
    PROTECT(list = allocVector(VECSXP, n));
    PROTECT(names = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if (TAG(args) != R_NilValue) {
	    SET_STRING_ELT(names, i, PRINTNAME(TAG(args)));
	    havenames = 1;
	}
	else {
	    SET_STRING_ELT(names, i, R_BlankString);
	}
	if (NAMED(CAR(args)))
	    SET_VECTOR_ELT(list, i, duplicate(CAR(args)));
	else
	    SET_VECTOR_ELT(list, i, CAR(args));
	args = CDR(args);
    }
    if (havenames) {
	setAttrib(list, R_NamesSymbol, names);
    }
    UNPROTECT(2);
    return list;
}

#ifdef NOT_used
SEXP do_namedlist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
}
#endif /* NOT_used */


SEXP do_expression(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, nms;
    int i, n, named;
    named = 0;
    n = length(args);
    PROTECT(ans = allocVector(EXPRSXP, n));
    a = args;
    for (i = 0; i < n; i++) {
	SET_VECTOR_ELT(ans, i, duplicate(CAR(a)));
	if (TAG(a) != R_NilValue) named = 1;
	a = CDR(a);
    }
    if (named) {
	PROTECT(nms = allocVector(STRSXP, n));
	a = args;
	for (i = 0; i < n; i++) {
	    if (TAG(a) != R_NilValue)
		SET_STRING_ELT(nms, i, PRINTNAME(TAG(a)));
	    else
		SET_STRING_ELT(nms, i, R_BlankString);
	    a = CDR(a);
	}
	setAttrib(ans, R_NamesSymbol, nms);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

/* vector(mode="logical", length=0) */
SEXP do_makevector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int len, i;
    SEXP s;
    SEXPTYPE mode;
    checkArity(op, args);
    len = asInteger(CADR(args));
    s = coerceVector(CAR(args), STRSXP);
    if (length(s) == 0)
	error("vector: zero-length type argument");
    mode = str2type(CHAR(STRING_ELT(s, 0)));
    if (mode == -1 && streql(CHAR(STRING_ELT(s, 0)), "double"))
	mode = REALSXP;
    switch (mode) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	s = allocVector(mode, len);
	break;
    case LISTSXP:
	s = allocList(len);
	break;
    default:
	error("vector: cannot make a vector of mode \"%s\".",
	      CHAR(STRING_ELT(s, 0)));
    }
    if (mode == INTSXP || mode == LGLSXP)
	for (i = 0; i < len; i++)
	    INTEGER(s)[i] = 0;
    else if (mode == REALSXP)
	for (i = 0; i < len; i++)
	    REAL(s)[i] = 0.0;
#ifdef OLD
    else if (mode == STRSXP) {
	for (i = 0; i < len; i++)
	    SET_STRING_ELT(s, i, R_BlankString);
    }
#endif
    return s;
}


/* do_lengthgets: assign a length to a vector or a list */
/* (if it is vectorizable). We could probably be fairly */
/* clever with memory here if we wanted to. */

SEXP lengthgets(SEXP x, int len)
{
    int lenx, i;
    SEXP rval, names, xnames, t;
    if (!isVector(x) && !isVectorizable(x))
	error("can not set length of non-vector");
    lenx = length(x);
    if (lenx == len)
	return (x);
    rval = allocVector(TYPEOF(x), len);
    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
    if (xnames != R_NilValue)
	names = allocVector(STRSXP, len);
    else names = R_NilValue;	/*- just for -Wall --- should we do this ? */
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		INTEGER(rval)[i] = INTEGER(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		INTEGER(rval)[i] = NA_INTEGER;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		REAL(rval)[i] = REAL(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		REAL(rval)[i] = NA_REAL;
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		COMPLEX(rval)[i] = COMPLEX(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else {
		COMPLEX(rval)[i].r = NA_REAL;
		COMPLEX(rval)[i].i = NA_REAL;
	    }
	break;
    case STRSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		SET_STRING_ELT(rval, i, STRING_ELT(x, i));
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		SET_STRING_ELT(rval, i, NA_STRING);
	break;
    case LISTSXP:
	for (t = rval; t != R_NilValue; t = CDR(t), x = CDR(x)) {
	    SETCAR(t, CAR(x));
	    SET_TAG(t, TAG(x));
	}
    case VECSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		SET_VECTOR_ELT(rval, i, VECTOR_ELT(x, i));
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	break;
    }
    if (isVector(x) && xnames != R_NilValue)
	setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(1);
    return rval;
}


SEXP do_lengthgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int len;
    SEXP x;
    checkArity(op, args);
    x = CAR(args);
    if (!isVector(x) && !isVectorizable(x))
       error("length<- invalid first argument");
    if (length(CADR(args)) != 1)
       error("length<- invalid second argument");
    len = asInteger(CADR(args));
    if (len == NA_INTEGER)
       error("length<- missing value for length");
    return lengthgets(x, len);
}



/* For switch, evaluate the first arg, if it is a character then try */
/* to match the name with the remaining args, and evaluate the match, */
/* if there is no match then evaluate the first unnamed arg.  If the */
/* value of the first arg is not a character string then coerce it to */
/* an integer k and choose the kth argument from those that remain */
/* provided 0 < k < (nargs-1). For character matching, if the value */
/* is missing then take the next non-missing arg as the value. Then */
/* things like switch(as.character(answer), yes=, YES=1, no=, NO=2, 3) */
/* will work. */

static SEXP switchList(SEXP el, SEXP rho)
{
    SEXP h;
    if (CAR(el) == R_DotsSymbol) {
	h = findVar(CAR(el), rho);
	if (h == R_NilValue)
	    return R_NilValue;
	if (TYPEOF(h) != DOTSXP) {
	    if (h == R_MissingArg)
		return R_MissingArg;
	    error("... used in an incorrect context");
	}
	return h;
    }
    else {
	error("invalid parameter in switch");
	return R_NilValue;/* for -Wall */
    }
}

SEXP do_switch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int argval;
    SEXP x, y, w;
    x = eval(CAR(args), rho);
    if (!isVector(x) || length(x) != 1)
	error("switch: EXPR must return a length 1 vector");
    PROTECT(w = switchList(CDR(args), rho));
    if (isString(x)) {
	for (y = w; y != R_NilValue; y = CDR(y))
	    if (TAG(y) != R_NilValue && pmatch(STRING_ELT(x, 0), TAG(y), 1)) {
		while (CAR(y) == R_MissingArg && y != R_NilValue)
		    y = CDR(y);
		UNPROTECT(1);
		return (eval(CAR(y), rho));
	    }
	for (y = w; y != R_NilValue; y = CDR(y))
	    if (TAG(y) == R_NilValue) {
		UNPROTECT(1);
		return (eval(CAR(y), rho));
	    }
	UNPROTECT(1);
	return R_NilValue;
    }
    argval = asInteger(x);
    if (argval <= 0 || argval > (length(w))) {
	UNPROTECT(1);
	return R_NilValue;
    }
    x = eval(CAR(nthcdr(w, argval - 1)), rho);
    UNPROTECT(1);
    return x;
}

