/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2012  The R Core Team.
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
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Print.h>
#include <Fileio.h>
#include <Rconnections.h>

#include <R_ext/RS.h> /* for Memzero */

attribute_hidden
R_xlen_t asVecSize(SEXP x)
{
    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case INTSXP:
	{
	    int res = INTEGER(x)[0];
	    if(res == NA_INTEGER) error(_("vector size cannot be NA"));
	    return (R_xlen_t) res;
	}
	case REALSXP:
	{
	    double d = REAL(x)[0];
	    if(ISNAN(d)) error(_("vector size cannot be NA/NaN"));
	    if(!R_FINITE(d)) error(_("vector size cannot be infinite"));
	    if(d > R_XLEN_T_MAX) error(_("vector size specified is too large"));
	    return (R_xlen_t) d;
	}
	case STRSXP:
	{
	    double d = asReal(x);
	    if(ISNAN(d)) error(_("vector size cannot be NA/NaN"));
	    if(!R_FINITE(d)) error(_("vector size cannot be infinite"));
	    if(d > R_XLEN_T_MAX) error(_("vector size specified is too large"));
	    return (R_xlen_t) d;
	}
	default:
	    break;
	}
    }
    return -999;  /* which gives error in the caller */
}

SEXP attribute_hidden do_delayed(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP name = R_NilValue /* -Wall */, expr, eenv, aenv;
    checkArity(op, args);

    if (!isString(CAR(args)) || length(CAR(args)) == 0)
	error(_("invalid first argument"));
    else
	name = installTrChar(STRING_ELT(CAR(args), 0));
    args = CDR(args);
    expr = CAR(args);

    args = CDR(args);
    eenv = CAR(args);
    if (isNull(eenv)) {
	error(_("use of NULL environment is defunct"));
	eenv = R_BaseEnv;
    } else
    if (!isEnvironment(eenv))
	errorcall(call, _("invalid '%s' argument"), "eval.env");

    args = CDR(args);
    aenv = CAR(args);
    if (isNull(aenv)) {
	error(_("use of NULL environment is defunct"));
	aenv = R_BaseEnv;
    } else
    if (!isEnvironment(aenv))
	errorcall(call, _("invalid '%s' argument"), "assign.env");

    defineVar(name, mkPROMISE(expr, eenv), aenv);
    return R_NilValue;
}

/* makeLazy(names, values, expr, eenv, aenv) */
SEXP attribute_hidden do_makelazy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP names, values, val, expr, eenv, aenv, expr0;
    R_xlen_t i;

    checkArity(op, args);
    names = CAR(args); args = CDR(args);
    if (!isString(names))
	error(_("invalid first argument"));
    values = CAR(args); args = CDR(args);
    expr = CAR(args); args = CDR(args);
    eenv = CAR(args); args = CDR(args);
    if (!isEnvironment(eenv)) error(_("invalid '%s' argument"), "eval.env");
    aenv = CAR(args);
    if (!isEnvironment(aenv)) error(_("invalid '%s' argument"), "assign.env");

    for(i = 0; i < XLENGTH(names); i++) {
	SEXP name = install(CHAR(STRING_ELT(names, i)));
	PROTECT(val = eval(VECTOR_ELT(values, i), eenv));
	PROTECT(expr0 = duplicate(expr));
	SETCAR(CDR(expr0), val);
	defineVar(name, mkPROMISE(expr0, eenv), aenv);
	UNPROTECT(2);
    }
    return R_NilValue;
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_onexit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *ctxt;
    SEXP code, oldcode, tmp, ap, argList;
    int addit = 0;

    PROTECT(ap = list2(R_NilValue, R_NilValue));
    SET_TAG(ap,  install("expr"));
    SET_TAG(CDR(ap), install("add"));
    PROTECT(argList =  matchArgs(ap, args, call));
    if (CAR(argList) == R_MissingArg) code = R_NilValue;
    else code = CAR(argList);
    if (CADR(argList) != R_MissingArg) {
	addit = asLogical(eval(CADR(args), rho));
	if (addit == NA_INTEGER)
	    errorcall(call, _("invalid '%s' argument"), "add");
    }

    ctxt = R_GlobalContext;
    /* Search for the context to which the on.exit action is to be
       attached. Lexical scoping is implemented by searching for the
       first closure call context with an environment matching the
       expression evaluation environment. */
    while (ctxt != R_ToplevelContext &&
	   !((ctxt->callflag & CTXT_FUNCTION) && ctxt->cloenv == rho) )
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
		PROTECT(tmp = allocList(1));
		SETCAR(tmp, code);
		ctxt->conexit = listAppend(duplicate(oldcode),tmp);
		UNPROTECT(1);
	    }
	}
	else
	    ctxt->conexit = code;
    }
    UNPROTECT(2);
    return R_NilValue;
}

SEXP attribute_hidden do_args(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s;

    checkArity(op,args);
    if (TYPEOF(CAR(args)) == STRSXP && length(CAR(args))==1) {
	PROTECT(s = installTrChar(STRING_ELT(CAR(args), 0)));
	SETCAR(args, findFun(s, rho));
	UNPROTECT(1);
    }

    if (TYPEOF(CAR(args)) == CLOSXP) {
	s = allocSExp(CLOSXP);
	SET_FORMALS(s, FORMALS(CAR(args)));
	SET_BODY(s, R_NilValue);
	SET_CLOENV(s, R_GlobalEnv);
	return s;
    }

    if (TYPEOF(CAR(args)) == BUILTINSXP || TYPEOF(CAR(args)) == SPECIALSXP) {
	char *nm = PRIMNAME(CAR(args));
	SEXP env, s2;
	PROTECT_INDEX xp;

	PROTECT_WITH_INDEX(env = findVarInFrame3(R_BaseEnv,
						 install(".ArgsEnv"), TRUE),
			   &xp);

	if (TYPEOF(env) == PROMSXP) REPROTECT(env = eval(env, R_BaseEnv), xp);
	PROTECT(s2 = findVarInFrame3(env, install(nm), TRUE));
	if(s2 != R_UnboundValue) {
	    s = duplicate(s2);
	    SET_CLOENV(s, R_GlobalEnv);
	    UNPROTECT(2);
	    return s;
	}
	UNPROTECT(1); /* s2 */
	REPROTECT(env = findVarInFrame3(R_BaseEnv, install(".GenericArgsEnv"),
					TRUE), xp);
	if (TYPEOF(env) == PROMSXP) REPROTECT(env = eval(env, R_BaseEnv), xp);
	PROTECT(s2 = findVarInFrame3(env, install(nm), TRUE));
	if(s2 != R_UnboundValue) {
	    s = allocSExp(CLOSXP);
	    SET_FORMALS(s, FORMALS(s2));
	    SET_BODY(s, R_NilValue);
	    SET_CLOENV(s, R_GlobalEnv);
	    UNPROTECT(2);
	    return s;
	}
	UNPROTECT(2);
    }
    return R_NilValue;
}

SEXP attribute_hidden do_formals(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return duplicate(FORMALS(CAR(args)));
    else
	return R_NilValue;
}

SEXP attribute_hidden do_body(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return duplicate(BODY_EXPR(CAR(args)));
    else return R_NilValue;
}

SEXP attribute_hidden do_bodyCode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return duplicate(BODY(CAR(args)));
    else return R_NilValue;
}

/* get environment from a subclass if possible; else return NULL */
#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : arg)


SEXP attribute_hidden do_envir(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (TYPEOF(CAR(args)) == CLOSXP)
	return CLOENV(CAR(args));
    else if (CAR(args) == R_NilValue)
	return R_GlobalContext->sysparent;
    else return getAttrib(CAR(args), R_DotEnvSymbol);
}

SEXP attribute_hidden do_envirgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, s = CAR(args);

    checkArity(op, args);
    check1arg(args, call, "x");

    env = CADR(args);

    if (TYPEOF(CAR(args)) == CLOSXP
	&& (isEnvironment(env) ||
	    isEnvironment(env = simple_as_environment(env)) ||
	    isNull(env))) {
	if (isNull(env))
	    error(_("use of NULL environment is defunct"));
	if(NAMED(s) > 1)
	    /* this copies but does not duplicate args or code */
	    s = duplicate(s);
	if (TYPEOF(BODY(s)) == BCODESXP)
	    /* switch to interpreted version if compiled */
	    SET_BODY(s, R_ClosureExpr(CAR(args)));
	SET_CLOENV(s, env);
    }
    else if (isNull(env) || isEnvironment(env) ||
	isEnvironment(env = simple_as_environment(env)))
	setAttrib(s, R_DotEnvSymbol, env);
    else
	error(_("replacement object is not an environment"));
    return s;
}


/** do_newenv() :  .Internal(new.env(hash, parent, size))
 *
 * @return a newly created environment()
 */
SEXP attribute_hidden do_newenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP enclos, size, ans;
    int hash;

    checkArity(op, args);

    hash = asInteger(CAR(args));
    args = CDR(args);
    enclos = CAR(args);
    if (isNull(enclos)) {
	error(_("use of NULL environment is defunct"));
	enclos = R_BaseEnv;
    } else
    if( !isEnvironment(enclos)   &&
	!isEnvironment((enclos = simple_as_environment(enclos))))
	error(_("'enclos' must be an environment"));

    if( hash ) {
	args = CDR(args);
	PROTECT(size = coerceVector(CAR(args), INTSXP));
	if (INTEGER(size)[0] == NA_INTEGER)
	    INTEGER(size)[0] = 0; /* so it will use the internal default */
	ans = R_NewHashedEnv(enclos, size);
	UNPROTECT(1);
    } else
	ans = NewEnvironment(R_NilValue, R_NilValue, enclos);
    return ans;
}

SEXP attribute_hidden do_parentenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP arg = CAR(args);

    if( !isEnvironment(arg)  &&
	!isEnvironment((arg = simple_as_environment(arg))))
	error( _("argument is not an environment"));
    if( arg == R_EmptyEnv )
	error(_("the empty environment has no parent"));
    return( ENCLOS(arg) );
}

SEXP attribute_hidden do_parentenvgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, parent;
    checkArity(op, args);

    env = CAR(args);
    if (isNull(env)) {
	error(_("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if( !isEnvironment(env) &&
	!isEnvironment((env = simple_as_environment(env))))
	error(_("argument is not an environment"));
    if( env == R_EmptyEnv )
	error(_("can not set parent of the empty environment"));
    parent = CADR(args);
    if (isNull(parent)) {
	error(_("use of NULL environment is defunct"));
	parent = R_BaseEnv;
    } else
    if( !isEnvironment(parent) &&
	!isEnvironment((parent = simple_as_environment(parent))))
	error(_("'parent' is not an environment"));

    SET_ENCLOS(env, parent);

    return( CAR(args) );
}

SEXP attribute_hidden do_envirName(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env = CAR(args), ans=mkString(""), res;

    checkArity(op, args);
    if (TYPEOF(env) == ENVSXP ||
	TYPEOF((env = simple_as_environment(env))) == ENVSXP) {
	if (env == R_GlobalEnv) ans = mkString("R_GlobalEnv");
	else if (env == R_BaseEnv) ans = mkString("base");
	else if (env == R_EmptyEnv) ans = mkString("R_EmptyEnv");
	else if (R_IsPackageEnv(env))
	    ans = ScalarString(STRING_ELT(R_PackageEnvName(env), 0));
	else if (R_IsNamespaceEnv(env))
	    ans = ScalarString(STRING_ELT(R_NamespaceEnvSpec(env), 0));
	else if (!isNull(res = getAttrib(env, R_NameSymbol))) ans = res;
    }
    return ans;
}

#ifdef Win32
# include "rgui_UTF8.h"
#endif
/* Uses R_alloc but called by a .Internal.  Result may be R_alloc-ed */
static const char *trChar(SEXP x)
{
    size_t n = strlen(CHAR(x));
    cetype_t ienc = getCharCE(x);

    if (ienc == CE_BYTES) {
	const char *p = CHAR(x), *q;
	char *pp = R_alloc(4*n+1, 1), *qq = pp, buf[5];
	for (q = p; *q; q++) {
	    unsigned char k = (unsigned char) *q;
	    if (k >= 0x20 && k < 0x80) {
		*qq++ = *q;
	    } else {
		snprintf(buf, 5, "\\x%02x", k);
		for(int j = 0; j < 4; j++) *qq++ = buf[j];
	    }
	}
	*qq = '\0';
	return pp;
    } else {
#ifdef Win32
	static char buf[106];
	char *p;
	/* Long strings will be rare, and few per cat() call so we
	   can afford to be profligate here: translateChar is */
	if (n < 100) p = buf; else p = R_alloc(n+7, 1);
	if (WinUTF8out && ienc == CE_UTF8) {
	    strcpy(p, UTF8in); strcat(p, CHAR(x)); strcat(p, UTF8out);
	    return p;
	} else
#endif
	    return translateChar(x);
    }
}

static void cat_newline(SEXP labels, int *width, int lablen, int ntot)
{
    Rprintf("\n");
    *width = 0;
    if (labels != R_NilValue) {
	Rprintf("%s ", EncodeString(STRING_ELT(labels, ntot % lablen),
				    1, 0, Rprt_adj_left));
	*width += Rstrlen(STRING_ELT(labels, ntot % lablen), 0) + 1;
    }
}

static void cat_sepwidth(SEXP sep, int *width, int ntot)
{
    if (sep == R_NilValue || LENGTH(sep) == 0)
	*width = 0;
    else
	*width = Rstrlen(STRING_ELT(sep, ntot % LENGTH(sep)), 0);
}

static void cat_printsep(SEXP sep, int ntot)
{
    const char *sepchar;
    if (sep == R_NilValue || LENGTH(sep) == 0)
	return;

    sepchar = trChar(STRING_ELT(sep, ntot % LENGTH(sep)));
    Rprintf("%s", sepchar);
    return;
}

typedef struct cat_info {
    Rboolean wasopen;
    int changedcon;
    Rconnection con;
} cat_info;

static void cat_cleanup(void *data)
{
    cat_info *pci = (cat_info *) data;
    Rconnection con = pci->con;
    Rboolean wasopen = pci->wasopen;
    int changedcon = pci->changedcon;

    con->fflush(con);
    if(changedcon) switch_stdout(-1, 0);
    /* previous line might have closed it */
    if(!wasopen && con->isopen) con->close(con);
#ifdef Win32
    WinUTF8out = FALSE;
#endif
}

SEXP attribute_hidden do_cat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    cat_info ci;
    RCNTXT cntxt;
    SEXP objs, file, fill, sepr, labs, s;
    int ifile;
    Rconnection con;
    int append;
    int i, iobj, n, nobjs, pwidth, width, sepw, lablen, ntot, nlsep, nlines;
    char buf[512];
    const char *p = "";

    checkArity(op, args);

    /* Use standard printing defaults */
    PrintDefaults();

    objs = CAR(args);
    args = CDR(args);

    file = CAR(args);
    ifile = asInteger(file);
    con = getConnection(ifile);
    if(!con->canwrite) /* if it is not open, we may not know yet */
	error(_("cannot write to this connection"));
    args = CDR(args);

    sepr = CAR(args);
    if (!isString(sepr))
	error(_("invalid '%s' specification"), "sep");
    nlsep = 0;
    for (i = 0; i < LENGTH(sepr); i++)
	if (strstr(CHAR(STRING_ELT(sepr, i)), "\n")) nlsep = 1; /* ASCII */
    args = CDR(args);

    fill = CAR(args);
    if ((!isNumeric(fill) && !isLogical(fill)) || (length(fill) != 1))
	error(_("invalid '%s' argument"), "fill");
    if (isLogical(fill)) {
	if (asLogical(fill) == 1)
	    pwidth = R_print.width;
	else
	    pwidth = INT_MAX;
    }
    else pwidth = asInteger(fill);
    if(pwidth <= 0) {
	warning(_("non-positive 'fill' argument will be ignored"));
	pwidth = INT_MAX;
    }
    args = CDR(args);

    labs = CAR(args);
    if (!isString(labs) && labs != R_NilValue)
	error(_("invalid '%s' argument"), "labels");
    lablen = length(labs);
    args = CDR(args);

    append = asLogical(CAR(args));
    if (append == NA_LOGICAL)
	error(_("invalid '%s' specification"), "append");

    ci.wasopen = con->isopen;

    ci.changedcon = switch_stdout(ifile, 0);
    /* will open new connection if required, and check for writeable */
#ifdef Win32
    /* do this after re-sinking output */
    WinCheckUTF8();
#endif

    ci.con = con;

    /* set up a context which will close the connection if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &cat_cleanup;
    cntxt.cenddata = &ci;

    nobjs = length(objs);
    width = 0;
    ntot = 0;
    nlines = 0;
    for (iobj = 0; iobj < nobjs; iobj++) {
	s = VECTOR_ELT(objs, iobj);
	if (iobj != 0 && !isNull(s))
	    cat_printsep(sepr, ntot++);
	n = length(s);
	/* 0-length objects are ignored */
	if (n > 0) {
	    if (labs != R_NilValue && (iobj == 0)
		&& (asInteger(fill) > 0)) {
		Rprintf("%s ", trChar(STRING_ELT(labs, nlines % lablen)));
		/* FIXME -- Rstrlen allows for double-width chars */
		width += Rstrlen(STRING_ELT(labs, nlines % lablen), 0) + 1;
		nlines++;
	    }
	    if (isString(s))
		p = trChar(STRING_ELT(s, 0));
	    else if (isSymbol(s)) /* length 1 */
		p = CHAR(PRINTNAME(s));
	    else if (isVectorAtomic(s)) {
		/* Not a string, as that is covered above.
		   Thus the maximum size is about 60.
		   The copy is needed as cat_newline might reuse the buffer.
		   Use strncpy is in case these assumptions change.
		*/
		p = EncodeElement(s, 0, 0, OutDec);
		strncpy(buf, p, 512); buf[511] = '\0';
		p = buf;
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
		errorcall(call,
			  _("argument %d (type '%s') cannot be handled by 'cat'"),
			  1+iobj, type2char(TYPEOF(s)));
	    /* FIXME : cat(...) should handle ANYTHING */
	    size_t w = strlen(p);
	    cat_sepwidth(sepr, &sepw, ntot);
	    if ((iobj > 0) && (width + w + sepw > pwidth)) {
		cat_newline(labs, &width, lablen, nlines);
		nlines++;
	    }
	    for (i = 0; i < n; i++, ntot++) {
		Rprintf("%s", p);
		width += (int)(w + sepw);
		if (i < (n - 1)) {
		    cat_printsep(sepr, ntot);
		    if (isString(s))
			p = trChar(STRING_ELT(s, i+1));
		    else {
			p = EncodeElement(s, i+1, 0, OutDec);
			strncpy(buf, p, 512); buf[511] = '\0';
			p = buf;
		    }
		    w = (int) strlen(p);
		    cat_sepwidth(sepr, &sepw, ntot);
		    /* This is inconsistent with the version above.
		       As from R 2.3.0, fill <= 0 is ignored. */
		    if ((width + w + sepw > pwidth) && pwidth) {
			cat_newline(labs, &width, lablen, nlines);
			nlines++;
		    }
		} else ntot--; /* we don't print sep after last, so don't advance */
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

SEXP attribute_hidden do_makelist(SEXP call, SEXP op, SEXP args, SEXP rho)
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

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_expression(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, ans, nms;
    int i, n, named;
    named = 0;
    n = length(args);
    PROTECT(ans = allocVector(EXPRSXP, n));
    a = args;
    for (i = 0; i < n; i++) {
	if(NAMED(CAR(a)))
	    SET_VECTOR_ELT(ans, i, duplicate(CAR(a)));
	else
	    SET_VECTOR_ELT(ans, i, CAR(a));
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
SEXP attribute_hidden do_makevector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    R_xlen_t len;
    SEXP s;
    SEXPTYPE mode;
    checkArity(op, args);
    if (length(CADR(args)) != 1) error(_("invalid '%s' argument"), "length");
    len = asVecSize(CADR(args));
    if (len < 0) error(_("invalid '%s' argument"), "length");
    s = coerceVector(CAR(args), STRSXP);
    if (length(s) != 1) error(_("invalid '%s' argument"), "mode");
    mode = str2type(CHAR(STRING_ELT(s, 0))); /* ASCII */
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
    case RAWSXP:
	s = allocVector(mode, len);
	break;
    case LISTSXP:
	if (len > INT_MAX) error("too long for a pairlist");
	s = allocList((int) len);
	break;
    default:
	error(_("vector: cannot make a vector of mode '%s'."),
	      translateChar(STRING_ELT(s, 0))); /* should be ASCII */
    }
    if (mode == INTSXP || mode == LGLSXP)
	Memzero(INTEGER(s), len);
    else if (mode == REALSXP)
	Memzero(REAL(s), len);
    else if (mode == CPLXSXP)
	Memzero(COMPLEX(s), len);
    else if (mode == RAWSXP)
	Memzero(RAW(s), len);
    /* other cases: list/expression have "NULL", ok */
    return s;
}


/* do_lengthgets: assign a length to a vector or a list */
/* (if it is vectorizable). We could probably be fairly */
/* clever with memory here if we wanted to. */

/* used in connections.c */
SEXP xlengthgets(SEXP x, R_xlen_t len)
{
    R_xlen_t lenx, i;
    SEXP rval, names, xnames, t;
    if (!isVector(x) && !isVectorizable(x))
	error(_("cannot set length of non-vector"));
    lenx = xlength(x);
    if (lenx == len)
	return (x);
    PROTECT(rval = allocVector(TYPEOF(x), len));
    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
    if (xnames != R_NilValue)
	names = allocVector(STRSXP, len);
    else names = R_NilValue;	/*- just for -Wall --- should we do this ? */
    switch (TYPEOF(x)) {
    case NILSXP:
	break;
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
    case RAWSXP:
	for (i = 0; i < len; i++)
	    if (i < lenx) {
		RAW(rval)[i] = RAW(x)[i];
		if (xnames != R_NilValue)
		    SET_STRING_ELT(names, i, STRING_ELT(xnames, i));
	    }
	    else
		RAW(rval)[i] = (Rbyte) 0;
	break;
    default:
	UNIMPLEMENTED_TYPE("length<-", x);
    }
    if (isVector(x) && xnames != R_NilValue)
	setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(2);
    return rval;
}

/* public older version */
SEXP lengthgets(SEXP x, R_len_t len)
{
    return xlengthgets(x, (R_xlen_t) len);
}


SEXP attribute_hidden do_lengthgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, ans;

    checkArity(op, args);
    check1arg(args, call, "x");

    x = CAR(args);

    if (PRIMVAL(op)) { /* xlength<- */
	if(isObject(x) && DispatchOrEval(call, op, "length<-", args,
					 rho, &ans, 0, 1))
	    return(ans);
	if (!isVector(x) && !isVectorizable(x))
	    error(_("invalid argument"));
	if (length(CADR(args)) != 1)
	    error(_("invalid value"));
	R_xlen_t len = asVecSize(CADR(args));
	return xlengthgets(x, len);
    }
    if(isObject(x) && DispatchOrEval(call, op, "length<-", args,
				     rho, &ans, 0, 1))
	return(ans);
    if (!isVector(x) && !isVectorizable(x))
	error(_("invalid argument"));
    if (length(CADR(args)) != 1)
	error(_("invalid value"));
    R_xlen_t len = asVecSize(CADR(args));
    if (len < 0) error(_("invalid value"));
    if (len > R_LEN_T_MAX) {
#ifdef LONG_VECTOR_SUPPORT
	return xlengthgets(x, len);
#else
	error(_("vector size specified is too large"));
	return x; /* -Wall */
#endif
    }
    return lengthgets(x, (R_len_t) len);
}

/* Expand dots in args, but do not evaluate */
static SEXP expandDots(SEXP el, SEXP rho)
{
    SEXP ans, tail;

    PROTECT(el); /* in do_switch, this is already protected */
    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while (el != R_NilValue) {
	if (CAR(el) == R_DotsSymbol) {
	    SEXP h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(CAR(h), R_NilValue));
		    tail = CDR(tail);
		    if(TAG(h) != R_NilValue) SET_TAG(tail, TAG(h));
		    h = CDR(h);
		}
	    } else if (h != R_MissingArg)
		error(_("'...' used in an incorrect context"));
	} else {
	    SETCDR(tail, CONS(CAR(el), R_NilValue));
	    tail = CDR(tail);
	    if(TAG(el) != R_NilValue) SET_TAG(tail, TAG(el));
	}
	el = CDR(el);
    }
    UNPROTECT(2);
    return CDR(ans);
}

/* This function is used in do_switch to record the default value and
   to detect multiple defaults, which are not allowed as of 2.13.x */

static SEXP setDflt(SEXP arg, SEXP dflt)
{
    if (dflt) {
	SEXP dflt1, dflt2;
	PROTECT(dflt1 = deparse1line(dflt, TRUE));
	PROTECT(dflt2 = deparse1line(CAR(arg), TRUE));
	error(_("duplicate 'switch' defaults: '%s' and '%s'"),
	      CHAR(STRING_ELT(dflt1, 0)), CHAR(STRING_ELT(dflt2, 0)));
	UNPROTECT(2); /* won't get here, but just for good form */
    }
    return(CAR(arg));
}

/* For switch, evaluate the first arg, if it is a character then try
 to match the name with the remaining args, and evaluate the match. If
 the value is missing then take the next non-missing arg as the value.
 Then things like switch(as.character(answer), yes=, YES=1, no=, NO=2,
 3) will work.  But if there is no 'next', return NULL. One arg beyond
 the first is allowed to be unnamed; it becomes the default value if
 there is no match.

 If the value of the first arg is not a character string
 then coerce it to an integer k and choose the kth argument from those
 that remain provided 1 < k < nargs.

 Changed in 2.11.0 to be primitive, so the wrapper does not partially
 match to EXPR, and to return NULL invisibly if it is an error
 condition.

 This is a SPECIALSXP, so arguments need to be evaluated as needed.
  And (see names.c) X=2, so it defaults to a visible value.
*/


SEXP attribute_hidden do_switch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int argval, nargs = length(args);
    SEXP x, y, z, w, ans, dflt = NULL;

    if (nargs < 1) errorcall(call, _("'EXPR' is missing"));
    check1arg(args, call, "EXPR");
    PROTECT(x = eval(CAR(args), rho));
    if (!isVector(x) || length(x) != 1)
	errorcall(call, _("EXPR must be a length 1 vector"));
    if (nargs > 1) {
	/* There is a complication: if called from lapply
	   there may be a ... argument */
	PROTECT(w = expandDots(CDR(args), rho));
	if (isString(x)) {
	    for (y = w; y != R_NilValue; y = CDR(y)) {
		if (TAG(y) != R_NilValue) {
		    if (pmatch(STRING_ELT(x, 0), TAG(y), 1 /* exact */)) {
			/* Find the next non-missing argument.
			   (If there is none, return NULL.) */
			while (CAR(y) == R_MissingArg) {
			    y = CDR(y);
			    if (y == R_NilValue) break;
			    if (TAG(y) == R_NilValue) dflt = setDflt(y, dflt);
			}
			if (y == R_NilValue) {
			    R_Visible = FALSE;
			    UNPROTECT(2);
			    return R_NilValue;
			}
			/* Check for multiple defaults following y.  This loop
			   is not necessary to determine the value of the
			   switch(), but it should be fast and will detect
			   typos. */
			for (z = CDR(y); z != R_NilValue; z = CDR(z))
			    if (TAG(z) == R_NilValue) dflt = setDflt(z, dflt);

			ans =  eval(CAR(y), rho);
			UNPROTECT(2);
			return ans;
		    }
		} else
		    dflt = setDflt(y, dflt);
	    }
	    if (dflt) {
		ans =  eval(dflt, rho);
		UNPROTECT(2);
		return ans;
	    }
	    /* fall through to error */
	} else { /* Treat as numeric */
	    argval = asInteger(x);
	    if (argval != NA_INTEGER && argval >= 1 && argval <= length(w)) {
		SEXP alt = CAR(nthcdr(w, argval - 1));
		if (alt == R_MissingArg)
		    error("empty alternative in numeric switch");
		ans =  eval(alt, rho);
		UNPROTECT(2);
		return ans;
	    }
	    /* fall through to error */
	}
	UNPROTECT(1); /* w */
    }
    /* an error */
    UNPROTECT(1); /* x */
    R_Visible = FALSE;
    return R_NilValue;
}
