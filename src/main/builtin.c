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
#include "Print.h"
#include "Fileio.h"

SEXP do_delay(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP expr, env;
	checkArity(op, args);
	expr = CAR(args);
	env = eval(CADR(args), rho);
	if(!isEnvironment(env))
		errorcall(call, "invalid argument\n");
	return mkPROMISE(expr, env);
}

SEXP do_onexit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	RCNTXT *ctxt;
	SEXP code;

	switch (length(args)) {
	case 0:
		code = R_NilValue;
		break;
	case 1:
		code = CAR(args);
		break;
	default:
		errorcall(call, "invalid number of arguments\n");
	}
	ctxt = R_GlobalContext;
	while (ctxt != R_ToplevelContext && ctxt->callflag != CTXT_RETURN)
		ctxt = ctxt->nextcontext;
	if (ctxt->callflag == CTXT_RETURN)
		ctxt->conexit = code;
	return R_NilValue;
}

SEXP do_args(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP s;

	checkArity(op,args);

	if (TYPEOF(CAR(args)) == STRSXP && length(CAR(args))==1) {
		PROTECT(s = install(CHAR(STRING(CAR(args))[0])));
		CAR(args) = findFun(s, rho);
		UNPROTECT(1);
	}

	if (TYPEOF(CAR(args)) == CLOSXP) {
		s = allocSExp(CLOSXP);
		FORMALS(s) = FORMALS(CAR(args));
		BODY(s) = R_NilValue;
		CLOENV(s) = R_GlobalEnv;
		return(s);
	}

	return R_NilValue;
}

SEXP do_formals(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);

	if( TYPEOF(CAR(args)) == CLOSXP )
		return duplicate(FORMALS(CAR(args)));
	else
		return R_NilValue;
}

SEXP do_body(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);

	if( TYPEOF(CAR(args)) == CLOSXP )
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
	return R_NilValue;
}

SEXP do_envirgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);

	if (TYPEOF(CAR(args)) == CLOSXP && isEnvironment(CADR(args)))
		CLOENV(CAR(args)) = CADR(args);
	return CAR(args);
}

static void cat_newline(SEXP labels, int * width, int lablen, int ntot)
{
	Rprintf("\n");
	*width = 0;
	if (labels != R_NilValue) {
		Rprintf("%s ", EncodeString(CHAR(STRING(labels)[ntot % lablen]), 1, 0, adj_left));
		*width += Rstrlen(CHAR(STRING(labels)[ntot % lablen])) + 1;
	}
}

static void cat_sepwidth(SEXP sep, int * width, int ntot)
{
	if (sep == R_NilValue || LENGTH(sep) == 0)
		*width = 0;
	else
		*width = Rstrlen(CHAR(STRING(sep)[ntot % LENGTH(sep)]));
}

static void cat_printsep(SEXP sep, int ntot)
{
	char *sepchar;

	if (sep == R_NilValue || LENGTH(sep) == 0)
		return;

	sepchar = CHAR(STRING(sep)[ntot % LENGTH(sep)]);
	Rprintf("%s",sepchar);
	return;
}

SEXP do_cat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP a, objs, file, fill, sepr, labs, s;
	FILE *savefp;
	int havefile, append;
	int w, i, n, pwidth, width, sepw, lablen, ntot, nlsep, nlines;
	char *p, buf[512];

	checkArity(op, args);

		/* Use standard printing defaults */

	PrintDefaults(rho);

	objs = CAR(args); args = CDR(args);

	file = CAR(args); args = CDR(args);
	if(!isString(file) || length(file) != 1)
		errorcall(call, "invalid file= specification\n");

	sepr = CAR(args); args = CDR(args);
	if (!isString(sepr))
		errorcall(call, "invalid sep= specification\n");
	nlsep = 0;
	for (i = 0; i < LENGTH(sepr); i++)
		if (strstr(CHAR(STRING(sepr)[i]), "\n")) nlsep = 1;

	fill = CAR(args); args = CDR(args);
	if ((!isNumeric(fill) && !isLogical(fill)) || (length(fill) != 1))
		errorcall(call, "invalid fill argument\n");
	if (isLogical(fill)) {
		if (asLogical(fill) == 1)
			pwidth = PRINT_WIDTH;
		else
			pwidth = INT_MAX;
	}
	else pwidth = asInteger(fill);

	labs = CAR(args); args = CDR(args);
	if (!isString(labs) && labs != R_NilValue)
		errorcall(call, "invalid label argument\n");
	lablen = length(labs);

	append = asLogical(CAR(args));
	if(append == NA_LOGICAL)
		errorcall(call, "invalid append specification\n");

	if (strlen(CHAR(STRING(file)[0])) > 0) {
		savefp = R_Outputfile;
		if (append)
			R_Outputfile = R_fopen(
				R_ExpandFileName(CHAR(STRING(file)[0])), "a");
		else
			R_Outputfile = R_fopen(
				R_ExpandFileName(CHAR(STRING(file)[0])), "w");
		if (!R_Outputfile) {
			R_Outputfile = savefp;
			errorcall(call, "unable to open file\n");
		}
		havefile = 1 ;
	}
	else havefile = 0;

	n = 0;
	for (a = objs; a != R_NilValue; a = CDR(a)) {
		if (!isVector(CAR(a)) && !isNull(CAR(a)))
			errorcall(call, "argument %d has invalid type\n", n+1);
		n += 1;
	}
	width = 0;
	ntot = 0;
	nlines = 0;
	for (a = objs; a != R_NilValue; a = CDR(a)) {
		s = CAR(a);
		if (a != objs && !isNull(s) )
			cat_printsep(sepr, 0);
		n = length(s);
		if (n > 0) {
			if (labs != R_NilValue && a == objs) {
				Rprintf("%s ", CHAR(STRING(labs)[nlines]));
				width += strlen(CHAR(STRING(labs)[nlines % lablen])) + 1;
				nlines++;
			}
			if(isString(s))
				p = CHAR(STRING(s)[0]);
			else {
				p = EncodeElement(s, 0, 0);
				strcpy(buf,p);
				p=buf;
			}
			w = strlen(p);
			cat_sepwidth(sepr, &sepw, ntot);
			if (a != objs && (width + w + sepw > pwidth)) {
				cat_newline(labs, &width, lablen, nlines);
				nlines++;
			}
			for (i = 0; i < n; i++, ntot++) {
				Rprintf("%s", p);
				width += w + sepw;
				if (i < (n - 1)) {
					cat_printsep(sepr, ntot);
					if(isString(s))
						p = CHAR(STRING(s)[i+1]);
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
	if (havefile) {
		fclose(R_Outputfile);
		R_Outputfile = savefp;
	}
	else
		fflush(stdout);
	return R_NilValue;
}


SEXP do_makelist(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP s = args;
	while (s != R_NilValue) {
		if (NAMED(CAR(s)))
			CAR(s) = duplicate(CAR(s));
		s = CDR(s);
	}
	return args;
}


SEXP do_makefactor(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, y;
	int i, j, nx, nl, ord;

	checkArity(op, args);
	x = CAR(args) = coerceVector(CAR(args), INTSXP);
	nl = asInteger(CADR(args));
	if (nl == NA_INTEGER || nl < 0)
		errorcall(call, "invalid number of factor levels\n");
	ord = asLogical(CADDR(args));
	if(ord == NA_LOGICAL) ord = 0;
	nx = LENGTH(x);
	PROTECT(y = allocVector(ord ? ORDSXP : FACTSXP, nx));
	LEVELS(y) = nl;
	for (i = 0; i < nx; i++) {
		j = INTEGER(x)[i];
		if (1 <= j && j <= nl)
			FACTOR(y)[i] = j;
		else
			FACTOR(y)[i] = NA_INTEGER;
	}
	if(ord) {
		PROTECT(x = allocVector(STRSXP, 2));
		STRING(x)[0] = mkChar("ordered");
		STRING(x)[1] = mkChar("factor");
	}
	else {
		PROTECT(x = allocVector(STRSXP, 1));
		STRING(x)[0] = mkChar("factor");
	}
	setAttrib(y, R_ClassSymbol, x);
	UNPROTECT(2);
	return y;
}

SEXP do_expression(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP a, blank, ans, nms;
	int i, n, named;
	named = 0;
	n = length(args);
	PROTECT(ans = allocVector(EXPRSXP, n));
	a = args;
	for(i=0 ; i<n ; i++) {
		VECTOR(ans)[i] = duplicate(CAR(a));
		if (TAG(a) != R_NilValue) named = 1;
		a = CDR(a);
	}
	if(named) {
		PROTECT(blank=mkChar(""));
		PROTECT(nms = allocVector(STRSXP, n));
		a = args;
		for(i=0 ; i<n ; i++) {
			if(TAG(a) != R_NilValue)
				STRING(nms)[i] = PRINTNAME(TAG(a));
			else
				STRING(nms)[i] = blank;
			a = CDR(a);
		}
		setAttrib(ans, R_NamesSymbol, nms);
		UNPROTECT(2);
	}
	UNPROTECT(1);
	return ans;
}

SEXP do_makevector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int len, i;
	SEXP s, blank;
	SEXPTYPE mode;

	checkArity(op, args);

	len = asInteger(CADR(args));
	s = coerceVector(CAR(args), STRSXP);
	if (length(s) == 0)
		error("vector: zero-length type argument\n");
	mode = str2type(CHAR(STRING(s)[0]));
	if (mode == -1 && streql(CHAR(STRING(s)[0]), "double"))
		mode = REALSXP;
	if ((CHARSXP < mode && mode <= STRSXP) || mode == EXPRSXP)
		s = allocVector(mode, len);
	else if (mode == LISTSXP)
		s = allocList(len);
	else
		error("vector: cannot make a vector of the type specified\n");
	if (mode == INTSXP || mode == LGLSXP)
		for (i = 0; i < len; i++)
			INTEGER(s)[i] = 0;
	if (mode == FACTSXP || mode == ORDSXP)
		for (i = 0; i < len; i++)
			FACTOR(s)[i] = 1;
	else if (mode == REALSXP)
		for (i = 0; i < len; i++)
			REAL(s)[i] = 0.0;
	else if (mode == STRSXP) {
		PROTECT(s);
		PROTECT(blank = mkChar(""));
		for (i = 0; i < len; i++)
			STRING(s)[i] = blank;
		UNPROTECT(2);
	}
	return s;
}


	/* do_lengthgets: assign a length to a vector or a list */
	/* (if it is vectorizable). We could probably be fairly */
	/* clever with memory here if we wanted to. */

SEXP do_lengthgets(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int len, lenx, i;
	SEXP rval, x, names, xnames, t;

	checkArity(op, args);
	x = CAR(args);

	if (!isVector(x) && !isVectorizable(x))
		error("length<- invalid first argument\n");

	if (length(CADR(args)) != 1)
		error("length<- invalid second argument\n");

	len = asInteger(CADR(args));
	if (len == NA_INTEGER)
		error("length<- missing value for length\n");

	lenx = length(x);
	if (lenx == len)
		return (x);

	rval = allocVector(TYPEOF(x), len);

	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	if (xnames != R_NilValue)
		names = allocVector(STRSXP, len);

	switch (TYPEOF(x)) {
	case LGLSXP:
	case FACTSXP:
	case ORDSXP:
	case INTSXP:
		for (i = 0; i < len; i++)
			if (i < lenx) {
				INTEGER(rval)[i] = INTEGER(x)[i];
				if (xnames != R_NilValue)
					STRING(names)[i] = STRING(xnames)[i];
			}
			else
				INTEGER(rval)[i] = NA_INTEGER;
		break;
	case REALSXP:
		for (i = 0; i < len; i++)
			if (i < lenx) {
				REAL(rval)[i] = REAL(x)[i];
				if (xnames != R_NilValue)
					STRING(names)[i] = STRING(xnames)[i];
			}
			else
				REAL(rval)[i] = NA_REAL;
		break;
#ifdef COMPLEX_DATA
	case CPLXSXP:
		for (i = 0; i < len; i++)
			if (i < lenx) {
				COMPLEX(rval)[i] = COMPLEX(x)[i];
				if (xnames != R_NilValue)
					STRING(names)[i] = STRING(xnames)[i];
			}
			else {
				COMPLEX(rval)[i].r = NA_REAL;
				COMPLEX(rval)[i].i = NA_REAL;
			}
		break;
#endif
	case STRSXP:
		for (i = 0; i < len; i++)
			if (i < lenx) {
				STRING(rval)[i] = STRING(x)[i];
				if (xnames != R_NilValue)
					STRING(names)[i] = STRING(xnames)[i];
			}
			else
				STRING(rval)[i] = NA_STRING;
		break;
	case LISTSXP:
		for (t = rval; t != R_NilValue; t = CDR(t), x = CDR(x)) {
			CAR(t) = CAR(x);
			TAG(t) = TAG(x);
		}
	}
	if (isVector(x) && xnames != R_NilValue)
		setAttrib(rval, R_NamesSymbol, names);
	UNPROTECT(1);
	return rval;
}

SEXP do_assign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP name, val, aenv;
	int ginherits;

	checkArity(op, args);
	name = findVar(CAR(args), rho);

	PROTECT(args = evalList(args, rho));

	if (!isString(CAR(args)) || length(CAR(args)) == 0)
		error("assign: invalid first argument\n");
	else
		name = install(CHAR(STRING(CAR(args))[0]));
	PROTECT(val = eval(CADR(args), rho));

	R_Visible = 0;

	aenv = CAR(CDDR(args));
	if (TYPEOF(aenv) != ENVSXP && aenv != R_NilValue)
		error("invalid envir argument\n");

	if (isLogical(CAR(nthcdr(args, 3))))
		ginherits = LOGICAL(CAR(nthcdr(args, 3)))[0];
	else
		error("get: invalid inherits argument\n");
	if (ginherits)
		setVar(name, val, aenv);
	else
		defineVar(name, val, aenv);
	UNPROTECT(2);
	return val;
}

/*
 * do_remove has 3 arguments, a list of names to remove, an optional
 * environment (if missing set it to R_GlobalEnv) and inherits, a logical
 * indicating whether to look in the parent env if a symbol is not found in
 * the supplied env. This is ignored if environment is not specified
 */

SEXP do_remove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP name, aenv, tsym, tenv, tframe;
	int ginherits;
	int set, i;

	checkArity(op, args);

	name = CAR(args);
	if (!isString(name))
		error("invalid first argument to remove.\n");

	if (CADR(args) != R_NilValue) {
		if (TYPEOF(CADR(args)) != ENVSXP)
			error("invalid envir argument\n");
		else
			aenv = CADR(args);
	}
	else
		aenv = R_GlobalContext->sysparent;

	if (isLogical(CAR(nthcdr(args, 2))))
		ginherits = LOGICAL(CAR(nthcdr(args, 2)))[0];
	else
		error("get: invalid inherits argument\n");

	for (i=0 ; i<LENGTH(name) ; i++) {
		set = 0;
		tsym = install(CHAR(STRING(name)[i]));
		tenv = aenv;
	rmset:	for (tframe = FRAME(tenv); tframe != R_NilValue; tframe = CDR(tframe))
			if (TAG(tframe) == tsym) {
				unbindVar(tsym, tenv);
				set = 1;
			}
		if (ginherits && !set && (tenv = ENCLOS(tenv)) != R_NilValue)
			goto rmset;
		if (!set)
			warning("remove: variable \"%s\" was not found\n", CHAR(PRINTNAME(tsym)));
	}
	return R_NilValue;
}

/*
 * do_get returns the SEXP associated with the character argument, do_get
 * needs the environment of the calling function as a default,
 *
 */

#define FUNSXP 999

SEXP do_get(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP rval, genv, t1;
	SEXPTYPE gmode;
	int ginherits, where;

	checkArity(op, args);

	/* Grab the environment off the first arg */
	/* for use as the default environment. */
	/* TODO: Don't we have a better way of doing this */
	/* using sys.xxx now? */

	rval = findVar(CAR(args), rho);
	if (TYPEOF(rval) == PROMSXP)
		genv = PRENV(rval);

	/* Now we can evaluate the arguments */

	args = evalList(args, rho);

	/* The first arg is the object name */
	/* It must be present and a string */

	if (!isString(CAR(args)) || length(CAR(args)) < 1
	    || strlen(CHAR(STRING(CAR(args))[0])) == 0)
		errorcall(call, "invalid first argument\n");
	else
		t1 = install(CHAR(STRING(CAR(args))[0]));

	/* Now we get the where= argument */

	if (CADR(args) != R_NilValue) {
		if (TYPEOF(CADR(args)) == REALSXP || TYPEOF(CADR(args)) == INTSXP) {
			where = asInteger(CADR(args));
			genv = sysframe(where,R_GlobalContext);
		}
		else if (TYPEOF(CADR(args)) != ENVSXP)
			errorcall(call,"invalid envir argument\n");
		else
			genv = CADR(args);
	}

	/* The mode of the object being sought */

	if (isString(CAR(CDDR(args)))) {
		if(!strcmp(CHAR(STRING(CAR(CDDR(args)))[0]),"function"))
			gmode = FUNSXP;
		else
			gmode = str2type(CHAR(STRING(CAR(CDDR(args)))[0]));
	} else errorcall(call,"invalid mode argument\n");

	if (isLogical(CAR(nthcdr(args, 3))))
		ginherits = LOGICAL(CAR(nthcdr(args, 3)))[0];
	else
		errorcall(call,"invalid inherits argument\n");

		/* Search for the object */
	rval = findVar1(t1, genv, gmode, ginherits);

	if (PRIMVAL(op)) {	/* we have a get */
		if (rval == R_UnboundValue)
			errorcall(call,"variable \"%s\" was not found\n", CHAR(PRINTNAME(t1)));
		/* We need to evaluate if it is a promise */

		if(TYPEOF(rval) == PROMSXP)
			rval = eval(rval, genv);
		NAMED(rval) = 1;
		return rval;
	}
	else {
		if (rval == R_UnboundValue)
			ginherits = 0;
		else
			ginherits = 1;
		rval = allocVector(LGLSXP, 1);
		LOGICAL(rval)[0] = ginherits;
		return (rval);
	}
}

SEXP findVar1(SEXP symbol, SEXP rho, SEXPTYPE mode, int inherits)
{
	SEXP vl;

	while (rho != R_NilValue) {
		vl = findVarInFrame(FRAME(rho), symbol);
		if (vl != R_UnboundValue ) {
			if(mode == ANYSXP || TYPEOF(vl) == mode) return vl;
			if(mode == FUNSXP && (
				TYPEOF(vl) == CLOSXP ||
				TYPEOF(vl) == BUILTINSXP ||
				TYPEOF(vl) == SPECIALSXP)) return (vl);
		}
		if (inherits)
			rho = ENCLOS(rho);
		else
			return (R_UnboundValue);
	}
	return (SYMVALUE(symbol));
}


/*
 * for switch, evaluate the first arg, if it is a character then try to match
 * the name with the remaining args, and evaluate the match, if there is no
 * match then evaluate the first unnamed arg.  If the value of the first arg
 * is not a character string then coerce it to integer (k) and choose the kth
 * argument from those that remain provided 0 < k < (nargs-1)
 * For character matching, if the value is missing then take the next
 * non-missing arg as the value
 * then things like
 *  switch(as.character(answer), yes=, YES=1, no=, NO=2, 3)
 * work
 */

SEXP switchList(SEXP el, SEXP rho)
{
	SEXP h;

	if (CAR(el) == R_DotsSymbol) {
		h = findVar(CAR(el), rho);
		if (h == R_NilValue)
			return R_NilValue;
		if (TYPEOF(h) != DOTSXP) {
			if (h == R_MissingArg)
				return R_MissingArg;
			error("... used in an incorrect context\n");
		}
		return h;
	}
	else
		error("bad parameter in switch \n");
}

SEXP do_switch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int argval;
	SEXP x, y, w;

	x = eval(CAR(args), rho);
	if (!isVector(x) && length(x) != 1)
		error("switch: EXPR must return a length 1 vector\n");

	PROTECT(w = switchList(CDR(args), rho));
	if (isString(x)) {
		for (y = w; y != R_NilValue; y = CDR(y))
			if (TAG(y) != R_NilValue && pmatch(STRING(x)[0], TAG(y), 1)) {
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
