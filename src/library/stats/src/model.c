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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

/* inline-able versions, used just once! */
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
 */

/* model.frame(terms, rownames, variables, varnames, */
/*             dots, dotnames, subset, na.action) */

SEXP modelframe(SEXP args)
{
    SEXP terms, data, names, variables, varnames, dots, dotnames, na_action;
    SEXP ans, row_names, subset, tmp, rho;
    char buf[256];
    int i, j, nr, nc;
    int nvars, ndots, nactualdots;

    args = CDR(args);
    terms = CAR(args); args = CDR(args);
    row_names = CAR(args); args = CDR(args);
    variables = CAR(args); args = CDR(args);
    varnames = CAR(args); args = CDR(args);
    dots = CAR(args); args = CDR(args);
    dotnames = CAR(args); args = CDR(args);
    subset = CAR(args); args = CDR(args);
    na_action = CAR(args); args = CDR(args);
    rho = CAR(args);

    /* Argument Sanity Checks */

    if (!isNewList(variables))
	error(_("invalid variables"));
    if (!isString(varnames))
	error(_("invalid variable names"));
    if ((nvars = length(variables)) != length(varnames))
	error(_("number of variables != number of variable names"));

    if (!isNewList(dots))
	error(_("invalid extra variables"));
    if ((ndots = length(dots)) != length(dotnames))
	error(_("number of variables != number of variable names"));
    if ( ndots && !isString(dotnames))
	error(_("invalid extra variable names"));

    /*  check for NULL extra arguments -- moved from interpreted code */

    nactualdots = 0;
    for (i = 0; i < ndots; i++)
	if (VECTOR_ELT(dots, i) != R_NilValue) nactualdots++;

    /* Assemble the base data frame. */

    PROTECT(data = allocVector(VECSXP, nvars + nactualdots));
    PROTECT(names = allocVector(STRSXP, nvars + nactualdots));

    for (i = 0; i < nvars; i++) {
	SET_VECTOR_ELT(data, i, VECTOR_ELT(variables, i));
	SET_STRING_ELT(names, i, STRING_ELT(varnames, i));
    }
    for (i = 0,j = 0; i < ndots; i++) {
	const char *ss;
	if (VECTOR_ELT(dots, i) == R_NilValue) continue;
	ss = translateChar(STRING_ELT(dotnames, i));
	if(strlen(ss) + 3 > 256) error(_("overlong names in '%s'"), ss);
	sprintf(buf, "(%s)", ss);
	SET_VECTOR_ELT(data, nvars + j, VECTOR_ELT(dots, i));
	SET_STRING_ELT(names, nvars + j,  mkChar(buf));
	j++;
    }
    setAttrib(data, R_NamesSymbol, names);
    UNPROTECT(2);

    /* Sanity checks to ensure that the the answer can become */
    /* a data frame.  Be deeply suspicious here! */

    nc = length(data);
    nr = 0;			/* -Wall */
    if (nc > 0) {
	nr = nrows(VECTOR_ELT(data, 0));
	for (i = 0; i < nc; i++) {
	    ans = VECTOR_ELT(data, i);
	    switch(TYPEOF(ans)) {
	    case LGLSXP:
	    case INTSXP:
	    case REALSXP:
	    case CPLXSXP:
	    case STRSXP:
	    case RAWSXP:
		break;
	    default:
		error(_("invalid type (%s) for variable '%s'"),
		      type2char(TYPEOF(ans)),
		      translateChar(STRING_ELT(names, i)));
	    }
	    if (nrows(ans) != nr)
		error(_("variable lengths differ (found for '%s')"),
		      translateChar(STRING_ELT(names, i)));
	}
    } else nr = length(row_names);

    PROTECT(data);
    PROTECT(subset);

    /* Turn the data "list" into a "data.frame" */
    /* so that subsetting methods will work. */
    /* To do this we must attach "class"  and */
    /* "row.names" attributes */

    PROTECT(tmp = mkString("data.frame"));
    setAttrib(data, R_ClassSymbol, tmp);
    UNPROTECT(1);
    if (length(row_names) == nr) {
	setAttrib(data, R_RowNamesSymbol, row_names);
    } else {
	/*
	PROTECT(row_names = allocVector(INTSXP, nr));
	for (i = 0; i < nr; i++) INTEGER(row_names)[i] = i+1; */
	PROTECT(row_names = allocVector(INTSXP, 2));
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nr;
	setAttrib(data, R_RowNamesSymbol, row_names);
	UNPROTECT(1);
    }

    /* Do the subsetting, if required. */
    /* Need to save and restore 'most' attributes */

    if (subset != R_NilValue) {
	PROTECT(tmp=install("[.data.frame"));
	PROTECT(tmp=LCONS(tmp,list4(data,subset,R_MissingArg,mkFalse())));
	data = eval(tmp, rho);
	UNPROTECT(2);
    }
    UNPROTECT(2);
    PROTECT(data);

    /* finally, we run na.action on the data frame */
    /* usually, this will be na.omit */

    if (na_action != R_NilValue) {
	/* some na.actions need this to distinguish responses from
	   explanatory variables */
	setAttrib(data, install("terms"), terms);
	if (isString(na_action) && length(na_action) > 0)
	    na_action = install(translateChar(STRING_ELT(na_action, 0)));
	PROTECT(na_action);
	PROTECT(tmp = lang2(na_action, data));
	PROTECT(ans = eval(tmp, rho));
	if (!isNewList(ans) || length(ans) != length(data))
	    error(_("invalid result from na.action"));
	/* need to transfer _all but tsp and dim_ attributes, possibly lost
	   by subsetting in na.action.  */
	for ( i = length(ans) ; i-- ; )
		copyMostAttribNoTs(VECTOR_ELT(data, i),VECTOR_ELT(ans, i));

	UNPROTECT(3);
    }
    else ans = data;
    UNPROTECT(1);
    PROTECT(ans);

    /* Finally, tack on a terms attribute
       Now done at R level.
       setAttrib(ans, install("terms"), terms); */
    UNPROTECT(1);
    return ans;
}


	/* The code below is related to model expansion */
	/* and is ultimately called by modelmatrix. */

static void firstfactor(double *x, int nrx, int ncx,
			double *c, int nrc, int ncc, int *v)
{
    double *cj, *xj;
    int i, j;

    for (j = 0; j < ncc; j++) {
	xj = &x[j*nrx];
	cj = &c[j*nrc];
	for (i = 0; i < nrx; i++)
	    if(v[i] == NA_INTEGER) xj[i] = NA_REAL;
	    else xj[i] = cj[v[i]-1];
    }
}

static void addfactor(double *x, int nrx, int ncx,
		      double *c, int nrc, int ncc, int *v)
{
    int i, j, k;
    double *ck, *xj, *yj;

    for (k = ncc - 1; k >= 0; k--) {
	for (j = 0; j < ncx; j++) {
	    xj = &x[j*nrx];
	    yj = &x[(k*ncx+j)*nrx];
	    ck = &c[k*nrc];
	    for (i = 0; i < nrx; i++)
	    if(v[i] == NA_INTEGER) yj[i] = NA_REAL;
	    else yj[i] = ck[v[i]-1] * xj[i];
	}
    }
}

static void firstvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
    double *cj, *xj;
    int i, j;

    for (j = 0; j < ncc; j++) {
	xj = &x[j*nrx];
	cj = &c[j*nrc];
	for (i = 0; i < nrx; i++)
	    xj[i] = cj[i];
    }
}

static void addvar(double *x, int nrx, int ncx, double *c, int nrc, int ncc)
{
    int i, j, k;
    double *ck, *xj, *yj;

    for (k = ncc - 1; k >= 0; k--) {
	for (j = 0; j < ncx; j++) {
	    xj = &x[j*nrx];
	    yj = &x[(k*ncx+j)*nrx];
	    ck = &c[k*nrc];
	    for (i = 0; i < nrx; i++)
		yj[i] = ck[i] * xj[i];
	}
    }
}

#define BUFSIZE 4096

static char *AppendString(char *buf, const char *str)
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

static SEXP ColumnNames(SEXP x)
{
    SEXP dn = getAttrib(x, R_DimNamesSymbol);
    if (dn == R_NilValue)
	return R_NilValue;
    else
	return VECTOR_ELT(dn, 1);
}

SEXP modelmatrix(SEXP args)
{
    SEXP rho, expr, factors, terms, vars, vnames, assign;
    SEXP xnames, tnames, rnames;
    SEXP count, contrast, contr1, contr2, nlevs, ordered, columns, x;
    SEXP variable, var_i;
    int fik, first, i, j, k, kk, ll, n, nc, nterms, nVar;
    int intrcept, jstart, jnext, risponse, indx, rhs_response;
    double dk, dnc;
    char buf[BUFSIZE]="\0";
    char *bufp;
    const char *addp;

    args = CDR(args);

    /* Get the "terms" structure and extract */
    /* the intercept and response attributes. */

    terms = CAR(args);

    intrcept = asLogical(getAttrib(terms, install("intercept")));
    if (intrcept == NA_INTEGER)
	intrcept = 0;

    risponse = asLogical(getAttrib(terms, install("response")));
    if (risponse == NA_INTEGER)
	risponse = 0;

    /* Get the factor pattern matrix.  We duplicate this because */
    /* we may want to alter it if we are in the no-intercept case. */
    /* Note: the values of "nVar" and "nterms" are the REAL number of */
    /* variables in the model data frame and the number of model terms. */

    nVar = nterms = 0;		/* -Wall */
    PROTECT(factors = duplicate(getAttrib(terms, install("factors"))));
    if (length(factors) == 0) {
	/* if (intrcept == 0)
	   error("invalid model (zero parameters).");*/
	nVar = 1;
	nterms = 0;
    }
    else if (isInteger(factors) && isMatrix(factors)) {
	nVar = nrows(factors);
	nterms = ncols(factors);
    }
    else error(_("invalid '%s' argument"), "terms");

    /* Get the variable names from the factor matrix */

    vnames = getAttrib(factors, R_DimNamesSymbol);
    if (length(factors) > 0) {
	if (length(vnames) < 1 ||
	    (nVar - intrcept > 0 && !isString(VECTOR_ELT(vnames, 0))))
	    error(_("invalid '%s' argument"), "terms");
	vnames = VECTOR_ELT(vnames, 0);
    }

    /* Get the variables from the model frame.  First perform */
    /* elementary sanity checks.  Notes:  1) We need at least */
    /* one variable (lhs or rhs) to compute the number of cases. */
    /* 2) We don't type-check the response. */

    vars = CADR(args);
    if (!isNewList(vars) || length(vars) < nVar)
	error(_("invalid model frame"));
    if (length(vars) == 0)
	error(_("do not know how many cases"));
    rho = CADDR(args);

    n = nrows(VECTOR_ELT(vars, 0));
    /* This could be generated, so need to protect it */
    PROTECT(rnames = getAttrib(vars, R_RowNamesSymbol));

    /* This section of the code checks the types of the variables
       in the model frame.  Note that it should really only check
       the variables if they appear in a term in the model.
       Because it does not, we need to allow other types here, as they
       might well occur on the LHS.
       The R code converts all character variables in the model frame to
       factors, so the only types that ought to be here are logical,
       integer (including factor), numeric and complex.
     */

    PROTECT(variable = allocVector(VECSXP, nVar));
    PROTECT(nlevs = allocVector(INTSXP, nVar));
    PROTECT(ordered = allocVector(LGLSXP, nVar));
    PROTECT(columns = allocVector(INTSXP, nVar));

    for (i = 0; i < nVar; i++) {
	var_i = SET_VECTOR_ELT(variable, i, VECTOR_ELT(vars, i));
	if (nrows(var_i) != n)
	    error(_("variable lengths differ (found for variable %d)"), i);
	if (isOrdered_int(var_i)) {
	    LOGICAL(ordered)[i] = 1;
	    if((INTEGER(nlevs)[i] = nlevels(var_i)) < 1)
		error(_("variable %d has no levels"), i+1);
	    /* will get updated later when contrasts are set */
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isUnordered_int(var_i)) {
	    LOGICAL(ordered)[i] = 0;
	    if((INTEGER(nlevs)[i] = nlevels(var_i)) < 1)
		error(_("variable %d has no levels"), i+1);
	    /* will get updated later when contrasts are set */
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isLogical(var_i)) {
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 2;
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else if (isNumeric(var_i)) {
	    SET_VECTOR_ELT(variable, i, coerceVector(var_i, REALSXP));
	    var_i = VECTOR_ELT(variable, i);
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 0;
	    INTEGER(columns)[i] = ncols(var_i);
	}
	else {
	    LOGICAL(ordered)[i] = 0;
	    INTEGER(nlevs)[i] = 0;
	    INTEGER(columns)[i] = ncols(var_i);
	}
/*	else
	    error(_("invalid variable type for '%s'"),
	    CHAR(STRING_ELT(vnames, i))); */
    }

    /* If there is no intercept we look through the factor pattern */
    /* matrix and adjust the code for the first factor found so that */
    /* it will be coded by dummy variables rather than contrasts. */

    if (!intrcept) {
	for (j = 0; j < nterms; j++) {
	    for (i = risponse; i < nVar; i++) {
		if (INTEGER(nlevs)[i] > 1
		    && INTEGER(factors)[i + j * nVar] > 0) {
		    INTEGER(factors)[i + j * nVar] = 2;
		    goto alldone;
		}
	    }
	}
    }
 alldone:
    ;

    /* Compute the required contrast or dummy variable matrices. */
    /* We set up a symbolic expression to evaluate these, substituting */
    /* the required arguments at call time.  The calls have the following */
    /* form: (contrast.type nlevs contrasts) */

    PROTECT(contr1 = allocVector(VECSXP, nVar));
    PROTECT(contr2 = allocVector(VECSXP, nVar));

    PROTECT(expr = allocList(3));
    SET_TYPEOF(expr, LANGSXP);
    SETCAR(expr, install("contrasts"));
    SETCADDR(expr, allocVector(LGLSXP, 1));

    /* FIXME: We need to allow a third argument to this function */
    /* which allows us to specify contrasts directly.  That argument */
    /* would be used here in exactly the same way as the below. */
    /* I.e. we would search the list of constrast specs before */
    /* we try the evaluation below. */

    for (i = 0; i < nVar; i++) {
	if (INTEGER(nlevs)[i]) {
	    k = 0;
	    for (j = 0; j < nterms; j++) {
		if (INTEGER(factors)[i + j * nVar] == 1)
		    k |= 1;
		else if (INTEGER(factors)[i + j * nVar] == 2)
		    k |= 2;
	    }
	    SETCADR(expr, VECTOR_ELT(variable, i));
	    if (k & 1) {
		LOGICAL(CADDR(expr))[0] = 1;
		SET_VECTOR_ELT(contr1, i, eval(expr, rho));
	    }
	    if (k & 2) {
		LOGICAL(CADDR(expr))[0] = 0;
		SET_VECTOR_ELT(contr2, i, eval(expr, rho));
	    }
	}
    }

    /* By convention, an rhs term identical to the response generates nothing
       in the model matrix (but interactions involving the response do). */

    rhs_response = -1;
    if (risponse > 0) /* there is a response specified */
	for (j = 0; j < nterms; j++)
	    if (INTEGER(factors)[risponse - 1 + j * nVar]) {
		for (i = 0, k = 0; i < nVar; i++)
		    k += INTEGER(factors)[i + j * nVar] > 0;
		if (k == 1) {
		    rhs_response = j;
		    break;
		}
	    }


    /* We now have everything needed to build the design matrix. */
    /* The first step is to compute the matrix size and to allocate it. */
    /* Note that "count" holds a count of how many columns there are */
    /* for each term in the model and "nc" gives the total column count. */

    PROTECT(count = allocVector(INTSXP, nterms));
    if (intrcept)
	dnc = 1;
    else
	dnc = 0;
    for (j = 0; j < nterms; j++) {
	if (j == rhs_response) {
	    warning(_("the response appeared on the right-hand side and was dropped"));
	    INTEGER(count)[j] = 0;  /* need this initialised */
	    continue;
	}
	dk = 1;	/* accumulate in a double to detect overflow */
	for (i = 0; i < nVar; i++) {
	    if (INTEGER(factors)[i + j * nVar]) {
		if (INTEGER(nlevs)[i]) {
		    switch(INTEGER(factors)[i + j * nVar]) {
		    case 1:
			dk *= ncols(VECTOR_ELT(contr1, i));
			break;
		    case 2:
			dk *= ncols(VECTOR_ELT(contr2, i));
			break;
		    }
		}
		else dk *= INTEGER(columns)[i];
	    }
	}
	if (dk > INT_MAX) error(_("term %d would require %.0g columns"), j+1, dk);
	INTEGER(count)[j] = (int) dk;
	dnc = dnc + dk;
    }
    if (dnc > INT_MAX) error(_("matrix would require %.0g columns"), dnc);
    nc = (int) dnc;

    /* Record which columns of the design matrix are associated */
    /* with which model terms. */

    PROTECT(assign = allocVector(INTSXP, nc));
    k = 0;
    if (intrcept) INTEGER(assign)[k++] = 0;
    for (j = 0; j < nterms; j++) {
	if(INTEGER(count)[j] <= 0)
	    warning(_("problem with term %d in model.matrix: no columns are assigned"),
		      j+1);
	for (i = 0; i < INTEGER(count)[j]; i++)
	    INTEGER(assign)[k++] = j+1;
    }


    /* Create column labels for the matrix columns. */

    PROTECT(xnames = allocVector(STRSXP, nc));


    /* Here we loop over the terms in the model and, within each */
    /* term, loop over the corresponding columns of the design */
    /* matrix, assembling the names. */

    /* FIXME : The body within these two loops should be embedded */
    /* in its own function. */

    k = 0;
    if (intrcept)
	SET_STRING_ELT(xnames, k++, mkChar("(Intercept)"));

    for (j = 0; j < nterms; j++) {
	if (j == rhs_response) continue;
	for (kk = 0; kk < INTEGER(count)[j]; kk++) {
	    first = 1;
	    indx = kk;
	    bufp = &buf[0];
	    for (i = 0; i < nVar; i++) {
		ll = INTEGER(factors)[i + j * nVar];
		if (ll) {
		    var_i = VECTOR_ELT(variable, i);
		    if (!first)
			bufp = AppendString(bufp, ":");
		    first = 0;
		    if (isFactor(var_i) || isLogical(var_i)) {
			if (ll == 1) {
			    x = ColumnNames(VECTOR_ELT(contr1, i));
			    ll = ncols(VECTOR_ELT(contr1, i));
			}
			else {
			    x = ColumnNames(VECTOR_ELT(contr2, i));
			    ll = ncols(VECTOR_ELT(contr2, i));
			}
			addp = translateChar(STRING_ELT(vnames, i));
			if(strlen(buf) + strlen(addp) < BUFSIZE)
			    bufp = AppendString(bufp, addp);
			else
			    warning(_("term names will be truncated"));
			if (x == R_NilValue) {
			    if(strlen(buf) + 10 < BUFSIZE)
				bufp = AppendInteger(bufp, indx % ll + 1);
			    else
				warning(_("term names will be truncated"));
			} else {
			    addp = translateChar(STRING_ELT(x, indx % ll));
			    if(strlen(buf) + strlen(addp) < BUFSIZE)
				bufp = AppendString(bufp, addp);
			    else
				warning(_("term names will be truncated"));
			}
		    } else if (isComplex(var_i)) {
			error(_("complex variables are not currently allowed in model matrices"));
		    } else if(isNumeric(var_i)) { /* numeric */
			x = ColumnNames(var_i);
			ll = ncols(var_i);
			addp = translateChar(STRING_ELT(vnames, i));
			if(strlen(buf) + strlen(addp) < BUFSIZE)
			    bufp = AppendString(bufp, addp);
			else
			    warning(_("term names will be truncated"));
			if (ll > 1) {
			    if (x == R_NilValue) {
				if(strlen(buf) + 10 < BUFSIZE)
				    bufp = AppendInteger(bufp, indx % ll + 1);
				else
				    warning(_("term names will be truncated"));
			    } else {
				addp = translateChar(STRING_ELT(x, indx % ll));
				if(strlen(buf) + strlen(addp) < BUFSIZE)
				    bufp = AppendString(bufp, addp);
				else
				    warning(_("term names will be truncated"));
			    }
			}
		    } else
			error(_("variables of type '%s' are not allowed in model matrices"),
			      type2char(TYPEOF(var_i)));
		    indx /= ll;
		}
	    }
	    SET_STRING_ELT(xnames, k++, mkChar(buf));
	}
    }

    /* Allocate and compute the design matrix. */

    PROTECT(x = allocMatrix(REALSXP, n, nc));

#ifdef R_MEMORY_PROFILING
    if (RTRACE(vars)){
       memtrace_report(vars, x);
       SET_RTRACE(x, 1);
    }
#endif

    /* a) Begin with a column of 1s for the intercept. */

    if ((jnext = jstart = intrcept) != 0) {
	for (i = 0; i < n; i++) {
	    REAL(x)[i] = 1.0;
	}
    }

    /* b) Now loop over the model terms */

    contrast = R_NilValue;	/* -Wall */
    for (k = 0; k < nterms; k++) {
	if (k == rhs_response) continue;
	for (i = 0; i < nVar; i++) {
	    if (INTEGER(columns)[i] == 0)
		continue;
	    var_i = VECTOR_ELT(variable, i);
#ifdef R_MEMORY_PROFILING
	    if (RTRACE(var_i)){
	       memtrace_report(var_i, x);
	       SET_RTRACE(x, 1);
	    }
#endif
	    fik = INTEGER(factors)[i + k * nVar];
	    if (fik) {
		switch(fik) {
		case 1:
		    contrast = coerceVector(VECTOR_ELT(contr1, i), REALSXP);
		    break;
		case 2:
		    contrast = coerceVector(VECTOR_ELT(contr2, i), REALSXP);
		    break;
		}
		PROTECT(contrast);
		if (jnext == jstart) {
		    if (INTEGER(nlevs)[i] > 0) {
			int adj = isLogical(var_i)?1:0;
			firstfactor(&REAL(x)[jstart * n], n, jnext - jstart,
				    REAL(contrast), nrows(contrast),
				    ncols(contrast), INTEGER(var_i)+adj);
			jnext = jnext + ncols(contrast);
		    }
		    else {
			firstvar(&REAL(x)[jstart * n], n, jnext - jstart,
				 REAL(var_i), n, ncols(var_i));
			jnext = jnext + ncols(var_i);
		    }
		}
		else {
		    if (INTEGER(nlevs)[i] > 0) {
			int adj = isLogical(var_i)?1:0;
			addfactor(&REAL(x)[jstart * n], n, jnext - jstart,
				  REAL(contrast), nrows(contrast),
				  ncols(contrast), INTEGER(var_i)+adj);
			jnext = jnext + (jnext - jstart)*(ncols(contrast) - 1);
		    }
		    else {
			addvar(&REAL(x)[jstart * n], n, jnext - jstart,
			       REAL(var_i), n, ncols(var_i));
			jnext = jnext + (jnext - jstart) * (ncols(var_i) - 1);
		    }
		}
		UNPROTECT(1);
	    }
	}
	jstart = jnext;
    }
    PROTECT(tnames = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(tnames, 0, rnames);
    SET_VECTOR_ELT(tnames, 1, xnames);
    setAttrib(x, R_DimNamesSymbol, tnames);
    setAttrib(x, install("assign"), assign);
    UNPROTECT(14);
    return x;
}

/* Update a model formula by the replacement of "." templates. */

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

SEXP updateform(SEXP old, SEXP new)
{
    SEXP _new;

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

    /* We must duplicate here because the */
    /* formulae may be part of the parse tree */
    /* and we don't want to modify it. */

    PROTECT(_new = duplicate(new));

    /* Check of new and old formulae. */
    if (TYPEOF(old) != LANGSXP ||
       (TYPEOF(_new) != LANGSXP && CAR(old) != tildeSymbol) ||
       CAR(_new) != tildeSymbol)
	error(_("formula expected"));

    if (length(old) == 3) {
	SEXP lhs = CADR(old);
	SEXP rhs = CADDR(old);
	/* We now check that new formula has a valid lhs.
	   If it doesn't, we add one and set it to the rhs of the old
	   formula. */
	if (length(_new) == 2)
	    SETCDR(_new, CONS(lhs, CDR(_new)));
	/* Now we check the left and right sides of the new formula
	   and substitute the correct value for any "." templates.
	   We must parenthesize the rhs or we might upset arity and
	   precedence. */
	PROTECT(rhs);
	SETCADR(_new, ExpandDots(CADR(_new), lhs));
	SETCADDR(_new, ExpandDots(CADDR(_new), rhs));
	UNPROTECT(1);
    }
    else {
	/* The old formula had no lhs, so we only expand the rhs of the
	   new formula. */
	SEXP rhs = CADR(old);
	if (length(_new) == 3)
	    SETCADDR(_new, ExpandDots(CADDR(_new), rhs));
	else
	    SETCADR(_new, ExpandDots(CADR(_new), rhs));
    }

    /* It might be overkill to zero the */
    /* the attribute list of the returned */
    /* value, but it can't hurt. */

    SET_ATTRIB(_new, R_NilValue);
    SET_OBJECT(_new, 0);
    SEXP DotEnvSymbol = install(".Environment");
    setAttrib(_new, DotEnvSymbol, getAttrib(old, DotEnvSymbol));

    UNPROTECT(1);
    return _new;
}
