/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-7   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> char here is either ASCII or handled as a whole.
   Tests for ':' are OK.
 */


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Rconnections.h>
#include "Rregex.h"

static SEXP allocMatrixNA(SEXPTYPE, int, int);
static void transferVector(SEXP s, SEXP t);

SEXP attribute_hidden do_readDCF(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int nwhat, nret, nc, nr, m, k, lastm, need;
    Rboolean blank_skip, field_skip = FALSE;
    int whatlen, dynwhat, buflen = 100;
    char line[MAXELTSIZE], *buf;
    regex_t blankline, contline, trailblank, regline;
    regmatch_t regmatch[1];
    SEXP file, what, what2, retval, retval2, dims, dimnames;
    Rconnection con = NULL;
    Rboolean wasopen;

    checkArity(op, args);

    file = CAR(args);
    con = getConnection(asInteger(file));
    if(!con->canread)
        error(_("cannot read from this connection"));
    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error(_("cannot open the connection"));

    PROTECT(what = coerceVector(CADR(args), STRSXP)); /* argument fields */
    nwhat = LENGTH(what);
    dynwhat = (nwhat == 0);

    buf = (char *) malloc(buflen);
    if(!buf) error(_("could not allocate memory for 'read.dcf'"));
    nret = 20;
    /* it is easier if we first have a record per column */
    PROTECT (retval = allocMatrixNA(STRSXP, LENGTH(what), nret));

    regcomp(&blankline, "^[[:blank:]]*$", REG_NOSUB & REG_EXTENDED);
    regcomp(&trailblank, "[[:blank:]]+$", REG_EXTENDED);
    regcomp(&contline, "^[[:blank:]]+", REG_EXTENDED);
    regcomp(&regline, "^[^:]+:[[:blank:]]*", REG_EXTENDED);

    k = 0;
    lastm = -1; /* index of the field currently being recorded */
    blank_skip = TRUE;
    while(Rconn_getline(con, line, MAXELTSIZE) >= 0) {
	if(strlen(line) == 0 || regexec(&blankline, line, 0, 0, 0) == 0) {
	    /* A blank line.  The first one after a record
	       ends a new record, subsequent ones are skipped */
	    if(!blank_skip) {
		k++;
		if(k > nret - 1){
		    nret *= 2;
		    PROTECT(retval2 = allocMatrixNA(STRSXP, LENGTH(what), nret));
		    transferVector(retval2, retval);
		    UNPROTECT_PTR(retval);
		    retval = retval2;
		}
	    }
	    blank_skip = TRUE;
	} else {
	    /* starting a new record */
	    blank_skip = FALSE;
	    /* remove trailing whitespace */
	    if(regexec(&trailblank, line, 1, regmatch, 0) == 0)
		line[regmatch[0].rm_so] = '\0';

	    /* A continuation line.  Are we currently recording?
	       Or are we skipping a field?  Or is this an error? */
	    if( (lastm >= 0 || field_skip) &&
		regexec(&contline, line, 1, regmatch, 0) == 0) {
		if(lastm >= 0) {
		    need = strlen(line+regmatch[0].rm_eo) +
			strlen(CHAR(STRING_ELT(retval, lastm + nwhat*k))) + 2;
		    if(buflen < need) {
			buf = (char *) realloc(buf, need);
			if(!buf)
			    error(_("could not allocate memory for 'read.dcf'"));
			buflen = need;
		    }
		    strcpy(buf,CHAR(STRING_ELT(retval, lastm + nwhat*k)));
		    strcat(buf, "\n");
		    strcat(buf, line+regmatch[0].rm_eo);
		    SET_STRING_ELT(retval, lastm + nwhat*k, mkChar(buf));
		}
	    } else {
		if(regexec(&regline, line, 1, regmatch, 0) == 0){
		    for(m = 0; m < nwhat; m++){
			whatlen = strlen(CHAR(STRING_ELT(what, m)));
			if(strlen(line) > whatlen &&
			   line[whatlen] == ':' &&
			   strncmp(CHAR(STRING_ELT(what, m)),
				   line, whatlen) == 0) {
			    SET_STRING_ELT(retval, m+nwhat*k,
					   mkChar(line + regmatch[0].rm_eo));
			    lastm = m;
			    field_skip = FALSE;
			    break;
			} else {
			    /* This is a field, but not one prespecified */
			    lastm = -1;
			    field_skip = TRUE;
			}
		    }
		    if(dynwhat && (lastm == -1)) {
			/* A previously unseen field and we are
			   recording all fields */
			field_skip = FALSE;
			PROTECT(what2 = allocVector(STRSXP, nwhat+1));
			PROTECT(retval2 = allocMatrixNA(STRSXP,
							nrows(retval)+1,
							ncols(retval)));
			if(nwhat > 0) {
			    copyVector(what2, what);
			    for(nr = 0; nr < nrows(retval); nr++){
				for(nc = 0; nc < ncols(retval); nc++){
				    SET_STRING_ELT(retval2, nr+nc*nrows(retval2),
						   STRING_ELT(retval,
							      nr+nc*nrows(retval)));
				}
			    }
			}
			UNPROTECT_PTR(retval);
			UNPROTECT_PTR(what);
			retval = retval2;
			what = what2;
			need = strlen(line+regmatch[0].rm_eo);
			if(buflen < need){
			    buf = (char *) realloc(buf, need);
			    if(!buf)
				error(_("could not allocate memory for 'read.dcf'"));
			    buflen = need;
			}
			strncpy(buf, line, Rf_strchr(line, ':') - line);
			buf[Rf_strchr(line, ':') - line] = '\0';
			SET_STRING_ELT(what, nwhat, mkChar(buf));
			nwhat++;
			/* lastm uses C indexing, hence nwhat - 1 */
			lastm = nwhat - 1;
			SET_STRING_ELT(retval, lastm + nwhat*k,
				       mkChar(line + regmatch[0].rm_eo));
		    }
		} else {
		    line[20] = '\0';
		    warning("Line starting '%s ...' is malformed!", line);
		}
	    }
	}
    }
    if(!wasopen) con->close(con);
    free(buf);
    regfree(&blankline);
    regfree(&contline);
    regfree(&trailblank);
    regfree(&regline);

    if(!blank_skip) k++;

    /* and now transpose the whole matrix */
    PROTECT(retval2 = allocMatrixNA(STRSXP, k, LENGTH(what)));
    copyMatrix(retval2, retval, 1);

    PROTECT(dimnames = allocVector(VECSXP, 2));
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = k;
    INTEGER(dims)[1] = LENGTH(what);
    SET_VECTOR_ELT(dimnames, 1, what);
    setAttrib(retval2, R_DimSymbol, dims);
    setAttrib(retval2, R_DimNamesSymbol, dimnames);
    UNPROTECT(5);
    return(retval2);
}


static SEXP allocMatrixNA(SEXPTYPE mode, int nrow, int ncol)
{
    int k;
    SEXP retval;

    PROTECT(retval = allocMatrix(mode, nrow, ncol));
    for(k = 0; k < LENGTH(retval); k++)
	SET_STRING_ELT(retval, k, NA_STRING);
    UNPROTECT(1);
    return(retval);
}

/* This one is needed because the normal copy operations will do
   recycling */

static void transferVector(SEXP s, SEXP t)
{
    int i, ns, nt;

    nt = LENGTH(t);
    ns = LENGTH(s);
    for (i = 0; i < nt; i++)
	SET_STRING_ELT(s, i, STRING_ELT(t, i));
}




