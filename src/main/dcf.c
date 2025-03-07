/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2025   The R Core Team.
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Rconnections.h>

#include <tre/tre.h>

static SEXP allocMatrixNA(SEXPTYPE, int, int);
static void transferVector(SEXP s, SEXP t);

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

static bool field_is_foldable_p(const char *, SEXP);

/* Use R_alloc as this might get interrupted */
static char *Rconn_getline2(Rconnection con, char *buf, int bufsize)
{
    int c, nbuf = 0;
    while((c = Rconn_fgetc(con)) != R_EOF) {
	if(nbuf+1 >= bufsize) { // allow for terminator below
	    bufsize *= 2;
	    char *buf2 = R_alloc(bufsize, sizeof(char));
	    memcpy(buf2, buf, nbuf);
	    buf = buf2;
	}
	if(c != '\n'){
	    buf[nbuf++] = (char) c;
	} else {
	    buf[nbuf++] = '\0';
	    break;
	}
    }
    if (!nbuf)
    	return NULL;
    /* Make sure it is null-terminated even if file did not end with
     *  newline.
     */
    if(buf[nbuf-1]) buf[nbuf] = '\0';
    return buf;
}

attribute_hidden SEXP do_readDCF(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int nwhat, nret, nc, nr, m, k, lastm, need, i, n_eblanklines = 0;
    bool blank_skip, field_skip = false;
    int whatlen, dynwhat, buflen = 8096; // was 100, but that re-alloced often
    char *line, *buf;
    regex_t blankline, contline, trailblank, regline, eblankline;
    regmatch_t regmatch[1];
    SEXP file, what, what2, retval, retval2, dims, dimnames;
    Rconnection con = NULL;
    bool wasopen, is_eblankline;
    RCNTXT cntxt;

    SEXP fold_excludes;
    bool field_fold = true, has_fold_excludes;
    const char *field_name;
    int offset = 0; /* -Wall */

    checkArity(op, args);

    file = CAR(args);
    con = getConnection(asInteger(file));
    wasopen = con->isopen;
    if(!wasopen) {
	if(!con->open(con)) error(_("cannot open the connection"));
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canread) error(_("cannot read from this connection"));

    args = CDR(args);
    PROTECT(what = coerceVector(CAR(args), STRSXP)); /* argument fields */
    nwhat = LENGTH(what);
    dynwhat = (nwhat == 0);

    args = CDR(args);
    PROTECT(fold_excludes = coerceVector(CAR(args), STRSXP));
    has_fold_excludes = (LENGTH(fold_excludes) > 0);

    buf = (char *) malloc(buflen);
    if(!buf) error(_("could not allocate memory for 'read.dcf'"));
    nret = 20;
    /* it is easier if we first have a record per column */
    PROTECT(retval = allocMatrixNA(STRSXP, LENGTH(what), nret));

    /* These used to use [:blank:] and [:space:] but those are locale-dependent
       and :blank: can match \xa0 as part of a UTF-8 character
       (and is nbspace on Windows). */
    tre_regcompb(&blankline, "^[ \t]*$", REG_NOSUB & REG_EXTENDED);
    tre_regcompb(&trailblank, "[ \t]+$", REG_EXTENDED);
    tre_regcompb(&contline, "^[ \t]+", REG_EXTENDED);
    tre_regcompb(&regline, "^[^:]+:[ \t]*", REG_EXTENDED);
    tre_regcompb(&eblankline, "^[ \f\n\r\t\v]+\\.[ \f\n\r\t\v]*$", REG_EXTENDED);

    k = 0;
    lastm = -1; /* index of the field currently being recorded */
    blank_skip = true;
    void *vmax = vmaxget();
    char buf0[MAXELTSIZE];
    while((line = Rconn_getline2(con, buf0, MAXELTSIZE))) {
	if(strlen(line) == 0 ||
	   tre_regexecb(&blankline, line, 0, 0, 0) == 0) {
	    /* A blank line.  The first one after a record ends a new
	     * record, subsequent ones are skipped */
	    if(!blank_skip) {
		k++;
		if(k > nret - 1){
		    nret *= 2;
		    PROTECT(retval2 = allocMatrixNA(STRSXP, LENGTH(what), nret));
		    transferVector(retval2, retval);
		    retval = retval2;
		    UNPROTECT(2); /* retval, retval2 */
		    PROTECT(retval);
		}
		blank_skip = true;
		lastm = -1;
		field_skip = false;
		field_fold = true;
		n_eblanklines = 0;
	    }
	} else {
	    blank_skip = false;
	    if(tre_regexecb(&contline, line, 1, regmatch, 0) == 0) {
		/* A continuation line: wrong if at the beginning of a
		   record. */
		if((lastm == -1) && !field_skip) {
		    line[20] = '\0';
		    error(_("Found continuation line starting '%s ...' at begin of record."),
			  line);
		}
		if(lastm >= 0) {
		    need = (int) strlen(CHAR(STRING_ELT(retval,
							lastm + nwhat * k))) + 2;
		    if(tre_regexecb(&eblankline, line, 0, NULL, 0) == 0) {
			is_eblankline = true;
			if(field_fold) {
			    n_eblanklines++;
			    continue;
			}
		    } else {
			is_eblankline = false;
			if(field_fold) {
			    offset = regmatch[0].rm_eo;
			    /* Also remove trailing whitespace. */
			    if((tre_regexecb(&trailblank, line, 1,
					     regmatch, 0) == 0))
				line[regmatch[0].rm_so] = '\0';
			} else {
			    offset = 0;
			}
			need += (int) strlen(line + offset) + n_eblanklines;
		    }
		    if(buflen < need) {
			char *tmp = (char *) realloc(buf, need);
			if(!tmp) {
			    free(buf);
			    error(_("could not allocate memory for 'read.dcf'"));
			} else buf = tmp;
			buflen = need;
		    }
		    strcpy(buf, CHAR(STRING_ELT(retval, lastm + nwhat * k)));
		    if(strlen(buf) || !field_fold)
			strcat(buf, "\n");
		    if(!is_eblankline) {
			if(n_eblanklines > 0) {
			    for(i = 0; i < n_eblanklines; i++) {
				strcat(buf, "\n");
			    }
			    n_eblanklines = 0;
			}
			strcat(buf, line + offset);
		    }
		    SET_STRING_ELT(retval, lastm + nwhat * k, mkChar(buf));
		}
	    } else {
		if(tre_regexecb(&regline, line, 1, regmatch, 0) == 0) {
		    for(m = 0; m < nwhat; m++){
			whatlen = (int) strlen(CHAR(STRING_ELT(what, m)));
			if(strlen(line) > whatlen &&
			   line[whatlen] == ':' &&
			   strncmp(CHAR(STRING_ELT(what, m)),
				   line, whatlen) == 0) {
			    /* An already known field we are recording. */
			    lastm = m;
			    field_skip = false;
			    field_name = CHAR(STRING_ELT(what, lastm));
			    if(has_fold_excludes) {
				field_fold =
				    field_is_foldable_p(field_name,
							fold_excludes);
			    }
			    offset = regmatch[0].rm_eo;
			    if(field_fold) {
				/* Also remove trailing whitespace. */
				if((tre_regexecb(&trailblank, line, 1,
						 regmatch, 0) == 0))
				    line[regmatch[0].rm_so] = '\0';
			    }
			    SET_STRING_ELT(retval, m + nwhat * k,
					   mkChar(line + offset));
			    break;
			} else {
			    /* This is a field, but not one prespecified */
			    lastm = -1;
			    field_skip = true;
			}
		    }
		    if(dynwhat && (lastm == -1)) {
			/* A previously unseen field and we are
			 * recording all fields */
			field_skip = false;
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
			retval = retval2;
			what = what2;
			UNPROTECT(5); /* what, fold_excludes, retval, what2, retval2 */
			PROTECT(what);
			PROTECT(fold_excludes);
			PROTECT(retval);
			/* Make sure enough space was used */
			need = (int) (Rf_strchr(line, ':') - line + 1);
			if(buflen < need){
			    char *tmp = (char *) realloc(buf, need);
			    if(!tmp) {
				free(buf);
				error(_("could not allocate memory for 'read.dcf'"));
			    } else buf = tmp;
			    buflen = need;
			}
			strncpy(buf, line, Rf_strchr(line, ':') - line);
			buf[Rf_strchr(line, ':') - line] = '\0';
			SET_STRING_ELT(what, nwhat, mkChar(buf));
			nwhat++;
			/* lastm uses C indexing, hence nwhat - 1 */
			lastm = nwhat - 1;
			field_name = CHAR(STRING_ELT(what, lastm));
			if(has_fold_excludes) {
			    field_fold =
				field_is_foldable_p(field_name,
						    fold_excludes);
			}
			offset = regmatch[0].rm_eo;
			if(field_fold) {
			    /* Also remove trailing whitespace. */
			    if((tre_regexecb(&trailblank, line, 1,
					     regmatch, 0) == 0))
				line[regmatch[0].rm_so] = '\0';
			}
			SET_STRING_ELT(retval, lastm + nwhat * k,
				       mkChar(line + offset));
		    }
		} else {
		    /* Must be a regular line with no tag ... */
		    line[20] = '\0';
		    error(_("Line starting '%s ...' is malformed!"), line);
		}
	    }
	}
    }
    vmaxset(vmax);
    if(!wasopen) {endcontext(&cntxt); con->close(con);}
    free(buf);
    tre_regfree(&blankline);
    tre_regfree(&contline);
    tre_regfree(&trailblank);
    tre_regfree(&regline);
    tre_regfree(&eblankline);

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
    UNPROTECT(6); /* what, fold_excludes, retval, retval2, dimnames, dims */
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
    for (int i = 0; i < LENGTH(t); i++)
	SET_STRING_ELT(s, i, STRING_ELT(t, i));
}

static bool field_is_foldable_p(const char *field, SEXP excludes)
{
    int i, n = LENGTH(excludes);
    for(i = 0; i < n; i++) {
	if(strcmp(field, CHAR(STRING_ELT(excludes, i))) == 0)
	    return false;
    }
    return true;
}
