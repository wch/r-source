/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2006  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8>
   char here is mainly either ASCII or handled as a whole.
   isBlankString has been be improved.
*/



#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define COMPILING_R 1 /* for Rinlinedfuns.h included via Defn.h */
#include <Defn.h>
#include <Rmath.h>
#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* Many small functions are included from Rinlinedfuns.h */

Rboolean tsConform(SEXP x, SEXP y)
{
    if ((x = getAttrib(x, R_TspSymbol)) != R_NilValue &&
	(y = getAttrib(y, R_TspSymbol)) != R_NilValue)
	return INTEGER(x)[0] == INTEGER(x)[0] &&
	    INTEGER(x)[1] == INTEGER(x)[1] &&
	    INTEGER(x)[2] == INTEGER(x)[2];
    return FALSE;
}

const static char * const truenames[] = {
    "T",
    "True",
    "TRUE",
    "true",
    (char *) 0,
};

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
    else error(_("object is not a matrix"));
    return -1;
}


int ncols(SEXP s)
{
    SEXP t;
    if (isVector(s) || isList(s)) {
	t = getAttrib(s, R_DimSymbol);
	if (t == R_NilValue) return 1;
	if (LENGTH(t) >= 2) return INTEGER(t)[1];
	/* This is a 1D (or possibly 0D array) */
	return 1;
    }
    else if (isFrame(s)) {
	return length(s);
    }
    else error(_("object is not a matrix"));
    return -1;/*NOTREACHED*/
}

const static char type_msg[] = "invalid type passed to internal function\n";


void internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    errorcall(call, type_msg);
	else
	    error(type_msg);
    }
}

const static char * const falsenames[] = {
    "F",
    "False",
    "FALSE",
    "false",
    (char *) 0,
};

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
	    formatReal(REAL(x), 1, &w, &d, &e, 0);
	    return mkChar(EncodeReal(REAL(x)[0], w, d, e, OutDec));
        case CPLXSXP:
	    formatComplex(COMPLEX(x), 1, &w, &d, &e, &wi, &di, &ei, 0);
	    return mkChar(EncodeComplex(COMPLEX(x)[0], w, d, e, wi, di, ei, OutDec));
	case STRSXP:
	    return STRING_ELT(x, 0);
	default:
	    return NA_STRING;
	}
    }
    return NA_STRING;
}


R_len_t asVecSize(SEXP x)
{
    int warn = 0, res;
    double d;

    if (isVectorAtomic(x) && LENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    res = IntegerFromLogical(LOGICAL(x)[0], &warn);
	    if(res == NA_INTEGER) error(_("vector size cannot be NA"));
	    return res;
	case INTSXP:
	    res = INTEGER(x)[0];
	    if(res == NA_INTEGER) error(_("vector size cannot be NA"));
	    return res;
	case REALSXP:
	    d = REAL(x)[0];
	    if(d < 0) error(_("vector size cannot be negative"));
	    if(d > R_LEN_T_MAX) error(_("vector size specified is too large"));
	    return (R_size_t) d;
	default:
	    UNIMPLEMENTED_TYPE("asVecSize", x);
	}
    }
    return -1;
}


const static struct {
    const char * const str;
    const int type;
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
#ifdef BYTECODE
    { "bytecode",	BCODESXP   },
#endif
    { "weakref",	WEAKREFSXP },
    { "raw",		RAWSXP },
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
    error(_("type %d is unimplemented in type2str"), t);
    return R_NilValue; /* for -Wall */
}

char *type2char(SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return TypeTable[i].str;
    }
    error(_("type %d is unimplemented in type2str"), t);
    return ""; /* for -Wall */
}

SEXP type2symbol(SEXPTYPE t)
{
    int i;
    /* for efficiency, a hash table set up to index TypeTable, and
       with TypeTable pointing to both the
       character string and to the symbol would be better */
    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return install((char *)&TypeTable[i].str);
    }
    error(_("type %d is unimplemented in type2symbol"), t);
    return R_NilValue; /* for -Wall */
}

void UNIMPLEMENTED_TYPEt(char *s, SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    error(_("unimplemented type '%s' in '%s'\n"), TypeTable[i].str, s);
    }
    error(_("unimplemented type (%d) in '%s'\n"), t, s);
}

void UNIMPLEMENTED_TYPE(char *s, SEXP x)
{
    UNIMPLEMENTED_TYPEt(s, TYPEOF(x));
}

#if defined(SUPPORT_MBCS)
# include <R_ext/Riconv.h>
# include <sys/param.h>
# include <errno.h>


/* Previous versions of R (< 2.3.0) assumed wchar_t was in Unicode
   (and it commonly is).  These functions do not. */
# ifdef WORDS_BIGENDIAN
static const char UCS2ENC[] = "UCS-2BE";
# else
static const char UCS2ENC[] = "UCS-2LE";
# endif

#if 0
/* <FIXME>
 * It would make a lot of sense to cache cd here, but it would need to be 
 * refreshed if the locale was changed.  However, this seems the
 * wrong way to do this, as mbrlen will do the job correctly.
 */
size_t mbcsMblen(char *in)
{
    unsigned int ucs4buf[1];
    unsigned short ucs2buf[1];
    void *cd = NULL, *buftype;
    char *i_buf, *o_buf;
    size_t i_len, o_len, status;
    int i;

    /* 6 == MB_LEN_MAX ? shift state is ignored... */
    for (i = 1 ; i <= 6 ; i++) {
	buftype = (void *) ucs4buf;
	if((void*)-1 == (cd = Riconv_open((char*)UCS4ENC, ""))) {
	    buftype = (void *)ucs2buf;
	    if ((void*)-1 == (cd = Riconv_open((char*)UCS2ENC, ""))) {
		return (size_t)(-1);
	    }
	}

	i_buf = in;
	i_len = i;
	o_buf = buftype == (void *) ucs4buf ?
	    (char *) ucs4buf : (char *) ucs2buf;
	o_len = buftype == (void *) ucs4buf ? 4 : 2;
	memset (o_buf, 0 , o_len);
	status = Riconv(cd, (char **)&i_buf, (size_t *)&i_len,
			(char **)&o_buf, (size_t *)&o_len);
	Riconv_close(cd);
	if (status == (size_t) -1) {
	    switch (errno){
	    case EINVAL:
		/* next char */
		break;
	    case E2BIG:
		return (size_t) -1;
	    case EILSEQ:
		return (size_t) -1;
	    }
	} else if ((size_t) 0 == status)
	    /* normal status */
	    return (size_t) i;
	else
	    return (size_t) status;
    }
    return (size_t) -1;
}

/* Currently only used in this file */
size_t ucs2Mblen(ucs2_t *in)
{
    char mbbuf[16];
    void *cd = NULL;
    char *i_buf, *o_buf;
    size_t i_len, o_len, status;

    if ((void*) -1 == (cd = Riconv_open("", (char *)UCS2ENC)))
	return (size_t) -1;

    memset(mbbuf, 0, sizeof(mbbuf));
    i_buf = (char *)in;
    i_len = sizeof(ucs2_t);
    o_buf = mbbuf;
    o_len = sizeof(mbbuf);
    memset(o_buf, 0 , o_len);
    status = Riconv(cd, (char **)&i_buf, (size_t *)&i_len,
		    (char **)&o_buf, (size_t *)&o_len);
    Riconv_close(cd);
    if ((size_t) -1 == status)
	switch (errno) {
	case EINVAL:
	    /* not case */
	    return (size_t) -1;
	case E2BIG:
	    /* probably few case */
	    return (size_t) -1;
	case EILSEQ:
	    return (size_t) -1;
	}
    return (size_t) strlen(mbbuf);
}
#endif

/*
 * out=NULL returns the number of the MBCS chars
 */
/* Note: this does not terminate out, as all current uses are to look
 * at 'out' a wchar at a time, and sometimes just one char.
 */
size_t mbcsToUcs2(char *in, ucs2_t *out, int nout)
{
    void   *cd = NULL ;
    char   *i_buf, *o_buf;
    size_t  i_len, o_len, status, wc_len;

    /* out length */
    /* i_buf = in;
    wc_len = 0;
    while(*i_buf){
	int rc;
	rc = (int) mbcsMblen(i_buf);
	if (rc < 0) return rc;
	i_buf += rc;
	wc_len++;
	} */
    wc_len = mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open((char *)UCS2ENC, "")))
	return (size_t) -1;

    i_buf = in;
    i_len = strlen(in); /* not including terminator */
    o_buf = (char *)out;
    o_len = nout * sizeof(ucs2_t);
    status = Riconv(cd, (char **)&i_buf, (size_t *)&i_len,
		    (char **)&o_buf, (size_t *)&o_len);

    Riconv_close(cd);
    if (status == (size_t)-1) {
        switch(errno){
        case EINVAL:
            return (size_t) -2;
        case EILSEQ:
            return (size_t) -1;
        case E2BIG:
            break;
        default:
	    errno = EILSEQ;
	    return (size_t) -1;
        }
    }
    return wc_len; /* status would be better? */
}

#if 0
/*
 * out returns the number of the bytes in the case of NULL
 */
/* Also does not terminate out, and currently unused */
size_t ucs2ToMbcs(ucs2_t *in, char *out)
{
    void   *cd = NULL ;
    ucs2_t *ucs = in ;
    char   *i_buf = (char *)in, *o_buf;
    size_t  i_len, o_len, status;

    /* out length */
    o_len = 0;
    i_len = 0;
    while(*ucs) {
	int rc;
	rc = ucs2Mblen(ucs);
	if(rc < 0) return rc;
	o_len += rc;
	i_len += sizeof(ucs2_t);
    }
    if ( out == NULL ) return o_len;

    if ((void*)-1 == (cd = Riconv_open("", (char *)UCS2ENC)))
	return((size_t)(-1));

    o_buf = (char *)out;
    status = Riconv(cd, (char **)&i_buf, (size_t *)&i_len,
		    (char **)&o_buf, (size_t *)&o_len);

    Riconv_close(cd);
    if (status == (size_t)-1){
        switch(errno){
        case EINVAL:
            return (size_t) -2;
        case EILSEQ:
            return (size_t) -1;
        case E2BIG:
            break;
        default:
	    errno=EILSEQ;
	    return (size_t) -1;
        }
    }
    return strlen(out);
}
#endif
#endif /* SUPPORT_MBCS */


#ifdef SUPPORT_MBCS
#include <wctype.h>
#endif

/* This one is not in Rinternals.h, but is used in internet module */
Rboolean isBlankString(char *s)
{
#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	wchar_t wc; int used; mbstate_t mb_st;
	mbs_init(&mb_st);
	while( (used = Mbrtowc(&wc, s, MB_CUR_MAX, &mb_st)) ) {
	    if(!iswspace(wc)) return FALSE;
	    s += used;
	}
    } else
#endif
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
	error(_("invalid tag in name extraction"));
    }
    return s;
}


void checkArity(SEXP op, SEXP args)
{
    if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args))
	error(P_("%d argument passed to '%s' which requires %d",
		 "%d arguments passed to '%s' which requires %d",
		 length(args)),
	      length(args), PRIMNAME(op), PRIMARITY(op));
}


SEXP nthcdr(SEXP s, int n)
{
    if (isList(s) || isLanguage(s) || isFrame(s) || TYPEOF(s) == DOTSXP ) {
	while( n-- > 0 ) {
	    if (s == R_NilValue)
		error(_("\"nthcdr\" list shorter than %d"), n);
	    s = CDR(s);
	}
	return s;
    }
    else error(_("\"nthcdr\" needs a list to CDR down"));
    return R_NilValue;/* for -Wall */
}


SEXP attribute_hidden do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
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


void attribute_hidden setIVector(int * vec, int len, int val)
{
    int i;
    for (i = 0; i < len; i++)
	vec[i] = val;
}


void attribute_hidden setRVector(double * vec, int len, double val)
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

/* merge(xinds, yinds, all.x, all.y) */
SEXP attribute_hidden do_merge(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP xi, yi, ansx, ansy, ans, ansnames, x_lone, y_lone;
    int y, nx = 0, ny = 0, i, j, k, nans = 0, nx_lone = 0, ny_lone = 0;
    int all_x = 0, all_y = 0, ll = 0/* "= 0" : for -Wall */;

    checkArity(op, args);
    xi = CAR(args);
    if ( !isInteger(xi) || !(nx = LENGTH(xi)) )
	error(_("invalid '%s' argument"), "xinds");
    yi = CADR(args);
    if ( !isInteger(yi) || !(ny = LENGTH(yi)) )
	error(_("invalid '%s' argument"), "yinds");
    if(!LENGTH(ans = CADDR(args)) || NA_LOGICAL == (all_x = asLogical(ans)))
	errorcall(call, _("'all.x' must be TRUE or FALSE"));
    if(!LENGTH(ans = CADDDR(args))|| NA_LOGICAL == (all_y = asLogical(ans)))
	errorcall(call, _("'all.y' must be TRUE or FALSE"));
    /* 1. determine result sizes */
    if(all_x) {
	for (i = 0; i < nx; i++)
	    if (INTEGER(xi)[i] == 0) nx_lone++;
    }
    for (j = 0; j < ny; j++)
	if ((y = INTEGER(yi)[j]) > 0) {
	    for (i = 0; i < nx; i++) {
		if (INTEGER(xi)[i] == y) nans++;
	    }
        } else /* y == 0 */ if (all_y) ny_lone++;
    /* 2. allocate and store result components */
    PROTECT(ans = allocVector(VECSXP, 4));
    ansx = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);
    if(all_x) {
	x_lone = allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	ll = 0;
	for (i = 0; i < nx; i++)
	    if (INTEGER(xi)[i] == 0) INTEGER(x_lone)[ll++] = i + 1;
    }
    if(all_y) {
	y_lone = allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	ll = 0;
    } else
	y_lone = R_NilValue;
    for (j = 0, k = 0; j < ny; j++)
	if ((y = INTEGER(yi)[j]) > 0) {
	    for (i = 0; i < nx; i++)
		if (INTEGER(xi)[i] == y) {
		INTEGER(ansx)[k]   = i + 1;
		INTEGER(ansy)[k++] = j + 1;
	    }
	} else /* y == 0 */ if (all_y) INTEGER(y_lone)[ll++] = j + 1;

    PROTECT(ansnames = allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, mkChar("xi"));
    SET_STRING_ELT(ansnames, 1, mkChar("yi"));
    SET_STRING_ELT(ansnames, 2, mkChar("x.alone"));
    SET_STRING_ELT(ansnames, 3, mkChar("y.alone"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}


/* Functions for getting and setting the working directory. */
#ifdef Win32
#include <windows.h>
#endif

SEXP attribute_hidden do_getwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval = R_NilValue;
    char buf[2 * PATH_MAX];

    checkArity(op, args);

#ifdef R_GETCWD
    R_GETCWD(buf, PATH_MAX);
#ifdef Win32
    R_fixslash(buf);
#endif
    rval = mkString(buf);
#endif
    return(rval);
}

#if defined(Win32) && defined(_MSC_VER)
#include <direct.h> /* for chdir */
#endif

SEXP attribute_hidden do_setwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue;	/* -Wall */
    const char *path;

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	errorcall(call, _("character argument expected"));
    path = R_ExpandFileName(CHAR(STRING_ELT(s, 0)));
#ifdef HAVE_CHDIR
    if(chdir(path) < 0)
#endif
	errorcall(call, _("cannot change working directory"));
    return(R_NilValue);
}

/* remove portion of path before file separator if one exists */

SEXP attribute_hidden do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	errorcall(call, _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	p = R_ExpandFileName(CHAR(STRING_ELT(s, i)));
	if (strlen(p) > PATH_MAX - 1)
	    errorcall(call, _("path too long"));
	strcpy (buf, p);
#ifdef Win32
	R_fixslash(buf);
#endif
	/* remove trailing file separator(s) */
	while ( *(p = buf + strlen(buf) - 1) == fsp ) *p = '\0';
	if ((p = Rf_strrchr(buf, fsp)))
	    p++;
	else
	    p = buf;
	SET_STRING_ELT(ans, i, mkChar(p));
    }
    UNPROTECT(1);
    return(ans);
}

/* remove portion of path after last file separator if one exists, else
   return "."
   */

SEXP attribute_hidden do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	errorcall(call, _("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	p = R_ExpandFileName(CHAR(STRING_ELT(s, i)));
	if (strlen(p) > PATH_MAX - 1)
	    errorcall(call, _("path too long"));
	strcpy (buf, p);
#ifdef Win32
	R_fixslash(buf);
#endif
	/* remove trailing file separator(s) */
	while ( *(p = buf + strlen(buf) - 1) == fsp  && p > buf
#ifdef Win32
		&& (p > buf+2 || *(p-1) != ':')
#endif
	    ) *p = '\0';
	p = Rf_strrchr(buf, fsp);
	if(p == NULL)
	    strcpy(buf, ".");
	else {
	    while(p > buf && *p == fsp
#ifdef Win32
		  && (p > buf+2 || *(p-1) != ':')
#endif
		) --p;
	    p[1] = '\0';
	}
	SET_STRING_ELT(ans, i, mkChar(buf));
    }
    UNPROTECT(1);
    return(ans);
}


/* encodeString(x, w, quote, justify) */
SEXP attribute_hidden do_encodeString(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, s;
    int i, len, w, quote = 0, justify, na;
    char *cs;
    Rboolean findWidth;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	errorcall(call, _("a character vector argument expected"));
    if(isNull(CADR(args))) w = NA_INTEGER;
    else {
	w = asInteger(CADR(args));
	if(w != NA_INTEGER && w < 0)
	    errorcall(call, _("invalid '%s' value"), "width");
    }
    findWidth = (w == NA_INTEGER);
    s = CADDR(args);
    if(LENGTH(s) != 1 || TYPEOF(s) != STRSXP)
	errorcall(call, _("invalid '%s' value"), "quote");
    cs = CHAR(STRING_ELT(s, 0));
    if(strlen(cs) > 0) quote = cs[0];
    if(strlen(cs) > 1)
	warningcall(call,
		    _("only the first character of 'quote' will be used"));
    justify = asInteger(CADDDR(args));
    if(justify == NA_INTEGER || justify < 0 || justify > 3)
	errorcall(call, _("invalid '%s' value"), "justify");
    if(justify == 3) w = 0;
    na = asLogical(CAD4R(args));
    if(na == NA_LOGICAL) errorcall(call, _("invalid '%s' value"), "na.encode");

    len = LENGTH(x);
    if(findWidth && justify < 3) {
	w  = 0;
	for(i = 0; i < len; i++) {
	    s = STRING_ELT(x, i);
	    if(na || s != NA_STRING)
		w = imax2(w, Rstrlen(s, quote));
	}
	if(quote) w +=2; /* for surrounding quotes */
    }
    PROTECT(ans = duplicate(x));
    for(i = 0; i < len; i++) {
	s = STRING_ELT(x, i);
	if(na || s != NA_STRING)
	    SET_STRING_ELT(ans, i, mkChar(EncodeString(s, w, quote, justify)));
    }
    UNPROTECT(1);
    return ans;
}

/* Note: this is designed to be fast and valid only for UTF-8 strings.
   It is also correct in EUC-* locales. */
Rboolean utf8strIsASCII(char *str)
{
    char *p;
    for(p = str; *p; p++)
	if((unsigned int)*p > 0x7F) return FALSE;
    return TRUE;
}

#ifdef SUPPORT_MBCS
/* Number of additional bytes */
static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

int utf8clen(char c)
{
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}

/* A version that reports failure as an error */
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return (size_t)0;
    used = mbrtowc(wc, s, n, ps);
    if((int)used < 0) error(_("invalid multibyte string"));
    return used;
}

Rboolean mbcsValid(char *str)
{
    return  ((int)mbstowcs(NULL, str, 0) >= 0);
}

/* We do this conversion ourselves to do our own error recovery */
void mbcsToLatin1(char *in, char *out)
{
    wchar_t *wbuff;
    int i;
    size_t res = mbstowcs(NULL, in, 0), mres;

    if(res == (size_t)(-1)) {
	warning(_("invalid input in mbcsToLatin1"));
	*out = '\0';
	return;
    }
    wbuff = (wchar_t *) alloca((res+1) * sizeof(wchar_t));
    R_CheckStack();
    if(!wbuff) error(_("allocation failure in 'mbcsToLatin1'"));
    mres = mbstowcs(wbuff, in, res+1);
    if(mres == (size_t)-1)
	error(_("invalid input in 'mbcsToLatin1'"));
    for(i = 0; i < res; i++) {
	/* here we do assume Unicode wchars */
	if(wbuff[i] > 0xFF) out[i] = '.';
	else out[i] = (char) wbuff[i];
    }
    out[res] = '\0';
}

/* MBCS-aware versions of common comparisons.  Only used for ASCII c */
char *Rf_strchr(const char *s, int c)
{
    char *p = (char *)s;
    mbstate_t mb_st;
    int used;

    if(!mbcslocale || utf8locale) return strchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) return p;
	p += used;
    }
    return (char *)NULL;
}

char *Rf_strrchr(const char *s, int c)
{
    char *p = (char *)s, *plast = NULL;
    mbstate_t mb_st;
    int used;

    if(!mbcslocale || utf8locale) return strrchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) plast = p;
	p += used;
    }
    return plast;
}
#else
/* Dummy entry points so R.dll always has them */
int utf8clen(char c) { return 1;}
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, void *ps)
{ return (size_t)(-1);}
Rboolean mbcsValid(char *str) { return TRUE; }
void mbcsToLatin1(char *in, char *out) {}
#undef Rf_strchr
char *Rf_strchr(const char *s, int c)
{
    return strchr(s, c);
}
#undef Rf_strrchr
char *Rf_strrchr(const char *s, int c)
{
    return strrchr(s, c);
}
#endif

#ifdef Win32
void R_fixslash(char *s)
{
    char *p = s;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *p = '/';
	    p += used;
	}
    } else
#endif
	for (; *p; p++) if (*p == '\\') *p = '/';
}

void R_fixbackslash(char *s)
{
    char *p = s;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '/') *p = '\\';
	    p += used;
	}
    } else
#endif
	for (; *p; p++) if (*p == '/') *p = '\\';
}
#endif

void F77_SYMBOL(rexitc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
        warning(_("error message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, nc);
    buf[nc] = '\0';
    error(buf);
}

void F77_SYMBOL(rwarnc)(char *msg, int *nchar)
{
    int nc = *nchar;
    char buf[256];
    if(nc > 255) {
        warning(_("warning message truncated to 255 chars"));
	nc = 255;
    }
    strncpy(buf, msg, nc);
    buf[nc] = '\0';
    warning(buf);
}

void F77_SYMBOL(rchkusr)(void)
{
    R_CheckUserInterrupt();
}
