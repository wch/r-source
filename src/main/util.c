/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
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
   isBlankString has been improved.
   do_basename and do_dirname now work in chars.
*/



#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#undef COMPILING_R

#define imax2(x, y) ((x < y) ? y : x)
#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef __cplusplus
#include "Clinkage.h"

extern "C" {
#endif
void F77_SYMBOL(rwarnc)(char *msg, int *nchar);
void F77_SYMBOL(rexitc)(char *msg, int *nchar);

#ifdef __cplusplus
}
#endif

/* Many small functions are included from ../include/Rinlinedfuns.h */

Rboolean tsConform(SEXP x, SEXP y)
{
    if ((x = getAttrib(x, R_TspSymbol)) != R_NilValue &&
	(y = getAttrib(y, R_TspSymbol)) != R_NilValue) {
	/* tspgets should enforce this, but prior to 2.4.0
	   had INTEGER() here */
	if(TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP)
            return REAL(x)[0] == REAL(x)[0] &&
                REAL(x)[1] == REAL(x)[1] &&
                REAL(x)[2] == REAL(x)[2];
	/* else fall through */
    }
    return FALSE;
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


void attribute_hidden internalTypeCheck(SEXP call, SEXP s, SEXPTYPE type)
{
    if (TYPEOF(s) != type) {
	if (call)
	    errorcall(call, type_msg);
	else
	    error(type_msg);
    }
}

const static char * const truenames[] = {
    "T",
    "True",
    "TRUE",
    "true",
    (char *) NULL,
};

const static char * const falsenames[] = {
    "F",
    "False",
    "FALSE",
    "false",
    (char *) NULL,
};

SEXP asChar(SEXP x)
{
    if (LENGTH(x) >= 1) {
	if (isVectorAtomic(x)) {
	    int w, d, e, wi, di, ei;
	    char buf[MAXELTSIZE];  /* probably 100 would suffice */

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
	else if(TYPEOF(x) == SYMSXP)
	    return PRINTNAME(x);
    }
    return NA_STRING;
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
    { "S4",		S4SXP },
    /* aliases : */
    { "numeric",	REALSXP	   },
    { "name",		SYMSXP	   },

    { (char *)NULL,	-1	   }
};


SEXPTYPE str2type(const char *s)
{
    int i;
    for (i = 0; TypeTable[i].str; i++) {
	if (!strcmp(s, TypeTable[i].str))
	    return TypeTable[i].type;
    }
    /* SEXPTYPE is an unsigned int, so the compiler warns us w/o the cast. */
    return (SEXPTYPE) -1;
}


SEXP type2str(SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return mkChar(TypeTable[i].str);
    }
    error(_("type %d is unimplemented in '%s'"), t, "type2str");
    return R_NilValue; /* for -Wall */
}

const char *type2char(SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return TypeTable[i].str;
    }
    error(_("type %d is unimplemented in '%s'"), t, "type2char");
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
	    return install((char *) &TypeTable[i].str);
    }
    error(_("type %d is unimplemented in '%s'"), t, "type2symbol");
    return R_NilValue; /* for -Wall */
}

void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t)
{
    int i;

    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    error(_("unimplemented type '%s' in '%s'\n"), TypeTable[i].str, s);
    }
    error(_("unimplemented type (%d) in '%s'\n"), t, s);
}

void UNIMPLEMENTED_TYPE(const char *s, SEXP x)
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


/*
 * out=NULL returns the number of the MBCS chars
 */
/* Note: this does not terminate out, as all current uses are to look
 * at 'out' a wchar at a time, and sometimes just one char.
 */
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout)
{
    void   *cd = NULL ;
    const char *i_buf;
    char *o_buf;
    size_t  i_len, o_len, status, wc_len;

    /* out length */
    wc_len = mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open(UCS2ENC, "")))
	return (size_t) -1;

    i_buf = (char *)in;
    i_len = strlen(in); /* not including terminator */
    o_buf = (char *)out;
    o_len = nout * sizeof(ucs2_t);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);

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
#endif /* SUPPORT_MBCS */


#ifdef SUPPORT_MBCS
#include <wctype.h>
#endif

/* This one is not in Rinternals.h, but is used in internet module */
Rboolean isBlankString(const char *s)
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

Rboolean StringTrue(const char *name)
{
    int i;
    for (i = 0; truenames[i]; i++)
	if (!strcmp(name, truenames[i]))
	    return TRUE;
    return FALSE;
}

Rboolean StringFalse(const char *name)
{
    int i;
    for (i = 0; falsenames[i]; i++)
	if (!strcmp(name, falsenames[i]))
	    return TRUE;
    return FALSE;
}

SEXP attribute_hidden EnsureString(SEXP s)
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

/* used in modules */
void Rf_checkArityCall(SEXP op, SEXP args, SEXP call)
{
    if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args)) {
	if (PRIMINTERNAL(op))
	    error(P_("%d argument passed to .Internal(%s) which requires %d",
		     "%d arguments passed to .Internal(%s) which requires %d",
		     length(args)),
		  length(args), PRIMNAME(op), PRIMARITY(op));
	else
	    errorcall(call, P_("%d argument passed to '%s' which requires %d",
			       "%d arguments passed to '%s' which requires %d",
			       length(args)),
		      length(args), PRIMNAME(op), PRIMARITY(op));
    }
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
    RCNTXT *cptr;
    int nargs = NA_INTEGER;
    for (cptr = R_GlobalContext; cptr != NULL; cptr = cptr->nextcontext) {
	if ((cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == rho) {
	    nargs = length(cptr->promargs);
	    break;
	}
    }
    return ScalarInteger(nargs);
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


static void isort_with_index(int *x, int *indx, int n)
{
    int i, j, h, iv, v;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && x[j - h] > v)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
}


/* merge(xinds, yinds, all.x, all.y) */
/* xinds, yinds are along x and y rows matching into the (numeric)
   common indices, with 0 for non-matches.

   all.x and all.y are boolean.

   The return value is a list with 4 elements (xi, yi, x.alone, y.alone),
   which are index vectors for rows of x or y.
*/
SEXP attribute_hidden do_merge(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP xi, yi, ansx, ansy, ans, ansnames, x_lone, y_lone;
    int nx = 0, ny = 0, i, j, k, nans = 0, nx_lone = 0, ny_lone = 0;
    int all_x = 0, all_y = 0, ll = 0/* "= 0" : for -Wall */;
    int *ix, *iy, tmp, nnx, nny, i0, j0;

    checkArity(op, args);
    xi = CAR(args);
    if ( !isInteger(xi) || !(nx = LENGTH(xi)) )
	error(_("invalid '%s' argument"), "xinds");
    yi = CADR(args);
    if ( !isInteger(yi) || !(ny = LENGTH(yi)) )
	error(_("invalid '%s' argument"), "yinds");
    if(!LENGTH(ans = CADDR(args)) || NA_LOGICAL == (all_x = asLogical(ans)))
	error(_("'all.x' must be TRUE or FALSE"));
    if(!LENGTH(ans = CADDDR(args))|| NA_LOGICAL == (all_y = asLogical(ans)))
	error(_("'all.y' must be TRUE or FALSE"));

    /* 0. sort the indices */
    ix = (int *) R_alloc(nx, sizeof(int));
    iy = (int *) R_alloc(ny, sizeof(int));
    for(i = 0; i < nx; i++) ix[i] = i+1;
    for(i = 0; i < ny; i++) iy[i] = i+1;
    isort_with_index(INTEGER(xi), ix, nx);
    isort_with_index(INTEGER(yi), iy, ny);

    /* 1. determine result sizes */
    for (i = 0; i < nx; i++) if (INTEGER(xi)[i] > 0) break; nx_lone = i;
    for (i = 0; i < ny; i++) if (INTEGER(yi)[i] > 0) break; ny_lone = i;
    for (i = nx_lone, j = ny_lone; i < nx; i = nnx, j = nny) {
	tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	/* the next is not in theory necessary,
	   since we have the common values only */
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	/* printf("i %d nnx %d j %d nny %d\n", i, nnx, j, nny); */
	nans += (nnx-i)*(nny-j);
    }


    /* 2. allocate and store result components */
    PROTECT(ans = allocVector(VECSXP, 4));
    ansx = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);

    if(all_x) {
	x_lone = allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	for (i = 0, ll = 0; i < nx_lone; i++)
	    INTEGER(x_lone)[ll++] = ix[i];
    }

    if(all_y) {
	y_lone = allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	for (i = 0, ll = 0; i < ny_lone; i++)
	    INTEGER(y_lone)[ll++] = iy[i];
    }

    for (i = nx_lone, j = ny_lone, k = 0; i < nx; i = nnx, j = nny) {
	tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	for(i0 = i; i0 < nnx; i0++)
	    for(j0 = j; j0 < nny; j0++) {
		INTEGER(ansx)[k]   = ix[i0];
		INTEGER(ansy)[k++] = iy[j0];
	    }
    }

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
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#endif

SEXP static intern_getwd()
{
    SEXP rval = R_NilValue;
    char buf[PATH_MAX+1];

#ifdef Win32
    int res = GetCurrentDirectory(PATH_MAX, buf);
    if(res > 0) {
	R_fixslash(buf);
	rval = mkString(buf);
    }
#elif defined(HAVE_GETCWD)
    char *res = getcwd(buf, PATH_MAX);
    if(res) rval = mkString(buf);
#endif
    return(rval);
}

SEXP attribute_hidden do_getwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return(intern_getwd());
}


#if defined(Win32) && defined(_MSC_VER)
# include <direct.h> /* for chdir, via io.h */
#endif

SEXP attribute_hidden do_setwd(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = R_NilValue, wd = R_NilValue;	/* -Wall */
    const char *path;

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	error(_("character argument expected"));

    /* get current directory to return */
    wd = intern_getwd();

    path = R_ExpandFileName(translateChar(STRING_ELT(s, 0)));
#ifdef HAVE_CHDIR
    if(chdir(path) < 0)
#endif
	error(_("cannot change working directory"));
    return(wd);
}

/* remove portion of path before file separator if one exists */

SEXP attribute_hidden do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    char  buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	if (strlen(pp) > PATH_MAX - 1)
	    error(_("path too long"));
	strcpy (buf, pp);
#ifdef Win32
	R_fixslash(buf);
#endif
	/* remove trailing file separator(s) */
	if (*buf) {
	    p = buf + strlen(buf) - 1;
	    while (p >= buf && *p == fsp) *(p--) = '\0';
	}
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
    char buf[PATH_MAX], *p, fsp = FILESEP[0];
    const char *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	if (strlen(pp) > PATH_MAX - 1)
	    error(_("path too long"));
	strcpy (buf, pp);
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
		  /* this covers both drives and network shares */
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
    const char *cs;
    Rboolean findWidth;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    if(isNull(CADR(args))) w = NA_INTEGER;
    else {
	w = asInteger(CADR(args));
	if(w != NA_INTEGER && w < 0)
	    error(_("invalid '%s' value"), "width");
    }
    findWidth = (w == NA_INTEGER);
    s = CADDR(args);
    if(LENGTH(s) != 1 || TYPEOF(s) != STRSXP)
	error(_("invalid '%s' value"), "quote");
    cs = translateChar(STRING_ELT(s, 0));
    if(strlen(cs) > 0) quote = cs[0];
    if(strlen(cs) > 1)
	warning(_("only the first character of 'quote' will be used"));
    justify = asInteger(CADDDR(args));
    if(justify == NA_INTEGER || justify < 0 || justify > 3)
	error(_("invalid '%s' value"), "justify");
    if(justify == 3) w = 0;
    na = asLogical(CAD4R(args));
    if(na == NA_LOGICAL) error(_("invalid '%s' value"), "na.encode");

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
	    SET_STRING_ELT(ans, i, mkChar(EncodeString(s, w, quote, (Rprt_adj) justify)));
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_encoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    int i, n;
    char *tmp;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    n = LENGTH(x);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if(IS_LATIN1(STRING_ELT(x, i))) tmp = "latin1";
	else if(IS_UTF8(STRING_ELT(x, i))) tmp = "UTF-8";
	else tmp = "unknown";
	SET_STRING_ELT(ans, i, mkChar(tmp));
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_setencoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, enc, tmp;
    int i, m, n;
    const char *this;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    if (TYPEOF(enc = CADR(args)) != STRSXP)
	error(_("a character vector 'value' expected"));
    m = LENGTH(enc);
    if(m == 0)
	error(_("'value' must be of positive length"));
    if(NAMED(x)) x = duplicate(x);
    PROTECT(x);
    n = LENGTH(x);
    for(i = 0; i < n; i++) {
	tmp = STRING_ELT(x, i);
	UNSET_LATIN1(tmp);
	UNSET_UTF8(tmp);
	this = CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if(streql(this, "latin1")) SET_LATIN1(tmp);
	else if(streql(this, "UTF-8")) SET_UTF8(tmp);
	SET_STRING_ELT(x, i, tmp);
    }
    UNPROTECT(1);
    return x;
}

void attribute_hidden markKnown(SEXP x, SEXP ref)
{
    if(TYPEOF(x) != CHARSXP)
	error("invalid use of 'markKnown'");
    if(IS_LATIN1(ref) || IS_UTF8(ref)) {
	if(known_to_be_latin1) SET_LATIN1(x);
	if(known_to_be_utf8) SET_UTF8(x);
    }
}

/* Note: this is designed to be fast and valid only for UTF-8 strings.
   It is also correct in EUC-* locales. */
Rboolean utf8strIsASCII(const char *str)
{
    const char *p;
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

int attribute_hidden utf8clen(char c)
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

Rboolean mbcsValid(const char *str)
{
    return  ((int)mbstowcs(NULL, str, 0) >= 0);
}

/* We do this conversion ourselves to do our own error recovery */
void mbcsToLatin1(const char *in, char *out)
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
Rboolean mbcsValid(const CHAR *str) { return TRUE; }
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
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
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
    error("%s", buf);
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
    warning("%s", buf);
}

void F77_SYMBOL(rchkusr)(void)
{
    R_CheckUserInterrupt();
}

/* Return a copy of a string using memory from R_alloc */
char *acopy_string(const char *in)
{
    char *out;
    int len = strlen(in);
    if (len > 0) {
        out = (char *) R_alloc(strlen(in), sizeof(char));
        strcpy(out, in);
    } else
        out = "";
    return out;
}
