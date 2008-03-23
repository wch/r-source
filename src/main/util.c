/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
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

#ifdef Win32
static void R_UTF8fixslash(char *s);
static void R_wfixslash(wchar_t *s);
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
	} else if(TYPEOF(x) == CHARSXP) {
	    return x;
	} else if(TYPEOF(x) == SYMSXP)
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
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc)
{
    void   *cd = NULL ;
    const char *i_buf;
    char *o_buf;
    size_t  i_len, o_len, status, wc_len;

    /* out length */
    wc_len = mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open(UCS2ENC, (enc == CE_UTF8) ? "UTF-8": "")))
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

SEXP static intern_getwd(void)
{
    SEXP rval = R_NilValue;
    char buf[PATH_MAX+1];

#ifdef Win32
    {
	wchar_t wbuf[PATH_MAX+1];
	int res = GetCurrentDirectoryW(PATH_MAX, wbuf);
	if(res > 0) {
	    wcstoutf8(buf, wbuf, PATH_MAX+1);
	    R_UTF8fixslash(buf);
	    rval = allocVector(STRSXP, 1);
	    SET_STRING_ELT(rval, 0, mkCharEnc(buf, UTF8_MASK));
	}
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

    checkArity(op, args);
    if (!isPairList(args) || !isValidString(s = CAR(args)))
	error(_("character argument expected"));
    if (STRING_ELT(s, 0) == NA_STRING)
	error(_("missing value is invalid"));

    /* get current directory to return */
    wd = intern_getwd();

#ifdef Win32
    {
	const wchar_t *path = filenameToWchar(STRING_ELT(s, 0), TRUE);
	if(_wchdir(path) < 0)
	    error(_("cannot change working directory"));
    }
#else
    {
	const char *path 
	    = R_ExpandFileName(translateChar(STRING_ELT(s, 0)));
# ifdef HAVE_CHDIR
    if(chdir(path) < 0)
# endif
	error(_("cannot change working directory"));
    }
#endif
    return(wd);
}

/* remove portion of path before file separator if one exists */

#ifdef Win32
SEXP attribute_hidden do_basename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    char sp[PATH_MAX];
    wchar_t  buf[PATH_MAX], *p;
    const wchar_t *pp;
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING) 
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1) error(_("path too long"));
	    wcscpy(buf, pp);
	    R_wfixslash(buf);
	    /* remove trailing file separator(s) */
	    if (*buf) {
		p = buf + wcslen(buf) - 1;
		while (p >= buf && *p == L'/') *(p--) = L'\0';
	    }
	    if ((p = wcsrchr(buf, L'/'))) p++; else p = buf;
	    wcstoutf8(sp, p, wcslen(p) + 1);
	    SET_STRING_ELT(ans, i, mkCharEnc(sp, UTF8_MASK));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
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
	if (STRING_ELT(s, i) == NA_STRING) 
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    strcpy (buf, pp);
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
    }
    UNPROTECT(1);
    return(ans);
}
#endif

/* remove portion of path after last file separator if one exists, else
   return "."
   */

#ifdef Win32
SEXP attribute_hidden do_dirname(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, s = R_NilValue;	/* -Wall */
    wchar_t buf[PATH_MAX], *p;
    const wchar_t *pp;
    char sp[PATH_MAX];
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING) 
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    wcscpy (buf, pp);
	    R_wfixslash(buf);
	    /* remove trailing file separator(s) */
	    while ( *(p = buf + wcslen(buf) - 1) == L'/'  && p > buf
		    && (p > buf+2 || *(p-1) != L':')) *p = L'\0';
	    p = wcsrchr(buf, L'/');
	    if(p == NULL) wcscpy(buf, L".");
	    else {
		while(p > buf && *p == L'/'
		      /* this covers both drives and network shares */
		      && (p > buf+2 || *(p-1) != L':')) --p;
		p[1] = L'\0';
	    }
	    wcstoutf8(sp, buf, wcslen(buf)+1);
	    SET_STRING_ELT(ans, i, mkCharEnc(sp, UTF8_MASK));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#else
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
	if (STRING_ELT(s, i) == NA_STRING) 
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    pp = R_ExpandFileName(translateChar(STRING_ELT(s, i)));
	    if (strlen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    strcpy (buf, pp);
	    /* remove trailing file separator(s) */
	    while ( *(p = buf + strlen(buf) - 1) == fsp  && p > buf) *p = '\0';
	    p = Rf_strrchr(buf, fsp);
	    if(p == NULL)
		strcpy(buf, ".");
	    else {
		while(p > buf && *p == fsp) --p;
		p[1] = '\0';
	    }
	    SET_STRING_ELT(ans, i, mkChar(buf));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif

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
	int ienc = 0;
	this = CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if(streql(this, "latin1")) ienc = LATIN1_MASK;
	else if(streql(this, "UTF-8")) ienc = UTF8_MASK;
	tmp = STRING_ELT(x, i);
	if (! ((ienc == LATIN1_MASK && IS_LATIN1(tmp)) ||
	       (ienc == UTF8_MASK && IS_UTF8(tmp)) ||
	       (ienc == 0 && ! IS_LATIN1(tmp) && ! IS_UTF8(tmp))))
	    SET_STRING_ELT(x, i, mkCharEnc(CHAR(tmp), ienc));
    }
    UNPROTECT(1);
    return x;
}

SEXP attribute_hidden markKnown(const char *s, SEXP ref)
{
    int ienc = 0;
    if(ENC_KNOWN(ref)) {
	if(known_to_be_latin1) ienc = LATIN1_MASK;
	if(known_to_be_utf8) ienc = UTF8_MASK;
    }
    return mkCharEnc(s, ienc);
}

Rboolean strIsASCII(const char *str)
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

/* This returns the result in wchar_t, but does not assume 
   wchar_t is UCS-2/4 and so is for internal use only */
size_t attribute_hidden 
utf8toucs(wchar_t *wc, const char *s)
{
    unsigned int byte;
    wchar_t local, *w;
    byte = *((unsigned char *)s);
    w = wc ? wc: &local;

    if (byte == 0) {
        *w = (wchar_t) 0;
        return 0;
    } else if (byte < 0xC0) {
        *w = (wchar_t) byte;
        return 1;
    } else if (byte < 0xE0) {
	if(strlen(s) < 2) return -2;
        if ((s[1] & 0xC0) == 0x80) {
            *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
            return 2;
        } else return -1;
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return -2;
        if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
            *w = (wchar_t) (((byte & 0x0F) << 12)
                    | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
	    byte = *w;
	    /* Surrogates range */
	    if(byte >= 0xD800 && byte <= 0xDFFF) return -1;
	    if(byte == 0xFFFE || byte == 0xFFFF) return -1;
            return 3;
        } else return -1;
    }
    if(sizeof(wchar_t) < 4) return -2;
    /* So now handle 4,5.6 byte sequences with no testing */
    if (byte < 0xf8) {
	if(strlen(s) < 4) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 18)
			| ((s[1] & 0x3F) << 12)
			| ((s[2] & 0x3F) << 6)
			| (s[3] & 0x3F));
	return 4;
    } else if (byte < 0xFC) {
	if(strlen(s) < 5) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 24)
			| ((s[1] & 0x3F) << 12)
			| ((s[2] & 0x3F) << 12)
			| ((s[3] & 0x3F) << 6)
			| (s[4] & 0x3F));
	return 5;
    } else {
	if(strlen(s) < 6) return -2;
	*w = (wchar_t) (((byte & 0x0F) << 30)
			| ((s[1] & 0x3F) << 24)
			| ((s[2] & 0x3F) << 18)
			| ((s[3] & 0x3F) << 12)
			| ((s[4] & 0x3F) << 6)
			| (s[5] & 0x3F));
	return 6;
    }
}

size_t attribute_hidden 
utf8towcs(wchar_t *wc, const char *s, size_t n)
{
    int m, res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    m  = utf8toucs(p, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (res >= n) break;
	}
    else
	for(t = s; ; res++, t += m) {
	    m  = utf8toucs(&local, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	}
    return res;
}

/* based on pcre.c */
static const int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

static size_t Rwcrtomb(char *s, const wchar_t wc)
{
    register int i, j;
    unsigned int cvalue = wc;
    char buf[10], *b;

    b = s ? s : buf;
    if(cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
	if (cvalue <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
	*b-- = 0x80 | (cvalue & 0x3f);
	cvalue >>= 6;
    }
    *b = utf8_table2[i] | cvalue;
    return i + 1;
}

/* attribute_hidden? */
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n)
{
    int m, res=0;
    char *t;
    const wchar_t *p;
    if(s) {
	for(p = wc, t = s; ; p++) {
	    m  = Rwcrtomb(t, *p);
	    if(m <= 0) break;
	    res += m;
	    if(res >= n) break;
	    t += m;
	}
    } else {
	for(p = wc; ; p++) {
	    m  = Rwcrtomb(NULL, *p);
	    if(m <= 0) break;
	    res += m;
	}
    }
    return res;
}

/* A version that reports failure as an error */
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return (size_t)0;
    used = mbrtowc(wc, s, n, ps);
    if((int) used < 0) {
	/* let's try to print out a readable version */
	char *err = alloca(4*strlen(s) + 1), *q;
	const char *p;
	R_CheckStack();
	for(p = s, q = err; *p; ) {
	    /* don't do the first to keep ps state straight */
	    if(p > s) used = mbrtowc(NULL, p, n, ps);
	    if(used == 0) break;
	    else if((int) used > 0) {
		memcpy(q, p, used);
		p += used;
		q += used;
		n -= used;
	    } else {
		sprintf(q, "<%02x>", (unsigned char) *p++);
		q += 4;
		n--;
	    }
	}
	*q = '\0';
	error(_("invalid multibyte string at '%s'"), err);
    }
    return used;
}

Rboolean mbcsValid(const char *str)
{
    return  ((int)mbstowcs(NULL, str, 0) >= 0);
}

#ifdef UNUSED
/* We do this conversion ourselves to do our own error recovery */
void mbcsToLatin1(const char *in, char *out)
{
    wchar_t *wbuff;
    int i;
    size_t res = mbstowcs(NULL, in, 0), mres;

    if(res == (size_t)(-1)) {
	/* let's try to print out a readable version */
	size_t used, n = strlen(in);
	char *err = alloca(4*n + 1), *q;
	const char *p;
	mbstate_t ps;
	R_CheckStack();
	for(p = in, q = err; *p; ) {
	    used = mbrtowc(NULL, p, n, &ps);
	    if(used == 0) break;
	    else if((int) used > 0) {
		memcpy(q, p, used);
		p += used;
		q += used;
		n -= used;
	    } else {
		sprintf(q, "<%02x>", (unsigned char) *p++);
		q += 4;
		n--;
	    }
	}
	*q = '\0';
	warning(_("invalid input '%s' in mbcsToLatin1: omitted"), err);
	*out = '\0';
	return;
    }
    wbuff = (wchar_t *) alloca((res+1) * sizeof(wchar_t));
    R_CheckStack();
    if(!wbuff) error(_("allocation failure in '%s'"), "mbcsToLatin1");
    mres = mbstowcs(wbuff, in, res+1);
    if(mres == (size_t)-1) /* we checked above, so should not get here */
	error("invalid input in 'mbcsToLatin1'");
    for(i = 0; i < res; i++) {
	/* here we do assume Unicode wchars */
	if(wbuff[i] > 0xFF) out[i] = '.';
	else out[i] = (char) wbuff[i];
    }
    out[res] = '\0';
}
#endif

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
Rboolean mbcsValid(const char *str) { return TRUE; }
#ifdef UNUSED
void mbcsToLatin1(char *in, char *out) {}
#endif
#undef Rf_strchr
char *Rf_strchr(const char *s, int c) {return strchr(s, c);}
#undef Rf_strrchr
char *Rf_strrchr(const char *s, int c) {return strrchr(s, c);}
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

static void R_UTF8fixslash(char *s)
{
    char *p = s;

	for (; *p; p++) if (*p == '\\') *p = '/';
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

static void R_wfixslash(wchar_t *s)
{
    wchar_t *p = s;

    for (; *p; p++) if (*p == L'\\') *p = L'/';
    /* preserve network shares */
    if(s[0] == L'/' && s[1] == L'/') s[0] = s[1] = L'\\';
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
        out = (char *) R_alloc(1+strlen(in), sizeof(char));
        strcpy(out, in);
    } else
        out = "";
    return out;
}


/* FIXME: consider inlining here */
#ifdef Win32

static int Rstrcoll(const char *s1, const char *s2)
{
    wchar_t *w1, *w2;
    w1 = (wchar_t *) alloca((strlen(s1)+1)*sizeof(wchar_t));
    w2 = (wchar_t *) alloca((strlen(s2)+1)*sizeof(wchar_t));
    R_CheckStack();
    utf8towcs(w1, s1, strlen(s1));
    utf8towcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

int Scollate(SEXP a, SEXP b)
{
    if(getCharEnc(a) == CE_UTF8 || getCharEnc(b) == CE_UTF8)
	return Rstrcoll(translateCharUTF8(a), translateCharUTF8(b));
    else 
	return strcoll(translateChar(a), translateChar(b));
}

int Seql(SEXP a, SEXP b)
{
    return (a == b) || !strcmp(translateCharUTF8(a), translateCharUTF8(b));
}

#else

# ifdef HAVE_STRCOLL
#  define STRCOLL strcoll
# else
#  define STRCOLL strcmp
# endif

int Scollate(SEXP a, SEXP b)
{
    return STRCOLL(translateChar(a), translateChar(b));
}

int Seql(SEXP a, SEXP b)
{
    return (a == b) || !strcmp(translateChar(a), translateChar(b));
}

#endif


/* Table from 
http://unicode.org/Public/MAPPINGS/VENDORS/ADOBE/symbol.txt
*/

static int s2u[224] = {
    0x0020, 0x0021, 0x2200, 0x0023, 0x2203, 0x0025, 0x0026, 0x220D,
    0x0028, 0x0029, 0x2217, 0x002B, 0x002C, 0x2212, 0x002E, 0x002F,
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037,
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F,
    0x2245, 0x0391, 0x0392, 0x03A7, 0x0394, 0x0395, 0x03A6, 0x0393,
    0x0397, 0x0399, 0x03D1, 0x039A, 0x039B, 0x039C, 0x039D, 0x039F,
    0x03A0, 0x0398, 0x03A1, 0x03A3, 0x03A4, 0x03A5, 0x03C2, 0x03A9,
    0x039E, 0x03A8, 0x0396, 0x005B, 0x2234, 0x005D, 0x22A5, 0x005F,
    0xF8E5, 0x03B1, 0x03B2, 0x03C7, 0x03B4, 0x03B5, 0x03C6, 0x03B3,
    0x03B7, 0x03B9, 0x03D5, 0x03BA, 0x03BB, 0x03BC, 0x03BD, 0x03BF,
    0x03C0, 0x03B8, 0x03C1, 0x03C3, 0x03C4, 0x03C5, 0x03D6, 0x03C9,
    0x03BE, 0x03C8, 0x03B6, 0x007B, 0x007C, 0x007D, 0x223C, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020,
    0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 0x0020, 
    0x20AC, 0x03D2, 0x2032, 0x2264, 0x2044, 0x221E, 0x0192, 0x2663,
    0x2666, 0x2665, 0x2660, 0x2194, 0x2190, 0x2191, 0x2192, 0x2193,
    0x00B0, 0x00B1, 0x2033, 0x2265, 0x00D7, 0x221D, 0x2202, 0x2022,
    0x00F7, 0x2260, 0x2261, 0x2248, 0x2026, 0xF8E6, 0xF8E7, 0x21B5,
    0x2135, 0x2111, 0x211C, 0x2118, 0x2297, 0x2295, 0x2205, 0x2229,
    0x222A, 0x2283, 0x2287, 0x2284, 0x2282, 0x2286, 0x2208, 0x2209,
    0x2220, 0x2207, 0xF6DA, 0xF6D9, 0xF6DB, 0x220F, 0x221A, 0x22C5,
    0x00AC, 0x2227, 0x2228, 0x21D4, 0x21D0, 0x21D1, 0x21D2, 0x21D3,
    0x25CA, 0x2329, 0xF8E8, 0xF8E9, 0xF8EA, 0x2211, 0xF8EB, 0xF8EC,
    0xF8ED, 0xF8EE, 0xF8EF, 0xF8F0, 0xF8F1, 0xF8F2, 0xF8F3, 0xF8F4,
    0x0020, 0x232A, 0x222B, 0x2320, 0xF8F5, 0x2321, 0xF8F6, 0xF8F7, 
    0xF8F8, 0xF8F9, 0xF8FA, 0xF8FB, 0xF8FC, 0xF8FD, 0xF8FE, 0x0020 
};

void *Rf_AdobeSymbol2utf8(char *work, const char *c0, int nwork) 
{
    const unsigned char *c = (unsigned char *) c0;
    unsigned char *t = (unsigned char *) work;
    while (*c) {
	if (*c < 32) *t++ = ' ';
	else {
	    unsigned int u = s2u[*c - 32];
	    if (u < 128) *t++ = u;
	    else if (u < 0x800) {
		*t++ = 0xc0 | (u >> 6);
		*t++ = 0x80 | (u & 0x3f);
	    } else {
		*t++ = 0xe0 | (u >> 12);
		*t++ = 0x80 | ((u >> 6) & 0x3f);
		*t++ = 0x80 | (u & 0x3f);
	    }
	}
	if (t+6 > (unsigned char *)(work + nwork)) break;
	c++;
    }
    *t = '\0';
    return (char*) work;
}

int attribute_hidden Rf_AdobeSymbol2ucs2(int n)
{
    if(n >= 32 && n < 256) return s2u[n-32];
    else return 0;
}

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA)
{
    LDOUBLE ans = 0.0, p10 = 10.0, fac = 1.0;
    int n, expn = 0, sign = 1, ndigits = 0;
    const char *p = str;

    /* optional whitespace */
    while (isspace(*p)) p++;

    if (NA && strncmp(p, "NA", 2) == 0) {
	ans = NA_REAL; 
	p += 2;
	goto done;
    }

   /* optional sign */
    switch (*p) {
    case '-': sign = -1;
    case '+': p++;
    default: ;
    }

    if (strncasecmp(p, "NaN", 3) == 0) {
	ans = R_NaN;
	p += 3;
	goto done;
    } else if (strncasecmp(p, "Inf", 3) == 0) {
	ans = R_PosInf; 
	p += 3;
	goto done;
    /* C99 specifies this */
    } else if (strncasecmp(p, "infinity", 8) == 0) {
        ans = R_PosInf;
        p += 8;
        goto done;
    }

    if(strlen(p) > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
	/* This will overflow to Inf if appropriate */
	for(p += 2; p; p++) {
	    if('0' <= *p && *p <= '9') ans = 16*ans + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ans = 16*ans + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ans = 16*ans + (*p -'A' + 10);
	    else break;
	}
	if (*p == 'p' || *p == 'P') {
	    int expsign = 1;
	    double p2 = 2.0;
	    switch(*++p) {
	    case '-': expsign = -1;
	    case '+': p++;
	    default: ;
	    }
	    for (n = 0; *p >= '0' && *p <= '9'; p++) n = n * 10 + (*p - '0');
	    expn += expsign * n;
	    if (expn < 0) {
		for (n = -expn, fac = 1.0; n; n >>= 1, p2 *= p2) 
		    if (n & 1) fac *= p2;
		ans /= fac;
	    } else {
		for (n = expn, fac = 1.0; n; n >>= 1, p2 *= p2)
		    if (n & 1) fac *= p2;
		ans *= fac;
	    }
	}
	goto done;
    }

    for ( ; *p >= '0' && *p <= '9'; p++, ndigits++) ans = 10*ans + (*p - '0');
    if (*p == dec)
	for (p++; *p >= '0' && *p <= '9'; p++, ndigits++, expn--)
	    ans = 10*ans + (*p - '0');
    if (ndigits == 0) goto done; /* maybe throw an error? */


    if (*p == 'e' || *p == 'E') {
	int expsign = 1;
	switch(*++p) {
	case '-': expsign = -1;
	case '+': p++;
	default: ;
	}
	for (n = 0; *p >= '0' && *p <= '9'; p++) n = n * 10 + (*p - '0');
	expn += expsign * n;
    }

    /* avoid unnecessary underflow for large negative exponents */
    if (expn + ndigits < -300) {
	for (n = 0; n < ndigits; n++) ans /= 10.0;
	expn += ndigits;
    }
    if (expn < -307) { /* use underflow, not overflow */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10) 
	    if (n & 1) fac /= p10;
	ans *= fac;	
    } else if (expn < 0) { /* positive powers are exact */
	for (n = -expn, fac = 1.0; n; n >>= 1, p10 *= p10) 
	    if (n & 1) fac *= p10;
	ans /= fac;
    } else {
	for (n = expn, fac = 1.0; n; n >>= 1, p10 *= p10)
	    if (n & 1) fac *= p10;
	ans *= fac;
    }


done:
    if (endptr) *endptr = (char *) p;
    return sign * (double) ans;
}

double R_strtod(const char *str, char **endptr)
{
    return R_strtod4(str, endptr, '.', FALSE);
}

double R_atof(const char *str)
{
    return R_strtod4(str, NULL, '.', FALSE);
}

