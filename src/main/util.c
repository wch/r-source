/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
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
#include <R_ext/Print.h>
#include <ctype.h>		/* for isspace */
#include <float.h>		/* for DBL_MAX */

#undef COMPILING_R

#define R_imax2(x, y) ((x < y) ? y : x)
#include <Print.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef Win32
void R_UTF8fixslash(char *s);
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

attribute_hidden
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

#ifdef UNUSED
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
#endif

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
    if (XLENGTH(x) >= 1) {
	if (isVectorAtomic(x)) {
	    int w, d, e, wi, di, ei;
	    char buf[MAXELTSIZE];  /* Probably 100 would suffice */

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
		snprintf(buf, MAXELTSIZE, "%d", INTEGER(x)[0]);
		return mkChar(buf);
	    case REALSXP:
		PrintDefaults();
		formatReal(REAL(x), 1, &w, &d, &e, 0);
		return mkChar(EncodeReal0(REAL(x)[0], w, d, e, OutDec));
	    case CPLXSXP:
		PrintDefaults();
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
    { "bytecode",	BCODESXP   },
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
	    return (SEXPTYPE) TypeTable[i].type;
    }
    /* SEXPTYPE is an unsigned int, so the compiler warns us w/o the cast. */
    return (SEXPTYPE) -1;
}

static struct {
    const char *cstrName;
    SEXP rcharName;
    SEXP rstrName;
    SEXP rsymName;
} Type2Table[MAX_NUM_SEXPTYPE];


int findTypeInTypeTable(SEXPTYPE t) {
    int i;
    for (i = 0; TypeTable[i].str; i++) {
	if (TypeTable[i].type == t)
	    return i;
    }
    return -1;
}

void InitTypeTables(void) {

    /* Type2Table */
    int type;
    for (type = 0; type < MAX_NUM_SEXPTYPE; type++) {
        int j = findTypeInTypeTable(type);

        if (j != -1) {
            const char *cstr = TypeTable[j].str;
            SEXP rchar = PROTECT(mkChar(cstr));
            SEXP rstr = ScalarString(rchar);
            MARK_NOT_MUTABLE(rstr);
            R_PreserveObject(rstr);
            UNPROTECT(1); /* rchar */
            SEXP rsym = install(cstr);

            Type2Table[type].cstrName = cstr;
            Type2Table[type].rcharName = rchar;
            Type2Table[type].rstrName = rstr;
            Type2Table[type].rsymName = rsym;
        } else {
            Type2Table[type].cstrName = NULL;
            Type2Table[type].rcharName = NULL;
            Type2Table[type].rstrName = NULL;
            Type2Table[type].rsymName = NULL;
        }
    }
}

SEXP type2str_nowarn(SEXPTYPE t) /* returns a CHARSXP */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rcharName;
        if (res != NULL) return res;
    }
    return R_NilValue;
}

SEXP type2str(SEXPTYPE t) /* returns a CHARSXP */
{
    SEXP s = type2str_nowarn(t);
    if (s != R_NilValue) {
        return s;
    }
    warning(_("type %d is unimplemented in '%s'"), t, "type2str");
    char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return mkChar(buf);
}

SEXP type2rstr(SEXPTYPE t) /* returns a STRSXP */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rstrName;
        if (res != NULL) return res;
    }
    error(_("type %d is unimplemented in '%s'"), t, 
	  "type2ImmutableScalarString");
    return R_NilValue; /* for -Wall */
}

const char *type2char(SEXPTYPE t) /* returns a char* */
{
    if (t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        const char * res = Type2Table[t].cstrName;
        if (res != NULL) return res;
    }
    warning(_("type %d is unimplemented in '%s'"), t, "type2char");
    static char buf[50];
    snprintf(buf, 50, "unknown type #%d", t);
    return buf;
}

#ifdef UNUSED
SEXP type2symbol(SEXPTYPE t)
{
    if (t >= 0 && t < MAX_NUM_SEXPTYPE) { /* FIXME: branch not really needed */
        SEXP res = Type2Table[t].rsymName;
        if (res != NULL) {
            return res;
        }
    }
    error(_("type %d is unimplemented in '%s'"), t, "type2symbol");
    return R_NilValue; /* for -Wall */
}
#endif

attribute_hidden
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
    wc_len = (enc == CE_UTF8)? utf8towcs(NULL, in, 0) : mbstowcs(NULL, in, 0);
    if (out == NULL || (int)wc_len < 0) return wc_len;

    if ((void*)-1 == (cd = Riconv_open(UCS2ENC, (enc == CE_UTF8) ? "UTF-8": "")))
	return (size_t) -1;

    i_buf = (char *)in;
    i_len = strlen(in); /* not including terminator */
    o_buf = (char *)out;
    o_len = ((size_t) nout) * sizeof(ucs2_t);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);
    int serrno = errno;
    Riconv_close(cd);
    if (status == (size_t)-1) {
	switch(serrno){
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


#include <wctype.h>

/* This one is not in Rinternals.h, but is used in internet module */
Rboolean isBlankString(const char *s)
{
    if(mbcslocale) {
	wchar_t wc; size_t used; mbstate_t mb_st;
	mbs_init(&mb_st);
	while( (used = Mbrtowc(&wc, s, MB_CUR_MAX, &mb_st)) ) {
	    if(!iswspace((wint_t) wc)) return FALSE;
	    s += used;
	}
    } else
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

/* used in bind.c and options.c */
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

/* FIXME: ngettext reguires unsigned long, but %u would seem appropriate */
void Rf_checkArityCall(SEXP op, SEXP args, SEXP call)
{
    if (PRIMARITY(op) >= 0 && PRIMARITY(op) != length(args)) {
	if (PRIMINTERNAL(op))
	    error(ngettext("%d argument passed to .Internal(%s) which requires %d",
		     "%d arguments passed to .Internal(%s) which requires %d",
			   (unsigned long) length(args)),
		  length(args), PRIMNAME(op), PRIMARITY(op));
	else
	    errorcall(call,
		      ngettext("%d argument passed to '%s' which requires %d",
			       "%d arguments passed to '%s' which requires %d",
			       (unsigned long) length(args)),
		      length(args), PRIMNAME(op), PRIMARITY(op));
    }
}

void attribute_hidden Rf_check1arg(SEXP arg, SEXP call, const char *formal)
{
    SEXP tag = TAG(arg);
    const char *supplied;
    size_t ns;
    if (tag == R_NilValue) return;
    supplied = CHAR(PRINTNAME(tag)); ns = strlen(supplied);
    if (ns > strlen(formal) || strncmp(supplied, formal, ns))
	errorcall(call, _("supplied argument name '%s' does not match '%s'"),
		  supplied, formal);
}


SEXP nthcdr(SEXP s, int n)
{
    if (isList(s) || isLanguage(s) || isFrame(s) || TYPEOF(s) == DOTSXP ) {
	while( n-- > 0 ) {
	    if (s == R_NilValue)
		error(_("'nthcdr' list shorter than %d"), n);
	    s = CDR(s);
	}
	return s;
    }
    else error(_("'nthcdr' needs a list to CDR down"));
    return R_NilValue;/* for -Wall */
}


/* This is a primitive (with no arguments) */
SEXP attribute_hidden do_nargs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    int nargs = NA_INTEGER;

    checkArity(op, args);
    for (cptr = R_GlobalContext; cptr != NULL; cptr = cptr->nextcontext) {
	if ((cptr->callflag & CTXT_FUNCTION) && cptr->cloenv == rho) {
	    nargs = length(cptr->promargs);
	    break;
	}
    }
    return ScalarInteger(nargs);
}


/* formerly used in subscript.c, in Utils.h */
void attribute_hidden setIVector(int * vec, int len, int val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
}


/* unused in R, in Utils.h, apparently used in Rcpp  */
void attribute_hidden setRVector(double * vec, int len, double val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
}

/* unused in R, in Rinternals.h */
void setSVector(SEXP * vec, int len, SEXP val)
{
    for (int i = 0; i < len; i++) vec[i] = val;
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
    SEXP xi, yi, ansx, ansy, ans;
    int nx = 0, ny = 0, i, j, k, nx_lone = 0, ny_lone = 0;
    int all_x = 0, all_y = 0, ll = 0/* "= 0" : for -Wall */;
    int nnx, nny;

    checkArity(op, args);
    xi = CAR(args);
    // NB: long vectors are not supported for input
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
    int *ix = (int *) R_alloc((size_t) nx, sizeof(int));
    int *iy = (int *) R_alloc((size_t) ny, sizeof(int));
    for(i = 0; i < nx; i++) ix[i] = i+1;
    for(i = 0; i < ny; i++) iy[i] = i+1;
    isort_with_index(INTEGER(xi), ix, nx);
    isort_with_index(INTEGER(yi), iy, ny);

    /* 1. determine result sizes */
    for (i = 0; i < nx; i++) if (INTEGER(xi)[i] > 0) break; nx_lone = i;
    for (i = 0; i < ny; i++) if (INTEGER(yi)[i] > 0) break; ny_lone = i;
    double dnans = 0;
    for (i = nx_lone, j = ny_lone; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	/* the next is not in theory necessary,
	   since we have the common values only */
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	/* printf("i %d nnx %d j %d nny %d\n", i, nnx, j, nny); */
	dnans += ((double)(nnx-i))*(nny-j);
    }
    if (dnans > R_XLEN_T_MAX)
	error(_("number of rows in the result exceeds maximum vector length"));
    R_xlen_t nans = (int) dnans;


    /* 2. allocate and store result components */

    const char *nms[] = {"xi", "yi", "x.alone", "y.alone", ""};
    ans = PROTECT(mkNamed(VECSXP, nms));
    ansx = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 0, ansx);
    ansy = allocVector(INTSXP, nans);    SET_VECTOR_ELT(ans, 1, ansy);

    if(all_x) {
	SEXP x_lone = allocVector(INTSXP, nx_lone);
	SET_VECTOR_ELT(ans, 2, x_lone);
	for (i = 0, ll = 0; i < nx_lone; i++)
	    INTEGER(x_lone)[ll++] = ix[i];
    }

    if(all_y) {
	SEXP y_lone = allocVector(INTSXP, ny_lone);
	SET_VECTOR_ELT(ans, 3, y_lone);
	for (i = 0, ll = 0; i < ny_lone; i++)
	    INTEGER(y_lone)[ll++] = iy[i];
    }

    for (i = nx_lone, j = ny_lone, k = 0; i < nx; i = nnx, j = nny) {
	int tmp = INTEGER(xi)[i];
	for(nnx = i; nnx < nx; nnx++) if(INTEGER(xi)[nnx] != tmp) break;
	for(; j < ny; j++) if(INTEGER(yi)[j] >= tmp) break;
	for(nny = j; nny < ny; nny++) if(INTEGER(yi)[nny] != tmp) break;
	for(int i0 = i; i0 < nnx; i0++)
	    for(int j0 = j; j0 < nny; j0++) {
		INTEGER(ansx)[k]   = ix[i0];
		INTEGER(ansy)[k++] = iy[j0];
	    }
    }

    UNPROTECT(1);
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
    char buf[4*PATH_MAX+1];

#ifdef Win32
    {
	wchar_t wbuf[PATH_MAX+1];
	int res = GetCurrentDirectoryW(PATH_MAX, wbuf);
	if(res > 0) {
	    wcstoutf8(buf, wbuf, PATH_MAX+1);
	    R_UTF8fixslash(buf);
	    PROTECT(rval = allocVector(STRSXP, 1));
	    SET_STRING_ELT(rval, 0, mkCharCE(buf, CE_UTF8));
	    UNPROTECT(1);
	}
    }
#else
    char *res = getcwd(buf, PATH_MAX); /* can return NULL */
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
    if(chdir(path) < 0)
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
    char sp[4*PATH_MAX];
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
	    memset(sp, 0, 4*PATH_MAX); /* safety */
	    wcstoutf8(sp, p, 4*wcslen(p) + 1);
	    SET_STRING_ELT(ans, i, mkCharCE(sp, CE_UTF8));
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
    char sp[4*PATH_MAX];
    int i, n;

    checkArity(op, args);
    if (TYPEOF(s = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(s)));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(s, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    memset(sp, 0, 4*PATH_MAX);
	    pp = filenameToWchar(STRING_ELT(s, i), TRUE);
	    if (wcslen(pp) > PATH_MAX - 1)
		error(_("path too long"));
	    if (wcslen(pp)) {
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
		wcstoutf8(sp, buf, 4*wcslen(buf)+1);
	    }
	    SET_STRING_ELT(ans, i, mkCharCE(sp, CE_UTF8));
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
	    size_t ll = strlen(pp);
	    if (ll) { // svMisc calls this with ""
		strcpy (buf, pp);
		/* remove trailing file separator(s) */
		while ( *(p = buf + ll - 1) == fsp  && p > buf) *p = '\0';
		p = Rf_strrchr(buf, fsp);
		if(p == NULL)
		    strcpy(buf, ".");
		else {
		    while(p > buf && *p == fsp) --p;
		    p[1] = '\0';
		}
	    } else buf[0] = '\0';
	    SET_STRING_ELT(ans, i, mkChar(buf));
	}
    }
    UNPROTECT(1);
    return(ans);
}
#endif


#ifndef Win32 /* Windows version is in src/gnuwin32/extra.c */
#ifndef HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif

SEXP attribute_hidden do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, paths = CAR(args);
    int i, n = LENGTH(paths);
    const char *path;
    char abspath[PATH_MAX+1];

    checkArity(op, args);
    if (!isString(paths))
	error(_("'path' must be a character vector"));

    int mustWork = asLogical(CADDR(args)); /* 1, NA_LOGICAL or 0 */

/* Does any platform not have this? */
#ifdef HAVE_REALPATH
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = translateChar(STRING_ELT(paths, i));
	char *res = realpath(path, abspath);
	if (res)
	    SET_STRING_ELT(ans, i, mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#else
    Rboolean OK;
    warning("this platform does not have realpath so the results may not be canonical");
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = translateChar(STRING_ELT(paths, i));
	OK = strlen(path) <= PATH_MAX;
	if (OK) {
	    if (path[0] == '/') strncpy(abspath, path, PATH_MAX);
	    else {
		OK = getcwd(abspath, PATH_MAX) != NULL;
		OK = OK && (strlen(path) + strlen(abspath) + 1 <= PATH_MAX);
		if (OK) {strcat(abspath, "/"); strcat(abspath, path);}
	    }
	}
	/* we need to check that this exists */
	if (OK) OK = (access(abspath, 0 /* F_OK */) == 0);
	if (OK) SET_STRING_ELT(ans, i, mkChar(abspath));
	else {
	    SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
	    /* and report the problem */
	    if (mustWork == 1)
		error("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	    else if (mustWork == NA_LOGICAL)
		warning("path[%d]=\"%s\": %s", i+1, path, strerror(errno));
	}
    }
#endif
    UNPROTECT(1);
    return ans;
}

#ifdef USE_INTERNAL_MKTIME
const char *getTZinfo(void)
{
    const char *p = getenv("TZ");
    if(p) return p;
#ifdef HAVE_REALPATH
    // This works on Linux, OS X and *BSD: other known OSes set TZ.
    static char abspath[PATH_MAX+1] = "";
    if(abspath[0]) return abspath + 20;
    if(realpath("/etc/localtime", abspath))
	return abspath + 20; // strip prefix of /usr/share/zoneinfo/
#endif
    warning("system timezone name is unknown: set environment variable TZ");
    return "unknown";
}
#endif

#endif // not Win32


/* encodeString(x, w, quote, justify) */
SEXP attribute_hidden do_encodeString(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, s;
    R_xlen_t i, len;
    int w, quote = 0, justify, na;
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

    len = XLENGTH(x);
    if(findWidth && justify < 3) {
	w  = 0;
	for(i = 0; i < len; i++) {
	    s = STRING_ELT(x, i);
	    if(na || s != NA_STRING)
		w = R_imax2(w, Rstrlen(s, quote));
	}
	if(quote) w +=2; /* for surrounding quotes */
    }
    PROTECT(ans = duplicate(x));
    for(i = 0; i < len; i++) {
	s = STRING_ELT(x, i);
	if(na || s != NA_STRING) {
	    cetype_t ienc = getCharCE(s);
	    if(ienc == CE_UTF8) {
		const char *ss = EncodeString(s, w-1000000, quote,
					      (Rprt_adj) justify);
		SET_STRING_ELT(ans, i, mkCharCE(ss, ienc));
	    } else {
		const char *ss = EncodeString(s, w, quote, (Rprt_adj) justify);
		SET_STRING_ELT(ans, i, mkChar(ss));
	    }
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_encoding(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    R_xlen_t i, n;
    char *tmp;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    n = XLENGTH(x);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	if(IS_BYTES(STRING_ELT(x, i))) tmp = "bytes";
	else if(IS_LATIN1(STRING_ELT(x, i))) tmp = "latin1";
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
    int m;
    R_xlen_t i, n;
    const char *this;

    checkArity(op, args);
    if (TYPEOF(x = CAR(args)) != STRSXP)
	error(_("a character vector argument expected"));
    if (TYPEOF(enc = CADR(args)) != STRSXP)
	error(_("a character vector 'value' expected"));
    m = LENGTH(enc);
    if(m == 0)
	error(_("'value' must be of positive length"));
    if(MAYBE_REFERENCED(x)) x = duplicate(x);
    PROTECT(x);
    n = XLENGTH(x);
    for(i = 0; i < n; i++) {
	cetype_t ienc = CE_NATIVE;
	this = CHAR(STRING_ELT(enc, i % m)); /* ASCII */
	if(streql(this, "latin1")) ienc = CE_LATIN1;
	else if(streql(this, "UTF-8")) ienc = CE_UTF8;
	else if(streql(this, "bytes")) ienc = CE_BYTES;
	tmp = STRING_ELT(x, i);
	if(tmp == NA_STRING) continue;
	if (! ((ienc == CE_LATIN1 && IS_LATIN1(tmp)) ||
	       (ienc == CE_UTF8 && IS_UTF8(tmp)) ||
	       (ienc == CE_BYTES && IS_BYTES(tmp)) ||
	       (ienc == CE_NATIVE && ! IS_LATIN1(tmp) && ! IS_UTF8(tmp))))
	    SET_STRING_ELT(x, i, mkCharLenCE(CHAR(tmp), LENGTH(tmp), ienc));
    }
    UNPROTECT(1);
    return x;
}

SEXP attribute_hidden markKnown(const char *s, SEXP ref)
{
    int ienc = 0;
    if(ENC_KNOWN(ref)) {
	if(known_to_be_latin1) ienc = CE_LATIN1;
	if(known_to_be_utf8) ienc = CE_UTF8;
    }
    return mkCharCE(s, ienc);
}

Rboolean strIsASCII(const char *str)
{
    const char *p;
    for(p = str; *p; p++)
	if((unsigned int)*p > 0x7F) return FALSE;
    return TRUE;
}

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

/* These return the result in wchar_t, but does assume
   wchar_t is UCS-2/4 and so are for internal use only */
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
	if(strlen(s) < 2) return (size_t)-2;
	if ((s[1] & 0xC0) == 0x80) {
	    *w = (wchar_t) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
	    return 2;
	} else return (size_t)-1;
    } else if (byte < 0xF0) {
	if(strlen(s) < 3) return (size_t)-2;
	if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
	    *w = (wchar_t) (((byte & 0x0F) << 12)
			    | (unsigned int) ((s[1] & 0x3F) << 6)
			    | (s[2] & 0x3F));
	    byte = (unsigned int) *w;
	    /* Surrogates range */
	    if(byte >= 0xD800 && byte <= 0xDFFF) return (size_t)-1;
	    if(byte == 0xFFFE || byte == 0xFFFF) return (size_t)-1;
	    return 3;
	} else return (size_t)-1;
    }
    if(sizeof(wchar_t) < 4) return (size_t)-2;
    /* So now handle 4,5.6 byte sequences with no testing */
    if (byte < 0xf8) {
	if(strlen(s) < 4) return (size_t)-2;
	*w = (wchar_t) (((byte & 0x0F) << 18)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 6)
			| (s[3] & 0x3F));
	return 4;
    } else if (byte < 0xFC) {
	if(strlen(s) < 5) return (size_t)-2;
	*w = (wchar_t) (((byte & 0x0F) << 24)
			| (unsigned int) ((s[1] & 0x3F) << 12)
			| (unsigned int) ((s[2] & 0x3F) << 12)
			| (unsigned int) ((s[3] & 0x3F) << 6)
			| (s[4] & 0x3F));
	return 5;
    } else {
	if(strlen(s) < 6) return (size_t)-2;
	*w = (wchar_t) (((byte & 0x0F) << 30)
			| (unsigned int) ((s[1] & 0x3F) << 24)
			| (unsigned int) ((s[2] & 0x3F) << 18)
			| (unsigned int) ((s[3] & 0x3F) << 12)
			| (unsigned int) ((s[4] & 0x3F) << 6)
			| (s[5] & 0x3F));
	return 6;
    }
}

size_t
utf8towcs(wchar_t *wc, const char *s, size_t n)
{
    ssize_t m, res = 0;
    const char *t;
    wchar_t *p;
    wchar_t local;

    if(wc)
	for(p = wc, t = s; ; p++, t += m) {
	    m  = (ssize_t) utf8toucs(p, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	    res ++;
	    if (res >= n) break;
	}
    else
	for(t = s; ; res++, t += m) {
	    m  = (ssize_t) utf8toucs(&local, t);
	    if (m < 0) error(_("invalid input '%s' in 'utf8towcs'"), s);
	    if (m == 0) break;
	}
    return (size_t) res;
}

/* based on pcre.c */
static const unsigned int utf8_table1[] =
  { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const unsigned int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

static size_t Rwcrtomb(char *s, const wchar_t wc)
{
    register size_t i, j;
    unsigned int cvalue = (unsigned int) wc;
    char buf[10], *b;

    b = s ? s : buf;
    if(cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
	if (cvalue <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
	*b-- = (char) (0x80 | (cvalue & 0x3f));
	cvalue >>= 6;
    }
    *b = (char) (utf8_table2[i] | cvalue);
    return i + 1;
}

attribute_hidden // but used in windlgs
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n)
{
    ssize_t m, res=0;
    char *t;
    const wchar_t *p;
    if(s) {
	for(p = wc, t = s; ; p++) {
	    m  = (ssize_t) Rwcrtomb(t, *p);
	    if(m <= 0) break;
	    res += m;
	    if(res >= n) break;
	    t += m;
	}
    } else {
	for(p = wc; ; p++) {
	    m  = (ssize_t) Rwcrtomb(NULL, *p);
	    if(m <= 0) break;
	    res += m;
	}
    }
    return (size_t) res;
}


/* A version that reports failure as an error */
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps)
{
    size_t used;

    if(n <= 0 || !*s) return (size_t)0;
    used = mbrtowc(wc, s, n, ps);
    if((int) used < 0) {
	/* This gets called from the menu setup in RGui */
	if (!R_Is_Running) return (size_t)-1;
	/* let's try to print out a readable version */
	R_CheckStack2(4*strlen(s) + 10);
	char err[4*strlen(s) + 1], *q;
	const char *p;
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

attribute_hidden
Rboolean mbcsValid(const char *str)
{
    return  ((int)mbstowcs(NULL, str, 0) >= 0);
}


/* used in src/library/grDevices/src/cairo/cairoFns.c */
#include "valid_utf8.h"
Rboolean utf8Valid(const char *str)
{
    return valid_utf8(str, strlen(str)) == 0;
}


/* MBCS-aware versions of common comparisons.  Only used for ASCII c */
char *Rf_strchr(const char *s, int c)
{
    char *p = (char *)s;
    mbstate_t mb_st;
    size_t used;

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
    size_t used;

    if(!mbcslocale || utf8locale) return strrchr(s, c);
    mbs_init(&mb_st);
    while( (used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) ) {
	if(*p == c) plast = p;
	p += used;
    }
    return plast;
}

#ifdef Win32
void R_fixslash(char *s)
{
    char *p = s;

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '\\') *p = '/';
	    p += used;
	}
    } else
	for (; *p; p++) if (*p == '\\') *p = '/';
	/* preserve network shares */
	if(s[0] == '/' && s[1] == '/') s[0] = s[1] = '\\';
}

void R_UTF8fixslash(char *s)
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

    if(mbcslocale) {
	mbstate_t mb_st; int used;
	mbs_init(&mb_st);
	while((used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st))) {
	    if(*p == '/') *p = '\\';
	    p += used;
	}
    } else
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
    strncpy(buf, msg, (size_t) nc);
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
    strncpy(buf, msg, (size_t) nc);
    buf[nc] = '\0';
    warning("%s", buf);
}

void F77_SYMBOL(rchkusr)(void)
{
    R_CheckUserInterrupt();
}

/* Return a copy of a string using memory from R_alloc.
   NB: caller has to manage R_alloc stack.  Used in platform.c
*/
char *acopy_string(const char *in)
{
    char *out;
    size_t len = strlen(in);
    if (len > 0) {
	out = (char *) R_alloc(1 + len, sizeof(char));
	strcpy(out, in);
    } else
	out = "";
    return out;
}




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

void *Rf_AdobeSymbol2utf8(char *work, const char *c0, size_t nwork)
{
    const unsigned char *c = (unsigned char *) c0;
    unsigned char *t = (unsigned char *) work;
    while (*c) {
	if (*c < 32) *t++ = ' ';
	else {
	    unsigned int u = (unsigned int) s2u[*c - 32];
	    if (u < 128) *t++ = (unsigned char) u;
	    else if (u < 0x800) {
		*t++ = (unsigned char) (0xc0 | (u >> 6));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
	    } else {
		*t++ = (unsigned char) (0xe0 | (u >> 12));
		*t++ = (unsigned char) (0x80 | ((u >> 6) & 0x3f));
		*t++ = (unsigned char) (0x80 | (u & 0x3f));
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

double R_strtod5(const char *str, char **endptr, char dec,
		 Rboolean NA, int exact)
{
    LDOUBLE ans = 0.0, p10 = 10.0, fac = 1.0;
    int n, expn = 0, sign = 1, ndigits = 0, exph = -1;
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
    /* C99 specifies this: must come first to avoid 'inf' match */
    } else if (strncasecmp(p, "infinity", 8) == 0) {
	ans = R_PosInf;
	p += 8;
	goto done;
    } else if (strncasecmp(p, "Inf", 3) == 0) {
	ans = R_PosInf;
	p += 3;
	goto done;
    }

    if(strlen(p) > 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
	/* This will overflow to Inf if appropriate */
	for(p += 2; p; p++) {
	    if('0' <= *p && *p <= '9') ans = 16*ans + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ans = 16*ans + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ans = 16*ans + (*p -'A' + 10);
	    else if(*p == dec) {exph = 0; continue;}
	    else break;
	    if (exph >= 0) exph += 4;
	}
#define strtod_EXACT_CLAUSE						\
	if(exact && ans > 0x1.fffffffffffffp52) {			\
	    if(exact == NA_LOGICAL)					\
		warning(_(						\
		"accuracy loss in conversion from \"%s\" to numeric"),	\
			str);						\
	    else {							\
		ans = NA_REAL;						\
		p = str; /* back out */					\
		goto done;						\
	    }								\
	}
	strtod_EXACT_CLAUSE;
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
	    if(exph > 0) expn -= exph;
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
    if (ndigits == 0) {
	ans = NA_REAL;
	p = str; /* back out */
	goto done;
    }
    strtod_EXACT_CLAUSE;

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

    /* explicit overflow to infinity */
    if (ans > DBL_MAX) {
	if (endptr) *endptr = (char *) p;
	return (sign > 0) ? R_PosInf : R_NegInf;
    }

done:
    if (endptr) *endptr = (char *) p;
    return sign * (double) ans;
}


double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA)
{
    return R_strtod5(str, endptr, dec, NA, FALSE);
}

double R_strtod(const char *str, char **endptr)
{
    return R_strtod5(str, endptr, '.', FALSE, FALSE);
}

double R_atof(const char *str)
{
    return R_strtod5(str, NULL, '.', FALSE, FALSE);
}

/* enc2native and enc2utf8, but they are the same in a UTF-8 locale */
/* primitive */
SEXP attribute_hidden do_enc2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, el;
    R_xlen_t i;
    Rboolean duped = FALSE;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (!isString(CAR(args)))
	errorcall(call, "argumemt is not a character vector");
    ans = CAR(args);
    for (i = 0; i < XLENGTH(ans); i++) {
	el = STRING_ELT(ans, i);
	if (el == NA_STRING) continue;
	if (PRIMVAL(op) || known_to_be_utf8) { /* enc2utf8 */
	    if (IS_UTF8(el) || IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (!duped) { ans = PROTECT(duplicate(ans)); duped = TRUE; }
	    SET_STRING_ELT(ans, i, 
			   mkCharCE(translateCharUTF8(el), CE_UTF8));
	} else if (ENC_KNOWN(el)) { /* enc2native */
	    if (IS_ASCII(el) || IS_BYTES(el)) continue;
	    if (known_to_be_latin1 && IS_LATIN1(el)) continue;
	    if (!duped) { PROTECT(ans = duplicate(ans)); duped = TRUE; }
	    if (known_to_be_latin1)
		SET_STRING_ELT(ans, i, mkCharCE(translateChar(el), CE_LATIN1));
	    else
		SET_STRING_ELT(ans, i, mkChar(translateChar(el)));
	}
    }
    if(duped) UNPROTECT(1);
    return ans;
}

#ifdef USE_ICU
# include <locale.h>
#ifdef USE_ICU_APPLE
/* Mac OS X is missing the headers */
typedef int UErrorCode; /* really an enum these days */
struct UCollator;
typedef struct UCollator UCollator;

typedef enum {
  UCOL_EQUAL    = 0,
  UCOL_GREATER    = 1,
  UCOL_LESS    = -1
} UCollationResult ;

typedef enum {
  UCOL_DEFAULT = -1,
  UCOL_PRIMARY = 0,
  UCOL_SECONDARY = 1,
  UCOL_TERTIARY = 2,
  UCOL_DEFAULT_STRENGTH = UCOL_TERTIARY,
  UCOL_CE_STRENGTH_LIMIT,
  UCOL_QUATERNARY=3,
  UCOL_IDENTICAL=15,
  UCOL_STRENGTH_LIMIT,
  UCOL_OFF = 16,
  UCOL_ON = 17,
  UCOL_SHIFTED = 20,
  UCOL_NON_IGNORABLE = 21,
  UCOL_LOWER_FIRST = 24,
  UCOL_UPPER_FIRST = 25,
  UCOL_ATTRIBUTE_VALUE_COUNT
} UColAttributeValue;

typedef UColAttributeValue UCollationStrength;

typedef enum {
      UCOL_FRENCH_COLLATION,
      UCOL_ALTERNATE_HANDLING,
      UCOL_CASE_FIRST,
      UCOL_CASE_LEVEL,
      UCOL_NORMALIZATION_MODE,
      UCOL_DECOMPOSITION_MODE = UCOL_NORMALIZATION_MODE,
      UCOL_STRENGTH,
      UCOL_HIRAGANA_QUATERNARY_MODE,
      UCOL_NUMERIC_COLLATION,
      UCOL_ATTRIBUTE_COUNT
} UColAttribute;

/* UCharIterator struct has to be defined since we use its instances as
   local variables, but we don't actually use any of its members. */
typedef struct UCharIterator {
  const void *context;
  int32_t length, start, index, limit, reservedField;
  void *fns[16]; /* we overshoot here (there is just 10 fns in ICU 3.6),
		    but we have to make sure that enough stack space
		    is allocated when used as a local var in future
		    versions */
} UCharIterator;

UCollator* ucol_open(const char *loc, UErrorCode *status);
void ucol_close(UCollator *coll);
void ucol_setAttribute(UCollator *coll, UColAttribute attr,
		       UColAttributeValue value, UErrorCode *status);
void ucol_setStrength(UCollator *coll, UCollationStrength strength);
UCollationResult ucol_strcollIter(const UCollator *coll,
				  UCharIterator *sIter,
				  UCharIterator *tIter,
				  UErrorCode *status);
void uiter_setUTF8(UCharIterator *iter, const char *s, int32_t length);

void uloc_setDefault(const char* localeID, UErrorCode* status);

typedef enum {
    ULOC_ACTUAL_LOCALE = 0,
    ULOC_VALID_LOCALE = 1,
    ULOC_DATA_LOCALE_TYPE_LIMIT = 3
} ULocDataLocaleType ;


const char* ucol_getLocaleByType(const UCollator *coll,
				 ULocDataLocaleType type,
				 UErrorCode *status);

#define U_ZERO_ERROR 0
#define U_FAILURE(x) ((x)>U_ZERO_ERROR)
#define ULOC_ACTUAL_LOCALE 0

#else
#include <unicode/utypes.h>
#include <unicode/ucol.h>
#include <unicode/uloc.h>
#include <unicode/uiter.h>
#endif

static UCollator *collator = NULL;

static Rboolean collationLocaleSet = FALSE;

/* called from platform.c */
void attribute_hidden resetICUcollator(void)
{
    if (collator) ucol_close(collator);
    collator = NULL;
    collationLocaleSet = FALSE;
}

static const struct {
    const char * const str;
    int val;
} ATtable[] = {
    { "case_first", UCOL_CASE_FIRST },
    { "upper", UCOL_UPPER_FIRST },
    { "lower", UCOL_LOWER_FIRST },
    { "default ", UCOL_DEFAULT },
    { "strength", 999 },
    { "primary ", UCOL_PRIMARY },
    { "secondary ", UCOL_SECONDARY },
    { "teritary ", UCOL_TERTIARY },
    { "guaternary ", UCOL_QUATERNARY },
    { "identical ", UCOL_IDENTICAL },
    { "french_collation", UCOL_FRENCH_COLLATION },
    { "on", UCOL_ON },
    { "off", UCOL_OFF },
    { "normalization", UCOL_NORMALIZATION_MODE },
    { "alternate_handling", UCOL_ALTERNATE_HANDLING },
    { "non_ignorable", UCOL_NON_IGNORABLE },
    { "shifted", UCOL_SHIFTED },
    { "case_level", UCOL_CASE_LEVEL },
    { "hiragana_quaternary", UCOL_HIRAGANA_QUATERNARY_MODE },
    { NULL,  0 }
};

#ifdef Win32
#define BUFFER_SIZE 512
typedef int (WINAPI *PGSDLN)(LPWSTR, int);

static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    if (p && p[0]) return p;

    // This call is >= Vista/Server 2008
    // ICU should accept almost all of these, e.g. en-US and uz-Latn-UZ
    PGSDLN pGSDLN = (PGSDLN)
	GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
		       "GetSystemDefaultLocaleName");
    if(pGSDLN) {
	WCHAR wcBuffer[BUFFER_SIZE];
	pGSDLN(wcBuffer, BUFFER_SIZE);
	static char locale[BUFFER_SIZE];
	WideCharToMultiByte(CP_ACP, 0, wcBuffer, -1,
			    locale, BUFFER_SIZE, NULL, NULL);
	return locale;
    } else return "root";
}
#else
static const char *getLocale(void)
{
    const char *p = getenv("R_ICU_LOCALE");
    return (p && p[0]) ? p : setlocale(LC_COLLATE, NULL);
}
#endif

SEXP attribute_hidden do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    UErrorCode  status = U_ZERO_ERROR;

    for (; args != R_NilValue; args = CDR(args)) {
	if (isNull(TAG(args))) error(_("all arguments must be named"));
	const char *this = CHAR(PRINTNAME(TAG(args)));
	const char *s;

	x = CAR(args);
	if (!isString(x) || LENGTH(x) != 1)
	    error(_("invalid '%s' argument"), this);
	s = CHAR(STRING_ELT(x, 0));
	if (streql(this, "locale")) {
	    if (collator) {
		ucol_close(collator);
		collator = NULL;
	    }
	    if(strcmp(s, "none")) {
		if(streql(s, "default")) 
		    uloc_setDefault(getLocale(), &status);
		else uloc_setDefault(s, &status);
		if(U_FAILURE(status))
		    error("failed to set ICU locale %s (%d)", s, status);
		collator = ucol_open(NULL, &status);
		if (U_FAILURE(status)) {
		    collator = NULL;
		    error("failed to open ICU collator (%d)", status);
		}
	    }
	    collationLocaleSet = TRUE;
	} else {
	    int i, at = -1, val = -1;
	    for (i = 0; ATtable[i].str; i++)
		if (streql(this, ATtable[i].str)) {
		    at = ATtable[i].val;
		    break;
		}
	    for (i = 0; ATtable[i].str; i++)
		if (streql(s, ATtable[i].str)) {
		    val = ATtable[i].val;
		    break;
		}
	    if (collator && at == 999 && val >= 0) {
		ucol_setStrength(collator, val);
	    } else if (collator && at >= 0 && val >= 0) {
		ucol_setAttribute(collator, at, val, &status);
		if (U_FAILURE(status))
		    error("failed to set ICU collator attribute");
	    }
	}
    }

    return R_NilValue;
}

SEXP attribute_hidden do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *ans = "unknown", *res;
    checkArity(op, args);

    if(collator) {
	UErrorCode  status = U_ZERO_ERROR;
	int type = asInteger(CAR(args));
	if (type < 1 || type > 2)
	    error(_("invalid '%s' value"), "type");
	
	res = ucol_getLocaleByType(collator, 
				   type == 1 ? ULOC_ACTUAL_LOCALE : ULOC_VALID_LOCALE, 
				   &status);
	if(!U_FAILURE(status) && res) ans = res;
    } else ans = "ICU not in use";
    return mkString(ans);
}

/* Caller has to manage the R_alloc stack */
/* NB: strings can have equal collation weight without being identical */
attribute_hidden
int Scollate(SEXP a, SEXP b)
{
    if (!collationLocaleSet) {
	collationLocaleSet = TRUE;
#ifndef Win32
	if (strcmp("C", getLocale()) ) {
#else
	const char *p = getenv("R_ICU_LOCALE");
        if(p && p[0]) {
#endif
	    UErrorCode status = U_ZERO_ERROR;
	    uloc_setDefault(getLocale(), &status);
	    if(U_FAILURE(status))
		error("failed to set ICU locale (%d)", status);
	    collator = ucol_open(NULL, &status);
	    if (U_FAILURE(status)) {
		collator = NULL;
		error("failed to open ICU collator (%d)", status);
	    }
	}
    }
    if (collator == NULL)
	return strcoll(translateChar(a), translateChar(b));

    UCharIterator aIter, bIter;
    const char *as = translateCharUTF8(a), *bs = translateCharUTF8(b);
    int len1 = (int) strlen(as), len2 = (int) strlen(bs);
    uiter_setUTF8(&aIter, as, len1);
    uiter_setUTF8(&bIter, bs, len2);
    UErrorCode status = U_ZERO_ERROR;
    int result = ucol_strcollIter(collator, &aIter, &bIter, &status);
    if (U_FAILURE(status)) error("could not collate using ICU");
    return result;
}

#else /* not USE_ICU */

SEXP attribute_hidden do_ICUset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning(_("ICU is not supported on this build"));
    return R_NilValue;
}

SEXP attribute_hidden do_ICUget(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString("ICU not in use");
}

void attribute_hidden resetICUcollator(void) {}

# ifdef Win32

static int Rstrcoll(const char *s1, const char *s2)
{
    R_CheckStack2(sizeof(wchar_t) * (2 + strlen(s1) + strlen(s2)));
    wchar_t w1[strlen(s1)+1], w2[strlen(s2)+1];
    utf8towcs(w1, s1, strlen(s1));
    utf8towcs(w2, s2, strlen(s2));
    return wcscoll(w1, w2);
}

int Scollate(SEXP a, SEXP b)
{
    if(getCharCE(a) == CE_UTF8 || getCharCE(b) == CE_UTF8)
	return Rstrcoll(translateCharUTF8(a), translateCharUTF8(b));
    else
	return strcoll(translateChar(a), translateChar(b));
}

# else
attribute_hidden
int Scollate(SEXP a, SEXP b)
{
    return strcoll(translateChar(a), translateChar(b));
}

# endif
#endif

#include <lzma.h>

SEXP attribute_hidden do_crc64(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP in = CAR(args);
    uint64_t crc = 0;
    char ans[17];
    if (!isString(in)) error("input must be a character string");
    const char *str = CHAR(STRING_ELT(in, 0));

    /* Seems this is really 64-bit only on 64-bit platforms */
    crc = lzma_crc64((uint8_t *)str, strlen(str), crc);
    snprintf(ans, 17, "%lx", (long unsigned int) crc);
    return mkString(ans);
}

static void
bincode(double *x, R_xlen_t n, double *breaks, int nb,
	int *code, int right, int include_border)
{
    int lo, hi, nb1 = nb - 1, new;
    int lft = !right;

    /* This relies on breaks being sorted, so wise to check that */
    for(int i = 1; i < nb; i++)
	if(breaks[i-1] > breaks[i]) error(_("'breaks' is not sorted"));

    for(R_xlen_t i = 0; i < n; i++) {
	code[i] = NA_INTEGER;
	if(!ISNAN(x[i])) {
	    lo = 0;
	    hi = nb1;
	    if(x[i] <  breaks[lo] || breaks[hi] < x[i] ||
	       (x[i] == breaks[lft ? hi : lo] && ! include_border)) ;
	    else {
		while(hi - lo >= 2) {
		    new = (hi + lo)/2;
		    if(x[i] > breaks[new] || (lft && x[i] == breaks[new]))
			lo = new;
		    else
			hi = new;
		}
		code[i] = lo + 1;
	    }
	}
    }
}

/* 'breaks' cannot be a long vector as the return codes are integer. */
SEXP attribute_hidden do_bincode(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x, breaks, right, lowest;
    x = CAR(args); args = CDR(args);
    breaks = CAR(args); args = CDR(args);
    right = CAR(args); args = CDR(args);
    lowest = CAR(args);
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(breaks))
	error(_("long vector '%s' is not supported"), "breaks");
#endif
    PROTECT(x = coerceVector(x, REALSXP));
    PROTECT(breaks = coerceVector(breaks, REALSXP));
    R_xlen_t n = XLENGTH(x);
    int nB = LENGTH(breaks), sr = asLogical(right), sl = asLogical(lowest);
    if (nB == NA_INTEGER) error(_("invalid '%s' argument"), "breaks");
    if (sr == NA_INTEGER) error(_("invalid '%s' argument"), "right");
    if (sl == NA_INTEGER) error(_("invalid '%s' argument"), "include.lowest");
    SEXP codes;
    PROTECT(codes = allocVector(INTSXP, n));
    bincode(REAL(x), n, REAL(breaks), nB, INTEGER(codes), sr, sl);
    UNPROTECT(3);
    return codes;
}

SEXP attribute_hidden do_tabulate(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP in = CAR(args), nbin = CADR(args);
    if (TYPEOF(in) != INTSXP)  error("invalid input");
    R_xlen_t n = XLENGTH(in);
    /* FIXME: could in principle be a long vector */
    int nb = asInteger(nbin);
    if (nb == NA_INTEGER || nb < 0)
	error(_("invalid '%s' argument"), "nbin");
    SEXP ans = allocVector(INTSXP, nb);
    int *x = INTEGER(in), *y = INTEGER(ans);
    memset(y, 0, nb * sizeof(int));
    for(R_xlen_t i = 0 ; i < n ; i++)
	if (x[i] != NA_INTEGER && x[i] > 0 && x[i] <= nb) y[x[i] - 1]++;
    return ans;
}

/* x can be a long vector but xt cannot since the result is integer */
SEXP attribute_hidden do_findinterval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP xt, x, right, inside;
    xt = CAR(args); args = CDR(args);
    x = CAR(args); args = CDR(args);
    right = CAR(args); args = CDR(args);
    inside = CAR(args);
    if(TYPEOF(xt) != REALSXP || TYPEOF(x) != REALSXP) error("invalid input");
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(xt))
	error(_("long vector '%s' is not supported"), "vec");
#endif
    int n = LENGTH(xt);
    if (n == NA_INTEGER) error(_("invalid '%s' argument"), "vec");
    R_xlen_t nx = XLENGTH(x);
    int sr = asLogical(right), si = asLogical(inside);
    if (sr == NA_INTEGER)
	error(_("invalid '%s' argument"), "rightmost.closed");
    if (si == NA_INTEGER)
	error(_("invalid '%s' argument"), "all.inside");
    SEXP ans = allocVector(INTSXP, nx);
    double *rxt = REAL(xt), *rx = REAL(x);
    int ii = 1;
    for(int i = 0; i < nx; i++) {
	if (ISNAN(REAL(x)[i])) ii = NA_INTEGER;
	else {
	    int mfl = si;
	    ii = findInterval(rxt, n, rx[i], sr, si, ii, &mfl);
	}
	INTEGER(ans)[i] = ii;
    }
    return ans;
}

#ifdef Win32
// this includes RS.h
# undef ERROR
#endif
#include <R_ext/Applic.h>
SEXP attribute_hidden do_pretty(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ans, nm, hi;
    double l = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(l)) error(_("invalid '%s' argument"), "l");
    double u = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(u)) error(_("invalid '%s' argument"), "u");
    int n = asInteger(CAR(args)); args = CDR(args);
    if (n == NA_INTEGER || n < 0) error(_("invalid '%s' argument"), "n");
    int min_n = asInteger(CAR(args)); args = CDR(args);
    if (min_n == NA_INTEGER || min_n < 0 || min_n > n)
	error(_("invalid '%s' argument"), "min.n");
    double shrink = asReal(CAR(args)); args = CDR(args);
    if (!R_FINITE(shrink) || shrink <= 0.)
	error(_("invalid '%s' argument"), "shrink.sml");
    PROTECT(hi = coerceVector(CAR(args), REALSXP)); args = CDR(args);
    double z;
    if (!R_FINITE(z = REAL(hi)[0]) || z < 0.)
	error(_("invalid '%s' argument"), "high.u.bias");
    if (!R_FINITE(z = REAL(hi)[1]) || z < 0.)
	error(_("invalid '%s' argument"), "u5.bias");
    int eps = asInteger(CAR(args)); /* eps.correct */
    if (eps == NA_INTEGER || eps < 0 || eps > 2)
	error(_("'eps.correct' must be 0, 1, or 2"));
    R_pretty(&l, &u, &n, min_n, shrink, REAL(hi), eps, 1);
    PROTECT(ans = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(ans, 0, ScalarReal(l));
    SET_VECTOR_ELT(ans, 1, ScalarReal(u));
    SET_VECTOR_ELT(ans, 2, ScalarInteger(n));
    nm = allocVector(STRSXP, 3);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("l"));
    SET_STRING_ELT(nm, 1, mkChar("u"));
    SET_STRING_ELT(nm, 2, mkChar("n"));
    UNPROTECT(2);
    return ans;
}

/*
    r <- .Internal(formatC(x, as.character(mode), width, digits,
                   as.character(format), as.character(flag), i.strlen))
*/

static void
str_signif(void *x, R_xlen_t n, const char *type, int width, int digits,
	   const char *format, const char *flag, char **result);

SEXP attribute_hidden do_formatC(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args); args = CDR(args);
    if (!isVector(x)) error(_("'x' must be a vector"));
    R_xlen_t n = XLENGTH(x);
    const char *type = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    int width = asInteger(CAR(args)); args = CDR(args);
    int digits = asInteger(CAR(args)); args = CDR(args);
    const char *fmt = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    const char *flag = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    SEXP i_strlen = PROTECT(coerceVector(CAR(args), INTSXP));
    char **cptr = (char **) R_alloc(n, sizeof(char*));
    for (R_xlen_t i = 0; i < n; i++) {
	int ix = INTEGER(i_strlen)[i] + 2;
	cptr[i] = (char *) R_alloc(ix + 1, sizeof(char));
	memset(cptr[i], ' ', ix);
	cptr[i][ix] = 0;
    }
    void *px = NULL /* -Wall */;
    switch(TYPEOF(x)) {
    case INTSXP: px = INTEGER(x); break;
    case REALSXP: px = REAL(x); break;
    default: error("unsupported type ");
    }
    str_signif(px, n, type, width, digits, fmt, flag, cptr);
    SEXP ans = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; i++) SET_STRING_ELT(ans, i, mkChar(cptr[i]));
    UNPROTECT(2);
    return ans;
}

/* Former src/appl/strsignif.c
 *
 *  Copyright (C) Martin Maechler, 1994, 1998
 *  Copyright (C) 2001-2013 the R Core Team
 *
 *  I want you to preserve the copyright of the original author(s),
 *  and encourage you to send me any improvements by e-mail. (MM).
 *
 *  Originally from Bill Dunlap
 *  bill@stat.washington.edu
 *  Wed Feb 21, 1990
 *
 *  Much improved by Martin Maechler, including the "fg" format.
 *
 *  Patched by Friedrich.Leisch@ci.tuwien.ac.at
 *  Fri Nov 22, 1996
 *
 *  Some fixes by Ross Ihaka
 *  ihaka@stat.auckland.ac.nz
 *  Sat Dec 21, 1996
 *  Integer arguments changed from "long" to "int"
 *  Bus error due to non-writable strings fixed
 *
 *  BDR 2001-10-30 use R_alloc not Calloc as memory was not
 *  reclaimed on error (and there are many error exits).
 *
 *	type	"double" or "integer" (R - numeric 'mode').
 *
 *	width	The total field width; width < 0 means to left justify
 *		the number in this field (equivalent to flag = "-").
 *		It is possible that the result will be longer than this,
 *		but that should only happen in reasonable cases.
 *
 *	digits	The desired number of digits after the decimal point.
 *		digits < 0 uses the default for C, namely 6 digits.
 *
 *	format	"d" (for integers) or "f", "e","E", "g", "G" (for 'real')
 *		"f" gives numbers in the usual "xxx.xxx" format;
 *		"e" and "E" give n.ddde<nn> or n.dddE<nn> (scientific format);
 *		"g" and "G" puts them into scientific format if it saves
 *		space to do so.
 *	    NEW: "fg" gives numbers in "xxx.xxx" format as "f",
 *		  ~~  however, digits are *significant* digits and,
 *		      if digits > 0, no trailing zeros are produced, as in "g".
 *
 *	flag	Format modifier as in K&R "C", 2nd ed., p.243;
 *		e.g., "0" pads leading zeros; "-" does left adjustment
 *		the other possible flags are  "+", " ", and "#".
 *	  New (Feb.98): if flag has more than one character, all are passed..
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef Win32
/* avoid latest MinGW's redefinition in stdio.h */
int trio_sprintf(char *buffer, const char *format, ...);
#define sprintf trio_sprintf
#endif

#include <Rmath.h>		/* fround */

static
void str_signif(void *x, R_xlen_t n, const char *type, int width, int digits,
		const char *format, const char *flag, char **result)
{
    int dig = abs(digits);
    Rboolean rm_trailing_0 = digits >= 0;
    Rboolean do_fg = !strcmp("fg", format); /* TRUE  iff  format == "fg" */
    double xx;
    int iex;
    size_t j, len_flag = strlen(flag);
    const void *vmax = vmaxget();

    char *f0  =	 R_alloc((size_t) do_fg ? 1+1+len_flag+3 : 1, sizeof(char));
    char *form = R_alloc((size_t) 1+1+len_flag+3 + strlen(format),
			 sizeof(char));

    if (width == 0)
	error("width cannot be zero");

    if (strcmp("d", format) == 0) {
	if (len_flag == 0)
	    strcpy(form, "%*d");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*d");
	}
	if (strcmp("integer", type) == 0)
	    for (R_xlen_t i = 0; i < n; i++)
		snprintf(result[i], strlen(result[i]) + 1,
			 form, width, ((int *)x)[i]);
	else
	    error("'type' must be \"integer\" for  \"d\"-format");
    }
    else { /* --- floating point --- */
	if (len_flag == 0)
	    strcpy(form, "%*.*");
	else {
	    strcpy(form, "%");
	    strcat(form, flag);
	    strcat(form, "*.*");
	}

	if(do_fg) {
	    strcpy(f0, "%");
	    strcat(f0, flag);
	    strcat(f0, ".*f");
	    strcat(form, "g");
	}
	else
	    strcat(form, format);
#ifdef DEBUG
	fprintf(stderr, "strsignif.c: form='%s', width=%d, dig=%d\n",
		form, width, dig);
	if(do_fg) fprintf(stderr, "\t\"fg\": f0='%s'.", f0);
#endif
	if (strcmp("double", type) == 0) {
	    if(do_fg) /* do smart "f" : */
		for (R_xlen_t i = 0; i < n; i++) {
		    xx = ((double *)x)[i];
		    if(xx == 0.)
			strcpy(result[i], "0");
		    else {
			/* This was iex= (int)floor(log10(fabs(xx)))
			   That's wrong, as xx might get rounded up,
			   and we do need some fuzz or 99.5 is correct.
			*/
			double xxx = fabs(xx), X;
			iex = (int)floor(log10(xxx) + 1e-12);
			X = fround(xxx/Rexp10((double)iex) + 1e-12,
				   (double)(dig-1));
			if(iex > 0 &&  X >= 10) {
			    xx = X * Rexp10((double)iex);
			    iex++;
			}
			if(iex == -4 && fabs(xx)< 1e-4) {/* VERY rare case */
			    iex = -5;
			}
			if(iex < -4) {
				/* "g" would result in 'e-' representation:*/
			    snprintf(result[i], strlen(result[i]) + 1,
				     f0, dig-1 + -iex, xx);
#ifdef DEBUG
			    fprintf(stderr, " x[%d]=%g, iex=%d\n", i, xx, iex);
			    fprintf(stderr, "\tres. = '%s'; ", result[i]);
#endif
			    /* Remove trailing  "0"s __ IFF flag has no '#': */
			    if(rm_trailing_0) {
				j = strlen(result[i])-1;
#ifdef DEBUG
				int jL = j;
#endif
				while(result[i][j] == '0') j--;
				result[i][j+1] = '\0';
#ifdef DEBUG
				fprintf(stderr, "\t>>> jL=%d, j=%d; new res= '%s'\n",
					jL, j, result[i]);
#endif
			    }

			} else { /* iex >= -4:	NOT "e-" */
				/* if iex >= dig, would have "e+" representation */
#ifdef DEBUG
			    fprintf(stderr, "\t  iex >= -4; using %d for 'dig'\n",
				    (iex >= dig) ? (iex+1) : dig);
#endif
			    snprintf(result[i], strlen(result[i]) + 1,
				     form, width, (iex >= dig) ? (iex+1) : dig, xx);
			}
		    } /* xx != 0 */
		} /* if(do_fg) for(i..) */
	    else
		for (R_xlen_t i = 0; i < n; i++)
		    snprintf(result[i], strlen(result[i]) + 1,
			     form, width, dig, ((double *)x)[i]);
	} else
	    error("'type' must be \"real\" for this format");
    }
    vmaxset(vmax);
}
