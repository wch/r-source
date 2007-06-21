/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  The R Development Core Team
 *  Copyright (C) 2003	      The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 51 Franklin Street
 *  Fifth Floor, Boston, MA 02110-1301  USA.
 */

/* <UTF8-FIXME>
   Need to convert character strings to and from 8-bit.
   Check other uses.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

#include <string.h>
#include <stdlib.h>

#include <Rmath.h>

#include <R_ext/GraphicsEngine.h> /* needed for GEDevDesc in do_Externalgr */

#include <R_ext/RConverters.h>
#ifdef HAVE_ICONV
#include <R_ext/Riconv.h>
#endif

#ifndef max
#define max(a, b) ((a > b)?(a):(b))
#endif


/* These are set during each call to do_dotCode() below. */

static SEXP NaokSymbol = NULL;
static SEXP DupSymbol = NULL;
static SEXP PkgSymbol = NULL;
static SEXP EncSymbol = NULL;

/* Global variable that should go. Should actually be doing this in 
   a much more straightforward manner. */
#include <Rdynpriv.h>
enum {FILENAME, DLL_HANDLE, R_OBJECT, NOT_DEFINED};
typedef struct {
    char DLLname[PATH_MAX];
    HINSTANCE dll;
    SEXP  obj;
    int type;
} DllReference;

/* Maximum length of entry-point name, including nul terminator */
#define MaxSymbolBytes 1024

/* This looks up entry points in DLLs in a platform specific way. */
#define MAX_ARGS 65

static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol);

static SEXP naokfind(SEXP args, int * len, int *naok, int *dup,
		     DllReference *dll);
static SEXP pkgtrim(SEXP args, DllReference *dll);
static SEXP enctrim(SEXP args, char *name, int len);

/*
  Checks whether the specified object correctly identifies a native routine.
  This can be
   a) a string,
   b) an external pointer giving the address of the routine
      (e.g. getNativeSymbolInfo("foo")$address)
   c) or a NativeSymbolInfo itself  (e.g. getNativeSymbolInfo("foo"))

   NB: in the last two cases it sets fun as well!
 */
static void
checkValidSymbolId(SEXP op, SEXP call, DL_FUNC *fun,
		   R_RegisteredNativeSymbol *symbol, char *buf)
{
    if (isValidString(op)) return;

    *fun = NULL;
    if(TYPEOF(op) == EXTPTRSXP) {
	char *p = NULL;
	if(R_ExternalPtrTag(op) == Rf_install("native symbol")) 
   	   *fun = (DL_FUNC) R_ExternalPtrAddr(op);
	else if(R_ExternalPtrTag(op) == Rf_install("registered native symbol")) {
   	   R_RegisteredNativeSymbol *tmp;
	   tmp = (R_RegisteredNativeSymbol *) R_ExternalPtrAddr(op);
	   if(tmp) {
  	      if(symbol->type != R_ANY_SYM && symbol->type != tmp->type)
 	         errorcall(call, _("NULL value passed as symbol address"));
   	        /* Check the type of the symbol. */
   	      switch(symbol->type) {
	      case R_C_SYM:
  	          *fun = tmp->symbol.c->fun;
  	          p = tmp->symbol.c->name;
		  break;
	      case R_CALL_SYM:
  	          *fun = tmp->symbol.call->fun;
  	          p = tmp->symbol.call->name;
		  break;
	      case R_FORTRAN_SYM:
  	          *fun = tmp->symbol.fortran->fun;
  	          p = tmp->symbol.fortran->name;
		  break;
	      case R_EXTERNAL_SYM:
  	          *fun = tmp->symbol.external->fun;
  	          p = tmp->symbol.external->name;
		  break;
	      default:
  	         /* Something unintended has happened if we get here. */
	          errorcall(call, _("Unimplemented type %d in createRSymbolObject"), 
			    symbol->type);
  	          break;
	      }
	      *symbol = *tmp;
	   }
	}
	/* This is illegal C */
	if(*fun == NULL)
	    errorcall(call, _("NULL value passed as symbol address"));

        /* copy the symbol name. */
	if (p) {
	    if (strlen(p) >= MaxSymbolBytes)
		error(_("symbol '%s' is too long"), p);
	    memcpy(buf, p, strlen(p)+1);
	    /* Ouch, no length check
	    q = buf;
	    while ((*q = *p) != '\0') {
	        p++;
	        q++;
	    } */
	}

	return;
    } 
    else if(inherits(op, "NativeSymbolInfo")) {
	checkValidSymbolId(VECTOR_ELT(op, 1), call, fun, symbol, buf);
	return;
    }
    
    errorcall(call,
      _("'name' must be a string (of length 1) or native symbol reference"));
    return; /* not reached */
}


/*
  This is the routine that is called by do_dotCode, do_dotcall and
  do_External to find the DL_FUNC to invoke. It handles processing the
  arguments for the PACKAGE argument, if present, and also takes care
  of the cases where we are given a NativeSymbolInfo object, an
  address directly, and if the DLL is specified. If no PACKAGE is
  provided, we check whether the calling function is in a namespace
  and look there.
*/

static SEXP
resolveNativeRoutine(SEXP args, DL_FUNC *fun,
		     R_RegisteredNativeSymbol *symbol, char *buf,
		     int *nargs, int *naok, int *dup, SEXP call)
{
    SEXP op;
    const char *p; char *q;
    DllReference dll = {"", NULL, NULL, NOT_DEFINED};

    op = CAR(args);
    /* NB, this sets fun, symbol and buf and is not just a check! */
    checkValidSymbolId(op, call, fun, symbol, buf); 

    /* The following code modifies the argument list */
    /* We know this is ok because do_dotCode is entered */
    /* with its arguments evaluated. */

    strcpy(dll.DLLname, "");
    if(symbol->type == R_C_SYM || symbol->type == R_FORTRAN_SYM) {
	args = naokfind(CDR(args), nargs, naok, dup, &dll);

	if(*naok == NA_LOGICAL)
	    errorcall(call, _("invalid '%s' value"), "naok");
	if(*nargs > MAX_ARGS)
	    errorcall(call, _("too many arguments in foreign function call"));
    } else {
	if (PkgSymbol == NULL) PkgSymbol = install("PACKAGE");
	/* This has the side effect of setting dll.type if a PACKAGE=
	   argument if found */
	args = pkgtrim(args, &dll);
    }

    /* Make up the load symbol and look it up. */

    if(TYPEOF(op) == STRSXP) {
	p = translateChar(STRING_ELT(op, 0));
	if(strlen(p) >= MaxSymbolBytes)
	    error(_("symbol '%s' is too long"), p);
	q = buf;
	while ((*q = *p) != '\0') {
	    if(symbol->type == R_FORTRAN_SYM) *q = tolower(*q);
	    p++;
	    q++;
	}
    }
 
    if(!*fun) {
	if(dll.type != FILENAME) {
	    /* no PACKAGE= arg, so see if we can identify a DLL
	       from the namespace defining the function */
	    *fun = R_FindNativeSymbolFromDLL(buf, &dll, symbol);
	    /* need to continue if there is no PACKAGE arg or if the
	       namespace search failed
	       if(!fun)
	           errorcall(call, _("cannot resolve native routine"));
	    */
	}

	/* NB: the actual conversion to the symbol is done in
	   R_dlsym in Rdynload.c.  That prepends an underscore (usually),
	   and may append one or more underscores.
	*/

	if (!*fun && !(*fun = R_FindSymbol(buf, dll.DLLname, symbol))) {
	    if(strlen(dll.DLLname))
		errorcall(call,
			  _("%s symbol name \"%s\" not in DLL for package \"%s\""),
			  symbol->type == R_FORTRAN_SYM ? "Fortran" : "C", buf,
			  dll.DLLname);
	    else
		errorcall(call, _("%s symbol name \"%s\" not in load table"),
			  symbol->type == R_FORTRAN_SYM ? "Fortran" : "C", buf);
	}
    }

    return(args);
}



/* Convert an R object to a non-moveable C/Fortran object and return
   a pointer to it.  This leaves pointers for anything other
   than vectors and lists unaltered.
*/

static Rboolean
checkNativeType(int targetType, int actualType)
{
    if(targetType > 0) {
	if(targetType == INTSXP || targetType == LGLSXP) {
	    return(actualType == INTSXP || actualType == LGLSXP);
	}
	return(targetType == actualType);
    }

    return(TRUE);
}

static void *RObjToCPtr(SEXP s, int naok, int dup, int narg, int Fort,
			const char *name, R_toCConverter **converter,
			int targetType, char* encname)
{
    Rbyte *rawptr;
    int *iptr;
    float *sptr;
    double *rptr;
    char **cptr, *fptr;
    Rcomplex *zptr;
    SEXP *lptr, CSingSymbol=install("Csingle");
    int i, l, n;

    if(converter)
	*converter = NULL;

    if(length(getAttrib(s, R_ClassSymbol))) {
	R_CConvertInfo info;
	int success;
        void *ans;

	info.naok = naok;
	info.dup = dup;
	info.narg = narg;
	info.Fort = Fort;
        info.name = name;

	ans = Rf_convertToC(s, &info, &success, converter);
	if(success)
	    return(ans);
    }

    if(checkNativeType(targetType, TYPEOF(s)) == FALSE) {
	if(!dup) {
	    error(_("explicit request not to duplicate arguments in call to '%s', but argument %d is of the wrong type (%d != %d)"),
		  name, narg + 1, targetType, TYPEOF(s));
	}

	if(targetType != SINGLESXP)
	    s = coerceVector(s, targetType);
    }

    switch(TYPEOF(s)) {
    case RAWSXP:
    n = LENGTH(s);
    rawptr = RAW(s);
    if (dup) {
        rawptr = (Rbyte *) R_alloc(n, sizeof(Rbyte));
        for (i = 0; i < n; i++)
            rawptr[i] = RAW(s)[i];
    }
    return (void *) rawptr;
    break;
    case LGLSXP:
    case INTSXP:
	n = LENGTH(s);
	iptr = INTEGER(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && iptr[i] == NA_INTEGER)
		error(_("NAs in foreign function call (arg %d)"), narg);
	}
	if (dup) {
	    iptr = (int*)R_alloc(n, sizeof(int));
	    for (i = 0 ; i < n ; i++)
		iptr[i] = INTEGER(s)[i];
	}
	return (void*)iptr;
	break;
    case REALSXP:
	n = LENGTH(s);
	rptr = REAL(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && !R_FINITE(rptr[i]))
		error(_("NA/NaN/Inf in foreign function call (arg %d)"), narg);
	}
	if (dup) {
	    if(asLogical(getAttrib(s, CSingSymbol)) == 1) {
		sptr = (float*)R_alloc(n, sizeof(float));
		for (i = 0 ; i < n ; i++)
		    sptr[i] = (float) REAL(s)[i];
		return (void*)sptr;
	    } else {
		rptr = (double*)R_alloc(n, sizeof(double));
		for (i = 0 ; i < n ; i++)
		    rptr[i] = REAL(s)[i];
		return (void*)rptr;
	    }
	} else
	    return (void*)rptr;
	break;
    case CPLXSXP:
	n = LENGTH(s);
	zptr = COMPLEX(s);
	for (i = 0 ; i < n ; i++) {
	    if(!naok && (!R_FINITE(zptr[i].r) || !R_FINITE(zptr[i].i)))
		error(_("complex NA/NaN/Inf in foreign function call (arg %d)"),
		      narg);
	}
	if (dup) {
	    zptr = (Rcomplex*)R_alloc(n, sizeof(Rcomplex));
	    for (i = 0 ; i < n ; i++)
		zptr[i] = COMPLEX(s)[i];
	}
	return (void*)zptr;
	break;
    case STRSXP:
	if(!dup)
	    error(_("character variables must be duplicated in .C/.Fortran"));
	n = LENGTH(s);
	if(Fort) {
	    const char *ss = translateChar(STRING_ELT(s, 0));
	    if(n > 1)
		warning(_("only first string in char vector used in .Fortran"));
	    l = strlen(ss);
	    fptr = (char*)R_alloc(max(255, l) + 1, sizeof(char));
	    strcpy(fptr, ss);
	    return (void*)fptr;
	} else {
	    cptr = (char**)R_alloc(n, sizeof(char*));
	    if(strlen(encname)) {
#ifdef HAVE_ICONV
		char *outbuf;
                const char *inbuf;
		size_t inb, outb, outb0, res;
		void *obj = Riconv_open("", encname); /* (to, from) */
		if(obj == (void *)-1)
		    error(_("unsupported encoding '%s'"), encname);
		for (i = 0 ; i < n ; i++) {
		    inbuf = CHAR(STRING_ELT(s, i));
		    inb = strlen(inbuf);
		    outb0 = 3*inb;
		restart_in:
		    cptr[i] = outbuf = (char*)R_alloc(outb0 + 1, sizeof(char));
		    outb = 3*inb;
		    Riconv(obj, NULL, NULL, &outbuf, &outb);
		    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
		    if(res == -1 && errno == E2BIG) {
			outb0 *= 3;
			goto restart_in;
		    }
		    if(res == -1) 
			error(_("conversion problem in re-encoding to '%s'"),
			      encname);
		    *outbuf = '\0';
		}
		Riconv_close(obj);
	    } else 
#else
		warning(_("re-encoding is not supported on this system"));
	    }
#endif
	    {
		for (i = 0 ; i < n ; i++) {
		    const char *ss = translateChar(STRING_ELT(s, i));
		    l = strlen(ss);
		    cptr[i] = (char*)R_alloc(l + 1, sizeof(char));
		    strcpy(cptr[i], ss);
		}
	    }
	    return (void*)cptr;
	}
	break;
    case VECSXP:
	if(!dup)
	    error(_("lists must be duplicated in .C"));
	/* if (!dup) return (void*)VECTOR_PTR(s); ***** Dangerous to GC!!! */
  	n = length(s);
	lptr = (SEXP*)R_alloc(n, sizeof(SEXP));
	for (i = 0 ; i < n ; i++) {
	    lptr[i] = VECTOR_ELT(s, i);
	}
 	return (void*)lptr;
	break;
    case LISTSXP:
	if(Fort) error(_("invalid mode to pass to Fortran (arg %d)"), narg);
	/* Warning : The following looks like it could bite ... */
	if(!dup) return (void*)s;
	n = length(s);
	cptr = (char**)R_alloc(n, sizeof(char*));
	for(i=0 ; i<n ; i++) {
	    cptr[i] = (char*)s;
	    s = CDR(s);
	}
	return (void*)cptr;
	break;
    default:
	if(Fort) error(_("invalid mode to pass to Fortran (arg %d)"), narg);
	return (void*)s;
    }
}


static SEXP CPtrToRObj(void *p, SEXP arg, int Fort,
		       R_NativePrimitiveArgType type, char *encname)
{
    Rbyte *rawptr;
    int *iptr, n=length(arg);
    float *sptr;
    double *rptr;
    char **cptr, buf[256];
    Rcomplex *zptr;
    SEXP *lptr, CSingSymbol = install("Csingle");
    int i;
    SEXP s, t;

    switch(type) {
    case RAWSXP:
    s = allocVector(type, n);
    rawptr = (Rbyte *)p;
    for (i = 0; i < n; i++)
        RAW(s)[i] = rawptr[i];
    break;
    case LGLSXP:
    case INTSXP:
	s = allocVector(type, n);
	iptr = (int*)p;
	for(i=0 ; i<n ; i++)
            INTEGER(s)[i] = iptr[i];
	break;
    case REALSXP:
    case SINGLESXP:
	s = allocVector(REALSXP, n);
	if(type == SINGLESXP || asLogical(getAttrib(arg, CSingSymbol)) == 1) {
	    sptr = (float*) p;
	    for(i=0 ; i<n ; i++) REAL(s)[i] = (double) sptr[i];
	} else {
	    rptr = (double*) p;
	    for(i=0 ; i<n ; i++) REAL(s)[i] = rptr[i];
	}
	break;
    case CPLXSXP:
	s = allocVector(type, n);
	zptr = (Rcomplex*)p;
	for(i=0 ; i<n ; i++) {
	    COMPLEX(s)[i] = zptr[i];
	}
	break;
    case STRSXP:
	if(Fort) {
	    /* only return one string: warned on the R -> Fortran step */
	    strncpy(buf, (char*)p, 255);
	    buf[255] = '\0';
	    PROTECT(s = allocVector(type, 1));
	    SET_STRING_ELT(s, 0, mkChar(buf));
	    UNPROTECT(1);
	} else {
	    PROTECT(s = allocVector(type, n));
	    cptr = (char**)p;
	    if(strlen(encname)) {
#ifdef HAVE_ICONV
                const char *inbuf;
		char *outbuf, *p;
		size_t inb, outb, outb0, res;
		void *obj = Riconv_open(encname, ""); /* (to, from) */
		if(obj == (void *)(-1))
		    error(_("unsupported encoding '%s'"), encname);
		for (i = 0 ; i < n ; i++) {
		    inbuf = cptr[i]; inb = strlen(inbuf);
		    outb0 = 3*inb;
		restart_out:
		    p = outbuf = (char*)R_alloc(outb0 + 1, sizeof(char));
		    outb = outb0;
		    Riconv(obj, NULL, NULL, &outbuf, &outb);
		    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
		    if(res == -1 && errno == E2BIG) {
			outb0 *= 3;
			goto restart_out;
		    }
		    if(res == -1) 
			error(_("conversion problem in re-encoding from '%s'"),
			      encname);
		    *outbuf = '\0';
		    SET_STRING_ELT(s, i, mkChar(p));
		}
		Riconv_close(obj);
	    } else 
#else
		warning(_("re-encoding is not supported on this system"));
	    }
#endif
	    {
		for(i = 0 ; i < n ; i++)
		    SET_STRING_ELT(s, i, mkChar(cptr[i]));
	    }
	    UNPROTECT(1);
	}
	break;
    case VECSXP:
	PROTECT(s = allocVector(VECSXP, n));
	lptr = (SEXP*)p;
	for (i = 0 ; i < n ; i++) {
	    SET_VECTOR_ELT(s, i, lptr[i]);
	}
	UNPROTECT(1);
	break;
    case LISTSXP:
	PROTECT(t = s = allocList(n));
	lptr = (SEXP*)p;
	for(i=0 ; i<n ; i++) {
	    SETCAR(t, lptr[i]);
	    t = CDR(t);
	}
	UNPROTECT(1);
    default:
	s = (SEXP)p;
    }
    return s;
}

#define THROW_REGISTRATION_TYPE_ERROR

#ifdef THROW_REGISTRATION_TYPE_ERROR
static Rboolean
comparePrimitiveTypes(R_NativePrimitiveArgType type, SEXP s, Rboolean dup)
{
   if(type == ANYSXP || TYPEOF(s) == type)
      return(TRUE);

   if(dup && type == SINGLESXP)
      return(asLogical(getAttrib(s, install("Csingle"))) == TRUE);

   return(FALSE);
}
#endif /* end of THROW_REGISTRATION_TYPE_ERROR */


/* Foreign Function Interface.  This code allows a user to call C */
/* or Fortran code which is either statically or dynamically linked. */

/* NB: this leaves NAOK and DUP arguments on the list */

/* find NAOK and DUP, find and remove PACKAGE */
static SEXP naokfind(SEXP args, int * len, int *naok, int *dup,
		     DllReference *dll)
{
    SEXP s, prev;
    int nargs=0, naokused=0, dupused=0, pkgused=0;
    const char *p;

    *naok = 0;
    *dup = 1;
    *len = 0;
    for(s = args, prev=args; s != R_NilValue;) {
	if(TAG(s) == NaokSymbol) {
	    *naok = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(naokused++ == 1) warning(_("NAOK used more than once"));
	} else if(TAG(s) == DupSymbol) {
	    *dup = asLogical(CAR(s));
	    /* SETCDR(prev, s = CDR(s)); */
	    if(dupused++ == 1) warning(_("DUP used more than once"));
	} else if(TAG(s) == PkgSymbol) {
	    dll->obj = CAR(s);
	    if(TYPEOF(CAR(s)) == STRSXP) {
		p = translateChar(STRING_ELT(CAR(s), 0));
		if(strlen(p) > PATH_MAX - 1)
		    error(_("DLL name is too long"));
		dll->type = FILENAME;
		strcpy(dll->DLLname, p);
		if(pkgused++ > 1) warning(_("PACKAGE used more than once"));
		/* More generally, this should allow us to process
		   any additional arguments and not insist that PACKAGE
		   be the last argument.
		*/
	    } else {
		    /* Have a DLL object*/
	        if(TYPEOF(CAR(s)) == EXTPTRSXP) {
		    dll->dll = (HINSTANCE) R_ExternalPtrAddr(CAR(s));
		    dll->type = DLL_HANDLE;
		} else if(TYPEOF(CAR(s)) == VECSXP) {
		    dll->type = R_OBJECT;
		    dll->obj = s;
		    strcpy(dll->DLLname,
			   translateChar(STRING_ELT(VECTOR_ELT(CAR(s), 1), 0)));
		    dll->dll = (HINSTANCE) R_ExternalPtrAddr(VECTOR_ELT(s, 4));
		}
	    }
	} else {
	    nargs++;
	    prev = s;
	    s = CDR(s);
	    continue;
	}
	if(s == args)
	    args = s = CDR(s);
	else
	    SETCDR(prev, s = CDR(s));
    }
    *len = nargs;
    return args;
}

static void setDLLname(SEXP s, char *DLLname)
{
    SEXP ss = CAR(s);
    const char *name;

    if(TYPEOF(ss) != STRSXP || length(ss) != 1)
	error(_("PACKAGE argument must be a single character string"));
    name = translateChar(STRING_ELT(ss, 0));
    /* allow the package: form of the name, as returned by find */
    if(strncmp(name, "package:", 8) == 0)
	name += 8;
    if(strlen(name) > PATH_MAX - 1)
	error(_("PACKAGE argument is too long"));
    strcpy(DLLname, name);
}

static SEXP pkgtrim(SEXP args, DllReference *dll)
{
    SEXP s, ss;
    int pkgused=0;

    for(s = args ; s != R_NilValue;) {
	ss = CDR(s);
	/* Look for PACKAGE=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == R_NilValue && TAG(s) == PkgSymbol) {
	    if(pkgused++ == 1) warning(_("PACKAGE used more than once"));
	    setDLLname(s, dll->DLLname);
	    dll->type = FILENAME;
	    return R_NilValue;
	}
	if(TAG(ss) == PkgSymbol) {
	    if(pkgused++ == 1) warning(_("PACKAGE used more than once"));
	    setDLLname(ss, dll->DLLname);
	    dll->type = FILENAME;
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}

static SEXP enctrim(SEXP args, char *name, int len)
{
    SEXP s, ss, sx;
    int pkgused=0;

    strcpy(name, "");
    for(s = args ; s != R_NilValue;) {
	ss = CDR(s);
	/* Look for ENCODING=. We look at the next arg, unless
	   this is the last one (which will only happen for one arg),
	   and remove it */
	if(ss == R_NilValue && TAG(s) == EncSymbol) {
	    sx = CAR(s);
	    if(pkgused++ == 1) warning(_("ENCODING used more than once"));
	    if(TYPEOF(sx) != STRSXP || length(sx) != 1)
		error(_("ENCODING argument must be a single character string"));
	    strncpy(name, translateChar(STRING_ELT(sx, 0)), len);
	    return R_NilValue;
	}
	if(TAG(ss) == EncSymbol) {
	    sx = CAR(ss);
	    if(pkgused++ == 1) warning(_("ENCODING used more than once"));
	    if(TYPEOF(sx) != STRSXP || length(sx) != 1)
		error(_("ENCODING argument must be a single character string"));
	    strncpy(name, translateChar(STRING_ELT(sx, 0)), len);
	    SETCDR(s, CDR(ss));
	}
	s = CDR(s);
    }
    return args;
}



SEXP attribute_hidden do_isloaded(SEXP call, SEXP op, SEXP args, SEXP env)
{
    const char *sym, *type="", *pkg = "";
    int val = 1, nargs = length(args);
    R_RegisteredNativeSymbol symbol = {R_FORTRAN_SYM, {NULL}, NULL};

    if (nargs < 1) error(_("no arguments supplied"));
    if (nargs > 3) error(_("too many arguments"));

    if(!isValidString(CAR(args)))
	error(R_MSG_IA);
    sym = translateChar(STRING_ELT(CAR(args), 0));
    if(nargs >= 2) {
	if(!isValidString(CADR(args)))
	    error(R_MSG_IA);
	pkg = translateChar(STRING_ELT(CADR(args), 0));
    }
    if(nargs >= 3) {
	if(!isValidString(CADDR(args)))
	    error(R_MSG_IA);
	type = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
	if(strcmp(type, "C") == 0) symbol.type = R_C_SYM;
	else if(strcmp(type, "Fortran") == 0) symbol.type = R_FORTRAN_SYM;
	else if(strcmp(type, "Call") == 0) symbol.type = R_CALL_SYM;
	else if(strcmp(type, "External") == 0) symbol.type = R_EXTERNAL_SYM;
    }
    if(strlen(type)) {
	if(!(R_FindSymbol(sym, pkg, &symbol))) val = 0;
    } else {
	if (!(R_FindSymbol(sym, pkg, NULL)) && 
	    !(R_FindSymbol(sym, pkg, &symbol))) val = 0;
    }
    return ScalarLogical(val);
}

/*   Call dynamically loaded "internal" functions */
/*   code by Jean Meloche <jean@stat.ubc.ca> */

typedef SEXP (*R_ExternalRoutine)(SEXP);

SEXP attribute_hidden do_External(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = NULL;
    R_ExternalRoutine fun = NULL;
    SEXP retval;
    R_RegisteredNativeSymbol symbol = {R_EXTERNAL_SYM, {NULL}, NULL};
    void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    args = resolveNativeRoutine(args, &ofun, &symbol, buf, NULL, NULL,
				NULL, call);
    fun = (R_ExternalRoutine) ofun;

    /* Some external symbols that are registered may have 0 as the
       expected number of arguments.  We may want a warning
       here. However, the number of values may vary across calls and
       that is why people use the .External() mechanism.  So perhaps
       we should just kill this check.
    */
#ifdef CHECK_EXTERNAL_ARG_COUNT         /* Off by default. */
    if(symbol.symbol.external && symbol.symbol.external->numArgs > -1) {
	if(symbol.symbol.external->numArgs != length(args))
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for %s"),
		      length(args), symbol.symbol.external->numArgs,
		      translateChar(STRING_ELT(CAR(args), 0)));
    }
#endif

    retval = (SEXP)fun(args);
    vmaxset(vmax);
    return retval;
}

#ifdef __cplusplus
typedef SEXP (*VarFun)(...);
#else
typedef DL_FUNC VarFun;
#endif

/* .Call(name, <args>) */
SEXP attribute_hidden do_dotcall(SEXP call, SEXP op, SEXP args, SEXP env)
{
    DL_FUNC ofun = NULL;
    VarFun fun = NULL;
    SEXP retval, nm, cargs[MAX_ARGS], pargs;
    R_RegisteredNativeSymbol symbol = {R_CALL_SYM, {NULL}, NULL};
    int nargs;
    void *vmax = vmaxget();
    char buf[MaxSymbolBytes];

    nm = CAR(args);
    args = resolveNativeRoutine(args, &ofun, &symbol, buf, NULL, NULL,
				NULL, call);
    args = CDR(args);
    fun = (VarFun) ofun;

    for(nargs = 0, pargs = args ; pargs != R_NilValue; pargs = CDR(pargs)) {
        if (nargs == MAX_ARGS)
            errorcall(call, _("too many arguments in foreign function call"));
	cargs[nargs] = CAR(pargs);
	nargs++;
    }
    if(symbol.symbol.call && symbol.symbol.call->numArgs > -1) {
	if(symbol.symbol.call->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for %s"),
		      nargs, symbol.symbol.call->numArgs,
		      translateChar(STRING_ELT(nm, 0)));
    }

    retval = R_NilValue;	/* -Wall */
    fun = (VarFun) ofun;
    switch (nargs) {
    case 0:
	retval = (SEXP)ofun();
	break;
    case 1:
	retval = (SEXP)fun(cargs[0]);
	break;
    case 2:
	retval = (SEXP)fun(cargs[0], cargs[1]);
	break;
    case 3:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	retval = (SEXP)fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	retval = (SEXP)fun(
	    cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    default:
	errorcall(call, _("too many arguments, sorry"));
    }
    vmaxset(vmax);
    return retval;
}

/*  Call dynamically loaded "internal" graphics functions */
/*  .External.gr  and  .Call.gr */

SEXP attribute_hidden do_Externalgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    GEDevDesc *dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_External(call, op, args, env));
    /*
     * If there is an error or user-interrupt in the above
     * evaluation, dd->recordGraphics is set to TRUE 
     * on all graphics devices (see GEonExit(); called in errors.c)
     */
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    errorcall(call, _("Invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

SEXP attribute_hidden do_dotcallgr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP retval;
    GEDevDesc *dd = GEcurrentDevice();
    Rboolean record = dd->recordGraphics;
    dd->recordGraphics = FALSE;
    PROTECT(retval = do_dotcall(call, op, args, env));
    /*
     * If there is an error or user-interrupt in the above
     * evaluation, dd->recordGraphics is set to TRUE 
     * on all graphics devices (see GEonExit(); called in errors.c)
     */
    dd->recordGraphics = record;
    if (GErecording(call, dd)) {
	if (!GEcheckState(dd))
	    errorcall(call, _("Invalid graphics state"));
	GErecordGraphicOperation(op, args, dd);
    }
    UNPROTECT(1);
    return retval;
}

static SEXP
Rf_getCallingDLL()
{
    SEXP e, ans;
    RCNTXT *cptr;
    SEXP rho = R_NilValue;
    Rboolean found = FALSE;

    /* First find the environment of the caller.
       Testing shows this is the right caller, despite the .C/.Call ...
     */
    for (cptr = R_GlobalContext;
	 cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	 cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_FUNCTION) {
		/* PrintValue(cptr->call); */
		rho = cptr->cloenv;
		break;
	    }
    /* Then search up until we hit a namespace or globalenv.
       The idea is that we will not find a namespace unless the caller
       was defined in one. */
    while(rho != R_NilValue) {
	if (rho == R_GlobalEnv) break;
	else if (R_IsNamespaceEnv(rho)) {
	    found = TRUE;
	    break;
	}
	rho = ENCLOS(rho);
    }
    if(!found) return R_NilValue;

    PROTECT(e = lang2(Rf_install("getCallingDLLe"), rho));
    ans = eval(e,  R_GlobalEnv);
    UNPROTECT(1);
    return(ans);
}


/*
  We are given the PACKAGE argument in dll.obj
  and we can try to figure out how to resolve this.
  0) dll.obj is NULL.  Then find the environment of the
   calling function and if it is a namespace, get the

  1) dll.obj is a DLLInfo object
*/
static DL_FUNC
R_FindNativeSymbolFromDLL(char *name, DllReference *dll,
			  R_RegisteredNativeSymbol *symbol)
{
    int numProtects = 0;
    DllInfo *info;
    DL_FUNC fun = NULL;

    if(dll->obj == NULL) {
	/* Rprintf("\nsearching for %s\n", name); */
	dll->obj = Rf_getCallingDLL();
	PROTECT(dll->obj); numProtects++;
    }

    if(inherits(dll->obj, "DLLInfo")) {
	SEXP tmp;
	tmp = VECTOR_ELT(dll->obj, 4);
	info = (DllInfo *) R_ExternalPtrAddr(tmp);
	if(!info)
	    error(_("NULL value for DLLInfoReference when looking for DLL"));
	fun = R_dlsym(info, name, symbol);
    }

    if(numProtects)
	UNPROTECT(numProtects);

    return(fun);
}



/* .C() {op=0}  or  .Fortran() {op=1} */
SEXP attribute_hidden do_dotCode(SEXP call, SEXP op, SEXP args, SEXP env)
{
    void **cargs;
    int dup, havenames, naok, nargs, which;
    DL_FUNC ofun = NULL;
    VarFun fun = NULL;
    SEXP ans, pargs, s;
    /* the post-call converters back to R objects. */
    R_toCConverter  *argConverters[65];
    R_RegisteredNativeSymbol symbol = {R_C_SYM, {NULL}, NULL};
    R_NativePrimitiveArgType *checkTypes = NULL;
    R_NativeArgStyle *argStyles = NULL;
    void *vmax;
    char symName[MaxSymbolBytes], encname[101];

    if (NaokSymbol == NULL || DupSymbol == NULL || PkgSymbol == NULL) {
	NaokSymbol = install("NAOK");
	DupSymbol = install("DUP");
	PkgSymbol = install("PACKAGE");
    }
    if (EncSymbol == NULL) EncSymbol = install("ENCODING");
    vmax = vmaxget();
    which = PRIMVAL(op);
    if(which)
	symbol.type = R_FORTRAN_SYM;

    args = enctrim(args, encname, 100);
    args = resolveNativeRoutine(args, &ofun, &symbol, symName, &nargs,
				&naok, &dup, call);
    fun = (VarFun) ofun;

    if(symbol.symbol.c && symbol.symbol.c->numArgs > -1) {
	if(symbol.symbol.c->numArgs != nargs)
	    errorcall(call,
		      _("Incorrect number of arguments (%d), expecting %d for %s"),
		      nargs, symbol.symbol.c->numArgs, symName);

	checkTypes = symbol.symbol.c->types;
	argStyles = symbol.symbol.c->styles;
    }


    /* Convert the arguments for use in foreign */
    /* function calls.  Note that we copy twice */
    /* once here, on the way into the call, and */
    /* once below on the way out. */
    cargs = (void**)R_alloc(nargs, sizeof(void*));
    nargs = 0;
    for(pargs = args ; pargs != R_NilValue; pargs = CDR(pargs)) {
#ifdef THROW_REGISTRATION_TYPE_ERROR
        if(checkTypes &&
	   !comparePrimitiveTypes(checkTypes[nargs], CAR(pargs), dup)) {
            /* We can loop over all the arguments and report all the
               erroneous ones, but then we would also want to avoid
               the conversions.  Also, in the future, we may just
               attempt to coerce the value to the appropriate
               type. This is why we pass the checkTypes[nargs] value
               to RObjToCPtr(). We just have to sort out the ability
               to return the correct value which is complicated by
               dup, etc. */
	    errorcall(call, _("Wrong type for argument %d in call to %s"),
		      nargs+1, symName);
	}
#endif
	cargs[nargs] = RObjToCPtr(CAR(pargs), naok, dup, nargs + 1,
				  which, symName, argConverters + nargs,
				  checkTypes ? checkTypes[nargs] : 0,
				  encname);
#ifdef R_MEMORY_PROFILING
	if (TRACE(CAR(pargs)) && dup)
		memtrace_report(CAR(pargs), cargs[nargs]);
#endif
	nargs++;
    }


    switch (nargs) {
    case 0:
	/* Silicon graphics C chokes here */
	/* if there is no argument to fun. */
	fun(0);
	break;
    case 1:
	fun(cargs[0]);
	break;
    case 2:
	fun(cargs[0], cargs[1]);
	break;
    case 3:
	fun(cargs[0], cargs[1], cargs[2]);
	break;
    case 4:
	fun(cargs[0], cargs[1], cargs[2], cargs[3]);
	break;
    case 5:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4]);
	break;
    case 6:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5]);
	break;
    case 7:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6]);
	break;
    case 8:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7]);
	break;
    case 9:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8]);
	break;
    case 10:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9]);
	break;
    case 11:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10]);
	break;
    case 12:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11]);
	break;
    case 13:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12]);
	break;
    case 14:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13]);
	break;
    case 15:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14]);
	break;
    case 16:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15]);
	break;
    case 17:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16]);
	break;
    case 18:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17]);
	break;
    case 19:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18]);
	break;
    case 20:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19]);
	break;
    case 21:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20]);
	break;
    case 22:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21]);
	break;
    case 23:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22]);
	break;
    case 24:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23]);
	break;
    case 25:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24]);
	break;
    case 26:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25]);
	break;
    case 27:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26]);
	break;
    case 28:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27]);
	break;
    case 29:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28]);
	break;
    case 30:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29]);
	break;
    case 31:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30]);
	break;
    case 32:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31]);
	break;
    case 33:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32]);
	break;
    case 34:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33]);
	break;
    case 35:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34]);
	break;
    case 36:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35]);
	break;
    case 37:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36]);
	break;
    case 38:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37]);
	break;
    case 39:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38]);
	break;
    case 40:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39]);
	break;
    case 41:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40]);
	break;
    case 42:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41]);
	break;
    case 43:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42]);
	break;
    case 44:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43]);
	break;
    case 45:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44]);
	break;
    case 46:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45]);
	break;
    case 47:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46]);
	break;
    case 48:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47]);
	break;
    case 49:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48]);
	break;
    case 50:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49]);
	break;
    case 51:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50]);
	break;
    case 52:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51]);
	break;
    case 53:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52]);
	break;
    case 54:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53]);
	break;
    case 55:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54]);
	break;
    case 56:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55]);
	break;
    case 57:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56]);
	break;
    case 58:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57]);
	break;
    case 59:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58]);
	break;
    case 60:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59]);
	break;
    case 61:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60]);
	break;
    case 62:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61]);
	break;
    case 63:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62]);
	break;
    case 64:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63]);
	break;
    case 65:
	fun(cargs[0],  cargs[1],  cargs[2],  cargs[3],  cargs[4],
	    cargs[5],  cargs[6],  cargs[7],  cargs[8],  cargs[9],
	    cargs[10], cargs[11], cargs[12], cargs[13], cargs[14],
	    cargs[15], cargs[16], cargs[17], cargs[18], cargs[19],
	    cargs[20], cargs[21], cargs[22], cargs[23], cargs[24],
	    cargs[25], cargs[26], cargs[27], cargs[28], cargs[29],
	    cargs[30], cargs[31], cargs[32], cargs[33], cargs[34],
	    cargs[35], cargs[36], cargs[37], cargs[38], cargs[39],
	    cargs[40], cargs[41], cargs[42], cargs[43], cargs[44],
	    cargs[45], cargs[46], cargs[47], cargs[48], cargs[49],
	    cargs[50], cargs[51], cargs[52], cargs[53], cargs[54],
	    cargs[55], cargs[56], cargs[57], cargs[58], cargs[59],
	    cargs[60], cargs[61], cargs[62], cargs[63], cargs[64]);
	break;
    default:
	errorcall(call, _("too many arguments, sorry"));
    }
    PROTECT(ans = allocVector(VECSXP, nargs));
    havenames = 0;
    if (dup) {
	R_FromCConvertInfo info;
	info.cargs = cargs;
	info.allArgs = args;
	info.nargs = nargs;
	info.functionName = symName;
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
	    if(argStyles && argStyles[nargs] == R_ARG_IN) {
	        PROTECT(s = R_NilValue);
	    } else if(argConverters[nargs]) {
                if(argConverters[nargs]->reverse) {
		    info.argIndex = nargs;
		    s = argConverters[nargs]->reverse(cargs[nargs], CAR(pargs),
						      &info,
						      argConverters[nargs]);
		} else
		    s = R_NilValue;
		PROTECT(s);
	    } else {
		PROTECT(s = CPtrToRObj(cargs[nargs], CAR(pargs), which,
				       checkTypes ? checkTypes[nargs] : TYPEOF(CAR(pargs)),
				       encname));
#if R_MEMORY_PROFILING
		if (TRACE(CAR(pargs)) && dup){
			memtrace_report(cargs[nargs], s);
			SET_TRACE(s, 1);
		}
#endif		
		DUPLICATE_ATTRIB(s, CAR(pargs));
	    }
	    if (TAG(pargs) != R_NilValue)
		havenames = 1;
	    SET_VECTOR_ELT(ans, nargs, s);
	    nargs++;
	    UNPROTECT(1);
	}
    }
    else {
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
	    if (TAG(pargs) != R_NilValue)
		havenames = 1;
	    SET_VECTOR_ELT(ans, nargs, CAR(pargs));
	    nargs++;
	}
    }
    if (havenames) {
	SEXP names;
	PROTECT(names = allocVector(STRSXP, nargs));
	nargs = 0;
	for (pargs = args ; pargs != R_NilValue ; pargs = CDR(pargs)) {
	    if (TAG(pargs) == R_NilValue)
		SET_STRING_ELT(names, nargs++, R_BlankString);
	    else
		SET_STRING_ELT(names, nargs++, PRINTNAME(TAG(pargs)));
        }
	setAttrib(ans, R_NamesSymbol, names);
	UNPROTECT(1);
    }
    UNPROTECT(1);
    vmaxset(vmax);
    return (ans);
}

/* FIXME : Must work out what happens here when we replace LISTSXP by
   VECSXP. */

static const struct {
    const char *name;
    const SEXPTYPE type;
}
typeinfo[] = {
    {"logical",	  LGLSXP },
    {"integer",	  INTSXP },
    {"double",	  REALSXP},
    {"complex",	  CPLXSXP},
    {"character", STRSXP },
    {"list",	  VECSXP },
    {NULL,	  0      }
};

static int string2type(char *s)
{
    int i;
    for (i = 0 ; typeinfo[i].name ; i++) {
	if(!strcmp(typeinfo[i].name, s)) {
	    return typeinfo[i].type;
	}
    }
    error(_("type \"%s\" not supported in interlanguage calls"), s);
    return 1; /* for -Wall */
}

void call_R(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    SEXP call, pcall, s;
    SEXPTYPE type;
    int i, j, n;

    if (!isFunction((SEXP)func))
	error(_("invalid function in call_R"));
    if (nargs < 0)
	error(_("invalid argument count in call_R"));
    if (nres < 0)
	error(_("invalid return value count in call_R"));
    PROTECT(pcall = call = allocList(nargs + 1));
    SET_TYPEOF(call, LANGSXP);
    SETCAR(pcall, (SEXP)func);
    s = R_NilValue;		/* -Wall */
    for (i = 0 ; i < nargs ; i++) {
	pcall = CDR(pcall);
	type = string2type(modes[i]);
	switch(type) {
	case LGLSXP:
	case INTSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(type, n));
	    memcpy(INTEGER(CAR(pcall)), arguments[i], n * sizeof(int));
	    break;
	case REALSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(REALSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(double));
	    break;
	case CPLXSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(CPLXSXP, n));
	    memcpy(REAL(CAR(pcall)), arguments[i], n * sizeof(Rcomplex));
	    break;
	case STRSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(STRSXP, n));
	    for (j = 0 ; j < n ; j++) {
		char *str = (char*)(arguments[i]);
		SET_STRING_ELT(CAR(pcall), i, mkChar(str));
 	    }
	    break;
	    /* FIXME : This copy is unnecessary! */
	    /* FIXME : This is obviously incorrect so disable
	case VECSXP:
	    n = lengths[i];
	    SETCAR(pcall, allocVector(VECSXP, n));
	    for (j = 0 ; j < n ; j++) {
		SET_VECTOR_ELT(s, i, (SEXP)(arguments[i]));
	    }
	    break; */
	default:
	    error(_("mode '%s' is not supported in call_R"), modes[i]);
	}
	if(names && names[i])
	    SET_TAG(pcall, install(names[i]));
	SET_NAMED(CAR(pcall), 2);
    }
    PROTECT(s = eval(call, R_GlobalEnv));
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	if(nres > 0)
	    results[0] = (char *) RObjToCPtr(s, 1, 1, 0, 0, (const char *)NULL,
					     NULL, 0, "");
	break;
    case VECSXP:
	n = length(s);
	if (nres < n) n = nres;
	for (i = 0 ; i < n ; i++) {
	    results[i] = (char *) RObjToCPtr(VECTOR_ELT(s, i), 1, 1, 0, 0,
					     (const char *)NULL, NULL, 0, "");
	}
	break;
    case LISTSXP:
	n = length(s);
	if(nres < n) n = nres;
	for(i=0 ; i<n ; i++) {
	    results[i] =(char *) RObjToCPtr(s, 1, 1, 0, 0, (const char *)NULL,
					    NULL, 0, "");
	    s = CDR(s);
	}
	break;
    }
    UNPROTECT(2);
    return;
}

void call_S(char *func, long nargs, void **arguments, char **modes,
	    long *lengths, char **names, long nres, char **results)
{
    call_R(func, nargs, arguments, modes, lengths, names, nres, results);
}
