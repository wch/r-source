/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2008   Robert Gentleman, Ross Ihaka
 *                            and the R Development Core Team
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

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for putenv */
#include <Defn.h>
#include <R_ext/Riconv.h>
#include <Rinterface.h>
#include <errno.h>

/*
  See ../unix/system.txt for a description of some of these functions.
  Formally part of ../unix/sys-common.c.
 */

/* The __APPLE__ and __APPLE_CC__ defines are for OS X */

/*
 * FILESYSTEM INTERACTION
 */

/*
 * This call provides a simple interface to the "stat" system call.
 */

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if HAVE_AQUA
extern int (*ptr_CocoaSystem)(char*);
extern	Rboolean useaqua;
#endif

Rboolean attribute_hidden R_FileExists(const char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}

double attribute_hidden R_FileMtime(const char *path)
{
    struct stat sb;
    if (stat(R_ExpandFileName(path), &sb) != 0)
	error(_("cannot determine file modification time of '%s'"), path);
    return sb.st_mtime;
}

    /*
     *  Unix file names which begin with "." are invisible.
     */

Rboolean attribute_hidden R_HiddenFile(const char *name)
{
    if (name && name[0] != '.') return 0;
    else return 1;
}

/* The MSVC runtime has a global to determine whether an unspecified
   file open is in text or binary mode.  We force explicit text mode
   here to avoid depending on that global, which may have been changed
   by user code (most likely in embedded applications of R).
*/

#ifdef Win32

static char * fixmode(const char *mode)
{
    /* Rconnection can have a mode of 4 chars plus a null; we might
     * add one char */
    static char fixedmode[6];
    fixedmode[4] = '\0';
    strncpy(fixedmode, mode, 4);
    if (!strpbrk(fixedmode, "bt")) {
    	strcat(fixedmode, "t");
    }
    return fixedmode;
}

static wchar_t * wcfixmode(const wchar_t *mode)
{
    static wchar_t wcfixedmode[6];
    wcfixedmode[4] = L'\0';
    wcsncpy(wcfixedmode, mode, 4);
    if (!wcspbrk(wcfixedmode, L"bt")) {
    	wcscat(wcfixedmode, L"t");
    }
    return wcfixedmode;
}

#else
#define fixmode(mode) (mode)
#define wcfixmode(mode) (mode)
#endif

FILE *R_fopen(const char *filename, const char *mode)
{
    return(filename ? fopen(filename, fixmode(mode)) : NULL );
}

/* The point of this function is to allow file names in foreign
   character sets.  On Unix-alikes in a UTF-8 locale all that is
   needed is to convert file names to UTF-8, since they will be stored
   in UTF-8.  For other locales, it seems that there is no way to specify
   a file name in UTF-8.

   On NT-based versions of Windows, file names are stored in 'Unicode'
   (UCS-2), and _wfopen is provided to access them by UCS-2 names.
*/

#if defined(Win32)

wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand)
{
    static wchar_t filename[MAX_PATH+1];
    void *obj;
    const char *from = "", *inbuf;
    char *outbuf;
    size_t inb, outb, res;

    if(IS_LATIN1(fn)) from = "latin1";
    if(IS_UTF8(fn)) from = "UTF-8";
    obj = Riconv_open("UCS-2LE", from);
    if(obj == (void *)(-1))
	error(_("unsupported conversion in 'filenameToWchar'"));

    if(expand) inbuf = R_ExpandFileName(CHAR(fn)); else inbuf = CHAR(fn);

    inb = strlen(inbuf)+1; outb = 2*(MAX_PATH+1);
    outbuf = (char *) filename;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(res == -1 || inb > 0) error(_("file name conversion problem"));

    return filename;
}

FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode)
{
    return(filename ? _wfopen(filename, wcfixmode(mode)) : NULL );
}


FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    wchar_t wmode[10];

    mbstowcs(wmode, fixmode(mode), 10);
    return _wfopen(filenameToWchar(fn, expand), wmode);
}
#else
FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    const char *filename = translateChar(fn);
    if(!filename) return NULL;
    if(expand) return fopen(R_ExpandFileName(filename), mode);
    else return fopen(filename, mode);
}
#endif

/*
 *  SYSTEM INFORMATION
 */

          /* The location of the R system files */

char *R_HomeDir(void)
{
    return getenv("R_HOME");
}


SEXP attribute_hidden do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarLogical( (R_Interactive) ? 1 : 0 );
}

SEXP attribute_hidden do_tempdir(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return mkString(R_TempDir);
}


SEXP attribute_hidden do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans, pattern, tempdir;
    const char *tn, *td;
    char *tm;
    int i, n1, n2, slen;

    checkArity(op, args);
    pattern = CAR(args); n1 = length(pattern);
    tempdir = CADR(args); n2 = length(tempdir);
    if (!isString(pattern))
        error(_("invalid filename pattern"));
    if (!isString(tempdir))
        error(_("invalid '%s' value"), "tempdir");
    if (n1 < 1)
	error(_("no 'pattern'"));
    if (n2 < 1)
	error(_("no 'tempdir'"));
    slen = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(STRSXP, slen));
    for(i = 0; i < slen; i++) {
	tn = translateChar( STRING_ELT( pattern , i%n1 ) );
	td = translateChar( STRING_ELT( tempdir , i%n2 ) );
	/* try to get a new file name */
	tm = R_tmpnam(tn, td);
	SET_STRING_ELT(ans, i, mkChar(tm));
	if(tm) free(tm);
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef HAVE_POPEN
FILE *R_popen(const char *command, const char *type)
{
    FILE *fp;
#ifdef __APPLE_CC__
    /* Luke recommends this to fix PR#1140 */
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
    fp = popen(command, type);
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
#else
    fp = popen(command, type);
#endif
    return fp;
}
#endif /* HAVE_POPEN */

int R_system(const char *command)
{
    int val;
#ifdef __APPLE_CC__
    /* Luke recommends this to fix PR#1140 */
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
#ifdef HAVE_AQUA
    char *cmdcpy;
    if(useaqua) {
        /* FIXME, is Cocoa's interface not const char*? */
        cmdcpy = acopy_string(command);
	val = ptr_CocoaSystem(cmdcpy);
    }
    else
#endif
    val = system(command);
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
#else
    val = system(command);
#endif
    return val;
}

#if defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
#else
extern char ** environ;
#endif

#ifdef Win32
# define WC_ENVIRON
#endif

#ifdef WC_ENVIRON
/* _wenviron is declared in stdlib.h */
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h> /* _wgetenv etc */
const wchar_t *wtransChar(SEXP x);
#endif

SEXP attribute_hidden do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    SEXP ans;

    checkArity(op, args);

    if (!isString(CAR(args)))
	error(_("wrong type for argument"));

    if (!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error(_("wrong type for argument"));

    i = LENGTH(CAR(args));
    if (i == 0) {
#ifdef WC_ENVIRON
	char *buf;
	int n = 0, N;
	wchar_t **w;
	for (i = 0, w = _wenviron; *w != NULL; i++, w++)
	    n = max(n, wcslen(*w));
	N = 3*n+1; buf = alloca(N);
	R_CheckStack();
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, w = _wenviron; *w != NULL; i++, w++) {
	    wcstombs(buf, *w, N); buf[N-1] = '\0';
	    SET_STRING_ELT(ans, i, mkCharEnc(buf, UTF8_MASK));
	}
#else
	char **e;
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    SET_STRING_ELT(ans, i, mkChar(*e));
#endif
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
#ifdef WC_ENVIRON
	    const wchar_t *wnm = wtransChar(STRING_ELT(CAR(args), j));
	    wchar_t *w = _wgetenv(wnm);
	    if (w == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		int n = wcslen(w), N = 3*n+1; /* UCS-2 maps to <=3 UTF-8 */
		char *buf = alloca(N); 
		R_CheckStack();
		wcstombs(buf, w, N); buf[N-1] = '\0'; /* safety */
		SET_STRING_ELT(ans, j, mkCharEnc(buf, UTF8_MASK));
	    }
#else
	    char *s = getenv(translateChar(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		SEXP tmp;
		if(known_to_be_latin1) tmp = mkCharEnc(s, LATIN1_MASK);
		else if(known_to_be_utf8) tmp = mkCharEnc(s, UTF8_MASK);
		else tmp = mkChar(s);
		SET_STRING_ELT(ans, j, tmp);
	    }
#endif
	}
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef WC_ENVIRON
static int Rwputenv(const wchar_t *nm, const wchar_t *val)
{
    wchar_t *buf;
    buf = (wchar_t *) malloc((wcslen(nm) + wcslen(val) + 2) * sizeof(wchar_t));
    if(!buf) return 1;
    wsprintfW(buf, L"%s=%s", nm, val);
    if(_wputenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#elif !defined(HAVE_SETENV) && defined(HAVE_PUTENV)
static int Rputenv(const char *nm, const char *val)
{
    char *buf;
    buf = (char *) malloc((strlen(nm) + strlen(val) + 2) * sizeof(char));
    if(!buf) return 1;
    sprintf(buf, "%s=%s", nm, val);
    if(putenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#endif


SEXP attribute_hidden do_setenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    int i, n;
    SEXP ans, nm, vars;

    checkArity(op, args);

    if (!isString(nm = CAR(args)))
	error(_("wrong type for argument"));
    if (!isString(vars = CADR(args)))
	error(_("wrong type for argument"));
    if(LENGTH(nm) != LENGTH(vars))
	error(_("wrong length for argument"));

    n = LENGTH(vars);
    PROTECT(ans = allocVector(LGLSXP, n));
#ifdef HAVE_SETENV
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = setenv(translateChar(STRING_ELT(nm, i)),
				 translateChar(STRING_ELT(vars, i)),
				 1) == 0;
#elif defined(WC_ENVIRON)
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rwputenv(wtransChar(STRING_ELT(nm, i)),
				   wtransChar(STRING_ELT(vars, i))) == 0;
#else
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rputenv(translateChar(STRING_ELT(nm, i)),
				  translateChar(STRING_ELT(vars, i))) == 0;
#endif
    UNPROTECT(1);
    return ans;
#else
    error(_("'Sys.setenv' is not available on this system"));
    return R_NilValue; /* -Wall */
#endif
}

SEXP attribute_hidden do_unsetenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, n;
    SEXP ans, vars;

    checkArity(op, args);

    if (!isString(vars = CAR(args)))
        error(_("wrong type for argument"));
    n = LENGTH(vars);

#if defined(HAVE_UNSETENV) || defined(HAVE_PUTENV_UNSET) || defined(HAVE_PUTENV_UNSET2)
#ifdef HAVE_UNSETENV
    for (i = 0; i < n; i++) unsetenv(translateChar(STRING_ELT(vars, i)));
#elif defined(HAVE_PUTENV_UNSET)
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s",  translateChar(STRING_ELT(vars, i)));
	putenv(buf);
    }
#elif defined(HAVE_PUTENV_UNSET2)
# ifdef WC_ENVIRON
    for (i = 0; i < n; i++) {
	const wchar_t *w = wtransChar(STRING_ELT(vars, i));
	wchar_t *buf = (wchar_t *) alloca(2*wcslen(w));
	R_CheckStack();
	wcscpy(buf, w);
	wcscat(buf, L"=");
	_wputenv(buf);
    }
# else
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s=", translateChar(STRING_ELT(vars, i)));
	putenv(buf);
    }
# endif
#endif

#elif defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    warning(_("this system cannot unset environment variables: setting to \"\""));
    n = LENGTH(vars);
    for (i = 0; i < n; i++) {
#ifdef HAVE_SETENV
	setenv(translateChar(STRING_ELT(vars, i)), "", 1);
#else
	Rputenv(translateChar(STRING_ELT(vars, i)), "");
#endif
    }

#else
    warning(_("'Sys.unsetenv' is not available on this system"));
#endif

    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++)
        LOGICAL(ans)[i] = !getenv(translateChar(STRING_ELT(vars, i)));
    UNPROTECT(1);
    return ans;
}

#if defined(HAVE_ICONV_H) && defined(ICONV_LATIN1)
# include <iconv.h>
#endif

#ifdef HAVE_ICONVLIST
static unsigned int cnt;

static int
count_one (unsigned int namescount, const char * const *names, void *data)
{
    cnt += namescount;
    return 0;
}

static int
write_one (unsigned int namescount, const char * const *names, void *data)
{
  unsigned int i;
  SEXP ans = (SEXP) data;

  for (i = 0; i < namescount; i++)
      SET_STRING_ELT(ans, cnt++, mkChar(names[i]));
  return 0;
}
#endif

#include "RBufferUtils.h"

/* iconv(x, from, to, sub) */
SEXP attribute_hidden do_iconv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_ICONV) && defined(ICONV_LATIN1)
    SEXP ans, x = CAR(args), si;
    void * obj;
    int i, j, nout;
    const char *inbuf;
    char *outbuf;
    const char *sub;
    size_t inb, outb, res;
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    checkArity(op, args);
    if(isNull(x)) {  /* list locales */
#ifdef HAVE_ICONVLIST
	cnt = 0;
	iconvlist(count_one, NULL);
	PROTECT(ans = allocVector(STRSXP, cnt));
	cnt = 0;
	iconvlist(write_one, (void *)ans);
#else
	PROTECT(ans = R_NilValue);
#endif
    } else {
	const char *from, *to;
	Rboolean isLatin1 = FALSE, isUTF8 = FALSE;

	if(TYPEOF(x) != STRSXP)
	    error(_("'x' must be a character vector"));
	if(!isString(CADR(args)) || length(CADR(args)) != 1)
	    error(_("invalid '%s' argument"), "from");
	if(!isString(CADDR(args)) || length(CADDR(args)) != 1)
	    error(_("invalid '%s' argument"), "to");
	if(!isString(CADDDR(args)) || length(CADDDR(args)) != 1)
	    error(_("invalid '%s' argument"), "sub");
	if(STRING_ELT(CADDDR(args), 0) == NA_STRING) sub = NULL;
	else sub = translateChar(STRING_ELT(CADDDR(args), 0));
	from = CHAR(STRING_ELT(CADR(args), 0)); /* ASCII */
	to = CHAR(STRING_ELT(CADDR(args), 0));
	/* Should we do something about marked CHARSXPs in 'from = ""'? */
	if(streql(to, "UTF-8")) isUTF8 = TRUE;
	if(streql(to, "latin1") || streql(to, "ISO_8859-1")) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_latin1) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_utf8) isUTF8 = TRUE;
	obj = Riconv_open(to, from);
	if(obj == (iconv_t)(-1))
	    error(_("unsupported conversion"));
	PROTECT(ans = duplicate(x));
	R_AllocStringBuffer(0, &cbuff);  /* 0 -> default */
	for(i = 0; i < LENGTH(x); i++) {
	    si = STRING_ELT(x, i);
	top_of_loop:
	    inbuf = CHAR(si); inb = LENGTH(si);
	    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
	    /* First initialize output */
	    Riconv (obj, NULL, NULL, &outbuf, &outb);
        next_char:
	    /* Then convert input  */
	    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
	    *outbuf = '\0';
	    /* other possible error conditions are incomplete
	       and invalid multibyte chars */
	    if(res == -1 && errno == E2BIG) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    } else if(res == -1 && errno == EILSEQ && sub) {
		/* it seems this gets thrown for non-convertible input too */
		if(strcmp(sub, "byte") == 0) {
		    if(outb < 5) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
		    outbuf += 4; outb -= 4;
		} else {
		    if(outb < strlen(sub)) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    memcpy(outbuf, sub, j = strlen(sub)); 
		    outbuf += j;
		    outb -= j;
		}
		inbuf++; inb--;
		goto next_char;
	    }

	    if(res != -1 && inb == 0) {
		/* we can currently only put the result in the CHARSXP
		   cache if it does not contain nuls. */
		Rboolean has_nul = FALSE;
		char *p = cbuff.data;

		nout = cbuff.bufsize - 1 - outb;
		for(j = 0; j < nout; j++) if(!*p++) {has_nul = TRUE; break;}
		if(has_nul) {
		    si = mkCharLen(cbuff.data, nout);
		} else {
		    if(isLatin1) si = mkCharEnc(cbuff.data, LATIN1_MASK);
		    else if(isUTF8) si = mkCharEnc(cbuff.data, UTF8_MASK);
		    else si = mkChar(cbuff.data);
		}
		SET_STRING_ELT(ans, i, si);
	    }
	    else SET_STRING_ELT(ans, i, NA_STRING);
	}
	Riconv_close(obj);
	R_FreeStringBuffer(&cbuff);
    }
    UNPROTECT(1);
    return ans;
#else
    error(_("'iconv' is not available on this system"));
    return R_NilValue; /* -Wall */
#endif
}

int getCharEnc(SEXP x)
{
    if(TYPEOF(x) != CHARSXP)
	error(_("'%s' must be called on a CHARSXP"), "getEncChar");
    if(IS_UTF8(x)) return CE_UTF8;
    else if(IS_LATIN1(x)) return CE_LATIN1;
    else return CE_NATIVE;
}


#if defined(HAVE_ICONV) && defined(ICONV_LATIN1)
void * Riconv_open (const char* tocode, const char* fromcode)
{
#ifdef Win32
    const char *cp = "UTF-8";
#ifndef SUPPORT_UTF8_WIN32
    cp = locale2charset(NULL);
#endif
    if(strcmp(tocode, "") == 0)  return iconv_open(cp, fromcode);
    else if(strcmp(fromcode, "") == 0) return iconv_open(tocode, cp);
    else return iconv_open(tocode, fromcode);
#else
    return iconv_open(tocode, fromcode);
#endif
}

/* Should be defined in config.h */
#ifndef ICONV_CONST
# define ICONV_CONST
#endif

size_t Riconv (void *cd, const char **inbuf, size_t *inbytesleft,
	       char **outbuf, size_t *outbytesleft)
{
    /* here libiconv has const char **, glibc has char ** for inbuf */
    return iconv((iconv_t) cd, (ICONV_CONST char **) inbuf, inbytesleft, 
		 outbuf, outbytesleft);
}

int Riconv_close (void *cd)
{
    return iconv_close((iconv_t) cd);
}

static void *latin1_obj = NULL, *utf8_obj=NULL, *ucsmb_obj=NULL;

const char *translateChar(SEXP x)
{
    void * obj;
    const char *inbuf, *ans = CHAR(x);
    char *outbuf, *p;
    size_t inb, outb, res;
#ifdef SUPPORT_MBCS
    int ienc = getCharEnc(x);
#endif
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    if(TYPEOF(x) != CHARSXP)
	error(_("'%s' must be called on a CHARSXP"), "translateChar");
    if(x == NA_STRING || !(ENC_KNOWN(x))) return ans;
    if(utf8locale && IS_UTF8(x)) return ans;
    if(latin1locale && IS_LATIN1(x)) return ans;
    if(strIsASCII(CHAR(x))) return ans;

    if(IS_LATIN1(x)) {
	if(!latin1_obj) {
	    obj = Riconv_open("", "latin1");
	    /* should never happen */
	    if(obj == (void *)(-1)) error(_("unsupported conversion"));
	    latin1_obj = obj;
	}
	obj = latin1_obj;
    } else {
	if(!utf8_obj) {
	    obj = Riconv_open("", "UTF-8");
	    /* should never happen */
	    if(obj == (void *)(-1)) error(_("unsupported conversion"));
	    utf8_obj = obj;
	}
	obj = utf8_obj;	
    }

    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && errno == EILSEQ) {
	if(outb < 13) {
	    R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	    goto top_of_loop;
	}
#ifdef SUPPORT_MBCS
	if (ienc == CE_UTF8) {
	    /* if starting in UTF-8, use \uxxxx */
	    /* This must be the first byte */
	    int clen;
	    wchar_t wc;
	    clen = utf8toucs(&wc, inbuf);
	    if(clen > 0 && inb >= clen) {
		inbuf += clen; inb -= clen;
# ifndef Win32
		if((unsigned int) wc < 65536) {
# endif
		    snprintf(outbuf, 9, "<U+%04X>", (unsigned int) wc);
		    outbuf += 8; outb -= 8;
# ifndef Win32
		} else {
		    snprintf(outbuf, 13, "<U+%08X>", (unsigned int) wc);
		    outbuf += 12; outb -= 12;		
		}
# endif
	    } else {
		snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
		outbuf += 4; outb -= 4;
		inbuf++; inb--;
	    }
	} else
#endif
	{
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	}
	goto next_char;
    }
    *outbuf = '\0';
    res = strlen(cbuff.data) + 1;
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

const char *translateCharUTF8(SEXP x)
{
    void *obj;
    const char *inbuf, *ans = CHAR(x);
    char *outbuf, *p;
    size_t inb, outb, res;
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    if(TYPEOF(x) != CHARSXP)
	error(_("'%s' must be called on a CHARSXP"), "translateCharUTF8");
    if(x == NA_STRING) return ans;
    if(IS_UTF8(x)) return ans;
    if(strIsASCII(CHAR(x))) return ans;

    obj = Riconv_open("UTF-8", IS_LATIN1(x) ? "latin1" : "");
    if(obj == (void *)(-1)) error(_("unsupported conversion"));
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && errno == EILSEQ) {
	if(outb < 5) {
	    R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	    goto top_of_loop;
	}
	snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	outbuf += 4; outb -= 4;
	inbuf++; inb--;
	goto next_char;
    }
    *outbuf = '\0';
    Riconv_close(obj);
    res = strlen(cbuff.data) + 1;
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

#ifdef Win32
static void *latin1_wobj = NULL, *utf8_wobj=NULL;

/* Translate from current encoding to wchar_t = UCS-2 on Windows
   (not using surrogates). NB: this is not general.
*/
const wchar_t *wtransChar(SEXP x)
{
    void * obj;
    const char *inbuf, *ans = CHAR(x);
    char *outbuf;
    wchar_t *p;
    size_t inb, outb, res, top;
    Rboolean knownEnc = FALSE;
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    if(TYPEOF(x) != CHARSXP)
	error(_("'%s' must be called on a CHARSXP"), "wtransChar");

    if(IS_LATIN1(x)) {
	if(!latin1_wobj) {
	    obj = Riconv_open("UCS-2LE", "latin1");
	    if(obj == (void *)(-1)) error(_("unsupported conversion"));
	    latin1_wobj = obj;
	} else
	    obj = latin1_wobj;
	knownEnc = TRUE;
    } else if(IS_UTF8(x)) {
	if(!utf8_wobj) {
	    obj = Riconv_open("UCS-2LE", "UTF-8");
	    if(obj == (void *)(-1)) error(_("unsupported conversion"));
	    utf8_wobj = obj;
	} else
	    obj = utf8_wobj;
	knownEnc = TRUE;
    } else {
	obj = Riconv_open("UCS-2LE", "");
	if(obj == (void *)(-1)) error(_("unsupported conversion"));
    }

    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
    /* Then convert input: should always work  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && errno == EILSEQ) {
	if(!knownEnc) Riconv_close(obj);
	error(_("invalid input in wtransChar"));
    }
    if(!knownEnc) Riconv_close(obj);
    res = (top - outb);
    p = (wchar_t *) R_alloc(res+2, 1);
    memset(p, 0, res+2);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}
#endif

extern const char *Rf_AdobeSymbol2utf8(const char *c0); /* from util.c */

const char *reEnc(const char *x, int ce_in, int ce_out, int subst)
{
    void * obj;
    const char *inbuf;
    char *outbuf, *p;
    size_t inb, outb, res, top;
    char *tocode = NULL, *fromcode = NULL;
#ifdef Win32
    char buf[20];
#endif
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    /* We can only encode from Symbol to UTF-8 */
    if(ce_in == ce_out || ce_out == CE_SYMBOL || 
       ce_in == CE_ANY || ce_out == CE_ANY) return x;
    if(ce_in == CE_SYMBOL) {
	if(ce_out == CE_UTF8) return Rf_AdobeSymbol2utf8(x); else return x;
    }
    if(utf8locale && ce_in == CE_NATIVE && ce_out == CE_UTF8) return x;
    if(utf8locale && ce_out == CE_NATIVE && ce_in == CE_UTF8) return x;
    if(latin1locale && ce_in == CE_NATIVE && ce_out == CE_LATIN1) return x;
    if(latin1locale && ce_out == CE_NATIVE && ce_in == CE_LATIN1) return x;

    if(strIsASCII(x)) return x;
    
    switch(ce_in) {
#ifdef Win32
    case CE_NATIVE: 
	{
	    /* Looks like CP1252 is treated a Latin-1 by iconv */
	    sprintf(buf, "CP%d", localeCP);
	    fromcode = buf; 
	    break;
	}
#else
    case CE_NATIVE: fromcode = ""; break;
#endif
    case CE_LATIN1: fromcode = "latin1"; break;
    case CE_UTF8:   fromcode = "UTF-8"; break;
    default: return x;
    }

    switch(ce_out) {
    case CE_NATIVE: tocode = ""; break;
    case CE_LATIN1: tocode = "latin1"; break;
    case CE_UTF8:   tocode = "UTF-8"; break;
    default: return x;
    }
    
    obj = Riconv_open(tocode, fromcode);
    if(obj == (void *)(-1)) return x;
    R_AllocStringBuffer(0, &cbuff);
top_of_loop:
    inbuf = x; inb = strlen(inbuf);
    outbuf = cbuff.data; top = outb = cbuff.bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
	goto top_of_loop;
    } else if(res == -1 && errno == EILSEQ) {
	switch(subst) {
	case 1: /* substitute hex */
	    if(outb < 5) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	    goto next_char; 
	    break;
	case 2: /* substitute . */
	    if(outb < 1) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    }
	    *outbuf++ = '.'; inbuf++; outb--; inb--;
	    goto next_char; 
	    break;
	default: /* skip byte */
	    inbuf++; inb--;
	    goto next_char; 
	}
    }
    Riconv_close(obj);
    *outbuf = '\0';
    res = (top-outb)+1; /* strlen(cbuff.data) + 1; */
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

void attribute_hidden
invalidate_cached_recodings(void)
{
    latin1_obj = NULL;
    utf8_obj = NULL;
    ucsmb_obj = NULL;
#ifdef Win32
    latin1_wobj = NULL; 
    utf8_wobj=NULL;
#endif    
}


#ifdef WORDS_BIGENDIAN
static const char UNICODE[] = "UCS-4BE";
#else
static const char UNICODE[] = "UCS-4LE";
#endif

/* used in gram.c and devX11.c */
size_t ucstomb(char *s, const unsigned int wc)
{
    char     buf[MB_CUR_MAX+1];
    void    *cd = NULL ;
    unsigned int  wcs[2];
    const char *inbuf = (const char *) wcs;
    size_t   inbytesleft = sizeof(unsigned int); /* better be 4 */
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;
    
    if(wc == 0) {*s = '\0'; return 1;}
    
    memset(buf, 0, sizeof(buf));
    memset(wcs, 0, sizeof(wcs));
    wcs[0] = wc;

    if(ucsmb_obj == NULL) {
	if((void *)(-1) == (cd = Riconv_open("", UNICODE))) {
#ifndef  Win32
	    char tocode[128];
	    /* locale set fuzzy case */
	    strncpy(tocode, locale2charset(NULL), sizeof(tocode));
	    if((void *)(-1) == (cd = Riconv_open(tocode, UNICODE)))
		return (size_t)(-1); 
#else
	    return (size_t)(-1);
#endif
	}
	ucsmb_obj = cd;
    }
    
    status = Riconv(ucsmb_obj, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == (size_t) -1) {
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
    buf[MB_CUR_MAX] = '\0'; /* safety measure */
    strcpy(s, buf);
    return strlen(buf);
}

/* used in plot.c for non-UTF-8 MBCS */
size_t attribute_hidden
mbtoucs(unsigned int *wc, const char *s, size_t n)
{
    unsigned int  wcs[2];
    char     buf[16];
    void    *cd;
    const char *inbuf = s;
    size_t   inbytesleft = strlen(s);
    char    *outbuf = (char *) wcs;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;
    
    if(s[0] == 0) {*wc = 0; return 1;}
    
    if((void *)(-1) == (cd = Riconv_open(UNICODE, ""))) return (size_t)(-1);
    status = Riconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == (size_t) -1) {
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
    *wc = wcs[0];
    return (size_t) 1;
}

/* made available for use in graphics devices */
size_t ucstoutf8(char *s, const unsigned int wc)
{
    char     buf[16];
    void    *cd = NULL ;
    unsigned int  wcs[2];
    const char *inbuf = (const char *) wcs;
    size_t   inbytesleft = sizeof(unsigned int); /* better be 4 */
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;
    
    if(wc == 0) {*s = '\0'; return 1;}
    
    memset(buf, 0, sizeof(buf));
    wcs[0] = wc; wcs[1] = 0;

    if((void *)(-1) == (cd = Riconv_open("UTF-8", UNICODE))) 
	return (size_t)(-1); 
    status = Riconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == (size_t) -1) {
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
    *outbuf = '\0';
    strcpy(s, buf);
    return strlen(buf);
}

#else
void * Riconv_open (const char* tocode, const char* fromcode)
{
    error(_("'iconv' is not available on this system"));
    return (void *)-1;
}

size_t Riconv (void *cd, const char **inbuf, size_t *inbytesleft,
               char **outbuf, size_t *outbytesleft)
{
    error(_("'iconv' is not available on this system"));
    return 0;
}

int Riconv_close (void * cd)
{
    error(_("'iconv' is not available on this system"));
    return -1;
}

const char *translateChar(SEXP x)
{
    return CHAR(x);
}

const char *translateCharUTF8(SEXP x)
{
    return CHAR(x);
}

const char *reEnc(const char *x, int ce_in, int ce_out)
{
    return x;
}

void attribute_hidden
invalidate_cached_recodings(void)
{
}
size_t
ucstoutf8(char *s, const unsigned int wc)
{
}
#endif

/* moved from src/unix/sys-unix.c and src/gnuwin32/extra.c */

#ifdef HAVE_STAT
# ifdef HAVE_ACCESS
#  ifdef HAVE_UNISTD_H
#   include <unistd.h>
#  endif
# endif

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h> /* For GetShortPathName */
#endif

#if !defined(S_IFDIR) && defined(__S_IFDIR)
# define S_IFDIR __S_IFDIR
#endif

static int isDir(char *path)
{
    struct stat sb;
    int isdir = 0;
    if(!path) return 0;
    if(stat(path, &sb) == 0) {
	isdir = (sb.st_mode & S_IFDIR) > 0; /* is a directory */
#ifdef HAVE_ACCESS
	/* We want to know if the directory is writable by this user,
	   which mode does not tell us */
	isdir &= (access(path, W_OK) == 0);
#endif
    }
    return isdir;
}
#else
static int isDir(char *path)
{
    return 1;
}
#endif /* HAVE_STAT */

#if !HAVE_DECL_MKDTEMP
extern char * mkdtemp (char *template);
#endif

void attribute_hidden InitTempDir()
{
    char *tmp, *tm, tmp1[PATH_MAX+11], *p;
    int len;
#ifdef Win32
    char tmp2[MAX_PATH];
    int hasspace = 0;
#endif

    if(R_TempDir) return; /* someone else set it */
    tmp = NULL; /* getenv("R_SESSION_TMPDIR");   no longer set in R.sh */
    if (!tmp) {
	tm = getenv("TMPDIR");
	if (!isDir(tm)) {
	    tm = getenv("TMP");
	    if (!isDir(tm)) {
		tm = getenv("TEMP");
		if (!isDir(tm))
#ifdef Win32
		    tm = getenv("R_USER"); /* this one will succeed */
#else
		    tm = "/tmp";
#endif
	    }
	}
#ifdef Win32
	/* make sure no spaces in path */
	for (p = tm; *p; p++)
	    if (isspace(*p)) { hasspace = 1; break; }
	if (hasspace) {
	    GetShortPathName(tm, tmp2, MAX_PATH);
	    tm = tmp2;
	}
	sprintf(tmp1, "%s\\RtmpXXXXXX", tm);
#else
	sprintf(tmp1, "%s/RtmpXXXXXX", tm);
#endif
	tmp = mkdtemp(tmp1);
	if(!tmp) R_Suicide(_("cannot mkdir R_TempDir"));
#ifndef Win32
# ifdef HAVE_SETENV
	if(setenv("R_SESSION_TMPDIR", tmp, 1))
	    errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
# elif defined(HAVE_PUTENV)
	{
	    char * buf = (char *) malloc((strlen(tmp) + 20) * sizeof(char));
	    if(buf) {
		sprintf(buf, "R_SESSION_TMPDIR=%s", tmp);
		if(putenv(buf)) 
		    errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
		/* no free here: storage remains in use */
	    } else 
		errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
	}
# endif
#endif
    }

    len = strlen(tmp) + 1;
    p = (char *) malloc(len);
    if(!p)
	R_Suicide(_("cannot allocate R_TempDir"));
    else {
	R_TempDir = p;
	strcpy(R_TempDir, tmp);
	Sys_TempDir = R_TempDir;
    }
}

char * R_tmpnam(const char * prefix, const char * tempdir)
{
    char tm[PATH_MAX], tmp1[PATH_MAX], *res;
    unsigned int n, done = 0;
#ifdef Win32
    char filesep[] = "\\";
#else
    char filesep[] = "/";
#endif

    if(!prefix) prefix = "";	/* NULL */
    if(strlen(tempdir) >= PATH_MAX) error(_("invalid 'tempdir' in R_tmpnam"));
    strcpy(tmp1, tempdir);
    for (n = 0; n < 100; n++) {
	/* try a random number at the end.  Need at least 6 hex digits */
#if RAND_MAX > 16777215
	sprintf(tm, "%s%s%s%x", tmp1, filesep, prefix, rand());
#else
	sprintf(tm, "%s%s%s%x%x", tmp1, filesep, prefix, rand(), rand());
#endif
        if(!R_FileExists(tm)) {
	    done = 1;
	    break;
	}
    }
    if(!done)
	error(_("cannot find unused tempfile name"));
    res = (char *) malloc((strlen(tm)+1) * sizeof(char));
    strcpy(res, tm);
    return res;
}

SEXP attribute_hidden do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
#ifdef _R_HAVE_TIMING_
{
    SEXP ans, nm;
    PROTECT(ans = allocVector(REALSXP, 5));
    PROTECT(nm = allocVector(STRSXP, 5));
    R_getProcTime(REAL(ans));
    SET_STRING_ELT(nm, 0, mkChar("user.self"));
    SET_STRING_ELT(nm, 1, mkChar("sys.self"));
    SET_STRING_ELT(nm, 2, mkChar("elapsed"));
    SET_STRING_ELT(nm, 3, mkChar("user.child"));
    SET_STRING_ELT(nm, 4, mkChar("sys.child"));
    setAttrib(ans, R_NamesSymbol, nm);
    setAttrib(ans, R_ClassSymbol, mkString("proc_time"));
    UNPROTECT(2);
    return ans;
}
#else
{
    error(_("proc.time() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif
