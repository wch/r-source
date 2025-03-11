/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2025   The R Core Team
 *  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
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
#include <config.h>
#endif

#include <stdlib.h> /* for putenv */
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h> // for size_t
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <R_ext/Riconv.h>
#include <Rinterface.h>
#include <errno.h>
#include <rlocale.h>

/*
  See ../unix/system.txt for a description of some of these functions.
  Formally part of ../unix/sys-common.c.
 */

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

attribute_hidden int R_isWriteableDir(char *path);

#ifdef HAVE_AQUA
int (*ptr_CocoaSystem)(const char*);
#endif

#ifdef Win32
bool R_FileExists(const char *path)
{
    struct _stati64 sb;
    return _stati64(R_ExpandFileName(path), &sb) == 0;
}

attribute_hidden double R_FileMtime(const char *path)
{
    struct _stati64 sb;
    if (_stati64(R_ExpandFileName(path), &sb) != 0)
	error(_("cannot determine file modification time of '%s'"), path);
    return sb.st_mtime;
}
#else
// used in tools
bool R_FileExists(const char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}

attribute_hidden double R_FileMtime(const char *path)
{
    struct stat sb;
    if (stat(R_ExpandFileName(path), &sb) != 0)
	error(_("cannot determine file modification time of '%s'"), path);
    return (double) sb.st_mtime;
}
#endif

    /*
     *  Unix file names which begin with "." are invisible.
     */

attribute_hidden bool R_HiddenFile(const char *name)
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
    /* Rconnection can have a mode of 4 chars plus a ccs= setting plus a null; we might
     * add one char if neither b nor t is specified. */
    static char fixedmode[20];
    fixedmode[19] = '\0';
    strncpy(fixedmode, mode, 19);
    if (!strpbrk(fixedmode, "bt")) {
	strcat(fixedmode, "t");
    }
    return fixedmode;
}

static wchar_t * wcfixmode(const wchar_t *mode)
{
    static wchar_t wcfixedmode[20];
    wcfixedmode[19] = L'\0';
    wcsncpy(wcfixedmode, mode, 19);
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
   <FIXME> since Windows 2000 they could be UTF-16LE
*/

#if defined(Win32)

#define BSIZE 100000
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand)
{
    static wchar_t filename[BSIZE+1];
    void *obj;
    const char *from = "", *inbuf;
    char *outbuf;
    size_t inb, outb, res;

    if(!strlen(CHAR(fn))) {
	wcscpy(filename, L"");
	return filename;
    }
    if(IS_LATIN1(fn))
#ifdef HAVE_ICONV_CP1252
	from = "CP1252";
#else
	from = "latin1";
#endif
    if(IS_UTF8(fn)) from = "UTF-8";
    if(IS_BYTES(fn)) error(_("encoding of a filename cannot be 'bytes'"));
    obj = Riconv_open("UTF-16LE", from);
    if(obj == (void *)(-1))
	error(_("unsupported conversion from '%s' in codepage %d"),
	      from, localeCP);

    if(expand) inbuf = R_ExpandFileName(CHAR(fn)); else inbuf = CHAR(fn);

    inb = strlen(inbuf)+1; outb = 2*BSIZE;
    outbuf = (char *) filename;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(inb > 0) error(_("file name conversion problem -- name too long?"));
    if(res == -1) error(_("file name conversion problem"));

    return filename;
}

FILE *R_wfopen(const wchar_t *filename, const wchar_t *mode)
{
    return filename ? _wfopen(filename, wcfixmode(mode)) : NULL;
}


FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    wchar_t wmode[10];

    if(fn == NA_STRING) return NULL;
    mbstowcs(wmode, fixmode(mode), 10);
    return _wfopen(filenameToWchar(fn, expand), wmode);
}
#else
FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    const void *vmax = vmaxget();
    const char *filename = translateCharFP(fn), *res;
    if(fn == NA_STRING || !filename) return NULL;
    if(expand) res = R_ExpandFileName(filename);
    else res = filename;
    vmaxset(vmax);
    return fopen(res, mode);
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

/* This is a primitive (with no arguments) */
attribute_hidden SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarLogical( (R_Interactive) ? 1 : 0 );
}

attribute_hidden SEXP do_tempdir(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    bool check = asBool2(CAR(args), call);
    if(check && !R_isWriteableDir(R_TempDir)) {
	R_TempDir = NULL;
	R_reInitTempDir(/* die_on_fail = */ FALSE);
    }
    return mkString(R_TempDir);
}


attribute_hidden SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans, pattern, fileext, tempdir;
    const char *tn, *td, *te;
    char *tm;
    int i, n1, n2, n3, slen;

    checkArity(op, args);
    pattern = CAR(args); n1 = length(pattern); args = CDR(args);
    tempdir = CAR(args); n2 = length(tempdir); args = CDR(args);
    fileext = CAR(args); n3 = length(fileext);
    if (!isString(pattern))
	error(_("invalid filename pattern"));
    if (!isString(tempdir))
	error(_("invalid '%s' value"), "tempdir");
    if (!isString(fileext))
	error(_("invalid file extension"));
    if (n1 < 1)
	error(_("no 'pattern'"));
    if (n2 < 1)
	error(_("no 'tempdir'"));
    if (n3 < 1)
	error(_("no 'fileext'"));
    slen = (n1 > n2) ? n1 : n2;
    slen = (n3 > slen) ? n3 : slen;
    PROTECT(ans = allocVector(STRSXP, slen));
    for(i = 0; i < slen; i++) {
	tn = translateCharFP( STRING_ELT( pattern , i%n1 ) );
	td = translateCharFP( STRING_ELT( tempdir , i%n2 ) );
	te = translateCharFP( STRING_ELT( fileext , i%n3 ) );
	/* try to get a new file name */
	tm = R_tmpnam2(tn, td, te);
	SET_STRING_ELT(ans, i, mkChar(tm));
	if(tm) free(tm);
    }
    UNPROTECT(1);
    return (ans);
}

FILE *R_popen(const char *command, const char *type)
{
    FILE *fp;
#ifdef OLD__APPLE__
    /* Luke recommends this to fix PR#1140 */
    /* As of 2016-01-06 on El Capitan this may no longer be needed -- LT */
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

#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

int R_system(const char *command)
{
    int res;
#ifdef __APPLE__
# ifdef OLD__APPLE__
    /* Luke recommends this to fix PR#1140 */
    /* As of 2016-01-06 on El Capitan this may no longer be needed -- LT */
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
# endif
#ifdef HAVE_AQUA
    if(ptr_CocoaSystem) res = ptr_CocoaSystem(command); else
#endif
    res = system(command);
# ifdef OLD__APPLE__
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
# endif
#else // not APPLE
    res = system(command);
#endif
#ifdef HAVE_SYS_WAIT_H
    if (WIFEXITED(res)) res = WEXITSTATUS(res);
#else
    /* assume that this is shifted if a multiple of 256 */
    if ((res % 256) == 0) res = res/256;
#endif
    if (res == -1) {
	/* this means that system() failed badly - it didn't
	   even get to try to run the shell */
	warning(_("system call failed: %s"), strerror(errno));
	/* R system() is documented to return 127 on failure, and a lot of
	   code relies on that - it will misinterpret -1 as success */
	res = 127;
    }
    return res;
}

#if defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
#elif defined(Win32)
/* _wenviron is declared in stdlib.h */
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h> /* _wgetenv etc */
#else
extern char ** environ;
#endif

// .Internal(Sys.getenv(x, unset))
attribute_hidden SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    SEXP ans;

    checkArity(op, args);

    if (!isString(CAR(args)))
	error(_("wrong type for argument"));

    if (!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error(_("wrong type for argument"));

    i = LENGTH(CAR(args));
    if (i == 0) { // full list of environment variables
#ifdef Win32
	int n = 0, N;
	wchar_t **w;
	for (i = 0, w = _wenviron; *w != NULL; i++, w++)
	    n = max(n, wcslen(*w));
	N = 4*n+1;
	char buf[N];
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, w = _wenviron; *w != NULL; i++, w++) {
	    wcstoutf8(buf, *w, sizeof(buf));
	    SET_STRING_ELT(ans, i, mkCharCE(buf, CE_UTF8));
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
#ifdef Win32
	    const wchar_t *wnm = wtransChar(STRING_ELT(CAR(args), j));
	    wchar_t *w = _wgetenv(wnm);
	    if (w == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		int n = wcslen(w), N = 4*n+1; /* UTF-16 maps to <= 4 UTF-8 */
		R_CheckStack2(N);
		char buf[N];
		wcstoutf8(buf, w, sizeof(buf));
		SET_STRING_ELT(ans, j, mkCharCE(buf, CE_UTF8));
	    }
#else
	    char *s = getenv(translateChar(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		SEXP tmp;
		if(known_to_be_latin1) tmp = mkCharCE(s, CE_LATIN1);
		else if(known_to_be_utf8) tmp = mkCharCE(s, CE_UTF8);
		else tmp = mkChar(s);
		SET_STRING_ELT(ans, j, tmp);
	    }
#endif
	}
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef Win32
static int Rwputenv(const wchar_t *nm, const wchar_t *val)
{
    wchar_t *buf;
    buf = (wchar_t *) malloc((wcslen(nm) + wcslen(val) + 2) * sizeof(wchar_t));
    if(!buf) return 1;
    /* previously wsprintfW, which had a limit of 1024 chars */
    wcscpy(buf, nm); wcscat(buf, L"="); wcscat(buf, val);
    if(_wputenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#elif !defined(HAVE_SETENV) && defined(HAVE_PUTENV)
static int Rputenv(const char *nm, const char *val)
{
    char *buf;
    size_t sz = (strlen(nm) + strlen(val) + 2) * sizeof(char);
    buf = (char *) malloc(sz);
    if(!buf) return 1;
    snprintf(buf, sz, "%s=%s", nm, val);
    if(putenv(buf)) return 1;
    /* no free here: storage remains in use */
    return 0;
}
#endif


// .Internal(Sys.setenv(nm, val)) : (nm_1=val_1, nm_2=val_2, ..., nm_<n>=val_<n>)
attribute_hidden SEXP do_setenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_PUTENV) || defined(HAVE_SETENV)

    checkArity(op, args);

    SEXP nm = CAR(args);
    if (!isString(nm))
	error(_("wrong type for argument"));
    SEXP val = CADR(args);
    if (!isString(val))
	error(_("wrong type for argument"));
    if(LENGTH(nm) != LENGTH(val))
	error(_("'%s' and '%s' are of different lengths"), "names", "val");

    int i, n = LENGTH(val);
    SEXP ans = PROTECT(allocVector(LGLSXP, n));
#ifdef HAVE_SETENV
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = setenv(translateChar(STRING_ELT(nm, i)),
				 translateChar(STRING_ELT(val, i)),
				 1) == 0;
#elif defined(Win32)
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rwputenv(wtransChar(STRING_ELT(nm, i)),
				   wtransChar(STRING_ELT(val, i))) == 0;
#else
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = Rputenv(translateChar(STRING_ELT(nm, i)),
				  translateChar(STRING_ELT(val, i))) == 0;
#endif
    UNPROTECT(1);
    return ans;
#else
    error(_("'Sys.setenv' is not available on this system"));
    return R_NilValue; /* -Wall */
#endif
}

// .Internal(Sys.unsetenv(nm))
attribute_hidden SEXP do_unsetenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP nm = CAR(args);
    if (!isString(nm))
	error(_("wrong type for argument"));
    int i, n = LENGTH(nm);

#if defined(HAVE_UNSETENV) || defined(HAVE_PUTENV_UNSET) || defined(HAVE_PUTENV_UNSET2)
#ifdef HAVE_UNSETENV
    for (i = 0; i < n; i++) unsetenv(translateChar(STRING_ELT(nm, i)));
#elif defined(HAVE_PUTENV_UNSET)
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s",  translateChar(STRING_ELT(nm, i)));
	putenv(buf);
    }
#elif defined(HAVE_PUTENV_UNSET2)
# ifdef Win32
    for (i = 0; i < n; i++) {
	const wchar_t *w = wtransChar(STRING_ELT(nm, i));
	wchar_t buf[2*wcslen(w)];
	wcscpy(buf, w);
	wcscat(buf, L"=");
	_wputenv(buf);
    }
# else
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s=", translateChar(STRING_ELT(nm, i)));
	putenv(buf);
    }
# endif
#endif

#elif defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    warning(_("this system cannot unset environment variables: setting to \"\""));
    n = LENGTH(nm);
    for (i = 0; i < n; i++) {
#ifdef HAVE_SETENV
	setenv(translateChar(STRING_ELT(nm, i)), "", 1);
#else
	Rputenv(translateChar(STRING_ELT(nm, i)), "");
#endif
    }

#else
    warning(_("'Sys.unsetenv' is not available on this system"));
#endif

    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    for (i = 0; i < n; i++)
	LOGICAL(ans)[i] = !getenv(translateChar(STRING_ELT(nm, i)));
    UNPROTECT(1);
    return ans;
}

#include <iconv.h>

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

// Copied from platform.c (only used condiitonally there)
// case-insensitive string comparison
int static R_strieql(const char *a, const char *b)
{
    while (*a && *b && toupper(*a) == toupper(*b)) { a++; b++; }
    return (*a == 0 && *b == 0);
}


#include "RBufferUtils.h"

/* iconv(x, from, to, sub, mark) */
attribute_hidden SEXP do_iconv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args), si;
    void * arg_obj = (iconv_t)-1;
    void * latin1_obj = (iconv_t)-1;
    void * utf8_obj = (iconv_t)-1;
    const char *inbuf;
    char *outbuf;
    const char *sub; // null for no substitution.
    size_t inb, outb, res;
    size_t inp_unit_size = 0; /* uninitialized */
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    bool isRawlist = false;

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
	int mark, toRaw;
	const char *from, *to;
	bool isLatin1 = false, isUTF8 = false;

	args = CDR(args);
	if(!isString(CAR(args)) || length(CAR(args)) != 1)
	    error(_("invalid '%s' argument"), "from");
	from = CHAR(STRING_ELT(CAR(args), 0)); /* ASCII */
	args = CDR(args);
	if(!isString(CAR(args)) || length(CAR(args)) != 1)
	    error(_("invalid '%s' argument"), "to");
	to = CHAR(STRING_ELT(CAR(args), 0));
	args = CDR(args);
	if(!isString(CAR(args)) || length(CAR(args)) != 1)
	    error(_("invalid '%s' argument"), "sub");
	if(STRING_ELT(CAR(args), 0) == NA_STRING) sub = NULL;
	else sub = translateChar(STRING_ELT(CAR(args), 0));
	args = CDR(args);
	mark = asLogical(CAR(args));
	if(mark == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "mark");
	args = CDR(args);
	toRaw = asLogical(CAR(args));
	if(toRaw == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "toRaw");
	/* some iconv's allow "UTF8", but GNU libiconv does not */
	if(R_strieql(from, "UTF8")) from = "UTF-8";
	if(R_strieql(to, "UTF8")) to = "UTF-8";
	if(streql(to, "UTF-8")) isUTF8 = TRUE;
	if(R_strieql(to, "latin1") || R_strieql(to, "ISO_8859-1")
	    || R_strieql(to, "CP1252")) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_latin1) isLatin1 = TRUE;
	if(streql(to, "") && known_to_be_utf8) isUTF8 = TRUE;
#ifdef OS_MUSL
	if(R_strieql(from, "latin-2") || R_strieql(from, "latin2") )
	    from = "iso88592";
	if(R_strieql(to, "latin-2") || R_strieql(to, "latin2") )
	    to = "iso88592";
	/* the following is redundant (musl does know about the latin9 alias) */
	if(R_strieql(from, "latin-9") || R_strieql(from, "latin9") )
	    from = "iso885915";
	if(R_strieql(to, "latin-9") || R_strieql(to, "latin9") )
	    to = "iso885915";
#endif
	isRawlist = (TYPEOF(x) == VECSXP);
	if(isRawlist) {
	    if(toRaw)
		PROTECT(ans = duplicate(x));
	    else {
		PROTECT(ans = allocVector(STRSXP, LENGTH(x)));
		SHALLOW_DUPLICATE_ATTRIB(ans, x);
	    }
	} else {
	    if(TYPEOF(x) != STRSXP)
		error(_("'x' must be a character vector"));
	    if(toRaw) {
		PROTECT(ans = allocVector(VECSXP, LENGTH(x)));
		SHALLOW_DUPLICATE_ATTRIB(ans, x);
	    } else
		PROTECT(ans = duplicate(x));
	}
	R_AllocStringBuffer(0, &cbuff);  /* 0 -> default */
	for(R_xlen_t i = 0; i < XLENGTH(x); i++) {
	    if (isRawlist) {
		si = VECTOR_ELT(x, i);
		if (TYPEOF(si) == NILSXP) {
		    if (!toRaw) SET_STRING_ELT(ans, i, NA_STRING);
		    continue;
		} else if (TYPEOF(si) != RAWSXP)
		    error(_("'x' must be a character vector or a list of NULL or raw vectors"));
	    } else {
		si = STRING_ELT(x, i);
		if (si == NA_STRING) {
		    if(!toRaw) SET_STRING_ELT(ans, i, NA_STRING);
		    continue;
		}
	    }
	    void * obj = (iconv_t)-1;
	    bool fromUTF8 = false;

	    /* With 'from = ""', encoding flags are used in preference
	       of native encoding.

	       FIXME: Should we go further and ignore "from" with any non-bytes,
	              non-raw input? */
	    if (!isRawlist && IS_UTF8(si) && streql(from, "")) {
		if (utf8_obj == (iconv_t)-1) {
		    utf8_obj = Riconv_open(to, "UTF-8");
		    if(utf8_obj == (iconv_t)(-1))
		#ifdef Win32
			error(_("unsupported conversion from '%s' to '%s' in codepage %d"),
			      "UTF-8", to, localeCP);
		#else
		    {
			// musl does not support ASCII//TRANSLIT but has
			// similar ASCII subsituting with *
			// In case there are others, we set sub here.
			if(streql(to, "ASCII//TRANSLIT")) {
			    to = "ASCII";
			    utf8_obj = Riconv_open(to, "UTF-8");
			    if(!sub) sub = "c99";
			}
			if(utf8_obj == (iconv_t)(-1))
			    error(_("unsupported conversion from '%s' to '%s'"),
				  "UTF-8", to);
		    }
		#endif
		}
		obj = utf8_obj;
		fromUTF8 = TRUE;
	    } else if (!isRawlist && IS_LATIN1(si) && streql(from, "")) {
		if (latin1_obj == (iconv_t)-1) {
		    latin1_obj = Riconv_open(to, "latin1");
		    if(latin1_obj == (iconv_t)(-1))
		#ifdef Win32
			error(_("unsupported conversion from '%s' to '%s' in codepage %d"),
			      "latin1", to, localeCP);
		#else
		    {
			if(streql(to, "ASCII//TRANSLIT")) {
			    to = "ASCII";
			    latin1_obj = Riconv_open(to, "latin1");
			    if(!sub) sub = "?";
			}
			if(latin1_obj == (iconv_t)(-1))
			    error(_("unsupported conversion from '%s' to '%s'"),
				  "latin1", to);			   
		    }
		#endif
		}
		obj = latin1_obj;
	    } else {
		if (arg_obj == (iconv_t)-1) {
		    arg_obj = Riconv_open(to, from);
		    if(arg_obj == (iconv_t)(-1))
		#ifdef Win32
			error(_("unsupported conversion from '%s' to '%s' in codepage %d"),
			      from, to, localeCP);
		#else
		    {
			if(streql(to, "ASCII//TRANSLIT")) {
			    to = "ASCII";
			    arg_obj = Riconv_open(to, from);
			    if(!sub) sub = "?";
			}
			if(arg_obj == (iconv_t)(-1))
			    error(_("unsupported conversion from '%s' to '%s'"),
				  from, to);			   
		    }
		#endif
		}
		obj = arg_obj;
		fromUTF8 = streql(from, "UTF-8")
		           || (streql(from, "") && known_to_be_utf8);
		           /* FIXME: utf8locale? as Riconv doesn't handle
		                     known_to_be_utf8 */
	    }
	top_of_loop:
	    inbuf = isRawlist ? (const char *) RAW(si) : CHAR(si);
	    inb = LENGTH(si);
	    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
	    /* First initialize output */
	    Riconv (obj, NULL, NULL, &outbuf, &outb);
	next_char:
	    /* Then convert input  */
	    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
	    *outbuf = '\0';
	    /* other possible error conditions are
	       incomplete and invalid multibyte chars */
	    if(res == -1 && errno == E2BIG) {
		R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		goto top_of_loop;
	    } else if(res == -1 && sub &&
		      (errno == EILSEQ || errno == EINVAL)) {
		/* it seems this gets thrown for non-convertible input too */
		/* EINVAL returned for invalid input on macOS with system
		   libiconv */
		/*
		  Should re-set with a stateful encoding, but some iconv
		  implementations forget byte-order learned from BOM.

		res = Riconv(obj, NULL, NULL, &outbuf, &outb);	
		if (res == -1 && errno == E2BIG) {
		    R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
		    goto top_of_loop;
		}
		*/
		if(fromUTF8 && streql(sub, "Unicode")) {
		    if(outb < 13) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    wchar_t wc;
		    ssize_t clen = utf8toucs(&wc, inbuf);
		    if(clen > 0 && inb >= clen) {
			R_wchar_t ucs;
			if (IS_HIGH_SURROGATE(wc))
			    ucs = utf8toucs32(wc, inbuf);
			else
			    ucs = (R_wchar_t) wc;
			inbuf += clen; inb -= clen;
			if(ucs < 65536) {
			    // gcc 7 objected to this with unsigned int
			    snprintf(outbuf, 9, "<U+%04X>", (unsigned short) ucs);
			    outbuf += 8; outb -= 8;
			} else {
			    /* R_wchar_t is unsigned int on Windows, 
			       otherwise wchar_t (usually int).
			       In any case Unicode points <= 0x10FFFF
			       so one could argue against zero-padding here.
			    */
			    snprintf(outbuf, 13, "<U+%04X>", (unsigned int) ucs);
			    size_t l = strlen(outbuf);
			    outbuf += l; outb -= l;
			}
		    }
		    goto next_char;
		} else if(fromUTF8 && streql(sub, "c99")) {
		    if(outb < 11) {
			R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
			goto top_of_loop;
		    }
		    wchar_t wc;
		    ssize_t clen = utf8toucs(&wc, inbuf);
		    if(clen > 0 && inb >= clen) {
			R_wchar_t ucs;
			if (IS_HIGH_SURROGATE(wc))
			    ucs = utf8toucs32(wc, inbuf);
			else
			    ucs = (R_wchar_t) wc;
			inbuf += clen; inb -= clen;
			if(ucs < 65536) {
			    // gcc 7 objected to this with unsigned int
			    snprintf(outbuf, 7, "\\u%04x", (unsigned short) ucs);
			    outbuf += 6; outb -= 6;
			} else {
			    /* R_wchar_t is unsigned int on Windows, 
			       otherwise wchar_t (usually int).
			       In any case Unicode points <= 0x10FFFF
			    */
			    snprintf(outbuf, 11, "\\U%08x", (unsigned int) ucs);
			    outbuf += 10; outb -= 10;
			}
		    }
		    goto next_char;
		} else {
		    if (!inp_unit_size) {
			if (!strncasecmp(from, "UTF-16", 6) ||
			    !strncasecmp(from, "UCS-2", 5))
			    inp_unit_size = 2;
			else if (!strncasecmp(from, "UTF-32", 6) ||
			           !strncasecmp(from, "UCS-4", 5))
			    inp_unit_size = 4;
			else
			    /* encodings supported directly by CHARSXP,
			       including the native encoding, all use
			       unit size 1 */
			    inp_unit_size = 1;
		    } 
		    for(int i = 0; i < inp_unit_size && inb > 0; i++) {
			if(strcmp(sub, "byte") == 0) {
			    if(outb < 5) {
				R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
				goto top_of_loop;
			    }
			    snprintf(outbuf, 5, "<%02x>",
			             (unsigned char)*inbuf);
			    outbuf += 4; outb -= 4;
			} else {
			    size_t sub_len = strlen(sub);
			    if(outb < sub_len) {
				R_AllocStringBuffer(2*cbuff.bufsize, &cbuff);
				goto top_of_loop;
			    }
			    if (sub_len)
				memcpy(outbuf, sub, sub_len);
			    outbuf += sub_len; outb -= sub_len;
			}
			inbuf++; inb--;
		    }
		}
		goto next_char;
	    }

	    if(toRaw) {
		if(res != -1 && inb == 0) {
		    size_t nout = cbuff.bufsize - 1 - outb;
		    SEXP el = allocVector(RAWSXP, nout);
		    if (nout)
			memcpy(RAW(el), cbuff.data, nout);
		    SET_VECTOR_ELT(ans, i, el);
		} /* otherwise is already NULL */
	    } else {
		if(res != -1 && inb == 0) {
		    cetype_t ienc = CE_NATIVE;

		    size_t nout = cbuff.bufsize - 1 - outb;
		    if(mark) {
			if(isLatin1) ienc = CE_LATIN1;
			else if(isUTF8) ienc = CE_UTF8;
		    }
		    /* FIXME: use "bytes" unless CE_NATIVE matches to? */
		    SET_STRING_ELT(ans, i,
				   mkCharLenCE(cbuff.data, (int) nout, ienc));
		} else SET_STRING_ELT(ans, i, NA_STRING);
	    }
	}
	if (latin1_obj != (iconv_t)-1) Riconv_close(latin1_obj);
	if (utf8_obj != (iconv_t)-1) Riconv_close(utf8_obj);
	if (arg_obj != (iconv_t)-1) Riconv_close(arg_obj);
	R_FreeStringBuffer(&cbuff);
    }
    UNPROTECT(1);
    return ans;
}

#define CHECK_CHARSXP(x) do { \
    SEXP __x__ = (x);            \
    if(TYPEOF(__x__) != CHARSXP) \
	error(_("'%s' must be called on a CHARSXP, but got '%s'"), \
	      __func__, R_typeToChar(__x__));			   \
} while(0);

cetype_t getCharCE(SEXP x)
{
    CHECK_CHARSXP(x);
    if(IS_UTF8(x)) return CE_UTF8;
    else if(IS_LATIN1(x)) return CE_LATIN1;
    else if(IS_BYTES(x)) return CE_BYTES;
    else return CE_NATIVE;
}

// In Rinternals.h
Rboolean charIsASCII(SEXP x)
{
    CHECK_CHARSXP(x);
    return IS_ASCII(x) ? TRUE : FALSE;
}

// In Rinternals.h
Rboolean charIsUTF8(SEXP x)
{
    CHECK_CHARSXP(x);
    if (IS_ASCII(x) || IS_UTF8(x)) return TRUE;
    if (IS_LATIN1(x) || IS_BYTES(x) || !utf8locale || x == NA_STRING)
	return FALSE;
    return TRUE;
}

// In Rinternals.h
Rboolean charIsLatin1(SEXP x)
{
    CHECK_CHARSXP(x);
    if (IS_ASCII(x) || IS_LATIN1(x)) return TRUE;
    if (!latin1locale || IS_UTF8(x) || IS_BYTES(x) || x == NA_STRING)
	return FALSE;
    return TRUE;
}

#ifdef __APPLE__
/* Work-around for system libiconv in macOS 14.1. When an invalid input byte
   is encountered while converting, subsequent valid bytes may be reported as
   invalid and libiconv may crash R due to an assertion failure e.g. once
   then converting an empty input. The problem does not seem to
   happen when the converter is re-set after error. The problem has been
   observed in libiconv-86 (which came with macOS 14.1) but no longer
   in libiconv-92 (macOS 14.2).

   While often one should reset the converter in such situation in order to
   support stateful encodings properly, the problem has been seen even when
   converting from UTF-8 to UTF-8 (UTF-8 is stateless).

   This work-around automatically re-sets the converter in Riconv in case
   of error for stateless encodings. It is enabled only based on a runtime
   check for the issue (also to avoid to running into problems with BOM
   handling, see R_MACOS_LIBICONV_HANDLE_BOM). 

   Can be disabled via env. variable _R_ICONV_RESET_AFTER_ERROR_. */
# define R_MACOS_LIBICONV_RESET_AFTER_ERROR

/* A hack to detect and undo transliteration (experimental, likely to change
   or be removed).  While POSIX says that iconv should transliterate or
   substitute valid input characters not representable in the output
   encoding, this typically is not the case.  Instead, non-representable
   characters usually cause an error (EILSEQ or even EINVAL) and R ended up
   depending on that.

   macOS 14.1 has a libiconv implementation which transliterates many
   characters.

   This feature, currently only available for stateless conversions, detects
   transliteration by converting the result back to the original encoding,
   compares with the original and then re-runs the conversion only to the
   to-be-transliterated character. This wouldn't work in cases when the
   conversion isn't unique, but such cases are unlikely (note implementations
   of iconv, including libiconv in macOS 14.1, do not support decomposed
   forms).
   
   Enabled at runtime via env. variable _R_ICONV_UNDO_TRANSLITERATION_. */
# define R_MACOS_LIBICONV_UNDO_TRANSLITERATION

/* Work-around for libiconv in macOS 14.1.  This version of
   libiconv accepts BOM for UTF-16, but on error (including EINVAL when it
   is not given enough input, which is a normal situation in processing a
   stream) it forgets the byte-order it has learned from the BOM. This
   problem was observed in libiconv-86 (which came with macOS 14.1) and
   still exists in libiconv-107 (in macOS 15.0).

   Also, in some cases iconv forgets the BOM on reset, i.e.
   iconv(cd, NULL, NULL, NULL, NULL) and then starts producing unexpected
   results. The default is usually big-endian ordering even on little-endian
   machines, so this particularly causes trouble when reading inputs
   produced on Windows.  The work-around falls back to UTF-16LE/BE or 
   UTF32-LE/BE based on the BOM, if present. This problem has been seen
   already in libiconv-64 (macOS 13.5) and is present also in some other
   iconv implementations, where the byte-order learned from the BOM is
   incorrectly treated as being part of the encoding state (but UTF-16
   and UTF-32 is stateless and the byte-order learned from the BOM 
   should be write-once property of the conversion descriptor). */
# define R_MACOS_LIBICONV_HANDLE_BOM
#endif

#ifndef R_MACOS_LIBICONV_WORKAROUND
# if defined(R_MACOS_LIBICONV_RESET_AFTER_ERROR) \
     || defined(R_MACOS_LIBICONV_UNDO_TRANSLITERATION) \
     || defined(R_MACOS_LIBICONV_HANDLE_BOM)
#  define R_MACOS_LIBICONV_WORKAROUND
# endif
#endif

#ifdef R_MACOS_LIBICONV_WORKAROUND
typedef struct {
    iconv_t cd;
    bool reset_after_error;
    iconv_t cd_back;
    bool undo_transliteration;
    size_t buflen;
    char *buf;
    bool handle_bom;
    size_t bomlen;
    char *tocode;
    char start[4];
    size_t startlen;
} Riconv_cd;

static bool is_stateful(const char *code)
{
    /* list from libiconv 1.17, but names are system-specific */
    static char *stateful[] = {
        "utf7", "UTF-7", "UNICODE-1-1-UTF-7", "csUnicode11UTF7", "cp1255",
        "CP1255", "WINDOWS-1255", "MS-HEBR", "cp1258", "CP1258",
        "WINDOWS-1258", "tcvn", "TCVN", "TCVN-5712", "TCVN5712-1",
        "TCVN5712-1:1993", "iso2022_jp", "ISO-2022-JP", "csISO2022JP",
        "iso2022_jp1", "ISO-2022-JP-1", "iso2022_jp2", "ISO-2022-JP-2",
        "csISO2022JP2", "iso2022_jpms", "ISO-2022-JP-MS", "CP50221",
        "iso2022_cn", "ISO-2022-CN", "csISO2022CN", "iso2022_cn_ext",
        "ISO-2022-CN-EXT", "hz", "HZ", "HZ-GB-2312", "big5hkscs1999",
        "BIG5-HKSCS:1999", "big5hkscs2001", "BIG5-HKSCS:2001",
        "big5hkscs2004", "BIG5-HKSCS:2004", "big5hkscs2008", "BIG5-HKSCS",
        "BIG5HKSCS", "BIG5-HKSCS:2008", "iso2022_kr", "ISO-2022-KR",
        "csISO2022KR", "euc_jisx0213", "EUC-JISX0213", "EUC-JIS-2004",
        "shift_jisx0213", "SHIFT_JISX0213", "SHIFT_JIS-2004", "iso2022_jp3",
        "ISO-2022-JP-3", "ISO-2022-JP-2004", NULL
    };

    if (!strcasecmp(code, "UTF-8") || !strcasecmp(code, "ISO-8859-1") ||
        !strcasecmp(code, "latin1"))
	return false;

    /* if performance of this becomes a problem, there could be a cache of
       recently used encodings or/and a perfect hashing function */
    for(int i = 0; stateful[i] ; i++)
	if (!strcasecmp(code, stateful[i]))
	    return true;
    return false;
}

# ifdef R_MACOS_LIBICONV_UNDO_TRANSLITERATION
static bool is_unicode(const char *code)
{
    /* list from libiconv 1.17, but names are system-specific */
    static char *unicode[] = {
        "UTF-8",
        "UCS-4", "UCS-4BE", "UCS-4LE",
        "UTF-16", "UTF-16BE", "UTF-16LE",
        "UTF-32", "UTF-32BE", "UTF-32LE",
        "UTF-7",
        "C99", "JAVA", NULL
    };

    for(int i = 0; unicode[i] ; i++)
	if (!strcasecmp(code, unicode[i]))
	    return true;
    return false;
}
# endif 

/* only for debugging, may be removed */
static int macos_libiconv_verbose(void) {
    static int verbose = -1;

    if (verbose == -1) {
	char *p = getenv("_R_ICONV_VERBOSE_");
	verbose = p && StringTrue(p);
    }
    return verbose;
}

static int iconv_close_internal(Riconv_cd *rcd)
{
    int res = 0;

    if (rcd->cd != (iconv_t)-1)
	res = iconv_close(rcd->cd);

# ifdef R_MACOS_LIBICONV_HANDLE_BOM
    if (rcd->handle_bom && rcd->tocode)
	free(rcd->tocode);
# endif

# ifdef R_MACOS_LIBICONV_UNDO_TRANSLITERATION
    if (rcd->undo_transliteration) {
	if (rcd->buf)
	    free(rcd->buf);
	if (rcd->cd_back != (iconv_t)-1) {
	    if (iconv_close(rcd->cd_back))
		res = -1;
	}
    }
# endif

    free(rcd);
    return res;
}
#endif

#ifdef R_MACOS_LIBICONV_HANDLE_BOM

/* Try converting input in UTF-16 iteratively (first 5 bytes, then the
   remaining byte) to UTF-8. The result should be "23".
   See fails_iteratively_with_bom().
 
   1 when iconv "successfully" returns wrong result
   -1 on (other) error
   0 on no error and correct result
*/
static int fails_iteratively_when_incomplete(char *input)
{
    iconv_t cd = iconv_open("UTF-8", "UTF-16");
    if (cd == (iconv_t)-1)
	return -1;

    char output[6]; // UTF-8-BOM (possibly) + "23" + NUL
    char *inbuf = input;
    size_t inbytesleft = 5;
    char *outbuf = output;
    size_t outbytesleft = 6;

    size_t res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if (res != (size_t)-1 || errno != EINVAL) {
	iconv_close(cd);
	return -1;
    }

    inbytesleft = 6 - (5 - inbytesleft);
    res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    iconv_close(cd);
    if (res == (size_t)-1)
	return -1;
    *outbuf = 0;

    /* remove UTF-8 BOM if present (unlikely) */
    int offset = 0;
    if ((outbuf - output >= 3) && !memcmp(output, "\xef\xbb\xbf", 3))
	offset = 3;

    if (!memcmp(output + offset, "23", 3))
	return 0; /* success, conversion works iteratively */
    else
	return 1; /* wrong result */ 
}

/* Try converting input in UTF-16 iteratively (first 4 bytes, then 
   re-set, then remaining two bytes) to UTF-8. The result should be "23".
   See fails_iteratively_with_bom().
 
   1 when iconv "successfully" returns wrong result
   -1 on (other) error
   0 on no error and correct result
*/
static int fails_iteratively_with_reset(char *input)
{
    iconv_t cd = iconv_open("UTF-8", "UTF-16");
    if (cd == (iconv_t)-1)
	return -1;

    char output[6]; // UTF-8-BOM (possibly) + "23" + NUL
    char *inbuf = input;
    size_t inbytesleft = 4;
    char *outbuf = output;
    size_t outbytesleft = 6;

    size_t res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if (res == (size_t)-1) {
	iconv_close(cd);
	return -1;
    }
    iconv(cd, NULL, NULL, NULL, NULL);

    inbytesleft = 6 - (4 - inbytesleft);
    res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    iconv_close(cd);
    if (res == (size_t)-1)
	return -1;
    *outbuf = 0;

    /* remove UTF-8 BOM if present (unlikely) */
    int offset = 0;
    if ((outbuf - output >= 3) && !memcmp(output, "\xef\xbb\xbf", 3)) {
	if (macos_libiconv_verbose()) 
	    fprintf(stderr, "ICONV: UTF-8 with BOM produced.\n");
	offset = 3;
    }

    if (!memcmp(output + offset, "23", 3))
	return 0; /* success, conversion works iteratively */
    else
	return 1; /* wrong result */ 
}

/* Test whether iterative conversion from UTF-16 with a BOM to UTF-8 fails
   by producing an incorrect result. This has been observed in Apple libiconv
   on macOS. A runtime test is used as that version cannot be reliably
   detected.

   1 when iconv "successfully" returns wrong result
   -1 on (other) error
   0 on no error and correct result
*/
static int fails_iteratively_with_bom(void)
{
    unsigned words[] = { 0xfeff /* BOM */, 0x32 /* 2 */, 0x33 /* 3 */};
    char big[6];
    char little[6];

    for(int i = 0; i < 6; i += 2) {
	unsigned w = words[i/2];
	big[i] = little[i+1] = (char) (w >> 8);
	big[i+1] = little[i] = w & 0xff;
    }

    int ile = fails_iteratively_when_incomplete(little);
    int ibe = fails_iteratively_when_incomplete(big);

    int rle = fails_iteratively_with_reset(little);
    int rbe = fails_iteratively_with_reset(big);

    if (macos_libiconv_verbose()) {
	fprintf(stderr, "ICONV: Fails iteratively when incomplete (LE): %d.\n",
	        ile);
	fprintf(stderr, "ICONV: Fails iteratively when incomplete (BE): %d.\n",
	        ibe);
	fprintf(stderr, "ICONV: Fails iteratively with reset (LE): %d.\n",
	        rle);
	fprintf(stderr, "ICONV: Fails iteratively with reset (BE): %d.\n",
	        rbe);
    }

    if (ile == 1 || ibe == 1 || rle == 1 || rbe == 1)
	return 1;
    if (ile == -1 || ibe == -1 || rle == -1 || rbe == -1)
	return -1;
    return 0;
}
#endif

#ifdef R_MACOS_LIBICONV_RESET_AFTER_ERROR

/* Test whether iconv, after encountering an invalid byte in input, keeps
   incorrectly reporting as invalid also additional valid bytes. This has
   been observed in Apple libiconv on macOS. A runtime test is used as that
   version cannot be reliably detected.

   1 when iconv "successfully" returns wrong result
   -1 on (other) error
   0 on no error and correct result */
static int breaks_after_invalid_byte(void)
{
    char *input = "1" "\xFC" "3456789";

    iconv_t cd = iconv_open("UTF-8", "UTF-8");
    if (cd == (iconv_t)-1)
        return -1;

    char output[10];
    char *inbuf = input;
    size_t inbytesleft = 10;
    char *outbuf = output;
    size_t outbytesleft = 10;

    size_t res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    if (res != (size_t)-1 || (errno != EILSEQ && errno != EINVAL)
        || outbytesleft != 9 || inbytesleft != 9) {

	iconv_close(cd);
	return -1;
    }

    /* advance over invalid byte */
    inbuf++;
    inbytesleft--;

    res = iconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    iconv_close(cd);
    if (res == (size_t)-1 && (errno == EINVAL || errno == EILSEQ))
        return 1; /* input incorrectly reported as invalid */
    *outbuf = '\0';
    if (memcmp(output, "13456789", 9)) {
	if (macos_libiconv_verbose())
	    fprintf(stderr, "ICONV: Incorrect conversion with invalid byte.\n");
	return -1; /* wrong result for other reason */
    }

    return 0; /* success, handling invalid bytes works */
}
#endif

static void *iconv_open_internal(const char *tocode, const char *fromcode)
{
#ifndef R_MACOS_LIBICONV_WORKAROUND
    return iconv_open(tocode, fromcode);
#else
    iconv_t cd = iconv_open(tocode, fromcode);
    if (cd == (iconv_t)-1)
	return cd;

    Riconv_cd *rcd = malloc(sizeof(Riconv_cd));
    if (!rcd) {
	errno = ENOMEM;
	return (void *)(iconv_t)-1;
    }
    rcd->cd = cd;

    rcd->reset_after_error = FALSE;
    rcd->handle_bom = FALSE;
    rcd->undo_transliteration = FALSE; /* for cleanup */

# ifdef R_MACOS_LIBICONV_RESET_AFTER_ERROR
    static int iconv_use_reset_after_error = -1; /* -1: not known yet */

    if (iconv_use_reset_after_error)
	rcd->reset_after_error = !is_stateful(tocode)
	                         && !is_stateful(fromcode);

    if (rcd->reset_after_error && iconv_use_reset_after_error == -1) {
	/* a run-time check whether iconv needs reset after error */
	int bib = breaks_after_invalid_byte();
	if (macos_libiconv_verbose())
	    fprintf(stderr, "ICONV: Breaks after invalid bytes: %d.\n", bib);

	char *p = getenv("_R_ICONV_RESET_AFTER_ERROR_");
	if (bib == 1 && (!p || !StringFalse(p))) {
	    if (macos_libiconv_verbose())
		fprintf(stderr, "ICONV: Reset after error enabled.\n");
	    iconv_use_reset_after_error = 1;
	} else {
	    iconv_use_reset_after_error = 0;
	    rcd->reset_after_error = FALSE;
	}
   } 
# endif

# ifdef R_MACOS_LIBICONV_HANDLE_BOM
    static int iconv_needs_bom_handling = -1; /* -1: not known yet */

    if (iconv_needs_bom_handling) {
	if (!strcasecmp(fromcode, "UTF-16")
	    || !strcasecmp(fromcode,"UNICODE")) {

	    rcd->handle_bom = TRUE;
	    rcd->bomlen = 2;
	} else if (!strcasecmp(fromcode, "UTF-32")) {
	    rcd->handle_bom = TRUE;
	    rcd->bomlen = 4;
	}
    }

    if (rcd->handle_bom && iconv_needs_bom_handling == -1) {
	/* a run-time check whether iconv needs bom handling */
	if (fails_iteratively_with_bom() == 1) {
	    if (macos_libiconv_verbose()) 
		fprintf(stderr, "ICONV: BOM handling enabled.\n");
	    iconv_needs_bom_handling = 1;
	} else {
	    iconv_needs_bom_handling = 0;
	    rcd->handle_bom = FALSE;
	}
    }

    if (rcd->handle_bom) {
	rcd->startlen = 0;
	size_t len = strlen(tocode)+1;
	rcd->tocode = malloc(len);
	if (!rcd->tocode) {
	    iconv_close_internal(rcd);
	    errno = ENOMEM;
	    return (void *)(iconv_t)-1;
	}
	memcpy(rcd->tocode, tocode, len);
    }
# endif

# ifdef R_MACOS_LIBICONV_UNDO_TRANSLITERATION
    rcd->undo_transliteration = !is_unicode(tocode)
                                && !is_stateful(tocode)
                                && !is_stateful(fromcode);

    char *p = getenv("_R_ICONV_UNDO_TRANSLITERATION_");
    if (!p || !StringTrue(p))
	rcd->undo_transliteration = FALSE;

    if (rcd->undo_transliteration) {
	rcd->buf = NULL; /* for cleanup */
	rcd->cd_back = iconv_open(fromcode, tocode);
	if (rcd->cd_back == (iconv_t)-1) {
	    iconv_close_internal(rcd);
	    return (iconv_t)-1;
	}
	rcd->buflen = 8192;
	rcd->buf = malloc(rcd->buflen);
	if (!rcd->buf) {
	    iconv_close_internal(rcd);
	    return (iconv_t)-1;
	}
    }
# endif
    return rcd;
#endif
}

void * Riconv_open (const char* tocode, const char* fromcode)
{
#if defined Win32 || __APPLE__
// These two support "utf8"
# ifdef Win32
    const char *cp = "ASCII";
    char to[20] = "";
    if (localeCP > 0) {snprintf(to, 20, "CP%d", localeCP); cp = to;}
# else /* __APPLE__ */
    const char *cp = "UTF-8";
    if (latin1locale) cp = "ISO-8859-1";
    else if (!utf8locale) cp = locale2charset(NULL);
# endif
    if (!*tocode && !*fromcode) return iconv_open_internal(cp, cp);
    if(!*tocode)  return iconv_open_internal(cp, fromcode);
    else if(!*fromcode) return iconv_open_internal(tocode, cp);
    else return iconv_open_internal(tocode, fromcode);
#else
// "utf8" is not valid but people keep on using it
    const char *to = tocode, *from = fromcode;
    if(strcasecmp(tocode, "utf8") == 0) to = "UTF-8";
    if(strcasecmp(fromcode, "utf8") == 0) from = "UTF-8";
    return iconv_open_internal(to, from);
#endif
}

/* Should be defined in config.h, but prior to 2.13.0 was only checked
   if the NLS was enabled  */
#ifndef ICONV_CONST
# define ICONV_CONST
#endif

size_t Riconv (void *cd, const char **inbuf, size_t *inbytesleft,
	       char **outbuf, size_t *outbytesleft)
{
#ifdef R_MACOS_LIBICONV_WORKAROUND
    Riconv_cd *rcd = (Riconv_cd *)cd;

# ifdef R_MACOS_LIBICONV_HANDLE_BOM
    const char *prev_inbuf = NULL;
    if (rcd->handle_bom && inbuf)
	prev_inbuf = *inbuf;
# endif

# ifdef R_MACOS_LIBICONV_UNDO_TRANSLITERATION
    const char *old_inbuf = NULL;
    size_t old_inbytesleft = 0;
    char *old_outbuf = NULL;
    size_t old_outbytesleft = 0;

    if (rcd->undo_transliteration) {
	old_inbuf = inbuf ? *inbuf : NULL;
	old_inbytesleft = inbytesleft ? *inbytesleft : 0;
	old_outbuf = outbuf ? *outbuf : NULL;
	old_outbytesleft = outbytesleft ? *outbytesleft : 0;
    }
# endif
#endif

    /* here libiconv has const char **, glibc has char ** for inbuf */
    size_t res = iconv(
#ifdef R_MACOS_LIBICONV_WORKAROUND
                       rcd->cd,
#else
                       (iconv_t)cd,
#endif
                       (ICONV_CONST char **) inbuf, inbytesleft,
                       outbuf, outbytesleft);

#ifdef R_MACOS_LIBICONV_WORKAROUND
# ifdef R_MACOS_LIBICONV_RESET_AFTER_ERROR
    if (rcd->reset_after_error &&
	(res == (size_t)-1 && (errno == EILSEQ || errno == EINVAL))) {

	int saveerrno = errno;
	iconv(rcd->cd, NULL, NULL, NULL, NULL);
	errno = saveerrno;
    }
# endif

# ifdef R_MACOS_LIBICONV_HANDLE_BOM
    if (rcd->handle_bom && prev_inbuf && inbuf) {
	size_t tocopy = rcd->bomlen - rcd->startlen;
	ptrdiff_t avail = *inbuf - prev_inbuf;

	if (avail < tocopy)
	    tocopy = (size_t) avail;
	memcpy(rcd->start + rcd->startlen, prev_inbuf, tocopy);
	rcd->startlen += tocopy;
	
	if (rcd->startlen == rcd->bomlen) {
	    const char *new_fromcode = NULL;

	    if (rcd->bomlen == 2) {
		if (!memcmp(rcd->start, "\xff\xfe", 2))
		    new_fromcode = "UTF-16LE";
		else if (!memcmp(rcd->start, "\xfe\xff", 2))
		    new_fromcode = "UTF-16BE";
	    } else if (rcd->bomlen == 4) {
		if (!memcmp(rcd->start, "\xff\xfe\x00\x00", 4))
		    new_fromcode = "UTF-32LE";
		else if (!memcmp(rcd->start, "\x00\x00\xfe\xff", 4))
		    new_fromcode = "UTF-32BE";
	    }
	    if (new_fromcode) {
		iconv_close((iconv_t) rcd->cd);
		rcd->cd = iconv_open(rcd->tocode, new_fromcode);
	    }

	    free(rcd->tocode);
	    rcd->tocode = NULL; /* for cleanup */
	    rcd->handle_bom = FALSE;
	}
    }
# endif

# ifdef R_MACOS_LIBICONV_UNDO_TRANSLITERATION
    if (rcd->undo_transliteration &&
        inbuf && inbytesleft && outbuf && outbytesleft) {

	int saveerrno = errno;
	size_t needed = old_inbytesleft - *inbytesleft;
	if (rcd->buflen < needed) {
	    free(rcd->buf);
	    rcd->buf = malloc(needed);
	    if (!rcd->buf) {
		errno = ENOMEM;
		return -1;
	    }
	    rcd->buflen = needed;
	}

	const char *back_inbuf = old_outbuf;
	size_t back_inbytesleft = old_outbytesleft - *outbytesleft;
	char *back_outbuf = rcd->buf;
	size_t back_outbytesleft = needed;

	iconv(rcd->cd_back, NULL, NULL, NULL, NULL);
	size_t back_res = iconv(rcd->cd_back,
	      (ICONV_CONST char **) &back_inbuf,
	      &back_inbytesleft,
	      (ICONV_CONST char **) &back_outbuf,
	      &back_outbytesleft);

	if (back_res == (size_t)-1) {
	    /* should not happen */
	    errno = saveerrno;
	    return res;
	}

	if (back_outbytesleft == 0 && back_inbytesleft == 0 &&
	    !memcmp(rcd->buf, old_inbuf, needed)) {

	    /* no transliteration happened */
	    errno = saveerrno;
	    return res;
	}

	size_t stored = needed - back_outbytesleft;
	size_t i;
	for(i = 0; i < stored; i++)
	    if (rcd->buf[i] != old_inbuf[i]) {
		/* byte at index i in old_inbuf was probably transliterated,
		   so convert again only i bytes and report error */
		*inbuf = old_inbuf;
		*outbuf = old_outbuf;
		*outbytesleft = old_outbytesleft;
		size_t reduced = i;
		res = iconv(rcd->cd,
		            (ICONV_CONST char **) inbuf,
		            &reduced,
		            (ICONV_CONST char **) outbuf,
		            outbytesleft);

		/* "reduced" should be 0 now */
		*inbytesleft = old_inbytesleft - (i - reduced);
		errno = EILSEQ;
		return -1;
	    }
	/* should not be reached */
	errno = saveerrno;
    } 
# endif
#endif
    return res;
}

int Riconv_close (void *cd)
{
#ifndef R_MACOS_LIBICONV_WORKAROUND
    return iconv_close((iconv_t) cd);

#else
    return iconv_close_internal((Riconv_cd *)cd);
#endif
}

typedef enum {
    NT_NONE        = 0, /* no translation is needed */
    NT_FROM_UTF8   = 1, /* need to translate from UTF8 */
    NT_FROM_LATIN1 = 2, /* need to translate from latin1 */
    NT_FROM_NATIVE = 3, /* need to translate from native encoding */
    NT_FROM_ASCII  = 4, /* need to translate from ASCII */
} nttype_t;

/* Decides whether translation to native encoding is needed. */
static R_INLINE nttype_t needsTranslation(SEXP x)
{
    if (IS_ASCII(x)) return NT_NONE;
    if (IS_UTF8(x)) {
	if (utf8locale || x == NA_STRING) return NT_NONE;
	return NT_FROM_UTF8;
    }
    if (IS_LATIN1(x)) {
	if (x == NA_STRING || latin1locale) return NT_NONE;
	return NT_FROM_LATIN1;
    }
    if (IS_BYTES(x))
	error(_("translating strings with \"bytes\" encoding is not allowed"));
    return NT_NONE;
}

static void *latin1_obj = NULL, *utf8_obj=NULL, *ucsmb_obj=NULL,
    *ucsutf8_obj=NULL;

/* Translates string in "ans" to native encoding returning it in string
   buffer "cbuff". */
static int translateToNative(const char *ans, R_StringBuffer *cbuff,
			     nttype_t ttype, int mustWork)
{
    if (ttype == NT_NONE)
	error(_("internal error: no translation needed"));

    void * obj;
    const char *inbuf, *from;
    char *outbuf;
    size_t inb, outb, res;
    bool failed = false;

    if(ttype == NT_FROM_LATIN1) {
	if(!latin1_obj) {
#ifdef HAVE_ICONV_CP1252
	    from = "CP1252";
#else
	    from = "latin1";
#endif
	    obj = Riconv_open("", from);
	    /* should never happen */
	    if(obj == (void *)(-1))
#ifdef Win32
		error(_("unsupported conversion from '%s' in codepage %d"),
		      from, localeCP);
#else
		error(_("unsupported conversion from '%s' to '%s'"),
		      from, "");
#endif
	    latin1_obj = obj;
	}
	obj = latin1_obj;
    } else { /* ttype == NT_FROM_UTF8 */
	if(!utf8_obj) {
	    obj = Riconv_open("", "UTF-8");
	    /* should never happen */
	    if(obj == (void *)(-1))
#ifdef Win32
		error(_("unsupported conversion from '%s' in codepage %d"),
		      "UTF-8", localeCP);
#else
		error(_("unsupported conversion from '%s' to '%s'"),
		      "UTF-8", "");
#endif
	    utf8_obj = obj;
	}
	obj = utf8_obj;
    }

    R_AllocStringBuffer(0, cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff->data; outb = cbuff->bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	res = Riconv(obj, NULL, NULL, &outbuf, &outb);
	if((res == -1 && errno == E2BIG) || outb < 13) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}
	failed = true;
	if (ttype == NT_FROM_UTF8) {
	    /* if starting in UTF-8, use \uxxxx */
	    /* This must be the first byte */
	    wchar_t wc;
	    ssize_t clen = utf8toucs(&wc, inbuf);
	    if(clen > 0 && inb >= clen) {
		R_wchar_t ucs;
	    	if (IS_HIGH_SURROGATE(wc))
	    	    ucs = utf8toucs32(wc, inbuf);
	    	else
	    	    ucs = (R_wchar_t) wc;
		inbuf += clen; inb -= clen;
		if(ucs < 65536) {
		// gcc 7 objects to this with unsigned int
		    snprintf(outbuf, 9, "<U+%04X>", (unsigned short) ucs);
		    outbuf += 8; outb -= 8;
		} else {
		    // R_wchar_t is usually unsigned int, but wchar_t need not be
		    snprintf(outbuf, 13, "<U+%08X>", (unsigned int) ucs);
		    outbuf += 12; outb -= 12;
		}
	    } else {
		snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
		outbuf += 4; outb -= 4;
		inbuf++; inb--;
	    }
	} else { // not from UTF-8
	    snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	    outbuf += 4; outb -= 4;
	    inbuf++; inb--;
	}
	goto next_char;
    }
    *outbuf = '\0';
    if (mustWork && failed) {
	/* copy to truncate and in case of error prevent memory leak */
	char err_buff[256];
	if (strlen(cbuff->data) > 255) {
	    strncpy(err_buff, cbuff->data, 252);
	    err_buff[252] = '\0';
	    mbcsTruncateToValid(err_buff);
	    strcat(err_buff, "...");
	} else
	    strcpy(err_buff, cbuff->data);

	if (mustWork == 2) {
	    warning(_("unable to translate '%s' to native encoding"),
		    err_buff);
	    return 1;
	} else {
	    R_FreeStringBuffer(cbuff);
	    error(_("unable to translate '%s' to native encoding"), err_buff);
	}
    }
    return 0;
}

static const char *copyAndFreeStringBuffer(R_StringBuffer *cbuff)
{
    size_t res = strlen(cbuff->data) + 1;
    char *p = R_alloc(res, 1);
    memcpy(p, cbuff->data, res);
    R_FreeStringBuffer(cbuff);
    return p;
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *translateChar(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslation(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToNative(ans, &cbuff, t, 0);
    return copyAndFreeStringBuffer(&cbuff);
}

/* Variant which does not return escaped string (which must work, throwing
   error when conversion fails). Used for file paths, including devices. */
const char *translateCharFP(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslation(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToNative(ans, &cbuff, t, 1);
    return copyAndFreeStringBuffer(&cbuff);
}

/* Variant which returns NULL (with a warning) when conversion fails,
   used for file paths. */
attribute_hidden
const char *translateCharFP2(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslation(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (translateToNative(ans, &cbuff, t, 2)) {
	R_FreeStringBuffer(&cbuff);
	return NULL;
    } else
	return copyAndFreeStringBuffer(&cbuff);
}

SEXP installTrChar(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslation(x);
    if (t == NT_NONE) return installNoTrChar(x);

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    // For back-compatibility this allows installing
    // symbols with escapes, with a warning.
    translateToNative(CHAR(x), &cbuff, t, 2);

    SEXP Sans = install(cbuff.data);
    R_FreeStringBuffer(&cbuff);
    return Sans;
}

/* Translates as from R 3.6.0.
   As from R 4.0.0 unused in newly installed code as installChar is
   remapped to Rf_installTrChar. 
 */
SEXP Rf_installChar(SEXP x)
{
    return Rf_installTrChar(x);
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack.

   Use for writeLines/Bin/Char, the first only with useBytes = TRUE.
*/
attribute_hidden const char *translateChar0(SEXP x)
{
    CHECK_CHARSXP(x);
    if(IS_BYTES(x)) return CHAR(x);
    return translateChar(x);
}

/* Decides whether translation to UTF-8 is needed. */
static R_INLINE nttype_t needsTranslationUTF8(SEXP x)
{
    if (IS_UTF8(x) || IS_ASCII(x) || x == NA_STRING) return NT_NONE;
    if (IS_BYTES(x))
	error(_("translating strings with \"bytes\" encoding is not allowed"));
    if (IS_LATIN1(x) || latin1locale) return NT_FROM_LATIN1;
    if (utf8locale) return NT_NONE;
    return NT_FROM_NATIVE;
}

/* Translates string in "ans" to UTF-8 returning it in string
   buffer "cbuff". */
static int translateToUTF8(const char *ans, R_StringBuffer *cbuff,
			     nttype_t ttype, int mustWork)
{
    if (ttype == NT_NONE)
	error(_("internal error: no translation needed"));

    void *obj;
    const char *inbuf, *from = "";
    char *outbuf;
    size_t inb, outb, res;
    bool failed = false;

    if (ttype == NT_FROM_LATIN1)
#ifdef HAVE_ICONV_CP1252
	from = "CP1252";
#else
	from = "latin1";
#endif
    /* else (ttype == NT_FROM_NATIVE) */
    obj = Riconv_open("UTF-8", from);
    if(obj == (void *)(-1))
#ifdef Win32
	error(_("unsupported conversion from '%s' in codepage %d"),
	      from, localeCP);
#else
	error(_("unsupported conversion from '%s' to '%s'"),
	      from, "UTF-8");
#endif
    R_AllocStringBuffer(0, cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff->data; outb = cbuff->bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	res = Riconv(obj, NULL, NULL, &outbuf, &outb);
	if((res == -1 && errno == E2BIG) || outb < 5) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}
	failed = true;
	snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
	outbuf += 4; outb -= 4;
	inbuf++; inb--;
	goto next_char;
    }
    *outbuf = '\0';
    Riconv_close(obj);
    if (mustWork && failed) {
	const void *vmax = vmaxget();
	const char *native_buf = reEnc(cbuff->data, CE_UTF8, CE_NATIVE, 2);

	/* copy to truncate */
	char err_buff[256];
	if (strlen(native_buf) > 255) {
	    strncpy(err_buff, native_buf, 252);
	    err_buff[252] = '\0';
	    mbcsTruncateToValid(err_buff);
	    strcat(err_buff, "...");
	} else
	    strcpy(err_buff, native_buf);

	if (mustWork == 2) {
	    warning(_("unable to translate '%s' to UTF-8"),
		    err_buff);
	    vmaxset(vmax);
	    return 1;
	} else {
	    R_FreeStringBuffer(cbuff);
	    error(_("unable to translate '%s' to UTF-8"), err_buff);
	}
	vmaxset(vmax);
    }
    return 0;
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *translateCharUTF8(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslationUTF8(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToUTF8(ans, &cbuff, t, 0);
    return copyAndFreeStringBuffer(&cbuff);
}

/* Variant which does not return escaped string (which must work, throwing
   error when conversion fails). */
attribute_hidden
const char *trCharUTF8(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslationUTF8(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToUTF8(ans, &cbuff, t, 1);
    return copyAndFreeStringBuffer(&cbuff);
}

/* Variant which returns NULL (with a warning) when conversion fails. */
attribute_hidden
const char *trCharUTF82(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = needsTranslationUTF8(x);
    const char *ans = CHAR(x);
    if (t == NT_NONE) return ans;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (translateToUTF8(ans, &cbuff, t, 2)) {
	R_FreeStringBuffer(&cbuff);
	return NULL;
    }  else
	return copyAndFreeStringBuffer(&cbuff);
}

/* Decides type of translation needed to get wchar_t*. */
static R_INLINE nttype_t wneedsTranslation(SEXP x)
{
    if (IS_BYTES(x))
	error(_("translating strings with \"bytes\" encoding is not allowed"));
    if (IS_ASCII(x)) return NT_FROM_ASCII;
    if (IS_UTF8(x)) return NT_FROM_UTF8;
    if (IS_LATIN1(x) || latin1locale) return NT_FROM_LATIN1;
    if (utf8locale) return NT_FROM_UTF8;
    return NT_FROM_NATIVE;
}

static const wchar_t *wcopyAndFreeStringBuffer(R_StringBuffer *cbuff)
{
    size_t res = wcslen((wchar_t *) cbuff->data) + 1;
    wchar_t *p = (wchar_t *) R_alloc(res, sizeof(wchar_t));
    memcpy(p, cbuff->data, res * sizeof(wchar_t));
    R_FreeStringBuffer(cbuff);
    return p;
}

static const wchar_t *wfromASCII(const char *src, size_t len)
{
    size_t i;
    wchar_t *p = (wchar_t *) R_alloc(len + 1, sizeof(wchar_t));
    for (i = 0; i < len; i++)
	p[i] = (wchar_t) src[i];
    p[i] = L'\0';
    return p;
}

#ifdef Win32
static const char TO_WCHAR[] = "UTF-16LE";
#else
# ifdef WORDS_BIGENDIAN
static const char TO_WCHAR[] = "UCS-4BE";
# else
static const char TO_WCHAR[] = "UCS-4LE";
# endif
#endif

static void *latin1_wobj = NULL, *utf8_wobj=NULL;

/* Translate from current encoding to wchar_t = UTF-16LE/UCS-4
   NB: that wchar_t is UCS-4 is an assumption, but not easy to avoid.
*/

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
static int translateToWchar(const char *ans, R_StringBuffer *cbuff,
                            nttype_t ttype, int mustWork)
{
    void * obj;
    const char *inbuf, *from;
    char *outbuf;
    size_t inb, outb, res;
    bool failed = false;

    if(ttype == NT_FROM_LATIN1) {
	if(!latin1_wobj) {
#ifdef HAVE_ICONV_CP1252
	    from = "CP1252";
#else
	    from = "latin1";
#endif
	    obj = Riconv_open(TO_WCHAR, from);
	    if(obj == (void *)(-1))
		error(_("unsupported conversion from '%s' to '%s'"),
		      from, TO_WCHAR);
	    latin1_wobj = obj;
	} else
	    obj = latin1_wobj;
    } else if(ttype == NT_FROM_UTF8) {
	if(!utf8_wobj) {
	    obj = Riconv_open(TO_WCHAR, "UTF-8");
	    if(obj == (void *)(-1))
		error(_("unsupported conversion from '%s' to '%s'"),
		      "UTF-8", TO_WCHAR);
	    utf8_wobj = obj;
	} else
	    obj = utf8_wobj;
    } else { /* t == NT_FROM_NATIVE */
	obj = Riconv_open(TO_WCHAR, "");
	if(obj == (void *)(-1))
#ifdef Win32
	    error(_("unsupported conversion to '%s' from codepage %d"),
		  TO_WCHAR, localeCP);
#else
	    error(_("unsupported conversion from '%s' to '%s'"), "", TO_WCHAR);
#endif
    }

    /* R_AllocStringBuffer returns correctly aligned for wchar_t */
    R_AllocStringBuffer(0, cbuff);
top_of_loop:
    inbuf = ans; inb = strlen(inbuf);
    outbuf = cbuff->data; outb = cbuff->bufsize - 1;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	res = Riconv(obj, NULL, NULL, &outbuf, &outb);
	if((res == -1 && errno == E2BIG) || outb < 5 * sizeof(wchar_t)) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}
	failed = true;
	swprintf((wchar_t*)outbuf, 5, L"<%02x>", (unsigned char)*inbuf);
	outbuf += 4 * sizeof(wchar_t); outb -= 4 * sizeof(wchar_t);
	inbuf++; inb--;
	goto next_char;
    }
    *((wchar_t *) outbuf) = L'\0'; /* terminate wide string */
    if(ttype == NT_FROM_NATIVE) Riconv_close(obj);
    if (mustWork && failed) {
	const void *vmax = vmaxget();
	const char *native_buf = reEnc3(cbuff->data, TO_WCHAR, "", 2);

	/* copy to truncate (and mark as truncated) */
	char err_buff[256];
	if (strlen(native_buf) > 255) {
	    strncpy(err_buff, native_buf, 252);
	    err_buff[252] = '\0';
	    mbcsTruncateToValid(err_buff);
	    strcat(err_buff, "...");
	} else
	    strcpy(err_buff, native_buf);

	if (mustWork == 2) {
	    warning(_("unable to translate '%s' to a wide string"),
	              err_buff);
	    vmaxset(vmax);
	    return 1;
	} else {
	    R_FreeStringBuffer(cbuff);
	    error(_("unable to translate '%s' to a wide string"),
	          err_buff);
	}
	vmaxset(vmax);
    }
    return 0;
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const wchar_t *wtransChar(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = wneedsTranslation(x);
    if (t == NT_FROM_ASCII)
	return wfromASCII(CHAR(x), LENGTH(x));

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    translateToWchar(CHAR(x), &cbuff, t, 0);
    return wcopyAndFreeStringBuffer(&cbuff);
}

/* Variant which returns NULL (with a warning) when conversion fails. */
attribute_hidden /* would need to be in an installed header if not hidden */
const wchar_t *wtransChar2(SEXP x)
{
    CHECK_CHARSXP(x);
    nttype_t t = wneedsTranslation(x);
    if (t == NT_FROM_ASCII)
	return wfromASCII(CHAR(x), LENGTH(x));

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (translateToWchar(CHAR(x), &cbuff, t, 2)) {
	R_FreeStringBuffer(&cbuff);
	return NULL;
    } else
	return wcopyAndFreeStringBuffer(&cbuff);
}

static int reEncodeIconv(const char *x, R_StringBuffer *cbuff,
                         const char *fromcode, const char *tocode, int subst)
{
    void * obj;
    const char *inbuf;
    char *outbuf;
    size_t inb, outb, res;
    bool fromWchar = !strcmp(fromcode, TO_WCHAR);

    obj = Riconv_open(tocode, fromcode);
    if(obj == (void *)(-1)) return 1;
    R_AllocStringBuffer(0, cbuff);
top_of_loop:
    inbuf = x;
    if (fromWchar)
	inb = wcslen((wchar_t *)inbuf) * sizeof(wchar_t);
    else
	inb = strlen(inbuf);
    outbuf = cbuff->data; outb = cbuff->bufsize - 3;
    /* First initialize output */
    Riconv (obj, NULL, NULL, &outbuf, &outb);
next_char:
    /* Then convert input  */
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    if(res == -1 && errno == E2BIG) {
	R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	goto top_of_loop;
    } else if(res == -1 && (errno == EILSEQ || errno == EINVAL)) {
	res = Riconv(obj, NULL, NULL, &outbuf, &outb);
	if(res == -1 && errno == E2BIG) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}
	size_t inb_per_char = fromWchar ? sizeof(wchar_t) : 1;

	/* ensure space in cbuff for substitution */	
	size_t need = 0; 
	switch(subst) {
	case 1: /* substitute hex */
	    need = inb_per_char * 4 + 1;
	    break;
	case 2: /* substitute . */
	case 3: /* substitute ? */
	    need = inb_per_char;
	    break;
	default: /* skip byte */
	    inbuf += inb_per_char;
	    inb -= inb_per_char;
	    goto next_char;
	}
	if(outb < need) {
	    R_AllocStringBuffer(2*cbuff->bufsize, cbuff);
	    goto top_of_loop;
	}

	/* substitute individual bytes, it makes more sense for users as
	   typically errors would be due to conversion from a single-byte
	   encoding */
	for(int i = 0; i < inb_per_char; i++) {
	    if (!inb) break;
	    switch(subst) {
	    case 1: /* substitute hex */
		snprintf(outbuf, 5, "<%02x>", (unsigned char)*inbuf);
		outbuf += 4; outb -= 4;
		inbuf++; inb--;
		break;
	    case 2: /* substitute . */
		*outbuf++ = '.'; inbuf++; outb--; inb--;
		break;
	    case 3: /* substitute ? */
		*outbuf++ = '?'; inbuf++; outb--; inb--;
		break;
	    }
	}
	goto next_char;
    }
    Riconv_close(obj);
    *outbuf = '\0';
    return 0;
}

#include <R_ext/GraphicsEngine.h>

/* returns 1 when no conversion is needed and in case of error, 0 otherwise */
static int reEncode(const char *x, R_StringBuffer *cbuff,
                    cetype_t ce_in, cetype_t ce_out, int subst)
{
    char *tocode = NULL, *fromcode = NULL;

    /* We can only encode from Symbol to UTF-8 */
    if(ce_in == ce_out || ce_out == CE_SYMBOL ||
       ce_in == CE_ANY || ce_out == CE_ANY) return 1;
    if(ce_in == CE_SYMBOL) {
	if(ce_out == CE_UTF8) {
	    size_t nc = 3*strlen(x)+1; /* all in BMP */
	    R_AllocStringBuffer(nc, cbuff);
	    Rf_AdobeSymbol2utf8(cbuff->data, x, cbuff->bufsize, TRUE);
	    return 0;
	} else return 1;
    }

    if(strIsASCII(x)) return 1;
    if(utf8locale && ce_in == CE_NATIVE && ce_out == CE_UTF8) return 1;
    if(utf8locale && ce_out == CE_NATIVE && ce_in == CE_UTF8) return 1;
    if(latin1locale && ce_in == CE_NATIVE && ce_out == CE_LATIN1) return 1;
    if(latin1locale && ce_out == CE_NATIVE && ce_in == CE_LATIN1) return 1;

    switch(ce_in) {
    case CE_NATIVE: fromcode = ""; break;
#ifdef HAVE_ICONV_CP1252
    case CE_LATIN1: fromcode = "CP1252"; break;
#else
    case CE_LATIN1: fromcode = "latin1"; break;
#endif
    case CE_UTF8:   fromcode = "UTF-8"; break;
    default: return 1;
    }

    switch(ce_out) {
    case CE_NATIVE: tocode = ""; break;
    case CE_LATIN1: tocode = "latin1"; break; /* ?? CP1252 */
    case CE_UTF8:   tocode = "UTF-8"; break;
    default: return 1;
    }

    return reEncodeIconv(x, cbuff, fromcode, tocode, subst);
}

/* This may return a R_alloc-ed result, so the caller has to manage the
   R_alloc stack */
const char *reEnc(const char *x, cetype_t ce_in, cetype_t ce_out, int subst)
{
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (reEncode(x, &cbuff, ce_in, ce_out, subst)) return x;
    size_t res = strlen(cbuff.data) + 1;
    char *p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

#ifdef Win32
/* A version avoiding R_alloc for use in the Rgui editor */
void reEnc2(const char *x, char *y, int ny,
	    cetype_t ce_in, cetype_t ce_out, int subst)
{
    int res;

    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (reEncode(x, &cbuff, ce_in, ce_out, subst)) {
	strncpy(y, x, ny);
	y[ny - 1] = '\0';
	return;
    }
    res = strlen(cbuff.data) + 1;
    if (res > ny) error("converted string too long for buffer");
    memcpy(y, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
}
#endif

/* A version that works with arbitrary iconv encodings, used for getting
   escaped invalid characters for error messages. */
attribute_hidden
const char *reEnc3(const char *x,
                   const char *fromcode, const char *tocode, int subst)
{
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
    if (reEncodeIconv(x, &cbuff, fromcode, tocode, subst)) return x;
    size_t res = strlen(cbuff.data) + 1;
    char *p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}

attribute_hidden void
invalidate_cached_recodings(void)
{
    if (latin1_obj) {
	Riconv_close(latin1_obj);
	latin1_obj = NULL;
    }
    if (utf8_obj) {
	Riconv_close(utf8_obj);
	utf8_obj = NULL;
    }
    if (ucsmb_obj) {
	Riconv_close(ucsmb_obj);
	ucsmb_obj = NULL;
    }
#ifdef Win32
    if (latin1_wobj) {
	Riconv_close(latin1_wobj);
	latin1_wobj = NULL;
    }
    if (utf8_wobj) {
	Riconv_close(utf8_wobj);
	utf8_wobj = NULL;
    }
#endif
}


/* in C11 these could use char32_t */
#ifdef WORDS_BIGENDIAN
static const char UNICODE[] = "UCS-4BE";
#else
static const char UNICODE[] = "UCS-4LE";
#endif

/* used in gram.c and devX11.c */
size_t ucstomb(char *s, const unsigned int wc)
{
    char     buf[R_MB_CUR_MAX+1];
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
	    strncpy(tocode, locale2charset(NULL), sizeof(tocode) - 1);
	    tocode[sizeof(tocode) - 1] = '\0';
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
    buf[R_MB_CUR_MAX] = '\0'; /* safety measure */
    strcpy(s, buf);
    return strlen(buf);
}

/* used in engine.c for non-UTF-8 MBCS */
attribute_hidden size_t
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
	    Riconv_close(cd);
	    return (size_t) -2;
	case EILSEQ:
	    Riconv_close(cd);
	    return (size_t) -1;
	case E2BIG:
	    break;
	default:
	    Riconv_close(cd);
	    errno = EILSEQ;
	    return (size_t) -1;
	}
    }
    Riconv_close(cd);
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

    if(ucsutf8_obj == NULL) {
	if((void *)(-1) == (cd = Riconv_open("UTF-8", UNICODE))) {
	    error(_("unsupported conversion from '%s' to '%s'"),
		  UNICODE, "UTF-8");
	    return (size_t)(-1);
	}
	ucsutf8_obj = cd;
    }

    status = Riconv(ucsutf8_obj, &inbuf, &inbytesleft, &outbuf, &outbytesleft);

    if (status == (size_t) -1) {
	switch(errno){
	case E2BIG:
	    break;
	default:
	    error(_("invalid Unicode point %u"), wc);
	    return (size_t) -1; // Not reached
	}
    }
    *outbuf = '\0';
    strcpy(s, buf);
    return strlen(buf);
}

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

attribute_hidden int R_isWriteableDir(char *path)
{
#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    int isdir = 0;
    if(!path) return 0;
#ifdef Win32
    if(_stati64(path, &sb) == 0) {
#else
    if(stat(path, &sb) == 0) {
#endif
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
attribute_hidden int R_isWriteableDir(char *path)
{
    return 1;
}
#endif /* HAVE_STAT */

#if !HAVE_DECL_MKDTEMP
extern char * mkdtemp (char *template);
#endif

#ifdef Win32
# include <ctype.h>
#endif

attribute_hidden /* would need to be in an installed header if not hidden */
void R_reInitTempDir(int die_on_fail)
{
    char *tmp = NULL, *tm;
    size_t len;

#define ERROR_MAYBE_DIE(MSG_) do {		\
    if(die_on_fail)				\
	R_Suicide(MSG_);			\
    else					\
	errorcall(R_NilValue, MSG_);            \
} while (0)

    if(R_TempDir) return; /* someone else set it */
    /* getenv("R_SESSION_TMPDIR");   no longer set in R.sh */

    tm = getenv("TMPDIR");
    if (!R_isWriteableDir(tm)) {
	tm = getenv("TMP");
	if (!R_isWriteableDir(tm)) {
	    tm = getenv("TEMP");
	    if (!R_isWriteableDir(tm)) {
#ifdef Win32
		tm = getenv("R_USER"); /* this one will succeed */
		if (!tm)
		    ERROR_MAYBE_DIE(_("'R_USER' not set"));
#else
		tm = "/tmp";
#endif
	    }
	}
    }

    /* make sure no spaces in path */
    int hasspace = 0;
    char *p;
    for (p = tm; *p; p++)
	if (isspace(*p)) { hasspace = 1; break; }
#ifdef Win32
    char *suffix = "\\RtmpXXXXXX";
    if (hasspace) {
	DWORD res = GetShortPathName(tm, NULL, 0);
	if (res > 0) {
	    len = res + strlen(suffix);
	    tmp = (char *)malloc(len);
	    if (!tmp)
		ERROR_MAYBE_DIE(_("cannot allocate 'R_TempDir'"));
	    DWORD res1 = GetShortPathName(tm, tmp, res);
	    if (res1 > 0 && res1 < res)
		strcat(tmp, suffix);
	    else { /* very unlikely */
		free(tmp);
		tmp = NULL;
	    }
	}
	if (tmp) {
	    /* GetShortPathName may return a long name, so check again */
	    hasspace = 0;
	    for (p = tmp; *p; p++)
		if (isspace(*p)) { hasspace = 1; break; }
	}
    }
#else
    char *suffix = "/RtmpXXXXXX";
#endif
    if (hasspace) {
	if (tmp)
	    free(tmp);
	ERROR_MAYBE_DIE(_("'R_TempDir' contains space"));
    }
    if (!tmp) {
	len = strlen(tm) + strlen(suffix) + 1;
	tmp = (char *)malloc(len);
	if (!tmp)
	    ERROR_MAYBE_DIE(_("cannot allocate 'R_TempDir'"));
	strcpy(tmp, tm);
	strcat(tmp, suffix);
    }
    if(!mkdtemp(tmp)) {
	free(tmp);
	ERROR_MAYBE_DIE(_("cannot create 'R_TempDir'"));
    }
#ifndef Win32
# ifdef HAVE_SETENV
    if(setenv("R_SESSION_TMPDIR", tmp, 1)) {
	free(tmp);
	errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
    }
# elif defined(HAVE_PUTENV)
    {
	len = strlen(tmp) + 20;
	char * buf = (char *) malloc((len) * sizeof(char));
	if(buf) {
	    snprintf(buf, len, "R_SESSION_TMPDIR=%s", tmp);
	    if(putenv(buf)) {
		free(tmp);
		free(buf);
		errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
	    }
	    /* no free here: storage remains in use */
	} else {
	    free(tmp);
	    errorcall(R_NilValue, _("unable to set R_SESSION_TMPDIR"));
	}
    }
# endif
#endif
    R_TempDir = tmp;
    Sys_TempDir = tmp;
}

attribute_hidden void InitTempDir(void) {
    R_reInitTempDir(/* die_on_fail = */ TRUE);
}

/* returns malloc'd result */
char * R_tmpnam(const char * prefix, const char * tempdir)
{
    return R_tmpnam2(prefix, tempdir, "");
}

/* NB for use with multicore: parent and all children share the same
   session directory and run in parallel.
   So as from 2.14.1, we make sure getpid() is part of the process.
*/
/* returns malloc'd result */
char * R_tmpnam2(const char *prefix, const char *tempdir, const char *fileext)
{
    unsigned int n, pid = getpid();
#ifdef Win32
    char filesep[] = "\\";
#else
    char filesep[] = "/";
#endif

    if(!prefix) prefix = "";	/* NULL */
    if(!fileext) fileext = "";  /*  "   */

    for (n = 0; n < 100; n++) {
	/* try a random number at the end.  Need at least 6 hex digits */
	int r1 = rand();
#if RAND_MAX > 16777215
# define TMPNAM2_SNPRINTF(BUF, SIZE) \
	snprintf(BUF, SIZE, "%s%s%s%x%x%s", tempdir, filesep, prefix, pid, r1, fileext)
#else
	int r2 = rand();
# define TMPNAM2_SNPRINTF(BUF, SIZE) \
	snprintf(BUF, SIZE, "%s%s%s%x%x%x%s", tempdir, filesep, prefix, pid, r1, r2, fileext)
#endif
	size_t needed = TMPNAM2_SNPRINTF(NULL, 0) + 1;
#ifdef Unix
	if (needed > R_PATH_MAX)
	    error(_("temporary name too long"));
#endif
	char *res = (char *) malloc(needed);
	if(!res)
	    error(_("allocation failed in R_tmpnam2"));
	TMPNAM2_SNPRINTF(res, needed);
	if (!R_FileExists(res))
	    return res;
	free(res);
    }
    error(_("cannot find unused tempfile name"));
}

void R_free_tmpnam(char *name)
{
    if (name) free(name);
}

attribute_hidden SEXP do_proctime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nm;

    checkArity(op, args);
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

attribute_hidden void resetTimeLimits(void)
{
    double data[5];
    R_getProcTime(data);

    elapsedLimit = (elapsedLimitValue > 0) ? data[2] + elapsedLimitValue : -1.0;
    if (elapsedLimit2 > 0.0 &&
	(elapsedLimit <= 0.0 || elapsedLimit2 < elapsedLimit))
	elapsedLimit = elapsedLimit2;

#ifdef Win32
    cpuLimit = (cpuLimitValue > 0) ? data[0] + data[1] + cpuLimitValue : -1.0;
#else
    cpuLimit = (cpuLimitValue > 0) ? data[0] + data[1] + data[3] + data[4] + cpuLimitValue : -1.0;
#endif
    if (cpuLimit2 > 0.0 && (cpuLimit <= 0.0 || cpuLimit2 < cpuLimit))
	cpuLimit = cpuLimit2;
}

attribute_hidden SEXP
do_setTimeLimit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double cpu, elapsed, old_cpu = cpuLimitValue,
	old_elapsed = elapsedLimitValue;
    int transient;

    checkArity(op, args);
    cpu = asReal(CAR(args));
    elapsed = asReal(CADR(args));
    transient = asLogical(CADDR(args));

    if (R_FINITE(cpu) && cpu > 0) cpuLimitValue = cpu; else cpuLimitValue = -1;

    if (R_FINITE(elapsed) && elapsed > 0) elapsedLimitValue = elapsed;
    else elapsedLimitValue = -1;

    resetTimeLimits();

    if (transient == TRUE) {
	cpuLimitValue = old_cpu;
	elapsedLimitValue = old_elapsed;
    }

    return R_NilValue;
}

attribute_hidden SEXP
do_setSessionTimeLimit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double cpu, elapsed, data[5];

    checkArity(op, args);
    cpu = asReal(CAR(args));
    elapsed = asReal(CADR(args));
    R_getProcTime(data);

    if (R_FINITE(cpu) && cpu > 0)
#ifdef Win32
	cpuLimit2 = cpu + data[0] + data[1];
#else
	cpuLimit2 = cpu + data[0] + data[1] + data[3] + data[4];
#endif
    else cpuLimit2 = -1;

    if (R_FINITE(elapsed) && elapsed > 0) elapsedLimit2 = elapsed + data[2];
    else elapsedLimit2 = -1;

    return R_NilValue;
}

attribute_hidden void R_CheckTimeLimits(void)
{
    if (cpuLimit > 0.0 || elapsedLimit > 0.0) {

	/* On Linux and macOS at least R_getProcTime can be quite slow;
	   currentTIme is somewhat faster. */

	/* To reduce overhead, skip checking TIME_CHECK_SKIP times. */
	const int TIME_CHECK_SKIP = 5;
	static int check_count = 0;
	if (check_count < TIME_CHECK_SKIP) {
	    check_count++;
	    return;
	}
	else check_count = 0;

	/* Before calling R_getProcTime first use checkTime to make
	   sure at least TIME_CHECK_DELTA seconds have elapsed since
	   the last call. */
	const double TIME_CHECK_DELTA = 0.05;
	static double check_time = 0;
	double tm = currentTime();
	if (tm < check_time)
	    return;
	else check_time = tm + TIME_CHECK_DELTA;

	double cpu, data[5];
	R_getProcTime(data);
#ifdef Win32
	cpu = data[0] + data[1];
#else
	cpu = data[0] + data[1] + data[3] + data[4];
#endif
	if (elapsedLimit > 0.0 && data[2] > elapsedLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (elapsedLimit2 > 0.0 && data[2] > elapsedLimit2) {
		elapsedLimit2 = -1.0;
		error(_("reached session elapsed time limit"));
	    } else
		error(_("reached elapsed time limit"));
	}
	if (cpuLimit > 0.0 && cpu > cpuLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (cpuLimit2 > 0.0 && cpu > cpuLimit2) {
		cpuLimit2 = -1.0;
		error(_("reached session CPU time limit"));
	    } else
		error(_("reached CPU time limit"));
	}
    }
}

/* moved from character.c in 2.10.0: configure requires this */

#ifdef HAVE_GLOB_H
# include <glob.h>
#endif
#ifdef Win32
# include <dos_wglob.h>
# define globfree dos_wglobfree
# define glob_t wglob_t
#else
# ifndef GLOB_QUOTE
#  define GLOB_QUOTE 0
# endif
#endif
attribute_hidden SEXP do_glob(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    R_xlen_t i, n;
    int res, dirmark;
    bool initialized = false;
    glob_t globbuf;
#ifdef Win32
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
#endif

    checkArity(op, args);
    if (!isString(x = CAR(args)))
	error(_("invalid '%s' argument"), "paths");
    if (!XLENGTH(x)) return allocVector(STRSXP, 0);
    dirmark = asLogical(CADR(args));
    if (dirmark == NA_LOGICAL)
	error(_("invalid '%s' argument"), "dirmark");
#ifndef GLOB_MARK
    if (dirmark)
	error(_("'dirmark = TRUE' is not supported on this platform"));
#endif

    for (i = 0; i < XLENGTH(x); i++) {
	SEXP el = STRING_ELT(x, i);
	if (el == NA_STRING) continue;
#ifdef Win32
	res = dos_wglob(filenameToWchar(el, FALSE),
			(dirmark ? GLOB_MARK : 0) |
			GLOB_QUOTE | (initialized ? GLOB_APPEND : 0),
			NULL, &globbuf);
	if (res == GLOB_NOSPACE)
	    error(_("internal out-of-memory condition"));
#else
	res = glob(translateChar(el),
# ifdef GLOB_MARK
		   (dirmark ? GLOB_MARK : 0) |
# endif
		   GLOB_QUOTE | (initialized ? GLOB_APPEND : 0),
		   NULL, &globbuf);
# ifdef GLOB_ABORTED
	if (res == GLOB_ABORTED)
	    warning(_("read error on '%s'"), translateChar(el));
# endif
# ifdef GLOB_NOSPACE
	if (res == GLOB_NOSPACE)
	    error(_("internal out-of-memory condition"));
# endif
#endif
	initialized = true;
    }
    n = initialized ? globbuf.gl_pathc : 0;
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
#ifdef Win32
    {
	wchar_t *w = globbuf.gl_pathv[i];
	char *buf;
	size_t nb = wcstoutf8(NULL, w, (size_t)INT_MAX + 2);
	buf = R_AllocStringBuffer(nb, &cbuff);
	wcstoutf8(buf, w, nb);
	SET_STRING_ELT(ans, i, mkCharCE(buf, CE_UTF8));
    }
#else
	SET_STRING_ELT(ans, i, mkChar(globbuf.gl_pathv[i]));
#endif
    UNPROTECT(1);
#ifdef Win32
    R_FreeStringBufferL(&cbuff);
#endif
    if (initialized) globfree(&globbuf);
    return ans;
}

/* isatty is in unistd.h, or io.h on Windows */
#ifdef Win32
# include <io.h>
#endif

#ifdef Win32

attribute_hidden int R_is_redirection_tty(int fd)
{
    /* for now detects only msys/cygwin redirection tty */
    HANDLE h = (HANDLE) _get_osfhandle(fd);
    if (h == INVALID_HANDLE_VALUE || GetFileType(h) != FILE_TYPE_PIPE)
	return 0;
    FILE_NAME_INFO *fnInfo;

    /* find out the required FileNameLength */
    DWORD size = sizeof(FILE_NAME_INFO);
    if (!(fnInfo = (FILE_NAME_INFO*)malloc(size)))
	return 0;
    fnInfo->FileNameLength = 0; /* most likely not needed */
    BOOL r = GetFileInformationByHandleEx(h, FileNameInfo, fnInfo, size);
    if (r || GetLastError() != ERROR_MORE_DATA) {
	free(fnInfo);
	return 0;
    }
    /* use the right length */
    DWORD fnLength = fnInfo->FileNameLength; /* most likely not needed */
    size = sizeof(FILE_NAME_INFO) + fnLength;
    free(fnInfo);
    if (!(fnInfo = (FILE_NAME_INFO*)malloc(size)))
	return 0;
    fnInfo->FileNameLength = fnLength;
    r = GetFileInformationByHandleEx(h, FileNameInfo, fnInfo, size);
    int res = 0;
    if (r)
	/* note that fnInfo->FileName is not null terminated */
	/* e.g. msys-1888ae32e00d56aa-pty0-from-master,
	        cygwin-e022582115c10879-pty0-from-master */
	/* test borrowed from git */
	res = ((wcsstr(fnInfo->FileName, L"msys-") ||
	        wcsstr(fnInfo->FileName, L"cygwin-")) &&
		wcsstr(fnInfo->FileName, L"-pty"));
    free(fnInfo);
    return res;
}
#endif

attribute_hidden int R_isatty(int fd)
{
#ifdef Win32
    if (R_is_redirection_tty(fd))
	return 1;
#endif
    return isatty(fd);
}

