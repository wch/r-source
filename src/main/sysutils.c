/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2007   Robert Gentleman, Ross Ihaka
                            and the R Development Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street Suite 330, Boston, MA 02111-1307,
  U.S.A.
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for putenv */
#include <Defn.h>
#include <R_ext/Riconv.h>
#include <Rinterface.h>

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


FILE *R_fopen(const char *filename, const char *mode)
{
    return(filename ? fopen(filename, mode) : NULL );
}

/* The point of this function is to allow file names in foreign
   character sets.  On Unix-alikes in a UTF-8 locale all that is
   needed is to convert file names to UTF-8, since they will be stored
   in UTF-8.  For other locales, it seems that there is no way to specify
   a file name in UTF-8.

   On NT-based versions of Windows, file names are stored in 'Unicode'
   (UCS-2), and _wfopen is provided to access them by UCS-2 names.
   This requires NT, so is currently disabled.
*/

#if 0 && defined(Win32)
FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand)
{
    wchar_t filename[MAX_PATH+1], wmode[10];
    void *obj;
    char *from = "", *inbuf, *outbuf;
    size_t inb, outb, res;
    
    if(IS_LATIN1(fn)) from = "latin1";
    if(IS_UTF8(fn)) from = "UTF=8";
    obj = Riconv_open("UCS-2LE", from);
    if(obj == (void *)(-1)) error(_("unsupported conversion"));

    if(expand) inbuf = R_ExpandFileName(CHAR(fn)); else inbuf = CHAR(fn);

    inb = strlen(inbuf)+1; outb = 2*(MAX_PATH+1);
    outbuf = (char *) filename;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(res == -1 || inb > 0) error(_("file name conversion problem"));

    mbstowcs(wmode, mode, 10);
    return _wfopen(filename, wmode);
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

char *R_HomeDir()
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

SEXP attribute_hidden do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s;
    SEXP ans;

    checkArity(op, args);

    if (!isString(CAR(args)))
	error(_("wrong type for argument"));

    if (!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error(_("wrong type for argument"));

    i = LENGTH(CAR(args));
    if (i == 0) {
	char **e;
	for (i = 0, e = environ; *e != NULL; i++, e++);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = environ; *e != NULL; i++, e++)
	    SET_STRING_ELT(ans, i, mkChar(*e));
    } else {
	PROTECT(ans = allocVector(STRSXP, i));
	for (j = 0; j < i; j++) {
	    s = getenv(translateChar(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, STRING_ELT(CADR(args), 0));
	    else {
		SEXP tmp = mkChar(s);
		if(known_to_be_latin1) SET_LATIN1(tmp);
		if(known_to_be_utf8) SET_UTF8(tmp);
		SET_STRING_ELT(ans, j, tmp);
	    }
	}
    }
    UNPROTECT(1);
    return (ans);
}

#if !defined(HAVE_SETENV) && defined(HAVE_PUTENV)
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
    for (i = 0; i < n; i++) putenv(translateChar(STRING_ELT(vars, i)));
#elif defined(HAVE_PUTENV_UNSET2)
    for (i = 0; i < n; i++) {
	char buf[1000];
	snprintf(buf, 1000, "%s=", translateChar(STRING_ELT(vars, i)));
	putenv(buf);
    }
#endif

#elif defined(HAVE_PUTENV) || defined(HAVE_SETENV)
    warning(_("this system cannot unset environment variables: setting to \"\""));
    n = LENGTH(vars);
    for (i = 0; i < n; i++) {
#ifdef HAVE_SETENV
	setenv(translateChar(STRING_ELT(vars, i)), "");
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


#if defined(HAVE_ICONV_H) && defined(ICONV_LATIN1) && !defined(Win32)
/* Unfortunately glibc and Solaris differ in the const in the iconv decl.
   libiconv agrees with Solaris here.
 */
# define const
# include <iconv.h>
# undef const
#endif

#ifdef Win32
static DL_FUNC ptr_iconv, ptr_iconv_open, ptr_iconv_close, ptr_iconvlist;

static void iconv_Init(void)
{
    static int initialized = 0;
    char dllpath[PATH_MAX];
    snprintf(dllpath, PATH_MAX, "%s%smodules%s%s%s", getenv("R_HOME"),
	     FILESEP, FILESEP, "iconv", SHLIB_EXT);
    if(!initialized) {
	int res = R_moduleCdynload("iconv", 1, 1);
	initialized = res ? 1 : -1;
	if(initialized > 0) {
	    ptr_iconv = R_FindSymbol("libiconv", "iconv", NULL);
	    ptr_iconv_open = R_FindSymbol("libiconv_open", "iconv", NULL);
	    ptr_iconv_close = R_FindSymbol("libiconv_close", "iconv", NULL);
	    ptr_iconvlist = R_FindSymbol("libiconvlist", "iconv", NULL);
	    if(!ptr_iconv)
		error(_("failed to find symbols in iconv.dll"));
	}
    }
    if(initialized < 0)
	error(_("iconv.dll is not available on this system"));
}
#undef iconv
#undef iconv_open
#undef iconv_close
#undef iconvlist
typedef void* iconv_t;
#define iconv(a,b,c,d,e) ((size_t)(*ptr_iconv)(a,b,c,d,e))
#define iconv_open(a, b) ((iconv_t)(*ptr_iconv_open)(a,b))
#define iconv_close(a) ((int)(*ptr_iconv_close)(a))
#define iconvlist (*ptr_iconvlist)
#endif /* Win32 */


#ifdef HAVE_ICONVLIST
static unsigned int cnt;

static int
count_one (unsigned int namescount, char * *names, void *data)
{
    cnt += namescount;
    return 0;
}

static int
write_one (unsigned int namescount, char * *names, void *data)
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
    SEXP ans, x = CAR(args);
    void * obj;
    int i, j;
    const char *inbuf; /* Solaris headers have const char*  here */
    char *outbuf;
    const char *sub;
    size_t inb, outb, res;
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    checkArity(op, args);
#ifdef Win32
    iconv_Init();
#endif
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
	top_of_loop:
	    inbuf = CHAR(STRING_ELT(x, i)); inb = strlen(inbuf);
	    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
	    /* First initialize output */
	    Riconv (obj, NULL, NULL, &outbuf, &outb);
        next_char:
	    /* Then convert input  */
	    res = iconv(obj, (char **) &inbuf , &inb, &outbuf, &outb);
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
		    for(j = 0; j < strlen(sub); j++) *outbuf++ = sub[j];
		    outb -= strlen(sub);
		}
		inbuf++; inb--;
		goto next_char;
	    }

	    if(res != -1 && inb == 0) {
		SET_STRING_ELT(ans, i, mkChar(cbuff.data));
		if(isLatin1) SET_LATIN1(STRING_ELT(ans, i));
		if(isUTF8) SET_UTF8(STRING_ELT(ans, i));
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

#if defined(HAVE_ICONV) && defined(ICONV_LATIN1)
void * Riconv_open (const char* tocode, const char* fromcode)
{
#ifdef Win32
    char *cp = "UTF-8";
    iconv_Init();
#ifndef SUPPORT_UTF8
    cp = locale2charset(NULL);
#endif
    if(strcmp(tocode, "") == 0)  return iconv_open(cp, fromcode);
    else if(strcmp(fromcode, "") == 0) return iconv_open(tocode, cp);
    else return iconv_open(tocode, fromcode);
#else
    /* const char * is right according to POSIX, but libiconv
       plays games so that on Solaris 10 it needs the casts */
    return iconv_open((char *)tocode, (char *)fromcode);
#endif
}

size_t Riconv (void *cd, const char **inbuf, size_t *inbytesleft,
	       char **outbuf, size_t *outbytesleft)
{
    return iconv((iconv_t) cd, (char **) inbuf, inbytesleft, 
		 outbuf, outbytesleft);
}

int Riconv_close (void *cd)
{
    return iconv_close((iconv_t) cd);
}

static void *latin1_obj = NULL, *utf8_obj=NULL;

const char *translateChar(SEXP x)
{
    void * obj;
    const char *inbuf, *ans = CHAR(x);
    char *outbuf, *p;
    size_t inb, outb, res;
    R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

    if(x == NA_STRING || !(IS_LATIN1(x) || IS_UTF8(x))) return ans;
    if(utf8locale && IS_UTF8(x)) return ans;
    if(latin1locale && IS_LATIN1(x)) return ans;
    if(utf8strIsASCII(CHAR(x))) return ans;

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
    res = strlen(cbuff.data) + 1;
    p = R_alloc(res, 1);
    memcpy(p, cbuff.data, res);
    R_FreeStringBuffer(&cbuff);
    return p;
}
#else
void * Riconv_open (const char* tocode, const char* fromcode)
{
    error(_("'iconv' is not available on this system"));
    return (void *)-1;
}

size_t Riconv (void *cd, const char *domain = "", const char **inbuf,
               size_t *inbytesleft, char **outbuf, size_t *outbytesleft)
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

#ifdef HAVE_GETTIMEOFDAY
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif
#else
# ifdef HAVE_TIME_H
# include <time.h>
# endif
#endif

void attribute_hidden InitRand()
{
    unsigned int seed;
#if HAVE_GETTIMEOFDAY
  {
    struct timeval tv;
    gettimeofday (&tv, NULL);
    seed = ((uint64_t) tv.tv_usec << 16) ^ tv.tv_sec;
  }
#elif HAVE_TIME
    seed = (unsigned int) time(NULL);
#else
    /* unlikely, but use random contents */
#endif
    srand(seed);
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
    SEXP ans = allocVector(REALSXP, 5), nm = allocVector(STRSXP, 5);
    R_getProcTime(REAL(ans));
    SET_STRING_ELT(nm, 0, mkChar("user.self"));
    SET_STRING_ELT(nm, 1, mkChar("sys.self"));
    SET_STRING_ELT(nm, 2, mkChar("elapsed"));
    SET_STRING_ELT(nm, 3, mkChar("user.child"));
    SET_STRING_ELT(nm, 4, mkChar("sys.child"));
    setAttrib(ans, R_NamesSymbol, nm);
    setAttrib(ans, R_ClassSymbol, mkString("proc_time"));
    return ans;
}
#else
{
    error(_("proc.time() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif
