/*
  R : A Computer Language for Statistical Data Analysis
  Copyright (C) 1995-1996   Robert Gentleman and Ross Ihaka
  Copyright (C) 1997-2004   Robert Gentleman, Ross Ihaka
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307,
  U.S.A.
 */

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>
#include <R_ext/Riconv.h>

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

#ifdef HAVE_STAT
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if HAVE_AQUA
extern int (*ptr_CocoaSystem)(char*);
extern	Rboolean useaqua;
#endif

Rboolean R_FileExists(char *path)
{
    struct stat sb;
    return stat(R_ExpandFileName(path), &sb) == 0;
}

double R_FileMtime(char *path)
{
    struct stat sb;
    if (stat(R_ExpandFileName(path), &sb) != 0)
	error(_("cannot determine file modification time of '%s'"), path);
    return sb.st_mtime;
}
#else
Rboolean R_FileExists(char *path)
{
    error(_("file existence is not available on this system"));
}

double R_FileMtime(char *path)
{
    error(_("file modification time is not available on this system"));
    return 0.0; /* not reached */
}
#endif

    /*
     *  Unix file names which begin with "." are invisible.
     */

Rboolean R_HiddenFile(char *name)
{
    if (name && name[0] != '.') return 0;
    else return 1;
}


FILE *R_fopen(const char *filename, const char *mode)
{
    return(filename ? fopen(filename, mode) : NULL );
}

/*
 *  SYSTEM INFORMATION
 */

          /* The location of the R system files */

char *R_HomeDir()
{
    return getenv("R_HOME");
}


SEXP do_interactive(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    rval=allocVector(LGLSXP, 1);
    LOGICAL(rval)[0]= (R_Interactive) ? 1 : 0;
    return rval;
}

SEXP do_tempdir(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans;

    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans, 0, mkChar(R_TempDir));
    UNPROTECT(1);
    return (ans);
}


SEXP do_tempfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans, pattern, tempdir;
    char *tn, *td, *tm;
    int i, n1, n2, slen;

    checkArity(op, args);
    pattern = CAR(args); n1 = length(pattern);
    tempdir = CADR(args); n2 = length(tempdir);
    if (!isString(pattern))
        errorcall(call, _("invalid filename pattern"));
    if (!isString(tempdir))
        errorcall(call, _("invalid 'tempdir'"));
    if (n1 < 1)
	errorcall(call, _("no 'pattern'"));
    if (n2 < 1)
	errorcall(call, _("no 'tempdir'"));
    slen = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(STRSXP, slen));
    for(i = 0; i < slen; i++) {
	tn = CHAR( STRING_ELT( pattern , i%n1 ) );
	td = CHAR( STRING_ELT( tempdir , i%n2 ) );
	/* try to get a new file name */
	tm = R_tmpnam(tn, td);
	SET_STRING_ELT(ans, i, mkChar(tm));
	if(tm) free(tm);
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef HAVE_POPEN
FILE *R_popen(char *command, char *type)
{
    FILE *fp;
#ifdef __APPLE_CC__
    /* Luke recommends this to fix PR#1140 */
    sigset_t ss;
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

int R_system(char *command)
{
    int val;
#ifdef __APPLE_CC__
    /* Luke recommends this to fix PR#1140 */
    sigset_t ss;
    sigaddset(&ss, SIGPROF);
    sigprocmask(SIG_BLOCK, &ss,  NULL);
#ifdef HAVE_AQUA
	if(useaqua)
		val = ptr_CocoaSystem(command);
    else
#endif		
    val = system(command);
    sigprocmask(SIG_UNBLOCK, &ss, NULL);
#else
    val = system(command);
#endif
    return val;
}

#ifdef Win32
# include <windows.h>
#elif defined(__APPLE__)
# include <crt_externs.h>
# define environ (*_NSGetEnviron())
#else
extern char ** environ;
#endif

SEXP do_getenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    char *s;
    SEXP ans;

    checkArity(op, args);

    if (!isString(CAR(args)))
	errorcall(call, _("wrong type for argument"));

    i = LENGTH(CAR(args));
    if (i == 0) {
#ifdef Win32
	char *envir, *e;
	envir = (char *) GetEnvironmentStrings();
	for (i = 0, e = envir; strlen(e) > 0; i++, e += strlen(e)+1);
	PROTECT(ans = allocVector(STRSXP, i));
	for (i = 0, e = envir; strlen(e) > 0; i++, e += strlen(e)+1)
	    SET_STRING_ELT(ans, i, mkChar(e));
	FreeEnvironmentStrings(envir);
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
	    s = getenv(CHAR(STRING_ELT(CAR(args), j)));
	    if (s == NULL)
		SET_STRING_ELT(ans, j, mkChar(""));
	    else
		SET_STRING_ELT(ans, j, mkChar(s));
	}
    }
    UNPROTECT(1);
    return (ans);
}

#ifdef HAVE_PUTENV
static int Rputenv(char *str)
{
    char *buf;
    buf = (char *) malloc((strlen(str) + 1) * sizeof(char));
    if(!buf) return 1;
    strcpy(buf, str);
    putenv(buf);
    /* no free here: storage remains in use */
    return 0;
}
#endif


SEXP do_putenv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_PUTENV
    int i, n;
    SEXP ans, vars;

    checkArity(op, args);

    if (!isString(vars = CAR(args)))
	errorcall(call, _("wrong type for argument"));

    n = LENGTH(vars);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = Rputenv(CHAR(STRING_ELT(vars, i))) == 0;
    }
    UNPROTECT(1);
    return ans;
#else
    error(_("'putenv' is not available on this system"));
    return R_NilValue; /* -Wall */
#endif
}


/* Unfortunately glibc and Solaris diff in the const in the iconv decl.
   libiconv agrees with Solaris here.
 */
#ifdef HAVE_ICONV_H
#define const
#include <iconv.h>
#undef const
#endif

#ifdef Win32
DL_FUNC ptr_iconv, ptr_iconv_open, ptr_iconv_close, ptr_iconvlist;

static void iconv_Init(void)
{
    static int initialized = 0;
    char dllpath[PATH_MAX];
    snprintf(dllpath, PATH_MAX, "%s%smodules%s%s%s", getenv("R_HOME"), 
	     FILESEP, FILESEP, "iconv", SHLIB_EXT);
    if(!initialized) {
	int res = moduleCdynload("iconv", 1, 1);
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
#define iconv(a,b,c,d,e) ((size_t)(*ptr_iconv)(a,b,c,d,e))
#define iconv_open(a, b) ((iconv_t)(*ptr_iconv_open)(a,b))
#define iconv_close(a) ((int)(*ptr_iconv_close)(a))
#define iconvlist (*ptr_iconvlist)
#endif


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
SEXP do_iconv(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_ICONV) && defined(ICONV_LATIN1)
    SEXP ans, x = CAR(args);
    iconv_t obj;
    int i, j;
    char *inbuf; /* Solaris headers have const char*  here */
    char *outbuf;
    char *sub;
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
	if(TYPEOF(x) != STRSXP)
	    errorcall(call, _("'x' must be a character vector"));
	if(!isString(CADR(args)) || length(CADR(args)) != 1)
	    errorcall(call, _("invalid 'from' argument"));
	if(!isString(CADDR(args)) || length(CADDR(args)) != 1)
	    errorcall(call, _("invalid 'to' argument"));
	if(!isString(CADDDR(args)) || length(CADDDR(args)) != 1)
	    errorcall(call, _("invalid 'sub' argument"));
	if(STRING_ELT(CADDDR(args), 0) == NA_STRING) sub = NULL;
	else sub = CHAR(STRING_ELT(CADDDR(args), 0));

	obj = iconv_open(CHAR(STRING_ELT(CADDR(args), 0)),
			 CHAR(STRING_ELT(CADR(args), 0)));
	if(obj == (iconv_t)(-1))
	    errorcall(call, _("unsupported conversion"));
	PROTECT(ans = duplicate(x));
	R_AllocStringBuffer(0, &cbuff);  /* just default */
	for(i = 0; i < LENGTH(x); i++) {
	top_of_loop:
	    inbuf = CHAR(STRING_ELT(x, i)); inb = strlen(inbuf);
	    outbuf = cbuff.data; outb = cbuff.bufsize - 1;
	    /* First initialize output */
	    iconv (obj, NULL, NULL, &outbuf, &outb);
        next_char:
	    /* Then convert input  */
	    res = iconv(obj, &inbuf , &inb, &outbuf, &outb);
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
	
	    if(res != -1 && inb == 0)
		SET_STRING_ELT(ans, i, mkChar(cbuff.data));
	    else SET_STRING_ELT(ans, i, NA_STRING);
	}
	iconv_close(obj);
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
void * Riconv_open (char* tocode, char* fromcode)
{
#ifdef Win32
    iconv_Init();
#ifdef SUPPORT_UTF8
    if(!strcmp(tocode, ""))  return iconv_open("UTF-8", fromcode);
    else if(!!strcmp(fromcode, "")) return iconv_open(tocode, "UTF-8");
    else
#endif
	return iconv_open(tocode, fromcode);
#else
    return iconv_open(tocode, fromcode);
#endif
}

size_t Riconv (void *cd, char **inbuf, size_t *inbytesleft, 
	       char **outbuf, size_t *outbytesleft)
{
    return iconv((iconv_t) cd, inbuf, inbytesleft, outbuf, outbytesleft);
}

int Riconv_close (void *cd)
{
    return iconv_close((iconv_t) cd);
}
#else
void * Riconv_open (char* tocode, char* fromcode)
{
    error(_("'iconv' is not available on this system"));
    return (void *)-1;
}

size_t Riconv (void *cd, char **inbuf, size_t *inbytesleft, 
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
#endif
