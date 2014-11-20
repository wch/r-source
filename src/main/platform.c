/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2014 The R Core Team
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


/* Notes on so-called 'Large File Support'

   The 'stat' structure returns a file size as 'off_t'.  On some
   32-bit systems this will fail if called on a file > 2GB.  On
   systems with LFS selected (see the notes in connections.c) the call
   is re-mapped to *stat64, which uses off64_t for the file size.

   file.info() returns file sizes as an R double.

   On Windows we need to remap for ourselves.  There are various
   versions of the 'stat' structure (some with 64-bit times and not
   available in the original MSVCRT.dll): we use _stati64 that simply
   replaces off_t by __int64_t.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <ctype.h>			/* toupper */
#include <time.h>			/* for ctime */

# include <errno.h>

/* Machine Constants */

static void
machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep, int *negep,
       int *iexp, int *minexp, int *maxexp, double *eps,
       double *epsneg, double *xmin, double *xmax);

static void Init_R_Machine(SEXP rho)
{
    SEXP ans, nms;

    machar(&R_AccuracyInfo.ibeta,
	   &R_AccuracyInfo.it,
	   &R_AccuracyInfo.irnd,
	   &R_AccuracyInfo.ngrd,
	   &R_AccuracyInfo.machep,
	   &R_AccuracyInfo.negep,
	   &R_AccuracyInfo.iexp,
	   &R_AccuracyInfo.minexp,
	   &R_AccuracyInfo.maxexp,
	   &R_AccuracyInfo.eps,
	   &R_AccuracyInfo.epsneg,
	   &R_AccuracyInfo.xmin,
	   &R_AccuracyInfo.xmax);

    R_dec_min_exponent = (int) floor(log10(R_AccuracyInfo.xmin)); /* smallest decimal exponent */
    PROTECT(ans = allocVector(VECSXP, 18));
    PROTECT(nms = allocVector(STRSXP, 18));
    SET_STRING_ELT(nms, 0, mkChar("double.eps"));
    SET_VECTOR_ELT(ans, 0, ScalarReal(R_AccuracyInfo.eps));

    SET_STRING_ELT(nms, 1, mkChar("double.neg.eps"));
    SET_VECTOR_ELT(ans, 1, ScalarReal(R_AccuracyInfo.epsneg));

    SET_STRING_ELT(nms, 2, mkChar("double.xmin"));
    SET_VECTOR_ELT(ans, 2, ScalarReal(R_AccuracyInfo.xmin));

    SET_STRING_ELT(nms, 3, mkChar("double.xmax"));
    SET_VECTOR_ELT(ans, 3, ScalarReal(R_AccuracyInfo.xmax));

    SET_STRING_ELT(nms, 4, mkChar("double.base"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(R_AccuracyInfo.ibeta));

    SET_STRING_ELT(nms, 5, mkChar("double.digits"));
    SET_VECTOR_ELT(ans, 5, ScalarInteger(R_AccuracyInfo.it));

    SET_STRING_ELT(nms, 6, mkChar("double.rounding"));
    SET_VECTOR_ELT(ans, 6, ScalarInteger(R_AccuracyInfo.irnd));

    SET_STRING_ELT(nms, 7, mkChar("double.guard"));
    SET_VECTOR_ELT(ans, 7, ScalarInteger(R_AccuracyInfo.ngrd));

    SET_STRING_ELT(nms, 8, mkChar("double.ulp.digits"));
    SET_VECTOR_ELT(ans, 8, ScalarInteger(R_AccuracyInfo.machep));

    SET_STRING_ELT(nms, 9, mkChar("double.neg.ulp.digits"));
    SET_VECTOR_ELT(ans, 9, ScalarInteger(R_AccuracyInfo.negep));

    SET_STRING_ELT(nms, 10, mkChar("double.exponent"));
    SET_VECTOR_ELT(ans, 10, ScalarInteger(R_AccuracyInfo.iexp));

    SET_STRING_ELT(nms, 11, mkChar("double.min.exp"));
    SET_VECTOR_ELT(ans, 11, ScalarInteger(R_AccuracyInfo.minexp));

    SET_STRING_ELT(nms, 12, mkChar("double.max.exp"));
    SET_VECTOR_ELT(ans, 12, ScalarInteger(R_AccuracyInfo.maxexp));

    SET_STRING_ELT(nms, 13, mkChar("integer.max"));
    SET_VECTOR_ELT(ans, 13, ScalarInteger(INT_MAX));

    SET_STRING_ELT(nms, 14, mkChar("sizeof.long"));
    SET_VECTOR_ELT(ans, 14, ScalarInteger(SIZEOF_LONG));

    SET_STRING_ELT(nms, 15, mkChar("sizeof.longlong"));
    SET_VECTOR_ELT(ans, 15, ScalarInteger(SIZEOF_LONG_LONG));

    SET_STRING_ELT(nms, 16, mkChar("sizeof.longdouble"));
#ifdef HAVE_LONG_DOUBLE
    SET_VECTOR_ELT(ans, 16, ScalarInteger(SIZEOF_LONG_DOUBLE));
#else
    SET_VECTOR_ELT(ans, 16, ScalarInteger(0));
#endif

    SET_STRING_ELT(nms, 17, mkChar("sizeof.pointer"));
    SET_VECTOR_ELT(ans, 17, ScalarInteger(sizeof(SEXP)));
    setAttrib(ans, R_NamesSymbol, nms);
    defineVar(install(".Machine"), ans, rho);
    UNPROTECT(2);
}


/*  Platform
 *
 *  Return various platform dependent strings.  This is similar to
 *  "Machine", but for strings rather than numerical values.  These
 *  two functions should probably be amalgamated.
 */
static const char  * const R_OSType = OSTYPE;
static const char  * const R_FileSep = FILESEP;

static void Init_R_Platform(SEXP rho)
{
    SEXP value, names;

    PROTECT(value = allocVector(VECSXP, 8));
    PROTECT(names = allocVector(STRSXP, 8));
    SET_STRING_ELT(names, 0, mkChar("OS.type"));
    SET_STRING_ELT(names, 1, mkChar("file.sep"));
    SET_STRING_ELT(names, 2, mkChar("dynlib.ext"));
    SET_STRING_ELT(names, 3, mkChar("GUI"));
    SET_STRING_ELT(names, 4, mkChar("endian"));
    SET_STRING_ELT(names, 5, mkChar("pkgType"));
    SET_STRING_ELT(names, 6, mkChar("path.sep"));
    SET_STRING_ELT(names, 7, mkChar("r_arch"));
    SET_VECTOR_ELT(value, 0, mkString(R_OSType));
    SET_VECTOR_ELT(value, 1, mkString(R_FileSep));
    SET_VECTOR_ELT(value, 2, mkString(SHLIB_EXT));
    SET_VECTOR_ELT(value, 3, mkString(R_GUIType));
#ifdef WORDS_BIGENDIAN
    SET_VECTOR_ELT(value, 4, mkString("big"));
#else
    SET_VECTOR_ELT(value, 4, mkString("little"));
#endif
/* pkgType should be "mac.binary" for CRAN build *only*, not for all
   AQUA builds. Also we want to be able to use "mac.binary.leopard",
   "mac.binary.mavericks" and similar. */
#ifdef PLATFORM_PKGTYPE
    SET_VECTOR_ELT(value, 5, mkString(PLATFORM_PKGTYPE));
#else /* unix default */
    SET_VECTOR_ELT(value, 5, mkString("source"));
#endif
#ifdef Win32
    SET_VECTOR_ELT(value, 6, mkString(";"));
#else /* not Win32 */
    SET_VECTOR_ELT(value, 6, mkString(":"));
#endif
#ifdef R_ARCH
    SET_VECTOR_ELT(value, 7, mkString(R_ARCH));
#else
    SET_VECTOR_ELT(value, 7, mkString(""));
#endif
    setAttrib(value, R_NamesSymbol, names);
    defineVar(install(".Platform"), value, rho);
    UNPROTECT(2);
}

void attribute_hidden Init_R_Variables(SEXP rho)
{
    Init_R_Machine(rho);
    Init_R_Platform(rho);
}

#ifdef HAVE_LANGINFO_CODESET
/* case-insensitive string comparison (needed for locale check) */
int static R_strieql(const char *a, const char *b)
{
    while (*a && *b && toupper(*a) == toupper(*b)) { a++; b++; }
    return (*a == 0 && *b == 0);
}
#endif

#include <locale.h>
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

/* retrieves information about the current locale and
   sets the corresponding variables (known_to_be_utf8,
   known_to_be_latin1, utf8locale, latin1locale and mbcslocale) */
void attribute_hidden R_check_locale(void)
{
    known_to_be_utf8 = utf8locale = FALSE;
    known_to_be_latin1 = latin1locale = FALSE;
    mbcslocale = FALSE;
#ifdef HAVE_LANGINFO_CODESET
    {
	char  *p = nl_langinfo(CODESET);
	/* more relaxed due to Darwin: CODESET is case-insensitive and
	   latin1 is ISO8859-1 */
	if (R_strieql(p, "UTF-8")) known_to_be_utf8 = utf8locale = TRUE;
	if (streql(p, "ISO-8859-1")) known_to_be_latin1 = latin1locale = TRUE;
	if (R_strieql(p, "ISO8859-1")) known_to_be_latin1 = latin1locale = TRUE;
# if __APPLE__
	/* On Darwin 'regular' locales such as 'en_US' are UTF-8 (hence
	   MB_CUR_MAX == 6), but CODESET is "" */
	if (*p == 0 && MB_CUR_MAX == 6)
	    known_to_be_utf8 = utf8locale = TRUE;
# endif
    }
#endif
    mbcslocale = MB_CUR_MAX > 1;
#ifdef Win32
    {
	char *ctype = setlocale(LC_CTYPE, NULL), *p;
	p = strrchr(ctype, '.');
	if (p && isdigit(p[1])) localeCP = atoi(p+1); else localeCP = 0;
	/* Not 100% correct, but CP1252 is a superset */
	known_to_be_latin1 = latin1locale = (localeCP == 1252);
    }
#endif
#if defined(SUPPORT_UTF8_WIN32) /* never at present */
    utf8locale = mbcslocale = TRUE;
#endif
}

/*  date
 *
 *  Return the current date in a standard format.  This uses standard
 *  POSIX calls which should be available on each platform.  We should
 *  perhaps check this in the configure script.
 */
/* BDR 2000/7/20.
 *  time and ctime are in fact ANSI C calls, so we don't check them.
 */
static char *R_Date(void)
{
    time_t t;
    static char s[26];		/* own space */

    time(&t);
    strcpy(s, ctime(&t));
    s[24] = '\0';		/* overwriting the final \n */
    return s;
}

SEXP attribute_hidden do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString(R_Date());
}

/*  file.show
 *
 *  Display file(s) so that a user can view it.  The function calls
 *  "R_ShowFiles" which is a platform-dependent hook that arranges
 *  for the file(s) to be displayed.
 */

// .Internal so manages R_alloc stack used by acopy_string
SEXP attribute_hidden do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl, hd, pg;
    const char **f, **h, *t, *pager = NULL /* -Wall */;
    Rboolean dl;
    int i, n;

    checkArity(op, args);
    fn = CAR(args); args = CDR(args);
    hd = CAR(args); args = CDR(args);
    tl = CAR(args); args = CDR(args);
    dl = (Rboolean) asLogical(CAR(args)); args = CDR(args);
    pg = CAR(args);
    n = 0;			/* -Wall */
    if (!isString(fn) || (n = length(fn)) < 1)
	error(_("invalid filename specification"));
    if (!isString(hd) || length(hd) != n)
	error(_("invalid '%s' argument"), "headers");
    if (!isString(tl))
	error(_("invalid '%s' argument"), "title");
    if (!isString(pg))
	error(_("invalid '%s' argument"), "pager");
    f = (const char**) R_alloc(n, sizeof(char*));
    h = (const char**) R_alloc(n, sizeof(char*));
    for (i = 0; i < n; i++) {
	SEXP el = STRING_ELT(fn, i);
	if (!isNull(el) && el != NA_STRING)
#ifdef Win32
	    f[i] = acopy_string(reEnc(CHAR(el), getCharCE(el), CE_UTF8, 1));
#else
	    f[i] = acopy_string(translateChar(el));
#endif
	else
            error(_("invalid filename specification"));
	if (STRING_ELT(hd, i) != NA_STRING)
	    h[i] = acopy_string(translateChar(STRING_ELT(hd, i)));
	else
            error(_("invalid '%s' argument"), "headers");
    }
    if (isValidStringF(tl))
	t = acopy_string(translateChar(STRING_ELT(tl, 0)));
    else
	t = "";
    if (isValidStringF(pg)) {
	SEXP pg0 = STRING_ELT(pg, 0);
        if (pg0 != NA_STRING)
            pager = acopy_string(CHAR(pg0));
        else
            error(_("invalid '%s' argument"), "pager");
    } else
	pager = "";
    R_ShowFiles(n, f, h, t, dl, pager);
    return R_NilValue;
}

/*  file.append
 *
 *  Given two vectors of file names as arguments and arranges for
 *  the second set of files to be appended to the first.
 */

#if defined(BUFSIZ) && (BUFSIZ > 512)
/* OS's buffer size in stdio.h, probably.
   Windows has 512, Solaris 1024, glibc 8192
 */
# define APPENDBUFSIZE BUFSIZ
#else
# define APPENDBUFSIZE 512
#endif

static int R_AppendFile(SEXP file1, SEXP file2)
{
    FILE *fp1, *fp2;
    char buf[APPENDBUFSIZE];
    size_t nchar;
    int status = 0;
    if ((fp1 = RC_fopen(file1, "ab", TRUE)) == NULL) return 0;
    if ((fp2 = RC_fopen(file2, "rb", TRUE)) == NULL) {
	fclose(fp1);
	return 0;
    }
    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
	if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE) goto append_error;
    if (fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
    status = 1;
 append_error:
    if (status == 0) warning(_("write error during file append"));
    fclose(fp1);
    fclose(fp2);
    return status;
}

SEXP attribute_hidden do_fileappend(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int n, n1, n2;

    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "file1");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "file2");
    if (n1 < 1)
	error(_("nothing to append to"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));
    for (int i = 0; i < n; i++) LOGICAL(ans)[i] = 0;  /* all FALSE */
    if (n1 == 1) { /* common case */
	FILE *fp1, *fp2;
	char buf[APPENDBUFSIZE];
	int status = 0;
	size_t nchar;
	if (STRING_ELT(f1, 0) == NA_STRING ||
	    !(fp1 = RC_fopen(STRING_ELT(f1, 0), "ab", TRUE)))
	   goto done;
	for (int i = 0; i < n; i++) {
	    status = 0;
	    if (STRING_ELT(f2, i) == NA_STRING ||
	       !(fp2 = RC_fopen(STRING_ELT(f2, i), "rb", TRUE))) continue;
	    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE)
		    goto append_error;
	    if (fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
	    status = 1;
	append_error:
	    if (status == 0)
		warning(_("write error during file append"));
	    LOGICAL(ans)[i] = status;
	    fclose(fp2);
	}
	fclose(fp1);
    } else {
	for (int i = 0; i < n; i++) {
	    if (STRING_ELT(f1, i%n1) == R_NilValue ||
		STRING_ELT(f2, i%n2) == R_NilValue)
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] =
		    R_AppendFile(STRING_ELT(f1, i%n1), STRING_ELT(f2, i%n2));
	}
    }
done:
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_filecreate(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    FILE *fp;
    int i, n, show;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid filename argument"));
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    n = length(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(fn, i) == NA_STRING) continue;
	if ((fp = RC_fopen(STRING_ELT(fn, i), "w", TRUE)) != NULL) {
	    LOGICAL(ans)[i] = 1;
	    fclose(fp);
	} else if (show) {
	    warning(_("cannot create file '%s', reason '%s'"),
		    translateChar(STRING_ELT(fn, i)), strerror(errno));
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_fileremove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f, ans;
    int i, n;
    checkArity(op, args);
    f = CAR(args);
    if (!isString(f))
	error(_("invalid first filename"));
    n = length(f);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f, i) != NA_STRING) {
	    LOGICAL(ans)[i] =
#ifdef Win32
		(_wremove(filenameToWchar(STRING_ELT(f, i), TRUE)) == 0);
#else
		(remove(R_ExpandFileName(translateChar(STRING_ELT(f, i)))) == 0);
#endif
	    if(!LOGICAL(ans)[i])
		warning(_("cannot remove file '%s', reason '%s'"),
			translateChar(STRING_ELT(f, i)), strerror(errno));
	} else LOGICAL(ans)[i] = FALSE;
    }
    UNPROTECT(1);
    return ans;
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for symlink, getpid */
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#ifdef Win32
/* Mingw-w64 defines this to be 0x0502 */
#ifndef _WIN32_WINNT
# define _WIN32_WINNT 0x0500 /* for CreateHardLink */
#endif
#include <windows.h>
typedef BOOLEAN (WINAPI *PCSL)(LPWSTR, LPWSTR, DWORD);
static PCSL pCSL = NULL;
const char *formatError(DWORD res);  /* extra.c */
/* Windows does not have link(), but it does have CreateHardLink() on NTFS */
#undef HAVE_LINK
#define HAVE_LINK 1
/* Windows does not have symlink(), but >= Vista does have
   CreateSymbolicLink() on NTFS */
#undef HAVE_SYMLINK
#define HAVE_SYMLINK 1
#endif

/* the Win32 stuff here is not ready for release:

   (i) It needs Windows >= Vista
   (ii) It matters whether 'from' is a file or a dir, and we could only
   know if it exists already.
   (iii) This needs specific privileges which in general only Adminstrators
   have, and which many people report granting in the Policy Editor
   fails to work.
*/
SEXP attribute_hidden do_filesymlink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_SYMLINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    if (n1 < 1)
	error(_("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;

#ifdef Win32
    // Vista, Server 2008 and later
    pCSL = (PCSL) GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
				 "CreateSymbolicLinkW");
    if(!pCSL)
	error(_("symbolic links are not supported on this version of Windows"));
#endif

#ifdef HAVE_SYMLINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f1, i%n1) == NA_STRING ||
	    STRING_ELT(f2, i%n2) == NA_STRING)
	    LOGICAL(ans)[i] = 0;
	else {
#ifdef Win32
	    wchar_t from[PATH_MAX+1], *to;
	    struct _stati64 sb;
	    from[PATH_MAX] = L'\0';
	    wcsncpy(from, filenameToWchar(STRING_ELT(f1, i%n1), TRUE), PATH_MAX);
	    /* This Windows system call does not accept slashes */
	    for (wchar_t *p = from; *p; p++) if (*p == L'/') *p = L'\\';
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    _wstati64(from, &sb);
	    int isDir = (sb.st_mode & S_IFDIR) > 0;
	    LOGICAL(ans)[i] = pCSL(to, from, isDir) != 0;
	    if(!LOGICAL(ans)[i])
		warning(_("cannot symlink '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
#else
	    char from[PATH_MAX], to[PATH_MAX];
	    const char *p;
	    p = R_ExpandFileName(translateChar(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);
	    p = R_ExpandFileName(translateChar(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(to, p);
	    /* Rprintf("linking %s to %s\n", from, to); */
	    LOGICAL(ans)[i] = symlink(from, to) == 0;
	    if(!LOGICAL(ans)[i])
		warning(_("cannot symlink '%s' to '%s', reason '%s'"),
			from, to, strerror(errno));
#endif
	}
    }
    UNPROTECT(1);
    return ans;
#else
    warning(_("symbolic links are not supported on this platform"));
    return allocVector(LGLSXP, n);
#endif
}


SEXP attribute_hidden do_filelink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_LINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    if (n1 < 1)
	error(_("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
#ifdef HAVE_LINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f1, i%n1) == NA_STRING ||
	    STRING_ELT(f2, i%n2) == NA_STRING)
	    LOGICAL(ans)[i] = 0;
	else {
#ifdef Win32
	    wchar_t from[PATH_MAX+1], *to;
	    from[PATH_MAX] = L'\0';
	    wcsncpy(from, filenameToWchar(STRING_ELT(f1, i%n1), TRUE), PATH_MAX);
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    LOGICAL(ans)[i] = CreateHardLinkW(to, from, NULL) != 0;
	    if(!LOGICAL(ans)[i]) {
		warning(_("cannot link '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
	    }
#else
	    char from[PATH_MAX], to[PATH_MAX];
	    const char *p;
	    p = R_ExpandFileName(translateChar(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);
	    p = R_ExpandFileName(translateChar(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(to, p);
	    LOGICAL(ans)[i] = link(from, to) == 0;
	    if(!LOGICAL(ans)[i]) {
		warning(_("cannot link '%s' to '%s', reason '%s'"),
			from, to, strerror(errno));
	    }
#endif
	}
    }
    UNPROTECT(1);
    return ans;
#else
    warning(_("(hard) links are not supported on this platform"));
    return allocVector(LGLSXP, n);
#endif
}

#ifdef Win32
int Rwin_rename(char *from, char *to);  /* in src/gnuwin32/extra.c */
int Rwin_wrename(const wchar_t *from, const wchar_t *to);
#endif

SEXP attribute_hidden do_filerename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n1, n2;
#ifdef Win32
    wchar_t from[PATH_MAX], to[PATH_MAX];
    const wchar_t *w;
#else
    char from[PATH_MAX], to[PATH_MAX];
    const char *p;
    int res;
#endif

    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "from");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "to");
    if (n2 != n1)
	error(_("'from' and 'to' are of different lengths"));
    PROTECT(ans = allocVector(LGLSXP, n1));
    for (i = 0; i < n1; i++) {
	if (STRING_ELT(f1, i) == NA_STRING ||
	    STRING_ELT(f2, i) == NA_STRING) {
	    LOGICAL(ans)[i] = 0;
	    continue;
	}
#ifdef Win32
	w = filenameToWchar(STRING_ELT(f1, i), TRUE);
	if (wcslen(w) >= PATH_MAX - 1)
	    error(_("expanded 'from' name too long"));
	wcsncpy(from, w, PATH_MAX - 1);
	w = filenameToWchar(STRING_ELT(f2, i), TRUE);
	if (wcslen(w) >= PATH_MAX - 1)
	    error(_("expanded 'to' name too long"));
	wcsncpy(to, w, PATH_MAX - 1);
	LOGICAL(ans)[i] = (Rwin_wrename(from, to) == 0);
#else
	p = R_ExpandFileName(translateChar(STRING_ELT(f1, i)));
	if (strlen(p) >= PATH_MAX - 1)
	    error(_("expanded 'from' name too long"));
	strncpy(from, p, PATH_MAX - 1);
	p = R_ExpandFileName(translateChar(STRING_ELT(f2, i)));
	if (strlen(p) >= PATH_MAX - 1)
	    error(_("expanded 'to' name too long"));
	strncpy(to, p, PATH_MAX - 1);
	res = rename(from, to);
	if(res) {
	    warning(_("cannot rename file '%s' to '%s', reason '%s'"),
		    from, to, strerror(errno));
	}
	LOGICAL(ans)[i] = (res == 0);
#endif
    }
    UNPROTECT(1);
    return ans;
}

# if defined(Unix) && defined(HAVE_PWD_H) && defined(HAVE_GRP_H) \
  && defined(HAVE_GETPWUID) && defined(HAVE_GETGRGID)
#  include <pwd.h>
#  include <grp.h>
#  define UNIX_EXTRAS 1
# endif

#ifdef Win32
# ifndef SCS_64BIT_BINARY
#  define SCS_64BIT_BINARY 6
# endif
#endif

#if defined HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC
# ifdef TYPEOF_STRUCT_STAT_ST_ATIM_IS_STRUCT_TIMESPEC
#  define STAT_TIMESPEC(st, st_xtim) ((st).st_xtim)
# else
#  define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim.tv_nsec)
# endif
#elif defined HAVE_STRUCT_STAT_ST_ATIMESPEC_TV_NSEC
# define STAT_TIMESPEC(st, st_xtim) ((st).st_xtim##espec)
#elif defined HAVE_STRUCT_STAT_ST_ATIMENSEC
# define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim##ensec)
#elif defined HAVE_STRUCT_STAT_ST_ATIM_ST__TIM_TV_NSEC
# define STAT_TIMESPEC_NS(st, st_xtim) ((st).st_xtim.st__tim.tv_nsec)
#endif

SEXP attribute_hidden do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans, ansnames, fsize, mtime, ctime, atime, isdir,
	mode, xxclass;
#ifdef UNIX_EXTRAS
    SEXP uid = R_NilValue, gid = R_NilValue, 
	uname = R_NilValue, grname = R_NilValue; // silence -Wall
#endif
#ifdef Win32
    SEXP exe = R_NilValue;
    struct _stati64 sb;
#else
    struct stat sb;
#endif

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid filename argument"));
    int extras = asInteger(CADR(args));
    if(extras == NA_INTEGER)
	error(_("invalid '%s' argument"), "extra_cols");
    int n = length(fn), ncols = 6;
    if(extras) {
#ifdef UNIX_EXTRAS
	ncols = 10;
#elif defined(Win32)
	ncols = 7;
#endif
    }
    PROTECT(ans = allocVector(VECSXP, ncols));
    PROTECT(ansnames = allocVector(STRSXP, ncols));
    fsize = SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 0, mkChar("size"));
    isdir = SET_VECTOR_ELT(ans, 1, allocVector(LGLSXP, n));
    SET_STRING_ELT(ansnames, 1, mkChar("isdir"));
    mode  = SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, n));
    SET_STRING_ELT(ansnames, 2, mkChar("mode"));
    mtime = SET_VECTOR_ELT(ans, 3, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 3, mkChar("mtime"));
    ctime = SET_VECTOR_ELT(ans, 4, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 4, mkChar("ctime"));
    atime = SET_VECTOR_ELT(ans, 5, allocVector(REALSXP, n));
    SET_STRING_ELT(ansnames, 5, mkChar("atime"));
    if (extras) {
#ifdef UNIX_EXTRAS
	uid = SET_VECTOR_ELT(ans, 6, allocVector(INTSXP, n));
	SET_STRING_ELT(ansnames, 6, mkChar("uid"));
	gid = SET_VECTOR_ELT(ans, 7, allocVector(INTSXP, n));
	SET_STRING_ELT(ansnames, 7, mkChar("gid"));
	uname = SET_VECTOR_ELT(ans, 8, allocVector(STRSXP, n));
	SET_STRING_ELT(ansnames, 8, mkChar("uname"));
	grname = SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, n));
	SET_STRING_ELT(ansnames, 9, mkChar("grname"));
#endif
#ifdef Win32
	exe = SET_VECTOR_ELT(ans, 6, allocVector(STRSXP, n));
	SET_STRING_ELT(ansnames, 6, mkChar("exe"));
#endif
    }
    for (int i = 0; i < n; i++) {
#ifdef Win32
	wchar_t *wfn = filenameToWchar(STRING_ELT(fn, i), TRUE);
	/* trailing \ is not valid on Windows except for the
	   root directory on a drive, specified as "\", or "D:\",
	   or "\\?\D:\", etc.  We remove it in other cases,
	   to help those who think they're on Unix. */
	size_t len = wcslen(wfn);
	if (len) {
	    wchar_t *p = wfn + (len - 1);
            if (len > 1 && (*p == L'/' || *p == L'\\') &&
            	*(p-1) != L':') *p = 0;
        }
#else
	const char *efn = R_ExpandFileName(translateChar(STRING_ELT(fn, i)));
#endif
	if (STRING_ELT(fn, i) != NA_STRING &&
#ifdef Win32
	    _wstati64(wfn, &sb)
#else
	    /* Target not link */
	    stat(efn, &sb)
#endif
	    == 0) {
	    REAL(fsize)[i] = (double) sb.st_size;
	    LOGICAL(isdir)[i] = (sb.st_mode & S_IFDIR) > 0;
	    INTEGER(mode)[i]  = (int) sb.st_mode & 0007777;

#if defined STAT_TIMESPEC
	    /* POSIX 2008 changed this to a struct timespec st_mtim etc
	       Not all OSes (e.g. Darwin) agree on this. */
	    REAL(mtime)[i] = (double) STAT_TIMESPEC(sb, st_mtim).tv_sec
		+ 1e-9 * (double) STAT_TIMESPEC(sb, st_mtim).tv_nsec;
	    REAL(ctime)[i] = (double) STAT_TIMESPEC(sb, st_ctim).tv_sec
		+ 1e-9 * (double) STAT_TIMESPEC(sb, st_ctim).tv_nsec;
	    REAL(atime)[i] = (double) STAT_TIMESPEC(sb, st_atim).tv_sec
		+ 1e-9 * (double) STAT_TIMESPEC(sb, st_atim).tv_nsec;
#else
	    /* FIXME: there are higher-resolution ways to do this on Windows */
	    REAL(mtime)[i] = (double) sb.st_mtime;
	    REAL(ctime)[i] = (double) sb.st_ctime;
	    REAL(atime)[i] = (double) sb.st_atime;
# ifdef STAT_TIMESPEC_NS
	    REAL(mtime)[i] += STAT_TIMESPEC_NS (sb, st_mtim);
	    REAL(ctime)[i] += STAT_TIMESPEC_NS (sb, st_ctim);
	    REAL(atime)[i] += STAT_TIMESPEC_NS (sb, st_atim);
# endif
#endif
	    if (extras) {
#ifdef UNIX_EXTRAS
		INTEGER(uid)[i] = (int) sb.st_uid;
		INTEGER(gid)[i] = (int) sb.st_gid;

		/* Usually all of the uid and gid values in a list of
		 * files are the same so we can avoid most of the calls
		 * to getpwuid() and getgrgid(), which can be quite slow
		 * on some systems.  (PR#15804)
		 */
		if (i && INTEGER(uid)[i - 1] == (int) sb.st_uid)
		    SET_STRING_ELT(uname, i, STRING_ELT(uname, i - 1));
		else {
		    struct passwd *stpwd = getpwuid(sb.st_uid);
		    SET_STRING_ELT(uname, i,
				   stpwd ? mkChar(stpwd->pw_name): NA_STRING);
		}

		if (i && INTEGER(gid)[i - 1] == (int) sb.st_gid)
		    SET_STRING_ELT(grname, i, STRING_ELT(grname, i - 1));
		else {
		    struct group *stgrp = getgrgid(sb.st_gid);
		    SET_STRING_ELT(grname, i, 
				   stgrp ? mkChar(stgrp->gr_name): NA_STRING);
		}
#endif
#ifdef Win32
		{
		    char *s="no";
		    DWORD type;
		    if (GetBinaryTypeW(wfn, &type))
			switch(type) {
			case SCS_64BIT_BINARY:
			    s = "win64";
			    break;
			case SCS_32BIT_BINARY:
			    s = "win32";
			    break;
			case SCS_DOS_BINARY:
			case SCS_PIF_BINARY:
			    s = "msdos";
			    break;
			case SCS_WOW_BINARY:
			    s = "win16";
			    break;
			default:
			    s = "unknown";
			}
		    SET_STRING_ELT(exe, i, mkChar(s));
		}
#endif
	    }
	} else {
	    REAL(fsize)[i] = NA_REAL;
	    LOGICAL(isdir)[i] = NA_INTEGER;
	    INTEGER(mode)[i]  = NA_INTEGER;
	    REAL(mtime)[i] = NA_REAL;
	    REAL(ctime)[i] = NA_REAL;
	    REAL(atime)[i] = NA_REAL;
	    if (extras) {
#ifdef UNIX_EXTRAS
		INTEGER(uid)[i] = NA_INTEGER;
		INTEGER(gid)[i] = NA_INTEGER;
		SET_STRING_ELT(uname, i, NA_STRING);
		SET_STRING_ELT(grname, i, NA_STRING);
#endif
#ifdef Win32
		SET_STRING_ELT(exe, i, NA_STRING);
#endif
	    }
	}
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(xxclass = mkString("octmode"));
    classgets(mode, xxclass);
    UNPROTECT(3);
    return ans;
}

SEXP attribute_hidden do_direxists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;

#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid filename argument"));
    int n = length(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (int i = 0; i < n; i++) {
#ifdef Win32
	wchar_t *wfn = filenameToWchar(STRING_ELT(fn, i), TRUE);
	/* trailing \ is not valid on Windows except for the
	   root directory on a drive, specified as "\", or "D:\",
	   or "\\?\D:\", etc.  We remove it in other cases,
	   to help those who think they're on Unix. */
	size_t len = wcslen(wfn);
	if (len) {
	    wchar_t *p = wfn + (len - 1);
            if (len > 1 && (*p == L'/' || *p == L'\\') &&
            	*(p-1) != L':') *p = 0;
        }
#else
	const char *efn = R_ExpandFileName(translateChar(STRING_ELT(fn, i)));
#endif
	if (STRING_ELT(fn, i) != NA_STRING &&
#ifdef Win32
	    _wstati64(wfn, &sb)
#else
	    /* Target not link */
	    stat(efn, &sb)
#endif
	    == 0) {
	    LOGICAL(ans)[i] = (sb.st_mode & S_IFDIR) > 0;

	} else LOGICAL(ans)[i] = 0;
    }
    // copy names?
    UNPROTECT(1);
    return ans;
}

/* No longer required by POSIX, but maybe on earlier OSes */
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#if HAVE_DIRENT_H
# include <dirent.h>
#elif HAVE_SYS_NDIR_H
# include <sys/ndir.h>
#elif HAVE_SYS_DIR_H
# include <sys/dir.h>
#elif HAVE_NDIR_H
# include <ndir.h>
#endif

#define CBUFSIZE 2*PATH_MAX+1
static SEXP filename(const char *dir, const char *file)
{
    SEXP ans;
    char cbuf[CBUFSIZE];
    if (dir) {
#ifdef Win32
	if ((strlen(dir) == 2 && dir[1] == ':') ||
	    dir[strlen(dir) - 1] == '/' ||  dir[strlen(dir) - 1] == '\\')
	    snprintf(cbuf, CBUFSIZE, "%s%s", dir, file);
	else
	    snprintf(cbuf, CBUFSIZE, "%s%s%s", dir, R_FileSep, file);
#else
	snprintf(cbuf, CBUFSIZE, "%s%s%s", dir, R_FileSep, file);
#endif
	ans = mkChar(cbuf);
    } else {
	snprintf(cbuf, CBUFSIZE, "%s", file);
	ans = mkChar(cbuf);
    }
    return ans;
}

#include <tre/tre.h>

static void
list_files(const char *dnp, const char *stem, int *count, SEXP *pans,
	   Rboolean allfiles, Rboolean recursive,
	   const regex_t *reg, int *countmax, PROTECT_INDEX idx,
	   Rboolean idirs, Rboolean allowdots)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX], stem2[PATH_MAX];
#ifdef Windows
    /* > 2GB files might be skipped otherwise */
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    R_CheckUserInterrupt(); // includes stack check
    if ((dir = opendir(dnp)) != NULL) {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
		Rboolean not_dot = strcmp(de->d_name, ".") && strcmp(de->d_name, "..");
		if (recursive) {
#ifdef Win32
		    if (strlen(dnp) == 2 && dnp[1] == ':') // e.g. "C:"
			snprintf(p, PATH_MAX, "%s%s", dnp, de->d_name);
		    else
#endif
			snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);

#ifdef Windows
		    _stati64(p, &sb);
#else
		    stat(p, &sb);
#endif
		    if ((sb.st_mode & S_IFDIR) > 0) {
			if (not_dot) {
			    if (idirs) {
#define IF_MATCH_ADD_TO_ANS						\
				if (!reg || tre_regexec(reg, de->d_name, 0, NULL, 0) == 0) { \
				    if (*count == *countmax - 1) {	\
					*countmax *= 2;			\
					REPROTECT(*pans = lengthgets(*pans, *countmax), idx); \
				    }					\
				    SET_STRING_ELT(*pans, (*count)++,	\
						   filename(stem, de->d_name));	\
				}
				IF_MATCH_ADD_TO_ANS
			    }
			    if (stem) {
#ifdef Win32
				if(strlen(stem) == 2 && stem[1] == ':')
				    snprintf(stem2, PATH_MAX, "%s%s", stem,
					     de->d_name);
				else
#endif
				    snprintf(stem2, PATH_MAX, "%s%s%s", stem,
					     R_FileSep, de->d_name);
			    } else
				strcpy(stem2, de->d_name);

			    list_files(p, stem2, count, pans, allfiles,
				       recursive, reg, countmax, idx, idirs,
				       allowdots);
			}
			continue;
		    }
		} // end if(recursive)

		if (not_dot || allowdots)
		    IF_MATCH_ADD_TO_ANS
	    }

        } // end while()
	closedir(dir);
    }
}
#undef IF_MATCH_ADD_TO_ANS

SEXP attribute_hidden do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int countmax = 128;

    checkArity(op, args);
    SEXP d = CAR(args);  args = CDR(args); // d := directory = path
    if (!isString(d)) error(_("invalid '%s' argument"), "path");
    SEXP p = CAR(args); args = CDR(args);
    Rboolean pattern = FALSE;
    if (isString(p) && length(p) >= 1 && STRING_ELT(p, 0) != NA_STRING)
	pattern = TRUE;
    else if (!isNull(p) && !(isString(p) && length(p) < 1))
	error(_("invalid '%s' argument"), "pattern");
    int allfiles = asLogical(CAR(args)); args = CDR(args);
    if (allfiles == NA_LOGICAL)
	error(_("invalid '%s' argument"), "all.files");
    int fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    int recursive = asLogical(CAR(args)); args = CDR(args);
    if (recursive == NA_LOGICAL)
	error(_("invalid '%s' argument"), "recursive");
    int igcase = asLogical(CAR(args)); args = CDR(args);
    if (igcase == NA_LOGICAL)
	error(_("invalid '%s' argument"), "ignore.case");
    int idirs = asLogical(CAR(args)); args = CDR(args);
    if (idirs == NA_LOGICAL)
	error(_("invalid '%s' argument"), "include.dirs");
    int nodots = asLogical(CAR(args));
    if (nodots == NA_LOGICAL)
	error(_("invalid '%s' argument"), "no..");

    int flags = REG_EXTENDED;
    if (igcase) flags |= REG_ICASE;
    regex_t reg;
    if (pattern && tre_regcomp(&reg, translateChar(STRING_ELT(p, 0)), flags))
	error(_("invalid 'pattern' regular expression"));
    PROTECT_INDEX idx;
    SEXP ans;
    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, countmax), &idx);
    int count = 0;
    for (int i = 0; i < LENGTH(d) ; i++) {
	if (STRING_ELT(d, i) == NA_STRING) continue;
	const char *dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	list_files(dnp, fullnames ? dnp : NULL, &count, &ans, allfiles,
		   recursive, pattern ? &reg : NULL, &countmax, idx,
		   idirs, /* allowdots = */ !nodots);
    }
    REPROTECT(ans = lengthgets(ans, count), idx);
    if (pattern) tre_regfree(&reg);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

static void list_dirs(const char *dnp, const char *nm, 
		      Rboolean full, int *count,
		      SEXP *pans, int *countmax, PROTECT_INDEX idx,
		      Rboolean recursive)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX];
#ifdef Windows
    /* > 2GB files might be skipped otherwise */
    struct _stati64 sb;
#else
    struct stat sb;
#endif
    R_CheckUserInterrupt(); // includes stack check

    if ((dir = opendir(dnp)) != NULL) {
	if (recursive) {
	    if (*count == *countmax - 1) {
		*countmax *= 2;
		REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
	    }
	    SET_STRING_ELT(*pans, (*count)++, mkChar(full ? dnp : nm));
	}
	while ((de = readdir(dir))) {
#ifdef Win32
	    if (strlen(dnp) == 2 && dnp[1] == ':')
		snprintf(p, PATH_MAX, "%s%s", dnp, de->d_name);
	    else
		snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#else
	    snprintf(p, PATH_MAX, "%s%s%s", dnp, R_FileSep, de->d_name);
#endif
#ifdef Windows
	    _stati64(p, &sb);
#else
	    stat(p, &sb);
#endif
	    if ((sb.st_mode & S_IFDIR) > 0) {
		if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) {
		    if(recursive) {
			char nm2[PATH_MAX];
			snprintf(nm2, PATH_MAX, "%s%s%s", nm, R_FileSep, 
				 de->d_name);
			list_dirs(p, nm[0] ? nm2 : de->d_name, full, count,
				  pans, countmax, idx, recursive);

		    } else {
			if (*count == *countmax - 1) {
			    *countmax *= 2;
			    REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
			}
			SET_STRING_ELT(*pans, (*count)++, 
				       mkChar(full ? p : de->d_name));
		    }
		}
	    }
	}
	closedir(dir);
    }
}

SEXP attribute_hidden do_listdirs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    PROTECT_INDEX idx;
    SEXP d, ans;
    int fullnames, count, i, recursive;
    const char *dnp;
    int countmax = 128;

    checkArity(op, args);
    d = CAR(args); args = CDR(args);
    if (!isString(d)) error(_("invalid '%s' argument"), "directory");
    fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    recursive = asLogical(CAR(args)); args = CDR(args);
    if (recursive == NA_LOGICAL)
	error(_("invalid '%s' argument"), "recursive");

    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, countmax), &idx);
    count = 0;
    for (i = 0; i < LENGTH(d) ; i++) {
	if (STRING_ELT(d, i) == NA_STRING) continue;
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	list_dirs(dnp, "", fullnames, &count, &ans, &countmax, idx, recursive);
    }
    REPROTECT(ans = lengthgets(ans, count), idx);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_Rhome(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *path;
    checkArity(op, args);
    if (!(path = R_HomeDir()))
	error(_("unable to determine R home location"));
    return mkString(path);
}

#ifdef Win32
static Rboolean attribute_hidden R_WFileExists(const wchar_t *path)
{
    struct _stati64 sb;
    return _wstati64(path, &sb) == 0;
}
#endif

SEXP attribute_hidden do_fileexists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, ans;
    int i, nfile;
    checkArity(op, args);
    if (!isString(file = CAR(args)))
	error(_("invalid '%s' argument"), "file");
    nfile = length(file);
    ans = allocVector(LGLSXP, nfile);
    for (i = 0; i < nfile; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(file, i) != NA_STRING) {
#ifdef Win32
	    /* Package XML sends arbitrarily long strings to file.exists! */
	    size_t len = strlen(CHAR(STRING_ELT(file, i)));
	    if (len > MAX_PATH)
		LOGICAL(ans)[i] = FALSE;
	    else
		LOGICAL(ans)[i] =
		    R_WFileExists(filenameToWchar(STRING_ELT(file, i), TRUE));
#else
	    LOGICAL(ans)[i] = R_FileExists(translateChar(STRING_ELT(file, i)));
#endif
	} else LOGICAL(ans)[i] = FALSE;
    }
    return ans;
}

#define CHOOSEBUFSIZE 1024

#ifndef Win32
SEXP attribute_hidden do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int _new, len;
    char buf[CHOOSEBUFSIZE];
    checkArity(op, args);
    _new = asLogical(CAR(args));
    if ((len = R_ChooseFile(_new, buf, CHOOSEBUFSIZE)) == 0)
	error(_("file choice cancelled"));
    if (len >= CHOOSEBUFSIZE - 1)
	error(_("file name too long"));
    return mkString(R_ExpandFileName(buf));
}
#endif

/* needed for access, and perhaps for realpath */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef Win32
extern int winAccessW(const wchar_t *path, int mode);
#endif

/* we require 'access' as from 2.12.0 */
SEXP attribute_hidden do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n, mode, modemask;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "names");
    n = length(fn);
    mode = asInteger(CADR(args));
    if (mode < 0 || mode > 7) error(_("invalid '%s' argument"), "mode");
    modemask = 0;
    if (mode & 1) modemask |= X_OK;
    if (mode & 2) modemask |= W_OK;
    if (mode & 4) modemask |= R_OK;
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++)
	if (STRING_ELT(fn, i) != NA_STRING) {
	    INTEGER(ans)[i] =
#ifdef Win32
		winAccessW(filenameToWchar(STRING_ELT(fn, i), TRUE), modemask);
#else
		access(R_ExpandFileName(translateChar(STRING_ELT(fn, i))),
		       modemask);
#endif
	} else INTEGER(ans)[i] = FALSE;
    UNPROTECT(1);
    return ans;
}

#ifdef Win32

static int R_rmdir(const wchar_t *dir)
{
    wchar_t tmp[MAX_PATH];
    GetShortPathNameW(dir, tmp, MAX_PATH);
    //printf("removing directory %ls\n", tmp);
    return _wrmdir(tmp);
}

/* Junctions and symbolic links are fundamentally reparse points, so
   apparently this is the way to detect them. */
static int isReparsePoint(const wchar_t *name)
{
    DWORD res = GetFileAttributesW(name);
    if(res == INVALID_FILE_ATTRIBUTES) {
	warning("cannot get info on '%ls', reason '%s'",
		name, formatError(GetLastError()));
	return 0;
    }
    // printf("%ls: %x\n", name, res);
    return res & FILE_ATTRIBUTE_REPARSE_POINT;
}

static int delReparsePoint(const wchar_t *name)
{
    HANDLE hd =
	CreateFileW(name, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING,
		    FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
		    0);
    if(hd == INVALID_HANDLE_VALUE) {
	warning("cannot open reparse point '%ls', reason '%s'",
		name, formatError(GetLastError()));
	return 1;
    }
    REPARSE_GUID_DATA_BUFFER rgdb = {0};
    rgdb.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
    DWORD dwBytes;
    BOOL res = DeviceIoControl(hd, FSCTL_DELETE_REPARSE_POINT, &rgdb,
			       REPARSE_GUID_DATA_BUFFER_HEADER_SIZE,
			       NULL, 0, &dwBytes, 0);
    CloseHandle(hd);
    if(res == 0)
	warning("cannot delete reparse point '%ls', reason '%s'",
		name, formatError(GetLastError()));
    else /* This may leave an empty dir behind */
	R_rmdir(name);
    return res == 0;
}

static int R_unlink(wchar_t *name, int recursive, int force)
{
    R_CheckStack(); // called recursively
    if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) return 0;
    //printf("R_unlink(%ls)\n", name);
    if (!R_WFileExists(name)) return 0;
    if (force) _wchmod(name, _S_IWRITE);

    if (recursive) {
	_WDIR *dir;
	struct _wdirent *de;
	wchar_t p[PATH_MAX];
	struct _stati64 sb;
	int n, ans = 0;

	_wstati64(name, &sb);
	/* We need to test for a junction first, as junctions
	   are detected as directories. */
	if (isReparsePoint(name)) ans += delReparsePoint(name);
	else if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	    if ((dir = _wopendir(name)) != NULL) {
		while ((de = _wreaddir(dir))) {
		    if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
			continue;
		    /* On Windows we need to worry about trailing seps */
		    n = wcslen(name);
		    if (name[n] == L'/' || name[n] == L'\\') {
			wcscpy(p, name); wcscat(p, de->d_name);
		    } else {
			wcscpy(p, name); wcscat(p, L"/"); wcscat(p, de->d_name);
		    }
		    /* printf("stat-ing %ls\n", p); */
		    _wstati64(p, &sb);
		    if (isReparsePoint(name)) ans += delReparsePoint(name);
		    else if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
			/* printf("is a directory\n"); */
			if (force) _wchmod(p, _S_IWRITE);
			ans += R_unlink(p, recursive, force);
		    } else {
			if (force) _wchmod(p, _S_IWRITE);
			ans += (_wunlink(p) == 0) ? 0 : 1;
		    }
		}
		_wclosedir(dir);
	    } else { /* we were unable to read a dir */
		ans++;
	    }
	    ans += (R_rmdir(name) == 0) ? 0 : 1;
	    return ans;
	}
	/* drop through */
    } else if (isReparsePoint(name)) return delReparsePoint(name);

    return _wunlink(name) == 0 ? 0 : 1;
}

void R_CleanTempDir(void)
{
    if (Sys_TempDir) {
	size_t n = strlen(Sys_TempDir);
	/* Windows cannot delete the current working directory */
	SetCurrentDirectory(R_HomeDir());
	wchar_t w[2*(n+1)];
	mbstowcs(w, Sys_TempDir, n+1);
	R_unlink(w, 1, 1); /* recursive=TRUE, force=TRUE */
    }
}
#else
static int R_unlink(const char *name, int recursive, int force)
{
    R_CheckStack(); // called recursively
    struct stat sb;
    int res, res2;

    if (streql(name, ".") || streql(name, "..")) return 0;
    /* We cannot use R_FileExists here since it is false for broken
       symbolic links
       if (!R_FileExists(name)) return 0; */
    res  = lstat(name, &sb);  /* better to be lstat */
    if (!res && force) chmod(name, sb.st_mode | S_IWUSR);

    if (!res && recursive) {
	DIR *dir;
	struct dirent *de;
	char p[PATH_MAX];
	int ans = 0;

	if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	    if ((dir = opendir(name)) != NULL) {
		while ((de = readdir(dir))) {
		    if (streql(de->d_name, ".") || streql(de->d_name, ".."))
			continue;
		    size_t n = strlen(name);
		    if (name[n] == R_FileSep[0])
			snprintf(p, PATH_MAX, "%s%s", name, de->d_name);
		    else
			snprintf(p, PATH_MAX, "%s%s%s", name, R_FileSep,
				 de->d_name);
		    lstat(p, &sb);
		    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
			if (force) chmod(p, sb.st_mode | S_IWUSR | S_IXUSR);
			ans += R_unlink(p, recursive, force);
		    } else {
			if (force) chmod(p, sb.st_mode | S_IWUSR);
			ans += (unlink(p) == 0) ? 0 : 1;
		    }
		}
		closedir(dir);
	    } else { /* we were unable to read a dir */
		ans++;
	    }
	    ans += (rmdir(name) == 0) ? 0 : 1;
	    return ans;
	}
	/* drop through */
    }
    res2 = unlink(name);
    /* We want to return 0 if either unlink succeeded or 'name' did not exist */
    return (res2 == 0 || res != 0) ? 0 : 1;
}

#endif


/* Note that wildcards are allowed in 'names' */
#ifdef Win32
# include <dos_wglob.h>
SEXP attribute_hidden do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, j, nfiles, res, failures = 0, recursive, force;
    const wchar_t *names;
    wglob_t globbuf;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "x");
	recursive = asLogical(CADR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	force = asLogical(CADDR(args));
	if (force == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "force");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		names = filenameToWchar(STRING_ELT(fn, i), TRUE);
		//Rprintf("do_unlink(%ls)\n", names);
		res = dos_wglob(names, GLOB_NOCHECK, NULL, &globbuf);
		if (res == GLOB_NOSPACE)
		    error(_("internal out-of-memory condition"));
		for (j = 0; j < globbuf.gl_pathc; j++)
		    failures += R_unlink(globbuf.gl_pathv[j], recursive, force);
		dos_wglobfree(&globbuf);
	    } else failures++;
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#else
# if defined(HAVE_GLOB) && defined(HAVE_GLOB_H)
#  include <glob.h>
# endif

SEXP attribute_hidden do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, nfiles, failures = 0, recursive, force;
    const char *names;
#if defined(HAVE_GLOB)
    int j, res;
    glob_t globbuf;
#endif

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    if (nfiles > 0) {
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "x");
	recursive = asLogical(CADR(args));
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	force = asLogical(CADDR(args));
	if (force == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "force");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		names = R_ExpandFileName(translateChar(STRING_ELT(fn, i)));
#if defined(HAVE_GLOB)
		res = glob(names, GLOB_NOCHECK, NULL, &globbuf);
# ifdef GLOB_ABORTED
		if (res == GLOB_ABORTED)
		    warning(_("read error on '%s'"), names);
# endif
# ifdef GLOB_NOSPACE
		if (res == GLOB_NOSPACE)
		    error(_("internal out-of-memory condition"));
# endif
		for (j = 0; j < globbuf.gl_pathc; j++)
		    failures += R_unlink(globbuf.gl_pathv[j], recursive, force);
		globfree(&globbuf);
	    } else failures++;
#else /* HAVE_GLOB */
	        failures += R_unlink(names, recursive, force);
	    } else failures++;
#endif
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#endif

#if 0
static void chmod_one(const char *name)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX];
#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif
#ifndef Win32
    mode_t mask = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR, /* 0644 */
	dirmask = mask | S_IXUSR | S_IXGRP | S_IXOTH; /* 0755 */
#endif

    if (streql(name, ".") || streql(name, "..")) return;
    if (!R_FileExists(name)) return;
#ifdef Win32
    _stati64(name, &sb);
    chmod(name, _S_IWRITE);
#else
    stat(name, &sb);
    chmod(name, (sb.st_mode | mask) & dirmask);
#endif
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
#ifndef Win32
	chmod(name, dirmask);
#endif
	if ((dir = opendir(name)) != NULL) {
	    while ((de = readdir(dir))) {
		if (streql(de->d_name, ".") || streql(de->d_name, ".."))
		    continue;
		size_t n = strlen(name);
		if (name[n-1] == R_FileSep[0])
		    snprintf(p, PATH_MAX, "%s%s", name, de->d_name);
		else
		    snprintf(p, PATH_MAX, "%s%s%s", name, R_FileSep, de->d_name);
		chmod_one(p);
	    }
	    closedir(dir);
	} else {
	    /* we were unable to read a dir */
	}
    }
}

/* recursively fix up permissions: used for R CMD INSTALL and build.
   NB: this overrides umask. */
SEXP attribute_hidden do_dirchmod(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP dr;
    checkArity(op, args);
    dr = CAR(args);
    if(!isString(dr) || length(dr) != 1)
	error(_("invalid '%s' argument"), "dir");
    chmod_one(translateChar(STRING_ELT(dr, 0)));

    return R_NilValue;
}
#endif


SEXP attribute_hidden do_getlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int cat;
    char *p = NULL;

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if (cat == NA_INTEGER || cat < 0)
	error(_("invalid '%s' argument"), "category");
    switch(cat) {
    case 1: cat = LC_ALL; break;
    case 2: cat = LC_COLLATE; break;
    case 3: cat = LC_CTYPE; break;
    case 4: cat = LC_MONETARY; break;
    case 5: cat = LC_NUMERIC; break;
    case 6: cat = LC_TIME; break;
#ifdef LC_MESSAGES
    case 7: cat = LC_MESSAGES; break;
#endif
#ifdef LC_PAPER
    case 8: cat = LC_PAPER; break;
#endif
#ifdef LC_MEASUREMENT
    case 9: cat = LC_MEASUREMENT; break;
#endif
    default: cat = NA_INTEGER;
    }
    if (cat != NA_INTEGER) p = setlocale(cat, NULL);
    return mkString(p ? p : "");
}

/* Locale specs are always ASCII */
SEXP attribute_hidden do_setlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP locale = CADR(args), ans;
    int cat;
    const char *p;

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if (cat == NA_INTEGER || cat < 0)
	error(_("invalid '%s' argument"), "category");
    if (!isString(locale) || LENGTH(locale) != 1)
	error(_("invalid '%s' argument"), "locale");
    switch(cat) {
    case 1:
    {
	const char *l = CHAR(STRING_ELT(locale, 0));
	cat = LC_ALL;
	/* assume we can set LC_CTYPE iff we can set the rest */
	if ((p = setlocale(LC_CTYPE, l))) {
	    setlocale(LC_COLLATE, l);
	    resetICUcollator();
	    setlocale(LC_MONETARY, l);
	    setlocale(LC_TIME, l);
	    /* Need to return value of LC_ALL */
	    p = setlocale(cat, NULL);
	}
	break;
    }
    case 2:
	cat = LC_COLLATE;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	resetICUcollator();
	break;
    case 3:
	cat = LC_CTYPE;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 4:
	cat = LC_MONETARY;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 5:
	cat = LC_NUMERIC;
	{
	    const char *new_lc_num = CHAR(STRING_ELT(locale, 0));
	    if (strcmp(new_lc_num, "C")) /* do not complain about C locale - that's the only
					    reliable way to restore sanity */
		warning(_("setting 'LC_NUMERIC' may cause R to function strangely"));
	    p = setlocale(cat, new_lc_num);
	}
	break;
    case 6:
	cat = LC_TIME;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	dt_invalidate_locale();
	break;
#if defined LC_MESSAGES
    case 7:
	cat = LC_MESSAGES;
#ifdef Win32
/* this seems to exist in MinGW, but it does not work in Windows */
	warning(_("LC_MESSAGES exists on Windows but is not operational"));
	p = NULL;
#else
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
#endif
	break;
#endif
#ifdef LC_PAPER
    case 8:
	cat = LC_PAPER;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#endif
#ifdef LC_MEASUREMENT
    case 9:
	cat = LC_MEASUREMENT;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#endif
    default:
	p = NULL; /* -Wall */
	error(_("invalid '%s' argument"), "category");
    }
    PROTECT(ans = allocVector(STRSXP, 1));
    if (p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  {
	SET_STRING_ELT(ans, 0, mkChar(""));
	warning(_("OS reports request to set locale to \"%s\" cannot be honored"),
		CHAR(STRING_ELT(locale, 0)));
    }
    UNPROTECT(1);
    R_check_locale();
    invalidate_cached_recodings();
    return ans;
}



SEXP attribute_hidden do_localeconv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    struct lconv *lc = localeconv();
    int i = 0;
    char buff[20];

    PROTECT(ans = allocVector(STRSXP, 18));
    PROTECT(ansnames = allocVector(STRSXP, 18));
    SET_STRING_ELT(ans, i, mkChar(lc->decimal_point));
    SET_STRING_ELT(ansnames, i++, mkChar("decimal_point"));
    SET_STRING_ELT(ans, i, mkChar(lc->thousands_sep));
    SET_STRING_ELT(ansnames, i++, mkChar("thousands_sep"));
    SET_STRING_ELT(ans, i, mkChar(lc->grouping));
    SET_STRING_ELT(ansnames, i++, mkChar("grouping"));
    SET_STRING_ELT(ans, i, mkChar(lc->int_curr_symbol));
    SET_STRING_ELT(ansnames, i++, mkChar("int_curr_symbol"));
    SET_STRING_ELT(ans, i, mkChar(lc->currency_symbol));
    SET_STRING_ELT(ansnames, i++, mkChar("currency_symbol"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_decimal_point));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_decimal_point"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_thousands_sep));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_thousands_sep"));
    SET_STRING_ELT(ans, i, mkChar(lc->mon_grouping));
    SET_STRING_ELT(ansnames, i++, mkChar("mon_grouping"));
    SET_STRING_ELT(ans, i, mkChar(lc->positive_sign));
    SET_STRING_ELT(ansnames, i++, mkChar("positive_sign"));
    SET_STRING_ELT(ans, i, mkChar(lc->negative_sign));
    SET_STRING_ELT(ansnames, i++, mkChar("negative_sign"));
    sprintf(buff, "%d", (int)lc->int_frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("int_frac_digits"));
    sprintf(buff, "%d", (int)lc->frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("frac_digits"));
    sprintf(buff, "%d", (int)lc->p_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_cs_precedes"));
    sprintf(buff, "%d", (int)lc->p_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sep_by_space"));
    sprintf(buff, "%d", (int)lc->n_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_cs_precedes"));
    sprintf(buff, "%d", (int)lc->n_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sep_by_space"));
    sprintf(buff, "%d", (int)lc->p_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sign_posn"));
    sprintf(buff, "%d", (int)lc->n_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sign_posn"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

/* .Internal function for path.expand */
SEXP attribute_hidden do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "path");
    n = length(fn);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
        SEXP tmp = STRING_ELT(fn, i);
        if (tmp != NA_STRING) {
            tmp = markKnown(R_ExpandFileName(translateChar(tmp)), tmp);
        }
	SET_STRING_ELT(ans, i, tmp);
    }
    UNPROTECT(1);
    return ans;
}

#ifdef Unix
static int var_R_can_use_X11 = -1;

extern Rboolean R_access_X11(void); /* from src/unix/X11.c */

static Rboolean R_can_use_X11(void)
{
    if (var_R_can_use_X11 < 0) {
#ifdef HAVE_X11
	if (strcmp(R_GUIType, "none") != 0) {
	    /* At this point we have permission to use the module, so try it */
	    var_R_can_use_X11 = R_access_X11();
	} else {
	    var_R_can_use_X11 = 0;
	}
#else
	var_R_can_use_X11 = 0;
#endif
    }

    return var_R_can_use_X11 > 0;
}
#endif

/* only actually used on Unix */
SEXP attribute_hidden do_capabilitiesX11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef Unix
    return ScalarLogical(R_can_use_X11());
#else
    return ScalarLogical(FALSE);
#endif
}

SEXP attribute_hidden do_capabilities(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    int i = 0;
#ifdef Unix
# ifdef HAVE_X11
    int X11 = NA_LOGICAL;
# else
    int X11 = FALSE;
# endif
#endif

    checkArity(op, args);

    PROTECT(ans = allocVector(LGLSXP, 17));
    PROTECT(ansnames = allocVector(STRSXP, 17));

    SET_STRING_ELT(ansnames, i, mkChar("jpeg"));
#ifdef HAVE_JPEG
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("png"));
#ifdef HAVE_PNG
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("tiff"));
#ifdef HAVE_TIFF
# if defined Unix && !defined HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = X11;
# else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("tcltk"));
#ifdef HAVE_TCLTK
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("X11"));
#ifdef HAVE_X11
# if defined(Unix)
    LOGICAL(ans)[i++] = X11;
# else
    LOGICAL(ans)[i++] = TRUE;
# endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("aqua"));
#ifdef HAVE_AQUA
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("http/ftp"));
#if HAVE_INTERNET
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("sockets"));
#ifdef HAVE_SOCKETS
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("libxml"));
#ifdef SUPPORT_LIBXML
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("fifo"));
#if (defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)) || defined(_WIN32)
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    /* This one is complex.  Set it to be true only in interactive use,
       with the Windows and GNOME GUIs (but not Tk GUI) or under Unix
       if readline is available and in use. */
    SET_STRING_ELT(ansnames, i, mkChar("cledit"));
    LOGICAL(ans)[i] = FALSE;
#if defined(Win32)
    if (R_Interactive) LOGICAL(ans)[i] = TRUE;
#endif
#ifdef Unix
    if (strcmp(R_GUIType, "GNOME") == 0) {  /* always interactive */
	LOGICAL(ans)[i] = TRUE;  /* also AQUA ? */
    } else {
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
	extern Rboolean UsingReadline;
	if (R_Interactive && UsingReadline) LOGICAL(ans)[i] = TRUE;
#endif
    }
#endif
    i++;

/* always true as from R 2.10.0 */
    SET_STRING_ELT(ansnames, i, mkChar("iconv"));
    LOGICAL(ans)[i++] = TRUE;

    SET_STRING_ELT(ansnames, i, mkChar("NLS"));
#ifdef ENABLE_NLS
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("profmem"));
#ifdef R_MEMORY_PROFILING
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("cairo"));
#ifdef HAVE_WORKING_CAIRO
    LOGICAL(ans)[i++] = TRUE;
#elif defined(Win32)
{
    /* This is true iff winCairo.dll is available */
    struct stat sb;
    char path[1000];
    snprintf(path, 1000, "%s/library/grDevices/libs/%s/winCairo.dll",
	     R_HomeDir(), R_ARCH);
    LOGICAL(ans)[i++] = stat(path, &sb) == 0;
}
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("ICU"));
#ifdef USE_ICU
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("long.double"));
    LOGICAL(ans)[i++] = sizeof(LDOUBLE) > sizeof(double);

    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

#if defined(HAVE_BSD_NETWORKING) && defined(HAVE_ARPA_INET_H)
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

SEXP attribute_hidden do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    const char *name; char ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    checkArity(op, args);
    if (!isString(CAR(args)) || length(CAR(args)) != 1)
	error(_("'hostname' must be a character vector of length 1"));
    name = translateChar(STRING_ELT(CAR(args), 0));

    hp = gethostbyname(name);

    if (hp == NULL) {		/* cannot resolve the address */
	warning(_("nsl() was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warning(_("unknown format returned by C function 'gethostbyname'"));
	}
	ans = mkString(ip);
    }
    return ans;
}
#else
SEXP attribute_hidden do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning(_("nsl() is not supported on this platform"));
    return R_NilValue;
}
#endif

SEXP attribute_hidden do_sysgetpid(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return ScalarInteger(getpid());
}


/* NB: we save errno immediately after the call here.  This should not
  be necessary on a POSIX OS, but it is on Windows, where it seems
  that on some versions strerror itself changes errno (something
  allowed in C99 but disallowed in POSIX).  Also, something under
  warning() might set errno in a future version.
*/
#ifndef Win32
/* mkdir is defined in <sys/stat.h> */
SEXP attribute_hidden do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP path;
    int res, show, recursive, mode, serrno = 0;
    char *p, dir[PATH_MAX];

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    mode = asInteger(CADDDR(args));
    if (mode == NA_LOGICAL) mode = 0777;
    strcpy(dir, R_ExpandFileName(translateChar(STRING_ELT(path, 0))));
    /* remove trailing slashes */
    p = dir + strlen(dir) - 1;
    while (*p == '/' && strlen(dir) > 1) *p-- = '\0';
    if (recursive) {
	p = dir;
	while ((p = Rf_strchr(p+1, '/'))) {
	    *p = '\0';
	    struct stat sb;
	    res = stat(dir, &sb);
	    if (res == 0) {
		if (! S_ISDIR (sb.st_mode)) {
		    /* file already exists but is not a directory */
		    res = -1;
		    serrno = ENOTDIR;
		    goto end;
		}
	    } else if (errno != ENOENT || !*dir) {
		serrno = errno;
		goto end;
	    } else
		res = mkdir(dir, (mode_t) mode);

	    /* Solaris 10 returns ENOSYS on automount, PR#13834
	       EROFS is allowed by POSIX, so we skip that too */
	    serrno = errno;
	    if (res && serrno != EEXIST && serrno != ENOSYS && serrno != EROFS)
		goto end;
	    *p = '/';
	}
    }
    res = mkdir(dir, (mode_t) mode);
    serrno = errno;
    if (show && res && serrno == EEXIST)
	warning(_("'%s' already exists"), dir);
end:
    if (show && res && serrno != EEXIST)
	warning(_("cannot create dir '%s', reason '%s'"), dir,
		strerror(serrno));
    return ScalarLogical(res == 0);
}
#else /* Win32 */
#include <io.h> /* mkdir is defined here */
SEXP attribute_hidden do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path;
    wchar_t *p, dir[MAX_PATH];
    int res, show, recursive, serrno = 0;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    wcscpy(dir, filenameToWchar(STRING_ELT(path, 0), TRUE));
    for (p = dir; *p; p++) if (*p == L'/') *p = L'\\';
    /* remove trailing slashes */
    p = dir + wcslen(dir) - 1;
    while (*p == L'\\' && wcslen(dir) > 1 && *(p-1) != L':') *p-- = L'\0';
    if (recursive) {
	p = dir;
	/* skip leading \\share */
	if (*p == L'\\' && *(p+1) == L'\\') {
	    p += 2;
	    p = wcschr(p, L'\\');
	}
	while ((p = wcschr(p+1, L'\\'))) {
	    *p = L'\0';
	    if (*(p-1) != L':') {
		res = _wmkdir(dir);
		serrno = errno;
		if (res && serrno != EEXIST) goto end;
	    }
	    *p = L'\\';
	}
    }
    res = _wmkdir(dir);
    serrno = errno;
    if (show && res && serrno == EEXIST)
	warning(_("'%ls' already exists"), dir);
    return ScalarLogical(res == 0);
end:
    if (show && res && serrno != EEXIST)
	warning(_("cannot create dir '%ls', reason '%s'"), dir,
		strerror(serrno));
    return ScalarLogical(res == 0);
}
#endif

/* take file/dir 'name' in dir 'from' and copy it to 'to'
   'from', 'to' should have trailing path separator if needed.
*/
#ifdef Win32
static void copyFileTime(const wchar_t *from, const wchar_t * to)
{
    HANDLE hFrom, hTo;
    FILETIME modft;
    
    hFrom = CreateFileW(from, GENERIC_READ, 0, NULL, OPEN_EXISTING,
			FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFrom == INVALID_HANDLE_VALUE) return;
    int res  = GetFileTime(hFrom, NULL, &modft, NULL);
    CloseHandle(hFrom);
    if(!res) return;
   
    hTo = CreateFileW(to, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		      FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hTo == INVALID_HANDLE_VALUE) return;
    SetFileTime(hTo, NULL, NULL, &modft);
    CloseHandle(hTo);
}

static int do_copy(const wchar_t* from, const wchar_t* name, const wchar_t* to,
		   int over, int recursive, int perms, int dates, int depth)
{
    R_CheckUserInterrupt(); // includes stack check
    if(depth > 100) {
	warning(_("too deep nesting"));
	return 1;
    }
    struct _stati64 sb;
    int nc, nfail = 0, res;
    wchar_t dest[PATH_MAX + 1], this[PATH_MAX + 1];

    if (wcslen(from) + wcslen(name) >= PATH_MAX) {
	warning(_("over-long path"));
	return 1;
    }
    wsprintfW(this, L"%ls%ls", from, name);
    _wstati64(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	_WDIR *dir;
	struct _wdirent *de;
	wchar_t p[PATH_MAX + 1];

	if (!recursive) return 1;
	nc = wcslen(to);
	if (wcslen(to) + wcslen(name) >= PATH_MAX) {
	    warning(_("over-long path"));
	    return 1;
	}
	wsprintfW(dest, L"%ls%ls", to, name);
	/* We could set the mode (only the 200 part matters) later */
	res = _wmkdir(dest);
	if (res && errno != EEXIST) {
	    warning(_("problem creating directory %ls: %s"),
		    dest, strerror(errno));
	    return 1;
	}
	// NB Windows' mkdir appears to require \ not /.
	if ((dir = _wopendir(this)) != NULL) {
	    depth++;
	    while ((de = _wreaddir(dir))) {
		if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
		    continue;
		if (wcslen(name) + wcslen(de->d_name) + 1 >= PATH_MAX) {
		    warning(_("over-long path"));
		    return 1;
		}
		wsprintfW(p, L"%ls%\\%ls", name, de->d_name);
		nfail += do_copy(from, p, to, over, recursive, 
				 perms, dates, depth);
	    }
	    _wclosedir(dir);
	} else {
	    warning(_("problem reading dir %ls: %s"), this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
	if(dates) copyFileTime(this, dest);
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;
	wchar_t buf[APPENDBUFSIZE];

	nfail = 0;
	nc = wcslen(to);
	if (nc + wcslen(name) >= PATH_MAX) {
	    warning(_("over-long path length"));
	    nfail++;
	    goto copy_error;
	}
	wsprintfW(dest, L"%ls%ls", to, name);
	if (over || !R_WFileExists(dest)) { /* FIXME */
	    if ((fp1 = _wfopen(this, L"rb")) == NULL ||
		(fp2 = _wfopen(dest, L"wb")) == NULL) {
		warning(_("problem copying %ls to %ls: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp2) != APPENDBUFSIZE) {
		    nfail++;
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		goto copy_error;
	    }
	}
	if(fp1) fclose(fp1); fp1 = NULL;
	if(fp2) fclose(fp2); fp2 = NULL;
	/* FIXME: perhaps manipulate mode as we do in Sys.chmod? */
	if(perms) _wchmod(dest, sb.st_mode & 0777);
	if(dates) copyFileTime(this, dest);
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    return nfail;
}

/* file.copy(files, dir, over, recursive=TRUE, perms), only */
SEXP attribute_hidden do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, to, ans;
    wchar_t *p, dir[PATH_MAX], from[PATH_MAX], name[PATH_MAX];
    int i, nfiles, over, recursive, perms, dates, nfail;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    PROTECT(ans = allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	args = CDR(args);
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	to = CAR(args); args = CDR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	over = asLogical(CAR(args)); args = CDR(args);
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "over");
	recursive = asLogical(CAR(args)); args = CDR(args);
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	perms = asLogical(CAR(args)); args = CDR(args);
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	dates = asLogical(CAR(args));
	if (dates == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.dates");
	wcsncpy(dir,
		filenameToWchar(STRING_ELT(to, 0), TRUE),
		PATH_MAX);
        dir[PATH_MAX - 1] = L'\0';		
	if (*(dir + (wcslen(dir) - 1)) !=  L'\\')
	    wcsncat(dir, L"\\", PATH_MAX);
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		wcsncpy(from,
			filenameToWchar(STRING_ELT(fn, i), TRUE),
			PATH_MAX);
		from[PATH_MAX - 1] = L'\0';
		if(wcslen(from)) {
		    /* If there was a trailing sep, this is a mistake */
		    p = from + (wcslen(from) - 1);
		    if(*p == L'\\') *p = L'\0';
		    p = wcsrchr(from, L'\\') ;
		    if (p) {
			wcsncpy(name, p+1, PATH_MAX);
			name[PATH_MAX - 1] = L'\0';
			*(p+1) = L'\0';
		    } else {
			if(wcslen(from) > 2 && from[1] == L':') {
			    wcsncpy(name, from+2, PATH_MAX);
			    name[PATH_MAX - 1] = L'\0';
			    from[2] = L'\0';
			} else {
			    wcsncpy(name, from, PATH_MAX);
			    name[PATH_MAX - 1] = L'\0';
			    wcsncpy(from, L".\\", PATH_MAX);
			}
		    }
		    nfail = do_copy(from, name, dir, over, recursive, 
				    perms, dates, 1);
		} else nfail = 1;
	    } else nfail = 1;
	    LOGICAL(ans)[i] = (nfail == 0);
	}
    }
    UNPROTECT(1);
    return ans;
}

#else

# ifdef HAVE_UTIMES
#  include <sys/time.h>
# elif defined(HAVE_UTIME)
#  include <utime.h>
# endif

static void copyFileTime(const char *from, const char * to)
{
    struct stat sb;
    if(stat(from, &sb)) return;
    double ftime;

#ifdef STAT_TIMESPEC
    ftime = (double) STAT_TIMESPEC(sb, st_mtim).tv_sec
	+ 1e-9 * (double) STAT_TIMESPEC(sb, st_mtim).tv_nsec;
#elif defined STAT_TIMESPEC_NS
    ftime = STAT_TIMESPEC_NS (sb, st_mtim);
#else
    ftime = (double) sb.st_mtime;
#endif

#if defined(HAVE_UTIMES)
    struct timeval times[2];

    times[0].tv_sec = times[1].tv_sec = (int)ftime;
    times[0].tv_usec = times[1].tv_usec = (int)(1e6*(ftime - (int)ftime));
    utimes(to, times);
#elif defined(HAVE_UTIME)
    struct utimbuf settime;

    settime.actime = settime.modtime = (int)ftime;
    utime(to, &settime);
#endif
}

static int do_copy(const char* from, const char* name, const char* to,
		   int over, int recursive, int perms, int dates, int depth)
{
    R_CheckUserInterrupt(); // includes stack check
    if(depth > 100) {
	warning(_("too deep nesting"));
	return 1;
    }

    struct stat sb;
    int nfail = 0, res, mask;
    char dest[PATH_MAX+1], this[PATH_MAX+1];

#ifdef HAVE_UMASK
    int um = umask(0); umask((mode_t) um);
    mask = 0777 & ~um;
#else
    mask = 0777;
#endif
    /* REprintf("from: %s, name: %s, to: %s\n", from, name, to); */
    if (strlen(from) + strlen(name) >= PATH_MAX) {
	warning(_("over-long path length"));
	return 1;
    }
    snprintf(this, PATH_MAX+1, "%s%s", from, name);
    /* Here we want the target not the link */
    stat(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	DIR *dir;
	struct dirent *de;
	char p[PATH_MAX+1];

	if (!recursive) return 1;
	if (strlen(to) + strlen(name) >= PATH_MAX) {
	    warning(_("over-long path length"));
	    return 1;
	}
	snprintf(dest, PATH_MAX+1, "%s%s", to, name);
	/* If a directory does not have write permission for the user,
	   we will fail to create files in that directory, so defer
	   setting mode */
	res = mkdir(dest, 0700);
	if (res && errno != EEXIST) {
	    warning(_("problem creating directory %s: %s"),
		    this, strerror(errno));
	    return 1;
	}
	strcat(dest, "/");
	if ((dir = opendir(this)) != NULL) {
	    depth++;
	    while ((de = readdir(dir))) {
		if (streql(de->d_name, ".") || streql(de->d_name, ".."))
		    continue;
		if (strlen(name) + strlen(de->d_name) + 1 >= PATH_MAX) {
		    warning(_("over-long path length"));
		    return 1;
		}
		snprintf(p, PATH_MAX+1, "%s/%s", name, de->d_name);
		nfail += do_copy(from, p, to, over, recursive, 
				 perms, dates, depth);
	    }
	    closedir(dir);
	} else {
	    warning(_("problem reading directory %s: %s"),
		    this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
	chmod(dest, (mode_t) (perms ? (sb.st_mode & mask): mask));
	if(dates) copyFileTime(this, dest);
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;
	char buf[APPENDBUFSIZE];

	nfail = 0;
	size_t nc = strlen(to);
	if (strlen(to) + strlen(name) >= PATH_MAX) {
	    warning(_("over-long path length"));
	    nfail++;
	    goto copy_error;
	}
	snprintf(dest, PATH_MAX+1, "%s%s", to, name);
	if (over || !R_FileExists(dest)) {
	    /* REprintf("copying %s to %s\n", this, dest); */
	    if ((fp1 = R_fopen(this, "rb")) == NULL ||
		(fp2 = R_fopen(dest, "wb")) == NULL) {
		warning(_("problem copying %s to %s: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp2) != APPENDBUFSIZE) {
		    nfail++;
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		goto copy_error;
	    }
	    if(fp2) fclose(fp2); fp2 = NULL;
	    if(perms) chmod(dest, sb.st_mode & mask);
	    if(dates) copyFileTime(this, dest);
	}
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    return nfail;
}

/* file.copy(files, dir, recursive), only */
SEXP attribute_hidden do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, to, ans;
    char *p, dir[PATH_MAX], from[PATH_MAX], name[PATH_MAX];
    int i, nfiles, over, recursive, perms, dates, nfail;

    checkArity(op, args);
    fn = CAR(args);
    nfiles = length(fn);
    PROTECT(ans = allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	args = CDR(args);
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	to = CAR(args); args = CDR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	over = asLogical(CAR(args)); args = CDR(args);
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "over");
	recursive = asLogical(CAR(args)); args = CDR(args);
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	perms = asLogical(CAR(args)); args = CDR(args);
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	dates = asLogical(CAR(args));
	if (dates == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.dates");
	strncpy(dir,
		R_ExpandFileName(translateChar(STRING_ELT(to, 0))),
		PATH_MAX);
        dir[PATH_MAX - 1] = '\0';
	if (*(dir + (strlen(dir) - 1)) !=  '/')
	    strncat(dir, "/", PATH_MAX);
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		strncpy(from,
			R_ExpandFileName(translateChar(STRING_ELT(fn, i))),
			PATH_MAX);
                from[PATH_MAX - 1] = '\0';
		size_t ll = strlen(from);
		if (ll) {  // people do pass ""
		    /* If there is a trailing sep, this is a mistake */
		    p = from + (ll - 1);
		    if(*p == '/') *p = '\0';
		    p = strrchr(from, '/') ;
		    if (p) {
			strncpy(name, p+1, PATH_MAX);
                        name[PATH_MAX - 1] = '\0';
			*(p+1) = '\0';
		    } else {
			strncpy(name, from, PATH_MAX);
                        name[PATH_MAX - 1] = '\0';
			strncpy(from, "./", PATH_MAX);
		    }
		    nfail = do_copy(from, name, dir, over, recursive, 
				    perms, dates, 1);
		} else nfail = 1;
	    } else nfail = 1;
	    LOGICAL(ans)[i] = (nfail == 0);
	}
    }
    UNPROTECT(1);
    return ans;
}
#endif

SEXP attribute_hidden do_l10n_info(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef Win32
    int len = 4;
#else
    int len = 3;
#endif
    SEXP ans, names;
    checkArity(op, args);
    PROTECT(ans = allocVector(VECSXP, len));
    PROTECT(names = allocVector(STRSXP, len));
    SET_STRING_ELT(names, 0, mkChar("MBCS"));
    SET_STRING_ELT(names, 1, mkChar("UTF-8"));
    SET_STRING_ELT(names, 2, mkChar("Latin-1"));
    SET_VECTOR_ELT(ans, 0, ScalarLogical(mbcslocale));
    SET_VECTOR_ELT(ans, 1, ScalarLogical(utf8locale));
    SET_VECTOR_ELT(ans, 2, ScalarLogical(latin1locale));
#ifdef Win32
    SET_STRING_ELT(names, 3, mkChar("codepage"));
    SET_VECTOR_ELT(ans, 3, ScalarInteger(localeCP));
#endif
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return ans;
}

/* do_normalizepath moved to util.c in R 2.13.0 */

SEXP attribute_hidden do_syschmod(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_CHMOD
    SEXP paths, smode, ans;
    int i, m, n, *modes, res;
    mode_t um = 0;

    checkArity(op, args);
    paths = CAR(args);
    if (!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    PROTECT(smode = coerceVector(CADR(args), INTSXP));
    modes = INTEGER(smode);
    m = LENGTH(smode);
    if(!m && n) error(_("'mode' must be of length at least one"));
    int useUmask = asLogical(CADDR(args));
    if (useUmask == NA_LOGICAL)
	error(_("invalid '%s' argument"), "use_umask");
#ifdef HAVE_UMASK
    um = umask(0); umask(um);
#endif
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	mode_t mode = (mode_t) modes[i % m];
	if (mode == NA_INTEGER) mode = 0777;
#ifdef HAVE_UMASK
	if(useUmask) mode = mode & ~um;
#endif
#ifdef Win32
	/* Windows' _[w]chmod seems only to support read access
	   or read-write access.  _S_IWRITE is 0200.
	*/
	mode = (mode & 0200) ? (_S_IWRITE | _S_IREAD): _S_IREAD;
#endif
	if (STRING_ELT(paths, i) != NA_STRING) {
#ifdef Win32
	    res = _wchmod(filenameToWchar(STRING_ELT(paths, i), TRUE), mode);
#else
	    res = chmod(R_ExpandFileName(translateChar(STRING_ELT(paths, i))),
			mode);
#endif
	} else res = 1;
	LOGICAL(ans)[i] = (res == 0);
    }
    UNPROTECT(2);
    return ans;
#else
    SEXP paths, ans;
    int i, n;

    checkArity(op, args);
    paths = CAR(args);
    if (!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    warning("insufficient OS support on this platform");
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) LOGICAL(ans)[i] = 0;
    UNPROTECT(1);
    return ans;
#endif
}

SEXP attribute_hidden do_sysumask(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    int mode;
    mode_t res = 0;

    checkArity(op, args);
    mode = asInteger(CAR(args));
#ifdef HAVE_UMASK
    if (mode == NA_INTEGER) {
	res = umask(0);
	umask(res);
	R_Visible = TRUE;
    } else {
	res = umask((mode_t) mode);
	R_Visible = FALSE;
    }
#else
    warning(_("insufficient OS support on this platform"));
    R_Visible = FALSE;
#endif
    PROTECT(ans = ScalarInteger(res));
    setAttrib(ans, R_ClassSymbol, mkString("octmode"));
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_readlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP paths, ans;
    int n;
#ifdef HAVE_READLINK
    char buf[PATH_MAX+1];
    ssize_t res;
    int i;
#endif

    checkArity(op, args);
    paths = CAR(args);
    if(!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    n = LENGTH(paths);
    PROTECT(ans = allocVector(STRSXP, n));
#ifdef HAVE_READLINK
    for (i = 0; i < n; i++) {
	memset(buf, 0, PATH_MAX+1);
	res = readlink(R_ExpandFileName(translateChar(STRING_ELT(paths, i))),
		       buf, PATH_MAX);
	if (res >= 0) SET_STRING_ELT(ans, i, mkChar(buf));
	else if (errno == EINVAL) SET_STRING_ELT(ans, i, mkChar(""));
	else SET_STRING_ELT(ans, i,  NA_STRING);
    }
#endif
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_Cstack_info(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, nms;

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 4));
    PROTECT(nms = allocVector(STRSXP, 4));
    /* FIXME: could be out of range */
    INTEGER(ans)[0] = (R_CStackLimit == -1) ? NA_INTEGER : (int) R_CStackLimit;
    INTEGER(ans)[1] = (R_CStackLimit == -1) ? NA_INTEGER : (int)
	(R_CStackDir * (R_CStackStart - (uintptr_t) &ans));
    INTEGER(ans)[2] = R_CStackDir;
    INTEGER(ans)[3] = R_EvalDepth;
    SET_STRING_ELT(nms, 0, mkChar("size"));
    SET_STRING_ELT(nms, 1, mkChar("current"));
    SET_STRING_ELT(nms, 2, mkChar("direction"));
    SET_STRING_ELT(nms, 3, mkChar("eval_depth"));

    UNPROTECT(2);
    setAttrib(ans, R_NamesSymbol, nms);
    return ans;
}

#ifdef Win32
static int winSetFileTime(const char *fn, time_t ftime)
{
    SYSTEMTIME st;
    FILETIME modft;
    struct tm *utctm;
    HANDLE hFile;

    utctm = gmtime(&ftime);
    if (!utctm) return 0;

    st.wYear         = (WORD) utctm->tm_year + 1900;
    st.wMonth        = (WORD) utctm->tm_mon + 1;
    st.wDayOfWeek    = (WORD) utctm->tm_wday;
    st.wDay          = (WORD) utctm->tm_mday;
    st.wHour         = (WORD) utctm->tm_hour;
    st.wMinute       = (WORD) utctm->tm_min;
    st.wSecond       = (WORD) utctm->tm_sec;
    st.wMilliseconds = (WORD) 0;
    if (!SystemTimeToFileTime(&st, &modft)) return 0;

    hFile = CreateFile(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		       FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return 0;
    int res  = SetFileTime(hFile, NULL, NULL, &modft);
    CloseHandle(hFile);
    return res != 0; /* success is non-zero */
}
#endif

SEXP attribute_hidden
do_setFileTime(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    const char *fn = translateChar(STRING_ELT(CAR(args), 0));
    int ftime = asInteger(CADR(args)), res;

#ifdef Win32
    res  = winSetFileTime(fn, (time_t)ftime);
#elif defined(HAVE_UTIMES)
    struct timeval times[2];

    times[0].tv_sec = times[1].tv_sec = ftime;
    times[0].tv_usec = times[1].tv_usec = 0;
    res = utimes(fn, times) == 0;
#elif defined(HAVE_UTIME)
    struct utimbuf settime;

    settime.actime = settime.modtime = ftime;
    res = utime(fn, &settime) == 0;
#endif
    return ScalarLogical(res);
}

#ifdef Win32
/* based on ideas in
   http://www.codeproject.com/KB/winsdk/junctionpoints.aspx
*/
typedef struct TMN_REPARSE_DATA_BUFFER
{
    DWORD  ReparseTag;
    WORD   ReparseDataLength;
    WORD   Reserved;
    WORD   SubstituteNameOffset;
    WORD   SubstituteNameLength;
    WORD   PrintNameOffset;
    WORD   PrintNameLength;
    WCHAR  PathBuffer[1024];
} TMN_REPARSE_DATA_BUFFER;

SEXP attribute_hidden do_mkjunction(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    wchar_t from[10000];
    const wchar_t *to;

    checkArity(op, args);
    /* from and to are both directories: and to exists */
    wcscpy(from, filenameToWchar(STRING_ELT(CAR(args), 0), FALSE));
    to = filenameToWchar(STRING_ELT(CADR(args), 0), TRUE);
    // printf("ln %ls %ls\n", from, to);

    HANDLE hd =
	CreateFileW(to, GENERIC_READ | GENERIC_WRITE, 0, 0, OPEN_EXISTING,
		    FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
		    0);
    if(hd == INVALID_HANDLE_VALUE) {
	warning("cannot open reparse point '%ls', reason '%s'",
		to, formatError(GetLastError()));
	return ScalarLogical(1);
    }
    TMN_REPARSE_DATA_BUFFER rdb;
    const size_t nbytes = wcslen(from) * 2;
    rdb.ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
    rdb.ReparseDataLength = nbytes + 12;
    wcscpy(rdb.PathBuffer, from);
    rdb.Reserved = 0;
    rdb.SubstituteNameOffset = 0;
    rdb.SubstituteNameLength = nbytes;
    rdb.PrintNameOffset = nbytes + 2;
    rdb.PrintNameLength = 0;
    DWORD dwBytes;
    const BOOL bOK =
	DeviceIoControl(hd, FSCTL_SET_REPARSE_POINT, &rdb,
			8 /* header */ + rdb.ReparseDataLength,
			NULL, 0, &dwBytes, 0);
    CloseHandle(hd);
    if(!bOK)
	warning("cannot set reparse point '%ls', reason '%s'",
		to, formatError(GetLastError()));
    return ScalarLogical(bOK != 0);
}
#endif

/* Formerly src/appl/machar.c:
 * void machar()  -- computes ALL `machine constants' at once.
 * -------------  -- compare with ../nmath/i1mach.c & ../nmath/d1mach.c
 *		     which use the C  <float.h> constants !
 *      algorithm 665, collected algorithms from acm.
 *      this work published in transactions on mathematical software,
 *      vol. 14, no. 4, pp. 303-311.
 *
 *  this fortran 77 subroutine is intended to determine the parameters
 *   of the floating-point arithmetic system specified below.  the
 *   determination of the first three uses an extension of an algorithm
 *   due to m. malcolm, cacm 15 (1972), pp. 949-951, incorporating some,
 *   but not all, of the improvements suggested by m. gentleman and s.
 *   marovich, cacm 17 (1974), pp. 276-277.  an earlier version of this
 *   program was published in the book software manual for the
 *   elementary functions by w. j. cody and w. waite, prentice-hall,
 *   englewood cliffs, nj, 1980.
 *
 *  the program as given here must be modified before compiling.  if
 *   a single (double) precision version is desired, change all
 *   occurrences of cs (  ) in columns 1 and 2 to blanks.
 *
 *  parameter values reported are as follows:
 *
 *       ibeta   - the radix for the floating-point representation
 *       it      - the number of base ibeta digits in the floating-point
 *                 significand
 *       irnd    - 0 if floating-point addition chops
 *                 1 if floating-point addition rounds, but not in the
 *                   ieee style
 *                 2 if floating-point addition rounds in the ieee style
 *                 3 if floating-point addition chops, and there is
 *                   partial underflow
 *                 4 if floating-point addition rounds, but not in the
 *                   ieee style, and there is partial underflow
 *                 5 if floating-point addition rounds in the ieee style,
 *                   and there is partial underflow
 *       ngrd    - the number of guard digits for multiplication with
 *                 truncating arithmetic.  it is
 *                 0 if floating-point arithmetic rounds, or if it
 *                   truncates and only  it  base  ibeta digits
 *                   participate in the post-normalization shift of the
 *                   floating-point significand in multiplication;
 *                 1 if floating-point arithmetic truncates and more
 *                   than  it  base  ibeta  digits participate in the
 *                   post-normalization shift of the floating-point
 *                   significand in multiplication.
 *       machep  - the largest negative integer such that
 *                 1.0+float(ibeta)**machep .ne. 1.0, except that
 *                 machep is bounded below by  -(it+3)
 *       negeps  - the largest negative integer such that
 *                 1.0-float(ibeta)**negeps .ne. 1.0, except that
 *                 negeps is bounded below by  -(it+3)
 *       iexp    - the number of bits (decimal places if ibeta = 10)
 *                 reserved for the representation of the exponent
 *                 (including the bias or sign) of a floating-point
 *                 number
 *       minexp  - the largest in magnitude negative integer such that
 *                 float(ibeta)**minexp is positive and normalized
 *       maxexp  - the smallest positive power of  beta  that overflows
 *       eps     - the smallest positive floating-point number such
 *                 that  1.0+eps .ne. 1.0. in particular, if either
 *                 ibeta = 2  or  irnd = 0, eps = float(ibeta)**machep.
 *                 otherwise,  eps = (float(ibeta)**machep)/2
 *       epsneg  - a small positive floating-point number such that
 *                 1.0-epsneg .ne. 1.0. in particular, if ibeta = 2
 *                 or  irnd = 0, epsneg = float(ibeta)**negeps.
 *                 otherwise,  epsneg = (ibeta**negeps)/2.  because
 *                 negeps is bounded below by -(it+3), epsneg may not
 *                 be the smallest number that can alter 1.0 by
 *                 subtraction.
 *       xmin    - the smallest non-vanishing normalized floating-point
 *                 power of the radix, i.e.,  xmin = float(ibeta)**minexp
 *       xmax    - the largest finite floating-point number.  in
 *                 particular  xmax = (1.0-epsneg)*float(ibeta)**maxexp
 *                 note - on some machines  xmax  will be only the
 *                 second, or perhaps third, largest number, being
 *                 too small by 1 or 2 units in the last digit of
 *                 the significand.
 *
 *     latest revision - april 20, 1987
 *
 *     author - w. j. cody
 *              argonne national laboratory
 *
 */


static void
machar(int *ibeta, int *it, int *irnd, int *ngrd, int *machep, int *negep,
       int *iexp, int *minexp, int *maxexp, double *eps,
       double *epsneg, double *xmin, double *xmax)
{
	volatile double a, b, beta, betain, betah, one,
		t, temp, tempa, temp1, two, y, z, zero;
	int i, itemp, iz, j, k, mx, nxres;

	one = 1;
	two = one+one;
	zero = one-one;

		/* determine ibeta, beta ala malcolm. */

	a = one;
	do {
		a = a + a;
		temp = a + one;
		temp1 = temp - a;
	}
	while(temp1 - one == zero);
	b = one;
	do {
		b = b + b;
		temp = a + b;
		itemp = (int)(temp - a);
	}
	while (itemp == 0);
	*ibeta = itemp;
	beta = *ibeta;

		/* determine it, irnd */

	*it = 0;
	b = one;
	do {
		*it = *it + 1;
		b = b * beta;
		temp = b + one;
		temp1 = temp - b;
	}
	while(temp1 - one == zero);
	*irnd = 0;
	betah = beta / two;
	temp = a + betah;
	if (temp - a != zero)
		*irnd = 1;
	tempa = a + beta;
	temp = tempa + betah;
	if (*irnd == 0 && temp - tempa != zero)
		*irnd = 2;

		/* determine negep, epsneg */

	*negep = *it + 3;
	betain = one / beta;
	a = one;
	for(i=1 ; i<=*negep ; i++)
		a = a * betain;
	b = a;
	for(;;) {
		temp = one - a;
		if (temp - one != zero)
			break;
		a = a * beta;
		*negep = *negep - 1;
	}
	*negep = -*negep;
	*epsneg = a;
	if (*ibeta != 2 && *irnd != 0) {
		a = (a * (one + a)) / two;
		temp = one - a;
		if (temp - one != zero)
			*epsneg = a;
	}

		/* determine machep, eps */

	*machep = -*it - 3;
	a = b;
	for(;;) {
		temp = one + a;
		if (temp - one != zero)
			break;
		a = a * beta;
		*machep = *machep + 1;
	}
	*eps = a;
	temp = tempa + beta * (one + *eps);
	if (*ibeta != 2 && *irnd != 0) {
		a = (a * (one + a)) / two;
		temp = one + a;
		if (temp - one != zero)
			*eps = a;
	}

		/* determine ngrd */

	*ngrd = 0;
	temp = one + *eps;
	if (*irnd == 0 && temp * one - one != zero)
		*ngrd = 1;

	/* determine iexp, minexp, xmin */

	/* loop to determine largest i and k = 2**i such that */
	/*        (1/beta) ** (2**(i)) */
	/* does not underflow. */
	/* exit from loop is signaled by an underflow. */

	i = 0;
	k = 1;
	z = betain;
	t = one + *eps;
	nxres = 0;
	for(;;) {
		y = z;
		z = y * y;

		/* check for underflow here */

		a = z * one;
		temp = z * t;
		if (a+a == zero || fabs(z) >= y)
			break;
		temp1 = temp * betain;
		if (temp1 * beta == z)
			break;
		i = i+1;
		k = k+k;
	}
	if (*ibeta != 10) {
		*iexp = i + 1;
		mx = k + k;
	}
	else {
		/* this segment is for decimal machines only */

		*iexp = 2;
		iz = *ibeta;
		while (k >= iz) {
			iz = iz * *ibeta;
			iexp = iexp + 1;
		}
		mx = iz + iz - 1;
	}
	do {
		/* loop to determine minexp, xmin */
		/* exit from loop is signaled by an underflow */

		*xmin = y;
		y = y * betain;

		/* check for underflow here */

		a = y * one;
		temp = y * t;
		if (a+a == zero || fabs(y) >= *xmin)
			goto L10;
		k = k + 1;
		temp1 = temp * betain;
	}
	while(temp1 * beta != y);
	nxres = 3;
	*xmin = y;
L10:	*minexp = -k;

	/* determine maxexp, xmax */

	if (mx <= k + k - 3 && *ibeta != 10) {
		mx = mx + mx;
		*iexp = *iexp + 1;
	}
	*maxexp = mx + *minexp;

	/* adjust irnd to reflect partial underflow */

	*irnd = *irnd + nxres;

	/* adjust for ieee-style machines */

	if (*irnd == 2 || *irnd == 5)
		*maxexp = *maxexp - 2;

	/* adjust for non-ieee machines with partial underflow */

	if (*irnd == 3 || *irnd == 4)
		*maxexp = *maxexp - *it;

	/* adjust for machines with implicit leading bit in binary */
	/* significand, and machines with radix point at extreme */
	/* right of significand. */

	i = *maxexp + *minexp;
	if (*ibeta == 2 && i == 0)
		*maxexp = *maxexp - 1;
	if (i > 20)
		*maxexp = *maxexp - 1;
	if (a != y)
		*maxexp = *maxexp - 2;
	*xmax = one - *epsneg;
	if (*xmax * one != *xmax)
		*xmax = one - beta * *epsneg;
	*xmax = *xmax / (beta * beta * beta * *xmin);
	i = *maxexp + *minexp + 3;
	if (i>0)
		for(j=1 ; j<=i ; j++) {
			if (*ibeta == 2)
				*xmax = *xmax + *xmax;
			if (*ibeta != 2)
				*xmax = *xmax * beta;
		}
}
