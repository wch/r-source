/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025 The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include "RBufferUtils.h"
#include <Fileio.h>
#include <ctype.h>			/* toupper */
//#include <float.h> // -> FLT_RADIX
#include <limits.h>
#include <string.h>
#include <stdlib.h>			/* for realpath */
#include <time.h>			/* for ctime */
#include <errno.h>

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
#include <windows.h>
#include <aclapi.h>			/* for GetSecurityInfo */
typedef BOOLEAN (WINAPI *PCSL)(LPWSTR, LPWSTR, DWORD);
const char *formatError(DWORD res);  /* extra.c */
/* Windows does not have link(), but it does have CreateHardLink() on NTFS */
#undef HAVE_LINK
#define HAVE_LINK 1
/* Windows does not have symlink(), but >= Vista does have
   CreateSymbolicLink() on NTFS */
#undef HAVE_SYMLINK
#define HAVE_SYMLINK 1
#endif

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
   AQUA builds. Also we want to be able to use "mac.binary.mavericks",
   "mac.binary.el-capitan" and similar. */
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

void Init_R_Machine(SEXP rho); // from machine.c

attribute_hidden void Init_R_Variables(SEXP rho)
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

static char native_enc[R_CODESET_MAX + 1];
attribute_hidden const char *R_nativeEncoding(void)
{
    return native_enc;
}

#ifdef Win32
static int defaultLocaleACP(const char *ctype)
{
    wchar_t wdefaultCP[6];
    size_t n, r;
    char defaultCP[6];

    n = strlen(ctype) + 1;
    wchar_t wctype[n];
    r = mbstowcs(wctype, ctype, n);
    if (r == (size_t)-1 || r >= n)
	return 0;

    /* It is not clear from the Microsoft documentation that GetLocaleInfoEx
       accepts all locale names returned by setlocale(). Hopefully this will
       get clarified. */
    if (!GetLocaleInfoEx(wctype, LOCALE_IDEFAULTANSICODEPAGE, wdefaultCP,
                         sizeof(wdefaultCP)))
	return 0;

    n = wcslen(wctype) + 1;
    r = wcstombs(defaultCP, wdefaultCP, n);
    if (r == (size_t)-1 || r >= n)
	return 0;

    if (!isdigit(defaultCP[0]))
	return 0;
    return atoi(defaultCP);
}
#endif

/* retrieves information about the current locale and
   sets the corresponding variables (known_to_be_utf8,
   known_to_be_latin1, utf8locale, latin1locale and mbcslocale) */

static char codeset[R_CODESET_MAX + 1];
attribute_hidden void R_check_locale(void)
{
    known_to_be_utf8 = utf8locale = FALSE;
    known_to_be_latin1 = latin1locale = FALSE;
    mbcslocale = FALSE;
    strcpy(native_enc, "ASCII");
    strcpy(codeset, "");
#ifdef HAVE_LANGINFO_CODESET
    /* not on Windows */
    {
	char  *p = nl_langinfo(CODESET);
	strcpy(codeset, p);  // copy just in case something else calls nl_langinfo.
	/* more relaxed due to Darwin: CODESET is case-insensitive and
	   latin1 is ISO8859-1 */
	if (R_strieql(p, "UTF-8")) known_to_be_utf8 = utf8locale = TRUE;
	if (streql(p, "ISO-8859-1")) known_to_be_latin1 = latin1locale = TRUE;
	if (R_strieql(p, "ISO8859-1")) known_to_be_latin1 = latin1locale = TRUE;
# if __APPLE__
	/* On Darwin 'regular' locales such as 'en_US' are UTF-8 (hence
	   MB_CUR_MAX == 6), but CODESET is "" 
	   2021: that comment dated from 2008: MB_CUR_MAX is now 4 in 
	   a UTF-8 locale, even on 10.13. 
	*/
	if (*p == 0 && (MB_CUR_MAX == 4 || MB_CUR_MAX == 6)) {
	    known_to_be_utf8 = utf8locale = TRUE;
	    strcpy(codeset, "UTF-8");
	}
# endif
	if (utf8locale)
	    strcpy(native_enc, "UTF-8");
	else if (latin1locale)
	    strcpy(native_enc, "ISO-8859-1");
	else {
	    strncpy(native_enc, p, R_CODESET_MAX);
	    native_enc[R_CODESET_MAX] = 0;
	}
    }
#endif
    mbcslocale = MB_CUR_MAX > 1;
    R_MB_CUR_MAX = (int)MB_CUR_MAX;
#ifdef __sun
    /* Solaris 10 (at least) has MB_CUR_MAX == 3 in some, but ==4
       in other UTF-8 locales. The former does not allow working
       with non-BMP characters using mbrtowc(). Work-around by
       allowing to use more. */
    if (utf8locale && R_MB_CUR_MAX < 4)
	R_MB_CUR_MAX = 4;
#endif
#ifdef Win32
    {
	char *ctype = setlocale(LC_CTYPE, NULL), *p;
	p = strrchr(ctype, '.');
	localeCP = 0;
	if (p) {
	    if (isdigit(p[1]))
		localeCP = atoi(p+1);
	    else if (!strcasecmp(p+1, "UTF-8") || !strcasecmp(p+1, "UTF8"))
		localeCP = 65001;
	} else if (strcmp(ctype, "C"))
	    /* setlocale() will fill in the codepage automatically for
	       "English", but not "en-US" */
	    localeCP = defaultLocaleACP(ctype);

	/* Not 100% correct, but CP1252 is a superset */
	known_to_be_latin1 = latin1locale = (localeCP == 1252);
	known_to_be_utf8 = utf8locale = (localeCP == 65001);
	if (localeCP == 65001)
	    strcpy(native_enc, "UTF-8");
	else if (localeCP) {
	    /* CP1252 when latin1locale is true */
	    snprintf(native_enc, R_CODESET_MAX, "CP%d", localeCP);
	    native_enc[R_CODESET_MAX] = 0;
	}
	systemCP = GetACP();
    }
#endif
}

/*  date
 *
 *  Return the current date in a standard format.  This uses standard
 *  POSIX calls which should be available on each platform.
 *  time and ctime are ISO C calls, so we don't check them.
 *
 * This needs the system time_t.
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

attribute_hidden SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
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
attribute_hidden SEXP do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl, hd, pg;
    const char **f, **h, *t, *pager = NULL /* -Wall */;
    bool dl;
    int i, n;

    checkArity(op, args);
    fn = CAR(args); args = CDR(args);
    hd = CAR(args); args = CDR(args);
    tl = CAR(args); args = CDR(args);
    dl = asBool2(CAR(args), call); args = CDR(args);
    pg = CAR(args);
    n = 0;			/* -Wall */
    if (!isString(fn) || (n = LENGTH(fn)) < 1)
	error(_("invalid filename specification"));
    if (!isString(hd) || LENGTH(hd) != n)
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
	    f[i] = acopy_string(translateCharFP(el));
	else
	    error(_("invalid filename specification"));
	if (STRING_ELT(hd, i) != NA_STRING)
	    h[i] = acopy_string(translateCharFP(STRING_ELT(hd, i)));
	else
	    error(_("invalid '%s' argument"), "headers");
    }
    if (isValidStringF(tl))
	t = acopy_string(translateCharFP(STRING_ELT(tl, 0)));
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

/* Coreutils use 128K (with some adjustments based on st_blksize
   and file size). Python uses 1M for Windows and 64K for other platforms.
   R 4.1 and earlier used min(BUFSIZ, 512), increased in R 4.2 (PR#18245). */
#ifdef Win32
# define APPENDBUFSIZE (1024*1024)
#else
# define APPENDBUFSIZE (128*1024)
#endif

/* RC_fopen but fails when fn is a directory.

   On Linux, a directory can be opened for reading, but not on Windows
   (PR#17337). */
static FILE
*RC_fopen_notdir(const SEXP fn, const char *mode, const bool expand)
{
    FILE *f = RC_fopen(fn, mode, expand);
#ifdef HAVE_SYS_STAT_H
    if (f) {
	struct stat sb;
	if (!fstat(fileno(f), &sb) && S_ISDIR(sb.st_mode)) {
	    fclose(f);
	    return NULL;
	}
    }
#endif
    return f;
}

static int R_AppendFile(SEXP file1, SEXP file2)
{
    FILE *fp1, *fp2;
    size_t nchar;
    int status = 0;
    if ((fp1 = RC_fopen_notdir(file1, "ab", TRUE)) == NULL) return 0;
    if ((fp2 = RC_fopen_notdir(file2, "rb", TRUE)) == NULL) {
	fclose(fp1);
	return 0;
    }
    char *buf = (char *)malloc(APPENDBUFSIZE);
    if (!buf) {
	fclose(fp1);
	fclose(fp2);
	error("could not allocate copy buffer");
    }
    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
	if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE) goto append_error;
    if (fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
    status = 1;
 append_error:
    free(buf);
    if (status == 0) warning(_("write error during file append"));
    fclose(fp1);
    fclose(fp2);
    return status;
}

attribute_hidden SEXP do_fileappend(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int n, n1, n2;

    checkArity(op, args);
    f1 = CAR(args);
    f2 = CADR(args);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "file1");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "file2");
    n1 = LENGTH(f1); n2 = LENGTH(f2);
    if (n1 < 1)
	error(_("nothing to append to"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));
    for (int i = 0; i < n; i++) LOGICAL(ans)[i] = 0;  /* all FALSE */
    if (n1 == 1) { /* common case */
	FILE *fp1, *fp2;
	int status = 0;
	size_t nchar;
	if (STRING_ELT(f1, 0) == NA_STRING ||
	    !(fp1 = RC_fopen_notdir(STRING_ELT(f1, 0), "ab", TRUE)))
	   goto done;
	for (int i = 0; i < n; i++) {
	    status = 0;
	    if (STRING_ELT(f2, i) == NA_STRING ||
	       !(fp2 = RC_fopen_notdir(STRING_ELT(f2, i), "rb", TRUE))) continue;
	    char *buf = (char *)malloc(APPENDBUFSIZE);
	    if (!buf) {
		fclose(fp1);
		fclose(fp2);
		error("could not allocate copy buffer");
	    }
	    while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
		if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE) {
		    free(buf);
		    goto append_error;
		}
	    if (fwrite(buf, 1, nchar, fp1) != nchar) {
		free(buf);
		goto append_error;
	    }
	    free(buf);
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

attribute_hidden SEXP do_filecreate(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    n = LENGTH(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(fn, i) == NA_STRING) continue;
	if ((fp = RC_fopen(STRING_ELT(fn, i), "w", TRUE)) != NULL) {
	    LOGICAL(ans)[i] = 1;
	    fclose(fp);
	} else if (show) {
	    // translateChar will translate the file, using escapes
	    warning(_("cannot create file '%s', reason '%s'"),
		    translateChar(STRING_ELT(fn, i)), strerror(errno));
	}
    }
    UNPROTECT(1);
    return ans;
}

attribute_hidden SEXP do_fileremove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f, ans;
    int i, n;
    checkArity(op, args);
    f = CAR(args);
    if (!isString(f))
	error(_("invalid first filename"));
    n = LENGTH(f);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f, i) != NA_STRING) {
	    LOGICAL(ans)[i] =
#ifdef Win32
		(_wremove(filenameToWchar(STRING_ELT(f, i), TRUE)) == 0);
#else
		(remove(R_ExpandFileName(translateCharFP(STRING_ELT(f, i)))) == 0);
#endif
	    if(!LOGICAL(ans)[i])
		warning(_("cannot remove file '%s', reason '%s'"),
			translateChar(STRING_ELT(f, i)), strerror(errno));
	} else LOGICAL(ans)[i] = FALSE;
    }
    UNPROTECT(1);
    return ans;
}

/* the Win32 stuff here is not ready for release:

   (i) It needs Windows >= Vista
   (ii) It matters whether 'from' is a file or a dir, and we could only
   know if it exists already.
   (iii) This needs specific privileges which in general only Administrators
   have, and which many people report granting in the Policy Editor
   fails to work.
*/
attribute_hidden SEXP do_filesymlink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_SYMLINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args);
    f2 = CADR(args);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    n1 = LENGTH(f1); n2 = LENGTH(f2);
    if (n1 < 1)
	error(_("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;

#ifdef HAVE_SYMLINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f1, i%n1) == NA_STRING ||
	    STRING_ELT(f2, i%n2) == NA_STRING)
	    LOGICAL(ans)[i] = 0;
	else {
#ifdef Win32
	    wchar_t *from, *to, *p;
	    struct _stati64 sb;
	    p = filenameToWchar(STRING_ELT(f1, i%n1), TRUE);
	    from = (wchar_t*) R_alloc(wcslen(p) + 1, sizeof(wchar_t));
	    wcscpy(from, p);
	    /* This Windows system call does not accept slashes */
	    R_wfixbackslash(from);
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    _wstati64(from, &sb);
	    int isDir = (sb.st_mode & S_IFDIR) > 0;
	    LOGICAL(ans)[i] = CreateSymbolicLinkW(to, from, isDir) != 0;
	    if(!LOGICAL(ans)[i])
		warning(_("cannot symlink '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
#else
	    char from[R_PATH_MAX], to[R_PATH_MAX];
	    const char *p;
	    p = R_ExpandFileName(translateCharFP(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= R_PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);

	    p = R_ExpandFileName(translateCharFP(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= R_PATH_MAX - 1) {
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


attribute_hidden SEXP do_filelink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_LINK
    SEXP ans;
    int i;
#endif
    checkArity(op, args);
    f1 = CAR(args);
    f2 = CADR(args);
    if (!isString(f1))
	error(_("invalid first filename"));
    if (!isString(f2))
	error(_("invalid second filename"));
    n1 = LENGTH(f1); n2 = LENGTH(f2);
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
	    wchar_t *from, *to, *p;
	    p = filenameToWchar(STRING_ELT(f1, i%n1), TRUE);
	    from = (wchar_t*) R_alloc(wcslen(p) + 1, sizeof(wchar_t));
	    wcscpy(from, p);
	    to = filenameToWchar(STRING_ELT(f2, i%n2), TRUE);
	    LOGICAL(ans)[i] = CreateHardLinkW(to, from, NULL) != 0;
	    if(!LOGICAL(ans)[i]) {
		warning(_("cannot link '%ls' to '%ls', reason '%s'"),
			from, to, formatError(GetLastError()));
	    }
#else
	    char from[R_PATH_MAX], to[R_PATH_MAX];
	    const char *p;
	    p = R_ExpandFileName(translateCharFP(STRING_ELT(f1, i%n1)));
	    if (strlen(p) >= R_PATH_MAX - 1) {
		LOGICAL(ans)[i] = 0;
		continue;
	    }
	    strcpy(from, p);

	    p = R_ExpandFileName(translateCharFP(STRING_ELT(f2, i%n2)));
	    if (strlen(p) >= R_PATH_MAX - 1) {
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

attribute_hidden SEXP do_filerename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n1, n2;
    int res;
#ifdef Win32
    wchar_t *from, *to;
    const wchar_t *w;
#else
    char from[R_PATH_MAX], to[R_PATH_MAX];
    const char *p;
#endif

    checkArity(op, args);
    f1 = CAR(args);
    f2 = CADR(args);
    if (!isString(f1))
	error(_("invalid '%s' argument"), "from");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "to");
    n1 = LENGTH(f1); n2 = LENGTH(f2);
   if (n2 != n1)
       error(_("'%s' and '%s' are of different lengths"), "from", "to");
    PROTECT(ans = allocVector(LGLSXP, n1));
    for (i = 0; i < n1; i++) {
	if (STRING_ELT(f1, i) == NA_STRING ||
	    STRING_ELT(f2, i) == NA_STRING) {
	    LOGICAL(ans)[i] = 0;
	    continue;
	}
#ifdef Win32
	w = filenameToWchar(STRING_ELT(f1, i), TRUE);
	from = (wchar_t *) R_alloc(wcslen(w) + 1, sizeof(wchar_t));
	wcscpy(from, w);
	w = filenameToWchar(STRING_ELT(f2, i), TRUE);
	to = (wchar_t *) R_alloc(wcslen(w) + 1, sizeof(wchar_t));
	wcscpy(to, w);
	res = Rwin_wrename(from, to);
	if(res) {
	    warning(_("cannot rename file '%ls' to '%ls', reason '%s'"),
		    from, to, formatError(GetLastError()));
	}
	LOGICAL(ans)[i] = (res == 0);
#else
	p = R_ExpandFileName(translateCharFP(STRING_ELT(f1, i)));
	if (strlen(p) >= R_PATH_MAX - 1)
	    error(_("expanded 'from' name too long"));
	strncpy(from, p, R_PATH_MAX - 1);
	p = R_ExpandFileName(translateCharFP(STRING_ELT(f2, i)));
	if (strlen(p) >= R_PATH_MAX - 1)
	    error(_("expanded 'to' name too long"));
	strncpy(to, p, R_PATH_MAX - 1);
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

attribute_hidden SEXP do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans, ansnames, fsize, mtime, ctime, atime, isdir,
	mode, xxclass, uname = R_NilValue;
    const void *vmax = vmaxget();
#ifdef UNIX_EXTRAS
    SEXP uid = R_NilValue, gid = R_NilValue,
	grname = R_NilValue; // silence -Wall
#endif
#ifdef Win32
    SEXP exe = R_NilValue, udomain = R_NilValue;
    char *ubuf = NULL;
    DWORD ubuflen = 0;
    char *dbuf = NULL;
    DWORD dbuflen = 0;
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
    int n = LENGTH(fn), ncols = 6;
    if(extras) {
#ifdef UNIX_EXTRAS
	ncols = 10;
#elif defined(Win32)
	ncols = 9;
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
	uname = SET_VECTOR_ELT(ans, 7, allocVector(STRSXP, n));
	SET_STRING_ELT(ansnames, 7, mkChar("uname"));
	udomain = SET_VECTOR_ELT(ans, 8, allocVector(STRSXP, n));
	SET_STRING_ELT(ansnames, 8, mkChar("udomain"));
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
	const char *p = translateCharFP2(STRING_ELT(fn, i));
	const char *efn = p ? R_ExpandFileName(p) : p;
#endif
	if (STRING_ELT(fn, i) != NA_STRING &&
#ifdef Win32
	    _wstati64(wfn, &sb)
#else
	    /* Target not link */
	    p && stat(efn, &sb)
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
#ifdef Win32
#define WINDOWS_TICK 10000000
#define SEC_TO_UNIX_EPOCH 11644473600LL
	    {
		FILETIME c_ft, a_ft, m_ft;
		HANDLE h;
		int success = 0;
		h = CreateFileW(wfn, 0,
		                FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
		                NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
		if (h != INVALID_HANDLE_VALUE) {
		    int res  = GetFileTime(h, &c_ft, &a_ft, &m_ft);
		    CloseHandle(h);
		    if (res) {
			ULARGE_INTEGER time;
			time.LowPart = m_ft.dwLowDateTime;
			time.HighPart = m_ft.dwHighDateTime;
			REAL(mtime)[i] = (((double) time.QuadPart) / WINDOWS_TICK - SEC_TO_UNIX_EPOCH);
			time.LowPart = c_ft.dwLowDateTime;
			time.HighPart = c_ft.dwHighDateTime;
			REAL(ctime)[i] = (((double) time.QuadPart) / WINDOWS_TICK - SEC_TO_UNIX_EPOCH);
			time.LowPart = a_ft.dwLowDateTime;
			time.HighPart = a_ft.dwHighDateTime;
			REAL(atime)[i] = (((double) time.QuadPart) / WINDOWS_TICK - SEC_TO_UNIX_EPOCH);
			success = 1;
		    }
		} else
		    warning(_("cannot open file '%ls': %s"),
		            wfn, formatError(GetLastError()));
		if (!success) {
		    REAL(mtime)[i] = NA_REAL;
		    REAL(ctime)[i] = NA_REAL;
		    REAL(atime)[i] = NA_REAL;
	        }
	    }
#else
	    REAL(mtime)[i] = (double) sb.st_mtime;
	    REAL(ctime)[i] = (double) sb.st_ctime;
	    REAL(atime)[i] = (double) sb.st_atime;
# ifdef STAT_TIMESPEC_NS
	    REAL(mtime)[i] += STAT_TIMESPEC_NS (sb, st_mtim);
	    REAL(ctime)[i] += STAT_TIMESPEC_NS (sb, st_ctim);
	    REAL(atime)[i] += STAT_TIMESPEC_NS (sb, st_atim);
# endif
#endif
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
		{
		    HANDLE h;
		    PSID owner_sid;
		    SID_NAME_USE suse = SidTypeUnknown;
		    PSECURITY_DESCRIPTOR sd = NULL;
		    DWORD saveerr = ERROR_SUCCESS;
		    int ok = 0;
		    /* NOTE: GENERIC_READ would be asking too much
		       (e.g. junctions under Users/Default in Windows 10) */
		    h = CreateFileW(wfn, READ_CONTROL,
				    FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE,
				    NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
		    ok = (h != INVALID_HANDLE_VALUE);

		    ok = ok && (GetSecurityInfo(h, SE_FILE_OBJECT,
					        OWNER_SECURITY_INFORMATION,
					        &owner_sid,
					        NULL, NULL, NULL, &sd)
		                == ERROR_SUCCESS);
		    if (ok) {
			DWORD ulen = ubuflen;
			DWORD dlen = dbuflen;
			ok = LookupAccountSid(NULL, owner_sid, ubuf, &ulen,
			                      dbuf, &dlen, &suse);
			if (!ok
			    && GetLastError() == ERROR_INSUFFICIENT_BUFFER
			    && (ulen > ubuflen || dlen > dbuflen)) {

			    if (ulen > ubuflen) {
				ubuf = R_alloc(ulen, 1);
				ubuflen = ulen;
			    }
			    if (dlen > dbuflen) {
				dbuf = R_alloc(dlen, 1);
				dbuflen = dlen;
			    }
			    ok = LookupAccountSid(NULL, owner_sid, ubuf, &ulen,
			                          dbuf, &dlen, &suse);
			}
		    }
		    if (!ok)
			saveerr = GetLastError();
		    if (sd)
			LocalFree(sd);
		    if (h != INVALID_HANDLE_VALUE)
			CloseHandle(h);
		    if (ok) {
			SET_STRING_ELT(uname, i, mkChar(ubuf));
			SET_STRING_ELT(udomain, i, mkChar(dbuf));
		    } else {
			warning(_("cannot resolve owner of file '%ls': %s"),
				wfn, formatError(saveerr));
			SET_STRING_ELT(uname, i, NA_STRING);
			SET_STRING_ELT(udomain, i, NA_STRING);
		    }
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
    vmaxset(vmax);
    UNPROTECT(3);
    return ans;
}

attribute_hidden SEXP do_direxists(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    int n = LENGTH(fn);
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
	if (STRING_ELT(fn, i) != NA_STRING && _wstati64(wfn, &sb) == 0) {
	    LOGICAL(ans)[i] = (sb.st_mode & S_IFDIR) > 0;

	} else LOGICAL(ans)[i] = 0;
#else
	const char *p = translateCharFP2(STRING_ELT(fn, i));
	if (p && STRING_ELT(fn, i) != NA_STRING &&
	    /* Target not link */
	    stat(R_ExpandFileName(p), &sb) == 0) {
	    LOGICAL(ans)[i] = (sb.st_mode & S_IFDIR) > 0;
	} else LOGICAL(ans)[i] = 0;
#endif
    }
    // copy names?
    UNPROTECT(1);
    return ans;
}

/* No longer required by POSIX, but maybe on earlier OSes */
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

/* POSIX opendir/readdir/closedir is not supported by Windows. MinGW-W64 has
   an implementation, which however does not support long paths.

   R_opendir/R_readdir/R_closedir implement a subset of the functionality,
   on Unix they fall back to POSIX API, on Windows they support long paths.
   Unlike MinGW-W64, they use wide-string search functions internally to
   support file names up to MAX_PATH wide characters.
   Note that d_name pointer may change between readdir operations.

   R_wopendir/R_wreaddir/R_wclosedir are wide-string variants for Windows. */

#ifdef Unix
# if HAVE_DIRENT_H
#  include <dirent.h>
# elif HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# elif HAVE_SYS_DIR_H
#  include <sys/dir.h>
# elif HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

struct R_DIR_INTERNAL {
#ifdef Win32
    wchar_t *pattern;
    WIN32_FIND_DATAW fdata;
    HANDLE hfind;
    R_StringBuffer cbuff;
#else
    DIR *dirp;
#endif
    struct R_dirent de;
};

#ifdef Win32
static wchar_t* search_wpattern(const wchar_t *name)
{
    const void *vmax = vmaxget();
    wchar_t *apath = R_getFullPathNameW(name);
    if (!apath) {
	errno = EFAULT;
	vmaxset(vmax);
	return NULL;
    }
    size_t len = wcslen(apath);
    /* <slash><star><null> */
    wchar_t *pattern = malloc((len + 3) * sizeof(wchar_t));
    if (!pattern) {
	errno = EFAULT;
	vmaxset(vmax);
	return NULL;
    }
    memcpy(pattern, apath, len * sizeof(wchar_t));
    /* apath is not D: (that would have been expanded) */

    /* add separator if not present and pattern not empty */
    if (len > 0 && pattern[len-1] != L'\\' && pattern[len-1] != L'/')
	pattern[len++] = L'/';
    pattern[len++] = L'*';
    pattern[len] = L'\0';
    vmaxset(vmax);
    return pattern;
}
#endif

R_DIR *R_opendir(const char *name)
{
    R_DIR *rdir = malloc(sizeof(R_DIR));
    if (!rdir) {
	errno = ENOMEM;
	return NULL;
    }
#ifdef Win32
    DWORD r = GetFileAttributes(name);
    if (r == INVALID_FILE_ATTRIBUTES) {
	errno = ENOENT;
	free(rdir);
	return NULL;
    }
    if (!(r & FILE_ATTRIBUTE_DIRECTORY)) {
	errno = ENOTDIR;
	free(rdir);
	return NULL;
    }
    int nc = (int) mbstowcs(NULL, name, 0);
    if (nc < 0) {
	errno = ENOENT;
	free(rdir);
	return NULL;
    }
    const void *vmax = vmaxget();
    wchar_t *wname = (wchar_t *) R_alloc(nc + 1, sizeof(wchar_t));
    mbstowcs(wname, name, nc + 1);
    rdir->pattern = search_wpattern(wname); /* malloc'd */
    if (!rdir->pattern) {
	vmaxset(vmax);
	free(rdir);
	return NULL;
    }
    rdir->hfind = INVALID_HANDLE_VALUE;
    rdir->cbuff.data = NULL;
    rdir->cbuff.bufsize = 0;
    rdir->cbuff.defaultSize = MAXELTSIZE;
    vmaxset(vmax);
#else
    rdir->dirp = opendir(name);
    if (!rdir->dirp) {
	free(rdir);
	return NULL;
    }
#endif
    return rdir;
}

struct R_dirent *R_readdir(R_DIR *rdir)
{
    if (!rdir) {
	errno = EFAULT;
	return NULL; 
    }
#ifdef Win32
    if (rdir->pattern) {
	/* starting the search */
	rdir->hfind = FindFirstFileW(rdir->pattern, &rdir->fdata);
	free(rdir->pattern);
	rdir->pattern = NULL;
	if (rdir->hfind == INVALID_HANDLE_VALUE)
	    /* keep errno, no files, even though not likely (., ..) */
	    return NULL;
    } else if (rdir->hfind != INVALID_HANDLE_VALUE) {
	/* continuing the search */
	if (!FindNextFileW(rdir->hfind, &rdir->fdata)) {
	    if (GetLastError() != ERROR_NO_MORE_FILES)
		warning(_("error while listing a directory: '%s'"),
		    formatError(GetLastError()));
	    /* keep errno, no more files */
	    return NULL;
	}
    } else {
	errno = EFAULT;
	return NULL;
    }
    wchar_t *wname = (wchar_t *)&rdir->fdata.cFileName;
    int nb = (int) wcstombs(NULL, wname, 0);
    if (nb < 0)
	/* invalid strings stop the search */
	return NULL;
    R_AllocStringBuffer(nb + 1, &rdir->cbuff);
    wcstombs(rdir->cbuff.data, wname, nb + 1);
    rdir->de.d_name = rdir->cbuff.data;
    return &rdir->de;
#else
    struct dirent *de;
    de = readdir(rdir->dirp);
    if (de) {
	rdir->de.d_name = de->d_name;
	return &rdir->de;
    } else
	return NULL;
#endif
}

int R_closedir(R_DIR *rdir)
{
    if (!rdir) {
	errno = EFAULT;
	return -1;
    }
#ifdef Win32
    R_FreeStringBuffer(&rdir->cbuff);
    if (rdir->pattern)
	free(rdir->pattern);
    BOOL r = 0;
    if (rdir->hfind != INVALID_HANDLE_VALUE)
	r = FindClose(rdir->hfind);
    free(rdir);
    if (r)
	return 0;
    else {
	errno = EFAULT;
	return -1;
    }
#else
    int res = closedir(rdir->dirp);
    free(rdir);
    return res;
#endif
}

#ifdef Win32

struct R_WDIR_INTERNAL {
    wchar_t *pattern;
    WIN32_FIND_DATAW fdata;
    HANDLE hfind;
    struct R_wdirent de;
};

attribute_hidden R_WDIR *R_wopendir(const wchar_t *name)
{
    R_WDIR *rdir = malloc(sizeof(R_WDIR));
    if (!rdir) {
	errno = ENOMEM;
	return NULL;
    }
    DWORD r = GetFileAttributesW(name);
    if (r == INVALID_FILE_ATTRIBUTES) {
	errno = ENOENT;
	free(rdir);
	return NULL;
    }
    if (!(r & FILE_ATTRIBUTE_DIRECTORY)) {
	errno = ENOTDIR;
	free(rdir);
	return NULL;
    }
    rdir->pattern = search_wpattern(name); /* malloc'd */
    if (!rdir->pattern) {
	free(rdir);
	return NULL;
    }   
    rdir->hfind = INVALID_HANDLE_VALUE;
    return rdir;
}

attribute_hidden struct R_wdirent *R_wreaddir(R_WDIR *rdir)
{
    if (!rdir) {
	errno = EFAULT;
	return NULL; 
    }
    if (rdir->pattern) {
	/* starting the search */
	rdir->hfind = FindFirstFileW(rdir->pattern, &rdir->fdata);
	free(rdir->pattern);
	rdir->pattern = NULL;
	if (rdir->hfind == INVALID_HANDLE_VALUE)
	    /* keep errno, no files, even though not likely (., ..) */
	    return NULL;
	rdir->de.d_name = (wchar_t *)&rdir->fdata.cFileName;
	return &rdir->de;
    } else if (rdir->hfind != INVALID_HANDLE_VALUE) {
	/* continuing the search */
	if (!FindNextFileW(rdir->hfind, &rdir->fdata))
	    /* keep errno, no more files */
	    return NULL;
	return &rdir->de;
    } else {
	errno = EFAULT;
	return NULL;
    }
}

attribute_hidden int R_wclosedir(R_WDIR *rdir)
{
    if (!rdir) {
	errno = EFAULT;
	return -1;
    }
    if (rdir->pattern)
	free(rdir->pattern);
    BOOL r = 0;
    if (rdir->hfind != INVALID_HANDLE_VALUE)
	r = FindClose(rdir->hfind);
    free(rdir);
    if (r)
	return 0;
    else {
	errno = EFAULT;
	return -1;
    }
}
#endif

static
size_t path_buffer_append(R_StringBuffer *pb, const char *name, size_t len)
{
    size_t namelen = strlen(name);
    size_t newlen = len + namelen + 1;
    if (newlen > pb->bufsize)
	R_AllocStringBuffer(newlen, pb);
    if (namelen)
	memcpy(pb->data + len, name, namelen);
    pb->data[newlen - 1] = '\0';
#ifdef Unix
    if (newlen > R_PATH_MAX)
	warning(_("over-long path"));
#endif
    return newlen;
}

/* added_separator is a hack to once be removed, see comment in list_dirs */
static
bool search_setup(R_StringBuffer *pb, SEXP path, R_DIR **dir,
                      size_t *pathlen, bool *added_separator)
{
    if (added_separator)
	*added_separator = FALSE;
    if (path == NA_STRING)
	return FALSE;
    const char *p = translateCharFP2(path);
    if (!p)
	return FALSE;
    const char *dnp = R_ExpandFileName(p);

    size_t len = strlen(dnp);
    if (len + 1 > pb->bufsize)
	R_AllocStringBuffer(len + 1, pb);
    if (len)
	memcpy(pb->data, dnp, len);

    /* open directory */
    pb->data[len] = '\0';
    *dir = R_opendir(pb->data);
    /* This happens to succeed even for "d:".
       Note though R_IsDirPath() returns FALSE, because _wstati64() returns -1
       (file not found). */
    if (!*dir)
	return FALSE;

    /* add separator if needed */
#ifdef Win32
    /* don't turn D: into D:\, don't duplicate existing separator */
    if ((len == 2 && dnp[1] == ':') ||
        (len >= 1 && (dnp[len - 1] == '/' || dnp[len - 1] == '\\'))) {

	*pathlen = len;
	return (int) len;
    }
#endif
    pb->data[len] = FILESEP[0];
    if (added_separator)
	*added_separator = true;
    *pathlen = len + 1;
    return true;
}

static void search_cleanup(void *data)
{
    R_StringBuffer *pb = (R_StringBuffer *)data;
    R_FreeStringBuffer(pb);
}

static void add_to_ans(SEXP *pans, const char *pathstr, int *count,
                       int *countmax, PROTECT_INDEX idx)
{
    if (*count == *countmax - 1) {
	*countmax *= 2;
	REPROTECT(*pans = lengthgets(*pans, *countmax), idx);
    }
    SET_STRING_ELT(*pans, (*count)++, mkChar(pathstr));
}

#include <tre/tre.h>

/* when a match is found, (pb->data + offset) is the path added to the result

   len is the number of bytes of the path, which includes a trailing
   separator if needed, so that appending another element is done to
   (pb->data + len)

   pb->data is not null terminated by the caller

   dir is already opened by the caller
*/
static void
list_files(R_StringBuffer *pb, size_t offset, size_t len, int *count, SEXP *pans,
	   bool allfiles, bool recursive,
	   const regex_t *reg, int *countmax, PROTECT_INDEX idx,
	   bool idirs, bool allowdots, R_DIR *dir)
{
    struct R_dirent *de;
    R_CheckUserInterrupt(); // includes stack check
    while ((de = R_readdir(dir))) {
	if (allfiles || !R_HiddenFile(de->d_name)) {
	    /* append current name and null terminate */
	    size_t newlen = path_buffer_append(pb, de->d_name, len);
	    bool not_dot = strcmp(de->d_name, ".") && strcmp(de->d_name, "..");
	    if (recursive) {
		if (R_IsDirPath(pb->data)) {
		    if (not_dot) {
			if (idirs) {
			    if (!reg || tre_regexec(reg, de->d_name, 0, NULL, 0) == 0)
				add_to_ans(pans, pb->data + offset,
					   count, countmax, idx);
			}
			R_DIR *newdir = R_opendir(pb->data);
			if (newdir != NULL) {
			    /* turn terminator into file separator */
			    pb->data[newlen - 1] = FILESEP[0];
			    list_files(pb, offset, newlen,
				       count, pans, allfiles, recursive, reg,
				       countmax, idx, idirs, allowdots, newdir);
			    /* FIXME: arrange to close on error */
			    R_closedir(newdir);
			}
		    }
		    continue;
		}
	    } // end if(recursive)
	    if (not_dot || allowdots) {
		if (!reg || tre_regexec(reg, de->d_name, 0, NULL, 0) == 0)
		    add_to_ans(pans, pb->data + offset,
			       count, countmax, idx);
	    }
	}

    } // end while()
}

/* .Internal(list.files(path, pattern, all.files, full.names, recursive,
                        ignore.case, include.dirs, no..))
*/
attribute_hidden SEXP do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int countmax = 128;

    checkArity(op, args);
    SEXP d = CAR(args);  args = CDR(args); // d := directory = path
    if (!isString(d)) error(_("invalid '%s' argument"), "path");
    SEXP p = CAR(args); args = CDR(args);
    bool pattern = FALSE;
    if (isString(p) && LENGTH(p) >= 1 && STRING_ELT(p, 0) != NA_STRING)
	pattern = true;
    else if (!isNull(p) && !(isString(p) && LENGTH(p) < 1))
	error(_("invalid '%s' argument"), "pattern");
    bool allfiles = asBool2(CAR(args), call); args = CDR(args);
//    if (allfiles == NA_LOGICAL)
//	error(_("invalid '%s' argument"), "all.files");
    int fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    bool recursive = asBool2(CAR(args), call); args = CDR(args);
//    if (recursive == NA_LOGICAL)
//	error(_("invalid '%s' argument"), "recursive");
    int igcase = asLogical(CAR(args)); args = CDR(args);
    if (igcase == NA_LOGICAL)
	error(_("invalid '%s' argument"), "ignore.case");
    bool idirs = asBool2(CAR(args), call); args = CDR(args);
//    if (idirs == NA_LOGICAL)
//	error(_("invalid '%s' argument"), "include.dirs");
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
    R_StringBuffer pb = {NULL, 0, 16};
    RCNTXT cntxt;
    /* set up a context which will free the string buffer if
       there is an error */
    cntxt.cend = &search_cleanup;
    cntxt.cenddata = &pb;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    for (int i = 0; i < LENGTH(d) ; i++) {
	R_DIR *dir;
	size_t len;
	if (search_setup(&pb, STRING_ELT(d, i), &dir, &len, NULL)) {
	    list_files(&pb, fullnames ? 0 : len, len, &count, &ans, allfiles,
		       recursive, pattern ? &reg : NULL, &countmax, idx,
		       idirs, /* allowdots = */ !nodots, dir);
	    R_closedir(dir);
	}
    }
    endcontext(&cntxt);
    search_cleanup(&pb);
    REPROTECT(ans = lengthgets(ans, count), idx);
    if (pattern) tre_regfree(&reg);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

/* see comments in list_files for how the path buffer works */
static void list_dirs(R_StringBuffer *pb, size_t offset, size_t len,
                      int *count, SEXP *pans, int *countmax,
                      PROTECT_INDEX idx, bool recursive, R_DIR *dir)
{
    struct R_dirent *de;
    R_CheckUserInterrupt(); // includes stack check

    while ((de = R_readdir(dir))) {
	/* append current name and null terminate */
	size_t newlen = path_buffer_append(pb, de->d_name, len);
	if (R_IsDirPath(pb->data)) {
	    if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) {
		add_to_ans(pans, pb->data + offset, count, countmax, idx);
		R_DIR *newdir;
		if (recursive && ((newdir = R_opendir(pb->data)) != NULL)) {
		    /* turn terminator into file separator */
		    pb->data[newlen - 1] = FILESEP[0];
		    list_dirs(pb, offset, newlen, count, pans, countmax,
			      idx, recursive, newdir);
		    /* FIXME: arrange to close on error */
		    R_closedir(newdir);
		}
	    }
	}
    }
}

attribute_hidden SEXP do_listdirs(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int countmax = 128;

    checkArity(op, args);
    SEXP d = CAR(args); args = CDR(args);
    if (!isString(d)) error(_("invalid '%s' argument"), "directory");
    int fullnames = asLogical(CAR(args)); args = CDR(args);
    if (fullnames == NA_LOGICAL)
	error(_("invalid '%s' argument"), "full.names");
    bool recursive = asBool2(CAR(args), call); args = CDR(args);
//    if (recursive == NA_LOGICAL)
//	error(_("invalid '%s' argument"), "recursive");

    PROTECT_INDEX idx;
    SEXP ans;
    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, countmax), &idx);
    int count = 0;
    R_StringBuffer pb = {NULL, 0, 16};
    RCNTXT cntxt;
    /* set up a context which will free the string buffer if
       there is an error */
    cntxt.cend = &search_cleanup;
    cntxt.cenddata = &pb;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    for (int i = 0; i < LENGTH(d) ; i++) {
	bool added_separator = FALSE;
	R_DIR *dir;
	size_t len;
	if (!search_setup(&pb, STRING_ELT(d, i), &dir, &len,
	                      &added_separator))
	    continue;

	/* Historically list.dirs(recursive = TRUE) returned also the initial
	   directory with full.names == TRUE and "" with full.names = FALSE.
	   list.files(recursive = TRUE, include.dirs = TRUE) does not do
	   that.

	   This block mimicks the previous behavior but could be removed when
	   that is no longer needed (from here and search_setup). */
	if (recursive) {
	    if (!fullnames) {
		add_to_ans(&ans, "", &count, &countmax, idx);
	    } else {
		char *dnp = R_alloc(len + 1, 1);
		if (len)
		    memcpy(dnp, pb.data, len);
		/* remove trailing separator if added by search_setup */
		if (added_separator)
		    dnp[len - 1] = '\0';
		else
		    dnp[len] = '\0';
		add_to_ans(&ans, dnp, &count, &countmax, idx);
	    }
	}
	list_dirs(&pb, fullnames ? 0 : len, len, &count, &ans,
	          &countmax, idx, recursive, dir);
	R_closedir(dir);
    }
    endcontext(&cntxt);
    search_cleanup(&pb);
    REPROTECT(ans = lengthgets(ans, count), idx);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

attribute_hidden SEXP do_Rhome(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *path;
    checkArity(op, args);
    if (!(path = R_HomeDir()))
	error(_("unable to determine R home location"));
    return mkString(path);
}

#ifdef Win32
static /*attribute_hidden*/ bool R_WFileExists(const wchar_t *path)
{
    struct _stati64 sb;
    return _wstati64(path, &sb) == 0;
}
#endif

attribute_hidden SEXP do_fileexists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, ans;
    int i, nfile;
    checkArity(op, args);
    if (!isString(file = CAR(args)))
	error(_("invalid '%s' argument"), "file");
    nfile = LENGTH(file);
    ans = PROTECT(allocVector(LGLSXP, nfile));
    for (i = 0; i < nfile; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(file, i) != NA_STRING) {
	    /* documented to silently report false for paths that would be too
	       long after expansion */
#ifdef Win32
	    /* Package XML sends arbitrarily long strings to file.exists! */
	    size_t len = strlen(CHAR(STRING_ELT(file, i)));
	    /* 32767 bytes will still fit to the wide char buffer used
	       by filenameToWchar */
	    if (len > 32767)
		LOGICAL(ans)[i] = FALSE;
	    else
		LOGICAL(ans)[i] =
		    R_WFileExists(filenameToWchar(STRING_ELT(file, i), TRUE));
#else
	    // returns NULL if not translatable
	    const char *p = translateCharFP2(STRING_ELT(file, i));
	    /* Package XML sends arbitrarily long strings to file.exists! */
	    if (!p || strlen(p) > R_PATH_MAX)
		LOGICAL(ans)[i] = FALSE;
	    else
		LOGICAL(ans)[i] = R_FileExists(p);
#endif
	} else LOGICAL(ans)[i] = FALSE;
    }
    UNPROTECT(1); /* ans */
    return ans;
}

#define CHOOSEBUFSIZE 1024

#ifndef Win32
attribute_hidden SEXP do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
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
attribute_hidden SEXP do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n, mode, modemask;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "names");
    n = LENGTH(fn);
    mode = asInteger(CADR(args));
    if (mode < 0 || mode > 7) error(_("invalid '%s' argument"), "mode");
    modemask = 0;
    if (mode & 1) modemask |= X_OK;
    if (mode & 2) modemask |= W_OK;
    if (mode & 4) modemask |= R_OK;
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++)
	if (STRING_ELT(fn, i) != NA_STRING) {
#ifdef Win32
	    INTEGER(ans)[i] =
		winAccessW(filenameToWchar(STRING_ELT(fn, i), TRUE), modemask);
#else
	    const char *p = translateCharFP2(STRING_ELT(fn, i));
	    INTEGER(ans)[i] = p ? access(R_ExpandFileName(p), modemask): -1;
#endif
	} else INTEGER(ans)[i] = -1; /* treat NA as non-existent file */
    UNPROTECT(1);
    return ans;
}

#ifdef Win32

static int R_rmdir(const wchar_t *dir)
{
    return _wrmdir(dir);
}

/* Junctions and symbolic links are fundamentally reparse points, so
   apparently this is the way to detect them. */
static int isReparsePoint(const wchar_t *name)
{
    DWORD res = GetFileAttributesW(name);
    if(res == INVALID_FILE_ATTRIBUTES)
	/* Do not warn, because this function is also used for files that don't
	   exist. R_WFileExists may return false for broken symbolic links. */
	return 0;
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

/* returns FALSE on error */
static bool R_WIsDirPath(const wchar_t *path)
{
    struct _stati64 sb;
    if (!_wstati64(path, &sb) && (sb.st_mode & S_IFDIR))
	return true;
    else
	return FALSE;
}

static int R_unlink(const wchar_t *name, int recursive, int force)
{
    R_CheckStack(); // called recursively
    if (wcscmp(name, L".") == 0 || wcscmp(name, L"..") == 0) return 0;
    /* We cannot use R_WFileExists here since it is false for broken
       symbolic links
       if (!R_WFileExists(name)) return 0; */
    int name_exists = (GetFileAttributesW(name) != INVALID_FILE_ATTRIBUTES);
    if (name_exists && force)
	_wchmod(name, _S_IWRITE);
    if (name_exists && isReparsePoint(name))
	return delReparsePoint(name);

    if (name_exists && recursive) {
	R_WDIR *dir;
	struct R_wdirent *de;
	wchar_t *p;
	int n, ans = 0;
	int is_drive = (name[0] != '\0' && name[1] == L':' &&
	                name[2] == L'\0');

	/* We need to test for a junction first, as junctions
	   are detected as directories. */
	if (R_WIsDirPath(name) || is_drive) {
	    if ((dir = R_wopendir(name)) != NULL) {
		while ((de = R_wreaddir(dir))) {
		    if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
			continue;
		    /* On Windows we need to worry about trailing seps */
		    n = wcslen(name);
		    const void *vmax = vmaxget();
		    p = (wchar_t *)R_alloc(n + 1 + wcslen(de->d_name) + 1,
		                           sizeof(wchar_t));
		    if (is_drive || name[n] == L'/' || name[n] == L'\\') {
			wcscpy(p, name); wcscat(p, de->d_name);
		    } else {
			wcscpy(p, name); wcscat(p, L"/"); wcscat(p, de->d_name);
		    }
		    if (isReparsePoint(name)) ans += delReparsePoint(name);
		    else if (R_WIsDirPath(p)) {
			if (force) _wchmod(p, _S_IWRITE);
			ans += R_unlink(p, recursive, force);
		    } else {
			if (force) _wchmod(p, _S_IWRITE);
			ans += (_wunlink(p) == 0) ? 0 : 1;
		    }
		    vmaxset(vmax);
		}
		R_wclosedir(dir);
	    } else { /* we were unable to read a dir */
		ans++;
	    }
	    ans += (R_rmdir(name) == 0) ? 0 : 1;
	    return ans;
	}
	/* drop through */
    }

    int unlink_succeeded = (_wunlink(name) == 0);
    /* We want to return 0 if either unlink succeeded or 'name' did not exist */
    return (unlink_succeeded || !name_exists) ? 0 : 1;
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
    res = lstat(name, &sb);  /* better to be lstat */
    if (!res && force) chmod(name, sb.st_mode | S_IWUSR);

    if (!res && recursive) {
	DIR *dir;
	struct dirent *de;
	char p[R_PATH_MAX];
	int ans = 0;

	if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	    if ((dir = opendir(name)) != NULL) {
		while ((de = readdir(dir))) {
		    if (streql(de->d_name, ".") || streql(de->d_name, ".."))
			continue;
		    size_t n = strlen(name);
		    int pres;
		    if (name[n] == R_FileSep[0])
			/* FIXME: do we have to test on Unix? cf filename_buf */
			pres = snprintf(p, R_PATH_MAX, "%s%s", name, de->d_name);
		    else
			pres = snprintf(p, R_PATH_MAX, "%s%s%s", name, R_FileSep,
				 de->d_name);
		    if (pres >= R_PATH_MAX)
			error(_("path too long"));
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

void R_CleanTempDir(void)
{
    char buf[R_PATH_MAX + 20];

    if((Sys_TempDir)) {
// Only __sun is neeed on Solaris >= 10 (2005).
#if defined(__sun) || defined(sun)
	/* On Solaris the working directory must be outside this one */
	chdir(R_HomeDir());
#endif
	char *special = "'\\`$\"\n";
	int hasspecial = 0;
	for(int i = 0; special[i] != '\0'; i++)
	    if (strchr(Sys_TempDir, special[i])) {
		hasspecial = 1;
		break;
	    }
	if (!hasspecial) {
	    /* rm -Rf may be optimized for specific file-systems.
	       Some file systems allow to delete directory trees without
	       explicit/synchronous traversal so that deletion appears to be
	       very fast (see e.g. zfs). */

	    /* might contain space */
	    snprintf(buf, sizeof(buf), "rm -Rf '%s'", Sys_TempDir);
	    buf[sizeof(buf)-1] = '\0';
	    R_system(buf);
	} else
	    R_unlink(Sys_TempDir, 1, 1); /* recursive=TRUE, force=TRUE */
    }
}
#endif

/* Note that wildcards are allowed in 'names' */
#ifdef Win32
# include <dos_wglob.h>
attribute_hidden SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, j, nfiles, res, failures = 0, recursive, force, expand;
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
	expand = asLogical(CADDDR(args));
	if (expand == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "expand");
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		/* FIXME: does not convert encodings, currently matching
		          filenameToWchar */
		if (streql(CHAR(STRING_ELT(fn, i)),"~"))
		    continue;
		names = filenameToWchar(STRING_ELT(fn, i), expand ? TRUE : FALSE);
		if (expand) {
		    res = dos_wglob(names, GLOB_NOCHECK, NULL, &globbuf);
		    if (res == GLOB_NOSPACE)
			error(_("internal out-of-memory condition"));
		    for (j = 0; j < globbuf.gl_pathc; j++)
			failures += R_unlink(globbuf.gl_pathv[j], recursive,
			                     force);
		    dos_wglobfree(&globbuf);
		} else {
		    failures += R_unlink(names, recursive, force);
		}
	    } else failures++;
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#else
# if defined(HAVE_GLOB) && defined(HAVE_GLOB_H)
#  include <glob.h>
# endif

attribute_hidden SEXP do_unlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  fn;
    int i, nfiles, failures = 0, recursive, force, expand;
    bool useglob = FALSE;
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
	expand = asLogical(CADDDR(args));
	if (expand == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "expand");
#if defined(HAVE_GLOB)
	if (expand)
	    useglob = true;
#endif
	for (i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		names = translateChar(STRING_ELT(fn, i));
		if (streql(names, "~"))
		    continue;
		if (expand)
		    names = R_ExpandFileName(names);
		if (useglob) {
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
			failures += R_unlink(globbuf.gl_pathv[j], recursive,
			                     force);
		    globfree(&globbuf);
#endif
		} else
		    failures += R_unlink(names, recursive, force);
	    } else failures++;
	}
    }
    return ScalarInteger(failures ? 1 : 0);
}
#endif

attribute_hidden SEXP do_getlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
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
#ifndef Win32
# ifdef LC_MESSAGES
    case 7: cat = LC_MESSAGES; break;
# endif
# ifdef LC_PAPER
    case 8: cat = LC_PAPER; break;
# endif
# ifdef LC_MEASUREMENT
    case 9: cat = LC_MEASUREMENT; break;
# endif
#endif
    default: cat = NA_INTEGER;
    }
    if (cat != NA_INTEGER) p = setlocale(cat, NULL);
    return mkString(p ? p : "");
}

/* Locale specs are always ASCII */
attribute_hidden SEXP do_setlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP locale = CADR(args), ans;
    int cat;
    const char *p;
    bool warned = FALSE;

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
	    /* disable the collator when setting to C to take
	       precedence over R_ICU_LOCALE */
	    resetICUcollator(!strcmp(l, "C"));
	    setlocale(LC_MONETARY, l);
	    setlocale(LC_TIME, l);
	    dt_invalidate_locale();
	    /* Need to return value of LC_ALL */
	    p = setlocale(cat, NULL);
	}
	break;
    }
    case 2:
    {
	const char *l = CHAR(STRING_ELT(locale, 0));
	cat = LC_COLLATE;
	p = setlocale(cat, l);
	/* disable the collator when setting to C to take
	   precedence over R_ICU_LOCALE */
	resetICUcollator(!strcmp(l, "C"));
	break;
    }
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
#ifdef Win32
    case 7: /* LC_MESSAGES (defined in gnuintl.h only for (d)gettext) */
    case 8: /* LC_PAPER */
    case 9: /* LC_MEASUREMENT */
	warning(_("%s does not exist on Windows"),
	    (cat == 7) ? "LC_MESSAGES" :
	    (cat == 8) ? "LC_PAPER"    :
	                 "LC_MEASUREMENT");
	p = NULL;
	warned = true;
	break;
#else /* not Win32 */
# ifdef LC_MESSAGES
    case 7:
	cat = LC_MESSAGES;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
# endif
# ifdef LC_PAPER
    case 8:
	cat = LC_PAPER;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
# endif
# ifdef	LC_MEASUREMENT
    case 9:
	cat = LC_MEASUREMENT;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
# endif
#endif
    default:
	p = NULL; /* -Wall */
	error(_("invalid '%s' argument"), "category");
    }
    PROTECT(ans = allocVector(STRSXP, 1));
    if (p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  {
	SET_STRING_ELT(ans, 0, mkChar(""));
	if (!warned)
	    warning(_("OS reports request to set locale to \"%s\" cannot be honored"),
	            CHAR(STRING_ELT(locale, 0)));
    }
#ifdef Win32
    int oldCP = localeCP;
#endif
    R_check_locale();
#ifdef Win32
    if (localeCP && systemCP != localeCP && oldCP != localeCP) {
	/* For now, don't warn for localeCP == 0, but it can cause problems
	   as well. Keep in step with main.c. */
	warning(_("using locale code page other than %d%s may cause problems"),
	    systemCP, systemCP == 65001 ? " (\"UTF-8\")" : "");
    }
#endif
    invalidate_cached_recodings();
    UNPROTECT(1);
    return ans;
}



attribute_hidden SEXP do_localeconv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    struct lconv *lc = localeconv();
    int i = 0;
    char buff[20];

    checkArity(op, args);
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
    snprintf(buff, 20, "%d", (int)lc->int_frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("int_frac_digits"));
    snprintf(buff, 20, "%d", (int)lc->frac_digits);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("frac_digits"));
    snprintf(buff, 20, "%d", (int)lc->p_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_cs_precedes"));
    snprintf(buff, 20, "%d", (int)lc->p_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sep_by_space"));
    snprintf(buff, 20, "%d", (int)lc->n_cs_precedes);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_cs_precedes"));
    snprintf(buff, 20, "%d", (int)lc->n_sep_by_space);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sep_by_space"));
    snprintf(buff, 20, "%d", (int)lc->p_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("p_sign_posn"));
    snprintf(buff, 20, "%d", (int)lc->n_sign_posn);
    SET_STRING_ELT(ans, i, mkChar(buff));
    SET_STRING_ELT(ansnames, i++, mkChar("n_sign_posn"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

/* .Internal function for path.expand */
attribute_hidden SEXP do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
	error(_("invalid '%s' argument"), "path");
    n = LENGTH(fn);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	SEXP tmp = STRING_ELT(fn, i);
	if (tmp != NA_STRING) {
#ifdef Win32
	    /* Windows can have files and home directories that aren't
	       representable in the native encoding (e.g. latin1). Translate
	       to UTF-8 when the input is in UTF-8 already or is in latin1,
	       but the native encoding is not latin1.

	       R (including R_ExpandFileNameUTF8) for now only supports R home
	       directories representable in native encoding.
	    */
	    if (IS_UTF8(tmp) || (IS_LATIN1(tmp) && !latin1locale))
		tmp = mkCharCE(R_ExpandFileNameUTF8(trCharUTF8(tmp)), CE_UTF8);
	    else
#endif
	    {
		const char *p = translateCharFP2(tmp);
		if (p)
		    tmp = markKnown(R_ExpandFileName(p), tmp);
	    }
	}
	SET_STRING_ELT(ans, i, tmp);
    }
    UNPROTECT(1);
    return ans;
}

#ifdef Unix
static Rboolean var_R_can_use_X11 = -1;

extern Rboolean R_access_X11(void); /* from src/unix/X11.c */

static bool R_can_use_X11(void)
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
attribute_hidden SEXP do_capabilitiesX11(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifdef Unix
    return ScalarLogical(R_can_use_X11());
#else
    return ScalarLogical(FALSE);
#endif
}

attribute_hidden SEXP do_capabilities(SEXP call, SEXP op, SEXP args, SEXP rho)
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

    PROTECT(ans      = allocVector(LGLSXP, 19));
    PROTECT(ansnames = allocVector(STRSXP, 19));

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
    LOGICAL(ans)[i++] = TRUE;

    SET_STRING_ELT(ansnames, i, mkChar("sockets"));
    LOGICAL(ans)[i++] = TRUE;

    SET_STRING_ELT(ansnames, i, mkChar("libxml"));
    LOGICAL(ans)[i++] = FALSE;

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
#if defined(HAVE_LIBREADLINE)
	extern Rboolean UsingReadline; // from ../unix/system.c
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

    SET_STRING_ELT(ansnames, i, mkChar("Rprof"));
#ifdef R_PROFILING
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
    char path[1000]; // R_HomeDir() should be at most 260 chars
# ifdef R_ARCH
    snprintf(path, 1000, "%s/library/grDevices/libs/%s/winCairo.dll",
	     R_HomeDir(), R_ARCH);
# else
    snprintf(path, 1000, "%s/library/grDevices/libs/winCairo.dll",
	     R_HomeDir());
# endif
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

    SET_STRING_ELT(ansnames, i, mkChar("libcurl"));
#ifdef HAVE_LIBCURL
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_sysgetpid(SEXP call, SEXP op, SEXP args, SEXP rho)
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
attribute_hidden SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP path;
    int res, show, recursive, mode, serrno = 0;
    char *p, dir[R_PATH_MAX];

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || LENGTH(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    mode = asInteger(CADDDR(args));
    if (mode == NA_LOGICAL) mode = 0777;
    strcpy(dir, R_ExpandFileName(translateCharFP(STRING_ELT(path, 0))));
    if (strlen(dir) == 0) error(_("zero-length 'path' argument"));
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
attribute_hidden SEXP do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path;
    wchar_t *p, *dir;
    int res, show, recursive, serrno = 0, maybeshare;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || LENGTH(path) != 1)
	error(_("invalid '%s' argument"), "path");
    if (STRING_ELT(path, 0) == NA_STRING) return ScalarLogical(FALSE);
    show = asLogical(CADR(args));
    if (show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if (recursive == NA_LOGICAL) recursive = 0;
    p = filenameToWchar(STRING_ELT(path, 0), TRUE);
    if (wcslen(p) == 0) error(_("zero-length 'path' argument"));
    dir = (wchar_t*) R_alloc(wcslen(p) + 1, sizeof(wchar_t));
    wcscpy(dir, p);
    R_wfixbackslash(dir);
    /* remove trailing slashes */
    p = dir + wcslen(dir) - 1;
    while (*p == L'\\' && wcslen(dir) > 1 && *(p-1) != L':') *p-- = L'\0';
    if (recursive) {
	p = dir;
	maybeshare = 0;
	/* skip leading \\server\\share, \\share */
	/* FIXME: is \\share (still) possible? */
	if (*p == L'\\' && *(p+1) == L'\\') {
	    p += 2;
	    p = wcschr(p, L'\\');
	    maybeshare = 1; /* the next element may be a share name */
	}
	while ((p = wcschr(p+1, L'\\'))) {
	    *p = L'\0';
	    if (*(p-1) != L':') {
		res = _wmkdir(dir);
		serrno = errno;
		if (res && serrno != EEXIST && !maybeshare) goto end;
	    }
	    maybeshare = 0;
	    *p = L'\\';
	}
    }
    res = _wmkdir(dir);
    serrno = errno;
    if (show && res) {
    	if (serrno == EEXIST)
	    warning(_("'%ls' already exists"), dir);
        else
            warning(_("cannot create dir '%ls', reason '%s'"), dir,
            	    strerror(serrno));
    }
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
    int res  = GetFileTime(hFrom, NULL, NULL, &modft);
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
    const void *vmax = vmaxget();
    struct _stati64 sb;
    int nfail = 0, res;
    wchar_t *dest, *this;

    this = (wchar_t *) R_alloc(wcslen(from) + wcslen(name) + 1, sizeof(wchar_t));
    wcscpy(this, from);
    wcscat(this, name);
    _wstati64(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	R_WDIR *dir;
	struct R_wdirent *de;
	wchar_t *p;

	if (!recursive) {
	    vmaxset(vmax);
	    return 1;
	}
	dest = (wchar_t *) R_alloc(wcslen(to) + wcslen(name) + 1,
	                           sizeof(wchar_t));
	wcscpy(dest, to);
	wcscat(dest, name);
	/* We could set the mode (only the 200 part matters) later */
	res = _wmkdir(dest);
	if (res) {
	    if (errno == EEXIST) {
		struct _stati64 dsb;
		if (over && _wstati64(dest, &dsb) == 0 &&
		   (dsb.st_mode & S_IFDIR) == 0) {

		    warning(_("cannot overwrite non-directory %ls with directory %ls"),
		            dest, this);
		    vmaxset(vmax);
		    return 1;
		}
	    } else {
		warning(_("problem creating directory %ls: %s"),
		          dest, strerror(errno));
		vmaxset(vmax);
		return 1;
	    }
	}
	// NB Windows' mkdir appears to require \ not /.
	if ((dir = R_wopendir(this)) != NULL) {
	    depth++;
	    while ((de = R_wreaddir(dir))) {
		if (!wcscmp(de->d_name, L".") || !wcscmp(de->d_name, L".."))
		    continue;
		p = (wchar_t *) R_alloc(wcslen(name) + 1 + wcslen(de->d_name) + 1,
		                        sizeof(wchar_t));
		wcscpy(p, name);
		wcscat(p, L"\\");
		wcscat(p, de->d_name);
		nfail += do_copy(from, p, to, over, recursive,
				 perms, dates, depth);
	    }
	    R_wclosedir(dir);
	} else {
	    warning(_("problem reading directory %ls: %s"), this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
	// chmod(dest, ... perms ...)  [TODO?]
	if(dates) copyFileTime(this, dest);
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;

	nfail = 0;
	int nc;
	dest = (wchar_t *) R_alloc(wcslen(to) + wcslen(name) + 1,
	                           sizeof(wchar_t));
	wcscpy(dest, to);
	wcscat(dest, name);
	if (over || !R_WFileExists(dest)) { /* FIXME */
	    if ((fp1 = _wfopen(this, L"rb")) == NULL ||
		(fp2 = _wfopen(dest, L"wb")) == NULL) {
		warning(_("problem copying %ls to %ls: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    wchar_t *buf = (wchar_t *)malloc(APPENDBUFSIZE * sizeof(wchar_t));
	    if (!buf) {
		fclose(fp1);
		fclose(fp2);
		error("could not allocate copy buffer");
	    }
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (    fwrite(buf, 1, APPENDBUFSIZE, fp2)  != APPENDBUFSIZE) {
		    nfail++;
		    free(buf);
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		free(buf);
		goto copy_error;
	    }
	    free(buf);
	} else if (!over) {
	    nfail++;
	    goto copy_error;
	}
	if(fp1) { fclose(fp1); fp1 = NULL; }
	if(fp2) { fclose(fp2); fp2 = NULL; }
	/* FIXME: perhaps manipulate mode as we do in Sys.chmod? */
	if(perms) _wchmod(dest, sb.st_mode & 0777);
	if(dates) copyFileTime(this, dest);
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    vmaxset(vmax);
    return nfail;
}

/* file.copy(from, to, overwrite, recursive, copy.mode, copy.date)
 * --------- Windows */
attribute_hidden SEXP do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP fn = CAR(args);
    int nfiles = length(fn);
    SEXP ans = PROTECT(allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	args = CDR(args);
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	SEXP to = CAR(args); args = CDR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	int over = asLogical(CAR(args)); args = CDR(args);
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "overwrite");
	int recursive = asLogical(CAR(args)); args = CDR(args);
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	int perms = asLogical(CAR(args)); args = CDR(args);
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	int dates = asLogical(CAR(args));
	if (dates == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.date");
	wchar_t *p = filenameToWchar(STRING_ELT(to, 0), TRUE);
	wchar_t *dir = (wchar_t *) R_alloc(wcslen(p) + 2, sizeof(wchar_t));
	wcscpy(dir, p);
	if (*(dir + (wcslen(dir) - 1)) !=  L'\\')
	    wcscat(dir, L"\\");
	int nfail;
	for (int i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
	    	p = filenameToWchar(STRING_ELT(fn, i), TRUE);
		/* the +2 defensively for the .\\ copied below */
		wchar_t *from;
		from = (wchar_t *) R_alloc(wcslen(p) + 1 + 2, sizeof(wchar_t));
		wcscpy(from, p);
		size_t ll = wcslen(from);
		if (ll) {  // people do pass ""
		    /* If there is a trailing sep, this is a mistake */
		    p = from + (ll - 1);
		    if(*p == L'\\') *p = L'\0';
		    p = wcsrchr(from, L'\\') ;
		    wchar_t *name = (wchar_t *) R_alloc(ll + 1,
		                                        sizeof(wchar_t));;
		    /* move the last element of "from" into "name" */
		    if (p) {
			wcscpy(name, p+1);
			*(p+1) = L'\0';
		    } else {
			if(wcslen(from) > 2 && from[1] == L':') {
			    wcscpy(name, from+2);
			    from[2] = L'\0';
			} else {
			    wcscpy(name, from);
			    wcscpy(from, L".\\");
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

#if defined(HAVE_UTIMENSAT)
# include <fcntl.h>
# include <sys/stat.h>
#elif defined(HAVE_UTIMES)
# include <sys/time.h>
#elif defined(HAVE_UTIME)
# include <utime.h>
#endif

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

#if defined(HAVE_UTIMENSAT)
    struct timespec times[2];

    times[0].tv_sec  = times[1].tv_sec  = (int)ftime;
    times[0].tv_nsec = times[1].tv_nsec = (int)(1e9*(ftime - (int)ftime));
    utimensat(AT_FDCWD, to, times, 0);
#elif defined(HAVE_UTIMES)
    struct timeval times[2];

    times[0].tv_sec  = times[1].tv_sec  = (int)ftime;
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
    int nfail = 0, res; // we only use nfail == 0
    size_t len;
    // After POSIX clarification, PATH_MAX would do (?)
    char dest[R_PATH_MAX + 1], this[R_PATH_MAX + 1];

    int mask;
#ifdef HAVE_UMASK
    int um = umask(0); umask((mode_t) um);
    mask = 0777 & ~um;
#else
    mask = 0777;
#endif
    /* REprintf("from: %s, name: %s, to: %s\n", from, name, to); */
    // We use snprintf to compute lengths to pacify GCC 12
    len = snprintf(NULL, 0, "%s%s", from, name);
    if (len >= R_PATH_MAX) {
	warning(_("over-long path"));
	return 1;
    }
    snprintf(this, len+1, "%s%s", from, name);
    /* Here we want the target not the link */
    stat(this, &sb);
    if ((sb.st_mode & S_IFDIR) > 0) { /* a directory */
	DIR *dir;
	struct dirent *de;
	char p[R_PATH_MAX + 1];

	if (!recursive) return 1;
	len = snprintf(NULL, 0, "%s%s", to, name);
	if (len >= R_PATH_MAX) {
	    warning(_("over-long path"));
	    return 1;
	}
	snprintf(dest, len+1, "%s%s", to, name);
	/* If a directory does not have write permission for the user,
	   we will fail to create files in that directory, so defer
	   setting mode */
	res = mkdir(dest, 0700);
	if (res) {
	    if (errno == EEXIST) {
		struct stat dsb;
		if (over && stat(dest, &dsb) == 0 &&
		   (dsb.st_mode & S_IFDIR) == 0) {

		    warning(_("cannot overwrite non-directory %s with directory %s"),
		            dest, this);
		    return 1;
		}
	    } else {
		warning(_("problem creating directory %s: %s"),
		        this, strerror(errno));
		return 1;
	    }
	}
	strcat(dest, "/");
	if ((dir = opendir(this)) != NULL) {
	    depth++;
	    while ((de = readdir(dir))) {
		if (streql(de->d_name, ".") || streql(de->d_name, ".."))
		    continue;
		len = snprintf(NULL, 0, "%s/%s", name, de->d_name);
		if (len >= R_PATH_MAX) {
		    warning(_("over-long path"));
		    closedir(dir);
		    return 1;
		}
		snprintf(p, len+1, "%s/%s", name, de->d_name);
		nfail += do_copy(from, p, to, over, recursive,
				 perms, dates, depth);
	    }
	    closedir(dir);
	} else {
	    warning(_("problem reading directory %s: %s"), this, strerror(errno));
	    nfail++; /* we were unable to read a dir */
	}
	chmod(dest, (mode_t) (perms ? (sb.st_mode & mask): mask));
	if(dates) copyFileTime(this, dest);
    } else { /* a file */
	FILE *fp1 = NULL, *fp2 = NULL;

	nfail = 0;
	len = snprintf(NULL, 0, "%s%s", to, name);
	if (len >= R_PATH_MAX) {
	    warning(_("over-long path"));
	    nfail++;
	    goto copy_error;
	}
	snprintf(dest, len+1, "%s%s", to, name);
	if (over || !R_FileExists(dest)) {
	    /* REprintf("copying %s to %s\n", this, dest); */
	    if ((fp1 = R_fopen(this, "rb")) == NULL ||
		(fp2 = R_fopen(dest, "wb")) == NULL) {
		warning(_("problem copying %s to %s: %s"),
			this, dest, strerror(errno));
		nfail++;
		goto copy_error;
	    }
	    char *buf = (char *)malloc(APPENDBUFSIZE);
	    if (!buf) {
		fclose(fp1);
		fclose(fp2);
		error("could not allocate copy buffer");
	    }
	    size_t nc;
	    while ((nc = fread(buf, 1, APPENDBUFSIZE, fp1)) == APPENDBUFSIZE)
		if (    fwrite(buf, 1, APPENDBUFSIZE, fp2)  != APPENDBUFSIZE) {
		    nfail++;
		    free(buf);
		    goto copy_error;
		}
	    if (fwrite(buf, 1, nc, fp2) != nc) {
		nfail++;
		free(buf);
		goto copy_error;
	    }
	    free(buf);
	} else if (!over) {
	    nfail++;
	    goto copy_error;
	}
	if(fp1) { fclose(fp1); fp1 = NULL; }
	if(fp2) { fclose(fp2); fp2 = NULL; }
	if(perms) chmod(dest, sb.st_mode & mask);
	if(dates) copyFileTime(this, dest);
copy_error:
	if(fp2) fclose(fp2);
	if(fp1) fclose(fp1);
    }
    return nfail;
}

/* file.copy(from, to, overwrite, recursive, copy.mode, copy.date)
 * --------- Unix-alike */
attribute_hidden SEXP do_filecopy(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP fn = CAR(args);
    int nfiles = length(fn);
    SEXP ans = PROTECT(allocVector(LGLSXP, nfiles));
    if (nfiles > 0) {
	args = CDR(args);
	if (!isString(fn))
	    error(_("invalid '%s' argument"), "from");
	SEXP to = CAR(args); args = CDR(args);
	if (!isString(to) || LENGTH(to) != 1)
	    error(_("invalid '%s' argument"), "to");
	int over = asLogical(CAR(args)); args = CDR(args);
	if (over == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "overwrite");
	int recursive = asLogical(CAR(args)); args = CDR(args);
	if (recursive == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "recursive");
	int perms = asLogical(CAR(args)); args = CDR(args);
	if (perms == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.mode");
	int dates = asLogical(CAR(args));
	if (dates == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "copy.date");
	const char* q = R_ExpandFileName(translateCharFP(STRING_ELT(to, 0)));
	if(strlen(q) > R_PATH_MAX - 2) // allow for '/' and terminator
	    error(_("invalid '%s' argument"), "to");
	char dir[R_PATH_MAX];
	// gcc 10 with sanitizers objects to R_PATH_MAX here.
	strncpy(dir, q, R_PATH_MAX - 1);
	dir[R_PATH_MAX - 1] = '\0';
	if (*(dir + (strlen(dir) - 1)) !=  '/')
	    strcat(dir, "/");
	int nfail;
	for (int i = 0; i < nfiles; i++) {
	    if (STRING_ELT(fn, i) != NA_STRING) {
		char from[R_PATH_MAX];
		strncpy(from,
			R_ExpandFileName(translateCharFP(STRING_ELT(fn, i))),
			R_PATH_MAX - 1);
		from[R_PATH_MAX - 1] = '\0';
		size_t ll = strlen(from);
		if (ll) {  // people do pass ""
		    /* If there is a trailing sep, this is a mistake */
		    char* p = from + (ll - 1);
		    if(*p == '/') *p = '\0';
		    p = strrchr(from, '/') ;
		    char name[R_PATH_MAX];
		    if (p) {
			strncpy(name, p+1, R_PATH_MAX - 1);
			name[R_PATH_MAX - 1] = '\0';
			*(p+1) = '\0';
		    } else {
			strncpy(name, from, R_PATH_MAX);
			name[R_PATH_MAX - 1] = '\0';
			strncpy(from, "./", R_PATH_MAX);
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

attribute_hidden SEXP do_l10n_info(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef Win32
    int len = 5;
#else
    int len = 4;
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
#ifndef Win32
    SET_STRING_ELT(names, 3, mkChar("codeset"));
    SET_VECTOR_ELT(ans, 3, mkString(codeset));
#endif
#ifdef Win32
    SET_STRING_ELT(names, 3, mkChar("codepage"));
    SET_VECTOR_ELT(ans, 3, ScalarInteger(localeCP));
    SET_STRING_ELT(names, 4, mkChar("system.codepage"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(systemCP));
#endif
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return ans;
}

/* do_normalizepath moved to util.c in R 2.13.0 */

attribute_hidden SEXP do_syschmod(SEXP call, SEXP op, SEXP args, SEXP env)
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
	    res = chmod(R_ExpandFileName(translateCharFP(STRING_ELT(paths, i))),
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

attribute_hidden SEXP do_sysumask(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    int mode;
    mode_t res = 0;
    bool visible;

    checkArity(op, args);
    mode = asInteger(CAR(args));
#ifdef HAVE_UMASK
    if (mode == NA_INTEGER) {
	res = umask(0);
	umask(res);
	visible = true;
    } else {
	res = umask((mode_t) mode);
	visible = FALSE;
    }
#else
    warning(_("insufficient OS support on this platform"));
    visible = FALSE;
#endif
    PROTECT(ans = ScalarInteger(res));
    setAttrib(ans, R_ClassSymbol, mkString("octmode"));
    UNPROTECT(1);
    R_Visible = visible;
    return ans;
}

attribute_hidden SEXP do_readlink(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP paths = CAR(args);
    if(!isString(paths))
	error(_("invalid '%s' argument"), "paths");
    int n = LENGTH(paths);
    SEXP ans = PROTECT(allocVector(STRSXP, n));
#ifdef HAVE_READLINK
    char buf[R_PATH_MAX+1];
    for (int i = 0; i < n; i++) {
	const char *p = translateCharFP2(STRING_ELT(paths, i));
	if (p) {
	    memset(buf, 0, R_PATH_MAX+1);
	    ssize_t res = readlink(R_ExpandFileName(p), buf, R_PATH_MAX);
	    if (res == R_PATH_MAX) {
		SET_STRING_ELT(ans, i, mkChar(buf));
		warning("possible truncation of value for element %d", i + 1);
	    } else if (res >= 0) SET_STRING_ELT(ans, i, mkChar(buf));
	    else if (errno == EINVAL) SET_STRING_ELT(ans, i, mkChar(""));
	    else SET_STRING_ELT(ans, i,  NA_STRING);
	} else SET_STRING_ELT(ans, i,  NA_STRING);
    }
#endif
    UNPROTECT(1);
    return ans;
}


attribute_hidden SEXP do_Cstack_info(SEXP call, SEXP op, SEXP args, SEXP rho)
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
static int winSetFileTime(const char *fn, double ftime)
{
    SYSTEMTIME st;
    FILETIME modft;
    struct tm *utctm;
    HANDLE hFile;
    time_t ftimei = (time_t) ftime;

    utctm = gmtime(&ftimei);
    if (!utctm) return 0;

    st.wYear         = (WORD) utctm->tm_year + 1900;
    st.wMonth        = (WORD) utctm->tm_mon + 1;
    st.wDayOfWeek    = (WORD) utctm->tm_wday;
    st.wDay          = (WORD) utctm->tm_mday;
    st.wHour         = (WORD) utctm->tm_hour;
    st.wMinute       = (WORD) utctm->tm_min;
    st.wSecond       = (WORD) utctm->tm_sec;
    st.wMilliseconds = (WORD) 1000*(ftime - ftimei);
    if (!SystemTimeToFileTime(&st, &modft)) return 0;

    hFile = CreateFile(fn, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		       FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return 0;
    int res  = SetFileTime(hFile, NULL, NULL, &modft);
    CloseHandle(hFile);
    return res != 0; /* success is non-zero */
}
#endif

attribute_hidden SEXP
do_setFileTime(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    const char *fn;
    double ftime;
    int res;
    R_xlen_t n, m;
    SEXP paths, times, ans;
    const void *vmax;

    paths = CAR(args);
    if (!isString(paths))
	error(_("invalid '%s' argument"), "path");
    n = XLENGTH(paths);
    PROTECT(times = coerceVector(CADR(args), REALSXP));
    m = XLENGTH(times);
    if (!m && n) error(_("'%s' must be of length at least one"), "time");

    PROTECT(ans = allocVector(LGLSXP, n));
    vmax = vmaxget();
    for(R_xlen_t i = 0; i < n; i++) {
	fn = translateCharFP(STRING_ELT(paths, i));
	ftime = REAL(times)[i % m];
	#ifdef Win32
	    res = winSetFileTime(fn, ftime);
	#elif defined(HAVE_UTIMENSAT)
	    struct timespec times[2];

	    times[0].tv_sec = times[1].tv_sec = (int)ftime;
	    times[0].tv_nsec = times[1].tv_nsec = (int)(1e9*(ftime - (int)ftime));

	    res = utimensat(AT_FDCWD, fn, times, 0) == 0;
	#elif defined(HAVE_UTIMES)
	    struct timeval times[2];

	    times[0].tv_sec = times[1].tv_sec = (int)ftime;
	    times[0].tv_usec = times[1].tv_usec = (int)(1e6*(ftime - (int)ftime));

	    res = utimes(fn, times) == 0;
	#elif defined(HAVE_UTIME)
	    struct utimbuf settime;

	    settime.actime = settime.modtime = (int)ftime;
	    res = utime(fn, &settime) == 0;
	#endif
	LOGICAL(ans)[i] = (res == 0) ? FALSE : TRUE;
	fn = NULL;
	vmaxset(vmax); // throws away result of translateCharFP
    }
    UNPROTECT(2); /* times, ans */
    return ans;
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

attribute_hidden SEXP do_mkjunction(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	return ScalarLogical(0);
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

#include <zlib.h>
#include <bzlib.h>
#include <lzma.h>

#ifdef HAVE_PCRE2
  /* PCRE2_CODE_UNIT_WIDTH is defined to 8 via config.h */
# include<pcre2.h>
#else
# ifdef HAVE_PCRE_PCRE_H
#  include <pcre/pcre.h>
# else
#  include <pcre.h>
# endif
#endif

#ifdef USE_ICU
# ifndef USE_ICU_APPLE
#  include <unicode/uversion.h>
# else
#  define U_MAX_VERSION_LENGTH 4
#  define U_MAX_VERSION_STRING_LENGTH 20
typedef uint8_t UVersionInfo[U_MAX_VERSION_LENGTH];
void u_versionToString(const UVersionInfo versionArray, char *versionString);
void u_getVersion(UVersionInfo versionArray);
# endif
#endif

#include <iconv.h>
#if defined(__GLIBC__)
# include <gnu/libc-version.h>
#endif

#ifdef HAVE_LIBREADLINE
// that ensures we have this header
# include <readline/readline.h>
#endif

#if defined(HAVE_REALPATH) && defined(HAVE_DECL_REALPATH) && !HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif

#ifdef HAVE_DLFCN_H
#include <dlfcn.h> /* for dladdr, dlsym */
#endif

#if defined(HAVE_DLADDR) && defined(HAVE_DECL_DLADDR) && !HAVE_DECL_DLADDR
extern int dladdr(void *addr, Dl_info *info);
#endif

#if defined(HAVE_DLSYM) && defined(HAVE_DECL_DLSYM) && !HAVE_DECL_DLSYM
extern void *dlsym(void *handle, const char *symbol);
#endif

#ifdef HAVE_LIBDEFLATE
# include <libdeflate.h>
#endif

/* extSoftVersion only detects versions of libraries that are available
   without loading any modules; libraries available via modules are
   treated individually (libcurlVersion(), La_version(), etc)
*/
attribute_hidden SEXP
do_eSoftVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(STRSXP, 10));
    SEXP nms = PROTECT(allocVector(STRSXP, 10));
    setAttrib(ans, R_NamesSymbol, nms);
    unsigned int i = 0;
    char p[256];
    snprintf(p, 256, "%s", zlibVersion());
    SET_STRING_ELT(ans, i, mkChar(p));
    SET_STRING_ELT(nms, i++, mkChar("zlib"));
    snprintf(p, 256, "%s", BZ2_bzlibVersion());
    SET_STRING_ELT(ans, i, mkChar(p));
    SET_STRING_ELT(nms, i++, mkChar("bzlib"));
    snprintf(p, 256, "%s", lzma_version_string());
    SET_STRING_ELT(ans, i, mkChar(p));
    SET_STRING_ELT(nms, i++, mkChar("xz"));
#ifdef HAVE_LIBDEFLATE
    snprintf(p, 256, "%s", LIBDEFLATE_VERSION_STRING);
    SET_STRING_ELT(ans, i, mkChar(p));
#else
    SET_STRING_ELT(ans, i, mkChar(""));
#endif
    SET_STRING_ELT(nms, i++, mkChar("libdeflate"));
#ifdef HAVE_PCRE2
    pcre2_config(PCRE2_CONFIG_VERSION, p);
#else
    snprintf(p, 256, "%s", pcre_version());
#endif
    SET_STRING_ELT(ans, i, mkChar(p));
    SET_STRING_ELT(nms, i++, mkChar("PCRE"));
#ifdef USE_ICU
    int use_icu = 1;
#ifdef Win32
    /* ICU 72 requires this function (and other from Windows 7) */
    if (!GetProcAddress(GetModuleHandle(TEXT("kernel32")),
                        "ResolveLocaleName"))
	use_icu = 0;
#endif
    if (use_icu) {
	UVersionInfo icu;
	char pu[U_MAX_VERSION_STRING_LENGTH];
	u_getVersion(icu);
	u_versionToString(icu, pu);
	SET_STRING_ELT(ans, i, mkChar(pu));
    } else
	SET_STRING_ELT(ans, i, mkChar(""));
#else
    SET_STRING_ELT(ans, i, mkChar(""));
#endif
    SET_STRING_ELT(nms, i++, mkChar("ICU"));
    snprintf(p, 256, "%s", tre_version());
    SET_STRING_ELT(ans, i, mkChar(p));
    SET_STRING_ELT(nms, i++, mkChar("TRE"));
#ifdef _LIBICONV_VERSION
    {
	int ver = _libiconv_version;
#ifdef __APPLE__
	// Apple patch to Citrix libiconv reoprts 1.11, but might be external libiconv
	if (ver == 0x010B)
	    snprintf(p, 256, "Apple or GNU libiconv %d.%d", ver/0x0100, ver%0x0100);
	else
	    snprintf(p, 256, "GNU libiconv %d.%d", ver/0x0100, ver%0x0100);
#else
	snprintf(p, 256, "GNU libiconv %d.%d", ver/0x0100, ver%0x0100);
#endif
    }
#elif defined(_WIN32)
    snprintf(p, 256, "%s", "win_iconv");
#elif defined(__GLIBC__)
    snprintf(p, 256, "glibc %s", gnu_get_libc_version());
#else
    snprintf(p, 256, "%s", "unknown");
#endif
#if defined(HAVE_DLADDR) && defined(HAVE_REALPATH) && defined(HAVE_DLSYM) \
    && defined(HAVE_DECL_RTLD_DEFAULT) && HAVE_DECL_RTLD_DEFAULT \
    && defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT && defined(__APPLE__)

    /* Look for function iconv_open and try to figure out in which
       binary/shared library it is defined. See BLAS detection below
       for detailed comments for how this is done, and keep the code
       in sync. This is used on macOS to help identifying when a system
       version of libiconv is used, which can be mapped to a specific
       patch via https://opensource.apple.com/releases/ that cannot be
       differentiated using _libiconv_version (i.e. 1.11 maps to
       different patches with different problems).
    */
    {
	void *addr = dlsym(RTLD_DEFAULT, "iconv_open");
	Dl_info dl_info;
	char buf[R_PATH_MAX+1];
	const char *path = NULL;
	if (addr && dladdr(addr, &dl_info)) {
	    path = realpath(dl_info.dli_fname, buf);
	    if (!path && errno == ENOENT)
		path = dl_info.dli_fname;
	}
	bool ok = FALSE;
	if (path) {
	    size_t len = strlen(p) + strlen(path) + 1 + 1;
	    char *iver = malloc(len);
	    if (iver) {
		snprintf(iver, len, "%s %s", p, path);
		SET_STRING_ELT(ans, i, mkChar(iver));
		free(iver);
		ok = true;
	    }
	}
	if (!ok)
	    SET_STRING_ELT(ans, i, mkChar(p));
    }
#else
    SET_STRING_ELT(ans, i, mkChar(p));
#endif
    SET_STRING_ELT(nms, i++, mkChar("iconv"));
#ifdef HAVE_LIBREADLINE
    /* libedit reports "EditLine wrapper": so we look at
       rl_readline_version, which is currently 0x0402 */
    const char *rl = rl_library_version;
    if (streql(rl, "EditLine wrapper")) {
	int num = rl_readline_version;
	int maj = num / 256, min = num % 256;
	char buf[40];
	snprintf(buf, 40, "%d.%d (%s)", maj, min, rl);
	SET_STRING_ELT(ans, i, mkChar(buf));
    } else
	SET_STRING_ELT(ans, i, mkChar(rl));
#else
    SET_STRING_ELT(ans, i, mkChar(""));
#endif
    SET_STRING_ELT(nms, i++, mkChar("readline"));

    SET_STRING_ELT(ans, i, mkChar(""));

#if defined(HAVE_DLADDR) && defined(HAVE_REALPATH) && defined(HAVE_DLSYM) \
    && defined(HAVE_DECL_RTLD_DEFAULT) && HAVE_DECL_RTLD_DEFAULT \
    && defined(HAVE_DECL_RTLD_NEXT) && HAVE_DECL_RTLD_NEXT

    /* Look for blas function dgemm and try to figure out in which
       binary/shared library it is defined.  That is based on experimentation
       and heuristics, and depends on implementation details
       of dynamic linkers.
    */
#ifdef HAVE_F77_UNDERSCORE
    char *dgemm_name = "dgemm_";
#else
    char *dgemm_name = "dgemm";
#endif

    bool ok = true;

    void *dgemm_addr = dlsym(RTLD_DEFAULT, dgemm_name);

    Dl_info dl_info1, dl_info2;

    /* these calls to dladdr() convert a function pointer to an object
       pointer, which is not allowed by ISO C, but there is no compliant
       alternative to using dladdr() */
#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wpedantic"
#elif defined __GNUC__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wpedantic"	
#endif
    if (!dladdr((void *)do_eSoftVersion, &dl_info1)) ok = FALSE;
    if (!dladdr((void *)dladdr, &dl_info2)) ok = FALSE;
#ifdef __clang__
# pragma clang diagnostic pop
#elif defined __GNUC__
# pragma GCC diagnostic pop
#endif

    if (ok && !strcmp(dl_info1.dli_fname, dl_info2.dli_fname)) {

	/* dladdr is not inside R, hence we probably have the PLT for
	   dynamically linked symbols; lets use dlsym(RTLD_NEXT) to
	   get the real address for dgemm.

	   PLT is used on Linux and on Solaris when the main binary
	   is _not_ position independent. PLT is not used on macOS.
	*/
	if (dgemm_addr != NULL) {

	    /* If dgemm_addr is NULL, dgemm is statically linked and
	       we are on Linux. On Solaris, dgemm_addr is never NULL.
	    */
	    void *dgemm_next_addr = dlsym(RTLD_NEXT, dgemm_name);
	    if (dgemm_next_addr != NULL)

		/* If dgemm_next_addr is NULL, dgemm is statically linked.
		   Otherwise, it is linked dynamically and dgemm_next_addr
		   is its true address (dgemm points to PLT).

		   On Linux, dgemm_next_addr is only NULL here when
		   dgemm is export-dynamic (yet statically linked).
		*/
		dgemm_addr = dgemm_next_addr;
	}
    }

    char buf[R_PATH_MAX+1];
    if (ok && dladdr(dgemm_addr, &dl_info1)) {
	char *res = realpath(dl_info1.dli_fname, buf);
	if (res) {
	    SEXP nfo = R_NilValue;
	    if (strstr(res, "flexiblas"))
		nfo = R_flexiblas_info();
	    if (isNull(nfo))
		nfo = mkChar(res);
	    SET_STRING_ELT(ans, i, nfo);
	} else if (errno == ENOENT)
	    /* macOs (Big Sur) has a cache for system-provided dynamic
	       libraries and they no longer exist as regular files. The 
	       dynamic linker knows how to find them, but not regular file
	       operations such as realpath(). Hence, when the file is not
	       found, report what we have from the dynamic linker. */
	    SET_STRING_ELT(ans, i, mkChar(dl_info1.dli_fname));
    }
#endif
    SET_STRING_ELT(nms, i++, mkChar("BLAS"));

    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP
do_compilerVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(STRSXP, 2));
    SEXP nms = PROTECT(allocVector(STRSXP, 2));
    setAttrib(ans, R_NamesSymbol, nms);
    SET_STRING_ELT(nms, 0, mkChar("C"));
    SET_STRING_ELT(nms, 1, mkChar("Fortran"));
#ifdef CC_VER
    SET_STRING_ELT(ans, 0, mkChar(CC_VER));
#else
    SET_STRING_ELT(ans, 1, mkChar(""));
#endif
#ifdef FC_VER
    SET_STRING_ELT(ans, 1, mkChar(FC_VER));
#else
    SET_STRING_ELT(ans, 1, mkChar(""));
#endif
    UNPROTECT(2);
    return ans;
}


/* platform-specific */
extern void Rsleep(double timeint);

attribute_hidden SEXP do_syssleep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    double time = asReal(CAR(args));
    if (ISNAN(time) || time < 0.)
	error(_("invalid '%s' value"), "time");
    Rsleep(time);
    return R_NilValue;
}

