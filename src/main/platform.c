/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2001-7 The R Development Core Team
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

/* <UTF8> char here is either ASCII or handled as a whole */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Rinterface.h>
#include <Fileio.h>
#include <R_ext/Applic.h>		/* machar */

#include <time.h>

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif


/* Machine Constants */

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

    R_dec_min_exponent = floor(log10(R_AccuracyInfo.xmin)); /* smallest decimal exponent */
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
    SET_VECTOR_ELT(ans, 16, ScalarInteger(SIZEOF_LONG_DOUBLE));

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
#ifdef Win32
    SET_VECTOR_ELT(value, 5, mkString("win.binary"));
#else /* not Win32 */
#ifdef HAVE_AQUA
    SET_VECTOR_ELT(value, 5, mkString("mac.binary"));
#else /* not Win32 nor Aqua */
    SET_VECTOR_ELT(value, 5, mkString("source"));
#endif
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


/*  date
 *
 *  Return the current date in a standard format.  This uses standard
 *  POSIX calls which should be available on each platform.  We should
 *  perhaps check this in the configure script.
 */
/* BDR 2000/7/20.
 *  time and ctime are in fact ANSI C calls, so we don't check them.
 */
static char *R_Date()
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

SEXP attribute_hidden do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl, hd, pg;
    char **f, **h, *t, *vm, *pager;
    Rboolean dl;
    int i, n;

    checkArity(op, args);
    vm = vmaxget();
    fn = CAR(args); args = CDR(args);
    hd = CAR(args); args = CDR(args);
    tl = CAR(args); args = CDR(args);
    dl = (Rboolean)asLogical(CAR(args)); args = CDR(args);
    pg = CAR(args);
    n = 0;			/* -Wall */
    if (!isString(fn) || (n = length(fn)) < 1)
	errorcall(call, _("invalid filename specification"));
    if (!isString(hd) || length(hd) != n)
	errorcall(call, _("invalid 'headers'"));
    if (!isString(tl))
	errorcall(call, _("invalid 'title'"));
    if (!isString(pg))
        errorcall(call, _("invalid '%s' specification"), "pager");
    f = (char**)R_alloc(n, sizeof(char*));
    h = (char**)R_alloc(n, sizeof(char*));
    for (i = 0; i < n; i++) {
	if (!isNull(STRING_ELT(fn, i)))
	    /* Do better later for file names? */
	    f[i] = translateChar(STRING_ELT(fn, i));
	else
	    f[i] = CHAR(R_BlankString);
	if (!isNull(STRING_ELT(hd, i)))
	    h[i] = translateChar(STRING_ELT(hd, i));
	else
	    h[i] = CHAR(R_BlankString);
    }
    if (length(tl) >= 1 || !isNull(STRING_ELT(tl, 0)))
	t = translateChar(STRING_ELT(tl, 0));
    else
	t = CHAR(R_BlankString);
    if (length(pg) >= 1 || !isNull(STRING_ELT(pg, 0)))
	pager = CHAR(STRING_ELT(pg, 0));
    else
	pager = CHAR(R_BlankString);
    R_ShowFiles(n, f, h, t, dl, pager);
    vmaxset(vm);
    return R_NilValue;
}

/*  file.edit
 *
 *  Open a file in a text editor. The function calls
 *  "R_EditFiles" which is a platform dependent hook that invokes
 *  the given editor.
 *
 */


SEXP attribute_hidden do_fileedit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ti, ed;
    char **f, *vm, **title, *editor;
    int i, n;

    checkArity(op, args);
    vm = vmaxget();
    fn = CAR(args); args = CDR(args);
    ti = CAR(args); args = CDR(args);
    ed = CAR(args);

    n = length(fn);
    if (!isString(ed))
	errorcall(call, _("invalid '%s' specification"), "editor");
    if (n > 0) {
	if (!isString(fn))
	    errorcall(call, _("invalid '%s' specification"), "filename");
	f = (char**) R_alloc(n, sizeof(char*));
	title = (char**) R_alloc(n, sizeof(char*));
	for (i = 0; i < n; i++) {
	    if (!isNull(STRING_ELT(fn, i)))
		/* Do better later for file names? */
		f[i] = translateChar(STRING_ELT(fn, i));
	    else
		f[i] = CHAR(R_BlankString);
	    if (!isNull(STRING_ELT(ti, i)))
	    	title[i] = translateChar(STRING_ELT(ti, i));
	    else
	    	title[i] = CHAR(R_BlankString);
	}
    }
    else {  /* open a new file for editing */
	n = 1;
	f = (char**) R_alloc(1, sizeof(char*));
	f[0] = CHAR(R_BlankString);
	title = (char**) R_alloc(1, sizeof(char*));
	title[0] = CHAR(R_BlankString);
    }
    if (length(ed) >= 1 || !isNull(STRING_ELT(ed, 0)))
	/* Do better later for file names? */
	editor = translateChar(STRING_ELT(ed, 0));
    else
	editor = CHAR(R_BlankString);
    R_EditFiles(n, f, title, editor);
    vmaxset(vm);
    return R_NilValue;
}


/*  append.file
 *
 *  Given two file names as arguments and arranges for
 *  the second file to be appended to the second.
 *  op = 2 is codeFiles.append.
 */

#if defined(BUFSIZ) && (BUFSIZ > 512)
/* OS's buffer size in stdio.h, probably */
# define APPENDBUFSIZE BUFSIZ
#else
# define APPENDBUFSIZE 512
#endif

static int R_AppendFile(SEXP file1, SEXP file2)
{
    FILE *fp1, *fp2;
    char buf[APPENDBUFSIZE];
    int nchar, status = 0;
    if((fp1 = RC_fopen(file1, "ab", TRUE)) == NULL) {
        return 0;
    }
    if((fp2 = RC_fopen(file2, "rb", TRUE)) == NULL) {
        fclose(fp1);
        return 0;
    }
    while((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
        if(fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE) {
            goto append_error;
        }
    if(fwrite(buf, 1, nchar, fp1) != nchar) {
        goto append_error;
    }
    status = 1;
 append_error:
    if (status == 0)
	warning(_("write error during file append"));
    fclose(fp1);
    fclose(fp2);
    return status;
}

SEXP attribute_hidden do_fileappend(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n, n1, n2;
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
        errorcall(call, _("invalid first filename"));
    if (!isString(f2))
        errorcall(call, _("invalid second filename"));
    if (n1 < 1)
	errorcall(call, _("nothing to append to"));
    if (PRIMVAL(op) > 0 && n1 > 1)
	errorcall(call, _("'outFile' must be a single file"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n)); /* all FALSE */
    if (n1 == 1) { /* common case */
	FILE *fp1, *fp2;
	char buf[APPENDBUFSIZE];
	int nchar, status = 0;
	if(!(fp1 = RC_fopen(STRING_ELT(f1, 0), "ab", TRUE)))
	   goto done;
	for(i = 0; i < n; i++) {
	    status = 0;
	    if(!(fp2 = RC_fopen(STRING_ELT(f2, i), "rb", TRUE))) continue;
	    while((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
		if(fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE)
		    goto append_error;
	    if(fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
	    if(PRIMVAL(op) == 1 && buf[nchar - 1] != '\n') {
		if(fwrite("\n", 1, 1, fp1) != 1) goto append_error;
	    }

	    status = 1;
	append_error:
	    if (status == 0)
		warning(_("write error during file append"));
	    LOGICAL(ans)[i] = status;
	    fclose(fp2);
	}
	fclose(fp1);
    } else {
	for(i = 0; i < n; i++) {
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
    int i, n;
    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, _("invalid filename argument"));
    n = length(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(fn, i) != R_NilValue &&
	    (fp = RC_fopen(STRING_ELT(fn, i), "w", TRUE))
	    != NULL) {
	    LOGICAL(ans)[i] = 1;
	    fclose(fp);
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
        errorcall(call, _("invalid first filename"));
    n = length(f);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f, i) != R_NilValue)
	    LOGICAL(ans)[i] =
		(remove(R_ExpandFileName(translateChar(STRING_ELT(f, i)))) == 0);
    }
    UNPROTECT(1);
    return ans;
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for symlink */
#endif

SEXP attribute_hidden do_filesymlink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2;
    int n, n1, n2;
#ifdef HAVE_SYMLINK
    SEXP ans;
    int i;
    char from[PATH_MAX], to[PATH_MAX], *p;
#endif
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
        errorcall(call, _("invalid first filename"));
    if (!isString(f2))
        errorcall(call, _("invalid second filename"));
    if (n1 < 1)
	errorcall(call, _("nothing to link"));
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
#ifdef HAVE_SYMLINK
    PROTECT(ans = allocVector(LGLSXP, n));
    for(i = 0; i < n; i++) {
        if (STRING_ELT(f1, i%n1) == R_NilValue || STRING_ELT(f2, i%n2) == R_NilValue)
            LOGICAL(ans)[i] = 0;
        else {
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
	}
    }
    UNPROTECT(1);
    return ans;
#else
    warningcall(call, _("symlinks are not supported on this platform"));
    return allocVector(LGLSXP, n);
#endif
}

#ifdef Win32
int Rwin_rename(char *from, char *to);  /* in src/gnuwin32/extra.c */
#endif

SEXP attribute_hidden do_filerename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char from[PATH_MAX], to[PATH_MAX], *p;

    checkArity(op, args);
    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("'source' must be a single string"));
    p = R_ExpandFileName(translateChar(STRING_ELT(CAR(args), 0)));
    if (strlen(p) >= PATH_MAX - 1)
	error(_("expanded source name too long"));
    strncpy(from, p, PATH_MAX - 1);

    if (TYPEOF(CADR(args)) != STRSXP || LENGTH(CADR(args)) != 1)
	error(_("'destination' must be a single string"));
    p = R_ExpandFileName(translateChar(STRING_ELT(CADR(args), 0)));
    if (strlen(p) >= PATH_MAX - 1)
	error(_("expanded destination name too long"));
    strncpy(to, p, PATH_MAX - 1);

#ifdef Win32
    return Rwin_rename(from, to) == 0 ? mkTrue() : mkFalse();
#endif

    return rename(from, to) == 0 ? mkTrue() : mkFalse();
}

#ifdef HAVE_STAT
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
# endif

# if defined(Unix) && defined(HAVE_PWD_H) && defined(HAVE_GRP_H) \
  && defined(HAVE_GETPWUID) && defined(HAVE_GETGRGID)
#  include <pwd.h>
#  include <grp.h>
#  define UNIX_EXTRAS 1
# endif

SEXP attribute_hidden do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans, ansnames, fsize, mtime, ctime, atime, isdir,
	mode, xxclass;
#ifdef UNIX_EXTRAS
    SEXP uid, gid, uname, grname;
    struct passwd *stpwd;
    struct group *stgrp;
#endif
    int i, n;
#ifdef Win32
    struct _stati64 sb;
#else
    struct stat sb;
#endif

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, _("invalid filename argument"));
    n = length(fn);
#ifdef UNIX_EXTRAS
    PROTECT(ans = allocVector(VECSXP, 10));
    PROTECT(ansnames = allocVector(STRSXP, 10));
#else
    PROTECT(ans = allocVector(VECSXP, 6));
    PROTECT(ansnames = allocVector(STRSXP, 6));
#endif
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
    for (i = 0; i < n; i++) {
	if (STRING_ELT(fn, i) != R_NilValue &&
#ifdef Win32
	    _stati64(R_ExpandFileName(translateChar(STRING_ELT(fn, i))), &sb)
#else
	    stat(R_ExpandFileName(translateChar(STRING_ELT(fn, i))), &sb)
#endif
	    == 0) {
	    REAL(fsize)[i] = (double) sb.st_size;
	    LOGICAL(isdir)[i] = (sb.st_mode & S_IFDIR) > 0;
	    INTEGER(mode)[i]  = (int) sb.st_mode & 0007777;
	    REAL(mtime)[i] = (double) sb.st_mtime;
	    REAL(ctime)[i] = (double) sb.st_ctime;
	    REAL(atime)[i] = (double) sb.st_atime;
#ifdef UNIX_EXTRAS
	    INTEGER(uid)[i] = (int) sb.st_uid;
	    INTEGER(gid)[i] = (int) sb.st_gid;
	    stpwd = getpwuid(sb.st_uid);
	    if(stpwd) SET_STRING_ELT(uname, i, mkChar(stpwd->pw_name));
	    else SET_STRING_ELT(uname, i, NA_STRING);
	    stgrp = getgrgid(sb.st_gid);
	    if(stgrp) SET_STRING_ELT(grname, i, mkChar(stgrp->gr_name));
	    else SET_STRING_ELT(grname, i, NA_STRING);
#endif
	} else {
	    REAL(fsize)[i] = NA_REAL;
	    LOGICAL(isdir)[i] = NA_INTEGER;
	    INTEGER(mode)[i]  = NA_INTEGER;
	    REAL(mtime)[i] = NA_REAL;
	    REAL(ctime)[i] = NA_REAL;
	    REAL(atime)[i] = NA_REAL;
#ifdef UNIX_EXTRAS
	    INTEGER(uid)[i] = NA_INTEGER;
	    INTEGER(gid)[i] = NA_INTEGER;
	    SET_STRING_ELT(uname, i, NA_STRING);
	    SET_STRING_ELT(grname, i, NA_STRING);
#endif
	}
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(xxclass = allocVector(STRSXP, 1));
    SET_STRING_ELT(xxclass, 0, mkChar("octmode"));
    classgets(mode, xxclass);
    UNPROTECT(3);
    return ans;
}
#else
SEXP attribute_hidden do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("file.info() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif

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

#include "Rregex.h"

static SEXP filename(char *dir, char *file)
{
    SEXP ans;
    if (dir) {
#ifdef Win32
	switch (dir[strlen(dir)-1])
	{
	    case '/':
	    case '\\':
	    case ':':
	    	ans = allocString(strlen(dir) + strlen(file));
	    	sprintf(CHAR(ans), "%s%s", dir, file);
	    	break;
	    default:
#endif
	ans = allocString(strlen(dir) + strlen(R_FileSep) + strlen(file));
	sprintf(CHAR(ans), "%s%s%s", dir, R_FileSep, file);
#ifdef Win32
    	}
#endif
    }
    else {
	ans = allocString(strlen(file));
	sprintf(CHAR(ans), "%s", file);
    }
    return ans;
}

static void count_files(char *dnp, int *count,
			int allfiles, int recursive, int pattern, regex_t reg)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX];
#ifdef HAVE_STAT
    struct stat sb;
#endif

    if (strlen(dnp) >= PATH_MAX)  /* should not happen! */
	error(_("directory/folder path name too long"));
    if ((dir = opendir(dnp)) == NULL) {
	warning(_("list.files: '%s' is not a readable directory"), dnp);
    } else {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
#ifdef HAVE_STAT
		if(recursive) {
		    snprintf(p, PATH_MAX, "%s%s%s", dnp,
			     R_FileSep, de->d_name);
		    stat(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (strcmp(de->d_name, ".") && strcmp(de->d_name, ".."))
				count_files(p, count, allfiles, recursive,
				   	    pattern, reg);
			continue;
		    }
		}
#endif
		if (pattern) {
		    if(regexec(&reg, de->d_name, 0, NULL, 0) == 0) (*count)++;
		} else (*count)++;
	    }
	}
	closedir(dir);
    }
}

static void list_files(char *dnp, char *stem, int *count, SEXP ans,
			int allfiles, int recursive, int pattern, regex_t reg)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX], stem2[PATH_MAX];
#ifdef HAVE_STAT
    struct stat sb;
#endif

    if ((dir = opendir(dnp)) != NULL) {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
#ifdef HAVE_STAT
		if(recursive) {
		    snprintf(p, PATH_MAX, "%s%s%s", dnp,
			     R_FileSep, de->d_name);
		    stat(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (strcmp(de->d_name, ".") && strcmp(de->d_name, "..")) {
			    if(stem)
			    	snprintf(stem2, PATH_MAX, "%s%s%s", stem,
				         R_FileSep, de->d_name);
			    else
			    	strcpy(stem2, de->d_name);
			    list_files(p, stem2, count, ans, allfiles, recursive,
				   pattern, reg);
		    	}
			continue;
		    }
		}
#endif
		if (pattern) {
		    if (regexec(&reg, de->d_name, 0, NULL, 0) == 0)
			SET_STRING_ELT(ans, (*count)++,
				       filename(stem, de->d_name));
		} else
		    SET_STRING_ELT(ans, (*count)++,
				   filename(stem, de->d_name));
	    }
	}
	closedir(dir);
    }
}

SEXP attribute_hidden do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, p, ans;
    int allfiles, fullnames, count, pattern, recursive;
    int i, ndir;
    char *dnp;
    regex_t reg;

    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d))
	errorcall(call, _("invalid '%s' argument"), "directory");
    p = CAR(args);  args = CDR(args);
    pattern = 0;
    if (isString(p) && length(p) >= 1 && STRING_ELT(p, 0) != R_NilValue)
	pattern = 1;
    else if (!isNull(p) && !(isString(p) && length(p) < 1))
	errorcall(call, _("invalid '%s' argument"), "pattern");
    allfiles = asLogical(CAR(args)); args = CDR(args);
    fullnames = asLogical(CAR(args)); args = CDR(args);
    recursive = asLogical(CAR(args));
#ifndef HAVE_STAT
    if(recursive) {
	warningcall(call,
		    _("'recursive = TRUE' is not supported on this platform"));
	recursive = FALSE;
    }
#endif
    ndir = length(d);
    if (pattern && regcomp(&reg, translateChar(STRING_ELT(p, 0)), REG_EXTENDED))
        errorcall(call, _("invalid 'pattern' regular expression"));
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	count_files(dnp, &count, allfiles, recursive, pattern, reg);
    }
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	if (fullnames)
	    list_files(dnp, dnp, &count, ans, allfiles, recursive,
		       pattern, reg);
	else
	    list_files(dnp, NULL, &count, ans, allfiles, recursive,
		       pattern, reg);
    }
    if (pattern)
	regfree(&reg);
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

SEXP attribute_hidden do_fileexists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, ans;
    int i, nfile;
    checkArity(op, args);
    if (!isString(file = CAR(args)))
        errorcall(call, _("invalid '%s' argument"), "file");
    nfile = length(file);
    ans = allocVector(LGLSXP, nfile);
    for(i = 0; i < nfile; i++) {
	LOGICAL(ans)[i] = 0;
        if (STRING_ELT(file, i) != R_NilValue)
	    LOGICAL(ans)[i] = R_FileExists(translateChar(STRING_ELT(file, i)));
    }
    return ans;
}

/* <FIXME> can \n or \r occur as part of a MBCS extra bytes?
   Not that I know of */
static int filbuf(char *buf, FILE *fp)
{
    int c;
    while((c = fgetc(fp)) != EOF) {
	if (c == '\n' || c == '\r') {
	    *buf = '\0';
	    return 1;
	}
	*buf++ = c;
    }
    return 0;
}

SEXP attribute_hidden do_indexsearch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* index.search(topic, path, file, .Platform$file.sep, type) */
    SEXP topic, path, indexname, sep, type;
    char linebuf[256], topicbuf[256], *p, ctype[256];
    int i, npath, ltopicbuf;
    FILE *fp;

    checkArity(op, args);
    topic = CAR(args); args = CDR(args);
    if(!isString(topic) || length(topic) < 1 || isNull(topic))
	error(_("invalid '%s' argument"), "topic");
    path = CAR(args); args = CDR(args);
    if(!isString(path) || length(path) < 1 || isNull(path))
	error(_("invalid '%s' argument"), "path");
    indexname = CAR(args); args = CDR(args);
    if(!isString(indexname) || length(indexname) < 1 || isNull(indexname))
	error(_("invalid '%s' argument"), "indexname");
    sep = CAR(args); args = CDR(args);
    if(!isString(sep) || length(sep) < 1 || isNull(sep))
	error(_("invalid '%s' argument"), "sep");
    type = CAR(args);
    if(!isString(type) || length(type) < 1 || isNull(type))
	error(_("invalid '%s' argument"), "type");
    strcpy(ctype, CHAR(STRING_ELT(type, 0)));
    snprintf(topicbuf, 256, "%s\t", translateChar(STRING_ELT(topic, 0)));
    ltopicbuf = strlen(topicbuf);
    npath = length(path);
    for (i = 0; i < npath; i++) {
	snprintf(linebuf, 256, "%s%s%s%s%s",
		translateChar(STRING_ELT(path, i)),
		CHAR(STRING_ELT(sep, 0)),
		"help", CHAR(STRING_ELT(sep, 0)),
		CHAR(STRING_ELT(indexname, 0)));
	if ((fp = R_fopen(R_ExpandFileName(linebuf), "rt")) != NULL){
	    while (filbuf(linebuf, fp)) {
		if(strncmp(linebuf, topicbuf, ltopicbuf) == 0) {
		    p = &linebuf[ltopicbuf - 1];
		    while(isspace((int)*p)) p++;
		    fclose(fp);
		    if (!strcmp(ctype, "html"))
			snprintf(topicbuf, 256, "%s%s%s%s%s%s",
				translateChar(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"html", CHAR(STRING_ELT(sep, 0)),
				p, ".html");
		    else if (!strcmp(ctype, "R-ex"))
			snprintf(topicbuf, 256, "%s%s%s%s%s%s",
				translateChar(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"R-ex", CHAR(STRING_ELT(sep, 0)),
				p, ".R");
		    else if (!strcmp(ctype, "latex"))
			snprintf(topicbuf, 256, "%s%s%s%s%s%s",
				translateChar(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"latex", CHAR(STRING_ELT(sep, 0)),
				p, ".tex");
		    else /* type = "help" */
			snprintf(topicbuf, 256, "%s%s%s%s%s",
				translateChar(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				ctype, CHAR(STRING_ELT(sep, 0)), p);
		    return mkString(topicbuf);
		}
	    }
	    fclose(fp);
	}
    }
    return mkString("");
}

#define CHOOSEBUFSIZE 1024

SEXP attribute_hidden do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int _new, len;
    char buf[CHOOSEBUFSIZE];
    checkArity(op, args);
    _new = asLogical(CAR(args));
    if ((len = R_ChooseFile(_new, buf, CHOOSEBUFSIZE)) == 0)
	error(_("file choice cancelled"));
    if (len >= CHOOSEBUFSIZE - 1)
	errorcall(call, _("file name too long"));
    return mkString(R_ExpandFileName(buf));
}


/* needed for access, and perhaps for realpath */
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_ACCESS
SEXP attribute_hidden do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n, mode, modemask;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, _("invalid '%s' argument"), "names");
    n = length(fn);
    mode = asInteger(CADR(args));
    if(mode < 0 || mode > 7) error(_("invalid '%s' value"), "mode");
    modemask = 0;
    /* Versions for Windows prior to Vista ignored X_OK, but it has
       been changed to be an error to set it. */
#ifndef Win32
    if (mode & 1) modemask |= X_OK;
#endif
    if (mode & 2) modemask |= W_OK;
    if (mode & 4) modemask |= R_OK;
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++)
	INTEGER(ans)[i] = access(R_ExpandFileName(translateChar(STRING_ELT(fn, i))),
				 modemask);
    UNPROTECT(1);
    return ans;
}
#else
SEXP attribute_hidden do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error(_("file.access() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

SEXP attribute_hidden do_getlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef HAVE_LOCALE_H
    SEXP ans;
    int cat;
    char *p = NULL;

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if(cat == NA_INTEGER || cat < 0)
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
    if(cat != NA_INTEGER) p = setlocale(cat, NULL);
    PROTECT(ans = allocVector(STRSXP, 1));
    if(p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  SET_STRING_ELT(ans, 0, mkChar(""));
    UNPROTECT(1);
    return ans;
#else
    return R_NilValue;
#endif
}

/* Locale specs are always ASCII */
SEXP attribute_hidden do_setlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef HAVE_LOCALE_H
    SEXP locale = CADR(args), ans;
    int cat;
    char *p = "";

    checkArity(op, args);
    cat = asInteger(CAR(args));
    if(cat == NA_INTEGER || cat < 0)
	errorcall(call, _("invalid '%s' argument"), "category");
    if(!isString(locale) || LENGTH(locale) != 1)
	errorcall(call, _("invalid '%s' argument"), "locale");
    switch(cat) {
    case 1:
	cat = LC_ALL;
	p = CHAR(STRING_ELT(locale, 0));
	setlocale(LC_COLLATE, p);
#ifdef Win32
	if(strcmp(p, "C") == 0) setlocale(LC_CTYPE, "en");
	else setlocale(LC_CTYPE, p);
#else
	setlocale(LC_CTYPE, p);
#endif 
	setlocale(LC_MONETARY, p);
	setlocale(LC_TIME, p);
	p = setlocale(cat, NULL);
	break;
    case 2:
	cat = LC_COLLATE;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 3:
	cat = LC_CTYPE;
#ifdef Win32
	/* LC_CTYPE=C bombs in mingwex */
	p = CHAR(STRING_ELT(locale, 0));
	/* LC_CTYPE=C bombs in mingwex */
	if(strcmp(p, "C") == 0) setlocale(LC_CTYPE, "en");
	else setlocale(LC_CTYPE, p);
#else
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
#endif
	break;
    case 4:
	cat = LC_MONETARY;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 5:
	cat = LC_NUMERIC;
	warningcall(call, _("setting 'LC_NUMERIC' may cause R to function strangely"));
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
    case 6:
	cat = LC_TIME;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#ifdef LC_MESSAGES
/* this seems to exist in MinGW, but it does not work in Windows */
#ifndef Win32
    case 7:
	cat = LC_MESSAGES;
	p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
	break;
#endif
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
	errorcall(call, _("invalid '%s' argument"), "category");
    }
    PROTECT(ans = allocVector(STRSXP, 1));
    if(p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  {
	SET_STRING_ELT(ans, 0, mkChar(""));
	warning(_("OS reports request to set locale to \"%s\" cannot be honored"),
		CHAR(STRING_ELT(locale, 0)));
    }
    UNPROTECT(1);
#ifdef HAVE_LANGINFO_CODESET
    p = nl_langinfo(CODESET);
    known_to_be_utf8 = utf8locale = streql(p, "UTF-8") ? TRUE : FALSE;
    known_to_be_latin1 = latin1locale = streql(p, "ISO-8859-1") ? TRUE : FALSE;
#endif
#ifdef SUPPORT_MBCS
    mbcslocale = MB_CUR_MAX > 1;
#endif
#ifdef Win32
    {
	char *ctype = setlocale(LC_CTYPE, NULL);
	p = strrchr(ctype, '.');
	if(p && isdigit(p[1])) localeCP = atoi(p+1); else localeCP = 1252;
	/* Not 100% correct */
	known_to_be_latin1 = latin1locale = (localeCP == 1252);
    }
#endif
#if defined(Win32) && defined(SUPPORT_UTF8)
    utf8locale = mbcslocale = TRUE;
#endif
    return ans;
#else
    return R_NilValue;
#endif
}



SEXP attribute_hidden do_localeconv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef HAVE_LOCALE_H
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
#else
    return R_NilValue;
#endif
}

/* .Internal function for path.expand */
SEXP attribute_hidden do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, _("invalid '%s' argument"), "path");
    n = length(fn);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	SEXP tmp = mkChar(R_ExpandFileName(translateChar(STRING_ELT(fn, i))));
	markKnown(tmp, STRING_ELT(fn, i));
	SET_STRING_ELT(ans, i, tmp);
    }
    UNPROTECT(1);
    return ans;
}

#ifdef Unix
static int var_R_can_use_X11 = -1;

extern Rboolean R_access_X11(void); /* from src/unix/X11.c */

static Rboolean R_can_use_X11()
{
    if (var_R_can_use_X11 < 0) {
#ifdef HAVE_X11
	if(strcmp(R_GUIType, "none") != 0) {
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


SEXP attribute_hidden do_capabilities(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP what, ans, ansnames;
    int i = 0;
#ifdef Unix
    int j = 0;
    Rboolean X11 = FALSE;
#endif

    checkArity(op, args);
    what = CAR(args);
    if(!isNull(what) && !isString(what))
	error(_("invalid value of 'what' argument"));

#if defined(Unix) && defined(HAVE_X11)
    /* Don't load the module and contact the X11 display
       unless it is necessary.
    */
    if(isNull(what)) X11 = R_can_use_X11();
    else
        for (j = 0; j < LENGTH(what); j++)
       	    if(streql(CHAR(STRING_ELT(what, j)), "X11")
#ifdef HAVE_JPEG
	       || streql(CHAR(STRING_ELT(what, j)), "jpeg")
#endif
#ifdef HAVE_PNG
	       || streql(CHAR(STRING_ELT(what, j)), "png")
#endif
	        ) {
	        X11 = R_can_use_X11();
	        break;
	    }
#endif
    PROTECT(ans = allocVector(LGLSXP, 12));
    PROTECT(ansnames = allocVector(STRSXP, 12));

    SET_STRING_ELT(ansnames, i, mkChar("jpeg"));
#ifdef HAVE_JPEG
#ifdef Unix
    LOGICAL(ans)[i++] = X11;
#else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
#endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("png"));
#ifdef HAVE_PNG
#ifdef Unix
    LOGICAL(ans)[i++] = X11;
#else /* Windows */
    LOGICAL(ans)[i++] = TRUE;
#endif
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
#if defined(Unix) && !defined(__APPLE_CC__)
    LOGICAL(ans)[i++] = X11;
#else
    LOGICAL(ans)[i++] = TRUE;
#endif
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
#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)
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
    if(R_Interactive) LOGICAL(ans)[i] = TRUE;
#endif
#ifdef Unix
    if(strcmp(R_GUIType, "GNOME") == 0) {  /* always interactive */
	LOGICAL(ans)[i] = TRUE;  /* also AQUA ? */
    } else {
#if defined(HAVE_LIBREADLINE) && defined(HAVE_READLINE_HISTORY_H)
	extern Rboolean UsingReadline;
	if(R_Interactive && UsingReadline) LOGICAL(ans)[i] = TRUE;
#endif
    }
#endif
    i++;

    SET_STRING_ELT(ansnames, i, mkChar("iconv"));
#if defined(HAVE_ICONV) && defined(ICONV_LATIN1)
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

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
    char *name, ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    checkArity(op, args);
    if(!isString(CAR(args)) || length(CAR(args)) != 1)
	error(_("'hostname' must be a character vector of length 1"));
    name = translateChar(STRING_ELT(CAR(args), 0));

    hp = gethostbyname(name);

    if(hp == NULL) {		/* cannot resolve the address */
	warning(_("nsl() was unable to resolve host '%s'"), name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warningcall(call, _("unknown format returned by gethostbyname"));
	}
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkChar(ip));
	UNPROTECT(1);
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
    SEXP ans;

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = getpid();
    UNPROTECT(1);
    return ans;
}


#ifndef Win32
/* mkdir is defined in <sys/stat.h> */
SEXP attribute_hidden do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP path, ans;
    int res, show, recursive;
    char *p, dir[PATH_MAX];

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	errorcall(call, _("invalid '%s' argument"), "path");
    show = asLogical(CADR(args));
    if(show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if(recursive == NA_LOGICAL) recursive = 0;
    strcpy(dir, R_ExpandFileName(translateChar(STRING_ELT(path, 0))));
    /* remove trailing slashes */
    p = dir + strlen(dir) - 1;
    while(*p == '/' && strlen(dir) > 1) *p-- = '\0';
    if(recursive) {
	p = dir;
	while((p = Rf_strchr(p+1, '/'))) {
	    *p = '\0';
	    res = mkdir(dir, 0777);
	    if(res && errno != EEXIST) goto end;
	    *p = '/';
	}
    }
     res = mkdir(dir, 0777);
    if(show && res && errno == EEXIST)
	warning(_("'%s' already exists"), dir);
end:
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = (res==0);
    UNPROTECT(1);
    return (ans);
}
#else
#include <io.h> /* mkdir is defined here */
SEXP attribute_hidden do_dircreate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  path, ans;
    char *p, dir[MAX_PATH];
    int res, show, recursive;

    checkArity(op, args);
    path = CAR(args);
    if (!isString(path) || length(path) != 1)
	errorcall(call, _("invalid '%s' argument"), "path");
    show = asLogical(CADR(args));
    if(show == NA_LOGICAL) show = 0;
    recursive = asLogical(CADDR(args));
    if(recursive == NA_LOGICAL) recursive = 0;
    strcpy(dir, R_ExpandFileName(translateChar(STRING_ELT(path, 0))));
    /* need DOS paths on Win 9x */
    R_fixbackslash(dir);
    /* remove trailing slashes */
    p = dir + strlen(dir) - 1;
    while(*p == '\\' && strlen(dir) > 1 && *(p-1) != ':') *p-- = '\0';
    if(recursive) {
	p = dir;
	while((p = Rf_strchr(p+1, '\\'))) {
	    *p = '\0';
	    if(*(p-1) != ':') {
		res = mkdir(dir);
		if(res && errno != EEXIST) goto end;
	    }
	    *p = '\\';
	}
    }
    res = mkdir(dir);
    if(show && res && errno == EEXIST)
	warning(_("'%s' already exists"), dir);
end:
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = (res==0);
    UNPROTECT(1);
    return (ans);
}
#endif

SEXP attribute_hidden do_l10n_info(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP  ans, names;
    checkArity(op, args);
    PROTECT(ans = allocVector(VECSXP, 3));
    PROTECT(names = allocVector(STRSXP, 3));
    SET_STRING_ELT(names, 0, mkChar("MBCS"));
    SET_STRING_ELT(names, 1, mkChar("UTF-8"));
    SET_STRING_ELT(names, 2, mkChar("Latin-1"));
    SET_VECTOR_ELT(ans, 0, ScalarLogical(mbcslocale));
    SET_VECTOR_ELT(ans, 1, ScalarLogical(utf8locale));
    SET_VECTOR_ELT(ans, 2, ScalarLogical(latin1locale));
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return ans;
}

#ifndef Win32 /* in src/gnuwin32/extra.c */
#ifndef HAVE_DECL_REALPATH
extern char *realpath(const char *path, char *resolved_path);
#endif

SEXP attribute_hidden do_normalizepath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#if defined(HAVE_GETCWD) && defined(HAVE_REALPATH)
    SEXP ans, paths = CAR(args);
    int i, n = LENGTH(paths);
    char *path, tmp[PATH_MAX+1], abspath[PATH_MAX+1], *res = NULL;
    Rboolean OK;

    checkArity(op, args);
    if(!isString(paths))
	errorcall(call, "'path' must be a character vector");
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++) {
	path = translateChar(STRING_ELT(paths, i));
	OK = strlen(path) <= PATH_MAX;
	if(OK) {
	    if(path[0] == '/') strncpy(abspath, path, PATH_MAX);
	    else {
		OK = getcwd(abspath, PATH_MAX) != NULL;
		OK = OK && (strlen(path) + strlen(abspath) + 1 <= PATH_MAX);
		if(OK) {
		    strcat(abspath, "/");
		    strcat(abspath, path);
		}
	    }
	}
	if(OK) res = realpath(abspath, tmp);
	if (OK && res) SET_STRING_ELT(ans, i, mkChar(tmp));
	else SET_STRING_ELT(ans, i, STRING_ELT(paths, i));
    }
    UNPROTECT(1);
    return ans;
#else
    checkArity(op, args);
    warningcall(call, "insufficient OS support on this platform");
    return CAR(args);
#endif
}
#endif

SEXP attribute_hidden do_Cstack_info(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, nms;
    
    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 4));
    PROTECT(nms = allocVector(STRSXP, 4));
    INTEGER(ans)[0] = (R_CStackLimit == -1) ? NA_INTEGER : R_CStackLimit;
    INTEGER(ans)[1] = (R_CStackLimit == -1) ? NA_INTEGER :
	R_CStackDir * (R_CStackStart - (unsigned long) &ans);
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
