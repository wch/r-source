/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2001-2 The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Fileio.h>

#include <time.h>

/*  Platform
 *
 *  Return various platform dependent strings.  This is similar to
 *  "Machine", but for strings rather than numerical values.  These
 *  two functions should probably be amalgamated.
 */
static char *R_OSType = OSTYPE;
static char *R_FileSep = FILESEP;

SEXP do_Platform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value, names;
    char *tmp;
    checkArity(op, args);
    PROTECT(value = allocVector(VECSXP, 5));
    PROTECT(names = allocVector(STRSXP, 5));
    SET_STRING_ELT(names, 0, mkChar("OS.type"));
    SET_STRING_ELT(names, 1, mkChar("file.sep"));
    SET_STRING_ELT(names, 2, mkChar("dynlib.ext"));
    SET_STRING_ELT(names, 3, mkChar("GUI"));
    SET_STRING_ELT(names, 4, mkChar("endian"));
    SET_VECTOR_ELT(value, 0, mkString(R_OSType));
    SET_VECTOR_ELT(value, 1, mkString(R_FileSep));
    tmp = (char *) malloc(strlen(SHLIB_EXT) + 1);
    if(!tmp) {
	error("Could not allocate memory");
    }
    sprintf(tmp, "%s", SHLIB_EXT);
    SET_VECTOR_ELT(value, 2, mkString(tmp));
    SET_VECTOR_ELT(value, 3, mkString(R_GUIType));
#ifdef WORDS_BIGENDIAN
    SET_VECTOR_ELT(value, 4, mkString("big"));
#else
    SET_VECTOR_ELT(value, 4, mkString("little"));
#endif
    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
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
char *R_Date()
{
    time_t t;
    static char s[26];		/* own space */

    time(&t);
    strcpy(s, ctime(&t));
    s[24] = '\0';		/* overwriting the final \n */
    return s;
}

SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return mkString(R_Date());
}

/*  show.file
 *
 *  Display a file so that a user can view it.  The function calls
 *  "R_ShowFile" which is a platform dependent hook that arranges
 *  for the file to be displayed. A reasonable approach would be to
 *  open a read-only edit window with the file displayed in it.
 *
 *  FIXME : this should in fact take a vector of filenames and titles
 *  and display them concatenated in a window.  For a pure console
 *  version, write down a pipe to a pager.
 */

#ifdef OLD
SEXP do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl;
    checkArity(op, args);
    fn = CAR(args);
    tl = CADR(args);
    if (!isString(fn) || length(fn) < 1 || STRING_ELT(fn, 0) == R_NilValue)
	errorcall(call, "invalid filename");
    if (!isString(tl) || length(tl) < 1 || STRING_ELT(tl, 0) == R_NilValue)
	errorcall(call, "invalid filename");
    if (!R_ShowFile(R_ExpandFileName(CHAR(STRING_ELT(fn, 0))),
                    CHAR(STRING_ELT(tl, 0))))
	error("unable to display file \"%s\"", CHAR(STRING_ELT(fn, 0)));
    return R_NilValue;
}
#else
SEXP do_fileshow(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	errorcall(call, "invalid filename specification");
    if (!isString(hd) || length(hd) != n)
	errorcall(call, "invalid headers");
    if (!isString(tl))
	errorcall(call, "invalid title");
    if (!isString(pg))
        errorcall(call, "invalid pager specification");
    f = (char**)R_alloc(n, sizeof(char*));
    h = (char**)R_alloc(n, sizeof(char*));
    for (i = 0; i < n; i++) {
	if (!isNull(STRING_ELT(fn, i)))
	    f[i] = CHAR(STRING_ELT(fn, i));
	else
	    f[i] = CHAR(R_BlankString);
	if (!isNull(STRING_ELT(hd, i)))
	    h[i] = CHAR(STRING_ELT(hd, i));
	else
	    h[i] = CHAR(R_BlankString);
    }
    if (length(tl) >= 1 || !isNull(STRING_ELT(tl, 0)))
	t = CHAR(STRING_ELT(tl, 0));
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
#endif


/*  append.file
 *
 *  Given two file names as arguments and arranges for
 *  the second file to be appended to the second.
 */

#define APPENDBUFSIZE 512

static int R_AppendFile(char *file1, char *file2)
{
    FILE *fp1, *fp2;
    char buf[APPENDBUFSIZE];
    int nchar, status = 0;
    if((fp1 = R_fopen(R_ExpandFileName(file1), "ab")) == NULL) {
        return 0;
    }
    if((fp2 = R_fopen(R_ExpandFileName(file2), "rb")) == NULL) {
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
	warning("write error during file append!");
    fclose(fp1);
    fclose(fp2);
    return status;
}

SEXP do_fileappend(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f1, f2, ans;
    int i, n, n1, n2;
    checkArity(op, args);
    f1 = CAR(args); n1 = length(f1);
    f2 = CADR(args); n2 = length(f2);
    if (!isString(f1))
        errorcall(call, "invalid first filename");
    if (!isString(f2))
        errorcall(call, "invalid second filename");
    if (n1 < 1)
	errorcall(call, "nothing to append to");
    if (n2 < 1)
	return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(ans = allocVector(LGLSXP, n));
    for(i = 0; i < n; i++) {
        if (STRING_ELT(f1, i%n1) == R_NilValue || STRING_ELT(f2, i%n2) == R_NilValue)
            LOGICAL(ans)[i] = 0;
        else
            LOGICAL(ans)[i] =
		R_AppendFile(CHAR(STRING_ELT(f1, i%n1)),
			     CHAR(STRING_ELT(f2, i%n2)));
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_filecreate(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    FILE *fp;
    int i, n;
    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, "invalid filename argument");
    n = length(fn);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	LOGICAL(ans)[i] = 0;
	if (STRING_ELT(fn, i) != R_NilValue &&
	    (fp = R_fopen(R_ExpandFileName(CHAR(STRING_ELT(fn, i))), "w"))
	    != NULL) {
	    LOGICAL(ans)[i] = 1;
	    fclose(fp);
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_fileremove(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP f, ans;
    int i, n;
    checkArity(op, args);
    f = CAR(args);
    if (!isString(f))
        errorcall(call, "invalid first filename");
    n = length(f);
    PROTECT(ans = allocVector(LGLSXP, n));
    for (i = 0; i < n; i++) {
	if (STRING_ELT(f, i) != R_NilValue)
	    LOGICAL(ans)[i] =
		(remove(R_ExpandFileName(CHAR(STRING_ELT(f, i)))) == 0);
    }
    UNPROTECT(1);
    return ans;
}

#if defined(Win32) || defined(Macintosh)
# include <errno.h>
#endif

#ifdef Win32
# include <io.h>		/* for unlink */
#endif

SEXP do_filerename(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char from[PATH_MAX], to[PATH_MAX], *p;
    checkArity(op, args);
    
    if (TYPEOF(CAR(args)) != STRSXP)
	error("source must be a string");
    p = R_ExpandFileName(CHAR(STRING_ELT(CAR(args), 0)));
    if (strlen(p) >= PATH_MAX - 1)
	error("expanded source name too long");
    strncpy(from, p, PATH_MAX - 1);

    if (TYPEOF(CADR(args)) != STRSXP)
	error("destination must be a string");
    p = R_ExpandFileName(CHAR(STRING_ELT(CADR(args), 0)));
    if (strlen(p) >= PATH_MAX - 1)
	error("expanded destination name too long");
    strncpy(to, p, PATH_MAX - 1);

#if defined(Win32) || defined(Macintosh)
    /* rename() on Windows does not overwrite files */
    if (!unlink(to))
	if (errno == EACCES) return mkFalse();
#endif
    return rename(from, to) == 0 ? mkTrue() : mkFalse();
}

#ifndef Macintosh
# include <sys/types.h>
#else 
# include <types.h>
#endif /* mac */

#if HAVE_DIRENT_H
# include <dirent.h>
#elif HAVE_SYS_NDIR_H
# include <sys/ndir.h>
#elif HAVE_SYS_DIR_H
# include <sys/dir.h>
#elif HAVE_NDIR_H
# include <ndir.h>
#elif defined(Macintosh)
# include "dirent.h"		/* use a local equivalent to dirent.h */
#endif

#ifdef USE_SYSTEM_REGEX
# include <regex.h>
#else
# include "Rregex.h"
#endif

static SEXP filename(char *dir, char *file)
{
    SEXP ans;
    if (dir) {
	ans = allocString(strlen(dir) + strlen(R_FileSep) + strlen(file));
	sprintf(CHAR(ans), "%s%s%s", dir, R_FileSep, file);
    }
    else {
	ans = allocString(strlen(file));
	sprintf(CHAR(ans), "%s", file);
    }
    return ans;
}

SEXP do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, p, ans;
    DIR *dir;
    struct dirent *de;
    int allfiles, fullnames, count, pattern;
    int i, ndir;
    char *dnp, dirname[PATH_MAX];
    regex_t reg;

    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d))
	errorcall(call, "invalid directory argument");
    p = CAR(args);  args = CDR(args);
    pattern = 0;
    if (isString(p) && length(p) >= 1 && STRING_ELT(p, 0) != R_NilValue)
	pattern = 1;
    else if (!isNull(p) && !(isString(p) && length(p) < 1))
	errorcall(call, "invalid pattern argument");
    allfiles = asLogical(CAR(args)); args = CDR(args);
    fullnames = asLogical(CAR(args));
    ndir = length(d);
    if (pattern && regcomp(&reg, CHAR(STRING_ELT(p, 0)), REG_EXTENDED))
        errorcall(call, "invalid pattern regular expression");
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(CHAR(STRING_ELT(d, i)));

	if (strlen(dnp) >= PATH_MAX)  /* should not happen! */
	    error("directory/folder path name too long");
	strcpy(dirname, dnp);
	if ((dir = opendir(dirname)) == NULL) {
	    /* errorcall(call, "invalid directory/folder name");*/
	    warning("list.files: %s is not a readable directory", dirname);
	} else {  
	    while ((de = readdir(dir))) {
		if (allfiles || !R_HiddenFile(de->d_name)) {
		    if (pattern) {
			if(regexec(&reg, de->d_name, 0, NULL, 0) == 0)
			    count++;
		    }
		    else
			count++;
		}
	    }
	    closedir(dir);
	}
    }
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(CHAR(STRING_ELT(d, i)));
/*	if (strlen(dnp) >= PATH_MAX) We've already checked!
	error("directory/folder path name too long"); */
	strcpy(dirname, dnp);
	if (fullnames)
	    dnp = dirname;
	else
	    dnp = NULL;
	if ((dir = opendir(dirname)) != NULL) {
	    while ((de = readdir(dir))) {
		if (allfiles || !R_HiddenFile(de->d_name)) {
		    if (pattern) {
			if (regexec(&reg, de->d_name, 0, NULL, 0) == 0)
			    SET_STRING_ELT(ans, count++, filename(dnp, de->d_name));
		    }
		    else
			SET_STRING_ELT(ans, count++, filename(dnp, de->d_name));
		}
	    }
	    closedir(dir);
	}
    }
    if (pattern)
	regfree(&reg);
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}

SEXP do_Rhome(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *path;
    checkArity(op, args);
    if (!(path = R_HomeDir()))
	error("unable to determine R home location");
    return mkString(path);
}

SEXP do_fileexists(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, ans;
    int i, nfile;
    checkArity(op, args);
    if (!isString(file = CAR(args)))
        errorcall(call, "invalid file argument");
    nfile = length(file);
    ans = allocVector(LGLSXP, nfile);
    for(i = 0; i < nfile; i++) {
	LOGICAL(ans)[i] = 0;
        if (STRING_ELT(file, i) != R_NilValue)
	    LOGICAL(ans)[i] = R_FileExists(CHAR(STRING_ELT(file, i)));
    }
    return ans;
}

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

SEXP do_indexsearch(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* index.search(topic, path, file, .Platform$file.sep, type) */
    SEXP topic, path, indexname, sep, type;
    char linebuf[256], topicbuf[256], *p, ctype[256];
    int i, npath, ltopicbuf;
    FILE *fp;

    checkArity(op, args);
    topic = CAR(args); args = CDR(args);
    if(!isString(topic) || length(topic) < 1 || isNull(topic))
	error("invalid \"topic\" argument");
    path = CAR(args); args = CDR(args);
    if(!isString(path) || length(path) < 1 || isNull(path))
	error("invalid \"path\" argument");
    indexname = CAR(args); args = CDR(args);
    if(!isString(indexname) || length(indexname) < 1 || isNull(indexname))
	error("invalid \"indexname\" argument");
    sep = CAR(args); args = CDR(args);
    if(!isString(sep) || length(sep) < 1 || isNull(sep))
	error("invalid \"sep\" argument");
    type = CAR(args);
    if(!isString(type) || length(type) < 1 || isNull(type))
	error("invalid \"type\" argument");
    strcpy(ctype, CHAR(STRING_ELT(type, 0)));
    sprintf(topicbuf, "%s\t", CHAR(STRING_ELT(topic, 0)));
    ltopicbuf = strlen(topicbuf);
    npath = length(path);
    for (i = 0; i < npath; i++) {
	sprintf(linebuf, "%s%s%s%s%s",
		CHAR(STRING_ELT(path, i)),
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
			sprintf(topicbuf, "%s%s%s%s%s%s",
				CHAR(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"html", CHAR(STRING_ELT(sep, 0)),
				p, ".html");
		    else if (!strcmp(ctype, "R-ex"))
			sprintf(topicbuf, "%s%s%s%s%s%s",
				CHAR(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"R-ex", CHAR(STRING_ELT(sep, 0)),
				p, ".R");
		    else if (!strcmp(ctype, "latex"))
			sprintf(topicbuf, "%s%s%s%s%s%s",
				CHAR(STRING_ELT(path, i)),
				CHAR(STRING_ELT(sep, 0)),
				"latex", CHAR(STRING_ELT(sep, 0)),
				p, ".tex");
		    else /* type = "help" */
			sprintf(topicbuf, "%s%s%s%s%s",
				CHAR(STRING_ELT(path, i)),
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

SEXP do_filechoose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int new, len;
    char buf[CHOOSEBUFSIZE];
    checkArity(op, args);
    new = asLogical(CAR(args));
    if ((len = R_ChooseFile(new, buf, CHOOSEBUFSIZE)) == 0)
	error("file choice cancelled");
    if (len >= CHOOSEBUFSIZE - 1)
	errorcall(call, "file name too long");
    return mkString(R_ExpandFileName(buf));
}

#ifdef HAVE_STAT
# ifndef Macintosh
#  ifdef HAVE_SYS_TYPES_H
#   include <sys/types.h>
#  endif
#  ifdef HAVE_SYS_STAT_H
#   include <sys/stat.h>
#  endif
# else /* Macintosh */
#  include <types.h>
#  ifndef __MRC__
#   include <stat.h>
#  else
#   include <mpw_stat.h>
#  endif
# endif /* Macintosh */

# if defined(Unix) && defined(HAVE_PWD_H) && defined(HAVE_GRP_H) \
  && defined(HAVE_GETPWUID) && defined(HAVE_GETGRGID)
#  include <pwd.h>
#  include <grp.h>
#  define UNIX_EXTRAS 1
# endif

SEXP do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans, ansnames, fsize, mtime, ctime, atime, isdir, 
	mode, xxclass;
#ifdef UNIX_EXTRAS
    SEXP uid, gid, uname, grname;
    struct passwd *stpwd;
    struct group *stgrp;
#endif
    int i, n;
    struct stat sb;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, "invalid filename argument");
    n = length(fn);
#ifdef UNIX_EXTRAS
    PROTECT(ans = allocVector(VECSXP, 10));
    PROTECT(ansnames = allocVector(STRSXP, 10));
#else
    PROTECT(ans = allocVector(VECSXP, 6));
    PROTECT(ansnames = allocVector(STRSXP, 6));
#endif
    fsize = SET_VECTOR_ELT(ans, 0, allocVector(INTSXP, n));
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
	    stat(R_ExpandFileName(CHAR(STRING_ELT(fn, i))), &sb) == 0) {
	    INTEGER(fsize)[i] = (int) sb.st_size;
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
	    INTEGER(fsize)[i] = NA_INTEGER;
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
SEXP do_fileinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("file.info is not implemented on this system");
    return R_NilValue;		/* -Wall */
}
#endif

#ifdef HAVE_ACCESS
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

SEXP do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n, mode, modemask;

    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, "invalid names argument");
    n = length(fn);
    mode = asInteger(CADR(args));
    if(mode < 0 || mode > 7) error("invalid mode value");
    modemask = 0;
    if (mode & 1) modemask |= X_OK;
    if (mode & 2) modemask |= W_OK;
    if (mode & 4) modemask |= R_OK;
    PROTECT(ans = allocVector(INTSXP, n));
    for (i = 0; i < n; i++)
	INTEGER(ans)[i] = access(R_ExpandFileName(CHAR(STRING_ELT(fn, i))),
				 modemask);
    UNPROTECT(1);
    return ans;
}
#else
SEXP do_fileaccess(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("file.access is not implemented on this system");
    return R_NilValue;		/* -Wall */
}
#endif

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

SEXP do_getlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef HAVE_LOCALE_H
    SEXP ans;
    int cat;
    char *p;
    
    checkArity(op, args);
    cat = asInteger(CAR(args));
    if(cat == NA_INTEGER || cat < 0)
	error("invalid `category' argument");
    switch(cat) {
    case 1: cat = LC_ALL; break;
    case 2: cat = LC_COLLATE; break;
    case 3: cat = LC_CTYPE; break;
    case 4: cat = LC_MONETARY; break;
    case 5: cat = LC_NUMERIC; break;
    case 6: cat = LC_TIME; break;
    }
    p = setlocale(cat, NULL);
    PROTECT(ans = allocVector(STRSXP, 1));
    if(p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  SET_STRING_ELT(ans, 0, mkChar(""));
    UNPROTECT(1);
    return ans;  
#else
    return R_NilValue;
#endif
}

SEXP do_setlocale(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef HAVE_LOCALE_H
    SEXP locale = CADR(args), ans;
    int cat;
    char *p;
    
    checkArity(op, args);
    cat = asInteger(CAR(args));
    if(cat == NA_INTEGER || cat < 0)
	error("invalid `category' argument");
    if(!isString(locale) || LENGTH(locale) != 1)
	error("invalid `locale' argument");
    switch(cat) {
    case 1: cat = LC_ALL; break;
    case 2: cat = LC_COLLATE; break;
    case 3: cat = LC_CTYPE; break;
    case 4: cat = LC_MONETARY; break;
    case 5: cat = LC_NUMERIC; break;
    case 6: cat = LC_TIME; break;
    }
    p = setlocale(cat, CHAR(STRING_ELT(locale, 0)));
    PROTECT(ans = allocVector(STRSXP, 1));
    if(p) SET_STRING_ELT(ans, 0, mkChar(p));
    else  {
	SET_STRING_ELT(ans, 0, mkChar(""));
	warningcall(call, "OS reports request cannot be honored");
    }
    UNPROTECT(1);
    return ans;
#else
    return R_NilValue;
#endif
}



SEXP do_localeconv(SEXP call, SEXP op, SEXP args, SEXP rho)
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
SEXP do_pathexpand(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, ans;
    int i, n;
    
    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn))
        errorcall(call, "invalid path argument");
    n = length(fn);
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
	SET_STRING_ELT(ans, i, 
		       mkChar(R_ExpandFileName(CHAR(STRING_ELT(fn, i)))));
    UNPROTECT(1);
    return ans;
}

SEXP do_capabilities(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    int i = 0;
    
    checkArity(op, args);
    PROTECT(ans = allocVector(LGLSXP, 13));
    PROTECT(ansnames = allocVector(STRSXP, 13));

    SET_STRING_ELT(ansnames, i, mkChar("jpeg"));
#ifdef HAVE_JPEG
#ifdef Unix
    LOGICAL(ans)[i++] = strcmp(R_GUIType, "X11") == 0;
#else
    LOGICAL(ans)[i++] = TRUE;
#endif
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("png"));
#ifdef HAVE_PNG
#ifdef Unix
    LOGICAL(ans)[i++] = strcmp(R_GUIType, "X11") == 0;
#else
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
#ifdef Unix
    LOGICAL(ans)[i++] = strcmp(R_GUIType, "X11") == 0;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("GNOME"));
#ifdef Unix
    LOGICAL(ans)[i++] = strcmp(R_GUIType, "GNOME") == 0;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("libz"));
    LOGICAL(ans)[i++] = TRUE;	/* always true in this version */

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
       with any of the GUIs or under Unix if readline is available and in 
       use */ 
    SET_STRING_ELT(ansnames, i, mkChar("cledit"));
    LOGICAL(ans)[i] = FALSE;
#if defined(Win32) || defined(Macintosh)
    if(R_Interactive) LOGICAL(ans)[i] = TRUE;
#endif
#ifdef Unix
    if(strcmp(R_GUIType, "GNOME") == 0) {
	if(R_Interactive) LOGICAL(ans)[i] = TRUE;
    } else {
#ifdef HAVE_LIBREADLINE
	extern Rboolean UsingReadline;
	if(R_Interactive && UsingReadline) LOGICAL(ans)[i] = TRUE;
#endif
    }
#endif
    i++;

    SET_STRING_ELT(ansnames, i, mkChar("IEEE754"));
#if defined(IEEE_754)
    LOGICAL(ans)[i++] = TRUE;
#else
    LOGICAL(ans)[i++] = FALSE;
#endif

    SET_STRING_ELT(ansnames, i, mkChar("bzip2"));
#if defined(HAVE_BZLIB)
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

SEXP do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;
    char *name, ip[] = "xxx.xxx.xxx.xxx";
    struct hostent *hp;

    checkArity(op, args);
    if(!isString(CAR(args)) || length(CAR(args)) != 1)
	error("hostname must be a character vector of length 1");
    name = CHAR(STRING_ELT(CAR(args), 0));
    
    hp = gethostbyname(name);

    if(hp == NULL) {		/* cannot resolve the address */
	warning("nsl() was unable to resolve host `%s'", name);
    } else {
	if (hp->h_addrtype == AF_INET) {
	    struct in_addr in;
	    memcpy(&in.s_addr, *(hp->h_addr_list), sizeof (in.s_addr));
	    strcpy(ip, inet_ntoa(in));
	} else {
	    warningcall(call, "unknown format returned by gethostbyname");
	}
	PROTECT(ans = allocVector(STRSXP, 1));
	SET_STRING_ELT(ans, 0, mkChar(ip));
	UNPROTECT(1);
    }
    return ans;
}
#else
SEXP do_nsl(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning("nsl is not supported on this platform");
    return R_NilValue;
}
#endif
