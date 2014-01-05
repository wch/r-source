/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2013 The R Core Team
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

/* Formerly in src/main/platform.c */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
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
static const char  * const R_FileSep = FILESEP;

static void chmod_one(const char *name, const int grpwrt)
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
    mode_t mask, dirmask;
    if (grpwrt) {
	mask = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP; /* 0664 */
	dirmask = mask | S_IXUSR | S_IXGRP | S_IXOTH;           /* 0755 */
    } else {
	mask = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR;           /* 0644 */
	dirmask = mask | S_IXUSR | S_IXGRP | S_IXOTH;           /* 0755 */
    }
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
		chmod_one(p, grpwrt);
	    }
	    closedir(dir);
	} else { 
	    /* we were unable to read a dir */
	}
    }
}

/* recursively fix up permissions: used for R CMD INSTALL and build.
   'gwsxp' means set group-write permissions on directories.
   NB: this overrides umask. */
/* This is a .Call so manages R_alloc stack */
SEXP dirchmod(SEXP dr, SEXP gwsxp)
{
    if(!isString(dr) || length(dr) != 1)
	error(_("invalid '%s' argument"), "dir");
    chmod_one(translateChar(STRING_ELT(dr, 0)), asLogical(gwsxp));

    return R_NilValue;
}

#if defined(BUFSIZ) && (BUFSIZ > 512)
/* OS's buffer size in stdio.h, probably.
   Windows has 512, Solaris 1024, glibc 8192
 */
# define APPENDBUFSIZE BUFSIZ
#else
# define APPENDBUFSIZE 512
#endif

SEXP codeFilesAppend(SEXP f1, SEXP f2)
{
    int n, n1, n2;
    n1 = length(f1);
    n2 = length(f2);
    if (!isString(f1) || n1 != 1)
	error(_("invalid '%s' argument"), "file1");
    if (!isString(f2))
	error(_("invalid '%s' argument"), "file2");
    if (n2 < 1) return allocVector(LGLSXP, 0);
    n = (n1 > n2) ? n1 : n2; // will be n2.
    SEXP ans = PROTECT(allocVector(LGLSXP, n));
    for (int i = 0; i < n; i++) LOGICAL(ans)[i] = 0;  /* all FALSE */
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
	snprintf(buf, APPENDBUFSIZE, "#line 1 \"%s\"\n",
		 CHAR(STRING_ELT(f2, i)));
	if(fwrite(buf, 1, strlen(buf), fp1) != strlen(buf)) goto append_error;
	while ((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
	    if (fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE)
		goto append_error;
	if (fwrite(buf, 1, nchar, fp1) != nchar) goto append_error;
	if (!nchar || buf[nchar - 1] != '\n')
	    if (fwrite("\n", 1, 1, fp1) != 1) goto append_error;
	
	status = 1;
    append_error:
	if (status == 0) warning(_("write error during file append"));
	LOGICAL(ans)[i] = status;
	fclose(fp2);
    }
    fclose(fp1);
done:
    UNPROTECT(1);
    return ans;
}
