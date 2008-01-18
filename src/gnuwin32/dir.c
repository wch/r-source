/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 2001-8 The R Development Core Team
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

/* A version of code in platform.c for Unix-alikes */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <sys/stat.h>
#include <dirent.h>

#define CBUFSIZE 2*PATH_MAX+1
static SEXP filename(const char *dir, const char *file)
{
    SEXP ans;
    char cbuf[CBUFSIZE];
    if (dir) {
	switch (dir[strlen(dir)-1])
	{
	case '/':
	case '\\':
	case ':':
	{
	    snprintf(cbuf, CBUFSIZE, "%s%s", dir, file);
	    ans = mkChar(cbuf);
	    break;
	}
	default:
	    snprintf(cbuf, CBUFSIZE, "%s/%s", dir, file);
	    ans = mkChar(cbuf);
    	}
    } else {
	snprintf(cbuf, CBUFSIZE, "%s", file);
        ans = mkChar(cbuf);
    }
    return ans;
}

static void count_files(const char *dnp, int *count,
			Rboolean allfiles, Rboolean recursive)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX];
    struct _stati64 sb;

    if (strlen(dnp) >= PATH_MAX)  /* should not happen! */
	error(_("directory/folder path name too long"));
    if ((dir = opendir(dnp)) == NULL) {
	warning(_("list.files: '%s' is not a readable directory"), dnp);
    } else {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
		if(recursive) {
		    snprintf(p, PATH_MAX, "%s/%s", dnp, de->d_name);
		    _stati64(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (strcmp(de->d_name, ".") && strcmp(de->d_name, ".."))
				count_files(p, count, allfiles, recursive);
			continue;
		    }
		}
		(*count)++;
	    }
	}
	closedir(dir);
    }
}

static void list_files(const char *dnp, const char *stem, int *count, SEXP ans,
		       Rboolean allfiles, Rboolean recursive)
{
    DIR *dir;
    struct dirent *de;
    char p[PATH_MAX], stem2[PATH_MAX];
    struct _stati64 sb;

    if ((dir = opendir(dnp)) != NULL) {
	while ((de = readdir(dir))) {
	    if (allfiles || !R_HiddenFile(de->d_name)) {
		if(recursive) {
		    snprintf(p, PATH_MAX, "%s/%s", dnp, de->d_name);
		    _stati64(p, &sb);
		    if((sb.st_mode & S_IFDIR) > 0) {
			if (strcmp(de->d_name, ".") && 
			    strcmp(de->d_name, "..")) {
			    if(stem)
			    	snprintf(stem2, PATH_MAX, "%s/%s", stem,
				         de->d_name);
			    else
			    	strcpy(stem2, de->d_name);
			    list_files(p, stem2, count, ans, allfiles, recursive);
		    	}
			continue;
		    }
		}
		SET_STRING_ELT(ans, (*count)++, filename(stem, de->d_name));
	    }
	}
	closedir(dir);
    }
}

SEXP attribute_hidden do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, ans;
    Rboolean allfiles, fullnames, recursive;
    int i, count, ndir;
    const char *dnp;

    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d))
	error(_("invalid '%s' argument"), "directory");
    allfiles = asLogical(CAR(args)); args = CDR(args);
    fullnames = asLogical(CAR(args)); args = CDR(args);
    recursive = asLogical(CAR(args));
    ndir = length(d);
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	count_files(dnp, &count, allfiles, recursive);
    }
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    for (i = 0; i < ndir ; i++) {
	dnp = R_ExpandFileName(translateChar(STRING_ELT(d, i)));
	if (fullnames)
	    list_files(dnp, dnp, &count, ans, allfiles, recursive);
	else
	    list_files(dnp, NULL, &count, ans, allfiles, recursive);
    }
    ssort(STRING_PTR(ans), count);
    UNPROTECT(1);
    return ans;
}
