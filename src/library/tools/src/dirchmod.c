/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2012 The R Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
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
SEXP dirchmod(SEXP dr)
{
    if(!isString(dr) || length(dr) != 1)
	error(_("invalid '%s' argument"), "dir");
    chmod_one(translateChar(STRING_ELT(dr, 0)));

    return R_NilValue;
}
