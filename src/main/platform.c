/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, The R Development Core Team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"

#include <time.h>

/*  Platform
 *
 *  Return various platform dependent strings.  This is similar to
 *  "Machine", but for strings rather than numerical values.  These
 *  two functions should probably be amalgamated.
 */

SEXP do_Platform(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value, names;
    checkArity(op, args);
    PROTECT(value = allocVector(VECSXP, 3));
    PROTECT(names = allocVector(STRSXP, 3));
    STRING(names)[0]  = mkChar("OS.type");
    STRING(names)[1]  = mkChar("file.sep");
    STRING(names)[2]  = mkChar("dynload.ext");
#ifdef Unix
    VECTOR(value)[0]  = mkString("Unix");
    VECTOR(value)[1]  = mkString("/");
    VECTOR(value)[2]  = mkString(".so");
#endif
#ifdef Macintosh
    VECTOR(value)[0]  = mkString("Macintosh");
    VECTOR(value)[1]  = mkString(":");
    VECTOR(value)[2]  = mkString(".dll");
#endif
#ifdef Win32
    VECTOR(value)[0]  = mkString("Windows");
    VECTOR(value)[1]  = mkString("\\");
    VECTOR(value)[2]  = mkString(".dll");
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

char *R_Date()
{     
    time_t t;
    time(&t);
    return ctime(&t);
}     

SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *date;
    checkArity(op, args);
    return mkString(R_Date());
}

/*  show.file
 *
 *  Display a file so that a user can view it.  The function calls
 *  "R_ShowFile" which is a platform dependent hook that arranges
 *  for the file to be displayed. A reasonable approach would be to
 *  open a read-only edit window with the file displayed in it.
 */

SEXP do_showfile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn, tl;
    checkArity(op, args);
    fn = CAR(args);
    tl = CADR(args);
    if (!isString(fn) || length(fn) < 1 || STRING(fn)[0] == R_NilValue)
	errorcall(call, "invalid filename\n");
    if (!isString(tl) || length(tl) < 1 || STRING(tl)[0] == R_NilValue)
	errorcall(call, "invalid filename\n");
    if (!R_ShowFile(R_ExpandFileName(CHAR(STRING(fn)[0])),
                    CHAR(STRING(tl)[0])))
	error("unable to display file \"%s\"\n", CHAR(STRING(fn)[0]));
    return R_NilValue;
}


/*  append.file
 *
 *  Given two file names as arguments and arranges for
 *  the second file to be appended to the second.
 */

#define APPENDBUFSIZE 512

void R_AppendFile(char *file1, char *file2)
{
    FILE *fp1, *fp2;
    char buf[APPENDBUFSIZE];
    int nchar;
    if((fp1 = fopen(file1, "a")) == NULL) {
        error("unable to open file %s for appending\n", file1);
    }
    if((fp2 = fopen(file2, "r")) == NULL) {
        fclose(fp1);
        error("unable to open file %s for reading\n", file2);
    }
    while((nchar = fread(buf, 1, APPENDBUFSIZE, fp2)) == APPENDBUFSIZE)
        if(fwrite(buf, 1, APPENDBUFSIZE, fp1) != APPENDBUFSIZE)
            goto append_error;
    if(fwrite(buf, 1, nchar, fp1) != nchar)
        goto append_error;
    fclose(fp1);
    fclose(fp2);
 append_error:
    error("error writing to file %s\n", file1);
}

SEXP do_appendfile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn1, fn2;
    checkArity(op, args);
    fn1 = CAR(args);
    fn2 = CADR(args);
    if (!isString(fn1) || length(fn1) < 1 || STRING(fn1)[0] == R_NilValue)
        errorcall(call, "invalid first filename\n");
    if (!isString(fn2) || length(fn2) < 1 || STRING(fn2)[0] == R_NilValue)
        errorcall(call, "invalid second filename\n");
    R_AppendFile(R_ExpandFileName(CHAR(STRING(fn1)[0])),
                 R_ExpandFileName(CHAR(STRING(fn2)[0])));
    return R_NilValue;
}

SEXP do_removefile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn1;
    int i, n;
    checkArity(op, args);
    fn1 = CAR(args);     
    if (!isString(fn1))
        errorcall(call, "invalid first filename\n");
    n = length(fn1);
    for (i = 0; i < n; i++)
	if (STRING(fn1)[i] != R_NilValue)
	    if (remove(R_ExpandFileName(CHAR(STRING(fn1)[i]))))
		warning("unable to remove file \"%s\"\n",
		        CHAR(STRING(fn1)[i]));
    return R_NilValue;
}

#ifndef Macintosh
#include <sys/types.h>
#endif
#include "dirent.h"
#ifdef HAVE_REGCOMP
#include "regex.h"
#endif

SEXP do_listfiles(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, p, ans;
    DIR *dir;
    struct dirent *de;
    int allfiles, count, pattern;
#ifdef HAVE_REGCOMP
    regex_t reg;
#endif
    checkArity(op, args);
    d = CAR(args);  args = CDR(args);
    if (!isString(d) || length(d) < 1 || STRING(d)[0] == R_NilValue)
	errorcall(call, "invalid directory argument\n");
    p = CAR(args);  args = CDR(args);
    if (isNull(p) || (isString(p) && length(p) < 1))
	pattern = 0;
    else if (isString(p) && length(p) >= 1 && STRING(p)[0] != R_NilValue)
	pattern = 1;
    else
	errorcall(call, "invalid pattern argument\n");
    allfiles = asLogical(CAR(args));
    if ((dir = opendir(R_ExpandFileName(CHAR(STRING(d)[0])))) == NULL)
	errorcall(call, "invalid directory/folder name\n");
#ifdef HAVE_REGCOMP
    if (pattern && regcomp(&reg, CHAR(STRING(p)[0]), REG_EXTENDED))
	errorcall(call, "invalid pattern regular expression\n");
#else
    warning("pattern specification is not available in \"list.files\"\n");
#endif
    count = 0;
    while (de = readdir(dir)) {
        if (allfiles || !R_HiddenFile(de->d_name))
#ifdef HAVE_REGCOMP
	    if (pattern) {
		if(regexec(&reg, de->d_name, 0, NULL, 0) == 0)
		    count++;
	    }
	    else
#endif
		count++;
    }
    rewinddir(dir);
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    while (de = readdir(dir)) {
        if (allfiles || !R_HiddenFile(de->d_name))
#ifdef HAVE_REGCOMP
	    if (pattern) {
		if (regexec(&reg, de->d_name, 0, NULL, 0) == 0)
		    STRING(ans)[count++] = mkChar(de->d_name);
	    }
	    else
#endif
		STRING(ans)[count++] = mkChar(de->d_name);
    }
    closedir(dir);
#ifdef HAVE_REGCOMP
    regfree(&reg);
#endif
    ssort(STRING(ans), count);
    UNPROTECT(1);
    return ans;
}

SEXP do_Rhome(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *path;
    checkArity(op, args);
    if (!(path = R_HomeDir()))
	error("unable to determine R home location\n");
    return mkString(path);
}
