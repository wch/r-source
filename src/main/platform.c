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

/* "show.file" displays a file so that a user can use it. */
/* The function calls "R_ShowFile" which is a platform dependent */
/* hook that arranges for the file to be displayed. A reasonable */
/* approach would be to open a read-only edit window with the file */
/* displayed in it. */

SEXP do_date(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *date;
    checkArity(op, args);
    return mkString(R_Date());
}

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
    R_ShowFile(CHAR(STRING(fn)[0]), CHAR(STRING(tl)[0]));
    return R_NilValue;
}


/* "append.file" takes two file names as arguments and arranges for */
/* the second file to be appended to the second. */

#define APPENDBUFSIZE 512

static char *Append_ErrMsg = "unable to open file %s for appending\n";

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
    R_AppendFile(CHAR(STRING(fn1)[0]), CHAR(STRING(fn2)[0]));
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
	    if (remove(CHAR(STRING(fn1)[i])))
		warning("unable to remove file \"%s\"\n",
		        CHAR(STRING(fn1)[i]));
    return R_NilValue;
}

#include <sys/types.h>
#include <dirent.h>

SEXP do_dir(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP d, ans;
    DIR *dir;
    struct dirent *de;
    int count;
    checkArity(op, args);
    d = CAR(args);
    if (!isString(d) || length(d) < 1 || STRING(d)[0] == R_NilValue)
	errorcall(call, "invalid directory argument\n");
    if ((dir = opendir(CHAR(STRING(d)[0]))) == NULL)
	errorcall(call, "invalid directory/folder name\n");
    count = 0;
    while (de = readdir(dir))
	count++;
    rewinddir(dir);
    PROTECT(ans = allocVector(STRSXP, count));
    count = 0;
    while (de = readdir(dir))
	STRING(ans)[count++] = mkChar(de->d_name);
    closedir(dir);
    UNPROTECT(1);
    return ans;
}
