/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-7   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> OK since this is intended to treat chars as byte streams */

#include <R.h>
#include "tools.h"
#define ROL_UNUSED
#include "md5.h"

SEXP Rmd5(SEXP files)
{
    SEXP ans;
    int i, j, nfiles = length(files), res;
    char *path, out[33];
    FILE *fp;
    unsigned char resblock[16];

    if(!isString(files)) error(_("argument 'files' must be character"));
    PROTECT(ans = allocVector(STRSXP, nfiles));
    for(i = 0; i < nfiles; i++) {
	path = translateChar(STRING_ELT(files, i));
#ifdef WIN32
	fp = fopen(path, "rb");
#else
	fp = fopen(path, "r");
#endif
	if(!fp) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    res = md5_stream(fp, &resblock);
	    if(res) {
		warning(_("md5 failed on file '%s'"), path);
		SET_STRING_ELT(ans, i, NA_STRING);
	    } else {
		for(j = 0; j < 16; j++)
		    sprintf (out+2*j, "%02x", resblock[j]);
		SET_STRING_ELT(ans, i, mkChar(out));
	    }
	    fclose(fp);
	}
    }
    UNPROTECT(1);
    return ans;
}
