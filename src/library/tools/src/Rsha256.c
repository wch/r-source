/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2024   The R Core Team.
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

/* <UTF8> OK since this is intended to treat chars as byte streams */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#undef _

#include "tools.h"
#include "sha256.h"

#define SHA256_HASH_SIZE 32
#define SHA256_HEX_SIZE  64

/* convenience fn for init + process + finish */
static void *Rsha256_buffer (const void *buffer, size_t len, void *resblock)
{
  struct sha256_ctx ctx;
  Rsha256_init_ctx(&ctx);
  Rsha256_process_bytes(buffer, len, &ctx);
  return Rsha256_finish_ctx(&ctx, resblock);
}

/* This is essentailly identical to Rmd5 */

/* .Call so manages R_alloc stack */
SEXP Rsha256(SEXP files)
{
    SEXP ans;
    int i, j, nfiles = length(files), res;
#ifdef _WIN32
    const wchar_t *wpath;
#else
    const char *path;
#endif
    char out[SHA256_HEX_SIZE + 1];
    FILE *fp;
    unsigned char resblock[SHA256_HASH_SIZE];

    /* RAW mode: hash of one buffer instead of files */
    if (TYPEOF(files) == RAWSXP) {
	/* there is really no failure possible, but just in case... */
	if (!Rsha256_buffer((const void *) RAW(files), XLENGTH(files), resblock))
	    return ScalarString(NA_STRING);
	for(j = 0; j < SHA256_HASH_SIZE; j++)
	  snprintf (out+2*j, sizeof(out) - 2*j, "%02x", resblock[j]);
	return mkString(out);
    }
    /* otherwise list of files */
    if(!isString(files)) error(_("argument 'files' must be character"));
    PROTECT(ans = allocVector(STRSXP, nfiles));
    for(i = 0; i < nfiles; i++) {
#ifdef _WIN32
	wpath = filenameToWchar(STRING_ELT(files, i), FALSE);
	fp = _wfopen(wpath, L"rb");
#else
	path = translateChar(STRING_ELT(files, i));
	fp = fopen(path, "r");
#endif
	if(!fp) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    res = Rsha256_stream(fp, &resblock);
	    if(res) {
#ifdef _WIN32
		warning(_("sha256 failed on file '%ls'"), wpath);
#else
		warning(_("sha256 failed on file '%s'"), path);
#endif
		SET_STRING_ELT(ans, i, NA_STRING);
	    } else {
		for(j = 0; j < SHA256_HASH_SIZE; j++)
		  snprintf (out+2*j, sizeof(out) - 2*j, "%02x", resblock[j]);
		SET_STRING_ELT(ans, i, mkChar(out));
	    }
	    fclose(fp);
	}
    }
    UNPROTECT(1);
    return ans;
}
