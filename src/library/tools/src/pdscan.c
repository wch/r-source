/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2020 The R Core Team.
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

#include <R.h>
#include <Rdefines.h>
#include <ctype.h>
#include "tools.h"

static SEXP package_dependencies_scan_one(SEXP this) {
    SEXP y;
    Rboolean save, skip;
    int size = 256, i, j, nb = 0, ne = 0, u, v, w;
    int *beg, *end;
    const char *s;
    char c, *t, *p, q = '\0';
    cetype_t e;
    
    if(this == NA_STRING) {
        return NEW_CHARACTER(0);
    }

    beg = Calloc(size, int);
    end = Calloc(size, int);

    e = getCharCE(this);
    s = CHAR(this);
    i = 0;
    save = FALSE;
    skip = FALSE;
    while((c = *s++) != '\0') {
	if(skip) {
	    if(c == ',')
		skip = FALSE;
	} else {
	    if(save) {
		if(!isalnum(c) && (c != '.')) {
		    save = FALSE;
		    if((q == 'R') && (beg[ne] == (i - 1)))
			nb--;
		    else {
			end[ne] = i - 1;
			ne++;
		    }
		}
	    } else {
		if(isalpha(c)) {
		    save = TRUE;
		    q = c;
		    if(nb >= size) {
			if(size > INT_MAX / 2)
			    error(_("too many items"));
			size *= 2;
			beg = Realloc(beg, size, int);
			end = Realloc(end, size, int);
		    }
		    beg[nb] = i;
		    nb++;
		}
	    }
	}
	i++;
    }
    if(ne < nb) {
	if((q == 'R') && (beg[ne] == (i - 1)))
	    nb--;
	else
	    end[ne] = i - 1;
    }
    
    PROTECT(y = NEW_CHARACTER(nb));
    s = CHAR(this);
    v = -1;
    for(i = 0; i < nb; i++) {
        u = beg[i];
        s += (u - v - 1);
        v = end[i];
        w = v - u + 1;
        p = t = (char *) R_alloc(w + 1, sizeof(char));
        for(j = 0; j < w; j++) {
            *t++ = *s++;
        }
        *t = '\0';
        SET_STRING_ELT(y, i, mkCharCE(p, e));
    }

    Free(beg);
    Free(end);

    UNPROTECT(1);

    return y;
}

SEXP package_dependencies_scan(SEXP x) {
    SEXP y, z, this;
    R_xlen_t i, j, k, nx, ny;

    if(TYPEOF(x) != STRSXP)
	error(_("non-character argument"));

    nx = LENGTH(x);

    if(nx < 1)
        return NEW_CHARACTER(0);

    if(nx == 1)
        return package_dependencies_scan_one(STRING_ELT(x, 0));

    PROTECT(z = NEW_LIST(nx));
    ny = 0;
    for(i = 0; i < nx; i++) {
        this = package_dependencies_scan_one(STRING_ELT(x, i));
        SET_VECTOR_ELT(z, i, this);
        ny += LENGTH(this);
    }
    // Now unlist.
    k = 0;
    PROTECT(y = NEW_STRING(ny));
    for(i = 0; i < nx; i++) {
        this = VECTOR_ELT(z, i);
        for(j = 0; j < LENGTH(this); j++, k++)
            SET_STRING_ELT(y, k, STRING_ELT(this, j));
    }

    UNPROTECT(2);

    return y;
}
