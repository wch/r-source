/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2092--2012     The R Core Team
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
 *
 */

#include <Rinternals.h>
#include "tools.h"


extern int extR_HTTPDCreate(const char *ip, int port);
extern void extR_HTTPDStop(void);

SEXP startHTTPD(SEXP sIP, SEXP sPort)
{
    const char *ip = 0;
    if (sIP != R_NilValue && (TYPEOF(sIP) != STRSXP || LENGTH(sIP) != 1))
	error(_("invalid bind address specification"));
    if (sIP != R_NilValue) ip = CHAR(STRING_ELT(sIP, 0));
    return ScalarInteger(extR_HTTPDCreate(ip, asInteger(sPort)));
}

SEXP stopHTTPD(void)
{
    extR_HTTPDStop();
    return R_NilValue;
}
