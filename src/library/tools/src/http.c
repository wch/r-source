/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2092--2024     The R Core Team
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
    int port = asInteger(sPort);
    if (port < 0 || port > 65535)
	error(
	    _("Invalid port number %d: should be in 0:65535, typically above 1024"),
	    port);
    return ScalarInteger(extR_HTTPDCreate(ip, port));
}

SEXP stopHTTPD(void)
{
    extR_HTTPDStop();
    return R_NilValue;
}
