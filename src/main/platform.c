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

SEXP do_showfile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn;
    checkArity(op, args);
    fn = CAR(args);
    if (!isString(fn) || length(fn) < 1 || STRING(fn)[0] == R_NilValue)
	errorcall(call, "invalid filename\n");
    R_ShowFile(CHAR(STRING(fn)[0]));
    return R_NilValue;
}

SEXP do_appendfile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP fn1, fn2;
    checkArity(op, args);
    fn1 = CAR(args);
    if (!isString(fn1) || length(fn1) < 1 || STRING(fn1)[0] == R_NilValue)
        errorcall(call, "invalid filename\n");
    if (!isString(fn2) || length(fn2) < 1 || STRING(fn2)[0] == R_NilValue)
        errorcall(call, "invalid filename\n");
    R_AppendFile(CHAR(STRING(fn1)[0]), CHAR(STRING(fn2)[0]));
    return R_NilValue;
}
