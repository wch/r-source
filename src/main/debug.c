/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

SEXP do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP s;
	checkArity(op,args);

	if( TYPEOF(CAR(args)) == STRSXP ) {
		PROTECT(s=install(CHAR(STRING(CAR(args))[0])));
		CAR(args)= findFun(s, rho);
		UNPROTECT(1);
	}

	if(TYPEOF(CAR(args)) != CLOSXP )
		errorcall(call,"argument must be a function\n");

	switch(PRIMVAL(op)) {
		case 0:
			DEBUG(CAR(args))=1;
			break;
		case 1:
			DEBUG(CAR(args))=0;
			break;
	}
	return R_NilValue;
}

SEXP do_trace(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op,args);

	if(TYPEOF(CAR(args)) != CLOSXP && TYPEOF(CAR(args)) != BUILTINSXP &&
		TYPEOF(CAR(args)) != SPECIALSXP) 
		errorcall(call,"argument must be a function\n");

	switch(PRIMVAL(op)) {
		case 0:
			TRACE(CAR(args))=1;
			break;
		case 1:
			TRACE(CAR(args))=0;
			break;
	}
	return R_NilValue;
}

