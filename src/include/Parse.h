/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2005 R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef R_PARSE_H
#define R_PARSE_H

#include <R_ext/Parse.h>

	/* Parse A Single Expression */

SEXP R_Parse1File(FILE*, int, ParseStatus *);
SEXP R_Parse1Buffer(IoBuffer*, int, ParseStatus *);
SEXP R_Parse1Vector(TextBuffer*, int, ParseStatus *);
SEXP R_Parse1General(int (*)(), int (*)(), int, ParseStatus *);

	/* Parse Several Expressions */

SEXP R_ParseFile(FILE*, int, ParseStatus *);
SEXP R_ParseBuffer(IoBuffer*, int, ParseStatus *, SEXP);
/* SEXP R_ParseVector(SEXP, int, ParseStatus *); in R_ext/Parse.h */
SEXP R_ParseGeneral(int (*)(), int (*)(), int, ParseStatus *);


#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status);

#endif /* not R_PARSE_H */
