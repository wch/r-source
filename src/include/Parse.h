/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-1999 R Development Core Team
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

#define PARSE_NULL		0
#define PARSE_OK		1
#define PARSE_INCOMPLETE	2
#define PARSE_ERROR		3
#define PARSE_EOF		4

#define INPUT_CONSOLE		1
#define INPUT_TEXT		2
#define INPUT_FILE		3

	/* Parse A Single Expression */

SEXP R_Parse1File(FILE*, int, int*);
SEXP R_Parse1Buffer(IoBuffer*, int, int*);
SEXP R_Parse1Vector(TextBuffer*, int, int *);
SEXP R_Parse1General(int (*)(), int (*)(), int, int *);

	/* Parse Several Expressions */

SEXP R_ParseFile(FILE*, int, int*);
SEXP R_ParseBuffer(IoBuffer*, int, int*, SEXP);
SEXP R_ParseVector(SEXP, int, int *);
SEXP R_ParseGeneral(int (*)(), int (*)(), int, int *);
