/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef ERRORMSG_H
#define ERRORMSG_H

#include "Error.h"

	/* Packaged Error Messages */
	/* Argument list length and type errors */

#define ERROR_NUMARGS		1
#define ERROR_ARGTYPE		2
#define ERROR_INCOMPAT_ARGS	3

	/* General type and length incompatibilities */

#define ERROR_TSVEC_MISMATCH	100

#define ERROR_UNIMPLEMENTED	9998
#define ERROR_UNKNOWN		9999



	/* Packaged Warning Messages */

#define WARNING_UNKNOWN		9999

#endif
