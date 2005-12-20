/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2003 R Development Core Team
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

/* NOTE:
   This file exports a part of the current internal parse interface.
   It is subject to change at any minor (x.y.0) version of R.
 */

#ifndef R_EXT_PARSE_H_
#define R_EXT_PARSE_H_

#ifdef __cplusplus
extern "C" {
#endif

/*
#define PARSE_NULL		0
#define PARSE_OK		1
#define PARSE_INCOMPLETE	2
#define PARSE_ERROR		3
#define PARSE_EOF		4
*/

typedef enum {
    PARSE_NULL,
    PARSE_OK,
    PARSE_INCOMPLETE,
    PARSE_ERROR,
    PARSE_EOF
} ParseStatus;

SEXP R_ParseVector(SEXP, int, ParseStatus *);

#ifdef __cplusplus
}
#endif

#endif
