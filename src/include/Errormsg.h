/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *                2000        the R Development Core Team
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


	/*-- Error Message Strings --- 1st step to i18n --- --*/

#ifdef IEEE_754
# define R_MSG_NA		"NaNs produced"
#else
# define R_MSG_NA		"NAs produced"
#endif

#define R_MSG_NONNUM_MATH 	"Non-numeric argument to mathematical function"

#define R_MSG_IA   "invalid argument"

#define R_MSG_char "a character string"
#define R_MSG_list "a list"
#define R_MSG_A1   "first argument"
#define R_MSG_A2   "second argument"

#define R_MSG_must "must be"
#define R_MSG_A1_char R_MSG_A1 " " R_MSG_must " " R_MSG_char
#define R_MSG_A1_list R_MSG_A1 " " R_MSG_must " " R_MSG_list
#define R_MSG_A2_char R_MSG_A2 " " R_MSG_must " " R_MSG_char
#define R_MSG_A2_list R_MSG_A2 " " R_MSG_must " " R_MSG_list

#define R_MSG_subs_o_b	"subscript out of bounds"
#define R_MSG_ob_nonsub	"object is not subsettable"
#define R_MSG_mode	"invalid \"mode\" of argument"
#define R_MSG_list_vec	"applies only to lists and vectors"
#define R_MSG_list_vec2	"() applied to non-(list or vector)"


	/*-- Packaged Error Messages ---> Handling & I18n ... */

/* Argument list length and type errors */

#define ERROR_NUMARGS		1
#define ERROR_ARGTYPE		2
#define ERROR_INCOMPAT_ARGS	3

/* General type and length incompatibilities */

#define ERROR_TSVEC_MISMATCH	100

#define ERROR_UNIMPLEMENTED	9998
#define ERROR_UNKNOWN		9999


	/*-- Packaged Warning Messages --*/

#define WARNING_UNKNOWN		9999

#endif
