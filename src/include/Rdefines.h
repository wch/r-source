/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2005 The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef R_DEFINES_H
#define R_DEFINES_H

#if !defined(R_R_H) && !defined(R_S_H)
/* user forget to include R.h or S.h */
#include <R_ext/Memory.h>
#include <R_ext/RS.h>
#endif

/*
 *  Much is from John Chambers' "Programming With Data".
 *  Some of this is from Doug Bates.
 *
 *  It is presented here to support a joint programming style which
 *  will work in both R and S.  In particular it helps with:
 *
 *    1. S/R <-> CORBA code.
 *    2. S/R <-> Java Code.
 *
 *  And to hide some internal nastiness.
 */

#include <Rinternals.h>

/*
 *  Added some macros defined in S.h from Splus 5.1
 */

#define NULL_USER_OBJECT	R_NilValue

#define AS_LOGICAL(x)		coerceVector(x,LGLSXP)
#define AS_INTEGER(x)		coerceVector(x,INTSXP)
#define AS_NUMERIC(x)		coerceVector(x,REALSXP)
#define AS_CHARACTER(x)		coerceVector(x,STRSXP)
#define AS_COMPLEX(x)		coerceVector(x,CPLXSXP)
#define AS_VECTOR(x)		coerceVector(x,VECSXP)
#define AS_LIST(x)		coerceVector(x,VECSXP)
#define AS_RAW(x)		coerceVector(x,RAWSXP)

#define IS_LOGICAL(x)		isLogical(x)
#define IS_INTEGER(x)		isInteger(x)
#define IS_NUMERIC(x)		isReal(x)
#define IS_CHARACTER(x)		isString(x)
#define IS_COMPLEX(x)		isComplex(x)
#define IS_VECTOR(x)		isVector(x)
#define IS_LIST(x)		IS_VECTOR(x)
#define IS_RAW(x)		(TYPEOF(x) == RAWSXP)

#define NEW_LOGICAL(n)		allocVector(LGLSXP,n)
#define NEW_INTEGER(n)		allocVector(INTSXP,n)
#define NEW_NUMERIC(n)		allocVector(REALSXP,n)
#define NEW_CHARACTER(n)	allocVector(STRSXP,n)
#define NEW_COMPLEX(n)		allocVector(CPLXSXP,n)
#define NEW_LIST(n)		allocVector(VECSXP,n)
#define NEW_STRING(n)		NEW_CHARACTER(n)
#define NEW_RAW(n)		allocVector(RAWSXP,n)

#define LOGICAL_POINTER(x)	LOGICAL(x)
#define INTEGER_POINTER(x)	INTEGER(x)
#define NUMERIC_POINTER(x)	REAL(x)
#define CHARACTER_POINTER(x)	STRING_PTR(x)
#define COMPLEX_POINTER(x)	COMPLEX(x)
#define LIST_POINTER(x)		VECTOR_PTR(x)
#define RAW_POINTER(x)		RAW(x)

/* The following are not defined in `Programming with Data' but are
   defined in S.h in Svr4 */

/*
 * Note that LIST_DATA and RAW_DATA are missing.
 * This is consistent with Svr4.
 */

#define LOGICAL_DATA(x)		(LOGICAL(x))
#define INTEGER_DATA(x)		(INTEGER(x))
#define DOUBLE_DATA(x)		(REAL(x))
#define NUMERIC_DATA(x)		(REAL(x))
#define CHARACTER_DATA(x)	(STRING_PTR(x))
#define COMPLEX_DATA(x)		(COMPLEX(x))
#define RECURSIVE_DATA(x)	(VECTOR_PTR(x))
#define VECTOR_DATA(x)		(VECTOR_PTR(x))

#define LOGICAL_VALUE(x)	asLogical(x)
#define INTEGER_VALUE(x)	asInteger(x)
#define NUMERIC_VALUE(x)	asReal(x)
#define CHARACTER_VALUE(x)	CHAR(asChar(x))
#define STRING_VALUE(x)		CHAR(asChar(x))
#define LIST_VALUE(x)		error("the `value' of a list object is not defined")
#define RAW_VALUE(x)		error("the `value' of a raw object is not defined")

#define SET_ELEMENT(x, i, val)	SET_VECTOR_ELT(x, i, val)
#define GET_ATTR(x,what)       	getAttrib(x, what)
#define GET_CLASS(x)       	getAttrib(x, R_ClassSymbol)
#define GET_DIM(x)       	getAttrib(x, R_DimSymbol)
#define GET_DIMNAMES(x)       	getAttrib(x, R_DimNamesSymbol)
#define GET_COLNAMES(x)       	GetColNames(x)
#define GET_ROWNAMES(x)       	GetRowNames(x)
#define GET_LEVELS(x)       	getAttrib(x, R_LevelsSymbol)
#define GET_TSP(x)       	getAttrib(x, R_TspSymbol)
#define GET_NAMES(x)		getAttrib(x, R_NamesSymbol)
#define SET_CLASS(x, n)     	setAttrib(x, R_ClassSymbol, n)
#define SET_DIM(x, n)     	setAttrib(x, R_DimSymbol, n)
#define SET_DIMNAMES(x, n)     	setAttrib(x, R_DimNamesSymbol, n)
#define SET_LEVELS(x, l)       	setAttrib(x, R_LevelsSymbol, l)
#define SET_NAMES(x, n)		setAttrib(x, R_NamesSymbol, n)
#define GET_LENGTH(x)		length(x)
#define SET_LENGTH(x, n)	(x = lengthgets(x, n))

#define GET_SLOT(x, what)       R_do_slot(x, what)
#define SET_SLOT(x, what, value)  R_do_slot_assign(x, what, value)

#define MAKE_CLASS(what)	R_do_MAKE_CLASS(what)
/* NEW_OBJECT is recommended; NEW is for green book compatibility */
#define NEW_OBJECT(class_def)		R_do_new_object(class_def)
#define NEW(class_def)		R_do_new_object(class_def)

#define s_object                SEXPREC
#define S_EVALUATOR             /**/
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define COPY_TO_USER_STRING(x)	mkChar(x)
#define CREATE_STRING_VECTOR(x)	mkChar(x)

#define CREATE_FUNCTION_CALL(name, argList) createFunctionCall(name, argList)

#define EVAL(x)			eval(x,R_GlobalEnv)


#endif
