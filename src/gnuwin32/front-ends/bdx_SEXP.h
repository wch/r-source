/*******************************************************************************
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999-2006 Thomas Baier
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 * 
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301, USA.
 *
 *  Conversion functions from SEXP to BDX and vice versa.
 *
 ******************************************************************************/

#ifndef _BDX_SEXP_H_
#define _BDX_SEXP_H_

#ifdef __cplusplus
extern "C" {
#endif

/* forward declaration */
struct SEXPREC;
struct _BDX_Data;

/*
 * BDX conversion functions:
 *
 *   These functions are used to convert to/from the BDX data format
 *   On success, the functions return 0, on error a negative number
 *
 * return codes:
 *
 *   0 ... success
 *  -1 ... BDX version mismatch
 *  -2 ... unsupported data types
 *  -3 ... error accessing memory/data
 *  -4 ... invalid argument to function
 *  -5 ... error coercing/marshalling data
 *  -6 ... empty/unknown symbol
 */
int BDX2SEXP(struct _BDX_Data const* pBDXData,struct SEXPREC** pSEXPData);
int SEXP2BDX(struct SEXPREC const* SEXPData,struct _BDX_Data** ppBDXData);

#ifdef __cplusplus
}
#endif

#endif
