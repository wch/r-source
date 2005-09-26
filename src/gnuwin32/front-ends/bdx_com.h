/*******************************************************************************
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999-2005 Thomas Baier
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
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 *  Conversion functions from VARIANT to BDX and vice versa.
 *
 ******************************************************************************/

#ifndef _BDX_COM_H_
#define _BDX_COM_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <ole2.h>

/* forward declaration */
struct _BDX_Data;

/*
 * BSTR helpers: return NULL on error. Caller must free allocated string
 */
BSTR ANSI2BSTR(char const* pStr);
char* BSTR2ANSI(BSTR pBSTR);
OLECHAR* com_getOLECHAR(char const* str);

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
 */
int WINAPI BDX2Variant(struct _BDX_Data* pBDXData,VARIANT* pVariantData);
int WINAPI Variant2BDX(VARIANT VariantData,struct _BDX_Data** pBDXData);

#ifdef __cplusplus
}
#endif

#endif
