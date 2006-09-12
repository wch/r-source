/*******************************************************************************
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999--2006 Thomas Baier
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
 ******************************************************************************/

#ifndef _BDX_H_
#define _BDX_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "SC_system.h"

/* BDX: Binary Data eXchange Format */

/*
 * BDX version information
 *
 * 1 ... first version, never set
 * 2 ... removed magic, added version, changed data type defines
 * 3 ... added generic data type, special symbols (N/A, ERROR), objects
 */
#define BDX_VERSION 3

/*
 * Vtbl version information:
 *
 * 1 ... first interface (bdx_free, bdx_trace, Variant2BDX, BDX2Variant)
 */
#define BDX_VTBL_VERSION 1


#if 1
/*
 * very simple implementation, maybe use HDF5 in the future, or expand this
 * format (see rest of file)
 *
 * supported are scalars, arrays, vectors and lists
 *
 * scalar types:
 *
 *  - GENERIC (element can be any type; only supported for arrays of GENERIC)
 *  - BOOL
 *  - double
 *  - (long) integer
 *  - STRING
 *  - SPECIAL (element is a special value, like n/a or an error code)
 *
 * arrays of scalars (of any dimension), this includes GENERIC, too
 */

/* 04-03-02 | baier | BDX_LIST new, BDX_CMASK extended, removed BDX_VECTOR */
#define BDX_SCALAR 0x00010000
#define BDX_ARRAY  0x00020000
/* #define BDX_VECTOR 0x00040000 */
#define BDX_LIST   0x00080000
#define BDX_CMASK  0x000f0000

/* 04-03-02 | baier | BDX_GENERIC new, BDX_NULL changed to BDX_SPECIAL */
/* 04-11-16 | baier | BDX_HANDLE new (for COM objects), BDX_POINTER new */
#define BDX_GENERIC 0x00000000
#define BDX_BOOL    0x00000001
#define BDX_INT     0x00000002
#define BDX_DOUBLE  0x00000004
#define BDX_STRING  0x00000008
#define BDX_SPECIAL 0x00000010
#define BDX_HANDLE  0x00000020 /* COM object in BDX_RawData::ptr; marshalled
				  in an IStream using
				  CoMarshalInterThreadInterfaceInStream() */
#define BDX_POINTER 0x00000040 /* raw pointer in BDX_RawData::ptr; reserved. */
#define BDX_SMASK   0x0000007f

/* 04-03-02 | baier | special value definitions */
/* 04-10-15 | baier | added rest */
#define BDX_SV_NULL 0x00000000 /* null element */
#define BDX_SV_NA   0x00000001 /* missing value */
#define BDX_SV_DIV0 0x00000002 /* division by zero */
#define BDX_SV_NAN  0x00000003 /* NAN (not a number) */
#define BDX_SV_INF  0x00000004 /* +Inf */
#define BDX_SV_NINF 0x00000005 /* -Inf */
#define BDX_SV_UNK  0xffffffff /* unknown code */

typedef long BDX_Dimension;
typedef unsigned long BDX_Count;
typedef unsigned long BDX_Version;
typedef unsigned long BDX_Type;

typedef union _BDX_RawData
{
  unsigned long bool_value;
  double        double_value;
  long int      int_value;
  char*         string_value;
  unsigned long special_value;
  void*         ptr;
} BDX_RawData;

/* 04-03-02 | baier | new for transfer of non-typed arrays */
typedef struct _BDX_RawDataWithType
{
  BDX_Type    type;       /* data type of element (not only scalar!) */
  BDX_RawData raw_data;
} BDX_RawDataWithType;
typedef struct _BDX_NamedRawDataWithType
{
  BDX_Type    type;       /* data type of element (not only scalar!) */
  char*       name;       /* name for list elements */
  BDX_RawData raw_data;
} BDX_NamedRawDataWithType;

/* 04-03-02 | baier | BDX_Data reworked, now data union with diff. members */
typedef struct _BDX_Data
{
  BDX_Version    version;
  BDX_Type       type;
  BDX_Count      dim_count;
  BDX_Dimension* dimensions;
  /* @TODO: attributes */
  union {
    BDX_RawData*              raw_data;
    BDX_RawDataWithType*      raw_data_with_type;
    BDX_NamedRawDataWithType* named_raw_data_with_type;
  } data;
} BDX_Data;

/* function type-defs */
typedef void (SYSCALL *BDX_FREE) (struct _BDX_Data* bdx);
typedef void (SYSCALL *BDX_TRACE) (struct _BDX_Data* bdx);
typedef int (SYSCALL *BDX_VARIANT2BDX) (VARIANT var,struct _BDX_Data** bdx);
typedef int (SYSCALL *BDX_BDX2VARIANT) (struct _BDX_Data* bdx,VARIANT* var);

/* function table for BDX library */
typedef struct _BDX_Vtbl
{
  BDX_FREE bdx_free;
  BDX_TRACE bdx_trace;
  BDX_VARIANT2BDX Variant2BDX;
  BDX_BDX2VARIANT BDX2Variant;
} BDX_Vtbl;

/* entry point: retrieve a proxy object with a given version */
typedef int (SYSCALL* BDX_GET_VTBL) (BDX_Vtbl**,unsigned long);


#else

/*
 * data type mapping:
 *
 *   don't use an enum for this, because various compilers will interpret enums
 *   as different size
 *
 *   a data type (description) describes a data type, which is either
 *   - a scalar (of some supported type) or
 *   - a compound type (of scalars or compound types)
 *
 *   scalars:
 *     ...
 *
 *   first version:
 *
 *     - only scalars or
 *     - vectors (of any dimension) of scalars
 *
 *   are supported.
 *
 *   e.g. an array of boolean values has a type of
 *
 *     R_TYPE_COMPOUND_ARRAY | R_TYPE_SCALAR_BOOL
 */


/*
 * type specifications:
 *
 *   data types are either scalars (boolean, short integer, long integer,
 *   single precision floating point, double precision floating point,
 *   complex and string) or compound types (always reduced to scalars)
 */
#define BDX_TYPESPEC_SCALAR 0x00000001
#define BDX_TYPESPEC_ARRAY  0x00000002
/* more to come */

#define BDX_SCALAR_BOOL     0x00000001
/*#define BDX_SCALAR_SHORT    0x00000002 */
#define BDX_SCALAR_LONG     0x00000003
/*#define BDX_SCALAR_FLOAT    0x00000004 */
#define BDX_SCALAR_DOUBLE   0x00000005
/*#define BDX_SCALAR_COMPLEX  0x00000006 */
/*#define BDX_SCALAR_CHAR     0x00000007 */
/*#define BDX_SCALAR_STRING   0x00000008 */
/*#define BDX_SCALAR_SIGNED   0x80000000 */

/* mapping from BDX_SCALAR_* to C types */
typedef unsigned long BDX_Scalar_BOOL;
typedef long BDX_Scalar_LONG;
typedef double BDX_Scalar_DOUBLE;

/* some typedefs for commonly used types */
typedef unsigned long BDX_Length;
typedef unsigned long BDX_Offset;
typedef unsigned long BDX_Compound_Type;
typedef unsigned long BDX_Scalar_Specification;
typedef long BDX_Dimension;
typedef unsigned long BDX_Count;
typedef unsigned long BDX_Magic;

/* a type specification */
typedef struct _BDX_Type_Specification
{
  BDX_Compound_Type type;
  /*  BDX_Offset        type_declaration; */
} BDX_Type_Specification;

/* basic structure */
typedef struct _BDX_Data
{
  BDX_Magic              magic;
  BDX_Length             length;
  BDX_Type_Specification type_specification;
  /* depending on type_specification, either BDX_Array_Specificaton or */
  /* BDX_Scalar_Specification follows. After all type specifications, the */
  /* data follows */
} BDX_Data;


/* bounds for an array type specification */
typedef struct _BDX_Array_Bounds
{
  BDX_Dimension lower;
  BDX_Dimension upper;
} BDX_Array_Bounds;

/* the array type specification itself */
typedef struct _BDX_Array_Specification
{
  BDX_Count                dimensions;
  /*  BDX_Type_Specification base_type_specification; */
  /* simplificaton: the base type is a scalar type at the moment */
  BDX_Scalar_Specification base_type;
  BDX_Array_Bounds         bounds[1];
  /* after dimensions bounds array members, the rest of the type specification */
  /* follows. The the data... */
} BDX_Array_Specification;


/*
 * examples:
 *
 *   single scalar of type long memory layout:
 *
 *     BDX_Data
 *     {
 *       magic = ???
 *       length = sizeof (BDX_Data)
 *                + sizeof (BDX_Scalar_Specification);
 *                + sizeof (BDX_Scalar_LONG);
 *       type_specification = BDX_Type_Specification
 *       {
 *         type = BDX_TYPESPEC_SCALAR;
 *       }
 *     }
 *     BDX_Scalar_Specification (induced by BDX_TYPESPEC_SCALAR)
 *       = BDX_SCALAR_LONG;
 *     BDX_Scalar_LONG (induced by BDX_SCALAR_LONG)
 *       = ...; (data value)
 *
 *
 *   3-dimensional array, dimensions [2..4],[-5..3],[40,50] of doubles:
 *
 *     BDX_Data
 *     {
 *       magic = ???
 *       length = sizeof (BDX_Data)
 *                + sizeof (BDX_Array_Specification)
 *                + 2 * sizeof (BDX_Array_Bounds)
 *                + 3 * 9 * 11 * sizeof (BDX_Scalar_DOUBLE);
 *       type_specification = BDX_Type_Specification
 *       {
 *         type = BDX_TYPESPEC_ARRAY;
 *       }
 *     }
 *     BDX_Array_Specification (induced by BDX_TYPESPEC_ARRAY)
 *     {
 *       dimensions = 3;
 *       base_type = BDX_Scalar_DOUBLE;
 *       bounds[0]
 *       {
 *         lower = 2;
 *         upper = 4;
 *       }
 *       bounds[1]
 *       {
 *         lower = -5;
 *         upper = 3;
 *       }
 *       bounds[2]
 *       {
 *         lower = 40;
 *         upper = 50;
 *       }
 *     }
 *     BDX_Scalar_DOUBLE[3][9][11] data follows
 */
#endif

#ifdef __cplusplus
}
#endif

#endif
