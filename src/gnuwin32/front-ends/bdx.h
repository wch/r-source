/*
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999 Thomas Baier
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
 */

#ifndef _BDX_H_
#define _BDX_H_

// BDX: Binary Data eXchange Format

#if 1
/*
 * very simple implementation, maybe use HDF5 in the future, or expand this
 * format (see rest of file)
 *
 * supported are scalars and arrays of scalars
 *
 * scalar types:
 *
 *  - BOOL
 *  - double
 *  - (long) integer
 *  - STRING
 *  - NULL
 *
 * arrays of scalars (of any dimension)
 */

#define BDX_SCALAR 0x00010000
#define BDX_ARRAY  0x00020000
#define BDX_VECTOR 0x00030000
#define BDX_CMASK 0x00030000

#define BDX_BOOL   0x00000001
#define BDX_INT    0x00000002
#define BDX_DOUBLE 0x00000003
#define BDX_STRING 0x00000004
#define BDX_NULL   0x00000005
#define BDX_SMASK  0x00000007

typedef long BDX_Dimension;
typedef unsigned long BDX_Count;
typedef unsigned long BDX_Magic;
typedef unsigned long BDX_Type;

typedef union _BDX_RawData
{
  unsigned long bool_value;
  double        double_value;
  long int      int_value;
  char*         string_value;
} BDX_RawData;

typedef struct _BDX_Data
{
  BDX_Magic      magic;
  BDX_Type       type;
  BDX_Count      dim_count;
  BDX_Dimension* dimensions;
  BDX_RawData*   raw_data;
} BDX_Data;

void bdx_free (BDX_Data* data);

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
// more to come

#define BDX_SCALAR_BOOL     0x00000001
//#define BDX_SCALAR_SHORT    0x00000002
#define BDX_SCALAR_LONG     0x00000003
//#define BDX_SCALAR_FLOAT    0x00000004
#define BDX_SCALAR_DOUBLE   0x00000005
//#define BDX_SCALAR_COMPLEX  0x00000006
//#define BDX_SCALAR_CHAR     0x00000007
//#define BDX_SCALAR_STRING   0x00000008
//#define BDX_SCALAR_SIGNED   0x80000000

// mapping from BDX_SCALAR_* to C types
typedef unsigned long BDX_Scalar_BOOL;
typedef long BDX_Scalar_LONG;
typedef double BDX_Scalar_DOUBLE;

// some typedefs for commonly used types
typedef unsigned long BDX_Length;
typedef unsigned long BDX_Offset;
typedef unsigned long BDX_Compound_Type;
typedef unsigned long BDX_Scalar_Specification;
typedef long BDX_Dimension;
typedef unsigned long BDX_Count;
typedef unsigned long BDX_Magic;

// a type specification
typedef struct _BDX_Type_Specification
{
  BDX_Compound_Type type;
  //  BDX_Offset        type_declaration;
} BDX_Type_Specification;

// basic structure
typedef struct _BDX_Data
{
  BDX_Magic              magic;
  BDX_Length             length;
  BDX_Type_Specification type_specification;
  // depending on type_specification, either BDX_Array_Specificaton or 
  // BDX_Scalar_Specification follows. After all type specifications, the
  // data follows
} BDX_Data;


// bounds for an array type specification
typedef struct _BDX_Array_Bounds
{
  BDX_Dimension lower;
  BDX_Dimension upper;
} BDX_Array_Bounds;

// the array type specification itself
typedef struct _BDX_Array_Specification
{
  BDX_Count                dimensions;
  //  BDX_Type_Specification base_type_specification;
  // simplificaton: the base type is a scalar type at the moment
  BDX_Scalar_Specification base_type;
  BDX_Array_Bounds         bounds[1];
  // after dimensions bounds array members, the rest of the type specification
  // follows. The the data...
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

#endif
