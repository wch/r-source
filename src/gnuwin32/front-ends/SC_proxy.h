/*
 *  StatConn: Connector interface between application and interpreter language
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

#ifndef _STATCONN_H_
#define _STATCONN_H_

#include "bdx.h"

// system-specifics should be moved to some include file
#include <windows.h>
#define SYSCALL WINAPI
#define EXPORT

// forward definition
struct _SC_Proxy_Object;

// error codes

// generic error and success codes
#define SC_PROXY_OK                            0x00000000
#define SC_PROXY_ERR_UNKNOWN                   0x80000000

#define SC_PROXY_ERR_INVALIDARG                0x80000001
#define SC_PROXY_ERR_INVALIDFORMAT             0x80000002
#define SC_PROXY_ERR_NOTIMPL                   0x80000003

// initialization and termination
#define SC_PROXY_ERR_INITIALIZED               0x80000004
#define SC_PROXY_ERR_NOTINITIALIZED            0x80000005

// evaluation, getting and setting symbols
#define SC_PROXY_ERR_INVALIDSYMBOL             0x80000006
#define SC_PROXY_ERR_PARSE_INVALID             0x80000007
#define SC_PROXY_ERR_PARSE_INCOMPLETE          0x80000008
#define SC_PROXY_ERR_UNSUPPORTEDTYPE           0x80000009

// version mismatch
#define SC_PROXY_ERR_INVALIDINTERFACEVERSION   0x80000010
#define SC_PROXY_ERR_INVALIDINTERPRETERVERSION 0x80000011

// function type-defs
typedef int (SYSCALL *SC_PROXY_GET_VERSION) (struct _SC_Proxy_Object* object,
					     unsigned long* version);
typedef int (SYSCALL *SC_PROXY_INIT) (struct _SC_Proxy_Object* object);
typedef int (SYSCALL *SC_PROXY_TERMINATE) (struct _SC_Proxy_Object* object);
typedef int (SYSCALL *SC_PROXY_RETAIN) (struct _SC_Proxy_Object* object);
typedef int (SYSCALL *SC_PROXY_RELEASE) (struct _SC_Proxy_Object* object);

typedef int (SYSCALL *SC_PROXY_SET_SYMBOL) (struct _SC_Proxy_Object* object,
					    char const* symbol,
					    BDX_Data /* const */ * data);
typedef int (SYSCALL *SC_PROXY_GET_SYMBOL) (struct _SC_Proxy_Object* object,
					    char const* symbol,
					    BDX_Data** data);
typedef int (SYSCALL *SC_PROXY_EVALUATE) (struct _SC_Proxy_Object* object,
					  char const* command,
					  BDX_Data** data);
typedef int (SYSCALL *SC_PROXY_EVALUATE_NORETURN) (struct _SC_Proxy_Object* object,
						   char const* command);
typedef int (SYSCALL *SC_PROXY_FREE_DATA_BUFFER) (struct _SC_Proxy_Object* object,
						  BDX_Data* data);
typedef int (SYSCALL *SC_PROXY_QUERY_TYPES) (struct _SC_Proxy_Object* object,
					     long* type_mask);
typedef int (SYSCALL *SC_PROXY_QUERY_OPS) (struct _SC_Proxy_Object* object,
					   long* op_mask);


/*
 * interface version information:
 *
 * 1 ... obsolete first interface
 * 2 ... get_version, set_symbol, evaluate_noreturn, evaluate
 */
#define SC_PROXY_INTERFACE_VERSION 2

// used for communication between the COM/CORBA server and R
typedef struct _SC_Proxy_Object_Vtbl
{
  // get interpreter version
  SC_PROXY_GET_VERSION get_version;

  // initialize the interpreter
  SC_PROXY_INIT init;
  // terminate the interpreter
  SC_PROXY_TERMINATE terminate;

  // increase the reference count to this interface object
  SC_PROXY_RELEASE retain;
  // decrease the reference count to this interface object, object will be
  // freed when count reaches 0
  SC_PROXY_RELEASE release;

  // set a symbol in the interpreter's global namespace to the BDX data passed
  SC_PROXY_SET_SYMBOL set_symbol;
  // return a symbol's value from the interpreter's global namespace
  SC_PROXY_GET_SYMBOL get_symbol;

  // evaluate an expression, return result in a BDX data buffer (synchronously)
  SC_PROXY_EVALUATE evaluate;
  // evaluate an expression, do not return a result (synchronously)
  SC_PROXY_EVALUATE_NORETURN evaluate_noreturn;

  // return information about BDX data types supported by this interface and
  // the interpreter
  SC_PROXY_QUERY_TYPES query_supported_types;
  // return information about available functionality in this interface and
  // the interpreter
  SC_PROXY_QUERY_OPS query_supported_operations;

  // free a BDX data buffer allocated by one of this interface's functions
  SC_PROXY_FREE_DATA_BUFFER free_data_buffer;
} SC_Proxy_Object_Vtbl;

// abstract data type (implementation adds data)
typedef struct _SC_Proxy_Object
{
  SC_Proxy_Object_Vtbl* vtbl;
} SC_Proxy_Object;

// entry point: retrieve a proxy object with a given version
typedef int (SYSCALL* SC_PROXY_GET_OBJECT) (SC_Proxy_Object**,unsigned long);

// this is valid for Windows only
#define SC_PROXY_GET_OBJECT_FUN "SC_Proxy_get_object@8"

#endif
