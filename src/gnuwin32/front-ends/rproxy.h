/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999  R Development Core Team
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

#ifndef _RPROXY_H_
#define _RPROXY_H_

#include "bdx.h"

/* system-specifics should be moved to some include file */
#include <windows.h>
#define SYSCALL WINAPI
#define EXPORT

/* forward definition */
struct _R_Proxy_Object;

/* error codes */

/* generic error and success codes */
#define R_PROXY_OK                 0x00000000
#define R_PROXY_ERR_UNKNOWN        0x80000000

#define R_PROXY_ERR_INVALIDARG     0x80000001
#define R_PROXY_ERR_INVALIDFORMAT  0x80000002
#define R_PROXY_ERR_NOTIMPL        0x80000003

/* initialization and termination */
#define R_PROXY_ERR_INITIALIZED    0x80000004
#define R_PROXY_ERR_NOTINITIALIZED 0x80000005

/* evaluation, getting and setting symbols */
#define R_PROXY_ERR_INVALIDSYMBOL      0x80000006
#define R_PROXY_ERR_PARSE_INVALID      0x80000007
#define R_PROXY_ERR_PARSE_INCOMPLETE   0x80000008
#define R_PROXY_ERR_UNSUPPORTEDTYPE    0x80000009

/* function type-defs */
typedef int (SYSCALL *R_PROXY_INIT) (struct _R_Proxy_Object* object);
typedef int (SYSCALL *R_PROXY_TERMINATE) (struct _R_Proxy_Object* object);
typedef int (SYSCALL *R_PROXY_RELEASE) (struct _R_Proxy_Object* object);

typedef int (SYSCALL *R_PROXY_SET_SYMBOL) (struct _R_Proxy_Object* object,
					   char const* symbol,
					   BDX_Data /* const */ * data);
typedef int (SYSCALL *R_PROXY_GET_SYMBOL) (struct _R_Proxy_Object* object,
					   char const* symbol,
					   BDX_Data** data);
typedef int (SYSCALL *R_PROXY_EVAL) (struct _R_Proxy_Object* object,
				     char const* command);
typedef int (SYSCALL *R_PROXY_FREE_DATA_BUFFER) (struct _R_Proxy_Object* object,
						 BDX_Data* data);


/* used for communication between the COM/CORBA server and R */
typedef struct _R_Proxy_Object_Vtbl
{
  R_PROXY_INIT init;
  R_PROXY_TERMINATE terminate;
  R_PROXY_RELEASE release;
  R_PROXY_SET_SYMBOL set_symbol;
  R_PROXY_GET_SYMBOL get_symbol;
  R_PROXY_EVAL eval;
  R_PROXY_FREE_DATA_BUFFER free_data_buffer;
} R_Proxy_Object_Vtbl;

/* abstract data type (implementation adds data) */
typedef struct _R_Proxy_Object
{
  R_Proxy_Object_Vtbl* vtbl;
} R_Proxy_Object;

/* entry point */
typedef int (SYSCALL* R_PROXY_GET_OBJECT) (R_Proxy_Object**);

/* this is valid for Windows only */
#define R_PROXY_GET_OBJECT_FUN "R_Proxy_get_object@4"

#endif
