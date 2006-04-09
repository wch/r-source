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
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301, USA.
 *
 *  helper functions for BDX (e.g. memory management)
 *
 ******************************************************************************/

#ifndef _BDX_UTIL_H_
#define _BDX_UTIL_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <windows.h>

/* tracing */
#ifdef _IN_RPROXY_
#include "rproxy.h"
#define BDX_TRACE RPROXY_TRACE
#define BDX_ERR RPROXY_ERR
#else
#define BDX_TRACE(x) bdx_trace_ ## x
#define BDX_ERR(x)   bdx_trace_ ## x
int bdx_trace_printf(char const*,...);
#endif

/* forward declaration */
struct _BDX_Data;

/*
 * allocate/release the BDX memory
 */
struct _BDX_Data* WINAPI bdx_alloc();
void WINAPI bdx_free(struct _BDX_Data* data);
void WINAPI bdx_trace(struct _BDX_Data* data); /* trace using OutputDebugString() */

#ifdef __cplusplus
}
#endif

#endif
