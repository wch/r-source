/*******************************************************************************
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 1999--2005 Thomas Baier
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

#ifndef _RPROXY_IMPL_H_
#define _RPROXY_IMPL_H_

#ifdef __cplusplus
extern "C" {
#endif

#include "bdx.h"

/* exported functions for implementation */

/* 00-02-18 | baier | init() now receives parameter-string */
int R_Proxy_init (char const* pParameters);
int R_Proxy_evaluate (char const* pCmd,BDX_Data** pData);
int R_Proxy_evaluate_noreturn (char const* pCmd);
int R_Proxy_get_symbol (char const* pSymbol,BDX_Data** pData);
int R_Proxy_set_symbol (char const* pSymbol,BDX_Data const* pData);
int R_Proxy_term ();

#ifdef __cplusplus
}
#endif

#endif
