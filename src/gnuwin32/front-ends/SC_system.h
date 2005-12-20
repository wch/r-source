/*******************************************************************************
 *  StatConn: Connector interface between application and interpreter language
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

#ifndef _SC_SYSTEM_H_
#define _SC_SYSTEM_H_

#ifdef __cplusplus
extern "C" {
#endif

/* system-specifics should be moved to some include file */
#include <windows.h>
#define SYSCALL WINAPI
#define EXPORT

/* entry points */
#define SC_PROXY_GET_OBJECT_FUN "SC_Proxy_get_object@8"
#define BDX_GET_VTBL_FUN "BDX_get_vtbl@8"


#ifdef __cplusplus
}
#endif

#endif
