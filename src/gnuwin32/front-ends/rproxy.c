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

#include <windows.h>
#include <stdio.h>
#include "Rconfig.h"
#include "Rversion.h"
#include "bdx.h"
#include "rproxy.h"
#include "rproxy_impl.h"
#include <assert.h>
#include <stdlib.h>

/* magic number for data structure */
#define R_BDX_MAGIC 0x8a35df01

typedef enum
{
    ps_none,
    ps_initialized
} R_Proxy_Object_State;

typedef struct _R_Proxy_Object_Impl
{
    R_Proxy_Object_Vtbl* vtbl;
    R_Proxy_Object_State state;
} R_Proxy_Object_Impl;

int SYSCALL R_init (R_Proxy_Object_Impl* object)
{
    int lRc = R_PROXY_ERR_UNKNOWN;

    if (object == NULL) 
	return R_PROXY_ERR_INVALIDARG;
    if (object->state != ps_none) 
	return R_PROXY_ERR_INITIALIZED;
    lRc = R_Proxy_init ();
    if (lRc == R_PROXY_OK) 
	object->state = ps_initialized;
    return lRc;
}

int SYSCALL R_terminate (R_Proxy_Object_Impl* object)
{
    int lRc = R_PROXY_ERR_UNKNOWN;

    if (object == NULL) 
	return R_PROXY_ERR_INVALIDARG;
    if (object->state != ps_initialized) 
	return R_PROXY_ERR_NOTINITIALIZED;
    lRc = R_Proxy_term ();
    if (lRc == R_PROXY_OK) 
	object->state = ps_none;
    return lRc;
}

int SYSCALL R_release (R_Proxy_Object_Impl* object)
{
    if (object == NULL) 
	return R_PROXY_ERR_INVALIDARG;
    if (object->state != ps_none) 
	return R_PROXY_ERR_INITIALIZED;
    free (object);
    return R_PROXY_OK;
}

int SYSCALL R_set_symbol (R_Proxy_Object_Impl* object,
			  char const* symbol,
			  BDX_Data* data)
{
    /* check parameters */
    if ((object == NULL) || (symbol == NULL) || 
	(strlen (symbol) == 0) || (data == NULL))
	return R_PROXY_ERR_INVALIDARG;
    if (data->magic != R_BDX_MAGIC)
	return R_PROXY_ERR_INVALIDFORMAT;
    return R_PROXY_ERR_NOTIMPL;
}

int SYSCALL R_get_symbol (R_Proxy_Object_Impl* object,
			  char const* symbol,
			  BDX_Data** data)
{
  int lRc = 0;

  /* check parameters */
  if ((object == NULL) || (symbol == NULL) || 
      (strlen (symbol) == 0) || (data == NULL))
      return R_PROXY_ERR_INVALIDARG;
  lRc = R_Proxy_get_symbol (symbol,data);
  if (lRc == R_PROXY_OK)
      (*data)->magic = R_BDX_MAGIC;
  return lRc;
}

int SYSCALL R_eval (R_Proxy_Object_Impl* object, char const* command)
{
    if (object == NULL) 
	return R_PROXY_ERR_INVALIDARG;
    if (object->state != ps_initialized) 
	return R_PROXY_ERR_NOTINITIALIZED;
    return R_Proxy_eval (command);
}

int SYSCALL R_free_data_buffer (R_Proxy_Object_Impl* object,
				BDX_Data* data)
{
    if ((data == NULL) || (object == NULL))
	return R_PROXY_ERR_INVALIDARG;
    if (data->magic != R_BDX_MAGIC)
	return R_PROXY_ERR_INVALIDFORMAT;
    assert (data != NULL);
    assert (data->magic == R_BDX_MAGIC);
    bdx_free (data);
    return R_PROXY_OK;
}


/* global object table */
R_Proxy_Object_Vtbl global_proxy_object_vtbl =
{
    (R_PROXY_INIT) R_init,
    (R_PROXY_TERMINATE) R_terminate,
    (R_PROXY_RELEASE) R_release,
    (R_PROXY_SET_SYMBOL) R_set_symbol,
    (R_PROXY_GET_SYMBOL) R_get_symbol,
    (R_PROXY_EVAL) R_eval,
    (R_PROXY_FREE_DATA_BUFFER) R_free_data_buffer
};

int SYSCALL EXPORT R_Proxy_get_object (R_Proxy_Object** obj)
{
    R_Proxy_Object_Impl* proxy_object = NULL;
  
    if (obj == NULL) 
	return R_PROXY_ERR_INVALIDARG;
    proxy_object = (R_Proxy_Object_Impl*) 
	malloc (sizeof (R_Proxy_Object_Impl));
    proxy_object->vtbl = &global_proxy_object_vtbl;
    proxy_object->state = ps_none;
    *obj = (R_Proxy_Object*) proxy_object;
    return R_PROXY_OK;
}
