/*
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 1999--2001 Thomas Baier
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
 *  $Id: rproxy.c,v 1.10 2001/12/10 09:35:34 ripley Exp $
 */

#define NONAMELESSUNION
#include <windows.h>
#include <stdio.h>
#include "config.h"
#include "Rversion.h"
#include "bdx.h"
#include "SC_proxy.h"
#include "rproxy_impl.h"
#include <assert.h>
#include <stdlib.h>

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>

// static connector information
#define CONNECTOR_NAME          "R Statistics Interpreter Connector"
#define CONNECTOR_DESCRIPTION   "Implements abstract connector interface to R"
#define CONNECTOR_COPYRIGHT     "(C) 1999-2001, Thomas Baier"
#define CONNECTOR_LICENSE       "GNU General Public License version 2 or greater"
#define CONNECTOR_VERSION_MAJOR "1"
#define CONNECTOR_VERSION_MINOR "0"

// interpreter information here at the moment until I know better...
#define INTERPRETER_NAME        "R"
#define INTERPRETER_DESCRIPTION "A Computer Language for Statistical Data Analysis"
#define INTERPRETER_COPYRIGHT   "(C) R Development Core Team"
#define INTERPRETER_LICENSE     "GNU General Public License version 2 or greater"

typedef enum
{
  ps_none,
  ps_initialized
} R_Proxy_Object_State;

SC_CharacterDevice* __output_device;
SC_GraphicsDevice* __graphics_device;

typedef struct _R_Proxy_Object_Impl
{
  SC_Proxy_Object_Vtbl* vtbl;
  R_Proxy_Object_State state;
  int                   ref_count;
} R_Proxy_Object_Impl;

int SYSCALL R_get_version (R_Proxy_Object_Impl* object,unsigned long* version)
{
  if ((object == NULL)
      || (version == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  *version = SC_PROXY_INTERFACE_VERSION;

  return SC_PROXY_OK;
}

// 00-02-18 | baier | R_init(), R_Proxy_init() now take parameter-string
int SYSCALL R_init (R_Proxy_Object_Impl* object,char const* parameters)
{
  int lRc = SC_PROXY_ERR_UNKNOWN;

  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }
  
  if (object->state != ps_none)
    {
      return SC_PROXY_ERR_INITIALIZED;
    }

  lRc = R_Proxy_init (parameters);

  if (lRc == SC_PROXY_OK)
    {
      object->state = ps_initialized;
    }

  return lRc;
}

int SYSCALL R_terminate (R_Proxy_Object_Impl* object)
{
  int lRc = SC_PROXY_ERR_UNKNOWN;

  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }
  
  if (object->state != ps_initialized)
    {
      return SC_PROXY_ERR_NOTINITIALIZED;
    }

  lRc = R_Proxy_term ();

  if (lRc == SC_PROXY_OK)
    {
      object->state = ps_none;
    }

  return lRc;
}

int SYSCALL R_retain (R_Proxy_Object_Impl* object)
{
  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  assert (object->ref_count > 0);

  (object->ref_count)++;

  return SC_PROXY_OK;
}

// 00-06-19 | baier | release graphics device
int SYSCALL R_release (R_Proxy_Object_Impl* object)
{
  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  // reference count must not be 0 here
  assert (object->ref_count > 0);

  (object->ref_count)--;

  if (object->ref_count > 0)
    {
      return SC_PROXY_OK;
    }

  if (object->state != ps_none)
    {
      return SC_PROXY_ERR_INITIALIZED;
    }
  
  if (__output_device)
    {
      __output_device->vtbl->release (__output_device);
      __output_device = NULL;
    }

  if (__graphics_device)
    {
      __graphics_device->vtbl->release (__graphics_device);
      __graphics_device = NULL;
    }

  free (object);
  
  return SC_PROXY_OK;
}

int SYSCALL R_set_symbol (R_Proxy_Object_Impl* object,
			  char const* symbol,
			  BDX_Data* data)
{
  int lRc = 0;

  // check parameters
  if ((object == NULL)
      || (symbol == NULL)
      || (strlen (symbol) == 0)
      || (data == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }
  
  if (data->version != BDX_VERSION)
    {
      return SC_PROXY_ERR_INVALIDFORMAT;
    }
  
  lRc = R_Proxy_set_symbol (symbol,data);

  return lRc;
}

int SYSCALL R_get_symbol (R_Proxy_Object_Impl* object,
			  char const* symbol,
			  BDX_Data** data)
{
  int lRc = 0;

  // check parameters
  if ((object == NULL)
      || (symbol == NULL)
      || (strlen (symbol) == 0)
      || (data == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  lRc = R_Proxy_get_symbol (symbol,data);

  if (lRc == SC_PROXY_OK)
    {
      (*data)->version = BDX_VERSION;
    }

  return lRc;
}

int SYSCALL R_evaluate (R_Proxy_Object_Impl* object,
			char const* command,
			BDX_Data** data )
{
  if ((object == NULL)
      || (command == NULL)
      || (strlen (command) == 0)
      || (data == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  if (object->state != ps_initialized)
    {
      return SC_PROXY_ERR_NOTINITIALIZED;
    }
  
  return R_Proxy_evaluate (command,data);
}

int SYSCALL R_evaluate_noreturn (R_Proxy_Object_Impl* object,
				 char const* command)
{
  if ((object == NULL)
      || (command == NULL)
      || (strlen (command) == 0))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  if (object->state != ps_initialized)
    {
      return SC_PROXY_ERR_NOTINITIALIZED;
    }
  
  return R_Proxy_evaluate_noreturn (command);
}


int SYSCALL R_query_types (R_Proxy_Object_Impl* object,
			   long* type_mask)
{
  if ((object == NULL)
      || (type_mask == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  *type_mask = (SC_TM_SCALAR_ALL | SC_TM_ARRAY_ALL | SC_TM_VECTOR_ALL);

  return SC_PROXY_OK;
}


int SYSCALL R_query_ops (R_Proxy_Object_Impl* object,
			 long* op_mask)
{
  if ((object == NULL)
      || (op_mask == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  *op_mask = 0;

  return SC_PROXY_ERR_NOTIMPL;
}

int SYSCALL R_free_data_buffer (R_Proxy_Object_Impl* object,
				BDX_Data* data)
{
  if ((data == NULL)
      || (object == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  if (data->version != BDX_VERSION)
    {
      return SC_PROXY_ERR_INVALIDFORMAT;
    }
  
  assert (data != NULL);
  assert (data->version == BDX_VERSION);

  bdx_free (data);
  //  free (data);

  return SC_PROXY_OK;
}

// 00-06-19 | baier | only set if version matches
int SYSCALL R_set_output_device (R_Proxy_Object_Impl* object,
				 struct _SC_CharacterDevice* device)
{
  unsigned long lCurrentVersion = 0;

  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  if (__output_device)
    {
      __output_device->vtbl->release (__output_device);
      __output_device = NULL;
    }

  if (device == NULL)
    {
      return SC_PROXY_OK;
    }

  if (device->vtbl->get_version (device,
				 &lCurrentVersion) != SC_PROXY_OK)
    {
      return SC_PROXY_ERR_UNKNOWN;
    }

  if (lCurrentVersion != SC_CHARACTERDEVICE_VERSION)
    {
      return SC_PROXY_ERR_INVALIDINTERFACEVERSION;
    }

  __output_device = device;
  __output_device->vtbl->retain (device);

  return SC_PROXY_OK;
}

int SYSCALL R_query_info (R_Proxy_Object_Impl* object,
			  long main_key,
			  long sub_key,
			  char const** information)
{
  if ((object == NULL)
      || (information == NULL))
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  switch (main_key)
    {
    case SC_INFO_MAIN_CONNECTOR:
      switch (sub_key)
	{
	case SC_INFO_SUB_NAME:
	  *information = INTERPRETER_NAME;
	  break;
	case SC_INFO_SUB_DESCRIPTION:
	  *information = INTERPRETER_DESCRIPTION;
	  break;
	case SC_INFO_SUB_COPYRIGHT:
	  *information = INTERPRETER_COPYRIGHT;
	  break;
	case SC_INFO_SUB_LICENSE:
	  *information = INTERPRETER_LICENSE;
	  break;
	case SC_INFO_SUB_MINORVERSION:
	  *information = R_MINOR;
	  break;
	case SC_INFO_SUB_MAJORVERSION:
	  *information = R_MAJOR;
	  break;
	default:
	  *information = "";
	}
      break;
    case SC_INFO_MAIN_INTERPRETER:
      switch (sub_key)
	{
	case SC_INFO_SUB_NAME:
	  *information = CONNECTOR_NAME;
	  break;
	case SC_INFO_SUB_DESCRIPTION:
	  *information = CONNECTOR_DESCRIPTION;
	  break;
	case SC_INFO_SUB_COPYRIGHT:
	  *information = CONNECTOR_COPYRIGHT;
	  break;
	case SC_INFO_SUB_LICENSE:
	  *information = CONNECTOR_LICENSE;
	  break;
	case SC_INFO_SUB_MINORVERSION:
	  *information = CONNECTOR_VERSION_MINOR;
	  break;
	case SC_INFO_SUB_MAJORVERSION:
	  *information = CONNECTOR_VERSION_MAJOR;
	  break;
	default:
	  *information = "";
	}
      break;
    default:
      *information = "";
    }

  return SC_PROXY_OK;
}
// 01-01-25 | baier | new parameters
int R_Proxy_Graphics_Driver (NewDevDesc* pDD,
			     char* pDisplay,
			     double pWidth,
			     double pHeight,
			     double pPointSize,
			     Rboolean pRecording,
			     int pResize,
			     struct _SC_GraphicsDevice* pDevice);


int SYSCALL R_set_graphics_device (struct _SC_Proxy_Object* object,
				   struct _SC_GraphicsDevice* device)
{
  unsigned long lCurrentVersion = 0;

  if (object == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }

  if (__graphics_device)
    {
      // remove the graphics device from the set of drivers
      // @TB
      __graphics_device->vtbl->release (__graphics_device);
      __graphics_device = NULL;
    }

  if (device == NULL)
    {
      return SC_PROXY_OK;
    }

  if (device->vtbl->get_version (device,
				 &lCurrentVersion) != SC_PROXY_OK)
    {
      return SC_PROXY_ERR_UNKNOWN;
    }

  if (lCurrentVersion != SC_GRAPHICSDEVICE_VERSION)
    {
      return SC_PROXY_ERR_INVALIDINTERFACEVERSION;
    }

  __graphics_device = device;
  __graphics_device->vtbl->retain (device);

  // add the graphics device to the set of drivers
  {
    NewDevDesc* lDev = (NewDevDesc*) calloc (1, sizeof (NewDevDesc));
    GEDevDesc *lDD;

    /* Do this for early redraw attempts */
    lDev->displayList = R_NilValue;
    R_Proxy_Graphics_Driver (lDev,
			     "ActiveXDevice 1",
			     100.0,
			     100.0,
			     10.0,
			     0,
			     0,
			     device);
    gsetVar(install(".Device"),
	    mkString("ActiveXDevice 1"), R_NilValue);
    lDD = GEcreateDevDesc(lDev);
    addDevice((DevDesc*) lDD);
    GEinitDisplayList(lDD);
  }
  return SC_PROXY_OK;
}

// global object table
SC_Proxy_Object_Vtbl global_proxy_object_vtbl =
{
  (SC_PROXY_GET_VERSION) R_get_version,
  (SC_PROXY_INIT) R_init,
  (SC_PROXY_TERMINATE) R_terminate,
  (SC_PROXY_RETAIN) R_retain,
  (SC_PROXY_RELEASE) R_release,
  (SC_PROXY_SET_SYMBOL) R_set_symbol,
  (SC_PROXY_GET_SYMBOL) R_get_symbol,
  (SC_PROXY_EVALUATE) R_evaluate,
  (SC_PROXY_EVALUATE_NORETURN) R_evaluate_noreturn,
  (SC_PROXY_QUERY_TYPES) R_query_types,
  (SC_PROXY_QUERY_OPS) R_query_ops,
  (SC_PROXY_FREE_DATA_BUFFER) R_free_data_buffer,
  (SC_PROXY_SET_CHARACTERDEVICE) R_set_output_device,
  (SC_PROXY_QUERY_INFO) R_query_info,
  (SC_PROXY_SET_GRAPHICSDEVICE) R_set_graphics_device
};

int SYSCALL EXPORT SC_Proxy_get_object (SC_Proxy_Object** obj,
					unsigned long version)
{
  R_Proxy_Object_Impl* proxy_object = NULL;

  if (obj == NULL)
    {
      return SC_PROXY_ERR_INVALIDARG;
    }
  
  if (version != SC_PROXY_INTERFACE_VERSION)
    {
      return SC_PROXY_ERR_INVALIDINTERFACEVERSION;
    }

  proxy_object = (R_Proxy_Object_Impl*) malloc (sizeof (R_Proxy_Object_Impl));
  
  proxy_object->vtbl = &global_proxy_object_vtbl;
  proxy_object->state = ps_none;
  proxy_object->ref_count = 1;

  *obj = (SC_Proxy_Object*) proxy_object;

  return SC_PROXY_OK;
}
