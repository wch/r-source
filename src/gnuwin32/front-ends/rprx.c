/*
 *  RProxy: Connector implementation between application and R language
 *  Copyright (C) 1999 Thomas Baier
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
 *
 *  $Id: rprx.c,v 1.2.4.1 1999/12/09 16:47:17 ripley Exp $
 */

// simple test application w/o a COM server
#include <windows.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "SC_proxy.h"

#define RDLL "rproxy.dll"

int main ()
{
  SC_Proxy_Object* lProxy = 0;
  HMODULE lModule = 0;
  SC_PROXY_GET_OBJECT lFunc = 0;
  ULONG lRc;

  if (getenv ("R_HOME")) { // BR
    char DLLlocation[MAX_PATH];
    strcpy(DLLlocation, getenv("R_HOME"));
    strcat(DLLlocation, "\\bin\\");
    strcat(DLLlocation, RDLL);
    lModule = LoadLibraryEx (DLLlocation, NULL,
			     LOAD_WITH_ALTERED_SEARCH_PATH);
  } else {
    lModule = LoadLibrary (RDLL);
  }

  lFunc = (SC_PROXY_GET_OBJECT) GetProcAddress (lModule,SC_PROXY_GET_OBJECT_FUN);

  lRc = lFunc (&lProxy,2);

  lRc = lProxy->vtbl->init (lProxy);

  {
    BDX_Data* lData = 0;

    lRc = lProxy->vtbl->get_symbol (lProxy,"abcxyz",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);

    lRc = lProxy->vtbl->evaluate (lProxy,"a<-5",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);
    lRc = lProxy->vtbl->evaluate (lProxy,"b<-7",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);
    lRc = lProxy->vtbl->evaluate (lProxy,"a+b",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);

    lRc = lProxy->vtbl->get_symbol (lProxy,"a",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);

    lRc = lProxy->vtbl->get_symbol (lProxy,"x",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);

    // set a vector of strings
    {
#define DIMENSIONS 1
#define SIZE 10
      unsigned int i;
      BDX_Data* lData = (BDX_Data*) malloc (sizeof (BDX_Data));

      // vector
      lData->type = BDX_VECTOR;
      lData->dim_count = DIMENSIONS;
      lData->dimensions =
	(BDX_Dimension*) malloc (sizeof (BDX_Dimension) * lData->dim_count);
      lData->dimensions[0] = SIZE;
      lData->raw_data =
	(BDX_RawData*) malloc (sizeof (BDX_RawData) * SIZE * DIMENSIONS);
      lData->type |= BDX_STRING;

      // copy the data
      for (i = 0;i < DIMENSIONS*SIZE;i++)
	{
	  char tmpbuffer[200];
	  sprintf (tmpbuffer,"String %d",i);
	  lData->raw_data[i].string_value = strdup (tmpbuffer);
	}
      
      lData->version = BDX_VERSION; // will be removed

      // set data to R
      lRc = lProxy->vtbl->set_symbol (lProxy,"x",lData);

      // should free now
#undef SIZE
#undef DIMENSIONS
    }
    lRc = lProxy->vtbl->get_symbol (lProxy,"x",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);

  }
  printf("success\n");
  return 0;
}
