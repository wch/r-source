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

    lRc = lProxy->vtbl->evaluate (lProxy,"a<-5",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);
    lRc = lProxy->vtbl->evaluate (lProxy,"b<-7",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);
    lRc = lProxy->vtbl->evaluate (lProxy,"a+b",&lData);
    lRc = lProxy->vtbl->free_data_buffer (lProxy,lData);
  }
  printf("success\n");
  return 0;
}
