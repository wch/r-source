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

/* A simple test program for the R proxy DLL: needs R.dll in the path */

#include <windows.h>
#include "rproxy.h"

#define RDLL "rproxy.dll"

int main (int argc, char** argv)
{
    HMODULE lModule = LoadLibrary (RDLL);
    R_PROXY_GET_OBJECT lFunc;
    R_Proxy_Object* lProxy;
    char lBuffer[1024];

    if (lModule == NULL) {
	DWORD lErr = GetLastError ();

	printf ("Error %d (%x) loading DLL %s\n", lErr, lErr, RDLL);
	exit (-1);
    }

    lFunc = (R_PROXY_GET_OBJECT) GetProcAddress (lModule, R_PROXY_GET_OBJECT_FUN);

    if (lFunc == NULL) {
	DWORD lErr = GetLastError ();

	printf ("Error %d (%x) getting function %s\n", lErr,
		lErr ,R_PROXY_GET_OBJECT_FUN);
	exit (-2);
    }

    printf ("Starting R\n");

    if (lFunc (&lProxy) != R_PROXY_OK) {
	printf ("Error getting proxy object\n");
	exit (-3);
    }

    printf ("INIT: %10x\n", lProxy->vtbl->init (lProxy));

    printf ("EVAL: a<-1:10 %10x\n", lProxy->vtbl->eval (lProxy,"a<-1:10"));
    printf ("EVAL: plot(a) %10x\n", lProxy->vtbl->eval (lProxy,"plot(a)"));

    do {
	printf ("EVAL>> ");
	gets (lBuffer);
	if (strlen (lBuffer) > 0) {
	    printf ("EVAL: %s: %10x\n", lBuffer, 
		    lProxy->vtbl->eval (lProxy, lBuffer));
	} else {
	    printf ("EVAL: quitting\n");
	}
	printf ("GET>> ");
	gets (lBuffer);
	if (strlen (lBuffer) > 0) {
	    BDX_Data* lData;
	    printf ("GET: %s: %10x\n", lBuffer, 
		    lProxy->vtbl->get_symbol (lProxy, lBuffer, &lData));
	    /* not freed! */
	} else {
	    printf ("EVAL: quitting\n");
	}
    }
    while (strlen (lBuffer) > 0);

    printf ("TERM: %10x\n", lProxy->vtbl->terminate (lProxy));
    printf ("RELEASE: %10x\n", lProxy->vtbl->release (lProxy));

    printf ("R done\n");

    return 0;
}
