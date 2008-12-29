/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-8  R Development Core Team
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
 *  along with this program; if not,  a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>		/* for exit */
#include <Rversion.h>

#define PRODUCER "R-core"

extern char *getRHOMElong(void); /* in ../rhome.c */

int main (int argc, char **argv)
{
    int status = 0;
    DWORD subkeys = 0;
  
    char *RHome, version[40], keyname[60];
    LONG rc;
    HKEY hkey, hkey2;
    
    snprintf(keyname, 60, "Software\\%s\\R", PRODUCER);

    /* Needs to match JRins.pl */
    if(strncmp(R_STATUS, "Under ", 6) == 0)
	snprintf(version, 40, "%s.%s Pre-release", R_MAJOR, R_MINOR);
    else
	snprintf(version, 40, "%s.%s %s", R_MAJOR, R_MINOR, R_STATUS);

    if(argc > 1) { /* remove the keys */
	printf("unregistering R ... ");
	if ((rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, keyname, 0, 
			       KEY_SET_VALUE, &hkey)) == ERROR_SUCCESS) {
	    RegDeleteValue(hkey, "InstallPath");
	    RegDeleteValue(hkey, "Current Version");
	    RegDeleteKey(hkey, version);
	    RegQueryInfoKey(hkey, NULL, NULL, NULL, &subkeys, NULL, NULL, NULL, NULL, NULL,
	                    NULL, NULL);
	    RegCloseKey(hkey);
	   if (!subkeys && (rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software\\R-core", 0, 
				  KEY_SET_VALUE, &hkey)) == ERROR_SUCCESS) {
	       RegDeleteKey(hkey, "R");
	       RegCloseKey(hkey);
	       if ((rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "Software", 0, 
				      KEY_SET_VALUE, &hkey)) == ERROR_SUCCESS) {
		   RegDeleteKey(hkey, PRODUCER);
		   RegCloseKey(hkey);
	       }
	   }
	   printf("succeeded\n");
	} else {
	    printf("was not registered\n");
	    status = 1;
	}
    } else {
    	RHome = getRHOMElong();
	if ((rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE, keyname, 0, 
			       KEY_ALL_ACCESS, &hkey)) != ERROR_SUCCESS) {
	    /* failed to open key, so try to create it */
	    rc = RegCreateKey(HKEY_LOCAL_MACHINE, keyname, &hkey);
	}
	if(rc == ERROR_SUCCESS) {
	    rc = RegSetValueEx(hkey, "InstallPath", 0, REG_SZ,
			       (CONST BYTE *)RHome, lstrlen(RHome)+1);
	    if (rc == ERROR_SUCCESS)
		rc = RegSetValueEx(hkey, "Current Version", 0, REG_SZ,
				   (CONST BYTE *)version, lstrlen(version)+1);

	    if (rc == ERROR_SUCCESS)
	    	rc = RegCreateKey(hkey, version, &hkey2);
	    
	    if (rc == ERROR_SUCCESS) {
	    	rc = RegSetValueEx(hkey2, "InstallPath", 0, REG_SZ,
	    			(CONST BYTE *)RHome, lstrlen(RHome)+1);
	    	RegCloseKey(hkey2);
	    }
	    RegCloseKey(hkey);	
	} else {
	    status = 1;
	}
    }
    exit(status);
}
