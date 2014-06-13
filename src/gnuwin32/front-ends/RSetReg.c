/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-10  R Core Team
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

#ifdef _WIN64
# define RK "R64"
#else
# define RK "R32"
#endif

extern char *getRHOMElong(int m); /* in ../rhome.c */

int main (int argc, char **argv)
{
    HKEY hk = HKEY_LOCAL_MACHINE;
    DWORD subkeys = 0;
  
    char *RHome, version[40], keyname[60];
    LONG rc;
    HKEY hkey, hkey2;
    int delete = 0, personal = 0;

    for(int i = 1; i < argc; i++) if(!strcmp(argv[i], "/U")) delete = 1;
    for(int i = 1; i < argc; i++) if(!strcmp(argv[i], "/Personal")) personal = 1;
    
    if(personal) hk = HKEY_CURRENT_USER;

    /* Needs to match JRins */
    if(strncmp(R_STATUS, "Under ", 6) == 0)
	snprintf(version, 40, "%s.%s Pre-release", R_MAJOR, R_MINOR);
    else
	snprintf(version, 40, "%s.%s %s", R_MAJOR, R_MINOR, R_STATUS);

    if(delete) {
	printf("unregistering R %s ... ", version);

	snprintf(keyname, 60, "Software\\%s\\%s\\%s", PRODUCER, RK, version);
	if (RegOpenKeyEx(hk, keyname, 0, KEY_SET_VALUE, &hkey) == ERROR_SUCCESS) {
	    RegDeleteValue(hkey, "InstallPath");
	    RegDeleteKey(hkey, version);
	    RegCloseKey(hkey);
	} else
	    fprintf(stderr, "\nWarning: failed to open key '%s'\n", keyname);
	
	snprintf(keyname, 60, "Software\\%s\\%s", PRODUCER, RK);
	if (RegOpenKeyEx(hk, keyname, 0, KEY_SET_VALUE, &hkey) == ERROR_SUCCESS) {
	    RegDeleteValue(hkey, "InstallPath");
	    RegDeleteValue(hkey, "Current Version");
	    RegDeleteKey(hkey, version);
	    RegQueryInfoKey(hkey, NULL, NULL, NULL, &subkeys, NULL, NULL, NULL, NULL, NULL,
	                    NULL, NULL);
	    RegCloseKey(hkey);
	    /* delete key if empty */
	    if (!subkeys && 
		RegOpenKeyEx(hk, "Software\\R-core", 0, KEY_SET_VALUE, &hkey)
		== ERROR_SUCCESS) {
		RegDeleteKey(hkey, RK);
		RegCloseKey(hkey);
	    }
	} else
	    fprintf(stderr, "\nWarning: failed to open key '%s'\n", keyname);

	snprintf(keyname, 60, "Software\\%s\\R\\%s", PRODUCER, version);
	if (RegOpenKeyEx(hk, keyname, 0, KEY_SET_VALUE, &hkey) == ERROR_SUCCESS) {
	    RegDeleteValue(hkey, "InstallPath");
	    RegDeleteKey(hkey, version);
	    RegCloseKey(hkey);
	} else
	    fprintf(stderr, "\nWarning: failed to open key '%s'\n", keyname);

	
	snprintf(keyname, 60, "Software\\%s\\R", PRODUCER);
	if (RegOpenKeyEx(hk, keyname, 0, KEY_SET_VALUE, &hkey) == ERROR_SUCCESS) {
	    RegDeleteValue(hkey, "InstallPath");
	    RegDeleteValue(hkey, "Current Version");
	    RegDeleteKey(hkey, version);
	    RegQueryInfoKey(hkey, NULL, NULL, NULL, &subkeys, NULL, NULL, NULL, NULL, NULL,
	                    NULL, NULL);
	    RegCloseKey(hkey);
	    /* delete key if empty */
	    if (!subkeys && 
		RegOpenKeyEx(hk, "Software\\R-core", 0, KEY_SET_VALUE, &hkey)
		== ERROR_SUCCESS) {
		RegDeleteKey(hkey, "R");
		RegCloseKey(hkey);
	    }

	    printf("succeeded\n");
	} else {
	    printf("was not registered\n");
	    exit(1);
	}
    } else {
	printf("registering R %s ... ", version);
    	RHome = getRHOMElong(3);

	snprintf(keyname, 60, "Software\\%s\\R", PRODUCER);
	if ((rc = RegOpenKeyEx(hk, keyname, 0, KEY_ALL_ACCESS, &hkey))
	    != ERROR_SUCCESS) {
	    /* failed to open key, so try to create it */
	    rc = RegCreateKey(hk, keyname, &hkey);
	}
	if(rc != ERROR_SUCCESS) {
	    fprintf(stderr, "\nError: failed to open key '%s'\n", keyname);
	    exit(1);
	}
	
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

	snprintf(keyname, 60, "Software\\%s\\%s", PRODUCER, RK);
	if ((rc = RegOpenKeyEx(hk, keyname, 0, 
			       KEY_ALL_ACCESS, &hkey)) != ERROR_SUCCESS) {
	    /* failed to open key, so try to create it */
	    rc = RegCreateKey(hk, keyname, &hkey);
	}
	if(rc != ERROR_SUCCESS) {	    
	    fprintf(stderr, "\nError: failed to open key '%s'\n", keyname);
	    exit(1);
	}
	
	rc = RegSetValueEx(hkey, "InstallPath", 0, REG_SZ,
			   (CONST BYTE *)RHome, lstrlen(RHome)+1);
	rc = RegSetValueEx(hkey, "Current Version", 0, REG_SZ,
			   (CONST BYTE *)version, lstrlen(version)+1);	
	rc = RegCreateKey(hkey, version, &hkey2);
	if (rc == ERROR_SUCCESS) {
	    rc = RegSetValueEx(hkey2, "InstallPath", 0, REG_SZ,
			       (CONST BYTE *)RHome, lstrlen(RHome)+1);
	    RegCloseKey(hkey2);
	}
	RegCloseKey(hkey);	

	printf("succeeded\n");
    }
    exit(0);
}
