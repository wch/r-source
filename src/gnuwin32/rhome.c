/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999--2010  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2021--2023  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#undef sprintf

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>		/* for strrchr(...) */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>		/* for exit */

extern void R_Suicide(char *s);

/* get R_HOME from the module path: used in RSetReg */
/* Returns a result to be freed by freeRHOMElong(). */
char *getRHOMElong(int m)
{
    DWORD size = 1;
    char *buf = NULL;
    
    /* GetModuleFileName doesn't return the needed buffer size. */
    for(;;) {
	buf = (char *)malloc(size);
	if (!buf)
	    return NULL;
	DWORD res = GetModuleFileName(NULL, buf, size);
	if (res > 0 && res < size) /* success */
	    break;
	free(buf);
	if (res != size) /* error */
	    return NULL;
	size *= 2; /* try again with 2x larger buffer */
    }
    for(int i=0; i < m; i++) {
	/* drop suffix starting with last backslash */
	/* <MBCS-FIXME> We can't just use Rf_strchr as this is called
	   from front-ends */
	char *p = strrchr(buf, '\\');
	if (!p) {
	    MessageBox(NULL, "Installation problem", "Terminating",
	               MB_TASKMODAL | MB_ICONSTOP | MB_OK);
	    free(buf);
	    exit(1);
	}
	*p = '\0';
    }
    return buf;
}

void freeRHOMElong(char *s)
{
    if (s)
	free(s);
}

/* get no-spaces version of R_HOME from the module path: 
   used in Rgui, Rterm and Rcmd
*/
/* Returns a result to be freed by freeRHOME(). Previously, pointer to a static
   fixed-size buffer has been returned, so older embedding applications would
   not know to free the result, but they would probably call this function
   only once. */
char *getRHOME(int m)
{
    char *p;
    int hasspace = 0;
    char *rhome = getRHOMElong(m);

    if (!rhome)
	return NULL;

    /* try removing spaces from path */
    for (p = rhome; *p; p++)
	if (isspace(*p)) { hasspace = 1; break; }
    if (hasspace) {
	/* NOTE: this fails when short names are not enabled */
	DWORD res = GetShortPathName(rhome, NULL, 0);
	if (res > 0) {
	    char *shome = (char*) malloc(res);
	    if (shome) {
		DWORD res1 = GetShortPathName(rhome, shome, res);
		if (res1 > 0 && res1 < res) {
		    free(rhome);
		    return shome;
		}
	    }
	}
    }
    return rhome;
}

void freeRHOME(char *s)
{
    if (s)
	free(s);
}

static char *rhome_from_registry(HKEY key, int *key_found)
{
    LONG rc;
    HKEY hkey;
    DWORD keytype = REG_SZ, cbData;
    rc = RegOpenKeyEx(key, "Software\\R-core\\R", 0,
                      KEY_READ, &hkey);
    if (rc == ERROR_SUCCESS) {
	*key_found = 1;
	char *rhome = NULL;
	cbData = 0;
	rc = RegQueryValueEx(hkey, "InstallPath", 0, &keytype,
                             NULL, &cbData);
	if (rc == ERROR_MORE_DATA) {
	    rhome = (char *)malloc(cbData);
	    if (!rhome)
		return NULL;
	    rc = RegQueryValueEx(hkey, "InstallPath", 0, &keytype,
	                         (LPBYTE) rhome, &cbData);
	}
	RegCloseKey (hkey);
	if (rc == ERROR_SUCCESS)
	    return rhome;
	else if (rhome)
	    free(rhome);
    } else
	*key_found = 0;
    return NULL;
}

/* get R_HOME from environment or registry: used in embedded apps */
/* Returns a result to be freed by free_R_HOME(). */
char *get_R_HOME(void)
{
    char *rhome;

    /* First try the C environment space */
    char *env = getenv("R_HOME");
    if(env) {
	rhome = (char *)malloc(strlen(env) + 1);
	if (rhome)
	    strcpy(rhome, env);
	return rhome;
    }

    /* Then the Windows API environment space */
    DWORD res = GetEnvironmentVariable("R_HOME", NULL, 0);
    if (res) {
	rhome = (char *)malloc(res);
	if (!rhome)
	    return NULL;
	DWORD res1 = GetEnvironmentVariable("R_HOME", rhome, res);
	if (res1 > 0 && res1 < res)
	    return rhome;
	free(rhome);
    }

    /* And then the registry */
    int key_found = 0; 
    rhome = rhome_from_registry(HKEY_CURRENT_USER, &key_found);
    if (key_found) {
	/* Do not look into the machine registry when there are any
	   versions of R installed for the current user. Any installation
	   of R updates the path in the registry (user or machine) and any
	   uninstallation clears it, so the entry may be unset even when
	   some versions of R are installed. One should use R_HOME
	   environment variable to avoid surprise.
	*/
	return rhome;
    }
    return rhome_from_registry(HKEY_LOCAL_MACHINE, &key_found);
}

void free_R_HOME(char *s)
{
    if (s)
	free(s);
}

void R_putenv_path_cpy(char *varname, char *value, int fixslash)
{
    size_t needed = strlen(varname) + 1 + strlen(value) + 1;
    char *buf = (char *)malloc(needed);
    if (!buf)
	R_Suicide("Allocation error");
    snprintf(buf, needed, "%s=%s", varname, value);
    if (fixslash)
	for (char *p = buf; *p; p++) if (*p == '\\') *p = '/';
    putenv(buf);
    /* no free here: storage remains in use */
}

