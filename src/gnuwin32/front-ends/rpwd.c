/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2023  R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

#include <windows.h>
#include <direct.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#if __MINGW32_MAJOR_VERSION == 3 && __MINGW32_MINOR_VERSION < 11
#error mingw-runtime version 3.11 or later is required
#endif

int main (int argc, char **argv)
{
    /* tasks:
       find pwd of (only) arg
       convert to short name if contains spaces
       print it to stdout.
     */

    if(argc == 2) {
	if(!SetCurrentDirectory(argv[1])) exit(1);
    }
    if(argc <= 2) {
	char *p, *buf;
	int hasspace = 0;
	DWORD res = GetCurrentDirectory(0, NULL);

	if (!res)
	    exit(1);
	buf = (char *)malloc(res);
	if (!buf || !GetCurrentDirectory(res, buf))
	    exit(1);

	for (p = buf; *p; p++) 
	    if (isspace(*p)) { hasspace = 1; break; }

	if (hasspace) {
	    /* NOTE: short names are not always enabled */
	    res = GetShortPathName(buf, NULL, 0);
	    if (res > 0) {
		char *sbuf = (char *)malloc(res);
		if (!sbuf)
		    exit(1);
		DWORD res1 = GetShortPathName(buf, sbuf, res);
		if (res1 > 0 && res1 < res) {
		    free(buf);
		    buf = sbuf;
		    sbuf = NULL;
		} else
		    free(sbuf);
	    }
	}
	for (p = buf; *p; p++)
	    if (*p == '\\') *p = '/';
	printf("%s", buf);
	free(buf);
	exit(0);
    } else exit(2);
}

