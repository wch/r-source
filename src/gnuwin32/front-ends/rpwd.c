/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000  R Development Core Team
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

#define NONAMELESSUNION
#include <windows.h>
#include <direct.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

int main (int argc, char **argv)
{
    /* tasks:
       find pwd of (only) arg
       convert to short name if contains spaces
       print it to stdout.
     */
    char *p, buf[MAX_PATH];
    int hasspace = 0;

    if(argc == 2) {
	if(chdir(argv[1])) exit(1);
    }
    if(argc <= 2) {
	getcwd(buf, MAX_PATH);
	for (p = buf; *p; p++) 
	    if (isspace(*p)) { hasspace = 1; break; }
	if (hasspace)
	    GetShortPathName(buf, buf, MAX_PATH);
	for (p = buf; *p; p++)
	    if (*p == '\\') *p = '/';
	printf("%s", buf);
	exit(0);
    } else exit(2);
}
