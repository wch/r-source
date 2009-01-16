/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008  R Development Core Team
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


int main (int argc, char **argv)
{
    int i, status = 0;
    unsigned int ret;

    if (argc < 2 || strcmp(argv[1], "--help") == 0) {
	fprintf(stderr, "Usage: Rcmd open file [file ...]\n\n");
	fprintf(stderr, "  opens each file with the application given by\n");
	fprintf(stderr, "  the Windows file association (if any)\n");
	exit(0);
    }
    for(i = 1; i < argc; i++) {
	ret = (unsigned int) ShellExecute(NULL, "open", argv[i], NULL, 
					  ".", SW_SHOW);
	if(ret <= 32) { /* an error condition */
	    status = 32 + ret;
	    if(ret == ERROR_FILE_NOT_FOUND  || ret == ERROR_PATH_NOT_FOUND
	       || ret == SE_ERR_FNF || ret == SE_ERR_PNF)
		fprintf(stderr, "'%s' not found", argv[i]);
	    else if(ret == SE_ERR_ASSOCINCOMPLETE || ret == SE_ERR_NOASSOC)
		fprintf(stderr, 
			"file association for '%s' not available or invalid\n",
			argv[i]);
	    else if(ret == SE_ERR_ACCESSDENIED || ret == SE_ERR_SHARE)
		fprintf(stderr, "access to '%s' denied\n", argv[i]);
	    else
		fprintf(stderr, "problem in displaying '%s'\n", argv[i]);
	}
    }
    exit(status);
}
