/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-11  R Core Team
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

#include <stdlib.h> /* for exit */
#include <string.h>

extern int rcmdfn (int cmdarg, int argc, char **argv); /* in rcmdfn.c */

int main (int argc, char **argv)
{
    int cmdarg = 0;

    if (argc > 1) {
	if (strcmp(argv[1], "-h") == 0
	 	|| strcmp(argv[1], "--help") == 0) cmdarg = 1;
	else {
	    /* see if any arg is 'CMD' */
	    for(int i = 1; i < argc; i++)
		if(strcmp(argv[i], "CMD") == 0) {
		    cmdarg = i + 1;
		    break;		    
		}
	    if (cmdarg >= 3) { /* something before CMD */
		/* Cannot set to empty value on Windows */
		char *Init = "R_PROFILE_USER=\r", *Site="R_RPOFILE=\r",
		    *Env1="R_ENVIRON=\r", *Env2="R_ENVIRON_USER=\r";
		for(int i = 1; i < cmdarg; i++) {
		    char *a = argv[i];
		    if (strcmp(a, "--no-init-file") == 0 ||
			strcmp(a, "--vanilla") == 0) putenv(Init);
		    if (strcmp(a, "--no-site-file") == 0 ||
			strcmp(a, "--vanilla") == 0) putenv(Site);
		    if (strcmp(a, "--no-environ") == 0 ||
			strcmp(a, "--vanilla") == 0) {
			putenv(Env1); putenv(Env2);
		    }
		}
	    }
	}
    }

    exit(rcmdfn(cmdarg, argc, argv));
 }
