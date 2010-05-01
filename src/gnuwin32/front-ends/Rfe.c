/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2010  R Development Core Team
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
#include <stdio.h>

extern char *getRHOME(int); /* in ../rhome.c */

#define CMD_LEN 10000
int main (int argc, char **argv)
{
    int cmdarg = 1;
    char arch[10] = R_ARCH, cmd[CMD_LEN];

    if (argc > 2 && strcmp(argv[1], "--arch") == 0) {
	cmdarg = 3;
	strncpy(arch, argv[2], 10); arch[9] = '\0';
	if(strcmp(arch, "32") == 0) strcpy(arch, "i386");
	if(strcmp(arch, "64") == 0) strcpy(arch, "x64");
	if(strcmp(arch, "i386") && strcmp(arch, "x64")) {
	    fprintf(stderr, "valid values for --arch are i386, x64, 32, 64\n");
	    exit(1);
	}
    }
    snprintf(cmd, CMD_LEN, "%s\\bin\\%s\\R.exe", getRHOME(2), arch);

    for(int i = cmdarg; i < argc; i++) {
	strcat(cmd, " ");
	if(strchr(argv[i], ' ')) {
	    /* We should really escape " here, I believe */
	    strcat(cmd, "\"");
	    strcat(cmd, argv[i]);
	    strcat(cmd, "\"");
	} else strcat(cmd, argv[i]);
    }
    
    exit(system(cmd));
 }
