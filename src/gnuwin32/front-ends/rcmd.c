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
#include <stdlib.h>
#include <stdio.h>

extern char *getRHOME(); /* in ../rhome.c */

int main (int argc, char **argv)
{
    /* tasks:
       find R_HOME
       set PATH to include R_HOME\bin
       set PERL5LIB to %R_HOME%/share/perl;%Perl5LIB%
       launch perl -S $*
     */
    int i, res, status = 0;
    char *RHome, PERL5LIB[MAX_PATH], PATH[MAX_PATH], RHOME[MAX_PATH],
	*p, cmd[256];

    RHome = getRHOME();
    strcpy(RHOME, "R_HOME=");
    strcat(RHOME, RHome);
    for (p = RHOME; *p; p++) if (*p == '\\') *p = '/';
    putenv(RHOME);

    strcpy(PATH, "PATH=");
    strcat(PATH, RHome); strcat(PATH, "\\bin;");
    strcat(PATH, getenv("PATH"));
    putenv(PATH);

    if( (p = getenv("TMPDIR")) && strlen(p)) {
	/* TMPDIR is already set */
    } else {
	putenv("TMPDIR=c:/TEMP");
    }

    strcpy(PERL5LIB, "PERL5LIB=");
    strcat(PERL5LIB, RHome); strcat(PERL5LIB, "\\share\\perl;");
    if( (p = getenv("PERL5LIB")) ) strcat(PERL5LIB, p);
    putenv(PERL5LIB);

    if(argc > 1) {
	p = argv[1];
	if(strcmp(".sh", p + strlen(p) - 3) == 0) {
	    strcpy(cmd, "sh "); strcat(cmd, RHome); strcat(cmd, "/bin/");
	} else if(strcmp(".bat", p + strlen(p) - 4) == 0) strcpy(cmd, "");
	else if(strcmp(".exe", p + strlen(p) - 4) == 0) strcpy(cmd, "");
	else {
	    strcpy(cmd, "perl "); strcat(cmd, RHome); strcat(cmd, "/bin/");
	}
	for (i = 1; i < argc; i++){
	    if (i > 1) strcat(cmd, " ");
	    strcat(cmd, argv[i]);
	}
/*	printf("cmd is %s\n", cmd); */
	res = system(cmd);
	if(res) status = 1;
    }
    exit(status);
}
