/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2007  Guido Masarotto and Brian Ripley
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <Fileio.h>

#ifndef MAX_PATH
# include <windows.h>
#endif


static FILE *ff = NULL;
static char optfl[MAX_PATH];
static int optln;

void optclosefile(void)
{
    if (!ff) return;
    fclose(ff);
    ff = NULL;
}


int optopenfile(const char *fname)
{
    optclosefile();
    if (!fname || !(ff = R_fopen(fname,"r"))) return 0;
    strcpy(optfl,fname);
    optln = 0;
    return 1;
}

char *
optfile(void)
{
    return optfl;
}

int
optline(void)
{
    return optln;
}


static char *rmspace(char *s)
{
    int   i;

    for (i = strlen(s) - 1; i >= 0 && s[i] == ' '; i--)
	s[i] = '\0';
    for (i = 0; s[i] == ' '; i++);
    return &s[i];
}


int optread(char *opt[], const char sep)
{
    static char sm[120];
    char *p, *s;
    int   l;

    if (!ff)
	return 0;
    l = 0;
    while (l == 0) {
	if (!fgets(sm, 120, ff)) {
	    fclose(ff);
	    ff = NULL;
	    return 0;
	}
	optln += 1;
	l = strlen(sm);
	if (sm[l - 1] != '\n')
	    return 1;
	else
	    sm[l - 1] = '\0';
	s = rmspace(sm);
	l = (*s == '#') ? 0 : strlen(s);
    }
    for (p = s; *p; p++)
	if (*p == '#') {
	    *p = '\0';
	    s = rmspace(s);
	    l = strlen(s);
	    break;
	}
    for (p = s; *p; p++)
	if (*p == sep)
	    break;
    if (!*p)
	return 1;
    *p = '\0';
    opt[0] = rmspace(s);
    opt[1] = rmspace(p + 1);
    if (strlen(opt[0]) && strlen(opt[1]))
	return 2;
    else if (strlen(opt[0]))
	return 3;
    else
	return 1;
}
