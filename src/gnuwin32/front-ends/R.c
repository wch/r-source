/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-4  R Development Core Team
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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern int rcmdfn (int cmdarg, int argc, char **argv); /* in rcmdfn.c */

int main (int argc, char **argv)
{
    int cmdarg = 0;

    if (argc > 1) {
	if (strcmp(argv[1], "CMD") == 0) cmdarg = 2;
	else if (strcmp(argv[1], "-h") == 0
	 	|| strcmp(argv[1], "--help") == 0) cmdarg = 1;
    }

    exit(rcmdfn(cmdarg, argc, argv));
 }
