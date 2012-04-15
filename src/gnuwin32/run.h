/*
 *  A simple 'reading' pipe (and a command executor)
 *  Copyright (C) 1999  Guido Masarotto
 *            (C) 2004-10  The R Core Team
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


#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

struct structRPIPE {
    PROCESS_INFORMATION pi;
    HANDLE thread;
    HANDLE read, write;
    int exitcode, active;
};

typedef struct structRPIPE rpipe;

/*
 * runcmd and rpipeClose return the exit code of the process
 * if runcmd return NOLAUNCH, problems in process start
*/
#define runcmd Rf_runcmd
int   runcmd(const char *cmd, cetype_t enc, int wait, int visible, 
	     const char *fin, const char *fout, const char *ferr);

rpipe *rpipeOpen(const char *cmd, cetype_t enc, int visible, 
		 const char *finput, int io,
		 const char *fout, const char *ferr);
char  *rpipeGets(rpipe *r, char *buf, int len);
int rpipeGetc(rpipe *r);
int rpipeClose(rpipe *r);

char *runerror(void);

/* Changed in R 2.12.0 to be the conventional Unix value -- previously -1 */
#define NOLAUNCH 127
