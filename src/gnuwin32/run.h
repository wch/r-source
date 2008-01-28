/*
 *  A simple 'reading' pipe (and a command executor)
 *  Copyright (C) 1999  Guido Masarotto
 *            (C) 2004-8  The R Development Core Team
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
    HANDLE process;
    HANDLE read, write;
    int exitcode, active;
};

typedef struct structRPIPE rpipe;

/*
 * runcmd and rpipeClose return the exit code of the process
 * if runcmd return -1, problems in process start
*/
#define runcmd Rf_runcmd
int   runcmd(const char *cmd, int enc, int wait, int visible, const char *finput);

rpipe *rpipeOpen(const char *cmd, int enc, int visible, const char *finput, int io);
char  *rpipeGets(rpipe *r, char *buf, int len);
int rpipeGetc(rpipe *r);
int rpipeClose(rpipe *r);

char *runerror(void);

#define NOLAUNCH -1
