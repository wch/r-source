/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998, 1998  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>

#ifdef Win32
typedef long ssize_t;
#endif

typedef unsigned short Sock_port_t;

typedef struct Sock_error_t {
    int error;
    int h_error;
} *Sock_error_t;

int Sock_init(void);
int Sock_open(Sock_port_t port, Sock_error_t perr);
int Sock_listen(int fd, char *cname, int buflen, Sock_error_t perr);
int Sock_connect(Sock_port_t port, char *sname, Sock_error_t perr);
int Sock_close(int fd, Sock_error_t perr);
ssize_t Sock_read(int fd, void *buf, size_t nbytes, Sock_error_t perr);
ssize_t Sock_write(int fd, void *buf, size_t nbytes, Sock_error_t perr);
