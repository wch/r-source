/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1998-2012   The R Core Team
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

/* POSIX says ssize_t is defined in unistd.h, but apparently on
   RH9 it is not.  (Dominick Samperi, R-devel, 2006-04-27)
   So include sys/types.h and remove Win32 special casing.
*/
#include <sys/types.h>

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
ssize_t Sock_write(int fd, const void *buf, size_t nbytes, Sock_error_t perr);

/* R interface (Rsock.c) :*/
void in_Rsockopen(int *port);
void in_Rsocklisten(int *sock, char **buf, int *len);
void in_Rsockconnect(int *port, char **host);
void in_Rsockclose(int *sockp);
void in_Rsockread (int *sockp, char **buf, int *maxlen);
void in_Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len);
int in_Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		   double timeout);

/* from Rsock.c, for sockconn.c */
int R_SockOpen(int port);
int R_SockListen(int sockp, char *buf, int len, int timeout);
int R_SockConnect(int port, char *host, int timeout);
int R_SockClose(int sockp);
ssize_t R_SockRead(int sockp, void *buf, size_t maxlen, int blocking, int timeout);
ssize_t R_SockWrite(int sockp, const void *buf, size_t len, int timeout);

/* from Rhttpd.c */
int in_R_HTTPDCreate(const char *ip, int port);
void in_R_HTTPDStop(void);
