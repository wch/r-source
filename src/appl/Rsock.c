/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001   Robert Gentleman, Ross Ihaka and the
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h> /* for NULL */
#include <limits.h>
#include <stdio.h>
#include <string.h>
#if defined(Macintosh)
#include <types.h>
#else
#include <sys/types.h>
#endif
/* #include <errno.h>*/
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "sock.h"

static int sock_inited = 0;


static int enter_sock(int fd)
{
#ifdef DEBUG
    printf("enter_sock(%d)\n", fd);
#endif
    if (fd == -1) return 0; else return fd;
}

static int close_sock(int fd)
{
    return Sock_close(fd, NULL) == -1 ? 0 : 1 ;
}

static void check_init(void)
{
    if (! sock_inited) {
#ifdef DEBUG
	printf("initing\n");
#endif
	Sock_init();
	sock_inited = 1;
    } 
}

void Rsockopen(int *port)
{
    check_init();
    *port = enter_sock(Sock_open(*port, NULL));
}

void Rsocklisten(int *sockp, char **buf, int *len)
{
    check_init();
    *sockp = enter_sock(Sock_listen(*sockp, *buf , *len, NULL));
}

void Rsockconnect(int *port, char **host)
{
    check_init();
#ifdef DEBUG
    printf("connect to %d at %s\n",*port, *host);
#endif
    *port = enter_sock(Sock_connect(*port, *host, NULL));
}

void Rsockclose(int *sockp)
{
    *sockp = close_sock(*sockp);
}

void Rsockread(int *sockp, char **buf, int *maxlen)
{
    check_init();
#ifdef DEBUG
    printf("Reading from %d\n",*sockp);
#endif
    *maxlen = (int) Sock_read(*sockp, *buf, *maxlen, NULL);
}

void Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)
{
    ssize_t n;
    if (*end > *len)
	*end = *len;
    if (*start < 0)
	*start = 0;
    if (*end < *start){
	*len = -1;
	return;
    }
    check_init();
#ifdef DEBUG
    printf("writing %s to %d", *buf, *sockp);
#endif
    n = Sock_write(*sockp, *buf + *start, *end - *start, NULL);
    *len = (int) n;
}

/* for use in socket connections */

int R_SockOpen(int port)
{
    check_init();
    return Sock_open(port, NULL);
}

int R_SockListen(int sockp, char *buf, int len)
{
    check_init();
    return Sock_listen(sockp, buf, len, NULL);
}

int R_SockConnect(int port, char *host)
{
    check_init();
    return Sock_connect(port, host, NULL);
}

int R_SockClose(int sockp)
{
    return close_sock(sockp);
}

int R_SockRead(int sockp, void *buf, int maxlen)
{
    check_init();
    return (int) Sock_read(sockp, buf, maxlen, NULL);
}

int R_SockWrite(int sockp, const void *buf, int len)
{
    check_init();
    return (int) Sock_write(sockp, buf, len, NULL);
}


#ifdef Unix
#include <signal.h>
#include <sys/wait.h>
static void sig_child(int sig)
{  
    int stat;
    while (waitpid(-1, &stat, WNOHANG) > 0);
}

static int sig_fork_inited = 0;

void Rsockfork(int *pidno)
{
    pid_t pid;
    if (! sig_fork_inited) {
	struct sigaction sa;
	sa.sa_handler = sig_child;
	sa.sa_flags = 0;
	sigaction(SIGCHLD, &sa, NULL);
	sig_fork_inited = 1;
    }
    pid = fork();
    *pidno = (int) pid;
}
#endif
