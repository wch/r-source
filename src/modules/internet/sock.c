/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1998-2015   The R Core Team
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

/* <UTF8> chars are only handled as a whole */

/* Simple sockets interface derived from the sockets UICI
   implementation in Appendix B of Practical UNIX Programming,
   K. A. Robbins and S. Robbins, Prentice Hall, 1996. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#if defined(Win32)
#  include <winsock2.h>
#  include <io.h>
#else
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  include <netdb.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#endif

#include <R_ext/Error.h>
#include "sock.h"

#if defined(__hpux)
   extern int h_errno; /* HP-UX 9.05 forgets to declare this in netdb.h */
#endif

#define MAXBACKLOG 5

static int Sock_error(Sock_error_t perr, int e, int he)
{
    if (perr != NULL) {
	perr->error = e;
	perr->h_error = he;
    }
    return -1;
}

/* <FIXME> is this classic MacOS X? */
#ifdef MACINTOSH
extern void __sinit(void);
extern int __initialize (void *ignoredParameter);
int __initialize(void *ignoredParameter) {
    __sinit();
    return(0);
}
#endif
/* </FIXME> */

/* Initialize the socket services */
int Sock_init()
{
#if defined(Win32)
    WSADATA wsaData;
    WORD wVers = MAKEWORD(1, 1);
    if (WSAStartup(wVers, &wsaData) != 0)
	return 1;
#elif defined(MACINTOSH)
    GUSISetup(GUSIwithInternetSockets);
#elif defined(SIGPIPE)
    struct sigaction act;
    if (sigaction(SIGPIPE, (struct sigaction *)NULL, &act) < 0)
	return 1;
    if (act.sa_handler == SIG_DFL) {
	act.sa_handler = SIG_IGN;
	if (sigaction(SIGPIPE, &act, (struct sigaction *)NULL) < 0)
	    return 1;
    }
#endif
    return 0;
}

/* open a socket for listening */
int Sock_open(Sock_port_t port, Sock_error_t perr)
{
    int sock;
    struct sockaddr_in server;

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	return Sock_error(perr, errno, 0);

    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons((short)port);

#ifndef Win32
    /* According to Stephens (1998) "Unix Network Programming, Vol 1",
       pp. 194 on UNIX we need to set the SO_REUSEADDR socket option
       if we want to be able to have a server create several
       connections that are open simultaneously.  If this option is
       not set, the call to `bind' will fail when attempting to open
       another server socket on a given port if a connection made to
       that port is still alive. This is not an issue if each socket
       connection is handled and terminated before the next server
       socket is created.  Stephens recommends setting SO_REUSEADDR on
       all servers.  Even with this option set it remains impossible
       to establish two simultaneous servers on the same IPADDR/port
       combination.  Tcl 8.3 uses the bit of code used here in
       tclUnixChan.c, so it should work on most UNIX platforms.

       Unfortunately things are different on Windows.  According to
       Quinn and Shute (1996) "Windows Sockets Network Programming",
       pp. 305, Windows sockets are quite happy to allow two servers
       to use the same IPADDR/port, with unpredictable results, if
       SO_REUSEADDR is set.  So setting this option on Windows is not
       a good idea.  It is unclear whether it is possible on WIndows
       to establish a new server socket while a connection from a
       previous server socket is still active.

       This would be less of an issue, but would not entirely
       disappear as an issue. if the R interface separated the
       `socket'/`bind'/`listen' part of setting up a server socket,
       which is only needed once per server instance, from the
       `accept' part, which is needed for each connection.  LT */
    {
	int val = 1;
	setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &val, sizeof(val));
    }
#endif

    if ((bind(sock, (struct sockaddr *)&server, sizeof(server)) < 0) ||
	(listen(sock, MAXBACKLOG) < 0))
	return Sock_error(perr, errno, 0);
    return sock;
}

/* listen on a socket, return name of connecting host in cname */
int Sock_listen(int fd, char *cname, int buflen, Sock_error_t perr)
{
    struct sockaddr_in net_client;
    R_SOCKLEN_T len = sizeof(struct sockaddr);
    int retval;
    struct hostent *hostptr;

    do
	retval = accept(fd, (struct sockaddr *)(&net_client), &len);
    while (retval == -1 && errno == EINTR);
    if (retval == -1)
	return Sock_error(perr, errno, 0);

    if (cname != NULL && buflen > 0) {
	size_t nlen;
	const char *name;
	struct in_addr *iaddr = &(net_client.sin_addr);
	hostptr = gethostbyaddr((char *)iaddr, sizeof(struct in_addr),
				AF_INET);
	name = (hostptr == NULL) ? "unknown" :  hostptr->h_name;
	nlen = strlen(name);
	if (buflen < nlen + 1)
	    nlen = buflen - 1;
	strncpy(cname, name, nlen);
	cname[nlen] = 0;
    }
    return retval;
}

/* open and connect to a socket */
int Sock_connect(Sock_port_t port, char *sname, Sock_error_t perr)
{
    struct sockaddr_in server;
    struct hostent *hp;
    int sock;
    int retval;

    if (! (hp = gethostbyname(sname))
	|| (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	return Sock_error(perr, errno, h_errno);

    memcpy((char *)&server.sin_addr, hp->h_addr_list[0], hp->h_length);
    server.sin_port = htons((short)port);
    server.sin_family = AF_INET;

    do
	retval = connect(sock, (struct sockaddr *) &server, sizeof(server));
    while (retval == -1 && errno == EINTR);
    if (retval == -1) {
	Sock_error(perr, errno, 0);
#ifdef Win32
	closesocket(sock);
#else
	close(sock);
#endif
	return -1;
    }
    return sock;
}

/* close a socket */
int Sock_close(int fd, Sock_error_t perr)
{
#ifdef Win32
    if (closesocket(fd) != 0)
	return Sock_error(perr, WSAENOTSOCK, 0);
#else
    if (close(fd) < 0)
	return Sock_error(perr, errno, 0);
#endif
    else
	return 0;
}

/* read from a socket */
ssize_t Sock_read(int fd, void *buf, size_t size, Sock_error_t perr)
{
    ssize_t retval;
    do
	retval = recv(fd, buf, size, 0);
    while (retval == -1 && errno == EINTR);
    if (retval == -1)
	return Sock_error(perr, errno, 0);
    else
	return retval;
}

/* write to a socket */
ssize_t Sock_write(int fd, const void *buf, size_t size, Sock_error_t perr)
{
    ssize_t retval;
    do
	retval = send(fd, buf, size, 0);
    while (retval == -1 && errno == EINTR);
    if (retval == -1)
	return Sock_error(perr, errno, 0);
    else
	return retval;
}
