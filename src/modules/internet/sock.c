/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1998-2020   The R Core Team
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
 *  https://www.R-project.org/Licenses/
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
#  include <io.h>
#else
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  include <netdb.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <netinet/tcp.h>
#  ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#  endif
#endif

#include <R_ext/Error.h>
#include "sock.h"

#ifndef Win32
#define SOCKET int
#endif

int R_close_socket(SOCKET s)
{
#ifdef Win32
    return(closesocket(s));
#else
    return(close(s));
#endif
}

int R_socket_errno(void)
{
#ifdef Win32
    return(WSAGetLastError());
#else
    return(errno);
#endif
}

int R_invalid_socket(SOCKET s)
{
#ifdef Win32
    return(s == INVALID_SOCKET);
#else
    return s < 0;
#endif
}

int R_socket_error(int s)
{
#ifdef Win32
    return(s == SOCKET_ERROR);
#else
    return s < 0;
#endif
}

int R_invalid_socket_eintr(SOCKET s)
{
#ifdef Win32
    return(s == INVALID_SOCKET && WSAGetLastError() == WSAEINTR);
#else
    return(s == -1 && errno == EINTR);
#endif
}

int R_socket_error_eintr(int s)
{
#ifdef Win32
    return(s == SOCKET_ERROR && WSAGetLastError() == WSAEINTR);
#else
    return(s == -1 && errno == EINTR);
#endif
}

char *R_socket_strerror(int errnum)
{
#ifdef Win32
    /* formatError() is not accessible */
    static char buf[1000];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                  NULL, errnum,
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                  buf, 1000, NULL);
    size_t i = strlen(buf);
    if (i > 0 && buf[i-1] == '\n') buf[--i] = '\0';
    if (i > 0 && buf[i-1] == '\r') buf[--i] = '\0';
    if (i > 0 && buf[i-1] == '.') buf[--i] = '\0';
    return buf;
#else
    return(strerror(errnum));
#endif
}

int R_set_nonblocking(SOCKET s)
{
    int status = 0;

#ifdef Win32
    {
	u_long one = 1;
	status = ioctlsocket(s, FIONBIO, &one) == SOCKET_ERROR ? -1 : 0;
    }
#else
# ifdef HAVE_FCNTL
    if ((status = fcntl(s, F_GETFL, 0)) != -1) {
#  ifdef O_NONBLOCK
	status |= O_NONBLOCK;
#  else /* O_NONBLOCK */
#   ifdef F_NDELAY
	status |= F_NDELAY;
#   endif
#  endif /* !O_NONBLOCK */
	status = fcntl(s, F_SETFL, status);
    }
# endif // HAVE_FCNTL
    if (status < 0) {
	R_close_socket(s);
	return -1;
    }
#endif
    /* Will return 0 (success) when running on Unix without the necessary
       fcntl support, which is unlikely. */
    return status; /* 0 */
}

int R_set_nodelay(SOCKET s)
{
    int val = 1;
    return setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char *) &val, sizeof(val));
}

#if defined(__hpux)
   extern int h_errno; /* HP-UX 9.05 forgets to declare this in netdb.h */
#endif

extern struct hostent *R_gethostbyname(const char *name);

#define MAXBACKLOG SOMAXCONN

static int Sock_error(Sock_error_t perr, int e, int he)
{
    if (perr != NULL) {
	perr->error = e;
	perr->h_error = he;
    }
    return -1;
}

/* <FIXME> is this classic Mac OS? */
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
int Sock_init(void)
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
int Sock_open(Sock_port_t port, int blocking, Sock_error_t perr)
{
    SOCKET sock, status;
    struct sockaddr_in server;
#ifdef Win32
    static int use_no_handle_inherit = 1;

    if (use_no_handle_inherit) {
# ifndef WSA_FLAG_NO_HANDLE_INHERIT
#  define WSA_FLAG_NO_HANDLE_INHERIT 0x80
# endif
	/* WSA_FLAG_NO_HANDLE_INHERIT is supported from Windows 7 SP1,
	   on older versions of Windows, WSASocket will fail when used.
	   Try once, fall back to socket() which does not use the flag. */
	sock = WSASocket(AF_INET, SOCK_STREAM, 0, NULL, 0,
			 WSA_FLAG_NO_HANDLE_INHERIT);
	if (R_invalid_socket(sock)) {
	    use_no_handle_inherit = 0;
	    sock = socket(AF_INET, SOCK_STREAM, 0);
	}
    } else
	    sock = socket(AF_INET, SOCK_STREAM, 0);
#else
    sock = socket(AF_INET, SOCK_STREAM, 0);
#endif

    if (R_invalid_socket(sock)) 
	return Sock_error(perr, R_socket_errno(), 0);

    if (!blocking && R_set_nonblocking(sock)) {
	R_close_socket(sock);
	return Sock_error(perr, R_socket_errno(), 0);
    }
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
       a good idea.  It is unclear whether it is possible on Windows
       to establish a new server socket while a connection from a
       previous server socket is still active.

       This would be less of an issue, but would not entirely
       disappear as an issue. if the R interface separated the
       `socket'/`bind'/`listen' part of setting up a server socket,
       which is only needed once per server instance, from the
       `accept' part, which is needed for each connection.  LT
       
       As of 77803,  we have serverSocket() for `socket'/`bind'/`listen', so
       a listening server socket can be re-used to `accept` multiple
       connections.  For security reasons on Windows, Microsoft recommends [1]
       that servers use SO_EXCLUSIVEADDRUSE, with which however they
       document it is not possible to establish a new server socket while a
       connection from a previous socket is still active, and that the
       connection may be active in lower layers of the stack outside of
       direct control of the application.  Snow/parallel PSOCK clusters have
       been relying on that it is possible with default options (neither
       SO_EXCLUSIVEADDRUSE nor SO_REUSEADDR) without receiving bug
       reports that would suggest otherwise. TK
       [1] https://docs.microsoft.com/en-us/windows/win32/winsock/using-so-reuseaddr-and-so-exclusiveaddruse
       */
    {
	int val = 1;
	setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &val, sizeof(val));
    }
#endif

#if defined(HAVE_FCNTL_H) && defined(HAVE_UNISTD_H) && !defined(Win32)
    /* Set FD_CLOEXEC so that child processes, including those run via system(),
       do not inherit the listening socket, thus blocking the port. */
    if ((status = fcntl(sock, F_GETFD, 0)) != -1) {
	status |= FD_CLOEXEC;
	status = fcntl(sock, F_SETFD, status);
    }
    if (status == -1) {
	close(sock);
	return Sock_error(perr, errno, 0);
    }
#elif defined(Win32)
    if (!use_no_handle_inherit) 
	/* Clearing the flag after WSASocket does not work with non-IFS LSPs
	   where "sock" is just a proxy for a real socket, hence we use
	   WSASocket with WSA_FLAG_NO_HANDLE_INHERIT when supported. */
	SetHandleInformation((HANDLE)(uintptr_t)sock,
	                     HANDLE_FLAG_INHERIT, 0);
#endif

    status = bind(sock, (struct sockaddr *)&server, sizeof(server));
    if (R_socket_error(status)) {
	R_close_socket(sock);
	return Sock_error(perr, R_socket_errno(), 0);
    }
    
    status = listen(sock, MAXBACKLOG);
    if (R_socket_error(status)) {
	R_close_socket(sock);
	return Sock_error(perr, R_socket_errno(), 0);
    }

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
    while (R_invalid_socket_eintr(retval));
    if (R_invalid_socket(retval))
	return Sock_error(perr, R_socket_errno(), 0);

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
    SOCKET sock;
    int retval;

    if (! (hp = R_gethostbyname(sname))) 
#ifdef Win32
	return Sock_error(perr, R_socket_errno(), WSAGetLastError());
#else
	return Sock_error(perr, R_socket_errno(), h_errno);
#endif

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (R_invalid_socket(sock))
	return Sock_error(perr, R_socket_errno(), 0);

    memcpy((char *)&server.sin_addr, hp->h_addr_list[0], hp->h_length);
    server.sin_port = htons((short)port);
    server.sin_family = AF_INET;

    do
	retval = connect(sock, (struct sockaddr *) &server, sizeof(server));
    while (R_socket_error_eintr(retval));
    if (R_socket_error(retval)) {
	R_close_socket(sock);
	return Sock_error(perr, R_socket_errno(), 0);
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
    while(R_socket_error_eintr((int)retval));
    if (R_socket_error((int)retval))
	return Sock_error(perr, R_socket_errno(), 0);
    else
	return retval;
}

/* write to a socket */
ssize_t Sock_write(int fd, const void *buf, size_t size, Sock_error_t perr)
{
    ssize_t retval;
    do
	retval = send(fd, buf, size, 0);
    while (R_socket_error_eintr((int)retval));
    if (R_socket_error((int)retval))
	return Sock_error(perr, R_socket_errno(), 0);
    else
	return retval;
}
