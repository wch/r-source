/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1998-2023   The R Core Team
 *  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
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

/* <UTF8> chars are handled as a whole */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern void R_ProcessEvents(void);
#ifdef Win32
#define R_SelectEx(n,rfd,wrd,efd,tv,ih) select(n,rfd,wrd,efd,tv)
#endif

#ifdef HAVE_STRINGS_H
   /* may be needed to define bzero in FD_ZERO (eg AIX) */
  #include <strings.h>
#endif

#include <stdlib.h> /* for NULL */
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
/* #include <errno.h>*/
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "sock.h"

#include <R_ext/Print.h> // for REprintf
#include <Rmath.h> /* for ceil */

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
    struct Sock_error_t perr;
    perr.error = 0;
    int res = Sock_close(fd, &perr);
    if (res == -1) {
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
	return -1;
    }
    return 0;
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

void in_Rsockopen(int *port)
{
    struct Sock_error_t perr;
    check_init();
    perr.error = 0;
    *port = enter_sock(Sock_open((Sock_port_t)*port, 1 /* blocking */,
                                  &perr));
    if(perr.error)
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
}

void in_Rsocklisten(int *sockp, char **buf, int *len)
{
    struct Sock_error_t perr;
    check_init();
    perr.error = 0;
    *sockp = enter_sock(Sock_listen(*sockp, *buf , *len, &perr));
    if(perr.error)
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
}

void in_Rsockconnect(int *port, char **host)
{
    struct Sock_error_t perr;
    check_init();
#ifdef DEBUG
    printf("connect to %d at %s\n",*port, *host);
#endif
    perr.error = perr.h_error = 0;
    *port = enter_sock(Sock_connect((Sock_port_t)*port, *host, &perr));
//    if(perr.h_error) REprintf("host lookup error: %s\n", hstrerror(perr.h_error));
    if(perr.error)
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
}

void in_Rsockclose(int *sockp)
{
    *sockp = close_sock(*sockp);
}

void in_Rsockread(int *sockp, char **buf, int *maxlen)
{
    struct Sock_error_t perr;
    check_init();
#ifdef DEBUG
    printf("Reading from %d\n",*sockp);
#endif
    perr.error = 0;
    *maxlen = (int) Sock_read(*sockp, *buf, *maxlen, &perr);
    if(perr.error)
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
}

void in_Rsockwrite(int *sockp, char **buf, int *start, int *end, int *len)
{
    struct Sock_error_t perr;
    ssize_t n;
    if (*end > *len)
	*end = *len;
    if (*start < 0)
	*start = 0;
    if (*end < *start) {
	*len = -1;
	return;
    }
    check_init();
#ifdef DEBUG
    printf("writing %s to %d", *buf, *sockp);
#endif
    perr.error = 0;
    n = Sock_write(*sockp, *buf + *start, *end - *start, &perr);
    *len = (int) n;
    if(perr.error)
	REprintf("socket error: %s\n", R_socket_strerror(perr.error));
}

/* --------- for use in socket connections ---------- */

#ifdef Win32
# include <io.h>
#else
# include <netdb.h>
# include <sys/socket.h>
# include <netinet/in.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

struct hostent *R_gethostbyname(const char *name);

#ifdef Unix
#include <R_ext/eventloop.h>

/* modified from src/unix/sys-std.c  */
static int
setSelectMask(InputHandler *handlers, fd_set *readMask)
{
    int maxfd = -1;
    InputHandler *tmp = handlers;
    FD_ZERO(readMask);

    while(tmp) {
	if(tmp->fileDescriptor > 0) {
	    /* FD_SETSIZE limit checked by addInputHandler */
	    FD_SET(tmp->fileDescriptor, readMask);
	    maxfd = maxfd < tmp->fileDescriptor ? tmp->fileDescriptor : maxfd;
	}
	tmp = tmp->next;
    }

    return(maxfd);
}
#endif

static void set_timeval(struct timeval *tv, int timeout)
{
#ifdef Unix
    if(R_wait_usec > 0) {
	tv->tv_sec = R_wait_usec / 1000000;
	tv->tv_usec = (suseconds_t)(R_wait_usec - tv->tv_sec * 1000000);
    } else {
	tv->tv_sec = timeout;
	tv->tv_usec = 0;
    }
#elif defined(Win32)
    tv->tv_sec = 0;
    tv->tv_usec = 2e5;
#else
    tv->tv_sec = timeout;
    tv->tv_usec = 0;
#endif
}

static int R_SocketWait(int sockfd, int write, int timeout)
{
    fd_set rfd, wfd;
    struct timeval tv;
    double used = 0.0;

    while(1) {
	int maxfd = 0, howmany;
	R_ProcessEvents();
	set_timeval(&tv, timeout);

#ifdef Unix
	if (sockfd >= FD_SETSIZE)
	    return -EINVAL;
	maxfd = setSelectMask(R_InputHandlers, &rfd);
#else
	FD_ZERO(&rfd);
#endif
	FD_ZERO(&wfd);
	if(write) FD_SET(sockfd, &wfd); else FD_SET(sockfd, &rfd);
	if(maxfd < sockfd) maxfd = sockfd;

	/* increment used value _before_ the select in case select
	   modifies tv (as Linux does) */
	used += tv.tv_sec + 1e-6 * tv.tv_usec;

	howmany = R_SelectEx(maxfd+1, &rfd, &wfd, NULL, &tv, NULL);

	if (R_socket_error(howmany)) {
	    return -R_socket_errno();
	}
	if (howmany == 0) {
	    if(used >= timeout) return 1;
	    continue;
	}

#ifdef Unix
	if((!write && !FD_ISSET(sockfd, &rfd)) ||
	   (write && !FD_ISSET(sockfd, &wfd)) || howmany > 1) {
	    /* was one of the extras */
	    InputHandler *what;
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(what != NULL) what->handler((void*) NULL);
	    continue;
	}
#endif
	/* the socket was ready */
	break;
    }
    return 0;
}

/**** FIXME: merge with R_SocketWait */
/**** FIXME: add timeout argument instead of using global?? */
int R_SocketWaitMultiple(int nsock, int *insockfd, int *ready, int *write,
			 double mytimeout)
{
    fd_set rfd, wfd;
    struct timeval tv;
    double used = 0.0;
    int nready = 0;

    while(1) {
	int maxfd = 0, howmany, i;
	R_ProcessEvents();
#ifdef Unix
	if(R_wait_usec > 0) {
	    int delta;
	    if (mytimeout < 0 || R_wait_usec / 1e-6 < mytimeout - used)
		delta = R_wait_usec;
	    else
		delta = (int)ceil(1e6 * (mytimeout - used));
	    tv.tv_sec = delta / 1000000;
	    tv.tv_usec = (suseconds_t)(delta - tv.tv_sec * 1000000);
	} else if (mytimeout >= 0) {
	    tv.tv_sec = (int)(mytimeout - used);
	    tv.tv_usec = (int)ceil(1e6 * (mytimeout - used - tv.tv_sec));
	} else {  /* always poll occasionally--not really necessary */
	    tv.tv_sec = 60;
	    tv.tv_usec = 0;
	}
#elif defined(Win32)
	tv.tv_sec = 0;
	tv.tv_usec = 2e5;
#else
	if (mytimeout >= 0) {
	    tv.tv_sec = mytimeout - used;
	    tv.tv_usec = ceil(1e6 * (mytimeout - used - tv.tv_sec));
	} else {  /* always poll occasionally--not really necessary */
	    tv.tv_sec = 60;
	    tv.tv_usec = 0;
	}
#endif


#ifdef Unix
	maxfd = setSelectMask(R_InputHandlers, &rfd);
#else
	FD_ZERO(&rfd);
#endif
	FD_ZERO(&wfd);
#ifdef Win32
	if (nsock > FD_SETSIZE)
	    return -WSAEINVAL;
#endif
	for (i = 0; i < nsock; i++) {
#ifdef Unix
	    if (insockfd[i] >= FD_SETSIZE)
		return -EINVAL;
#endif
	    if(write[i]) FD_SET(insockfd[i], &wfd);
	    else FD_SET(insockfd[i], &rfd);
	    if(maxfd < insockfd[i]) maxfd = insockfd[i];
	}

	/* increment used value _before_ the select in case select
	   modifies tv (as Linux does) */
	used += tv.tv_sec + 1e-6 * tv.tv_usec;

	howmany = R_SelectEx(maxfd+1, &rfd, &wfd, NULL, &tv, NULL);

	if (R_socket_error(howmany)) {
	    return -R_socket_errno();
	}
	if (howmany == 0) {
	    if(mytimeout >= 0 && used >= mytimeout) {
		for (i = 0; i < nsock; i++)
		    ready[i] = 0; /* FALSE */
		return 0;
	    }
	    continue;
	}

	for (i = 0; i < nsock; i++)
	    if ((!write[i] && FD_ISSET(insockfd[i], &rfd)) ||
		(write[i] && FD_ISSET(insockfd[i], &wfd))) {
		ready[i] = 1; /* TRUE */
		nready++;
	    }
	    else ready[i] = 0; /* FALSE */

#ifdef Unix
	if(howmany > nready) {
	    /* one of the extras is ready */
	    InputHandler *what;
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(what != NULL) what->handler((void*) NULL);
	    continue;
	}
#endif
	/* some sockets are ready */
	break;
    }
    return nready;
}

int in_Rsockselect(int nsock, int *insockfd, int *ready, int *write,
		   double timeout)
{
    return R_SocketWaitMultiple(nsock, insockfd, ready, write, timeout);
}

int R_SockConnect(int port, char *host, int timeout)
{
    SOCKET s;
    fd_set wfd, rfd;
#ifdef Win32
    fd_set efd;
#endif
    struct timeval tv;
    int status = 0;
    double used = 0.0;
    struct sockaddr_in server;
    struct hostent *hp;

    check_init();
    s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (R_invalid_socket(s))  return -1;

#define CLOSE_N_RETURN(_ST_) { R_close_socket(s); return(_ST_); }

    if (R_set_nonblocking(s))
	return -1;

    if (! (hp = R_gethostbyname(host))) CLOSE_N_RETURN(-1);

    memcpy((char *)&server.sin_addr, hp->h_addr_list[0], hp->h_length);
    server.sin_port = htons((short)port);
    server.sin_family = AF_INET;

    if (R_socket_error(connect(s, (struct sockaddr *) &server,
                               sizeof(server)))) {

	switch (R_socket_errno()) {
#if !defined(Win32)
	case EINPROGRESS:
	case EWOULDBLOCK:
# if EAGAIN != EWOULDBLOCK
	case EAGAIN:
# endif
#else
	case WSAEINPROGRESS:
	case WSAEWOULDBLOCK:
#endif
	    break;
	default:
	    CLOSE_N_RETURN(-1);
	}
    } else
	return(s);

    while(1) {
	int maxfd = 0;
	R_ProcessEvents();
	set_timeval(&tv, timeout);

#ifdef Unix
	if (s >= FD_SETSIZE) {
	    errno = EINVAL;
	    CLOSE_N_RETURN(-1);
	}
	maxfd = setSelectMask(R_InputHandlers, &rfd);
#else
	FD_ZERO(&rfd);
#endif
	FD_ZERO(&wfd);
	FD_SET(s, &wfd);
#ifdef Win32
	FD_ZERO(&efd);
	FD_SET(s, &efd);
#endif
	if(maxfd < s) maxfd = s;

	/* increment used value _before_ the select in case select
	   modifies tv (as Linux does) */
	used += tv.tv_sec + 1e-6 * tv.tv_usec;

#ifdef Win32
	status = R_SelectEx(maxfd+1, &rfd, &wfd, &efd, &tv, NULL);
#else
	status = R_SelectEx(maxfd+1, &rfd, &wfd, NULL, &tv, NULL);
#endif
	if (R_socket_error(status))
	    /* Ermm.. ?? */
	    CLOSE_N_RETURN(-1);
	    
	if (status == 0) {
	    /* Time out */
	    if(used < timeout) continue;
	    CLOSE_N_RETURN(-1);
	} else if ( FD_ISSET(s, &wfd) ) {
	    R_SOCKLEN_T len;
	    len = sizeof(status);
	    if (getsockopt(s, SOL_SOCKET, SO_ERROR, (char*)&status, &len) < 0){
		/* Solaris error code */
		return (-1);
	    }
	    if ( status ) {
		errno = status;
		CLOSE_N_RETURN(-1);
	    } else return(s);
#ifdef Win32
	} else if ( FD_ISSET(s, &efd) ) {
	    R_SOCKLEN_T len;
	    len = sizeof(status);
	    if (getsockopt(s, SOL_SOCKET, SO_ERROR, (char*)&status, &len) != 0)
		return (-1);
	    errno = status;
	    CLOSE_N_RETURN(-1);
#endif
#ifdef Unix
	} else { /* some other handler needed */
	    InputHandler *what;
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(what != NULL) what->handler((void*) NULL);
	    continue;
#endif
	}
    }
    /* not reached
    return(-1); */
}

int R_SockClose(int sockp)
{
    return R_close_socket(sockp);
}

ssize_t R_SockRead(int sockp, void *buf, size_t len, int blocking, int timeout)
{
    ssize_t res;

    /* EINTR is propagated to the caller (sock_read_helper). When !"blocking",
       the caller expects also EAGAIN/EWOULDBLOCK.

       A known bug in Linux may cause recv() to block with a blocking socket,
       even when select() reported readability. To be robust against spurious
       readability, "sockp" is always non-blocking, even when "blocking" is
       TRUE. */

    for(;;) {
	if(blocking && (res = R_SocketWait(sockp, 0, timeout)) != 0)
	    return res < 0 ? res : 0; /* socket error or timeout */
	res = recv(sockp, buf, len, 0);
	if (R_socket_error((int)res)) {
	    switch(R_socket_errno()) {
#if !defined(Win32)
	    case EWOULDBLOCK:
# if EAGAIN != EWOULDBLOCK
	    case EAGAIN:
# endif
#else
	    case WSAEWOULDBLOCK:
#endif
		if (blocking)
		    /* spurious readability, can happen on Linux */
		    continue;
		/* fall through */
	    default:
		    return -R_socket_errno();
	    }
	} else
	    return res;
    }
}

int R_SockOpen(int port)
{
    check_init();
    return Sock_open((Sock_port_t)port, 0 /* non-blocking */, NULL);
}

int R_SockListen(int sockp, char *buf, int len, int timeout)
{
    fd_set rfd;
    struct timeval tv;
    double used = 0.0;
    int maxfd = 0;
    int status = 0;

    check_init();
    /* The listening socket sockp has been opened in non-blocking mode,
       via R_SockOpen. With a blocking listening socket, there would be a
       race condition between select() and the following accept():
       the connection may be reset by the client just after select() and
       before accept(), which depending on the OS may lead to accept()
       blocking indefinitely, hence timeout not enforced. See chapter
       16.6 of "UNIX Network Programming: The sockets networking API", vol 1,
       Stevens, Fenner, Rudoff.
    */
       
    while(1) {
	R_ProcessEvents();
	set_timeval(&tv, timeout);

#ifdef Unix
	if (sockp >= FD_SETSIZE) {
	    errno = EINVAL;
	    return -1;
	}
	maxfd = setSelectMask(R_InputHandlers, &rfd);
#else
	FD_ZERO(&rfd);
#endif
	FD_SET(sockp, &rfd);
	if(maxfd < sockp) maxfd = sockp;

	/* increment used value _before_ the select in case select
	   modifies tv (as Linux does) */
	double maybe_used = used + tv.tv_sec + 1e-6 * tv.tv_usec;

	status = R_SelectEx(maxfd+1, &rfd, NULL, NULL, &tv, NULL);

	if (R_socket_error_eintr(status))
	    /* do not advance used on EINTR */
	    continue;
	if (R_socket_error(status))
	    return -1;

	used = maybe_used;
	if (status == 0) {
	    /* time out */
	    if (used < timeout) continue;
	    return -1;
	} else if (FD_ISSET(sockp, &rfd)) {
	    /* the socket was ready, but maybe no longer is */
	    struct Sock_error_t perr;
	    perr.error = 0;
	    int s = Sock_listen(sockp, buf, len, &perr);
	    if (s == -1) {
		switch(perr.error) {
#ifndef Win32
		case EINPROGRESS:
		case EWOULDBLOCK:
		case ECONNABORTED:
# if EAGAIN != EWOULDBLOCK
		case EAGAIN:
# endif
		case EPROTO:
#else
		case WSAEINPROGRESS:
		case WSAEWOULDBLOCK:
		case WSAECONNABORTED:
#endif
		    continue;
		default:
		    return -1; /* socket error */
		}
	    }
	    /* got a connection */
	    if (R_set_nonblocking(s))
		return -1;
	    return s;
#ifdef Unix
	} else {
	    /* was one of the extras */
	    InputHandler *what;
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(what != NULL) what->handler((void*) NULL);
	    continue;
#endif
	}
    }
    /* not reached */
}

ssize_t R_SockWrite(int sockp, const void *buf, size_t len, int timeout)
{
    ssize_t res, out = 0;

    /* Rprintf("socket %d writing |%s|\n", sockp, buf); */
    /* This function is not passed a `blocking' argument so the code
       here is equivalent to blocking == TRUE; it's not clear
       non-blocking writes make much sense with the current connection
       interface since there is no way to tell how much, if anything,
       has been written.  LT */
    do {
	if((res = R_SocketWait(sockp, 1, timeout)) != 0)
	    return res < 0 ? res : 0; /* socket error or timeout */
	res = send(sockp, buf, len, 0);
	if (R_socket_error((int)res)) {
	    switch(R_socket_errno()) {
#if !defined(Win32)
	    case EWOULDBLOCK:
# if EAGAIN != EWOULDBLOCK
	    case EAGAIN:
# endif
#else
	    case WSAEWOULDBLOCK:
#endif
		/* Spurious writability to the socket, should not happen. */
		continue;
	    default:
		return -R_socket_errno();
	    }
	} else {
	    { const char *cbuf = buf; cbuf += res; buf = cbuf; }
	    len -= res;
	    out += res;
	}
    } while (/* ! blocking && */len > 0);
    return out;
}

struct hostent *R_gethostbyname(const char *name)
{
    struct hostent *ans = gethostbyname(name);

    /* hard-code IPv4 address for localhost to be robust against
       misconfigured systems */

    if (ans == NULL && !strcmp(name, "localhost"))
	ans = gethostbyname("127.0.0.1");
    return ans;
}

