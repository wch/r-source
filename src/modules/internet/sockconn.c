/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001-2024   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


/* ------------------- socket connections  --------------------- */

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Rconnections.h>
#include "sock.h"
#include <errno.h>

static void listencleanup(void *data)
{
    int *psock = data;
    R_SockClose(*psock);
}

static Rboolean sock_open(Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;
    int sock, sock1, mlen;
    int timeout = this->timeout;
    char buf[256];

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    this->pend = this->pstart = this->inbuf;

    if(this->server) {
	if (this->serverfd == -1) {
	    sock1 = R_SockOpen(this->port); /* socket(), bind(), listen() */
	    if(sock1 < 0) {
		warning("port %d cannot be opened", this->port);
		return FALSE;
	    }
#ifdef Unix
	    if (sock1 >= FD_SETSIZE) {
		/* R_SockListen below would fail */
		R_SockClose(sock1);
		warning(_("file descriptor is too large for select()"));
		return FALSE;
	    }
#endif
	    {
		RCNTXT cntxt;

		/* set up a context which will close socket on jump. */
		begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv,
			     R_BaseEnv, R_NilValue, R_NilValue);
		cntxt.cend = &listencleanup;
		cntxt.cenddata = &sock1;
		sock = R_SockListen(sock1, buf, 256, timeout); /* accept() */
		endcontext(&cntxt);
	    }
	    R_SockClose(sock1);
	    if(sock < 0) {
		/* NOTE: potentially confusing as the error was in accept() */
		warning("problem in listening on this socket");
		return FALSE;
	    }
	} else {
	    /* accept() */
	    sock = R_SockListen(this->serverfd, buf, 256, timeout);
	    if(sock < 0) {
		/* "accepting" as this is used with socketAccept() */
		warning("problem in accepting connections on this socket");
		return FALSE;
	    }
	}
#ifdef Unix
	if (sock >= FD_SETSIZE && (con->canwrite || con->blocking)) {
	    /* Reading/writing via such socket would fail */
	    R_SockClose(sock);
	    warning(_("file descriptor is too large for select()"));
	    return FALSE;
	}
#endif
	free(con->description);
	size_t sz = strlen(buf) + 10;
	con->description = (char *) malloc(sz); // FIXME check allocation 
	snprintf(con->description, sz, "<-%s:%d", buf, this->port);
    } else {
	sock = R_SockConnect(this->port, con->description, timeout);
	if(sock < 0) {
	    warning("%s:%d cannot be opened", con->description, this->port);
	    return FALSE;
	}
	snprintf(buf, 256, "->%s:%d", con->description, this->port);
	strcpy(con->description, buf);
    }
    this->fd = sock;

    if (this->options & RSC_SET_TCP_NODELAY)
	R_set_nodelay(sock);

    mlen = (int) strlen(con->mode);
    con->isopen = TRUE;
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con); /* OK for output, at least */
    con->save = -1000;
    return TRUE;
}

static void sock_close(Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;
    R_SockClose(this->fd);
    con->isopen = FALSE;
}

static void servsock_close(Rconnection con)
{
    Rservsockconn this = (Rservsockconn)con->private;
    R_SockClose(this->fd);
    con->isopen = FALSE;
}

static ssize_t sock_read_helper(Rconnection con, void *ptr, size_t size)
{
    Rsockconn this = (Rsockconn)con->private;
    ssize_t res;
    size_t nread = 0, n;

    con->incomplete = FALSE;
    do {
	/* read data into the buffer if it's empty and size > 0 */
	if (size > 0 && this->pstart == this->pend) {
	    this->pstart = this->pend = this->inbuf;
	    do
		res = R_SockRead(this->fd, this->inbuf, 4096, 
				 con->blocking, this->timeout);
#ifdef Win32
	    while (-res == WSAEINTR);
	    if (! con->blocking && -res == WSAEWOULDBLOCK) {
#else
	    while (-res == EINTR);
	    if (! con->blocking && (-res == EAGAIN || -res == EWOULDBLOCK)) {
#endif
		con->incomplete = TRUE;
		return nread;
	    }
	    else if (res == 0) /* should mean EOF */
		return nread;
	    else if (res < 0) return res;
	    else this->pend = this->inbuf + res;
	}

	/* copy data from buffer to ptr */
	if (this->pstart + size <= this->pend)
	    n = size;
	else
	    n = this->pend - this->pstart;
	if (n)
	    memcpy(ptr, this->pstart, n);
	ptr = ((char *) ptr) + n;
	this->pstart += n;
	size -= n;
	nread += n;
    } while (size > 0);

    return nread;
}


static int sock_fgetc_internal(Rconnection con)
{
    unsigned char c;
    ssize_t n;

    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t sock_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    ssize_t n = sock_read_helper(con, ptr, size * nitems)/((ssize_t)size);
    return n > 0 ? n : 0;
}

static size_t sock_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;
    ssize_t n = R_SockWrite(this->fd, ptr, (size_t)(size * nitems),
			    this->timeout)/((ssize_t)size);
    return n > 0 ? n : 0;
}

Rconnection in_R_newsock(const char *host, int port, int server, int serverfd,
			 const char * const mode, int timeout, int options)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of socket connection failed"));
    new->class = (char *) malloc(strlen("sockconn") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of socket connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "sockconn");
    new->description = (char *) malloc(strlen(host) + 10);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of socket connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, host, CE_NATIVE, mode);
    new->open = &sock_open;
    new->close = &sock_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &sock_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->read = &sock_read;
    new->write = &sock_write;
    new->private = (void *) malloc(sizeof(struct sockconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of socket connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rsockconn)new->private)-> port = port;
    ((Rsockconn)new->private)-> server = server;
    ((Rsockconn)new->private)-> timeout = timeout;
    ((Rsockconn)new->private)-> serverfd = serverfd;
    ((Rsockconn)new->private)-> options = options;
    return new;
}

Rconnection in_R_newservsock(int port)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of server socket connection failed"));
    new->class = (char *) malloc(strlen("servsockconn") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "servsockconn");
    new->description = (char *) malloc(strlen("localhost") + 10);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of server socket connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, "localhost", CE_NATIVE, "a+");
    new->close = &servsock_close;
    new->private = (void *) malloc(sizeof(struct servsockconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of server socket connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rservsockconn)new->private)-> port = port;

    /* socket(), bind(), listen() */
    int sock = R_SockOpen(port); 
    if(sock < 0) {
	free(new->private); free(new->description); free(new->class); free(new);
	error(_("creation of server socket failed: port %d cannot be opened"),
	      port);
	/* for Solaris 12.5 */ new = NULL;
    }
#ifdef Unix
    if (sock >= FD_SETSIZE) {
	/* R_SockListen (accept) called from sock_open would fail */
	free(new->private); free(new->description); free(new->class); free(new);
	R_SockClose(sock);
	error(_("file descriptor is too large for select()"));
	/* for Solaris 12.5 */ new = NULL;
    }
#endif
    ((Rservsockconn)new->private)-> fd = sock;
    new->isopen = TRUE;

    return new;
}

