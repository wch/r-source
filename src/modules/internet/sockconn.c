/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001-12   The R Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_SOCKETS


/* ------------------- socket connections  --------------------- */

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
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
	sock1 = R_SockOpen(this->port);
	if(sock1 < 0) {
	    warning("port %d cannot be opened", this->port);
	    return FALSE;
	}
	{
	    RCNTXT cntxt;

	    /* set up a context which will close socket on jump. */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv,
			 R_BaseEnv, R_NilValue, R_NilValue);
	    cntxt.cend = &listencleanup;
	    cntxt.cenddata = &sock1;
	    sock = R_SockListen(sock1, buf, 256, timeout);
	    endcontext(&cntxt);
	}
	if(sock < 0) {
	    warning("problem in listening on this socket");
	    R_SockClose(sock1);
	    return FALSE;
	}
	free(con->description);
	con->description = (char *) malloc(strlen(buf) + 10);
	sprintf(con->description, "<-%s:%d", buf, this->port);
	R_SockClose(sock1);
    } else {
	sock = R_SockConnect(this->port, con->description, timeout);
	if(sock < 0) {
	    warning("%s:%d cannot be opened", con->description, this->port);
	    return FALSE;
	}
	sprintf(buf, "->%s:%d", con->description, this->port);
	strcpy(con->description, buf);
    }
    this->fd = sock;

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

static size_t sock_read_helper(Rconnection con, void *ptr, size_t size)
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
	    while (-res == EINTR);
	    if (! con->blocking && -res == EAGAIN) {
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
    size_t n;

    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t sock_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    return sock_read_helper(con, ptr, size * nitems)/size;
}

static size_t sock_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;

    return R_SockWrite(this->fd, ptr, (int)(size * nitems), this->timeout)/size;
}

Rconnection in_R_newsock(const char *host, int port, int server,
			 const char * const mode, int timeout)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of socket connection failed"));
    new->class = (char *) malloc(strlen("sockconn") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of socket connection failed"));
    }
    strcpy(new->class, "sockconn");
    new->description = (char *) malloc(strlen(host) + 10);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of socket connection failed"));
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
    }
    ((Rsockconn)new->private)-> port = port;
    ((Rsockconn)new->private)-> server = server;
    ((Rsockconn)new->private)-> timeout = timeout;
    return new;
}

#endif /* HAVE_SOCKETS */
