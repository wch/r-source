/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)  2001   The R Development Core Team.
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

#ifdef HAVE_SOCKETS

#include <R_ext/R-ftp-http.h>



/* ------------------- socket connections  --------------------- */

#include <Defn.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>

static Rboolean sock_open(Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;
    int sock, sock1, mlen;
    int timeout = asInteger(GetOption(install("timeout"), R_NilValue));
    char buf[256];

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    R_SockTimeout(timeout);
    this->pend = this->pstart = this->inbuf;

    if(this->server) {
	sock1 = R_SockOpen(this->port);
	if(sock1 < 0) {
	    warning("port %d cannot be opened", this->port);
	    return FALSE;
	}
	sock = R_SockListen(sock1, buf, 256);
	if(sock < 0) {
	    warning("problem in listening on this socket");
	    return FALSE;
	}
	free(con->description);
	con->description = (char *) malloc(strlen(buf) + 10);
	sprintf(con->description, "<-%s:%d", buf, this->port);
	R_SockClose(sock1);
    } else {
	sock = R_SockConnect(this->port, con->description);
	if(sock < 0) {
	    warning("%s:%d cannot be opened", con->description, this->port);
	    return FALSE;
	}
	sprintf(buf, "->%s:%d", con->description, this->port);
	strcpy(con->description, buf);
    }
    this->fd = sock;
    
    mlen = strlen(con->mode);
    con->isopen = TRUE;
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    return TRUE;
}

static void sock_close(Rconnection con)
{
    Rsockconn this = (Rsockconn)con->private;
    R_SockClose(this->fd);
    con->isopen = FALSE;
}

static int sock_read_helper(Rconnection con, void *ptr, size_t size)
{
    Rsockconn this = (Rsockconn)con->private;
    int res;
    int nread = 0, n;

    do {
	/* read data into the buffer if it's empty and size > 0 */
	if (size > 0 && this->pstart == this->pend) {
	    this->pstart = this->pend = this->inbuf;
	    do
		res = R_SockRead(this->fd, this->inbuf, 4096, con->blocking);
	    while (-res == EINTR);
	    if (! con->blocking && -res == EAGAIN) {
		con->incomplete = TRUE;
		return nread > 0 ? nread : res;
	    }
	    else if (con->blocking && res == 0) /* should mean EOF */
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

    con->incomplete = FALSE;
    return nread;
}


static int sock_fgetc(Rconnection con)
{
    unsigned char c;
    int n;
  
    n = sock_read_helper(con, (char *)&c, 1);
    return (n == 1) ? con->encoding[c] : R_EOF;
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

    return R_SockWrite(this->fd, ptr, size * nitems)/size;
}

Rconnection in_R_newsock(char *host, int port, int server, char *mode)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of file connection failed");
    new->class = (char *) malloc(strlen("socket") + 1);
    if(!new->class) {
	free(new);
	error("allocation of socket connection failed");
    }
    strcpy(new->class, "socket");
    new->description = (char *) malloc(strlen(host) + 10);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of socket connection failed");
    }
    init_con(new, host, mode);
    new->open = &sock_open;
    new->close = &sock_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &sock_fgetc;
    new->read = &sock_read;
    new->write = &sock_write;
    new->private = (void *) malloc(sizeof(struct sockconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of socket connection failed");
    }
    ((Rsockconn)new->private)-> port = port;
    ((Rsockconn)new->private)-> server = server;
    return new;
}

#endif /* HAVE_SOCKETS */
