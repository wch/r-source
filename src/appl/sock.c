/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1998, 1999  Robert Gentleman, Ross Ihaka and the
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

/* Simple sockets interface derived from the sockets UICI
   implementation in Appendix B of Practical UNIX Programming,
   K. A. Robbins and S. Robbins, Prentice Hall, 1996. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(__MWERKS__) && defined(macintosh)
#  define MACINTOSH
#  define EINTR 15
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <errno.h>
#if defined(Win32)
#  include <winsock.h>
#  include <io.h>
#elif defined(MACINTOSH)
#  include <GUSI.h>
#else
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  ifdef HAVE_BSD_NETWORKING
#    include <netdb.h>
#    include <sys/socket.h>
#    include <netinet/in.h>
#    include <netinet/tcp.h>
#  endif
#endif
#include "R_ext/Error.h"
#include "sock.h"

#if defined(__hpux) || defined(MACINTOSH)
   extern int h_errno; /* HP-UX 9.05 and GUSI forget to declare this in netdb.h */
#endif

#define MAXBACKLOG 5

#if defined(Unix) && !defined(HAVE_BSD_NETWORKING)
static char socket_msg[] = "sockets are not available on this system\n";
#endif

static int Sock_error(Sock_error_t perr, int e, int he)
{
  if (perr != NULL) {
    perr->error = e;
    perr->h_error = he;
  }
  return -1;
}

#ifdef MACINTOSH
extern void __sinit(void);
extern int __initialize (void *ignoredParameter);
int __initialize(void *ignoredParameter) {
  __sinit();
  return(0);
}
#endif

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

int Sock_open(Sock_port_t port, Sock_error_t perr)
{
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)
  int sock;
  struct sockaddr_in server;

  if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return Sock_error(perr, errno, 0);

  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons((short)port);

  if ((bind(sock, (struct sockaddr *)&server, sizeof(server)) < 0) ||
      (listen(sock, MAXBACKLOG) < 0))
    return Sock_error(perr, errno, 0);
  return sock;
#else
  error(socket_msg);
  return(-1);
#endif
}

int Sock_listen(int fd, char *cname, int buflen, Sock_error_t perr)
{
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)
  struct sockaddr_in net_client;
  size_t len = sizeof(struct sockaddr);
  int retval;
  struct hostent *hostptr;

  do
    retval = accept(fd, (struct sockaddr *)(&net_client), &len);
  while (retval == -1 && errno == EINTR);
  if (retval == -1)
    return Sock_error(perr, errno, 0);

  if (cname != NULL && buflen > 0) {
    size_t nlen;
    char *name;
    struct in_addr *iaddr = &(net_client.sin_addr);
    hostptr = gethostbyaddr((char *)iaddr, sizeof(struct in_addr), AF_INET);
    name = (hostptr == NULL) ? "unknown" :  hostptr->h_name;
    nlen = strlen(name);
    if (buflen < nlen + 1)
      nlen = buflen - 1;
    strncpy(cname, name, nlen);
    cname[nlen] = 0;
  }
  return retval;
#else
  error(socket_msg);
  return(-1);
#endif
}

int Sock_connect(Sock_port_t port, char *sname, Sock_error_t perr)
{
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)
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
#else
  error(socket_msg);
  return(-1);
#endif
}

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

ssize_t Sock_read(int fd, void *buf, size_t size, Sock_error_t perr)
{
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)
  ssize_t retval;
  do
    retval = recv(fd, buf, size, 0);
  while (retval == -1 && errno == EINTR);
  if (retval == -1)
    return Sock_error(perr, errno, 0);
  else
    return retval;
#else
  error(socket_msg);
  return(-1);
#endif
}

ssize_t Sock_write(int fd, void *buf, size_t size, Sock_error_t perr)
{
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)
  ssize_t retval;
  do
    retval = send(fd, buf, size, 0);
  while (retval == -1 && errno == EINTR);
  if (retval == -1)
    return Sock_error(perr, errno, 0);
  else
    return retval;
#else
  error(socket_msg);
  return(-1);
#endif
}
