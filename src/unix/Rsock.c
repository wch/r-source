#define NULL 0
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>
#include <unistd.h>
#include "sock.h"

#ifndef OPEN_MAX
#  define OPEN_MAX 64
#endif

static int sock[OPEN_MAX];
static int sock_inited = 0;

#define SOCK_MAX OPEN_MAX

/* -Wall: not used
static void cleanup(void)
{
  int i;
  for (i = 0; i < SOCK_MAX; i++)
    if (sock[i] != -1) {
      Sock_close(sock[i], NULL);
      sock[i] = -1;
    }
}
*/

static int enter_sock(int fd)
{
#ifdef DEBUG
    printf("entersock(%d)\n",fd);
#endif
    if (fd == -1)
	return 0;
    else {
	int i;
	for (i = 0; i < SOCK_MAX; i++)
	    if (sock[i] == -1) {
		sock[i] = fd;
		return fd;
	    }
	Sock_close(fd, NULL);
	return 0;
    }
}

static int close_sock(int fd)
{
    int i;
    for (i = 0; i < SOCK_MAX; i++)
	if (sock[i] == fd) {
	    sock[i] = -1;
	    return Sock_close(fd, NULL) == -1 ? 0 : 1 ;
	}
    return 0;
}

static void check_init(void)
{
    if (! sock_inited) {
	int i;
	for (i = 0; i < SOCK_MAX; i++)
	    sock[i] = -1;
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

void Rsocklisten(int *sock,char **buf, int *len)
{
    check_init();
    *sock = enter_sock(Sock_listen(*sock, *buf , *len, NULL));
}

void Rsockconnect(int *port, char **host)
{
    check_init();
#ifdef DEBUG
    printf("connect to %d at %s\n",*port, *host);
#endif
    *port = enter_sock(Sock_connect(*port, *host, NULL));
}

void Rsockclose(int *sock )
{
    *sock = close_sock(*sock);
}

void Rsockread(int *sock, char **buf, int *maxlen)
{
    check_init();
#ifdef DEBUG
    printf("Reading from %d\n",*sock);
#endif
    *maxlen = (int) Sock_read(*sock, *buf, *maxlen, NULL);
}

void Rsockwrite(int *sock, char **buf, int *start, int *end, int *len)
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
    printf("writing %s to %d",*buf,*sock);
#endif
    n = Sock_write(*sock, *buf + *start, *end - *start, NULL);
    *len = (int) n;
}

#ifndef Win32
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
