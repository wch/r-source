/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-6  The R Core Team.
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

/* <UTF8> the only interpretation of char is ASCII
   <MBCS> only does strchr on ASCII strings, unless user@passwd in URLs
   can be non-ASCII.
 */

/* based on libxml2-2.4.10:
 * nanoftp.c: basic FTP client support
 *
 *  Reference: RFC 959
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

extern void R_ProcessEvents(void);
#if !defined(Unix) || defined(HAVE_BSD_NETWORKING)

#ifdef Win32
#include <io.h>
#include <winsock2.h>
#define _WINSOCKAPI_
#define R_SelectEx(n,rfd,wrd,efd,tv,ih) select(n,rfd,wrd,efd,tv)
#endif

#ifdef HAVE_STRINGS_H
   /* may be needed to define bzero in FD_ZERO (eg AIX) */
  #include <strings.h>
#endif

#include <R_ext/R-ftp-http.h>
/* #define DEBUG_FTP */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_BSD_NETWORKING
#  include <netdb.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
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


#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(snprintf) && defined(HAVE_DECL_SNPRINTF) && !HAVE_DECL_SNPRINTF
extern int snprintf (char *s, size_t n, const char *format, ...);
#endif


#define xmlFree free
#define xmlMalloc malloc
#define xmlMemStrdup strdup


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
	    FD_SET(tmp->fileDescriptor, readMask);
	    maxfd = maxfd < tmp->fileDescriptor ? tmp->fileDescriptor : maxfd;
	}
	tmp = tmp->next;
    }

    return(maxfd);
}
#endif

/**
 * A couple of portability macros
 */
#ifndef _WINSOCKAPI_
#define closesocket(s) close(s)
#define SOCKET int
#endif

#define FTP_COMMAND_OK		200
#define FTP_SYNTAX_ERROR	500
#define FTP_GET_PASSWD		331
#define FTP_BUF_SIZE		1024

typedef struct RxmlNanoFTPCtxt {
    char *protocol;	/* the protocol name */
    char *hostname;	/* the host name */
    int port;		/* the port */
    char *path;		/* the path within the URL */
    char *user;		/* user string */
    char *passwd;	/* passwd string */
    struct sockaddr_in ftpAddr; /* the socket address struct */
    int passive;	/* currently we support only passive !!! */
    SOCKET controlFd;	/* the file descriptor for the control socket */
    SOCKET dataFd;	/* the file descriptor for the data socket */
    int state;		/* WRITE / READ / CLOSED */
    int returnValue;	/* the protocol return value */
    int contentLength;
    /* buffer for data received from the control connection */
    char controlBuf[FTP_BUF_SIZE + 1];
    int controlBufIndex;
    int controlBufUsed;
    int controlBufAnswer;
} RxmlNanoFTPCtxt, *RxmlNanoFTPCtxtPtr;

static int initialized = 0;
static char *proxy = NULL;	/* the proxy name if any */
static int proxyPort = 0;	/* the proxy port if any */
static char *proxyUser = NULL;	/* user for proxy authentication */
static char *proxyPasswd = NULL;/* passwd for proxy authentication */
static int proxyType = 0;	/* uses TYPE or a@b ? */
static unsigned int timeout = 60;/* the select() timeout in seconds */

static void RxmlNanoFTPScanProxy(const char *URL);
static int  RxmlNanoFTPCheckResponse(void *ctx);
static void RxmlNanoFTPFreeCtxt(void * ctx);

/**
 * RxmlNanoFTPInit:
 *
 * Initialize the FTP protocol layer.
 * Currently it just checks for proxy informations,
 * and get the hostname
 */

static void
RxmlNanoFTPInit(void) {
    const char *env;
#ifdef _WINSOCKAPI_
    WSADATA wsaData;
#endif

    if (initialized)
	return;

#ifdef _WINSOCKAPI_
    if (WSAStartup(MAKEWORD(1, 1), &wsaData) != 0)
	return;
#endif

    proxyPort = 21;
    env = getenv("no_proxy");
    if (env && ((env[0] == '*' ) && (env[1] == 0)))
	return;
    env = getenv("ftp_proxy");
    if (env != NULL) {
	RxmlNanoFTPScanProxy(env);
    } else {
	env = getenv("FTP_PROXY");
	if (env != NULL) {
	    RxmlNanoFTPScanProxy(env);
	}
    }
    env = getenv("ftp_proxy_user");
    if (env != NULL) {
	proxyUser = xmlMemStrdup(env);
    }
    env = getenv("ftp_proxy_password");
    if (env != NULL) {
	proxyPasswd = xmlMemStrdup(env);
    }
    initialized = 1;
}

/**
 * RxmlNanoFTPCleanup:
 *
 * Cleanup the FTP protocol layer. This cleanup proxy informations.
 */

void
RxmlNanoFTPCleanup(void) {
    if (proxy != NULL) {
	xmlFree(proxy);
	proxy = NULL;
    }
    if (proxyUser != NULL) {
	xmlFree(proxyUser);
	proxyUser = NULL;
    }
    if (proxyPasswd != NULL) {
	xmlFree(proxyPasswd);
	proxyPasswd = NULL;
    }
#ifdef _WINSOCKAPI_
    if (initialized)
	WSACleanup();
#endif
    initialized = 0;
}


/**
 * RxmlNanoFTPScanURL:
 * @ctx:  an FTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an FTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */

static void
RxmlNanoFTPScanURL(void *ctx, const char *URL) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    const char *cur = URL;
    char buf[4096];
    int indx = 0;
    int port = 0;

    /*
     * Clear any existing data from the context
     */
    if (ctxt->protocol != NULL) {
	xmlFree(ctxt->protocol);
	ctxt->protocol = NULL;
    }
    if (ctxt->hostname != NULL) {
	xmlFree(ctxt->hostname);
	ctxt->hostname = NULL;
    }
    if (ctxt->path != NULL) {
	xmlFree(ctxt->path);
	ctxt->path = NULL;
    }
    if (URL == NULL) return;
    buf[indx] = 0;
    while (*cur != 0) {
	if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
	    buf[indx] = 0;
	    ctxt->protocol = xmlMemStrdup(buf);
	    indx = 0;
	    cur += 3;
	    break;
	}
	if(indx >= 4095)
	    RxmlMessage(2, _("RxmlNanoFTPScanURL: overlong (invalid?) URL"));
	buf[indx++] = *cur++;
    }
    if (*cur == 0) return;

    buf[indx] = 0;
    /* allow user@ and user:pass@ forms */
    {
	const char *p = strchr(cur, '@');
	if(p) {
	    while(1) {
		if(cur[0] == ':' || cur[0] == '@') break;
		if(indx >= 4095)
		    RxmlMessage(2, _("RxmlNanoFTPScanURL: overlong (invalid?) URL"));
		buf[indx++] = *cur++;
	    }
	    buf[indx] = 0;
	    ctxt->user = xmlMemStrdup(buf);
	    indx = 0;
	    if(cur[0] == ':') {
		cur++;
		while(1) {
		    if(cur[0] == '@') break;
		    if(indx >= 4095)
			RxmlMessage(2, _("RxmlNanoFTPScanURL: overlong (invalid?) URL"));
		    buf[indx++] = *cur++;
		}
		buf[indx] = 0;
		ctxt->passwd = xmlMemStrdup(buf);
		indx = 0;
	    }
	    cur = p+1;
	}
    }

    while (1) {
	if (cur[0] == ':') {
	    buf[indx] = 0;
	    ctxt->hostname = xmlMemStrdup(buf);
	    indx = 0;
	    cur += 1;
	    while ((*cur >= '0') && (*cur <= '9')) {
		port *= 10;
		port += *cur - '0';
		cur++;
	    }
	    if (port != 0) ctxt->port = port;
	    while ((cur[0] != '/') && (*cur != 0))
		cur++;
	    break;
	}
	if ((*cur == '/') || (*cur == 0)) {
	    buf[indx] = 0;
	    ctxt->hostname = xmlMemStrdup(buf);
	    indx = 0;
	    break;
	}
	if(indx >= 4095)
	    RxmlMessage(2, _("RxmlNanoFTPScanURL: overlong (invalid?) URL"));
	buf[indx++] = *cur++;
    }
    if (*cur == 0)
	ctxt->path = xmlMemStrdup("/");
    else {
	indx = 0;
	buf[indx] = 0;
	while (*cur != 0) {
	    if(indx >= 4095)
		RxmlMessage(2, _("RxmlNanoFTPScanURL: overlong (invalid?) URL"));
	    buf[indx++] = *cur++;
	}
	buf[indx] = 0;
	ctxt->path = xmlMemStrdup(buf);
    }
}

/**
 * RxmlNanoFTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the FTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like ftp://myproxy/ or ftp://myproxy:3128/
 * A NULL URL cleans up proxy informations.
 */

static void
RxmlNanoFTPScanProxy(const char *URL) {
    const char *cur = URL;
    char buf[4096];
    int indx = 0;
    int port = 0;

    if (proxy != NULL) {
	xmlFree(proxy);
	proxy = NULL;
    }
    /*if (proxyPort != 0) {
	proxyPort = 0;
	}*/
    if (URL == NULL)
	RxmlMessage(0, _("removing FTP proxy info"));
    else
	RxmlMessage(1, _("using FTP proxy '%s'"), URL);
    if (URL == NULL) return;
    buf[indx] = 0;
    while (*cur != 0) {
	if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
	    buf[indx] = 0;
	    indx = 0;
	    cur += 3;
	    break;
	}
	if(indx >= 4095)
	    RxmlMessage(2, _("RxmlNanoFTPScanProxy: overlong (invalid?) URL"));
	buf[indx++] = *cur++;
    }
    if (*cur == 0) return;

    buf[indx] = 0;
    while (1) {
	if (cur[0] == ':') {
	    buf[indx] = 0;
	    proxy = xmlMemStrdup(buf);
	    indx = 0;
	    cur += 1;
	    while ((*cur >= '0') && (*cur <= '9')) {
		port *= 10;
		port += *cur - '0';
		cur++;
	    }
	    if (port != 0) proxyPort = port;
	    while ((cur[0] != '/') && (*cur != 0))
		cur++;
	    break;
	}
	if ((*cur == '/') || (*cur == 0)) {
	    buf[indx] = 0;
	    proxy = xmlMemStrdup(buf);
	    indx = 0;
	    break;
	}
	if(indx >= 4095)
	    RxmlMessage(2, _("RxmlNanoFTPScanProxy: overlong (invalid?) URL"));
	buf[indx++] = *cur++;
    }
}

/**
 * RxmlNanoFTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new FTP context.
 *
 * Returns an FTP context or NULL in case of error.
 */

static void*
RxmlNanoFTPNewCtxt(const char *URL) {
    RxmlNanoFTPCtxtPtr ret;

    ret = (RxmlNanoFTPCtxtPtr) xmlMalloc(sizeof(RxmlNanoFTPCtxt));
    if (ret == NULL) {
	RxmlMessage(1, "error allocating FTP context");
	return(NULL);
    }

    memset(ret, 0, sizeof(RxmlNanoFTPCtxt));
    ret->port = 21;
    ret->passive = 1;
    ret->returnValue = 0;
    ret->contentLength = -1;
    ret->controlBufIndex = 0;
    ret->controlBufUsed = 0;
    ret->controlFd = -1;

    if (URL != NULL)
	RxmlNanoFTPScanURL(ret, URL);

    return(ret);
}

/**
 * RxmlNanoFTPFreeCtxt:
 * @ctx:  an FTP context
 *
 * Frees the context after closing the connection.
 */

static void
RxmlNanoFTPFreeCtxt(void * ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    if (ctxt == NULL) return;
    if (ctxt->hostname != NULL) xmlFree(ctxt->hostname);
    if (ctxt->protocol != NULL) xmlFree(ctxt->protocol);
    if (ctxt->path != NULL) xmlFree(ctxt->path);
    ctxt->passive = 1;
    if (ctxt->controlFd > 2) closesocket(ctxt->controlFd);
    ctxt->controlFd = -1;
    ctxt->controlBufIndex = -1;
    ctxt->controlBufUsed = -1;
    xmlFree(ctxt);
}

/**
 * RxmlNanoFTPParseResponse:
 * @buf:  the buffer containing the response
 * @len:  the buffer length
 *
 * Parsing of the server answer, we just extract the code.
 *
 * returns 0 for errors
 *     +XXX for last line of response
 *     -XXX for response to be continued
 */
static int
RxmlNanoFTPParseResponse(char *buf, int len) {
    int val = 0;

    if (len < 3) return(-1);
    if ((*buf >= '0') && (*buf <= '9'))
	val = val * 10 + (*buf - '0');
    else
	return(0);
    buf++;
    if ((*buf >= '0') && (*buf <= '9'))
	val = val * 10 + (*buf - '0');
    else
	return(0);
    buf++;
    if ((*buf >= '0') && (*buf <= '9'))
	val = val * 10 + (*buf - '0');
    else
	return(0);
    buf++;
    if (*buf == '-')
	return(-val);
    return(val);
}

/**
 * RxmlNanoFTPGetMore:
 * @ctx:  an FTP context
 *
 * Read more information from the FTP control connection
 * Returns the number of bytes read, < 0 indicates an error
 */
static int
RxmlNanoFTPGetMore(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    int len;
    int size;

    if ((ctxt == NULL) || (ctxt->controlFd < 0)) return(-1);

    if ((ctxt->controlBufIndex < 0) || (ctxt->controlBufIndex > FTP_BUF_SIZE)) {
	RxmlMessage(0, "RxmlNanoFTPGetMore : controlBufIndex = %d",
		    ctxt->controlBufIndex);
	return(-1);
    }

    if ((ctxt->controlBufUsed < 0) || (ctxt->controlBufUsed > FTP_BUF_SIZE)) {
	RxmlMessage(0, "RxmlNanoFTPGetMore : controlBufUsed = %d",
	    ctxt->controlBufUsed);
	return(-1);
    }
    if (ctxt->controlBufIndex > ctxt->controlBufUsed) {
	RxmlMessage(0, "RxmlNanoFTPGetMore : controlBufIndex > controlBufUsed %d > %d\n",
	    ctxt->controlBufIndex, ctxt->controlBufUsed);
	return(-1);
    }

    /*
     * First pack the control buffer
     */
    if (ctxt->controlBufIndex > 0) {
	memmove(&ctxt->controlBuf[0], &ctxt->controlBuf[ctxt->controlBufIndex],
		ctxt->controlBufUsed - ctxt->controlBufIndex);
	ctxt->controlBufUsed -= ctxt->controlBufIndex;
	ctxt->controlBufIndex = 0;
    }
    size = FTP_BUF_SIZE - ctxt->controlBufUsed;
    if (size == 0) {
	RxmlMessage(0, "RxmlNanoFTPGetMore : buffer full %d", ctxt->controlBufUsed);
	return(0);
    }

    /*
     * Read the amount left on the control connection
     */
    if ((len = (int) recv(ctxt->controlFd, &ctxt->controlBuf[ctxt->controlBufIndex],
			  size, 0)) < 0) {
	RxmlMessage(1, "recv failed");
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	ctxt->controlFd = -1;
	return(-1);
    }
    RxmlMessage(0, "RxmlNanoFTPGetMore : read %d [%d - %d]", len,
		ctxt->controlBufUsed, ctxt->controlBufUsed + len);
    ctxt->controlBufUsed += len;
    ctxt->controlBuf[ctxt->controlBufUsed] = 0;

    return(len);
}

static void RxmlFindLength(void *ctxt, char *ptr)
{
    char *p, *q;
    int len;

    p = strrchr(ptr, '(');
    if(p) {
	p++;
	q = strchr(p, 'b');
	if(!q || strncmp(q, "bytes)", 6) != 0) return;
	len = atoi(p);
	if(len >=0) ((RxmlNanoFTPCtxtPtr) ctxt)->contentLength = len;
    }
}

/**
 * RxmlNanoFTPReadResponse:
 * @ctx:  an FTP context
 *
 * Read the response from the FTP server after a command.
 * Returns the code number
 */
static int
RxmlNanoFTPReadResponse(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char *ptr, *end;
    int len;
    int res = -1, cur = -1;

    if ((ctxt == NULL) || (ctxt->controlFd < 0)) return(-1);

get_more:
    /*
     * Assumes everything up to controlBuf[controlBufIndex] has been read
     * and analyzed.
     */
    len = RxmlNanoFTPGetMore(ctx);
    if (len < 0) {
	return(-1);
    }
    if ((ctxt->controlBufUsed == 0) && (len == 0)) {
	return(-1);
    }
    ptr = &ctxt->controlBuf[ctxt->controlBufIndex];
    end = &ctxt->controlBuf[ctxt->controlBufUsed];

    RxmlMessage(0, "\n<<<\n%s\n--\n", ptr);
    while (ptr < end) {
	cur = RxmlNanoFTPParseResponse(ptr, (int)(end - ptr));
	if (cur > 0) {
	    /*
	     * Successfully scanned the control code, scratch
	     * till the end of the line, but keep the index to be
	     * able to analyze the result if needed.
	     */
	    res = cur;
	    if(res == 150) RxmlFindLength(ctxt, ptr);
	    ptr += 3;
	    ctxt->controlBufAnswer = (int)(ptr - ctxt->controlBuf);
	    while ((ptr < end) && (*ptr != '\n')) ptr++;
	    if (*ptr == '\n') ptr++;
	    if (*ptr == '\r') ptr++;
	    break;
	}
	while ((ptr < end) && (*ptr != '\n')) ptr++;
	if (ptr >= end) {
	    ctxt->controlBufIndex = ctxt->controlBufUsed;
	    goto get_more;
	}
	if (*ptr != '\r') ptr++;
    }

    if (res < 0) goto get_more;
    ctxt->controlBufIndex = (int)(ptr - ctxt->controlBuf);
    ptr = &ctxt->controlBuf[ctxt->controlBufIndex];
    RxmlMessage(1, "\n---\n%s\n--\n", ptr);
    RxmlMessage(1, "Got %d", res);
    return(res / 100);
}

/**
 * RxmlNanoFTPGetResponse:
 * @ctx:  an FTP context
 *
 * Get the response from the FTP server after a command.
 * Returns the code number
 */

static int
RxmlNanoFTPGetResponse(void *ctx) {
    int res;

    res = RxmlNanoFTPReadResponse(ctx);

    return(res);
}

/**
 * RxmlNanoFTPCheckResponse:
 * @ctx:  an FTP context
 *
 * Check if there is a response from the FTP server after a command.
 * Returns the code number, or 0
 */

static int
RxmlNanoFTPCheckResponse(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    fd_set rfd;
    struct timeval tv;

    if ((ctxt == NULL) || (ctxt->controlFd < 0)) return(-1);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&rfd);
    FD_SET(ctxt->controlFd, &rfd);
    /* no-block select call */
    switch(R_SelectEx(ctxt->controlFd + 1, &rfd, NULL, NULL, &tv, NULL)) {
	case 0:
	    return(0);
	case -1:
#ifdef DEBUG_FTP
	    perror("select");
#endif
	    return(-1);

    }

    return(RxmlNanoFTPReadResponse(ctx));
}

/**
 * Send the user authentication
 */

static int
RxmlNanoFTPSendUser(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;
    ssize_t res;

    if (ctxt->user == NULL)
	snprintf(buf, sizeof(buf), "USER anonymous\r\n");
    else
	snprintf(buf, sizeof(buf), "USER %s\r\n", ctxt->user);
    buf[sizeof(buf) - 1] = 0;
    len = (int) strlen(buf);
    RxmlMessage(0, "%s", buf);
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	RxmlMessage(1, "send failed");
	return(res);
    }
    return(0);
}

/**
 * Send the password authentication
 */

static int
RxmlNanoFTPSendPasswd(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;
    ssize_t res;

    if (ctxt->passwd == NULL)
	snprintf(buf, sizeof(buf), "PASS anonymous@\r\n");
    else
	snprintf(buf, sizeof(buf), "PASS %s\r\n", ctxt->passwd);
    buf[sizeof(buf) - 1] = 0;
    len = (int) strlen(buf);
    RxmlMessage(0, "%s", buf);
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	RxmlMessage(1, "send failed");
	return(res);
    }
    return(0);
}

/**
 * RxmlNanoFTPQuit:
 * @ctx:  an FTP context
 *
 * Send a QUIT command to the server
 *
 * Returns -1 in case of error, 0 otherwise
 */


static int
RxmlNanoFTPQuit(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;

    if ((ctxt == NULL) || (ctxt->controlFd < 0)) return(-1);

    snprintf(buf, sizeof(buf), "QUIT\r\n");
    len = (int) strlen(buf);
    RxmlMessage(0, "%s", buf);
    send(ctxt->controlFd, buf, len, 0);
    return(0);
}

/**
 * RxmlNanoFTPConnect:
 * @ctx:  an FTP context
 *
 * Tries to open a control connection
 *
 * Returns -1 in case of error, 0 otherwise
 */

static int
RxmlNanoFTPConnect(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    struct hostent *hp;
    int port;
    ssize_t res;

    if (ctxt == NULL)
	return(-1);
    if (ctxt->hostname == NULL)
	return(-1);

    /*
     * do the blocking DNS query.
     */
    if (proxy)
	hp = gethostbyname(proxy);
    else
	hp = gethostbyname(ctxt->hostname);
    if (hp == NULL) {
	RxmlMessage(1, _("cannot resolve host"));
	return(-1);
    }


    /*
     * Prepare the socket
     */
    memset(&ctxt->ftpAddr, 0, sizeof(ctxt->ftpAddr));
    ctxt->ftpAddr.sin_family = AF_INET;
    memcpy(&ctxt->ftpAddr.sin_addr, hp->h_addr_list[0], hp->h_length);
    if (proxy) {
	port = proxyPort;
    } else {
	port = ctxt->port;
    }
    if (port == 0)
	port = 21;
    ctxt->ftpAddr.sin_port = htons(port);
    ctxt->controlFd = socket(AF_INET, SOCK_STREAM, 0);
    if (ctxt->controlFd < 0)
	return(-1);

    /*
     * Do the connect.
     */
    if (connect(ctxt->controlFd, (struct sockaddr *) &ctxt->ftpAddr,
		sizeof(struct sockaddr_in)) < 0) {
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	ctxt->controlFd = -1;
	RxmlMessage(1, _("failed to connect to server"));
	return(-1);
    }

    /*
     * Wait for the HELLO from the server.
     */
    res = RxmlNanoFTPGetResponse(ctxt);
    if (res != 2) {
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	ctxt->controlFd = -1;
	RxmlMessage(1, _("failed to get response from server"));
	return(-1);
    }

    /*
     * State diagram for the login operation on the FTP server
     *
     * Reference: RFC 959
     *
     *                       1
     * +---+   USER    +---+------------->+---+
     * | B |---------->| W | 2       ---->| E |
     * +---+           +---+------  |  -->+---+
     *                  | |       | | |
     *                3 | | 4,5   | | |
     *    --------------   -----  | | |
     *   |                      | | | |
     *   |                      | | | |
     *   |                 ---------  |
     *   |               1|     | |   |
     *   V                |     | |   |
     * +---+   PASS    +---+ 2  |  ------>+---+
     * |   |---------->| W |------------->| S |
     * +---+           +---+   ---------->+---+
     *                  | |   | |     |
     *                3 | |4,5| |     |
     *    --------------   --------   |
     *   |                    | |  |  |
     *   |                    | |  |  |
     *   |                 -----------
     *   |             1,3|   | |  |
     *   V                |  2| |  |
     * +---+   ACCT    +---+--  |   ----->+---+
     * |   |---------->| W | 4,5 -------->| F |
     * +---+           +---+------------->+---+
     *
     * Of course in case of using a proxy this get really nasty and is not
     * standardized at all :-(
     */
    if (proxy) {
	int len;
	char buf[400];

	if (proxyUser != NULL) {
	    /*
	     * We need proxy auth
	     */
	    snprintf(buf, sizeof(buf), "USER %s\r\n", proxyUser);
	    buf[sizeof(buf) - 1] = 0;
	    len = (int) strlen(buf);
	    RxmlMessage(0, "%s", buf);
	    res = send(ctxt->controlFd, buf, len, 0);
	    if (res < 0) {
		RxmlMessage(1, "send failed");
		closesocket(ctxt->controlFd);
		ctxt->controlFd = -1;
		return(res);
	    }
	    res = RxmlNanoFTPGetResponse(ctxt);
	    switch (res) {
		case 2:
		    if (proxyPasswd == NULL)
			break;
		case 3:
		    if (proxyPasswd != NULL)
			snprintf(buf, sizeof(buf), "PASS %s\r\n", proxyPasswd);
		    else
			snprintf(buf, sizeof(buf), "PASS anonymous@\r\n");
		    buf[sizeof(buf) - 1] = 0;
		    len = (int) strlen(buf);
		    RxmlMessage(0, "%s", buf);
		    res = send(ctxt->controlFd, buf, len, 0);
		    if (res < 0) {
			RxmlMessage(1, "send failed");
			closesocket(ctxt->controlFd);
			ctxt->controlFd = -1;
			return(res);
		    }
		    res = RxmlNanoFTPGetResponse(ctxt);
		    if (res > 3) {
			closesocket(ctxt->controlFd);
			ctxt->controlFd = -1;
			return(-1);
		    }
		    break;
		case 1:
		    break;
		case 4:
		case 5:
		case -1:
		default:
		    closesocket(ctxt->controlFd);
		    ctxt->controlFd = -1;
		    return(-1);
	    }
	}

	/*
	 * We assume we don't need more authentication to the proxy
	 * and that it succeeded :-\
	 */
	switch (proxyType) {
	    case 0:
		/* we will try in sequence */
	    case 1:
		/* Using SITE command */
		snprintf(buf, sizeof(buf), "SITE %s\r\n", ctxt->hostname);
		buf[sizeof(buf) - 1] = 0;
		len = (int) strlen(buf);
		RxmlMessage(0, "%s", buf);
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    RxmlMessage(1, "send failed");
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = RxmlNanoFTPGetResponse(ctxt);
		if (res == 2) {
		    /* we assume it worked :-\ 1 is error for SITE command */
		    proxyType = 1;
		    break;
		}
		if (proxyType == 1) {
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(-1);
		}
	    case 2:
		/* USER user@host command */
		if (ctxt->user == NULL)
		    snprintf(buf, sizeof(buf), "USER anonymous@%s\r\n",
				   ctxt->hostname);
		else
		    snprintf(buf, sizeof(buf), "USER %s@%s\r\n",
				   ctxt->user, ctxt->hostname);
		buf[sizeof(buf) - 1] = 0;
		len = (int) strlen(buf);
		RxmlMessage(0, "%s", buf);
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    RxmlMessage(1, "send failed");
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = RxmlNanoFTPGetResponse(ctxt);
		if ((res == 1) || (res == 2)) {
		    /* we assume it worked :-\ */
		    proxyType = 2;
		    return(0);
		}
		if (ctxt->passwd == NULL)
		    snprintf(buf, sizeof(buf), "PASS anonymous@\r\n");
		else

		    snprintf(buf, sizeof(buf), "PASS %s\r\n", ctxt->passwd);
		buf[sizeof(buf) - 1] = 0;
		len = (int) strlen(buf);
		RxmlMessage(0, "%s", buf);
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    RxmlMessage(1, "send failed");
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = RxmlNanoFTPGetResponse(ctxt);
		if ((res == 1) || (res == 2)) {
		    /* we assume it worked :-\ */
		    proxyType = 2;
		    return(0);
		}
		if (proxyType == 2) {
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(-1);
		}
	    case 3:
		/*
		 * If you need support for other Proxy authentication scheme
		 * send the code or at least the sequence in use.
		 */
	    default:
		closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		ctxt->controlFd = -1;
		return(-1);
	}
    }
    /*
     * Non-proxy handling.
     */
    res = RxmlNanoFTPSendUser(ctxt);
    if (res < 0) {
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	ctxt->controlFd = -1;
	return(-1);
    }
    res = RxmlNanoFTPGetResponse(ctxt);
    switch (res) {
	case 2:
	    return(0);
	case 3:
	    break;
	case 1:
	case 4:
	case 5:
	case -1:
	default:
	    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	    ctxt->controlFd = -1;
	    return(-1);
    }
    res = RxmlNanoFTPSendPasswd(ctxt);
    if (res < 0) {
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	ctxt->controlFd = -1;
	return(-1);
    }
    res = RxmlNanoFTPGetResponse(ctxt);
    switch (res) {
	case 2:
	    break;
	case 3:
	    RxmlMessage(1, "FTP server asking for ACCNT on anonymous");
	case 1:
	case 4:
	case 5:
	case -1:
	default:
	    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
	    ctxt->controlFd = -1;
	    return(-1);
    }

    return(0);
}



/**
 * RxmlNanoFTPGetConnection:
 * @ctx:  an FTP context
 *
 * Try to open a data connection to the server. Currently only
 * passive mode is supported.
 *
 * Returns -1 in case of error, 0 otherwise
 */

static int
RxmlNanoFTPGetConnection(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char buf[200], *cur;
    int len, i;
    int res;
    unsigned char ad[6], *adp, *portp;
    unsigned int temp[6];
    struct sockaddr_in dataAddr;
    R_SOCKLEN_T dataAddrLen;

    ctxt->dataFd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (ctxt->dataFd < 0) {
	RxmlMessage(2, _("RxmlNanoFTPGetConnection: failed to create socket"));
	return(-1);
    }
    dataAddrLen = sizeof(dataAddr);
    memset(&dataAddr, 0, dataAddrLen);
    dataAddr.sin_family = AF_INET;

    if (ctxt->passive) {
	snprintf (buf, sizeof(buf), "PASV\r\n");
	len = (int) strlen(buf);
#ifdef DEBUG_FTP
	RxmlMessage(0, "%s", buf);
#endif
	res = send(ctxt->controlFd, buf, len, 0);
	if (res < 0) {
	    RxmlMessage(1, "send failed");
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(res);
	}
	res = RxmlNanoFTPReadResponse(ctx);
	if (res != 2) {
	    if (res == 5) {
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
		return(-1);
	    } else {
		/*
		 * retry with an active connection
		 */
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
		ctxt->passive = 0;
	    }
	}
	cur = &ctxt->controlBuf[ctxt->controlBufAnswer];
	while (((*cur < '0') || (*cur > '9')) && *cur != '\0') cur++;
	if (sscanf(cur, "%u,%u,%u,%u,%u,%u", &temp[0], &temp[1], &temp[2],
		    &temp[3], &temp[4], &temp[5]) != 6) {
	    RxmlMessage(1, "Invalid answer to PASV");
	    if (ctxt->dataFd != -1) {
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    }
	    return(-1);
	}
	for (i=0; i<6; i++) ad[i] = (unsigned char) (temp[i] & 0xff);
	memcpy(&dataAddr.sin_addr, &ad[0], 4);
	memcpy(&dataAddr.sin_port, &ad[4], 2);
	if (connect(ctxt->dataFd, (struct sockaddr *) &dataAddr, dataAddrLen) < 0) {
	    RxmlMessage(2, _("failed to create a data connection"));
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
    } else {
	getsockname(ctxt->dataFd, (struct sockaddr *) &dataAddr, &dataAddrLen);
	dataAddr.sin_port = 0;
	if (bind(ctxt->dataFd, (struct sockaddr *) &dataAddr, dataAddrLen) < 0) {
	    RxmlMessage(2, _("failed to bind a port"));
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
	getsockname(ctxt->dataFd, (struct sockaddr *) &dataAddr, &dataAddrLen);

	if (listen(ctxt->dataFd, 1) < 0) {
	    RxmlMessage(2, _("could not listen on port %d"),
			ntohs(dataAddr.sin_port));
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
	adp = (unsigned char *) &dataAddr.sin_addr;
	portp = (unsigned char *) &dataAddr.sin_port;
	snprintf(buf, sizeof(buf), "PORT %d,%d,%d,%d,%d,%d\r\n",
	       adp[0] & 0xff, adp[1] & 0xff, adp[2] & 0xff, adp[3] & 0xff,
	       portp[0] & 0xff, portp[1] & 0xff);
	buf[sizeof(buf) - 1] = 0;
	len = (int) strlen(buf);
#ifdef DEBUG_FTP
	RxmlMessage(1, "%s", buf);
#endif

	res = send(ctxt->controlFd, buf, len, 0);
	if (res < 0) {
	    RxmlMessage(1, "send failed");
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(res);
	}
	res = RxmlNanoFTPGetResponse(ctxt);
	if (res != 2) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
	}
    }
    return(ctxt->dataFd);

}



/**
 * RxmlNanoFTPGetSocket:
 * @ctx:  an FTP context
 * @filename:  the file to retrieve (or NULL if path is in context).
 *
 * Initiate fetch of the given file from the server.
 *
 * Returns the socket for the data connection, or <0 in case of error
 */


static int
RxmlNanoFTPGetSocket(void *ctx, const char *filename) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    char buf[300];
    int res, len;
    if (ctx == NULL)
	return(-1);
    if ((filename == NULL) && (ctxt->path == NULL))
	return(-1);
    ctxt->dataFd = RxmlNanoFTPGetConnection(ctxt);
    if (ctxt->dataFd == -1)
	return(-1);

    snprintf(buf, sizeof(buf), "TYPE I\r\n");
    len = (int) strlen(buf);
#ifdef DEBUG_FTP
    RxmlMessage(0, "%s", buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	RxmlMessage(1, "send failed");
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(res);
    }
    res = RxmlNanoFTPReadResponse(ctxt);
    if (res != 2) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(-res);
    }
    if (filename == NULL)
	snprintf(buf, sizeof(buf), "RETR %s\r\n", ctxt->path);
    else
	snprintf(buf, sizeof(buf), "RETR %s\r\n", filename);
    buf[sizeof(buf) - 1] = 0;
    len = (int) strlen(buf);
#ifdef DEBUG_FTP
    RxmlMessage(0, "%s", buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	RxmlMessage(1, "send failed");
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(res);
    }
    res = RxmlNanoFTPReadResponse(ctxt);
    if (res != 1) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(-res);
    }
    return(ctxt->dataFd);
}

/**
 * RxmlNanoFTPRead:
 * @ctx:  the FTP context
 * @dest:  a buffer
 * @len:  the buffer length
 *
 * This function tries to read @len bytes from the existing FTP connection
 * and saves them in @dest. This is a non-blocking version of the original.
 *
 * Returns the number of byte read. 0 is an indication of an end of connection.
 *         -1 indicates a parameter error.
 */

int
RxmlNanoFTPRead(void *ctx, void *dest, int len)
{
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;
    int got = 0, res;
    fd_set rfd;
    struct timeval tv;
    double used = 0.0;

    if (ctx == NULL) return(-1);
    if (ctxt->dataFd < 0) return(0);
    if (dest == NULL) return(-1);
    if (len <= 0) return(0);
    while(1) {
	int maxfd = 0;
	R_ProcessEvents();
#ifdef Unix
	if(R_wait_usec > 0) {
	    tv.tv_sec = 0;
	    tv.tv_usec = R_wait_usec;
	} else {
	    tv.tv_sec = 1;
	    tv.tv_usec = 0;
	}
#elif defined(Win32)
	tv.tv_sec = 0;
	tv.tv_usec = 2e5;
#else
	tv.tv_sec = 1;
	tv.tv_usec = 0;
#endif

#ifdef Unix
	maxfd = setSelectMask(R_InputHandlers, &rfd);
#else
	FD_ZERO(&rfd);
#endif
	FD_SET(ctxt->dataFd, &rfd);
	if(maxfd < ctxt->dataFd) maxfd = ctxt->dataFd;

	res = R_SelectEx(maxfd + 1, &rfd, NULL, NULL, &tv, NULL);

	if (res < 0) { /* socket error */
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
	}
	if (res == 0) { /* timeout, no data available yet */
	    used += tv.tv_sec + 1e-6 * tv.tv_usec;
	    if (used > timeout) return(0);
	    res = RxmlNanoFTPCheckResponse(ctxt);
	    if (res < 0) {
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
		ctxt->dataFd = -1;
		return(-1);
	    }
	    if (res == 2) {
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
		return(0);
	    }
	    continue;
	}
	/* one or more fd is ready */
#ifdef Unix
	if(!FD_ISSET(ctxt->dataFd, &rfd) || res > 1) {
	    /* was one of the extras */
	    InputHandler *what;
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(what != NULL) what->handler((void*) NULL);
	    continue;
	}
#endif

	/* was the socket */
	got = recv(ctxt->dataFd, dest, len, 0);
	if (got < 0) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
	} else  break;
    }
    return got;
}


/**
 * RxmlNanoFTPOpen:
 * @URL: the URL to the resource
 *
 * Start to fetch the given ftp:// resource
 *
 * Returns an FTP context, or NULL
 */

void*
RxmlNanoFTPOpen(const char *URL) {
    RxmlNanoFTPCtxtPtr ctxt;
    int sock;

    RxmlNanoFTPInit();
    if (URL == NULL) return(NULL);
    if (strncmp("ftp://", URL, 6)) return(NULL);

    ctxt = (RxmlNanoFTPCtxtPtr) RxmlNanoFTPNewCtxt(URL);
    if (ctxt == NULL) return(NULL);
    if (RxmlNanoFTPConnect(ctxt) < 0) {
	RxmlNanoFTPFreeCtxt(ctxt);
	return(NULL);
    }
    sock = RxmlNanoFTPGetSocket(ctxt, ctxt->path);
    if (sock < 0) {
	RxmlNanoFTPFreeCtxt(ctxt);
	return(NULL);
    }
    return(ctxt);
}

/**
 * RxmlNanoFTPClose:
 * @ctx: an FTP context
 *
 * Close the connection and both control and transport
 *
 * Returns -1 incase of error, 0 otherwise
 */

int
RxmlNanoFTPClose(void *ctx) {
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;

    if (ctxt == NULL)
	return(-1);

    if (ctxt->dataFd >= 0) {
	closesocket(ctxt->dataFd);
	ctxt->dataFd = -1;
    }
    if (ctxt->controlFd >= 0) {
	RxmlNanoFTPQuit(ctxt);
	closesocket(ctxt->controlFd);
	ctxt->controlFd = -1;
    }
    RxmlNanoFTPFreeCtxt(ctxt);
    return(0);
}

/**
 * RxmlNanoFTPTimeout:
 * @delay:  the delay in seconds
 *
 * Set the FTP timeout, (default is 60secs).  0 means immediate
 * return, while -1 infinite.
 */

void
RxmlNanoFTPTimeout(int delay) {
    timeout = (unsigned int) delay;
}

int
RxmlNanoFTPContentLength(void *ctx)
{
    RxmlNanoFTPCtxtPtr ctxt = (RxmlNanoFTPCtxtPtr) ctx;

    if (ctxt == NULL) return(-1);
    return(ctxt->contentLength);
}

#endif /* !Unix or BSD_NETWORKING */
