/*
 * nanoftp.c: basic FTP client support
 *
 *  Reference: RFC 959
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef Win32
#define INCLUDE_WINSOCK
#include "win32config.h"
#define _WINSOCKAPI_
#endif

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
#  include <netinet/tcp.h>
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
#ifdef HAVE_STRINGS_H
#include <strings.h>
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
	FD_SET(tmp->fileDescriptor, readMask);
	maxfd = maxfd < tmp->fileDescriptor ? tmp->fileDescriptor : maxfd;
	tmp = tmp->next;
    }

    return(maxfd);
}
#endif

#include <R_ext/Error.h>
#define err warning

/* #define DEBUG_FTP 1  */

/**
 * A couple of portability macros
 */
#ifndef _WINSOCKAPI_
#define closesocket(s) close(s)
#define SOCKET int
#endif

static char hostname[100];

#define FTP_COMMAND_OK		200
#define FTP_SYNTAX_ERROR	500
#define FTP_GET_PASSWD		331
#define FTP_BUF_SIZE		512

typedef struct xmlNanoFTPCtxt {
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
    /* buffer for data received from the control connection */
    char controlBuf[FTP_BUF_SIZE + 1];
    int controlBufIndex;
    int controlBufUsed;
    int controlBufAnswer;
} xmlNanoFTPCtxt, *xmlNanoFTPCtxtPtr;

static int initialized = 0;
static char *proxy = NULL;	/* the proxy name if any */
static int proxyPort = 0;	/* the proxy port if any */
static char *proxyUser = NULL;	/* user for proxy authentication */
static char *proxyPasswd = NULL;/* passwd for proxy authentication */
static int proxyType = 0;	/* uses TYPE or a@b ? */
static unsigned int timeout = 60;/* the select() timeout in seconds */

static void	xmlNanoFTPScanProxy	(const char *URL);

/**
 * xmlNanoFTPInit:
 *
 * Initialize the FTP protocol layer.
 * Currently it just checks for proxy informations,
 * and get the hostname
 */

static void
xmlNanoFTPInit(void) {
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

    gethostname(hostname, sizeof(hostname));

    proxyPort = 21;
    env = getenv("no_proxy");
    if (env != NULL)
	return;
    env = getenv("ftp_proxy");
    if (env != NULL) {
	xmlNanoFTPScanProxy(env);
    } else {
	env = getenv("FTP_PROXY");
	if (env != NULL) {
	    xmlNanoFTPScanProxy(env);
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
 * xmlNanoFTPCleanup:
 *
 * Cleanup the FTP protocol layer. This cleanup proxy informations.
 */

void
xmlNanoFTPCleanup(void) {
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
    hostname[0] = 0;
#ifdef _WINSOCKAPI_
    if (initialized)
	WSACleanup();
#endif
    initialized = 0;
}


/**
 * xmlNanoFTPScanURL:
 * @ctx:  an FTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an FTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */

static void
xmlNanoFTPScanURL(void *ctx, const char *URL) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    const char *cur = URL;
    char buf[4096];
    int index = 0;
    int port = 0;

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
    buf[index] = 0;
    while (*cur != 0) {
        if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
	    buf[index] = 0;
	    ctxt->protocol = xmlMemStrdup(buf);
	    index = 0;
            cur += 3;
	    break;
	}
	buf[index++] = *cur++;
    }
    if (*cur == 0) return;

    buf[index] = 0;
    while (1) {
        if (cur[0] == ':') {
	    buf[index] = 0;
	    ctxt->hostname = xmlMemStrdup(buf);
	    index = 0;
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
	    buf[index] = 0;
	    ctxt->hostname = xmlMemStrdup(buf);
	    index = 0;
	    break;
	}
	buf[index++] = *cur++;
    }
    if (*cur == 0)
        ctxt->path = xmlMemStrdup("/");
    else {
        index = 0;
        buf[index] = 0;
	while (*cur != 0)
	    buf[index++] = *cur++;
	buf[index] = 0;
	ctxt->path = xmlMemStrdup(buf);
    }
}

/**
 * xmlNanoFTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the FTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like ftp://myproxy/ or ftp://myproxy:3128/
 * A NULL URL cleans up proxy informations.
 */

static void
xmlNanoFTPScanProxy(const char *URL) {
    const char *cur = URL;
    char buf[4096];
    int index = 0;
    int port = 0;

    if (proxy != NULL) {
        xmlFree(proxy);
	proxy = NULL;
    }
    if (proxyPort != 0) {
	proxyPort = 0;
    }
#ifdef DEBUG_FTP
    if (URL == NULL)
	err("Removing FTP proxy info\n");
    else
	err("Using FTP proxy %s\n", URL);
#endif
    if (URL == NULL) return;
    buf[index] = 0;
    while (*cur != 0) {
        if ((cur[0] == ':') && (cur[1] == '/') && (cur[2] == '/')) {
	    buf[index] = 0;
	    index = 0;
            cur += 3;
	    break;
	}
	buf[index++] = *cur++;
    }
    if (*cur == 0) return;

    buf[index] = 0;
    while (1) {
        if (cur[0] == ':') {
	    buf[index] = 0;
	    proxy = xmlMemStrdup(buf);
	    index = 0;
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
	    buf[index] = 0;
	    proxy = xmlMemStrdup(buf);
	    index = 0;
	    break;
	}
	buf[index++] = *cur++;
    }
}

/**
 * xmlNanoFTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new FTP context.
 *
 * Returns an FTP context or NULL in case of error.
 */

static void*
xmlNanoFTPNewCtxt(const char *URL) {
    xmlNanoFTPCtxtPtr ret;

    ret = (xmlNanoFTPCtxtPtr) xmlMalloc(sizeof(xmlNanoFTPCtxt));
    if (ret == NULL) return(NULL);

    memset(ret, 0, sizeof(xmlNanoFTPCtxt));
    ret->port = 21;
    ret->passive = 1;
    ret->returnValue = 0;
    ret->controlBufIndex = 0;
    ret->controlBufUsed = 0;

    if (URL != NULL)
	xmlNanoFTPScanURL(ret, URL);

    return(ret);
}

/**
 * xmlNanoFTPFreeCtxt:
 * @ctx:  an FTP context
 *
 * Frees the context after closing the connection.
 */

void
xmlNanoFTPFreeCtxt(void * ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    if (ctxt == NULL) return;
    if (ctxt->hostname != NULL) xmlFree(ctxt->hostname);
    if (ctxt->protocol != NULL) xmlFree(ctxt->protocol);
    if (ctxt->path != NULL) xmlFree(ctxt->path);
    ctxt->passive = 1;
    if (ctxt->controlFd >= 0) closesocket(ctxt->controlFd);
    ctxt->controlFd = -1;
    ctxt->controlBufIndex = -1;
    ctxt->controlBufUsed = -1;
    xmlFree(ctxt);
}

/**
 * xmlNanoFTPParseResponse:
 * @ctx:  the FTP connection context
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
xmlNanoFTPParseResponse(void *ctx, char *buf, int len) {
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
 * xmlNanoFTPGetMore:
 * @ctx:  an FTP context
 *
 * Read more information from the FTP control connection
 * Returns the number of bytes read, < 0 indicates an error
 */
static int
xmlNanoFTPGetMore(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    int len;
    int size;

    if ((ctxt->controlBufIndex < 0) || (ctxt->controlBufIndex > FTP_BUF_SIZE)) {
#ifdef DEBUG_FTP
        err("xmlNanoFTPGetMore : controlBufIndex = %d\n",
	    ctxt->controlBufIndex);
#endif
	return(-1);
    }

    if ((ctxt->controlBufUsed < 0) || (ctxt->controlBufUsed > FTP_BUF_SIZE)) {
#ifdef DEBUG_FTP
        err("xmlNanoFTPGetMore : controlBufUsed = %d\n",
	    ctxt->controlBufUsed);
#endif
	return(-1);
    }
    if (ctxt->controlBufIndex > ctxt->controlBufUsed) {
#ifdef DEBUG_FTP
        err("xmlNanoFTPGetMore : controlBufIndex > controlBufUsed %d > %d\n",
	    ctxt->controlBufIndex, ctxt->controlBufUsed);
#endif
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
#ifdef DEBUG_FTP
        err("xmlNanoFTPGetMore : buffer full %d \n", ctxt->controlBufUsed);
#endif
	return(0);
    }

    /*
     * Read the amount left on the control connection
     */
    if ((len = recv(ctxt->controlFd, &ctxt->controlBuf[ctxt->controlBufIndex],
		    size, 0)) < 0) {
	closesocket(ctxt->controlFd); ctxt->controlFd = -1;
        ctxt->controlFd = -1;
        return(-1);
    }
#ifdef DEBUG_FTP
    err("xmlNanoFTPGetMore : read %d [%d - %d]\n", len,
	ctxt->controlBufUsed, ctxt->controlBufUsed + len);
#endif
    ctxt->controlBufUsed += len;
    ctxt->controlBuf[ctxt->controlBufUsed] = 0;

    return(len);
}

/**
 * xmlNanoFTPReadResponse:
 * @ctx:  an FTP context
 *
 * Read the response from the FTP server after a command.
 * Returns the code number
 */
static int
xmlNanoFTPReadResponse(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char *ptr, *end;
    int len;
    int res = -1, cur = -1;

get_more:
    /*
     * Assumes everything up to controlBuf[controlBufIndex] has been read
     * and analyzed.
     */
    len = xmlNanoFTPGetMore(ctx);
    if (len < 0) {
        return(-1);
    }
    if ((ctxt->controlBufUsed == 0) && (len == 0)) {
        return(-1);
    }
    ptr = &ctxt->controlBuf[ctxt->controlBufIndex];
    end = &ctxt->controlBuf[ctxt->controlBufUsed];

#ifdef DEBUG_FTP
    err("\n<<<\n%s\n--\n", ptr);
#endif
    while (ptr < end) {
        cur = xmlNanoFTPParseResponse(ctxt, ptr, end - ptr);
	if (cur > 0) {
	    /*
	     * Successfully scanned the control code, scratch
	     * till the end of the line, but keep the index to be
	     * able to analyze the result if needed.
	     */
	    res = cur;
	    ptr += 3;
	    ctxt->controlBufAnswer = ptr - ctxt->controlBuf;
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
    ctxt->controlBufIndex = ptr - ctxt->controlBuf;
#ifdef DEBUG_FTP
    ptr = &ctxt->controlBuf[ctxt->controlBufIndex];
    err("\n---\n%s\n--\n", ptr);
#endif

#ifdef DEBUG_FTP
    err("Got %d\n", res);
#endif
    return(res / 100);
}

/**
 * xmlNanoFTPGetResponse:
 * @ctx:  an FTP context
 *
 * Get the response from the FTP server after a command.
 * Returns the code number
 */

static int
xmlNanoFTPGetResponse(void *ctx) {
    int res;

    res = xmlNanoFTPReadResponse(ctx);

    return(res);
}

/**
 * xmlNanoFTPCheckResponse:
 * @ctx:  an FTP context
 *
 * Check if there is a response from the FTP server after a command.
 * Returns the code number, or 0
 */

int
xmlNanoFTPCheckResponse(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    fd_set rfd;
    struct timeval tv;

    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&rfd);
    FD_SET(ctxt->controlFd, &rfd);
    /* no-block select call */
    switch(select(ctxt->controlFd + 1, &rfd, NULL, NULL, &tv)) {
	case 0:
	    return(0);
	case -1:
#ifdef DEBUG_FTP
	    perror("select");
#endif
	    return(-1);

    }

    return(xmlNanoFTPReadResponse(ctx));
}

/**
 * Send the user authentification
 */

static int
xmlNanoFTPSendUser(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;
    int res;

    if (ctxt->user == NULL)
	sprintf(buf, "USER anonymous\r\n");
    else
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "USER %s\r\n", ctxt->user);
#else
	sprintf(buf, "USER %s\r\n", ctxt->user);
#endif
    buf[sizeof(buf) - 1] = 0;
    len = strlen(buf);
#ifdef DEBUG_FTP
    err(buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) return(res);
    return(0);
}

/**
 * Send the password authentification
 */

static int
xmlNanoFTPSendPasswd(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;
    int res;

    if (ctxt->passwd == NULL)
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "PASS libxml@%s\r\n", hostname);
#else
	sprintf(buf, "PASS libxml@%s\r\n", hostname);
#endif
    else
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "PASS %s\r\n", ctxt->passwd);
#else
	sprintf(buf, "PASS %s\r\n", ctxt->passwd);
#endif
    buf[sizeof(buf) - 1] = 0;
    len = strlen(buf);
#ifdef DEBUG_FTP
    err(buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) return(res);
    return(0);
}

/**
 * xmlNanoFTPQuit:
 * @ctx:  an FTP context
 *
 * Send a QUIT command to the server
 *
 * Returns -1 in case of error, 0 otherwise
 */


static int
xmlNanoFTPQuit(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char buf[200];
    int len;
    int res;

    sprintf(buf, "QUIT\r\n");
    len = strlen(buf);
#ifdef DEBUG_FTP
    err(buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    return(0);
}

/**
 * xmlNanoFTPConnect:
 * @ctx:  an FTP context
 *
 * Tries to open a control connection
 *
 * Returns -1 in case of error, 0 otherwise
 */

static int
xmlNanoFTPConnect(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    struct hostent *hp;
    int port;
    int res;

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
    if (hp == NULL)
        return(-1);

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
	return(-1);
    }

    /*
     * Wait for the HELLO from the server.
     */
    res = xmlNanoFTPGetResponse(ctxt);
    if (res != 2) {
        closesocket(ctxt->controlFd); ctxt->controlFd = -1;
        ctxt->controlFd = -1;
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
#ifdef HAVE_SNPRINTF
	    snprintf(buf, sizeof(buf), "USER %s\r\n", proxyUser);
#else
	    sprintf(buf, "USER %s\r\n", proxyUser);
#endif
            buf[sizeof(buf) - 1] = 0;
            len = strlen(buf);
#ifdef DEBUG_FTP
	    err(buf);
#endif
	    res = send(ctxt->controlFd, buf, len, 0);
	    if (res < 0) {
		closesocket(ctxt->controlFd);
		ctxt->controlFd = -1;
	        return(res);
	    }
	    res = xmlNanoFTPGetResponse(ctxt);
	    switch (res) {
		case 2:
		    if (proxyPasswd == NULL)
			break;
		case 3:
		    if (proxyPasswd != NULL)
#ifdef HAVE_SNPRINTF
			snprintf(buf, sizeof(buf), "PASS %s\r\n", proxyPasswd);
#else
			sprintf(buf, "PASS %s\r\n", proxyPasswd);
#endif
		    else
#ifdef HAVE_SNPRINTF
			snprintf(buf, sizeof(buf), "PASS libxml@%s\r\n",
			               hostname);
#else
			sprintf(buf, "PASS libxml@%s\r\n", hostname);
#endif
                    buf[sizeof(buf) - 1] = 0;
                    len = strlen(buf);
#ifdef DEBUG_FTP
		    err(buf);
#endif
		    res = send(ctxt->controlFd, buf, len, 0);
		    if (res < 0) {
			closesocket(ctxt->controlFd);
			ctxt->controlFd = -1;
			return(res);
		    }
		    res = xmlNanoFTPGetResponse(ctxt);
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
		/* we will try in seqence */
	    case 1:
		/* Using SITE command */
#ifdef HAVE_SNPRINTF
		snprintf(buf, sizeof(buf), "SITE %s\r\n", ctxt->hostname);
#else
		sprintf(buf, "SITE %s\r\n", ctxt->hostname);
#endif
                buf[sizeof(buf) - 1] = 0;
                len = strlen(buf);
#ifdef DEBUG_FTP
		err(buf);
#endif
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = xmlNanoFTPGetResponse(ctxt);
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
#ifdef HAVE_SNPRINTF
		    snprintf(buf, sizeof(buf), "USER anonymous@%s\r\n",
			           ctxt->hostname);
#else
		    sprintf(buf, "USER anonymous@%s\r\n", ctxt->hostname);
#endif
		else
#ifdef HAVE_SNPRINTF
		    snprintf(buf, sizeof(buf), "USER %s@%s\r\n",
			           ctxt->user, ctxt->hostname);
#else
		    sprintf(buf, "USER %s@%s\r\n",
			           ctxt->user, ctxt->hostname);
#endif
                buf[sizeof(buf) - 1] = 0;
                len = strlen(buf);
#ifdef DEBUG_FTP
		err(buf);
#endif
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = xmlNanoFTPGetResponse(ctxt);
		if ((res == 1) || (res == 2)) {
		    /* we assume it worked :-\ */
		    proxyType = 2;
		    return(0);
		}
		if (ctxt->passwd == NULL)
#ifdef HAVE_SNPRINTF
		    snprintf(buf, sizeof(buf), "PASS libxml@%s\r\n", hostname);
#else
		    sprintf(buf, "PASS libxml@%s\r\n", hostname);
#endif
		else
#ifdef HAVE_SNPRINTF
		    snprintf(buf, sizeof(buf), "PASS %s\r\n", ctxt->passwd);
#else
		    sprintf(buf, "PASS %s\r\n", ctxt->passwd);
#endif
                buf[sizeof(buf) - 1] = 0;
                len = strlen(buf);
#ifdef DEBUG_FTP
		err(buf);
#endif
		res = send(ctxt->controlFd, buf, len, 0);
		if (res < 0) {
		    closesocket(ctxt->controlFd); ctxt->controlFd = -1;
		    ctxt->controlFd = -1;
		    return(res);
		}
		res = xmlNanoFTPGetResponse(ctxt);
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
    res = xmlNanoFTPSendUser(ctxt);
    if (res < 0) {
        closesocket(ctxt->controlFd); ctxt->controlFd = -1;
        ctxt->controlFd = -1;
	return(-1);
    }
    res = xmlNanoFTPGetResponse(ctxt);
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
    res = xmlNanoFTPSendPasswd(ctxt);
    if (res < 0) {
        closesocket(ctxt->controlFd); ctxt->controlFd = -1;
        ctxt->controlFd = -1;
	return(-1);
    }
    res = xmlNanoFTPGetResponse(ctxt);
    switch (res) {
	case 2:
	    break;
	case 3:
	    err("FTP server asking for ACCNT on anonymous\n");
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
 * xmlNanoFTPGetConnection:
 * @ctx:  an FTP context
 *
 * Try to open a data connection to the server. Currently only
 * passive mode is supported.
 *
 * Returns -1 in case of error, 0 otherwise
 */

static int
xmlNanoFTPGetConnection(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char buf[200], *cur;
    int len, i;
    int res;
    unsigned char ad[6], *adp, *portp;
    unsigned int temp[6];
    struct sockaddr_in dataAddr;
    SOCKLEN_T dataAddrLen;

    ctxt->dataFd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (ctxt->dataFd < 0) {
        err("xmlNanoFTPGetConnection: failed to create socket\n");
	return(-1);
    }
    dataAddrLen = sizeof(dataAddr);
    memset(&dataAddr, 0, dataAddrLen);
    dataAddr.sin_family = AF_INET;

    if (ctxt->passive) {
	sprintf(buf, "PASV\r\n");
        len = strlen(buf);
#ifdef DEBUG_FTP
	err(buf);
#endif
	res = send(ctxt->controlFd, buf, len, 0);
	if (res < 0) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(res);
	}
        res = xmlNanoFTPReadResponse(ctx);
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
	    err("Invalid answer to PASV\n");
	    if (ctxt->dataFd != -1) {
		closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    }
	    return(-1);
	}
	for (i=0; i<6; i++) ad[i] = (unsigned char) (temp[i] & 0xff);
	memcpy(&dataAddr.sin_addr, &ad[0], 4);
	memcpy(&dataAddr.sin_port, &ad[4], 2);
	if (connect(ctxt->dataFd, (struct sockaddr *) &dataAddr, dataAddrLen) < 0) {
	    err("Failed to create a data connection\n");
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
    } else {
        getsockname(ctxt->dataFd, (struct sockaddr *) &dataAddr, &dataAddrLen);
	dataAddr.sin_port = 0;
	if (bind(ctxt->dataFd, (struct sockaddr *) &dataAddr, dataAddrLen) < 0) {
	    err("Failed to bind a port\n");
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
        getsockname(ctxt->dataFd, (struct sockaddr *) &dataAddr, &dataAddrLen);

	if (listen(ctxt->dataFd, 1) < 0) {
	    err("Could not listen on port %d\n",
		ntohs(dataAddr.sin_port));
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return (-1);
	}
	adp = (unsigned char *) &dataAddr.sin_addr;
	portp = (unsigned char *) &dataAddr.sin_port;
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "PORT %d,%d,%d,%d,%d,%d\r\n",
	       adp[0] & 0xff, adp[1] & 0xff, adp[2] & 0xff, adp[3] & 0xff,
	       portp[0] & 0xff, portp[1] & 0xff);
#else
	sprintf(buf, "PORT %d,%d,%d,%d,%d,%d\r\n",
	       adp[0] & 0xff, adp[1] & 0xff, adp[2] & 0xff, adp[3] & 0xff,
	       portp[0] & 0xff, portp[1] & 0xff);
#endif
        buf[sizeof(buf) - 1] = 0;
        len = strlen(buf);
#ifdef DEBUG_FTP
	err(buf);
#endif

	res = send(ctxt->controlFd, buf, len, 0);
	if (res < 0) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(res);
	}
        res = xmlNanoFTPGetResponse(ctxt);
	if (res != 2) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
        }
    }
    return(ctxt->dataFd);

}



/**
 * xmlNanoFTPGetSocket:
 * @ctx:  an FTP context
 * @filename:  the file to retrieve (or NULL if path is in context).
 *
 * Initiate fetch of the given file from the server.
 *
 * Returns the socket for the data connection, or <0 in case of error
 */


static int
xmlNanoFTPGetSocket(void *ctx, const char *filename) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
    char buf[300];
    int res, len;
    if ((filename == NULL) && (ctxt->path == NULL))
	return(-1);
    ctxt->dataFd = xmlNanoFTPGetConnection(ctxt);
    if (ctxt->dataFd == -1)
	return(-1);

    sprintf(buf, "TYPE I\r\n");
    len = strlen(buf);
#ifdef DEBUG_FTP
    err(buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(res);
    }
    res = xmlNanoFTPReadResponse(ctxt);
    if (res != 2) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(-res);
    }
    if (filename == NULL)
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "RETR %s\r\n", ctxt->path);
#else
	sprintf(buf, "RETR %s\r\n", ctxt->path);
#endif
    else
#ifdef HAVE_SNPRINTF
	snprintf(buf, sizeof(buf), "RETR %s\r\n", filename);
#else
	sprintf(buf, "RETR %s\r\n", filename);
#endif
    buf[sizeof(buf) - 1] = 0;
    len = strlen(buf);
#ifdef DEBUG_FTP
    err(buf);
#endif
    res = send(ctxt->controlFd, buf, len, 0);
    if (res < 0) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(res);
    }
    res = xmlNanoFTPReadResponse(ctxt);
    if (res != 1) {
	closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	return(-res);
    }
    return(ctxt->dataFd);
}

/**
 * xmlNanoFTPRead:
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
xmlNanoFTPRead(void *ctx, void *dest, int len) 
{
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;
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
#ifdef Unix
	InputHandler *what;

	if(R_wait_usec > 0) {
	    R_PolledEvents();
	    tv.tv_sec = 0;
	    tv.tv_usec = R_wait_usec;
	} else {   
	    tv.tv_sec = 1;
	    tv.tv_usec = 0;
	}
#elif defined(Win32)
	tv.tv_sec = 0;
	tv.tv_usec = 2e5;
	R_ProcessEvents();
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

	res = select(maxfd + 1, &rfd, NULL, NULL, &tv);

	if (res < 0) { /* socket error */
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
	}
	if (res == 0) { /* timeout, no data available yet */
	    used += tv.tv_sec + 1e-6 * tv.tv_usec;
	    res = xmlNanoFTPCheckResponse(ctxt);
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
	if(!FD_ISSET(ctxt->dataFd, &rfd)) { /* was one of the extras */
	    what = getSelectedHandler(R_InputHandlers, &rfd);
	    if(!what) what->handler((void*) NULL);
	    continue;
	}
#endif
	
	/* was the socket */
	got = recv(ctxt->dataFd, dest, len, 0);
	if (got < 0) {
	    closesocket(ctxt->dataFd); ctxt->dataFd = -1;
	    return(-1);
	} else  break;
	if (used > timeout) return(0);
    }
    return got;
}


/**
 * xmlNanoFTPOpen:
 * @URL: the URL to the resource
 *
 * Start to fetch the given ftp:// resource
 *
 * Returns an FTP context, or NULL
 */

void*
xmlNanoFTPOpen(const char *URL) {
    xmlNanoFTPCtxtPtr ctxt;
    int sock;

    xmlNanoFTPInit();
    if (URL == NULL) return(NULL);
    if (strncmp("ftp://", URL, 6)) return(NULL);

    ctxt = (xmlNanoFTPCtxtPtr) xmlNanoFTPNewCtxt(URL);
    if (ctxt == NULL) return(NULL);
    if (xmlNanoFTPConnect(ctxt) < 0) {
	xmlNanoFTPFreeCtxt(ctxt);
	return(NULL);
    }
    sock = xmlNanoFTPGetSocket(ctxt, ctxt->path);
    if (sock < 0) {
	xmlNanoFTPFreeCtxt(ctxt);
	return(NULL);
    }
    return(ctxt);
}

/**
 * xmlNanoFTPClose:
 * @ctx: an FTP context
 *
 * Close the connection and both control and transport
 *
 * Returns -1 incase of error, 0 otherwise
 */

int
xmlNanoFTPClose(void *ctx) {
    xmlNanoFTPCtxtPtr ctxt = (xmlNanoFTPCtxtPtr) ctx;

    if (ctxt == NULL)
	return(-1);

    if (ctxt->dataFd >= 0) {
	closesocket(ctxt->dataFd);
	ctxt->dataFd = -1;
    }
    if (ctxt->controlFd >= 0) {
	xmlNanoFTPQuit(ctxt);
	closesocket(ctxt->controlFd);
	ctxt->controlFd = -1;
    }
    xmlNanoFTPFreeCtxt(ctxt);
    return(0);
}

/**
 * xmlNanoFTPTimeout:
 * @delay:  the delay in seconds
 *
 * Set the FTP timeout, (default is 60secs).  0 means immediate
 * return, while -1 infinite.
 */

void
xmlNanoFTPTimeout(int delay) {
    timeout = (unsigned int) delay;
}
