/*
 * nanohttp.c: minimalist HTTP GET implementation to fetch external subsets.
 *             focuses on size, streamability, reentrancy and portability
 *
 * This is clearly not a general purpose HTTP implementation
 * If you look for one, check:
 *         http://www.w3.org/Library/
 *
 * See Copyright for the status of this software.
 *
 * Daniel.Veillard@w3.org
 */
 
/* TODO add compression support, Send the Accept- , and decompress on the
        fly with ZLIB if found at compile-time */

#ifdef Win32
#define INCLUDE_WINSOCK
#include "win32config.h"
#else
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#endif

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
#ifdef SUPPORT_IP6
#include <resolv.h>
#endif


#define BAD_CAST (unsigned char *)

#define xmlFree free
#define xmlMalloc malloc
#define xmlRealloc realloc
#define xmlMemStrdup strdup

static void xmlNanoHTTPScanProxy(const char *URL);
static void *xmlNanoHTTPMethod(const char *URL, const char *method,
			       const char *input, char **contentType,
			       const char *headers);

#define err Rf_warning

/**
 * A couple of portability macros
 */
#ifndef _WINSOCKAPI_
#define closesocket(s) close(s)
#define SOCKET int
#endif

#define xmlStrncasecmp(a, b, n) strncasecmp((char *)a, (char *)b, n)


#define XML_NANO_HTTP_MAX_REDIR	10

#define XML_NANO_HTTP_CHUNK	4096

#define XML_NANO_HTTP_CLOSED	0
#define XML_NANO_HTTP_WRITE	1
#define XML_NANO_HTTP_READ	2
#define XML_NANO_HTTP_NONE	4

typedef struct xmlNanoHTTPCtxt {
    char *protocol;	/* the protocol name */
    char *hostname;	/* the host name */
    int port;		/* the port */
    char *path;		/* the path within the URL */
    SOCKET fd;		/* the file descriptor for the socket */
    int state;		/* WRITE / READ / CLOSED */
    char *out;		/* buffer sent (zero terminated) */
    char *outptr;	/* index within the buffer sent */
    char *in;		/* the receiving buffer */
    char *content;	/* the start of the content */
    char *inptr;	/* the next byte to read from network */
    char *inrptr;	/* the next byte to give back to the client */
    int inlen;		/* len of the input buffer */
    int last;		/* return code for last operation */
    int returnValue;	/* the protocol return value */
    char *contentType;	/* the MIME type for the input */
    char *location;	/* the new URL in case of redirect */
    char *authHeader;	/* contents of {WWW,Proxy}-Authenticate header */
} xmlNanoHTTPCtxt, *xmlNanoHTTPCtxtPtr;

static int initialized = 0;
static char *proxy = NULL;	 /* the proxy name if any */
static int proxyPort;	/* the proxy port if any */
static unsigned int timeout = 60;/* the select() timeout in seconds */

/**
 * A portability function
 */
int socket_errno(void) {
#ifdef _WINSOCKAPI_
    return(WSAGetLastError());
#else
    return(errno);
#endif
}

/**
 * xmlNanoHTTPInit:
 *
 * Initialize the HTTP protocol layer.
 * Currently it just checks for proxy informations
 */

void
xmlNanoHTTPInit(void) {
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

    if (proxy == NULL) {
	proxyPort = 80;
	env = getenv("no_proxy");
	if (env != NULL)
	    goto done;
	env = getenv("http_proxy");
	if (env != NULL) {
	    xmlNanoHTTPScanProxy(env);
	    goto done;
	}
	env = getenv("HTTP_PROXY");
	if (env != NULL) {
	    xmlNanoHTTPScanProxy(env);
	    goto done;
	}
    }
done:
    initialized = 1;
}

/**
 * xmlNanoHTTPClenup:
 *
 * Cleanup the HTTP protocol layer.
 */

void
xmlNanoHTTPCleanup(void) {
    if (proxy != NULL)
	xmlFree(proxy);
#ifdef _WINSOCKAPI_
    if (initialized)
	WSACleanup();
#endif
    initialized = 0;
    return;
}

/**
 * xmlNanoHTTPTimeout:
 * @delay:  the delay in seconds
 *
 * Set the HTTP timeout, (default is 60secs).  0 means immediate
 * return, while -1 infinite.
 */

void
xmlNanoHTTPTimeout(int delay) {
    timeout = (unsigned int) delay;
}

/**
 * xmlNanoHTTPScanURL:
 * @ctxt:  an HTTP context
 * @URL:  The URL used to initialize the context
 *
 * (Re)Initialize an HTTP context by parsing the URL and finding
 * the protocol host port and path it indicates.
 */

static void
xmlNanoHTTPScanURL(xmlNanoHTTPCtxtPtr ctxt, const char *URL) {
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
 * xmlNanoHTTPScanProxy:
 * @URL:  The proxy URL used to initialize the proxy context
 *
 * (Re)Initialize the HTTP Proxy context by parsing the URL and finding
 * the protocol host port it indicates.
 * Should be like http://myproxy/ or http://myproxy:3128/
 * A NULL URL cleans up proxy informations.
 */

void
xmlNanoHTTPScanProxy(const char *URL) {
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
#ifdef DEBUG_HTTP
    if (URL == NULL)
	err(
		"Removing HTTP proxy info\n");
    else
	err(
		"Using HTTP proxy %s\n", URL);
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
 * xmlNanoHTTPNewCtxt:
 * @URL:  The URL used to initialize the context
 *
 * Allocate and initialize a new HTTP context.
 *
 * Returns an HTTP context or NULL in case of error.
 */

static xmlNanoHTTPCtxtPtr
xmlNanoHTTPNewCtxt(const char *URL) {
    xmlNanoHTTPCtxtPtr ret;

    ret = (xmlNanoHTTPCtxtPtr) xmlMalloc(sizeof(xmlNanoHTTPCtxt));
    if (ret == NULL) return(NULL);

    memset(ret, 0, sizeof(xmlNanoHTTPCtxt));
    ret->port = 80;
    ret->returnValue = 0;
    ret->fd = -1;

    xmlNanoHTTPScanURL(ret, URL);

    return(ret);
}

/**
 * xmlNanoHTTPFreeCtxt:
 * @ctxt:  an HTTP context
 *
 * Frees the context after closing the connection.
 */

static void
xmlNanoHTTPFreeCtxt(xmlNanoHTTPCtxtPtr ctxt) {
    if (ctxt == NULL) return;
    if (ctxt->hostname != NULL) xmlFree(ctxt->hostname);
    if (ctxt->protocol != NULL) xmlFree(ctxt->protocol);
    if (ctxt->path != NULL) xmlFree(ctxt->path);
    if (ctxt->out != NULL) xmlFree(ctxt->out);
    if (ctxt->in != NULL) xmlFree(ctxt->in);
    if (ctxt->contentType != NULL) xmlFree(ctxt->contentType);
    if (ctxt->location != NULL) xmlFree(ctxt->location);
    if (ctxt->authHeader != NULL) xmlFree(ctxt->authHeader);
    ctxt->state = XML_NANO_HTTP_NONE;
    if (ctxt->fd >= 0) closesocket(ctxt->fd);
    ctxt->fd = -1;
    xmlFree(ctxt);
}

/**
 * xmlNanoHTTPSend:
 * @ctxt:  an HTTP context
 *
 * Send the input needed to initiate the processing on the server side
 */

static void
xmlNanoHTTPSend(xmlNanoHTTPCtxtPtr ctxt) {
    if (ctxt->state & XML_NANO_HTTP_WRITE) {
        int total_sent = 0;
        while (total_sent <strlen(ctxt->outptr)) {
            int nsent = send(ctxt->fd, ctxt->outptr+total_sent,
                             strlen(ctxt->outptr)-total_sent, 0);
            if (nsent>0)
                total_sent += nsent;
}

        ctxt->last = total_sent;
    }
}

/**
 * xmlNanoHTTPRecv:
 * @ctxt:  an HTTP context
 *
 * Read information coming from the HTTP connection.
 * This is a blocking call (but it blocks in select(), not read()).
 *
 * Returns the number of byte read or -1 in case of error.
 */

static int
xmlNanoHTTPRecv(xmlNanoHTTPCtxtPtr ctxt) {
    fd_set rfd;
    struct timeval tv;


    while (ctxt->state & XML_NANO_HTTP_READ) {
	if (ctxt->in == NULL) {
	    ctxt->in = (char *) xmlMalloc(65000 * sizeof(char));
	    if (ctxt->in == NULL) {
	        ctxt->last = -1;
		return(-1);
	    }
	    ctxt->inlen = 65000;
	    ctxt->inptr = ctxt->content = ctxt->inrptr = ctxt->in;
	}
	if (ctxt->inrptr > ctxt->in + XML_NANO_HTTP_CHUNK) {
	    int delta = ctxt->inrptr - ctxt->in;
	    int len = ctxt->inptr - ctxt->inrptr;
	    
	    memmove(ctxt->in, ctxt->inrptr, len);
	    ctxt->inrptr -= delta;
	    ctxt->content -= delta;
	    ctxt->inptr -= delta;
	}
        if ((ctxt->in + ctxt->inlen) < (ctxt->inptr + XML_NANO_HTTP_CHUNK)) {
	    int d_inptr = ctxt->inptr - ctxt->in;
	    int d_content = ctxt->content - ctxt->in;
	    int d_inrptr = ctxt->inrptr - ctxt->in;

	    ctxt->inlen *= 2;
            ctxt->in = (char *) xmlRealloc(ctxt->in, ctxt->inlen);
	    if (ctxt->in == NULL) {
	        ctxt->last = -1;
		return(-1);
	    }
            ctxt->inptr = ctxt->in + d_inptr;
            ctxt->content = ctxt->in + d_content;
            ctxt->inrptr = ctxt->in + d_inrptr;
	}
	ctxt->last = recv(ctxt->fd, ctxt->inptr, XML_NANO_HTTP_CHUNK, 0);
	if (ctxt->last > 0) {
	    ctxt->inptr += ctxt->last;
	    return(ctxt->last);
	}
	if (ctxt->last == 0) {
	    return(0);
	}
	if (ctxt->last == -1) {
	    switch (socket_errno()) {
		case EINPROGRESS:
		case EWOULDBLOCK:
#if defined(EAGAIN) && EAGAIN != EWOULDBLOCK
		case EAGAIN:
#endif
		    break;
		default:
		    return(0);
	    }
	}

	tv.tv_sec = timeout;
	tv.tv_usec = 0;
	FD_ZERO(&rfd);
	FD_SET(ctxt->fd, &rfd);
	
	if (select(ctxt->fd+1, &rfd, NULL, NULL, &tv)<1)
		return(0);
    }
    return(0);
}

/**
 * xmlNanoHTTPReadLine:
 * @ctxt:  an HTTP context
 *
 * Read one line in the HTTP server output, usually for extracting
 * the HTTP protocol informations from the answer header.
 *
 * Returns a newly allocated string with a copy of the line, or NULL
 *         which indicate the end of the input.
 */

static char *
xmlNanoHTTPReadLine(xmlNanoHTTPCtxtPtr ctxt) {
    char buf[4096];
    char *bp = buf;
    
    while (bp - buf < 4095) {
	if (ctxt->inrptr == ctxt->inptr) {
	    if (xmlNanoHTTPRecv(ctxt) == 0) {
		if (bp == buf)
		    return(NULL);
		else
		    *bp = 0;
		return(xmlMemStrdup(buf));
	    }
	}
	*bp = *ctxt->inrptr++;
	if (*bp == '\n') {
	    *bp = 0;
	    return(xmlMemStrdup(buf));
	}
	if (*bp != '\r')
	    bp++;
    }
    buf[4095] = 0;
    return(xmlMemStrdup(buf));
}


/**
 * xmlNanoHTTPScanAnswer:
 * @ctxt:  an HTTP context
 * @line:  an HTTP header line
 *
 * Try to extract useful informations from the server answer.
 * We currently parse and process:
 *  - The HTTP revision/ return code
 *  - The Content-Type
 *  - The Location for redirrect processing.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static void
xmlNanoHTTPScanAnswer(xmlNanoHTTPCtxtPtr ctxt, const char *line) {
    const char *cur = line;

    if (line == NULL) return;

    if (!strncmp(line, "HTTP/", 5)) {
        int version = 0;
	int ret = 0;

	cur += 5;
	while ((*cur >= '0') && (*cur <= '9')) {
	    version *= 10;
	    version += *cur - '0';
	    cur++;
	}
	if (*cur == '.') {
	    cur++;
	    if ((*cur >= '0') && (*cur <= '9')) {
		version *= 10;
		version += *cur - '0';
		cur++;
	    }
	    while ((*cur >= '0') && (*cur <= '9'))
		cur++;
	} else
	    version *= 10;
	if ((*cur != ' ') && (*cur != '\t')) return;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if ((*cur < '0') || (*cur > '9')) return;
	while ((*cur >= '0') && (*cur <= '9')) {
	    ret *= 10;
	    ret += *cur - '0';
	    cur++;
	}
	if ((*cur != 0) && (*cur != ' ') && (*cur != '\t')) return;
	ctxt->returnValue = ret;
    } else if (!xmlStrncasecmp(BAD_CAST line, BAD_CAST"Content-Type:", 13)) {
        cur += 13;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if (ctxt->contentType != NULL)
	    xmlFree(ctxt->contentType);
	ctxt->contentType = xmlMemStrdup(cur);
    } else if (!xmlStrncasecmp(BAD_CAST line, BAD_CAST"ContentType:", 12)) {
        cur += 12;
	if (ctxt->contentType != NULL) return;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	ctxt->contentType = xmlMemStrdup(cur);
    } else if (!xmlStrncasecmp(BAD_CAST line, BAD_CAST"Location:", 9)) {
        cur += 9;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if (ctxt->location != NULL)
	    xmlFree(ctxt->location);
	ctxt->location = xmlMemStrdup(cur);
    } else if (!xmlStrncasecmp(BAD_CAST line, BAD_CAST"WWW-Authenticate:", 17)) {
        cur += 17;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if (ctxt->authHeader != NULL)
	    xmlFree(ctxt->authHeader);
	ctxt->authHeader = xmlMemStrdup(cur);
    } else if (!xmlStrncasecmp(BAD_CAST line, BAD_CAST"Proxy-Authenticate:", 19)) {
        cur += 19;
	while ((*cur == ' ') || (*cur == '\t')) cur++;
	if (ctxt->authHeader != NULL)
	    xmlFree(ctxt->authHeader);
	ctxt->authHeader = xmlMemStrdup(cur);
    }
}

/**
 * xmlNanoHTTPConnectAttempt:
 * @ia:  an internet adress structure
 * @port:  the port number
 *
 * Attempt a connection to the given IP:port endpoint. It forces
 * non-blocking semantic on the socket, and allow 60 seconds for
 * the host to answer.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static int
xmlNanoHTTPConnectAttempt(struct sockaddr *addr, int port)
{
    SOCKET s = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    fd_set wfd;
    struct timeval tv;
    int status;
    
    if (s==-1) {
#ifdef DEBUG_HTTP
	perror("socket");
#endif
	return(-1);
    }
    
#ifdef _WINSOCKAPI_
    {
	u_long one = 1;

	status = ioctlsocket(s, FIONBIO, &one) == SOCKET_ERROR ? -1 : 0;
    }
#else /* _WINSOCKAPI_ */
#if defined(VMS)
    {
	int enable = 1;
	status = ioctl(s, FIONBIO, &enable);
    }
#else /* VMS */
    if ((status = fcntl(s, F_GETFL, 0)) != -1) {
#ifdef O_NONBLOCK
	status |= O_NONBLOCK;
#else /* O_NONBLOCK */
#ifdef F_NDELAY
	status |= F_NDELAY;
#endif /* F_NDELAY */
#endif /* !O_NONBLOCK */
	status = fcntl(s, F_SETFL, status);
    }
    if (status < 0) {
#ifdef DEBUG_HTTP
	perror("nonblocking");
#endif
	closesocket(s);
	return(-1);
    }
#endif /* !VMS */
#endif /* !_WINSOCKAPI_ */


    if ((connect(s, addr, sizeof(*addr))==-1)) {
	switch (socket_errno()) {
	    case EINPROGRESS:
	    case EWOULDBLOCK:
		break;
	    default:
		perror("connect");
		closesocket(s);
		return(-1);
	}
    }	
    
    tv.tv_sec = timeout;
    tv.tv_usec = 0;
    
    FD_ZERO(&wfd);
    FD_SET(s, &wfd);
    
    switch(select(s+1, NULL, &wfd, NULL, &tv))
    {
	case 0:
	    /* Time out */
	    closesocket(s);
	    return(-1);
	case -1:
	    /* Ermm.. ?? */
#ifdef DEBUG_HTTP
	    perror("select");
#endif
	    closesocket(s);
	    return(-1);
    }

    if ( FD_ISSET(s, &wfd) ) {
	SOCKLEN_T len;
	len = sizeof(status);
	if (getsockopt(s, SOL_SOCKET, SO_ERROR, (char*)&status, &len) < 0 ) {
	    /* Solaris error code */
	    return (-1);
	}
	if ( status ) {
	    closesocket(s);
	    errno = status;
	    return (-1);
	}
    } else {
	/* pbm */
	return (-1);
    }
    
    return(s);
}
 
/**
 * xmlNanoHTTPConnectHost:
 * @host:  the host name
 * @port:  the port number
 *
 * Attempt a connection to the given host:port endpoint. It tries
 * the multiple IP provided by the DNS if available.
 *
 * Returns -1 in case of failure, the file descriptor number otherwise
 */

static int
xmlNanoHTTPConnectHost(const char *host, int port)
{
    struct hostent *h;
    struct sockaddr *addr;
    struct in_addr ia;
    struct sockaddr_in sin;
#ifdef SUPPORT_IP6
    struct in6_addr ia6;
    struct sockaddr_in6 sin6;
#endif
    int i;
    int s;
    
#if defined(SUPPORT_IP6) && defined(RES_USE_INET6)
    if (!(_res.options & RES_INIT))
	res_init();
    _res.options |= RES_USE_INET6;
#endif
    h=gethostbyname(host);
    if (h==NULL)
    {
#ifdef DEBUG_HTTP
	err("unable to resolve '%s'.\n", host);
#endif
	return(-1);
    }
    
    for(i=0; h->h_addr_list[i]; i++)
    {
	if (h->h_addrtype == AF_INET) {
	    /* A records (IPv4) */
	    memcpy(&ia, h->h_addr_list[i], h->h_length);
	    sin.sin_family = h->h_addrtype;
	    sin.sin_addr   = ia;
	    sin.sin_port   = htons(port);
	    addr = (struct sockaddr *)&sin;
#ifdef SUPPORT_IP6
	} else if (h->h_addrtype == AF_INET6) {
	    /* AAAA records (IPv6) */
	    memcpy(&ia6, h->h_addr_list[i], h->h_length);
	    sin6.sin_family = h->h_addrtype;
	    sin6.sin_addr   = ia6;
	    sin6.sin_port   = htons(port);
	    addr = (struct sockaddr *)&sin6;
#endif
	} else
	    break; /* for */
	
	s = xmlNanoHTTPConnectAttempt(addr, port);
	if (s != -1)
	    return(s);
    }

#ifdef DEBUG_HTTP
    err(
	    "unable to connect to '%s'.\n", host);
#endif
    return(-1);
}


/**
 * xmlNanoHTTPOpen:
 * @URL:  The URL to load
 * @contentType:  if available the Content-Type information will be
 *                returned at that location
 *
 * This function try to open a connection to the indicated resource
 * via HTTP GET.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */

void*
xmlNanoHTTPOpen(const char *URL, char **contentType) {
    if (contentType != NULL) *contentType = NULL;
    return xmlNanoHTTPMethod(URL, NULL, NULL, contentType, NULL);
}

/**
 * xmlNanoHTTPRead:
 * @ctx:  the HTTP context
 * @dest:  a buffer
 * @len:  the buffer length
 *
 * This function tries to read @len bytes from the existing HTTP connection
 * and saves them in @dest. This is a blocking call.
 *
 * Returns the number of byte read. 0 is an indication of an end of connection.
 *         -1 indicates a parameter error.
 */
int
xmlNanoHTTPRead(void *ctx, void *dest, int len) {
    xmlNanoHTTPCtxtPtr ctxt = (xmlNanoHTTPCtxtPtr) ctx;

    if (ctx == NULL) return(-1);
    if (dest == NULL) return(-1);
    if (len <= 0) return(0);

    while (ctxt->inptr - ctxt->inrptr < len) {
        if (xmlNanoHTTPRecv(ctxt) == 0) break;
    }
    if (ctxt->inptr - ctxt->inrptr < len)
        len = ctxt->inptr - ctxt->inrptr;
    memcpy(dest, ctxt->inrptr, len);
    ctxt->inrptr += len;
    return(len);
}

/**
 * xmlNanoHTTPClose:
 * @ctx:  the HTTP context
 *
 * This function closes an HTTP context, it ends up the connection and
 * free all data related to it.
 */
void
xmlNanoHTTPClose(void *ctx) {
    xmlNanoHTTPCtxtPtr ctxt = (xmlNanoHTTPCtxtPtr) ctx;

    if (ctx == NULL) return;

    xmlNanoHTTPFreeCtxt(ctxt);
}

/**
 * xmlNanoHTTPMethod:
 * @URL:  The URL to load
 * @method:  the HTTP method to use
 * @input:  the input string if any
 * @contentType:  the Content-Type information IN and OUT
 * @headers:  the extra headers
 *
 * This function try to open a connection to the indicated resource
 * via HTTP using the given @method, adding the given extra headers
 * and the input buffer for the request content.
 *
 * Returns NULL in case of failure, otherwise a request handler.
 *     The contentType, if provided must be freed by the caller
 */

void*
xmlNanoHTTPMethod(const char *URL, const char *method, const char *input,
                  char **contentType, const char *headers) {
    xmlNanoHTTPCtxtPtr ctxt;
    char *bp, *p;
    int blen, ilen, ret;
    int head;
    int nbRedirects = 0;
    char *redirURL = NULL;
    
    if (URL == NULL) return(NULL);
    if (method == NULL) method = "GET";
    xmlNanoHTTPInit();

retry:
    if (redirURL == NULL)
	ctxt = xmlNanoHTTPNewCtxt(URL);
    else {
	ctxt = xmlNanoHTTPNewCtxt(redirURL);
	xmlFree(redirURL);
	redirURL = NULL;
    }

    if ((ctxt->protocol == NULL) || (strcmp(ctxt->protocol, "http"))) {
        xmlNanoHTTPFreeCtxt(ctxt);
	if (redirURL != NULL) xmlFree(redirURL);
        return(NULL);
    }
    if (ctxt->hostname == NULL) {
        xmlNanoHTTPFreeCtxt(ctxt);
        return(NULL);
    }
    if (proxy) {
	blen = strlen(ctxt->hostname) * 2 + 16;
	ret = xmlNanoHTTPConnectHost(proxy, proxyPort);
    }
    else {
	blen = strlen(ctxt->hostname);
	ret = xmlNanoHTTPConnectHost(ctxt->hostname, ctxt->port);
    }
    if (ret < 0) {
        xmlNanoHTTPFreeCtxt(ctxt);
        return(NULL);
    }
    ctxt->fd = ret;

    if (input != NULL) {
	ilen = strlen(input);
	blen += ilen + 32;
    }
    else
	ilen = 0;
    if (headers != NULL)
	blen += strlen(headers);
    if (contentType && *contentType)
	blen += strlen(*contentType) + 16;
    blen += strlen(method) + strlen(ctxt->path) + 23;
    bp = xmlMalloc(blen);
    if (proxy) {
	if (ctxt->port != 80) {
	    sprintf(bp, "%s http://%s:%d%s", method, ctxt->hostname,
		 ctxt->port, ctxt->path);
	}
	else
	    sprintf(bp, "%s http://%s%s", method, ctxt->hostname, ctxt->path);
    }
    else
	sprintf(bp, "%s %s", method, ctxt->path);
    p = bp + strlen(bp);
    sprintf(p, " HTTP/1.0\r\nHost: %s\r\n", ctxt->hostname);
    p += strlen(p);
    if (contentType != NULL && *contentType) {
	sprintf(p, "Content-Type: %s\r\n", *contentType);
	p += strlen(p);
    }
    if (headers != NULL) {
	strcpy(p, headers);
	p += strlen(p);
    }
    if (input != NULL)
	sprintf(p, "Content-Length: %d\r\n\r\n%s", ilen, input);
    else
	strcpy(p, "\r\n");
#ifdef DEBUG_HTTP
    err(
	    "-> %s%s", proxy? "(Proxy) " : "", bp);
    if ((blen -= strlen(bp)+1) < 0)
	err(
		"ERROR: overflowed buffer by %d bytes\n", -blen);
#endif
    ctxt->outptr = ctxt->out = bp;
    ctxt->state = XML_NANO_HTTP_WRITE;
    xmlNanoHTTPSend(ctxt);
    ctxt->state = XML_NANO_HTTP_READ;
    head = 1;

    while ((p = xmlNanoHTTPReadLine(ctxt)) != NULL) {
        if (head && (*p == 0)) {
	    head = 0;
	    ctxt->content = ctxt->inrptr;
	    xmlFree(p);
	    break;
	}
	xmlNanoHTTPScanAnswer(ctxt, p);

#ifdef DEBUG_HTTP
	err("<- %s\n", p);
#endif
        xmlFree(p);
    }

    if ((ctxt->location != NULL) && (ctxt->returnValue >= 300) &&
        (ctxt->returnValue < 400)) {
#ifdef DEBUG_HTTP
	err(
		"\nRedirect to: %s\n", ctxt->location);
#endif
	while (xmlNanoHTTPRecv(ctxt)) ;
        if (nbRedirects < XML_NANO_HTTP_MAX_REDIR) {
	    nbRedirects++;
	    redirURL = xmlMemStrdup(ctxt->location);
	    xmlNanoHTTPFreeCtxt(ctxt);
	    goto retry;
	}
	xmlNanoHTTPFreeCtxt(ctxt);
#ifdef DEBUG_HTTP
	err(
		"Too many redirects, aborting ...\n");
#endif
	return(NULL);

    }

    if (contentType != NULL) {
	if (ctxt->contentType != NULL)
	    *contentType = xmlMemStrdup(ctxt->contentType);
	else
	    *contentType = NULL;
    }

#ifdef DEBUG_HTTP
    if (ctxt->contentType != NULL)
	err(
		"\nCode %d, content-type '%s'\n\n",
	       ctxt->returnValue, ctxt->contentType);
    else
	err(
		"\nCode %d, no content-type\n\n",
	       ctxt->returnValue);
#endif

    return((void *) ctxt);
}



/**
 * xmlNanoHTTPReturnCode:
 * @ctx:  the HTTP context
 *
 * Returns the HTTP return code for the request.
 */
int
xmlNanoHTTPReturnCode(void *ctx) {
    xmlNanoHTTPCtxtPtr ctxt = (xmlNanoHTTPCtxtPtr) ctx;

    if (ctxt == NULL) return(-1);

    return(ctxt->returnValue);
}

