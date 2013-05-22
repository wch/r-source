/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2009-12 The R Core Team.
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

/* This is a small HTTP server that serves requests by evaluating
 * the httpd() function and passing the result to the browser. */

/* Example:
   httpd <- function(path,query=NULL,...) {
      cat("Request for:", path,"\n"); print(query);
      list(paste("Hello, <b>world</b>!<p>You asked for \"",path,"\".",sep=''))
   }
   .Internal(startHTTPD("127.0.0.1",8080))
 */

/* size of the line buffer for each worker (request and header only)
 * requests that have longer headers will be rejected with 413 */
#define LINE_BUF_SIZE 1024

/* maximum number of active workers (parallel connections)
 * when exceeded the server closes new connections */
#define MAX_WORKERS 32


/* --- Rhttpd implementation --- */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <Rmodules/Rinternet.h>

#define HttpdServerActivity 8
#define HttpdWorkerActivity 9

/* this is orignally from sisock.h - system independent sockets */

#ifndef WIN32
# include <R_ext/eventloop.h>
# include <sys/types.h>
# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif
# ifdef HAVE_BSD_NETWORKING
#  include <netdb.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
# endif
# include <errno.h>

# define sockerrno errno
# define SOCKET int
# define INVALID_SOCKET (-1)
# define closesocket(A) close(A)
# define initsocks()
# define donesocks()
#else
/* --- Windows-only --- */
# include <windows.h>
# include <winsock.h>
# include <string.h>

# define sockerrno WSAGetLastError()

# define ECONNREFUSED WSAECONNREFUSED
# define EADDRINUSE WSAEADDRINUSE
# define ENOTSOCK WSAENOTSOCK
# define EISCONN WSAEISCONN
# define ETIMEDOUT WSAETIMEDOUT
# define ENETUNREACH WSAENETUNREACH
# define EINPROGRESS WSAEINPROGRESS
# define EALREADY WSAEALREADY
# define EAFNOSUPPORT WSAEAFNOSUPPORT
# define EOPNOTSUPP WSAEOPNOTSUPP
# define EWOULDBLOCK WSAEWOULDBLOCK
/* those are occasionally defined by MinGW's errno, so override them
 * with socket equivalents */
# ifdef EBADF
#  undef EBADF
# endif
# ifdef EINVAL
#  undef EINVAL
# endif
# ifdef EFAULT
#  undef EFAULT
# endif
# ifdef EACCES
#  undef EACCES
# endif
# define EFAULT WSAEFAULT
# define EINVAL WSAEINVAL
# define EACCES WSAEACCES
# define EBADF WSAEBADF

static int initsocks(void)
{
    WSADATA dt;
    /* initialize WinSock 1.1 */
    return (WSAStartup(0x0101, &dt)) ? -1 : 0;
}

# define donesocks() WSACleanup()
typedef int socklen_t;

#endif /* WIN32 */

/* --- system-independent part --- */

#define SA struct sockaddr
#define SAIN struct sockaddr_in

static struct sockaddr *build_sin(struct sockaddr_in *sa, const char *ip, int port) {
    memset(sa, 0, sizeof(struct sockaddr_in));
    sa->sin_family = AF_INET;
    sa->sin_port = htons(port);
    sa->sin_addr.s_addr = (ip) ? inet_addr(ip) : htonl(INADDR_ANY);
    return (struct sockaddr*)sa;
}

/* --- END of sisock.h --- */

/* debug output - change the DBG(X) X to enable debugging output */
#define DBG(X)

/* --- httpd --- */

#define PART_REQUEST 0
#define PART_HEADER  1
#define PART_BODY    2

#define METHOD_POST  1
#define METHOD_GET   2
#define METHOD_HEAD  3
#define METHOD_OTHER 8 /* for custom requests only */

/* attributes of a connection/worker */
#define CONNECTION_CLOSE  0x01 /* Connection: close response behavior is requested */
#define HOST_HEADER       0x02 /* headers contained Host: header (required for HTTP/1.1) */
#define HTTP_1_0          0x04 /* the client requested HTTP/1.0 */
#define CONTENT_LENGTH    0x08 /* Content-length: was specified in the headers */
#define THREAD_OWNED      0x10 /* the worker is owned by a thread and cannot removed */
#define THREAD_DISPOSE    0x20 /* the thread should dispose of the worker */
#define CONTENT_TYPE      0x40 /* message has a specific content type set */
#define CONTENT_FORM_UENC 0x80 /* message content type is application/x-www-form-urlencoded */

struct buffer {
    struct buffer *next, *prev;
    size_t size, length;
    char data[1];
};

/* we have to protect re-entrance and not continue processing if there is
   a worker inside R already. If we did not then another client connection
   would trigger handler and pile up eval on top of the stack, leading to
   exhaustion very quickly and a big mess */
static int in_process;

/* --- connection/worker structure holding all data for an active connection --- */
typedef struct httpd_conn {
    SOCKET sock;         /* client socket */
    struct in_addr peer; /* IP address of the peer */
#ifdef WIN32
    HANDLE thread;       /* worker thread */
#else
    InputHandler *ih;    /* worker input handler */
#endif
    char line_buf[LINE_BUF_SIZE];  /* line buffer (used for request and headers) */
    char *url, *body;              /* URL and request body */
    char *content_type;            /* content type (if set) */
    size_t line_pos, body_pos; /* positions in the buffers */
    long content_length;           /* desired content length */
    char part, method, attr;       /* request part, method and connection attributes */
    struct buffer *headers;        /* buffer holding header lines */
} httpd_conn_t;

#define IS_HTTP_1_1(C) (((C)->attr & HTTP_1_0) == 0)

/* returns the HTTP/x.x string for a given connection - we support 1.0 and 1.1 only */
#define HTTP_SIG(C) (IS_HTTP_1_1(C) ? "HTTP/1.1" : "HTTP/1.0")

/* --- static list of currently active workers --- */
static httpd_conn_t *workers[MAX_WORKERS];

/* --- flag determining whether one-time initialization is yet to be performed --- */
static int needs_init = 1;

#ifdef WIN32
#define WM_RHTTP_CALLBACK ( WM_USER + 1 )
static HWND message_window;
static LRESULT CALLBACK
RhttpdWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#ifndef HWND_MESSAGE
#define HWND_MESSAGE ((HWND)-3) /* NOTE: this is supported by W2k/XP and up only! */
#endif
#endif

static void first_init()
{
    initsocks();
#ifdef WIN32
    /* create a dummy message-only window for synchronization with the
     * main event loop */
    HINSTANCE instance = GetModuleHandle(NULL);
    LPCTSTR class = "Rhttpd";
    WNDCLASS wndclass = { 0, RhttpdWindowProc, 0, 0, instance, NULL, 0, 0,
			  NULL, class };
    RegisterClass(&wndclass);
    message_window = CreateWindow(class, "Rhttpd", 0, 1, 1, 1, 1,
				  HWND_MESSAGE, NULL, instance, NULL);
#endif
    needs_init = 0;
}

/* free buffers starting from the tail(!!) */
static void free_buffer(struct buffer *buf) {
    if (!buf) return;
    if (buf->prev) free_buffer(buf->prev);
    free(buf);
}

/* allocate a new buffer */
static struct buffer *alloc_buffer(int size, struct buffer *parent) {
    struct buffer *buf = (struct buffer*) malloc(sizeof(struct buffer) + size);
    if (!buf) return buf;
    buf->next = 0;
    buf->prev = parent;
    if (parent) parent->next = buf;
    buf->size = size;
    buf->length = 0;
    return buf;
}

/* convert doubly-linked buffers into one big raw vector */
static SEXP collect_buffers(struct buffer *buf) {
    SEXP res;
    char *dst;
    int len = 0;
    if (!buf) return allocVector(RAWSXP, 0);
    while (buf->prev) { /* count the total length and find the root */
	len += buf->length;
	buf = buf->prev;
    }
    res = allocVector(RAWSXP, len + buf->length);
    dst = (char*) RAW(res);
    while (buf) {
	memcpy(dst, buf->data, buf->length);
	dst += buf->length;
	buf = buf->next;
    }
    return res;
}

static void finalize_worker(httpd_conn_t *c)
{
    DBG(printf("finalizing worker %p\n", (void*) c));
#ifndef WIN32
    if (c->ih) {
	removeInputHandler(&R_InputHandlers, c->ih);
	c->ih = NULL;
    }
#endif
    if (c->url) {
	free(c->url);
	c->url = NULL;
    }

    if (c->body) {
	free(c->body);
	c->body = NULL;
    }

    if (c->content_type) {
	free(c->content_type);
	c->content_type = NULL;
    }
    if (c->headers) {
	free_buffer(c->headers);
	c->headers = NULL;
    }
    if (c->sock != INVALID_SOCKET) {
	closesocket(c->sock);
	c->sock = INVALID_SOCKET;
    }
}

/* adds a worker to the worker list and returns 0. If the list is
 * full, the worker is finalized and returns -1.
 * Note that we don't need locking, because add_worker is guaranteed
 * to be called by the same thread (server thread).
  */
static int add_worker(httpd_conn_t *c) {
    unsigned int i = 0;
    for (; i < MAX_WORKERS; i++)
	if (!workers[i]) {
#ifdef WIN32
	    DBG(printf("registering worker %p as %d (thread=0x%x)\n", (void*) c, i, (int) c->thread));
#else
	    DBG(printf("registering worker %p as %d (handler=%p)\n", (void*) c, i, (void*) c->ih));
#endif
	    workers[i] = c;
	    return 0;
	}
    /* FIXME: ok no more space for a new worker - what do we do now?
     * for now we just drop it on the floor .. */
    finalize_worker(c);
    free(c);
    return -1;
}

/* finalize worker, remove it from the list and free the memory. If
 * the worker is owned by a thread, it is not finalized and the
 * THREAD_DISPOSE flag is set instead. */
static void remove_worker(httpd_conn_t *c)
{
    unsigned int i = 0;
    if (!c) return;
    if (c->attr & THREAD_OWNED) { /* if the worker is used by a
				   * thread, we can only signal for
				   * its removal */
	c->attr |= THREAD_DISPOSE;
	return;
    }
    finalize_worker(c);
    for (; i < MAX_WORKERS; i++)
	if (workers[i] == c)
	    workers[i] = NULL;
    DBG(printf("removing worker %p\n", (void*) c));
    free(c);
}

#ifndef Win32
extern int R_ignore_SIGPIPE; /* defined in src/main/main.c on unix */
#else
static int R_ignore_SIGPIPE; /* for simplicity of the code below */
#endif

static int send_response(SOCKET s, const char *buf, size_t len)
{
    unsigned int i = 0;
    /* we have to tell R to ignore SIGPIPE otherwise it can raise an error
       and get us into deep trouble */
    R_ignore_SIGPIPE = 1;
    while (i < len) {
	ssize_t n = send(s, buf + i, len - i, 0);
	if (n < 1) {
	    R_ignore_SIGPIPE = 0;
	    return -1;
	}
	i += n;
    }
    R_ignore_SIGPIPE = 0;
    return 0;
}

/* sends HTTP/x.x plus the text (which should be of the form " XXX ...") */
static int send_http_response(httpd_conn_t *c, const char *text) {
    char buf[96];
    const char *s = HTTP_SIG(c);
    size_t l = strlen(text);
    ssize_t res;
    /* reduce the number of packets by sending the payload en-block from buf */
    if (l < sizeof(buf) - 10) {
	strcpy(buf, s);
	strcpy(buf + 8, text);
	return send_response(c->sock, buf, l + 8);
    }
    R_ignore_SIGPIPE = 1;
    res = send(c->sock, s, 8, 0);
    R_ignore_SIGPIPE = 0;
    if (res < 8) return -1;
    return send_response(c->sock, text, strlen(text));
}

/* decode URI in place (decoding never expands) */
static void uri_decode(char *s)
{
    char *t = s;
    while (*s) {
	if (*s == '+') { /* + -> SPC */
	    *(t++) = ' '; s++;
	} else if (*s == '%') {
	    unsigned char ec = 0;
	    s++;
	    if (*s >= '0' && *s <= '9') ec |= ((unsigned char)(*s - '0')) << 4;
	    else if (*s >= 'a' && *s <= 'f') ec |= ((unsigned char)(*s - 'a' + 10)) << 4;
	    else if (*s >= 'A' && *s <= 'F') ec |= ((unsigned char)(*s - 'A' + 10)) << 4;
	    if (*s) s++;
	    if (*s >= '0' && *s <= '9') ec |= (unsigned char)(*s - '0');
	    else if (*s >= 'a' && *s <= 'f') ec |= (unsigned char)(*s - 'a' + 10);
	    else if (*s >= 'A' && *s <= 'F') ec |= (unsigned char)(*s - 'A' + 10);
	    if (*s) s++;
	    *(t++) = (char) ec;
	} else *(t++) = *(s++);
    }
    *t = 0;
}

/* parse a query string into a named character vector - must NOT be
 * URI decoded */
static SEXP parse_query(char *query)
{
    int parts = 0;
    SEXP res, names;
    char *s = query, *key = 0, *value = query, *t = query;
    while (*s) {
	if (*s == '&') parts++;
	s++;
    }
    parts++;
    res = PROTECT(allocVector(STRSXP, parts));
    names = PROTECT(allocVector(STRSXP, parts));
    s = query;
    parts = 0;
    while (1) {
	if (*s == '=' && !key) { /* first '=' in a part */
	    key = value;
	    *(t++) = 0;
	    value = t;
	    s++;
	} else if (*s == '&' || !*s) { /* next part */
	    int last_entry = !*s;
	    *(t++) = 0;
	    if (!key) key = "";
	    SET_STRING_ELT(names, parts, mkChar(key));
	    SET_STRING_ELT(res, parts, mkChar(value));
	    parts++;
	    if (last_entry) break;
	    key = 0;
	    value = t;
	    s++;
	} else if (*s == '+') { /* + -> SPC */
	    *(t++) = ' '; s++;
	} else if (*s == '%') { /* we cannot use uri_decode becasue we need &/= *before* decoding */
	    unsigned char ec = 0;
	    s++;
	    if (*s >= '0' && *s <= '9') ec |= ((unsigned char)(*s - '0')) << 4;
	    else if (*s >= 'a' && *s <= 'f') ec |= ((unsigned char)(*s - 'a' + 10)) << 4;
	    else if (*s >= 'A' && *s <= 'F') ec |= ((unsigned char)(*s - 'A' + 10)) << 4;
	    if (*s) s++;
	    if (*s >= '0' && *s <= '9') ec |= (unsigned char)(*s - '0');
	    else if (*s >= 'a' && *s <= 'f') ec |= (unsigned char)(*s - 'a' + 10);
	    else if (*s >= 'A' && *s <= 'F') ec |= (unsigned char)(*s - 'A' + 10);
	    if (*s) s++;
	    *(t++) = (char) ec;
	} else *(t++) = *(s++);
    }
    setAttrib(res, R_NamesSymbol, names);
    UNPROTECT(2);
    return res;
}

static SEXP R_ContentTypeName, R_HandlersName;

/* create an object representing the request body. It is NULL if the body is empty (or zero length).
 * In the case of a URL encoded form it will have the same shape as the query string (named string vector).
 * In all other cases it will be a raw vector with a "content-type" attribute (if specified in the headers) */
static SEXP parse_request_body(httpd_conn_t *c) {
    if (!c || !c->body) return R_NilValue;

    if (c->attr & CONTENT_FORM_UENC) { /* URL encoded form - return parsed form */
	c->body[c->content_length] = 0; /* the body is guaranteed to have an extra byte for the termination */
	return parse_query(c->body);
    } else { /* something else - pass it as a raw vector */
	SEXP res = PROTECT(Rf_allocVector(RAWSXP, c->content_length));
	if (c->content_length)
	    memcpy(RAW(res), c->body, c->content_length);
	if (c->content_type) { /* attach the content type so it can be interpreted */
	    if (!R_ContentTypeName) R_ContentTypeName = install("content-type");
	    setAttrib(res, R_ContentTypeName, mkString(c->content_type));
	}
	UNPROTECT(1);
	return res;
    }
}

#ifdef WIN32
/* on Windows we have to guarantee that process_request is performed
 * on the main thread, so we have to dispatch it through a message */
static void process_request_main_thread(httpd_conn_t *c);

static void process_request(httpd_conn_t *c)
{
    /* SendMessage is synchronous, so it will wait until the message
     * is processed */
    DBG(Rprintf("enqueuing process_request_main_thread\n"));
    SendMessage(message_window, WM_RHTTP_CALLBACK, 0, (LPARAM) c);
    DBG(Rprintf("process_request_main_thread returned\n"));
}
#define process_request process_request_main_thread
#endif

/* finalize a request - essentially for HTTP/1.0 it means that
 * we have to close the connection */
static void fin_request(httpd_conn_t *c) {
    if (!IS_HTTP_1_1(c))
	c->attr |= CONNECTION_CLOSE;
}

static SEXP custom_handlers_env;

/* returns a httpd handler (closure) for a given path. As a special case
 * it can return a symbol that will be resolved in the "tools" namespace.
 * currently it allows custom handlers for paths of the form
 * /custom/<name>[/.*] where <name> must less than 64 characters long
 * and is matched against closures in tools:::.httpd.handlers.env */
static SEXP handler_for_path(const char *path) {
    if (path && !strncmp(path, "/custom/", 8)) { /* starts with /custom/ ? */
	const char *c = path + 8, *e = c;
	while (*c && *c != '/') c++; /* find out the name */
	if (c - e > 0 && c - e < 64) { /* if it's 1..63 chars long, proceed */
	    char fn[64];
	    memcpy(fn, e, c - e); /* create a local C string with the name for the install() call */
	    fn[c - e] = 0;
	    DBG(Rprintf("handler_for_path('%s'): looking up custom handler '%s'\n", path, fn));
	    /* we cache custom_handlers_env so in case it has not been loaded yet, fetch it */
	    if (!custom_handlers_env) {
		if (!R_HandlersName) R_HandlersName = install(".httpd.handlers.env");
		custom_handlers_env = eval(R_HandlersName, R_FindNamespace(mkString("tools")));
	    }
	    /* we only proceed if .httpd.handlers.env really exists */
	    if (TYPEOF(custom_handlers_env) == ENVSXP) {
		SEXP cl = findVarInFrame3(custom_handlers_env, install(fn), TRUE);
		if (cl != R_UnboundValue && TYPEOF(cl) == CLOSXP) /* we need a closure */
		    return cl;
	    }
	}
    }
    DBG(Rprintf(" - falling back to default httpd\n"));
    return install("httpd");
}

/* process a request by calling the httpd() function in R */
static void process_request_(void *ptr)
{
    httpd_conn_t *c = (httpd_conn_t*) ptr;
    const char *ct = "text/html";
    char *query = 0, *s;
    SEXP sHeaders = R_NilValue;
    int code = 200;
    DBG(Rprintf("process request for %p\n", (void*) c));
    if (!c || !c->url) return; /* if there is not enough to process, bail out */
    s = c->url;
    while (*s && *s != '?') s++; /* find the query part */
    if (*s) {
	*(s++) = 0;
	query = s;
    }
    uri_decode(c->url); /* decode the path part */
    {   /* construct "try(httpd(url, query, body), silent=TRUE)" */
	SEXP sTrue = PROTECT(ScalarLogical(TRUE));
	SEXP sBody = PROTECT(parse_request_body(c));
	SEXP sQuery = PROTECT(query ? parse_query(query) : R_NilValue);
	SEXP sReqHeaders = PROTECT(c->headers ? collect_buffers(c->headers) : R_NilValue);
	SEXP sArgs = PROTECT(list4(mkString(c->url), sQuery, sBody, sReqHeaders));
	SEXP sTry = install("try");
	SEXP y, x = PROTECT(lang3(sTry,
				  LCONS(handler_for_path(c->url), sArgs),
				  sTrue));
	SET_TAG(CDR(CDR(x)), install("silent"));
	DBG(Rprintf("eval(try(httpd('%s'),silent=TRUE))\n", c->url));
	
	/* evaluate the above in the tools namespace */
	x = PROTECT(eval(x, R_FindNamespace(mkString("tools"))));

	/* the result is expected to have one of the following forms:

	   a) character vector of length 1 => error (possibly from try),
	      will create 500 response

	  b) list(payload[, content-type[, headers[, status code]]])

	      payload: can be a character vector of length one or a
	      raw vector. if the character vector is named "file" then
	      the content of a file of that name is the payload

	      content-type: must be a character vector of length one
	      or NULL (if present, else default is "text/html")

	      headers: must be a character vector - the elements will
	      have CRLF appended and neither Content-type nor
	      Content-length may be used

	     status code: must be an integer if present (default is 200)
	 */

	if (TYPEOF(x) == STRSXP && LENGTH(x) > 0) { /* string means there was an error */
	    const char *s = CHAR(STRING_ELT(x, 0));
	    send_http_response(c, " 500 Evaluation error\r\nConnection: close\r\nContent-type: text/plain\r\n\r\n");
	    DBG(Rprintf("respond with 500 and content: %s\n", s));
	    if (c->method != METHOD_HEAD)
		send_response(c->sock, s, strlen(s));
	    c->attr |= CONNECTION_CLOSE; /* force close */
	    UNPROTECT(7);
	    return;
	}

	if (TYPEOF(x) == VECSXP && LENGTH(x) > 0) { /* a list (generic vector) can be a real payload */
	    SEXP xNames = getAttrib(x, R_NamesSymbol);
	    if (LENGTH(x) > 1) {
		SEXP sCT = VECTOR_ELT(x, 1); /* second element is content type if present */
		if (TYPEOF(sCT) == STRSXP && LENGTH(sCT) > 0)
		    ct = CHAR(STRING_ELT(sCT, 0));
		if (LENGTH(x) > 2) { /* third element is headers vector */
		    sHeaders = VECTOR_ELT(x, 2);
		    if (TYPEOF(sHeaders) != STRSXP)
			sHeaders = R_NilValue;
		    if (LENGTH(x) > 3) /* fourth element is HTTP code */
			code = asInteger(VECTOR_ELT(x, 3));
		}
	    }
	    y = VECTOR_ELT(x, 0);
	    if (TYPEOF(y) == STRSXP && LENGTH(y) > 0) {
		char buf[64];
		const char *cs = CHAR(STRING_ELT(y, 0)), *fn = 0;
		if (code == 200)
		    send_http_response(c, " 200 OK\r\nContent-type: ");
		else {
		    sprintf(buf, "%s %d Code %d\r\nContent-type: ", HTTP_SIG(c), code, code);
		    send_response(c->sock, buf, strlen(buf));
		}
		send_response(c->sock, ct, strlen(ct));
		if (sHeaders != R_NilValue) {
		    unsigned int i = 0, n = LENGTH(sHeaders);
		    for (; i < n; i++) {
			const char *hs = CHAR(STRING_ELT(sHeaders, i));
			send_response(c->sock, "\r\n", 2);
			send_response(c->sock, hs, strlen(hs));
		    }
		}
		/* special content - a file: either list(file="") or list(c("*FILE*", "")) - the latter will go away */
		if (TYPEOF(xNames) == STRSXP && LENGTH(xNames) > 0 &&
		    !strcmp(CHAR(STRING_ELT(xNames, 0)), "file"))
		    fn = cs;
		if (LENGTH(y) > 1 && !strcmp(cs, "*FILE*"))
		    fn = CHAR(STRING_ELT(y, 1));
		if (fn) {
		    char *fbuf;
		    FILE *f = fopen(fn, "rb");
		    long fsz = 0;
		    if (!f) {
			send_response(c->sock, "\r\nContent-length: 0\r\n\r\n", 23);
			UNPROTECT(7);
			fin_request(c);
			return;
		    }
		    fseek(f, 0, SEEK_END);
		    fsz = ftell(f);
		    fseek(f, 0, SEEK_SET);
		    sprintf(buf, "\r\nContent-length: %ld\r\n\r\n", fsz);
		    send_response(c->sock, buf, strlen(buf));
		    if (c->method != METHOD_HEAD) {
			fbuf = (char*) malloc(32768);
			if (fbuf) {
			    while (fsz > 0 && !feof(f)) {
				size_t rd = (fsz > 32768) ? 32768 : fsz;
				if (fread(fbuf, 1, rd, f) != rd) {
				    free(fbuf);
				    UNPROTECT(7);
				    c->attr |= CONNECTION_CLOSE;
				    return;
				}
				send_response(c->sock, fbuf, rd);
				fsz -= rd;
			    }
			    free(fbuf);
			} else { /* allocation error - get out */
			    UNPROTECT(7);
			    c->attr |= CONNECTION_CLOSE;
			    return;
			}
		    }
		    fclose(f);
		    UNPROTECT(7);
		    fin_request(c);
		    return;
		}
		sprintf(buf, "\r\nContent-length: %u\r\n\r\n", (unsigned int) strlen(cs));
		send_response(c->sock, buf, strlen(buf));
		if (c->method != METHOD_HEAD)
		    send_response(c->sock, cs, strlen(cs));
		UNPROTECT(7);
		fin_request(c);
		return;
	    }
	    if (TYPEOF(y) == RAWSXP) {
		char buf[64];
		Rbyte *cs = RAW(y);
		if (code == 200)
		    send_http_response(c, " 200 OK\r\nContent-type: ");
		else {
		    sprintf(buf, "%s %d Code %d\r\nContent-type: ", HTTP_SIG(c), code, code);
		    send_response(c->sock, buf, strlen(buf));
		}
		send_response(c->sock, ct, strlen(ct));
		if (sHeaders != R_NilValue) {
		    unsigned int i = 0, n = LENGTH(sHeaders);
		    for (; i < n; i++) {
			const char *hs = CHAR(STRING_ELT(sHeaders, i));
			send_response(c->sock, "\r\n", 2);
			send_response(c->sock, hs, strlen(hs));
		    }
		}
		sprintf(buf, "\r\nContent-length: %u\r\n\r\n", LENGTH(y));
		send_response(c->sock, buf, strlen(buf));
		if (c->method != METHOD_HEAD)
		    send_response(c->sock, (char*) cs, LENGTH(y));
		UNPROTECT(7);
		fin_request(c);
		return;
	    }
	}
	UNPROTECT(7);
    }
    send_http_response(c, " 500 Invalid response from R\r\nConnection: close\r\nContent-type: text/plain\r\n\r\nServer error: invalid response from R\r\n");
    c->attr |= CONNECTION_CLOSE; /* force close */
}

/* wrap the actual call with ToplevelExec since we need to have a guaranteed
   return so we can track the presence of a worker code inside R to prevent
   re-entrance from other clients */
static void process_request(httpd_conn_t *c)
{
    in_process = 1;
    R_ToplevelExec(process_request_, c);
    in_process = 0;
}

#ifdef WIN32
#undef process_request
#endif

/* this function is called to fetch new data from the client
 * connection socket and process it */
static void worker_input_handler(void *data) {
    httpd_conn_t *c = (httpd_conn_t*) data;

    DBG(printf("worker_input_handler, data=%p\n", data));
    if (!c) return;

    if (in_process) return; /* we don't allow recursive entrance */

    DBG(printf("input handler for worker %p (sock=%d, part=%d, method=%d, line_pos=%d)\n", (void*) c, (int)c->sock, (int)c->part, (int)c->method, (int)c->line_pos));

    /* FIXME: there is one edge case that is not caught on unix: if
     * recv reads two or more full requests into the line buffer then
     * this function exits after the first one, but input handlers may
     * not trigger, because there may be no further data. It is not
     * trivial to fix, because just checking for a full line at the
     * beginning and not calling recv won't trigger a new input
     * handler. However, under normal circumstance this should not
     * happen, because clients should wait for the response and even
     * if they don't it's unlikely that both requests get combined
     * into one packet. */
    if (c->part < PART_BODY) {
	char *s = c->line_buf;
	ssize_t n = recv(c->sock, c->line_buf + c->line_pos, 
			 LINE_BUF_SIZE - c->line_pos - 1, 0);
	DBG(printf("[recv n=%d, line_pos=%d, part=%d]\n", n, c->line_pos, (int)c->part));
	if (n < 0) { /* error, scrape this worker */
	    remove_worker(c);
	    return;
	}
	if (n == 0) { /* connection closed -> try to process and then remove */
	    process_request(c);
	    remove_worker(c);
	    return;
	}
	c->line_pos += n;
	c->line_buf[c->line_pos] = 0;
	DBG(printf("in buffer: {%s}\n", c->line_buf));
	while (*s) {
	    /* ok, we have genuine data in the line buffer */
	    if (s[0] == '\n' || (s[0] == '\r' && s[1] == '\n')) { /* single, empty line - end of headers */
		/* --- check request validity --- */
		DBG(printf(" end of request, moving to body\n"));
		if (!(c->attr & HTTP_1_0) && !(c->attr & HOST_HEADER)) { /* HTTP/1.1 mandates Host: header */
		    send_http_response(c, " 400 Bad Request (Host: missing)\r\nConnection: close\r\n\r\n");
		    remove_worker(c);
		    return;
		}
		if (c->attr & CONTENT_LENGTH && c->content_length) {
		    if (c->content_length < 0 ||  /* we are parsing signed so negative numbers are bad */
			c->content_length > 2147483640 || /* R will currently have issues with body around 2Gb or more, so better to not go there */
			!(c->body = (char*) malloc(c->content_length + 1 /* allocate an extra termination byte */ ))) {
			send_http_response(c, " 413 Request Entity Too Large (request body too big)\r\nConnection: close\r\n\r\n");
			remove_worker(c);
			return;
		    }
		}
		c->body_pos = 0;
		c->part = PART_BODY;
		if (s[0] == '\r') s++;
		s++;
		/* move the body part to the beginning of the buffer */
		c->line_pos -= s - c->line_buf;
		memmove(c->line_buf, s, c->line_pos);
		/* GET/HEAD or no content length mean no body */
		if (c->method == METHOD_GET || c->method == METHOD_HEAD ||
		    !(c->attr & CONTENT_LENGTH) || c->content_length == 0) {
		    if ((c->attr & CONTENT_LENGTH) && c->content_length > 0) {
			send_http_response(c, " 400 Bad Request (GET/HEAD with body)\r\n\r\n");
			remove_worker(c);
			return;
		    }
		    process_request(c);
		    if (c->attr & CONNECTION_CLOSE) {
			remove_worker(c);
			return;
		    }
		    /* keep-alive - reset the worker so it can process a new request */
		    if (c->url) { free(c->url); c->url = NULL; }
		    if (c->body) { free(c->body); c->body = NULL; }
		    if (c->content_type) { free(c->content_type); c->content_type = NULL; }
		    if (c->headers) { free_buffer(c->headers); c->headers = NULL; }
		    c->body_pos = 0;
		    c->method = 0;
		    c->part = PART_REQUEST;
		    c->attr = 0;
		    c->content_length = 0;
		    return;
		}
		/* copy body content (as far as available) */
		c->body_pos = (c->content_length < c->line_pos) ? c->content_length : c->line_pos;
		if (c->body_pos) {
		    memcpy(c->body, c->line_buf, c->body_pos);
		    c->line_pos -= c->body_pos; /* NOTE: we are NOT moving the buffer since non-zero left-over causes connection close */
		}
		/* POST will continue into the BODY part */
		break;
	    }
	    {
		char *bol = s;
		while (*s && *s != '\r' && *s != '\n') s++;
		if (!*s) { /* incomplete line */
		    if (bol == c->line_buf) {
			if (c->line_pos < LINE_BUF_SIZE) /* one, incomplete line, but the buffer is not full yet, just return */
			    return;
			/* the buffer is full yet the line is incomplete - we're in trouble */
			send_http_response(c, " 413 Request entity too large\r\nConnection: close\r\n\r\n");
			remove_worker(c);
			return;
		    }
		    /* move the line to the begining of the buffer for later requests */
		    c->line_pos -= bol - c->line_buf;
		    memmove(c->line_buf, bol, c->line_pos);
		    return;
		} else { /* complete line, great! */
		    if (*s == '\r') *(s++) = 0;
		    if (*s == '\n') *(s++) = 0;
		    DBG(printf("complete line: {%s}\n", bol));
		    if (c->part == PART_REQUEST) {
			/* --- process request line --- */
			size_t rll = strlen(bol); /* request line length */
			char *url = strchr(bol, ' ');
			if (!url || rll < 14 || strncmp(bol + rll - 9, " HTTP/1.", 8)) { /* each request must have at least 14 characters [GET / HTTP/1.0] and have HTTP/1.x */
			    send_response(c->sock, "HTTP/1.0 400 Bad Request\r\n\r\n", 28);
			    remove_worker(c);
			    return;
			}
			url++;
			if (!strncmp(bol + rll - 3, "1.0", 3)) c->attr |= HTTP_1_0;
			if (!strncmp(bol, "GET ", 4)) c->method = METHOD_GET;
			if (!strncmp(bol, "POST ", 5)) c->method = METHOD_POST;
			if (!strncmp(bol, "HEAD ", 5)) c->method = METHOD_HEAD;
			/* only custom handlers can use other methods */
			if (!strncmp(url, "/custom/", 8)) {
			    char *mend = url - 1;
			    /* we generate a header with the method so it can be passed to the handler */
			    if (!c->headers)
				c->headers = alloc_buffer(1024, NULL);
			    /* make sure it fits */
			    if (c->headers->size - c->headers->length >= 18 + (mend - bol)) {
				if (!c->method) c->method = METHOD_OTHER;
				/* add "Request-Method: xxx" */
				memcpy(c->headers->data + c->headers->length, "Request-Method: ", 16);
				c->headers->length += 16;
				memcpy(c->headers->data + c->headers->length, bol, mend - bol);
				c->headers->length += mend - bol;	
				c->headers->data[c->headers->length++] = '\n';
			    }
			}
			if (!c->method) {
			    send_http_response(c, " 501 Invalid or unimplemented method\r\n\r\n");
			    remove_worker(c);
			    return;
			}
			bol[strlen(bol) - 9] = 0;
			c->url = strdup(url);
			c->part = PART_HEADER;
			DBG(printf("parsed request, method=%d, URL='%s'\n", (int)c->method, c->url));
		    } else if (c->part == PART_HEADER) {
			/* --- process headers --- */
			char *k = bol;
			if (!c->headers)
			    c->headers = alloc_buffer(1024, NULL);
			if (c->headers) { /* record the header line in the buffer */
			    size_t l = strlen(bol);
			    if (l) { /* this should be really always true */
				if (c->headers->length + l + 1 > c->headers->size) { /* not enough space? */
				    size_t fits = c->headers->size - c->headers->length;
				    if (fits) memcpy(c->headers->data + c->headers->length, bol, fits);
				    if (alloc_buffer(2048, c->headers)) {
					c->headers = c->headers->next;
					memcpy(c->headers->data, bol + fits, l - fits);
					c->headers->length = l - fits;
					c->headers->data[c->headers->length++] = '\n';
				    }
				} else {
				    memcpy(c->headers->data + c->headers->length, bol, l);
				    c->headers->length += l;	
				    c->headers->data[c->headers->length++] = '\n';
				}
			    }
			}
			while (*k && *k != ':') {
			    if (*k >= 'A' && *k <= 'Z')
				*k |= 0x20;
			    k++;
			}
			if (*k == ':') {
			    *(k++) = 0;
			    while (*k == ' ' || *k == '\t') k++;
			    DBG(printf("header '%s' => '%s'\n", bol, k));
			    if (!strcmp(bol, "content-length")) {
				c->attr |= CONTENT_LENGTH;
				c->content_length = atol(k);
			    }
			    if (!strcmp(bol, "content-type")) {
				char *l = k;
				while (*l) { if (*l >= 'A' && *l <= 'Z') *l |= 0x20; l++; }
				c->attr |= CONTENT_TYPE;
				if (c->content_type) free(c->content_type);
				c->content_type = strdup(k);
				if (!strncmp(k, "application/x-www-form-urlencoded", 33))
				    c->attr |= CONTENT_FORM_UENC;
			    }
			    if (!strcmp(bol, "host"))
				c->attr |= HOST_HEADER;
			    if (!strcmp(bol, "connection")) {
				char *l = k;
				while (*l) { if (*l >= 'A' && *l <= 'Z') *l |= 0x20; l++; }
				if (!strncmp(k, "close", 5))
				    c->attr |= CONNECTION_CLOSE;
			    }
			}
		    }
		}
	    }
	}
	if (c->part < PART_BODY) {
	    /* we end here if we processed a buffer of exactly one line */
	    c->line_pos = 0;
	    return;
	}
    }
    if (c->part == PART_BODY && c->body) { /* BODY  - this branch always returns */
	if (c->body_pos < c->content_length) { /* need to receive more ? */
	    DBG(printf("BODY: body_pos=%d, content_length=%ld\n", c->body_pos, c->content_length));
	    ssize_t n = recv(c->sock, c->body + c->body_pos, 
			    c->content_length - c->body_pos, 0);
	    DBG(printf("      [recv n=%d - had %u of %lu]\n", n, c->body_pos, c->content_length));
	    c->line_pos = 0;
	    if (n < 0) { /* error, scrap this worker */
		remove_worker(c);
		return;
	    }
	    if (n == 0) { /* connection closed -> try to process and then remove */
		process_request(c);
	    remove_worker(c);
		return;
	    }
	    c->body_pos += n;
	}
	if (c->body_pos == c->content_length) { /* yay! we got the whole body */
	    process_request(c);
	    if (c->attr & CONNECTION_CLOSE || c->line_pos) { /* we have to close the connection if there was a double-hit */
		remove_worker(c);
		return;
	    }
	    /* keep-alive - reset the worker so it can process a new request */
	    if (c->url) { free(c->url); c->url = NULL; }
	    if (c->body) { free(c->body); c->body = NULL; }
	    if (c->content_type) { free(c->content_type); c->content_type = NULL; }
	    if (c->headers) { free_buffer(c->headers); c->headers = NULL; }
	    c->line_pos = 0; c->body_pos = 0;
	    c->method = 0;
	    c->part = PART_REQUEST;
	    c->attr = 0;
	    c->content_length = 0;
	    return;
	}
    }

    /* we enter here only if recv was used to leave the headers with no body */
    if (c->part == PART_BODY && !c->body) {
	char *s = c->line_buf;
	if (c->line_pos > 0) {
	    if ((s[0] != '\r' || s[1] != '\n') && (s[0] != '\n')) {
		send_http_response(c, " 411 length is required for non-empty body\r\nConnection: close\r\n\r\n");
		remove_worker(c);
		return;
	    }
	    /* empty body, good */
	    process_request(c);
	    if (c->attr & CONNECTION_CLOSE) {
		remove_worker(c);
		return;
	    } else { /* keep-alive */
		int sh = 1;
		if (s[0] == '\r') sh++;
		if (c->line_pos <= sh)
		    c->line_pos = 0;
		else { /* shift the remaining buffer */
		    memmove(c->line_buf, c->line_buf + sh, c->line_pos - sh);
		    c->line_pos -= sh;
		}
		/* keep-alive - reset the worker so it can process a new request */
		if (c->url) { free(c->url); c->url = NULL; }
		if (c->body) { free(c->body); c->body = NULL; }
		if (c->content_type) { free(c->content_type); c->content_type = NULL; }
		if (c->headers) { free_buffer(c->headers); c->headers = NULL; }
		c->body_pos = 0;
		c->method = 0;
		c->part = PART_REQUEST;
		c->attr = 0;
		c->content_length = 0;
		return;
	    }
	}
	ssize_t n = recv(c->sock, c->line_buf + c->line_pos, 
			 LINE_BUF_SIZE - c->line_pos - 1, 0);
	if (n < 0) { /* error, scrap this worker */
	    remove_worker(c);
	    return;
	}
	if (n == 0) { /* connection closed -> try to process and then remove */
	    process_request(c);
	    remove_worker(c);
	    return;
	}
	if ((s[0] != '\r' || s[1] != '\n') && (s[0] != '\n')) {
	    send_http_response(c, " 411 length is required for non-empty body\r\nConnection: close\r\n\r\n");
	    remove_worker(c);
	    return;
	}
    }
}

static void srv_input_handler(void *data);

static SOCKET srv_sock = INVALID_SOCKET;

#ifdef WIN32
/* Windows implementation uses threads to accept and serve
   connections, using the main event loop to synchronize with R
   through a message-only window which is created on the R thread
 */
static LRESULT CALLBACK RhttpdWindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    DBG(Rprintf("RhttpdWindowProc(%x, %x, %x, %x)\n", (int) hwnd, (int) uMsg, (int) wParam, (int) lParam));
    if (hwnd == message_window && uMsg == WM_RHTTP_CALLBACK) {
	httpd_conn_t *c = (httpd_conn_t*) lParam;
	process_request_main_thread(c);
	return 0;
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

/* server thread - accepts connections on the server socket and
   creates worker threads
 */
static DWORD WINAPI ServerThreadProc(LPVOID lpParameter) {
    while (srv_sock != INVALID_SOCKET) {
	srv_input_handler(lpParameter);
    }
    return 0;
}

/* worker thread - processes one client connection socket */
static DWORD WINAPI WorkerThreadProc(LPVOID lpParameter) {
    httpd_conn_t *c = (httpd_conn_t*) lpParameter;
    if (!c) return 0;
    while ((c->attr & THREAD_DISPOSE) == 0) {
	c->attr |= THREAD_OWNED; /* make sure the worker is not removed by the handler since we need it */
	worker_input_handler(c);
    }
    /* the handler signalled a desire to remove the worker, do it */
    c->attr = 0; /* reset the flags */
    remove_worker(c); /* free the worker */
    return 0;
}

/* global server thread - currently we support only one server at a time */
HANDLE server_thread;
#else
/* on unix we register all used sockets (server and workers) as input
 * handlers such that we can avoid polling */

/* global input handler for the server socket */
static InputHandler *srv_handler;
#endif

static void srv_input_handler(void *data)
{
    httpd_conn_t *c;
    SAIN peer_sa;
    socklen_t peer_sal = sizeof(peer_sa);
    SOCKET cl_sock = accept(srv_sock, (SA*) &peer_sa, &peer_sal);
    if (cl_sock == INVALID_SOCKET) /* accept failed, don't bother */
	return;
    c = (httpd_conn_t*) calloc(1, sizeof(httpd_conn_t));
    c->sock = cl_sock;
    c->peer = peer_sa.sin_addr;
#ifndef WIN32
    c->ih = addInputHandler(R_InputHandlers, cl_sock, &worker_input_handler,
			    HttpdWorkerActivity);
    if (c->ih) c->ih->userData = c;
    add_worker(c);
#else
    if (!add_worker(c)) { /* create worker thread only if the worker
			   * was accepted */
	if (!(c->thread = CreateThread(NULL, 0, WorkerThreadProc,
				       (LPVOID) c, 0, 0)))
	    remove_worker(c);
    }
#endif
}

int in_R_HTTPDCreate(const char *ip, int port) 
{
#ifndef WIN32
    int reuse = 1;
#endif
    SAIN srv_sa;

    if (needs_init) /* initialization may need to be performed on first use */
	first_init();

    /* is already in use, close the current socket */
    if (srv_sock != INVALID_SOCKET)
	closesocket(srv_sock);

#ifdef WIN32
    /* on Windows stop the server thread if it exists */
    if (server_thread) {
	DWORD ts = 0;
	if (GetExitCodeThread(server_thread, &ts) && ts == STILL_ACTIVE)
	    TerminateThread(server_thread, 0);
	server_thread = 0;
    }
#endif

    /* create a new socket */
    srv_sock = socket(AF_INET, SOCK_STREAM, 0);
    if (srv_sock == INVALID_SOCKET)
	Rf_error("unable to create socket");

#ifndef WIN32
    /* set socket for reuse so we can re-init if we die */
    /* But on Windows, this lets us stomp on any port already in use, so don't do it. */
    setsockopt(srv_sock, SOL_SOCKET, SO_REUSEADDR,
	       (const char*)&reuse, sizeof(reuse));
#endif

    /* bind to the desired port */
    if (bind(srv_sock, build_sin(&srv_sa, ip, port), sizeof(srv_sa))) {
	if (sockerrno == EADDRINUSE) {
	    closesocket(srv_sock);
	    srv_sock = INVALID_SOCKET;
	    return -2;
	} else {
	    closesocket(srv_sock);
	    srv_sock = INVALID_SOCKET;
	    Rf_error("unable to bind socket to TCP port %d", port);
	}
    }

    /* setup listen */
    if (listen(srv_sock, 8))
	Rf_error("cannot listen to TCP port %d", port);

#ifndef WIN32
    /* all went well, register the socket as a handler */
    if (srv_handler) removeInputHandler(&R_InputHandlers, srv_handler);
    srv_handler = addInputHandler(R_InputHandlers, srv_sock,
				  &srv_input_handler, HttpdServerActivity);
#else
    /* do the desired Windows synchronization */
    server_thread = CreateThread(NULL, 0, ServerThreadProc, 0, 0, 0);
#endif
    return 0;
}

void in_R_HTTPDStop(void)
{
    if (srv_sock != INVALID_SOCKET) closesocket(srv_sock);
    srv_sock = INVALID_SOCKET;

#ifdef WIN32
    /* on Windows stop the server thread if it exists */
    if (server_thread) {
	DWORD ts = 0;
	if (GetExitCodeThread(server_thread, &ts) && ts == STILL_ACTIVE)
	    TerminateThread(server_thread, 0);
	server_thread = 0;
    }
#else
    if (srv_handler) removeInputHandler(&R_InputHandlers, srv_handler);
#endif
}

/* Create an internal http server in R. Note that currently there can
   only be at most one http server running at any given time so the
   behavior is undefined if a server already exists (currently any
   previous servers will be shut down by this call but the shutdown
   may not be clean).

   @param sIP is the IP to bind to (or NULL for any)
   @param sPort is the TCP port number to bin to
   @return returns an integer value -- 0L on success, other values
   denote failures: -2L means that the address/port combination is
   already in use
*/
SEXP R_init_httpd(SEXP sIP, SEXP sPort)
{
    const char *ip = 0;
    if (sIP != R_NilValue && (TYPEOF(sIP) != STRSXP || LENGTH(sIP) != 1))
	Rf_error("invalid bind address specification");
    if (sIP != R_NilValue)
	ip = CHAR(STRING_ELT(sIP, 0));
    return ScalarInteger(in_R_HTTPDCreate(ip, asInteger(sPort)));
}
