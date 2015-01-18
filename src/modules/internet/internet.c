/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2015   The R Core Team.
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

/* <UTF8> the only interpretation of char is ASCII */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <R_ext/R-ftp-http.h>
#include <errno.h>
#include <R_ext/Print.h>

static void *in_R_HTTPOpen(const char *url, const char *headers, const int cacheOK);
static int   in_R_HTTPRead(void *ctx, char *dest, int len);
static void  in_R_HTTPClose(void *ctx);

static void *in_R_FTPOpen(const char *url);
static int   in_R_FTPRead(void *ctx, char *dest, int len);
static void  in_R_FTPClose(void *ctx);
SEXP in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho);
Rconnection in_newCurlUrl(const char *description, const char * const mode);


#include <Rmodules/Rinternet.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
/* Solaris and AIX define open as open64 under some circumstances */
# undef open
#endif

/* ------------------- internet access functions  --------------------- */

#if defined(USE_WININET_ASYNC) && !defined(USE_WININET)
#define USE_WININET 2
#endif

static Rboolean IDquiet=TRUE;

static Rboolean url_open(Rconnection con)
{
    void *ctxt;
    char *url = con->description;
    UrlScheme type = ((Rurlconn)(con->private))->type;

    if(con->mode[0] != 'r') {
	REprintf("can only open URLs for reading");
	return FALSE;
    }

    switch(type) {
    case HTTPsh:
#ifdef USE_WININET
    case HTTPSsh:
#endif
    {
	SEXP sheaders, agentFun;
	const char *headers;
	SEXP s_makeUserAgent = install("makeUserAgent");
#ifdef USE_WININET
	PROTECT(agentFun = lang2(s_makeUserAgent, ScalarLogical(0)));
#else
	PROTECT(agentFun = lang1(s_makeUserAgent));
#endif
	PROTECT(sheaders = eval(agentFun, R_FindNamespace(mkString("utils"))));

	if(TYPEOF(sheaders) == NILSXP)
	    headers = NULL;
	else
	    headers = CHAR(STRING_ELT(sheaders, 0));
	ctxt = in_R_HTTPOpen(url, headers, 0);
	UNPROTECT(2);
	if(ctxt == NULL) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->private))->ctxt = ctxt;
    }
	break;
    case FTPsh:
	ctxt = in_R_FTPOpen(url);
	if(ctxt == NULL) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	  /* error("cannot open URL '%s'", url); */
	    return FALSE;
	}
	((Rurlconn)(con->private))->ctxt = ctxt;
	break;

#if defined Win32 && !defined USE_WININET
    case HTTPSsh:
	warning(_("for https:// URLs use setInternet2(TRUE)"));
	return FALSE;
#endif

    default:
	warning(_("URL scheme unsupported by this method"));
	return FALSE;
    }

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static void url_close(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    switch(type) {
    case HTTPSsh:
	in_R_HTTPClose(((Rurlconn)(con->private))->ctxt);
	break;
    case FTPsh:
	in_R_FTPClose(((Rurlconn)(con->private))->ctxt);
	break;
    default:
	break;
    }
    con->isopen = FALSE;
}

static int url_fgetc_internal(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    void * ctxt = ((Rurlconn)(con->private))->ctxt;
    unsigned char c;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
	n = in_R_HTTPRead(ctxt, (char *)&c, 1);
	break;
    case FTPsh:
	n = in_R_FTPRead(ctxt, (char *)&c, 1);
	break;
    default:
	break;
    }
    return (n == 1) ? c : R_EOF;
}

static size_t url_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    void * ctxt = ((Rurlconn)(con->private))->ctxt;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
	n = in_R_HTTPRead(ctxt, ptr, (int)(size*nitems));
	break;
    case FTPsh:
	n = in_R_FTPRead(ctxt, ptr, (int)(size*nitems));
	break;
    default:
	break;
    }
    return n/size;
}


static Rconnection in_R_newurl(const char *description, const char * const mode)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of url connection failed"));
    new->class = (char *) malloc(strlen("url") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of url connection failed"));
    }
    strcpy(new->class, "url");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of url connection failed"));
    }
    init_con(new, description, CE_NATIVE, mode);
    new->canwrite = FALSE;
    new->open = &url_open;
    new->close = &url_close;
    new->fgetc_internal = &url_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->read = &url_read;
    new->private = (void *) malloc(sizeof(struct urlconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of url connection failed"));
    }

    IDquiet = TRUE;
    return new;
}



#ifndef Win32
static void putdots(DLsize_t *pold, DLsize_t new)
{
    DLsize_t i, old = *pold;
    *pold = new;
    for(i = old; i < new; i++) {
	REprintf(".");
	if((i+1) % 50 == 0) REprintf("\n");
	else if((i+1) % 10 == 0) REprintf(" ");
    }
    if(R_Consolefile) fflush(R_Consolefile);
}

static void putdashes(int *pold, int new)
{
    int i, old = *pold;
    *pold = new;
    for(i = old; i < new; i++)  REprintf("=");
    if(R_Consolefile) fflush(R_Consolefile);
}
#endif

/* note, ALL the possible structures have the first two elements */
typedef struct {
    DLsize_t length;
    char *type;
    void *ctxt;
} inetconn;

#ifdef Win32
#include <ga.h>

typedef struct {
    window wprog;
    progressbar pb;
    label l_url;
    RCNTXT cntxt;
    int pc;
} winprogressbar;

static winprogressbar pbar = {NULL, NULL, NULL};

static void doneprogressbar(void *data)
{
    winprogressbar *pbar = data;
    hide(pbar->wprog);
}
#endif

/* download(url, destfile, quiet, mode, headers, cacheOK) */

#define CPBUFSIZE 65536
#define IBUFSIZE 4096
static SEXP in_do_download(SEXP args)
{
    SEXP scmd, sfile, smode, sheaders, agentFun;
    const char *url, *file, *mode, *headers;
    int quiet, status = 0, cacheOK;
#ifdef Win32
    char pbuf[30];
    int pc;
#endif

    scmd = CAR(args); args = CDR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    if(length(scmd) > 1)
	warning(_("only first element of 'url' argument used"));
    url = CHAR(STRING_ELT(scmd, 0));
    sfile = CAR(args); args = CDR(args);
    if(!isString(sfile) || length(sfile) < 1)
	error(_("invalid '%s' argument"), "destfile");
    if(length(sfile) > 1)
	warning(_("only first element of 'destfile' argument used"));
    file = translateChar(STRING_ELT(sfile, 0));
    IDquiet = quiet = asLogical(CAR(args)); args = CDR(args);
    if(quiet == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quiet");
    smode =  CAR(args); args = CDR(args);
    if(!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = CHAR(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args));
    if(cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");
#ifdef USE_WININET
    PROTECT(agentFun = lang2(install("makeUserAgent"), ScalarLogical(0)));
#else
    PROTECT(agentFun = lang1(install("makeUserAgent")));
#endif
    PROTECT(sheaders = eval(agentFun, R_FindNamespace(mkString("utils"))));
    UNPROTECT(1);
    if(TYPEOF(sheaders) == NILSXP)
	headers = NULL;
    else
	headers = CHAR(STRING_ELT(sheaders, 0));
#ifdef Win32
    if (!quiet && !pbar.wprog) {
	pbar.wprog = newwindow(_("Download progress"), rect(0, 0, 540, 100),
			       Titlebar | Centered);
	setbackground(pbar.wprog, dialog_bg());
	pbar.l_url = newlabel(" ", rect(10, 15, 520, 25), AlignCenter);
	pbar.pb = newprogressbar(rect(20, 50, 500, 20), 0, 1024, 1024, 1);
	pbar.pc = 0;
    }
#endif
    if(strncmp(url, "file://", 7) == 0) {
	FILE *in, *out;
	static char buf[CPBUFSIZE];
	size_t n;
	int nh = 7;
#ifdef Win32
	/* on Windows we have file:///d:/path/to
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif

	/* Use binary transfers */
	in = R_fopen(R_ExpandFileName(url+nh), (mode[2] == 'b') ? "rb" : "r");
	if(!in) {
	    error(_("cannot open URL '%s', reason '%s'"),
		  url, strerror(errno));
	}

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    fclose(in);
	    error(_("cannot open destfile '%s', reason '%s'"),
		  file, strerror(errno));
	}
	while((n = fread(buf, 1, CPBUFSIZE, in)) > 0) {
	    size_t res = fwrite(buf, 1, n, out);
	    if(res != n) error(_("write failed"));
	}
	fclose(out); fclose(in);

#ifdef HAVE_INTERNET
    } else if (strncmp(url, "http://", 7) == 0
#ifdef USE_WININET
	       || strncmp(url, "https://", 8) == 0
#endif
	) {

	FILE *out;
	void *ctxt;
	DLsize_t len, total, guess, nbytes = 0;
	char buf[IBUFSIZE];
#ifndef Win32
	int ndashes = 0;
	DLsize_t ndots = 0;
#else
	int factor = 1;
#endif

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    error(_("cannot open destfile '%s', reason '%s'"),
		  file, strerror(errno));
	}

	R_Busy(1);
	if(!quiet) REprintf(_("trying URL '%s'\n"), url);
#ifdef Win32
	R_FlushConsole();
#endif
	ctxt = in_R_HTTPOpen(url, headers, cacheOK);
	if(ctxt == NULL) status = 1;
	else {
	    if(!quiet) REprintf(_("opened URL\n"), url);
	    guess = total = ((inetconn *)ctxt)->length;
#ifdef Win32
	    if (guess <= 0) guess = 100 * 1024;
	    if (guess > 1e9) factor = guess/1e6;
	    R_FlushConsole();
	    strcpy(buf, "URL: ");
	    if(strlen(url) > 60) {
		strcat(buf, "... ");
		strcat(buf, url + (strlen(url) - 60));
	    } else strcat(buf, url);
	    if(!quiet) {
		settext(pbar.l_url, buf);
		setprogressbarrange(pbar.pb, 0, guess/factor);
		setprogressbar(pbar.pb, 0);
		settext(pbar.wprog, "Download progress");
		show(pbar.wprog);
		begincontext(&(pbar.cntxt), CTXT_CCODE, R_NilValue, R_NilValue,
			     R_NilValue, R_NilValue, R_NilValue);
		pbar.cntxt.cend = &doneprogressbar;
		pbar.cntxt.cenddata = &pbar;
		pbar.pc = 0;
	    }
#endif
	    while ((len = in_R_HTTPRead(ctxt, buf, sizeof(buf))) > 0) {
		size_t res = fwrite(buf, 1, len, out);
		if(res != len) error(_("write failed"));
		nbytes += len;
#ifdef Win32
		if(!quiet) {
		    if(nbytes > guess) {
			guess *= 2;
			if (guess > 1e9) factor = guess/1e6;
			setprogressbarrange(pbar.pb, 0, guess/factor);
		    }
		    setprogressbar(pbar.pb, nbytes/factor);
		    if (total > 0) {
			pc = 0.499 + 100.0*nbytes/total;
			if (pc > pbar.pc) {
			    snprintf(pbuf, 30, "%d%% downloaded", pc);
			    settext(pbar.wprog, pbuf);
			    pbar.pc = pc;
			}
		    }
		}
#else
		if(!quiet) {
		    if(guess <= 0) putdots(&ndots, nbytes/1024);
		    else putdashes(&ndashes, (int)(50*nbytes/guess));
		}
#endif
	    }
	    in_R_HTTPClose(ctxt);
	    if(!quiet) {
#ifndef Win32
		REprintf("\n");
#endif
		if(nbytes > 1024*1024)
		    REprintf("downloaded %0.1f MB\n\n",
			     (double)nbytes/1024/1024, url);
		else if(nbytes > 10240)
		    REprintf("downloaded %d KB\n\n", (int) nbytes/1024, url);
		else
		    REprintf("downloaded %d bytes\n\n", (int) nbytes, url);
	    }
#ifdef Win32
	    R_FlushConsole();
	    if(!quiet) {
		endcontext(&(pbar.cntxt));
		doneprogressbar(&pbar);
	    }
#endif
	    if (total > 0 && total != nbytes)
		warning(_("downloaded length %0.f != reported length %0.f"),
			(double)nbytes, (double)total);
	}
	fclose(out);
	R_Busy(0);
	if (status == 1) error(_("cannot open URL '%s'"), url);

    } else if (strncmp(url, "ftp://", 6) == 0) {

	FILE *out;
	void *ctxt;
	DLsize_t len, total, guess, nbytes = 0;
	char buf[IBUFSIZE];
#ifndef Win32
	int ndashes = 0;
	DLsize_t ndots = 0;
#else
	int factor = 1;
#endif

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    error(_("cannot open destfile '%s', reason '%s'"),
		  file, strerror(errno));
	}

	R_Busy(1);
	if(!quiet) REprintf(_("trying URL '%s'\n"), url);
#ifdef Win32
	R_FlushConsole();
#endif
	ctxt = in_R_FTPOpen(url);
	if(ctxt == NULL) status = 1;
	else {
	    if(!quiet) REprintf(_("opened URL\n"), url);
	    guess = total = ((inetconn *)ctxt)->length;
#ifdef Win32
	    if (guess <= 0) guess = 100 * 1024;
	    if (guess > 1e9) factor = guess/1e6;
	    R_FlushConsole();
	    strcpy(buf, "URL: ");
	    if(strlen(url) > 60) {
		strcat(buf, "... ");
		strcat(buf, url + (strlen(url) - 60));
	    } else strcat(buf, url);
	    if(!quiet) {
		settext(pbar.l_url, buf);
		setprogressbarrange(pbar.pb, 0, guess/factor);
		setprogressbar(pbar.pb, 0);
		settext(pbar.wprog, "Download progress");
		show(pbar.wprog);

		/* set up a context which will close progressbar on error. */
		begincontext(&(pbar.cntxt), CTXT_CCODE, R_NilValue, R_NilValue,
			     R_NilValue, R_NilValue, R_NilValue);
		pbar.cntxt.cend = &doneprogressbar;
		pbar.cntxt.cenddata = &pbar;
		pbar.pc = 0;
	    }

#endif
	    while ((len = in_R_FTPRead(ctxt, buf, sizeof(buf))) > 0) {
		size_t res = fwrite(buf, 1, len, out);
		if(res != len) error(_("write failed"));
		nbytes += len;
#ifdef Win32
		if(!quiet) {
		    if(nbytes > guess) {
			guess *= 2;
			if (guess > 1e9) factor = guess/1e6;
			setprogressbarrange(pbar.pb, 0, guess/factor);
		    }
		    setprogressbar(pbar.pb, nbytes/factor);
		    if (total > 0) {
			pc = 0.499 + 100.0*nbytes/total;
			if (pc > pbar.pc) {
			    snprintf(pbuf, 30, "%d%% downloaded", pc);
			    settext(pbar.wprog, pbuf);
			    pbar.pc = pc;
			}
		    }
		}
#else
		if(!quiet) {
		    if(guess <= 0) putdots(&ndots, nbytes/1024);
		    else putdashes(&ndashes, (int)(50*nbytes/guess));
		}
#endif
	    }
	    in_R_FTPClose(ctxt);
	    if(!quiet) {
#ifndef Win32
		REprintf("\n");
#endif
		if(nbytes > 1024*1024)
		    REprintf("downloaded %0.1f MB\n\n",
			     (double)nbytes/1024/1024, url);
		else if(nbytes > 10240)
		    REprintf("downloaded %d KB\n\n", (int) nbytes/1024, url);
		else
		    REprintf("downloaded %d bytes\n\n", (int) nbytes, url);
	    }
#ifdef Win32
	    R_FlushConsole();
	    if(!quiet) {
		endcontext(&(pbar.cntxt));
		doneprogressbar(&pbar);
	    }
#endif
	    if (total > 0 && total != nbytes)
		warning(_("downloaded length %0.f != reported length %0.f"),
			(double)nbytes, (double)total);
	}
	R_Busy(0);
	fclose(out);
	if (status == 1) error(_("cannot open URL '%s'"), url);
#endif

    } else
	error(_("unsupported URL scheme"));

    UNPROTECT(1);
    return ScalarInteger(status);
}


#if defined(SUPPORT_LIBXML) && !defined(USE_WININET)

void *in_R_HTTPOpen(const char *url, const char *headers, const int cacheOK)
{
    inetconn *con;
    void *ctxt;
    int timeout = asInteger(GetOption1(install("timeout")));
    DLsize_t len = -1;
    char *type = NULL;

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;

    RxmlNanoHTTPTimeout(timeout);
    ctxt = RxmlNanoHTTPOpen(url, NULL, headers, cacheOK);
    if(ctxt != NULL) {
	int rc = RxmlNanoHTTPReturnCode(ctxt);
	if(rc != 200) {
	    warning(_("cannot open: HTTP status was '%d %s'"), rc,
		    RxmlNanoHTTPStatusMsg(ctxt));
	    RxmlNanoHTTPClose(ctxt);
	    return NULL;
	} else {
	    type = RxmlNanoHTTPContentType(ctxt);
	    len = RxmlNanoHTTPContentLength(ctxt);
	    if(!IDquiet){
		REprintf("Content type '%s'", type ? type : "unknown");
		if(len > 1024*1024)
		    // might be longer than long, and is on 64-bit windows
		    REprintf(" length %0.0f bytes (%0.1f MB)\n", (double)len,
			len/1024.0/1024.0);
		else if(len > 10240)
		    REprintf(" length %d bytes (%d KB)\n", 
			     (int)len, (int)(len/1024));
		else if(len >= 0)
		    REprintf(" length %d bytes\n", (int)len);
		else REprintf(" length unknown\n", len);
#ifdef Win32
		R_FlushConsole();
#endif
	    }
	}
    } else return NULL;
    con = (inetconn *) malloc(sizeof(inetconn));
    if(con) {
	con->length = len;
	con->type = type;
	con->ctxt = ctxt;
    }
    return con;
}

static int in_R_HTTPRead(void *ctx, char *dest, int len)
{
    return RxmlNanoHTTPRead(((inetconn *)ctx)->ctxt, dest, len);
}

static void in_R_HTTPClose(void *ctx)
{
    if(ctx) {
	RxmlNanoHTTPClose(((inetconn *)ctx)->ctxt);
	free(ctx);
    }
}

static void *in_R_FTPOpen(const char *url)
{
    inetconn *con;
    void *ctxt;
    int timeout = asInteger(GetOption1(install("timeout")));
    DLsize_t len = 0;

    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    RxmlNanoFTPTimeout(timeout);
    ctxt = RxmlNanoFTPOpen(url);
    if(!ctxt) return NULL;
    if(!IDquiet) {
	len = RxmlNanoFTPContentLength(ctxt);
	if(len >= 0)
	    REprintf("ftp data connection made, file length %ld bytes\n", len);
	else
	    REprintf("ftp data connection made, file length unknown\n");
#ifdef Win32
	R_FlushConsole();
#endif
    }
    con = (inetconn *) malloc(sizeof(inetconn));
    if(con) {
	con->length = len;
	con->type = NULL;
	con->ctxt = ctxt;
    }
    return con;
}

static int in_R_FTPRead(void *ctx, char *dest, int len)
{
    return RxmlNanoFTPRead(((inetconn *)ctx)->ctxt, dest, len);
}

static void in_R_FTPClose(void *ctx)
{
    if(ctx) {
	RxmlNanoFTPClose(((inetconn *)ctx)->ctxt);
	free(ctx);
    }
}
#endif /* SUPPORT_LIBXML */


#ifdef USE_WININET

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wininet.h>
typedef struct wictxt {
    int length;
    char * type;
    HINTERNET hand;
    HINTERNET session;
} wIctxt, *WIctxt;

#ifdef USE_WININET_ASYNC
static int timeout;

static int callback_status;
static LPINTERNET_ASYNC_RESULT callback_res;

static void CALLBACK
InternetCallback(HINTERNET hInternet, DWORD context, DWORD Status,
		 LPVOID lpvStatusInformation,
		 DWORD dwStatusInformationLength)
{
    callback_status = Status;
    /* printf("callback with context %ld, code %ld\n", context, Status); */
    if(Status == INTERNET_STATUS_REQUEST_COMPLETE) {
	callback_res = (LPINTERNET_ASYNC_RESULT) lpvStatusInformation;
    }
}
#endif /* USE_WININET_ASYNC */

static void *in_R_HTTPOpen(const char *url, const char *headers,
			   const int cacheOK)
{
    WIctxt  wictxt;
    DWORD status, d1 = 4, d2 = 0, d3 = 100;
    char buf[101], *p;

/*	BOOL res = InternetAttemptConnect(0);

	if (res != ERROR_SUCCESS) {
	warning("no Internet connection available");
	return NULL;
	}*/

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = NULL;
    wictxt->hand =
	InternetOpen(headers, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL,
#ifdef USE_WININET_ASYNC
		     INTERNET_FLAG_ASYNC
#else
		     0
#endif
		     );
    if(!wictxt->hand) {
	free(wictxt);
	/* error("cannot open Internet connection"); */
	return NULL;
    }

#ifdef USE_WININET_ASYNC
    timeout = asInteger(GetOption1(install("timeout")));
    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    InternetSetStatusCallback(wictxt->hand,
			      (INTERNET_STATUS_CALLBACK) InternetCallback);
/*    if(!IDquiet) {
	REprintf("using Asynchronous WinInet calls, timeout %d secs\n",
		timeout);
	R_FlushConsole();
	}*/

    callback_status = 0;
    InternetOpenUrl(wictxt->hand, url,
		    NULL, 0,
	INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE,
		    17);

    {
	DWORD t1 = GetTickCount();
	while(callback_status != INTERNET_STATUS_REQUEST_COMPLETE
	      && GetTickCount() < t1 + 1000*timeout) {
	    R_ProcessEvents();
	    Sleep(100);
	}
	if(callback_status != INTERNET_STATUS_REQUEST_COMPLETE) {
	    InternetCloseHandle(wictxt->hand);
	    free(wictxt);
	    warning(_("InternetOpenUrl timed out"));
	    return NULL;
	}
    }

    wictxt->session = (HINTERNET) callback_res->dwResult;
#else
/*    if(!IDquiet) {
	REprintf("using Synchronous WinInet calls\n");
	R_FlushConsole();
	} */
    wictxt->session = InternetOpenUrl(wictxt->hand, url,
				      NULL, 0,
	INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE,
				      0);
#endif /* USE_WININET_ASYNC */
    if(!wictxt->session) {
	DWORD err1 = GetLastError(), err2, blen = 101;
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	if (err1 == ERROR_INTERNET_EXTENDED_ERROR) {
	    InternetGetLastResponseInfo(&err2, buf, &blen);
	    /* some of these messages end in \r\n */
	    while(1) {
		p = buf + strlen(buf) - 1;
		if(*p == '\n' || *p == '\r') *p = '\0'; else break;
	    }
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return NULL;
	} else {
	    FormatMessage(
		FORMAT_MESSAGE_FROM_HMODULE,
		GetModuleHandle("wininet.dll"),
		err1,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		buf, 101, NULL);
	    /* some of these messages end in \r\n */
	    while(1) {
		p = buf + strlen(buf) - 1;
		if(*p == '\n' || *p == '\r') *p = '\0'; else break;
	    }
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return NULL;
	}
    }

    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
		  &status, &d1, &d2);
    if(status != 200) {
	d2 = 0;
	HttpQueryInfo(wictxt->session,
		      HTTP_QUERY_STATUS_TEXT, &buf, &d3, &d2);
	InternetCloseHandle(wictxt->session);
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	warning(_("cannot open: HTTP status was '%d %s'"), status, buf);
	return NULL;
    }

    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_CONTENT_TYPE, &buf, &d3, &d2);
    d2 = 0;
    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER,
		  &status, &d1, &d2);
    wictxt->length = status;
    wictxt->type = strdup(buf);
    if(!IDquiet) {
	if(status > 1024*1024)
	    // might be longer than long, and is on 64-bit windows
	    REprintf("Content type '%s' length %0.0f bytes (%0.1f MB)\n",
		     buf, (double) status, status/1024.0/1024.0);
	else if(status > 10240)
	    REprintf("Content type '%s' length %d bytes (%d KB)\n",
		     buf, (int) status, (int) (status/1024));
	else
	    REprintf("Content type '%s' length %d bytes\n", buf, (int) status);
	R_FlushConsole();
    }

    R_ProcessEvents();
    return (void *)wictxt;
}

static int in_R_HTTPRead(void *ctx, char *dest, int len)
{
    DWORD nread;

    InternetReadFile(((WIctxt)ctx)->session, dest, len, &nread);
#ifdef USE_WININET_ASYNC
    {
	DWORD t1 = GetTickCount();
	while(callback_status != INTERNET_STATUS_REQUEST_COMPLETE
	      && GetTickCount() < t1 + 1000*timeout) {
	    R_ProcessEvents();
	    Sleep(100);
	}
	if(callback_status != INTERNET_STATUS_REQUEST_COMPLETE) {
	    warning(_("Internet read timed out"));
	    nread = 0;
	}
    }
#endif
    R_ProcessEvents();
    return (int) nread;
}


static void in_R_HTTPClose(void *ctx)
{
    InternetCloseHandle(((WIctxt)ctx)->session);
    InternetCloseHandle(((WIctxt)ctx)->hand);
    if(((WIctxt)ctx)->type) free(((WIctxt)ctx)->type);
    free(ctx);
}

static void *in_R_FTPOpen(const char *url)
{
    WIctxt  wictxt;

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = NULL;

    wictxt->hand =
	InternetOpen("R", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL,
#ifdef USE_WININET_ASYNC
		     INTERNET_FLAG_ASYNC
#else
		     0
#endif
		     );
    if(!wictxt->hand) {
	free(wictxt);
	return NULL;
    }

#ifdef USE_WININET_ASYNC
    timeout = asInteger(GetOption1(install("timeout")));
    if(timeout == NA_INTEGER || timeout <= 0) timeout = 60;
    InternetSetStatusCallback(wictxt->hand,
			      (INTERNET_STATUS_CALLBACK) InternetCallback);
    if(!IDquiet) {
	REprintf("using Asynchronous WinInet calls, timeout %d secs\n",
		timeout);
	R_FlushConsole();
    }

    callback_status = 0;
    InternetOpenUrl(wictxt->hand, url,
		    NULL, 0,
	INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE,
		    17);
    {
	DWORD t1 = GetTickCount();
	while(callback_status != INTERNET_STATUS_REQUEST_COMPLETE
	      && GetTickCount() < t1 + 1000*timeout) {
	    R_ProcessEvents();
	    Sleep(100);
	}
	if(callback_status != INTERNET_STATUS_REQUEST_COMPLETE) {
	    InternetCloseHandle(wictxt->hand);
	    free(wictxt);
	    warning(_("InternetOpenUrl timed out"));
	    return NULL;
	}
    }

    wictxt->session = (HINTERNET) callback_res->dwResult;
#else
    if(!IDquiet) {
	REprintf("using Synchronous WinInet calls\n");
	R_FlushConsole();
    }
    wictxt->session = InternetOpenUrl(wictxt->hand, url,
				      NULL, 0,
	INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE,
				      0);
    if(!wictxt->session) {
	char buf[256];
	DWORD err1 = GetLastError(), err2, blen = 256;
	InternetCloseHandle(wictxt->hand);
	free(wictxt);
	if (err1 == ERROR_INTERNET_EXTENDED_ERROR) {
	    InternetGetLastResponseInfo(&err2, buf, &blen);
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return NULL;
	} else {
	    FormatMessage(
		FORMAT_MESSAGE_FROM_HMODULE,
		GetModuleHandle("wininet.dll"),
		err1,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		buf, 101, NULL);
	    warning(_("InternetOpenUrl failed: '%s'"), buf);
	    return NULL;
	}
    }
#endif /* USE_WININET_ASYNC */
    R_ProcessEvents();
    return (void *)wictxt;
}

static int in_R_FTPRead(void *ctx, char *dest, int len)
{
    return R_HTTPRead(ctx, dest, len);
}

static void in_R_FTPClose(void *ctx)
{
    R_HTTPClose(ctx);
}
#endif

#ifndef HAVE_INTERNET
static void *in_R_HTTPOpen(const char *url, const char *headers,
			   const int cacheOK)
{
    return NULL;
}

static int in_R_HTTPRead(void *ctx, char *dest, int len)
{
    return -1;
}

static void in_R_HTTPClose(void *ctx)
{
}

static void *in_R_FTPOpen(const char *url)
{
    return NULL;
}

static int in_R_FTPRead(void *ctx, char *dest, int len)
{
    return -1;
}

static void in_R_FTPClose(void *ctx)
{
}
#endif


#define MBUFSIZE 8192
void RxmlMessage(int level, const char *format, ...)
{
    int clevel;
    char buf[MBUFSIZE], *p;
    va_list(ap);

    clevel = asInteger(GetOption1(install("internet.info")));
    if(clevel == NA_INTEGER) clevel = 2;

    if(level < clevel) return;

    va_start(ap, format);
    vsnprintf(buf, MBUFSIZE, format, ap);
    buf[MBUFSIZE-1] = '\0';
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    warning(buf);
}

#include "sock.h"
#define STRICT_R_HEADERS
#include <R_ext/RS.h> /* for R_Calloc */
#include <R_ext/Rdynload.h>

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
#ifdef USE_WININET
R_init_internet2(DllInfo *info)
#else
R_init_internet(DllInfo *info)
#endif
{
    R_InternetRoutines *tmp;
    tmp = R_Calloc(1, R_InternetRoutines);

    tmp->download = in_do_download;
    tmp->newurl =  in_R_newurl;
    tmp->newsock = in_R_newsock;

    tmp->HTTPOpen = in_R_HTTPOpen;
    tmp->HTTPRead = in_R_HTTPRead;
    tmp->HTTPClose = in_R_HTTPClose;

    tmp->FTPOpen = in_R_FTPOpen;
    tmp->FTPRead = in_R_FTPRead;
    tmp->FTPClose = in_R_FTPClose;

    tmp->sockopen = in_Rsockopen;
    tmp->socklisten = in_Rsocklisten;
    tmp->sockconnect = in_Rsockconnect;
    tmp->sockclose = in_Rsockclose;
    tmp->sockread = in_Rsockread;
    tmp->sockwrite = in_Rsockwrite;

    tmp->sockselect = in_Rsockselect;

    tmp->HTTPDCreate = in_R_HTTPDCreate;
    tmp->HTTPDStop = in_R_HTTPDStop;

    tmp->curlVersion = in_do_curlVersion;
    tmp->curlGetHeaders = in_do_curlGetHeaders;
    tmp->curlDownload = in_do_curlDownload;
    tmp->newcurlurl =  in_newCurlUrl;

    R_setInternetRoutines(tmp);
}
