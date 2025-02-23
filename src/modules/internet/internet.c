/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2024   The R Core Team.
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

/* <UTF8> the only interpretation of char is ASCII */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// for contexts
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <errno.h>
#include <R_ext/Print.h>

// formerly in R-ftp-http.h
#include <stdint.h>
typedef int_fast64_t DLsize_t; // used for download lengths and sizes


SEXP in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho);
SEXP in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho);
Rconnection
in_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type);

#ifdef Win32
static void *in_R_HTTPOpen2(const char *url, const char *agent, const char *headers, int cacheOK);
static int   in_R_HTTPRead2(void *ctx, char *dest, int len);
static void  in_R_HTTPClose2(void *ctx);
static void *in_R_FTPOpen2(const char *url);
#endif

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

static Rboolean IDquiet = TRUE;

/* 
   Support for url().
   As from R 4.2.0, this only provides method = "wininet" on Windows.
   file:// URLs are handled in connections.c and method = "libcurl" 
   in R_newCurlUrl.

   method = "internal" is defunct for http:// and ftp:// URLs.
 */

#ifdef Win32
static Rboolean url_open2(Rconnection con)
{
    void *ctxt;
    char *url = con->description;
    UrlScheme type = ((Rurlconn)(con->private))->type;
    int mlen;

    if(con->mode[0] != 'r') {
	REprintf("can only open URLs for reading");
	return FALSE;
    }

    switch(type) {
    case HTTPSsh:
    case HTTPsh:
    {
	warning(_("the 'wininet' method of url() is deprecated for http:// and https:// URLs"));
	SEXP sagent, agentFun;
	const char *agent;
	SEXP s_makeUserAgent = install("makeUserAgent");
	struct urlconn * uc = con->private;
	agentFun = PROTECT(lang2(s_makeUserAgent, ScalarLogical(0)));
	sagent = PROTECT(eval(agentFun, R_FindNamespace(mkString("utils"))));
	if(TYPEOF(sagent) == NILSXP)
	    agent = NULL;
	else
	    agent = CHAR(STRING_ELT(sagent, 0));
	ctxt = in_R_HTTPOpen2(url, agent, uc->headers, 0);
	UNPROTECT(2);
	if(ctxt == NULL) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	    return FALSE;
	}
	((Rurlconn)(con->private))->ctxt = ctxt;
    }
	break;
    case FTPsh:
	warning(_("the 'wininet' method of url() is deprecated for ftp:// URLs"));
	ctxt = in_R_FTPOpen2(url);
	if(ctxt == NULL) {
	  /* if we call error() we get a connection leak*/
	  /* so do_url has to raise the error*/
	    return FALSE;
	}
	((Rurlconn)(con->private))->ctxt = ctxt;
	break;

    default:
	warning(_("scheme not supported in URL '%s'"), url);
	return FALSE;
    }

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static void url_close2(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	in_R_HTTPClose2(((Rurlconn)(con->private))->ctxt);
	break;
    default:
	break;
    }
    con->isopen = FALSE;
}

static int url_fgetc_internal2(Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    void * ctxt = ((Rurlconn)(con->private))->ctxt;
    unsigned char c;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	n = in_R_HTTPRead2(ctxt, (char *)&c, 1);
	break;
    default:
	break;
    }
    return (n == 1) ? c : R_EOF;
}

static size_t url_read2(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    UrlScheme type = ((Rurlconn)(con->private))->type;
    void * ctxt = ((Rurlconn)(con->private))->ctxt;
    size_t n = 0; /* -Wall */

    switch(type) {
    case HTTPsh:
    case HTTPSsh:
    case FTPsh:
	n = in_R_HTTPRead2(ctxt, ptr, (int)(size*nitems));
	break;
    default:
	break;
    }
    return n/size;
}
#endif

#ifdef Win32
static Rconnection
in_R_newurl(const char *description, const char * const mode, SEXP headers, int type)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of url connection failed"));
    new->class = (char *) malloc(strlen("url-wininet") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->canwrite = FALSE;
    if (type) {
	new->open = &url_open2;
	new->read = &url_read2;
	new->close = &url_close2;
	new->fgetc_internal = &url_fgetc_internal2;
	strcpy(new->class, "url-wininet");
   } else {
	free(new->description); free(new->class); free(new);
	error(_("the 'internal' method of url() is defunct for http:// and ftp:// URLs"));
	/* for Solaris 12.5 */ new = NULL;
    }
    new->fgetc = &dummy_fgetc;
    struct urlconn *uc = new->private = (void *) malloc(sizeof(struct urlconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of url connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    uc->headers = NULL;
    if(!isNull(headers)) {
	uc->headers = strdup(CHAR(STRING_ELT(headers, 0)));
	if(!uc->headers) {
	    free(new->description); free(new->class); free(new->private); free(new);
	    error(_("allocation of url connection failed"));
	    /* for Solaris 12.5 */ new = NULL;
	}
    }

    IDquiet = TRUE;
    return new;
}
#endif

/* ------------------- download.file() internals  --------------------- */

/* As from R 4.2.0 this only supports

   file:// URLs
   http:// and https:// URLs for method = "wininet" on Windows

*/

#ifdef Win32
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
    SEXP scmd, sfile, smode, sheaders;
    const char *url, *file, *mode;
    int quiet, status = 0, cacheOK, meth = 0;
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
    IDquiet = quiet = asRbool(CAR(args), R_NilValue); args = CDR(args);
    if(quiet == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quiet");
    smode =  CAR(args); args = CDR(args);
    if(!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = CHAR(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args)); args = CDR(args);
    if(cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");
    Rboolean file_URL = (strncmp(url, "file://", 7) == 0);
    sheaders = CAR(args);
    if(TYPEOF(sheaders) != NILSXP && !isString(sheaders))
        error(_("invalid '%s' argument"), "headers");
#ifdef Win32
    meth = asLogical(CADR(args));
    if(meth == NA_LOGICAL)
	error(_("invalid '%s' argument"), "method");
    if (!file_URL && R_Interactive && !quiet && !pbar.wprog) {
	pbar.wprog = newwindow(_("Download progress"), rect(0, 0, 540, 100),
			       Titlebar | Centered);
	setbackground(pbar.wprog, dialog_bg());
	pbar.l_url = newlabel(" ", rect(10, 15, 520, 25), AlignCenter);
	pbar.pb = newprogressbar(rect(20, 50, 500, 20), 0, 1024, 1024, 1);
	pbar.pc = 0;
    }
#endif
    if(file_URL) {
// ---------  file:// code ---------------
	FILE *in, *out;
	static char buf[CPBUFSIZE];
	size_t n;
	int nh = 7, mlen;
#ifdef Win32
	/* on Windows we have file:///d:/path/to
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif

	/* Use binary transfers? */
	mlen = (int) strlen(mode);
	in = R_fopen(R_ExpandFileName(url+nh),
	             (mlen >= 2 && mode[mlen - 1] == 'b') ? "rb" : "r");
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
// ---------  end of file:// code ---------------

    } else if(!meth && strncmp(url, "http://", 7) == 0) {
	error(_("the 'internal' method for http:// URLs is defunct"));
#ifdef Win32
// ---------  wininet only code ---------------
    } else if (meth &&
	       (strncmp(url, "http://", 7) == 0
		|| (strncmp(url, "https://", 8) == 0))
	) {

	warning(_("the 'wininet' method is deprecated for http:// and https:// URLs"));
	FILE *out;
	void *ctxt;
	DLsize_t len, total, guess, nbytes = 0;
	char buf[IBUFSIZE];
	int ndashes = 0;
	DLsize_t ndots = 0;
	int factor = 1;

	out = R_fopen(R_ExpandFileName(file), mode);
	if(!out) {
	    error(_("cannot open destfile '%s', reason '%s'"),
		  file, strerror(errno));
	}

	R_Busy(1);
	if(!quiet) REprintf(_("trying URL '%s'\n"), url);
	SEXP agentFun, sagent;
	R_FlushConsole();
	agentFun = PROTECT(lang2(install("makeUserAgent"), ScalarLogical(0)));
	SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
	sagent = eval(agentFun, utilsNS);
	UNPROTECT(1); /* utilsNS */
	PROTECT(sagent);
	const char *cagent = (TYPEOF(sagent) == NILSXP) ?
	    NULL : CHAR(STRING_ELT(sagent, 0));
	/* TODO: flatten headers */
	const char *cheaders = (TYPEOF(sheaders) == NILSXP) ?
	    NULL : CHAR(STRING_ELT(sheaders, 0));
	ctxt = in_R_HTTPOpen2(url, cagent, cheaders, cacheOK);
	UNPROTECT(2);
	if(ctxt == NULL) status = 1;
	else {
//	    if(!quiet) REprintf(_("opened URL\n"), url);
	    guess = total = ((inetconn *)ctxt)->length;
	    if(R_Interactive) {
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
	    }
	    while ((len = in_R_HTTPRead2(ctxt, buf, sizeof(buf))) > 0) {
		size_t res = fwrite(buf, 1, len, out);
		if(res != len) error(_("write failed"));
		nbytes += len;
		if(!quiet) {
		    if(R_Interactive) {
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
		    } else {
			if(guess <= 0) putdots(&ndots, nbytes/1024);
			else putdashes(&ndashes, (int)(50*nbytes/guess));
		    }
		}
	    }
	    in_R_HTTPClose2(ctxt);
	    if(!quiet) {
		if(!R_Interactive) REprintf("\n");
		if(nbytes > 1024*1024)
		    REprintf("downloaded %0.1f MB\n\n",
			     (double)nbytes/1024/1024);
		else if(nbytes > 10240)
		    REprintf("downloaded %d KB\n\n", (int) nbytes/1024);
		else
		    REprintf("downloaded %d bytes\n\n", (int) nbytes);
	    }
	    R_FlushConsole();
	    if(R_Interactive && !quiet) {
		endcontext(&(pbar.cntxt));
		doneprogressbar(&pbar);
	    }
	    if (total > 0 && total != nbytes)
		warning(_("downloaded length %0.f != reported length %0.f"),
			(double)nbytes, (double)total);
	}
	fclose(out);
	if (status == 1 && strchr(mode, 'w')) unlink(R_ExpandFileName(file));
	R_Busy(0);
	if (status == 1) error(_("cannot open URL '%s'"), url);
// ---------  end of wininet only code ---------------
#endif
    } else if (strncmp(url, "ftp://", 6) == 0) {
	if(meth)
	    error(_("the 'wininet' method for ftp:// URLs is defunct"));
	else
	    error(_("the 'internal' method for ftp:// URLs is defunct"));
   } else
	error(_("scheme not supported in URL '%s'"), url);

    return ScalarInteger(status);
}

#ifdef Win32

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wininet.h>
typedef struct wictxt {
    DLsize_t length;
    char * type;
    HINTERNET hand;
    HINTERNET session;
} wIctxt, *WIctxt;

static void *in_R_HTTPOpen2(const char *url, const char *agent, const char *headers,
			    int cacheOK)
{
    WIctxt  wictxt;
    DWORD status = 0, len = 0, d1 = 4, d2 = 0, d3 = 100;
    char buf[101], *p;

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = NULL;
    wictxt->hand =
	InternetOpen(agent, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    if(!wictxt->hand) {
	free(wictxt);
	/* error("cannot open Internet connection"); */
	return NULL;
    }

    // use keep-alive semantics, do not use local WinINet cache.
    DWORD flags = INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE |
                  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS | INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;
    if(!cacheOK) flags |= INTERNET_FLAG_PRAGMA_NOCACHE;
    wictxt->session = InternetOpenUrl(wictxt->hand, url, headers, headers ? -1 : 0, flags, 0);
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
	warning(_("cannot open URL '%s': %s status was '%lu %s'"),
		url, "HTTP", (unsigned long)status, buf);
	return NULL;
    }

    HttpQueryInfo(wictxt->session,
		  HTTP_QUERY_CONTENT_TYPE, &buf, &d3, &d2);
    d2 = 0;
    // NB: this can only retrieve in a DWORD, so up to 2GB or 4GB?
    if (HttpQueryInfo(wictxt->session,
		      HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER,
		      &len, &d1, &d2))
	wictxt->length = len;
    wictxt->type = Rstrdup(buf);
    if(!IDquiet) {
	REprintf("Content type '%s'", buf);
	if(len > 1024*1024)
	    REprintf(" length %0.0f bytes (%0.1f MB)\n", (double)len,
		     len/1024.0/1024.0);
	else if(len > 10240)
	    REprintf(" length %d bytes (%d KB)\n",
		     (int)len, (int)(len/1024));
	else if(wictxt->length >= 0) /* signed; len is not */
	    REprintf(" length %d bytes\n", (int)len);
	else REprintf(" length unknown\n");
	R_FlushConsole();
    }

    R_ProcessEvents();
    return (void *)wictxt;
}

static int in_R_HTTPRead2(void *ctx, char *dest, int len)
{
    DWORD nread;

    InternetReadFile(((WIctxt)ctx)->session, dest, len, &nread);
    R_ProcessEvents();
    return (int) nread;
}


static void in_R_HTTPClose2(void *ctx)
{
    InternetCloseHandle(((WIctxt)ctx)->session);
    InternetCloseHandle(((WIctxt)ctx)->hand);
    if(((WIctxt)ctx)->type) free(((WIctxt)ctx)->type);
    free(ctx);
}

static void *in_R_FTPOpen2(const char *url)
{
    WIctxt  wictxt;

    wictxt = (WIctxt) malloc(sizeof(wIctxt));
    wictxt->length = -1;
    wictxt->type = NULL;

    wictxt->hand =
	InternetOpen("R", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
    if(!wictxt->hand) {
	free(wictxt);
	return NULL;
    }

    DWORD flag = INTERNET_FLAG_KEEP_CONNECTION | INTERNET_FLAG_NO_CACHE_WRITE;
    wictxt->session = InternetOpenUrl(wictxt->hand, url, NULL, 0,
    	flag | INTERNET_FLAG_PASSIVE, 0);
    if(!wictxt->session)
    	wictxt->session = InternetOpenUrl(wictxt->hand, url, NULL, 0, flag, 0);
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
    R_ProcessEvents();
    return (void *)wictxt;
}
#endif // Win32

#include "sock.h"
#include <R_ext/RS.h> /* for R_Calloc */
#include <R_ext/Rdynload.h>

void
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_internet(DllInfo *info)
{
    R_InternetRoutines *tmp;
    tmp = R_Calloc(1, R_InternetRoutines);

    tmp->download = in_do_download;
#ifdef Win32
    tmp->newurl =  in_R_newurl;
#endif
    tmp->newsock = in_R_newsock;
    tmp->newservsock = in_R_newservsock;

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
