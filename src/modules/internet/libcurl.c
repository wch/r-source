/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2015 The R Core Team
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef Win32
# define R_USE_SIGNALS 1
#endif
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <errno.h>

#ifdef HAVE_LIBCURL
# include <curl/curl.h>
/*
  This need libcurl >= 7.28.0 (Oct 2012) for curl_multi_wait.
  There is a configure test but it is not used on Windows and system
  software can change.
*/
# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
# error libcurl 7.28.0 or later is required.
# endif
#endif

SEXP attribute_hidden in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP ans = PROTECT(allocVector(STRSXP, 1));
#ifdef HAVE_LIBCURL
    curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);
    SET_STRING_ELT(ans, 0, mkChar(d->version));
    SEXP sSSLVersion = install("ssl_version");
    setAttrib(ans, sSSLVersion,
	      mkString(d->ssl_version ? d->ssl_version : "none"));
    SEXP sLibSSHVersion = install("libssh_version");
    setAttrib(ans, sLibSSHVersion,
	      mkString(((d->age >= 3) && d->libssh_version) ? d->libssh_version : ""));
    const char * const *p;
    int n, i;
    for (p = d->protocols, n = 0; *p; p++, n++) ;
    SEXP protocols = PROTECT(allocVector(STRSXP, n));
    for (p = d->protocols, i = 0; i < n; i++, p++)
	SET_STRING_ELT(protocols, i, mkChar(*p));
    setAttrib(ans, install("protocols"), protocols);
    UNPROTECT(1);
#else
    SET_STRING_ELT(ans, 0, mkChar(""));
#endif
    UNPROTECT(1);
    return ans;
}

#ifdef HAVE_LIBCURL
static const char *http_errstr(const long status)
{
    const char *str;
    switch(status) {
    case 400: str = "Bad Request"; break;
    case 401: str = "Unauthorized"; break;
    case 402: str = "Payment Required"; break;
    case 403: str = "Forbidden"; break;
    case 404: str = "Not Found"; break;
    case 405: str = "Method Not Allowed"; break;
    case 406: str = "Not Acceptable"; break;
    case 407: str = "Proxy Authentication Required"; break;
    case 408: str = "Request Timeout"; break;
    case 409: str = "Conflict"; break;
    case 410: str = "Gone"; break;
    case 411: str = "Length Required"; break;
    case 412: str = "Precondition Failed"; break;
    case 413: str = "Request Entity Too Large"; break;
    case 414: str = "Request-URI Too Long"; break;
    case 415: str = "Unsupported Media Type"; break;
    case 416: str = "Requested Range Not Satisfiable"; break;
    case 417: str = "Expectation Failed"; break;
    case 500: str = "Internal Server Error"; break;
    case 501: str = "Not Implemented"; break;
    case 502: str = "Bad Gateway"; break;
    case 503: str = "Service Unavailable"; break;
    case 504: str = "Gateway Timeout"; break;
    default: str = "Unknown Error"; break;
    }
    return str;
}

static const char *ftp_errstr(const long status)
{
    const char *str;
    switch (status) {
    case 421: str = "Service not available, closing control connection"; break;
    case 425: str = "Cannot open data connection"; break;
    case 426: str = "Connection closed; transfer aborted"; break;
    case 430: str = "Invalid username or password"; break;
    case 434: str = "Requested host unavailable"; break;
    case 450: str = "Requested file action not taken"; break;
    case 451: str = "Requested action aborted; local error in processing"; break;
    case 452:
        str = "Requested action not taken; insufficient storage space in system";
        break;
    case 501: str = "Syntax error in parameters or arguments"; break;
    case 502: str = "Command not implemented"; break;
    case 503: str = "Bad sequence of commands"; break;
    case 504: str = "Command not implemented for that parameter"; break;
    case 530: str = "Not logged in"; break;
    case 532: str = "Need account for storing files"; break;
    case 550:
        str = "Requested action not taken; file unavailable";
        break;
    case 551: str = "Requested action aborted; page type unknown"; break;
    case 552:
        str = "Requested file action aborted; exceeded storage allocation";
        break;
    case 553: str = "Requested action not taken; file name not allowed"; break;
    default: str = "Unknown Error"; break;
    }
    return str;
}

/*
  Check curl_multi_info_read for errors, reporting as warnings

  Return: number of errors encountered
 */
static int curlMultiCheckerrs(CURLM *mhnd)
{
    int retval = 0;
    for(int n = 1; n > 0;) {
	CURLMsg *msg = curl_multi_info_read(mhnd, &n);
	if (msg && (msg->data.result != CURLE_OK)) {
	    const char *url, *strerr;
	    long status = 0;
	    curl_easy_getinfo(msg->easy_handle, CURLINFO_EFFECTIVE_URL, &url);
	    curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE,
			      &status);
	    if (status >= 400) {
		if (url && url[0] == 'h')
		    strerr = http_errstr(status);
		else
		    strerr = ftp_errstr(status);
		warning(_("URL '%s': status was '%d %s'"), url, status,
			strerr);
	    } else {
		strerr = curl_easy_strerror(msg->data.result);
		warning(_("URL '%s': status was '%s'"), url, strerr);
	    }
	    retval += 1;
	}
    }
    return retval;
}

static void curlCommon(CURL *hnd, int redirect, int verify)
{
    const char *capath = getenv("CURL_CA_BUNDLE");
    if (verify) {
	if (capath && capath[0])
	    curl_easy_setopt(hnd, CURLOPT_CAINFO, capath);
#ifdef Win32
	else
	    curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYPEER, 0L);
#endif
    } else {
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYHOST, 0L);
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYPEER, 0L);
    }
    // for consistency, but all that does is look up an option.
    SEXP sMakeUserAgent = install("makeUserAgent");
    SEXP agentFun = PROTECT(lang2(sMakeUserAgent, ScalarLogical(0)));
    SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
    SEXP sua = eval(agentFun, utilsNS);
    UNPROTECT(1); /* utilsNS */
    PROTECT(sua);
    if(TYPEOF(sua) != NILSXP)
	curl_easy_setopt(hnd, CURLOPT_USERAGENT, CHAR(STRING_ELT(sua, 0)));
    UNPROTECT(2);
    int timeout0 = asInteger(GetOption1(install("timeout")));
    long timeout = timeout0 = NA_INTEGER ? 0 : 1000L * timeout0;
    curl_easy_setopt(hnd, CURLOPT_CONNECTTIMEOUT_MS, timeout);
    curl_easy_setopt(hnd, CURLOPT_TIMEOUT_MS, timeout);
    if (redirect) {
	curl_easy_setopt(hnd, CURLOPT_FOLLOWLOCATION, 1L);
	curl_easy_setopt(hnd, CURLOPT_MAXREDIRS, 20L);
    }
    int verbosity = asInteger(GetOption1(install("internet.info")));
    if (verbosity < 2) curl_easy_setopt(hnd, CURLOPT_VERBOSE, 1L);

    // enable the cookie engine, keep cookies in memory
    curl_easy_setopt(hnd, CURLOPT_COOKIEFILE, "");
}

static char headers[500][2049]; // allow for terminator
static int used;

static size_t
rcvHeaders(void *buffer, size_t size, size_t nmemb, void *userp)
{
    char *d = (char*)buffer;
    size_t result = size * nmemb, res = result > 2048 ? 2048 : result;
    if (used >= 500) return result;
    strncpy(headers[used], d, res);
    // 'Do not assume that the header line is zero terminated!'
    headers[used][res] = '\0';
    used++;
    return result;
}

static size_t
rcvBody(void *buffer, size_t size, size_t nmemb, void *userp)
{
    // needed to discard spurious ftp 'body' otherwise written to stdout
    return size * nmemb;
}
#endif


SEXP attribute_hidden
in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_LIBCURL
    error(_("curlGetHeaders is not supported on this platform"));
    return R_NilValue;
#else
    if (!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
       error("invalid %s argument", "url");
    const char *url = translateChar(STRING_ELT(CAR(args), 0));
    used = 0;
    int redirect = asLogical(CADR(args));
    if (redirect == NA_LOGICAL)
	error(_("invalid %s argument"), "redirect");
    int verify = asLogical(CADDR(args));
    if (verify == NA_LOGICAL)
	error(_("invalid %s argument"), "verify");

    CURL *hnd = curl_easy_init();
    curl_easy_setopt(hnd, CURLOPT_URL, url);
    curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(hnd, CURLOPT_NOBODY, 1L);
    curl_easy_setopt(hnd, CURLOPT_HEADERFUNCTION, &rcvHeaders);
    curl_easy_setopt(hnd, CURLOPT_WRITEHEADER, &headers);
    /* libcurl (at least 7.40.0) does not respect CURLOPT_NOBODY
       for some ftp header info (Content-Length and Accept-ranges). */
    curl_easy_setopt(hnd, CURLOPT_WRITEFUNCTION, &rcvBody);
    curlCommon(hnd, redirect, verify);

    char errbuf[CURL_ERROR_SIZE];
    curl_easy_setopt(hnd, CURLOPT_ERRORBUFFER, errbuf);
    CURLcode ret = curl_easy_perform(hnd);
    if (ret != CURLE_OK)
	error(_("libcurl error code %d\n\t%s\n"), ret, errbuf);
    long http_code = 0;
    curl_easy_getinfo (hnd, CURLINFO_RESPONSE_CODE, &http_code);
    curl_easy_cleanup(hnd);

    SEXP ans = PROTECT(allocVector(STRSXP, used));
    for (int i = 0; i < used; i++)
	SET_STRING_ELT(ans, i, mkChar(headers[i]));
    SEXP sStatus = install("status");
    setAttrib(ans, sStatus, ScalarInteger((int) http_code));
    UNPROTECT(1);
    return ans;
#endif
}

#ifdef HAVE_LIBCURL
static double total;

static int ndashes;
static void putdashes(int *pold, int new)
{
    for (int i = *pold; i < new; i++)  REprintf("=");
    if (R_Consolefile) fflush(R_Consolefile);
    *pold = new;
}

#ifdef Win32
// ------- Windows progress bar -----------
#include <ga.h>

/* We could share this window with internet.c, then re-positioning
   would apply to both */
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

static
int progress(void *clientp, double dltotal, double dlnow,
	     double ultotal, double ulnow)
{
    static int factor = 1;
    // we only use downloads.  dltotal may be zero.
    if (dltotal > 0.) {
	if (total == 0.) {
	    total = dltotal;
	    char *type = NULL;
	    CURL *hnd = (CURL *) clientp;
	    curl_easy_getinfo(hnd, CURLINFO_CONTENT_TYPE, &type);
	    if (total > 1024.0*1024.0)
		// might be longer than long, and is on 64-bit windows
		REprintf(" length %0.0f bytes (%0.1f MB)\n",
			 total, total/1024.0/1024.0);
	    else if (total > 10240)
		REprintf("Content length %d bytes (%d KB)\n",
			 (int)total, (int)(total/1024));
	    else
		REprintf("Content length %d bytes\n", (int)total);
	    R_FlushConsole();
	    if(R_Interactive) {
		if (total > 1e9) factor = total/1e6; else factor = 1;
		setprogressbarrange(pbar.pb, 0, total/factor);
		show(pbar.wprog);
	    }
	}
	if (R_Interactive) {
	    setprogressbar(pbar.pb, dlnow/factor);
	    if (total > 0) {
		static char pbuf[30];
		int pc = 0.499 + 100.0*dlnow/total;
		if (pc > pbar.pc) {
		    snprintf(pbuf, 30, "%d%% downloaded", pc);
		    settext(pbar.wprog, pbuf);
		    pbar.pc = pc;
		}
	    }
	} else putdashes(&ndashes, (int)(50*dlnow/total));
    }
    R_ProcessEvents();
    return 0;
}

#else
// ------- Unix-alike progress bar -----------

static
int progress(void *clientp, double dltotal, double dlnow,
	     double ultotal, double ulnow)
{
    // we only use downloads.  dltotal may be zero.
    if (dltotal > 0.) {
	if (total == 0.) {
	    total = dltotal;
	    char *type = NULL;
	    CURL *hnd = (CURL *) clientp;
	    curl_easy_getinfo(hnd, CURLINFO_CONTENT_TYPE, &type);
	    REprintf("Content type '%s'", type ? type : "unknown");
	    if (total > 1024.0*1024.0)
		// might be longer than long, and is on 64-bit windows
		REprintf(" length %0.0f bytes (%0.1f MB)\n",
			 total, total/1024.0/1024.0);
	    else if (total > 10240)
		REprintf(" length %d bytes (%d KB)\n",
			 (int)total, (int)(total/1024));
	    else
		REprintf(" length %d bytes\n", (int)total);
	    if (R_Consolefile) fflush(R_Consolefile);
	}
	putdashes(&ndashes, (int)(50*dlnow/total));
    }
    return 0;
}
#endif

extern void Rsleep(double timeint);
#endif

/* download(url, destfile, quiet, mode, headers, cacheOK) */

SEXP attribute_hidden
in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_LIBCURL
    error(_("download.file(method = \"libcurl\") is not supported on this platform"));
    return R_NilValue;
#else
    SEXP scmd, sfile, smode;
    const char *url, *file, *mode;
    int quiet, cacheOK;
    struct curl_slist *slist1 = NULL;

    scmd = CAR(args); args = CDR(args);
    if (!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    int nurls = length(scmd);
    sfile = CAR(args); args = CDR(args);
    if (!isString(sfile) || length(sfile) < 1)
	error(_("invalid '%s' argument"), "destfile");
    if (length(sfile) != length(scmd))
	error(_("lengths of 'url' and 'destfile' must match"));
    quiet = asLogical(CAR(args)); args = CDR(args);
    if (quiet == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quiet");
    smode =  CAR(args); args = CDR(args);
    if (!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = CHAR(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args));
    if (cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");

    /* This comes mainly from curl --libcurl on the call used by
       download.file(method = "curl").
       Also http://curl.haxx.se/libcurl/c/multi-single.html.
    */

    if (!cacheOK) {
	/* This _is_ the right way to do this: see §14.9 of
	   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html */
	slist1 = curl_slist_append(slist1, "Pragma: no-cache");
    }

    CURLM *mhnd = curl_multi_init();
    int still_running, repeats = 0, n_err = 0;
    CURL **hnd[nurls];
    FILE *out[nurls];

    for(int i = 0; i < nurls; i++) {
	out[i] = NULL;
	url = CHAR(STRING_ELT(scmd, i));
	hnd[i] = curl_easy_init();
	curl_easy_setopt(hnd[i], CURLOPT_URL, url);
	curl_easy_setopt(hnd[i], CURLOPT_FAILONERROR, 1L);
	/* Users will normally expect to follow redirections, although
	   that is not the default in either curl or libcurl. */
	curlCommon(hnd[i], 1, 1);
	curl_easy_setopt(hnd[i], CURLOPT_TCP_KEEPALIVE, 1L);
	if (!cacheOK)
	    curl_easy_setopt(hnd[i], CURLOPT_HTTPHEADER, slist1);

	/* check that connection can be opened... */
	curl_easy_setopt(hnd[i], CURLOPT_NOPROGRESS, 1L);
	curl_easy_setopt(hnd[i], CURLOPT_NOBODY, 1L);
	curl_easy_setopt(hnd[i], CURLOPT_HEADERFUNCTION, &rcvHeaders);
	curl_easy_setopt(hnd[i], CURLOPT_WRITEHEADER, &headers);
	/* libcurl (at least 7.40.0) does not respect CURLOPT_NOBODY
	   for some ftp header info (Content-Length and Accept-ranges). */
	curl_easy_setopt(hnd[i], CURLOPT_WRITEFUNCTION, &rcvBody);
	CURLcode ret = curl_easy_perform(hnd[i]);
	curl_multi_add_handle(mhnd, hnd[i]);
	if (ret != CURLE_OK) {
	    n_err += 1;
	    /* warning signalled via curlMultiCheckerrs */
	    continue;
	}
	/* ...and that destfile can be written */
	file = translateChar(STRING_ELT(sfile, i));
	out[i] = R_fopen(R_ExpandFileName(file), mode);
	if (!out[i]) {
	    n_err += 1;
	    warning(_("URL %s: cannot open destfile '%s', reason '%s'"),
		    url, file, strerror(errno));
	    continue;
	}
	curl_easy_setopt(hnd[i], CURLOPT_WRITEFUNCTION, NULL); /* necessary? */
	curl_easy_setopt(hnd[i], CURLOPT_WRITEDATA, out[i]);
	curl_easy_setopt(hnd[i], CURLOPT_NOBODY, 0L);
	curl_easy_setopt(hnd[i], CURLOPT_HEADER, 0L);

	total = 0.;
	if (!quiet && nurls <= 1) {
	    // It would in principle be possible to have
	    // multiple progress bars on Windows.
	    curl_easy_setopt(hnd[i], CURLOPT_NOPROGRESS, 0L);
	    ndashes = 0;
#ifdef Win32
	    if (R_Interactive) {
		if (!pbar.wprog) {
		    pbar.wprog = newwindow(_("Download progress"),
					   rect(0, 0, 540, 100),
					   Titlebar | Centered);
		    setbackground(pbar.wprog, dialog_bg());
		    pbar.l_url = newlabel(" ", rect(10, 15, 520, 25), 
					  AlignCenter);
		    pbar.pb = newprogressbar(rect(20, 50, 500, 20),
					     0, 1024, 1024, 1);
		    pbar.pc = 0;
		}
	    
		settext(pbar.l_url, url);
		setprogressbar(pbar.pb, 0);
		settext(pbar.wprog, "Download progress");
		show(pbar.wprog);
		begincontext(&(pbar.cntxt), CTXT_CCODE, R_NilValue, R_NilValue,
			     R_NilValue, R_NilValue, R_NilValue);
		pbar.cntxt.cend = &doneprogressbar;
		pbar.cntxt.cenddata = &pbar;
	    }
#endif
	    // For libcurl >= 7.32.0 use CURLOPT_XFERINFOFUNCTION
	    curl_easy_setopt(hnd[i], CURLOPT_PROGRESSFUNCTION, progress);
	    curl_easy_setopt(hnd[i], CURLOPT_PROGRESSDATA, hnd[i]);
	}

	/* This would allow the negotiation of compressed HTTP transfers,
	   but it is not clear it is always a good idea.
	   curl_easy_setopt(hnd[i], CURLOPT_ACCEPT_ENCODING, "gzip, deflate");
	*/

	if (!quiet) REprintf(_("trying URL '%s'\n"), url);
    }

    R_Busy(1);
    //  curl_multi_wait needs curl >= 7.28.0 .
    curl_multi_perform(mhnd, &still_running);
    do {
	int numfds;
	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds);
	if (mc != CURLM_OK)  // internal, do not translate
	    error("curl_multi_wait() failed, code %d", mc);
	if (!numfds) {
	    /* 'numfds' being zero means either a timeout or no file
	       descriptors to wait for. Try timeout on first
	       occurrence, then assume no file descriptors to wait for
	       means 'sleep for 100 milliseconds'.
	    */
	    if (repeats++ > 0) Rsleep(0.1); // do not block R process
	} else repeats = 0;
	R_ProcessEvents();
	curl_multi_perform(mhnd, &still_running);
    } while(still_running);
    R_Busy(0);
#ifdef Win32
    if (R_Interactive && !quiet) {
	endcontext(&(pbar.cntxt));
	doneprogressbar(&pbar);
    } else if (total > 0.) {
	REprintf("\n");
	R_FlushConsole();
    }
#else
    if (total > 0.) REprintf("\n");
    if (R_Consolefile) fflush(R_Consolefile);
#endif
    if (nurls == 1) {
	double cl, dl;
	curl_easy_getinfo(hnd[0], CURLINFO_SIZE_DOWNLOAD, &dl);
	if (!quiet) {
	    if (dl > 1024*1024)
		REprintf("downloaded %0.1f MB\n\n", (double)dl/1024/1024);
	    else if (dl > 10240)
		REprintf("downloaded %d KB\n\n", (int) dl/1024);
	    else
		REprintf("downloaded %d bytes\n\n", (int) dl);
	}
	curl_easy_getinfo(hnd[0], CURLINFO_CONTENT_LENGTH_DOWNLOAD, &cl);
	if (cl >= 0 && dl != cl)
	    warning(_("downloaded length %0.f != reported length %0.f"), dl, cl);
    }

    n_err += curlMultiCheckerrs(mhnd);

    for (int i = 0; i < nurls; i++) {
	if (out[i])
            fclose(out[i]);
	curl_multi_remove_handle(mhnd, hnd[i]);
	curl_easy_cleanup(hnd[i]);
    }
    curl_multi_cleanup(mhnd);
    if (!cacheOK) curl_slist_free_all(slist1);

    if (n_err != 0)
	error(_("cannot download all files"));

    return ScalarInteger(0);
#endif
}

/* -------------------------- connections part ------------------------*/

/* Unfortunately the libcurl interface is not well adapted to reading
   data in user-requested chunks.

   But it does read in up to CURL_MAX_WRITE_SIZE chunks, which is 16K.
   So we implement a buffer which holds two chunks, and when what we
   have is not enough we move down what it left and fetch another
   chunk above it.  For safety, the buffer is expandable but this
   should not be exercised.

   An alternative design would be for consumeData to return what is
   available and reset current.  Then rcvData would only be called on
   a completely empty buffer.
 */

#include <Rconnections.h>

#define R_MIN(a, b) ((a) < (b) ? (a) : (b))

#ifdef HAVE_LIBCURL
typedef struct Curlconn {
    char *buf, *current; // base of buffer, last read address
    size_t bufsize, filled;  // buffer size, amount which has been filled
    Rboolean available; // to be read out
    int sr; // 'still running' count
    CURLM *mh; CURL *hnd;
} *RCurlconn;

static size_t rcvData(void *ptr, size_t size, size_t nitems, void *ctx)
{
    RCurlconn ctxt = (RCurlconn) ctx;

    /* move down any unused data: can overlap */
    if (ctxt->filled) memmove(ctxt->buf, ctxt->current, ctxt->filled);
    ctxt->current = ctxt->buf;

    size_t add = size * nitems;
    if (add) {
	/* Allocate more space if required: unlikely.
	   Do so as an integer multiple of the current size.
	 */
	if (ctxt->filled + add > ctxt->bufsize) {
	    int mult = (int) ceil((double)(ctxt->filled + add)/ctxt->bufsize);
	    size_t newbufsize = mult * ctxt->bufsize;
	    void *newbuf = realloc(ctxt->buf, newbufsize);
	    if (!newbuf) error("Failure in re-allocation in rcvData");
	    ctxt->buf = newbuf; ctxt->bufsize = newbufsize;
	}

	memcpy(ctxt->buf + ctxt->filled, ptr, add);
	ctxt->filled += add;
	ctxt->available = TRUE;
    }
    return add;
}

static size_t consumeData(void *ptr, size_t max, RCurlconn ctxt)
{
    size_t size = R_MIN(ctxt->filled, max);  // guaranteed > 0
    memcpy(ptr, ctxt->current, size);
    ctxt->current += size; ctxt->filled -= size;
    return size;
}

/*
  return: number of errors encountered
 */
static int fetchData(RCurlconn ctxt)
{
    int repeats = 0;
    CURLM *mhnd = ctxt->mh;
    
    do {
	int numfds;
	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds);
	if (mc != CURLM_OK)
	    error("curl_multi_wait() failed, code %d", mc);
	if (!numfds) {
	    if (repeats++ > 0) Rsleep(0.1);
	} else repeats = 0;
	curl_multi_perform(mhnd, &ctxt->sr);
	if (ctxt->available) break;
	R_ProcessEvents();
    } while(ctxt->sr);

    return curlMultiCheckerrs(mhnd);
}

static void Curl_close(Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->private);

    curl_multi_remove_handle(ctxt->mh, ctxt->hnd);
    curl_easy_cleanup(ctxt->hnd);
    curl_multi_cleanup(ctxt->mh);
    con->isopen = FALSE;
}

static void Curl_destroy(Rconnection con)
{
    RCurlconn ctxt;

    if (NULL == con)
        return;
    ctxt = (RCurlconn)(con->private);

    if (NULL == ctxt)
        return;

    free(ctxt->buf);
    free(ctxt);
}

static size_t Curl_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->private);
    size_t nbytes = size*nitems;
    char *p = (char *) ptr;
    size_t total = consumeData(ptr, nbytes, ctxt);
    int n_err = 0;
    while((total < nbytes) && ctxt->sr) {
	n_err += fetchData(ctxt);
	total += consumeData(p + total, (nbytes - total), ctxt);
    }
    if (n_err != 0) {
	Curl_close(con);
	error(_("cannot read from connection"), n_err);
    }
    return total/size;
}

static Rboolean Curl_open(Rconnection con)
{
    char *url = con->description;
    RCurlconn ctxt = (RCurlconn)(con->private);

    if (con->mode[0] != 'r') {
	REprintf("can only open URLs for reading");
	return FALSE;
    }

    ctxt->hnd = curl_easy_init();
    curl_easy_setopt(ctxt->hnd, CURLOPT_URL, url);
    curl_easy_setopt(ctxt->hnd, CURLOPT_FAILONERROR, 1L);
    curlCommon(ctxt->hnd, 1, 1);
    curl_easy_setopt(ctxt->hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(ctxt->hnd, CURLOPT_TCP_KEEPALIVE, 1L);

    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEFUNCTION, rcvData);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEDATA, ctxt);
    ctxt->mh = curl_multi_init();
    curl_multi_add_handle(ctxt->mh, ctxt->hnd);

    ctxt->current = ctxt->buf; ctxt->filled = 0; ctxt->available = FALSE;

    // Establish the connection: not clear if we should do this now.
    ctxt->sr = 1;
    int n_err = 0;
    while(ctxt->sr && !ctxt->available)
	n_err += fetchData(ctxt);
    if (n_err != 0) {
	Curl_close(con);
	error(_("cannot open connection"), n_err);
    }

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if (strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static int Curl_fgetc_internal(Rconnection con)
{
    unsigned char c;
    size_t n = Curl_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}
#endif


// 'type' is unused.
Rconnection 
in_newCurlUrl(const char *description, const char * const mode, int type)
{
#ifdef HAVE_LIBCURL
    Rconnection new = (Rconnection) malloc(sizeof(struct Rconn));
    if (!new) error(_("allocation of url connection failed"));
    new->class = (char *) malloc(strlen("url-libcurl") + 1);
    if (!new->class) {
	free(new);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "url-libcurl");
    new->description = (char *) malloc(strlen(description) + 1);
    if (!new->description) {
	free(new->class); free(new);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->canwrite = FALSE;
    new->open = &Curl_open;
    new->close = &Curl_close;
    new->destroy = &Curl_destroy;
    new->fgetc_internal = &Curl_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->read = &Curl_read;
    new->private = (void *) malloc(sizeof(struct Curlconn));
    if (!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of url connection failed"));
        /* for Solaris 12.5 */ new = NULL;
    }
    RCurlconn ctxt = (RCurlconn) new->private;
    ctxt->bufsize = 2 * CURL_MAX_WRITE_SIZE;
    ctxt->buf = malloc(ctxt->bufsize);
    if (!ctxt->buf) {
	free(new->description); free(new->class); free(new->private);
	free(new);
	error(_("allocation of url connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    return new;
#else
    error(_("url(method = \"libcurl\") is not supported on this platform"));
    return (Rconnection)0; /* -Wall */
#endif
}
