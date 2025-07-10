/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2015-2025 The R Core Team
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

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
// for unlink
# include <unistd.h>
#endif

#ifdef HAVE_LIBCURL
# include <curl/curl.h>
/*
  This needed libcurl >= 7.28.0 (Oct 2012) for curl_multi_wait.
  Substitute code is provided for a Unix-alike only.

  There is a configure test but it is not used on Windows and system
  software can change.
*/
# ifdef Win32
#  if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
#  error libcurl 7.28.0 or later is required.
#  endif
# else
#  if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 22)
#  error libcurl 7.22.0 or later is required.
#  endif
# endif
extern void Rsleep(double timeint);
#endif

static int current_timeout = 0;
static double current_time = 0;

# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)

// curl/curl.h includes <sys/select.h> and headers it requires.

#define curl_multi_wait R_curl_multi_wait


static CURLMcode
R_curl_multi_wait(CURLM *multi_handle,
		  /* IGNORED */ void *unused,
		  /* IGNORED */ unsigned int extra,
		  int timeout_ms, int *ret)
{
    fd_set fdread;
    fd_set fdwrite;
    fd_set fdexcep;
    FD_ZERO(&fdread);
    FD_ZERO(&fdwrite);
    FD_ZERO(&fdexcep);

    struct timeval timeout;

    timeout.tv_sec = timeout_ms / 1000;
    timeout.tv_usec = (timeout_ms % 1000) * 1000;

    int maxfd = -1;
    CURLMcode
	mc = curl_multi_fdset(multi_handle, &fdread, &fdwrite, &fdexcep, &maxfd);
    if (maxfd == -1) {
	*ret = 0;
	Rsleep(0.1);
    } else
	/* file descriptors should be checked against FD_SETSIZE by caller */
	*ret = select(maxfd+1, &fdread, &fdwrite, &fdexcep, &timeout);

    return mc;
}
#endif

attribute_hidden SEXP in_do_curlVersion(SEXP call, SEXP op, SEXP args, SEXP rho)
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

/* Report a download error based on libcurl message. Issue warnings and
   record a flag (see errs[] in in_do_curlDownload) when errs[] is used. */
static void download_report_url_error(CURLMsg *msg)
{
    const char *url, *strerr, *type;
    long status = 0;
    int *url_errs = NULL;
    int timedout = 0;
    curl_easy_getinfo(msg->easy_handle, CURLINFO_EFFECTIVE_URL, &url);
    curl_easy_getinfo(msg->easy_handle, CURLINFO_RESPONSE_CODE,
		      &status);
    if (curl_easy_getinfo(msg->easy_handle, CURLINFO_PRIVATE, &url_errs)
	== CURLE_OK && url_errs) (*url_errs)++;
    
    // This reports the redirected URL
    if (status >= 400) {
	if (url && url[0] == 'h') {
	    strerr = http_errstr(status);
	    type = "HTTP";
	} else {
	    strerr = ftp_errstr(status);
	    type = "FTP";
	}
	warning(_("cannot open URL '%s': %s status was '%ld %s'"),
		url, type, status, strerr);
    } else {
	strerr = curl_easy_strerror(msg->data.result);
	timedout = msg->data.result == CURLE_OPERATION_TIMEDOUT
	           || msg->data.result == CURLE_ABORTED_BY_CALLBACK
	           || streql(strerr, "Timeout was reached");
	             
	if (timedout)
	    warning(_("URL '%s': Timeout of %d seconds was reached"),
		    url, current_timeout);
	else
	    warning(_("URL '%s': status was '%s'"), url, strerr);
    }
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
	    download_report_url_error(msg);
	    retval++;
	}
    }
    return retval;
}

static
void curlCommon(CURL *hnd, int redirect, int verify)
{
    const char *capath = getenv("CURL_CA_BUNDLE");
    if (verify) {
#ifdef Win32
	struct curl_tlssessioninfo *tls_backend_info = NULL;
	CURLcode ret = curl_easy_getinfo(hnd, CURLINFO_TLS_SSL_PTR,
	                                 &tls_backend_info);
	if (!ret && tls_backend_info->backend == CURLSSLBACKEND_SCHANNEL) {
	    capath = NULL;
# if LIBCURL_VERSION_NUM >= 0x074600
	    const char *rbe = getenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT");
	    if (rbe && StringTrue(rbe)) 
		curl_easy_setopt(hnd, CURLOPT_SSL_OPTIONS,
				 CURLSSLOPT_REVOKE_BEST_EFFORT);
# endif
	}
#endif
	if (capath && capath[0])
	    curl_easy_setopt(hnd, CURLOPT_CAINFO, capath);
    } else {
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYHOST, 0L);
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYPEER, 0L);
    }
#if 0
    // for consistency, but all utils:::makeUserAgent does is look up an option.
    SEXP sMakeUserAgent = install("makeUserAgent");
    SEXP agentFun = PROTECT(lang2(sMakeUserAgent, ScalarLogical(0)));
    SEXP utilsNS = PROTECT(R_FindNamespace(mkString("utils")));
    SEXP sua = eval(agentFun, utilsNS);
    UNPROTECT(1); /* utilsNS */
    PROTECT(sua);
    if(TYPEOF(sua) != NILSXP)
	curl_easy_setopt(hnd, CURLOPT_USERAGENT, CHAR(STRING_ELT(sua, 0)));
    UNPROTECT(2);
#else
    int Default = 1;
    SEXP sua = GetOption1(install("HTTPUserAgent")); // set in utils startup
    if (TYPEOF(sua) == STRSXP && LENGTH(sua) == 1 ) {
	const void *vmax = vmaxget();
	const char *p = translateChar(STRING_ELT(sua, 0));
	if (p[0] && p[1] && p[2] && p[0] == 'R' && p[1] == ' ' && p[2] == '(') {
	} else {
	    Default = 0;
	    curl_easy_setopt(hnd, CURLOPT_USERAGENT, p);
	}
	vmaxset(vmax);
    }
    if (Default) {
	char buf[20];
	curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);
	snprintf(buf, 20, "libcurl/%s", d->version);
	curl_easy_setopt(hnd, CURLOPT_USERAGENT, buf);
    }
#endif
    int timeout0 = asInteger(GetOption1(install("timeout")));
    long timeout = (timeout0 == NA_INTEGER) ? 0 : (1000L * timeout0);
    current_timeout = (timeout0 == NA_INTEGER) ? 0 : timeout0;
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
#endif /* HAVE_LIBCURL */

static void handle_cleanup(void *data)
{
    CURL *hnd = data;
    if (hnd)
	curl_easy_cleanup(hnd);
}

attribute_hidden SEXP
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
    int timeout = asInteger(CADDDR(args));
    if (timeout == NA_INTEGER)
	error(_("invalid %s argument"), "timeout");
    SEXP sTLS = CAD4R(args);
    const char *TLS = "";
    if (isString(sTLS) && LENGTH(sTLS) == 1 && STRING_ELT(sTLS, 0) != NA_STRING)
	TLS = translateChar(STRING_ELT(sTLS, 0));
    else error(_("invalid %s argument"), "TLS");

    CURL *hnd = curl_easy_init();
    if (!hnd)
	error(_("could not create curl handle"));
    /* Set up a context which will free the handle on error (also from
       curlCommon) */
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &handle_cleanup;
    cntxt.cenddata = hnd;
    curl_easy_setopt(hnd, CURLOPT_URL, url);
    curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(hnd, CURLOPT_NOBODY, 1L);
    curl_easy_setopt(hnd, CURLOPT_HEADERFUNCTION, &rcvHeaders);
    curl_easy_setopt(hnd, CURLOPT_WRITEHEADER, &headers);
    /* libcurl (at least 7.40.0) does not respect CURLOPT_NOBODY
       for some ftp header info (Content-Length and Accept-ranges). */
    curl_easy_setopt(hnd, CURLOPT_WRITEFUNCTION, &rcvBody);
    curlCommon(hnd, redirect, verify);
    if (timeout > 0) {
	curl_easy_setopt(hnd, CURLOPT_TIMEOUT, (long)timeout);
	current_timeout = timeout;
    }
    if (!streql(TLS, "")) {
	// 7.34.0 was released 2013-12-17
#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 34)
	long TLS_ver = CURL_SSLVERSION_TLSv1_0;
	if (streql(TLS, "1.0")) TLS_ver = CURL_SSLVERSION_TLSv1_0; 
	else if (streql(TLS, "1.1")) TLS_ver = CURL_SSLVERSION_TLSv1_1; 
	else if (streql(TLS, "1.2")) TLS_ver = CURL_SSLVERSION_TLSv1_2;
# if LIBCURL_VERSION_MAJOR > 7 ||  (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 52)
	else if (streql(TLS, "1.3")) TLS_ver = CURL_SSLVERSION_TLSv1_3;
# endif
	else
	    error(_("invalid %s argument"), "TLS");
	curl_easy_setopt(hnd, CURLOPT_SSLVERSION, TLS_ver);
# else
	error("TLS argument is unsupported in this libcurl version %d.%d",
	      LIBCURL_VERSION_MAJOR, LIBCURL_VERSION_MINOR);
#endif
    }

    char errbuf[CURL_ERROR_SIZE];
    curl_easy_setopt(hnd, CURLOPT_ERRORBUFFER, errbuf);
    // libcurl does not initialize this
    errbuf[0] = '\0';
    CURLcode ret = curl_easy_perform(hnd);
    if (ret != CURLE_OK) {
	if (errbuf[0])
	    error(_("libcurl error code %d:\n\t%s\n"), ret, errbuf);
	else if(ret == 77)
	    error(_("libcurl error code %d:\n\t%s\n"), ret,
		  "unable to access SSL/TLS CA certificates");
	else // rare case, error but no message
	    error("libcurl error code %d\n", ret);
    }
    long http_code = 0;
    curl_easy_getinfo (hnd, CURLINFO_RESPONSE_CODE, &http_code);
    endcontext(&cntxt);
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

# ifdef Win32
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
# endif // Win32

#if LIBCURL_VERSION_NUM >= 0x072000
# define CURL_LEN curl_off_t
#else
# define CURL_LEN double
#endif

/* display a progress bar (used when downloading a single file) */
static
int progress(void *clientp, CURL_LEN dltotal, CURL_LEN dlnow,
	     CURL_LEN ultotal, CURL_LEN ulnow)
{
    CURL *hnd = (CURL *) clientp;
    long status;
    curl_easy_getinfo(hnd, CURLINFO_RESPONSE_CODE, &status);

# ifdef Win32
    static int factor = 1;
# endif

    // we only use downloads.  dltotal may be zero.
    if ((status < 300) && (dltotal > 0.)) {
	if (total == 0.) {
	    total = dltotal;
	    char *type = NULL;
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
# ifdef Win32
	    R_FlushConsole();
	    if(R_Interactive) {
		if (total > 1e9) factor = total/1e6; else factor = 1;
		setprogressbarrange(pbar.pb, 0, total/factor);
		show(pbar.wprog);
	    }
	}
	if (R_Interactive) {
	    setprogressbar(pbar.pb, 1.0*dlnow/factor);
	    if (total > 0) {
		static char pbuf[30];
		int pc = 0.499 + 100.0*dlnow/total;
		if (pc > pbar.pc) {
		    snprintf(pbuf, 30, "%d%% downloaded", pc);
		    settext(pbar.wprog, pbuf);
		    pbar.pc = pc;
		}
	    }
	} else putdashes(&ndashes, (int)(50.0*dlnow/total));
    }
    R_ProcessEvents();
    return 0;
# else

	    if (R_Consolefile) fflush(R_Consolefile);
	}
	putdashes(&ndashes, (int)(50.0*dlnow/total));
    }
    return 0;
# endif
}

/* implement absolute-time timeout for the transfer time,
   used when downloading multiple files */
static
int progress_multi(void *clientp, CURL_LEN dltotal, CURL_LEN dlnow,
	     CURL_LEN ultotal, CURL_LEN ulnow)
{
    double *tstart = (double *) clientp;
    if (tstart) {
	if (*tstart == 0. && (dlnow > 0 || dltotal > 0)) 
	    *tstart = current_time; /* record when transfer started */
	else if (*tstart > 0. && (current_time - *tstart) > current_timeout)
	    return 1; /* abort transfer */
    }
    return 0;
}

#if LIBCURL_VERSION_NUM >= 0x075000
/* If this callback is available, use it to record the current time
   as start of transfer, for the purpose of absolute-time timeout for
   transfer time. This is to make sure that sending the request up to
   receiving the first byte of the file, or information about file length,
   is protected by a timeout. */
static
int prereq_multi(void *clientp, char *conn_primary_ip, char *conn_local_ip,
                 int conn_primary_port, int conn_local_port)
{
    double *tstart = (double *) clientp;
    *tstart = current_time;
    return CURL_PREREQFUNC_OK;
}
#endif
#endif // HAVE_LIBCURL

typedef struct {
    struct curl_slist *headers;
    CURLM *mhnd;
    int nurls;
    CURL ***hnd;
    FILE **out;
    double *tstart;
    SEXP sfile;
    int *errs;
#ifdef Win32
    winprogressbar *pbar;
#endif
} download_cleanup_info;

/* Clean up URL at given index. Close the file handle, delete file if 
   obviously wrong, remove the easy handle if present. */
static void download_cleanup_url(int i, download_cleanup_info *c)
{
    if (c->out && c->out[i]) {
	fclose(c->out[i]);
	c->out[i] = NULL;
#if LIBCURL_VERSION_NUM >= 0x073700
	curl_off_t dl;
	curl_easy_getinfo(c->hnd[i], CURLINFO_SIZE_DOWNLOAD_T, &dl);
#else
	double dl;
	curl_easy_getinfo(c->hnd[i], CURLINFO_SIZE_DOWNLOAD, &dl);
#endif
	if (c->sfile) {
	    long status = 0L;
	    curl_easy_getinfo(c->hnd[i], CURLINFO_RESPONSE_CODE, &status);
	    // should we do something about incomplete transfers?
	    if (status != 200 && dl == 0.) {
		const void *vmax = vmaxget();
		unlink(R_ExpandFileName(translateChar(STRING_ELT(c->sfile, i))));
		vmaxset(vmax);
	    }
	}
	curl_multi_remove_handle(c->mhnd, c->hnd[i]);
    }
    if (c->hnd && c->hnd[i]) {
	curl_easy_cleanup(c->hnd[i]);
	c->hnd[i] = NULL;
    }
}

static void download_cleanup(void *data)
{
    download_cleanup_info *c = data;

    for (int i = 0; i < c->nurls; i++)
	download_cleanup_url(i, c);
    if (c->mhnd)
	curl_multi_cleanup(c->mhnd);
    if (c->headers)
	curl_slist_free_all(c->headers);

#ifdef Win32
    if (c->pbar)
	hide(c->pbar->wprog);
#endif
}

/* Add URL at index i: open file, create easy handle, add to multi-handle.
   Report errors (issue warnings, set flags) only when mustWork != 0. 
   Returns 0 on success. */
static int download_add_url(int i, SEXP scmd, const char *mode,
                            int quiet, int single, int mustwork,
                            download_cleanup_info *c)
{
    const char *url, *file;
    const void *vmax = vmaxget();

    url = translateChar(STRING_ELT(scmd, i));
    c->hnd[i] = curl_easy_init();
    if (!c->hnd[i]) {
	if (mustwork) {
	    c->errs[i]++;
	    warning(_("could not create curl handle"));
	}
	vmaxset(vmax);
	return 1;
    }
    curl_easy_setopt(c->hnd[i], CURLOPT_URL, url);
    curl_easy_setopt(c->hnd[i], CURLOPT_FAILONERROR, 1L);
#if LIBCURL_VERSION_NUM >= 0x072b00
    /* Wait for the first conneciton to the server to reveal whether it
       allows multi-plexing before starting a new connection.  */
    curl_easy_setopt(c->hnd[i], CURLOPT_PIPEWAIT, 1L);
#endif
    /* Users will normally expect to follow redirections, although
       that is not the default in either curl or libcurl. */
    curlCommon(c->hnd[i], 1, 1);
    // all but Unix-alikes with ancient libcurl (before 2012-03-22)
#if (LIBCURL_VERSION_MAJOR > 7) || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 25)
    curl_easy_setopt(c->hnd[i], CURLOPT_TCP_KEEPALIVE, 1L);
#endif
    curl_easy_setopt(c->hnd[i], CURLOPT_HTTPHEADER, c->headers);

    /* check that destfile can be written */
    file = translateChar(STRING_ELT(c->sfile, i));
    c->out[i] = R_fopen(R_ExpandFileName(file), mode);
    if (!c->out[i]) {
	if (mustwork) {
	    c->errs[i]++;
	    warning(_("URL %s: cannot open destfile '%s', reason '%s'"),
		    url, file, strerror(errno));
	}
	vmaxset(vmax);
	return 1;
    }
#ifdef Unix
# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
    if (fileno(c->out[i]) >= FD_SETSIZE) {
	fclose(c->out[i]);
	c->out[i] = NULL;
	if (mustwork) {
	    c->errs[i]++;
	    warning(_("file descriptor is too large for select()"));
	}
	vmaxset(vmax);
	return 1;
    }
# endif
#endif
    // This uses the internal CURLOPT_WRITEFUNCTION
    curl_easy_setopt(c->hnd[i], CURLOPT_WRITEDATA, c->out[i]);
    curl_multi_add_handle(c->mhnd, c->hnd[i]);

    curl_easy_setopt(c->hnd[i], CURLOPT_PRIVATE, c->errs+i);

    total = 0.;
    if (!quiet && single) {
	// It would in principle be possible to have
	// multiple progress bars on Windows.
	curl_easy_setopt(c->hnd[i], CURLOPT_NOPROGRESS, 0L);
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
	    c->pbar = &pbar;
	}
#endif
	// For libcurl >= 7.32.0 use CURLOPT_XFERINFOFUNCTION
#if LIBCURL_VERSION_NUM >= 0x072000
	curl_easy_setopt(c->hnd[i], CURLOPT_XFERINFOFUNCTION, progress);
	curl_easy_setopt(c->hnd[i], CURLOPT_XFERINFODATA, c->hnd[i]);
#else
	curl_easy_setopt(c->hnd[i], CURLOPT_PROGRESSFUNCTION, progress);
	curl_easy_setopt(c->hnd[i], CURLOPT_PROGRESSDATA, c->hnd[i]);
#endif
    } else if (quiet && single) {
	curl_easy_setopt(c->hnd[i], CURLOPT_NOPROGRESS, 1L);
    } else {
	curl_easy_setopt(c->hnd[i], CURLOPT_NOPROGRESS, 0L);

	/* Implement absolute-time timeout as a replacement to CURLOPT_TIMEOUT
	   for simultaneous download. The goal is to keep all parts of the
	   download protected by a timeout, while reducing the risk that
	   implementation details of the simultaneous download (such as the
	   number of concurrent connections, limit of connections to the same
	   server, waiting on HTTP/2 multiplexing) would cause downloads to
	   time out unpredictably. Except contention over network bandwidth,
	   this should also reduce the risk that timeouts that worked with
	   sequential downloads would not be sufficient. All within the scope
	   of R's single timeout value for internet operations. This assumes
	   curl connection timeout is in use. CURLOPT_TIMEOUT is not used
	   because it includes also the time transfers are queued by curl.
	*/
	curl_easy_setopt(c->hnd[i], CURLOPT_TIMEOUT, 0L);
	c->tstart[i] = 0.;
	/* cover the time from when first data is received */
	// For libcurl >= 7.32.0 use CURLOPT_XFERINFOFUNCTION
    #if LIBCURL_VERSION_NUM >= 0x072000
	curl_easy_setopt(c->hnd[i], CURLOPT_XFERINFOFUNCTION, progress_multi);
	curl_easy_setopt(c->hnd[i], CURLOPT_XFERINFODATA, &c->tstart[i]);
    #else
	curl_easy_setopt(c->hnd[i], CURLOPT_PROGRESSFUNCTION, progress_multi);
	curl_easy_setopt(c->hnd[i], CURLOPT_PROGRESSDATA, &c->tstart[i]);
    #endif
	/* cover the time between a connection is established and first
	   data is received */
    #if LIBCURL_VERSION_NUM >= 0x075000
	curl_easy_setopt(c->hnd[i], CURLOPT_PREREQFUNCTION, prereq_multi);
	curl_easy_setopt(c->hnd[i], CURLOPT_PREREQDATA, &c->tstart[i]);
    #elif LIBCURL_VERSION_NUM >= 0x070100
	curl_easy_setopt(c->hnd[i], CURLOPT_LOW_SPEED_TIME,
	                 (long)current_timeout);
	curl_easy_setopt(c->hnd[i], CURLOPT_LOW_SPEED_LIMIT, 1L);
    #endif
    }

    /* This would allow the negotiation of compressed HTTP transfers,
       but it is not clear it is always a good idea.
       curl_easy_setopt(hnd[i], CURLOPT_ACCEPT_ENCODING, "gzip, deflate");
    */

    if (!quiet) REprintf(_("trying URL '%s'\n"), url);
    vmaxset(vmax);
    return 0;
}

/* Add one URL to the multi-handle, possibly trying multiple URLs in case
   of failures. Report any errors. Advance URL index accordingly. */
static int download_add_one_url(int *i, SEXP scmd, const char *mode, int quiet,
                                int single, download_cleanup_info *c)
{
    while(*i < c->nurls) {
	if (!download_add_url((*i)++, scmd, mode, quiet, single, 1, c))
	    return 0; /* success */
    }
    return 1; /* failure */ 
}

/* Add at most n URLs to the multi-handle, if possible. Bail out when an URL
   cannot be added (do not advance, do not report errors). The idea is that
   when an URL cannot be added, it can be due to lack of resources, so it makes
   sense to try later. Advance only when URLs have been added. */
static int download_try_add_urls(int *i, int n, SEXP scmd,
                                 const char *mode, int quiet, int single,
                                 download_cleanup_info *c)
{
    int added = 0;

    while(added < n && *i < c->nurls) {
	if (!download_add_url(*i, scmd, mode, quiet, single, 0, c)) {
	    (*i)++;
	    added++;
	} else
	    break;
    }
    return added; /* number of added URLs */
}

/* For all finished downloads from the multi handle, clean up the easy
   handle, close the file, report errors, delete the file if obviously
   wrong. */
static void download_close_finished(download_cleanup_info *c)
{
    for(int n = 1; n > 0;) {
        CURLMsg *msg = curl_multi_info_read(c->mhnd, &n);
	if (!msg)
	    break;

	/* compute URL index */
	int *url_errs = NULL;
	curl_easy_getinfo(msg->easy_handle, CURLINFO_PRIVATE, &url_errs);
	int i = (int)(url_errs - c->errs);

        if (msg->data.result != CURLE_OK)
            download_report_url_error(msg);
	download_cleanup_url(i, c);
    }
}

/* download(url, destfile, quiet, mode, headers, cacheOK) */

attribute_hidden SEXP
in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_LIBCURL
    error(_("download.file(method = \"libcurl\") is not supported on this platform"));
    return R_NilValue;
#else
    SEXP scmd, sfile, smode, sheaders;
    const char *mode;
    int quiet, cacheOK;
    struct curl_slist *headers = NULL;
    const void *vmax = vmaxget();
    RCNTXT cntxt;
    download_cleanup_info c;

    scmd = CAR(args); args = CDR(args);
    if (!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    int nurls = length(scmd);
    int single = (nurls == 1);
    const int MAX_CONCURRENT_URLS = 15;

#ifdef Win32
    /* not used as 7.28 is required */
# if LIBCURL_VERSION_MAJOR < 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR < 28)
    if (nurls > FD_SETSIZE)
	error(_("too many file descriptors for select()"));
# endif
#endif

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
    mode = translateChar(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args)); args = CDR(args);
    if (cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");
    sheaders = CAR(args);
    if(TYPEOF(sheaders) != NILSXP && !isString(sheaders))
	error(_("invalid '%s' argument"), "headers");

    c.mhnd = NULL;
    c.nurls = nurls;
    c.hnd = NULL;
    c.out = NULL;
    c.errs = NULL;
    if (strchr(mode, 'w'))
	c.sfile = sfile;
    else
	c.sfile = NULL;
#ifdef Win32
    c.pbar = NULL;
#endif
    c.headers = NULL;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &download_cleanup;
    cntxt.cenddata = &c;
    if(TYPEOF(sheaders) != NILSXP) {
	for (int i = 0; i < LENGTH(sheaders); i++) {
	    struct curl_slist *tmp =
		curl_slist_append(headers,
		                  translateChar(STRING_ELT(sheaders, i)));
	    if (!tmp)
		error(_("out of memory"));
	    c.headers = headers = tmp;
	}
    }

    /* This comes mainly from curl --libcurl on the call used by
       download.file(method = "curl").
       Also https://curl.haxx.se/libcurl/c/multi-single.html.
    */

    if (!cacheOK) {
	/* This _is_ the right way to do this: see §14.9 of
	   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html */
	struct curl_slist *tmp =
	    curl_slist_append(headers, "Pragma: no-cache");
	if(!tmp)
	    error(_("out of memory"));
	c.headers = headers = tmp;
    }

    CURLM *mhnd = curl_multi_init();
    if (!mhnd)
	error(_("could not create curl handle"));
    c.mhnd = mhnd;

    int still_running, repeats = 0, n_err = 0;
    R_CheckStack2((size_t)nurls
                  * (sizeof(int) + sizeof(FILE *) + sizeof(CURL **)));
    CURL **hnd[nurls];
    FILE *out[nurls];
    int errs[nurls];
    double tstart[nurls];

    for(int i = 0; i < nurls; i++) {
	hnd[i] = NULL;
	out[i] = NULL;
	errs[i] = 0;
	tstart[i] = 0;
    }
    c.hnd = hnd;
    c.out = out;
    c.errs = errs;
    c.tstart = tstart;

    int next_url = 0;

    /* We keep adding URLs to the libcurl multi-handle while the download
       is in progress (no more than MAX_CONCURRENT_URLS at a time). We are
       not adding more to avoid running out of resources (number of file
       handles, FILE streams, etc) and to not hurt performance (too many
       parallel writes).

       When no URL is active, at least one must be added to ensure progress.
       But if possible we add more (up to MAX_CONCURRENT_URLS) - we do that
       only optionally to reduce the risks of failing due to lack of resources.

       Note that an URL may be added but the transfer be delayed. Currently
       this can happen when the limit on the number of connections per host
       is reached, but in principle curl can do it also in other cases.
    */

#if LIBCURL_VERSION_NUM >= 0x071e00
    /* Most current browsers would use at least 6. RFC2616 recommends at most
       2, but this has been removed in RFC7230. The limit is important for
       HTTP 1.1 transfers. HTTP 2 (with CURLOPT_PIPEWAIT) would multiplex
       downloads from the same server through a single connection. */
    curl_multi_setopt(mhnd, CURLMOPT_MAX_HOST_CONNECTIONS, 6L);
#endif

    if (!single) current_time = currentTime();
    if (download_add_one_url(&next_url, scmd, mode, quiet, single, &c)) {
        // no dest files could be opened, so bail out
        endcontext(&cntxt);
        download_cleanup(&c);
        vmaxset(vmax);
	return ScalarInteger(1);
    }

    download_try_add_urls(&next_url, MAX_CONCURRENT_URLS - 1, scmd,
                          mode, quiet, single, &c);

    R_Busy(1);
    if (!single) current_time = currentTime();
    //  curl_multi_wait needs curl >= 7.28.0 .
    curl_multi_perform(mhnd, &still_running);
    do {
	if (!single) current_time = currentTime();
	int numfds;
	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds);
	if (mc != CURLM_OK)  { // internal, do not translate
	    warning("curl_multi_wait() failed, code %d", mc);
	    break;
	}
	if (!numfds) {
	    /* 'numfds' being zero means either a timeout or no file
	       descriptors to wait for. Try timeout on first
	       occurrence, then assume no file descriptors to wait for
	       means 'sleep for 100 milliseconds'.
	    */
	    if (repeats++ > 0) Rsleep(0.1); // do not block R process
	} else repeats = 0;
	R_ProcessEvents();
	if (!single) current_time = currentTime();
	curl_multi_perform(mhnd, &still_running);

	if (!single)
	    /* don't do for single URL downloads to preserve special reporting
	       (below) which needs the easy handle */
	    download_close_finished(&c); /* releases resources */

	if (!still_running) {
	    if (!download_add_one_url(&next_url, scmd, mode, quiet, single,
	                              &c))
		still_running++;
	}

	download_try_add_urls(&next_url, MAX_CONCURRENT_URLS - still_running,
	                      scmd, mode, quiet, single, &c);

	if (!single) current_time = currentTime();
	curl_multi_perform(mhnd, &still_running);
    } while(still_running || next_url < nurls);
    R_Busy(0);
#ifdef Win32
    if (R_Interactive && !quiet && single) {
	c.pbar = NULL;
	hide(pbar.wprog);
    } else if (total > 0.) {
	REprintf("\n");
	R_FlushConsole();
    }
#else
    if (total > 0.) REprintf("\n");
    if (R_Consolefile) fflush(R_Consolefile);
#endif
    if (single) {
	long status;
	curl_easy_getinfo(hnd[0], CURLINFO_RESPONSE_CODE, &status);
	// new interface in libcurl >= 7.55.0
#if LIBCURL_VERSION_NUM >= 0x073700
	curl_off_t cl, dl;
	curl_easy_getinfo(hnd[0], CURLINFO_SIZE_DOWNLOAD_T, &dl);
#else
	double cl ,dl;
	curl_easy_getinfo(hnd[0], CURLINFO_SIZE_DOWNLOAD, &dl);
#endif
	if (!quiet && status == 200) {
	    if (dl > 1024*1024)
		REprintf("downloaded %0.1f MB\n\n", (double)dl/1024/1024);
	    else if (dl > 10240)
		REprintf("downloaded %d KB\n\n", (int) (dl/1024.0));
	    else
		REprintf("downloaded %d bytes\n\n", (int) dl);
	}
#if LIBCURL_VERSION_NUM >= 0x073700
	curl_easy_getinfo(hnd[0], CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &cl);
#else
	curl_easy_getinfo(hnd[0], CURLINFO_CONTENT_LENGTH_DOWNLOAD, &cl);
#endif
	if (cl >= 0 && dl != cl)
	    warning(_("downloaded length %0.f != reported length %0.f"),
	            (double) dl, (double) cl);
    }

    /* record status of single URL download, because download_close_finished()
       cleans up the easy handle */
    long status = 0;
    if (single)
	curl_easy_getinfo(hnd[0], CURLINFO_RESPONSE_CODE, &status);	

    download_close_finished(&c); /* updates errs */

    for(int i = 0; i < nurls; i++)
	if (errs[i]) n_err++;

    if(!single) {
	if (n_err == nurls) error(_("cannot download any files"));
	else if (n_err) warning(_("some files were not downloaded"));
    } else if(n_err) {
	if (status != 200)
	    error(_("cannot open URL '%s'"),
	          translateChar(STRING_ELT(scmd, 0)));
	else
	    error(_("download from '%s' failed"),
	          translateChar(STRING_ELT(scmd, 0)));
    }
    endcontext(&cntxt);
    download_cleanup(&c);
    vmaxset(vmax);
    SEXP ans = ScalarInteger(0);
    if(nurls > 1) {
	PROTECT(ans);
	SEXP sretvals = install("retvals");
	SEXP retval = allocVector(INTSXP, nurls);
	setAttrib(ans, sretvals, retval);
	for(int i = 0; i < nurls; i++)
	    INTEGER(retval)[i] = errs[i] ? 1 : 0;
	UNPROTECT(1); /* ans */
    }
    return ans;
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

   It seems that expanding was being done by a couple of packages and
   gave a use-after-free error with libcurl 7.64.0.  So initial size
   increased to 16x.

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
    struct curl_slist *headers;
} *RCurlconn;

static size_t rcvData(void *ptr, size_t size, size_t nitems, void *ctx)
{
    RCurlconn ctxt = (RCurlconn) ctx;

    /* move down any unused data: can overlap */
    if (ctxt->filled) memmove(ctxt->buf, ctxt->current, ctxt->filled);

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
    ctxt->current = ctxt->buf;
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
	if (mc != CURLM_OK) {
	    warning("curl_multi_wait() failed, code %d", mc);
	    break;
	}
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

    curl_slist_free_all(ctxt->headers);
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
	/* FIXME: A an error from fetchData() or a warning turned into error
	          would not close the connection. But the GC would do it later.
	*/
	n_err += fetchData(ctxt);
	total += consumeData(p + total, (nbytes - total), ctxt);
    }
    if (n_err != 0) {
	Curl_close(con);
	error(_("cannot read from connection"));
    }
    return total/size;
}

static Rboolean Curl_open(Rconnection con)
{
    char *url = con->description;
    RCurlconn ctxt = (RCurlconn)(con->private);
    int mlen;

    if (con->mode[0] != 'r') {
	REprintf("can only open URLs for reading");
	return FALSE;
    }

    ctxt->hnd = curl_easy_init();
    if (!ctxt->hnd)
	error(_("could not create curl handle"));
    /* Set up a context which will free the handle on error (also from
       curlCommon) */
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    cntxt.cend = &handle_cleanup;
    cntxt.cenddata = ctxt->hnd;
    curl_easy_setopt(ctxt->hnd, CURLOPT_URL, url);
    curl_easy_setopt(ctxt->hnd, CURLOPT_FAILONERROR, 1L);
    curlCommon(ctxt->hnd, 1, 1);
    curl_easy_setopt(ctxt->hnd, CURLOPT_NOPROGRESS, 1L);
#if LIBCURL_VERSION_MAJOR > 7 || (LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 25)
    curl_easy_setopt(ctxt->hnd, CURLOPT_TCP_KEEPALIVE, 1L);
#endif

    if (ctxt->headers)
	curl_easy_setopt(ctxt->hnd, CURLOPT_HTTPHEADER, ctxt->headers);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEFUNCTION, rcvData);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEDATA, ctxt);
    ctxt->mh = curl_multi_init();
    if (!ctxt->mh) 
	error(_("could not create curl handle"));
    /* FIXME: suitability of file handle for select should be checked with
              libcurl older than 7.28 */
    curl_multi_add_handle(ctxt->mh, ctxt->hnd);

    ctxt->current = ctxt->buf; ctxt->filled = 0; ctxt->available = FALSE;

    // Establish the connection: not clear if we should do this now.
    ctxt->sr = 1;
    int n_err = 0;
    endcontext(&cntxt); /* from now leave ctxt->hnd cleanup to GC */
    con->isopen = TRUE; /* enable GC cleanup of opened connections */
    while(ctxt->sr && !ctxt->available)
	/* FIXME: A an error from fetchData() or a warning turned into error
	          would not close the connection. But the GC would do it later.
	*/
	n_err += fetchData(ctxt);
    if (n_err != 0) {
	Curl_close(con);
	error(_("cannot open the connection to '%s'"), url);
    }

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if (mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
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
in_newCurlUrl(const char *description, const char * const mode,
	      SEXP headers, int type)
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
    ctxt->bufsize = 16 * CURL_MAX_WRITE_SIZE;
    ctxt->buf = malloc(ctxt->bufsize);
    if (!ctxt->buf) {
	free(new->description); free(new->class); free(new->private);
	free(new);
	error(_("allocation of url connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ctxt->headers = NULL;
    const void *vmax = vmaxget();
    for (int i = 0; i < LENGTH(headers); i++) {
	struct curl_slist *tmp =
	    curl_slist_append(ctxt->headers,
	                      translateChar(STRING_ELT(headers, i)));
	if (!tmp) {
	    free(new->description); free(new->class); free(new->private);
	    free(new); curl_slist_free_all(ctxt->headers);
	    error(_("allocation of url connection failed"));
	    /* for Solaris 12.5 */ new = NULL;
	}
	ctxt->headers = tmp;
    }
    vmaxset(vmax);
    return new;
#else
    error(_("url(method = \"libcurl\") is not supported on this platform"));
    return (Rconnection)0; /* -Wall */
#endif
}
