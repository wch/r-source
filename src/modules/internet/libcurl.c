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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <errno.h>

#ifdef HAVE_CURL_CURL_H
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
#ifdef HAVE_CURL_CURL_H
    curl_version_info_data *d = curl_version_info(CURLVERSION_NOW);
    SET_STRING_ELT(ans, 0, mkChar(d->version));
    setAttrib(ans, install("ssl_version"), 
	      mkString(d->ssl_version ? d->ssl_version : "none"));
    setAttrib(ans, install("libssh_version"), 
	      mkString(((d->age >= 3) && d->libssh_version) ? d->libssh_version : ""));
    const char * const *p;
    int n, i; 
    for(p = d->protocols, n = 0; *p; p++, n++) ;
    SEXP protocols = PROTECT(allocVector(STRSXP, n));
    for(p = d->protocols, i = 0; i < n; i++, p++)
	SET_STRING_ELT(protocols, i, mkChar(*p));
    setAttrib(ans, install("protocols"), protocols);
    UNPROTECT(1);
#else
    SET_STRING_ELT(ans, 0, mkChar(""));
#endif
    UNPROTECT(1);
    return ans;
}


#ifdef HAVE_CURL_CURL_H
// extract some common code
static void curlCommon(CURL *hnd, int redirect)
{
    const char *capath = getenv("CURL_CA_BUNDLE");
    if (capath && capath[0])
	curl_easy_setopt(hnd, CURLOPT_CAINFO, capath);
#ifdef Win32
    else
	curl_easy_setopt(hnd, CURLOPT_SSL_VERIFYPEER, 0L);
#endif
    const char *ua = CHAR(STRING_ELT(GetOption1(install("HTTPUserAgent")),0));
    curl_easy_setopt(hnd, CURLOPT_USERAGENT, ua);
    int timeout0 = asInteger(GetOption1(install("timeout")));
    long timeout = timeout0 = NA_INTEGER ? 0 : 1000L * timeout0;
    curl_easy_setopt(hnd, CURLOPT_CONNECTTIMEOUT_MS, timeout);
    curl_easy_setopt(hnd, CURLOPT_TIMEOUT_MS, timeout);
    if(redirect) {
	curl_easy_setopt(hnd, CURLOPT_FOLLOWLOCATION, 1L);
	curl_easy_setopt(hnd, CURLOPT_MAXREDIRS, 20L);
    }
    int verbosity = asInteger(GetOption1(install("internet.info")));
    if(verbosity < 2) curl_easy_setopt(hnd, CURLOPT_VERBOSE, 1L);

    // enable the cookie engine, keep cookies in memory
    curl_easy_setopt(hnd, CURLOPT_COOKIEFILE, "");
}

static char headers[500][2048];
static int used;

static size_t 
rcvHeaders(void *buffer, size_t size, size_t nmemb, void *userp) 
{
    char *d = (char*)buffer;
    size_t result = size * nmemb, res = result > 2048 ? 2048 : result;
    if(used >= 500) return result;
    strncpy(headers[used], d, res);
    headers[used][res] = '\0';
    used++;
    return result;      
}
#endif

SEXP attribute_hidden in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_CURL_CURL_H
    error("curlGetHeaders is not supported on this platform");
    return R_NilValue;
#else
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
       error("invalid %s argument", "url");
    const char *url = translateChar(STRING_ELT(CAR(args), 0));
    used = 0;
    int redirect = asLogical(CADR(args));
    if(redirect == NA_LOGICAL)
       error("invalid %s argument", "redirect");

    CURL *hnd = curl_easy_init();
    curl_easy_setopt(hnd, CURLOPT_URL, url);
    curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(hnd, CURLOPT_NOBODY, 1L);
    curl_easy_setopt(hnd, CURLOPT_HEADERFUNCTION, &rcvHeaders);
    curl_easy_setopt(hnd, CURLOPT_WRITEHEADER, &headers);
    curlCommon(hnd, redirect);

    char errbuf[CURL_ERROR_SIZE];
    curl_easy_setopt(hnd, CURLOPT_ERRORBUFFER, errbuf);
    CURLcode ret = curl_easy_perform(hnd);
    if (ret != CURLE_OK)
	error("libcurl error code %d\n\t%s\n", ret, errbuf);
    long http_code = 0;
    curl_easy_getinfo (hnd, CURLINFO_RESPONSE_CODE, &http_code);
    curl_easy_cleanup(hnd);

    SEXP ans = PROTECT(allocVector(STRSXP, used));
    for (int i = 0; i < used; i++)
	SET_STRING_ELT(ans, i, mkChar(headers[i]));
    setAttrib(ans, install("status"), ScalarInteger((int) http_code));
    UNPROTECT(1);
    return ans;
#endif
}

extern void Rsleep(double timeint);

/* download(url, destfile, quiet, mode, headers, cacheOK) */

SEXP attribute_hidden 
in_do_curlDownload(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_CURL_CURL_H
    error("download.file(method = \"libcurl\") is not supported on this platform");
    return R_NilValue;
#else
    SEXP scmd, sfile, smode;
    const char *url, *file, *mode;
    int quiet, cacheOK;
    struct curl_slist *slist1 = NULL;
    FILE *out;

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
    quiet = asLogical(CAR(args)); args = CDR(args);
    if(quiet == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quiet");
    smode =  CAR(args); args = CDR(args);
    if(!isString(smode) || length(smode) != 1)
	error(_("invalid '%s' argument"), "mode");
    mode = CHAR(STRING_ELT(smode, 0));
    cacheOK = asLogical(CAR(args));
    if(cacheOK == NA_LOGICAL)
	error(_("invalid '%s' argument"), "cacheOK");

    /* This comes mainly from curl --libcurl on the call used by
       download.file(method = "curl").
       Also http://curl.haxx.se/libcurl/c/multi-single.html.

       It would be a good idea to use a custom progress callback, and
       it is said that in future libcurl may not have one at all.
    */

    CURL *hnd = curl_easy_init();
    CURLM *multi_handle = curl_multi_init();
    curl_multi_add_handle(multi_handle, hnd);
    int still_running, repeats = 0;
 
    curl_easy_setopt(hnd, CURLOPT_URL, url);
    curl_easy_setopt(hnd, CURLOPT_HEADER, 0L);
    if(!quiet) curl_easy_setopt(hnd, CURLOPT_NOPROGRESS, 0L);
    /* Users will normally expect to follow redirections, although 
       that is not the default in either curl or libcurl. */
    curlCommon(hnd, 1);
    curl_easy_setopt(hnd, CURLOPT_TCP_KEEPALIVE, 1L);

    /* This allows the negotiation of compressed HTTP transfers,
       but it is not clear it is always a good idea.

    curl_easy_setopt(hnd, CURLOPT_ACCEPT_ENCODING, "gzip, deflate");
    */

    if (!cacheOK) {
	/* This _is_ the right way to do this: see §14.9 of
	   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html */
	slist1 = curl_slist_append(slist1, "Pragma: no-cache");
	curl_easy_setopt(hnd, CURLOPT_HTTPHEADER, slist1);
    }

    out = R_fopen(R_ExpandFileName(file), mode);
    if(!out)
	error(_("cannot open destfile '%s', reason '%s'"), 
	      file, strerror(errno));
    curl_easy_setopt(hnd, CURLOPT_WRITEDATA, out);

    if(!quiet) REprintf(_("trying URL '%s'\n"), url);

    R_Busy(1);
    curl_multi_perform(multi_handle, &still_running);
    do {
	int numfds; // This needs curl >= 7.28.0
 	CURLMcode mc = curl_multi_wait(multi_handle, NULL, 0, 100, &numfds); 
	if(mc != CURLM_OK)
	    error("curl_multi_wait() failed, code %d", mc);
	if(!numfds) {
	    /* 'numfds' being zero means either a timeout or no file
	       descriptors to wait for. Try timeout on first
	       occurrence, then assume no file descriptors and no file
	       descriptors to wait for means 'sleep for 100 milliseconds'.
	    */ 
	    if(repeats++ > 0) Rsleep(0.1);
	} else repeats = 0;
	curl_multi_perform(multi_handle, &still_running);
    } while(still_running);
    R_Busy(0);

    for(int n = 1; n > 0;) {
	CURLMsg *msg = curl_multi_info_read(multi_handle, &n);
	if(msg) {
	    CURLcode ret = msg->data.result;
	    if (ret != CURLE_OK) {
		if(!quiet) REprintf("\n"); // clear progress display
		error("  %s\n", curl_easy_strerror(ret));
	    }
	}
    }

    fclose(out);

    curl_multi_remove_handle(multi_handle, hnd);
    curl_easy_cleanup(hnd);
    curl_multi_cleanup(multi_handle);
    if (!cacheOK) curl_slist_free_all(slist1);

    return ScalarInteger(0);
#endif
}
