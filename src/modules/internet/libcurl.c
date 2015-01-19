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

SEXP attribute_hidden 
in_do_curlGetHeaders(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifndef HAVE_CURL_CURL_H
    error(_("curlGetHeaders is not supported on this platform"));
    return R_NilValue;
#else
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
       error("invalid %s argument", "url");
    const char *url = translateChar(STRING_ELT(CAR(args), 0));
    used = 0;
    int redirect = asLogical(CADR(args));
    if(redirect == NA_LOGICAL)
	error(_("invalid %s argument"), "redirect");

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
	error(_("libcurl error code %d\n\t%s\n"), ret, errbuf);
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
    error(_("download.file(method = \"libcurl\") is not supported on this platform"));
    return R_NilValue;
#else
    SEXP scmd, sfile, smode;
    const char *url, *file, *mode;
    int quiet, cacheOK;
    struct curl_slist *slist1 = NULL;

    scmd = CAR(args); args = CDR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "url");
    int nurls = length(scmd);
    sfile = CAR(args); args = CDR(args);
    if(!isString(sfile) || length(sfile) < 1)
	error(_("invalid '%s' argument"), "destfile");
    if(length(sfile) != length(scmd))
	error(_("lengths of 'url' and 'destfile' must match"));
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

    if (!cacheOK) {
	/* This _is_ the right way to do this: see §14.9 of
	   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html */
	slist1 = curl_slist_append(slist1, "Pragma: no-cache");
    }

    CURLM *mhnd = curl_multi_init();
    int still_running, repeats = 0;
    CURL **hnd[nurls];
    FILE *out[nurls];

    for(int i = 0; i < nurls; i++) {
	hnd[i] = curl_easy_init();
	curl_multi_add_handle(mhnd, hnd[i]);
 
	url = CHAR(STRING_ELT(scmd, i));
	curl_easy_setopt(hnd[i], CURLOPT_URL, url);
	curl_easy_setopt(hnd[i], CURLOPT_HEADER, 0L);
	if(!quiet && nurls <= 1) 
	    curl_easy_setopt(hnd[i], CURLOPT_NOPROGRESS, 0L);
	/* Users will normally expect to follow redirections, although 
	   that is not the default in either curl or libcurl. */
	curlCommon(hnd[i], 1);
	curl_easy_setopt(hnd[i], CURLOPT_TCP_KEEPALIVE, 1L);
	if (!cacheOK) curl_easy_setopt(hnd[i], CURLOPT_HTTPHEADER, slist1);

	/* This allows the negotiation of compressed HTTP transfers,
	   but it is not clear it is always a good idea.

	   curl_easy_setopt(hnd[i], CURLOPT_ACCEPT_ENCODING, "gzip, deflate");
	*/


	file = translateChar(STRING_ELT(sfile, i));
	out[i] = R_fopen(R_ExpandFileName(file), mode);
	if(!out[i])
	    error(_("cannot open destfile '%s', reason '%s'"), 
		  file, strerror(errno));
	curl_easy_setopt(hnd[i], CURLOPT_WRITEDATA, out[i]);

	if(!quiet) REprintf(_("trying URL '%s'\n"), url);
    }

    R_Busy(1);
    curl_multi_perform(mhnd, &still_running);
    do {
	int numfds; // This needs curl >= 7.28.0
 	CURLMcode mc = curl_multi_wait(mhnd, NULL, 0, 100, &numfds); 
	if(mc != CURLM_OK)  // internal, do not translate
	    error("curl_multi_wait() failed, code %d", mc);
	if(!numfds) {
	    /* 'numfds' being zero means either a timeout or no file
	       descriptors to wait for. Try timeout on first
	       occurrence, then assume no file descriptors and no file
	       descriptors to wait for means 'sleep for 100 milliseconds'.
	    */ 
	    if(repeats++ > 0) Rsleep(0.1);
	} else repeats = 0;
	curl_multi_perform(mhnd, &still_running);
    } while(still_running);
    R_Busy(0);

    for(int n = 1; n > 0;) {
	CURLMsg *msg = curl_multi_info_read(mhnd, &n);
	if(msg) {
	    CURLcode ret = msg->data.result;
	    if (ret != CURLE_OK) {
		if(!quiet) REprintf("\n"); // clear progress display
		error("  %s\n", curl_easy_strerror(ret));
	    }
	}
    }


    for(int i = 0; i < nurls; i++) {
	fclose(out[i]);
	curl_multi_remove_handle(mhnd, hnd[i]);
	curl_easy_cleanup(hnd[i]);
    }
    curl_multi_cleanup(mhnd);
    if (!cacheOK) curl_slist_free_all(slist1);

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
	/* allocate more space if required: unlikely */
	if (ctxt->filled + add > ctxt->bufsize) {
	    size_t newbufsize = 2 * ctxt->bufsize;
	    void *newbuf = realloc(ctxt->buf, newbufsize);
	    if (!newbuf) error("Failure in re-allocation in rcvData");
	    ctxt->buf = newbuf;
	    ctxt->bufsize = newbufsize;
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

void fetchData(RCurlconn ctxt) 
{
    int repeats = 0;
    do {
	int numfds;
	CURLMcode mc = curl_multi_wait(ctxt->mh, NULL, 0, 100, &numfds); 
	if (mc != CURLM_OK) 
	    error("curl_multi_wait() failed, code %d", mc);
	if (!numfds) {
	    if (repeats++ > 0) Rsleep(0.1);
	} else repeats = 0;
	curl_multi_perform(ctxt->mh, &ctxt->sr);
	if (ctxt->available) break;
    } while(ctxt->sr);

    for(int msg = 1; msg > 0;) {
	CURLMsg *out = curl_multi_info_read(ctxt->mh, &msg);
	if (out) {
	    CURLcode ret = out->data.result;
	    if (ret != CURLE_OK) error(curl_easy_strerror(ret));
	}
    }
}

static size_t Curl_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->private);
    size_t nbytes = size*nitems;
    char *p = (char *) ptr;
    size_t total = consumeData(ptr, nbytes, ctxt);
    while((total < nbytes) && ctxt->sr) {
	fetchData(ctxt);
	total += consumeData(p + total, (nbytes - total), ctxt);
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
    curlCommon(ctxt->hnd, 1);
    curl_easy_setopt(ctxt->hnd, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt(ctxt->hnd, CURLOPT_TCP_KEEPALIVE, 1L);

    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEFUNCTION, rcvData);
    curl_easy_setopt(ctxt->hnd, CURLOPT_WRITEDATA, ctxt);
    ctxt->mh = curl_multi_init();
    curl_multi_add_handle(ctxt->mh, ctxt->hnd);

    ctxt->current = ctxt->buf; ctxt->filled = 0; ctxt->available = FALSE; 

    // Establish the connection: not clear if we should do this now.
    ctxt->sr = 1;
    while(ctxt->sr && !ctxt->available) fetchData(ctxt);

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if (strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    set_iconv(con);
    return TRUE;
}

static void Curl_close(Rconnection con)
{
    RCurlconn ctxt = (RCurlconn)(con->private);

    curl_multi_remove_handle(ctxt->mh, ctxt->hnd);
    curl_easy_cleanup(ctxt->hnd);
    curl_multi_cleanup(ctxt->mh);
    free(ctxt->buf);

    con->isopen = FALSE;
}

static int Curl_fgetc_internal(Rconnection con)
{
    unsigned char c;
    size_t n = Curl_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}

Rconnection in_newCurlUrl(const char *description, const char * const mode)
{
#ifdef HAVE_CURL_CURL_H
    Rconnection new = (Rconnection) malloc(sizeof(struct Rconn));
    if (!new) error(_("allocation of url connection failed"));
    new->class = (char *) malloc(strlen("url-libcurl") + 1);
    if (!new->class) {
	free(new);
	error(_("allocation of url connection failed"));
    }
    strcpy(new->class, "url-libcurl");
    new->description = (char *) malloc(strlen(description) + 1);
    if (!new->description) {
	free(new->class); free(new);
	error(_("allocation of url connection failed"));
    }
    init_con(new, description, CE_NATIVE, mode);
    new->canwrite = FALSE;
    new->open = &Curl_open;
    new->close = &Curl_close;
    new->fgetc_internal = &Curl_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->read = &Curl_read;
    new->private = (void *) malloc(sizeof(struct Curlconn));
    if (!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of url connection failed"));
    }
    RCurlconn ctxt = (RCurlconn) new->private;
    ctxt->bufsize = 2 * CURL_MAX_WRITE_SIZE;
    ctxt->buf = malloc(ctxt->bufsize);
    if (!ctxt->buf) {
	free(new->description); free(new->class); free(new->private);
	free(new);
	error(_("allocation of url connection failed"));
    }
    return new;
#else
    error(_("url(method = \"libcurl\") is not supported on this platform"));
    return (Rconnection)0; /* -Wall */
#endif
}
