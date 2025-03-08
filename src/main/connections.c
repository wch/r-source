/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2025   The R Core Team.
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

/* Notes on so-called 'Large File Support':

   The C stdio functions such as fseek and ftell are defined using
   'long' for file positioning: also fread/fwrite use size_t for size
   and number of items.  (The latter can cause problems with
   reading/writing large blocks, but not in R.)  POSIX introduced
   off_t and fseeko/ftello to allow larger file sizes, since 'long'
   may limit file positioning to 2GB.  (C99 introduced fpos_t and
   f[gs]etpos.)

   Note that the issue really only arises if 'long' is 32-bit, which
   is not the case on all known 64-bit platforms except Windows.
   However, off_t (defined in sys/types.h) is itself often 32-bit,
   which has led to workarounds.  On Linux systems, the macros

   __USE_FILE_OFFSET64
   __USE_LARGEFILE64

   select between __off_t and __off64_t.  Since these are different
   types, the functions using them have to be remapped, and the
   __off64_t versions call fopen64, fseeko64, ftello64 and so on.

   These macros are not intended to be used directly but via (features.h)

   _FILE_OFFSET_BITS=N  Select default filesystem interface.
   _LARGEFILE_SOURCE    Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE  Additional functionality from LFS for large files.

   The last makes system calls like open64 visible directly, and so
   should not be needed in R.

   This is commonly known as LFS; _but_ 'LFS Linux' is something else.
   See http://en.wikipedia.org/wiki/Large_file_support and
   http://www.suse.de/~aj/linux_lfs.html

   Solaris has a similar scheme: see 'man lf64', 'man lfcompile' and
   'man lfcompile64'.

   On macOS, off_t is typedef-ed to __darwin_off_t, which is
   __int64_t, so the issue never arises.  Similarly on FreeBSD.

   The situation with Windows is similar, but off64_t, fseeko64 etc
   need to be selected explicitly (even on Win64).

   There are also issues with the glob(), readdir(), stat() system
   calls: see platform.c and sysutils.c

   saveload.c uses f[gs]etpos: they have 64-bit versions on LFS Linux
   and Solaris.  But this only used for pre-1.4.0 formats, and fpos_t
   is 64-bit on Windows.
*/

/*
  NCONNECTIONS.  Prior to R 4.4.0 this was set to 128 in this file.
  There was concern that file-like connections use file descriptions
  which have a quite low default limit (256 on macOS, 1024 on Linux),
  and are needed for other uses including loading DLLs.  (Parallel
  clusters use a file connection per cluster member.)  Windows is said
  to have a default limit of 512 simultaneously open files at stream
  I/O level.

  As from R 4.4.0 the default limit remains 128, but can be overriden
  by the startup option --max-connections.  This does not allow it to
  be set below 128 but has a limit of 4096 (see CommandLineArgs.c).
  The upper limit is conservative, but as creating a new connection
  uses a linear search in the connections table, some limit is
  needed. (When checked the table used 488 bytes per connection.)

  Using a dynamic upper limit would not be hard, but not very useful
  because of the non-dynamic fd limit.

  The current implementation of socket connections uses select(). On
  POSIX systems, only FD_SETSIZE descriptors are supported and they 
  must have numbers between 0 and FD_SETSIZE-1, inclusive. On Linux and macOS,
  FD_SETSIZE is normally 1024. On macOS, the limit could be overcome via
  _DARWIN_UNLIMITED_SELECT (not used by R), but a POSIX solution would
  be to use poll() instead of select(). On Windows, by default 1024 different
  descriptors are supported in a single select() call, but these include
  valid socket file descriptors of arbitrary numbers, much larger
  than FD_SETSIZE.  On Windows, the limit can be set in the program
  by setting FD_SETSIZE before including WinSock headers, R sets it to
  1024 (in sock.h and here in connections.c).
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h> // for size_t
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <R_ext/Complex.h>
#include <R_ext/RS.h>		/* R_chk_calloc and R_Free */
#include <R_ext/Riconv.h>
#include <R_ext/Print.h> // REprintf, REvprintf
#undef ERROR			/* for compilation on Windows */

#ifdef Win32
#include <trioremap.h>
#endif

attribute_hidden int R_OutputCon; /* used in printutils.c */

static void con_destroy(int i);

#include <errno.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
/* Solaris and AIX define open as open64 under some circumstances */
# undef open
/* AIX defines truncate as truncate64 under some circumstances */
# undef truncate
#endif

#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

/* This works on Win64 where long is 4 bytes but long long is 8 bytes. */
#if defined __GNUC__ && __GNUC__ >= 2
__extension__ typedef long long int _lli_t;
#else
typedef long long int _lli_t;
#endif

/* Win32 does have popen, but it does not work in GUI applications,
   so test that later */
#ifdef Win32
# include <Startup.h>
#endif

static int NCONNECTIONS = 128; /* need one per cluster node */
//static Rconnection Connections[NCONNECTIONS];
#define NSINKS 21

#include <R_ext/RStartup.h>
static Rconnection * Connections;
static SEXP OutTextData;

static int R_SinkNumber;
static int SinkCons[NSINKS], SinkConsClose[NSINKS], R_SinkSplit[NSINKS];

/* We need a unique id for a connection to ensure that the finalizer
   does not try to close it after it is already closed.  And that id
   will be passed as a pointer, so it seemed easiest to use void *.
*/
static void * current_id = NULL;

/* ------------- admin functions (see also at end) ----------------- */

static int NextConnection(void)
{
    int i;
    for(i = 3; i < NCONNECTIONS; i++)
	if(!Connections[i]) break;
    if(i >= NCONNECTIONS) {
	R_gc(); /* Try to reclaim unused ones */
	for(i = 3; i < NCONNECTIONS; i++)
	    if(!Connections[i]) break;
	/* To make this dynamic, realloc Connections and set the new
	   members to NULL. */
	if(i >= NCONNECTIONS)
	    error(_("all %d connections are in use"), NCONNECTIONS);
    }
    return i;
}

static int ConnIndex(Rconnection con)
{
    int i;
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i] == con) break;
    if(i >= NCONNECTIONS)
	error(_("connection not found"));
    return i;
}

/* internal, not the same as R function getConnection */
Rconnection getConnection(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	error(_("invalid connection"));
    return con;

}

attribute_hidden
int getActiveSink(int n)
{
    if (n >= R_SinkNumber || n < 0)
	return 0;
    if (R_SinkSplit[R_SinkNumber - n])
	return SinkCons[R_SinkNumber - n - 1];
    else
	return 0;
}

static void conFinalizer(SEXP ptr)
{
    int i, ncon = 0;
    void *cptr = R_ExternalPtrAddr(ptr);

    if(!cptr) return;

    for(i = 3; i < NCONNECTIONS; i++)
	if(Connections[i] && Connections[i]->id == cptr) {
	    ncon = i;
	    break;
	}
    if(i >= NCONNECTIONS) return;

    char buf[R_PATH_MAX + 50]; /* much longer than limit for warning */
    Rboolean warn = FALSE;
    {
	Rconnection this = getConnection(ncon);
	if(this->isopen && strcmp(this->class, "textConnection")) {
	    warn = TRUE;
	    snprintf(buf, sizeof(buf),
	             _("closing unused connection %d (%s)\n"),
	             ncon, this->description);
	}
    }

    con_destroy(ncon);
    R_ClearExternalPtr(ptr); /* not really needed */

    if (warn)
	warning("%s", buf); /* may be turned into error */
}


/* for use in REvprintf */
attribute_hidden
Rconnection getConnection_no_err(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	return NULL;
    return con;

}

NORET static void set_iconv_error(Rconnection con, char* from, char* to)
{
    char buf[100];
    snprintf(buf, 100, _("unsupported conversion from '%s' to '%s'"), from, to);
    con_destroy(ConnIndex(con));
    error("%s", buf);
}

/* ------------------- buffering --------------------- */

#define RBUFFCON_LEN_DEFAULT 4096

# define MAX(a, b) ((a) > (b) ? (a) : (b))
# define MIN(a, b) ((a) > (b) ? (b) : (a))

static size_t buff_set_len(Rconnection con, size_t len) {
    size_t unread_len = 0;
    unsigned char *buff;

    if (con->buff_len == len)
	return len;

    if (con->buff) {
	unread_len = con->buff_stored_len - con->buff_pos;
	len = MAX(len, unread_len);
    }

    buff = (unsigned char *)malloc(sizeof(unsigned char) * len);

    if (con->buff) {
	if (unread_len)
	    memcpy(buff, con->buff + con->buff_pos, unread_len);
	free(con->buff);
    }

    con->buff = buff;
    con->buff_len = len;
    con->buff_pos = 0;
    con->buff_stored_len = unread_len;

    return len;
}

static void buff_init(Rconnection con)
{
    con->buff_pos = con->buff_stored_len = 0;
    buff_set_len(con, RBUFFCON_LEN_DEFAULT);
}

static void buff_reset(Rconnection con) {
    size_t unread_len = con->buff_stored_len - con->buff_pos;

    if (unread_len > 0)
	memmove(con->buff, con->buff + con->buff_pos, unread_len);

    con->buff_pos = 0;
    con->buff_stored_len = unread_len;
}

static size_t buff_fill(Rconnection con) {
    size_t free_len, read_len;

    buff_reset(con);

    free_len = con->buff_len - con->buff_stored_len;
    read_len = con->read(con->buff, sizeof(unsigned char), free_len, con);
    if ((int)read_len < 0)
	error("error reading from the connection");
    con->buff_stored_len += read_len;

    return read_len;
}

static int buff_fgetc(Rconnection con)
{
    size_t unread_len;

    unread_len = con->buff_stored_len - con->buff_pos;
    if (unread_len == 0) {
	size_t filled_len = buff_fill(con);
	if (filled_len == 0)
	    return R_EOF;
    }

    return con->buff[con->buff_pos++];
}

static double buff_seek(Rconnection con, double where, int origin, int rw)
{
    size_t unread_len = con->buff_stored_len - con->buff_pos;

    if (rw == 2) /* write */
	return con->seek(con, where, origin, rw);

    if (ISNA(where)) /* tell */
	return con->seek(con, where, origin, rw) - unread_len;

    if (origin == 2) { /* current */
	if (where < unread_len) {
	    con->buff_pos += (size_t) where;
	    return con->seek(con, NA_REAL, origin, rw);
	} else {
	    where -= unread_len;
	}
    }
    con->buff_pos = con->buff_stored_len = 0;

    return con->seek(con, where, origin, rw);
}

static void set_buffer(Rconnection con) {
    if (con->canread && con->text) {
	buff_init(con);
    }
}

void set_iconv(Rconnection con)
{
    void *tmp;

    /* need to test if this is text, open for reading to writing or both,
       and set inconv and/or outconv */
    if(!con->text || !strlen(con->encname) ||
       strcmp(con->encname, "native.enc") == 0) {
	con->UTF8out = FALSE;
	return;
    }
    if(con->canread) {
	size_t onb = 50;
	char *ob = con->oconvbuff;
	/* UTF8out is set in readLines() and scan()
	   Was Windows-only until 2.12.0, but we now require iconv.
	 */
	Rboolean useUTF8 = !utf8locale && con->UTF8out;
	const char *enc =
	    streql(con->encname, "UTF-8-BOM") ? "UTF-8" : con->encname;
	tmp = Riconv_open(useUTF8 ? "UTF-8" : "", enc);
	if(tmp != (void *)-1) con->inconv = tmp;
	else set_iconv_error(con, con->encname, useUTF8 ? "UTF-8" : "");
	con->EOF_signalled = FALSE;
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, NULL, NULL, &ob, &onb);
	con->navail = (short)(50-onb); con->inavail = 0;
	/* libiconv can handle BOM marks on Windows Unicode files, but
	   glibc's iconv cannot. Aargh ... */
	if(streql(con->encname, "UCS-2LE") ||
	   streql(con->encname, "UTF-16LE")) con->inavail = -2;
	/* Discard BOM */
	if(streql(con->encname, "UTF-8-BOM")) con->inavail = -3;
    }
    if(con->canwrite) {
	size_t onb = 25;
	char *ob = con->init_out;
	tmp = Riconv_open(con->encname, "");
	if(tmp != (void *)-1) con->outconv = tmp;
	else set_iconv_error(con, con->encname, "");
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, NULL, NULL, &ob, &onb);
	ob[25-onb] = '\0';
    }
}


/* ------------------- null connection functions --------------------- */

NORET static Rboolean null_open(Rconnection con)
{
    error(_("%s not enabled for this connection"), "open");
}

static void null_close(Rconnection con)
{
    con->isopen = FALSE;
}

static void null_destroy(Rconnection con)
{
    if(con->private) free(con->private);
}

NORET static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error(_("%s not enabled for this connection"), "printing");
}

/* va_copy is C99, but a draft standard had __va_copy.  Glibc has
   __va_copy declared unconditionally */


#if defined(HAVE_VASPRINTF) && !HAVE_DECL_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap);
#endif

# define BUFSIZE 10000
// similar to Rcons_vprintf in printutils.c
int dummy_vfprintf(Rconnection con, const char *format, va_list ap)
{
    R_CheckStack2(BUFSIZE); // prudence
    char buf[BUFSIZE], *b = buf;
    int res;
    const void *vmax = NULL; /* -Wall*/
    int usedVasprintf = FALSE;
    va_list aq;

    va_copy(aq, ap);
    res = Rvsnprintf_mbcs(buf, BUFSIZE, format, aq);
    va_end(aq);
#ifdef HAVE_VASPRINTF
    if(res >= BUFSIZE || res < 0) {
	res = vasprintf(&b, format, ap);
	if (res < 0) {
	    b = buf;
	    warning(_("printing of extremely long output is truncated"));
	    res = (int)strlen(buf);
	} else usedVasprintf = TRUE;
    }
#else
    if(res >= BUFSIZE) { /* res is the desired output length */
	vmax = vmaxget();
	/* apparently some implementations count short,
	   <http://unixpapa.com/incnote/stdio.html>
	   so add some margin here */
	b = R_alloc(res + 101, sizeof(char));
	vsnprintf(b, res + 100, format, ap);
    } else if(res < 0) {
	/* Some non-C99 conforming vsnprintf implementations return -1 on
	   truncation instead of only on error. */
	vmax = vmaxget();
	b = R_alloc(10*BUFSIZE, sizeof(char));
	res = Rvsnprintf_mbcs(b, 10*BUFSIZE, format, ap);
	if (res < 0 || res >= 10*BUFSIZE) {
	    warning(_("printing of extremely long output is truncated"));
	    res = (int)strlen(b);
	}
    }
#endif /* HAVE_VASPRINTF */
    if(con->outconv) { /* translate the buffer */
	char outbuf[BUFSIZE+1], *ob;
	const char *ib = b;
	size_t inb = res, onb, ires;
	Rboolean again = FALSE;
	size_t ninit = strlen(con->init_out);
	do {
	    onb = BUFSIZE; /* leave space for nul */
	    ob = outbuf;
	    if(ninit) {
		strcpy(ob, con->init_out);
		ob += ninit; onb -= ninit; ninit = 0;
	    }
	    errno = 0;
	    ires = Riconv(con->outconv, &ib, &inb, &ob, &onb);
	    again = (ires == (size_t)(-1) && errno == E2BIG);
	    if(ires == (size_t)(-1) && errno != E2BIG) {
		Riconv(con->outconv, NULL, NULL, NULL, NULL);
		/* is this safe? */
		warning(_("invalid char string in output conversion"));
	    }
	    *ob = '\0';
	    con->write(outbuf, 1, ob - outbuf, con);
	} while(again && inb > 0);  /* it seems some iconv signal -1 on
				       zero-length input */
    } else
	con->write(b, 1, res, con);
    if(vmax) vmaxset(vmax);
    if(usedVasprintf) free(b);
    return res;
}

int dummy_fgetc(Rconnection con)
{
    if(con->inconv) {
	while(con->navail <= 0) {
	    unsigned int i, inew = 0;
	    char *p, *ob;
	    const char *ib;
	    size_t inb, onb, res;
	    Rboolean checkBOM = FALSE, checkBOM8 = FALSE;

	    if(con->EOF_signalled) return R_EOF;
	    if (con->inavail < 0) {
		switch(con->inavail) {
		case -2:
		    con->inavail = 0;
		    checkBOM = TRUE;
		    break;
		case -3:
		    con->inavail = 0;
		    checkBOM8 = TRUE;
		    break;
		case -21:
		    con->inavail = 1;
		    checkBOM = TRUE;
		    break;
		case -31:
		    con->inavail = 1;
		    checkBOM8 = TRUE;
		    break;
		case -32:
		    con->inavail = 2;
		    checkBOM8 = TRUE;
		    break;
		}
	    }
	    p = con->iconvbuff + con->inavail;
	    for(i = con->inavail; i < 25; i++) {
		int c = (con->buff)
		    ? buff_fgetc(con)
		    : con->fgetc_internal(con);
		if(c == R_EOF)
		    /* Do not set EOF_signalled, because subsequent reads from
		       a non-blocking connections may succeed (PR18555). */
		    break;
		*p++ = (char) c;
		con->inavail++;
		inew++;
		if(!con->buff && (c == '\n' || c == '\r'))
		    /* Possibly a line separator: better stop filling in the
		       encoding conversion buffer if not buffering the input
		       anyway, as not to confuse interactive applications
		       (PR17634).
		    */
		    break;
	    }
	    if (checkBOM || checkBOM8) {
		/* Handle the case of partial BOMs, e.g. in non-blocking
		   connections, when we do not have enough data to tell
		   whether there is a BOM or not.  Indicate by negative
		   con->inavail which BOM still needs to be checked and
		   how many bytes were already checked. */
		if(con->inavail == 0) {
		    if (checkBOM)
			con->inavail = -2;
		    else if (checkBOM8)
			con->inavail = -3;
		    return R_EOF;
		}
		if (con->inavail == 1) {
		    if (checkBOM && (((int)con->iconvbuff[0] & 0xff) == 255)) {
			con->inavail = -21;
			return R_EOF;
		    }
		    if (checkBOM8 && con->iconvbuff[0] == '\xef') {
			con->inavail = -31;
			return R_EOF;
		    }
		}
		if (con->inavail == 2 && checkBOM8 &&
		    con->iconvbuff[1] == '\xbb') {

		    con->inavail = -32;
		    return R_EOF;
		}
	    }
	    if(inew == 0) return R_EOF;
	    if(checkBOM && con->inavail >= 2 &&
	       ((int)con->iconvbuff[0] & 0xff) == 255 &&
	       ((int)con->iconvbuff[1] & 0xff) == 254) {
		con->inavail -= (short) 2;
		memmove(con->iconvbuff, con->iconvbuff+2, con->inavail);
	    }
	    if(checkBOM8 && con->inavail >= 3 &&
	       !memcmp(con->iconvbuff, "\xef\xbb\xbf", 3)) {
		con->inavail -= (short) 3;
		memmove(con->iconvbuff, con->iconvbuff+3, con->inavail);
	    }
	    ib = con->iconvbuff; inb = con->inavail;
	    ob = con->oconvbuff; onb = 50;
	    errno = 0;
	    res = Riconv(con->inconv, &ib, &inb, &ob, &onb);
	    con->inavail = (short) inb;
	    con->next = con->oconvbuff;
	    con->navail = (short)(50 - onb);
	    if(res == (size_t)-1) { /* an error condition */
		if(errno == EINVAL || errno == E2BIG) {
		    /* incomplete input char or no space in output buffer */
		    memmove(con->iconvbuff, ib, inb);
		} else {/*  EILSEQ invalid input */
		    Riconv(con->inconv, NULL, NULL, NULL, NULL);
		    warning(_("invalid input found on input connection '%s'"),
			    con->description);
		    con->inavail = 0;
		    /* Set to prevent reading any more bytes from input,
		       possibly those following the invalid bytes currently
		       encountered. */
		    con->EOF_signalled = TRUE;
		}
	    }
	}
	con->navail--;
	/* the cast prevents sign extension of 0xFF to -1 (R_EOF) */
	return (unsigned char)*con->next++;
    } else if (con->buff)
	return buff_fgetc(con);
    else
	return con->fgetc_internal(con);
}

NORET static int null_fgetc(Rconnection con)
{
    error(_("%s not enabled for this connection"), "'getc'");
}

NORET static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("%s not enabled for this connection"), "'seek'");
}

NORET static void null_truncate(Rconnection con)
{
    error(_("%s not enabled for this connection"), "truncation");
}

static int null_fflush(Rconnection con)
{
    return 0;
}

NORET static size_t null_read(void *ptr, size_t size, size_t nitems,
			      Rconnection con)
{
    error(_("%s not enabled for this connection"), "'read'");
}

NORET static size_t null_write(const void *ptr, size_t size, size_t nitems,
			       Rconnection con)
{
    error(_("%s not enabled for this connection"), "'write'");
}

void init_con(Rconnection new, const char *description, int enc,
	      const char * const mode)
{
    strcpy(new->description, description);
    new->enc = enc;
    strncpy(new->mode, mode, 4); new->mode[4] = '\0';
    new->isopen = new->incomplete = new->blocking = new->isGzcon = FALSE;
    new->canread = new->canwrite = TRUE; /* in principle */
    new->canseek = FALSE;
    new->text = TRUE;
    new->open = &null_open;
    new->close = &null_close;
    new->destroy = &null_destroy;
    new->vfprintf = &null_vfprintf;
    new->fgetc = new->fgetc_internal = &null_fgetc;
    new->seek = &null_seek;
    new->truncate = &null_truncate;
    new->fflush = &null_fflush;
    new->read = &null_read;
    new->write = &null_write;
    new->nPushBack = 0;
    new->save = new->save2 = -1000;
    new->private = NULL;
    new->inconv = new->outconv = NULL;
    new->UTF8out = FALSE;
    new->buff = NULL;
    new->buff_pos = new->buff_stored_len = new->buff_len = 0;
    /* increment id, avoid NULL */
    current_id = (void *)((size_t) current_id+1);
    if(!current_id) current_id = (void *) 1;
    new->id = current_id;
    new->ex_ptr = NULL;
    new->status = NA_INTEGER;
}

/* ------------------- file connections --------------------- */

#ifdef Win32
# define f_seek fseeko64
# define f_tell ftello64
# define OFF_T off64_t
#elif defined(HAVE_OFF_T) && defined(HAVE_FSEEKO)
# define f_seek fseeko
# define f_tell ftello
# define OFF_T off_t
#else
# define f_seek fseek
# define f_tell ftell
# define OFF_T long
#endif

#ifdef Win32
size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);
#endif

typedef struct fileconn {
    FILE *fp;
    OFF_T rpos, wpos;
    Rboolean last_was_write;
    Rboolean raw;
#ifdef Win32
    Rboolean anon_file;
    Rboolean use_fgetwc;
    Rboolean have_wcbuffered;
    char wcbuf;
    char *name;
#endif
} *Rfileconn;

static Rboolean shouldBuffer(int fd) {
#ifdef HAVE_SYS_STAT_H
    struct stat sb;
    int err = fstat(fd, &sb);
    return err ? FALSE : S_ISREG(sb.st_mode);
#else
    return FALSE;
#endif
}

/* returns FALSE on error */
static Rboolean isDir(FILE *fd)
{
#ifdef HAVE_SYS_STAT_H
    struct stat sb;
    int err = fstat(fileno(fd), &sb);
    return err ? FALSE : S_ISDIR(sb.st_mode);
#else
    return FALSE;
#endif
}

/* returns FALSE on error */
attribute_hidden Rboolean R_IsDirPath(const char *path)
{
#ifdef HAVE_SYS_STAT_H
# ifdef Win32
    /* to support > 2GB files */
    struct _stati64 sb;
    if (!_stati64(path, &sb) && (sb.st_mode & S_IFDIR))
	return TRUE;
    /* This is FALSE for D: */
# else
    struct stat sb;
    if (!stat(path, &sb) && S_ISDIR(sb.st_mode))
	return TRUE;
# endif
#endif
    return FALSE;
}

static Rboolean file_open(Rconnection con)
{
    const char *name;
    FILE *fp = NULL;
    Rfileconn this = con->private;
    Rboolean temp = FALSE;
#ifdef HAVE_FCNTL
    int fd, flags;
#endif
    int mlen = (int) strlen(con->mode); // short

    if(strlen(con->description) == 0) {
	temp = TRUE;
	name = R_tmpnam("Rf", R_TempDir);
    } else name = R_ExpandFileName(con->description);
    errno = 0; /* some systems require this */
    if(strcmp(name, "stdin")) {
#ifdef Win32
	char mode[20]; /* 4 byte mode plus "t,ccs=UTF-16LE" plus one for luck. */
	strncpy(mode, con->mode, 4);
	mode[4] = '\0';
	if (!strpbrk(mode, "bt"))
	    strcat(mode, "t");
	// See PR#16737, https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/fopen-wfopen?view=vs-2019
	// ccs= is also supported by glibc but not macOS
	if (strchr(mode, 't')
	    && (!strcmp(con->encname, "UTF-16LE") || !strcmp(con->encname, "UCS-2LE"))) {
	    strcat(mode, ",ccs=UTF-16LE");
	    if (con->canread) {
	    	this->use_fgetwc = TRUE;
	    	this->have_wcbuffered = FALSE;
	    }
	}
	if(con->enc == CE_UTF8) {
	    int n = strlen(name);
	    wchar_t wname[2 * (n+1)], wmode[20];
	    mbstowcs(wmode, mode, 19);
	    R_CheckStack();
	    Rf_utf8towcs(wname, name, n+1);
	    fp = _wfopen(wname, wmode);
	    if(!fp) {
		if (temp)
		    free((char *)name);
		warning(_("cannot open file '%ls': %s"), wname, strerror(errno));
		return FALSE;
	    }
	    if (isDir(fp)) {
		fclose(fp);
		if (temp)
		    free((char *)name);
		warning(_("cannot open file '%ls': it is a directory"), wname);
		return FALSE;
	    }
	} else {
	    fp = R_fopen(name, mode);
	}
#else
	fp = R_fopen(name, con->mode);
#endif
    } else {  /* use file("stdin") to refer to the file and not the console */
#ifdef HAVE_FDOPEN
	int dstdin = dup(0);
# ifdef Win32
	if (strchr(con->mode, 'b'))
	    /* fdopen won't set dstdin to binary mode */
	    setmode(dstdin, _O_BINARY);
# endif
        fp = fdopen(dstdin, con->mode);
	con->canseek = FALSE;
#else
	warning(_("cannot open file '%s': %s"), name,
		"fdopen is not supported on this platform");
#endif
    }
    if(!fp) {
	warning(_("cannot open file '%s': %s"), name, strerror(errno));
	if (temp)
	    free((char *)name);
	return FALSE;
    }
    if (isDir(fp)) {
	fclose(fp);
	warning(_("cannot open file '%s': it is a directory"), name);
	if (temp)
	    free((char *)name);
	return FALSE;
    }
    if(temp) {
	/* This will fail on Windows, so arrange to remove in
	 * file_close.  An alternative strategy would be to manipulate
	 * the underlying file handle to add FILE_SHARE_DELETE (so the
	 * unlink is valid) or FILE_FLAG_DELETE_ON_CLOSE.  E.g. create
	 * via CreateFile, get an fd by _open_osfhandle and a file
	 * stream by fdopen.  See
	 * e.g. http://www.codeproject.com/KB/files/handles.aspx
	 *
	 * unlink(name);
	 */
#ifdef Win32
	this->name = (char *)name; /* malloc'd in R_tmpnam */
#else
	unlink(name);
	free((char *) name); /* only free if allocated by R_tmpnam */
#endif
    }
#ifdef Win32
    this->anon_file = temp;
#endif
    this->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(mlen >= 2 && con->mode[1] == '+')
	con->canread = con->canwrite = TRUE;
    this->last_was_write = !con->canread;
    this->rpos = 0;
    if(con->canwrite) this->wpos = f_tell(fp);
    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    if (shouldBuffer(fileno(fp)))
	set_buffer(con);
    set_iconv(con);

#ifdef HAVE_FCNTL
    if(!con->blocking) {
	fd = fileno(fp);
	flags = fcntl(fd, F_GETFL);
	flags |= O_NONBLOCK;
	fcntl(fd, F_SETFL, flags);
    }
#endif
    return TRUE;
}

static void file_close(Rconnection con)
{
    Rfileconn this = con->private;
    if(con->isopen) // && strcmp(con->description, "stdin"))
	con->status = fclose(this->fp);
    con->isopen = FALSE;
#ifdef Win32
    if(this->anon_file && this->name) {
	unlink(this->name);
	free(this->name);
	this->name = NULL;
    }
#endif
}

static int file_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Rfileconn this = con->private;

    if(!this->last_was_write) {
	this->rpos = f_tell(this->fp);
	this->last_was_write = TRUE;
	f_seek(this->fp, this->wpos, SEEK_SET);
    }
    if(con->outconv) return dummy_vfprintf(con, format, ap);
    else return vfprintf(this->fp, format, ap);
}

static int file_fgetc_internal(Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
    int c;

    if(this->last_was_write) {
	this->wpos = f_tell(this->fp);
	this->last_was_write = FALSE;
	f_seek(this->fp, this->rpos, SEEK_SET);
    }
#ifdef Win32
    if (this->use_fgetwc) {
    	if (this->have_wcbuffered) {
    	    c = this->wcbuf;
    	    this->have_wcbuffered = FALSE;
    	} else {
    	    wint_t wc = fgetwc(fp);
    	    c = (char) wc & 0xFF;
    	    this->wcbuf = (char) wc >> 8;
    	    this->have_wcbuffered = TRUE;
    	}
    } else
#endif
    c = fgetc(fp);
    if (c == EOF && feof(fp)) {
	/* Clear the end-of-file indicator on the stream so that additional
	   data, if appended to the file, may be read by subsequent calls.
	   This is needed according to the C standard and, at the time of this
	   writing, required in practice on macOS. */
	clearerr(fp);
	return R_EOF;
     } else
	return c;
}

static double file_seek(Rconnection con, double where, int origin, int rw)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
    OFF_T pos;
    int whence = SEEK_SET;

    /* make sure both positions are set */
    pos = f_tell(fp);
    if(this->last_was_write) this->wpos = pos; else this->rpos = pos;
    if(rw == 1) {
	if(!con->canread) error(_("connection is not open for reading"));
	pos = this->rpos;
	this->last_was_write = FALSE;
    }
    if(rw == 2) {
	if(!con->canwrite) error(_("connection is not open for writing"));
	pos = this->wpos;
	this->last_was_write = TRUE;
    }
    if(ISNA(where)) return (double) pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: whence = SEEK_END;
//#ifdef Win32
	    /* work around a bug in MinGW runtime 3.8 fseeko64, PR#7896
	       seems no longer to be needed */
//	    if(con->canwrite) fflush(fp);
//#endif
	    break;
    default: whence = SEEK_SET;
    }
    f_seek(fp, (OFF_T) where, whence);
    if(this->last_was_write) this->wpos = f_tell(this->fp);
    else this->rpos = f_tell(this->fp);
    return (double) pos;
}

static void file_truncate(Rconnection con)
{
    Rfileconn this = con->private;
#ifdef HAVE_FTRUNCATE
    FILE *fp = this->fp;
    int fd = fileno(fp);
/* ftruncate64 is in Mingw-64 trunk, but not in current toolkit */
# ifdef W64_to_come
    off64_t size = lseek64(fd, 0, SEEK_CUR);
# else
    OFF_T size = lseek(fd, 0, SEEK_CUR);
# endif
#endif

    if(!con->isopen || !con->canwrite)
	error(_("can only truncate connections open for writing"));

    if(!this->last_was_write) this->rpos = f_tell(this->fp);
#ifdef W64_to_come
    if(ftruncate64(fd, size)) error(_("file truncation failed"));
#elif defined(HAVE_FTRUNCATE)
    if(ftruncate(fd, size)) error(_("file truncation failed"));
#else
    error(_("file truncation unavailable on this platform"));
#endif
    this->last_was_write = TRUE;
    this->wpos = f_tell(this->fp);
}

static int file_fflush(Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;

    return fflush(fp);
}

static size_t file_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;

    if(this->last_was_write) {
	this->wpos = f_tell(this->fp);
	this->last_was_write = FALSE;
	f_seek(this->fp, this->rpos, SEEK_SET);
    }
    size_t res = fread(ptr, size, nitems, fp);
    if (res != nitems && feof(fp))
	/* see comment in file_fgetc_internal */
	clearerr(fp);
    return res;
}

static size_t file_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;

    if(!this->last_was_write) {
	this->rpos = f_tell(this->fp);
	this->last_was_write = TRUE;
	f_seek(this->fp, this->wpos, SEEK_SET);
    }
    return fwrite(ptr, size, nitems, fp);
}

static Rconnection newfile(const char *description, int enc, const char *mode,
			   Rboolean raw)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of file connection failed"));
    new->class = (char *) malloc(strlen("file") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "file");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, enc, mode);
    new->open = &file_open;
    new->close = &file_close;
    new->vfprintf = &file_vfprintf;
    new->fgetc_internal = &file_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &file_seek;
    new->truncate = &file_truncate;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->canseek = (raw == 0);
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of file connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rfileconn)(new->private))->raw = raw;
#ifdef Win32
    ((Rfileconn)(new->private))->use_fgetwc = FALSE;
#endif
    return new;
}

/* file() is now implemented as an op of do_url */


/* ------------------- fifo connections --------------------- */

#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)

#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

# include <errno.h>

typedef struct fifoconn {
    int fd;
} *Rfifoconn;


static Rboolean fifo_open(Rconnection con)
{
    const char *name;
    Rfifoconn this = con->private;
    int fd, flags, res;
    int mlen = (int) strlen(con->mode); // short
    struct stat sb;
    Rboolean temp = FALSE;

    if(strlen(con->description) == 0) {
	temp = TRUE;
	name = R_tmpnam("Rf", R_TempDir);
    } else name = R_ExpandFileName(con->description);
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(mlen >= 2 && con->mode[1] == '+') con->canread = TRUE;

    /* if we are to write, create the fifo if needed */
    if(con->canwrite) {
	res = stat(name, &sb);
	if(res) { /* error, does not exist? */
	    errno = 0;
	    res = mkfifo(name, 00644);
	    if(res) {
		warning(_("cannot create fifo '%s', reason '%s'"), name,
			strerror(errno));
	    }
	    if(res) return FALSE;
	} else {
	    if(!(sb.st_mode & S_IFIFO)) {
		warning(_("'%s' exists but is not a fifo"), name);
		return FALSE;
	    }
	}
    }

    if(con->canread && con->canwrite) flags = O_RDWR;
    else if(con->canread) flags = O_RDONLY;
    else flags = O_WRONLY;
    if(!con->blocking) flags |= O_NONBLOCK;
    if(con->mode[0] == 'a') flags |= O_APPEND;
    errno = 0; /* precaution */
    fd = open(name, flags);
    if(fd < 0) {
	if(errno == ENXIO) warning(_("fifo '%s' is not ready"), name);
	else warning(_("cannot open fifo '%s'"), name);
	return FALSE;
    }
    if(temp) {
	unlink(name);
	free((char *) name); /* allocated by R_tmpnam */
    }

    this->fd = fd;
    con->isopen = TRUE;

    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void fifo_close(Rconnection con)
{
    con->status = close(((Rfifoconn)(con->private))->fd);
    con->isopen = FALSE;
}

static int fifo_fgetc_internal(Rconnection con)
{
    Rfifoconn this = con->private;
    unsigned char c;
    ssize_t n;

    n = read(this->fd, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t fifo_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rfifoconn this = con->private;

    /* uses 'size_t' for len */
    if ((double) size * (double) nitems > (double) SSIZE_MAX)
	error(_("too large a block specified"));
    return read(this->fd, ptr, size * nitems)/size;
}

static size_t fifo_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfifoconn this = con->private;

    /* uses 'size_t' for len */
    if ((double) size * (double) nitems > (double) SSIZE_MAX)
	error(_("too large a block specified"));
    return write(this->fd, ptr, size * nitems)/size;
}

#elif defined(Win32)  // ----- Windows part ------

// PR#15600, based on https://github.com/0xbaadf00d/r-project_win_fifo
# define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <wchar.h>

/* Microsoft addition, not supported in Win XP
errno_t strcat_s(char *strDestination, size_t numberOfElements,
		 const char *strSource);
*/

typedef struct fifoconn
{
    HANDLE hdl_namedpipe;
    LPOVERLAPPED overlapped_write;
} *Rfifoconn;

static char* win_getlasterror_str(void)
{
    LPVOID lpv_tempmsg = NULL;
    unsigned int err_msg_len;
    char *err_msg = NULL;

    err_msg_len =
	FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
		      FORMAT_MESSAGE_FROM_SYSTEM |
		      FORMAT_MESSAGE_IGNORE_INSERTS, NULL, GetLastError(),
		      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		      (LPTSTR)&lpv_tempmsg, 0, NULL);
    err_msg = (char*) malloc(err_msg_len);
    if (!err_msg) return NULL;
    ZeroMemory(err_msg, err_msg_len);
    strncpy(err_msg, (LPTSTR)lpv_tempmsg, err_msg_len - sizeof(wchar_t));
    LocalFree(lpv_tempmsg);
    return err_msg;
}

static Rboolean	fifo_open(Rconnection con)
{
    Rfifoconn this = con->private;
    unsigned int uin_mode_len = strlen(con->mode);
    char *hch_pipename = NULL;
    Rboolean boo_retvalue = TRUE;
    const char *pipe_prefix = "\\\\.\\pipe\\";

    /* Prepare FIFO filename */
    if (strlen(con->description) == 0) 
	hch_pipename = R_tmpnam("fifo", pipe_prefix); /* malloc */
    else {
	const char* hch_tempname = R_ExpandFileName(con->description);
	size_t len = strlen(hch_tempname);
	Rboolean add_prefix = FALSE;
	if (strncmp(pipe_prefix, con->description, strlen(pipe_prefix)) != 0) {
	    len += strlen(pipe_prefix);
	    add_prefix = TRUE;
	}	
	hch_pipename = (char*) malloc(len+1);
	if (!hch_pipename)
	    error(_("allocation of fifo name failed"));
	if (add_prefix) {
	    strcpy(hch_pipename, pipe_prefix);
	    strcat(hch_pipename, hch_tempname);
	} else
	    strcpy(hch_pipename, hch_tempname);
    }

    /* Prepare FIFO open mode */
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if (uin_mode_len >= 2 && con->mode[1] == '+') con->canread = TRUE;

    /*
    ** FIFO using Windows API -> CreateNamedPipe() OR CreateFile()
    ** http://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
    ** http://msdn.microsoft.com/en-us/library/windows/desktop/aa365150(v=vs.85).aspx
    */
    this->hdl_namedpipe = NULL;
    this->overlapped_write = CreateEventA(NULL, TRUE, TRUE, NULL);
    if (con->canwrite) {
	SECURITY_ATTRIBUTES win_namedpipe_secattr = {0};
	win_namedpipe_secattr.nLength = sizeof(SECURITY_ATTRIBUTES);
	win_namedpipe_secattr.lpSecurityDescriptor = NULL;
	win_namedpipe_secattr.bInheritHandle = FALSE;

	this->hdl_namedpipe =
	    CreateNamedPipeA(hch_pipename,
			     (con->canread ? PIPE_ACCESS_DUPLEX :
			      PIPE_ACCESS_OUTBOUND) | FILE_FLAG_OVERLAPPED,
			     PIPE_TYPE_BYTE, PIPE_UNLIMITED_INSTANCES , 0, 0,
			     FILE_FLAG_NO_BUFFERING, &win_namedpipe_secattr);
	if (this->hdl_namedpipe == INVALID_HANDLE_VALUE) {
	    /*
	    ** If GetLastError() return 231 (All pipe instances are busy) == File
	    ** already exist on Unix/Linux...
	    */
	    if (GetLastError() != 231) {
		char *hch_err_msg = win_getlasterror_str();
		warning(_("cannot create fifo '%s', reason '%s'"),
			hch_pipename, hch_err_msg);
		free(hch_err_msg);
		boo_retvalue = FALSE;
	    }
	}
    }

    /* Open existing named pipe */
    if ((boo_retvalue || GetLastError() == 231) &&
	this->hdl_namedpipe <= (HANDLE)(LONG_PTR) 0) {
	DWORD dwo_openmode = 0;
	if (con->canread) dwo_openmode |= GENERIC_READ;
	if (con->canwrite) dwo_openmode |= GENERIC_WRITE;
	this->hdl_namedpipe =
	    CreateFileA(hch_pipename, dwo_openmode,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING,
			FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
			NULL);
	if (this->hdl_namedpipe == INVALID_HANDLE_VALUE) {
	    char *hch_err_msg = win_getlasterror_str();
	    warning(_("cannot open fifo '%s', reason '%s'"),
		    hch_pipename, hch_err_msg);
	    free(hch_err_msg);
	    boo_retvalue = FALSE;
	}
    }

    /* Free malloc-ed variables */
    free(hch_pipename);

    /* Finalize FIFO configuration (only if FIFO is opened/created) */
    if (boo_retvalue && this->hdl_namedpipe) {
	con->isopen = TRUE;
	con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
	set_iconv(con);
	con->save = -1000;
    }

    /* Done */
    return boo_retvalue;
}

static void fifo_close(Rconnection con)
{
    Rfifoconn this = con->private;
    con->isopen = FALSE;
    con->status = CloseHandle(this->hdl_namedpipe) ? 0 : -1;
    if (this->overlapped_write) CloseHandle(this->overlapped_write);
}

static size_t fifo_read(void* ptr, size_t size, size_t nitems, Rconnection con)
{
    Rfifoconn this = con->private;
    size_t read_byte = 0;

    // avoid integer overflow
    if ((double)size * sizeof(wchar_t) * nitems > UINT_MAX)
	error(_("too large a block specified"));

    wchar_t *buffer = (wchar_t*)malloc((size * sizeof(wchar_t)) * nitems);
    if (!buffer) error(_("allocation of fifo buffer failed"));
    ReadFile(this->hdl_namedpipe, buffer,
	     (size * sizeof(wchar_t)) * nitems, (LPDWORD)&read_byte,
	     this->overlapped_write);
    wcstombs(ptr, buffer, read_byte / sizeof(wchar_t));
    free(buffer);
    return (read_byte / sizeof(wchar_t)) / size;
}

static size_t
fifo_write(const void *ptr, size_t size, size_t nitems, Rconnection con)
{
    Rfifoconn this = con->private;
    size_t written_bytes = 0;

    if (size * sizeof(wchar_t) * nitems > UINT_MAX)
	error(_("too large a block specified"));

    /* Wait for a client process to connect */
    ConnectNamedPipe(this->hdl_namedpipe, NULL);

    /* Convert char* to wchar_t* */
    int str_len = size * nitems;
    wchar_t *buffer = malloc((str_len + 1) * sizeof(wchar_t));
    if (!buffer) error(_("allocation of fifo buffer failed"));
    mbstowcs(buffer, (const char*) ptr, str_len);

    /* Write data */
    if (WriteFile(this->hdl_namedpipe, buffer,
		  size * sizeof(wchar_t) * nitems, (LPDWORD) &written_bytes,
		  NULL) == FALSE && GetLastError() != ERROR_IO_PENDING) {
	char *hch_err_msg = win_getlasterror_str();
	warning(_("cannot write FIFO '%s'"), hch_err_msg);
	free(hch_err_msg);
    }

    /* Free data malloc-ed by windows_towchar */
    free(buffer);

    /* Done */
    return written_bytes / nitems;
}

static int fifo_fgetc_internal(Rconnection con)
{
    Rfifoconn  this = con->private;
    DWORD available_bytes = 0;
    DWORD read_byte = 0;
    DWORD len = 1 * sizeof(wchar_t);
    wchar_t c;

    /* Check available bytes on named pipe */
    PeekNamedPipe(this->hdl_namedpipe, NULL, 0, NULL, &available_bytes, NULL);

    /* Read char if available bytes > 0, otherwise, return R_EOF */
    if (available_bytes > 0) {
	ReadFile(this->hdl_namedpipe, &c, len, &read_byte, NULL);
	return (read_byte == len) ? (char) c : R_EOF;
    }
    return R_EOF;
}

#endif // WIN32

static Rconnection newfifo(const char *description, const char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of fifo connection failed"));
    new->class = (char *) malloc(strlen("fifo") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "fifo");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->open = &fifo_open;
    new->close = &fifo_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &fifo_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->truncate = &null_truncate;
    new->fflush = &null_fflush;
    new->read = &fifo_read;
    new->write = &fifo_write;
    new->private = (void *) malloc(sizeof(struct fifoconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of fifo connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    return new;
}

static void cend_con_destroy(void *data)
{
    int ncon = *(int *)data;
    con_destroy(ncon);
} 

static void checked_open(int ncon)
{
    Rconnection con = Connections[ncon];
    RCNTXT cntxt;

    /* Set up a context which will destroy the connection on error,
       including warning turned into error (PR#18491) */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &cend_con_destroy;
    cntxt.cenddata = &ncon;
    Rboolean success = con->open(con);
    endcontext(&cntxt);
    if(!success) {
	con_destroy(ncon);
	error(_("cannot open the connection"));
    }
}

attribute_hidden SEXP do_fifo(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if (defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)) || defined(_WIN32)
    SEXP sfile, sopen, ans, class, enc;
    const char *file, *open;
    int ncon;
    Rboolean block;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateCharFP(STRING_ELT(sfile, 0)); /* for now, like fopen */
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asRbool(CADDR(args), call);
//    if(block == NA_LOGICAL)
//	error(_("invalid '%s' argument"), "block");
    enc = CADDDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strlen(file) == 0) {
	if(!strlen(open)) open ="w+";
	if(strcmp(open, "w+") != 0 && strcmp(open, "w+b") != 0) {
	    open ="w+";
	    warning(_("fifo(\"\") only supports open = \"w+\" and open = \"w+b\": using the former"));
	}
    }
    ncon = NextConnection();
    con = Connections[ncon] = newfifo(file, strlen(open) ? open : "r");
    con->blocking = block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("fifo"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
#else
    error(_("fifo connections are not available on this system"));
    return R_NilValue;		/* -Wall */
#endif
}

/* ------------------- pipe connections --------------------- */

/* This implementation is no longer used on Windows, even in Rterm,
   because of termination of background processes using Ctrl+C (PR#17764). */

#ifndef Win32
static Rboolean pipe_open(Rconnection con)
{
    FILE *fp;
    char mode[3];
    Rfileconn this = con->private;
    int mlen;

#ifdef Win32
    strncpy(mode, con->mode, 2);
    mode[2] = '\0';
#else
    mode[0] = con->mode[0];
    mode[1] = '\0';
#endif
    errno = 0;
#ifdef Win32
    if(con->enc == CE_UTF8) {
	int n = strlen(con->description);
	wchar_t wname[2 * (n+1)], wmode[10];
	R_CheckStack();
	Rf_utf8towcs(wname, con->description, n+1);
	mbstowcs(wmode, con->mode, 10);
	fp = _wpopen(wname, wmode);
	if(!fp) {
	    warning(_("cannot pipe() cmd '%ls': %s"), wname, strerror(errno));
	    return FALSE;
	}
    } else
	/* this implementation is prone to termination of background
	   processes using Ctrl+C (PR#17764) */
	fp = R_popen(con->description, mode);
#else
	fp = R_popen_pg(con->description, mode);
#endif
    if(!fp) {
	warning(_("cannot open pipe() cmd '%s': %s"), con->description,
		strerror(errno));
	return FALSE;
    }
    this->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w');
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen - 1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    this->last_was_write = !con->canread;
    this->rpos = this->wpos = 0;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void pipe_close(Rconnection con)
{
#ifdef Win32
    con->status = pclose(((Rfileconn)(con->private))->fp);
#else
    con->status = R_pclose_pg(((Rfileconn)(con->private))->fp);
#endif
    con->isopen = FALSE;
}

static Rconnection
newpipe(const char *description, int ienc, const char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of pipe connection failed"));
    new->class = (char *) malloc(strlen("pipe") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "pipe");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, ienc, mode);
    new->open = &pipe_open;
    new->close = &pipe_close;
    new->vfprintf = &file_vfprintf;
    new->fgetc_internal = &file_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    return new;
}

#else
extern Rconnection
newWpipe(const char *description, int enc, const char *mode);
#endif

attribute_hidden SEXP do_pipe(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    const char *file, *open;
    int ncon;
    cetype_t ienc = CE_NATIVE;
    Rconnection con = NULL;

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || LENGTH(scmd) != 1 ||
       STRING_ELT(scmd, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(scmd) > 1)
	warning(_("only first element of 'description' argument used"));
#ifdef Win32
    if( !IS_ASCII(STRING_ELT(scmd, 0)) ) {
	ienc = CE_UTF8;
	file = trCharUTF8(STRING_ELT(scmd, 0));
    } else {
	ienc = CE_NATIVE;
	file = translateCharFP(STRING_ELT(scmd, 0));
    }
#else
    file = translateCharFP(STRING_ELT(scmd, 0));
#endif
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    ncon = NextConnection();
#ifdef Win32
    con = newWpipe(file, ienc, strlen(open) ? open : "r");
#else
    con = newpipe(file, ienc, strlen(open) ? open : "r");
#endif
    Connections[ncon] = con;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("pipe"));
#ifdef Win32
    if(CharacterMode != RTerm)
	SET_STRING_ELT(class, 0, mkChar("pipeWin32"));
#endif
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* ------------------- [bgx]zipped file connections --------------------- */
#include "gzio.h"

/* needs to be declared before con_close1 */
typedef struct gzconn {
    Rconnection con;
    int cp; /* compression level */
    z_stream s;
    int z_err, z_eof;
    uLong crc;
    Byte buffer[Z_BUFSIZE];
    int nsaved;
    char saved[2];
    Rboolean allow;
} *Rgzconn;


typedef struct gzfileconn {
    void *fp;
    int compress;
} *Rgzfileconn;

static Rboolean gzfile_open(Rconnection con)
{
    gzFile fp;
    char mode[6];
    Rgzfileconn gzcon = con->private;
    const char *name;

    strcpy(mode, con->mode);
    /* Must open as binary */
    if(strchr(con->mode, 'w')) snprintf(mode, 6, "wb%1d", gzcon->compress);
    else if (con->mode[0] == 'a') snprintf(mode, 6, "ab%1d", gzcon->compress);
    else strcpy(mode, "rb");
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    /* We cannot use isDir, because we cannot get the fd from gzFile
       (it would be possible with gzdopen, if supported) */
    if (R_IsDirPath(name)) {
	warning(_("cannot open file '%s': it is a directory"), name);
	return FALSE;
    }
    fp = R_gzopen(name, mode);
    if(!fp) {
	warning(_("cannot open compressed file '%s', probable reason '%s'"),
	        name, strerror(errno));
	return FALSE;
    }
    ((Rgzfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    set_buffer(con);
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void gzfile_close(Rconnection con)
{
    R_gzclose(((Rgzfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static int gzfile_fgetc_internal(Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    unsigned char c;

    return R_gzread(fp, &c, 1) == 1 ? c : R_EOF;
}

/* This can only seek forwards when writing (when it writes nul bytes).
   When reading, it either seeks forwards or rewinds and reads again */
static double gzfile_seek(Rconnection con, double where, int origin, int rw)
{
    gzFile  fp = ((Rgzfileconn)(con->private))->fp;
    Rz_off_t pos = R_gztell(fp);
    int res, whence = SEEK_SET;

    if (ISNA(where)) return (double) pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: error(_("whence = \"end\" is not implemented for gzfile connections"));
    default: whence = SEEK_SET;
    }
    res = R_gzseek(fp, (z_off_t) where, whence);
    if(res == -1)
	warning(_("seek on a gzfile connection returned an internal error"));
    return (double) pos;
}

static int gzfile_fflush(Rconnection con)
{
    return 0;
}

static size_t gzfile_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    /* uses 'unsigned' for len */
    if ((double) size * (double) nitems > UINT_MAX)
	error(_("too large a block specified"));
    return R_gzread(fp, ptr, (unsigned int)(size*nitems))/size;
}

static size_t gzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    /* uses 'unsigned' for len */
    if ((double) size * (double) nitems > UINT_MAX)
	error(_("too large a block specified"));
    return R_gzwrite(fp, (voidp)ptr, (unsigned int)(size*nitems))/size;
}

static Rconnection newgzfile(const char *description, const char *mode,
			     int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of gzfile connection failed"));
    new->class = (char *) malloc(strlen("gzfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "gzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);

    new->canseek = TRUE;
    new->open = &gzfile_open;
    new->close = &gzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &gzfile_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &gzfile_seek;
    new->fflush = &gzfile_fflush;
    new->read = &gzfile_read;
    new->write = &gzfile_write;
    new->private = (void *) malloc(sizeof(struct gzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of gzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rgzfileconn)new->private)->compress = compress;
    return new;
}

#include <bzlib.h>
typedef struct bzfileconn {
    FILE *fp;
    BZFILE *bfp;
    int compress;
} *Rbzfileconn;

static Rboolean bzfile_open(Rconnection con)
{
    Rbzfileconn bz = (Rbzfileconn) con->private;
    FILE* fp;
    BZFILE* bfp;
    int bzerror;
    char mode[] = "rb";
    const char *name;

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    fp = R_fopen(name, mode);
    if(!fp) {
	warning(_("cannot open bzip2-ed file '%s', probable reason '%s'"),
		name, strerror(errno));
	return FALSE;
    }
    if (isDir(fp)) {
	fclose(fp);
	warning(_("cannot open file '%s': it is a directory"), name);
	return FALSE;
    }
    if(con->canread) {
	bfp = BZ2_bzReadOpen(&bzerror, fp, 0, 0, NULL, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzReadClose(&bzerror, bfp);
	    fclose(fp);
	    warning(_("file '%s' appears not to be compressed by bzip2"),
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    } else {
	bfp = BZ2_bzWriteOpen(&bzerror, fp, bz->compress, 0, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzWriteClose(&bzerror, bfp, 0, NULL, NULL);
	    fclose(fp);
	    warning(_("initializing bzip2 compression for file '%s' failed"),
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    }
    bz->fp = fp;
    bz->bfp = bfp;
    con->isopen = TRUE;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    set_buffer(con);
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void bzfile_close(Rconnection con)
{
    int bzerror;
    Rbzfileconn bz = con->private;

    if(con->canread)
	BZ2_bzReadClose(&bzerror, bz->bfp);
    else
	BZ2_bzWriteClose(&bzerror, bz->bfp, 0, NULL, NULL);
    fclose(bz->fp);
    con->isopen = FALSE;
}

static size_t bzfile_read(void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rbzfileconn bz = con->private;
    int nread = 0,  nleft;
    int bzerror;

    /* BZ2 uses 'int' for len */
    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));

    nleft = (int)(size * nitems);
    /* we try to fill the buffer, because fgetc can interact with the stream boundaries
       resulting in truncated text streams while binary streams work fine */
    while (nleft > 0) {
	/* Need a cast as 'nread' needs to be interpreted in bytes */
	int n = BZ2_bzRead(&bzerror, bz->bfp, (char *)ptr + nread, nleft);
	if (bzerror == BZ_STREAM_END) { /* this could mean multiple streams so we need to check */
	    char *unused, *next_unused = NULL;
	    int nUnused;
	    BZ2_bzReadGetUnused(&bzerror, bz->bfp, (void**) &unused, &nUnused);
	    if (bzerror == BZ_OK) {
		if (nUnused > 0) { /* unused bytes present - need to retain them */
		    /* given that this should be rare I don't want to add that overhead
		       to the entire bz structure so we allocate memory temporarily */
		    next_unused = (char*) malloc(nUnused);
		    if (!next_unused)
			error(_("allocation of overflow buffer for bzfile failed"));
		    memcpy(next_unused, unused, nUnused);
		}
		if (nUnused > 0 || !feof(bz->fp)) {
		    BZ2_bzReadClose(&bzerror, bz->bfp);
		    bz->bfp = BZ2_bzReadOpen(&bzerror, bz->fp, 0, 0, next_unused, nUnused);
		    if(bzerror != BZ_OK)
			warning(_("file '%s' has trailing content that appears not to be compressed by bzip2"),
				R_ExpandFileName(con->description));
		}
		if (next_unused) free(next_unused);
	    }
	} else if (bzerror != BZ_OK) {
	    /* This happens also when the file accidentally starts with BZh,
	       but is not a bzip2 file. (PR#18768) */
	    if (bzerror == BZ_DATA_ERROR_MAGIC)
		/* FIXME: the warning probably should also be displayed for some
		   other errors, but not for BZ_SEQUENCE_ERROR, because that is
		   caused by the code above when the stream ends but there are no
		   "unused" bytes, and another read is attempted. */
		warning(_("file '%s' appears not to be compressed by bzip2"),
		    R_ExpandFileName(con->description));
	    /* bzlib docs say in this case n is invalid - but historically
	       we still used n in that case, so I keep it for now */
	    nread += n;
	    break;
	}
	nread += n;
	nleft -= n;
    }

    return nread / size;
}

static int bzfile_fgetc_internal(Rconnection con)
{
    char buf[1];
    size_t size;

    size = bzfile_read(buf, 1, 1, con);
    return (size < 1) ? R_EOF : (buf[0] % 256);
}

static size_t bzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    Rbzfileconn bz = con->private;
    int bzerror;

    /* uses 'int' for len */
    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));
    BZ2_bzWrite(&bzerror, bz->bfp, (voidp) ptr, (int)(size*nitems));
    if(bzerror != BZ_OK) return 0;
    else return nitems;
}

static Rconnection newbzfile(const char *description, const char *mode,
			     int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of bzfile connection failed"));
    new->class = (char *) malloc(strlen("bzfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "bzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);

    new->canseek = FALSE;
    new->open = &bzfile_open;
    new->close = &bzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &bzfile_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &bzfile_read;
    new->write = &bzfile_write;
    new->private = (void *) malloc(sizeof(struct bzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of bzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rbzfileconn)new->private)->compress = compress;
    return new;
}

#include <lzma.h>

typedef struct xzfileconn {
    FILE *fp;
    lzma_stream stream;
    lzma_action action;
    int compress;
    int type;
    lzma_filter filters[2];
    lzma_options_lzma opt_lzma;
    unsigned char buf[BUFSIZE];
} *Rxzfileconn;

static Rboolean xzfile_open(Rconnection con)
{
    Rxzfileconn xz = con->private;
    lzma_ret ret;
    char mode[] = "rb";
    const char *name;

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    xz->fp = R_fopen(name, mode);
    if(!xz->fp) {
	warning(_("cannot open compressed file '%s', probable reason '%s'"),
		name, strerror(errno));
	return FALSE;
    }
    if (isDir(xz->fp)) {
	fclose(xz->fp);
	warning(_("cannot open file '%s': it is a directory"), name);
	return FALSE;
    }
    if(con->canread) {
	xz->action = LZMA_RUN;
	/* probably about 80Mb is required, but 512Mb seems OK as a limit */
	if (xz->type == 1)
	    ret = lzma_alone_decoder(&xz->stream, 536870912);
	else
	    ret = lzma_stream_decoder(&xz->stream, 536870912,
				      LZMA_CONCATENATED);
	if (ret != LZMA_OK) {
	    warning(_("cannot initialize lzma decoder, error %d"), ret);
	    return FALSE;
	}
	xz->stream.avail_in = 0;
    } else {
	lzma_stream *strm = &xz->stream;
	uint32_t preset_number = abs(xz->compress);
	if(xz->compress < 0) preset_number |= LZMA_PRESET_EXTREME;
	if(lzma_lzma_preset(&xz->opt_lzma, preset_number))
	    error("problem setting presets");
	xz->filters[0].id = LZMA_FILTER_LZMA2;
	xz->filters[0].options = &(xz->opt_lzma);
	xz->filters[1].id = LZMA_VLI_UNKNOWN;

	ret = lzma_stream_encoder(strm, xz->filters, LZMA_CHECK_CRC32);
	if (ret != LZMA_OK) {
	    warning(_("cannot initialize lzma encoder, error %d"), ret);
	    return FALSE;
	}
    }
    con->isopen = TRUE;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    set_buffer(con);
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void xzfile_close(Rconnection con)
{
    Rxzfileconn xz = con->private;

    if(con->canwrite) {
	lzma_ret ret;
	lzma_stream *strm = &(xz->stream);
	size_t nout, res;
	unsigned char buf[BUFSIZE];
	while(1) {
	    strm->avail_out = BUFSIZE; strm->next_out = buf;
	    ret = lzma_code(strm, LZMA_FINISH);
	    nout = BUFSIZE - strm->avail_out;
	    res = fwrite(buf, 1, nout, xz->fp);
	    if (res != nout) error("fwrite error");
	    if (ret != LZMA_OK) break;
	}
    }
    lzma_end(&(xz->stream));
    fclose(xz->fp);
    con->isopen = FALSE;
}

static size_t xzfile_read(void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rxzfileconn xz = con->private;
    lzma_stream *strm = &(xz->stream);
    lzma_ret ret;
    size_t s = size*nitems, have, given = 0;
    unsigned char *p = ptr;

    if (!s) return 0;

    while(1) {
	if (strm->avail_in == 0 && xz->action != LZMA_FINISH) {
	    strm->next_in = xz->buf;
	    strm->avail_in = fread(xz->buf, 1, BUFSIZ, xz->fp);
	    if (feof(xz->fp)) xz->action = LZMA_FINISH;
	}
	strm->avail_out = s; strm->next_out = p;
	ret = lzma_code(strm, xz->action);
	have = s - strm->avail_out;  given += have;
	//printf("available: %d, ready: %d/%d\n", strm->avail_in, given, s);
	if (ret != LZMA_OK) {
	    if (ret != LZMA_STREAM_END) {
		switch(ret) {
		case LZMA_MEM_ERROR:
		case LZMA_MEMLIMIT_ERROR:
		    warning("lzma decoder needed more memory");
		    break;
		case LZMA_FORMAT_ERROR:
		    warning("lzma decoder format error");
		    break;
		case LZMA_DATA_ERROR:
		    warning("lzma decoder corrupt data");
		    break;
		default:
		    warning("lzma decoding result %d", ret);
		}
	    }
	    return given/size;
	}
	s -= have;
	if (!s) return nitems;
	p += have;
    }
}

static int xzfile_fgetc_internal(Rconnection con)
{
    char buf[1];
    size_t size = xzfile_read(buf, 1, 1, con);

    return (size < 1) ? R_EOF : (buf[0] % 256);
}


static size_t xzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    Rxzfileconn xz = con->private;
    lzma_stream *strm = &(xz->stream);
    lzma_ret ret;
    size_t s = size*nitems, nout, res;
    const unsigned char *p = ptr;
    unsigned char buf[BUFSIZE];

    if (!s) return 0;

    strm->avail_in = s;
    strm->next_in = p;
    while(1) {
	strm->avail_out = BUFSIZE; strm->next_out = buf;
	ret = lzma_code(strm, LZMA_RUN);
	if (ret > 1) {
	    switch(ret) {
	    case LZMA_MEM_ERROR:
		warning("lzma encoder needed more memory");
		break;
	    default:
		warning("lzma encoding result %d", ret);
	    }
	    return 0;
	}
	nout = BUFSIZE - strm->avail_out;
	res = fwrite(buf, 1, nout, xz->fp);
	if (res != nout) error("fwrite error");
	if (strm->avail_in == 0) return nitems;
    }
}

static Rconnection
newxzfile(const char *description, const char *mode, int type, int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of xzfile connection failed"));
    new->class = (char *) malloc(strlen("xzfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "xzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);

    new->canseek = FALSE;
    new->open = &xzfile_open;
    new->close = &xzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &xzfile_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &xzfile_read;
    new->write = &xzfile_write;
    new->private = (void *) malloc(sizeof(struct xzfileconn));
    memset(new->private, 0, sizeof(struct xzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of xzfile connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rxzfileconn) new->private)->type = type;
    ((Rxzfileconn) new->private)->compress = compress;
    return new;
}


#ifdef HAVE_ZSTD

#ifndef HAVE_ZSTD_DECOMPRESSBOUND
static unsigned long long ZSTD_decompressBound(const void* src, size_t srcSize) {
    /* FIXME: it is stupid, but we could add a full streaming decompression pass as a fall-back */
    error("The used zstd library does not include support for streaming decompression bounds so we cannot decompress streams in memory.");
}
#else
/* seems silly, but this is needed to declare ZSTD_decompressBound */
#define ZSTD_STATIC_LINKING_ONLY 1
#endif

#include <zstd.h>

typedef struct zstdfileconn {
    FILE *fp;
    ZSTD_DCtx *dc;
    ZSTD_CCtx *cc;
    ZSTD_inBuffer  input;
    ZSTD_outBuffer output;
    unsigned char *inbuf, *outbuf;
    size_t buf_size;
    int compress;
} *Rzstdfileconn;

static Rboolean zstdfile_open(Rconnection con)
{
    Rzstdfileconn zstd = con->private;
    char mode[] = "rb";
    const char *name;

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    errno = 0; /* precaution */
    name = R_ExpandFileName(con->description);
    zstd->fp = R_fopen(name, mode);
    if(!zstd->fp) {
	warning(_("cannot open compressed file '%s', probable reason '%s'"),
		name, strerror(errno));
	return FALSE;
    }
    if (isDir(zstd->fp)) {
	fclose(zstd->fp);
	warning(_("cannot open file '%s': it is a directory"), name);
	return FALSE;
    }
    if (!zstd->inbuf) {
	/* to ballpark: minimum is ZSTD_BLOCKSIZE_MAX = 128k, for output add block header (512) + hash(4) */
	zstd->buf_size = 512*1024;
	if (!(zstd->inbuf = (unsigned char*) malloc(zstd->buf_size)) ||
	    !(zstd->outbuf = (unsigned char*) malloc(zstd->buf_size))) {
	    warning(_("cannot initialize zstd decompressor"));
	    return FALSE;
	}
    }
    zstd->input.src = zstd->inbuf;
    zstd->input.size = zstd->input.pos = 0;
    zstd->output.dst = zstd->outbuf;
    zstd->output.size = zstd->output.pos = 0;
    if(con->canread) {
	if (!(zstd->dc = ZSTD_createDCtx())) {
	    warning(_("cannot initialize zstd decompressor"));
	    return FALSE;
	}
    } else {
	if (!(zstd->cc = ZSTD_createCCtx())) {
	    warning(_("cannot initialize zstd compressor"));
	    return FALSE;
	}
	/* official sizes could be obtained via:
	size_t const buffInSize  = ZSTD_CStreamInSize();
	size_t const buffOutSize = ZSTD_CStreamOutSize(); */
	ZSTD_CCtx_setParameter(zstd->cc, ZSTD_c_compressionLevel, zstd->compress);
	ZSTD_CCtx_setParameter(zstd->cc, ZSTD_c_checksumFlag, 1);
	/* if we want threading: ZSTD_CCtx_setParameter(cctx, ZSTD_c_nbWorkers, nbThreads); */
    }
    con->isopen = TRUE;
    con->text = strchr(con->mode, 'b') ? FALSE : TRUE;
    set_buffer(con);
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static int zstdfile_fflush(Rconnection con) {
    Rzstdfileconn zstd = con->private;
    /* compression must flush internal buffers to finish the last frame */
    if (con->canwrite) {
	ZSTD_inBuffer input = { zstd->inbuf, 0, 0 };
	size_t rem = 0;
	do {
	    zstd->output.size = zstd->buf_size;
	    zstd->output.pos = 0;
	    rem = ZSTD_compressStream2(zstd->cc, &zstd->output, &input, ZSTD_e_end);
	    if (zstd->output.pos) {
		size_t res = fwrite(zstd->output.dst, 1, zstd->output.pos, zstd->fp);
		if (res != zstd->output.pos)
		    error("fwrite error");
	    }
	} while (rem > 0);
	/* technially, the output can be buffered, but in practice unlikely */
	fflush(zstd->fp);
    }
    return 0;
}

static void zstdfile_close(Rconnection con)
{
    Rzstdfileconn zstd = con->private;

    if(con->canwrite)
	zstdfile_fflush(con);

    if (con->canread)
	ZSTD_freeDCtx(zstd->dc);
    else
	ZSTD_freeCCtx(zstd->cc);

    free(zstd->inbuf);
    zstd->inbuf = 0;
    free(zstd->outbuf);
    zstd->outbuf = 0;

    fclose(zstd->fp);
    con->isopen = FALSE;
}

static size_t zstdfile_read(void *ptr, size_t size, size_t nitems, Rconnection con)
{
    Rzstdfileconn zstd = con->private;
    size_t s = size * nitems, ppos = 0, need = s;
    unsigned char *p = ptr;

    if (!s) return 0;

    if (zstd->output.size > 0) { /* got something left over from last time? */
	if (s <= zstd->output.size - zstd->output.pos) { /* can fulfill all? */
	    memcpy(ptr, zstd->outbuf + zstd->output.pos, s);
	    zstd->output.pos += s;
	    return nitems;
	}
	/* copy what we have */
	ppos = zstd->output.size - zstd->output.pos;
	need -= ppos;
	memcpy(ptr, zstd->outbuf + zstd->output.pos, ppos);
	zstd->output.size = 0;
    }
    /* at this point the output buffer is empty */
    while (1) {
	/* have to read more? */
	if (zstd->input.pos == zstd->input.size) {
	    size_t n = fread(zstd->inbuf, 1, zstd->buf_size, zstd->fp);
	    if (n > 0) {
		zstd->input.size = n;
		zstd->input.pos = 0;
	    }
	}
	/* anything left to decompress? */
	while (zstd->input.pos < zstd->input.size) {
	    zstd->output.size = zstd->buf_size;
	    zstd->output.pos  = 0;
	    size_t const ret = ZSTD_decompressStream(zstd->dc, &zstd->output , &zstd->input);
	    if (ZSTD_isError(ret))
		error("decompress error: %s", ZSTD_getErrorName(ret));
	    if (zstd->output.pos > need) { /* have more than what we need  - need to keep it */
		zstd->output.size = zstd->output.pos;
		zstd->output.pos = need;
		memcpy(p + ppos, zstd->output.dst, need);
		return nitems;
	    }
	    memcpy(p + ppos, zstd->output.dst, zstd->output.pos);
	    ppos += zstd->output.pos;
	    need -= zstd->output.pos;
	    /* we used it all */
	    zstd->output.size = 0;
	    if (!need)
		return nitems;
	}
	if (feof(zstd->fp)) /* no more input? */
	    break;
    }
    return ppos / size;
}

static int zstdfile_fgetc_internal(Rconnection con)
{
    char buf[1];
    size_t size = zstdfile_read(buf, 1, 1, con);

    return (size < 1) ? R_EOF : (buf[0] % 256);
}


static size_t zstdfile_write(const void *ptr, size_t size, size_t nitems, Rconnection con)
{
    Rzstdfileconn zstd = con->private;
    size_t s = size * nitems;
    ZSTD_inBuffer input = { ptr, s, 0 };

    if (!s) return 0;

    do {
	zstd->output.size = zstd->buf_size;
	zstd->output.pos = 0;
	/* we have no way of knowing if that's the only write, so we have to use ZSTD_e_continue */
	/* size_t rem = (not used) */ ZSTD_compressStream2(zstd->cc, &zstd->output, &input, ZSTD_e_continue);
	if (zstd->output.pos) {
	    size_t res = fwrite(zstd->output.dst, 1, zstd->output.pos, zstd->fp);
	    if (res != zstd->output.pos)
		error("fwrite error");
	}
    } while (input.pos < input.size);
    /* NB: there may be still remaining data in the internal buffers (rem > 0) until we flush which is ok */
    return nitems;
}

static Rconnection
newzstdfile(const char *description, const char *mode, int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of zstdfile connection failed"));
    new->class = (char *) malloc(strlen("zstdfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of zstdfile connection failed"));
	new = NULL;
    }
    strcpy(new->class, "zstdfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of zstdfile connection failed"));
	new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);

    new->canseek = FALSE;
    new->open = &zstdfile_open;
    new->close = &zstdfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &zstdfile_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->fflush = &zstdfile_fflush;
    new->read = &zstdfile_read;
    new->write = &zstdfile_write;
    new->private = (void *) malloc(sizeof(struct zstdfileconn));
    memset(new->private, 0, sizeof(struct zstdfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of zstdfile connection failed"));
	new = NULL;
    }
    ((Rzstdfileconn) new->private)->compress = compress;
    return new;
}

#else
static Rconnection
newzstdfile(const char *description, const char *mode, int compress) {
    error("Zstd compression support was not included in this R binary.");
    /* unreachable */
    return 0;
}
#endif

typedef enum { COMP_UNKNOWN = 0, COMP_GZ, COMP_BZ, COMP_XZ, COMP_ZSTD } comp_type;

static comp_type
comp_type_from_memory(char *buf, size_t len, Rboolean with_zlib, int *subtype)
{
    if(len >= 2 && buf[0] == '\x1f' && buf[1] == '\x8b')
	return COMP_GZ;
    else if(with_zlib && len>=2 && buf[0] == '\x78' && buf[1] == '\x9c')
	/* zlib commression starts with 2 bytes, which for default settings are
	   \x78\x9c.  We could use that */
	return COMP_GZ;
    else if(len >= 10 && !strncmp(buf, "BZh", 3)) { 
	/* check also the block size and the block/eos magic to reduce
	   the risk of picking up an uncompressed file (PR#18768) */
	if (buf[3] >= '1' && buf[3] <= '9') {
	    // 0x314159265359 (BCD (pi))
	    // 0x177245385090 (BCD sqrt(pi))
	    if (!memcmp(buf+4, "\x31\x41\x59\x26\x53\x59", 6) ||
	        !memcmp(buf+4, "\x17\x72\x45\x38\x50\x90", 6))

		return COMP_BZ;
	}
    } else if(len >= 5 && buf[0] == '\xFD' && !strncmp(buf+1, "7zXZ", 4)) {
	*subtype = 0;
	return COMP_XZ;
    } else if(len >= 5 && buf[0] == '\xFF' && !strncmp(buf+1, "LZMA", 4)) {
	*subtype = 1;
	return COMP_XZ;
    } else if(len >= 5 && !memcmp(buf, "]\0\0\200\0", 5)) {
	*subtype = 1;
	return COMP_XZ;
    } else if(len >= 4 && buf[0] == '\x89' && !strncmp(buf+1, "LZO", 3))
	error(_("this is a %s-compressed file which this build of R does not support"),
	        "lzop");
    else if(len >= 4 && !memcmp(buf, "\x28\xb5\x2f\xfd", 4))
	return COMP_ZSTD;
    return COMP_UNKNOWN; 
}

static comp_type 
comp_type_from_file(const char *name, Rboolean with_zlib, int *subtype)
{
    FILE *fp = fopen(name, "rb");
    char buf[10];

    if (fp) {
	size_t res = fread(buf, 1, sizeof(buf), fp);
	fclose(fp);
	if(res > 0)
	    return comp_type_from_memory(buf, res, with_zlib, subtype);
    }
    return COMP_UNKNOWN;
}

/* op 0 is gzfile, 1 is bzfile, 2 is xz/lzma, 3 is zstd */
attribute_hidden SEXP do_gzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    const char *file, *open;
    int ncon, compress = 9;
    Rconnection con = NULL;
    int type = PRIMVAL(op);
    int subtype = 0;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateCharFP(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    if(type < 2) {
	compress = asInteger(CADDDR(args));
	if(compress == NA_LOGICAL || compress < 0 || compress > 9)
	    error(_("invalid '%s' argument"), "compress");
    }
    if(type == 2) {
	compress = asInteger(CADDDR(args));
	if(compress == NA_LOGICAL || abs(compress) > 9)
	    error(_("invalid '%s' argument"), "compress");
    }
    if(type == 3) {
	compress = asInteger(CADDDR(args));
	if(compress == NA_LOGICAL || abs(compress) > 19)
	    error(_("invalid '%s' argument"), "compress");
    }
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if (type == 0 && (!open[0] || open[0] == 'r')) {
	/* check magic no */
	comp_type ct;
	ct = comp_type_from_file(R_ExpandFileName(file), FALSE, &subtype);
	switch(ct) {
	case COMP_GZ:
	case COMP_UNKNOWN: type = 0; break;
	case COMP_BZ: type = 1; break;
	case COMP_XZ: type = 2; break;
	case COMP_ZSTD: type = 3; break;
	}
    }
    switch(type) {
    case 0:
	/* gzfile connection handles also transparent (uncompressed) files */
	con = newgzfile(file, strlen(open) ? open : "rb", compress);
	break;
    case 1:
	con = newbzfile(file, strlen(open) ? open : "rb", compress);
	break;
    case 2:
	con = newxzfile(file, strlen(open) ? open : "rb", subtype, compress);
	break;
    case 3:
	con = newzstdfile(file, strlen(open) ? open : "rb", compress);
	break;
    }
    ncon = NextConnection();
    Connections[ncon] = con;
    con->blocking = TRUE;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';

    /* see the comment in do_url */
    if (con->encname[0] && !streql(con->encname, "native.enc"))
	con->canseek = 0;
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    switch(type) {
    case 0:
	SET_STRING_ELT(class, 0, mkChar("gzfile"));
	break;
    case 1:
	SET_STRING_ELT(class, 0, mkChar("bzfile"));
	break;
    case 2:
	SET_STRING_ELT(class, 0, mkChar("xzfile"));
	break;
    case 3:
	SET_STRING_ELT(class, 0, mkChar("zstdfile"));
	break;
    }
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* ------------------- clipboard connections --------------------- */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
extern int GA_clipboardhastext(void); /* from ga.h */
#endif

#ifdef Unix
Rboolean R_ReadClipboard(Rclpconn clpcon, char *type);
#endif

static Rboolean clp_open(Rconnection con)
{
    Rclpconn this = con->private;

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    this->pos = 0;
    if (strlen(con->encname) > 0 && strcmp(con->encname, "native.enc") &&
        strcmp(con->encname, "UTF-16LE"))
	/* R <= 4.1 allowed writing data to clipboard in given encoding,
	   but did not specify that encoding using CF_LOCALE. Similarly, it
	   would read data assuming a given encoding, without checking
	   CF_LOCALE. Using CF_UNICODETEXT is simpler as it avoids the need
	   for specifying CF_LOCALE and hence the conversion between iconv
	   encoding names and Windows locale IDs. */
	warning(_("argument '%s' will be ignored"), "encoding");
    if(con->canread) {
	/* copy the clipboard contents now */
#ifdef Win32
	HGLOBAL hglb;
	wchar_t *wpc;
	if(GA_clipboardhastext() &&
	   OpenClipboard(NULL) &&
	   (hglb = GetClipboardData(CF_UNICODETEXT)) &&
	   (wpc = (wchar_t *)GlobalLock(hglb))) {
	    
	    int len = (int)wcslen(wpc) * sizeof(wchar_t);
	    this->buff = (char *)malloc(len + 1);
	    this->last = this->len = len;
	    if(this->buff) {
		memcpy(this->buff, wpc, len + 1);
		GlobalUnlock(hglb);
		CloseClipboard();
	    } else {
		GlobalUnlock(hglb);
		CloseClipboard();
		this->buff = NULL; this->last = this->len = 0;
		warning(_("memory allocation to copy clipboard failed"));
		return FALSE;
	    }
	} else {
	    this->buff = NULL; this->last = this->len = 0;
	    warning(_("clipboard cannot be opened or contains no text"));
	    return FALSE;
	}
#else
	Rboolean res = R_ReadClipboard(this, con->description);
	if(!res) return FALSE;
#endif
    } else {
	int len = (this->sizeKB)*1024;
	this->buff = (char *) malloc(len + 1);
	if(!this->buff) {
	    warning(_("memory allocation to open clipboard failed"));
	    return FALSE;
	}
	this->len = len;
	this->last = 0;
    }
    con->text = TRUE;
    /* Not calling set_buffer(con) as the data is already buffered */
#ifdef Win32
    strncpy(con->encname, "UTF-16LE", 100);
    con->encname[100 - 1] = '\0';
#endif
    set_iconv(con);
    con->save = -1000;
    this->warned = FALSE;

    return TRUE;
}

static void clp_writeout(Rconnection con)
{
#ifdef Win32
    Rclpconn this = con->private;

    /* see comment on CF_UNICODETEXT/CF_TEXT in clp_open */
    HGLOBAL hglb;
    wchar_t *s;
    int wlen = (this->last + sizeof(wchar_t) - 1) / sizeof(wchar_t);
    if ( (hglb = GlobalAlloc(GHND, (wlen + 1) * sizeof(wchar_t))) &&
	 (s = (wchar_t *)GlobalLock(hglb)) ) {
	memcpy(s, this->buff, wlen * sizeof(wchar_t));
	s[wlen] = L'\0';
	GlobalUnlock(hglb);
	if (!OpenClipboard(NULL) || !EmptyClipboard()) {
	    GlobalFree(hglb);
	    warning(_("unable to open the clipboard"));
	} else {
	    if(!SetClipboardData(CF_UNICODETEXT, hglb)) {
		GlobalFree(hglb);
		warning(_("unable to write to the clipboard"));
	    }
	    CloseClipboard();
	}
    }
#endif
}

static void clp_close(Rconnection con)
{
    Rclpconn this = con->private;

    con->isopen = FALSE;
    if(con->canwrite)
	clp_writeout(con);
    if(this->buff) free(this->buff);
}

static int clp_fgetc_internal(Rconnection con)
{
    Rclpconn this = con->private;

    if (this->pos >= this->len) return R_EOF;
    /* the cast prevents sign extension of 0xFF to -1 (R_EOF) */
    return (unsigned char)this->buff[this->pos++];
}

static double clp_seek(Rconnection con, double where, int origin, int rw)
{
    Rclpconn this = con->private;
    int newpos, oldpos = this->pos;

    if(ISNA(where)) return oldpos;

    switch(origin) {
    case 2: newpos = this->pos + (int) where; break;
    case 3: newpos = this->last + (int) where; break;
    default: newpos = (int) where;
    }
    if(newpos < 0 || newpos >= this->last)
	error(_("attempt to seek outside the range of the clipboard"));
    else this->pos = newpos;

    return (double) oldpos;
}

static void clp_truncate(Rconnection con)
{
    Rclpconn this = con->private;

    if(!con->isopen || !con->canwrite)
	error(_("can only truncate connections open for writing"));
    this->last = this->pos;
}

static int clp_fflush(Rconnection con)
{
    if(!con->isopen || !con->canwrite) return 1;
    clp_writeout(con);
    return 0;
}

static size_t clp_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rclpconn this = con->private;
    int available = this->len - this->pos, request = (int)(size*nitems), used;
    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));
    used = (request < available) ? request : available;
    strncpy(ptr, this->buff + this->pos, used);
    this->pos += used;
    return (size_t) used/size;
}

static size_t clp_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rclpconn this = con->private;
    int len = (int)(size * nitems), used = 0;

    if(!con->canwrite)
	error(_("clipboard connection is open for reading only"));
    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));

#ifdef Win32
    /* clipboard requires CRLF termination, copy by wchar_t */
    int i;
    wchar_t wc, *p = (wchar_t *) ptr, *q = (wchar_t *) (this->buff + this->pos);

    for(i = 0; i < len; i += sizeof(wchar_t)) {
	if(this->pos >= this->len) break;
	wc = *p++;
	if(wc == L'\n') {
	    *q++ = L'\r';
	    this->pos += sizeof(wchar_t);
	    if(this->pos >= this->len) break;
	}
	*q++ = wc;
	this->pos += sizeof(wchar_t);
	used += sizeof(wchar_t);
    }
#else
    /* NOTE: not reachable as clipboard is not writeable on Unix */
    /* copy byte-by-byte */
    int space = this->len - this->pos;
    used = (space < len) ? space : len;
    if (used)
	memcpy(this->buff + this->pos, ptr, used);
    this->pos += used;
#endif

    if (used < len && !this->warned) {
	this->warned = TRUE;
	warning(_("clipboard buffer is full and output lost"));
    }
    if(this->last < this->pos) this->last = this->pos;
    return (size_t) used/size;
}

static Rconnection newclp(const char *url, const char *inmode)
{
    Rconnection new;
    const char *description;
    int sizeKB = 64;
    char mode[4];

    mode[3] = '\0';
    strncpy(mode, inmode, 3);

    if(strlen(mode) == 2 && mode[1] == 't') mode[1] = '\0';

    if(strlen(mode) != 1 ||
       (mode[0] != 'r' && mode[0] != 'w'))
	error(_("'mode' for the clipboard must be 'r' or 'w'"));
#ifdef Unix
    if(mode[0] != 'r')
	error(_("'mode' for the clipboard must be 'r' on Unix"));
#endif
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of clipboard connection failed"));
    if(strncmp(url, "clipboard", 9) == 0) description = "clipboard";
    else description = url;
    new->class = (char *) malloc(strlen(description) + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->open = &clp_open;
    new->close = &clp_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &clp_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->seek = &clp_seek;
    new->truncate = &clp_truncate;
    new->fflush = &clp_fflush;
    new->read = &clp_read;
    new->write = &clp_write;
    new->canseek = TRUE;
    new->private = (void *) malloc(sizeof(struct clpconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of clipboard connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rclpconn)new->private)->buff = NULL;
    if (strncmp(url, "clipboard-", 10) == 0) {
	sizeKB = atoi(url+10);
	if(sizeKB < 64) sizeKB = 64;
	/* Rprintf("setting clipboard size to %dKB\n", sizeKB); */
    }
    ((Rclpconn)new->private)->sizeKB = sizeKB;
    return new;
}

/* ------------------- terminal connections --------------------- */

static unsigned char  ConsoleBuf[CONSOLE_BUFFER_SIZE+1];
static unsigned char *ConsoleBufp;
static int  ConsoleBufCnt;

static int ConsoleGetchar(void)
{
    if (--ConsoleBufCnt < 0) {
	ConsoleBuf[CONSOLE_BUFFER_SIZE] = '\0';
	if (R_ReadConsole("", ConsoleBuf, CONSOLE_BUFFER_SIZE, 0) == 0) {
	    R_ClearerrConsole();
	    return R_EOF;
	}
	ConsoleBufp = ConsoleBuf;
	ConsoleBufCnt = (int) strlen((char *)ConsoleBuf); // must be short
	ConsoleBufCnt--;
    }
    return *ConsoleBufp++;
}

static int stdin_fgetc(Rconnection con)
{
    return ConsoleGetchar();
}

static int stdout_vfprintf(Rconnection con, const char *format, va_list ap)
{
    if(R_Outputfile)
	return vfprintf(R_Outputfile, format, ap);
    else
	return Rcons_vprintf(format, ap);
}

static int stdout_fflush(Rconnection con)
{
    if(R_Outputfile) return fflush(R_Outputfile);
    return 0;
}

static int stderr_vfprintf(Rconnection con, const char *format, va_list ap)
{
    return REvprintf_internal(format, ap);
}

static int stderr_fflush(Rconnection con)
{
    /* normally stderr and hence unbuffered, but it needs not be,
       e.g. it is stdout on Win9x */
    if(R_Consolefile) return fflush(R_Consolefile);
    return 0;
}

static Rconnection newterminal(const char *description, const char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of terminal connection failed"));
    new->class = (char *) malloc(strlen("terminal") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of terminal connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "terminal");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of terminal connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->isopen = TRUE;
    new->canread = (strcmp(mode, "r") == 0);
    new->canwrite = (strcmp(mode, "w") == 0);
    new->destroy = &null_close;
    new->private = NULL;
    return new;
}


attribute_hidden SEXP do_stdin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(0);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(0));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_stdout(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(R_OutputCon);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(R_OutputCon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}


attribute_hidden SEXP do_stderr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(2);

    checkArity(op, args);
    PROTECT(ans = ScalarInteger(2));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}


attribute_hidden SEXP do_isatty(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int con;
    /* FIXME: is this correct for consoles? */
    checkArity(op, args);
    con = asInteger(CAR(args));
    return ScalarLogical(con == NA_LOGICAL ? FALSE : R_isatty(con) );
}

/* ------------------- raw connections --------------------- */

/* Possible future redesign: store nbytes as TRUELENGTH */

typedef struct rawconn {
    SEXP data; /* all the data, stored as a raw vector */
    /* replace nbytes by TRUELENGTH in due course? */
    size_t pos, nbytes; /* current pos and number of bytes
			   (same pos for read and write) */
} *Rrawconn;


/* copy a raw vector into a buffer */
static void raw_init(Rconnection con, SEXP raw)
{
    Rrawconn this = con->private;

    this->data = MAYBE_REFERENCED(raw) ? duplicate(raw) : raw;
    R_PreserveObject(this->data);
    this->nbytes = XLENGTH(this->data);
    this->pos = 0;
}

static Rboolean raw_open(Rconnection con)
{
    return TRUE;
}

static void raw_close(Rconnection con)
{
}

static void raw_destroy(Rconnection con)
{
    Rrawconn this = con->private;

    R_ReleaseObject(this->data);
    free(this);
}

static void raw_resize(Rrawconn this, size_t needed)
{
    size_t nalloc = 64;
    SEXP tmp;

    if (needed > 8192) nalloc = (size_t)(1.2*(double)needed); /* 20% over-allocation */
    else while(nalloc < needed) nalloc *= 2;  /* use powers of 2 if small */
    PROTECT(tmp = allocVector(RAWSXP, nalloc));
    if (this->nbytes)
	memcpy(RAW(tmp), RAW(this->data), this->nbytes);
    R_ReleaseObject(this->data);
    this->data = tmp;
    R_PreserveObject(this->data);
    UNPROTECT(1);
}

static size_t raw_write(const void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rrawconn this = con->private;
    size_t freespace = XLENGTH(this->data) - this->pos, bytes = size*nitems;

    if ((double) size * (double) nitems + (double) this->pos > R_XLEN_T_MAX)
	error(_("attempting to add too many elements to raw vector"));
    /* resize may fail, when this will give an error */
    if(bytes >= freespace) raw_resize(this, bytes + this->pos);
    /* the source just might be this raw vector */
    if (bytes)
	memmove(RAW(this->data) + this->pos, ptr, bytes);
    this->pos += bytes;
    if(this->nbytes < this->pos) this->nbytes = this->pos;
    return nitems;
}

static void raw_truncate(Rconnection con)
{
    Rrawconn this = con->private;
    this->nbytes = this->pos;
}

static size_t raw_read(void *ptr, size_t size, size_t nitems,
		       Rconnection con)
{
    Rrawconn this = con->private;
    size_t available = this->nbytes - this->pos, request = size*nitems, used;

    if ((double) size * (double) nitems + (double) this->pos > R_XLEN_T_MAX)
	error(_("too large a block specified"));
    used = (request < available) ? request : available;
    memmove(ptr, RAW(this->data) + this->pos, used);
    this->pos += used;
    return used/size;
}

static int raw_fgetc(Rconnection con)
{
    Rrawconn this = con->private;
    if(this->pos >= this->nbytes) return R_EOF;
    else return (int) RAW(this->data)[this->pos++];
}

static double raw_seek(Rconnection con, double where, int origin, int rw)
{
    Rrawconn this = con->private;
    double newpos;
    size_t oldpos = this->pos;

    if(ISNA(where)) return (double) oldpos;

    /* Do the calculations here as double to avoid integer overflow */
    switch(origin) {
    case 2: newpos = (double) this->pos + where; break;
    case 3: newpos = (double) this->nbytes + where; break;
    default: newpos = where;
    }
    if(newpos < 0 || newpos > this->nbytes)
	error(_("attempt to seek outside the range of the raw connection"));
    else this->pos = (size_t) newpos;

    return (double) oldpos;
}

static Rconnection newraw(const char *description, SEXP raw, const char *mode)
{
    Rconnection new;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of raw connection failed"));
    new->class = (char *) malloc(strlen("rawConnection") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "rawConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->isopen = TRUE;
    new->text = FALSE;
    new->blocking = TRUE;
    new->canseek = TRUE;
    new->canwrite = (mode[0] == 'w' || mode[0] == 'a');
    new->canread = mode[0] == 'r';
    if(strlen(mode) >= 2 && mode[1] == '+') new->canread = new->canwrite = TRUE;
    new->open = &raw_open;
    new->close = &raw_close;
    new->destroy = &raw_destroy;
    if(new->canwrite) {
	new->write = &raw_write;
	new->vfprintf = &dummy_vfprintf;
	new->truncate = &raw_truncate;
    }
    if(new->canread) {
	new->read = &raw_read;
	new->fgetc = &raw_fgetc;
    }
    new->seek = &raw_seek;
    new->private = (void*) malloc(sizeof(struct rawconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of raw connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    raw_init(new, raw);
    if(mode[0] == 'a') raw_seek(new, 0, 3, 0);
    return new;
}

// .Internal(rawConnection(deparse(substitute(object)), object, open))
attribute_hidden SEXP do_rawconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sraw, sopen, ans, class;
    const char *desc, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    desc = translateCharFP(STRING_ELT(sfile, 0));
    sraw = CADR(args);
    sopen = CADDR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strchr(open, 't'))
	error(_("invalid '%s' argument"), "open");
    ncon = NextConnection();
    if(TYPEOF(sraw) != RAWSXP)
	error(_("invalid '%s' argument"), "raw");
    con = Connections[ncon] = newraw(desc, sraw, open);

    /* already opened */

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("rawConnection"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    con->ex_ptr = R_MakeExternalPtr(con->id, install("connection"), R_NilValue);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(2);
    return ans;
}

static Rconnection getConnectionCheck(SEXP rcon, const char *cls,
				      const char *var)
{
    if(!inherits(rcon, cls))
	error(_("'%s' is not a %s"), var, cls);
    Rconnection con = getConnection(asInteger(rcon));
    /* check that the R class and internal class match */
    if (strcmp(con->class, cls))
	error(_("internal connection is not a %s"), cls);
    return con;
}

attribute_hidden SEXP do_rawconvalue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con=NULL;
    Rrawconn this;
    SEXP ans;

    checkArity(op, args);
    con = getConnectionCheck(CAR(args), "rawConnection", "con");
    if(!con->canwrite)
	error(_("'con' is not an output rawConnection"));
    this = con->private;
    ans = allocVector(RAWSXP, this->nbytes); /* later, use TRUELENGTH? */
    if (this->nbytes)
	memcpy(RAW(ans), RAW(this->data), this->nbytes);
    return ans;
}

/* ------------------- text connections --------------------- */

typedef struct textconn {
    char *data;  /* all the data */
    size_t cur, nchars; /* current pos and number of chars */
    char save; /* pushback */
} *Rtextconn;

typedef struct outtextconn {
    size_t len;  /* number of lines */
    SEXP namesymbol;
    SEXP data;
    char *lastline;
    int lastlinelength; /* buffer size */
} *Routtextconn;

/* read a R character vector into a buffer  --- helper for newtext() */
static void text_init(Rconnection con, SEXP text, int type)
{
    R_xlen_t nlines = xlength(text);  // not very plausible that this is long
    size_t nchars = 0; /* -Wall */
    double dnc = 0.0;
    Rtextconn this = con->private;
    const void *vmax = vmaxget();

    for(R_xlen_t i = 0; i < nlines; i++)
	dnc +=
	    /*     type =  1 |    2    |    3    <==>
	     * encoding = "" | "bytes" | "UTF-8" */
	    (double) strlen(type == 1 ? translateChar(STRING_ELT(text, i))
			    : ((type == 3) ?translateCharUTF8(STRING_ELT(text, i))
			       : CHAR(STRING_ELT(text, i))) ) + 1;
    if (dnc >= (double) SIZE_MAX)
	error(_("too many characters for text connection"));
    else nchars = (size_t) dnc;
    this->data = (char *) malloc(nchars+1);
    if(!this->data) {
	free(this); free(con->description); free(con->class); free(con);
	error(_("cannot allocate memory for text connection"));
    }
    char *t = this->data;
    for(R_xlen_t i = 0; i < nlines; i++) {
	const char *s = (type == 1) ? translateChar(STRING_ELT(text, i))
	    : ((type == 3) ? translateCharUTF8(STRING_ELT(text, i))
	       : CHAR(STRING_ELT(text, i)));
	while(*s) *t++ = *s++;
	*t++ = '\n';
    }
    *t = '\0';
    this->nchars = nchars;
    this->cur = this->save = 0;
    vmaxset(vmax);
}

static Rboolean text_open(Rconnection con)
{
    con->save = -1000;
    return TRUE;
}

static void text_close(Rconnection con)
{
}

static void text_destroy(Rconnection con)
{
    Rtextconn this = con->private;

    free(this->data);
    /* this->cur = this->nchars = 0; */
    free(this);
}

static int text_fgetc(Rconnection con)
{
    Rtextconn this = con->private;
    if(this->save) {
	int c;
	c = this->save;
	this->save = 0;
	return c;
    }
    if(this->cur >= this->nchars) return R_EOF;
    else return (int) (this->data[this->cur++]);
}

static double text_seek(Rconnection con, double where, int origin, int rw)
{
    if(where >= 0) error(_("seek is not relevant for text connection"));
    return 0; /* if just asking, always at the beginning */
}

// helper for do_textconnection(.., open = "r") :
static Rconnection newtext(const char *description, SEXP text, int type)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of text connection failed"));
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, "r");
    new->isopen = TRUE;
    new->canwrite = FALSE;
    new->open = &text_open;
    new->close = &text_close;
    new->destroy = &text_destroy;
    new->fgetc = &text_fgetc;
    new->seek = &text_seek;
    new->private = (void*) malloc(sizeof(struct textconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    text_init(new, text, type);
    return new;
}


static SEXP mkCharLocal(const char *s)
{
    int ienc = CE_NATIVE;
    if(known_to_be_latin1) ienc = CE_LATIN1;
    if(known_to_be_utf8) ienc = CE_UTF8;
    return mkCharCE(s, ienc);
}

static void outtext_close(Rconnection con)
{
    Routtextconn this = con->private;
    int idx = ConnIndex(con);
    SEXP tmp, env = VECTOR_ELT(OutTextData, idx);

    if(this->namesymbol && R_existsVarInFrame(env, this->namesymbol))
	R_unLockBinding(this->namesymbol, env);
    if(strlen(this->lastline) > 0) {
	PROTECT(tmp = xlengthgets(this->data, ++this->len));
	SET_STRING_ELT(tmp, this->len - 1, mkCharLocal(this->lastline));
	if(this->namesymbol) defineVar(this->namesymbol, tmp, env);
	ENSURE_NAMEDMAX(tmp);
	this->data = tmp;
	UNPROTECT(1);
    }
}

static void outtext_destroy(Rconnection con)
{
    Routtextconn this = con->private;
    int idx = ConnIndex(con);
    /* OutTextData is preserved, and that implies that the environment
       we are writing it and hence the character vector is protected.
       However, this could be quite expensive.
    */
    SET_VECTOR_ELT(OutTextData, idx, R_NilValue);
    if(!this->namesymbol) R_ReleaseObject(this->data);
    free(this->lastline); free(this);
}

#define LAST_LINE_LEN 256

static int text_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Routtextconn this = con->private;
    char buf[BUFSIZE], *b = buf, *p, *q;
    const void *vmax = NULL;
    int res = 0, buffree,
	already = (int) strlen(this->lastline); // we do not allow longer lines
    SEXP tmp;

    va_list aq;
    va_copy(aq, ap);
    if(already >= BUFSIZE) {
	/* This will fail so just call vsnprintf to get the length of
	   the new piece */
	res = vsnprintf(buf, 0, format, aq);
	if(res > 0) res += already;
	buffree = 0;
    } else {
	strcpy(b, this->lastline);
	p = b + already;
	buffree = BUFSIZE - already; // checked < BUFSIZE above
	res = vsnprintf(p, buffree, format, aq);
    }
    va_end(aq);
    if(res >= buffree) { /* res is the desired output length */
	vmax = vmaxget();
	size_t sz = res + already + 1;
	b = R_alloc(sz, sizeof(char));
	strcpy(b, this->lastline);
	p = b + already;
	vsnprintf(p, sz - already, format, ap);
    } else if(res < 0) { /* just a failure indication */
#define NBUFSIZE (already + 100*BUFSIZE)
	vmax = vmaxget();
	b = R_alloc(NBUFSIZE, sizeof(char));
	strncpy(b, this->lastline, NBUFSIZE); /* `already` < NBUFSIZE */
	*(b + NBUFSIZE - 1) = '\0';
	p = b + already;
	res = Rvsnprintf_mbcs(p, NBUFSIZE - already, format, ap);
	if (res < 0 || res >= NBUFSIZE - already) {
	    warning(_("printing of extremely long output is truncated"));
	}
    }

    /* copy buf line-by-line to object */
    for(p = b; ; p = q+1) {
	q = Rf_strchr(p, '\n');
	if(q) {
	    int idx = ConnIndex(con);
	    SEXP env = VECTOR_ELT(OutTextData, idx);
	    *q = '\0';
	    PROTECT(tmp = xlengthgets(this->data, ++this->len));
	    SET_STRING_ELT(tmp, this->len - 1, mkCharLocal(p));
	    if(this->namesymbol) {
		if(R_existsVarInFrame(env, this->namesymbol))
		    R_unLockBinding(this->namesymbol, env);
		defineVar(this->namesymbol, tmp, env);
		R_LockBinding(this->namesymbol, env);
	    } else {
		R_ReleaseObject(this->data);
		R_PreserveObject(tmp);
	    }
	    this->data = tmp;
	    ENSURE_NAMEDMAX(tmp);
	    UNPROTECT(1);
	} else {
	    /* retain the last line */
	    if(strlen(p) >= this->lastlinelength) {
		size_t newlen = strlen(p) + 1;
		if (newlen > INT_MAX) error("last line is too long");
		void * tmp = realloc(this->lastline, newlen);
		if (tmp) {
		    this->lastline = tmp;
		    this->lastlinelength = (int) newlen;
		} else {
		    warning("allocation problem for last line");
		    this->lastline = NULL;
		    this->lastlinelength = 0;
		}
	    }
	    strcpy(this->lastline, p);
	    con->incomplete = strlen(this->lastline) > 0;
	    break;
	}
    }
    if(vmax) vmaxset(vmax);
    return res;
}

// finalizing helper for  newouttext() :
static void outtext_init(Rconnection con, SEXP stext, const char *mode, int idx)
{
    Routtextconn this = con->private;
    SEXP val;

    if(stext == R_NilValue) {
	this->namesymbol = NULL;
	    /* create variable pointed to by con->description */
	val = allocVector(STRSXP, 0);
	R_PreserveObject(val);
    } else {
	this->namesymbol = install(con->description);
	if(strcmp(mode, "w") == 0) {
	    /* create variable pointed to by con->description */
	    PROTECT(val = allocVector(STRSXP, 0));
	    defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
	    /* Not clear if this is needed, but be conservative */
	    ENSURE_NAMEDMAX(val);
	    UNPROTECT(1);
	} else {
	    /* take over existing variable */
	    val = findVar1(this->namesymbol, VECTOR_ELT(OutTextData, idx),
			   STRSXP, FALSE);
	    if(val == R_UnboundValue) {
		warning(_("text connection: appending to a non-existent char vector"));
		PROTECT(val = allocVector(STRSXP, 0));
		defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
		ENSURE_NAMEDMAX(val);
		UNPROTECT(1);
	    }
	    PROTECT(val);
	    R_LockBinding(this->namesymbol, VECTOR_ELT(OutTextData, idx));
	    UNPROTECT(1);
	}
    }
    this->len = LENGTH(val);
    this->data = val;
    this->lastline[0] = '\0';
    this->lastlinelength = LAST_LINE_LEN;
}

// helper for do_textconnection(.., open = "w" or "a") :
static Rconnection newouttext(const char *description, SEXP stext,
			      const char *mode, int idx)
{
    Rconnection new;
    void *tmp;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of text connection failed"));
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of text connection failed"));
 	/* for Solaris 12.5 */ new = NULL;
   }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->isopen = TRUE;
    new->canread = FALSE;
    new->open = &text_open;
    new->close = &outtext_close;
    new->destroy = &outtext_destroy;
    new->vfprintf = &text_vfprintf;
    new->seek = &text_seek;
    new->private = (void*) malloc(sizeof(struct outtextconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Routtextconn)new->private)->lastline = tmp = malloc(LAST_LINE_LEN);
    if(!tmp) {
	free(new->private);
	free(new->description); free(new->class); free(new);
	error(_("allocation of text connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    outtext_init(new, stext, mode, idx);
    return new;
}

// .Internal(textConnection(name, object, open, env, type))
attribute_hidden SEXP do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sdesc, stext, sopen, ans, class, venv;
    const char *desc, *open;
    int ncon, type;
    Rconnection con = NULL;

    checkArity(op, args);
    sdesc = CAR(args);
    if(!isString(sdesc) || LENGTH(sdesc) != 1 ||
       STRING_ELT(sdesc, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    desc = translateChar(STRING_ELT(sdesc, 0));
    stext = CADR(args); // object
    sopen = CADDR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    venv = CADDDR(args);
    if (isNull(venv))
	error(_("use of NULL environment is defunct"));
    if (!isEnvironment(venv))
	error(_("invalid '%s' argument"), "environment");
    type = asInteger(CAD4R(args));
    if (type == NA_INTEGER)
	error(_("invalid '%s' argument"), "encoding");
    ncon = NextConnection();
    if(!strlen(open) || strncmp(open, "r", 1) == 0) {
	if(!isString(stext))
	    error(_("invalid '%s' argument"), "text");
	con = Connections[ncon] = newtext(desc, stext, type);
    } else if (strncmp(open, "w", 1) == 0 || strncmp(open, "a", 1) == 0) {
	if (OutTextData == NULL) {
	    OutTextData = allocVector(VECSXP, NCONNECTIONS);
	    R_PreserveObject(OutTextData);
	}
	SET_VECTOR_ELT(OutTextData, ncon, venv);
	if(stext == R_NilValue)
	    con = Connections[ncon] = newouttext("NULL", stext, open, ncon);
	else if(isString(stext) && LENGTH(stext) == 1)
	    con = Connections[ncon] =
		newouttext(translateChar(STRING_ELT(stext, 0)), stext,
			   open, ncon);
	else
	    error(_("invalid '%s' argument"), "text");
    }
    else
	error(_("unsupported mode"));
    /* already opened */

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("textConnection"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    con->ex_ptr = R_MakeExternalPtr(con->id, install("connection"), R_NilValue);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_textconvalue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con=NULL;
    Routtextconn this;

    checkArity(op, args);
    con = getConnectionCheck(CAR(args), "textConnection", "con");
    if(!con->canwrite)
	error(_("'con' is not an output textConnection"));
    this = con->private;
    return this->data;
}



/* ------------------- socket connections  --------------------- */


/* socketConnection(host, port, server, blocking, open, encoding, timeout, options) */
/* socketAccept(socket, blocking, open, encoding, timeout, options) */
attribute_hidden SEXP do_sockconn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    const char *host, *open;
    int ncon, port, server, timeout, serverfd, options = 0;
    Rboolean blocking;
    Rconnection con = NULL;
    Rservsockconn scon = NULL;

    checkArity(op, args);
    if (PRIMVAL(op) == 0) { /* socketConnection */
	scmd = CAR(args);
	if(!isString(scmd) || LENGTH(scmd) != 1)
	    error(_("invalid '%s' argument"), "host");
	host = translateCharFP(STRING_ELT(scmd, 0));
	args = CDR(args);
	port = asInteger(CAR(args));
	if(port == NA_INTEGER || port < 0)
	    error(_("invalid '%s' argument"), "port");
	args = CDR(args);
	server = asLogical(CAR(args));
	if(server == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "server");
	serverfd = -1;
    } else { /* socketAccept */
	scon = getConnectionCheck(CAR(args), "servsockconn", "socket")->private;
	port = scon->port;
	server = 1;
	host = "localhost"; /* ignored */
	serverfd = scon->fd;
    }
    args = CDR(args);
    blocking = asRbool(CAR(args), call);
    if(blocking == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    args = CDR(args);
    sopen = CAR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    args = CDR(args);
    enc = CAR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    args = CDR(args);
    timeout = asInteger(CAR(args));
    args = CDR(args);
    /* we don't issue errors/warnings on unknown options to allow for
       future extensions */
    if (isString(CAR(args))) {
	SEXP sOpts = CAR(args);
	int i = 0, n = LENGTH(sOpts);
	while (i < n) {
	    const char *opt = CHAR(STRING_ELT(sOpts, i));
	    if (!strcmp("no-delay", opt))
		options |= RSC_SET_TCP_NODELAY;
	    i++;
	}
    }

    ncon = NextConnection();
    con = R_newsock(host, port, server, serverfd, open, timeout, options);
    Connections[ncon] = con;
    con->blocking = blocking;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("sockconn"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);
    return ans;
}

/* ------------------- unz connections  --------------------- */

/* .Internal(unz(paste(description, filename, sep = ":"),
 *               open, encoding)) */
attribute_hidden SEXP do_unz(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    const char *file, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || LENGTH(sfile) != 1 ||
       STRING_ELT(sfile, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateCharFP(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = NextConnection();
    con = Connections[ncon] = R_newunz(file, strlen(open) ? open : "r"); // see dounzip.c for the details
    con->blocking = TRUE;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("unz"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

/* -------------- open, close, seek, truncate, flush ------------------ */

attribute_hidden SEXP do_open(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    Rconnection con=NULL;
    SEXP sopen;
    const char *open;
    Rboolean success, block;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    i = asInteger(CAR(args));
    con = getConnection(i);
    if(i < 3) error(_("cannot open standard connections"));
    if(con->isopen) {
	warning(_("connection is already open"));
	return R_NilValue;
    }
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asRbool(CADDR(args), call);
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strlen(open) > 0) strcpy(con->mode, open);
    con->blocking = block;
    success = con->open(con);
    if(!success) {
	/* con_destroy(i); user might have a reference */
	error(_("cannot open the connection"));
    }
    return R_NilValue;
}

attribute_hidden SEXP do_isopen(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    int rw, res;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    rw = asInteger(CADR(args));
    res = con->isopen != FALSE;
    switch(rw) {
    case 0: break;
    case 1: res = res & con->canread; break;
    case 2: res = res & con->canwrite; break;
    default: error(_("unknown 'rw' value"));
    }
    return ScalarLogical(res);
}

attribute_hidden SEXP do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    return ScalarLogical(con->incomplete != FALSE);
}

attribute_hidden SEXP do_isseekable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    return ScalarLogical(con->canseek != FALSE);
}

static void checkClose(Rconnection con)
{
    if (con->isopen) {
        errno = 0;
    	con->close(con);
    	if (con->status != NA_INTEGER && con->status < 0) {
    	    int serrno = errno;
            if (serrno)
		warning(_("Problem closing connection:  %s"), strerror(serrno));
	    else
		warning(_("Problem closing connection"));
   	 }
    }
}

static int con_close1(Rconnection con)
{
    int status;
    checkClose(con);
    status = con->status;
    if(con->isGzcon) {
	Rgzconn priv = con->private;
	con_close1(priv->con);
	R_ReleaseObject(priv->con->ex_ptr);
    }
    /* close inconv and outconv if open */
    if(con->inconv) Riconv_close(con->inconv);
    if(con->outconv) Riconv_close(con->outconv);
    con->destroy(con);
    free(con->class);
    con->class = NULL;
    free(con->description);
    con->description = NULL;
    /* clear the pushBack */
    if(con->nPushBack > 0) { // already cleared on closed connection,
	                     // so no double-free pace -fanalyzer
	int j;

	for(j = 0; j < con->nPushBack; j++)
	    free(con->PushBack[j]);
	free(con->PushBack);
    }
    con->nPushBack = 0;
    if (con->buff) {
	free(con->buff);
	con->buff = NULL;
    }
    con->buff_len = con->buff_pos = con->buff_stored_len = 0;
    con->open = &null_open;
    con->close = &null_close;
    con->destroy = &null_destroy;
    con->vfprintf = &null_vfprintf;
    con->fgetc = con->fgetc_internal = &null_fgetc;
    con->seek = &null_seek;
    con->truncate = &null_truncate;
    con->fflush = &null_fflush;
    con->read = &null_read;
    con->write = &null_write;
    return status;
}

static void con_destroy(int i)
{
    Rconnection con=NULL;

    con = getConnection(i);
    con_close1(con);
    free(Connections[i]);
    Connections[i] = NULL;
}

attribute_hidden SEXP do_close(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    i = asInteger(CAR(args));
    if(i < 3) error(_("cannot close standard connections"));
    for(j = 0; j < R_SinkNumber; j++)
	if(i == SinkCons[j])
	    error(_("cannot close 'output' sink connection"));
    if(i == R_ErrorCon)
	error(_("cannot close 'message' sink connection"));
    Rconnection con = getConnection(i);
    int status = con_close1(con);
    free(Connections[i]);
    Connections[i] = NULL;
    return (status != NA_INTEGER) ? ScalarInteger(status) : R_NilValue;
}

static double Rconn_seek(Rconnection con, double where, int origin, int rw) {
    if (con->buff)
	return buff_seek(con, where, origin, rw);
    return con->seek(con, where, origin, rw);
}

/* seek(con, where = numeric(), origin = "start", rw = "") */
attribute_hidden SEXP do_seek(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int origin, rw;
    Rconnection con = NULL;
    double where;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    if(!con->isopen) error(_("connection is not open"));
    where = asReal(CADR(args));
    origin = asInteger(CADDR(args));
    rw = asInteger(CADDDR(args));
    if(!ISNAN(where) && con->nPushBack > 0) {
	/* clear pushback */
	int j;
	for(j = 0; j < con->nPushBack; j++) free(con->PushBack[j]);
	free(con->PushBack);
	con->nPushBack = 0;
    }
    return ScalarReal(Rconn_seek(con, where, origin, rw));
}

/* truncate(con) */
attribute_hidden SEXP do_truncate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    con->truncate(con);
    return R_NilValue;
}

attribute_hidden SEXP do_flush(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    if(con->canwrite) con->fflush(con);
    return R_NilValue;
}

/* ------------------- read, write  text --------------------- */

int Rconn_fgetc(Rconnection con)
{
    char *curLine;
    int c;

    if (con->save2 != -1000) {
	c = con->save2;
	con->save2 = -1000;
	return c;
    }
    if(con->nPushBack <= 0) {
	/* map CR or CRLF to LF */
	if (con->save != -1000) {
	    c = con->save;
	    con->save = -1000;
	    return c;
	}
	c = con->fgetc(con);
	if (c == '\r') {
	    c = con->fgetc(con);
	    if (c != '\n') {
		con->save = (c != '\r') ? c : '\n';
		return('\n');
	    }
	}
	return c;
    }
    curLine = con->PushBack[con->nPushBack-1];
    c = (unsigned char) curLine[con->posPushBack++];
    if(con->posPushBack >= strlen(curLine)) {
	/* last character on a line, so pop the line */
	free(curLine);
	con->nPushBack--;
	con->posPushBack = 0;
	if(con->nPushBack == 0) free(con->PushBack);
    }
    return c;
}

#ifdef UNUSED
int Rconn_ungetc(int c, Rconnection con)
{
    con->save2 = c;
    return c;
}
#endif

/* read one line (without trailing newline) from con and store it in buf */
/* return number of characters read, -1 on EOF */
attribute_hidden
size_t Rconn_getline(Rconnection con, char *buf, size_t bufsize)
{
    int c;
    ssize_t nbuf = -1;

    while((c = Rconn_fgetc(con)) != R_EOF) {
	if(nbuf+1 >= bufsize)
	    error(_("line longer than buffer size %lu"), (unsigned long) bufsize);
	if(c != '\n'){
	    buf[++nbuf] = (char) c; /* compiler-defined conversion behavior */
	} else {
	    buf[++nbuf] = '\0';
	    break;
	}
    }
    /* Make sure it is null-terminated and count is correct, even if
     *  file did not end with newline.
     */
    if(nbuf >= 0 && buf[nbuf]) {
	if(nbuf+1 >= bufsize)
	    error(_("line longer than buffer size %lu"), (unsigned long) bufsize);
	buf[++nbuf] = '\0';
    }
    return (size_t) nbuf;
}

int Rconn_printf(Rconnection con, const char *format, ...)
{
    int res;
    errno = 0;
    va_list(ap);
    va_start(ap, format);
    /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
    res = (con->vfprintf)(con, format, ap);
    va_end(ap);
    /* PR#17243:  write.table and friends silently failed if the disk was full (or there was another error) */
    if (res < 0) {
	if (errno)
	    error(_("Error writing to connection:  %s"), strerror(errno));
	else
	    error(_("Error writing to connection"));
    }
    return res;
}

static void con_cleanup(void *data)
{
    Rconnection con = data;
    checkClose(con);
}

/* readLines(con = stdin(), n = 1, ok = TRUE, warn = TRUE) */
#define BUF_SIZE 1000
attribute_hidden SEXP do_readLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, ans2;
    int ok, warn, skipNul, c;
    size_t nbuf, buf_size = BUF_SIZE;
    int oenc = CE_NATIVE;
    Rconnection con = NULL;
    Rboolean wasopen;
    char *buf;
    const char *encoding;
    RCNTXT cntxt;
    R_xlen_t i, n, nn, nnn, nread;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args))); args = CDR(args);
    n = asVecSize(CAR(args)); args = CDR(args);
    if(n == -999)
	error(_("invalid '%s' argument"), "n");
    ok = asLogical(CAR(args));  args = CDR(args);
    if(ok == NA_LOGICAL)
	error(_("invalid '%s' argument"), "ok");
    warn = asLogical(CAR(args));  args = CDR(args);
    if(warn == NA_LOGICAL)
	error(_("invalid '%s' argument"), "warn");
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"), "encoding");
    encoding = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args); /* ASCII */
    skipNul = asLogical(CAR(args));
    if(skipNul == NA_LOGICAL)
	error(_("invalid '%s' argument"), "skipNul");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	con->UTF8out = TRUE;  /* a request */
	strcpy(mode, con->mode);
	strcpy(con->mode, "rt");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
	if(!con->canread) error(_("cannot read from this connection"));
    } else {
	if(!con->canread) error(_("cannot read from this connection"));
	/* for a non-blocking connection, more input may
	   have become available, so re-position */
	if(con->canseek && !con->blocking)
	    Rconn_seek(con, Rconn_seek(con, -1, 1, 1), 1, 1);
    }
    con->incomplete = FALSE;
    if(con->UTF8out || streql(encoding, "UTF-8")) oenc = CE_UTF8;
    else if(streql(encoding, "latin1")) oenc = CE_LATIN1;
    else if(streql(encoding, "bytes")) oenc = CE_BYTES;

    buf = (char *) malloc(buf_size);
    if(!buf)
	error(_("cannot allocate buffer in readLines"));
    nn = (n < 0) ? 1000 : n; /* initially allocate space for 1000 lines */
    nnn = (n < 0) ? R_XLEN_T_MAX : n;
    PROTECT(ans = allocVector(STRSXP, nn));
    for(nread = 0; nread < nnn; nread++) {
	if(nread >= nn) {
	    double dnn = 2. * (double) nn;
	    if (dnn > R_XLEN_T_MAX) error("too many items");
	    ans2 = allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	nbuf = 0;
	while((c = Rconn_fgetc(con)) != R_EOF) {
	    if(nbuf == buf_size-1) {  /* need space for the terminator */
		buf_size *= 2;
		char *tmp = (char *) realloc(buf, buf_size);
		if(!tmp) {
		    free(buf);
		    error(_("cannot allocate buffer in readLines"));
		} else buf = tmp;
	    }
	    if(skipNul && c == '\0') continue;
	    if(c != '\n')
		/* compiler-defined conversion behavior */
		buf[nbuf++] = (char) c;
	    else
		break;
	}
	buf[nbuf] = '\0';
	/* Remove UTF-8 BOM */
	const char *qbuf = buf;
	// avoid valgrind warning if < 3 bytes
	if (nread == 0 && utf8locale && strlen(buf) >= 3 &&
	    !memcmp(buf, "\xef\xbb\xbf", 3)) qbuf = buf + 3;
	SET_STRING_ELT(ans, nread, mkCharCE(qbuf, oenc));
	if (warn && strlen(buf) < nbuf)
	    warning(_("line %lld appears to contain an embedded nul"),
	            (long long)nread + 1);
	if(c == R_EOF) goto no_more_lines;
    }
    if(!wasopen) {endcontext(&cntxt); con->close(con);}
    UNPROTECT(1);
    free(buf);
    return ans;
no_more_lines:
    if(!wasopen) {endcontext(&cntxt); con->close(con);}
    if(nbuf > 0) { /* incomplete last line */
	if(con->text && !con->blocking &&
	   (strcmp(con->class, "gzfile") != 0)) {
	    /* push back the rest */
	    con_pushback(con, 0, buf);
	    con->incomplete = TRUE;
	} else {
	    nread++;
	    if(warn)
		warning(_("incomplete final line found on '%s'"),
			con->description);
	}
    }
    free(buf);
    if(nread < nnn && !ok)
	error(_("too few lines read in readLines"));
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    return ans2;
}

/* writeLines(text, con = stdout(), sep = "\n", useBytes) */
attribute_hidden SEXP do_writelines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int con_num, useBytes;
    Rboolean wasopen;
    Rconnection con=NULL;
    const char *ssep;
    SEXP text, sep;
    RCNTXT cntxt;

    checkArity(op, args);
    text = CAR(args);
    if(!isString(text)) error(_("invalid '%s' argument"), "text");
    if(!inherits(CADR(args), "connection"))
	error(_("'con' is not a connection"));
    con_num = asInteger(CADR(args));
    con = getConnection(con_num);
    sep = CADDR(args);
    if(!isString(sep)) error(_("invalid '%s' argument"), "sep");
    useBytes = asLogical(CADDDR(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	/* Documented behaviour */
	strcpy(mode, con->mode);
	strcpy(con->mode, "wt");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canwrite) error(_("cannot write to this connection"));
    /* NB: translateChar0() is the same as CHAR() for IS_BYTES strings */
    if(useBytes)
	ssep = CHAR(STRING_ELT(sep, 0));
    else
	ssep = translateChar0(STRING_ELT(sep, 0));

    /* New for 2.7.0: split the output if sink was split.
       It would be slightly simpler just to call Rvprintf if the
       connection was stdout(), but this way is more efficient */
    if(con_num == R_OutputCon) {
	int j = 0;
	Rconnection con0;
	do {
	    con0 = getConnection(con_num);
	    for(R_xlen_t i = 0; i < XLENGTH(text); i++)
		Rconn_printf(con0, "%s%s",
			     useBytes ? CHAR(STRING_ELT(text, i)) :
			     translateChar0(STRING_ELT(text, i)), ssep);
	    con0->fflush(con0);
	    con_num = getActiveSink(j++);
	} while (con_num > 0);
    } else {
	for(R_xlen_t i = 0; i < XLENGTH(text); i++)
	    Rconn_printf(con, "%s%s",
			 useBytes ? CHAR(STRING_ELT(text, i)) :
			 translateChar0(STRING_ELT(text, i)), ssep);
    }

    if(!wasopen) {
    	endcontext(&cntxt);
    	checkClose(con);
    }
    return R_NilValue;
}

/* ------------------- read, write  binary --------------------- */

static void swapb(void *result, int size)
{
    int i;
    char *p = result, tmp;

    if (size == 1) return;
    for (i = 0; i < size/2; i++) {
	tmp = p[i];
	p[i] = p[size - i - 1];
	p[size - i - 1] = tmp;
    }
}

static SEXP readOneString(Rconnection con)
{
    char buf[10001], *p;
    int pos, m;

    for(pos = 0; pos < 10000; pos++) {
	p = buf + pos;
	m = (int) con->read(p, sizeof(char), 1, con);
	if (m < 0) error("error reading from the connection");
	if(!m) {
	    if(pos > 0)
		warning(_("incomplete string at end of file has been discarded"));
	    return R_NilValue;
	}
	if(*p == '\0') break;
    }
    if(pos == 10000)
	warning(_("null terminator not found: breaking string at 10000 bytes"));
    return mkChar(buf);
}

static R_xlen_t
rawRead(char *p, int size, R_xlen_t n, Rbyte *bytes, R_xlen_t nbytes, R_xlen_t *np)
{
    R_xlen_t avail, m;

    avail = (nbytes - *np)/size;
    m = n;
    if (m > avail) m = avail;
    if (m > 0) {
	if (size) memcpy(p, bytes + *(np), m*size);
	*np += m*size;
    }
    return m;
}

static SEXP rawOneString(Rbyte *bytes, R_xlen_t nbytes, R_xlen_t *np)
{
    Rbyte *p;
    R_xlen_t i;
    char *buf;
    SEXP res;

    /* just look for null terminator */
    for(i = *np, p = bytes+(*np); i < nbytes; p++, i++)
	if(*p == '\0') break;
    if(i < nbytes) { /* has terminator */
	p = bytes+(*np);
	*np = i+1;
	return mkChar((char *)p);
    }
    /* so no terminator */
    buf = R_chk_calloc(nbytes - (*np) + 1, 1);
    memcpy(buf, bytes+(*np), nbytes-(*np));
    res = mkChar(buf);
    R_Free(buf);
    *np = nbytes;
    return res;
}

/* readBin(con, what, n, swap) */
#define BLOCK 8096
attribute_hidden SEXP do_readbin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, swhat;
    int size, signd, swap, sizedef= 4, mode = 1;
    const char *what;
    void *p = NULL;
    Rboolean wasopen = TRUE, isRaw = FALSE;
    Rconnection con = NULL;
    Rbyte *bytes = NULL;
    RCNTXT cntxt;
    R_xlen_t i, n,  m = 0, nbytes = 0, np = 0;

    checkArity(op, args);

    if(TYPEOF(CAR(args)) == RAWSXP) {
	isRaw = TRUE;
	bytes = RAW(CAR(args));
	nbytes = XLENGTH(CAR(args));
    } else {
	con = getConnection(asInteger(CAR(args)));
	if(con->text) error(_("can only read from a binary connection"));
    }

    args = CDR(args);
    swhat = CAR(args); args = CDR(args);
    if(!isString(swhat) || LENGTH(swhat) != 1)
	error(_("invalid '%s' argument"), "what");
    what = CHAR(STRING_ELT(swhat, 0)); /* ASCII */
    n = asVecSize(CAR(args)); args = CDR(args);
    if(n < 0) error(_("invalid '%s' argument"), "n");
    size = asInteger(CAR(args)); args = CDR(args);
    signd = asLogical(CAR(args)); args = CDR(args);
    if(signd == NA_LOGICAL)
	error(_("invalid '%s' argument"), "signed");
    swap = asLogical(CAR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid '%s' argument"), "swap");
    if(!isRaw) {
	wasopen = con->isopen;
	if(!wasopen) {
	    /* Documented behaviour */
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "rb");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    /* Set up a context which will close the connection on error */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			 R_NilValue, R_NilValue);
	    cntxt.cend = &con_cleanup;
	    cntxt.cenddata = con;
	}
	if(!con->canread) error(_("cannot read from this connection"));
    }
    if(!strcmp(what, "character")) {
	SEXP onechar;
	PROTECT(ans = allocVector(STRSXP, n));
	for(i = 0, m = 0; i < n; i++) {
	    onechar = isRaw ? rawOneString(bytes, nbytes, &np)
		: readOneString(con);
	    if(onechar != R_NilValue) {
		SET_STRING_ELT(ans, i, onechar);
		m++;
	    } else break;
	}
    } else if(!strcmp(what, "complex")) {
	if(size == NA_INTEGER) size = sizeof(Rcomplex);
	if(size != sizeof(Rcomplex))
	    error(_("size changing is not supported for complex vectors"));
	PROTECT(ans = allocVector(CPLXSXP, n));
	p = (void *) COMPLEX(ans);
	if(isRaw) m = rawRead(p, size, n, bytes, nbytes, &np);
	else {
	    /* Do this in blocks to avoid large buffers in the connection */
	    char *pp = p;
	    R_xlen_t m0, n0 = n;
	    m = 0;
	    while(n0) {
		size_t n1 = (n0 < BLOCK) ? n0 : BLOCK;
		m0 = con->read(pp, size, n1, con);
		if (m0 < 0) error("error reading from the connection");
		m += m0;
		if (m0 < n1) break;
		n0 -= n1;
		pp += n1 * size;
	    }
	}
	if(swap)
	    for(i = 0; i < m; i++) {
		swapb(&(COMPLEX(ans)[i].r), sizeof(double));
		swapb(&(COMPLEX(ans)[i].i), sizeof(double));
	    }
    } else {
	if (!strcmp(what, "integer") || !strcmp(what, "int")) {
	    sizedef = sizeof(int); mode = 1;

#if (SIZEOF_LONG == 8) && (SIZEOF_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(long):
#elif (SIZEOF_LONG_LONG == 8) && (SIZEOF_LONG_LONG > SIZEOF_INT)
#  define CASE_LONG_ETC case sizeof(_lli_t):
#else
#  define CASE_LONG_ETC
#endif

#define CHECK_INT_SIZES(SIZE, DEF) do {					\
	    if(SIZE == NA_INTEGER) SIZE = DEF;				\
	    switch (SIZE) {						\
	    case sizeof(signed char):					\
	    case sizeof(short):						\
	    case sizeof(int):						\
	    CASE_LONG_ETC						\
		break;							\
	    default:							\
		error(_("size %d is unknown on this machine"), SIZE);	\
	    }								\
	} while(0)

	    CHECK_INT_SIZES(size, sizedef);
	    PROTECT(ans = allocVector(INTSXP, n));
	    p = (void *) INTEGER(ans);
	} else if (!strcmp(what, "logical")) {
	    sizedef = sizeof(int); mode = 1;
	    CHECK_INT_SIZES(size, sizedef);
	    PROTECT(ans = allocVector(LGLSXP, n));
	    p = (void *) LOGICAL(ans);
	} else if (!strcmp(what, "raw")) {
	    sizedef = 1; mode = 1;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case 1:
		break;
	    default:
		error(_("raw is always of size 1"));
	    }
	    PROTECT(ans = allocVector(RAWSXP, n));
	    p = (void *) RAW(ans);
	} else if (!strcmp(what, "numeric") || !strcmp(what, "double")) {
	    sizedef = sizeof(double); mode = 2;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
#endif
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    PROTECT(ans = allocVector(REALSXP, n));
	    p = (void *) REAL(ans);
	} else
	    error(_("invalid '%s' argument"), "what");

	if(!signd && (mode != 1 || size > 2))
	    warning(_("'signed = FALSE' is only valid for integers of sizes 1 and 2"));
	if(size == sizedef) {
	    if(isRaw) {
		m = rawRead(p, size, n, bytes, nbytes, &np);
	    } else {
		/* Do this in blocks to avoid large buffers in the connection */
		char *pp = p;
		R_xlen_t m0, n0 = n;
		m = 0;
		while(n0) {
		    size_t n1 = (n0 < BLOCK) ? n0 : BLOCK;
		    m0 = con->read(pp, size, n1, con);
		    if (m0 < 0) error("error reading from the connection");
		    m += m0;
		    if (m0 < n1) break;
		    n0 -= n1;
		    pp += n1 * size;
		}
	    }
	    if(swap && size > 1)
		for(i = 0; i < m; i++) swapb((char *)p+i*size, size);
	} else {
	    R_xlen_t s;
	    union {
		signed char sch;
		unsigned char uch;
		signed short ssh;
		unsigned short ush;
		long l;
		long long ll;
		float f;
#if HAVE_LONG_DOUBLE
		long double ld;
#endif
	    } u;
	    if (size > sizeof u)
		error(_("size %d is unknown on this machine"), size);
	    if(mode == 1) { /* integer result */
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead((char*) &u, size, 1, bytes, nbytes, &np)
			: (int) con->read((char*) &u, size, 1, con);
		    if (s < 0) error("error reading from the connection");
		    if(s) m++; else break;
		    if(swap && size > 1) swapb((char *) &u, size);
		    switch(size) {
		    case sizeof(signed char):
			if(signd)
			    INTEGER(ans)[i] = u.sch;
			else
			    INTEGER(ans)[i] = u.uch;
			break;
		    case sizeof(short):
			if(signd)
			    INTEGER(ans)[i] = u.ssh;
			else
			    INTEGER(ans)[i] = u.ush;
			break;
#if SIZEOF_LONG == 8
		    case sizeof(long):
			INTEGER(ans)[i] = (int) u.l;
			break;
#elif SIZEOF_LONG_LONG == 8
		    case sizeof(_lli_t):
			INTEGER(ans)[i] = (int) u.ll;
			break;
#endif
		    default:
			error(_("size %d is unknown on this machine"), size);
		    }
		}
	    } else if (mode == 2) { /* double result */
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead((char*) &u, size, 1, bytes, nbytes, &np)
			: (int) con->read((char*) &u, size, 1, con);
		    if (s < 0) error("error reading from the connection");
		    if(s) m++; else break;
		    if(swap && size > 1) swapb((char *) &u, size);
		    switch(size) {
		    case sizeof(float):
			REAL(ans)[i] = u.f;
			break;
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
		    case sizeof(long double):
			REAL(ans)[i] = (double) u.ld;
			break;
#endif
		    default:
			error(_("size %d is unknown on this machine"), size);
		    }
		}
	    }
	}
    }
    if(!wasopen) {endcontext(&cntxt); con->close(con);}
    if(m < n)
	ans = xlengthgets(ans, m);
    UNPROTECT(1);
    return ans;
}

/* writeBin(object, con, size, swap, useBytes) */
attribute_hidden SEXP do_writebin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP object = CAR(args);
    if(!isVectorAtomic(object))
	error(_("'x' is not an atomic vector type"));
    Rboolean
	isRaw = TYPEOF(CADR(args)) == RAWSXP,
	wasopen = isRaw;
    Rconnection con = NULL;
    if(!isRaw) {
	con = getConnection(asInteger(CADR(args)));
	if(con->text) error(_("can only write to a binary connection"));
	wasopen = con->isopen;
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    int size = asInteger(CADDR(args)),
	swap = asLogical(CADDDR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid '%s' argument"), "swap");
    int useBytes = asLogical(CAD4R(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");
    R_xlen_t i, len = XLENGTH(object);
    if(len == 0)
	return (isRaw) ? allocVector(RAWSXP, 0) : R_NilValue;

#ifndef LONG_VECTOR_SUPPORT
    /* without long vectors RAW vectors are limited to 2^31 - 1 bytes */
    if(len * (double)size > INT_MAX) {
	if(isRaw)
	    error(_("only 2^31-1 bytes can be written to a raw vector"));
	else
	    error(_("only 2^31-1 bytes can be written in a single writeBin() call"));
    }
#endif

    RCNTXT cntxt;
    if(!wasopen) {
	/* Documented behaviour */
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    SEXP ans = R_NilValue;
    if(TYPEOF(object) == STRSXP) {
	const char *s;
	if(isRaw) {
	    Rbyte *bytes;
	    size_t np, outlen = 0;
	    if(useBytes)
		for(i = 0; i < len; i++)
		    outlen += strlen(CHAR(STRING_ELT(object, i))) + 1;
	    else
		for(i = 0; i < len; i++)
		    outlen += strlen(translateChar0(STRING_ELT(object, i))) + 1;
	    PROTECT(ans = allocVector(RAWSXP, outlen));
	    bytes = RAW(ans);
	    /* translateChar0() is the same as CHAR for IS_BYTES strings */
	    for(i = 0, np = 0; i < len; i++) {
		if(useBytes)
		    s = CHAR(STRING_ELT(object, i));
		else
		    s = translateChar0(STRING_ELT(object, i));
		memcpy(bytes+np, s, strlen(s) + 1);
		np +=  strlen(s) + 1;
	    }
	} else {
	    /* translateChar0() is the same as CHAR for IS_BYTES strings */
	    for(i = 0; i < len; i++) {
		if(useBytes)
		    s = CHAR(STRING_ELT(object, i));
		else
		    s = translateChar0(STRING_ELT(object, i));
		size_t nwrite = con->write(s, sizeof(char), strlen(s) + 1, con);
		if(!nwrite) {
		    warning(_("problem writing to connection"));
		    break;
		}
	    }
	}
    } else {
	switch(TYPEOF(object)) {
	case LGLSXP:
	case INTSXP:
	    CHECK_INT_SIZES(size, sizeof(int));
	    break;
	case REALSXP:
	    if(size == NA_INTEGER) size = sizeof(double);
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
#endif
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case CPLXSXP:
	    if(size == NA_INTEGER) size = sizeof(Rcomplex);
	    if(size != sizeof(Rcomplex))
		error(_("size changing is not supported for complex vectors"));
	    break;
	case RAWSXP:
	    if(size == NA_INTEGER) size = 1;
	    if(size != 1)
		error(_("size changing is not supported for raw vectors"));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("writeBin", object);
	}
	char *buf = R_chk_calloc(len, size);
	R_xlen_t j;
	switch(TYPEOF(object)) {
	case LGLSXP:
	case INTSXP:
	    switch (size) {
	    case sizeof(int):
		memcpy(buf, INTEGER(object), size * len);
		break;
#if SIZEOF_LONG == 8
	    case sizeof(long):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    long l1 = (long) INTEGER(object)[i];
		    memcpy(buf + j, &l1, size);
		}
		break;
	    }
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    _lli_t ll1 = (_lli_t) INTEGER(object)[i];
		    memcpy(buf + j, &ll1, size);
		}
		break;
	    }
#endif
	    case 2:
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    short s1 = (short) INTEGER(object)[i];
		    memcpy(buf + j, &s1, size);
		}
		break;
	    }
	    case 1:
		for (i = 0; i < len; i++)
		    /* compiler-defined conversion behavior */
		    buf[i] = (signed char) INTEGER(object)[i];
		break;
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case REALSXP:
	    switch (size) {
	    case sizeof(double):
		memcpy(buf, REAL(object), size * len);
		break;
	    case sizeof(float):
	    {
		for (i = 0, j = 0; i < len; i++, j += size) {
		    float f1 = (float) REAL(object)[i];
		    memcpy(buf+j, &f1, size);
		}
		break;
	    }
#if HAVE_LONG_DOUBLE && (SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE)
	    case sizeof(long double):
	    {
		/* some systems have problems with memcpy from
		   the address of an automatic long double,
		   e.g. ix86/x86_64 Linux with gcc4 */
		static long double ld1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    ld1 = (long double) REAL(object)[i];
		    memcpy(buf+j, &ld1, size);
		}
		break;
	    }
#endif
	    default:
		error(_("size %d is unknown on this machine"), size);
	    }
	    break;
	case CPLXSXP:
	    memcpy(buf, COMPLEX(object), size * len);
	    break;
	case RAWSXP:
	    memcpy(buf, RAW(object), len); /* size = 1 */
	    break;
	}

	if(swap && size > 1) {
	    if (TYPEOF(object) == CPLXSXP)
		for(i = 0; i < len; i++) {
		    int sz = size/2;
		    swapb(buf+sz*2*i, sz);
		    swapb(buf+sz*(2*i+1), sz);
		}
	    else
		for(i = 0; i < len; i++) swapb(buf+size*i, size);
	}

	/* write it now */
	if(isRaw) { /* for non-long vectors, we checked size*len < 2^31-1 above */
	    PROTECT(ans = allocVector(RAWSXP, size*len));
	    memcpy(RAW(ans), buf, size*len);
	} else {
	    size_t nwrite = con->write(buf, size, len, con);
	    if(nwrite < len) warning(_("problem writing to connection"));
	}
	R_Free(buf);
    }

    if(!wasopen) {
        endcontext(&cntxt);
        checkClose(con);
    }
    if(isRaw) {
	UNPROTECT(1);
	R_Visible = TRUE;
    } else R_Visible = FALSE;
    return ans;
}

/* FIXME: could do any MBCS locale, but would need pushback */
static SEXP
readFixedString(Rconnection con, int len, int useBytes, Rboolean *warnOnNul)
{
    SEXP ans;
    char *buf;
    int  m;
    const void *vmax = vmaxget();

    if(utf8locale && !useBytes) {
	R_SIZE_T count = R_MB_CUR_MAX * (R_SIZE_T)len +1;
	char *p = buf = (char *) R_alloc(count, sizeof(char));
	memset(buf, 0, count);
	mbstate_t mb_st;
	mbs_init(&mb_st);
	for(int i = 0; i < len; i++) {
	    char *q = p;
	    m = (int) con->read(p, sizeof(char), 1, con);
	    if(!m) { if(i == 0) return R_NilValue; else break;}
	    int clen = utf8clen(*p++);
	    if(clen > 1) {
		m = (int) con->read(p, sizeof(char), clen - 1, con);
		if(m < clen - 1) error(_("invalid UTF-8 input in readChar()"));
		p += clen - 1;
		/* NB: this only checks validity of multi-byte characters */
		if((int)mbrtowc(NULL, q, clen, &mb_st) < 0)
		    error(_("invalid UTF-8 input in readChar()"));
	    } else if (*q == '\0' && *warnOnNul) {
		*warnOnNul = FALSE;
		warning(_("truncating string with embedded nuls"));
	    }
	}
	*p = '\0';
    } else {
	buf = (char *) R_alloc(len+1, sizeof(char));
	if (len+1)
	    memset(buf, 0, len+1);
	m = (int) con->read(buf, sizeof(char), len, con);
	if(len && !m) return R_NilValue;
	buf[m] = '\0';
	if (strlen(buf) < m && *warnOnNul) {
	    *warnOnNul = FALSE;
	    warning(_("truncating string with embedded nuls"));
	}
    }
    /* String may contain nuls which we now (R >= 2.8.0) assume to be
       padding and ignore */
    ans = mkChar(buf);
    vmaxset(vmax);
    return ans;
}

static SEXP
rawFixedString(Rbyte *bytes, int len, int nbytes, int *np, int useBytes)
{
    if(*np + len > nbytes) {
	len = nbytes - *np;
	if (!len) return(R_NilValue);
    }

    SEXP res;
    const void *vmax = vmaxget();
    char *buf;

    /* Note: mkCharLenCE signals an error on embedded nuls. */
    if(utf8locale && !useBytes) {
	int i, clen, iread = *np;
	char *p = buf = (char *) R_alloc(R_MB_CUR_MAX*(R_SIZE_T)len +1, sizeof(char));
	for(i = 0; i < len; i++, p += clen, iread += clen) {
	    if (iread >= nbytes) break;
	    Rbyte *q = bytes + iread;
	    clen = utf8clen(*q);
	    if (iread + clen > nbytes)
		error(_("invalid UTF-8 input in readChar()"));
	    memcpy(p, q, clen);
	}
	clen = iread - (*np);
	*np = iread;
	*p = '\0';
	res = mkCharLenCE(buf, clen, CE_NATIVE);
    } else {
	/* no terminator */
	buf = R_chk_calloc(len + 1, 1);
	if (len)
	    memcpy(buf, bytes + (*np), len);
	*np += len;
	res = mkCharLenCE(buf, len, CE_NATIVE);
	R_Free(buf);
    }
    vmaxset(vmax);
    return res;
}


/* readChar(con, nchars) */
attribute_hidden SEXP do_readchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, onechar, nchars;
    R_xlen_t i, n, m = 0;
    int nbytes = 0, np = 0, useBytes;
    Rboolean wasopen = TRUE, isRaw = FALSE, warnOnNul = TRUE;
    Rconnection con = NULL;
    Rbyte *bytes = NULL;
    RCNTXT cntxt;
    checkArity(op, args);

    if(TYPEOF(CAR(args)) == RAWSXP) {
	isRaw = TRUE;
	bytes = RAW(CAR(args));
	nbytes = LENGTH(CAR(args));
    } else {
	con = getConnection(asInteger(CAR(args)));
	if(!con->canread)
	    error(_("cannot read from this connection"));
    }
    /* We did as.integer in the wrapper */
    nchars = CADR(args);
    n = XLENGTH(nchars);
    if(n == 0) return allocVector(STRSXP, 0);
    useBytes = asLogical(CADDR(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    if (!isRaw) {
	wasopen = con->isopen;
	if(!wasopen) {
	    /* Documented behaviour */
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "rb");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    /* Set up a context which will close the connection on error */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			 R_NilValue, R_NilValue);
	    cntxt.cend = &con_cleanup;
	    cntxt.cenddata = con;
	}
	if(!con->canread) error(_("cannot read from this connection"));
    }
    if (mbcslocale && !utf8locale && !useBytes)
	warning(_("can only read in bytes in a non-UTF-8 MBCS locale" ));
    PROTECT(ans = allocVector(STRSXP, n));
    if(!isRaw && con->text &&
       (con->buff || con->nPushBack >= 0 || con->inconv))

	/* could be turned into runtime error */
	warning(_("text connection used with %s(), results may be incorrect"),
	          "readChar");
    for(i = 0, m = 0; i < n; i++) {
	int len = INTEGER(nchars)[i];
	if(len == NA_INTEGER || len < 0)
	    error(_("invalid '%s' argument"), "nchars");
	onechar = isRaw ? rawFixedString(bytes, len, nbytes, &np, useBytes)
	    : readFixedString(con, len, useBytes, &warnOnNul);
	if(onechar != R_NilValue) {
	    SET_STRING_ELT(ans, i, onechar);
	    m++;
	} else break;
    }

    if(!wasopen) {endcontext(&cntxt); con->close(con);}
    if(m < n) {
	PROTECT(ans = xlengthgets(ans, m));
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

/* writeChar(object, con, nchars, sep, useBytes) */
attribute_hidden SEXP do_writechar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object, nchars, sep, ans = R_NilValue, si;
    R_xlen_t i, n, len;
    int useBytes;
    size_t slen, tlen, lenb, lenc;
    char *buf;
    const char *s, *ssep = "";
    Rboolean wasopen = TRUE, usesep, isRaw = FALSE;
    Rconnection con = NULL;
    mbstate_t mb_st;
    RCNTXT cntxt;

    checkArity(op, args);
    object = CAR(args);
    if(TYPEOF(object) != STRSXP)
	error(_("invalid '%s' argument"), "object");
    if(TYPEOF(CADR(args)) == RAWSXP) {
	isRaw = TRUE;
    } else {
	con = getConnection(asInteger(CADR(args)));
	if(!con->canwrite)
	    error(_("cannot write to this connection"));
	wasopen = con->isopen;
    }

    /* We did as.integer in the wrapper */
    nchars = CADDR(args);
    sep = CADDDR(args);
    useBytes = asLogical(CAD4R(args));
    if(useBytes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useBytes");

    if(isNull(sep)) {
	usesep = FALSE;
	slen = 0;
    } else {
	usesep = TRUE;
	if (!isString(sep) || LENGTH(sep) != 1)
	    error(_("invalid '%s' argument"), "sep");
	if(useBytes)
	    ssep = CHAR(STRING_ELT(sep, 0));
	else
	    ssep = translateChar(STRING_ELT(sep, 0));
	slen = strlen(ssep) + 1;
    }
    n = XLENGTH(nchars);
    if(XLENGTH(object) < n)
	error(_("'object' is too short"));
    if(n == 0) {
	if(isRaw) return allocVector(RAWSXP, 0); else return R_NilValue;
    }

    len = 0;
    if (!isRaw) {
	for(i = 0; i < n; i++) {
	    /* This is not currently needed, just future-proofing in case
	       the logic gets changed */
	    if(useBytes)
		tlen = strlen(CHAR(STRING_ELT(object, i)));
	    else
		tlen = strlen(translateChar(STRING_ELT(object, i)));
	    if (tlen > len) len = tlen;
	    int tt = INTEGER(nchars)[i];
	    if(tt == NA_INTEGER || tt < 0)
		error(_("invalid '%s' argument"), "nchars");
	    if (tt > len) len = tt;
	}
	buf = (char *) R_alloc(len + slen, sizeof(char));
    } else {
	double dlen = 0;
	for (i = 0; i < n; i++)
	    dlen += (double)(INTEGER(nchars)[i] + slen);
	if (dlen > R_XLEN_T_MAX)
	    error("too much data for a raw vector on this platform");
	len = (R_xlen_t) dlen;
	PROTECT(ans = allocVector(RAWSXP, len));
	buf = (char*) RAW(ans);
    }

    if(!wasopen) {
	/* Documented behaviour */
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
	if(!con->canwrite) error(_("cannot write to this connection"));
    }

    if(!isRaw && con->text && con->outconv)
	/* could be turned into runtime error */
	warning(_("text connection used with %s(), results may be incorrect"),
	          "writeChar");

    for(i = 0; i < n; i++) {
	len = INTEGER(nchars)[i];
	si = STRING_ELT(object, i);
	if(strlen(CHAR(si)) < LENGTH(si)) {
	    if(len > LENGTH(si)) {
		warning(_("writeChar: more bytes requested than are in the string - will zero-pad"));
	    }
	    if (len + slen)
		memset(buf, '\0', len + slen);
	    if (len)
		memcpy(buf, CHAR(si), len);
	    if (usesep) {
		strcpy(buf + len, ssep);
		len += slen;
	    }
	    if (!isRaw) {
		size_t nwrite = con->write(buf, sizeof(char), len, con);
		if(!nwrite) {
		    warning(_("problem writing to connection"));
		    break;
		}
	    } else
		buf += len;
	} else {
	    if(useBytes)
		s = CHAR(si);
	    else
		s = translateChar(si);
	    lenb = lenc = strlen(s);
	    if(mbcslocale) lenc = mbstowcs(NULL, s, 0);
	    /* As from 1.8.1, zero-pad if too many chars are requested. */
	    if(len > lenc) {
		warning(_("writeChar: more characters requested than are in the string - will zero-pad"));
		lenb += (len - lenc);
	    }
	    if(len < lenc) {
		if(mbcslocale) {
		    /* find out how many bytes we need to write */
		    size_t i, used;
		    const char *p = s;
		    mbs_init(&mb_st);
		    for(i = 0, lenb = 0; i < len; i++) {
			used = Mbrtowc(NULL, p, R_MB_CUR_MAX, &mb_st);
			p += used;
			lenb += used;
		    }
		} else
		    lenb = len;
	    }
	    if (lenb + slen)
		memset(buf, '\0', lenb + slen);
	    strncpy(buf, s, lenb);
	    if (usesep) {
		strcpy(buf + lenb, ssep);
		lenb += slen;
	    }
	    if (!isRaw) {
		size_t nwrite = con->write(buf, sizeof(char), lenb, con);
		if(!nwrite) {
		    warning(_("problem writing to connection"));
		    break;
		}
	    } else
		buf += lenb;
	}
    }
    if(!wasopen) {
        endcontext(&cntxt);
        checkClose(con);
    }
    if(isRaw) {
	UNPROTECT(1);
	R_Visible = TRUE;
    } else {
	ans = R_NilValue;
	R_Visible = FALSE;
    }
    return ans;
}

/* ------------------- push back text  --------------------- */


/* used in readLines and scan */
void con_pushback(Rconnection con, Rboolean newLine, char *line)
{
    int nexists = con->nPushBack;
    char **q;

    if (nexists == INT_MAX)
	error(_("maximum number of pushback lines exceeded"));
    if(nexists > 0) {
	q = (char **) realloc(con->PushBack, (nexists+1)*sizeof(char *));
    } else {
	q = (char **) malloc(sizeof(char *));
    }
    if(!q) error(_("could not allocate space for pushback"));
    else con->PushBack = q;
    q += nexists;
    *q = (char *) malloc(strlen(line) + 1 + newLine);
    if(!(*q)) error(_("could not allocate space for pushback"));
    strcpy(*q, line);
    if(newLine) strcat(*q, "\n");
    q++;
    con->posPushBack = 0;
    con->nPushBack++;
}


attribute_hidden SEXP do_pushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, n, nexists, newLine, type;
    Rconnection con = NULL;
    SEXP stext;
    const char *p;
    char **q;

    checkArity(op, args);

    stext = CAR(args);
    if(!isString(stext))
	error(_("invalid '%s' argument"), "data");
    con = getConnection(asInteger(CADR(args)));
    newLine = asLogical(CADDR(args));
    if(newLine == NA_LOGICAL)
	error(_("invalid '%s' argument"), "newLine");
    type = asInteger(CADDDR(args));
    if(!con->canread && !con->isopen)
	error(_("can only push back on open readable connections"));
    if(!con->text)
	error(_("can only push back on text-mode connections"));
    nexists = con->nPushBack;
    if((n = LENGTH(stext)) > 0) {
	if(nexists > 0)
	    q = (char **) realloc(con->PushBack, (n+nexists)*sizeof(char *));
	else
	    q = (char **) malloc(n*sizeof(char *));
	if(!q) error(_("could not allocate space for pushback"));
	con->PushBack = q;
	q += nexists;
	for(i = 0; i < n; i++) {
	    p = type == 1 ? translateChar(STRING_ELT(stext, n - i - 1))
			  : ((type == 3) ? translateCharUTF8(STRING_ELT(stext, n - i - 1))
					 : CHAR(STRING_ELT(stext, n - i - 1)));
	    *q = (char *) malloc(strlen(p) + 1 + newLine);
	    if(!(*q)) error(_("could not allocate space for pushback"));
	    strcpy(*q, p);
	    if(newLine) strcat(*q, "\n");
	    q++;
	}
	con->posPushBack = 0;
	con->nPushBack += n;
    }
    return R_NilValue;
}

attribute_hidden SEXP do_pushbacklength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    return ScalarInteger(con->nPushBack);
}

attribute_hidden SEXP do_clearpushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int j;
    Rconnection con = NULL;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));

    if(con->nPushBack > 0) { // so con_close1 has not been called
	for(j = 0; j < con->nPushBack; j++) free(con->PushBack[j]);
	free(con->PushBack);
	con->nPushBack = 0;
    }
    return R_NilValue;
}

/* ------------------- sink functions  --------------------- */

/* Switch output to connection number icon, or pop stack if icon < 0
 */

static Rboolean
switch_or_tee_stdout(int icon, int closeOnExit, int tee)
{
    int toclose;

    if(icon == R_OutputCon) return FALSE;

    if(icon >= 0 && R_SinkNumber >= NSINKS - 1)
	error(_("sink stack is full"));

    if(icon == 0)
	error(_("cannot switch output to stdin"));
    else if(icon == 1 || icon == 2) {
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	R_SinkSplit[R_SinkNumber] = tee;
	SinkConsClose[R_SinkNumber] = 0;
    } else if(icon >= 3) {
	Rconnection con = getConnection(icon); /* checks validity */
	toclose = 2*closeOnExit;
	if(!con->isopen) {
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "wt");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    if(!con->canwrite) {
		con->close(con);
		error(_("cannot write to this connection"));
	    }
	    toclose = 1;
	} else if(!con->canwrite)
	    error(_("cannot write to this connection"));
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	SinkConsClose[R_SinkNumber] = toclose;
	R_SinkSplit[R_SinkNumber] = tee;
	R_PreserveObject(con->ex_ptr);
   } else { /* removing a sink */
	if (R_SinkNumber <= 0) {
	    warning(_("no sink to remove"));
	    return FALSE;
	} else {
	    R_OutputCon = SinkCons[--R_SinkNumber];
	    if((icon = SinkCons[R_SinkNumber + 1]) >= 3) {
		Rconnection con = getConnection(icon);
		R_ReleaseObject(con->ex_ptr);
		if(SinkConsClose[R_SinkNumber + 1] == 1) { /* close it */
		    checkClose(con);
		} else if (SinkConsClose[R_SinkNumber + 1] == 2) /* destroy it */
		    con_destroy(icon);
	    }
	}
    }
    return TRUE;
}

/* This is only used by cat() */
attribute_hidden Rboolean switch_stdout(int icon, int closeOnExit)
{
  return switch_or_tee_stdout(icon, closeOnExit, 0);
}

attribute_hidden SEXP do_sink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  int icon, closeOnExit, errcon, tee;

    checkArity(op, args);
    icon = asInteger(CAR(args));
    closeOnExit = asLogical(CADR(args));
    if(closeOnExit == NA_LOGICAL)
	error(_("invalid '%s' argument"), "closeOnExit");
    errcon = asLogical(CADDR(args));
    if(errcon == NA_LOGICAL) error(_("invalid '%s' argument"), "type");
    tee = asLogical(CADDDR(args));
    if(tee == NA_LOGICAL) error(_("invalid '%s' argument"), "split");

    if(!errcon) {
	/* allow space for cat() to use sink() */
	if(icon >= 0 && R_SinkNumber >= NSINKS - 2)
	    error(_("sink stack is full"));
	switch_or_tee_stdout(icon, closeOnExit, tee);
    } else {
	if(icon < 0 || icon == 2) {
	    if (R_ErrorCon > 2)
		R_ReleaseObject(getConnection(R_ErrorCon)->ex_ptr);
	    R_ErrorCon = 2;
	} else {
	    Rconnection con = getConnection(icon); /* check validity */
	    R_ErrorCon = icon;
	    if (icon > 2)
		R_PreserveObject(con->ex_ptr);
	}
    }

    return R_NilValue;
}

attribute_hidden SEXP do_sinknumber(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int errcon;
    checkArity(op, args);

    errcon = asLogical(CAR(args));
    if(errcon == NA_LOGICAL)
	error(_("invalid '%s' argument"), "type");
    return ScalarInteger(errcon ? R_SinkNumber : R_ErrorCon);
}

#ifdef Win32
void WinCheckUTF8(void)
{
    if(EmitEmbeddedUTF8) /* RGui */
	WinUTF8out = (SinkCons[R_SinkNumber] == 1 ||
	              SinkCons[R_SinkNumber] == 2) && localeCP != 65001;
    else
	WinUTF8out = FALSE;
}
#endif

/* ------------------- admin functions  --------------------- */

attribute_hidden void R_SetNconn(int nconn)
{
    if (nconn > 128) NCONNECTIONS = nconn;
}


attribute_hidden void InitConnections(void)
{
    Connections = (Rconnection *) malloc(NCONNECTIONS * sizeof(Rconnection));
    if (!Connections)
	R_Suicide("could not allocate space for the connections table");
    Connections[0] = newterminal("stdin", "r");
    Connections[0]->fgetc = stdin_fgetc;
    Connections[1] = newterminal("stdout", "w");
    Connections[1]->vfprintf = stdout_vfprintf;
    Connections[1]->fflush = stdout_fflush;
    Connections[2] = newterminal("stderr", "w");
    Connections[2]->vfprintf = stderr_vfprintf;
    Connections[2]->fflush = stderr_fflush;
    for(int i = 3; i < NCONNECTIONS; i++) Connections[i] = NULL;
    R_OutputCon = 1;
    R_SinkNumber = 0;
    SinkCons[0] = 1; R_ErrorCon = 2;
}

attribute_hidden SEXP
do_getallconnections(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j=0, n=0;
    SEXP ans;
    checkArity(op, args);
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i]) n++;
    PROTECT(ans = allocVector(INTSXP, n));
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i])
	    INTEGER(ans)[j++] = i;
    UNPROTECT(1);
    return ans;
}

attribute_hidden SEXP
do_getconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    int what;
    Rconnection con;

    checkArity(op, args);
    what = asInteger(CAR(args));
    if (what == NA_INTEGER)
	error(_("there is no connection NA"));
    if (what < 0 || what >= NCONNECTIONS || !Connections[what])
	error(_("there is no connection %d"), what);

    con = Connections[what];
    PROTECT(ans = ScalarInteger(what));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    if (what > 2)
	setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_sumconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, names, tmp;
    Rconnection Rcon;

    checkArity(op, args);
    Rcon = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(VECSXP, 7));
    PROTECT(names = allocVector(STRSXP, 7));
    SET_STRING_ELT(names, 0, mkChar("description"));
    PROTECT(tmp = allocVector(STRSXP, 1));
    if(Rcon->enc == CE_UTF8)
	SET_STRING_ELT(tmp, 0, mkCharCE(Rcon->description, CE_UTF8));
    else
	SET_STRING_ELT(tmp, 0, mkChar(Rcon->description));
    SET_VECTOR_ELT(ans, 0, tmp);
    SET_STRING_ELT(names, 1, mkChar("class"));
    SET_VECTOR_ELT(ans, 1, mkString(Rcon->class));
    SET_STRING_ELT(names, 2, mkChar("mode"));
    SET_VECTOR_ELT(ans, 2, mkString(Rcon->mode));
    SET_STRING_ELT(names, 3, mkChar("text"));
    SET_VECTOR_ELT(ans, 3, mkString(Rcon->text? "text":"binary"));
    SET_STRING_ELT(names, 4, mkChar("opened"));
    SET_VECTOR_ELT(ans, 4, mkString(Rcon->isopen? "opened":"closed"));
    SET_STRING_ELT(names, 5, mkChar("can read"));
    SET_VECTOR_ELT(ans, 5, mkString(Rcon->canread? "yes":"no"));
    SET_STRING_ELT(names, 6, mkChar("can write"));
    SET_VECTOR_ELT(ans, 6, mkString(Rcon->canwrite? "yes":"no"));
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(3);
    return ans;
}


#if defined(USE_WININET_ASYNC) && !defined(USE_WININET)
# define USE_WININET 2
#endif

// in internet module: 'type' is unused
extern Rconnection
R_newCurlUrl(const char *description, const char * const mode, SEXP headers, int type);


/* op = 0: .Internal( url(description, open, blocking, encoding, method, headers))
   op = 1: .Internal(file(description, open, blocking, encoding, method, raw))
*/
attribute_hidden SEXP do_url(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc, headers = R_NilValue;
#ifdef Win32
    SEXP headers_flat = R_NilValue;
#endif
    char *class2 = "url";
    const char *url, *open;
    int ncon, block, defmeth,
	meth = 0, // 0: "internal" | "wininet", 1: "libcurl"
	winmeth = 0;  // 0: "internal", 1: "wininet" (Windows only)
    cetype_t ienc = CE_NATIVE;
    Rconnection con = NULL;
    Rboolean raw = FALSE;

    checkArity(op, args);
    // --------- description
    scmd = CAR(args);
    if(!isString(scmd) || LENGTH(scmd) != 1 ||
       STRING_ELT(scmd, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "description");
    if(LENGTH(scmd) > 1)
	warning(_("only first element of 'description' argument used"));
#ifdef Win32
    winmeth = 1;
    if(PRIMVAL(op) == 1 && !IS_ASCII(STRING_ELT(scmd, 0)) ) { // file(<non-ASCII>, *)
	ienc = CE_UTF8;
	url = trCharUTF8(STRING_ELT(scmd, 0));
    } else {
	ienc = getCharCE(STRING_ELT(scmd, 0));
	if(ienc == CE_UTF8)
	    url = CHAR(STRING_ELT(scmd, 0));
	else
	    url = translateCharFP(STRING_ELT(scmd, 0));
    }
#else
    winmeth = 0;
    url = translateCharFP(STRING_ELT(scmd, 0));
#endif

    // curl-based url() does not need to know the type. so
    // only set for use by the wininet method.
#ifdef Win32
    UrlScheme type = HTTPsh;	/* -Wall */
#endif
    Rboolean inet = TRUE;
    if (strncmp(url, "http://", 7) == 0) {
#ifdef Win32
	type = HTTPsh;
#endif
    } else if (strncmp(url, "ftp://", 6) == 0) {
#ifdef Win32
	type = FTPsh;
#endif
    } else if (strncmp(url, "https://", 8) == 0) {
#ifdef Win32
	type = HTTPSsh;
 #endif
    // ftps:// is available via most libcurl, only
    } else if (strncmp(url, "ftps://", 7) == 0) {
#ifdef Win32
	type = FTPSsh;
#endif
    } else
	inet = FALSE; // file:// URL or a file path

    // --------- open
    sopen = CADR(args);
    if(!isString(sopen) || LENGTH(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    // --------- blocking
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    // --------- encoding
    enc = CADDDR(args);
    if(!isString(enc) || LENGTH(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    // --------- method
    const char *cmeth = CHAR(asChar(CAD4R(args)));
    meth = streql(cmeth, "libcurl"); // 1 if "libcurl", else 0
    defmeth = streql(cmeth, "default");
//#ifndef Win32
    if(defmeth) meth = 1; // default to libcurl
//#endif
    if (streql(cmeth, "wininet")) {
#ifdef Win32
	winmeth = 1;
#else
	error(_("method = \"wininet\" is only supported on Windows"));
#endif
    }
#ifdef Win32
    else if (streql(cmeth, "internal")) winmeth = 0;
#endif

    // --------- raw, for file() only
    if(PRIMVAL(op) == 1) {
	raw = asRbool(CAD5R(args), call);
	if(raw == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "raw");
    }

    // --------- headers, for url() only
    if(PRIMVAL(op) == 0) {
 	SEXP lheaders = CAD5R(args);
	if (!isNull(lheaders)) {
	    headers = VECTOR_ELT(lheaders, 0);
#ifdef Win32
	    headers_flat = VECTOR_ELT(lheaders, 1);
#endif
	}
    }

    if(!meth) {
	if (strncmp(url, "ftps://", 7) == 0) {
#ifdef HAVE_LIBCURL
	    if (defmeth) meth = 1; else
#endif
		error("ftps:// URLs are not supported by this method");
	}
#ifdef Win32
	if (!winmeth && strncmp(url, "https://", 8) == 0) {
# ifdef HAVE_LIBCURL
	    if (defmeth) meth = 1; else
# endif
		error("https:// URLs are not supported by this method");
	}
#else // Unix
	if (strncmp(url, "https://", 8) == 0) {
	    // We check the libcurl build does support https as from R 3.3.0
	    if (defmeth) meth = 1; else
		error("https:// URLs are not supported by the \"internal\" method");
	}
#endif
    }

    ncon = NextConnection();
    if(strncmp(url, "file://", 7) == 0) {
	int nh = 7;
#ifdef Win32
	/* on Windows we have file:///d:/path/to
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif
	con = newfile(url + nh, ienc, strlen(open) ? open : "r", raw);
	class2 = "file";
    } else if (inet) {
	if(meth) {
# ifdef HAVE_LIBCURL
	    con = R_newCurlUrl(url, strlen(open) ? open : "r", headers, 0);
# else
	    error("url(method = \"libcurl\") is not supported on this platform");
# endif
	} else {
	    if(!winmeth)
		error(_("the 'internal' method of url() is defunct for http:// and ftp:// URLs"));
#ifdef Win32
	    // so for "wininet' only
	    con = R_newurl(url, strlen(open) ? open : "r", headers_flat, winmeth);
	    ((Rurlconn)con->private)->type = type;
#endif
	}
    } else {
	if(PRIMVAL(op) == 1) { /* call to file() */
	    if(strlen(url) == 0) {
		if(!strlen(open)) open ="w+";
		if(strcmp(open, "w+") != 0 && strcmp(open, "w+b") != 0) {
		    open ="w+";
		    warning(_("file(\"\") only supports open = \"w+\" and open = \"w+b\": using the former"));
		}
	    }
	    if(strcmp(url, "clipboard") == 0 ||
#ifdef Win32
	       strncmp(url, "clipboard-", 10) == 0
#else
	       strcmp(url, "X11_primary") == 0
	       || strcmp(url, "X11_secondary") == 0
	       || strcmp(url, "X11_clipboard") == 0
#endif
		)
		con = newclp(url, strlen(open) ? open : "r");
	    else {
		const char *efn = R_ExpandFileName(url);
#ifndef Win32
		if (!raw) {
		    struct stat sb;
		    int res = stat(efn, &sb);
		    if (!res && (sb.st_mode & S_IFIFO)) {
			raw = TRUE;
			warning(_("using 'raw = TRUE' because '%s' is a fifo or pipe"),
				url);
		    } else if (!res && !(sb.st_mode & S_IFREG) &&
			       strcmp(efn, "/dev/null"))
			/* not setting 'raw' to FALSE because character devices may be
			   seekable; unfortunately there is no reliable way to detect
			   that without changing the device state */
			warning(_("'raw = FALSE' but '%s' is not a regular file"),
			        url);
		}
#endif
		if (!raw &&
		    (!strlen(open) || streql(open, "r") || streql(open, "rt"))) {
		    /* check if this is a compressed file */
		    int subtype = 0, compress = 0;
		    comp_type ct = comp_type_from_file(efn, FALSE, &subtype);
		    switch(ct) {
		    case COMP_UNKNOWN:
			con = newfile(url, ienc, strlen(open) ? open : "r", raw);
			break;
		    case COMP_GZ:
			con = newgzfile(url, strlen(open) ? open : "rt", compress);
			break;
		    case COMP_BZ:
			con = newbzfile(url, strlen(open) ? open : "rt", compress);
			break;
		    case COMP_XZ:
			con = newxzfile(url, strlen(open) ? open : "rt", subtype,
			                compress);
			break;
		    case COMP_ZSTD:
			con = newzstdfile(url, strlen(open) ? open : "rt", compress);
			break;
		    }
		} else
		    con = newfile(url, ienc, strlen(open) ? open : "r", raw);
	    }
	    class2 = "file";
	} else { // url()
	    error(_("URL scheme unsupported by this method"));
	}
    }

    Connections[ncon] = con;
    con->blocking = (Rboolean) block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */
    con->encname[100 - 1] = '\0';

    /* only text-mode connections are affected, but we can't tell that
       until the connection is opened, and why set an encoding on a
       connection intended to be used in binary mode? */
    if (con->encname[0] && !streql(con->encname, "native.enc"))
	con->canseek = 0;
    /* This is referenced in do_getconnection, so set up before
       any warning */
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"),
					    R_NilValue));

    /* open it if desired */
    if(strlen(open))
	checked_open(ncon);

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(class2));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}

size_t R_WriteConnection(Rconnection con, void *buf, size_t n)
{
    if(!con->isopen) error(_("connection is not open"));
    if(!con->canwrite) error(_("cannot write to this connection"));

    return con->write(buf, 1, n, con);
}

size_t R_ReadConnection(Rconnection con, void *buf, size_t n)
{
    if(!con->isopen) error(_("connection is not open"));
    if(!con->canread) error(_("cannot read from this connection"));

    return con->read(buf, 1, n, con);
}

Rconnection R_GetConnection(SEXP sConn) {
    if (!inherits(sConn, "connection")) error(_("invalid connection"));
    return getConnection(asInteger(sConn));
}

/* ------------------- (de)compression functions  --------------------- */

/* Code for gzcon connections is modelled on gzio.c from zlib 1.2.3 */

#define get_byte() (icon->read(&ccc, 1, 1, icon), ccc)

static Rboolean gzcon_open(Rconnection con)
{
    Rgzconn priv = con->private;
    Rconnection icon = priv->con;

    if(!icon->isopen && !icon->open(icon)) return FALSE;
    con->isopen = TRUE;
    con->canwrite = icon->canwrite;
    con->canread = !con->canwrite;
    con->save = -1000;

    priv->s.zalloc = (alloc_func)0;
    priv->s.zfree = (free_func)0;
    priv->s.opaque = (voidpf)0;
    priv->s.next_in = Z_NULL;
    priv->s.next_out = Z_NULL;
    priv->s.avail_in = priv->s.avail_out = 0;
    priv->z_err = Z_OK;
    priv->z_eof = 0;
    priv->crc = crc32(0L, Z_NULL, 0);

    if(con->canread) {
	/* read header */
	char c, ccc, method, flags, dummy[6];
	unsigned char head[2];
	uInt len;

	icon->read(head, 1, 2, icon);
	if(head[0] != gz_magic[0] || head[1] != gz_magic[1]) {
	    if(!priv->allow) {
		warning(_("file stream does not have gzip magic number"));
		return FALSE;
	    }
	    priv->nsaved = 2;
	    priv->saved[0] = head[0];
	    priv->saved[1] = head[1];
	    return TRUE;
	}
	icon->read(&method, 1, 1, icon);
	icon->read(&flags, 1, 1, icon);
	if (method != Z_DEFLATED || (flags & RESERVED) != 0) {
	    warning(_("file stream does not have valid gzip header"));
	    return FALSE;
	}
	icon->read(dummy, 1, 6, icon);
	if ((flags & EXTRA_FIELD) != 0) { /* skip the extra field */
	    len  =  (uInt) get_byte();
	    len += ((uInt) get_byte()) << 8;
	    /* len is garbage if EOF but the loop below will quit anyway */
	    while (len-- != 0 && get_byte() != EOF) ;
	}
	if ((flags & ORIG_NAME) != 0) { /* skip the original file name */
	    while ((c = get_byte()) != 0 && c != EOF) ;
	}
	if ((flags & COMMENT) != 0) {   /* skip the .gz file comment */
	    while ((c = get_byte()) != 0 && c != EOF) ;
	}
	if ((flags & HEAD_CRC) != 0) {  /* skip the header crc */
	    for (len = 0; len < 2; len++) (void) get_byte();
	}
	priv->s.next_in  = priv->buffer;
	inflateInit2(&(priv->s), -MAX_WBITS);
    } else {
	/* write a header */
	char head[11];
	snprintf(head, 11, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
		Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/,
		OS_CODE);
	icon->write(head, 1, 10, icon);
	deflateInit2(&(priv->s), priv->cp, Z_DEFLATED, -MAX_WBITS,
		     8, Z_DEFAULT_STRATEGY);
	priv->s.next_out = priv->buffer;
	priv->s.avail_out = Z_BUFSIZE;
    }

    return TRUE;
}

static void putLong(Rconnection con, uLong x)
{
    int n;
    unsigned char buf[4];

    for (n = 0; n < 4; n++) {
	buf[n] = (x & 0xff);
	x >>= 8;
    }
    con->write(&buf, 4, 1, con);
}


static void gzcon_close(Rconnection con)
{
    Rgzconn priv = con->private;
    Rconnection icon = priv->con;

    if(icon->canwrite) {
	uInt len;
	int done = 0;
	priv->s.avail_in = 0; /* should be zero already anyway */
	for (;;) {
	    len = Z_BUFSIZE - priv->s.avail_out;

	    if (len != 0) {
		if (icon->write(priv->buffer, 1, len, icon) != len) {
		    priv->z_err = Z_ERRNO;
		    error(_("writing error whilst flushing 'gzcon' connection"));
		}
		priv->s.next_out = priv->buffer;
		priv->s.avail_out = Z_BUFSIZE;
	    }
	    if (done) break;
	    priv->z_err = deflate(&(priv->s), Z_FINISH);

	    /* deflate has finished flushing only when it hasn't used up
	     * all the available space in the output buffer:
	     */
	    done = (priv->s.avail_out != 0 || priv->z_err == Z_STREAM_END);

	    if (priv->z_err != Z_OK && priv->z_err != Z_STREAM_END) break;
	}
	deflateEnd(&(priv->s));
	/* NB: these must be little-endian */
	putLong(icon, priv->crc);
	putLong(icon, (uLong) (priv->s.total_in & 0xffffffff));
    } else inflateEnd(&(priv->s));

    if(icon->isopen) icon->close(icon);
    con->isopen = FALSE;
}

static int gzcon_byte(Rgzconn priv)
{
    Rconnection icon = priv->con;

    if (priv->z_eof) return EOF;
    if (priv->s.avail_in == 0) {
	priv->s.avail_in = (uInt) icon->read(priv->buffer, 1, Z_BUFSIZE, icon);
	if (priv->s.avail_in == 0) {
	    priv->z_eof = 1;
	    return EOF;
	}
	priv->s.next_in = priv->buffer;
    }
    priv->s.avail_in--;
    return *(priv->s.next_in)++;
}


static size_t gzcon_read(void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rgzconn priv = con->private;
    Rconnection icon = priv->con;
    Bytef *start = (Bytef*) ptr;
    uLong crc;
    int n;

    if (priv->z_err == Z_STREAM_END) return 0;  /* EOF */

    /* wrapped connection only needs to handle INT_MAX */
    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));
    if (priv->nsaved >= 0) { /* non-compressed mode */
	size_t len = size*nitems;
	int i, nsaved = priv->nsaved;
	if (len == 0) return 0;
	if (len >= 2) {
	    for(i = 0; i < priv->nsaved; i++)
		((char *)ptr)[i] = priv->saved[i];
	    priv->nsaved = 0;
	    return (nsaved + icon->read((char *) ptr+nsaved, 1, len - nsaved,
					icon))/size;
	}
	if (len == 1) { /* size must be one */
	    if (nsaved > 0) {
		((char *) ptr)[0] = priv->saved[0];
		priv->saved[0] = priv->saved[1];
		priv->nsaved--;
		return 1;
	    } else
		return icon->read(ptr, 1, 1, icon);
	}
    }

    priv->s.next_out = (Bytef*) ptr;
    priv->s.avail_out = (uInt)(size*nitems);

    while (priv->s.avail_out != 0) {
	if (priv->s.avail_in == 0 && !priv->z_eof) {
	    priv->s.avail_in = (uInt)icon->read(priv->buffer, 1, Z_BUFSIZE, icon);
	    if (priv->s.avail_in == 0) priv->z_eof = 1;
	    priv->s.next_in = priv->buffer;
	}
	priv->z_err = inflate(&(priv->s), Z_NO_FLUSH);

	if (priv->z_err == Z_STREAM_END) {
	    /* Check CRC */
	    priv->crc = crc32(priv->crc, start,
			      (uInt)(priv->s.next_out - start));
	    start = priv->s.next_out;
	    crc = 0;
	    for (n = 0; n < 4; n++) {
		crc >>= 8;
		crc += ((uLong) gzcon_byte(priv) << 24);
	    }
	    if (crc != priv->crc) {
		priv->z_err = Z_DATA_ERROR;
		REprintf(_("crc error %lx %lx\n"), crc, priv->crc);
	    }
	    /* finally, get (and ignore) length */
	    for (n = 0; n < 4; n++) gzcon_byte(priv);
	}
	if (priv->z_err != Z_OK || priv->z_eof) break;
    }
    priv->crc = crc32(priv->crc, start, (uInt)(priv->s.next_out - start));
    return (size_t)(size*nitems - priv->s.avail_out)/size;
}

static size_t gzcon_write(const void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rgzconn priv = con->private;
    Rconnection icon = priv->con;

    if ((double) size * (double) nitems > INT_MAX)
	error(_("too large a block specified"));
    priv->s.next_in = (Bytef*) ptr;
    priv->s.avail_in = (uInt)(size*nitems);

    while (priv->s.avail_in != 0) {
	if (priv->s.avail_out == 0) {
	    priv->s.next_out = priv->buffer;
	    if (icon->write(priv->buffer, 1, Z_BUFSIZE, icon) != Z_BUFSIZE) {
		priv->z_err = Z_ERRNO;
		warning(_("write error on 'gzcon' connection"));
		break;
	    }
	    priv->s.avail_out = Z_BUFSIZE;
	}
	priv->z_err = deflate(&(priv->s), Z_NO_FLUSH);
	if (priv->z_err != Z_OK) break;
    }
    priv->crc = crc32(priv->crc, (const Bytef *) ptr, (uInt)(size*nitems));
    return (size_t)(size*nitems - priv->s.avail_in)/size;
}

static int gzcon_fgetc(Rconnection con)
{
    unsigned char c;
    size_t n = gzcon_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}


/* gzcon(con, level, allowNonCompressed) */
attribute_hidden SEXP do_gzcon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, class;
    int icon, level, allow;
    Rconnection incon = NULL, new = NULL;
    char *m, *mode = NULL /* -Wall */,  description[1000];
    int text;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	error(_("'con' is not a connection"));
    incon = getConnection(icon = asInteger(CAR(args)));
    level = asInteger(CADR(args));
    if(level == NA_INTEGER || level < 0 || level > 9)
	error(_("'level' must be one of 0 ... 9"));
    allow = asLogical(CADDR(args));
    if(allow == NA_INTEGER)
	error(_("'allowNonCompression' must be TRUE or FALSE"));
    text = asLogical(CADDDR(args));
    if(text == NA_INTEGER)
        error(_("'text' must be TRUE or FALSE"));

    if(incon->isGzcon) {
	warning(_("this is already a 'gzcon' connection"));
	return CAR(args);
    }
    m = incon->mode;
    if(strcmp(m, "r") == 0 || strncmp(m, "rb", 2) == 0) mode = "rb";
    else if (strcmp(m, "w") == 0 || strncmp(m, "wb", 2) == 0) mode = "wb";
    else error(_("can only use read- or write- binary connections"));
    if(strcmp(incon->class, "file") == 0 &&
       (strcmp(m, "r") == 0 || strcmp(m, "w") == 0))
	warning(_("using a text-mode 'file' connection may not work correctly"));

    else if(strcmp(incon->class, "textConnection") == 0 && strcmp(m, "w") == 0)
	error(_("cannot create a 'gzcon' connection from a writable textConnection; maybe use rawConnection"));

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of 'gzcon' connection failed"));
    new->class = (char *) malloc(strlen("gzcon") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of 'gzcon' connection failed"));
 	/* for Solaris 12.5 */ new = NULL;
   }
    strcpy(new->class, "gzcon");
    Rsnprintf_mbcs(description, 1000, "gzcon(%s)", incon->description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of 'gzcon' connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    new->text = (Rboolean) text;
    new->isGzcon = TRUE;
    new->open = &gzcon_open;
    new->close = &gzcon_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &gzcon_fgetc;
    new->read = &gzcon_read;
    new->write = &gzcon_write;
    new->private = (void *) malloc(sizeof(struct gzconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of 'gzcon' connection failed"));
	/* for Solaris 12.5 */ new = NULL;
    }
    ((Rgzconn)(new->private))->con = incon;
    ((Rgzconn)(new->private))->cp = level;
    ((Rgzconn)(new->private))->nsaved = -1;
    ((Rgzconn)(new->private))->allow = (Rboolean) allow;

    /* as there might not be an R-level reference to the wrapped connection */
    R_PreserveObject(incon->ex_ptr);

    Connections[icon] = new;
    // gcc 8 with sanitizers selected objects to truncation here with 99 or 100.
    strncpy(new->encname, incon->encname, 100);
    new->encname[100 - 1] = '\0';
    new->ex_ptr = PROTECT(R_MakeExternalPtr((void *)new->id, install("connection"),
					    R_NilValue));
    if(incon->isopen) new->open(new);

    PROTECT(ans = ScalarInteger(icon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("gzcon"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, new->ex_ptr);
    /* Disable, as e.g. load() leaves no reference to the new connection */
    //R_RegisterCFinalizerEx(new->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    return ans;
}


/* code for in-memory (de)compression of data stored in a raw vector. 
   Uses a 4-byte header of length, in XDR order. */

#ifndef WORDS_BIGENDIAN
static unsigned int uiSwap (unsigned int x)
{
  return((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
}
#else
#define uiSwap(x) (x)
#endif

/* These are all hidden and used only in serialize.c,
   so managing R_alloc stack is prudence. */

#if defined(HAVE_LIBDEFLATE) && defined(USE_LIBDEFLATE)
# include <libdeflate.h>

attribute_hidden
SEXP R_compress1(SEXP in)
{
    if(TYPEOF(in) != RAWSXP)
	error("R_compress1 requires a raw vector");

    static struct libdeflate_compressor *c = NULL;
    if(c == NULL) {
       c = libdeflate_alloc_compressor(6);
       if(c == NULL)
           error("allocation error in R_compress1 with libdeflate");
    }

    const void *vmax = vmaxget();

    unsigned int inlen = LENGTH(in);
    size_t outlen = libdeflate_zlib_compress_bound(c, inlen);
    Bytef *buf = (Bytef *) R_alloc(outlen + 4, sizeof(Bytef));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    size_t res =
	libdeflate_zlib_compress(c, RAW(in), inlen, buf + 4, outlen);
    if(res == 0)
	error("internal libdeflate error in R_compress1 with libdeflate");
    SEXP ans = allocVector(RAWSXP, res + 4);
    memcpy(RAW(ans), buf, res + 4);
    vmaxset(vmax);
    return ans;
}

attribute_hidden
SEXP R_decompress1(SEXP in, Rboolean *err)
{
    if(TYPEOF(in) != RAWSXP)
	error("R_decompress1 requires a raw vector");

    const void *vmax = vmaxget();

    static struct libdeflate_decompressor *d = NULL;
    if(d == NULL) {
	d = libdeflate_alloc_decompressor();
	if(d == NULL)
	    error("allocation error in R_decompress1 with libdeflate");
    }

    unsigned char *p = RAW(in);
    size_t inlen = LENGTH(in);
    size_t outlen = uiSwap(*((unsigned int *) p));
    Bytef *buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    size_t actual_out;
    enum libdeflate_result res =
	libdeflate_zlib_decompress(d, p + 4, inlen - 4, buf,
				   outlen, &actual_out);

    if(res != LIBDEFLATE_SUCCESS) {
	warning("internal error %d in R_decompress1 with libdeflate", res);
	*err = TRUE;
	return R_NilValue;
    }
    SEXP ans = allocVector(RAWSXP, actual_out);
    if (actual_out)
	memcpy(RAW(ans), buf, actual_out);
    vmaxset(vmax);
    return ans;
}

#else

attribute_hidden
SEXP R_compress1(SEXP in)
{
    if(TYPEOF(in) != RAWSXP)
	error("R_compress1 requires a raw vector");

    const void *vmax = vmaxget();

    unsigned int inlen = LENGTH(in);
    uLong outlen = (uLong)(1.001*inlen + 20);
    Bytef *buf = (Bytef *) R_alloc(outlen + 4, sizeof(Bytef));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    int res = compress(buf + 4, &outlen, (Bytef *)RAW(in), inlen);
    if(res != Z_OK) error("internal error %d in R_compress1", res);
    SEXP ans = allocVector(RAWSXP, outlen + 4);
    memcpy(RAW(ans), buf, outlen + 4);
    vmaxset(vmax);
    return ans;
}

attribute_hidden
SEXP R_decompress1(SEXP in, Rboolean *err)
{
    if(TYPEOF(in) != RAWSXP)
	error("R_decompress1 requires a raw vector");

    const void *vmax = vmaxget();

    unsigned char *p = RAW(in);
    uLong inlen = LENGTH(in);
    uLong outlen = (uLong) uiSwap(*((unsigned int *) p));
    Bytef *buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    int res = uncompress(buf, &outlen, (Bytef *)(p + 4), inlen - 4);
    if(res != Z_OK) {
	warning("internal error %d in R_decompress1", res);
	*err = TRUE;
	return R_NilValue;
    }
    SEXP ans = allocVector(RAWSXP, outlen);
    if (outlen)
	memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}
#endif

attribute_hidden
SEXP R_compress2(SEXP in)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    int res;
    char *buf;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error("R_compress2 requires a raw vector");
    inlen = LENGTH(in);
    outlen = inlen + inlen/100 + 600; // 1.01 * ..  staying int
    buf = R_alloc(outlen + 5, sizeof(char));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    buf[4] = '2';
    res = BZ2_bzBuffToBuffCompress(buf + 5, &outlen,
				   (char *)RAW(in), inlen,
				   9, 0, 0);
    if(res != BZ_OK) error("internal error %d in R_compress2", res);
    /* printf("compressed %d to %d\n", inlen, outlen); */
    if (res != BZ_OK || outlen > inlen) {
	outlen = inlen;
	buf[4] = '0';
	if (inlen)
	    memcpy(buf+5, (char *)RAW(in), inlen);
    }
    ans = allocVector(RAWSXP, outlen + 5);
    memcpy(RAW(ans), buf, outlen + 5);
    vmaxset(vmax);
    return ans;
}

attribute_hidden
SEXP R_decompress2(SEXP in, Rboolean *err)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    int res;
    char *buf, *p = (char *) RAW(in), type;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error("R_decompress2 requires a raw vector");
    inlen = LENGTH(in);
    outlen = uiSwap(*((unsigned int *) p));
    buf = R_alloc(outlen, sizeof(char));
    type = p[4];
    if (type == '2') {
	res = BZ2_bzBuffToBuffDecompress(buf, &outlen, p + 5, inlen - 5, 0, 0);
	if(res != BZ_OK) {
	    warning("internal error %d in R_decompress2", res);
	    *err = TRUE;
	    return R_NilValue;
	}
    } else if (type == '1') {
	uLong outl;
	res = uncompress((unsigned char *) buf, &outl,
			 (Bytef *)(p + 5), inlen - 5);
	if(res != Z_OK) {
	    warning("internal error %d in R_decompress1", res);
	    *err = TRUE;
	    return R_NilValue;
	}
    } else if (type == '0') {
	buf = p + 5;
    } else {
	warning("unknown type in R_decompress2");
	*err = TRUE;
	return R_NilValue;
    }
    ans = allocVector(RAWSXP, outlen);
    if (outlen)
	memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}


attribute_hidden SEXP do_sockselect(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean immediate = FALSE;
    int nsock, i;
    SEXP insock, write, val, insockfd;
    double timeout;
    int fdlim;

#ifdef Win32
    fdlim = 1024; /* keep in step with sock.h */
#else
    fdlim = FD_SETSIZE;
#endif

    checkArity(op, args);

    insock = CAR(args);
    if (TYPEOF(insock) != VECSXP || LENGTH(insock) == 0)
	error(_("not a list of sockets"));
    nsock = LENGTH(insock);

    write = CADR(args);
    if (TYPEOF(write) != LGLSXP || LENGTH(write) != nsock)
	error(_("bad write indicators"));

    timeout = asReal(CADDR(args));

    PROTECT(insockfd = allocVector(INTSXP, nsock));
    PROTECT(val = allocVector(LGLSXP, nsock));

    for (i = 0; i < nsock; i++) {
	Rconnection conn = getConnection(asInteger(VECTOR_ELT(insock, i)));
	if (!strcmp(conn->class, "sockconn")) {
	    Rsockconn scp = conn->private;
	    INTEGER(insockfd)[i] = scp->fd;
	    if (! LOGICAL(write)[i] && scp->pstart < scp->pend) {
		LOGICAL(val)[i] = TRUE;
		immediate = TRUE;
	    }
	    else LOGICAL(val)[i] = FALSE;
	} else if (!strcmp(conn->class, "servsockconn")) {
	    Rservsockconn sop = conn->private;
	    INTEGER(insockfd)[i] = sop->fd;
	    LOGICAL(val)[i] = FALSE;
	    if (LOGICAL(write)[i])
		warning(_("a server socket connection cannot be writeable"));
	} else
	    error(_("not a socket connection"));
#ifdef Unix
	if (INTEGER(insockfd)[i] >= fdlim && !immediate)
	    error(_("file descriptor is too large for select()"));
#endif
    }

#ifdef Win32
    if (nsock > fdlim && !immediate)
	error(_("too many file descriptors for select()"));
#endif
    if (! immediate)
	Rsockselect(nsock, INTEGER(insockfd), LOGICAL(val), LOGICAL(write),
		    timeout);

    UNPROTECT(2);
    return val;
}

attribute_hidden SEXP do_serversocket(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, class;
    int ncon, port;
    Rconnection con = NULL;

    checkArity(op, args);

    port = asInteger(CAR(args));
    if(port == NA_INTEGER || port < 0)
	error(_("invalid '%s' argument"), "port");

    ncon = NextConnection();
    con = R_newservsock(port);
    Connections[ncon] = con;
    con->ex_ptr = PROTECT(R_MakeExternalPtr(con->id, install("connection"), R_NilValue));

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("servsockconn"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, con->ex_ptr);
    R_RegisterCFinalizerEx(con->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);
    return ans;
}

/* socketTimeout(socket, timeout) */
attribute_hidden SEXP do_socktimeout(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int tnew, told;
    Rsockconn scon;

    checkArity(op, args);

    if(!inherits(CAR(args), "sockconn"))
	error(_("invalid '%s' argument"), "socket");

    scon = getConnection(asInteger(CAR(args)))->private;
    told = scon->timeout;

    tnew = asInteger(CADR(args));
    if(tnew == NA_INTEGER)
	error(_("invalid '%s' argument"), "timeout");

    if (tnew >= 0)
	scon->timeout = tnew;

    return ScalarInteger(told);
}

static lzma_filter filters[LZMA_FILTERS_MAX + 1];

static void init_filters(void)
{
    static uint32_t preset_number = 6; /* 9 | LZMA_PRESET_EXTREME; */
    static lzma_options_lzma opt_lzma;
    static Rboolean set = FALSE;
    if(set) return;
    if(lzma_lzma_preset(&opt_lzma, preset_number))
	error("problem setting presets");
    filters[0].id = LZMA_FILTER_LZMA2;
    filters[0].options = &opt_lzma;
    filters[1].id = LZMA_VLI_UNKNOWN;
    set = TRUE;
    /*
      printf("encoding memory usage %lu\n", lzma_raw_encoder_memusage(filters));
      printf("decoding memory usage %lu\n", lzma_raw_decoder_memusage(filters));
    */
}

attribute_hidden
SEXP R_compress3(SEXP in)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    unsigned char *buf;
    SEXP ans;
    lzma_stream strm = LZMA_STREAM_INIT;
    lzma_ret ret;

    if(TYPEOF(in) != RAWSXP)
	error("R_compress3 requires a raw vector");
    inlen = LENGTH(in);
    outlen = inlen + 5;  /* don't allow it to expand */
    buf = (unsigned char *) R_alloc(outlen + 5, sizeof(unsigned char));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    buf[4] = 'Z';

    init_filters();
    ret = lzma_raw_encoder(&strm, filters);
    if (ret != LZMA_OK) error("internal error %d in R_compress3", ret);
    strm.next_in = RAW(in);
    strm.avail_in = inlen;
    strm.next_out = buf + 5;
    strm.avail_out = outlen;
    while(!ret) ret = lzma_code(&strm, LZMA_FINISH);
    if (ret != LZMA_STREAM_END || (strm.avail_in > 0)) {
	warning("internal error %d in R_compress3", ret);
	outlen = inlen;
	buf[4] = '0';
	memcpy(buf+5, (char *)RAW(in), inlen);
    } else outlen = (unsigned int) strm.total_out;
    lzma_end(&strm);

    /* printf("compressed %d to %d\n", inlen, outlen); */
    ans = allocVector(RAWSXP, outlen + 5);
    memcpy(RAW(ans), buf, outlen + 5);
    vmaxset(vmax);
    return ans;
}

attribute_hidden
SEXP R_decompress3(SEXP in, Rboolean *err)
{
    const void *vmax = vmaxget();
    unsigned int inlen, outlen;
    unsigned char *buf, *p = RAW(in), type = p[4];
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error("R_decompress3 requires a raw vector");
    inlen = LENGTH(in);
    outlen = (unsigned int) uiSwap(*((unsigned int *) p));
    buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));

    if (type == 'Z') {
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	init_filters();
	ret = lzma_raw_decoder(&strm, filters);
	if (ret != LZMA_OK) {
	    warning("internal error %d in R_decompress3", (int)ret);
	    *err = TRUE;
	    return R_NilValue;
	}
	strm.next_in = p + 5;
	strm.avail_in = inlen - 5;
	strm.next_out = buf;
	strm.avail_out = outlen;
	ret = lzma_code(&strm, LZMA_RUN);
	if (ret != LZMA_OK && (strm.avail_in > 0)) {
	    warning("internal error %d in R_decompress3 %llu",
		    (int)ret, (unsigned long long)strm.avail_in);
	    *err = TRUE;
	    return R_NilValue;
	}
	lzma_end(&strm);
#if 0 /* not enabled - just ready if we ever want to allow zstd */
    } else if (type == 'S') {
#ifdef HAVE_ZSTD
	unsigned long long sz = ZSTD_getFrameContentSize(p + 5, inlen - 5);
	if (sz == ZSTD_CONTENTSIZE_UNKNOWN) /* possible streaming so no size in the header */
	    sz = ZSTD_decompressBound(p + 5, inlen - 5);
	if (sz == ZSTD_CONTENTSIZE_ERROR ||
	    ZSTD_isError((outlen = ZSTD_decompress(buf, outlen, p + 5, inlen - 5)))) {
	    warning("internal error in zstd R_decompress3");
	    *err = TRUE;
	    return R_NilValue;
	}
#else
	error("Zstd compression support was not included in this R binary.");
#endif
#endif
    } else if (type == '2') {
	int res;
	res = BZ2_bzBuffToBuffDecompress((char *)buf, &outlen,
					 (char *)(p + 5), inlen - 5, 0, 0);
	if(res != BZ_OK) {
	    warning("internal error %d in R_decompress2", res);
	    *err = TRUE;
	    return R_NilValue;
	}
    } else if (type == '1') {
	uLong outl; int res;
	res = uncompress(buf, &outl, (Bytef *)(p + 5), inlen - 5);
	if(res != Z_OK) {
	    warning("internal error %d in R_decompress1", res);
	    *err = TRUE;
	    return R_NilValue;
	}
    } else if (type == '0') {
	buf = p + 5;
    } else {
	warning("unknown type in R_decompress3");
	*err = TRUE;
	return R_NilValue;
    }
    ans = allocVector(RAWSXP, outlen);
    if (outlen)
	memcpy(RAW(ans), buf, outlen);
    vmaxset(vmax);
    return ans;
}

#ifdef HAVE_LIBDEFLATE
# include <libdeflate.h>
#endif
attribute_hidden SEXP
do_memCompress(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, from;
    int type, res;

    checkArity(op, args);
    ans = from = CAR(args);
    if(TYPEOF(from) != RAWSXP) error("'from' must be raw or character");
    type = asInteger(CADR(args));
    switch(type) {
    case 1: break; /* none */
    case 2: /*gzip */
#ifdef HAVE_LIBDEFLATE
    {
	static struct libdeflate_compressor *c = NULL;
	if(c == NULL) {
	    c = libdeflate_alloc_compressor(6);
	    if(c == NULL)
		error("allocation error in memCompress with libdeflate");
	}
	size_t inlen = XLENGTH(from);
	size_t outlen = libdeflate_zlib_compress_bound(c, inlen);
	Bytef *buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
        size_t res =
	    libdeflate_zlib_compress(c, RAW(from), inlen, buf, outlen);
	if(res == 0)
	    error("internal libdeflate error in memCompress");
	ans = allocVector(RAWSXP, res);
	memcpy(RAW(ans), buf, res);
	break;
    }
#else
    {
	Bytef *buf;
	/* could use outlen = compressBound(inlen) */
	uLong inlen = XLENGTH(from),
	    outlen = (uLong)(1.001*(double)inlen + 20);
	buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
	res = compress(buf, &outlen, (Bytef *)RAW(from), inlen);
	if(res != Z_OK) error("internal error %d in memCompress", res);
	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
#endif
    case 3: /* bzip */
    {
	// bzlib does not support long inputs
	char *buf;
	unsigned int inlen = LENGTH(from),
	    outlen = (unsigned int)(1.01*inlen + 600);
	buf = R_alloc(outlen, sizeof(char));
	res = BZ2_bzBuffToBuffCompress(buf, &outlen, (char *)RAW(from),
				       inlen, 9, 0, 0);
	if(res != BZ_OK) error("internal error %d in memCompress", res);
	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 4: /* xz */
    {
	unsigned char *buf;
	size_t inlen = XLENGTH(from), outlen;
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	lzma_filter filters[LZMA_FILTERS_MAX + 1];
	uint32_t preset_number = 9 | LZMA_PRESET_EXTREME;
	lzma_options_lzma opt_lzma;

	if(lzma_lzma_preset(&opt_lzma, preset_number))
	    error("problem setting presets");
	filters[0].id = LZMA_FILTER_LZMA2;
	filters[0].options = &opt_lzma;
	filters[1].id = LZMA_VLI_UNKNOWN;

	ret = lzma_stream_encoder(&strm, filters, LZMA_CHECK_CRC32);
	if (ret != LZMA_OK) error("internal error %d in memCompress", ret);

	outlen = inlen + inlen/100 + 600; /* FIXME, copied from bzip2 case */
	buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));
	strm.next_in = RAW(from);
	strm.avail_in = inlen;
	strm.next_out = buf;
	strm.avail_out = outlen;
	while(!ret) ret = lzma_code(&strm, LZMA_FINISH);
	if (ret != LZMA_STREAM_END || (strm.avail_in > 0))
	    error("internal error %d in memCompress", ret);
	/* If LZMZ_BUF_ERROR, could realloc and continue */
	outlen = (unsigned int)strm.total_out;
	lzma_end(&strm);
	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 5: /* zstd */
#ifdef HAVE_ZSTD
    {
	size_t inlen = XLENGTH(from);
	size_t outlen = ZSTD_compressBound(inlen);
	Bytef *buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
        size_t res = /* FIXME: what should be the compression level? 3 is undocumented "default" in zstd if 0 is used */
	    ZSTD_compress(buf, outlen, RAW(from), inlen, 3);
	if (ZSTD_isError(res))
	    error("internal libzstd error (%s) in memCompress", ZSTD_getErrorName(res));
	ans = allocVector(RAWSXP, res);
	memcpy(RAW(ans), buf, res);
	break;
    }
#else
    error("Zstd compression support was not included in this R binary.");
#endif
    default:
	break;
    }

    return ans;
}

#ifdef HAVE_LIBDEFLATE
// from libdeflate/programs/gzip.c
typedef uint8_t u8;
typedef uint32_t u32;
static inline u32
get_unaligned_le32(const u8 *p)
{
    return ((u32)p[3] << 24) | ((u32)p[2] << 16) |
	((u32)p[1] << 8) | p[0];
}
#endif


attribute_hidden SEXP
do_memDecompress(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, from;
    int type, subtype = 0;

    checkArity(op, args);
    ans = from = CAR(args);
    if(TYPEOF(from) != RAWSXP) error("'from' must be raw or character");
    type = asInteger(CADR(args));
    if (type == 5) {/* type = 5 is "unknown" */
	char *p = (char *) RAW(from);
	comp_type ct;
	ct = comp_type_from_memory(p, LENGTH(from), TRUE, &subtype);
	switch(ct) {
	case COMP_GZ: type = 2; break;
	case COMP_BZ: type = 3; break;
	case COMP_XZ: type = 4; break;
	case COMP_ZSTD: type = 6; break;
	case COMP_UNKNOWN:
	    warning(_("unknown compression, assuming none"));
	    type = 1;
	    break;
	}
    }

    switch(type) {
    case 1: break; /* none */
    case 2: /* gzip */
#ifdef HAVE_LIBDEFLATE
    {
	static struct libdeflate_decompressor *d = NULL;
	if(d == NULL) {
	    d = libdeflate_alloc_decompressor();
	    if(d == NULL)
		error("allocation error in memDecompress with libdeflate");
	}

	size_t inlen = XLENGTH(from), outlen = 3*inlen, actual_out;
        enum libdeflate_result res;
	Bytef *buf, *p = (Bytef *)RAW(from);

	if (p[0] == 0x1f && p[1] == 0x8b) { // in-memory gzip file
	    while(1) {
		outlen = get_unaligned_le32(&p[inlen - 4]);
		if (outlen == 0) outlen = 1;

		buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
		res = libdeflate_gzip_decompress(d, RAW(from), inlen,
						 buf, outlen, &actual_out);
		if(res == LIBDEFLATE_INSUFFICIENT_SPACE) {
		    // should not happen but recorded length might be wrong.
		    if(outlen < ULONG_MAX/2) {
			outlen *= 2; continue;
		    } else break;
		}
		if(res == LIBDEFLATE_SUCCESS) break;
		error("internal error %d in memDecompress(%s)", res,
		      "type = \"libdeflate\"");
	    }
	} else {
	    while(1) {
		buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
		res = libdeflate_zlib_decompress(d, RAW(from), inlen,
						 buf, outlen, &actual_out);
		if(res == LIBDEFLATE_INSUFFICIENT_SPACE) {
		    if(outlen < ULONG_MAX/2) {
			outlen *= 2; continue;
		    } else break;
		}
		if(res == LIBDEFLATE_SUCCESS) break;
		error("internal error %d in memDecompress(%s)", res,
		      "type = \"libdeflate\"");
	    }
	}
	ans = allocVector(RAWSXP, actual_out);
	if (actual_out)
	    memcpy(RAW(ans), buf, actual_out);
	break;
    }
#else
    {
	uLong inlen = XLENGTH(from), outlen = 3*inlen;
	Bytef *buf, *p = (Bytef *)RAW(from);
	int opt = 0;

	if (p[0] == 0x1f && p[1] == 0x8b) { // in-memory gzip file
	    // in this case we could read outlen from the trailer
	    opt = 16;  // force gzip format
	}
	while(1) {
	    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
	    // R_uncompress is in gzio.h
	    int res = R_uncompress(buf, &outlen, p, inlen, opt);
	    if(res == Z_BUF_ERROR) {
		if(outlen < ULONG_MAX/2) {
		    outlen *= 2; continue;
		} else break;
	    }
	    if(res >= 0) break;
	    error("internal error %d in memDecompress(%s)", res,
		  "type = \"gzip\"");
	}

	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
#endif
    case 3: /* bzip2 */
    {
	// bzlib does not support long inputs
	unsigned int inlen = LENGTH(from), outlen;
	double o0 = 3.0*inlen;
	outlen = (o0 > UINT_MAX)? UINT_MAX: (unsigned int)o0;
	int res;
	char *buf, *p = (char *) RAW(from);
	while(1) {
	    buf = R_alloc(outlen, sizeof(char));
	    res = BZ2_bzBuffToBuffDecompress(buf, &outlen, p, inlen, 0, 0);
	    if(res == BZ_OUTBUFF_FULL) {
		if(outlen < UINT_MAX/2) {
		    outlen *= 2; continue;
		} else break;
	    }
	    if(res == BZ_OK) break;
	    error("internal error %d in memDecompress(%s)", res,
		  "type = \"bzip2\"");
	}
	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 4: /* xz */
    {
	unsigned char *buf;
	size_t inlen = XLENGTH(from);
	size_t outlen = 3*inlen;
	lzma_stream strm = LZMA_STREAM_INIT;
	lzma_ret ret;
	while(1) {
	    /* Initialize lzma_stream in each iteration. */
	    /* probably at most 80Mb is required, but 512Mb seems OK as a limit */
	    if (subtype == 1)
		ret = lzma_alone_decoder(&strm, 536870912);
	    else
		ret = lzma_stream_decoder(&strm, 536870912, LZMA_CONCATENATED);
	    if (ret != LZMA_OK)
		error(_("cannot initialize lzma decoder, error %d"), ret);

	    buf = (unsigned char *) R_alloc(outlen, sizeof(unsigned char));
	    strm.avail_in = inlen;
	    strm.avail_out = outlen;
	    strm.next_in = (unsigned char *) RAW(from);
	    strm.next_out = buf;

	    ret = lzma_code(&strm, LZMA_FINISH);
	    /* Did lzma_code() leave some input? */
	    if (strm.avail_in > 0) {
		/* Decompression failed, free lzma_stream. */
		lzma_end(&strm);
		/* Because it ran out of output buffer?
		 *
		 * This used to only check if LZMA_BUF_ERROR was
		 * returned, but apparently XZ will also signal an out
		 * of buffer condition by returning LZMA_OK and
		 * leaving avail_in > 0 (i.e. not all input was
		 * consumed).
		 */
		if (ret == LZMA_BUF_ERROR || ret == LZMA_OK) {
		    outlen *= 2;
		    continue;
		} else {
		    error("internal error %d in memDecompress(%s) at %llu",
			  (int)ret, "type = \"xz\"",
		          (unsigned long long)strm.avail_in);
		}
	    } else {
		break;
	    }
	}
	outlen = strm.total_out;
	lzma_end(&strm);
	ans = allocVector(RAWSXP, outlen);
	if (outlen)
	    memcpy(RAW(ans), buf, outlen);
	break;
    }
    case 6: /* zstd */
#ifdef HAVE_ZSTD
    {
	size_t inlen = XLENGTH(from), res;
	unsigned long long outlen;
	Bytef *buf, *p = (Bytef *)RAW(from);

	outlen = ZSTD_getFrameContentSize(p, inlen);
	if (outlen == ZSTD_CONTENTSIZE_UNKNOWN)
	    outlen = ZSTD_decompressBound(p, inlen);
	if (outlen == ZSTD_CONTENTSIZE_ERROR)
	    error("internal error in memDecompress(%s)", "type = \"zstd\"");
	buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
	res = ZSTD_decompress(buf, outlen, p, inlen);
	if (ZSTD_isError(res))
	    error("internal error in memDecompress(%s)", ZSTD_getErrorName(res));
	ans = allocVector(RAWSXP, res);
	if (res)
	    memcpy(RAW(ans), buf, res);
	break;
    }
#else
    error("Zstd compression support was not included in this R binary.");
#endif
    // case 5 is "unknown', covered above
    default:
	break;
    }
    return ans;
}

/* --- C-level entry to create a custom connection object -- */
/* The returned value is the R-side instance. To avoid additional call to getConnection()
   the internal Rconnection pointer will be placed in ptr[0] if ptr is not NULL.
   It is the responsibility of the caller to customize callbacks in the structure,
   they are initialized to dummy_ (where available) and null_ (all others) callbacks.
   Also note that the resulting object has a finalizer, so any clean up (including after
   errors) is done by garbage collection - the caller may not free anything in the
   structure explicitly (that includes the con->private pointer!).
 */
SEXP R_new_custom_connection(const char *description, const char *mode, const char *class_name, Rconnection *ptr)
{
    Rconnection new;
    SEXP ans, class;

    int ncon = NextConnection();

    /* built-in connections do this in a separate new<class>() function */
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of %s connection failed"), class_name);
    new->class = (char *) malloc(strlen(class_name) + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of %s connection failed"), class_name);
	/* for Solaris 12.5 */ new = NULL;
    }
    strcpy(new->class, class_name);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of %s connection failed"), class_name);
	/* for Solaris 12.5 */ new = NULL;
    }
    init_con(new, description, CE_NATIVE, mode);
    /* all ptrs are init'ed to null_* so no need to repeat that,
       but the following two are useful tools which could not be accessed otherwise */
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &dummy_fgetc;

    /* here we use the new connection to create a SEXP */
    Connections[ncon] = new;
    /* new->blocking = block; */
    new->encname[0] = 0; /* "" (should have the same effect as "native.enc") */
    new->ex_ptr = PROTECT(R_MakeExternalPtr(new->id, install("connection"), R_NilValue));

    PROTECT(ans = ScalarInteger(ncon));
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(class_name));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    setAttrib(ans, R_ConnIdSymbol, new->ex_ptr);
    R_RegisterCFinalizerEx(new->ex_ptr, conFinalizer, FALSE);
    UNPROTECT(3);

    if (ptr) ptr[0] = new;

    return ans;
}
