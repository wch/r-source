/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-7   The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if defined(HAVE_GLIBC2)
/* for fileno */
# define _POSIX_SOURCE 1
#endif

#include <Defn.h>
#include <Fileio.h>
#include <zlib.h>		/* needs to be before Rconnections.h */
#include <Rconnections.h>
#include <R_ext/Complex.h>
#include <R_ext/R-ftp-http.h>
#include <R_ext/RS.h>		/* R_chk_calloc and Free */
#include <R_ext/Riconv.h>
#undef ERROR			/* for compilation on Windows */

int attribute_hidden R_OutputCon; /* used in printutils.c */

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
/* Solaris and AIX define open as open64 under some circumstances */
# undef open
#endif

/* This should work on Win64, as long is 4 bytes but long long is 8 bytes. */
#if defined __GNUC__ && __GNUC__ >= 2
__extension__ typedef long long int _lli_t;
#else
typedef long long int _lli_t;
#endif

/* Win32 does have popen, but it does not work in GUI applications,
   so test that later */
#ifdef Win32
# include <R_ext/RStartup.h>
  extern UImode  CharacterMode;
#endif

#define NCONNECTIONS 128 /* snow needs one per slave node */
#define NSINKS 21

static Rconnection Connections[NCONNECTIONS];
static SEXP OutTextData;

static int R_SinkNumber;
static int SinkCons[NSINKS], SinkConsClose[NSINKS], R_SinkSplit[NSINKS];

/* ------------- admin functions (see also at end) ----------------- */

int attribute_hidden NextConnection()
{
    int i;
    for(i = 3; i < NCONNECTIONS; i++)
	if(!Connections[i]) break;
    if(i >= NCONNECTIONS)
	error(_("all connections are in use"));
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
attribute_hidden
Rconnection getConnection(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	error(_("invalid connection"));
    return con;

}

attribute_hidden
int getActiveSink(int n){
  if (n>=R_SinkNumber || n<0)
    return 0;
  if (R_SinkSplit[R_SinkNumber-n])
    return SinkCons[R_SinkNumber-n-1];
  else
    return 0;
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

void set_iconv(Rconnection con)
{
    void *tmp;
    /* need to test if this is text, open for reading to writing or both,
       and set inconv and/or outconv */
    if(!con->text || !strlen(con->encname) ||
       strcmp(con->encname, "native.enc") == 0) return;
    if(con->canread) {
	size_t onb = 50;
	char *ob = con->oconvbuff;
	tmp = Riconv_open("", con->encname);
	if(tmp != (void *)-1) con->inconv = tmp;
	else error(_("conversion from encoding '%s' is unsupported"), 
		   con->encname);
	con->EOF_signalled = FALSE;
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, NULL, NULL, &ob, &onb);
	con->navail = 50-onb; con->inavail = 0;
	/* libiconv can handle BOM marks on Windows Unicode files, but
	   glibc's iconv cannot. Aargh ... */
	if(streql(con->encname, "UCS-2LE")) con->inavail = -2;
    }
    if(con->canwrite) {
	size_t onb = 25;
	char *ob = con->init_out;
	tmp = Riconv_open(con->encname, "");
	if(tmp != (void *)-1) con->outconv = tmp;
	else error(_("conversion to encoding '%s' is unsupported"),
		   con->encname);
	/* initialize state, and prepare any initial bytes */
	Riconv(tmp, NULL, NULL, &ob, &onb);
	ob[25-onb] = '\0';
    }
}


/* ------------------- null connection functions --------------------- */

static Rboolean null_open(Rconnection con)
{
    error(_("open/close not enabled for this connection"));
    return FALSE;		/* -Wall */
}

static void null_close(Rconnection con)
{
    con->isopen = FALSE;
}

static void null_destroy(Rconnection con)
{
    if(con->private) free(con->private);
}

static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error(_("printing not enabled for this connection"));
    return 0;			/* -Wall */
}

/* va_copy is C99, but a draft standard had __va_copy.  Glibc has
   __va_copy declared uncondiitonally */


#if defined(HAVE_VASPRINTF) && !HAVE_DECL_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap);
#endif

#if !HAVE_VA_COPY && HAVE___VA_COPY
# define va_copy __va_copy
# undef HAVE_VA_COPY
# define HAVE_VA_COPY 1
#endif

#ifdef HAVE_VA_COPY
# define BUFSIZE 10000
#else
# define BUFSIZE 100000
#endif
int dummy_vfprintf(Rconnection con, const char *format, va_list ap)
{
    char buf[BUFSIZE], *b = buf;
    int res;
#ifdef HAVE_VA_COPY
    char *vmax = vmaxget();
    int usedRalloc = FALSE, usedVasprintf = FALSE;
    va_list aq;

    va_copy(aq, ap);
    res = vsnprintf(buf, BUFSIZE, format, aq);
    va_end(aq);
#ifdef HAVE_VASPRINTF
    if(res >= BUFSIZE || res < 0) {
	vasprintf(&b, format, ap);
	usedVasprintf = TRUE;
    }
#else
    if(res >= BUFSIZE) { /* res is the desired output length */
	usedRalloc = TRUE;
	/* apparently some implementations count short,
	   <http://unixpapa.com/incnote/stdio.html>
	   so add some margin here */
	b = R_alloc(res + 101, sizeof(char));
	vsnprintf(b, res+100, format, ap);
    } else if(res < 0) { /* just a failure indication */
	usedRalloc = TRUE;
	b = R_alloc(10*BUFSIZE, sizeof(char));
	res = vsnprintf(b, 10*BUFSIZE, format, ap);
	if (res < 0) {
	    b[10*BUFSIZE - 1] = '\0';
	    warning(_("printing of extremely long output is truncated"));
	    res = 10*BUFSIZE;
	}
    }
#endif /* HAVE_VASPRINTF */
#else  /* no VA_COPY */
    res = vsnprintf(buf, BUFSIZE, format, ap);
    if(res >= BUFSIZE || res < 0) { 
	/* res is the desired output length or just a failure indication */
	    buf[BUFSIZE - 1] = '\0';
	    warning(_("printing of extremely long output is truncated"));
	    res = BUFSIZE;
    }
#endif
#ifdef HAVE_ICONV
    if(con->outconv) { /* translate the buffer */
	char outbuf[BUFSIZE+1], *ib = b, *ob;
	size_t inb = res, onb, ires;
	Rboolean again = FALSE;
	int ninit = strlen(con->init_out);
	do {
	    onb = BUFSIZE; /* leave space for nul */
	    ob = outbuf;
	    if(ninit) {
		strcpy(ob, con->init_out);
		ob += ninit; onb -= ninit; ninit = 0;
	    }
	    ires = Riconv(con->outconv, &ib, &inb, &ob, &onb);
	    if(ires == (size_t)(-1) && errno == E2BIG) again = TRUE;
	    if(ires == (size_t)(-1) && errno != E2BIG)
		/* is this safe? */
		warning(_("invalid char string in output conversion"));
	    *ob = '\0';
	    con->write(outbuf, 1, strlen(outbuf), con);
	} while(again);
    } else
#endif /* HAVE_VA_COPY */
	con->write(b, 1, res, con);
#ifdef HAVE_VA_COPY
    if(usedRalloc) vmaxset(vmax);
    if(usedVasprintf) free(b);
#endif
    return res;
}

int dummy_fgetc(Rconnection con)
{
    int c;
    Rboolean checkBOM = FALSE;

    if(con->inconv) {
	if(con->navail <= 0) {
	    unsigned int i, inew = 0;
	    char *p, *ib, *ob;
	    size_t inb, onb, res;

	    if(con->EOF_signalled) return R_EOF;
	    if(con->inavail == -2) {
		con->inavail = 0;
		checkBOM = TRUE;
	    }
	    p = con->iconvbuff + con->inavail;
	    for(i = con->inavail; i < 25; i++) {
		c = con->fgetc_internal(con);
		if(c == R_EOF){ con->EOF_signalled = TRUE; break; }
		*p++ = c;
		con->inavail++;
		inew++;
	    }
	    if(inew == 0) return R_EOF;
	    if(checkBOM && con->inavail >= 2 &&
	       ((int)con->iconvbuff[0] & 0xff) == 255 &&
	       ((int)con->iconvbuff[1] & 0xff) == 254) {
		con->inavail -= 2;
		memmove(con->iconvbuff, con->iconvbuff+2, con->inavail);
	    }
	    ib = con->iconvbuff; inb = con->inavail;
	    ob = con->oconvbuff; onb = 50;
	    res = Riconv(con->inconv, &ib, &inb, &ob, &onb);
	    con->inavail = inb;
	    if(res == (size_t)-1) { /* an error condition */
		if(errno == EINVAL || errno == E2BIG) {
		    /* incomplete input char or no space in output buffer */
		    memmove(con->iconvbuff, ib, inb);
  		} else {/*  EILSEQ invalid input */
		    warning(_("invalid input found on input connection '%s'"),
			    con->description);
		    con->inavail = 0;
		    con->EOF_signalled = TRUE;
		}
	    }
	    con->next = con->oconvbuff;
	    con->navail = 50 - onb;
	}
	con->navail--;
	return *con->next++;
    } else
	return con->fgetc_internal(con);
}

static int null_fgetc(Rconnection con)
{
    error(_("getc not enabled for this connection"));
    return 0;			/* -Wall */
}

static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("seek not enabled for this connection"));
    return 0.;			/* -Wall */
}

static void null_truncate(Rconnection con)
{
    error(_("truncation not enabled for this connection"));
}

static int null_fflush(Rconnection con)
{
    return 0;
}

static size_t null_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    error(_("read not enabled for this connection"));
    return 0;			/* -Wall */
}

static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error(_("write not enabled for this connection"));
    return 0;			/* -Wall */
}

void init_con(Rconnection new, char *description, const char * const mode)
{
    strcpy(new->description, description);
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
}

/* ------------------- file connections --------------------- */

#if defined(HAVE_OFF_T) && defined(HAVE_SEEKO)
#define f_seek fseeko
#define f_tell ftello
#else
#ifdef Win32
#define f_seek fseeko64
#define f_tell ftello64
#else
#define f_seek fseek
#define f_tell ftell
#endif
#endif

static Rboolean file_open(Rconnection con)
{
    char *name;
    FILE *fp = NULL;
    Rfileconn this = con->private;
    Rboolean temp = FALSE;
#ifdef HAVE_FCNTL
    int fd, flags;
#endif
    int mlen = strlen(con->mode);

    if(strlen(con->description) == 0) {
	temp = TRUE;
	name = R_tmpnam("Rf", R_TempDir);
    } else name = R_ExpandFileName(con->description);
    errno = 0; /* some systems require this */
    if(strcmp(name, "stdin")) {
	fp = R_fopen(name, con->mode);
    } else {  /* use file("stdin") to refer to the file and not the console */
#ifdef HAVE_FDOPEN
	fp = fdopen(0, con-> mode);
#else
        warning(_("cannot open file '%s', reason '%s'"), name,
		"fdopen is not supported on this platform");
#endif
    }
    if(!fp) {
#ifdef HAVE_STRERROR
	warning(_("cannot open file '%s', reason '%s'"), name, strerror(errno));
#else
        warning(_("cannot open file '%s'"), name);
#endif
	return FALSE;
    }
    if(temp) {
	unlink(name);
#ifdef Win32
	strncpy(this->name, name, PATH_MAX);
#endif
	free(name);
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
    if(strcmp(con->description, "stdin")) fclose(this->fp);
    con->isopen = FALSE;
#ifdef Win32
    if(this->anon_file) unlink(this->name);
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
    c =fgetc(fp);
    return feof(fp) ? R_EOF : c;
}

static double file_seek(Rconnection con, double where, int origin, int rw)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
#if defined(HAVE_OFF_T) && defined(HAVE_SEEKO)
    off_t pos;
#else
#ifdef Win32
    off64_t pos;
#else
    long pos;
#endif
#endif
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
    if(ISNA(where)) return pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: whence = SEEK_END; 
#ifdef Win32
	    /* work around a bug in MinGW runtime 3.8 fseeko64, PR#7896 */
	    if(con->canwrite) fflush(fp);
#endif
	    break;
    default: whence = SEEK_SET;
    }
    f_seek(fp, where, whence);
    if(this->last_was_write) this->wpos = f_tell(this->fp);
    else this->rpos = f_tell(this->fp);
    return pos;
}

static void file_truncate(Rconnection con)
{
    Rfileconn this = con->private;
#ifdef HAVE_FTRUNCATE
    FILE *fp = this->fp;
    int fd = fileno(fp);
#ifdef HAVE_OFF_T
    off_t size = lseek(fd, 0, SEEK_CUR);
#else
#ifdef Win32
    __int64 size = lseek64(fd, 0, SEEK_CUR);
#else
    int size = lseek(fd, 0, SEEK_CUR);
#endif
#endif
#endif

    if(!con->isopen || !con->canwrite)
	error(_("can only truncate connections open for writing"));

    if(!this->last_was_write) this->rpos = f_tell(this->fp);
#ifdef HAVE_FTRUNCATE
    if(ftruncate(fd, size))
	error(_("file truncation failed"));
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
    return fread(ptr, size, nitems, fp);
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

static Rconnection newfile(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of file connection failed"));
    new->class = (char *) malloc(strlen("file") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of file connection failed"));
    }
    strcpy(new->class, "file");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of file connection failed"));
    }
    init_con(new, description, mode);
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
    new->canseek = TRUE;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of file connection failed"));
    }
    return new;
}

/* file() is now implemented as an op of do_url */

/* ------------------- fifo connections --------------------- */

#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)

#ifdef HAVE_STAT
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
# endif
# ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
# endif
#endif /* HAVE_STAT */

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

static Rboolean fifo_open(Rconnection con)
{
    char *name;
    Rfifoconn this = con->private;
    int fd, flags, res;
    int mlen = strlen(con->mode);
    struct stat sb;

    name = R_ExpandFileName(con->description);
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
#ifdef HAVE_STRERROR
                warning(_("cannot create fifo '%s', reason '%s'"), name, 
                        strerror(errno));
#else
                warning(_("cannot create fifo '%s'"), name);
#endif
		return FALSE;
	    }
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
    fd = open(name, flags);
    if(fd < 0) {
	if(errno == ENXIO) warning(_("fifo '%s' is not ready"), name);
	else warning(_("cannot open fifo '%s'"), name);
	return FALSE;
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
    close(((Rfifoconn)(con->private))->fd);
    con->isopen = FALSE;
}

static int fifo_fgetc_internal(Rconnection con)
{
    Rfifoconn this = (Rfifoconn)con->private;
    unsigned char c;
    int n;

    n = read(this->fd, (char *)&c, 1);
    return (n == 1) ? c : R_EOF;
}

static size_t fifo_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    Rfifoconn this = (Rfifoconn)con->private;

    return read(this->fd, ptr, size * nitems)/size;
}

static size_t fifo_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfifoconn this = (Rfifoconn)con->private;

    return write(this->fd, ptr, size * nitems)/size;
}


static Rconnection newfifo(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of fifo connection failed"));
    new->class = (char *) malloc(strlen("fifo") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of fifo connection failed"));
    }
    strcpy(new->class, "fifo");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of fifo connection failed"));
    }
    init_con(new, description, mode);
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
    }
    return new;
}
#endif

SEXP attribute_hidden do_fifo(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int ncon, block;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateChar(STRING_ELT(sfile, 0)); /* for now, like fopen */
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "block");
    enc = CADDDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = NextConnection();
    con = Connections[ncon] = newfifo(file, strlen(open) ? open : "r");
    con->blocking = block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("fifo"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
#else
    error(_("fifo connections are not available on this system"));
    return R_NilValue;		/* -Wall */
#endif
}

/* ------------------- pipe connections --------------------- */

#ifdef HAVE_POPEN
static Rboolean pipe_open(Rconnection con)
{
    FILE *fp;
    char mode[3];

#ifdef Win32
    strncpy(mode, con->mode, 2);
    mode[2] = '\0';
#else
    mode[0] = con->mode[0];
    mode[1] = '\0';
#endif
    errno = 0;
    fp = R_popen(con->description, mode);
    if(!fp) {
#ifdef HAVE_STRERROR
        warning(_("cannot open pipe() cmd '%s', reason '%s'"), con->description,
                        strerror(errno));
#else
	warning(_("cannot open pipe() cmd '%s'"), con->description);
#endif
	return FALSE;
    }
    ((Rfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void pipe_close(Rconnection con)
{
    pclose(((Rfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static Rconnection newpipe(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of pipe connection failed"));
    new->class = (char *) malloc(strlen("pipe") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of pipe connection failed"));
    }
    strcpy(new->class, "pipe");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
    }
    init_con(new, description, mode);
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
    }
    return new;
}
#endif

#ifdef Win32
extern Rconnection newWpipe(char *description, char *mode);
#endif

SEXP attribute_hidden do_pipe(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_POPEN
    SEXP scmd, sopen, ans, class, enc;
    char *file, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "description");
    if(length(scmd) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateChar(STRING_ELT(scmd, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    enc = CADDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    ncon = NextConnection();
#ifdef Win32
    if(CharacterMode != RTerm)
	con = newWpipe(file, strlen(open) ? open : "r");
    else
	con = newpipe(file, strlen(open) ? open : "r");
#else
    con = newpipe(file, strlen(open) ? open : "r");
#endif
    Connections[ncon] = con;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("pipe"));
#ifdef Win32
    if(CharacterMode != RTerm)
	SET_STRING_ELT(class, 0, mkChar("pipeWin32"));
#endif
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
#else
    error(_("pipe connections are not available on this system"));
    return R_NilValue;		/* -Wall */
#endif
}

/* ------------------- gzipped file connections --------------------- */

static Rboolean gzfile_open(Rconnection con)
{
    gzFile fp;
    char mode[6];
    
    strcpy(mode, con->mode);
    if(!strchr(mode, 'b')) strcat(mode, "b");

    fp = gzopen(R_ExpandFileName(con->description), mode);
    if(!fp) {
	warning(_("cannot open compressed file '%s'"),
	      R_ExpandFileName(con->description));
	return FALSE;
    }
    ((Rgzfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void gzfile_close(Rconnection con)
{
    gzclose(((Rgzfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static int gzfile_fgetc_internal(Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    int c;

    /* Looks like eof is signalled one char early */
    /* -- sometimes! gzgetc may still return EOF */
    if(gzeof(fp)) return R_EOF;
    c = gzgetc(fp);
    return (c == EOF) ? R_EOF : c;
}

static double gzfile_seek(Rconnection con, double where, int origin, int rw)
{
    gzFile  fp = ((Rgzfileconn)(con->private))->fp;
    z_off_t pos = gztell(fp);
    int res, whence = SEEK_SET;

    switch(origin) {
    case 2: whence = SEEK_CUR;
    case 3: error(_("whence = \"end\" is not implemented for gzfile connections"));
    default: whence = SEEK_SET;
    }
    if(where >= 0) {
	res = gzseek(fp, (z_off_t) where, whence);
	if(res == -1)
	    warning(_("seek on a gzfile connection returned an internal error"));
    }
    return (double) pos;
}

static int gzfile_fflush(Rconnection con)
{
    /* Degrades compression too much, as Rvprintf calls fflush.

       gzFile fp = ((Rgzfileconn)(con->private))->fp;

       return gzflush(fp, Z_SYNC_FLUSH); */

    return 0;
}

static size_t gzfile_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    return gzread(fp, ptr, size*nitems)/size;
}

static size_t gzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;
    return gzwrite(fp, (voidp)ptr, size*nitems)/size;
}

static Rconnection newgzfile(char *description, char *mode, int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of gzfile connection failed"));
    new->class = (char *) malloc(strlen("gzfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of gzfile connection failed"));
    }
    strcpy(new->class, "gzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of gzfile connection failed"));
    }
    init_con(new, description, "");
    strncpy(new->mode, mode, 1);
    sprintf(new->mode+1, "b%1d", compress);

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
    }
    return new;
}

SEXP attribute_hidden do_gzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int ncon, compress;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateChar(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    compress = asInteger(CADDDR(args));
    if(compress == NA_LOGICAL || compress < 0 || compress > 9)
	error(_("invalid '%s' argument"), "compress");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = NextConnection();
    con = Connections[ncon] = newgzfile(file, strlen(open) ? open : "r",
					compress);
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("gzfile"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}

/* ------------------- bzipped file connections --------------------- */

#include <bzlib.h>

static Rboolean bzfile_open(Rconnection con)
{
    FILE* fp;
    BZFILE* bfp;
    int bzerror;
    char mode[] = "rb";

    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    /* regardless of the R view of the file, the file must be opened in
       binary mode where it matters */
    mode[0] = con->mode[0];
    fp = R_fopen(R_ExpandFileName(con->description), mode);
    if(!fp) {
	warning(_("cannot open bzip2-ed file '%s'"),
		R_ExpandFileName(con->description));
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
	bfp = BZ2_bzWriteOpen(&bzerror, fp, 9, 0, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzWriteClose(&bzerror, bfp, 0, NULL, NULL);
	    fclose(fp);
	    warning(_("file '%s' appears not to be compressed by bzip2"),
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    }
    ((Rbzfileconn)(con->private))->fp = fp;
    ((Rbzfileconn)(con->private))->bfp = bfp;
    con->isopen = TRUE;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    set_iconv(con);
    con->save = -1000;
    return TRUE;
}

static void bzfile_close(Rconnection con)
{
    int bzerror;
    BZFILE* bfp = (BZFILE *)((Rbzfileconn)(con->private))->bfp;
    FILE* fp = (FILE *)((Rbzfileconn)(con->private))->fp;

    if(con->canread)
	BZ2_bzReadClose(&bzerror, bfp);
    else
	BZ2_bzWriteClose(&bzerror, bfp, 0, NULL, NULL);
    fclose(fp);
    con->isopen = FALSE;
}

static int bzfile_fgetc_internal(Rconnection con)
{
    BZFILE* bfp = (BZFILE *)((Rbzfileconn)(con->private))->bfp;
    char buf[1];
    int bzerror, size;

    size = BZ2_bzRead(&bzerror, bfp, buf, 1);
    return (size < 1) ? R_EOF : (buf[0] % 256);
}

static size_t bzfile_read(void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    BZFILE* bfp = (BZFILE *)((Rbzfileconn)(con->private))->bfp;
    int bzerror;

    return BZ2_bzRead(&bzerror, bfp, ptr, size*nitems)/size;
}

static size_t bzfile_write(const void *ptr, size_t size, size_t nitems,
			   Rconnection con)
{
    BZFILE* bfp = (BZFILE *)((Rbzfileconn)(con->private))->bfp;
    int bzerror;

    BZ2_bzWrite(&bzerror, bfp, (voidp)ptr, size*nitems);
    if(bzerror != BZ_OK) return 0;
    else return nitems;
}

static Rconnection newbzfile(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of bzfile connection failed"));
    new->class = (char *) malloc(strlen("bzfile") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of bzfile connection failed"));
    }
    strcpy(new->class, "bzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of bzfile connection failed"));
    }
    init_con(new, description, mode);

    new->canseek = FALSE;
    new->open = &bzfile_open;
    new->close = &bzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc_internal = &bzfile_fgetc_internal;
    new->fgetc = &dummy_fgetc;
    new->fgetc = &dummy_fgetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &bzfile_read;
    new->write = &bzfile_write;
    new->private = (void *) malloc(sizeof(struct bzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of bzfile connection failed"));
    }
    return new;
}

SEXP attribute_hidden do_bzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateChar(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = NextConnection();
    con = Connections[ncon] = newbzfile(file, strlen(open) ? open : "r");
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("bzfile"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}


/* ------------------- clipboard connections --------------------- */

#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
extern int GA_clipboardhastext(); /* from ga.h */
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
    if(con->canread) {
	/* copy the clipboard contents now */
#ifdef Win32
	HGLOBAL hglb;
	char *pc;
	if(GA_clipboardhastext() &&
	   OpenClipboard(NULL) &&
	   (hglb = GetClipboardData(CF_TEXT)) &&
	   (pc = (char *)GlobalLock(hglb))) {
	    int len = strlen(pc);
	    this->buff = (char *)malloc(len + 1);
	    this->last = this->len = len;
	    if(this->buff) {
		strcpy(this->buff, pc);
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
    set_iconv(con);
    con->save = -1000;
    this->warned = FALSE;

    return TRUE;
}

static void clp_writeout(Rconnection con)
{
#ifdef Win32
    Rclpconn this = con->private;

    HGLOBAL hglb;
    char *s, *p;
    if ( (hglb = GlobalAlloc(GHND, this->len)) &&
	 (s = (char *)GlobalLock(hglb)) ) {
	p = this->buff;
	while(p < this->buff + this->pos) *s++ = *p++;
	*s = '\0';
	GlobalUnlock(hglb);
	if (!OpenClipboard(NULL) || !EmptyClipboard()) {
	    warning(_("Unable to open the clipboard"));
	    GlobalFree(hglb);
	} else {
	    if(!SetClipboardData(CF_TEXT, hglb)) {
		warning(_("Unable to write to the clipboard"));
		GlobalFree(hglb);
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
    if(this-> buff) free(this->buff);
}

static int clp_fgetc_internal(Rconnection con)
{
    Rclpconn this = con->private;

    if (this->pos >= this->len) return R_EOF;
    return this->buff[this->pos++];
}

static double clp_seek(Rconnection con, double where, int origin, int rw)
{
    Rclpconn this = con->private;
    int newpos, oldpos = this->pos;

    if(ISNA(where)) return oldpos;

    switch(origin) {
    case 2: newpos = this->pos + (int)where; break;
    case 3: newpos = this->last + (int)where; break;
    default: newpos = where;
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
    int available = this->len - this->pos, request = size*nitems, used;
    used = (request < available) ? request : available;
    strncpy(ptr, this->buff, used);
    return (size_t) used/size;
}

static size_t clp_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rclpconn this = con->private;
    int i, len = size * nitems, used = 0;
    char c, *p = (char *)ptr, *q = this->buff + this->pos;

    if(!con->canwrite)
	error(_("clipboard connection is open for reading only"));

    for(i = 0; i < len; i++) {
	if(this->pos >= this->len) break;
	c = *p++;
#ifdef Win32
    /* clipboard requires CRLF termination */
	if(c == '\n') {
	    *q++ = '\r';
	    this->pos++;
	    if(this->pos >= this->len) break;
	}
#endif
	*q++ = c;
	this->pos++;
	used++;
    }
    if (used < len && !this->warned) {
	warning(_("clipboard buffer is full and output lost"));
	this->warned = TRUE;
    }
    if(this->last < this->pos) this->last = this->pos;
    return (size_t) used/size;
}

static Rconnection newclp(char *url, char *mode)
{
    Rconnection new;
    char *description;
    int sizeKB = 32;

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
    }
    strcpy(new->class, description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of clipboard connection failed"));
    }
    init_con(new, description, mode);
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
    }
    ((Rclpconn)new->private)->buff = NULL;
    if (strncmp(url, "clipboard-", 10) == 0) {
	sizeKB = atoi(url+10);
	if(sizeKB < 32) sizeKB = 32;
	/* Rprintf("setting clipboard size to %dKB\n", sizeKB); */
    }
    ((Rclpconn)new->private)->sizeKB = sizeKB;
    return new;
}

/* ------------------- terminal connections --------------------- */

static unsigned char  ConsoleBuf[CONSOLE_BUFFER_SIZE+1];
static unsigned char *ConsoleBufp;
static int  ConsoleBufCnt;

static int ConsoleGetchar()
{
    if (--ConsoleBufCnt < 0) {
	ConsoleBuf[CONSOLE_BUFFER_SIZE] = '\0';
	if (R_ReadConsole("", ConsoleBuf, CONSOLE_BUFFER_SIZE, 0) == 0) {
	    R_ClearerrConsole();
	    return R_EOF;
	}
	ConsoleBufp = ConsoleBuf;
	ConsoleBufCnt = strlen((char *)ConsoleBuf);
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
    if(R_Outputfile) vfprintf(R_Outputfile, format, ap);
    else Rcons_vprintf(format, ap);
    return 0;
}

static int stdout_fflush(Rconnection con)
{
    if(R_Outputfile) return fflush(R_Outputfile);
    return 0;
}

static int stderr_vfprintf(Rconnection con, const char *format, va_list ap)
{
    REvprintf(format, ap);
    return 0;
}

static int stderr_fflush(Rconnection con)
{
    if(R_Consolefile) return fflush(R_Consolefile);
    return 0;
}

static Rconnection newterminal(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of terminal connection failed"));
    new->class = (char *) malloc(strlen("terminal") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of terminal connection failed"));
    }
    strcpy(new->class, "terminal");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of terminal connection failed"));
    }
    init_con(new, description, mode);
    new->isopen = TRUE;
    new->canread = (strcmp(mode, "r") == 0);
    new->canwrite = (strcmp(mode, "w") == 0);
    new->destroy = &null_close;
    new->private = NULL;
    return new;
}


SEXP attribute_hidden do_stdin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(0);

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = 0;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_stdout(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(R_OutputCon);

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = R_OutputCon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}


SEXP attribute_hidden do_stderr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, class;
    Rconnection con = getConnection(2);

    checkArity(op, args);
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = 2;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(con->class));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}

/* ------------------- text connections --------------------- */

/* read a R character vector into a buffer */
static void text_init(Rconnection con, SEXP text)
{
    int i, nlines = length(text), nchars = 0;
    Rtextconn this = (Rtextconn)con->private;

    for(i = 0; i < nlines; i++)
	nchars += strlen(translateChar(STRING_ELT(text, i))) + 1;
    this->data = (char *) malloc(nchars+1);
    if(!this->data) {
	free(this); free(con->description); free(con->class); free(con);
	error(_("cannot allocate memory for text connection"));
    }
    *(this->data) = '\0';
    for(i = 0; i < nlines; i++) {
	strcat(this->data, translateChar(STRING_ELT(text, i)));
	strcat(this->data, "\n");
    }
    this->nchars = nchars;
    this->cur = this->save = 0;
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
    Rtextconn this = (Rtextconn)con->private;

    free(this->data);
    /* this->cur = this->nchars = 0; */
    free(this);
}

static int text_fgetc(Rconnection con)
{
    Rtextconn this = (Rtextconn)con->private;
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

static Rconnection newtext(char *description, SEXP text)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of text connection failed"));
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of text connection failed"));
    }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of text connection failed"));
    }
    init_con(new, description, "r");
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
    }
    text_init(new, text);
    return new;
}

static void outtext_close(Rconnection con)
{
    Routtextconn this = (Routtextconn)con->private;
    int idx = ConnIndex(con);
    SEXP tmp, env = VECTOR_ELT(OutTextData, idx);

    if(this->namesymbol &&
       findVarInFrame3(env, this->namesymbol, FALSE) != R_UnboundValue)
	R_unLockBinding(this->namesymbol, env);
    if(strlen(this->lastline) > 0) {
	PROTECT(tmp = lengthgets(this->data, ++this->len));
	SET_STRING_ELT(tmp, this->len - 1, mkChar(this->lastline));
	if(this->namesymbol) defineVar(this->namesymbol, tmp, env);
	SET_NAMED(tmp, 2);
	this->data = tmp;
	UNPROTECT(1);
    }
}

static void outtext_destroy(Rconnection con)
{
    Routtextconn this = (Routtextconn)con->private;
    int idx = ConnIndex(con);
    /* OutTextData is preserved, and that implies that the environment 
       we are writing it and hence the character vector is protected.
       However, this could be quite expensive.
    */
    SET_VECTOR_ELT(OutTextData, idx, R_NilValue);
    free(this->lastline); free(this);
}

#define LAST_LINE_LEN 256

static int text_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Routtextconn this = (Routtextconn)con->private;
    char buf[BUFSIZE], *b = buf, *p, *q, *vmax = vmaxget();
    int res = 0, usedRalloc = FALSE, buffree,
	already = strlen(this->lastline);
    SEXP tmp;

    if(already >= BUFSIZE) {
	/* This will fail so just call vsnprintf to get the length of
	   the new piece */
	res = vsnprintf(buf, 0, format, ap);
	if(res > 0) res += already;
	buffree = 0;
    } else {
	strcpy(b, this->lastline);
	p = b + already;
	buffree = BUFSIZE - already;
	res = vsnprintf(p, buffree, format, ap);
    }
    if(res >= buffree) { /* res is the desired output length */
	usedRalloc = TRUE;
	b = R_alloc(res + already + 1, sizeof(char));
	strcpy(b, this->lastline);
	p = b + already;
	vsprintf(p, format, ap);
    } else if(res < 0) { /* just a failure indication -- e.g. Windows */
#define NBUFSIZE (already + 100*BUFSIZE)
	usedRalloc = TRUE;
	b = R_alloc(NBUFSIZE, sizeof(char));
	strncpy(b, this->lastline, NBUFSIZE);
	*(b + NBUFSIZE - 1) = '\0';
	p = b + already;
	res = vsnprintf(p, NBUFSIZE - already, format, ap);
	if (res < 0) {
	    *(b + NBUFSIZE - 1) = '\0';
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
	    PROTECT(tmp = lengthgets(this->data, ++this->len));
	    SET_STRING_ELT(tmp, this->len - 1, mkChar(p));
	    if(this->namesymbol) {
		if(findVarInFrame3(env, this->namesymbol, FALSE) 
		   != R_UnboundValue) R_unLockBinding(this->namesymbol, env);
		defineVar(this->namesymbol, tmp, env);
		R_LockBinding(this->namesymbol, env);
	    }
	    this->data = tmp;
	    SET_NAMED(tmp, 2);
	    UNPROTECT(1);
	} else {
	    /* retain the last line */
	    if(strlen(p) >= this->lastlinelength) {
		int newlen = strlen(p) + 1;
		this->lastline = realloc(this->lastline, newlen);
		this->lastlinelength = newlen;
	    }
	    strcpy(this->lastline, p);
	    con->incomplete = strlen(this->lastline) > 0;
	    break;
	}
    }
    if(usedRalloc) vmaxset(vmax);
    return res;
}

static void outtext_init(Rconnection con, SEXP stext, char *mode, int idx)
{
    Routtextconn this = (Routtextconn)con->private;
    SEXP val;

    if(stext == R_NilValue) {
	this->namesymbol = NULL;
	    /* create variable pointed to by con->description */
	val = allocVector(STRSXP, 0);
    } else {
	this->namesymbol = install(con->description);
	if(strcmp(mode, "w") == 0) {
	    /* create variable pointed to by con->description */
	    PROTECT(val = allocVector(STRSXP, 0));
	    defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
	    /* Not clear if this is needed, but be conservative */
	    SET_NAMED(val, 2);
	    UNPROTECT(1);
	} else {
	    /* take over existing variable */
	    val = findVar1(this->namesymbol, VECTOR_ELT(OutTextData, idx),
			   STRSXP, FALSE);
	    if(val == R_UnboundValue) {
		warning(_("text connection: appending to a non-existent char vector"));
		PROTECT(val = allocVector(STRSXP, 0));
		defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
		SET_NAMED(val, 2);
		UNPROTECT(1);
	    }
	    R_LockBinding(this->namesymbol, VECTOR_ELT(OutTextData, idx));
	}
    }
    this->len = LENGTH(val);
    this->data = val;
    this->lastline[0] = '\0';
    this->lastlinelength = LAST_LINE_LEN;
}


static Rconnection newouttext(char *description, SEXP stext, char *mode,
			      int idx)
{
    Rconnection new;
    void *tmp;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of text connection failed"));
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of text connection failed"));
    }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of text connection failed"));
    }
    init_con(new, description, mode);
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
    }
    ((Routtextconn)new->private)->lastline = tmp = malloc(LAST_LINE_LEN);
    if(!tmp) {
	free(new->private);
	free(new->description); free(new->class); free(new);
	error(_("allocation of text connection failed"));
    }
    outtext_init(new, stext, mode, idx);
    return new;
}

SEXP attribute_hidden do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, stext, sopen, ans, class, venv;
    char *desc, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) != 1)
	error(_("invalid '%s' argument"), "description");
    desc = translateChar(STRING_ELT(sfile, 0));
    stext = CADR(args);
    sopen = CADDR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    venv = CADDDR(args);
    if (isNull(venv))
	error(_("use of NULL environment is defunct"));
    if (!isEnvironment(venv))
	error(_("invalid '%s' argument"), "environment");
    ncon = NextConnection();
    if(!strlen(open) || strncmp(open, "r", 1) == 0) {
	if(!isString(stext))
	    error(_("invalid '%s' argument"), "text");
	con = Connections[ncon] = newtext(desc, stext);
    } else if (strncmp(open, "w", 1) == 0 || strncmp(open, "a", 1) == 0) {
	if (OutTextData == NULL) {
	    OutTextData = allocVector(VECSXP, NCONNECTIONS);
	    R_PreserveObject(OutTextData);
	}
	SET_VECTOR_ELT(OutTextData, ncon, venv);
	if(stext == R_NilValue)
	    con = Connections[ncon] = newouttext("NULL", stext, open, ncon);
	else if(isString(stext) && length(stext) == 1)
	    con = Connections[ncon] =
		newouttext(translateChar(STRING_ELT(stext, 0)), stext, 
			   open, ncon);
	else
	    error(_("invalid '%s' argument"), "text");
    }
    else
	errorcall(call, _("unsupported mode"));
    /* already opened */

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("textConnection"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_textconvalue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con=NULL;
    Routtextconn this;
    
    checkArity(op, args);
    if(!inherits(CAR(args), "textConnection"))
	errorcall(call, _("'con' is not a textConnection"));
    con = getConnection(asInteger(CAR(args)));
    if(!con->canwrite)
	error(_("'con' is not an output textConnection"));
    this = (Routtextconn)con->private;
    return this->data;
}



/* ------------------- socket connections  --------------------- */


/* socketConnection(host, port, server, blocking, open, encoding) */
SEXP attribute_hidden do_sockconn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    char *host, *open;
    int ncon, port, server, blocking;
    Rconnection con = NULL;

    checkArity(op, args);
#ifdef HAVE_SOCKETS
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) != 1)
	error(_("invalid '%s' argument"), "host");
    host = translateChar(STRING_ELT(scmd, 0));
    args = CDR(args);
    port = asInteger(CAR(args));
    if(port == NA_INTEGER || port < 0)
	error(_("invalid '%s' argument"), "port");
    args = CDR(args);
    server = asLogical(CAR(args));
    if(server == NA_LOGICAL)
	error(_("invalid '%s' argument"), "server");
    args = CDR(args);
    blocking = asLogical(CAR(args));
    if(blocking == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    args = CDR(args);
    sopen = CAR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    args = CDR(args);
    enc = CAR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    ncon = NextConnection();
    con = R_newsock(host, port, server, open);
    Connections[ncon] = con;
    con->blocking = blocking;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("sockconn"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);
#else
    error(_("sockets are not available on this system"));
#endif
    return ans;
}

/* ------------------- unz connections  --------------------- */

/* see dounzip.c for the details */
SEXP attribute_hidden do_unz(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, _("invalid '%s' argument"), "description");
    if(length(sfile) > 1)
	warning(_("only first element of 'description' argument used"));
    file = translateChar(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    enc = CADDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    ncon = NextConnection();
    con = Connections[ncon] = R_newunz(file, strlen(open) ? open : "r");
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("unz"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}

/* -------------- open, close, seek, truncate, flush ------------------ */

SEXP attribute_hidden do_open(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, block;
    Rconnection con=NULL;
    SEXP sopen;
    char *open;
    Rboolean success;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    i = asInteger(CAR(args));
    con = getConnection(i);
    if(i < 3) error(_("cannot open standard connections"));
    if(con->isopen) {
	warning(_("connection is already open"));
	return R_NilValue;
    }
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "blocking");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    if(strlen(open) > 0) strcpy(con->mode, open);
    con->blocking = block;
    success = con->open(con);
    if(!success) {
	/* con_close(i); user might have a reference */
	error(_("unable to open connection"));
    }
    return R_NilValue;
}

SEXP attribute_hidden do_isopen(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;
    int rw, res;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    rw = asInteger(CADR(args));
    res = con->isopen != FALSE;
    switch(rw) {
    case 0: break;
    case 1: res = res & con->canread; break;
    case 2: res = res & con->canwrite; break;
    default: errorcall(call, _("unknown 'rw' value"));
    }
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = res;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = con->incomplete != FALSE;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_isseekable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = con->canseek != FALSE;
    UNPROTECT(1);
    return ans;
}

static void con_close1(Rconnection con)
{
    if(con->isopen) con->close(con);
    if(con->isGzcon) {
	Rgzconn priv = (Rgzconn)con->private;
	con_close1(priv->con);
    }
    /* close inconv and outconv if open */
    if(con->inconv) Riconv_close(con->inconv);
    if(con->outconv) Riconv_close(con->outconv);
    con->destroy(con);
    free(con->class);
    free(con->description);
    /* clear the pushBack */
    if(con->nPushBack > 0) {
	int j;

	for(j = 0; j < con->nPushBack; j++)
	    free(con->PushBack[j]);
	free(con->PushBack);
    }
}


void con_close(int i)
{
    Rconnection con=NULL;

    con = getConnection(i);
    con_close1(con);
    free(Connections[i]);
    Connections[i] = NULL;
}


SEXP attribute_hidden do_close(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    i = asInteger(CAR(args));
    if(i < 3) error(_("cannot close standard connections"));
    for(j = 0; j < R_SinkNumber; j++)
	if(i == SinkCons[j])
	    error(_("cannot close output sink connection"));
    if(i == R_ErrorCon)
	error(_("cannot close messages sink connection"));
    con_close(i);
    return R_NilValue;
}

/* seek(con, where = numeric(), origin = "start", rw = "") */
SEXP attribute_hidden do_seek(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int origin, rw;
    SEXP ans;
    Rconnection con = NULL;
    double where;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    if(!con->isopen) error(_("connection is not open"));
    where = asReal(CADR(args));
    origin = asInteger(CADDR(args));
    rw = asInteger(CADDDR(args));
    PROTECT(ans = allocVector(REALSXP, 1));
    REAL(ans)[0] = con->seek(con, where, origin, rw);
    UNPROTECT(1);
    return ans;
}

/* truncate(con) */
SEXP attribute_hidden do_truncate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    con->truncate(con);
    return R_NilValue;
}

SEXP attribute_hidden do_flush(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
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

int Rconn_ungetc(int c, Rconnection con)
{
    con->save2 = c;
    return c;
}

/* read one line (without trailing newline) from con and store it in buf */
/* return number of characters read, -1 on EOF */
int Rconn_getline(Rconnection con, char *buf, int bufsize)
{
    int c, nbuf = -1;

    while((c = Rconn_fgetc(con)) != R_EOF) {
	if(nbuf+1 >= bufsize) error(_("Line longer than buffer size"));
	if(c != '\n'){
	    buf[++nbuf] = c;
	} else {
	    buf[++nbuf] = '\0';
	    break;
	}
    }
    /* Make sure it is null-terminated and count is correct, even if
     *  file did not end with newline.
     */
    if(nbuf >= 0 && buf[nbuf]) {
	if(nbuf+1 >= bufsize) error(_("Line longer than buffer size"));
	buf[++nbuf] = '\0';
    }
    return(nbuf);
}


int Rconn_printf(Rconnection con, const char *format, ...)
{
    int res;
    va_list(ap);

    va_start(ap, format);
    /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
    res = (con->vfprintf)(con, format, ap);
    va_end(ap);
    return res;
}


/* readLines(con = stdin(), n = 1, ok = TRUE, warn = TRUE) */
#define BUF_SIZE 1000
SEXP attribute_hidden do_readLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, ans2, tmp;
    int i, n, nn, nnn, ok, warn, nread, c, nbuf, buf_size = BUF_SIZE;
    Rconnection con = NULL;
    Rboolean wasopen;
    char *buf, *encoding;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CAR(args)));
    n = asInteger(CADR(args));
    if(n == NA_INTEGER)
	errorcall(call, _("invalid value for '%s'"), "n");
    ok = asLogical(CADDR(args));
    if(ok == NA_LOGICAL)
	errorcall(call, _("invalid value for '%s'"), "ok");
    warn = asLogical(CADDDR(args));
    if(warn == NA_LOGICAL)
	errorcall(call, _("invalid value for '%s'"), "warn");
    if(!con->canread)
	errorcall(call, _("cannot read from this connection"));
    if(!isString(CAD4R(args)) || LENGTH(CAD4R(args)) != 1)
	errorcall(call, _("invalid '%s' value"), "encoding");
    encoding = CHAR(STRING_ELT(CAD4R(args), 0)); /* ASCII */
    wasopen = con->isopen;
    if(!wasopen) {
	if(!con->open(con)) error(_("cannot open the connection"));
    } else { /* for a non-blocking connection, more input may
		have become available, so re-position */
	if(con->canseek && !con->blocking)
	    con->seek(con, con->seek(con, -1, 1, 1), 1, 1);
    }
    con->incomplete = FALSE;

    buf = (char *) malloc(buf_size);
    if(!buf)
	error(_("cannot allocate buffer in readLines"));
    nn = (n < 0) ? 1000 : n; /* initially allocate space for 1000 lines */
    nnn = (n < 0) ? INT_MAX : n;
    PROTECT(ans = allocVector(STRSXP, nn));
    for(nread = 0; nread < nnn; nread++) {
	if(nread >= nn) {
	    ans2 = allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	nbuf = 0;
	while((c = Rconn_fgetc(con)) != R_EOF) {
	    if(nbuf == buf_size) {
		buf_size *= 2;
		buf = (char *) realloc(buf, buf_size);
		if(!buf)
		    error(_("cannot allocate buffer in readLines"));
	    }
	    if(c != '\n') buf[nbuf++] = c; else break;
	}
	buf[nbuf] = '\0';
	tmp = mkChar(buf);
	if(streql(encoding, "latin1")) SET_LATIN1(tmp);
	else if(streql(encoding, "UTF-8")) SET_UTF8(tmp);
	SET_STRING_ELT(ans, nread, tmp);
	if(c == R_EOF) goto no_more_lines;
    }
    UNPROTECT(1);
    free(buf);
    if(!wasopen) con->close(con);
    return ans;
no_more_lines:
    if(!wasopen) con->close(con);
    if(nbuf > 0) { /* incomplete last line */
	if(con->text && con->blocking) {
	    nread++;
	    if(warn)
		warning(_("incomplete final line found by readLines on '%s'"),
			con->description);
	} else {
	    /* push back the rest */
	    con_pushback(con, 0, buf);
	    con->incomplete = TRUE;
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

/* writeLines(text, con = stdout(), sep = "\n") */
SEXP attribute_hidden do_writelines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    Rboolean wasopen;
    Rconnection con=NULL;
    char *ssep;
    SEXP text, sep;

    checkArity(op, args);
    text = CAR(args);
    if(!isString(text)) error(_("invalid '%s' argument"), "text");
    if(!inherits(CADR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    con = getConnection(asInteger(CADR(args)));
    sep = CADDR(args);
    if(!isString(sep)) error(_("invalid '%s' argument"), "sep");
    if(!con->canwrite)
	error(_("cannot write to this connection"));
    wasopen = con->isopen;
    if(!wasopen) {
	strcpy(con->mode, "wt");
	if(!con->open(con)) error(_("cannot open the connection"));
    }
    ssep = translateChar(STRING_ELT(sep, 0));
    for(i = 0; i < length(text); i++)
	Rconn_printf(con, "%s%s", translateChar(STRING_ELT(text, i)),
		     ssep);
    if(!wasopen) con->close(con);
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
    char *buf, *p, *new;
    int ibfs = 500, pos, m;

    buf = (char *) R_alloc(ibfs, sizeof(char));
    for(pos = 0; pos < 10000; pos++) {
	p = buf + pos;
	m = con->read(p, sizeof(char), 1, con);
	if(!m) {
	    if(pos > 0)
		warning(_("incomplete string at end of file has been discarded"));
	    return R_NilValue;
	}
	if(*p == '\0') break;
	if(pos >= ibfs - 1) {
	    new = (char *) R_alloc(2*ibfs, sizeof(char));
	    memcpy(new, buf, pos+1);
	    buf = new;
	    ibfs *= 2;
	}
    }
    if(pos == 10000)
	warning(_("null terminator not found: breaking string at 10000 chars"));
    return mkChar(buf);
}

static int
rawRead(char *p, int size, int n, Rbyte *bytes, int nbytes, int *np) 
{
    int avail, m;

    avail = (nbytes - *np)/size;
    m = n;
    if (m > avail) m = avail;
    if (m > 0) {
	memcpy(p, bytes + *(np), m*size);
	*np += m*size;
    }
    return m;
}

static SEXP rawOneString(Rbyte *bytes, int nbytes, int *np)
{
    Rbyte *p;
    int i;
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
    Free(buf);
    *np = nbytes;
    return res;
}

static SEXP rawFixedString(Rbyte *bytes, int len, int nbytes, int *np)
{
    int i;
    char *buf;
    SEXP res;

    i = *np + len;
    if(i < nbytes) nbytes = i;
    /* no terminator */
    buf = R_chk_calloc(nbytes - (*np) + 1, 1);
    memcpy(buf, bytes+(*np), nbytes-(*np));
    res = mkChar(buf);
    Free(buf);
    *np = nbytes;
    return res;
}

/* readBin(con, what, n, swap) */
SEXP attribute_hidden do_readbin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, swhat;
    int i, size, signd, swap, n, m = 0, sizedef= 4, mode = 1,
	nbytes = 0, np = 0;
    char *what;
    void *p = NULL;
    Rboolean wasopen = TRUE, isRaw = FALSE;
    Rconnection con = NULL;
    char *vmax = vmaxget();
    Rbyte *bytes = NULL;

    checkArity(op, args);
    
    if(TYPEOF(CAR(args)) == RAWSXP) {
	isRaw = TRUE;
	bytes = RAW(CAR(args));
	nbytes = LENGTH(CAR(args));
    } else {
	i = asInteger(CAR(args)); 
	if(i == NA_INTEGER || !(con = Connections[i]))
	    error(_("invalid connection"));
	if(con->text) error(_("can only read from a binary connection"));
    }
    
    args = CDR(args);
    swhat = CAR(args); args = CDR(args);
    if(!isString(swhat) || length(swhat) != 1)
	error(_("invalid value for '%s'"), "what");
    what = CHAR(STRING_ELT(swhat, 0)); /* ASCII */
    n = asInteger(CAR(args)); args = CDR(args);
    if(n == NA_INTEGER || n < 0) error(_("invalid value for '%s'"), "n");
    size = asInteger(CAR(args)); args = CDR(args);
    signd = asLogical(CAR(args)); args = CDR(args);
    if(signd == NA_LOGICAL)
	error(_("invalid value for '%s'"), "signed");
    swap = asLogical(CAR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid value for '%s'"), "swap");
    if(!isRaw) {
	if(!con->canread)
	    error(_("cannot read from this connection"));
	wasopen = con->isopen;
	if(!wasopen)
	    if(!con->open(con)) error(_("cannot open the connection"));
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
	m = isRaw ? rawRead(p, size, n, bytes, nbytes, &np) 
	    : con->read(p, size, n, con);
	if(swap)
	    for(i = 0; i < m; i++) {
		swapb(&(COMPLEX(ans)[i].r), sizeof(double));
		swapb(&(COMPLEX(ans)[i].i), sizeof(double));
	    }
    } else {
	if (!strcmp(what, "integer") || !strcmp(what, "int")) {
	    sizedef = sizeof(int); mode = 1;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(signed char):
	    case sizeof(short):
	    case sizeof(int):
#if SIZEOF_LONG == 8
	    case sizeof(long):
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
#endif
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
	    }
	    PROTECT(ans = allocVector(INTSXP, n));
	    p = (void *) INTEGER(ans);
	} else if (!strcmp(what, "logical")) {
	    sizedef = sizeof(int); mode = 1;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(signed char):
	    case sizeof(short):
	    case sizeof(int):
#if SIZEOF_LONG == 8
	    case sizeof(long):
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
#endif
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
	    }
	    PROTECT(ans = allocVector(LGLSXP, n));
	    p = (void *) LOGICAL(ans);
	} else if (!strcmp(what, "raw")) {
	    sizedef = 1; mode = 1;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case 1:
		break;
	    default:
		errorcall(call, _("raw is always of size 1"));
	    }
	    PROTECT(ans = allocVector(RAWSXP, n));
	    p = (void *) RAW(ans);
	} else if (!strcmp(what, "numeric") || !strcmp(what, "double")) {
	    sizedef = sizeof(double); mode = 2;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE
	    case sizeof(long double):
#endif
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
	    }
	    PROTECT(ans = allocVector(REALSXP, n));
	    p = (void *) REAL(ans);
	} else
	    error(_("invalid value for '%s'"), "what");
	    
	if(size == sizedef) {
	    m = isRaw ? rawRead(p, size, n, bytes, nbytes, &np) 
		: con->read(p, size, n, con);
	    if(swap && size > 1)
		for(i = 0; i < m; i++) swapb((char *)p+i*size, size);
	} else {
	    char * buf = R_alloc(1, size);
	    int s;
	    if(mode == 1) {
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead(buf, size, 1, bytes, nbytes, &np) 
			: con->read(buf, size, 1, con);
		    if(s) m++; else break;
		    if(swap && size > 1) swapb(buf, size);
		    switch(size) {
		    case sizeof(signed char):
			if(signd)
			    INTEGER(ans)[i] = (int)*((signed char *)buf);
			else
			    INTEGER(ans)[i] = (int)*((unsigned char *)buf);
			break;
		    case sizeof(short):
			if(signd)
			    INTEGER(ans)[i] = (int)*((short *)buf);
			else
			    INTEGER(ans)[i] = (int)*((unsigned short *)buf);
			break;
#if SIZEOF_LONG == 8
		    case sizeof(long):
			INTEGER(ans)[i] = (int)*((long *)buf);
			break;
#elif SIZEOF_LONG_LONG == 8
		    case sizeof(_lli_t):
			INTEGER(ans)[i] = (int)*((_lli_t *)buf);
			break;
#endif
		    default:
			errorcall(call,
				  _("size %d is unknown on this machine"),
				  size);
		    }
		}
	    } else if (mode == 2) {
		for(i = 0, m = 0; i < n; i++) {
		    s = isRaw ? rawRead(buf, size, 1, bytes, nbytes, &np) 
			: con->read(buf, size, 1, con);
		    if(s) m++; else break;
		    if(swap && size > 1) swapb(buf, size);
		    switch(size) {
		    case sizeof(float):
			REAL(ans)[i] = (double)*((float *)buf);
			break;
#if SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE
		    case sizeof(long double):
			REAL(ans)[i] = (double)*((long double *)buf);
			break;
#endif
		    default:
			errorcall(call, 
				  _("size %d is unknown on this machine"),
				  size);
		    }
		}
	    }
	}
    }
    vmaxset(vmax);
    if(!wasopen) con->close(con);
    if(m < n) {
	PROTECT(ans = lengthgets(ans, m));
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

/* writeBin(object, con, swap) */
SEXP attribute_hidden do_writebin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object, ans = R_NilValue;
    int i, j, size, swap, len, n = 0;
    char *s, *buf;
    Rboolean wasopen = TRUE, isRaw = FALSE;
    Rconnection con = NULL;

    checkArity(op, args);
    object = CAR(args);
    if(!isVectorAtomic(object))
	error(_("'x' is not an atomic vector type"));
    
    if(TYPEOF(CADR(args)) == RAWSXP) {
	isRaw = TRUE;
    } else {
	i = asInteger(CADR(args));
	if(i == NA_INTEGER || !(con = Connections[i]))
	    error("invalid connection");
	if(con->text) error(_("can only write to a binary connection"));
	wasopen = con->isopen;
	if(!con->canwrite)
	    error(_("cannot write to this connection"));
    }
    
    size = asInteger(CADDR(args));
    swap = asLogical(CADDDR(args));
    if(swap == NA_LOGICAL)
	error(_("invalid value for '%s'"), "swap");
    len = LENGTH(object);
    if(len == 0) {
	if(isRaw) return allocVector(RAWSXP, 0); else return R_NilValue;
    }

    if(!wasopen)
	if(!con->open(con)) error(_("cannot open the connection"));


    if(TYPEOF(object) == STRSXP) {
	if(isRaw) {
	    Rbyte *bytes;
	    int np, outlen;
	    for(i = 0, outlen = 0; i < len; i++) 
		outlen += strlen(translateChar(STRING_ELT(object, i))) + 1;
	    PROTECT(ans = allocVector(RAWSXP, outlen));
	    bytes = RAW(ans);
	    for(i = 0, np = 0; i < len; i++) {
		s = translateChar(STRING_ELT(object, i));
		memcpy(bytes+np, s, strlen(s) + 1);
		np +=  strlen(s) + 1;
	    }
	} else {
	    for(i = 0; i < len; i++) {
		s = translateChar(STRING_ELT(object, i));
		n = con->write(s, sizeof(char), strlen(s) + 1, con);
		if(!n) {
		    warning(_("problem writing to connection"));
		    break;
		}
	    }
	}
    } else {
	switch(TYPEOF(object)) {
	case LGLSXP:
	case INTSXP:
	    if(size == NA_INTEGER) size = sizeof(int);
	    switch (size) {
	    case sizeof(signed char):
	    case sizeof(short):
	    case sizeof(int):
#if SIZEOF_LONG == 8
	    case sizeof(long):
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
#endif
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
	    }
	    break;
	case REALSXP:
	    if(size == NA_INTEGER) size = sizeof(double);
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE
	    case sizeof(long double):
#endif
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
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
	buf = R_chk_calloc(len, size); /* R_alloc(len, size); */
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
		long l1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    l1 = (long) INTEGER(object)[i];
		    memcpy(buf + j, &l1, size);
		}
		break;
	    }
#elif SIZEOF_LONG_LONG == 8
	    case sizeof(_lli_t):
	    {
		_lli_t ll1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    ll1 = (_lli_t) INTEGER(object)[i];
		    memcpy(buf + j, &ll1, size);
		}
		break;
	    }
#endif
	    case 2:
	    {
		short s1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    s1 = (short) INTEGER(object)[i];
		    memcpy(buf + j, &s1, size);
		}
		break;
	    }
	    case 1:
		for (i = 0; i < len; i++)
		    buf[i] = (signed char) INTEGER(object)[i];
		break;
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
	    }
	    break;
	case REALSXP:
	    switch (size) {
	    case sizeof(double):
		memcpy(buf, REAL(object), size * len);
		break;
	    case sizeof(float):
	    {
		float f1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    f1 = (float) REAL(object)[i];
		    memcpy(buf+j, &f1, size);
		}
		break;
	    }
#if SIZEOF_LONG_DOUBLE > SIZEOF_DOUBLE
	    case sizeof(long double):
	    {
		long double ld1;
		for (i = 0, j = 0; i < len; i++, j+=size) {
		    ld1 = (long double) REAL(object)[i];
		    memcpy(buf+j, &ld1, size);
		}
		break;
	    }
#endif
	    default:
		errorcall(call, _("size %d is unknown on this machine"), size);
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
	if(isRaw) {
	    PROTECT(ans = allocVector(RAWSXP, size*len));
	    memcpy(RAW(ans), buf, size*len);
	} else {
	    n = con->write(buf, size, len, con);
	    if(n < len) warning(_("problem writing to connection"));
	}
	Free(buf);
    }

    if(!wasopen) con->close(con);
    if(isRaw) {
	R_Visible = TRUE;
	UNPROTECT(1);
    } else R_Visible = FALSE;
    return ans;
}

static SEXP readFixedString(Rconnection con, int len)
{
    char *buf;
    int  pos, m;
    SEXP ans;

#ifdef SUPPORT_UTF8
    if(utf8locale) {
	int i, clen;
	char *p, *q;
	p = buf = (char *) R_alloc(MB_CUR_MAX*len+1, sizeof(char));
	memset(buf, 0, MB_CUR_MAX*len+1);
	for(i = 0; i < len; i++) {
	    q = p;
	    m = con->read(p, sizeof(char), 1, con);
	    if(!m) { if(i == 0) return R_NilValue; else break;}
	    clen = utf8clen(*p++);
	    if(clen > 1) {
		m = con->read(p, sizeof(char), clen - 1, con);
		if(m < clen - 1) error(_("invalid UTF-8 input in readChar()"));
		p += clen - 1;
		if((int)mbrtowc(NULL, q, clen, NULL) < 0)
		    error(_("invalid UTF-8 input in readChar()"));
	    }
	}
	pos = p - buf;
    } else
#endif
    {
	buf = (char *) R_alloc(len+1, sizeof(char));
	memset(buf, 0, len+1);
	m = con->read(buf, sizeof(char), len, con);
	if(m == 0) return R_NilValue;
	pos = m;
    }
    /* String may contain nuls so don't use mkChar */
    ans = allocString(pos);
    memcpy(CHAR(ans), buf, pos);
    return ans;
}


/* readChar(con, nchars) */
SEXP attribute_hidden do_readchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, onechar, nchars;
    int i, len, n, m = 0, nbytes = 0, np = 0;
    Rboolean wasopen = TRUE;
    Rboolean isRaw = FALSE;
    Rconnection con = NULL;
    char *vmax = vmaxget();
    Rbyte *bytes = NULL;

    checkArity(op, args);
    
    if(TYPEOF(CAR(args)) == RAWSXP) {
    	isRaw = TRUE;
    	bytes = RAW(CAR(args));
    	nbytes = LENGTH(CAR(args));
    } else {
    	i = asInteger(CAR(args));
    	if(i == NA_INTEGER || !(con = Connections[i]))
	    error(_("invalid connection"));
    	if(!con->canread)
	    error(_("cannot read from this connection"));
    }
    nchars = CADR(args);
    n = LENGTH(nchars);
    if(n == 0) return allocVector(STRSXP, 0);
  
    if (!isRaw) {
    	wasopen = con->isopen;
    	if(!wasopen)
	    if(!con->open(con)) error(_("cannot open the connection"));
    }
    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0, m = i+1; i < n; i++) {
	len = INTEGER(nchars)[i];
	if(len == NA_INTEGER || len < 0)
	    error(_("invalid value for '%s'"), "nchar");
	onechar = isRaw ? rawFixedString(bytes, len, nbytes, &np)
	    : readFixedString(con, len);
	if(onechar != R_NilValue) {
	    SET_STRING_ELT(ans, i, onechar);
	    m++;
	} else break;
    }
    vmaxset(vmax);
    if(!wasopen) con->close(con);
    if(m < n) {
	PROTECT(ans = lengthgets(ans, m));
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return ans;
}

/* writeChar(object, con, nchars, sep) */
SEXP attribute_hidden do_writechar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object, nchars, sep, ans = R_NilValue;
    int i, len, lenb, lenc, n, nwrite=0, slen, tlen;
    char *s, *buf, *ssep = "";
    Rboolean wasopen = TRUE, usesep, isRaw = FALSE;
    Rconnection con = NULL;
    char *vmax = vmaxget();
#ifdef SUPPORT_MBCS
    mbstate_t mb_st;
#endif

    checkArity(op, args);
    object = CAR(args);
    if(TYPEOF(object) != STRSXP)
	error(_("invalid value for '%s'"), "object");
    if(TYPEOF(CADR(args)) == RAWSXP) {
    	isRaw = TRUE;
    } else {
    	i = asInteger(CADR(args));
    	if(i == NA_INTEGER || !(con = Connections[i]))
	    error(_("invalid connection"));
    	if(!con->canwrite)
	    error(_("cannot write to this connection"));
	wasopen = con->isopen;
    }
    
    nchars = CADDR(args);
    sep = CADDDR(args);
    if(isNull(sep)) {
	usesep = FALSE;
	slen = 0;
    } else {
	usesep = TRUE;
	if (!isString(sep) || length(sep) != 1)
	    error(_("invalid value for '%s'"), "sep");
	ssep = translateChar(STRING_ELT(sep, 0));
	slen = strlen(ssep) + 1;
    }
    n = LENGTH(nchars);
    if(LENGTH(object) < n)
	error(_("'object' is too short"));    
    if(n == 0) {
    	if(isRaw) return allocVector(RAWSXP, 0); else return R_NilValue;
    }

    len = 0;
    if (!isRaw) {
	for(i = 0; i < n; i++) {
	    /* This is not currently needed, just future-proofing in case
	       the logic gets changed */
	    tlen = strlen(translateChar(STRING_ELT(object, i)));
	    if (tlen > len) len = tlen;
	    tlen = INTEGER(nchars)[i];
	    if(tlen == NA_INTEGER || tlen < 0)
		error(_("invalid value for '%s'"), "nchar");
	    if (tlen > len) len = tlen;
	}
	buf = (char *) R_alloc(len + slen, sizeof(char));
    } else {
    	for (i = 0; i < n; i++) 
    	    len += INTEGER(nchars)[i] + slen;
    	PROTECT(ans = allocVector(RAWSXP, len));
    	buf = (char*)RAW(ans);
    }
    
    if(!wasopen)
	if(!con->open(con)) error(_("cannot open the connection"));

    	    
    for(i = 0; i < n; i++) {
	len = INTEGER(nchars)[i];
	s = translateChar(STRING_ELT(object, i));
	lenb = lenc = strlen(s);
#ifdef SUPPORT_MBCS
	if(mbcslocale) lenc = mbstowcs(NULL, s, 0);
#endif
	/* As from 1.8.1, zero-pad if too many chars are requested. */
	if(len > lenc) {
	    warning(_("writeChar: more characters requested than are in the string - will zero-pad"));
	    lenb += (len - lenc);
	}
	if(len < lenc) {
#ifdef SUPPORT_MBCS
	    if(mbcslocale) {
		/* find out how many bytes we need to write */
		int i, used;
		char *p = s;
		mbs_init(&mb_st);
		for(i = 0, lenb = 0; i < len; i++) {
		    used = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st);
		    p += used;
		    lenb += used;
		}
	    } else
#endif
		lenb = len;
	}
	memset(buf, '\0', lenb + slen);
	strncpy(buf, s, lenb);
	if (usesep) {
	    strcat(buf, ssep);
	    lenb += slen;
	}
	if (!isRaw) {
	    nwrite = con->write(buf, sizeof(char), lenb, con);
	    if(!nwrite) {
	    	warning(_("problem writing to connection"));
	    	break;
	    }
	} else 
	    buf += lenb;
    }
    vmaxset(vmax);
    if(!wasopen) con->close(con);
    if(isRaw) {
    	R_Visible = TRUE;
    	UNPROTECT(1);
    } else {
    	ans = R_NilValue;
    	R_Visible = FALSE;
    }
    return ans;
}

/* ------------------- push back text  --------------------- */


/* used in readLines and scan */
attribute_hidden
void con_pushback(Rconnection con, Rboolean newLine, char *line)
{
    int nexists = con->nPushBack;
    char **q;

    if(nexists > 0) {
	q = con->PushBack =
	    (char **) realloc(con->PushBack, (nexists+1)*sizeof(char *));
    } else {
	q = con->PushBack = (char **) malloc(sizeof(char *));
    }
    if(!q) error(_("could not allocate space for pushBack"));
    q += nexists;
    *q = (char *) malloc(strlen(line) + 1 + newLine);
    if(!(*q)) error(_("could not allocate space for pushBack"));
    strcpy(*q, line);
    if(newLine) strcat(*q, "\n");
    q++;
    con->posPushBack = 0;
    con->nPushBack++;
}


SEXP attribute_hidden do_pushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, n, nexists, newLine;
    Rconnection con = NULL;
    SEXP stext;
    char *p, **q;

    checkArity(op, args);

    stext = CAR(args);
    if(!isString(stext))
	error(_("invalid '%s' argument"), "data");
    i = asInteger(CADR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error(_("invalid connection"));
    newLine = asLogical(CADDR(args));
    if(newLine == NA_LOGICAL)
	error(_("invalid '%s' argument"), "newLine");
    if(!con->canread && !con->isopen)
	error(_("can only push back on open readable connections"));
    if(!con->text)
	error(_("can only push back on text-mode connections"));
    nexists = con->nPushBack;
    if((n = length(stext)) > 0) {
	if(nexists > 0) {
	    q = con->PushBack =
		(char **) realloc(con->PushBack, (n+nexists)*sizeof(char *));
	} else {
	    q = con->PushBack = (char **) malloc(n*sizeof(char *));
	}
	if(!q) error(_("could not allocate space for pushBack"));
	q += nexists;
	for(i = 0; i < n; i++) {
	    p = translateChar(STRING_ELT(stext, n - i - 1));
	    *q = (char *) malloc(strlen(p) + 1 + newLine);
	    if(!(*q)) error(_("could not allocate space for pushBack"));
	    strcpy(*q, p);
	    if(newLine) strcat(*q, "\n");
	    q++;
	}
	con->posPushBack = 0;
	con->nPushBack += n;
    }
    return R_NilValue;
}

SEXP attribute_hidden do_pushbacklength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    Rconnection con = NULL;
    SEXP ans;

    i = asInteger(CAR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error(_("invalid connection"));
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = con->nPushBack;
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_clearpushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;
    Rconnection con = NULL;

    i = asInteger(CAR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error(_("invalid connection"));

    if(con->nPushBack > 0) {
	for(j = 0; j < con->nPushBack; j++) free(con->PushBack[j]);
	free(con->PushBack);
    }
    return R_NilValue;
}

/* ------------------- sink functions  --------------------- */

/* Switch output to connection number icon, or popd stack if icon < 0
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
	    if(!con->open(con)) error(_("cannot open the connection"));
	    toclose = 1;
	}
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	SinkConsClose[R_SinkNumber] = toclose;
 	R_SinkSplit[R_SinkNumber] = tee;
   } else { /* removing a sink */
	if (R_SinkNumber <= 0) {
	    warning(_("no sink to remove"));
	    return FALSE;
	} else {
	    R_OutputCon = SinkCons[--R_SinkNumber];
	    if((icon = SinkCons[R_SinkNumber + 1]) >= 3) {
		Rconnection con = getConnection(icon);
		if(SinkConsClose[R_SinkNumber + 1] == 1) /* close it */
		    con->close(con);
		else if (SinkConsClose[R_SinkNumber + 1] == 2) /* destroy it */
		    con_close(icon);
	    }
	}
    }
    return TRUE;
}

/* This is not only used by cat(), but is in a public
   header, so we need a wrapper 

   Mo, Rconnections.h is not public and not installed.
*/

Rboolean attribute_hidden switch_stdout(int icon, int closeOnExit)
{
  return switch_or_tee_stdout(icon, closeOnExit, 0);
}

SEXP attribute_hidden do_sink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
  int icon, closeOnExit, errcon, tee;

    checkArity(op, args);
    icon = asInteger(CAR(args));
    closeOnExit = asLogical(CADR(args));
    if(closeOnExit == NA_LOGICAL)
	error(_("invalid value for '%s'"), "closeOnExit");
    errcon = asLogical(CADDR(args));
    if(errcon == NA_LOGICAL) error(_("invalid value for '%s'"), "type");
    tee = asLogical(CADDDR(args));
    if(tee == NA_LOGICAL) error(_("invalid value for '%s'"), "split");

#ifndef HAVE_VA_COPY
    if(tee) error(_("this platform does not support 'split=TRUE'"));
#endif

    if(!errcon) {
	/* allow space for cat() to use sink() */
	if(icon >= 0 && R_SinkNumber >= NSINKS - 2)
	    error(_("sink stack is full"));
	switch_or_tee_stdout(icon, closeOnExit, tee);
    } else {
	if(icon < 0) R_ErrorCon = 2;
	else {
	    getConnection(icon); /* check validity */
	    R_ErrorCon = icon;
	}
    }

    return R_NilValue;
}

SEXP attribute_hidden do_sinknumber(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int errcon;
    checkArity(op, args);

    errcon = asLogical(CAR(args));
    if(errcon == NA_LOGICAL)
	error(_("invalid value for '%s'"), "type");
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = errcon ? R_SinkNumber : R_ErrorCon;
    UNPROTECT(1);
    return ans;
}


/* ------------------- admin functions  --------------------- */

void attribute_hidden InitConnections()
{
    int i;
    Connections[0] = newterminal("stdin", "r");
    Connections[0]->fgetc = stdin_fgetc;
    Connections[1] = newterminal("stdout", "w");
    Connections[1]->vfprintf = stdout_vfprintf;
    Connections[1]->fflush = stdout_fflush;
    Connections[2] = newterminal("stderr", "w");
    Connections[2]->vfprintf = stderr_vfprintf;
    Connections[2]->fflush = stderr_fflush;
    for(i = 3; i < NCONNECTIONS; i++) Connections[i] = NULL;
    R_OutputCon = 1;
    R_SinkNumber = 0;
    SinkCons[0] = 1; R_ErrorCon = 2;
}

SEXP attribute_hidden do_getallconnections(SEXP call, SEXP op, SEXP args, SEXP env)
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

SEXP attribute_hidden do_sumconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, names;
    Rconnection Rcon;

    checkArity(op, args);
    Rcon = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(VECSXP, 7));
    PROTECT(names = allocVector(STRSXP, 7));
    SET_STRING_ELT(names, 0, mkChar("description"));
    SET_VECTOR_ELT(ans, 0, mkString(Rcon->description));
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
    UNPROTECT(2);
    return ans;
}


#if defined(USE_WININET_ASYNC) && !defined(USE_WININET)
# define USE_WININET 2
#endif


/* url(description, open, encoding) */
SEXP attribute_hidden do_url(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    char *url, *open, *class2 = "url";
    int ncon, block;
    Rconnection con = NULL;
#ifdef HAVE_INTERNET
    UrlScheme type = HTTPsh;	/* -Wall */
#endif

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error(_("invalid '%s' argument"), "description");
    if(length(scmd) > 1)
	warning(_("only first element of 'description' argument used"));
    url = CHAR(STRING_ELT(scmd, 0)); /* ASCII */
#ifdef HAVE_INTERNET
    if (strncmp(url, "http://", 7) == 0) type = HTTPsh;
    else if (strncmp(url, "ftp://", 6) == 0) type = FTPsh;
    else if (strncmp(url, "https://", 8) == 0) type = HTTPSsh;
#endif

    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error(_("invalid '%s' argument"), "open");
    open = CHAR(STRING_ELT(sopen, 0)); /* ASCII */
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error(_("invalid '%s' argument"), "block");
    enc = CADDDR(args);
    if(!isString(enc) || length(enc) != 1 ||
       strlen(CHAR(STRING_ELT(enc, 0))) > 100) /* ASCII */
	error(_("invalid '%s' argument"), "encoding");

    ncon = NextConnection();
    if(strncmp(url, "file://", 7) == 0) {
	int nh = 7;
#ifdef Win32
	/* on Windows we have file:///d:/path/to 
	   whereas on Unix it is file:///path/to */
	if (strlen(url) > 9 && url[7] == '/' && url[9] == ':') nh = 8;
#endif
	con = newfile(url + nh, strlen(open) ? open : "r");
	class2 = "file";
#ifdef HAVE_INTERNET
    } else if (strncmp(url, "http://", 7) == 0 ||
	       strncmp(url, "https://", 8) == 0 ||
	       strncmp(url, "ftp://", 6) == 0) {
       con = R_newurl(url, strlen(open) ? open : "r");
       ((Rurlconn)con->private)->type = type;
#endif
    } else {
	if(PRIMVAL(op)) { /* call to file() */
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
	    else
		con = newfile(url, strlen(open) ? open : "r");
	    class2 = "file";
	} else {
	    error(_("unsupported URL scheme"));
	}
    }

    Connections[ncon] = con;
    con->blocking = block;
    strncpy(con->encname, CHAR(STRING_ELT(enc, 0)), 100); /* ASCII */

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error(_("unable to open connection"));
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar(class2));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}

/* This function allows C code to call the write method of a
   connection.  It is mainly intended as a means for C code to do a
   buffered write to sockets, but could be the start of a more
   extensive C-level connection API.  LT */
size_t R_WriteConnection(Rconnection con, void *buf, size_t n)
{
    if(!con->isopen) error(_("connection is not open"));
    if(!con->canwrite) error(_("cannot write to this connection"));

    return con->write(buf, 1, n, con);
}

/* ------------------- (de)compression functions  --------------------- */

static int gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

#define get_byte() (icon->read(&ccc, 1, 1, icon), ccc)
#define Z_BUFSIZE 16384

static Rboolean gzcon_open(Rconnection con)
{
    Rgzconn priv = (Rgzconn)con->private;
    Rconnection icon = priv->con;
    int err;

    if(!icon->open(icon)) return FALSE;
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
    priv->inbuf = priv->outbuf = Z_NULL;
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
	    len  =  (uInt)get_byte();
	    len += ((uInt)get_byte())<<8;
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
	    for (len = 0; len < 2; len++) (void)get_byte();
	}
        priv->s.next_in  = priv->inbuf = (Byte*)malloc(Z_BUFSIZE);
        err = inflateInit2(&(priv->s), -MAX_WBITS);
    } else {
	/* write a header */
	char head[11];
        sprintf(head, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
		Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/,
		0 /*OS_CODE*/);
	icon->write(head, 1, 10, icon);
        err = deflateInit2(&(priv->s), priv->cp, Z_DEFLATED, -MAX_WBITS,
			   8, Z_DEFAULT_STRATEGY);
	priv->s.next_out = priv->outbuf = (Byte*)malloc(Z_BUFSIZE);
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
    Rgzconn priv = (Rgzconn)con->private;
    Rconnection icon = priv->con;
    int err;

    if(icon->canwrite) {
	uInt len;
	int done = 0;
	priv->s.avail_in = 0; /* should be zero already anyway */
	for (;;) {
	    len = Z_BUFSIZE - priv->s.avail_out;

	    if (len != 0) {
		if (icon->write(priv->outbuf, 1, len, icon) != len) {
		    priv->z_err = Z_ERRNO;
		    error(_("writing error whilst flushing 'gzcon' connection"));
		}
		priv->s.next_out = priv->outbuf;
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
	err = deflateEnd(&(priv->s));
	/* NB: these must be little-endian */
	putLong(icon, priv->crc);
	putLong(icon, (uLong)(priv->s.total_in & 0xffffffff));
    } else err = inflateEnd(&(priv->s));
    if(priv->inbuf) {free(priv->inbuf); priv->inbuf = Z_NULL;}
    if(priv->outbuf) {free(priv->outbuf); priv->outbuf = Z_NULL;}
    if(icon->isopen) icon->close(icon);
    con->isopen = FALSE;
}

static int gzcon_byte(Rgzconn priv)
{
    Rconnection icon = priv->con;

    if (priv->z_eof) return EOF;
    if (priv->s.avail_in == 0) {
	priv->s.avail_in = icon->read(priv->inbuf, 1, Z_BUFSIZE, icon);
        if (priv->s.avail_in == 0) {
            priv->z_eof = 1;
            return EOF;
        }
        priv->s.next_in = priv->inbuf;
    }
    priv->s.avail_in--;
    return *(priv->s.next_in)++;
}


static size_t gzcon_read(void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rgzconn priv = (Rgzconn)con->private;
    Rconnection icon = priv->con;
    Bytef *start = (Bytef*)ptr;
    uLong crc;
    int n;

    if (priv->z_err == Z_STREAM_END) return 0;  /* EOF */

    if (priv->nsaved >= 0) { /* non-compressed mode */
	size_t len = size*nitems;
	int i, nsaved = priv->nsaved;
	if (len == 0) return 0;
	if (len >= 2) {
	    for(i = 0; i < priv->nsaved; i++)
		((char *)ptr)[i] = priv->saved[i];
	    priv->nsaved = 0;
	    return (nsaved + icon->read((char *)ptr+nsaved, 1, len - nsaved,
					icon))/size;
	}
	if (len == 1) { /* size must be one */
	    if (nsaved > 0) {
		((char *)ptr)[0] = priv->saved[0];
		priv->saved[0] = priv->saved[1];
		priv->nsaved--;
		return 1;
	    } else
		return icon->read(ptr, 1, 1, icon);
	}
    }

    priv->s.next_out = (Bytef*)ptr;
    priv->s.avail_out = size*nitems;

    while (priv->s.avail_out != 0) {
        if (priv->s.avail_in == 0 && !priv->z_eof) {
            priv->s.avail_in = icon->read(priv->inbuf, 1, Z_BUFSIZE, icon);
            if (priv->s.avail_in == 0) priv->z_eof = 1;
            priv->s.next_in = priv->inbuf;
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
		crc += ((uLong)gzcon_byte(priv) << 24);
	    }
	    if (crc != priv->crc) {
		priv->z_err = Z_DATA_ERROR;
		REprintf(_("crc error %x %x\n"), crc, priv->crc);
	    }
	    /* finally, get (and ignore) length */
	    for (n = 0; n < 4; n++) gzcon_byte(priv);
	}
	if (priv->z_err != Z_OK || priv->z_eof) break;
    }
    priv->crc = crc32(priv->crc, start, (uInt)(priv->s.next_out - start));
    return (int)(size*nitems - priv->s.avail_out)/size;
}

static size_t gzcon_write(const void *ptr, size_t size, size_t nitems,
			  Rconnection con)
{
    Rgzconn priv = (Rgzconn)con->private;
    Rconnection icon = priv->con;

    priv->s.next_in = (Bytef*)ptr;
    priv->s.avail_in = size*nitems;

    while (priv->s.avail_in != 0) {
        if (priv->s.avail_out == 0) {
            priv->s.next_out = priv->outbuf;
            if (icon->write(priv->outbuf, 1, Z_BUFSIZE, icon) != Z_BUFSIZE) {
                priv->z_err = Z_ERRNO;
		warning(_("write error on 'gzcon' connection"));
                break;
            }
            priv->s.avail_out = Z_BUFSIZE;
        }
        priv->z_err = deflate(&(priv->s), Z_NO_FLUSH);
        if (priv->z_err != Z_OK) break;
    }
    priv->crc = crc32(priv->crc, (const Bytef *)ptr, size*nitems);
    return (int)(size*nitems - priv->s.avail_in)/size;
}

static int gzcon_fgetc(Rconnection con)
{
    unsigned char c;
    int n = gzcon_read(&c, 1, 1, con);
    return (n == 1) ? c : R_EOF;
}


/* gzcon(con, level) */
SEXP attribute_hidden do_gzcon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, class;
    int icon, level, allow;
    Rconnection incon=NULL, new=NULL;
    char *m, *mode = NULL /* -Wall */,  description[1000];

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, _("'con' is not a connection"));
    incon = getConnection(icon = asInteger(CAR(args)));
    level = asInteger(CADR(args));
    if(level == NA_INTEGER || level < 0 || level > 9)
	errorcall(call, _("'level' must be one of 0 ... 9"));
    allow = asLogical(CADDR(args));
    if(allow == NA_INTEGER)
	errorcall(call, _("'allowNonCompression' must be TRUE or FALSE"));

    if(incon->isGzcon) {
	warningcall(call, _("this is already a gzcon connection"));
	return CAR(args);
    }
    m = incon->mode;
    if(strcmp(m, "r") == 0 || strncmp(m, "rb", 2) == 0) mode = "rb";
    else if (strcmp(m, "w") == 0 || strncmp(m, "wb", 2) == 0) mode = "wb";
    else errorcall(call, _("can only use read- or write- binary connections"));
    if(strcmp(incon->class, "file") == 0 &&
       (strcmp(m, "r") == 0 || strcmp(m, "w") == 0))
	warning(_("using a text-mode 'file' connection may not work correctly"));

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of 'gzcon' connection failed"));
    new->class = (char *) malloc(strlen("gzcon") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of 'gzcon' connection failed"));
    }
    strcpy(new->class, "gzcon");
    sprintf(description, "gzcon(%s)", incon->description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of 'gzcon' connection failed"));
    }
    init_con(new, description, mode);
    new->text = FALSE;
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
    }
    ((Rgzconn)(new->private))->con = incon;
    ((Rgzconn)(new->private))->cp = level;
    ((Rgzconn)(new->private))->nsaved = -1;
    ((Rgzconn)(new->private))->allow = allow;

    Connections[icon] = new;
    strncpy(new->encname, incon->encname, 100);
    if(incon->isopen) new->open(new);
    /* show we do encoding here */

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = icon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("gzcon"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}


/* code for in-memory (de)compression
   of data stored in a scalar string. Uses a 4-byte header of length,
   in XDR order. */

#ifndef WORDS_BIGENDIAN
static unsigned int uiSwap (unsigned int x)
{
  return((x << 24) | ((x & 0xff00) << 8) | ((x & 0xff0000) >> 8) | (x >> 24));
}
#else
#define uiSwap(x) (x)
#endif

attribute_hidden
SEXP R_compress1(SEXP in)
{
    uLong inlen, outlen;
    int res;
    Bytef *buf;
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("R_decompress1 requires a raw vector"));
    inlen = LENGTH(in);
    outlen = 1.001*inlen + 20;
    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    res = compress(buf + 4, &outlen, (Bytef *)RAW(in), inlen);
    if(res != Z_OK) error(_("internal error in R_compress1"));
    ans = allocVector(RAWSXP, outlen + 4);
    memcpy(RAW(ans), buf, outlen + 4);
    return ans;
}

attribute_hidden
SEXP R_decompress1(SEXP in)
{
    uLong inlen, outlen;
    int res;
    Bytef *buf;
    unsigned char *p = RAW(in);
    SEXP ans;

    if(TYPEOF(in) != RAWSXP)
	error(_("R_decompress1 requires a raw vector"));
    inlen = LENGTH(in);
    outlen = (uLong) uiSwap(*((unsigned int *) p));
    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    res = uncompress(buf, &outlen, (Bytef *)(p + 4), inlen - 4);
    if(res != Z_OK) error(_("internal error in R_decompress1"));
    ans = allocVector(RAWSXP, outlen);
    memcpy(RAW(ans), buf, outlen);
    return ans;
}

SEXP attribute_hidden do_sockselect(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    Rboolean immediate = FALSE;
    int nsock, i;
    SEXP insock, write, val, insockfd;
    double timeout;

    checkArity(op, args);

    insock = CAR(args);
    if (TYPEOF(insock) != VECSXP || LENGTH(insock) == 0)
	errorcall(call, _("not a list of sockets"));
    nsock = LENGTH(insock);

    write = CADR(args);
    if (TYPEOF(write) != LGLSXP || LENGTH(write) != nsock)
	errorcall(call, _("bad write indicators"));

    timeout = asReal(CADDR(args));

    PROTECT(insockfd = allocVector(INTSXP, nsock));
    PROTECT(val = allocVector(LGLSXP, nsock));

    for (i = 0; i < nsock; i++) {
	Rconnection conn = getConnection(asInteger(VECTOR_ELT(insock, i)));
	Rsockconn scp = (Rsockconn) conn->private;
	if (strcmp(conn->class, "socket") != 0)
	    errorcall(call, _("not a socket connection"));
	INTEGER(insockfd)[i] = scp->fd;
	if (! LOGICAL(write)[i] && scp->pstart < scp->pend) {
	    LOGICAL(val)[i] = TRUE;
	    immediate = TRUE;
	}
	else LOGICAL(val)[i] = FALSE;
    }

    if (! immediate)
	Rsockselect(nsock, INTEGER(insockfd), LOGICAL(val), LOGICAL(write),
		    timeout);

    UNPROTECT(2);
    return val;
}
