/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-3   The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Fileio.h>
#include <zlib.h> /* needs to be before Rconnections.h */
#include <Rconnections.h>
#include <R_ext/Complex.h>
#include <R_ext/R-ftp-http.h>
#include <R_ext/RS.h> /* R_chk_calloc and Free */
#undef ERROR /* for compilation on Windows */

int R_OutputCon;		/* used in printutils.c */

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

/* Win32 does have popen, but it does not work in GUI applications,
   so test that later */
#ifdef Win32
# include <Startup.h>
  extern UImode  CharacterMode;
#endif

#define NCONNECTIONS 50
#define NSINKS 21

static Rconnection Connections[NCONNECTIONS];
static SEXP OutTextData;

static int R_SinkNumber;
static int SinkCons[NSINKS], SinkConsClose[NSINKS];

static void
pushback(Rconnection con, int newLine, char *line);

/* ------------- admin functions (see also at end) ----------------- */

int NextConnection()
{
    int i;
    for(i = 3; i < NCONNECTIONS; i++)
	if(!Connections[i]) break;
    if(i >= NCONNECTIONS)
	error("All connections are in use");
    return i;
}

static int ConnIndex(Rconnection con)
{
    int i;
    for(i = 0; i < NCONNECTIONS; i++)
	if(Connections[i] == con) break;
    if(i >= NCONNECTIONS)
	error("connection not found");
    return i;
}

/* internal, not the same as R function getConnection */
Rconnection getConnection(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	error("invalid connection");
    return con;

}

/* for use in REvprintf */
Rconnection getConnection_no_err(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n >= NCONNECTIONS || n == NA_INTEGER ||
       !(con = Connections[n]))
	return NULL;
    return con;

}

void Rconn_setEncoding(Rconnection con, SEXP enc)
{
    int i;

    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");
    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];
}


/* ------------------- null connection functions --------------------- */

static Rboolean null_open(Rconnection con)
{
    error("open/close not enabled for this connection");
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
    error("printing not enabled for this connection");
    return 0;			/* -Wall */
}

#define BUFSIZE 1000
int dummy_vfprintf(Rconnection con, const char *format, va_list ap)
{
    char buf[BUFSIZE], *b = buf, *vmax = vmaxget();
    int res, usedRalloc = FALSE;

    res = vsnprintf(buf, BUFSIZE, format, ap);
    if(res >= BUFSIZE) { /* res is the desired output length */
	usedRalloc = TRUE;
	b = R_alloc(res + 1, sizeof(char));
	vsprintf(b, format, ap);
    } else if(res < 0) { /* just a failure indication -- e.g. Windows */
	usedRalloc = TRUE;
	b = R_alloc(100*BUFSIZE, sizeof(char));
	res = vsnprintf(b, 100*BUFSIZE, format, ap);
	if (res < 0) {
	    *(b + 100*BUFSIZE - 1) = '\0';
	    warning("printing of extremely long output is truncated");
	    res = 100*BUFSIZE;
	}
    }
    con->write(b, 1, res, con);
    if(usedRalloc) vmaxset(vmax);
    return res;
}

static int null_fgetc(Rconnection con)
{
    error("getc not enabled for this connection");
    return 0;			/* -Wall */
}

static long null_seek(Rconnection con, int where, int origin, int rw)
{
    error("seek not enabled for this connection");
    return 0;			/* -Wall */
}

static void null_truncate(Rconnection con)
{
    error("truncate not enabled for this connection");
}

static int null_fflush(Rconnection con)
{
    return 0;
}

static size_t null_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    error("read not enabled for this connection");
    return 0;			/* -Wall */
}

static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error("write not enabled for this connection");
    return 0;			/* -Wall */
}

void init_con(Rconnection new, char *description, char *mode)
{
    strcpy(new->description, description);
    strncpy(new->mode, mode, 4); new->mode[4] = '\0';
    new->isopen = new->incomplete = FALSE;
    new->canread = new->canwrite = TRUE; /* in principle */
    new->canseek = FALSE;
    new->text = TRUE;
    new->isGzcon = FALSE;
    new->open = &null_open;
    new->close = &null_close;
    new->destroy = &null_destroy;
    new->vfprintf = &null_vfprintf;
    new->fgetc = &null_fgetc;
    new->seek = &null_seek;
    new->truncate = &null_truncate;
    new->fflush = &null_fflush;
    new->read = &null_read;
    new->write = &null_write;
    new->nPushBack = 0;
    new->save = new->save2 = -1000;
    new->private = NULL;
}

/* ------------------- file connections --------------------- */

static Rboolean file_open(Rconnection con)
{
    char *name;
    FILE *fp;
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
    fp = R_fopen(name, con->mode);
    if(!fp) {
	warning("cannot open file `%s'", name);
	return FALSE;
    }
    if(temp) unlink(name);
    this->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(mlen >= 2 && con->mode[1] == '+')
	con->canread = con->canwrite = TRUE;
    this->last_was_write = !con->canread;
    this->rpos = 0;
    if(con->canwrite) this->wpos = ftell(fp);
    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;

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
    fclose(((Rfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static int file_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Rfileconn this = con->private;

    if(!this->last_was_write) {
	this->rpos = ftell(this->fp);
	this->last_was_write = TRUE;
	fseek(this->fp, this->wpos, SEEK_SET);
    }
    return vfprintf(this->fp, format, ap);
}

static int file_fgetc(Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
    int c;

    if(this->last_was_write) {
	this->wpos = ftell(this->fp);
	this->last_was_write = FALSE;
	fseek(this->fp, this->rpos, SEEK_SET);
    }
    c = fgetc(fp);
    return feof(fp) ? R_EOF : con->encoding[c];
}

static long file_seek(Rconnection con, int where, int origin, int rw)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
    long pos = ftell(fp);
    int whence = SEEK_SET;

    /* make sure both positions are set */
    if(this->last_was_write) this->wpos = pos; else this->rpos = pos;
    if(rw == 1) {
	if(!con->canread) error("connection is not open for reading");
	pos = this->rpos;
	this->last_was_write = FALSE;
    }
    if(rw == 2) {
	if(!con->canwrite) error("connection is not open for writiing");
	pos = this->wpos;
	this->last_was_write = TRUE;
    }
    if(where == NA_INTEGER) return pos;

    switch(origin) {
    case 2: whence = SEEK_CUR; break;
    case 3: whence = SEEK_END; break;
    default: whence = SEEK_SET;
    }
    fseek(fp, where, whence);
    if(this->last_was_write) this->wpos = ftell(this->fp);
    else this->rpos = ftell(this->fp);
    return pos;
}

static void file_truncate(Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;
    int fd = fileno(fp);
    int size = lseek(fd, 0, SEEK_CUR);

    if(!con->isopen || !con->canwrite)
	error("can only truncate connections open for writing");

    if(!this->last_was_write) this->rpos = ftell(this->fp);
#ifdef HAVE_FTRUNCATE
    if(ftruncate(fd, size))
	error("file truncation failed");
#elif defined(Win32)
    if(chsize(fd, size))
	error("file truncation failed");
#else
    error("Unavailable on this platform");
#endif
    this->last_was_write = TRUE;
    this->wpos = ftell(this->fp);
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
	this->wpos = ftell(this->fp);
	this->last_was_write = FALSE;
	fseek(this->fp, this->rpos, SEEK_SET);
    }
    return fread(ptr, size, nitems, fp);
}

static size_t file_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rfileconn this = con->private;
    FILE *fp = this->fp;

    if(!this->last_was_write) {
	this->rpos = ftell(this->fp);
	this->last_was_write = TRUE;
	fseek(this->fp, this->wpos, SEEK_SET);
    }
    return fwrite(ptr, size, nitems, fp);
}

static Rconnection newfile(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of file connection failed");
    new->class = (char *) malloc(strlen("file") + 1);
    if(!new->class) {
	free(new);
	error("allocation of file connection failed");
    }
    strcpy(new->class, "file");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of file connection failed");
    }
    init_con(new, description, mode);
    new->open = &file_open;
    new->close = &file_close;
    new->vfprintf = &file_vfprintf;
    new->fgetc = &file_fgetc;
    new->seek = &file_seek;
    new->truncate = &file_truncate;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->canseek = TRUE;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of file connection failed");
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
	    res = mkfifo(name, 00644);
	    if(res) {
		warning("cannot create fifo `%s'", name);
		return FALSE;
	    }
	} else {
	    if(!(sb.st_mode & S_IFIFO)) {
		warning("`%s' exists but is not a fifo", name);
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
	if(errno == ENXIO) warning("fifo `%s' is not ready", name);
	else warning("cannot open fifo `%s'", name);
	return FALSE;
    }

    this->fd = fd;
    con->isopen = TRUE;

    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    return TRUE;
}

static void fifo_close(Rconnection con)
{
    close(((Rfifoconn)(con->private))->fd);
    con->isopen = FALSE;
}

static int fifo_fgetc(Rconnection con)
{
    Rfifoconn this = (Rfifoconn)con->private;
    unsigned char c;
    int n;

    n = read(this->fd, (char *)&c, 1);
    return (n == 1) ? con->encoding[c] : R_EOF;
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
    if(!new) error("allocation of fifo connection failed");
    new->class = (char *) malloc(strlen("fifo") + 1);
    if(!new->class) {
	free(new);
	error("allocation of fifo connection failed");
    }
    strcpy(new->class, "fifo");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of fifo connection failed");
    }
    init_con(new, description, mode);
    new->open = &fifo_open;
    new->close = &fifo_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &fifo_fgetc;
    new->seek = &null_seek;
    new->truncate = &null_truncate;
    new->fflush = &null_fflush;
    new->read = &fifo_read;
    new->write = &fifo_write;
    new->private = (void *) malloc(sizeof(struct fifoconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of fifo connection failed");
    }
    return new;
}
#endif

SEXP do_fifo(SEXP call, SEXP op, SEXP args, SEXP env)
{
#if defined(HAVE_MKFIFO) && defined(HAVE_FCNTL_H)
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int i, ncon, block;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, "invalid `description' argument");
    if(length(sfile) > 1)
	warning("only first element of `description' argument used");
    file = CHAR(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error("invalid `block' argument");
    enc = CADDDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    ncon = NextConnection();
    con = Connections[ncon] = newfifo(file, strlen(open) ? open : "r");
    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];
    con->blocking = block;

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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
    error("fifo connections are not available on this system");
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
    fp = R_popen(con->description, mode);
    if(!fp) {
	warning("cannot open cmd `%s'", con->description);
	return FALSE;
    }
    ((Rfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
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
    if(!new) error("allocation of pipe connection failed");
    new->class = (char *) malloc(strlen("pipe") + 1);
    if(!new->class) {
	free(new);
	error("allocation of pipe connection failed");
    }
    strcpy(new->class, "pipe");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of pipe connection failed");
    }
    init_con(new, description, mode);
    new->open = &pipe_open;
    new->close = &pipe_close;
    new->vfprintf = &file_vfprintf;
    new->fgetc = &file_fgetc;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of pipe connection failed");
    }
    return new;
}
#endif

#ifdef Win32
extern Rconnection newWpipe(char *description, char *mode);
#endif

SEXP do_pipe(SEXP call, SEXP op, SEXP args, SEXP env)
{
#ifdef HAVE_POPEN
    SEXP scmd, sopen, ans, class, enc;
    char *file, *open;
    int i, ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error("invalid `description' argument");
    if(length(scmd) > 1)
	warning("only first element of `description' argument used");
    file = CHAR(STRING_ELT(scmd, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    enc = CADDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");

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
    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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
    error("pipe connections are not available on this system");
    return R_NilValue;		/* -Wall */
#endif
}

/* ------------------- gzipped file connections --------------------- */

static Rboolean gzfile_open(Rconnection con)
{
    gzFile fp;

    fp = gzopen(R_ExpandFileName(con->description), con->mode);
    if(!fp) {
	warning("cannot open compressed file `%s'",
	      R_ExpandFileName(con->description));
	return FALSE;
    }
    ((Rgzfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    return TRUE;
}

static void gzfile_close(Rconnection con)
{
    gzclose(((Rgzfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static int gzfile_fgetc(Rconnection con)
{
    gzFile fp = ((Rgzfileconn)(con->private))->fp;

    /* Looks like eof is signalled one char early */
    if(gzeof(fp)) return R_EOF;
    return con->encoding[gzgetc(fp)];
}

static long gzfile_seek(Rconnection con, int where, int origin, int rw)
{
    gzFile  fp = ((Rgzfileconn)(con->private))->fp;
    long pos = gztell(fp);
    int whence = SEEK_SET;

    switch(origin) {
    case 2: whence = SEEK_CUR;
    case 3: whence = SEEK_END;
    default: whence = SEEK_SET;
    }
    if(where >= 0) gzseek(fp, where, whence);
    return pos;
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
    return gzwrite(fp, (const voidp)ptr, size*nitems)/size;
}

static Rconnection newgzfile(char *description, char *mode, int compress)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of file connection failed");
    new->class = (char *) malloc(strlen("gzfile") + 1);
    if(!new->class) {
	free(new);
	error("allocation of gzfile connection failed");
    }
    strcpy(new->class, "gzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of gzfile connection failed");
    }
    init_con(new, description, "");
    strncpy(new->mode, mode, 1);
    sprintf(new->mode+1, "b%1d", compress);

    new->canseek = TRUE;
    new->open = &gzfile_open;
    new->close = &gzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &gzfile_fgetc;
    new->seek = &gzfile_seek;
    new->fflush = &gzfile_fflush;
    new->read = &gzfile_read;
    new->write = &gzfile_write;
    new->private = (void *) malloc(sizeof(struct gzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of gzfile connection failed");
    }
    return new;
}

SEXP do_gzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int i, ncon, compress;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, "invalid `description' argument");
    if(length(sfile) > 1)
	warning("only first element of `description' argument used");
    file = CHAR(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    enc = CADDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");
    compress = asInteger(CADDDR(args));
    if(compress == NA_LOGICAL || compress < 0 || compress > 9)
	error("invalid `compress' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    ncon = NextConnection();
    con = Connections[ncon] = newgzfile(file, strlen(open) ? open : "r",
					compress);

    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
	}
    }

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("file"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}

/* ------------------- bzipped file connections --------------------- */

#if defined(HAVE_BZLIB) || defined(Unix) || defined(Win32)
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
    fp = fopen(R_ExpandFileName(con->description), mode);
    if(!fp) {
	warning("cannot open bzip2-ed file `%s'",
		R_ExpandFileName(con->description));
	return FALSE;
    }
    if(con->canread) {
	bfp = BZ2_bzReadOpen(&bzerror, fp, 0, 0, NULL, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzReadClose(&bzerror, bfp);
	    fclose(fp);
	    warning("file `%s' appears not to be compressed by bzip2",
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    } else {
	bfp = BZ2_bzWriteOpen(&bzerror, fp, 9, 0, 0);
	if(bzerror != BZ_OK) {
	    BZ2_bzWriteClose(&bzerror, bfp, 0, NULL, NULL);
	    fclose(fp);
	    warning("file `%s' appears not to be compressed by bzip2",
		    R_ExpandFileName(con->description));
	    return FALSE;
	}
    }
    ((Rbzfileconn)(con->private))->fp = fp;
    ((Rbzfileconn)(con->private))->bfp = bfp;
    con->isopen = TRUE;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
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

static int bzfile_fgetc(Rconnection con)
{
    BZFILE* bfp = (BZFILE *)((Rbzfileconn)(con->private))->bfp;
    char buf[1];
    int bzerror, size, p;

    size = BZ2_bzRead(&bzerror, bfp, buf, 1);
    /* Some versions seem to signal end a char or two early, so play safe
       if(bzerror == BZ_STREAM_END) return R_EOF;
       if(bzerror != BZ_OK || size < 1) return R_EOF; */
    if(size < 1) return R_EOF;
    p = buf[0] % 256;
    return con->encoding[p];
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

    BZ2_bzWrite(&bzerror, bfp, (const voidp)ptr, size*nitems);
    if(bzerror != BZ_OK) return 0;
    else return nitems;
}

static Rconnection newbzfile(char *description, char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of file connection failed");
    new->class = (char *) malloc(strlen("bzfile") + 1);
    if(!new->class) {
	free(new);
	error("allocation of bzfile connection failed");
    }
    strcpy(new->class, "bzfile");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of bzfile connection failed");
    }
    init_con(new, description, mode);

    new->canseek = FALSE;
    new->open = &bzfile_open;
    new->close = &bzfile_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &bzfile_fgetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &bzfile_read;
    new->write = &bzfile_write;
    new->private = (void *) malloc(sizeof(struct bzfileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of bzfile connection failed");
    }
    return new;
}

SEXP do_bzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int i, ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, "invalid `description' argument");
    if(length(sfile) > 1)
	warning("only first element of `description' argument used");
    file = CHAR(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    enc = CADDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    ncon = NextConnection();
    con = Connections[ncon] = newbzfile(file, strlen(open) ? open : "r");

    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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
#else
SEXP do_bzfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("bzfile is not available on this system");
    return R_NilValue;		/* -Wall */
}
#endif


/* ------------------- clipboard connections --------------------- */

#ifdef Win32
#include <windows.h>
extern int clipboardhastext(); /* from ga.h */


static Rboolean clp_open(Rconnection con)
{
    Rclpconn this = con->private;
    HGLOBAL hglb;
    char *pc;

    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    this->pos = 0;
    if(con->canread) {
	/* copy the clipboard contents now */
	if(clipboardhastext() &&
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
		warning("memory allocation to copy clipboard failed");
		return FALSE;
	    }
	} else {
	    warning("clipboard cannot be opened or contains no text");
	    return FALSE;
	}
    } else {
	int len = 32*1024;
	this->buff = (char *)malloc(len + 1);
	this->len = len;
	this->last = 0;
	if(!this->buff) {
	    warning("memory allocation to open clipboard failed");
	    return FALSE;
	}
    }
    con->text = TRUE;
    con->save = -1000;

    return TRUE;
}

static void clp_writeout(Rconnection con)
{
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
	    warning("Unable to open the clipboard");
	    GlobalFree(hglb);
	} else {
	    if(!SetClipboardData(CF_TEXT, hglb)) {
		warning("Unable to write to the clipboard");
		GlobalFree(hglb);
	    }
	    CloseClipboard();
	}
    }
}

static void clp_close(Rconnection con)
{
    Rclpconn this = con->private;

    con->isopen = FALSE;
    if(con->canwrite)
	clp_writeout(con);
    free(this->buff);
}

static int clp_fgetc(Rconnection con)
{
    Rclpconn this = con->private;
    int c;

    if (this->pos >= this->len) return R_EOF;
    c = this->buff[this->pos++];
    return con->encoding[c];
}

static long clp_seek(Rconnection con, int where, int origin, int rw)
{
    Rclpconn this = con->private;
    int newpos, oldpos = this->pos;

    if(where == NA_INTEGER) return oldpos;

    switch(origin) {
    case 2: newpos = this->pos + where; break;
    case 3: newpos = this->last + where; break;
    default: newpos = where;
    }
    if(newpos < 0 || newpos >= this->last)
	error("attempt to seek outside the range of the clipboard");
    else this->pos = newpos;

    return oldpos;
}

static void clp_truncate(Rconnection con)
{
    Rclpconn this = con->private;

    if(!con->isopen || !con->canwrite)
	error("can only truncate connections open for writing");
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

    /* clipboard requires CRLF termination */
    for(i = 0; i < len; i++) {
	if(this->pos >= this->len) break;
	c = *p++;
	if(c == '\n') {
	    *q++ = '\r';
	    this->pos++;
	    if(this->pos >= this->len) break;
	}
	*q++ = c;
	this->pos++;
	used++;
    }
    if(this->last < this->pos) this->last = this->pos;
    return (size_t) used/size;
}

static Rconnection newclp(char *mode)
{
    Rconnection new;
    char description[] = "clipboard";

    if(strlen(mode) != 1 ||
       (mode[0] != 'r' && mode[0] != 'w'))
	error("`mode' for the clipboard must be `r' or `w'");
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of clipboard connection failed");
    new->class = (char *) malloc(strlen(description) + 1);
    if(!new->class) {
	free(new);
	error("allocation of clipboard connection failed");
    }
    strcpy(new->class, description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of clipboard connection failed");
    }
    init_con(new, description, mode);
    new->open = &clp_open;
    new->close = &clp_close;
    new->vfprintf = &dummy_vfprintf;
    new->fgetc = &clp_fgetc;
    new->seek = &clp_seek;
    new->truncate = &clp_truncate;
    new->fflush = &clp_fflush;
    new->read = &clp_read;
    new->write = &clp_write;
    new->canseek = TRUE;
    new->private = (void *) malloc(sizeof(struct clpconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of clipboard connection failed");
    }
    return new;
}

#endif /* Win32 */

/* ------------------- terminal connections --------------------- */

/* The size of the console buffer */
#define CONSOLE_BUFFER_SIZE	1024

static unsigned char  ConsoleBuf[CONSOLE_BUFFER_SIZE];
static unsigned char *ConsoleBufp;
static int  ConsoleBufCnt;

static int ConsoleGetchar()
{
    if (--ConsoleBufCnt < 0) {
	if (R_ReadConsole("", ConsoleBuf, CONSOLE_BUFFER_SIZE, 0) == 0) {
	    R_ClearerrConsole();
	    return R_EOF;
	}
	R_ParseCnt++;
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
    if(!new) error("allocation of terminal connection failed");
    new->class = (char *) malloc(strlen("terminal") + 1);
    if(!new->class) {
	free(new);
	error("allocation of terminal connection failed");
    }
    strcpy(new->class, "terminal");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of terminal connection failed");
    }
    init_con(new, description, mode);
    new->isopen = TRUE;
    new->canread = (strcmp(mode, "r") == 0);
    new->canwrite = (strcmp(mode, "w") == 0);
    new->destroy = &null_close;
    new->private = NULL;
    return new;
}


SEXP do_stdin(SEXP call, SEXP op, SEXP args, SEXP env)
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

SEXP do_stdout(SEXP call, SEXP op, SEXP args, SEXP env)
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


SEXP do_stderr(SEXP call, SEXP op, SEXP args, SEXP env)
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
	nchars += strlen(CHAR(STRING_ELT(text, i))) + 1;
    this->data = (char *) malloc(nchars+1);
    if(!this->data) {
	free(this); free(con->description); free(con->class); free(con);
	error("cannot allocate memory for text connection");
    }
    *(this->data) = '\0';
    for(i = 0; i < nlines; i++) {
	strcat(this->data, CHAR(STRING_ELT(text, i)));
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
    this->cur = this->nchars = 0;
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

static long text_seek(Rconnection con, int where, int origin, int rw)
{
    if(where >= 0) error("seek is not relevant for text connection");
    return 0; /* if just asking, always at the beginning */
}

static Rconnection newtext(char *description, SEXP text)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of text connection failed");
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error("allocation of text connection failed");
    }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of text connection failed");
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
	error("allocation of text connection failed");
    }
    text_init(new, text);
    return new;
}

static void outtext_close(Rconnection con)
{
    Routtextconn this = (Routtextconn)con->private;
    SEXP tmp;
    int idx = ConnIndex(con);

    if(strlen(this->lastline) > 0) {
	PROTECT(tmp = lengthgets(this->data, ++this->len));
	SET_STRING_ELT(tmp, this->len - 1, mkChar(this->lastline));
	defineVar(this->namesymbol, tmp, VECTOR_ELT(OutTextData, idx));
	this->data = tmp;
	UNPROTECT(1);
    }
    SET_VECTOR_ELT(OutTextData, idx, R_NilValue);
}

static void outtext_destroy(Rconnection con)
{
    Routtextconn this = (Routtextconn)con->private;
    free(this->lastline);
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
	    warning("printing of extremely long output is truncated");
	}
    }

    /* copy buf line-by-line to object */
    for(p = b; ; p = q+1) {
	q = strchr(p, '\n');
	if(q) {
	    int idx = ConnIndex(con);
	    *q = '\0';
	    PROTECT(tmp = lengthgets(this->data, ++this->len));
	    SET_STRING_ELT(tmp, this->len - 1, mkChar(p));
	    defineVar(this->namesymbol, tmp, VECTOR_ELT(OutTextData, idx));
	    this->data = tmp;
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

static void outtext_init(Rconnection con, char *mode, int idx)
{
    Routtextconn this = (Routtextconn)con->private;
    SEXP val;

    this->namesymbol = install(con->description);
    if(strcmp(mode, "w") == 0) {
	/* create variable pointed to by con->description */
	PROTECT(val = allocVector(STRSXP, 0));
	defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
	UNPROTECT(1);
    } else {
	/* take over existing variable */
	val = findVar1(this->namesymbol, VECTOR_ELT(OutTextData, idx),
		       STRSXP, FALSE);
	if(val == R_UnboundValue) {
	    warning("text connection: appending to a non-existent char vector");
	    PROTECT(val = allocVector(STRSXP, 0));
	    defineVar(this->namesymbol, val, VECTOR_ELT(OutTextData, idx));
	    UNPROTECT(1);
	}
    }
    this->len = LENGTH(val);
    this->data = val;
    this->lastline[0] = '\0';
    this->lastlinelength = LAST_LINE_LEN;
}


static Rconnection newouttext(char *description, SEXP sfile, char *mode,
			      int idx)
{
    Rconnection new;
    void *tmp;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of text connection failed");
    new->class = (char *) malloc(strlen("textConnection") + 1);
    if(!new->class) {
	free(new);
	error("allocation of text connection failed");
    }
    strcpy(new->class, "textConnection");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of text connection failed");
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
	error("allocation of text connection failed");
    }
    ((Routtextconn)new->private)->lastline = tmp = malloc(LAST_LINE_LEN);
    if(!tmp) {
	free(new->private);
	free(new->description); free(new->class); free(new);
	error("allocation of text connection failed");
    }
    outtext_init(new, mode, idx);
    return new;
}

SEXP do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, stext, sopen, ans, class, venv;
    char *desc, *open;
    int ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) != 1)
	error("invalid `description' argument");
    desc = CHAR(STRING_ELT(sfile, 0));
    stext = CADR(args);
    if(!isString(stext))
	error("invalid `text' argument");
    sopen = CADDR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    venv = CADDDR(args);
    if (!isEnvironment(venv) && venv != R_NilValue)
	error("invalid 'environment' argument");
    ncon = NextConnection();
    if(!strlen(open) || strncmp(open, "r", 1) == 0)
	con = Connections[ncon] = newtext(desc, stext);
    else if (strncmp(open, "w", 1) == 0 || strncmp(open, "a", 1) == 0) {
	if (OutTextData == NULL) {
	    OutTextData = allocVector(VECSXP, NCONNECTIONS);
	    R_PreserveObject(OutTextData);
	}
	SET_VECTOR_ELT(OutTextData, ncon, venv);
	con = Connections[ncon] =
	    newouttext(CHAR(STRING_ELT(stext, 0)), sfile, open, ncon);
    }
    else
	errorcall(call, "unsupported mode");
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

/* ------------------- socket connections  --------------------- */


/* socketConnection(host, port, server, blocking, open, encoding) */
SEXP do_sockconn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    char *host, *open;
    int i, ncon, port, server, blocking;
    Rconnection con = NULL;

    checkArity(op, args);
#ifdef HAVE_SOCKETS
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) != 1)
	error("invalid `host' argument");
    host = CHAR(STRING_ELT(scmd, 0));
    args = CDR(args);
    port = asInteger(CAR(args));
    if(port == NA_INTEGER || port < 0)
	error("invalid `port' argument");
    args = CDR(args);
    server = asLogical(CAR(args));
    if(server == NA_LOGICAL)
	error("invalid `server' argument");
    args = CDR(args);
    blocking = asLogical(CAR(args));
    if(blocking == NA_LOGICAL)
	error("invalid `blocking' argument");
    args = CDR(args);
    sopen = CAR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    args = CDR(args);
    enc = CAR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");

    ncon = NextConnection();
    con = R_newsock(host, port, server, open);
    Connections[ncon] = con;
    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];
    con->blocking = blocking;

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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
    error("sockets are not available on this system");
#endif
    return ans;
}

/* ------------------- unz connections  --------------------- */

/* see dounzip.c for the details */
SEXP do_unz(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class, enc;
    char *file, *open;
    int i, ncon;
    Rconnection con = NULL;

    checkArity(op, args);
    sfile = CAR(args);
    if(!isString(sfile) || length(sfile) < 1)
	errorcall(call, "invalid `description' argument");
    if(length(sfile) > 1)
	warning("only first element of `description' argument used");
    file = CHAR(STRING_ELT(sfile, 0));
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    enc = CADDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    ncon = NextConnection();
    con = Connections[ncon] = R_newunz(file, strlen(open) ? open : "r");

    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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

SEXP do_open(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, block;
    Rconnection con=NULL;
    SEXP sopen;
    char *open;
    Rboolean success;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    i = asInteger(CAR(args));
    con = getConnection(i);
    if(i < 3) error("cannot open standard connections");
    if(con->isopen) {
	warning("connection is already open");
	return R_NilValue;
    }
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error("invalid `blocking' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    if(strlen(open) > 0) strcpy(con->mode, open);
    con->blocking = block;
    success = con->open(con);
    if(!success) {
	/* con_close(i); user might have a reference */
	error("unable to open connection");
    }
    return R_NilValue;
}

SEXP do_isopen(SEXP call, SEXP op, SEXP args, SEXP env)
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
    }
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = res;
    UNPROTECT(1);
    return ans;
}

SEXP do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    con = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = con->incomplete != FALSE;
    UNPROTECT(1);
    return ans;
}

SEXP do_isseekable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
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


SEXP do_close(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, j;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    i = asInteger(CAR(args));
    if(i < 3) error("cannot close standard connections");
    for(j = 0; j < R_SinkNumber; j++)
	if(i == SinkCons[j])
	    error("cannot close output sink connection");
    if(i == R_ErrorCon)
	error("cannot close messages sink connection");
    con_close(i);
    return R_NilValue;
}

/* seek(con, where = numeric(), origin = "start", rw = "") */
SEXP do_seek(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int where, origin, rw;
    SEXP ans;
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    con = getConnection(asInteger(CAR(args)));
    if(!con->isopen) error("connection is not open");
    where = asInteger(CADR(args));
    origin = asInteger(CADDR(args));
    rw = asInteger(CADDDR(args));
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = con->seek(con, where, origin, rw);
    UNPROTECT(1);
    return ans;
}

/* truncate(con) */
SEXP do_truncate(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    con = getConnection(asInteger(CAR(args)));
    con->truncate(con);
    return R_NilValue;
}

SEXP do_flush(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con = NULL;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
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
	if(nbuf+1 >= bufsize) error("Line longer than buffer size");
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
	if(nbuf+1 >= bufsize) error("Line longer than buffer size");
	buf[++nbuf] = '\0';
    }
    return(nbuf);
}


int Rconn_printf(Rconnection con, const char *format, ...)
{
    int res;
    va_list(ap);

    va_start(ap, format);
    res = con->vfprintf(con, format, ap);
    va_end(ap);
    return res;
}


/* readLines(con = stdin(), n = 1, ok = TRUE) */
#define BUF_SIZE 1000
SEXP do_readLines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, ans2;
    int i, n, nn, nnn, ok, nread, c, nbuf, buf_size = BUF_SIZE;
    Rconnection con = NULL;
    Rboolean wasopen;
    char *buf;

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    con = getConnection(asInteger(CAR(args)));
    n = asInteger(CADR(args));
    if(n == NA_INTEGER)
	errorcall(call, "invalid value for `n'");
    ok = asLogical(CADDR(args));
    if(ok == NA_LOGICAL)
	errorcall(call,"invalid value for `ok'");
    if(!con->canread)
	errorcall(call, "cannot read from this connection");
    wasopen = con->isopen;
    if(!wasopen) {
	if(!con->open(con)) error("cannot open the connection");
    } else { /* for a non-blocking connection, more input may
		have become available, so re-position */
	if(con->canseek && !con->blocking)
	    con->seek(con, con->seek(con, -1, 1, 1), 1, 1);
    }
    con->incomplete = FALSE;

    buf = (char *) malloc(buf_size);
    if(!buf)
	error("cannot allocate buffer in readLines");
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
		    error("cannot allocate buffer in readLines");
	    }
	    if(c != '\n') buf[nbuf++] = c; else break;
	}
	buf[nbuf] = '\0';
	SET_STRING_ELT(ans, nread, mkChar(buf));
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
	    warning("incomplete final line found by readLines on `%s'",
		    con->description);
	} else {
	    /* push back the rest */
	    pushback(con, 0, buf);
	    con->incomplete = TRUE;
	}
    }
    free(buf);
    if(nread < nnn && !ok)
	error("too few lines read in readLines");
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    return ans2;
}

static void writecon(Rconnection con, char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    con->vfprintf(con, format, ap);
    va_end(ap);
}

/* writelines(text, con = stdout(), sep = "\n") */
SEXP do_writelines(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    Rboolean wasopen;
    Rconnection con=NULL;
    SEXP text, sep;

    checkArity(op, args);
    text = CAR(args);
    if(!isString(text)) error("invalid `text' argument");
    if(!inherits(CADR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    con = getConnection(asInteger(CADR(args)));
    sep = CADDR(args);
    if(!isString(sep)) error("invalid `sep' argument");
    if(!con->canwrite)
	error("cannot write to this connection");
    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error("cannot open the connection");
    for(i = 0; i < length(text); i++)
	writecon(con, "%s%s", CHAR(STRING_ELT(text, i)),
		 CHAR(STRING_ELT(sep, 0)));
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
		warning("incomplete string at end of file has been discarded");
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
	warning("null terminator not found: breaking string at 10000 chars");
    return mkChar(buf);
}

/* readBin(con, what, n, swap) */
SEXP do_readbin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, swhat;
    int i, size, signd, swap, n, m = 0, sizedef= 4, mode = 1;
    char *what;
    void *p = NULL;
    Rboolean wasopen;
    Rconnection con = NULL;
    char *vmax = vmaxget();

    checkArity(op, args);
    i = asInteger(CAR(args)); args = CDR(args);
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    if(con->text) error("can only read from a binary connection");
    swhat = CAR(args); args = CDR(args);
    if(!isString(swhat) || length(swhat) != 1)
	error("invalid value of `what'");
    what = CHAR(STRING_ELT(swhat, 0));
    n = asInteger(CAR(args)); args = CDR(args);
    if(n == NA_INTEGER || n < 0) error("invalid value of `n'");
    size = asInteger(CAR(args)); args = CDR(args);
    signd = asLogical(CAR(args)); args = CDR(args);
    if(signd == NA_LOGICAL)
	error("invalid value of `signed'");
    swap = asLogical(CAR(args));
    if(swap == NA_LOGICAL)
	error("invalid value of `swap'");
    if(!con->canread)
	error("cannot read from this connection");

    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error("cannot open the connection");

    if(!strcmp(what, "character")) {
	SEXP onechar;
	PROTECT(ans = allocVector(STRSXP, n));
	for(i = 0, m = i+1; i < n; i++) {
	    onechar = readOneString(con);
	    if(onechar != R_NilValue) {
		SET_STRING_ELT(ans, i, onechar);
		m++;
	    } else break;
	}
    } else if(!strcmp(what, "complex")) {
	if(size == NA_INTEGER) size = sizeof(Rcomplex);
	if(size != sizeof(Rcomplex))
	    error("size changing is not supported for complex vectors");
	PROTECT(ans = allocVector(CPLXSXP, n));
	p = (void *) COMPLEX(ans);
	m = con->read(p, size, n, con);
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
	    case sizeof(long long):
#endif
		break;
	    default:
		error("That size is unknown on this machine");
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
	    case sizeof(long long):
#endif
		break;
	    default:
		error("That size is unknown on this machine");
	    }
	    PROTECT(ans = allocVector(LGLSXP, n));
	    p = (void *) LOGICAL(ans);
	} else if (!strcmp(what, "numeric") || !strcmp(what, "double")) {
	    sizedef = sizeof(double); mode = 2;
	    if(size == NA_INTEGER) size = sizedef;
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if SIZEOF_LONG_DOUBLE > 8
	    case sizeof(long double):
#endif
		break;
	    default:
		error("That size is unknown on this machine");
	    }
	    PROTECT(ans = allocVector(REALSXP, n));
	    p = (void *) REAL(ans);
	}
	if(size == sizedef) {
	    m = con->read(p, size, n, con);
	    if(swap && size > 1)
		for(i = 0; i < m; i++) swapb((char *)p+i*size, size);
	} else {
	    char * buf = R_alloc(1, size);
	    int s;
	    if(mode == 1) {
		for(i = 0, m = 0; i < n; i++) {
		    s = con->read(buf, size, 1, con);
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
		    case sizeof(long long):
			INTEGER(ans)[i] = (int)*((long long *)buf);
			break;
#endif
		    }
		}
	    } else if (mode == 2) {
		for(i = 0, m = 0; i < n; i++) {
		    s = con->read(buf, size, 1, con);
		    if(s) m++; else break;
		    if(swap && size > 1) swapb(buf, size);
		    switch(size) {
		    case sizeof(float):
			REAL(ans)[i] = (double)*((float *)buf);
			break;
#if SIZEOF_LONG_DOUBLE > 8
		    case sizeof(long double):
			REAL(ans)[i] = (double)*((long double *)buf);
			break;
#endif
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
SEXP do_writebin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object;
    int i, j, size, swap, len, n=0;
    char *s, *buf;
    Rboolean wasopen;
    Rconnection con = NULL;

    checkArity(op, args);
    object = CAR(args);
    if(!isVectorAtomic(object))
	error("`x' is not an atomic vector type");
    i = asInteger(CADR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    if(con->text) error("can only write to a binary connection");
    size = asInteger(CADDR(args));
    swap = asLogical(CADDDR(args));
    if(swap == NA_LOGICAL)
	error("invalid value of `swap'");
    if(!con->canwrite)
	error("cannot write to this connection");
    len = LENGTH(object);
    if(len == 0) return R_NilValue;

    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error("cannot open the connection");

    if(TYPEOF(object) == STRSXP) {
	for(i = 0; i < len; i++) {
	    s = CHAR(STRING_ELT(object, i));
	    n = con->write(s, sizeof(char), strlen(s) + 1, con);
	    if(!n) {
		warning("problem writing to connection");
		break;
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
	    case sizeof(long long):
#endif
		break;
	    default:
		error("That size is unknown on this machine");
	    }
	    break;
	case REALSXP:
	    if(size == NA_INTEGER) size = sizeof(double);
	    switch (size) {
	    case sizeof(double):
	    case sizeof(float):
#if SIZEOF_LONG_DOUBLE > 8
	    case sizeof(long double):
#endif
		break;
	    default:
		error("That size is unknown on this machine");
	    }
	    break;
	case CPLXSXP:
	    if(size == NA_INTEGER) size = sizeof(Rcomplex);
	    if(size != sizeof(Rcomplex))
		error("size changing is not supported for complex vectors");
	    break;
	default:
	    error("That type is unimplemented");
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
	    case sizeof(long long):
	    {
		long long ll1;
		for (i = 0, j = 0; i < len; i++, j += size) {
		    ll1 = (long long) INTEGER(object)[i];
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
#if SIZEOF_LONG_DOUBLE > 8
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
	    }
	    break;
	case CPLXSXP:
	    memcpy(buf, COMPLEX(object), size * len);
	    break;
	}

	if(swap && size > 1)
	    for(i = 0; i < len; i++) swapb(buf+size*i, size);

	/* write it now */
	n = con->write(buf, size, len, con);
	if(n < len) warning("problem writing to connection");
	Free(buf);
    }

    if(!wasopen) con->close(con);
    return R_NilValue;
}

static SEXP readFixedString(Rconnection con, int len)
{
    char *buf, *p;
    int  pos, m;

    buf = (char *) R_alloc(len+1, sizeof(char));
    for(pos = 0; pos < len; pos++) {
	p = buf + pos;
	m = con->read(p, sizeof(char), 1, con);
	if(!m) {
	    if(pos == 0) return R_NilValue; else break;
	}
    }
    buf[pos] = '\0';
    return mkChar(buf);
}


/* readChar(con, nchars) */
SEXP do_readchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, onechar, nchars;
    int i, len, n, m = 0;
    Rboolean wasopen;
    Rconnection con = NULL;
    char *vmax = vmaxget();

    checkArity(op, args);
    i = asInteger(CAR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    if(!con->canread)
	error("cannot read from this connection");
    nchars = CADR(args);
    n = LENGTH(nchars);
    if(n == 0) return allocVector(STRSXP, 0);

    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error("cannot open the connection");

    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0, m = i+1; i < n; i++) {
	len = INTEGER(nchars)[i];
	if(len == NA_INTEGER || len < 0)
	    error("supplied length is invalid");
	onechar = readFixedString(con, len);
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
SEXP do_writechar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP object, nchars, sep;
    int i, len, n, nwrite=0, slen, tlen;
    char *s, *buf, *ssep = "";
    Rboolean wasopen, usesep;
    Rconnection con = NULL;
    char *vmax = vmaxget();

    checkArity(op, args);
    object = CAR(args);
    i = asInteger(CADR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    if(!con->canwrite)
	error("cannot write to this connection");

    nchars = CADDR(args);
    sep = CADDDR(args);
    if(isNull(sep)) {
	usesep = FALSE;
	slen = 0;
    } else {
	usesep = TRUE;
	if (!isString(sep) || length(sep) != 1)
	    error("invalid value of `sep'");
	ssep = CHAR(STRING_ELT(sep, 0));
	slen = strlen(ssep) + 1;
    }
    n = LENGTH(nchars);
    if(n == 0) return R_NilValue;

    len = 0;
    for(i = 0; i < n; i++) {
	tlen = strlen(CHAR(STRING_ELT(object, i)));
	if (tlen > len) len = tlen;
    }
    buf = (char *) R_alloc(len + slen, sizeof(char));

    wasopen = con->isopen;
    if(!wasopen)
	if(!con->open(con)) error("cannot open the connection");

    if(TYPEOF(object) == STRSXP) {
	for(i = 0; i < n; i++) {
	    len = INTEGER(nchars)[i];
	    s = CHAR(STRING_ELT(object, i));
	    memset(buf, '\0', len + slen);
	    strncpy(buf, s, len);
	    if (usesep) {
		strcat(buf, ssep);
		len += slen;
	    }
	    nwrite = con->write(buf, sizeof(char), len, con);
	    if(!nwrite) {
		warning("problem writing to connection");
		break;
	    }
	}
    }
    vmaxset(vmax);
    if(!wasopen) con->close(con);
    return R_NilValue;
}

/* ------------------- push back text  --------------------- */


static void
pushback(Rconnection con, int newLine, char *line)
{
    int nexists = con->nPushBack;
    char **q;

    if(nexists > 0) {
	q = con->PushBack =
	    (char **) realloc(con->PushBack, (nexists+1)*sizeof(char *));
    } else {
	q = con->PushBack = (char **) malloc(sizeof(char *));
    }
    if(!q) error("could not allocate space for pushBack");
    q += nexists;
    *q = (char *) malloc(strlen(line) + 1 + newLine);
    if(!(*q)) error("could not allocate space for pushBack");
    strcpy(*q, line);
    if(newLine) strcat(*q, "\n");
    q++;
    con->posPushBack = 0;
    con->nPushBack++;
}


SEXP do_pushback(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, n, nexists, newLine;
    Rconnection con = NULL;
    SEXP stext;
    char *p, **q;

    checkArity(op, args);

    stext = CAR(args);
    if(!isString(stext))
	error("invalid `data' argument");
    i = asInteger(CADR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    newLine = asLogical(CADDR(args));
    if(newLine == NA_LOGICAL)
	error("invalid `newLine' argument");
    if(!con->canread && !con->isopen)
	error("can only push back on open readable connections");
    if(!con->text)
	error("can only push back on text-mode connections");
    nexists = con->nPushBack;
    if((n = length(stext)) > 0) {
	if(nexists > 0) {
	    q = con->PushBack =
		(char **) realloc(con->PushBack, (n+nexists)*sizeof(char *));
	} else {
	    q = con->PushBack = (char **) malloc(n*sizeof(char *));
	}
	if(!q) error("could not allocate space for pushBack");
	q += nexists;
	for(i = 0; i < n; i++) {
	    p = CHAR(STRING_ELT(stext, n - i - 1));
	    *q = (char *) malloc(strlen(p) + 1 + newLine);
	    if(!(*q)) error("could not allocate space for pushBack");
	    strcpy(*q, p);
	    if(newLine) strcat(*q, "\n");
	    q++;
	}
	con->posPushBack = 0;
	con->nPushBack += n;
    }
    return R_NilValue;
}

SEXP do_pushbacklength(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;
    Rconnection con = NULL;
    SEXP ans;

    i = asInteger(CAR(args));
    if(i == NA_INTEGER || !(con = Connections[i]))
	error("invalid connection");
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = con->nPushBack;
    UNPROTECT(1);
    return ans;
}


/* ------------------- sink functions  --------------------- */

/* Switch output to connection number icon, or popd stack if icon < 0
 */
Rboolean switch_stdout(int icon, int closeOnExit)
{
    int toclose;

    if(icon == R_OutputCon) return FALSE;

    if(icon >= 0 && R_SinkNumber >= NSINKS - 1)
	error("sink stack is full");

    if(icon == 0)
	error("cannot switch output to stdin");
    else if(icon == 1 || icon == 2) {
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	SinkConsClose[R_SinkNumber] = 0;
    } else if(icon >= 3) {
	Rconnection con = getConnection(icon); /* checks validity */
	toclose = 2*closeOnExit;
	if(!con->isopen) {
	    if(!con->open(con)) error("cannot open the connection");
	    toclose = 1;
	}
	R_OutputCon = SinkCons[++R_SinkNumber] = icon;
	SinkConsClose[R_SinkNumber] = toclose;
    } else { /* removing a sink */
	if (R_SinkNumber <= 0) {
	    warning("no sink to remove");
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

SEXP do_sink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int icon, closeOnExit, errcon;

    checkArity(op, args);
    icon = asInteger(CAR(args));
    closeOnExit = asLogical(CADR(args));
    if(closeOnExit == NA_LOGICAL)
	error("invalid value for closeOnExit");
    errcon = asLogical(CADDR(args));
    if(errcon == NA_LOGICAL) error("invalid value for type");

    if(!errcon) {
	/* allow space for cat() to use sink() */
	if(icon >= 0 && R_SinkNumber >= NSINKS - 2)
	    error("sink stack is full");
	switch_stdout(icon, closeOnExit);
    } else {
	if(icon < 0) R_ErrorCon = 2;
	else {
	    getConnection(icon); /* check validity */
	    R_ErrorCon = icon;
	}
    }

    return R_NilValue;
}

SEXP do_sinknumber(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int errcon;
    checkArity(op, args);

    errcon = asLogical(CAR(args));
    if(errcon == NA_LOGICAL)
	error("invalid value for type");
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = errcon ? R_SinkNumber : R_ErrorCon;
    UNPROTECT(1);
    return ans;
}


/* ------------------- admin functions  --------------------- */

void InitConnections()
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

SEXP do_getallconnections(SEXP call, SEXP op, SEXP args, SEXP env)
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

SEXP do_sumconnection(SEXP call, SEXP op, SEXP args, SEXP env)
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
SEXP do_url(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP scmd, sopen, ans, class, enc;
    char *url, *open, *class2 = "url";
    int i, ncon, block;
    Rconnection con = NULL;
#ifdef HAVE_INTERNET
    UrlScheme type = HTTPsh;	/* -Wall */
#endif

    checkArity(op, args);
    scmd = CAR(args);
    if(!isString(scmd) || length(scmd) < 1)
	error("invalid `description' argument");
    if(length(scmd) > 1)
	warning("only first element of `description' argument used");
    url = CHAR(STRING_ELT(scmd, 0));
#ifdef HAVE_INTERNET
    if (strncmp(url, "http://", 7) == 0) {
	type = HTTPsh;
    } else if (strncmp(url, "ftp://", 6) == 0) {
	type = FTPsh;
    }
#endif

    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error("invalid `block' argument");
    enc = CADDDR(args);
    if(!isInteger(enc) || length(enc) != 256)
	error("invalid `enc' argument");

    ncon = NextConnection();
    if(strncmp(url, "file://", 7) == 0) {
       con = newfile(url + 7, strlen(open) ? open : "r");
       class2 = "file";
#ifdef HAVE_INTERNET
    } else if (strncmp(url, "http://", 7) == 0 ||
	       strncmp(url, "ftp://", 6) == 0) {
       con = R_newurl(url, strlen(open) ? open : "r");
       ((Rurlconn)con->private)->type = type;
#endif
    } else {
	if(PRIMVAL(op)) { /* call to file() */
	    if(strlen(url) == 0) open ="w+";
#ifdef Win32
	    if(strcmp(url, "clipboard") == 0)
		con = newclp(strlen(open) ? open : "r");
	    else
#endif
		con = newfile(url, strlen(open) ? open : "r");
	    class2 = "file";
	} else {
	    error("unsupported URL scheme");
	}
    }

    Connections[ncon] = con;
    for(i = 0; i < 256; i++)
	con->encoding[i] = (unsigned char) INTEGER(enc)[i];
    con->blocking = block;

    /* open it if desired */
    if(strlen(open)) {
	Rboolean success = con->open(con);
	if(!success) {
	    con_close(ncon);
	    error("unable to open connection");
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
    if(!con->isopen) error("connection is not open");
    if(!con->canwrite) error("cannot write to this connection");

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
		warning("file stream does not have gzip magic number");
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
	    warning("file stream does not have valid gzip header");
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
		    error("writing error whilst flushing gzcon connection");
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
	putLong(icon, priv->s.total_in);
    } else err = inflateEnd(&(priv->s));
    if(priv->inbuf) {free(priv->inbuf); priv->inbuf = Z_NULL;}
    if(priv->outbuf) {free(priv->outbuf); priv->outbuf = Z_NULL;}
    if(icon->isopen) icon->close(icon);
    con->isopen = FALSE;
}

static size_t gzcon_read(void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    Rgzconn priv = (Rgzconn)con->private;
    Rconnection icon = priv->con;
    Bytef *start = (Bytef*)ptr, buf[4];
    uLong crc;
    int n;

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
	    /* CRC is little-endian on file */
	    icon->read(&buf, 1, sizeof(uLong), icon);
	    crc = 0;
	    for (n = 0; n < 4; n++) {crc <<= 8; crc += buf[n];}
	    if (crc != priv->crc) priv->z_err = Z_DATA_ERROR;
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
		warning("write error on gzcon connection");
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
    return (n == 1) ? con->encoding[c] : R_EOF;
}


/* gzcon(con, level) */
SEXP do_gzcon(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, class;
    int i, icon, level, allow;
    Rconnection incon=NULL, new=NULL;
    char *m, *mode = NULL /* -Wall */,  description[1000];

    checkArity(op, args);
    if(!inherits(CAR(args), "connection"))
	errorcall(call, "`con' is not a connection");
    incon = getConnection(icon = asInteger(CAR(args)));
    level = asInteger(CADR(args));
    if(level == NA_INTEGER || level < 0 || level > 9)
	errorcall(call, "`level' must be one of 0 ... 9");
    allow = asLogical(CADDR(args));
    if(allow == NA_INTEGER)
	errorcall(call, "`allowNonCompression' must be TRUE or FALSE");


    /* if(incon->text)
       error("gzcon can only work with binary connections");*/
    m = incon->mode;
    if(strcmp(m, "r") == 0 || strcmp(m, "rb") == 0) mode = "rb";
    else if (strcmp(m, "w") == 0 || strcmp(m, "wb") == 0) mode = "wb";
    else errorcall(call, "can only use read- or write- binary connections");


    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error("allocation of gzcon connection failed");
    new->class = (char *) malloc(strlen("gzcon") + 1);
    if(!new->class) {
	free(new);
	error("allocation of gzcon connection failed");
    }
    strcpy(new->class, "gzcon");
    sprintf(description, "gzcon(%s)", incon->description);
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error("allocation of gzcon connection failed");
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
	error("allocation of gzcon connection failed");
    }
    ((Rgzconn)(new->private))->con = incon;
    ((Rgzconn)(new->private))->cp = level;
    ((Rgzconn)(new->private))->nsaved = -1;
    ((Rgzconn)(new->private))->allow = allow;

    Connections[icon] = new;
    for(i = 0; i < 256; i++)
	new->encoding[i] = incon->encoding[i];
    if(incon->isopen) new->open(new);

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = icon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("gzcon"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}


/* Experimental code for in-memory (de)compression
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

SEXP R_compress1(SEXP in)
{
    uLong inlen, outlen;
    int res;
    Bytef *buf;
    SEXP ans;

    if(!isString(in) || length(in) !=1)
	error("requires a scalar string");
    inlen = LENGTH(STRING_ELT(in, 0));
    outlen = 1.001*inlen + 20;
    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    /* we want this to be system-independent */
    *((unsigned int *)buf) = (unsigned int) uiSwap(inlen);
    res = compress(buf + 4, &outlen, (Bytef *)CHAR(STRING_ELT(in, 0)), inlen);
    if(res != Z_OK) error("internal error in compress1");
    ans = allocVector(CHARSXP, outlen + 4);
    memcpy(CHAR(ans), buf, outlen + 4);
    return ScalarString(ans);
}

SEXP R_decompress1(SEXP in)
{
    uLong inlen, outlen;
    int res;
    Bytef *buf;
    char *p = CHAR(STRING_ELT(in, 0));
    SEXP ans;

    if(!isString(in) || length(in) !=1)
	error("requires a scalar string");
    inlen = LENGTH(STRING_ELT(in, 0));
    outlen = (uLong) uiSwap(*((unsigned int *) p));
    buf = (Bytef *) R_alloc(outlen, sizeof(Bytef));
    res = uncompress(buf, &outlen, (Bytef *)(p + 4), inlen - 4);
    if(res != Z_OK) error("internal error in decompress1");
    ans = allocVector(CHARSXP, outlen);
    memcpy(CHAR(ans), buf, outlen);
    return ScalarString(ans);
}

SEXP do_sockselect(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nsock, i;
    SEXP insock, write, val, insockfd;
    double timeout;

    checkArity(op, args);

    insock = CAR(args);
    if (TYPEOF(insock) != VECSXP || LENGTH(insock) == 0)
	errorcall(call, "not a list of sockets");
    nsock = LENGTH(insock);

    write = CADR(args);
    if (TYPEOF(write) != LGLSXP || LENGTH(write) != nsock)
	errorcall(call, "bad write indicators");

    timeout = asReal(CADDR(args));
    
    PROTECT(insockfd = allocVector(INTSXP, nsock));
    PROTECT(val = allocVector(LGLSXP, nsock));

    for (i = 0; i < nsock; i++) {
	Rconnection conn = getConnection(asInteger(VECTOR_ELT(insock, i)));
	if (strcmp(conn->class, "socket") != 0)
	    errorcall(call, "not a socket connection");
	INTEGER(insockfd)[i] = ((Rsockconn) conn->private)->fd;
    }

    Rsockselect(nsock, INTEGER(insockfd), LOGICAL(val), LOGICAL(write),
		timeout);
    
    UNPROTECT(2);
    return val;
}
