/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000,2001   The R Development Core Team.
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
#include <config.h>
#endif

#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>
#include <R_ext/Complex.h>
/* #include <fcntl.h> not yet */

/* Win32 does have popen, but it does not work in GUI applications,
   so test that later */
#ifdef Win32
# define HAVE_POPEN
# include <Startup.h>
  extern UImode  CharacterMode;
#endif

#define NCONNECTIONS 50

static Rconnection Connections[NCONNECTIONS];

/* ------------- admin functions (see also at end) ----------------- */

int NextConnection()
{
    int i;
    for(i = 3; i < NCONNECTIONS; i++)
	if(!Connections[i]) break;
    if(i > NCONNECTIONS)
	error("All connections are in use");
    return i;
}

/* internal, not the same as R function getConnection */
Rconnection getConnection(int n)
{
    Rconnection con = NULL;

    if(n < 0 || n == NA_INTEGER || !(con = Connections[n]))
	error("invalid connection");
    return con;

}

/* ------------------- null connection functions --------------------- */

static void null_open(Rconnection con)
{
    error("open/close not enabled for this connection");
}

static int null_vfprintf(Rconnection con, const char *format, va_list ap)
{
    error("printing not enabled for this connection");
    return 0; /* -Wall */
}

static int null_fgetc(Rconnection con)
{
    error("getc not enabled for this connection");
    return 0; /* -Wall */
}

static int null_ungetc(int c, Rconnection con)
{
    error("ungetc not enabled for this connection");
    return 0; /* -Wall */
}

static long null_seek(Rconnection con, int where)
{
    error("seek not enabled for this connection");
    return 0; /* -Wall */
}

static int null_fflush(Rconnection con)
{
    return 0;
}

static size_t null_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    error("read not enabled for this connection");
    return 0; /* -Wall */
}

static size_t null_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    error("write not enabled for this connection");
    return 0; /* -Wall */
}

/* ------------------- file connections --------------------- */

static void file_open(Rconnection con)
{
    FILE *fp;
/*    int fd, flags; */  /* fcntl does not exist on Windows */

    fp = R_fopen(R_ExpandFileName(con->description), con->mode);
    if(!fp) error("cannot open file `%s'",
		  R_ExpandFileName(con->description));
    ((Rfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w' || con->mode[0] == 'a');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;

/*    if(!con->blocking) {
	fd = fileno(fp);
	flags = fcntl(fd, F_GETFL);
	flags |= O_NONBLOCK;
	fcntl(fd, F_SETFL, flags);
	}*/
}

static void file_close(Rconnection con)
{
    fclose(((Rfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static void file_destroy(Rconnection con)
{
    free(con->private);
}

static int file_vfprintf(Rconnection con, const char *format, va_list ap)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
    return vfprintf(fp, format, ap);
}

static int file_fgetc(Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
    int c = fgetc(fp);
    return feof(fp) ? R_EOF : c;
}

static int file_ungetc(int c, Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
    return ungetc(c, fp);
}

static long file_seek(Rconnection con, int where)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
    long pos = ftell(fp);

    if(where >= 0) fseek(fp, where, SEEK_SET);
    return pos;
}

static int file_fflush(Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;

    return fflush(fp);
}

static size_t file_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
    return fread(ptr, size, nitems, fp);
}

static size_t file_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    FILE *fp = ((Rfileconn)(con->private))->fp;
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
    strcpy(new->description, description);
    strncpy(new->mode, mode, 4); new->mode[4] = '\0';
    new->isopen = new->incomplete = FALSE;
    new->canread = new->canwrite = TRUE; /* in principle */
    new->canseek = TRUE;
    new->text = TRUE;
    new->open = &file_open;
    new->close = &file_close;
    new->destroy = &file_destroy;
    new->vfprintf = &file_vfprintf;
    new->fgetc = &file_fgetc;
    new->ungetc = &file_ungetc;
    new->seek = &file_seek;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->nPushBack = 0;
    new->private = (void *) malloc(sizeof(struct fileconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of file connection failed");
    }
    return new;
}

SEXP do_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, sopen, ans, class;
    char *file, *open;
    int ncon, block;
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
    open = CHAR(STRING_ELT(sopen, 0));
    ncon = NextConnection();
    con = Connections[ncon] = newfile(file, strlen(open) ? open : "r");

    /* open it if desired */
    if(strlen(open)) con->open(con);

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = ncon;
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("file"));
    SET_STRING_ELT(class, 1, mkChar("connection"));
    classgets(ans, class);
    UNPROTECT(2);

    return ans;
}

/* ------------------- pipe connections --------------------- */

#ifdef HAVE_POPEN
static void pipe_open(Rconnection con)
{
    FILE *fp;

    fp = popen(con->description, con->mode);
    if(!fp) error("cannot open cmd `%s'", con->description);
    ((Rfileconn)(con->private))->fp = fp;
    con->isopen = TRUE;
    con->canwrite = (con->mode[0] == 'w');
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
}

static void pipe_close(Rconnection con)
{
    pclose(((Rfileconn)(con->private))->fp);
    con->isopen = FALSE;
}

static void pipe_destroy(Rconnection con)
{
    free(con->private);
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
    strcpy(new->description, description);
    strncpy(new->mode, mode, 4); new->mode[4] = '\0';
    new->isopen = new->incomplete = FALSE;
    new->canread = new->canwrite = TRUE; /* in principle */
    new->canseek = FALSE;
    new->text = TRUE;
    new->open = &pipe_open;
    new->close = &pipe_close;
    new->destroy = &pipe_destroy;
    new->vfprintf = &file_vfprintf;
    new->fgetc = &file_fgetc;
    new->ungetc = &file_ungetc;
    new->seek = &null_seek;
    new->fflush = &file_fflush;
    new->read = &file_read;
    new->write = &file_write;
    new->nPushBack = 0;
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
    SEXP scmd, sopen, ans, class;
    char *file, *open;
    int ncon;
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

    /* open it if desired */
    if(strlen(open)) con->open(con);

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
    return R_NilValue; /* -Wall */
#endif
}

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

static int save = 0;
static int stdin_fgetc(Rconnection con)
{
    if (save) {
	int c = save;
	save = 0;
	return c;
    } else return ConsoleGetchar();
}

static int stdin_ungetc(int c, Rconnection con)
{
    save = c;
    return c;
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
    strcpy(new->description, description);
    strncpy(new->mode, mode, 4); new->mode[4] = '\0';
    new->isopen = TRUE;
    new->incomplete = FALSE;
    new->text = TRUE;
    new->canread = (strcmp(mode, "r") == 0);
    new->canwrite = (strcmp(mode, "w") == 0);
    new->canseek = FALSE;
    new->open = &null_open;
    new->close = &null_open;
    new->destroy = &null_open;
    new->vfprintf = &null_vfprintf;
    new->fgetc = &null_fgetc;
    new->ungetc = &null_ungetc;
    new->seek = &null_seek;
    new->fflush = &null_fflush;
    new->read = &null_read;
    new->write = &null_write;
    new->nPushBack = 0;
    new->save = -1000;
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

/* Switch output to connection number icon.
   We don't close the old connection.
 */
void switch_stdout(int icon)
{
    if(icon == R_OutputCon) return;
    if(icon >= 3) {
	Rconnection con = getConnection(icon); /* checks validity */
	if(!con->isopen) {
	    con->open(con);
	    if(R_SinkCon_to_close == 0) R_SinkCon_to_close = 2;
	}
	R_OutputCon = icon;
    } else if(icon == 0)
	error("cannot switch output to stdin");
    else if(icon == 2)
	error("cannot switch output to stderr");
    else {
	R_OutputCon = 1;
    }
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

static void text_open(Rconnection con)
{
    con->save = -1000;
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

static int text_ungetc(int c, Rconnection con)
{
    Rtextconn this = (Rtextconn)con->private;
    this->save = c;
    return c;
}

static long text_seek(Rconnection con, int where)
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
    strcpy(new->description, description);
    strcpy(new->mode, "r");
    new->isopen = new->text = TRUE;
    new->incomplete = FALSE;
    new->canread = TRUE; new->canwrite = FALSE;
    new->canseek = FALSE;
    new->open = &text_open;
    new->close = &text_close;
    new->destroy = &text_destroy;
    new->vfprintf = &null_vfprintf;
    new->fgetc = &text_fgetc;
    new->ungetc = &text_ungetc;
    new->seek = &text_seek;
    new->fflush = &null_fflush;
    new->read = &null_read;
    new->write = &null_write;
    new->nPushBack = 0;
    new->private = (void*) malloc(sizeof(struct textconn));
    new->save = -1000;
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

    if(strlen(this->lastline) > 0) {
	PROTECT(tmp = lengthgets(this->data, ++this->len));
	SET_STRING_ELT(tmp, this->len - 1, mkChar(this->lastline));
	defineVar(this->namesymbol, tmp, R_GlobalEnv);
	this->data = tmp;
	UNPROTECT(1);
    }
}

static void outtext_destroy(Rconnection con)
{
}

#define BUFSIZE 1000
static int text_vfprintf(Rconnection con, const char *format, va_list ap)
{
    Routtextconn this = (Routtextconn)con->private;
    char buf[BUFSIZE], *b = buf, *p, *q, *vmax = vmaxget();
    int res = 0, usedRalloc = FALSE, buffree,
	already = strlen(this->lastline);
    SEXP tmp;

    strcpy(b, this->lastline);
    p = b + already;
    buffree = BUFSIZE - already;

#ifdef HAVE_VSNPRINTF
    res = vsnprintf(p, buffree, format, ap);
    if(res >= buffree) { /* res is the desired output length */
	usedRalloc = TRUE;
	b = R_alloc(res + already + 1, sizeof(char));
	strcpy(b, this->lastline);
	p = b + already;
	vsprintf(p, format, ap);
    } else if(res < 0) { /* just a failure indication */
	usedRalloc = TRUE;
	b = R_alloc(10*BUFSIZE, sizeof(char));
	strcpy(b, this->lastline);
	p = b + already;
	res = vsnprintf(p, 10*BUFSIZE - already, format, ap);
	if (res < 0) {
	    *(b + 10*BUFSIZE) = '\0';
	    warning("printing of extremely long output is truncated");
	}
    }
#else
    /* allocate a large buffer and hope */
    b = R_alloc(10*BUFSIZE, sizeof(char));
    strcpy(b, this->lastline);
    p = b + already;
    res = vsprintf(p, format, ap);
#endif

    /* copy buf line-by-line to object */
    for(p = buf; ; p = q+1) {
	q = strchr(p, '\n');
	if(q) {
	    *q = '\0';
	    PROTECT(tmp = lengthgets(this->data, ++this->len));
	    SET_STRING_ELT(tmp, this->len - 1, mkChar(p));
	    defineVar(this->namesymbol, tmp, R_GlobalEnv);
	    this->data = tmp;
	    UNPROTECT(1);
	} else {
	    /* retain the last line */
	    if(strlen(this->lastline) < LAST_LINE_LEN) {
		strcpy(this->lastline, p);
	    } else {
		strncpy(this->lastline, p, LAST_LINE_LEN - 1);
		this->lastline[LAST_LINE_LEN - 1] = '\0';
		warning("line truncated in output text connection");
	    }
	    con->incomplete = strlen(this->lastline) > 0;
	    break;
	}
    }
    if(usedRalloc) vmaxset(vmax);
    return res;
}

static void outtext_init(Rconnection con, char *mode)
{
    Routtextconn this = (Routtextconn)con->private;
    SEXP val;

    this->namesymbol = install(con->description);
    if(strcmp(mode, "w") == 0) {
	/* create variable pointed to by con->description */
	PROTECT(val = allocVector(STRSXP, 0));
	defineVar(this->namesymbol, val, R_GlobalEnv);
	UNPROTECT(1);
    } else {
	/* take over existing variable */
	val = findVar1(this->namesymbol, R_GlobalEnv, STRSXP, FALSE);
	if(val == R_UnboundValue) {
	    warning("text connection: appending to a non-existent char vector");
	    PROTECT(val = allocVector(STRSXP, 0));
	    defineVar(this->namesymbol, val, R_GlobalEnv);
	    UNPROTECT(1);
	}
    }
    this->len = LENGTH(val);
    this->data = val;
    this->lastline[0] = '\0';
}


static Rconnection newouttext(char *description, SEXP sfile, char *mode)
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
    strcpy(new->description, description);
    strcpy(new->mode, mode); /* must be "w" or "a" at this point */
    new->isopen = new->text = TRUE;
    new->incomplete = FALSE;
    new->canread = FALSE; new->canwrite = TRUE;
    new->canseek = FALSE;
    new->open = &text_open;
    new->close = &outtext_close;
    new->destroy = &outtext_destroy;
    new->vfprintf = &text_vfprintf;
    new->fgetc = &null_fgetc;
    new->ungetc = &null_ungetc;
    new->seek = &text_seek;
    new->fflush = &null_fflush;
    new->read = &null_read;
    new->write = &null_write;
    new->nPushBack = 0;
    new->private = (void*) malloc(sizeof(struct outtextconn));
    new->save = -1000;
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error("allocation of text connection failed");
    }
    outtext_init(new, mode);
    return new;
}

SEXP do_textconnection(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP sfile, stext, sopen, ans, class;
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
    ncon = NextConnection();
    if(!strlen(open) || strncmp(open, "r", 1) == 0)
	con = Connections[ncon] = newtext(desc, stext);
    else if (strncmp(open, "w", 1) == 0 || strncmp(open, "a", 1) == 0)
	con = Connections[ncon] =
	    newouttext(CHAR(STRING_ELT(stext, 0)), sfile, open);
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

/* ------------------- open, close, seek --------------------- */

SEXP do_open(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i, block;
    Rconnection con=NULL;
    SEXP sopen;
    char *open;

    checkArity(op, args);
    i = asInteger(CAR(args));
    con = getConnection(i);
    if(i < 3) error("cannot open standard connections");
    sopen = CADR(args);
    if(!isString(sopen) || length(sopen) != 1)
	error("invalid `open' argument");
    block = asLogical(CADDR(args));
    if(block == NA_LOGICAL)
	error("invalid `block' argument");
    open = CHAR(STRING_ELT(sopen, 0));
    if(strlen(open) > 0) strcpy(con->mode, open);
    con->blocking = block;
    con->open(con);
    return R_NilValue;
}

SEXP do_isopen(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = con->isopen != FALSE;
    UNPROTECT(1);
    return ans;
}

SEXP do_isincomplete(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rconnection con;
    SEXP ans;

    checkArity(op, args);
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
    con = getConnection(asInteger(CAR(args)));
    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = con->canseek != FALSE;
    UNPROTECT(1);
    return ans;
}

void con_close(int i)
{
    int j;
    Rconnection con=NULL;

    con = getConnection(i);
    if(con->isopen) con->close(con);
    con->destroy(con);
    free(con->class);
    free(con->description);
    /* clear the pushBack */
    if(con->nPushBack > 0) {
	for(j = 0; j < con->nPushBack; j++)
	    free(con->PushBack[j]);
	free(con->PushBack);
    }
    free(Connections[i]);
    Connections[i] = NULL;
}


SEXP do_close(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int i;

    checkArity(op, args);
    i = asInteger(CAR(args));
    if(i < 3) error("cannot close standard connections");
    if(i == R_SinkCon) error("cannot close sink connection");
    con_close(i);
    return R_NilValue;
}

/* seek(con, where = numeric(), rw = "") */
SEXP do_seek(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int where;
    SEXP ans;
    Rconnection con = NULL;

    checkArity(op, args);
    con = getConnection(asInteger(CAR(args)));
    where = asInteger(CADR(args));
    if(where == NA_INTEGER || where < 0) where = -1;
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = con->seek(con, where);
    UNPROTECT(1);
    return ans;
}

/* ------------------- read, write  text --------------------- */

int Rconn_fgetc(Rconnection con)
{
    char *curLine;
    int c;

    if(con->nPushBack <= 0) {
	/* map CR or CRLF to LF */
	if (con->save != -1000) {
	    c = con->save;
	    con->save = con->save2;
	    con->save2 = -1000;
	    return c;
	}
	c = con->fgetc(con);
	if (c == '\r') {
	    c = con->fgetc(con);
	    if (c != '\n') {
		con->save2 = con->save;
		con->save = (c != '\r') ? c : '\n';
		return('\n');
	    }
	}
	return c;
    }
    curLine = con->PushBack[con->nPushBack-1];
    c = curLine[con->posPushBack++];
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
    con->save2 = con->save;
    con->save = c;
    return c;
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
    if(!wasopen) con->open(con);
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
    free(buf);
    if(!wasopen) con->close(con);
    if(nbuf > 0) { /* incomplete last line */
	nread++;
	warningcall(call, "incomplete final line");
    }
    if(n < nnn && !ok)
	errorcall(call, "too few lines read");
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
    con = getConnection(asInteger(CADR(args)));
    sep = CADDR(args);
    if(!isString(sep)) error("invalid `sep' argument");
    if(!con->canwrite)
	error("cannot write to this connection");
    wasopen = con->isopen;
    if(!wasopen) con->open(con);
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
	if(!m) return R_NilValue;
	if(*p == '\0') break;
	if(pos >= ibfs - 1) {
	    new = (char *) R_alloc(2*ibfs, sizeof(char));
	    memcpy(new, buf, pos+1);
	    buf = new;
	    ibfs *= 2;
	}
    }
    return mkChar(buf);
}

/* readBin(con, what, n, swap) */
SEXP do_readbin(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue, swhat;
    int i, size, swap, n, m = 0, sizedef= 4, mode = 1;
    char *what;
    void *p = NULL;
    Rboolean wasopen;
    Rconnection con = NULL;

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
    swap = asLogical(CAR(args));
    if(swap == NA_LOGICAL)
	error("invalid value of `swap'");
    if(!con->canread)
	error("cannot read from this connection");

    wasopen = con->isopen;
    if(!wasopen) con->open(con);

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
			INTEGER(ans)[i] = (int)*((signed char *)buf);
			break;
		    case sizeof(short):
			INTEGER(ans)[i] = (int)*((short *)buf);
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
    if(!wasopen) con->open(con);

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
	buf = R_alloc(len, size);
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
    }

    if(!wasopen) con->close(con);
    return R_NilValue;
}

/* ------------------- push back text  --------------------- */


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
	for(i = 0; i < n; i++) {
	    p = CHAR(STRING_ELT(stext, n - i - 1));
	    q += nexists + i;
	    *q = (char *) malloc(strlen(p) + 1 + newLine);
	    if(!(*q)) error("could not allocate space for pushBack");
	    strcpy(*q, p);
	    if(newLine) strcat(*q, "\n");
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


/* ------------------- admin functions  --------------------- */

void InitConnections()
{
    int i;
    Connections[0] = newterminal("stdin", "r");
    Connections[0]->fgetc = stdin_fgetc;
    Connections[0]->ungetc = stdin_ungetc;
    Connections[1] = newterminal("stdout", "w");
    Connections[1]->vfprintf = stdout_vfprintf;
    Connections[1]->fflush = stdout_fflush;
    Connections[2] = newterminal("stderr", "w");
    Connections[2]->vfprintf = stderr_vfprintf;
    Connections[2]->fflush = stderr_fflush;
    for(i = 3; i < NCONNECTIONS; i++) Connections[i] = NULL;
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
