/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000, 2001   The R Development Core Team.
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

#include <R_ext/Boolean.h>

typedef struct Rconn 
{
    char* class;
    char* description;
    char mode[5];
    Rboolean text, isopen, incomplete, canread, canwrite, canseek, blocking;
    void (*open)(struct Rconn *);
    void (*close)(struct Rconn *); /* routine closing after auto open */
    void (*destroy)(struct Rconn *); /* when closing connection */
    int (*vfprintf)(struct Rconn *, const char *, va_list);
    int (*fgetc)(struct Rconn *);
/*    int (*ungetc)(int c, struct Rconn *); */
    long (*seek)(struct Rconn *, int, int, int);
    void (*truncate)(struct Rconn *);
    int (*fflush)(struct Rconn *);
    size_t (*read)(void *, size_t, size_t, struct Rconn *);
    size_t (*write)(const void *, size_t, size_t, struct Rconn *);
/*    void (*onerror)(struct Rconn *); */
    int nPushBack, posPushBack; /* number of lines, position on top line */
    char **PushBack;
    int save, save2;
    unsigned char encoding[256];
    void *private;
} *Rconnection;

typedef struct fileconn {
    FILE *fp;
    long rpos, wpos;
    Rboolean last_was_write;
} *Rfileconn;

typedef struct fifoconn {
    int fd;
} *Rfifoconn;

#ifdef HAVE_ZLIB
typedef struct gzfileconn {
    void *fp;
    int cp;
} *Rgzfileconn;
#endif

typedef struct textconn {
    char *data;  /* all the data */
    int cur, nchars; /* current pos and number of chars */
    char save; /* pushback */
} *Rtextconn;

#define LAST_LINE_LEN 256
typedef struct outtextconn {
    int len;  /* number of lines */
    SEXP namesymbol;
    SEXP data;
    char lastline[LAST_LINE_LEN];
} *Routtextconn;

typedef enum {HTTPsh, FTPsh} UrlScheme;

typedef struct urlconn {
    void *ctxt;
    UrlScheme type;
} *Rurlconn;

typedef struct sockconn {
    int port;
    int server;
    int fd;
    char *host;
    char inbuf[4096], *pstart, *pend;
} *Rsockconn;

int Rconn_fgetc(Rconnection con);
int Rconn_ungetc(int c, Rconnection con);
int Rconn_getline(Rconnection con, char *buf, int bufsize);
int Rconn_printf(Rconnection con, const char *format, ...);
Rconnection getConnection(int n);
Rconnection getConnection_no_err(int n);
Rboolean switch_stdout(int icon, int closeOnExit);
SEXP R_ParseConn(Rconnection con, int n, int *status);
void con_close(int i);
void Rconn_setEncoding(Rconnection con, SEXP enc);
void init_con(Rconnection new, char *description, char *mode);
Rconnection R_newurl(char *description, char *mode);
Rconnection R_newsock(char *host, int port, int server, char *mode);
int dummy_vfprintf(Rconnection con, const char *format, va_list ap);
