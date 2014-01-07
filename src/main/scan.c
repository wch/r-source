/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2013   The R Core Team.
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

/* <UTF8>
   byte-level access needed checks.
   OK in UTF-8 provided quotes, comment, sep and dec chars are ASCII.
   Also OK in DBCS.

   We use only ' ', tab, CR, LF as space chars.
   There is also the possibility of other digits (which we should
   probably continue to ignore).
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <float.h>  /* for DBL_DIG */
#include <Fileio.h>
#include <Rconnections.h>
#include <errno.h>
#include <Print.h>

#include <rlocale.h> /* for btowc */

/* The size of vector initially allocated by scan */
#define SCAN_BLOCKSIZE		1000
/* The size of the console buffer */
/* NB:  in Windows this also needs to be set in gnuwin32/getline/getline.c */
#define CONSOLE_PROMPT_SIZE	256

#define NO_COMCHAR 100000 /* won't occur even in Unicode */


/* The number of distinct strings to track */
#define MAX_STRINGS	10000


static unsigned char ConsoleBuf[CONSOLE_BUFFER_SIZE+1], *ConsoleBufp;
static int  ConsoleBufCnt;
static char ConsolePrompt[CONSOLE_PROMPT_SIZE];

typedef struct {
    SEXP NAstrings;
    int quiet;
    int sepchar; /*  = 0 */      /* This gets compared to ints */
    char decchar; /* = '.' */    /* This only gets compared to chars */
    char *quoteset; /* = NULL */
    int comchar; /* = NO_COMCHAR */
    int ttyflag; /* = 0 */
    Rconnection con; /* = NULL */
    Rboolean wasopen; /* = FALSE */
    Rboolean escapes; /* = FALSE */
    int save; /* = 0; */
    Rboolean isLatin1; /* = FALSE */
    Rboolean isUTF8; /* = FALSE */
    Rboolean atStart;
    Rboolean embedWarn;
    char convbuf[100];
} LocalData;

static SEXP insertString(char *str, LocalData *l)
{
    cetype_t enc = CE_NATIVE;
    if (l->con->UTF8out || l->isUTF8) enc = CE_UTF8;
    else if (l->isLatin1) enc = CE_LATIN1;
    return mkCharCE(str, enc);
}

static R_INLINE Rboolean Rspace(unsigned int c)
{
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r') return TRUE;
#ifdef Win32
    /* 0xa0 is NBSP in all 8-bit Windows locales */
    if(!mbcslocale && c == 0xa0) return TRUE;
#else
     /* 0xa0 is NBSP in Latin-1 */
    if(known_to_be_latin1 && c == 0xa0) return TRUE;
#endif
    return FALSE;
}


/* used by readline() and menu() */
static int ConsoleGetchar(void)
{
    if (--ConsoleBufCnt < 0) {
	ConsoleBuf[CONSOLE_BUFFER_SIZE] = '\0';
	if (R_ReadConsole(ConsolePrompt, ConsoleBuf,
			  CONSOLE_BUFFER_SIZE, 0) == 0) {
	    R_ClearerrConsole();
	    return R_EOF;
	}
	ConsoleBufp = ConsoleBuf;
	ConsoleBufCnt = (int) strlen((char *)ConsoleBuf);
	ConsoleBufCnt--;
    }
    /* at this point we need to use unsigned char or similar */
    return (int) *ConsoleBufp++;
}

/* used by scan() */
static int ConsoleGetcharWithPushBack(Rconnection con)
{
    char *curLine;
    int c;

    if(con->nPushBack > 0) {
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
    } else
	return ConsoleGetchar();
}

/* Like strtol, but for ints not longs and returns NA_INTEGER on overflow */
static int Strtoi(const char *nptr, int base)
{
    long res;
    char *endp;

    errno = 0;
    res = strtol(nptr, &endp, base);
    if (*endp != '\0') res = NA_INTEGER;
    /* next can happen on a 64-bit platform */
    if (res > INT_MAX || res < INT_MIN) res = NA_INTEGER;
    if (errno == ERANGE) res = NA_INTEGER;
    return (int) res;
}

static double
Strtod (const char *nptr, char **endptr, Rboolean NA, LocalData *d)
{
    return R_strtod4(nptr, endptr, d->decchar, NA);
}

static Rcomplex
strtoc(const char *nptr, char **endptr, Rboolean NA, LocalData *d)
{
    Rcomplex z;
    double x, y;
    char *s, *endp;

    x = Strtod(nptr, &endp, NA, d);
    if (isBlankString(endp)) {
	z.r = x; z.i = 0;
    } else if (*endp == 'i')  {
	z.r = 0; z.i = x;
	endp++;
    } else {
	s = endp;
	y = Strtod(s, &endp, NA, d);
	if (*endp == 'i') {
	    z.r = x; z.i = y;
	    endp++;
	} else {
	    z.r = 0; z.i = 0;
	    endp = (char *) nptr; /* -Wall */
	}
    }
    *endptr = endp;
    return z;
}

static Rbyte
strtoraw (const char *nptr, char **endptr)
{
    const char *p = nptr;
    int i, val = 0;

    /* should have whitespace plus exactly 2 hex digits */
    while(Rspace(*p)) p++;
    for(i = 1; i <= 2; i++, p++) {
	val *= 16;
	if(*p >= '0' && *p <= '9') val += *p - '0';
	else if (*p >= 'A' && *p <= 'F') val += *p - 'A' + 10;
	else if (*p >= 'a' && *p <= 'f') val += *p - 'a' + 10;
	else {val = 0; break;}
    }
    *endptr = (char *) p;
    return (Rbyte) val;
}

static R_INLINE int scanchar_raw(LocalData *d)
{
    int c = (d->ttyflag) ? ConsoleGetcharWithPushBack(d->con) :
	Rconn_fgetc(d->con);
    if(c == 0) d->embedWarn = TRUE;
    return c;
}

static R_INLINE void unscanchar(int c, LocalData *d)
{
    d->save = c;
}

/* For second bytes in a DBCS:
   should not be called when a char is saved, but be cautious
*/
static R_INLINE int scanchar2(LocalData *d)
{
    int next;
    if (d->save) {
	next = d->save;
	d->save = 0;
    } else
	next = scanchar_raw(d);
    return next;
}

static int scanchar(Rboolean inQuote, LocalData *d)
{
    int next;
    if (d->save) {
	next = d->save;
	d->save = 0;
    } else
	next = scanchar_raw(d);
    if(next == d->comchar && !inQuote) {
	do
	    next = scanchar_raw(d);
	while (next != '\n' && next != R_EOF);
    }
    if(next == '\\' && d->escapes) {
	next = scanchar_raw(d);
	if ('0' <= next && next <= '8') {
	    int octal = next - '0';
	    if ('0' <= (next = scanchar_raw(d)) && next <= '8') {
		octal = 8 * octal + next - '0';
		if ('0' <= (next = scanchar_raw(d)) && next <= '8') {
		    octal = 8 * octal + next - '0';
		} else unscanchar(next, d);
	    } else unscanchar(next, d);
	    next = octal;
	} else
	    switch(next) {
	    case 'a': next = '\a'; break;
	    case 'b': next = '\b'; break;
	    case 'f': next = '\f'; break;
	    case 'n': next = '\n'; break;
	    case 'r': next = '\r'; break;
	    case 't': next = '\t'; break;
	    case 'v': next = '\v'; break;
	    case 'x': {
		int val = 0; int i, ext;
		for(i = 0; i < 2; i++) {
		    next = scanchar_raw(d);
		    if(next >= '0' && next <= '9') ext = next - '0';
		    else if (next >= 'A' && next <= 'F') ext = next - 'A' + 10;
		    else if (next >= 'a' && next <= 'f') ext = next - 'a' + 10;
		    else {unscanchar(next, d); break;}
		    val = 16*val + ext;
		}
		next = val;
	    }
		break;
	    default:
		/* Any other char and even EOF escapes to itself, but we
		   need to preserve \" etc inside quotes.
		 */
		if(inQuote && strchr(d->quoteset, next)) {
		    unscanchar(next, d);
		    next = '\\';
		}
		break;
	    }
    }
    return next;
}

/* utility to close connections after interrupts */
static void scan_cleanup(void *data)
{
    LocalData *ld = data;
    if(!ld->ttyflag && !ld->wasopen) ld->con->close(ld->con);
    if (ld->quoteset[0]) free(ld->quoteset);
}

#include "RBufferUtils.h"

/*XX  Can we pass this routine an R_StringBuffer? appears so.
   But do we have to worry about continuation lines and whatever
   is currently in the buffer before we call this? In other words,
   what if this appends to the existing content. Appears it writes in
   directly at position 0.
 */
static char *
fillBuffer(SEXPTYPE type, int strip, int *bch, LocalData *d,
	   R_StringBuffer *buffer)
{
/* The basic reader function, called from scanVector() and scanFrame().
   Reads into _buffer_	which later will be read out by extractItem().

   bch is used to distinguish \r, \n and EOF from more input available.
*/
    char *bufp;
    int c, quote, filled, nbuf = MAXELTSIZE, m, mm = 0;
    Rboolean dbcslocale = (MB_CUR_MAX == 2);

    m = 0;
    filled = 1;
    if (d->sepchar == 0) {
	/* skip all space or tabs: only look at lead bytes here */
	strip = 0; /* documented to be ignored in this case */
	while ((c = scanchar(FALSE, d)) == ' ' || c == '\t') ;
	if (c == '\n' || c == '\r' || c == R_EOF) {
	    filled = c;
	    goto donefill;
	}
	if ((type == STRSXP || type == NILSXP) && strchr(d->quoteset, c)) {
	    quote = c;
	    while ((c = scanchar(TRUE, d)) != R_EOF && c != quote) {
		if (m >= nbuf - 3) {
		    nbuf *= 2;
		    R_AllocStringBuffer(nbuf, buffer);
		}
		if (c == '\\') {
		    /* If this is an embedded quote, unquote it, but
		       otherwise keep backslashes */
		    c = scanchar(TRUE, d);
		    if (c == R_EOF) break;
		    if(c != quote) buffer->data[m++] = '\\';
		}
		buffer->data[m++] = (char) c;
		if(dbcslocale && btowc(c) == WEOF)
		    buffer->data[m++] = (char) scanchar2(d);
	    }
	    if (c == R_EOF)
	    	warning(_("EOF within quoted string"));
	    c = scanchar(FALSE, d);
	    mm = m;
	}
	else { /* not a quoted char string */
	    do {
		if (m >= nbuf - 3) {
		    nbuf *= 2;
		    R_AllocStringBuffer(nbuf, buffer);
		}
		buffer->data[m++] = (char) c;
		if(dbcslocale && btowc(c) == WEOF)
		    buffer->data[m++] = (char) scanchar2(d);
		c = scanchar(FALSE, d);
	    } while (!Rspace(c) && c != R_EOF);
	}
	/* skip all space or tabs: only look at lead bytes here */
	while (c == ' ' || c == '\t') c = scanchar(FALSE, d);
	if (c == '\n' || c == '\r' || c == R_EOF)
	    filled = c;
	else
	    unscanchar(c, d);
    }
    else { /* have separator */
	while ((c = scanchar(FALSE, d)) != d->sepchar &&
	       c != '\n' && c != '\r' && c != R_EOF)
	    {
		/* eat white space */
		if (type != STRSXP)
		    while (c == ' ' || c == '\t')
			if ((c = scanchar(FALSE, d)) == d->sepchar
			    || c == '\n' || c == '\r' || c == R_EOF) {
			    filled = c;
			    goto donefill;
			}
		/* CSV style quoted string handling */
		if ((type == STRSXP || type == NILSXP)
		    && c != 0 && strchr(d->quoteset, c)) {
		    quote = c;
		inquote:
		    while ((c = scanchar(TRUE, d)) != R_EOF && c != quote) {
			if (m >= nbuf - 3) {
			    nbuf *= 2;
			    R_AllocStringBuffer(nbuf, buffer);
			}
			buffer->data[m++] = (char) c;
			if(dbcslocale && btowc(c) == WEOF)
			    buffer->data[m++] = (char) scanchar2(d);
		    }
		    if (c == R_EOF)
		    	warning(_("EOF within quoted string"));
		    c = scanchar(TRUE, d); /* only peek at lead byte
					      unless ASCII */
		    if (c == quote) {
			if (m >= nbuf - 3) {
			    nbuf *= 2;
			    R_AllocStringBuffer(nbuf, buffer);
			}
			buffer->data[m++] = (char) quote;
			goto inquote; /* FIXME: Ick! Clean up logic */
		    }
		    mm = m;
		    if (c == d->sepchar || c == '\n' || c == '\r' || c == R_EOF){
			filled = c;
			goto donefill;
		    }
		    else {
			unscanchar(c, d);
			continue;
		    }
		} /* end of CSV-style quote handling */
		if (!strip || m > 0 || !Rspace(c)) { /* only lead byte */
		    if (m >= nbuf - 3) {
			nbuf *= 2;
			R_AllocStringBuffer(nbuf, buffer);
		    }
		    buffer->data[m++] = (char) c;
		    if(dbcslocale && btowc(c) == WEOF)
			buffer->data[m++] = (char) scanchar2(d);
		}
	    }
	filled = c; /* last lead byte in a DBCS */
    }
 donefill:
    /* strip trailing white space, if desired and if item is non-null */
    bufp = &buffer->data[m];
    if (strip && m > mm) {
	do {c = (int)*--bufp;} while(m-- > mm && Rspace(c));
	bufp++;
    }
    *bufp = '\0';
    /* Remove UTF-8 BOM */
    if(d->atStart && utf8locale && 
       !memcmp(buffer->data, "\xef\xbb\xbf", 3))
	memmove(buffer->data, buffer->data+3, strlen(buffer->data) + 1);
    d->atStart = FALSE;
    *bch = filled;
    return buffer->data;
}

/* If mode = 0 use for numeric fields where "" is NA
   If mode = 1 use for character fields where "" is verbatim unless
   na.strings includes "" */
static R_INLINE int isNAstring(const char *buf, int mode, LocalData *d)
{
    int i;

    if(!mode && strlen(buf) == 0) return 1;
    for (i = 0; i < length(d->NAstrings); i++)
	if (!strcmp(CHAR(STRING_ELT(d->NAstrings, i)), buf)) return 1;
    return 0;
}

static R_INLINE void expected(char *what, char *got, LocalData *d)
{
    int c;
    if (d->ttyflag) { /* This is safe in a MBCS */
	while ((c = scanchar(FALSE, d)) != R_EOF && c != '\n')
	    ;
    }
    error(_("scan() expected '%s', got '%s'"), what, got);
}

static void extractItem(char *buffer, SEXP ans, int i, LocalData *d)
{
    char *endp;
    switch(TYPEOF(ans)) {
    case NILSXP:
	break;
    case LGLSXP:
	if (isNAstring(buffer, 0, d))
	    LOGICAL(ans)[i] = NA_INTEGER;
	else {
	    int tr = StringTrue(buffer), fa = StringFalse(buffer);
	    if(tr || fa) LOGICAL(ans)[i] = tr;
	    else expected("a logical", buffer, d);
	}
	break;
    case INTSXP:
	if (isNAstring(buffer, 0, d))
	    INTEGER(ans)[i] = NA_INTEGER;
	else {
	    INTEGER(ans)[i] = Strtoi(buffer, 10);
	    if (INTEGER(ans)[i] == NA_INTEGER)
		expected("an integer", buffer, d);
	}
	break;
    case REALSXP:
	if (isNAstring(buffer, 0, d))
	    REAL(ans)[i] = NA_REAL;
	else {
	    REAL(ans)[i] = Strtod(buffer, &endp, TRUE, d);
	    if (!isBlankString(endp))
		expected("a real", buffer, d);
	}
	break;
    case CPLXSXP:
	if (isNAstring(buffer, 0, d))
	    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
	else {
	    COMPLEX(ans)[i] = strtoc(buffer, &endp, TRUE, d);
	    if (!isBlankString(endp))
		expected("a complex", buffer, d);
	}
	break;
    case STRSXP:
	if (isNAstring(buffer, 1, d))
	    SET_STRING_ELT(ans, i, NA_STRING);
	else
	    SET_STRING_ELT(ans, i, insertString(buffer, d));
	break;
    case RAWSXP:
	if (isNAstring(buffer, 0, d))
	    RAW(ans)[i] = 0;
	else {
	    RAW(ans)[i] = strtoraw(buffer, &endp);
	    if (!isBlankString(endp))
		expected("a raw", buffer, d);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("extractItem", ans);
    }
}

static SEXP scanVector(SEXPTYPE type, int maxitems, int maxlines,
		       int flush, SEXP stripwhite, int blskip, LocalData *d)
{
    SEXP ans, bns;
    int blocksize, c, i, n, linesread, nprev,strip, bch;
    char *buffer;
    R_StringBuffer strBuf = {NULL, 0, MAXELTSIZE};

    if (maxitems > 0) blocksize = maxitems;
    else blocksize = SCAN_BLOCKSIZE;

    R_AllocStringBuffer(0, &strBuf);
    PROTECT(ans = allocVector(type, blocksize));

    nprev = 0; n = 0; linesread = 0; bch = 1;

    if (d->ttyflag) sprintf(ConsolePrompt, "1: ");

    strip = asLogical(stripwhite);

    for (;;) {
	if(n % 10000 == 9999) R_CheckUserInterrupt();
	if (bch == R_EOF) {
	    if (d->ttyflag) R_ClearerrConsole();
	    break;
	}
	else if (bch == '\n') {
	    linesread++;
	    if (linesread == maxlines)
		break;
	    if (d->ttyflag) sprintf(ConsolePrompt, "%d: ", n + 1);
	    nprev = n;
	}
	if (n == blocksize) {
	    /* enlarge the vector*/
	    bns = ans;
	    if(blocksize > INT_MAX/2) error(_("too many items"));
	    blocksize = 2 * blocksize;
	    ans = allocVector(type, blocksize);
	    UNPROTECT(1);
	    PROTECT(ans);
	    copyVector(ans, bns);
	}
	buffer = fillBuffer(type, strip, &bch, d, &strBuf);
	if (nprev == n && strlen(buffer)==0 &&
	    ((blskip && bch =='\n') || bch == R_EOF)) {
	    if (d->ttyflag || bch == R_EOF)
		break;
	}
	else {
	    extractItem(buffer, ans, n, d);
	    if (++n == maxitems) {
		if (d->ttyflag && bch != '\n') { /* MBCS-safe */
		    while ((c = scanchar(FALSE, d)) != '\n')
			;
		}
		break;
	    }
	}
	if (flush && (bch != '\n') && (bch != R_EOF)) { /* MBCS-safe */
	    while ((c = scanchar(FALSE, d)) != '\n' && (c != R_EOF));
	    bch = c;
	}
    }
    if (!d->quiet) REprintf("Read %d item%s\n", n, (n == 1) ? "" : "s");
    if (d->ttyflag) ConsolePrompt[0] = '\0';

    if (n == 0) {
	UNPROTECT(1);
	R_FreeStringBuffer(&strBuf);
	return allocVector(type,0);
    }
    if (n == maxitems) {
	UNPROTECT(1);
	R_FreeStringBuffer(&strBuf);
	return ans;
    }

    bns = allocVector(type, n);
    switch (type) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    INTEGER(bns)[i] = INTEGER(ans)[i];
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    REAL(bns)[i] = REAL(ans)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    COMPLEX(bns)[i] = COMPLEX(ans)[i];
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(bns, i, STRING_ELT(ans, i));
	break;
    case RAWSXP:
	for (i = 0; i < n; i++)
	    RAW(bns)[i] = RAW(ans)[i];
	break;
    default:
	UNIMPLEMENTED_TYPEt("scanVector", type);
    }
    UNPROTECT(1);
    R_FreeStringBuffer(&strBuf);
    return bns;
}


static SEXP scanFrame(SEXP what, int maxitems, int maxlines, int flush,
		      int fill, SEXP stripwhite, int blskip, int multiline,
		      LocalData *d)
{
    SEXP ans, new, old, w;
    char *buffer = NULL;
    int blksize, c, i, ii, j, n, nc, linesread, colsread, strip, bch;
    int badline, nstring = 0;
    R_StringBuffer buf = {NULL, 0, MAXELTSIZE};

    nc = length(what);
    if (!nc) {
	    error(_("empty 'what' specified"));
    }

    if (maxitems > 0) blksize = maxitems;
    else if (maxlines > 0) blksize = maxlines;
    else blksize = SCAN_BLOCKSIZE;

    R_AllocStringBuffer(0, &buf);
    PROTECT(ans = allocVector(VECSXP, nc));
    for (i = 0; i < nc; i++) {
	w = VECTOR_ELT(what, i);
	if (!isNull(w)) {
	    if (!isVector(w)) {
		error(_("invalid '%s' argument"), "what");
	    }
	    if(TYPEOF(w) == STRSXP) nstring++;
	    SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(w), blksize));
	}
    }
    setAttrib(ans, R_NamesSymbol, getAttrib(what, R_NamesSymbol));

    n = 0; linesread = 0; colsread = 0; ii = 0;
    badline = 0;
    bch = 1;
    c = 0;			/* -Wall */

    if (d->ttyflag) sprintf(ConsolePrompt, "1: ");

    strip = asLogical(stripwhite);

    for (;;) {
	if(linesread % 1000 == 999) R_CheckUserInterrupt();

	if (bch == R_EOF) {
	    if (d->ttyflag) R_ClearerrConsole();
	    goto done;
	}
	else if (bch == '\n') {
	    linesread++;
	    if (colsread != 0) {
		if (fill) {
		    buffer[0] = '\0';
		    for (ii = colsread; ii < nc; ii++) {
			extractItem(buffer, VECTOR_ELT(ans, ii), n, d);
		    }
		    n++;
		    ii = 0;
		    colsread = 0;
		} else if (!badline && !multiline)
		    badline = linesread;
		if(badline && !multiline)
		error(_("line %d did not have %d elements"), badline, nc);
	    }
	    if (maxitems > 0 && n >= maxitems)
		goto done;
	    if (maxlines > 0 && linesread == maxlines)
		goto done;
	    if (d->ttyflag)
		sprintf(ConsolePrompt, "%d: ", n + 1);
	}
	if (n == blksize && colsread == 0) {
	    if(blksize > INT_MAX/2) error(_("too many items"));
	    blksize = 2 * blksize;
	    for (i = 0; i < nc; i++) {
		old = VECTOR_ELT(ans, i);
		if(!isNull(old)) {
		    new = allocVector(TYPEOF(old), blksize);
		    copyVector(new, old);
		    SET_VECTOR_ELT(ans, i, new);
		}
	    }
	}

	buffer = fillBuffer(TYPEOF(VECTOR_ELT(ans, ii)), strip, &bch, d, &buf);
	if (colsread == 0 &&
	    strlen(buffer) == 0 &&
	    ((blskip && bch =='\n') || bch == R_EOF)) {
	    if (d->ttyflag || bch == R_EOF)
		break;
	}
	else {
	    extractItem(buffer, VECTOR_ELT(ans, ii), n, d);
	    ii++;
	    colsread++;
	    if (length(stripwhite) == length(what))
		strip = LOGICAL(stripwhite)[colsread];
	    /* increment n and reset i after filling a row */
	    if (colsread == nc) {
		n++;
		ii = 0;
		colsread = 0;
		if (flush && (bch != '\n') && (bch != R_EOF)) { /* MBCS-safe */
		    while ((c = scanchar(FALSE, d)) != '\n' && c != R_EOF);
		    bch = c;
		}
		if (length(stripwhite) == length(what))
		    strip = LOGICAL(stripwhite)[0];
	    }
	}
    }

 done:
    if (colsread != 0) {
	if (!fill)
	    warning(_("number of items read is not a multiple of the number of columns"));
	buffer[0] = '\0';	/* this is an NA */
	for (ii = colsread; ii < nc; ii++) {
	    extractItem(buffer, VECTOR_ELT(ans, ii), n, d);
	}
	n++;
    }
    if (!d->quiet) REprintf("Read %d record%s\n", n, (n == 1) ? "" : "s");
    if (d->ttyflag) ConsolePrompt[0] = '\0';

    for (i = 0; i < nc; i++) {
	old = VECTOR_ELT(ans, i);
	new = allocVector(TYPEOF(old), n);
	switch (TYPEOF(old)) {
	case LGLSXP:
	case INTSXP:
	    for (j = 0; j < n; j++)
		INTEGER(new)[j] = INTEGER(old)[j];
	    break;
	case REALSXP:
	    for (j = 0; j < n; j++)
		REAL(new)[j] = REAL(old)[j];
	    break;
	case CPLXSXP:
	    for (j = 0; j < n; j++)
		COMPLEX(new)[j] = COMPLEX(old)[j];
	    break;
	case STRSXP:
	    for (j = 0; j < n; j++)
		SET_STRING_ELT(new, j, STRING_ELT(old, j));
	    break;
	case RAWSXP:
	    for (j = 0; j < n; j++)
		RAW(new)[j] = RAW(old)[j];
	    break;
	case NILSXP:
	    break;
	default:
	    UNIMPLEMENTED_TYPE("scanFrame", old);
	}
	SET_VECTOR_ELT(ans, i, new);
    }
    UNPROTECT(1);
    R_FreeStringBuffer(&buf);
    return ans;
}

SEXP attribute_hidden do_scan(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, file, sep, what, stripwhite, dec, quotes, comstr;
    int i, c, nlines, nmax, nskip, flush, fill, blskip, multiline, escapes;
    const char *p, *encoding;
    RCNTXT cntxt;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE, FALSE};
    data.NAstrings = R_NilValue;

    checkArity(op, args);

    file = CAR(args);		   args = CDR(args);
    what = CAR(args);		   args = CDR(args);
    nmax = asInteger(CAR(args));   args = CDR(args);
    sep = CAR(args);		   args = CDR(args);
    dec = CAR(args);		   args = CDR(args);
    quotes = CAR(args);		   args = CDR(args);
    nskip = asInteger(CAR(args));  args = CDR(args);
    nlines = asInteger(CAR(args)); args = CDR(args);
    data.NAstrings = CAR(args);	   args = CDR(args);
    flush = asLogical(CAR(args));  args = CDR(args);
    fill  = asLogical(CAR(args));  args = CDR(args);
    stripwhite = CAR(args);	   args = CDR(args);
    data.quiet = asLogical(CAR(args));  args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    multiline = asLogical(CAR(args)); args = CDR(args);
    comstr = CAR(args);            args = CDR(args);
    escapes = asLogical(CAR(args));args = CDR(args);
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' argument"), "encoding");
    encoding = CHAR(STRING_ELT(CAR(args), 0)); /* ASCII */
    if(streql(encoding, "latin1")) data.isLatin1 = TRUE;
    if(streql(encoding, "UTF-8"))  data.isUTF8 = TRUE;

    if (data.quiet == NA_LOGICAL)		data.quiet = 0;
    if (blskip == NA_LOGICAL)			blskip = 1;
    if (multiline == NA_LOGICAL)		multiline = 1;
    if (nskip < 0 || nskip == NA_INTEGER)	nskip = 0;
    if (nlines < 0 || nlines == NA_INTEGER)	nlines = 0;
    if (nmax < 0 || nmax == NA_INTEGER)		nmax = 0;

    if (TYPEOF(stripwhite) != LGLSXP)
	error(_("invalid '%s' argument"), "strip.white");
    if (length(stripwhite) != 1 && length(stripwhite) != length(what))
	error(_("invalid 'strip.white' length"));
    if (TYPEOF(data.NAstrings) != STRSXP)
	error(_("invalid '%s' argument"), "na.strings");
    if (TYPEOF(comstr) != STRSXP || length(comstr) != 1)
	error(_("invalid '%s' argument"), "comment.char");

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else {
	    const char *sc = translateChar(STRING_ELT(sep, 0));
	    if(strlen(sc) > 1)
		error(_("invalid 'sep' value: must be one byte"));
	    data.sepchar = (unsigned char) sc[0];
	}
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' argument"), "sep");

    if (isString(dec) || isNull(dec)) {
	if (length(dec) == 0)
	    data.decchar = '.';
	else {
	    const char *dc = translateChar(STRING_ELT(dec, 0));
	    if(strlen(dc) != 1)
		error(_("invalid decimal separator: must be one byte"));
	    data.decchar = dc[0];
	}
    }
    else
	error(_("invalid decimal separator"));

    if (isString(quotes)) {
	const char *sc = translateChar(STRING_ELT(quotes, 0));
	if (strlen(sc)) data.quoteset = strdup(sc);
	else data.quoteset = "";
    } else if (isNull(quotes))
	data.quoteset = "";
    else
	error(_("invalid quote symbol set"));

    p = translateChar(STRING_ELT(comstr, 0));
    data.comchar = NO_COMCHAR; /*  here for -Wall */
    if (strlen(p) > 1)
	error(_("invalid '%s' argument"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (unsigned char)*p;
    if(escapes == NA_LOGICAL)
	error(_("invalid '%s' argument"), "allowEscapes");
    data.escapes = escapes != 0;

    i = asInteger(file);
    data.con = getConnection(i);
    if(i == 0) {
	data.atStart = FALSE;
	data.ttyflag = 1;
    } else {
	data.atStart = (nskip == 0);
	data.ttyflag = 0;
	data.wasopen = data.con->isopen;
	if(!data.wasopen) {
	    data.con->UTF8out = TRUE;  /* a request */
	    strcpy(data.con->mode, "r");
	    if(!data.con->open(data.con))
		error(_("cannot open the connection"));
	    if(!data.con->canread) {
		data.con->close(data.con);
		error(_("cannot read from this connection"));
	    }
	} else {
	    if(!data.con->canread) 
		error(_("cannot read from this connection"));
	}
	for (i = 0; i < nskip; i++) /* MBCS-safe */
	    while ((c = scanchar(FALSE, &data)) != '\n' && c != R_EOF);
    }

    ans = R_NilValue;		/* -Wall */
    data.save = 0;

    /* set up a context which will close the connection if there is
       an error or user interrupt */
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &scan_cleanup;
    cntxt.cenddata = &data;

    switch (TYPEOF(what)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	ans = scanVector(TYPEOF(what), nmax, nlines, flush, stripwhite,
			 blskip, &data);
	break;

    case VECSXP:
	ans = scanFrame(what, nmax, nlines, flush, fill, stripwhite,
			blskip, multiline, &data);
	break;
    default:
	error(_("invalid '%s' argument"), "what");
    }
    endcontext(&cntxt);

    /* we might have a character that was unscanchar-ed.
       So pushback if possible */
    if (data.save && !data.ttyflag && data.wasopen) {
	char line[2] = " ";
	line[0] = (char) data.save;
	con_pushback(data.con, FALSE, line);
    }
    if (!data.ttyflag && !data.wasopen)
	data.con->close(data.con);
    if (data.quoteset[0]) free(data.quoteset);
    if (data.embedWarn) warning(_("embedded nul found in input"));
    return ans;
}

SEXP attribute_hidden do_readln(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int c;
    char buffer[MAXELTSIZE], *bufp = buffer;
    SEXP ans, prompt;

    checkArity(op,args);

    prompt = CAR(args);
    if (prompt == R_NilValue) {
	ConsolePrompt[0] = '\0'; /* precaution */
	PROTECT(prompt);
    } else {
	PROTECT(prompt = coerceVector(prompt, STRSXP));
	if(length(prompt) > 0)
	    strncpy(ConsolePrompt, translateChar(STRING_ELT(prompt, 0)),
		    CONSOLE_PROMPT_SIZE - 1);
    }

    if(R_Interactive) {
	/* skip space or tab */
	while ((c = ConsoleGetchar()) == ' ' || c == '\t') ;
	if (c != '\n' && c != R_EOF) {
	    *bufp++ = (char) c;
	    while ((c = ConsoleGetchar())!= '\n' && c != R_EOF) {
		if (bufp >= &buffer[MAXELTSIZE - 2]) continue;
		*bufp++ = (char) c;
	    }
	}
	/* now strip white space off the end as well */
	while (--bufp >= buffer && (*bufp == ' ' || *bufp == '\t'))
	    ;
	*++bufp = '\0';
	ConsolePrompt[0] = '\0';

	ans = mkString(buffer);
    } else {
	/* simulate CR as response */
	Rprintf("%s\n", ConsolePrompt);
	ans = mkString("");
    }
    UNPROTECT(1);
    return ans;
}
