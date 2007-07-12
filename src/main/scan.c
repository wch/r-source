/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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

#include <Defn.h>
#include <Fileio.h>
#include <Rconnections.h>

static R_INLINE int imin2(int x, int y)
{
    return (x < y) ? x : y;
}

#ifdef SUPPORT_MBCS
#include <wchar.h> /* for btowc */
#include <R_ext/rlocale.h> /* for btowc */
#endif

/* The size of vector initially allocated by scan */
#define SCAN_BLOCKSIZE		1000
/* The size of the console buffer */
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
    char convbuf[100];
} LocalData;

static SEXP insertString(char *str, LocalData *l)
{
    if (!utf8strIsASCII(str)) {
        if (l->isLatin1) return mkCharEnc(str, LATIN1_MASK);
        if (l->isUTF8)   return mkCharEnc(str, UTF8_MASK);
    }
    return mkCharEnc(str, 0);
}

#define Rspace(c) (c == ' ' || c == '\t' || c == '\n' || c == '\r')

/* used by readline() and menu() */
static int ConsoleGetchar()
{
    if (--ConsoleBufCnt < 0) {
	ConsoleBuf[CONSOLE_BUFFER_SIZE] = '\0';
	if (R_ReadConsole(ConsolePrompt, ConsoleBuf,
			  CONSOLE_BUFFER_SIZE, 0) == 0) {
	    R_ClearerrConsole();
	    return R_EOF;
	}
	ConsoleBufp = ConsoleBuf;
	ConsoleBufCnt = strlen((char *)ConsoleBuf);
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
    return(res);
}

/* Like R_strtod, but allow NA to be a failure if NA arg is false */
static double Rs_strtod(const char *c, char **end, Rboolean NA)
{
    double x;

    if (NA && strncmp(c, "NA", 2) == 0){
	x = NA_REAL; *end = (char *)c + 2; /* coercion for -Wall */
    }
    else if (strncmp(c, "NaN", 3) == 0) {
	x = R_NaN; *end = (char *)c + 3;
    }
    else if (strncmp(c, "Inf", 3) == 0) {
	x = R_PosInf; *end = (char *)c + 3;
    }
    else if (strncmp(c, "-Inf", 4) == 0) {
	x = R_NegInf; *end = (char *)c + 4;
    }
    else
        x = strtod(c, end);
    return x;
}

static double
Strtod (const char *nptr, char **endptr, Rboolean NA, LocalData *d)
{
    if (d->decchar == '.')
	return Rs_strtod(nptr, endptr, NA);
    else {
	/* jump through some hoops... This is a kludge!
	   Should most likely use regexps instead */

	char *end;
	double x;
	int i;

	strncpy(d->convbuf, nptr, 100);
	for ( i = 0 ; i < 100 ; i++ )
	    /* switch '.' and decchar around */
	    if (d->convbuf[i] == d->decchar)
		d->convbuf[i] = '.';
	    else if (d->convbuf[i] == '.')
		d->convbuf[i] = d->decchar;
	x = Rs_strtod(d->convbuf, &end, NA);
	if(endptr)
  	   *endptr = (char *) nptr + (end - d->convbuf);
	return x;
    }
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
    }
    else if (*endp == 'i')  {
	z.r = 0; z.i = x;
	endp++;
    }
    else {
	s = endp;
	y = Strtod(s, &endp, NA, d);
	if (*endp == 'i') {
	    z.r = x; z.i = y;
	    endp++;
	}
	else {
	    z.r = 0; z.i = 0;
	    endp = (char *) nptr; /* -Wall */
	}
    }
    *endptr = endp;
    return(z);
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
    return (d->ttyflag) ? ConsoleGetcharWithPushBack(d->con) :
	Rconn_fgetc(d->con);
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
    int c, quote, filled, nbuf = MAXELTSIZE, m;
#ifdef SUPPORT_MBCS
    Rboolean dbcslocale = (MB_CUR_MAX == 2);
#endif

    m = 0;
    filled = 1;
    if (d->sepchar == 0) {
	/* skip all space or tabs: only look at lead bytes here */
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
		buffer->data[m++] = c;
#ifdef SUPPORT_MBCS
		if(dbcslocale && btowc(c) == WEOF)
		    buffer->data[m++] = scanchar2(d);
#endif
	    }
	    c = scanchar(FALSE, d);
	}
	else { /* not a quoted char string */
	    do {
		if (m >= nbuf - 3) {
		    nbuf *= 2;
		    R_AllocStringBuffer(nbuf, buffer);
		}
		buffer->data[m++] = c;
#ifdef SUPPORT_MBCS
		if(dbcslocale && btowc(c) == WEOF)
		    buffer->data[m++] = scanchar2(d);
#endif
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
		    && strchr(d->quoteset, c)) {
		    quote = c;
		inquote:
		    while ((c = scanchar(TRUE, d)) != R_EOF && c != quote) {
			if (m >= nbuf - 3) {
			    nbuf *= 2;
			    R_AllocStringBuffer(nbuf, buffer);
			}
			buffer->data[m++] = c;
#ifdef SUPPORT_MBCS
			if(dbcslocale && btowc(c) == WEOF)
			    buffer->data[m++] = scanchar2(d);
#endif
		    }
		    c = scanchar(TRUE, d); /* only peek at lead byte
					      unless ASCII */
		    if (c == quote) {
			if (m >= nbuf - 3) {
			    nbuf *= 2;
			    R_AllocStringBuffer(nbuf, buffer);
			}
			buffer->data[m++] = quote;
			goto inquote; /* FIXME: Ick! Clean up logic */
		    }
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
		    buffer->data[m++] = c;
#ifdef SUPPORT_MBCS
		    if(dbcslocale && btowc(c) == WEOF)
			buffer->data[m++] = scanchar2(d);
#endif
		}
	    }
	filled = c; /* last lead byte in a DBCS */
    }
 donefill:
    /* strip trailing white space, if desired and if item is non-null */
    bufp = &buffer->data[m];
   if (strip && m > 0) {
	do {c = (int)*--bufp;} while(Rspace(c));
	bufp++;
    }
    *bufp = '\0';
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
		error(_("invalid 'what' specified"));
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
		      FALSE, 0, FALSE, FALSE};
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
	error(_("invalid '%s' value"), "encoding");
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
	error(_("invalid '%s' value"), "strip.white");
    if (length(stripwhite) != 1 && length(stripwhite) != length(what))
	error(_("invalid 'strip.white' length"));
    if (TYPEOF(data.NAstrings) != STRSXP)
	error(_("invalid '%s' value"), "na.strings");
    if (TYPEOF(comstr) != STRSXP || length(comstr) != 1)
	error(_("invalid '%s' value"), "comment.char");

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else {
	    const char *sc = translateChar(STRING_ELT(sep, 0));
	    if(strlen(sc) > 1)
		error(_("invalid 'sep' value: must be one byte"));
	    data.sepchar = (unsigned char) sc[0];
	}
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' value"), "sep");

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
	error(_("invalid '%s' value"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (unsigned char)*p;
    if(escapes == NA_LOGICAL)
	error(_("invalid '%s' value"), "allowEscapes");
    data.escapes = escapes != 0;

    i = asInteger(file);
    data.con = getConnection(i);
    if(i == 0) {
	data.ttyflag = 1;
    } else {
	data.ttyflag = 0;
	data.wasopen = data.con->isopen;
	if(!data.wasopen) {
	    strcpy(data.con->mode, "r");
	    if(!data.con->open(data.con))
		error(_("cannot open the connection"));
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
	error(_("invalid 'what' specified"));
    }
    endcontext(&cntxt);

    /* we might have a character that was unscanchar-ed.
       So pushback if possible */
    if (data.save && !data.ttyflag && data.wasopen) {
	char line[2] = " ";
	line[0] = data.save;
	con_pushback(data.con, FALSE, line);
    }
    if (!data.ttyflag && !data.wasopen)
	data.con->close(data.con);
    if (data.quoteset[0]) free(data.quoteset);
    return ans;
}

SEXP attribute_hidden do_countfields(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, file, sep,  bns, quotes, comstr;
    int nfields, nskip, i, c, inquote, quote = 0;
    int blocksize, nlines, blskip;
    const char *p;
#ifdef SUPPORT_MBCS
    Rboolean dbcslocale = (MB_CUR_MAX == 2);
#endif
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE};
    data.NAstrings = R_NilValue;

    checkArity(op, args);

    file = CAR(args);	args = CDR(args);
    sep = CAR(args);	args = CDR(args);
    quotes = CAR(args);	 args = CDR(args);
    nskip = asInteger(CAR(args));  args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    comstr = CAR(args);
    if (TYPEOF(comstr) != STRSXP || length(comstr) != 1)
	error(_("invalid '%s' value"), "comment.char");
    p = translateChar(STRING_ELT(comstr, 0));
    data.comchar = NO_COMCHAR; /*  here for -Wall */
    if (strlen(p) > 1)
	error(_("invalid '%s' value"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (unsigned char)*p;

    if (nskip < 0 || nskip == NA_INTEGER) nskip = 0;
    if (blskip == NA_LOGICAL) blskip = 1;

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else data.sepchar = (unsigned char) translateChar(STRING_ELT(sep, 0))[0];
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' value"), "sep");

    if (isString(quotes)) {
	const char *sc = translateChar(STRING_ELT(quotes, 0));
	if (strlen(sc)) data.quoteset = strdup(sc);
	else data.quoteset = "";
    } else if (isNull(quotes))
	data.quoteset = "";
    else
	error(_("invalid quote symbol set"));

    i = asInteger(file);
    data.con = getConnection(i);
    if(i == 0) {
	data.ttyflag = 1;
    } else {
	data.ttyflag = 0;
	data.wasopen = data.con->isopen;
	if(!data.wasopen) {
	    strcpy(data.con->mode, "r");
	    if(!data.con->open(data.con))
		error(_("cannot open the connection"));
	}
	for (i = 0; i < nskip; i++) /* MBCS-safe */
	    while ((c = scanchar(FALSE, &data)) != '\n' && c != R_EOF);
    }

    blocksize = SCAN_BLOCKSIZE;
    PROTECT(ans = allocVector(INTSXP, blocksize));
    nlines = 0;
    nfields = 0;
    inquote = 0;

    data.save = 0;

    for (;;) {
	c = scanchar(inquote, &data);
	if (c == R_EOF)	 {
	    if (nfields != 0)
		INTEGER(ans)[nlines] = nfields;
	    else nlines--;
	    goto donecf;
	}
	else if (c == '\n') {
	    if (nfields || !blskip) {
		INTEGER(ans)[nlines] = nfields;
		nlines++;
		nfields = 0;
		inquote = 0;
	    }
	    if (nlines == blocksize) {
		bns = ans;
		blocksize = 2 * blocksize;
		ans = allocVector(INTSXP, blocksize);
		UNPROTECT(1);
		PROTECT(ans);
		copyVector(ans, bns);
	    }
	    continue;
	}
	else if (data.sepchar) {
	    if (nfields == 0)
		nfields++;
	    if (inquote && (c == R_EOF || c == '\n')) {
		if(!data.wasopen) data.con->close(data.con);
		error(_("string terminated by newline or EOF"));
	    }
	    if (inquote && c == quote)
		inquote = 0;
	    else if (strchr(data.quoteset, c)) {
		inquote = 1;
		quote = c;
	    }
	    if (c == data.sepchar && !inquote)
		nfields++;
	}
	else if (!Rspace(c)) {
	    if (strchr(data.quoteset, c)) {
		quote = c;
		inquote = 1;
		while ((c = scanchar(inquote, &data)) != quote) {
		    if (c == R_EOF || c == '\n') {
			if(!data.wasopen) data.con->close(data.con);
			error(_("string terminated by newline or EOF"));
		    }
		}
		inquote = 0;
	    } else {
		do {
#ifdef SUPPORT_MBCS
		    if(dbcslocale && btowc(c) == WEOF) scanchar2(&data);
#endif
		    c = scanchar(FALSE, &data);
		} while (!Rspace(c) && c != R_EOF);
		if (c == R_EOF) c = '\n';
		unscanchar(c, &data);
	    }
	    nfields++;
	}

    }
 donecf:
    /* we might have a character that was unscanchar-ed.
       So pushback if possible */
    if (data.save && !data.ttyflag && data.wasopen) {
	char line[2] = " ";
	line[0] = data.save;
	con_pushback(data.con, FALSE, line);
    }
    if(!data.wasopen) data.con->close(data.con);

    if (nlines < 0) {
	UNPROTECT(1);
	return R_NilValue;
    }
    if (nlines == blocksize) {
	UNPROTECT(1);
	return ans;
    }

    bns = allocVector(INTSXP, nlines+1);
    for (i = 0; i <= nlines; i++)
	INTEGER(bns)[i] = INTEGER(ans)[i];
    UNPROTECT(1);
    if (data.quoteset[0]) free(data.quoteset);
    return bns;
}

/* A struct used by do_typecvt to keep track of possible types for the input */
typedef struct typecvt_possible_types {
    unsigned int islogical  : 1;
    unsigned int isinteger  : 1;
    unsigned int isreal     : 1;
    unsigned int iscomplex  : 1;
} Typecvt_Info;

/* Sets fields of typeInfo, ruling out possible types based on s.
 *
 * The typeInfo struct should be initialized with all fields TRUE.
 */
static void ruleout_types(const char *s, Typecvt_Info *typeInfo, LocalData *data)
{
    int res;
    char *endp;

    if (typeInfo->islogical) {
        if (strcmp(s, "F") == 0 || strcmp(s, "FALSE") == 0
            || strcmp(s, "T") == 0 || strcmp(s, "TRUE") == 0) {
            typeInfo->isinteger = FALSE;
            typeInfo->isreal = FALSE;
            typeInfo->iscomplex = FALSE;
        } else {
            typeInfo->islogical = TRUE;
        }
    }

    if (typeInfo->isinteger) {
        res = Strtoi(s, 10);
        if (res == NA_INTEGER)
            typeInfo->isinteger = FALSE;
    }

    if (typeInfo->isreal) {
        Strtod(s, &endp, TRUE, data);
        if (!isBlankString(endp))
            typeInfo->isreal = FALSE;
    }

    if (typeInfo->iscomplex) {
        strtoc(s, &endp, TRUE, data);
        if (!isBlankString(endp))
            typeInfo->iscomplex = FALSE;
    }
}


/* type.convert(char, na.strings, as.is, dec) */

/* This is a horrible hack which is used in read.table to take a
   character variable, if possible to convert it to a logical,
   integer, numeric or complex variable.  If this is not possible,
   the result is a character string if as.is == TRUE
   or a factor if as.is == FALSE. */


SEXP attribute_hidden do_typecvt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP cvec, a, dup, levs, dims, names, dec;
    SEXP rval = R_NilValue; /* -Wall */
    int i, j, len, numeric, asIs;
    Rboolean done = FALSE;
    char *endp;
    const char *tmp = NULL;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE};
    Typecvt_Info typeInfo;      /* keep track of possible types of cvec */
    typeInfo.islogical = TRUE;  /* we can't rule anything out initially */
    typeInfo.isinteger = TRUE;
    typeInfo.isreal = TRUE;
    typeInfo.iscomplex = TRUE;
    data.NAstrings = R_NilValue;

    checkArity(op,args);

    if (!isString(CAR(args)))
	error(_("the first argument must be of mode character"));

    data.NAstrings = CADR(args);
    if (TYPEOF(data.NAstrings) != STRSXP)
	error(_("invalid '%s' value"), "na.strings");

    asIs = asLogical(CADDR(args));
    if (asIs == NA_LOGICAL) asIs = 0;

    dec = CADDDR(args);

    if (isString(dec) || isNull(dec)) {
	if (length(dec) == 0)
	    data.decchar = '.';
	else
	    data.decchar = translateChar(STRING_ELT(dec, 0))[0];
    }

    cvec = CAR(args);
    len = length(cvec);

    numeric = 1;

    /* save the dim/dimnames attributes */

    PROTECT(dims = getAttrib(cvec, R_DimSymbol));
    if (isArray(cvec))
	PROTECT(names = getAttrib(cvec, R_DimNamesSymbol));
    else
	PROTECT(names = getAttrib(cvec, R_NamesSymbol));

    /* Use the first non-NA to screen */
    for (i = 0; i < len; i++) {
	tmp = CHAR(STRING_ELT(cvec, i));
	if (!(STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
	      || isNAstring(tmp, 1, &data) || isBlankString(tmp)))
	    break;
    }
    if (i < len) {  /* not all entries are NA */
        ruleout_types(tmp, &typeInfo, &data);
    }

    if (typeInfo.islogical) {
	PROTECT(rval = allocVector(LGLSXP, len));
	for (i = 0; i < len; i++) {
	    tmp = CHAR(STRING_ELT(cvec, i));
	    if (STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
		|| isNAstring(tmp, 1, &data) || isBlankString(tmp))
		LOGICAL(rval)[i] = NA_LOGICAL;
	    else {
		if (strcmp(tmp, "F") == 0 || strcmp(tmp, "FALSE") == 0)
		    LOGICAL(rval)[i] = 0;
		else if(strcmp(tmp, "T") == 0 || strcmp(tmp, "TRUE") == 0)
		    LOGICAL(rval)[i] = 1;
		else {
		    typeInfo.islogical = FALSE;
                    ruleout_types(tmp, &typeInfo, &data);
		    break;
		}
	    }
	}
	if (typeInfo.islogical) done = TRUE; else UNPROTECT(1);
    }

    if (!done && typeInfo.isinteger) {
	PROTECT(rval = allocVector(INTSXP, len));
	for (i = 0; i < len; i++) {
	    tmp = CHAR(STRING_ELT(cvec, i));
	    if (STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
		|| isNAstring(tmp, 1, &data) || isBlankString(tmp))
		INTEGER(rval)[i] = NA_INTEGER;
	    else {
		INTEGER(rval)[i] = Strtoi(tmp, 10);
		if (INTEGER(rval)[i] == NA_INTEGER) {
		    typeInfo.isinteger = FALSE;
                    ruleout_types(tmp, &typeInfo, &data);
		    break;
		}
	    }
	}
	if(typeInfo.isinteger) done = TRUE; else UNPROTECT(1);
    }

    if (!done && typeInfo.isreal) {
	PROTECT(rval = allocVector(REALSXP, len));
	for (i = 0; i < len; i++) {
	    tmp = CHAR(STRING_ELT(cvec, i));
	    if (STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
		|| isNAstring(tmp, 1, &data) || isBlankString(tmp))
		REAL(rval)[i] = NA_REAL;
	    else {
		REAL(rval)[i] = Strtod(tmp, &endp, FALSE, &data);
		if (!isBlankString(endp)) {
		    typeInfo.isreal = FALSE;
                    ruleout_types(tmp, &typeInfo, &data);
		    break;
		}
	    }
	}
	if(typeInfo.isreal) done = TRUE; else UNPROTECT(1);
    }

    if (!done && typeInfo.iscomplex) {
	PROTECT(rval = allocVector(CPLXSXP, len));
	for (i = 0; i < len; i++) {
	    tmp = CHAR(STRING_ELT(cvec, i));
	    if (STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
		|| isNAstring(tmp, 1, &data) || isBlankString(tmp))
		COMPLEX(rval)[i].r = COMPLEX(rval)[i].i = NA_REAL;
	    else {
		COMPLEX(rval)[i] = strtoc(tmp, &endp, FALSE, &data);
		if (!isBlankString(endp)) {
		    typeInfo.iscomplex = FALSE;
                    /* this is not needed, unless other cases are added */
                    ruleout_types(tmp, &typeInfo, &data);
		    break;
		}
	    }
	}
	if(typeInfo.iscomplex) done = TRUE; else UNPROTECT(1);
    }

    if (!done) {
	if (asIs) {
	    PROTECT(rval = duplicate(cvec));
	    for (i = 0; i < len; i++)
		if(isNAstring(CHAR(STRING_ELT(rval, i)), 1, &data))
		    SET_STRING_ELT(rval, i, NA_STRING);
	}
	else {
	    PROTECT(dup = duplicated(cvec, FALSE));
	    j = 0;
	    for (i = 0; i < len; i++) {
		/* <NA> is never to be a level here */
		if (STRING_ELT(cvec, i) == NA_STRING) continue;
		if (LOGICAL(dup)[i] == 0 && !isNAstring(CHAR(STRING_ELT(cvec, i)), 1, &data))
		    j++;
	    }

	    PROTECT(levs = allocVector(STRSXP,j));
	    j = 0;
	    for (i = 0; i < len; i++) {
		if (STRING_ELT(cvec, i) == NA_STRING) continue;
		if (LOGICAL(dup)[i] == 0 && !isNAstring(CHAR(STRING_ELT(cvec, i)), 1, &data))
		    SET_STRING_ELT(levs, j++, STRING_ELT(cvec, i));
	    }

            /* We avoid an allocation by reusing dup,
             * a LGLSXP of the right length
             */
            rval = dup;
	    SET_TYPEOF(rval, INTSXP);

	    /* put the levels in lexicographic order */

	    sortVector(levs, FALSE);

	    PROTECT(a = match(levs, cvec, NA_INTEGER));
	    for (i = 0; i < len; i++)
		INTEGER(rval)[i] = INTEGER(a)[i];

	    setAttrib(rval, R_LevelsSymbol, levs);
	    PROTECT(a = mkString("factor"));
	    setAttrib(rval, R_ClassSymbol, a);
	    UNPROTECT(3);
	}
    }

    setAttrib(rval, R_DimSymbol, dims);
    if (isArray(cvec))
	setAttrib(rval, R_DimNamesSymbol, names);
    else
	setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(3);
    return rval;
}

SEXP attribute_hidden do_readln(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int c;
    char buffer[MAXELTSIZE], *bufp = buffer;
    SEXP ans, prompt;

    checkArity(op,args);

    prompt = CAR(args);
    if (prompt == R_NilValue)
	PROTECT(prompt);
    else {
	PROTECT(prompt = coerceVector(prompt, STRSXP));
	if(length(prompt) > 0)
	    strncpy(ConsolePrompt, translateChar(STRING_ELT(prompt, 0)),
		    CONSOLE_PROMPT_SIZE - 1);
    }

    /* skip space or tab */
    while ((c = ConsoleGetchar()) == ' ' || c == '\t') ;
    if (c != '\n' && c != R_EOF) {
	*bufp++ = c;
	while ((c = ConsoleGetchar())!= '\n' && c != R_EOF) {
	    if (bufp >= &buffer[MAXELTSIZE - 2]) continue;
	    *bufp++ = c;
	}
    }
    /* now strip white space off the end as well */
    while (--bufp >= buffer && (*bufp == ' ' || *bufp == '\t'))
	;
    *++bufp = '\0';
    ConsolePrompt[0] = '\0';

    PROTECT(ans = allocVector(STRSXP,1));
    SET_STRING_ELT(ans, 0, mkChar(buffer));
    UNPROTECT(2);
    return ans;
}

/* Works with digits, but OK in UTF-8 */
SEXP attribute_hidden do_menu(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int c, j;
    double first;
    char buffer[MAXELTSIZE], *bufp = buffer;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE};
    data.NAstrings = R_NilValue;

    checkArity(op,args);

    if (!isString(CAR(args)))
	error(_("invalid argument"));

    sprintf(ConsolePrompt, _("Selection: "));

    while ((c = ConsoleGetchar()) != '\n' && c != R_EOF) {
	if (bufp >= &buffer[MAXELTSIZE - 2]) continue;
	*bufp++ = c;
    }
    *bufp++ = '\0';
    ConsolePrompt[0] = '\0';

    bufp = buffer;
    while (Rspace((int)*bufp)) bufp++;
    first = LENGTH(CAR(args)) + 1;
    if (isdigit((int)*bufp)) {
	first = Strtod(buffer, NULL, TRUE, &data);
    } else {
	for (j = 0; j < LENGTH(CAR(args)); j++) {
	    if (streql(translateChar(STRING_ELT(CAR(args), j)), buffer)) {
		first = j + 1;
		break;
	    }
	}
    }
    return ScalarInteger(first);
}

/* readTableHead(file, nlines, comment.char, blank.lines.skip, quote, sep) */
/* simplified version of readLines, with skip of blank lines and
   comment-only lines */
#define BUF_SIZE 1000
SEXP attribute_hidden do_readtablehead(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, comstr, ans = R_NilValue, ans2, quotes, sep;
    int nlines, i, c, quote=0, nread, nbuf, buf_size = BUF_SIZE, blskip;
    const char *p; char *buf;
    Rboolean empty, skip;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE};
    data.NAstrings = R_NilValue;

    checkArity(op, args);

    file = CAR(args);		   args = CDR(args);
    nlines = asInteger(CAR(args)); args = CDR(args);
    comstr = CAR(args);		   args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    quotes = CAR(args);		   args = CDR(args);
    sep = CAR(args);

    if (nlines <= 0 || nlines == NA_INTEGER)
	error(_("invalid '%s' value"), "nlines");
    if (blskip == NA_LOGICAL) blskip = 1;
    if (isString(quotes)) {
	const char *sc = translateChar(STRING_ELT(quotes, 0));
	if (strlen(sc)) data.quoteset = strdup(sc);
	else data.quoteset = "";
    } else if (isNull(quotes))
	data.quoteset = "";
    else
	error(_("invalid quote symbol set"));

    if (TYPEOF(comstr) != STRSXP || length(comstr) != 1)
	error(_("invalid '%s' value"), "comment.char");
    p = translateChar(STRING_ELT(comstr, 0));
    data.comchar = NO_COMCHAR; /*  here for -Wall */
    if (strlen(p) > 1)
	error(_("invalid '%s' value"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (int)*p;
    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else data.sepchar = (unsigned char) translateChar(STRING_ELT(sep, 0))[0];
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' value"), "sep");

    i = asInteger(file);
    data.con = getConnection(i);
    data.ttyflag = (i == 0);
    data.wasopen = data.con->isopen;
    if(!data.wasopen) {
	strcpy(data.con->mode, "r");
	if(!data.con->open(data.con)) error(_("cannot open the connection"));
    } else { /* for a non-blocking connection, more input may
		have become available, so re-position */
	if(data.con->canseek && !data.con->blocking)
	    data.con->seek(data.con, data.con->seek(data.con, -1, 1, 1), 1, 1);
    }

    buf = (char *) malloc(buf_size);
    if(!buf)
	error(_("cannot allocate buffer in 'readTableHead'"));

    PROTECT(ans = allocVector(STRSXP, nlines));
    for(nread = 0; nread < nlines; ) {
	nbuf = 0; empty = TRUE, skip = FALSE;
	if (data.ttyflag) sprintf(ConsolePrompt, "%d: ", nread);
	/* want to interpret comments here, not in scanchar */
	while((c = scanchar(TRUE, &data)) != R_EOF) {
	    if(nbuf >= buf_size -1) {
		buf_size *= 2;
		buf = (char *) realloc(buf, buf_size);
		if(!buf)
		    error(_("cannot allocate buffer in 'readTableHead'"));
	    }
	    /* Need to handle escaped embedded quotes, and how they are
	       escaped depends on 'sep' */
	    if(quote) {
		if(data.sepchar == 0 && c == '\\') {
		    /* all escapes should be passed through */
		    buf[nbuf++] = c;
		    c = scanchar(TRUE, &data);
		    if(c == R_EOF)
			error(_("\\ followed by EOF"));
		    buf[nbuf++] = c;
		    continue;
		} else if(quote && c == quote) {
		    if(data.sepchar == 0)
			quote = 0;
		    else { /* need to check for doubled quote */
			char c2 = scanchar(TRUE, &data);
			if(c2 == quote)
			    buf[nbuf++] = c; /* and c = c2 */
			else {
			    unscanchar(c2, &data);
			    quote = 0;
			}
		    }
		}
	    } else if(!quote && !skip && strchr(data.quoteset, c)) quote = c;
	    /* A line is empty only if it contains nothing before
	       EOL, EOF or a comment char.
	       A line containing just white space is not empty if sep=","
	    */
	    if(empty && !skip)
		if(c != '\n' && c != data.comchar) empty = FALSE;
	    if(!quote && !skip && c == data.comchar) skip = TRUE;
	    if(quote || c != '\n') buf[nbuf++] = c; else break;
	}
	buf[nbuf] = '\0';
	if(data.ttyflag && empty) goto no_more_lines;
	if(!empty || !blskip) {
	    SET_STRING_ELT(ans, nread, mkChar(buf));
	    nread++;
	}
	if(c == R_EOF) goto no_more_lines;
    }
    UNPROTECT(1);
    free(buf);
    if(!data.wasopen) data.con->close(data.con);
    if (data.quoteset[0]) free(data.quoteset);
    return ans;

no_more_lines:
    if(!data.wasopen) data.con->close(data.con);
    if(nbuf > 0) { /* incomplete last line */
	if(data.con->text && data.con->blocking) {
	    warning(_("incomplete final line found by readTableHeader on '%s'"),
		    data.con->description);
	} else
	    error(_("incomplete final line found by readTableHeader on '%s'"),
		  data.con->description);
    }
    free(buf);
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    if (data.quoteset[0]) free(data.quoteset);
    return ans2;
}

/* --------- write.table --------- */
#include <Print.h>

/* write.table(x, file, nr, nc, rnames, sep, eol, na, dec, quote, qstring)
   x is a matrix or data frame
   file is a connection
   sep eol dec qstring are character strings
   quote is a numeric vector
 */

static Rboolean isna(SEXP x, int indx)
{
    Rcomplex rc;
    switch(TYPEOF(x)) {
    case LGLSXP:
	return LOGICAL(x)[indx] == NA_LOGICAL;
	break;
    case INTSXP:
	return INTEGER(x)[indx] == NA_INTEGER;
	break;
    case REALSXP:
	return ISNAN(REAL(x)[indx]);
	break;
    case STRSXP:
	return STRING_ELT(x, indx) == NA_STRING;
	break;
    case CPLXSXP:
	rc = COMPLEX(x)[indx];
	return ISNAN(rc.r) || ISNAN(rc.i);
	break;
    default:
	break;
    }
    return FALSE;
}

#ifdef UNUSED
static void change_dec(char *tmp, char cdec, SEXPTYPE t)
{
    char *p;
    switch(t) {
    case REALSXP:
    case CPLXSXP:
	for(p = tmp; *p; p++) if(*p == '.') *p = cdec;
	break;
    default:
	break;
    }
}
#endif

/* a version of EncodeElement with different escaping of char strings */
static const char 
*EncodeElement2(SEXP x, int indx, Rboolean quote,
		Rboolean qmethod, R_StringBuffer *buff, char cdec)
{
    int nbuf;
    char *q;
    const char *p, *p0;

    if(TYPEOF(x) == STRSXP) {
	p0 = translateChar(STRING_ELT(x, indx));
	if(!quote) return p0;
	for(nbuf = 2, p = p0; *p; p++) /* find buffer length needed */
	    nbuf += (*p == '"') ? 2 : 1;
	R_AllocStringBuffer(nbuf, buff);
	q = buff->data; *q++ = '"';
	for(p = p0; *p;) {
	    if(*p == '"') *q++ = qmethod ? '\\' : '"';
	    *q++ = *p++;
	}
	*q++ = '"'; *q = '\0';
	return buff->data;
    }
    return EncodeElement(x, indx, quote ? '"' : 0, cdec);
}

typedef struct wt_info {
    Rboolean wasopen;
    Rconnection con;
    R_StringBuffer *buf;
    int savedigits;
} wt_info;

/* utility to cleanup e.g. after interrpts */
static void wt_cleanup(void *data)
{
    wt_info *ld = data;
    if(!ld->wasopen) ld->con->close(ld->con);
    R_FreeStringBuffer(ld->buf);
    R_print.digits = ld->savedigits;
}

SEXP attribute_hidden do_writetable(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, sep, rnames, eol, na, dec, quote, xj;
    int nr, nc, i, j, qmethod;
    Rboolean wasopen, quote_rn = FALSE, *quote_col;
    Rconnection con;
    const char *csep, *ceol, *cna, *sdec, *tmp=NULL /* -Wall */;
    char cdec;
    SEXP *levels;
    R_StringBuffer strBuf = {NULL, 0, MAXELTSIZE};
    wt_info wi;
    RCNTXT cntxt;

    checkArity(op, args);

    x = CAR(args);		   args = CDR(args);
    /* this is going to be a connection open or openable for writing */
    if(!inherits(CAR(args), "connection"))
	error(_("'file' is not a connection"));
    con = getConnection(asInteger(CAR(args))); args = CDR(args);
    if(!con->canwrite)
	error(_("cannot write to this connection"));
    wasopen = con->isopen;
    if(!wasopen) {
	strcpy(con->mode, "wt");
	if(!con->open(con)) error(_("cannot open the connection"));
    }
    nr = asInteger(CAR(args));	   args = CDR(args);
    nc = asInteger(CAR(args));	   args = CDR(args);
    rnames = CAR(args);		   args = CDR(args);
    sep = CAR(args);		   args = CDR(args);
    eol = CAR(args);		   args = CDR(args);
    na = CAR(args);		   args = CDR(args);
    dec = CAR(args);		   args = CDR(args);
    quote = CAR(args);		   args = CDR(args);
    qmethod = asLogical(CAR(args));

    if(nr == NA_INTEGER) error(_("invalid '%s' value"), "nr");
    if(nc == NA_INTEGER) error(_("invalid '%s' value"), "nc");
    if(!isNull(rnames) && !isString(rnames))
	error(_("invalid '%s' value"), "rnames");
    if(!isString(sep)) error(_("invalid '%s' value"), "sep");
    if(!isString(eol)) error(_("invalid '%s' value"), "eol");
    if(!isString(na)) error(_("invalid '%s' value"), "na");
    if(!isString(dec)) error(_("invalid '%s' value"), "dec");
    if(qmethod == NA_LOGICAL) error(_("invalid '%s' value"), "qmethod");
    csep = translateChar(STRING_ELT(sep, 0));
    ceol = translateChar(STRING_ELT(eol, 0));
    cna = translateChar(STRING_ELT(na, 0));
    sdec = translateChar(STRING_ELT(dec, 0));
    if(strlen(sdec) != 1)
	error(_("'dec' must be a single character"));
    cdec = sdec[0];
    quote_col = (Rboolean *) R_alloc(nc, sizeof(Rboolean));
    for(j = 0; j < nc; j++) quote_col[j] = FALSE;
    for(i = 0; i < length(quote); i++) { /* NB, quote might be NULL */
	int this = INTEGER(quote)[i];
	if(this == 0) quote_rn = TRUE;
	if(this >  0) quote_col[this - 1] = TRUE;
    }
    R_AllocStringBuffer(0, &strBuf);
    PrintDefaults(R_NilValue);
    wi.savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
    wi.con = con;
    wi.wasopen = wasopen;
    wi.buf = &strBuf;
    begincontext(&cntxt, CTXT_CCODE, call, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &wt_cleanup;
    cntxt.cenddata = &wi;

    if(isVectorList(x)) { /* A data frame */

	/* handle factors internally, check integrity */
	levels = (SEXP *) R_alloc(nc, sizeof(SEXP));
	for(j = 0; j < nc; j++) {
	    xj = VECTOR_ELT(x, j);
	    if(LENGTH(xj) != nr)
		error(_("corrupt data frame -- length of column %d does not not match nrows"), j+1);
	    if(inherits(xj, "factor")) {
		levels[j] = getAttrib(xj, R_LevelsSymbol);
	    } else levels[j] = R_NilValue;
	}

	for(i = 0; i < nr; i++) {
	    if(i % 1000 == 999) R_CheckUserInterrupt();
	    if(!isNull(rnames))
		Rconn_printf(con, "%s%s",
			     EncodeElement2(rnames, i, quote_rn, qmethod,
					    &strBuf, cdec), csep);
	    for(j = 0; j < nc; j++) {
		xj = VECTOR_ELT(x, j);
		if(j > 0) Rconn_printf(con, "%s", csep);
		if(isna(xj, i)) tmp = cna;
		else {
		    if(!isNull(levels[j])) {
			/* We cannot assume factors have integer levels */
			if(TYPEOF(xj) == INTSXP)
			    tmp = EncodeElement2(levels[j], INTEGER(xj)[i] - 1,
						 quote_col[j], qmethod,
						 &strBuf, cdec);
			else if(TYPEOF(xj) == REALSXP)
			    tmp = EncodeElement2(levels[j], REAL(xj)[i] - 1,
						 quote_col[j], qmethod,
						 &strBuf, cdec);
			else
			    error("column %s claims to be a factor but does not have numeric codes", j+1);
		    } else {
			tmp = EncodeElement2(xj, i, quote_col[j], qmethod,
					     &strBuf, cdec);
		    }
		    /* if(cdec) change_dec(tmp, cdec, TYPEOF(xj)); */
		}
		Rconn_printf(con, "%s", tmp);
	    }
	    Rconn_printf(con, "%s", ceol);
	}

    } else { /* A matrix */

	if(!isVectorAtomic(x))
	    UNIMPLEMENTED_TYPE("write.table, matrix method", x);
	/* quick integrity check */
	if(LENGTH(x) != nr * nc)
	    error(_("corrupt matrix -- dims not not match length"));

	for(i = 0; i < nr; i++) {
	    if(i % 1000 == 999) R_CheckUserInterrupt();
	    if(!isNull(rnames))
		Rconn_printf(con, "%s%s",
			     EncodeElement2(rnames, i, quote_rn, qmethod,
					    &strBuf, cdec), csep);
	    for(j = 0; j < nc; j++) {
		if(j > 0) Rconn_printf(con, "%s", csep);
		if(isna(x, i + j*nr)) tmp = cna;
		else {
		    tmp = EncodeElement2(x, i + j*nr, quote_col[j], qmethod,
					&strBuf, cdec);
		    /* if(cdec) change_dec(tmp, cdec, TYPEOF(x)); */
		}
		Rconn_printf(con, "%s", tmp);
	    }
	    Rconn_printf(con, "%s", ceol);
	}

    }
    endcontext(&cntxt);
    wt_cleanup(&wi);
    return R_NilValue;
}
