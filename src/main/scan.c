/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001   The R Development Core Team.
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

/* The size of vector initially allocated by scan */
#define SCAN_BLOCKSIZE		1000
/* The size of the console buffer */
#define CONSOLE_BUFFER_SIZE	1024
#define CONSOLE_PROMPT_SIZE	32

static unsigned char  ConsoleBuf[CONSOLE_BUFFER_SIZE];
static unsigned char *ConsoleBufp;
static int  ConsoleBufCnt;
static char  ConsolePrompt[CONSOLE_PROMPT_SIZE];


static int save = 0;
static int sepchar = 0;
static int decchar = '.';
static char *quoteset;
static char *quotesave = NULL;
static Rconnection con;
static Rboolean wasopen;
static int ttyflag;
static int quiet;
static SEXP NAstrings;

static char convbuf[100];

#ifdef NOT_used
static void InitConsoleGetchar()
{
    ConsoleBufCnt = 0;
    ConsolePrompt[0] = '\0';
}
#endif

static int ConsoleGetchar()
{
    if (--ConsoleBufCnt < 0) {
	if (R_ReadConsole(ConsolePrompt, ConsoleBuf,
			  CONSOLE_BUFFER_SIZE, 0) == 0) {
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

static double Strtod (const char *nptr, char **endptr) 
{
    if (decchar == '.')
	return strtod(nptr, endptr);
    else { 
	/* jump through some hoops... This is a kludge! 
	   Should most likely use regexps instead */

	char *end;
	double x;
	int i;

	strncpy(convbuf, nptr, 100);
	for ( i = 0 ; i < 100 ; i++ )
	    /* switch '.' and decchar around */
	    if (convbuf[i] == decchar)
		convbuf[i] = '.';
	    else if (convbuf[i] == '.')
		convbuf[i] = decchar;
	x = R_strtod(convbuf, &end);
	*endptr = (char *) nptr + (end - convbuf);
	return x;
    } 
}	

static Rcomplex strtoc(const char *nptr, char **endptr) {
    Rcomplex z;
    double x, y;
    char *s, *endp;

    x = Strtod(nptr, &endp);
    if (isBlankString(endp)) {
	z.r = x; z.i = 0;
    }
    else if (*endp == 'i')  {
	z.r = 0; z.i = x;
	endp++;
    }
    else {
	s = endp;
	y = Strtod(s, &endp);
	if (*endp == 'i') {
	    z.r = x; z.i = y;
	    endp++;
	}
	else {
	    z.r = 0; z.i = 0;
	}
    }
    *endptr = endp;
    return(z);
}

static int scanchar(void)
{
    if (save) {
	int c = save;
	save = 0;
	return c;
    }
    return (ttyflag) ? ConsoleGetchar() : Rconn_fgetc(con);
}

static void unscanchar(int c)
{
    save = c;
}

static int fillBuffer(char *buffer, SEXPTYPE type, int strip)
{
/* The basic reader function, called from scanVector() and scanFrame().
   Reads into _buffer_	which later will be read out by extractItem().
*/
    char *bufp = buffer;
    int c, quote, filled;
    Rboolean warned = FALSE;

    filled = 1;
    if (sepchar == 0) {
	/* skip all white space */
	while ((c = scanchar()) == ' ' || c == '\t')
	    ;
	if (c == '\n' || c == '\r' || c == R_EOF) {
	    filled = c;
	    goto donefill;
	}
	warned = FALSE;
	if (type == STRSXP && strchr(quoteset, c)) {
	    quote = c;
	    while ((c = scanchar()) != R_EOF && c != quote) {
		if (bufp >= &buffer[MAXELTSIZE - 2]) {
		    if(!warned) {
			warning("string truncated to 8190 chars in scan");
			warned = TRUE;
		    }
		    continue;
		}
		if (c == '\\') {
		    c = scanchar();
		    if (c == R_EOF) break;
		    else if (c == 'n') c = '\n';
		    else if (c == 'r') c = '\r';
		}
		*bufp++ = c;
	    }
	    c = scanchar();
	    while (c == ' ' || c == '\t')
		c = scanchar();
	    if (c == '\n' || c == '\r' || c == R_EOF)
		filled = c;
	    else
		unscanchar(c);
	}
	else { /* not a char string */
	    do {
		if (bufp >= &buffer[MAXELTSIZE - 2]) {
		    if(!warned) {
			warning("string truncated to 8190 chars in scan");
			warned = TRUE;
		    }
		    continue;
		}
		*bufp++ = c;
	    } while (!isspace(c = scanchar()) && c != R_EOF);
	    while (c == ' ' || c == '\t')
		c = scanchar();
	    if (c == '\n' || c == '\r' || c == R_EOF)
		filled = c;
	    else
		unscanchar(c);
	}
    }
    else { /* have separator */
	while ((c = scanchar()) != sepchar && c != '\n' && c != '\r'
	      && c != R_EOF)
	    {
		/* eat white space */
		if (type != STRSXP)
		    while (c == ' ' || c == '\t')
			if ((c=scanchar()) == sepchar || c == '\n' ||
			    c == '\r' || c == R_EOF) {
			    filled = c;
			    goto donefill;
			}
		/* CSV style quoted string handling */
		if (type == STRSXP && strchr(quoteset, c)) {
		    quote = c;
		inquote:
		    while ((c = scanchar()) != R_EOF && c != quote) {
			if (bufp >= &buffer[MAXELTSIZE - 2]) {
			    if(!warned) {
				warning("string truncated to 8190 chars in scan");
				warned = TRUE;
			    }
			    continue;
			}
			*bufp++ = c;
		    }
		    c = scanchar();
		    if (c == quote) {
			if (bufp < &buffer[MAXELTSIZE - 2]) 
			    *bufp++ = quote;
			goto inquote; /* FIXME: Ick! Clean up logic */
		    }
		    if (c == sepchar || c == '\n' || c == '\r' || c == R_EOF){
			filled = c;
			goto donefill;
		    }
		    else {
			unscanchar(c);
			continue;
		    }
		}
		if (!strip || bufp != buffer || !isspace(c)) {
		    if (bufp >= &buffer[MAXELTSIZE - 2]) {
			if(!warned) {
			    warning("string truncated to 8190 chars in scan");
			    warned = TRUE;
			}
			continue;
		    } else
			*bufp++ = c;
		}
	    }
	filled = c;
    }
 donefill:
    /* strip trailing white space, if desired and if item is non-null */
    if (strip && bufp > buffer) {
	while (isspace((int)*--bufp))
	    ;
	bufp++;
    }
    *bufp = '\0';
    return filled;
}

/* If mode = 0 use for numeric fields where "" is NA
   If mode = 1 use for character fields where "" is verbatim unless
   na.strings includes "" */
static int isNAstring(char *buf, int mode)
{
    int i;

    if(!mode && strlen(buf) == 0) return 1;
    for (i = 0; i < length(NAstrings); i++)
	if (!strcmp(CHAR(STRING_ELT(NAstrings, i)),buf)) return 1;
    return 0;
}

static void expected(char *what, char *got)
{
    int c;
    if (ttyflag) {
	while ((c = scanchar()) != R_EOF && c != '\n')
	    ;
    }
    else
	if(!wasopen) con->close(con);
    error("\"scan\" expected %s, got \"%s\"", what, got);
}

static void extractItem(char *buffer, SEXP ans, int i)
{
    char *endp;
    switch(TYPEOF(ans)) {
    case LGLSXP:
	if (isNAstring(buffer, 0))
	    LOGICAL(ans)[i] = NA_INTEGER;
	else
	    LOGICAL(ans)[i] = StringTrue(buffer);
	break;
    case INTSXP:
	if (isNAstring(buffer, 0))
	    INTEGER(ans)[i] = NA_INTEGER;
	else {
	    INTEGER(ans)[i] = strtol(buffer, &endp, 10);
	    if (*endp != '\0')
		expected("an integer", buffer);
	}
	break;
    case REALSXP:
	if (isNAstring(buffer, 0))
	    REAL(ans)[i] = NA_REAL;
	else {
	    REAL(ans)[i] = Strtod(buffer, &endp);
	    if (!isBlankString(endp))
		expected("a real", buffer);
	}
	break;
    case CPLXSXP:
	if (isNAstring(buffer, 0))
	    COMPLEX(ans)[i].r = COMPLEX(ans)[i].i = NA_REAL;
	else {
	    COMPLEX(ans)[i] = strtoc(buffer, &endp);
	    if (!isBlankString(endp))
		expected("a complex", buffer);
	}
	break;
    case STRSXP:
	if (isNAstring(buffer, 1))
	    SET_STRING_ELT(ans, i, NA_STRING);
	else
	    SET_STRING_ELT(ans, i, mkChar(buffer));
	break;
    }
}

static SEXP scanVector(SEXPTYPE type, int maxitems, int maxlines,
		       int flush, SEXP stripwhite, int blskip)
{
    SEXP ans, bns;
    int blocksize, c, i, n, linesread, nprev,strip, bch;
    char buffer[MAXELTSIZE];

    if (maxitems > 0) blocksize = maxitems;
    else blocksize = SCAN_BLOCKSIZE;

    PROTECT(ans = allocVector(type, blocksize));

    nprev = 0; n = 0; linesread = 0; bch = 1;

    if (ttyflag) sprintf(ConsolePrompt, "1: ");

    strip = asLogical(stripwhite);

    for (;;) {
	if (bch == R_EOF) {
	    if (ttyflag) R_ClearerrConsole();
	    break;
	}
	else if (bch == '\n') {
	    linesread++;
	    if (linesread == maxlines)
		break;
	    if (ttyflag) {
		sprintf(ConsolePrompt, "%d: ", n + 1);
	    }
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
	bch = fillBuffer(buffer, type, strip);
	if (nprev == n && strlen(buffer)==0 &&
	    ((blskip && bch =='\n') || bch == R_EOF)) {
	    if (ttyflag || bch == R_EOF)
		break;
	}
	else {
	    extractItem(buffer, ans, n);
	    if (++n == maxitems) {
		if (ttyflag && bch != '\n') {
		    while ((c=scanchar()) != '\n')
			;
		}
		break;
	    }
	}
	if (flush && (bch != '\n') && (bch != R_EOF)) {
	    while ((c = scanchar()) != '\n' && (c != R_EOF));
	    bch = c;
	}
    }
    if (!quiet) REprintf("Read %d items\n", n);
    if (ttyflag) ConsolePrompt[0] = '\0';

    if (n == 0) {
	UNPROTECT(1);
	return allocVector(type,0);
    }
    if (n == maxitems) {
	UNPROTECT(1);
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
    }
    UNPROTECT(1);
    return bns;
}


static SEXP scanFrame(SEXP what, int maxitems, int maxlines, int flush,
		      int fill, SEXP stripwhite, int blskip, int multiline)
{
    SEXP ans, new, old, w;
    char buffer[MAXELTSIZE];
    int blksize, c, i, ii, j, n, nc, linesread, colsread, strip, bch;
    int badline;

    nc = length(what);
    if (!nc) {
	    if (!ttyflag & !wasopen) con->close(con);
	    error("empty `what=' specified");	
    }

    if (maxitems > 0) blksize = maxitems;
    else if (maxlines > 0) blksize = maxlines;
    else blksize = SCAN_BLOCKSIZE;

    PROTECT(ans = allocVector(VECSXP, nc));
    for (i = 0; i < nc; i++) {
	w = VECTOR_ELT(what, i);
	if (!isVector(w)) {
	    if (!ttyflag & !wasopen) con->close(con);
	    error("invalid `what=' specified");
	}
	SET_VECTOR_ELT(ans, i, allocVector(TYPEOF(w), blksize));
    }
    setAttrib(ans, R_NamesSymbol, getAttrib(what, R_NamesSymbol));

    n = 0; linesread = 0; colsread = 0; ii = 0;
    badline = 0;
    bch = 1;
    c = 0;			/* -Wall */

    if (ttyflag) sprintf(ConsolePrompt, "1: ");

    strip = asLogical(stripwhite);

    for (;;) {

	if (bch == R_EOF) {
	    if (ttyflag) R_ClearerrConsole();
	    goto done;
	}
	else if (bch == '\n') {
	    linesread++;
	    if (colsread != 0) {
		if (fill) {
		    buffer[0] = '\0';
		    for (ii = colsread; ii < nc; ii++) {
			extractItem(buffer, VECTOR_ELT(ans, ii), n);
		    }
		    n++;
		    ii = 0;
		    colsread = 0;
		} else if (!badline)
		    badline = linesread;
	    }
	    if (maxitems > 0 && n >= maxitems)
		goto done;
	    if (maxlines > 0 && linesread == maxlines)
		goto done;
	    if (ttyflag)
		sprintf(ConsolePrompt, "%d: ", n + 1);
	}
	if (n == blksize && colsread == 0) {
	    blksize = 2 * blksize;
	    for (i = 0; i < nc; i++) {
		old = VECTOR_ELT(ans, i);
		new = allocVector(TYPEOF(old), blksize);
		copyVector(new, old);
		SET_VECTOR_ELT(ans, i, new);
	    }
	}

	bch = fillBuffer(buffer, TYPEOF(VECTOR_ELT(ans, ii)), strip);
	if (colsread == 0 &&
	    strlen(buffer) == 0 &&
	    ((blskip && bch =='\n') || bch == R_EOF)) {
	    if (ttyflag || bch == R_EOF)
		break;
	}
	else {
	    extractItem(buffer, VECTOR_ELT(ans, ii), n);
	    ii++;
	    colsread++;
	    if (length(stripwhite) == length(what))
		strip = LOGICAL(stripwhite)[colsread];
	    /* increment n and reset i after filling a row */
	    if (colsread == nc) {
		n++;
		ii = 0;
		colsread = 0;
		if (flush && (bch != '\n') && (bch != R_EOF)) {
		    while ((c = scanchar()) != '\n' && c != R_EOF);
		    bch = c;
		}
		if (length(stripwhite) == length(what))
		    strip = LOGICAL(stripwhite)[0];
	    }
	}
    }

 done:
    if (badline) {
	if(multiline)
	    warning("line %d did not have %d elements", badline, nc);
	else
	    error("line %d did not have %d elements", badline, nc);
    }

    if (colsread != 0) {
	if (!fill) 
	    warning("number of items read is not a multiple of the number of columns");
	buffer[0] = '\0';	/* this is an NA */
	for (ii = colsread; ii < nc; ii++) {
	    extractItem(buffer, VECTOR_ELT(ans, ii), n);
	}
	n++;
    }
    if (!quiet) REprintf("Read %d records\n", n);
    if (ttyflag) ConsolePrompt[0] = '\0';

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
	}
	SET_VECTOR_ELT(ans, i, new);
    }
    UNPROTECT(1);
    return ans;
}

SEXP do_scan(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, file, sep, what, stripwhite, dec, quotes;
    int i, c, nlines, nmax, nskip, flush, fill, blskip, multiline;

    checkArity(op, args);

    file = CAR(args);		   args = CDR(args);
    what = CAR(args);		   args = CDR(args);
    nmax = asInteger(CAR(args));   args = CDR(args);
    sep = CAR(args);		   args = CDR(args);
    dec = CAR(args);		   args = CDR(args);
    quotes = CAR(args);		   args = CDR(args);
    nskip = asInteger(CAR(args));  args = CDR(args);
    nlines = asInteger(CAR(args)); args = CDR(args);
    NAstrings = CAR(args);	   args = CDR(args);
    flush = asLogical(CAR(args));  args = CDR(args);
    fill  = asLogical(CAR(args));  args = CDR(args);
    stripwhite = CAR(args);	   args = CDR(args);
    quiet = asLogical(CAR(args));  args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    multiline = asLogical(CAR(args));
    if (quiet == NA_LOGICAL)			quiet = 0;
    if (blskip == NA_LOGICAL)			blskip = 1;
    if (multiline == NA_LOGICAL)		multiline = 1;
    if (nskip < 0 || nskip == NA_INTEGER)	nskip = 0;
    if (nlines < 0 || nlines == NA_INTEGER)	nlines = 0;
    if (nmax < 0 || nmax == NA_INTEGER)		nmax = 0;

    if (TYPEOF(stripwhite) != LGLSXP)
	errorcall(call, "invalid strip.white value");
    if (length(stripwhite) != 1 && length(stripwhite) != length(what))
	errorcall(call, "invalid strip.white length");
    if (TYPEOF(NAstrings) != STRSXP)
	errorcall(call, "invalid na.strings value");

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) sepchar = 0;
	else sepchar = CHAR(STRING_ELT(sep, 0))[0];
    } else errorcall(call, "invalid sep value");

    if (isString(dec) || isNull(dec)) {		
	if (length(dec) == 0)
	    decchar = '.';	
	else		
	    decchar = CHAR(STRING_ELT(dec, 0))[0];
    }						
    else					
	errorcall(call, "invalid decimal separator");

    if (isString(quotes)) {
	/* This appears to be necessary to protect quoteset against GC */
	quoteset = CHAR(STRING_ELT(quotes, 0));
	quotesave = realloc(quotesave, strlen(quoteset) + 1);
	if (!quotesave)
	    errorcall(call, "out of memory");
	strcpy(quotesave, quoteset);
	quoteset = quotesave;
    } else if (isNull(quotes)) 
	quoteset = ""; 
    else
	errorcall(call, "invalid quote symbol set");


    i = asInteger(file);
    if(i == 0) {
	ttyflag = 1;
    } else {
	con = getConnection(i);
	ttyflag = 0;
	wasopen = con->isopen; 
	if(!wasopen) {
	    strcpy(con->mode, "r");
	    con->open(con);
	}
	for (i = 0; i < nskip; i++)
	    while ((c = scanchar()) != '\n' && c != R_EOF);
    }

    ans = R_NilValue;		/* -Wall */
    save = 0;

    switch (TYPEOF(what)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
	ans = scanVector(TYPEOF(what), nmax, nlines, flush, stripwhite, 
			 blskip);
	break;

    case VECSXP:
	ans = scanFrame(what, nmax, nlines, flush, fill, stripwhite, 
			blskip, multiline);
	break;
    default:
	if (!ttyflag && !wasopen)
	    con->close(con);
	errorcall(call, "invalid \"what=\" specified");
    }
    if (!ttyflag && !wasopen)
	con->close(con);
    return ans;
}

SEXP do_countfields(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, file, sep,  bns, quotes;
    int nfields, nskip, i, c, inquote, quote = 0;
    int blocksize, nlines, blskip;

    checkArity(op, args);

    file = CAR(args);	args = CDR(args);
    sep = CAR(args);	args = CDR(args);
    quotes = CAR(args);	 args = CDR(args);
    nskip = asInteger(CAR(args));  args = CDR(args);
    blskip = asLogical(CAR(args));

    if (nskip < 0 || nskip == NA_INTEGER) nskip = 0;
    if (blskip == NA_LOGICAL) blskip = 1;

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) sepchar = 0;
	else sepchar = CHAR(STRING_ELT(sep, 0))[0];
    } else errorcall(call, "invalid sep value");

    if (isString(quotes)) {
	/* This appears to be necessary to protect quoteset against GC */
	quoteset = CHAR(STRING_ELT(quotes, 0));
	quotesave = realloc(quotesave, strlen(quoteset) + 1);
	if (!quotesave)
	    errorcall(call, "out of memory");
	strcpy(quotesave, quoteset);
	quoteset = quotesave;
    } else if (isNull(quotes)) 
	quoteset = ""; 
    else
	errorcall(call, "invalid quote symbol set");

    i = asInteger(file);
    if(i == 0) {
	ttyflag = 1;
    } else {
	con = getConnection(i);
	ttyflag = 0;
	wasopen = con->isopen; 
	if(!wasopen) {
	    strcpy(con->mode, "r");
	    con->open(con);
	}
	for (i = 0; i < nskip; i++)
	    while ((c = scanchar()) != '\n' && c != R_EOF);
    }

    blocksize = SCAN_BLOCKSIZE;
    PROTECT(ans = allocVector(INTSXP, blocksize));
    nlines = 0;
    nfields = 0;
    inquote = 0;

    save = 0;

    for (;;) {
	c = scanchar();
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
	else if (sepchar) {
	    if (nfields == 0)
		nfields++;
	    if (inquote && (c == R_EOF || c == '\n')) {
		if(!wasopen) con->close(con);
		errorcall(call, "string terminated by newline or EOF");
	    }
	    if (inquote && c == quote)
		inquote = 0;
	    else if (strchr(quoteset, c)) {
		inquote = 1;
		quote = c;
	    }
	    if (c == sepchar && !inquote)
		nfields++;
	}
	else if (!isspace(c)) {
	    if (strchr(quoteset, c)) {
		quote = c;
		while ((c=scanchar()) != quote) {
		    if (c == R_EOF || c == '\n') {
			if(!wasopen) con->close(con);
			errorcall(call, "string terminated by newline or EOF");
		    }
		}
	    }
	    else {
		while (!isspace(c=scanchar()) && c != R_EOF)
		    ;
		if (c == R_EOF) c='\n';
		unscanchar(c);
	    }
	    nfields++;
	}

    }
 donecf:
    if(!wasopen) con->close(con);

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
    return bns;
}

/* frame.convert(char, na.strings, as.is) */

/* This is a horrible hack which is used in read.table to take a */
/* character variable, if possible to convert it to a numeric */
/* variable.  If this is not possible, the result is a character */
/* string if as.is == TRUE or a factor if as.is == FALSE. */


SEXP do_typecvt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP cvec, a, rval, dup, levs, dims, names, dec;
    int i, j, len, numeric, asIs;
    char *endp, *tmp;

    checkArity(op,args);

    if (!isString(CAR(args)))
	errorcall(call,"the first argument must be of mode character");

    NAstrings = CADR(args);
    if (TYPEOF(NAstrings) != STRSXP)
	errorcall(call, "invalid na.strings value");

    asIs = asLogical(CADDR(args));
    if (asIs == NA_LOGICAL) asIs = 0;

    dec = CADDDR(args);

    if (isString(dec) || isNull(dec)) {		
	if (length(dec) == 0)
	    decchar = '.';	
	else		
	    decchar = CHAR(STRING_ELT(dec, 0))[0];
    }

    cvec = CAR(args);
    len = length(cvec);

    numeric = 1;

    /* save the dim/dimname attributes */

    PROTECT(dims = getAttrib(cvec, R_DimSymbol));
    if (isArray(cvec))
	PROTECT(names = getAttrib(cvec, R_DimNamesSymbol));
    else
	PROTECT(names = getAttrib(cvec, R_NamesSymbol));

    PROTECT(rval = allocVector(REALSXP, length(cvec)));
    for (i = 0; i < len; i++) {
	tmp = CHAR(STRING_ELT(cvec, i));
	if (strlen(tmp) == 0 || isNAstring(tmp, 1) || isBlankString(tmp))
	    REAL(rval)[i] = NA_REAL;
	else {
	    REAL(rval)[i] = Strtod(tmp, &endp);
	    if (!isBlankString(endp)) {
		numeric = 0;
		break;
	    }
	}
    }
    if (!numeric) {
	if (asIs) {
	    rval = cvec;
	}
	else {
	    PROTECT(rval = allocVector(INTSXP,length(cvec)));
	    PROTECT(dup = duplicated(cvec));
	    j = 0;
	    for (i = 0; i < len; i++)
		if (LOGICAL(dup)[i] == 0 && !isNAstring(CHAR(STRING_ELT(cvec, i)), 1))
		    j++;
	    PROTECT(levs = allocVector(STRSXP,j));
	    j = 0;
	    for (i = 0; i < len; i++)
		if (LOGICAL(dup)[i] == 0 && !isNAstring(CHAR(STRING_ELT(cvec, i)), 1))
		    SET_STRING_ELT(levs, j++, STRING_ELT(cvec, i));

	    /* put the levels in lexicographic order */

	    sortVector(levs);

	    PROTECT(a = match(levs, cvec, NA_INTEGER));
	    for (i = 0; i < len; i++)
		INTEGER(rval)[i] = INTEGER(a)[i];

	    setAttrib(rval, R_LevelsSymbol, levs);
	    PROTECT(a = allocVector(STRSXP, 1));
	    SET_STRING_ELT(a, 0, mkChar("factor"));
	    setAttrib(rval, R_ClassSymbol, a);
	    UNPROTECT(5);
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

SEXP do_readln(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	    strncpy(ConsolePrompt, CHAR(STRING_ELT(prompt, 0)),
		CONSOLE_PROMPT_SIZE - 1);
    }

    /* skip white space */
    while ((c= ConsoleGetchar()) == ' ' || c=='\t');
    if (c != '\n' && c != R_EOF) {
	*bufp++ = c;
	while ((c = ConsoleGetchar())!= '\n' && c != R_EOF) {
	    if (bufp >= &buffer[MAXELTSIZE - 2])
		continue;
	    *bufp++ = c;
	}
    }
    /* now strip white space off the end as well */
    while (--bufp >= buffer && isspace((int)*bufp))
	;
    *++bufp = '\0';
    ConsolePrompt[0] = '\0';

    PROTECT(ans = allocVector(STRSXP,1));
    SET_STRING_ELT(ans, 0, mkChar(buffer));
    UNPROTECT(2);
    return ans;
}

SEXP do_menu(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int c, j;
    double first;
    char buffer[MAXELTSIZE], *bufp = buffer;
    SEXP ans;

    checkArity(op,args);

    if (!isString(CAR(args)))
	errorcall(call,"wrong argument");

    sprintf(ConsolePrompt, "Selection: ");

    while ((c = ConsoleGetchar()) != '\n' && c != R_EOF) {
	if (bufp >= &buffer[MAXELTSIZE - 2])
	    continue;
	*bufp++ = c;
    }
    *bufp++ = '\0';
    ConsolePrompt[0] = '\0';

    bufp = buffer;
    while (isspace((int)*bufp)) bufp++;
    first = LENGTH(CAR(args)) + 1;
    if (isdigit((int)*bufp)) {
	first = Strtod(buffer, NULL);
    }
    else {
	for (j = 0; j < LENGTH(CAR(args)); j++) {
	    if (streql(CHAR(STRING_ELT(CAR(args), j)), buffer)) {
		first = j + 1;
		break;
	    }
	}
    }
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = first;
    return ans;
}
