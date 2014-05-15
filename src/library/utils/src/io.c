/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2014   The R Core Team.
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
#include <float.h>  /* for DBL_DIG */
#include <Fileio.h>
#include <Rconnections.h>
#include <errno.h>
#include <Print.h>

#include <rlocale.h> /* for btowc */

#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("utils", String)
#else
#define _(String) (String)
#endif


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
    Rboolean skipNul;
    char convbuf[100];
} LocalData;

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

// ../../../main/util.c
extern double R_strtod5(const char *str, char **endptr, char dec,
			Rboolean NA, Rboolean exact);

static double
Strtod (const char *nptr, char **endptr, Rboolean NA, LocalData *d, int i_exact)
{
    return R_strtod5(nptr, endptr, d->decchar, NA, i_exact);
}

static Rcomplex
strtoc(const char *nptr, char **endptr, Rboolean NA, LocalData *d, int i_exact)
{
    Rcomplex z;
    double x, y;
    char *s, *endp;

    x = Strtod(nptr, &endp, NA, d, i_exact);
    if (isBlankString(endp)) {
	z.r = x; z.i = 0;
    } else if (*endp == 'i')  {
	z.r = 0; z.i = x;
	endp++;
    } else {
	s = endp;
	y = Strtod(s, &endp, NA, d, i_exact);
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


static R_INLINE int scanchar_raw(LocalData *d)
{
    int c = (d->ttyflag) ? ConsoleGetcharWithPushBack(d->con) :
	Rconn_fgetc(d->con);
    if(c == 0) {
	if(d->skipNul) {
	    do {
		c = (d->ttyflag) ? ConsoleGetcharWithPushBack(d->con) :
		    Rconn_fgetc(d->con);
	    } while(c == 0);
	}
    }
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


#include "RBufferUtils.h"


SEXP countfields(SEXP args)
{
    SEXP ans, file, sep,  bns, quotes, comstr;
    int nfields, nskip, i, c, inquote, quote = 0;
    int blocksize, nlines, blskip;
    const char *p;
    Rboolean dbcslocale = (MB_CUR_MAX == 2);
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE,	 FALSE};
    data.NAstrings = R_NilValue;

    args = CDR(args);

    file = CAR(args);	args = CDR(args);
    sep = CAR(args);	args = CDR(args);
    quotes = CAR(args);	 args = CDR(args);
    nskip = asInteger(CAR(args));  args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    comstr = CAR(args);
    if (TYPEOF(comstr) != STRSXP || length(comstr) != 1)
	error(_("invalid '%s' argument"), "comment.char");
    p = translateChar(STRING_ELT(comstr, 0));
    data.comchar = NO_COMCHAR; /*  here for -Wall */
    if (strlen(p) > 1)
	error(_("invalid '%s' argument"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (unsigned char)*p;

    if (nskip < 0 || nskip == NA_INTEGER) nskip = 0;
    if (blskip == NA_LOGICAL) blskip = 1;

    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else data.sepchar = (unsigned char) translateChar(STRING_ELT(sep, 0))[0];
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' argument"), "sep");

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
	    if (inquote) {
	    	INTEGER(ans)[nlines] = NA_INTEGER;
	    	nlines++;
	    } else if (nfields || !blskip) {
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
	    if (inquote && c == R_EOF) {
		if(!data.wasopen) data.con->close(data.con);
		error(_("quoted string on line %d terminated by EOF"), inquote);
	    }
	    if (inquote && c == quote)
		inquote = 0;
	    else if (strchr(data.quoteset, c)) {
		inquote = nlines + 1;
		quote = c;
	    }
	    if (c == data.sepchar && !inquote)
		nfields++;
	}
	else if (!Rspace(c)) {
	    if (strchr(data.quoteset, c)) {
		quote = c;
		inquote = nlines + 1;
		while ((c = scanchar(inquote, &data)) != quote) {
		    if (c == R_EOF) {
			if(!data.wasopen) data.con->close(data.con);
		        error(_("quoted string on line %d terminated by EOF"), inquote);
		    } else if (c == '\n') {
		        INTEGER(ans)[nlines] = NA_INTEGER;
		        nlines++;
		        if (nlines == blocksize) {
			    bns = ans;
			    blocksize = 2 * blocksize;
			    ans = allocVector(INTSXP, blocksize);
			    UNPROTECT(1);
			    PROTECT(ans);
			    copyVector(ans, bns);
	    		}
		    }
		}
		inquote = 0;
	    } else {
		do {
		    if(dbcslocale && btowc(c) == WEOF) scanchar2(&data);
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
	line[0] = (char) data.save;
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

/* A struct used by typeconvert to keep track of possible types for the input */
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
static void ruleout_types(const char *s, Typecvt_Info *typeInfo, LocalData *data,
			  Rboolean exact)
{
    int res;
    char *endp;

    if (typeInfo->islogical) {
	if (strcmp(s, "F") == 0 || strcmp(s, "T") == 0 ||
	    strcmp(s, "FALSE") == 0 || strcmp(s, "TRUE") == 0) {
	    typeInfo->isinteger = FALSE;
	    typeInfo->isreal = FALSE;
	    typeInfo->iscomplex = FALSE;
	    return; // short cut
	} else {
	    typeInfo->islogical = FALSE;
	}
    }

    if (typeInfo->isinteger) {
	res = Strtoi(s, 10);
	if (res == NA_INTEGER)
	    typeInfo->isinteger = FALSE;
    }

    if (typeInfo->isreal) {
	Strtod(s, &endp, TRUE, data, exact);
	if (!isBlankString(endp))
	    typeInfo->isreal = FALSE;
    }

    if (typeInfo->iscomplex) {
	strtoc(s, &endp, TRUE, data, exact);
	if (!isBlankString(endp))
	    typeInfo->iscomplex = FALSE;
    }
}


/* type.convert(char, na.strings, as.is, dec, numerals) */

/* This is a horrible hack which is used in read.table to take a
   character variable, if possible to convert it to a logical,
   integer, numeric or complex variable.  If this is not possible,
   the result is a character string if as.is == TRUE
   or a factor if as.is == FALSE. */


SEXP typeconvert(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP cvec, a, dup, levs, dims, names, dec, numerals;
    SEXP rval = R_NilValue; /* -Wall */
    int i, j, len, asIs, i_exact;
    Rboolean done = FALSE, exact;
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

    args = CDR(args);

    if (!isString(CAR(args)))
	error(_("the first argument must be of mode character"));

    data.NAstrings = CADR(args);
    if (TYPEOF(data.NAstrings) != STRSXP)
	error(_("invalid '%s' argument"), "na.strings");

    asIs = asLogical(CADDR(args));
    if (asIs == NA_LOGICAL) asIs = 0;

    dec = CADDDR(args);
    if (isString(dec) || isNull(dec)) {
	if (length(dec) == 0)
	    data.decchar = '.';
	else
	    data.decchar = translateChar(STRING_ELT(dec, 0))[0];
    }

    numerals = CAD4R(args); // string, one of c("allow.loss", "warn.loss", "no.loss")
    if (isString(numerals)) {
	tmp = CHAR(STRING_ELT(numerals, 0));
	if(strcmp(tmp, "allow.loss") == 0) {
	    i_exact = FALSE;
	    exact = FALSE;
	} else if(strcmp(tmp, "warn.loss") == 0) {
	    i_exact = NA_INTEGER;
	    exact = FALSE;
	} else if(strcmp(tmp, "no.loss") == 0) {
	    i_exact = TRUE;
	    exact = TRUE;
	} else // should never happen
	    error(_("invalid 'numerals' string: \"%s\""), tmp);

    } else { // (currently never happens): use default
	i_exact = FALSE;
	exact = FALSE;
    }

    cvec = CAR(args);
    len = length(cvec);

    /* save the dim/dimnames attributes */

    PROTECT(dims = getAttrib(cvec, R_DimSymbol));
    if (isArray(cvec))
	PROTECT(names = getAttrib(cvec, R_DimNamesSymbol));
    else
	PROTECT(names = getAttrib(cvec, R_NamesSymbol));

    /* Find the first non-NA entry (empty => NA) */
    for (i = 0; i < len; i++) {
	tmp = CHAR(STRING_ELT(cvec, i));
	if (!(STRING_ELT(cvec, i) == NA_STRING || strlen(tmp) == 0
	      || isNAstring(tmp, 1, &data) || isBlankString(tmp)))
	    break;
    }
    if (i < len) { // Found non-NA entry; use it to screen:
	ruleout_types(tmp, &typeInfo, &data, exact);
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
		    ruleout_types(tmp, &typeInfo, &data, exact);
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
		    ruleout_types(tmp, &typeInfo, &data, exact);
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
		REAL(rval)[i] = Strtod(tmp, &endp, FALSE, &data, i_exact);
		if (!isBlankString(endp)) {
		    typeInfo.isreal = FALSE;
		    ruleout_types(tmp, &typeInfo, &data, exact);
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
		COMPLEX(rval)[i] = strtoc(tmp, &endp, FALSE, &data, i_exact);
		if (!isBlankString(endp)) {
		    typeInfo.iscomplex = FALSE;
		    /* this is not needed, unless other cases are added */
		    ruleout_types(tmp, &typeInfo, &data, exact);
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

	    PROTECT(a = matchE(levs, cvec, NA_INTEGER, env));
	    for (i = 0; i < len; i++)
		INTEGER(rval)[i] = INTEGER(a)[i];

	    setAttrib(rval, R_LevelsSymbol, levs);
	    PROTECT(a = mkString("factor"));
	    setAttrib(rval, R_ClassSymbol, a);
	    UNPROTECT(3);
	}
    }

    setAttrib(rval, R_DimSymbol, dims);
    setAttrib(rval, isArray(cvec) ? R_DimNamesSymbol : R_NamesSymbol, names);
    UNPROTECT(3);
    return rval;
}


/* Works with digits, but OK in UTF-8 */
SEXP menu(SEXP choices)
{
    int c, j;
    double first;
    char buffer[MAXELTSIZE], *bufp = buffer;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE};
    data.NAstrings = R_NilValue;


    if (!isString(choices))
	error(_("invalid '%s' argument"), "choices");

    snprintf(ConsolePrompt, CONSOLE_PROMPT_SIZE, _("Selection: "));

    while ((c = ConsoleGetchar()) != '\n' && c != R_EOF) {
	if (bufp >= &buffer[MAXELTSIZE - 2]) continue;
	*bufp++ = (char) c;
    }
    *bufp++ = '\0';
    ConsolePrompt[0] = '\0';

    bufp = buffer;
    while (Rspace((int)*bufp)) bufp++;
    first = LENGTH(choices) + 1;
    if (isdigit((int)*bufp)) {
	first = Strtod(buffer, NULL, TRUE, &data, /*exact*/FALSE);
    } else {
	for (j = 0; j < LENGTH(choices); j++) {
	    if (streql(translateChar(STRING_ELT(choices, j)), buffer)) {
		first = j + 1;
		break;
	    }
	}
    }
    return ScalarInteger((int)first);
}

/* readTableHead(file, nlines, comment.char, blank.lines.skip, quote, sep) */
/* simplified version of readLines, with skip of blank lines and
   comment-only lines */
#define BUF_SIZE 1000
SEXP readtablehead(SEXP args)
{
    SEXP file, comstr, ans = R_NilValue, ans2, quotes, sep;
    int nlines, i, c, quote = 0, nread, nbuf, buf_size = BUF_SIZE,
	blskip, skipNul;
    const char *p; char *buf;
    Rboolean empty, skip, firstnonwhite;
    LocalData data = {NULL, 0, 0, '.', NULL, NO_COMCHAR, 0, NULL, FALSE,
		      FALSE, 0, FALSE, FALSE, FALSE};
    data.NAstrings = R_NilValue;

    args = CDR(args);

    file = CAR(args);		   args = CDR(args);
    nlines = asInteger(CAR(args)); args = CDR(args);
    comstr = CAR(args);		   args = CDR(args);
    blskip = asLogical(CAR(args)); args = CDR(args);
    quotes = CAR(args);		   args = CDR(args);
    sep = CAR(args);		   args = CDR(args);
    skipNul = asLogical(CAR(args));

    if (nlines <= 0 || nlines == NA_INTEGER)
	error(_("invalid '%s' argument"), "nlines");
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
	error(_("invalid '%s' argument"), "comment.char");
    p = translateChar(STRING_ELT(comstr, 0));
    data.comchar = NO_COMCHAR; /*  here for -Wall */
    if (strlen(p) > 1)
	error(_("invalid '%s' argument"), "comment.char");
    else if (strlen(p) == 1) data.comchar = (int)*p;
    if (isString(sep) || isNull(sep)) {
	if (length(sep) == 0) data.sepchar = 0;
	else data.sepchar = (unsigned char) translateChar(STRING_ELT(sep, 0))[0];
	/* gets compared to chars: bug prior to 1.7.0 */
    } else error(_("invalid '%s' argument"), "sep");
    if (skipNul == NA_LOGICAL) error(_("invalid '%s' argument"), "skipNul");
    data.skipNul = skipNul;

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
	nbuf = 0; empty = TRUE; skip = FALSE; firstnonwhite = TRUE;
	if (data.ttyflag)
	    snprintf(ConsolePrompt, CONSOLE_PROMPT_SIZE, "%d: ", nread);
	/* want to interpret comments here, not in scanchar */
	while((c = scanchar(TRUE, &data)) != R_EOF) {
	    if(nbuf >= buf_size -1) {
		buf_size *= 2;
		char *tmp = (char *) realloc(buf, buf_size);
		if(!tmp) {
		    free(buf);
		    error(_("cannot allocate buffer in 'readTableHead'"));
		} else buf = tmp;
	    }
	    /* Need to handle escaped embedded quotes, and how they are
	       escaped depends on 'sep' */
	    if(quote) {
		if(data.sepchar == 0 && c == '\\') {
		    /* all escapes should be passed through */
		    buf[nbuf++] = (char) c;
		    c = scanchar(TRUE, &data);
		    if(c == R_EOF)
			error(_("\\ followed by EOF"));
		    buf[nbuf++] = (char) c;
		    continue;
		} else if(quote && c == quote) {
		    if(data.sepchar == 0)
			quote = 0;
		    else { /* need to check for doubled quote */
			char c2 = (char) scanchar(TRUE, &data);
			if(c2 == quote)
			    buf[nbuf++] = (char) c; /* and c = c2 */
			else {
			    unscanchar(c2, &data);
			    quote = 0;
			}
		    }
		}
	    } else if(!skip && firstnonwhite && strchr(data.quoteset, c)) quote = c;
	    else if (Rspace(c) || c == data.sepchar) firstnonwhite = TRUE;
	    else firstnonwhite = FALSE;
	    /* A line is empty only if it contains nothing before
	       EOL, EOF or a comment char.
	       A line containing just white space is not empty if sep=","
	       However foo\nEOF does not have a final empty line.
	    */
	    if(empty && !skip)
		if(c != '\n' && c != data.comchar) empty = FALSE;
	    if(!quote && !skip && c == data.comchar) skip = TRUE;
	    if(quote || c != '\n') buf[nbuf++] = (char) c; else break;
	}
	buf[nbuf] = '\0';
	if(data.ttyflag && empty) goto no_more_lines;
	if(!empty || (c != R_EOF && !blskip)) { /* see previous comment */
	    SET_STRING_ELT(ans, nread, mkChar(buf));
	    nread++;
	    if (strlen(buf) < nbuf) // PR#15625
		warning("line %d appears to contain embedded nulls", nread);
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

/* a version of EncodeElement with different escaping of char strings */
static const char
*EncodeElement2(SEXP x, int indx, Rboolean quote,
		Rboolean qmethod, R_StringBuffer *buff, char cdec)
{
    int nbuf;
    char *q;
    const char *p, *p0;

    if (indx < 0 || indx >= length(x))
	error(_("index out of range"));
    if(TYPEOF(x) == STRSXP) {
	const void *vmax = vmaxget();
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
	vmaxset(vmax);
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

SEXP writetable(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sep, rnames, eol, na, dec, quote, xj;
    Rboolean wasopen, quote_rn = FALSE, *quote_col;
    Rconnection con;
    const char *csep, *ceol, *cna, *sdec, *tmp = NULL /* -Wall */;
    char cdec;
    SEXP *levels;
    R_StringBuffer strBuf = {NULL, 0, MAXELTSIZE};
    wt_info wi;
    RCNTXT cntxt;

    args = CDR(args);

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
    int nr = asInteger(CAR(args)); args = CDR(args);
    int nc = asInteger(CAR(args)); args = CDR(args);
    rnames = CAR(args);		   args = CDR(args);
    sep = CAR(args);		   args = CDR(args);
    eol = CAR(args);		   args = CDR(args);
    na = CAR(args);		   args = CDR(args);
    dec = CAR(args);		   args = CDR(args);
    quote = CAR(args);		   args = CDR(args);
    int qmethod = asLogical(CAR(args));

    if(nr == NA_INTEGER) error(_("invalid '%s' argument"), "nr");
    if(nc == NA_INTEGER) error(_("invalid '%s' argument"), "nc");
    if(!isNull(rnames) && !isString(rnames))
	error(_("invalid '%s' argument"), "rnames");
    if(!isString(sep)) error(_("invalid '%s' argument"), "sep");
    if(!isString(eol)) error(_("invalid '%s' argument"), "eol");
    if(!isString(na)) error(_("invalid '%s' argument"), "na");
    if(!isString(dec)) error(_("invalid '%s' argument"), "dec");
    if(qmethod == NA_LOGICAL) error(_("invalid '%s' argument"), "qmethod");
    csep = translateChar(STRING_ELT(sep, 0));
    ceol = translateChar(STRING_ELT(eol, 0));
    cna = translateChar(STRING_ELT(na, 0));
    sdec = translateChar(STRING_ELT(dec, 0));
    if(strlen(sdec) != 1)
	error(_("'dec' must be a single character"));
    cdec = sdec[0];
    quote_col = (Rboolean *) R_alloc(nc, sizeof(Rboolean));
    for(int j = 0; j < nc; j++) quote_col[j] = FALSE;
    for(int i = 0; i < length(quote); i++) { /* NB, quote might be NULL */
	int this = INTEGER(quote)[i];
	if(this == 0) quote_rn = TRUE;
	if(this >  0) quote_col[this - 1] = TRUE;
    }
    R_AllocStringBuffer(0, &strBuf);
    PrintDefaults();
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
	for(int j = 0; j < nc; j++) {
	    xj = VECTOR_ELT(x, j);
	    if(LENGTH(xj) != nr)
		error(_("corrupt data frame -- length of column %d does not not match nrows"),
		      j+1);
	    if(inherits(xj, "factor")) {
		levels[j] = getAttrib(xj, R_LevelsSymbol);
	    } else levels[j] = R_NilValue;
	}

	for(int i = 0; i < nr; i++) {
	    if(i % 1000 == 999) R_CheckUserInterrupt();
	    if(!isNull(rnames))
		Rconn_printf(con, "%s%s",
			     EncodeElement2(rnames, i, quote_rn, qmethod,
					    &strBuf, cdec), csep);
	    for(int j = 0; j < nc; j++) {
		xj = VECTOR_ELT(x, j);
		if(j > 0) Rconn_printf(con, "%s", csep);
		if(isna(xj, i)) tmp = cna;
		else {
		    if(!isNull(levels[j])) {
			/* We do not assume factors have integer levels,
			   although they should. */
			if(TYPEOF(xj) == INTSXP)
			    tmp = EncodeElement2(levels[j], INTEGER(xj)[i] - 1,
						 quote_col[j], qmethod,
						 &strBuf, cdec);
			else if(TYPEOF(xj) == REALSXP)
			    tmp = EncodeElement2(levels[j],
						 (int) (REAL(xj)[i] - 1),
						 quote_col[j], qmethod,
						 &strBuf, cdec);
			else
			    error(_("column %s claims to be a factor but does not have numeric codes"),
				  j+1);
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
	if(XLENGTH(x) != (R_len_t)nr * nc)
	    error(_("corrupt matrix -- dims not not match length"));

	for(int i = 0; i < nr; i++) {
	    if(i % 1000 == 999) R_CheckUserInterrupt();
	    if(!isNull(rnames))
		Rconn_printf(con, "%s%s",
			     EncodeElement2(rnames, i, quote_rn, qmethod,
					    &strBuf, cdec), csep);
	    for(int j = 0; j < nc; j++) {
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
