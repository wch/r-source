/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999--2007  The R Development Core Team
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

   char here is either ASCII or handled as a whole, apart from Rstrlen
   and EncodeString.

   Octal representation of strings replaced by \u+4/8hex (can that be
   improved?).
*/


/* =========
 * Printing:
 * =========
 *
 * All printing in R is done via the functions Rprintf and REprintf
 * or their (v) versions Rvprintf and REvprintf.
 * These routines work exactly like (v)printf(3).  Rprintf writes to
 * ``standard output''.	 It is redirected by the sink() function,
 * and is suitable for ordinary output.	 REprintf writes to
 * ``standard error'' and is useful for error messages and warnings.
 * It is not redirected by sink().
 *
 *  See ./format.c  for the  format_FOO_  functions which provide
 *	~~~~~~~~~~  the	 length, width, etc.. that are used here.
 *  See ./print.c  for do_printdefault, do_prmatrix, etc.
 *
 *
 * Here, the following UTILITIES are provided:
 *
 * The utilities EncodeLogical, EncodeFactor, EncodeInteger, EncodeReal
 * and EncodeString can be used to convert R objects to a form suitable
 * for printing.  These print the values passed in a formatted form
 * or, in the case of NA values, an NA indicator.  EncodeString takes
 * care of printing all the standard ANSI escapes \a, \t \n etc.
 * so that these appear in their backslash form in the string.	There
 * is also a routine called Rstrlen which computes the length of the
 * string in its escaped rather than literal form.
 *
 * Finally there is a routine called EncodeElement which will encode
 * a single R-vector element.  This is used in deparse and write.table.
 */

/* if ESC_BARE_QUOTE is defined, " in an unquoted string is replaced
   by \".  " in a quoted string is always replaced by \". */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Print.h>
#include <R_ext/RS.h>
#include <Rconnections.h>

#include "RBufferUtils.h"

extern int R_OutputCon; /* from connections.c */


#define BUFSIZE 8192  /* used by Rprintf etc */

/* Only if ierr < 0 or not is currently used */
R_size_t R_Decode2Long(char *p, int *ierr)
{
    R_size_t v = strtol(p, &p, 10);
    *ierr = 0;
    if(p[0] == '\0') return v;
    /* else look for letter-code ending : */
    if(R_Verbose)
	REprintf("R_Decode2Long(): v=%ld\n", v);
    if(p[0] == 'G') {
	if((Giga * (double)v) > R_SIZE_T_MAX) { *ierr = 4; return(v); }
	return (Giga*v);
    }
    else if(p[0] == 'M') {
	if((Mega * (double)v) > R_SIZE_T_MAX) { *ierr = 1; return(v); }
	return (Mega*v);
    }
    else if(p[0] == 'K') {
	if((1024 * (double)v) > R_SIZE_T_MAX) { *ierr = 2; return(v); }
	return (1024*v);
    }
    else if(p[0] == 'k') {
	if((1000 * (double)v) > R_SIZE_T_MAX) { *ierr = 3; return(v); }
	return (1000*v);
    }
    else {
	*ierr = -1;
	return(v);
    }
}

/* There is no documented (or enforced) limit on 'w' here,
   so use snprintf */
#define NB 1000
char *EncodeLogical(int x, int w)
{
    static char buff[NB];
    if(x == NA_LOGICAL) snprintf(buff, NB, "%*s", w, CHAR(R_print.na_string));
    else if(x) snprintf(buff, NB, "%*s", w, "TRUE");
    else snprintf(buff, NB, "%*s", w, "FALSE");
    buff[NB-1] = '\0';
    return buff;
}

char *EncodeInteger(int x, int w)
{
    static char buff[NB];
    if(x == NA_INTEGER) snprintf(buff, NB, "%*s", w, CHAR(R_print.na_string));
    else snprintf(buff, NB, "%*d", w, x);
    buff[NB-1] = '\0';
    return buff;
}

char *EncodeRaw(Rbyte x)
{
    static char buff[10];
    sprintf(buff, "%02x", x);
    return buff;
}

char *EncodeReal(double x, int w, int d, int e, char cdec)
{
    static char buff[NB];
    char *p, fmt[20];

    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x)) snprintf(buff, NB, "%*s", w, CHAR(R_print.na_string));
	else if(ISNAN(x)) snprintf(buff, NB, "%*s", w, "NaN");
	else if(x > 0) snprintf(buff, NB, "%*s", w, "Inf");
	else snprintf(buff, NB, "%*s", w, "-Inf");
    }
    else if (e) {
	if(d) {
	    sprintf(fmt,"%%#%d.%de", w, d);
	    snprintf(buff, NB, fmt, x);
	}
	else {
	    sprintf(fmt,"%%%d.%de", w, d);
	    snprintf(buff, NB, fmt, x);
	}
    }
    else { /* e = 0 */
	sprintf(fmt,"%%%d.%df", w, d);
	snprintf(buff, NB, fmt, x);
    }
    buff[NB-1] = '\0';

    if(cdec != '.')
      for(p = buff; *p; p++) if(*p == '.') *p = cdec;

    return buff;
}

char *EncodeReal2(double x, int w, int d, int e)
{
    static char buff[NB];
    char fmt[20];

    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x)) snprintf(buff, NB, "%*s", w, CHAR(R_print.na_string));
	else if(ISNAN(x)) snprintf(buff, NB, "%*s", w, "NaN");
	else if(x > 0) snprintf(buff, NB, "%*s", w, "Inf");
	else snprintf(buff, NB, "%*s", w, "-Inf");
    }
    else if (e) {
	if(d) {
	    sprintf(fmt,"%%#%d.%de", w, d);
	    snprintf(buff, NB, fmt, x);
	}
	else {
	    sprintf(fmt,"%%%d.%de", w, d);
	    snprintf(buff, NB, fmt, x);
	}
    }
    else { /* e = 0 */
	sprintf(fmt,"%%#%d.%df", w, d);
	snprintf(buff, NB, fmt, x);
    }
    buff[NB-1] = '\0';
    return buff;
}

void z_prec_r(Rcomplex *r, Rcomplex *x, double digits);

char *EncodeComplex(Rcomplex x, int wr, int dr, int er, int wi, int di, int ei,
		    char cdec)
{
    static char buff[NB];
    char Re[NB], *Im, *tmp;
    int  flagNegIm = 0;
    Rcomplex y;

    /* IEEE allows signed zeros; strip these here */
    if (x.r == 0.0) x.r = 0.0;
    if (x.i == 0.0) x.i = 0.0;

    if (ISNA(x.r) || ISNA(x.i)) {
	snprintf(buff, NB, "%*s%*s", R_print.gap, "", wr+wi+2,
		CHAR(R_print.na_string));
    } else {
	/* formatComplex rounded, but this does not, and we need to
	   keep it that way so we don't get strange trailing zeros.
	   But we do want to avoid printing small exponentials that
	   are probably garbage.
	 */
	z_prec_r(&y, &x, R_print.digits);
	/* EncodeReal has static buffer, so copy */
	if(y.r == 0.) tmp = EncodeReal(y.r, wr, dr, er, cdec);
	else tmp = EncodeReal(x.r, wr, dr, er, cdec);
	strcpy(Re, tmp);
	if ( (flagNegIm = (x.i < 0)) ) x.i = -x.i;
	if(y.i == 0.) Im = EncodeReal(y.i, wi, di, ei, cdec);
	else Im = EncodeReal(x.i, wi, di, ei, cdec);
	snprintf(buff, NB, "%s%s%si", Re, flagNegIm ? "-" : "+", Im);
    }
    buff[NB-1] = '\0';
    return buff;
}

/* <FIXME>
   encodeString and Rstrwid assume that the wchar_t representation
   used to hold multibyte chars is Unicode.  This is usually true, and
   we warn if it is not known to be true.  Potentially looking at
   wchar_t ranges as we do is incorrect, but that is even less likely to
   be problematic.

   On Windows with surrogate pairs it will not be canonical, but AFAIK
   they do not occur in any MBCS (so it would only matter if we implement
   UTF-8).
*/

#ifdef SUPPORT_MBCS
# include <R_ext/rlocale.h>
#include <wchar.h>
#include <wctype.h>
#endif
/* strlen() using escaped rather than literal form,
   and allowing for embedded nuls.
   In MBCS locales it works in characters, and reports in display width.
 */
int Rstrwid(char *str, int slen, int quote)
{
    char *p = str;
    int len = 0, i;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	int res;
	mbstate_t mb_st;
	wchar_t wc;
	unsigned int k; /* not wint_t as it might be signed */

	mbs_init(&mb_st);
	for (i = 0; i < slen; i++) {
	    res = (int) mbrtowc(&wc, p, MB_CUR_MAX, NULL);
	    if(res >= 0) {
		k = wc;
		if(0x20 <= k && k < 0x7f && iswprint(wc)) {
		    switch(wc) {
		    case L'\\':
			len += 2;
			break;
		    case L'\'':
		    case L'"':
			len += (quote == *p) ? 2 : 1;
			break;
		    default:
			len++; /* assumes these are all width 1 */
			break;
		    }
		    p++;
		} else if (k < 0x80) {
		    switch(wc) {
		    case L'\a':
		    case L'\b':
		    case L'\f':
		    case L'\n':
		    case L'\r':
		    case L'\t':
		    case L'\v':
		    case L'\0':
			len += 2; break;
		    default:
			/* print in octal */
			len += 4; break;
		    }
		    p++;
		} else {
		    len += iswprint((wint_t)wc) ? Ri18n_wcwidth(wc) :
#ifdef Win32
			6;
#else
		    (k > 0xffff ? 10 : 6);
#endif
		    i += (res - 1);
		    p += res;
		}
	    } else {
		len += 4;
		p++;
	    }
	}
    } else
#endif
	for (i = 0; i < slen; i++) {
	    /* ASCII */
	    if((unsigned char) *p < 0x80) {
		if(isprint((int)*p)) {
		    switch(*p) {
		    case '\\':
			len += 2; break;
		    case '\'':
		    case '"':
			len += (quote == *p)? 2 : 1; break;
		    default:
			len++; break;
		    }
		} else switch(*p) {
		case '\a':
		case '\b':
		case '\f':
		case '\n':
		case '\r':
		case '\t':
		case '\v':
		case '\0':
		    len += 2; break;
		default:
		    /* print in octal */
		    len += 4; break;
		}
		p++;
	    } else { /* 8 bit char */
#ifdef Win32 /* It seems Windows does not know what is printable! */
		len++;
#else
		len += isprint((int)*p) ? 1 : 4;
#endif
		p++;
	    }
	}

    return len;
}

int Rstrlen(SEXP s, int quote)
{
    return Rstrwid(CHAR(s), LENGTH(s), quote);
}

/* Here w is the minimum field width
   If 'quote' is non-zero the result should be quoted (and internal quotes
   escaped and NA strings handled differently).
 */
char *EncodeString(SEXP s, int w, int quote, Rprt_adj justify)
{
    int b, b0, i, j, cnt;
    char *p, *q, buf[11];

    /* We have to do something like this as the result is returned, and
       passed on by EncodeElement -- so no way could be enduser be
       responsible for freeing it.  However, this is not thread-safe. */
    static R_StringBuffer gBuffer = {NULL, 0, BUFSIZE};
    R_StringBuffer *buffer = &gBuffer;

    if (s == NA_STRING) {
	p = quote ? CHAR(R_print.na_string) : CHAR(R_print.na_string_noquote);
	cnt = i = quote ? strlen(CHAR(R_print.na_string)) :
	    strlen(CHAR(R_print.na_string_noquote));
	quote = 0;
    } else {
	p = translateChar(s);
	if(p == CHAR(s)) {
	    i = Rstrlen(s, quote);
	    cnt = LENGTH(s);
	} else { /* drop anything after embedded nul */
	    cnt = strlen(p);
	    i = Rstrwid(p, cnt, quote);
	}
    }

    /* We need enough space for the encoded string, including escapes.
       Octal encoding turns one byte into four.
       Unicode encoding can turn a multibyte into six or ten,
       but it turns 2/3 into 6, and 4 (and perhaps 5/6) into 10.
       Let's be wasteful here (the worst case appears to be an MBCS with
       two bytes for an upper-plane Unicode point output as ten bytes).
     */
    R_AllocStringBuffer(imax2(5*cnt+2, w), buffer); /* +2 allows for quotes */
    q = buffer->data;
    b = w - i - (quote ? 2 : 0); /* total amount of padding */
    if(justify == Rprt_adj_none) b = 0;
    if(b > 0 && justify != Rprt_adj_left) {
	b0 = (justify == Rprt_adj_centre) ? b/2 : b;
	for(i = 0 ; i < b0 ; i++) *q++ = ' ';
	b -= b0;
    }
    if(quote) *q++ = quote;
#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	int j, res;
	mbstate_t mb_st;
	wchar_t wc;
	unsigned int k; /* not wint_t as it might be signed */
        Rboolean Unicode_warning=FALSE;

	mbs_init(&mb_st);
	for (i = 0; i < cnt; i++) {
	    res = (int) mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
	    if(res >= 0) { /* res = 0 is a terminator */
		k = wc;
		/* To be portable, treat \0 explicitly */
		if(res == 0) {k = 0; wc = L'\0';}
		if(0x20 <= k && k < 0x7f && iswprint(wc)) {
		    switch(wc) {
		    case L'\\': *q++ = '\\'; *q++ = '\\'; p++;
			break;
		    case L'\'':
		    case L'"':
			if(quote == *p)  *q++ = '\\'; *q++ = *p++;
			break;
		    default:
			for(j = 0; j < res; j++) *q++ = *p++;
			break;
		    }
		} else if (k < 0x80) {
		    /* ANSI Escapes */
		    switch(wc) {
		    case L'\a': *q++ = '\\'; *q++ = 'a'; break;
		    case L'\b': *q++ = '\\'; *q++ = 'b'; break;
		    case L'\f': *q++ = '\\'; *q++ = 'f'; break;
		    case L'\n': *q++ = '\\'; *q++ = 'n'; break;
		    case L'\r': *q++ = '\\'; *q++ = 'r'; break;
		    case L'\t': *q++ = '\\'; *q++ = 't'; break;
		    case L'\v': *q++ = '\\'; *q++ = 'v'; break;
		    case L'\0': *q++ = '\\'; *q++ = '0'; break;

		    default:
			/* print in octal */
			snprintf(buf, 5, "\\%03o", k);
			for(j = 0; j < 4; j++) *q++ = buf[j];
			break;
		    }
		    p++;
		} else {
		    if(iswprint(wc)) {
			for(j = 0; j < res; j++) *q++ = *p++;
		    } else {
#ifndef Win32
			Unicode_warning = TRUE;
			if(k > 0xffff)
			    snprintf(buf, 11, "\\U%08x", k);
			else
#endif
			    snprintf(buf, 11, "\\u%04x", k);
			for(j = 0; j < strlen(buf); j++) *q++ = buf[j];
			p += res;
		    }
		    i += (res - 1);
		}

	    } else { /* invalid char */
		snprintf(q, 5, "\\x%02x", *((unsigned char *)p));
		q += 4; p++;
	    }
	}
#ifndef __STDC_ISO_10646__
	if(Unicode_warning)
	    warning(_("it is not known that wchar_t is Unicode on this platform"));
#endif

    } else
#endif
	for (i = 0; i < cnt; i++) {

	    /* ASCII */
	    if((unsigned char) *p < 0x80) {
		if(*p != '\t' && isprint((int)*p)) { /* Windows has \t as printable */
		    switch(*p) {
		    case '\\': *q++ = '\\'; *q++ = '\\'; break;
		    case '\'':
		    case '"':
			if(quote == *p)  *q++ = '\\'; *q++ = *p; break;
		    default: *q++ = *p; break;
		    }
		} else switch(*p) {
		    /* ANSI Escapes */
		case '\a': *q++ = '\\'; *q++ = 'a'; break;
		case '\b': *q++ = '\\'; *q++ = 'b'; break;
		case '\f': *q++ = '\\'; *q++ = 'f'; break;
		case '\n': *q++ = '\\'; *q++ = 'n'; break;
		case '\r': *q++ = '\\'; *q++ = 'r'; break;
		case '\t': *q++ = '\\'; *q++ = 't'; break;
		case '\v': *q++ = '\\'; *q++ = 'v'; break;
		case '\0': *q++ = '\\'; *q++ = '0'; break;

		default:
		    /* print in octal */
		    snprintf(buf, 5, "\\%03o", (unsigned char) *p);
		    for(j = 0; j < 4; j++) *q++ = buf[j];
		    break;
		}
		p++;
	    } else {  /* 8 bit char */
#ifdef Win32 /* It seems Windows does not know what is printable! */
		*q++ = *p++;
#else
		if(!isprint((int)*p & 0xff)) {
		    /* print in octal */
		    snprintf(buf, 5, "\\%03o", (unsigned char) *p);
		    for(j = 0; j < 4; j++) *q++ = buf[j];
		    p++;
		} else *q++ = *p++;
#endif
	    }
	}

    if(quote) *q++ = quote;
    if(b > 0 && justify != Rprt_adj_right) {
	for(i = 0 ; i < b ; i++) *q++ = ' ';
    }
    *q = '\0';
    return buffer->data;
}

char *EncodeElement(SEXP x, int indx, int quote, char dec)
{
    int w, d, e, wi, di, ei;
    char *res;

    switch(TYPEOF(x)) {
    case LGLSXP:
	formatLogical(&LOGICAL(x)[indx], 1, &w);
	res = EncodeLogical(LOGICAL(x)[indx], w);
	break;
    case INTSXP:
	formatInteger(&INTEGER(x)[indx], 1, &w);
	res = EncodeInteger(INTEGER(x)[indx], w);
	break;
    case REALSXP:
	formatReal(&REAL(x)[indx], 1, &w, &d, &e, 0);
	res = EncodeReal(REAL(x)[indx], w, d, e, dec);
	break;
    case STRSXP:
	formatString(&STRING_PTR(x)[indx], 1, &w, quote);
	res = EncodeString(STRING_ELT(x, indx), w, quote, Rprt_adj_left);
	break;
    case CPLXSXP:
	formatComplex(&COMPLEX(x)[indx], 1, &w, &d, &e, &wi, &di, &ei, 0);
	res = EncodeComplex(COMPLEX(x)[indx], w, d, e, wi, di, ei, dec);
	break;
    case RAWSXP:
	res = EncodeRaw(RAW(x)[indx]);
	break;
    default:
	res = NULL; /* -Wall */
	UNIMPLEMENTED_TYPE("EncodeElement", x);
    }
    return res;
}

#if 0
char *Rsprintf(char *format, ...)
{
    static char buffer[1001]; /* unsafe, as assuming max length, but all
				 internal uses are for a few characters */
    va_list(ap);

    va_start(ap, format);
    vsnprintf(buffer, 1000, format, ap);
    va_end(ap);
    buffer[1000] = '\0';
    return buffer;
}
#endif

void Rprintf(char *format, ...)
{
    va_list(ap);

    va_start(ap, format);
    Rvprintf(format, ap);
    va_end(ap);
}

/*
  REprintf is used by the error handler do not add
  anything unless you're sure it won't
  cause problems
*/
void REprintf(char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    REvprintf(format, ap);
    va_end(ap);
}

#if defined(HAVE_VASPRINTF) && !HAVE_DECL_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap)
#ifdef __cplusplus
	throw ()
#endif
;
#endif

#if !HAVE_VA_COPY && HAVE___VA_COPY
# define va_copy __va_copy
# undef HAVE_VA_COPY
# define HAVE_VA_COPY 1
#endif

#ifdef HAVE_VA_COPY
# define R_BUFSIZE BUFSIZE
#else
# define R_BUFSIZE 100000
#endif
void Rcons_vprintf(const char *format, va_list arg)
{
    char buf[R_BUFSIZE], *p = buf;
    int res;
#ifdef HAVE_VA_COPY
    char *vmax = vmaxget();
    int usedRalloc = FALSE, usedVasprintf = FALSE;
    va_list aq;

    va_copy(aq, arg);
    res = vsnprintf(buf, R_BUFSIZE, format, aq);
    va_end(aq);
#ifdef HAVE_VASPRINTF
    if(res >= R_BUFSIZE || res < 0)
	vasprintf(&p, format, arg);
#else
    if(res >= R_BUFSIZE) { /* res is the desired output length */
	usedRalloc = TRUE;
	p = R_alloc(res+1, sizeof(char));
	vsprintf(p, format, arg);
    } else if(res < 0) { /* just a failure indication */
	p = R_alloc(10*R_BUFSIZE, sizeof(char));
	res = vsnprintf(p, 10*R_BUFSIZE, format, arg);
	if (res < 0) {
	    *(p + 10*R_BUFSIZE) = '\0';
	    warning("printing of extremely long output is truncated");
	}
    }
#endif /* HAVE_VASPRINTF */
#else
    res = vsnprintf(p, R_BUFSIZE, format, arg);
    if(res >= R_BUFSIZE || res < 0) {
	/* res is the desired output length or just a failure indication */
	    buf[R_BUFSIZE - 1] = '\0';
	    warning(_("printing of extremely long output is truncated"));
	    res = R_BUFSIZE;
    }
#endif /* HAVE_VA_COPY */
    R_WriteConsole(p, strlen(p));
#ifdef HAVE_VA_COPY
    if(usedRalloc) vmaxset(vmax);
    if(usedVasprintf) free(p);
#endif
}

void Rvprintf(const char *format, va_list arg)
{
    int i=0, con_num=R_OutputCon;
    Rconnection con;
#ifdef HAVE_VA_COPY
    va_list argcopy;
#endif
    static int printcount = 0;

    if (++printcount > 100) {
	R_CheckUserInterrupt();
	printcount = 0 ;
    }

    do{
      con = getConnection(con_num);
#ifdef HAVE_VA_COPY
      va_copy(argcopy, arg);
      /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
      (con->vfprintf)(con, format, argcopy);
      va_end(argcopy);
#else /* don't support sink(,split=TRUE) */
      (con->vfprintf)(con, format, arg);
#endif
      con->fflush(con);
      con_num = getActiveSink(i++);
#ifndef HAVE_VA_COPY
      if (con_num>0) error("Internal error: this platform does not support split output");
#endif
    } while(con_num>0);


}

/*
   REvprintf is part of the error handler.
   Do not change it unless you are SURE that
   your changes are compatible with the
   error handling mechanism.

   It is also used in R_Suicide on Unix.
*/

void REvprintf(const char *format, va_list arg)
{
    if(R_ErrorCon != 2) {
	Rconnection con = getConnection_no_err(R_ErrorCon);
	if(con == NULL) {
	    /* should never happen, but in case of corruption... */
	    R_ErrorCon = 2;
	} else {
	    /* Parentheses added for FC4 with gcc4 and -D_FORTIFY_SOURCE=2 */
	    (con->vfprintf)(con, format, arg);
	    con->fflush(con);
	    return;
	}
    }
    if(R_Consolefile) {
	/* try to interleave stdout and stderr carefully */
	if(R_Outputfile && (R_Outputfile != R_Consolefile)) {
	    fflush(R_Outputfile);
	    vfprintf(R_Consolefile, format, arg);
	    fflush(R_Consolefile);
	} else vfprintf(R_Consolefile, format, arg);
    } else {
	char buf[BUFSIZE];

	vsnprintf(buf, BUFSIZE, format, arg);
	buf[BUFSIZE-1] = '\0';
	R_WriteConsoleEx(buf, strlen(buf), 1);
    }
}

int attribute_hidden IndexWidth(int n)
{
    return (int) (log10(n + 0.5) + 1);
}

void attribute_hidden VectorIndex(int i, int w)
{
/* print index label "[`i']" , using total width `w' (left filling blanks) */
    Rprintf("%*s[%ld]", w-IndexWidth(i)-2, "", i);
}

void attribute_hidden MatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;
    SEXP tmp;

    if (!isNull(cl)) {
        tmp = STRING_ELT(cl, j);
	if(tmp == NA_STRING) l = R_print.na_width_noquote;
	else l = Rstrlen(tmp, 0);
	Rprintf("%*s%s", w-l, "",
		EncodeString(tmp, l, 0, Rprt_adj_left));
    }
    else {
	Rprintf("%*s[,%ld]", w-IndexWidth(j+1)-3, "", j+1);
    }
}

void attribute_hidden RightMatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;
    SEXP tmp;

    if (!isNull(cl)) {
        tmp = STRING_ELT(cl, j);
	if(tmp == NA_STRING) l = R_print.na_width_noquote;
	else l = Rstrlen(tmp, 0);
	/* This does not work correctly at least on FC3
	Rprintf("%*s", R_print.gap+w,
		EncodeString(tmp, l, 0, Rprt_adj_right)); */
	Rprintf("%*s%s", R_print.gap+w-l, "",
	        EncodeString(tmp, l, 0, Rprt_adj_right));
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-IndexWidth(j+1)-3, "");
    }
}

void attribute_hidden LeftMatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;
    SEXP tmp;

    if (!isNull(cl)) {
        tmp= STRING_ELT(cl, j);
	if(tmp == NA_STRING) l = R_print.na_width_noquote;
	else l = Rstrlen(tmp, 0);
	Rprintf("%*s%s%*s", R_print.gap, "",
		EncodeString(tmp, l, 0, Rprt_adj_left), w-l, "");
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-IndexWidth(j+1)-3, "");
    }
}

void attribute_hidden MatrixRowLabel(SEXP rl, int i, int rlabw, int lbloff)
{
    int l;
    SEXP tmp;

    if (!isNull(rl)) {
        tmp= STRING_ELT(rl, i);
	if(tmp == NA_STRING) l = R_print.na_width_noquote;
	else l = Rstrlen(tmp, 0);
	Rprintf("\n%*s%s%*s", lbloff, "",
		EncodeString(tmp, l, 0, Rprt_adj_left),
		rlabw-l-lbloff, "");
    }
    else {
	Rprintf("\n%*s[%ld,]", rlabw-3-IndexWidth(i + 1), "", i+1);
    }
}
