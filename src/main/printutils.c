/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999, The R Development Core Team
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

/* =========
 * Printing:
 * =========
 *
 * All printing in R is done via the functions Rprintf and REprintf.
 * These routines work exactly like printf(3).	Rprintf writes to
 * ``standard output''.	 It is redirected by the sink() function,
 * and is suitable for ordinary output.	 REprintf writes to
 * ``standard error'' and is useful for error messages and warnings.
 * It is not redirected by sink().
 *
 *  See ./format.c  for the  format_FOO_  functions which provide
 *	~~~~~~~~~~  the	 length, width, etc.. that are used here.
 *  See ./print.c  for do_printdefault, do_printmatrix, etc.
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
 * a single R-vector element.  This is mainly used in gizmos like deparse.
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Mathlib.h"
#include "Print.h"

#define BUFSIZE 8192  /* used by Rprintf etc */
static char *Encodebuf=NULL;

static void AllocBuffer(int len)
{
    static int bufsize = 0;
    if(len*sizeof(char) < bufsize) return;
    len = (len+1)*sizeof(char);
    if(len < BUFSIZE) len = BUFSIZE;
    Encodebuf = (char *) realloc(Encodebuf, len);
    bufsize = len;
    if(!Encodebuf) {
	bufsize = 0;
	error("Could not allocate memory for Encodebuf");
    }
}

long Decode2Long(char *p, int *ierr)
{
    long v = strtol(p, &p, 10);
    *ierr = 0;
    if(p[0] == '\0') return v;
    /* else look for letter-code ending : */
    if(R_Verbose)
	REprintf("Decode2Long(): v=%ld\n", v);
    if(p[0] == 'M') {
	if((Mega * (double)v) > LONG_MAX) { *ierr = 1; return(v); }
	return (Mega*v);
    }
    else if(p[0] == 'K') {
	if((1024 * (double)v) > LONG_MAX) { *ierr = 2; return(v); }
	return (1024*v);
    }
    else if(p[0] == 'k') {
	if((1000 * (double)v) > LONG_MAX) { *ierr = 3; return(v); }
	return (1000*v);
    }
    else {
	*ierr = -1;
	return(v);
    }
}

char *EncodeLogical(int x, int w)
{
    AllocBuffer(0);
    if(x == NA_LOGICAL) sprintf(Encodebuf, "%*s", w, CHAR(R_print.na_string));
    else if(x) sprintf(Encodebuf, "%*s", w, "TRUE");
    else sprintf(Encodebuf, "%*s", w, "FALSE");
    return Encodebuf;
}

char *EncodeInteger(int x, int w)
{
    AllocBuffer(0);
    if(x == NA_INTEGER) sprintf(Encodebuf, "%*s", w, CHAR(R_print.na_string));
    else sprintf(Encodebuf, "%*d", w, x);
    return Encodebuf;
}

char *EncodeReal(double x, int w, int d, int e)
{
    char fmt[20];

    AllocBuffer(0);
    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
#ifdef IEEE_754
	if(ISNA(x)) sprintf(Encodebuf, "%*s", w, CHAR(R_print.na_string));
	else if(ISNAN(x)) sprintf(Encodebuf, "%*s", w, "NaN");
	else if(x > 0) sprintf(Encodebuf, "%*s", w, "Inf");
	else sprintf(Encodebuf, "%*s", w, "-Inf");
#else
	sprintf(Encodebuf, "%*s", w, CHAR(R_print.na_string));
#endif
    }
    else if (e) {
#ifndef Win32
	if(d) {
	    sprintf(fmt,"%%#%d.%de", w, d);
	    sprintf(Encodebuf, fmt, x);
	}
	else {
	    sprintf(fmt,"%%%d.%de", w, d);
	    sprintf(Encodebuf, fmt, x);
	}
#else
	/* Win32 libraries always use e+xxx format so avoid them */
	double xx = prec(x, (double)(d+1)); /* might have 9.99997e-7 */
	int kp = (xx == 0.0)? 0 : floor(log10(fabs(xx))+1e-12), ee = 1;
	x = x / pow(10.0, (double)kp);
	if(abs(kp) >= 100) ee = 2;
	if(d)
	    sprintf(fmt, "%%#%d.%dfe%%+0%dd", w-ee-3, d, ee+2);
	else
	    sprintf(fmt, "%%%d.%dfe%%+0%dd", w-ee-3, d, ee+2);
	sprintf(Encodebuf, fmt, x, kp);
#endif
    }
    else {
	sprintf(fmt,"%%%d.%df",w,d);
	sprintf(Encodebuf, fmt, x);
    }
    return Encodebuf;
}

char *EncodeComplex(Rcomplex x, int wr, int dr, int er, int wi, int di, int ei)
{
#if OLD
    char fmt[64], *efr, *efi;
#else
    char *Re, *Im, *tmp;
    int  flagNegIm = 0;
#endif

    AllocBuffer(0);
    /* IEEE allows signed zeros; strip these here */
    if (x.r == 0.0) x.r = 0.0;
    if (x.i == 0.0) x.i = 0.0;


    if (ISNA(x.r) || ISNA(x.i)) {
	sprintf(Encodebuf, "%*s%*s", R_print.gap, "", wr+wi+2,
		CHAR(R_print.na_string));
    }
    else {
#if OLD
	if(er) efr = "e"; else efr = "f";
	if(ei) efi = "e"; else efi = "f";
	sprintf(fmt,"%%%d.%d%s%%+%d.%d%si", wr, dr, efr, wi, di, efi);
	sprintf(Encodebuf, fmt, x.r, x.i);
#else

	/* EncodeReal returns pointer to static storage so copy */

	tmp = EncodeReal(x.r, wr, dr, er);
	Re = (char *) calloc(strlen(tmp)+1, sizeof(char));
	strcpy(Re, tmp);

	if ( (flagNegIm = (x.i < 0)) )
	    x.i = -x.i;
	tmp = EncodeReal(x.i, wi, di, ei);
	Im = (char *) calloc(strlen(tmp)+1, sizeof(char));
	strcpy(Im, tmp);

	sprintf(Encodebuf, "%s%s%si", Re, flagNegIm ? "-" : "+", Im);

	free(Re); free(Im);
    }
#endif
    return Encodebuf;
}

	/* There is a heavy ASCII emphasis here */
	/* Latin1 types are (rightfully) upset */
	/* WHAT NEEDS TO CHANGE */

#ifdef OLD
static int hexdigit(unsigned int x)
{
    return ((x <= 9)? '0' :	 'A'-10) + x;
}
#endif

/* strlen() using escaped rather than literal form */
int Rstrlen(char *s)
{
    char *p;
    int len;
    len = 0;
    p = s;
    while(*p) {
	if(isprint((int)*p)) {
	    switch(*p) {
	    case '\\':
#ifdef ESCquote
	    case '\'':
#endif
	    case '\"': len += 2; break;
	    default: len += 1; break;
	    }
	}
	else switch(*p) {
	case '\a':
	case '\b':
	case '\f':
	case '\n':
	case '\r':
	case '\t':
	case '\v':
	    len += 2; break;
	default:
#ifdef OLD
	    len += 4; break;
#else
	    len += 1; break;
#endif
	}
	p++;
    }
    return len;
}

char *EncodeString(char *s, int w, int quote, int right)
{
    int b, i ;
    char *p, *q;

    i = Rstrlen(s);
    AllocBuffer(i);
    q = Encodebuf;
    if(right) { /*Right justifying */
	b = w - i - (quote ? 2 : 0);
	for(i=0 ; i<b ; i++) *q++ = ' ';
    }
    if(quote) *q++ = quote;
    if (s == CHAR(NA_STRING) )
	p = CHAR(R_print.na_string);
    else	p = s;
    while(*p) {

	/* ASCII */

	if(isprint((int)*p)) {
	    switch(*p) {
	    case '\\': *q++ = '\\'; *q++ = '\\'; break;
#ifdef ESCquote
	    case '\'': *q++ = '\\'; *q++ = '\''; break;
#endif
	    case '\"': *q++ = '\\'; *q++ = '\"'; break;
	    default: *q++ = *p; break;
	    }
	}

	/* ANSI Escapes */

	else switch(*p) {
	case '\a': *q++ = '\\'; *q++ = 'a'; break;
	case '\b': *q++ = '\\'; *q++ = 'b'; break;
	case '\f': *q++ = '\\'; *q++ = 'f'; break;
	case '\n': *q++ = '\\'; *q++ = 'n'; break;
	case '\r': *q++ = '\\'; *q++ = 'r'; break;
	case '\t': *q++ = '\\'; *q++ = 't'; break;
	case '\v': *q++ = '\\'; *q++ = 'v'; break;

	    /* Latin1 Swallowed Here */

#ifdef OLD
	default: *q++ = '0'; *q++ = 'x';
	    *q++ = hexdigit((*p & 0xF0) >> 4);
	    *q++ = hexdigit(*p & 0x0F);
#else
	default:
	    *q++ = *p; break;
#endif
	}
	p++;
    }
    if(quote) *q++ = quote;
    if(!right) { /* Left justifying */
	*q = '\0';
	b = w - strlen(Encodebuf);
	for(i=0 ; i<b ; i++) *q++ = ' ';
    }
    *q = '\0';
    return Encodebuf;
}

char *EncodeElement(SEXP x, int index, int quote)
{
    int w, d, e, wi, di, ei;

    switch(TYPEOF(x)) {
    case LGLSXP:
	formatLogical(&INTEGER(x)[index], 1, &w);
	EncodeLogical(INTEGER(x)[index], w);
	break;
    case INTSXP:
	formatInteger(&INTEGER(x)[index], 1, &w);
	EncodeInteger(INTEGER(x)[index], w);
	break;
    case REALSXP:
	formatReal(&REAL(x)[index], 1, &w, &d, &e);
	EncodeReal(REAL(x)[index], w, d, e);
	break;
    case STRSXP:
	formatString(&STRING(x)[index], 1, &w, quote);
	EncodeString(CHAR(STRING(x)[index]), w, quote, adj_left);
	break;
    case CPLXSXP:
	formatComplex(&COMPLEX(x)[index], 1,
		      &w, &d, &e, &wi, &di, &ei);
	EncodeComplex(COMPLEX(x)[index],
		      w, d, e, wi, di, ei);
	break;
    }
    return Encodebuf;
}

char *Rsprintf(char *format, ...)
{
    va_list(ap);

    AllocBuffer(0);
    va_start(ap, format);
    vsprintf(Encodebuf, format, ap);
    va_end(ap);
    return Encodebuf;
}

void Rprintf(char *format, ...)
{
    va_list(ap);

    va_start(ap, format);
    if(R_Outputfile) {
	vfprintf(R_Outputfile, format, ap);
	fflush(R_Outputfile);
    }
    else {
	char buf[BUFSIZE]; int len;
	vsprintf(buf, format, ap);
	len = strlen(buf);
	R_WriteConsole(buf, len);
    }
    va_end(ap);
}

void REprintf(char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    if(R_Consolefile) {
	vfprintf(R_Consolefile, format, ap);
    }
    else {
	char buf[BUFSIZE]; int len;
	vsprintf(buf, format, ap);
	len = strlen(buf);
	R_WriteConsole(buf, len);
    }
    va_end(ap);
}

void Rvprintf(const char *format, va_list arg)
{
    if(R_Outputfile) {
	vfprintf(R_Outputfile, format, arg);
	fflush(R_Outputfile);
    }
    else {
	char buf[BUFSIZE]; int slen;
	vsprintf(buf, format, arg);
	slen = strlen(buf);
	R_WriteConsole(buf, slen);
    }
}

void REvprintf(const char *format, va_list arg)
{
    if(R_Consolefile) {
	vfprintf(R_Consolefile, format, arg);
    }
    else {
	char buf[BUFSIZE]; int slen;
	vsprintf(buf, format, arg);
	slen = strlen(buf);
	R_WriteConsole(buf, slen);
    }
}

int IndexWidth(int n)
{
    return (int) (log10(n + 0.5) + 1);
}

void VectorIndex(int i, int w)
{
    Rprintf("%*s[%ld]", w-IndexWidth(i)-2, "", i);
}

void MatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;

    if (!isNull(cl)) {
	l = Rstrlen(CHAR(STRING(cl)[j]));
	Rprintf("%*s%s", w-l, "",
		EncodeString(CHAR(STRING(cl)[j]), l, 0, adj_left));
    }
    else {
	Rprintf("%*s[,%ld]", w-IndexWidth(j+1)-3, "", j+1);
    }
}

void RightMatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;

    if (!isNull(cl)) {
	l = Rstrlen(CHAR(STRING(cl)[j]));
	Rprintf("%*s", R_print.gap+w,
		EncodeString(CHAR(STRING(cl)[j]), l, 0, adj_right));
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-IndexWidth(j+1)-3, "");
    }
}

void LeftMatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;

    if (!isNull(cl)) {
	l = Rstrlen(CHAR(STRING(cl)[j]));
	Rprintf("%*s%s%*s", R_print.gap, "",
		EncodeString(CHAR(STRING(cl)[j]), l, 0, adj_left), w-l, "");
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-IndexWidth(j+1)-3, "");
    }
}

void MatrixRowLabel(SEXP rl, int i, int rlabw, int lbloff)
{
    int l;

    if (!isNull(rl)) {
	l = Rstrlen(CHAR(STRING(rl)[i]));
	Rprintf("\n%*s%s%*s", lbloff, "",
		EncodeString(CHAR(STRING(rl)[i]), l, 0, adj_left),
		rlabw-l-lbloff, "");
    }
    else {
	Rprintf("\n%*s[%ld,]", rlabw-3-IndexWidth(i + 1), "", i+1);
    }
}
