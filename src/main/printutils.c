/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999--2004  The R Development Core Team
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
 * a single R-vector element.  This is mainly used in gizmos like deparse.
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
static R_StringBuffer gBuffer = {NULL, 0, BUFSIZE};
static R_StringBuffer *buffer = &gBuffer; /*XX Add appropriate const here
                                            and in the routines that use it. */

R_size_t R_Decode2Long(char *p, int *ierr)
{
    R_size_t v = strtol(p, &p, 10);
    *ierr = 0;
    if(p[0] == '\0') return v;
    /* else look for letter-code ending : */
    if(R_Verbose)
	REprintf("R_Decode2Long(): v=%ld\n", v);
    if(p[0] == 'G') {
	if((Giga * (double)v) > R_SIZE_T_MAX) { *ierr = 1; return(v); }
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

char *EncodeLogical(int x, int w)
{
    R_AllocStringBuffer(0, buffer);
    if(x == NA_LOGICAL) sprintf(buffer->data, "%*s", w, CHAR(R_print.na_string));
    else if(x) sprintf(buffer->data, "%*s", w, "TRUE");
    else sprintf(buffer->data, "%*s", w, "FALSE");
    return buffer->data;
}

char *EncodeInteger(int x, int w)
{
    R_AllocStringBuffer(0, buffer);
    if(x == NA_INTEGER) sprintf(buffer->data, "%*s", w, CHAR(R_print.na_string));
    else sprintf(buffer->data, "%*d", w, x);
    return buffer->data;
}

char *EncodeRaw(Rbyte x)
{
    R_AllocStringBuffer(0, buffer);
    sprintf(buffer->data, "%02x", x);
    return buffer->data;
}

char *EncodeReal(double x, int w, int d, int e)
{
    char fmt[20];

    R_AllocStringBuffer(0, buffer);
    /* IEEE allows signed zeros (yuck!) */
    if (x == 0.0) x = 0.0;
    if (!R_FINITE(x)) {
	if(ISNA(x)) sprintf(buffer->data, "%*s", w, CHAR(R_print.na_string));
	else if(ISNAN(x)) sprintf(buffer->data, "%*s", w, "NaN");
	else if(x > 0) sprintf(buffer->data, "%*s", w, "Inf");
	else sprintf(buffer->data, "%*s", w, "-Inf");
    }
    else if (e) {
#ifndef Win32
	if(d) {
	    sprintf(fmt,"%%#%d.%de", w, d);
	    sprintf(buffer->data, fmt, x);
	}
	else {
	    sprintf(fmt,"%%%d.%de", w, d);
	    sprintf(buffer->data, fmt, x);
	}
#else
	/* Win32 libraries always use e+xxx format so avoid them */
	double X= x, xx = prec(x, (double)(d+1)); /* might have 9.99997e-7 */
	int kp = (xx == 0.0) ? 0 : floor(log10(fabs(xx))+1e-12), ee = 1;
	if(kp > 0) {
	    x = x / pow(10.0, (double)kp);
	} else if (kp < 0) {
	    x = x * pow(10.0, (double)(-kp));
	}
	if(abs(kp) >= 100) {
	    if(d) sprintf(fmt,"%%#%d.%de", w, d);
	    else sprintf(fmt,"%%%d.%de", w, d);
	    sprintf(buffer->data, fmt, X);
	} else {
	    if(d) sprintf(fmt, "%%#%d.%dfe%%+0%dd", w-ee-3, d, ee+2);
	    else sprintf(fmt, "%%%d.%dfe%%+0%dd", w-ee-3, d, ee+2);
	    sprintf(buffer->data, fmt, x, kp);
	}
#endif
    }
    else { /* e = 0 */
	sprintf(fmt,"%%%d.%df", w, d);
	sprintf(buffer->data, fmt, x);
    }
    return buffer->data;
}

char *EncodeComplex(Rcomplex x, int wr, int dr, int er, int wi, int di, int ei)
{
    char *Re, *Im, *tmp;
    int  flagNegIm = 0;

    R_AllocStringBuffer(0, buffer);
    /* IEEE allows signed zeros; strip these here */
    if (x.r == 0.0) x.r = 0.0;
    if (x.i == 0.0) x.i = 0.0;

    if (ISNA(x.r) || ISNA(x.i)) {
	sprintf(buffer->data, "%*s%*s", R_print.gap, "", wr+wi+2,
		CHAR(R_print.na_string));
    }
    else {
	/* EncodeReal returns pointer to static storage so copy */

	tmp = EncodeReal(x.r, wr, dr, er);
	Re = Calloc(strlen(tmp)+1, char);
	strcpy(Re, tmp);

	if ( (flagNegIm = (x.i < 0)) )
	    x.i = -x.i;
	tmp = EncodeReal(x.i, wi, di, ei);
	Im = Calloc(strlen(tmp)+1, char);
	strcpy(Im, tmp);

	sprintf(buffer->data, "%s%s%si", Re, flagNegIm ? "-" : "+", Im);

	Free(Re); Free(Im);
    }
    return buffer->data;
}


/* strlen() using escaped rather than literal form, 
   and allows for embedded nuls */
int Rstrlen(SEXP s, int quote)
{
    char *p;
    int len, i;

    len = 0;
    p = CHAR(s);
    for (i = 0; i < LENGTH(s); i++) {
	if(isprint((int)*p)) {
	    switch(*p) {
	    case '\\':
#ifdef ESCquote
	    case '\'':
#endif
		 len += 2; break;
#ifdef ESC_BARE_QUOTE
	    case '\"': len += 2; break;
#else
	    case '\"': len += quote ? 2 : 1; break;
#endif
	    default: len += 1; break;
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
	default: /* print in octal */
	    len += 5; break;
	}
	p++;
    }
    return len;
}

/* Here w appears to be the minimum field width */
char *EncodeString(SEXP s, int w, int quote, int right)
{
    int b, i, j, cnt;
    char *p, *q, buf[5];

    if (s == NA_STRING) {
	p = quote ? CHAR(R_print.na_string) : CHAR(R_print.na_string_noquote);
	cnt = i = quote ? strlen(CHAR(R_print.na_string)) :
	    strlen(CHAR(R_print.na_string_noquote));
	quote = 0;
    } else {
	p = CHAR(s);
	i = Rstrlen(s, quote);
	cnt = LENGTH(s);
    }

    R_AllocStringBuffer((i+2 >= w)?(i+2):w, buffer); /* +2 allows for quotes */
    q = buffer->data;
    if(right) { /* Right justifying */
	b = w - i - (quote ? 2 : 0);
	for(i=0 ; i<b ; i++) *q++ = ' ';
    }
    if(quote) *q++ = quote;
    for (i = 0; i < cnt; i++) {

	/* ASCII */

	if(isprint((int)*p)) {
	    switch(*p) {
	    case '\\': *q++ = '\\'; *q++ = '\\'; break;
#ifdef ESCquote
	    case '\'': *q++ = '\\'; *q++ = '\''; break;
#endif
#ifdef ESC_BARE_QUOTE
	    case '\"': *q++ = '\"'; break;
#else
	    case '\"': if(quote) *q++ = '\\'; *q++ = '\"'; break;
#endif
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
	case '\0': *q++ = '\\'; *q++ = '0'; break;

	default: /* print in octal */
	    snprintf(buf, 5, "\\%03o", (unsigned char) *p);
	    for(j = 0; j < 4; j++) *q++ = buf[j];
	    break;
	}
	p++;
    }
    if(quote) *q++ = quote;
    if(!right) { /* Left justifying */
	*q = '\0';
	b = w - strlen(buffer->data);
	for(i=0 ; i<b ; i++) *q++ = ' ';
    }
    *q = '\0';
    return buffer->data;
}

char *EncodeElement(SEXP x, int indx, int quote)
{
    int w, d, e, wi, di, ei;

    switch(TYPEOF(x)) {
    case LGLSXP:
	formatLogical(&INTEGER(x)[indx], 1, &w);
	EncodeLogical(INTEGER(x)[indx], w);
	break;
    case INTSXP:
	formatInteger(&INTEGER(x)[indx], 1, &w);
	EncodeInteger(INTEGER(x)[indx], w);
	break;
    case REALSXP:
	formatReal(&REAL(x)[indx], 1, &w, &d, &e, 0);
	EncodeReal(REAL(x)[indx], w, d, e);
	break;
    case STRSXP:
	formatString(&STRING_PTR(x)[indx], 1, &w, quote);
	EncodeString(STRING_ELT(x, indx), w, quote, Rprt_adj_left);
	break;
    case CPLXSXP:
	formatComplex(&COMPLEX(x)[indx], 1,
		      &w, &d, &e, &wi, &di, &ei, 0);
	EncodeComplex(COMPLEX(x)[indx],
		      w, d, e, wi, di, ei);
	break;
    case RAWSXP:
	EncodeRaw(RAW(x)[indx]);
	break;
    }
    return buffer->data;
}

char *Rsprintf(char *format, ...)
{
    va_list(ap);

    R_AllocStringBuffer(0, buffer); /* unsafe, as assuming length, but all internal
 		                       uses are for a few characters */
    va_start(ap, format);
    vsprintf(buffer->data, format, ap);
    va_end(ap);
    return buffer->data;
}

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

void Rcons_vprintf(const char *format, va_list arg)
{
    char buf[BUFSIZE], *p = buf, *vmax = vmaxget();
    int res;

    res = vsnprintf(p, BUFSIZE, format, arg);
    if(res >= BUFSIZE) { /* res is the desired output length */
	p = R_alloc(res+1, sizeof(char));
	vsprintf(p, format, arg);
    } else if(res < 0) { /* just a failure indication */
	p = R_alloc(10*BUFSIZE, sizeof(char));
	res = vsnprintf(p, 10*BUFSIZE, format, arg);
	if (res < 0) {
	    *(p + 10*BUFSIZE) = '\0';
	    warning("printing of extremely long output is truncated");
	}
    }
    R_WriteConsole(p, strlen(buf));
    vmaxset(vmax);
}

void Rvprintf(const char *format, va_list arg)
{
    int i=0, con_num=R_OutputCon;
    Rconnection con;
    static int printcount = 0;
    if (++printcount > 100) {
	R_CheckUserInterrupt();
	printcount = 0 ;
    }
    
    do{
      con = getConnection(con_num);
      con->vfprintf(con, format, arg);
      con->fflush(con);
      con_num = getActiveSink(i++);
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
	    con->vfprintf(con, format, arg);
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
	int slen;

	vsnprintf(buf, BUFSIZE, format, arg);
	buf[BUFSIZE-1] = '\0';
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
/* print index label "[`i']" , using total width `w' (left filling blanks) */
    Rprintf("%*s[%ld]", w-IndexWidth(i)-2, "", i);
}

void MatrixColumnLabel(SEXP cl, int j, int w)
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

void RightMatrixColumnLabel(SEXP cl, int j, int w)
{
    int l;
    SEXP tmp;

    if (!isNull(cl)) {
        tmp = STRING_ELT(cl, j);
	if(tmp == NA_STRING) l = R_print.na_width_noquote;
	else l = Rstrlen(tmp, 0);
	Rprintf("%*s", R_print.gap+w,
		EncodeString(tmp, l, 0, Rprt_adj_right));
    }
    else {
	Rprintf("%*s[,%ld]%*s", R_print.gap, "", j+1, w-IndexWidth(j+1)-3, "");
    }
}

void LeftMatrixColumnLabel(SEXP cl, int j, int w)
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

void MatrixRowLabel(SEXP rl, int i, int rlabw, int lbloff)
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
