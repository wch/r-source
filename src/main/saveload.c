/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1999  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Mathlib.h"
#include "Fileio.h"

#ifdef HAVE_HDF5
#include <hdf5.h>
#endif

/* Static Globals */

static char smbuf[512];		/* Small buffer for temp use */
static char *buf=NULL;		/* Buffer for character strings */
static char *bufp;		/* A pointer to that buffer */
static int bufsize=0;		/* Current buffer size */

static int NSymbol;		/* Number of symbols */
static int NSave;		/* Number of non-symbols */
static int NTotal;		/* NSymbol + NSave */
static int NVSize;		/* Number of vector cells */

static int *OldOffset;		/* Offsets in previous incarnation */
static SEXP *NewAddress;	/* Addresses in this incarnation */

static int VersionId;
static int DLstartup;		/* Allows different error action on startup */

static SEXP DataLoad(FILE*);
static void DataSave(SEXP, FILE*);

static void AllocBuffer(int len)
{
    if(len >= 0 ) {
	if(len*sizeof(char) < bufsize) return;
	len = (len+1)*sizeof(char);
	if(len < MAXELTSIZE) len = MAXELTSIZE;
	buf = (char *) realloc(buf, len);
	bufsize = len;
	if(!buf) {
	    bufsize = 0;
	    error("Could not allocate memory for string save/load");
	}
    } else {
	if(bufsize == MAXELTSIZE) return;
 /* frees if non-zero */
	realloc(buf, 0);
	buf = (char *) realloc(buf, MAXELTSIZE);
	bufsize = MAXELTSIZE;
    }
}


/* I/O Function Pointers */

static void	(*OutInit)(FILE*);
static void	(*OutInteger)(FILE*, int);
static void	(*OutReal)(FILE*, double);
static void	(*OutComplex)(FILE*, complex);
static void	(*OutString)(FILE*, char*);
static void	(*OutSpace)(FILE*);
static void	(*OutNewline)(FILE*);
static void	(*OutTerm)(FILE*);

static void	(*InInit)(FILE*);
static int	(*InInteger)(FILE*);
static double	(*InReal)(FILE*);
static complex	(*InComplex)(FILE*);
static char*	(*InString)(FILE*);
static void	(*InTerm)(FILE*);



/* Dummy Placeholder Routine */

static void Dummy(FILE *fp)
{
}


/* Functions for Ascii Pickling */

static void AsciiOutInteger(FILE *fp, int i)
{
    if (i == NA_INTEGER)
	fprintf(fp, "NA");
    else
	fprintf(fp, "%d", i);
}

static int AsciiInInteger(FILE *fp)
{
    int x;
    fscanf(fp, "%s", smbuf);
    if (strcmp(smbuf, "NA") == 0)
	return NA_INTEGER;
    else {
	sscanf(smbuf, "%d", &x);
	return x;
    }
}

static void AsciiOutReal(FILE *fp, double x)
{
    if (!R_FINITE(x)) {
	if (ISNAN(x))
	    fprintf(fp, "NA");
	else if (x < 0)
	    fprintf(fp, "-Inf");
	else
	    fprintf(fp, "Inf");
    }
    else fprintf(fp, "%.16g", x);/* 16: full precision; 17 gives 999, 000 &c */
}

static double AsciiInReal(FILE *fp)
{
    double x;
    fscanf(fp, "%s", smbuf);
    if (strcmp(smbuf, "NA") == 0)
	x = NA_REAL;
    else if (strcmp(smbuf, "Inf") == 0)
	x = R_PosInf;
    else if (strcmp(smbuf, "-Inf") == 0)
	x = R_NegInf;
    else
	sscanf(smbuf, "%lg", &x);
    return x;
}

static void AsciiOutComplex(FILE *fp, complex x)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	fprintf(fp, "NA NA");
    else
	fprintf(fp, "%g %g", x.r, x.i);
}

static complex AsciiInComplex(FILE *fp)
{
    complex x;
    fscanf(fp, "%s", smbuf);
    if (strcmp(smbuf, "NA") == 0)
	x.r = NA_REAL;
    else if (strcmp(smbuf, "Inf") == 0)
	x.r = R_PosInf;
    else if (strcmp(smbuf, "-Inf") == 0)
	x.r = R_NegInf;
    else
	sscanf(smbuf, "%lg", &x.r);

    fscanf(fp, "%s", smbuf);
    if (strcmp(smbuf, "NA") == 0)
	x.i = NA_REAL;
    else if (strcmp(smbuf, "Inf") == 0)
	x.i = R_PosInf;
    else if (strcmp(smbuf, "-Inf") == 0)
	x.i = R_NegInf;
    else
	sscanf(smbuf, "%lg", &x.i);
    return x;
}

static void AsciiOutSpace(FILE *fp)
{
	fputc(' ', fp);
}

static void AsciiOutNewline(FILE *fp)
{
	fputc('\n', fp);
}

/* FIXME : To make saved files completely portable, the output */
/* representation of strings should be completely ascii.  This */
/* includes control characters and non-ascii characters. */
/* This could be done with \ooo escapes. */

static void AsciiOutString(FILE *fp, char *s)
{
    char *p = s;
    fputc('\"', fp);
    while (*p) {
	switch(*p) {
	case '\n': fputc('\\', fp); fputc('n', fp); break;
	case '\t': fputc('\\', fp); fputc('t', fp); break;
	case '\v': fputc('\\', fp); fputc('v', fp); break;
	case '\b': fputc('\\', fp); fputc('b', fp); break;
	case '\r': fputc('\\', fp); fputc('r', fp); break;
	case '\f': fputc('\\', fp); fputc('f', fp); break;
	case '\a': fputc('\\', fp); fputc('a', fp); break;
	case '\\': fputc('\\', fp); fputc('\\', fp); break;
	case '\?': fputc('\\', fp); fputc('\?', fp); break;
	case '\'': fputc('\\', fp); fputc('\'', fp); break;
	case '\"': fputc('\\', fp); fputc('\"', fp); break;
	default:   fputc(*p, fp); break;
	}
	p++;
    }
    fputc('\"', fp);
}

static char *AsciiInString(FILE *fp)
{
    int c;
    bufp = buf;
    while ((c = R_fgetc(fp)) != '"');
    while ((c = R_fgetc(fp)) != R_EOF && c != '"') {
	if (c == '\\') {
	    if ((c = R_fgetc(fp)) == R_EOF) break;
	    switch(c) {
	    case 'n':  c = '\n'; break;
	    case 't':  c = '\t'; break;
	    case 'v':  c = '\v'; break;
	    case 'b':  c = '\b'; break;
	    case 'r':  c = '\r'; break;
	    case 'f':  c = '\f'; break;
	    case 'a':  c = '\a'; break;
	    case '\\': c = '\\'; break;
	    case '\?': c = '\?'; break;
	    case '\'': c = '\''; break;
	    case '\"': c = '\"'; break;
	    default:  break;
	    }
	}
	*bufp++ = c;
    }
    *bufp = '\0';
    return buf;
}

void AsciiSave(SEXP s, FILE *fp)
{
    OutInit = Dummy;
    OutInteger = AsciiOutInteger;
    OutReal = AsciiOutReal;
    OutComplex = AsciiOutComplex;
    OutString = AsciiOutString;
    OutSpace = AsciiOutSpace;
    OutNewline = AsciiOutNewline;
    OutTerm = Dummy;
    DataSave(s, fp);
}

SEXP AsciiLoad(FILE *fp)
{
    VersionId = 0;
    InInit = Dummy;
    InInteger = AsciiInInteger;
    InReal = AsciiInReal;
    InComplex = AsciiInComplex;
    InString = AsciiInString;
    InTerm = Dummy;
    return DataLoad(fp);
}

SEXP AsciiLoadOld(FILE *fp, int version)
{
    VersionId = version;
    InInit = Dummy;
    InInteger = AsciiInInteger;
    InReal = AsciiInReal;
    InComplex = AsciiInComplex;
    InString = AsciiInString;
    InTerm = Dummy;
    return DataLoad(fp);
}


#ifdef HAVE_RPC_XDR_H

/* Functions for Binary Pickling Using XDR */

#include <rpc/rpc.h>

XDR xdrs;

static void XdrOutInit(FILE *fp)
{
    xdrstdio_create(&xdrs, fp, XDR_ENCODE);
}

static void XdrOutTerm(FILE *fp)
{
    xdr_destroy(&xdrs);
}

static void XdrInInit(FILE *fp)
{
    xdrstdio_create(&xdrs, fp, XDR_DECODE);
}

static void XdrInTerm(FILE *fp)
{
    xdr_destroy(&xdrs);
}

static void XdrOutInteger(FILE *fp, int i)
{
    if (!xdr_int(&xdrs, &i)) {
	xdr_destroy(&xdrs);
	error("a I write error occured\n");
    }
}

static int XdrInInteger(FILE * fp)
{
    int i;
    if (!xdr_int(&xdrs, &i)) {
	xdr_destroy(&xdrs);
	error("a read error occured\n");
    }
    return i;
}

static void XdrOutReal(FILE *fp, double x)
{
    if (!xdr_double(&xdrs, &x)) {
	xdr_destroy(&xdrs);
	error("a R write error occured\n");
    }
}

static double XdrInReal(FILE * fp)
{
    double x;
    if (!xdr_double(&xdrs, &x)) {
	xdr_destroy(&xdrs);
	error("a read error occured\n");
    }
    return x;
}

static void XdrOutComplex(FILE *fp, complex x)
{
    if (!xdr_double(&xdrs, &(x.r)) || !xdr_double(&xdrs, &(x.i))) {
	xdr_destroy(&xdrs);
	error("a write error occured\n");
    }
}

static complex XdrInComplex(FILE * fp)
{
    complex x;
    if (!xdr_double(&xdrs, &(x.r)) || !xdr_double(&xdrs, &(x.i))) {
	xdr_destroy(&xdrs);
	error("a read error occured\n");
    }
    return x;
}

static void XdrOutString(FILE *fp, char *s)
{
    if (!xdr_string(&xdrs, &s, strlen(s))) {
	xdr_destroy(&xdrs);
	error("a S write error occured\n");
    }
}

static char *XdrInString(FILE *fp)
{
    char *bufp = buf;
    if (!xdr_string(&xdrs, &bufp, bufsize)) {
	xdr_destroy(&xdrs);
	error("a read error occured\n");
    }
    return buf;
}

#ifdef EXPERIMENTAL
#include "saveload-x.c"
#endif

static void XdrSave(SEXP s, FILE *fp)
{
    OutInit = XdrOutInit;
    OutInteger = XdrOutInteger;
    OutReal = XdrOutReal;
    OutComplex = XdrOutComplex;
    OutString = XdrOutString;
    OutSpace = Dummy;
    OutNewline = Dummy;
    OutTerm = XdrOutTerm;
#ifdef EXPERIMENTAL
    cky_DataSave(s, fp);
#else
    DataSave(s, fp);
#endif
}

static SEXP XdrLoad(FILE *fp)
{
    VersionId = 0;
    InInit = XdrInInit;
    InInteger = XdrInInteger;
    InReal = XdrInReal;
    InComplex = XdrInComplex;
    InString = XdrInString;
    InTerm = XdrInTerm;
#ifdef EXPERIMENTAL
    return cky_DataLoad(fp);
#else
    return DataLoad(fp);
#endif
}
#endif

/* Functions for Binary Pickling */

static void BinaryOutInteger(FILE *fp, int i)
{
    if (fwrite(&i, sizeof(int), 1, fp) != 1)
	error("a write error occured");
}

static int BinaryInInteger(FILE * fp)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error("a read error occured\n");
    return i;
}

static void BinaryOutReal(FILE *fp, double x)
{
    if (fwrite(&x, sizeof(double), 1, fp) != 1)
	error("a write error occured\n");
}

static double BinaryInReal(FILE * fp)
{
    double x;
    if (fread(&x, sizeof(double), 1, fp) != 1)
	error("a read error occured\n");
    return x;
}

static void BinaryOutComplex(FILE *fp, complex x)
{
	if (fwrite(&x, sizeof(complex), 1, fp) != 1)
		error("a write error occured\n");
}

static complex BinaryInComplex(FILE * fp)
{
    complex x;
    if (fread(&x, sizeof(complex), 1, fp) != 1)
	error("a read error occured\n");
    return x;
}

static void BinaryOutString(FILE *fp, char *s)
{
    int n = strlen(s) + 1;	/* NULL too */
    if (fwrite(s, sizeof(char), n, fp) != n)
	error("a write error occured\n");
}

static char *BinaryInString(FILE *fp)
{
    bufp = buf;
    do {
	*bufp = R_fgetc(fp);
    }
    while (*bufp++);
    return buf;
}

void BinarySave(SEXP s, FILE *fp)
{
    OutInit = Dummy;
    OutInteger = BinaryOutInteger;
    OutReal = BinaryOutReal;
    OutComplex = BinaryOutComplex;
    OutString = BinaryOutString;
    OutSpace = Dummy;
    OutNewline = Dummy;
    OutTerm = Dummy;
    DataSave(s, fp);
}

SEXP BinaryLoad(FILE *fp)
{
    VersionId = 0;
    InInit = Dummy;
    InInteger = BinaryInInteger;
    InReal = BinaryInReal;
    InComplex = BinaryInComplex;
    InString = BinaryInString;
    InTerm = Dummy;
    return DataLoad(fp);
}

SEXP BinaryLoadOld(FILE *fp, int version)
{
    VersionId = version;
    InInit = Dummy;
    InInteger = BinaryInInteger;
    InReal = BinaryInReal;
    InComplex = BinaryInComplex;
    InString = BinaryInString;
    InTerm = Dummy;
    return DataLoad(fp);
}

/*   Magic Numbers for R Save File Types   */

void R_WriteMagic(FILE *fp, int number)
{
    unsigned char buf[5];
    number = abs(number);
    buf[0] = (number/1000) % 10 + '0';
    buf[1] = (number/100) % 10 + '0';
    buf[2] = (number/10) % 10 + '0';
    buf[3] = number % 10 + '0';
    buf[4] = '\n';
    fwrite((char*)buf, sizeof(char), 5, fp);
}

int R_ReadMagic(FILE *fp)
{
    unsigned char buf[6];
    int d1, d2, d3, d4, d1234;
    fread((char*)buf, sizeof(char), 5, fp);
    /* Intel gcc seems to screw up a single expression here */
    d1 = (buf[3]-'0') % 10;
    d2 = (buf[2]-'0') % 10;
    d3 = (buf[1]-'0') % 10;
    d4 = (buf[0]-'0') % 10;
    return d1234 = d1 + 10 * d2 + 100 * d3 + 1000 * d4;
}

static void ReallocVector(SEXP s, int length)
{
    long size;
    switch (TYPEOF(s)) {
    case CHARSXP:
	size = 1 + BYTE2VEC(length + 1);
	break;
    case LGLSXP:
    case INTSXP:
	if (length <= 0) size = 0;
	else size = 1 + INT2VEC(length);
	break;
    case REALSXP:
	if (length <= 0) size = 0;
	else size = 1 + FLOAT2VEC(length);
	break;
    case CPLXSXP:
	if (length <= 0) size = 0;
	else size = 1 + COMPLEX2VEC(length);
	break;
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
	if (length <= 0) size = 0;
	else size = 1 + PTR2VEC(length);
	break;
    default:
	error("invalid type in ReallocVector\n"); size=0;
    }
    if (R_VMax - R_VTop < size)
	error("restore memory exhausted (should not happen)\n");

    LENGTH(s) = length;
    if (size > 0) {
	CHAR(s) = (char *) (R_VTop + 1);
	BACKPOINTER(*R_VTop) = s;
	R_VTop += size;
    }
    else CHAR(s) = (char*)0;
}

static void ReallocString(SEXP s, int length)
{
    long size = 1 + BYTE2VEC(length + 1);
    if (R_VMax - R_VTop < size)
	error("restore memory exhausted (should not happen)\n");
    if (TYPEOF(s) != CHARSXP)
	error("ReallocString: type conflict\n");
    CHAR(s) = (char*)(R_VTop + 1);
    LENGTH(s) = length;
    TAG(s) = R_NilValue;
    NAMED(s) = 0;
    ATTRIB(s) = R_NilValue;
    BACKPOINTER(*R_VTop) = s;
    R_VTop += size;
}

static void MarkSave(SEXP s)
{
    int i, len;

    if (s == R_NilValue
       || s == R_GlobalEnv
       || s == R_UnboundValue
       || s == R_MissingArg) return;

    if (s && !MARK(s)) {
	MARK(s) = 1;
	if (ATTRIB(s) != R_NilValue)
	    MarkSave(ATTRIB(s));

	switch (TYPEOF(s)) {
	case BUILTINSXP:
	case SPECIALSXP:
	    NSave++;
	    break;
	case SYMSXP:
	    NSymbol++;
	    break;
	case CHARSXP:
	    NSave++;
	    NVSize += 1 + BYTE2VEC(LENGTH(s) + 1);
	    break;
	case LGLSXP:
	case INTSXP:
	    NSave++;
	    NVSize += 1 + INT2VEC(LENGTH(s));
	    break;
	case REALSXP:
	    NSave++;
	    NVSize += 1 + FLOAT2VEC(LENGTH(s));
	    break;
	case CPLXSXP:
	    NSave++;
	    NVSize += 1 + COMPLEX2VEC(LENGTH(s));
	    break;
	case STRSXP:
	case VECSXP:
	case EXPRSXP:
	    NSave++;
	    NVSize += 1 + PTR2VEC(len=LENGTH(s));
	    for (i=0; i < len; i++)
		MarkSave(VECTOR(s)[i]);
	    break;
	case ENVSXP:
	    NSave++;
	    MarkSave(FRAME(s));
	    MarkSave(ENCLOS(s));
	    break;
	case CLOSXP:
	case PROMSXP:
	case LISTSXP:
	case LANGSXP:
	case DOTSXP:
	    NSave++;
	    MarkSave(TAG(s));
	    MarkSave(CAR(s));
	    MarkSave(CDR(s));
	    break;
	}
    }
}

static int NodeToOffset(SEXP s)
{
    if (s == R_NilValue) return -1;
    if (s == R_GlobalEnv) return -2;
    if (s == R_UnboundValue) return -3;
    if (s == R_MissingArg) return -4;
    return s - R_NHeap;;
}

static SEXP OffsetToNode(int offset)
{
    int l, m, r;

    if (offset == -1) return R_NilValue;
    if (offset == -2) return R_GlobalEnv;
    if (offset == -3) return R_UnboundValue;
    if (offset == -4) return R_MissingArg;

    /* binary search for offset */

    l = 0;
    r = NTotal - 1;
    do {
	m = (l + r) / 2;
	if (offset < OldOffset[m])
	    r = m - 1;
	else
	    l = m + 1;
    }
    while (offset != OldOffset[m] && l <= r);
    if (offset == OldOffset[m]) return NewAddress[m];

    error("unresolved node during restore\n");
    return R_NilValue;/* for -Wall */
}

static void DataSave(SEXP s, FILE *fp)
{
    int i, j, k, l, n;

    /* compute the storage requirements */
    /* and write these to the save file */
    /* NSymbol = # of symbols written */
    /* NSave = # of symbols written */
    /* NVSize = # of vector cells written */

    NSave = 0;
    NSymbol = 0;
    NVSize = 0;
    unmarkPhase();
    MarkSave(s);

    OutInit(fp);

    OutInteger(fp, NSymbol); OutSpace(fp);
    OutInteger(fp, NSave); OutSpace(fp);
    OutInteger(fp, NVSize); OutNewline(fp);

    /* write out any required symbols */

    k = 0; n = 0;
    for (i = 0; i < R_NSize; i++) {
	if (MARK(&R_NHeap[i])) {
	    if (TYPEOF(&R_NHeap[i]) == SYMSXP) {
		OutInteger(fp, n);
		OutSpace(fp);
		OutInteger(fp, NodeToOffset(&R_NHeap[i]));
		OutSpace(fp);
		OutString(fp, CHAR(PRINTNAME(&R_NHeap[i])));
		OutNewline(fp);
		k++;
	    }
	    n++;
	}
    }
    if (k != NSymbol || n != NSymbol+NSave)
	error("symbol count conflict\n");

    /* write out the forwarding address table */

    k = 0; n = 0;
    for (i = 0; i < R_NSize; i++) {
	if (MARK(&R_NHeap[i])) {
	    if (TYPEOF(&R_NHeap[i]) != SYMSXP) {
		OutInteger(fp, n);
		OutSpace(fp);
		OutInteger(fp, NodeToOffset(&R_NHeap[i]));
		OutNewline(fp);
		k++;
	    }
	    n++;
	}
    }
    if (k != NSave || n != NSymbol+NSave)
	error("node count conflict\n");

    k = 0; n = 0;
    for (i = 0; i < R_NSize; i++) {
	if (MARK(&R_NHeap[i])) {
	    if (TYPEOF(&R_NHeap[i]) != SYMSXP) {

		OutInteger(fp, n);
		OutSpace(fp);
		OutInteger(fp, TYPEOF(&R_NHeap[i]));
		OutSpace(fp);
		OutInteger(fp, OBJECT(&R_NHeap[i]));
		OutSpace(fp);
		OutInteger(fp,	LEVELS(&R_NHeap[i]));
		OutSpace(fp);
		OutInteger(fp,	NodeToOffset(ATTRIB(&R_NHeap[i])));
		OutSpace(fp);

		switch (TYPEOF(&R_NHeap[i])) {
		case LISTSXP:
		case LANGSXP:
		case CLOSXP:
		case PROMSXP:
		case ENVSXP:
		    OutInteger(fp, NodeToOffset(CAR(&R_NHeap[i])));
		    OutSpace(fp);
		    OutInteger(fp, NodeToOffset(CDR(&R_NHeap[i])));
		    OutSpace(fp);
		    OutInteger(fp, NodeToOffset(TAG(&R_NHeap[i])));
		    OutNewline(fp);
		    break;
		case SPECIALSXP:
		case BUILTINSXP:
		    OutInteger(fp, strlen(PRIMNAME(&R_NHeap[i])));
		    OutSpace(fp);
		    OutString(fp, PRIMNAME(&R_NHeap[i]));
		    OutNewline(fp);
		    break;
		case CHARSXP:
		    OutInteger(fp, LENGTH(&R_NHeap[i]));
		    OutSpace(fp);
		    OutString(fp, CHAR(&R_NHeap[i]));
		    OutNewline(fp);
		    break;
		case REALSXP:
		    l = LENGTH(&R_NHeap[i]);
		    OutInteger(fp, l);
		    OutNewline(fp);
		    for (j = 0; j < l; j++) {
			OutReal(fp, REAL(&R_NHeap[i])[j]);
			if ((j + 1) % 10 == 0 || j == l - 1)
			    OutNewline(fp);
			else
			    OutSpace(fp);
		    }
		    break;
		case CPLXSXP:
		    l = LENGTH(&R_NHeap[i]);
		    OutInteger(fp, l);
		    OutNewline(fp);
		    for (j = 0; j < l; j++) {
			OutComplex(fp, COMPLEX(&R_NHeap[i])[j]);
			if ((j + 1) % 10 == 0 || j == l - 1)
			    OutNewline(fp);
			else
			    OutSpace(fp);
		    }
		    break;
		case INTSXP:
		case LGLSXP:
		    l = LENGTH(&R_NHeap[i]);
		    OutInteger(fp, l);
		    OutNewline(fp);
		    for (j = 0; j < l; j++) {
			OutInteger(fp, INTEGER(&R_NHeap[i])[j]);
			if ((j + 1) % 10 == 0 || j == l - 1)
			    OutNewline(fp);
			else
			    OutSpace(fp);
		    }
		    break;
		case STRSXP:
		case VECSXP:
		case EXPRSXP:
		    l = LENGTH(&R_NHeap[i]);
		    OutInteger(fp, l);
		    OutNewline(fp);
		    for (j = 0; j < l; j++) {
			OutInteger(fp, NodeToOffset(VECTOR(&R_NHeap[i])[j]));
			if ((j + 1) % 10 == 0 || j == l - 1)
			    OutNewline(fp);
			else
			    OutSpace(fp);
		    }
		}
		k++;
	    }
	    n++;
	}
    }
    if (k != NSave) error("node count conflict\n");

    /* write out the offset of the list */

    OutInteger(fp, NodeToOffset(s));
    OutNewline(fp);

    OutTerm(fp);
}

static void RestoreSEXP(SEXP s, FILE *fp)
{
    unsigned int j;
    int len;

    TYPEOF(s) = InInteger(fp);

    if (VersionId) {
	switch(VersionId) {

	case 16:
	    /* In the version 0.16.1 -> 0.50 switch */
	    /* we really introduced complex values */
	    /* and found that numeric/complex numbers */
	    /* had to be contiguous.  Hence this switch */
	    if (TYPEOF(s) == STRSXP)
		TYPEOF(s) = CPLXSXP;
	    else if (TYPEOF(s) == CPLXSXP)
		TYPEOF(s) = STRSXP;
	    break;

	default:
	    error("restore compatibility error - no version %d compatibility\n", VersionId);
	}
    }

    /* Map old factors to new ...  (0.61->0.62) */
    if (TYPEOF(s) == 11 || TYPEOF(s) == 12)
	TYPEOF(s) = 13;

    OBJECT(s) = InInteger(fp);
    LEVELS(s) = InInteger(fp);
    ATTRIB(s) = OffsetToNode(InInteger(fp));
    switch (TYPEOF(s)) {
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case ENVSXP:
	CAR(s) = OffsetToNode(InInteger(fp));
	CDR(s) = OffsetToNode(InInteger(fp));
	TAG(s) = OffsetToNode(InInteger(fp));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	len = InInteger(fp);
	AllocBuffer(MAXELTSIZE - 1);
	PRIMOFFSET(s) = StrToInternal(InString(fp));
	break;
    case CHARSXP:
	LENGTH(s) = len = InInteger(fp);
	ReallocString(s, len);
	AllocBuffer(len);
	strcpy(CHAR(s), InString(fp));
	break;
    case REALSXP:
	LENGTH(s) = len = InInteger(fp);
	ReallocVector(s, len);
	for (j = 0; j < len; j++)
	    REAL(s)[j] = InReal(fp);
	break;
    case CPLXSXP:
	LENGTH(s) = len = InInteger(fp);
	ReallocVector(s, len);
	for (j = 0; j < len; j++)
	    COMPLEX(s)[j] = InComplex(fp);
	break;
    case INTSXP:
    case LGLSXP:
	LENGTH(s) = len = InInteger(fp);;
	ReallocVector(s, len);
	for (j = 0; j < len; j++)
	    INTEGER(s)[j] = InInteger(fp);
	break;
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
	LENGTH(s) = len = InInteger(fp);
	ReallocVector(s, len);
	for (j = 0; j < len; j++) {
	    VECTOR(s)[j] = OffsetToNode(InInteger(fp));
	}
	break;
    }
}

static SEXP DataLoad(FILE *fp)
{
    int i, j, vsmall, nsmall;
    char *vmaxsave, msg[512], s[256];;

    /* read in the size information */

    InInit(fp);

    NSymbol = InInteger(fp);
    NSave = InInteger(fp);
    NVSize = InInteger(fp);
    NTotal = NSymbol + NSave;

    /* allocate the forwarding-address tables */
    /* these are non-relocatable, so we must */
    /* save the current non-relocatable base */

    vmaxsave = vmaxget();
    OldOffset = (int*)R_alloc(NSymbol+NSave, sizeof(int));
    NewAddress = (SEXP*)R_alloc(NSymbol+NSave, sizeof(SEXP));
    for (i = 0 ; i < NTotal ; i++) {
	OldOffset[i] = 0;
	NewAddress[i] = R_NilValue;
    }

    /* read in the required symbols */
    /* expanding the symbol table and */
    /* computing the forwarding addresses */

    for (i = 0 ; i < NSymbol ; i++) {
	j = InInteger(fp);
	OldOffset[j] = InInteger(fp);
	AllocBuffer(MAXELTSIZE - 1);
	NewAddress[j] = install(InString(fp));
    }

    /* symbols are all installed */
    /* gc() and check space */

    gc();

    /* a gc after this point will be a disaster */
    /* because nothing will have been protected */

    if(DLstartup) {
	vsmall = NVSize - ((VECREC *)vmaxget() - R_VTop) ;
	nsmall = NSave - R_Collected;
	msg[0] = '\0';
	if(vsmall > 0) {
	    sprintf(s, "vector heap is too small to restore data -- need about %dM\n", (int)ceil((16.*(R_VSize + vsmall))/Mega));
	    strcat(msg, s);
	}
	if (nsmall > 0) {
	    sprintf(s, "cons heap is too small to restore data -- need about %dk\n", (int)ceil((R_NSize + nsmall)/1000.));
		    strcat(msg, s);
	}
	if(vsmall > 0 || nsmall > 0)
	   R_Suicide(msg);
    } else {
	if ((VECREC *)vmaxget() - R_VTop < NVSize)
	    error("vector heap is too small to restore data\n");

	if (R_Collected < NSave)
	    error("cons heap is too small to restore data\n");
    }


    /* build the full forwarding table */
    /* allocating SEXPs from the free list */

    for (i = 0 ; i < NSave ; i++) {
	j = InInteger(fp);
	OldOffset[j] = InInteger(fp);
	NewAddress[j] = R_FreeSEXP;
	R_FreeSEXP = CDR(R_FreeSEXP);
    }

    /* restore the saved nodes */

    for (i = 0 ; i < NSave ;  i++) {
	RestoreSEXP(NewAddress[InInteger(fp)], fp);
    }

    /* restore the heap */

    vmaxset(vmaxsave);

    /* clean the string buffer */
    AllocBuffer(-1);

    /* return the "top-level" object */
    /* this is usually a list */

    i = InInteger(fp);
    InTerm(fp);

    return OffsetToNode(i);
}

void R_SaveToFile(SEXP obj, FILE *fp, int ascii)
{
    if (ascii) {
	R_WriteMagic(fp, R_MAGIC_ASCII);
	AsciiSave(obj, fp);
    }
    else {
#ifdef HAVE_RPC_XDR_H
	R_WriteMagic(fp, R_MAGIC_XDR);
	XdrSave(obj, fp);
#else
	R_WriteMagic(fp, R_MAGIC_BINARY);
	BinarySave(obj, fp);
#endif
    }
}

SEXP R_LoadFromFile(FILE *fp, int startup)
{
    DLstartup = startup; /* different handling of errors */
    switch(R_ReadMagic(fp)) {
#ifdef HAVE_RPC_XDR_H
    case R_MAGIC_XDR:
	return(XdrLoad(fp));
#endif
    case R_MAGIC_BINARY:
	return(BinaryLoad(fp));
    case R_MAGIC_ASCII:
	return(AsciiLoad(fp));
    case R_MAGIC_BINARY_VERSION16:
	return(BinaryLoadOld(fp, 16));
    case R_MAGIC_ASCII_VERSION16:
	return(AsciiLoadOld(fp, 16));
    default:
	fclose(fp);
	error("restore file corrupted -- no data loaded\n");
	return(R_NilValue);/* for -Wall */
    }
}


/* Interpreter Interface Functions */

SEXP do_save(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t;
    int len, j;
    FILE *fp;

    checkArity(op, args);


    if (TYPEOF(CAR(args)) != STRSXP)
	errorcall(call, "first argument must be a character vector\n");
    if (TYPEOF(CADR(args)) != STRSXP)
	errorcall(call, "second argument must be a string\n");
    if (TYPEOF(CADDR(args)) != LGLSXP)
	errorcall(call, "third argument must be a logical vector\n");

    fp = R_fopen(R_ExpandFileName(CHAR(STRING(CADR(args))[0])), "wb");
    if (!fp)
	errorcall(call, "unable to open file\n");

    len = length(CAR(args));
    PROTECT(s = allocList(len));

    t = s;
    for (j = 0; j < len; j++, t = CDR(t)) {
	TAG(t) = install(CHAR(STRING(CAR(args))[j]));
	CAR(t) = findVar(TAG(t), R_GlobalContext->sysparent);
	if (CAR(t) == R_UnboundValue)
	    error("Object \"%s\" not found\n", CHAR(PRINTNAME(TAG(t))));
    }

    R_SaveToFile(s, fp, INTEGER(CADDR(args))[0]);

    UNPROTECT(1);
    fclose(fp);
    return R_NilValue;
}

/* These functions convert old (pairlist) lists into new */
/* (vectorlist) lists.	The conversion can be defeated by */
/* hiding things inside closures, but it is doubtful that */
/* anyone has done this. */

static SEXP ConvertPairToVector(SEXP);

static SEXP ConvertAttributes(SEXP attrs)
{
    SEXP ap = attrs;
    while (ap != R_NilValue) {
	if (TYPEOF(CAR(ap)) == LISTSXP)
	    CAR(ap) = ConvertPairToVector(CAR(ap));
	ap = CDR(ap);
    }
    return attrs;
}

#ifdef NOTYET
/* It may be that there are LISTXP hiding in closures. */
/* This will convert them. */

static SEXP ConvertEnvironment(SEXP env)
{
    SEXP frame = FRAME(env);
    while (frame != R_NilValue) {
	if (TYPEOF(CAR(frame)) == LISTSXP)
	    CAR(frame) = ConvertPairToVector(CAR(frame));
	frame = CDR(frame);
    }
    return env;
}
#endif

static SEXP ConvertPairToVector(SEXP obj)
{
    int i, n;
    switch (TYPEOF(obj)) {
    case LISTSXP:
	PROTECT(obj = PairToVectorList(obj));
	n = length(obj);
	for (i = 0; i < n; i++)
	    VECTOR(obj)[i] = ConvertPairToVector(VECTOR(obj)[i]);
	UNPROTECT(1);
	break;
    case VECSXP:
	break;
    default:
	;
    }
    ATTRIB(obj) = ConvertAttributes(ATTRIB(obj));
    return obj;
}

void R_LoadSavedData(FILE *fp, SEXP aenv)
{
    SEXP a, ans;
    ans = R_LoadFromFile(fp, 0);

    /* Store the components of the list in the Global Env */
    /* We either replace the existing objects in the Global */
    /* Environment or establish new bindings for them. */
    /* Note that we try to convert old "pairlist" objects */
    /* to new "pairlist" objects. */

    PROTECT(a = ans);
    while (a != R_NilValue) {
#ifdef OLD
	for (e = FRAME(aenv); e != R_NilValue ; e = CDR(e)) {
	    if (TAG(e) == TAG(a)) {
		CAR(e) = CAR(a);
		a = CDR(a);
		CAR(a) = ConvertPairToVector(CAR(a)); /* PAIRLIST conv */
		goto NextItem;
	    }
	}
	e = a;
	a = CDR(a);
	UNPROTECT(1);
	PROTECT(a);
	CDR(e) = FRAME(aenv);
	FRAME(aenv) = e;
	CAR(e) = ConvertPairToVector(CAR(e)); /* PAIRLIST conv */
    NextItem:
	;
#else
        defineVar(TAG(a), ConvertPairToVector(CAR(a)), aenv);
        a = CDR(a);
#endif
    }
    UNPROTECT(1);
}

SEXP do_load(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP fname, aenv;
    FILE *fp;

    checkArity(op, args);

    if (!isValidString(fname = CAR(args)))
	errorcall (call, "first argument must be a file name\n");

    /* GRW 1/26/99 GRW : added environment parameter so that */
    /* the loaded objects can be placed where desired  */

    aenv = CADR(args);
    if (TYPEOF(aenv) != ENVSXP && aenv != R_NilValue)
	error("invalid envir argument\n");

    /* Process the saved file to obtain a list of saved objects. */
    fp = R_fopen(R_ExpandFileName(CHAR(STRING(fname)[0])), "rb");
    if (!fp)
	errorcall(call, "unable to open file\n");
    R_LoadSavedData(fp, aenv);
    fclose(fp);
    return R_NilValue;
}

#ifdef HAVE_HDF5

#define STRING2REF_CONV "string->ref"
#define REF2STRING_CONV "ref->string"
#define ROWNAMES "row.names"

static herr_t
ref_string (hid_t sid, hid_t did, H5T_cdata_t *cdata,
	    size_t count, size_t stride,
	    void *buf, void *bkg,
	    hid_t dset_xfer_plid)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      SEXPREC *srcbuf[count];
      char *destbuf = buf;
      SEXPREC **recptr = srcbuf;
      size_t i;
      size_t maxlen = H5Tget_size (did);

      memcpy (srcbuf, buf, sizeof (srcbuf));

      for (i = 0; i < count; i++)
	{
	  strncpy (destbuf, CHAR (*recptr), maxlen);
	  recptr++;
	  destbuf += maxlen;
	}
    }
  return 0;
}

static herr_t
string_ref (hid_t sid, hid_t did, H5T_cdata_t *cdata,
	    size_t count, size_t stride,
	    void *buf, void *bkg,
	    hid_t dset_xfer_plid)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      size_t size = H5Tget_size (sid);
      unsigned char srcbuf[size * count], *srcptr = srcbuf;
      size_t i;

      memcpy (srcbuf, buf, sizeof (srcbuf));
      for (i = 0; i < count;i ++)
	{
	  ((SEXPREC **)buf)[i] = mkChar (srcptr);
	  srcptr += size;
	}
    }
  return 0;
}

struct permute_info {
  SEXP call;
  int writeflag;
  SEXPTYPE type;
  unsigned rank;
  hssize_t *dims;
  hssize_t *coord;
  hid_t dataset;
  hid_t memtid;
  hid_t space;
  hid_t mspace;
  void *buf;
};

static void
permute (struct permute_info *pinfo, unsigned dimnum)
{
  hssize_t i;

  if (dimnum < pinfo->rank)
    {
      for (i = 0; i < pinfo->dims[dimnum]; i++)
	{
	  pinfo->coord[dimnum] = i;
	  permute (pinfo, dimnum + 1);
	}
    }
  else
    {
      unsigned offset, mult;

      if (H5Sselect_elements (pinfo->space, H5S_SELECT_SET, 1,
			      (const hssize_t **) pinfo->coord) < 0)
	errorcall (pinfo->call, "Unable to select file elements");

      offset = pinfo->coord[0];
      mult = 1;
      for (i = 1; i < pinfo->rank; i++)
	{
	  mult *= pinfo->dims[i - 1];
	  offset += pinfo->coord[i] * mult;
	}

      {
	void *pointaddr;

	switch (pinfo->type)
	  {
	  case STRSXP: case VECSXP:
	    pointaddr = &((SEXPREC **)pinfo->buf)[offset];
	    break;
	  case REALSXP:
	    pointaddr = &((double *)pinfo->buf)[offset];
	    break;
	  case INTSXP: case LGLSXP:
	    pointaddr = &((int *)pinfo->buf)[offset];
	    break;
	  default:
	    errorcall (pinfo->call, "No support for R type: %d", pinfo->type);
	    pointaddr = &offset;/* unreached; -Wall */
	  }

	if (pinfo->writeflag)
	  {
	    if (H5Dwrite (pinfo->dataset,
			  pinfo->memtid,
			  pinfo->mspace,
			  pinfo->space,
			  H5P_DEFAULT,
			  pointaddr) < 0)
	      errorcall (pinfo->call, "Unable to write dataset");
	  }
	else
	  {
	    if (H5Dread (pinfo->dataset,
			 pinfo->memtid,
			 pinfo->mspace,
			 pinfo->space,
			 H5P_DEFAULT,
			 pointaddr) < 0)
	      errorcall (pinfo->call, "Unable to read dataset");
	  }
      }
    }
}

static hid_t
make_sexp_ref_type (SEXP call)
{
  hid_t memtid;

  if ((memtid = H5Tcopy (H5T_STD_REF_OBJ)) < 0)
    errorcall (call, "Unable to copy H5T_STD_REF_OBJ");
  if (H5Tset_size (memtid, sizeof (SEXPREC *)) < 0)
    errorcall (call, "unable to set size of reference type");
  return memtid;
}

static hid_t
get_string_type (SEXP call, SEXP vec)
{
  hid_t stid;
  unsigned vecpos;
  size_t maxstrlen = 0;

  for (vecpos = 0; vecpos < LENGTH (vec); vecpos++)
    {
      SEXP stritem = STRING (vec)[vecpos];

      if (LENGTH (stritem) > maxstrlen)
	maxstrlen = LENGTH (stritem);
    }

  if ((stid = H5Tcopy (H5T_C_S1)) < 0)
    errorcall (call, "Cannot copy string type");

  if (H5Tset_size (stid, maxstrlen + 1) < 0)
    errorcall (call, "Cannot set size of string type");

  return stid;
}


static void
vector_io (SEXP call, int writeflag, hid_t dataset, hid_t space, SEXP obj)
{
  int rank = H5Sget_simple_extent_ndims (space);
  hsize_t mdims[1] = {1};
  hsize_t dims[rank], maxdims[rank];
  SEXPTYPE type = TYPEOF (obj);
  hid_t memtid, tid, mspace;
  void *buf;

  if ((tid = H5Dget_type (dataset)) < 0)
    errorcall (call, "Unable to get type for dataset");

  switch (type) {

  case STRSXP:
      memtid = make_sexp_ref_type (call);
      buf = STRING (obj);
      break;
  case REALSXP:
      memtid = H5T_NATIVE_DOUBLE;
      buf = REAL (obj);
      break;
  case INTSXP:
      memtid = H5T_NATIVE_INT;
      buf = INTEGER (obj);
      break;
  case LGLSXP:
      memtid = H5T_NATIVE_UINT;
      buf = INTEGER (obj);
      break;
  default:
      errorcall (call, "Can't get type for R type: %d (IO)", type);
      /* unreached (-Wall): */ memtid = tid;  buf = &tid;
  }

  if (H5Sget_simple_extent_dims (space, dims, maxdims) < 0)
    errorcall (call, "Unable to get dimensions of space");

  if ((mspace = H5Screate_simple (1, mdims, NULL)) < 0)
    errorcall (call, "Unable to create point space");

  {
    struct permute_info pinfo;
    hssize_t coord[rank];

    pinfo.call = call;
    pinfo.writeflag = writeflag;
    pinfo.type = type;
    pinfo.rank = rank;
    pinfo.coord = coord;
    pinfo.dims = dims;
    pinfo.dataset = dataset;
    pinfo.memtid = memtid;
    pinfo.space = space;
    pinfo.mspace = mspace;
    pinfo.buf = buf;

    permute (&pinfo, 0);
  }

  if (H5Sclose (mspace) < 0)
    errorcall (call, "Unable to close point space");

  if (type == STRSXP) {
      if (H5Tclose (memtid) < 0)
	errorcall (call, "Unable to close string reference type");
    }
}

static void
hdf5_save_attributes (SEXP call, hid_t loc_id, SEXP val)
{
  SEXP l;

  for (l = ATTRIB (val); l != R_NilValue; l = CDR (l))
    {
      SEXP attr = CAR (l);
      SEXPTYPE type = TYPEOF (attr);
      const char *name = CHAR (PRINTNAME (TAG (l)));
      void *buf;
      hid_t tid, memtid;
      hid_t sid, aid;
      unsigned count = LENGTH (attr);
      /*SEXPREC *stringptrs[count];*/

      if (TAG (l) == R_RowNamesSymbol
	  || TAG (l) == R_ClassSymbol
	  || TAG (l) == R_NamesSymbol
	  || TAG (l) == R_DimNamesSymbol)
	continue;
      {
	hsize_t dims[1];

	dims[0] = count;

	if ((sid = H5Screate_simple (1, dims, NULL)) < 0)
	  errorcall (call,
		     "unable to create vector space for attribute `%s'", name);
      }

      if (type == STRSXP)
	{
	  memtid = make_sexp_ref_type (call);
	  tid = get_string_type (call, attr);
	  buf = STRING (attr);
	}
      else if (type == LGLSXP)
	{
	  memtid = H5T_NATIVE_INT;
	  tid = H5Tcopy (H5T_NATIVE_UINT);
	  H5Tset_precision (tid, 1);
	  H5Tset_size (tid, 1);
	  buf = INTEGER (attr);
	}
     else if (type == INTSXP)
	{
	  memtid = H5T_NATIVE_INT;
	  tid = H5T_NATIVE_INT;
	  buf = INTEGER (attr);
	}
      else if (type == REALSXP)
	{
	  memtid = H5T_NATIVE_DOUBLE;
	  tid = H5T_NATIVE_DOUBLE;
	  buf = REAL (attr);
	}
      else
	abort ();

      if ((aid = H5Acreate (loc_id, name, tid, sid, H5P_DEFAULT)) < 0)
	errorcall (call, "unable to create attribute `%s'", name);

      if (H5Awrite (aid, memtid, buf) < 0)
	errorcall (call, "unable to write attribute `%s'", name);

      if (H5Aclose (aid) < 0)
	errorcall (call, "unable to close attribute `%s'", name);

      if (type == STRSXP)
	{
	  if (H5Tclose (memtid) < 0)
	    errorcall (call,
		       "unable to close string reference type `%s'",
		       name);
	}
      if (type == LGLSXP || type == STRSXP)
	{
	  if (H5Tclose (tid) < 0)
	    errorcall (call, "unable to close output type `%s'", name);
	}
      if (H5Sclose (sid) < 0)
	errorcall (call, "unable to close space for attribute `%s'", name);
    }
}

static void
hdf5_write_vector (SEXP call, hid_t id, const char *symname, SEXP val)
{
  unsigned i, rank;
  SEXP dimvec;
  hid_t space, dataset;
  SEXPTYPE type = TYPEOF (val);
  hid_t tid;

  dimvec = getAttrib (val, R_DimSymbol);
  rank = (dimvec == R_NilValue) ? 1 : LENGTH (dimvec);

  {
    hsize_t dims[rank];

    if (rank > 1)
      for (i = 0; i < rank; i++)
	dims[i] = INTEGER (dimvec)[i];
    else
      dims[0] = length(val);

    if ((space = H5Screate_simple (rank, dims, NULL)) < 0)
      errorcall (call, "Unable to create file dataspace");

    if (type == STRSXP)
      tid = get_string_type (call, val);
    else if (type == LGLSXP)
      {
	tid = H5Tcopy (H5T_NATIVE_UINT);
	H5Tset_precision (tid, 1);
	H5Tset_size (tid, 1);
      }
    else if (type == INTSXP)
      tid = H5T_NATIVE_INT;
    else if (type == REALSXP)
      tid = H5T_NATIVE_DOUBLE;
    else {
      errorcall (call, "Can't get type for R type: %d (Creating)", type);
      tid = H5T_NATIVE_INT;/*unreached; -Wall*/
    }

    if ((dataset = H5Dcreate (id,
			      symname,
			      tid,
			      space,
			      H5P_DEFAULT)) < 0)
      errorcall (call, "Unable to create dataset");

    vector_io (call, TRUE, dataset, space, val);
    hdf5_save_attributes (call, dataset, val);

    if (type == LGLSXP || type == STRSXP)
      if (H5Tclose (tid) < 0)
	errorcall (call, "Unable to close type");

    if (H5Dclose (dataset) < 0)
      errorcall (call, "Unable to close dataset");
    if (H5Sclose (space) < 0)
      errorcall (call, "Unable to close space");
  }
}

static void
hdf5_write_string (SEXP call, hid_t fid, const char *symname, const char *str)
{
  hid_t stringtype;
  hid_t dataset;
  hid_t dataspace;

  dataspace = H5Screate (H5S_SCALAR);

  stringtype = H5Tcopy (H5T_C_S1);
  H5Tset_size (stringtype, strlen (str) + 1);

  if ((dataset = H5Dcreate (fid,
			    symname,
			    stringtype,
			    dataspace,
			    H5P_DEFAULT)) < 0)
    errorcall (call, "Unable to create dataset");

  if (H5Dwrite (dataset,
		stringtype,
		H5S_ALL,
		H5S_ALL,
		H5P_DEFAULT,
		str) < 0)
    errorcall (call, "Unable to write dataset");

  H5Dclose (dataset);
  H5Sclose (dataspace);
  H5Tclose (stringtype);
}

static unsigned
align (unsigned offset, unsigned alignto)
{
#if 0
  unsigned mask = alignto - 1;

  if ((offset & mask) == 0)
    return offset;
  else
    return (offset + alignto) & ~mask;
#else
  return offset;
#endif
}

static void
create_rownames_dataset_attribute (SEXP call, hid_t dataset, SEXP rownames)
{
  hid_t stringtid = get_string_type (call, rownames);
  hid_t rtid = make_sexp_ref_type (call);
  hid_t rnattrib, rndataspace;
  unsigned rowcount = LENGTH (rownames);
  hsize_t dims[1];

  dims[0] = rowcount;

  if ((rndataspace = H5Screate_simple (1, dims, NULL)) < 0)
    errorcall (call, "Unable to create row names vector space");

  if ((rnattrib = H5Acreate (dataset, ROWNAMES,
			     stringtid, rndataspace, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to create row names dataset");

  if (H5Awrite (rnattrib, rtid, STRING (rownames)) < 0)
    errorcall (call, "unable to write row names dataset");

  if (H5Aclose (rnattrib) < 0)
    errorcall (call, "unable to close row names dataset");

  if (H5Sclose (rndataspace) < 0)
    errorcall (call, "unable to close row names dataspace");

  if (H5Tclose (stringtid) < 0)
    errorcall (call, "unable to close row names string type");

  if (H5Tclose (rtid) < 0)
    errorcall (call, "unable to close reference type");
}

static void
hdf5_save_object (SEXP call, hid_t fid, const char *symname, SEXP val)
{
  if (isFrame (val))
    {
      unsigned colcount = length (val), pos;
      size_t offsets[colcount];
      hid_t hdftypes[colcount];
      hid_t hdfbooltype;
      size_t size = 0;

      if ((hdfbooltype = H5Tcopy (H5T_NATIVE_UINT)) < 0)
	errorcall (call, "Cannot copy unsigned integer type");
      if (H5Tset_precision (hdfbooltype, 1) < 0)
	errorcall (call, "Cannot set precision of boolean type");
      if (H5Tset_size (hdfbooltype, 1) < 0)
	errorcall (call, "Cannot set size of boolean type");

      for (pos = 0; pos < colcount; pos++)
	{
	  SEXPTYPE type = TYPEOF (VECTOR (val)[pos]);

	  switch (type)
	    {
	    case REALSXP:
	      hdftypes[pos] = H5T_NATIVE_DOUBLE;
	      break;
	    case INTSXP:
	      hdftypes[pos] = H5T_NATIVE_INT;
	      break;
	    case STRSXP:
	      hdftypes[pos] = get_string_type (call, VECTOR (val)[pos]);
	      break;
	    case LGLSXP:
	      hdftypes[pos] = hdfbooltype;
	      break;
	    default:
	      errorcall (call,
			 "No support for converting R type: %d to HDF5",
			 type);
	      break;
	    }
	  if (H5Tget_class (hdftypes[pos]) == H5T_STRING)
	    size = align (size, 8);
	  else
	    size = align (size, H5Tget_size (hdftypes[pos]));
	  offsets[pos] = size;
	  size += H5Tget_size (hdftypes[pos]);
	}
      size = align (size, H5Tget_size (hdftypes[0]));
      {
	hid_t ctid;
	hid_t dataset;
	hid_t dataspace;
	unsigned rowcount = length (VECTOR (val)[0]);
	{
	  hsize_t dims[1];

	  dims[0] = rowcount;
	  if ((dataspace = H5Screate_simple (1, dims, NULL)) < 0)
	    errorcall (call, "Unable to create dataframe vector space");
	}

	{
	  SEXP colnames = getAttrib (val, R_NamesSymbol);

	  if ((ctid = H5Tcreate (H5T_COMPOUND, size)) < 0)
	    errorcall (call, "unable to create compound type");

	  for (pos = 0; pos < colcount; pos++)
	    if (H5Tinsert (ctid,
			   CHAR (STRING (colnames)[pos]),
			   offsets[pos],
			   hdftypes[pos]) < 0)
	      errorcall (call, "unable to insert type into compound type");
	}
	if (H5Tpack (ctid) < 0)
	  errorcall (call, "Unable to pack type");

	if (H5Tlock (ctid) < 0)
	  errorcall (call, "Unable to lock type");

	if ((dataset = H5Dcreate (fid, symname,
				  ctid, dataspace, H5P_DEFAULT)) < 0)
	  errorcall (call, "unable to create dataframe dataset");

	{
	  unsigned ri;
	  unsigned char buf[rowcount][size];

	  for (ri = 0; ri < rowcount; ri++)
	    for (pos = 0; pos < colcount; pos++)
	      {
		SEXP item = VECTOR (val)[pos];
		SEXPTYPE type = TYPEOF (item);
		void *ptr = &buf[ri][offsets[pos]];

		switch (type)
		  {
		  case REALSXP:
		    memcpy (ptr, &REAL (item)[ri], sizeof (double));
		    break;
		  case INTSXP:
		    memcpy (ptr, &INTEGER (item)[ri], sizeof (int));
		    break;
		  case STRSXP:
		    {
		      SEXP stritem = STRING (item)[ri];
		      /*size_t len = LENGTH (stritem);*/

		      memset (ptr, 0, H5Tget_size (hdftypes[pos]));
		      strcpy ((char *)ptr, CHAR (stritem));
		    }
		    break;
		  case LGLSXP:
		    *(unsigned char *)ptr = INTEGER (item)[ri];
		    break;
		  default:
		    abort ();
		  }
	      }
	  if (H5Dwrite (dataset,
			ctid,
			dataspace,
			dataspace,
			H5P_DEFAULT,
			buf) < 0)
	    errorcall (call, "Unable to write dataframe");
	}
	hdf5_save_attributes (call, dataset, val);
	{
	  SEXP rownames = getAttrib (val, R_RowNamesSymbol);

	  if (rownames != R_NilValue)
	    create_rownames_dataset_attribute (call, dataset, rownames);
	}
	if (H5Dclose (dataset) < 0)
	  errorcall (call, "Cannot close dataset");
	if (H5Sclose (dataspace) < 0)
	  errorcall (call, "Cannot close dataspace");
      }

      for (pos = 0; pos < colcount; pos++)
	if (H5Tget_class (hdftypes[pos]) == H5T_STRING)
	  if (H5Tclose (hdftypes[pos]) < 0)
	    errorcall (call, "Cannot close string type");
      if (H5Tclose (hdfbooltype) < 0)
	errorcall (call, "Cannot close boolean type");

    }
  else if (isNull (val))
    {
    }
  else
    {
      SEXPTYPE type = TYPEOF (val);

      switch (type)
	{
	case LGLSXP: case INTSXP: case REALSXP: case STRSXP:
	  hdf5_write_vector (call, fid, symname, val);
	  break;
	case LISTSXP: case VECSXP:
	  {
	    unsigned len = length (val);
	    hid_t gid;
	    unsigned pos;
	    char buf[(sizeof (pos) * 8 / 3 + 1) + 1];

	    if ((gid = H5Gcreate (fid, symname, len * 8)) < 0)
	      errorcall (call, "unable to create group");

	    if (type == LISTSXP)
	      {
		SEXP l;

		for (l = val, pos = 0; l != R_NilValue; l = CDR (l), pos++)
		  {
		    SEXP s = CAR (l);

		    if (!isNull (TAG (l)))
		      hdf5_save_object (call, gid,
				       CHAR (PRINTNAME (TAG (l))), s);
		    else
		      {
			sprintf (buf, "%u", pos);
			hdf5_save_object (call, gid, buf, s);
		      }
		  }
	      }
	    else
	      {
		for (pos = 0; pos < len; pos++)
		  {
		    SEXP s = VECTOR (val)[pos];
		    SEXP names = getAttrib (val, R_NamesSymbol);

		    if (!isNull (names))
		      hdf5_save_object (call, gid,
				       CHAR (STRING (names) [pos]),
				       s);
		    else
		      {
			sprintf (buf, "%u", pos);
			hdf5_save_object (call, gid, buf, s);
		      }
		  }
	      }
	    hdf5_save_attributes (call, gid, val);

	    if (H5Gclose (gid) < 0)
	      errorcall (call, "unable to close group");
	  }
	  break;
	case SYMSXP:
	  {
	    const char *pn = CHAR (PRINTNAME (val));

	    hdf5_write_string (call, fid, symname, pn);
	  }
	  break;
	default:
	  errorcall (call, "unhandled type: %d", type);
	  break;
	}
    }
}


static void
hdf5_save_symbol (SEXP call, hid_t fid, SEXP sym, SEXP env)
{
  SEXP val;

  val = findVar (sym, env);

  hdf5_save_object (call, fid, CHAR (PRINTNAME (sym)), val);
}

SEXP do_hdf5save (SEXP call, SEXP op, SEXP args, SEXP env)
{
  const char *path;
  hid_t fid;
  SEXP s;

  checkArity (op, args);

  if (length (args) < 2)
    errorcall (call, "Two arguments are required: HDF-path and an object)");

  if (TYPEOF (CAR (args)) != STRSXP)
    errorcall (call, "first argument must be a pathname\n");

  path = CHAR (STRING (CAR (args))[0]);

  H5dont_atexit ();

  if (H5Tregister (H5T_PERS_SOFT,
		   REF2STRING_CONV,
		   H5T_STD_REF_OBJ,
		   H5T_C_S1, ref_string) < 0)
    errorcall (call, "Unable to register ref->string converter");

  if ((fid = H5Fcreate (path, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to create HDF file: %s", path);

  for (s = CDR (args); s != R_NilValue; s = CDR (s))
    hdf5_save_symbol (call, fid, CAR (s), env);

  if (H5Fclose (fid) < 0)
    errorcall (call, "unable to close HDF file: %s", path);

  if (H5Tunregister (H5T_PERS_SOFT, REF2STRING_CONV, -1, -1, ref_string) < 0)
    errorcall (call, "Unable to unregister ref->string converter");

  return R_NilValue;
}

struct hdf5_iterate_info {
  SEXP call;
  void (*add) (struct hdf5_iterate_info *, const char *, SEXP);
  SEXP env;
  SEXP ret;
};

static void
add_to_list (struct hdf5_iterate_info *iinfo, const char *name, SEXP obj)
{
  PROTECT (iinfo->ret);
  iinfo->ret = CONS (obj, iinfo->ret);
  TAG (iinfo->ret) = install ((char *) name);
  UNPROTECT (1);
}


static SEXP
collect (SEXP call, hid_t id, H5G_iterate_t iterate_func, SEXP env)
{
  struct hdf5_iterate_info iinfo;

  iinfo.call = call;
  iinfo.add = add_to_list;
  iinfo.ret = R_NilValue;
  iinfo.env = env;

  if (H5Giterate (id, ".", NULL, iterate_func, &iinfo) < 0)
    errorcall (call, "unable to collect HDF group");

  {
    SEXP nl = R_NilValue, l;
    SEXP rl = iinfo.ret;

    PROTECT (rl);
    l = rl;
    while (l != R_NilValue)
      {
	PROTECT (nl);
	nl = CONS (CAR (l), nl);
	TAG (nl) = TAG (l);
	UNPROTECT (1);
	l = CDR (l);
      }
    UNPROTECT (1);
    return nl;
  }
}

static void
load_rownames_dataset_attribute (SEXP call, hid_t dataset, SEXP vec)
{
  hid_t rnattrib, rnspace, rntid;
  unsigned rowcount;
  SEXP rownames;
  H5E_auto_t errfunc;
  void *client_data;

  H5Eset_auto (NULL, NULL);
  rnattrib = H5Aopen_name (dataset, ROWNAMES);
  H5Eget_auto (&errfunc, &client_data);
  H5Eset_auto (errfunc, client_data);

  if (rnattrib < 0)
    return;

  if ((rnspace = H5Aget_space (rnattrib)) < 0)
    errorcall (call, "could not get space for rownames attribute");

  if ((rntid = H5Aget_type (rnattrib)) < 0)
    errorcall (call, "could not get element type of rownames attribute");

  if (H5Sget_simple_extent_ndims (rnspace) != 1)
    errorcall (call, "rownames space should be of rank 1");

  {
    hsize_t dims[1], maxdims[1];

    if (H5Sget_simple_extent_dims (rnspace, dims, maxdims)< 0)
      errorcall (call, "can't get attribute space dims");
    rowcount = dims[0];
  }
  PROTECT (rownames = allocVector (STRSXP, rowcount));
  {
    SEXPREC *strptrs[rowcount];
    unsigned ri;
    hid_t rtid = make_sexp_ref_type (call);

    for (ri = 0; ri < rowcount; ri++)
      strptrs[ri] = STRING (rownames)[ri];

    if (H5Aread (rnattrib, rtid, strptrs) < 0)
      errorcall (call, "can't read rownames");

    for (ri = 0; ri < rowcount; ri++)
      STRING (rownames)[ri] = strptrs [ri];
  }
  setAttrib (vec, R_RowNamesSymbol, rownames);
  UNPROTECT (1);
}

struct hdf5_attribute_info {
  SEXP call;
  SEXP obj;
  const char *name;
};

static herr_t
hdf5_process_attribute (hid_t loc_id, const char *attrName, void *data)
{
  struct hdf5_attribute_info *ainfo = data;
  hid_t aid, sid, tid;
  H5T_class_t class;
  size_t tid_size;

  if ((aid = H5Aopen_name (loc_id, attrName)) < 0)
    errorcall (ainfo->call, "could not open attribute `%s'", attrName);

  if ((sid = H5Aget_space (aid)) < 0)
    errorcall (ainfo->call, "could not open space of attribute `%s'",
	       attrName);

  if ((tid = H5Aget_type (aid)) < 0)
    errorcall (ainfo->call, "could not get type of attribute `%s'", attrName);

  if ((tid_size = H5Tget_size (tid)) < 0)
    errorcall (ainfo->call, "could not get size of attribute `%s' tid",
	       attrName);

  if ((class = H5Tget_class (tid)) < 0)
    errorcall (ainfo->call, "could not get type class of attribute `%s'",
	       attrName);

  {
    int rank;

    if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
      errorcall (ainfo->call,
		 "could not get rank of attribute space `%s'",
		 attrName);
    {
      hsize_t dims[rank];

      if (H5Sget_simple_extent_dims (sid, dims, NULL) < 0)
	errorcall (ainfo->call,
		   "could not get extent of attribute space `%s'",
		   attrName);

      if (rank == 1)
	{
	  unsigned count = dims[0];
	  SEXPTYPE type;
	  hid_t memtid;
	  SEXP vec;
	  void *buf;

	  switch (class)
	    {
	    case H5T_INTEGER:
	      type = (tid_size == 1) ? LGLSXP : INTSXP;
	      memtid = H5Tcopy (H5T_NATIVE_INT);
	      break;
	    case H5T_FLOAT:
	      type = REALSXP;
	      memtid = H5Tcopy (H5T_NATIVE_DOUBLE);
	      break;
	    case H5T_STRING:
	      type = STRSXP;
	      memtid = make_sexp_ref_type (ainfo->call);
	      break;
	    default:
	      warningcall (ainfo->call, "skipping attribute `%s' due to type",
			   attrName);
	      goto done;
	    }
	  PROTECT (vec = allocVector (type, count));
	  switch (class)
	    {
	    case H5T_INTEGER:
	      buf = INTEGER (vec);
	      break;
	    case H5T_FLOAT:
	      buf = REAL (vec);
	      break;
	    case H5T_STRING:
	      buf = STRING (vec);
	      break;
	    default:
	      abort ();
	    }
	  if (H5Aread (aid, memtid, buf) < 0)
	    errorcall (ainfo->call, "unable to read attribute `%s'", attrName);

	  if (!isNull (ainfo->obj))
	    setAttrib (ainfo->obj, install ((char *) attrName), vec);

	  UNPROTECT (1);

	  if (H5Tclose (memtid) < 0)
	    errorcall (ainfo->call,
		       "unable to close reference type in attribute `%s'",
		       attrName);
	}
      else
	warningcall (ainfo->call, "skipping attribute `%s' due to rank",
		     attrName);
    }
  }
 done:
  if (H5Sclose (sid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s' space", attrName);
  if (H5Tclose (tid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s' type", attrName);
  if (H5Aclose (aid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s'", attrName);
  return 0;
}

static void
hdf5_load_attributes (SEXP call, hid_t id, SEXP obj, const char *name)
{
  unsigned idx = 0;
  struct hdf5_attribute_info ainfo;

  ainfo.call = call;
  ainfo.obj = obj;
  ainfo.name = name;

  if (H5Aiterate (id, &idx, hdf5_process_attribute, &ainfo) < 0)
    errorcall (call, "unable to iterate over attributes");
}

static herr_t
hdf5_process_object (hid_t id, const char *name, void *client_data)
{
  struct hdf5_iterate_info *iinfo = client_data;

  H5G_stat_t statbuf;

  if (H5Gget_objinfo (id, name, 1, &statbuf) < 0)
    errorcall (iinfo->call, "Cannot query object `%s'", name);

  if (statbuf.type == H5G_GROUP)
    {
      SEXP l;
      hid_t gid = H5Gopen (id, name);

      if (gid < 0)
	errorcall (iinfo->call, "unable to open group `%s'", name);

      PROTECT (l = collect (iinfo->call, gid, hdf5_process_object, iinfo->env));
      iinfo->add (iinfo, name, l);

      hdf5_load_attributes (iinfo->call, gid, l, name);
      UNPROTECT (1);

      if (H5Gclose (gid) < 0)
	errorcall (iinfo->call, "unable to close group");
    }
  else if (statbuf.type == H5G_DATASET)
    {
      hid_t dataset, space, tid;
      int rank;
      SEXPTYPE type = NILSXP;
      /*H5T_class_t class;*/

      if ((dataset = H5Dopen (id, name)) < 0)
	errorcall (iinfo->call, "unable to load dataset `%s'", name);

      if ((tid = H5Dget_type (dataset)) < 0)
	errorcall (iinfo->call, "unable to get dataset type");

      switch (H5Tget_class (tid))
	{
	case H5T_INTEGER:
	  if (H5Tget_precision (tid) == 1)
	    type = LGLSXP;
	  else
	    type = INTSXP;
	  break;
	case H5T_FLOAT:
	  type = REALSXP;
	  break;
	case H5T_STRING:
	  type = STRSXP;
	  break;
	case H5T_COMPOUND:
	  type = VECSXP;
	  break;
	default:
	  errorcall (iinfo->call, "can't handle hdf type %d", tid);
	  break;
	}

      if ((space = H5Dget_space (dataset)) < 0)
	errorcall (iinfo->call, "unable to get dataset space");

      if (H5Sis_simple (space) != TRUE)
	errorcall (iinfo->call, "space not simple");

      if ((rank = H5Sget_simple_extent_ndims (space)) < 0)
	errorcall (iinfo->call, "unable to get space rank");

      {
	hsize_t dims[rank];
	hsize_t maxdims[rank];

	if (H5Sget_simple_extent_dims (space, dims, maxdims) < 0)
	  errorcall (iinfo->call, "unable to get space extent");

	if (type == VECSXP && rank == 1)
	  {
	    unsigned colcount = H5Tget_nmembers (tid), ci;
	    SEXP vec;
	    size_t size = H5Tget_size (tid);
	    unsigned ri, rowcount = dims[0];
	    char buf[rowcount][size];
	    hid_t rtid = make_sexp_ref_type (iinfo->call);
	    SEXP names;

	    if (H5Dread (dataset, tid, space, space, H5P_DEFAULT,
			 buf) < 0)
	      errorcall (iinfo->call, "can't read compound data vector");

	    PROTECT (vec = allocVector (VECSXP, colcount));
	    PROTECT (names = allocVector (STRSXP, colcount));

	    for (ci = 0; ci < colcount; ci++)
	      {
		hid_t ctid = H5Tget_member_type (tid, ci);
		H5T_class_t class = H5Tget_class (ctid);
		size_t csize = H5Tget_size (ctid);
		size_t coffset = H5Tget_member_offset (tid, ci);
		SEXPREC **rowptr = &VECTOR (vec)[ci];
		unsigned char itembuf[size]; /* for overrun */

#define VECLOOP(vectype, vecref, dtid) \
  { \
    size_t dsize = H5Tget_size (dtid); \
    for (ri = 0; ri < rowcount; ri++) \
      { \
	memcpy (itembuf, &buf[ri][coffset], csize); \
	if (H5Tconvert (ctid, dtid, 1, itembuf, NULL, H5P_DEFAULT) < 0) \
	  errorcall (iinfo->call, "type conversion failed"); \
	memcpy (&vecref (*rowptr)[ri], itembuf, dsize); \
      } \
  }

		{
		  char *colname = H5Tget_member_name (tid, ci);

		  if (colname)
		    STRING (names)[ci] = mkChar (colname);
		}
		switch (class) {
		  case H5T_INTEGER:
		    {
		      SEXPTYPE type = (csize == 1) ? LGLSXP : INTSXP;

		      *rowptr = allocVector (type, rowcount);
		      VECLOOP (type, INTEGER, H5T_NATIVE_INT);
		    }
		    break;
		  case H5T_FLOAT:
		    *rowptr = allocVector (REALSXP, rowcount);
		    VECLOOP (REALSXP, REAL, H5T_NATIVE_DOUBLE);
		    break;
		  case H5T_STRING:
		    *rowptr = allocVector (STRSXP, rowcount);
		    VECLOOP (STRSXP, STRING, rtid);
		    break;
		  default:
		    errorcall (iinfo->call, "can't handle hdf class %d",
			       class);
		  }
	      }
	    load_rownames_dataset_attribute (iinfo->call, dataset, vec);
	    setAttrib (vec, R_NamesSymbol, names);
	    UNPROTECT (1);
	    setAttrib (vec, R_ClassSymbol, mkString ("data.frame"));
	    iinfo->add (iinfo, name, vec);
	    hdf5_load_attributes (iinfo->call, dataset, vec, name);
	    UNPROTECT (1);
	    if (H5Tclose (rtid) < 0)
	      errorcall (iinfo->call, "could not close reference type");
	  }
	else
	  {
	    SEXP obj;

	    PROTECT (obj = ((rank == 1)
			    ? allocVector (type, dims[0])
			    : allocMatrix (type, dims[0], dims[1])));
	    vector_io (iinfo->call, FALSE, dataset, space, obj);
	    iinfo->add (iinfo, name, obj);
	    hdf5_load_attributes (iinfo->call, dataset, obj, name);
	    UNPROTECT (1);
	  }
      }
      if (H5Sclose (space) < 0)
	errorcall (iinfo->call, "unable to close dataspace");
      if (H5Tclose (tid) < 0)
	errorcall (iinfo->call, "unable to close datatype");
      if (H5Dclose (dataset) < 0)
	errorcall (iinfo->call, "unable to close dataset");
    }
  else
    errorcall (iinfo->call, "no support for HDF object type: %d",
	       statbuf.type);
  return 0;
}

static void
add_to_symbol_table (struct hdf5_iterate_info *iinfo,
		     const char *name,
		     SEXP obj)
{
  setVar (install ((char *)name), obj, iinfo->env);
}

static void
add_to_return_list (struct hdf5_iterate_info *iinfo,
		    const char *name,
		    SEXP obj)
{
  PROTECT (iinfo->ret);
  iinfo->ret = CONS (obj, iinfo->ret);
  TAG (iinfo->ret) = install ((char *) name);
  UNPROTECT (1);
}

SEXP do_hdf5load (SEXP call, SEXP op, SEXP args, SEXP env)
{
  const char *path;
  hid_t fid;
  int restore_syms;
  struct hdf5_iterate_info iinfo;

  checkArity (op, args);

  if (!isValidString(CAR(args)))
    errorcall (call, "first argument must be a pathname\n");

  if (TYPEOF (CADR (args)) != LGLSXP)
    errorcall (call, "second argument must be a logical vector\n");

  path = CHAR(STRING (CAR(args))[0]);
  restore_syms = INTEGER (CADR(args))[0];

  H5dont_atexit ();

  if ((fid = H5Fopen (path, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to open HDF file: %s", path);

  if (H5Tregister (H5T_PERS_SOFT,
		   STRING2REF_CONV,
		   H5T_C_S1,
		   H5T_STD_REF_OBJ, string_ref) < 0)
    errorcall (call, "Unable to register string->ref converter");

  iinfo.call = call;
  iinfo.add = restore_syms ? add_to_symbol_table : add_to_return_list;
  iinfo.env = env;
  iinfo.ret = R_NilValue;

  if (H5Giterate (fid, "/", NULL, hdf5_process_object, &iinfo) < 0)
    errorcall (call, "unable to iterate over HDF file: %s", path);

  if (H5Tunregister (H5T_PERS_SOFT, STRING2REF_CONV, -1, -1, string_ref) < 0)
    errorcall (call, "Unable to unregister string->ref converter");

  if (H5Fclose (fid) < 0)
    errorcall (call, "unable to close HDF file");

  return iinfo.ret;
}

#else

SEXP do_hdf5save (SEXP call, SEXP op, SEXP args, SEXP env)
{
    errorcall(call, "HDF5 support unavailable\n");
    return(R_NilValue);/* -Wall */
}
SEXP do_hdf5load (SEXP call, SEXP op, SEXP args, SEXP env)
{
    errorcall(call, "HDF5 support unavailable\n");
    return(R_NilValue);/* -Wall */
}
#endif
