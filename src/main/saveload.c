/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
 
#include "Defn.h"
#include "Mathlib.h"
#include "Fileio.h"

	/* Static Globals */

static char buf[MAXELTSIZE];	/* Buffer for character strings */
static char *bufp;		/* A pointer to that buffer */

static int NSymbol;		/* Number of symbols */
static int NSave;		/* Number of non-symbols */
static int NTotal;		/* NSymbol + NSave */
static int NVSize;		/* Number of vector cells */

static int *OldOffset;		/* Offsets in previous incarnation */
static SEXP *NewAddress;	/* Addresses in this incarnation */

static int VersionId;

static SEXP DataLoad(FILE*);
static void DataSave(SEXP, FILE*);

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



	/*********************************/
	/*				 */
	/*   Dummy Placeholder Routine   */
	/*				 */
	/*********************************/

static void Dummy(FILE *fp)
{
}


	/************************************/
	/*				    */
	/*   Functions for Ascii Pickling   */
	/*				    */
	/************************************/

static void AsciiOutInteger(FILE *fp, int i)
{
	if(i == NA_INTEGER) fprintf(fp, "NA");
	else fprintf(fp, "%d", i);
}

static int AsciiInInteger(FILE *fp)
{
	int x;
	fscanf(fp, "%s", buf);
	if(strcmp(buf, "NA") == 0) return NA_INTEGER;
	else {
		sscanf(buf, "%d", &x);
		return x;
	}
}

static void AsciiOutReal(FILE *fp, double x)
{
	if(!FINITE(x)) fprintf(fp, "NA");
	else fprintf(fp, "%g", x);
}

static double AsciiInReal(FILE *fp)
{
	double x;
	fscanf(fp, "%s", buf);
	if(strcmp(buf, "NA") == 0) x = NA_REAL;
	else sscanf(buf, "%lg", &x);
	return x;
}

static void AsciiOutComplex(FILE *fp, complex x)
{
	if(!FINITE(x.r) || !FINITE(x.i)) fprintf(fp, "NA NA");
	else fprintf(fp, "%g %g", x.r, x.i);
}

static complex AsciiInComplex(FILE *fp)
{
	complex x;
	fscanf(fp, "%s", buf);
	if(strcmp(buf, "NA") == 0) x.r = NA_REAL;
	else sscanf(buf, "%lg", &x.r);
	fscanf(fp, "%s", buf);
	if(strcmp(buf, "NA") == 0) x.i = NA_REAL;
	else sscanf(buf, "%lg", &x.i);
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

/* TODO: To make saved files completely portable, the output */
/* representation of strings should be completely ascii.  This */
/* includes control characters and non-ascii characters. */
/* This could be done with \ooo escapes. */

static void AsciiOutString(FILE *fp, char *s)
{
	char *p = s;
	fputc('\"', fp);
	while(*p) {
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
	int c, quote;
	bufp = buf;
	while ((c = R_fgetc(fp)) != '"');
	while ((c = R_fgetc(fp)) != R_EOF && c != '"') {
		if (c == '\\') {
			if((c = R_fgetc(fp)) == R_EOF) break;
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

	/***********************************************/
	/*					       */
	/*   Functions for Binary Pickling Using XDR   */
	/*					       */
	/***********************************************/

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
		error("a write error occured\n");
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
		error("a write error occured\n");
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
        if (!xdr_double(&xdrs, &(x.r)) | !xdr_double(&xdrs, &(x.i))) {
		xdr_destroy(&xdrs);
                error("a write error occured\n");
	}
}

static complex XdrInComplex(FILE * fp)
{
	complex x;
        if (!xdr_double(&xdrs, &(x.r)) | !xdr_double(&xdrs, &(x.i))) {
		xdr_destroy(&xdrs);
		error("a read error occured\n");
	}
	return x;
}

static void XdrOutString(FILE *fp, char *s)
{
	if(!xdr_string(&xdrs, &s, MAXELTSIZE-1)) {
		xdr_destroy(&xdrs);
		error("a write error occured\n");
	}
}

static char *XdrInString(FILE *fp)
{
	char *bufp = buf;
	if(!xdr_string(&xdrs, &bufp, MAXELTSIZE-1)) {
		xdr_destroy(&xdrs);
		error("a read error occured\n");
	}
	return buf;
}

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
	DataSave(s, fp);
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
	return DataLoad(fp);
}
#endif

	/*************************************/
	/*				     */
	/*   Functions for Binary Pickling   */
	/*				     */
	/*************************************/

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
	int n = strlen(s)+1;	/* NULL too */
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


	/*******************************************/
	/*					   */
	/*   Magic Numbers for R Save File Types   */
	/*					   */
	/*******************************************/


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
	return d1234 = d1 + 10*d2 + 100*d3 + 1000*d4;
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
	case FACTSXP:
	case ORDSXP:
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
		error("invalid type in ReallocVector\n");
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

	if(s == R_NilValue || s == R_GlobalEnv
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
		case FACTSXP:
		case ORDSXP:
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

	if(offset == -1) return R_NilValue;
	if(offset == -2) return R_GlobalEnv;
	if(offset == -3) return R_UnboundValue;
	if(offset == -4) return R_MissingArg;
	
		/* binary search for offset */

	l = 0;
	r = NTotal - 1;
	do {
		m = (l + r)/2;
		if(offset < OldOffset[m])
			r = m - 1;
		else
			l = m + 1;
	}
	while(offset != OldOffset[m] && l <= r);
	if(offset == OldOffset[m]) return NewAddress[m];

	error("unresolved node during restore\n");
}

static void DataSave(SEXP s, FILE *fp)
{
	int i, j, k, l, n;
	char *strp;
	SEXP t;

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
	for (i=0 ; i<R_NSize ; i++) {
		if (MARK(&R_NHeap[i])) {
			if(TYPEOF(&R_NHeap[i]) == SYMSXP) {
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
	if(k != NSymbol || n != NSymbol+NSave)
		error("symbol count conflict\n");

		/* write out the forwarding address table */

	k = 0; n = 0;
	for (i=0 ; i<R_NSize ; i++) {
		if (MARK(&R_NHeap[i])) {
			if(TYPEOF(&R_NHeap[i]) != SYMSXP) {
				OutInteger(fp, n);
				OutSpace(fp);
				OutInteger(fp, NodeToOffset(&R_NHeap[i]));
				OutNewline(fp);
				k++;
			}
			n++;
		}
	}
	if(k != NSave || n != NSymbol+NSave)
		error("node count conflict\n");

	k = 0; n = 0;
	for (i = 0; i < R_NSize; i++) {
		if (MARK(&R_NHeap[i])) {
			if(TYPEOF(&R_NHeap[i]) != SYMSXP) {

				OutInteger(fp, n);
				OutSpace(fp);
				OutInteger(fp, TYPEOF(&R_NHeap[i]));
				OutSpace(fp);
				OutInteger(fp, OBJECT(&R_NHeap[i]));
				OutSpace(fp);
				OutInteger(fp, 	LEVELS(&R_NHeap[i]));
				OutSpace(fp);
				OutInteger(fp, 	NodeToOffset(ATTRIB(&R_NHeap[i])));
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
						if((j+1)%10 == 0 || j==l-1)
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
						if((j+1)%10 == 0 || j==l-1)
							OutNewline(fp);
						else
							OutSpace(fp);
					}
					break;
				case INTSXP:
				case LGLSXP:
				case FACTSXP:
				case ORDSXP:
					l = LENGTH(&R_NHeap[i]);
					OutInteger(fp, l);
					OutNewline(fp);
					for (j = 0; j < l; j++) {
						OutInteger(fp, INTEGER(&R_NHeap[i])[j]);
						if((j+1)%10 == 0 || j==l-1)
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
						if((j+1)%10 == 0 || j==l-1)
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
	if(k != NSave) error("node count conflict\n");

		/* write out the offset of the list */

	OutInteger(fp, NodeToOffset(s));
	OutNewline(fp);

	OutTerm(fp);
}

static void RestoreSEXP(SEXP s, FILE *fp)
{
	unsigned int i, j, k, l;
	int len, t1;

	TYPEOF(s) = InInteger(fp);

	if(VersionId) {
		switch(VersionId) {

		case 16:
			/* In the version 0.16.1 -> 0.50 switch */
			/* we really introduced complex values */
			/* and found that numeric/complex numbers */
			/* had to be contiguous.  Hence this switch */
			if(TYPEOF(s) == STRSXP) TYPEOF(s) = CPLXSXP;
			else if(TYPEOF(s) == CPLXSXP) TYPEOF(s) = STRSXP;
			break;

		default:
			error("restore compatibility error - no version %d compatibility\n", VersionId);
		}
	}

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
		PRIMOFFSET(s) = StrToInternal(InString(fp));
		break;
	case CHARSXP:
		LENGTH(s) = len = InInteger(fp);
		ReallocString(s, len);
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
	case FACTSXP:
	case ORDSXP:
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
	int i, j, k;
	char *vmaxsave;

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
	for(i=0 ; i<NTotal ; i++) {
		OldOffset[i] = 0;
		NewAddress[i] = R_NilValue;
	}

		/* read in the required symbols */
		/* expanding the symbol table and */
		/* computing the forwarding addresses */

	for(i=0 ; i<NSymbol ; i++) {
		j = InInteger(fp);
		OldOffset[j] = InInteger(fp);
		NewAddress[j] = install(InString(fp));
	}

		/* symbols are all installed */
		/* gc() and check space */

	gc();

		/* a gc after this point will be a disaster */
		/* because nothing will have been protected */

	if ((VECREC *)vmaxget() - R_VTop < NVSize)
		error("vector heap is too small to restore data\n");

	if (R_Collected < NSave)
		error("cons heap is too small to restore data\n");

		/* build the full forwarding table */
		/* allocating SEXPs from the free list */

	for(i=0 ; i<NSave ; i++) {
		j = InInteger(fp);
		OldOffset[j] = InInteger(fp);
		NewAddress[j] = R_FreeSEXP;
		R_FreeSEXP = CDR(R_FreeSEXP);
	}

		/* restore the saved nodes */

	for (i=0 ; i < NSave ;  i++) {
		RestoreSEXP(NewAddress[InInteger(fp)], fp);
	}

		/* restore the heap */

	vmaxset(vmaxsave);

		/* return the "top-level" object */
		/* this is usually a list */

	InTerm(fp);

	return OffsetToNode(InInteger(fp));
}

void R_SaveToFile(SEXP obj, FILE *fp, int ascii)
{
	if(ascii) {
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

SEXP R_LoadFromFile(FILE *fp)
{
	SEXP ans;

	switch(R_ReadMagic(fp)) {
#ifdef HAVE_RPC_XDR_H
	case R_MAGIC_XDR:
		ans = XdrLoad(fp);
		break;
#endif
	case R_MAGIC_BINARY:
		ans = BinaryLoad(fp);
		break;
	case R_MAGIC_ASCII:
		ans = AsciiLoad(fp);
		break;
	case R_MAGIC_BINARY_VERSION16:
		ans = BinaryLoadOld(fp, 16);
		break;
	case R_MAGIC_ASCII_VERSION16:
		ans = AsciiLoadOld(fp, 16);
		break;
	default:
		fclose(fp);
		error("restore file corrupted -- no data loaded\n");
	}
	return ans;
}


	/***************************************/
	/*				       */
	/*   Interpreter Interface Functions   */
	/*				       */
	/***************************************/

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

	fp = R_fopen(CHAR(STRING(CADR(args))[0]), "wb");
	if (!fp)
		errorcall(call, "unable to open file\n");

	len = length(CAR(args));
	PROTECT(s = allocList(len));

	t = s;
	for (j = 0; j < len; j++, t = CDR(t)) {
		TAG(t) = install(CHAR(STRING(CAR(args))[j]));
		CAR(t) = findVar(TAG(t), R_GlobalContext->sysparent);
	}

	R_SaveToFile(s, fp, INTEGER(CADDR(args))[0]);

	UNPROTECT(1);
	fclose(fp);
	return R_NilValue;
}

SEXP do_load(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP a, ans, e;
	int i;
	FILE *fp;

	checkArity(op, args);

	if (TYPEOF(CAR(args)) != STRSXP)
		errorcall(call, "first argument must be a string\n");
	i = INTEGER(CADR(args))[0];

	fp = R_fopen(CHAR(STRING(CAR(args))[0]), "rb");
	if (!fp)
		errorcall(call, "unable to open file\n");

	ans = R_LoadFromFile(fp);

	fclose(fp);


		/* store the components of the list in the Global Env */

	a = ans;
	while(a != R_NilValue) {
		for(e=FRAME(R_GlobalEnv) ; e!=R_NilValue ; e=CDR(e)) {
			if(TAG(e) == TAG(a)) {
				CAR(e) = CAR(a);
				a = CDR(a);
				goto NextItem;
			}
		}
		e = a;
		a = CDR(a);
		CDR(e) = FRAME(R_GlobalEnv);
		FRAME(R_GlobalEnv) = e;
	NextItem:
		;
	}
	return R_NilValue;
}
