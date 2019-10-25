/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/* <UTF8> byte-level access is only to compare with chars <= 0x7F */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define NEED_CONNECTION_PSTREAMS
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Rinterface.h>
#include <Rmath.h>
#include <Fileio.h>
#include <R_ext/RS.h>
#include <errno.h>
#include <ctype.h>		/* for isspace */

/* From time to time changes in R, such as the addition of a new SXP,
 * may require changes in the save file format.  Here are some
 * guidelines on handling format changes:
 *
 *    Starting with R 1.4.0 there is a version number associated with
 *    save file formats.  This version number should be incremented
 *    when the format is changed so older versions of R can recognize
 *    and reject the new format with a meaningful error message.
 *
 *    R should remain able to write older workspace formats.  An error
 *    should be signaled if the contents to be saved is not compatible
 *    with the requested format.
 *
 *    To allow older versions of R to give useful error messages, the
 *    header now contains the version of R that wrote the workspace
 *    and the oldest version that can read the workspace.  These
 *    versions are stored as an integer packed by the R_Version macro
 *    from Rversion.h.  Some workspace formats may only exist
 *    temporarily in the development stage.  If readers are not
 *    provided in a release version, then these should specify the
 *    oldest reader R version as -1.
 */

#define R_MAGIC_ASCII_V3   3001
#define R_MAGIC_BINARY_V3  3002
#define R_MAGIC_XDR_V3     3003
#define R_MAGIC_ASCII_V2   2001
#define R_MAGIC_BINARY_V2  2002
#define R_MAGIC_XDR_V2     2003
#define R_MAGIC_ASCII_V1   1001
#define R_MAGIC_BINARY_V1  1002
#define R_MAGIC_XDR_V1     1003
#define R_MAGIC_EMPTY      999
#define R_MAGIC_CORRUPT    998
#define R_MAGIC_MAYBE_TOONEW 997

/* pre-1 formats (R < 0.99.0) */
#define R_MAGIC_BINARY 1975
#define R_MAGIC_ASCII  1976
#define R_MAGIC_XDR    1977
#define R_MAGIC_BINARY_VERSION16 1971
#define R_MAGIC_ASCII_VERSION16	 1972


/* Static Globals, DIE, DIE, DIE! */


#include "RBufferUtils.h"

/* These are used by OffsetToNode & DataLoad.
 OffsetToNode is called by DataLoad() and RestoreSEXP()
 which itself is only called by RestoreSEXP.
 */
typedef struct {
 int NSymbol;		/* Number of symbols */
 int NSave;		/* Number of non-symbols */
 int NTotal;		/* NSymbol + NSave */
 int NVSize;		/* Number of vector cells */

 int *OldOffset;        /* Offsets in previous incarnation */

 SEXP NewAddress;       /* Addresses in this incarnation */
} NodeInfo;


#ifndef INT_32_BITS
/* The way XDR is used pretty much assumes that int is 32 bits and
   maybe even 2's complement representation--without that, NA_INTEGER
   is not likely to be preserved properly.  Since 32 bit ints (and 2's
   complement) are pretty much universal, we can worry about that when
   the need arises.  To be safe, we signal a compiler error if int is
   not 32 bits. There may be similar issues with doubles. */
*/
# error code requires that int have 32 bits
#endif


#include <rpc/types.h>
#include <rpc/xdr.h>

#define SMBUF_SIZE 512
#define SMBUF_SIZED_STRING "%511s"

typedef struct {
/* These variables are accessed in the
   InInteger, InComplex, InReal, InString
   methods for Ascii, Binary, XDR.
   bufsize is only used in XdrInString!

The Ascii* routines could declare their own local
copy of smbuf and use that (non-static). That would
mean some of them wouldn't need the extra argument.
*/

    R_StringBuffer buffer;
    char smbuf[SMBUF_SIZE];	/* Small buffer for temp use */
				/* smbuf is only used by Ascii. */
    XDR xdrs;
} SaveLoadData;

/* ----- I / O -- F u n c t i o n -- P o i n t e r s ----- */

typedef struct {
 void	(*OutInit)(FILE*, SaveLoadData *d);
 void	(*OutInteger)(FILE*, int, SaveLoadData *);
 void	(*OutReal)(FILE*, double, SaveLoadData *);
 void	(*OutComplex)(FILE*, Rcomplex, SaveLoadData *);
 void	(*OutString)(FILE*, const char*, SaveLoadData *);
 void	(*OutSpace)(FILE*, int, SaveLoadData *);
 void	(*OutNewline)(FILE*, SaveLoadData *);
 void	(*OutTerm)(FILE*, SaveLoadData *);
} OutputRoutines;

typedef struct {
 void	(*InInit)(FILE*, SaveLoadData *d);
 int	(*InInteger)(FILE*, SaveLoadData *);
 double	(*InReal)(FILE*, SaveLoadData *);
 Rcomplex	(*InComplex)(FILE*, SaveLoadData *);
 char*	(*InString)(FILE*, SaveLoadData *);
 void	(*InTerm)(FILE*, SaveLoadData *d);
} InputRoutines;

typedef struct {
  FILE *fp;
  OutputRoutines *methods;
  SaveLoadData *data;
} OutputCtxtData;

typedef struct {
  FILE *fp;
  InputRoutines *methods;
  SaveLoadData *data;
} InputCtxtData;


static SEXP DataLoad(FILE*, int startup, InputRoutines *m, int version, SaveLoadData *d);


/* ----- D u m m y -- P l a c e h o l d e r -- R o u t i n e s ----- */

static void DummyInit(FILE *fp, SaveLoadData *d)
{
}

static void DummyOutSpace(FILE *fp, int nspace, SaveLoadData *d)
{
}

static void DummyOutNewline(FILE *fp, SaveLoadData *d)
{
}

static void DummyTerm(FILE *fp, SaveLoadData *d)
{
}

/* ----- O l d - s t y l e  (p r e 1. 0)  R e s t o r e ----- */

/* This section is only used to load old-style workspaces / objects */


/* ----- L o w l e v e l -- A s c i i -- I / O ----- */

static int AsciiInInteger(FILE *fp, SaveLoadData *d)
{
    int x, res;
    res = fscanf(fp, SMBUF_SIZED_STRING, d->smbuf);
    if(res != 1) error(_("read error"));
    if (strcmp(d->smbuf, "NA") == 0)
	return NA_INTEGER;
    else {
	res = sscanf(d->smbuf, "%d", &x);
	if(res != 1) error(_("read error"));
	return x;
    }
}

static double AsciiInReal(FILE *fp, SaveLoadData *d)
{
    double x;
    int res = fscanf(fp, SMBUF_SIZED_STRING, d->smbuf);
    if(res != 1) error(_("read error"));
    if (strcmp(d->smbuf, "NA") == 0)
	x = NA_REAL;
    else if (strcmp(d->smbuf, "Inf") == 0)
	x = R_PosInf;
    else if (strcmp(d->smbuf, "-Inf") == 0)
	x = R_NegInf;
    else
	res  = sscanf(d->smbuf, "%lg", &x);
    if(res != 1) error(_("read error"));
    return x;
}

static Rcomplex AsciiInComplex(FILE *fp, SaveLoadData *d)
{
    Rcomplex x;
    int res;
    res = fscanf(fp, SMBUF_SIZED_STRING, d->smbuf);
    if(res != 1) error(_("read error"));
    if (strcmp(d->smbuf, "NA") == 0)
	x.r = NA_REAL;
    else if (strcmp(d->smbuf, "Inf") == 0)
	x.r = R_PosInf;
    else if (strcmp(d->smbuf, "-Inf") == 0)
	x.r = R_NegInf;
    else {
	res  = sscanf(d->smbuf, "%lg", &x.r);
	if(res != 1) error(_("read error"));
    }

    res = fscanf(fp, SMBUF_SIZED_STRING, d->smbuf);
    if(res != 1) error(_("read error"));
    if (strcmp(d->smbuf, "NA") == 0)
	x.i = NA_REAL;
    else if (strcmp(d->smbuf, "Inf") == 0)
	x.i = R_PosInf;
    else if (strcmp(d->smbuf, "-Inf") == 0)
	x.i = R_NegInf;
    else {
	res = sscanf(d->smbuf, "%lg", &x.i);
	if(res != 1) error(_("read error"));
    }
    return x;
}


static char *AsciiInString(FILE *fp, SaveLoadData *d)
{
    int c;
    char *bufp = d->buffer.data;
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
	*bufp++ = (char) c;
    }
    *bufp = '\0';
    return d->buffer.data;
}

static SEXP AsciiLoad(FILE *fp, int startup, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = AsciiInInteger;
    m.InReal = AsciiInReal;
    m.InComplex = AsciiInComplex;
    m.InString = AsciiInString;
    m.InTerm = DummyTerm;
    return DataLoad(fp, startup, &m, 0, d);
}

static SEXP AsciiLoadOld(FILE *fp, int version, int startup, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = AsciiInInteger;
    m.InReal = AsciiInReal;
    m.InComplex = AsciiInComplex;
    m.InString = AsciiInString;
    m.InTerm = DummyTerm;
    return DataLoad(fp, startup, &m, version, d);
}

/* ----- L o w l e v e l -- X D R -- I / O ----- */

static void XdrInInit(FILE *fp, SaveLoadData *d)
{
    xdrstdio_create(&d->xdrs, fp, XDR_DECODE);
}

static void XdrInTerm(FILE *fp, SaveLoadData *d)
{
    xdr_destroy(&d->xdrs);
}

static int XdrInInteger(FILE * fp, SaveLoadData *d)
{
    int i;
    if (!xdr_int(&d->xdrs, &i)) {
	xdr_destroy(&d->xdrs);
	error(_("a I read error occurred"));
    }
    return i;
}

static double XdrInReal(FILE * fp, SaveLoadData *d)
{
    double x;
    if (!xdr_double(&d->xdrs, &x)) {
	xdr_destroy(&d->xdrs);
	error(_("a R read error occurred"));
    }
    return x;
}

static Rcomplex XdrInComplex(FILE * fp, SaveLoadData *d)
{
    Rcomplex x;
    if (!xdr_double(&d->xdrs, &(x.r)) || !xdr_double(&d->xdrs, &(x.i))) {
	xdr_destroy(&d->xdrs);
	error(_("a C read error occurred"));
    }
    return x;
}

static char *XdrInString(FILE *fp, SaveLoadData *d)
{
    char *bufp = d->buffer.data;
    if (!xdr_string(&d->xdrs, &bufp, (unsigned int)d->buffer.bufsize)) {
	xdr_destroy(&d->xdrs);
	error(_("a S read error occurred"));
    }
    return d->buffer.data;
}

static SEXP XdrLoad(FILE *fp, int startup, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = XdrInInit;
    m.InInteger = XdrInInteger;
    m.InReal = XdrInReal;
    m.InComplex = XdrInComplex;
    m.InString = XdrInString;
    m.InTerm = XdrInTerm;
    return DataLoad(fp, startup, &m, 0, d);
}


/* ----- L o w l e v e l -- B i n a r y -- I / O ----- */

static int BinaryInInteger(FILE * fp, SaveLoadData *unused)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error(_("a read error occurred"));
    return i;
}

static double BinaryInReal(FILE * fp, SaveLoadData *unused)
{
    double x;
    if (fread(&x, sizeof(double), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static Rcomplex BinaryInComplex(FILE * fp, SaveLoadData *unused)
{
    Rcomplex x;
    if (fread(&x, sizeof(Rcomplex), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static char *BinaryInString(FILE *fp, SaveLoadData *d)
{
    char *bufp = d->buffer.data;
    do {
	*bufp = (char) R_fgetc(fp);
    }
    while (*bufp++);
    return d->buffer.data;
}

static SEXP BinaryLoad(FILE *fp, int startup, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = BinaryInInteger;
    m.InReal = BinaryInReal;
    m.InComplex = BinaryInComplex;
    m.InString = BinaryInString;
    m.InTerm = DummyTerm;
    return DataLoad(fp, startup, &m, 0, d);
}

static SEXP BinaryLoadOld(FILE *fp, int version, int startup, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = BinaryInInteger;
    m.InReal = BinaryInReal;
    m.InComplex = BinaryInComplex;
    m.InString = BinaryInString;
    m.InTerm = DummyTerm;
    return DataLoad(fp, startup, &m, version, d);
}

static SEXP OffsetToNode(int offset, NodeInfo *node)
{
    int l, m, r;

    if (offset == -1) return R_NilValue;
    if (offset == -2) return R_GlobalEnv;
    if (offset == -3) return R_UnboundValue;
    if (offset == -4) return R_MissingArg;

    /* binary search for offset */

    l = 0;
    r = node->NTotal - 1;
    do {
	m = (l + r) / 2;
	if (offset < node->OldOffset[m])
	    r = m - 1;
	else
	    l = m + 1;
    }
    while (offset != node->OldOffset[m] && l <= r);
    if (offset == node->OldOffset[m]) return VECTOR_ELT(node->NewAddress, m);

    /* Not supposed to happen: */
    warning(_("unresolved node during restore"));
    return R_NilValue;
}

static unsigned int FixupType(unsigned int type, int VersionId)
{
    if (VersionId) {
	switch(VersionId) {

	case 16:
	    /* In the version 0.16.1 -> 0.50 switch */
	    /* we really introduced complex values */
	    /* and found that numeric/complex numbers */
	    /* had to be contiguous.  Hence this switch */
	    if (type == STRSXP)
		type = CPLXSXP;
	    else if (type == CPLXSXP)
		type = STRSXP;
	    break;

	default:
	    error(_("restore compatibility error - no version %d compatibility"),
		  VersionId);
	}
    }

    /* Map old factors to new ...  (0.61->0.62) */
    if (type == 11 || type == 12)
	type = 13;

    return type;
}

static void RemakeNextSEXP(FILE *fp, NodeInfo *node, int version, InputRoutines *m, SaveLoadData *d)
{
    unsigned int j, idx, type;
    int len;
    SEXP s = R_NilValue;	/* -Wall */

    idx = m->InInteger(fp, d);
    type = FixupType(m->InInteger(fp, d), version);

    /* skip over OBJECT, LEVELS, and ATTRIB */
    /* OBJECT(s) = */ m->InInteger(fp, d);
    /* LEVELS(s) = */ m->InInteger(fp, d);
    /* ATTRIB(s) = */ m->InInteger(fp, d);
    switch (type) {
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case ENVSXP:
	s = allocSExp(type);
	/* skip over CAR, CDR, and TAG */
	/* CAR(s) = */ m->InInteger(fp, d);
	/* CDR(s) = */ m->InInteger(fp, d);
	/* TAG(s) = */ m->InInteger(fp, d);
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	s = allocSExp(type);
	/* skip over length and name fields */
	/* length = */ m->InInteger(fp, d);
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	/* name = */ m->InString(fp, d);
	break;
    case CHARSXP:
	len = m->InInteger(fp, d);
	s = allocCharsxp(len); /* This is not longer correct */
	R_AllocStringBuffer(len, &(d->buffer));
	/* skip over the string */
	/* string = */ m->InString(fp, d);
	break;
    case REALSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /*REAL(s)[j] = */ m->InReal(fp, d);
	break;
    case CPLXSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /* COMPLEX(s)[j] = */ m->InComplex(fp, d);
	break;
    case INTSXP:
    case LGLSXP:
	len = m->InInteger(fp, d);;
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++)
	    /* INTEGER(s)[j] = */ m->InInteger(fp, d);
	break;
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
	len = m->InInteger(fp, d);
	s = allocVector(type, len);
	/* skip over the vector content */
	for (j = 0; j < len; j++) {
	    /* VECTOR(s)[j] = */ m->InInteger(fp, d);
	}
	break;
    default: error(_("bad SEXP type in data file"));
    }

    /* install the new SEXP */
    SET_VECTOR_ELT(node->NewAddress, idx, s);
}

static void RestoreSEXP(SEXP s, FILE *fp, InputRoutines *m, NodeInfo *node, int version, SaveLoadData *d)
{
    unsigned int j, type;
    int len;

    type = FixupType(m->InInteger(fp, d), version);
    if (type != TYPEOF(s))
      error(_("mismatch on types"));

    SET_OBJECT(s, m->InInteger(fp, d));
    SETLEVELS(s, m->InInteger(fp, d));
    SET_ATTRIB(s, OffsetToNode(m->InInteger(fp, d), node));
    switch (TYPEOF(s)) {
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case ENVSXP:
	SETCAR(s, OffsetToNode(m->InInteger(fp, d), node));
	SETCDR(s, OffsetToNode(m->InInteger(fp, d), node));
	SET_TAG(s, OffsetToNode(m->InInteger(fp, d), node));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	len = m->InInteger(fp, d);
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	int index = StrToInternal(m->InString(fp, d));
	if (index == NA_INTEGER) {
	    warning(_("unrecognized internal function name \"%s\""), d->buffer.data);
	    index = 0;   /* zero doesn't make sense, but is back compatible with 3.0.0 and earlier */
	}
	SET_PRIMOFFSET(s, index);
	break;
    case CHARSXP:
	len = m->InInteger(fp, d);
	R_AllocStringBuffer(len, &(d->buffer));
	/* Better to use a fresh copy in the cache */
	strcpy(CHAR_RW(s), m->InString(fp, d));
	break;
    case REALSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    REAL(s)[j] = m->InReal(fp, d);
	break;
    case CPLXSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    COMPLEX(s)[j] = m->InComplex(fp, d);
	break;
    case INTSXP:
    case LGLSXP:
	len = m->InInteger(fp, d);;
	for (j = 0; j < len; j++)
	    INTEGER(s)[j] = m->InInteger(fp, d);
	break;
    case STRSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    SET_STRING_ELT(s, j, OffsetToNode(m->InInteger(fp, d), node));
	break;
    case VECSXP:
    case EXPRSXP:
	len = m->InInteger(fp, d);
	for (j = 0; j < len; j++)
	    SET_VECTOR_ELT(s, j, OffsetToNode(m->InInteger(fp, d), node));
	break;
    default: error(_("bad SEXP type in data file"));
    }
}

static void RestoreError(/* const */ char *msg, int startup)
{
    if(startup)
	R_Suicide(msg);
    else
	error("%s", msg);
}

/* used for pre-version 1 formats */
static SEXP DataLoad(FILE *fp, int startup, InputRoutines *m,
		     int version, SaveLoadData *d)
{
    int i, j;
    const void *vmaxsave;
    fpos_t savepos;
    NodeInfo node;

    /* read in the size information */

    m->InInit(fp, d);

    node.NSymbol = m->InInteger(fp, d);
    node.NSave = m->InInteger(fp, d);
    node.NVSize = m->InInteger(fp, d);
    node.NTotal = node.NSymbol + node.NSave;

    /* allocate the forwarding-address tables */
    /* these are non-relocatable, so we must */
    /* save the current non-relocatable base */

    vmaxsave = vmaxget();
    node.OldOffset = (int*)R_alloc(node.NSymbol + node.NSave, sizeof(int));
    PROTECT(node.NewAddress = allocVector(VECSXP, node.NSymbol + node.NSave));
    for (i = 0 ; i < node.NTotal ; i++) {
	node.OldOffset[i] = 0;
	SET_VECTOR_ELT(node.NewAddress, i, R_NilValue);
    }

    /* read in the required symbols */
    /* expanding the symbol table and */
    /* computing the forwarding addresses */

    for (i = 0 ; i < node.NSymbol ; i++) {
	j = m->InInteger(fp, d);
	node.OldOffset[j] = m->InInteger(fp, d);
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	SET_VECTOR_ELT(node.NewAddress, j, install(m->InString(fp, d)));
    }

    /* build the full forwarding table */

    for (i = 0 ; i < node.NSave ; i++) {
	j = m->InInteger(fp, d);
	node.OldOffset[j] = m->InInteger(fp, d);
    }


    /* f[gs]etpos are 64-bit on MSVCRT Windows */
    /* save the file position */
    if (fgetpos(fp, &savepos))
	RestoreError(_("cannot save file position while restoring data"),
		     startup);


    /* first pass: allocate nodes */

    for (i = 0 ; i < node.NSave ; i++) {
	RemakeNextSEXP(fp, &node, version, m, d);
    }


    /* restore the file position */
    if (fsetpos(fp, &savepos))
	RestoreError(_("cannot restore file position while restoring data"),
		     startup);


    /* second pass: restore the contents of the nodes */

    for (i = 0 ; i < node.NSave ;  i++) {
	RestoreSEXP(VECTOR_ELT(node.NewAddress, m->InInteger(fp, d)), fp, m, &node, version, d);
    }

    /* restore the heap */

    vmaxset(vmaxsave);
    UNPROTECT(1);

    /* clean the string buffer */
    R_FreeStringBufferL(&(d->buffer));

    /* return the "top-level" object */
    /* this is usually a list */

    i = m->InInteger(fp, d);
    m->InTerm(fp, d);

    return OffsetToNode(i, &node);
}


/* ----- V e r s i o n -- O n e -- S a v e / R e s t o r e ----- */

/*  Code Developed by  Chris K. Young <cky@pobox.com>
 *  and Ross Ihaka for Chris' Honours project -- 1999.
 *  Copyright Assigned to the R Project.
 */

/*  An assert function which doesn't crash the program.
 *  Something like this might be useful in an R header file
 */

#ifdef NDEBUG
#define R_assert(e) ((void) 0)
#else
/* The line below requires an ANSI C preprocessor (stringify operator) */
#define R_assert(e) ((e) ? (void) 0 : error("assertion `%s' failed: file `%s', line %d\n", #e, __FILE__, __LINE__))
#endif /* NDEBUG */


static void NewWriteItem (SEXP s, SEXP sym_list, SEXP env_list, FILE *fp, OutputRoutines *, SaveLoadData *);
static SEXP NewReadItem (SEXP sym_table, SEXP env_table, FILE *fp, InputRoutines *, SaveLoadData *);


/*  We use special (negative) type codes to indicate the special
 *  values: R_NilValue, R_GlobalEnv, R_UnboundValue, R_MissingArg.
 *  The following routines handle these conversions (both
 *  directions). */

static int NewSaveSpecialHook (SEXP item)
{
    if (item == R_NilValue)     return -1;
    if (item == R_GlobalEnv)    return -2;
    if (item == R_UnboundValue) return -3;
    if (item == R_MissingArg)   return -4;
    return 0;
}

static SEXP NewLoadSpecialHook (SEXPTYPE type)
{
    switch (type) {
    case -1: return R_NilValue;
    case -2: return R_GlobalEnv;
    case -3: return R_UnboundValue;
    case -4: return R_MissingArg;
    }
    return (SEXP) 0;	/* not strictly legal... */
}


/*  If "item" is a special value (as defined in "NewSaveSpecialHook")
 *  then a negative value is returned.
 *
 *  If "item" is present in "list" the a positive value is returned
 *  (the 1-based offset into the list).
 *
 *   Otherwise, a value of zero is returned.
 *
 *  The "list" is managed with a hash table.  This results in
 *  significant speedups for saving large amounts of code.  A fixed
 *  hash table size is used; this is not ideal but seems adequate for
 *  now.  The hash table representation consists of a (list . vector)
 *  pair.  The hash buckets are in the vector.  The list holds the
 *  list of keys.  This list is in reverse order to the way the keys
 *  were added (i.e. the most recently added key is first).  The
 *  indices produced by HashAdd are in order.  Since the list is
 *  written out in order, we either have to reverse the list or
 *  reverse the indices; to retain byte for byte compatibility the
 *  function FixHashEntries reverses the indices.  FixHashEntries must
 *  be called after filling the tables and before using them to find
 *  indices.  LT */

#define HASHSIZE 1099

#define PTRHASH(obj) (((R_size_t) (obj)) >> 2)

#define HASH_TABLE_KEYS_LIST(ht) CAR(ht)
#define SET_HASH_TABLE_KEYS_LIST(ht, v) SETCAR(ht, v)

#define HASH_TABLE_COUNT(ht) ((int) TRUELENGTH(CDR(ht)))
#define SET_HASH_TABLE_COUNT(ht, val) SET_TRUELENGTH(CDR(ht), ((int) (val)))

#define HASH_TABLE_SIZE(ht) LENGTH(CDR(ht))

#define HASH_BUCKET(ht, pos) VECTOR_ELT(CDR(ht), pos)
#define SET_HASH_BUCKET(ht, pos, val) SET_VECTOR_ELT(CDR(ht), pos, val)

static SEXP MakeHashTable(void)
{
    SEXP val = CONS(R_NilValue, allocVector(VECSXP, HASHSIZE));
    SET_HASH_TABLE_COUNT(val, 0);
    return val;
}

static void FixHashEntries(SEXP ht)
{
    SEXP cell;
    int count;
    for (cell = HASH_TABLE_KEYS_LIST(ht), count = 1;
	 cell != R_NilValue;
	 cell = CDR(cell), count++)
	INTEGER(TAG(cell))[0] = count;
}

static void HashAdd(SEXP obj, SEXP ht)
{
    R_size_t pos = PTRHASH(obj) % HASH_TABLE_SIZE(ht);
    int count = HASH_TABLE_COUNT(ht) + 1;
    SEXP val = ScalarInteger(count);
    SEXP cell = CONS(val, HASH_BUCKET(ht, pos));

    SET_HASH_TABLE_COUNT(ht, count);
    SET_HASH_BUCKET(ht, pos, cell);
    SET_TAG(cell, obj);
    SET_HASH_TABLE_KEYS_LIST(ht, CONS(obj, HASH_TABLE_KEYS_LIST(ht)));
    SET_TAG(HASH_TABLE_KEYS_LIST(ht), val);
}

static int HashGet(SEXP item, SEXP ht)
{
    R_size_t pos = PTRHASH(item) % HASH_TABLE_SIZE(ht);
    SEXP cell;
    for (cell = HASH_BUCKET(ht, pos); cell != R_NilValue; cell = CDR(cell))
	if (item == TAG(cell))
	    return INTEGER(CAR(cell))[0];
    return 0;
}

static int NewLookup (SEXP item, SEXP ht)
{
    int count = NewSaveSpecialHook(item);

    if (count != 0)
	return count;
    else
	return HashGet(item, ht);
}

/*  This code carries out the basic inspection of an object, building
 *  the tables of symbols and environments.
 *
 *  We don't really need to build a table of symbols here, but it does
 *  prevent repeated "install"s.  On the other hand there will generally
 *  be huge delays because of disk or network latency ...
 *
 *  CKY: One thing I've found out is that you have to build all the
 *  lists together or you risk getting infinite loops.  Of course, the
 *  method used here somehow shoots functional programming in the
 *  head --- sorry.  */

static void NewMakeLists (SEXP obj, SEXP sym_list, SEXP env_list)
{
    int count, length;

    if (NewSaveSpecialHook(obj))
	return;
    switch (TYPEOF(obj)) {
    case SYMSXP:
	if (NewLookup(obj, sym_list))
	    return;
	HashAdd(obj, sym_list);
	break;
    case ENVSXP:
	if (NewLookup(obj, env_list))
	    return;
	if (obj == R_BaseNamespace)
	    warning(_("base namespace is not preserved in version 1 workspaces"));
	else if (R_IsNamespaceEnv(obj))
	    error(_("cannot save namespace in version 1 workspaces"));
	if (R_HasFancyBindings(obj))
	    error(_("cannot save environment with locked/active bindings \
in version 1 workspaces"));
	HashAdd(obj, env_list);
	/* FALLTHROUGH */
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case DOTSXP:
	NewMakeLists(TAG(obj), sym_list, env_list);
	NewMakeLists(CAR(obj), sym_list, env_list);
	NewMakeLists(CDR(obj), sym_list, env_list);
	break;
    case EXTPTRSXP:
	NewMakeLists(EXTPTR_PROT(obj), sym_list, env_list);
	NewMakeLists(EXTPTR_TAG(obj), sym_list, env_list);
	break;
    case VECSXP:
    case EXPRSXP:
	length = LENGTH(obj);
	for (count = 0; count < length; ++count)
	    NewMakeLists(VECTOR_ELT(obj, count), sym_list, env_list);
	break;
    case WEAKREFSXP:
	error(_("cannot save weak references in version 1 workspaces"));
    }
    NewMakeLists(ATTRIB(obj), sym_list, env_list);
}

/* e.g., OutVec(fp, obj, INTEGER, OutInteger)
 The passMethods argument tells it whether to call outfunc with the
 other methods. This is only needed when calling OutCHARSXP
 since it needs to know how to write sub-elements!
*/
#define OutVec(fp, obj, accessor, outfunc, methods, d)	                \
	do {								\
		int cnt;						\
		for (cnt = 0; cnt < LENGTH(obj); ++cnt) {		\
			methods->OutSpace(fp, 1,d);			\
			outfunc(fp, accessor(obj, cnt), d);	        \
			methods->OutNewline(fp, d);                     \
		}							\
	} while (0)

#define LOGICAL_ELT(x,__i__)	LOGICAL(x)[__i__]
#define INTEGER_ELT(x,__i__)	INTEGER(x)[__i__]
#define REAL_ELT(x,__i__)	REAL(x)[__i__]
#define COMPLEX_ELT(x,__i__)	COMPLEX(x)[__i__]

/* Simply outputs the string associated with a CHARSXP, one day this
 * will handle null characters in CHARSXPs and not just blindly call
 * OutString.  */
static void OutCHARSXP (FILE *fp, SEXP s, OutputRoutines *m, SaveLoadData *d)
{
    R_assert(TYPEOF(s) == CHARSXP);
    m->OutString(fp, CHAR(s), d);
}

static void NewWriteVec (SEXP s, SEXP sym_list, SEXP env_list, FILE *fp, OutputRoutines *m, SaveLoadData *d)
{
    int count;

    /* I can assert here that `s' is one of the vector types, but
     * it'll turn out to be one big ugly statement... so I'll do it at
     * the bottom.  */

    m->OutInteger(fp, LENGTH(s), d);
    m->OutNewline(fp, d);
    switch (TYPEOF(s)) {
    case CHARSXP:
	m->OutSpace(fp, 1, d);
	OutCHARSXP(fp, s, m, d);
	break;
    case LGLSXP:
    case INTSXP:
	OutVec(fp, s, INTEGER_ELT, m->OutInteger, m, d);
	break;
    case REALSXP:
	OutVec(fp, s, REAL_ELT, m->OutReal, m, d);
	break;
    case CPLXSXP:
	OutVec(fp, s, COMPLEX_ELT, m->OutComplex, m, d);
	break;
    case STRSXP:
	do {
		int cnt;
		for (cnt = 0; cnt < LENGTH(s); ++cnt) {
			m->OutSpace(fp, 1, d);
			OutCHARSXP(fp, STRING_ELT(s, cnt), m, d);
			m->OutNewline(fp, d);
		}
	} while (0);
	break;
    case VECSXP:
    case EXPRSXP:
	for (count = 0; count < LENGTH(s); ++count) {
	    /* OutSpace(fp, 1); */
	    NewWriteItem(VECTOR_ELT(s, count), sym_list, env_list, fp, m, d);
	    m->OutNewline(fp, d);
	}
	break;
    default:
	error(_("NewWriteVec called with non-vector type"));
    }
}

static void NewWriteItem (SEXP s, SEXP sym_list, SEXP env_list, FILE *fp, OutputRoutines *m, SaveLoadData *d)
{
    int i;

    if ((i = NewSaveSpecialHook(s))) {
	m->OutInteger(fp, i, d);
	m->OutNewline(fp, d);
    }
    else {
	m->OutInteger(fp, TYPEOF(s), d);
	m->OutSpace(fp, 1, d); m->OutInteger(fp, LEVELS(s), d);
	m->OutSpace(fp, 1, d); m->OutInteger(fp, OBJECT(s), d);
	m->OutNewline(fp, d);
	switch (TYPEOF(s)) {
	    /* Note : NILSXP can't occur here */
	case SYMSXP:
	    i = NewLookup(s, sym_list);
	    R_assert(i);
	    m->OutInteger(fp, i, d); m->OutNewline(fp, d);
	    break;
	case ENVSXP:
	    i = NewLookup(s, env_list);
	    R_assert(i);
	    m->OutInteger(fp, i, d); m->OutNewline(fp, d);
	    break;
	case LISTSXP:
	case LANGSXP:
	case CLOSXP:
	case PROMSXP:
	case DOTSXP:
	    /* Dotted pair objects */
	    NewWriteItem(TAG(s), sym_list, env_list, fp, m, d);
	    NewWriteItem(CAR(s), sym_list, env_list, fp, m, d);
	    NewWriteItem(CDR(s), sym_list, env_list, fp, m, d);
	    break;
	case EXTPTRSXP:
	    NewWriteItem(EXTPTR_PROT(s), sym_list, env_list, fp, m, d);
	    NewWriteItem(EXTPTR_TAG(s), sym_list, env_list, fp, m, d);
	    break;
	case WEAKREFSXP:
	    /* Weak references */
	    break;
	case SPECIALSXP:
	case BUILTINSXP:
	    /* Builtin functions */
	    m->OutString(fp, PRIMNAME(s), d); m->OutNewline(fp, d);
	    break;
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case VECSXP:
	case EXPRSXP:
	    /* Vector Objects */
	    NewWriteVec(s, sym_list, env_list, fp, m, d);
	    break;
	case BCODESXP:
	    error(_("cannot save byte code objects in version 1 workspaces"));
	default:
	    error(_("NewWriteItem: unknown type %i"), TYPEOF(s));
	}
	NewWriteItem(ATTRIB(s), sym_list, env_list, fp, m, d);
    }
}

/*  General format: the total number of symbols, then the total number
 *  of environments.  Then all the symbol names get written out,
 *  followed by the environments, then the items to be saved.  If
 *  symbols or environments are encountered, references to them are
 *  made instead of writing them out totally.  */

static void newdatasave_cleanup(void *data)
{
    OutputCtxtData *cinfo = (OutputCtxtData*)data;
    FILE *fp = cinfo->fp;
    cinfo->methods->OutTerm(fp, cinfo->data);
}

static void NewDataSave (SEXP s, FILE *fp, OutputRoutines *m, SaveLoadData *d)
{
    SEXP sym_table, env_table, iterator;
    int sym_count, env_count;
    RCNTXT cntxt;
    OutputCtxtData cinfo;
    cinfo.fp = fp; cinfo.methods = m;  cinfo.data = d;

    PROTECT(sym_table = MakeHashTable());
    PROTECT(env_table = MakeHashTable());
    NewMakeLists(s, sym_table, env_table);
    FixHashEntries(sym_table);
    FixHashEntries(env_table);

    m->OutInit(fp, d);
    /* set up a context which will call OutTerm if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &newdatasave_cleanup;
    cntxt.cenddata = &cinfo;

    m->OutInteger(fp, sym_count = HASH_TABLE_COUNT(sym_table), d); m->OutSpace(fp, 1, d);
    m->OutInteger(fp, env_count = HASH_TABLE_COUNT(env_table), d); m->OutNewline(fp, d);
    for (iterator = HASH_TABLE_KEYS_LIST(sym_table);
	 sym_count--;
	 iterator = CDR(iterator)) {
	R_assert(TYPEOF(CAR(iterator)) == SYMSXP);
	m->OutString(fp, CHAR(PRINTNAME(CAR(iterator))), d);
	m->OutNewline(fp, d);
    }
    for (iterator = HASH_TABLE_KEYS_LIST(env_table);
	 env_count--;
	 iterator = CDR(iterator)) {
	R_assert(TYPEOF(CAR(iterator)) == ENVSXP);
	NewWriteItem(ENCLOS(CAR(iterator)), sym_table, env_table, fp, m, d);
	NewWriteItem(FRAME(CAR(iterator)), sym_table, env_table, fp, m, d);
	NewWriteItem(TAG(CAR(iterator)), sym_table, env_table, fp, m, d);
    }
    NewWriteItem(s, sym_table, env_table, fp, m, d);

    /* end the context after anything that could raise an error but before
       calling OutTerm so it doesn't get called twice */
    endcontext(&cntxt);

    m->OutTerm(fp, d);
    UNPROTECT(2);
}

#define InVec(fp, obj, accessor, infunc, length, d)			\
	do {								\
		int cnt;						\
		for (cnt = 0; cnt < length; ++cnt)		\
			accessor(obj, cnt, infunc(fp, d));		\
	} while (0)



#define SET_LOGICAL_ELT(x,__i__,v)	(LOGICAL_ELT(x,__i__)=(v))
#define SET_INTEGER_ELT(x,__i__,v)	(INTEGER_ELT(x,__i__)=(v))
#define SET_REAL_ELT(x,__i__,v)		(REAL_ELT(x,__i__)=(v))
#define SET_COMPLEX_ELT(x,__i__,v)	(COMPLEX_ELT(x,__i__)=(v))

static SEXP InCHARSXP (FILE *fp, InputRoutines *m, SaveLoadData *d)
{
    SEXP s;
    char *tmp;
    size_t len;

    /* FIXME: rather than use strlen, use actual length of string when
     * sized strings get implemented in R's save/load code.  */
    tmp = m->InString(fp, d);
    len = strlen(tmp);
    R_AllocStringBuffer(len, &(d->buffer));
    s = mkChar(tmp);
    return s;
}

static SEXP NewReadVec(SEXPTYPE type, SEXP sym_table, SEXP env_table, FILE *fp, InputRoutines *m, SaveLoadData *d)
{
    int length, count;
    SEXP my_vec;

    length = m->InInteger(fp, d);
    PROTECT(my_vec = allocVector(type, length));
    switch(type) {
    case CHARSXP:
	my_vec = InCHARSXP(fp, m, d);
	break;
    case LGLSXP:
    case INTSXP:
	InVec(fp, my_vec, SET_INTEGER_ELT, m->InInteger, length, d);
	break;
    case REALSXP:
	InVec(fp, my_vec, SET_REAL_ELT, m->InReal, length, d);
	break;
    case CPLXSXP:
	InVec(fp, my_vec, SET_COMPLEX_ELT, m->InComplex, length, d);
	break;
    case STRSXP:
	do {
	    int cnt;
	    for (cnt = 0; cnt < length(my_vec); ++cnt)
		SET_STRING_ELT(my_vec, cnt, InCHARSXP(fp, m, d));
	} while (0);
	break;
    case VECSXP:
    case EXPRSXP:
	for (count = 0; count < length; ++count)
	    SET_VECTOR_ELT(my_vec, count, NewReadItem(sym_table, env_table, fp, m, d));
	break;
    default:
	error(_("NewReadVec called with non-vector type"));
    }
    UNPROTECT(1);
    return my_vec;
}

static SEXP NewReadItem (SEXP sym_table, SEXP env_table, FILE *fp,
			 InputRoutines *m, SaveLoadData *d)
{
    SEXPTYPE type;
    SEXP s;
    int pos, levs, objf;

    R_assert(TYPEOF(sym_table) == VECSXP && TYPEOF(env_table) == VECSXP);
    type = m->InInteger(fp, d);
    if ((s = NewLoadSpecialHook(type)))
	return s;
    levs = m->InInteger(fp, d);
    objf = m->InInteger(fp, d);
    switch (type) {
    case SYMSXP:
	pos = m->InInteger(fp, d);
	PROTECT(s = pos ? VECTOR_ELT(sym_table, pos - 1) : R_NilValue);
	break;
    case ENVSXP:
	pos = m->InInteger(fp, d);
	PROTECT(s = pos ? VECTOR_ELT(env_table, pos - 1) : R_NilValue);
	break;
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case DOTSXP:
	PROTECT(s = allocSExp(type));
	SET_TAG(s, NewReadItem(sym_table, env_table, fp, m, d));
	SETCAR(s, NewReadItem(sym_table, env_table, fp, m, d));
	SETCDR(s, NewReadItem(sym_table, env_table, fp, m, d));
	/*UNPROTECT(1);*/
	break;
    case EXTPTRSXP:
	PROTECT(s = allocSExp(type));
	R_SetExternalPtrAddr(s, NULL);
	R_SetExternalPtrProtected(s, NewReadItem(sym_table, env_table, fp, m, d));
	R_SetExternalPtrTag(s, NewReadItem(sym_table, env_table, fp, m, d));
	/*UNPROTECT(1);*/
	break;
    case WEAKREFSXP:
	PROTECT(s = R_MakeWeakRef(R_NilValue, R_NilValue, R_NilValue, FALSE));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	R_AllocStringBuffer(MAXELTSIZE - 1, &(d->buffer));
	int index = StrToInternal(m->InString(fp, d));
	if (index == NA_INTEGER) {
	    warning(_("unrecognized internal function name \"%s\""), d->buffer.data);
	    PROTECT(s = R_NilValue);
	} else
	    PROTECT(s = mkPRIMSXP(index, type == BUILTINSXP));
	break;
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
	PROTECT(s = NewReadVec(type, sym_table, env_table, fp, m, d));
	break;
    case BCODESXP:
	error(_("cannot read byte code objects from version 1 workspaces"));
    default:
	error(_("NewReadItem: unknown type %i"), type);
    }
    SETLEVELS(s, (unsigned short) levs);
    SET_OBJECT(s, objf);
    SET_ATTRIB(s, NewReadItem(sym_table, env_table, fp, m, d));
    UNPROTECT(1); /* s */
    return s;
}

static void newdataload_cleanup(void *data)
{
    InputCtxtData *cinfo = (InputCtxtData*)data;
    FILE *fp = (FILE *) data;
    cinfo->methods->InTerm(fp, cinfo->data);
}

static SEXP NewDataLoad (FILE *fp, InputRoutines *m, SaveLoadData *d)
{
    int sym_count, env_count, count;
    SEXP sym_table, env_table, obj;
    RCNTXT cntxt;
    InputCtxtData cinfo;
    cinfo.fp = fp; cinfo.methods = m; cinfo.data = d;

    m->InInit(fp, d);

    /* set up a context which will call InTerm if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &newdataload_cleanup;
    cntxt.cenddata = &cinfo;

    /* Read the table sizes */
    sym_count = m->InInteger(fp, d);
    env_count = m->InInteger(fp, d);

    /* Allocate the symbol and environment tables */
    PROTECT(sym_table = allocVector(VECSXP, sym_count));
    PROTECT(env_table = allocVector(VECSXP, env_count));

    /* Read back and install symbols */
    for (count = 0; count < sym_count; ++count) {
	SET_VECTOR_ELT(sym_table, count, install(m->InString(fp, d)));
    }
    /* Allocate the environments */
    for (count = 0; count < env_count; ++count)
	SET_VECTOR_ELT(env_table, count, allocSExp(ENVSXP));

    /* Now fill them in  */
    for (count = 0; count < env_count; ++count) {
	obj = VECTOR_ELT(env_table, count);
	SET_ENCLOS(obj, NewReadItem(sym_table, env_table, fp, m, d));
	SET_FRAME(obj, NewReadItem(sym_table, env_table, fp, m, d));
	SET_TAG(obj, NewReadItem(sym_table, env_table, fp, m, d));
	R_RestoreHashCount(obj);
    }

    /* Read the actual object back */
    PROTECT(obj = NewReadItem(sym_table, env_table, fp, m, d));

    /* end the context after anything that could raise an error but before
       calling InTerm so it doesn't get called twice */
    endcontext(&cntxt);

    /* Wrap up */
    m->InTerm(fp, d);
    UNPROTECT(3); /* obj, env_table, sym_table */
    return obj;
}

/* ----- L o w l e v e l -- A s c i i -- I / O ------ */

static void OutSpaceAscii(FILE *fp, int nspace, SaveLoadData *unused)
{
    while(--nspace >= 0)
	fputc(' ', fp);
}
static void OutNewlineAscii(FILE *fp, SaveLoadData *unused)
{
    fputc('\n', fp);
}

static void OutIntegerAscii(FILE *fp, int x, SaveLoadData *unused)
{
    if (x == NA_INTEGER) fprintf(fp, "NA");
    else fprintf(fp, "%d", x);
}

static int InIntegerAscii(FILE *fp, SaveLoadData *unused)
{
    char buf[128];
    int x, res;
    res = fscanf(fp, "%127s", buf);
    if(res != 1) error(_("read error"));
    if (strcmp(buf, "NA") == 0)
	return NA_INTEGER;
    else {
	res = sscanf(buf, "%d", &x);
	if(res != 1) error(_("read error"));
    }
    return x;
}

static void OutStringAscii(FILE *fp, const char *x, SaveLoadData *unused)
{
    size_t i, nbytes;
    nbytes = strlen(x);
    fprintf(fp, "%d ", (int) nbytes);
    for (i = 0; i < nbytes; i++) {
	switch(x[i]) {
	case '\n': fprintf(fp, "\\n");  break;
	case '\t': fprintf(fp, "\\t");  break;
	case '\v': fprintf(fp, "\\v");  break;
	case '\b': fprintf(fp, "\\b");  break;
	case '\r': fprintf(fp, "\\r");  break;
	case '\f': fprintf(fp, "\\f");  break;
	case '\a': fprintf(fp, "\\a");  break;
	case '\\': fprintf(fp, "\\\\"); break;
	case '\?': fprintf(fp, "\\?");  break;
	case '\'': fprintf(fp, "\\'");  break;
	case '\"': fprintf(fp, "\\\""); break;
	default  :
	    /* cannot print char in octal mode -> cast to unsigned
	       char first */
	    /* actually, since x is signed char and '\?' == 127
	       is handled above, x[i] > 126 can't happen, but
	       I'm superstitious...  -pd */
	    if (x[i] <= 32 || x[i] > 126)
		fprintf(fp, "\\%03o", (unsigned char) x[i]);
	    else
		fputc(x[i], fp);
	}
    }
}

static char *InStringAscii(FILE *fp, SaveLoadData *unused)
{
    static char *buf = NULL;
    static int buflen = 0;
    int c, d, i, j;
    int nbytes, res;
    res = fscanf(fp, "%d", &nbytes);
    if(res != 1) error(_("read error"));
    /* FIXME : Ultimately we need to replace */
    /* this with a real string allocation. */
    /* All buffers must die! */
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL) /* buf remains allocated */
	    error(_("out of memory reading ascii string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    while(isspace(c = fgetc(fp)))
	;
    ungetc(c, fp);
    for (i = 0; i < nbytes; i++) {
	if ((c =  fgetc(fp)) == '\\') {
	    switch(c = fgetc(fp)) {
	    case 'n' : buf[i] = '\n'; break;
	    case 't' : buf[i] = '\t'; break;
	    case 'v' : buf[i] = '\v'; break;
	    case 'b' : buf[i] = '\b'; break;
	    case 'r' : buf[i] = '\r'; break;
	    case 'f' : buf[i] = '\f'; break;
	    case 'a' : buf[i] = '\a'; break;
	    case '\\': buf[i] = '\\'; break;
	    case '?' : buf[i] = '\?'; break;
	    case '\'': buf[i] = '\''; break;
	    case '\"': buf[i] = '\"'; break;
	    case '0': case '1': case '2': case '3':
	    case '4': case '5': case '6': case '7':
		d = 0; j = 0;
		while('0' <= c && c < '8' && j < 3) {
		    d = d * 8 + (c - '0');
		    c = fgetc(fp);
		    j++;
		}
		buf[i] = (char) d;
		ungetc(c, fp);
		break;
	    default  : buf[i] = (char) c;
	    }
	}
	else buf[i] = (char) c;
    }
    buf[i] = '\0';
    return buf;
}

static void OutDoubleAscii(FILE *fp, double x, SaveLoadData *unused)
{
    if (!R_FINITE(x)) {
	if (ISNAN(x)) fprintf(fp, "NA");
	else if (x < 0) fprintf(fp, "-Inf");
	else fprintf(fp, "Inf");
    }
    /* 16: full precision; 17 gives 999, 000 &c */
    else fprintf(fp, "%.16g", x);
}

static double InDoubleAscii(FILE *fp, SaveLoadData *unused)
{
    char buf[128];
    double x;
    int res;
    res = fscanf(fp, "%127s", buf);
    if(res != 1) error(_("read error"));
    if (strcmp(buf, "NA") == 0)
	x = NA_REAL;
    else if (strcmp(buf, "Inf") == 0)
	x = R_PosInf;
    else if (strcmp(buf, "-Inf") == 0)
	x = R_NegInf;
    else {
	res = sscanf(buf, "%lg", &x);
	if(res != 1) error(_("read error"));
    }
    return x;
}

static void OutComplexAscii(FILE *fp, Rcomplex x, SaveLoadData *unused)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	fprintf(fp, "NA NA");
    else {
	OutDoubleAscii(fp, x.r, unused);
	OutSpaceAscii(fp, 1, unused);
	OutDoubleAscii(fp, x.i, unused);
    }
}

static Rcomplex InComplexAscii(FILE *fp, SaveLoadData *unused)
{
    Rcomplex x;
    x.r = InDoubleAscii(fp, unused);
    x.i = InDoubleAscii(fp, unused);
    return x;
}

static void NewAsciiSave(SEXP s, FILE *fp, SaveLoadData *d)
{
    OutputRoutines m;

    m.OutInit = DummyInit;
    m.OutInteger = OutIntegerAscii;
    m.OutReal = OutDoubleAscii;
    m.OutComplex = OutComplexAscii;
    m.OutString = OutStringAscii;
    m.OutSpace = OutSpaceAscii;
    m.OutNewline = OutNewlineAscii;
    m.OutTerm = DummyTerm;
    NewDataSave(s, fp, &m, d);
}

static SEXP NewAsciiLoad(FILE *fp, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = InIntegerAscii;
    m.InReal = InDoubleAscii;
    m.InComplex = InComplexAscii;
    m.InString = InStringAscii;
    m.InTerm = DummyTerm;
    return NewDataLoad(fp, &m, d);
}

/* ----- L o w l e v e l -- B i n a r y -- I / O ----- */

static int InIntegerBinary(FILE * fp, SaveLoadData *unused)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error(_("a binary read error occurred"));
    return i;
}

static char *InStringBinary(FILE *fp, SaveLoadData *unused)
{
    static char *buf = NULL;
    static int buflen = 0;
    int nbytes = InIntegerBinary(fp, unused);
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL)
	    error(_("out of memory reading binary string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    if (fread(buf, sizeof(char), nbytes, fp) != nbytes)
	error(_("a binary string read error occurred"));
    buf[nbytes] = '\0';
    return buf;
}

static double InRealBinary(FILE * fp, SaveLoadData *unused)
{
    double x;
    if (fread(&x, sizeof(double), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static Rcomplex InComplexBinary(FILE * fp, SaveLoadData *unused)
{
    Rcomplex x;
    if (fread(&x, sizeof(Rcomplex), 1, fp) != 1)
	error(_("a read error occurred"));
    return x;
}

static SEXP NewBinaryLoad(FILE *fp, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = DummyInit;
    m.InInteger = InIntegerBinary;
    m.InReal = InRealBinary;
    m.InComplex = InComplexBinary;
    m.InString = InStringBinary;
    m.InTerm = DummyTerm;
    return NewDataLoad(fp, &m, d);
}


/* ----- L o w l e v e l -- X D R -- I / O ----- */

static void InInitXdr(FILE *fp, SaveLoadData *d)
{
    xdrstdio_create(&d->xdrs, fp, XDR_DECODE);
}

static void OutInitXdr(FILE *fp, SaveLoadData *d)
{
    xdrstdio_create(&d->xdrs, fp, XDR_ENCODE);
}

static void InTermXdr(FILE *fp, SaveLoadData *d)
{
    xdr_destroy(&d->xdrs);
}

static void OutTermXdr(FILE *fp, SaveLoadData *d)
{
    xdr_destroy(&d->xdrs);
}

static void OutIntegerXdr(FILE *fp, int i, SaveLoadData *d)
{
    if (!xdr_int(&d->xdrs, &i))
	error(_("an xdr integer data write error occurred"));
}

static int InIntegerXdr(FILE *fp, SaveLoadData *d)
{
    int i;
    if (!xdr_int(&d->xdrs, &i))
	error(_("an xdr integer data read error occurred"));
    return i;
}

static void OutStringXdr(FILE *fp, const char *s, SaveLoadData *d)
{
    unsigned int n = (unsigned int) strlen(s);
    char *t = CallocCharBuf(n);
    bool_t res;
    /* This copy may not be needed, will xdr_bytes ever modify 2nd arg? */
    strcpy(t, s);
    OutIntegerXdr(fp, n, d);
    res = xdr_bytes(&d->xdrs, &t, &n, n);
    Free(t);
    if (!res)
	error(_("an xdr string data write error occurred"));
}

static char *InStringXdr(FILE *fp, SaveLoadData *d)
{
    static char *buf = NULL;
    static int buflen = 0;
    unsigned int nbytes = InIntegerXdr(fp, d);
    if (nbytes >= buflen) {
	char *newbuf;
	/* Protect against broken realloc */
	if(buf) newbuf = (char *) realloc(buf, nbytes + 1);
	else newbuf = (char *) malloc(nbytes + 1);
	if (newbuf == NULL)
	    error(_("out of memory reading binary string"));
	buf = newbuf;
	buflen = nbytes + 1;
    }
    if (!xdr_bytes(&d->xdrs, &buf, &nbytes, nbytes))
	error(_("an xdr string data write error occurred"));
    buf[nbytes] = '\0';
    return buf;
}

static void OutRealXdr(FILE *fp, double x, SaveLoadData *d)
{
    if (!xdr_double(&d->xdrs, &x))
	error(_("an xdr real data write error occurred"));
}

static double InRealXdr(FILE * fp, SaveLoadData *d)
{
    double x;
    if (!xdr_double(&d->xdrs, &x))
	error(_("an xdr real data read error occurred"));
    return x;
}

static void OutComplexXdr(FILE *fp, Rcomplex x, SaveLoadData *d)
{
    if (!xdr_double(&d->xdrs, &(x.r)) || !xdr_double(&d->xdrs, &(x.i)))
	error(_("an xdr complex data write error occurred"));
}

static Rcomplex InComplexXdr(FILE * fp, SaveLoadData *d)
{
    Rcomplex x;
    if (!xdr_double(&d->xdrs, &(x.r)) || !xdr_double(&d->xdrs, &(x.i)))
	error(_("an xdr complex data read error occurred"));
    return x;
}

static void NewXdrSave(SEXP s, FILE *fp, SaveLoadData *d)
{
    OutputRoutines m;

    m.OutInit = OutInitXdr;
    m.OutInteger = OutIntegerXdr;
    m.OutReal = OutRealXdr;
    m.OutComplex = OutComplexXdr;
    m.OutString = OutStringXdr;
    m.OutSpace = DummyOutSpace;
    m.OutNewline = DummyOutNewline;
    m.OutTerm = OutTermXdr;
    NewDataSave(s, fp, &m, d);
}

static SEXP NewXdrLoad(FILE *fp, SaveLoadData *d)
{
    InputRoutines m;

    m.InInit = InInitXdr;
    m.InInteger = InIntegerXdr;
    m.InReal = InRealXdr;
    m.InComplex = InComplexXdr;
    m.InString = InStringXdr;
    m.InTerm = InTermXdr;
    return NewDataLoad(fp, &m, d);
}


/* ----- F i l e -- M a g i c -- N u m b e r s ----- */

static void R_WriteMagic(FILE *fp, int number)
{
    unsigned char buf[5];
    size_t res;

    number = abs(number);
    switch (number) {
    case R_MAGIC_ASCII_V1:   /* Version 1 - R Data, ASCII Format */
	strcpy((char*)buf, "RDA1");
	break;
    case R_MAGIC_BINARY_V1:  /* Version 1 - R Data, Binary Format */
	strcpy((char*)buf, "RDB1");
	break;
    case R_MAGIC_XDR_V1:     /* Version 1 - R Data, XDR Binary Format */
	strcpy((char*)buf, "RDX1");
	break;
    case R_MAGIC_ASCII_V2:   /* Version 2 - R Data, ASCII Format */
	strcpy((char*)buf, "RDA2");
	break;
    case R_MAGIC_BINARY_V2:  /* Version 2 - R Data, Binary Format */
	strcpy((char*)buf, "RDB2");
	break;
    case R_MAGIC_XDR_V2:     /* Version 2 - R Data, XDR Binary Format */
	strcpy((char*)buf, "RDX2");
	break;
    case R_MAGIC_ASCII_V3:   /* Version >=3 - R Data, ASCII Format */
	strcpy((char*)buf, "RDA3");
	break;
    case R_MAGIC_BINARY_V3:  /* Version >=3 - R Data, Binary Format */
	strcpy((char*)buf, "RDB3");
	break;
    case R_MAGIC_XDR_V3:     /* Version >=3 - R Data, XDR Binary Format */
	strcpy((char*)buf, "RDX3");
	break;
    default:
	buf[0] = (unsigned char)((number/1000) % 10 + '0');
	buf[1] = (unsigned char)((number/100) % 10 + '0');
	buf[2] = (unsigned char)((number/10) % 10 + '0');
	buf[3] = (unsigned char)(number % 10 + '0');
    }
    buf[4] = '\n';
    res = fwrite((char*)buf, sizeof(char), 5, fp);
    if(res != 5) error(_("write failed"));
}

static int R_ReadMagic(FILE *fp)
{
    unsigned char buf[6];
    int d1, d2, d3, d4;
    size_t count;

    count = fread((char*)buf, sizeof(char), 5, fp);
    if (count != 5) {
	if (count == 0)
	    return R_MAGIC_EMPTY;
	else
	    return R_MAGIC_CORRUPT;
    }

    if (strncmp((char*)buf, "RDA1\n", 5) == 0) {
	return R_MAGIC_ASCII_V1;
    }
    else if (strncmp((char*)buf, "RDB1\n", 5) == 0) {
	return R_MAGIC_BINARY_V1;
    }
    else if (strncmp((char*)buf, "RDX1\n", 5) == 0) {
	return R_MAGIC_XDR_V1;
    }
    if (strncmp((char*)buf, "RDA2\n", 5) == 0) {
	return R_MAGIC_ASCII_V2;
    }
    else if (strncmp((char*)buf, "RDB2\n", 5) == 0) {
	return R_MAGIC_BINARY_V2;
    }
    else if (strncmp((char*)buf, "RDX2\n", 5) == 0) {
	return R_MAGIC_XDR_V2;
    }
    if (strncmp((char*)buf, "RDA3\n", 5) == 0) {
	return R_MAGIC_ASCII_V3;
    }
    else if (strncmp((char*)buf, "RDB3\n", 5) == 0) {
	return R_MAGIC_BINARY_V3;
    }
    else if (strncmp((char*)buf, "RDX3\n", 5) == 0) {
	return R_MAGIC_XDR_V3;
    }
    else if (strncmp((char *)buf, "RD", 2) == 0)
	return R_MAGIC_MAYBE_TOONEW;

    /* Intel gcc seems to screw up a single expression here */
    d1 = (buf[3]-'0') % 10;
    d2 = (buf[2]-'0') % 10;
    d3 = (buf[1]-'0') % 10;
    d4 = (buf[0]-'0') % 10;
    return d1 + 10 * d2 + 100 * d3 + 1000 * d4;
}

static int defaultSaveVersion()
{
    static int dflt = -1;

    if (dflt < 0) {
	char *valstr = getenv("R_DEFAULT_SAVE_VERSION");
	int val = -1;
	if (valstr != NULL)
	    val = atoi(valstr);
	if (val == 2 || val == 3)
	    dflt = val;
	else
	    dflt = 3; /* the default */
    }
    return dflt;
}

/* ----- E x t e r n a l -- I n t e r f a c e s ----- */

void attribute_hidden R_SaveToFileV(SEXP obj, FILE *fp, int ascii, int version)
{
    SaveLoadData data = {{NULL, 0, MAXELTSIZE}};

    if (version == 1) {
	if (ascii) {
	    R_WriteMagic(fp, R_MAGIC_ASCII_V1);
	    NewAsciiSave(obj, fp, &data);
	} else {
	    R_WriteMagic(fp, R_MAGIC_XDR_V1);
	    NewXdrSave(obj, fp, &data);
	}
    }
    else {
	struct R_outpstream_st out;
	R_pstream_format_t type;
	int magic;

	/* version == 0 means default version */
	int v = (version == 0) ? defaultSaveVersion() : version;
	if (ascii) {
	    magic = (v == 2) ? R_MAGIC_ASCII_V2 : R_MAGIC_ASCII_V3;
	    type = R_pstream_ascii_format;
	}
	else {
	    magic = (v == 2) ? R_MAGIC_XDR_V2 : R_MAGIC_XDR_V3;
	    type = R_pstream_xdr_format;
	}
	R_WriteMagic(fp, magic);
	/* version == 0 means defaultSerializeVersion()
	   unsupported version will result in error  */
	R_InitFileOutPStream(&out, fp, type, version, NULL, NULL);
	R_Serialize(obj, &out);
    }
}

void attribute_hidden R_SaveToFile(SEXP obj, FILE *fp, int ascii)
{
    R_SaveToFileV(obj, fp, ascii, defaultSaveVersion());
}

    /* different handling of errors */

#define return_and_free(X) {r = X; R_FreeStringBuffer(&data.buffer); return r;}
SEXP attribute_hidden R_LoadFromFile(FILE *fp, int startup)
{
    struct R_inpstream_st in;
    int magic;
    SaveLoadData data = {{NULL, 0, MAXELTSIZE}};
    SEXP r;

    magic = R_ReadMagic(fp);
    switch(magic) {
    case R_MAGIC_XDR:
	return_and_free(XdrLoad(fp, startup, &data));
    case R_MAGIC_BINARY:
	return_and_free(BinaryLoad(fp, startup, &data));
    case R_MAGIC_ASCII:
	return_and_free(AsciiLoad(fp, startup, &data));
    case R_MAGIC_BINARY_VERSION16:
	return_and_free(BinaryLoadOld(fp, 16, startup, &data));
    case R_MAGIC_ASCII_VERSION16:
	return_and_free(AsciiLoadOld(fp, 16, startup, &data));
    case R_MAGIC_ASCII_V1:
	return_and_free(NewAsciiLoad(fp, &data));
    case R_MAGIC_BINARY_V1:
	return_and_free(NewBinaryLoad(fp, &data));
    case R_MAGIC_XDR_V1:
	return_and_free(NewXdrLoad(fp, &data));
    case R_MAGIC_ASCII_V2:
    case R_MAGIC_ASCII_V3:
	R_InitFileInPStream(&in, fp, R_pstream_ascii_format, NULL, NULL);
	return_and_free(R_Unserialize(&in));
    case R_MAGIC_BINARY_V2:
    case R_MAGIC_BINARY_V3:
	R_InitFileInPStream(&in, fp, R_pstream_binary_format, NULL, NULL);
	return_and_free(R_Unserialize(&in));
    case R_MAGIC_XDR_V2:
    case R_MAGIC_XDR_V3:
	R_InitFileInPStream(&in, fp, R_pstream_xdr_format, NULL, NULL);
	return_and_free(R_Unserialize(&in));
    default:
	R_FreeStringBuffer(&data.buffer);
	switch (magic) {
	case R_MAGIC_EMPTY:
	    error(_("restore file may be empty -- no data loaded"));
	case R_MAGIC_MAYBE_TOONEW:
	    error(_("restore file may be from a newer version of R -- no data loaded"));
	default:
	    error(_("bad restore file magic number (file may be corrupted) -- no data loaded"));
	}
	return(R_NilValue);/* for -Wall */
    }
}

SEXP attribute_hidden do_loadfile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP file, s;
    FILE *fp;

    checkArity(op, args);

    PROTECT(file = coerceVector(CAR(args), STRSXP));

    if (! isValidStringF(file))
	error(_("bad file name"));

    fp = RC_fopen(STRING_ELT(file, 0), "rb", TRUE);
    if (!fp)
	error(_("unable to open 'file'"));
    s = R_LoadFromFile(fp, 0);
    fclose(fp);

    UNPROTECT(1);
    return s;
}

SEXP attribute_hidden do_savefile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    FILE *fp;
    int version;

    checkArity(op, args);

    if (!isValidStringF(CADR(args)))
	error(_("'file' must be non-empty string"));
    if (TYPEOF(CADDR(args)) != LGLSXP)
	error(_("'ascii' must be logical"));
    if (CADDDR(args) == R_NilValue)
	version = defaultSaveVersion();
    else
	version = asInteger(CADDDR(args));
    if (version == NA_INTEGER || version <= 0)
	error(_("invalid '%s' argument"), "version");

    fp = RC_fopen(STRING_ELT(CADR(args), 0), "wb", TRUE);
    if (!fp)
	error(_("unable to open 'file'"));

    R_SaveToFileV(CAR(args), fp, INTEGER(CADDR(args))[0], version);

    fclose(fp);
    return R_NilValue;
}

static void saveload_cleanup(void *data)
{
    FILE *fp = (FILE *) data;
    fclose(fp);
}

/* Only used for version 1 saves */
SEXP attribute_hidden do_save(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* save(list, file, ascii, version, environment) */

    SEXP s, t, source, tmp;
    int len, j, version, ep;
    FILE *fp;
    RCNTXT cntxt;

    checkArity(op, args);


    if (TYPEOF(CAR(args)) != STRSXP)
	error(_("first argument must be a character vector"));
    if (!isValidStringF(CADR(args)))
	error(_("'file' must be non-empty string"));
    if (TYPEOF(CADDR(args)) != LGLSXP)
	error(_("'ascii' must be logical"));
    if (CADDDR(args) == R_NilValue)
	version = defaultSaveVersion();
    else
	version = asInteger(CADDDR(args));
    if (version == NA_INTEGER || version <= 0)
	error(_("invalid '%s' argument"), "version");
    source = CAR(nthcdr(args,4));
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error(_("invalid '%s' argument"), "environment");
    ep = asLogical(CAR(nthcdr(args,5)));
    if (ep == NA_LOGICAL)
	error(_("invalid '%s' argument"), "eval.promises");

    fp = RC_fopen(STRING_ELT(CADR(args), 0), "wb", TRUE);
    if (!fp) {
	const char *cfile = CHAR(STRING_ELT(CADR(args), 0));
	error(_("cannot open file '%s': %s"), cfile, strerror(errno));
    }

    /* set up a context which will close the file if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &saveload_cleanup;
    cntxt.cenddata = fp;

    len = length(CAR(args));
    PROTECT(s = allocList(len));

    t = s;
    for (j = 0; j < len; j++, t = CDR(t)) {
	SET_TAG(t, installTrChar(STRING_ELT(CAR(args), j)));
	tmp = findVar(TAG(t), source);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), EncodeChar(PRINTNAME(TAG(t))));
	if(ep && TYPEOF(tmp) == PROMSXP) {
	    PROTECT(tmp);
	    tmp = eval(tmp, source);
	    UNPROTECT(1);
	}
	SETCAR(t, tmp);
   }

    R_SaveToFileV(s, fp, INTEGER(CADDR(args))[0], version);

    UNPROTECT(1);
    /* end the context after anything that could raise an error but before
       closing the file so it doesn't get done twice */
    endcontext(&cntxt);
    fclose(fp);
    return R_NilValue;
}

static SEXP RestoreToEnv(SEXP ans, SEXP aenv)
{
    SEXP a, names, obj;
    int cnt = 0;
    /* Store the components of the list in aenv.  We either replace
     * the existing objects in aenv or establish new bindings for
     * them.
     */

    /* allow ans to be a vector-style list */
    if (TYPEOF(ans) == VECSXP) {
	int i;
	PROTECT(ans);
	PROTECT(names = getAttrib(ans, R_NamesSymbol)); /* PROTECT needed?? */
	if (TYPEOF(names) != STRSXP || LENGTH(names) != LENGTH(ans))
	    error(_("not a valid named list"));
	for (i = 0; i < LENGTH(ans); i++) {
	    SEXP sym = installTrChar(STRING_ELT(names, i));
	    obj = VECTOR_ELT(ans, i);
	    defineVar(sym, obj, aenv);
	    if(R_seemsOldStyleS4Object(obj))
		warningcall(R_NilValue,
			    _("'%s' looks like a pre-2.4.0 S4 object: please recreate it"),
			    CHAR(STRING_ELT(names, i)));
	}
	UNPROTECT(2);
	return names;
    }

    if (! isList(ans))
	error(_("loaded data is not in pair list form"));

    PROTECT(ans);
    a = ans;
    while (a != R_NilValue) {a = CDR(a); cnt++;}
    PROTECT(names = allocVector(STRSXP, cnt));
    cnt = 0;
    a = ans;
    while (a != R_NilValue) {
	SET_STRING_ELT(names, cnt++, PRINTNAME(TAG(a)));
	defineVar(TAG(a), CAR(a), aenv);
	if(R_seemsOldStyleS4Object(CAR(a)))
	    warningcall(R_NilValue,
			_("'%s' looks like a pre-2.4.0 S4 object: please recreate it"),
			CHAR(PRINTNAME(TAG(a))));
	a = CDR(a);
    }
    UNPROTECT(2);
    return names;
}

static SEXP R_LoadSavedData(FILE *fp, SEXP aenv)
{
    return RestoreToEnv(R_LoadFromFile(fp, 0), aenv);
}

/* This is only used for version 1 or earlier formats */
SEXP attribute_hidden do_load(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP fname, aenv, val;
    FILE *fp;
    RCNTXT cntxt;

    checkArity(op, args);

    if (!isValidString(fname = CAR(args)))
	error(_("first argument must be a file name"));

    /* GRW 1/26/99 GRW : added environment parameter so that */
    /* the loaded objects can be placed where desired  */

    aenv = CADR(args);
    if (TYPEOF(aenv) == NILSXP)
	error(_("use of NULL environment is defunct"));
    else if (TYPEOF(aenv) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");

    /* Process the saved file to obtain a list of saved objects. */
    fp = RC_fopen(STRING_ELT(fname, 0), "rb", TRUE);
    if (!fp) error(_("unable to open file"));

    /* set up a context which will close the file if there is an error */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &saveload_cleanup;
    cntxt.cenddata = fp;

    PROTECT(val = R_LoadSavedData(fp, aenv));

    /* end the context after anything that could raise an error but before
       closing the file so it doesn't get done twice */
    endcontext(&cntxt);
    fclose(fp);
    UNPROTECT(1);
    return val;
}

/* defined in Rinternals.h
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4
*/

void attribute_hidden R_XDREncodeDouble(double d, void *buf)
{
    XDR xdrs;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_DOUBLE_SIZE, XDR_ENCODE);
    success = xdr_double(&xdrs, &d);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR write failed"));
}

double attribute_hidden R_XDRDecodeDouble(void *buf)
{
    XDR xdrs;
    double d;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_DOUBLE_SIZE, XDR_DECODE);
    success = xdr_double(&xdrs, &d);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR read failed"));
    return d;
}

void attribute_hidden R_XDREncodeInteger(int i, void *buf)
{
    XDR xdrs;
    int success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_INTEGER_SIZE, XDR_ENCODE);
    success = xdr_int(&xdrs, &i);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR write failed"));
}

int attribute_hidden R_XDRDecodeInteger(void *buf)
{
    XDR xdrs;
    int i, success;

    xdrmem_create(&xdrs, (char *) buf, R_XDR_INTEGER_SIZE, XDR_DECODE);
    success = xdr_int(&xdrs, &i);
    xdr_destroy(&xdrs);
    if (! success)
	error(_("XDR read failed"));
    return i;
}

/* Next two were used in gnomeGUI package, are in Rinterface.h  */
void R_SaveGlobalEnvToFile(const char *name)
{
    SEXP sym = install("sys.save.image");
    if (findVar(sym, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	FILE *fp = R_fopen(name, "wb"); /* binary file */
	if (!fp) {
	    error(_("cannot save data -- unable to open '%s': %s"),
		  name, strerror(errno));
	}
	R_SaveToFile(FRAME(R_GlobalEnv), fp, 0);
	fclose(fp);
    }
    else {
	SEXP args, call;
	args = LCONS(ScalarString(mkChar(name)), R_NilValue);
	PROTECT(call = LCONS(sym, args));
	eval(call, R_GlobalEnv);
	UNPROTECT(1);
    }
}

void R_RestoreGlobalEnvFromFile(const char *name, Rboolean quiet)
{
    SEXP sym = install("sys.load.image");
    if (findVar(sym, R_GlobalEnv) == R_UnboundValue) { /* not a perfect test */
	FILE *fp = R_fopen(name, "rb"); /* binary file */
	if(fp != NULL) {
	    R_LoadSavedData(fp, R_GlobalEnv);
	    if(! quiet)
		Rprintf("[Previously saved workspace restored]\n\n");
	    fclose(fp);
	}
    }
    else {
	SEXP args, call, sQuiet;
	sQuiet = quiet ? mkTrue() : mkFalse();
	PROTECT(args = LCONS(sQuiet, R_NilValue));
	args = LCONS(ScalarString(mkChar(name)), args);
	PROTECT(call = LCONS(sym, args));
	eval(call, R_GlobalEnv);
	UNPROTECT(2);
    }
}


#include <Rconnections.h>

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}


/* Ideally it should be possible to do this entirely in R code with
   something like

	magic <- if (ascii) "RDA3\n" else ...
	writeChar(magic, con, eos = NULL)
	val <- lapply(list, get, envir = envir)
	names(val) <- list
	invisible(serialize(val, con, ascii = ascii))

   Unfortunately, this will result in too much duplication in the lapply
   (and any other way of doing this).  Hence we need an internal version.

   In case anyone wants to do this another way, in fact it is a
   pairlist of objects that is serialized, but RestoreToEnv copes
   with either a pairlist or list.
*/

SEXP attribute_hidden do_saveToConn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* saveToConn(list, conn, ascii, version, environment) */

    SEXP s, t, source, list, tmp;
    Rboolean ascii, wasopen;
    int len, j, version, ep;
    Rconnection con;
    struct R_outpstream_st out;
    R_pstream_format_t type;
    char magic[6];
    RCNTXT cntxt;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP)
	error(_("first argument must be a character vector"));
    list = CAR(args);

    con = getConnection(asInteger(CADR(args)));

    if (TYPEOF(CADDR(args)) != LGLSXP)
	error(_("'ascii' must be logical"));
    ascii = INTEGER(CADDR(args))[0];

    if (CADDDR(args) == R_NilValue)
	version = defaultSaveVersion();
    else
	version = asInteger(CADDDR(args));
    if (version == NA_INTEGER || version <= 0)
	error(_("invalid '%s' argument"), "version");
    if (version < 2)
	error(_("cannot save to connections in version %d format"), version);
    source = CAR(nthcdr(args,4));
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error(_("invalid '%s' argument"), "environment");
    ep = asLogical(CAR(nthcdr(args,5)));
    if (ep == NA_LOGICAL)
	error(_("invalid '%s' argument"), "eval.promises");

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* set up a context which will close the connection
	   if there is an error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canwrite)
	error(_("connection not open for writing"));

    strcpy(magic, "RD??\n");
    if (ascii) {
	magic[2] = 'A';
	type = (ascii == NA_LOGICAL) ?
	    R_pstream_asciihex_format : R_pstream_ascii_format;
    }
    else {
	if (con->text)
	    error(_("cannot save XDR format to a text-mode connection"));
	magic[2] = 'X';
	type = R_pstream_xdr_format;
    }
    /* if version is too high, R_Serialize will fail with error */
    magic[3] = (char)('0' + version);

    if (con->text)
	Rconn_printf(con, "%s", magic);
    else {
	size_t len = strlen(magic);
	if (len != con->write(magic, 1, len, con))
	    error(_("error writing to connection"));
    }

    R_InitConnOutPStream(&out, con, type, version, NULL, NULL);

    len = length(list);
    PROTECT(s = allocList(len));

    t = s;
    for (j = 0; j < len; j++, t = CDR(t)) {
	SET_TAG(t, installTrChar(STRING_ELT(list, j)));
	SETCAR(t, findVar(TAG(t), source));
	tmp = findVar(TAG(t), source);
	if (tmp == R_UnboundValue)
	    error(_("object '%s' not found"), EncodeChar(PRINTNAME(TAG(t))));
	if(ep && TYPEOF(tmp) == PROMSXP) {
	    PROTECT(tmp);
	    tmp = eval(tmp, source);
	    UNPROTECT(1);
	}
	SETCAR(t, tmp);
    }

    R_Serialize(s, &out);
    if (!wasopen) con->close(con);
    UNPROTECT(1);
    return R_NilValue;
}

/* Read and checks the magic number, open the connection if needed */

SEXP attribute_hidden do_loadFromConn2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* 0 .. loadFromConn2(conn, environment, verbose) */
    /* 1 .. loadInfoFromConn2(conn) */

    struct R_inpstream_st in;
    Rconnection con;
    SEXP aenv = R_NilValue, res = R_NilValue;
    unsigned char buf[6];
    size_t count;
    Rboolean wasopen;
    RCNTXT cntxt;

    checkArity(op, args);

    con = getConnection(asInteger(CAR(args)));

    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "rb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* set up a context which will close the connection
	   if there is an error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canread) error(_("connection not open for reading"));
    if(con->text) error(_("can only load() from a binary connection"));

    if (PRIMVAL(op) == 0) {
	aenv = CADR(args);
	if (TYPEOF(aenv) == NILSXP)
	    error(_("use of NULL environment is defunct"));
	else if (TYPEOF(aenv) != ENVSXP)
	    error(_("invalid '%s' argument"), "envir");
    }

    /* check magic */
    memset(buf, 0, 6);
    count = con->read(buf, sizeof(char), 5, con);
    if (count == 0) error(_("no input is available"));
    if (strncmp((char*)buf, "RDA2\n", 5) == 0 ||
	strncmp((char*)buf, "RDB2\n", 5) == 0 ||
	strncmp((char*)buf, "RDX2\n", 5) == 0 ||
	strncmp((char*)buf, "RDA3\n", 5) == 0 ||
	strncmp((char*)buf, "RDB3\n", 5) == 0 ||
	strncmp((char*)buf, "RDX3\n", 5) == 0) {
	R_InitConnInPStream(&in, con, R_pstream_any_format, NULL, NULL);
	if (PRIMVAL(op) == 0) {
	    int old_InitReadItemDepth = R_InitReadItemDepth,
		old_ReadItemDepth = R_ReadItemDepth;
	    R_InitReadItemDepth = R_ReadItemDepth = -asInteger(CADDR(args));
	    res = RestoreToEnv(R_Unserialize(&in), aenv);
	    R_InitReadItemDepth = old_InitReadItemDepth;
	    R_ReadItemDepth = old_ReadItemDepth;
	} else 
	    res = R_SerializeInfo(&in);
	if(!wasopen) {
	    /* PROTECT is paranoia: some close() method might allocate */
	    PROTECT(res);
	    endcontext(&cntxt);
	    con->close(con);
	    UNPROTECT(1);
	}
    } else
	error(_("the input does not start with a magic number compatible with loading from a connection"));
    return res;
}

