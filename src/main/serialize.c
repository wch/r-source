/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2019  The R Core Team
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
#include <Rmath.h>
#include <Fileio.h>
#include <Rversion.h>
#include <R_ext/Riconv.h>
#include <R_ext/RS.h>           /* for CallocCharBuf, Free */
#include <errno.h>
#include <ctype.h>		/* for isspace */
#include <stdarg.h>
#ifdef Win32
#include <trioremap.h>
#endif

/* From time to time changes in R, such as the addition of a new SXP,
 * may require changes in the save file format.  Here are some
 * guidelines on handling format changes:
 *
 *    Starting with 1.4 there is a version number associated with save
 *    file formats.  This version number should be incremented when
 *    the format is changed so older versions of R can recognize and
 *    reject the new format with a meaningful error message.
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
 *    provided in a released version, then these should specify the
 *    oldest reader R version as -1.
 */
 
 /* It is now customary that the version (1, 2, 3) of the format is
  * reflected also in magic numbers (such as RDX2, RDX3, ...), together with
  * type (xdr/ascii/binary).  Adding a new serialization format thus now
  * also requires adding a new set of magic numbers, yet in principle this
  * could be changed in the future.  The code in this file does not need the
  * magic numbers, it relies on version and type information in the
  * serialization header (version 2 and 3).
  */

/* ----- V e r s i o n -- T w o -- S a v e / R e s t o r e ----- */

/* Adapted from Chris Young and Ross Ihaka's Version One by Luke
   Tierney.  Copyright Assigned to the R Project.

   The approach used here uses a single pass over the node tree to be
   serialized.  Sharing of reference objects is preserved, but sharing
   among other objects is ignored.  The first time a reference object
   is encountered it is entered in a hash table; the value stored with
   the object is the index in the sequence of reference objects (1 for
   first reference object, 2 for second, etc.).  When an object is
   seen again, i.e. it is already in the hash table, a reference
   marker along with the index is written out.  The unserialize code
   does not know in advance how many reference objects it will see, so
   it starts with an initial array of some reasonable size and doubles
   it each time space runs out.  Reference objects are entered as they
   are encountered.

   This means the serialize and unserialize code needs to agree on
   what is a reference object.  Making a non-reference object into
   a reference object requires a version change in the format.  An
   alternate design would be to precede each reference object with a
   marker that says the next thing is a possibly shared object and
   needs to be entered into the reference table.

   Adding new SXP types is easy, whether they are reference objects or
   not.  The unserialize code will signal an error if it sees a type
   value it does not know.  It is of course better to increment the
   serialization format number when a new SXP is added, but if that
   SXP is unlikely to be saved by users then it may be simpler to keep
   the version number and let the error handling code deal with it.

   The output format for dotted pairs writes the ATTRIB value first
   rather than last.  This allows CDR's to be processed by iterative
   tail calls to avoid recursion stack overflows when processing long
   lists.  The writing code does take advantage of this, but the
   reading code does not.  It hasn't been a big issue so far--the only
   case where it has come up is in saving a large unhashed environment
   where saving succeeds but loading fails because the PROTECT stack
   overflows.  With the ability to create hashed environments at the
   user level this is likely to be even less of an issue now.  But if
   we do need to deal with it we can do so without a change in the
   serialization format--just rewrite ReadItem to pass the place to
   store the CDR it reads. (It's a bit of a pain to do, that is why it
   is being deferred until it is clearly needed.)

   CHARSXPs are now handled in a way that preserves both embedded null
   characters and NA_STRING values.

   The XDR save format now only uses the in-memory xdr facility for
   converting integers and doubles to a portable format.

   The output format packs the type flag and other flags into a single
   integer.  This produces more compact output for code; it has little
   effect on data.

   Environments recognized as package or namespace environments are
   not saved directly. Instead, a STRSXP is saved that is then used to
   attempt to find the package/namespace when unserialized.  The
   exact mechanism for choosing the name and finding the package/name
   space from the name still has to be developed, but the
   serialization format should be able to accommodate any reasonable
   mechanism.

   The mechanism assumes that user code supplies one routine for
   handling single characters and one for handling an array of bytes.
   Higher level interfaces that serialize to/from a FILE * pointer or
   an Rconnection pointer are provided in this file; others can be
   built easily.

   A mechanism is provided to allow special handling of non-system
   reference objects (all weak references and external pointers, and
   all environments other than package environments, namespace
   environments, and the global environment).  The hook function
   consists of a function pointer and a data value.  The serialization
   function pointer is called with the reference object and the data
   value as arguments.  It should return R_NilValue for standard
   handling and an STRSXP for special handling.  If an STRSXP is
   returned, then a special handing mark is written followed by the
   strings in the STRSXP (attributes are ignored).  On unserializing,
   any specially marked entry causes a call to the hook function with
   the reconstructed STRSXP and data value as arguments.  This should
   return the value to use for the reference object.  A reasonable
   convention on how to use this mechanism is neded, but again the
   format should be compatible with any reasonable convention.

   Eventually it may be useful to use these hooks to allow objects
   with a class to have a class-specific serialization mechanism.  The
   serialization format should support this.  It is trickier than in
   Java and other reference based languages where creation and
   initialization can be separated--we don't really have that option
   at the R level.  */

/* ----- V e r s i o n -- T h r e e -- S a v e / R e s t o r e ----- */

/* This format extends version 2 format by adding an identifier of the
   current native encoding to the serialization header.  On deserialization,
   strings without an encoding flag will be converted to the current native
   encoding, if possible, or to (flagged) UTF-8.  The conversion may fail
   when the original encoding is not supported by iconv (unlikely) or when
   the string is not valid in its declared encoding, which unfortunately is
   not uncommon.  The conversion code now deliberately does not check
   whether strings are valid when no conversion is needed, but such check
   could be added in the future without changing the format.

   Version 3 also adds support for custom ALTREP serialization. Under
   version 2 ALTREP objects are serialied like non-ALTREP ones. */

/*
 * Forward Declarations
 */

static void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table);
static void WriteItem (SEXP s, SEXP ref_table, R_outpstream_t stream);
static SEXP ReadItem(SEXP ref_table, R_inpstream_t stream);
static void WriteBC(SEXP s, SEXP ref_table, R_outpstream_t stream);
static SEXP ReadBC(SEXP ref_table, R_inpstream_t stream);

/*
 * Constants
 */

/* The default version used when a stream Init function is called with
   version = 0 */

static int defaultSerializeVersion()
{
    static int dflt = -1;

    if (dflt < 0) {
	char *valstr = getenv("R_DEFAULT_SERIALIZE_VERSION");
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

/*
 * Utility Functions
 *
 * An assert function which doesn't crash the program.
 * Something like this might be useful in an R header file
 */

#ifdef NDEBUG
#define R_assert(e) ((void) 0)
#else
/* The line below requires an ANSI C preprocessor (stringify operator) */
#define R_assert(e) ((e) ? (void) 0 : error("assertion '%s' failed: file '%s', line %d\n", #e, __FILE__, __LINE__))
#endif /* NDEBUG */

/* Rsnprintf: like snprintf, but guaranteed to null-terminate. */
static int Rsnprintf(char *buf, int size, const char *format, ...)
{
    int val;
    va_list(ap);
    va_start(ap, format);
    /* On Windows this no longer uses the non-C99 MSVCRT.dll version */
    val = vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    va_end(ap);
    return val;
}


/*
 * Basic Output Routines
 */

static void OutInteger(R_outpstream_t stream, int i)
{
    char buf[128];
    switch (stream->type) {
    case R_pstream_ascii_format:
    case R_pstream_asciihex_format:
	if (i == NA_INTEGER)
	    Rsnprintf(buf, sizeof(buf), "NA\n");
	else
	    Rsnprintf(buf, sizeof(buf), "%d\n", i);
	stream->OutBytes(stream, buf, (int)strlen(buf));
	break;
    case R_pstream_binary_format:
	stream->OutBytes(stream, &i, sizeof(int));
	break;
    case R_pstream_xdr_format:
	R_XDREncodeInteger(i, buf);
	stream->OutBytes(stream, buf, R_XDR_INTEGER_SIZE);
	break;
    default:
	error(_("unknown or inappropriate output format"));
    }
}

static void OutReal(R_outpstream_t stream, double d)
{
    char buf[128];
    switch (stream->type) {
    case R_pstream_ascii_format:
	if (! R_FINITE(d)) {
	    if (ISNA(d))
		Rsnprintf(buf, sizeof(buf), "NA\n");
	    else if (ISNAN(d))
		Rsnprintf(buf, sizeof(buf), "NaN\n");
	    else if (d < 0)
		Rsnprintf(buf, sizeof(buf), "-Inf\n");
	    else
		Rsnprintf(buf, sizeof(buf), "Inf\n");
	}
	else
	    /* 16: full precision; 17 gives 999, 000 &c */
	    Rsnprintf(buf, sizeof(buf), "%.16g\n", d);
	stream->OutBytes(stream, buf, (int)strlen(buf));
	break;
    case R_pstream_asciihex_format:
	if (! R_FINITE(d)) {
	    if (ISNA(d))
		Rsnprintf(buf, sizeof(buf), "NA\n");
	    else if (ISNAN(d))
		Rsnprintf(buf, sizeof(buf), "NaN\n");
	    else if (d < 0)
		Rsnprintf(buf, sizeof(buf), "-Inf\n");
	    else
		Rsnprintf(buf, sizeof(buf), "Inf\n");
	}
	else
	    Rsnprintf(buf, sizeof(buf), "%a\n", d);
	stream->OutBytes(stream, buf, (int)strlen(buf));
	break;
    case R_pstream_binary_format:
	stream->OutBytes(stream, &d, sizeof(double));
	break;
    case R_pstream_xdr_format:
	R_XDREncodeDouble(d, buf);
	stream->OutBytes(stream, buf, R_XDR_DOUBLE_SIZE);
	break;
    default:
	error(_("unknown or inappropriate output format"));
    }
}

static void OutComplex(R_outpstream_t stream, Rcomplex c)
{
    OutReal(stream, c.r);
    OutReal(stream, c.i);
}

static void OutByte(R_outpstream_t stream, Rbyte i)
{
    char buf[128];
    switch (stream->type) {
    case R_pstream_ascii_format:
    case R_pstream_asciihex_format:
	Rsnprintf(buf, sizeof(buf), "%02x\n", i);
	stream->OutBytes(stream, buf, (int)strlen(buf));
	break;
    case R_pstream_binary_format:
    case R_pstream_xdr_format:
	stream->OutBytes(stream, &i, 1);
	break;
    default:
	error(_("unknown or inappropriate output format"));
    }
}

/* This assumes CHARSXPs remain limited to 2^31-1 bytes */
static void OutString(R_outpstream_t stream, const char *s, int length)
{
    if (stream->type == R_pstream_ascii_format ||
	stream->type == R_pstream_asciihex_format) {
	int i;
	char buf[128];
	for (i = 0; i < length; i++) {
	    switch(s[i]) {
	    case '\n': sprintf(buf, "\\n");  break;
	    case '\t': sprintf(buf, "\\t");  break;
	    case '\v': sprintf(buf, "\\v");  break;
	    case '\b': sprintf(buf, "\\b");  break;
	    case '\r': sprintf(buf, "\\r");  break;
	    case '\f': sprintf(buf, "\\f");  break;
	    case '\a': sprintf(buf, "\\a");  break;
	    case '\\': sprintf(buf, "\\\\"); break;
	    case '\?': sprintf(buf, "\\?");  break;
	    case '\'': sprintf(buf, "\\'");  break;
	    case '\"': sprintf(buf, "\\\""); break;
	    default  :
		/* cannot print char in octal mode -> cast to unsigned
		   char first */
		/* actually, since s is signed char and '\?' == 127
		   is handled above, s[i] > 126 can't happen, but
		   I'm superstitious...  -pd */
		if (s[i] <= 32 || s[i] > 126)
		    sprintf(buf, "\\%03o", (unsigned char) s[i]);
		else
		    sprintf(buf, "%c", s[i]);
	    }
	    stream->OutBytes(stream, buf, (int)strlen(buf));
	}
	stream->OutChar(stream, '\n');
    }
    else
	stream->OutBytes(stream, (void *)s, length); /* FIXME: is this case right? */
}


/*
 * Basic Input Routines
 */

static void InWord(R_inpstream_t stream, char * buf, int size)
{
    int c, i;
    i = 0;
    do {
	c = stream->InChar(stream);
	if (c == EOF)
	    error(_("read error"));
    } while (isspace(c));
    while (! isspace(c) && i < size) {
	buf[i++] = (char) c;
	c = stream->InChar(stream);
    }
    if (i == size)
	error(_("read error"));
    buf[i] = 0;
}

static int InInteger(R_inpstream_t stream)
{
    char word[128];
    char buf[128];
    int i;

    switch (stream->type) {
    case R_pstream_ascii_format:
	InWord(stream, word, sizeof(word));
	if(sscanf(word, "%127s", buf) != 1) error(_("read error"));
	if (strcmp(buf, "NA") == 0)
	    return NA_INTEGER;
	else
	    if(sscanf(buf, "%d", &i) != 1) error(_("read error"));
	return i;
    case R_pstream_binary_format:
	stream->InBytes(stream, &i, sizeof(int));
	return i;
    case R_pstream_xdr_format:
	stream->InBytes(stream, buf, R_XDR_INTEGER_SIZE);
	return R_XDRDecodeInteger(buf);
    default:
	return NA_INTEGER;
    }
}

#ifdef Win32
extern int trio_sscanf(const char *buffer, const char *format, ...);

#endif

static double InReal(R_inpstream_t stream)
{
    char word[128];
    char buf[128];
    double d;

    switch (stream->type) {
    case R_pstream_ascii_format:
	InWord(stream, word, sizeof(word));
	if(sscanf(word, "%127s", buf) != 1) error(_("read error"));
	if (strcmp(buf, "NA") == 0)
	    return NA_REAL;
	else if (strcmp(buf, "NaN") == 0)
	    return R_NaN;
	else if (strcmp(buf, "Inf") == 0)
	    return R_PosInf;
	else if (strcmp(buf, "-Inf") == 0)
	    return R_NegInf;
	else
	    if(
#ifdef Win32
		trio_sscanf(buf, "%lg", &d)
#else
		sscanf(buf, "%lg", &d)
#endif
		!= 1) error(_("read error"));
	return d;
    case R_pstream_binary_format:
	stream->InBytes(stream, &d, sizeof(double));
	return d;
    case R_pstream_xdr_format:
	stream->InBytes(stream, buf, R_XDR_DOUBLE_SIZE);
	return R_XDRDecodeDouble(buf);
    default:
	return NA_REAL;
    }
}

static Rcomplex InComplex(R_inpstream_t stream)
{
    Rcomplex c;
    c.r = InReal(stream);
    c.i = InReal(stream);
    return c;
}

/* These utilities for reading characters with an unget option are
   defined so the code in InString can match the code in
   saveload.c:InStringAscii--that way it is easier to match changes in
   one to the other. */
typedef struct R_instring_stream_st {
    int last;
    R_inpstream_t stream;
} *R_instring_stream_t;

static void InitInStringStream(R_instring_stream_t s, R_inpstream_t stream)
{
    s->last = EOF;
    s->stream = stream;
}

static int GetChar(R_instring_stream_t s)
{
    int c;
    if (s->last != EOF) {
	c = s->last;
	s->last = EOF;
    }
    else c = s->stream->InChar(s->stream);
    return c;
}

static void UngetChar(R_instring_stream_t s, int c)
{
    s->last = c;
}


static void InString(R_inpstream_t stream, char *buf, int length)
{
    if (stream->type == R_pstream_ascii_format) {
	if (length > 0) {
	    int c, d, i, j;
	    struct R_instring_stream_st iss;

	    InitInStringStream(&iss, stream);
	    while(isspace(c = GetChar(&iss)))
		;
	    UngetChar(&iss, c);
	    for (i = 0; i < length; i++) {
		if ((c =  GetChar(&iss)) == '\\') {
		    switch(c = GetChar(&iss)) {
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
		    case '\"': buf[i] = '\"'; break; /* closing " for emacs */
		    case '0': case '1': case '2': case '3':
		    case '4': case '5': case '6': case '7':
			d = 0; j = 0;
			while('0' <= c && c < '8' && j < 3) {
			    d = d * 8 + (c - '0');
			    c = GetChar(&iss);
			    j++;
			}
			buf[i] = (char) d;
			UngetChar(&iss, c);
			break;
		    default  : buf[i] = (char) c;
		    }
		}
		else buf[i] = (char) c;
	    }
	}
    }
    else  /* this limits the string length: used for CHARSXPs */
	stream->InBytes(stream, buf, length);
}


/*
 * Format Header Reading and Writing
 *
 * The header starts with one of three characters, A for ascii, B for
 * binary, or X for xdr.
 */

static void OutFormat(R_outpstream_t stream)
{
/*    if (stream->type == R_pstream_binary_format) {
	warning(_("binary format is deprecated; using xdr instead"));
	stream->type = R_pstream_xdr_format;
	} */
    switch (stream->type) {
    case R_pstream_ascii_format:
    case R_pstream_asciihex_format:
	stream->OutBytes(stream, "A\n", 2); break;
	/* on deserialization, asciihex_format is treated exactly the same
	   way as ascii_format; the distinction is handled inside scanf %lg */
    case R_pstream_binary_format: stream->OutBytes(stream, "B\n", 2); break;
    case R_pstream_xdr_format:    stream->OutBytes(stream, "X\n", 2); break;
    case R_pstream_any_format:
	error(_("must specify ascii, binary, or xdr format"));
    default: error(_("unknown output format"));
    }
}

static void InFormat(R_inpstream_t stream)
{
    char buf[2];
    R_pstream_format_t type;
    stream->InBytes(stream, buf, 2);
    switch (buf[0]) {
    case 'A': type = R_pstream_ascii_format; break; /* also for asciihex */
    case 'B': type = R_pstream_binary_format; break;
    case 'X': type = R_pstream_xdr_format; break;
    case '\n':
	/* GROSS HACK: ASCII unserialize may leave a trailing newline
	   in the stream.  If the stream contains a second
	   serialization, then a second unserialize will fail if such
	   a newline is present.  The right fix is to make sure
	   unserialize consumes exactly what serialize produces.  But
	   this seems hard because of the current use of whitespace
	   skipping in unserialize.  So a temporary hack to cure the
	   symptom is to deal with a possible leading newline.  I
	   don't think more than one is possible, but I'm not sure.
	   LT */
	if (buf[1] == 'A') {
	    type = R_pstream_ascii_format;
	    stream->InBytes(stream, buf, 1);
	    break;
	}
    default:
	type = R_pstream_any_format;  /* keep compiler happy */
	error(_("unknown input format"));
    }
    if (stream->type == R_pstream_any_format)
	stream->type = type;
    else if (type != stream->type)
	error(_("input format does not match specified format"));
}


/*
 * Hash Table Functions
 *
 * Hashing functions for hashing reference objects during writing.
 * Objects are entered, and the order in which they are encountered is
 * recorded.  GashGet returns this number, a positive integer, if the
 * object was seen before, and zero if not.  A fixed hash table size
 * is used; this is not ideal but seems adequate for now.  The hash
 * table representation consists of a (R_NilValue . vector) pair.  The
 * hash buckets are in the vector.  This indirect representation
 * should allow resizing the table at some point.
 */

#define HASHSIZE 1099

#define PTRHASH(obj) (((R_size_t) (obj)) >> 2)

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

static void HashAdd(SEXP obj, SEXP ht)
{
    R_size_t pos = PTRHASH(obj) % HASH_TABLE_SIZE(ht);
    int count = HASH_TABLE_COUNT(ht) + 1;
    SEXP val = ScalarInteger(count);
    SEXP cell = CONS(val, HASH_BUCKET(ht, pos));

    SET_HASH_TABLE_COUNT(ht, count);
    SET_HASH_BUCKET(ht, pos, cell);
    SET_TAG(cell, obj);
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


/*
 * Administrative SXP values
 *
 * These macros defind SXP "type" for specifying special object, such
 * as R_NilValue, or control information, like REFSXP or NAMESPACESXP.
 * The range of SXP types is limited to 5 bit by the current sxpinfo
 * layout, but just in case these values are placed at the top of the
 * 8 bit range.
 */

#define REFSXP            255
#define NILVALUE_SXP      254
#define GLOBALENV_SXP     253
#define UNBOUNDVALUE_SXP  252
#define MISSINGARG_SXP    251
#define BASENAMESPACE_SXP 250
#define NAMESPACESXP      249
#define PACKAGESXP        248
#define PERSISTSXP        247
/* the following are speculative--we may or may not need them soon */
#define CLASSREFSXP       246
#define GENERICREFSXP     245
#define BCREPDEF          244
#define BCREPREF          243
#define EMPTYENV_SXP	  242
#define BASEENV_SXP	  241

/* The following are needed to preserve attribute information on
   expressions in the constant pool of byte code objects. This is
   mainly for preserving source references attributes.  The original
   implementation of the sharing-preserving writing and reading of byte
   code objects did not account for the need to preserve attributes,
   so there is now a work-around using these SXP types to flag when
   the ATTRIB field has been written out. Object bits and S4 bits are
   still not preserved.  In the long run it might be better to change
   to a scheme in which all sharing is preserved and byte code objects
   don't need to be handled as a special case.  LT */
#define ATTRLANGSXP       240
#define ATTRLISTSXP       239

#define ALTREP_SXP	  238

/*
 * Type/Flag Packing and Unpacking
 *
 * To reduce space consumption for serializing code (lots of list
 * structure) the type (at most 8 bits), several single bit flags,
 * and the sxpinfo gp field (LEVELS, 16 bits) are packed into a single
 * integer.  The integer is signed, so this shouldn't be pushed too
 * far.  It assumes at least 28 bits, but that should be no problem.
 */

#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)
#define ENCODE_LEVELS(v) ((v) << 12)
#define DECODE_LEVELS(v) ((v) >> 12)
#define DECODE_TYPE(v) ((v) & 255)

static int PackFlags(int type, int levs, int isobj, int hasattr, int hastag)
{
    /* We don't write out bit 5 as from R 2.8.0.
       It is used to indicate if an object is in CHARSXP cache
       - not that it matters to this version of R, but it saves
       checking all previous versions.

       Also make sure the HASHASH bit is not written out.
    */
    int val;
    if (type == CHARSXP) levs &= (~(CACHED_MASK | HASHASH_MASK));
    val = type | ENCODE_LEVELS(levs);
    if (isobj) val |= IS_OBJECT_BIT_MASK;
    if (hasattr) val |= HAS_ATTR_BIT_MASK;
    if (hastag) val |= HAS_TAG_BIT_MASK;
    return val;
}

static void UnpackFlags(int flags, SEXPTYPE *ptype, int *plevs,
			int *pisobj, int *phasattr, int *phastag)
{
    *ptype = DECODE_TYPE(flags);
    *plevs = DECODE_LEVELS(flags);
    *pisobj = flags & IS_OBJECT_BIT_MASK ? TRUE : FALSE;
    *phasattr = flags & HAS_ATTR_BIT_MASK ? TRUE : FALSE;
    *phastag = flags & HAS_TAG_BIT_MASK ? TRUE : FALSE;
}


/*
 * Reference/Index Packing and Unpacking
 *
 * Code will contain many references to symbols. As long as there are
 * not too many references, the index ant the REFSXP flag indicating a
 * reference can be packed in a single integeger.  Since the index is
 * 1-based, a 0 is used to indicate an index that doesn't fit and
 * therefore follows.
 */

#define PACK_REF_INDEX(i) (((i) << 8) | REFSXP)
#define UNPACK_REF_INDEX(i) ((i) >> 8)
#define MAX_PACKED_INDEX (INT_MAX >> 8)

static void OutRefIndex(R_outpstream_t stream, int i)
{
    if (i > MAX_PACKED_INDEX) {
	OutInteger(stream, REFSXP);
	OutInteger(stream, i);
    }
    else OutInteger(stream, PACK_REF_INDEX(i));
}

static int InRefIndex(R_inpstream_t stream, int flags)
{
    int i = UNPACK_REF_INDEX(flags);
    if (i == 0)
	return InInteger(stream);
    else
	return i;
}


/*
 * Persistent Name Hooks
 *
 * These routines call the appropriate hook functions for allowing
 * customized handling of reference objects.
 */

static SEXP GetPersistentName(R_outpstream_t stream, SEXP s)
{
    if (stream->OutPersistHookFunc != NULL) {
	switch (TYPEOF(s)) {
	case WEAKREFSXP:
	case EXTPTRSXP: break;
	case ENVSXP:
	    if (s == R_GlobalEnv ||
		s == R_BaseEnv ||
		s == R_EmptyEnv ||
		R_IsNamespaceEnv(s) ||
		R_IsPackageEnv(s))
		return R_NilValue;
	    else
		break;
	default: return R_NilValue;
	}
	return stream->OutPersistHookFunc(s, stream->OutPersistHookData);
    }
    else
	return R_NilValue;
}

static SEXP PersistentRestore(R_inpstream_t stream, SEXP s)
{
    if (stream->InPersistHookFunc == NULL)
	error(_("no restore method available"));
    return stream->InPersistHookFunc(s, stream->InPersistHookData);
}


/*
 * Serialization Code
 */

static int SaveSpecialHook(SEXP item)
{
    if (item == R_NilValue)      return NILVALUE_SXP;
    if (item == R_EmptyEnv)	 return EMPTYENV_SXP;
    if (item == R_BaseEnv)	 return BASEENV_SXP;
    if (item == R_GlobalEnv)     return GLOBALENV_SXP;
    if (item == R_UnboundValue)  return UNBOUNDVALUE_SXP;
    if (item == R_MissingArg)    return MISSINGARG_SXP;
    if (item == R_BaseNamespace) return BASENAMESPACE_SXP;
    return 0;
}

static void WriteLENGTH(R_outpstream_t stream, SEXP s)
{
#ifdef LONG_VECTOR_SUPPORT
    if (IS_LONG_VEC(s)) {
	OutInteger(stream, -1);
	R_xlen_t len = XLENGTH(s);
	OutInteger(stream, (int)(len / 4294967296L));
	OutInteger(stream, (int)(len % 4294967296L));
   } else OutInteger(stream, LENGTH(s));
#else
    OutInteger(stream, LENGTH(s));
#endif
}

static void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table)
{
    R_assert(TYPEOF(s) == STRSXP);

#ifdef WARN_ABOUT_NAMES_IN_PERSISTENT_STRINGS
    SEXP names = getAttrib(s, R_NamesSymbol);
    if (names != R_NilValue)
	warning(_("names in persistent strings are currently ignored"));
#endif

    R_xlen_t len = XLENGTH(s);
    OutInteger(stream, 0); /* place holder to allow names if we want to */
    WriteLENGTH(stream, s);
    for (R_xlen_t i = 0; i < len; i++)
	WriteItem(STRING_ELT(s, i), ref_table, stream);
}

#include <rpc/types.h>
#include <rpc/xdr.h>

#define CHUNK_SIZE 8096

#define min2(a, b) ((a) < (b)) ? (a) : (b)


static R_INLINE void
OutIntegerVec(R_outpstream_t stream, SEXP s, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(int)];
	R_xlen_t done, this;
	XDR xdrs;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(int)), XDR_ENCODE);
	    for(int cnt = 0; cnt < this; cnt++)
		if(!xdr_int(&xdrs, INTEGER(s) + done + cnt))
		    error(_("XDR write failed"));
	    xdr_destroy(&xdrs);
	    stream->OutBytes(stream, buf, (int)(sizeof(int) * this));
	}
	break;
    }
    case R_pstream_binary_format:
    {
	/* write in chunks to avoid overflowing ints */
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->OutBytes(stream, INTEGER(s) + done,
			     (int)(sizeof(int) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    OutInteger(stream, INTEGER(s)[cnt]);
    }
}

static R_INLINE void
OutRealVec(R_outpstream_t stream, SEXP s, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(double)];
	R_xlen_t done, this;
	XDR xdrs;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(double)), XDR_ENCODE);
	    for(int cnt = 0; cnt < this; cnt++)
		if(!xdr_double(&xdrs, REAL(s) + done + cnt))
		    error(_("XDR write failed"));
	    xdr_destroy(&xdrs);
	    stream->OutBytes(stream, buf, (int)(sizeof(double) * this));
	}
	break;
    }
    case R_pstream_binary_format:
    {
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->OutBytes(stream, REAL(s) + done,
			     (int)(sizeof(double) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    OutReal(stream, REAL(s)[cnt]);
    }
}

static R_INLINE void
OutComplexVec(R_outpstream_t stream, SEXP s, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(Rcomplex)];
	R_xlen_t done, this;
	XDR xdrs;
	Rcomplex *c = COMPLEX(s);
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(Rcomplex)), XDR_ENCODE);
	    for(int cnt = 0; cnt < this; cnt++) {
		if(!xdr_double(&xdrs, &(c[done+cnt].r)) ||
		   !xdr_double(&xdrs, &(c[done+cnt].i)))
		    error(_("XDR write failed"));
	    }
	    stream->OutBytes(stream, buf, (int)(sizeof(Rcomplex) * this));
	    xdr_destroy(&xdrs);
	}
	break;
    }
    case R_pstream_binary_format:
    {
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->OutBytes(stream, COMPLEX(s) + done,
			     (int)(sizeof(Rcomplex) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    OutComplex(stream, COMPLEX(s)[cnt]);
    }
}

static void WriteItem (SEXP s, SEXP ref_table, R_outpstream_t stream)
{
    int i;
    SEXP t;

    if (R_compile_pkgs && TYPEOF(s) == CLOSXP && TYPEOF(BODY(s)) != BCODESXP &&
        !R_disable_bytecode &&
        (!IS_S4_OBJECT(s) || (!inherits(s, "refMethodDef") &&
                              !inherits(s, "defaultBindingFunction")))) {

	/* Do not compile reference class methods in their generators, because
	   the byte-code is dropped as soon as the method is installed into a
	   new environment. This is a performance optimization but it also
	   prevents byte-compiler warnings about no visible binding for super
	   assignment to a class field.

	   Do not compile default binding functions, because the byte-code is
	   dropped as fields are set in constructors (just an optimization).
	*/
	    
	SEXP new_s;
	R_compile_pkgs = FALSE;
	PROTECT(new_s = R_cmpfun1(s));
	WriteItem (new_s, ref_table, stream);
	UNPROTECT(1);
	R_compile_pkgs = TRUE;
	return;
    }

 tailcall:
    R_CheckStack();
    if (ALTREP(s) && stream->version >= 3) {
	SEXP info = ALTREP_SERIALIZED_CLASS(s);
	SEXP state = ALTREP_SERIALIZED_STATE(s);
	if (info != NULL && state != NULL) {
	    int flags = PackFlags(ALTREP_SXP, LEVELS(s), OBJECT(s), 0, 0);
	    PROTECT(state);
	    PROTECT(info);
	    OutInteger(stream, flags);
	    WriteItem(info, ref_table, stream);
	    WriteItem(state, ref_table, stream);
	    WriteItem(ATTRIB(s), ref_table, stream);
	    UNPROTECT(2); /* state, info */
	    return;
	}
	/* else fall through to standard processing */
    }
    if ((t = GetPersistentName(stream, s)) != R_NilValue) {
	R_assert(TYPEOF(t) == STRSXP && LENGTH(t) > 0);
	PROTECT(t);
	HashAdd(s, ref_table);
	OutInteger(stream, PERSISTSXP);
	OutStringVec(stream, t, ref_table);
	UNPROTECT(1);
    }
    else if ((i = SaveSpecialHook(s)) != 0)
	OutInteger(stream, i);
    else if ((i = HashGet(s, ref_table)) != 0)
	OutRefIndex(stream, i);
    else if (TYPEOF(s) == SYMSXP) {
	/* Note : NILSXP can't occur here */
	HashAdd(s, ref_table);
	OutInteger(stream, SYMSXP);
	WriteItem(PRINTNAME(s), ref_table, stream);
    }
    else if (TYPEOF(s) == ENVSXP) {
	HashAdd(s, ref_table);
	if (R_IsPackageEnv(s)) {
	    SEXP name = R_PackageEnvName(s);
	    warning(_("'%s' may not be available when loading"),
		    CHAR(STRING_ELT(name, 0)));
	    OutInteger(stream, PACKAGESXP);
	    OutStringVec(stream, name, ref_table);
	}
	else if (R_IsNamespaceEnv(s)) {
#ifdef WARN_ABOUT_NAME_SPACES_MAYBE_NOT_AVAILABLE
	    warning(_("namespaces may not be available when loading"));
#endif
	    OutInteger(stream, NAMESPACESXP);
	    OutStringVec(stream, PROTECT(R_NamespaceEnvSpec(s)), ref_table);
	    UNPROTECT(1);
	}
	else {
	    OutInteger(stream, ENVSXP);
	    OutInteger(stream, R_EnvironmentIsLocked(s) ? 1 : 0);
	    WriteItem(ENCLOS(s), ref_table, stream);
	    WriteItem(FRAME(s), ref_table, stream);
	    WriteItem(HASHTAB(s), ref_table, stream);
	    WriteItem(ATTRIB(s), ref_table, stream);
	}
    }
    else {
	int flags, hastag, hasattr;
	R_xlen_t len;
	switch(TYPEOF(s)) {
	case LISTSXP:
	case LANGSXP:
	case CLOSXP:
	case PROMSXP:
	case DOTSXP: hastag = TAG(s) != R_NilValue; break;
	default: hastag = FALSE;
	}
	/* With the CHARSXP cache chains maintained through the ATTRIB
	   field the content of that field must not be serialized, so
	   we treat it as not there. */
	hasattr = (TYPEOF(s) != CHARSXP && ATTRIB(s) != R_NilValue);
	flags = PackFlags(TYPEOF(s), LEVELS(s), OBJECT(s),
			  hasattr, hastag);
	OutInteger(stream, flags);
	switch (TYPEOF(s)) {
	case LISTSXP:
	case LANGSXP:
	case CLOSXP:
	case PROMSXP:
	case DOTSXP:
	    /* Dotted pair objects */
	    /* These write their ATTRIB fields first to allow us to avoid
	       recursion on the CDR */
	    if (hasattr)
		WriteItem(ATTRIB(s), ref_table, stream);
	    if (TAG(s) != R_NilValue)
		WriteItem(TAG(s), ref_table, stream);
	    if (BNDCELL_TAG(s))
		R_expand_binding_value(s);
	    WriteItem(CAR(s), ref_table, stream);
	    /* now do a tail call to WriteItem to handle the CDR */
	    s = CDR(s);
	    goto tailcall;
	case EXTPTRSXP:
	    /* external pointers */
	    HashAdd(s, ref_table);
	    WriteItem(EXTPTR_PROT(s), ref_table, stream);
	    WriteItem(EXTPTR_TAG(s), ref_table, stream);
	    break;
	case WEAKREFSXP:
	    /* Weak references */
	    HashAdd(s, ref_table);
	    break;
	case SPECIALSXP:
	case BUILTINSXP:
	    /* Builtin functions */
	    OutInteger(stream, (int)strlen(PRIMNAME(s)));
	    OutString(stream, PRIMNAME(s), (int)strlen(PRIMNAME(s)));
	    break;
	case CHARSXP:
	    if (s == NA_STRING)
		OutInteger(stream, -1);
	    else {
		OutInteger(stream, LENGTH(s));
		OutString(stream, CHAR(s), LENGTH(s));
	    }
	    break;
	case LGLSXP:
	case INTSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    OutIntegerVec(stream, s, len);
	    break;
	case REALSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    OutRealVec(stream, s, len);
	    break;
	case CPLXSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    OutComplexVec(stream, s, len);
	    break;
	case STRSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    for (R_xlen_t ix = 0; ix < len; ix++)
		WriteItem(STRING_ELT(s, ix), ref_table, stream);
	    break;
	case VECSXP:
	case EXPRSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    for (R_xlen_t ix = 0; ix < len; ix++)
		WriteItem(VECTOR_ELT(s, ix), ref_table, stream);
	    break;
	case BCODESXP:
	    WriteBC(s, ref_table, stream);
	    break;
	case RAWSXP:
	    len = XLENGTH(s);
	    WriteLENGTH(stream, s);
	    switch (stream->type) {
	    case R_pstream_xdr_format:
	    case R_pstream_binary_format:
	    {
		R_xlen_t done, this;
		for (done = 0; done < len; done += this) {
		    this = min2(CHUNK_SIZE, len - done);
		    stream->OutBytes(stream, RAW(s) + done, (int) this);
		}
		break;
	    }
	    default:
		for (R_xlen_t ix = 0; ix < len; ix++)
		    OutByte(stream, RAW(s)[ix]);
	    }
	    break;
	case S4SXP:
	  break; /* only attributes (i.e., slots) count */
	default:
	    error(_("WriteItem: unknown type %i"), TYPEOF(s));
	}
	if (hasattr)
	    WriteItem(ATTRIB(s), ref_table, stream);
    }
}

static SEXP MakeCircleHashTable(void)
{
    return CONS(R_NilValue, allocVector(VECSXP, HASHSIZE));
}

static Rboolean AddCircleHash(SEXP item, SEXP ct)
{
    SEXP table, bucket, list;

    table = CDR(ct);
    R_size_t pos = PTRHASH(item) % LENGTH(table);
    bucket = VECTOR_ELT(table, pos);
    for (list = bucket; list != R_NilValue; list = CDR(list))
	if (TAG(list) == item) {
	    if (CAR(list) == R_NilValue) {
		/* this is the second time; enter in list and mark */
		SETCAR(list, R_UnboundValue); /* anything different will do */
		SETCAR(ct, CONS(item, CAR(ct)));
	    }
	    return TRUE;
	}

    /* If we get here then this is a new item; enter in the table */
    bucket = CONS(R_NilValue, bucket);
    SET_TAG(bucket, item);
    SET_VECTOR_ELT(table, pos, bucket);
    return FALSE;
}

static void ScanForCircles1(SEXP s, SEXP ct)
{
    switch (TYPEOF(s)) {
    case LANGSXP:
    case LISTSXP:
	if (! AddCircleHash(s, ct)) {
	    ScanForCircles1(CAR(s), ct);
	    ScanForCircles1(CDR(s), ct);
	}
	break;
    case BCODESXP:
	{
	    int i, n;
	    SEXP consts = BCODE_CONSTS(s);
	    n = LENGTH(consts);
	    for (i = 0; i < n; i++)
		ScanForCircles1(VECTOR_ELT(consts, i), ct);
	}
	break;
    default: break;
    }
}

static SEXP ScanForCircles(SEXP s)
{
    SEXP ct;
    PROTECT(ct = MakeCircleHashTable());
    ScanForCircles1(s, ct);
    UNPROTECT(1);
    return CAR(ct);
}

static SEXP findrep(SEXP x, SEXP reps)
{
    for (; reps != R_NilValue; reps = CDR(reps))
	if (x == CAR(reps))
	    return reps;
    return R_NilValue;
}

static void WriteBCLang(SEXP s, SEXP ref_table, SEXP reps,
			R_outpstream_t stream)
{
    int type = TYPEOF(s);
    if (type == LANGSXP || type == LISTSXP) {
	SEXP r = findrep(s, reps);
	int output = TRUE;
	if (r != R_NilValue) {
	    /* we have a cell referenced more than once */
	    if (TAG(r) == R_NilValue) {
		/* this is the first reference, so update and register
		   the counter */
		int i = INTEGER(CAR(reps))[0]++;
		SET_TAG(r, allocVector(INTSXP, 1));
		INTEGER(TAG(r))[0] = i;
		OutInteger(stream, BCREPDEF);
		OutInteger(stream, i);
	    }
	    else {
		/* we've seen it before, so just put out the index */
		OutInteger(stream, BCREPREF);
		OutInteger(stream, INTEGER(TAG(r))[0]);
		output = FALSE;
	    }
	}
	if (output) {
	    SEXP attr = ATTRIB(s);
	    if (attr != R_NilValue) {
		switch(type) {
		case LANGSXP: type = ATTRLANGSXP; break;
		case LISTSXP: type = ATTRLISTSXP; break;
		}
	    }
	    OutInteger(stream, type);
	    if (attr != R_NilValue)
		WriteItem(attr, ref_table, stream);
	    WriteItem(TAG(s), ref_table, stream);
	    WriteBCLang(CAR(s), ref_table, reps, stream);
	    WriteBCLang(CDR(s), ref_table, reps, stream);
	}
    }
    else {
	OutInteger(stream, 0); /* pad */
	WriteItem(s, ref_table, stream);
    }
}

static void WriteBC1(SEXP s, SEXP ref_table, SEXP reps, R_outpstream_t stream)
{
    int i, n;
    SEXP code, consts;
    PROTECT(code = R_bcDecode(BCODE_CODE(s)));
    WriteItem(code, ref_table, stream);
    consts = BCODE_CONSTS(s);
    n = LENGTH(consts);
    OutInteger(stream, n);
    for (i = 0; i < n; i++) {
	SEXP c = VECTOR_ELT(consts, i);
	int type = TYPEOF(c);
	switch (type) {
	case BCODESXP:
	    OutInteger(stream, type);
	    WriteBC1(c, ref_table, reps, stream);
	    break;
	case LANGSXP:
	case LISTSXP:
	    WriteBCLang(c, ref_table, reps, stream);
	    break;
	default:
	    OutInteger(stream, type);
	    WriteItem(c, ref_table, stream);
	}
    }
    UNPROTECT(1);
}

static void WriteBC(SEXP s, SEXP ref_table, R_outpstream_t stream)
{
    SEXP reps = ScanForCircles(s);
    PROTECT(reps = CONS(R_NilValue, reps));
    OutInteger(stream, length(reps));
    SETCAR(reps, allocVector(INTSXP, 1));
    INTEGER(CAR(reps))[0] = 0;
    WriteBC1(s, ref_table, reps, stream);
    UNPROTECT(1);
}

void R_Serialize(SEXP s, R_outpstream_t stream)
{
    SEXP ref_table;
    int version = stream->version;

    OutFormat(stream);

    switch(version) {
    case 2:
	OutInteger(stream, version);
	OutInteger(stream, R_VERSION);
	OutInteger(stream, R_Version(2,3,0));
	break;
    case 3:
    {
	OutInteger(stream, version);
	OutInteger(stream, R_VERSION);
	OutInteger(stream, R_Version(3,5,0));
	const char *natenc = R_nativeEncoding();
	int nelen = (int) strlen(natenc);
	OutInteger(stream, nelen);
	OutString(stream, natenc, nelen);
	break;
    }
    default: error(_("version %d not supported"), version);
    }

    PROTECT(ref_table = MakeHashTable());
    WriteItem(s, ref_table, stream);
    UNPROTECT(1);
}


/*
 * Unserialize Code
 */

attribute_hidden int R_ReadItemDepth = 0, R_InitReadItemDepth;
static char lastname[8192];

#define INITIAL_REFREAD_TABLE_SIZE 128

static SEXP MakeReadRefTable(void)
{
    SEXP data = allocVector(VECSXP, INITIAL_REFREAD_TABLE_SIZE);
    SET_TRUELENGTH(data, 0);
    return CONS(data, R_NilValue);
}

static SEXP GetReadRef(SEXP table, int index)
{
    int i = index - 1;
    SEXP data = CAR(table);

    if (i < 0 || i >= LENGTH(data))
	error(_("reference index out of range"));
    return VECTOR_ELT(data, i);
}

static void AddReadRef(SEXP table, SEXP value)
{
    SEXP data = CAR(table);
    R_xlen_t count = TRUELENGTH(data) + 1;
    if (count >= LENGTH(data)) {
	R_xlen_t i, len;
	SEXP newdata;

	PROTECT(value);
	len = 2 * count;
	newdata = allocVector(VECSXP, len);
	for (i = 0; i < LENGTH(data); i++)
	    SET_VECTOR_ELT(newdata, i, VECTOR_ELT(data, i));
	SETCAR(table, newdata);
	data = newdata;
	UNPROTECT(1);
    }
    SET_TRUELENGTH(data, count);
    SET_VECTOR_ELT(data, count - 1, value);
}

static SEXP InStringVec(R_inpstream_t stream, SEXP ref_table)
{
    SEXP s;
    int i, len;
    if (InInteger(stream) != 0)
	error(_("names in persistent strings are not supported yet"));
    len = InInteger(stream);
    PROTECT(s = allocVector(STRSXP, len));
    R_ReadItemDepth++;
    for (i = 0; i < len; i++)
	SET_STRING_ELT(s, i, ReadItem(ref_table, stream));
    R_ReadItemDepth--;
    UNPROTECT(1);
    return s;
}

/* use static buffer to reuse storage */
static R_INLINE void
InIntegerVec(R_inpstream_t stream, SEXP obj, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(int)];
	R_xlen_t done, this;
	XDR xdrs;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, buf, (int)(sizeof(int) * this));
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(int)), XDR_DECODE);
	    for(int cnt = 0; cnt < this; cnt++)
		if(!xdr_int(&xdrs, INTEGER(obj) + done + cnt))
		    error(_("XDR read failed"));
	    xdr_destroy(&xdrs);
	}
	break;
    }
    case R_pstream_binary_format:
    {
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, INTEGER(obj) + done,
			    (int)(sizeof(int) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    INTEGER(obj)[cnt] = InInteger(stream);
    }
}

static R_INLINE void
InRealVec(R_inpstream_t stream, SEXP obj, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(double)];
	R_xlen_t done, this;
	XDR xdrs;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, buf, (int)(sizeof(double) * this));
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(double)), XDR_DECODE);
	    for(R_xlen_t cnt = 0; cnt < this; cnt++)
		if(!xdr_double(&xdrs, REAL(obj) + done + cnt))
		    error(_("XDR read failed"));
	    xdr_destroy(&xdrs);
	}
	break;
    }
    case R_pstream_binary_format:
    {
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, REAL(obj) + done,
			    (int)(sizeof(double) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    REAL(obj)[cnt] = InReal(stream);
    }
}

static R_INLINE void
InComplexVec(R_inpstream_t stream, SEXP obj, R_xlen_t length)
{
    switch (stream->type) {
    case R_pstream_xdr_format:
    {
	static char buf[CHUNK_SIZE * sizeof(Rcomplex)];
	R_xlen_t done, this;
	XDR xdrs;
	Rcomplex *output = COMPLEX(obj);
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, buf, (int)(sizeof(Rcomplex) * this));
	    xdrmem_create(&xdrs, buf, (int)(this * sizeof(Rcomplex)), XDR_DECODE);
	    for(R_xlen_t cnt = 0; cnt < this; cnt++) {
		if(!xdr_double(&xdrs, &(output[done+cnt].r)) ||
		   !xdr_double(&xdrs, &(output[done+cnt].i)))
		    error(_("XDR read failed"));
	    }
	    xdr_destroy(&xdrs);
	}
	break;
    }
    case R_pstream_binary_format:
    {
	R_xlen_t done, this;
	for (done = 0; done < length; done += this) {
	    this = min2(CHUNK_SIZE, length - done);
	    stream->InBytes(stream, COMPLEX(obj) + done,
			    (int)(sizeof(Rcomplex) * this));
	}
	break;
    }
    default:
	for (R_xlen_t cnt = 0; cnt < length; cnt++)
	    COMPLEX(obj)[cnt] = InComplex(stream);
    }
}

static int TryConvertString(void *obj, const char *inp, size_t inplen,
                            char *buf, size_t *bufleft)
{
    if (Riconv(obj, NULL, NULL, &buf, bufleft) == -1)
	return -1;
    return (int) Riconv(obj, &inp, &inplen, &buf, bufleft);
}

static SEXP
ConvertChar(void *obj, char *inp, size_t inplen, cetype_t enc)
{
    size_t buflen = inplen;

    for(;;) {
	size_t bufleft = buflen;
	if (buflen < 1000) {
	    char buf[buflen + 1];
	    if (TryConvertString(obj, inp, inplen, buf, &bufleft) == -1) {
		if (errno == E2BIG) {
		    buflen *= 2;
		    continue;
		} else
		    return R_NilValue;
	    }
	    return mkCharLenCE(buf, (int)(buflen - bufleft), enc);
	} else {
	    char *buf = CallocCharBuf(buflen);
	    if (TryConvertString(obj, inp, inplen, buf, &bufleft) == -1) {
		Free(buf);
		if (errno == E2BIG) {
		    buflen *= 2;
		    continue;
		} else
		    return R_NilValue;
	    }
	    SEXP ans = mkCharLenCE(buf, (int)(buflen - bufleft), enc);
	    Free(buf);
	    return ans;
	}
    }
}

static char *native_fromcode(R_inpstream_t stream)
{
    char *from = stream->native_encoding;
#ifdef HAVE_ICONV_CP1252
    if (!strcmp(from, "ISO-8859-1"))
	from = "CP1252";
#endif
    return from;
}

/* Read string into pre-allocated buffer, convert encoding if necessary, and
   return a CHARSXP */
static SEXP
ReadChar(R_inpstream_t stream, char *buf, int length, int levs)
{ 
    InString(stream, buf, length);
    buf[length] = '\0';
    if (levs & UTF8_MASK)
	return mkCharLenCE(buf, length, CE_UTF8);
    if (levs & LATIN1_MASK)
	return mkCharLenCE(buf, length, CE_LATIN1);
    if (levs & BYTES_MASK)
	return mkCharLenCE(buf, length, CE_BYTES);
    if (levs & ASCII_MASK)
	return mkCharLenCE(buf, length, CE_NATIVE);

    /* native encoding, not ascii */
    if (!stream->native_encoding[0] || /* original native encoding unknown */
        (stream->nat2nat_obj == (void *)-1 && /* translation impossible or disabled */
         stream->nat2utf8_obj == (void *)-1))
	return mkCharLenCE(buf, length, CE_NATIVE);
    /* try converting to native encoding */
    if (!stream->nat2nat_obj &&
        !strcmp(stream->native_encoding, R_nativeEncoding())) {
	/* No translation needed. Performance optimization but also leaves
	   invalid strings in their encoding undetected. */
	stream->nat2nat_obj = (void *)-1;
	stream->nat2utf8_obj = (void *)-1;
#ifdef WARN_DESERIALIZE_INVALID_UTF8
	if (known_to_be_utf8 && !utf8Valid(buf))
	    warning(_("deserializing invalid UTF-8 string '%s'"), buf);
#endif
    }
    if (!stream->nat2nat_obj) {
	char *from = native_fromcode(stream);
	stream->nat2nat_obj = Riconv_open("", from);
	if (stream->nat2nat_obj == (void *)-1)
	    warning(_("unsupported conversion from '%s' to '%s'"), from, "");
    }
    if (stream->nat2nat_obj != (void *)-1) {
	cetype_t enc = CE_NATIVE;
	if (known_to_be_utf8) enc = CE_UTF8;
	else if (known_to_be_latin1) enc = CE_LATIN1;
	SEXP ans = ConvertChar(stream->nat2nat_obj, buf, length, enc);
	if (ans != R_NilValue)
	    return ans;
	if (known_to_be_utf8) {
	    /* nat2nat_obj is converting to UTF-8, no need to use nat2utf8_obj */
	    stream->nat2utf8_obj = (void *)-1;
	    char *from = native_fromcode(stream);
	    warning(_("input string '%s' cannot be translated to UTF-8, is it valid in '%s'?"),
	            buf, from);
	}
    }
    /* try converting to UTF-8 */
    if (!stream->nat2utf8_obj) {
	char *from = native_fromcode(stream);
	stream->nat2utf8_obj = Riconv_open("UTF-8", from);
	if (stream->nat2utf8_obj == (void *)-1) {
	    /* very unlikely */
	    warning(_("unsupported conversion from '%s' to '%s'"),
	            from, "UTF-8");
	    warning(_("strings not representable in native encoding will not be translated"));
	} else
	    warning(_("strings not representable in native encoding will be translated to UTF-8"));	
    }
    if (stream->nat2utf8_obj != (void *)-1) {
	SEXP ans = ConvertChar(stream->nat2utf8_obj, buf, length, CE_UTF8);
	if (ans != R_NilValue)
	    return ans;
	char *from = native_fromcode(stream);
	warning(_("input string '%s' cannot be translated to UTF-8, is it valid in '%s' ?"),
	        buf, from);
    }
    /* no translation possible */
    return mkCharLenCE(buf, length, CE_NATIVE); 
}

static R_xlen_t ReadLENGTH (R_inpstream_t stream)
{
    int len = InInteger(stream);
#ifdef LONG_VECTOR_SUPPORT
    if (len < -1)
	error(_("negative serialized length for vector"));
    if (len == -1) {
	unsigned int len1, len2;
	len1 = InInteger(stream); /* upper part */
	len2 = InInteger(stream); /* lower part */
	R_xlen_t xlen = len1;
	/* sanity check for now */
	if (len1 > 65536)
	    error (_("invalid upper part of serialized vector length"));
	return (xlen << 32) + len2;
    } else return len;
#else
    if (len < 0)
	error(_("negative serialized vector length:\nperhaps long vector from 64-bit version of R?"));
    return len;
#endif
}

/* differs when it fails from version in envir.c */
static SEXP R_FindNamespace1(SEXP info)
{
    SEXP expr, val, where;
    PROTECT(info);
    where = PROTECT(ScalarString(mkChar(lastname)));
    SEXP s_getNamespace = install("..getNamespace");
    PROTECT(expr = LCONS(s_getNamespace,
			 LCONS(info, LCONS(where, R_NilValue))));
    val = eval(expr, R_GlobalEnv);
    UNPROTECT(3);
    return val;
}


static SEXP ReadItem (SEXP ref_table, R_inpstream_t stream)
{
    SEXPTYPE type;
    SEXP s;
    R_xlen_t len, count;
    int flags, levs, objf, hasattr, hastag, length;

    R_assert(TYPEOF(ref_table) == LISTSXP && TYPEOF(CAR(ref_table)) == VECSXP);

    flags = InInteger(stream);
    UnpackFlags(flags, &type, &levs, &objf, &hasattr, &hastag);

    switch(type) {
    case NILVALUE_SXP:      return R_NilValue;
    case EMPTYENV_SXP:	    return R_EmptyEnv;
    case BASEENV_SXP:	    return R_BaseEnv;
    case GLOBALENV_SXP:     return R_GlobalEnv;
    case UNBOUNDVALUE_SXP:  return R_UnboundValue;
    case MISSINGARG_SXP:    return R_MissingArg;
    case BASENAMESPACE_SXP:
	return R_BaseNamespace;
    case REFSXP:
	return GetReadRef(ref_table, InRefIndex(stream, flags));
    case PERSISTSXP:
	PROTECT(s = InStringVec(stream, ref_table));
	s = PersistentRestore(stream, s);
	UNPROTECT(1);
	AddReadRef(ref_table, s);
	return s;
    case ALTREP_SXP:
	{
	    R_ReadItemDepth++;
	    SEXP info = PROTECT(ReadItem(ref_table, stream));
	    SEXP state = PROTECT(ReadItem(ref_table, stream));
	    SEXP attr = PROTECT(ReadItem(ref_table, stream));
	    s = ALTREP_UNSERIALIZE_EX(info, state, attr, objf, levs);
	    UNPROTECT(3); /* info, state, attr */
	    R_ReadItemDepth--;
	    return s;
	}
    case SYMSXP:
	R_ReadItemDepth++;
	PROTECT(s = ReadItem(ref_table, stream)); /* print name */
	R_ReadItemDepth--;
	s = installTrChar(s);
	AddReadRef(ref_table, s);
	UNPROTECT(1);
	return s;
    case PACKAGESXP:
	PROTECT(s = InStringVec(stream, ref_table));
	s = R_FindPackageEnv(s);
	UNPROTECT(1);
	AddReadRef(ref_table, s);
	return s;
    case NAMESPACESXP:
	PROTECT(s = InStringVec(stream, ref_table));
	s = R_FindNamespace1(s);
	AddReadRef(ref_table, s);
	UNPROTECT(1);
	return s;
    case ENVSXP:
	{
	    int locked = InInteger(stream);

	    PROTECT(s = allocSExp(ENVSXP));

	    /* MUST register before filling in */
	    AddReadRef(ref_table, s);

	    /* Now fill it in  */
	    R_ReadItemDepth++;
	    SET_ENCLOS(s, ReadItem(ref_table, stream));
	    SET_FRAME(s, ReadItem(ref_table, stream));
	    SET_HASHTAB(s, ReadItem(ref_table, stream));
	    SET_ATTRIB(s, ReadItem(ref_table, stream));
	    R_ReadItemDepth--;
	    if (ATTRIB(s) != R_NilValue &&
		getAttrib(s, R_ClassSymbol) != R_NilValue)
		/* We don't write out the object bit for environments,
		   so reconstruct it here if needed. */
		SET_OBJECT(s, 1);
	    R_RestoreHashCount(s);
	    if (locked) R_LockEnvironment(s, FALSE);
	    /* Convert a NULL enclosure to baseenv() */
	    if (ENCLOS(s) == R_NilValue) SET_ENCLOS(s, R_BaseEnv);
	    UNPROTECT(1);
	    return s;
	}
    case LISTSXP:
    case LANGSXP:
    case CLOSXP:
    case PROMSXP:
    case DOTSXP:
	/* This handling of dotted pair objects still uses recursion
	   on the CDR and so will overflow the PROTECT stack for long
	   lists.  The save format does permit using an iterative
	   approach; it just has to pass around the place to write the
	   CDR into when it is allocated.  It's more trouble than it
	   is worth to write the code to handle this now, but if it
	   becomes necessary we can do it without needing to change
	   the save format. */
	PROTECT(s = allocSExp(type));
	SETLEVELS(s, levs);
	SET_OBJECT(s, objf);
	R_ReadItemDepth++;
	SET_ATTRIB(s, hasattr ? ReadItem(ref_table, stream) : R_NilValue);
	SET_TAG(s, hastag ? ReadItem(ref_table, stream) : R_NilValue);
	if (hastag && R_ReadItemDepth == R_InitReadItemDepth + 1 &&
	    isSymbol(TAG(s))) {
	    snprintf(lastname, 8192, "%s", CHAR(PRINTNAME(TAG(s))));
	}
	if (hastag && R_ReadItemDepth <= 0) {
	    Rprintf("%*s", 2*(R_ReadItemDepth - R_InitReadItemDepth), "");
	    PrintValue(TAG(s));
	}
	SETCAR(s, ReadItem(ref_table, stream));
	R_ReadItemDepth--; /* do this early because of the recursion. */
	SETCDR(s, ReadItem(ref_table, stream));
	/* For reading closures and promises stored in earlier versions, convert NULL env to baseenv() */
	if      (type == CLOSXP && CLOENV(s) == R_NilValue) SET_CLOENV(s, R_BaseEnv);
	else if (type == PROMSXP && PRENV(s) == R_NilValue) SET_PRENV(s, R_BaseEnv);
	UNPROTECT(1); /* s */
	return s;
    default:
	/* These break out of the switch to have their ATTR,
	   LEVELS, and OBJECT fields filled in.  Each leaves the
	   newly allocated value PROTECTed */
	switch (type) {
	case EXTPTRSXP:
	    PROTECT(s = allocSExp(type));
	    AddReadRef(ref_table, s);
	    R_SetExternalPtrAddr(s, NULL);
	    R_ReadItemDepth++;
	    R_SetExternalPtrProtected(s, ReadItem(ref_table, stream));
	    R_SetExternalPtrTag(s, ReadItem(ref_table, stream));
	    R_ReadItemDepth--;
	    break;
	case WEAKREFSXP:
	    PROTECT(s = R_MakeWeakRef(R_NilValue, R_NilValue, R_NilValue,
				      FALSE));
	    AddReadRef(ref_table, s);
	    break;
	case SPECIALSXP:
	case BUILTINSXP:
	    {
		/* These are all short strings */
		length = InInteger(stream);
		char cbuf[length+1];
		InString(stream, cbuf, length);
		cbuf[length] = '\0';
		int index = StrToInternal(cbuf);
		if (index == NA_INTEGER) {
		    warning(_("unrecognized internal function name \"%s\""), cbuf);
		    PROTECT(s = R_NilValue);
		} else
		    PROTECT(s = mkPRIMSXP(index, type == BUILTINSXP));
	    }
	    break;
	case CHARSXP:
	    /* Let us suppose these will still be limited to 2^31 -1 bytes */
	    length = InInteger(stream);
	    if (length == -1)
		PROTECT(s = NA_STRING);
	    else if (length < 1000) {
		char cbuf[length+1];
		PROTECT(s = ReadChar(stream, cbuf, length, levs));
	    } else {
		char *cbuf = CallocCharBuf(length);
		PROTECT(s = ReadChar(stream, cbuf, length, levs));
		Free(cbuf);
	    }
	    break;
	case LGLSXP:
	case INTSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    InIntegerVec(stream, s, len);
	    break;
	case REALSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    InRealVec(stream, s, len);
	    break;
	case CPLXSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    InComplexVec(stream, s, len);
	    break;
	case STRSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    R_ReadItemDepth++;
	    for (count = 0; count < len; ++count)
		SET_STRING_ELT(s, count, ReadItem(ref_table, stream));
	    R_ReadItemDepth--;
	    break;
	case VECSXP:
	case EXPRSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    R_ReadItemDepth++;
	    for (count = 0; count < len; ++count) {
		if (R_ReadItemDepth <= 0)
		    Rprintf("%*s[%d]\n", 2*(R_ReadItemDepth - R_InitReadItemDepth), "", count+1);
		SET_VECTOR_ELT(s, count, ReadItem(ref_table, stream));
	    }
	    R_ReadItemDepth--;
	    break;
	case BCODESXP:
	    PROTECT(s = ReadBC(ref_table, stream));
	    break;
	case CLASSREFSXP:
	    error(_("this version of R cannot read class references"));
	case GENERICREFSXP:
	    error(_("this version of R cannot read generic function references"));
	case RAWSXP:
	    len = ReadLENGTH(stream);
	    PROTECT(s = allocVector(type, len));
	    switch (stream->type) {
	    case R_pstream_ascii_format:
		for (R_xlen_t ix = 0; ix < len; ix++) {
		    char word[128];
		    unsigned int i; // unsigned to avoid compiler warnings
		    InWord(stream, word, sizeof(word));
		    if(sscanf(word, "%2x", &i) != 1) error(_("read error"));
		    RAW(s)[ix] = (Rbyte) i;
		}
		break;
	    default:
	    {
		R_xlen_t done, this;
		for (done = 0; done < len; done += this) {
		    this = min2(CHUNK_SIZE, len - done);
		    stream->InBytes(stream, RAW(s) + done, (int) this);
		}
	    }
	    }
	    break;
	case S4SXP:
	    PROTECT(s = allocS4Object());
	    break;
	default:
	    s = R_NilValue; /* keep compiler happy */
	    error(_("ReadItem: unknown type %i, perhaps written by later version of R"), type);
	}
	if (type != CHARSXP) SETLEVELS(s, levs);
	SET_OBJECT(s, objf);
	if (TYPEOF(s) == CHARSXP) {
	    /* With the CHARSXP cache maintained through the ATTRIB
	       field that field has already been filled in by the
	       mkChar/mkCharCE call above, so we need to leave it
	       alone.  If there is an attribute (as there might be if
	       the serialized data was created by an older version) we
	       read and ignore the value. */
	    R_ReadItemDepth++;
	    if (hasattr) ReadItem(ref_table, stream);
	    R_ReadItemDepth--;
	}
	else {
	    R_ReadItemDepth++;
	    SET_ATTRIB(s, hasattr ? ReadItem(ref_table, stream) : R_NilValue);
	    R_ReadItemDepth--;
	}
	UNPROTECT(1); /* s */
	if (TYPEOF(s) == BCODESXP && !R_BCVersionOK(s))
	    return R_BytecodeExpr(s);
	return s;
    }
}

static SEXP ReadBC1(SEXP ref_table, SEXP reps, R_inpstream_t stream);

static SEXP ReadBCLang(int type, SEXP ref_table, SEXP reps,
		       R_inpstream_t stream)
{
    switch (type) {
    case BCREPREF:
	return VECTOR_ELT(reps, InInteger(stream));
    case BCREPDEF:
    case LANGSXP:
    case LISTSXP:
    case ATTRLANGSXP:
    case ATTRLISTSXP:
	{
	    SEXP ans;
	    int pos = -1;
	    int hasattr = FALSE;
	    if (type == BCREPDEF) {
		pos = InInteger(stream);
		type = InInteger(stream);
	    }
	    switch (type) {
	    case ATTRLANGSXP: type = LANGSXP; hasattr = TRUE; break;
	    case ATTRLISTSXP: type = LISTSXP; hasattr = TRUE; break;
	    }
	    PROTECT(ans = allocSExp(type));
	    if (pos >= 0)
		SET_VECTOR_ELT(reps, pos, ans);
	    R_ReadItemDepth++;
	    if (hasattr)
		SET_ATTRIB(ans, ReadItem(ref_table, stream));
	    SET_TAG(ans, ReadItem(ref_table, stream));
	    R_ReadItemDepth--;
	    SETCAR(ans, ReadBCLang(InInteger(stream), ref_table, reps,
				   stream));
	    SETCDR(ans, ReadBCLang(InInteger(stream), ref_table, reps,
				   stream));
	    UNPROTECT(1);
	    return ans;
	}
    default:
	{
	    R_ReadItemDepth++;
	    SEXP res = ReadItem(ref_table, stream);
	    R_ReadItemDepth--;
	    return res;
	}
    }
}

static SEXP ReadBCConsts(SEXP ref_table, SEXP reps, R_inpstream_t stream)
{
    SEXP ans, c;
    int i, n;
    n = InInteger(stream);
    PROTECT(ans = allocVector(VECSXP, n));
    for (i = 0; i < n; i++) {
	int type = InInteger(stream);
	switch (type) {
	case BCODESXP:
	    c = ReadBC1(ref_table, reps, stream);
	    SET_VECTOR_ELT(ans, i, c);
	    break;
	case LANGSXP:
	case LISTSXP:
	case BCREPDEF:
	case BCREPREF:
	case ATTRLANGSXP:
	case ATTRLISTSXP:
	    c = ReadBCLang(type, ref_table, reps, stream);
	    SET_VECTOR_ELT(ans, i, c);
	    break;
	default:
	    R_ReadItemDepth++;
	    SET_VECTOR_ELT(ans, i, ReadItem(ref_table, stream));
	    R_ReadItemDepth--;
	}
    }
    UNPROTECT(1);
    return ans;
}

static SEXP ReadBC1(SEXP ref_table, SEXP reps, R_inpstream_t stream)
{
    SEXP s;
    PROTECT(s = allocSExp(BCODESXP));
    R_ReadItemDepth++;
    SETCAR(s, ReadItem(ref_table, stream)); /* code */
    R_ReadItemDepth--;
    SEXP bytes = PROTECT(CAR(s));
    SETCAR(s, R_bcEncode(bytes));
    SETCDR(s, ReadBCConsts(ref_table, reps, stream)); /* consts */
    SET_TAG(s, R_NilValue); /* expr */
    R_registerBC(bytes, s);
    UNPROTECT(2);
    return s;
}

static SEXP ReadBC(SEXP ref_table, R_inpstream_t stream)
{
    SEXP reps, ans;
    PROTECT(reps = allocVector(VECSXP, InInteger(stream)));
    ans = ReadBC1(ref_table, reps, stream);
    UNPROTECT(1);
    return ans;
}

static void DecodeVersion(int packed, int *v, int *p, int *s)
{
    *v = packed / 65536; packed = packed % 65536;
    *p = packed / 256; packed = packed % 256;
    *s = packed;
}

SEXP R_Unserialize(R_inpstream_t stream)
{
    int version;
    int writer_version, min_reader_version;
    SEXP obj, ref_table;

    InFormat(stream);

    /* Read the version numbers */
    version = InInteger(stream);
    writer_version = InInteger(stream);
    min_reader_version = InInteger(stream); 
    switch (version) {
    case 2: break;
    case 3:
    {
	int nelen = InInteger(stream);
	if (nelen > R_CODESET_MAX)
	    error(_("invalid length of encoding name"));
	InString(stream, stream->native_encoding, nelen);
	stream->native_encoding[nelen] = '\0';
	break;
    }
    default:
	{
	    int vw, pw, sw;
	    DecodeVersion(writer_version, &vw, &pw, &sw);
	    if (min_reader_version < 0)
		error(_("cannot read unreleased workspace version %d written by experimental R %d.%d.%d"), version, vw, pw, sw);
	    else {
		int vm, pm, sm;
		DecodeVersion(min_reader_version, &vm, &pm, &sm);
		error(_("cannot read workspace version %d written by R %d.%d.%d; need R %d.%d.%d or newer"),
		      version, vw, pw, sw, vm, pm, sm);
	    }
	}
    }

    /* Read the actual object back */
    PROTECT(ref_table = MakeReadRefTable());
    obj =  ReadItem(ref_table, stream);

    if (version == 3) {
	if (stream->nat2nat_obj && stream->nat2nat_obj != (void *)-1) {
	    Riconv_close(stream->nat2nat_obj);
	    stream->nat2nat_obj = NULL;
	}
	if (stream->nat2utf8_obj && stream->nat2utf8_obj != (void *)-1) {
	    Riconv_close(stream->nat2utf8_obj);
	    stream->nat2utf8_obj = NULL;
	}
    }
    UNPROTECT(1);

    return obj;
}

SEXP R_SerializeInfo(R_inpstream_t stream)
{
    int version;
    int writer_version, min_reader_version, vv, vp, vs;
    int anslen = 4;
    SEXP ans, names;
    char buf[128];

    InFormat(stream);

    /* Read the version numbers */
    version = InInteger(stream);
    if (version == 3)
	anslen++;
    writer_version = InInteger(stream);
    min_reader_version = InInteger(stream);

    PROTECT(ans = allocVector(VECSXP, anslen));
    PROTECT(names = allocVector(STRSXP, anslen));
    SET_STRING_ELT(names, 0, mkChar("version"));
    SET_VECTOR_ELT(ans, 0, ScalarInteger(version));
    SET_STRING_ELT(names, 1, mkChar("writer_version"));
    DecodeVersion(writer_version, &vv, &vp, &vs);
    snprintf(buf, 128, "%d.%d.%d", vv, vp, vs); 
    SET_VECTOR_ELT(ans, 1, mkString(buf));
    SET_STRING_ELT(names, 2, mkChar("min_reader_version"));
    if (min_reader_version < 0)
	/* unreleased version of R */
	SET_VECTOR_ELT(ans, 2, ScalarString(NA_STRING));
    else { 
	DecodeVersion(min_reader_version, &vv, &vp, &vs);
	snprintf(buf, 128, "%d.%d.%d", vv, vp, vs);
	SET_VECTOR_ELT(ans, 2, mkString(buf));
    }
    SET_STRING_ELT(names, 3, mkChar("format"));
    switch(stream->type) {
    case R_pstream_ascii_format:
	SET_VECTOR_ELT(ans, 3, mkString("ascii"));
	break;
    case R_pstream_binary_format:
	SET_VECTOR_ELT(ans, 3, mkString("binary"));
	break;
    case R_pstream_xdr_format:
	SET_VECTOR_ELT(ans, 3, mkString("xdr"));
	break;
    default:
	error(_("unknown input format"));
    }
    if (version == 3) {
	SET_STRING_ELT(names, 4, mkChar("native_encoding"));
	int nelen = InInteger(stream);
	if (nelen > R_CODESET_MAX)
	    error(_("invalid length of encoding name"));
	char nbuf[nelen + 1];
	InString(stream, nbuf, nelen);
	nbuf[nelen] = '\0';
	SET_VECTOR_ELT(ans, 4, mkString(nbuf));
    }
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2); /* ans, names */

    return ans;
}

/*
 * Generic Persistent Stream Initializers
 */

void
R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		R_pstream_format_t type,
		int (*inchar)(R_inpstream_t),
		void (*inbytes)(R_inpstream_t, void *, int),
		SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    stream->data = data;
    stream->type = type;
    stream->InChar = inchar;
    stream->InBytes = inbytes;
    stream->InPersistHookFunc = phook;
    stream->InPersistHookData = pdata;
    stream->native_encoding[0] = 0;
    stream->nat2nat_obj = NULL;
    stream->nat2utf8_obj = NULL; 
}

void
R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		 R_pstream_format_t type, int version,
		 void (*outchar)(R_outpstream_t, int),
		 void (*outbytes)(R_outpstream_t, void *, int),
		 SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    stream->data = data;
    stream->type = type;
    stream->version = version != 0 ? version : defaultSerializeVersion();
    stream->OutChar = outchar;
    stream->OutBytes = outbytes;
    stream->OutPersistHookFunc = phook;
    stream->OutPersistHookData = pdata;
}


/*
 * Persistent File Streams
 */

static void OutCharFile(R_outpstream_t stream, int c)
{
    FILE *fp = stream->data;
    fputc(c, fp);
}


static int InCharFile(R_inpstream_t stream)
{
    FILE *fp = stream->data;
    return fgetc(fp);
}

static void OutBytesFile(R_outpstream_t stream, void *buf, int length)
{
    FILE *fp = stream->data;
    size_t out = fwrite(buf, 1, length, fp);
    if (out != length) error(_("write failed"));
}

static void InBytesFile(R_inpstream_t stream, void *buf, int length)
{
    FILE *fp = stream->data;
    size_t in = fread(buf, 1, length, fp);
    if (in != length) error(_("read failed"));
}

void
R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    R_InitOutPStream(stream, (R_pstream_data_t) fp, type, version,
		     OutCharFile, OutBytesFile, phook, pdata);
}

void
R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    R_InitInPStream(stream, (R_pstream_data_t) fp, type,
		    InCharFile, InBytesFile, phook, pdata);
}


/*
 * Persistent Connection Streams
 */

#include <Rconnections.h>

static void CheckInConn(Rconnection con)
{
    if (! con->isopen)
	error(_("connection is not open"));
    if (! con->canread || con->read == NULL)
	error(_("cannot read from this connection"));
}

static void CheckOutConn(Rconnection con)
{
    if (! con->isopen)
	error(_("connection is not open"));
    if (! con->canwrite || con->write == NULL)
	error(_("cannot write to this connection"));
}

static void InBytesConn(R_inpstream_t stream, void *buf, int length)
{
    Rconnection con = (Rconnection) stream->data;
    CheckInConn(con);
    if (con->text) {
	int i;
	char *p = buf;
	for (i = 0; i < length; i++)
	    p[i] = (char) Rconn_fgetc(con);
    }
    else {
	if (stream->type == R_pstream_ascii_format) {
	    char linebuf[4];
	    unsigned char *p = buf;
	    int i;
	    unsigned int res;
	    for (i = 0; i < length; i++) {
		size_t ncread = Rconn_getline(con, linebuf, 3);
		if (ncread != 2)
		    error(_("error reading from ascii connection"));
		if (!sscanf(linebuf, "%02x", &res))
		    error(_("unexpected format in ascii connection"));
		*p++ = (unsigned char)res;
	    }
	} else {
	    if (length != con->read(buf, 1, length, con))
		error(_("error reading from connection"));
	}
    }
}

static int InCharConn(R_inpstream_t stream)
{
    char buf[1];
    Rconnection con = (Rconnection) stream->data;
    CheckInConn(con);
    if (con->text)
	return Rconn_fgetc(con);
    else {
	if (1 != con->read(buf, 1, 1, con))
	    error(_("error reading from connection"));
	return buf[0];
    }
}

static void OutBytesConn(R_outpstream_t stream, void *buf, int length)
{
    Rconnection con = (Rconnection) stream->data;
    CheckOutConn(con);
    if (con->text) {
	int i;
	char *p = buf;
	for (i = 0; i < length; i++)
	    Rconn_printf(con, "%c", p[i]);
    }
    else {
	if (length != con->write(buf, 1, length, con))
	    error(_("error writing to connection"));
    }
}

static void OutCharConn(R_outpstream_t stream, int c)
{
    Rconnection con = (Rconnection) stream->data;
    CheckOutConn(con);
    if (con->text)
	Rconn_printf(con, "%c", c);
    else {
	char buf[1];
	buf[0] = (char) c;
	if (1 != con->write(buf, 1, 1, con))
	    error(_("error writing to connection"));
    }
}

void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    CheckOutConn(con);
    if (con->text &&
	!(type == R_pstream_ascii_format || type == R_pstream_asciihex_format) )
	error(_("only ascii format can be written to text mode connections"));
    R_InitOutPStream(stream, (R_pstream_data_t) con, type, version,
		     OutCharConn, OutBytesConn, phook, pdata);
}

void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    CheckInConn(con);
    if (con->text) {
	if (type == R_pstream_any_format)
	    type = R_pstream_ascii_format;
	else if (type != R_pstream_ascii_format)
	    error(_("only ascii format can be read from text mode connections"));
    }
    R_InitInPStream(stream, (R_pstream_data_t) con, type,
		    InCharConn, InBytesConn, phook, pdata);
}

/* ought to quote the argument, but it should only be an ENVSXP or STRSXP */
static SEXP CallHook(SEXP x, SEXP fun)
{
    SEXP val, call;
    PROTECT(call = LCONS(fun, LCONS(x, R_NilValue)));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return val;
}

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

/* Used from saveRDS().
   This became public in R 2.13.0, and that version added support for
   connections internally */
SEXP attribute_hidden
do_serializeToConn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* serializeToConn(object, conn, ascii, version, hook) */

    SEXP object, fun;
    Rboolean ascii, wasopen;
    int version;
    Rconnection con;
    struct R_outpstream_st out;
    R_pstream_format_t type;
    SEXP (*hook)(SEXP, SEXP);
    RCNTXT cntxt;

    checkArity(op, args);

    object = CAR(args);
    con = getConnection(asInteger(CADR(args)));

    if (TYPEOF(CADDR(args)) != LGLSXP)
	error(_("'ascii' must be logical"));
    ascii = INTEGER(CADDR(args))[0];
    if (ascii == NA_LOGICAL) type = R_pstream_asciihex_format;
    else if (ascii) type = R_pstream_ascii_format;
    else type = R_pstream_xdr_format;

    if (CADDDR(args) == R_NilValue)
	version = defaultSerializeVersion();
    else
	version = asInteger(CADDDR(args));
    if (version == NA_INTEGER || version <= 0)
	error(_("bad version value"));
    if (version < 2)
	error(_("cannot save to connections in version %d format"), version);

    fun = CAR(nthcdr(args,4));
    hook = fun != R_NilValue ? CallHook : NULL;

    /* Now we need to do some sanity checking of the arguments.
       A filename will already have been opened, so anything
       not open was specified as a connection directly.
     */
    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, ascii ? "w" : "wb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if (!ascii && con->text)
	error(_("binary-mode connection required for ascii=FALSE"));
    if(!con->canwrite)
	error(_("connection not open for writing"));

    R_InitConnOutPStream(&out, con, type, version, hook, fun);
    R_Serialize(object, &out);
    if(!wasopen) {endcontext(&cntxt); con->close(con);}

    return R_NilValue;
}

/* unserializeFromConn(conn, hook) used from readRDS().
   It became public in R 2.13.0, and that version added support for
   connections internally */
SEXP attribute_hidden
do_unserializeFromConn(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /* 0 .. unserializeFromConn(conn, hook) */
    /* 1 .. serializeInfoFromConn(conn) */

    struct R_inpstream_st in;
    Rconnection con;
    SEXP fun, ans;
    SEXP (*hook)(SEXP, SEXP);
    Rboolean wasopen;
    RCNTXT cntxt;

    checkArity(op, args);

    con = getConnection(asInteger(CAR(args)));

    /* Now we need to do some sanity checking of the arguments.
       A filename will already have been opened, so anything
       not open was specified as a connection directly.
     */
    wasopen = con->isopen;
    if(!wasopen) {
	char mode[5];
	strcpy(mode, con->mode);
	strcpy(con->mode, "rb");
	if(!con->open(con)) error(_("cannot open the connection"));
	strcpy(con->mode, mode);
	/* Set up a context which will close the connection on error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &con_cleanup;
	cntxt.cenddata = con;
    }
    if(!con->canread) error(_("connection not open for reading"));

    fun = PRIMVAL(op) == 0 ? CADR(args) : R_NilValue;
    hook = fun != R_NilValue ? CallHook : NULL;
    R_InitConnInPStream(&in, con, R_pstream_any_format, hook, fun);
    ans = PRIMVAL(op) == 0 ? R_Unserialize(&in) : R_SerializeInfo(&in);    
    if(!wasopen) {
	PROTECT(ans); /* paranoia about next line */
	endcontext(&cntxt);
	con->close(con);
	UNPROTECT(1);
    }
    return ans;
}

/*
 * Persistent Buffered Binary Connection Streams
 */

/**** should eventually come from a public header file */
size_t R_WriteConnection(Rconnection con, void *buf, size_t n);

#define BCONBUFSIZ 4096

typedef struct bconbuf_st {
    Rconnection con;
    int count;
    unsigned char buf[BCONBUFSIZ];
} *bconbuf_t;

static void flush_bcon_buffer(bconbuf_t bb)
{
    if (R_WriteConnection(bb->con, bb->buf, bb->count) != bb->count)
	error(_("error writing to connection"));
    bb->count = 0;
}

static void OutCharBB(R_outpstream_t stream, int c)
{
    bconbuf_t bb = stream->data;
    if (bb->count >= BCONBUFSIZ)
	flush_bcon_buffer(bb);
    bb->buf[bb->count++] = (char) c;
}

static void OutBytesBB(R_outpstream_t stream, void *buf, int length)
{
    bconbuf_t bb = stream->data;
    if (bb->count + length > BCONBUFSIZ)
	flush_bcon_buffer(bb);
    if (length <= BCONBUFSIZ) {
	memcpy(bb->buf + bb->count, buf, length);
	bb->count += length;
    }
    else if (R_WriteConnection(bb->con, buf, length) != length)
	error(_("error writing to connection"));
}

static void InitBConOutPStream(R_outpstream_t stream, bconbuf_t bb,
			       Rconnection con,
			       R_pstream_format_t type, int version,
			       SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    bb->count = 0;
    bb->con = con;
    R_InitOutPStream(stream, (R_pstream_data_t) bb, type, version,
		     OutCharBB, OutBytesBB, phook, pdata);
}

/* only for use by serialize(), with binary write to a socket connection */
static SEXP
R_serializeb(SEXP object, SEXP icon, SEXP xdr, SEXP Sversion, SEXP fun)
{
    struct R_outpstream_st out;
    SEXP (*hook)(SEXP, SEXP);
    struct bconbuf_st bbs;
    Rconnection con = getConnection(asInteger(icon));
    int version;

    if (Sversion == R_NilValue)
	version = defaultSerializeVersion();
    else version = asInteger(Sversion);
    if (version == NA_INTEGER || version <= 0)
	error(_("bad version value"));

    hook = fun != R_NilValue ? CallHook : NULL;

    InitBConOutPStream(&out, &bbs, con,
		       asLogical(xdr) ? R_pstream_xdr_format : R_pstream_binary_format,
		       version, hook, fun);
    R_Serialize(object, &out);
    flush_bcon_buffer(&bbs);
    return R_NilValue;
}


/*
 * Persistent Memory Streams
 */

typedef struct membuf_st {
    R_size_t size;
    R_size_t count;
    unsigned char *buf;
} *membuf_t;


#define INCR MAXELTSIZE
static void resize_buffer(membuf_t mb, R_size_t needed)
{
    if(needed > R_XLEN_T_MAX)
	error(_("serialization is too large to store in a raw vector"));
#ifdef LONG_VECTOR_SUPPORT
    if(needed < 10000000) /* ca 10MB */
	needed = (1+2*needed/INCR) * INCR;
    else
	needed = (R_size_t)((1+1.2*(double)needed/INCR) * INCR);
#else
    if(needed < 10000000) /* ca 10MB */
	needed = (1+2*needed/INCR) * INCR;
    else if(needed < 1700000000) /* close to 2GB/1.2 */
	needed = (R_size_t)((1+1.2*(double)needed/INCR) * INCR);
    else if(needed < INT_MAX - INCR)
	needed = (1+needed/INCR) * INCR;
#endif
    unsigned char *tmp = realloc(mb->buf, needed);
    if (tmp == NULL) {
	free(mb->buf); mb->buf = NULL;
	error(_("cannot allocate buffer"));
    } else mb->buf = tmp;
    mb->size = needed;
}

static void OutCharMem(R_outpstream_t stream, int c)
{
    membuf_t mb = stream->data;
    if (mb->count >= mb->size)
	resize_buffer(mb, mb->count + 1);
    mb->buf[mb->count++] = (char) c;
}

static void OutBytesMem(R_outpstream_t stream, void *buf, int length)
{
    membuf_t mb = stream->data;
    R_size_t needed = mb->count + (R_size_t) length;
#ifndef LONG_VECTOR_SUPPORT
    /* There is a potential overflow here on 32-bit systems */
    if((double) mb->count + length > (double) INT_MAX)
	error(_("serialization is too large to store in a raw vector"));
#endif
    if (needed > mb->size) resize_buffer(mb, needed);
    memcpy(mb->buf + mb->count, buf, length);
    mb->count = needed;
}

static int InCharMem(R_inpstream_t stream)
{
    membuf_t mb = stream->data;
    if (mb->count >= mb->size)
	error(_("read error"));
    return mb->buf[mb->count++];
}

static void InBytesMem(R_inpstream_t stream, void *buf, int length)
{
    membuf_t mb = stream->data;
    if (mb->count + (R_size_t) length > mb->size)
	error(_("read error"));
    memcpy(buf, mb->buf + mb->count, length);
    mb->count += length;
}

static void InitMemInPStream(R_inpstream_t stream, membuf_t mb,
			     void *buf, R_size_t length,
			     SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    mb->count = 0;
    mb->size = length;
    mb->buf = buf;
    R_InitInPStream(stream, (R_pstream_data_t) mb, R_pstream_any_format,
		    InCharMem, InBytesMem, phook, pdata);
}

static void InitMemOutPStream(R_outpstream_t stream, membuf_t mb,
			      R_pstream_format_t type, int version,
			      SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    mb->count = 0;
    mb->size = 0;
    mb->buf = NULL;
    R_InitOutPStream(stream, (R_pstream_data_t) mb, type, version,
		     OutCharMem, OutBytesMem, phook, pdata);
}

static void free_mem_buffer(void *data)
{
    membuf_t mb = data;
    if (mb->buf != NULL) {
	unsigned char *buf = mb->buf;
	mb->buf = NULL;
	free(buf);
    }
}

static SEXP CloseMemOutPStream(R_outpstream_t stream)
{
    SEXP val;
    membuf_t mb = stream->data;
    /* duplicate check, for future proofing */
#ifndef LONG_VECTOR_SUPPORT
    if(mb->count > INT_MAX)
	error(_("serialization is too large to store in a raw vector"));
#endif
    PROTECT(val = allocVector(RAWSXP, mb->count));
    memcpy(RAW(val), mb->buf, mb->count);
    free_mem_buffer(mb);
    UNPROTECT(1);
    return val;
}

static SEXP
R_serialize(SEXP object, SEXP icon, SEXP ascii, SEXP Sversion, SEXP fun)
{
    struct R_outpstream_st out;
    R_pstream_format_t type;
    SEXP (*hook)(SEXP, SEXP);
    int version;

    if (Sversion == R_NilValue)
	version = defaultSerializeVersion();
    else version = asInteger(Sversion);
    if (version == NA_INTEGER || version <= 0)
	error(_("bad version value"));

    hook = fun != R_NilValue ? CallHook : NULL;

    // Prior to 3.2.0 this was logical, values 0/1/NA for binary.
    int asc = asInteger(ascii);
    switch(asc) {
    case 1: type = R_pstream_ascii_format; break;
    case 2: type = R_pstream_asciihex_format; break;
    case 3: type = R_pstream_binary_format; break;
    default: type = R_pstream_xdr_format; break;
    }

    if (icon == R_NilValue) {
	RCNTXT cntxt;
	struct membuf_st mbs;
	SEXP val;

	/* set up a context which will free the buffer if there is an error */
	begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	cntxt.cend = &free_mem_buffer;
	cntxt.cenddata = &mbs;

	InitMemOutPStream(&out, &mbs, type, version, hook, fun);
	R_Serialize(object, &out);

	PROTECT(val = CloseMemOutPStream(&out));

	/* end the context after anything that could raise an error but before
	   calling OutTerm so it doesn't get called twice */
	endcontext(&cntxt);

	UNPROTECT(1); /* val */
	return val;
    }
    else {
	Rconnection con = getConnection(asInteger(icon));
	R_InitConnOutPStream(&out, con, type, version, hook, fun);
	R_Serialize(object, &out);
	return R_NilValue;
    }
}


SEXP attribute_hidden R_unserialize(SEXP icon, SEXP fun)
{
    struct R_inpstream_st in;
    SEXP (*hook)(SEXP, SEXP);

    hook = fun != R_NilValue ? CallHook : NULL;

    if (TYPEOF(icon) == STRSXP && LENGTH(icon) > 0) {
	/* was the format in R < 2.4.0, removed in R 2.8.0 */
	error("character vectors are no longer accepted by unserialize()");
	return R_NilValue; /* -Wall */
    } else if (TYPEOF(icon) == RAWSXP) {
	/* We might want to read from a long raw vector */
	struct membuf_st mbs;
	void *data = RAW(icon);
	R_size_t length = XLENGTH(icon);
	InitMemInPStream(&in, &mbs, data,  length, hook, fun);
	return R_Unserialize(&in);
    } else {
	Rconnection con = getConnection(asInteger(icon));
	R_InitConnInPStream(&in, con, R_pstream_any_format, hook, fun);
	return R_Unserialize(&in);
    }
}


/*
 * Support Code for Lazy Loading of Packages
 */


#define IS_PROPER_STRING(s) (TYPEOF(s) == STRSXP && LENGTH(s) > 0)

/* Appends a raw vector to the end of a file using binary mode.
   Returns an integer vector of the initial offset of the string in
   the file and the length of the vector. */

static SEXP appendRawToFile(SEXP file, SEXP bytes)
{
    FILE *fp;
    size_t len, out;
    long pos;  // what ftell gives: won't work for > 2GB files
    SEXP val;

    if (! IS_PROPER_STRING(file))
	error(_("not a proper file name"));
    if (TYPEOF(bytes) != RAWSXP)
	error(_("not a proper raw vector"));
#ifdef HAVE_WORKING_FTELL
    /* Windows' ftell returns position 0 with "ab" */
    if ((fp = R_fopen(CHAR(STRING_ELT(file, 0)), "ab")) == NULL) {
	error( _("cannot open file '%s': %s"), CHAR(STRING_ELT(file, 0)),
	       strerror(errno));
    }
#else
    if ((fp = R_fopen(CHAR(STRING_ELT(file, 0)), "r+b")) == NULL) {
	error( _("cannot open file '%s': %s"), CHAR(STRING_ELT(file, 0)),
	       strerror(errno));
    }
    fseek(fp, 0, SEEK_END);
#endif

    len = LENGTH(bytes);
    pos = ftell(fp);
    out = fwrite(RAW(bytes), 1, len, fp);
    fclose(fp);

    if (out != len) error(_("write failed"));
    if (pos == -1) error(_("could not determine file position"));

    val = allocVector(INTSXP, 2);
    INTEGER(val)[0] = (int) pos;
    INTEGER(val)[1] = (int) len;
    return val;
}

/* Interface to cache the pkg.rdb files */

#define NC 100
static int used = 0;
static char names[NC][PATH_MAX];
static char *ptr[NC];

SEXP attribute_hidden
do_lazyLoadDBflush(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    int i;
    const char *cfile = CHAR(STRING_ELT(CAR(args), 0));

    /* fprintf(stderr, "flushing file %s", cfile); */
    for (i = 0; i < used; i++)
	if(strcmp(cfile, names[i]) == 0) {
	    strcpy(names[i], "");
	    free(ptr[i]);
	    /* fprintf(stderr, " found at pos %d in cache", i); */
	    break;
	}
    /* fprintf(stderr, "\n"); */
    return R_NilValue;
}


/* Reads, in binary mode, the bytes in the range specified by a
   position/length vector and returns them as raw vector. */

/* There are some large lazy-data examples, e.g. 80Mb for SNPMaP.cdm */
#define LEN_LIMIT 10*1048576
static SEXP readRawFromFile(SEXP file, SEXP key)
{
    FILE *fp;
    int offset, len, in, i, icache = -1;
    long filelen;
    SEXP val;
    const char *cfile = CHAR(STRING_ELT(file, 0));

    if (! IS_PROPER_STRING(file))
	error(_("not a proper file name"));
    if (TYPEOF(key) != INTSXP || LENGTH(key) != 2)
	error(_("bad offset/length argument"));

    offset = INTEGER(key)[0];
    len = INTEGER(key)[1];

    val = allocVector(RAWSXP, len);
    /* Do we have this database cached? */
    for (i = 0; i < used; i++)
	if(strcmp(cfile, names[i]) == 0) {icache = i; break;}
    if (icache >= 0) {
	memcpy(RAW(val), ptr[icache]+offset, len);
	return val;
    }

    /* find a vacant slot? */
    for (i = 0; i < used; i++)
	if(strcmp("", names[i]) == 0) {icache = i; break;}
    if(icache < 0 && used < NC) icache = used++;

    if(icache >= 0) {
	if ((fp = R_fopen(cfile, "rb")) == NULL)
	    error(_("cannot open file '%s': %s"), cfile, strerror(errno));
	if (fseek(fp, 0, SEEK_END) != 0) {
	    fclose(fp);
	    error(_("seek failed on %s"), cfile);
	}
	filelen = ftell(fp);
	if (filelen < LEN_LIMIT) {
	    char *p;
	    /* fprintf(stderr, "adding file '%s' at pos %d in cache, length %d\n",
	       cfile, icache, filelen); */
	    p = (char *) malloc(filelen);
	    if (p) {
		strcpy(names[icache], cfile);
		ptr[icache] = p;
		if (fseek(fp, 0, SEEK_SET) != 0) {
		    fclose(fp);
		    error(_("seek failed on %s"), cfile);
		}
		in = (int) fread(p, 1, filelen, fp);
		fclose(fp);
		if (filelen != in) error(_("read failed on %s"), cfile);
		memcpy(RAW(val), p+offset, len);
	    } else {
		if (fseek(fp, offset, SEEK_SET) != 0) {
		    fclose(fp);
		    error(_("seek failed on %s"), cfile);
		}
		in = (int) fread(RAW(val), 1, len, fp);
		fclose(fp);
		if (len != in) error(_("read failed on %s"), cfile);
	    }
	    return val;
	} else {
	    if (fseek(fp, offset, SEEK_SET) != 0) {
		fclose(fp);
		error(_("seek failed on %s"), cfile);
	    }
	    in = (int) fread(RAW(val), 1, len, fp);
	    fclose(fp);
	    if (len != in) error(_("read failed on %s"), cfile);
	    return val;
	}
    }

    if ((fp = R_fopen(cfile, "rb")) == NULL)
	error(_("cannot open file '%s': %s"), cfile, strerror(errno));
    if (fseek(fp, offset, SEEK_SET) != 0) {
	fclose(fp);
	error(_("seek failed on %s"), cfile);
    }
    in = (int) fread(RAW(val), 1, len, fp);
    fclose(fp);
    if (len != in) error(_("read failed on %s"), cfile);
    return val;
}

/* Gets the binding values of variables from a frame and returns them
   as a list.  If the force argument is true, promises are forced;
   otherwise they are not. */

static SEXP R_getVarsFromFrame(SEXP vars, SEXP env, SEXP forcesxp)
{
    SEXP val, tmp, sym;
    Rboolean force;
    int i, len;

    if (TYPEOF(env) == NILSXP) {
	error(_("use of NULL environment is defunct"));
	env = R_BaseEnv;
    } else
    if (TYPEOF(env) != ENVSXP)
	error(_("bad environment"));
    if (TYPEOF(vars) != STRSXP)
	error(_("bad variable names"));
    force = asLogical(forcesxp);

    len = LENGTH(vars);
    PROTECT(val = allocVector(VECSXP, len));
    for (i = 0; i < len; i++) {
	sym = installTrChar(STRING_ELT(vars, i));

	tmp = findVarInFrame(env, sym);
	if (tmp == R_UnboundValue) {
/*		PrintValue(env);
		PrintValue(R_GetTraceback(0)); */  /* DJM debugging */
	    error(_("object '%s' not found"), EncodeChar(STRING_ELT(vars, i)));
	    }
	if (force && TYPEOF(tmp) == PROMSXP) {
	    PROTECT(tmp);
	    tmp = eval(tmp, R_GlobalEnv);
	    ENSURE_NAMEDMAX(tmp);
	    UNPROTECT(1);
	}
	else ENSURE_NAMED(tmp); /* should not really be needed - LT */
	SET_VECTOR_ELT(val, i, tmp);
    }
    setAttrib(val, R_NamesSymbol, vars);
    UNPROTECT(1);

    return val;
}

/* from connections.c */
SEXP R_compress1(SEXP in);
SEXP R_decompress1(SEXP in, Rboolean *err);
SEXP R_compress2(SEXP in);
SEXP R_decompress2(SEXP in, Rboolean *err);
SEXP R_compress3(SEXP in);
SEXP R_decompress3(SEXP in, Rboolean *err);

/* Serializes and, optionally, compresses a value and appends the
   result to a file.  Returns the key position/length key for
   retrieving the value */

static SEXP
R_lazyLoadDBinsertValue(SEXP value, SEXP file, SEXP ascii,
			SEXP compsxp, SEXP hook)
{
    PROTECT_INDEX vpi;
    int compress = asInteger(compsxp);
    SEXP key;

    value = R_serialize(value, R_NilValue, ascii, R_NilValue, hook);
    PROTECT_WITH_INDEX(value, &vpi);
    if (compress == 3)
	REPROTECT(value = R_compress3(value), vpi);
    else if (compress == 2)
	REPROTECT(value = R_compress2(value), vpi);
    else if (compress)
	REPROTECT(value = R_compress1(value), vpi);
    key = appendRawToFile(file, value);
    UNPROTECT(1);
    return key;
}

/* Retrieves a sequence of bytes as specified by a position/length key
   from a file, optionally decompresses, and unserializes the bytes.
   If the result is a promise, then the promise is forced. */

SEXP attribute_hidden
do_lazyLoadDBfetch(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP key, file, compsxp, hook;
    PROTECT_INDEX vpi;
    int compressed;
    Rboolean err = FALSE;
    SEXP val;

    checkArity(op, args);
    key = CAR(args); args = CDR(args);
    file = CAR(args); args = CDR(args);
    compsxp = CAR(args); args = CDR(args);
    hook = CAR(args);
    compressed = asInteger(compsxp);

    PROTECT_WITH_INDEX(val = readRawFromFile(file, key), &vpi);
    if (compressed == 3)
	REPROTECT(val = R_decompress3(val, &err), vpi);
    else if (compressed == 2)
	REPROTECT(val = R_decompress2(val, &err), vpi);
    else if (compressed)
	REPROTECT(val = R_decompress1(val, &err), vpi);
    if (err) error("lazy-load database '%s' is corrupt",
		   CHAR(STRING_ELT(file, 0)));
    val = R_unserialize(val, hook);
    if (TYPEOF(val) == PROMSXP) {
	REPROTECT(val, vpi);
	val = eval(val, R_GlobalEnv);
	ENSURE_NAMEDMAX(val);
    }
    UNPROTECT(1);
    return val;
}

SEXP attribute_hidden
do_getVarsFromFrame(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return R_getVarsFromFrame(CAR(args), CADR(args), CADDR(args));
}


SEXP attribute_hidden
do_lazyLoadDBinsertValue(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP value, file, ascii, compsxp, hook;
    value = CAR(args); args = CDR(args);
    file = CAR(args); args = CDR(args);
    ascii = CAR(args); args = CDR(args);
    compsxp = CAR(args); args = CDR(args);
    hook = CAR(args); args = CDR(args);
    return R_lazyLoadDBinsertValue(value, file, ascii, compsxp, hook);
}

SEXP attribute_hidden
do_serialize(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    if (PRIMVAL(op) == 2) return R_unserialize(CAR(args), CADR(args));

    SEXP object, icon, type, ver, fun;
    object = CAR(args); args = CDR(args);
    icon = CAR(args); args = CDR(args);
    type = CAR(args); args = CDR(args);
    ver = CAR(args); args = CDR(args);
    fun = CAR(args);

    if(PRIMVAL(op) == 1)
	return R_serializeb(object, icon, type, ver, fun);
    else
	return R_serialize(object, icon, type, ver, fun);
}
