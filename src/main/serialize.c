/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka and the
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
#include <config.h>
#endif

/* we substitute if XDR is not found */
#ifndef HAVE_XDR
# define HAVE_XDR 1
#endif

#define NEED_CONNECTION_PSTREAMS
#include <Defn.h>
#include <Rmath.h>
#include <Fileio.h>
#include <Rversion.h>

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
 *    provided in a release version, then these should specify the
 *    oldest reader R version as -1.
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

   Environments recognized as package or name space environments are
   not saved directly. Instead, a STRSXP is saved that is then used to
   attempt to find the package/name space when unserialized.  The
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
   all environments other than package environments, name space
   environments, and the global environment).  The hook function
   consists of a function pointer and a data value.  The serialization
   function pointer is called with the reference object and the data
   value as arguments.  It should return R_NilValue for standard
   handling and an STRSXP for special handling.  In an STRSXP is
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

/*
 * Forward Declarations
 */

static void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table);
static void WriteItem (SEXP s, SEXP ref_table, R_outpstream_t stream);
static SEXP ReadItem(SEXP ref_table, R_inpstream_t stream);


/*
 * Constants
 */

/* The default version used when a stream Init function is called with
   version = 0 */
static int R_DefaultSerializeVersion = 2;


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
#define R_assert(e) ((e) ? (void) 0 : error("assertion `%s' failed: file `%s', line %d\n", #e, __FILE__, __LINE__))
#endif /* NDEBUG */

/* Rsnprintf: like snprintf, but guaranteed to null-terminate. */
static int Rsnprintf(char *buf, int size, const char *format, ...)
{
    int val;
    va_list(ap);
    va_start(ap, format);
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
	if (i == NA_INTEGER)
	    Rsnprintf(buf, sizeof(buf), "NA\n");
	else
	    Rsnprintf(buf, sizeof(buf), "%d\n", i);
	stream->OutBytes(stream, buf, strlen(buf));
	break;
    case R_pstream_binary_format:
	stream->OutBytes(stream, &i, sizeof(int));
	break;
    case R_pstream_xdr_format:
	R_XDREncodeInteger(i, buf);
	stream->OutBytes(stream, buf, R_XDR_INTEGER_SIZE);
	break;
    default:
	error("unknown or inappropriate output format");
    }
}

static void OutReal(R_outpstream_t stream, double d)
{
    char buf[128];
    switch (stream->type) {
    case R_pstream_ascii_format:
	if (! R_FINITE(d)) {
	    if (ISNAN(d))
		Rsnprintf(buf, sizeof(buf), "NA\n");
	    else if (d < 0)
		Rsnprintf(buf, sizeof(buf), "-Inf\n");
	    else
		Rsnprintf(buf, sizeof(buf), "Inf\n");
	}
	else
	    /* 16: full precision; 17 gives 999, 000 &c */
	    Rsnprintf(buf, sizeof(buf), "%.16g\n", d);
	stream->OutBytes(stream, buf, strlen(buf));
	break;
    case R_pstream_binary_format:
	stream->OutBytes(stream, &d, sizeof(double));
	break;
    case R_pstream_xdr_format:
	R_XDREncodeDouble(d, buf);
	stream->OutBytes(stream, buf, R_XDR_DOUBLE_SIZE);
	break;
    default:
	error("unknown or inappropriate output format");
    }
}

static void OutComplex(R_outpstream_t stream, Rcomplex c)
{
    OutReal(stream, c.r);
    OutReal(stream, c.i);
}

static void OutString(R_outpstream_t stream, char *s, int length)
{
    if (stream->type == R_pstream_ascii_format) {
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
	    stream->OutBytes(stream, buf, strlen(buf));
	}
	stream->OutChar(stream, '\n');
    }
    else
	stream->OutBytes(stream, s, length);
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
	    error("read error");
    } while (isspace(c));
    while (! isspace(c) && i < size) {
	buf[i++] = c;
	c = stream->InChar(stream);
    }
    if (i == size)
	error("read error");
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
	sscanf(word, "%s", buf);
	if (strcmp(buf, "NA") == 0)
	    return NA_INTEGER;
	else
	    sscanf(buf, "%d", &i);
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

static double InReal(R_inpstream_t stream)
{
    char word[128];
    char buf[128];
    double d;

    switch (stream->type) {
    case R_pstream_ascii_format:
	InWord(stream, word, sizeof(word));
	sscanf(word, "%s", buf);
	if (strcmp(buf, "NA") == 0)
	    return NA_REAL;
	else if (strcmp(buf, "Inf") == 0)
	    return R_PosInf;
	else if (strcmp(buf, "-Inf") == 0)
	    return R_NegInf;
	else
	    sscanf(buf, "%lg", &d);
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
			buf[i] = d;
			UngetChar(&iss, c);
			break;
		    default  : buf[i] = c;
		    }
		}
		else buf[i] = c;
	    }
	}
    }
    else
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
    if (stream->type == R_pstream_binary_format) {
#ifdef HAVE_XDR
	warning("binary format is deprecated; using xdr instead");
	stream->type = R_pstream_xdr_format;
#else
	warning("portable binary format is not available; using ascii");
	stream->type = R_pstream_ascii_format;
#endif
    }
    switch (stream->type) {
    case R_pstream_ascii_format:  stream->OutBytes(stream, "A\n", 2); break;
    case R_pstream_binary_format: stream->OutBytes(stream, "B\n", 2); break;
    case R_pstream_xdr_format:    stream->OutBytes(stream, "X\n", 2); break;
    case R_pstream_any_format:
	error("must specify ascii, binary, or xdr format");
    default: error("unknown output format");
    }
}

static void InFormat(R_inpstream_t stream)
{
    char buf[2];
    R_pstream_format_t type;
    stream->InBytes(stream, buf, 2);
    switch (buf[0]) {
    case 'A': type = R_pstream_ascii_format; break;
    case 'B': type = R_pstream_binary_format; break;
    case 'X': type = R_pstream_xdr_format; break;
    default:
	type = R_pstream_any_format;  /* keep compiler happy */
	error("unknown input format");
    }
    if (stream->type == R_pstream_any_format)
	stream->type = type;
    else if (type != stream->type)
	error("input format does not match specified format");
}


/*
 * Hash Table Functions
 *
 * Hashing functions for hashing reference objects diring writing.
 * Objects are entered, and the order in which they are encountered is
 * recorded.  GashGet returns this number, a positive integer, if the
 * object was seen before, and zero if not.  A fixed hash table size
 * is used; this is not ideal but seems adequate for now.  The hash
 * table representation consists of a (R_NilValue . vector) pair.  The
 * hash buckets are in the vector.  This indirect representation
 * should allow resizing the table at some point.
 */

#define HASHSIZE 1099

#define PTRHASH(obj) (((unsigned long) (obj)) >> 2)

#define HASH_TABLE_COUNT(ht) TRUELENGTH(CDR(ht))
#define SET_HASH_TABLE_COUNT(ht, val) SET_TRUELENGTH(CDR(ht), val)

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
    int pos = PTRHASH(obj) % HASH_TABLE_SIZE(ht);
    int count = HASH_TABLE_COUNT(ht) + 1;
    SEXP val = ScalarInteger(count);
    SEXP cell = CONS(val, HASH_BUCKET(ht, pos));

    SET_HASH_TABLE_COUNT(ht, count);
    SET_HASH_BUCKET(ht, pos, cell);
    SET_TAG(cell, obj);
}

static int HashGet(SEXP item, SEXP ht)
{
    int pos = PTRHASH(item) % HASH_TABLE_SIZE(ht);
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

/*
 * Type/Flag Packing and Unpacking
 *
 * To reduce space consumption for serializing code (lots of list
 * structure) the type a9at most 8 bits), several single bit flags,
 * and the sxpinfo gp field (LEVELS, 16 bits) are packed into a single
 * integer.  The integer is signed, so this shouldn't be pushed too
 * far.  It assumes at least 28 bits, but that should be no problem.
 */

#define IS_OBJECT_BIT_MASK (1 << 8)
#define HAS_ATTR_BIT_MASK (1 << 9)
#define HAS_TAG_BIT_MASK (1 << 10)
#define ENCODE_LEVELS(v) (v << 12)
#define DECODE_LEVELS(v) (v >> 12)
#define DECODE_TYPE(v) (v & 255)

static int PackFlags(int type, int levs, int isobj, int hasattr, int hastag)
{
    int val = type | ENCODE_LEVELS(levs);
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
#ifdef EXPERIMENTAL_NAMESPACES
		R_IsNamespaceEnv(s) ||
#endif
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
	error("no restore method available");
    return stream->InPersistHookFunc(s, stream->InPersistHookData);
}


/*
 * Serialization Code
 */

static int SaveSpecialHook(SEXP item)
{
    if (item == R_NilValue)      return NILVALUE_SXP;
    if (item == R_GlobalEnv)     return GLOBALENV_SXP;
    if (item == R_UnboundValue)  return UNBOUNDVALUE_SXP;
    if (item == R_MissingArg)    return MISSINGARG_SXP;
#ifdef EXPERIMENTAL_NAMESPACES
    if (item == R_BaseNamespace) return BASENAMESPACE_SXP;
#endif
    return 0;
}

static void OutStringVec(R_outpstream_t stream, SEXP s, SEXP ref_table)
{
    int i, len;
    SEXP names;

    R_assert(TYPEOF(s) == STRSXP);

    names = getAttrib(s, R_NamesSymbol);
    if (names != R_NilValue)
	warning("names in persistent strings are currently ignored");

    len = LENGTH(s);
    OutInteger(stream, 0); /* place holder to allow names if we want to */
    OutInteger(stream, len);
    for (i = 0; i < len; i++)
	WriteItem(STRING_ELT(s, i), ref_table, stream);
}	

/* e.g., OutVec(fp, obj, INTEGER, OutInteger) */
#define OutVec(fp, obj, accessor, outfunc)	 			\
	do { 								\
		int cnt;						\
		for (cnt = 0; cnt < LENGTH(obj); ++cnt) 		\
			outfunc(fp, accessor(obj, cnt));		\
	} while (0)

#define LOGICAL_ELT(x,__i__)	LOGICAL(x)[__i__]
#define INTEGER_ELT(x,__i__)	INTEGER(x)[__i__]
#define REAL_ELT(x,__i__)	REAL(x)[__i__]
#define COMPLEX_ELT(x,__i__)	COMPLEX(x)[__i__]

static void WriteItem (SEXP s, SEXP ref_table, R_outpstream_t stream)
{
    int i;
    SEXP t;

 tailcall:
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
	    warning("%s may not be available when loading",
		    CHAR(STRING_ELT(name, 0)));
	    OutInteger(stream, PACKAGESXP);
	    OutStringVec(stream, name, ref_table);
	}
#ifdef EXPERIMENTAL_NAMESPACES
	else if (R_IsNamespaceEnv(s)) {
	    warning("namespaces may not be available when loading");
	    OutInteger(stream, NAMESPACESXP);
	    OutStringVec(stream, R_NamespaceEnvName(s), ref_table);
	}
#endif
	else {
	    OutInteger(stream, ENVSXP);
#ifdef ENVIRONMENT_LOCKING
	    OutInteger(stream, R_EnvironmentIsLocked(s) ? 1 : 0);
#else
	    OutInteger(stream, 0);
#endif
	    WriteItem(ENCLOS(s), ref_table, stream);
	    WriteItem(FRAME(s), ref_table, stream);
	    WriteItem(TAG(s), ref_table, stream);
	    WriteItem(ATTRIB(s), ref_table, stream);
	}
    }
    else {
	int flags = PackFlags(TYPEOF(s), LEVELS(s), OBJECT(s),
			      ATTRIB(s) != R_NilValue, TAG(s) != R_NilValue);
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
	    if (ATTRIB(s) != R_NilValue)
		WriteItem(ATTRIB(s), ref_table, stream);
	    if (TAG(s) != R_NilValue)
		WriteItem(TAG(s), ref_table, stream);
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
	    OutInteger(stream, strlen(PRIMNAME(s)));
	    OutString(stream, PRIMNAME(s), strlen(PRIMNAME(s)));
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
	    OutInteger(stream, LENGTH(s));
	    OutVec(stream, s, INTEGER_ELT, OutInteger);
	    break;
	case REALSXP:
	    OutInteger(stream, LENGTH(s));
	    OutVec(stream, s, REAL_ELT, OutReal);
	    break;
	case CPLXSXP:
	    OutInteger(stream, LENGTH(s));
	    OutVec(stream, s, COMPLEX_ELT, OutComplex);
	    break;
	case STRSXP:
	case VECSXP:
	case EXPRSXP:
	    OutInteger(stream, LENGTH(s));
	    for (i = 0; i < LENGTH(s); i++)
		WriteItem(VECTOR_ELT(s, i), ref_table, stream);
	    break;
	default:
	    error("WriteItem: unknown type %i", TYPEOF(s));
	}
	if (ATTRIB(s) != R_NilValue)
	    WriteItem(ATTRIB(s), ref_table, stream);
    }
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
	OutInteger(stream, R_Version(1,4,0));
	break;
    default: error("version %d not supported", version);
    }

    PROTECT(ref_table = MakeHashTable());
    WriteItem(s, ref_table, stream);
    UNPROTECT(1);
}


/*
 * Unserialize Code
 */

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
	error("reference index out of range");
    return VECTOR_ELT(data, i);
}

static void AddReadRef(SEXP table, SEXP value)
{
    SEXP data = CAR(table);
    int count = TRUELENGTH(data) + 1;
    if (count >= LENGTH(data)) {
	int i, len;
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
	error("names in persistent strings are not supported yet");
    len = InInteger(stream);
    PROTECT(s = allocVector(STRSXP, len));
    for (i = 0; i < len; i++)
	SET_STRING_ELT(s, i, ReadItem(ref_table, stream));
    UNPROTECT(1);
    return s;
}
    
#define InVec(fp, obj, accessor, infunc, length)			\
	do {								\
		int cnt;						\
		for (cnt = 0; cnt < length; ++cnt)		\
			accessor(obj, cnt, infunc(fp));		\
	} while (0)



#define SET_LOGICAL_ELT(x,__i__,v)	(LOGICAL_ELT(x,__i__)=(v))
#define SET_INTEGER_ELT(x,__i__,v)	(INTEGER_ELT(x,__i__)=(v))
#define SET_REAL_ELT(x,__i__,v)		(REAL_ELT(x,__i__)=(v))
#define SET_COMPLEX_ELT(x,__i__,v)	(COMPLEX_ELT(x,__i__)=(v))

static SEXP ReadItem (SEXP ref_table, R_inpstream_t stream)
{
    SEXPTYPE type;
    SEXP s;
    int flags, levs, objf, hasattr, hastag, length, count;

    R_assert(TYPEOF(ref_table) == LISTSXP && TYPEOF(CAR(ref_table)) == VECSXP);

    flags = InInteger(stream);
    UnpackFlags(flags, &type, &levs, &objf, &hasattr, &hastag);

    switch(type) {
    case NILVALUE_SXP:      return R_NilValue;
    case GLOBALENV_SXP:     return R_GlobalEnv;
    case UNBOUNDVALUE_SXP:  return R_UnboundValue;
    case MISSINGARG_SXP:    return R_MissingArg;
    case BASENAMESPACE_SXP:
#ifdef EXPERIMENTAL_NAMESPACES
	return R_BaseNamespace;
#else
	warning("base namespace not available in this verison;"
		" using .GlobalEnv");
	return R_GlobalEnv;
#endif
    case REFSXP:
	return GetReadRef(ref_table, InRefIndex(stream, flags));
    case PERSISTSXP:
	PROTECT(s = InStringVec(stream, ref_table));
	s = PersistentRestore(stream, s);
	UNPROTECT(1);
	AddReadRef(ref_table, s);
	return s;
    case SYMSXP:
	PROTECT(s = ReadItem(ref_table, stream)); /* print name */
	s = install(CHAR(s));
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
#ifdef EXPERIMENTAL_NAMESPACES
	PROTECT(s = InStringVec(stream, ref_table));
	s = R_FindNamespace(s);
	AddReadRef(ref_table, s);
	UNPROTECT(1);
#else
	error("namespaces not available in this version");
#endif
	return s;
    case ENVSXP:
	{
	    int locked = InInteger(stream);

	    PROTECT(s = allocSExp(ENVSXP));

	    /* MUST register before filling in */
	    AddReadRef(ref_table, s);

	    /* Now fill it in  */
	    SET_ENCLOS(s, ReadItem(ref_table, stream));
	    SET_FRAME(s, ReadItem(ref_table, stream));
	    SET_TAG(s, ReadItem(ref_table, stream));
	    SET_ATTRIB(s, ReadItem(ref_table, stream));
	    R_RestoreHashCount(s);
#ifdef ENVIRONMENT_LOCKING
	    if (locked) R_LockEnvironment(s, FALSE);
#endif
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
	SET_ATTRIB(s, hasattr ? ReadItem(ref_table, stream) : R_NilValue);
	SET_TAG(s, hastag ? ReadItem(ref_table, stream) : R_NilValue);
	SETCAR(s, ReadItem(ref_table, stream));
	SETCDR(s, ReadItem(ref_table, stream));
	UNPROTECT(1); /* s */
	return s;
    default:
	/* These break out of the swith to have their ATTR,
	   LEVELS, and OBJECT fields filled in.  Each leaves the
	   newly allocated value PROTECTed */
	switch (type) {
	case EXTPTRSXP:
	    PROTECT(s = allocSExp(type));
	    AddReadRef(ref_table, s);
	    R_SetExternalPtrAddr(s, NULL);
	    R_SetExternalPtrProtected(s, ReadItem(ref_table, stream));
	    R_SetExternalPtrTag(s, ReadItem(ref_table, stream));
	    break;
	case WEAKREFSXP:
	    PROTECT(s = R_MakeWeakRef(R_NilValue, R_NilValue, R_NilValue,
				      FALSE));
	    AddReadRef(ref_table, s);
	    break;
	case SPECIALSXP:
	case BUILTINSXP:
	    length = InInteger(stream);
	    PROTECT(s = allocVector(CHARSXP, length)); /* as buffer */
	    InString(stream,CHAR(s), length);
	    s = mkPRIMSXP(StrToInternal(CHAR(s)), type == BUILTINSXP);
	    UNPROTECT(1);  /* pop the old s off the protect stack */
	    PROTECT(s);    /* and push the new s on it */
	    break;
	case CHARSXP:
	    length = InInteger(stream);
	    if (length == -1)
		PROTECT(s = NA_STRING);
	    else {
		PROTECT(s = allocVector(CHARSXP, length));
		InString(stream, CHAR(s), length);
	    }
	    break;
	case LGLSXP:
	case INTSXP:
	    length = InInteger(stream);
	    PROTECT(s = allocVector(type, length));
	    InVec(stream, s, SET_INTEGER_ELT, InInteger, length);
	    break;
	case REALSXP:
	    length = InInteger(stream);
	    PROTECT(s = allocVector(type, length));
	    InVec(stream, s, SET_REAL_ELT, InReal, length);
	    break;
	case CPLXSXP:
	    length = InInteger(stream);
	    PROTECT(s = allocVector(type, length));
	    InVec(stream, s, SET_COMPLEX_ELT, InComplex, length);
	    break;
	case STRSXP:
	case VECSXP:
	case EXPRSXP:
	    length = InInteger(stream);
	    PROTECT(s = allocVector(type, length));
	    for (count = 0; count < length; ++count)
		SET_VECTOR_ELT(s, count, ReadItem(ref_table, stream));
	    break;
	case BCODESXP:
	    error("this version of R cannot read byte code objects");
	case CLASSREFSXP:
	    error("this version of R cannot read class references");
	case GENERICREFSXP:
	    error("this version of R cannot read generic function references");
	default:
	    s = R_NilValue; /* keep compiler happy */
	    error("ReadItem: unknown type %i", type);
	}
	SETLEVELS(s, levs);
	SET_OBJECT(s, objf);
	SET_ATTRIB(s, hasattr ? ReadItem(ref_table, stream) : R_NilValue);
	UNPROTECT(1); /* s */
	return s;
    }
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
    int writer_version, release_version;
    SEXP obj, ref_table;

    InFormat(stream);

    /* Read the version numbers */
    version = InInteger(stream);
    writer_version = InInteger(stream);
    release_version = InInteger(stream);
    switch (version) {
    case 2: break;
    default:
	if (version != 2) {
	    int vw, pw, sw;
	    DecodeVersion(writer_version, &vw, &pw, &sw);
	    if (release_version < 0)
		error("can't read unreleased workspace version %d written by"
		      " experimental R %d.%d.%d", version, vw, pw, sw);
	    else {
		int vm, pm, sm;
		DecodeVersion(release_version, &vm, &pm, &sm);
		error("can't read workspace version %d written by R %d.%d.%d;"
		      " need R %d.%d.%d or newer",
		      version, vw, pw, sw, vm, pm, sm);
	    }
	}
    }

    /* Read the actual object back */
    PROTECT(ref_table = MakeReadRefTable());
    obj =  ReadItem(ref_table, stream);
    UNPROTECT(1);

    return obj;
}


/* 
 * Generic Persistent Stream Initializers
 */

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
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
}

void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    stream->data = data;
    stream->type = type;
    stream->version = version != 0 ? version : R_DefaultSerializeVersion;
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
    fwrite(buf, 1, length, fp); /**** error message */
}

static void InBytesFile(R_inpstream_t stream, void *buf, int length)
{
    FILE *fp = stream->data;
    fread(buf, 1, length, fp); /**** error message */
}

void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    R_InitOutPStream(stream, (R_pstream_data_t) fp, type, version,
		     OutCharFile, OutBytesFile, phook, pdata);
}

void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
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
	error("connection is not open");
    if (! con->canread || con->read == NULL)
	error("cannot read from this connection");
}

static void CheckOutConn(Rconnection con)
{
    if (! con->isopen)
	error("connection is not open");
    if (! con->canwrite || con->write == NULL)
	error("cannot write to this connection");
}

static void InBytesConn(R_inpstream_t stream, void *buf, int length)
{
    Rconnection con = (Rconnection) stream->data;
    CheckInConn(con);
    if (con->text) {
	int i;
	char *p = buf;
	for (i = 0; i < length; i++)
	    p[i] = Rconn_fgetc(con);
    }
    else {
	if (length != con->read(buf, 1, length, con))
	    error("error reading from connection");
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
	    error("error reading from connection");
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
	    error("error writing to connection");
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
	    error("error writing to connection");
    }
}

void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata)
{
    CheckOutConn(con);
    if (con->text && type != R_pstream_ascii_format)
	error("only ascii format can be written to text mode connections");
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
	    error("only ascii format can be read from text mode connections");
    }
    R_InitInPStream(stream, (R_pstream_data_t) con, type,
		    InCharConn, InBytesConn, phook, pdata);
}
