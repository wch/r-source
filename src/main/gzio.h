/* 
   Based on gzio.c from zlib 1.2.3, but considerably modified!

   Copyright (C) 1995-2005 Jean-loup Gailly.
   For conditions of distribution and use, see copyright notice in zlib.h:

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu

*/


#ifdef HAVE_CONFIG_H
#include <config.h> /* for Win32, HAVE_OFF_T and HAVE_FSEEKO */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "zlib.h"

#ifdef Win32
# define OS_CODE  0x06
#else
# define OS_CODE  0x03 
#endif

/* R ADDITION */
#ifdef Win32
# define Rz_off_t off64_t
#elif defined(HAVE_OFF_T) && defined(HAVE_FSEEKO)
# define Rz_off_t off_t
#else
# define Rz_off_t long
#endif


#define Z_BUFSIZE 16384

typedef struct gz_stream {
    z_stream stream;
    int      z_err;   /* error code for last stream operation */
    int      z_eof;   /* set if end of input file */
    FILE     *file;   /* .gz file */
    Byte     buffer[Z_BUFSIZE];  /* input or output buffer */
    uLong    crc;     /* crc32 of uncompressed data */
    int      transparent; /* 1 if input file is not compressed */
    char     mode;    /* 'w' or 'r' */
    Rz_off_t  start;  /* start of compressed data in file (header skipped) */
    Rz_off_t  in;     /* bytes into deflate or inflate */
    Rz_off_t  out;    /* bytes out of deflate or inflate */
} gz_stream;


static int get_byte(gz_stream *s)
{
    if (s->z_eof) return EOF;
    if (s->stream.avail_in == 0) {
        errno = 0;
        s->stream.avail_in = (uInt) fread(s->buffer, 1, Z_BUFSIZE, s->file);
        if (s->stream.avail_in == 0) {
            s->z_eof = 1;
            if (ferror(s->file)) s->z_err = Z_ERRNO;
            return EOF;
        }
        s->stream.next_in = s->buffer;
    }
    s->stream.avail_in--;
    return *(s->stream.next_in)++;
}

static int destroy (gz_stream *s)
{
    int err = Z_OK;

    if (!s) return Z_STREAM_ERROR;

    if (s->stream.state != NULL) {
        if (s->mode == 'w') err = deflateEnd(&(s->stream));
        else if (s->mode == 'r') err = inflateEnd(&(s->stream));
    }
    if (s->file != NULL && fclose(s->file)) {
#ifdef ESPIPE
        if (errno != ESPIPE) /* fclose is broken for pipes in HP/UX */
#endif
            err = Z_ERRNO;
    }
    if (s->z_err < 0) err = s->z_err;

    if(s) free(s);
    return err;
}

static int const gz_magic[2] = {0x1f, 0x8b}; /* gzip magic header */

/* gzip flag byte */
#define ASCII_FLAG   0x01 /* bit 0 set: file probably ascii text, unused */
#define HEAD_CRC     0x02 /* bit 1 set: header CRC present */
#define EXTRA_FIELD  0x04 /* bit 2 set: extra field present */
#define ORIG_NAME    0x08 /* bit 3 set: original file name present */
#define COMMENT      0x10 /* bit 4 set: file comment present */
#define RESERVED     0xE0 /* bits 5..7: reserved */

static void check_header(gz_stream *s)
{
    int method; /* method byte */
    int flags;  /* flags byte */
    uInt len;
    int c;

    /* Assure two bytes in the buffer so we can peek ahead -- handle case
       where first byte of header is at the end of the buffer after the last
       gzip segment */
    len = s->stream.avail_in;
    if (len < 2) {
        if (len) s->buffer[0] = s->stream.next_in[0];
        errno = 0;
        len = (uInt) fread(s->buffer + len, 1, Z_BUFSIZE >> len, s->file);
        if (len == 0 && ferror(s->file)) s->z_err = Z_ERRNO;
        s->stream.avail_in += len;
        s->stream.next_in = s->buffer;
        if (s->stream.avail_in < 2) {
            s->transparent = s->stream.avail_in;
            return;
        }
    }

    /* Peek ahead to check the gzip magic header */
    if (s->stream.next_in[0] != gz_magic[0] ||
        s->stream.next_in[1] != gz_magic[1]) {
        s->transparent = 1;
        return;
    }
    s->stream.avail_in -= 2;
    s->stream.next_in += 2;

    /* Check the rest of the gzip header */
    method = get_byte(s);
    flags = get_byte(s);
    if (method != Z_DEFLATED || (flags & RESERVED) != 0) {
        s->z_err = Z_DATA_ERROR;
        return;
    }

    /* Discard time, xflags and OS code: */
    for (len = 0; len < 6; len++) (void)get_byte(s);

    if ((flags & EXTRA_FIELD) != 0) { /* skip the extra field */
        len  =  (uInt )get_byte(s);
        len += ((uInt) get_byte(s)) << 8;
        /* len is garbage if EOF but the loop below will quit anyway */
        while (len-- != 0 && get_byte(s) != EOF) ;
    }
    if ((flags & ORIG_NAME) != 0) { /* skip the original file name */
        while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & COMMENT) != 0) {   /* skip the .gz file comment */
        while ((c = get_byte(s)) != 0 && c != EOF) ;
    }
    if ((flags & HEAD_CRC) != 0) {  /* skip the header crc */
        for (len = 0; len < 2; len++) (void) get_byte(s);
    }
    s->z_err = s->z_eof ? Z_DATA_ERROR : Z_OK;
}

gzFile R_gzopen (const char *path, const char *mode)
{
    int err;
    int level = Z_DEFAULT_COMPRESSION; /* compression level */
    int strategy = Z_DEFAULT_STRATEGY; /* compression strategy */
    char *p = (char *) mode;
    gz_stream *s;
    char fmode[80]; /* copy of mode, without the compression level */
    char *m = fmode;

    if (!path || !mode) return Z_NULL;

    s = (gz_stream *) malloc(sizeof(gz_stream));
    if (!s) return Z_NULL;

    s->stream.zalloc = (alloc_func) 0;
    s->stream.zfree = (free_func) 0;
    s->stream.opaque = (voidpf) 0;
    s->stream.next_in = s->buffer;
    s->stream.next_out = s->buffer;
    s->stream.avail_in = s->stream.avail_out = 0;
    s->file = NULL;
    s->z_err = Z_OK;
    s->z_eof = 0;
    s->in = 0;
    s->out = 0;
    s->crc = crc32(0L, Z_NULL, 0);
    s->transparent = 0;
    s->mode = '\0';
    do {
        if (*p == 'r') s->mode = 'r';
        if (*p == 'w' || *p == 'a') s->mode = 'w';
        if (*p >= '0' && *p <= '9') level = *p - '0';
        else if (*p == 'f') strategy = Z_FILTERED;
        else if (*p == 'h') strategy = Z_HUFFMAN_ONLY;
        else if (*p == 'R') strategy = Z_RLE;
        else *m++ = *p; /* copy the mode */
    } while (*p++ && m != fmode + sizeof(fmode));
    if (s->mode == '\0') return destroy(s), (gzFile) Z_NULL;

    if (s->mode == 'w') {
        err = deflateInit2(&(s->stream), level,
                           Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL, strategy);
        /* windowBits is passed < 0 to suppress zlib header */
        if (err != Z_OK) return destroy(s), (gzFile) Z_NULL;
    } else {
        err = inflateInit2(&(s->stream), -MAX_WBITS);
        /* windowBits is passed < 0 to tell that there is no zlib header.
         * Note that in this case inflate *requires* an extra "dummy" byte
         * after the compressed stream in order to complete decompression and
         * return Z_STREAM_END. Here the gzip CRC32 ensures that 4 bytes are
         * present after the compressed stream.
         */
        if (err != Z_OK) return destroy(s), (gzFile) Z_NULL;
    }
    s->stream.avail_out = Z_BUFSIZE;

    errno = 0;
    s->file = fopen(path, fmode);
    if (s->file == NULL) return destroy(s), (gzFile) Z_NULL;

    if (s->mode == 'w') {
        /* Write a very simple .gz header */
        fprintf(s->file, "%c%c%c%c%c%c%c%c%c%c", gz_magic[0], gz_magic[1],
		Z_DEFLATED, 0 /*flags*/, 0,0,0,0 /*time*/, 0 /*xflags*/, 
		OS_CODE);
        s->start = 10L;
    } else {
        check_header(s); /* skip the .gz header */
        s->start = f_tell(s->file) - s->stream.avail_in;
    }
    return (gzFile) s;
}

static void z_putLong (FILE *file, uLong x)
{
    int n;
    for (n = 0; n < 4; n++) {
        fputc((int) (x & 0xff), file);
        x >>= 8;
    }
}

static uLong getLong (gz_stream *s)
{
    uLong x = (uLong) get_byte(s);
    int c;

    x += ((uLong) get_byte(s)) << 8;
    x += ((uLong) get_byte(s)) << 16;
    c = get_byte(s);
    if (c == EOF) s->z_err = Z_DATA_ERROR;
    x += ((uLong) c) << 24;
    return x;
}

static int R_gzread (gzFile file, voidp buf, unsigned len)
{
    gz_stream *s = (gz_stream*) file;
    Bytef *start = (Bytef*) buf; /* starting point for crc computation */
    Byte  *next_out; /* == stream.next_out but not forced far (for MSDOS) */

    if (s == NULL || s->mode != 'r') return Z_STREAM_ERROR;

    if (s->z_err == Z_DATA_ERROR) {
	warning("invalid or incomplete compressed data");
	return -1;
    } else if(s->z_err == Z_ERRNO) {
	warning("error reading the file");
	return -1;
    }
    if (s->z_err == Z_STREAM_END) return 0;  /* EOF */

    next_out = (Byte*) buf;
    s->stream.next_out = (Bytef*) buf;
    s->stream.avail_out = len;

    while (s->stream.avail_out != 0) {

        if (s->transparent) {
            /* Copy first the lookahead bytes: */
            uInt n = s->stream.avail_in;
            if (n > s->stream.avail_out) n = s->stream.avail_out;
            if (n > 0) {
                memcpy(s->stream.next_out, s->stream.next_in, n);
                next_out += n;
                s->stream.next_out = next_out;
                s->stream.next_in   += n;
                s->stream.avail_out -= n;
                s->stream.avail_in  -= n;
            }
            if (s->stream.avail_out > 0) {
                s->stream.avail_out -=
                    (uInt) fread(next_out, 1, s->stream.avail_out, s->file);
            }
            len -= s->stream.avail_out;
            s->in  += len;
            s->out += len;
            if (len == 0) s->z_eof = 1;
            return (int)len;
        }
        if (s->stream.avail_in == 0 && !s->z_eof) {
            errno = 0;
            s->stream.avail_in = (uInt) fread(s->buffer, 1, Z_BUFSIZE, s->file);
            if (s->stream.avail_in == 0) {
                s->z_eof = 1;
                if (ferror(s->file)) {
                    s->z_err = Z_ERRNO;
                    break;
                }
            }
            s->stream.next_in = s->buffer;
        }
        s->in += s->stream.avail_in;
        s->out += s->stream.avail_out;
        s->z_err = inflate(&(s->stream), Z_NO_FLUSH);
        s->in -= s->stream.avail_in;
        s->out -= s->stream.avail_out;

        if (s->z_err == Z_STREAM_END) {
            /* Check CRC and original size */
            s->crc = crc32(s->crc, start, (uInt) (s->stream.next_out - start));
            start = s->stream.next_out;

            if (getLong(s) != s->crc) {
		warning("invalid or incomplete compressed data");
                s->z_err = Z_DATA_ERROR;
            } else {
                (void)getLong(s);
                /* The uncompressed length returned by above getlong() may be
                 * different from s->out in case of concatenated .gz files.
                 * Check for such files:
                 */
                check_header(s);
                if (s->z_err == Z_OK) {
                    inflateReset(&(s->stream));
                    s->crc = crc32(0L, Z_NULL, 0);
                }
            }
        }
        if (s->z_err != Z_OK || s->z_eof) break;
    }
    s->crc = crc32(s->crc, start, (uInt) (s->stream.next_out - start));

    if (len == s->stream.avail_out &&
        (s->z_err == Z_DATA_ERROR || s->z_err == Z_ERRNO)) {
	if(s->z_err == Z_DATA_ERROR)
	    warning("invalid or incomplete compressed data");
	else if(s->z_err == Z_ERRNO)
	    warning("error reading the file");
	return -1;
    }
    return (int)(len - s->stream.avail_out);
}

/* for devPS.c */
char *R_gzgets(gzFile file, char *buf, int len)
{
    char *b = buf;
    if (buf == Z_NULL || len <= 0) return Z_NULL;

    while (--len > 0 && R_gzread(file, buf, 1) == 1 && *buf++ != '\n') ;
    *buf = '\0';
    return b == buf && len > 0 ? Z_NULL : b;
}


static int R_gzwrite (gzFile file, voidpc buf, unsigned len)
{
    gz_stream *s = (gz_stream*) file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.next_in = (Bytef*) buf;
    s->stream.avail_in = len;

    while (s->stream.avail_in != 0) {
        if (s->stream.avail_out == 0) {
            s->stream.next_out = s->buffer;
            if (fwrite(s->buffer, 1, Z_BUFSIZE, s->file) != Z_BUFSIZE) {
                s->z_err = Z_ERRNO;
                break;
            }
            s->stream.avail_out = Z_BUFSIZE;
        }
        s->in += s->stream.avail_in;
        s->out += s->stream.avail_out;
        s->z_err = deflate(&(s->stream), Z_NO_FLUSH);
        s->in -= s->stream.avail_in;
        s->out -= s->stream.avail_out;
        if (s->z_err != Z_OK) break;
    }
    s->crc = crc32(s->crc, (const Bytef *) buf, len);

    return (int) (len - s->stream.avail_in);
}


static int gz_flush (gzFile file, int flush)
{
    uInt len;
    int done = 0;
    gz_stream *s = (gz_stream*) file;

    if (s == NULL || s->mode != 'w') return Z_STREAM_ERROR;

    s->stream.avail_in = 0; /* should be zero already anyway */

    for (;;) {
        len = Z_BUFSIZE - s->stream.avail_out;
        if (len != 0) {
            if ((uInt)fwrite(s->buffer, 1, len, s->file) != len) {
                s->z_err = Z_ERRNO;
                return Z_ERRNO;
            }
            s->stream.next_out = s->buffer;
            s->stream.avail_out = Z_BUFSIZE;
        }
        if (done) break;
        s->out += s->stream.avail_out;
        s->z_err = deflate(&(s->stream), flush);
        s->out -= s->stream.avail_out;

        /* Ignore the second of two consecutive flushes: */
        if (len == 0 && s->z_err == Z_BUF_ERROR) s->z_err = Z_OK;

        /* deflate has finished flushing only when it hasn't used up
         * all the available space in the output buffer:
         */
        done = (s->stream.avail_out != 0 || s->z_err == Z_STREAM_END);

        if (s->z_err != Z_OK && s->z_err != Z_STREAM_END) break;
    }
    return  s->z_err == Z_STREAM_END ? Z_OK : s->z_err;
}

/* return value 0 for success, 1 for failure */
static int int_gzrewind (gzFile file)
{
    gz_stream *s = (gz_stream*) file;

    if (s == NULL || s->mode != 'r') return -1;

    s->z_err = Z_OK;
    s->z_eof = 0;
    s->stream.avail_in = 0;
    s->stream.next_in = s->buffer;
    s->crc = crc32(0L, Z_NULL, 0);
    if (!s->transparent) (void) inflateReset(&s->stream);
    s->in = 0;
    s->out = 0;
    return f_seek(s->file, s->start, SEEK_SET);
}

static Rz_off_t R_gztell (gzFile file)
{
    gz_stream *s = (gz_stream*) file;
    if (s->mode == 'w') return s->in; else return s->out;
}

/* NB: return value is in line with fseeko, not gzseek */
static int R_gzseek (gzFile file, Rz_off_t offset, int whence)
{
    gz_stream *s = (gz_stream*) file;

    if (s == NULL || whence == SEEK_END ||
        s->z_err == Z_ERRNO || s->z_err == Z_DATA_ERROR) return -1;

    if (s->mode == 'w') {
        if (whence == SEEK_SET) offset -= s->in;
        if (offset < 0) return -1;

        /* At this point, offset is the number of zero bytes to write. */
	memset(s->buffer, 0, Z_BUFSIZE);
	while (offset > 0)  {
            uInt size = Z_BUFSIZE;
            if (offset < Z_BUFSIZE) size = (uInt) offset;
            size = R_gzwrite(file, s->buffer, size);
            if (size == 0) return -1;
            offset -= size;
        }
        return 0;
    }

    /* Rest of function is for reading only */

    /* compute absolute position */
    if (whence == SEEK_CUR) offset += s->out;
    if (offset < 0) return -1;

    if (s->transparent) {
        s->stream.avail_in = 0;
        s->stream.next_in = s->buffer;
        if (f_seek(s->file, offset, SEEK_SET) < 0) return -1;
        s->in = s->out = offset;
        return 0;
    }

    /* For a negative seek, rewind and use positive seek */
    if (offset >= s->out) offset -= s->out;
    else if (int_gzrewind(file) < 0) return -1;

    /* offset is now the number of bytes to skip. */
    while (offset > 0)  {
        int size = Z_BUFSIZE;
        if (offset < Z_BUFSIZE) size = (int) offset;
        size = R_gzread(file, s->buffer, (uInt) size);
        if (size <= 0) return -1;
        offset -= size;
    }
    return 0;
}

int R_gzclose (gzFile file)
{
    gz_stream *s = (gz_stream*) file;
    if (s == NULL) return Z_STREAM_ERROR;
    if (s->mode == 'w') {
        if (gz_flush (file, Z_FINISH) != Z_OK) 
	    return destroy((gz_stream*) file);
        z_putLong (s->file, s->crc);
        z_putLong (s->file, (uLong) (s->in & 0xffffffff));
    }
    return destroy((gz_stream*) file);
}

/*
static voidpf
R_zlib_alloc(voidpf ptr, uInt items, uInt size)
{
    return R_alloc(items, size);
}

static void
R_zlib_free(voidpf ptr, voidpf addr) {}
*/

/* added in 4.0.0, modified from uncompress[2] */
static int
R_uncompress(Bytef *dest, uLong *destLen, Bytef *source, uLong sourceLen,
	     int opt)
{
    z_stream stream;
    
    stream.next_in = source;
    stream.avail_in = 0;
    stream.zalloc = (alloc_func)0;
    stream.zfree = (free_func)0;
    stream.opaque = (voidpf)0;

    /* 
       opt is the main difference from uncompress
       0 means no headers (zlib)
       16 selects gzip
       32 allows auto-selection.
    */
    int err = inflateInit2( &stream, MAX_WBITS + opt );
    if(err != Z_OK) return err;

    const uInt max = (uInt)-1;  // could have used UINT_MAX
    uLong len = sourceLen, left = *destLen;
    stream.next_out = dest;
    stream.avail_out = 0;

    do { // do >= 2^32 bits in chunks
        if (stream.avail_out == 0) {
            stream.avail_out = left > (uLong)max ? max : (uInt)left;
            left -= stream.avail_out;
        }
        if (stream.avail_in == 0) {
            stream.avail_in = len > (uLong)max ? max : (uInt)len;
            len -= stream.avail_in;
        }
        err = inflate(&stream, Z_NO_FLUSH);
    } while (err == Z_OK);

    *destLen = stream.total_out;
    inflateEnd(&stream);
    return err;
}
