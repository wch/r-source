/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: gif.c -- cross platform GIF source code.
 * Platform: Neutral  Version: 2.35  Date: 1998/03/03
 *
 * Version: 2.30  Changes: Original version by Lachlan Patrick.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/*
 *  Gif.c - Cross-platform code for loading and saving GIFs
 *
 *  The LZW encoder and decoder used in this file were
 *  written by Gershon Elber and Eric S. Raymond as part of
 *  the GifLib package.
 *
 *  The remainder of the code was written by Lachlan Patrick
 *  as part of the GraphApp cross-platform graphics library.
 *
 *  GIF(sm) is a service mark property of CompuServe Inc.
 *  For better compression and more features than GIF,
 *  use PNG: the Portable Network Graphics format.
 */

/*
 *  Copyright and patent information:
 *
 *  Because the LZW algorithm has been patented by
 *  CompuServe Inc, you probably can't use this file
 *  in a commercial application without first paying
 *  CompuServe the appropriate licensing fee.
 *  Contact CompuServe for more information about that.
 */

/*
 *  Known problems with this code:
 *
 *  There is really only one thing to watch out for:
 *  on a PC running a 16-bit operating system, such
 *  as Windows 95 or Windows 3.1, there is a 64K limit
 *  to the size of memory blocks. In practice this
 *  limits the size of GIF files you can load to less
 *  than 256 pixels x 256 pixels.
 */

#include <stdio.h>
#include <string.h>
#include "internal.h"

/*
 *  Unsigned types defined for convenience.
 */
#ifndef _GRAPHAPP_H
  typedef unsigned char   byte;
#endif

/*
 *  Colours.
 */
#ifndef rgb

  typedef unsigned long rgb;
  #define rgb(r,g,b)	( (((rgb)r)<<16)|(((rgb)g)<<8)|((rgb)b) )
  #define getalpha(col)	(((col) & 0xFF000000UL) >> 24)
  #define getred(col)	(((col) & 0x00FF0000UL) >> 16)
  #define getgreen(col)	(((col) & 0x0000FF00UL) >> 8)
  #define getblue(col)	(((col) & 0x000000FFUL))

#endif /* rgb definition */

/*
 *  Platform independent image data:
 */

#ifndef _GRAPHAPP_H

typedef struct imagedata {
	int     depth;
	int     width;
	int     height;
	int     cmapsize;
	rgb *   cmap;
	byte *  pixels;
  } * image;

extern image   convert32to8(image img);
extern void    delimage(image img);

#endif

#ifndef array

  #define create(type)  ( (type*) memalloc(sizeof(type)) )
  #define array(n,type) ( (type*) memalloc(n*sizeof(type)) )
  #define len(a)        ( memlength((char*)(a))/sizeof((a)[0]) )
  #define element(a,i)  ( (((i)<len(a)) && ((i)>=0)) ? (a)[i] : 0 )
  #define append(a,e)   ( *(char**)&(a)=memexpand((char*)(a),sizeof((a)[0])),\
				(a)[len(a)-1]=(e) )
  #define join(a,b)     ( *(char**)&(a)=memjoin((char*)(a),(char*)(b)) )
  #define discard(a)    ( memfree((char*)(a)), (a)=0 )

  char * memalloc(long size);
  void   memfree(char *a);
  long   memlength(char *a);
  char * memexpand(char *a, long extra);
  char * memjoin(char *a, char *b);

#endif

/*
 *  Gif structures:
 */

typedef struct {
    int      length;
    rgb *    data;
  } rgbmap;

typedef struct {
    int      width, height;
    int      has_cmap, color_res, sorted, cmap_depth;
    int      bgcolor, aspect;
    rgbmap * cmap;
  } GifScreen;

typedef struct {
    int      marker;
    byte **  data;
  } GifExtension;

typedef struct {
    int      left, top, width, height;
    int      has_cmap, interlace, sorted, reserved, cmap_depth;
    rgbmap * cmap;
    byte *   data;
  } GifPicture;

typedef struct {
    int            intro;
    GifPicture *   pic;
    GifExtension * ext;
  } GifBlock;

typedef struct {
    char        header[8];
    GifScreen * screen;
    GifBlock ** blocks;
  } Gif;

/*
 *  Gif internal definitions:
 */

#define LZ_MAX_CODE	4095	/* Largest 12 bit code */
#define LZ_BITS		12

#define FLUSH_OUTPUT	4096    /* Impossible code = flush */
#define FIRST_CODE	4097    /* Impossible code = first */
#define NO_SUCH_CODE	4098    /* Impossible code = empty */

#define HT_SIZE 	8192	/* 13 bit hash table size */
#define HT_KEY_MASK	0x1FFF	/* 13 bit key mask */

#define IMAGE_LOADING	0	/* file_state = processing */
#define IMAGE_SAVING	0	/* file_state = processing */
#define IMAGE_COMPLETE	1	/* finished reading or writing */

typedef struct {
    FILE *file;
    int depth,
        clear_code, eof_code,
        running_code, running_bits,
        max_code_plus_one,
	prev_code, current_code,
        stack_ptr,
        shift_state;
    unsigned long shift_data;
    unsigned long pixel_count;
    int   file_state, position, bufsize;
    byte  buf[256];
    unsigned long hash_table[HT_SIZE];
  } GifEncoder;

typedef struct {
    FILE *file;
    int depth,
        clear_code, eof_code,
        running_code, running_bits,
        max_code_plus_one,
        prev_code, current_code,
        stack_ptr,
        shift_state;
    unsigned long shift_data;
    unsigned long pixel_count;
    int   file_state, position, bufsize;
    byte  buf[256];
    byte  stack[LZ_MAX_CODE+1];
    byte  suffix[LZ_MAX_CODE+1];
    unsigned int  prefix[LZ_MAX_CODE+1];
  } GifDecoder;


/*
 *  GIF file input/output functions.
 */

static unsigned char read_byte(FILE *file)
{
	int ch = getc(file);
	if (ch == EOF)
		ch = 0;
	return ch;
}

#define write_byte(file, ch)	 putc(ch, file)

static void read_stream(FILE *file, byte buffer[], int length)
{
	int count = fread(buffer, 1, length, file);
	while (count < length)
		buffer[count++] = '\0';
}

#define write_stream(file, buf, len) fwrite((buf),1,(int)(len),(file))

static int read_gif_int(FILE *file)
{
	int output;
	unsigned char buf[2];

	if (fread(buf, 1, 2, file) != 2)
		return 0;
	output = (((unsigned int) buf[1]) << 8) | buf[0];
	return output;
}

static void write_gif_int(FILE *file, int output)
{
	putc ( (output & 0xff), file);
	putc ( (((unsigned int) output) >> 8) & 0xff, file);
}

/*
 *  Gif data blocks:
 */

#define new_gif_data(size)      array((size), byte)
#define del_gif_data(data)	discard(data)

/*
 *  Read one code block from the Gif file.
 *  This routine should be called until NULL is returned.
 *  Use discard() to free the returned array of bytes.
 */
static byte * read_gif_data(FILE *file)
{
	byte *data;
	int size;

	size = read_byte(file);

	if (size > 0) {
		data = new_gif_data(size);
		read_stream(file, data, size);
	}
	else {
		data = NULL;
	}
	return data;
}

/*
 *  Write a Gif data block to a file.
 *  A Gif data block is a size-byte followed by that many
 *  bytes of data (0 to 255 of them).
 */
static void write_gif_data(FILE *file, byte *data)
{
	if (data) {
		write_byte(file, len(data));
		write_stream(file, data, len(data));
	}
	else
		write_byte(file, 0);
}

#if DEBUG
static void print_gif_data(FILE *file, byte *data)
{
	int i, ch, prev;
	int ch_printable, prev_printable;

	if (data) {
		fprintf(file, "(length=%d) [", len(data));
		prev_printable = 1;
		for (i=0; i < len(data); i++) {
			ch = data[i];
			ch_printable = isprint(ch) ? 1 : 0;

			if (ch_printable != prev_printable)
				fprintf(file, " ");

			if (ch_printable)
				fprintf(file, "%c", (char)ch);
			else
				fprintf(file, "%-02.2X,", ch);

			prev = ch;
			prev_printable = isprint(prev) ? 1 : 0;
		}
		fprintf(file, "]\n");
	}
	else {
		fprintf(file, "[]\n");
	}
}
#endif

/*
 *  Read the next byte from a Gif file.
 *
 *  This function is aware of the block-nature of Gif files,
 *  and will automatically skip to the next block to find
 *  a new byte to read, or return 0 if there is no next block.
 */
static byte read_gif_byte(FILE *file, GifDecoder *decoder)
{
	byte *buf = decoder->buf;
	byte next;

	if (decoder->file_state == IMAGE_COMPLETE)
		return '\0';

	if (decoder->position == decoder->bufsize)
	{	/* internal buffer now empty! */
		/* read the block size */
		decoder->bufsize = read_byte(file);
		if (decoder->bufsize == 0) {
			decoder->file_state = IMAGE_COMPLETE;
			return '\0';
		}
		read_stream(file, buf, decoder->bufsize);
		next = buf[0];
		decoder->position = 1;	/* where to get chars */
	}
	else {
		next = buf[decoder->position++];
	}

	return next;
}

/*
 *  Read to end of an image, including the zero block.
 */
static void finish_gif_picture(FILE *file, GifDecoder *decoder)
{
	byte *buf = decoder->buf;

	while (decoder->bufsize != 0) {
		decoder->bufsize = read_byte(file);
		if (decoder->bufsize == 0) {
			decoder->file_state = IMAGE_COMPLETE;
			break;
		}
		read_stream(file, buf, decoder->bufsize);
	}
}

/*
 *  Write a byte to a Gif file.
 *
 *  This function is aware of Gif block structure and buffers
 *  chars until 255 can be written, writing the size byte first.
 *  If FLUSH_OUTPUT is the char to be written, the buffer is
 *  written and an empty block appended.
 */
static void write_gif_byte(FILE *file, GifEncoder *encoder, int ch)
{
	byte *buf = encoder->buf;

	if (encoder->file_state == IMAGE_COMPLETE)
		return;

	if (ch == FLUSH_OUTPUT)
	{
		if (encoder->bufsize) {
			write_byte(file, encoder->bufsize);
			write_stream(file, buf, encoder->bufsize);
			encoder->bufsize = 0;
		}
		/* write an empty block to mark end of data */
		write_byte(file, 0);
		encoder->file_state = IMAGE_COMPLETE;
	}
	else {
		if (encoder->bufsize == 255) {
			/* write this buffer to the file */
			write_byte(file, encoder->bufsize);
			write_stream(file, buf, encoder->bufsize);
			encoder->bufsize = 0;
		}
		buf[encoder->bufsize++] = ch;
	}
}

/*
 *  Colour maps:
 */

#define new_rgbmap()	create(rgbmap)

static void del_rgbmap(rgbmap *cmap)
{
	discard(cmap->data);
	discard(cmap);
}

static void read_rgbmap(FILE *file, rgbmap *cmap)
{
	int i;
	byte r, g, b;

	for (i=0; i<cmap->length; i++) {
		r = read_byte(file);
		g = read_byte(file);
		b = read_byte(file);
		append(cmap->data, rgb(r,g,b));
	}
}

static void write_rgbmap(FILE *file, rgbmap *cmap)
{
	int i;
	rgb c;

	for (i=0; i<cmap->length; i++) {
		c = cmap->data[i];
		write_byte(file, getred(c));
		write_byte(file, getgreen(c));
		write_byte(file, getblue(c));
	}
}

#if DEBUG
static void print_rgbmap(FILE *file, rgbmap *cmap)
{
	int i;

	fprintf(file, "  rgbmap (length=%d):\n", cmap->length);
	for (i=0; i<cmap->length; i++) {
		fprintf(file, "   %-02.2X = ", i);
		fprintf(file, "0x%-08.8lX\n", cmap->data[i]);
	}
}
#endif

/*
 *  GifScreen:
 */

static GifScreen * new_gif_screen(void)
{
	GifScreen *screen = create(GifScreen);
	if (screen)
		screen->cmap = new_rgbmap();
	return screen;
}

static void del_gif_screen(GifScreen *screen)
{
	del_rgbmap(screen->cmap);
	discard(screen);
}

static void read_gif_screen(FILE *file, GifScreen *screen)
{
	byte info;

	screen->width       = read_gif_int(file);
	screen->height      = read_gif_int(file);

	info                = read_byte(file);
	screen->has_cmap    =  (info & 0x80) >> 7;
	screen->color_res   = ((info & 0x70) >> 4) + 1;
	screen->sorted      =  (info & 0x08) >> 3;
	screen->cmap_depth  =  (info & 0x07)       + 1;

	screen->bgcolor     = read_byte(file);
	screen->aspect      = read_byte(file);

	if (screen->has_cmap) {
		screen->cmap->length = 1 << screen->cmap_depth;
		read_rgbmap(file, screen->cmap);
	}
}

static void write_gif_screen(FILE *file, GifScreen *screen)
{
	byte info;

	write_gif_int(file, screen->width);
	write_gif_int(file, screen->height);

	info = 0;
	info = info | (screen->has_cmap ? 0x80 : 0x00);
	info = info | ((screen->color_res - 1) << 4);
	info = info | (screen->sorted ? 0x08 : 0x00);
	info = info | ((screen->cmap_depth) - 1);
	write_byte(file, info);

	write_byte(file, screen->bgcolor);
	write_byte(file, screen->aspect);

	if (screen->has_cmap) {
		write_rgbmap(file, screen->cmap);
	}
}

#if DEBUG
static void print_gif_screen(FILE *file, GifScreen *screen)
{
	fprintf(file, " GifScreen:\n");
	fprintf(file, "  width      = %d\n", screen->width);
	fprintf(file, "  height     = %d\n", screen->height);

	fprintf(file, "  has_cmap   = %d\n", screen->has_cmap ? 1:0);
	fprintf(file, "  color_res  = %d\n", screen->color_res);
	fprintf(file, "  sorted     = %d\n", screen->sorted ? 1:0);
	fprintf(file, "  cmap_depth = %d\n", screen->cmap_depth);

	fprintf(file, "  bgcolor    = %-02.2X\n", screen->bgcolor);
	fprintf(file, "  aspect     = %d\n", screen->aspect);

	if (screen->has_cmap) {
		print_rgbmap(file, screen->cmap);
	}
}
#endif

/*
 *  GifExtension:
 */

#define new_gif_extension()	create(GifExtension)

static void del_gif_extension(GifExtension *ext)
{
	int i;

	for (i=0; i < len(ext->data); i++)
		del_gif_data(ext->data[i]);
	discard(ext->data);
	discard(ext);
}

static void read_gif_extension(FILE *file, GifExtension *ext)
{
	byte *data;

	ext->marker = read_byte(file);

	data = read_gif_data(file);
	while (data) {
		append(ext->data, data);
		data = read_gif_data(file);
	}
}

static void write_gif_extension(FILE *file, GifExtension *ext)
{
	int i;

	write_byte(file, ext->marker);

	for (i=0; i < len(ext->data); i++)
		write_gif_data(file, ext->data[i]);
	write_gif_data(file, NULL);
}

#if DEBUG
static void print_gif_extension(FILE *file, GifExtension *ext)
{
	int i;

	fprintf(file, " GifExtension:\n");
	fprintf(file, "  marker = 0x%-02.2X\n", ext->marker);
	for (i=0; i < len(ext->data); i++) {
		fprintf(file, "  data = ");
		print_gif_data(file, ext->data[i]);
	}
}
#endif

/*
 *  GifDecoder:
 */

#define new_gif_decoder()	 create(GifDecoder)
#define del_gif_decoder(decoder) discard(decoder)

static void init_gif_decoder(FILE *file, GifDecoder *decoder)
{
	int i, depth;
	int lzw_min;
	unsigned int *prefix;

	lzw_min = read_byte(file);
	depth = lzw_min;

	decoder->file_state   = IMAGE_LOADING;
	decoder->position     = 0;
	decoder->bufsize      = 0;
	decoder->buf[0]       = 0;
	decoder->depth        = depth;
	decoder->clear_code   = (1 << depth);
	decoder->eof_code     = decoder->clear_code + 1;
	decoder->running_code = decoder->eof_code + 1;
	decoder->running_bits = depth + 1;
	decoder->max_code_plus_one = 1 << decoder->running_bits;
	decoder->stack_ptr    = 0;
	decoder->prev_code    = NO_SUCH_CODE;
	decoder->shift_state  = 0;
	decoder->shift_data   = 0;

	prefix = decoder->prefix;
	for (i = 0; i <= LZ_MAX_CODE; i++)
		prefix[i] = NO_SUCH_CODE;
}

/*
 *  Read the next Gif code word from the file.
 *
 *  This function looks in the decoder to find out how many
 *  bits to read, and uses a buffer in the decoder to remember
 *  bits from the last byte input.
 */
static int read_gif_code(FILE *file, GifDecoder *decoder)
{
	int code;
	byte next_byte;
	static int code_masks[] = {
		0x0000, 0x0001, 0x0003, 0x0007,
		0x000f, 0x001f, 0x003f, 0x007f,
		0x00ff, 0x01ff, 0x03ff, 0x07ff,
		0x0fff
	};

	while (decoder->shift_state < decoder->running_bits)
	{
		/* Need more bytes from input file for next code: */
		next_byte = read_gif_byte(file, decoder);
		decoder->shift_data |=
		  ((unsigned long) next_byte) << decoder->shift_state;
		decoder->shift_state += 8;
	}

	code = decoder->shift_data
		& code_masks[decoder->running_bits];

	decoder->shift_data >>= decoder->running_bits;
	decoder->shift_state -= decoder->running_bits;

	/* If code cannot fit into running_bits bits,
	 * we must raise its size.
	 * Note: codes above 4095 are used for signalling. */
	if (++decoder->running_code > decoder->max_code_plus_one
		&& decoder->running_bits < LZ_BITS)
	{
		decoder->max_code_plus_one <<= 1;
		decoder->running_bits++;
	}
	return code;
}

/*
 *  Routine to trace the prefix-linked-list until we get
 *  a prefix which is a pixel value (less than clear_code).
 *  Returns that pixel value.
 *
 *  If picture is defective, we might loop here forever,
 *  so we limit the loops to the maximum possible if the
 *  picture is okay, i.e. LZ_MAX_CODE times.
 */
static int trace_prefix(unsigned int *prefix, int code, int clear_code)
{
	int i = 0;

	while (code > clear_code && i++ <= LZ_MAX_CODE)
		code = prefix[code];
	return code;
}

/*
 *  The LZ decompression routine:
 *  Call this function once per scanline to fill in a picture.
 */
static void read_gif_line(FILE *file, GifDecoder *decoder, byte *line, int length)
{
    int i = 0, j;
    int current_code, eof_code, clear_code;
    int current_prefix, prev_code, stack_ptr;
    byte *stack, *suffix;
    unsigned int *prefix;

    prefix	= decoder->prefix;
    suffix	= decoder->suffix;
    stack	= decoder->stack;
    stack_ptr	= decoder->stack_ptr;
    eof_code	= decoder->eof_code;
    clear_code	= decoder->clear_code;
    prev_code	= decoder->prev_code;

    if (stack_ptr != 0) {
	/* Pop the stack */
	while (stack_ptr != 0 && i < length)
		line[i++] = stack[--stack_ptr];
    }

    while (i < length)
    {
	current_code = read_gif_code(file, decoder);

	if (current_code == eof_code)
	{
	   /* unexpected EOF */
	   if (i != length - 1 || decoder->pixel_count != 0)
		return;
	   i++;
	}
	else if (current_code == clear_code)
	{
	    /* reset prefix table etc */
	    for (j = 0; j <= LZ_MAX_CODE; j++)
		prefix[j] = NO_SUCH_CODE;
	    decoder->running_code = decoder->eof_code + 1;
	    decoder->running_bits = decoder->depth + 1;
	    decoder->max_code_plus_one = 1 << decoder->running_bits;
	    prev_code = decoder->prev_code = NO_SUCH_CODE;
	}
	else {
	    /* Regular code - if in pixel range
	     * simply add it to output pixel stream,
	     * otherwise trace code-linked-list until
	     * the prefix is in pixel range. */
	    if (current_code < clear_code) {
		/* Simple case. */
		line[i++] = current_code;
	    }
	    else {
		/* This code needs to be traced:
		 * trace the linked list until the prefix is a
		 * pixel, while pushing  the suffix pixels on
		 * to the stack. If finished, pop the stack
		 * to output the pixel values. */
	    	if (prefix[current_code] == NO_SUCH_CODE) {
		    /* Only allowed if current_code is exactly
		     * the running code:
		     * In that case current_code = XXXCode,
		     * current_code or the prefix code is the
		     * last code and the suffix char is
		     * exactly the prefix of last code! */
		    if (current_code == decoder->running_code - 2) {
			current_prefix = prev_code;
			suffix[decoder->running_code - 2]
			    = stack[stack_ptr++]
			    = trace_prefix(prefix, prev_code, clear_code);
		    }
		    else {
			return; /* image defect */
		    }
		}
		else
		    current_prefix = current_code;

		/* Now (if picture is okay) we should get
		 * no NO_SUCH_CODE during the trace.
		 * As we might loop forever (if picture defect)
		 * we count the number of loops we trace and
		 * stop if we get LZ_MAX_CODE.
		 * Obviously we cannot loop more than that. */
		j = 0;
		while (j++ <= LZ_MAX_CODE
			&& current_prefix > clear_code
			&& current_prefix <= LZ_MAX_CODE)
		{
		    stack[stack_ptr++] = suffix[current_prefix];
		    current_prefix = prefix[current_prefix];
		}
		if (j >= LZ_MAX_CODE || current_prefix > LZ_MAX_CODE)
		    return; /* image defect */

		/* Push the last character on stack: */
		stack[stack_ptr++] = current_prefix;

		/* Now pop the entire stack into output: */
		while (stack_ptr != 0 && i < length)
		    line[i++] = stack[--stack_ptr];
	    }
	    if (prev_code != NO_SUCH_CODE) {
		prefix[decoder->running_code - 2] = prev_code;

		if (current_code == decoder->running_code - 2) {
		    /* Only allowed if current_code is exactly
		     * the running code:
		     * In that case current_code = XXXCode,
		     * current_code or the prefix code is the
		     * last code and the suffix char is
		     * exactly the prefix of the last code! */
		    suffix[decoder->running_code - 2]
			= trace_prefix(prefix, prev_code, clear_code);
		}
		else {
		    suffix[decoder->running_code - 2]
			= trace_prefix(prefix, current_code, clear_code);
		}
	    }
	    prev_code = current_code;
	}
    }

    decoder->prev_code = prev_code;
    decoder->stack_ptr = stack_ptr;
}

/*
 *  Hash table:
 */

/*
 *  The 32 bits contain two parts: the key & code:
 *  The code is 12 bits since the algorithm is limited to 12bits
 *  The key is a 12 bit prefix code + 8 bit new char = 20 bits.
 */
#define HT_GET_KEY(x)	(x >> 12)
#define HT_GET_CODE(x)	(x & 0x0FFF)
#define HT_PUT_KEY(x)	(x << 12)
#define HT_PUT_CODE(x)	(x & 0x0FFF)

/*
 *  Generate a hash key from the given unique key.
 *  The given key is assumed to be 20 bits as follows:
 *    lower 8 bits are the new postfix character,
 *    the upper 12 bits are the prefix code.
 */
static int gif_hash_key(unsigned long key)
{
	return ((key >> 12) ^ key) & HT_KEY_MASK;
}

/*
 *  Clear the hash_table to an empty state.
 */
static void clear_gif_hash_table(unsigned long *hash_table)
{
	int i;
	for (i=0; i<HT_SIZE; i++)
		hash_table[i] = 0xFFFFFFFFL;
}

/*
 *  Insert a new item into the hash_table.
 *  The data is assumed to be new.
 */
static void add_gif_hash_entry(unsigned long *hash_table, unsigned long key, int code)
{
	int hkey = gif_hash_key(key);

	while (HT_GET_KEY(hash_table[hkey]) != 0xFFFFFL) {
		hkey = (hkey + 1) & HT_KEY_MASK;
	}
	hash_table[hkey] = HT_PUT_KEY(key) | HT_PUT_CODE(code);
}

/*
 *  Determine if given key exists in hash_table and if so
 *  returns its code, otherwise returns -1.
 */
static int lookup_gif_hash(unsigned long *hash_table, unsigned long key)
{
	int hkey = gif_hash_key(key);
	unsigned long htkey;

	while ((htkey = HT_GET_KEY(hash_table[hkey])) != 0xFFFFFL) {
		if (key == htkey)
			return HT_GET_CODE(hash_table[hkey]);
		hkey = (hkey + 1) & HT_KEY_MASK;
	}
	return -1;
}

/*
 *  GifEncoder:
 */

#define new_gif_encoder()	 create(GifEncoder)
#define del_gif_encoder(encoder) discard(encoder);

/*
 *  Write a Gif code word to the output file.
 *
 *  This function packages code words up into whole bytes
 *  before writing them. It uses the encoder to store
 *  codes until enough can be packaged into a whole byte.
 */
static void
write_gif_code(FILE *file, GifEncoder *encoder, int code)
{
	if (code == FLUSH_OUTPUT) {
		/* write all remaining data */
		while (encoder->shift_state > 0)
		{
			write_gif_byte(file, encoder,
				encoder->shift_data & 0xff);
			encoder->shift_data >>= 8;
			encoder->shift_state -= 8;
		}
		encoder->shift_state = 0;
		write_gif_byte(file, encoder, FLUSH_OUTPUT);
	}
	else {
		encoder->shift_data |=
			((long) code) << encoder->shift_state;
		encoder->shift_state += encoder->running_bits;

		while (encoder->shift_state >= 8)
		{
			/* write full bytes */
			write_gif_byte(file, encoder,
				encoder->shift_data & 0xff);
			encoder->shift_data >>= 8;
			encoder->shift_state -= 8;
		}
	}

	/* If code can't fit into running_bits bits, raise its size.
	 * Note that codes above 4095 are for signalling. */
	if (encoder->running_code >= encoder->max_code_plus_one
		&& code <= 4095)
	{
    		encoder->max_code_plus_one = 1 << ++encoder->running_bits;
	}
}

/*
 *   Initialise the encoder, given a rgbmap depth.
 */
static void
init_gif_encoder(FILE *file, GifEncoder *encoder, int depth)
{
	int lzw_min = depth = (depth < 2 ? 2 : depth);

	encoder->file_state   = IMAGE_SAVING;
	encoder->position     = 0;
	encoder->bufsize      = 0;
	encoder->buf[0]       = 0;
	encoder->depth        = depth;
	encoder->clear_code   = (1 << depth);
	encoder->eof_code     = encoder->clear_code + 1;
	encoder->running_code = encoder->eof_code + 1;
	encoder->running_bits = depth + 1;
	encoder->max_code_plus_one = 1 << encoder->running_bits;
	encoder->current_code = FIRST_CODE;
	encoder->shift_state  = 0;
	encoder->shift_data   = 0;

	/* Write the LZW minimum code size: */
	write_byte(file, lzw_min);

	/* Clear hash table, output Clear code: */
	clear_gif_hash_table(encoder->hash_table);
	write_gif_code(file, encoder, encoder->clear_code);
}

/*
 *  Write one scanline of pixels out to the Gif file,
 *  compressing that line using LZW into a series of codes.
 */
static void
write_gif_line(FILE *file, GifEncoder *encoder, byte *line, int length)
{
    int i = 0, current_code, new_code;
    unsigned long new_key;
    byte pixval;
    unsigned long *hash_table;

    hash_table = encoder->hash_table;

    if (encoder->current_code == FIRST_CODE)
	current_code = line[i++];
    else
	current_code = encoder->current_code;

    while (i < length)
    {
	pixval = line[i++]; /* Fetch next pixel from stream */

	/* Form a new unique key to search hash table for the code
	 * Combines current_code as prefix string with pixval as
	 * postfix char */
	new_key = (((unsigned long) current_code) << 8) + pixval;
	if ((new_code = lookup_gif_hash(hash_table, new_key)) >= 0) {
	    /* This key is already there, or the string is old,
	     * so simply take new code as current_code */
	    current_code = new_code;
	}
	else {
	    /* Put it in hash table, output the prefix code,
	     * and make current_code equal to pixval */
	    write_gif_code(file, encoder, current_code);
	    current_code = pixval;

	    /* If the hash_table if full, send a clear first
	     * then clear the hash table: */
	    if (encoder->running_code >= LZ_MAX_CODE) {
		write_gif_code(file, encoder, encoder->clear_code);
		encoder->running_code = encoder->eof_code + 1;
		encoder->running_bits = encoder->depth + 1;
		encoder->max_code_plus_one = 1 << encoder->running_bits;
		clear_gif_hash_table(hash_table);
	    }
	    else {
		/* Put this unique key with its relative code in hash table */
		add_gif_hash_entry(hash_table, new_key, encoder->running_code++);
	    }
	}
    }

    /* Preserve the current state of the compression algorithm: */
    encoder->current_code = current_code;
}

static void flush_gif_encoder(FILE *file, GifEncoder *encoder)
{
	write_gif_code(file, encoder, encoder->current_code);
	write_gif_code(file, encoder, encoder->eof_code);
	write_gif_code(file, encoder, FLUSH_OUTPUT);
}

/*
 *  GifPicture:
 */

static GifPicture * new_gif_picture(void)
{
	GifPicture *pic = create(GifPicture);
	if (pic) {
		pic->cmap = new_rgbmap();
		pic->data = NULL;
	}
	return pic;
}

static void del_gif_picture(GifPicture *pic)
{
	del_rgbmap(pic->cmap);
	discard(pic->data);
	discard(pic);
}

static void read_gif_picture_data(FILE *file, GifPicture *pic)
{
	GifDecoder *decoder;
	long w, h;
	int interlace_start[] = {0, 4, 2, 1};
	int interlace_step[]  = {8, 8, 4, 2};
	int scan_pass, row;

	w = pic->width;
	h = pic->height;
	pic->data = array(w * h, byte);
	if (pic->data == NULL)
		return;

	decoder = new_gif_decoder();
	init_gif_decoder(file, decoder);

	if (pic->interlace) {
	  for (scan_pass = 0; scan_pass < 4; scan_pass++) {
	    row = interlace_start[scan_pass];
	    while (row < h) {
		read_gif_line(file, decoder, pic->data + w*row, w);
		row += interlace_step[scan_pass];
	    }
	  }
	}
	else {
	  row = 0;
	  while (row < h) {
		read_gif_line(file, decoder, pic->data + w*row, w);
		row += 1;
	  }
	}
	finish_gif_picture(file, decoder);

	del_gif_decoder(decoder);
}

static void read_gif_picture(FILE *file, GifPicture *pic)
{
	byte info;

	pic->left   = read_gif_int(file);
	pic->top    = read_gif_int(file);
	pic->width  = read_gif_int(file);
	pic->height = read_gif_int(file);

	info = read_byte(file);
	pic->has_cmap    = (info & 0x80) >> 7;
	pic->interlace   = (info & 0x40) >> 6;
	pic->sorted      = (info & 0x20) >> 5;
	pic->reserved    = (info & 0x18) >> 4;
	pic->cmap_depth  = (info & 0x07) + 1;

	if (pic->has_cmap) {
		pic->cmap->length = 1 << pic->cmap_depth;
		read_rgbmap(file, pic->cmap);
	}

	read_gif_picture_data(file, pic);
}

static void write_gif_picture_data(FILE *file, GifPicture *pic)
{
	GifEncoder *encoder;
	long w, h;
	int interlace_start[] = {0, 4, 2, 1};
	int interlace_step[]  = {8, 8, 4, 2};
	int scan_pass, row;

	w = pic->width;
	h = pic->height;

	encoder = new_gif_encoder();
	init_gif_encoder(file, encoder, pic->cmap_depth);

	if (pic->interlace) {
	  for (scan_pass = 0; scan_pass < 4; scan_pass++) {
	    row = interlace_start[scan_pass];
	    while (row < h) {
		write_gif_line(file, encoder, pic->data + w*row, w);
		row += interlace_step[scan_pass];
	    }
	  }
	}
	else {
	  row = 0;
	  while (row < h) {
		write_gif_line(file, encoder, pic->data + w*row, w);
		row += 1;
	  }
	}

	flush_gif_encoder(file, encoder);
	del_gif_encoder(encoder);
}

static void write_gif_picture(FILE *file, GifPicture *pic)
{
	byte info;

	write_gif_int(file, pic->left);
	write_gif_int(file, pic->top);
	write_gif_int(file, pic->width);
	write_gif_int(file, pic->height);

	info = 0;
	info = info | (pic->has_cmap    ? 0x80 : 0x00);
	info = info | (pic->interlace   ? 0x40 : 0x00);
	info = info | (pic->sorted      ? 0x20 : 0x00);
	info = info | (pic->reserved << 4);
	info = info | (pic->cmap_depth - 1);
	write_byte(file, info);

	if (pic->has_cmap)
		write_rgbmap(file, pic->cmap);

	write_gif_picture_data(file, pic);
}

#if DEBUG
static void print_gif_picture_data(FILE *file, GifPicture *pic)
{
	int pixval, row, col;

	for (row = 0; row < pic->height; row++) {
	  fprintf(file, "   [");
	  for (col = 0; col < pic->width; col++) {
	    pixval = pic->data[row*pic->width+col];
	    fprintf(file, "%-02.2X", pixval);
	  }
	  fprintf(file, "]\n");
	}
}

static void print_gif_picture(FILE *file, GifPicture *pic)
{
	fprintf(file, " GifPicture:\n");
	fprintf(file, "  left       = %d\n", pic->left);
	fprintf(file, "  top        = %d\n", pic->top);
	fprintf(file, "  width      = %d\n", pic->width);
	fprintf(file, "  height     = %d\n", pic->height);

	fprintf(file, "  has_cmap   = %d\n", pic->has_cmap);
	fprintf(file, "  interlace  = %d\n", pic->interlace);
	fprintf(file, "  sorted     = %d\n", pic->sorted);
	fprintf(file, "  reserved   = %d\n", pic->reserved);
	fprintf(file, "  cmap_depth = %d\n", pic->cmap_depth);

	if (pic->has_cmap)
		print_rgbmap(file, pic->cmap);

	print_gif_picture_data(file, pic);
}
#endif

/*
 *  GifBlock:
 */

#define new_gif_block()	create(GifBlock)

static void del_gif_block(GifBlock *block)
{
	if (block->pic)
		del_gif_picture(block->pic);
	if (block->ext)
		del_gif_extension(block->ext);
	discard(block);
}

static void read_gif_block(FILE *file, GifBlock *block)
{
	block->intro = read_byte(file);
	if (block->intro == 0x2C) {
		block->pic = new_gif_picture();
		read_gif_picture(file, block->pic);
	}
	else if (block->intro == 0x21) {
		block->ext = new_gif_extension();
		read_gif_extension(file, block->ext);
	}
}

static void write_gif_block(FILE *file, GifBlock *block)
{
	write_byte(file, block->intro);
	if (block->pic)
		write_gif_picture(file, block->pic);
	if (block->ext)
		write_gif_extension(file, block->ext);
}

#if DEBUG
static void print_gif_block(FILE *file, GifBlock *block)
{
	fprintf(file, " GifBlock (intro=0x%-02.2X):\n", block->intro);
	if (block->pic)
		print_gif_picture(file, block->pic);
	if (block->ext)
		print_gif_extension(file, block->ext);
}
#endif

/*
 *  Gif:
 */

static Gif * new_gif(void)
{
	Gif *gif = create(Gif);
	if (gif) {
		strcpy(gif->header, "GIF87a");
		gif->screen = new_gif_screen();
		gif->blocks = NULL;
	}
	return gif;
}

static void del_gif(Gif *gif)
{
	int i;

	del_gif_screen(gif->screen);
	for (i=0; i < len(gif->blocks); i++)
		del_gif_block(gif->blocks[i]);
	discard(gif);
}

#if 0 /* currently unused but completely functional */
static void read_gif(FILE *file, Gif *gif)
{
	int i;
	GifBlock *block;

	for (i=0; i<6; i++)
		gif->header[i] = read_byte(file);
	if (strncmp(gif->header, "GIF", 3) != 0)
		return; /* error */

	read_gif_screen(file, gif->screen);

	while (1) {
		block = new_gif_block();
		read_gif_block(file, block);

		if (block->intro == 0x3B) {	/* terminator */
			del_gif_block(block);
			break;
		}
		else  if (block->intro == 0x2C)	/* image */
			append(gif->blocks, block);

		else  if (block->intro == 0x21)	/* extension */
			append(gif->blocks, block);

		else {	/* error */
			del_gif_block(block);
			break;
		}
	}
}
#endif

static void read_one_gif_picture(FILE *file, Gif *gif)
{
	int i;
	GifBlock *block;

	for (i=0; i<6; i++)
		gif->header[i] = read_byte(file);
	if (strncmp(gif->header, "GIF", 3) != 0)
		return; /* error */

	read_gif_screen(file, gif->screen);

	while (1) {
		block = new_gif_block();
		read_gif_block(file, block);

		if (block->intro == 0x3B) {	/* terminator */
			del_gif_block(block);
			break;
		}
		else if (block->intro == 0x2C) { /* image */
			append(gif->blocks, block);
			break;
		}
		else if (block->intro == 0x21) { /* extension */
			append(gif->blocks, block);
			continue;
		}
		else {	/* error! */
			del_gif_block(block);
			break;
		}
	}
}

static void write_gif(FILE *file, Gif *gif)
{
	int i;

	fprintf(file, "%s", gif->header);
	write_gif_screen(file, gif->screen);
	for (i=0; i < len(gif->blocks); i++)
		write_gif_block(file, gif->blocks[i]);
	write_byte(file, 0x3B);
}

#if DEBUG
static void print_gif(FILE *file, Gif *gif)
{
	int i;

	fprintf(file, "Gif header=%s\n", gif->header);
	print_gif_screen(file, gif->screen);
	for (i=0; i < len(gif->blocks); i++)
		print_gif_block(file, gif->blocks[i]);
	fprintf(file, "End of gif.\n\n");
}
#endif

/*
 *  Reading and Writing Gif files:
 */

static Gif * read_gif_file(char *filename)
{
	Gif *gif;
	FILE *file;

	file = fopen(filename, "rb");
	if (file == NULL)
		return NULL;
	gif = new_gif();
	if (gif == NULL) {
		fclose(file);
		return NULL;
	}
	read_one_gif_picture(file, gif);
	fclose(file);
	if (strncmp(gif->header, "GIF", 3) != 0) {
		del_gif(gif);
		gif = NULL;
	}
	return gif;
}

static void write_gif_file(char *filename, Gif *gif)
{
	FILE *file;

	file = fopen(filename, "wb");
	if (file == NULL)
		return;
	if (gif == NULL) {
		fclose(file);
		return;
	}
	write_gif(file, gif);
	fclose(file);
}

/*
 *  Loading and Saving images:
 */

image load_gif(char *filename)
{
	image img;
	Gif *gif;
	GifPicture *pic = NULL;
	GifExtension *ext = NULL;
	rgbmap *cmap;
	int i, trans_value;

	/* Read the Gif file into memory: */
	gif = read_gif_file(filename);
	if (gif == NULL)
		return NULL;

	/* Find an image block and the extension just before it: */
	for (i=0; i < len(gif->blocks); i++) {
		if (gif->blocks[i]->ext) {
			ext = gif->blocks[i]->ext;
		}
		if (gif->blocks[i]->pic) {
			pic = gif->blocks[i]->pic;
			break;
		}
	}
	if (pic == NULL) {
		del_gif(gif);
		return NULL;
	}

	/* Find a colour map: */
	cmap = pic->cmap;
	if ((cmap == NULL) || (cmap->length == 0))
		cmap = gif->screen->cmap;
	if (cmap == NULL) {
		del_gif(gif);
		return NULL;
	}

	/* Check for a transparent colour: */
	if ((ext) && (ext->marker == 0xF9))	/* graphic control block */
	{
		if ((ext->data) && (len(ext->data[0]) == 4)
			&& (ext->data[0][0] & 0x01)) /* transparent flag */
		{
			trans_value = ext->data[0][3];

			if (cmap->length > trans_value)
				cmap->data[trans_value] |= 0xFF000000UL;
		}
	}

	/* Create a image: */
	img = create(struct imagedata);
	if (img == NULL) {
		del_gif(gif);
		return NULL;
	}

	/* Fill the image with information: */
	img->depth      = 8;
	img->width      = pic->width;
	img->height     = pic->height;
	img->cmapsize   = cmap->length;
	img->cmap       = cmap->data;
	img->pixels     = pic->data;

	/* Unlink the pointers from their original locations: */
	cmap->data = NULL;
	pic->data = NULL;

	/* Clean up and return the image: */
	del_gif(gif);
	return img;
}

static int bitsize(int cmap_length)
{
	int depth;

	for (depth = 1; depth <= 8; depth++)
		if ((1 << depth) >= cmap_length)
			break;
	return depth;
}

void save_gif(image img, char *filename)
{
	Gif *gif;
	rgbmap *cmap;
	GifPicture *pic;
	GifExtension *ext = NULL;
	byte *data;
	GifBlock *block;
	int i, depth;
	image img8 = NULL;

	/* Create a blank Gif: */
	gif = new_gif();
	if (gif == NULL)
		return;

	if (img->depth > 8)
		img = img8 = convert32to8 (img);

	/* Set the screen information: */
	depth = bitsize(img->cmapsize);

	gif->screen->width      = img->width;
	gif->screen->height     = img->height;
	gif->screen->has_cmap   = 1;
	gif->screen->color_res  = 8;
	gif->screen->cmap_depth = depth;

	/* Fill the colour map: */
	cmap = gif->screen->cmap;
	cmap->length = 1 << depth;

	for (i=0; i < img->cmapsize; i++) {
		append(cmap->data, img->cmap[i]);

		if (getalpha(img->cmap[i]) != 0xFF)
			continue;	/* not transparent */

		/* Create transparent colour block: */
		if (ext)
			continue;	/* already made one */

		ext = new_gif_extension();
		ext->marker = 0xF9;
		data = NULL;
		append(data, 0x09);
		append(data, 0x00);
		append(data, 0x00);
		append(data, i);
		append(ext->data, data);

		/* Link the transparency block to the Gif: */
		block = new_gif_block();
		block->intro = 0x21;
		block->ext = ext;
		append(gif->blocks, block);
	}

	/* Fill rest of colour map with zeros: */
	for ( ; i < cmap->length; i++)
		append(cmap->data, 0x00000000UL);

	/* Create a new GifPicture: */
	pic = new_gif_picture();
	pic->width      = img->width;
	pic->height     = img->height;
	pic->has_cmap   = 0;
	pic->cmap_depth = depth;

	/* Link GifPicture data to supplied pixels: */
	pic->data = img->pixels;

	/* Link the GifPicture to the Gif: */
	block = new_gif_block();
	block->intro = 0x2C;
	block->pic   = pic;
	append(gif->blocks, block);

	/* Write the Gif file: */
	write_gif_file(filename, gif);

	/* Unlink the pixel data and clean up: */
	pic->data = NULL;
	del_gif(gif);
	delimage(img8);
}
