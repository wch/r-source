/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: image.c -- cross-platform image type.
 * Platform: Neutral  Version: 2.40  Date: 1998/05/05
 *
 * Version: 2.30  Changes: Original version by Lachlan Patrick.
 * Version: 2.40  Changes: Faster drawing, image scaling and cropping.
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
 *  Image
 *  -----
 *  An image is a platform-indepedent representation of a
 *  rectangular picture, in RGB colour format. There are two
 *  possible formats the picture can use: 8-bit indexed colour,
 *  and 32-bit true colour (RGB plus alpha channel).
 *
 *  An image in 8-bit format has the following properties:
 *    - depth is set to 8
 *    - pixels is an array of (width*height) bytes
 *    - cmapsize is set to a non-zero value
 *    - cmap is an array of cmapsize rgb values
 *
 *  An image in 32-bit format has the following properties:
 *    - depth is set to 32
 *    - pixels is an array of (width * height) rgb values
 *    - cmapsize is zero and cmap is NULL (an empty array)
 *
 *  Any other depth is deliberately not supported.
 *  Transparency is handled in the alpha channel of each rgb value
 *  (either inside the cmap, or in the pixels array itself).
 */

#include "internal.h"

/*
 *  Create a new image:
 */
image newimage(int width, int height, int depth)
{
	image img;

	if ((depth != 8) && (depth != 32))
		return NULL;

	img = create(struct imagedata);

	if (! img)
		return img;

	img->width  = width;
	img->height = height;

	if (depth == 8) {
		img->depth  = 8;
		img->pixels = array(width*height, byte);
	}
	else {
		img->depth  = 32;
		img->pixels = (byte *) array(width*height, rgb);
	}

	return img;
}

/*
 *  Make a new copy of an image:
 */
image copyimage(image img)
{
	image new_img;

	if (! img)
		return img;

	new_img = newimage(img->width, img->height, img->depth);
	setpixels(new_img, img->pixels);
	setpalette(new_img, img->cmapsize, img->cmap);

	return new_img;
}

/*
 *  Delete an image:
 */
void delimage(image img)
{
	if (img) {
		discard(img->cmap);
		discard(img->pixels);
		discard(img);
	}
}

/*
 *  Print an image:
 */
#if DEBUG
PROTECTED
void printimage(FILE *file, image img)
{
	int i;
	long w, h, n;

	fprintf(file, "Pixmap:\n");
	fprintf(file, " depth     = %d\n", img->depth);
	fprintf(file, " width     = %d\n", img->width);
	fprintf(file, " height    = %d\n", img->height);
	fprintf(file, " cmapsize = %d\n", img->cmapsize);
	fprintf(file, " cmap:\n");
	for (i=0; i < img->cmapsize; i++)
		fprintf(file, "  %-02.2X = %-08.8lX\n", i, img->cmap[i]);
	fprintf(file, " pixels:\n");
	for (h=0; h < img->height; h++) {
		fprintf(file, "  [");
		for (w=0; w < (img->width * img->depth / 8); w++) {
			n = h * (img->width * img->depth / 8) + w;
			fprintf(file, "%-02.2X", img->pixels[n]);
		}
		fprintf(file, "]\n");
	}
}
#endif

/*
 *  Discover the dimensions of the image:
 */

int imagedepth(image img)  { return ((img)?(img->depth):0);}
int imagewidth(image img)  { return ((img)?(img->width):0);}
int imageheight(image img) { return ((img)?(img->height):0);}

/*
 *  Change an image's pixels:
 */
void setpixels(image img, byte pixels[])
{
	long i, length;

	if (! img)
		return;

	length = img->width * img->height;
	if (img->depth > 8)
		length = length * sizeof(rgb);

	for (i=0; i < length; i++)
		img->pixels[i] = pixels[i];
}

/*
 *  Return an image's pixel array:
 */
byte * getpixels(image img)
{
	if (img)
		return img->pixels;
	else
		return NULL;
}

/*
 *  Change an image's cmap:
 */
void setpalette(image img, int cmapsize, rgb *cmap)
{
	int i;

	if (! img)
		return;

	discard(img->cmap);

	img->cmapsize = cmapsize;
	img->cmap = array(cmapsize, rgb);

	for (i=0; i < cmapsize; i++)
		img->cmap[i] = cmap[i];
}

/*
 *  Return information about an image's cmap:
 */
rgb * getpalette(image img)
{
	if (img)
		return img->cmap;
	else
		return NULL;
}

int getpalettesize(image img)
{
	if (img)
		return img->cmapsize;
	else
		return 0;
}

/*
 *  Try to generate an 8-bit version of an image:
 *  If there are less than 256 unique colours in a 32-bit
 *  image, this routine will return the corresponding
 *  indexed 8-bit image.
 *  Returns NULL if more than 256 colours are found.
 */
static image fast_find_cmap (image img)
{
	image new_img;
	long   i, j, length;
	rgb *  pixel32;
	byte * pixel8;
	int    cmapsize, low, high, mid;
	rgb    col;
	rgb    cmap[256];

	pixel32 = (rgb *) img->pixels;

	/* the first colour goes into the cmap automatically: */
	length = img->width * img->height;
	cmapsize = 0;  mid = 0;

	for (i=0; i < length; i++)
	{
		col = *pixel32 ++;
		/* only allow one transparent colour in the cmap: */
		if (col & 0xF0000000UL)
			col  = 0xFFFFFFFFUL;	/* transparent */
		else
			col &= 0x00FFFFFFUL;	/* opaque */

		/* binary search the cmap: */
		low = 0;  high = cmapsize - 1;
		while (low <= high) {
			mid = (low+high)/2;
			if      (col < cmap[mid]) high = mid - 1;
			else if (col > cmap[mid]) low  = mid + 1;
			else break;
		}

		if (high < low) {
			/* didn't find colour in cmap, insert it: */
			if (cmapsize >= 256)
				return NULL;
			for (j=cmapsize; j > low; j--)
				cmap[j] = cmap[j-1];
			cmap[low] = col;
			cmapsize ++;
		}
	}

	/* now create the 8-bit indexed image: */

	new_img = newimage(img->width, img->height, 8);
	if (! new_img)
		return new_img;
	setpalette(new_img, cmapsize, cmap);

	/* now convert each 32-bit pixel into an 8-bit pixel: */

	pixel32 = (rgb *) img->pixels;
	pixel8 = (byte *) new_img->pixels;

	for (i=0; i < length; i++)
	{
		col = *pixel32 ++;

		/* only allow one transparent colour in the cmap: */
		if (col & 0xF0000000UL)
			col  = 0xFFFFFFFFUL;	/* transparent */
		else
			col &= 0x00FFFFFFUL;	/* opaque */

		/* binary search the cmap (the colour must be there): */
		low = 0;  high = cmapsize - 1;
		while (low <= high) {
			mid = (low+high)/2;
			if      (col < cmap[mid]) high = mid - 1;
			else if (col > cmap[mid]) low  = mid + 1;
			else break;
		}

		if (high < low) {
			/* impossible situation */
			delimage(new_img);
			return NULL;
		}

		*pixel8 = mid;  pixel8 ++;
	}

	return new_img;
}

/*
 *  Try to generate an 8-bit version of an image:
 *  This routine will approximate a 32-bit image using a
 *  7x7x5 colour cube.
 *  If it runs out of memory, it returns NULL.
 */
static image fast_generate_cmap (image img)
{
	image new_img;
	long   i, length, col;
	int    r, g, b, value;
	rgb *  pixel32;
	byte * pixel8;
	rgb    cmap[256];

	/* Generate the colour map: */

	for (r=0; r<7; r++)		/* 7x7x5 colour cube */
	  for (g=0; g<7; g++)
	    for (b=0; b<5; b++)
		cmap [r*35 + g*5 + b] = rgb(r,g,b);

	for (i=0; i<9; i++)		/* greyscale ramp */
	{
		value = 255 * i / 8;
		cmap [254 - 8 + i] = rgb(value, value, value);
	}

	cmap [255] = 0xFFFFFFFFUL;	/* transparent */

	/* Generate the 8-bit indexed image: */

	new_img = newimage(img->width, img->height, 8);
	if (! new_img)
		return new_img;
	setpalette(new_img, 256, cmap);

	/* Translate the pixels from 32-bit to 8-bit: */

	length = img->width * img->height;
	pixel32 = (rgb *) img->pixels;
	pixel8 = (byte *) new_img->pixels;

	for (i=0; i < length; i++) {
		col = *pixel32 ++;
		r = getred(col);
		g = getgreen(col);
		b = getblue(col);

		if (getalpha(col) > 0x7F)  /* transparent */
			value = 255;

		else if ((r == g) && (r == b))	/* grey */
		{
			r = (r + 16) / 32;
			if (r == 0)
				value = 0;	/* black */
			else
				value = 254 - 8 + r;
		}

		else	/* map to 7x7x5 colour cube */
		{
			r = (r + 21) / 42;
			g = (g + 21) / 42;
			b = (b + 32) / 64;

			value = r*35 + g*5 + b;
		}

		*pixel8 = value;  pixel8 ++;
	}

	return new_img;
}

/*
 *  Try to generate an 8-bit version of a 32-bit image:
 *  Return NULL on failure.
 */
image convert32to8 (image img)
{
	image new_img;

	if (! img)
		return img;

	if (img->depth <= 8)
		return copyimage(img);

	new_img = fast_find_cmap(img);
	if (! new_img)
		new_img = fast_generate_cmap(img);

	return new_img;
}

/*
 *  Try to generate a 32-bit version of an 8-bit image:
 *  Return NULL if there is no memory left.
 */
image convert8to32 (image img)
{
	image new_img;
	long i;
	rgb *pixel32;
	byte *pixel8;
	byte value;

	if (! img)
		return img;

	new_img = newimage(img->width, img->height, 32);
	if (! new_img)
		return new_img;

	pixel32 = (rgb *) new_img->pixels;
	pixel8 = (byte *) img->pixels;

	for (i=img->width * img->height; i; i--) {
		value = *pixel8 ++;
		if (value >= img->cmapsize)
			value = img->cmapsize - 1;
		*pixel32 ++ = img->cmap[value];
	}

	return new_img;
}

/*
 *  Sort an image's colour map, eliminating redudancies.
 *  This operation transforms an existing image.
 */

typedef int (*qsort_func)(const void *a, const void *b);

static int compare_freq(long *a, long *b)
{
	long freq_a, freq_b;
	int value_a, value_b;

	freq_a = (*a) >> 8;
	freq_b = (*b) >> 8;
	value_a = (*a) & 0x00FF;
	value_b = (*b) & 0x00FF;

	if (freq_a < freq_b)        return (+1);
	else if (freq_a > freq_b)   return (-1);
	else                        return (value_a - value_b);
}

void sortpalette(image img)
{
	long	i, j, length;
	int 	old_value, new_value;
	rgb 	col;
	int 	new_size;
	long *	histogram;
	byte *	translate;
	rgb *	new_cmap;

	if (! img)
		return;
	if (img->depth > 8)
		return;

	histogram = array(256, long);
	translate = array(256, byte);

	/* Generate a colour histogram: */
	length = img->width * img->height;
	for (i=0; i < length; i++)
		histogram[img->pixels[i]] ++;

	/* Place colour indexes in low byte of histogram: */
	for (i=0; i < 256; i++) {
		histogram[i] <<= 8;
		histogram[i] |= i;
	}

	/* Sort the histogram in decreasing frequency order: */
	qsort(histogram, 256, sizeof(long), (qsort_func) compare_freq);

	/* Generate a colour translation table: */
	new_size = img->cmapsize;

	for (i=255; i >= 0; i--)
	{
		old_value = histogram[i] & 0x00FFL;
		new_value = i;
		col = img->cmap[i];

		/* coalesce identical colours in cmap */
		for (j=i-1; j >= 0; j--) {
			if (img->cmap[j] == col)
				new_value = j;
		}

		translate[old_value] = new_value;

		/* find smallest useless colour */
		if ((histogram[i] >> 8) == 0)
			new_size = i;
	}

	/* Generate a sorted colour map: */
	new_cmap = array(new_size, rgb);

	for (i=0; i < new_size; i++) {
		old_value = histogram[i] & 0x00FFL;
		new_value = i;
		new_cmap[new_value] = img->cmap[old_value];
	}

	/* Change the existing colour map: */
	img->cmapsize = new_size;
	for (i=0; i < new_size; i++)
		img->cmap[i] = new_cmap[i];
	discard(new_cmap);

	/* Translate the pixels to the new colour map: */
	for (i=0 ; i < length; i++)
		img->pixels[i] = translate[img->pixels[i]];

	/* Clean up and return new image: */
	discard(translate);
	discard(histogram);
}

/*
 *  Load and save images (utility functions):
 */
static int string_ends_with(char *name, char *ending)
{
	int i, j, result;

	if (name == NULL)
		return (ending == NULL);

	result = 1;
	for (i=0; name[i]; )
		i = i + 1;
	for (j=0; ending[j]; )
		j = j + 1;
	while (result && (i >= 0) && (j >= 0)) {
		if (tolower(name[i]) != tolower(ending[j]))
			result = 0;
		i = i - 1;
		j = j - 1;
	}
	if ((i == 0) && (j > 0))
		result = 0;
	return result;
}

static unsigned char read_hex_byte(FILE *file)
{
	int ch;
	int i, value;
	unsigned char result = 0;

	ch = 0;
	while (ch != EOF) {
		if ((ch = getc(file)) != '0') continue;
		if ((ch = getc(file)) != 'x') continue;

		for (i=0; i<2; i++) {
			ch = getc(file);
			if (isdigit(ch))
				value = ch - '0';
			else if (isalpha(ch))
				value = tolower(ch) - 'a' + 10;
			else
				return result;
			result = (result << 4) | value;
		}
		break;
	}
	return result;
}

static unsigned long read_hex_long(FILE *file)
{
	int ch;
	int i, value;
	unsigned long result = 0;

	ch = 0;
	while (ch != EOF) {
		if ((ch = getc(file)) != '0') continue;
		if ((ch = getc(file)) != 'x') continue;

		for (i=0; i<8; i++) {
			ch = getc(file);
			if (isdigit(ch))
				value = ch - '0';
			else if (isalpha(ch)) {
				ch = tolower(ch);
				if (ch > 'f')
					return result;
				value = ch - 'a' + 10;
			}
			else
				return result;
			result = (result << 4) | value;
		}
		break;
	}
	return result;
}

static char * header_comment   = "/* GraphApp image type 1 */\n";
static char * depth_comment    = "/* depth  = %d */\n";
static char * width_comment    = "/* width  = %d */\n";
static char * height_comment   = "/* height = %d */\n";
static char * cmapsize_comment = "/* cmapsize = %d */\n";

static image load_header_image_file(FILE *file)
{
	char  line[100];
	long  i, size;
	int   width = 0, height = 0, depth = 8;
	int   cmapsize = 0;
	rgb * cmap = NULL;
	rgb  *pixel32;
	byte *pixel8;
	image img = NULL;

	if (file == NULL)
		return NULL;

	if (fgets(line, sizeof(line)-2, file) == NULL)
		return NULL;
	if (strcmp(line, header_comment))
		return NULL;

	for (i=0; i<4; i++) {
		if (fgets(line, sizeof(line)-2, file) == NULL)
			return NULL;
		if (! strncmp(line, depth_comment, 12))
			depth = atoi(line+12);
		if (! strncmp(line, width_comment, 12))
			width = atoi(line+12);
		if (! strncmp(line, height_comment, 12))
			height = atoi(line+12);
		if (! strncmp(line, cmapsize_comment, 14))
			cmapsize = atoi(line+14);
	}

	img = newimage(width, height, depth);
	if (img == NULL)
		return NULL;

	if (depth <= 8) {
		if (fgets(line, sizeof(line)-2, file) == NULL)
			return NULL;
		if (strncmp(line, "rgb ", 4) != 0) {
			delimage(img);
			return NULL;
		}
		for (i=0; i<cmapsize; i++) {
			append(cmap, read_hex_long(file));
		}
		if (fgets(line, sizeof(line)-2, file) == NULL)
			return NULL;

		img->cmapsize = cmapsize;
		img->cmap = cmap;
	}

	pixel32 = (rgb *) img->pixels;
	pixel8  = img->pixels;
	size    = (long) width * height;

	if (fgets(line, sizeof(line)-2, file) == NULL)
		return NULL;

	if (depth <= 8) {
		for (i=0; i<size; i++)
			pixel8[i] = read_hex_byte(file);
	}
	else {
		for (i=0; i<size; i++)
			pixel32[i] = read_hex_long(file);
	}

	return img;
}

static void save_header_image_file(FILE *file, char *name, image img)
{
	long i, size;
	int width;
	rgb * pixel32;
	byte *pixel8;

	if (file == NULL)
		return;
	if (img == NULL)
		return;

	fprintf(file, header_comment);
	fprintf(file, depth_comment,  img->depth);
	fprintf(file, width_comment,  img->width);
	fprintf(file, height_comment, img->height);
	fprintf(file, cmapsize_comment, img->cmapsize);

	if (img->depth <= 8) {
		fprintf(file, "rgb %s_cmap [] = {\n", name);
		for (i=0; i<img->cmapsize; i++)
			fprintf(file, "\t0x%-8.8lXUL,\n", img->cmap[i]);
		fprintf(file, "};\n");
	}

	pixel32 = (rgb *) img->pixels;
	pixel8  = img->pixels;
	size    = (long) img->width * img->height;
	width   = img->width;

	if (img->depth <= 8) {
		fprintf(file, "byte %s_pixels [] = {", name);
		if (width > 12) width = 12;
		for (i=0; i<size; i++) {
			if ((i%width) == 0)
				fprintf(file, "\n  ");
			fprintf(file, "0x%-2.2X, ", pixel8[i]);
		}
		fprintf(file, "\n};\n");
	}
	else {
		fprintf(file, "rgb %s_pixels [] = {", name);
		if (width > 5) width = 5;
		for (i=0; i<size; i++) {
			if ((i%width) == 0)
				fprintf(file, "\n  ");
			fprintf(file, "0x%-8.8lXUL, ", pixel32[i]);
		}
		fprintf(file, "\n};\n");
	}

	fprintf(file, "imagedata %s_imagedata = {\n", name);
	fprintf(file, "\t%d,\t/* depth */\n",  img->depth);
	fprintf(file, "\t%d,\t/* width */\n",  img->width);
	fprintf(file, "\t%d,\t/* height */\n", img->height);
	fprintf(file, "\t%d,\t/* cmapsize */\n", img->cmapsize);
	if (img->depth <= 8) {
		fprintf(file, "\t%s_cmap,\n", name);
		fprintf(file, "\t%s_pixels\n", name);
	}
	else {
		fprintf(file, "\t(rgb *) 0\n");
		fprintf(file, "\t(byte *) %s_pixels\n", name);
	}
	fprintf(file, "};\n");
	fprintf(file, "image %s_image = & %s_imagedata;\n",
			name, name);
	fprintf(file, "\n");
}

static image load_header_image(char *filename)
{
	FILE *file;
	image img;

	file = fopen(filename, "r");
	img = load_header_image_file(file);
	fclose(file);
	return img;
}

static char * base_file_name(char *filename)
{
	char *name = NULL;
	int i, start, end;

	end = strlen(filename);
	while (filename[end] != '.')
		end--;
	for (start=end; start > 0; start--) {
		if ((filename[start] == '\\')
		 || (filename[start] == '/')
		 || (filename[start] == ':')) {
			start ++;
			break;
		}
	}
	for (i=start; i < end; i++)
		append(name, tolower(filename[i]));
	return name;
}

static void save_header_image(image img, char *filename)
{
	FILE *file;
	char *name;

	file = fopen(filename, "w");
	name = base_file_name(filename);
	save_header_image_file(file, name, img);
	discard(name);
	fclose(file);
}

/*
 *  Top-level functions for loading and saving images:
 */
image loadimage(char *filename)
{
	if (string_ends_with(filename, ".gif"))
		return load_gif(filename);
	else if (string_ends_with(filename, ".h"))
		return load_header_image(filename);
	else if (string_ends_with(filename, ".img"))
		return load_header_image(filename);
	else
		return NULL;
}

void saveimage(image img, char *filename)
{
	if (string_ends_with(filename, ".gif"))
		save_gif(img, filename);
	else if (string_ends_with(filename, ".h"))
		save_header_image(img, filename);
	else if (string_ends_with(filename, ".img"))
		save_header_image(img, filename);
}

/*
 *  Changing an rgb's value:
 */

rgb darker(rgb pixel)
{
	int r, g, b;

	if (getalpha(pixel) > 0x7F)
		return Transparent;

	r = getred(pixel);
	g = getgreen(pixel);
	b = getblue(pixel);

	return rgb((r+1)*3/4,(g+1)*3/4,(b+1)*3/4);
}

rgb brighter(rgb pixel)
{
	int r, g, b;

	if (getalpha(pixel) > 0x7F)
		return Transparent;

	r = getred(pixel);
	g = getgreen(pixel);
	b = getblue(pixel);

	r = (r) * 4 / 3; if (r > 255) r = 255;
	g = (g) * 4 / 3; if (g > 255) g = 255;
	b = (b) * 4 / 3; if (b > 255) b = 255;
	return rgb(r,g,b);
}

static rgb monochrome(rgb pixel)
{
	int min, max, g, b;

	if (getalpha(pixel) > 0x7F)
		return Transparent;

	max = min = getred(pixel);
	g = getgreen(pixel);
	if      (g < min) min = g;
	else if (g > max) max = g;
	b = getblue(pixel);
	if      (b < min) min = b;
	else if (b > max) max = b;

	if (min > 0xE0) 	pixel = White;
	else if (max < 0x10)	pixel = Black;
	else if (max < 0x60)	pixel = Black;
	else if (max < 0xD0)	pixel = Black;
	else			pixel = White;

	return pixel;
}

static rgb greyscale(rgb pixel)
{
	int min, max, g, b;

	if (getalpha(pixel) > 0x7F)
		return Transparent;

	max = min = getred(pixel);
	g = getgreen(pixel);
	if      (g < min) min = g;
	else if (g > max) max = g;
	b = getblue(pixel);
	if      (b < min) min = b;
	else if (b > max) max = b;

	if (min > 0xE0) 	pixel = White;
	else if (max < 0x10)	pixel = Black;
	else if (max < 0x60)	pixel = DarkGrey;
	else if (max < 0xD0)	pixel = Grey;
	else			pixel = LightGrey;

	return pixel;
}

/*
 *  Determine pixel values from an image:
 */

PROTECTED
rgb get_image_pixel(image img, int x, int y)
{
	int value;
	rgb pixel;

	if ((x < 0) || (x >= img->width))	return Transparent;
	if ((y < 0) || (y >= img->height))	return Transparent;

	if (img->depth <= 8) {
		value = img->pixels[y*img->width + x];
		pixel = img->cmap[value];
	} else {
		pixel = ((rgb *)(img->pixels))[y*img->width + x];
	}

	if (getalpha(pixel) > 0x7F) return Transparent;
	return (pixel & White);
}

PROTECTED
rgb get_monochrome_pixel(image img, int x, int y)
{
	return monochrome(get_image_pixel(img, x, y));
}

PROTECTED
rgb get_grey_pixel(image img, int x, int y)
{
	return greyscale(get_image_pixel(img, x, y));
}

/*
 *  Return an image scaled to a new width and/or height.
 *  The source rectangle sr can be used to crop the source image.
 *  The returned image should be deleted using del() when
 *  it is no longer needed.
 */

static void scale_8_bit_image(image dest, image src, rect dr, rect sr)
{
	int value;
	long x, y;
	long dx, dy, sx, sy;
	long dw, dh, sw, sh;
	byte * src_pixels = src->pixels;
	byte * dest_pixels = dest->pixels;

	dw = dest->width;
	dh = dest->height;
	sw = src->width;
	sh = src->height;

	for (y=0; y < dr.height; y++)
	  for (x=0; x < dr.width; x++) {
		sy = sr.y + y * sr.height / dr.height;
		sx = sr.x + x * sr.width / dr.width;
		if ((sx >= 0) && (sx < sw) && (sy >= 0) && (sy < sh))
			value = src_pixels [sy * sw + sx];
		else
			value = 0;
		dy = dr.y + y;
		dx = dr.x + x;
		if ((dx >= 0) && (dx < dw) && (dy >= 0) && (dy < dh))
			dest_pixels [dy * dw + dx] = value;
	  }
}

static void scale_32_bit_image(image dest, image src, rect dr, rect sr)
{
	rgb value;
	long x, y;
	long dx, dy, sx, sy;
	long dw, dh, sw, sh;
	rgb * src_pixels = (rgb *) src->pixels;
	rgb * dest_pixels = (rgb *) dest->pixels;

	dw = dest->width;
	dh = dest->height;
	sw = src->width;
	sh = src->height;

	for (y=0; y < dr.height; y++)
	  for (x=0; x < dr.width; x++) {
		sy = sr.y + y * sr.height / dr.height;
		sx = sr.x + x * sr.width / dr.width;
		if ((sx >= 0) && (sx < sw) && (sy >= 0) && (sy < sh))
			value = src_pixels [sy * sw + sx];
		else
			value = 0;
		dy = dr.y + y;
		dx = dr.x + x;
		if ((dx >= 0) && (dx < dw) && (dy >= 0) && (dy < dh))
			dest_pixels [dy * dw + dx] = value;
	  }
}

image scaleimage(image src, rect dr, rect sr)
{
	image dest;

	if (! src)
		return NULL;

	dest = newimage(dr.width, dr.height, src->depth);
	if (! dest)
		return NULL;

	if (src->depth == 8) {
		setpalette(dest, src->cmapsize, src->cmap);
		scale_8_bit_image(dest, src, dr, sr);
	}
	else if (src->depth == 32) {
		scale_32_bit_image(dest, src, dr, sr);
	}
	else {
		del(dest);
		dest = NULL;
	}

	return dest;
}

/*
 *  Functions for drawing an image:
 */

static int get_mono_pixval(image src, image dest, int x, int y)
{
	int i;
	rgb pixel = get_monochrome_pixel(src, x, y);

	for (i=0; i < dest->cmapsize; i++)
		if (pixel == dest->cmap[i])
			return i;
	return dest->cmapsize - 1;
}

static int get_grey_pixval(image src, image dest, int x, int y)
{
	int i;
	rgb pixel = get_grey_pixel(src, x, y);

	for (i=0; i < dest->cmapsize; i++)
		if (pixel == dest->cmap[i])
			return i;
	return dest->cmapsize - 1;
}

void drawmonochrome(image src, rect dr, rect sr)
{
	image dest;
	int x, y;
	rgb cmap[3] = {Black, White, Transparent};

	if (! src)
		return;

	dest = newimage(src->width, src->height, 8);
	setpalette(dest, 3, cmap);

	for (y=0; y < dest->height; y++)
	  for (x=0; x < dest->width; x++)
		dest->pixels[y * dest->width + x]
			= get_mono_pixval(src, dest, x, y);
	drawimage(dest, dr, sr);
	del(dest);
}

void drawgreyscale(image src, rect dr, rect sr)
{
	image dest;
	int x, y;
	rgb cmap[6] = {Black, DarkGrey, Grey, LightGrey, White, Transparent};

	if (! src)
		return;

	dest = newimage(src->width, src->height, 8);
	setpalette(dest, 6, cmap);

	for (y=0; y < dest->height; y++)
	  for (x=0; x < dest->width; x++)
		dest->pixels[y * dest->width + x]
			= get_grey_pixval(src, dest, x, y);
	drawimage(dest, dr, sr);
	del(dest);
}

void drawdarker(image src, rect dr, rect sr)
{
	image dest = NULL;
	int i, x, y;
	rgb *newcmap = NULL, *oldcmap = NULL;
	rgb *pixels;

	if (! src)
		return;

	if (src->depth == 8) {
		newcmap = array(src->cmapsize, rgb);
		for (i=0; i < src->cmapsize; i++)
			newcmap[i] = darker(src->cmap[i]);
		oldcmap = src->cmap;
		src->cmap = newcmap;
		dest = src;
	}
	else if (src->depth == 32) {
		dest = newimage(src->width, src->height, src->depth);
		if (dest) {
		  pixels = (rgb *) dest->pixels;
		  for (y=0; y < dest->height; y++)
		    for (x=0; x < dest->width; x++)
			pixels[y * dest->width + x]
				= darker(get_image_pixel(src, x, y));
		} else {
			dest = src;
		}
	}
	drawimage(dest, dr, sr);
	if (dest != src)
		del(dest);
	if (src->cmap == newcmap)
		src->cmap = oldcmap;
	discard(newcmap);
}

void drawbrighter(image src, rect dr, rect sr)
{
	image dest = NULL;
	int i, x, y;
	rgb *newcmap = NULL, *oldcmap = NULL;
	rgb *pixels;

	if (! src)
		return;

	if (src->depth == 8) {
		newcmap = array(src->cmapsize, rgb);
		for (i=0; i < src->cmapsize; i++)
			newcmap[i] = brighter(src->cmap[i]);
		oldcmap = src->cmap;
		src->cmap = newcmap;
		dest = src;
	}
	else if (src->depth == 32) {
		dest = newimage(src->width, src->height, src->depth);
		if (dest) {
		  pixels = (rgb *) dest->pixels;
		  for (y=0; y < dest->height; y++)
		    for (x=0; x < dest->width; x++)
			pixels[y * dest->width + x]
				= brighter(get_image_pixel(src,x,y));
		} else {
			dest = src;
		}
	}
	drawimage(dest, dr, sr);
	if (dest != src)
		del(dest);
	if (src->cmap == newcmap)
		src->cmap = oldcmap;
	discard(newcmap);
}
