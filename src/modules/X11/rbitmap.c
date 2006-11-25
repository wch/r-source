/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999, 2001, 2004  Guido Masarotto and the R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* 
 * This file aims to be system independent so it sees the underlying
 * structures only using:
 * void *d : an 'opaque' view of the source of the pixels;
 * int width, height: dimensions in pixels;
 * unsigned long (*gp)(void *d, int x, int y): a function which
 *     returns the colour of the (x,y) pixels stored either as
 *     BGR (R model, see include/Graphics.h) or as RGB in the
 *     24 least sig. bits (8 bit for channel).
 *     (0,0) is the left-top corner. (3,2) is the third pixel 
 *     in the fourth scanline.
 * int bgr: if != 0, order is BGR else is RGB.
 * int quality: only for jpeg (0-100 measure of how much to compress).
 * FILE * fp is the destination. 
 * 
 */

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_PNG
#include "png.h"
#include <setjmp.h>
#endif

/* 8 bits red, green and blue channel */
#define DECLARESHIFTS int RSHIFT=(bgr)?0:16, GSHIFT=8, BSHIFT=(bgr)?16:0
#define GETRED(col)    (((col) >> RSHIFT) & 0xFFUL)
#define GETGREEN(col)  (((col) >> GSHIFT) & 0xFFUL)
#define GETBLUE(col)   (((col) >> BSHIFT) & 0xFFUL)

#include <R_ext/Error.h>

#ifdef HAVE_PNG
/* 
 * Try to save the content of the device 'd' in 'filename' as png.
 * If numbers of colors is less than 256 we use a 'palette' png.
 * Return 1 on success, 0 on failure 
*/

/*  
    I don't use 'error' since (1) we must free 'scanline' and 
   (2) we can be arrived here from a button or menuitem callback maybe
   in a different thread from the one where R runs.
*/ 
static void my_png_error(png_structp png_ptr, png_const_charp msg) 
{
  R_ShowMessage((char *) msg);
  longjmp(png_ptr->jmpbuf,1);
}

static void my_png_warning(png_structp png_ptr, png_const_charp msg) 
{
  warning("libpng: %s",(char *) msg);
}

int R_SaveAsPng(void  *d, int width, int height, 
		unsigned long (*gp)(void *, int, int),
		int bgr, FILE *fp, unsigned int transparent, int res) 
{
  png_structp png_ptr;
  png_infop info_ptr;
  unsigned long  col, palette[256];
  png_color pngpalette[256];
  png_bytep pscanline, scanline = (png_bytep) calloc(3*width,sizeof(png_byte));
  png_byte trans[256];
  png_color_16 trans_values[1];
  int i, j, r, ncols, mid, high, low, withpalette;
  DECLARESHIFTS;

  /* Have we enough memory?*/
  if (scanline == NULL) 
    return 0;

  /* Create and initialize the png_struct with the desired error handler
   * functions.  If you want to use the default stderr and longjump method,
   * you can supply NULL for the last three parameters.  We also check that
   * the library version is compatible with the one used at compile time,
   * in case we are using dynamically linked libraries.  REQUIRED.
   */
  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (png_ptr == NULL) {
    free(scanline);
    return 0;
  }

  /* Allocate/initialize the image information data.  REQUIRED */
  info_ptr = png_create_info_struct(png_ptr);
  if (info_ptr == NULL) {
    free(scanline);
    png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
    return 0;
  }
  
  /* Set error handling.  REQUIRED if you aren't supplying your own
   * error handling functions in the png_create_write_struct() call.
   */
  if (setjmp(png_ptr->jmpbuf)) {
    /* If we get here, we had a problem writing the file */
    free(scanline);
    png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
    return 0;
  }
  png_set_error_fn(png_ptr, NULL, my_png_error, my_png_warning);
  
  /* I/O initialization functions is REQUIRED */
  png_init_io(png_ptr, fp);
  /* Have we less than 256 different colors? */
  ncols = 0;
  if(transparent) palette[ncols++] = transparent & 0xFFFFFFUL;
  mid = ncols;
  withpalette = 1;
  for (i = 0; (i < height) && withpalette ; i++) {
    for (j = 0; (j < width) && withpalette ; j++) {
      col = gp(d,i,j) & 0xFFFFFFUL ;
      /* binary search the palette: */
      low = 0;  
      high = ncols - 1;
      while (low <= high) {
	mid = (low + high)/2;
	if ( col < palette[mid] ) high = mid - 1;
	else if ( col > palette[mid] ) low  = mid + 1;
	else break;
      }
      if (high < low) {
	/* didn't find colour in palette, insert it: */
	if (ncols >= 256) {
	  withpalette = 0;
	} else {
	  for (r = ncols; r > low; r--)
	    palette[r] = palette[r-1] ;
	  palette[low] = col;
	  ncols ++;
	}
      }
    }
  }

  /* Set the image information here.  Width and height are up to 2^31,
   * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
   * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
   * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
   * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
   * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
   * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
   */
  png_set_IHDR(png_ptr, info_ptr, width, height, 8, 
	       withpalette ? PNG_COLOR_TYPE_PALETTE : PNG_COLOR_TYPE_RGB,
	       PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, 
	       PNG_FILTER_TYPE_BASE);

  if (withpalette) {
    for (i = 0; i < ncols ; i++) {
      col = palette[i];
      pngpalette[i].red = GETRED(col);
      pngpalette[i].green = GETGREEN(col);
      pngpalette[i].blue = GETBLUE(col);
    } 
    png_set_PLTE(png_ptr, info_ptr, pngpalette, ncols);
  } 
  /* Deal with transparency */
  if(transparent) {
      if(withpalette) {
	  for (i = 0; i < ncols ; i++)
	      trans[i] = (palette[i] == (transparent & 0xFFFFFFUL)) ? 0:255;
      } else {
	  trans_values[0].red = GETRED(transparent);
	  trans_values[0].blue = GETBLUE(transparent);
	  trans_values[0].green = GETGREEN(transparent);
      }
      png_set_tRNS(png_ptr, info_ptr, trans, ncols, trans_values);
  }

  if(res > 0) 
      png_set_pHYs(png_ptr, info_ptr, res/0.0254, res/0.0254,
		   PNG_RESOLUTION_METER);

  /* Write the file header information.  REQUIRED */
  png_write_info(png_ptr, info_ptr);

  /* 
   * Now, write the pixels
   */
  for (i=0 ; i<height ; i++) { 
    /* Build the scanline */
    pscanline = scanline;
    for ( j=0 ; j<width ; j++) {
      col = gp(d, i, j);
      if (withpalette) { 
	    /* binary search the palette (the colour must be there): */
	    low = 0;  high = ncols - 1;
	    while (low <= high) {
		mid = (low + high)/2;
		if      (col < palette[mid]) high = mid - 1;
		else if (col > palette[mid]) low  = mid + 1;
		else break;
	    }
	    *pscanline++ = mid;
      } else { 
	*pscanline++ = GETRED(col) ;
        *pscanline++ = GETGREEN(col) ;
        *pscanline++ = GETBLUE(col) ;
      }
    }
    png_write_row(png_ptr, scanline);
  } 

  /* It is REQUIRED to call this to finish writing the rest of the file */
  png_write_end(png_ptr, info_ptr);
  
  /* clean up after the write, and free any memory allocated */
  free(scanline);
  png_destroy_write_struct(&png_ptr, (png_infopp)NULL);

  /* that's it */
  return 1;
}
#else
int R_SaveAsPng(void  *d, int width, int height, 
		unsigned long (*gp)(void *, int, int),
		int bgr, FILE *fp, unsigned int transparent) 
{
    warning("No png support in this version of R");
    return 0;
}

#endif /* HAVE_PNG */


#ifdef HAVE_JPEG

/* jconfig.h included by jpeglib.h may define these unconditionally */
#undef HAVE_STDDEF_H
#undef HAVE_STDLIB_H
#include <jpeglib.h>
#include <setjmp.h>

/* Here's the extended error handler struct */

struct my_error_mgr {
  struct jpeg_error_mgr pub;	/* "public" fields */
  jmp_buf setjmp_buffer;	/* for return to caller */
};

typedef struct my_error_mgr * my_error_ptr;

/*
 * Here's the routine that will replace the standard error_exit method:
*/

static void my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  /* Always display the message. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}

/* We also replace the output method */
static void my_output_message (j_common_ptr cinfo)
{
  char buffer[JMSG_LENGTH_MAX];

  /* Create the message */
  (*cinfo->err->format_message) (cinfo, buffer);

  /* and show it */  
  R_ShowMessage(buffer);
}



int R_SaveAsJpeg(void  *d, int width, int height, 
		unsigned long (*gp)(void *, int, int),
		int bgr, int quality, FILE *outfile, int res) 
{
  struct jpeg_compress_struct cinfo;
  struct my_error_mgr jerr;
  /* More stuff */
  JSAMPLE *pscanline, *scanline = (JSAMPLE *) calloc(3*width,sizeof(JSAMPLE));
  int i, j;
  unsigned long col;
  DECLARESHIFTS;

  /* Have we enough memory?*/
  if (scanline == NULL) 
    return 0;

  /* Step 1: allocate and initialize JPEG compression object */

  /* 
   * We set up the normal JPEG error routines, then override error_exit
   * and output_message
  */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit ;
  jerr.pub.output_message = my_output_message ;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
    /* If we get here, the JPEG code has signaled an error.
     * We need to clean up the JPEG object, close the input file, and return.
     */
    jpeg_destroy_compress(&cinfo);
    free(scanline);
    if (outfile) fclose(outfile);
    return 0;
  }
  /* Now we can initialize the JPEG compression object. */
  jpeg_create_compress(&cinfo);

  /* Step 2: specify data destination (eg, a file) */
  jpeg_stdio_dest(&cinfo, outfile);

  /* Step 3: set parameters for compression */
  /* First we supply a description of the input image.
   * Four fields of the cinfo struct must be filled in:
   */
  cinfo.image_width = width; 	/* image width and height, in pixels */
  cinfo.image_height = height;
  cinfo.input_components = 3;		/* # of color components per pixel */
  cinfo.in_color_space = JCS_RGB; 	/* colorspace of input image */
  jpeg_set_defaults(&cinfo);
  if(res > 0) {
      cinfo.density_unit = 1;  /* pixels per inch */
      cinfo.X_density = res;
      cinfo.Y_density = res;
  }
  jpeg_set_quality(&cinfo, quality, TRUE);
  /* Step 4: Start compressor */
  jpeg_start_compress(&cinfo, TRUE);

  /* Step 5: while (scan lines remain to be written) */
  /*           jpeg_write_scanlines(...); */
  for (i=0 ; i<height ; i++) { 
  /* Build the scanline */
    pscanline = scanline;
    for ( j=0 ; j<width ; j++) {
      col = gp(d, i, j);
      *pscanline++ = GETRED(col) ;
      *pscanline++ = GETGREEN(col) ;
      *pscanline++ = GETBLUE(col) ;
    }
    jpeg_write_scanlines(&cinfo, (JSAMPARRAY) &scanline, 1);
  } 

  /* Step 6: Finish compression */

  jpeg_finish_compress(&cinfo);

  /* Step 7: release JPEG compression object */

  /* This is an important step since it will release a good deal of memory. */
  free(scanline);
  jpeg_destroy_compress(&cinfo);


  /* And we're done! */
  return 1;
}

#else
int R_SaveAsJpeg(void  *d, int width, int height, 
		unsigned long (*gp)(void *, int, int),
		int bgr, int quality, FILE *outfile) 
{
    warning("No jpeg support in this version of R");
    return 0;
}
#endif /* HAVE_JPEG */

