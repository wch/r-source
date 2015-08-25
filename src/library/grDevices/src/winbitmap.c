/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999       Guido Masarotto
 *  Copyright (C) 1999-2014  The R Core Team
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


/*
 * This file aims to be system independent so it sees the underlying
 * structures only using:
 * void *d : an 'opaque' view of the source of the pixels;
 * int width, height: dimensions in pixels;
 * unsigned int (*gp)(void *d, int x, int y): a function which
 *     returns the colour of the (x,y) pixels stored either as
 *     BGR (R model, see GraphicsDevice.h) or as RGB in the
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
#include <setjmp.h>

/* 8 bits red, green and blue channel */
#define DECLARESHIFTS int RSHIFT=(bgr)?0:16, GSHIFT=8, BSHIFT=(bgr)?16:0
#define GETRED(col)    (((col) >> RSHIFT) & 0xFF)
#define GETGREEN(col)  (((col) >> GSHIFT) & 0xFF)
#define GETBLUE(col)   (((col) >> BSHIFT) & 0xFF)
#define GETALPHA(col)   (((col) >> 24) & 0xFF)

#include <R_ext/Error.h>

#ifdef HAVE_PNG

#include "png.h"
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
#if PNG_LIBPNG_VER < 10400
    longjmp(png_ptr->jmpbuf,1);
#else
    longjmp(png_jmpbuf(png_ptr),1);
#endif
}

static void my_png_warning(png_structp png_ptr, png_const_charp msg)
{
    warning("libpng: %s",(char *) msg);
}

int R_SaveAsPng(void  *d, int width, int height,
		unsigned int (*gp)(void *, int, int),
		int bgr, FILE *fp, unsigned int transparent, int res)
{
    png_structp png_ptr;
    png_infop info_ptr;
    unsigned int  col, palette[256];
    png_color pngpalette[256];
    png_bytep pscanline;
    png_bytep scanline = (png_bytep) calloc((size_t)(4*width),sizeof(png_byte));
    png_byte trans[256];
    png_color_16 trans_values[1];
    int i, j, r, ncols, mid, high, low, withpalette, have_alpha;
    volatile DECLARESHIFTS;

    /* Have we enough memory?*/
    if (scanline == NULL)
	return 0;

    if (fp == NULL) {
	free(scanline);
	return 0;
    }

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
#if PNG_LIBPNG_VER < 10400
    if (setjmp(png_ptr->jmpbuf))
#else
    if (setjmp(png_jmpbuf(png_ptr)))
#endif
{
	/* If we get here, we had a problem writing the file */
	free(scanline);
	png_destroy_write_struct(&png_ptr, &info_ptr);
	return 0;
    }
    png_set_error_fn(png_ptr, NULL, my_png_error, my_png_warning);

    /* I/O initialization functions is REQUIRED */
    png_init_io(png_ptr, fp);
    /* Have we less than 256 different colors? */
    ncols = 0;
    if(transparent) palette[ncols++] = transparent & 0xFFFFFF;
    mid = ncols;
    withpalette = 1;
    have_alpha = 0;
    for (i = 0; (i < height) && withpalette ; i++) {
	for (j = 0; (j < width) && withpalette ; j++) {
	    col = gp(d,i,j);
	    if (GETALPHA(col) < 255) have_alpha = 1;
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
    col = gp(d,0,0);
    //have_alpha &= (transparent == 0);

    /* Set the image information here.  Width and height are up to 2^31,
     * bit_depth is one of 1, 2, 4, 8, or 16, but valid values also depend on
     * the color_type selected. color_type is one of PNG_COLOR_TYPE_GRAY,
     * PNG_COLOR_TYPE_GRAY_ALPHA, PNG_COLOR_TYPE_PALETTE, PNG_COLOR_TYPE_RGB,
     * or PNG_COLOR_TYPE_RGB_ALPHA.  interlace is either PNG_INTERLACE_NONE or
     * PNG_INTERLACE_ADAM7, and the compression_type and filter_type MUST
     * currently be PNG_COMPRESSION_TYPE_BASE and PNG_FILTER_TYPE_BASE. REQUIRED
     */
    png_set_IHDR(png_ptr, info_ptr, width, height, 8,
		 withpalette ? PNG_COLOR_TYPE_PALETTE :
		 (have_alpha ? PNG_COLOR_TYPE_RGB_ALPHA : PNG_COLOR_TYPE_RGB),
		 PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE,
		 PNG_FILTER_TYPE_BASE);

    if (withpalette) {
	for (i = 0; i < ncols ; i++) {
	    col = palette[i];
	    if(transparent) {
		trans[i] = (col == transparent) ? 0:255;
		pngpalette[i].red = GETRED(col);
		pngpalette[i].green = GETGREEN(col);
		pngpalette[i].blue = GETBLUE(col);
	    } else {
		/* PNG needs NON-premultiplied alpha */
		int a = GETALPHA(col);
		trans[i] = a;
		if(a == 255 || a == 0) {
		    pngpalette[i].red = GETRED(col);
		    pngpalette[i].green = GETGREEN(col);
		    pngpalette[i].blue = GETBLUE(col);
		} else {
		    pngpalette[i].red = 0.49 + 255.0*GETRED(col)/a;
		    pngpalette[i].green = 0.49 + 255.0*GETGREEN(col)/a;
		    pngpalette[i].blue = 0.49 + 255.0*GETBLUE(col)/a;

		}
	    }
	}
	png_set_PLTE(png_ptr, info_ptr, pngpalette, ncols);
	if (transparent || have_alpha)
	    png_set_tRNS(png_ptr, info_ptr, trans, ncols, trans_values);
    }
    /* Deal with transparency */
    if(transparent && !withpalette) {
	trans_values[0].red = GETRED(transparent);
	trans_values[0].blue = GETBLUE(transparent);
	trans_values[0].green = GETGREEN(transparent);
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
    for (i = 0 ; i < height ; i++) {
	/* Build the scanline */
	pscanline = scanline;
	for (j = 0 ; j < width ; j++) {
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
		if(have_alpha) {
		    /* PNG needs NON-premultiplied alpha */
		    int a = GETALPHA(col);
		    if(a == 255 || a == 0) {
			*pscanline++ = GETRED(col) ;
			*pscanline++ = GETGREEN(col) ;
			*pscanline++ = GETBLUE(col) ;
			*pscanline++ =  a;
		    } else {
			*pscanline++ = 0.49 + 255.0*GETRED(col)/a ;
			*pscanline++ = 0.49 + 255.0*GETGREEN(col)/a ;
			*pscanline++ = 0.49 + 255.0*GETBLUE(col)/a ;
			*pscanline++ =  a;
		    }
		} else {
		    *pscanline++ = GETRED(col) ;
		    *pscanline++ = GETGREEN(col) ;
		    *pscanline++ = GETBLUE(col) ;
		}
	    }
	}
	png_write_row(png_ptr, scanline);
    }

    /* It is REQUIRED to call this to finish writing the rest of the file */
    png_write_end(png_ptr, info_ptr);

    /* clean up after the write, and free any memory allocated */
    free(scanline);
    png_destroy_write_struct(&png_ptr, &info_ptr);

    /* that's it */
    return 1;
}

#endif /* HAVE_PNG */


#ifdef HAVE_JPEG

/* jconfig.h included by jpeglib.h may define these unconditionally */
#undef HAVE_STDDEF_H
#undef HAVE_STDLIB_H
#include <jpeglib.h>

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
		unsigned int (*gp)(void *, int, int),
		int bgr, int quality, FILE *outfile, int res)
{
    struct jpeg_compress_struct cinfo;
    struct my_error_mgr jerr;
    /* More stuff */
    JSAMPLE *pscanline, *scanline = (JSAMPLE *) calloc(3*width,sizeof(JSAMPLE));
    int i, j;
    unsigned int col;
    DECLARESHIFTS;

    /* Have we enough memory?*/
    if (scanline == NULL)
	return 0;

    if (outfile == NULL) {
	free(scanline);
	return 0;
    }

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
    cinfo.image_width = width;	/* image width and height, in pixels */
    cinfo.image_height = height;
    cinfo.input_components = 3;		/* # of color components per pixel */
    cinfo.in_color_space = JCS_RGB;	/* colorspace of input image */
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
	    col = gp(d, i, j) & 0xFFFFFF;
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

#endif /* HAVE_JPEG */

#ifdef HAVE_TIFF
#include <tiffio.h>

int R_SaveAsTIFF(void  *d, int width, int height,
		unsigned int (*gp)(void *, int, int),
		int bgr, const char *outfile, int res, int compression)
{
    TIFF *out;
    int sampleperpixel;
    tsize_t linebytes;
    unsigned char *buf, *pscanline;
    unsigned int col, i, j;
    int have_alpha = 0;

    DECLARESHIFTS;

    for (i = 0; i < height; i++)
	for (j = 0; j < width; j++) {
	    col = gp(d,i,j);
	    if (GETALPHA(col) < 255) {
		have_alpha = 1;
		break;
	    }
	}
    sampleperpixel = 3 + have_alpha;

    out = TIFFOpen(outfile, "w");
    if (!out) {
	warning("unable to open TIFF file '%s'", outfile);
	return 0;
    }
    TIFFSetField(out, TIFFTAG_IMAGEWIDTH, width);
    TIFFSetField(out, TIFFTAG_IMAGELENGTH, height);
    TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, sampleperpixel);
    TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 8);
    TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
#if 0
    /* Possible compression values
       COMPRESSION_NONE = 1;
       COMPRESSION_CCITTRLE = 2;
       COMPRESSION_CCITTFAX3 = COMPRESSION_CCITT_T4 = 3;
       COMPRESSION_CCITTFAX4 = COMPRESSION_CCITT_T6 = 4;
       COMPRESSION_LZW = 5;
       COMPRESSION_JPEG = 7;
       COMPRESSION_DEFLATE = 32946;
       COMPRESSION_ADOBE_DEFLATE = 8;
    */
    TIFFSetField(out, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
#endif
    if(compression > 1) {
	if (compression > 10) {
	    TIFFSetField(out, TIFFTAG_COMPRESSION, compression - 10);
	    TIFFSetField(out, TIFFTAG_PREDICTOR, 2);
	} else 
	    TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
    }

    if (res > 0) {
	TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
	TIFFSetField(out, TIFFTAG_XRESOLUTION, (float) res);
	TIFFSetField(out, TIFFTAG_YRESOLUTION, (float) res);
    }

    linebytes = sampleperpixel * width;
    if (TIFFScanlineSize(out))
	buf =(unsigned char *)_TIFFmalloc(linebytes);
    else
	buf = (unsigned char *)_TIFFmalloc(TIFFScanlineSize(out));

    for (i = 0; i < height; i++) {
	pscanline = buf;
	for(j = 0; j < width; j++) {
	    col = gp(d, i, j);
	    *pscanline++ = GETRED(col) ;
	    *pscanline++ = GETGREEN(col) ;
	    *pscanline++ = GETBLUE(col) ;
	    if(have_alpha) *pscanline++ = GETALPHA(col) ;
	}
	TIFFWriteScanline(out, buf, i, 0);
    }
    TIFFClose(out);
    _TIFFfree(buf);
    return 1;
}
#endif  /* HAVE_TIFF */


/*
 * Try to save the content of the device 'd' in 'filename' as Windows BMP.
 * If numbers of colors is less than 256 we use a 'palette' BMP.
 * Return 1 on success, 0 on failure
*/

#define BMPERROR {R_ShowMessage("Problems writing to 'bmp' file");return 0;}
#define BMPW(a) {wrd=a;if(fwrite(&wrd,sizeof(unsigned short),1,fp)!=1) BMPERROR}
#define BMPDW(a) {dwrd=a;if(fwrite(&dwrd,sizeof(unsigned int),1,fp)!=1) BMPERROR}
#define BMPPUTC(a) if(fputc(a,fp)==EOF) BMPERROR;
#define HEADERSIZE 54

int R_SaveAsBmp(void  *d, int width, int height,
		unsigned int (*gp)(void *, int, int), int bgr, FILE *fp,
		int res)
{
    unsigned int  col, palette[256];
    int i, j, r, ncols, mid, high, low, withpalette;
    int bfOffBits, bfSize, biBitCount, biClrUsed , pad;
    unsigned short wrd;
    unsigned int dwrd;
    int lres;
    DECLARESHIFTS;

    if (fp == NULL)
	return 0;

    /* Have we less than 256 different colors? */
    ncols = mid = 0;
    withpalette = 1;
    for (i = 0; i < 256 ; i++) palette[i] = 0;
    for (i = 0; (i < height) && withpalette ; i++) {
	for (j = 0; (j < width) && withpalette ; j++) {
	    col = gp(d,i,j) & 0xFFFFFF ;
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
    /* Compute some part of the header */
    if (withpalette) {
	bfOffBits = HEADERSIZE + 4 * 256;
	bfSize = bfOffBits + width * height ;
	biBitCount = 8;
	biClrUsed = 256;
    } else {
	bfOffBits = HEADERSIZE + 4;
	bfSize = bfOffBits + 3 * width * height ;
	biBitCount = 24;
	biClrUsed = 0;
    }

    /* write the header */

    BMPPUTC('B');BMPPUTC('M');
    BMPDW(bfSize); /*bfSize*/
    BMPW(0);BMPW(0); /* bfReserved1 and bfReserved2 must be 0*/
    BMPDW(bfOffBits); /* bfOffBits */
    BMPDW(40);	/* Windows V3. size 40 bytes */
    BMPDW(width); /* biWidth */
    BMPDW(height); /* biHeight */
    BMPW(1);	/* biPlanes - must be 1 */
    BMPW(biBitCount); /* biBitCount */
    BMPDW(0); /* biCompression=BI_RGB */
    BMPDW(0); /* biSizeImage (with BI_RGB not needed)*/
    if (res > 0)
	lres = (int)(0.5 + res/0.0254);
    else lres = 2835; // 72ppi = 2835 pixels/metre.
    BMPDW(lres); /* XPels/M */
    BMPDW(lres); /* XPels/M */
    BMPDW(biClrUsed); /* biClrUsed */
    BMPDW(0) ; /* biClrImportant All colours are important */

    /* and now the image */
    if (withpalette) {
	/* 8 bit image; write the palette */
	for (i = 0; i < 256; i++) {
	    col = palette[i];
	    BMPPUTC(GETBLUE(col));
	    BMPPUTC(GETGREEN(col));
	    BMPPUTC(GETRED(col));
	    BMPPUTC(0);
	}
	/* Rows must be padded to 4-byte boundary */
	for (pad = 0; ((width+pad) & 3) != 0; pad++);
	/* and then the pixels */
	for (i = height-1 ; i >= 0 ; i--) {
	    for (j = 0 ; j < width ; j++) {
		col = gp(d, i, j) & 0xFFFFFF;
		/* binary search the palette (the colour must be there): */
		low = 0;  high = ncols - 1;
		while (low <= high) {
		    mid = (low + high)/2;
		    if      (col < palette[mid]) high = mid - 1;
		    else if (col > palette[mid]) low  = mid + 1;
		    else break;
		}
		BMPPUTC(mid);
	    }
	    for (j = 0; j < pad; j++) BMPPUTC(0);
	}
    } else {
	/* 24 bits image */
	BMPDW(0); /* null bmiColors */
	for (pad = 0; ((3*width+pad) & 3) != 0; pad++); /*padding*/
	for (i = height-1 ; i>=0 ; i--) {
	    for (j = 0 ; j < width ; j++) {
		col = gp(d, i, j) & 0xFFFFFF;
		BMPPUTC(GETBLUE(col));
		BMPPUTC(GETGREEN(col));
		BMPPUTC(GETRED(col));
	    }
	    for (j = 0; j < pad; j++) BMPPUTC(0);
	}
    }
    return 1;
}

const char * R_pngVersion(void)
{
#ifdef HAVE_PNG
    return png_get_header_ver(NULL /*ignored*/);
#else
    return "";
#endif
}
const char * R_jpegVersion(void)
{
#ifdef HAVE_JPEG
    static char ans[10];
#ifdef JPEG_LIB_VERSION_MAJOR
    sprintf(ans, "%d.%d", JPEG_LIB_VERSION_MAJOR, JPEG_LIB_VERSION_MINOR);
#else
    sprintf(ans, "%d.%d", JPEG_LIB_VERSION/10, JPEG_LIB_VERSION%10);
#endif
    return ans;
#else
    return "";
#endif
}
const char * R_tiffVersion(void)
{
#ifdef HAVE_TIFF
    return TIFFGetVersion();
#else
    return "";
#endif
}
