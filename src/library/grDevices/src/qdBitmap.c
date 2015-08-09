/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-9  The R Foundation
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
 *
 *  Bitmap output Quartz device module
 *
 *  Original author: Byron Ellis
 *
 *  This file should be compiled only if AQUA is enabled
 */


#include "qdBitmap.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/QuartzDevice.h>
#define _(String) (String)

typedef struct {
    CGContextRef bitmap;	/* Bitmap drawing context */
    char *uti;			/* Type of bitmap to produce */
    char *path;			/* Path for file save during close (can be NULL) */
    int page;			/* current page number increased by NewPage (0 right after init) */
    unsigned int length;	/* Size of the bitmap */
    char data[1];		/* Actual bitmap bytes */
} QuartzBitmapDevice;

static QuartzFunctions_t *qf;

CGContextRef QuartzBitmap_GetCGContext(QuartzDesc_t dev, void *userInfo)
{
    return ((QuartzBitmapDevice*) userInfo)->bitmap;
}

void QuartzBitmap_Output(QuartzDesc_t dev, QuartzBitmapDevice *qbd)
{
    if(qbd->path && qbd->uti) {
        /* On 10.4+ we can employ the CGImageDestination API to create a
           variety of different bitmap formats */
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
	char buf[PATH_MAX+1];
	snprintf(buf, PATH_MAX, qbd->path, qbd->page); buf[PATH_MAX] = '\0';
        CFStringRef pathString = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) buf, strlen(buf), kCFStringEncodingUTF8, FALSE);
        CFURLRef path;
        if(CFStringFind(pathString, CFSTR("://"), 0).location != kCFNotFound) {
            CFStringRef pathEscaped = CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault, pathString, NULL, NULL, kCFStringEncodingUTF8);
            path = CFURLCreateWithString(kCFAllocatorDefault, pathEscaped, NULL);
            CFRelease(pathEscaped);
        } else {
            path = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (const UInt8*) buf, strlen(buf), FALSE);
        }
        CFRelease(pathString);

        CFStringRef scheme = CFURLCopyScheme(path);
       	CFStringRef type  = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) qbd->uti, strlen(qbd->uti), kCFStringEncodingUTF8, FALSE);
    	CGImageRef image = CGBitmapContextCreateImage(qbd->bitmap);
        if(CFStringCompare(scheme,CFSTR("file"), 0) == 0) { /* file output */
            CGImageDestinationRef dest = CGImageDestinationCreateWithURL(path, type, 1, NULL);
	    if(dest) {
		CGImageDestinationAddImage(dest, image, NULL);
		CGImageDestinationFinalize(dest);
		CFRelease(dest);
	    } else 
		error(_("QuartzBitmap_Output - unable to open file '%s'"), buf);
        } else if(CFStringCompare(scheme, CFSTR("clipboard"), 0) == 0) { /* clipboard output */
            CFMutableDataRef      data = CFDataCreateMutable(kCFAllocatorDefault, 0);
            CGImageDestinationRef dest = CGImageDestinationCreateWithData(data, type, 1, NULL);
            CGImageDestinationAddImage(dest, image, NULL);
            CGImageDestinationFinalize(dest);
            CFRelease(dest);
            PasteboardRef pb = NULL;
            if(PasteboardCreate(kPasteboardClipboard, &pb) == noErr) {
                PasteboardClear(pb);
                PasteboardSynchronize(pb);
                PasteboardPutItemFlavor(pb, (PasteboardItemID) 1, type, data, 0);
            }
            CFRelease(data);
        } else
            warning(_("not a supported scheme, no image data written"));
        CFRelease(scheme);
       	CFRelease(type);
        CFRelease(path);
        CFRelease(image);
#endif
    }
}

void QuartzBitmap_NewPage(QuartzDesc_t dev, void *userInfo, int flags)
{
    QuartzBitmapDevice *qbd = (QuartzBitmapDevice*) userInfo;

    if (qbd->page) QuartzBitmap_Output(dev, qbd); /* save the image unless the first page is being created */
    qbd->page++;
}

void QuartzBitmap_Close(QuartzDesc_t dev, void *userInfo)
{
    QuartzBitmapDevice *qbd = (QuartzBitmapDevice*) userInfo;

    /* FIXME: do this only if device is dirty? */
    if (qbd->page) QuartzBitmap_Output(dev, qbd);

    /* Free ourselves */
    if (qbd->bitmap) CFRelease(qbd->bitmap);
    if (qbd->uti)    free(qbd->uti);
    if (qbd->path)   free(qbd->path);
    free(qbd);
}

QuartzDesc_t QuartzBitmap_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    /* In the case of a zero length string we default to PNG presently. This
       should probably be an option somewhere. */
    double *dpi = par->dpi;
    double width = par->width, height = par->height;
    const char *type = par->type;
    double mydpi[2] = { 72.0, 72.0 }; /* fall-back to 72dpi if none was specified */
    QuartzDesc_t ret = NULL;
    if (!qf) qf = fn;
    if(!type || strlen(type) == 0) type = "public.png";
    if (!dpi) dpi=mydpi;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
    /* We'll gladly support any image destination type */
    CFArrayRef  types = CGImageDestinationCopyTypeIdentifiers();
    CFStringRef mine  = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) type, strlen(type), kCFStringEncodingUTF8, FALSE);
    if(CFArrayContainsValue(types,CFRangeMake(0, CFArrayGetCount(types)), mine)) {
        size_t w = (size_t) (dpi[0] * width);
        size_t h = (size_t) (dpi[1] * height);
        size_t rb= (w*8*4+7)/8; /* Bytes per row */
        size_t s = h*rb;
        /* QuartzDesc_t qd; */
        /* Allocate sufficient space */
	/* FIXME: check allocations */
        QuartzBitmapDevice *dev = malloc(sizeof(QuartzBitmapDevice)+s);
        dev->length = (unsigned int) s;
        dev->uti  = type ? strdup(type) : NULL;
        dev->path = par->file ? strdup(par->file) : NULL;
        dev->page = 0;
        memset(dev->data, 0, s);
        dev->bitmap = CGBitmapContextCreate(dev->data, w, h, 8, rb,
					    CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB),
					    kCGImageAlphaPremultipliedLast);
	/* bitmaps use flipped coordinates (top-left is the origin), so we need to pre-set CTM. */
	CGContextTranslateCTM(dev->bitmap, 0.0, height * dpi[1]);
	CGContextScaleCTM(dev->bitmap, 1.0, -1.0);
	QuartzBackend_t qdef = {
	    sizeof(qdef), width, height, dpi[0]/72.0 , dpi[1]/72.0, par->pointsize,
	    par->bg, par->canvas, par->flags | QDFLAG_RASTERIZED,
	    dev,
	    QuartzBitmap_GetCGContext,
	    NULL,	/* locate */
	    QuartzBitmap_Close,
	    QuartzBitmap_NewPage,
	    NULL,	/* state */
	    NULL,	/* par */
	    NULL,       /* sync */
	    NULL,       /* cap */
	};


	if (!(ret = qf->Create(dd, &qdef)))
            QuartzBitmap_Close(NULL, dev);
        else {
	    /* since this device is non-resizable we set the size right away (as opposed to on-display) */
	    qf->SetSize(ret, width, height);
	    /* tell Quartz to prepare our new context */
            qf->ResetContext(ret);
        }
    }
    CFRelease(mine);
    CFRelease(types);
#endif
    return ret;
}

