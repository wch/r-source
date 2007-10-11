/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  The R Foundation
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
 *  http://www.r-project.org/Licenses/
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

typedef struct {
    CGContextRef bitmap;
    char *uti;			/* Type of bitmap to produce */
    char *path;			/* Path for file save during close (can be NULL) */
    unsigned int length;	/* Size of the bitmap */
    char data[1];		/* Actual bitmap bytes */
} QuartzBitmapDevice;

CGContextRef QuartzBitmap_GetCGContext(QuartzDesc_t dev,void *userInfo) {
    return ((QuartzBitmapDevice*)userInfo)->bitmap;
}

void QuartzBitmap_Close(QuartzDesc_t dev,void *userInfo) {
    QuartzBitmapDevice *qbd = (QuartzBitmapDevice*)userInfo;
    
    if(qbd->path && qbd->uti) {
        /* On 10.4+ we can employ the CGImageDestination API to create a
           variety of different bitmap formats */
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
        CFStringRef pathString = CFStringCreateWithBytes(kCFAllocatorDefault,(UInt8*)qbd->path,strlen(qbd->path),kCFStringEncodingUTF8,FALSE);
        CFURLRef path;
        if(CFStringFind(pathString,CFSTR("://"),0).location != kCFNotFound) {
            CFStringRef pathEscaped= CFURLCreateStringByAddingPercentEscapes(kCFAllocatorDefault,pathString,NULL,NULL,kCFStringEncodingUTF8);
            path = CFURLCreateWithString(kCFAllocatorDefault,pathEscaped,NULL);
            CFRelease(pathEscaped);
        } else {
            path = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault,(const UInt8*)qbd->path,strlen(qbd->path),FALSE);
        }
        CFRelease(pathString);
        
        CFStringRef scheme = CFURLCopyScheme(path);
       	CFStringRef type  = CFStringCreateWithBytes(kCFAllocatorDefault,(UInt8*)qbd->uti,strlen(qbd->uti),kCFStringEncodingUTF8,FALSE);
    	CGImageRef image = CGBitmapContextCreateImage(qbd->bitmap);
        if(CFStringCompare(scheme,CFSTR("file"),0) == 0) {
            CGImageDestinationRef dest = CGImageDestinationCreateWithURL(path,type,1,NULL);
            CGImageDestinationAddImage(dest,image,NULL);
            CGImageDestinationFinalize(dest);
            CFRelease(dest);
        } else if(CFStringCompare(scheme,CFSTR("clipboard"),0) == 0) {
            //Copy our image into data
            CFMutableDataRef      data = CFDataCreateMutable(kCFAllocatorDefault,0);
            CGImageDestinationRef dest = CGImageDestinationCreateWithData(data,type,1,NULL);
            CGImageDestinationAddImage(dest,image,NULL);
            CGImageDestinationFinalize(dest);
            CFRelease(dest);
            PasteboardRef pb = NULL;
            if(noErr == PasteboardCreate(kPasteboardClipboard,&pb)) {
                PasteboardClear(pb);
                PasteboardSyncFlags syncFlags = PasteboardSynchronize(pb);
                PasteboardPutItemFlavor(pb,(PasteboardItemID)1,type,data,0);
            }
            CFRelease(data);
        } else
            warning("Not a supported scheme, no image data written.");
        CFRelease(scheme);
       	CFRelease(type);
        CFRelease(path);
        CFRelease(image);
#endif
    }
    /* Free ourselves */
    if (qbd->bitmap) CFRelease(qbd->bitmap);
    if (qbd->uti)    free(qbd->uti);
    if (qbd->path)   free(qbd->path);
    free(qbd);
}

Rboolean QuartzBitmap_DeviceCreate(void *dd,const char *type,const char *file,double width,double height,double pointsize,const char *family,
                                   Rboolean antialias,Rboolean smooth,Rboolean autorefresh,int quartzpos,int bg, double *dpi) {
    /* In the case of a zero length string we default to PNG presently. This 
       should probably be an option somewhere. */
    double mydpi[2] = { 72.0, 72.0 };
    if(!type || strlen(type) == 0) type = "public.png";
    Rboolean ret = FALSE;
    if (!dpi) dpi=mydpi;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
    /* We'll gladly support any image destination type */
    CFArrayRef  types = CGImageDestinationCopyTypeIdentifiers();
    CFStringRef mine  = CFStringCreateWithBytes(kCFAllocatorDefault,(UInt8*)type,strlen(type),kCFStringEncodingUTF8,FALSE);
    if(CFArrayContainsValue(types,CFRangeMake(0,CFArrayGetCount(types)),mine)) {
        size_t w = dpi[0]*width;
        size_t h = dpi[1]*height;
        size_t rb= (w*8*4+7)/8; /* Bytes per row */
        size_t s = h*rb;
        QuartzDesc_t qd;
        /* Allocate sufficient space */
        QuartzBitmapDevice *dev = malloc(sizeof(QuartzBitmapDevice)+s);
        dev->length = s;
        dev->uti  = (NULL == type) ? NULL : strdup(type);
        dev->path = (NULL == file) ? NULL : strdup(file);
        memset(dev->data,0,s);
        dev->bitmap = CGBitmapContextCreate(dev->data,w,h,8,rb,CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB),kCGImageAlphaPremultipliedLast);
        CGContextTranslateCTM(dev->bitmap,0.0,height*dpi[1]);
        CGContextScaleCTM(dev->bitmap,1.0,-1.0);
        /* Rprintf("dpi=%f/%f, scale=%f/%f, wh=%f/%f\n", dpi[0], dpi[1], dpi[0]/72.0, dpi[1]/72.0, width, height); */
        if(!(qd=QuartzDevice_Create(dd,dpi[0]/72.0,dpi[1]/72.0,pointsize,width,height,bg,antialias,smooth,
                                       QuartzBitmap_GetCGContext,
                                       NULL,	/* locate */
                                       QuartzBitmap_Close,
                                       NULL,	/* new page */
                                       NULL,	/* state */
                                       NULL,	/* par */
                                       NULL,    /* sync */
                                       dev)))
            QuartzBitmap_Close(NULL,dev);
        else {
            ret = TRUE;
            QuartzDevice_ResetContext(qd);
        }
    }
    CFRelease(mine);
    CFRelease(types);
#endif
    return ret;
}

