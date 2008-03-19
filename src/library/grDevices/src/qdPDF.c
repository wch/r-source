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
 *  PDF output Quartz device module
 *
 *  This file should be compiled only if AQUA is enabled
 */


#include "qdPDF.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/QuartzDevice.h>

typedef struct {
    CGContextRef context;
    CFURLRef url;
    int connection;
    int page;
    CGRect bbox;
} QuartzPDFDevice;

static QuartzFunctions_t *qf;

CGContextRef QuartzPDF_GetCGContext(QuartzDesc_t dev,void *userInfo) 
{
    return ((QuartzPDFDevice*)userInfo)->context;
}

void QuartzPDF_NewPage(QuartzDesc_t dev, void *userInfo, int flags) 
{
    QuartzPDFDevice *qpd = (QuartzPDFDevice*) userInfo;
    if (qpd->context) { /* hopefully that's true */
        if (qpd->page) CGContextEndPage(qpd->context);
        CGContextBeginPage(qpd->context, &qpd->bbox);
    }
    qpd->page++;
}

void QuartzPDF_Close(QuartzDesc_t dev, void *userInfo)
{
    QuartzPDFDevice *qpd = (QuartzPDFDevice*) userInfo;

    if (qpd->context) { /* hopefully that's true */
        if (qpd->page) CGContextEndPage(qpd->context);
        CGContextRelease(qpd->context);
    }
    /* Free ourselves */
    if (qpd->url) CFRelease(qpd->url);
    free(qpd);
}

Rboolean 
QuartzPDF_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    double *dpi = par->dpi;
    double mydpi[2] = { 72.0, 72.0 };
    double width = par->width, height = par->height;
    Rboolean ret = FALSE;
    /* DPI is ignored, because PDF is resolution independent.
       More precisely 72dpi is used to guatantee that PDF and GE 
       coordinates are the same */
    dpi=mydpi;
    
    if (!qf) qf = fn;
    
    size_t w = dpi[0] * width;
    size_t h = dpi[1] * height;
    size_t rb= (w*8*4+7)/8; /* Bytes per row */
    size_t s = h * rb;
    QuartzDesc_t qd;
    QuartzPDFDevice *dev = malloc(sizeof(QuartzPDFDevice) + s);
    
    if (par->file && *par->file) {
        CGRect bbox;
        CFStringRef path = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) par->file, strlen(par->file), kCFStringEncodingUTF8, FALSE);
        if (!path || !(dev->url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, false))) {
            free(dev);
            return ret;
        }
        dev->bbox = CGRectMake(0, 0, width*72.0, height*72.0);
        CFRelease(path);
        CFDictionaryRef ai = 0;
        { /* optional PDF auxiliary info - we could support more ... */ 
            int numK = 1;
            CFStringRef keys[2], values[2];
            keys[0] = kCGPDFContextCreator;
            values[0] = CFSTR("Quartz R Device");
            if (par->title) {
                keys[numK] = kCGPDFContextTitle;
                values[numK] = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) par->title, strlen(par->title), kCFStringEncodingUTF8, FALSE);
                numK++;
            }
            ai = CFDictionaryCreate(0, (void*) keys, (void*) values, numK, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
            while (numK) CFRelease(values[--numK]);
        }
        if (!(dev->context = CGPDFContextCreateWithURL(dev->url, &dev->bbox, ai))) {
            if (ai) CFRelease(ai);
            CFRelease(dev->url);
            free(dev);
            return ret;
        }
        if (ai) CFRelease(ai);
        dev->page = 0;
        /* we need to flip the y coordinates */
        CGContextTranslateCTM(dev->context, 0.0, height*dpi[1]);
        CGContextScaleCTM(dev->context, 1.0, -1.0);
	
	QuartzBackend_t qdef = {
	    sizeof(qdef), width, height,
	    dpi[0]/72.0, dpi[1]/72.0, par->pointsize,
	    par->bg, par->canvas, par->flags,
	    dev,
	    QuartzPDF_GetCGContext,
	    NULL,	/* locate */
	    QuartzPDF_Close,
	    QuartzPDF_NewPage,
	    NULL,	/* state */
	    NULL,	/* par */
	    NULL
	};
	
	if (!qf->Create(dd, &qdef))
            QuartzPDF_Close(NULL,dev);
        else {
            ret = TRUE;
            qf->ResetContext(qd);
        }
    }
    return ret;
}

