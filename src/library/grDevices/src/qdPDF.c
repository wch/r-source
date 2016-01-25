/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007-8  The R Foundation
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
 *  PDF output Quartz device module
 *
 *  This module creates PDF output using CoreGraphics. Currently
 *  supported targets are file and CFMutableData. The latter is
 *  passed as parv in parameters.
 *
 *  This file should be compiled only if AQUA is enabled
 */


#include "qdPDF.h"

//#include <R.h>
#include <Rinternals.h>
//#include <R_ext/QuartzDevice.h>
#define _(String) (String)

typedef struct {
    CGContextRef context;   /* drawing context */
    CFURLRef url;           /* destication URL (on NULL is connection or data is used) */
    int connection;         /* destination connection (currently unsupported) */
    int page;               /* page number (0 before first NewPage call) */
    CGRect bbox;            /* bounding box (in points) */
    CFMutableDataRef data;  /* destination data (if writing to CFMutableData) */
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
    if (qpd->data) CFRelease(qpd->data);
    free(qpd);
}

QuartzDesc_t
QuartzPDF_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    QuartzDesc_t ret = NULL;
    double *dpi = par->dpi;
    double mydpi[2] = { 72.0, 72.0 };
    double width = par->width, height = par->height;
    /* DPI is ignored, because PDF is resolution independent.
       More precisely 72dpi is used to guarantee that PDF and GE
       coordinates are the same */
    dpi = mydpi;

    if (!qf) qf = fn;

    QuartzPDFDevice *dev = calloc(1, sizeof(QuartzPDFDevice));

    if ((!par->file || ! *par->file)) par->file = "Rplots.pdf";

    if (par->parv) dev->data = (CFMutableDataRef) CFRetain((CFTypeRef) par->parv); /* parv if set is CFMutableDataRef to write to */
    else if (par->file && *par->file) {
        CFStringRef path = CFStringCreateWithBytes(kCFAllocatorDefault, (UInt8*) par->file, strlen(par->file), kCFStringEncodingUTF8, FALSE);
        if (!path || !(dev->url = CFURLCreateWithFileSystemPath (NULL, path, kCFURLPOSIXPathStyle, false))) {
	    warning(_("cannot open file '%s'"), par->file);
            free(dev);
            return ret;
        }
        CFRelease(path);
    }
    dev->bbox = CGRectMake(0, 0, width * 72.0, height * 72.0);
    CFDictionaryRef ai = 0;
    { /* optional PDF auxiliary info: we add creator and title (if present) - we could support more ... */
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

    if (dev->data) {
	CGDataConsumerRef consumer = CGDataConsumerCreateWithCFData(dev->data);
	if (consumer) {
	    dev->context = CGPDFContextCreate(consumer, &dev->bbox, ai);
	    CFRelease(consumer);
	}
    } else
	dev->context = CGPDFContextCreateWithURL(dev->url, &dev->bbox, ai);

    if (dev->context == NULL) {
	if (ai) CFRelease(ai);
	if (dev->url) CFRelease(dev->url);
	free(dev);
	return ret;
    }
    if (ai) CFRelease(ai);
    dev->page = 0;

    /* we need to flip the y coordinate */
    CGContextTranslateCTM(dev->context, 0.0, dev->bbox.size.height);
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
	NULL,   /* sync */
        NULL,   /* cap */
    };

    if (!(ret = qf->Create(dd, &qdef)))
	QuartzPDF_Close(NULL,dev);
    else {
	qf->SetSize(ret, width, height);
	qf->ResetContext(ret);
    }

    return ret;
}

