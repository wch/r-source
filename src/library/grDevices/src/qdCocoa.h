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
 *  https://www.R-project.org/Licenses/
 *
 *  Quartz Quartz device module header file
 *
 */

#include <R.h>
#include <R_ext/QuartzDevice.h>

/* inofficial API that can be used by other applications */

#define QCF_SET_PEPTR  1  /* set ProcessEvents function pointer */
#define QCF_SET_FRONT  2  /* set application mode to front */

void QuartzCocoa_SetupEventLoop(int flags, unsigned long latency);
int  QuartzCocoa_SetLatency(unsigned long latency);

/* this is the designated creator, used by the Quartz dispatcher */
QuartzDesc_t QuartzCocoa_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par);

#ifdef __OBJC__

#import <Cocoa/Cocoa.h>

typedef struct sQuartzCocoaDevice QuartzCocoaDevice;

@interface QuartzCocoaView : NSView
{
	QuartzCocoaDevice *ci;
}

+ (QuartzCocoaView*) quartzWindowWithRect: (NSRect) rect andInfo: (void*) info;

- (id) initWithFrame: (NSRect) fram andInfo: (void*) info;

@end

#endif
