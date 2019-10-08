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
 *  Cocoa Quartz device module
 *
 *  This file should be compiled only if AQUA is enabled
 */

#include "qdCocoa.h"
#include "qdPDF.h"  /* we use qdPDF for clipboard export and Save As */

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/QuartzDevice.h>
#include <R_ext/eventloop.h>

/* --- userInfo structure for the CocoaDevice --- */
#define histsize 16

struct sQuartzCocoaDevice {
    QuartzDesc_t    qd;
    QuartzCocoaView *view;
    NSWindow        *window;
    CGLayerRef      layer;   /* layer */
    CGContextRef    layerContext; /* layer context */
    CGContextRef    context; /* window drawing context */
    NSRect          bounds;  /* set along with context */
    BOOL            closing;
    BOOL            pdfMode; /* this flag is set when printing, bypassing CGLayer to avoid rasterization */
    int             inLocator;
    double          locator[2]; /* locaton click position (x,y) */
    BOOL            inHistoryRecall;
    int             inHistory;
    SEXP            history[histsize];
    int             histptr;
    const char     *title;
    QuartzParameters_t pars; /* initial parameters */
};

static QuartzFunctions_t *qf;

#pragma mark --- QuartzCocoaView class ---

@implementation QuartzCocoaView

/* we define them manually so we don't have to deal with GraphicsDevice/GraphicsEngine issues */
#define R_RED(col)      (((col)    )&255)
#define R_GREEN(col)    (((col)>> 8)&255)
#define R_BLUE(col)     (((col)>>16)&255)
#define R_ALPHA(col)    (((col)>>24)&255)
#define R_RGB(r,g,b)    ((r)|((g)<<8)|((b)<<16)|0xFF000000)
#define R_RGBA(r,g,b,a) ((r)|((g)<<8)|((b)<<16)|((a)<<24))

- (NSColor *) canvasColor
{
    int canvas = ci->pars.canvas;
    return [NSColor colorWithCalibratedRed: R_RED(canvas)/255.0 green:R_GREEN(canvas)/255.0 blue:R_BLUE(canvas)/255.0 alpha:R_ALPHA(canvas)/255.0];
}

/* can return nil on an error */
+ (QuartzCocoaView*) quartzWindowWithRect: (NSRect) rect andInfo: (void*) info
{
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*) info;
    QuartzCocoaView* view = nil;
    NSWindow* window = nil;
    NSColor* canvasColor = nil;

    /* do everything in a try block -- this is not merely theoretical,
       for example NSWindow will throw an expection when the supplied
       rect is too big */
    @try {
	view = [[QuartzCocoaView alloc] initWithFrame: rect andInfo: info];
	window = [[NSWindow alloc] initWithContentRect: rect
					     styleMask: NSTitledWindowMask|NSClosableWindowMask|
				   NSMiniaturizableWindowMask|NSResizableWindowMask//|NSTexturedBackgroundWindowMask
					       backing:NSBackingStoreBuffered defer:NO];
	NSColor *canvasColor = [view canvasColor];
	[window setBackgroundColor:canvasColor ? canvasColor : [NSColor colorWithCalibratedRed:1.0 green:1.0 blue:1.0 alpha:0.5]];
	[window setOpaque:NO];
	ci->window = window;
	
	[window setDelegate: view];
	[window setContentView: view];
	[window setInitialFirstResponder: view];
	/* [window setAcceptsMouseMovedEvents:YES]; not neeed now, maybe later */
	[window setTitle: [NSString stringWithUTF8String: ((QuartzCocoaDevice*)info)->title]];

        NSMenu *menu, *mainMenu;
        NSMenuItem *menuItem;
	/* soleMenu is set if we have no menu at all, so we have to create it. Otherwise we are loading into an application that has already some menu, so we need only our specific stuff. */
        BOOL soleMenu = ([NSApp mainMenu] == NULL);

        if (soleMenu) [NSApp setMainMenu:[[NSMenu alloc] init]];
	mainMenu = [NSApp mainMenu];

	/* File menu is tricky - it may have a different name in different localizations. Hence we use a trick - the File menu should be first and have the <Cmd><W> shortcut for "Close Window" by convenience */
	BOOL hasFileMenu = NO;
	if (!soleMenu) { /* in the case of a soleMenu we already know that we don't have it. Otherwise look for it. */
	    if (!hasFileMenu && [mainMenu indexOfItemWithTitle:@"File"]) hasFileMenu = YES; /* first shot is cheap - it will succeed if we added the menu ourself */
	    if (!hasFileMenu && [mainMenu numberOfItems] > 0 && (menuItem = [mainMenu itemAtIndex:0]) && (menu = [menuItem submenu])) { /* potentially a File menu */
		int i = 0, n = [menu numberOfItems];
		while (i < n) {
		    NSString *ke = [[menu itemAtIndex: i++] keyEquivalent];
		    if (ke && [ke isEqualToString:@"w"]) { hasFileMenu = YES; break; }
		}
	    }
	}
	if (!hasFileMenu) { /* No file menu? Add it. */
            menu = [[NSMenu alloc] initWithTitle:@"File"];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Close Window" action:@selector(performClose:) keyEquivalent:@"w"]; [menu addItem:menuItem]; [menuItem release];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Save" action:@selector(saveDocument:) keyEquivalent:@"s"]; [menu addItem:menuItem]; [menuItem release];
	    [menu addItem:[NSMenuItem separatorItem]];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Page Setup…" action:@selector(runPageLayout:) keyEquivalent:@"P"]; [menu addItem:menuItem]; [menuItem release];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Print" action:@selector(printDocument:) keyEquivalent:@"p"]; [menu addItem:menuItem]; [menuItem release];   
	    
            menuItem = [[NSMenuItem alloc] initWithTitle:[menu title] action:nil keyEquivalent:@""]; /* the "Quartz" item in the main menu */
            [menuItem setSubmenu:menu];
	    [mainMenu insertItem: menuItem atIndex:0];
	}

	/* same trick for Edit */
	BOOL hasEditMenu = NO;
	if (!soleMenu) { /* in the case of a soleMenu we already know that we don't have it. Otherwise look for it. */
	    if (!hasEditMenu && [mainMenu indexOfItemWithTitle:@"Edit"]) hasEditMenu = YES; /* first shot is cheap - it will succeed if we added the menu ourself */
	    if (!hasEditMenu && [mainMenu numberOfItems] > 1 && (menuItem = [mainMenu itemAtIndex:1]) && (menu = [menuItem submenu])) { /* potentially a Edit menu */
		int i = 0, n = [menu numberOfItems];
		while (i < n) {
		    NSString *ke = [[menu itemAtIndex: i++] keyEquivalent];
		    if (ke && [ke isEqualToString:@"c"]) { hasEditMenu = YES; break; }
		}
	    }
	}
	if (!hasEditMenu) { /* We really use just Copy, but we add some more to be consistent with other apps */
            menu = [[NSMenu alloc] initWithTitle:@"Edit"];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Undo" action:@selector(undo:) keyEquivalent:@"z"]; [menu addItem:menuItem]; [menuItem release];   
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Redo" action:@selector(redo:) keyEquivalent:@"Z"]; [menu addItem:menuItem]; [menuItem release];   
	    [menu addItem:[NSMenuItem separatorItem]];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Copy" action:@selector(copy:) keyEquivalent:@"c"]; [menu addItem:menuItem]; [menuItem release];   
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Paste" action:@selector(paste:) keyEquivalent:@"v"]; [menu addItem:menuItem]; [menuItem release];   
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Delete" action:@selector(delete:) keyEquivalent:@""]; [menu addItem:menuItem]; [menuItem release];   
	    [menu addItem:[NSMenuItem separatorItem]];
	    menuItem = [[NSMenuItem alloc] initWithTitle:@"Activate" action:@selector(activateQuartzDevice:) keyEquivalent:@"A"]; [menu addItem:menuItem]; [menuItem release];   
	    
            menuItem = [[NSMenuItem alloc] initWithTitle:[menu title] action:nil keyEquivalent:@""]; /* the "Quartz" item in the main menu */
            [menuItem setSubmenu:menu];
	    if ([mainMenu numberOfItems] > 0)
		[mainMenu insertItem: menuItem atIndex:1];
	    else /* this should never be the case because we have added "File" menu, but just in case something goes wrong ... */
		[mainMenu addItem: menuItem];
	}
	
        if ([mainMenu indexOfItemWithTitle:@"Quartz"] < 0) { /* Quartz menu - if it doesn't exist, add it */
            unichar leftArrow = NSLeftArrowFunctionKey, rightArrow = NSRightArrowFunctionKey;
            menu = [[NSMenu alloc] initWithTitle:@"Quartz"];
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Back" action:@selector(historyBack:) keyEquivalent:[NSString stringWithCharacters:&leftArrow length:1]]; [menu addItem:menuItem]; [menuItem release];
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Forward" action:@selector(historyForward:) keyEquivalent:[NSString stringWithCharacters:&rightArrow length:1]]; [menu addItem:menuItem]; [menuItem release];
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Clear History" action:@selector(historyFlush:) keyEquivalent:@"L"]; [menu addItem:menuItem]; [menuItem release];
	    
            menuItem = [[NSMenuItem alloc] initWithTitle:[menu title] action:nil keyEquivalent:@""]; /* the "Quartz" item in the main menu */
            [menuItem setSubmenu:menu];

            if (soleMenu)
                [[NSApp mainMenu] addItem:menuItem];
            else {
                int wmi; /* put us just before the Windows menu if possible */
                if ([NSApp windowsMenu] && ((wmi = [[NSApp mainMenu] indexOfItemWithSubmenu: [NSApp windowsMenu]])>=0))
                    [[NSApp mainMenu] insertItem: menuItem atIndex: wmi];
                else
                    [[NSApp mainMenu] addItem:menuItem];
            }
        }
        if (soleMenu) { /* those should be standard if we have some menu */
            menu = [[NSMenu alloc] initWithTitle:@"Window"];
            
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Minimize" action:@selector(performMiniaturize:) keyEquivalent:@"m"]; [menu addItem:menuItem];
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Zoom" action:@selector(performZoom:) keyEquivalent:@""]; [menu addItem:menuItem];
            
            /* Add to menubar */
            menuItem = [[NSMenuItem alloc] initWithTitle:@"Window" action:nil keyEquivalent:@""];
            [menuItem setSubmenu:menu];
            [[NSApp mainMenu] addItem:menuItem];
            [NSApp setWindowsMenu:menu];
            [menu release];
            [menuItem release];
        }        
    } @catch (NSException *ex) {
	/* on error release what we know about, issue a warning and return nil */
	if (window) {
	    ci->window = nil;
	    [window release];
	}
	if (view)
	    [view release];
	if (ex) {
	    /* we don't bother localizing this since the exception is likely in English anyway */
	    warning("Unable to create Cocoa Quartz window: %s (%s)",
		    [[ex reason] UTF8String], [[ex name] UTF8String]);
	}
	return nil;
    }

    return view;
}

- (id) initWithFrame: (NSRect) frame andInfo: (void*) info
{
    self = [super initWithFrame: frame];
    if (self) {
        ci = (QuartzCocoaDevice*) info;
        ci->view = self;
        ci->closing = NO;
        ci->inLocator = NO;
        ci->inHistoryRecall = NO;
        ci->inHistory = -1;
        ci->histptr = 0;
        memset(ci->history, 0, sizeof(ci->history));
    }
    return self;
}

- (BOOL)isFlipped { return YES; } /* R uses flipped coordinates */

- (IBAction) activateQuartzDevice:(id) sender
{
    if (qf && ci && ci-> qd) qf->Activate(ci->qd);
}

- (BOOL) writeAsPDF: (NSString*) fileName
{
    QuartzParameters_t qpar = ci->pars;
    qpar.file = [fileName UTF8String];
    qpar.connection = 0;
	qpar.parv = NULL;
    qpar.flags = 0;
    qpar.width = qf->GetWidth(ci->qd);
    qpar.height = qf->GetHeight(ci->qd);
    qpar.canvas = 0; /* disable canvas */
    QuartzDesc_t qd = Quartz_C(&qpar, QuartzPDF_DeviceCreate, NULL);
    if (qd == NULL) return NO;
    void *ss = qf->GetSnapshot(ci->qd, 0);
    qf->RestoreSnapshot(qd, ss);
    qf->Kill(qd);
	return YES;
}

- (IBAction) saveDocumentAs: (id) sender
{
	NSSavePanel *sp = [NSSavePanel savePanel];
	[sp setRequiredFileType:@"pdf"];
	[sp setTitle:@"Save Quartz To PDF File"];
	int answer = [sp runModalForDirectory:nil file:@"Rplot.pdf"];
	if(answer == NSOKButton)
		if (![self writeAsPDF:[sp filename]]) NSBeep();
}

- (IBAction) saveDocument: (id) sender
{
	[self saveDocumentAs:sender];
}

- (IBAction) copy: (id) sender
{
    /* currently we use qdPDF to create the PDF for the clipboard.
       Now that we have pdfMode we could use it instead, saving some memory ... */
    NSPasteboard *pb = [NSPasteboard generalPasteboard];
    QuartzParameters_t qpar = ci->pars;
    qpar.file = 0;
    qpar.connection = 0;
    CFMutableDataRef data = CFDataCreateMutable(NULL, 0);
    if (!data) { NSBeep(); return; } /* cannot copy */
    qpar.parv = data;
    qpar.flags = 0;
    qpar.width = qf->GetWidth(ci->qd);
    qpar.height = qf->GetHeight(ci->qd);
    qpar.canvas = 0; /* have to disable canvas */
    /* replay our snapshot and close the PDF device */
    QuartzDesc_t qd = Quartz_C(&qpar, QuartzPDF_DeviceCreate, NULL);
    if (qd == NULL) {
	CFRelease(data);
	NSBeep();
	return;
    }
    void *ss = qf->GetSnapshot(ci->qd, 0);
    qf->RestoreSnapshot(qd, ss);
    qf->Kill(qd);
    /* the result should be in the data by now */
    [pb declareTypes: [NSArray arrayWithObjects: NSPDFPboardType, nil ] owner:nil];
    [pb setData: (NSMutableData*) data forType:NSPDFPboardType];
    CFRelease(data);
}

- (IBAction)printDocument:(id)sender
{
    NSPrintInfo *printInfo;
    NSPrintOperation *printOp;
    
    printInfo = [[NSPrintInfo alloc] initWithDictionary: [[NSPrintInfo sharedPrintInfo] dictionary]];
    [printInfo setHorizontalPagination: NSFitPagination];
    [printInfo setVerticalPagination: NSAutoPagination];
    [printInfo setVerticallyCentered:NO];
    
    ci->pdfMode = YES;
    @try {
	printOp = [NSPrintOperation printOperationWithView:self 
						 printInfo:printInfo];
	[printOp setShowsPrintPanel:YES];
	[printOp setShowsProgressPanel:NO];
	[printOp runOperation];
    }
    @catch (NSException *ex) {}
    ci->pdfMode = NO;
}

- (void)drawRect:(NSRect)aRect
{
    CGRect rect;
    CGContextRef ctx = [[NSGraphicsContext currentContext] graphicsPort];
    ci->context = ctx;
    ci->bounds = [self bounds];        
    rect = CGRectMake(0.0, 0.0, ci->bounds.size.width, ci->bounds.size.height);
    
    if (ci->pdfMode) {
	qf->ReplayDisplayList(ci->qd);
	return;
    }

    /* Rprintf("drawRect, ctx=%p, bounds=(%f x %f)\n", ctx, ci->bounds.size.width, ci->bounds.size.height); */
    if (!ci->layer) {
        CGSize size = CGSizeMake(ci->bounds.size.width, ci->bounds.size.height);
        /* Rprintf(" - have no layer, creating one (%f x %f)\n", ci->bounds.size.width, ci->bounds.size.height); */
        ci->layer = CGLayerCreateWithContext(ctx, size, 0);
        ci->layerContext = CGLayerGetContext(ci->layer);
        qf->ResetContext(ci->qd);
        if (ci->inHistoryRecall && ci->inHistory >= 0) {
            qf->RestoreSnapshot(ci->qd, ci->history[ci->inHistory]);
            ci->inHistoryRecall = NO;
        } else
            qf->ReplayDisplayList(ci->qd);
    } else {
        CGSize size = CGLayerGetSize(ci->layer);
        /* Rprintf(" - have layer %p\n", ci->layer); */
        if (size.width != rect.size.width || size.height != rect.size.height) { /* resize */
            /* Rprintf(" - but wrong size (%f x %f vs %f x %f; drawing scaled version\n", size.width, size.height, rect.size.width, rect.size.height); */
            
            /* if we are in live resize, skip this all */
            if (![self inLiveResize]) {
                /* first draw a rescaled version */
                CGContextDrawLayerInRect(ctx, rect, ci->layer);
                /* release old layer */
                CGLayerRelease(ci->layer);
                ci->layer = 0;
                ci->layerContext = 0;
                /* set size */
                qf->SetScaledSize(ci->qd, ci->bounds.size.width, ci->bounds.size.height);
                /* issue replay */
                if (ci->inHistoryRecall && ci->inHistory >= 0) {
                    qf->RestoreSnapshot(ci->qd, ci->history[ci->inHistory]);
                    ci->inHistoryRecall = NO;
                } else
                    qf->ReplayDisplayList(ci->qd);
            }
        }
    }
    if ([self inLiveResize]) CGContextSetAlpha(ctx, 0.6); 
    if (ci->layer)
        CGContextDrawLayerInRect(ctx, rect, ci->layer);
    if ([self inLiveResize]) CGContextSetAlpha(ctx, 1.0); 
}

- (void)mouseDown:(NSEvent *)theEvent
{
    if (ci->inLocator) {
        NSPoint pt = [theEvent locationInWindow];
        unsigned int mf = [theEvent modifierFlags];
        ci->locator[0] = pt.x;
        ci->locator[1] = pt.y;
        /* Note: we still use menuForEvent:  because no other events than left click get here ..*/
        if (mf&(NSControlKeyMask|NSRightMouseDownMask|NSOtherMouseDownMask))
            ci->locator[0] = -1.0;
        ci->inLocator = NO;
    }
}

/* right-click does NOT generate mouseDown: events, sadly, so we have to (ab)use menuForEvent: */
- (NSMenu *)menuForEvent:(NSEvent *)theEvent
{
    if (ci->inLocator) {
        ci->locator[0] = -1.0;
        ci->inLocator = NO;
        return nil;
    }
    return [super menuForEvent:theEvent];
}

/* <Esc> is caught before so keyDown: won't work */
- (BOOL)performKeyEquivalent:(NSEvent *)theEvent
{
    if (ci->inLocator && [theEvent keyCode] == 53 /* ESC - can't find the proper constant for this */) {
        ci->locator[0] = -1.0;
        ci->inLocator = NO;
        return TRUE;
    }
    return FALSE;
}

static void QuartzCocoa_SaveHistory(QuartzCocoaDevice *ci, int last) {
    SEXP ss = (SEXP) qf->GetSnapshot(ci->qd, last);
    if (ss) { /* ss will be NULL if there is no content, e.g. during the first call */
        R_PreserveObject(ss);
        if (ci->inHistory != -1) { /* if we are editing an existing snapshot, replace it */
            /* Rprintf("(updating plot in history at %d)\n", ci->inHistory); */
            if (ci->history[ci->inHistory]) R_ReleaseObject(ci->history[ci->inHistory]);
            ci->history[ci->inHistory] = ss;
        } else {
            /* Rprintf("(adding plot to history at %d)\n", ci->histptr); */
            if (ci->history[ci->histptr]) R_ReleaseObject(ci->history[ci->histptr]);
            ci->history[ci->histptr++] = ss;
            ci->histptr &= histsize - 1;
        }
    }
}

- (void)historyBack: (id) sender
{
    int hp = ci->inHistory - 1;
    if (ci->inHistory == -1)
        hp = (ci->histptr - 1);
    hp &= histsize - 1;
    if (hp == ci->histptr || !ci->history[hp])
        return;	
    if (qf->GetDirty(ci->qd)) /* save the current snapshot if it is dirty */
        QuartzCocoa_SaveHistory(ci, 0);
    ci->inHistory = hp;
    ci->inHistoryRecall = YES;
    /* Rprintf("(activating history entry %d) ", hp); */
    /* get rid of the current layer and force a repaint which will fetch the right entry */
    CGLayerRelease(ci->layer);
    ci->layer = 0;
    ci->layerContext = 0;
    [self setNeedsDisplay:YES];
}

- (void)historyForward: (id) sender
{
    int hp = ci->inHistory + 1;
    if (ci->inHistory == -1) return;
    hp &= histsize - 1;
    if (hp == ci->histptr || !ci->history[hp]) /* we can't really get past the last entry */
        return;
    if (qf->GetDirty(ci->qd)) /* save the current snapshot if it is dirty */
        QuartzCocoa_SaveHistory(ci, 0);
    
    ci->inHistory = hp;
    /* Rprintf("(activating history entry %d)\n", hp); */
    ci->inHistoryRecall = YES;
    
    CGLayerRelease(ci->layer);
    ci->layer = 0;
    ci->layerContext = 0;
    [self setNeedsDisplay:YES];
}	

- (void)historyFlush: (id) sender
{
    int i = 0;
    ci->inHistory = -1;
    ci->inHistoryRecall = NO;
    ci->histptr = 0;
    while (i < histsize) {
        if (ci->history[i]) {
            R_ReleaseObject(ci->history[i]);
            ci->history[i]=0;
        }
        i++;
    }
}

- (void)viewDidEndLiveResize
{
    [self setNeedsDisplay: YES];
}

- (void)windowWillClose:(NSNotification *)aNotification {
    ci->closing = YES;
    qf->Kill(ci->qd);
}

- (void)resetCursorRects
{
    if (ci->inLocator)
        [self addCursorRect:[self bounds] cursor:[NSCursor crosshairCursor]];
}

@end

#pragma mark --- Cocoa event loop ---

/* --- Cocoa event loop
   This EL is enabled upon the first use of Quartz or alternatively using
   the QuartzCocoa_SetupEventLoop function */

static BOOL el_active = YES;   /* the worker thread work until this is NO */
static BOOL el_fired  = NO;    /* flag set when an event was fired */
static int  el_ofd, el_ifd;    /* communication file descriptors */
static unsigned long el_sleep; /* latency in ms */
static long el_serial = 0;     /* serial number for the time slice */
static long el_pe_serial = 0;  /* ProcessEvents serial number, event are
                                  only when the serial number changes */
static BOOL el_inhibit = NO;   /* this flag is used by special code that
				  needs to inhibit running the event loop */

/* helper function - sleep X milliseconds */
static void millisleep(unsigned long tout) {
    struct timeval tv;
    tv.tv_usec = (tout%1000)*1000;
    tv.tv_sec  = tout/1000;
    select(0, 0, 0, 0, &tv);
}

/* from aqua.c */
extern void (*ptr_R_ProcessEvents)(void);
/* from Defn.h */
extern Rboolean R_isForkedChild;

static void cocoa_process_events() {
    /* this is a precaution if cocoa_process_events is called
       via R_ProcessEvents and the R code calls it too often */
    if (!R_isForkedChild && !el_inhibit && el_serial != el_pe_serial) {
        NSEvent *event;
        while ((event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                          untilDate:nil
                                             inMode:NSDefaultRunLoopMode 
                                            dequeue:YES]))
            [NSApp sendEvent:event];
        el_pe_serial = el_serial;
    }
}

static void input_handler(void *data) {
    char buf[16];
    
    read(el_ifd, buf, 16);
    cocoa_process_events();
    el_fired = NO;
}

@interface ELThread : NSObject
- (int) eventsThread: (id) args;
@end

@implementation ELThread
- (int) eventsThread: (id) arg
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    char buf[16];
    
    while (el_active) {
        millisleep(el_sleep);
        el_serial++;
        if (!el_fired) {
            el_fired = YES; *buf=0;
            write(el_ofd, buf, 1);
        }
    }
    
    [pool release];
    return 0;
}
@end

static ELThread* el_obj = nil;

/* setup Cocoa event loop */
void QuartzCocoa_SetupEventLoop(int flags, unsigned long latency) {
    if (!el_obj) {
        int fds[2];
        pipe(fds);
        el_ifd = fds[0];
        el_ofd = fds[1];

        if (flags&QCF_SET_PEPTR)
            ptr_R_ProcessEvents = cocoa_process_events;

        el_sleep = latency;
        
        addInputHandler(R_InputHandlers, el_ifd, &input_handler, 31);

        el_obj = [[ELThread alloc] init];
        [NSThread detachNewThreadSelector:@selector(eventsThread:) toTarget:el_obj withObject:nil];
    }
    if (flags&QCF_SET_FRONT) {
        void CPSEnableForegroundOperation(ProcessSerialNumber* psn);
        ProcessSerialNumber myProc, frProc;
        Boolean sameProc;
        
        if (GetFrontProcess(&frProc) == noErr) {
            if (GetCurrentProcess(&myProc) == noErr) {
                if (SameProcess(&frProc, &myProc, &sameProc) == noErr && !sameProc) {
                    CPSEnableForegroundOperation(&myProc);
                }
                SetFrontProcess(&myProc);
            }
        }
    }
    
}

/* set Cocoa event loop latency in ms */
int QuartzCocoa_SetLatency(unsigned long latency) {
    el_sleep = latency;
    return (el_obj)?YES:NO;
}

/* inhibit Cocoa from running the event loop (e.g., when R is forked) */
void QuartzCocoa_InhibitEventLoop(int flag) {
    el_inhibit = flag ? YES : NO;
}

#pragma mark --- R Quartz interface ---

/*----- R Quartz interface ------*/

static int cocoa_initialized = 0;
static NSAutoreleasePool *global_pool = 0;

static void initialize_cocoa() {
    /* check embedding parameters to see if Rapp (or other Cocoa app) didn't do the work for us */
    int eflags = 0;
    if (qf) {
	int *p_eflags = (int*) qf->GetParameter(NULL, QuartzParam_EmbeddingFlags);
	if (p_eflags) eflags = p_eflags[0];
    }
    if ((eflags & QP_Flags_CFLoop) && (eflags & QP_Flags_Cocoa) && (eflags & QP_Flags_Front)) {
	cocoa_initialized = 1;
	return;
    }

    NSApplicationLoad();
    global_pool = [[NSAutoreleasePool alloc] init];
    if (eflags & QP_Flags_CFLoop) {
	cocoa_initialized = 1;
	return;
    }

    if (!ptr_R_ProcessEvents)
        QuartzCocoa_SetupEventLoop(QCF_SET_PEPTR|QCF_SET_FRONT, 100);

    
    [NSApplication sharedApplication];
    cocoa_process_events();
    cocoa_initialized = 1;
}

static CGContextRef QuartzCocoa_GetCGContext(QuartzDesc_t dev, void *userInfo) {
    QuartzCocoaDevice *qd = (QuartzCocoaDevice*)userInfo;
    return qd->pdfMode ? qd->context : qd->layerContext;
}

static void QuartzCocoa_Close(QuartzDesc_t dev,void *userInfo) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
	
    /* cancel any locator events */
    ci->inLocator = NO;
    ci->locator[0] = -1.0;
	
    /* release all history objects */
    ci->inHistory = -1;
    ci->inHistoryRecall = NO;
    ci->histptr = 0;
    {
        int i = 0;
        while (i < histsize) {
            if (ci->history[i]) {
                R_ReleaseObject(ci->history[i]);
                ci->history[i] = 0;
            }
            i++;
        }
    }

    if (ci->pars.family) free((void*)ci->pars.family);
    if (ci->pars.title) free((void*)ci->pars.title);
    if (ci->pars.file) free((void*)ci->pars.file);

    /* close the window (if it's not already closing) */
    if (ci && ci->view && !ci->closing)
        [[ci->view window] close];
	
    if (ci->view) [ci->view release]; /* this is our own release, the window should still have a copy */
    if (ci->window) [ci->window release]; /* that should close it all */
    ci->view = nil;
    ci->window = nil;
}

static int QuartzCocoa_Locator(QuartzDesc_t dev, void* userInfo, double *x, double*y) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
    
    if (!ci || !ci->view || ci->inLocator) return FALSE;
    
    ci->locator[0] = -1.0;
    ci->inLocator = YES;
    [[ci->view window] invalidateCursorRectsForView: ci->view];
    
    while (ci->inLocator && !ci->closing) {
        NSEvent *event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                            untilDate:[NSDate dateWithTimeIntervalSinceNow:0.2]
                                               inMode:NSDefaultRunLoopMode 
                                              dequeue:YES];
        if (event) [NSApp sendEvent:event];
    }
    [[ci->view window] invalidateCursorRectsForView: ci->view];
    *x = ci->locator[0];
    *y = ci->bounds.size.height - ci->locator[1];
    return (*x >= 0.0)?TRUE:FALSE;
}

static void QuartzCocoa_NewPage(QuartzDesc_t dev,void *userInfo, int flags) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
    if (!ci) return;
    if (ci->pdfMode) {
	if (ci->context)
	    qf->ResetContext(dev);
	return;
    }
    if ((flags&QNPF_REDRAW)==0) { /* no redraw -> really new page */
        QuartzCocoa_SaveHistory(ci, 1);
        ci->inHistory = -1;
    }
    if (ci->layer) {
        CGLayerRelease(ci->layer);
        ci->layer = 0;
        ci->layerContext = 0;
    }
    if (ci->context) {
        CGSize size = CGSizeMake(ci->bounds.size.width, ci->bounds.size.height);
        ci->layer = CGLayerCreateWithContext(ci->context, size, 0);
        ci->layerContext = CGLayerGetContext(ci->layer);
        qf->ResetContext(dev);
        /* Rprintf(" - creating new layer (%p - ctx: %p, %f x %f)\n", ci->layer, ci->layerContext,  size.width, size.height); */
    }
}

static void QuartzCocoa_Sync(QuartzDesc_t dev,void *userInfo) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
    if (!ci || !ci->view || ci->pdfMode) return;
    /* we have to force display now, enqueuing it on the event loop
     * via setNeedsDisplay: YES has issues since dev.flush() won't
     * be synchronous and thus animation using dev.flush(); dev.hold()
     * will break by the time the event loop is run */
    [ci->view display];
}

static void QuartzCocoa_State(QuartzDesc_t dev, void *userInfo, int state) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
    NSString *title;
    if (!ci || !ci->view) return;
    if (!ci->title) ci->title=strdup("Quartz %d");
    title = [NSString stringWithFormat: [NSString stringWithUTF8String: ci->title], qf->DevNumber(dev)];
    if (state) title = [title stringByAppendingString: @" [*]"];
    [[ci->view window] setTitle: title];
}

static void* QuartzCocoa_Cap(QuartzDesc_t dev, void *userInfo) {
    QuartzCocoaDevice *ci = (QuartzCocoaDevice*)userInfo;
    SEXP raster = R_NilValue;

    if (!ci || !ci->view) {
        return (void*) raster;
    } else {
        unsigned int i, pixels, stride, j = 0;
        unsigned int *rint;
        SEXP dim;
        NSSize size = [ci->view frame].size;
	pixels = size.width * size.height;
	
	// make sure the view is up-to-date (fix for PR#14260)
	[ci->view display];

        if (![ci->view canDraw])
            warning("View not able to draw!?");

        [ci->view lockFocus];
        NSBitmapImageRep* rep = [[NSBitmapImageRep alloc] 
                                    initWithFocusedViewRect:
                                        NSMakeRect(0, 0, 
                                                   size.width, size.height)];

	int bpp = (int) [rep bitsPerPixel];
	int spp = (int) [rep samplesPerPixel];
	NSBitmapFormat bf = [rep bitmapFormat];
	/* Rprintf("format: bpp=%d, bf=0x%x, bps=%d, spp=%d, planar=%s, colorspace=%s\n", bpp, (int) bf, [rep bitsPerSample], spp, [rep isPlanar] ? "YES" : "NO", [[rep colorSpaceName] UTF8String]); */
	/* we only support meshed (=interleaved) formats of 8 bits/component with 3 or 4 components. We should really check for RGB/RGBA as well.. */
	if ([rep isPlanar] || [rep bitsPerSample] != 8 || (bf & NSFloatingPointSamplesBitmapFormat) || (bpp != 24 && bpp != 32)) {
	    warning("Unsupported image format");
	    return (void*) raster;
	}

        unsigned char *screenData = [rep bitmapData];

        PROTECT(raster = allocVector(INTSXP, pixels));

	/* FIXME: the current implementation of rasters seems to be endianness-dependent which is deadly (whether that is intentional or not). It needs to be fixed before it can work properly. The code below is sort of ok in little-endian machines, but the resulting raster is interpreted wrongly on big-endian machines. This needs to be discussed with Paul as all details are missing from his write-up... */
        /* Copy each byte of screen to an R matrix. 
         * The ARGB32 needs to be converted to an R ABGR32 */
        rint = (unsigned int *) INTEGER(raster);
	stride = (bpp == 24) ? 3 : 4; /* convers bpp to stride in bytes */

	if (bf & NSAlphaFirstBitmapFormat) /* ARGB */
	    for (i = 0; i < pixels; i++, j += stride)
		rint[i] = R_RGBA(screenData[j + 1], screenData[j + 2], screenData[j + 3], screenData[j]);
	else if (spp == 4) /* RGBA */
	    for (i = 0; i < pixels; i++, j += stride)
		rint[i] = R_RGBA(screenData[j], screenData[j + 1], screenData[j + 2], screenData[j + 3]);
	else /* RGB */
	    for (i = 0; i < pixels; i++, j += stride)
		rint[i] = R_RGB(screenData[j + 0], screenData[j + 1], screenData[j + 2]);

	[rep release];
	
	PROTECT(dim = allocVector(INTSXP, 2));
        INTEGER(dim)[0] = size.height;
        INTEGER(dim)[1] = size.width;
        setAttrib(raster, R_DimSymbol, dim);
	
        UNPROTECT(2);

        [ci->view unlockFocus];
    }
    
    return (void *) raster;
}

QuartzDesc_t QuartzCocoa_DeviceCreate(void *dd, QuartzFunctions_t *fn, QuartzParameters_t *par)
{
    QuartzDesc_t qd;
    double *dpi = par->dpi, width = par->width, height = par->height;
    double mydpi[2] = { 72.0, 72.0 };
    double scalex = 1.0, scaley = 1.0;
    QuartzCocoaDevice *dev;
	
    if (!qf) qf = fn;
    
    { /* check whether we have access to a display at all */
	CGDisplayCount dcount = 0;
	CGGetOnlineDisplayList(255, NULL, &dcount);
	if (dcount < 1) {
	    warning("No displays are available");
	    return NULL;
	}
    }

    if (!dpi) {
        CGDirectDisplayID md = CGMainDisplayID();
        if (md) {
            CGSize ds = CGDisplayScreenSize(md);
            double width  = (double)CGDisplayPixelsWide(md);
            double height = (double)CGDisplayPixelsHigh(md);
	    /* landscape screen, portrait resolution -> rotated screen */
	    if (ds.width > ds.height && width < height) {
		mydpi[0] = width / ds.height * 25.4;
		mydpi[1] = height / ds.width * 25.4;
	    } else {
		mydpi[0] = width / ds.width * 25.4;
		mydpi[1] = height / ds.height * 25.4;
	    }
            /* Rprintf("screen resolution %f x %f\n", mydpi[0], mydpi[1]); */
        }
        dpi = mydpi;
    }

    scalex = dpi[0] / 72.0;
    scaley = dpi[1] / 72.0;

    if (width * height > 20736.0) {
	warning("Requested on-screen area is too large (%.1f by %.1f inches).", width, height);
	return NULL;
    }

    /* FIXME: check allocations [better now, but strdups below are not covered; also check dev->pars] */
    dev = malloc(sizeof(QuartzCocoaDevice));
    memset(dev, 0, sizeof(QuartzCocoaDevice));

    QuartzBackend_t qdef = {
	sizeof(qdef), width, height, scalex, scaley, par->pointsize,
	par->bg, par->canvas, par->flags | QDFLAG_INTERACTIVE | QDFLAG_DISPLAY_LIST | QDFLAG_RASTERIZED,
	dev,
	QuartzCocoa_GetCGContext,
	QuartzCocoa_Locator,
	QuartzCocoa_Close,
	QuartzCocoa_NewPage,
	QuartzCocoa_State,
	NULL,/* par */
	QuartzCocoa_Sync,
        QuartzCocoa_Cap,
    };
    
    qd = qf->Create(dd, &qdef);
    if (!qd) {
	free(dev);
	return NULL;
    }
    dev->qd = qd;
    
    /* copy parameters for later */
    memcpy(&dev->pars, par, (par->size < sizeof(QuartzParameters_t))? par->size : sizeof(QuartzParameters_t));
    if (par->size > sizeof(QuartzParameters_t)) dev->pars.size = sizeof(QuartzParameters_t);
    if (par->family) dev->pars.family = strdup(par->family);
    if (par->title) dev->pars.title = strdup(par->title);
    if (par->file) dev->pars.file = strdup(par->file);
    
    /* we cannot substitute the device number as it is not yet known at this point */
    dev->title = strdup(par->title);
    {
        NSRect rect = NSMakeRect(20.0, 20.0, /* FIXME: proper position */
                                 qf->GetScaledWidth(qd), qf->GetScaledHeight(qd));
        if (!cocoa_initialized) initialize_cocoa();
        /* Rprintf("scale=%f/%f; size=%f x %f\n", scalex, scaley, rect.size.width, rect.size.height); */
        if (![QuartzCocoaView quartzWindowWithRect: rect andInfo: dev]) {
	    free((char*)dev->title);
	    free(qd);
	    free(dev);
	    return NULL;
	}
    }
    if (dev->view)
        [[dev->view window] makeKeyAndOrderFront: dev->view];
    return qd;
}
