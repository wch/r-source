/*  R : A Computer Language for Statistical Data Analysis
 *  file RBitMap.c
 *  Copyright (C) 1998-1999  Ross Ihaka
 *                2000-2001  Stefano M. Iacus and the R core team
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <fp.h>
#include "RIntf.h"


/*void ExerciseOffScreen();
*/
void SetUpOffScreen();
void WriteTo();
void CopyTo();
void TurnOff();
void WriteTo2();




CGrafPtr   colorPort;   /* Graphics environment for color off screen */
GDHandle   colorDevice; /* Color environment for color off screen */
GrafPtr    savedPort;   /* Pointer to the saved graphics environment */
GDHandle   savedDevice; /* Handle to the saved color environment */
Rect       circleRect;  /* Rectangles for circle-drawing */

OSErr SetUpPixMap(
    short        depth,       /* Desired number of bits/pixel in off-screen*/
    Rect         *bounds,     /* Bounding rectangle of off-screen */
    CTabHandle   colors,      /* Color table to assign to off-screen */
    short        bytesPerRow, /* Number of bytes per row in the PixMap */
    PixMapHandle aPixMap)     /* Handle to the PixMap being initialized */
{
    CTabHandle newColors;   /* Color table used for the off-screen PixMap */
    Ptr        offBaseAddr; /* Pointer to the off-screen pixel image */
    OSErr      error;       /* Returns error code */

    error = noErr;
    newColors = nil;
    offBaseAddr = nil;

    /* Clone the clut if indexed color; allocate a dummy clut if direct color*/
    if (depth <= 8)
    {
        newColors = colors;
        error = HandToHand( (Handle *)&newColors );
    }
    else
    {
        newColors = (CTabHandle)NewHandle( sizeof (ColorTable) -
					   sizeof (CSpecArray) );
        error = MemError();
    }
    if (error == noErr)
    {
        /* Allocate pixel image; long integer multiplication avoids overflow*/
        offBaseAddr = NewPtr( (unsigned long)bytesPerRow * (bounds->bottom-bounds->top) );
        if (offBaseAddr != nil)
        {
            /* Initialize fields common to indexed and direct PixMaps */
            (**aPixMap).baseAddr = offBaseAddr;  /* Point to image */
            (**aPixMap).rowBytes = bytesPerRow | /* MSB set for PixMap */
		0x8000;
            (**aPixMap).bounds = *bounds;        /* Use given bounds */
            (**aPixMap).pmVersion = 0;           /* No special stuff */
            (**aPixMap).packType = 0;            /* Default PICT pack */
            (**aPixMap).packSize = 0;            /* Always zero in mem */
            (**aPixMap).hRes = kDefaultRes;      /* 72 DPI default res */
            (**aPixMap).vRes = kDefaultRes;      /* 72 DPI default res */
            (**aPixMap).pixelSize = depth;       /* Set # bits/pixel */
/*            (**aPixMap).planeBytes = 0;   */       /* Not used */
/*            (**aPixMap).pmReserved = 0;     */     /* Not used */

            /* Initialize fields specific to indexed and direct PixMaps */
            if (depth <= 8)
            {
                /* PixMap is indexed */
                (**aPixMap).pixelType = 0;       /* Indicates indexed */
                (**aPixMap).cmpCount = 1;        /* Have 1 component */
                (**aPixMap).cmpSize = depth;     /* Component size=depth */
                (**aPixMap).pmTable = newColors; /* Handle to CLUT */
            }
            else
            {
                /* PixMap is direct */
                (**aPixMap).pixelType = RGBDirect; /* Indicates direct */
                (**aPixMap).cmpCount = 3;          /* Have 3 components */
                if (depth == 16)
                    (**aPixMap).cmpSize = 5;       /* 5 bits/component */
                else
                    (**aPixMap).cmpSize = 8;       /* 8 bits/component */
                (**newColors).ctSeed = 3 * (**aPixMap).cmpSize;
                (**newColors).ctFlags = 0;
                (**newColors).ctSize = 0;
                (**aPixMap).pmTable = newColors;
            }
        }
        else
            error = MemError();
    }
    else
        newColors = nil;

    /* If no errors occurred, return a handle to the new off-screen PixMap */
    if (error != noErr)
    {
        if (newColors != nil)
            DisposeCTable( newColors );
    }

    /* Return the error code */
    return error;
}


OSErr CreateOffScreen(
    Rect       *bounds,     /* Bounding rectangle of off-screen */
    short      depth,       /* Desired number of bits per pixel in off-screen*/
    CTabHandle colors,      /* Color table to assign to off-screen */
    CGrafPtr   *retPort,    /* Returns a pointer to the new CGrafPort */
    GDHandle   *retGDevice) /* Returns a handle to the new GDevice */
{
    CGrafPtr     newPort;     /* Pointer to the new off-screen CGrafPort */
    PixMapHandle newPixMap;   /* Handle to the new off-screen PixMap */
    GDHandle     newDevice;   /* Handle to the new off-screen GDevice */
    long         qdVersion;   /* Version of QuickDraw currently in use */
    GrafPtr      savedPort;   /* Pointer to GrafPort used for save/restore */
    SignedByte   savedState;  /* Saved state of color table handle */
    short        bytesPerRow; /* Number of bytes per row in the PixMap */
    OSErr        error;       /* Returns error code */

    /* Initialize a few things before we begin */
    newPort = nil;
    newPixMap = nil;
    newDevice = nil;
    error = noErr;

    /* Save the color table's current state and make sure it isn't purgeable*/
    if (colors != nil)
    {
        savedState = HGetState( (Handle)colors );
        HNoPurge( (Handle)colors );
    }

    /* Calculate the number of bytes per row in the off-screen PixMap */
    bytesPerRow = ((depth * (bounds->right - bounds->left) + 31) >>5) << 2;

    /* Get the current QuickDraw version */
    (void)Gestalt( gestaltQuickdrawVersion, &qdVersion );

    /* Make sure depth is indexed or depth is direct and 32-Bit QD installed*/
    if (depth == 1 || depth == 2 || depth == 4 || depth == 8 ||
	((depth == 16 || depth == 32) && qdVersion >=gestalt32BitQD))
    {
        /* Maximum number of bytes per row is 16,382; make sure within range*/
        if (bytesPerRow <= kMaxRowBytes)
        {
            /* Make sure a color table is provided if the depth is indexed */
            if (depth <= 8)
                if (colors == nil)
		    /* Indexed depth and clut is NIL; is parameter error */
		    error = paramErr;
        }
        else
            /* # of bytes per row is more than 16,382; is parameter error */
            error = paramErr;
    }
    else
        /* Pixel depth isn't valid; is parameter error */
        error = paramErr;

    /* If sanity checks succeed, then allocate a new CGrafPort */
    if (error == noErr)
    {
        newPort = (CGrafPtr)NewPtr( sizeof (CGrafPort) );
        if (newPort != nil)
        {
            /* Save the current port */
            GetPort( &savedPort );

            /* Initialize the new CGrafPort and make it the current port */
            OpenCPort( newPort );

            /* Set portRect, visRgn, and clipRgn to the given bounds rect */
            newPort->portRect = *bounds;
            RectRgn( newPort->visRgn, bounds );
            ClipRect( bounds );

            /* Initialize the new PixMap for off-screen drawing */
            error = SetUpPixMap( depth, bounds, colors, bytesPerRow,
				 newPort->portPixMap );
            if (error == noErr)
            {
                /* Grab the initialized PixMap handle */
                newPixMap = newPort->portPixMap;

                /* Allocate and initialize a new GDevice */
                error = CreateGDevice( newPixMap, &newDevice );
            }

            /* Restore the saved port */
            SetPort( savedPort );
        }
        else
            error = MemError();
    }

    /* Restore the given state of the color table */
    if (colors != nil)
        HSetState( (Handle)colors, savedState );

    /* One Last Look Around The House Before We Go... */
    if (error != noErr)
    {
        /* Some error occurred; dispose of everything we allocated */
        if (newPixMap != nil)
        {
            DisposeCTable( (**newPixMap).pmTable );
            DisposePtr( (**newPixMap).baseAddr );
        }
        if (newDevice != nil)
        {
            DisposeHandle( (Handle)(**newDevice).gdITable );
            DisposeHandle( (Handle)newDevice );
        }
        if (newPort != nil)
        {
            CloseCPort( newPort );
            DisposePtr( (Ptr)newPort );
        }
    }
    else
    {
        /* Everything's OK; return refs to off-screen CGrafPort and GDevice*/
        *retPort = newPort;
        *retGDevice = newDevice;
    }
    return error;
}



OSErr CreateGDevice(
    PixMapHandle basePixMap,  /* Handle to the PixMap to base GDevice on */
    GDHandle     *retGDevice) /* Returns a handle to the new GDevice */
{
    GDHandle   newDevice;  /* Handle to the new GDevice */
    ITabHandle embryoITab; /* Handle to the embryonic inverse table */
    Rect       deviceRect; /* Rectangle of GDevice */
    OSErr      error;      /* Error code */

    /* Initialize a few things before we begin */
    error = noErr;
    newDevice = nil;
    embryoITab = nil;

    /* Allocate memory for the new GDevice */
    newDevice = (GDHandle)NewHandle( sizeof (GDevice) );
    if (newDevice != nil)
    {
        /* Allocate the embryonic inverse table */
        embryoITab = (ITabHandle)NewHandleClear( 2 );
        if (embryoITab != nil)
        {
            /* Set rectangle of device to PixMap bounds */
            deviceRect = (**basePixMap).bounds;

            /* Initialize the new GDevice fields */
            (**newDevice).gdRefNum = 0;            /* Only used for screens*/
            (**newDevice).gdID = 0;                /* Won't normally use */
            if ((**basePixMap).pixelSize <= 8)
                (**newDevice).gdType = clutType;   /* Depth<=8; clut device*/
            else
                (**newDevice).gdType = directType; /* Depth>8; direct device*/
            (**newDevice).gdITable = embryoITab;   /* 2-byte handle for now*/
            (**newDevice).gdResPref = kITabRes;    /* Normal inv table res */
            (**newDevice).gdSearchProc = nil;      /* No color-search proc */
            (**newDevice).gdCompProc = nil;        /* No complement proc */
            (**newDevice).gdFlags = 0;             /* Will set these later */
            (**newDevice).gdPMap = basePixMap;     /* Reference our PixMap */
            (**newDevice).gdRefCon = 0;            /* Won't normally use */
            (**newDevice).gdNextGD = nil;          /* Not in GDevice list */
            (**newDevice).gdRect = deviceRect;     /* Use PixMap dimensions*/
            (**newDevice).gdMode = -1;             /* For nonscreens */
            (**newDevice).gdCCBytes = 0;           /* Only used for screens*/
            (**newDevice).gdCCDepth = 0;           /* Only used for screens*/
            (**newDevice).gdCCXData = 0;           /* Only used for screens*/
            (**newDevice).gdCCXMask = 0;           /* Only used for screens*/
            (**newDevice).gdReserved = 0;          /* Currently unused */

            /* Set color-device bit if PixMap isn't black & white */
            if ((**basePixMap).pixelSize > 1)
                SetDeviceAttribute( newDevice, gdDevType, true );

            /* Set bit to indicate that the GDevice has no video driver */
            SetDeviceAttribute( newDevice, noDriver, true );

            /* Initialize the inverse table */
            if ((**basePixMap).pixelSize <= 8)
            {
                MakeITable( (**basePixMap).pmTable, (**newDevice).gdITable,
			    (**newDevice).gdResPref );
                error = QDError();
            }
        }
        else
            error = MemError();
    }
    else
        error = MemError();

    /* Handle any errors along the way */
    if (error != noErr)
    {
        if (embryoITab != nil)
            DisposeHandle( (Handle)embryoITab );
        if (newDevice != nil)
            DisposeHandle( (Handle)newDevice );
    }
    else
        *retGDevice = newDevice;

    /* Return a handle to the new GDevice */
    return error;
}

void DisposeOffScreen(
    CGrafPtr doomedPort,    /* Pointer to the CGrafPort to be disposed of */
    GDHandle doomedGDevice) /* Handle to the GDevice to be disposed of */
{
    CGrafPtr currPort;    /* Pointer to the current port */
    GDHandle currGDevice; /* Handle to the current GDevice */

    /* Check to see whether the doomed CGrafPort is the current port */
    GetPort( (GrafPtr *)&currPort );
    if (currPort == doomedPort)
    {
        /* It is; set current port to Window Manager CGrafPort */
        GetCWMgrPort( &currPort );
        SetPort( (GrafPtr)currPort );
    }

    /* Check to see whether the doomed GDevice is the current GDevice */
    currGDevice = GetGDevice();
    if (currGDevice == doomedGDevice)
        /* It is; set current GDevice to the main screen's GDevice */
        SetGDevice( GetMainDevice() );

    /* Throw everything away */
    (**doomedGDevice).gdPMap = nil;
    DisposeGDevice( doomedGDevice );
    DisposePtr( (**doomedPort->portPixMap).baseAddr );
    if ((**doomedPort->portPixMap).pmTable != nil)
        DisposeCTable( (**doomedPort->portPixMap).pmTable );
    CloseCPort( doomedPort );
    DisposePtr( (Ptr)doomedPort );
}


#define kMaxRowBytes 0x3FFE /* Maximum number of bytes in a row of pixels */

OSErr UpdateOffScreen(
    Rect       *newBounds, /* New bounding rectangle of off-screen */
    short      newDepth,   /* New number of bits per pixel in off-screen */
    CTabHandle newColors,  /* New color table to assign to off-screen */
    CGrafPtr   updPort,    /* Returns a pointer to the updated CGrafPort */
    GDHandle   updGDevice) /* Returns a handle to the updated GDevice */
{
    PixMapHandle newPixMap;   /* Handle to the new off-screen PixMap */
    PixMapHandle oldPixMap;   /* Handle to the old off-screen PixMap */
    Rect         bounds;      /* Boundary rectangle of off-screen */
    short        depth;       /* Depth of the off-screen PixMap */
    short        bytesPerRow; /* Number of bytes per row in the PixMap */
    CTabHandle   colors;      /* Colors for the off-screen PixMap */
    RGBColor     savedFore;   /* Saved foreground color */
    RGBColor     savedBack;   /* Saved background color */
    RGBColor     aColor;      /* Used to set foreground and background color*/
    long         qdVersion;   /* Version of QuickDraw currently in use */
    GrafPtr      savedPort;   /* Pointer to GrafPort used for save/restore */
    GDHandle     savedDevice; /* Handle to GDevice used for save/restore */
    SignedByte   savedState;  /* Saved state of color table handle */
    OSErr        error;       /* Returns error code */

    /* Initialize a few things before we begin */
    newPixMap = nil;
    error = noErr;

    /* Keep the old bounds rectangle, or get the new one */
    if (EmptyRect( newBounds ))
        bounds = updPort->portRect;
    else
        bounds = *newBounds;

    /* Keep the old depth, or get the old one */
    if (newDepth == 0)
        depth = (**updPort->portPixMap).pixelSize;
    else
        depth = newDepth;

    /* Get the old clut, or save new clut's state and make it nonpurgeable */
    if (newColors == nil)
        colors = (**updPort->portPixMap).pmTable;
    else
    {
        savedState = HGetState( (Handle)newColors );
        HNoPurge( (Handle)newColors );
        colors = newColors;
    }

    /* Calculate the number of bytes per row in the off-screen PixMap */
    bytesPerRow = ((depth * (bounds.right - bounds.left) + 31) >> 5)<< 2;

    /* Get the current QuickDraw version */
    (void)Gestalt( gestaltQuickdrawVersion, &qdVersion );

    /* Make sure depth is indexed or depth is direct and 32-Bit QD installed*/
    if (depth == 1 || depth == 2 || depth == 4 || depth == 8 ||
	((depth == 16 || depth == 32) && qdVersion >=gestalt32BitQD))
    {
        /* Maximum number of bytes per row is 16,382; make sure within range*/
        if (bytesPerRow <= kMaxRowBytes)
        {
            /* Make sure a color table is provided if the depth is indexed */
            if (depth <= 8)
                if (colors == nil)
                    /* Indexed depth and clut is NIL; is parameter error */
                    error = paramErr;
        }
        else
            /* # of bytes per row is more than 16,382; is parameter error */
            error = paramErr;
    }
    else
        /* Pixel depth isn't valid; is parameter error */
        error = paramErr;

    /* If sanity checks succeed, attempt to create a new graphics environment*/
    if (error == noErr)
    {
        /* Allocate a new PixMap */
        newPixMap = (PixMapHandle)NewHandleClear( sizeof (PixMap) );
        if (newPixMap != nil)
        {
            /* Initialize the new PixMap for off-screen drawing */
            error = SetUpPixMap( depth, &bounds, colors, bytesPerRow, newPixMap );
            if (error == noErr)
            {
                /* Save the old PixMap and install the new, initialized one*/
                oldPixMap = updPort->portPixMap;
                updPort->portPixMap = newPixMap;

                /* Save current port & GDevice and set ones we're updating*/
                GetPort( &savedPort );
                savedDevice = GetGDevice();
                SetPort( (GrafPtr)updPort );
                SetGDevice( updGDevice );

                /* Set portRect, visRgn, and clipRgn to the given bounds rect*/
                updPort->portRect = bounds;
                RectRgn( updPort->visRgn, &bounds );
                ClipRect( &bounds );

                /* Update the GDevice */
                if ((**newPixMap).pixelSize <= 8)
                    (**updGDevice).gdType = clutType;
                else
                    (**updGDevice).gdType = directType;
                (**updGDevice).gdPMap = newPixMap;
                (**updGDevice).gdRect = (**newPixMap).bounds;

                /* Set color-device bit if PixMap isn't black & white */
                if ((**newPixMap).pixelSize > 1)
                    SetDeviceAttribute( updGDevice, gdDevType, true );
                else
                    SetDeviceAttribute( updGDevice, gdDevType, false );

                /* Save current foreground/background colors and set to B&W*/
                GetForeColor( &savedFore );
                GetBackColor( &savedBack );
                aColor.red = aColor.green = aColor.blue = 0;
                RGBForeColor( &aColor );
                aColor.red = aColor.green = aColor.blue = 0xFFFF;
                RGBBackColor( &aColor );

                /* Copy old image to the new graphics environment */
                HLock( (Handle)oldPixMap );
                CopyBits( (BitMapPtr)*oldPixMap, &((GrafPtr)updPort)->portBits,
			  &(**oldPixMap).bounds, &updPort->portRect,
			  srcCopy, nil );
                HUnlock( (Handle)oldPixMap );

                /* Restore the foreground/background color */
                RGBForeColor( &savedFore );
                RGBBackColor( &savedBack );

                /* Restore the saved port */
                SetPort( savedPort );
                SetGDevice( savedDevice );

                /* Get rid of the old PixMap and its dependents */
                DisposePtr( (**oldPixMap).baseAddr );
                DisposeCTable( (**oldPixMap).pmTable ) ;
                DisposeHandle( (Handle)oldPixMap );
            }
        }
        else
            error = MemError();
    }

    /* Restore the given state of the color table */
    if (colors != nil)
        HSetState( (Handle)colors, savedState );

    /* One Last Look Around The House Before We Go... */
    if (error != noErr)
    {
        /* Some error occurred; dispose of everything we allocated */
        if (newPixMap != nil)
        {
            if ((**newPixMap).pmTable)
                DisposeCTable( (**newPixMap).pmTable );
            if ((**newPixMap).baseAddr)
                DisposePtr ( (**newPixMap).baseAddr );
            DisposeHandle( (Handle)newPixMap );
        }
    }
    return error;
}

void SetUpOffScreen()
{
    CTabHandle offColors;   /* Colors for off-screen environments */
    Rect       offRect;     /* Rectangle of off-screen environments */
    short      count;       /* Generic counter */
    RGBColor   aColor;      /* Color used for drawing off screen */
    OSErr      error;       /* Error return from off-screen creation */

    /* Set up the rectangle for the off-screen graphics environments */
    SetRect( &offRect, 0, 0, 256, 256 );
    offColors = GetCTable( rColorClut );

    /* Create the color off-screen graphics environment */
    error = CreateOffScreen( &offRect, kOffDepth, offColors,
			     &colorPort, &colorDevice );

    if (error == noErr)
    {
        /* Save the current graphics environment */
        GetPort( &savedPort );
        savedDevice = GetGDevice();
        /* Copy gray ramp into color off-screen colorized with green */
        SetPort( (GrafPtr)colorPort );
        SetGDevice( colorDevice );
        EraseRect(&colorPort->portRect);
        SetPort( savedPort );
        SetGDevice( savedDevice );
    }

}

void WriteTo()
{
    RGBColor   aColor;      /* Color used for drawing off screen */

    GetPort( &savedPort );
    savedDevice = GetGDevice();
    /* Copy gray ramp into color off-screen colorized with green */
    SetPort( (GrafPtr)colorPort );
    SetGDevice( colorDevice );

    PenSize( 8, 8 );
    aColor.red = 0xFFFF; aColor.green = 0x0000; aColor.blue = 0x0000;
    RGBForeColor( &aColor );
    circleRect = colorPort->portRect;
    FrameOval( &circleRect );

    SetPort( savedPort );
    SetGDevice( savedDevice );


}

void WriteTo2()
{
    RGBColor   aColor;      /* Color used for drawing off screen */

    GetPort( &savedPort );
    savedDevice = GetGDevice();
    /* Copy gray ramp into color off-screen colorized with green */
    SetPort( (GrafPtr)colorPort );
    SetGDevice( colorDevice );


    PenSize( 8, 8 );
    aColor.red = 0x0000; aColor.green = 0xFFFF; aColor.blue = 0x0000;
    RGBForeColor( &aColor );
    InsetRect( &circleRect, 20, 20 );
    FrameOval( &circleRect );

    SetPort( savedPort );
    SetGDevice( savedDevice );
}

void CopyTo()
{
    CopyBits( &((GrafPtr)colorPort)->portBits,&savedPort->portBits,
	      &colorPort->portRect, &savedPort->portRect,
	      srcCopy, nil );

}

void TurnOff()
{
    DisposeOffScreen( colorPort, colorDevice );
}
#ifdef FFFFF
void ExerciseOffScreen()
{


    GrafPtr    savedPort;   /* Pointer to the saved graphics environment */
    GDHandle   savedDevice; /* Handle to the saved color environment */
    CTabHandle offColors;   /* Colors for off-screen environments */
    Rect       offRect;     /* Rectangle of off-screen environments */
    Rect       circleRect;  /* Rectangles for circle-drawing */
    short      count;       /* Generic counter */
    RGBColor   aColor;      /* Color used for drawing off screen */
    OSErr      error;       /* Error return from off-screen creation */

    /* Set up the rectangle for the off-screen graphics environments */
    SetRect( &offRect, 0, 0, 256, 256 );

#ifdef FFFFF
    /* Get the color table for the gray off-screen graphics environment */
    offColors = GetCTable( rGrayClut );

    /* Create the gray off-screen graphics environment */
    error = CreateOffScreen( &offRect, kOffDepth, offColors,
			     &grayPort, &grayDevice );
#endif
    /*  if (error == noErr) */
    if (true)
    {
        /* Get the color table for the color off-screen graphics environment*/
        offColors = GetCTable( rColorClut );

        /* Create the color off-screen graphics environment */
        error = CreateOffScreen( &offRect, kOffDepth, offColors,
				 &colorPort, &colorDevice );

        if (error == noErr)
        {
            /* Save the current graphics environment */
            GetPort( &savedPort );
            savedDevice = GetGDevice();
#ifdef FFFFF
            /* Set the current graphics environment to the gray one */
            SetPort( (GrafPtr)grayPort );
            SetGDevice( grayDevice );

            /* Draw gray-scale ramp into the gray off-screen environment */
            for (count = 0; count < 256; ++count)
            {
                aColor.red = aColor.green = aColor.blue = count * 257;
                RGBForeColor( &aColor );
                MoveTo( 0, count );
                LineTo( 255, count );
            }
#endif
            /* Copy gray ramp into color off-screen colorized with green */
            SetPort( (GrafPtr)colorPort );
            SetGDevice( colorDevice );
#ifdef FFFFF
            aColor.red = 0x0000; aColor.green = 0xFFFF; aColor.blue = 0x0000;
            RGBForeColor( &aColor );
            CopyBits( &((GrafPtr)grayPort)->portBits,
		      &((GrafPtr)colorPort)->portBits,
		      &grayPort->portRect,
		      &colorPort->portRect,
		      srcCopy | ditherCopy, nil );
#endif
            EraseRect(&colorPort->portRect);
	    /* Draw red, green, and blue circles */
            PenSize( 8, 8 );
            aColor.red = 0x0000; aColor.green = 0x0000; aColor.blue = 0x0000;
            RGBForeColor( &aColor );
            circleRect = colorPort->portRect;
            FrameOval( &circleRect );
            aColor.red = 0x0000; aColor.green = 0xFFFF; aColor.blue = 0x0000;
            RGBForeColor( &aColor );
            InsetRect( &circleRect, 20, 20 );
            FrameOval( &circleRect );
            aColor.red = 0x0000; aColor.green = 0x0000; aColor.blue = 0xFFFF;
            RGBForeColor( &aColor );
            InsetRect( &circleRect, 20, 20 );
            FrameOval( &circleRect );

            /* Copy the color off-screen environment to the current port */
            SetPort( savedPort );
            SetGDevice( savedDevice );
            CopyBits( &((GrafPtr)colorPort)->portBits,&savedPort->portBits,
		      &colorPort->portRect, &savedPort->portRect,
		      srcCopy, nil );

            /* Dispose of the off-screen graphics environments */
#ifdef FFFFF
            DisposeOffScreen( grayPort, grayDevice );
#endif
            DisposeOffScreen( colorPort, colorDevice );
        }
    }
}


#endif
