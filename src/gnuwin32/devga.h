/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998--2003  Guido Masarotto and Brian Ripley
 *  Copyright (C) 2004        The R Foundation
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <Graphics.h>
#include <R_ext/Boolean.h>

enum DeviceKinds {SCREEN=0, PRINTER, METAFILE, PNG, JPEG, BMP};

typedef struct {
    /* R Graphics Parameters */
    /* local device copy so that we can detect */
    /* when parameter changes */
    int   col;			   /* Color */
    int   bg;			   /* Background */
    int   fontface;		   /* Typeface */
    int   fontsize, basefontsize;  /* Size in points */
    double fontangle;

    /* devga Driver Specific */
    /* parameters with copy per devga device */

    enum DeviceKinds kind;
    int   windowWidth;		/* Window width (pixels) */
    int   windowHeight;		/* Window height (pixels) */
    int   showWidth;		/* device width (pixels) */
    int   showHeight;		/* device height (pixels) */
    int   origWidth, origHeight, xshift, yshift;
    Rboolean resize;		/* Window resized */
    window gawin;		/* Graphics window */
  /*FIXME: we should have union for this stuff and
    maybe change gawin to canvas*/
  /* SCREEN section*/
    popup locpopup, grpopup;
    button  stoploc;
    menubar mbar, mbarloc, mbarconfirm;
    menu  msubsave;
    menuitem mpng, mbmp, mjpeg50, mjpeg75, mjpeg100;
    menuitem mps, mpdf, mwm, mclpbm, mclpwm, mprint, mclose;
    menuitem mrec, madd, mreplace, mprev, mnext, mclear, msvar, mgvar;
    menuitem mR, mfit, mfix, grmenustayontop, mnextplot;
    Rboolean recording, replaying, needsave;
    bitmap bm;
  /* PNG and JPEG section */
    FILE *fp;
    char filename[512];
    int quality;
    int npage;
    double w, h;
  /* Used to rescale font size so that bitmap devices have 72dpi */
    int truedpi, wanteddpi;
    rgb   fgcolor;		/* Foreground color */
    rgb   bgcolor;		/* Background color */
    rgb   canvascolor;		/* Canvas color */
    rgb   outcolor;		/* Outside canvas color */
    rect  clip;			/* The clipping rectangle */
    Rboolean usefixed;
    font  fixedfont;
    font  font;
    char fontfamily[50];

    Rboolean locator;
    Rboolean confirmation;
    
    int clicked; /* {0,1,2} */
    int	px, py, lty, lwd;
    int resizing; /* {1,2,3} */
    double rescale_factor;
    int fast; /* Use fast fixed-width lines? */
    unsigned int pngtrans; /* what PNG_TRANS get mapped to */
    Rboolean buffered;
    int timeafter, timesince;
    SEXP psenv;
    double res_dpi;
    R_GE_lineend lend;
    R_GE_linejoin ljoin;
    float lmitre;
    Rboolean enterkey; /* Set true when enter key is hit */
    SEXP eventRho;     /* Environment during event handling */
    SEXP eventResult;  /* Result of event handler */
} gadesc;
