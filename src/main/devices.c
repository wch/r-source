/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2004   The R Development Core Team.
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <Graphics.h>
#include <Rdevices.h>


/*  PostScript Device Driver Parameters:
 *  ------------------------		--> devPS.c
 *  file	= output filename
 *  paper	= paper type
 *  family	= typeface = "family"
 *  encoding	= char encoding file name
 *  bg		= background color
 *  fg		= foreground color
 *  width	= width in inches
 *  height	= height in inches
 *  horizontal	= {TRUE: landscape; FALSE: portrait}
 *  ps		= pointsize
 *  onefile     = {TRUE: normal; FALSE: single EPSF page}
 *  pagecentre  = centre plot region on paper?
 *  printit     = `print' after closing device?
 *  command     = `print' command
 *  title       = character string
 */

SEXP do_PS(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    char *vmax;
    char *file, *paper, *family=NULL, *bg, *fg, *cmd;
    char *afms[5], *encoding, *title;
    int i, horizontal, onefile, pagecentre, printit;
    double height, width, ps;
    SEXP fam, fonts;

    vmax = vmaxget();
    file = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    paper = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);

    /* `family' can be either one string or a 5-vector of afmpaths. */
    fam = CAR(args); args = CDR(args);
    if(length(fam) == 1) 
	family = CHAR(STRING_ELT(fam, 0));
    else if(length(fam) == 5) {
	if(!isString(fam)) errorcall(call, "invalid `family' parameter");
	family = "User";
	for(i = 0; i < 5; i++) afms[i] = CHAR(STRING_ELT(fam, i));
    } else 
	errorcall(call, "invalid `family' parameter");
    
    encoding = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    bg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    fg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    width = asReal(CAR(args));	      args = CDR(args);
    height = asReal(CAR(args));	      args = CDR(args);
    horizontal = asLogical(CAR(args));args = CDR(args);
    if(horizontal == NA_LOGICAL)
	horizontal = 1;
    ps = asReal(CAR(args));	      args = CDR(args);
    onefile = asLogical(CAR(args));   args = CDR(args);
    pagecentre = asLogical(CAR(args));args = CDR(args);
    printit = asLogical(CAR(args));   args = CDR(args);
    cmd = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    title = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);    
    fonts = CAR(args); 
    if (!isNull(fonts) && !isString(fonts))
	errorcall(call, "invalid `fonts' parameter");
    
    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if(!PSDeviceDriver((DevDesc*) dev, file, paper, family, afms, encoding, bg, fg,
			   width, height, (double)horizontal, ps, onefile,
			   pagecentre, printit, cmd, title, fonts)) {
	    free(dev);
	    errorcall(call, "unable to start device PostScript");
	}
	gsetVar(install(".Device"), mkString("postscript"), R_NilValue);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}



/*  XFig Device Driver Parameters:
 *  ------------------------		--> devPS.c
 *  file	= output filename
 *  paper	= paper type
 *  family	= typeface = "family"
 *  bg		= background color
 *  fg		= foreground color
 *  width	= width in inches
 *  height	= height in inches
 *  horizontal	= {TRUE: landscape; FALSE: portrait}
 *  ps		= pointsize
 *  onefile     = {TRUE: normal; FALSE: single EPSF page}
 *  pagecentre  = centre plot region on paper?
 */

SEXP do_XFig(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    char *vmax;
    char *file, *paper, *family, *bg, *fg;
    int horizontal, onefile, pagecentre;
    double height, width, ps;

    vmax = vmaxget();
    file = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    paper = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    family = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    bg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    fg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    width = asReal(CAR(args));	      args = CDR(args);
    height = asReal(CAR(args));	      args = CDR(args);
    horizontal = asLogical(CAR(args));args = CDR(args);
    if(horizontal == NA_LOGICAL)
	horizontal = 1;
    ps = asReal(CAR(args));	      args = CDR(args);
    onefile = asLogical(CAR(args));   args = CDR(args);
    pagecentre = asLogical(CAR(args));

    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if(!XFigDeviceDriver((DevDesc*) dev, file, paper, family, bg, fg, width, height,
			     (double)horizontal, ps, onefile, pagecentre)) {
	    free(dev);
	    errorcall(call, "unable to start device xfig");
	}
	gsetVar(install(".Device"), mkString("xfig"), R_NilValue);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}


/*  PDF Device Driver Parameters:
 *  ------------------------		--> devPS.c
 *  file	= output filename
 *  family	= typeface = "family"
 *  encoding	= char encoding file name
 *  bg		= background color
 *  fg		= foreground color
 *  width	= width in inches
 *  height	= height in inches
 *  ps		= pointsize
 *  onefile     = {TRUE: normal; FALSE: single page per file}
 *  title
 *  fonts
 *  versionMajor
 *  versionMinor
 */

SEXP do_PDF(SEXP call, SEXP op, SEXP args, SEXP env)
{
    NewDevDesc *dev = NULL;
    GEDevDesc *dd;
    char *vmax;
    char *file, *encoding, *family, *bg, *fg, *title;
    double height, width, ps;
    int onefile, major, minor;
    SEXP fonts;

    vmax = vmaxget();
    file = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    family = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    encoding = CHAR(STRING_ELT(CAR(args), 0));  args = CDR(args);
    bg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    fg = CHAR(STRING_ELT(CAR(args), 0));    args = CDR(args);
    width = asReal(CAR(args));	      args = CDR(args);
    height = asReal(CAR(args));	      args = CDR(args);
    ps = asReal(CAR(args));           args = CDR(args);
    onefile = asLogical(CAR(args)); args = CDR(args);
    title = CHAR(STRING_ELT(CAR(args), 0)); args = CDR(args);
    fonts = CAR(args); args = CDR(args);
    if (!isNull(fonts) && !isString(fonts))
	errorcall(call, "invalid `fonts' parameter");
    major = asInteger(CAR(args)); args = CDR(args);
    minor = asInteger(CAR(args)); 

    R_CheckDeviceAvailable();
    BEGIN_SUSPEND_INTERRUPTS {
	if (!(dev = (NewDevDesc *) calloc(1, sizeof(NewDevDesc))))
	    return 0;
	/* Do this for early redraw attempts */
	dev->displayList = R_NilValue;
	/* Make sure that this is initialised before a GC can occur.
	 * This (and displayList) get protected during GC
	 */
	dev->savedSnapshot = R_NilValue;
	if(!PDFDeviceDriver((DevDesc*) dev, file, family, encoding, bg, fg, 
			    width, height, ps, onefile, title, fonts,
			    major, minor)) {
	    free(dev);
	    errorcall(call, "unable to start device pdf");
	}
	gsetVar(install(".Device"), mkString("pdf"), R_NilValue);
	dd = GEcreateDevDesc(dev);
	addDevice((DevDesc*) dd);
	GEinitDisplayList(dd);
    } END_SUSPEND_INTERRUPTS;
    vmaxset(vmax);
    return R_NilValue;
}
