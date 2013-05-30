#  File src/library/grDevices/R/zzz.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

.noGenerics <- TRUE

if (.Platform$OS.type == "windows")
    utils::globalVariables(c("C_cairoProps", "C_makeQuartzDefault"), add = TRUE)
    
.onLoad <- function(libname, pkgname)
{
    op <- options()
    extras <- if(.Platform$OS.type == "windows")
        list(windowsTimeouts = c(100L,500L)) else
    list(bitmapType = if(capabilities("aqua")) "quartz"
    else if(.Call(C_cairoProps, 2L)) "cairo" else "Xlib")
    defdev <- Sys.getenv("R_DEFAULT_DEVICE")
    ## Use devices rather than names to make it harder to get masked.
    if(!nzchar(defdev)) defdev <- pdf
    device <- if(interactive()) {
        intdev <- Sys.getenv("R_INTERACTIVE_DEVICE")
        if(nzchar(intdev)) intdev
        else {
            dsp <- Sys.getenv("DISPLAY")
            if(.Platform$OS.type == "windows") windows
            else if (.Platform$GUI == "AQUA" ||
                     ((!nzchar(dsp) || grepl("^/tmp/launch-", dsp))
                      && .Call(C_makeQuartzDefault))) quartz
            else if (nzchar(dsp) && .Platform$GUI %in% c("X11", "Tk")) X11
	    else defdev
        }
    } else defdev

    if (.Platform$OS.type != "windows" && !.Call(C_cairoProps, 2L))
        X11.options(type = "Xlib")
    op.grDevices <- c(list(locatorBell = TRUE, device.ask.default = FALSE),
                  extras, device = device)
    toset <- !(names(op.grDevices) %in% names(op))
    if(any(toset)) options(op.grDevices[toset])
}

.onUnload <- function(libpath)
    library.dynam.unload("grDevices", libpath)


### Used by text, mtext, strwidth, strheight, title, axis,
### L_text and L_textBounds, all of which
### coerce SYMSXPs and LANGSXPs to EXPRSXPs
### We don't want to use as.expression here as that is generic
### even though is.language no longer is

### Possibly later have
### if (is.language(x)) x
### else if(isS4(x)) methods::as(x, "character")
### else if(is.object(x)) as.character(x)
### else x

as.graphicsAnnot <- function(x)
    if(is.language(x) || !is.object(x)) x else as.character(x)
