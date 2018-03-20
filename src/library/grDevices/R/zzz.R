#  File src/library/grDevices/R/zzz.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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
#  https://www.R-project.org/Licenses/

.noGenerics <- TRUE

if (.Platform$OS.type == "windows") {
    utils::globalVariables(c("C_cairoProps", "C_makeQuartzDefault"))
    utils::suppressForeignCheck(c("C_cairoProps", "C_makeQuartzDefault"))
}

.onLoad <- function(libname, pkgname)
{
    if (.Platform$OS.type != "windows" && !.Call(C_cairoProps, 2L))
        X11.options(type = "Xlib")

    extras <- if(.Platform$OS.type == "windows")
        list(windowsTimeouts = c(100L,500L)) else
        list(bitmapType = if(capabilities("aqua")) "quartz"
        else if(.Call(C_cairoProps, 2L)) "cairo" else "Xlib")
    op.grDevices <- c(list(locatorBell = TRUE, device.ask.default = FALSE),
                      extras, list(device = .select_device()))
    toset <- !(names(op.grDevices) %in% names(.Options))
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
