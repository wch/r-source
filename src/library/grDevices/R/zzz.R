#  File src/library/grDevices/R/zzz.R
#  Part of the R package, http://www.R-project.org
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

.onLoad <- function(libname, pkgname)
{
    op <- options()
    extras <- if(.Platform$OS.type == "windows")
        list(graphics.record = FALSE,
             windowsBuffered = TRUE,
             windowsTimeouts = c(100L,500L))
    else
        ## these must be set for x11 to be used, even non-interactively
        list(X11colortype = "true",
             X11fonts = c("-adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*",
             "-adobe-symbol-medium-r-*-*-%d-*-*-*-*-*-*-*"))
    defdev <- Sys.getenv("R_DEFAULT_DEVICE")
    if(!nzchar(defdev)) defdev <- "pdf"
    device <- if(interactive()) {
        intdev <- Sys.getenv("R_INTERACTIVE_DEVICE")
        if(nzchar(intdev)) intdev
        else {
            if(.Platform$OS.type == "windows") "windows"
            else if (.Platform$GUI == "AQUA") "quartz"
            else if (!is.null(Sys.info) && (Sys.info()["sysname"] == "Darwin")
                     && (Sys.getenv("DISPLAY") != "")) "X11"
            else if (Sys.getenv("DISPLAY") != "")
                switch(.Platform$GUI, "Tk" = "X11",
                       "X11" = "X11", "GNOME" = "X11", defdev)
            else defdev
        }
    } else defdev

    op.utils <- c(list(locatorBell = TRUE, par.ask.default = FALSE),
                  extras, device=device)
    toset <- !(names(op.utils) %in% names(op))
    if(any(toset)) options(op.utils[toset])
}

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
