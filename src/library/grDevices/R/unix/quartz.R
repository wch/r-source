#  File src/library/grDevices/R/unix/quartz.R
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

.Quartzenv <- new.env()

assign(".quartz.Options",
       list(title = "Quartz %d",
            width = 7, height = 7, pointsize = 12,
            family = "Helvetica",
            antialias = TRUE,
            type = "native",
            bg = "transparent", canvas = "white",
            dpi = NA_real_),
       envir = .Quartzenv)

assign(".quartz.Options.default",
       get(".quartz.Options", envir = .Quartzenv),
       envir = .Quartzenv)

quartz.options <- function(..., reset = FALSE)
{
    old <- get(".quartz.Options", envir = .Quartzenv)
    if(reset) {
        assign(".quartz.Options",
               get(".quartz.Options.default", envir = .Quartzenv),
               envir = .Quartzenv)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".quartz.Options", envir = .Quartzenv,
                  assign.opt = l... > 0L)
    if(reset || l... > 0L) invisible(old) else old
}

quartz <- function(title, width, height, pointsize, family, antialias,
                   type, file = NULL, bg, canvas, dpi)
{
    if (missing(type) || type %in% c("", "native", "Cocoa")) {
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop"))
            stop(msg, domain = NA)
        else if (identical(check, "warn"))
            warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
    }

    new <- list()
    if(!missing(title)) new$title <- title
    if(!missing(width)) new$width <- width
    if(!missing(height)) new$height <- height
    if(!missing(pointsize)) new$pointsize <- pointsize
    if(!missing(family)) new$family <- family
    if(!missing(antialias)) new$antialias <- antialias
    if(!missing(bg)) new$bg <- bg
    if(!missing(canvas)) new$canvas <- canvas
    if(!missing(type)) new$type <- type
    if(!missing(dpi)) new$dpi <- dpi
    if(!checkIntFormat(new$title)) stop("invalid 'title'")
    if(!is.null(file) && !checkIntFormat(file)) stop("invalid 'file'")
    d <- check.options(new, name.opt = ".quartz.Options", envir = .Quartzenv)
    .External(C_Quartz, d$type, file, d$width, d$height, d$pointsize, d$family,
              d$antialias, d$title, d$bg, d$canvas,
              if(is.na(d$dpi)) NULL else d$dpi)
    invisible()
}

#########
# QUARTZ font database
# To map device-independent font to device-specific font
#########

# Each font family has only a name
assign(".Quartz.Fonts", list(), envir = .Quartzenv)

# Check that the font has the correct structure and information
checkQuartzFont <- function(font) {
    if (!is.character(font) || length(font) != 4)
        stop("invalid Quartz font:  must be 4 strings")
    font
}

setQuartzFonts <- function(fonts, fontNames) {
    fonts <- lapply(fonts, checkQuartzFont)
    fontDB <- get(".Quartz.Fonts", envir=.Quartzenv)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0L)
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(".Quartz.Fonts", fontDB, envir=.Quartzenv)
}

printFont <- function(font) {
    paste0(font, "\n")
}

printFonts <- function(fonts) {
    cat(paste0(names(fonts), ": ", unlist(lapply(fonts, printFont)),
               collapse=""))
}

# If no arguments spec'ed, return entire font database
# If no named arguments spec'ed, all args should be font names
# to get info on from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
quartzFonts <- function(...) {
    ndots <- length(fonts <- list(...))
    if (ndots == 0L)
        get(".Quartz.Fonts", envir=.Quartzenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
            if (!all(sapply(fonts, is.character)))
                stop("invalid arguments in 'quartzFonts' (must be font names)")
            else
                get(".Quartz.Fonts", envir=.Quartzenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("invalid arguments in 'quartzFonts' (need named args)")
            setQuartzFonts(fonts, fontNames)
        }
    }
}

# Create a valid quartz font description
quartzFont <- function(family) {
    checkQuartzFont(family)
}

quartzFonts(# Default Serif font is Times
            serif = quartzFont(c("Times-Roman", "Times-Bold",
            "Times-Italic", "Times-BoldItalic")),
            ## Default Sans Serif font is Helvetica,
            ## even the device default is Arial
            sans = quartzFont(c("Helvetica", "Helvetica-Bold",
            "Helvetica-Oblique", "Helvetica-BoldOblique")),
            ## Default Monospace font is Courier
            mono = quartzFont(c("Courier", "Courier-Bold",
            "Courier-Oblique", "Courier-BoldOblique")))

## Formerly for R.app only
quartz.save <- function(file, type = 'png', device = dev.cur(), dpi = 100, ...)
{
    ## modified version of dev.copy2pdf
    dev.set(device)
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if (nm == "null device") stop("no device to print from")
    if (!dev.displaylist()) stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- as.name("dev.copy")
    oc$file <- NULL
    oc$device <- quartz
    oc$type <- type
    if(missing(file)) file <- paste0("Rplot.", type)
    oc$file <- file
    oc$dpi <- dpi
    din <- dev.size("in")
    w <- din[1L]
    h <- din[2L]
    if (is.null(oc$width))
        oc$width <- if (!is.null(oc$height)) w/h * eval.parent(oc$height) else w
    if (is.null(oc$height))
        oc$height <- if (!is.null(oc$width)) h/w * eval.parent(oc$width) else h
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}
