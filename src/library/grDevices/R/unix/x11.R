#  File src/library/grDevices/R/unix/x11.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

## An environment not exported from namespace:grDevices used to
## pass .X11.Fonts to the X11 device.
.X11env <- new.env()

assign(".X11.Options",
       list(display = "",
            width = NA_real_, height = NA_real_, pointsize = 12,
            bg = "transparent", canvas = "white",
            gamma = 1,
            colortype = "true", maxcubesize = 256,
            fonts = c("-adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*",
            "-adobe-symbol-medium-r-*-*-%d-*-*-*-*-*-*-*"),
            family = "sans",
            xpos = NA_integer_, ypos = NA_integer_,
	    title = "", type = "cairo", antialias = "default"),
       envir = .X11env)

assign(".X11.Options.default",
       get(".X11.Options", envir = .X11env),
       envir = .X11env)

aa.cairo  <- c("default", "none", "gray", "subpixel")

X11.options <- function(..., reset = FALSE)
{
    old <- get(".X11.Options", envir = .X11env)
    if(reset) {
        assign(".X11.Options",
               get(".X11.Options.default", envir = .X11env),
               envir = .X11env)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".X11.Options", envir = .X11env,
                  assign.opt = l... > 0)
    if(reset || l... > 0) invisible(old) else old
}

check_for_XQuartz <- function()
{
    DSO <- file.path(R.home("modules"), "R_X11.so")
    out <- system2("otool", c("-L", shQuote(DSO)), stdout = TRUE)
    ind <- grep("libX11[.][0-9]+[.]dylib", out)
    if(length(ind)) {
        this <- sub(" .*", "", sub("^\t", "", out[ind]))
        if(!file.exists(this))
            stop("X11 library is missing: install XQuartz from xquartz.macosforge.org", domain = NA)
    }
}


X11 <- function(display = "", width, height, pointsize, gamma,
                bg, canvas, fonts, family,
                xpos, ypos, title, type, antialias)
{
    if(display != "XImage") { # used by tkrplot
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop"))
            stop(msg, domain = NA)
        else if (identical(check, "warn"))
            warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
    }

    if(display == "" && .Platform$GUI == "AQUA" &&
       is.na(Sys.getenv("DISPLAY", NA))) Sys.setenv(DISPLAY = ":0")

    new <- list()
    if(!missing(display)) new$display <- display
    if(!missing(width)) new$width <- width
    if(!missing(height)) new$height <- height
    if(!missing(gamma)) new$gamma <- gamma
    if(!missing(pointsize)) new$pointsize <- pointsize
    if(!missing(bg)) new$bg <- bg
    if(!missing(canvas)) new$canvas <- canvas
    if(!missing(xpos)) new$xpos <- xpos
    if(!missing(ypos)) new$ypos <- ypos
    if(!missing(title)) new$title <- title
    if(!checkIntFormat(new$title)) stop("invalid 'title'")
    if(!missing(type)) {
        new$type <- match.arg(type, c("Xlib", "cairo", "nbcairo", "dbcairo"))
        if(!capabilities("cairo") && type != "Xlib")
            warning("cairo-based types are not supported on this build - using \"Xlib\"")
    }
    if(!missing(family)) new$family <- family
    if(!missing(fonts)) new$fonts <- fonts
    if(!missing(antialias) && type != "Xlib")
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    if(d$type == "Xlib" && !missing(family)) {
        fns <- X11Fonts()
        if (! family %in% names(fns))
            stop('unknown family for X11(type = "XLib")')
        d$fonts[1] <- fns[[family]]
    }
    type <-
	if(capabilities("cairo"))
            switch(d$type, "cairo" = 1L, "nbcairo" = 2L, "dbcairo" = 3L, 0L)
	else 0L
    ## Aargh -- trkplot has a trapdoor and does not set type.
    if (display == "XImage") type <- 0L
    antialias <- match(d$antialias, aa.cairo)
    if (grepl("darwin", R.version$os)) check_for_XQuartz()
    .External2(C_X11, d$display, d$width, d$height, d$pointsize, d$gamma,
               d$colortype, d$maxcubesize, d$bg, d$canvas, d$fonts,
               NA_integer_, d$xpos, d$ypos, d$title,
               type, antialias, d$family)
    invisible()
}

x11 <- X11


####################
# X11 font database
####################

assign(".X11.Fonts", list(), envir = .X11env)

X11FontError <- function(errDesc)
    stop("invalid X11 font specification: ", errDesc)


# Check that the font has the correct structure and information
# Already checked that it had a name
checkX11Font <- function(font)
{
    if (!is.character(font))
        X11FontError("must be a string")
    ## Check it has the right format
    if (length(grep("(-[^-]+){14}", font)) > 0) {
        ## Force the %s and %d substitution formats into the right spots
        font <- sub("((-[^-]+){2})(-[^-]+){2}((-[^-]+){2})(-[^-]+)((-[^-]+){7})",
                    "\\1-%s-%s\\4-%d\\7", font, perl = TRUE)
    } else {
        X11FontError("incorrect format")
    }
    font
}

setX11Fonts <- function(fonts, fontNames)
{
    fonts <- lapply(fonts, checkX11Font)
    fontDB <- get(".X11.Fonts", envir=.X11env)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0)
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(".X11.Fonts", fontDB, envir=.X11env)
}

printFont <- function(font) paste(font, "\n", sep="")


printFonts <- function(fonts)
    cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
              sep="", collapse=""))

# If no arguments spec'ed, return entire font database
# If no named arguments spec'ed, all args should be font names
# to get info on from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid X11 font descriptions and
# all of which must be named args)
X11Fonts <- function(...)
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0)
        get(".X11.Fonts", envir=.X11env)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop("invalid arguments in 'X11Fonts' (must be font names)")
            else
                get(".X11.Fonts", envir=.X11env)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("invalid arguments in 'X11Fonts' (need named args)")
            setX11Fonts(fonts, fontNames)
        }
    }
}

# Create a valid X11 font description
X11Font <- function(font) checkX11Font(font)

X11Fonts(# Default Serif font is Times
         serif = X11Font("-*-times-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         # Default Sans Serif font is Helvetica
         sans = X11Font("-*-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         # Default Monospace font is Courier
         mono = X11Font("-*-courier-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         Times = X11Font("-adobe-times-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         Helvetica = X11Font("-adobe-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         CyrTimes = X11Font("-cronyx-times-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         CyrHelvetica = X11Font("-cronyx-helvetica-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         Arial = X11Font("-monotype-arial-%s-%s-*-*-%d-*-*-*-*-*-*-*"),
         Mincho = X11Font("-*-mincho-%s-%s-*-*-%d-*-*-*-*-*-*-*")
         )

savePlot <- function(filename = paste("Rplot", type, sep="."),
                     type = c("png", "jpeg", "tiff", "bmp"),
                     device = dev.cur())
{
    type <- match.arg(type)
    devlist <- dev.list()
    devcur <- match(device, devlist, NA)
    if(is.na(devcur)) stop("no such device")
    devname <- names(devlist)[devcur]
    if(devname != "X11cairo")
        stop("can only copy from 'X11(type=\"*cairo\")' devices")
    invisible(.External2(C_savePlot, filename, type, device))
}
