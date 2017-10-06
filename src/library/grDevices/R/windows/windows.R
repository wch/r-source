#  File src/library/grDevices/R/windows/windows.R
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

.WindowsEnv <- new.env()

assign(".Windows.Options",
       list(width = 7, height = 7, pointsize = 12,
            record = FALSE,
            rescale = "R",
            xpinch = NA_real_, ypinch = NA_real_,
            bg = "transparent", canvas = "white",
            gamma = 1,
            xpos = NA_integer_, ypos = NA_integer_,
            buffered = TRUE,
            restoreConsole = FALSE,
            clickToConfirm = TRUE,
            title = "",
            fillOddEven = TRUE,
            antialias = "default",
            bitmap.aa.win = "default"),
       envir = .WindowsEnv)

assign(".Windows.Options.default",
       get(".Windows.Options", envir = .WindowsEnv),
       envir = .WindowsEnv)

aa.win <- c("default", "none", "cleartype", "gray")
aa.cairo <- c("default", "none", "gray", "subpixel")

windows.options <- function(..., reset=FALSE)
{
    old <- get(".Windows.Options", envir = .WindowsEnv)
    if(reset) {
        assign(".Windows.Options",
               get(".Windows.Options.default", envir = .WindowsEnv),
               envir = .WindowsEnv)
    }
    l... <- length(new <- list(...))
    check.options(new = new, envir = .WindowsEnv,
                  name.opt = ".Windows.Options",
                  assign.opt = l... > 0)
    if(reset || l... > 0) invisible(old) else old
}

windows <-
    function(width, height, pointsize,
             record, rescale, xpinch, ypinch,
             bg, canvas, gamma, xpos, ypos,
             buffered, title, restoreConsole, clickToConfirm,
             fillOddEven, family = "", antialias)
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "screen devices should not be used in examples etc"
    if (identical(check, "stop"))
        stop(msg, domain = NA)
    else if (identical(check, "warn"))
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)

    new <- list()
    if(!missing(width)) new$width <- as.double(width)
    if(!missing(height)) new$height <- as.double(height)
    if(!missing(pointsize)) new$pointsize <- as.double(pointsize)
    if(!missing(record)) new$record <- record
    if(!missing(rescale)) new$rescale <- rescale
    if(!missing(xpinch)) new$xpinch <- as.double(xpinch)
    if(!missing(ypinch)) new$ypinch <- as.double(ypinch)
    if(!missing(bg)) new$bg <- bg
    if(!missing(canvas)) new$canvas <- canvas
    if(!missing(gamma)) new$gamma <- gamma
    if(!missing(xpos)) new$xpos <- as.integer(xpos)
    if(!missing(ypos)) new$ypos <- as.integer(ypos)
    if(!missing(buffered)) new$buffered <- buffered
    if(!missing(title)) new$title <- title
    if(!missing(restoreConsole)) new$restoreConsole <- restoreConsole
    if(!missing(clickToConfirm)) new$clickToConfirm <- clickToConfirm
    if(!missing(fillOddEven)) new$fillOddEven <- fillOddEven
    if(!missing(antialias)) new$antialias <- match.arg(antialias, aa.win)
    d <- check.options(new = new, envir = .WindowsEnv,
                       name.opt = ".Windows.Options",
                       reset = FALSE, assign.opt = FALSE)
    rescale <- match(d$rescale, c("R", "fit", "fixed"))
    if(is.na(rescale)) stop("invalid value for 'rescale'")
    antialias <- pmatch(d$antialias, aa.win)
    invisible(.External(C_devga, "", d$width, d$height, d$pointsize,
                        d$record, rescale, d$xpinch, d$ypinch,
                        d$canvas, d$gamma, d$xpos, d$ypos,
                        d$buffered, .PSenv, d$bg,
                        d$restoreConsole, d$title, d$clickToConfirm,
                        d$fillOddEven, family, antialias))
}

## historical wrappers
win.graph <- function(width, height, pointsize)
    windows(width = width, height = height, pointsize = pointsize)

x11 <- X11 <-
     function(width, height, pointsize, bg, gamma, xpos, ypos, title)
    windows(width = width, height = height, pointsize = pointsize,
            bg = bg, gamma = gamma,
            xpos = xpos, ypos = ypos, title = title)

win.print <-
    function(width = 7, height = 7, pointsize = 12, printer = "",
             family = "", antialias = "default", restoreConsole = TRUE)
{
    check <- Sys.getenv("_R_CHECK_WINDOWS_DEVICE_", "")
    if (identical(check, "stop"))
        stop("windows devices should not be used in examples etc", domain = NA)

    antialias <- match(match.arg(antialias, aa.win), aa.win)
    invisible(.External(C_devga, paste0("win.print:", printer),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, "white", 1,
                        NA_integer_, NA_integer_,
                        FALSE, .PSenv, NA, restoreConsole, "", FALSE,
                        TRUE, family, antialias))
}

win.metafile <-
    function(filename = "", width = 7, height = 7, pointsize = 12,
             family = "", restoreConsole = TRUE)
{
    check <- Sys.getenv("_R_CHECK_WINDOWS_DEVICE_", "")
    if (identical(check, "stop"))
        stop("windows devices should not be used in examples etc", domain = NA)

    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    filename <- path.expand(filename)
    invisible(.External(C_devga, paste0("win.metafile:", filename),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, "white", 1,
                        NA_integer_, NA_integer_, FALSE, .PSenv, NA,
                        restoreConsole, "", FALSE, TRUE, family, 1L))
}

bringToTop <- function(which = dev.cur(), stay = FALSE)
{
    if(!exists(".Devices")) {
	.Devices <- list("null device")
    }
    if(which > 0 && .Devices[[which]] != "windows")
        stop("can only bring windows devices to the front")
    .Call(C_bringToTop, as.integer(which), as.logical(stay))
    invisible()
}

msgWindow <-
    function(type = c("minimize", "restore", "maximize", "hide",
                      "recordOn", "recordOff"),
             which = dev.cur())
{
    itype <- match(match.arg(type), eval(formals()$type))
    if(which == -1 && itype > 3L)
        stop("'type' not applicable to the R console")
    if(!exists(".Devices")) .Devices <- list("null device")
    if(which > 0 && .Devices[[which]] != "windows")
        stop("can only manipulate windows devices")
    .Call(C_msgWindow, as.integer(which), as.integer(itype))
    invisible()
}

savePlot <- function(filename = "Rplot",
                     type = c("wmf", "emf", "png", "jpg", "jpeg", "bmp",
                              "tif", "tiff", "ps", "eps", "pdf"),
                     device = dev.cur(),
                     restoreConsole = TRUE)
{
    type <- match.arg(type)
    devlist <- dev.list()
    devcur <- match(device, devlist, NA)
    if(is.na(devcur)) stop("no such device")
    devname <- names(devlist)[devcur]
    if(devname != "windows") stop("can only copy from 'windows' devices")
    if(filename == "clipboard" && type == "wmf") filename <- ""
    else if(regexpr("\\.",filename) < 0)
        filename <- paste(filename, type, sep = ".")
    filename <- path.expand(filename)
    invisible(.External(C_savePlot, device, filename, type, restoreConsole))
}

print.SavedPlots <- function(x, ...)
{
    if(x[[1L]] != 31416) {
        cat("object is not of class `SavedPlots'\n")
        return()
    }
    cat("Saved Plots from R version 1.4.0 or later\n\n")
    cat("  Contains", x[[2L]], "out of a maximum", x[[3L]], "plots\n")
    lens <- sapply(x[[5L]], length)[1L:x[[2L]]]
    cat("  #plot calls are", paste(lens, collapse=", "), "\n")
    cat("  Current position is plot", 1L + x[[4L]], "\n")
    invisible(x)
}

`[.SavedPlots` <- function(x, i, ...)
{
    numplots <- x[[2L]]
    if(i > numplots || i < 1) stop("subscript out of range")
    x[[5]][[i]]
}

#########
# WINDOWS font database
# To map device-independent font to device-specific font
#########

# Each font family has only a name
assign(".Windows.Fonts", list(), envir = .WindowsEnv)

# Check that the font has the correct structure and information
checkWindowsFont <- function(font)
{
    # For now just use the simple format that is used in Rdevga
    # i.e., just a font family name, possibly with "TT" as the first
    # two characters to indicate a TrueType font
    if (!is.character(font) || length(font) != 1)
        stop("invalid Windows font:  must be a single font family name")
    font
}

setWindowsFonts <- function(fonts, fontNames)
{
    fonts <- lapply(fonts, checkWindowsFont)
    fontDB <- get(".Windows.Fonts", envir=.WindowsEnv)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0)
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(".Windows.Fonts", fontDB, envir=.WindowsEnv)
}

printFont <- function(font) paste0(font, "\n")

printFonts <- function(fonts)
    cat(paste0(names(fonts), ": ", unlist(lapply(fonts, printFont)),
               collapse=""))

# If no arguments spec'ed, return entire font database
# If no named arguments spec'ed, all args should be font names
# to get info on from the database
# Else, must specify new fonts to enter into database (all
# of which must be valid PostScript font descriptions and
# all of which must be named args)
windowsFonts <- function(...)
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0)
        get(".Windows.Fonts", envir=.WindowsEnv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop("invalid arguments in 'windowsFonts' (must be font names)")
            else
                get(".Windows.Fonts", envir=.WindowsEnv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("invalid arguments in 'windowsFonts' (need named args)")
            setWindowsFonts(fonts, fontNames)
        }
    }
}

# Create a valid windows font description
windowsFont <- function(family)
    checkWindowsFont(family)


windowsFonts(# Default Serif font is Times-like
             serif = windowsFont("TT Times New Roman"),
             # Default Sans Serif font is Helvetica-like
             sans = windowsFont("TT Arial"),
             # Default Monospace font is Courier-like
             mono = windowsFont("TT Courier New"))
