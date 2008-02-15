#  File src/library/grDevices/R/windows/windows.R
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
            title = ""),
       envir = .WindowsEnv)

assign(".Windows.Options.default",
       get(".Windows.Options", envir = .WindowsEnv),
       envir = .WindowsEnv)

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

windows <- function(width, height, pointsize,
                    record, rescale, xpinch, ypinch,
                    bg, canvas, gamma, xpos, ypos,
                    buffered, title, restoreConsole)
{
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
    if(!missing(canvas)) new$gamma <- gamma
    if(!missing(xpos)) new$xpos <- as.integer(xpos)
    if(!missing(ypos)) new$ypos <- as.integer(ypos)
    if(!missing(buffered)) new$buffered <- buffered
    if(!missing(title)) new$title <- title
    if(!missing(restoreConsole)) new$restoreConsole <- restoreConsole
    old <- check.options(new = new, envir = .WindowsEnv,
                         name.opt = ".Windows.Options",
			 reset = FALSE, assign.opt = FALSE)
    rescale <- pmatch(old$rescale, c("R", "fit", "fixed"))
    if(is.na(rescale)) stop("invalid value for 'rescale")
    invisible(.External(Cdevga, "", old$width, old$height, old$pointsize,
                        old$record, rescale, old$xpinch, old$ypinch,
                        old$canvas, old$gamma, old$xpos, old$ypos,
                        old$buffered, .PSenv, old$bg,
                        old$restoreConsole, old$title))
}

win.graph <- function(width, height, pointsize)
{
    new <- list()
    if(!missing(width)) new$width <- as.double(width)
    if(!missing(height)) new$height <- as.double(height)
    if(!missing(pointsize)) new$pointsize <- as.double(pointsize)
    old <- check.options(new = new, envir = .WindowsEnv,
                         name.opt = ".Windows.Options",
			 reset = FALSE, assign.opt = FALSE)
    invisible(.External(Cdevga, "", old$width, old$height, old$pointsize,
                        FALSE, 1L, old$xpinch, old$ypinch, "white",
                        old$gamma, NA_integer_, NA_integer_, old$buffered,
                        .PSenv, NA, old$restoreConsole, ""))
}

win.print <- function(width = 7, height = 7, pointsize = 12, printer = "",
                      restoreConsole = TRUE)
    invisible(.External(Cdevga, paste("win.print:", printer, sep=""),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, "white", 1,
                        NA_integer_, NA_integer_,
                        FALSE, .PSenv, NA, restoreConsole, ""))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12,
                         restoreConsole = TRUE)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    invisible(.External(Cdevga, paste("win.metafile:", filename, sep=""),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, "white", 1,
                        NA_integer_, NA_integer_, FALSE, .PSenv, NA,
                        restoreConsole, ""))
}

png <- function(filename = "Rplot%03d.png", width = 480, height = 480,
                units = "px",
                pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
{
    checkIntFormat(filename)
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=1/25.4, "px"=1) * width
    invisible(.External(Cdevga, paste("png:", filename, sep=""),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, bg, 1,
                        as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                        restoreConsole, ""))
}

bmp <- function(filename = "Rplot%03d.bmp", width = 480, height = 480,
                units = "px",
                pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=1/25.4, "px"=1) * width
    invisible(.External(Cdevga, paste("bmp:", filename, sep=""),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, bg, 1,
                        as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                        restoreConsole, ""))
}

jpeg <- function(filename = "Rplot%03d.jpg", width = 480, height = 480,
                 units = "px",
                 pointsize = 12, quality=75, bg = "white", res = NA,
                 restoreConsole = TRUE)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=1/25.4, "px"=1) * width
    invisible(.External(Cdevga, paste("jpeg:", quality, ":",filename, sep=""),
                        width, height, pointsize, FALSE, 1L,
                        NA_real_, NA_real_, bg, 1,
                        as.integer(res), NA_integer_, FALSE, .PSenv, NA,
                        restoreConsole, ""))
}

bringToTop <- function(which = dev.cur(), stay = FALSE)
{
    if(!exists(".Devices")) {
	.Devices <- list("null device")
    }
    if(which > 0 && .Devices[[which]] != "windows")
        stop("can only bring windows devices to the front")
    invisible(.Internal(bringToTop(as.integer(which), as.logical(stay))))
}

savePlot <- function(filename = "Rplot",
                     type = c("wmf", "emf", "png", "jpeg", "jpg", "bmp",
                     "ps", "eps", "pdf"),
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
    if(nzchar(filename)) filename <- paste(filename, type, sep=".")
    invisible(.External(CsavePlot, device, filename, type, restoreConsole))
}

print.SavedPlots <- function(x, ...)
{
    if(x[[1]] != 31416) {
        cat("object is not of class `SavedPlots'\n")
        return()
    }
    cat("Saved Plots from R version 1.4.0 or later\n\n")
    cat("  Contains", x[[2]], "out of a maximum", x[[3]], "plots\n")
    lens <- sapply(x[[5]], length)[1:x[[2]]]
    cat("  #plot calls are", paste(lens, collapse=", "), "\n")
    cat("  Current position is plot", 1 + x[[4]], "\n")
    invisible(x)
}

"[.SavedPlots" <- function(x, i, ...)
{
    numplots <- x[[2]]
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

printFont <- function(font) paste(font, "\n", sep="")

printFonts <- function(fonts)
    cat(paste(names(fonts), ": ", unlist(lapply(fonts, printFont)),
              sep="", collapse=""))

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


windowsFonts(# Default Serif font is Times
             serif = windowsFont("TT Times New Roman"),
             # Default Sans Serif font is Helvetica
             sans = windowsFont("TT Arial"),
             # Default Monospace font is Courier
             mono = windowsFont("TT Courier New"),
             # Default Symbol font is Symbol
             # Deprecated: remove in 2.8.0
             symbol = windowsFont("TT Symbol"))
