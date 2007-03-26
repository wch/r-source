windows <- function(width = 7, height = 7, pointsize = 12,
                    record = getOption("graphics.record"),
                    rescale = c("R", "fit", "fixed"), xpinch, ypinch,
                    bg = "transparent", canvas = "white",
                    gamma = getOption("gamma"),
                    xpos = NA, ypos = NA,
                    buffered = getOption("windowsBuffered"),
                    restoreConsole = FALSE)
{
    rescale <- match.arg(rescale)
    rescale <- match(rescale, c("R", "fit", "fixed"))
    if(missing(xpinch))
        if(!length(xpinch <- getOption("xpinch"))) xpinch <- NA
    xpinch <- as.double(xpinch)
    if(missing(ypinch))
        if(!length(ypinch <- getOption("ypinch"))) ypinch <- NA
    ypinch <- as.double(ypinch)
    invisible(.External("devga", "", width, height, pointsize, record,
                        rescale, xpinch, ypinch, canvas,
                        if(is.null(gamma)) 1 else gamma,
                        as.integer(xpos), as.integer(ypos), buffered,
                        .PSenv, bg, restoreConsole, PACKAGE = "grDevices"))
}

win.graph <- function(width = 7, height = 7, pointsize = 12,
                      restoreConsole = FALSE)
{
    gamma <- getOption("gamma")
    if(!length(xpinch <- getOption("xpinch"))) xpinch <- NA
    if(!length(ypinch <- getOption("ypinch"))) ypinch <- NA
    invisible(.External("devga", "", width, height, pointsize,
                        getOption("graphics.record"), 1,
                        as.double(xpinch), as.double(ypinch), "white",
                        if(is.null(gamma)) 1 else gamma,
                        as.integer(NA), as.integer(NA),
                        getOption("windowsBuffered"),
                        .PSenv, NA, restoreConsole, PACKAGE = "grDevices"))
}

win.print <- function(width = 7, height = 7, pointsize = 12, printer = "",
                      restoreConsole = TRUE)
    invisible(.External("devga", paste("win.print:", printer, sep=""),
                        width, height, pointsize, FALSE, 1,
                        NA, NA, "white", 1, as.integer(NA), as.integer(NA),
                        FALSE, .PSenv, NA, restoreConsole,
                        PACKAGE = "grDevices"))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12,
                         restoreConsole = TRUE)
    invisible(.External("devga", paste("win.metafile:", filename, sep=""),
                        width, height, pointsize, FALSE, 1, NA, NA, "white", 1,
                        as.integer(NA), as.integer(NA), FALSE, .PSenv, NA,
                        restoreConsole, PACKAGE = "grDevices"))

png <- function(filename = "Rplot%03d.png", width = 480, height = 480,
                pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
    invisible(.External("devga", paste("png:", filename, sep=""),
                        width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                        as.integer(res), as.integer(NA), FALSE, .PSenv, NA,
                        restoreConsole, PACKAGE = "grDevices"))

bmp <- function(filename = "Rplot%03d.bmp", width = 480, height = 480,
                pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
    invisible(.External("devga", paste("bmp:", filename, sep=""),
                        width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                        as.integer(res), as.integer(NA), FALSE, .PSenv, NA,
                        restoreConsole, PACKAGE = "grDevices"))

jpeg <- function(filename = "Rplot%03d.jpg", width = 480, height = 480,
                 pointsize = 12, quality=75, bg = "white", res = NA, restoreConsole = TRUE)
    invisible(.External("devga", paste("jpeg:", quality, ":",filename, sep=""),
                        width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                        as.integer(res), as.integer(NA), FALSE, .PSenv, NA, restoreConsole,
                        PACKAGE = "grDevices"))

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
    if(nchar(filename) > 0) filename <- paste(filename, type, sep=".")
    invisible(.External("savePlot", device, filename, type, restoreConsole,
                        PACKAGE = "grDevices"))
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

.Windowsenv <- new.env()

# Each font family has only a name
assign(".Windows.Fonts", list(), envir = .Windowsenv)

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
    fontDB <- get(".Windows.Fonts", envir=.Windowsenv)
    existingFonts <- fontNames %in% names(fontDB)
    if (sum(existingFonts) > 0)
        fontDB[fontNames[existingFonts]] <- fonts[existingFonts]
    if (sum(existingFonts) < length(fontNames))
        fontDB <- c(fontDB, fonts[!existingFonts])
    assign(".Windows.Fonts", fontDB, envir=.Windowsenv)
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
        get(".Windows.Fonts", envir=.Windowsenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character)))
                stop("invalid arguments in 'windowsFonts' (must be font names)")
            else
                get(".Windows.Fonts", envir=.Windowsenv)[unlist(fonts)]
        } else {
            if (ndots != nnames)
                stop("invalid arguments in 'windowsFonts' (need named args)")
            setWindowsFonts(fonts, fontNames)
        }
    }
}

# Create a valid windows font description
windowsFont <- function(family) checkWindowsFont(family)


windowsFonts(# Default Serif font is Times
             serif = windowsFont("TT Times New Roman"),
             # Default Sans Serif font is Helvetica
             sans = windowsFont("TT Arial"),
             # Default Monospace font is Courier
             mono = windowsFont("TT Courier New"),
             # Default Symbol font is Symbol
             symbol = windowsFont("TT Symbol"))
