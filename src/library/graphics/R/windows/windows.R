windows <- function(width = 7, height = 7, pointsize = 12,
                    record = getOption("graphics.record"),
                    rescale = c("R", "fit", "fixed"), xpinch, ypinch,
                    bg = "transparent", canvas = "white",
                    gamma = getOption("gamma"),
                    xpos = NA, ypos = NA,
                    buffered = getOption("windowsBuffered"))
{
    rescale <- match.arg(rescale)
    rescale <- match(rescale, c("R", "fit", "fixed"))
    if(missing(xpinch))
        if(!length(xpinch <- getOption("xpinch"))) xpinch <- NA
    xpinch <- as.double(xpinch)
    if(missing(ypinch))
        if(!length(ypinch <- getOption("ypinch"))) ypinch <- NA
    ypinch <- as.double(ypinch)
    .Internal(devga("", width, height, pointsize, record, rescale,
                    xpinch, ypinch, canvas,
                    if(is.null(gamma)) 1 else gamma,
                    as.integer(xpos), as.integer(ypos), buffered, .PSenv, bg
                    ))
}

win.graph <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("", width, height, pointsize, FALSE, 1, NA, NA, "white",
                    1, as.integer(NA), as.integer(NA), TRUE, .PSenv, NA))

win.print <- function(width = 7, height = 7, pointsize = 12, printer = "")
    .Internal(devga(paste("win.print:", printer, sep=""),
                    width, height, pointsize, FALSE, 1,
                    NA, NA, "white", 1, as.integer(NA), as.integer(NA),
                    FALSE, .PSenv, NA))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12)
    .Internal(devga(paste("win.metafile:", filename, sep=""),
                    width, height, pointsize, FALSE, 1, NA, NA, "white", 1,
                    as.integer(NA), as.integer(NA), FALSE, .PSenv, NA))

png <- function(filename = "Rplot%03d.png", width = 480, height = 480,
                pointsize = 12, bg = "white", res = NA)
    .Internal(devga(paste("png:", filename, sep=""),
                    width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                    as.integer(res), as.integer(NA), FALSE, .PSenv, NA))

bmp <- function(filename = "Rplot%03d.bmp", width = 480, height = 480,
                pointsize = 12, bg = "white", res = NA)
    .Internal(devga(paste("bmp:", filename, sep=""),
                    width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                    as.integer(res), as.integer(NA), FALSE, .PSenv, NA))

jpeg <- function(filename = "Rplot%03d.jpg", width = 480, height = 480,
                 pointsize = 12, quality=75, bg = "white", res = NA)
    .Internal(devga(paste("jpeg:", quality, ":",filename, sep=""),
                    width, height, pointsize, FALSE, 1, NA, NA, bg, 1,
                    as.integer(res), as.integer(NA), FALSE, .PSenv, NA))

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
                     type = c("wmf", "png", "jpeg", "jpg", "bmp", "ps", "pdf"),
                     device = dev.cur())
{
    type <- match.arg(type)
    devlist <- dev.list()
    devcur <- match(device, devlist, NA)
    if(is.na(devcur)) stop("no such device")
    devname <- names(devlist)[devcur]
    if(devname != "windows") stop("can only copy from `windows' devices")
    if(filename == "clipboard" && type == "wmf") filename <- ""
    if(nchar(filename) > 0) filename <- paste(filename, type, sep=".")
    invisible(.Internal(saveDevga(device, filename, type)))
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
