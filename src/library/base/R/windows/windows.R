windows <- function(width = 7, height = 7, pointsize = 12,
                    record = getOption("graphics.record"),
                    rescale = c("R", "fit", "fixed"), xpinch, ypinch)
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
                    xpinch, ypinch))
}

win.graph <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("", width, height, pointsize, FALSE, 1, NA, NA))

win.print <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("win.print", width, height, pointsize, FALSE, 1, NA, NA))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12)
    .Internal(devga(paste("win.metafile:", filename, sep=""),
                  width, height, pointsize, FALSE, 1, NA, NA))

png <- function(filename = "Rplot.png", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("png:", filename, sep=""),
                  width, height, pointsize, FALSE, 1, NA, NA))

bmp <- function(filename = "Rplot.bmp", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("bmp:", filename, sep=""),
                  width, height, pointsize, FALSE, 1, NA, NA))

jpeg <- function(filename = "Rplot.jpg", width = 480, height = 480,
                 pointsize = 12, quality=75)
    .Internal(devga(paste("jpeg:", quality, ":",filename, sep=""),
                  width, height, pointsize, FALSE, 1, NA, NA))

bringToTop <- function(which = dev.cur())
{
    if(!exists(".Devices")) {
	.Devices <- list("null device")
    }
    if(.Devices[[which]] != "windows")
        stop("can only bring windows devices to the front")
    invisible(.Internal(bringToTop(as.integer(which))))
}

savePlot <- function(filename = "Rplot",
                     type = c("wmf", "png", "jpeg", "jpg", "bmp","ps"),
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
