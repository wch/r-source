windows <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("", width=width, height=height, pointsize=pointsize, 1))

win.graph <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("", width=width, height=height, pointsize=pointsize, 1))

win.print <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("win.print", width=width, height=height, pointsize=pointsize, 1))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12)
    .Internal(devga(paste("win.metafile:", filename, sep=""),
                  width=width, height=height, pointsize=pointsize, 1))

png <- function(filename = "Rplot.png", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("png:", filename, sep=""),
                  width=width, height=height, pointsize=pointsize, 1))

bmp <- function(filename = "Rplot.bmp", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("bmp:", filename, sep=""),
                  width=width, height=height, pointsize=pointsize, 1))

jpeg <- function(filename = "Rplot.jpg", width = 480, height = 480,
                 pointsize = 12, quality=75)
    .Internal(devga(paste("jpeg:", quality, ":",filename, sep=""),
                  width=width, height=height, pointsize=pointsize, 1))

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
    filename <- paste(filename, type, sep=".")
    invisible(.Internal(saveDevga(device, filename, type)))
}
