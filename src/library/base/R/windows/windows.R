windows <- function(width = 7, height = 7, pointsize = 12,
                    record = getOption("graphics.record"))
    .Internal(devga("", width, height, pointsize, record))

win.graph <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("", width, height, pointsize, FALSE))

win.print <- function(width = 7, height = 7, pointsize = 12)
    .Internal(devga("win.print", width, height, pointsize, FALSE))

win.metafile <- function(filename = "", width = 7, height = 7, pointsize = 12)
    .Internal(devga(paste("win.metafile:", filename, sep=""),
                  width, height, pointsize, FALSE))

png <- function(filename = "Rplot.png", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("png:", filename, sep=""),
                  width, height, pointsize, FALSE))

bmp <- function(filename = "Rplot.bmp", width = 480, height = 480,
                pointsize = 12)
    .Internal(devga(paste("bmp:", filename, sep=""),
                  width, height, pointsize, FALSE))

jpeg <- function(filename = "Rplot.jpg", width = 480, height = 480,
                 pointsize = 12, quality=75)
    .Internal(devga(paste("jpeg:", quality, ":",filename, sep=""),
                  width, height, pointsize, FALSE))

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
