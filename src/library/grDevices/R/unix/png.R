#  File src/library/grDevices/R/unix/png.R
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

png <- function(filename = "Rplot%03d.png",
                width = 480, height = 480, units = "px",
                pointsize = 12, bg = "white", res = NA, ...,
                type = c("cairo", "Xlib", "quartz"), antialias)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    new <- list(...)
    if(missing(type)) type <- getOption("bitmapType")
    type <- match.arg(type)
    antialiases <- get("antialiases", envir = .X11env)
    if(!missing(antialias))
        new$antialias <- match.arg(antialias, antialiases)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    antialias <- match(d$antialias, antialiases)
    if(type == "quartz" && capabilities("aqua")) {
        width <- width/ifelse(is.na(res), 72, res);
        height <- height/ifelse(is.na(res), 72, res);
        invisible(.External(CQuartz, "png", path.expand(filename),
                            width, height,
                            pointsize, d$family,
                            antialias != 2, TRUE, "", bg,
                            "white", if(is.na(res)) NULL else res))
    } else if (type == "cairo" && capabilities("cairo"))
        invisible(.External(devCairo, filename, 2L, width, height, pointsize,
                            bg, res, antialias, 100L, d$family))
    else
        .Internal(X11(paste("png::", filename, sep=""),
                      width, height, pointsize, d$gamma,
                      d$colortype, d$maxcubesize, bg, bg, d$fonts, res,
                      0L, 0L, "", 0, 0))
}

jpeg <- function(filename = "Rplot%03d.jpeg",
                 width = 480, height = 480, units = "px",
                 pointsize = 12, quality = 75,
                 bg = "white", res = NA, ...,
                 type = c("cairo", "Xlib", "quartz"), antialias)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    new <- list(...)
    if(!missing(type)) new$type <- match.arg(type)
    antialiases <- get("antialiases", envir = .X11env)
    if(!missing(antialias))
        new$antialias <- match.arg(antialias, antialiases)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    if(type == "quartz" && capabilities("aqua")) {
        width <- width/ifelse(is.na(res), 72, res);
        height <- height/ifelse(is.na(res), 72, res);
        invisible(.External(CQuartz, "jpeg", path.expand(filename),
                            width, height, pointsize, d$family,
                            antialias != 2, TRUE, "", bg,
                            "white", if(is.na(res)) NULL else res))
    } else if (type == "cairo" && capabilities("cairo"))
        invisible(.External(devCairo, filename, 3L, width, height, pointsize,
                            bg, res, match(d$antialias, antialiases),
                            quality, d$family))
    else
        .Internal(X11(paste("jpeg::", quality, ":", filename, sep=""),
                      width, height, pointsize, d$gamma,
                      d$colortype, d$maxcubesize, bg, bg, d$fonts, res,
                      0L, 0L, "", 0, 0))
}

tiff <- function(filename = "Rplot%03d.tiff",
                 width = 480, height = 480, units = "px", pointsize = 12,
                 compression = c("none", "rle", "lzw", "jpeg", "zip"),
                 bg = "white", res = NA, ...,
                 type = c("cairo", "Xlib", "quartz"), antialias)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    new <- list(...)
    type <- if(!missing(type)) match.arg(type) else getOption("bitmapType")
    antialiases <- get("antialiases", envir = .X11env)
    if(!missing(antialias))
        new$antialias <- match.arg(antialias, antialiases)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    comp <- switch( match.arg(compression),
                   "none" = 1, "rle" = 2, "lzw" = 5, "jpeg" = 7, "zip" = 8)
    if(type == "quartz" && capabilities("aqua")) {
        width <- width/ifelse(is.na(res), 72, res);
        height <- height/ifelse(is.na(res), 72, res);
        invisible(.External(CQuartz, "tiff", path.expand(filename),
                            width, height, pointsize, d$family,
                            antialias != 2, TRUE, "", bg,
                            "white", if(is.na(res)) NULL else res))
    } else if (type == "cairo" && capabilities("cairo"))
        invisible(.External(devCairo, filename, 8L, width, height, pointsize,
                            bg, res, match(d$antialias, antialiases),
                            comp, d$family))
    else
        .Internal(X11(paste("tiff::", comp, ":", filename, sep=""),
                      width, height, pointsize, d$gamma,
                      d$colortype, d$maxcubesize, bg, bg, d$fonts, res,
                      0L, 0L, "", 0, 0))
}

bmp <- function(filename = "Rplot%03d.bmp",
                width = 480, height = 480, units = "px", pointsize = 12,
                bg = "white", res = NA, ...,
                type = c("cairo", "Xlib", "quartz"), antialias)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    new <- list(...)
    type <- if(!missing(type)) match.arg(type) else getOption("bitmapType")
    antialiases <- get("antialiases", envir = .X11env)
    if(!missing(antialias))
        new$antialias <- match.arg(antialias, antialiases)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    if(type == "quartz" && capabilities("aqua")) {
        width <- width/ifelse(is.na(res), 72, res);
        height <- height/ifelse(is.na(res), 72, res);
        invisible(.External(CQuartz, "bmp", path.expand(filename),
                            width, height, pointsize, d$family,
                            antialias != 2, TRUE, "", bg,
                            "white", if(is.na(res)) NULL else res))
    } else if (type == "cairo" && capabilities("cairo"))
        invisible(.External(devCairo, filename, 9L, width, height, pointsize,
                            bg, res, match(d$antialias, antialiases),
                            100L, d$family))
    else
        .Internal(X11(paste("bmp::", filename, sep=""),
                      width, height, pointsize, d$gamma,
                      d$colortype, d$maxcubesize, bg, bg, d$fonts, res,
                      0L, 0L, "", 0, 0))
}
