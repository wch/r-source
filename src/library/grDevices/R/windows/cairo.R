#  File src/library/grDevices/R/windows/cairo.R
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

## The bitmap devices are undocumented and may never be released in this form
cairo_png <-
    function(filename = "Rplot%03d.png",
             width = 480, height = 480, units = "px",
             pointsize = 12, bg = "white", res = NA,
             antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 2L, width, height, pointsize, bg,
                        res, antialias, 100L))
}

cairo_jpeg <-
    function(filename = "Rplot%03d.jpeg",
             width = 480, height = 480, units = "px",
             pointsize = 12, quality = 75,
             bg = "white", res = NA,
             antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 3L, width, height, pointsize, bg,
                        res, antialias, quality))
}

cairo_tiff <-
    function(filename = "Rplot%03d.tiff",
             width = 480, height = 480, units = "px", pointsize = 12,
             compression = c("none", "rle", "lzw", "jpeg", "zip"),
             bg = "white", res = NA,
             antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    comp <- switch( match.arg(compression),
                   "none" = 1, "rle" = 2, "lzw" = 5, "jpeg" = 7, "zip" = 8)
    invisible(.External(winCairo, filename, 8L, width, height, pointsize, bg,
                        res, antialias, comp))
}

cairo_bmp <-
    function(filename = "Rplot%03d.bmp",
             width = 480, height = 480, units = "px", pointsize = 12,
             bg = "white", res = NA,
             antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    if(units != "px" && is.na(res))
        stop("'res' must be specified unless 'units = \"px\"'")
    height <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * height
    width <-
        switch(units, "in"=res, "cm"=res/2.54, "mm"=res/25.4, "px"=1) * width
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 9L, width, height, pointsize, bg,
                        res, antialias, 100L))
}

svg <- function(filename = if(onefile) "Rplots.svg" else "Rplot%03d.svg",
                width = 7, height = 7, pointsize = 12,
                onefile = FALSE, bg = "white",
                antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    new <- list()
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 4L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile))
}

cairo_pdf <- function(filename = if(onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                      width = 7, height = 7, pointsize = 12,
                      onefile = FALSE, bg = "white",
                      antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 6L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile))
}

cairo_ps <- function(filename = if(onefile) "Rplots.ps" else "Rplot%03d.ps",
                     width = 7, height = 7, pointsize = 12,
                     onefile = FALSE, bg = "white",
                     antialias = c("default", "none", "gray", "subpixel"))
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(winCairo, filename, 7L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile))
}
