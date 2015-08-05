#  File src/library/grDevices/R/windows/cairo.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

## These interfaces are undocumented and unexported.
cairo_png <-
    function(filename = "Rplot%03d.png",
             width = 480, height = 480, units = "px", pointsize = 12,
             bg = "white", res = NA, family = "sans",
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
    invisible(.External(C_devCairo, filename, 2L, width, height, pointsize, bg,
                        res, antialias, 100L, family, 300))
}

cairo_jpeg <-
    function(filename = "Rplot%03d.jpeg",
             width = 480, height = 480, units = "px", pointsize = 12,
             quality = 75,
             bg = "white", res = NA, family = "sans",
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
    invisible(.External(C_devCairo, filename, 3L, width, height, pointsize, bg,
                        res, antialias, quality, family, 300))
}

cairo_tiff <-
    function(filename = "Rplot%03d.tiff",
             width = 480, height = 480, units = "px", pointsize = 12,
             compression = c("none", "rle", "lzw", "jpeg", "zip"),
             bg = "white", res = NA, family = "sans",
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
    invisible(.External(C_devCairo, filename, 8L, width, height, pointsize, bg,
                        res, antialias, comp, family, 300))
}

cairo_bmp <-
    function(filename = "Rplot%03d.bmp",
             width = 480, height = 480, units = "px", pointsize = 12,
             bg = "white", res = NA, family = "sans",
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
    invisible(.External(C_devCairo, filename, 9L, width, height, pointsize, bg,
                        res, antialias, 100L, family, 300))
}
