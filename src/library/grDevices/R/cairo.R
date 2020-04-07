#  File src/library/grDevices/R/cairo.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

svg <- function(filename = if(onefile) "Rplots.svg" else "Rplot%03d.svg",
                width = 7, height = 7, pointsize = 12,
                onefile = FALSE, family = "sans", bg = "white",
                antialias = c("default", "none", "gray", "subpixel"),
                symbolfamily)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    if (missing(symbolfamily)) symbolfamily <- symbolfamilyDefault(family)
    invisible(.External(C_devCairo, filename, 4L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile,
                        family, 300, checkSymbolFont(symbolfamily)))
}

cairo_pdf <- function(filename = if(onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                      width = 7, height = 7, pointsize = 12,
                      onefile = FALSE, family = "sans", bg = "white",
                      antialias = c("default", "none", "gray", "subpixel"),
                      fallback_resolution = 300, symbolfamily)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    if (missing(symbolfamily)) symbolfamily <- symbolfamilyDefault(family)
    invisible(.External(C_devCairo, filename, 6L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile,
                        family, fallback_resolution, 
                        checkSymbolFont(symbolfamily)))
}

cairo_ps <- function(filename = if(onefile) "Rplots.ps" else "Rplot%03d.ps",
                     width = 7, height = 7, pointsize = 12,
                     onefile = FALSE, family = "sans", bg = "white",
                     antialias = c("default", "none", "gray", "subpixel"),
                     fallback_resolution = 300, symbolfamily)
{
    if(!checkIntFormat(filename)) stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    if (missing(symbolfamily)) symbolfamily <- symbolfamilyDefault(family)
    invisible(.External(C_devCairo, filename, 7L, 72*width, 72*height,
                        pointsize, bg, NA_integer_, antialias, onefile,
                        family, fallback_resolution,
                        checkSymbolFont(symbolfamily)))
}

cairoVersion <- function() .Call(C_cairoVersion)

pangoVersion <- function() .Call(C_pangoVersion)

cairoFT <- function() .Call(C_cairoFT)

symbolType1support <- function() {
    pangoVersion <- grSoftVersion()["pango"]
    pangoVersion == "" ||
        compareVersion(pangoVersion, "1.44") < 0
}
    
cairoSymbolFont <- function(family, usePUA = TRUE) {
    font <- as.character(family)
    attr(font, "usePUA") <- as.logical(usePUA)
    class(font) <- "CairoSymbolFont"
    font
}

checkSymbolFont <- function(x) {
    if (inherits(x, "CairoSymbolFont")) {
        x
    } else {
        cairoSymbolFont(x)
    }
}

symbolfamilyDefault <- function(family) {
    if (symbolType1support()) {
        if (grSoftVersion()["cairoFT"] == "yes") {
            if (.Platform$OS.type == "windows") {
                "Standard Symbols L"
            } else {
                "Symbol"
            }
        } else {
            "Symbol"
        }
    } else {
        cairoSymbolFont(family, usePUA=FALSE)
    }
}

