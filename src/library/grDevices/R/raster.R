#  File src/library/grDevices/R/raster.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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


# A raster object is a character vector
# of colour strings
# plus a number of rows and columns
# The vector gives colours in ROW ORDER,
# starting from the TOP row
#
# due to the inherent inefficiency of
# raster implementation the graphics
# routines also support "nativeRaster"
# which is the native R representation
# (integer matrix) of colors in the same
# order as raster, suitable for practical
# use with images

is.raster <- function(x)
    inherits(x, "raster")

as.raster <- function(x, ...)
    UseMethod("as.raster")

as.raster.raster <- function(x, ...)  x

as.raster.logical <- function(x, max = 1, ...)
    as.raster(matrix(x, ...), max)

as.raster.numeric <- as.raster.logical

as.raster.character <- as.raster.logical

as.raster.matrix <- function(x, max = 1, ...)
{
    if (is.character(x)) {
        ## Assume to be color names
        r <- t(x)
    } else if (is.numeric(x) || is.logical(x)) {
        ## Assume greyscale or b&w values
        ## We have to use rgb() indirectly as it
        ## doesn't hande NAs correctly
        tx <- t(x)
        tx.na <- which(is.na(tx))
        if (length(tx.na)) tx[tx.na] <- 0
        r <- rgb(tx, tx, tx, maxColorValue = max)
        if (length(tx.na)) r[tx.na] <- NA
    } else
        stop("a raster matrix must be character, or numeric, or logical")
    ## Transposed, but retain original dimensions
    dim(r) <- dim(x)
    class(r) <- "raster"
    r
}

as.raster.array <- function(x, max = 1, ...)
{
    if (!is.numeric(x))
        stop("a raster array must be numeric")
    if (length(dim(x)) != 3L)
        stop("a raster array must have exactly 3 dimensions")
    r <- if (dim(x)[3L] == 3L)
        rgb(t(x[,,1L]), t(x[,,2L]), t(x[,,3L]), maxColorValue = max)
    else if (dim(x)[3] == 4L)
        rgb(t(x[,,1L]), t(x[,,2L]), t(x[,,3L]), t(x[,,4L]), maxColorValue = max)
    else
        stop("a raster array must have exactly 3 or 4 planes")
    dim(r) <- dim(x)[1:2]
    class(r) <- "raster"
    r
}

# Conversion to (character) matrix
as.matrix.raster <- function(x, ...)
{
    dim <- dim(x)
    m <- matrix(x, nrow = dim[1L], ncol = dim[2L], byrow = TRUE)
    m
}

is.na.raster <- function(x) is.na(as.matrix(x))
anyNA.raster <- function(x, recursive = FALSE) anyNA(as.matrix(x))

# FIXME:
# It would be useful to have conversion to array (rgb[a])
# so that people could play with numeric machinations
# with raster images

print.raster <- function(x, ...) print(as.matrix(x), ...)


# Subsetting methods
# Non-standard because raster is ROW-wise
# Try to piggy-back on existing methods as much as possible
# IGNORE 'drop' -- i.e. use "drop = FALSE" -- in all cases, but  m[i]
`[.raster` <- function(x, i, j, drop, ...)
{
    mdrop <- missing(drop)
    nA <- nargs() - (!mdrop)
    if(!mdrop && !identical(drop,FALSE))
        warning("'drop' is always implicitly FALSE in '[.raster'")
    m <- as.matrix(x)
    m <-
	if (missing(i)) {
	    if(missing(j)) m[ , drop = FALSE] else m[, j, drop = FALSE]
	} else if (missing(j)) {
	    if (nA == 2) ## is.matrix(i) || is.logical(i))
		return(m[i]) # behave as a matrix and directly return character vector
	    else if(nA == 3) m[i, , drop = FALSE]
	    else stop("invalid raster subsetting")
	} else m[i, j, drop = FALSE]
    as.raster(m)
}

`[<-.raster` <- function(x, i, j, value)
{
    nA <- nargs()
    m <- as.matrix(x)
    if (missing(i)) {
	if(missing(j)) m[] <- value else m[, j] <- value
    } else if (missing(j)) {
	if (nA == 3) ## typically is.matrix(i) || is.logical(i))
	    m[i] <- value
	else if(nA == 4) m[i, ] <- value
	else stop("invalid raster subassignment")
    } else m[i, j] <- value
    as.raster(m)
}

Ops.raster <- function(e1, e2)
{
    if (.Generic %in% c("==", "!=")) {
        ## Allows comparison of rasters with each other or with colour names
        if (is.raster(e1)) e1 <- as.matrix(e1)
        if (is.raster(e2)) e2 <- as.matrix(e2)
        ## The result is a logical matrix
        get(.Generic)(e1, e2)
    } else {
        stop("operator not meaningful for raster objects")
    }
}

