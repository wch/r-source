
# A raster object is a vector of colours
# plus a number of rows and columns
# The vector gives colours in ROW ORDER,
# starting from the TOP row

is.raster <- function(x) {
    inherits(x, "raster")
}

as.raster <- function(x, ...) {
    UseMethod("as.raster")
}

as.raster.raster<- function(x, ...) {
    x
}

as.raster.logical <- function(x, max=1, ...) {
    as.raster(matrix(x, ...), max)
}

as.raster.numeric <- as.raster.logical 

as.raster.character <- as.raster.logical

as.raster.matrix <- function(x, max=1, ...) {
    if (is.character(x)) {
        # Assume to be color names
        r <- t(x)
    } else if (is.numeric(x) || is.logical(x)) {
        # Assume greyscale or b&w values
        # Use rgb() to allow for different 'max' value
        tx <- t(x)
        r <- rgb(tx, tx, tx, max=max)
    } else {
        stop("A raster matrix must be character, or numeric, or logical")
    }
    # Transposed, but retain original dimensions
    dim(r) <- dim(x)
    class(r) <- "raster"
    r
}

as.raster.array <- function(x, max=1, ...) {
    if (!is.numeric(x))
        stop("A raster array must be numeric")
    if (length(dim(x)) != 3) {
        stop("A raster array must have exactly 3 dimensions")
    }
    if (dim(x)[3] == 3) {
        r <- rgb(t(x[,,1]), t(x[,,2]), t(x[,,3]), max=max)
    } else if (dim(x)[3] == 4) {
        r <- rgb(t(x[,,1]), t(x[,,2]), t(x[,,3]), t(x[,,4]), max=max)
    } else {
        stop("A raster array must have exactly 3 or 4 planes")
    }
    dim(r) <- dim(x)[1:2]
    class(r) <- "raster"
    r
}

# Conversion to (character) matrix
as.matrix.raster <- function(x, ...) {
    dim <- dim(x)
    m <- matrix(x, nrow=dim[1], ncol=dim[2], byrow=TRUE)
    m
}

# FIXME:
# It would be useful to have conversion to array (rgb[a])
# so that people could play with numeric machinations
# with raster images

print.raster <- function(x, ...) {
    print(as.matrix(x))
}

# Subsetting methods
# Non-standard because raster is ROW-wise
# Try to piggy-back on existing methods as much as possible
# IGNORE 'drop' -- i.e. use "drop = FALSE" -- in all cases, but  m[i]
`[.raster` <- function(x, i, j, drop, ...) {
    mdrop <- missing(drop)
    nA <- nargs() - (!mdrop)
    if(!mdrop && !identical(drop,FALSE))
        warning("'drop' is always implicitly FALSE in '[.raster'")
    m <- as.matrix(x)
    m <-
	if (missing(i)) {
	    if(missing(j)) m[ , drop=FALSE] else m[, j, drop=FALSE]
	} else if (missing(j)) {
	    if (nA == 2) ## is.matrix(i) || is.logical(i))
		return(m[i]) # behave as a matrix and directly return character vector
	    else if(nA == 3) m[i, , drop=FALSE]
	    else stop("invalid raster subsetting")
	} else m[i, j, drop=FALSE]
    as.raster(m)
}

"[<-.raster" <- function(x, i, j, value) {
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

Ops.raster <- function(e1, e2) {
    if (.Generic %in% c("==", "!=")) {
        # Allows comparison of rasters with each other
        # or with colour names
        if (is.raster(e1))
            e1 <- as.matrix(e1)
        if (is.raster(e2))
            e2 <- as.matrix(e2)        
        # The result is a logical MATRIX
        get(.Generic)(e1, e2)
    } else {
        stop("Operator not meaningful for raster objects")
    }
}

