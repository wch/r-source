
# A raster object is a vector of colours
# plus a number of rows and columns
# The vector gives colours in ROW ORDER,
# starting from the TOP row

is.raster <- function(x) {
    inherits(x, "raster")
}

as.raster <- function(x) {
    UseMethod("as.raster")
}

as.raster.raster<- function(x) {
    x
}

# FIXME:  method for vectors, with row and col args

# FIXME:  character matrix assumed to be colour names
#         (just pass through)
#         numeric matrix assumed to be greyscales
#         (convert using grey())
#         3D numeric array assumed to be RGB
#         (convert using rgb())
as.raster.matrix <- function(x) {
    # Transpose ...
    r <- t(x)
    # ... but retain original dimensions
    dim(r) <- dim(x)
    class(r) <- "raster"
    r
}

# For completeness ...
raster <- function(x) {
    as.raster(x)
}


