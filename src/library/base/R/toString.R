#functions to convert their first argument to strings
toString <- function(x, ...)
    UseMethod("toString")

toString.default <- function(x, width, ...)
{
    string <- paste(x, collapse=", ")
    if( missing(width) )
        return( string )
    if( width <= 0 )
        stop("'width' must be positive")
    if(nchar(string, type="c") > width) {
        width <- max(6, width) ## Leave something!
        string <- paste(strtrim(string, width-4), "....", sep = "")
    }
    string
}

