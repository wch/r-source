#functions to convert their first argument to strings
toString <- function(x, ...) UseMethod("toString")

toString.default <- function(x, width = NULL, ...)
{
    string <- paste(x, collapse=", ")
    if( missing(width) || is.null(width) || width == 0) return(string)
    if( width < 0 ) stop("'width' must be positive")
    if(nchar(string, type = "w") > width) {
        width <- max(6, width) ## Leave something!
        string <- paste(strtrim(string, width - 4), "....", sep = "")
    }
    string
}
