#functions to convert their first argument to strings
toString <- function(x, ...)
    UseMethod("toString")

toString.default <- function(x, width, ...) {
  string <- paste(x, collapse=", ")
  if( missing(width) )
    return( string )
  if( width <= 0 )
    stop("width must be positive")
  if(nchar(string) > width) {
    if(width < 6)
      width <- 6  ## Leave something!
    string <- paste(substring(string, 1, width-4), "....", sep="")
  }
  string
}

