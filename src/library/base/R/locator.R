###-- 'msg' interface should be device dependent... -> "next" version
locator <- function(n=512, msg = "Click left to locate points;  right to end") 
{
    do.msg <- is.character(msg) && nchar(msg) > 0
    if(do.msg) cat(msg,": ",sep="")
    z <- .Internal(locator(n))# n <= 0 gives error
    if(do.msg) cat("\n")
    x <- z[[1]]
    y <- z[[2]]
    if((n <- z[[3]]) > 0) list(x=x[1:n], y=y[1:n]) 
}
locator <- function(n=512)
{
    z <- .Internal(locator(n))# n <= 0 gives error
    x <- z[[1]]
    y <- z[[2]]
    if((n <- z[[3]]) > 0) list(x=x[1:n], y=y[1:n]) 
}
