stop <- function(..., call. = TRUE)
{
    if(nargs() == 0) message <- NULL else message <- paste(..., sep="")
    .Internal(stop(as.logical(call.),message))
}

stopifnot <- function(...)
{
    n <- length(ll <- list(...))
    if(n == 0)
        return(invisible())
    mc <- match.call()
    for(i in 1:n)
        if(!(is.logical(r <- eval(ll[[i]])) && all(r)))
            stop(paste(deparse(mc[[i+1]]), "is not TRUE"), call. = FALSE)
}
