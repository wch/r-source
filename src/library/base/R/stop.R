stop <- function(..., call. = TRUE)
{
    args <- list(...)
    if (length(args) > 0)
        message <- paste(..., sep="")
    else message <- ""
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

warning <- function(..., call. = TRUE)
{
    args <- list(...)
    if (length(args) > 0)
        message <- paste(..., sep="")
    else message <- ""
    .Internal(warning(as.logical(call.), message))
}

