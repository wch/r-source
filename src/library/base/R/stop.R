stop <- function(..., call. = TRUE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1 && inherits(args[[1]], "condition")) {
        cond <- args[[1]]
        message <- conditionMessage(cond)
        call = conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
    }
    else {
        if (length(args) > 0) {
            args <- lapply(list(...), as.character)
            ## don't simplify this, as call sequence matters.
            if(!is.na(domain))
                args <- .Internal(gettext(domain, unlist(args)))
            message <- paste(args, collapse = "")
        }
        else message <- ""
        .Internal(stop(as.logical(call.), message))
    }
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

warning <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1 && inherits(args[[1]], "condition")) {
        cond <- args[[1]]
        message <- conditionMessage(cond)
        call = conditionCall(cond)
        withRestarts({
                .Internal(.signalCondition(cond, message, call))
                .Internal(.dfltStop(message, call))
            }, muffleWarning = function() NULL) #**** allow simpler form??
        invisible(message)
    }
    else {
        if (length(args) > 0) {
            args <- lapply(list(...), as.character)
            ## don't simplify this, as call sequence matters.
            if(!is.na(domain))
                args <- .Internal(gettext(domain, unlist(args)))
            message <- paste(args, collapse = "")
        } else message <- ""
        .Internal(warning(as.logical(call.), as.logical(immediate.), message))
    }
}

gettext <- function(..., domain = NULL) {
    args <- lapply(list(...), as.character)
    .Internal(gettext(domain, unlist(args)))
}

bindtextdomain <- function(domain, dirname = NULL)
    .Internal(bindtextdomain(domain, dirname))
