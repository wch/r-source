stop <- function(..., call. = TRUE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1 && inherits(args[[1]], "condition")) {
        cond <- args[[1]]
        if(nargs() > 1)
            warning("additional arguments ignored in stop()")
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        .Internal(.signalCondition(cond, message, call))
        .Internal(.dfltStop(message, call))
    } else
        .Internal(stop(as.logical(call.), .makeMessage(..., domain = domain)))
}

stopifnot <- function(...)
{
    n <- length(ll <- list(...))
    if(n == 0)
	return(invisible())
    mc <- match.call()
    for(i in 1:n)
	if(!(is.logical(r <- eval(ll[[i]])) && !any(is.na(r)) && all(r))) {
	    ch <- deparse(mc[[i+1]], width.cutoff = 60)
	    if(length(ch) > 1) ch <- paste(ch[1], "....")
	    stop(paste(ch, " is not ", if(length(r) > 1)"all ", "TRUE", sep=''),
		 call.= FALSE)
	}
}

warning <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1 && inherits(args[[1]], "condition")) {
        cond <- args[[1]]
        if(nargs() > 1)
            cat(gettext("additional arguments ignored in warning()"),
                "\n", sep="", file = stderr())
        message <- conditionMessage(cond)
        call <- conditionCall(cond)
        withRestarts({
                .Internal(.signalCondition(cond, message, call))
                .Internal(.dfltWarn(message, call))
            }, muffleWarning = function() NULL) #**** allow simpler form??
        invisible(message)
    } else
        .Internal(warning(as.logical(call.), as.logical(immediate.),
                          .makeMessage(..., domain = domain)))
}

gettext <- function(..., domain = NULL) {
    args <- lapply(list(...), as.character)
    .Internal(gettext(domain, unlist(args)))
}

bindtextdomain <- function(domain, dirname = NULL)
    .Internal(bindtextdomain(domain, dirname))

ngettext <- function(n, msg1, msg2, domain = NULL)
    .Internal(ngettext(n, msg1, msg2, domain))

gettextf <- function(fmt, ..., domain = NULL)
    sprintf(gettext(fmt, domain = domain), ...)
