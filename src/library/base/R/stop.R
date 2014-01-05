#  File src/library/base/R/stop.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-20123 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

stop <- function(..., call. = TRUE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if(nargs() > 1L)
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
    if(n == 0L)
	return(invisible())
    mc <- match.call()
    for(i in 1L:n)
	if(!(is.logical(r <- ll[[i]]) && !anyNA(r) && all(r))) {
	    ch <- deparse(mc[[i+1]], width.cutoff = 60L)
	    if(length(ch) > 1L) ch <- paste(ch[1L], "....")
            stop(sprintf(ngettext(length(r),
                                  "%s is not TRUE",
                                  "%s are not all TRUE"),
                         ch), call. = FALSE, domain = NA)
	}
    invisible()
}

warning <- function(..., call. = TRUE, immediate. = FALSE, domain = NULL)
{
    args <- list(...)
    if (length(args) == 1L && inherits(args[[1L]], "condition")) {
        cond <- args[[1L]]
        if(nargs() > 1L)
            cat(gettext("additional arguments ignored in warning()"),
                "\n", sep = "", file = stderr())
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
