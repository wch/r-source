#  File src/library/base/R/traceback.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

.traceback <- function(x = NULL,
		       max.lines = getOption("traceback.max.lines",
					     getOption("deparse.max.lines", -1L)))
{
    stopifnot(length(max.lines) <= 1) # TODO?  allow vector along x
    .is.positive.intlike <- function(x)
        is.numeric(x) && length(x) == 1L && !is.na(x) && as.integer(x) >= 0L

    if((is.null(x) && !is.null(x <- get0(".Traceback", envir = baseenv()))) ||
       is.pairlist(x) || is.list(x))
    {
        valid.max.lines <- .is.positive.intlike(max.lines)
        nlines <- if(valid.max.lines) max.lines + 1L else max.lines
        for(i in seq_along(x)) {
            srcref <- attr(x[[i]], 'srcref')
            if(typeof(x[[i]]) == "language")
                x[[i]] <- deparse(x[[i]], nlines=nlines)
            if(valid.max.lines && length(x[[i]]) > max.lines) {
                x[[i]] <- x[[i]][seq_len(max.lines)]
                attr(x[[i]], 'truncated') <- TRUE
            }
            attr(x[[i]], 'srcref') <- srcref
        }
    }
    else if(is.numeric(x))
    	x <- .Internal(traceback(x))
    x
}

traceback <- function(x = NULL,
                      max.lines = getOption("traceback.max.lines",
                                            getOption("deparse.max.lines", -1L)))
{
    n <- length(x <- .traceback(x, max.lines=max.lines))
    if(n == 0L)
        cat(gettext("No traceback available"), "\n")
    else {
        for(i in 1L:n) {
            xi <- x[[i]]
            label <- paste0(n-i+1L, ": ")
            m <- length(xi)
            ## Find source location (NULL if not available)
            srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
                srcfile <- attr(srcref, "srcfile")
                paste0(" at ", basename(srcfile$filename), "#", srcref[1L])
            }
            ## Truncate deparsed code (destroys attributes of xi)
            if(isTRUE(attr(xi, 'truncated'))) {
                xi <- c(xi, " ...")
                m <- length(xi)
            }
            if (!is.null(srcloc)) {
                xi[m] <- paste0(xi[m], srcloc)
            }
            if(m > 1)
                label <- c(label, rep(substr("          ", 1L,
                                             nchar(label, type="w")),
                                      m - 1L))
            cat(paste0(label, xi), sep="\n")
        }
    }
    invisible(x)
}
