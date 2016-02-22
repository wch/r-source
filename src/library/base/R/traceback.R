#  File src/library/base/R/traceback.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

.traceback <- function(x = NULL) {
    if(is.null(x) && !is.null(x <- get0(".Traceback", envir = baseenv())))
	{}
    else if (is.numeric(x))
    	x <- .Internal(traceback(x))
    x
}

traceback <- function(x = NULL, max.lines = getOption("deparse.max.lines"))
{
    n <- length(x <- .traceback(x))
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
            if(is.numeric(max.lines) && max.lines > 0L && max.lines < m) {
                xi <- c(xi[seq_len(max.lines)], " ...")
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
